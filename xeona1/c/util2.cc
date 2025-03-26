//  XEONA ENERGY SYSTEMS MODELING ENVIRONMENT
//
//  file-create-name : util2.cc
//  file-create-date : Thu 06-Nov-2008 16:53 UTC
//  file-initiator   : Robbie Morrison <robbie@actrix.co.nz>
//
//  file-role        : free functions which offer general utilities 2 / implementation
//  file-status      : working
//  file-release-tag :
//
//  LEGAL NOTICE
//
//  Software  : This file is part of the source code for the xeona energy
//              systems modeling environment.
//  License   : This software is distributed under the GNU General Public
//              License version 3, a copy of which is provided in the text
//              file LICENSE_GPLv3.
//  Warranty  : There is no warranty for this software, to the extent permitted
//              by applicable law.  Refer to the license for further details.
//  Copyright : This software is copyright (c) 2007 - 2012 Robbie Morrison.
//  Request   : The software is distributed with the request that you forward
//              any modifications you make to the xeona project for possible
//              inclusion in the main codebase.
//
//  PROJECT CONTACT
//
//  Robbie Morrison
//  Institute for Energy Engineering
//  Technical University of Berlin
//  Marchstrasse 18, D-10587 Berlin, Germany
//  Email: robbie@actrix.co.nz
//
//  SVN VERSION CONTROL
//
//  $Author: robbie $
//  $URL: file:///home/robbie/svn-root/xeona/futz/trunk/xeona1/c/util2.cc $
//
//  GENERAL NOTES FOR THIS FILE

//  LOCAL AND SYSTEM INCLUDES

#include "util2.h"            // companion header for this file (place first)

#include "../f/ospinfo.h"     // domain mode interpretation
#include "../c/util4.h"       // free functions for trigonometry and maths
#include "../b/lmpbid.h"      // LMP auction bidset

#include "../a/logger.h"      // standard logging functionality (as required)
#include "../c/smart_ptr.h"   // toggle between Boost and TR1 smart pointers
#include ".././common.h"      // common definitions for project (place last)

#include <algorithm>          // STL copying, searching, and sorting, max()
#include <iterator>           // STL additional iterators, std::distance()
#include <numeric>            // STL numerical algorithms
#include <sstream>            // string-streams
#include <string>             // C++ strings
#include <vector>             // STL sequence container

#include <cmath>              // C-style maths, abs(), ceil(), floor(), sqrt()

#include <boost/foreach.hpp>            // BOOST_FOREACH iteration macro
#include <boost/logic/tribool.hpp>      // three state boolean
#include <boost/logic/tribool_io.hpp>   // CAUTION: essential for std::boolalpha reporting

//  NAMESPACE DECLARATIONS

using boost::logic::tribool;            // three state boolean
using boost::logic::indeterminate;      // allows 'indeterminate' and 'indeterminate()'

//  CODE

namespace xeona
{

  // ---------------------------------------------------------
  //  FREE FUNCTION   : geometricProgression
  // ---------------------------------------------------------
  //  Description  : transform positive integer to geometric progression
  //  Role         : called by 'xeona::isTwoContained', also by class 'DomainModeDatabase'
  //  Techniques   : bitfield masks, '&' bitwise and operator
  //  Status       : complete
  //
  //  Design notes
  //
  //      This routine now uses bitwise manipulation.  See r6810
  //      for the arithmetic version: $ svn cat --revision 6810 c/util2.cc
  //
  //  Examples
  //
  //           0 -> empty vector
  //           1 -> { 1 }
  //          30 -> { 0  2  4  8 16 }
  //          32 -> { 0  0  0  0  0 32 }
  //
  // ---------------------------------------------------------

  std::vector<int>
  geometricProgression
  (const int aggregate)
  {
    // logger
    static logga::spLogger logger = logga::ptrLogStream();

    // geometric progression vector
    std::vector<int> reds;                   // vector of remainder values or zeros

    // integrity checks
    if ( aggregate < 0 )                     // non-negativity condition
      {
        logger->repx(logga::warn, "negative aggregate supplied", aggregate);
        logger->repx(logga::dbug, "abandoning function", "");
        return reds;
      }

    // active code
    if ( aggregate > 0 )                    // protects against log2(zero) as well
      {
        // establish an upper limit power-of-two
        const double   log2 = xeona::log2(aggregate);       // see unit 'c/util4'
        const double   temp = std::ceil(log2);              // round up to next integer
        const unsigned uppa = static_cast<unsigned>(temp);  // type cast to 'unsigned'

        // run the masks
        for (unsigned i = 0; i <= uppa; ++i )
          {
            const int mask = static_cast<int>(std::pow(2.0, i));
            if ( aggregate & mask ) reds.push_back(mask);   // bitwise and operator
            else                    reds.push_back(0);
          }
        while ( reds.back() == 0 ) reds.pop_back();         // remove any final zero
      } // nonzero aggregates

    // check the 'reds' vector sums to 'aggregate'
    const int sum = std::accumulate          // refer <numeric>
      (reds.begin(), reds.end(),             // range [first, last)
       0);                                   // initial value
    if ( aggregate != sum )
      {
        logger->repx(logga::warn, "aggregate != sum", "");
        std::cerr << "** coding error 01 in source file " << __FILE__ << std::endl;
      }

    // additional reporting
    // YEEK 9 CODE (set by '--yeek')
#ifndef _XUTEST                              // not a unit test build
    if ( xeona::yeek == 9 || xeona::yeek == 1 || xeona::yeek == 2 )
#else
    if ( true )
#endif
      {
        std::ostringstream put;
        put << std::boolalpha;
        put << "  geometric progression aggregate : " << aggregate
            << "    vector :";
        BOOST_FOREACH( int i, reds ) put << " " << i;
        put << "    interpret : " << xeona::infoDomainModePure(reds) << "\n"; // [1]
        logger->repx(logga::dbug, "additional reporting follows, yeek", xeona::yeek);
        logger->putx(logga::dbug, put);
      }

    // CAUTION: [1] do NOT CALL the 'int' version of
    // 'xeona::infoDomainModePure' from here or else an
    // endless ping-pong of embedded calls will rapidly
    // segfault -- this bug spanned commits r5862 to r5866

    // return
    return reds;

  } // function 'xeona::geometricProgression'

  // ---------------------------------------------------------
  //  FREE FUNCTION   : xeona::isTwoContained
  // ---------------------------------------------------------
  //  Description  : test if 'candidate' is a power-of-two sub-sequence of 'aggregate'
  //  Role         : used for checking commitment strategy codes
  //  On success   : returns 'true' if 'candidate' is contained in 'aggregate'
  //  On fail      : returns 'indeterminate' for faulty input
  //  Techniques   : 'std::set_difference'
  //  Status       : complete
  // ---------------------------------------------------------

  tribool                                    // 'indeterminate' means faulty input
  isTwoContained
  (const int candidate,                      // greater than zero
   const int aggregate)                      // non-negative, can be less than 'candidate'
  {
    // logger
    static logga::spLogger logger = logga::ptrLogStream();

    // basic range checks
    if ( candidate < 1 ) return indeterminate;    // 2
    if ( aggregate < 0 ) return indeterminate;    // 2

    // grab the geometric progressions
    std::vector<int> redCan = xeona::geometricProgression(candidate);
    std::vector<int> redAgg = xeona::geometricProgression(aggregate);

    // remove zeros and, in the process, produce sorted containers
    redCan.erase(std::remove(redCan.begin(), redCan.end(), 0), redCan.end());
    redAgg.erase(std::remove(redAgg.begin(), redAgg.end(), 0), redAgg.end());

    // vector for set-difference residuals
    std::vector<int> resids;

    // CAUTION: sorting: the 'set_difference' function template
    // from <algorithm> requires sorted ranges -- moreover here
    // the range is the entire container

    std::set_difference(redCan.begin(), redCan.end(),  // refer <algorithm>
                        redAgg.begin(), redAgg.end(),
                        std::back_inserter(resids));   // refer <iterator>, [1]

    // [1] results container: Josuttis (1999 p420) writes "the
    // caller must ensure that the destination range is big
    // enough or that insert iterators are used" -- the latter
    // approach is used here as also indicated on p272

    // additional reporting
    // YEEK 10 CODE (set by '--yeek')
    if ( xeona::yeek == 10 || xeona::yeek == 1 || xeona::yeek == 2 )
      {
        const std::string func = XEONA_FUNC; // preprocessor macro defined in 'common.h'
        std::ostringstream put;
        put << std::boolalpha;
        put << "  is-two-contained reporting"             << "\n"
            << "    function   : "  << func               << "\n"
            << "    candidate  : "  << candidate          << "\n"
            << "    aggregate  : "  << aggregate          << "\n"
            << "    return     : "  << resids.empty()     << "\n";
        put << "    dezeroed reduced candidate :";
        BOOST_FOREACH( int i, redCan ) put << " " << i;
        put << "\n";
        put << "    dezeroed reduced aggregate :";
        BOOST_FOREACH( int i, redAgg ) put << " " << i;
        put << "\n";
        put << "    residual   :";
        BOOST_FOREACH( int i, resids ) put << " " << i;
        put << "\n";
        logger->repx(logga::dbug, "additional reporting follows, yeek", xeona::yeek);
        logger->putx(logga::dbug, put);
      } // if 'yeek 10'

    // return
    if ( resids.empty() ) return true;       // 1
    else                  return false;      // 0

  } // function 'isTwoContained'

  // ---------------------------------------------------------
  //  FREE FUNCTION   : xeona::reducedVector
  // ---------------------------------------------------------

  std::string                                // no trailing newline
  reducedVector                              // wrapper-style call
  (const int         aggregate,
   const std::string separator)              // note default
  {
    const std::vector<int> reds = geometricProgression(aggregate);

    std::stringstream oss;
    BOOST_FOREACH( int i, reds )
      {
        oss << i << separator;
      }

    std::string buf = oss.str();
    const int len   = buf.size() - separator.size();
    buf = buf.substr(0, len);                // trim final separator

    return buf;
  }

} // namespace 'xeona'

// ---------------------------------------------------------
//  FREE FUNCTION   : xeona::coeffProduct <>
// ---------------------------------------------------------
//  Description  : calculate inner product while honoring constant term
//  Role         : obtain objective result without using the solver
//  Techniques   : 'std::inner_product'
//  Status       : complete
//
//  Equation
//
//      answer = a_0 + sum( a_i * x_i )   for i in { 1, .., n }
//
//          with 'x_0' ignored for calculation purposes
//          and the vectors 'a' and 'x' being of length n+1
//
//  Design notes
//
//      The STL 'std::inner_product' function is employed.  This
//      function is discussed by Lischner (2003 p628) and
//      Josuttis (1999 pp427-429).
//
//      The 'boost::numeric::ublas::inner_prod' function from the
//      'Boost.uBLAS' library could also have been used.
//
//      The N() is default constructed, so for type 'double' this
//      simply yields 0.0.
//
//      Although templated, this function will probably only be
//      instantiated with 'double.'
//
// ---------------------------------------------------------

namespace xeona
{
  template<typename N>                       // implicit instantiation supported
  N
  coeffProduct
  (const std::vector<N> a,                   // coefficients vector
   const std::vector<N> x)                   // variable values vector
  {
    // logger
    static logga::spLogger logger = logga::ptrLogStream();
    logger->repx(logga::adhc, "entering member function", "");

    // integrity checks
    const int alen = a.size();
    const int xlen = x.size();
    if ( alen != xlen                        // imbalance
         || alen < 1                         // empty
         || xlen < 1 )                       // empty
      {
        // warn and abandon
        std::ostringstream oss;
        oss << alen << " : " << xlen;
        logger->repx(logga::warn, "vector size problem, a : x", oss.str());
        logger->repx(logga::xtra, "returning default value", N());
        return N();
      }
    if ( x.front() != N() )
      {
        // warn and continue
        logger->repx(logga::warn, "vector x zeroth element not zero", x.front());
        logger->repx(logga::xtra, "continuing nevertheless", "");
      }

    // calculation
    const N inprod = std::inner_product(++a.begin(),   // see <numeric>, skip first
                                          a.end(),
                                        ++x.begin(),   // similarly skip first element
                                          a.front());  // grab the "shift" term
    return inprod;
  }

} // namespace 'xeona'

// ---------------------------------------------------------
//  FREE FUNCTION   : xeona::isIntegerValued (double)
// ---------------------------------------------------------
//  Description  : checks for integer-valued floating point number
//  Role         : general use
//  Techniques   : 'trunc' (requires C99)
//  Note         : there is a similar non-C99 function in 'd/siglp'
//  Status       : complete
// ---------------------------------------------------------

namespace xeona
{
  bool
  isIntegerValued
  (const double test)
  {
    return ( trunc(test) == test );          // refer <cmath>, not 'std::trunc', needs C99
  }
}

// ---------------------------------------------------------
//  FREE FUNCTION   : xeona::isEven (integral types)
// ---------------------------------------------------------
//  Description  : checks if integer 'test' is even
//  Role         : general usage
//  Techniques   : '%' modulo operator
//  Status       : complete
//
//  Some design variations
//
//        if ( test & 1 ) ;        // odd
//
//        if ( test % 2 == 0 ) ;   // even
//
//        const double check = static_cast<double>(test) / 2.0;
//        if ( check == std::floor(check) ) ; // even
//
//      Note that the opposite of even as odd is only the case
//      for integral types.  In which case a 'Boost.Tribool'
//      return might be useful.
//
//      In addition, note 'std::modf' from <cmath> and
//      'boost::math::modf' from
//      <boost/math/special_functions/modf.hpp>.  Check the
//      syntax too, its a bit strange.
//
// ---------------------------------------------------------

namespace xeona
{
  template <typename I>                      // implicit instantiation supported
  bool                                       // 'true' if even, else odd
  isEven
  (const I test)
  {
#if 0 // 0 = robust for all types, 1 = faster for integral types
    const double check = static_cast<double>(test) / 2.0;
    if ( check == std::floor(check) ) return true;    // see <cmath>
    else                              return false;
#else
    if ( test % 2 == 0 ) return true;
    else                 return false;
#endif // 0
  }
}

// ---------------------------------------------------------
//  FREE FUNCTION   : xeona::reorderUpwards <>
// ---------------------------------------------------------
//  Description  : reorder 'data' based on 'ordering', utilizes 'std::sort'
//  Role         : 'AsopLmpBidAdaptive1::establish'
//  Techniques   : 'std::sort' from <algorithm>
//  Status       : complete
//
//  Design notes:
//
//     'std::sort' uses 'operator<' and hence the ordering is
//     from small to large.
//
//     This version requires a 'shared_ptr', but a straight
//     version could also be implemented.
//
//  Inspiration
//
//      http://stackoverflow.com/questions/2961996/
//        c-sorting-a-vector-based-on-values-of-other-vector-or-whats-faster
//      http://stackoverflow.com/questions/236172/
//        how-do-i-sort-a-stdvector-by-the-values-of-a-different-stdvector
//
// ---------------------------------------------------------

namespace xeona
{
  template <typename S,                      // must support 'operator<'
            typename T>                      // no technical restrictions
  bool                                       // 'false' on vector length mismatch
  reorderUpwards
  (const std::vector<S>&        ordering,    // unsorted
   std::vector<shared_ptr<T> >& data)        // reorder by increasing 'ordering'
  {
    // typedef
    typedef std::pair<S, shared_ptr<T> > pair_type;

    // confirm lengths
    if ( ordering.size() != data.size() ) return false;

    // preamble
    const unsigned len = ordering.size();
    std::vector<pair_type> pairs;

    // fill 'pairs'
    pairs.resize(len);
    for ( size_t i = 0; i < len; ++i )
      {
        pairs.at(i).first  = ordering.at(i);
        pairs.at(i).second = data.at(i);
    }

    // sort 'pairs' noting sort utilizes 'first' in this case
    std::sort(pairs.begin(), pairs.end());   // defaults to '<' comparison

    // refill 'data'
    for ( size_t i = 0; i < len; ++i )
      {
        data.at(i) = pairs.at(i).second;
      }

    // indicate success
    return true;
  }
} // namespace 'xeona'

// ---------------------------------------------------------
//  EXPLICIT TEMPLATE INSTANTIATIONS
// ---------------------------------------------------------

class TechnicalAsset;

template double xeona::coeffProduct(const std::vector<double>, const std::vector<double>);
template  float xeona::coeffProduct(const std::vector<float> , const std::vector<float>);
#if 0 // 0 = prevent integer-valued calculations
template    int xeona::coeffProduct(const std::vector<int>   , const std::vector<int>);
#endif // 0

template bool xeona::isEven(const int);
template bool xeona::isEven(const unsigned);

template bool xeona::reorderUpwards(const std::vector<int>&,
                                    std::vector<shared_ptr<TechnicalAsset> >&);
template bool xeona::reorderUpwards(const std::vector<double>&,
                                    std::vector<shared_ptr<TechnicalAsset> >&);

#ifdef _XUTEST

class UnitTestClass;
template bool xeona::reorderUpwards(const std::vector<double>&,
                                    std::vector<shared_ptr<UnitTestClass> >&);

#endif // _XUTEST

//  end of file

