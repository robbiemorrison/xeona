//  XEONA ENERGY SYSTEMS MODELING ENVIRONMENT
//
//  file-create-name : utils.h
//  file-create-date : Thu 14-Jun-2007 15:45 UTC
//  file-initiator   : Robbie Morrison <robbie@actrix.co.nz>
//
//  file-role        : free functions which offer general utilities 1 / header
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
//  $URL: file:///home/robbie/svn-root/xeona/futz/trunk/xeona1/c/util1.h $

//  GENERAL NOTES FOR THIS FILE
//
//  This unit provides free functions to support:
//
//    random numbers          : obtain random doubles
//    quantity formatting     : format SI units, format unit prices
//    vectors                 : repetition, printing, copying
//    smart pointers          : generate null shared_ptr
//    boost time              : system time resolution, format boost duration
//    direct logging          : special direct logging ('logga::repx' is normally better)
//    sequence containers     : reverse order, append tail,
//    type name demangling    : repair GCC run-time type information

//  HEADER GUARD

#ifndef _UTIL1_H_
#define _UTIL1_H_

//  LOCAL AND SYSTEM INCLUDES

#include "../c/smart_ptr.h"   // toggle between TR1 and Boost smart pointers

#include <algorithm>          // STL copying, searching, and sorting, max()
#include <iomanip>            // setw() and family
#include <iostream>           // standard io
#include <iterator>           // STL additional iterators, std::distance()
#include <sstream>            // string-streams
#include <string>             // C++ strings
#include <vector>             // STL sequence container

#include <boost/logic/tribool.hpp>      // three state boolean
#include <boost/logic/tribool_io.hpp>   // CAUTION: essential for std::boolalpha reporting

#include <boost/any.hpp>                // type heterogeneous storage
#include <boost/filesystem.hpp>         // path objects, iterators, useful operations
#include <boost/format.hpp>             // printf style formatting
#include <boost/date_time/posix_time/posix_time_duration.hpp>   // limited header

//  NAMESPACE DECLARATIONS

using boost::logic::tribool;            // three state boolean
using boost::logic::indeterminate;      // allows 'indeterminate' and 'indeterminate()'

//  FORWARD (PARTIAL) DECLARATIONS

namespace xeona { extern const unsigned consoleWidth; }     // from "common.h"

//  CODE

namespace xeona
{

  // ---------------------------------------------------------
  //  FREE FUNCTION   : xeona::getRandom (int, int)
  // ---------------------------------------------------------
  //  Description  : retrieve random ints in required range
  // ---------------------------------------------------------

  int                                        // retrieve a random number
  getRandom
  (int lower,                                // inclusive lower bound
   int upper);                               // inclusive upper bound

  // ---------------------------------------------------------
  //  FREE FUNCTION   : xeona::getRandom (double, double)
  // ---------------------------------------------------------
  //  Description  : retrieve random doubles in required range
  // ---------------------------------------------------------

  double                                     // retrieve a random number
  getRandom
  (double lower,                             // inclusive lower bound
   double upper);                            // inclusive upper bound

  // ---------------------------------------------------------
  //  FREE FUNCTION   : xeona::fmtQuantity
  // ---------------------------------------------------------
  //  Description  : return a quantity in resolved SI format
  // ---------------------------------------------------------

  std::string
  fmtQuatity
  (const int         value,
   const std::string baseUnit);

  std::string
  fmtQuantity
  (const double      value,                  // numerical value
   const std::string baseUnit);              // for instance, 'W' for Watts

  // ---------------------------------------------------------
  //  FREE FUNCTION   : xeona::fmtPriceRate
  // ---------------------------------------------------------
  //  Description  : return a price rate in readable 'kilo' format
  // ---------------------------------------------------------

  std::string
  fmtPriceRate
  (const int         priceRate,              // numerical value
   const std::string baseUnit);              // for instance, 'W' for Watts

  std::string
  fmtPriceRate
  (const double      priceRate,              // numerical value
   const std::string baseUnit);              // for instance, 'W' for Watts

  // ---------------------------------------------------------
  //  FREE FUNCTION   : xeona::vectorRepeat <>
  // ---------------------------------------------------------
  //  Description  : create a vector from a repeating pattern
  //  Techniques   : explicit template instantiations in source file
  // ---------------------------------------------------------

  template <typename T>                      // meaning a std::vector<T> container
  bool                                       // returns false if pattern is empty
  vectorRepeat
  (const std::vector<T>& pattern,            // pattern vector, must not be empty
   std::vector<T>&       fullset);           // output vector must be correct length

  // ---------------------------------------------------------
  //  FREE FUNCTION   : xeona::vectorReport <>
  // ---------------------------------------------------------
  //  Description  : print a vector to a nominated ostream
  //  Role         : for testing purposes
  //  Techniques   : fully defined in header to avoid template instantiation problems
  //  Status       : complete
  //
  //  Usage
  //
  //      std::vector<int> vec(10, 2)                 // contains ten twos
  //      xeona::vectorReport(vec);                   // note implicit instantiation
  //      xeona::vectorReport(vec, "\n", std::clog);  // one per line to stderr
  //
  //  Design notes
  //
  //      Unlike many sequence container display functions, this
  //      code does not insert a trailing separator.
  //
  //      See Breyman (2000 p59) for the 'while' idiom.  See
  //      Stephens etal (2006 pp356-357) for stream state code.
  //
  //      One improvement might be to utilize the Boost.Io_state
  //      (i/o stream-state saver) library
  //
  //      See also: 'xeona::vectorPrint' in unit 'c/tsops'
  //
  // ---------------------------------------------------------

  template <typename T>                      // meaning a std::vector<T> container
  void
  vectorReport
  (const std::vector<T>& vec,                // CAUTION: const requires const_iterator
   const std::string&    sep = " ",          // can be "\n"
   std::ostream&         os  = std::clog)    // defaults to stdlog
  {
    std::ios::fmtflags prior = os.flags();   // save stream state
    os << std::boolalpha;                    // report bool and tribool in words
    typename std::vector<T>::const_iterator pos = vec.begin();
    if ( pos != vec.end() )
      os << *pos++;
    while ( pos != vec.end() )
      os << sep << *pos++;
    os << std::endl;
    os.flags(prior);
  }

  // ---------------------------------------------------------
  //  FREE FUNCTION   : xeona::vectorCopy <>
  // ---------------------------------------------------------
  //  Description  : copy one shared pointer vector to another
  //  Role         : as required
  //  Techniques   : defined in header to simplify template instantiation, 'std::copy'
  //  Status       : complete
  //
  //  Design notes
  //
  //      This function will silently overwrite the 'target'
  //      vector.
  //
  //      Regarding STL headers, 'std::copy' is from <algorithm>
  //      and 'std::back_inserter' is from <iterator>.
  //
  // ---------------------------------------------------------

  template <typename T>
  int                                        // size of new vector
  vectorCopy
  (shared_ptr<std::vector<T> > source,
   shared_ptr<std::vector<T> > target)
  {
    const int len = source->size();
    target->clear();                         // remove existing elements
    target->reserve(len);                    // useful but not mandatory
    std::copy(source->begin(), source->end(), std::back_inserter(*target));
    return target->size();
  }

  // ---------------------------------------------------------
  //  FREE FUNCTION   : xeona::vectorAbs <>
  // ---------------------------------------------------------
  //  Description  : return an absolute valued vector
  //  Role         : as required
  //  Techniques   : defined in header to simplify template instantiation
  //  Status       : complete
  //
  //  Design notes
  //
  //      Works as expected if given an empty vector.
  //
  //  Background
  //
  //      list    : comp.lang.c++
  //      subject : std::transform container => std::abs(container)
  //
  // ---------------------------------------------------------

  template <typename T>
  shared_ptr<std::vector<T> >
  vectorAbs
  (shared_ptr<std::vector<T> > input)
  {
    shared_ptr<std::vector<T> > abs(new std::vector<T>(*input));
    T (*absolute)(T) = static_cast<T (*)(T)>(std::abs);     // also = &std::abs
    std::transform(abs->begin(),
                   abs->end(),
                   abs->begin(),
                   absolute);
    return abs;
  }

  // ---------------------------------------------------------
  //  FREE FUNCTION   : makeNullShared_ptr <>
  // ---------------------------------------------------------
  //  Description  : makes a null shared pointer
  //  Role         : as required
  //  Techniques   : fully defined in header to avoid template instantiation problems
  //  Status       : complete
  //
  //  Note explicit calls (useful for static data initialization)
  //
  //      empty : shared_ptr<T>::shared_ptr()
  //      null  : shared_ptr<T>::shared_ptr(static_cast<T*>(0))
  //
  // ---------------------------------------------------------

  template <typename T>
  inline
  shared_ptr<T>
  makeNullShared_ptr()
  {
    // CAUTION: g++ requires "typename" for templated version

    return typename shared_ptr<T>::shared_ptr(static_cast<T*>(0));
  }

  // ---------------------------------------------------------
  //  FREE FUNCTION   : reportShared_ptr <>
  // ---------------------------------------------------------
  //  Description  : report on pointer status
  //  Role         : testing
  //  Techniques   : fully defined in header to avoid template instantiation problems
  //  Status       : complete
  //
  //  Design notes
  //                        use_count()  unique()  get()    bool test
  //    smart pointer ..    -----------------------------------------
  //      which is empty           0        false     0         false
  //      holding null pointer     1        true      0         false
  //      not copied               1        true      0x000     true
  //      with copies            > 1        false     0x000     true
  //
  //  Output
  //
  //      empty null single multiple(n)
  //
  // ---------------------------------------------------------

  template <typename T>
  std::string
  reportShared_ptr
  (const shared_ptr<T>& sp)
  {
    std::ostringstream oss;
    switch ( sp.use_count() )
      {
      case 0:
        oss << "empty";
        break;
      case 1:
        if ( sp.get() == 0 )
          oss << "null";
        else
          oss << "single";
        break;
      default:
        oss << "multiple(" << sp.use_count() << ")";
        break;
      }
    return oss.str();
  }

  // ---------------------------------------------------------
  //  FREE FUNCTION   : xeona::getSystemTimerResol
  // ---------------------------------------------------------

  long                                       // C++ standard guarantees 2147483647
  getSystemTimerResol();                     // Linux 2.6 64-bit times microseconds

  // ---------------------------------------------------------
  //  FREE FUNCTION   : xeona::formatDuration
  // ---------------------------------------------------------

  std::string
  formatDuration
  (const boost::posix_time::time_duration& delta);

  // ---------------------------------------------------------
  //  FREE FUNCTION   : xeona::logDirect <>
  // ---------------------------------------------------------
  //  Description  : approximately mimics a 'Logger::repx' call
  //  Role         : in circumstances where a normal call might be problematic
  //  Techniques   : fully defined in header to avoid template instantiation problems,
  //                 preprocessor macros
  //  Status       : complete
  //
  //  Typical usage
  //
  //      xeona::logDirect(__FILE__, __LINE__, __func__, "destructor call", 1.23456789);
  //
  //    gives:
  //
  //      |  84   frag-log-direct-1.cc  main                --              NONE [slash]
  //      |   destructor call                      1.23
  //
  // ---------------------------------------------------------

  template <typename T>
  inline
  bool
  logDirect
  (const std::string file,                   // from preprocessor macro __FILE__
   const int         line,                   // from preprocessor macro __LINE__
   const std::string func,                   // from preprocessor macro __func__
   const std::string text,                   // message
   const T&          value,                  // comment/value
   std::ostream&     os = std::clog)         // reporting stream with default value
  {
    if ( ! os )                              // this code assumes std::clog is always open
      {
        std::string msg = "nominated ostream not open, see";
        logDirect(__FILE__, __LINE__, __func__, msg, line + "  " + file);
        return false;
      }

    const std::string rank = "FUNC";         // 4 chars like a 'logga::Rank' string
    const unsigned TERM    = xeona::consoleWidth;      // set in "common.h"

    std::string macs;                        // duly formatted preprocessor macros
    std::string info;                        // text and value information
    std::string cons;                        // actual line of console output

    std::ostringstream ssValue;
    ssValue << std::fixed                    // floats only: never use scientific format
           << std::setprecision(2);          // .. and always print 2 decimal places
    ssValue << value;                        // utilize std::ostream conversions

    macs = boost::str(boost::format("%4s   %-15s  %s")    % line % file % func   );
    info = boost::str(boost::format("%-36s %s")           % text % ssValue.str() );
    cons = boost::str(boost::format("%-48s -- %17s   %s") % macs % rank % info   );

    if ( cons.length() > TERM )              // truncate if necessary
      {
        cons.resize(TERM - 2);
        cons += " >";                        // add truncation symbol
      }

    os << cons << "\n";                      // stream the collated information

    return true;
  }

  // ---------------------------------------------------------
  //  FREE FUNCTION   : xeona::reverseSequence <>
  // ---------------------------------------------------------
  //  Description  : reverse and return a sequence container by value
  //  Role         : general, tested using 'std::vector' 'std::deque' 'std::list'
  //  Techniques   : fully defined in header to avoid template instantiation problems
  //  Headers      : <algorithm>, <iterator>
  //  Status       : complete
  //
  //  Design notes
  //
  //      Simple function to reverse the order in sequence
  //      containers and return an 'rvalue'.
  //
  //      Implicit template instantiation is sufficient in many (if
  //      not all) circumstances.
  //
  //      This call can be used in 'BOOST_FOREACH' constructs to
  //      traverse backwards (unless your compiler is too old):
  //
  //          BOOST_FOREACH( shared_ptr<FullEntity> fe,
  //                         xeona::reverseSequence(d_FullEntitys) )
  //            {
  //              fe->doSomething();
  //            }
  //
  //      The 'reverse_copy' function template from <algorithm> is
  //      used with the 'back_inserter' function template from
  //      <iterator>.  This application is demonstrated in Josuttis
  //      (1999 p364) with the comment "use back_inserter to insert
  //      instead of overwrite".  For more information on the
  //      'back_inserter' see Lischner (2003 p538) and Stephens
  //      etal (2006 pp266-267).
  //
  //  CAUTION: supported sequence containers
  //
  //      tested using     : vector, deque, list
  //      also works       : string (holding chars) (pseudo container)
  //      not tested using : valarray (pseudo container)
  //
  // ---------------------------------------------------------

  template <typename S>                      // 'S' is a specialized sequence container
  S                                          // return an 'rvalue' of type 'S'
  reverseSequence
  (const S& forward)
  {
    S reverse;
    std::reverse_copy(forward.begin(),                 // refer <algorithm>
                      forward.end(),
                      std::back_inserter(reverse));    // refer <iterator>
    return reverse;
  }

  // ---------------------------------------------------------
  //  FREE FUNCTION   : xeona::tailCombine <>
  // ---------------------------------------------------------
  //  Description  : append sequence container 'tail' to 'combine' and thus modify
  //  Role         : general, specifically used to combine record subsets
  //  Techniques   : explicit template instantiations in source file
  //  Status       : complete
  // ---------------------------------------------------------

  template <typename S>                      // 'S' is a specialized sequence container
  void
  tailCombine
  (S&       combined,                        // resultant container, need not be empty
   const S& tail);                           // container to be "back inserted"

  // ---------------------------------------------------------
  //  FREE FUNCTION   : xeona::mapCombine <>
  // ---------------------------------------------------------
  //  Description  : merge map 'addition' with map 'combined' and thus modify the latter
  //  Role         : general, specifically written for registering gateways
  //  Techniques   : explicit template instantiations in source file
  //  Caution      : requires UNIQUE keys, warnings issued for duplicates
  //  Status       : complete
  // ---------------------------------------------------------

  template <typename S>
  bool                                       // 'false' indicates a problem
  mapCombine
  (S&       combined,
   const S& addition);

  // ---------------------------------------------------------
  //  FREE FUNCTION   : xeona::trimLeadingDigits
  // ---------------------------------------------------------
  //  Description  : trims leading digits from given string
  //  Role         : various, including use by 'xeona::demangle'
  //  Techniques   : C-style 'std::isdigit' from <cctype>
  //  Status       : complete
  // ---------------------------------------------------------

  std::string                                // this return may be streamed
  trimLeadingDigits
  (std::string str);                         // CAUTION: do not change to pass-by-ref

  // ---------------------------------------------------------
  //  FREE FUNCTION   : xeona::demangle
  // ---------------------------------------------------------
  //  Description  : attempts to reinterpret straightforward g++ typenames
  //  Role         : repairing typeid names from GCC RTTI
  //  Techniques   : simple reverse engineering
  //  Status       : complete
  // ---------------------------------------------------------

  std::string                                // this return may be streamed
  demangle                                   // GCC-specific for class typenames
  (std::string tid);                         // most often 'tid' is a 'const char*'

} // namespace xeona

#endif // _UTIL1_H_

//  end of file

