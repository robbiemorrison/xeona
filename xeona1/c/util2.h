//  XEONA ENERGY SYSTEMS MODELING ENVIRONMENT
//
//  file-create-name : util2.h
//  file-create-date : Thu 06-Nov-2008 16:53 UTC
//  file-initiator   : Robbie Morrison <robbie@actrix.co.nz>
//
//  file-role        : free functions which offer general utilities 2 / header
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
//  $URL: file:///home/robbie/svn-root/xeona/futz/trunk/xeona1/c/util2.h $
//
//  GENERAL NOTES FOR THIS FILE
//
//  This unit provides free functions and classes to support:
//
//    geometric progression   : transform integer to geometric progression, ditto no zeros
//    integer                 : is two-contained
//    smart pointers          : poppable container class
//    objective functions     : inner-product while honoring constant (shift) term
//    floating point numbers  : NaN, inf, finite (not Nan nor inf), integer-valued tests
//    sequence containers     : reorder based on supplied numeric vector

//  HEADER GUARD

#ifndef _UTIL2_H_
#define _UTIL2_H_

//  LOCAL AND SYSTEM INCLUDES

#include "../a/logger_fwd.h"  // some forward declarations from "logger.h"
#include "../c/smart_ptr.h"   // toggle between Boost and TR1 smart pointers

#include <algorithm>          // STL copying, searching, and sorting, max()
#include <deque>              // STL sequence container, double-ended vector
#include <limits>             // numeric_limits<T>::infinity() and similar
#include <sstream>            // string-streams
#include <stdexcept>          // standard exception classes, runtime_error()
#include <string>             // C++ strings
#include <vector>             // STL sequence container

#include <cmath>              // C-style maths, abs(), ceil(), floor(), sqrt()

#include <boost/logic/tribool.hpp>      // three state boolean
#include <boost/logic/tribool_io.hpp>   // CAUTION: essential for std::boolalpha reporting
#include <boost/math/special_functions/fpclassify.hpp> // floating point classification

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
  (const int aggregate);

  // ---------------------------------------------------------
  //  FREE FUNCTION   : xeona::isTwoContained
  // ---------------------------------------------------------
  //  Description  : test if 'candidate' is a power-of-two sub-sequence of 'aggregate'
  //  Role         : used for checking commitment strategy codes
  //  On success   : returns 'true' if 'candidate' is contained in 'aggregate'
  //  On fail      : returns 'indeterminate' for faulty input
  //  Techniques   : relies on 'xeona::geometricProgression'
  //  Status       : complete
  //
  //  Design notes
  //
  //      Note 'pure' must be a power-of-two greater than zero.
  //      But 'aggregate' may be less than 'pure' and can be zero
  //
  // ---------------------------------------------------------

  tribool                                    // 'indeterminate' means faulty input
  isTwoContained
  (const int candidate,                      // greater than zero
   const int aggregate);                     // non-negative, can be less than 'candidate'

  // ---------------------------------------------------------
  //  FREE FUNCTION   : xeona::reducedVector (string return)
  // ---------------------------------------------------------
  //  Description  : returns string
  //  Role         :
  //  Techniques   : (nothing special)
  //  Status       : complete
  // ---------------------------------------------------------

  std::string                                // no trailing newline
  reducedVector
  (const int         aggregate,
   const std::string separator = " ");

} // namespace 'xeona'

// ---------------------------------------------------------
//  CLASS           : SmartPtrPopper <>
// ---------------------------------------------------------
//  Description  : small popable container class that uses smart pointers
//  Role         : to assist 'capsolve' algorithm
//  Techniques   : 'std::deque', header implementation to simplify template instantiation
//  Status       : complete
// ---------------------------------------------------------

template <typename E>
class SmartPtrPopper
{
  // DISABLED

private:

  SmartPtrPopper(const SmartPtrPopper& orig);               // copy constructor
  SmartPtrPopper& operator= (const SmartPtrPopper& orig);   // copy assignment operator

  // CREATORS

public:

  SmartPtrPopper();

  // ACCESSORS

  int
  size();

  bool                                       // similar to 'std::vector::empty'
  empty();

  // MANIPULATORS

  int
  load
  (std::vector<shared_ptr<E> > spvec);

  int
  push                                       // similar to a returning 'push_front'
  (shared_ptr<E> sp);

  shared_ptr<E>                              // returns empty pointer if deque is empty
  pop();                                     // similar to a returning 'pop_front'

private:

  std::deque<shared_ptr<E> >    d_spdeq;

};

// the 'SmartPtrPopper' class is implemented in the header in
// order to avoid explicit template instantiations

// CREATORS

template <typename E>
SmartPtrPopper<E>::SmartPtrPopper() :
  d_spdeq()
{
}

// ACCESSORS

template <typename E>
int
SmartPtrPopper<E>::size()
{
  return d_spdeq.size();
}

template <typename E>
bool
SmartPtrPopper<E>::empty()
{
  return d_spdeq.empty();
}

// MANIPULATORS

template <typename E>
int
SmartPtrPopper<E>::load
(std::vector<shared_ptr<E> > spvec)
{
  d_spdeq.clear();                           // empty all elements
  std::copy(spvec.begin(),                   //  refer <algorithm>, note 'reverse_copy'
            spvec.end(),
            std::back_inserter(d_spdeq));    // refer <iterator>
  return d_spdeq.size();
}

template <typename E>
int
SmartPtrPopper<E>::push
(shared_ptr<E> sp)
{
  d_spdeq.push_front(sp);
  return size();
}

template <typename E>
shared_ptr<E>
SmartPtrPopper<E>::pop()
{
  if ( d_spdeq.empty() )
    {
      return shared_ptr<E>();                // empty shared pointer
    }
  else
    {
      shared_ptr<E> temp = d_spdeq.front();  // grab first element
      d_spdeq.pop_front();                   // remove first element
      return temp;
    }
}

// ---------------------------------------------------------
//  CLASS           : NormalPopper <>
// ---------------------------------------------------------
//  Description  : small popable container class that used normal types
//  Role         : to assist 'interface' algorithm
//  Techniques   : 'std::deque', header implementation to simplify template instantiation
//  Status       : complete
//  On failure   : throws 'std::out_of_range' exception
// ---------------------------------------------------------

template <typename E>
class NormalPopper
{
  // DISABLED

private:

  NormalPopper(const NormalPopper& orig);              // copy constructor
  NormalPopper& operator= (const NormalPopper& orig);  // copy assignment operator

  // CREATORS

public:

  NormalPopper();

  // ACCESSORS

  int
  size();

  bool
  empty();

  // MANIPULATORS

  int
  load
  (std::vector<E> vec);

  std::vector<E>
  dump();

  int
  push                                       // similar to a returning 'push_font'
  (E sp);

  E                                          // returns empty pointer if deque is empty
  pop()                                      // similar to a returning 'pop_front'
    throw(std::out_of_range);                // exception specification

private:

  std::deque<E>    d_deq;

};

// CREATORS

template <typename E>
NormalPopper<E>::NormalPopper() :
  d_deq()
{
}

// ACCESSORS

template <typename E>
int
NormalPopper<E>::size()
{
  return d_deq.size();
}

template <typename E>
bool
NormalPopper<E>::empty()
{
  return d_deq.empty();
}

// MANIPULATORS

template <typename E>
int
NormalPopper<E>::load
(std::vector<E> vec)
{
  d_deq.clear();                             // empty all elements
  std::copy(vec.begin(),                     // refer <algorithm>, note 'reverse_copy'
            vec.end(),
            std::back_inserter(d_deq));      // refer <iterator>
  return d_deq.size();
}

template <typename E>
std::vector<E>
NormalPopper<E>::dump()
{
  std::vector<E> temp = d_deq;
  d_deq.clear();
  return temp;
}

template <typename E>
int
NormalPopper<E>::push
(E element)
{
  d_deq.push_front(element);
  return size();
}

template <typename E>
E
NormalPopper<E>::pop()
  throw(std::out_of_range)                   // exception specification
{
  if ( d_deq.empty() )
    {
#if 1 // 1 = throw, 0 = return default constructed
      throw std::out_of_range("attempt to pop an empty NormalPopper object");
#else
      return E();                            // default constructed
#endif // 0

    }
  else
    {
      const E temp = d_deq.front();          // grab first element
      d_deq.pop_front();                     // remove first element
      return temp;
    }
}

// ---------------------------------------------------------
//  FREE FUNCTION   : xeona::coeffProduct <>
// ---------------------------------------------------------
//  Description  : calculate an inner product while honoring constant term
//  Role         : calculating objective results without re-calling the solver
//  Techniques   : 'std::inner_product'
//  Status       : complete
//
//  Equation
//
//      answer = a_0 + sum( a_i * x_i )   for i in { 1, .., n }
//
//          with x_0 ignored for calculation purposes
//          and the vectors a and x of length n+1
//
// ---------------------------------------------------------

namespace xeona
{
  template<typename N>                       // implicit instantiation supported
  N
  coeffProduct
  (const std::vector<N> a,                   // coefficients vector
   const std::vector<N> x);                  // variable values vector

} // namespace 'xeona'

// ---------------------------------------------------------
//  FREE FUNCTION   : xeona::isNan    <> (scalar)
//  FREE FUNCTION   : xeona::isNanVec <> (vector)
// ---------------------------------------------------------
//  Description  : checks for IEEE 754 NaN
//  Role         : general usage
//  Techniques   : implicit template instantiation, 'boost::math::isnan'
//  Status       : complete
//
//  Design notes
//
//      NaN is floating point not-a-number.  Here are some ways
//      to make NaNs:
//
//          0.0/0.0
//          std::sqrt(-1.0)                           // <cmath>
//          std::numeric_limits<double>::quiet_NaN()  // refer <limits>
//
//      NaN's are equal to nothing, not even themselves.
//      Moreover NaN's have no sign.
//
//      Note also: 'std::isnan' from <cmath>.
//
//  Usage
//
//      if ( xeona::isNan(value) )  // a NaN
//
//  CAUTION: potential compile-time conflict with 'isnan' macro
//
//      Moreover the Boost documentation (1.48.0) notes that the
//      following will cause a compiler error if "isnan" is a
//      native macro:
//
//          boost::math::isnan(x)
//
//      So instead always use:
//
//          (boost::math::isnan)(x)
//
//     This advice is not deployed here in the interests of
//     readability!
//
// ---------------------------------------------------------

namespace xeona
{
  template<typename T>
  bool                                       // 'true' if NaN
  isNan
  (const T number)
    throw(std::domain_error)                 // exception specification
  {
    // abort if 'T' is an integral type
    if ( std::numeric_limits<T>::is_integer )
      {
        std::ostringstream oss;
        oss << "caller passed a non-floating point type of value: " << number;
        throw std::domain_error(oss.str());
      }
    // main code
    return boost::math::isnan(number);
  }

  template<typename T>
  unsigned                                   // '0' if entire vector contains no 'NaN's
  isNanVec
  (const std::vector<T> numbers)             // vector version
    throw(std::domain_error)                 // exception specification
  {
    const unsigned cnt = std::count_if(numbers.begin(),
                                       numbers.end(),
                                       xeona::isNan<T>);
    return cnt;
  }
} // namespace 'xeona'

// ---------------------------------------------------------
//  FREE FUNCTION   : xeona::isInf    <> (scalar)
//  FREE FUNCTION   : xeona::isInfVec <> (vector)
// ---------------------------------------------------------
//  Description  : checks for IEEE 754 inf
//  Role         : general usage
//  Techniques   : implicit template instantiation, 'boost::math::isinf'
//  Status       : complete
//
//  Design notes
//
//      inf is floating point infinity.
//
//      Unlike NaN's, inf's can be compared and posses a sign.
//
//      Note also: 'std::isinf' from <cmath>.
//
//  Usage
//
//      if ( xeona::isInf(value) )  // an +inf or -inf
//
// ---------------------------------------------------------

namespace xeona
{
  template<typename T>
  bool                                       // true if +'inf' or -'inf'
  isInf
  (const T number)
    throw(std::domain_error)                 // exception specification
  {
    // abort if 'T' is an integral type
    if ( std::numeric_limits<T>::is_integer )
      {
        std::ostringstream oss;
        oss << "caller passed a non-floating point type of value: " << number;
        throw std::domain_error(oss.str());
      }
    // main code
    return ( boost::math::isinf(number) );
  }

  template<typename T>
  unsigned                                   // '0' if entire vector contains no 'inf's
  isInfVec
  (const std::vector<T> numbers)             // vector version
    throw(std::domain_error)                 // exception specification
  {
    const unsigned cnt = std::count_if(numbers.begin(),
                                       numbers.end(),
                                       xeona::isInf<T>);
    return cnt;
  }
} // namespace 'xeona'

// ---------------------------------------------------------
//  FREE FUNCTION   : xeona::isNotFinite    <> (scalar)
//  FREE FUNCTION   : xeona::isNotFiniteVec <> (vector)
// ---------------------------------------------------------
//  Description  : checks for absence of IEEE 754 inf and NaN
//  Role         : general usage
//  Techniques   : implicit template instantiation, 'boost::math::isfinite'
//  Status       : complete
//
//  Usage
//
//      if ( xeona::isNotFinite(value) )  // a +inf or -inf or NaN
//
// ---------------------------------------------------------

namespace xeona
{
  template<typename T>
  bool                                       // 'true' if not finite
  isNotFinite
  (const T number)
    throw(std::domain_error)                 // exception specification
  {
    // abort if 'T' is an integral type
    if ( std::numeric_limits<T>::is_integer )
      {
        std::ostringstream oss;
        oss << "caller passed a non-floating point type of value: " << number;
        throw std::domain_error(oss.str());
      }
    // main code
    return ( ! boost::math::isfinite(number) );   // CAUTION: logical negation operator
  }

  template<typename T>
  unsigned                                   // '0' if entire vector is finite
  isNotFiniteVec
  (const std::vector<T> numbers)             // vector version
    throw(std::domain_error)                 // exception specification
  {
    const unsigned cnt = std::count_if(numbers.begin(),
                                       numbers.end(),
                                       xeona::isNotFinite<T>);
    return cnt;
  }
} // namespace 'xeona'

// ---------------------------------------------------------
//  FREE FUNCTION   : xeona::isIntegerValued (double)
// ---------------------------------------------------------
//  Description  : checks for integer-valued floating point number
//  Role         : general usage
//  Techniques   : 'trunc' (requires C99)
//  Status       : complete
// ---------------------------------------------------------

namespace xeona
{
  bool                                       // 'true' if integer-valued
  isIntegerValued
  (const double test);
}

// ---------------------------------------------------------
//  FREE FUNCTION   : xeona::isOdd (integral types)
// ---------------------------------------------------------
//  Description  : checks if integer 'test' is even
//  Role         : general usage
//  Techniques   : %' modulo operator
//  Status       : complete
// ---------------------------------------------------------

namespace xeona
{
  template<typename I>                       // I in { 'int' 'unsigned' }
  bool                                       // 'true' if even, else odd
  isEven
  (const I test);
}

// ---------------------------------------------------------
//  FREE FUNCTION   : xeona::reorderUpwards <>
// ---------------------------------------------------------
//  Description  : reorder 'data' based on 'ordering', utilizes 'std::sort'
//  Role         : 'AsopLmpBidAdaptive1::establish'
//  Techniques   : 'std::sort' from <algorithm>
//  Status       : complete
//
//  Usage
//
//     Type 'S' would usually be an 'int' or 'double' and type
//     'T' a user type like 'TechnicalAsset'.  These types need
//     to be explicitly instantiated in the implementation file.
//
//  Example (using initializer lists)
//
//     std::vector<double>               ordering = { 2.2, 4.4, 1,1 };
//     std::vector<shared_ptr<MyClass> > myObs    = { A, B, C };
//     xeona::reorderUpwards(ordering, myObs);
//     std::reverse(myObs.begin(), myObs.end());  // 'myObs' now: { B, A, C }
//
// ---------------------------------------------------------

namespace xeona
{
  template <typename S,                      // must support 'operator<'
            typename T>                      // no technical restrictions
  bool                                       // 'false' on vector length mismatch
  reorderUpwards
  (const std::vector<S>&        ordering,    // unsorted
   std::vector<shared_ptr<T> >& data);       // reorder by increasing 'ordering'

} // namespace 'xeona'

#endif // _UTIL2_H_

//  end of file

