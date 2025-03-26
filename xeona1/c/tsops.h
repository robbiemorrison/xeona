//  XEONA ENERGY SYSTEMS MODELING ENVIRONMENT
//
//  file-create-name : tsops.h
//  file-create-date : Wed 16-Sep-2009 16:57 UTC
//  file-initiator   : Robbie Morrison <robbie@actrix.co.nz>
//
//  file-role        : overloaded operators for timeseries / header
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
//  $URL: file:///home/robbie/svn-root/xeona/futz/trunk/xeona1/c/tsops.h $
//
//  GENERAL NOTES FOR THIS FILE

// ---------------------------------------------------------
//  documentation   : Timeseries
// ---------------------------------------------------------
//
//  Introduction
//
//      This unit implements a number of free functions,
//      including overloaded operators, for manipulating
//      timeseries.  In 'xeona', a timeseries is of the form:
//
//          shared_ptr<std::vector<T> >
//
//      Which means that 'xeona' has no dedicated timeseries
//      class.  This design decision was influenced by
//      considerations of readability and run-time efficiency.
//      (Now three years on, I may have made a different choice.)
//
//      Timeseries usage needs protocols covering the choice of
//      indexing base (zero or one) and covering the location of
//      the duration midpoint in time.
//
//  Indexing
//
//      Internally, 'xeona' uses zero-based indexing because this
//      aligns best with C++ programming:
//
//          Y_t, t in { 0, ..., n-1 }
//
//      But aside from the interpretation of some logging
//      messages, the user can normally retain the more usual
//      one-based indexing.  That said, the notion of indexing is
//      often used implicitly.
//
//  Timezone mapping
//
//      Regarding timezones, 'xeona', where necessary, assumes
//      Universal Coordinated Time:
//
//          presumed time zone is UTC +0000
//
//      Users can nominate significant 'start-hour' and
//      'start-day' offsets in the 'time-horizon' entity.  In
//      which case, 'xeona' automatically assumes that the
//      supplied timeseries have been suitably truncated.  This
//      feature enables models to be built for other timezones
//      and non-start-of-year starts.
//
//  Data midpoint
//
//      The more significant issue, data-wise, is specifying
//      exactly where Y_t is located in fine-grained terms on the
//      t axis.  The following protocol is used, assuming
//      zero-based indexing and, for the sake of argument, hourly
//      data:
//
//          hourly data-point Y_0 spans [00:00 01:00]
//
//      And so on.  This means the data-point Y_0 should be line
//      plotted with an x-axis value of 00:30 (although a bar
//      plot would be generally more appropriate).
//
// ---------------------------------------------------------

//  HEADER GUARD

#ifndef _TSOPS_H_
#define _TSOPS_H_

//  AD-HOC NOTES
//
//  Code originally developed in 'frag-arith-overload-1.cc',
//  'frag-xeona-sample24-1.cc', 'frag-xeona-vec-tup-1.cc', and
//  'frag-xeona-vec-fill-1.cc'.

//  LOCAL AND SYSTEM INCLUDES

#include "../b/entity.h"      // entity base class plus lazy linking

#include "../a/logger_fwd.h"  // some forward declarations from "logger.h"
#include "../c/smart_ptr.h"   // toggle between Boost and TR1 smart pointers

#include <iostream>           // standard io
#include <stdexcept>          // standard exception classes, runtime_error()
#include <string>             // C++ strings
#include <vector>             // STL sequence container

#include <boost/tuple/tuple.hpp>   // n-tuples, ref(), cref()

//  FORWARD (PARTIAL) DECLARATIONS

namespace boost { class bad_lexical_cast; }

//  CODE

// ---------------------------------------------------------
//  notes           : non-member overloaded operators
// ---------------------------------------------------------
//
//  Operators implemented as non-member functions
//
//      Dattatri (2002 pp398-401) covers such operators.  He
//      observes:
//
//         "Any operator that does not require an l-value and is
//          commutative is better implemented as a non-member
//          function (+, -, etc).  This allows the compiler to
//          apply a conversion in case of argument mismatch for
//          the first argument." (p396)
//
//      Dattatri, Kayshav.  2002.  C++ : effective-object
//        oriented software construction : concepts, principles,
//        industrial strategies and practices -- Second edition.
//        Prentice Hall PTR, Upper Saddle River, New Jersey, USA.
//        ISBN 0-13-086769-1.
//
// ---------------------------------------------------------

// ---------------------------------------------------------
//  FREE FUNCTION   : xeona::vectorSum <>
// ---------------------------------------------------------
//  Description  : sums a numerical vector
//  Role         : general use
//  Techniques   : 'std::accumulate'
//  Status       : complete
//
//  Usage
//
//      double sum = xeona::vectorSum(vec);
//
//  Note the following alternative approach, which also makes
//  available other common statistics:
//
//      const Statistics<double> statVec(vec);
//      double sum = statVec.sum();
//
// ---------------------------------------------------------

namespace xeona
{
  template <typename T>
  T
  vectorSum
  (const std::vector<T>& one);

  template <typename T>
  T
  vectorSum
  (const shared_ptr<std::vector<T> >& one);
}

// ---------------------------------------------------------
//  FREE FUNCTION   : xeona::vectorMean <>
// ---------------------------------------------------------
//  Description  : find the mean a numerical vector
//  Role         : general use
//  Techniques   : 'vectorSum'
//  Status       : complete
//
//  Usage
//
//      double mean = xeona::vectorMean(vec);
//
//  See also the design notes for 'xeona::vectorSum'.
//
// ---------------------------------------------------------

namespace xeona
{
  template <typename T>
  T
  vectorMean
  (const std::vector<T>& one);

  template <typename T>
  T
  vectorMean
  (const shared_ptr<std::vector<T> >& one);
}

// ---------------------------------------------------------
//  FREE FUNCTION   : operator+ <>
//  FREE FUNCTION   : operator- <>
// ---------------------------------------------------------
//  Description  : operates on two numerical vector
//  Role         : general use
//  Techniques   : 'for' loop and underlying operator
//  Status       : complete
// ---------------------------------------------------------

template <typename T>
shared_ptr<std::vector<T> >
operator+
(const shared_ptr<std::vector<T> >& one,
 const shared_ptr<std::vector<T> >& two);

template <typename T>
shared_ptr<std::vector<T> >
operator-
(const shared_ptr<std::vector<T> >& one,
 const shared_ptr<std::vector<T> >& two);

// ---------------------------------------------------------
//  FREE FUNCTION   : xeona::vectorFillRandom (double)
// ---------------------------------------------------------
//  Description  : fill vector with random doubles
//  Role         : normally for testing
//  Techniques   : 'std::rand' 'std::generate_n'
//  Status       : complete
//
//  Design notes
//
//      This function uses the standard 'std::rand' call from
//      <cstdlib>, rather than the higher performing Boost
//      libraries equivalent.
//
//      The initialize function 'std::srand' is not called (which
//      implies that "std::srand(1)" was called) and means that
//      the generated values are repeatable.
//
//      'RAND_MAX' an integral constant expression whose value is
//      the maximum value returned by the 'rand' function -- its
//      value is library dependent, but granted to be at least
//      32767
//
// ---------------------------------------------------------

namespace xeona
{
  int                                             // final size of 'stub'
  vectorFillRandom
  (shared_ptr<std::vector<double> > stub,         // will be 'clear'ed
   const double                     lower,        // lower bound
   const double                     upper,        // upper bound
   const int size = Entity::getHorizonSteps());   // number of elements
}

// ---------------------------------------------------------
//  FREE FUNCTION   : xeona::vectorFillIncrement (double)
// ---------------------------------------------------------
//  Description  : fill vector with random doubles
//  Role         : normally for testing
//  Techniques   : 'std::generate_n'
//  Status       : complete
// ---------------------------------------------------------

namespace xeona
{
  int                                             // final size of 'stub'
  vectorFillIncrement
  (shared_ptr<std::vector<double> > stub,         // will be 'clear'ed
   const double                     start,        // start value
   const double                     step,         // step size
   const int size = Entity::getHorizonSteps());   // number of elements
}

// ---------------------------------------------------------
//  FREE FUNCTION   : xeona::vectorFillDiurnal (double)
// ---------------------------------------------------------
//  Description  : fill vector with diurnal (day-cyclic) data plus optional noise
//  Role         : normally for testing
//  Techniques   : 'std::generate_n' 'std::sin' 'std::rand'
//  Status       : complete
//
//  Design notes
//
//      Diurnal, in this case, means day-cyclic.  This function
//      starts with a one day periodic sin wave and optionally
//      superimposes gaussian noise.
//
//      The 'std::sin' function takes radians.
//
//      This function uses the standard 'std::rand' call from
//      <cstdlib>, rather than the higher performing Boost
//      equivalent.
//
//      The initialize function 'std::srand' is not called, which
//      implies "std::srand(1)" was called, and the generated
//      values are repeatable.
//
//      'RAND_MAX' an integral constant expression whose value is
//      the maximum value returned by the 'rand' function -- its
//      value is library dependent, but granted to be at least
//      32767
//
// ---------------------------------------------------------

namespace xeona
{
  int                                             // final size of 'stub'
  vectorFillDiurnal
  (shared_ptr<std::vector<double> > stub,         // will be 'clear'ed
   const double                     mean,         // mean value (before noise)
   const double                     amplitude,    // peak-to-peak measure (before noise)
   const int                        offset,       // temporal shift in hours [1]
   const double                     randomness,   // noise relative to amplitude [2]
   const int                        resolution,   // samples per day
   const int size = Entity::getHorizonSteps());   // number of elements

  // [1] starts at midnight, zero indicates no shift, negative means start earlier
  // [2] maximum noise relative to amplitude, set to zero to disable
}

// ---------------------------------------------------------
//  FREE FUNCTION   : xeona::vectorPrint <>
// ---------------------------------------------------------
//  Description  : print a vector
//  Role         : normally for testing
//  Techniques   : 'std::copy' 'std::ostream_iterator'
//  Status       : complete
//
//  Design notes
//
//      Currently 'T' in { int double bool }.
//
//      If 'msg' string is not empty, a " : " is appended.
//
//      The element 'separator' defaults to a space.
//
//      This function does not empty 'os' first, rather it adds
//      to the current stream.  Moreover, it does not change the
//      ios state of the stream.
//
// ---------------------------------------------------------

namespace xeona
{
  template <typename T>
  void
  vectorPrint
  (const shared_ptr<std::vector<T> > vec,    // vector to print
   const std::string&                msg       = "",
   std::ostream&                     os        = std::cout,
   const std::string&                separator = " ",
   const std::string&                final     = "\n");
}

// ---------------------------------------------------------
//  FREE FUNCTION   : xeona::vector2TupleExtract0 <>
//  FREE FUNCTION   : xeona::vector2TupleExtract1 <>
// ---------------------------------------------------------
//  Description  : extract normal vector from vector of uniform type 2-tuples
//  Role         : general use
//  Techniques   : 'Boost.Tuple', 'Boost.Foreach'
//  Status       : complete
//
//  Design notes
//
//      These free functions have explicit template
//      instantiations in the implementation file.  If necessary,
//      this list can be extended.
//
//      These functions assume the same type T for both elements.
//
//  CAUTION
//
//      Note 'boost::tuples' for the namespace and 'boost::tuple'
//      for the class.  Note also the need to use the fully
//      qualified free function 'get' and and not the member
//      function 'get' (the member function delegates to the free
//      function in any case).
//
// ---------------------------------------------------------

namespace xeona
{
  template <typename T>
  shared_ptr<std::vector<T> >
  vector2TupleExtract0                       // select first element
  (const shared_ptr<std::vector<boost::tuple<T, T> > > input);

  template <typename T>
  shared_ptr<std::vector<T> >
  vector2TupleExtract1                       // select second element
  (const shared_ptr<std::vector<boost::tuple<T, T> > > input);

} // namespace 'xeona'

// ---------------------------------------------------------
//  FREE FUNCTION   : xeona::vector2TupleZip <>
// ---------------------------------------------------------
//  Description  : zip two same type normal vectors into a 2-tuple vector
//  Role         : general use
//  Techniques   : 'Boost.Tuple', 'for' loop
//  Status       : complete
//
//  Design notes
//
//      The input vectors must be the same length.
//
// ---------------------------------------------------------

namespace xeona
{
  template <typename T>
  shared_ptr<std::vector<boost::tuple<T, T> > >   // empty on failure
  vector2TupleZip
  (const shared_ptr<std::vector<T> > left,
   const shared_ptr<std::vector<T> > right);

}

// ---------------------------------------------------------
//  FREE FUNCTION   : xeona::stringParse   <>
//  FREE FUNCTION   : xeona::stringParseSP <>
// ---------------------------------------------------------
//  Description  : convert string containing numbers to a numerical vector
//  Role         : general use
//  Techniques   : Boost.String_algo and Boost.Conversion libraries, exceptions
//  Status       : complete
//
//  Design notes
//
//       The 'boost::token_compress_on' parameter is set -- which
//       means that adjacent tokens will be treated as one.
//
//       For CSV (comma-separated variables) data which may also
//       contain spaces, be sure to use ", " as the separator
//       string.
//
//       This template function is explicitly instantiated as
//       follows: T in {unsigned int double}
//
//  Usage
//
//      Assuming that exceptions will be caught and handled:
//
//          #include <boost/lexical_cast.hpp>
//
//          const std::string sep = " ";
//          const std::string input = "1.2 3.4 5.6";
//          shared_ptr<std::vector<double> > vec;
//          try
//            {
//              vec = xeona::stringParseSP<double>(input, sep);
//            }
//          catch( boost::bad_lexical_cast& e )
//            {
//              // handle here
//            }
//
// ---------------------------------------------------------

namespace xeona
{
  template <typename T>
  std::vector<T>
  stringParse                                // returns vector
  (const std::string& input,
   const std::string  sep = " ")             // concatenation of chars, not a string
    throw(boost::bad_lexical_cast);          // exception specification

  template <typename T>
  shared_ptr<std::vector<T> >
  stringParseSP                              // returns smart pointer
  (const std::string& input,
   const std::string  sep = " ")             // concatenation of chars, not a string
    throw(boost::bad_lexical_cast);          // exception specification

} // namespace 'xeona'

#endif // _TSOPS_H_

//  end of file

