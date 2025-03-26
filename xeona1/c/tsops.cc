//  XEONA ENERGY SYSTEMS MODELING ENVIRONMENT
//
//  file-create-name : tsops.cc
//  file-create-date : Wed 16-Sep-2009 16:57 UTC
//  file-initiator   : Robbie Morrison <robbie@actrix.co.nz>
//
//  file-role        : overloaded operators for timeseries / implementation
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
//  $URL: file:///home/robbie/svn-root/xeona/futz/trunk/xeona1/c/tsops.cc $
//
//  GENERAL NOTES FOR THIS FILE

//  LOCAL AND SYSTEM INCLUDES

#include "tsops.h"            // companion header for this file (place first)

#include "../c/util3.h"       // free functions for floating point comparison
#include "../c/util2.h"       // free functions which offer general utilities 2

#include "../a/logger.h"      // standard logging functionality (as required)
#include "../c/smart_ptr.h"   // toggle between Boost and TR1 smart pointers
#include ".././common.h"      // common definitions for project (place last)

#include <algorithm>          // STL copying, searching, and sorting, max()
#include <deque>              // STL sequence container, double-ended vector
#include <iterator>           // STL additional iterators, std::distance()
#include <numeric>            // STL numerical algorithms
#include <sstream>            // string-streams
#include <vector>             // STL sequence container

#include <cmath>              // C-style maths, abs(), ceil(), floor(), sqrt()
#include <cstdlib>            // C-style exit(), getenv(), system(), NULL, EXIT_SUCCESS

#include <boost/algorithm/string.hpp>             // string recasing, trimming, splitting
#include <boost/assign/std/vector.hpp>            // assign operator+=() for std::vector
#include <boost/foreach.hpp>                      // BOOST_FOREACH iteration macro
#include <boost/format.hpp>                       // printf style formatting
#include <boost/lexical_cast.hpp>                 // lexical_cast<> string to number conv
#include <boost/math/constants/constants.hpp>     // high-precision mathematical constants
#include <boost/math/special_functions/round.hpp> // round to closest integer
#include <boost/math/special_functions/trunc.hpp> // truncate to integer

//  NAMESPACE DECLARATIONS

using namespace boost::assign;               // for: vec += 0, 1;

//  FILE-LOCAL GLOBAL CONSTANTS

namespace
{
  const double pi     = boost::math::constants::pi<double>();
  const double twopi  = 2.0 * pi;
  const double halfpi = 0.5 * pi;
}

//  CODE

// ---------------------------------------------------------
//  FREE FUNCTION   : xeona::vectorSum <>
// ---------------------------------------------------------

namespace xeona
{
  template <typename T>
  T
  vectorSum
  (const std::vector<T>& one)
  {
    // active code
    const T zero(0);
    const T sum = std::accumulate(one.begin(),    // refer <numeric>
                                  one.end(),
                                  zero);          // initial value

    // return
    return sum;

  } // function 'xeona::vectorSum <>'

  template <typename T>
  T
  vectorSum
  (const shared_ptr<std::vector<T> >& one)
  {
    // logging support
    static logga::spLogger logger = logga::ptrLogStream();

    // integrity checks
    if ( ! one )
      {
        logger->repx(logga::warn, "supplied vector empty/null", "");
        return T();
      }

    // active code
    const T zero(0);
    const T sum = std::accumulate(one->begin(),   // refer <numeric>
                                  one->end(),
                                  zero);          // initial value

    // return
    return sum;

  } // function 'xeona::vectorSum <>'

} // namespace 'xeona'

// ---------------------------------------------------------
//  FREE FUNCTION   : xeona::vectorMean <>
// ---------------------------------------------------------

namespace xeona
{
  template <typename T>
  T
  vectorMean
  (const std::vector<T>& one)
  {
    // logging support
    static logga::spLogger logger = logga::ptrLogStream();

    // integrity checks
    if ( one.empty() )
      {
        logger->repx(logga::warn, "empty vector supplied", one.size());
        return T();                          // default contruction value, probably zero
      }

    // active code
    const int len  = one.size();
    const T   sum  = xeona::vectorSum<T>(one);
    const T   mean = sum / len;

    // return
    return mean;
  }

  template <typename T>
  T
  vectorMean
  (const shared_ptr<std::vector<T> >& one)
  {
    // logging support
    static logga::spLogger logger = logga::ptrLogStream();

    // integrity checks
    if ( ! one )
      {
        logger->repx(logga::warn, "supplied vector empty/null", "");
        return T();                          // default contruction value, probably zero
      }
    if ( one->empty() )
      {
        logger->repx(logga::warn, "empty vector supplied", one->size());
        return T();                          // default contruction value, probably zero
      }

    // active code
    const int len  = one->size();
    const T   sum  = xeona::vectorSum<T>(one);
    const T   mean = sum / len;

    // return
    return mean;
  }
}

// ---------------------------------------------------------
//  FREE FUNCTION   : operator+ <>
// ---------------------------------------------------------

template <typename T>
shared_ptr<std::vector<T> >
operator+
(const shared_ptr<std::vector<T> >& one,
 const shared_ptr<std::vector<T> >& two)
{
  // logging support
  static logga::spLogger logger = logga::ptrLogStream();

  // integrity checks
  if ( ! one || ! two )
    {
      logger->repx(logga::warn, "supplied vector(s) empty/null", "");
      return shared_ptr<std::vector<T> >();
    }
  const int lenOne = one->size();
  const int lenTwo = two->size();
  if ( lenOne != lenTwo )
    {
      std::ostringstream oss;
      oss << lenOne << " : " << lenTwo;
      logger->repx(logga::warn, "vector length mismatch, one : two", oss.str());
      return shared_ptr<std::vector<T> >();
    }

  // active code
  shared_ptr<std::vector<T> > buf(new std::vector<T>());
  for ( int i = 0; i < lenOne; ++i )
    {
      buf->push_back(one->at(i) + two->at(i));    // note the underlying operator is here
    }

  // return
  return buf;

} // function 'operator+ <>'

// ---------------------------------------------------------
//  FREE FUNCTION   : operator- <>
// ---------------------------------------------------------

template <typename T>
shared_ptr<std::vector<T> >
operator-
(const shared_ptr<std::vector<T> >& one,
 const shared_ptr<std::vector<T> >& two)
{
  // logging support
  static logga::spLogger logger = logga::ptrLogStream();

  // integrity checks
  if ( ! one || ! two )
    {
      logger->repx(logga::warn, "supplied vector(s) empty/null", "");
      return shared_ptr<std::vector<T> >();
    }
  const int lenOne = one->size();
  const int lenTwo = two->size();
  if ( lenOne != lenTwo )
    {
      std::ostringstream oss;
      oss << lenOne << " : " << lenTwo;
      logger->repx(logga::warn, "vector length mismatch, one : two", oss.str());
      return shared_ptr<std::vector<T> >();
    }

  // active code
  shared_ptr<std::vector<T> > buf(new std::vector<T>());
  for ( int i = 0; i < lenOne; ++i )
    {
      buf->push_back(one->at(i) - two->at(i));    // note the underlying operator is here
    }

  // return
  return buf;

} // function 'operator- <>'

// ---------------------------------------------------------
//  FREE FUNCTION   : xeona::vectorFillRandom
// ---------------------------------------------------------
//
//  Design notes
//
//      Function 'std::rand' returns a pseudo-random integer in
//      the range 0 to 'RAND_MAX', inclusive.  'RAND_MAX' is
//      implementation-dependent.
//
//      The default seed for 'std::srand' is 1.  Not explicitly
//      calling 'std::srand' acts as if the default call is made.
//
// ---------------------------------------------------------

namespace xeona
{
  class functorRandom
  {
  public:

    functorRandom
    (const double lower,                     // lower bound
     const double upper) :                   // upper bound
      d_lower(lower),
      d_range(upper - lower)
    { }

    double
    operator()() const
    {
      return d_lower + d_range * (std::rand() / static_cast<double>(RAND_MAX));
    }

  private:

    const double    d_lower;
    const double    d_range;
  };

  int
  vectorFillRandom
  (shared_ptr<std::vector<double> > stub,
   const double                     lower,
   const double                     upper,
   const int                        size)
  {
    stub->clear();
    stub->reserve(size);
    std::generate_n(std::back_inserter(*stub),
                    size,
                    xeona::functorRandom(lower, upper));
    return stub->size();

  } // function 'xeona::vectorFillRandom<>'

} // namespace 'xeona'

// ---------------------------------------------------------
//  FREE FUNCTION   : xeona::vectorFillDiurnal
// ---------------------------------------------------------

namespace xeona
{
  class functorDiurnal
  {
  public:

    functorDiurnal
    (const double mean,
     const double amplitude,                 // peak-to-peak
     const int    offset,
     const double randomness,                // can be zero
     const int    resolution) :
      d_mean(mean),
      d_amplitude(amplitude),
      d_offset(offset),
      d_shift(::halfpi - ::twopi * static_cast<double>(offset) / 24),
      d_randomness(randomness),
      d_resolution(resolution),
      d_step(::twopi / static_cast<double>(resolution)),
      d_marker(0)                            // non-const
    {
    }

    ~functorDiurnal()
    {
      // preamble
      const double resolution = static_cast<double>(d_resolution);
      const double marker     = static_cast<double>(d_marker);
      const double interval   = resolution / 24;
      const double hours      = marker / resolution * 24;
      const double days       = hours / 24;

      // reporting
      std::ostringstream put;
      put << "\n"
          << "  + mean                            : " << d_mean                    << "\n"
          << "  + peak-to-peak                    : " << d_amplitude               << "\n"
          << "  + offset (int) [h]                : " << d_offset                  << "\n"
          << "    sine shift [rad]                : " << d_shift                   << "\n"
          << "  + relative randomness [-]         : " << d_randomness * 100 << "%" << "\n"
          << "  + sampling resolution (int) [/d]  : " << d_resolution              << "\n"
          << "    interval [h]                    : " << interval                  << "\n"
          << "    step size [rad]                 : " << d_step                    << "\n"
          << "  + samples count (int)             : " << d_marker                  << "\n"
          << "    time span [h] [d]               : " << hours << "  " << days     <<"\n";
      logger->putx(logga::adhc, put);
    }

    double
    operator()()
    {
      // signal -- the 0.5 because amplitude is peak-to-peak
      const double theta   = d_step * (d_marker + 0.5);
      const double signal  = d_mean + 0.5 * d_amplitude * std::sin(theta - d_shift);
      // noise
      const double zeroone = std::rand() / static_cast<double>(RAND_MAX);
      const double noise   = d_amplitude * d_randomness * (zeroone - 0.5);
      // total
      const double total   = signal + noise;
      // report from the first four calls
      if ( d_marker < 4 )
        {
          std::ostringstream put;
          put << "  marker    : " << d_marker << "\n"
              << "  theta     : " << theta    << "\n"
              << "  signal    : " << signal   << "\n"
              << "  noise     : " << noise    << "\n"
              << "  total     : " << total    << "\n";
          logger->addSmartBlank(logga::dbug);
          logger->putx(logga::adhc, put);
        }
      // housekeeping
      d_marker++;
      // return
      return total;
    }

  private:

    const double    d_mean;
    const double    d_amplitude;
    const int       d_offset;                // only for reporting
    const double    d_shift;                 // calculated
    const double    d_randomness;
    const int       d_resolution;
    const double    d_step;                  // calculated

    int             d_marker;                // zero-based

    static logga::spLogger logger;
  };

  logga::spLogger functorDiurnal::logger = logga::ptrLogStream();  // logging support

  int
  vectorFillDiurnal
  (shared_ptr<std::vector<double> > stub,         // will be 'clear'ed
   const double                     mean,         // mean value (before noise)
   const double                     amplitude,    // peak-to-peak measure (before noise)
   const int                        offset,       // temporal shift in hours [1]
   const double                     randomness,   // noise relative to amplitude [2]
   const int                        resolution,   // samples per day
   const int                        size)         // number of elements

  // [1] starts at midnight, zero indicates no shift, negative means start earlier
  // [2] maximum noise relative to amplitude, set to zero to disable

  {
    static logga::spLogger logger = logga::ptrLogStream();  // logging support
    logger->repx(logga::adhc, "entering member function, size", size);

    stub->clear();
    stub->reserve(size);
    std::generate_n(std::back_inserter(*stub),
                    size,
                    xeona::functorDiurnal(mean,
                                          amplitude,
                                          offset,
                                          randomness,
                                          resolution));
    return stub->size();

  } // function 'xeona::vectorFillIDiurnal'

} // namespace 'xeona'

// ---------------------------------------------------------
//  FREE FUNCTION   : xeona::vectorFillIncrement
// ---------------------------------------------------------

namespace xeona
{
  class functorInc
  {
  public:

    functorInc
    (const double start,                     // start
     const double step) :                    // step
      d_start(start),
      d_step(step),
      d_marker(start - step)                 // decrement at outset
    { }

    double
    operator()()
    {
      return d_marker += d_step;
    }

  private:

    const double    d_start;
    const double    d_step;
    double          d_marker;
  };

  int
  vectorFillIncrement
  (shared_ptr<std::vector<double> > stub,
   const double                     start,
   const double                     step,
   const int                        size)
  {
    stub->clear();
    stub->reserve(size);
    std::generate_n(std::back_inserter(*stub),
                    size,
                    xeona::functorInc(start, step));
    return stub->size();

  } // function 'xeona::vectorFillIncrement<>'

} // namespace 'xeona'

// ---------------------------------------------------------
//  FREE FUNCTION   : xeona::vectorPrint <>
// ---------------------------------------------------------

namespace xeona
{
  template <typename T>
  void
  vectorPrint
  (const shared_ptr<std::vector<T> > vec,
   const std::string&                msg,
   std::ostream&                     os,
   const std::string&                separator,
   const std::string&                final)
  {
    if ( vec->empty() )
      {
        os << "  " << msg << " : (empty)" << "\n";
        os << std::flush;
      }
    else
      {
        // print message if given
        if ( ! msg.empty() ) os << "  " << msg << " : ";
        // print all but the final element
        std::copy(vec->begin(),
                  vec->end() - 1,
                  std::ostream_iterator<T>(os, separator.c_str()));
        // print the final element
        std::copy(vec->end() - 1,
                  vec->end(),
                  std::ostream_iterator<T>(os, ""));
        // add final string
        os << final;
        os << std::flush;
      }

  } // function 'xeona::vectorPrint'
} // function namespace 'xeona'

// ---------------------------------------------------------
//  FREE FUNCTION   : xeona::vectorSampleDay <> (repetitive)
// ---------------------------------------------------------

namespace xeona
{
  template<typename T>
  shared_ptr<std::vector<T> >                     // one day timeseries
  vectorSampleDay
  (const T                           input,       // simple repetition
   const int                         interval)    // stated interval length [s]
  {
    // logging support
    static logga::spLogger logger = logga::ptrLogStream();  // bind logger
    logger->repx(logga::adhc, "entering free function, interval", interval);

    // preparation
    const int day  = 24 * 3600;              // one day in seconds

    // integrity checks
    const double test = static_cast<double>(day) / static_cast<double>(interval);
    if ( std::floor(std::abs(test)) != test )
      {
        logger->repx(logga::warn, "non-integral segmentation", test);
        return shared_ptr<std::vector<T> >();
      }
    const int segments = static_cast<int>(test);  // 'test' is guaranteed integer-valued
    logger->repx(logga::adhc, "day segments", segments);

    // active code
    shared_ptr<std::vector<T> > temp(new std::vector<T>(segments, input));
    return temp;

  } // function 'xeona::vectorSampleDay<> (repetitive)'

} // namespace 'xeona'

// ---------------------------------------------------------
//  FREE FUNCTION   : xeona::vector2TupleExtract0 <>
//  FREE FUNCTION   : xeona::vector2TupleExtract1 <>
// ---------------------------------------------------------

namespace xeona
{
  template <typename T>
  shared_ptr<std::vector<T> >
  vector2TupleExtract0                               // first element
  (const shared_ptr<std::vector<boost::tuple<T, T> > > input)
  {
    shared_ptr<std::vector<T> > buffer(new std::vector<T>());
    buffer->reserve(input->size());
    typedef boost::tuple<T, T> tuple_type;           // CAUTION: typedef necessary
    BOOST_FOREACH( tuple_type e, *input )
      {
        buffer->push_back(boost::tuples::get<0>(e)); // CAUTION: need the free function
      }
    return buffer;
  }

  template <typename T>
  shared_ptr<std::vector<T> >
  vector2TupleExtract1                               // second element
  (const shared_ptr<std::vector<boost::tuple<T, T> > > input)
  {
    shared_ptr<std::vector<T> > buffer(new std::vector<T>());
    buffer->reserve(input->size());
    typedef boost::tuple<T, T> tuple_type;           // CAUTION: typedef necessary
    BOOST_FOREACH( tuple_type e, *input )
      {
        buffer->push_back(boost::tuples::get<1>(e)); // CAUTION: need the free function
      }
    return buffer;
  }

} // namespace 'xeona'

// ---------------------------------------------------------
//  FREE FUNCTION   : xeona::vector2TupleZip <>
// ---------------------------------------------------------

namespace xeona
{
  template <typename T>
  shared_ptr<std::vector<boost::tuple<T, T> > >
  vector2TupleZip
  (const shared_ptr<std::vector<T> > left,
   const shared_ptr<std::vector<T> > right)
  {
    // logging support
    static logga::spLogger logger = logga::ptrLogStream();

    // preamble
    typedef boost::tuple<T, T> tuple_type;
    shared_ptr<std::vector<tuple_type> > buffer(new std::vector<tuple_type>());

    // integrity checks
    const int llen =  left->size();
    const int rlen = right->size();
    if ( llen != rlen )
      {
        std::ostringstream oss;
        oss << llen << "  " << rlen;
        logger->repx(logga::warn, "length mismatch", oss.str());
        return shared_ptr<std::vector<tuple_type> >();
      }

    // loop and load
    for ( int i = 0; i < llen; ++i )
      {
        buffer->push_back(boost::make_tuple(left->at(i),
                                            right->at(i)));
      }

    // return
    return buffer;
  }
}

// ---------------------------------------------------------
//  FREE FUNCTION   : xeona::stringParse   <>
//  FREE FUNCTION   : xeona::stringParseSP <>
// ---------------------------------------------------------

namespace xeona
{
  template <typename T>
  std::vector<T>                             // returns vector
  stringParse
  (const std::string& input,
   const std::string  sep)                   // concatenation of chars, not a string
    throw(boost::bad_lexical_cast)           // exception specification
  {
    // short-circuit if empty
    if ( input.empty() ) return std::vector<T>();

    // split the string
    std::vector<T> output;
    std::vector<std::string> buffer;
    boost::split(buffer, input, boost::is_any_of(sep), boost::token_compress_on);

    // cast the string
    BOOST_FOREACH( std::string s, buffer )
      {
        T conv = boost::lexical_cast<T>(s);  // can throw 'boost::bad_lexical_cast'
        output.push_back(conv);
      }

    // return created vector
    return output;
  }

  template <typename T>
  shared_ptr<std::vector<T> >                // returns smart pointer
  stringParseSP                              // wrapper to 'xeona::stringParse'
  (const std::string& input,
   const std::string  sep)                   // concatenation of chars, not a string
    throw(boost::bad_lexical_cast)           // exception specification
  {
    shared_ptr<std::vector<T> >
      output(new std::vector<T>(xeona::stringParse<T>(input, sep)));
    return output;
  }

} // namespace 'xeona'

// ---------------------------------------------------------
//  EXPLICIT TEMPLATE INSTANTIATIONS
// ---------------------------------------------------------

template unsigned xeona::vectorSum(const std::vector<unsigned>&);
template int      xeona::vectorSum(const std::vector<int     >&);
template double   xeona::vectorSum(const std::vector<double  >&);

template unsigned xeona::vectorSum(const shared_ptr<std::vector<unsigned> >&);
template int      xeona::vectorSum(const shared_ptr<std::vector<int     > >&);
template double   xeona::vectorSum(const shared_ptr<std::vector<double  > >&);

template unsigned xeona::vectorMean(const std::vector<unsigned>&);
template int      xeona::vectorMean(const std::vector<int     >&);
template double   xeona::vectorMean(const std::vector<double  >&);

template unsigned xeona::vectorMean(const shared_ptr<std::vector<unsigned> >&);
template int      xeona::vectorMean(const shared_ptr<std::vector<int     > >&);
template double   xeona::vectorMean(const shared_ptr<std::vector<double  > >&);

template shared_ptr<std::vector<int   > > operator+
(const shared_ptr<std::vector<int   > >&,
 const shared_ptr<std::vector<int   > >&);
template shared_ptr<std::vector<double> > operator+
(const shared_ptr<std::vector<double> >&,
 const shared_ptr<std::vector<double> >&);

template shared_ptr<std::vector<int   > > operator-
(const shared_ptr<std::vector<int   > >&,
 const shared_ptr<std::vector<int   > >&);
template shared_ptr<std::vector<double> > operator-
(const shared_ptr<std::vector<double> >&,
 const shared_ptr<std::vector<double> >&);

template void
xeona::vectorPrint
(const shared_ptr<std::vector<int   > >,
 const std::string&, std::ostream&, const std::string&, const std::string&);
template void
xeona::vectorPrint
(const shared_ptr<std::vector<double> >,
 const std::string&, std::ostream&, const std::string&, const std::string&);
template void
xeona::vectorPrint
(const shared_ptr<std::vector<bool  > >,
 const std::string&, std::ostream&, const std::string&, const std::string&);

template shared_ptr<std::vector<int   > > xeona::vector2TupleExtract0
(const shared_ptr<std::vector<boost::tuple<int   , int   > > >);
template shared_ptr<std::vector<double> > xeona::vector2TupleExtract0
(const shared_ptr<std::vector<boost::tuple<double, double> > >);
template shared_ptr<std::vector<bool  > > xeona::vector2TupleExtract0
(const shared_ptr<std::vector<boost::tuple<bool  , bool  > > >);

template shared_ptr<std::vector<int   > > xeona::vector2TupleExtract1
(const shared_ptr<std::vector<boost::tuple<int   , int   > > >);
template shared_ptr<std::vector<double> > xeona::vector2TupleExtract1
(const shared_ptr<std::vector<boost::tuple<double, double> > >);
template shared_ptr<std::vector<bool  > > xeona::vector2TupleExtract1
(const shared_ptr<std::vector<boost::tuple<bool  , bool  > > >);

template shared_ptr<std::vector<boost::tuple<int   , int   > > >
xeona::vector2TupleZip(const shared_ptr<std::vector<int   > >,
                       const shared_ptr<std::vector<int   > >);
template shared_ptr<std::vector<boost::tuple<double, double> > >
xeona::vector2TupleZip(const shared_ptr<std::vector<double> >,
                       const shared_ptr<std::vector<double> >);
template shared_ptr<std::vector<boost::tuple<bool  , bool  > > >
xeona::vector2TupleZip(const shared_ptr<std::vector<bool  > >,
                       const shared_ptr<std::vector<bool  > >);

template std::vector<unsigned>
xeona::stringParse(const std::string&, const std::string);
template std::vector<int>
xeona::stringParse(const std::string&, const std::string);
template std::vector<double>
xeona::stringParse(const std::string&, const std::string);

template shared_ptr<std::vector<unsigned> >
xeona::stringParseSP(const std::string&, const std::string);
template shared_ptr<std::vector<int> >
xeona::stringParseSP(const std::string&, const std::string);
template shared_ptr<std::vector<double> >
xeona::stringParseSP(const std::string&, const std::string);

//  end of file

