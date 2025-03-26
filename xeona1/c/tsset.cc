//  XEONA ENERGY SYSTEMS MODELING ENVIRONMENT
//
//  file-create-name : tsset.cc
//  file-create-date : Tue 24-May-2011 23:58 UTC
//  file-initiator   : Robbie Morrison <robbie@actrix.co.nz>
//
//  file-role        : timeseries classes for added functionality / implementation
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
//  $URL: file:///home/robbie/svn-root/xeona/futz/trunk/xeona1/c/tsset.cc $
//
//  GENERAL NOTES FOR THIS FILE

//  LOCAL AND SYSTEM INCLUDES

#include "tsset.h"            // companion header for this file (place first)

#include "../i/gnuplot.h"     // gnuplot interface, originally by Daniel Stahlke
#include "../c/util3.h"       // free functions for floating point comparison
#include "../c/tsalgos.h"     // custom STL algorithms for timeseries
#include "../c/files.h"       // free functions for regular files

#include "../a/logger.h"      // standard logging functionality (as required)
#include "../c/smart_ptr.h"   // toggle between Boost and TR1 smart pointers
#include ".././common.h"      // common definitions for project (place last)

#include <algorithm>          // STL copying, searching, and sorting, max()
#include <deque>              // STL sequence container, double-ended vector
#include <functional>         // STL function objects
#include <iostream>           // standard io
#include <iterator>           // additional iterators
#include <limits>             // numeric_limits<T>::inf() and similar
#include <numeric>            // STL numerical algorithms
#include <sstream>            // string-streams
#include <stdexcept>          // runtime_error()
#include <string>             // C++ strings
#include <vector>             // STL sequence container

#include <cmath>              // C-style maths, ceil(), floor(), sqrt()

#include <boost/assign/std/vector.hpp>       // assign operator+=() for std::vector
#include <boost/foreach.hpp>                 // BOOST_FOREACH iteration macro
#include <boost/format.hpp>                  // printf style formatting
#include <boost/tuple/tuple.hpp>             // n-tuples, ref(), cref()

#include <boost/date_time/posix_time/posix_time.hpp>   // plus io

// NAMESPACE DIRECTIVES

using namespace boost::assign;               // for: vec += 0, 1;

//  CODE

// ---------------------------------------------------------
//  CLASS           : TsBase (abstract)
// ---------------------------------------------------------

// STATIC DEFINITIONS - with and without explicit initialization

// 's_intervals' contains a list of valid intervals -- it needs
// to match the values in class 'TimeHorizon'

#define S_CVALS_SIZE 12                      // CAUTION: must equal the number of elements
const int TsBase::s_cvals[] =
  {
    5  *   60,    // sub-hour multiples [m * 60]
    10 *   60,
    15 *   60,
    20 *   60,
    30 *   60,
    1  * 3600,    // hour multiples [h * 3600]
    2  * 3600,
    3  * 3600,
    4  * 3600,
    6  * 3600,
    12 * 3600,
    24 * 3600
  };

const std::vector<int> TsBase::s_intervals(s_cvals, s_cvals + S_CVALS_SIZE);

bool            TsBase::s_checks  = false;
const double    TsBase::s_nan     = std::numeric_limits<double>::quiet_NaN();
const double    TsBase::s_inf     = std::numeric_limits<double>::infinity();

logga::spLogger TsBase::s_logger  = logga::ptrLogStream();

// ---------------------------------------------------------
//  MEMBER FUNCTION : TsBase (no-data constructor)
// ---------------------------------------------------------

TsBase::TsBase                               // no-data constructor
(const std::string& label) :
  // text information
  d_label(label),                            // always present
  d_caller(),                                // optional, set by a dedicated call
  // series
  d_series(),
  d_wrappable(false),
  d_size(d_series.size()),
  d_empty(d_series.empty()),
  d_scaling(1.0),
  d_offsetting(0.0),
  // statistics
  d_count(0),
  d_sum(s_nan),
  d_mean(s_nan),
  d_stddev(s_nan),
  d_median(s_nan),
  d_min(s_nan),
  d_max(s_nan),
  d_range(s_nan),
  d_first(s_nan),
  d_last(s_nan)
{
  s_logger->repx(logga::adhc, "constructor call", "no-data");
}

// ---------------------------------------------------------
//  MEMBER FUNCTION : TsBase (usual constructor)
// ---------------------------------------------------------

TsBase::TsBase                               // usual constructor
(const std::string&        label,
 const std::vector<double> series,
 const bool                wrappable) :      // note default
  // text information
  d_label(label),
  d_caller(),
  // series
  d_series(series),
  d_wrappable(wrappable),
  d_size(d_series.size()),
  d_empty(d_series.empty()),
  d_scaling(1.0),
  d_offsetting(0.0),
  // statistics
  d_count(0),
  d_sum(s_nan),
  d_mean(s_nan),
  d_stddev(s_nan),
  d_median(s_nan),
  d_min(s_nan),
  d_max(s_nan),
  d_range(s_nan),
  d_first(s_nan),
  d_last(s_nan)
{
  s_logger->repx(logga::adhc, "constructor call", "usual");
  calcStatistics();
}

// ---------------------------------------------------------
//  MEMBER FUNCTION : TsBase(const TsBase&) (copy ctor)
// ---------------------------------------------------------

TsBase::TsBase(const TsBase& orig) :         // copy constructor
  // text information
  d_label(orig.d_label),
  d_caller(orig.d_caller),
  // series
  d_series(orig.d_series),
  d_wrappable(orig.d_wrappable),
  d_size(orig.d_size),
  d_empty(orig.d_empty),
  d_scaling(orig.d_scaling),
  d_offsetting(orig.d_offsetting),
  // statistics
  d_count(orig.d_count),
  d_sum(orig.d_sum),
  d_mean(orig.d_mean),
  d_stddev(orig.d_stddev),
  d_median(orig.d_median),
  d_min(orig.d_min),
  d_max(orig.d_max),
  d_range(orig.d_range),
  d_first(orig.d_first),
  d_last(orig.d_last)
{
  s_logger->repx(logga::adhc, "copy constructor call", "");
}

// ---------------------------------------------------------
//  MEMBER FUNCTION : ~TsBase (destructor)
// ---------------------------------------------------------

TsBase::~TsBase()
{
  s_logger->repx(logga::adhc, "destructor call", "");
}

// ---------------------------------------------------------
//  MEMBER FUNCTION : swap
// ---------------------------------------------------------

void
TsBase::swap
(TsBase& rhs)                                // swap helper, used by derived classes
{
  s_logger->repx(logga::adhc, "entering member function", "TsBase");
  std::swap(d_caller,     rhs.d_caller);     // <algorithm> or container specializations
  std::swap(d_label,      rhs.d_label);
  std::swap(d_series,     rhs.d_series);
  std::swap(d_wrappable,  rhs.d_wrappable);
  std::swap(d_size,       rhs.d_size);
  std::swap(d_empty,      rhs.d_empty);
  std::swap(d_scaling,    rhs.d_scaling);
  std::swap(d_offsetting, rhs.d_offsetting);
  std::swap(d_count,      rhs.d_count);
  std::swap(d_sum,        rhs.d_sum);
  std::swap(d_mean,       rhs.d_mean);
  std::swap(d_stddev,     rhs.d_stddev);
  std::swap(d_median,     rhs.d_median);
  std::swap(d_min,        rhs.d_min);
  std::swap(d_max,        rhs.d_max);
  std::swap(d_range,      rhs.d_range);
  std::swap(d_first,      rhs.d_first);
  std::swap(d_last,       rhs.d_last);
}

// ---------------------------------------------------------
//  MEMBER FUNCTION : value
// ---------------------------------------------------------

double
TsBase::value
(const unsigned index) const
  throw(std::out_of_range)                   // exception specification
{
  if ( d_empty )
    {
      std::stringstream oss;
      oss << "TsBase::value: timeseries object is empty, sought index " << index;
      throw std::out_of_range(oss.str());
    }
  else if ( index < d_size )
    {
      return d_series.at(index);
    }
  else if ( d_wrappable == true )
    {
      const int newindex = index % d_size;   // works fine for unwrapped indexes too
      return d_series.at(newindex);
    }
  else
    {
      std::stringstream oss;
      oss << "timeseries not wrappable, sought index: " << index;
      throw std::out_of_range(oss.str());
    }
}

// ---------------------------------------------------------
//  MEMBER FUNCTION : recover
// ---------------------------------------------------------

const std::vector<double>&
TsBase::recover() const
{
  return d_series;
}

// ---------------------------------------------------------
//  MEMBER FUNCTION : pointer (resized)
// ---------------------------------------------------------

shared_ptr<std::vector<double> >                     // by truncated new smart pointer
TsBase::pointer
(const unsigned resize) const
{
  if ( resize == 0 || resize == d_size )     // no resizing requested or required
    {
      shared_ptr<std::vector<double> > temp(new std::vector<double>(d_series));
      return temp;
    }
  else if ( resize < d_size )                // truncation
    {
      s_logger->repx(logga::dbug, "truncation will occur", d_size - resize);
      shared_ptr<std::vector<double> > temp(new std::vector<double>(d_series));
      temp->resize(resize);
      return temp;
    }
  else                                       // default elements added
    {
      s_logger->repx(logga::warn, "resizing will add zeros", resize - d_size);
      shared_ptr<std::vector<double> > temp(new std::vector<double>(d_series));
      temp->resize(resize);
      return temp;
    }
}

// ---------------------------------------------------------
//  MEMBER FUNCTION : copyfill (resized, direct)
// ---------------------------------------------------------
//  Description  : copy a potentially resized vector
//  Role         : client usage
//  Techniques   : 'std::copy' 'std::back_inserter'
//  Status       : complete
//
//  Design notes
//
//       Specially created because some usages in 'xeona' would
//       not work with the 'pointer' function.
//
// ---------------------------------------------------------

int
TsBase::copyfill
(std::vector<double>& vec,                   // pass-by-reference
 const unsigned       resize) const
{
  // initial reporting
  s_logger->repx(logga::adhc, "entering member function", "");

  // debug reporting
  const int invec = vec.size();

  // active code
  if ( resize == 0 || resize == d_size )     // no resizing requested or required
    {
      vec.clear();                           // remove existing elements
      vec.reserve(d_size);                   // useful but not mandatory
      std::copy(d_series.begin(), d_series.end(), std::back_inserter(vec));
    }
  else if ( resize < d_size )                // truncation
    {
      s_logger->repx(logga::dbug, "truncation will occur, omitted", d_size - resize);
      vec.clear();                           // remove existing elements
      vec.reserve(resize);                   // useful but not mandatory
      std::copy(d_series.begin(), d_series.begin() + resize, std::back_inserter(vec));
    }
  else                                       // default elements added
    {
      s_logger->repx(logga::warn, "resizing will add zeros, created", resize - d_size);
      vec.clear();                           // remove existing elements
      vec.reserve(resize);                   // useful but not mandatory
      std::copy(d_series.begin(), d_series.end(), std::back_inserter(vec));
      vec.resize(resize);
    }

  // debug reporting
  std::ostringstream put;
  const int outvec = vec.size();
  put << " ";
  if ( resize == 0 ) put << "resize = " << resize << " (source determines size)   ";
  else               put << "resize = " << resize << " (this value determines size)   ";
  put << "d_size = " << d_size << "   "
      << "vec.size() : "
      << "in = "  << invec  << "  "
      << "out = " << outvec << "\n";
  s_logger->repx(logga::adhc, "additional reporting follows", "");
  s_logger->putx(logga::adhc, put);

  // return
  return vec.size();
}

// ---------------------------------------------------------
//  MEMBER FUNCTION : copyfill (resized, smart pointer wrapper)
// ---------------------------------------------------------
//  Description  : copy a potentially resized vector
//  Role         : client usage
//  Techniques   : wrapper
//  Status       : complete
// ---------------------------------------------------------

int
TsBase::copyfill
(shared_ptr<std::vector<double> > vec,
 const unsigned                   resize) const
{
  // defensive programming
  if ( ! vec )
    {
      s_logger->repx(logga::warn, "empty pointer (try new std::vector)", vec);
    }

  // active code
  return copyfill(*vec, resize);
}

// ---------------------------------------------------------
//  MEMBER FUNCTION : getLabel
//  MEMBER FUNCTION : getCaller
//  MEMBER FUNCTION : empty
//  MEMBER FUNCTION : size
//  MEMBER FUNCTION : isWrappable
//  MEMBER FUNCTION : getScaling
//  MEMBER FUNCTION : getCount
//  MEMBER FUNCTION : getSum
//  MEMBER FUNCTION : getMean
//  MEMBER FUNCTION : getStdDev
//  MEMBER FUNCTION : getMedian
//  MEMBER FUNCTION : getMin
//  MEMBER FUNCTION : getMax
//  MEMBER FUNCTION : getRange
//  MEMBER FUNCTION : getFirst
//  MEMBER FUNCTION : getLast
// ---------------------------------------------------------

  // text information

std::string TsBase::getLabel()      const { return d_label;      }
std::string TsBase::getCaller()     const { return d_caller;     }

  // metadata (check for further relevant calls in the derived classes)

bool        TsBase::empty()         const { return d_empty;      }
unsigned    TsBase::size()          const { return d_size;       }
bool        TsBase::isWrappable()   const { return d_wrappable;  }
double      TsBase::getScaling()    const { return d_scaling;    }
double      TsBase::getOffsetting() const { return d_offsetting; }

  // statistics

int         TsBase::getCount()      const { return d_count;      }
double      TsBase::getSum()        const { return d_sum;        }
double      TsBase::getMean()       const { return d_mean;       }
double      TsBase::getStdDev()     const { return d_stddev;     }
double      TsBase::getMedian()     const { return d_median;     }
double      TsBase::getMin()        const { return d_min;        }
double      TsBase::getMax()        const { return d_max;        }
double      TsBase::getRange()      const { return d_range;      }
double      TsBase::getFirst()      const { return d_first;      }
double      TsBase::getLast()       const { return d_last;       }

// ---------------------------------------------------------
//  FREE FUNCTION   : getStatistics
// ---------------------------------------------------------
//  Description  : recover all statistics in a 10-tuple
//  Role         : client usage
//  Techniques   : 'boost::tuple'
//  Status       : complete
//
//  Recovery
//
//      One can later access the return value as follows (the
//      index must be const integral expression)
//
//      boost::tuple<see below> stats;
//      stats = myTs.getStats();
//      int count;
//      count = stats.get<0>();        // member function usage
//      count = boost::get<0>(stats);  // free function usage (no "tuple::")
//
// ---------------------------------------------------------

boost::tuple
<int,                                        // count
 double,                                     // sum
 double,                                     // mean
 double,                                     // stddev
 double,                                     // median
 double,                                     // min
 double,                                     // max
 double,                                     // range
 double,                                     // first
 double>                                     // last
TsBase::getStatistics() const
{
  return boost::make_tuple(d_count,          // 10 elements is the default maximum
                           d_sum,
                           d_mean,
                           d_stddev,
                           d_median,
                           d_min,
                           d_max,
                           d_range,
                           d_first,
                           d_last);
}

// MANIPULATORS

// ---------------------------------------------------------
//  MEMBER FUNCTION : setCaller
// ---------------------------------------------------------

std::string
TsBase::setCaller                            // optional registration
(const std::string& caller)                  // either __func__ or __PRETTY_FUNCTION__
{
  const std::string was = d_caller;
  d_caller              = caller;
  return was;
}

// ---------------------------------------------------------
//  MEMBER FUNCTION : rescale
// ---------------------------------------------------------

void
TsBase::rescale
(const double       scaleFactor,
 const std::string& sublabel)
{
  if ( ! sublabel.empty() ) d_label += " : " + sublabel;
  scalarMultiply(scaleFactor);               // also recalculates the statistics
}

// ---------------------------------------------------------
//  MEMBER FUNCTION : unscale
// ---------------------------------------------------------

bool
TsBase::unscale
(const std::string& sublabel)
{
  if ( ! sublabel.empty() ) d_label += " : " + sublabel;
  return scalarMultiply( 1.0 / d_scaling);   // also recalculates the statistics
}

// ---------------------------------------------------------
//  MEMBER FUNCTION : reoffset
// ---------------------------------------------------------

void
TsBase::reoffset
(const double       offsetFactor,
 const std::string& sublabel)
{
  if ( ! sublabel.empty() ) d_label += " : " + sublabel;
  scalarAdd(offsetFactor);                   // also recalculates the statistics
}

// ---------------------------------------------------------
//  MEMBER FUNCTION : unoffset
// ---------------------------------------------------------

bool
TsBase::unoffset
(const std::string& sublabel)
{
  if ( ! sublabel.empty() ) d_label += " : " + sublabel;
  return scalarAdd( -1.0 * d_scaling);       // also recalculates the statistics
}

// ---------------------------------------------------------
//  MEMBER FUNCTION : clear
// ---------------------------------------------------------

void
TsBase::clear()                              // but 'd_label' and 'd_caller' persist
{
  // series
  d_series.clear();                          // empties the container completely
  d_wrappable  = false;
  d_size       = d_series.size();
  d_empty      = d_series.empty();
  d_scaling    = 1.0;
  d_offsetting = 0.0;
  // statistics
  d_count      = 0;
  d_sum        = s_nan;
  d_mean       = s_nan;
  d_stddev     = s_nan;
  d_median     = s_nan;
  d_min        = s_nan;
  d_max        = s_nan;
  d_range      = s_nan;
  d_first      = s_nan;
  d_last       = s_nan;
}

// STATIC MANIPULATORS

// ---------------------------------------------------------
//  MEMBER FUNCTION : checksOn (static)
// ---------------------------------------------------------

bool                                         // prior value
TsBase::checksOn()
{
  const bool was = s_checks;
  s_checks       = true;
  return was;
}

// ---------------------------------------------------------
//  MEMBER FUNCTION : checksOff (static)
// ---------------------------------------------------------

bool                                         // prior value
TsBase::checksOff()
{
  const bool was = s_checks;
  s_checks       = false;
  return was;
}

// UTILITY FUNCTIONS

// ---------------------------------------------------------
//  MEMBER FUNCTION : confirmInterval
// ---------------------------------------------------------

bool
TsBase::confirmInterval
(const int interval) const
{
  // utter hack for plotting a 'TsMonthly' object as a 'TsNormal' object
  if ( interval == 1 ) return true;

  // search
  std::vector<int>::const_iterator pos;
  pos = std::find(s_intervals.begin(), s_intervals.end(), interval);

  // interpret
  if ( pos == s_intervals.end() )
    {
      s_logger->repx(logga::warn, "unsupported interval encountered", interval);
      return false;
    }
  else
    {
      return true;
    }
}

// ---------------------------------------------------------
//  MEMBER FUNCTION : calcStatistics
// ---------------------------------------------------------

void
TsBase::calcStatistics()
{
  if ( d_empty )
    {
      d_count  = 0;
      d_sum    = s_nan;
      d_mean   = s_nan;
      d_stddev = s_nan;
      d_median = s_nan;
      d_min    = s_nan;
      d_max    = s_nan;
      d_range  = s_nan;
      d_first  = s_nan;
      d_last   = s_nan;
    }
  else
    {
      d_count  = std::distance(d_series.begin(), d_series.end());
      d_sum    = std::accumulate(d_series.begin(), d_series.end(), double(0.0));
      d_mean   = d_sum / static_cast<double>(d_count);
      d_stddev = calcStdDev();                                             // utility
      d_median = calcPercentile(0.5);                                      // utility
      d_min    = *std::min_element(d_series.begin(), d_series.end());      // dereferenced
      d_max    = *std::max_element(d_series.begin(), d_series.end());      // dereferenced
      d_range  = d_max - d_min;
      d_first  = d_series.front();
      d_last   = d_series.back();
    }

  if ( d_size != d_count )
    {
      std::ostringstream oss;
      oss << d_size << " " << d_count;
      s_logger->repx(logga::warn, "size and count differ", oss.str());
    }
}

// ---------------------------------------------------------
//  MEMBER FUNCTION : calcStdDev
// ---------------------------------------------------------
//  Description  : calculate the standard deviation
//  Role         : statistics support
//  Techniques   : functors, integral (nontype) template arguments
//  Status       : complete
//
//  Design notes
//
//      The standard deviation formula used here is based on
//      Stephens etal (2006 pp403-407) and uses an "unbiased"
//      definition for variance -- also known as the second
//      central moment (note also the typo in table 11-1 where
//      the "divide by count" is omitted):
//
//          var    = sum (x_i - mean)^2 / count
//          stddev = sqrt(var)
//
//      For more information on std::accumulate from <numeric>,
//      see Josuttis (1999 pp425-429).  This standard algorithm
//      is also used with the functor 'SumDiffNthPower' in this
//      code.  See comments in the code for more information.
//
//      Note the used of "nontype parameters" in templates, as
//      described by Loudon (2003 pp100-101).
//
//   CAUTION: std::sqrt
//
//      The library function 'std::sqrt' is supposed to throw a
//      'std::domain_error' if the argument is out of range, that
//      is, in this case, negative -- however testing shows
//      std::sqrt(-1.0) in this environment returns a NaN
//      instead.  Returning a NaN is actually more suitable
//      response this case.  Perhaps your system behaves
//      differently.
//
//  REFERENCES
//
//      Loudon, Kyle.  2003.  C++ pocket reference.  O'Reilly and
//        Associates, Sebastopol, California, USA.  ISBN
//        0-596-00496-6.
//
//      Stephens, D Ryan, Christopher Diggins, Jonathan Turkanis,
//        and Jeff Cogswell.  2006.  C++ cookbook : solutions and
//        examples for C++ programmers.  O'Reilly Media,
//        Sebastopol, California, USA.  ISBN 0-596-00761-2.
//
// ---------------------------------------------------------

double
TsBase::calcStdDev() const
{
  if ( d_empty ) return s_nan;

  const double var    = nthMoment<2>();      // second central moment
  const double stddev = std::sqrt(var);      // returns 'nan' if argument is negative
  return stddev;
}

// ---------------------------------------------------------
//  MEMBER FUNCTION : nthMoment <>
// ---------------------------------------------------------

template <int N>
double
TsBase::nthMoment() const
{
  const double count  = getCount();
  const double mean   = getMean();
  const double moment = std::accumulate
    (d_series.begin(),                       // start position
     d_series.end(),                         // end position
     double(0),                              // initial value
     SumDiffNthPower<N>(mean))               // functor defined below
    / count;

  return moment;
}

// ---------------------------------------------------------
//  MEMBER FUNCTION : SumDiffNthPower <> (ctor)
// ---------------------------------------------------------
//
//  'SumDiffNthPower' is a two argument (binary) functor used
//  when calculating the standard deviation in conjunction with
//  the std::accumulate algorithm.  The functor must not modify
//  the passed arguments and should return the desired outcome
//  (such as a rolling summation or product).  That outcome is
//  labeled here as the 'rol'.  The functor is called on each
//  element, labeled here as 'cur'.
//
// ---------------------------------------------------------

template <int N>                           // N is the nth power
TsBase::SumDiffNthPower<N>::SumDiffNthPower
(const double mean) :
  d_mean(mean)
{
}

// ---------------------------------------------------------
//  MEMBER FUNCTION : SumDiffNthPower::operator() <>
// ---------------------------------------------------------

template <int N>                           // N is the nth power
double
TsBase::SumDiffNthPower<N>::operator()
(double rol, double cur) const
{
  return rol + std::pow(cur - d_mean, N);
}

// ---------------------------------------------------------
//  MEMBER FUNCTION : calcPercentile
// ---------------------------------------------------------

double
TsBase::calcPercentile
(const double fraction) const
{
  std::vector<double>::iterator percentileIter;
  std::vector<double> temp(d_series);        // 'std::nth_element' mods the container [1]
  percentileIter
    = temp.begin()
    + temp.size() * fraction;                // CAUTION: integer arithmetic is reasonable
  std::nth_element(temp.begin(),             // refer <algorithm>
                   percentileIter,
                   temp.end());
  return *percentileIter;

  // [1] 'std::nth_element' sorts the container, at least in
  // part -- although during some trials the sort looked complete
}

// ---------------------------------------------------------
//  MEMBER FUNCTION : scalarMultiply
// ---------------------------------------------------------

bool                                                        // 'true' means actioned
TsBase::scalarMultiply
(const double scaleFactor)
{
  if ( scaleFactor == 1.0 ) return false;                   // nothing to do
  std::transform(d_series.begin(),                          // refer <algorithm>
                 d_series.end(),
                 d_series.begin(),
                 std::bind1st(std::multiplies<double>(),    // refer <functional>
                              scaleFactor));
  d_scaling *= scaleFactor;
  calcStatistics();
  return true;
}

// ---------------------------------------------------------
//  MEMBER FUNCTION : scalarAdd
// ---------------------------------------------------------

bool                                                        // 'true' means actioned
TsBase::scalarAdd
(const double offsetFactor)
{
  if ( offsetFactor == 0.0 ) return false;                  // nothing to do
  std::transform(d_series.begin(),                          // refer <algorithm>
                 d_series.end(),
                 d_series.begin(),
                 std::bind1st(std::plus<double>(),          // refer <functional>
                              offsetFactor));
  d_offsetting += offsetFactor;
  calcStatistics();
  return true;
}

// ---------------------------------------------------------
//  MEMBER FUNCTION : considerEqual
// ---------------------------------------------------------
//  Description  : two means are equal if they are nearly equal or both close to zero
//  Role         : sampling support
//  Techniques   : robust comparison
//  Status       : complete
//
//  Design notes
//
//          a : 27.87939135007659174903
//          b : 27.87939135007646385134
//                             ^^^^^^^^
//
//          a : 2.51651e-15
//          b : 2.36848e-15
//                ^^^^^  ^^
//
//      The 'xeona::numic' threshold currently works but could be
//      let out to 'xeona::tight' on the 'nearZero' test if
//      conditions indicate that this is necessary.
//
// ---------------------------------------------------------

bool
TsBase::considerEqual
(const double mean1,
 const double mean2) const
{
  return
    ( xeona::almostEqual(mean1, mean2, xeona::numic)   // close to one-another
      ||                                               // or both near zero
      ( xeona::nearZero(mean1, xeona::numic) &&
        xeona::nearZero(mean2, xeona::numic) ) );
}

// ---------------------------------------------------------
//  CLASS           : TsNormal (TsBase specialization)
// ---------------------------------------------------------
//
//      See also the header file documentation.
//
//  Design notes
//
//      This class was initially templated, but it became
//      difficult to provide consistent semantics for both
//      integral and floating point types.  It now supports just
//      the 'double' type.
//
//      A 'std::vector' was selected over a 'std::valarray' as
//      that is what Stephens etal used.  Moreover the
//      'std::valarray' class often gets bad reviews.
//      Conversely, there is a reasonable case for using
//      'std::deque' as the primary container.
//
//      For information on boost::tuple's, see Becker (2007
//      pp3-22) or Karlsson (2006 pp209-234).
//
//  Storing time in 'int's.
//
//      A 32-bit signed integer ('time_t' for example) holds
//      approximately 68 years as seconds with a maximum value of
//      2'147'483'647 s -- it should be noted that the minimum
//      guaranteed storage for a C++ 'int' is 16-bits yielding
//      just 9 hours.  Indeed 'xeona' checks for 31 digit
//      (32-bit) 'int's at compile-time.
//
// ---------------------------------------------------------

// ---------------------------------------------------------
//  MEMBER FUNCTION : TsNormal (no-data ctor)
// ---------------------------------------------------------

TsNormal::TsNormal                           // no-data constructor, used as a return
(const std::string& label) :
  TsBase(label),
  // metadata
  d_interval(0),
  d_hasFeb29(false),
  d_span(0)
{
  s_logger->repx(logga::adhc, "constructor call", "no-data");
}

// ---------------------------------------------------------
//  MEMBER FUNCTION : TsNormal (usual ctor, direct)
// ---------------------------------------------------------

TsNormal::TsNormal                           // usual constructor, direct
(std::vector<double> original,
 const int           interval,
 const std::string&  label,
 const bool          hasFeb29,               // note default
 const bool          wrappable)              // note default
  throw (std::domain_error) :
  TsBase(label,
         original),
  // metadata
  d_interval(interval),
  d_hasFeb29(hasFeb29),
  d_span(d_series.size() * d_interval)
{
  s_logger->repx(logga::adhc, "constructor call", "direct");

  if ( ! TsBase::confirmInterval(d_interval) )
    {
      s_logger->repx(logga::warn, "interval not unsupported", d_interval);
      s_logger->repx(logga::dbug, "about to throw", "");
      std::ostringstream oss;
      oss << "given interval not valid: " << d_interval;
      throw std::domain_error(oss.str());
    }
  if ( d_empty )
    {
      s_logger->repx(logga::warn, "original timeseries empty", "");
    }
}

// ---------------------------------------------------------
//  MEMBER FUNCTION : TsNormal (usual ctor, smart pointer)
// ---------------------------------------------------------

// NOTE: 'TsNormal' could have been specialized, but
// maintaining two straightforward and stable constructors
// seemed simpler

TsNormal::TsNormal                                // usual constructor, smart pointer
(shared_ptr<std::vector<double> > original,
 const int                        interval,
 const std::string&               label,
 const bool                       hasFeb29,       // note default
 const bool                       wrappable)      // note default
  throw (std::domain_error) :
  TsBase(label,
         *original),
  // metadata
  d_interval(interval),
  d_hasFeb29(hasFeb29),
  d_span(d_series.size() * d_interval)
{
  // initial reporting
  s_logger->repx(logga::adhc, "constructor call", "smart pointer");

  // integrity checks for smart pointer
  if ( ! original )
    {
      s_logger->repx(logga::warn, "empty pointer (try new std::vector)", original);
    }
  // further integrity checks
  if ( ! TsBase::confirmInterval(d_interval) )
    {
      s_logger->repx(logga::warn, "interval not unsupported", d_interval);
      s_logger->repx(logga::dbug, "about to throw", "");
      std::ostringstream oss;
      oss << "given interval not valid: " << d_interval;
      throw std::domain_error(oss.str());
    }
  if ( d_empty )
    {
      s_logger->repx(logga::warn, "original timeseries empty", "");
    }
}

// ---------------------------------------------------------
//  MEMBER FUNCTION : TsNormal (repetition constructor)
// ---------------------------------------------------------

TsNormal::TsNormal
  (const int          steps,
   const double       value,
   const int          interval,
   const std::string& label,
   const bool         hasFeb29,              // note default
   const bool         wrappable)             // note default
    throw (std::domain_error) :
    TsBase(label,
           std::vector<double>(steps, value)), // create vector
    // metadata
    d_interval(interval),
    d_hasFeb29(hasFeb29),
    d_span(steps * d_interval)
{
  // initial reporting
  s_logger->repx(logga::adhc, "constructor call", "repetition");

  // further integrity checks
  if ( ! TsBase::confirmInterval(d_interval) )
    {
      s_logger->repx(logga::warn, "interval not unsupported", d_interval);
      s_logger->repx(logga::dbug, "about to throw", "");
      std::ostringstream oss;
      oss << "given interval not valid: " << d_interval;
      throw std::domain_error(oss.str());
    }
  if ( d_empty )
    {
      s_logger->repx(logga::warn, "original timeseries empty", "");
    }
}

// ---------------------------------------------------------
//  MEMBER FUNCTION : TsNormal(const TsNormal&) (copy ctor)
// ---------------------------------------------------------

TsNormal::TsNormal(const TsNormal& orig) :             // copy constructor
  TsBase(orig),                                        // copy construct the base class
  d_interval(orig.d_interval),
  d_hasFeb29(orig.d_hasFeb29),
  d_span(orig.d_span)
{
  s_logger->repx(logga::adhc, "copy constructor call", "");
}

// ---------------------------------------------------------
//  MEMBER FUNCTION : operator= (copy assignment)
// ---------------------------------------------------------

TsNormal& TsNormal::operator= (const TsNormal& orig)   // copy assignment operator
{
  s_logger->repx(logga::adhc, "copy assignment call", "TsNormal");
  TsNormal temp(orig);
  swap(temp);                                          // also swaps base class members
  return *this;
}

// ---------------------------------------------------------
//  MEMBER FUNCTION : ~TsNormal (destructor)
// ---------------------------------------------------------

TsNormal::~TsNormal()
{
  s_logger->repx(logga::adhc, "destructor call", "");
}

// ---------------------------------------------------------
//  MEMBER FUNCTION : swap
// ---------------------------------------------------------

void
TsNormal::swap
(TsNormal& rhs)
{
  s_logger->repx(logga::adhc, "entering member function", "TsNormal");
  TsBase::swap(rhs);                       // CAUTION: swap the base class too
  std::swap(d_interval,  rhs.d_interval);
  std::swap(d_hasFeb29,  rhs.d_hasFeb29);
  std::swap(d_span,      rhs.d_span);
}

// ---------------------------------------------------------
//  MEMBER FUNCTION : load (overloaded)
// ---------------------------------------------------------

//  requires empty object

bool
TsNormal::load
(std::vector<double> original,
 const int           interval,
 const bool          hasFeb29,               // note default
 const bool          wrappable)              // note default
  throw (std::domain_error)
{
  // initial report
  s_logger->repx(logga::adhc, "entering member function", "workhorse");

  // refuse to overwrite non-empty data
  if ( ! d_empty )
    {
      s_logger->repx(logga::warn, "original timeseries not empty", "");
      return false;
    }

  // transfer data
  d_series     = original;
  d_wrappable  = wrappable;
  d_size       = d_series.size();
  d_empty      = d_series.empty();
  d_scaling    = 1.0;
  d_offsetting = 0.0;
  d_interval   = interval;
  d_hasFeb29   = hasFeb29;
  d_span       = d_size * d_interval;

  // throw if interval not supported
  if ( ! TsBase::confirmInterval(d_interval) )
    {
      s_logger->repx(logga::warn, "interval not unsupported", d_interval);
      s_logger->repx(logga::dbug, "about to throw", "");
      std::ostringstream oss;
      oss << "given interval not valid: " << d_interval;
      throw std::domain_error(oss.str());
    }

  // housekeeping
  calcStatistics();
  return true;
}

bool
TsNormal::load                               // as per constructor, smart pointer
(shared_ptr<std::vector<double> > original,
 const int                        interval,
 const bool                       hasFeb29,  // note default
 const bool                       wrappable) // note default
  throw (std::domain_error)
{
  return load(*original, interval, hasFeb29, wrappable);
}

bool
TsNormal::load                               // as per constructor, repetition
(const int    steps,
 const double value,
 const int    interval,
 const bool   hasFeb29,                      // note default
 const bool   wrappable)                     // note default
 throw (std::domain_error)
 {
   return load(std::vector<double>(steps, value), interval, hasFeb29, wrappable);
 }

// ---------------------------------------------------------
//  MEMBER FUNCTION : getInterval
//  MEMBER FUNCTION : hasFeb29
//  MEMBER FUNCTION : getSpan
// ---------------------------------------------------------

int  TsNormal::getInterval() const { return d_interval;   }
bool TsNormal::hasFeb29()    const { return d_hasFeb29;   }
int  TsNormal::getSpan()     const { return d_span;       }

// ---------------------------------------------------------
//  MEMBER FUNCTION : sameMetaData
// ---------------------------------------------------------

bool                                       // 'true' if the same
TsNormal::sameMetaData
(const TsNormal& other) const
{
  return sameMeta(other);                  // utility function
}

// ---------------------------------------------------------
//  MEMBER FUNCTION : getDurationSeconds
//  MEMBER FUNCTION : getDurationHours
// ---------------------------------------------------------

int
TsNormal::getDurationSeconds() const
{
  return d_interval * d_size;                // both are integers
}

double
TsNormal::getDurationHours() const
{
  const int seconds = getDurationSeconds();
  return static_cast<double>(seconds) / 3600.0;
}

// ---------------------------------------------------------
//  MEMBER FUNCTION : hasSufficientData
// ---------------------------------------------------------

bool
TsNormal::hasSufficientData
(const int interval,
 const int steps,
 const int startOffset) const                // note default
{
  const int want = interval * steps + startOffset;
  const int have = d_interval * d_size;

  if ( ! TsBase::confirmInterval(interval) )
    {
      s_logger->repx(logga::warn, "given interval not supported", interval);
      return false;
    }
  else if ( d_wrappable )
    {
      return true;
    }
  else if ( want <= have )
    {
      return true;
    }
  else
    {
      std::ostringstream oss;
      oss << want << " " << have;
      s_logger->repx(logga::warn, "insufficient data, want have", oss.str());
      return false;
    }
}

// ---------------------------------------------------------
//  MEMBER FUNCTION : summary
// ---------------------------------------------------------

// gives:   new = 720  3600s  8760h  21.22  [10.3 99.3]

std::string
TsNormal::summary
(const int tab) const                        // note default
{
  const double spanHours = static_cast<double>(d_span) / 3600.0;
  std::ostringstream oss;
  oss << ""   << std::setw(tab) << d_label << " = ";
  oss << ""   << std::setw(5)   << d_count
      << "  " << std::setw(5)   << d_interval << "s"
      << "  " << std::setw(4)   << spanHours  << "h"
      << "  " << d_mean
      << "  " << "[" << d_min << " " << d_max << "]";
  return oss.str();
}

// ---------------------------------------------------------
//  MEMBER FUNCTION : report
// ---------------------------------------------------------

void
TsNormal::report
(std::ostream& os,
 const int     indent) const                 // note default
{
  const std::string pad(indent, ' ');
  const std::string label = "\"" + d_label + "\"";
  std::ostringstream oss1;
  oss1 << d_interval << "  "                                      // in seconds
       << (static_cast<double>(d_interval) / 3600.0);            // in fractional hours
  const std::string interval = oss1.str();
  std::ostringstream oss2;
  oss2 << d_span << "  "                                         // in seconds
       << (static_cast<double>(d_span) /       3600.0 ) << "  "  // in fractional hours
       << (static_cast<double>(d_span) / (24 * 3600.0));         // in fractional days
  const std::string span = oss2.str();

  os << pad << "timeseries report"                                                << "\n"
     << pad << "    label          : " << label                                   << "\n";
  if ( ! d_caller.empty() ) os << pad << "    caller         : " << d_caller      << "\n";
  os << pad << "    class          : " << "TsNormal"                              << "\n";
  os << pad << "  vector data                                    "
     << pad << "  statistics"                                                     << "\n"
     << pad << "    size           : " << boost::format("%-30d")  % d_size
     << pad << "    count          : " << boost::format("%20.5d") % d_count       << "\n"
     << pad << "    scaling        : " << boost::format("%-30g")  % d_scaling
     << pad << "    sum            : " << boost::format("%20.5f") % d_sum         << "\n"
     << pad << "    offsetting     : " << boost::format("%-30g")  % d_offsetting
     << pad << "    mean           : " << boost::format("%20.5f") % d_mean        << "\n"
     << pad << "  meta data                                        "
     << pad << "    stddev         : " << boost::format("%20.5f") % d_stddev      << "\n"
     << pad << "    interval [s,h] : " << boost::format("%-30s")  % interval
     << pad << "    median         : " << boost::format("%20.5f") % d_median      << "\n"
     << pad << "    29feb          : " << boost::format("%-30d")  % d_hasFeb29
     << pad << "    min            : " << boost::format("%20.5f") % d_min         << "\n"
     << pad << "    wrappable      : " << boost::format("%-30d")  % d_wrappable
     << pad << "    max            : " << boost::format("%20.5f") % d_max         << "\n"
     << pad << "    span   [s,h,d] : " << boost::format("%-30s")  % span
     << pad << "    range          : " << boost::format("%20.5f") % d_range       << "\n"
     << pad << "                     " << boost::format("%-30s")  % ""
     << pad << "    first          : " << boost::format("%20.5f") % d_first       << "\n"
     << pad << "                     " << boost::format("%-30s")  % ""
     << pad << "    last           : " << boost::format("%20.5f") % d_last        << "\n";
}

// ---------------------------------------------------------
//  MEMBER FUNCTION : sampleDuplicate (overloaded)
//  MEMBER FUNCTION : sampleLinear    (overloaded)
// ---------------------------------------------------------

TsNormal
TsNormal::sampleDuplicate
(const int          interval,
 const int          startOffset,
 const bool         leapYear,
 const double       scaleFactor,
 const double       offset) const
{
  const std::string label = "RESAMPLE";
  return sample(label,
                interval,
                startOffset,
                leapYear,
                scaleFactor,
                offset,
                TsBase::e_duplic);
}

TsNormal
TsNormal::sampleDuplicate
(const std::string& label,
 const int          interval,
 const int          startOffset,
 const bool         leapYear,
 const double       scaleFactor,
 const double       offset) const
{
  return sample(label,
                interval,
                startOffset,
                leapYear,
                scaleFactor,
                offset,
                TsBase::e_duplic);
}

TsNormal
TsNormal::sampleLinear
(const int          interval,
 const int          startOffset,
 const bool         leapYear,
 const double       scaleFactor,
 const double       offset) const
{
  const std::string label = "RESAMPLE";
  return sample(label,
                interval,
                startOffset,
                leapYear,
                scaleFactor,
                offset,
                TsBase::e_linear);
}

TsNormal
TsNormal::sampleLinear
(const std::string& label,
 const int          interval,
 const int          startOffset,
 const bool         leapYear,
 const double       scaleFactor,
 const double       offset) const
{
  return sample(label,
                interval,
                startOffset,
                leapYear,
                scaleFactor,
                offset,
                TsBase::e_linear);
}

// ---------------------------------------------------------
//  MEMBER FUNCTION : sample
// ---------------------------------------------------------
//  Description  : resample and export the current timeseries
//  Role         : client usage
//  Techniques   : custom algorithms (always using the "functor" identifier)
//  Status       : complete
//
//  Design notes
//
//      Three potential adjustments are made, in the following
//      order:
//
//        * resampling
//        * leap year adjustments - assumes start of year base
//        * startOffsetting
//
//      The current design is kinda wasteful.  The case of no
//      resampling, no leap year adjustment, and no startOffsetting,
//      still requires three identical vector copy operations.
//      This could be revised.
//
//      In passing, note the various "day count conventions" used
//      in financial mathematics.
//
//  Extension
//
//      These methods may be too simplistic for your needs.  More
//      sophisticated strategies include splines, curve fitting,
//      and techniques from timeseries analysis.  In which case,
//      the Boost Libraries, the GNU Scientific Library, and the
//      CzSpline library may be of interest.
//
// ---------------------------------------------------------

TsNormal
TsNormal::sample                             // workhorse
(const std::string&         label,
 const int                  interval,        // desired interval length [s]
 const int                  startOffset,
 const bool                 leapYear,
 const double               scaleFactor,
 const double               offset,
 const TsBase::SampleMethod method) const
{
  // initial reporting
  s_logger->repx(logga::adhc, "entering member function", "TsNormal");

  // integrity checks
  if ( ! TsBase::confirmInterval(interval) )
    {
      if ( d_caller.empty() )
        {
          s_logger->repx(logga::warn, "unsupported interval attempted", "");
        }
      else
        {
          s_logger->repx(logga::warn, "unsupported interval attempted, see", d_caller);
        }
    }
  if ( interval == 0 )
    {
      s_logger->repx(logga::warn, "div-by-zero exception immanent", interval);
    }

  // preliminaries
  const int  origSize      = d_size;
  const int  origInterval  = d_interval;
  const bool origFeb29     = d_hasFeb29;
  const bool origWrappable = d_wrappable;

  // CAUTION: reserve() can produce nasty physical side-effects
  // on partially filled vectors -- the function will not change
  // the logical contents, but it can invalidate iterators and
  // such, see Josuttis (1999 p151 footnote 7).

  // sample1 vector
  std::vector<double> sample1;
  int size1 = origSize;
  if ( interval > origInterval ) size1 *= interval / origInterval + 1;
  sample1.reserve(size1);

  // contract or grow as required
  if ( interval > origInterval )
    {
      // contraction
      const int shrink = interval / origInterval;
      xeona::ResampleTsCompress functor(shrink);
      functor(d_series.begin(), d_series.end(), std::back_inserter(sample1));
    }
  else if ( interval  < origInterval )
    {
      // expansion
      const int grow = origInterval / interval;
      switch ( method )
        {
        case TsBase::e_notSpecified:
          s_logger->repx(logga::warn, "no method specified", method);
          break;
        case TsBase::e_duplic:
          {                                  // CAUTION: enclosing blocks are necessary
            xeona::ResampleTsDuplicate functor(grow);
            functor(d_series.begin(), d_series.end(), std::back_inserter(sample1));
          }
          break;
        case TsBase::e_linear:
          {
            xeona::ResampleTsLinear functor(grow);
            functor(d_series.begin(), d_series.end(), std::back_inserter(sample1));
          }
          break;
        case TsBase::e_spline:
          s_logger->repx(logga::warn, "spline expansion not yet supported", "");
          break;
        default:
          std::clog << "** coding error 01 in source file " << __FILE__ << std::endl;
          break;
        }
    }
  else  // this is kinda wasteful
    {
      std::copy(d_series.begin(), d_series.end(), std::back_inserter(sample1));
    }

  // sample2 vector
  std::vector<double> sample2;
  int size2 = sample1.size();
  if ( leapYear == true ) size2 += 24 * (3600 / interval) + 1;
  sample2.reserve(size2);

  // releap or deleap year - assumes year start
  if ( leapYear == true && origFeb29 == false )
    {
      // duplicate 28-Feb data
      xeona::ModifyTsReLeapYear functor(interval);
      functor(sample1.begin(), sample1.end(), std::back_inserter(sample2));
    }
  else if ( leapYear == false && origFeb29 == true )
    {
      // remove 29-Feb data
      xeona::ModifyTsDeLeapYear functor(interval);
      functor(sample1.begin(), sample1.end(), std::back_inserter(sample2));
    }
  else  // this is kinda wasteful
    {
      std::copy(sample1.begin(), sample1.end(), std::back_inserter(sample2));
    }

  // process 'startOffset'
  std::vector<double> sample3;
  sample3.reserve(sample2.size());

  { // make the functor block-local like the others
    xeona::OffsetTs functor(startOffset, origWrappable);
    functor(sample2.begin(), sample2.end(), std::back_inserter(sample3));
  }

  // debug reporting
  std::ostringstream put;
  put << std::boolalpha
      << "  "
      << "interval = "             << interval       << "   "
      << "start offset = "         << startOffset    << "   "
      << "wrappable = "            << origWrappable  << "   "
      << "original has 29-Feb = "  << origFeb29      << "   "
      << "is leap year = "         << leapYear       << "\n";
  put << "  "
      << "d_series size = " << d_series.size()   << "   "
      << "sample 1 size = " << sample1.size()    << "   "
      << "sample 2 size = " << sample2.size()    << "   "
      << "sample 3 size = " << sample3.size()    << "\n";
  s_logger->repx(logga::adhc, "additional reporting follows", "");
  s_logger->putx(logga::adhc, put);

  // repack
  TsNormal buffer(sample3, interval, label, leapYear, origWrappable);

  // further adjustments -- the order is significant
  buffer.rescale(scaleFactor, "");
  buffer.reoffset(offset, "");

  // checks
  if ( s_checks )
    {
      const double origMean = getMean();
      const double buffMean = buffer.getMean();
      if ( ! considerEqual(origMean, buffMean ) )
        {
          std::ostringstream oss;
          oss << origMean << " " << buffMean;
          s_logger->repx(logga::rankJumpy, "the two means differ", oss.str());
        }
    }

  // return
  return buffer;

} // function 'TsNormal::sample'

// ---------------------------------------------------------
//  MEMBER FUNCTION : dayInfo
// ---------------------------------------------------------
//  Description  : extract a 24 hour timeseries for characterization purposes
//  Role         : typically, technical assets requiring additional context information
//  Techniques   : 'std::vector' 'std::deque'
//  Status       : complete
//
//  Design notes
//
//      Some entities require a 24 hour timeseries for
//      characterization.  One such example concerns the
//      calculation of building thermal performance, which needs
//      weather information from a few hours earlier to calculate
//      heat flows thru the structure.
//
//      This function extracts a twenty-four hour timeseries from
//      a full timeseries.  If the source timeseries is less than
//      48 hours long, some tricks are applied.  This would
//      normally only be necessary during testing when the time
//      horizon may only span say 6 hours.
//
//      The routine first attempts to shift forward or back a day
//      to get a clean sample.  If that fails, it creates a
//      timeseries that pads each end as required with
//      repetitions of the first and last elements respectively.
//
//      The same day-wise shift is also applied when sampling
//      from the beginning and end of a long simulation (a
//      context entity with "overhang" might be better but that
//      would also complicate matters considerably).
//
//  Semantic issues
//
//      In some cases two timeseries should be supplied -- one
//      containing specific data for ambient conditions
//      calculations and another containing multi-year averaged
//      data for design calculations.  In many cases, specific
//      data can be used for design calculations without
//      significant distortion -- if not, the modeler will need
//      to resort to two distinct timeseries.
//
// ---------------------------------------------------------

shared_ptr<std::vector<double> >
TsNormal::dayInfo
(const int                  pivot,
 const int                  interval,        // note default
 const bool                 leapYear,        // note default
 const TsBase::SampleMethod method) const    // note default
{
  // initial reporting
  s_logger->repx(logga::adhc, "entering member function, pivot", pivot);

  // check interval
  if ( ! TsBase::confirmInterval(interval) )
    {
      s_logger->repx(logga::warn, "invalid interval", interval);
    }

  // resample from this object
  TsNormal temp = this->sample("temp",       // label
                               interval,     // interval
                               0,            // start offset
                               leapYear,     // leap year
                               1.0,          // scale factor
                               0.0,          // offset
                               method);      // sample method enum

  // obtain the underlying vector
  std::vector<double> buffer(temp.recover());  // genuine deep copy

  // create and fill a 'std::deque' as this is more convenient than a 'std::vector'
  std::deque<double> deck(buffer.begin(), buffer.end());

  // preparation -- where "ipd" means intervals-per-day
  const int size = deck.size();              // timeseries length
  const int day  = 24 * 3600;                // one day in seconds
  const int ipd  = day / interval;           // integer arithmetic is correct

  // integrity checks
  if ( pivot > size )
    {
      std::ostringstream oss;
      oss << pivot << " > " << size;
      s_logger->repx(logga::warn, "mismatch, pivot > size", oss.str());
      shared_ptr<std::vector<double> > null; // null pointer
      return null;
    }

  // local variables
  int       center = pivot;
  const int num    = ipd;                    // 'num' could be potentially read in

  // attempt day-wise offset if required
  if ( center < num        ) center += num;
  if ( center > size - num ) center -= num;     // may reverse previous change

  // calculate the two indexes (somewhat clumsy but it works fine)
  int loIndex = center;
  int hiIndex = center;
  int count   = num;
  while ( --count )
    {
      if ( count % 2 == 0 ) ++hiIndex;       // 'count' is even
      else                  --loIndex;       // 'count' is odd
    }

  // pad the 'deck' from the front and back in case we need to sample from there
  for ( int i = 0; i < num; ++i ) deck.push_front(deck.front());
  for ( int i = 0; i < num; ++i ) deck.push_back(deck.back());

  // transfer the required range across to a smart pointer vector
  shared_ptr<std::vector<double> > info(new std::vector<double>());
  info->assign(deck.begin() + loIndex + num,      // 'num' due to the front padding
               deck.begin() + hiIndex + num + 1); // 'num' due to the front padding

  // another integrity check
  const int rsize = info->size();
  if ( rsize != num )
    {
      std::ostringstream oss;
      oss << rsize << " " << num;
      s_logger->repx(logga::warn, "size mismatch", oss.str());
    }

  // additional reporting as appropriate
  // YEEK 38 CODE (set by '--yeek')
  if ( xeona::yeek == 38 || xeona::yeek == 1 || xeona::yeek == 2 )
    {
      std::ostringstream put;                    // used in reporting
      put << "  size                : " << size     << "\n"
          << "  pivot (zero-based)  : " << pivot    << "\n"
          << "  interval length [s] : " << interval << "\n"
          << "  day length [s]      : " << day      << "\n";
      s_logger->repx(logga::dbug, "additional reporting follows, yeek", xeona::yeek);
      s_logger->putx(logga::dbug, put);
    }

  //return the smart pointer vector
  return info;

} // function 'TsNormal::dayInfo'

// ---------------------------------------------------------
//  MEMBER FUNCTION : clear
// ---------------------------------------------------------

void
TsNormal::clear
(const std::string& label)                   // note default
{
  TsBase::clear();
  // metadata
  d_interval = 0;
  if ( ! label.empty() ) d_label = label;
  d_hasFeb29  = false;
  d_span      = 0;
}

// ---------------------------------------------------------
//  MEMBER FUNCTION : add
// ---------------------------------------------------------
//  Description  : adds a pass-by-reference 'TsNormal' object to this object
//  Role         : client use
//  Techniques   : 'std::transform' 'std::plus<>', in-place modification
//  Status       : complete
//
//  Design notes
//
//      The two 'TsNormal' objects need identical metadata.
//
// ---------------------------------------------------------

bool
TsNormal::add
(const TsNormal&    more,
 const std::string& sublabel)
{
  // require identical metadata
  if ( ! sameMeta(more) )
    {
      s_logger->repx(logga::warn, "operation refused, metadata differs", "");
      return false;
    }

  // note if same object
  if ( this == &more )
    {
      std::ostringstream oss;
      oss << this << " " << &more << std::endl;
      s_logger->repx(logga::dbug, "adding same object", oss.str());
    }

  // process label
  if ( ! sublabel.empty() ) d_label += " : " + sublabel;

  // perform element-wise addition
  std::transform(d_series.begin(),         // refer <algorithm>
                 d_series.end(),
                 more.recover().begin(),   // second container
                 d_series.begin(),         // modify the first container in-place
                 std::plus<double>());     // refer <functional>

  // housekeeping
  calcStatistics();
  return true;
}

// UTILITY FUNCTIONS

// ---------------------------------------------------------
//  MEMBER FUNCTION : sameMeta
// ---------------------------------------------------------

bool
TsNormal::sameMeta
(const TsNormal& other) const
{
  return ( d_wrappable == other.isWrappable() &&
           d_size      == other.size()        &&
           d_interval  == other.getInterval() &&
           d_hasFeb29  == other.hasFeb29() );
}

// ---------------------------------------------------------
//  CLASS           : TsMonthly (TsBase specialization)
// ---------------------------------------------------------

// STATIC DEFINITIONS - with and without explicit initialization

#define S_MLS_SIZE 12                        // CAUTION: must equal the number of elements
const unsigned TsMonthly::s_mls[] = { 31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31 };
const std::vector<unsigned> TsMonthly::s_monthLengths(s_mls, s_mls + S_MLS_SIZE);

// ---------------------------------------------------------
//  MEMBER FUNCTION : TsMonthly (no-data ctor)
// ---------------------------------------------------------

TsMonthly::TsMonthly
(const std::string& label) :
  TsBase(label)
{
  s_logger->repx(logga::adhc, "constructor call", "no-data");
}

// ---------------------------------------------------------
//  MEMBER FUNCTION : TsMonthly (usual ctor, direct)
// ---------------------------------------------------------

TsMonthly::TsMonthly
(std::vector<double> original,
 const std::string&  label) :
  TsBase(label,
         original)
{
  // initial report
  s_logger->repx(logga::adhc, "constructor call", "direct");

  // silently accept 1 to 12 values, else complain
  if ( d_size < 1 )
    {
      s_logger->repx(logga::warn, "less than one month values, size", d_size);
    }
  if ( d_size > 12 )
    {
      s_logger->repx(logga::warn, "more than 12 month values, size", d_size);
    }
}

// ---------------------------------------------------------
//  MEMBER FUNCTION : TsMonthly (usual ctor, smart pointer)
// ---------------------------------------------------------

TsMonthly::TsMonthly
(shared_ptr<std::vector<double> > original,
 const std::string&               label) :
  TsBase(label,
         *original)
{
  // initial reporting
  s_logger->repx(logga::adhc, "constructor call", "smart pointer");

  // integrity checks for smart pointer
  if ( ! original )
    {
      s_logger->repx(logga::warn, "empty pointer (try new std::vector)", original);
    }

  // silently accept 1 to 12 values, else complain
  if ( d_size < 1 )
    {
      s_logger->repx(logga::warn, "less than one month values, size", d_size);
    }
  if ( d_size > 12 )
    {
      s_logger->repx(logga::warn, "more than 12 month values, size", d_size);
    }
}

// ---------------------------------------------------------
//  MEMBER FUNCTION : TsMonthly(const TsMonthly&) (copy ctor)
// ---------------------------------------------------------

TsMonthly::TsMonthly(const TsMonthly& orig) :          // copy constructor
  TsBase(orig)                                         // copy construct the base class
{
  s_logger->repx(logga::adhc, "copy constructor call", "TsMonthly");
}

// ---------------------------------------------------------
//  MEMBER FUNCTION : operator= (copy assignment)
// ---------------------------------------------------------

TsMonthly& TsMonthly::operator= (const TsMonthly& orig) // copy assignment operator
{
  s_logger->repx(logga::adhc, "copy assignment call", "TsMonthly");

  TsMonthly temp(orig);
  swap(temp);                                          // also swaps base class members
  return *this;
}

// ---------------------------------------------------------
//  MEMBER FUNCTION : ~TsMonthly (destructor)
// ---------------------------------------------------------

TsMonthly::~TsMonthly()
{
  s_logger->repx(logga::adhc, "destructor call", "");
}

// ---------------------------------------------------------
//  MEMBER FUNCTION : swap
// ---------------------------------------------------------

void
TsMonthly::swap(TsMonthly& rhs)
{
  s_logger->repx(logga::adhc, "entering member function", "TsMonthly");
  TsBase::swap(rhs);                                   // swap the base class too
}

// ---------------------------------------------------------
//  MEMBER FUNCTION : sameMetaData
// ---------------------------------------------------------

bool                                         // 'true' if the same
TsMonthly::sameMetaData
(const TsMonthly& other) const
{
  return sameMeta(other);                    // utility function
}

// ---------------------------------------------------------
//  MEMBER FUNCTION : summary
// ---------------------------------------------------------

// gives:  next = 12  21.22  [10.3 99.3]

std::string
TsMonthly::summary
(const int tab) const                        // note default
{
  std::ostringstream oss;
  oss << ""   << std::setw(tab) << d_label << " = ";
  oss << ""   << std::setw(5)   << d_count
      << "  " << d_mean
      << "  " << "[" << d_min << " " << d_max << "]";
  return oss.str();
}

// ---------------------------------------------------------
//  MEMBER FUNCTION : report
// ---------------------------------------------------------

void
TsMonthly::report
(std::ostream& os,
 const int     indent) const                 // note default
{
  const std::string pad(indent, ' ');
  const std::string label = "\"" + d_label + "\"";

  os << pad << "monthseries report"                                               << "\n"
     << pad << "    label          : " << label                                   << "\n";
  if ( ! d_caller.empty() ) os << pad << "    caller         : " << d_caller      << "\n";
  os << pad << "    class          : " << "TsMonthly"                             << "\n";
  os << pad << "  vector data                                    "
     << pad << "  statistics"                                                     << "\n"
     << pad << "    size           : " << boost::format("%-30d")  % d_size
     << pad << "    count          : " << boost::format("%20.5d") % d_count       << "\n"
     << pad << "    scaling        : " << boost::format("%-30g")  % d_scaling
     << pad << "    sum            : " << boost::format("%20.5f") % d_sum         << "\n"
     << pad << "    offsetting     : " << boost::format("%-30g")  % d_offsetting
     << pad << "    mean           : " << boost::format("%20.5f") % d_mean        << "\n"
     << pad << "  meta data                                        "
     << pad << "    stddev         : " << boost::format("%20.5f") % d_stddev      << "\n"
     << pad << "    wrappable      : " << boost::format("%-30d")  % d_wrappable
     << pad << "    median         : " << boost::format("%20.5f") % d_median      << "\n"
     << pad << "                     " << boost::format("%-30s")  % ""
     << pad << "    min            : " << boost::format("%20.5f") % d_min         << "\n"
     << pad << "                     " << boost::format("%-30s")  % ""
     << pad << "    max            : " << boost::format("%20.5f") % d_max         << "\n"
     << pad << "                     " << boost::format("%-30s")  % ""
     << pad << "    range          : " << boost::format("%20.5f") % d_range       << "\n";
}

// ---------------------------------------------------------
//  MEMBER FUNCTION : sampleDuplicate (overloaded)
// ---------------------------------------------------------

TsNormal
TsMonthly::sampleDuplicate
(const int          interval,
 const int          startOffset,
 const bool         leapYear,
 const double       scaleFactor,
 const double       offset) const
{
  const std::string label = "RESAMPLE";
  return sample(label,
                interval,
                startOffset,
                leapYear,
                scaleFactor,
                offset,
                TsBase::e_duplic);
}

TsNormal
TsMonthly::sampleDuplicate
(const std::string& label,
 const int          interval,
 const int          startOffset,
 const bool         leapYear,
 const double       scaleFactor,
 const double       offset) const
{
  return sample(label,
                interval,
                startOffset,
                leapYear,
                scaleFactor,
                offset,
                TsBase::e_duplic);
}

// ---------------------------------------------------------
//  MEMBER FUNCTION : sample
// ---------------------------------------------------------

TsNormal
TsMonthly::sample                            // workhorse
(const std::string&         label,
 const int                  interval,        // interval length [s]
 const int                  startOffset,
 const bool                 leapYear,
 const double               scaleFactor,
 const double               offset,
 const TsBase::SampleMethod method) const    // enum
{
  // initial report
  s_logger->repx(logga::adhc, "entering member function", "TsMonthly");

  // integrity checks
  if ( ! TsBase::confirmInterval(interval) )
    {
      if ( d_caller.empty() )
        {
          s_logger->repx(logga::warn, "unsupported interval attempted", "");
        }
      else
        {
          s_logger->repx(logga::warn, "unsupported interval attempted, see", d_caller);
        }
    }
  if ( interval == 0 )
    {
      s_logger->repx(logga::warn, "div-by-zero exception immanent", interval);
    }

  // preliminaries
  const int  origSize      = d_size;
  const bool origWrappable = ( d_size % 12 == 0 ) ? true : false;     // reverse engineer

  // sample vector
  std::vector<double> sample1;
  int size1 = origSize;
  size1 *= (3600 / static_cast<double>(interval)) * 24 * 31;
  sample1.reserve(size1);

  // CAUTION: reserve() can produce nasty physical side-effects
  // on partially filled vectors -- the function will not
  // change the logical contents, but it can invalidate
  // iterators and such, see Josuttis (1999 p151 footnote 7).

  // active code
  switch ( method )
    {
    case TsBase::e_notSpecified:
      s_logger->repx(logga::warn, "no method specified", method);
      break;
    case TsBase::e_duplic:
      {                                // CAUTION: enclosing blocks are necessary
        xeona::Month2TsDuplicate functor(interval, leapYear);
        functor(d_series.begin(), d_series.end(), std::back_inserter(sample1));
      }
      break;
    case TsBase::e_linear:
      {
        s_logger->repx(logga::warn, "linear interpolation not yet supported", "");
      }
      break;
    case TsBase::e_spline:
      s_logger->repx(logga::warn, "spline expansion not yet supported", "");
      break;
    default:
      std::clog << "** coding error 02 in source file " << __FILE__ << std::endl;
      break;
    }

  s_logger->repx(logga::adhc, "sample 1 size", sample1.size());

  // process 'startOffset'
  std::vector<double> sample2;
  sample2.reserve(sample1.size());

  xeona::OffsetTs functor(startOffset, origWrappable);
  functor(sample1.begin(), sample1.end(), std::back_inserter(sample2));

  s_logger->repx(logga::adhc, "sample 2 size", sample2.size());

  // repack
  TsNormal buffer(sample2, interval, label, leapYear, origWrappable);

  // further adjustments -- the order is significant
  buffer.rescale(scaleFactor, "");
  buffer.reoffset(offset, "");

  // checks
  if ( s_checks )
    {
      const double origMean = getMean();
      const double buffMean = buffer.getMean();
      if ( ! considerEqual(origMean, buffMean ) )
        {
          std::ostringstream oss;
          oss << origMean << " " << buffMean;
          s_logger->repx(logga::rankJumpy, "the two means differ", oss.str());
        }
    }

  // return
  return buffer;

} // function 'TsMonthly::sample'

// ---------------------------------------------------------
//  MEMBER FUNCTION : sameMeta
// ---------------------------------------------------------

bool
TsMonthly::sameMeta
(const TsMonthly& other) const
{
  return ( d_wrappable == other.isWrappable() &&
           d_size      == other.size() );
}

// ---------------------------------------------------------
//  CLASS           : TsGnuplot (no inheritance)
// ---------------------------------------------------------

// STATIC DEFINITIONS - with and without explicit initialization

logga::spLogger TsGnuplot::s_logger  = logga::ptrLogStream();

// ---------------------------------------------------------
//  MEMBER FUNCTION : TsGnuplot (zero-argument constructor)
// ---------------------------------------------------------

TsGnuplot::TsGnuplot() :
  d_metas(),
  d_caller(),                                // empty string
  d_gnuplot(xeona::gnuplot)                  // string set in 'common.cc'
{
  s_logger->repx(logga::adhc, "constructor call", "");
}

// ---------------------------------------------------------
//  MEMBER FUNCTION : TsGnuplot (destructor)
// ---------------------------------------------------------

TsGnuplot::~TsGnuplot()                      // destructor
{
  s_logger->repx(logga::adhc, "destructor call", "");
}

// ---------------------------------------------------------
//  MEMBER FUNCTION : generateTimestampUtc
// ---------------------------------------------------------

// requires linking to -lboost_date_time

std::string  // outputs: 2007-04-16 23:59:59 UTC
TsGnuplot::generateTimestampUtc() const
{
  boost::posix_time::ptime now  = boost::posix_time::second_clock::universal_time();
  const std::string        date = boost::gregorian::to_iso_extended_string(now.date());
  const std::string        time = boost::posix_time::to_simple_string(now.time_of_day());
  return date + " " + time + " UTC";
}

// ---------------------------------------------------------
//  MEMBER FUNCTION : addCaller
// ---------------------------------------------------------

void
TsGnuplot::addCaller                         // added to right-hand side of plot
(const std::string file,                     // usually __FILE__
 const int         line)                     // usually __LINE__
{
  std::ostringstream oss;
  oss << file << ":" << line;
  d_caller = oss.str();
}

// ---------------------------------------------------------
//  MEMBER FUNCTION : addTs (normal)
// ---------------------------------------------------------

bool                                         // 'false' means 'series' rejected
TsGnuplot::addTs
(const TsNormal& series)
{
  if ( series.empty() ) return false;
  d_metas.push_back(series);                 // object copied in
  return true;
}

// ---------------------------------------------------------
//  MEMBER FUNCTION : addTs (monthly hack)
// ---------------------------------------------------------

// this is a bit rough but if it proves useful the concept could
// be developed

bool                                         // 'false' means 'series' rejected
TsGnuplot::addTs
(const TsMonthly& series,
 const int        fakeInterval)                // note default
{
  // preamble
  const std::string label = series.getLabel();
  const bool hasFeb29     = false;
  const int  len          = series.size();
  const bool loopable     = ( len % 12 == 0  ? true : false );

  // fake a normal timeseries
  TsNormal temp(series.pointer(),            // vector as smart pointer
                fakeInterval,
                label,
                hasFeb29,
                loopable);

  // add
  return addTs(temp);                        // copied in
}

// ---------------------------------------------------------
//  MEMBER FUNCTION : reset
// ---------------------------------------------------------

int                                          // number of deletions
TsGnuplot::reset()
{
  const int count = d_metas.size();
  d_metas.clear();
  return count;
}

// ---------------------------------------------------------
//  MEMBER FUNCTION : plot
// ---------------------------------------------------------
//  Description  : plots preloaded timeseries
//  Role         : client use
//  Techniques   : class 'Gnuplot' from unit 'i/gnuplot', 'xeona::tout' global variable
//  Status       : complete
//
//  Design notes
//
//      The plotting of 'TsMonthly' objects via 'addTs' is a
//      little untidy but otherwise works fine.  The design could
//      be cleaned up however.
//
// ---------------------------------------------------------

bool                                         // 'false' means no attempt to plot
TsGnuplot::plot
(const std::string& title,
 const int          truncate,                // note default is zero
 const char         plottype) const          // note default
{
  // initial reporting
  std::ostringstream oss;
  oss << "\"" << xeona::tout << "\"";    // place in double-quotes
  s_logger->repx(logga::dbug, "entering member function, gnuplot", oss.str());

  // abandon if 'xeona::tout' not set
  if ( xeona::tout.empty() )
    {
      // some wasted execution, but simpler to abandon here once
      // than to always test 'xeona::tout' in the client code
      s_logger->repx(logga::adhc, "abandoning execution at start", "");
      return false;
    }

  // protection
  if ( d_metas.empty() )
    {
      return false;
    }

  // determine "stylespec"
  std::string with;
  switch ( plottype )
    {
    case 'l': with = "lines";       break;
    case 'p': with = "linespoints"; break;
    case 's': with = "histeps";     break;
    default:
      // complain
      with = "linespoints";
      break;
    }

  // hard-codes and revisions
  std::string plotTitle = title;
  if ( plotTitle.empty() ) plotTitle = " ";       // for side-effect to retain plot space

  // LOOP A : process the timebase and y-values, also report
  const int bigint = std::numeric_limits<int>::max();
  int loSpan       = bigint;                      // least span
  int hiSpan       = 0;                           // longest span
  int loInterval   = bigint;                      // least interval
  int hiInterval   = 0;                           // longest interval
  const double inf = std::numeric_limits<double>::infinity();
  double min = +inf;
  double max = -inf;
  int    cnt = 0;
  std::ostringstream put;
  BOOST_FOREACH( TsNormal meta, d_metas )
    {
      const int localSpan     = meta.getSpan();
      const int localInterval = meta.getInterval();
      if ( localSpan < loSpan ) loSpan = localSpan;
      if ( localSpan > hiSpan ) hiSpan = localSpan;
      if ( localInterval < loInterval ) loInterval = localInterval;
      if ( localInterval > hiInterval ) hiInterval = localInterval;
      const int    localCnt = meta.getCount();
      const double localMin = meta.getMin();
      const double localMax = meta.getMax();
      if ( localCnt > cnt ) cnt = localCnt;
      if ( localMin < min ) min = localMin;
      if ( localMax > max ) max = localMax;
      meta.report(put);                      // report call
    }
  s_logger->putx(logga::dbug, put);

  // LOOP B : formulate the plot command
  double       colrat = 0.0;
  const double doubleLoInterval = static_cast<double>(loInterval);    // becomes timebase
  std::ostringstream ossPlot;
  ossPlot << "plot";
  BOOST_FOREACH( TsNormal meta, d_metas )
    {
      const std::string label    = meta.getLabel();
      const int         interval = meta.getInterval();
      const double      ratio = static_cast<double>(interval) / doubleLoInterval;
      const std::string scale = boost::str(boost::format("%.3f") % ratio);
      const double      ofset = (ratio - 1.0) / 2.0;   // more intuitive with stairs
      const std::string shift =  boost::str(boost::format("%.3f") % ofset);
      ossPlot << " '-'";
      ossPlot << "using ($0 * " << scale << " + "<< shift << "):1";
      ossPlot << " with " << with;
      if ( ! label.empty() ) ossPlot  << " title '" << label << "'";  // line label
      ossPlot << "linecolor palette fraction " << (colrat++ / 5.0);   // line color
      ossPlot << ",";
    }
  std::string plotCommand = ossPlot.str();
  plotCommand.erase(plotCommand.length() - 1);    // remove the final comma

  // formulate 'yrange' information: x-axis sticky or grow accordingly
  const double extra = 0.1;                  // adjust here to set level of undercrowding
  double lo = min;
  double hi = max;
  if ( hi > 0.0 ) hi = (1.0 + extra) * hi;
  else            hi = 0.0;
  if ( lo > 0.0 ) lo = 0.0;
  else            lo = (1.0 + extra) * lo;;

  // create x-label
  const double loCount = static_cast<double>(loSpan) / doubleLoInterval;
  const double hiCount = static_cast<double>(hiSpan) / doubleLoInterval;
  std::ostringstream ossXlab;
  ossXlab << "step count [" << loInterval << "s]";
  if ( loInterval == hiInterval ) ossXlab << "";
  else                            ossXlab << " / interval range "
                                          << loInterval << " " << hiInterval;
  if ( loCount == hiCount ) ossXlab << " / count " << loCount;
  else                      ossXlab << " / count range " << loCount << " " << hiCount;
  if ( truncate > 0 ) ossXlab << " / truncated to " << truncate;
  const std::string xLabel = ossXlab.str();

  // create small y2 label
  std::ostringstream ossNote;
  ossNote << "created : " << "TsGnuplot::" << __func__;
  if ( ! d_caller.empty() ) ossNote << " | source : " << d_caller;
  ossNote << " | date : " << generateTimestampUtc();
  const std::string note = ossNote.str();
  const std::string smallFont = "center font 'arial,9'";

  // formulate 'xtics' information
  int xlast = 0;
  if ( truncate > 0 ) xlast = truncate - 1;
  else                xlast = cnt - 1;
  int xinc = 0;
  if      ( xlast <   20 ) xinc =    1;
  else if ( xlast <  200 ) xinc =   10;
  else if ( xlast < 2000 ) xinc =  100;
  else                     xinc = 1000;
  std::ostringstream ossXtics;
  ossXtics << 0 << "," << xinc << "," << xlast;
  const std::string xTics = ossXtics.str();

  // abandon if 'gnuplot' call string is NOT set in 'common.cc'
  if ( xeona::gnuplot.empty() )
    {
      s_logger->repx(logga::adhc, "xeona::gnuplot empty, cannot plot", xeona::gnuplot);
      return false;
    }

  // make our 'Gnuplot' object block-local
  {
    // constructor call
    Gnuplot gp(d_gnuplot,                    // gnuplot invocation command
               Gnuplot::e_debugLogsOff);     // set dedicated logging status

    // set the terminal up
    if ( xeona::tout.empty() )
      {
        s_logger->repx(logga::warn, "coding problem", "refer to code");
        return false;
      }
    else if ( xeona::tout == "dumb" )
      {
        // CAUTION: do not call 'setPlotWindowTitle(plotTitle)' as there is no plot window
        gp.setTerminal("dumb");              // dumb terminal means the console
        gp.setPlotWindowSize(148, 40);       // width (cols) x height (lines)
      }
    else if ( xeona::tout == "x11" )
      {
        // CAUTION: 'setPersist' does not work (use 'wxt' instead)
        gp.setTerminal("x11");               // standard x11 libraries
        gp.setPlotWindowTitle(plotTitle);    // also prepends a counter
      }
    else if ( xeona::tout == "wxt" )
      {
        gp.setTerminal("wxt");               // best with gnuplot version 4.4
        gp.setPersist();                     // keep open after client terminates
        gp.setPlotWindowSize(1100, 600);     // width (px) x height (px) [1]
        gp.setPlotWindowTitle(plotTitle);    // also prepends a counter
      }
    else if ( xeona::tout == "svg" )
      {
        // unique filename
        const std::string stub    = "gnuplot-";
        const std::string svgname = xeona::temporaryFilename("svg", stub);

        // gnuplot
        gp.setTerminal("svg");               // SVG output
        gp.setOutput(svgname);
      }
    else
      {
        std::clog << "** coding error 03 in source file " << __FILE__ << std::endl;
      }

    // gnuplot model
    gp << "set title '" << plotTitle << "'"               << "\n"; // plot title
    gp << "set xlabel '" << xLabel << "'"                 << "\n"; // x-axis label
    gp << "set y2label '" << note << "' " << smallFont    << "\n"; // y2-axis note
    gp << "unset colorbox"                                << "\n"; // suppress sidebox
    gp << "set offsets 1,1,0,0"                           << "\n"; // l r t b
    gp << "set xtics " << xTics                           << "\n"; // x-axis tics
    if ( lo != hi ) gp << "set yrange [" << lo << ":" << hi << "]" << "\n"; // range [2]
    gp << plotCommand                                     << "\n";

    // LOOP C : gnuplot data -- note that truncation occurs after y-axis ranging
    BOOST_FOREACH( TsNormal meta, d_metas )
      {
        if ( truncate > 0 && truncate < meta.getCount() ) // conditional truncation [3]
          {
            std::vector<double> temp(meta.recover());     // break the 'const' reference
            temp.resize(truncate, 0.0);                   // can never grow
            gp.send(temp);
          }
        else
          {
            gp.send(meta.recover());
          }
      }

    // finalize as required
    if  ( xeona::tout == "svg" )
      {
        gp.setFinalize();
      }

  } // 'Gnuplot' object will destruct on block exit

  // [1] requires gnuplot version 4.4, largest on 'hinau' is 1350 x 650
  // [2] else 'gnuplot' complains that ymin = ymax
  // [3] only truncate when requested and when too long, lest
  //     we get default constructed zeros

  // signal a genuine attempt to plot
  return true;
}

// ---------------------------------------------------------
//  FREE FUNCTION   : operator== (TsNormal&, TsNormal&)
// ---------------------------------------------------------
//  Description  : operates on two 'TsNormal' objects
//  Role         : general use
//  Techniques   : member data comparison
//  Status       : complete
// ---------------------------------------------------------

bool
operator==
(const TsNormal& lhs,
 const TsNormal& rhs)
{
  return (lhs.d_series     == rhs.d_series &&
          lhs.d_wrappable  == rhs.d_wrappable &&
          lhs.d_scaling    == rhs.d_scaling &&
          lhs.d_offsetting == rhs.d_offsetting &&
          lhs.d_interval   == rhs.d_interval &&
          lhs.d_label      == rhs.d_label &&
          lhs.d_hasFeb29   == rhs.d_hasFeb29);
}

// ---------------------------------------------------------
//  FREE FUNCTION   : operator!= (TsNormal&, TsNormal&)
// ---------------------------------------------------------
//  Description  : operates on two 'TsNormal' objects
//  Role         : general use
//  Techniques   : member data comparison
//  Status       : complete
// ---------------------------------------------------------

bool
operator!=
(const TsNormal& lhs,
 const TsNormal& rhs)
{
  if ( lhs == rhs ) return false;
  else              return true;
}

// ---------------------------------------------------------
//  FREE FUNCTION   : operator== (TsMonthly&, TsMonthly&)
// ---------------------------------------------------------
//  Description  : operates on two 'TsMonthly' objects
//  Role         : general use
//  Techniques   : member data comparison
//  Status       : complete
// ---------------------------------------------------------

bool
operator==
(const TsMonthly& lhs,
 const TsMonthly& rhs)
{
  return (lhs.d_series == rhs.d_series &&
          lhs.d_label  == rhs.d_label);
}

// ---------------------------------------------------------
//  FREE FUNCTION   : operator!= (TsMonthly&, TsMonthly&)
// ---------------------------------------------------------
//  Description  : operates on two 'TsMonthly' objects
//  Role         : general use
//  Techniques   : member data comparison
//  Status       : complete
// ---------------------------------------------------------

bool
operator!=
(const TsMonthly& lhs,
 const TsMonthly& rhs)
{
  if ( lhs == rhs ) return false;
  else              return true;
}

// ---------------------------------------------------------
//  FREE FUNCTION   : sameMetaData (TsNormal)
// ---------------------------------------------------------

bool
sameMetaData
(const TsNormal& lhs,
 const TsNormal& rhs)
{
  return lhs.sameMetaData(rhs);
}

// ---------------------------------------------------------
//  FREE FUNCTION   : sameMetaData (TsMonthly)
// ---------------------------------------------------------

bool
sameMetaData
(const TsMonthly& lhs,
 const TsMonthly& rhs)
{
  return lhs.sameMetaData(rhs);
}

//  end of file

