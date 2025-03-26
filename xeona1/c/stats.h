//  XEONA ENERGY SYSTEMS MODELING ENVIRONMENT
//
//  file-create-name : stats.h
//  file-create-date : Wed 16-Sep-2009 13:34 UTC
//  file-initiator   : Robbie Morrison <robbie@actrix.co.nz>
//
//  file-role        : on-the-fly statistical calculations / header
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
//  $URL: file:///home/robbie/svn-root/xeona/futz/trunk/xeona1/c/stats.h $
//
//  GENERAL NOTES FOR THIS FILE

//  HEADER GUARD

#ifndef _STATS_H_
#define _STATS_H_

//  AD-HOC NOTES
//
//  Code originally developed in 'frag-stats-on-the-fly-3.cc'.

//  LOCAL AND SYSTEM INCLUDES

#include "../a/logger_fwd.h"  // some forward declarations from "logger.h"
#include "../c/smart_ptr.h"   // toggle between Boost and TR1 smart pointers

#include <algorithm>          // STL copying, searching, and sorting
#include <cmath>              // C-style maths, ceil(), floor(), sqrt()
#include <limits>             // numeric_limits<T>::infinity() and similar
#include <string>             // C++ strings
#include <vector>             // STL sequence container

#include <boost/foreach.hpp>  // BOOST_FOREACH iteration macro

//  CODE

// ---------------------------------------------------------
//  documentation   : statistical calculations
// ---------------------------------------------------------
//
//  Source
//
//      Lischner (2003 pp269-270) with two corrections and some
//      additions.
//
//      Lischner notes: "pass an instance of 'Statistics' to the
//      'for_each' algorithm to accumulate the statistics. The
//      copy that is returned from 'for_each' contains the
//      desired results."  (p269)
//
//  Purpose
//
//      Compute statistics on the fly using a functor approach.
//      No copies of the original data are kept.
//
//      Note that it is not possible to track the median value,
//      or percentiles more generally, on-the-fly.  The entire
//      dataset is required for sorting prior to calculating such
//      statistics.
//
//  CAUTION: int specialization
//
//      As currently coded, Statistics<int> will use truncated
//      arithmetic and create false results (this could be
//      readily fixed however).
//
//  CAUTION: confirm formulas
//
//      Users should verify the statistical formulas for
//      themselves, particularly the (n - 1) divisor for
//      variance.
//
//  Further comments
//
//      The functor is templated, thereby allowing the calling
//      code to specify the type (fundamental or user-defined)
//      which most suits their requirements.
//
//      Non-computable expressions naturally return 'nan' (for
//      not-a-number), rather than throwing and then crashing.
//      Hence, empty and one-element datasets both yield
//      intuitive output.  This feature is rather convenient.
//
//  Tested
//
//      OUTPUT dataset is from Stephens etal (2006 p405).
//
//  Development environment
//
//      - GNU GCC 4.1.2 compiler (using -Wall -Weffc++ -pedantic)
//      - Linux Ubuntu 6.10 operating system (now superseded by 7.04)
//      - run-time memory checking with valgrind ver "valgrind-3.2.0-Debian"
//      - laptop is Toshiba Tecra A2 330, new 2004, 1.4GHz Intel Celeron M
//
//  Note on 'std::for_each'
//
//      'std::for_each' requires an operation (function or
//      functor) that uses call-by-reference semantics -- this
//      then allows elements to be modified directly (by
//      contrast, 'std::transform' uses call-by-value semantics
//      and requires a return value).  Josuttis (1999 p325)
//      discusses this topic in detail.
//
//  References
//
//    Josuttis, Nicolai M.  1999.  The C++ Standard Library : a
//      tutorial and reference.  Addison-Wesley, Boston, USA.  ISBN
//      0-201-37926-0.
//
//    Lischner, Ray.  2003.  C++ in a nutshell : a language and
//      library reference, O'Reilly and Associates, Sebastopol,
//      California, USA.  ISBN 0-596-00298-X.
//
//    Stephens, D Ryan, Christopher Diggins, Jonathan Turkanis,
//      and Jeff Cogswell.  2006.  C++ cookbook : solutions and
//      examples for C++ programmers.  O'Reilly Media, Sebastopol,
//      California, USA.  ISBN 0-596-00761-2.
//
// ---------------------------------------------------------

// ---------------------------------------------------------
//  CLASS           : Statistics <>
// ---------------------------------------------------------
//  Description  : calculate and store statistics on-the-fly
//  Role         : general use, including by 'xeona::fillStatistics'
//  Techniques   : header implementation
//  Status       : complete
//
//  CAUTION: (n - 1) divisor
//
//      Users should verify the statistical formulas for
//      themselves, particularly the (n - 1) divisor for
//      variance.
//
// ---------------------------------------------------------

template <typename T>
class Statistics
{
public:

  // CREATORS

  Statistics() :
    d_count(0),
    d_first(std::numeric_limits<T>::quiet_NaN()), // set to nonsensical value
    d_last(std::numeric_limits<T>::quiet_NaN()),  // set to nonsensical value
    d_sum(0),
    d_sumsq(0),
    d_min(+std::numeric_limits<T>::infinity()),   // set to maximum possible
    d_max(-std::numeric_limits<T>::infinity()),   // set to minimum possible
    d_zeros(0)
  {
  }

  Statistics(const std::vector<T>& timeseries) :
    d_count(0),
    d_first(std::numeric_limits<T>::quiet_NaN()), // set to nonsensical value
    d_last(std::numeric_limits<T>::quiet_NaN()),  // set to nonsensical value
    d_sum(0),
    d_sumsq(0),
    d_min(+std::numeric_limits<T>::infinity()),   // set to maximum possible
    d_max(-std::numeric_limits<T>::infinity()),   // set to minimum possible
    d_zeros(0)
  {
    operator()(timeseries);
  }

  Statistics(const shared_ptr<std::vector<T> >& timeseries) :
    d_count(0),
    d_first(std::numeric_limits<T>::quiet_NaN()), // set to nonsensical value
    d_last(std::numeric_limits<T>::quiet_NaN()),  // set to nonsensical value
    d_sum(0),
    d_sumsq(0),
    d_min(+std::numeric_limits<T>::infinity()),   // set to maximum possible
    d_max(-std::numeric_limits<T>::infinity()),   // set to minimum possible
    d_zeros(0)
  {
    operator()(timeseries);
  }

  // FUNCTION CALL OPERATOR

  // the 'void' function call operator is essentially wrapper to
  // the same operator carrying an 'inf' -- it is provided for
  // notational convenience

  void operator() ()
  {
    const T inf = std::numeric_limits<T>::infinity();
    operator()(inf);
  }

  void operator() (T x)
  {
    d_count += 1;
    if ( d_count == 1 ) d_first = x;
    d_last   = x;
    d_sum   += x;
    d_sumsq += x * x;
    if ( x < d_min ) d_min = x;
    if ( x > d_max ) d_max = x;
    if ( x == 0 ) d_zeros += 1;
  }

  void operator() (const std::vector<T>& timeseries)
  {
    *this = std::for_each
      (timeseries.begin(),
       timeseries.end(),
       Statistics<T>());
  }

  void operator() (const shared_ptr<std::vector<T> >& timeseries)
  {
    *this = std::for_each
      (timeseries->begin(),
       timeseries->end(),
       Statistics<T>());
  }

  // ACCESSORS

  operator bool() const { return ( d_count > 0 ); }         // 'false' is not yet loaded

  int    count()      const { return d_count; }
  T      first()      const { return d_first; }              // first entry
  T      last()       const { return d_last; }               // last entry
  T      sum()        const { return d_sum; }
  T      sumsq()      const { return d_sumsq; }
  T      mean()       const { return d_sum / d_count; }
  T      var()        const { return (d_sumsq - d_sum * d_sum / d_count) / (d_count - 1);}
  T      var2()       const { return (d_sumsq - d_sum * d_sum / d_count) / (d_count); }
  T      sdev()       const { return std::sqrt(var()); }     // refer <cmath>
  T      sdev2()      const { return std::sqrt(var2()); }
  T      min()        const { return d_min; }
  T      max()        const { return d_max; }
  T      range()      const { return d_max - d_min; }
  int    zeros()      const { return d_zeros; }
  int    nonzeros()   const { return d_count - d_zeros; }
  double zerorat()    const { return double(d_zeros)           / double(d_count); }
  double nonzerorat() const { return double(d_count - d_zeros) / double(d_count); }

  T      opmean()     const                  // "opmean" for operational mean
  {
    if ( d_zeros == d_count ) return T();
    else                      return d_sum / (d_count - d_zeros);
  }

  bool   strictPos()  const { return ( d_min >  0.0 ); }
  bool   strictNeg()  const { return ( d_max <  0.0 ); }
  bool   zeroPos()    const { return ( d_min >= 0.0 ); }
  bool   zeroNeg()    const { return ( d_max <= 0.0 ); }
  bool   range01()    const { return ( d_min >= 0.0 && d_max <= 1.0 ); }

  // INSTANCE DATA

private:

  // on-the-fly registers

  int    d_count;                            // was std::size_t (two places)
  T      d_first;                            // first entry
  T      d_last;                             // last entry
  T      d_sum;
  T      d_sumsq;                            // sum of squares
  T      d_min;
  T      d_max;
  int    d_zeros;

}; // class template 'Statistics<>'

// ---------------------------------------------------------
//  FREE FUNCTION   : xeona::fillStatistics <>
// ---------------------------------------------------------
//  Description  : simplify filling of 'Statistics' objects
//  Role         : general used, including entity 'conclude' calls
//  Techniques   : 'std::for_each'
//  Status       : complete
//
//  Design notes
//
//      Superseded by the overloaded function operators above.
//
// ---------------------------------------------------------

namespace xeona
{

  template <typename T>
  const Statistics<T>
  fillStatistics
  (const std::vector<T>& timeseries)
  {
    Statistics<T> stat = std::for_each
      (timeseries.begin(),
       timeseries.end(),
       Statistics<T>());
    return stat;
  }

  template <typename T>
  const Statistics<T>
  fillStatistics
  (const shared_ptr<std::vector<T> >& timeseries)
  {
    Statistics<T> stat = std::for_each
      (timeseries->begin(),
       timeseries->end(),
       Statistics<T>());
    return stat;
  }

} // namespace 'xeona'

#endif // _STATS_H_

//  end of file

