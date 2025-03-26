//  XEONA ENERGY SYSTEMS MODELING ENVIRONMENT
//
//  file-create-name : opssched.cc
//  file-create-date : Mon 08-Nov-2010 22:41 UTC
//  file-initiator   : Robbie Morrison <robbie@actrix.co.nz>
//
//  file-role        : class to track clock time and schedule plant / implementation
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
//  $URL: file:///home/robbie/svn-root/xeona/futz/trunk/xeona1/c/opssched.cc $
//
//  GENERAL NOTES FOR THIS FILE

//  LOCAL AND SYSTEM INCLUDES

#include "opssched.h"         // companion header for this file (place first)

#include "../a/logger.h"      // standard logging functionality (as required)
#include "../c/smart_ptr.h"   // toggle between Boost and TR1 smart pointers
#include ".././common.h"      // common definitions for project (place last)

#include <iostream>           // standard io
#include <ostream>            // output streams
#include <sstream>            // string-streams
#include <string>             // C++ strings

#include <boost/foreach.hpp>  // BOOST_FOREACH iteration macro
#include <boost/format.hpp>   // printf style formatting

//  CODE

// ---------------------------------------------------------
//  CLASS           : OpsScheduler
// ---------------------------------------------------------

// STATIC DEFINITIONS - with and without explicit initialization

logga::spLogger OpsScheduler::s_logger = logga::ptrLogStream();

// ---------------------------------------------------------
//  MEMBER FUNCTION : OpsScheduler
// ---------------------------------------------------------
//  Description  : constructor
//  Role         : client usage
//  Techniques   : modular arithmetic
//  Status       : complete
//
//  CAUTION: intervals over one hour
//
//      The 'OpsScheduler' does not support 'xeona' intervals
//      exceeding one hour -- which means that entities using the
//      operations scheduler also share this restriction.
//
// ---------------------------------------------------------

OpsScheduler::OpsScheduler
(const int interval,                         // normal horizon interval [s]
 const int offsetHour) :                     // starting hour, zero means midnight
  d_interval(interval),
  d_offsetHour(offsetHour),
  d_workStartHour(0),                        // default value, reset via 'setSchedule'
  d_workStopHour(0),                         // default value, reset via 'setSchedule'
  d_count(0),
  d_days(0),
  d_hours(offsetHour),
  d_minutes(0)
{
  // initial reporting
  s_logger->repx(logga::adhc, "constructor call", "");

  // integrity checks
  const int i = interval;                    // for convenience
  if ( i == 0 )                              // zero occurs in some forms of testing,
    {                                        //   including under the 'xmok' script
      s_logger->repx(logga::info, "zero interval value", interval);
    }
  else if ( i ==  300 || i ==  600 || i ==  900 || i == 1200 || i == 1800 || i == 3600 )
    {
      s_logger->repx(logga::adhc, "acceptable interval value", interval);
    }
  else
    {
      s_logger->repx(logga::warn, "unacceptable interval value", interval);
    }

  // bed-in 'offset's exceeding 23 hours
  d_days  += d_hours / 24;
  d_hours %= 24;                           // roll over if required
}

// ---------------------------------------------------------
//  MEMBER FUNCTION : ~OpsScheduler
// ---------------------------------------------------------

OpsScheduler::~OpsScheduler()
{
  // initial reporting
  s_logger->repx(logga::adhc, "destructor call", "");
}

// MANIPULATORS

// ---------------------------------------------------------
//  MEMBER FUNCTION : setSchedule
// ---------------------------------------------------------

int                                        // final hour
OpsScheduler::setSchedule
(const int startHour,
 const int stopHour)
{
  d_workStartHour    = startHour;
  d_workStopHour     = stopHour;
  const int duration = d_workStopHour - d_workStartHour;

  // integrity checks
  if (    startHour < 0 || startHour > 24
       || stopHour  < 0 || stopHour  > 24
       || startHour > stopHour )
    {
      s_logger->repx(logga::warn, "unacceptable schedule", showSchedule());
    }
  else
    {
      s_logger->repx(logga::adhc, "schedule set", showSchedule());
    }

  return duration;
}

// ---------------------------------------------------------
//  MEMBER FUNCTION : operator++
// ---------------------------------------------------------

OpsScheduler&
OpsScheduler::operator++ ()
{
  ++d_count;

  d_minutes += d_interval / 60;              // interval is always a multiple of 60
  d_hours   += d_minutes  / 60;
  d_days    += d_hours    / 24;
  d_minutes %= 60;                           // grab residual
  d_hours   %= 24;                           // roll over if required

  return *this;
}

// ---------------------------------------------------------
//  MEMBER FUNCTION : bump
// ---------------------------------------------------------

bool                                         // 'true' if moved forward
OpsScheduler::bump                           // attempt to update scheduler
(const int currentStep)                      // usually 'd_step'
{
  const int diff = currentStep - d_count;
  switch ( diff )
    {
    case 0:                                  // current
      return false;
    case 1:                                  // stale
      ++(*this);                             // CAUTION: dereference necessary
      return true;
    default:                                 // should never get here
      std::clog << "** coding error 01 in source file " << __FILE__ << std::endl;
      return false;
    }
}

// ---------------------------------------------------------
//  MEMBER FUNCTION : skipForward
// ---------------------------------------------------------

void
OpsScheduler::skipForward
(const unsigned skip)
{
  s_logger->repx(logga::info, "clock being skipped by", skip);
  for ( unsigned i = 0; i < skip; ++i ) ++(*this);
}

// ---------------------------------------------------------
//  MEMBER FUNCTION : getInterval
//  MEMBER FUNCTION : getIntervalCount
//  MEMBER FUNCTION : getElapsedSeconds
//  MEMBER FUNCTION : getOffsetHour
//  MEMBER FUNCTION : getDuration
// ---------------------------------------------------------

// ACCESSORS

int OpsScheduler::getInterval()       const { return d_interval; }
int OpsScheduler::getIntervalCount()  const { return d_count; }
int OpsScheduler::getElapsedSeconds() const { return d_count * d_interval; }
int OpsScheduler::getOffsetHour()     const { return d_offsetHour; }
int OpsScheduler::getDuration()       const { return d_workStopHour - d_workStartHour; }

// ---------------------------------------------------------
//  MEMBER FUNCTION : getFractionalDay
// ---------------------------------------------------------

double
OpsScheduler::getFractionalDay() const
{
  const double partDay
    = (static_cast<double>(d_hours)   /       24)
    + (static_cast<double>(d_minutes) / (60 * 24));
  return partDay;
}

// ---------------------------------------------------------
//  MEMBER FUNCTION : isWorkingHour
// ---------------------------------------------------------
//  Description  : indicates whether the current step is a working hour or not
//  Role         : client use
//  Techniques   : arithmetic comparison
//  Status       : complete
//
//  Design notes
//
//      Assume the period 08:00 thru 18:00.
//
//      Under hash-if "1" (see code) the first working step is 8.
//      Under hash-if "0" (see code) the first working step is 7.
//
//      The "1" arrangement was selected as correct.
//
//  CAUTION: increment first
//
//      The client code must increment the scheduler first using
//      the prefix increment operator.  This is almost certainly
//      a design flaw and not a feature.
//
// ---------------------------------------------------------

bool
OpsScheduler::isWorkingHour() const
{
#if 1 // 0 = hour earlier, 1 = hour later
  if ( d_hours  > d_workStartHour && d_hours <= d_workStopHour ) return true;
#else
  if ( d_hours >= d_workStartHour && d_hours <  d_workStopHour ) return true;
#endif // 0

  else                                                           return false;
}

// ---------------------------------------------------------
//  MEMBER FUNCTION : getDaysHoursMinutes
// ---------------------------------------------------------

OpsScheduler::DHM_type
OpsScheduler::getDaysHoursMinutes() const
{
  return boost::make_tuple(d_days, d_hours, d_minutes);
}

// ---------------------------------------------------------
//  MEMBER FUNCTION : showCurrent
// ---------------------------------------------------------

std::string
OpsScheduler::showCurrent() const
{
  std::ostringstream oss;
  oss << boost::format("%02d:%02d") % d_hours % d_minutes;
  return oss.str();
}

// ---------------------------------------------------------
//  MEMBER FUNCTION : showSchedule
// ---------------------------------------------------------

std::string
OpsScheduler::showSchedule() const
{
  std::ostringstream oss;
#if 1 // 1 = 00:00 thru 00:00, 0 = 00-00
  oss << boost::format("%02d:00") % d_workStartHour
      << " thru "
      << boost::format("%02d:00") % d_workStopHour;
#else
  oss << boost::format("%02d-%02d") % d_workStartHour % d_workStopHour;
#endif // 0
  return oss.str();
}

// ---------------------------------------------------------
//  MEMBER FUNCTION : report
// ---------------------------------------------------------

void
OpsScheduler::report
(std::ostream& os) const
{
  os << std::boolalpha
     << "  interval [s]          : " << getInterval()              << "\n"
     << "  offset [h]            : " << getOffsetHour()            << "\n"
     << "  interval count        : " << getIntervalCount()         << "\n"
     << "  elapsed seconds   [s] : " << getElapsedSeconds()        << "\n"
     << "  current time          : " << showCurrent()              << "\n"
     << "  fractional day    [d] : " << getFractionalDay()         << "\n"
     << "  schedule period       : " << showSchedule()             << "\n"
     << "  schedule duration [h] : " << getDuration()              << "\n"
     << "  schedule active       : " << isWorkingHour()            << "\n"
     << std::noboolalpha
     << std::flush;
}

//  end of file

