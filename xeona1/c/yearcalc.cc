//  XEONA ENERGY SYSTEMS MODELING ENVIRONMENT
//
//  file-create-name : yearcalc.cc
//  file-create-date : Thu 21-Oct-2010 18:40 UTC
//  file-initiator   : Robbie Morrison <robbie@actrix.co.nz>
//
//  file-role        : free functions for calendar calculations / implementation
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
//  $URL: file:///home/robbie/svn-root/xeona/futz/trunk/xeona1/c/yearcalc.cc $
//
//  GENERAL NOTES FOR THIS FILE

//  LOCAL AND SYSTEM INCLUDES

#include "yearcalc.h"         // companion header for this file (place first)

#include "../b/entity.h"      // entity base class plus lazy linking

#include "../a/logger.h"      // standard logging functionality (as required)
#include "../c/smart_ptr.h"   // toggle between Boost and TR1 smart pointers
#include ".././common.h"      // common definitions for project (place last)

#include <string>             // C++ strings
#include <sstream>            // string-streams

#include <boost/date_time/gregorian/gregorian.hpp>     // may require -lboost_date_time
#include <boost/date_time/posix_time/posix_time.hpp>   // may require -lboost_date_time
#include <boost/format.hpp>                            // printf style formatting

//  CODE

namespace xeona
{
  // ---------------------------------------------------------
  //  FREE FUNCTION   : xeona::timeOffseToTimestamp
  // ---------------------------------------------------------
  //  Description  : convert 'ydh + offset' to 'ymdhms'
  //  Role         : preparing arguments for 'xeona::sunpos' calls
  //  Techniques   : 'Boost.Time_Date' library
  //  Status       : complete
  //
  //  Design notes
  //
  //      Use of the 'Boost.Time_Date' library means this
  //      function is succinct and safe.
  //
  //      Creating 'boost::gregorian::date' and
  //      'boost::posix_time::ptime' objects each time is clearly
  //      wasteful.
  //
  //      If necessary, this function could combined with class
  //      'OpsScheduler' to become a fully-fledged calendar
  //      class.
  //
  //  CAUTION: half-interval adjustment
  //
  //      The client code needs to make any required
  //      half-interval adjustment.  See the unit test for an
  //      example.
  //
  //  CAUTION: leap years
  //
  //      If 'entity.time-horizon.leap-year' is "0" (the
  //      default), then this function skips over the date
  //      29-Feb.  This behavior means that a one year hourly
  //      timeseries is always 8760 in length.  It also means
  //      that leap year timeseries must be suitably structured.
  //
  //      Otherwise this function will naturally account for leap
  //      years because the underlying Boost.Date_Time library
  //      does so.  But it also means that the year will comprise
  //      366 days or 8784 hours.
  //
  // ---------------------------------------------------------

  timestamp_type
  timeOffseToTimestamp
  (const int startYear,                      // for instance, 2010
   const int startDay,                       // starting day 1
   const int startHour,                      // starting hour 0
   const int offsetSeconds)                  // offset to be included
  {
    // namespace aliases
    namespace bg = boost::gregorian;         // calendar related functionality
    namespace bt = boost::posix_time;        // timestamp and time duration functionality

    // create the start point
    bg::date          calStartYear(startYear, bg::Jan, 1); // 01 Jan startYear
    bt::time_duration timStartShift          // calculate the start shift
      = bt::hours(24 * (startDay - 1))       // day 1 means beginning of year
      + bt::hours(startHour);                // hour 0 needs no adjustment
    bt::ptime         timStart(calStartYear, timStartShift);

    // adjust for the progress of the simulation
    bt::time_duration timElapsed = bt::seconds(offsetSeconds);
    bt::ptime         timCurrent = timStart + timElapsed;

    const bool leapyear = Entity::getLeapYear();
    if ( leapyear == false )
      {
        // skip over leap days -- common practice for timeseries
        const int monthOfYear = timCurrent.date().month();
        const int dayOfMonth  = timCurrent.date().day();
        if ( monthOfYear == 2 && dayOfMonth == 29 )
          {
            // log just once
            static logga::spLogger logger = logga::ptrLogStream();
            static bool wasLogged = false;
            if ( wasLogged == false )
              {
                std::ostringstream oss;
                oss << timCurrent;
                logger->repx(logga::info, "skipping over leap day", oss.str());
                logger->repx(logga::dbug, "entity.time-horizon.leap-year is", leapyear);
                wasLogged = true;
              }
            bg::date_duration oneday(1);
            timCurrent += oneday;
          }
      }

    // recover and return data
    return boost::make_tuple(timCurrent.date().year(),
                             timCurrent.date().month(),
                             timCurrent.date().day(),
                             timCurrent.time_of_day().hours(),
                             timCurrent.time_of_day().minutes(),
                             timCurrent.time_of_day().seconds());
  }

} // namespace 'xeona'

// ---------------------------------------------------------
//  FREE FUNCTION   : xeona::hms
// ---------------------------------------------------------

namespace xeona
{
  std::string
  hms
  (const double seconds)                     // integral or decimal
  {
    double secs = seconds;                   // to remove const

    double frac = 0;                         // fractional seconds
    double sec  = 0;                         // whole seconds
    double min  = 0;                         // whole minutes
    double hour = 0;                         // whole hours

    if ( secs == -0.0 ) secs = 0.0;          // necessary protection

    std::string sign;
    if ( secs < 0 )                          // process negative numbers
      {
        sign = "-";
        secs = -secs;                        // process as positive
      }
    else
      {
        sign="";                             // could also be "+" or " "
      }

    frac = std::modf(secs,   &sec);          // returns fractional, sets integral
    sec  = std::modf(sec/60, &min)  * 60;
    min  = std::modf(min/60, &hour) * 60;

    std::ostringstream oss1;                 // work up factional seconds
    std::string        buf1;
    if ( frac == 0.0 )
      {
        buf1 = "";                           // could also be ".0"
      }
    else                                     // necessarily on interval [1,0]
      {
        oss1 << frac;
        buf1 = oss1.str();
        buf1.replace(0, 1, "");              // remove leading "0"
      }

    std::ostringstream oss2;                      // work up return string
    oss2 << std::fixed << std::setprecision(0);   // no decimals displayed
    oss2 << sign
         << std::setw(2) << std::setfill('0') << hour << ":"
         << std::setw(2) << std::setfill('0') << min  << ":"
         << std::setw(2) << std::setfill('0') << sec
         << buf1;

    return oss2.str();

  } // function 'xeona::hms'
} // namespace 'xeona'

//  end of file

