//  XEONA ENERGY SYSTEMS MODELING ENVIRONMENT
//
//  file-create-name : opssched.h
//  file-create-date : Mon 08-Nov-2010 22:41 UTC
//  file-initiator   : Robbie Morrison <robbie@actrix.co.nz>
//
//  file-role        : class to track clock time and schedule plant / header
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
//  $URL: file:///home/robbie/svn-root/xeona/futz/trunk/xeona1/c/opssched.h $
//
//  GENERAL NOTES FOR THIS FILE

//  HEADER GUARD

#ifndef _OPSSCHED_H_
#define _OPSSCHED_H_

//  LOCAL AND SYSTEM INCLUDES

#include "../a/logger_fwd.h"  // some forward declarations from "logger.h"
#include "../c/smart_ptr.h"   // toggle between Boost and TR1 smart pointers

#include <string>             // C++ strings
#include <sstream>            // string-streams

#include <boost/tuple/tuple.hpp>                  // n-tuples, ref(), cref()

//  CODE

// ---------------------------------------------------------
//  CLASS           : OpsScheduler
// ---------------------------------------------------------
//  Description  : class to track clock time and schedule plant operations
//  Role         : support for the simulation of part-time plant, including HVAC plant
//  Techniques   : 'modulo' (remainder) operator
//  Status       : complete
//
//  Design notes
//
//      This class translates from simulation intervals to clock
//      time.  Clock time is important for engineering facilities
//      being operated on fixed schedules.  For instance, an HVAC
//      plant usually runs for just 10 hours each day.
//
//        - the client code passes the simulation 'interval'
//          (3600s or less) and the 'offsetHours' from midnight
//          (which defaults to zero and may exceed 24) on
//          construction
//
//        - this information is translated into clock hours and
//          minutes and tracked
//
//        - an operations schedule can be specified by calling
//          member function 'setSchedule' and tested by calling
//          'isWorkingHour'
//
//      The baseline starting point, without any offset,
//      commences midnight 00:00 at day 0.
//
//      Each period in time -- be it an interval or work start or
//      stop -- is assumed to aligned with the BEGINNING of the
//      period.
//
//  Design extensions
//
//      This class is not especially sophisticated.  It could be
//      extended to accommodate more complex schedules and be
//      refactored using 'Boost.Date_Time' calls if the need
//      arises.
//
//      This class could also be combined with free function
//      'xeona::timeOffseToTimestamp' to create a fully-fledged
//      'Calendar' class which is also day-of-week and
//      day-of-year aware.
//
//  CAUTION: interval length restricted
//
//      The interval length (in seconds) is restricted to certain
//      values.  Clients of this class will need to enforce this
//      restriction, which may be less permissive than 'xeona' at
//      large.
//
//  CAUTION: hours used
//
//      This class works extensively with hours as the unit of
//      time -- which is rather unusual for 'xeona'.
//
// ---------------------------------------------------------

class OpsScheduler
{
  // TYPEDEFS

public:

  typedef boost::tuple<int, int, int> DHM_type;   // (day, hour, minute) tuple

  // DISABLED

private:

  OpsScheduler();                                      // zero-argument constructor
  OpsScheduler(const OpsScheduler& orig);              // copy constructor
  OpsScheduler& operator= (const OpsScheduler& orig);  // copy assignment operator

public:

  // CREATORS

  explicit
  OpsScheduler
  (const int interval,                       // normal horizon interval [s]
   const int offsetHour = 0);                // starting hour offset

  ~OpsScheduler();

  // MANIPULATORS

  // CAUTION: usually safer to use 'bump' than 'operator++'

  int                                        // duration [h]
  setSchedule
  (const int startHour,                      // schedule start at beginning of hour
   const int stopHour);                      // schedule stop at beginning of hour

  OpsScheduler&                              // standard for this particular operator
  operator++ ();                             // prefix increment operator (++clock)

  bool                                       // 'true' if moved forward
  bump                                       // attempt to update scheduler
  (const int currentStep);                   // usually 'd_step'

  void
  skipForward
  (const unsigned skip);                     // increment 'skip' times

  // ACCESSORS

  // recover current values
  int         getInterval()             const;    // from constructor [s]
  int         getIntervalCount()        const;    // excludes 'offsetHour', zero-based
  int         getElapsedSeconds()       const;    // excludes 'offsetHour', current [s]
  int         getOffsetHour()           const;    // from constructor [h]
  int         getDuration()             const;    // [h]
  bool        isWorkingHour()           const;    // return 'true' means in operation
  double      getFractionalDay()        const;    // includes 'offsetHour'
  DHM_type    getDaysHoursMinutes()     const;    // includes 'offsetHour'

  // formatted output
  std::string showCurrent()             const;    // in 'HH:MM' format
  std::string showSchedule()            const;    // in 'HH-HH' format
  void        report (std::ostream& os) const;    // extensive reporting

private:

  // timer parameters
  const int    d_interval;
  const int    d_offsetHour;                 // zero-based as per 'xeona' timeseries
  int          d_workStartHour;              // start of schedule
  int          d_workStopHour;               // end of schedule, exclusive

  // timer registers
  int          d_count;                      // zero-based
  int          d_days;                       // 0 onward
  int          d_hours;                      // [0,23]
  int          d_minutes;                    // [0,59]

  // STATIC DATA

protected:

  static logga::spLogger    s_logger;        // shared_ptr to single logger object

};

#endif // _OPSSCHED_H_

//  end of file

