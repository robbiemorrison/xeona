//  XEONA ENERGY SYSTEMS MODELING ENVIRONMENT
//
//  file-create-name : builtins.h
//  file-create-date : Mon 28-Jan-2008 18:25 UTC
//  file-initiator   : Robbie Morrison <robbie@actrix.co.nz>
//
//  file-role        : time horizon entity / header
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
//  $URL: file:///home/robbie/svn-root/xeona/futz/trunk/xeona1/b/builtins.h $
//
//  GENERAL NOTES FOR THIS FILE
//
//  Support the various 'builtin' sub-entities (not to be
//  confused with the 'inbuilt' model defined in the 'c/inbuilt'
//  unit):
//
//    * TimeHorizon
//

//  HEADER GUARD

#ifndef _BUILTINS_H_
#define _BUILTINS_H_

//  LOCAL AND SYSTEM INCLUDES

#include "../a/logger_fwd.h"  // some forward declarations from "logger.h"
#include "../b/entity.h"      // entity base class
#include "../c/smart_ptr.h"   // toggle between Boost and TR1 smart pointers

#include <iostream>           // standard io, used here for overloaded stream inserters
#include <string>             // C++ strings

//  FORWARD (PARTIAL) DECLARATIONS

class Record;                                // see "recset.h"

//  CODE

// ---------------------------------------------------------
//  CLASS           : TimeHorizon
// ---------------------------------------------------------
//  Description  : time horizon support
//  Role         : mandatory entity
//  Techniques   : static data, C-style arrays
//  Status       : complete
//
//      Only certain intervals-per-day [-] values are supported.
//
//                ipd     secs    hrs   mins
//              --------------------------------
//                288      300             5
//                144      600            10
//                 96      900            15
//                 72     1200            20
//                 48     1800            30
//                 24     3600      1
//                 12     7200      2
//                  8    10800      3
//                  6    14400      4
//                  4    21600      6
//                  2    43200     12
//                  1    86400     24
//
//      It is also important to note that all the
//      intervals-per-day are divisible by two (excepting the 24h
//      duration).  This feature is particularly important for
//      routines which resample timeseries.
//
// ---------------------------------------------------------

class TimeHorizon :
  public Entity                              // CAUTION: inherits directly from 'Entity'
{
private:

  // DISABLED

  TimeHorizon();                                  // prevent zero-argument construction
  TimeHorizon& operator= (const TimeHorizon&);    // prevent assignment

public:

  // CREATORS

  TimeHorizon
  (const std::string entityId,               // enforced unique identifier
   Record&           record);                // associated record

  virtual                                    // support for polymorphic destruction
  ~TimeHorizon();

  void
  factoryInitialize();

  // STATIC ACCESSORS

  static
  std::string
  stringifyValidInts();                      // space-separated list of valid intervals

  static
  bool
  checkInterval
  (const int interval);                      // intervals length in seconds

  // STREAM INSERTION SUPPORT

  virtual                                    // CAUTION: needed for polymorphic behavior
  std::ostream&
  streamOut                                  // support for overloaded operator<<
  (std::ostream& os) const;

  // INTERNAL DATA

private:

  std::string&         d_builtinRemarkTh;    // as per 'FullEntity'
  Record&              d_record;             // not strictly necessary

  const int&           d_steps;
  const int&           d_interval;
  const int&           d_startHour;
  const int&           d_startDay;
  const bool&          d_leapYear;
  const std::string    d_hemisphere;

  xeona::LeapYear      d_enumLeapYear;
  xeona::Hemisphere    d_enumHemisphere;

  // STATIC DATA

private:

  static const int          s_intervals[];   // 13 acceptable intervals [1]

  // [1] a C-style int array was required because it has to be
  // explicitly instantiated in the implementation file -- note
  // that the Boost.Assign library cannot do this on one line (as
  // is required), but that the upcoming C++11 standard will
  // support initializer lists

}; // class TimeHorizon

//  ==== XEDOC =================================================
//
//  entity.time-horizon
//
//      class                                    > TimeHorizon
//
//        the TimeHorizon entity is REQUIRED and the
//        'time-horizon' identifier is MANDATORY
//
//      builtin-remark s                         <
//
//      steps [-] i                              > 6
//      interval [s] i                           > 3600
//      start-hour [-] i                         > 0
//      start-day [-] i                          > 1
//      leap-year [-] b                          > 0
//
//        the interval [300,86400] is further restricted to
//        common multiples of one hour ranging 5 mins to 24 hours
//
//        the start-hour begins midnight local time and ranges
//        [0,23] and the start-day begins 01-Jan and ranges
//        [1,365] -- used to align internal and model timeseries
//        when the latter commences after 00:00 on 01 January
//
//        when leap-year is true then 29 February is presumed to
//        exist (although not all entities might support this)
//
//      hemisphere s                             > "N"
//
//        the hemisphere is {N,S} for north and south
//
//        the modeler should ensuring that timeseries data given
//        elsewhere aligns with the specification given here
//
//  ============================================================

#endif // _BUILTINS_H_

//  end of file

