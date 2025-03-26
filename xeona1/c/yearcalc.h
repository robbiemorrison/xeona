//  XEONA ENERGY SYSTEMS MODELING ENVIRONMENT
//
//  file-create-name : yearcalc.h
//  file-create-date : Thu 21-Oct-2010 18:40 UTC
//  file-initiator   : Robbie Morrison <robbie@actrix.co.nz>
//
//  file-role        : free functions for calendar calculations / header
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
//  $URL: file:///home/robbie/svn-root/xeona/futz/trunk/xeona1/c/yearcalc.h $
//
//  GENERAL NOTES FOR THIS FILE

//  HEADER GUARD

#ifndef _YEARCALC_H_
#define _YEARCALC_H_

//  LOCAL AND SYSTEM INCLUDES

#include "../a/logger_fwd.h"  // some forward declarations from "logger.h"
#include "../c/smart_ptr.h"   // toggle between Boost and TR1 smart pointers

#include <string>             // C++ strings
#include <sstream>            // string-streams

#include <cmath>              // C-style maths, abs(), ceil(), floor(), sqrt()

#include <boost/tuple/tuple.hpp>   // n-tuples, ref(), cref()

//  CODE

namespace xeona
{
  // typedef declaration
  typedef boost::tuple <int,                 // year
                        int,                 // month
                        int,                 // day
                        int,                 // hour
                        int,                 // minute
                        int> timestamp_type; // second

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
  //      See implementation file.  This function can skip 29-Feb
  //      leap days as a hard-coded elective.
  //
  // ---------------------------------------------------------

  // free functions
  timestamp_type
  timeOffseToTimestamp
  (const int startYear,                      // for instance, 2010
   const int startDay,                       // starting day 1
   const int startHour,                      // starting hour 0
   const int offsetSeconds);                 // offset to be included

} // namespace 'xeona'

// ---------------------------------------------------------
//  FREE FUNCTION   : xeona::hms
// ---------------------------------------------------------
//  Description  : convert decimal seconds to hh::mm::ss format
//  Role         : general usage
//  Techniques   : 'std::modf' (returns fractional, sets integral) from <cmath>
//  Status       : complete
//
//  Design notes
//
//      integral seconds = hh:mm:ss
//      decimal seconds  = hh:mm::ss.sss
//
//  Source
//
//      Core code taken from my (Robbie Morrison) well-tested
//      'hms' utility.
//
// ---------------------------------------------------------

namespace xeona
{
  std::string
  hms
  (const double seconds);                    // integral or decimal

} // namespace 'xeona'

#endif // _YEARCALC_H_

//  end of file

