//  XEONA ENERGY SYSTEMS MODELING ENVIRONMENT
//
//  file-create-name : suncalc.h
//  file-create-date : Thu 21-Oct-2010 17:08 UTC
//  file-initiator   : Robbie Morrison <robbie@actrix.co.nz>
//
//  file-role        : free functions for solar calculations / header
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
//  $URL: file:///home/robbie/svn-root/xeona/futz/trunk/xeona1/c/suncalc.h $
//
//  GENERAL NOTES FOR THIS FILE
//
//  The code in this file derives from 'SunPos' code.  The
//  structs, free function call, and internal variables retain
//  their original identifiers -- aside from being placed in the
//  'xeona' namespace.
//
//  Details of the source and accompanying paper are given in the
//  implementation file.

//  HEADER GUARD

#ifndef _SUNCALC_H_
#define _SUNCALC_H_

//  LOCAL AND SYSTEM INCLUDES

#include "../a/logger_fwd.h"  // some forward declarations from "logger.h"
#include "../c/smart_ptr.h"   // toggle between Boost and TR1 smart pointers

#include <iostream>           // standard io
#include <sstream>            // string-streams
#include <string>             // C++ strings

//  CODE

namespace xeona
{

  // ---------------------------------------------------------
  //  STRUCT          : xeona::cTime
  // ---------------------------------------------------------
  //  Description  : input values for UTC date and time
  //  Role         : client usage
  //  Techniques   : C-style struct
  //  Status       : complete
  //
  //  CAUTION: UTC time required
  //
  //      This date and time data must be in Universal
  //      Coordinated Time.  This requirement is also consistent
  //      with the rest of 'xeona'.
  //
  //  CAUTION: range checking not undertaken
  //
  // ---------------------------------------------------------

  struct cTime
  {
    int       iYear;
    int       iMonth;                        // input, starting 1, CAUTION: unlike Unix!
    int       iDay;                          // input, starting 1
    double    dHours;
    double    dMinutes;
    double    dSeconds;
  };

  // ---------------------------------------------------------
  //  STRUCT          : xeona::cLocation
  // ---------------------------------------------------------
  //  Description  : input values for location as longitude and latitude
  //  Role         : client usage
  //  Techniques   : C-style struct
  //  Status       : complete
  //
  //  Design notes
  //
  //      Longitude, in degrees, ranges [-180,180] and takes
  //      decimal fractions.  Positive values indicate east of
  //      zero, negative values indicate west.
  //
  //      Latitude, in degrees, ranges [-180,180] and takes
  //      decimal fractions.  Positive values indicate the
  //      northern hemisphere, negative values indicate the
  //      southern hemisphere.
  //
  //  CAUTION: range checking not undertaken
  //
  // ---------------------------------------------------------

  struct cLocation
  {
    double    dLongitude;                    // input in degrees positive towards east
    double    dLatitude;                     // input in degrees positive towards north
  };

  // ---------------------------------------------------------
  //  STRUCT          : xeona::cSunCoordinates
  // ---------------------------------------------------------
  //  Description  : output values
  //  Role         : client use
  //  Techniques   : C-style struct, pass-by-pointer
  //  Status       : complete
  //
  //  Design notes
  //
  //      The zenith angle, in degrees, ranges [0,180].  Values
  //      above 90 degrees indicate night.
  //
  //      The azimuth angle, in degrees, ranges [0,360].
  //
  //      The question of northern or southern hemisphere are
  //      automatically taken into account by the two angles and
  //      do not require additional treatment.
  //
  //      The qualifier "angle" is added to "zenith" because,
  //      strictly speaking, the zenith is the point vertically
  //      overhead.
  //
  //      The elevation, in degrees, ranges [90,-90] and is
  //      simply the zenith angle minus 90 degrees.
  //
  //      See the implementation file for more details.
  //
  //  CAUTION: not checked past 2015
  //
  //      The underlying function has not been guaranteed
  //      accurate beyond the year 2015.  Prior to this date, the
  //      results should be within +/- 0.1 degree of angle.
  //
  // ---------------------------------------------------------

  struct cSunCoordinates
  {
    double    dZenithAngle;                  // output in degrees from vertical
    double    dAzimuth;                      // output in degrees positive east from north
  };

  // ---------------------------------------------------------
  //  FREE FUNCTION   : xeona::sunpos
  // ---------------------------------------------------------
  //  Description  : principal call
  //  Role         : client use
  //  Techniques   : see implementation file
  //  Status       : complete
  // ---------------------------------------------------------

  void
  sunpos
  (const cTime      udtTime,                 // UTC date and time
   const cLocation  udtLocation,             // longitude and latitude
   cSunCoordinates* udtSunCoordinates);      // results

  // --------------------------------------------------------
  //  FREE FUNCTION   : sunposReport
  // ---------------------------------------------------------
  //  Description  : ostream reporting
  //  Role         : client use
  //  Techniques   : ostream argument passed-by-reference
  //  Status       : complete
  // ---------------------------------------------------------

  void
  sunposReport
  (const cTime           udtTime,
   const cLocation       udtLocation,
   const cSunCoordinates udtSunCoordinates,
   std::ostream&         os);                // for instance 'std::cout'

}

#endif // _SUNCALC_H_

//  end of file

