//  XEONA ENERGY SYSTEMS MODELING ENVIRONMENT
//
//  file-create-name : suncalc.cc
//  file-create-date : Thu 21-Oct-2010 17:09 UTC
//  file-initiator   : Robbie Morrison <robbie@actrix.co.nz>
//
//  file-role        : free functions for solar calculations / implementation
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
//  $URL: file:///home/robbie/svn-root/xeona/futz/trunk/xeona1/c/suncalc.cc $
//
//  GENERAL NOTES FOR THIS FILE

//  LOCAL AND SYSTEM INCLUDES

#include "suncalc.h"          // companion header for this file (place first)

#include "../a/logger.h"      // standard logging functionality (as required)
#include "../c/smart_ptr.h"   // toggle between Boost and TR1 smart pointers
#include ".././common.h"      // common definitions for project (place last)

#include <string>             // C++ strings
#include <sstream>            // string-streams

#include <cmath>              // C-style maths, abs(), ceil(), floor(), sqrt()

#include <boost/format.hpp>                       // printf style formatting
#include <boost/math/constants/constants.hpp>     // high-precision mathematical constants

// STATIC DEFINITIONS

// CAUTION: static global variables are not normally used in
// 'xeona' but 'pi' and friends below are an exception because
// they replace the original preprocessor macros of the same name

static const double pi    = boost::math::constants::pi<double>();
static const double twopi = 2.0 * pi;
static const double rad   = pi / 180;
static const double dEarthMeanRadius  = 6371.01;       // [km]
static const double dAstronomicalUnit = 149597890;     // [km]

//  CODE

namespace xeona
{

  // ---------------------------------------------------------
  //  FREE FUNCTION   : xeona::sunpos
  // ---------------------------------------------------------
  //  Description  : calculate sun position from time and location
  //  Role         : support for ambient solar contexts
  //  Techniques   : published algorithm
  //  Status       : complete and verified for one data-point
  //
  //  Source
  //
  //      As listed in Blanco-Muriel etal (2001), Appendix A.
  //      The source code files SunPos.{h,cpp} are also available
  //      on the web.
  //
  //      I did attempt to email the author (for correspondence)
  //      to confirm that this code is in the public domain, but
  //      my email bounced.
  //
  //      Section 5 of Blanco-Muriel etal (2001) describes the
  //      algorithm in detail.
  //
  //  Code modifications
  //
  //      The basic code is identical.  But the hash-defined
  //      constants are now (file-local) global constants.  The
  //      input structs are now deemed 'const' arguments.  And
  //      the C-style cast is now a 'static_cast<>'.  In
  //      addition, one pair of unnecessary parentheses was
  //      removed.
  //
  //  Integrity checking
  //
  //      There are a number of websites which offer the same
  //      calculation and which can be used for integrity
  //      checking, for instance:
  //
  //  http://www.esrl.noaa.gov/gmd/grad/solcalc/
  //  http://www.usno.navy.mil/USNO/astronomical-applications/data-services/alt-az-world/
  //
  //      The United States Naval Observatory (USNO) site was
  //      used to provide a one data-point check.
  //
  //  CAUTION: not checked past 2015
  //
  //      This function has not been guaranteed accurate beyond
  //      the year 2015.  Prior to this date, the results should
  //      be within +/- 0.1 degree of angle.
  //
  //  References
  //
  //    Blanco-Muriel, Manuel, Diego C Alarcon-Padilla, Teodoro
  //      Lopez-Moratalla, and Martin Lara-Coira.  2001.
  //      Computing the solar vector.  Solar Energy, v70 no5
  //      pp431-441.  doi:10.1016/S0038-092X(00)00156-0.
  //
  // ---------------------------------------------------------

  void
  sunpos
  (const cTime      udtTime,                 // input timestamp
   const cLocation  udtLocation,             // input location
   cSunCoordinates* udtSunCoordinates)       // output angles
  {
    // main variables
    double dElapsedJulianDays;
    double dDecimalHours;
    double dEclipticLongitude;
    double dEclipticObliquity;
    double dRightAscension;
    double dDeclination;

    // auxiliary variables
    double dY;
    double dX;

    // calculate difference in days between the current Julian
    // day and JD 2451545.0, which is noon 1 January 2000
    // Universal Time
    {
      double dJulianDate;
      long int liAux1;
      long int liAux2;

      // calculate time of the day in UT decimal hours
      dDecimalHours = udtTime.dHours
        + (udtTime.dMinutes + udtTime.dSeconds / 60.0 ) / 60.0;
      // calculate current Julian day
      liAux1 = (udtTime.iMonth - 14) / 12;
      liAux2 = (1461 * (udtTime.iYear + 4800 + liAux1)) / 4
        + (367 * (udtTime.iMonth - 2 - 12 * liAux1)) / 12
        - (3 * ((udtTime.iYear + 4900 + liAux1) / 100)) / 4
        + udtTime.iDay
        - 32075;
      dJulianDate = static_cast<double>(liAux2) - 0.5 + dDecimalHours / 24.0;
      // calculate difference between current Julian day and JD 2451545.0
      dElapsedJulianDays = dJulianDate - 2451545.0;
    }

    // calculate ecliptic coordinates (ecliptic longitude and
    // obliquity of the ecliptic in radians but without limiting
    // the angle to be less than 2*pi (that is, the result may be
    // greater than 2*pi)
    {
      double dMeanLongitude;
      double dMeanAnomaly;
      double dOmega;

      dOmega = 2.1429
        - 0.0010394594 * dElapsedJulianDays;
      dMeanLongitude = 4.8950630
        + 0.017202791698 * dElapsedJulianDays;    // radians
      dMeanAnomaly = 6.2400600
        + 0.0172019699 * dElapsedJulianDays;
      dEclipticLongitude = dMeanLongitude
        + 0.03341607 * sin(dMeanAnomaly)
        + 0.00034894 * sin(2 * dMeanAnomaly)
        - 0.0001134
        - 0.0000203 * sin(dOmega);
      dEclipticObliquity = 0.4090928
        - 6.2140e-9 * dElapsedJulianDays
        + 0.0000396 * cos(dOmega);
    }

    // calculate celestial coordinates (right ascension and
    // declination) in radians but without limiting the angle to
    // be less than 2*pi (that is, the result may be greater than
    // 2*pi)
    {
      double dSin_EclipticLongitude;

      dSin_EclipticLongitude = sin(dEclipticLongitude);
      dY = cos(dEclipticObliquity) * dSin_EclipticLongitude;
      dX = cos(dEclipticLongitude);
      dRightAscension = atan2(dY, dX);
      if ( dRightAscension < 0.0 )
        dRightAscension = dRightAscension + twopi;
      dDeclination = asin(sin(dEclipticObliquity) * dSin_EclipticLongitude);
    }

    // calculate local coordinates (azimuth and zenith angle) in
    // degrees
    {
      double dGreenwichMeanSiderealTime;
      double dLocalMeanSiderealTime;
      double dLatitudeInRadians;
      double dHourAngle;
      double dCos_Latitude;
      double dSin_Latitude;
      double dCos_HourAngle;
      double dParallax;

      dGreenwichMeanSiderealTime = 6.6974243242
        + 0.0657098283 * dElapsedJulianDays
        + dDecimalHours;
      dLocalMeanSiderealTime =
        (dGreenwichMeanSiderealTime * 15 + udtLocation.dLongitude) * rad;
      dHourAngle = dLocalMeanSiderealTime - dRightAscension;
      dLatitudeInRadians = udtLocation.dLatitude * rad;
      dCos_Latitude  = cos(dLatitudeInRadians);
      dSin_Latitude  = sin(dLatitudeInRadians);
      dCos_HourAngle = cos(dHourAngle);
      udtSunCoordinates->dZenithAngle =
        acos(dCos_Latitude * dCos_HourAngle * cos(dDeclination)
             + sin(dDeclination) * dSin_Latitude);
      dY = -sin(dHourAngle);
      dX = tan(dDeclination) * dCos_Latitude
        - dSin_Latitude * dCos_HourAngle;
      udtSunCoordinates->dAzimuth = atan2(dY, dX);
      if ( udtSunCoordinates->dAzimuth < 0.0 )
        udtSunCoordinates->dAzimuth = udtSunCoordinates->dAzimuth + twopi;
      udtSunCoordinates->dAzimuth = udtSunCoordinates->dAzimuth / rad;
      // parallax correction
      dParallax =
        (dEarthMeanRadius / dAstronomicalUnit) * sin(udtSunCoordinates->dZenithAngle);
      udtSunCoordinates->dZenithAngle =
        (udtSunCoordinates->dZenithAngle + dParallax) / rad;
    }

  } // function 'xeona::sunpos'

  // ---------------------------------------------------------
  //  FREE FUNCTION   : sunposReport
  // ---------------------------------------------------------
  //  Description  : report using input and output structs
  //  Role         : logging support
  //  Techniques   : (nothing special)
  //  Status       : complete (not part of the original 'SunPos' code)
  // ---------------------------------------------------------

  void
  sunposReport
  (const cTime           time,
   const cLocation       location,
   const cSunCoordinates coordinates,
   std::ostream&         os)
  {
    os << boost::format("  %-10s : %g\n")  % "year"      % time.iYear
       << boost::format("  %-10s : %g\n")  % "month"     % time.iMonth
       << boost::format("  %-10s : %g\n")  % "day"       % time.iDay
       << boost::format("  %-10s : %g\n")  % "hour"      % time.dHours
       << boost::format("  %-10s : %g\n")  % "minute"    % time.dMinutes
       << boost::format("  %-10s : %g\n")  % "second"    % time.dSeconds;

    os << boost::format("  %-10s : %+g\n") % "latitude"  % location.dLatitude
       << boost::format("  %-10s : %+g\n") % "longitude" % location.dLongitude;

    os << boost::format("\n");

    const double elevation = 90 - coordinates.dZenithAngle;

    os << boost::format("  %-10s : %+g\n") % "zenith"    % coordinates.dZenithAngle
       << boost::format("  %-10s : %+g\n") % "elevation" % elevation
       << boost::format("  %-10s : %+g\n") % "azimuth"   % coordinates.dAzimuth;
  }

} // namespace 'xeona'

//  end of file

