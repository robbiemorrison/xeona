//  XEONA ENERGY SYSTEMS MODELING ENVIRONMENT
//
//  file-create-name : suncalc.ut0.cc
//  file-create-date : Thu 21-Oct-2010 17:09 UTC
//  file-initiator   : Robbie Morrison <robbie@actrix.co.nz>
//
//  file-role        : free functions for solar calculations / unit test
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
//  $URL: file:///home/robbie/svn-root/xeona/futz/trunk/xeona1/c/suncalc.ut0.cc $
//
//  GENERAL NOTES FOR THIS FILE
//
//  This is a unit test file and hence not essential to the
//  application proper.

//  LOCAL AND SYSTEM INCLUDES

#include "suncalc.h"          // unit under test (place early)

#include "../a/logger.h"      // standard logging functionality (as required)
#include "../c/smart_ptr.h"   // toggle between Boost and TR1 smart pointers
#include ".././common.h"      // common definitions for project (place last)

#include <string>             // C++ strings
#include <sstream>            // string-streams

//  CODE

// ---------------------------------------------------------
//  MAIN FUNCTION   : main
// ---------------------------------------------------------

int
main(int argc, char* argv[])
{
  logga::spLogger logger = logga::ptrLogStream();  // main function logger
  logger->repx(logga::info, "beginning of main", "~~~~~~~~~~");
  logger->addSmartBlank();
  logger->setReportLevel(logga::adhc);       // maximum reporting

  // PRELIMINARY

  std::ostringstream put;

  // ---------------------------------------------------------
  //  test ONE        : xeona::sunpos trial
  // ---------------------------------------------------------

  logger->test(1, "xeona::sunpos trial");

  {
    // timestamp: 19-Oct-2010
    xeona::cTime time;
    time.iYear    = 2010;
    time.iMonth   = 10;
    time.iDay     = 19;
    time.dHours   = 10;
    time.dMinutes = 30;
    time.dSeconds =  0;

    // Berlin: 52*30'2"N, 13*23'56"E = 52.500556, 13.398889
    // source: http://en.wikipedia.org/wiki/Berlin
    xeona::cLocation location;
    location.dLongitude = 13.4;
    location.dLatitude  = 52.5;

    // the value 1000 is well out of range
    xeona::cSunCoordinates sunangles;
    sunangles.dZenithAngle = 1000.0;
    sunangles.dAzimuth     = 1000.0;

    // call
    xeona::sunpos(time, location, &sunangles);

    // report
    xeona::sunposReport(time, location, sunangles, put);
    logger->putx(logga::dbug, put);

    // add comparison
    std::string msg = \
      "  Astronomical Applications Dept.\n"
      "  U.S. Naval Observatory\n"
      "  Washington, DC 20392-5420\n"
      "\n"
      "  BERLIN\n"
      "  E 13 24', N 52 30'\n"
      "\n"
      "  Altitude and Azimuth of the Sun\n"
      "  Oct 19, 2010\n"
      "  Universal Time\n"
      "\n"
      "            Altitude    Azimuth (E of N)\n"
      "\n"
      "  10:30       27.3       174.1\n";

    put << "\n" << "  ---\n" << "\n"<< msg;
    logger->putx(logga::dbug, put);

  }

  // ---------------------------------
  //  HOUSEKEEPING
  // ---------------------------------

  logger->test(0);                           // test end

  logger->repx(logga::info, "end of main", "~~~~~~~~~~");
  return xeona::exit_success;

} // main function

//  end of file

