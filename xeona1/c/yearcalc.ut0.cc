//  XEONA ENERGY SYSTEMS MODELING ENVIRONMENT
//
//  file-create-name : yearcalc.ut0.cc
//  file-create-date : Thu 21-Oct-2010 18:40 UTC
//  file-initiator   : Robbie Morrison <robbie@actrix.co.nz>
//
//  file-role        : free functions for calendar calculations / unit test
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
//  $URL: file:///home/robbie/svn-root/xeona/futz/trunk/xeona1/c/yearcalc.ut0.cc $
//
//  GENERAL NOTES FOR THIS FILE
//
//  This is a unit test file and hence not essential to the
//  application proper.

//  LOCAL AND SYSTEM INCLUDES

#include "yearcalc.h"         // unit under test (place early)

#include "../a/logger.h"      // standard logging functionality (as required)
#include "../c/smart_ptr.h"   // toggle between Boost and TR1 smart pointers
#include ".././common.h"      // common definitions for project (place last)

#include <string>             // C++ strings
#include <sstream>            // string-streams

#include <boost/foreach.hpp>  // BOOST_FOREACH iteration macro

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

  xeona::yeek = 1;
  std::ostringstream put;

  // ---------------------------------------------------------
  //  test ONE        : 'timeOffseToTimestamp' test
  // ---------------------------------------------------------

  logger->test(1, "'timeOffseToTimestamp' test");

  {
    const int startYear = 2010;
    const int startDay  = 1;
    const int startHour = 0;
    const int interval  = 3600;
    int step;

    step = 0;
    step = 2;
    step = 365 * 24;

    put << "    start year = " << startYear       << "\n"
        << "    start day  = " << startDay        << "\n"
        << "    start hour = " << startHour       << "\n"
        << "    interval   = " << interval        << "\n"
        << "    step       = " << step            << "\n";
    logger->putx(logga::dbug, put);

    const int offset = (step + 0.5) * interval;   // note the 0.5 adjustment
    xeona::timestamp_type tstamp = xeona::timeOffseToTimestamp(startYear,
                                                               startDay,
                                                               startHour,
                                                               offset);
    put << "\n";
    put << "    year       = " << tstamp.get<0>() << "\n"
        << "    month      = " << tstamp.get<1>() << "\n"
        << "    day        = " << tstamp.get<2>() << "\n"
        << "    hour       = " << tstamp.get<3>() << "\n"
        << "    minute     = " << tstamp.get<4>() << "\n"
        << "    seconds    = " << tstamp.get<5>() << "\n";
    logger->putx(logga::dbug, put);

  }

  // ---------------------------------------------------------
  //  test TWO        : leap year trial
  // ---------------------------------------------------------

  logger->test(2, "leap year trial");

  {
    const int startYear = 2000;              // 2000, a leap year
    const int startDay  = 0;
    const int startHour = 0;
    const int interval  = 3600;
    int step;

    step = (31 + 29) * 24;                   // 29-Jan, a leap day

    put << "    start year = " << startYear       << "\n"
        << "    start day  = " << startDay        << "\n"
        << "    start hour = " << startHour       << "\n"
        << "    interval   = " << interval        << "\n"
        << "    step       = " << step            << "\n";
    logger->putx(logga::dbug, put);

    const int offset = (step + 0.5) * interval;   // note the 0.5 adjustment
    xeona::timestamp_type tstamp = xeona::timeOffseToTimestamp(startYear,
                                                               startDay,
                                                               startHour,
                                                               offset);
    put << "\n";
    put << "    year       = " << tstamp.get<0>() << "\n"
        << "    month      = " << tstamp.get<1>() << "\n"
        << "    day        = " << tstamp.get<2>() << "\n"
        << "    hour       = " << tstamp.get<3>() << "\n"
        << "    minute     = " << tstamp.get<4>() << "\n"
        << "    seconds    = " << tstamp.get<5>() << "\n";
    logger->putx(logga::dbug, put);

  }

  // ---------------------------------------------------------
  //  test THREE      : xeona::hms calculations
  // ---------------------------------------------------------

  logger->test(3, "xeona::hms calculations");

  {
    std::vector<double> hours;
    hours.push_back(-10.0);
    hours.push_back(0.0);
    hours.push_back(0.0044);
    hours.push_back(12.3);
    hours.push_back(8760);

    BOOST_FOREACH( double d, hours )
      {
        put << std::fixed
            << "    "
            << std::right << std::setw(15) << d
            << "  "
            << xeona::hms(d * 3600)
            << "\n";
      }
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

