//  XEONA ENERGY SYSTEMS MODELING ENVIRONMENT
//
//  file-create-name : opssched.ut0.cc
//  file-create-date : Mon 08-Nov-2010 22:42 UTC
//  file-initiator   : Robbie Morrison <robbie@actrix.co.nz>
//
//  file-role        : class to track clock time and schedule plant / unit test
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
//  $URL: file:///home/robbie/svn-root/xeona/futz/trunk/xeona1/c/opssched.ut0.cc $
//
//  GENERAL NOTES FOR THIS FILE
//
//  This is a unit test file and hence not essential to the
//  application proper.

//  LOCAL AND SYSTEM INCLUDES

#include "opssched.h"         // unit under test (place early)

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

  xeona::yeek = 1;                           // maximum yeeking
  std::ostringstream put;

  // ---------------------------------------------------------
  //  test ONE        : simple construction
  // ---------------------------------------------------------

  logger->test(1, "simple construction");

  {
    OpsScheduler timer(3600);
  }

  // ---------------------------------------------------------
  //  test TWO        : full example
  // ---------------------------------------------------------

  logger->test(2, "full example");

  {
    OpsScheduler timer(1800,                 // interval [s]
                       28);                  // offset [h]

    timer.report(put);                       // extensive reporting
    logger->putx(logga::dbug, put);

    timer.setSchedule(8, 18);                // overwrite default of (0, 24)

    // 'get' method
    OpsScheduler::DHM_type time1 = timer.getDaysHoursMinutes();
    put << "  days       : " << time1.get<0>()    << "\n"
        << "  hours      : " << time1.get<1>()    << "\n"
        << "  minutes    : " << time1.get<2>()    << "\n";
    logger->addSmartBlank(logga::dbug);
    logger->putx(logga::dbug, put);

    // increments
    ++timer;                                 // single increment
    timer.skipForward(200);                  // also generates a 'logga::info' message

    timer.report(put);                       // extensive reporting
    logger->putx(logga::dbug, put);

    // 'tie' method
    double days;
    double hours;
    double minutes;
    boost::tie(days, hours, minutes) = timer.getDaysHoursMinutes();
    put << "  days       : " << days              << "\n"
        << "  hours      : " << hours             << "\n"
        << "  minutes    : " << minutes           << "\n";
    logger->addSmartBlank(logga::dbug);
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

