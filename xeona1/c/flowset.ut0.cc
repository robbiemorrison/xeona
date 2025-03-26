//  XEONA ENERGY SYSTEMS MODELING ENVIRONMENT
//
//  file-create-name : flowset.ut0.cc
//  file-create-date : Thu 03-Feb-2011 10:45 UTC
//  file-initiator   : Robbie Morrison <robbie@actrix.co.nz>
//
//  file-role        : lake inflow dataset support / unit test
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
//  $URL: file:///home/robbie/svn-root/xeona/futz/trunk/xeona1/c/flowset.ut0.cc $
//
//  GENERAL NOTES FOR THIS FILE
//
//  This is a unit test file and hence not essential to the
//  application proper.

//  LOCAL AND SYSTEM INCLUDES

#include "flowset.h"          // unit under test (place early)

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
  //  test ONE        : general test
  // ---------------------------------------------------------

  logger->test(1, "general test");

  {
    const std::string description = "test one dataset";
    const int         year        = 1801;

    const int    daycount = 365;             // must be either 365 or 366
    const int    moncount = 12;
    const double value    = 101.1;
    std::vector<double>   days(daycount, value);
    std::vector<double> months(moncount, value);

    InflowSet inflowset(description, year, days, months);

    const int reading = 200;
    put << "  description   : " << inflowset.description                   << "\n"
        << "  year          : " << inflowset.year                          << "\n"
        << "  inflow ("<< reading << ")  : " << inflowset.days.at(reading) << "\n";

    logger->putx(logga::yeek, put);
  }

  // ---------------------------------------------------------
  //  test TWO        : loadInfoSets test
  // ---------------------------------------------------------

  logger->test(2, "loadInfoSets test");

  {
    shared_ptr<xeona::inflowsets_type> inflowsets;
    inflowsets = xeona::loadInflowSets();

    const int size  = inflowsets->size();

    const std::string rightKey = "benmore1983";
    const std::string wrongKey = "reallywrong";
    const int keys1 = inflowsets->count(rightKey);
    const int keys2 = inflowsets->count(wrongKey);

    put << "  size                     : "          << size  << "\n"
        << "  key count \"" << rightKey << "\"  : " << keys1 << "\n"
        << "  key count \"" << wrongKey << "\"  : " << keys2 << "\n";
    logger->putx(logga::yeek, put);
  }

#if 0 // currently disabled, but this functionality might be
      // needed in due course

  // ---------------------------------------------------------
  //  test THREE      : copy assignment
  // ---------------------------------------------------------

  logger->test(2, "copy assignment");

  {
    const std::string description = "test two dataset";
    const int         year        = 1802;
    std::vector<double>  days(365, 202.2);
    std::vector<double> months(12, 198.2);

    InflowSet first(description, year, days, months);
    InflowSet next = next;
  }

#endif // 0

  // ---------------------------------
  //  HOUSEKEEPING
  // ---------------------------------

  logger->test(0);                           // test end

  logger->repx(logga::info, "end of main", "~~~~~~~~~~");
  return xeona::exit_success;

} // main function

//  end of file

