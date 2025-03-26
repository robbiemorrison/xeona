//  XEONA ENERGY SYSTEMS MODELING ENVIRONMENT
//
//  file-create-name : helpers.ut0.cc
//  file-create-date : Mon 18-May-2009 11:08 UTC
//  file-initiator   : Robbie Morrison <robbie@actrix.co.nz>
//
//  file-role        : application helper functions / unit test
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
//  $URL: file:///home/robbie/svn-root/xeona/futz/trunk/xeona1/a/helpers.ut0.cc $
//
//  GENERAL NOTES FOR THIS FILE
//
//  This is a unit test file and hence not essential to the
//  application proper.

//  LOCAL AND SYSTEM INCLUDES

#include "helpers.h"          // unit under test (place early)

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
  //  test ONE        :test xeona::logRankToGlpkLevel
  // ---------------------------------------------------------

  logger->test(1, "test free function 'xeona::logRankToGlpkLevel'");

  {
    int reportLevel                      = -1;
    svif::ReportingLevel svifReportLevel = svif::not_specified;

    reportLevel     = logger->getReportLevelInt();
    svifReportLevel = xeona::logRankToGlpkLevel(reportLevel);

    shared_ptr<svif::SolverIf> solver(new svif::SolverIf("high report", svifReportLevel));

    put << "   turn down reporting, capture (in silence), reinstate log level" << "\n";
    logger->putx(logga::adhc, put);

    const logga::Rank lev = logger->setReportLevel(logga::warn); // turn down reporting
    reportLevel           = logger->getReportLevelInt();
    logger->setReportLevel(lev);                                 // return to previous

    put << "   reset shared pointer" << "\n";
    logger->putx(logga::adhc, put);

    svifReportLevel = xeona::logRankToGlpkLevel(reportLevel);

    // note that 'shared_ptr<>::reset()' can now take a raw pointer

    solver.reset(new svif::SolverIf("low report", svifReportLevel));

  }

  // ---------------------------------
  //  HOUSEKEEPING
  // ---------------------------------

  logger->test(0);                           // test end

  logger->repx(logga::info, "end of main", "~~~~~~~~~~");
  return xeona::exit_success;

} // main function

//  end of file

