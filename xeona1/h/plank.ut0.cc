//  XEONA ENERGY SYSTEMS MODELING ENVIRONMENT
//
//  file-create-name : plank.ut0.cc
//  file-create-date : Thu 06-Jan-2011 16:56 UTC
//  file-initiator   : Robbie Morrison <robbie@actrix.co.nz>
//
//  file-role        : chiller model / unit test
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
//  $URL: file:///home/robbie/svn-root/xeona/futz/trunk/xeona1/h/plank.ut0.cc $
//
//  GENERAL NOTES FOR THIS FILE
//
//  This is a unit test file and hence not essential to the
//  application proper.

//  LOCAL AND SYSTEM INCLUDES

#include "plank.h"            // unit under test (place early)

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
  //  test ONE        : simple instantiation
  // ---------------------------------------------------------

  logger->test(1, "simple instantiation");

  {
    Chiller chiller;
  }

  // ---------------------------------------------------------
  //  test TWO        : plank model run
  // ---------------------------------------------------------

  logger->test(2, "plank model run");

  {
    Chiller chiller(4.0);                    // change default cop
    const double cop1 = chiller.coeffOfPerformance(10.0, 30.0);
    const double cop2 = chiller.coeffOfPerformance(10.0, 40.0);

    put << "  summary" << "\n"
        << "    cop1 : " << cop1 << "\n"     // 8.49
        << "    cop2 : " << cop2 << "\n";    // 5.07
    logger->putx(logga::dbug, put);
  }

  // ---------------------------------------------------------
  //  test THREE      : out-of-bounds temperature
  // ---------------------------------------------------------

  logger->test(3, "out-of-bounds temperature (std::domain_error)");

  {
    Chiller chiller;
    const double cop = chiller.coeffOfPerformance(10.0,     // [5,15]
                                                  20.0);    // [25,50] => error

    put << "  summary" << "\n"
        << "    cop  : " << cop  << "\n";    // error
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

