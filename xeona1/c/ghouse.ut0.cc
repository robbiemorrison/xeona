//  XEONA ENERGY SYSTEMS MODELING ENVIRONMENT
//
//  file-create-name : greenhouse.ut0.cc
//  file-create-date : Mon 12-Oct-2009 09:10 UTC
//  file-initiator   : Robbie Morrison <robbie@actrix.co.nz>
//
//  file-role        : global warming potential support / unit test
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
//  $URL: file:///home/robbie/svn-root/xeona/futz/trunk/xeona1/c/ghouse.ut0.cc $
//
//  GENERAL NOTES FOR THIS FILE
//
//  This is a unit test file and hence not essential to the
//  application proper.

//  LOCAL AND SYSTEM INCLUDES

#include "ghouse.h"           // unit under test (place early)

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
  //  test ONE        : create some GWP bundles using class 'Gwp100Bundle'
  // ---------------------------------------------------------

  logger->test(1, "create zero-argument GWP gas bundle using class 'Gwp100Bundle'");

  {
    Gwp100Bundle gwp();
  }

  // ---------------------------------------------------------
  //  test TWO        : create some GWP bundles using class 'Gwp100Bundle'
  // ---------------------------------------------------------

  logger->test(2, "create some more complex GWP bundles");

  {
    Gwp100Bundle gwp(2.0, 1.0, 1.0);
    gwp  = 5 * gwp;                          // multiply by five
    gwp *= 2;                                // and multiple by two
    put << gwp.summarizeMe("test two");
    logger->putx(logga::dbug, put);
    gwp.reset();
  }

  // ---------------------------------------------------------
  //  test THREE      : GWP data (static call)
  // ---------------------------------------------------------

  logger->test(3, "GWP data (static call)");

  {
    put << Gwp100Bundle::displayGwps();
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

