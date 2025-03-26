//  XEONA ENERGY SYSTEMS MODELING ENVIRONMENT
//
//  file-create-name : xedocs.ut0.cc
//  file-create-date : Tue 23-Sep-2008 06:29 UTC
//  file-initiator   : Robbie Morrison <robbie@actrix.co.nz>
//
//  file-role        : processing of 'xedoc' entity documentation / unit test
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
//  $URL: file:///home/robbie/svn-root/xeona/futz/trunk/xeona1/a/xedocs.ut0.cc $
//
//  GENERAL NOTES FOR THIS FILE
//
//  This is a unit test file and hence not essential to the
//  application proper.
//
//  This unit test requires the text file defined by
//  'XEDOCS_FILE' be present.  Moreover the contents of this text
//  file (even if lacking information) must be set in double
//  quotes.

//  LOCAL AND SYSTEM INCLUDES

#include "xedocs.h"           // unit under test (place early)

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
  //  test ONE        : simple creation
  // ---------------------------------------------------------

  logger->test(1, "simple creation");

  {
    Xedocs xedocs;
  }

  // ---------------------------------------------------------
  //  test TWO        : search for class
  // ---------------------------------------------------------

  logger->test(2, "search for class");

  {
    std::string result1 = "";
    std::string result2 = "";
    Xedocs searchable;
    Xedocs::FindStatus findStatus1 = searchable.findXedocForClass("One", result1);
    put << "  find status (1 is good): " << findStatus1 << "\n\n";
    put << result1 << "\n";
    logger->putx(logga::dbug, put);
    Xedocs::FindStatus findStatus2 = searchable.findXedocForClass("Nonexistent", result2);
    put << "  find status (1 is good): " << findStatus2 << "\n\n";
    put << result2 << "\n";
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

