//  XEONA ENERGY SYSTEMS MODELING ENVIRONMENT
//
//  file-create-name : ospinfo.ut0.cc
//  file-create-date : Mon 19-Oct-2009 11:04 UTC
//  file-initiator   : Robbie Morrison <robbie@actrix.co.nz>
//
//  file-role        : domain mode interpretation / unit test
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
//  $URL: file:///home/robbie/svn-root/xeona/futz/trunk/xeona1/f/ospinfo.ut0.cc $
//
//  GENERAL NOTES FOR THIS FILE
//
//  This is a unit test file and hence not essential to the
//  application proper.

//  LOCAL AND SYSTEM INCLUDES

#include "ospinfo.h"          // unit under test (place early)

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
  //  test ONE        : mixed test
  // ---------------------------------------------------------

  logger->test(1, "mixed test");

  {
    const int margin = 2;
    int modes;
    modes  =  2;                             // works as expected
    modes  = 15;                             // works as expected
    put << xeona::infoDomainModeLong(modes, margin);
    logger->putx(logga::dbug, put);
  }

  // ---------------------------------------------------------
  //  test TWO        : dump
  // ---------------------------------------------------------

  logger->test(2, "dump");

  {
    DomainModeDatabase data;
    data.test(put);
    logger->putx(logga::dbug, put);
  }

  // ---------------------------------------------------------
  //  test THREE      : pure test
  // ---------------------------------------------------------

  logger->test(3, "pure test");

  {
    int mode;
    mode = -1;          // "mode not found"
    mode = 2048 + 1;    // "mode found but mixed"
    mode = 2048;        // "2048 = commitment : locational marginal pricing auction"

    put << "  interpretation (" << mode << ") : "
        << xeona::infoDomainModePure(mode) << "\n";
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

