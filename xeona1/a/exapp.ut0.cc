//  XEONA ENERGY SYSTEMS MODELING ENVIRONMENT
//
//  file-create-name : app_except.ut0.cc
//  file-create-date : Tue 28-Apr-2009 10:15 UTC
//  file-initiator   : Robbie Morrison <robbie@actrix.co.nz>
//
//  file-role        : application exception classes / unit test
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
//  $URL: file:///home/robbie/svn-root/xeona/futz/trunk/xeona1/a/exapp.ut0.cc $
//
//  GENERAL NOTES FOR THIS FILE
//
//  This is a unit test file and hence not essential to the
//  application proper.
//
//  AD-HOC NOTES

//  LOCAL AND SYSTEM INCLUDES

#include "exapp.h"            // unit under test (place early)

#include "../a/logger.h"      // standard logging functionality (as required)
#include "../c/smart_ptr.h"   // toggle between Boost and TR1 smart pointers
#include ".././common.h"      // common definitions for project (place last)

#include <string>             // C++ strings
#include <sstream>            // string-streams

//  CODE

// ---------------------------------------------------------
//  FREE FUNCTION   : ::buggy
// ---------------------------------------------------------

namespace
{
  bool buggy()
    throw(xeona::app_exception,              // catch all
          xeona::empty_wrap)                 // specific
  {
    logga::spLogger logger = logga::ptrLogStream();  // main function logger
    logger->repx(logga::dbug, "entering member function", "");
    throw xeona::empty_wrap("type");
    return true;
  }
} // unnamed namespace

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
  //  test ONE        : buggy function call
  // ---------------------------------------------------------

  logger->test(1, "buggy function call");

  {
    try
      {
        ::buggy();
      }
    catch ( const xeona::app_exception& ex )
      {
        put << ex.expl() << "\n";
        logger->putx(logga::warn, put);
        logger->flush();

        std::cout << "  " << ex.tell() << " caught, execution abandoned" << "\n";
        std::cout << std::flush;

        // return ex.code();

        put << "  hardcoded exit code (would normally be returned): "
            << ex.code() << "\n";
        logger->putx(logga::warn, put);
      }
    catch ( ... )
      {
        put << "  catch all" << "\n";
        logger->putx(logga::warn, put);
      }
  }

  // ---------------------------------
  //  HOUSEKEEPING
  // ---------------------------------

  logger->test(0);                           // test end

  logger->repx(logga::info, "end of main", "~~~~~~~~~~");
  return xeona::exit_success;

} // main function

//  end of file

