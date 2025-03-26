//  XEONA ENERGY SYSTEMS MODELING ENVIRONMENT
//
//  file-create-name : inbuilt.ut0.cc
//  file-create-date : Mon 14-Jan-2008 15:30 UTC
//  file-initiator   : Robbie Morrison <robbie@actrix.co.nz>
//
//  file-role        : compiled-in test file generation / unit test
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
//  $URL: file:///home/robbie/svn-root/xeona/futz/trunk/xeona1/c/inbuilt.ut0.cc $
//
//  GENERAL NOTES FOR THIS FILE
//
//  This is a unit test file and hence not essential to the
//  application proper.

//  LOCAL AND SYSTEM INCLUDES

#include "inbuilt.h"          // unit under test (place early)

#include "../a/logger.h"      // standard logging functionality (as required)
#include "../c/smart_ptr.h"   // toggle between Boost and TR1 smart pointers
#include ".././common.h"      // common definitions for project (place last)

#include <string>             // C++ strings
#include <sstream>            // string-streams

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

  const std::string modelname = "inbuilt.xem";    // this is now the default too
  const unsigned steps = 5;

  // ---------------------------------------------------------
  //  test ONE        : create model name routine
  // ---------------------------------------------------------

  logger->test(1);

  {
    put << "about to test createModelName()" << "\n";
    put << "\n";
    logger->putx(logga::dbug, put);

    const std::string null = "";
    put << "    " << null << "(null) -> " << xeona::createModelName(null)    << "\n";
    logger->putx(logga::dbug, put);

    const std::string leaf = "leaf";
    put << "    " << leaf << " -> "       << xeona::createModelName(leaf)    << "\n";
    logger->putx(logga::dbug, put);

    const std::string partial = "relpath/stub";
    put << "    " << partial << " -> "    << xeona::createModelName(partial) << "\n";
    logger->putx(logga::dbug, put);

    const std::string full = "/complete/path.xem";
    put << "    " << full << " -> "       << xeona::createModelName(full)    << "\n";
    logger->putx(logga::dbug, put);
  }

  // ---------------------------------------------------------
  //  test TWO        : dumping to 'modelname'
  // ---------------------------------------------------------

  logger->test(2);

  put << "about to generate file: " << modelname  << "\n";
  logger->putx(logga::dbug, put);

  bool ret = xeona::dumpToFile(modelname, steps);

  if ( ! ret )
    {
      put <<  "    dump to file failed"           << "\n";
      logger->putx(logga::warn, put);
      return 1;
    }
  else
    {
      put <<  "    dump to file succeeded"        << "\n";
      logger->putx(logga::dbug, put);
    }

  // ---------------------------------------------------------
  //  test THREE      : show line count
  // ---------------------------------------------------------

  logger->test(3);

#ifdef __unix__ // UNIX-specific code

  put <<  "    filename   : " << modelname          << "\n";
  logger->putx(logga::dbug, put);

  std::string call;
  call += "wc --line " + modelname;
  call += " | awk '{ print \"    line count : \" $1 }'";
  const char* ccall = call.c_str();
  system(ccall);

#else

  put
    <<  "unix specific code being omitted, refer to unit test file: "
    << __FILE__
    << "\n";
  logger->putx(logga::warn, put);

#endif // __unix__

  // ---------------------------------
  //  HOUSEKEEPING
  // ---------------------------------

  logger->test(0);                           // test end

  logger->repx(logga::info, "end of main", "~~~~~~~~~~");
  return xeona::exit_success;

} // main function

//  end of file

