//  XEONA ENERGY SYSTEMS MODELING ENVIRONMENT
//
//  file-create-name : files.ut0.cc
//  file-create-date : Fri 09-Nov-2007 13:39 UTC
//  file-initiator   : Robbie Morrison <robbie@actrix.co.nz>
//
//  file-role        : file utilities (writability) / unit test
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
//  $URL: file:///home/robbie/svn-root/xeona/futz/trunk/xeona1/c/files.ut0.cc $
//
//  GENERAL NOTES FOR THIS FILE
//
//  This is a unit test file and hence not essential to the
//  application proper.

//  AD-HOC NOTES

//  LOCAL AND SYSTEM INCLUDES

#include "files.h"            // unit under test (place early)

#include "../a/logger.h"      // standard logging functionality (as required)
#include "../c/smart_ptr.h"   // toggle between Boost and TR1 smart pointers
#include ".././common.h"      // common definitions for project (place last)

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
  //  test ONE       : path is writable
  // ---------------------------------------------------------

  logger->test(1);

  {
    // create an empty Boost.Filesystem path object using default
    // (zero-argument) construction

    boost::filesystem::path empty;           // empty path object
    if ( xeona::testWritable(empty) != true )
      logger->repx(logga::warn, "file not proved writable", "as expected");

    // create a valid Boost.Filesystem path object, albeit zero
    // size using the 'touch' utility bundled with UNIX and Linux

#ifdef __unix__  // UNIX-specific code

    std::string filename = "pathWriteFile.tmp";
    std::string call;
    call += "touch ";                        // UNIX 'touch' utility
    call += filename;
    if ( system(call.c_str()) != 0 )
      logger->repx(logga::warn, "system() returned fail", "");
    boost::filesystem::path full(filename);
    if ( xeona::testWritable(full) != true )
      logger->repx(logga::warn, "file not proved writable", "NOT expected");
    else
      logger->repx(logga::info, "file is writable", "as expected");
    boost::filesystem::remove(filename);     // clean up

#else
# warning "UNIX-specific code being omitted"
#endif // __unix__

  }

  // ---------------------------------------------------------
  //  test TWO        : xeona::readonly
  // ---------------------------------------------------------

  logger->test(2, "xeona::readonly");

  {
    const std::string filename = "eraseme";

    const std::string call = "touch " + filename;
    system(call.c_str());

    const bool ret = xeona::readonly(filename);
    put << std::boolalpha
        << "  xeona::readonly(" << filename << ") call returned: " << ret << "\n";

    logger->putx(logga::dbug, put);
  }

  // ---------------------------------------------------------
  //  test THREE      : xeona::temporaryFilename
  // ---------------------------------------------------------

  logger->test(3, "xeona::temporaryFilename");

  {
    const std::string temp1 = xeona::temporaryFilename();
    const std::string temp2 = xeona::temporaryFilename("xxx");
    const std::string temp3 = xeona::temporaryFilename("xxx", "stub-");

    put << "  temporary file name (no extension)   : " << temp1 << "\n";
    put << "  temporary file name (with extension) : " << temp2 << "\n";
    put << "  temporary file name (with stub too)  : " << temp3 << "\n";

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

