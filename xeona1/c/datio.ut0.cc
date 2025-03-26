//  XEONA ENERGY SYSTEMS MODELING ENVIRONMENT
//
//  file-create-name : datio.ut0.cc
//  file-create-date : Mon 01-Oct-2007 12:24 UTC
//  file-initiator   : Robbie Morrison <robbie@actrix.co.nz>
//
//  file-role        : model io (limited to object instantiation) / unit test
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
//  $URL: file:///home/robbie/svn-root/xeona/futz/trunk/xeona1/c/datio.ut0.cc $
//
//  GENERAL NOTES FOR THIS FILE
//
//  This is a unit test file and hence not essential to the
//  application proper.

//  LOCAL AND SYSTEM INCLUDES

#include "datio.h"            // unit under test (place early)

#include "../a/logger.h"      // standard logging functionality (as required)
#include "../c/smart_ptr.h"   // toggle between Boost and TR1 smart pointers
#include ".././common.h"      // common definitions for project (place last)

#include <iostream>           // standard io
#include <sstream>            // string-streams
#include <string>             // C++ strings

#include <boost/algorithm/string.hpp>   // string recasing, trimming, splitting
#include <boost/foreach.hpp>            // BOOST_FOREACH macro

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

  std::ostringstream put;

  // ---------------------------------------------------------
  //  test ONE        : very basic testing
  // ---------------------------------------------------------

  logger->test(1);

  {
    // a couple of object instantiations
    boost::filesystem::path model;           // now empty, will be a complete path
    RecordSet recset;                        // external object
    DataIo dataIo (recset);                  // data io admin object
  }

  // FINAL MESSAGE

  put << "** very basic trials, should probably be unit testing 'datrec.ut1.cc' instead"
      << "\n";
  logger->putx(logga::warn, put);

  // ---------------------------------
  //  HOUSEKEEPING
  // ---------------------------------

  logger->test(0);                           // test end

  logger->repx(logga::info, "end of main", "~~~~~~~~~~");
  return xeona::exit_success;

} // main function

//  end of file

