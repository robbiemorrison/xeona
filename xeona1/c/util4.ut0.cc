//  XEONA ENERGY SYSTEMS MODELING ENVIRONMENT
//
//  file-create-name : util4.ut0.cc
//  file-create-date : Fri 05-Mar-2010 09:11 UTC
//  file-initiator   : Robbie Morrison <robbie@actrix.co.nz>
//
//  file-role        : free functions for trigonometry and maths / unit test
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
//  $URL: file:///home/robbie/svn-root/xeona/futz/trunk/xeona1/c/util4.ut0.cc $
//
//  GENERAL NOTES FOR THIS FILE
//
//  This is a unit test file and hence not essential to the
//  application proper.

//  LOCAL AND SYSTEM INCLUDES

#include "util4.h"            // unit under test (place early)

#include "../a/logger.h"      // standard logging functionality (as required)
#include "../c/smart_ptr.h"   // toggle between Boost and TR1 smart pointers
#include ".././common.h"      // common definitions for project (place last)

#include <string>             // C++ strings
#include <sstream>            // string-streams

#include <boost/math/constants/constants.hpp>     // high-precision mathematical constants

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
  const double PI = boost::math::constants::pi<double>();   // "static" is file-local

  // ---------------------------------------------------------
  //  test ONE        : straight function tests
  // ---------------------------------------------------------

  logger->test(1, "straight function tests");

  {
    put << std::showpos;

    const double deg1 = -30.0;
    const double rad0 = xeona::degree2radian(deg1);
    const double rad1 = xeona::normalizePlusMinusPi(xeona::degree2radian(deg1));
    const double rad2 = xeona::normalizeZeroTwoPi(xeona::degree2radian(deg1));

    put << "    degrees (-30) to radians" << "\n"
        << "    deg1       : " << deg1    << "\n"
        << "    rad0       : " << rad0    << "\n"
        << "    rad1       : " << rad1    << "\n"
        << "    rad2       : " << rad2    << "\n";

    logger->putx(logga::dbug, put);

    const double rad3 = -4.0 * PI;
    const double deg3 = xeona::radian2degree(rad3);
    const double deg4 = xeona::radian2degree(xeona::normalizeZeroTwoPi(rad3));

    put << "\n";
    put << "    radians (-4pi) to degrees" << "\n"
        << "    rad3       : " << rad3     << "\n"
        << "    deg3       : " << deg3     << "\n"
        << "    deg4       : " << deg4     << "\n";

    logger->putx(logga::dbug, put);
  }

  // ---------------------------------------------------------
  //  test TWO        : wrapper function tests
  // ---------------------------------------------------------

  logger->test(2, "wrapper function tests");

  {
    put << std::showpos;

    const double deg1 = -360.0;
    const double deg2 = xeona::normalizePlusMinus180(deg1);
    const double deg3 = xeona::normalizeZero360(deg1);

    put << "    degrees (-360)" << "\n"
        << "    deg1       : " << deg1    << "\n"
        << "    deg2       : " << deg2    << "\n"
        << "    deg3       : " << deg3    << "\n";

    logger->putx(logga::dbug, put);
  }

  // ---------------------------------------------------------
  //  test THREE      : log_2 tests
  // ---------------------------------------------------------

  logger->test(3, "log_2 (aka lb) tests");

  {
    const double num1   = 32.0;
    const double lbNum1 = xeona::log2(num1);
    put << std::noshowpos;
    put << "    num1   : " << num1   << "\n"
        << "    log_2  : " << lbNum1 << "\n";
    logger->putx(logga::dbug, put);

    const double num2   = 0.0;
    const double lbNum2 = xeona::log2(num2);
    put << "    num2   : " << num2   << "\n"
        << "    log_2  : " << lbNum2 << "\n";
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

