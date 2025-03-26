//  XEONA ENERGY SYSTEMS MODELING ENVIRONMENT
//
//  file-create-name : bandtaf.ut0.cc
//  file-create-date : Tue 18-Nov-2008 09:37 UTC
//  file-initiator   : Robbie Morrison <robbie@actrix.co.nz>
//
//  file-role        : banded tariff set and support / unit test
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
//  $URL: file:///home/robbie/svn-root/xeona/futz/trunk/xeona1/b/bandtaf.ut0.cc $
//
//  GENERAL NOTES FOR THIS FILE
//
//  This is a unit test file and hence not essential to the
//  application proper.

//  LOCAL AND SYSTEM INCLUDES

#include "bandtaf.h"          // unit under test (place early)

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
  //  test ONE        : create a banded-tariffset
  // ---------------------------------------------------------

  logger->test(1, "create a banded-tariffset");

  {
    BandedTariffSet tariffs("for-unit-testing");
  }

  // ---------------------------------------------------------
  //  test TWO        : fill, report, and pop a tariffset
  // ---------------------------------------------------------

  logger->test(2, "fill, report, and pop a tariffset");

  {
    BandedTariffSet tariffs("test-two");

    std::string tariffStr;                   // rotate as required
    tariffStr = "";                          // nothing!
    tariffStr = "4.4 * 8.8";                 // empty with two fixed charges given
    tariffStr = "4.4 * 80.00e+06 0.0";       // take-or-pay
    tariffStr = "4.4 * 40e+06 28e-09 * 30e+06 40e-09 * 20e+06 80e-09";  // convex
    tariffStr = "4.4 * 40e+06 28e-09 * 30e+06 10e-09 * 20e+06 80e-09";  // non-convex

    put << "  " << "tariff string : " << tariffStr << "\n";
    logger->putx(logga::dbug, put);
    logger->addSmartBlank(logga::dbug);

    tariffs.pushString(tariffStr);

    // note that 'summarizeAll' also tests a lot of internal calls

    put << tariffs.summarizeAll();             // trailing newline not required
    logger->putx(logga::dbug, put);
    logger->addSmartBlank(logga::dbug);

    std::pair<double, double> tariff = tariffs.popFirst();
    put << "  " << "quantity : " << tariff.first  << "\n"
        << "  " << "price    : " << tariff.second << "\n";
    logger->putx(logga::dbug, put);
    logger->addSmartBlank(logga::dbug);
  }

  // ---------------------------------------------------------
  //  test THREE      : capacitation
  // ---------------------------------------------------------

  logger->test(3, "capacitation");

  {
    BandedTariffSet tariffs("test-three");

    const std::string tariffStr
      = "4.4"
      " * 40e+06 28e-09"
      " * 30e+06 10e-09"
      " * 20e+06 80e-09";                    // non-convex
    const double capacity = 60.0e+06;

    put << std::boolalpha
        << "  " << "opening band sum : " << tariffs.getBandSum()  << "\n"
        << "  " << "opening capacity : " << tariffs.getCapacity() << "\n";

    tariffs.pushString(tariffStr);
    const bool ret = tariffs.truncate(capacity);

    put << "  " << "new band sum     : " << tariffs.getBandSum()  << "\n"
        << "  " << "new capacity     : " << tariffs.getCapacity() << "\n"
        << "  " << "return           : " << ret                   << "\n";
    logger->putx(logga::dbug, put);
    logger->addSmartBlank(logga::dbug);
  }

  // ---------------------------------------------------------
  //  test FOUR       : marginal price
  // ---------------------------------------------------------

  logger->test(4, "marginal price");

  {
    BandedTariffSet tariffs("test-four");

    const std::string tariffStr
      = "4.4"
      " * 40e+06 28e-09"
      " * 30e+06 10e-09"
      " * 20e+06 80e-09";                    // non-convex

    tariffs.pushString(tariffStr);

    const double testSale      = 50e+06;
    const double marginalPrice = tariffs.getMarginalPrice(testSale);

    put << "  " << "test sale (50e+06))     : " << testSale              << "\n"
        << "  " << "marginalPrice (10e-09)) : " << marginalPrice         << "\n";

    logger->putx(logga::dbug, put);
    logger->addSmartBlank(logga::dbug);

  }

  // ---------------------------------------------------------
  //  test FIVE       : copy construction
  // ---------------------------------------------------------

  logger->test(5, "copy construction");

  {
    shared_ptr<BandedTariffSet> tafsA(new BandedTariffSet("test-5"));

    const std::string tafStr = "40.00e+06 10.00e-09 * 20.00e+06 20.00e-09";
    tafsA->pushString(tafStr);

    shared_ptr<BandedTariffSet> tafsB(new BandedTariffSet(*tafsA));
  }

  // ---------------------------------------------------------
  //  test SIX        : copy assignment
  // ---------------------------------------------------------

  logger->test(6, "copy assignment");

  {
    shared_ptr<BandedTariffSet> tafsA(new BandedTariffSet("test-6a"));

    const std::string tafstr = "40.00e+06 10.00e-09 * 20.00e+06 20.00e-09";
    tafsA->pushString(tafstr);

    shared_ptr<BandedTariffSet> tafsB(new BandedTariffSet("test-6b"));     // [1]
    *tafsB = *tafsA;                                                       // [2]

    // [1] must allocate memory
    // [2] must dereference
  }

  // ---------------------------------
  //  HOUSEKEEPING
  // ---------------------------------

  logger->test(0);                           // test end

  logger->repx(logga::info, "end of main", "~~~~~~~~~~");
  return xeona::exit_success;

} // main function

//  end of file

