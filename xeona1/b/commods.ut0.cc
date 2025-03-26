//  XEONA ENERGY SYSTEMS MODELING ENVIRONMENT
//
//  file-create-name : commods.ut0.cc
//  file-create-date : Wed 30-Jul-2008 07:37 UTC
//  file-initiator   : Robbie Morrison <robbie@actrix.co.nz>
//
//  file-role        : commodities hierarchy / unit test
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
//  $URL: file:///home/robbie/svn-root/xeona/futz/trunk/xeona1/b/commods.ut0.cc $
//
//  GENERAL NOTES FOR THIS FILE
//
//  This is a unit test file and hence not essential to the
//  application proper.

//  LOCAL AND SYSTEM INCLUDES

#include "commods.h"          // unit under test (place early)

#include "../c/recset.h"      // record set support
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
  //  test ONE        : make a simple CmHeat
  // ---------------------------------------------------------
  //
  //  CAUTION: tied commodities
  //
  //      Cannot test with commodities containing tied data
  //      because a read-in 'Record' instance cannot be easily
  //      faked.
  //
  // ---------------------------------------------------------

  logger->test(1, "make a simple CmHeat");

  {
    Record r;
    r.hackUnitTestRecord("simple heat commodity");

    shared_ptr<Commodity> cmHeat(new CmHeat("commod-heat-1", r));
    // shared_ptr<CmHeat> cmHeat(new CmHeat("commod-heat-1", r)); works too!

    put << "  interface pairs : "
        << cmHeat->getInterfacePairs()
        << "\n";
    logger->putx(logga::dbug, put);
  }

  // ---------------------------------------------------------
  //  test TWO        : physico-chemical definitions
  // ---------------------------------------------------------

  logger->test(2, "physico-chemical definitions");

  {
    put << "  absolute zero [degrees C] = "
        << xeona::absoluteZero
        << "\n";
    put << "  mass specific heat capacity of liquid water [J/kgC] = "
        <<  xeona::massCp_liquidWater
        << "\n";
    logger->putx(logga::dbug, put);
  }

  // ---------------------------------------------------------
  //  test THREE      : tie constant trial
  // ---------------------------------------------------------

  logger->test(3, "tie constant trial");

  {
//      Record r;
//      r.hackUnitTestRecord("simple oxidize commodity");
//
//      shared_ptr<CmOxidize> cmOxidize(new CmOxidize("commod-oxid-1", r));
//
//      put << "  specific enthalpy : "
//          << cmOxidize->getSpecCombEnthalpy()
//          << "\n";
//      logger->putx(logga::dbug, put);
  }

  // ---------------------------------
  //  HOUSEKEEPING
  // ---------------------------------

  logger->test(0);                           // test end

  logger->repx(logga::info, "end of main", "~~~~~~~~~~");
  return xeona::exit_success;

} // main function

//  end of file

