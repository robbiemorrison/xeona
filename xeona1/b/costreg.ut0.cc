//  XEONA ENERGY SYSTEMS MODELING ENVIRONMENT
//
//  file-create-name : costreg.ut0.cc
//  file-create-date : Mon 20-Oct-2008 08:57 UTC
//  file-initiator   : Robbie Morrison <robbie@actrix.co.nz>
//
//  file-role        : cost registers / unit test
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
//  $URL: file:///home/robbie/svn-root/xeona/futz/trunk/xeona1/b/costreg.ut0.cc $
//
//  GENERAL NOTES FOR THIS FILE
//
//  This is a unit test file and hence not essential to the
//  application proper.

//  LOCAL AND SYSTEM INCLUDES

#include "costreg.h"          // unit under test (place early)

#include "../c/recset.h"      // records and fields and also record-sets

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
  //  test ONE        : CostRegister instance test
  // ---------------------------------------------------------

  logger->test(1, "CostRegister instance test");

  {
    Record r1;
    r1.hackUnitTestRecord("test one");
    shared_ptr<CostRegister> costreg(new CostRegister(r1));
    costreg->resetRegister();
  }

  // ---------------------------------------------------------
  //  test TWO        : CostRegisterDCF instance test
  // ---------------------------------------------------------

  logger->test(2, "CostRegisterDCF instance test");

  {
    // Record r2;
    // r2.hackUnitTestRecord("test two");

    // CAUTION: the following call causes exit(11), whereas the
    // test one call does not -- the most likely explanation is
    // that 'CostRegister' contains only timeseries as shared
    // pointers (holding vectors), while CostRegisterDCF contains
    // references (to native datatypes).  The following code is
    // thus duly disabled.

    // shared_ptr<CostRegisterFin> costregfin(new CostRegisterFin(r2));
    // costregfin->resetAllCosts();

    // shared_ptr<CostRegister> costregdcf(new CostRegisterDcf(r2));
    // costregdcf->resetStandardCosts();
  }

  // ---------------------------------
  //  HOUSEKEEPING
  // ---------------------------------

  logger->test(0);                           // test end

  logger->repx(logga::info, "end of main", "~~~~~~~~~~");
  return xeona::exit_success;

} // main function

//  end of file

