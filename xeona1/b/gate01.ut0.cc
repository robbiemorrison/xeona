//  XEONA ENERGY SYSTEMS MODELING ENVIRONMENT
//
//  file-create-name : gate1.ut0.cc
//  file-create-date : Wed 15-Apr-2009 21:34 UTC
//  file-initiator   : Robbie Morrison <robbie@actrix.co.nz>
//
//  file-role        : concrete gateways 1 - stated tariff / unit test
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
//  $URL: file:///home/robbie/svn-root/xeona/futz/trunk/xeona1/b/gate01.ut0.cc $
//
//  GENERAL NOTES FOR THIS FILE
//
//  This is a unit test file and hence not essential to the
//  application proper.

//  LOCAL AND SYSTEM INCLUDES

#include "gate01.h"           // unit under test (place early)

#include "../c/recset.h"      // records and fields and also record-sets
#include "../b/commods.h"     // commodities hierarchy

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

//  NOTE: could transfer to a test entity, 'GateStatedTariff'
//  last passed on r2978 with this test code
//
//    // ---------------------------------------------------------
//    //  test ONE        : basic tests
//    // ---------------------------------------------------------
//
//    logger->test(1, "basic tests");
//
//    {
//      Record r;
//      r.hackUnitTestRecord("test one");
//      const int modeSum = 128;
//
//      shared_ptr<GateStatedTariff<CmWork> >
//        test(new GateStatedTariff<CmWork>("test", r, modeSum));
//      test->unitTestSay();
//    }
//
//    // ---------------------------------------------------------
//    //  test TWO        : implicit upcast
//    // ---------------------------------------------------------
//
//    logger->test(2, "implicit upcast");
//
//    {
//      Record r;
//      r.hackUnitTestRecord("test one");
//      const int modeSum = 128;
//
//      shared_ptr<GateStatedTariff<CmWork> >
//        test2(new GateStatedTariff<CmWork>("test2", r, modeSum));
//      test2->unitTestSay();
//
//  shared_ptr<GateCom<CmWork> > test1(new GateStatedTariff<CmWork>("test1", r, modeSum));
//  shared_ptr<Gateway>          test3(new GateStatedTariff<CmWork>("test3", r, modeSum));
//    }
//
//    // ---------------------------------------------------------
//    //  test THREE      : selgate constrain call resolution
//    // ---------------------------------------------------------
//
//    logger->test(3, "selgate constrain call resolution");
//
//    {
//      // preamble
//
//      Record r;
//      r.hackUnitTestRecord("test");
//      const int modeSum = 128;
//
//      shared_ptr<GateStatedTariff<CmWork> >
//        gate(new GateStatedTariff<CmWork>("gate", r, modeSum));
//
//      // loop-specific initialization call followed by constrain call
//
//      const int step         = 12;
//      xeona::DomainMode mode = xeona::e_shortrunFin;     // 32 at the time of writing
//
//  shared_ptr<svif::SolverIf> solver(new svif::SolverIf("unit-test-solver", svif::high));
//
//      const int rowStart = solver->getConCount();
//      put << "  getConCount return : " << rowStart << "\n";
//      logger->putx(logga::dbug, put);
//
//  gate->initialize(step, solver, mode);    // CAUTION: must precede constrain-style call
//
//      const int re2 = gate->constrainSelSide();
//      put << "  constrain call return : " << re2 << "\n";
//      logger->putx(logga::dbug, put);
//
//      // member function resolution -- note the two equivalent
//      // idioms (except BuySide and SelSide)
//
//      dynamic_pointer_cast<BuySide>(gate)->registerDomain("dynamic-cast");
//      gate->SelSide::registerDomain("scope-resolution");
//    }

  // ---------------------------------
  //  HOUSEKEEPING
  // ---------------------------------

  logger->test(0);                           // test end

  logger->repx(logga::info, "end of main", "~~~~~~~~~~");
  return xeona::exit_success;

} // main function

//  end of file

