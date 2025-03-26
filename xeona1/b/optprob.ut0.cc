//  XEONA ENERGY SYSTEMS MODELING ENVIRONMENT
//
//  file-create-name : optprob.ut0.cc
//  file-create-date : Fri 17-Oct-2008 08:20 UTC
//  file-initiator   : Robbie Morrison <robbie@actrix.co.nz>
//
//  file-role        : optimization sub-problem (OSP) and key sub-classes / unit test
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
//  $URL: file:///home/robbie/svn-root/xeona/futz/trunk/xeona1/b/optprob.ut0.cc $
//
//  GENERAL NOTES FOR THIS FILE
//
//  This is a unit test file and hence not essential to the
//  application proper.

//  LOCAL AND SYSTEM INCLUDES

#include "optprob.h"          // unit under test (place early)

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
  //  test ONE        : solver interface creation
  // ---------------------------------------------------------

  logger->test(1, "solver interface creation (in preparation for test 2)");

  shared_ptr<svif::SolverIf> solver(new svif::SolverIf("unit-test"));

  // ---------------------------------------------------------
  //  test TWO        : 'Connection' creation
  // ---------------------------------------------------------

  std::string msg2;
  msg2 += "connection OSP creation (the only concrete class in this unit)";
  msg2 += ", note intentional gol index collision";

  logger->test(2, msg2);

  shared_ptr<ConnectionOsp> cnn(new ConnectionOsp(solver));

  std::vector<int> gols;
  gols.push_back(12);

  bool ret1 = cnn->bindGols(gols, gols);
  put << "  connection complete (no)  : " << (ret1 ? "yes" : "no") << "\n";
  logger->putx(logga::dbug, put);

  // ---------------------------------------------------------
  //  test THREE      : report on OSP build integrity
  // ---------------------------------------------------------

  logger->test(3, "report on OSP Connection class build integrity");

  cnn->reportBuildIntegrity(logga::dbug);

  // ---------------------------------------------------------
  //  test FOUR        : 'Coupling' creation and use
  // ---------------------------------------------------------

  logger->test(4, "coupling creation and use");

  shared_ptr<svif::SolverIf> solver2(new svif::SolverIf("coupling"));

  shared_ptr<CouplingOsp> cpl(new CouplingOsp(solver2));
  cpl->coupleGols(0, 0, "safe");             // non-zeros will crash GLPK

  // ---------------------------------------------------------
  //  test FIVE       : free function 'couple'
  // ---------------------------------------------------------

  logger->test(5, "free function couple");

  xeona::couple(solver2, 0, 0, "safe");      // 'solver' created in test one

  // ---------------------------------------------------------
  //  test SIX        : counts test
  // ---------------------------------------------------------

  logger->test(6, "counts test");

  if ( ! cnn->checkCounts() )
    {
      cnn->reportBuildIntegrity(logga::dbug, "test six");
    }
  else
    {
      logger->repx(logga::dbug, "check counts clean", "");
    }

  // ---------------------------------
  //  HOUSEKEEPING
  // ---------------------------------

  logger->test(0);                           // test end

  logger->repx(logga::info, "end of main", "~~~~~~~~~~");
  return xeona::exit_success;

} // main function
//  end of file

