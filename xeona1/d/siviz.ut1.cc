//  XEONA ENERGY SYSTEMS MODELING ENVIRONMENT
//
//  file-create-name : siviz.ut1.cc
//  file-create-date : Mon 09-Jun-2008 14:07 UTC
//  file-initiator   : Robbie Morrison <robbie@actrix.co.nz>
//
//  file-role        : solver interface plus HTML visualization / unit test
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
//  $URL: file:///home/robbie/svn-root/xeona/futz/trunk/xeona1/d/siviz.ut1.cc $
//
//  GENERAL NOTES FOR THIS FILE
//
//  This is a unit test file and hence not essential to the
//  application proper.

//  LOCAL AND SYSTEM INCLUDES

#include "siglp.h"            // unit under test (place early)
#include "glpkviz.h"          // unit under test (place early)

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
  //  test ONE        : sample problem
  // ---------------------------------------------------------

  logger->test(1);

  {
    put << "sample.c problem" << "\n";
    logger->putx(logga::dbug, put);

    // INITIALIZATION

    shared_ptr<svif::SolverIf> sample
      (new svif::SolverIf
       ("glpk-tutorial",                     // mandatory
        svif::high));                        // defaults to level chosen by _XDEBUG

    // optional initialization calls

    sample->initSimplexPresolver();
    sample->initSetPrefLPSolver(svif::solver_interior);

    //  BUILD PROBLEM

    // set objective sense

    sample->setObjectiveSense(svif::maximize, "higher-goal");

    // cols as (normally non-zero but can be negative) objective coefficients

    sample->loadObj(1, 10.0, "x1");
    sample->loadObj(2,  6.0, "x2");
    sample->loadObj(3,  4.0, "x3");

    // rows as potentially negative RHS constraints

    sample->loadRhs(1, 100, svif::L, "p");   // L indicates '<= rhs'
    sample->loadRhs(2, 600, svif::L, "q");
    sample->loadRhs(3, 300, svif::L, "r");

    // rows x cols as (normally non-zero but can be negative) structural coefficients

    sample->loadCof(1, 1,  1.0);
    sample->loadCof(1, 2,  1.0);
    sample->loadCof(1, 3,  1.0);

    sample->loadCof(2, 1, 10.0);
    sample->loadCof(2, 2,  4.0);
    sample->loadCof(2, 3,  5.0);

    sample->loadCof(3, 1,  2.0);
    sample->loadCof(3, 2,  2.0);
    sample->loadCof(3, 3,  6.0);

    // extension to make MILP, overrides any interior point
    // preference (comment in or out)

    sample->markVarInteger(2);               // mark col as integer
    double inf = sample->getInf();
    sample->reviseBnds(2, -inf, 2.0);        // reset bounds

    //  SOLVE PROBLEM

    sample->runSolver();

    //  REPORT RESULTS

    // check first for alerts

    int alertCount = sample->getAlertCount();

    put << "   alert count : "
        << alertCount
        << "\n";

    logger->putx(logga::dbug, put);
    logger->addSmartBlank();

    // obtain results

    put << sample->getFormattedResults();
    logger->putx(logga::dbug, put);

    // display known results

    put << "\n"
        << "   KNOWN LP RESULTS\n"
        << "   Z  = 733.3333\n"
        << "   x1 =  33.3333\n"
        << "   x2 =  66.6667\n"
        << "   x3 =   0.0\n";

    logger->putx(logga::dbug, put);

    //  VISUALIZE PROBLEM INSTANCE

    GlpkViz webbrowse("firefox");
    webbrowse(sample, "sample model but modified", 4, true);

    // HOUSEKEEPING

    // free memory -- although the only instance is about to go
    // out of scope in any case

    sample.reset();                          // redundant in the circumstances

  }

  // ---------------------------------------------------------
  //  test TWO        : plan.lp problem
  // ---------------------------------------------------------

  logger->test(2);

  {
    put << "plan.lp problem" << "\n";
    logger->putx(logga::dbug, put);

    // INITIALIZATION

    shared_ptr<svif::SolverIf>
      mip(new svif::SolverIf
          ("",                               // mandatory
           svif::high));                     // defaults to level chosen by _XDEBUG

    //  BUILD PROBLEM (see elsewhere for commentary)

    mip->setProblemLabel("plan.lp-siviz");

    // objective

    mip->setObjectiveSense(svif::minimize);

    mip->loadObj(1, 0.03, "bin-1");
    mip->loadObj(2, 0.08, "bin-2");
    mip->loadObj(3, 0.17, "bin-3");
    mip->loadObj(4, 0.12, "bin-4");
    mip->loadObj(5, 0.15, "bin-5");
    mip->loadObj(6, 0.21, "alum");
    mip->loadObj(7, 0.38, "silicon");

    // structural constraints

    mip->loadRhs(1, 2000.0, svif::E,   "yield");

    mip->loadCof(1, 1, 1.0);
    mip->loadCof(1, 2, 1.0);
    mip->loadCof(1, 3, 1.0);
    mip->loadCof(1, 4, 1.0);
    mip->loadCof(1, 5, 1.0);
    mip->loadCof(1, 6, 1.0);
    mip->loadCof(1, 7, 1.0);

    mip->loadRhs(2,   60.0, svif::L,   "fe");

    mip->loadCof(2, 1, 0.15);
    mip->loadCof(2, 2, 0.04);
    mip->loadCof(2, 3, 0.02);
    mip->loadCof(2, 4, 0.04);
    mip->loadCof(2, 5, 0.02);
    mip->loadCof(2, 6, 0.01);
    mip->loadCof(2, 7, 0.03);

    mip->loadRhs(3,  100.0, svif::L,   "cu");

    mip->loadCof(3, 1, 0.03);
    mip->loadCof(3, 2, 0.05);
    mip->loadCof(3, 3, 0.08);
    mip->loadCof(3, 4, 0.02);
    mip->loadCof(3, 5, 0.06);
    mip->loadCof(3, 6, 0.01);
    mip->loadCof(3, 7, 0.0);

    mip->loadRhs(4,   40.0, svif::L,   "mn");

    mip->loadCof(4, 1, 0.02);
    mip->loadCof(4, 2, 0.04);
    mip->loadCof(4, 3, 0.01);
    mip->loadCof(4, 4, 0.02);
    mip->loadCof(4, 5, 0.02);
    mip->loadCof(4, 6, 0.0);
    mip->loadCof(4, 7, 0.0);

    mip->loadRhs(5,   30.0, svif::L,   "mg");

    mip->loadCof(5, 1, 0.02);
    mip->loadCof(5, 2, 0.03);
    mip->loadCof(5, 3, 0.0);
    mip->loadCof(5, 4, 0.0);
    mip->loadCof(5, 5, 0.01);
    mip->loadCof(5, 6, 0.0);
    mip->loadCof(5, 7, 0.0);

    mip->loadRhs(6, 1500.0, svif::G,   "al");

    mip->loadCof(6, 1, 0.70);
    mip->loadCof(6, 2, 0.75);
    mip->loadCof(6, 3, 0.80);
    mip->loadCof(6, 4, 0.75);
    mip->loadCof(6, 5, 0.80);
    mip->loadCof(6, 6, 0.97);
    mip->loadCof(6, 7, 0.0);

    mip->loadRhs(7,  250.0, svif::G,   "si1");

    mip->loadCof(7, 1, 0.02);
    mip->loadCof(7, 2, 0.06);
    mip->loadCof(7, 3, 0.08);
    mip->loadCof(7, 4, 0.12);
    mip->loadCof(7, 5, 0.02);
    mip->loadCof(7, 6, 0.01);
    mip->loadCof(7, 7, 0.97);

    mip->loadRhs(8,  300.0, svif::L,   "si2");

    mip->loadCof(8, 1, 0.02);
    mip->loadCof(8, 2, 0.06);
    mip->loadCof(8, 3, 0.08);
    mip->loadCof(8, 4, 0.12);
    mip->loadCof(8, 5, 0.02);
    mip->loadCof(8, 6, 0.01);
    mip->loadCof(8, 7, 0.97);

    // variable bounds

#if 1 // 0 = 'loadRhs' and 'loadCof', 1 = 'reviseBnds'

    mip->reviseBnds(1, 0.0,    200.0);
    mip->reviseBnds(2, 0.0,   2500.0);
    mip->reviseBnds(3, 400.0,  800.0);
    mip->reviseBnds(4, 100.0,  700.0);
    mip->reviseBnds(5, 0.0,   1500.0);

#else

    mip->loadRhs(9, 200.0, svif::L,    "bin-1-upper");
    mip->loadCof(9, 1, 1.0);

    mip->loadRhs(10, 2500.0, svif::L,  "bin-2-upper");
    mip->loadCof(10, 2, 1.0);

    mip->loadRhs(11, 800.0, svif::L,   "bin-3-upper");
    mip->loadCof(11, 3, 1.0);

    mip->loadRhs(12, 400.0, svif::G,   "bin-3-lower");
    mip->loadCof(12, 3, 1.0);

    mip->loadRhs(13, 700.0, svif::L,   "bin-4-upper");
    mip->loadCof(13, 4, 1.0);

    mip->loadRhs(14, 100.0, svif::G,   "bin-4-lower");
    mip->loadCof(14, 4, 1.0);

    mip->loadRhs(15, 1500.0, svif::L,  "bin-5-upper");
    mip->loadCof(15, 5, 1.0);

#endif // 0

    // MODIFY ORIGINAL PROBLEM

#if 0 // 1 = modify original problem, otherwise 0

    mip->markVarInteger(1);
    mip->markVarInteger(2);
    mip->markVarInteger(3);
    mip->markVarInteger(4);
    mip->markVarInteger(5);
    mip->markVarInteger(6);
    mip->markVarInteger(7);

#endif // 0

    //  SOLVE PROBLEM

    mip->runSolver();

    //  REPORT RESULTS

    put << mip->getFormattedResults("goal","var");
    logger->putx(logga::dbug, put);

    // display known results

    put << "\n"
        << "   KNOWN LP RESULTS\n"
        << "   Z  = 296.217\n"
        << "   x1 = 0\n"
        << "   x2 = 665.343\n"
        << "   x3 = 490.253\n"
        << "   x4 = 424.188\n"
        << "   x5 = 0\n"
        << "   x6 = 299.639\n"
        << "   x7 = 120.578\n";

    logger->putx(logga::dbug, put);

    //  VISUALIZE PROBLEM INSTANCE

    GlpkViz webbrowse;                       // take default webbrowser string
    webbrowse(mip, "plan lp model", 4, true);

  }

  // ---------------------------------
  //  HOUSEKEEPING
  // ---------------------------------

  logger->test(0);                           // test end

  logger->repx(logga::info, "end of main", "~~~~~~~~~~");
  return xeona::exit_success;

} // main function

//  end of file

