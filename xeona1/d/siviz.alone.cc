//  XEONA ENERGY SYSTEMS MODELING ENVIRONMENT
//
//  file-create-name : siviz.alone.cc
//  file-create-date : Wed 18-Jun-2008 21:20 UTC
//  file-initiator   : Robbie Morrison <robbie@actrix.co.nz>
//
//  file-role        : auxiliary testing of 'GlpkViz' and 'SolverIf' / stand-alone
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
//  $URL: file:///home/robbie/svn-root/xeona/futz/trunk/xeona1/d/siviz.alone.cc $
//
//  GENERAL NOTES FOR THIS FILE
//
//  This file is not an essential part of 'xeona', but can be
//  used to test 'GlpkViz' in a stand-alone context.
//
//  Invoke this unit test via: 'glpkviz.alone.sh'.
//
//  This particular bash script, in turn, invokes the generic
//  'makefile' with the correct settings.

//  LOCAL AND SYSTEM INCLUDES

#include "glpkviz.h"          // unit under test (place early)
#include "../d/siglp.h"       // GLPK solver interface in 'svif' namespace (current APIs)
#include "../d/siglpx.h"      // extended support for GLPK solver interface
#include "../a/logger.h"      // standard logging functionality (as required)
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
  logger->setReportLevel(logga::xtra);       // maximum reporting

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

    sample->initUseSimpPresolver();
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

    // extension to make milp, overrides any interior point
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

#if 0 // 02-Feb-2009: test 2 disabled, although previous status not known

  // ---------------------------------------------------------
  //  test TWO        : triangle diagram
  // ---------------------------------------------------------

  logger->test(2);

  {
    put << "triangle diagram" << "\n";
    logger->putx(logga::dbug, put);

    // VISUALIZER

    GlpkViz webbrowse("firefox");

    // INITIALIZATION

    shared_ptr<svif::SolverIf> mip
      (new svif::SolverIf
       ("triangle-diagram",                  // mandatory
        svif::high));                        // defaults to level chosen by _XDEBUG

    // optional initialization calls

    mip->initUseSimpPresolver();

    //  BUILD PROBLEM

    // set objective sense

    mip->setObjectiveSense(svif::minimize, "lmp-objective");

    // generator 1

    mip->loadObj(     1,   0.0, "gen1-lo");                      // low bid
    mip->loadObj(     2,   2.0, "gen1-mi");                      // middle bid
    mip->loadObj(     3,   3.0, "gen1-hi");                      // high bid
    mip->loadObj(     4,   0.0, "gen1-out");

    mip->loadRhs( 1,       0.0, svif::G, "gen1-lo-lower");       // low bid lower bound
    mip->loadCof( 1,  1,   1.0);
    mip->loadRhs( 2,     100.0, svif::L, "gen1-lo-upper");       // low bid upper bound
    mip->loadCof( 2,  1,   1.0);

    mip->loadRhs( 3,     100.0, svif::G, "gen1-mi-lower");       // middle bid lower bound
    mip->loadCof( 3,  2,   1.0);
    mip->loadRhs( 4,     200.0, svif::L, "gen1-mi-upper");       // middle bid upper bound
    mip->loadCof( 4,  2,   1.0);

    mip->loadRhs( 5,     200.0, svif::G, "gen1-hi-lower");       // upper bid lower bound
    mip->loadCof( 5,  3,   1.0);
    mip->loadRhs( 6,     300.0, svif::L, "gen1-hi-upper");       // upper bid upper bound
    mip->loadCof( 6,  3,   1.0);

    mip->loadRhs( 7,       0.0, svif::E, "gen1-out-bal");
    mip->loadCof( 7,  1,   1.0);
    mip->loadCof( 7,  2,   1.0);
    mip->loadCof( 7,  3,   1.0);
    mip->loadCof( 7,  4,  -1.0);

    // generator 2

    mip->loadObj(     5,   0.0, "gen2-lo");                      // low bid
    mip->loadObj(     6,   2.0, "gen2-mi");                      // middle bid
    mip->loadObj(     7,   3.0, "gen2-hi");                      // high bid
    mip->loadObj(     8,   0.0, "gen2-out");

    mip->loadRhs( 8,       0.0, svif::G, "gen2-lo-lower");       // low bid lower bound
    mip->loadCof( 8,  5,   1.0);
    mip->loadRhs( 9,     100.0, svif::L, "gen2-lo-upper");       // low bid upper bound
    mip->loadCof( 9,  5,   1.0);

    mip->loadRhs(10,     100.0, svif::G, "gen2-mi-lower");       // middle bid lower bound
    mip->loadCof(10,  6,   1.0);
    mip->loadRhs(11,     200.0, svif::L, "gen2-mi-upper");       // middle bid upper bound
    mip->loadCof(11,  6,   1.0);

    mip->loadRhs(12,     200.0, svif::G, "gen2-hi-lower");       // upper bid lower bound
    mip->loadCof(12,  7,   1.0);
    mip->loadRhs(13,     300.0, svif::L, "gen2-hi-upper");       // upper bid upper bound
    mip->loadCof(13,  7,   1.0);

    mip->loadRhs(14,       0.0, svif::E, "gen2-out-bal");
    mip->loadCof(14,  5,   1.0);
    mip->loadCof(14,  6,   1.0);
    mip->loadCof(14,  7,   1.0);
    mip->loadCof(14,  8,  -1.0);

    // load 1

    mip->loadObj(     9,   0.0, "load1-fixed");                  // fixed load

    mip->loadRhs(15,     350.0, svif::E, "load1-demand");        // demand
    mip->loadCof(15,  9,   1.0);

    // line 1

    mip->loadObj(    10,   0.0, "load1-fwd-loss");
    mip->loadObj(    10,   0.0, "load1-fwd-loss");
    mip->loadObj(    10,   0.0, "load1-fwd-loss");

//      // connect
//
//      mip->loadRhs(14,       0.0, svif::E, "node-a");
//      mip->loadCof(14,  1,   1.0);
//      mip->loadCof(14,  2,   1.0);
//      mip->loadCof(14,  3,   1.0);
//
//      mip->loadCof(14,  7,  -1.0);
//      mip->loadCof(14,  3,  -1.0);

    //  VISUALIZE PROBLEM INSTANCE

    logger->repx(logga::dbug, "about to call web browser", "");
    webbrowse(mip, "before solver call", 4, true);

    //  SOLVE PROBLEM

    mip->runSolver();

    //  REPORT RESULTS

    // check first for alerts

    int alertCount = mip->getAlertCount();

    put << "   alert count : "
        << alertCount
        << "\n";

    logger->putx(logga::dbug, put);
    logger->addSmartBlank();

    // obtain results

    put << mip->getFormattedResults();
    logger->putx(logga::dbug, put);

    //  VISUALIZE PROBLEM INSTANCE

    webbrowse(mip, "after solver call", 4, true);

    // HOUSEKEEPING

    // free memory -- although the only instance is about to go
    // out of scope in any case

    mip.reset()  ;                           // redundant in the circumstances

  }

#endif // 0

  // HOUSEKEEPING

  logger->test(0);                           // test end

  logger->repx(logga::info, "end of main", "~~~~~~~~~~");
  return xeona::exit_success;

} // main function

//  end of file

