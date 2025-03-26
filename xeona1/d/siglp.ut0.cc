//  XEONA ENERGY SYSTEMS MODELING ENVIRONMENT
//
//  file-create-name : siglp.ut0.cc
//  file-create-date : Tue 22-Apr-2008 14:37 UTC
//  file-initiator   : Robbie Morrison <robbie@actrix.co.nz>
//
//  file-role        : semi-intelligent interface to GLPK MILP solver / unit test
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
//  $URL: file:///home/robbie/svn-root/xeona/futz/trunk/xeona1/d/siglp.ut0.cc $
//
//  GENERAL NOTES FOR THIS FILE
//
//  This is a unit test file and hence not essential to the
//  application proper.
//
//  This file contains small examples from the GLPK API manual
//  and "Numerical Recipes in C".  Further details are contained
//  within the code.
//
//  It would also be useful to read the overarching notes section
//  in "siglp.h".

//  LOCAL AND SYSTEM INCLUDES

#include "siglp.h"            // unit under test (place early)

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
  logger->repx(logga::dbug, "xeona::DBUG (set via _XDEBUG)", xeona::DBUG);
  logger->repx(logga::dbug, "svif::DBUG  (set via _XDEBUG)", svif::DBUG);
  logger->addSmartBlank();
  logger->setReportLevel(logga::adhc);       // maximum reporting

  // PRELIMINARY

  xeona::yeek = 1;                           // maximum yeeking
  std::ostringstream put;                    // used for logging to console

  // ---------------------------------------------------------
  //  test ONE        : GLPK version, stdout redirect
  // ---------------------------------------------------------

  logger->test(1, "GLPK version");

  const std::string glpkver = svif::SolverIf::getGlpkVersion();
  put << "glpk version : " << glpkver << "\n";
  logger->putx(logga::dbug, put);

#if 0 // 0 = ignore, 1 = redirect

  logger->addSmartBlank();
  svif::SolverIf::stdoutRedirect();
  svif::SolverIf::stdoutRedirect("stdout.txt");

#endif // 0

  // ---------------------------------------------------------
  //  test TWO        : basic object construction
  // ---------------------------------------------------------

  logger->test(2, "basic object construction");

  {
    put << "creating basic SI objects" << "\n";
    logger->putx(logga::dbug, put);

    // raw pointer version

    const std::string tag1 = "test-2";
    svif::SolverIf* mip1 = new svif::SolverIf(tag1);

    put << mip1->reportGlpkProblem();
    logger->putx(logga::dbug, put);

    delete mip1;                             // explicitly release heap memory

    logger->addSmartBlank();

    // smart pointer version

    shared_ptr<svif::SolverIf> mip2(new svif::SolverIf(""));
    mip2.reset();                            // nuke smart pointer
  }

  // ---------------------------------------------------------
  //  test THREE      : default setting and problem building
  // ---------------------------------------------------------

  logger->test(3, "default setting and problem building");

  {
    put << "testing non-default construction and some initialization calls" << "\n";
    logger->putx(logga::dbug, put);

    shared_ptr<svif::SolverIf> mip3(new svif::SolverIf("", svif::high));

    mip3->setProblemLabel("relabel-trial");  // maximum chars before screen truncation

    mip3->initSetPrefLPSolver(svif::solver_interior);
    mip3->initSimplexPresolver();

    mip3->setObjectiveSense(svif::minimize);

    const int rowStart = mip3->getConCount();
    put << "  rowStart ('getConCount' return, should be 0) : " << rowStart << "\n";
    logger->putx(logga::dbug, put);
    logger->addSmartBlank(logga::dbug);

    put << "test some problem load calls" << "\n";
    logger->putx(logga::dbug, put);

    mip3->loadObj(2, 0);                     // skipped 1, also zero-valued
    mip3->loadObj(1, 2.3, "second load");    // overwriting a previous (unset) col
  }

  // ---------------------------------------------------------
  //  test FOUR       : terminal blocking
  // ---------------------------------------------------------

  logger->test(4, "terminal blocking");

  put << "test terminal blocking" << "\n";
  logger->putx(logga::dbug, put);

  svif::SolverIf::outputSuspend();
  svif::SolverIf::outputResume();

#if 0 // 1 = suspend SolverIf native and GLPK output, 0 otherwise
  svif::SolverIf::outputSuspend();
#endif // 0

  // ---------------------------------------------------------
  //  test FIVE       : GLPK tutorial problem or MIP variant
  // ---------------------------------------------------------

  // ---------------------------------------------------------
  //   GLPK tutorial problem, pp12-13 (sample.c)
  //
  //    *  maximize
  //    *  eqn (1.1), the objective
  //    *  eqn (1.2), the system
  //    *  the primary constraint of non-negative structural vars
  //
  //    Makhorin, Andrew.  2007.  GNU linear programming kit :
  //      reference manual version 4.25 -- Draft edition
  //      (December 2007).  Free Software Foundation, Boston, MA,
  //      USA.  [author affiliation: Department for Applied
  //      Informatics, Moscow Aviation Institute, Moscow, Russia]
  //      (distributed with the GLPK source code as LaTeX, DVI,
  //      and PostScript)
  //
  // ---------------------------------------------------------

  logger->test(5, "GLPK tutorial problem or MIP variant");

  {
    put << "based on GLPK tutorial problem (p12-13)" << "\n";
    logger->putx(logga::dbug, put);

    // INITIALIZATION

    const std::string label = "glpk-tutorial";

    shared_ptr<svif::SolverIf> sample
      (new svif::SolverIf
       (label,                               // mandatory
        svif::high));                        // defaults to level chosen by _XDEBUG

    // optional initialization calls

    sample->initSimplexPresolver();
    sample->initSetPrefLPSolver(svif::solver_interior);
    sample->initKktReport(3);                // 3 is the maximum noise

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

    // extension to make MIP, overrides any interior point
    // preference (comment in or out)

    sample->markVarInteger(2);               // mark col as integer

    //  SOLVE PROBLEM

    sample->runSolver();

    //  REPORT RESULTS

    // check first for alerts

    put << "   alert count : "
        << sample->getAlertCount()
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

    // print info to file

    sample->writeInfo();                     // this variant uses the problem label

    //  SUPPLEMENTARY

    // supplementary constraint coefficient test
    const int    row  = 2;
    const int    col  = 1;
    const double gave = 10.0;                // see above
    const double orig = sample->getOrigCof(row, col);
    const double prob = sample->getProbCof(row, col);

    put << "  supplementary constraint coefficient test"        << "\n"
        << "    row and col  : " << row << " " << col           << "\n"
        << "    expected     : " << gave                        << "\n"
        << "    getOrigCof   : " << orig                        << "\n"
        << "    getProbCof   : " << prob                        << "\n";

    logger->putx(logga::dbug, put);

    if ( orig != prob )
      {
        logger->repx(logga::warn, "constraint coefficient test failed", "");
      }

    // supplementary range reporting

    double span = 0.0;
    sample->reportCofs(span, put);
    logger->putx(logga::dbug, put);

    // HOUSEKEEPING

    // free memory -- although the only instance is about to go
    // out of scope in any case

    sample.reset();                          // redundant in the circumstances

  }

  // ---------------------------------------------------------
  //  test SIX        : Numerical Recipes example or MIP variant
  // ---------------------------------------------------------

  // ---------------------------------------------------------
  //   Numerical Recipes LP example, pp431-432
  //
  //    *  maximize
  //    *  eqn (10.8.6), the objective
  //    *  eqn (10.8.7), the system
  //    *  the primary constraint of non-negative structural vars
  //
  //    Press, William H, Saul A Teukolsky, William T
  //      Vetterling, and Brian P Flannery.  1992.  Numerical
  //      recipes in C : the art of scientific computing --
  //      Second edition.  Cambridge University Press, Cambridge,
  //      UK.  ISBN 0-521-43108-5.
  //      http://lib-www.lanl.gov/numerical/bookcpdf.html
  //
  // ---------------------------------------------------------

  logger->test(6, "Numerical Recipes example or MIP variant");

  {
    put << "based on Numerical Recipes tutorial problem (pp431-432)" << "\n";
    logger->putx(logga::dbug, put);

    // INITIALIZATION

    shared_ptr<svif::SolverIf> recipes
      (new svif::SolverIf
       ("numerical-recipes-tutorial",        // mandatory
        svif::high));                        // defaults to level chosen by _XDEBUG

    // optional initialization calls

    recipes->initSimplexPresolver();
    recipes->initSetPrefLPSolver(svif::solver_interior);

    //  BUILD PROBLEM

    // set objective sense

    recipes->setObjectiveSense(svif::maximize, "10-8");

    // cols as (normally non-zero but can be negative) objective coefficients

    recipes->loadObj(1,  1.0, "1");
    recipes->loadObj(2,  1.0, "2");
    recipes->loadObj(3,  3.0, "3");
    recipes->loadObj(4, -0.5, "4");

    // rows as potentially negative RHS constraints

    recipes->loadRhs(1,  740.0, svif::L, "a");
    recipes->loadRhs(2,    0.0, svif::L, "b");
    recipes->loadRhs(3,    0.5, svif::G, "c");
    recipes->loadRhs(4,    9.0, svif::E, "d");

    // rows x cols (normally non-zero (but can be negative) structural coefficients

    recipes->loadCof(1, 1,  1.0);
    recipes->loadCof(1, 3,  2.0);

    recipes->loadCof(2, 2,  2.0);
    recipes->loadCof(2, 4, -7.0);

    recipes->loadCof(3, 2,  1.0);
    recipes->loadCof(3, 3, -1.0);
    recipes->loadCof(3, 4,  2.0);

    recipes->loadCof(4, 1,  1.0);
    recipes->loadCof(4, 2,  1.0);
    recipes->loadCof(4, 3,  1.0);
    recipes->loadCof(4, 4,  1.0);

    // extension to make MIP, overrides any interior point
    // preference (comment in or out)

    recipes->markVarInteger(2);              // mark col as integer

    //  SOLVE PROBLEM

    recipes->runSolver();

    //  REPORT RESULTS

    // check first for alerts

    int alertCount = recipes->getAlertCount();

    put << "   alert count : "
        << alertCount
        << "\n";

    logger->putx(logga::dbug, put);
    logger->addSmartBlank();

    // obtain results

    put << recipes->getFormattedResults();
    logger->putx(logga::dbug, put);

    // display known results

    put << "\n"
        << "   KNOWN LP RESULTS\n"
        << "   Z  = 17.03\n"
        << "   x1 =  0.0\n"
        << "   x2 =  3.33\n"
        << "   x3 =  4.73\n"
        << "   x4 =  0.95\n";

    logger->putx(logga::dbug, put);

    // print info to file

    recipes->writeInfo();                    // this variant uses the problem label

    // HOUSEKEEPING

    recipes.reset();                         // free memory

  }

  // ---------------------------------------------------------
  //  test SEVEN      : tiny problem
  // ---------------------------------------------------------

  logger->test(7, "tiny problem");

  {
    put << "tiny problem" << "\n";
    logger->putx(logga::dbug, put);

    // INITIALIZATION

    svif::SolverIf* tiny =
      new svif::SolverIf
      ("tiny-problem",                       // mandatory
       svif::high);                          // defaults to level chosen by _XDEBUG

    //  BUILD PROBLEM (see above for commentary)

    tiny->setObjectiveSense(svif::maximize, "goal");
    tiny->loadObj(1,   1.5,          "1");
    tiny->loadRhs(1, 210.5, svif::L, "a");
    tiny->loadCof(1, 1, 3.5);
    tiny->markVarBinary(1);

    //  SOLVE PROBLEM

    tiny->runSolver();

    //  REPORT RESULTS

    put << tiny->getFormattedResults("goal","var");
    logger->putx(logga::dbug, put);

    // display known results

    put << "\n"
        << "   KNOWN LP RESULTS\n"
        << "   Z  = 90.2143\n"
        << "   x1 = 60.1429\n";

    logger->putx(logga::dbug, put);

    delete tiny;                             // free memory

  }

  // ---------------------------------------------------------
  //  test EIGHT      : plan.lp problem
  // ---------------------------------------------------------

  logger->test(8, "plan.lp problem");

  {
    put << "plan.lp problem" << "\n";
    logger->putx(logga::dbug, put);

    // INITIALIZATION

    svif::SolverIf* mip =
      new svif::SolverIf
      ("",                                   // mandatory
       svif::high);                          // defaults to level chosen by _XDEBUG

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

    //  FURTHER TESTS

    mip->resetGlpkProb();
    mip->recreateProblem("new tag");

    //  HOUSEKEEPING

    delete mip;                             // free memory -- essential

  }

  // ---------------------------------------------------------
  //  test NINE       : obtain glp_prob pointer
  // ---------------------------------------------------------

  logger->test(9, "obtain glp_prob pointer");

  {
    put << "obtain glp_prob pointer" << "\n";
    logger->putx(logga::dbug, put);

    const std::string tag = "obtain-glp-prob";
    svif::SolverIf* mip   = new svif::SolverIf(tag);

    glp_prob* ptr         = mip->obtainGlpProbPtr();
    const int direction   = glp_get_obj_dir(ptr);

    put << "problem direction (direct) : " << direction << "\n";
    logger->putx(logga::dbug, put);

    ptr = NULL;                              // good practice
    delete mip;                              // explicitly release heap memory

    put << "obtained glp_prob pointer address (should be zero) : " << ptr << "\n";
    logger->putx(logga::dbug, put);

  }

  // ---------------------------------
  //  HOUSEKEEPING
  // ---------------------------------

  logger->test(0);

  logger->repx(logga::info, "end of main", "~~~~~~~~~~");
  return xeona::exit_success;

} // main function

//  end of file

