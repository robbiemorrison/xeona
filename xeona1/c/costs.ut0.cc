//  XEONA ENERGY SYSTEMS MODELING ENVIRONMENT
//
//  file-create-name : costs.ut0.cc
//  file-create-date : Fri 17-Oct-2008 12:01 UTC
//  file-initiator   : Robbie Morrison <robbie@actrix.co.nz>
//
//  file-role        : cost sets and support / unit test
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
//  $URL: file:///home/robbie/svn-root/xeona/futz/trunk/xeona1/c/costs.ut0.cc $
//
//  GENERAL NOTES FOR THIS FILE
//
//  This is a unit test file and hence not essential to the
//  application proper.

//  LOCAL AND SYSTEM INCLUDES

#include "costs.h"            // unit under test (place early)

#include "../a/logger.h"      // standard logging functionality (as required)
#include "../c/smart_ptr.h"   // toggle between Boost and TR1 smart pointers
#include ".././common.h"      // common definitions for project (place last)

#include <limits>             // numeric_limits<T>::infinity() and similar
#include <string>             // C++ strings
#include <sstream>            // string-streams

//  CODE

// ---------------------------------------------------------
//  FREE FUNCTION   : ::pushObj
// ---------------------------------------------------------
//
//  A proxy for 'OptimSubProb::pushObj' member function.

namespace
{
  void
  pushObj
  (const CostSet& costSet,                   // 'pass-by-ref' NOT essential
   std::string    tag = "")                  // passed thru to solver
  {
    // switch statement here based on 'optimCT'
    double finVal = 0.0;
    finVal        = costSet.fin;             // recover value

    logga::spLogger logger = logga::ptrLogStream();
    std::ostringstream put;
    put << "  call to 'pushObj'"   << "\n"
        << "     tag : " << tag    << "\n"
        << "     fin : " << finVal << "\n";
    logger->putx(logga::dbug, put);
    logger->addSmartBlank(logga::dbug);

    // costSet.fin = 5.5;                    // CAUTION: cannot reset value here
  }

} // unnamed namespace

// ---------------------------------------------------------
//  MAIN FUNCTION   : main
// ---------------------------------------------------------

int
main(int argc, char* argv[])
{
  logga::spLogger logger = logga::ptrLogStream();      // main function logger
  logger->repx(logga::info, "beginning of main", "~~~~~~~~~~");
  logger->addSmartBlank();
  logger->setReportLevel(logga::adhc);       // maximum reporting

  // PRELIMINARY

  std::ostringstream put;

  double specFin = 1.1;
  double specGhg = 2.2;
  double specNox = 3.3;
  double specDep = 4.4;
  double specLuc = 5.5;

  {

    // ---------------------------------------------------------
    //  test ONE        : create some cost sets
    // ---------------------------------------------------------

    logger->test(1, "create some cost sets using class 'CostSet'");

    CostSet cs00;
    put << cs00.summarizeMeF("cs00 (f format)")   << "\n";
    put << cs00.summarizeMeG("cs00 (g format)")   << "\n";
    logger->putx(logga::dbug, put);
    logger->addSmartBlank(logga::dbug);

    CostSet cs01(specFin, specGhg, specNox, specDep, specLuc);
    put << cs01.summarizeMeF("cs01 (f format)")   << "\n";
    put << cs01.summarizeMeG("cs01 (g format)")   << "\n";
    logger->putx(logga::dbug, put);
    logger->addSmartBlank(logga::dbug);

    CostSet cs02(123);
    put << cs02.summarizeMeF("cs02 (123)")        << "\n";
    logger->putx(logga::dbug, put);

    // ---------------------------------------------------------
    //  test TWO        : manipulate elements
    // ---------------------------------------------------------

    logger->test(2, "manipulate elements");

    cs01.plusFin(20.0);
    put << cs01.summarizeMeF("cs01 (plusFin)")    << "\n";
    logger->putx(logga::dbug, put);

    cs01.fin += 20.0;
    put << cs01.summarizeMeF("cs01 (fin +=)")     << "\n";
    logger->putx(logga::dbug, put);

    // ---------------------------------------------------------
    //  test THREE      : arithmetic (double, tuple, cost set)
    // ---------------------------------------------------------

    logger->test(3, "arithmetic (double, tuple, cost set)");

    cs01 *= 4.0;
    put << cs01.summarizeMeF("cs01 (*= 4.0)")     << "\n";
    logger->putx(logga::dbug, put);

    CostSet cs03(1.0);
    put << cs03.summarizeMeF("cs03 (ctor 1.0)")   << "\n";
    tupleD5 tp1 = boost::make_tuple(-1.1, -2.2, -3.3, -4.4, -5.5);
    cs03 *= tp1;
    put << cs03.summarizeMeF("cs03 (*= tuple)")   << "\n";
    logger->putx(logga::dbug, put);

    CostSet cs04 = cs01 + cs03;
    put << cs04.summarizeMeF("cs04 (01 + 03)")    << "\n";
    logger->putx(logga::dbug, put);

    // ---------------------------------------------------------
    //  test FOUR       : tuple construction and export
    // ---------------------------------------------------------

    logger->test(4, "tuple construction and export");

    CostSet cs05(boost::make_tuple(10.1, 20.2, 30.3, 40.4, 50.5));
    put << cs05.summarizeMeF("cs05 (ctor)")       << "\n";
    logger->putx(logga::dbug, put);
    logger->addSmartBlank(logga::dbug);

    tupleD5 tp2 = cs05.tuple();
    put << "  tuple exported, first element : " << tp2.get<0>() << "\n";
    logger->putx(logga::dbug, put);

  }

  // ---------------------------------------------------------
  //  test FIVE       : pointer vector usage
  // ---------------------------------------------------------

  logger->test(5, "pointer vector usage");

  {
    std::vector<CostSet>                           costsets;
    shared_ptr<std::vector<CostSet> >              p_costsets;
    shared_ptr<std::vector<shared_ptr<CostSet> > > pp_costsets;

    std::vector<CostSet> data(9);
    data.at(1).fin = 11.11;
    put << "  data 1 fin : " << data.at(1).fin << "\n"
        << "  data 7 ghg : " << data.at(7).ghg << "\n";
    logger->putx(logga::dbug, put);
  }

  // ---------------------------------------------------------
  //  test SIX        : trial of ::pushObj
  // ---------------------------------------------------------

  logger->test(6, "trial of ::pushObj");

  {
    CostSet cs01(specFin, specGhg, specNox, specDep, specLuc);
    ::pushObj(cs01, "pushObj trial");
  }

  // ---------------------------------------------------------
  //  test SEVEN      : cost set streaming
  // ---------------------------------------------------------

  logger->test(7, "cost set streaming");

  {
    CostSet costset(1.1, 2.2, 3.3, 4.4, 5.5);
    put << "  streamed cost set : " << costset << "\n";
    logger->putx(logga::dbug, put);
  }

  // ---------------------------------------------------------
  //  test EIGHT      : unary minus operator
  // ---------------------------------------------------------

  logger->test(8, "unary minus operator");

  {
    CostSet costset(1.1, 2.2, 3.3, 4.4, 5.5);
    put << "  streamed negative cost set : " << -costset << "\n";
    logger->putx(logga::dbug, put);
  }

  // ---------------------------------------------------------
  //  test NINE       : tuple-based unary minus operator
  // ---------------------------------------------------------

  logger->test(9, "tuple-based unary minus operator");

  {
    CostSet cs1(0.0, 1.1, 2.2, 3.3, 4.4);
    CostSet cs2(5.5, 6.6, 7.7, 8.8, 9.9);
    boost::tuple<CostSet, CostSet> tup = boost::make_tuple(cs1, cs2);
    boost::tuple<CostSet, CostSet> nup = -tup;
  }

  // ---------------------------------------------------------
  //  test TEN        : is zero test
  // ---------------------------------------------------------

  logger->test(10, "is zero test (first true, second false)");

  {
    CostSet zero(0.0);
    CostSet  one(1.0);
    put << std::boolalpha
        << "  zero cost set is zero : " << zero.isZero() << "\n"
        << "   one cost set is zero : " << one.isZero()  << "\n"
        << std::noboolalpha;                 // reset
    logger->putx(logga::dbug, put);
  }

  // ---------------------------------------------------------
  //  test ELEVEN     : is near zero, is finite tests
  // ---------------------------------------------------------

  logger->test(11, "is near zero (true), is finite tests (false)");

  {
    CostSet near(0.000000001);
    CostSet inf(std::numeric_limits<double>::infinity());
    put << "\n";         // because the 'xeona::nearZero' are noisy under unit testing
    put << std::boolalpha
        << "  near cost set is near zero : " << near.isNearZero(xeona::zero6) << "\n"
        << "  inf cost set is not finite : " << inf.isFinite()                << "\n"
        << std::noboolalpha;                 // reset
    logger->putx(logga::dbug, put);
  }

  // ---------------------------------------------------------
  //  test TWELVE     : division by double test
  // ---------------------------------------------------------

  logger->test(12, "division by double test");

  {
    const double divisor1 = 1.0;
    put << "  " << "divisor : " << divisor1        << "\n";
    logger->putx(logga::dbug, put);
    CostSet cs12a(0.0);
    put << cs12a.summarizeMeG("cs12 (original)")   << "\n";
    logger->putx(logga::dbug, put);
    CostSet cs12b = cs12a / divisor1;
    put << cs12b.summarizeMeG("cs12 (divided) ")   << "\n";
    logger->putx(logga::dbug, put);

    put << "\n";
    logger->putx(logga::dbug, put);

    const double divisor2 = 9.0;
    put << "  " << "divisor : " << divisor2        << "\n";
    logger->putx(logga::dbug, put);
    CostSet cs12c(specFin, specGhg, specNox, specDep, specLuc);
    put << cs12c.summarizeMeG("cs12 (original)")   << "\n";
    logger->putx(logga::dbug, put);
    CostSet cs12d = cs12c / divisor2;
    put << cs12d.summarizeMeG("cs12 (divided) ")   << "\n";
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

