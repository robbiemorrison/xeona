//  XEONA ENERGY SYSTEMS MODELING ENVIRONMENT
//
//  file-create-name : util2.ut0.cc
//  file-create-date : Thu 06-Nov-2008 16:53 UTC
//  file-initiator   : Robbie Morrison <robbie@actrix.co.nz>
//
//  file-role        : free functions which offer general utilities 2 / unit test
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
//  $URL: file:///home/robbie/svn-root/xeona/futz/trunk/xeona1/c/util2.ut0.cc $
//
//  GENERAL NOTES FOR THIS FILE
//
//  This is a unit test file and hence not essential to the
//  application proper.

//  LOCAL AND SYSTEM INCLUDES

#include "util2.h"            // unit under test (place early)

#include "../c/util1.h"       // free functions which offer general utilities 1

#include "../a/logger.h"      // standard logging functionality (as required)
#include "../c/smart_ptr.h"   // toggle between Boost and TR1 smart pointers
#include ".././common.h"      // common definitions for project (place last)

#include <iomanip>            // setw() and family
#include <string>             // C++ strings
#include <sstream>            // string-streams
#include <utility>            // STL pair, make_pair()

#include <boost/foreach.hpp>  // BOOST_FOREACH iteration macro

//  CODE

// ---------------------------------------------------------
//  CLASS           : UnitTestClass
// ---------------------------------------------------------

class UnitTestClass
{
private:
  UnitTestClass();
public:
  UnitTestClass(const std::string& label) : d_label(label) { }
  std::string label() { return d_label; }
private:
  const std::string d_label;
};

// ---------------------------------------------------------
//  FREE FUNCTION   : ::sayData
// ---------------------------------------------------------

namespace
{
  void
  sayData
  (const std::string&                             msg,
   const std::vector<shared_ptr<UnitTestClass> >& data,
   std::ostream&                                  os)
  {
    os << "  " << msg << " :";
    BOOST_FOREACH( shared_ptr<UnitTestClass> my, data )
      {
        os << " " << my->label();
      }
    os << "\n";
  }
} // unnamed namespace

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

  xeona::yeek = 1;                           // maximum yeeking
  std::ostringstream put;

  // ---------------------------------------------------------
  //  test ONE        : function isTwoContained
  // ---------------------------------------------------------

  logger->test(1, "function isTwoContained");

  {
    xeona::yeek = 10;                        // additional reporting

    const int cand =  6;                     // unity or greater
    const int aggr = 30;                     // zero or greater

    put << "\n";

    put << std::boolalpha;
    put << "  return : " << xeona::isTwoContained(cand, aggr) << "\n";
    logger->putx(logga::dbug, put);
  }

  // ---------------------------------------------------------
  //  test TWO        : function reducedVector
  // ---------------------------------------------------------

  logger->test(2, "function reducedVector");

  {
    const int agg1 = 30;                               // simple
    put << "  aggregate      : " << agg1 << "\n";
    put << "  reduced vector : " << xeona::reducedVector(agg1) << "\n";
    logger->putx(logga::dbug, put);

    const int agg2 = static_cast<int>(std::pow(2.0, 15)); // 2**15 = xeona::e_maxAggregate
    put << "  aggregate      : " << agg2 << "\n";
    put << "  reduced vector : " << xeona::reducedVector(agg2, "-") << "\n";
    logger->putx(logga::dbug, put);
  }

  // ---------------------------------------------------------
  //  test THREE      : SmartPtrPopper test
  // ---------------------------------------------------------

  logger->test(3, "SmartPtrPopper test");

  {
    std::vector<shared_ptr<int> > siv;
    siv.push_back(shared_ptr<int>(new int(1)));
    siv.push_back(shared_ptr<int>(new int(2)));
    siv.push_back(shared_ptr<int>(new int(3)));
    siv.push_back(shared_ptr<int>(new int(4)));

    SmartPtrPopper<int> spp;
    spp.load(siv);

    put << "  spp :";
    while ( ! spp.empty() )
      {
        put << " " << *spp.pop();
      }
    put << "\n";

    put << "  remaining item is ";
    if ( spp.pop() )  put << "not empty or null" << "\n";
    else              put << "empty or null"     << "\n";

    put << "  address : " << spp.pop() << "\n";

    spp.load(siv);                           // reload
    shared_ptr<int> in;
    if ( (in = spp.pop()) )                  // the "=" is correct
      {
        put << "  in : " << *in << "\n";
      }

    logger->putx(logga::dbug, put);
  }

  // ---------------------------------------------------------
  //  test FOUR       : coefficient product
  // ---------------------------------------------------------

  logger->test(4, "coefficient product test");

  {
    std::vector<double> coeffs;
    std::vector<double> values;

    coeffs.push_back(1.1);
    values.push_back(0.0);
    coeffs.push_back(2.2);
    values.push_back(1.0);
    coeffs.push_back(3.3);
    values.push_back(2.0);

    const double result = xeona::coeffProduct(coeffs, values);
    put << "  result (9.9) : " << result << "\n";
    logger->putx(logga::dbug, put);
  }

  // ---------------------------------------------------------
  //  test FIVE       : isNan and isInf and isNotFinite
  // ---------------------------------------------------------

  logger->test(5, "isNan and isInf and isNotFinite tests");

  {
    // the negative makes no difference
    const double nan = -std::numeric_limits<double>::quiet_NaN();
    put << "  " << "nan    : " << nan;
    if ( xeona::isNan(nan) ) put << " is an IEEE 754 NaN"             << "\n";
    else                     put << " is not an IEEE 754 NaN (fail)"  << "\n";
    logger->putx(logga::dbug, put);

    // the negative makes a difference but the function ignores this
    const double inf = -std::numeric_limits<double>::infinity();
    put << "  " << "inf    : " << inf;
    if ( xeona::isInf(inf) ) put << " is an IEEE 754 inf"             << "\n";
    else                     put << " is not an IEEF 754 inf (fail)"  << "\n";
    logger->putx(logga::dbug, put);

    // the negative makes no difference
    const double finite = -12345.6789;
    put << "  " << "finite : " << finite;
    if ( xeona::isNotFinite(finite) ) put << " is not finite (fail)"  << "\n";
    else put << " is neither an IEEF 754 NaN nor an inf"              << "\n";
    logger->putx(logga::dbug, put);
  }

  // ---------------------------------------------------------
  //  test SIX        : is integer valued
  // ---------------------------------------------------------

  logger->test(6, "is integer valued");

  {
    const double five = 5.0;
    const double half = 0.5;
    put << "  try   " <<  five << " : " << xeona::isIntegerValued(five) << "\n";
    put << "  try "   <<  half << " : " << xeona::isIntegerValued(half) << "\n";
    logger->putx(logga::dbug, put);
  }

  // ---------------------------------------------------------
  //  test SEVEN      : geometric progression
  // ---------------------------------------------------------

  logger->test(7, "geometric progression");

  {
    typedef std::pair<int, std::string> test_type;     // CAUTION: needed 'BOOST_FOREACH'
    std::vector<test_type> tests;

    tests.push_back(std::make_pair(43, "1 2 0 8 0 32"));    // within 64
    tests.push_back(std::make_pair(32, "0 0 0 0 0 32"));    // within 32
    tests.push_back(std::make_pair( 1, "1"));
    tests.push_back(std::make_pair( 0, ""));

    BOOST_FOREACH( test_type t, tests )      // cycle thru tests
      {
        // define and fill the 'reds'
        const int         domainModes = t.first;
        const std::string expected    = t.second;
        std::vector<int> reds = xeona::geometricProgression(domainModes);

        // report details
        put << "    domain modes     : " << domainModes    << "\n"
            << "    reds vector size : " << reds.size()    << "\n";
        put << "    reds elements    : ";
        xeona::vectorReport(reds, " ", put); // fill stream
        put << "    expected         : " << expected << "\n";
        logger->addSmartBlank(logga::dbug);
        logger->putx(logga::dbug, put);
      }
  }

  // ---------------------------------------------------------
  //  test EIGHT      : odd or even test
  // ---------------------------------------------------------

  logger->test(8, "odd or even test");

  {
    // load tests
    std::vector<int> tests;
    tests.push_back(        0);
    tests.push_back(        1.1);
    tests.push_back(        2);
    tests.push_back(       -3);
    tests.push_back(   398330);
    tests.push_back(  -398331);
    tests.push_back(398330944);

    // trial and report
    put << std::boolalpha;
    BOOST_FOREACH( int i, tests )
      {
        put << "    " << std::setw(10) << i
            << "  " << (xeona::isEven(i) ? "even" : "odd") << "\n";
      }
    logger->putx(logga::dbug, put);
  }

  // ---------------------------------------------------------
  //  test NINE       : reorder upwards
  // ---------------------------------------------------------

  logger->test(9, "reorder upwards");

  {
    std::vector<shared_ptr<UnitTestClass> > myObs;

    shared_ptr<UnitTestClass> A(new UnitTestClass("A"));
    shared_ptr<UnitTestClass> B(new UnitTestClass("B"));
    shared_ptr<UnitTestClass> C(new UnitTestClass("C"));
    shared_ptr<UnitTestClass> D(new UnitTestClass("D"));

    myObs.push_back(A);
    myObs.push_back(B);
    myObs.push_back(C);
    myObs.push_back(D);

    std::vector<double> ordering;

    ordering.push_back(2.2);
    ordering.push_back(4.4);
    ordering.push_back(1.1);
    ordering.push_back(6.6);

    put << "  " << "ordering :";
    BOOST_FOREACH( double d, ordering )
      put << " " << d;
    put << "\n";

    logger->putx(logga::dbug, put);

    sayData("original", myObs, put);
    xeona::reorderUpwards(ordering, myObs);
    sayData("reorder ", myObs, put);
    std::reverse(myObs.begin(), myObs.end());
    sayData("reverse ", myObs, put);

    logger->putx(logga::dbug, put);
  }

  // ---------------------------------------------------------
  //  test TEN        : isNan and isInf and isNotFinite vectors
  // ---------------------------------------------------------

  logger->test(10, "isNan and isInf and isNotFinite tests for vectors");

  {
    std::vector<double> test;
    test.push_back(-1.1);
    test.push_back(+0.0);
    test.push_back(+1.1);
    test.push_back(+2.2);
    test.push_back(+3.3);

    // the negative makes no difference
    const double nan = -std::numeric_limits<double>::quiet_NaN();
    test.push_back(nan);
    test.push_back(+5.5);
    put << "  " << "nan    : " << std::setw(10) << nan;
    put << "  " << ": vector ";
    if ( xeona::isNanVec(test) ) put << "contains IEEE 754 NaN"                   << "\n";
    else                         put << "does not contain IEEE 754 NaN (fail)"    << "\n";
    logger->putx(logga::dbug, put);
    test.pop_back();
    test.pop_back();

    // the negative makes a difference but the function ignores this
    const double inf = -std::numeric_limits<double>::infinity();
    test.push_back(inf);
    test.push_back(+5.5);
    put << "  " << "inf    : " << std::setw(10) << inf;
    put << "  " << ": vector ";
    if ( xeona::isInfVec(test) ) put << "contains IEEE 754 inf"                   << "\n";
    else                         put << "does not contain IEEF 754 inf (fail)"    << "\n";
    logger->putx(logga::dbug,    put);
    test.pop_back();
    test.pop_back();

    // the negative makes no difference
    const double finite = -12345.6789;
    test.push_back(finite);
    test.push_back(+5.5);
    put << "  " << "finite : " << std::setw(10) << finite;
    put << "  " << ": vector ";
    if ( ! xeona::isNotFiniteVec(test) ) put << "is finite"                       << "\n";
    else                                 put << "contains IEEE 754 NaN/inf (fail)"<< "\n";
    test.pop_back();
    test.pop_back();

    // report
    logger->putx(logga::dbug, put);

  }

  // ---------------------------------------------------------
  //  test ELEVEN     : NormalPopper test
  // ---------------------------------------------------------

  logger->test(11, "NormalPopper test");

  {
    std::vector<int> iv;
    iv.push_back(1);
    iv.push_back(2);
    iv.push_back(3);
    iv.push_back(4);

    NormalPopper<int> pp;
    pp.load(iv);

    put << "  pp : ";
    while ( ! pp.empty() )
      {
        put << " " << pp.pop();
      }
    put << "\n";
    logger->putx(logga::dbug, put);

    try
      {
        logger->flush();
        std::cout << "  value (default constructed) : " << pp.pop() << std::endl;
      }
    catch ( const std::out_of_range& e )
      {
        put << "  just caught std::out_of_range exception: " << e.what() << "\n";
        logger->putx(logga::dbug, put);
      }

    pp.load(iv);                               // reload
    int in;
    if ( (in = pp.pop()) )
      {
        put << "  in : " << in << "\n";
        logger->putx(logga::dbug, put);
      }
  }

  // ---------------------------------
  //  HOUSEKEEPING
  // ---------------------------------

  logger->test(0);                           // test end

  logger->repx(logga::info, "end of main", "~~~~~~~~~~");
  return xeona::exit_success;

} // main function

//  end of file

