//  XEONA ENERGY SYSTEMS MODELING ENVIRONMENT
//
//  file-create-name : util3.ut0.cc
//  file-create-date : Thu 31-Dec-2009 00:18 UTC
//  file-initiator   : Robbie Morrison <robbie@actrix.co.nz>
//
//  file-role        : free functions for floating point comparison / unit test
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
//  $URL: file:///home/robbie/svn-root/xeona/futz/trunk/xeona1/c/util3.ut0.cc $
//
//  GENERAL NOTES FOR THIS FILE
//
//  This is a unit test file and hence not essential to the
//  application proper.

//  LOCAL AND SYSTEM INCLUDES

#include "util3.h"            // unit under test (place early)

#include "../a/logger.h"      // standard logging functionality (as required)
#include "../c/smart_ptr.h"   // toggle between Boost and TR1 smart pointers
#include ".././common.h"      // common definitions for project (place last)

#include <string>             // C++ strings
#include <sstream>            // string-streams

#include <limits>             // numeric_limits<T>::infinity() and similar

//  CODE

// ---------------------------------------------------------
//  CLASS           : Tester
// ---------------------------------------------------------
//  Description  : test 'xeona::almostEqual' and 'xeona::nearZero' and count errors
//  Role         : unit testing
//  Techniques   : overloaded member function, destructor reporting
//  Status       : complete
// ---------------------------------------------------------

class Tester
{
public:

  Tester() :
    d_callCount(0),
    d_callErrors(0),
    d_oss()
  { }

  ~Tester()
  {
    // destruction reporting
    d_oss << "  " << __func__ << " reporting"     << "\n"
          << "  calls     : " << d_callCount      << "\n"
          << "  errors    : " << d_callErrors     << "\n";
    s_logger->addSmartBlank(logga::dbug);
    s_logger->putx(logga::dbug, d_oss);
  }

  void
  call
  (const double           a,
   const double           b,
   const xeona::Precision precision,
   const bool             expectedResult)
  {
    s_logger->addSmartBlank(logga::dbug);
    const bool ret = xeona::almostEqual(a, b, precision);   // principle call
    if ( ret != expectedResult )
      {
        d_oss << std::boolalpha;
        d_oss << "  problem   : got " << ret << " and wanted " << expectedResult << "\n";
        s_logger->putx(logga::dbug, d_oss);
        ++d_callErrors;
      }
    ++d_callCount;
  }

  void
  call
  (const double           a,
   const xeona::Precision precision,
   const bool             expectedResult)
  {
    s_logger->addSmartBlank(logga::dbug);
    const bool ret = xeona::nearZero(a, precision);    // principle call
    if ( ret != expectedResult )
      {
        d_oss << std::boolalpha;
        d_oss << "  problem   : got " << ret << " and wanted " << expectedResult << "\n";
        s_logger->putx(logga::dbug, d_oss);
        ++d_callErrors;
      }
    ++d_callCount;
  }

private:

  int                       d_callCount;     // call count
  int                       d_callErrors;    // unexpected returns count
  std::ostringstream        d_oss;

  static logga::spLogger    s_logger;

};

// STATIC DEFINITIONS

logga::spLogger Tester::s_logger = logga::ptrLogStream();    // bind logger

// ---------------------------------------------------------
//  FREE FUNCTION   : ::testerRoundZero1
// ---------------------------------------------------------

namespace
{
  void
  testerRoundZero1
  (const double  test,
   std::ostream& os)
  {
    const xeona::Precision trip = xeona::zero8;
    double local = test;                     // remove const

    os << std::boolalpha;
    os << std::scientific << std::setprecision(20);
    os << "  ";
    os << std::left << std::setw(30) << local;
    os << std::left << std::setw(10) << xeona::roundZero(local, trip);
    os << local;
    os << "\n";

    std::cout << std::endl;                  // due to unit test reporting by 'roundZero'
  }
} // unnamed namespace

// ---------------------------------------------------------
//  FREE FUNCTION   : ::testerRoundZero2
// ---------------------------------------------------------

namespace
{
  void
  testerRoundZero2
  (const double  test,
   std::ostream& os)
  {
    double local = test;                     // remove const

    os << std::boolalpha;
    os << std::scientific << std::setprecision(20);
    os << "  ";
    os << std::left << std::setw(30) << local;
    os << std::left << std::setw(10) << xeona::roundZero(local, 1.0e-08);
    os << local;
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

  xeona::yeek = 1;
  std::ostringstream put;

  // ---------------------------------------------------------
  //  test ONE        : sizeof information
  // ---------------------------------------------------------

  logger->test(1, "sizeof information");

  {
    put << "  short       : " << sizeof(short)       << " bytes" << "\n"
        << "  int         : " << sizeof(int)         << " bytes" << "\n"
        << "  long        : " << sizeof(long)        << " bytes" << "\n";
//  put << "  long long   : " << sizeof(long long)   << " bytes" << "\n"; // [1]

    put << "\n"
        << "  float       : " << sizeof(float)       << " bytes" << "\n"
        << "  double      : " << sizeof(double)      << " bytes" << "\n"
        << "  long double : " << sizeof(long double) << " bytes" << "\n";

    logger->putx(logga::dbug, put);

    // [1] without GCC option '-Wno-long-long' the compiler will warn
    // under '-pedantic': ISO C++ 1998 does not support 'long long'
  }

  // ---------------------------------------------------------
  //  test TWO        : almost equal tests
  // ---------------------------------------------------------

  logger->test(2, "almost equal tests");

  {
    const double a = 1.000000000;
    const double b = 1.000000001;
    const double c = 1.00001;
    const double f = std::numeric_limits<double>::infinity();
    const double n = std::numeric_limits<double>::quiet_NaN();
    const double h = std::numeric_limits<double>::max();

    {
      class Tester tester;

      tester.call( a,  a, xeona::exact, true );
      tester.call( a,  b, xeona::numic, true );
      tester.call( a,  c, xeona::numic, false);
      tester.call( a,  c, xeona::loose, true );
      tester.call( a,  f, xeona::tight, false);
      tester.call( a,  n, xeona::tight, false);
      tester.call( a,  h, xeona::tight, false);
      tester.call( h,  h, xeona::exact, true );
      tester.call(-h,  h, xeona::tight, false);

    }                                        // destructor report on block exit
  }

  // ---------------------------------------------------------
  //  test THREE      : near zero tests
  // ---------------------------------------------------------

  logger->test(3, "near zero tests");

  {
    const double a = 0.000000000;
    const double b = 0.000000001;
    const double c = 0.00001;
    const double h = std::numeric_limits<double>::max();

    {
      class Tester tester;

      tester.call( a, xeona::exact, true );
      tester.call( a, xeona::numic, true );
      tester.call( a, xeona::loose, true );
      tester.call( b, xeona::tight, false);
      tester.call( c, xeona::tight, false);
      tester.call( h, xeona::exact, false );
      tester.call(-h, xeona::tight, false);

    }                                        // destructor report on block exit
  }

  // ---------------------------------------------------------
  //  test FOUR       : round zero test one
  // ---------------------------------------------------------

  logger->test(4, "round zero test one");

  {
    const double a = 0.0;
    const double b = 1.0e-22;
    const double c = 2.67028e-09;            // typical near-zero from the GLPK solver
    const double d = 0.00001;
    const double h = std::numeric_limits<double>::max();

    ::testerRoundZero1(a, put);
    ::testerRoundZero1(b, put);
    ::testerRoundZero1(c, put);
    ::testerRoundZero1(d, put);
    ::testerRoundZero1(h, put);

    logger->putx(logga::dbug, put);
  }

  // ---------------------------------------------------------
  //  test FIVE       : round zero test two
  // ---------------------------------------------------------

  logger->test(5, "round zero test two");

  {
    const double a = 0.0;
    const double b = 1.0e-22;
    const double c = 2.67028e-09;            // typical near-zero from the GLPK solver
    const double d = 0.00001;
    const double h = std::numeric_limits<double>::max();

    ::testerRoundZero2(a, put);
    ::testerRoundZero2(b, put);
    ::testerRoundZero2(c, put);
    ::testerRoundZero2(d, put);
    ::testerRoundZero2(h, put);

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

