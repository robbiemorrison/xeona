//  XEONA ENERGY SYSTEMS MODELING ENVIRONMENT
//
//  file-create-name : label.ut0.cc
//  file-create-date : Fri 24-Oct-2008 12:18 UTC
//  file-initiator   : Robbie Morrison <robbie@actrix.co.nz>
//
//  file-role        : helper class for solver labels  / unit test
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
//  $URL: file:///home/robbie/svn-root/xeona/futz/trunk/xeona1/c/label.ut0.cc $
//
//  GENERAL NOTES FOR THIS FILE
//
//  This is a unit test file and hence not essential to the
//  application proper.

//  LOCAL AND SYSTEM INCLUDES

#include "label.h"            // unit under test (place early)

#include "../a/logger.h"      // standard logging functionality (as required)
#include "../c/smart_ptr.h"   // toggle between Boost and TR1 smart pointers
#include ".././common.h"      // common definitions for project (place last)

#include <string>             // C++ strings
#include <sstream>            // string-streams

#include <boost/format.hpp>   // printf style formatting

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

  // CAUTION: chaining is not supported: lab1 << "first" << "second";

  // ---------------------------------------------------------
  //  test ONE        : empty label
  // ---------------------------------------------------------

  logger->test(1, "empty label");

  {
    Label lab1;

    put << "  lab1 (as string) : " << lab1.str() << "\n"
        << "  lab1 (streaming) : " << lab1       << "\n";
    logger->putx(logga::dbug, put);
  }

  // ---------------------------------------------------------
  //  test TWO        : full label
  // ---------------------------------------------------------

  logger->test(2, "full label");

  {
    Label lab2("yes");

    int a = 4;
    lab2 << "hello";
    lab2 << "";                              // skip input
    lab2 << "world";
    lab2 << "int";
    lab2 << a;
    lab2 << "bigint";
    lab2 << std::numeric_limits<int>::max(); // 2147483647
    lab2 << "double";
    lab2 << 1234.5678e+34;
    lab2 << "truth";
    lab2 << true;                            // 1

    put << "  lab2 (as string) : " << lab2.str() << "\n"
        << "  lab2 (streaming) : " << lab2       << "\n";
    logger->putx(logga::dbug, put);
  }

  // ---------------------------------------------------------
  //  test THREE      : use of Boost.Format
  // ---------------------------------------------------------

  logger->test(2, "use of Boost.Format");

  {
    Label lab3("boost.format");
    lab3 << boost::format("%.1f") % 12.3456789;   // limited to one decimal place

    put << "  lab3 (as string) : " << lab3.str() << "\n"
        << "  lab3 (streaming) : " << lab3       << "\n";
    logger->putx(logga::dbug, put);
  }

  // ---------------------------------------------------------
  //  test FOUR       : use of str("extra")
  // ---------------------------------------------------------

  logger->test(4, "use of str(\"extra\") and add()");

  {
    Label lab4("");
    lab4 << "next";
    lab4 << "is";
    put << "  lab4 (as string(\"extra\"))   : " << lab4.str("extra") << "\n"
        << "  lab4 (as string()           : "   << lab4.str()        << "\n";
    lab4.trim(1);
    put << "  lab4 (trimmed one)          : "   << lab4.str()        << "\n";
    lab4.add("add");
    put << "  lab4 (added)                : "   << lab4.str()        << "\n";
    logger->putx(logga::dbug, put);
  }

  // ---------------------------------------------------------
  //  test FIVE       : integer padding
  // ---------------------------------------------------------

  logger->test(5, "integer padding");

  {
    const int four = 4;
    Label lab5("");
    lab5 << "integer-pad-four";
    put << "  lab5 (padstr call)          : " << lab5.padstr(four, 4) << "\n";
    lab5.resetToCtor();
    const unsigned five = 5;
    lab5 << "integer-pad-stream";
    lab5 << five;
    put << "  lab5 (stream variant)       : " << lab5                 << "\n";
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

