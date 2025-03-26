//  XEONA ENERGY SYSTEMS MODELING ENVIRONMENT
//
//  file-create-name : logger.test.cc
//  file-create-date : Tue 15-May-2007 08:41 UTC
//  file-initiator   : Robbie Morrison <robbie@actrix.co.nz>
//
//  file-role        : logging functionality / unit test
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
//  $URL: file:///home/robbie/svn-root/xeona/futz/trunk/xeona1/a/logger.ut0.cc $
//
//  GENERAL NOTES FOR THIS FILE
//
//  This is a unit test file and hence not essential to the
//  application proper.

//  LOCAL AND SYSTEM INCLUDES

#include "logger.h"           // unit under test (place early)

#include "../c/smart_ptr.h"   // switch easily between TR1 and Boost smart pointers
#include ".././common.h"      // common definitions for project (place last)

// see below for more hash-includes

//  CODE

// ---------------------------------------------------------
//  FILES           : short-and-long.{h,cc}
// ---------------------------------------------------------

#include "logger.h"           // standard logging functionality (as required)
#include ".././common.h"      // common definitions for project (place last)

// ---------------------------------------------------------
//  CLASS           : ShortButWith22LongName
// ---------------------------------------------------------

class ShortButWith22LongName
{
public:

  ShortButWith22LongName()
  {
    // OPTION 1 : short-lived approach, suitable for member and free functions

    logga::spLogger logger = logga::ptrLogStream(); // short-lived alias
    logger->setReportLevel(logga::adhc);
    logger->repx(logga::dbug, "constructor call", "short-lived logger");
  }

};

// ---------------------------------------------------------
//  CLASS           : Long
// ---------------------------------------------------------

class Long
{
public:

  // OPTION 2 : one per object long-lived approach, bound on construction

  Long(std::string id) :
    d_id(id),
    d_logger(logga::ptrLogStream())          // bind logger on construction
  {
    d_logger->setReportLevel(logga::adhc);
    d_logger->repx(logga::dbug, "constructor call", "long-lived logger");
    d_logger->repx(logga::info, "d_id", d_id);
    d_logger->repx(logga::info, "d_id (c-str'ed)", d_id.c_str());  // c_str() works

    // explicit specialization frustrates macro, see next
    d_logger->repx<std::string>(logga::yeek, "d_id (explicit specialization)", d_id);
    return;
  }

  ~Long()
  {
    d_logger->repx(logga::warn, "destructor call", "");
    return;
  }

private:

  std::string        d_id;                   // trial
  logga::spLogger    d_logger;               // long-lived shared_ptr to logger object

};

// ---------------------------------------------------------
//  CLASS           : Static
// ---------------------------------------------------------

class Static
{
public:

  Static()
  {
    s_logger->setReportLevel(logga::adhc);
    s_logger->repx(logga::info, "constructor call", "");
    return;
  }

  ~Static()
  {
    s_logger->repx(logga::dbug, "destructor call", "");
    return;
  }

private:

  static logga::spLogger s_logger;           // shared_ptr to single logger object

};

// OPTION 3 : one per class long-lived approach, bound on definition

logga::spLogger
Static::s_logger = logga::ptrLogStream();    // bind logger on definition

// ---------------------------------------------------------
//  FILE            : main.cc
// ---------------------------------------------------------

#include <iostream>           // standard io
#include <sstream>            // string-streams
#include <string>             // C++ strings

ShortButWith22LongName short1;               // global object with logging
Static static1;                              // global object with logging

logga::spLogger global = logga::ptrLogStream();

// ---------------------------------------------------------
//  MAIN FUNCTION   : main
// ---------------------------------------------------------

int
main(int argc, char* argv[])
{
  logga::spLogger logger = logga::ptrLogStream();
  global->repx(logga::info, "beginning of main", "~~~~~~~~~~");
  logger->addSmartBlank();
  logger->setReportLevel(logga::adhc);       // takes enum too

  logger->addSmartBlank();
  logger->addSmartBlank();
  logger->addSmartBlank();
  logger->addDumbBlank();                    // results in additional physical line

  // ---------------------------------------------------------
  //  test ONE        : general
  // ---------------------------------------------------------

  logger->test(1);                           // test start

  {
    logger->repx(logga::yeek, "argument count, int argc", argc);

    logger->repx
      (logga::yeek,
       "35_long_string_xxxxxxxxxxxxxxxxxxxx",
       "30_long_string_xxxxxxxxxxxxxxx");

    global->repx(logga::info, "global in main", "");
    logger->repx(logga::yeek, "local in main", "");

    ShortButWith22LongName short2;           // main function object with logging
    Long long1("long one");                  // another such object
    Static static2;                          // and another such object

    std::string appname(argv[0]);            // binary as invoked, including perhaps "./"
    logger->repx(logga::info, "argv[0]", appname);

    int zed = 456;
    logger->repx(logga::info, "int 456", zed);

    double num = 1.23456789e3;
    logger->repx(logga::yeek, "double 1.23456789e3 (see rounding)", num);  // informative

    logger->repx
      (logga::yeek, "long line to be truncated",
       "this line is rather long and will be truncated");
  }

  // ---------------------------------------------------------
  //  test TWO        : end of run reporting
  // ---------------------------------------------------------

  logger->test(2);                           // test start

  {
    logger->repx(logga::warn, "you were warned", "");

    std::ostringstream put;                  // typical usage follows
    put << "    we were" << (logger->getWarnFixedCount() ? "" : "n't") << " warned"
        << "\n";
    logger->putx(logga::dbug, put);          // CAUTION: also resets ostringstream

    put << "    all log rankings     :  " << logger->getAllTriggerStr() << "\n";
    logger->putx(logga::dbug, put);
  }

  // ---------------------------------------------------------
  //  test THREE      : stringstream tests
  // ---------------------------------------------------------

  logger->test(3);                           // test start

  {
    logger->repx(logga::info, "stringstream next", "");

    std::ostringstream put;
    put << "HEY LOOK AT ME !!" << "\n";
    put << "AND AGAIN"         << "\n";
    logger->putx(logga::dbug, put);

    logger->repx(logga::info, "stringstream again", "");
    logger->repx(logga::info, "stringstream again", "");

    logger->dotx(logga::warn, "some message");    // add highlight line

    logger->repx(logga::info, "stringstream again", "");
  }

  // ---------------------------------------------------------
  //  test FOUR       : stringstream reset trial
  // ---------------------------------------------------------

  logger->test(4);

  {
    std::ostringstream put;
    put << "    false : " << false << "\n";
    logger->putx(logga::dbug, put);

    put << std::boolalpha;
    put << "    false : " << false << "\n";
    logger->putx(logga::dbug, put);

    logger->resetOSS(put);                   // restore defaults

    put << "    false : " << false << "\n";
    logger->putx(logga::dbug, put);
  }

  // ---------------------------------------------------------
  //  test FIVE       : rank count reset
  // ---------------------------------------------------------

  logger->test(5, "about to reset the rank counts using default logga::kill");

  {
    logger->resetRankCounts();               // defaults to logga::kill
  }

  // ---------------------------------------------------------
  //  test SIX        : beep trials (near end of exectution)
  // ---------------------------------------------------------

  logger->test(6, "about to enable beeping and so forth");

  {
    logger->enableBeeping();                 // Logger default is silent
    logger->disableBeeping();
    logger->enableBeeping();
    logger->setBeepOnOrAbove(logga::warn);   // Logger default is logga::yeek
    logger->setBeepOnCompletion();           // Logger default is false
  }

  // ---------------------------------------------------------
  //  test SEVEN      : ad-hoc reporting level
  // ---------------------------------------------------------

  logger->test(7, "ad-hoc reporting level");

  {
    logger->repx(logga::adhc, "entity test", "");
  }

  // ---------------------------------------------------------
  //  test EIGHT      : trigger setting
  // ---------------------------------------------------------

  logger->test(8);

  {
    std::ostringstream put;
    std::string input = "somefile";
    logger->setTrigger(input);                         // set
    const std::string output = logger->getTrigger();   // get
    put << "    trigger input  : " << input  << "\n";
    put << "    trigger output : " << output << "\n";
    logger->putx(logga::dbug, put);
  }

  // ---------------------------------------------------------
  //  test NINE       : test
  // ---------------------------------------------------------

  logger->test(9, "'test' test");

  {
    logger->repx(logga::dbug, "check spacing above", "");
  }

  // ---------------------------------------------------------
  //  test TEN        : update return status test
  // ---------------------------------------------------------

  logger->test(10, "update return status test");

  {
    logger->updateReturnStatus(2, "data from test TEN");
  }

  // ---------------------------------------------------------
  //  test ELEVEN     : highest rank call
  // ---------------------------------------------------------

  logger->test(11, "highest rank call");

  {
    const int highestRankLogCallRank = logger->getHighestRankAll();
    const int highestRankLogCallInt  = static_cast<int>(highestRankLogCallRank);

    std::ostringstream put;
    put << "  " << "highest rank log call : " << highestRankLogCallInt << "\n";
    put << "  " << "zero means 'logga::yeek' called " << "\n";
    logger->putx(logga::dbug, put);
  }

  // ---------------------------------------------------------
  //  test ----       : an unrelated test for mach
  // ---------------------------------------------------------

#if 0 // 0 = safe

  logger->test(6);

  {
    int* p0 = new int(-100);                 // p1 to give compile error
    int* p1 = new int;                       // int; (was int(100);) will memory error
    std::cout << "p1 : " << *p1 << std::endl;
    delete p1;
    delete p1;                               // double delete will give core dump
    int* p2 = new int(200);
    std::cout << "p2 : " << *p2 << std::endl;
    // delete p2;                             // no delete will give memory leak
  }

#endif

  // ---------------------------------
  //  housekeeping
  // ---------------------------------

  logger->test(0);                           // test end
  logger->flush();                           // not generally needed

  logger->repx(logga::kill, "end of main", "~~~~~~~~~~");

  return xeona::exit_success;

} // main function

//  end of file

