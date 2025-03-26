//  XEONA ENERGY SYSTEMS MODELING ENVIRONMENT
//
//  file-create-name : tests.ut0.cc
//  file-create-date : Thu 24-Jan-2008 15:43 UTC
//  file-initiator   : Robbie Morrison <robbie@actrix.co.nz>
//
//  file-role        : test sub-entities for use with --inbuilt and such / unit test
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
//  $URL: file:///home/robbie/svn-root/xeona/futz/trunk/xeona1/b/tests.ut0.cc $
//
//  GENERAL NOTES FOR THIS FILE
//
//  This is a unit test file and hence not essential to the
//  application proper.

//  AD-HOC NOTES

//  LOCAL AND SYSTEM INCLUDES

#include "tests.h"            // unit under test (place early)

#include "../a/logger.h"      // standard logging functionality (as required)
#include "../c/smart_ptr.h"   // toggle between Boost and TR1 smart pointers
#include ".././common.h"      // common definitions for project (place last)

#include <string>             // C++ strings
#include <sstream>            // string-streams

//  CODE

#if 1 // 0 = fake class 'Record' code, 1 = genuine recset suit
# include "../c/recset.h"     // records and fields and also record-sets
#else

//  *** WARNING: this Record class is for unit testing only

// ---------------------------------------------------------
//  CLASS           : Record (fake for this unit only)
// ---------------------------------------------------------
//
// this class definition exists to save adding 'recset.cc' and
// dependencies to the SOURCES list

class Record
{
public:
  Record()                                   // genuine Record is zero-argument too
  {
    logga::spLogger logger = logga::ptrLogStream();
    logger->repx(logga::kill, "constructor call", "FAKE class");
  }

  ~Record()
  {
    logga::spLogger logger = logga::ptrLogStream();
    logger->repx(logga::dbug, "destructor call", "FAKE class");
  }
};

#endif // 0

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
  //  test ONE        : make an TestEntity0
  // ---------------------------------------------------------

  logger->test(1, "make a TestEntity0");

  {
    Record r;                                  // generally available fake record
    r.hackUnitTestRecord("test one");
    TestEntity0 stack1("se1-stack", r);
  }

  // ---------------------------------------------------------
  //  test TWO        : traversals
  // ---------------------------------------------------------

  logger->test(2, "traversals");

  {
    Record r;
    r.hackUnitTestRecord("test two");

    TestEntity0 stack1("se1-stack", r);
    shared_ptr<Entity> heap1(new TestEntity0("se1-heap", r));

    heap1.reset();

    put << "    se1 : " << TestEntity0::traverseFullPopulation();
    logger->putx(logga::dbug, put);

    put << "    ent : " << Entity::traverseFullPopulation();
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

