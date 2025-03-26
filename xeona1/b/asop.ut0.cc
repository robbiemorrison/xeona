//  XEONA ENERGY SYSTEMS MODELING ENVIRONMENT
//
//  file-create-name : asop.ut0.cc
//  file-create-date : Wed 15-Apr-2009 13:08 UTC
//  file-initiator   : Robbie Morrison <robbie@actrix.co.nz>
//
//  file-role        : asset operator entity / unit test
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
//  $URL: file:///home/robbie/svn-root/xeona/futz/trunk/xeona1/b/asop.ut0.cc $
//
//  GENERAL NOTES FOR THIS FILE
//
//  This is a unit test file and hence not essential to the
//  application proper.

//  LOCAL AND SYSTEM INCLUDES

#include "asop.h"             // unit under test (place early)

#include "../c/recset.h"      // records and fields and also record-sets
#include "../a/logger.h"      // standard logging functionality (as required)
#include "../c/smart_ptr.h"   // toggle between Boost and TR1 smart pointers
#include ".././common.h"      // common definitions for project (place last)

#include <string>             // C++ strings
#include <sstream>            // string-streams

//  CODE

// ---------------------------------------------------------
//  CLASS           : SubAsop_UT
// ---------------------------------------------------------

class SubAsop_UT :
  public AssetOperator
{
public:

  explicit
  SubAsop_UT
  (const std::string entityId,
   Record&           record) :
    CostRegister(record),
    AssetOperator(entityId, record, 10)     // [1]
  {
    s_logger->repx(logga::adhc, "constructor call", getIdAndKind());
    d_builtinRemark = "incomplete at sub-asop level";
  }

  // [1] the value ten encodes the supported commitment modes

  void      establish() { }
  void      conclude()  { }
  const int constrain(const xeona::DomainMode capacityMode) { return 88815; }
  void      washup()    { }

};

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
  //  test ONE        :
  // ---------------------------------------------------------

  logger->test(1, "");

  {
    Record r;
    r.hackUnitTestRecord("test one");

// problem arising from the absence of a XEM read-in
//
//  926  c/recset.cc  locateField   WARN  field name not found         technical-assets
// 1143  a/logger.cc  ptrLogStream  adhc  binding                      stdlog
//  207  c/recset.h   wrapExit      dbug  about to caution and probably exit
//
// shared_ptr<AssetOperator> subasop(new SubAsop_UT("sub-asop", r));
// subasop->constrain();

  }

  // ---------------------------------
  //  HOUSEKEEPING
  // ---------------------------------

  logger->test(0);                           // test end

  logger->repx(logga::info, "end of main", "~~~~~~~~~~");
  return xeona::exit_success;

} // main function

//  end of file

