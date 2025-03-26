//  XEONA ENERGY SYSTEMS MODELING ENVIRONMENT
//
//  file-create-name : teas.ut0.cc
//  file-create-date : Fri 14-Nov-2008 10:56 UTC
//  file-initiator   : Robbie Morrison <robbie@actrix.co.nz>
//
//  file-role        : technical asset entity / unit test
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
//  $URL: file:///home/robbie/svn-root/xeona/futz/trunk/xeona1/b/teas.ut0.cc $
//
//  GENERAL NOTES FOR THIS FILE
//
//  This is a unit test file and hence not essential to the
//  application proper.
//
//  AD-HOC NOTES
//
//  This file was created subsequent to the original unit being
//  coded.

//  LOCAL AND SYSTEM INCLUDES

#include "teas.h"             // unit under test (place early)

#include "../c/recset.h"      // records and fields and also record-sets
#include "../a/logger.h"      // standard logging functionality (as required)
#include "../c/smart_ptr.h"   // toggle between Boost and TR1 smart pointers
#include ".././common.h"      // common definitions for project (place last)

#include <string>             // C++ strings
#include <sstream>            // string-streams

//  CODE

// ---------------------------------------------------------
//  CLASS           : SubTeas_UT
// ---------------------------------------------------------

class SubTeas_UT :
  public TechnicalAsset
{
public:

  explicit
  SubTeas_UT
  (const std::string entityId,
   Record&           record) :
    CostRegister(record),
    TechnicalAsset(entityId, record, 10)     // [1]
  {
    s_logger->repx(logga::adhc, "constructor call", getIdAndKind());
    d_builtinRemark = "incomplete at sub-teas level";
  }

  // [1] the value ten encodes the supported commitment modes

  void      establish() { }
  void      conclude()  { }
  const int constrain(const xeona::DomainMode capacityMode) { return 88813; }
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
  //  test ONE        : make a 'SubTeas_UT' instance
  // ---------------------------------------------------------

  logger->test(1, "make a 'SubTeas_UT' instance");

  {
    Record r;
    r.hackUnitTestRecord("test one");
    shared_ptr<TechnicalAsset> subteas(new SubTeas_UT("sub-teas", r));
    subteas->constrain(xeona::e_modeNotSpecified);
  }

  // ---------------------------------
  //  HOUSEKEEPING
  // ---------------------------------

  logger->test(0);                           // test end

  logger->repx(logga::info, "end of main", "~~~~~~~~~~");
  return xeona::exit_success;

} // main function

//  end of file

