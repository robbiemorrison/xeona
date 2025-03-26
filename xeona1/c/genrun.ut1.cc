//  XEONA ENERGY SYSTEMS MODELING ENVIRONMENT
//
//  file-create-name : genrun.ut1.cc
//  file-create-date : Tue 12-Aug-2008 15:29 UTC
//  file-initiator   : Robbie Morrison <robbie@actrix.co.nz>
//
//  file-role        : test for 'xemgen' and 'xemrun' / unit test
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
//  $URL: file:///home/robbie/svn-root/xeona/futz/trunk/xeona1/c/genrun.ut1.cc $
//
//  GENERAL NOTES FOR THIS FILE
//
//  This is a unit test file and hence not essential to the
//  application proper.

//  AD-HOC NOTES
//
//  This unit could also be a prototype for a general form of
//  build-time model testing (although that might be better
//  achieved through scripts and not units).

//  LOCAL AND SYSTEM INCLUDES

#include "xemgen.h"           // unit under test (place early)
#include "xemrun.h"           // unit under test (place early)

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
  logger->addSmartBlank();
  logger->setReportLevel(logga::adhc);       // maximum reporting

  // PRELIMINARY

  std::ostringstream put;

  // ---------------------------------------------------------
  //  test ONE        : simple construction
  // ---------------------------------------------------------

  logger->test(1, "simple construction");

  {
    XemGenerator xg;
    XemRun       xr(__FILE__);
  }

  // ---------------------------------------------------------
  //  test TWO        : simple 'xemgen' and 'xemrun'
  // ---------------------------------------------------------

  logger->test(2, "simple 'xemgen' and 'xemrun'");

  {
    XemGenerator xg(45);

    // subset of current 'inbuilt.xem'

    std::ostringstream ofile;
    ofile << __FILE__;

    xg.note();
    xg.com("this file generated from code within  : " + ofile.str());
    xg.com("svn revision                          : (not current)");
    xg.com("model complexity (1-8 increasing)     : 8");

    xg.rule("program admin");

    xg.special("program.last-run");
    xg.out("run-kind", "");
    xg.out("used-svn", "");
    xg.out("simulate-return", "");

    xg.special("program.data-format");
    xg.in("minimum-svn", "0");

    xg.rule("mandatory entities");

    xg.horizon(6);

    xg.rule("hierarchical structure");

    xg.entity("Overseer", "overseer");
    xg.inQ("domain-controllers s", "domain-con-1");
    xg.outq("builtin-remark s", "none");

    xg.com("to become a mandatory entity");
    xg.entity("DomainController", "domain-con-1");
    xg.inQ("asset-operators s", "");
    xg.outq("builtin-remark s", "none");
    xg.com("note the empty 'asset-operators' list");

    xg.rule("miscellaneous");

    xg.emacs();

    xg.end();

    // ---

    XemRun xr(__FILE__);
    xr.setBinary();                          // use the defaults in this case
    xr.setOptions("--report 1");
    xr.write(xg.string());
    xr.run();
    xr.dump(std::cout);

  }

  // ---------------------------------
  //  HOUSEKEEPING
  // ---------------------------------

  logger->test(0);                           // test end

  logger->repx(logga::info, "end of main", "~~~~~~~~~~");
  return xeona::exit_success;

} // main function

//  end of file

