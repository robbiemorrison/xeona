//  XEONA ENERGY SYSTEMS MODELING ENVIRONMENT
//
//  file-create-name : xemgen.ut0.cc
//  file-create-date : Mon 11-Aug-2008 14:46 UTC
//  file-initiator   : Robbie Morrison <robbie@actrix.co.nz>
//
//  file-role        : class to generate well-formatted XEM models / unit test
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
//  $URL: file:///home/robbie/svn-root/xeona/futz/trunk/xeona1/c/xemgen.ut0.cc $
//
//  GENERAL NOTES FOR THIS FILE
//
//  This is a unit test file and hence not essential to the
//  application proper.

//  LOCAL AND SYSTEM INCLUDES

#include "xemgen.h"           // unit under test (place early)

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
  //  test ONE        : inbuilt model as of 11-Aug-2008
  // ---------------------------------------------------------

  logger->test(1, "inbuilt model as of 11-Aug-2008");

  {
    XemGenerator x(45);

    x.note();

//      std::ostringstream ofile;
//      ofile << __FILE__;
//      x.com("this file generated from code within  : " + ofile.str(), 4);

    x.ident("Revision");
    x.ident("Date");
    x.ident("Author");
    x.ident("URL");

    x.meta("role");

    x.rule("program admin");

    x.special("program.last-run");
    x.out("process-id", "");
    x.out("run-kind", "");
    x.out("used-svn", "");
    x.out("simulate-return", "");

    x.special("program.data-format");
    x.in("minimum-svn", "0");

    x.rule("mandatory entities");

    x.horizon(6);                            // 6 steps

    x.entity("Overseer", "overseer");
    x.outq("builtin-remark s", "none");
    x.inq("captrans-algorithm s","simple");
    x.inQ("ranked-org-domains L", "domain-con-0");
    x.com("header: b/overseer.h");

    x.rule("hierarchical structure");

    x.com("(model goes here)");

    x.rule("miscellaneous");

    x.note();

    x.ident("Id");

    x.note();

    x.emacs();

    x.end();

    x.blanks(1);

    x.print(std::cout);
  }

  // ---------------------------------------------------------
  //  test TWO        : overseer and domain controller as of Oct-2011
  // ---------------------------------------------------------

  logger->test(2, "overseer and domain controller as of Oct-2011");

  {
    XemGenerator x(50);

    x.overseer();

    x.dule("A", "test two");

    x.domainController("domain-controller-a");

    x.blanks(1);

    x.print(std::cout);
  }

  // ---------------------------------
  //  HOUSEKEEPING
  // ---------------------------------

  logger->test(0);                           // test end

  logger->repx(logga::info, "end of main", "~~~~~~~~~~");
  return xeona::exit_success;

} // main function

//  end of file

