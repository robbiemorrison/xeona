//  XEONA ENERGY SYSTEMS MODELING ENVIRONMENT
//
//  file-create-name : recorder.ut0.cc
//  file-create-date : Mon 26-Apr-2010 12:11 UTC
//  file-initiator   : Robbie Morrison <robbie@actrix.co.nz>
//
//  file-role        : summarizing recorder class / unit test
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
//  $URL: file:///home/robbie/svn-root/xeona/futz/trunk/xeona1/a/recorder.ut0.cc $
//
//  GENERAL NOTES FOR THIS FILE
//
//  This is a unit test file and hence not essential to the
//  application proper.

//  LOCAL AND SYSTEM INCLUDES

#include "recorder.h"         // unit under test (place early)

#include "../a/logger.h"      // standard logging functionality (as required)
#include "../c/smart_ptr.h"   // toggle between Boost and TR1 smart pointers
#include ".././common.h"      // common definitions for project (place last)

#include <string>             // C++ strings
#include <sstream>            // string-streams

//  CODE

// CAUTION: the following code is a bit strange because of the
// need to mask the real 'DomainController' and 'Gateway'
// implementations while unit testing both this unit and 'f/cta'.

class DomainController { public: std::string getIdAndKind(); };
class Gateway          { public: std::string getIdAndKind(); };

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
  //  test ONE        : recorder test
  // ---------------------------------------------------------

  logger->test(1, "recorder test");

  {
    shared_ptr<DomainController> domcon(new DomainController());
    shared_ptr<Gateway>          gate(new Gateway());

    EventRecorder ctalog("cta", 10);

    ctalog.cta("CtaSimple");
    ctalog.event("event","details", "remark");
    ctalog.capset(domcon, gate, "capset");
    ctalog.alert("reason");
    ctalog.note("some long note");
    ctalog.alert("uh-ohh!");

    put << ctalog.output();                  // final newline not required

    logger->putx(logga::yeek, put);
  }

  // ---------------------------------
  //  HOUSEKEEPING
  // ---------------------------------

  logger->test(0);                           // test end

  logger->repx(logga::info, "end of main", "~~~~~~~~~~");
  return xeona::exit_success;

} // main function

// OUTPUT
//
//   cta summary
//     step = 10
//     cnt   event             details                                 remark
//     ----------------------------------------------------------------------------------
//       0   EventRecorder     event recorder under construction       cta
//       1   cta               CtaSimple
//       2   event             details                                 remark
//   *   3   capset            gate-1 (from domcon-1)                  capset / reason
//   *   4   note              some long note                          uh-ohh!

// end of file

