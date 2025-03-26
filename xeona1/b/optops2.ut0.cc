//  XEONA ENERGY SYSTEMS MODELING ENVIRONMENT
//
//  file-create-name : optops2.ut0.cc
//  file-create-date : Tue 10-Jan-2012 13:13 UTC
//  file-initiator   : Robbie Morrison <robbie@actrix.co.nz>
//
//  file-role        : operations OSPs for technical assets 2 / unit test
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
//  $URL: file:///home/robbie/svn-root/xeona/futz/trunk/xeona1/b/optops2.ut0.cc $
//
//  GENERAL NOTES FOR THIS FILE
//
//  This is a unit test file and hence not essential to the
//  application proper.

//  LOCAL AND SYSTEM INCLUDES

#include "optops2.h"          // unit under test (place early)

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

  xeona::yeek = 1;                           // maximum yeeking
  std::ostringstream put;

  // ---------------------------------------------------------
  //  test ONE        : class OpsLoadFlow
  // ---------------------------------------------------------

  logger->test(1, "trials using class 'OpsLoadFlow'");

  {
    shared_ptr<svif::SolverIf> solver(new svif::SolverIf("unit-test"));
    xeona::DomainMode commitMode = xeona::e_shortrunFin;    // currently 32
    shared_ptr<OpsLoadFlow_A> trans(new OpsLoadFlow_A(solver, commitMode));

    const double capacity            = 100.0e+03;
    const double voltage             =  11.0e+03;
    const double resistancePerMetre  =   1.0e-03;
    const double reactancePerMetre   =   1.0e-03;
    const double length              = 100.0e+03;
    const double maxShiftDegrees     = 90;
    const int    discretization      = 10;

    trans->uploadEngineering
      (capacity,
       voltage,
       resistancePerMetre,
       reactancePerMetre,
       length,
       maxShiftDegrees,
       discretization);
  }

  // ---------------------------------
  //  HOUSEKEEPING
  // ---------------------------------

  logger->test(0);                           // test end

  logger->repx(logga::info, "end of main", "~~~~~~~~~~");
  return xeona::exit_success;

} // main function

//  end of file

