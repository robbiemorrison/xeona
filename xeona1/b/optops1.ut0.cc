//  XEONA ENERGY SYSTEMS MODELING ENVIRONMENT
//
//  file-create-name : optops.ut0.cc
//  file-create-date : Fri 17-Oct-2008 14:41 UTC
//  file-initiator   : Robbie Morrison <robbie@actrix.co.nz>
//
//  file-role        : operations OSPs for technical assets / unit test
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
//  $URL: file:///home/robbie/svn-root/xeona/futz/trunk/xeona1/b/optops1.ut0.cc $
//
//  GENERAL NOTES FOR THIS FILE
//
//  This is a unit test file and hence not essential to the
//  application proper.

//  LOCAL AND SYSTEM INCLUDES

#include "optops1.h"          // unit under test (place early)

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
  //  test ONE        : class OpsTransmission
  // ---------------------------------------------------------

  logger->test(1, "trials using class 'OpsTransmisson'");

  {
    shared_ptr<svif::SolverIf> solver(new svif::SolverIf("unit-test"));
    xeona::DomainMode commitMode = xeona::e_shortrunFin;    // currently 32
    shared_ptr<OpsDcTrans_A> trans(new OpsDcTrans_A(solver, commitMode));

    const double inputCapacity  = 100.0e+03;
    const double voltage        =  11.0e+03;
    const double ohmsPerMetre   =   1.0e-03;
    const double length         = 100.0e+03;
    const int    discretization = 5;

    trans->uploadEngineering(inputCapacity,
                             voltage,
                             ohmsPerMetre,
                             length,
                             discretization);
  }

  // ---------------------------------------------------------
  //  test TWO        : class OpsFac1Out1
  // ---------------------------------------------------------

  logger->test(2, "trials using class 'OpsFac1Out1'");

  {
    shared_ptr<svif::SolverIf> solver(new svif::SolverIf("unit-test"));
    xeona::DomainMode commitMode = xeona::e_shortrunFin;    // currently 32
    shared_ptr<OpsFac1Out1_A> tport(new OpsFac1Out1_A(solver, commitMode));

    const double outputLoBound      = 0.0;   // lower output below which the asset trips
    const double outputHiBound      = 0.0;   // upper output
    const double marginalEfficiency = 0.0;   // slope (as decimal not percentage)
    const double noloadInput        = 0.0;   // fuel use on idle (input-axis intercept)
    const double ancillaryInput     = 0.0;   // fuel use on shutdown (for ancillaries)
    const double rampLo             = 0.0;   // lo ramp restriction
    const double rampHi             = 0.0;   // hi ramp restriction
    const double specEnthalpy       = 0.0;   // kg->J conversion
    const double scaleFactor        = 0.0;   // range reduction scaling factor

    tport->uploadEngineering(outputLoBound,
                             outputHiBound,
                             marginalEfficiency,
                             noloadInput,
                             ancillaryInput,
                             rampLo,
                             rampHi,
                             specEnthalpy,
                             scaleFactor);
  }

  // ---------------------------------
  //  HOUSEKEEPING
  // ---------------------------------

  logger->test(0);                           // test end

  logger->repx(logga::info, "end of main", "~~~~~~~~~~");
  return xeona::exit_success;

} // main function
//  end of file

