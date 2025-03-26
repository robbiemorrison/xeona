//  XEONA ENERGY SYSTEMS MODELING ENVIRONMENT
//
//  file-create-name : hydro.ut0.cc
//  file-create-date : Fri 04-Feb-2011 12:44 UTC
//  file-initiator   : Robbie Morrison <robbie@actrix.co.nz>
//
//  file-role        : hydro asset to operator data transfer / unit test
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
//  $URL: file:///home/robbie/svn-root/xeona/futz/trunk/xeona1/c/hydro.ut0.cc $
//
//  GENERAL NOTES FOR THIS FILE
//
//  This is a unit test file and hence not essential to the
//  application proper.

//  LOCAL AND SYSTEM INCLUDES

#include "hydro.h"            // unit under test (place early)

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
  //  test ONE        : general test
  // ---------------------------------------------------------

  logger->test(1, "general test");

  {
    int step;
    step =  0;
    step = -1;
    const double a = 88.88;
    shared_ptr<HydroStatus>
      hydrostatus(new HydroStatus("test", step, a, a, a, a, a, a, a, a, a, a, a));

    put << "  prior spill volume : " << hydrostatus->priorSpillVol << "\n";
    logger->putx(logga::yeek, put);
  }

  // ---------------------------------------------------------
  //  test TWO        : summary report
  // ---------------------------------------------------------

  logger->test(2, "summary report");

  {
    const double io  = 750e03;
    const double gen = 0.8e05 * io * 1.02 / 3600;      // assume hour interval
    HydroStatus hydrostatus("teas-1",        // identifier
                            42,              // step
                            io,              // io
                            540e6,           // generator capacity
                            74e06,           // storage volume
                            +50.2e06,        // inventory
                            +50.0e06,        // prior inventory
                            +2.0e05,         // inflow volume
                            +1.0e05,         // historical inflow volume
                            +1.0e05,         // prior inflow volume
                            -0.8e05,         // prior take volume
                            -0.0e05,         // prior spill volume
                            gen);            // prior dispatch

    put << hydrostatus.summarize(4);
    logger->putx(logga::yeek, put);
  }

  // ---------------------------------
  //  HOUSEKEEPING
  // ---------------------------------

  logger->test(0);                           // test end

  logger->repx(logga::info, "end of main", "~~~~~~~~~~");
  return xeona::exit_success;

} // main function

//  TEST 2 OUTPUT
//
//      hydro status summary / outflow is -ve
//          hydro identifier       : teas-1
//          current step           : 42
//          interval (called) [s]  : 7200
//        conversion
//          io factor [J/m3]       : 750000
//        generation
//          generator capacity [W] : 540000000
//          prior dispatch     [W] :  17000000
//        current
//          storage volume  [m3]   :    74000000
//          start inventory [m3]   :    50200000
//          inflow          [m3]   :     +200000
//          hist inflow     [m3]   :     +100000
//        prior
//          start inventory [m3]   :    50000000
//          inflow          [m3]   :     +100000
//          take            [m3]   :      -80000
//          spill           [m3]   :          -0
//        calculated
//          start inventory [m3]   :    50020000
//        generation usage
//          maximum duty take [m3] :     5184000
//          truncated take    [m3] :     5184000
//        some metrics
//          current [-]            : +67.84 %
//          prior   [-]            : +67.57 %
//          delta   [-]            :  -0.27 %
//          change  [-]            :  -0.40 %

//  end of file

