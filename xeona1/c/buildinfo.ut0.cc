//  XEONA ENERGY SYSTEMS MODELING ENVIRONMENT
//
//  file-create-name : buildinfo.ut0.cc
//  file-create-date : Tue 12-Apr-2011 10:24 UTC
//  file-initiator   : Robbie Morrison <robbie@actrix.co.nz>
//
//  file-role        : building asset to simulation data transfer / unit test
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
//  $URL: file:///home/robbie/svn-root/xeona/futz/trunk/xeona1/c/buildinfo.ut0.cc $
//
//  GENERAL NOTES FOR THIS FILE
//
//  This is a unit test file and hence not essential to the
//  application proper.

//  LOCAL AND SYSTEM INCLUDES

#include "buildinfo.h"        // unit under test (place early)

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
  //  test ONE        : simple instantiation
  // ---------------------------------------------------------

  logger->test(1, "simple instantiation");

  {
    BuildingFabricData buildingData("test one");

    put << "    annotation : " << buildingData.getAnnotation() << "\n";
    logger->putx(logga::dbug, put);
  }

  // ---------------------------------------------------------
  //  test TWO        : full instantiation and report
  // ---------------------------------------------------------

  logger->test(2, "full instantiation and report");

  {
    // type    name                         comment
    // ------------------------------------------------------------
    // glazing
    //
    // double  transDirect                  solar direct transmission factor [-]
    // double  transDifuse                  solar diffuse transmission factor [-]
    // double  absorptance                  solar absorption factor [-]
    // double  transGeneral                 solar transmission factor [-]
    // double  surfaceHeatTferCoeffOutside  glazing heat transfer coeff, outside [W/m2K]
    // double  surfaceHeatTferCoeffInside   glazing heat transfer coeff, inside [W/m2K]
    //
    // opaque structure elements: walls, roof, windows
    //
    // double  uvalue                       area-averaged U-value [W/m2K]
    // double  yvalue                       area-averaged U-value [W/m2K]
    // double  decf                         decrement factor [-]
    // int     lag                          time lag [h]

    // loading -- most data from Chadderton (1989a, tables 3.2 and 3.3 and example 3.13)

    BuildingFabricData buildingData1("average fabric, 4mm single glazing");

    buildingData1.glazing.transDirect                 = 0.89;
    buildingData1.glazing.transDifuse                 = 0.82;
    buildingData1.glazing.absorptance                 = 0.11;
    buildingData1.glazing.transGeneral                = 0.86;

    buildingData1.glazing.surfaceHeatTferCoeffOutside = 16.7;
    buildingData1.glazing.surfaceHeatTferCoeffInside  =  8.3;

    buildingData1.walls.uvalue                        = 0.33;
    buildingData1.walls.yvalue                        = 2.40;
    buildingData1.walls.decf                          = 0.35;
    buildingData1.walls.lag                           = 9;

    buildingData1.roof.uvalue                         = 0.40;
    buildingData1.roof.yvalue                         = 0.70;
    buildingData1.roof.decf                           = 0.99;
    buildingData1.roof.lag                            = 1;

    buildingData1.windows.uvalue                      = 5.90;
    buildingData1.windows.yvalue                      = 5.90;
    buildingData1.windows.decf                        = 1.00;
    buildingData1.windows.lag                         = 0;

    buildingData1.setComplete();

    // reporting

    buildingData1.report(put, 4);
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

