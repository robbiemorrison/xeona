//  XEONA ENERGY SYSTEMS MODELING ENVIRONMENT
//
//  file-create-name : thrmperf.ut0.cc
//  file-create-date : Mon 25-Oct-2010 14:39 UTC
//  file-initiator   : Robbie Morrison <robbie@actrix.co.nz>
//
//  file-role        : building thermal performance and HVAC model / unit test
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
//  $URL: file:///home/robbie/svn-root/xeona/futz/trunk/xeona1/h/thrmperf.ut0.cc $
//
//  GENERAL NOTES FOR THIS FILE
//
//  This is a unit test file and hence not essential to the
//  application proper.

//  LOCAL AND SYSTEM INCLUDES

#include "thrmperf.h"         // unit under test (place early)

#include "../c/wxinfo.h"      // weather data class for testing
#include "../c/tsops.h"       // overloaded operators and similar for timeseries
#include "../c/smart_ptr.h"   // toggle between Boost and TR1 smart pointers
#include "../c/buildinfo.h"   // building asset to simulation data transfer
#include "../a/logger.h"      // standard logging functionality (as required)
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

  xeona::yeek = 1;
  std::ostringstream put;

  // ---------------------------------------------------------
  //  test ONE        : construction
  // ---------------------------------------------------------

  logger->test(1, "construction");

  {
    BuildingSim unfurnished (1800);          // interval length [s]
  }

  // ---------------------------------------------------------
  //  test TWO        : full simulation
  // ---------------------------------------------------------

  // note the prior non-WeatherData code was last in commit r5428

  logger->test(2, "full simulation");

  {
    // TYPEDEFS
    typedef boost::tuple<double, double> tuple_type;
    typedef std::vector<double>          ts_type;

    // CREATE CONTEXT DATA

    // interval details
    const int interval = 3600;               // horizon interval [s]
    const int samples  = 24;                 // samples per day

    // set point
    const double meanSetPointTemp = 18.0;

#if 1 // 0 = diurnal variation fill, 1 = constant fill
    shared_ptr<ts_type> setPointTempDay(new ts_type(samples,
                                                    meanSetPointTemp));
#else
    shared_ptr<ts_type> setPointTempDay(new ts_type());
    xeona::vectorFillDiurnal(setPointTempDay,     // stub
                             meanSetPointTemp,    // mean
                             2.0,                 // peak-to-peak
                             0,                   // offset
                             0.0,                 // randomness
                             samples,             // resolution
                             samples);            // size
#endif // 0

    // weather data object
    const WeatherData wx;

    wx.metadata(put);
    logger->putx(logga::dbug, put);

    // integrity check
    if ( interval != wx.getInterval() )
      {
        logger->repx(logga::warn, "interval mismatch, please check", __FILE__);
      }

    // fill vectors starting midnight
    shared_ptr<std::vector<double> > solarDirectDay  = wx.getSolarDirect(samples);
    shared_ptr<std::vector<double> > solarDifuseDay  = wx.getSolarDifuse(samples);
    shared_ptr<std::vector<double> > zenithAngleDay  = wx.getZenithAngles(samples);
    shared_ptr<std::vector<double> > azimuthAngleDay = wx.getAzimuthAngles(samples);
    shared_ptr<std::vector<double> > airTempDay      = wx.getAirTemps(samples);

    // recover scalars
    const int mid = 11;                      // same behavior as the hourize function
    const double setPointTemp = setPointTempDay->at(mid);
    const double airTemp      = airTempDay->at(mid);
    const double solarDirect  = solarDirectDay->at(mid);
    const double solarDifuse  = solarDifuseDay->at(mid);
    const double zenithAngle  = zenithAngleDay->at(mid);
    const double azimuthAngle = azimuthAngleDay->at(mid);

    // MISC

    bool runstatus;
    runstatus = false;
    runstatus = true;

    // BUILDING FABRIC

    shared_ptr<BuildingFabricData> fabricData(new BuildingFabricData("test"));

    fabricData->glazing.transDirect                  = 0.89;
    fabricData->glazing.transDifuse                  = 0.82;
    fabricData->glazing.absorptance                  = 0.11;
    fabricData->glazing.transGeneral                 = 0.86;

    fabricData->glazing.surfaceHeatTferCoeffOutside  = 16.7;
    fabricData->glazing.surfaceHeatTferCoeffInside   =  8.3;

    fabricData->walls.uvalue                         = 0.33;
    fabricData->walls.yvalue                         = 2.40;
    fabricData->walls.decf                           = 0.35;
    fabricData->walls.lag                            = 9;

    fabricData->roof.uvalue                          = 0.40;
    fabricData->roof.yvalue                          = 0.70;
    fabricData->roof.decf                            = 0.99;
    fabricData->roof.lag                             = 1;

    fabricData->windows.uvalue                       = 5.90;
    fabricData->windows.yvalue                       = 5.90;
    fabricData->windows.decf                         = 1.00;
    fabricData->windows.lag                          = 0;

    fabricData->setComplete();

    // BUILDING

    // constructor call
    BuildingSim building (interval);         // interval length [s]

    // building orientation
    double angle;
    angle = -90.0;                           // total solar gain [W] = 9277
    angle = -45.0;                           // total solar gain [W] = 8890
    angle =   0.0;                           // total solar gain [W] = 8916
    angle = +45.0;                           // total solar gain [W] = 8662
    angle = +90.0;                           // total solar gain [W] = 9277

    // input some data
    building.setBuildingData
      (15e3,                                 // capacity (hi bound) [W]
       angle,                                // building orientation [degrees]
       200.0,                                // floor area [m2]
       2.0,                                  // floor aspect ratio [-]
       3.0,                                  // height [m]
       0.40);                                // window-to-wall ratio [-]

    building.setBuildingFabric
      (fabricData);

    building.setPlantData
      (1.0,                                  // room air changes per hour [1/h]
       8e3,                                  // chiller cooling capacity [W]
       0.05,                                 // normalized chiller hysteresis factor [-]
       8e3,                                  // heater heating capacity [W]
       0.05,                                 // normalized heater hysteresis factor [-]
       0.3,                                  // duct loss factor [-]
       7.0,                                  // air-to-air heat exchanger LMTD [degrees C]
       runstatus);                           // run status

    building.setAmbientConditions
      (airTemp,                              // temperature of outside air [C]
       solarDirect,                          // solar direct irradiation [W/m2]
       solarDifuse,                          // solar diffuse irradiation [W/m2]
       zenithAngle,                          // zenith angle [degrees]
       azimuthAngle,                         // azimuth angle [degrees]
       airTempDay,                           // day versions of above follow
       solarDirectDay,
       solarDifuseDay,
       zenithAngleDay,
       azimuthAngleDay);

    // for experimental purposes
    double elecDemand = 2e3;
    //  elecDemand *=  +8;                   // chiller at full load + carryover warning
    //  elecDemand *=  +1;                   // chiller at full load
    //  elecDemand *=  -8;                   // chiller at part load
    //  elecDemand *= -12;                   //  heater at part load

    building.setBuildingOccupancyData
      (setPointTemp,                         // temperature set point [C]
       setPointTempDay,                      // day version of above
       180.0,                                // load contribution but not electricity [W]
       elecDemand);                          // electricity demand [W]

    // CALCULATE

    building.calculate();

    // REPORT

    // report
    building.report(put);
    logger->repx(logga::dbug, "in-depth reporting follows", "");
    logger->putx(logga::dbug, put);

    // data recovery
    logger->addSmartBlank(logga::dbug);
    double totalElectricityDemand = building.getTotalElecDemand();
    put << "  electricity demand : " << totalElectricityDemand << "\n";
    logger->repx(logga::dbug, "end of test reporting", "");
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

