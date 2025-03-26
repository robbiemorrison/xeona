//  XEONA ENERGY SYSTEMS MODELING ENVIRONMENT
//
//  file-create-name : wxinfo.ut0.cc
//  file-create-date : Fri 05-Nov-2010 11:29 UTC
//  file-initiator   : Robbie Morrison <robbie@actrix.co.nz>
//
//  file-role        : weather data class for testing / unit test
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
//  $URL: file:///home/robbie/svn-root/xeona/futz/trunk/xeona1/c/wxinfo.ut0.cc $
//
//  GENERAL NOTES FOR THIS FILE
//
//  This is a unit test file and hence not essential to the
//  application proper.

//  LOCAL AND SYSTEM INCLUDES

#include "wxinfo.h"           // unit under test (place early)

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

  xeona::yeek = 1;
  std::ostringstream put;

  // ---------------------------------------------------------
  //  test ONE        : simple instanciation
  // ---------------------------------------------------------

  logger->test(1, "simple instanciation");

  {
    const WeatherData wx;                    // proves that the object cannot be modified
  }

  // ---------------------------------------------------------
  //  test TWO        : over trunctation test
  // ---------------------------------------------------------

  logger->test(2, "over truncation test (warning log expected)");

  {
    const WeatherData wx;                    // proves that the object cannot be modified

    const int length = wx.getLength();

    shared_ptr<std::vector<double> > windSpeeds1 = wx.getWindSpeeds(length - 1);
    shared_ptr<std::vector<double> > windSpeeds2 = wx.getWindSpeeds(length + 0);
    shared_ptr<std::vector<double> > windSpeeds3 = wx.getWindSpeeds(length + 1);
  }

  // ---------------------------------------------------------
  //  test THREE      : NREL Colorado weather data set
  // ---------------------------------------------------------

  logger->test(3, "NREL Colorado weather data set");

  {
    const WeatherData wx;                    // proves that the object cannot be modified

    wx.metadata(put);
    logger->putx(logga::dbug, put);

    shared_ptr<std::vector<double> > solarDirect   = wx.getSolarDirect();
    shared_ptr<std::vector<double> > solarDifuse   = wx.getSolarDifuse();
    shared_ptr<std::vector<double> > zenithAngles  = wx.getZenithAngles();
    shared_ptr<std::vector<double> > azimuthAngles = wx.getAzimuthAngles();
    shared_ptr<std::vector<double> > airTemps      = wx.getAirTemps();
    shared_ptr<std::vector<double> > windSpeeds    = wx.getWindSpeeds();

    // airTemps->at(0) = +10.4878;           // shows that these vectors can be modified

    logger->addSmartBlank(logga::dbug);

    WeatherData::summarizeTimeseries(solarDirect,   "solar direct [W/m2]",      put, 0);
    WeatherData::summarizeTimeseries(solarDifuse,   "solar diffuse [W/m2]",     put, 1);
    WeatherData::summarizeTimeseries(zenithAngles,  "zenith angles [degrees]",  put, 1);
    WeatherData::summarizeTimeseries(azimuthAngles, "azimuth angles [degrees]", put, 1);
    WeatherData::summarizeTimeseries(airTemps,      "air temps [C]",            put, 1);
    WeatherData::summarizeTimeseries(windSpeeds,    "wind speeds [m/s]",        put, 1);
    logger->putx(logga::dbug, put);

    const int interval         = wx.getInterval();
    const std::string filename = wx.getFile();

    logger->addSmartBlank(logga::dbug);

    put << "  interval [s] : " << interval << "\n"
        << "  data source  : " << filename << "\n";

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

