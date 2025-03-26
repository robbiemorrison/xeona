//  XEONA ENERGY SYSTEMS MODELING ENVIRONMENT
//
//  file-create-name : sandia.ut0.cc
//  file-create-date : Mon 10-Jan-2011 12:15 UTC
//  file-initiator   : Robbie Morrison <robbie@actrix.co.nz>
//
//  file-role        : Sandia photovoltaic array model / unit test
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
//  $URL: file:///home/robbie/svn-root/xeona/futz/trunk/xeona1/h/sandia.ut0.cc $
//
//  GENERAL NOTES FOR THIS FILE
//
//  This is a unit test file and hence not essential to the
//  application proper.

//  LOCAL AND SYSTEM INCLUDES

#include "sandia.h"           // unit under test (place early)

#include "../a/logger.h"      // standard logging functionality (as required)
#include "../c/smart_ptr.h"   // toggle between Boost and TR1 smart pointers
#include ".././common.h"      // common definitions for project (place last)

#include <iomanip>            // setw() and family
#include <sstream>            // string-streams
#include <string>             // C++ strings

#include <boost/foreach.hpp>       // BOOST_FOREACH iteration macro
#include <boost/tuple/tuple.hpp>   // n-tuples, ref(), cref()

//  DATA SOURCE

#include "../g/pvdata.h"      // static const std::string csvSiemensSM55;

//  CODE

// ---------------------------------------------------------
//  CLASS           : SandiaTests
// ---------------------------------------------------------

class SandiaTests
{
public:

  typedef boost::tuple<std::string, bool, double> entry_type;

public:

  SandiaTests() :
    d_prods()
  { }

public:

  double
  test
  (const std::string label,
   const double      moduleTilt,
   const double      moduleAzimuth,
   const double      siteAltitude,
   const double      solarZenith,            // order "reversal" of angles and irradiance
   const double      solarAzimuth,
   const double      directSolar,            // order "reversal" of irradiance and angles
   const double      difuseSolar,
   const double      airTemperature,
   const double      windSpeed,
   const double      systemEffyFactor = 1.0)
  {
    PvModule pv(label);                      // mandatory label
    pv.slurp(csvSiemensSM55);                // 'csv' is a duly-modified Sandia CSV row
    pv.setInstallation(moduleTilt,           // module tilt, horizontal is zero [degrees]
                       moduleAzimuth,        // module azimuth angle [degrees]
                       siteAltitude);        // site altitude [m]
    pv.setWeather(directSolar,               // direct solar component [W/m2]
                  difuseSolar,               // diffuse solar component [W/m2]
                  solarZenith,               // solar zenith angle [degrees]
                  solarAzimuth,              // solar azimuth angle [degrees]
                  airTemperature,            // air temperature [C]
                  windSpeed);                // wind speed at 10m [m/s]
    double production = 0.0;                 // production [W]
    const bool okay = pv.calculate(production,
                                   systemEffyFactor); // effy [-], external to module

    std::ostringstream put;
    pv.printReport(put);
    s_logger->putx(logga::dbug, put);

    pv.printDataIssues(put);
    s_logger->addSmartBlank(logga::dbug);
    s_logger->putx(logga::dbug, put);

    put << std::setprecision(4);
    put << std::boolalpha;
    put << "  model                   : " << pv.getModel()    << "\n"
        << "  no data issues          : " << okay             << "\n"
        << "  system effy factor [-]  : " << systemEffyFactor << "\n"
        << "  production [W]          : " << production       << "\n";
    s_logger->addSmartBlank(logga::dbug);
    s_logger->putx(logga::dbug, put);

    d_prods.push_back(boost::make_tuple(label, okay, production));

    return production;
  }

  void
  report()
  {
    std::ostringstream put;
    put << "  production summary" << "\n";
    put << "\n";
    BOOST_FOREACH( entry_type e, d_prods )
      {
        put << "    " << std::setw(8) << std::left <<  e.get<0>()
            << "  : " << std::setw(6) << std::left << (e.get<1>() ? "" : "faulty")
            << "   "                               <<  e.get<2>() << "\n";
      }
    s_logger->putx(logga::dbug, put);
  }

private:

  std::vector<entry_type>    d_prods;
  static logga::spLogger     s_logger;

};

logga::spLogger SandiaTests::s_logger = logga::ptrLogStream();

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
    PvModule pv("simple instantiation");
    put << "  client object identifier : " << pv.getIdentifier() << "\n";
    logger->putx(logga::dbug, put);
  }

  // ---------------------------------------------------------
  //  test TWO        : full trial
  // ---------------------------------------------------------

  logger->test(2, "full trial");

  {
    PvModule pv("full trial");               // mandatory label
    pv.slurp(csvSiemensSM55);                // 'csv' is a duly-modified Sandia CSV row
    pv.setInstallation(10.0,                 // module tilt, horizontal is zero [degrees]
                       180.0,                // module azimuth angle [degrees]
                       300.0);               // site altitude [m]
    pv.setWeather(1000.0,                    // direct solar component [W/m2]
                  200.0,                     // diffuse solar component [W/m2]
                  20.0,                      // solar zenith angle [degrees]
                  20.0,                      // solar azimuth angle [degrees]
                  20.0,                      // air temperature [C]
                  2.0);                      // wind speed at 10m [m/s]
    double production = 0.0;                 // production [W]
    const double slf  = 0.90;                // system effy factor [-], external to module
    const bool okay = pv.calculate(production,
                                   slf);

    pv.printReport(put);
    logger->putx(logga::dbug, put);

    pv.printDataIssues(put);
    logger->addSmartBlank(logga::dbug);
    logger->putx(logga::dbug, put);

    put << std::setprecision(4);
    put << std::boolalpha;
    put << "  model                   : " << pv.getModel() << "\n"
        << "  no data issues (okay)   : " << okay          << "\n"
        << "  system effy factor [-]  : " << slf           << "\n"
        << "  production [W]          : " << production    << "\n";
    logger->addSmartBlank(logga::dbug);
    logger->putx(logga::dbug, put);
  }

  // ---------------------------------------------------------
  //  test THREE      : automated test
  // ---------------------------------------------------------

  logger->test(3, "automated test");

  {
    // argument layout:
    //
    //   label
    //
    //   moduleTilt
    //   moduleAzimuth
    //   siteAltitude
    //
    //   solarZenith
    //   solarAzimuth
    //   directSolar
    //   difuseSolar
    //
    //   airTemperature
    //   windSpeed
    //
    //   systemEffyFactor = 1.0

    // one  : guessed data
    // two  : calculated sun angles, guessed irradiation
    // NIWA : record from NIWA Otago TMY dataset

    SandiaTests st;                          // class defined above

    st.test("one",  10, 180, 300, 20,  20, 1000, 200, 20,  2);
    st.test("two",  30, 180, 100, 89, 131,  500, 500, 11, 14);
    st.test("NIWA", 30,   0, 370, 47, 304,  973,  56, 11, 14);

    st.report();

    //    production summary
    //
    //      one       :          51.621
    //      two       : faulty   0
    //      NIWA      :          46.2936

  }

  // ---------------------------------
  //  HOUSEKEEPING
  // ---------------------------------

  logger->test(0);                           // test end

  logger->repx(logga::info, "end of main", "~~~~~~~~~~");
  return xeona::exit_success;

} // main function

//  end of file

