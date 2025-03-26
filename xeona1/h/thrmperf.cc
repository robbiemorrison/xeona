//  XEONA ENERGY SYSTEMS MODELING ENVIRONMENT
//
//  file-create-name : thrmperf.cc
//  file-create-date : Mon 25-Oct-2010 14:39 UTC
//  file-initiator   : Robbie Morrison <robbie@actrix.co.nz>
//
//  file-role        : building thermal performance and HVAC model / implementation
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
//  $URL: file:///home/robbie/svn-root/xeona/futz/trunk/xeona1/h/thrmperf.cc $
//
//  GENERAL NOTES FOR THIS FILE

//  LOCAL AND SYSTEM INCLUDES

#include "thrmperf.h"         // companion header for this file (place first)

#include "../h/plank.h"       // chiller model
#include "../c/util4.h"       // free functions for trigonometry and maths
#include "../c/tsset.h"       // timeseries classes for added functionality
#include "../c/tsops.h"       // overloaded operators and similar for timeseries
#include "../c/stats.h"       // on-the-fly statistical calculations
#include "../c/buildinfo.h"   // building asset to simulation data transfer

#include "../a/logger.h"      // standard logging functionality (as required)
#include "../c/smart_ptr.h"   // toggle between Boost and TR1 smart pointers
#include ".././common.h"      // common definitions for project (place last)

#include <limits>             // numeric_limits<T>::infinity() and similar
#include <numeric>            // STL numerical algorithms
#include <sstream>            // string-streams
#include <string>             // C++ strings

#include <cmath>              // C-style maths, abs(), ceil(), floor(), sqrt()

#include <boost/foreach.hpp>                      // BOOST_FOREACH iteration macro
#include <boost/math/constants/constants.hpp>     // high-precision mathematical constants

//  CODE

// ---------------------------------------------------------
//  CLASS           : BuildingSim
// ---------------------------------------------------------

// STATIC DEFINITIONS - with and without explicit initialization

logga::spLogger BuildingSim::s_logger = logga::ptrLogStream();
const double    BuildingSim::s_init   = std::numeric_limits<double>::quiet_NaN();

// note that a 'quiet_NaN' streams as "nan"

// CREATORS

// ---------------------------------------------------------
//  MEMBER FUNCTION : BuildingSim
// ---------------------------------------------------------

BuildingSim::BuildingSim
(const int interval,
 const int hourStart) :
  // administration
  d_callTracker(0),                          // tracker to ensure call order is honored
  d_oss(),                                   // report buffer output string stream
  // constants set using constructor arguments
  d_interval(interval),
  d_hourStart(hourStart),
  // parameters
  d_demandHiBound(s_init),
  d_buildingOrientation(s_init),
  d_floorArea(s_init),
  d_floorAspectRatio(s_init),
  d_wallHeight(s_init),
  d_windowToWall(s_init),
  d_airChangesPerHour(s_init),
  d_chillerCapacity(s_init),
  d_chillerHysteresis(s_init),
  d_heaterCapacity(s_init),
  d_heaterHysteresis(s_init),
  d_ductLossFactor(s_init),
  d_airHxLmtd(s_init),
  d_tempSetPoint(s_init),
  d_activityLoad(s_init),
  d_electricityDemand(s_init),
  d_tempAirOutside(s_init),
  d_solarDirect(s_init),
  d_solarDifuse(s_init),
  d_zenithAngle(s_init),
  d_azimuthAngle(s_init),
  d_chillerCarryOverTrip(0.0),               // default value, zero tolerance
  d_heaterCarryOverTrip(0.0),                // default value, zero tolerance
  d_fabricData(),                            // empty shared pointer
  d_tempSetPointDayMean(s_init),
  d_tempAirOutsideDayMean(s_init),
  d_tempSolAir(s_init),
  d_tempSolAirDayMean(s_init),
  d_tempSolAirSwings12(),                    // empty vector (rather than empty pointer)
  // internal variables
  d_runStatus(true),
  d_shortDimension(s_init),
  d_chillerDuty(s_init),
  d_heaterDuty(s_init),
  d_hvacElecDemand(s_init),
  d_totalElecDemand(s_init),
  d_hvacLoadCarryOverTmp(0.0),
  d_hvacLoadCarryOver(0.0),
  // support classes
  d_chiller(new Chiller())
{
  // initial reporting
  s_logger->repx(logga::adhc, "constructor call", "");

} // function 'BuildingSim::BuildingSim'

// ---------------------------------------------------------
//  MEMBER FUNCTION : ~BuildingSim
// ---------------------------------------------------------

BuildingSim::~BuildingSim()                  // destructor
{
  s_logger->repx(logga::adhc, "destructor call", "");
}

// ---------------------------------------------------------
//  MEMBER FUNCTION : zeroCallTracker
// ---------------------------------------------------------

unsigned
BuildingSim::zeroCallTracker()               // reset 'd_callTracker'
{
  d_oss.str("");                             // empty the report buffer [1]

  // [1] emptying an ostringsteam: 'str(const std::string s)'
  // will set the buffer contents to 's', see Lischner (2003
  // p653) and elsewhere

  // start a new report
  d_oss << "  building simulation summary"                                        << "\n"
        << "  (+ indicates primary input)"                                        << "\n"
        << "\n"
        << "  constructor set parameters:"                                        << "\n"
        << "\n"
        << "  + interval [s]                     : " << d_interval                << "\n";

  // housekeeping
  const unsigned tmp = d_callTracker;
  d_callTracker      = 0;
  return tmp;
}

//// DATA CAPTURE ////////////////////////////////////////////////////////////////////////

void
BuildingSim::setHvacBacklogTrips             // for warning messages, nothing else
(const double chillerBacklogTrip,
 const double heaterBacklogTrip)
{
  d_chillerCarryOverTrip = chillerBacklogTrip;
  d_heaterCarryOverTrip  = heaterBacklogTrip;
}

// ---------------------------------------------------------
//  MEMBER FUNCTION : setBuildingData
// ---------------------------------------------------------

void
BuildingSim::setBuildingData
(const double demandHiBound,
 const double buildingOrientation,
 const double floorArea,
 const double floorAspectRatio,
 const double wallHeight,
 const double windowToWall)
{
  // process arguments
  d_demandHiBound       = demandHiBound;
  d_buildingOrientation = buildingOrientation;
  d_floorArea           = floorArea;
  d_floorAspectRatio    = floorAspectRatio;
  d_wallHeight          = wallHeight;
  d_windowToWall        = windowToWall;

  // intermediate calculation
  d_shortDimension      = std::sqrt(d_floorArea / d_floorAspectRatio);

  // integrity checks
  if ( d_buildingOrientation < -90.0 )
    {
      s_logger->repx(logga::warn,
                     "building orientation below -90",
                     d_buildingOrientation);
    }
  else if ( d_buildingOrientation > +90.0 )
    {
      s_logger->repx(logga::warn,
                     "building orientation above +90",
                     d_buildingOrientation);
    }

  // load the report buffer
  d_oss << "\n"
        << "  building data:"                                                     << "\n"
        << "\n"
        << "  + supply capacity [W] (hi bound)   : " << d_demandHiBound           << "\n"
        << "  + building orientation [degrees]   : " << d_buildingOrientation     << "\n"
        << "  + floor area [m2]                  : " << d_floorArea               << "\n"
        << "  + floor aspect ratio [-]           : " << d_floorAspectRatio        << "\n"
        << "  + wall height [m]                  : " << d_wallHeight              << "\n"
        << "  + window-to-wall ratio [-]         : " << d_windowToWall            << "\n"
        << "\n"
        << "    short dimension [m]              : " << d_shortDimension          << "\n";

  // housekeeping
  d_callTracker += 1;                        // call order protection
}

// ---------------------------------------------------------
//  MEMBER FUNCTION : setBuildingFabric
// ---------------------------------------------------------

void
BuildingSim::setBuildingFabric
(shared_ptr<BuildingFabricData> fabricData)
{
  // check the dataset is marked as complete
  const bool complete = fabricData->isComplete();
  if ( complete == false )
    {
      const std::string annotation = fabricData->getAnnotation();
      s_logger->repx(logga::warn, "fabric dataset not marked complete", annotation);
    }

  // transfer the data
  d_fabricData = fabricData;

  // load the report buffer
  const std::string finished = d_fabricData->isComplete() ? "yes" : "no";
  d_oss << "\n";
  d_oss << "  fabric data:"                                                       << "\n"
        << "\n";
  d_oss << "    complete   : " << finished                                        << "\n";
  d_oss << "    annotation : " << d_fabricData->getAnnotation()                   << "\n"
        << "  heat transmission thru glazing"                                     << "\n"
        << "    glazing"                                                          << "\n"
        << "  +   solar direct transmission factor [-]                  : "
        << d_fabricData->glazing.transDirect                                      << "\n"
        << "  +   solar diffuse transmission factor [-]                 : "
        << d_fabricData->glazing.transDifuse                                      << "\n"
        << "  +   solar absorption factor [-]                           : "
        << d_fabricData->glazing.absorptance                                      << "\n"
        << "  +   solar transmission factor [-]                         : "
        << d_fabricData->glazing.transGeneral                                     << "\n"
        << "  +   surface heat transfer coefficient / inside [W/m2K]    : "
        << d_fabricData->glazing.surfaceHeatTferCoeffOutside                      << "\n"
        << "  +   surface heat transfer coefficient / outside [W/m2K]   : "
        << d_fabricData->glazing.surfaceHeatTferCoeffInside                       << "\n"
        << "  heat gain thru opaque structure"                                    << "\n"
        << "    walls"                                                            << "\n"
        << "  +   element kind                                          : "
        << d_fabricData->say(d_fabricData->walls.kind)                            << "\n"
        << "  +   area-averaged U-value (24h mean response) [W/m2K]     : "
        << d_fabricData->walls.uvalue                                             << "\n"
        << "  +   area-averaged Y-value (transient response) [W/m2K]    : "
        << d_fabricData->walls.yvalue                                             << "\n"
        << "  +   decrement factor (attenuation) [-]                    : "
        << d_fabricData->walls.decf                                               << "\n"
        << "  +   lag (time delay) [h]                                  : "
        << d_fabricData->walls.lag                                                << "\n"
        << "    roof"                                                             << "\n"
        << "  +   element kind                                          : "
        << d_fabricData->say(d_fabricData->roof.kind)                             << "\n"
        << "  +   area-averaged U-value (24h mean response) [W/m2K]     : "
        << d_fabricData->roof.uvalue                                              << "\n"
        << "  +   area-averaged Y-value (transient response) [W/m2K]    : "
        << d_fabricData->roof.yvalue                                              << "\n"
        << "  +   decrement factor (attenuation) [-]                    : "
        << d_fabricData->roof.decf                                                << "\n"
        << "  +   lag (time delay) [h]                                  : "
        << d_fabricData->roof.lag                                                 << "\n"
        << "    windows (opaque behavior)"                                        << "\n"
        << "  +   element kind                                          : "
        << d_fabricData->say(d_fabricData->windows.kind)                          << "\n"
        << "  +   area-averaged U-value (24h mean response) [W/m2K]     : "
        << d_fabricData->windows.uvalue                                           << "\n"
        << "  +   area-averaged Y-value (transient response) [W/m2K]    : "
        << d_fabricData->windows.yvalue                                           << "\n"
        << "  +   decrement factor (attenuation) [-]                    : "
        << d_fabricData->windows.decf                                             << "\n"
        << "  +   lag (time delay) [h]                                  : "
        << d_fabricData->windows.lag                                              << "\n";

  // housekeeping
  d_callTracker += 2;                        // call order protection
}

// ---------------------------------------------------------
//  MEMBER FUNCTION : setPlantData
// ---------------------------------------------------------

void
BuildingSim::setPlantData
(const double airChangesPerHour,
 const double chillerCapacity,
 const double chillerHysteresis,
 const double heaterCapacity,
 const double heaterHysteresis,
 const double ductLossFactor,
 const double airHxLmtd,
 const bool   runStatus)
{
  // load data
  d_airChangesPerHour  = airChangesPerHour;
  d_chillerCapacity    = chillerCapacity;
  d_chillerHysteresis  = chillerHysteresis;
  d_heaterCapacity     = heaterCapacity;
  d_heaterHysteresis   = heaterHysteresis;
  d_ductLossFactor     = ductLossFactor;
  d_airHxLmtd          = airHxLmtd;
  d_runStatus          = runStatus;

  // load the report buffer
  d_oss << "\n"
        << "  plant data:"                                                        << "\n"
        << "\n"
        << "  + air changes per hour [1/h]       : " << d_airChangesPerHour       << "\n"
        << "  + chiller thermal capacity [W]     : " << d_chillerCapacity         << "\n"
        << "  + chiller hysteresis [W]           : " << d_chillerHysteresis       << "\n"
        << "  + heater thermal capacity [W]      : " << d_heaterCapacity          << "\n"
        << "  + heater hysteresis [W]            : " << d_heaterHysteresis        << "\n"
        << "  + duct loss factor (chiller) [-]   : " << d_ductLossFactor          << "\n"
        << "  + air-to-air hx LMTD [C]           : " << d_airHxLmtd               << "\n"
        << "  + run status                       : "
        << (d_runStatus == true ? "on" : "off")                                   << "\n";
}

// ---------------------------------------------------------
//  MEMBER FUNCTION : setAmbientConditions (also wrapper)
// ---------------------------------------------------------
//  Description  : calculate averages and store related data
//  Role         : client support
//  Techniques   : 'Statistics' functors
//  Status       : complete
//
//  Design notes
//
//      The member function 'std::vector<>::at' will throw a
//      range error exception if an invalid index is supplied.
//      Short vectors mean the client code is faulty.  Moreover,
//      there is no need to catch and process this exception.
//
//      The 'Statistics' functors are fully documented within
//      their own unit.
//
//  Notation
//
//      t_eo    d_tempSolAir             instantaneous sol-air temperature **
//      t_eo    d_tempSolAirDayMean      24 hour mean sol-air temperature * **
//
//      * would be better to add a overhead bar for mean values (\bar{t}_c etc)
//     ** also referred to "environment outside" temperature, hence the "eo" subscript
//
// ---------------------------------------------------------

void
BuildingSim::setAmbientConditions
(const double                                            tempAirOutside,
 boost::tuple<double, double>                            solarIrradiation,
 boost::tuple<double, double>                            sunAngle,
 shared_ptr<std::vector<double> >                        tempAirOutsideDay,
 shared_ptr<std::vector<boost::tuple<double, double> > > solarIrradiationDay,
 shared_ptr<std::vector<boost::tuple<double, double> > > sunAnglesDay)
{
  // initial reporting
  s_logger->repx(logga::adhc, "entering member function", "wrapper");

  // wrapper call
  setAmbientConditions(tempAirOutside,
                       solarIrradiation.get<0>(),
                       solarIrradiation.get<1>(),
                       sunAngle.get<0>(),
                       sunAngle.get<1>(),
                       tempAirOutsideDay,
                       xeona::vector2TupleExtract0(solarIrradiationDay),
                       xeona::vector2TupleExtract1(solarIrradiationDay),
                       xeona::vector2TupleExtract0(sunAnglesDay),
                       xeona::vector2TupleExtract1(sunAnglesDay));
}

void
BuildingSim::setAmbientConditions
(const double                     tempAirOutside,
 const double                     solarDirect,
 const double                     solarDifuse,
 const double                     zenithAngle,
 const double                     azimuthAngle,
 shared_ptr<std::vector<double> > tempAirOutsideDay,
 shared_ptr<std::vector<double> > solarDirectDay,
 shared_ptr<std::vector<double> > solarDifuseDay,
 shared_ptr<std::vector<double> > zenithAngleDay,
 shared_ptr<std::vector<double> > azimuthAngleDay)
{
  // initial reporting (important because a range error exception may result)
  s_logger->repx(logga::adhc, "entering member function", "substantive");

  // typedef declaration for convenience
  typedef std::vector<double> vec_type;

  // ---------------------------------
  //  process scalar arguments
  // ---------------------------------

  d_tempAirOutside = tempAirOutside;
  d_solarDirect    = solarDirect;
  d_solarDifuse    = solarDifuse;
  d_zenithAngle    = zenithAngle;
  d_azimuthAngle   = azimuthAngle;

  // current
  d_tempSolAir = calcTempSolAir(tempAirOutside,
                                solarDirect,
                                solarDifuse,
                                zenithAngle,
                                azimuthAngle);

  // ---------------------------------
  //  mean value calculations
  // ---------------------------------

  // YEEK 51 information gathering
  std::ostringstream put;
  if ( xeona::yeek == 51 || xeona::yeek == 1 || xeona::yeek == 2 )
    {
      put << "  " << "tempSolAir calculations" << "\n";
    }

  // on-the-fly statistics functor
  Statistics<double> solairs;
  Statistics<double> outsides;

  // process the vector
  const int len = tempAirOutsideDay->size(); // not necessarily 24
  for ( int i = 0; i < len; ++i )
    {
      const double tempSolAir = calcTempSolAir(tempAirOutsideDay->at(i),
                                               solarDirectDay->at(i),
                                               solarDifuseDay->at(i),
                                               zenithAngleDay->at(i),
                                               azimuthAngleDay->at(i));
      solairs(tempSolAir);                   // update statistics

      // YEEK 51 information gathering
      if ( xeona::yeek == 51 || xeona::yeek == 1 || xeona::yeek == 2 )
        {
          put << "\n";
          put << "    inputs / i            : " << i                          << "\n"
              << "      stats functor count : " << solairs.count()            << "\n"
              << "      length              : " << len                        << "\n"
              << "      tempAirOutsideDay   : " << tempAirOutsideDay->at(i)   << "\n"
              << "      solarDirectDay      : " << solarDirectDay->at(i)      << "\n"
              << "      solarDifuseDay      : " << solarDifuseDay->at(i)      << "\n"
              << "      zenithAngleDay      : " << zenithAngleDay->at(i)      << "\n"
              << "      azimuthAngleDay     : " << azimuthAngleDay->at(i)     << "\n";
          put << "    calculated:"                                            << "\n"
              << "      tempSolAir          : " << tempSolAir                 << "\n";
        }
    }

  // additional reporting as appropriate
  // YEEK 51 CODE (set by '--yeek')
  if ( xeona::yeek == 51 || xeona::yeek == 1 || xeona::yeek == 2 )
    {
      s_logger->repx(logga::dbug, "additional reporting follows, yeek", xeona::yeek);
      s_logger->putx(logga::dbug, put);
    }

  // fill the functor directly
  outsides = xeona::fillStatistics(tempAirOutsideDay);

  // recover the mean values
  d_tempSolAirDayMean = solairs.mean();
  d_tempAirOutsideDayMean = outsides.mean();

  // ---------------------------------
  //  swing value calculations
  // ---------------------------------

  // declare a buffer
  std::vector<double> buffer;

  // process the vector
  for ( int i = 0; i < len; ++i )
    {
      const double tempSolAir = calcTempSolAir(tempAirOutsideDay->at(i),
                                               solarDirectDay->at(i),
                                               solarDifuseDay->at(i),
                                               zenithAngleDay->at(i),
                                               azimuthAngleDay->at(i));
      const double tempSolAirSwing = tempSolAir - d_tempSolAirDayMean;
      buffer.push_back(tempSolAirSwing);
    }

  // hourize using a timeseries wrapper
  TsNormal tempTs(buffer, d_interval, "sol air temp original");
  TsNormal hourTs = tempTs.sampleLinear("sol air temp resampled",
                                        3600);    // hourize
  hourTs.copyfill(buffer, 12);                    // copy over
  std::reverse(buffer.begin(), buffer.end());     // obtain count back ordering

  // load
  d_tempSolAirSwings12 = buffer;

  // ---------------------------------
  //  yeek reporting
  // ---------------------------------

  // additional reporting as appropriate : print the sol-air mean and swings
  // YEEK 39 CODE (set by '--yeek')
  if ( xeona::yeek == 39 || xeona::yeek == 1 || xeona::yeek == 2 )
    {
      std::ostringstream put;
      put << "  sol-air temp swing values count back from no lag to 11 h lag"   << "\n"
          << "  original length         : " << len                              << "\n"
          << "  sol-air temp mean [C]   : " << d_tempSolAirDayMean              << "\n";
      shared_ptr<std::vector<double> > tmp(new std::vector<double>(d_tempSolAirSwings12));
      xeona::vectorPrint(tmp, "sol-air temp swings [C]", put);
      s_logger->repx(logga::dbug, "additional reporting follows, yeek", xeona::yeek);
      s_logger->putx(logga::dbug, put);
    }

  // ---------------------------------
  //  load the report buffer
  // ---------------------------------

  const double currentSwing = d_tempSolAirSwings12.front();
  d_oss << "\n"
        << "  ambient conditions:"                                                << "\n"
        << "\n"
        << "  + solar direct [W/m2]              : " << d_solarDirect             << "\n"
        << "  + solar diffuse [W/m2]             : " << d_solarDifuse             << "\n"
        << std::showpos
        << "  + zenith [degrees]                 : " << d_zenithAngle             << "\n"
        << "  + azimuth [degrees]                : " << d_azimuthAngle            << "\n"
        << std::noshowpos
        << "  + outside air temperature [C]      : " << d_tempAirOutside          << "\n"
        << "\n"
        << "    sol-air temperature [C]          : " << d_tempSolAir              << "\n"
        << "    sol-air temperature mean [C]     : " << d_tempSolAirDayMean       << "\n"
        << "    sol-air current swing [C]        : " << currentSwing              << "\n";

  // ---------------------------------
  //  housekeeping
  // ---------------------------------

  d_callTracker += 4;                        // call order protection

}

// ---------------------------------------------------------
//  MEMBER FUNCTION : setBuildingOccupancyData
// ---------------------------------------------------------
//
//  Notation
//
//      t_c     d_tempSetPoint           instantaneous indoor temperature
//      t_c     d_tempSetPointDayMean    24 hour mean indoor temperature *
//
//      * would be better to add a overhead bar for mean values (\bar{t}_c etc)
//
// ---------------------------------------------------------

void
BuildingSim::setBuildingOccupancyData
(const double                     tempSetPoint,
 shared_ptr<std::vector<double> > tempSetPointDay,
 const double                     activityLoad,
 const double                     elecDemand)
{
  // mean value calculation
  const double total  = std::accumulate(tempSetPointDay->begin(),
                                        tempSetPointDay->end(),
                                        0.0);     // initial value
  const double length              = tempSetPointDay->size();
  const double tempSetPointDayMean = total / length;

  // load values
  d_tempSetPoint        = tempSetPoint;
  d_tempSetPointDayMean = tempSetPointDayMean;
  d_activityLoad        = activityLoad;
  d_electricityDemand   = elecDemand;

  // load the report buffer
  d_oss << "\n"
        << "  occupancy data:" << "\n"
        << "\n"
        << "  + current set point [C]            : " << d_tempSetPoint            << "\n"
        << "  + mean set point [C]               : " << d_tempSetPointDayMean     << "\n"
        << "  + activity load [W]                : " << d_activityLoad            << "\n"
        << "  + internal electricity demand [W]  : " << d_electricityDemand       << "\n";

  // housekeeping
  d_callTracker += 8;                        // call order protection
}

//// RETRIEVAL AND REPORTING /////////////////////////////////////////////////////////////

// RETRIEVE CALLS

// ---------------------------------------------------------
//  MEMBER FUNCTION : getInterval
// ---------------------------------------------------------

int
BuildingSim::getInterval() const
{
  return d_interval;
}

// ---------------------------------------------------------
//  MEMBER FUNCTION : getChillerDuty
// ---------------------------------------------------------

double
BuildingSim::getChillerDuty() const
{
  if ( d_callTracker != 31 )                 // coding error in client code
    {
      s_logger->repx(logga::warn, "call order issue, tracker not 31", d_callTracker);
    }
  return d_chillerDuty;
}

// ---------------------------------------------------------
//  MEMBER FUNCTION : getHeaterDuty
// ---------------------------------------------------------

double
BuildingSim::getHeaterDuty() const
{
  if ( d_callTracker != 31 )                 // coding error in client code
    {
      s_logger->repx(logga::warn, "call order issue, tracker not 31", d_callTracker);
    }
  return d_heaterDuty;
}

// ---------------------------------------------------------
//  MEMBER FUNCTION : getHvacElecDemand
// ---------------------------------------------------------

double
BuildingSim::getHvacElecDemand() const
{
  if ( d_callTracker != 31 )                 // coding error in client code
    {
      s_logger->repx(logga::warn, "call order issue, tracker not 31", d_callTracker);
    }
  return d_hvacElecDemand;
}

// ---------------------------------------------------------
//  MEMBER FUNCTION : getTotalElecDemand
// ---------------------------------------------------------

double
BuildingSim::getTotalElecDemand() const
{
  if ( d_callTracker != 31 )                 // coding error in client code
    {
      s_logger->repx(logga::warn, "call order issue, tracker not 31", d_callTracker);
    }
  return d_totalElecDemand;
}

// ---------------------------------------------------------
//  MEMBER FUNCTION : getHvacMetric
// ---------------------------------------------------------

double
BuildingSim::getHvacMetric()  const
{
  if ( d_callTracker != 31 )                 // coding error in client code
    {
      s_logger->repx(logga::warn, "call order issue, tracker not 31", d_callTracker);
    }
  return d_hvacElecDemand / d_floorArea;
}

// ---------------------------------------------------------
//  MEMBER FUNCTION : getTotalMetric
// ---------------------------------------------------------

double
BuildingSim::getTotalMetric() const
{
  if ( d_callTracker != 31 )                 // coding error in client code
    {
      s_logger->repx(logga::warn, "call order issue, tracker not 31", d_callTracker);
    }
  return d_totalElecDemand / d_floorArea;
}

// REPORT CALLS

// ---------------------------------------------------------
//  MEMBER FUNCTION : report
// ---------------------------------------------------------

void
BuildingSim::report
(std::ostream& os) const
{
  // final information
  std::ostringstream oss;
  oss << "\n"
      << "  call status information:"                                             << "\n"
      << "\n"
      << "    call tracker                     : "
      << d_callTracker << " = "  << interpretCallTrack(d_callTracker)             << "\n"
      << "\n"
      << "  building simulation summary ends"                                     << "\n";

  // stream the report
  os << d_oss.str()
     << oss.str();
}

//// CALCULATIONS ////////////////////////////////////////////////////////////////////////

// CALCULATE CALL

// ---------------------------------------------------------
//  MEMBER FUNCTION : calculate
// ---------------------------------------------------------
//  Description  : public call to undertake the simulation
//  Role         : client support
//  Techniques   : calls a number of utility functions
//  Status       : complete
//
//  Design notes
//
//      See the header documentation for an overview.  And also
//      the documentation for the individual calls for details.
//
// ---------------------------------------------------------

void
BuildingSim::calculate()
{
  // initial reporting
  s_logger->repx(logga::adhc, "entering member function", "");

  // call order checking
  if ( d_callTracker != 15 )                  // coding error in client code
    {
      s_logger->repx(logga::warn, "call order issue, tracker not 15", d_callTracker);
    }
  else
    {
      d_callTracker += 16;                   // call order protection
    }

  // calculate calls
  const double glazingLoad  = calcGlazingLoad();
  const double opaqueLoad   = calcOpaqueLoad();
  const double internalLoad = calcInternalLoad();
  const double totalLoad    = glazingLoad + opaqueLoad + internalLoad;
  const double hvacElec     = calcHvac(totalLoad, d_runStatus);
  const double totalElec    = hvacElec + d_electricityDemand;

  // store values
  d_hvacElecDemand  = hvacElec;
  d_totalElecDemand = totalElec;

  // confirm supply capacity is sufficient
  if ( d_totalElecDemand > d_demandHiBound )
    {
      std::ostringstream oss;
      oss << d_totalElecDemand << " > " << d_demandHiBound;
      s_logger->repx(logga::warn, "electricity demand exceeds capacity", oss.str());
    }

  // load the report buffer
  d_oss << "\n"
        << "  aggregate results:"                                                 << "\n"
        << "\n"
        << std::showpos
        << "    environmental load [W]           : " << glazingLoad + opaqueLoad  << "\n"
        << "    internal load [W]                : " << internalLoad              << "\n"
        << "    total load [W]                   : " << totalLoad                 << "\n"
        << std::noshowpos
        << "\n"
        << "    HVAC electricity [W]             : " << d_hvacElecDemand          << "\n"
        << "    total electricity [W]            : " << d_totalElecDemand         << "\n"
        << "\n"
        << "    HVAC metric [W/m2]               : " << getHvacMetric()           << "\n"
        << "    total metric [W/m2]              : " << getTotalMetric()          << "\n";

} // function 'calculate'

// ---------------------------------------------------------
//  MEMBER FUNCTION : saveState
// ---------------------------------------------------------
//  Description  : update all state information (currently just the carryover HVAC load)
//  Role         : called by client 'washup'
//  Techniques   : (nothing special)
//  Status       : complete
//
//  Design notes
//
//      This function is illustrates an important point regarding
//      stateful helper classes, namely:
//
//      State information should only be updated in 'washup'
//      calls and persistent non-entity helper classes (like this
//      one) should only have their state information stored
//      properly at this point.
//
// ---------------------------------------------------------

void
BuildingSim::saveState()                     // called by host DURING washup
{
  d_hvacLoadCarryOver    = d_hvacLoadCarryOverTmp;
  d_hvacLoadCarryOverTmp = 0.0;              // should always be updated by other code
}

// UTILITY FUNCTIONS

// ---------------------------------------------------------
//  MEMBER FUNCTION : calcGlazingLoad
// ---------------------------------------------------------
//  Description  : define windows and invoke the window modeling routine
//  Role         : called by 'calculate'
//  Techniques   : 'simulateGlazing'
//  Status       : complete
// ---------------------------------------------------------

double
BuildingSim::calcGlazingLoad()
{
  // ---------------------------------
  //  window calls
  // ---------------------------------

  // standard glass areas
  const double endWallArea    = d_shortDimension * d_wallHeight;
  const double glassAreaShort = d_windowToWall * endWallArea * 1.0;
  const double glassAreaLong  = d_windowToWall * endWallArea * d_floorAspectRatio;

  // load the report buffer
  d_oss << "\n"
        << "  glazing calculations:"                                              << "\n";

  // window calls in order
  double summedSolarGain = 0.0;
  summedSolarGain += simulateGlazing(d_buildingOrientation +   0.0, glassAreaLong);
  summedSolarGain += simulateGlazing(d_buildingOrientation +  90.0, glassAreaShort);
  summedSolarGain += simulateGlazing(d_buildingOrientation + 180.0, glassAreaLong);
  summedSolarGain += simulateGlazing(d_buildingOrientation + 270.0, glassAreaShort);

  // ---------------------------------
  //  yeek reporting
  // ---------------------------------

  // additional reporting as appropriate
  // YEEK 39 CODE (set by '--yeek')
  if ( xeona::yeek == 39 || xeona::yeek == 1 || xeona::yeek == 2 )
    {
      std::ostringstream put;
      put << "  aggregate glazing element calculations"               << "\n"
          << "    outputs:"                                           << "\n"
          << "      total glazing load [W]       : " << summedSolarGain   << "\n";
      s_logger->repx(logga::dbug, "additional reporting follows, yeek", xeona::yeek);
      s_logger->putx(logga::dbug, put);
    }

  // ---------------------------------
  //  housekeeping
  // ---------------------------------

  // load the report buffer
  d_oss << "\n"
        << "    total solar gain [W]             : " << summedSolarGain           << "\n";

  return summedSolarGain;
}

// ---------------------------------------------------------
//  MEMBER FUNCTION : simulateGlazing
// ---------------------------------------------------------
//  Description  : calculate heat transmission thru glazing
//  Role         : internal
//  Techniques   : sun/windows geometry, glass properties
//  Status       : complete
//
//  Design notes
//
//      See Chadderton (1989a) section "Heat transmission through
//      glazing".
//
//      Shading effects are not considered -- in other words, the
//      sunlit area is equal to the window area.
//
//      Unfortunately the notation used by Chadderton is not
//      fully resolved.  Some quantities shown below are implied
//      but never expressly stated by Chadderton.
//
//  Notation
//
//      A           glassArea                              [m2]
//      A, angle    90 - d_zenithAngle                     [degrees]
//      B, angle    d_azimuthAngle                         [degrees]
//      C, angle    windowOrientation                      [degrees]
//      D, angle    glassSunAzimuth                        [degrees]
//
//      I_D         d_solarDirect                          [W/m2]
//      I_d         d_solarDifuse                          [W/m2]
//      I_DVd       irradDirectGlass                       [W/m2]
//      I_TVd       irradTotalGlass                        [W/m2]
//      I_THd       irradTotalHoriz                        [W/m2]
//
//      A           glazingAbsorptance                     [-]
//      h_so        glazingSurfaceHeatTferCoeffOutside     [W/m2K]
//      h_si        glazingSurfaceHeatTferCoeffInside      [W/m2K]
//      T_D         glazingTransDirect                     [-]
//      T_d         glazingTransDifuse                     [-]
//      T           glazingTransGeneral                    [-]
//
//      Q_D         glassDirectGain                        [W]
//      Q_d         glassDifuseGain                        [W]
//      Q           glassGain                              [W]
//      Q           glassCond                              [W]
//
//      t_g         tempGlass                              [C]
//      t_ao        tempAirOutside                         [C]
//      t_ai        d_tempSetPoint                         [C]
//
//                  groundReflectFactor                    [-]
//
//  CAUTION: hard-coded parameters
//
//      Some hard-coded parameters should properly be looked up
//      in suitably encoded CIBSE tables.
//
// ---------------------------------------------------------

double
BuildingSim::simulateGlazing
(const double windowOrientation,
 const double glassArea,
 const double groundReflectFactor)           // note default value
{
  // ---------------------------------
  //   import properties
  // ---------------------------------

  const double glazingTransDirect  = d_fabricData->glazing.transDirect;
  const double glazingTransDifuse  = d_fabricData->glazing.transDifuse;
  const double glazingAbsorptance  = d_fabricData->glazing.absorptance;
  const double glazingTransGeneral = d_fabricData->glazing.transGeneral;

  const double glazingSurfaceHeatTferCoeffOutside
    = d_fabricData->glazing.surfaceHeatTferCoeffOutside;
  const double glazingSurfaceHeatTferCoeffInside
    = d_fabricData->glazing.surfaceHeatTferCoeffInside;

  // ---------------------------------
  //    preamble
  // ---------------------------------

  double glassGain     = 0.0;                // solar gain, both direct and diffuse
  double glassCond     = 0.0;                // conductive transfer, includes absorption
  double glassTransfer = 0.0;                // total transfer

  // ---------------------------------
  //  general conditions
  // ---------------------------------

  // glass-sun azimuth (compass angle) in range [0,180]
  double glassSunAzimuth = 0.0;
  glassSunAzimuth = d_azimuthAngle - windowOrientation;
  glassSunAzimuth = xeona::normalizePlusMinus180(glassSunAzimuth); // normalize [-180,180]
  glassSunAzimuth = std::abs(glassSunAzimuth);                     // take absolute value

  // calculate the direct irradiation on the glass
  double irradDirectGlass = 0.0;             // direct irradiation on glass
  // check for sun up
  if ( d_zenithAngle < 90.0 )                // CAUTION: must be strict less-than [1]
    {
      // check for direct sunlight
      if ( glassSunAzimuth < 90.0 )
        {
          // direct solar gain, note that zenith angle = 90.0 - elevation
          irradDirectGlass
            = d_solarDirect
            * std::sin(d_zenithAngle) * std::cos(glassSunAzimuth);
        }
    }

  // [1] some datasets (NIWA TMY, for instance) truncate on [0,90]
  // so that midnight will record as 90 degrees

  // ---------------------------------
  //  solar gain
  // ---------------------------------

  double glassDirectGain = 0.0;
  double glassDifuseGain = 0.0;

  // check for sun up
  if ( d_zenithAngle < 90.0 )
    {
      // direct solar gain, noting 'irradDirectGlass' can be zero
      glassDirectGain = glazingTransDirect * irradDirectGlass * glassArea;
      // indirect solar gain
#if 1 // 1 = my correction: only half the sky is visible, 0 = as per Chadderton (1989a)
      glassDifuseGain = 0.5 * glazingTransDifuse * d_solarDifuse * glassArea;
#else
      glassDifuseGain = 1.0 * glazingTransDifuse * d_solarDifuse * glassArea;
#endif // 0
      // transfer based on solar gain
      glassGain = glassDirectGain + glassDifuseGain;
    }

  // ---------------------------------
  //  absorption, conductive transfer
  // ---------------------------------

  double irradTotalGlass = 0.0;              // total irradiation on vertical surface
  // check for sun up
  if ( d_zenithAngle < 90.0 )
    {
      // total irradiation on glass surface, noting 'irradDirectGlass' can be zero
      const double irradTotalHoriz
        = d_solarDirect * std::cos(d_zenithAngle)           // angular effects
        + 1.0 * d_solarDifuse;                              // no angular effects
      irradTotalGlass
        = irradDirectGlass
        + 0.5 * d_solarDifuse                               // half the sky is visible
        + 0.5 * groundReflectFactor * irradTotalHoriz;      // reflected contribution
    }

  // glass temperature based on energy balance
  const double tempGlass
    = (glazingAbsorptance * irradTotalGlass
       + glazingSurfaceHeatTferCoeffOutside * d_tempAirOutside
       + glazingSurfaceHeatTferCoeffInside  * d_tempSetPoint)
    / (glazingSurfaceHeatTferCoeffOutside + glazingSurfaceHeatTferCoeffInside);

  // conductive transfer
  glassCond
    = glazingSurfaceHeatTferCoeffInside * (tempGlass - d_tempSetPoint)
    + glazingTransGeneral * irradTotalGlass;

  // ---------------------------------
  //  total transfer
  // ---------------------------------

  glassTransfer = glassGain + glassCond;

  // ---------------------------------
  //  yeek reporting
  // ---------------------------------

  // additional reporting as appropriate
  // YEEK 39 CODE (set by '--yeek')
  if ( xeona::yeek == 39 || xeona::yeek == 1 || xeona::yeek == 2 )
    {
      std::ostringstream put;
      put << "  individual glazing element calculations"                        << "\n"
          << "    inputs:"                                                      << "\n"
          << "      window orientation [degrees] : " << windowOrientation       << "\n"
          << "      glass area [m2]              : " << glassArea               << "\n"
          << "      ground reflection factor [-] : " << groundReflectFactor     << "\n"
          << "    outputs:"                                                     << "\n"
          << "      direct gain [W]              : " << glassDirectGain         << "\n"
          << "      diffuse gain [W]             : " << glassDifuseGain         << "\n"
          << "      absorption gain [W]          : " << glassCond               << "\n"
          << "      total transfer [W]           : " << glassTransfer           << "\n";
      s_logger->repx(logga::dbug, "additional reporting follows, yeek", xeona::yeek);
      s_logger->putx(logga::dbug, put);
    }

  // ---------------------------------
  //  load the report buffer
  // ---------------------------------

  d_oss << "\n"
        << "    orientation [degrees]            : " << windowOrientation         << "\n"
        << "    direct gain [W]                  : " << glassDirectGain           << "\n"
        << "    diffuse gain [W]                 : " << glassDifuseGain           << "\n"
        << "    absorption [W] (inward)          : " << glassCond                 << "\n";

  // ---------------------------------
  //  housekeeping
  // ---------------------------------

  return glassTransfer;

} // function 'simulateGlazing'

// ---------------------------------------------------------
//  MEMBER FUNCTION : calcOpaqueLoad
// ---------------------------------------------------------
//  Description  : define opaque envelope and calculate heat flows
//  Role         : called by 'calculate'
//  Techniques   :
//  Status       : complete
//
//  Design notes
//
//      The CIBSE method is an approximation to full dynamic
//      analysis.  It estimates and then adds the mean and cyclic
//      components.
//
//      The method requires a knowledge of 24 hour means for the
//      sol-air temperature and the indoor temperature.  This
//      information is loaded, calculated, and stored here by
//      'simulateOpaqueElems'.
//
//      Glass can also behave as an opaque element.  In this
//      case, f = 1, lag = 0, and the outside temperature is the
//      actual temperature and not the sol-air temperature.
//
//  Notation
//
//      Q_u    opaqueCondMean        [W]      mean conduction heat gain
//      Q_u~   opaqueCondCyclic      [W]      cyclic variation conduction heat gain
//      A      area                  [m2]     surface area
//      U      uvalue                [W/m2K]  thermal transmittance (U-value)
//      Y      yvalue                [W/m2K]  thermal admittance (Y-value)
//
//      V      internalVolume        [m3]     room volume
//      N      airChanges            [1/h]    room air changes per hour
//
//      t_c    d_tempSetPointDayMean [C]      24 hour mean indoor temperature
//      t_eo   d_tempSolAirDayMean   [C]      24 hour mean sol-air temperature
//
//      F_u    factorTrans           [-]      thermal transmittance factor
//      F_y    factorAdmit           [-]      thermal admittance factor
//      F_v    factorVent            [-]      ventilation factor
//      F_2    factorLoss            [-]      heat loss factor, subscript 2
//
//      f      decf                  [-]      decrement factor
//      Lag    lag                   [h]      time lag
//
//      sumA   sumA                  [m2]     room surface areas
//      sumAU  sumAU                 [W/K]
//      sumAY  sumAY                 [W/K]
//
// ---------------------------------------------------------

double
BuildingSim::calcOpaqueLoad()
{
  // ---------------------------------
  //  element specifications
  // ---------------------------------

  // declarations
  std::vector<element_type> elements;

  // element areas
  const double perimeter    = 2.0 * d_shortDimension * (1.0 + d_floorAspectRatio);
  const double verticalArea = perimeter * d_wallHeight;     // intermediate value
  const double wallArea     = verticalArea * (1.0 - d_windowToWall);
  const double windowArea   = verticalArea * d_windowToWall;
  const double roofArea     = d_shortDimension * d_shortDimension * d_floorAspectRatio;

  // interior volume
  const double internalVolume = verticalArea * d_wallHeight;

  // load elements
  elements.push_back(boost::make_tuple(d_fabricData->walls.kind,
                                       wallArea,
                                       d_fabricData->walls.uvalue,
                                       d_fabricData->walls.yvalue,
                                       d_fabricData->walls.decf,
                                       d_fabricData->walls.lag,
                                       d_fabricData->walls.note));
  elements.push_back(boost::make_tuple(d_fabricData->roof.kind,
                                       roofArea,
                                       d_fabricData->roof.uvalue,
                                       d_fabricData->roof.yvalue,
                                       d_fabricData->roof.decf,
                                       d_fabricData->roof.lag,
                                       d_fabricData->roof.note));
  elements.push_back(boost::make_tuple(d_fabricData->windows.kind,
                                       windowArea,
                                       d_fabricData->windows.uvalue,
                                       d_fabricData->windows.yvalue,
                                       d_fabricData->windows.decf,
                                       d_fabricData->windows.lag,
                                       d_fabricData->windows.note));

  // ---------------------------------
  //  simulation call
  // ---------------------------------

  // load the report buffer
  d_oss << "\n"
        << "  opaque element calculations:"                                       << "\n";

  const double totalLoad = simulateOpaqueElems(elements,
                                               internalVolume,
                                               d_airChangesPerHour);

  // ---------------------------------
  //  yeek reporting
  // ---------------------------------

  // additional reporting as appropriate
  // YEEK 39 CODE (set by '--yeek')
  if ( xeona::yeek == 39 || xeona::yeek == 1 || xeona::yeek == 2 )
    {
      const int elementCount = elements.size();
      std::ostringstream put;
      put << "  aggregate opaque elements calculations"     << "\n"
          << "    elements count  : " << elementCount       << "\n"
          << "    total loads [W] : " << totalLoad          << "\n";
      s_logger->repx(logga::dbug, "additional reporting follows, yeek", xeona::yeek);
      s_logger->putx(logga::dbug, put);
    }

  // ---------------------------------
  //  housekeeping
  // ---------------------------------

  // load the report buffer
  d_oss << "\n"
        << "    total opaque gain [W]            : " << totalLoad                 << "\n";

  return totalLoad;
}

// ---------------------------------------------------------
//  MEMBER FUNCTION : simulateOpaqueElems
// ---------------------------------------------------------

double
BuildingSim::simulateOpaqueElems
(const std::vector<element_type> elements,
 const double                    internalVolume,
 const double                    airChanges)
{
  // ---------------------------------
  //  variables
  // ---------------------------------

  double summedTotalGain = 0.0;              // sum of all opaque element gains

  // ---------------------------------
  //  loop 1 : summations
  // ---------------------------------

  double sumA  = 0.0;
  double sumAU = 0.0;
  double sumAY = 0.0;

  BOOST_FOREACH( element_type e, elements )
    {
      const double area   = e.get<1>();
      const double uValue = e.get<2>();
      const double yValue = e.get<3>();

      // calculate summed variables
      sumA  += area;
      sumAU += area * uValue;
      sumAY += area * yValue;

    } // element loop 1

  // ---------------------------------
  //  factor equations
  // ---------------------------------

  const double factorTrans = 18 * sumA / (18 * sumA + sumAU);
  const double factorAdmit = 18 * sumA / (18 * sumA + sumAY);
  const double factorVent  =  6 * sumA / ( 6 * sumA + 0.33 * airChanges * internalVolume);
  const double factorLoss  = 1.0 + (factorTrans * sumAU / (6 * sumA));

  // ---------------------------------
  //  yeek reporting
  // ---------------------------------

  // additional reporting as appropriate
  // YEEK 39 CODE (set by '--yeek')
  if ( xeona::yeek == 39 || xeona::yeek == 1 || xeona::yeek == 2 )
    {
      std::ostringstream put;
      put << "  factors for opaque calculations"            << "\n"
          << "    sum A [m2]            : " << sumA         << "\n"
          << "    sum AU                : " << sumAU        << "\n"
          << "    sum AY                : " << sumAY        << "\n"
          << "    transmittance factor  : " << factorTrans  << "\n"
          << "    admittance factor     : " << factorAdmit  << "\n"
          << "    ventilation factor    : " << factorVent   << "\n"
          << "    loss factor           : " << factorLoss   << "\n";
      s_logger->repx(logga::dbug, "additional reporting follows, yeek", xeona::yeek);
      s_logger->putx(logga::dbug, put);
    }

  // ---------------------------------
  //  loop 2 : mean and cyclic flows
  // ---------------------------------

  BOOST_FOREACH( element_type e, elements )
    {
      const elementKind elkind = e.get<0>();
      const double      area   = e.get<1>();
      const double      uvalue = e.get<2>();
      const double      yvalue = e.get<3>();
      const double      decf   = e.get<4>();
      const unsigned    lag    = e.get<5>();
      const std::string note   = e.get<6>();

      // ---------------------------------
      //  mean and cyclic components
      // ---------------------------------

      double meanGain        = 0.0;
      double cyclicGain      = 0.0;
      double totalGain       = 0.0;

      double swing           = 0.0;
      double tempMeanDelta   = 0.0;
      double tempCyclicDelta = 0.0;

      // lag values above 11 [h] are not supported by this code
      const unsigned swinglen = d_tempSolAirSwings12.size();
      if ( lag < swinglen )
        {
          swing = d_tempSolAirSwings12.at(lag);
        }
      else
        {
          std::ostringstream oss;
          oss << lag << " : " << swinglen;
          s_logger->repx(logga::warn, "out of range lag value", oss.str());
          s_logger->repx(logga::warn, "current calculation will be faulty", "");
        }

      // determine temperature deltas
      switch ( elkind )
        {
        case BuildingSim::solid:
          tempMeanDelta   = d_tempSolAirDayMean - d_tempSetPointDayMean;
          tempCyclicDelta = swing;
          break;
        case BuildingSim::glass:             // opaque behavior
          tempMeanDelta   = d_tempAirOutside - d_tempSetPointDayMean;
          tempCyclicDelta = d_tempAirOutside - d_tempAirOutsideDayMean;    // lag = 0
          // integrity checks
          if ( lag  != 0.0 ) s_logger->repx(logga::warn, "lag not zero",   lag);
          if ( decf != 1.0 ) s_logger->repx(logga::warn, "decf not unity", decf);
          break;
        default:
          s_logger->repx(logga::warn, "coding error, unsupported elkind", elkind);
        }

      // gain calculations
      meanGain   = (factorTrans / factorVent) * area * uvalue * tempMeanDelta;
      cyclicGain = (factorAdmit / factorVent) * area * uvalue * decf * tempCyclicDelta;

      // aggregate

      totalGain        = meanGain + cyclicGain;
      summedTotalGain += totalGain;

      // ---------------------------------
      //  yeek reporting
      // ---------------------------------

      // additional reporting as appropriate
      // YEEK 39 CODE (set by '--yeek')
      if ( xeona::yeek == 39 || xeona::yeek == 1 || xeona::yeek == 2 )
        {
          std::string elstr;
          switch ( elkind )
            {
            case BuildingSim::solid: elstr = "solid";     break;
            case BuildingSim::glass: elstr = "glass";     break;
            default:                 elstr = "(not set)"; break;
            }
          std::ostringstream put;
          put << "  individual opaque element calculations"                 << "\n"
              << "    inputs:"                                              << "\n"
              << "      building element          : " << note               << "\n"
              << "      element type              : " << elstr              << "\n"
              << "      area [m2]                 : " << area               << "\n"
              << "      U-value [W/m2K]           : " << uvalue             << "\n"
              << "      Y-value [W/m2K]           : " << yvalue             << "\n"
              << "      decrement factor (f) [-]  : " << decf               << "\n"
              << "      time lag [h]              : " << lag                << "\n"
              << "      sol-air temp swing [C]    : " << swing              << "\n"
              << "    outputs:"                                             << "\n"
              << "      mean gain [W]             : " << meanGain           << "\n"
              << "      cyclic gain [W]           : " << cyclicGain         << "\n"
              << "      total gain [W]            : " << totalGain          << "\n";
          s_logger->repx(logga::dbug, "additional reporting follows, yeek", xeona::yeek);
          s_logger->putx(logga::dbug, put);
        }

      // ---------------------------------
      //  load the report buffer
      // ---------------------------------

      d_oss << "\n"
            << "    building element                 : " << note                  << "\n"
            << "    mean + cyclic gain [W]           : "
            << meanGain << " + " << cyclicGain                                    << "\n"
            << "    total gain [W]                   : " << totalGain             << "\n";

    } // element loop 2

  // ---------------------------------
  //  infiltration gains
  // ---------------------------------

  const double tempDelta = d_tempAirOutside - d_tempSetPoint;
  const double airGain   = 0.33 * factorLoss * airChanges * internalVolume * tempDelta;

  // load the report buffer
  d_oss << "\n"
        << "    infiltration gain [W]            : " << airGain                   << "\n";

  // ---------------------------------
  //  final tally
  // ---------------------------------

  summedTotalGain += airGain;

  return summedTotalGain;

} // function 'simulateOpaqueElems'

// ---------------------------------------------------------
//  MEMBER FUNCTION : calcInternalLoad
// ---------------------------------------------------------
//  Description  : calculate internal heat sources
//  Role         : called by 'calculate'
//  Techniques   : simple tally
//  Status       : complete
//
//  Design notes
//
//      This is just a simple first law tally.
//
// ---------------------------------------------------------

double
BuildingSim::calcInternalLoad()
{
  const double internalLoad = d_activityLoad + d_electricityDemand;
  return internalLoad;
}

// ---------------------------------------------------------
//  MEMBER FUNCTION : calcHvac
// ---------------------------------------------------------
//  Description  : calculate HVAC usage
//  Role         : called by 'calculate'
//  Techniques   : 'd_hysteresis'
//  Status       : complete
//
//  Design notes
//
//      The HVAC operation decision is normally determined using
//      the sol-air balance temperature.  However the thermal
//      load on the building has already been calculated and that
//      information is more easily used.  Refer to the balance
//      temperature plot in Chadderton (1989a fig3.17 p111) for a
//      rationale for this choice.
//
//      The 'load' argument is signed: positive indicates
//      chilling and negative indicates heating.
//
//      The 'run' argument determines whether the HVAC plant can
//      be run or not and is indirectly set by the client via the
//      'd_runStatus' variable.  Moreover, the client may well be
//      using the 'OpsScheduler' class to specify a run schedule
//      and thus determine operations.
//
//      Resistance heating is used for heating here -- but it
//      would not be difficult to run the chiller in reverse as a
//      heat-pump.
//
//      The chiller-mode duct loss factor 'd_ductLossFactor'
//      covers ancillary equipment demand, including duct fans.
//
//      The heat exchangers in HVAC systems are know as "air
//      coils" and can be sized using the LMTD (log mean
//      temperature difference) method.  For a given thermal
//      duty, the LMTD depends on the exchanger area and heat
//      transfer coefficient -- the latter depending further on
//      the flow regime and coil and fin geometry.  In this code,
//      the LMTD is set at 7C -- based on a worked example from
//      Haines and Wilson (2003, p310).
//
//      Relative hysteresis (perhaps set at 10%) can be applied
//      by the modeler.  Hysteresis helps prevent the HVAC unit
//      from alternating between heating and chilling for no real
//      gain.
//
//      Thermal load carryover is also supported, which allows
//      for slippage in extreme conditions and hence a reduced
//      installed capacity.  If the carryover exceeds a
//      predefined trip, then a warning is logged so that the
//      modeler can resize the HVAC plant.
//
//  REFERENCES
//
//      Haines, Roger W and C Lewis Wilson.  2003.  HVAC systems
//        design handbook -- Third edition.  McGraw-Hill, New
//        York, USA.  ISBN-10 0-07-139586-5.
//
// ---------------------------------------------------------

double
BuildingSim::calcHvac
(const double hvacLoadAsk,                   // as requested, +ve chilling, -ve heating
 const bool   runStatus)                     // can be 'false', say outside business hours
{
  // initial reporting
  const std::string runStatusText = ( runStatus == true ) ? "(will try)" : "(cannot try)";
  std::ostringstream oss;
  oss << hvacLoadAsk << " [W] " << runStatusText;
  s_logger->repx(logga::adhc, "entering member function, request", oss.str());

  // overwrite state temporaries from stored state
  d_hvacLoadCarryOverTmp = d_hvacLoadCarryOver;

  // internal variables
  double cop               = 0.0;            // heater or chiller unit CoP
  double efy               = 0.0;            // HVAC system efficiency
  double electricityDemand = 0.0;            // current electricity demand, also returned
  double cumulativeLoad    = 0.0;            // includes carryover
  std::string currentRole  = "(not set)";

  // zero the duty variables
  d_chillerDuty = 0.0;
  d_heaterDuty  = 0.0;

  // stringify the 'runStatus' argument for reporting purposes
  const std::string strStatus = ( runStatus == true ) ? "available" : "unavailable";

  // amend the current load in relation to any backlog
  cumulativeLoad = hvacLoadAsk + d_hvacLoadCarryOverTmp;

  // denormalize the hysteresis inputs -- used to reduce toggling behavior
  const double chillerHysteresis = d_chillerCapacity * d_chillerHysteresis;
  const double heaterHysteresis  = d_heaterCapacity  * d_heaterHysteresis;

  // ---------------------------------
  //  plant non-operative
  // ---------------------------------

  // some HVAC plant only run during pre-specified hours, say 08:00 thru 18:00

  if ( runStatus == false )                  // "non-operative" (different from "idle")
    {
      currentRole            = "non-operative";
      d_hvacLoadCarryOverTmp = cumulativeLoad;

      // load the report buffer
      d_oss << "\n"
            << "  HVAC performance (chiller load +ve):"                           << "\n"
            << ""                                                                 << "\n"
            << "    current plant status             : " << strStatus             << "\n"
            << "    current plant role               : " << currentRole           << "\n"
            << std::showpos
            << "    current thermal load [W]         : " << cumulativeLoad        << "\n"
            << "    HVAC carryover [W]               : " << d_hvacLoadCarryOverTmp<< "\n"
            << std::noshowpos
            << "    HVAC electricity demand [W]      : " << electricityDemand     << "\n";
    }

  // ---------------------------------
  //  chilling role
  // ---------------------------------

  else if ( +cumulativeLoad > chillerHysteresis )
    {
      // account for air-to-air heat exchanger attributes
      double lo = d_tempSetPoint   - d_airHxLmtd;
      double hi = d_tempAirOutside + d_airHxLmtd;

      // some temperature bounds are inviolable
      if ( lo < 5.0 )
        {
          currentRole = "R-134a operating temperature violated";
          s_logger->repx(logga::info, "R-134a evaporator temp too low", lo);
        }
      if ( hi > 50.0 )
        {
          currentRole = "R-134a operating temperature violated";
          s_logger->repx(logga::info, "R-134a condenser temp too high", hi);
        }

      // some temperature bounds can be "morphed" by increasing the ratio of
      // by-pass air -- these tests are also mutually exclusive to the above screen
      if ( lo > 15.0 ) lo = 15.0;
      if ( hi < 25.0 ) hi = 25.0;

      // chiller calculations
      cop = d_chiller->coeffOfPerformance(lo, hi);     // cold [5,15], hot [25,50]
      efy = cop * (1.0 - d_ductLossFactor);            // include non-chiller losses

      // capacity and carryover calculations
      if ( +cumulativeLoad > d_chillerCapacity )
        {
          // insufficient capacity
          currentRole            = "chiller at full load";
          d_hvacLoadCarryOverTmp = cumulativeLoad - d_chillerCapacity;
          electricityDemand      = d_chillerCapacity / efy;
          d_chillerDuty          = d_chillerCapacity;
        }
      else
        {
          // sufficient capacity
          currentRole            = "chiller at part load";
          d_hvacLoadCarryOverTmp = 0.0;
          electricityDemand      = +cumulativeLoad / efy;
          d_chillerDuty          = +cumulativeLoad;
        }

      // backlog warning
      const double carryOverFactor = d_hvacLoadCarryOverTmp / d_chillerCapacity;
      if ( carryOverFactor > d_chillerCarryOverTrip )
        {
          s_logger->repx(logga::warn,
                         "chiller carryover excessive, factor",
                         carryOverFactor);
        }

      // load the report buffer
      d_oss << "\n"
            << "  HVAC performance (chiller load +ve):"                           << "\n"
            << ""                                                                 << "\n"
            << "    current plant status             : " << strStatus             << "\n"
            << "    current plant role               : " << currentRole           << "\n"
            << std::showpos
            << "    chiller thermal capacity [W]     : " << d_chillerCapacity     << "\n"
            << "    chiller absolute hysteresis [W]  : " << chillerHysteresis     << "\n"
            << "    current thermal load [W]         : " << cumulativeLoad        << "\n"
            << "    HVAC carryover [W]               : " << d_hvacLoadCarryOverTmp<< "\n"
            << std::noshowpos
            << "    HVAC chiller carryover trip [-]  : " << d_chillerCarryOverTrip<< "\n"
            << "    chiller temperatures [C]         : " << lo  << "  " << hi     << "\n"
            << "    chiller COP, system effy [-]     : " << cop << "  " << efy    << "\n"
            << "    HVAC electricity demand [W]      : " << electricityDemand     << "\n";
    }

  // ---------------------------------
  //  heating role
  // ---------------------------------

  else if ( -cumulativeLoad > heaterHysteresis )
    {
      // heater calculations
      cop = 1.0;                                       // resistance heating
      efy = cop;                                       // no general losses

      // capacity and carryover calculations
      if ( -cumulativeLoad > d_heaterCapacity )
        {
          // insufficient capacity
          currentRole            = "heater at full load";
          d_hvacLoadCarryOverTmp = cumulativeLoad + d_heaterCapacity;
          electricityDemand      = d_heaterCapacity / efy;
          d_heaterDuty           = d_heaterCapacity;
        }
      else
        {
          // sufficient capacity
          currentRole            = "heater at part load";
          d_hvacLoadCarryOverTmp = 0.0;
          electricityDemand      = -cumulativeLoad / efy;
          d_heaterDuty           = -cumulativeLoad;
        }

      // backlog warning
      const double carryOverFactor = d_hvacLoadCarryOverTmp / d_heaterCapacity;
      if ( carryOverFactor > d_heaterCarryOverTrip )
        {
          s_logger->repx(logga::warn,
                         "heater carryover excessive, factor",
                         carryOverFactor);
        }

      // load the report buffer
      d_oss << "\n"
            << "  HVAC performance (heater load -ve):"                            << "\n"
            << ""                                                                 << "\n"
            << "    current plant status             : " << strStatus             << "\n"
            << "    current plant role               : " << currentRole           << "\n"
            << std::showpos
            << "    heater thermal capacity [W]      : " << d_heaterCapacity      << "\n"
            << "    heater absolute hysteresis [W]   : " << heaterHysteresis      << "\n"
            << "    current thermal load [W]         : " << cumulativeLoad        << "\n"
            << "    HVAC carryover [W]               : " << d_hvacLoadCarryOverTmp<< "\n"
            << std::noshowpos
            << "    HVAC heater carryover trip [-]   : " << d_heaterCarryOverTrip << "\n"
            << "    HVAC electricity demand [W]      : " << electricityDemand     << "\n";
    }

  // ---------------------------------
  //  plant is available but idle
  // ---------------------------------

  else
    {
      // update information
      currentRole       = "idle";
      electricityDemand = 0.0;

      // update carryover
      d_hvacLoadCarryOverTmp += hvacLoadAsk; // non-zero hysteresis makes this necessary

      // load the report buffer
      d_oss << "\n"
            << "  HVAC performance:"                                              << "\n"
            << ""                                                                 << "\n"
            << "    current plant status             : " << strStatus             << "\n"
            << "    current plant role               : " << currentRole           << "\n"
            << std::showpos
            << "    current thermal load [W]         : " << cumulativeLoad        << "\n"
            << "    HVAC carryover [W]               : " << d_hvacLoadCarryOverTmp<< "\n"
            << std::noshowpos
            << "    HVAC electricity demand [W]      : " << electricityDemand     << "\n";
    }

  // ---------------------------------
  //  housekeeping
  // ---------------------------------

  // return
  return electricityDemand;
}

// ---------------------------------------------------------
//  MEMBER FUNCTION : calcTempSolAir
// ---------------------------------------------------------
//  Description  : calculate the so-called sol-air temperature
//  Role         : internal
//  Techniques   : sol-air temperature definition
//  Status       : complete
//
//  Design notes
//
//      The sol-air temperature can, on a sunny day for instance,
//      boost an outside air temperature of 19 degrees to 67
//      degrees.
//
// ---------------------------------------------------------

double
BuildingSim::calcTempSolAir                            // wrapper version
(const double                 tempAirOutside,
 boost::tuple<double, double> solarIrradiation,
 boost::tuple<double, double> sunAngle,
 const double                 longwaveRadiation,       // [1], note default
 const double                 thermResistSurf) const   // [2], note default
{
  return calcTempSolAir(tempAirOutside,           // main call
                        solarIrradiation.get<0>(),
                        solarIrradiation.get<1>(),
                        sunAngle.get<0>(),
                        longwaveRadiation,
                        thermResistSurf);
}

double
BuildingSim::calcTempSolAir                  // main version
(const double tempAirOutside,
 const double solarDirect,
 const double solarDifuse,
 const double zenithAngle,
 const double longwaveRadiation,             // [1], note default in header
 const double thermResistSurf) const         // [2], note default in header
{
  // [1] long-wave radiation from a surface, mostly a property of the surface [W/m2]
  // [2] external surface film thermal resistance [m2K/W]

  // hard-coded parameters
  const double absorptivity = 0.9;           // external surface absorptivity [-]
  const double emissivity   = 0.9;           // external surface emissivity [-]

  // calculate
  const double irradTotalHoriz
    = solarDirect * std::cos(zenithAngle)    // angular effects
    + 1.0 * solarDifuse;                     // no angular effects
  const double tempSolAir
    = tempAirOutside
    + thermResistSurf * (absorptivity * irradTotalHoriz - emissivity * longwaveRadiation);

  // return
  return tempSolAir;
}

std::string
BuildingSim::interpretCallTrack
(const int callTrack) const
{
  switch ( callTrack )
    {
    case  0: return "no calls made";
    case  1: return "building data";
    case  3: return "building and fabric data";
    case  7: return "building, fabric, and ambient data";
    case 15: return "building, fabric, ambient, and occupant data";
    case 31: return "simulated";
    default: return "out of order calls detected";
    }
}

//  end of file

