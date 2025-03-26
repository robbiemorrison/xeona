//  XEONA ENERGY SYSTEMS MODELING ENVIRONMENT
//
//  file-create-name : teas05.cc
//  file-create-date : Thu 21-Oct-2010 22:17 UTC
//  file-initiator   : Robbie Morrison <robbie@actrix.co.nz>
//
//  file-role        : concrete technical assets 5 / implementation
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
//  $URL: file:///home/robbie/svn-root/xeona/futz/trunk/xeona1/b/teas05.cc $
//
//  GENERAL NOTES FOR THIS FILE

//  LOCAL AND SYSTEM INCLUDES

#include "teas05.h"           // companion header for this file (place first)

#include "../h/thrmperf.h"    // building thermal performance and HVAC model"
#include "../e/cxamb02.h"     // concrete ambient conditions contexts 2
#include "../e/cxamb01.h"     // concrete ambient conditions contexts 1
#include "../c/yearcalc.h"    // free functions for calendar calculations
#include "../c/stats.h"       // on-the-fly statistical calculations
#include "../c/recset.h"      // records and fields and also record-sets
#include "../c/label.h"       // helper class to format solver labels
#include "../c/conex.h"       // create and connect block interfaces
#include "../c/buildinfo.h"   // building asset to simulation data transfer
#include "../b/optops1.h"     // operate optimization sub-problems for hard assets 1
#include "../b/commods.h"     // commodities hierarchy
#include "../a/exent.h"       // entity exception classes

#include "../a/logger.h"      // standard logging functionality (as required)
#include "../c/smart_ptr.h"   // toggle between Boost and TR1 smart pointers
#include ".././common.h"      // common definitions for project (place last)

#include <limits>             // numeric_limits<T>::infinity() and similar
#include <string>             // C++ strings
#include <sstream>            // string-streams
#include <utility>            // STL pair, make_pair()

#include <boost/format.hpp>   // printf style formatting

//  CODE

// ---------------------------------------------------------
//  FREE FUNCTION   : ::hvacDuty
// ---------------------------------------------------------
//  Description  : returns the HVAC duty, making heating negative
//  Role         : support for 'TeasBuildingElec::washup'
//  Techniques   : returns NaN on both services running simultaneously
//  Status       : complete
// ---------------------------------------------------------

namespace
{
  double
  hvacDuty
  (const double chillerDuty,
   const double heaterDuty)
  {
    static const double nan = std::numeric_limits<double>::quiet_NaN();
    if ( chillerDuty > 0.0 && heaterDuty > 0.0 ) return nan;     // should never run
    else if ( chillerDuty > 0.0 ) return chillerDuty;
    else if ( heaterDuty  > 0.0 ) return -heaterDuty;  // CAUTION: note unary minus
    else  return 0.0;
  }
}

// ---------------------------------------------------------
//  FREE FUNCTION   : ::hourOfDay
// ---------------------------------------------------------
//  Description  : returns hour of day in "hh::mm:ss.ss" format
//  Role         : reporting purposes
//  Techniques   : 'xeona::hms' (takes seconds)
//  Status       : complete
// ---------------------------------------------------------

namespace
{
  std::string
  hourOfDay
  (const int step)
  {
    // static data
    static const int intsPerDay  = Entity::getHorizonIntsPerDay();  // ranging 1 thru 288
    static const int offsetSteps = Entity::getHorizonOffset();      // measured in "steps"

    // modular arithmetic -- first work in steps, then decimal hours, then decimal seconds
    const int    stepOfDay = (step + offsetSteps) % intsPerDay;
    const double hourOfDay = stepOfDay * (24.0 / static_cast<double>(intsPerDay));
    const double secsOfDay = hourOfDay * 3600;

    // stringify in "hh:mm:ss.ss" format and return
    const std::string hms = xeona::hms(secsOfDay);
    return hms;

  } // function '::hourOfDay'
} // unnamed namespace

// ---------------------------------------------------------
//  FREE FUNCTION   : ::loadFabricData
// ---------------------------------------------------------
//  Description  : load hardcoded building fabric datasets
//  Role         : support 'TeasBuildingElec::establish'
//  Techniques   : std::map, pass-by-reference, class 'BuildingFabricData'
//  Status       : complete
//
//  Design notes
//
//      Add new building fabric datasets here.
//
//      This function could migrate to either subdirectory 'g' or
//      'h' in due course.
//
// ---------------------------------------------------------

namespace
{
  int
  loadFabricData
  (std::map<std::string, shared_ptr<BuildingFabricData> >& fabricDatas)
  {
    // bind logger
    static logga::spLogger logger = logga::ptrLogStream();

    // initial reporting
    logger->repx(logga::adhc, "entering free function", "");

    // integrity check
    if ( ! fabricDatas.empty() )
      {
        logger->repx(logga::warn, "expected an empty map", "");
      }

    // ---------------------------------
    //  create fabric data
    // ---------------------------------

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

    // loading 1

    // most data from Chadderton (1989a, tables 3.2 and 3.3 and example 3.13)

    const std::string key1 = "average";
    const std::string ann1 = "average fabric, 4mm single glazing";
    shared_ptr<BuildingFabricData> fabricData1(new BuildingFabricData(ann1));

    fabricData1->glazing.transDirect                 = 0.89;
    fabricData1->glazing.transDifuse                 = 0.82;
    fabricData1->glazing.absorptance                 = 0.11;
    fabricData1->glazing.transGeneral                = 0.86;

    fabricData1->glazing.surfaceHeatTferCoeffOutside = 16.7;
    fabricData1->glazing.surfaceHeatTferCoeffInside  =  8.3;

    fabricData1->walls.uvalue                        = 0.33;
    fabricData1->walls.yvalue                        = 2.40;
    fabricData1->walls.decf                          = 0.35;
    fabricData1->walls.lag                           = 9;

    fabricData1->roof.uvalue                         = 0.40;
    fabricData1->roof.yvalue                         = 0.70;
    fabricData1->roof.decf                           = 0.99;
    fabricData1->roof.lag                            = 1;

    fabricData1->windows.uvalue                      = 5.90;
    fabricData1->windows.yvalue                      = 5.90;
    fabricData1->windows.decf                        = 1.00;
    fabricData1->windows.lag                         = 0;

    fabricData1->setComplete();

    fabricDatas.insert(std::make_pair(key1, fabricData1));

    // loading 2

    const std::string key2 = "good";
    const std::string ann2 = "better fabric, coated double glazing";
    shared_ptr<BuildingFabricData> fabricData2(new BuildingFabricData(ann2));

    fabricData2->glazing.transDirect                 = 0.85;
    fabricData2->glazing.transDifuse                 = 0.80;
    fabricData2->glazing.absorptance                 = 0.15;
    fabricData2->glazing.transGeneral                = 0.78;

    fabricData2->glazing.surfaceHeatTferCoeffOutside = 16.7;
    fabricData2->glazing.surfaceHeatTferCoeffInside  =  8.3;

    fabricData2->walls.uvalue                        = 0.20;
    fabricData2->walls.yvalue                        = 2.80;
    fabricData2->walls.decf                          = 0.30;
    fabricData2->walls.lag                           = 9;

    fabricData2->roof.uvalue                         = 0.30;
    fabricData2->roof.yvalue                         = 0.90;
    fabricData2->roof.decf                           = 0.90;
    fabricData2->roof.lag                            = 3;

    fabricData2->windows.uvalue                      = 1.60;
    fabricData2->windows.yvalue                      = 1.60;
    fabricData2->windows.decf                        = 1.00;
    fabricData2->windows.lag                         = 0;

    fabricData2->setComplete();

    fabricDatas.insert(std::make_pair(key2, fabricData2));

    // housekeeping
    const int size = fabricDatas.size();
    logger->repx(logga::adhc, "leaving free function, fabric sets", size);
    return size;

  } // function '::loadFabricData'
} // unnamed namespace

// ---------------------------------------------------------
//  CLASS           : TeasBuildingElec
// ---------------------------------------------------------

// ---------------------------------------------------------
//  MEMBER FUNCTION : TeasBuildingElec
// ---------------------------------------------------------

TeasBuildingElec::TeasBuildingElec
(const std::string entityId,
 Record&           record) :
  CostRegister(record),
  TechnicalAsset(entityId, record, xeona::e_commitmentModes),
  CostRegisterSRFin(record),
  CostRegisterEmbFin(record),
  d_ambientAirContext(record.lazyLink<CxAmbientAir>("ambient-air-context")),
  d_ambientSolarContext(record.lazyLink<CxAmbientSolar>("ambient-solar-context")),
  d_count(record.tieSingle<int>("count")),
  d_demandHiBound(record.tieSingle<double>("demand-hi-bound")),
  d_orientation(record.tieSingle<double>("orientation")),
  d_floorArea(record.tieSingle<double>("floor-area")),
  d_floorAspectRatio(record.tieSingle<double>("floor-aspect-ratio")),
  d_wallHeight(record.tieSingle<double>("wall-height")),
  d_windowToWallRatio(record.tieSingle<double>("window-to-wall-ratio")),
  d_constructionType(record.tieSingle<std::string>("construction-type")),
  d_chillerCapacity(record.tieSingle<double>("chiller-thermal-capacity")),
  d_heaterCapacity(record.tieSingle<double>("heater-thermal-capacity")),
  d_hvacOpsStart(record.tieSingle<int>("hvac-ops-start")),
  d_hvacOpsEnd(record.tieSingle<int>("hvac-ops-end")),
  d_hvacBacklogTrip(record.tieSingle<double>("hvac-backlog-trip")),
  d_totalCapacity(record.tieSingle<double>("total-capacity")),
  d_demands(record.tieTimeseries<double>("demands")),
  d_hvacDutys(record.tieTimeseries<double>("hvac-dutys")),
  d_floorPerformance(record.tieSingle<double>("floor-performance")),
  d_chillerMaxDuty(record.tieSingle<double>("chiller-max-duty")),
  d_heaterMaxDuty(record.tieSingle<double>("heater-max-duty")),
  d_inElec(Cable<CmElectricity>::create
           (entityId,                        // me
            record.tieSingle<std::string>("socket-1"),
            record.tieSingle<std::string>("cable-electricity-commodity"))),
  d_temperatureSetPoint(0.0),
  d_activityLoad(0.0),
  d_electricityDemand(0.0),
  d_fabricData(),                            // empty shared pointer
  d_fabricDatas(),                           // empty map
  d_timerHvac(Entity::getHorizonInterval(), Entity::getHorizonStartHour()),
  d_buildingSim(new BuildingSim(Entity::getHorizonInterval())),
  d_hvacElecDemandStats(),
  d_totalElecDemandStats(),
  d_ops()
{
  s_logger->repx(logga::xtra, "constructor call", getIdAndKind());
  d_builtinRemark = "beta";

  // ---------------------------------
  //  integrity checks
  // ---------------------------------

  // integrity checks - general
  if ( d_count < 0 )
    {
      s_logger->repx(logga::warn, "invalid house count", d_count);
    }
  else if ( d_count == 0 )
    {
      s_logger->repx(logga::rankJumpy, "zero house count", d_count);
    }
  if ( d_orientation < -90.0 || d_orientation > +90.0 )
    {
      s_logger->repx(logga::warn, "orientation outside range [-90,+90]", d_orientation);
    }
  // integrity checks - HVAC operation
  if ( d_hvacOpsStart < 0 || d_hvacOpsStart > 24 )
    {
      s_logger->repx(logga::warn, "HVAC ops start is not [0,24]", d_hvacOpsStart);
    }
  if ( d_hvacOpsEnd < 0 || d_hvacOpsEnd > 24 )
    {
      s_logger->repx(logga::warn, "HVAC ops end is not [0,24]", d_hvacOpsEnd);
    }
  if ( d_hvacOpsStart > d_hvacOpsEnd )
    {
      std::ostringstream oss;
      oss << d_hvacOpsStart << " : " << d_hvacOpsEnd;
      s_logger->repx(logga::warn, "HVAC ops start exceeds HVAC ops end", oss.str());
    }
  if ( d_hvacBacklogTrip < 0.0 )
    {
      s_logger->repx(logga::warn, "HVAC backlog trip negative", d_hvacBacklogTrip);
    }

  // ---------------------------------
  //  operational schedule
  // ---------------------------------

  // set the operational schedule, see unit 'c/opssched' for details
  d_timerHvac.setSchedule(d_hvacOpsStart, d_hvacOpsEnd);
  // report uploaded schedule
  std::ostringstream oss;
  oss << boost::format("%02d:00") % d_hvacOpsStart  // create 00:00 format
      << " thru "
      << boost::format("%02d:00") % d_hvacOpsEnd;
  s_logger->repx(logga::xtra, "HVAC operations set", oss.str());
  // report download details
  std::ostringstream put;
  d_timerHvac.report(put);
  s_logger->repx(logga::dbug, "additional reporting follows", "");
  s_logger->putx(logga::adhc, put);

  // ---------------------------------
  //  miscellaneous
  // ---------------------------------

  d_totalCapacity = d_count * d_demandHiBound;

} // function 'TeasBuildingElec::TeasBuildingElec'

// ---------------------------------------------------------
//  MEMBER FUNCTION : ~TeasBuildingElec
// ---------------------------------------------------------

TeasBuildingElec::~TeasBuildingElec()
{
  s_logger->repx(logga::adhc, "destructor call", getIdAndKind());
}

// ---------------------------------------------------------
//  MEMBER FUNCTION : establish
// ---------------------------------------------------------
//  Description  : loads and recovers required fabric dataset
//  Role         : beginning of horizon code
//  Techniques   : 'std::map'
//  Status       : complete
// ---------------------------------------------------------

void
TeasBuildingElec::establish()
  throw(xeona::entity_issue_2)
{
  // initial reporting
  s_logger->repx(logga::adhc, "entering member function", "");

  // ---------------------------------
  //  load fabric datasets
  // ---------------------------------

  const int count = ::loadFabricData(d_fabricDatas);
  if ( count == 0 )
    {
      s_logger->repx(logga::warn, "no building fabric datasets loaded", "trouble ahead");
    }

  // ---------------------------------
  //  recover fabric data
  // ---------------------------------

  // hunt thru map
  std::map<std::string, shared_ptr<BuildingFabricData> >::const_iterator pos1;
  std::map<std::string, shared_ptr<BuildingFabricData> >::const_iterator pos2;
  pos1 = d_fabricDatas.find(d_constructionType);
  if ( pos1 == d_fabricDatas.end() )          // the key is faulty
    {
      s_logger->repx(logga::warn, "failed to find construction type", d_constructionType);

      // function name
      const std::string func = XEONA_FUNC;   // preprocessor macro based '__func__' etc

      std::ostringstream put;
      put << "  function                    : "   << func                       << "\n";
      put << "  host identifier             : "   << getIdAndKind()             << "\n";
      put << "  requested construction type : \"" << d_constructionType << "\"" << "\n";
      put << "  supported types             :";
      for ( pos2 = d_fabricDatas.begin(); pos2 != d_fabricDatas.end(); ++pos2 )
        {
          put << " \"" << pos2->first << "\"";
        }
      put << "\n";
      s_logger->putx(logga::dbug, put);
      // choke
      if ( xeona::nopro == false )           // meaning option '--krazy' not applied
        {
          s_logger->repx(logga::warn, "about to throw", "xeona::entity_issue_2");
          throw xeona::entity_issue_2("TeasBuildingElec",
                                      func,
                                      getIdAndKind(),
                                      "requested fabric data not found");
        }
    }
  else                                       // the desired value
    {
      d_fabricData = pos1->second;           // buildin name
    }

} // function 'TeasBuildingElec::establish'

// ---------------------------------------------------------
//  MEMBER FUNCTION : initialize
// ---------------------------------------------------------
//  Description  : standard function
//  Role         : beginning of interval code, called before 'constrain' call
//  Techniques   : (nothing special)
//  Status       : complete
// ---------------------------------------------------------

void
TeasBuildingElec::initialize()
{
  // initial reporting
  s_logger->repx(logga::adhc, "entering member function", "");

} // function 'TeasBuildingElec::initialize'

// ---------------------------------------------------------
//  MEMBER FUNCTION : passBuildingOccData
// ---------------------------------------------------------
//  Description  : occupant data upload
//  Role         : called by 'AsopOccupant' and 'AsopOccupantParam'
//  Techniques   : (nothing special)
//  Status       : complete
// ---------------------------------------------------------

void
TeasBuildingElec::passBuildingOccData
(const double temperatureSetPoint,           // temperature set point [C] (Celsius)
 const double activityLoad,                  // load contribution but not electricity [W]
 const double electricityDemand)             // electricity demand [W]
{
  // initial reporting
  s_logger->repx(logga::adhc, "entering member function", "");

  // simple assignment
  d_temperatureSetPoint = temperatureSetPoint;
  d_activityLoad        = activityLoad;
  d_electricityDemand   = electricityDemand;
}

// ---------------------------------------------------------
//  MEMBER FUNCTION : constrain
// ---------------------------------------------------------
//  Description  : engineering characterization
//  Role         : beginning of interval code, called after 'initialize' call
//  Techniques   : protected member function 'calcBuildingDemand'
//  Status       : complete
//
//  Design notes
//
//      Use of count buildings
//
//          The role of 'd_count' needs careful attention.  The
//          'calcBuildingDemand' call calculates for just one
//          building.
//
// ---------------------------------------------------------

const int                                    // duty gol
TeasBuildingElec::constrain
(const xeona::DomainMode capacityMode)
{
  // initial reporting
  s_logger->repx(logga::adhc, "entering member function", "TeasBuildingElec");

  // create and fill a label object
  const std::string teasId = getIdentifier();
  Label lab(teasId);

  // recreate new operations OSP of the required type
  d_ops.reset(new OpsFac1Out0(d_solver, d_commitmentMode));
  d_ops->loadOspLabel(lab.str());

  // define a global col for internal use
  int inGol = -1;                            // nonsensical value

  // calculate electricity commodity demand (key call)
  const double demand = calcBuildingDemand(d_step, d_count);
  s_logger->repx(logga::xtra, "aggregate demand [W]", demand);

  // upload the engineering (using single argument inflexible call)
  boost::tie(inGol) = d_ops->uploadEngineering(demand);

  // upload specific costs -- including increment the "shift" term
  d_ops->uploadShortrunCosts(d_dutySpecCosts, inGol);
  d_ops->uploadShortrunCosts(d_sizeSpecCosts * d_nameplateSize + d_standingCosts);

  // bind global cols to the relevant interfaces
  d_inElec->bindOsp(d_solver, inGol);

  // store values
  d_demands->at(d_step) = demand;

  // store duty values
  d_floorDuty   = demand;
  d_ceilingDuty = demand;

  // return the duty gol
  return inGol;

} // function 'TeasBuildingElec::constrain'

// ---------------------------------------------------------
//  MEMBER FUNCTION : calcBuildingDemand
// ---------------------------------------------------------
//  Description  : simplified building thermal performance and HVAC models
//  Role         : called by 'constrain' to calculate electricity demand
//  Techniques   : essentially a wrapper to class 'BuildingSim'
//  Status       : complete
//
//  Design notes
//
//      Be aware that this utility function also sets data
//      members.
//
// ---------------------------------------------------------

double                                       // collective electricity demand
TeasBuildingElec::calcBuildingDemand
(const int step,                             // horizon step
 const int count)
{
  // initial reporting
  s_logger->repx(logga::adhc, "entering member function", "");

  // preparation
  const int interval = Entity::getHorizonInterval();

  // update timer as required and recover schedule
  d_timerHvac.bump(d_step);                           // CAUTION: must precede recovery
  const bool runOkay = d_timerHvac.isWorkingHour();   // 'true' means HVAC in operation

  // internal variables
  // none

  // ---------------------------------
  //  collect context information
  // ---------------------------------

  // step data
  const double                 tempAirOutside = d_ambientAirContext->getAirTemp(step);
  boost::tuple<double, double> irradiation    = d_ambientSolarContext->getSolar(step);
  boost::tuple<double, double> sunAngle       = d_ambientSolarContext->getSunAngles(step);

  // day data
  shared_ptr<std::vector<double> >                        tempAirOutsideDay;
  shared_ptr<std::vector<boost::tuple<double, double> > > irradiationDay;
  shared_ptr<std::vector<boost::tuple<double, double> > > sunAngleDay;

  tempAirOutsideDay = d_ambientAirContext->getAirTempDay(step, interval);
  irradiationDay    = d_ambientSolarContext->getSolarDay(step, interval);
  sunAngleDay       = d_ambientSolarContext->getSolarDay(step, interval);

  // create occupant timeseries
  shared_ptr<std::vector<double> >
    temperatureSetPointDay(new std::vector<double>(24, d_temperatureSetPoint));

  // ---------------------------------
  //  building simulation
  // ---------------------------------

  // reset call tracker
  d_buildingSim->zeroCallTracker();          // will ensure all necessary calls are made

  // transfer warning trip
  d_buildingSim->setHvacBacklogTrips         // optional call, 'h/thrmperf' sets default
    (d_hvacBacklogTrip,                      // chiller trip
     d_hvacBacklogTrip);                     // heater trip

  // upload data
  d_buildingSim->setBuildingData
    (d_demandHiBound,                        // electrical connection capacity [W]
     d_orientation,                          // building orientation [degrees]
     d_floorArea,                            // floor area [m2]
     d_floorAspectRatio,                     // floor aspect ratio [-] (from unity)
     d_wallHeight,                           // wall height [m2]
     d_windowToWallRatio);                   // window-to-wall ratio [0,1] [-]

  d_buildingSim->setBuildingFabric
    (d_fabricData);                          // consistent building fabric characteristics

  d_buildingSim->setPlantData
    (1.0,                                    // room air changes per hour [1/h]
     d_chillerCapacity,                      // chiller thermal capacity [W]
     0.05,                                   // chiller hysteresis [W]
     d_heaterCapacity,                       // heater thermal capacity [W]
     0.05,                                   // chiller hysteresis [W]
     0.2,                                    // non-chiller loss factor (chiller-mode) [-]
     7.0,                                    // air-to-air heat exchanger LMTD [degrees C]
     runOkay);                               // run status [bool]

  d_buildingSim->setAmbientConditions
    (tempAirOutside,                         // temperature of outside air [C]
     irradiation,                            // solar irradiation (direct, diffuse) [W/m2]
     sunAngle,                               // sun angles (zenith, azimuth) [degrees]
     tempAirOutsideDay,                      // day form of above
     irradiationDay,                         // day form of above
     sunAngleDay);                           // day form of above

  d_buildingSim->setBuildingOccupancyData
    (d_temperatureSetPoint,                  // temperature set point [C] (Celsius)
     temperatureSetPointDay,
     d_activityLoad,                         // load contribution but not electricity [W]
     d_electricityDemand);                   // electricity demand [W]

  // simulate the building
  d_buildingSim->calculate();

  // ---------------------------------
  //  information recovery
  // ---------------------------------

  // recover constraint information
  const double singleElecDemand  = d_buildingSim->getTotalElecDemand();
  const double collectElecDemand = count * singleElecDemand;

  // additional reporting as appropriate
  // YEEK 36 CODE (set by '--yeek')
  if ( xeona::yeek == 36 || xeona::yeek == 1 || xeona::yeek == 2 )
    {
      std::ostringstream put;
      d_buildingSim->report(put);            // major call
      s_logger->repx(logga::dbug, "additional reporting follows, yeek", xeona::yeek);
      s_logger->repx(logga::dbug, "current step", step);
      s_logger->repx(logga::dbug, "time of day (hh:mm:ss)", ::hourOfDay(step));
      s_logger->putx(logga::dbug, put);

      // tack on some local data too
      put << "\n";                           // spacer
      put << "  additional information"                                           << "\n"
          << ""                                                                   << "\n"
          << "    caller                           : " << XEONA_FUNC              << "\n"
          << "    count                        [-] : " << count                   << "\n"
          << "    single elec demand           [W] : " << singleElecDemand        << "\n"
          << "    collective elec demand       [W] : " << collectElecDemand       << "\n"
          << "    current step                     : " << d_step                  << "\n";
      s_logger->putx(logga::dbug, put);
    }

  // return
  return collectElecDemand;

} // function 'TeasBuildingElec::calcBuildingDemand'

// ---------------------------------------------------------
//  MEMBER FUNCTION : washup
// ---------------------------------------------------------
//  Description  : technical asset washup call
//  Role         : recover and record solution
//  Techniques   : 'boost::tuple'
//  Status       : complete
//
//  Design notes
//
//      Use of count buildings
//
//          Note that 'd_count' is only used in this class and is
//          not passed thru to the 'BuildingSim' object.  But
//          'd_count' has already been used to scale the
//          electrical demand passed thru to the solver.  Hence
//          there is nothing more to do here.
//
//          Be aware that most out-data reporting is per-building
//          but that some is total.  Physical totals comprise the
//          fields: "demands".
//
//          In contrast, all financial reporting is total.
//
// ---------------------------------------------------------

void
TeasBuildingElec::washup()
{
  // initial reporting
  s_logger->repx(logga::adhc, "entering member function", "");

  // results recovery
  double electricityDemand      = -1.0;      // nonsensical value
  boost::tie(electricityDemand) = d_ops->downloadSolution();

  // store entity state information
  d_demands->at(d_step) = electricityDemand;

  // another local variable
  const double capacity = d_demandHiBound;

  // accrued costs processing
  CostSet varCosts(0.0);                               // transfer object
  CostSet fixCosts(0.0);                               // transfer object
  d_ops->downloadVarCosts(varCosts);                   // recover variable costs
  d_ops->downloadFixCosts(fixCosts);                   // recover fixed costs
  updateShortrunCosts(d_step, varCosts, fixCosts);     // utilize supplied data
  updateEmbeddedCosts(d_step);                         // utilize in-house calcs

  // ratchet up maximum duty variables as required
  const double chillerCurrentDuty = d_buildingSim->getChillerDuty();
  const double heaterCurrentDuty  = d_buildingSim->getHeaterDuty();
  // but first defensively warn about simultaneous heating and cooling
  if ( chillerCurrentDuty > 0.0 && heaterCurrentDuty > 0.0 )
    {
      std::ostringstream oss;
      oss << chillerCurrentDuty << " : " << heaterCurrentDuty;
      s_logger->repx(logga::warn, "chiller and heater together, duties", oss.str());
    }
  // then ratchet
  if ( d_chillerMaxDuty < chillerCurrentDuty ) d_chillerMaxDuty = chillerCurrentDuty;
  if ( d_heaterMaxDuty  < heaterCurrentDuty  ) d_heaterMaxDuty  = heaterCurrentDuty;

  // update hvac-dutys, while noting heating is deemed negative
  // and this selection and any sign change is undertaken by
  // local free function '::hvacDuty'
  d_hvacDutys->at(d_step) = ::hvacDuty(chillerCurrentDuty, heaterCurrentDuty);

  // store some on-the-fly statistics
  d_dutyStats(electricityDemand);            // functor provided by class 'Block'
  d_sizeStats(capacity);                     // functor provided by class 'Block'

  // and also
  const double hvacElecDemand  = d_buildingSim->getHvacElecDemand();
  const double totalElecDemand = d_buildingSim->getTotalElecDemand();
  d_hvacElecDemandStats(hvacElecDemand);
  d_totalElecDemandStats(totalElecDemand);

  // finally ask the building simulation to update its state information
  d_buildingSim->saveState();

  // ---------------------------------
  //  protective measure
  // ---------------------------------

#ifdef _XDEBUG

  // these values should be overwritten by
  // 'TeasBuildingElec::passBuildingOccData'

  static const double nan = std::numeric_limits<double>::quiet_NaN();
  d_temperatureSetPoint = nan;
  d_activityLoad        = nan;
  d_electricityDemand   = nan;

#endif // _XDEBUG

} // function 'TeasBuildingElec::washup'

// ---------------------------------------------------------
//  MEMBER FUNCTION : conclude
// ---------------------------------------------------------
//  Description  : end of horizon code
//  Role         : standard call
//  Techniques   : class template 'Statistics<>'
//  Status       : complete
// ---------------------------------------------------------

void
TeasBuildingElec::conclude()
{
  // initial reporting
  s_logger->repx(logga::adhc, "entering member function", "");

  // calculate final metrics
  d_floorPerformance = d_hvacElecDemandStats.mean() / d_floorArea;

} // function 'TeasBuildingElec::conclude'

//  end of file

