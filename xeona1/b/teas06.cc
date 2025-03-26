//  XEONA ENERGY SYSTEMS MODELING ENVIRONMENT
//
//  file-create-name : teas06.cc
//  file-create-date : Mon 10-Jan-2011 16:52 UTC
//  file-initiator   : Robbie Morrison <robbie@actrix.co.nz>
//
//  file-role        : concrete technical assets 6 / implementation
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
//  $URL: file:///home/robbie/svn-root/xeona/futz/trunk/xeona1/b/teas06.cc $
//
//  GENERAL NOTES FOR THIS FILE

//  LOCAL AND SYSTEM INCLUDES

#include "teas06.h"           // companion header for this file (place first)

#include "../e/cxamb02.h"     // concrete ambient conditions contexts 2
#include "../e/cxamb01.h"     // concrete ambient conditions contexts 1
#include "../c/tsops.h"       // overloaded operators and similar for timeseries
#include "../c/stats.h"       // on-the-fly statistical calculations
#include "../c/recset.h"      // records and fields and also record-sets
#include "../c/label.h"       // helper class to format solver labels
#include "../c/conex.h"       // create and connect block interfaces
#include "../b/optops1.h"     // operate optimization sub-problems for hard assets 1
#include "../b/commods.h"     // commodities hierarchy

#include "../a/logger.h"      // standard logging functionality (as required)
#include "../c/smart_ptr.h"   // toggle between Boost and TR1 smart pointers
#include ".././common.h"      // common definitions for project (place last)

#include <string>             // C++ strings
#include <sstream>            // string-streams

#include <cmath>              // C-style maths, abs(), ceil(), floor(), sqrt()

//  DATA SOURCE

#include "../g/pvdata.h"      // static const std::string csvSiemensSM55;

//  CODE

// ---------------------------------------------------------
//  CLASS           : TeasPvInstallation
// ---------------------------------------------------------

// ---------------------------------------------------------
//  MEMBER FUNCTION : TeasPvInstallation
// ---------------------------------------------------------

TeasPvInstallation::TeasPvInstallation
(const std::string entityId,
 Record&           record) :
  CostRegister(record),
  TechnicalAsset(entityId, record, xeona::e_commitmentModes),
  CostRegisterSRFin(record),
  CostRegisterEmbFin(record),
  d_ambientSolarContext(record.lazyLink<CxAmbientSolar>("ambient-solar-context")),
  d_ambientAirContext(record.lazyLink<CxAmbientAir>("ambient-air-context")),
  d_panelModel(record.tieSingle<std::string>("panel-model")),
  d_count(record.tieSingle<int>("count")),
  d_panelZenith(record.tieSingle<double>("panel-zenith")),
  d_panelAzimuth(record.tieSingle<double>("panel-azimuth")),
  d_systemLossFactor(record.tieSingle<double>("system-loss-factor")),
  d_siteAltitude(record.tieSingle<double>("site-altitude")),
  d_internalDumpFlag(record.tieSingle<bool>("internal-dump-flag")),
  d_potentialProductions(record.tieTimeseries<double>("potential-productions")),
  d_actualProductions(record.tieTimeseries<double>("actual-productions")),
  d_discard(record.tieSingle<double>("discard")),
  d_outElec(Socket<CmElectricity>::create
            (entityId,                       // me
             "elec-1",                       // hard-coded socket label
             record.tieSingle<std::string>("socket-electricity-commodity"))),
  d_ops(),
  d_module(new PvModule(entityId))           // note constructor call with 'entityId'
{
  s_logger->repx(logga::xtra, "constructor call", getIdAndKind());
  d_builtinRemark = "beta";
}

// ---------------------------------------------------------
//  MEMBER FUNCTION : ~TeasPvInstallation
// ---------------------------------------------------------

TeasPvInstallation::~TeasPvInstallation()
{
  s_logger->repx(logga::adhc, "destructor call", getIdAndKind());
}

// ---------------------------------------------------------
//  MEMBER FUNCTION : establish
// ---------------------------------------------------------
//  Description  : low priority development reporting
//  Role         : loads 'd_panelModel' with Sandia data
//  Techniques   : helper class 'PvModule', integrity checks
//  Status       : complete
// ---------------------------------------------------------

void
TeasPvInstallation::establish()
{
  // initial reporting
  s_logger->repx(logga::adhc, "entering member function", "TeasPvInstallation");

  // load the pvModule with data, uses duly-modified Sandia CSV row
  if ( d_panelModel == "Siemens Solar SM55" ) d_module->slurp(csvSiemensSM55);
  else s_logger->repx(logga::warn, "panel model not supported", d_panelModel);

  // obtain data
  const xeona::Hemisphere hemisphere = Entity::getHemisphere();
  const double            latitude   = d_ambientSolarContext->getLatitude();

  // hemisphere tests
  switch ( hemisphere )
    {
    case xeona::e_north:
      // check latitude settings [0,90] for integrity [1]
      if ( latitude < 0.0 )
        {
          s_logger->repx(logga::warn, "northern hemisphere with latitude", latitude);
        }
      // check panel azimuth angle [90,270] for integrity [2]
      if ( ! ( d_panelAzimuth >=  90.0 && d_panelAzimuth <= 270.0 ) )
        {
          s_logger->repx(logga::rankJumpy,
                         "panel azimuth not [90,270]",
                         d_panelAzimuth);
        }
      break;
    case xeona::e_south:
      // check latitude settings [0,-90] for integrity [1]
      if ( latitude > 0.0 )
        {
          s_logger->repx(logga::warn, "southern hemisphere with latitude", latitude);
        }
      // check panel azimuth angle [0,90] or [270,360] for integrity [2]
      if ( !
           ( d_panelAzimuth >=   0.0 && d_panelAzimuth <=  90.0 )
           ||
           ( d_panelAzimuth >= 270.0 && d_panelAzimuth <= 360.0 ) )
        {
          s_logger->repx(logga::rankJumpy,
                         "panel azimuth not [270,360] [0,90]",
                         d_panelAzimuth);
        }
      break;
    case xeona::e_hemiNotSet:                // the compiler required all cases be treated
      s_logger->repx(logga::warn, "hemisphere enum not properly set", hemisphere);
      break;
    }

  // [1] naturally checks on [-180,0] range too
  // [2] there is nothing absolute about these bounds but it is
  // unlikely that a sensible installation would transgress them

  // check tilt angle (panel zenith) against latitude
  const double latTiltDiff    = std::abs(std::abs(latitude) - d_panelZenith);
  const double thresholdAngle = 20.0;        // in [degrees]
  if ( latTiltDiff > thresholdAngle )
    {
      std::ostringstream oss;
      oss << latitude << " : " << d_panelZenith;
      s_logger->repx(logga::rankJumpy, "latitude/tilt difference [degrees]", oss.str());

      std::ostringstream put;
      put << "  panel tilt mismatch alert [degrees]"                             << "\n"
          << "    latitude (southern hemisphere -ve) : " << latitude             << "\n"
          << "    panel zenith (tilt angle)          : " << d_panelZenith        << "\n"
          << "    alert threshold                    : " << thresholdAngle       << "\n"
          << "    current difference                 : " << latTiltDiff          << "\n";
      s_logger->putx(logga::dbug, put);
    }
}

// ---------------------------------------------------------
//  MEMBER FUNCTION : constrain
// ---------------------------------------------------------
//  Description  : constrain call
//  Role         : characterize current engineering and short-run costs
//  Techniques   : helper class 'PvModule'
//  Status       : complete
//
//  Design notes
//
//      The site altitude from the solar dataset will be used if
//      non-zero.  Otherwise the supplied site altitude will be
//      uses.
//
// ---------------------------------------------------------

const int                                    // duty gol
TeasPvInstallation::constrain
(const xeona::DomainMode capacityMode)
{
  // initial reporting
  s_logger->repx(logga::adhc, "entering member function", "TeasPvInstallation");

  // create and fill a label object
  const std::string teasId = getIdentifier();
  Label lab(teasId);

  // recreate new operations OSP of the required type
  d_ops.reset(new OpsFac0Out1(d_solver, d_commitmentMode));
  d_ops->loadOspLabel(lab.str());

  // define a global col for internal use
  int outGol = -1;                           // nonsensical value

  // reset panel to enable a new suite of calls -- the 'slurp'
  // call persists however
  d_module->reset();

  // use 'siteElevation' if not zero, else use 'd_siteAltitude'
  double siteAltitude        = 0.0;
  const double siteElevation = d_ambientSolarContext->getSiteElevation();
  if ( siteElevation > 0.0 ) siteAltitude = siteElevation;
  else                       siteAltitude = d_siteAltitude;

  // set installation details
  d_module->setInstallation(d_panelZenith,   // module tilt, horizontal is zero [degrees]
                            d_panelAzimuth,  // module azimuth angle [degrees]
                            siteAltitude);   // site altitude [m]

  // get prevailing air data
  const double windSpeed = d_ambientAirContext->getWindSpeed(d_step);
  const double airTemp   = d_ambientAirContext->getAirTemp(d_step);

  // get prevailing solar data
  double directSolar  = -1.0;                // nonsensical value
  double difuseSolar  = -1.0;                // nonsensical value
  double solarZenith  = -1.0;                // nonsensical value
  double solarAzimuth = -1.0;                // nonsensical value

  boost::tie(directSolar, difuseSolar)  = d_ambientSolarContext->getSolar(d_step);
  boost::tie(solarZenith, solarAzimuth) = d_ambientSolarContext->getSunAngles(d_step);

  // set the weather details
  d_module->setWeather(directSolar,          // direct solar component [W/m2]
                       difuseSolar,          // diffuse solar component [W/m2]
                       solarZenith,          // solar zenith angle [degrees]
                       solarAzimuth,         // solar azimuth angle [degrees]
                       airTemp,              // air temperature [C]
                       windSpeed);           // wind speed at 10m [m/s]

  // calculate maximum production using Sandia model
  double hiPowerSingle = -1.0;               // nonsensical value
  const bool okay = d_module->calculate(hiPowerSingle,      // pass-by-reference
                                        1.0 - d_systemLossFactor);

  // report if okay
  if ( okay )
    {
      s_logger->repx(logga::adhc, "module calculate returned clean", okay);
    }

  // additional reporting as appropriate
  // YEEK 40 CODE (set by '--yeek')
  if ( xeona::yeek == 40 || xeona::yeek == 1 || xeona::yeek == 2       // yeek trigger
       ||
       std::isnan(hiPowerSingle)                  // nan, refer <cmath>
       ||
       ! okay )                                   // not okay
    {
      std::ostringstream put;
      s_logger->repx(logga::dbug, "additional reporting follows, yeek", xeona::yeek);
      d_module->printReport(put);            // print report
      s_logger->putx(logga::dbug, put);
      s_logger->addSmartBlank(logga::dbug);
      d_module->printDataIssues(put);        // print issues
      s_logger->putx(logga::dbug, put);
      s_logger->addSmartBlank(logga::dbug);
      put << std::boolalpha;
      put << "  model                   : " << d_module->getModel()    << "\n"
          << "  system loss factor [-]  : " << d_systemLossFactor      << "\n"
          << "  site production [W]     : " << d_count * hiPowerSingle << "\n"
          << "  okay                    : " << okay                    << "\n";
      s_logger->putx(logga::dbug, put);
    }

  // protection against GLPK abort in face of a nan from the Sandia model
  // update: 'PvModule' cannot, from r5580, return a nan and instead supplies a zero
  if ( std::isnan(hiPowerSingle) )           // refer <cmath>
    {
      if ( xeona::nopro == false )           // meaning option '--krazy' not applied
        {
          s_logger->repx(logga::warn, "will throw xeona::bad_submodel", "");
          throw xeona::bad_submodel("Sandia photovoltaic array performance model",
                                    "nan returned for loss factor adjusted power output");
        }
      else
        {
          hiPowerSingle = 0.0;
        }
    }

  // determine high power
  const double hiPower = d_count * hiPowerSingle;
  s_logger->repx(logga::xtra, "panel count", d_count);
  s_logger->repx(logga::xtra, "potential maximum power, hiPower", hiPower);

  // determine low power
  double loPower = -1.0;                     // nonsensical value
  switch ( d_internalDumpFlag )
    {
    case true:                               // internal dumping allowed
      loPower = 0.0;
      break;
    case false:
      loPower = hiPower;                     // internal dumping disallowed
      break;
    }

  // upload the engineering
  boost::tie(outGol) = d_ops->uploadEngineering(loPower, hiPower);

  // upload specific costs -- including increment the "shift" term
  d_ops->uploadShortrunCosts(d_dutySpecCosts, outGol);
  d_ops->uploadShortrunCosts(d_sizeSpecCosts * d_nameplateSize + d_standingCosts);

  // bind global cols to the relevant interfaces
  d_outElec->bindOsp(d_solver, outGol);

  // store values
  d_potentialProductions->at(d_step) = hiPower;

  // store duty values
  d_floorDuty   = loPower;
  d_ceilingDuty = hiPower;

  // return the duty gol
  return outGol;
}

// ---------------------------------------------------------
//  MEMBER FUNCTION : washup
// ---------------------------------------------------------
//  Description  : technical asset washup call
//  Role         : recover and record solution
//  Techniques   : (nothing special)
//  Status       : complete
// ---------------------------------------------------------

void
TeasPvInstallation::washup()
{
  // initial reporting
  s_logger->repx(logga::dbug, "entering member function, step", d_step);

  // results recovery
  double actualProduction      = -1.0;       // nonsensical value
  boost::tie(actualProduction) = d_ops->downloadSolution();

  // store entity state information
  d_actualProductions->at(d_step) = actualProduction;

  // accrued costs processing
  CostSet varCosts(0.0);                               // transfer object
  CostSet fixCosts(0.0);                               // transfer object
  d_ops->downloadVarCosts(varCosts);                   // recover variable costs
  d_ops->downloadFixCosts(fixCosts);                   // recover fixed costs
  updateShortrunCosts(d_step, varCosts, fixCosts);     // utilize supplied data
  updateEmbeddedCosts(d_step);                         // utilize in-house calcs

  // store some on-the-fly statistics
  const double potentialProduction = d_potentialProductions->at(d_step);
  d_dutyStats(actualProduction);             // functor provided by class 'Block'
  d_sizeStats(potentialProduction);          // functor provided by class 'Block'
}

// ---------------------------------------------------------
//  MEMBER FUNCTION : conclude
// ---------------------------------------------------------

void
TeasPvInstallation::conclude()
{
  // normalized discard = discarded production / potential production
  const double totalPotential = xeona::vectorSum(d_potentialProductions);
  const double totalActual    = xeona::vectorSum(d_actualProductions);
  if ( totalPotential == 0.0 )               // protect against div-by-zero
    {
      d_discard = 0.0;                       // zero makes the most sense
    }
  else
    {
      d_discard = (totalPotential - totalActual) / totalPotential;
    }
}

//  end of file

