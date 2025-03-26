//  XEONA ENERGY SYSTEMS MODELING ENVIRONMENT
//
//  file-create-name : teas13.cc
//  file-create-date : Thu 17-Nov-2011 22:12 UTC
//  file-initiator   : Robbie Morrison <robbie@actrix.co.nz>
//
//  file-role        : concrete technical assets 13 / implementation
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
//  $URL: file:///home/robbie/svn-root/xeona/futz/trunk/xeona1/b/teas13.cc $
//
//  GENERAL NOTES FOR THIS FILE

//  LOCAL AND SYSTEM INCLUDES

#include "teas13.h"           // companion header for this file (place first)

#include "../e/cxamb01.h"     // concrete ambient conditions contexts 1
#include "../c/tsops.h"       // overloaded operators and similar for timeseries
#include "../c/stats.h"       // on-the-fly statistical calculations
#include "../c/recset.h"      // records and fields and also record-sets
#include "../c/label.h"       // helper class to format solver labels
#include "../c/conex.h"       // create and connect block interfaces
#include "../b/optops1.h"     // operate optimization sub-problems for hard assets 1
#include "../b/commods01.h"   // concrete commodities 1
#include "../b/commods.h"     // commodities hierarchy

#include "../a/logger.h"      // standard logging functionality (as required)
#include "../c/smart_ptr.h"   // toggle between Boost and TR1 smart pointers
#include ".././common.h"      // common definitions for project (place last)

#include <string>             // C++ strings
#include <sstream>            // string-streams

//  CODE

// ---------------------------------------------------------
//  CLASS           : TeasWindfarm
// ---------------------------------------------------------

// ---------------------------------------------------------
//  MEMBER FUNCTION : TeasWindfarm
// ---------------------------------------------------------

TeasWindfarm::TeasWindfarm
(const std::string entityId,
 Record&           record) :
  CostRegister(record),
  TechnicalAsset(entityId, record, xeona::e_commitmentModes),
  CostRegisterSRFin(record),
  CostRegisterEmbFin(record),
  d_ambientAirContext(record.lazyLink<CxAmbientAir>("ambient-air-context")),
  d_count(record.tieSingle<int>("count")),
  d_turbineRating(record.tieSingle<double>("turbine-rating")),
  d_loCutSpeed(record.tieSingle<double>("lo-cut-speed")),
  d_hiCutSpeed(record.tieSingle<double>("hi-cut-speed")),
  d_potentialProductions(record.tieTimeseries<double>("potential-productions")),
  d_actualProductions(record.tieTimeseries<double>("actual-productions")),
  d_availability(record.tieSingle<double>("availability")),
  d_spill(record.tieSingle<double>("spill")),
  d_outElec(Socket<CmElectricity>::create
            (entityId,                       // me
             "elec-1",                       // hard-coded socket label
             record.tieSingle<std::string>("socket-electricity-commodity"))),
  d_ops()
{
  s_logger->repx(logga::xtra, "constructor call", getIdAndKind());
  d_builtinRemark = "beta";
}

// ---------------------------------------------------------
//  MEMBER FUNCTION : ~TeasWindfarm
// ---------------------------------------------------------

TeasWindfarm::~TeasWindfarm()
{
  s_logger->repx(logga::adhc, "destructor call", getIdAndKind());
}

// ---------------------------------------------------------
//  MEMBER FUNCTION : establish
// ---------------------------------------------------------
//  Description  : low priority development reporting
//  Role         : logging output solely
//  Techniques   :
//  Status       : complete
// ---------------------------------------------------------

void
TeasWindfarm::establish()
{
  // CAUTION: this code provides only low priority development
  // reporting -- it should not be used as a template for other
  // 'establish' calls

  // describe  ambient air context
  std::ostringstream put;
  s_logger->repx(logga::adhc, "development reporting 1 follows", "");
  put << "  post-link identifier : " << d_ambientAirContext->getIdentifier() << "\n"
      << "  post-link resource   : " << d_ambientAirContext.get()            << "\n";
  s_logger->putx(logga::adhc, put);

  // get and report the current windspeed
  // CAUTION: cannot use 'd_step' as it still remains at its
  // nonsensical -1 initialization
  const int index   = 0;
  const double wind = d_ambientAirContext->getWindSpeed(index);

  s_logger->repx(logga::adhc, "development reporting 2 follows", "");
  put << "  link test : 'CxAmbientAir' linked entity from 'TeasWindfarm' entity" << "\n"
      << "    fixed index (step not yet set) : " << index                        << "\n"
      << "    wind-speed                     : " << wind                         << "\n";
  s_logger->putx(logga::adhc, put);
}

// ---------------------------------------------------------
//  MEMBER FUNCTION : constrain
// ---------------------------------------------------------
//  Description  : constrain call
//  Role         : characterize current engineering and short-run costs
//  Techniques   : protected member function 'calcTurbinePower'
//  Status       : complete
// ---------------------------------------------------------

const int                                    // duty gol
TeasWindfarm::constrain
(const xeona::DomainMode capacityMode)
{
  // initial reporting
  s_logger->repx(logga::adhc, "entering member function", "TeasWindfarm");

  // create and fill a label object
  const std::string teasId = getIdentifier();
  Label lab(teasId);

  // recreate new operations OSP of the required type
  d_ops.reset(new OpsFac0Out1(d_solver, d_commitmentMode));
  d_ops->loadOspLabel(lab.str());

  // define a global col for internal use
  int outGol = -1;                           // nonsensical value

  // get prevailing wind-speed
  const double wspeed  = d_ambientAirContext->getWindSpeed(d_step);

  // calculate power range
  const double loPower = 0.0;
  const double hiPower = d_count * calcTurbinePower(wspeed);
  s_logger->repx(logga::xtra, "turbine count", d_count);
  s_logger->repx(logga::xtra, "potential park production, hiPower", hiPower);

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
TeasWindfarm::washup()
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
TeasWindfarm::conclude()
{
  // availability = potential production / nameplate production (spill not considered)
  const Statistics<double> potproStats = xeona::fillStatistics(d_potentialProductions);
  d_availability                       = potproStats.mean() / (d_count * d_turbineRating);

  // normalized spill = discarded production / potential production
  const double totalPotential = xeona::vectorSum(d_potentialProductions);
  const double totalActual    = xeona::vectorSum(d_actualProductions);
  if ( totalPotential == 0.0 )               // protect against div-by-zero
    {
      d_spill = 0.0;
    }
  else
    {
      d_spill = (totalPotential - totalActual) / totalPotential;
      xeona::roundZero(d_spill, xeona::zero0);    // see unit 'c/util3'
    }
}

// ---------------------------------------------------------
//  MEMBER FUNCTION : calcTurbinePower
// ---------------------------------------------------------
//  Description  : returns power for single turbine
//  Role         : utility function
//  Techniques   : maths
//  Status       : complete
//
//  Formula
//
//      cube-law mapping windspeed 's' to power 'p':
//
//          p = A * s^3 + B when lo < s < hi, else p = 0
//
//          with constants A = C/hi^3 and B = 0
//
//      where 'C' is the turbine rating, 'lo' is the low
//      cut-in/out and 'hi' is the high cut-in/out (hysteresis
//      not modeled)
//
//      the cube-law is from physics and runs thru the origin
//      (that is, zero power at zero speed)
//
// ---------------------------------------------------------

double                                       // resultant power [W]
TeasWindfarm::calcTurbinePower               // single turbine
(const double windSpeed)                     // known wind speed [m/s]
{
  double power = 0.0;                        // zero output
  if ( windSpeed > d_loCutSpeed && windSpeed < d_hiCutSpeed )
    {
      // turbine is within its working range
      const double normSpeed = windSpeed / d_hiCutSpeed;
      power                  = d_turbineRating * std::pow(normSpeed, 3);
      s_logger->repx(logga::xtra, "turbine within working range", power);
    }
  else
    {
      std::ostringstream oss;
      oss << d_loCutSpeed << " - " << d_hiCutSpeed;
      s_logger->repx(logga::xtra, "turbine outside working range", oss.str());
    }
  return power;
}

//  end of file

