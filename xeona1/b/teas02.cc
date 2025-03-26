//  XEONA ENERGY SYSTEMS MODELING ENVIRONMENT
//
//  file-create-name : teas02.cc
//  file-create-date : Wed 22-Apr-2009 12:40 UTC
//  file-initiator   : Robbie Morrison <robbie@actrix.co.nz>
//
//  file-role        : concrete technical assets 2 / implementation
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
//  $URL: file:///home/robbie/svn-root/xeona/futz/trunk/xeona1/b/teas02.cc $
//
//  GENERAL NOTES FOR THIS FILE

//  LOCAL AND SYSTEM INCLUDES

#include "teas02.h"           // companion header for this file (place first)

#include "../i/gnuplot.h"     // gnuplot interface, originally by Daniel Stahlke
#include "../e/cxamb01.h"     // concrete ambient conditions contexts 1
#include "../c/tsops.h"       // overloaded operators and similar for timeseries
#include "../c/stats.h"       // on-the-fly statistical calculations
#include "../c/recset.h"      // records and fields and also record-sets
#include "../c/label.h"       // helper class to format solver labels
#include "../c/files.h"       // free functions for regular files
#include "../c/conex.h"       // create and connect block interfaces
#include "../b/optops1.h"     // operate optimization sub-problems for hard assets 1
#include "../b/commods01.h"   // concrete commodities 1
#include "../b/commods.h"     // commodities hierarchy

#include "../a/logger.h"      // standard logging functionality (as required)
#include "../c/smart_ptr.h"   // toggle between Boost and TR1 smart pointers
#include ".././common.h"      // common definitions for project (place last)

#include <limits>             // numeric_limits<T>::infinity() and similar
#include <string>             // C++ strings
#include <sstream>            // string-streams

#include <unistd.h>           // POSIX sleep(), usleep(), access(), chown()

#include <typeinfo>           // run-time type information (RTTI)

#include <boost/format.hpp>   // printf style formatting

//  CODE

// ---------------------------------------------------------
//  CLASS           : TeasOxidToElec
// ---------------------------------------------------------

// ---------------------------------------------------------
//  MEMBER FUNCTION : TeasOxidToElec
// ---------------------------------------------------------

TeasOxidToElec::TeasOxidToElec
(const std::string entityId,
 Record&           record) :
  CostRegister(record),
  TechnicalAsset(entityId, record, xeona::e_commitmentModes),
  CostRegisterSRFin(record),
  CostRegisterEmbFin(record),
  d_prodLoBound(record.tieSingle<double>("prod-lo-bound")),
  d_prodHiBound(record.tieSingle<double>("prod-hi-bound")),
  d_marginalEfficiency(record.tieSingle<double>("marginal-efficiency")),
  d_fuelNoload(record.tieSingle<double>("fuel-noload")),
  d_fuelAncillary(record.tieSingle<double>("fuel-ancillary")),
  d_rampRestraintDown(record.tieSingle<double>("ramp-restraint-down")),
  d_rampRestraintUp(record.tieSingle<double>("ramp-restraint-up")),
  d_productions(record.tieTimeseries<double>("productions")),
  d_shutdownStatuss(record.tieTimeseries<bool>("shutdown-statuss")),
  d_inOxid(Cable<CmOxidize>::create
           (entityId,                        // me
            record.tieSingle<std::string>("socket-oxidize"),
            record.tieSingle<std::string>("cable-oxidize-commodity"))),
  d_outElec(Socket<CmElectricity>::create
            (entityId,                       // me
             "elec-1",                       // hard-coded socket label
             record.tieSingle<std::string>("socket-electricity-commodity"))),
  d_oxid(),
  d_ops()
{
  // initial reporting
  s_logger->repx(logga::xtra, "constructor call", getIdAndKind());

  // builtin remark
  d_builtinRemark = "beta";

  // integrity checks
  if ( d_rampRestraintDown < 0.0 )
    {
      s_logger->repx(logga::warn,
                     "negative downward ramp restraint",
                     d_rampRestraintDown);
    }
  if ( d_rampRestraintUp < 0.0 )
    {
      s_logger->repx(logga::warn,
                     "negative upward ramp restraint",
                     d_rampRestraintUp);
    }
}

// ---------------------------------------------------------
//  MEMBER FUNCTION : ~TeasOxidToElec
// ---------------------------------------------------------

TeasOxidToElec::~TeasOxidToElec()
{
  s_logger->repx(logga::adhc, "destructor call", getIdAndKind());
}

// ---------------------------------------------------------
//  MEMBER FUNCTION : establish
// ---------------------------------------------------------

void
TeasOxidToElec::establish()
{
  // initial reporting
  s_logger->repx(logga::adhc, "entering member function", "");

  // CAUTION: 'Interface::getCm' can only be called AFTER the
  // interface connections are complete -- which is why this code
  // is here and not in the constructor

  // obtain fuel commodity
  const shared_ptr<Commodity> com = d_inOxid->getCm();
  d_oxid                          = dynamic_pointer_cast<CmOxidize>(com);

  // extra reporting during development
  s_logger->repx(logga::adhc,
                 "combustion enthalpy recovered [J/kg]",
                 d_oxid->getSpecCombEnthalpy());
}

// ---------------------------------------------------------
//  MEMBER FUNCTION : constrain
// ---------------------------------------------------------
//  Description  : technical asset constrain call
//  Role         : characterize current engineering and short-run costs
//  Techniques   : 'boost::tuples::ignore' to ignore tuple ties
//  Status       : complete
// ---------------------------------------------------------

const int                                    // duty gol
TeasOxidToElec::constrain
(const xeona::DomainMode capacityMode)
{
  // initial reporting
  s_logger->repx(logga::adhc, "entering member function", "TeasOxidToElec");

  // create and fill a label object
  const std::string teasId = getIdentifier();
  Label lab(teasId);

  // obtain fuel attributes
  const double specEnthalpy = d_oxid->getSpecCombEnthalpy();     // about 20e+06 J/kg
  const double specCo2equiv = d_oxid->getSpecCo2equiv();         // for carbon, 3.7 kg/kg

  // set scale factor
  const double scaleFactor  = 1.0e+07;

  // recreate new operations OSP of the required type
  d_ops.reset(new OpsFac1Out1(d_solver, d_commitmentMode));
  d_ops->loadOspLabel(lab.str());

  // define global cols for internal use
  int  inGol = -1;                           // nonsensical value
  int outGol = -1;                           // nonsensical value

  // calculate ramp rate restrictions
  double rampLoSize = -1.0;                  // nonsensical value
  double rampHiSize = -1.0;                  // nonsensical value
  if ( d_step == 0 )                         // no prior information
    {
      rampLoSize = 0.0;
      rampHiSize = d_prodHiBound;
    }
  else                                       // modify based on previous production
    {
      const double priorProduction = d_productions->at(d_step - 1);
      rampLoSize = priorProduction - (d_nameplateSize * d_rampRestraintDown);
      rampHiSize = priorProduction + (d_nameplateSize * d_rampRestraintUp);
    }

  // upload the engineering
  boost::tie(inGol,                          // factor (fuel) stream
             outGol,                         // output (product) stream
             boost::tuples::ignore)          // trip status variable
    = d_ops->uploadEngineering(d_prodLoBound,
                               d_prodHiBound,
                               d_marginalEfficiency,
                               d_fuelNoload,
                               d_fuelAncillary,
                               rampLoSize,
                               rampHiSize,
                               specEnthalpy, // kg->J conversion
                               scaleFactor); // to improve the LP problem

  // upload specific costs -- including increment the "shift" term
  d_dutySpecCosts.ghg = specCo2equiv;
  d_ops->uploadShortrunCosts(d_dutySpecCosts, inGol);
  d_ops->uploadShortrunCosts(d_sizeSpecCosts * d_nameplateSize + d_standingCosts);

  // ad-hoc reporting
  s_logger->repx(logga::adhc, "duty-specific ghg cost [kg/J]", d_dutySpecCosts.ghg);

  // bind global cols to the relevant interfaces
  d_inOxid ->bindOsp(d_solver,  inGol);
  d_outElec->bindOsp(d_solver, outGol);

  // store duty values
  d_floorDuty   = d_prodLoBound;
  d_ceilingDuty = d_prodHiBound;

  // return the duty gol
  return outGol;

} // function 'TeasOxidToElec::constrain'

// ---------------------------------------------------------
//  MEMBER FUNCTION : washup
// ---------------------------------------------------------
//  Description  : technical asset washup call
//  Role         : recover and record solution
//  Techniques   : (nothing special)
//  Status       : complete
// ---------------------------------------------------------

void
TeasOxidToElec::washup()
{
  // initial reporting
  s_logger->repx(logga::dbug, "entering member function, step", d_step);

  // results recovery
  double fuelUsage        = -1.0;            // nonsensical value
  double actualProduction = -1.0;            // nonsensical value
  bool   tripStatus       = false;           // arbitrary value
  boost::tie(fuelUsage,
             actualProduction,
             tripStatus) = d_ops->downloadSolution();

  // store entity state information
  d_productions->at(d_step)     = actualProduction;
  d_shutdownStatuss->at(d_step) = tripStatus;

  // another local variable
  const double potentialProduction = d_prodHiBound;

  // accrued costs processing
  CostSet varCosts(0.0);                               // transfer object
  CostSet fixCosts(0.0);                               // transfer object
  d_ops->downloadVarCosts(varCosts);                   // recover variable costs
  d_ops->downloadFixCosts(fixCosts);                   // recover fixed costs
  updateShortrunCosts(d_step, varCosts, fixCosts);     // utilize supplied data
  updateEmbeddedCosts(d_step);                         // utilize in-house calcs

  // ad-hoc reporting
  s_logger->repx(logga::adhc, "variable ghg cost [kg/s]", varCosts.ghg);

  // store some on-the-fly statistics
  d_dutyStats(actualProduction);             // functor provided by class 'Block'
  d_sizeStats(potentialProduction);          // functor provided by class 'Block'
}

// ---------------------------------------------------------
//  CLASS           : TeasCcgt
// ---------------------------------------------------------
//
//  Design notes
//
//      CCGT is "combined-cycle gas turbine".  The CCGT model
//      presented here is intended for industrial-scale
//      installations -- typically from 100MW.
//
//      The function 'TeasCcgt::characterize' documents the
//      underlying model in detail.
//
//      Note the use of protected variables
//      'd_onDesignCapacity_M' and 'd_onDesignEfficiency_M'.
//
// ---------------------------------------------------------

// ---------------------------------------------------------
//  MEMBER FUNCTION : TeasCcgt
// ---------------------------------------------------------

TeasCcgt::TeasCcgt
(const std::string entityId,
 Record&           record) :
  CostRegister(record),
  TechnicalAsset(entityId, record, xeona::e_commitmentModes),
  CostRegisterSRFin(record),
  CostRegisterEmbFin(record),
  d_ambientAirContext(record.lazyLink<CxAmbientAir>("ambient-air-context")),
  d_onDesignCapacity(record.tieSingle<double>("on-design-capacity")),
  d_onDesignEfficiency(record.tieSingle<double>("on-design-efficiency")),
  d_cutoutCapacityFactor(record.tieSingle<double>("cutout-capacity-factor")),
  d_cutoutEfficiencyFactor(record.tieSingle<double>("cutout-efficiency-factor")),
  d_fuelAncillary(record.tieSingle<double>("fuel-ancillary")),
  d_productions(record.tieTimeseries<double>("productions")),
  d_fuelDemands(record.tieTimeseries<double>("fuel-demands")),
  d_capacitys(record.tieTimeseries<double>("capacitys")),
  d_shutdownStatuss(record.tieTimeseries<bool>("shutdown-statuss")),
  d_varCostsGhg(record.tieTimeseries<double>("variable-costs-greenhouse")),
  d_carbonEmissions(record.tieTimeseries<double>("carbon-emissions")),
  d_inOxid(Cable<CmOxidGas>::create
           (entityId,                        // me
            record.tieSingle<std::string>("socket-oxidize"),
            record.tieSingle<std::string>("cable-oxidize-commodity"))),
  d_outElec(Socket<CmElectricity>::create
            (entityId,                       // me
             "elec-1",                       // hard-coded socket label
             record.tieSingle<std::string>("socket-electricity-commodity"))),
  d_oxidSpecCosts(0.0),
  d_oxid(),
  d_ops(),
  d_onDesignCapacity_M(d_onDesignCapacity),
  d_onDesignEfficiency_M(d_onDesignEfficiency)
{
  // initial reporting
  s_logger->repx(logga::xtra, "constructor call", getIdAndKind());

  // builtin remark
  d_builtinRemark = "beta";

  // integrity checks for reference on-design capacity and efficiency
  // and cutout factors covering capacity and efficiency
  if ( d_onDesignCapacity < 0.0 )
    {
      s_logger->repx(logga::warn, "problematic on-design capacity", d_onDesignCapacity);
    }
  else if ( d_onDesignCapacity < 50e+06 || d_onDesignCapacity > 600e+06 )  // 600MW limit
    {
      s_logger->repx(logga::rankJumpy,
                     "unusual on-design capacity",
                     d_onDesignCapacity);
    }
  if ( d_onDesignEfficiency < 0.0 || d_onDesignEfficiency > 1.0)
    {
      s_logger->repx(logga::warn,
                     "problematic on-design efficiency",
                     d_onDesignEfficiency);
    }
  else if ( d_onDesignEfficiency < 0.2 || d_onDesignEfficiency > 0.65 )    // 65% limit
    {
      s_logger->repx(logga::rankJumpy,
                     "unusual on-design efficiency",
                     d_onDesignEfficiency);
    }
  if ( d_cutoutCapacityFactor < 0.3 || d_cutoutCapacityFactor > 1.0 )
    {
      s_logger->repx(logga::warn,
                     "problematic cutout capacity factor",
                     d_cutoutCapacityFactor);
    }
  else if ( d_cutoutCapacityFactor < 0.4 || d_cutoutCapacityFactor > 0.7 )
    {
      s_logger->repx(logga::rankJumpy,
                     "unusual cutout capacity factor",
                     d_cutoutCapacityFactor);
    }
  if ( d_cutoutEfficiencyFactor < 0.0 || d_cutoutEfficiencyFactor > 1.0 )
    {
      s_logger->repx(logga::warn,
                     "problematic cutout efficiency factor",
                     d_cutoutEfficiencyFactor);
    }
  if ( d_cutoutEfficiencyFactor < 0.7 || d_cutoutEfficiencyFactor > 0.9 )
    {
      s_logger->repx(logga::rankJumpy,
                     "unusual cutout efficiency factor",
                     d_cutoutEfficiencyFactor);
    }
}

// ---------------------------------------------------------
//  MEMBER FUNCTION : ~TeasCcgt
// ---------------------------------------------------------

TeasCcgt::~TeasCcgt()
{
  s_logger->repx(logga::adhc, "destructor call", getIdAndKind());
}

// ---------------------------------------------------------
//  MEMBER FUNCTION : establish
// ---------------------------------------------------------

void
TeasCcgt::establish()
{
  // initial reporting
  s_logger->repx(logga::adhc, "entering member function", "");

  // CAUTION: 'Interface::getCm' can only be called AFTER the
  // interface connections are complete -- which is why this code
  // is here and not in the constructor

  // obtain fuel commodity
  const shared_ptr<Commodity> com = d_inOxid->getCm();
  d_oxid                          = dynamic_pointer_cast<CmOxidGas>(com);

  // extra reporting during development
  s_logger->repx(logga::adhc,
                 "combustion enthalpy recovered [J/kg]",
                 d_oxid->getSpecCombEnthalpy());
}

// ---------------------------------------------------------
//  MEMBER FUNCTION : constrain
// ---------------------------------------------------------
//  Description  : technical asset constrain call
//  Role         : characterize current engineering and short-run costs
//  Techniques   : essentially a wrapper with cost functionality
//  Status       : complete
//
//  Design notes
//
//    The cost upload is specified HERE and not in
//    'constrainCore'.  Which means that these costs are NOT
//    forced on 'TeasCcgtCapture' (or any other derived class)
//    because it makes no call to 'TeasCcgt::contrain' (often
//    derived class functions do call their base equivalents
//    first, but not in this case).
//
// ---------------------------------------------------------

const int                                    // duty gol
TeasCcgt::constrain
(const xeona::DomainMode capacityMode)
{
  // initial reporting
  s_logger->repx(logga::adhc, "entering member function", "TeasCcgt");

  // obtain fuel attributes
  const double specCo2equiv = d_oxid->getSpecCo2equiv();         // methane is 2.7 kg/kg

  // workhorse call
  int dutyGol = -1.0;                        // nonsensical value
  int oxidGol = -1.0;                        // nonsensical value, not used here
  boost::tie(dutyGol,
             oxidGol) = TeasCcgt::constrainCore(capacityMode);

  // class-only code for "oxid-specific" costs (see associated upload call too)
  d_oxidSpecCosts.ghg = specCo2equiv;

  // upload specific costs -- including increment the "shift" term
  d_ops->uploadShortrunCosts(d_oxidSpecCosts, oxidGol);
  d_ops->uploadShortrunCosts(d_dutySpecCosts, dutyGol);
  d_ops->uploadShortrunCosts(d_sizeSpecCosts * d_nameplateSize + d_standingCosts);

  // ad-hoc reporting
  s_logger->repx(logga::adhc, "duty-specific ghg cost [kg/J]", d_oxidSpecCosts.ghg);

  // return the duty gol
  return dutyGol;

} // function 'TeasCcgt::constrain'

// ---------------------------------------------------------
//  MEMBER FUNCTION : constrainCore (protected)
// ---------------------------------------------------------
//  Description  : workhorse constrain call
//  Role         : used by this class and the derived 'TeasCcgtCapture'
//  Techniques   : utility function 'characterize', 'boost::tuples::ignore'
//  Status       : complete
// ---------------------------------------------------------

boost::tuple
<int,                                      // duty gol
 int>                                      // oxid gol
TeasCcgt::constrainCore
(const xeona::DomainMode capacityMode)
{
  // initial reporting
  s_logger->repx(logga::adhc, "entering member function", "");

  // create and fill a label object
  const std::string teasId = getIdentifier();
  Label lab(teasId);

  // obtain fuel attributes
  const double specEnthalpy = d_oxid->getSpecCombEnthalpy();     // methane is 55e+06 J/kg

  // set scale factor
  const double scaleFactor  = 1.0e+07;

  // obtain prevailing air temperature
  const double airTemp = d_ambientAirContext->getAirTemp(d_step);

  // recreate new operations OSP of the required type
  d_ops.reset(new OpsFac1Out1(d_solver, d_commitmentMode));
  d_ops->loadOspLabel(lab.str());

  // define global cols for internal use
  int  inGol = -1;                           // nonsensical value
  int outGol = -1;                           // nonsensical value

  // determine the temperature-adjusted efficiency coefficients
  // and capacities using the utility function 'characterize'
  double coeffA      = -1.0;                 // unlikely value
  double coeffB      = -1.0;                 // unlikely value
  double prodLoBound = -1.0;                 // nonsensical value
  double prodHiBound = -1.0;                 // nonsensical value
  boost::tie(coeffA,                         // reciprocal of marginal efficiency
             coeffB,                         // no-load fuel
             prodLoBound,                    // temperature-adjusted cutout capacity
             prodHiBound)                    // temperature-adjusted on-design capacity
    = characterize(airTemp);                 // prevailing air temperature

  // reinterpret the coefficients
  const double marginalEfficiency = 1.0 / coeffA;
  const double fuelNoLoad         = coeffB;

  // set the ramp rate restriction to 'inf', thereby disabling it
  const double rampRateRestriction = std::numeric_limits<double>::infinity();

  // upload the engineering
  boost::tie(inGol,                          // factor (fuel) stream
             outGol,                         // output (product) stream
             boost::tuples::ignore)          // trip status variable
    = d_ops->uploadEngineering(prodLoBound,
                               prodHiBound,
                               marginalEfficiency,
                               fuelNoLoad,
                               d_fuelAncillary,
                               rampRateRestriction,
                               rampRateRestriction,
                               specEnthalpy, // kg->J conversion
                               scaleFactor); // to improve the LP problem

  // CAUTION: in a normal 'constrain' function, the specific
  // costs would be uploaded here -- but this is a utility
  // function and that task is left to the client code to
  // undertake as they see best

  // bind global cols to the relevant interfaces
  d_inOxid ->bindOsp(d_solver,  inGol);
  d_outElec->bindOsp(d_solver, outGol);

  // store duty values
  d_floorDuty   = prodLoBound;
  d_ceilingDuty = prodHiBound;

  // return the duty gol and the fuel gol (the later for use by
  // the derived class 'TeasCcgtCapture')
  return boost::make_tuple(outGol,           // duty
                           inGol);           // oxid

} // function 'TeasCcgt::constrainCore'

// ---------------------------------------------------------
//  MEMBER FUNCTION : washup
// ---------------------------------------------------------
//  Description  : technical asset washup call
//  Role         : recover and record solution
//  Techniques   : (nothing special)
//  Status       : complete
// ---------------------------------------------------------

void
TeasCcgt::washup()
{
  // initial reporting
  s_logger->repx(logga::dbug, "entering member function, step", d_step);

  // another local variable
  const double potentialProduction = d_ceilingDuty;

  // results recovery
  double fuelUsage        = -1.0;            // nonsensical value
  double actualProduction = -1.0;            // nonsensical value
  bool   tripStatus       = false;           // arbitrary value
  boost::tie(fuelUsage,
             actualProduction,
             tripStatus) = d_ops->downloadSolution();

  // calculate CO2e emissions
  const double specCo2equiv  = d_oxid->getSpecCo2equiv();   // methane is 2.7 kg/kg
  const double co2eEmissions = specCo2equiv * fuelUsage;

  // store entity state information
  d_productions->at(d_step)     = actualProduction;
  d_fuelDemands->at(d_step)     = fuelUsage;
  d_carbonEmissions->at(d_step) = co2eEmissions;
  d_capacitys->at(d_step)       = potentialProduction;
  d_shutdownStatuss->at(d_step) = tripStatus;

  // accrued costs processing
  CostSet varCosts(0.0);                               // transfer object
  CostSet fixCosts(0.0);                               // transfer object
  d_ops->downloadVarCosts(varCosts);                   // recover variable costs
  d_ops->downloadFixCosts(fixCosts);                   // recover fixed costs
  updateShortrunCosts(d_step, varCosts, fixCosts);     // utilize supplied data
  updateEmbeddedCosts(d_step);                         // utilize in-house calcs

  // store information
  d_varCostsGhg->at(d_step) = varCosts.ghg;

  // ad-hoc reporting
  s_logger->repx(logga::adhc, "variable ghg cost [kg/s]", varCosts.ghg);
  s_logger->repx(logga::adhc, "fixed ghg cost [kg/s]",    fixCosts.ghg);

  // store some on-the-fly statistics
  d_dutyStats(actualProduction);             // functor provided by class 'Block'
  d_sizeStats(potentialProduction);          // functor provided by class 'Block'

} // function 'TeasCcgt::washup'

// ---------------------------------------------------------
//  MEMBER FUNCTION : TeasCcgt::characterize
// ---------------------------------------------------------
//  Description  : transform CCGT data for use in an OSP (optimization sub-problem)
//  Role         : support for 'constrain' function
//  Techniques   : curve-fitting
//  Status       : complete
//
//  Design notes
//
//      The CCGT model presented here is for industrial-scale
//      installations and covers the following features:
//
//        * ambient air temperature corrections for supplied
//          on-design (full-load) reference (15C, 100kPa)
//          efficiency and capacity (C = degrees Celsius))
//
//        * constant marginal efficiency plus offset to
//          approximate a typical performance curve over the
//          active range
//
//        * a cutout limit below which the plant is idle
//
//        * no restrictions on repeated toggling between idle and
//          run states or on ramp rates in general
//
//      If these assumptions are too simplistic, they can, in
//      most cases, be remedied through refinements to this
//      model.
//
//      For instance, the commercial 'GateCycle' program contains
//      a library of gas turbines from a number of manufacturers
//      (and not just GE) and could be used to develop more
//      sophisticated CCGT entities -- that said, these new
//      entities would need to use piecewise performance curves
//      and perhaps also binary variables.  Whether the
//      additional work and complexity could be justified remains
//      an open question.
//
//      Technical details regarding industrial-scale CCGT
//      performance were obtained from Brooks (2000) and Wick
//      (2006).  The first document is published by GE, USA and
//      the second by Alstom, Switzerland.  In addition, Alstom
//      (2007) provides a few details for the Taranaki CCGT plant
//      in New Zealand.
//
//      Performance degradation occurs over the life of an
//      installation and can range between 2 and 6% of original
//      performance -- only some of which can be reversed thru
//      maintenance and overhaul.  This aspect of CCGT
//      performance is not implemented here, but would not be
//      conceptually difficult to do so.
//
//  Implementation
//
//      This function ultimately calculates the 'a' and 'b'
//      coefficients needed by the OSP (optimization sub-problem)
//      formulation, namely:
//
//          f(x) = ax + b
//
//          where: x is the net-production of electricity, f is
//          the fuel usage, 1/a is the marginal efficiency, and b
//          is the offset
//
//      and also:
//
//          x ranges [l,h]
//
//          where: l is the temperature-adjusted cutout capacity
//          and h is the temperature-adjusted on-design capacity
//
//      The temperature-adjusted on-design capacity 'h' is also
//      returned by this function.
//
//      The introduction of a cutout capacity means that the
//      optimization problem is necessarily mixed-integer.
//
//      The definition for average efficiency is also required:
//
//          g(x) = x / f(x)
//               = x / ax + b
//
//          where: g is the average efficiency
//
//      The model proceeds in two steps:
//
//        * first, the on-design-capacity and
//          on-design-efficiency values are adjusted for
//          temperature dependency (but not for pressure or
//          humidity dependencies)
//
//        * second, the optimization sub-problem (OSP)
//          coefficients are formed for a simple shutdown mode
//          model
//
//  Background
//
//      The background development for this model was undertake
//      using 'gnuplot'.  Please see file 'ccgt.gp' and related
//      files.
//
//  REFERENCES
//
//      Alstom.  2007.  GT24 and GT26 gas turbines : clean, high
//        performance, flexible -- brochure
//        PT-PE/BPROB/GSTRBN08/eng/TMG/10.08/CH/6703.  Alstom
//        (Switzerland) Ltd, Baden, Switzerland
//
//      Brooks, Frank J.  2000.  GE gas turbine performance
//        characteristics -- GER-3567H.  GE Power Systems, New
//        York, USA.  See fig9, p8: Effect of ambient
//        temperature.
//
//      Wick, Johannes.  2006.  Advanced gas turbine technology
//        GT26.  Presentation at the Jornada Tecnologica, Madrid,
//        Spain on 26 October 2006.  See slide 11: GT24/GT26 gas
//        turbine : part load efficiency.  Alstom. *
//
//        * for slide 11, the y-axis label and the plot title
//          differ a little in meaning -- the axis label is
//          assumed correct
//
// ---------------------------------------------------------

boost::tuple
<double,                                     // coefficient 'a'
 double,                                     // coefficient 'b'
 double,                                     // temperature-adjusted cutout capacity
 double>                                     // temperature-adjusted on-design capacity
TeasCcgt::characterize
(const double airTemp)                       // prevailing air temperature
{
  // initial reporting
  s_logger->repx(logga::adhc, "entering member function, air temp", airTemp);

  // integrity checks
  // the Brook (2000) corrections are restricted to ambient air
  // temperatures [C] in the range [-18,+49]
  if ( airTemp < -18.0 || airTemp > +49.0 )
    {
      s_logger->repx(logga::warn, "ambient air temp outside [-18,49]", airTemp);
    }

  // ---------------------------------
  //  step 1 - modify on-design parameters
  // ---------------------------------

  // the reference 'on-design-capacity' and
  // 'on-design-efficiency' values are adjusted for temperature
  // dependency (but not for pressure or humidity dependencies)
  //
  // the adjustment parameters were extracted from figure 9 in
  // Brooks (2000, p8, fig9) -- and were design-normalized in
  // their original depiction and hence can be used without the
  // need to consider engineering units, aside from, of course,
  // the Celsius denominator
  //
  // as an example and using the figures below, a minor heat wave
  // of 20C means a 12% reduction in electrical output capacity

  // parameter values extracted from Brooks (2000, p8, fig9)
  const double refAirTemp         = +15.0;   // an industry standard [C]
  const double outputFactor       = -0.0062; // all values [1/C]
  const double heatrateFactorWarm = +0.0017; // above 'refAirTemp' [1/C]
  const double heatrateFactorCool = +0.0012; // below 'refAirTemp' [1/C]

  // reference air temperature difference
  const double deltaAirTemp = airTemp - refAirTemp;

  // on-design capacity (output) and cutout adjustments
  const double capacity = d_onDesignCapacity_M * (1.0 + deltaAirTemp * outputFactor);
  const double cutout   = capacity * d_cutoutCapacityFactor;

  // on-design efficiency adjustments [1]
  double heatrateFactor = 0.0;
  if      ( deltaAirTemp > 0.0 ) heatrateFactor = heatrateFactorWarm;
  else if ( deltaAirTemp < 0.0 ) heatrateFactor = heatrateFactorCool;
  else                           heatrateFactor = 0.0;     // any value would do
  const double heatrate   = 1.0 / d_onDesignEfficiency_M;
  const double efficiency = 1.0 / (heatrate * (1.0 + deltaAirTemp * heatrateFactor));

  // [1] note the need to convert to the heat rate for the
  // adjustment and then back again -- with the heat rate simply
  // being the reciprocal of efficiency

  // ---------------------------------
  //  step 2 - create off-design model
  // ---------------------------------

  // the optimization sub-problem (OSP) is formed using a simple
  // shutdown mode formulation
  //
  // the formulae below were derived by solving g(x) at two
  // operating points: at capacity and at cutout
  //
  // the curves generated by substituting 'a' and 'b' into g(x)
  // were compared with those on slide 11 from Wick (2006) and
  // deemed to be close enough for use in 'xeona' -- of course,
  // the final decision on this matter rests with the modeler in
  // relation to their circumstances and needs

  const double min = d_cutoutCapacityFactor;      // normalized, say 0.4
  const double hi  = efficiency;                  // temperature-adjusted, say 0.55
  const double rel = d_cutoutEfficiencyFactor;    // normalized, say 0.8

  const double a = (min - rel)/(rel * hi * (min - 1.0));    // derived from g(x)
  const double b = 1/hi - a;                                // derived from g(x)

  const double coeffA = a;                   // scaling by capacity not required
  const double coeffB = b * capacity;        // scaling by capacity required

  // integrity check: '1/a' must be greater than or equal to 'hi'
  // otherwise equation g(x) goes haywire
  if ( 1/a < hi )
    {
      std::ostringstream oss;
      oss << 1/a << " : " << hi;
      s_logger->repx(logga::warn, "coefficient problem, 1/a < hi", oss.str());
    }

  // additional reporting as appropriate
  // YEEK 41 CODE (set by '--yeek')
  if ( xeona::yeek == 41 || xeona::yeek == 1 || xeona::yeek == 2 )
    {
      const std::string id       = getIdAndKind();
      const double refCutoutCap  = d_cutoutCapacityFactor   * d_onDesignCapacity_M;
      const double refCutoutEffy = d_cutoutEfficiencyFactor * d_onDesignEfficiency_M;
      const double adjCutoutEffy = d_cutoutEfficiencyFactor * efficiency;

      std::ostringstream put;
      put
        << "  CCGT report (- input value, + export value)"                         << "\n"
        << "    note: mod indicates original values modified by a derived class"   << "\n"
        << "      identifier                       : " << id                       << "\n"
        << "    environment"                                                       << "\n"
        << "        air temperature            [C] : " << airTemp                  << "\n"
        << "        reference air temperature  [C] : " << refAirTemp               << "\n"
        << "    cutout factors"                                                    << "\n"
        << "      - cutout capacity factor     [-] : " << d_cutoutCapacityFactor   << "\n"
        << "      - cutout efficiency factor   [-] : " << d_cutoutEfficiencyFactor << "\n"
        << "    capacities"                                                        << "\n"
        << "      - on-design, reference, orig [W] : " << d_onDesignCapacity       << "\n"
        << "      - on-design, reference, mod  [W] : " << d_onDesignCapacity_M     << "\n"
        << "      + on-design, adjusted        [W] : " << capacity                 << "\n"
        << "        cutout, reference          [W] : " << refCutoutCap             << "\n"
        << "      + cutout, adjusted           [W] : " << cutout                   << "\n"
        << "    efficiencies and relative cutouts"                                 << "\n"
        << "      - on-design, reference, orig [-] : " << d_onDesignEfficiency     << "\n"
        << "      - on-design, reference, mod  [-] : " << d_onDesignEfficiency_M   << "\n"
        << "        on-design, adjusted        [-] : " << efficiency               << "\n"
        << "        cutout, reference          [-] : " << refCutoutEffy            << "\n"
        << "        cutout, adjusted           [-] : " << adjCutoutEffy            << "\n"
        << "    optimization coefficients (energy-based)"                          << "\n"
        << "      + slope (a)                  [-] : " << coeffA                   << "\n"
        << "      + offset (b)                 [W] : " << coeffB                   << "\n"
        << "        marginal efficiency (1/a)  [-] : " << 1.0/coeffA               <<"\n";

      // see below for an actual 'gnuplot' call

      put
        << ""                                                                    << "\n"
        << "  CCGT performance curve (for gnuplot)"                              << "\n"
        << "      f(x) = x / (" << coeffA << " * x + " << coeffB << ")"          << "\n"
        << "      set xrange [" << cutout << " : " << capacity << "]"            << "\n"
        << "      set xlabel \"net-output [W]\"; set ylabel \"efficiency [-]\""  << "\n"
        << "      set title \"Performance curve for CCGT '" << id << "'\""       << "\n"
        << "      set key off"                                                   << "\n"
        << "      plot f(x)"                                                     << "\n";

      s_logger->repx(logga::dbug, "additional reporting follows, yeek", xeona::yeek);
      s_logger->putx(logga::dbug, put);

      // note simple gnuplot script:
      //
      //  f(x) = x / (1.48788 * x + 1.1709e+08)
      //  set xrange [1.57392e+08 : 3.9348e+08]
      //  set xlabel "net-output [W]"; set ylabel "efficiency [-]"
      //  set title "Performance curve for CCGT 'teas-ccgt-1'"
      //  set key off
      //  plot f(x)
    }

  // gnuplot plot window counter
  //
  // tests show that at least 102 plot windows are
  // acceptable, hence the static counter is set to 200

  static int plotCount           = 0;
  static const int plotCountTrip = 200;      // change as required

  // additional reporting as appropriate
  // YEEK 37 CODE (set by '--yeek'), also governed by --tout
  if ( xeona::yeek == 37 && ++plotCount < plotCountTrip )
    {
      // initial reporting
      s_logger->repx(logga::dbug, "gnuplotting follows, yeek", xeona::yeek);
      s_logger->repx(logga::dbug, "local gnuplot plot window count", plotCount);
      if ( xeona::tout.empty() && plotCount < 2 )
        {
          s_logger->repx(logga::warn, "requires for instance", "--tout wxt");
        }

      // preamble
      const std::string id = getIdAndKind();
      const std::string plotTitle = "CCGT characterization curve";

      // gnuplot preparatory code taken from source file 'c/tsset.cc'

      // constructor call
      Gnuplot gp(xeona::gnuplot,             // gnuplot invocation command
                 Gnuplot::e_debugLogsOff);   // set dedicated logging status

      // set the terminal up
      if ( xeona::tout.empty() )
        {
          s_logger->repx(logga::warn, "gnuplot term not set", "");
        }
      else if ( xeona::tout == "dumb" )
        {
          gp.setTerminal("dumb");            // dumb terminal means the console
          gp.setPlotWindowSize(148, 40);     // width (cols) x height (lines)
        }
      else if ( xeona::tout == "x11" )
        {
          gp.setTerminal("x11");             // standard x11 libraries
          gp.setPlotWindowTitle(plotTitle);  // also prepends a counter
        }
      else if ( xeona::tout == "wxt" )
        {
          gp.setTerminal("wxt");             // best with gnuplot version 4.4
          gp.setPersist();                   // keep open after client terminates
          gp.setPlotWindowSize(1100, 600);   // width (px) x height (px) [1]
          gp.setPlotWindowTitle(plotTitle);  // also prepends a counter
        }
      else if ( xeona::tout == "svg" )
        {
          // unique filename
          std::ostringstream stub;
          stub << "ccgt-curve-";
          stub << boost::format("%02d") % plotCount;
          stub << "-";
          const std::string svgname = xeona::temporaryFilename("svg", stub.str());

          // gnuplot
          gp.setTerminal("svg");             // SVG output
          gp.setOutput(svgname);
        }
      else
        {
          std::clog << "** coding error 01 in source file " << __FILE__ << std::endl;
        }

      // gnuplot model
      gp << "f(x) = x / (" << coeffA << " * x + " << coeffB << ")"          << "\n"
         << "set xrange [" << cutout << " : " << capacity << "]"            << "\n"
         << "set xlabel \"net-output [W]\"; set ylabel \"efficiency [-]\""  << "\n"
         << "set title \"Performance curve for CCGT '" << id << "'\""       << "\n"
         << "set key off"                                                   << "\n"
         << "plot f(x)"                                                     << "\n";

      // finalize as required
      if  ( xeona::tout == "svg" )
        {
          gp.setFinalize();
        }

      // finishing up -- the 'sleep' is just for viewing convenience
      sleep(1);                              // refer <unistd.h>
      s_logger->repx(logga::dbug, "gnuplotting complete", "");

    } // 'Gnuplot' object will destruct on block exit

  // [1] requires gnuplot version 4.4, largest on 'hinau' is 1350 x 650

  // return the temperature-adjusted efficiency coefficients and
  // the temperature-adjusted capacity
  return boost::make_tuple(coeffA,           // coefficient 'a'
                           coeffB,           // coefficient 'b'
                           cutout,
                           capacity);        // temperature-adjusted capacity

} // function 'TeasCcgt::characterize' (not constrain)

// ---------------------------------------------------------
//  CLASS           : TeasCcgtCapture
// ---------------------------------------------------------
//
//  Design notes
//
//      CCGT means "combined-cycle gas turbine" and "capture"
//      indicates carbon capture technology is present.
//
//      This class inherits from 'TeasCcgt' and uses the CCGT
//      characterizations there for the bulk of the numerical
//      work.  The base class function 'TeasCcgt::characterize'
//      documents the underlying CCGT model in detail.
//
//      The code here determines how much supercritical carbon
//      dioxide will be produced and hence require sequestration
//      services.
//
//      Note the use of protected super-class variables
//      'TeasCcgt::d_onDesignCapacity_M' and
//      'TeasCcgt::d_onDesignEfficiency_M'.  Note also that some
//      variables are have the same scope-unqualified (that part
//      after the ::) identifiers.
//
// ---------------------------------------------------------

// ---------------------------------------------------------
//  MEMBER FUNCTION : TeasCcgtCapture
// ---------------------------------------------------------

TeasCcgtCapture::TeasCcgtCapture
(const std::string entityId,
 Record&           record) :
  CostRegister(record),                      // CAUTION: necessary
  TeasCcgt(entityId, record),                // CCGT unit
  d_carbonCaptureRate(record.tieSingle<double>("carbon-capture-rate")),
  d_efficiencyHit(record.tieSingle<double>("efficiency-hit")),
  d_capacityHit(record.tieSingle<double>("capacity-hit")),
  d_carbonCaptures(record.tieTimeseries<double>("carbon-captures")),
  d_inCseq(Cable<CmCarbonSeq>::create
           (entityId,                        // me
            record.tieSingle<std::string>("socket-carbon-seq"),
            record.tieSingle<std::string>("cable-carbon-seq-commodity"))),
  d_cseqLoBound(0.0),                        // hard-coded
  // set the high bound restriction to 'inf', thereby disabling it
  d_cseqHiBound(std::numeric_limits<double>::infinity()),
  d_cseq(),
  d_ops()                                    // note also 'TeasCcgt::d_ops'
{
  // initial reporting
  s_logger->repx(logga::xtra, "constructor call", getIdAndKind());

  // builtin remark
  d_builtinRemark = "beta";

  // integrity checks
  if ( d_carbonCaptureRate < 0.0 || d_carbonCaptureRate > 1.0 )
    {
      s_logger->repx(logga::warn, "problematic carbon capture rate", d_carbonCaptureRate);
    }
  if ( d_efficiencyHit < 0.0 || d_efficiencyHit > 1.0)
    {
      s_logger->repx(logga::warn, "problematic efficiency hit", d_efficiencyHit);
    }
  if ( d_capacityHit < 0.0 || d_capacityHit > 1.0)
    {
      s_logger->repx(logga::warn, "problematic capacity hit", d_capacityHit);
    }
}

// ---------------------------------------------------------
//  MEMBER FUNCTION : ~TeasCcgtCapture
// ---------------------------------------------------------

TeasCcgtCapture::~TeasCcgtCapture()
{
  s_logger->repx(logga::adhc, "destructor call", getIdAndKind());
}

// ---------------------------------------------------------
//  MEMBER FUNCTION : establish
// ---------------------------------------------------------

void
TeasCcgtCapture::establish()
{
  // initial reporting
  s_logger->repx(logga::adhc, "entering member function", "");

  // base class call
  TeasCcgt::establish();
}

// ---------------------------------------------------------
//  MEMBER FUNCTION : constrain
// ---------------------------------------------------------
//  Description  : technical asset constrain call
//  Role         : characterize current engineering and short-run costs
//  Techniques   : utility function 'characterize', 'boost::tuples::ignore'
//  Status       : complete
// ---------------------------------------------------------

const int                                    // duty gol
TeasCcgtCapture::constrain
(const xeona::DomainMode capacityMode)
{
  // initial reporting
  s_logger->repx(logga::adhc, "entering member function", "TeasCcgtCapture");

  // obtain fuel attributes
  const double specCo2equiv = d_oxid->getSpecCo2equiv();    // methane is 2.7 kg/kg

  // ---------------------------------
  //  'TeasCcgt' base class code
  // ---------------------------------

  s_logger->repx(logga::adhc, "calling base class code", "");

  // set revised factors for CCGT, noting that the underscore
  // variants are used by the various member functions
  TeasCcgt::d_onDesignCapacity_M
    = TeasCcgt::d_onDesignCapacity
    * (1.0 - d_capacityHit);
  TeasCcgt::d_onDesignEfficiency_M
    = TeasCcgt::d_onDesignEfficiency
    * (1.0 - d_efficiencyHit);

  // constrain the CCGT and obtain two gols -- note that this
  // call does not upload specific costs
  int dutyGol = -1.0;                        // nonsensical value
  int oxidGol = -1.0;                        // nonsensical value
  boost::tie(dutyGol,
             oxidGol) = TeasCcgt::constrainCore(capacityMode);

  // ---------------------------------
  //  'TeasCcgtCapture' code
  // ---------------------------------

  s_logger->repx(logga::adhc, "calling my own code", "");

  // create and fill a label object
  const std::string teasId = getIdentifier();
  Label lab(teasId);

  // obtain fuel attributes (pure methane as a fuel would produce 2.7 kg/kg)
  const double specCarbonDioxide = TeasCcgt::d_oxid->getSpecCarbonDioxide();

  // recreate new operations OSP of the required type
  d_ops.reset(new OpsFuelToCseq(d_solver, d_commitmentMode));
  d_ops->loadOspLabel(lab.str());

  // define global cols for internal use
  int fuelGol = -1;                          // nonsensical value
  int cseqGol = -1;                          // nonsensical value

  // upload the engineering
  boost::tie(fuelGol,                        // fuel stream as factor
             cseqGol)                        // cseq stream as factor
    = d_ops->uploadEngineering(d_cseqLoBound,
                               d_cseqHiBound,
                               specCarbonDioxide,
                               d_carbonCaptureRate);

  // upload specific costs -- including increment the "shift" term
  d_dutySpecCosts.ghg = specCo2equiv * (1.0 - d_carbonCaptureRate);
  d_ops->uploadShortrunCosts(d_dutySpecCosts, fuelGol);
  d_ops->uploadShortrunCosts(d_sizeSpecCosts * d_nameplateSize + d_standingCosts);

  // ---------------------------------
  //  reporting
  // ---------------------------------

  // additional reporting as appropriate
  // YEEK 48 CODE (set by '--yeek')
  if ( xeona::yeek == 48 || xeona::yeek == 1 || xeona::yeek == 2 )
    {
      const std::string func = XEONA_FUNC;   // preprocessor macro defined in 'common.h'
      std::ostringstream put;
      put
        << "  TeasCcgtCapture report (++ means 'fuel-couple'd here)"               << "\n"
        << "    function                   : " << func                             << "\n"
        << "    label                      : " << getIdAndKind()                   << "\n"
        << "    design capacity modified   : " << TeasCcgt::d_onDesignCapacity_M   << "\n"
        << "    design efficiency modified : " << TeasCcgt::d_onDesignEfficiency_M << "\n"
        << "    returned dutyGol (base)    : " << dutyGol                          << "\n"
        << "    returned oxidGol (base) +  : " << oxidGol                          << "\n"
        << "    returned fuelGol (me)   +  : " << fuelGol                          << "\n"
        << "    returned cseqGol (me)      : " << cseqGol                          <<"\n";
      s_logger->repx(logga::dbug, "additional reporting follows, yeek", xeona::yeek);
      s_logger->putx(logga::dbug, put);
    }

  // ---------------------------------
  //  joint code
  // ---------------------------------

  // finally couple the two fuel gols (normally this call is used
  // to bridge between a technical asset and its asset operator

  // OSP couple call (from unit 'b/optprob')
  xeona::couple(d_solver,
                oxidGol,                     // from base class 'TeasCcgt'
                fuelGol,                     // from this class 'TeasCcgtCapture'
                lab.str("factor-fuel"));     // note ".osp-couple" gets later appended

  // ---------------------------------
  //  interface
  // ---------------------------------

  // note that 'd_inOxid' and 'd_outElec' have already been bound
  // by the base class in an earlier call

  // bind global cols to the relevant interfaces
  d_inCseq->bindOsp(d_solver, cseqGol);

  // omit the usual: store duty values 'd_floorDuty' 'd_ceilingDuty'

  // ---------------------------------
  //  housekeeping
  // ---------------------------------

  // return the duty gol
  return dutyGol;

} // function 'TeasCcgtCapture::constrain'

// ---------------------------------------------------------
//  MEMBER FUNCTION : washup
// ---------------------------------------------------------
//  Description  : technical asset washup call
//  Role         : recover and record solution
//  Techniques   : (nothing special)
//  Status       : complete
// ---------------------------------------------------------

void
TeasCcgtCapture::washup()
{
  // initial reporting
  s_logger->repx(logga::dbug, "entering member function, step", d_step);

  // another local variable
  const double potentialProduction = d_ceilingDuty;

  // results recovery -- 'TeasCcgt'
  double fuelUsage1       = -1.0;            // nonsensical value
  double actualProduction = -1.0;            // nonsensical value
  bool   tripStatus       = false;           // arbitrary value
  boost::tie(fuelUsage1,
             actualProduction,
             tripStatus) = TeasCcgt::d_ops->downloadSolution();

  // results recovery -- 'TeasCcgtCapture'
  double fuelUsage2    = -1.0;               // nonsensical value
  double carbonCapture = -1.0;               // nonsensical value
  double co2eEmissions = -1.0;               // nonsensical value
  boost::tie(fuelUsage2,
             carbonCapture,
             co2eEmissions) = TeasCcgtCapture::d_ops->downloadSolution();

  // confirm fuel usage is numerically equal
  if ( ! xeona::almostEqual(fuelUsage1, fuelUsage2, xeona::numic) )
    {
      std::ostringstream oss;
      oss << fuelUsage1 << " " << fuelUsage2;
      s_logger->repx(logga::warn, "fuel usage mismatch (bad OSP code?)", oss.str());
    }

  // store entity state information -- 'TeasCcgt'
  d_productions->at(d_step)     = actualProduction;
  d_fuelDemands->at(d_step)     = fuelUsage1;
  d_carbonEmissions->at(d_step) = co2eEmissions;
  d_capacitys->at(d_step)       = potentialProduction;
  d_shutdownStatuss->at(d_step) = tripStatus;

  // store entity state information -- 'TeasCcgtCapture'
  d_carbonEmissions->at(d_step) = co2eEmissions;
  d_carbonCaptures->at(d_step)  = carbonCapture;

  // accrued costs processing -- 'TeasCcgt'
  CostSet varCosts1(0.0);                              // transfer object
  CostSet fixCosts1(0.0);                              // transfer object
  TeasCcgt::d_ops->downloadVarCosts(varCosts1);        // recover variable costs
  TeasCcgt::d_ops->downloadFixCosts(fixCosts1);        // recover fixed costs

  // accrued costs processing -- 'TeasCcgtCapture'
  CostSet varCosts2(0.0);                              // transfer object
  CostSet fixCosts2(0.0);                              // transfer object
  TeasCcgtCapture::d_ops->downloadVarCosts(varCosts2); // recover variable costs
  TeasCcgtCapture::d_ops->downloadFixCosts(fixCosts2); // recover fixed costs

  // accrued costs processing -- combine and store
  CostSet varCosts(0.0);                               // transfer object
  CostSet fixCosts(0.0);                               // transfer object
  varCosts = varCosts1 + varCosts2;
  fixCosts = fixCosts1 + fixCosts2;
  updateShortrunCosts(d_step, varCosts, fixCosts);     // utilize supplied data
  updateEmbeddedCosts(d_step);                         // utilize in-house calcs

  // store information
  d_varCostsGhg->at(d_step) = varCosts.ghg;

  // ad-hoc reporting
  s_logger->repx(logga::adhc, "variable ghg cost [kg/s]", varCosts.ghg);
  s_logger->repx(logga::adhc, "fixed ghg cost [kg/s]",    fixCosts.ghg);

  // store some on-the-fly statistics
  d_dutyStats(actualProduction);             // functor provided by class 'Block'
  d_sizeStats(potentialProduction);          // functor provided by class 'Block'

} // function 'TeasCcgtCapture::washup'

//  end of file

