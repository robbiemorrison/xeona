//  XEONA ENERGY SYSTEMS MODELING ENVIRONMENT
//
//  file-create-name : teas09.cc
//  file-create-date : Thu 10-Mar-2011 12:58 UTC
//  file-initiator   : Robbie Morrison <robbie@actrix.co.nz>
//
//  file-role        : concrete technical assets 9 / implementation
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
//  $URL: file:///home/robbie/svn-root/xeona/futz/trunk/xeona1/b/teas09.cc $
//
//  GENERAL NOTES FOR THIS FILE

//  LOCAL AND SYSTEM INCLUDES

#include "teas09.h"           // companion header for this file (place first)

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

#include <typeinfo>           // run-time type information (RTTI)

//  CODE

// ---------------------------------------------------------
//  CLASS           : TeasCcsGeological
// ---------------------------------------------------------

// ---------------------------------------------------------
//  MEMBER FUNCTION : TeasCcsGeological
// ---------------------------------------------------------

TeasCcsGeological::TeasCcsGeological
(const std::string entityId,
 Record&           record) :
  CostRegister(record),
  TechnicalAsset(entityId, record, xeona::e_commitmentModes),
  CostRegisterSRFin(record),
  CostRegisterEmbFin(record),
  d_divisor(record.tieSingle<int>("divisor")),
  d_demandHiBound(record.tieSingle<double>("demand-hi-bound")),
  d_specificElectricityUsage(record.tieSingle<double>("specific-electricity-usage")),
  d_carbonDioxideLeakageRate(record.tieSingle<double>("carbon-dioxide-leakage-rate")),
  d_burials(record.tieTimeseries<double>("burials")),
  d_electricityDemands(record.tieTimeseries<double>("electricity-demands")),
  d_annualLeakageContribution(record.tieSingle<double>("annual-leakage-contribution")),
  d_inElec(Cable<CmElectricity>::create
           (entityId,                        // me
            record.tieSingle<std::string>("socket-electricity"),
            record.tieSingle<std::string>("cable-electricity-commodity"))),
  d_outCSeq(Socket<CmCarbonSeq>::create
            (entityId,                       // me
             "cseq-1",                       // hard-coded socket label
             record.tieSingle<std::string>("socket-carbon-seq-commodity"))),
  d_demandLoBound(0.0),
  d_cseq(),
  d_ops()
{
  // initial reporting
  s_logger->repx(logga::xtra, "constructor call", getIdAndKind());

  // builtin remark
  d_builtinRemark = "beta";

  // integrity checks
  if ( d_divisor == 0 )
    {
      s_logger->repx(logga::warn, "divisor cannot be zero", d_divisor);
      s_logger->repx(logga::warn, "div-by-zero problems ahead", "");
    }
  else if ( d_divisor < 0 )
    {
      s_logger->repx(logga::warn, "divisor must be strictly positive", d_divisor);
    }
  if ( d_specificElectricityUsage < 0.0 )
    {
      s_logger->repx(logga::warn,
                     "-ve specific electricity usage",
                     d_specificElectricityUsage);
    }
  if ( d_carbonDioxideLeakageRate < 0.0 )
    {
      s_logger->repx(logga::warn,
                     "-ve CO2 leakage rate [1/1000yr]",
                     d_carbonDioxideLeakageRate);
    }
  if ( d_carbonDioxideLeakageRate > 0.10 )
    {
      s_logger->repx(logga::rankJumpy,
                     "high CO2 leakage rate [1/1000yr]",
                     d_carbonDioxideLeakageRate);
    }

} // constructor

// ---------------------------------------------------------
//  MEMBER FUNCTION : ~TeasCcsGeological
// ---------------------------------------------------------

TeasCcsGeological::~TeasCcsGeological()
{
  s_logger->repx(logga::adhc, "destructor call", getIdAndKind());
}

// ---------------------------------------------------------
//  MEMBER FUNCTION : establish
// ---------------------------------------------------------
//  Description  : identify the commodity
//  Role         : beginning-of-horizon call
//  Techniques   : (nothing special)
//  Status       : complete
//
//  Design notes
//
//      As currently coded, 'd_cseq' is not actually used.
//
// ---------------------------------------------------------

void
TeasCcsGeological::establish()
{
  // initial reporting
  s_logger->repx(logga::adhc, "entering member function", "");
  const double descalingFactor = 1.0 / static_cast<double>(d_divisor);
  s_logger->repx(logga::adhc, "descaling factor (1/divisor)", descalingFactor);

  // CAUTION: 'Interface::getCm' can only be called AFTER the
  // interface connections are complete -- which is why this code
  // is here and not in the constructor

  // obtain fuel commodity
  const shared_ptr<Commodity> com = d_outCSeq->getCm();
  d_cseq                          = dynamic_pointer_cast<CmCarbonSeq>(com);

} // function 'TeasCcsGeological::establish'

// ---------------------------------------------------------
//  MEMBER FUNCTION : constrain
// ---------------------------------------------------------
//  Description  : technical asset constrain call
//  Role         : characterize current engineering and short-run costs
//  Techniques   : 'boost::tuples::ignore' to ignore tuple ties
//  Status       : complete
//
//  Design notes
//
//      The critical pressure for CO2 is 7.38e+06 Pa (74 bar) and
//      the critical temperature is 31.0 C.
//
//      It is assumed that no carbon dioxide is released from the
//      facility, either thru leakage or by combustion in some
//      form.
//
//      The descaling exercise using 'd_divisor' is entirely
//      linear.  This is reasonable because the purpose is to
//      descale for model building purposes and not to downsize
//      for engineering reasons.
//
// ---------------------------------------------------------

const int                                    // duty gol
TeasCcsGeological::constrain
(const xeona::DomainMode capacityMode)
{
  // initial reporting
  s_logger->repx(logga::adhc, "entering member function", "TeasCcsGeological");

  // create and fill a label object
  const std::string teasId = getIdentifier();
  Label lab(teasId);

  // recreate new operations OSP of the required type
  d_ops.reset(new OpsFixedEffy(d_solver, d_commitmentMode));
  d_ops->loadOspLabel(lab.str());

  // define global cols for internal use
  int  inGol = -1;                           // nonsensical value
  int outGol = -1;                           // nonsensical value

  // set efficiencies
  const double fixedEfficiency = 1.0 / d_specificElectricityUsage;
  const double noLoadLoss      = 0.0;        // simple hard-code here

  // descaling exercise
  const double  divisor = static_cast<double>(d_divisor);
  const double  deDemandLoBound = d_demandLoBound / divisor;
  const double  deDemandHiBound = d_demandHiBound / divisor;
  const double  nameplateSize   = d_nameplateSize / divisor;
  const double  deNameplateSize = nameplateSize   / divisor;
  const CostSet deStandingCosts = d_standingCosts / divisor;
  const double  deNoLoadLoss    = noLoadLoss      / divisor;

  // upload the engineering
  boost::tie(inGol,                          // factor (fuel) stream
             outGol)                         // output (product) stream
    = d_ops->uploadEngineering(deDemandLoBound,
                               deDemandHiBound,
                               fixedEfficiency,
                               deNoLoadLoss);

  // upload specific costs -- including increment the "shift" term
  d_ops->uploadShortrunCosts(d_dutySpecCosts, outGol);
  d_ops->uploadShortrunCosts(d_sizeSpecCosts * deNameplateSize + deStandingCosts);

  // bind global cols to the relevant interfaces
  d_inElec ->bindOsp(d_solver,  inGol);
  d_outCSeq->bindOsp(d_solver, outGol);

  // store duty values
  d_floorDuty   = deDemandLoBound;
  d_ceilingDuty = deDemandHiBound;

  // return the duty gol
  return outGol;

} // function 'TeasCcsGeological::constrain'

// ---------------------------------------------------------
//  MEMBER FUNCTION : washup
// ---------------------------------------------------------
//  Description  : technical asset washup call
//  Role         : recover and record solution
//  Techniques   : (nothing special)
//  Status       : complete
// ---------------------------------------------------------

void
TeasCcsGeological::washup()
{
  // initial reporting
  s_logger->repx(logga::dbug, "entering member function, step", d_step);

  // results recovery
  double electricityDemand = -1.0;           // nonsensical value
  double actualProduction  = -1.0;           // nonsensical value
  boost::tie(electricityDemand,
             actualProduction) = d_ops->downloadSolution();

  // store entity state information
  d_burials->at(d_step)            = actualProduction;
  d_electricityDemands->at(d_step) = electricityDemand;

  // another local variable
  const double potentialProduction = d_demandHiBound;

  // accrued costs processing
  CostSet varCosts(0.0);                               // transfer object
  CostSet fixCosts(0.0);                               // transfer object
  d_ops->downloadVarCosts(varCosts);                   // recover variable costs
  d_ops->downloadFixCosts(fixCosts);                   // recover fixed costs
  updateShortrunCosts(d_step, varCosts, fixCosts);     // utilize supplied data
  updateEmbeddedCosts(d_step);                         // utilize in-house calcs

  // ad-hoc reporting
  s_logger->repx(logga::adhc, "variable ghg cost [kg/s]", varCosts.ghg);
  s_logger->repx(logga::adhc, "fixed ghg cost [kg/s]",    fixCosts.ghg);

  // additional reporting as appropriate
  // YEEK 57 CODE (set by '--yeek')
  if ( xeona::yeek == 57 )
    {
      const int    interval      = Entity::getHorizonInterval();
      const double accruedVarGhg = varCosts.ghg * interval;
      const double accruedFixGhg = fixCosts.ghg * interval;
      std::ostringstream put;
      put << "  function : "                       << XEONA_FUNC          << "\n";
      put << "    " << "step                   : " << d_step              << "\n"
          << "    " << "carbon burial          : " << actualProduction    << "\n"
          << "    " << "carbon hi bound        : " << d_demandHiBound     << "\n"
          << "    " << "electricity demand     : " << electricityDemand   << "\n"
          << "    " << "accrued var ghg cost   : " << accruedVarGhg       << "\n"
          << "    " << "accrued fix ghg cost   : " << accruedFixGhg       << "\n";
      put << " " << varCosts.summarizeMeG("variable costs")               << "\n"
          << " " << fixCosts.summarizeMeG("fixed costs")                  << "\n";
      // as from commit r8249, this code should not be needed
      if ( xeona::isInf(accruedVarGhg) )     // plus or minus infinity
        {
          s_logger->repx(logga::warn, "accrued var ghg cost is +/-inf", accruedVarGhg);
        }
      if ( xeona::isInf(accruedFixGhg) )     // plus or minus infinity
        {
          s_logger->repx(logga::warn, "accrued fix ghg cost is +/-inf", accruedFixGhg);
        }
      s_logger->repx(logga::dbug, "additional reporting follows, yeek", xeona::yeek);
      s_logger->putx(logga::dbug, put);
    }

  // store some on-the-fly statistics
  d_dutyStats(actualProduction);             // functor provided by class 'Block'
  d_sizeStats(potentialProduction);          // functor provided by class 'Block'

} // function 'TeasCcsGeological::washup'

// ---------------------------------------------------------
//  MEMBER FUNCTION : conclude
// ---------------------------------------------------------
//  Description  : technical asset washup call
//  Role         : recover and record solution
//  Techniques   : (nothing special)
//  Status       : complete
// ---------------------------------------------------------

void
TeasCcsGeological::conclude()
{
  // initial reporting
  s_logger->repx(logga::dbug, "entering member function, step", d_step);

  // annual leakage contribution calculations
  const int    interval         = Entity::getHorizonInterval();
  const int    steps            = Entity::getHorizonSteps();

  const double co2lr            = d_carbonDioxideLeakageRate;    // for convenience
  const double elapsedSeconds   = interval * steps;
  const double yearFraction     = elapsedSeconds / (3600 * 8760);
  const double todateProduction = d_dutyStats.sum();
  const double annualProduction = todateProduction / yearFraction;
  const double annualLeakage    = annualProduction * (co2lr / 1000);
  d_annualLeakageContribution   = annualLeakage;

  // additional reporting as appropriate
  // YEEK 49 CODE (set by '--yeek')
  if ( xeona::yeek == 49 || xeona::yeek == 1 || xeona::yeek == 2 )
    {
      const double descalingFactor = 1.0 / static_cast<double>(d_divisor);
      std::ostringstream put;
      put << "  TeasCcsGeological conclude report"                              << "\n"
          << "    identifier                            : " << getIdAndKind()   << "\n"
          << "    descaling factor (1/divisor)      [-] : " << descalingFactor  << "\n"
          << "    elapsed seconds                   [s] : " << elapsedSeconds   << "\n"
          << "    total burial to date             [kg] : " << todateProduction << "\n"
          << "    leakage rate per 1000 years       [-] : " << co2lr            << "\n"
          << "    projected leakage contribution [kg/a] : " << annualLeakage    << "\n";
      s_logger->repx(logga::dbug, "additional reporting follows, yeek", xeona::yeek);
      s_logger->putx(logga::dbug, put);
    }

} // function 'TeasCcsGeological::conclude'

//  end of file

