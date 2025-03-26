//  XEONA ENERGY SYSTEMS MODELING ENVIRONMENT
//
//  file-create-name : asop03.cc
//  file-create-date : Thu 26-Nov-2009 13:22 UTC
//  file-initiator   : Robbie Morrison <robbie@actrix.co.nz>
//
//  file-role        : concrete asset operators 3 / implementation
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
//  $URL: file:///home/robbie/svn-root/xeona/futz/trunk/xeona1/b/asop03.cc $
//
//  GENERAL NOTES FOR THIS FILE

//  LOCAL AND SYSTEM INCLUDES

#include "asop03.h"           // companion header for this file (place first)

#include "../c/tsops.h"       // overloaded operators and similar for timeseries
#include "../c/recset.h"      // records and fields and also record-sets
#include "../c/label.h"       // helper class to format solver labels
#include "../b/teas.h"        // technical asset entity
#include "../b/optctl.h"      // control optimization sub-problems for asset operators
#include "../b/node.h"        // LMP node entity
#include "../b/lmpbid.h"      // LMP auction bidset
#include "../b/entity.h"      // entity base class plus lazy linking
#include "../b/bandtaf.h"     // banded tariff set and support
#include "../a/exent.h"       // entity exception classes

#include "../a/logger.h"      // standard logging functionality (as required)
#include ".././common.h"      // common definitions for project (place last)

#include <string>             // C++ strings
#include <sstream>            // string-streams

#include <boost/foreach.hpp>  // BOOST_FOREACH iteration macro

//  CODE

// ---------------------------------------------------------
//  CLASS           : AsopInelasticTs
// ---------------------------------------------------------

// ---------------------------------------------------------
//  MEMBER FUNCTION : AsopInelasticTs
// ---------------------------------------------------------

AsopInelasticTs::AsopInelasticTs
(const std::string entityId,
 Record&           record) :
  CostRegister(record),
  AssetOperator(entityId, record, xeona::e_commitmentModes),
  d_demands(record.tieTimeseries<double>("demands")),
  d_ctl(),                                   // empty shared pointer
  d_ctls()                                   // empty vector
{
  s_logger->repx(logga::xtra, "constructor call", getIdAndKind());
  d_builtinRemark = "beta";
}

// ---------------------------------------------------------
//  MEMBER FUNCTION : ~AsopInelasticTs
// ---------------------------------------------------------

AsopInelasticTs::~AsopInelasticTs()
{
  s_logger->repx(logga::adhc, "destructor call", getIdAndKind());
}

// DOMAIN CONTROLLER POINTS OF ENTRY

// ---------------------------------------------------------
//  MEMBER FUNCTION : constrain
// ---------------------------------------------------------
//  Description  : constrain call
//  Role         : standard call
//  Techniques   : duty coupling
//  Status       : complete
// ---------------------------------------------------------

const int                          // number of technical assets processed
AsopInelasticTs::constrain
(const xeona::DomainMode capacityMode)
{
  // initial reporting
  s_logger->repx(logga::adhc, "entering member function", "");

  // preamble
  const std::string asopId = getIdentifier();

  // clear previous OSPs because each invocation produces a new set of OSPs
  d_ctls.clear();                            // strictly necessary

  // demand value
  const double demand = d_demands->at(d_step);

  // declare some administration counters
  int teasLoops = 0;                         // number of technical assets processed

  // step thru technical assets in no required order
  BOOST_FOREACH( shared_ptr<TechnicalAsset> ta, d_technicalAssets )
    {
      // keep count
      ++teasLoops;
      xeona::putxId(ta, "AsopInelasticTs::constrain technical asset loop");

      // create and fill a label object
      Label lab(asopId);

      // constrain the associated technical asset
      const int opsDutyGol = ta->constrain(capacityMode);

      // bid coupling is required
      if ( opsDutyGol == 0 )
        {
          const std::string teasId = ta->getIdentifier();
          s_logger->repx(logga::warn, "hollow coupling encountered, gol", opsDutyGol);
          s_logger->repx(logga::xtra, "abandoning current asset processing", teasId);
          continue;
        }

      // recreate and label new control OSP of the required type
      d_ctl.reset(new CtlQuan(d_solver, d_commitmentMode));
      d_ctl->loadOspLabel(lab.str("inelastic-ts"));

      // upload demand quantity
      int ctlDutyGol = -1;                   // nonsensical value
      boost::tie(ctlDutyGol) = d_ctl->uploadControl(demand);

      // OSP couple call (from unit 'b/optprob')
      xeona::couple(d_solver,
                    opsDutyGol,
                    ctlDutyGol,
                    lab.str("couple"));

      // store some information
      d_ctls.push_back(d_ctl);

    } // technical assets loop

  // return combined count
  return teasLoops;

} // function 'AsopInelasticTs::constrain'

// ---------------------------------------------------------
//  CLASS           : AsopInelasticParam
// ---------------------------------------------------------

// ---------------------------------------------------------
//  MEMBER FUNCTION : AsopInelasticParam
// ---------------------------------------------------------

AsopInelasticParam::AsopInelasticParam
(const std::string entityId,
 Record&           record) :
  CostRegister(record),
  AssetOperator(entityId, record, xeona::e_commitmentModes),
  d_demandMean(record.tieSingle<double>("demand-mean")),
  d_demandAmplitude(record.tieSingle<double>("demand-amplitude")),
  d_temporalShift(record.tieSingle<int>("temporal-shift")),
  d_demandRandomness(record.tieSingle<double>("demand-randomness")),
  d_demands(new std::vector<double>()),
  d_ctl(),                                   // empty shared pointer
  d_ctls()                                   // empty vector
{
  // initial reporting
  s_logger->repx(logga::xtra, "constructor call", getIdAndKind());
  d_builtinRemark = "beta";

  // integrity checks
  if ( d_temporalShift < -24 || d_temporalShift > 24 )
    {
      s_logger->repx(logga::rankJumpy, "temporal shift not [-24,+24]", d_temporalShift);
    }
  if ( d_demandRandomness < 0.0 )
    {
      s_logger->repx(logga::warn, "demand randomness is negative", d_demandRandomness);
    }

  // create the demand timeseries
  const int size = xeona::vectorFillDiurnal(d_demands,      // see unit 'c/tsops'
                                            d_demandMean,
                                            d_demandAmplitude,
                                            d_temporalShift,
                                            d_demandRandomness,
                                            Entity::getHorizonIntsPerDay(),
                                            Entity::getHorizonSteps());
  s_logger->repx(logga::adhc, "loaded diurnal vector, size", size);

  // completion reporting
  s_logger->repx(logga::adhc, "leaving member function", "");

} // function 'AsopInelasticParam::AsopInelasticParam'

// ---------------------------------------------------------
//  MEMBER FUNCTION : ~AsopInelasticParam
// ---------------------------------------------------------

AsopInelasticParam::~AsopInelasticParam()
{
  s_logger->repx(logga::adhc, "destructor call", getIdAndKind());
}

// DOMAIN CONTROLLER POINTS OF ENTRY

// ---------------------------------------------------------
//  MEMBER FUNCTION : constrain
// ---------------------------------------------------------
//  Description  : constrain call
//  Role         : standard call
//  Techniques   : duty coupling
//  Status       : complete
// ---------------------------------------------------------

const int                          // number of technical assets processed
AsopInelasticParam::constrain
(const xeona::DomainMode capacityMode)
{
  // initial reporting
  s_logger->repx(logga::adhc, "entering member function", "");

  // preamble
  const std::string asopId = getIdentifier();

  // clear previous OSPs because each invocation produces a new set of OSPs
  d_ctls.clear();                            // strictly necessary

  // demand value
  const double demand = d_demands->at(d_step);

  // declare some administration counters
  int teasLoops = 0;                         // number of technical assets processed

  // step thru technical assets in no required order
  BOOST_FOREACH( shared_ptr<TechnicalAsset> ta, d_technicalAssets )
    {
      // keep count
      ++teasLoops;
      xeona::putxId(ta, "AsopInelasticParam::constrain technical asset loop");

      // create and fill a label object
      Label lab(asopId);

      // constrain the associated technical asset
      const int opsDutyGol = ta->constrain(capacityMode);

      // bid coupling is required
      if ( opsDutyGol == 0 )
        {
          const std::string teasId = ta->getIdentifier();
          s_logger->repx(logga::warn, "hollow coupling encountered, gol", opsDutyGol);
          s_logger->repx(logga::xtra, "abandoning current asset processing", teasId);
          continue;
        }

      // recreate and label new control OSP of the required type
      d_ctl.reset(new CtlQuan(d_solver, d_commitmentMode));
      d_ctl->loadOspLabel(lab.str("inelastic-ts"));

      // upload demand quantity
      int ctlDutyGol = -1;                   // nonsensical value
      boost::tie(ctlDutyGol) = d_ctl->uploadControl(demand);

      // OSP couple call (from unit 'b/optprob')
      xeona::couple(d_solver,
                    opsDutyGol,
                    ctlDutyGol,
                    lab.str("couple"));

      // store some information
      d_ctls.push_back(d_ctl);

    } // technical assets loop

  // return combined count
  return teasLoops;

} // function 'AsopInelasticParam::constrain'

// ---------------------------------------------------------
//  CLASS           : AsopAdaptiveTs
// ---------------------------------------------------------

// ---------------------------------------------------------
//  MEMBER FUNCTION : AsopAdaptiveTs
// ---------------------------------------------------------

AsopAdaptiveTs::AsopAdaptiveTs
(const std::string entityId,
 Record&           record) :
  CostRegister(record),
  AssetOperator(entityId, record, xeona::e_commitmentModes),
  d_demands(record.tieTimeseries<double>("demands")),
  d_unitPriceThreshold(record.tieSingle<double>("unit-price-threshold")),
  d_adaptFactor(record.tieSingle<double>("adapt-factor")),
  d_curtailments(record.tieTimeseries<bool>("curtailments")),
  d_ctl(),                                   // empty shared pointer
  d_ctls()                                   // empty vector
{
  s_logger->repx(logga::xtra, "constructor call", getIdAndKind());
  d_builtinRemark = "beta";

  // integrity checks
  if ( d_adaptFactor < 0.0 )
    {
      s_logger->repx(logga::warn, "negative adapt factor", d_adaptFactor);
    }
  else if ( d_adaptFactor > 1.0 )
    {
      s_logger->repx(logga::info, "above unity adapt factor", d_adaptFactor);
    }
}

// ---------------------------------------------------------
//  MEMBER FUNCTION : ~AsopAdaptiveTs
// ---------------------------------------------------------

AsopAdaptiveTs::~AsopAdaptiveTs()
{
  s_logger->repx(logga::adhc, "destructor call", getIdAndKind());
}

// DOMAIN CONTROLLER POINTS OF ENTRY

// ---------------------------------------------------------
//  MEMBER FUNCTION : constrain
// ---------------------------------------------------------
//  Description  : constrain call
//  Role         : standard call
//  Techniques   : duty coupling
//  Status       : complete
// ---------------------------------------------------------

const int                          // number of technical assets processed
AsopAdaptiveTs::constrain
(const xeona::DomainMode capacityMode)
{
  // initial reporting
  s_logger->repx(logga::adhc, "entering member function", "");

  // preamble
  const std::string asopId = getIdentifier();

  // clear previous OSPs because each invocation produces a new set of OSPs
  d_ctls.clear();                            // strictly necessary

  // demand value
  double demand = d_demands->at(d_step);

  // declare some administration counters
  int teasLoops = 0;                         // number of technical assets processed

  // step thru technical assets in no required order
  BOOST_FOREACH( shared_ptr<TechnicalAsset> ta, d_technicalAssets )
    {
      // keep count
      ++teasLoops;
      xeona::putxId(ta, "AsopAdaptiveTs::constrain technical asset loop");

      // adaptive behavior
      adapt1(ta, demand);

      // create and fill a label object
      Label lab(asopId);

      // constrain the associated technical asset
      const int opsDutyGol = ta->constrain(capacityMode);

      // bid coupling is required
      if ( opsDutyGol == 0 )
        {
          const std::string teasId = ta->getIdentifier();
          s_logger->repx(logga::warn, "hollow coupling encountered, gol", opsDutyGol);
          s_logger->repx(logga::xtra, "abandoning current asset processing", teasId);
          continue;
        }

      // recreate and label new control OSP of the required type
      d_ctl.reset(new CtlQuan(d_solver, d_commitmentMode));
      d_ctl->loadOspLabel(lab.str("inelastic-ts"));

      // upload demand quantity
      int ctlDutyGol = -1;                   // nonsensical value
      boost::tie(ctlDutyGol) = d_ctl->uploadControl(demand);

      // OSP couple call (from unit 'b/optprob')
      xeona::couple(d_solver,
                    opsDutyGol,
                    ctlDutyGol,
                    lab.str("couple"));

      // store some information
      d_ctls.push_back(d_ctl);

    } // technical assets loop

  // return combined count
  return teasLoops;

} // function 'AsopAdaptiveTs::constrain'

// ---------------------------------------------------------
//  MEMBER FUNCTION : adapt1
// ---------------------------------------------------------

bool
AsopAdaptiveTs::adapt1
(shared_ptr<TechnicalAsset> ta,
 double&                    demand)
{
  // initial reporting
  s_logger->repx(logga::adhc, "entering member function", "");

  // preamble
  const double old = demand;
  bool curtailment = false;

  // adaptive behavior
  shared_ptr<BandedTariffSet> tariffset = ta->obtainTariffSet();
  const double marginalPrice            = tariffset->getMarginalPrice(demand);
  if ( marginalPrice > d_unitPriceThreshold )
    {
      // modify current demand
      curtailment = true;
      demand *= d_adaptFactor;
    }
  else
    {
      curtailment = false;
    }

  // update curtailments data
  d_curtailments->at(d_step) = curtailment;

  // additional reporting as appropriate
  // YEEK 35 CODE (set by '--yeek')
  if ( xeona::yeek == 35 || xeona::yeek == 1 || xeona::yeek == 2 )
    {
      std::ostringstream put;
      put << "  curtailment : " << curtailment << "\n"
          << "  demand old  : " << old         << "\n"
          << "  demand new  : " << demand      << "\n";
      s_logger->repx(logga::dbug, "additional reporting follows, yeek", xeona::yeek);
      s_logger->putx(logga::dbug, put);
    }

  // return
  return curtailment;

} // function 'AsopAdaptiveTs::adapt1'

//  end of file

