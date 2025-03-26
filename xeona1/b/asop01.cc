//  XEONA ENERGY SYSTEMS MODELING ENVIRONMENT
//
//  file-create-name : asop1.cc
//  file-create-date : Wed 15-Apr-2009 21:52 UTC
//  file-initiator   : Robbie Morrison <robbie@actrix.co.nz>
//
//  file-role        : concrete asset operators 1 / implementation
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
//  $URL: file:///home/robbie/svn-root/xeona/futz/trunk/xeona1/b/asop01.cc $
//
//  GENERAL NOTES FOR THIS FILE

//  LOCAL AND SYSTEM INCLUDES

#include "asop01.h"           // companion header for this file (place first)

#include "../c/recset.h"      // records and fields and also record-sets
#include "../c/label.h"       // helper class to format solver labels
#include "../b/teas.h"        // technical asset entity
#include "../b/optctl.h"      // control optimization sub-problems for asset operators
#include "../a/exent.h"       // entity exception classes (at writing, just test purposes)

#include "../a/logger.h"      // standard logging functionality (as required)
#include ".././common.h"      // common definitions for project (place last)

#include <string>             // C++ strings
#include <sstream>            // string-streams

#include <boost/foreach.hpp>  // BOOST_FOREACH iteration macro

//  CODE

// ---------------------------------------------------------
//  CLASS           : AsopBasic
// ---------------------------------------------------------

// ---------------------------------------------------------
//  MEMBER FUNCTION : AsopBasic
// ---------------------------------------------------------

AsopBasic::AsopBasic
(const std::string entityId,
 Record&           record) :
  CostRegister(record),
  AssetOperator(entityId, record, xeona::e_commitmentModes)      // no restriction on mode
{
  s_logger->repx(logga::xtra, "constructor call", getIdAndKind());
  d_builtinRemark = "beta";
}

// ---------------------------------------------------------
//  MEMBER FUNCTION : ~AsopBasic
// ---------------------------------------------------------

AsopBasic::~AsopBasic()
{
  s_logger->repx(logga::adhc, "destructor call", getIdAndKind());
}

// ---------------------------------------------------------
//  MEMBER FUNCTION : AsopBasic::constrain
// ---------------------------------------------------------
//  Description  : constrain call
//  Role         : standard call
//  Techniques   : duty coupling not required
//  Status       : complete
// ---------------------------------------------------------

const int                                    // number of technical assets processesed
AsopBasic::constrain
(const xeona::DomainMode capacityMode)
{
  // initial reporting
  s_logger->repx(logga::adhc, "entering member function", "");

  // declare some administration counters
  int teasLoop = 0;                          // number of technical assets processed

  // step thru technical assets in no required order
  BOOST_FOREACH( shared_ptr<TechnicalAsset> ta, d_technicalAssets )
    {
      // keep count
      ++teasLoop;
      xeona::putxId(ta, "AsopBasic::constrain technical asset loop");

      // constrain the associated technical asset
      ta->constrain(capacityMode);
    }

  // special test for entity throw code
  // YEEK 8 CODE (set by '--yeek')
  if ( xeona::yeek == 8 )
    {
      s_logger->repx(logga::dbug, "entering yeek code", xeona::yeek);
      s_logger->repx(logga::warn, "this code is strictly for testing", "");
      s_logger->repx(logga::warn, "will throw xeona::entity_issue", "");
      const std::string msg = "test throw from asset operator '" + getIdAndKind() + "'\n";
      throw xeona::entity_issue(msg);
    }

  // housekeeping
  return teasLoop;
}

// ---------------------------------------------------------
//  CLASS           : AsopPrescribedOrder
// ---------------------------------------------------------

// ---------------------------------------------------------
//  MEMBER FUNCTION : AsopPrescribedOrder
// ---------------------------------------------------------

AsopPrescribedOrder::AsopPrescribedOrder
(const std::string entityId,
 Record&           record) :
  CostRegister(record),
  AssetOperator(entityId, record, xeona::e_adminMerit),
  AuxHeatLead(record),
  d_ctl(),                                   // empty shared pointer
  d_ctls()                                   // empty vector
{
  s_logger->repx(logga::xtra, "constructor call", getIdAndKind());

  d_builtinRemark = "beta";
  d_builtinRemark = "needs debugging";
}

// ---------------------------------------------------------
//  MEMBER FUNCTION : ~AsopPrescribedOrder
// ---------------------------------------------------------

AsopPrescribedOrder::~AsopPrescribedOrder()
{
  s_logger->repx(logga::adhc, "destructor call", getIdAndKind());
}

// ---------------------------------------------------------
//  MEMBER FUNCTION : AsopPrescribedOrder::constrain
// ---------------------------------------------------------
//  Description  : constrain call
//  Role         : standard call
//  Techniques   : duty coupling
//  Status       : complete
//
//  Design notes
//
//      Merit order uses a technical asset list
//
//          The order in which assets are listed in the technical
//          asset list defines the prescribed order used here.
//          (Whether this order represents "merit" or not is a
//          matter for the modeler!)
//
// ---------------------------------------------------------

const int                                    // number of technical assets processed
AsopPrescribedOrder::constrain
(const xeona::DomainMode capacityMode)
{
  // initial reporting
  s_logger->repx(logga::adhc, "entering member function", "");

  // preamble
  const std::string asopId = getIdentifier();

  // clear previous OSPs because each invokation produces a new set of OSPs
  d_ctls.clear();                            // strictly necessary

  // declare some administration counters
  int teasLoop = 0;                          // number of technical assets processed

  // declare a rank counter
  int rank     = 0;                          // rank sequence { 1, 2, 3, 4, .. }

  // step thru technical assets in no required order
  BOOST_FOREACH( shared_ptr<TechnicalAsset> ta, d_technicalAssets )
    {
      // keep count
      ++teasLoop;
      xeona::putxId(ta, "AsopPrescribedOrder::constrain technical asset loop");

      // create and fill a label object
      Label lab(asopId);

      // set the heat lead weighting (with 0.0 = power led and 1.0 = heat led)
      const double heatLeadtWeight = getCogenHeatLeadWeight();
      ta->setCogenHeatWeight(heatLeadtWeight);

      // constrain the associated technical asset (also set floor and ceiling duties)
      const int opsDutyGol = ta->constrain(capacityMode);

      // check for meaningful coupling or loop again
      if ( opsDutyGol == 0 ) continue;

      // recreate and label new control OSP of the required type
      d_ctl.reset(new CtlMeritOrder(d_solver, d_commitmentMode));
      d_ctl->loadOspLabel(lab.str());

      // obtain some technical asset information
      const double upperCapacity = ta->getCeilingDuty();

      // OSP upload call
      int ctlDutyGol;
      boost::tie(ctlDutyGol) = d_ctl->uploadRank(++rank,          // prescribed order
                                                 upperCapacity);  // "big M"-style code

      // OSP couple call (from unit 'b/optprob')
      xeona::couple(d_solver,
                    opsDutyGol,
                    ctlDutyGol,
                    lab.str("couple"));

      // store some information
      d_ctls.push_back(d_ctl);

    } // technical assets loop

  // housekeeping
  return teasLoop;
}

// ---------------------------------------------------------
//  CLASS           : AsopInternalCosts
// ---------------------------------------------------------

// ---------------------------------------------------------
//  MEMBER FUNCTION : AsopInternalCosts
// ---------------------------------------------------------

AsopInternalCosts::AsopInternalCosts
(const std::string entityId,
 Record&           record) :
  CostRegister(record),
  AssetOperator(entityId, record, xeona::e_shortrunModes),
  CostRegisterAsop(record),
  d_ctl(),                                   // empty shared pointer
  d_ctls()                                   // empty vector
{
  s_logger->repx(logga::xtra, "constructor call", getIdAndKind());
  d_builtinRemark = "beta";
}

// ---------------------------------------------------------
//  MEMBER FUNCTION : ~AsopInternalCosts
// ---------------------------------------------------------

AsopInternalCosts::~AsopInternalCosts()
{
  s_logger->repx(logga::adhc, "destructor call", getIdAndKind());
}

// ---------------------------------------------------------
//  MEMBER FUNCTION : AsopInternalCosts::constrain
// ---------------------------------------------------------
//  Description  : constrain call
//  Role         : standard call
//  Techniques   : duty coupling not required, own costs added
//  Status       : complete
//
//  Design notes
//
//      This asset operator does not need to overwrite the cost
//      minimization objective supplied by the technical assets
//      -- hence the lack of duty coupling code.
//
// ---------------------------------------------------------

const int                                    // number of technical assets processesed
AsopInternalCosts::constrain
(const xeona::DomainMode capacityMode)
{
  // initial reporting
  s_logger->repx(logga::adhc, "entering member function", "");

  // preamble
  const std::string asopId = getIdentifier();

  // clear previous OSPs because each invokation produces a new set of OSPs
  d_ctls.clear();                            // strictly not necessary in this case

  // declare some loop counters
  int teasLoop = 0;                          // number of technical assets processed

  // create a label object
  Label lab(asopId);
  lab << "constrain";

  // recreate and label new control OSP of the required type
  d_ctl.reset(new CtlLeastCost(d_solver, d_commitmentMode));
  d_ctl->loadOspLabel(lab.str());

  // upload specific costs -- in this case, the standing costs
  d_ctl->uploadShortrunCosts(d_standingCosts);    // loaded as incremental "shift" costs

  // store some information
  d_ctls.push_back(d_ctl);

  // step thru technical assets in no required order
  BOOST_FOREACH( shared_ptr<TechnicalAsset> ta, d_technicalAssets )
    {
      // keep count
      ++teasLoop;
      xeona::putxId(ta, "AsopInternalCosts::constrain technical asset loop");

      // constrain the associated technical asset
      ta->constrain(capacityMode);

    } // technical assets loop

  // housekeeping
  return teasLoop;

} // member function 'constrain'

// ---------------------------------------------------------
//  MEMBER FUNCTION : AsopInternalCosts::washup
// ---------------------------------------------------------

void
AsopInternalCosts::washup()
{
  // initial reporting
  s_logger->repx(logga::dbug, "entering member function, step", d_step);

  // base class call
  AssetOperator::washup();

  // standing costs processing
  updateStandingCosts(d_step);
}

//  end of file

