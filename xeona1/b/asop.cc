//  XEONA ENERGY SYSTEMS MODELING ENVIRONMENT
//
//  file-create-name : assop.cc
//  file-create-date : Mon 25-Aug-2008 12:32 UTC
//  file-initiator   : Robbie Morrison <robbie@actrix.co.nz>
//
//  file-role        : asset operator entity / implementation
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
//  $URL: file:///home/robbie/svn-root/xeona/futz/trunk/xeona1/b/asop.cc $

//  LOCAL AND SYSTEM INCLUDES

#include "asop.h"             // companion header for this file (place first)

#include "../b/teas.h"        // technical asset entity
#include "../b/node.h"        // LMP node entity
#include "../b/gate.h"        // gateway entity
#include "../b/entity.h"      // entity base class plus lazy linking
#include "../b/actor.h"       // actor entity

#include "../c/recset.h"      // records and fields and also record-sets
#include "../d/siglp.h"       // semi-intelligent interface to GLPK MILP solver
#include "../a/logger.h"      // standard logging functionality (as required)
#include "../c/smart_ptr.h"   // toggle between Boost and TR1 smart pointers
#include ".././common.h"      // common definitions for project (place last)

#include <string>             // C++ strings
#include <sstream>            // string-streams

#include <boost/foreach.hpp>  // BOOST_FOREACH iteration macro

//  CODE

// ---------------------------------------------------------
//  MEMBER FUNCTION : AssetOperator
// ---------------------------------------------------------

AssetOperator::AssetOperator
(const std::string entityId,
 Record&           record,
 const int         commitmentModeSum) :
  CostRegister(record),
  Actor(entityId, record),
  TicToc(commitmentModeSum),
  d_technical_assets(record.tieSingle<std::string>("technical-assets")),
  d_technicalAssets(),                       // empty vector
  d_lmpNodes()
{
  s_logger->repx(logga::xtra, "constructor call", getIdAndKind());
}

// ---------------------------------------------------------
//  MEMBER FUNCTION : ~AssetOperator
// ---------------------------------------------------------

AssetOperator::~AssetOperator()
{
  s_logger->repx(logga::adhc, "destructor call", getIdAndKind());
}

// DOMAIN CONTROLLER POINTS OF ENTRY

// ---------------------------------------------------------
//  MEMBER FUNCTION : getTechnicalAssets
// ---------------------------------------------------------

std::vector<shared_ptr<TechnicalAsset> >
AssetOperator::getTechnicalAssets()
{
  s_logger->repx(logga::adhc, "entering member function", "");
  d_technicalAssets.clear();
  FullEntity::listToVec<TechnicalAsset>(d_technical_assets, d_technicalAssets);
  return d_technicalAssets;
}

// ---------------------------------------------------------
//  MEMBER FUNCTION : getLmpNodes
// ---------------------------------------------------------

std::vector<shared_ptr<LmpNode> >
AssetOperator::getLmpNodes()
{
  s_logger->repx(logga::adhc, "entering member function", "");
  return d_lmpNodes;
}

// ---------------------------------------------------------
//  MEMBER FUNCTION : establish
// ---------------------------------------------------------

void
AssetOperator::establish()
{
  // preamble
  s_logger->repx(logga::dbug, "entering member function", "AssetOperator");

  // CAUTION: if required, place own stuff further down the
  // inheritance hierarchy and call this function first:
  // AssetOperator::establish()

  // do own stuff
  CostRegister::resetRegister();             // reset entire register

  // load, report, and step thru technical assets in no required order
  d_technicalAssets.clear();
  const int technicalAssetCnt
    = FullEntity::listToVec<TechnicalAsset>(d_technical_assets, d_technicalAssets);
  s_logger->repx(logga::adhc, "technical asset count", technicalAssetCnt);
  BOOST_FOREACH( shared_ptr<TechnicalAsset> ta, d_technicalAssets )
    {
      ta->CostRegister::resetRegister();     // reset entire register
      ta->establish();
    }

  // step thru LMP nodes in no required order
  // CAUTION: load will take place down the inheritance chain as appropriate
  BOOST_FOREACH( shared_ptr<LmpNode> ln, d_lmpNodes )
    {
      ln->establish();
    }
}

// ---------------------------------------------------------
//  MEMBER FUNCTION : restructure
// ---------------------------------------------------------

void
AssetOperator::restructure
(const xeona::DomainMode commitmentMode)
{
  // step thru technical assets in no required order
  BOOST_FOREACH( shared_ptr<TechnicalAsset> ta, d_technicalAssets )
    {
      ta->restructure(commitmentMode);
    }

  // step thru LMP nodes in no required order
  BOOST_FOREACH( shared_ptr<LmpNode> ln, d_lmpNodes )
    {
      ln->restructure(commitmentMode);
    }

  // do own stuff
  TicToc::restructure(commitmentMode);
}

// ---------------------------------------------------------
//  MEMBER FUNCTION : initialize
// ---------------------------------------------------------

void
AssetOperator::initialize
(const int                  step,            // CAUTION: the step count is ZERO-based
 shared_ptr<svif::SolverIf> solver)          // solver instance passed thru
{
  // step thru technical assets in no required order
  BOOST_FOREACH( shared_ptr<TechnicalAsset> ta, d_technicalAssets )
    {
      ta->initialize(step, solver);
    }

  // step thru LMP nodes in no required order
  BOOST_FOREACH( shared_ptr<LmpNode> ln, d_lmpNodes )
    {
      ln->initialize(step, solver);
    }

  // do own stuff
  TicToc::initialize(step, solver);
}

// ---------------------------------------------------------
//  MEMBER FUNCTION : washup
// ---------------------------------------------------------

void
AssetOperator::washup()
{
  // CAUTION: if required, place own stuff further down the
  // inheritance hierarchy and call this function first:
  // AssetOperator::washup()

  // step thru technical assets in no required order
  BOOST_FOREACH( shared_ptr<TechnicalAsset> ta, d_technicalAssets )
    {
      ta->washup();
    }

  // step thru LMP nodes in no required order
  BOOST_FOREACH( shared_ptr<LmpNode> ln, d_lmpNodes )
    {
      ln->washup();
    }
}

// ---------------------------------------------------------
//  MEMBER FUNCTION : conclude
// ---------------------------------------------------------

void
AssetOperator::conclude()
{
  // CAUTION: if required, place own stuff further down the
  // inheritance hierarchy and call this function first:
  // AssetOperator::conclude()

  // initial reporting
  s_logger->repx(logga::adhc, "entering member function", getIdAndKind());

  // step thru technical assets in no required order
  BOOST_FOREACH( shared_ptr<TechnicalAsset> ta, d_technicalAssets )
    {
      ta->conclude();
    }

  // step thru LMP nodes in no required order
  BOOST_FOREACH( shared_ptr<LmpNode> ln, d_lmpNodes )
    {
      ln->conclude();
    }

  // final reporting
  const int lmpTeasCnt = d_technicalAssets.size();
  const int lmpNodeCnt = d_lmpNodes.size();
  std::ostringstream oss;
  oss << lmpTeasCnt << " " << lmpNodeCnt;
  s_logger->repx(logga::adhc, "leaving function, teas, nodes", oss.str());
}

// ---------------------------------------------------------
//  MEMBER FUNCTION : setCogenHeatWeight (virtual)
// ---------------------------------------------------------

void
AssetOperator::setCogenHeatWeight
(const double cogenHeatLeadWeight)
{
  s_logger->repx(logga::adhc, "entering member function, weighting", cogenHeatLeadWeight);
  s_logger->repx(logga::warn, "function not overwritten", "");
}

// ---------------------------------------------------------
//  MEMBER FUNCTION : getCogenHeatWeight (virtual)
// ---------------------------------------------------------

const double
AssetOperator::getCogenHeatWeight() const
{
  const double invalid = -1.0;
  s_logger->repx(logga::adhc, "entering member function", "");
  s_logger->repx(logga::warn, "function not overwritten", "");
  s_logger->repx(logga::dbug, "about to return invalid value", invalid);
  return invalid;
}

//  end of file

