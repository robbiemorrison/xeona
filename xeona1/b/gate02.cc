//  XEONA ENERGY SYSTEMS MODELING ENVIRONMENT
//
//  file-create-name : gate02.cc
//  file-create-date : Fri 19-Feb-2010 15:12 UTC
//  file-initiator   : Robbie Morrison <robbie@actrix.co.nz>
//
//  file-role        : concrete gateways 2 - exotic / implementation
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
//  $URL: file:///home/robbie/svn-root/xeona/futz/trunk/xeona1/b/gate02.cc $
//
//  GENERAL NOTES FOR THIS FILE

//  LOCAL AND SYSTEM INCLUDES

#include "gate02.h"           // companion header for this file (place first)

#include "../c/recset.h"      // records and fields and also record-sets
#include "../c/label.h"       // helper class to format solver labels
#include "../c/conex.h"       // create and connect block interfaces
#include "../b/optprob.h"     // optimization sub-problem and key sub-classes
#include "../b/optgate.h"     // various OSPs for gateways
#include "../b/domcon.h"      // domain controller entity
#include "../b/commods.h"     // commodities hierarchy
#include "../b/bandtaf.h"     // banded tariff set and support

#include "../a/logger.h"      // standard logging functionality (as required)
#include "../c/smart_ptr.h"   // toggle between Boost and TR1 smart pointers
#include ".././common.h"      // common definitions for project (place last)

#include <string>             // C++ strings
#include <sstream>            // string-streams

//  CODE

// ---------------------------------------------------------
//  CLASS           : GateStochasticPrice <>
// ---------------------------------------------------------

// ---------------------------------------------------------
//  MEMBER FUNCTION : GateStochasticPrice <>
// ---------------------------------------------------------

// the modeler sets the upper and lower price bounds

template <typename C>
GateStochasticPrice<C>::GateStochasticPrice
(const std::string entityId,
 Record&           record,
 const int         commitmentModeSum) :      // note default
   Gateway(entityId, record, commitmentModeSum),
   GateCom<C>(entityId, record, commitmentModeSum),
   d_specFixedCharge(0.0),
   d_unitPriceLo(0.0),
   d_unitPriceHi(0.0),
   d_contractualCapacity(0.0),
   d_volatile()
{
  s_logger->repx(logga::dbug, "constructor call", getIdAndKind());
  GateCom<C>::d_builtinRemark = "very incomplete";     // CAUTION: note scope resolution
}

// ---------------------------------------------------------
//  MEMBER FUNCTION : constrainSelSide <>
// ---------------------------------------------------------

template <typename C>
const int
GateStochasticPrice<C>::constrainSelSide
(const xeona::DomainMode capacityMode)
{
  // define some local variables for convenience
  shared_ptr<svif::SolverIf> solver = GateCom<C>::BuySide::d_solver;
  const xeona::DomainMode mode      = GateCom<C>::BuySide::d_commitmentMode;
  const int step                    = GateCom<C>::BuySide::d_step;

  // create a stochastic price
  std::pair<double, double> stochasticTariff;     // tariff type
  const double stochasticUnitPrice =xeona::getRandom (d_unitPriceLo, d_unitPriceHi);
  stochasticTariff = std::make_pair(d_contractualCapacity, stochasticUnitPrice);

  // reload the tariff set
  d_volatile->clear();
  d_volatile->setLabel("TOFIX");
  d_volatile->setSpecificFixedCharge(d_specFixedCharge);
  d_volatile->pushTariff(stochasticTariff);

  // create new contract OSP of the required type
  shared_ptr<OfrTariffSet> taf(new OfrTariffSet(solver, mode, ""));
  taf->loadOspLabel(getIdentifier() + "-tariff");

  // load the OSP
  taf->uploadTariffSet(d_volatile, d_capacity);

  // tofix: stochastic price gateway: complete the code

  return 88810;
}

// ---------------------------------------------------------
//  CLASS           : GateUnilateralTariff <>
// ---------------------------------------------------------

// tariff set is given by either the seller or buyer on the
// capacity or quantity sweep respectively

// ---------------------------------------------------------
//  MEMBER FUNCTION : setTariffset <>
// ---------------------------------------------------------

template <typename C>
void
GateUnilateralTariff<C>::setTariffset
(const shared_ptr<BandedTariffSet> tariffset)
{
  d_tariffset = tariffset;
}

// ---------------------------------------------------------
//  CLASS           : GateNegotiatedTariff <>
// ---------------------------------------------------------

//  end of file

