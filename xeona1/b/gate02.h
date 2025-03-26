//  XEONA ENERGY SYSTEMS MODELING ENVIRONMENT
//
//  file-create-name : gate02.h
//  file-create-date : Fri 19-Feb-2010 15:12 UTC
//  file-initiator   : Robbie Morrison <robbie@actrix.co.nz>
//
//  file-role        : concrete gateways 2 -exotic / header
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
//  $URL: file:///home/robbie/svn-root/xeona/futz/trunk/xeona1/b/gate02.h $
//
//  GENERAL NOTES FOR THIS FILE

//  HEADER GUARD

#ifndef _GATE02_H_
#define _GATE02_H_

// =========================================================
//
//  CAUTION:
//
//  1. this file is not listed under 'sources' in 'makefile'
//  2. the classes here are not registered in 'b/register.cc'
//  3. this unit is, however, unit tested
//
// =========================================================

//  LOCAL AND SYSTEM INCLUDES

#include "../b/gate.h"        // gateway entity

#include "../b/tictoc.h"      // inherited interface for entities using common calls
#include "../b/block.h"       // block entity

#include "../a/logger_fwd.h"  // some forward declarations from "logger.h"
#include "../c/smart_ptr.h"   // toggle between Boost and TR1 smart pointers

#include <string>             // C++ strings
#include <sstream>            // string-streams

//  CODE

// ---------------------------------------------------------
//  CLASS           : GateStochasticPrice <>
// ---------------------------------------------------------
//  Description  : gateway with internally-generated stochastic pricing
//  Role         : concrete class
//  Techniques   : (nothing special)
//  Status       : incomplete -- implemented but needs checking
//
//  Design notes
//
//      The modeler sets the upper and lower price bounds.  The
//      actual price is sampled from a normal distribution.
//
// ---------------------------------------------------------

class OfrTariffSet_A;

template <typename C>
class GateStochasticPrice :
  public GateCom<C>
{
  // USING DECLARATIONS

protected:

  using Entity::s_logger;                    // place in common scope for this class
  using Entity::getIdentifier;               // place in common scope for this class
  using Entity::getIdAndKind;                // place in common scope for this class

  using Gateway::d_capacity;

  // TYPEDEFS

protected:

  typedef OfrTariffSet_A OfrTariffSet;       // used for switching implementations

  // CREATORS

public:

  explicit
  GateStochasticPrice
  (const std::string entityId,
   Record&           record,
   const int         commitmentModeSum = xeona::e_commitmentModes);

  const int
  constrainSelSide
  (const xeona::DomainMode capacityMode);

  // INTERNAL DATA

  // tariff-related input data

  double             d_specFixedCharge;      // invariant
  double             d_unitPriceLo;          // unit price low
  double             d_unitPriceHi;          // unit price high
  double             d_contractualCapacity;  // invariant

  shared_ptr<BandedTariffSet>    d_volatile; // filled locally

}; // class 'GateStochasticPrice<>'

//  ==== xedoc =================================================
//
//  entity.gate-stochastic-price-*-0
//
//        base commodity in {Cert,Cseq,Elec,Fund,Heat,Oxid,Thrm,Work}
//
//      class                                    > GateStochasticPrice:Work
//
//      builtin-remark s                         <
//
//        optional comment
//
//  ============================================================

// ---------------------------------------------------------
//  CLASS           : GateUnilateralTariff <>
// ---------------------------------------------------------
//  Description  : gateway with unilateral pricing setting
//  Role         : concrete class
//  Techniques   : (nothing special)
//  Status       : incomplete -- not implemented
//
//  Design notes
//
//      The tariff set is given by either the seller or buyer on
//      the capacity or quantity sweep respectively.
//
// ---------------------------------------------------------

template <typename C>
class GateUnilateralTariff :
  public GateCom<C>
{
public:

  void
  setTariffset
  (const shared_ptr<BandedTariffSet> tariffset);

private:

  shared_ptr<BandedTariffSet>    d_tariffset;

}; // class 'GateUnilateralTariff<>'

// ---------------------------------------------------------
//  CLASS           : GateNegotiatedTariff <>
// ---------------------------------------------------------
//  Description  : gateway with negotiated pricing setting
//  Role         : concrete class
//  Techniques   : (nothing special)
//  Status       : incomplete -- not implemented
//
//  Design notes
//
//      Requires that the two contract managers to be known and
//      in contact.
//
// ---------------------------------------------------------

template <typename C>
class GateNegotiatedTariff :
  public GateCom<C>
{

  // note the need for 'registerContractMgr' in class 'BuySide'
  // and 'SelSide', see also 'hop' code

}; // class 'GateNegotiatedTariff<>'

#endif // _GATE02_H_

//  end of file

