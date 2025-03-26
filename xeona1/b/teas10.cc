//  XEONA ENERGY SYSTEMS MODELING ENVIRONMENT
//
//  file-create-name : teas10.cc
//  file-create-date : Mon 04-Apr-2011 10:19 UTC
//  file-initiator   : Robbie Morrison <robbie@actrix.co.nz>
//
//  file-role        : concrete technical assets 10 / implementation
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
//  $URL: file:///home/robbie/svn-root/xeona/futz/trunk/xeona1/b/teas10.cc $
//
//  GENERAL NOTES FOR THIS FILE

//  LOCAL AND SYSTEM INCLUDES

#include "teas10.h"           // companion header for this file (place first)

#include "../c/util3.h"       // free functions for floating point comparison
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

#include <limits>             // numeric_limits<T>::infinity() and similar
#include <string>             // C++ strings
#include <sstream>            // string-streams

#include <boost/math/special_functions/sign.hpp>  // sign manipulation, sign() etc

//  CODE

// ---------------------------------------------------------
//  CLASS           : TeasSimpleStorage <>
// ---------------------------------------------------------

// ---------------------------------------------------------
//  MEMBER FUNCTION : TeasSimpleStorage<C>
// ---------------------------------------------------------

template <typename C>                        // 'C' for commodity
TeasSimpleStorage<C>::TeasSimpleStorage
(const std::string entityId,
 Record&           record) :
  CostRegister(record),
  TechnicalAsset(entityId, record, xeona::e_commitmentModes),
  CostRegisterSRFin(record),
  CostRegisterEmbFin(record),
  d_commodity(record.tieSingle<std::string>("stored-commodity")),
  d_roundTripEffy(record.tieSingle<double>("round-trip-effy")),
  d_halfLife(record.tieSingle<double>("half-life")),
  d_storageCapacity(record.tieSingle<double>("capacity")),
  d_openingStorage(record.tieSingle<double>("opening-storage")),
  d_rechargeRate(record.tieSingle<double>("recharge-rate")),
  d_dischargeRate(record.tieSingle<double>("discharge-rate")),
  d_spillFlag(record.tieSingle<bool>("spill-flag")),
  d_dischargeUnitCost(record.tieSingle<double>("discharge-unit-cost")),
  d_modes(record.tieTimeseries<int>("modes")),
  d_charges(record.tieTimeseries<double>("charges")),
  d_closingInventorys(record.tieTimeseries<double>("closing-inventorys")),
  d_inCm(Cable<C>::create
         (entityId,                          // me
          record.tieSingle<std::string>("socket"),
          d_commodity)),                     // common value
  d_outCm(Socket<C>::create
          (entityId,                         // me
           "sock-1",                         // hard-coded socket label
           d_commodity)),
  d_openingInventory(0.0),
  d_ops(),
  d_dischargeSpecCosts(d_dischargeUnitCost,  // fin (financial)
                       0.0,
                       0.0,
                       0.0,
                       0.0)
{
  // initial reporting
  s_logger->repx(logga::xtra, "constructor call", getIdAndKind());

  // integrity checks
  if ( d_halfLife == 0.0 )
    {
      s_logger->repx(logga::adhc, "no decay (due to zero half-life)", d_halfLife);
    }
  else if ( d_halfLife < 0.0 )
    {
      s_logger->repx(logga::warn, "negative half-life", d_halfLife);
    }
  else if ( d_halfLife < 3600 )              // 50% decay in under one hour
    {
      s_logger->repx(logga::rankJumpy, "suprisingly short half-life", d_halfLife);
    }
  if ( d_openingStorage < 0.0 || d_openingStorage > 1.0 )
    {
      s_logger->repx(logga::warn, "opening storage not [0,1]", d_openingStorage);
    }

  // set opening inventory
  d_openingInventory = d_openingStorage * d_storageCapacity;
  s_logger->repx(logga::adhc, "opening inventory", d_openingInventory);

  // builtin remark
  d_builtinRemark = "beta";
}

// ---------------------------------------------------------
//  MEMBER FUNCTION : ~TeasSimpleStorage<C>
// ---------------------------------------------------------

template <typename C>                        // 'C' for commodity
TeasSimpleStorage<C>::~TeasSimpleStorage()
{
  s_logger->repx(logga::adhc, "destructor call", getIdAndKind());
}

// ---------------------------------------------------------
//  MEMBER FUNCTION : establish
// ---------------------------------------------------------

template <typename C>
void
TeasSimpleStorage<C>::establish()
{
  // initial reporting
  s_logger->repx(logga::adhc, "entering member function", "");
}

// ---------------------------------------------------------
//  MEMBER FUNCTION : constrain
// ---------------------------------------------------------
//  Description  : technical asset constrain call
//  Role         : characterize current engineering and short-run costs
//  Techniques   : exponential decay
//  Status       : complete
//
//  Design notes
//
//      The 'normalizedDecay' variable is the one minus the
//      normalized amount remaining, calculated over the
//      interval:
//
//                                  interval
//                              1   --------
//          normalizedDecay = ( - ) halfLife
//                              2
//
//      For example, the normalized-decay, for a one hour
//      interval and a half-life of 4 weeks, equates to 0.10%.
//
//      Duty costs are only applied on discharge.  Duty costs
//      could also be applied on recharge if required -- in which
//      case, the modification should be self-evident.
//
// ---------------------------------------------------------

template <typename C>
const int                                    // duty gol
TeasSimpleStorage<C>::constrain
(const xeona::DomainMode capacityMode)
{
  // initial reporting
  s_logger->repx(logga::adhc, "entering member function", "TeasSimpleStorage");

  // preamble
  const int    interval = Entity::getHorizonInterval();
  const double inf      = std::numeric_limits<double>::infinity();

  // create and fill a label object
  const std::string teasId = getIdentifier();
  Label lab(teasId);

  // recreate new operations OSP of the required type
  d_ops.reset(new OpsStore(d_solver, d_commitmentMode));
  d_ops->loadOspLabel(lab.str());

  // define global cols for internal use
  int recGol = -1;                           // nonsensical value
  int disGol = -1;                           // nonsensical value

  // determine the opening inventory
  double openingInventory = 0.0;
  if ( d_step == 0 )                         // first step
    {
      openingInventory = d_openingInventory;
    }
  else                                       // remainder of simulation
    {
      openingInventory = d_closingInventorys->at(d_step - 1);    // previous step
    }

  // determine the decay over the interval
  double normalizedDecay = 0.0;
  if ( d_halfLife == 0.0 )                   // semantics for no decay [1]
    {
      normalizedDecay = 0.0;
    }
  else
    {
      // assume exponential decay
      const double period = static_cast<double>(interval);
      normalizedDecay = 1.0 - std::pow(0.5, period / d_halfLife);
    }

  // other values
  const double loCapacity = 0.0;             // zero is reasonable
  const double hiCapacity = d_storageCapacity;

  // upload the engineering
  boost::tie(recGol,                         // recharge stream
             disGol)                         // discharge stream
    = d_ops->uploadEngineering(d_spillFlag,
                               openingInventory,
                               loCapacity,
                               hiCapacity,
                               d_rechargeRate  == -1.0 ? inf : d_rechargeRate,  // [2]
                               d_dischargeRate == -1.0 ? inf : d_dischargeRate, // [2]
                               d_roundTripEffy,
                               normalizedDecay,
                               interval);    // interval length in [s]

  // [1] CAUTION: a half-life value of 0.0 means no decay will
  // occur (as the literal meaning is that of instantaneous
  // destruction)
  //
  // [2] CAUTION: a value of -1.0 means disregard this
  // restriction altogether, noting that 'OpsStore' tests for
  // 'inf's and omits the constraint if an infinity is found

  // upload specific costs -- including increment the "shift" term
  d_dutySpecCosts += d_dischargeSpecCosts;
  d_ops->uploadShortrunCosts(d_dutySpecCosts, disGol);      // actioned on discharge
  d_ops->uploadShortrunCosts(d_sizeSpecCosts * d_nameplateSize + d_standingCosts);

  // bind global cols to the relevant interfaces
  d_inCm ->bindOsp(d_solver, recGol);
  d_outCm->bindOsp(d_solver, disGol);

  // store duty values
  d_floorDuty   = loCapacity;
  d_ceilingDuty = hiCapacity;

  // return the discharge gol
  return disGol;
}

// ---------------------------------------------------------
//  MEMBER FUNCTION : washup
// ---------------------------------------------------------
//  Description  : technical asset washup call
//  Role         : recover and record solution
//  Techniques   : 'boost::math::sign'
//  Status       : complete
// ---------------------------------------------------------

template <typename C>
void
TeasSimpleStorage<C>::washup()
{
  // initial reporting
  s_logger->repx(logga::dbug, "entering member function, step", d_step);

  // results recovery
  double charge           =  0.0;            // neutral value, +ve for recharge
  double openingInventory = -1.0;            // nonsensical value
  double closingInventory = -1.0;            // nonsensical value
  boost::tie(charge,
             openingInventory,
             closingInventory) = d_ops->downloadSolution();

  // store entity state information
  // 'boost::math::sign' returns 1 if x > 0, -1 if x < 0, and 0 if x is zero
  d_modes->at(d_step)             = boost::math::sign(charge);
  d_charges->at(d_step)           = charge;
  d_closingInventorys->at(d_step) = closingInventory;

  // accrued costs processing
  CostSet varCosts(0.0);                               // transfer object
  CostSet fixCosts(0.0);                               // transfer object
  d_ops->downloadVarCosts(varCosts);                   // recover variable costs
  d_ops->downloadFixCosts(fixCosts);                   // recover fixed costs
  updateShortrunCosts(d_step, varCosts, fixCosts);     // utilize supplied data
  updateEmbeddedCosts(d_step);                         // utilize in-house calcs

  // store some on-the-fly statistics
  d_dutyStats(charge);                       // functor provided by class 'Block'
  d_sizeStats(d_storageCapacity);            // functor provided by class 'Block'

} // function 'TeasSimpleStorage::washup'

// ---------------------------------------------------------
//  MEMBER FUNCTION : conclude
// ---------------------------------------------------------

template <typename C>
void
TeasSimpleStorage<C>::conclude()
{
  // initial reporting
  s_logger->repx(logga::adhc, "entering member function", "");
}

// ---------------------------------------------------------
//  EXPLICIT TEMPLATE INSTANTIATIONS
// ---------------------------------------------------------

// CAUTION: explicit template instantiations must be placed at
// the very end of the implementation file
//
// For more information, see the excellent documentation in
// 'recset.cc' regarding explicit template instantiations.

// class 'Commodity' typedefs for convenience

typedef CmOxidize      Oxid;
typedef CmCarbonCert   Cert;
typedef CmCarbonSeq    Cseq;
typedef CmElectricity  Elec;
typedef CmWork         Work;
typedef CmHeat         Heat;
typedef CmThermalFluid Thrm;
typedef CmFunds        Fund;

// class 'TeasSimpleStorage<>'

template TeasSimpleStorage<Oxid>::TeasSimpleStorage(const std::string, Record&);
template TeasSimpleStorage<Cert>::TeasSimpleStorage(const std::string, Record&);
template TeasSimpleStorage<Cseq>::TeasSimpleStorage(const std::string, Record&);
template TeasSimpleStorage<Elec>::TeasSimpleStorage(const std::string, Record&);
template TeasSimpleStorage<Work>::TeasSimpleStorage(const std::string, Record&);
template TeasSimpleStorage<Heat>::TeasSimpleStorage(const std::string, Record&);
template TeasSimpleStorage<Thrm>::TeasSimpleStorage(const std::string, Record&);
template TeasSimpleStorage<Fund>::TeasSimpleStorage(const std::string, Record&);

template TeasSimpleStorage<Oxid>::~TeasSimpleStorage();
template TeasSimpleStorage<Cert>::~TeasSimpleStorage();
template TeasSimpleStorage<Cseq>::~TeasSimpleStorage();
template TeasSimpleStorage<Elec>::~TeasSimpleStorage();
template TeasSimpleStorage<Work>::~TeasSimpleStorage();
template TeasSimpleStorage<Heat>::~TeasSimpleStorage();
template TeasSimpleStorage<Thrm>::~TeasSimpleStorage();
template TeasSimpleStorage<Fund>::~TeasSimpleStorage();

template const int TeasSimpleStorage<Oxid>::constrain(const xeona::DomainMode);
template const int TeasSimpleStorage<Cert>::constrain(const xeona::DomainMode);
template const int TeasSimpleStorage<Cseq>::constrain(const xeona::DomainMode);
template const int TeasSimpleStorage<Elec>::constrain(const xeona::DomainMode);
template const int TeasSimpleStorage<Work>::constrain(const xeona::DomainMode);
template const int TeasSimpleStorage<Heat>::constrain(const xeona::DomainMode);
template const int TeasSimpleStorage<Thrm>::constrain(const xeona::DomainMode);
template const int TeasSimpleStorage<Fund>::constrain(const xeona::DomainMode);

template void TeasSimpleStorage<Oxid>::washup();
template void TeasSimpleStorage<Cert>::washup();
template void TeasSimpleStorage<Cseq>::washup();
template void TeasSimpleStorage<Elec>::washup();
template void TeasSimpleStorage<Work>::washup();
template void TeasSimpleStorage<Heat>::washup();
template void TeasSimpleStorage<Thrm>::washup();
template void TeasSimpleStorage<Fund>::washup();

// ---------------------------------
//  derived instantiations
// ---------------------------------

typedef CmOxidGas        OGas;
typedef CmOxidNaturalGas NatG;
typedef CmOxidBiocoal    BioC;
typedef CmOxidHydrogen   Htwo;

// class 'TeasSimpleStorage<>'

template TeasSimpleStorage<OGas>::TeasSimpleStorage(const std::string, Record&);
template TeasSimpleStorage<NatG>::TeasSimpleStorage(const std::string, Record&);
template TeasSimpleStorage<BioC>::TeasSimpleStorage(const std::string, Record&);
template TeasSimpleStorage<Htwo>::TeasSimpleStorage(const std::string, Record&);

template TeasSimpleStorage<OGas>::~TeasSimpleStorage();
template TeasSimpleStorage<NatG>::~TeasSimpleStorage();
template TeasSimpleStorage<BioC>::~TeasSimpleStorage();
template TeasSimpleStorage<Htwo>::~TeasSimpleStorage();

template const int TeasSimpleStorage<OGas>::constrain(const xeona::DomainMode);
template const int TeasSimpleStorage<NatG>::constrain(const xeona::DomainMode);
template const int TeasSimpleStorage<BioC>::constrain(const xeona::DomainMode);
template const int TeasSimpleStorage<Htwo>::constrain(const xeona::DomainMode);

template void TeasSimpleStorage<OGas>::washup();
template void TeasSimpleStorage<NatG>::washup();
template void TeasSimpleStorage<BioC>::washup();
template void TeasSimpleStorage<Htwo>::washup();

//  end of file

