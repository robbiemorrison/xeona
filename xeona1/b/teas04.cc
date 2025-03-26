//  XEONA ENERGY SYSTEMS MODELING ENVIRONMENT
//
//  file-create-name : teas04.cc
//  file-create-date : Thu 08-Oct-2009 12:58 UTC
//  file-initiator   : Robbie Morrison <robbie@actrix.co.nz>
//
//  file-role        : concrete technical assets 4 / implementation
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
//  $URL: file:///home/robbie/svn-root/xeona/futz/trunk/xeona1/b/teas04.cc $
//
//  GENERAL NOTES FOR THIS FILE

//  LOCAL AND SYSTEM INCLUDES

#include "teas04.h"           // companion header for this file (place first)

#include "../c/recset.h"      // records and fields and also record-sets
#include "../c/label.h"       // helper class to format solver labels
#include "../c/conex.h"       // create and connect block interfaces
#include "../b/optops1.h"     // operate optimization sub-problems for hard assets 1
#include "../b/gate.h"        // gateway entity
#include "../b/commods01.h"   // concrete commodities 1
#include "../b/commods.h"     // commodities hierarchy
#include "../b/bandtaf.h"     // banded tariff set and support

#include "../a/logger.h"      // standard logging functionality (as required)
#include "../c/smart_ptr.h"   // toggle between Boost and TR1 smart pointers
#include ".././common.h"      // common definitions for project (place last)

#include <string>             // C++ strings
#include <sstream>            // string-streams

//  CODE

// ---------------------------------------------------------
//  CLASS           : TeasLoad <>
// ---------------------------------------------------------

// ---------------------------------------------------------
//  MEMBER FUNCTION : TeasLoad<>
// ---------------------------------------------------------

template <typename C>                        // 'C' for commodity
TeasLoad<C>::TeasLoad
(const std::string entityId,
 Record&           record) :
  CostRegister(record),
  TechnicalAsset(entityId, record, xeona::e_commitmentModes),
  d_commodity(record.tieSingle<std::string>("cable-commodity")),
  d_demandHiBound(record.tieSingle<double>("demand-hi-bound")),
  d_loads(record.tieTimeseries<double>("loads")),
  d_inCommodity(Cable<C>::create
                (entityId,                   // me
                 record.tieSingle<std::string>("socket-1"),
                 d_commodity)),              // common value
  d_ops()
{
  // initial reporting
  s_logger->repx(logga::xtra, "constructor call", getIdAndKind());

  // builtin remark
  d_builtinRemark = "beta";
}

// ---------------------------------------------------------
//  MEMBER FUNCTION : ~TeasLoad<>
// ---------------------------------------------------------

template <typename C>
TeasLoad<C>::~TeasLoad()
{
  // initial reporting
  s_logger->repx(logga::xtra, "destructor call", getIdAndKind());
}

// ---------------------------------------------------------
//  MEMBER FUNCTION : constrain
// ---------------------------------------------------------
//  Description  : engineering characterization
//  Role         : host call
//  Techniques   : (nothing special)
//  Status       : complete
//
//  Design notes
//
//      The factor equality constraint is set by my operator.
//
// ---------------------------------------------------------

template <typename C>
const int                                    // duty gol
TeasLoad<C>::constrain
(const xeona::DomainMode capacityMode)
{
  // additional initial reporting as appropriate
  // YEEK 30 CODE (set by '--yeek')
  if ( xeona::yeek == 30 || xeona::yeek == 1 || xeona::yeek == 2 )
    {
      const std::string subtype = xeona::demangle(typeid(*this).name()); // CAUTION: deref
      s_logger->repx(logga::adhc, "entering member function", subtype);
    }
  else
    {
      s_logger->repx(logga::adhc, "entering member function", "TeasLoad<C>");
    }

  // create and fill a label object
  const std::string teasId = getIdentifier();
  Label lab(teasId);

  // recreate new operations OSP of the required type
  d_ops.reset(new OpsFac1Out0(d_solver, d_commitmentMode));
  d_ops->loadOspLabel(lab.str());

  // define a global col for internal use
  int inGol = -1;                            // nonsensical value

  // upload the engineering (using the double argument flexible call)
  const double demandLoBound = 0.0;
  boost::tie(inGol) = d_ops->uploadEngineering(  demandLoBound,
                                               d_demandHiBound);

  // bind global cols to the relevant interfaces
  d_inCommodity->bindOsp(d_solver, inGol);

  // store duty values
  d_floorDuty   =   demandLoBound;
  d_ceilingDuty = d_demandHiBound;

  // return the duty gol
  return inGol;
}

// ---------------------------------------------------------
//  MEMBER FUNCTION : washup
// ---------------------------------------------------------
//  Description  : washup call
//  Role         : obtain and process results
//  Techniques   : (nothing special)
//  Status       : complete
// ---------------------------------------------------------

template <typename C>
void
TeasLoad<C>::washup()
{
  // initial reporting
  s_logger->repx(logga::adhc, "entering member function, step", d_step);

  // results recovery
  double actualLoad      = -1.0;             // nonsensical value
  boost::tie(actualLoad) = d_ops->downloadSolution();

  // store entity state information
  d_loads->at(d_step) = actualLoad;

  // store some on-the-fly statistics
  d_dutyStats(actualLoad);                   // functor provided by class 'Block'
  d_sizeStats(d_demandHiBound);              // functor provided by class 'Block'
}

// ---------------------------------------------------------
//  MEMBER FUNCTION : obtainGateway
// ---------------------------------------------------------

template <typename C>
shared_ptr<GateCom<C> >
TeasLoad<C>::obtainGateway() const
{
  // preliminary reporting
  s_logger->repx(logga::adhc, "entering member function", "");

  // active code
  shared_ptr<Entity> entity = d_inCommodity->getPartner();
  if ( entity == 0 )
    {
      s_logger->repx(logga::adhc, "gateway as Entity not obtained", "");
    }
  else
    {
      s_logger->repx(logga::adhc, "gateway as Entity obtained", "");
    }
  shared_ptr<GateCom<C> > gateway = dynamic_pointer_cast<GateCom<C> >(entity);
  if ( gateway == 0 )
    {
      s_logger->repx(logga::warn, "GateCom <> cast failed", "");
    }
  return gateway;
}

// ---------------------------------------------------------
//  MEMBER FUNCTION : obtainTariffSet
// ---------------------------------------------------------

template <typename C>
shared_ptr<BandedTariffSet>
TeasLoad<C>::obtainTariffSet() const
{
  // preliminary reporting
  s_logger->repx(logga::adhc, "entering member function", "");

  // active code
  shared_ptr<GateCom<C> >     gateway = obtainGateway();
  shared_ptr<BandedTariffSet> tariffs = gateway->obtainTariffSet();
  if ( tariffs == 0 )
    {
      s_logger->repx(logga::warn, "tariffs not obtained", tariffs);
    }
  else
    {
      s_logger->repx(logga::adhc, "tariffs obtained", "");
    }

  // return
  return tariffs;
}

// ---------------------------------------------------------
//  CLASS           : TeasLoadFin <>
// ---------------------------------------------------------

// ---------------------------------------------------------
//  MEMBER FUNCTION : TeasLoadFin<>
// ---------------------------------------------------------

template <typename C>                        // 'C' for commodity
TeasLoadFin<C>::TeasLoadFin
(const std::string entityId,
 Record&           record) :
  CostRegister(record),
  TeasLoad<C>(entityId, record),
  CostRegisterSRFin(record),
  CostRegisterEmbFin(record)
{
  // initial reporting
  s_logger->repx(logga::xtra, "constructor call", getIdAndKind());

  // builtin remark
  d_builtinRemark = "beta";
}

// ---------------------------------------------------------
//  MEMBER FUNCTION : ~TeasLoadFin<>
// ---------------------------------------------------------

template <typename C>
TeasLoadFin<C>::~TeasLoadFin()
{
  // initial reporting
  s_logger->repx(logga::xtra, "destructor call", getIdAndKind());
}

// ---------------------------------------------------------
//  MEMBER FUNCTION : constrain
// ---------------------------------------------------------
//  Description  : engineering characterization
//  Role         : host call
//  Techniques   : (nothing special)
//  Status       : complete
//
//  Design notes
//
//      The factor equality constraint is set by my operator.
//
// ---------------------------------------------------------

template <typename C>
const int                                    // duty gol
TeasLoadFin<C>::constrain
(const xeona::DomainMode capacityMode)
{
  // additional initial reporting as appropriate
  // YEEK 30 CODE (set by '--yeek')
  if ( xeona::yeek == 30 || xeona::yeek == 1 || xeona::yeek == 2 )
    {
      const std::string subtype = xeona::demangle(typeid(*this).name()); // CAUTION: deref
      s_logger->repx(logga::adhc, "entering member function", subtype);
    }
  else
    {
      s_logger->repx(logga::adhc, "entering member function", "TeasLoadFin<C>");
    }

  // create and fill a label object
  const std::string teasId = getIdentifier();
  Label lab(teasId);

  // recreate new operations OSP of the required type
  d_ops.reset(new OpsFac1Out0(d_solver, d_commitmentMode));
  d_ops->loadOspLabel(lab.str());

  // define a global col for internal use
  int inGol = -1;                            // nonsensical value

  // upload the engineering (using the double argument flexible call)
  const double demandLoBound = 0.0;
  boost::tie(inGol) = d_ops->uploadEngineering(  demandLoBound,
                                               d_demandHiBound);

  // upload specific costs -- including increment the "shift" term
  d_ops->uploadShortrunCosts(d_dutySpecCosts, inGol);
  d_ops->uploadShortrunCosts(d_sizeSpecCosts * d_nameplateSize + d_standingCosts);

  // bind global cols to the relevant interfaces
  d_inCommodity->bindOsp(d_solver, inGol);

  // store duty values
  d_floorDuty   =   demandLoBound;
  d_ceilingDuty = d_demandHiBound;

  // return the duty gol
  return inGol;
}

// ---------------------------------------------------------
//  MEMBER FUNCTION : washup
// ---------------------------------------------------------
//  Description  : washup call
//  Role         : obtain and process results
//  Techniques   : (nothing special)
//  Status       : complete
// ---------------------------------------------------------

template <typename C>
void
TeasLoadFin<C>::washup()
{
  // initial reporting
  s_logger->repx(logga::adhc, "entering member function, step", d_step);

  // results recovery
  double actualLoad      = -1.0;             // nonsensical value
  boost::tie(actualLoad) = d_ops->downloadSolution();

  // store entity state information
  d_loads->at(d_step) = actualLoad;

  // accrued costs processing
  CostSet varCosts(0.0);                               // transfer object
  CostSet fixCosts(0.0);                               // transfer object
  d_ops->downloadVarCosts(varCosts);                   // recover variable costs
  d_ops->downloadFixCosts(fixCosts);                   // recover fixed costs
  updateShortrunCosts(d_step, varCosts, fixCosts);     // utilize supplied data
  updateEmbeddedCosts(d_step);                         // utilize in-house calcs

  // store some on-the-fly statistics
  d_dutyStats(actualLoad);                   // functor provided by class 'Block'
  d_sizeStats(d_demandHiBound);              // functor provided by class 'Block'
}

// ---------------------------------------------------------
//  MEMBER FUNCTION : obtainGateway
// ---------------------------------------------------------

template <typename C>
shared_ptr<GateCom<C> >
TeasLoadFin<C>::obtainGateway() const
{
  // preliminary reporting
  s_logger->repx(logga::adhc, "entering member function", "");

  // active code
  shared_ptr<Entity> entity = d_inCommodity->getPartner();
  if ( entity == 0 )
    {
      s_logger->repx(logga::adhc, "gateway as Entity not obtained", "");
    }
  else
    {
      s_logger->repx(logga::adhc, "gateway as Entity obtained", "");
    }
  shared_ptr<GateCom<C> > gateway = dynamic_pointer_cast<GateCom<C> >(entity);
  if ( gateway == 0 )
    {
      s_logger->repx(logga::warn, "GateCom <> cast failed", "");
    }
  return gateway;
}

// ---------------------------------------------------------
//  MEMBER FUNCTION : obtainTariffSet
// ---------------------------------------------------------

template <typename C>
shared_ptr<BandedTariffSet>
TeasLoadFin<C>::obtainTariffSet() const
{
  // preliminary reporting
  s_logger->repx(logga::adhc, "entering member function", "");

  // active code
  shared_ptr<GateCom<C> >     gateway = obtainGateway();
  shared_ptr<BandedTariffSet> tariffs = gateway->obtainTariffSet();
  if ( tariffs == 0 )
    {
      s_logger->repx(logga::warn, "tariffs not obtained", tariffs);
    }
  else
    {
      s_logger->repx(logga::adhc, "tariffs obtained", "");
    }

  // return
  return tariffs;
}

// ---------------------------------------------------------
//  CLASS           : TeasLoadAll <>
// ---------------------------------------------------------

// ---------------------------------------------------------
//  MEMBER FUNCTION : TeasLoadAll<>
// ---------------------------------------------------------

template <typename C>                        // 'C' for commodity
TeasLoadAll<C>::TeasLoadAll
(const std::string entityId,
 Record&           record) :
  CostRegister(record),
  TeasLoad<C>(entityId, record),
  CostRegisterSRAll(record),
  CostRegisterEmbAll(record)
{
  // initial reporting
  s_logger->repx(logga::xtra, "constructor call", getIdAndKind());

  // builtin remark
  d_builtinRemark = "beta";
}

// ---------------------------------------------------------
//  MEMBER FUNCTION : ~TeasLoadAll<>
// ---------------------------------------------------------

template <typename C>
TeasLoadAll<C>::~TeasLoadAll()
{
  // initial reporting
  s_logger->repx(logga::xtra, "destructor call", getIdAndKind());
}

// ---------------------------------------------------------
//  MEMBER FUNCTION : constrain
// ---------------------------------------------------------
//  Description  : engineering characterization
//  Role         : host call
//  Techniques   : (nothing special)
//  Status       : complete
//
//  Design notes
//
//      The factor equality constraint is set by my operator.
//
// ---------------------------------------------------------

template <typename C>
const int                                    // duty gol
TeasLoadAll<C>::constrain
(const xeona::DomainMode capacityMode)
{
  // additional initial reporting as appropriate
  // YEEK 30 CODE (set by '--yeek')
  if ( xeona::yeek == 30 || xeona::yeek == 1 || xeona::yeek == 2 )
    {
      const std::string subtype = xeona::demangle(typeid(*this).name()); // CAUTION: deref
      s_logger->repx(logga::adhc, "entering member function", subtype);
    }
  else
    {
      s_logger->repx(logga::adhc, "entering member function", "TeasLoadAll<C>");
    }

  // create and fill a label object
  const std::string teasId = getIdentifier();
  Label lab(teasId);

  // recreate new operations OSP of the required type
  d_ops.reset(new OpsFac1Out0(d_solver, d_commitmentMode));
  d_ops->loadOspLabel(lab.str());

  // define a global col for internal use
  int inGol = -1;                            // nonsensical value

  // upload the engineering (using the double argument flexible call)
  const double demandLoBound = 0.0;
  boost::tie(inGol) = d_ops->uploadEngineering(  demandLoBound,
                                               d_demandHiBound);

  // upload specific costs -- including increment the "shift" term
  d_ops->uploadShortrunCosts(d_dutySpecCosts, inGol);
  d_ops->uploadShortrunCosts(d_sizeSpecCosts * d_nameplateSize + d_standingCosts);

  // bind global cols to the relevant interfaces
  d_inCommodity->bindOsp(d_solver, inGol);

  // store duty values
  d_floorDuty   =   demandLoBound;
  d_ceilingDuty = d_demandHiBound;

  // return the duty gol
  return inGol;
}

// ---------------------------------------------------------
//  MEMBER FUNCTION : washup
// ---------------------------------------------------------
//  Description  : washup call
//  Role         : obtain and process results
//  Techniques   : (nothing special)
//  Status       : complete
// ---------------------------------------------------------

template <typename C>
void
TeasLoadAll<C>::washup()
{
  // initial reporting
  s_logger->repx(logga::adhc, "entering member function, step", d_step);

  // results recovery
  double actualLoad      = -1.0;             // nonsensical value
  boost::tie(actualLoad) = d_ops->downloadSolution();

  // store entity state information
  d_loads->at(d_step) = actualLoad;

  // accrued costs processing
  CostSet varCosts(0.0);                               // transfer object
  CostSet fixCosts(0.0);                               // transfer object
  d_ops->downloadVarCosts(varCosts);                   // recover variable costs
  d_ops->downloadFixCosts(fixCosts);                   // recover fixed costs
  updateShortrunCosts(d_step, varCosts, fixCosts);     // utilize supplied data
  updateEmbeddedCosts(d_step);                         // utilize in-house calcs

  // store some on-the-fly statistics
  d_dutyStats(actualLoad);                   // functor provided by class 'Block'
  d_sizeStats(d_demandHiBound);              // functor provided by class 'Block'
}

// ---------------------------------------------------------
//  MEMBER FUNCTION : obtainGateway
// ---------------------------------------------------------

template <typename C>
shared_ptr<GateCom<C> >
TeasLoadAll<C>::obtainGateway() const
{
  // preliminary reporting
  s_logger->repx(logga::adhc, "entering member function", "");

  // active code
  shared_ptr<Entity> entity = d_inCommodity->getPartner();
  if ( entity == 0 )
    {
      s_logger->repx(logga::adhc, "gateway as Entity not obtained", "");
    }
  else
    {
      s_logger->repx(logga::adhc, "gateway as Entity obtained", "");
    }
  shared_ptr<GateCom<C> > gateway = dynamic_pointer_cast<GateCom<C> >(entity);
  if ( gateway == 0 )
    {
      s_logger->repx(logga::warn, "GateCom <> cast failed", "");
    }
  return gateway;
}

// ---------------------------------------------------------
//  MEMBER FUNCTION : obtainTariffSet
// ---------------------------------------------------------

template <typename C>
shared_ptr<BandedTariffSet>
TeasLoadAll<C>::obtainTariffSet() const
{
  // preliminary reporting
  s_logger->repx(logga::adhc, "entering member function", "");

  // active code
  shared_ptr<GateCom<C> >     gateway = obtainGateway();
  shared_ptr<BandedTariffSet> tariffs = gateway->obtainTariffSet();
  if ( tariffs == 0 )
    {
      s_logger->repx(logga::warn, "tariffs not obtained", tariffs);
    }
  else
    {
      s_logger->repx(logga::adhc, "tariffs obtained", "");
    }

  // return
  return tariffs;
}

// ---------------------------------------------------------
//  CLASS           : TeasSource <>
// ---------------------------------------------------------

// ---------------------------------------------------------
//  MEMBER FUNCTION : TeasSource<>
// ---------------------------------------------------------

template <typename C>                        // 'C' for commodity
TeasSource<C>::TeasSource
(const std::string entityId,
 Record&           record) :
  CostRegister(record),
  TechnicalAsset(entityId, record, xeona::e_commitmentModes),
  d_commodity(record.tieSingle<std::string>("socket-commodity")),
  d_extractLoBound(record.tieSingle<double>("extract-lo-bound")),
  d_extractHiBound(record.tieSingle<double>("extract-hi-bound")),
  d_extractions(record.tieTimeseries<double>("extractions")),
  d_outCommodity(Socket<C>::create
                 (entityId,                  // me
                  "sock-1",                  // hard-coded socket label
                  d_commodity)),             // common value
  d_ops()
{
  // initial reporting
  s_logger->repx(logga::xtra, "constructor call", getIdAndKind());

  // builtin remark
  d_builtinRemark = "beta";
}

// ---------------------------------------------------------
//  MEMBER FUNCTION : ~TeasSource<>
// ---------------------------------------------------------

template <typename C>
TeasSource<C>::~TeasSource()
{
  // initial reporting
  s_logger->repx(logga::xtra, "destructor call", getIdAndKind());
}

// ---------------------------------------------------------
//  MEMBER FUNCTION : constrain
// ---------------------------------------------------------
//  Description  : engineering characterization
//  Role         : host call
//  Techniques   : (nothing special)
//  Status       : complete
//
//  Design notes
//
//      The factor equality constraint is set by my operator.
//
// ---------------------------------------------------------

template <typename C>
const int                                    // duty gol
TeasSource<C>::constrain
(const xeona::DomainMode capacityMode)
{
  // additional initial reporting as appropriate
  // YEEK 30 CODE (set by '--yeek')
  if ( xeona::yeek == 30 || xeona::yeek == 1 || xeona::yeek == 2 )
    {
      const std::string subtype = xeona::demangle(typeid(*this).name()); // CAUTION: deref
      s_logger->repx(logga::adhc, "entering member function", subtype);
    }
  else
    {
      s_logger->repx(logga::adhc, "entering member function", "TeasSource<C>");
    }

  // create and fill a label object
  const std::string teasId = getIdentifier();
  Label lab(teasId);

  // recreate new operations OSP of the required type
  d_ops.reset(new OpsFac0Out1(d_solver, d_commitmentMode));
  d_ops->loadOspLabel(lab.str());

  // define a global col for internal use
  int outGol = -1;                           // nonsensical value

  // upload the engineering
  boost::tie(outGol) = d_ops->uploadEngineering(d_extractLoBound,
                                                d_extractHiBound);

  // store duty values
  d_floorDuty   = d_extractLoBound;
  d_ceilingDuty = d_extractHiBound;

  // bind global cols to the relevant interfaces
  d_outCommodity->bindOsp(d_solver, outGol);

  // return the duty gol
  return outGol;
}

// ---------------------------------------------------------
//  MEMBER FUNCTION : washup
// ---------------------------------------------------------
//  Description  : washup call
//  Role         : obtain and process results
//  Techniques   : (nothing special)
//  Status       : complete
// ---------------------------------------------------------

template <typename C>
void
TeasSource<C>::washup()
{
  // initial reporting
  s_logger->repx(logga::adhc, "entering member function, step", d_step);

  // results recovery
  double actualExtraction      = -1.0;       // nonsensical value
  boost::tie(actualExtraction) = d_ops->downloadSolution();

  // store entity state information
  d_extractions->at(d_step)    = actualExtraction;

  // store some on-the-fly statistics
  d_dutyStats(actualExtraction);             // functor provided by class 'Block'
  d_sizeStats(d_extractHiBound);             // functor provided by class 'Block'
}

// ---------------------------------------------------------
//  CLASS           : TeasSourceFin <>
// ---------------------------------------------------------

// ---------------------------------------------------------
//  MEMBER FUNCTION : TeasSourceFin<>
// ---------------------------------------------------------

template <typename C>                        // 'C' for commodity
TeasSourceFin<C>::TeasSourceFin
(const std::string entityId,
 Record&           record) :
  CostRegister(record),
  TeasSource<C>(entityId, record),
  CostRegisterSRFin(record),
  CostRegisterEmbFin(record)
{
  // initial reporting
  s_logger->repx(logga::xtra, "constructor call", getIdAndKind());

  // builtin remark
  d_builtinRemark = "beta";
}

// ---------------------------------------------------------
//  MEMBER FUNCTION : ~TeasSourceFin<>
// ---------------------------------------------------------

template <typename C>
TeasSourceFin<C>::~TeasSourceFin()
{
  // initial reporting
  s_logger->repx(logga::xtra, "destructor call", getIdAndKind());
}

// ---------------------------------------------------------
//  MEMBER FUNCTION : constrain
// ---------------------------------------------------------
//  Description  : engineering characterization
//  Role         : host call
//  Techniques   : (nothing special)
//  Status       : complete
//
//  Design notes
//
//      The factor equality constraint is set by my operator.
//
// ---------------------------------------------------------

template <typename C>
const int                                    // duty gol
TeasSourceFin<C>::constrain
(const xeona::DomainMode capacityMode)
{
  // additional initial reporting as appropriate
  // YEEK 30 CODE (set by '--yeek')
  if ( xeona::yeek == 30 || xeona::yeek == 1 || xeona::yeek == 2 )
    {
      const std::string subtype = xeona::demangle(typeid(*this).name()); // CAUTION: deref
      s_logger->repx(logga::adhc, "entering member function", subtype);
    }
  else
    {
      s_logger->repx(logga::adhc, "entering member function", "TeasSourceFin<C>");
    }

  // create and fill a label object
  const std::string teasId = getIdentifier();
  Label lab(teasId);

  // recreate new operations OSP of the required type
  d_ops.reset(new OpsFac0Out1(d_solver, d_commitmentMode));
  d_ops->loadOspLabel(lab.str());

  // define a global col for internal use
  int outGol = -1;                           // nonsensical value

  // upload the engineering
  boost::tie(outGol) = d_ops->uploadEngineering(d_extractLoBound,
                                                d_extractHiBound);

  // upload specific costs -- including increment the "shift" term
  d_ops->uploadShortrunCosts(d_dutySpecCosts, outGol);
  d_ops->uploadShortrunCosts(d_sizeSpecCosts * d_nameplateSize + d_standingCosts);

  // store duty values
  d_floorDuty   = d_extractLoBound;
  d_ceilingDuty = d_extractHiBound;

  // bind global cols to the relevant interfaces
  d_outCommodity->bindOsp(d_solver, outGol);

  // return the duty gol
  return outGol;
}

// ---------------------------------------------------------
//  MEMBER FUNCTION : washup
// ---------------------------------------------------------
//  Description  : washup call
//  Role         : obtain and process results
//  Techniques   : code switch on YEEK 37
//  Status       : complete
// ---------------------------------------------------------

template <typename C>
void
TeasSourceFin<C>::washup()
{
  // initial reporting
  s_logger->repx(logga::adhc, "entering member function, step", d_step);

  // results recovery
  double actualExtraction      = -1.0;       // nonsensical value
  boost::tie(actualExtraction) = d_ops->downloadSolution();

  // store entity state information
  d_extractions->at(d_step)    = actualExtraction;

  // accrued costs processing
  CostSet varCosts(0.0);                               // transfer object
  CostSet fixCosts(0.0);                               // transfer object
  d_ops->downloadVarCosts(varCosts);                   // recover variable costs
  d_ops->downloadFixCosts(fixCosts);                   // recover fixed costs
  updateShortrunCosts(d_step, varCosts, fixCosts);     // utilize supplied data
  updateEmbeddedCosts(d_step);                         // utilize in-house calcs

  // store some on-the-fly statistics
  d_dutyStats(actualExtraction);             // functor provided by class 'Block'
  d_sizeStats(d_extractHiBound);             // functor provided by class 'Block'
}

// ---------------------------------------------------------
//  CLASS           : TeasSourceAll <>
// ---------------------------------------------------------

// ---------------------------------------------------------
//  MEMBER FUNCTION : TeasSourceAll<>
// ---------------------------------------------------------

template <typename C>                        // 'C' for commodity
TeasSourceAll<C>::TeasSourceAll
(const std::string entityId,
 Record&           record) :
  CostRegister(record),
  TeasSource<C>(entityId, record),
  CostRegisterSRAll(record),
  CostRegisterEmbAll(record)
{
  // initial reporting
  s_logger->repx(logga::xtra, "constructor call", getIdAndKind());

  // builtin remark
  d_builtinRemark = "beta";
}

// ---------------------------------------------------------
//  MEMBER FUNCTION : ~TeasSourceAll<>
// ---------------------------------------------------------

template <typename C>
TeasSourceAll<C>::~TeasSourceAll()
{
  // initial reporting
  s_logger->repx(logga::xtra, "destructor call", getIdAndKind());
}

// ---------------------------------------------------------
//  MEMBER FUNCTION : constrain
// ---------------------------------------------------------
//  Description  : engineering characterization
//  Role         : host call
//  Techniques   : (nothing special)
//  Status       : complete
//
//  Design notes
//
//      The factor equality constraint is set by my operator.
//
// ---------------------------------------------------------

template <typename C>
const int                                    // duty gol
TeasSourceAll<C>::constrain
(const xeona::DomainMode capacityMode)
{
  // additional initial reporting as appropriate
  // YEEK 30 CODE (set by '--yeek')
  if ( xeona::yeek == 30 || xeona::yeek == 1 || xeona::yeek == 2 )
    {
      const std::string subtype = xeona::demangle(typeid(*this).name()); // CAUTION: deref
      s_logger->repx(logga::adhc, "entering member function", subtype);
    }
  else
    {
      s_logger->repx(logga::adhc, "entering member function", "TeasSourceAll<C>");
    }

  // create and fill a label object
  const std::string teasId = getIdentifier();
  Label lab(teasId);

  // recreate new operations OSP of the required type
  d_ops.reset(new OpsFac0Out1(d_solver, d_commitmentMode));
  d_ops->loadOspLabel(lab.str());

  // define a global col for internal use
  int outGol = -1;                           // nonsensical value

  // upload the engineering
  boost::tie(outGol) = d_ops->uploadEngineering(d_extractLoBound,
                                                d_extractHiBound);

  // upload specific costs -- including increment the "shift" term
  d_ops->uploadShortrunCosts(d_dutySpecCosts, outGol);
  d_ops->uploadShortrunCosts(d_sizeSpecCosts * d_nameplateSize + d_standingCosts);

  // store duty values
  d_floorDuty   = d_extractLoBound;
  d_ceilingDuty = d_extractHiBound;

  // bind global cols to the relevant interfaces
  d_outCommodity->bindOsp(d_solver, outGol);

  // return the duty gol
  return outGol;
}

// ---------------------------------------------------------
//  MEMBER FUNCTION : washup
// ---------------------------------------------------------
//  Description  : washup call
//  Role         : obtain and process results
//  Techniques   : (nothing special)
//  Status       : complete
// ---------------------------------------------------------

template <typename C>
void
TeasSourceAll<C>::washup()
{
  // initial reporting
  s_logger->repx(logga::adhc, "entering member function, step", d_step);

  // results recovery
  double actualExtraction      = -1.0;       // nonsensical value
  boost::tie(actualExtraction) = d_ops->downloadSolution();

  // store entity state information
  d_extractions->at(d_step)    = actualExtraction;

  // accrued costs processing
  CostSet varCosts(0.0);                               // transfer object
  CostSet fixCosts(0.0);                               // transfer object
  d_ops->downloadVarCosts(varCosts);                   // recover variable costs
  d_ops->downloadFixCosts(fixCosts);                   // recover fixed costs
  updateShortrunCosts(d_step, varCosts, fixCosts);     // utilize supplied data
  updateEmbeddedCosts(d_step);                         // utilize in-house calcs

  // store some on-the-fly statistics
  d_dutyStats(actualExtraction);             // functor provided by class 'Block'
  d_sizeStats(d_extractHiBound);             // functor provided by class 'Block'
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

// class 'TeasLoad<>'

template TeasLoad<Oxid>::TeasLoad(const std::string, Record&);
template TeasLoad<Cert>::TeasLoad(const std::string, Record&);
template TeasLoad<Cseq>::TeasLoad(const std::string, Record&);
template TeasLoad<Elec>::TeasLoad(const std::string, Record&);
template TeasLoad<Work>::TeasLoad(const std::string, Record&);
template TeasLoad<Heat>::TeasLoad(const std::string, Record&);
template TeasLoad<Thrm>::TeasLoad(const std::string, Record&);
template TeasLoad<Fund>::TeasLoad(const std::string, Record&);

template TeasLoad<Oxid>::~TeasLoad();
template TeasLoad<Cert>::~TeasLoad();
template TeasLoad<Cseq>::~TeasLoad();
template TeasLoad<Elec>::~TeasLoad();
template TeasLoad<Work>::~TeasLoad();
template TeasLoad<Heat>::~TeasLoad();
template TeasLoad<Thrm>::~TeasLoad();
template TeasLoad<Fund>::~TeasLoad();

template const int TeasLoad<Oxid>::constrain(const xeona::DomainMode);
template const int TeasLoad<Cert>::constrain(const xeona::DomainMode);
template const int TeasLoad<Cseq>::constrain(const xeona::DomainMode);
template const int TeasLoad<Elec>::constrain(const xeona::DomainMode);
template const int TeasLoad<Work>::constrain(const xeona::DomainMode);
template const int TeasLoad<Heat>::constrain(const xeona::DomainMode);
template const int TeasLoad<Thrm>::constrain(const xeona::DomainMode);
template const int TeasLoad<Fund>::constrain(const xeona::DomainMode);

template void TeasLoad<Oxid>::washup();
template void TeasLoad<Cert>::washup();
template void TeasLoad<Cseq>::washup();
template void TeasLoad<Elec>::washup();
template void TeasLoad<Work>::washup();
template void TeasLoad<Heat>::washup();
template void TeasLoad<Thrm>::washup();
template void TeasLoad<Fund>::washup();

template shared_ptr<GateCom<Oxid> > TeasLoad<Oxid>::obtainGateway() const;
template shared_ptr<GateCom<Cert> > TeasLoad<Cert>::obtainGateway() const;
template shared_ptr<GateCom<Cseq> > TeasLoad<Cseq>::obtainGateway() const;
template shared_ptr<GateCom<Elec> > TeasLoad<Elec>::obtainGateway() const;
template shared_ptr<GateCom<Work> > TeasLoad<Work>::obtainGateway() const;
template shared_ptr<GateCom<Heat> > TeasLoad<Heat>::obtainGateway() const;
template shared_ptr<GateCom<Thrm> > TeasLoad<Thrm>::obtainGateway() const;
template shared_ptr<GateCom<Fund> > TeasLoad<Fund>::obtainGateway() const;

template shared_ptr<BandedTariffSet> TeasLoad<Oxid>::obtainTariffSet() const;
template shared_ptr<BandedTariffSet> TeasLoad<Cert>::obtainTariffSet() const;
template shared_ptr<BandedTariffSet> TeasLoad<Cseq>::obtainTariffSet() const;
template shared_ptr<BandedTariffSet> TeasLoad<Elec>::obtainTariffSet() const;
template shared_ptr<BandedTariffSet> TeasLoad<Work>::obtainTariffSet() const;
template shared_ptr<BandedTariffSet> TeasLoad<Heat>::obtainTariffSet() const;
template shared_ptr<BandedTariffSet> TeasLoad<Thrm>::obtainTariffSet() const;
template shared_ptr<BandedTariffSet> TeasLoad<Fund>::obtainTariffSet() const;

// class 'TeasLoadFin<>'

template TeasLoadFin<Oxid>::TeasLoadFin(const std::string, Record&);
template TeasLoadFin<Cert>::TeasLoadFin(const std::string, Record&);
template TeasLoadFin<Cseq>::TeasLoadFin(const std::string, Record&);
template TeasLoadFin<Elec>::TeasLoadFin(const std::string, Record&);
template TeasLoadFin<Work>::TeasLoadFin(const std::string, Record&);
template TeasLoadFin<Heat>::TeasLoadFin(const std::string, Record&);
template TeasLoadFin<Thrm>::TeasLoadFin(const std::string, Record&);
template TeasLoadFin<Fund>::TeasLoadFin(const std::string, Record&);

template TeasLoadFin<Oxid>::~TeasLoadFin();
template TeasLoadFin<Cert>::~TeasLoadFin();
template TeasLoadFin<Cseq>::~TeasLoadFin();
template TeasLoadFin<Elec>::~TeasLoadFin();
template TeasLoadFin<Work>::~TeasLoadFin();
template TeasLoadFin<Heat>::~TeasLoadFin();
template TeasLoadFin<Thrm>::~TeasLoadFin();
template TeasLoadFin<Fund>::~TeasLoadFin();

template const int TeasLoadFin<Oxid>::constrain(const xeona::DomainMode);
template const int TeasLoadFin<Cert>::constrain(const xeona::DomainMode);
template const int TeasLoadFin<Cseq>::constrain(const xeona::DomainMode);
template const int TeasLoadFin<Elec>::constrain(const xeona::DomainMode);
template const int TeasLoadFin<Work>::constrain(const xeona::DomainMode);
template const int TeasLoadFin<Heat>::constrain(const xeona::DomainMode);
template const int TeasLoadFin<Thrm>::constrain(const xeona::DomainMode);
template const int TeasLoadFin<Fund>::constrain(const xeona::DomainMode);

template void TeasLoadFin<Oxid>::washup();
template void TeasLoadFin<Cert>::washup();
template void TeasLoadFin<Cseq>::washup();
template void TeasLoadFin<Elec>::washup();
template void TeasLoadFin<Work>::washup();
template void TeasLoadFin<Heat>::washup();
template void TeasLoadFin<Thrm>::washup();
template void TeasLoadFin<Fund>::washup();

template shared_ptr<GateCom<Oxid> > TeasLoadFin<Oxid>::obtainGateway() const;
template shared_ptr<GateCom<Cert> > TeasLoadFin<Cert>::obtainGateway() const;
template shared_ptr<GateCom<Cseq> > TeasLoadFin<Cseq>::obtainGateway() const;
template shared_ptr<GateCom<Elec> > TeasLoadFin<Elec>::obtainGateway() const;
template shared_ptr<GateCom<Work> > TeasLoadFin<Work>::obtainGateway() const;
template shared_ptr<GateCom<Heat> > TeasLoadFin<Heat>::obtainGateway() const;
template shared_ptr<GateCom<Thrm> > TeasLoadFin<Thrm>::obtainGateway() const;
template shared_ptr<GateCom<Fund> > TeasLoadFin<Fund>::obtainGateway() const;

template shared_ptr<BandedTariffSet> TeasLoadFin<Oxid>::obtainTariffSet() const;
template shared_ptr<BandedTariffSet> TeasLoadFin<Cert>::obtainTariffSet() const;
template shared_ptr<BandedTariffSet> TeasLoadFin<Cseq>::obtainTariffSet() const;
template shared_ptr<BandedTariffSet> TeasLoadFin<Elec>::obtainTariffSet() const;
template shared_ptr<BandedTariffSet> TeasLoadFin<Work>::obtainTariffSet() const;
template shared_ptr<BandedTariffSet> TeasLoadFin<Heat>::obtainTariffSet() const;
template shared_ptr<BandedTariffSet> TeasLoadFin<Thrm>::obtainTariffSet() const;
template shared_ptr<BandedTariffSet> TeasLoadFin<Fund>::obtainTariffSet() const;

// class 'TeasLoadAll<>'

template TeasLoadAll<Oxid>::TeasLoadAll(const std::string, Record&);
template TeasLoadAll<Cert>::TeasLoadAll(const std::string, Record&);
template TeasLoadAll<Cseq>::TeasLoadAll(const std::string, Record&);
template TeasLoadAll<Elec>::TeasLoadAll(const std::string, Record&);
template TeasLoadAll<Work>::TeasLoadAll(const std::string, Record&);
template TeasLoadAll<Heat>::TeasLoadAll(const std::string, Record&);
template TeasLoadAll<Thrm>::TeasLoadAll(const std::string, Record&);
template TeasLoadAll<Fund>::TeasLoadAll(const std::string, Record&);

template TeasLoadAll<Oxid>::~TeasLoadAll();
template TeasLoadAll<Cert>::~TeasLoadAll();
template TeasLoadAll<Cseq>::~TeasLoadAll();
template TeasLoadAll<Elec>::~TeasLoadAll();
template TeasLoadAll<Work>::~TeasLoadAll();
template TeasLoadAll<Heat>::~TeasLoadAll();
template TeasLoadAll<Thrm>::~TeasLoadAll();
template TeasLoadAll<Fund>::~TeasLoadAll();

template const int TeasLoadAll<Oxid>::constrain(const xeona::DomainMode);
template const int TeasLoadAll<Cert>::constrain(const xeona::DomainMode);
template const int TeasLoadAll<Cseq>::constrain(const xeona::DomainMode);
template const int TeasLoadAll<Elec>::constrain(const xeona::DomainMode);
template const int TeasLoadAll<Work>::constrain(const xeona::DomainMode);
template const int TeasLoadAll<Heat>::constrain(const xeona::DomainMode);
template const int TeasLoadAll<Thrm>::constrain(const xeona::DomainMode);
template const int TeasLoadAll<Fund>::constrain(const xeona::DomainMode);

template void TeasLoadAll<Oxid>::washup();
template void TeasLoadAll<Cert>::washup();
template void TeasLoadAll<Cseq>::washup();
template void TeasLoadAll<Elec>::washup();
template void TeasLoadAll<Work>::washup();
template void TeasLoadAll<Heat>::washup();
template void TeasLoadAll<Thrm>::washup();
template void TeasLoadAll<Fund>::washup();

template shared_ptr<GateCom<Oxid> > TeasLoadAll<Oxid>::obtainGateway() const;
template shared_ptr<GateCom<Cert> > TeasLoadAll<Cert>::obtainGateway() const;
template shared_ptr<GateCom<Cseq> > TeasLoadAll<Cseq>::obtainGateway() const;
template shared_ptr<GateCom<Elec> > TeasLoadAll<Elec>::obtainGateway() const;
template shared_ptr<GateCom<Work> > TeasLoadAll<Work>::obtainGateway() const;
template shared_ptr<GateCom<Heat> > TeasLoadAll<Heat>::obtainGateway() const;
template shared_ptr<GateCom<Thrm> > TeasLoadAll<Thrm>::obtainGateway() const;
template shared_ptr<GateCom<Fund> > TeasLoadAll<Fund>::obtainGateway() const;

template shared_ptr<BandedTariffSet> TeasLoadAll<Oxid>::obtainTariffSet() const;
template shared_ptr<BandedTariffSet> TeasLoadAll<Cert>::obtainTariffSet() const;
template shared_ptr<BandedTariffSet> TeasLoadAll<Cseq>::obtainTariffSet() const;
template shared_ptr<BandedTariffSet> TeasLoadAll<Elec>::obtainTariffSet() const;
template shared_ptr<BandedTariffSet> TeasLoadAll<Work>::obtainTariffSet() const;
template shared_ptr<BandedTariffSet> TeasLoadAll<Heat>::obtainTariffSet() const;
template shared_ptr<BandedTariffSet> TeasLoadAll<Thrm>::obtainTariffSet() const;
template shared_ptr<BandedTariffSet> TeasLoadAll<Fund>::obtainTariffSet() const;

// 'TeasSource<>'

template TeasSource<Oxid>::TeasSource(const std::string, Record&);
template TeasSource<Cert>::TeasSource(const std::string, Record&);
template TeasSource<Cseq>::TeasSource(const std::string, Record&);
template TeasSource<Elec>::TeasSource(const std::string, Record&);
template TeasSource<Work>::TeasSource(const std::string, Record&);
template TeasSource<Heat>::TeasSource(const std::string, Record&);
template TeasSource<Thrm>::TeasSource(const std::string, Record&);
template TeasSource<Fund>::TeasSource(const std::string, Record&);

template TeasSource<Oxid>::~TeasSource();
template TeasSource<Cert>::~TeasSource();
template TeasSource<Cseq>::~TeasSource();
template TeasSource<Elec>::~TeasSource();
template TeasSource<Work>::~TeasSource();
template TeasSource<Heat>::~TeasSource();
template TeasSource<Thrm>::~TeasSource();
template TeasSource<Fund>::~TeasSource();

template const int TeasSource<Oxid>::constrain(const xeona::DomainMode);
template const int TeasSource<Cert>::constrain(const xeona::DomainMode);
template const int TeasSource<Cseq>::constrain(const xeona::DomainMode);
template const int TeasSource<Elec>::constrain(const xeona::DomainMode);
template const int TeasSource<Work>::constrain(const xeona::DomainMode);
template const int TeasSource<Heat>::constrain(const xeona::DomainMode);
template const int TeasSource<Thrm>::constrain(const xeona::DomainMode);
template const int TeasSource<Fund>::constrain(const xeona::DomainMode);

template void TeasSource<Oxid>::washup();
template void TeasSource<Cert>::washup();
template void TeasSource<Cseq>::washup();
template void TeasSource<Elec>::washup();
template void TeasSource<Work>::washup();
template void TeasSource<Heat>::washup();
template void TeasSource<Thrm>::washup();
template void TeasSource<Fund>::washup();

// 'TeasSourceFin<>'

template TeasSourceFin<Oxid>::TeasSourceFin(const std::string, Record&);
template TeasSourceFin<Cert>::TeasSourceFin(const std::string, Record&);
template TeasSourceFin<Cseq>::TeasSourceFin(const std::string, Record&);
template TeasSourceFin<Elec>::TeasSourceFin(const std::string, Record&);
template TeasSourceFin<Work>::TeasSourceFin(const std::string, Record&);
template TeasSourceFin<Heat>::TeasSourceFin(const std::string, Record&);
template TeasSourceFin<Thrm>::TeasSourceFin(const std::string, Record&);
template TeasSourceFin<Fund>::TeasSourceFin(const std::string, Record&);

template TeasSourceFin<Oxid>::~TeasSourceFin();
template TeasSourceFin<Cert>::~TeasSourceFin();
template TeasSourceFin<Cseq>::~TeasSourceFin();
template TeasSourceFin<Elec>::~TeasSourceFin();
template TeasSourceFin<Work>::~TeasSourceFin();
template TeasSourceFin<Heat>::~TeasSourceFin();
template TeasSourceFin<Thrm>::~TeasSourceFin();
template TeasSourceFin<Fund>::~TeasSourceFin();

template const int TeasSourceFin<Oxid>::constrain(const xeona::DomainMode);
template const int TeasSourceFin<Cert>::constrain(const xeona::DomainMode);
template const int TeasSourceFin<Cseq>::constrain(const xeona::DomainMode);
template const int TeasSourceFin<Elec>::constrain(const xeona::DomainMode);
template const int TeasSourceFin<Work>::constrain(const xeona::DomainMode);
template const int TeasSourceFin<Heat>::constrain(const xeona::DomainMode);
template const int TeasSourceFin<Thrm>::constrain(const xeona::DomainMode);
template const int TeasSourceFin<Fund>::constrain(const xeona::DomainMode);

template void TeasSourceFin<Oxid>::washup();
template void TeasSourceFin<Cert>::washup();
template void TeasSourceFin<Cseq>::washup();
template void TeasSourceFin<Elec>::washup();
template void TeasSourceFin<Work>::washup();
template void TeasSourceFin<Heat>::washup();
template void TeasSourceFin<Thrm>::washup();
template void TeasSourceFin<Fund>::washup();

// 'TeasSourceAll<>'

template TeasSourceAll<Oxid>::TeasSourceAll(const std::string, Record&);
template TeasSourceAll<Cert>::TeasSourceAll(const std::string, Record&);
template TeasSourceAll<Cseq>::TeasSourceAll(const std::string, Record&);
template TeasSourceAll<Elec>::TeasSourceAll(const std::string, Record&);
template TeasSourceAll<Work>::TeasSourceAll(const std::string, Record&);
template TeasSourceAll<Heat>::TeasSourceAll(const std::string, Record&);
template TeasSourceAll<Thrm>::TeasSourceAll(const std::string, Record&);
template TeasSourceAll<Fund>::TeasSourceAll(const std::string, Record&);

template TeasSourceAll<Oxid>::~TeasSourceAll();
template TeasSourceAll<Cert>::~TeasSourceAll();
template TeasSourceAll<Cseq>::~TeasSourceAll();
template TeasSourceAll<Elec>::~TeasSourceAll();
template TeasSourceAll<Work>::~TeasSourceAll();
template TeasSourceAll<Heat>::~TeasSourceAll();
template TeasSourceAll<Thrm>::~TeasSourceAll();
template TeasSourceAll<Fund>::~TeasSourceAll();

template const int TeasSourceAll<Oxid>::constrain(const xeona::DomainMode);
template const int TeasSourceAll<Cert>::constrain(const xeona::DomainMode);
template const int TeasSourceAll<Cseq>::constrain(const xeona::DomainMode);
template const int TeasSourceAll<Elec>::constrain(const xeona::DomainMode);
template const int TeasSourceAll<Work>::constrain(const xeona::DomainMode);
template const int TeasSourceAll<Heat>::constrain(const xeona::DomainMode);
template const int TeasSourceAll<Thrm>::constrain(const xeona::DomainMode);
template const int TeasSourceAll<Fund>::constrain(const xeona::DomainMode);

template void TeasSourceAll<Oxid>::washup();
template void TeasSourceAll<Cert>::washup();
template void TeasSourceAll<Cseq>::washup();
template void TeasSourceAll<Elec>::washup();
template void TeasSourceAll<Work>::washup();
template void TeasSourceAll<Heat>::washup();
template void TeasSourceAll<Thrm>::washup();
template void TeasSourceAll<Fund>::washup();

// ---------------------------------
//  derived instantiations
// ---------------------------------

typedef CmOxidGas        OGas;
typedef CmOxidNaturalGas NatG;
typedef CmOxidBiocoal    BioC;
typedef CmOxidHydrogen   Htwo;

// class 'TeasLoad<>'

template TeasLoad<OGas>::TeasLoad(const std::string, Record&);
template TeasLoad<NatG>::TeasLoad(const std::string, Record&);
template TeasLoad<BioC>::TeasLoad(const std::string, Record&);
template TeasLoad<Htwo>::TeasLoad(const std::string, Record&);

template TeasLoad<OGas>::~TeasLoad();
template TeasLoad<NatG>::~TeasLoad();
template TeasLoad<BioC>::~TeasLoad();
template TeasLoad<Htwo>::~TeasLoad();

template const int TeasLoad<OGas>::constrain(const xeona::DomainMode);
template const int TeasLoad<NatG>::constrain(const xeona::DomainMode);
template const int TeasLoad<BioC>::constrain(const xeona::DomainMode);
template const int TeasLoad<Htwo>::constrain(const xeona::DomainMode);

template void TeasLoad<OGas>::washup();
template void TeasLoad<NatG>::washup();
template void TeasLoad<BioC>::washup();
template void TeasLoad<Htwo>::washup();

template shared_ptr<GateCom<OGas> > TeasLoad<OGas>::obtainGateway() const;
template shared_ptr<GateCom<NatG> > TeasLoad<NatG>::obtainGateway() const;
template shared_ptr<GateCom<BioC> > TeasLoad<BioC>::obtainGateway() const;
template shared_ptr<GateCom<Htwo> > TeasLoad<Htwo>::obtainGateway() const;

template shared_ptr<BandedTariffSet> TeasLoad<OGas>::obtainTariffSet() const;
template shared_ptr<BandedTariffSet> TeasLoad<NatG>::obtainTariffSet() const;
template shared_ptr<BandedTariffSet> TeasLoad<BioC>::obtainTariffSet() const;
template shared_ptr<BandedTariffSet> TeasLoad<Htwo>::obtainTariffSet() const;

// class 'TeasLoadFin<>'

template TeasLoadFin<OGas>::TeasLoadFin(const std::string, Record&);
template TeasLoadFin<NatG>::TeasLoadFin(const std::string, Record&);
template TeasLoadFin<BioC>::TeasLoadFin(const std::string, Record&);
template TeasLoadFin<Htwo>::TeasLoadFin(const std::string, Record&);

template TeasLoadFin<OGas>::~TeasLoadFin();
template TeasLoadFin<NatG>::~TeasLoadFin();
template TeasLoadFin<BioC>::~TeasLoadFin();
template TeasLoadFin<Htwo>::~TeasLoadFin();

template const int TeasLoadFin<OGas>::constrain(const xeona::DomainMode);
template const int TeasLoadFin<NatG>::constrain(const xeona::DomainMode);
template const int TeasLoadFin<BioC>::constrain(const xeona::DomainMode);
template const int TeasLoadFin<Htwo>::constrain(const xeona::DomainMode);

template void TeasLoadFin<OGas>::washup();
template void TeasLoadFin<NatG>::washup();
template void TeasLoadFin<BioC>::washup();
template void TeasLoadFin<Htwo>::washup();

template shared_ptr<GateCom<OGas> > TeasLoadFin<OGas>::obtainGateway() const;
template shared_ptr<GateCom<NatG> > TeasLoadFin<NatG>::obtainGateway() const;
template shared_ptr<GateCom<BioC> > TeasLoadFin<BioC>::obtainGateway() const;
template shared_ptr<GateCom<Htwo> > TeasLoadFin<Htwo>::obtainGateway() const;

template shared_ptr<BandedTariffSet> TeasLoadFin<OGas>::obtainTariffSet() const;
template shared_ptr<BandedTariffSet> TeasLoadFin<NatG>::obtainTariffSet() const;
template shared_ptr<BandedTariffSet> TeasLoadFin<BioC>::obtainTariffSet() const;
template shared_ptr<BandedTariffSet> TeasLoadFin<Htwo>::obtainTariffSet() const;

// class 'TeasLoadAll<>'

template TeasLoadAll<OGas>::TeasLoadAll(const std::string, Record&);
template TeasLoadAll<NatG>::TeasLoadAll(const std::string, Record&);
template TeasLoadAll<BioC>::TeasLoadAll(const std::string, Record&);
template TeasLoadAll<Htwo>::TeasLoadAll(const std::string, Record&);

template TeasLoadAll<OGas>::~TeasLoadAll();
template TeasLoadAll<NatG>::~TeasLoadAll();
template TeasLoadAll<BioC>::~TeasLoadAll();
template TeasLoadAll<Htwo>::~TeasLoadAll();

template const int TeasLoadAll<OGas>::constrain(const xeona::DomainMode);
template const int TeasLoadAll<NatG>::constrain(const xeona::DomainMode);
template const int TeasLoadAll<BioC>::constrain(const xeona::DomainMode);
template const int TeasLoadAll<Htwo>::constrain(const xeona::DomainMode);

template void TeasLoadAll<OGas>::washup();
template void TeasLoadAll<NatG>::washup();
template void TeasLoadAll<BioC>::washup();
template void TeasLoadAll<Htwo>::washup();

template shared_ptr<GateCom<OGas> > TeasLoadAll<OGas>::obtainGateway() const;
template shared_ptr<GateCom<NatG> > TeasLoadAll<NatG>::obtainGateway() const;
template shared_ptr<GateCom<BioC> > TeasLoadAll<BioC>::obtainGateway() const;
template shared_ptr<GateCom<Htwo> > TeasLoadAll<Htwo>::obtainGateway() const;

template shared_ptr<BandedTariffSet> TeasLoadAll<OGas>::obtainTariffSet() const;
template shared_ptr<BandedTariffSet> TeasLoadAll<NatG>::obtainTariffSet() const;
template shared_ptr<BandedTariffSet> TeasLoadAll<BioC>::obtainTariffSet() const;
template shared_ptr<BandedTariffSet> TeasLoadAll<Htwo>::obtainTariffSet() const;

// 'TeasSource<>'

template TeasSource<OGas>::TeasSource(const std::string, Record&);
template TeasSource<NatG>::TeasSource(const std::string, Record&);
template TeasSource<BioC>::TeasSource(const std::string, Record&);
template TeasSource<Htwo>::TeasSource(const std::string, Record&);

template TeasSource<OGas>::~TeasSource();
template TeasSource<NatG>::~TeasSource();
template TeasSource<BioC>::~TeasSource();
template TeasSource<Htwo>::~TeasSource();

template const int TeasSource<OGas>::constrain(const xeona::DomainMode);
template const int TeasSource<NatG>::constrain(const xeona::DomainMode);
template const int TeasSource<BioC>::constrain(const xeona::DomainMode);
template const int TeasSource<Htwo>::constrain(const xeona::DomainMode);

template void TeasSource<OGas>::washup();
template void TeasSource<NatG>::washup();
template void TeasSource<BioC>::washup();
template void TeasSource<Htwo>::washup();

// 'TeasSourceFin<>'

template TeasSourceFin<OGas>::TeasSourceFin(const std::string, Record&);
template TeasSourceFin<NatG>::TeasSourceFin(const std::string, Record&);
template TeasSourceFin<BioC>::TeasSourceFin(const std::string, Record&);
template TeasSourceFin<Htwo>::TeasSourceFin(const std::string, Record&);

template TeasSourceFin<OGas>::~TeasSourceFin();
template TeasSourceFin<NatG>::~TeasSourceFin();
template TeasSourceFin<BioC>::~TeasSourceFin();
template TeasSourceFin<Htwo>::~TeasSourceFin();

template const int TeasSourceFin<OGas>::constrain(const xeona::DomainMode);
template const int TeasSourceFin<NatG>::constrain(const xeona::DomainMode);
template const int TeasSourceFin<BioC>::constrain(const xeona::DomainMode);
template const int TeasSourceFin<Htwo>::constrain(const xeona::DomainMode);

template void TeasSourceFin<OGas>::washup();
template void TeasSourceFin<NatG>::washup();
template void TeasSourceFin<BioC>::washup();
template void TeasSourceFin<Htwo>::washup();

// 'TeasSourceAll<>'

template TeasSourceAll<OGas>::TeasSourceAll(const std::string, Record&);
template TeasSourceAll<NatG>::TeasSourceAll(const std::string, Record&);
template TeasSourceAll<BioC>::TeasSourceAll(const std::string, Record&);
template TeasSourceAll<Htwo>::TeasSourceAll(const std::string, Record&);

template TeasSourceAll<OGas>::~TeasSourceAll();
template TeasSourceAll<NatG>::~TeasSourceAll();
template TeasSourceAll<BioC>::~TeasSourceAll();
template TeasSourceAll<Htwo>::~TeasSourceAll();

template const int TeasSourceAll<OGas>::constrain(const xeona::DomainMode);
template const int TeasSourceAll<NatG>::constrain(const xeona::DomainMode);
template const int TeasSourceAll<BioC>::constrain(const xeona::DomainMode);
template const int TeasSourceAll<Htwo>::constrain(const xeona::DomainMode);

template void TeasSourceAll<OGas>::washup();
template void TeasSourceAll<NatG>::washup();
template void TeasSourceAll<BioC>::washup();
template void TeasSourceAll<Htwo>::washup();

//  end of file

