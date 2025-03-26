//  XEONA ENERGY SYSTEMS MODELING ENVIRONMENT
//
//  file-create-name : teas04.h
//  file-create-date : Thu 08-Oct-2009 12:57 UTC
//  file-initiator   : Robbie Morrison <robbie@actrix.co.nz>
//
//  file-role        : concrete technical assets 4 / header
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
//  $URL: file:///home/robbie/svn-root/xeona/futz/trunk/xeona1/b/teas04.h $
//
//  GENERAL NOTES FOR THIS FILE

//  HEADER GUARD

#ifndef _TEAS04_H_
#define _TEAS04_H_

//  LOCAL AND SYSTEM INCLUDES

#include "../b/teas.h"        // technical asset entity

#include "../a/logger_fwd.h"  // some forward declarations from "logger.h"
#include "../c/smart_ptr.h"   // toggle between Boost and TR1 smart pointers

#include <string>             // C++ strings
#include <sstream>            // string-streams

//  FORWARD (PARTIAL) DECLARATIONS

class Record;                                // constructor argument
namespace svif { class SolverIf; }           // member function argument

template <typename C> class Cable;           // 'C' is of base class 'Commodity' etc
template <typename C> class Socket;

//  CODE

// ---------------------------------------------------------
//  CLASS           : TeasLoad <>
// ---------------------------------------------------------
//  Description  : operator-coupled load entity
//  Role         : concrete entity
//  Techniques   : also forms base for 'TeasLoadFin' and 'TeasLoadAll'
//  Status       : complete
// ---------------------------------------------------------

template <typename C> class GateCom;         // used by utility functions

class OpsFac1Out0_A;                         // necessary for OSP typedef below

template <typename C>                        // 'C' for commodity
class TeasLoad :
  public TechnicalAsset

{
  // USING DECLARATIONS

protected:

  using Entity::s_logger;                    // place in common scope for this class

  // TYPEDEFS

private:

  typedef OpsFac1Out0_A OpsFac1Out0;         // used for switching implementations

  // DISABLED

private:

  TeasLoad();                                     // zero-argument constructor
  TeasLoad(const TeasLoad& orig);                 // copy constructor
  TeasLoad& operator= (const TeasLoad& orig);     // copy assignment operator

  // CREATORS

public:

  explicit
  TeasLoad
  (const std::string entityId,
   Record&           record);

  virtual
  ~TeasLoad();

  // CALLS

public:

  virtual void establish() { }               // necessary redefinition

  virtual
  const int                                  // duty gol
  constrain
  (const xeona::DomainMode capacityMode);

  virtual
  void
  washup();

  virtual void conclude()  { }               // necessary redefinition

  // ADAPTIVE SUPPORT

public:

  virtual
  shared_ptr<BandedTariffSet>                // current tariff set for adaptive behavior
  obtainTariffSet() const;

  // UTILITY FUNCTIONS

protected:

  shared_ptr<GateCom<C> >
  obtainGateway() const;

  // INSTANCE DATA

protected:                                   // CAUTION: needed by derived classes

  // tied quantities

  const std::string&                  d_commodity;     // for the 'create' interface calls
  const double&                       d_demandHiBound;

  shared_ptr<std::vector<double> >    d_loads;

  const shared_ptr<Cable<C> >         d_inCommodity;

  // local quantities

  shared_ptr<OpsFac1Out0>             d_ops;           // specialization required

};

//  ==== XEDOC =================================================
//
//  entity.teas-load-*-0
//
//        base commodity in {Cert,Cseq,Elec,Fund,Heat,Oxid,Thrm,Work}
//        or derived in {OxidBiocoal,OxidGas,OxidHydrogen,OxidNaturalGas}
//        quantifying extensity * in {J,kg,$}
//
//      class                                    > TeasLoad:Elec
//
//        a load entity which is operator coupled for load
//        definition, for example to AsopInelasticTs, but without
//        support for costs
//
//      builtin-remark s                         <
//
//      socket-1 l                               > "teas-supplier-1.elec-1"
//
//        socket-1 is my supplier
//
//      cable-commodity l                        > "cm-electricity-0"
//
//        cable-commodity defines the underlying commodity
//
//      demand-hi-bound [*/s] f                  > 5.0e+06
//
//      loads [*/s] F                            < 0.0 ..
//
//        demand-hi-bound is the maximum load, loads are the
//        actual loads
//
//  ============================================================

// ---------------------------------------------------------
//  CLASS           : TeasLoadFin <>
// ---------------------------------------------------------
//  Description  : operator-coupled load entity
//  Role         : inherits from 'TeasLoad'
//  Techniques   : (nothing special)
//  Status       : complete
// ---------------------------------------------------------

template <typename C> class GateCom;         // used by utility functions

class OpsFac1Out0_A;                         // necessary for OSP typedef below

template <typename C>                        // 'C' for commodity
class TeasLoadFin :
  public TeasLoad<C>,                        // note the template
  public CostRegisterSRFin,
  public CostRegisterEmbFin

{
  // USING DECLARATIONS

protected:

  using Entity::s_logger;                    // place in common scope for this class
  using Entity::getIdAndKind;
  using Entity::getIdentifier;

  using FullEntity::d_builtinRemark;

  using TicToc::d_commitmentMode;
  using TicToc::d_step;
  using TicToc::d_solver;

  using Block::d_dutyStats;
  using Block::d_sizeStats;

  using TechnicalAsset::d_floorDuty;
  using TechnicalAsset::d_ceilingDuty;

  using TeasLoad<C>::d_commodity;
  using TeasLoad<C>::d_demandHiBound;
  using TeasLoad<C>::d_loads;
  using TeasLoad<C>::d_inCommodity;
  using TeasLoad<C>::d_ops;

  // TYPEDEFS

private:

  typedef OpsFac1Out0_A OpsFac1Out0;         // used for switching implementations

  // DISABLED

private:

  TeasLoadFin();                                       // zero-argument constructor
  TeasLoadFin(const TeasLoadFin& orig);                // copy constructor
  TeasLoadFin& operator= (const TeasLoadFin& orig);    // copy assignment operator

  // CREATORS

public:

  explicit
  TeasLoadFin
  (const std::string entityId,
   Record&           record);

  virtual
  ~TeasLoadFin();

  // CALLS

public:

  virtual void establish() { }               // necessary redefinition

  virtual
  const int                                  // duty gol
  constrain
  (const xeona::DomainMode capacityMode);

  virtual
  void
  washup();

  virtual void conclude()  { }               // necessary redefinition

  // ADAPTIVE SUPPORT

public:

  virtual
  shared_ptr<BandedTariffSet>                // current tariff set for adaptive behavior
  obtainTariffSet() const;

  // UTILITY FUNCTIONS

protected:

  shared_ptr<GateCom<C> >
  obtainGateway() const;

  // INSTANCE DATA

private:

  // no data is correct

};

//  ==== XEDOC =================================================
//
//  entity.teas-load-fin-*-0
//
//        base commodity in {Cert,Cseq,Elec,Fund,Heat,Oxid,Thrm,Work}
//        or derived in {OxidBiocoal,OxidGas,OxidHydrogen,OxidNaturalGas}
//        quantifying extensity * in {J,kg,$}
//
//      class                                    > TeasLoadFin:Elec
//
//        a load entity which is operator coupled for load
//        definition, for example to AsopInelasticTs, and with
//        support for financial costs
//
//      builtin-remark s                         <
//
//      socket-1 l                               > "teas-supplier-1.elec-1"
//
//        socket-1 is my supplier
//
//      cable-commodity l                        > "cm-electricity-0"
//
//        cable-commodity defines the underlying commodity
//
//      demand-hi-bound [*/s] f                  > 5.0e+06
//
//      loads [*/s] F                            < 0.0 ..
//
//        demand-hi-bound is the maximum load, loads are the
//        actual loads
//
//      nameplate-capacity [*/s] f               > 6.0e+06
//      duty-specific-cost-financial [$/*] f     > 100.0
//      size-specific-cost-financial [$/*/s/s] f > 200.0
//      standing-cost-financial [$/s] f          > 300.0
//
//      variable-costs-financial [$] F           < 0.0 ..
//      fixed-costs-financial [$] F              < 0.0 ..
//
//      annual-discount-rate-decimal [-] f       > 0.10
//      economic-life [y] i                      > 25
//      capex-initial [$] f                      > 120e+03
//      capex-terminal [$] f                     > -10e+03
//      current-age [y] i                        > 2
//
//        a negative capex-terminal indicates salvage income as
//        opposed to decommissioning cost
//
//      embedded-costs-financial [$] F           < 0.0 ..
//
//        the embedded-costs-financial are calculated using the
//        DCF annuity method over the economic life of the entity
//        in question
//
//  ============================================================

// ---------------------------------------------------------
//  CLASS           : TeasLoadAll <>
// ---------------------------------------------------------
//  Description  : operator-coupled load entity
//  Role         : concrete entity
//  Role         : inherits from 'TeasLoad'
//  Status       : complete
// ---------------------------------------------------------

template <typename C> class GateCom;         // used by utility functions

class OpsFac1Out0_A;                         // necessary for OSP typedef below

template <typename C>                        // 'C' for commodity
class TeasLoadAll :
  public TeasLoad<C>,                        // note the template
  public CostRegisterSRAll,
  public CostRegisterEmbAll

{
  // USING DECLARATIONS

protected:

  using Entity::s_logger;                    // place in common scope for this class
  using Entity::getIdAndKind;
  using Entity::getIdentifier;

  using FullEntity::d_builtinRemark;

  using TicToc::d_commitmentMode;
  using TicToc::d_step;
  using TicToc::d_solver;

  using Block::d_dutyStats;
  using Block::d_sizeStats;

  using TechnicalAsset::d_floorDuty;
  using TechnicalAsset::d_ceilingDuty;

  using TeasLoad<C>::d_commodity;
  using TeasLoad<C>::d_demandHiBound;
  using TeasLoad<C>::d_loads;
  using TeasLoad<C>::d_inCommodity;
  using TeasLoad<C>::d_ops;

  // TYPEDEFS

private:

  typedef OpsFac1Out0_A OpsFac1Out0;         // used for switching implementations

  // DISABLED

private:

  TeasLoadAll();                                       // zero-argument constructor
  TeasLoadAll(const TeasLoadAll& orig);                // copy constructor
  TeasLoadAll& operator= (const TeasLoadAll& orig);    // copy assignment operator

  // CREATORS

public:

  explicit
  TeasLoadAll
  (const std::string entityId,
   Record&           record);

  virtual
  ~TeasLoadAll();

  // CALLS

public:

  virtual void establish() { }               // necessary redefinition

  virtual
  const int                                  // duty gol
  constrain
  (const xeona::DomainMode capacityMode);

  virtual
  void
  washup();

  virtual void conclude()  { }               // necessary redefinition

  // ADAPTIVE SUPPORT

public:

  virtual
  shared_ptr<BandedTariffSet>                // current tariff set for adaptive behavior
  obtainTariffSet() const;

  // UTILITY FUNCTIONS

protected:

  shared_ptr<GateCom<C> >
  obtainGateway() const;

  // INSTANCE DATA

private:

  // no data is correct

};

//  ==== XEDOC =================================================
//
//  entity.teas-load-all-*-0
//
//        base commodity in {Cert,Cseq,Elec,Fund,Heat,Oxid,Thrm,Work}
//        or derived in {OxidBiocoal,OxidGas,OxidHydrogen,OxidNaturalGas}
//        quantifying extensity * in {J,kg,$}
//
//      class                                    > TeasLoadAll:Elec
//
//        a load entity which is operator coupled for load
//        definition, for example to AsopInelasticTs, and with
//        support for all costs
//
//      builtin-remark s                         <
//
//      socket-1 l                               > "teas-supplier-1.elec-1"
//
//        socket-1 is my supplier
//
//      cable-commodity l                        > "cm-electricity-0"
//
//        cable-commodity defines the underlying commodity
//
//      demand-hi-bound [*/s] f                  > 5.0e+06
//
//      loads [*/s] F                            < 0.0 ..
//
//        demand-hi-bound is the maximum load, loads are the
//        actual loads
//
//      nameplate-capacity [*/s] f               > 6.0e+06
//      duty-specific-cost-financial [$/*] f     > 100.0
//      size-specific-cost-financial [$/*/s/s] f > 200.0
//      standing-cost-financial [$/s] f          > 300.0
//
//      variable-costs-financial [$] F           < 0.0 ..
//      fixed-costs-financial [$] F              < 0.0 ..
//
//      annual-discount-rate-decimal [-] f       > 0.10
//      economic-life [y] i                      > 25
//      capex-initial [$] f                      > 120e+03
//      capex-terminal [$] f                     > -10e+03
//      current-age [y] i                        > 2
//
//        a negative capex-terminal indicates salvage income as
//        opposed to decommissioning cost
//
//      embedded-costs-financial [$] F           < 0.0 ..
//
//        the embedded-costs-financial are calculated using the
//        DCF annuity method over the economic life of the entity
//        in question
//
//      duty-specific-cost-greenhouse [kg/.] f   > 1.0
//      size-specific-cost-greenhouse [kg/*/s] f > 2.0
//      standing-cost-greenhouse [kg/s] f        > 3.0
//      duty-specific-cost-nox [kg/.] f          > 1.0
//      size-specific-cost-nox [kg/*/s] f        > 2.0
//      standing-cost-nox [kg/s] f               > 3.0
//      duty-specific-cost-depletion [J/.] f     > 1.0
//      size-specific-cost-depletion [J/*/s] f   > 2.0
//      standing-cost-depletion [J/s] f          > 3.0
//      duty-specific-cost-landuse [m2/.] f      > 1.0
//      size-specific-cost-landuse [m2/*/s] f    > 2.0
//      standing-cost-landuse [m2/s] f           > 3.0
//
//      variable-costs-greenhouse [kg] F         < 0.0 ..
//      fixed-costs-greenhouse [kg] F            < 0.0 ..
//      variable-costs-nox [kg] F                < 0.0 ..
//      fixed-costs-nox [kg] F                   < 0.0 ..
//      variable-costs-depletion [J] F           < 0.0 ..
//      fixed-costs-depletion [J] F              < 0.0 ..
//      variable-costs-landuse [m2] F            < 0.0 ..
//      fixed-costs-landuse [m2] F               < 0.0 ..
//
//      physical-life [y] i                      > 30
//      investment-greenhouse [kg] f             > 3.0e+03
//      investment-nox [kg] f                    > 0.0
//      investment-depletion [J] f               > 3.0e+03
//      investment-landuse [m2] f                > 0.0
//
//      embedded-costs-greenhouse [kg] F         < 0.0 ..
//      embedded-costs-nox [kg] F                < 0.0 ..
//      embedded-costs-depletion [J] F           < 0.0 ..
//      embedded-costs-landuse [m2] F            < 0.0 ..
//
//  ============================================================

// ---------------------------------------------------------
//  CLASS           : TeasSource <>
// ---------------------------------------------------------
//  Description  : operator-coupled commodity source entity without costs
//  Role         : concrete entity
//  Techniques   : also forms base for 'TeasSourceFin' and 'TeasSourceAll'
//  Status       : complete
// ---------------------------------------------------------

class OpsFac0Out1_A;                         // necessary for OSP typedef below

template <typename C>                        // 'C' for commodity
class TeasSource :
  public TechnicalAsset
{
  // USING DECLARATIONS

protected:

  using Entity::s_logger;                    // place in common scope for this class

  // TYPEDEFS

private:

  typedef OpsFac0Out1_A OpsFac0Out1;         // used for switching implementations

  // DISABLED

private:

  TeasSource();                                        // zero-argument constructor
  TeasSource(const TeasSource& orig);                  // copy constructor
  TeasSource& operator= (const TeasSource& orig);      // copy assignment operator

  // CREATORS

public:

  explicit
  TeasSource
  (const std::string entityId,
   Record&           record);

  virtual
  ~TeasSource();

  // CALLS

public:

  virtual void establish() { }               // necessary redefinition

  virtual
  const int                                  // duty gol
  constrain
  (const xeona::DomainMode capacityMode);

  virtual
  void
  washup();

  virtual void conclude()  { }               // necessary redefinition

  // INSTANCE DATA

protected:                                   // CAUTION: needed by derived classes

  // tied quantities

  const std::string&                  d_commodity;     // for 'create' interface calls
  const double&                       d_extractLoBound;
  const double&                       d_extractHiBound;

  shared_ptr<std::vector<double> >    d_extractions;
  shared_ptr<Socket<C> >              d_outCommodity;

  // local quantities

  shared_ptr<OpsFac0Out1>             d_ops;           // specialization required

}; // class 'TeasSource<C>'

//  ==== XEDOC =================================================
//
//  entity.teas-source-*-0
//
//        base commodity in {Cert,Cseq,Elec,Fund,Heat,Oxid,Thrm,Work}
//        or derived in {OxidBiocoal,OxidGas,OxidHydrogen,OxidNaturalGas}
//        quantifying extensity * in {J,kg,$}
//
//      class                                    > TeasSource:Elec
//
//        an operator-coupled source entity with support for
//        capacity bounds but not costs
//
//        my socket label is 'sock-1'
//
//      builtin-remark s                         <
//
//      socket-commodity l                       > "cm-electricity-0"
//
//        socket-commodity defines the supplied commodity
//
//      extract-lo-bound [*/s] f                 > 0.0
//      extract-hi-bound [*/s] f                 > 8.0e+06
//
//        extract-hi-bound is used by the simulation whereas
//        nameplate-capacity informs the financial calculations
//
//      extractions [*/s] F                      < 0.0 ..
//
//        extractions are the actual extractions
//
//  ============================================================

// ---------------------------------------------------------
//  CLASS           : TeasSourceFin <>
// ---------------------------------------------------------
//  Description  : operator-coupled commodity source entity
//  Role         : concrete entity
//  Techniques   : inherits from 'TeasSource'
//  Status       : complete
// ---------------------------------------------------------

template <typename C>                        // 'C' for commodity
class TeasSourceFin :
  public TeasSource<C>,                      // note the template
  public CostRegisterSRFin,
  public CostRegisterEmbFin
{
  // USING DECLARATIONS

protected:

  using Entity::s_logger;                    // place in common scope for this class
  using Entity::getIdAndKind;
  using Entity::getIdentifier;

  using FullEntity::d_builtinRemark;

  using TicToc::d_commitmentMode;
  using TicToc::d_step;
  using TicToc::d_solver;

  using Block::d_dutyStats;
  using Block::d_sizeStats;

  using TechnicalAsset::d_floorDuty;
  using TechnicalAsset::d_ceilingDuty;

  using TeasSource<C>::d_commodity;
  using TeasSource<C>::d_extractLoBound;
  using TeasSource<C>::d_extractHiBound;
  using TeasSource<C>::d_extractions;
  using TeasSource<C>::d_outCommodity;
  using TeasSource<C>::d_ops;

  // TYPEDEFS

private:

  typedef OpsFac0Out1_A OpsFac0Out1;         // used for switching implementations

  // DISABLED

private:

  TeasSourceFin();                                          // zero-argument constructor
  TeasSourceFin(const TeasSourceFin& orig);                 // copy constructor
  TeasSourceFin& operator= (const TeasSourceFin& orig);     // copy assignment operator

  // CREATORS

public:

  explicit
  TeasSourceFin
  (const std::string entityId,
   Record&           record);

  virtual
  ~TeasSourceFin();

  // CALLS

public:

  virtual void establish() { }               // necessary redefinition

  virtual
  const int                                  // duty gol
  constrain
  (const xeona::DomainMode capacityMode);

  virtual
  void
  washup();

  virtual void conclude()  { }               // necessary redefinition

  // INSTANCE DATA

private:

  // no data is correct

}; // class 'TeasSourceFin<C>'

//  ==== XEDOC =================================================
//
//  entity.teas-source-fin-*-0
//
//        base commodity in {Cert,Cseq,Elec,Fund,Heat,Oxid,Thrm,Work}
//        or derived in {OxidBiocoal,OxidGas,OxidHydrogen,OxidNaturalGas}
//        quantifying extensity * in {J,kg,$}
//
//      class                                    > TeasSourceFin:Elec
//
//        an operator-coupled source entity with support for
//        capacity bounds and financial costs
//
//        my socket label is 'sock-1'
//
//      builtin-remark s                         <
//
//      socket-commodity l                       > "cm-electricity-0"
//
//        socket-commodity defines the supplied commodity
//
//      extract-lo-bound [*/s] f                 > 0.0
//      extract-hi-bound [*/s] f                 > 8.0e+06
//
//        extract-hi-bound is used by the simulation whereas
//        nameplate-capacity informs the financial calculations
//
//      extractions [*/s] F                      < 0.0 ..
//
//        extractions are the actual extractions
//
//      nameplate-capacity [*/s] f               > 6.0e+06
//      duty-specific-cost-financial [$/*] f     > 100.0
//      size-specific-cost-financial [$/*/s/s] f > 200.0
//      standing-cost-financial [$/s] f          > 300.0
//
//      variable-costs-financial [$] F           < 0.0 ..
//      fixed-costs-financial [$] F              < 0.0 ..
//
//      annual-discount-rate-decimal [-] f       > 0.10
//      economic-life [y] i                      > 25
//      capex-initial [$] f                      > 120e+03
//      capex-terminal [$] f                     > -10e+03
//      current-age [y] i                        > 2
//
//        a negative capex-terminal indicates salvage income as
//        opposed to decommissioning cost
//
//      embedded-costs-financial [$] F           < 0.0 ..
//
//        the embedded-costs-financial are calculated using the
//        DCF annuity method over the economic life of the entity
//        in question
//
//  ============================================================

// ---------------------------------------------------------
//  CLASS           : TeasSourceAll <>
// ---------------------------------------------------------
//  Description  : operator-coupled commodity source entity with depletion costs
//  Role         : concrete entity
//  Techniques   : inherits from 'TeasSource'
//  Status       : complete
// ---------------------------------------------------------

class OpsFac0Out1_A;                         // necessary for OSP typedef below

template <typename C>                        // 'C' for commodity
class TeasSourceAll :
  public TeasSource<C>,                      // note the template
  public CostRegisterSRAll,                  // primarily for the depletion costs
  public CostRegisterEmbAll                  // primarily for the depletion costs
{
  // USING DECLARATIONS

protected:

  using Entity::s_logger;                    // place in common scope for this class
  using Entity::getIdAndKind;
  using Entity::getIdentifier;

  using FullEntity::d_builtinRemark;

  using TicToc::d_commitmentMode;
  using TicToc::d_step;
  using TicToc::d_solver;

  using Block::d_dutyStats;
  using Block::d_sizeStats;

  using TechnicalAsset::d_floorDuty;
  using TechnicalAsset::d_ceilingDuty;

  using TeasSource<C>::d_commodity;
  using TeasSource<C>::d_extractLoBound;
  using TeasSource<C>::d_extractHiBound;
  using TeasSource<C>::d_extractions;
  using TeasSource<C>::d_outCommodity;
  using TeasSource<C>::d_ops;

  // TYPEDEFS

private:

  typedef OpsFac0Out1_A OpsFac0Out1;         // used for switching implementations

  // DISABLED

private:

  TeasSourceAll();                                          // zero-argument constructor
  TeasSourceAll(const TeasSourceAll& orig);                 // copy constructor
  TeasSourceAll& operator= (const TeasSourceAll& orig);     // copy assignment operator

  // CREATORS

public:

  explicit
  TeasSourceAll
  (const std::string entityId,
   Record&           record);

  virtual
  ~TeasSourceAll();

  // CALLS

public:

  virtual void establish() { }               // necessary redefinition

  virtual
  const int                                  // duty gol
  constrain
  (const xeona::DomainMode capacityMode);

  virtual
  void
  washup();

  virtual void conclude()  { }               // necessary redefinition

  // INSTANCE DATA

private:

  // no data is correct

}; // class 'TeasSourceAll<C>'

//  ==== XEDOC =================================================
//
//  entity.teas-source-all-*-0
//
//        base commodity in {Cert,Cseq,Elec,Fund,Heat,Oxid,Thrm,Work}
//        or derived in {OxidBiocoal,OxidGas,OxidHydrogen,OxidNaturalGas}
//        quantifying extensity * in {J,kg,$}
//
//      class                                    > TeasSourceAll:Elec
//
//        an operator-coupled source entity with support for
//        capacity bounds and all costs
//
//        my socket label is 'sock-1'
//
//      builtin-remark s                         <
//
//      socket-commodity l                       > "cm-electricity-0"
//
//        socket-commodity defines the supplied commodity
//
//      extract-lo-bound [*/s] f                 > 0.0
//      extract-hi-bound [*/s] f                 > 8.0e+06
//
//        extract-hi-bound is used by the simulation whereas
//        nameplate-capacity informs the financial calculations
//
//      extractions [*/s] F                      < 0.0 ..
//
//        extractions are the actual extractions
//
//      nameplate-capacity [*/s] f               > 6.0e+06
//      duty-specific-cost-financial [$/*] f     > 100.0
//      size-specific-cost-financial [$/*/s/s] f > 200.0
//      standing-cost-financial [$/s] f          > 300.0
//
//      variable-costs-financial [$] F           < 0.0 ..
//      fixed-costs-financial [$] F              < 0.0 ..
//
//      annual-discount-rate-decimal [-] f       > 0.10
//      economic-life [y] i                      > 25
//      capex-initial [$] f                      > 120e+03
//      capex-terminal [$] f                     > -10e+03
//      current-age [y] i                        > 2
//
//        a negative capex-terminal indicates salvage income as
//        opposed to decommissioning cost
//
//      embedded-costs-financial [$] F           < 0.0 ..
//
//        the embedded-costs-financial are calculated using the
//        DCF annuity method over the economic life of the entity
//        in question
//
//      duty-specific-cost-greenhouse [kg/.] f   > 1.0
//      size-specific-cost-greenhouse [kg/*/s] f > 2.0
//      standing-cost-greenhouse [kg/s] f        > 3.0
//      duty-specific-cost-nox [kg/.] f          > 1.0
//      size-specific-cost-nox [kg/*/s] f        > 2.0
//      standing-cost-nox [kg/s] f               > 3.0
//      duty-specific-cost-depletion [J/.] f     > 1.0
//      size-specific-cost-depletion [J/*/s] f   > 2.0
//      standing-cost-depletion [J/s] f          > 3.0
//      duty-specific-cost-landuse [m2/.] f      > 1.0
//      size-specific-cost-landuse [m2/*/s] f    > 2.0
//      standing-cost-landuse [m2/s] f           > 3.0
//
//      variable-costs-greenhouse [kg] F         < 0.0 ..
//      fixed-costs-greenhouse [kg] F            < 0.0 ..
//      variable-costs-nox [kg] F                < 0.0 ..
//      fixed-costs-nox [kg] F                   < 0.0 ..
//      variable-costs-depletion [J] F           < 0.0 ..
//      fixed-costs-depletion [J] F              < 0.0 ..
//      variable-costs-landuse [m2] F            < 0.0 ..
//      fixed-costs-landuse [m2] F               < 0.0 ..
//
//      physical-life [y] i                      > 30
//      investment-greenhouse [kg] f             > 3.0e+03
//      investment-nox [kg] f                    > 0.0
//      investment-depletion [J] f               > 3.0e+03
//      investment-landuse [m2] f                > 0.0
//
//      embedded-costs-greenhouse [kg] F         < 0.0 ..
//      embedded-costs-nox [kg] F                < 0.0 ..
//      embedded-costs-depletion [J] F           < 0.0 ..
//      embedded-costs-landuse [m2] F            < 0.0 ..
//
//  ============================================================

#endif // _TEAS04_H_

//  end of file

