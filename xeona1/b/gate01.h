//  XEONA ENERGY SYSTEMS MODELING ENVIRONMENT
//
//  file-create-name : gate1.h
//  file-create-date : Wed 15-Apr-2009 21:34 UTC
//  file-initiator   : Robbie Morrison <robbie@actrix.co.nz>
//
//  file-role        : concrete gateways 1 - stated tariff / header
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
//  $URL: file:///home/robbie/svn-root/xeona/futz/trunk/xeona1/b/gate01.h $
//
//  GENERAL NOTES FOR THIS FILE

//  HEADER GUARD

#ifndef _GATE01_H_
#define _GATE01_H_

//  LOCAL AND SYSTEM INCLUDES

#include "../b/gate.h"        // gateway entity

#include "../b/tictoc.h"      // inherited interface for entities using common calls
#include "../b/block.h"       // block entity

#include "../a/logger_fwd.h"  // some forward declarations from "logger.h"
#include "../c/smart_ptr.h"   // toggle between Boost and TR1 smart pointers

#include <string>             // C++ strings
#include <sstream>            // string-streams

//  FORWARD (PARTIAL) DECLARATIONS

class BandedTariffSet;                       // from "b/bandtaf.h" with banded tariff set

//  CODE

// ---------------------------------------------------------
//  CLASS           : GateStatedTariff <>
// ---------------------------------------------------------
//  Description  : prescribed tariffset with embedded information
//  Role         : concrete class (and the first gateway implemented)
//  Techniques   : the 'optgate' OSP will later support concave tariffsets
//  Status       : complete
//
//  Design notes
//
//      Intent
//
//          In this class, the tariffset is prescribed -- meaning
//          that it is not stochastic, not unilaterally set by
//          one or other party, and not jointly renegotiated at
//          each interval.  Indeed, the modeler can be
//          interpreted as, in some sense at least, the broker of
//          this contract.
//
//      Commitment mode sum
//
//          The commitment mode sum MUST be specified as a
//          default constructor argument.
//
//      The 'using declaration'
//
//          First see Stroustrup (1997 pp392-394).  In this case,
//          the "protected" access specifier restricts external
//          visibility.
//
//     The so-called in-side tariffset structure
//
//         Data member is 'd_ofr' is shared between the buy and
//         sell sides.  Hence the designation "in" side.
//
//  CAUTION: function 'initialize'
//
//      Function 'initialize' does not need redefinition here but
//      note the need to resolve the call (see the unit test for
//      a working example), namely:
//
//          buygate->BuySide::initialize(step, solver);
//
//  CAUTION: "object missing" error
//
//      This can be solved by using a correct scope resolution,
//      for example:
//
//          std::cout << GateCom<C>::SelSide::d_commitmentMode;
//
//      or alternatively (absolutely not recommended):
//
//          std::cout << this->SelSide::d_commitmentMode;
//
//      to avoid the following compiler complaint:
//
//          "error: object missing in reference to 'TicToc::d_commitmentMode'"
//
// ---------------------------------------------------------

//  FORWARD (PARTIAL) DECLARATIONS

class OfrTariffSet_A;                        // CAUTION: for class declaration typedef
class QanObligToSupply_A;                    // CAUTION: for class declaration typedef
class QanTechCapacity_A;                     // CAUTION: for class declaration typedef

//  CODE

template <typename C>
class GateStatedTariff :
  public GateCom<C>
{
  // USING DECLARATIONS

protected:

  // CAUTION: the following place variables in common scope for
  // this class, thereby remove a "declaration must be available"
  // or "not declared in this scope" errors

  using Entity::s_logger;
  using Entity::getIdentifier;
  using Entity::getIdAndKind;

  using Block::d_dutyStats;
  using Block::d_sizeStats;

  using Gateway::d_techCapacity;
  using Gateway::d_commCapacity;
  using Gateway::d_capacity;
  using Gateway::d_transaction;
  using Gateway::d_totalCost;

  using GateCom<C>::d_cable;
  using GateCom<C>::d_socket;

  // TYPEDEFS

protected:

  typedef OfrTariffSet_A     OfrTariffSet;        // used for switching implementations
  typedef QanObligToSupply_A QanObligToSupply;    // used for switching implementations
  typedef QanTechCapacity_A  QanTechCapacity;     // used for switching implementations

  // CREATORS

public:

  explicit
  GateStatedTariff
  (const std::string entityId,
   Record&           record,
   const int         commitmentModeSum = xeona::e_commitmentModes);

  virtual
  ~GateStatedTariff();

  // ACCESSORS

  virtual
  shared_ptr<BandedTariffSet>
  obtainTariffSet() const
  {
    // initial reporting
    s_logger->repx(logga::adhc, "entering member function", "");

    // return, noting that the tariff set is not "locked" and
    // could be modified by external objects -- that said,
    // function 'BandedTariffSet::getMarginalPrice' is 'const'

    return d_tariffset;
  }

  // MANIPULATORS

public:

  // from class 'TicToc'

  virtual void establish();                  // necessary redefinition, currently hollow
  virtual void conclude();                   // necessary redefinition, currently hollow

  // from class 'Gateway'

  virtual void initializeBuySide(const int step, shared_ptr<svif::SolverIf> solver);
  virtual void initializeSelSide(const int step, shared_ptr<svif::SolverIf> solver);

  virtual const int constrainBuySide(const xeona::DomainMode capacityMode);
  virtual const int constrainSelSide(const xeona::DomainMode capacityMode);

  virtual void washupBuySide();
  virtual void washupSelSide();

  // PROVIDED FOR DEVELOPMENT ASSISTANCE

  void
  sayStatus
  (std::ostream& os);

  // INSTANCE DATA

protected:                                   // derived classes exist

  // tied quantities

  shared_ptr<std::vector<std::string> >    d_tariffsets;
  shared_ptr<std::vector<double> >         d_definedCapacitys;   // [1]

  // [1] normally invariant -- but the technical capacity could
  // be made context-dependent in a derived class if need be

  shared_ptr<std::vector<double> >         d_quantitys;
  shared_ptr<std::vector<double> >         d_marginalPrices;
  shared_ptr<std::vector<double> >         d_totalCosts;

  // internal quantities

  shared_ptr<BandedTariffSet>              d_tariffset;     // filled using 'pushString'

  // OSP specializations required

  shared_ptr<QanTechCapacity>              d_capSel;        // sel-side capacity
  shared_ptr<QanTechCapacity>              d_capBuy;        // buy-side capacity
  shared_ptr<QanObligToSupply>             d_ots;           // sel-side obligation
  shared_ptr<OfrTariffSet>                 d_ofr;           // in-side tariffset

}; // class 'GateStatedTariff'

//  ==== XEDOC =================================================
//
//  entity.gate-stated-tariff-*-0
//
//        base commodity in {Cert,Cseq,Elec,Fund,Heat,Oxid,Thrm,Work}
//        or derived in {OxidBiocoal,OxidGas,OxidHydrogen,OxidNaturalGas}
//        quantifying extensity * in {J,kg,$}
//
//      class                                    > GateStatedTariff:Work
//
//        gateway entity which uses stated (rather than
//        stochastic, unilaterally set, or negotiated) tariffs
//
//        time-of-use (TOU) tariffs can be explicitly given in
//        the tariffsets timeseries
//
//        the 'Cm*' qualifier (here * = Work) specifies the
//        relevant high-level commodity class
//
//        my socket label is 'sock-1'
//
//      builtin-remark s                         <
//
//      socket l                                 > "teas-1.work-1"
//      common-commodity l                       > "cm-work-0"
//
//        the above fields define my supplier and their socket
//        label and our common commodity
//
//      tariffsets X                             > "1.6e-06 * 40.00e+06 28.00e-09" ..
//
//        the optional leading term (1.6e-06) in the tariffsets
//        field represents a fixed charge [$/s] -- the remaining
//        star-separated pairs being not necessarily convex
//        quantity/price (*, $) offers
//
//      defined-capacitys [*/s] F                > 50.00e+06 ..
//
//        data capture follows
//
//      quantitys [*/s] F                        < 0.0 ..
//      marginal-prices [$/*] F                  < 0.0 ..
//      total-costs [$] F                        < 0.0 ..
//
//  ============================================================

// ---------------------------------------------------------
//  CLASS           : GateStatedTariffEFac
// ---------------------------------------------------------
//  Description  : stated tariffs gateway with an average CO2e emissions factor
//  Role         : concrete entity
//  Techniques   : inherits from 'GateStatedTariff'
//  Status       : complete
//
//  Design notes
//
//      This class inherits from the basic 'GateStatedTariff'
//      gateway.
//
//      But this class does NOT inherit from a cost register like
//      'CostRegisterSRAll' because the cost contributions that
//      would arise would then be consolidated into the combined
//      accounts and wrongly double counted.
//
//      The motivation for this gateway is to "include" a carbon
//      dioxide equivalent penalty onto an incoming electricity
//      supply stream.  It is a modeling "hack" in the sense that
//      this information is not available from the supply side --
//      and a good example of market transactions destroying
//      information.
//
//      This information is provided for by the in-data field
//      "ghg-unit-penalty" to represent an average value (it
//      could also be read in as a timeseries and not a scalar).
//
// ---------------------------------------------------------

//  FORWARD (PARTIAL) DECLARATIONS

class OfrTariffSet_A;                        // CAUTION: for class declaration typedef
class QanObligToSupply_A;                    // CAUTION: for class declaration typedef
class QanTechCapacity_A;                     // CAUTION: for class declaration typedef

class CostSet;                               // unusual for a gateway to contain a costset

//  CODE

template <typename C>
class GateStatedTariffEFac :
  public GateStatedTariff<C>
{
  // USING DECLARATIONS

protected:

  // CAUTION: the following place variables in common scope for
  // this class, thereby remove a "declaration must be available"
  // or "not declared in this scope" errors

  using Entity::s_logger;
  using Entity::getIdentifier;
  using Entity::getIdAndKind;

  using Block::d_dutyStats;
  using Block::d_sizeStats;

  using Gateway::d_techCapacity;
  using Gateway::d_commCapacity;
  using Gateway::d_capacity;
  using Gateway::d_transaction;
  using Gateway::d_totalCost;

  using GateCom<C>::d_cable;
  using GateCom<C>::d_socket;

  using GateStatedTariff<C>::d_definedCapacitys;
  using GateStatedTariff<C>::d_capBuy;
  using GateStatedTariff<C>::d_ofr;
  using GateStatedTariff<C>::d_tariffset;

  // TYPEDEFS

protected:

  typedef OfrTariffSet_A     OfrTariffSet;        // used for switching implementations
  typedef QanObligToSupply_A QanObligToSupply;    // used for switching implementations
  typedef QanTechCapacity_A  QanTechCapacity;     // used for switching implementations

  // CREATORS

public:

  explicit
  GateStatedTariffEFac
  (const std::string entityId,
   Record&           record,
   const int         commitmentModeSum = xeona::e_commitmentModes);

  virtual
  ~GateStatedTariffEFac();

  // ACCESSORS

  // the inherited versions are fine

  // MANIPULATORS

public:

  // the inherited versions are fine on the sell-side, but need
  // overwriting on the buy-side

  // from class 'Gateway'

  virtual void      initializeBuySide(const int step, shared_ptr<svif::SolverIf> solver);
  virtual const int constrainBuySide(const xeona::DomainMode capacityMode);
  virtual void      washupBuySide();

  // INSTANCE DATA

private:

  // tied quantities

  const double&                       d_ghgUnitPenalty;
  const double&                       d_noxUnitPenalty;
  const double&                       d_depUnitPenalty;
  const double&                       d_lucUnitPenalty;

  shared_ptr<std::vector<double> >    d_totalGhgPenaltys;

  // internal quantities

  CostSet                             d_dutySpecCosts; // penalties not consolidated costs

}; // class 'GateStatedTariffEFac'

//  ==== XEDOC =================================================
//
//  entity.gate-stated-tariff-efac-*-0
//
//        base commodity in {Cert,Cseq,Elec,Fund,Heat,Oxid,Thrm,Work}
//        or derived in {OxidBiocoal,OxidGas,OxidHydrogen,OxidNaturalGas}
//        quantifying extensity * in {J,kg,$}
//
//      class                                    > GateStatedTariffEFac:Work
//
//        gateway entity which uses stated (rather than
//        stochastic, unilaterally set, or negotiated) tariffs
//        and which supports an average emissions factor
//
//        time-of-use (TOU) tariffs can be explicitly given in
//        the tariffsets timeseries
//
//        the 'Cm*' qualifier (here * = Work) specifies the
//        relevant high-level commodity class
//
//        my socket label is 'sock-1'
//
//      builtin-remark s                         <
//
//      socket l                                 > "teas-1.work-1"
//      common-commodity l                       > "cm-work-0"
//
//        the above fields define my supplier and their socket
//        label and our common commodity
//
//      tariffsets X                             > "1.6e-06 * 40.00e+06 28.00e-09" ..
//
//        the optional leading term (1.6e-06) in the tariffsets
//        field represents a fixed charge [$/s] -- the remaining
//        star-separated pairs being not necessarily convex
//        quantity/price (*, $) offers
//
//      ghg-unit-penalty [kg/*] f                > 140.0e-09
//      nox-unit-penalty [kg/*] f                > 0.0
//      dep-unit-penalty [J/*] f                 > 0.0
//      luc-unit-penalty [m2/*] f                > 0.0
//
//        the ghg-unit-penalty is used to add a non-consolidating
//        CO2e penalty to the buy-side stream -- thereby allowing
//        electricity supply to include an average emissions factor
//
//      defined-capacitys [*/s] F                > 50.00e+06 ..
//
//        data capture follows
//
//      quantitys [*/s] F                        < 0.0 ..
//      marginal-prices [$/*] F                  < 0.0 ..
//      total-costs [$] F                        < 0.0 ..
//      total-ghg-penaltys [kg] F                < 0.0 ..
//
//        the total-ghg-penaltys are based on ghg-unit-penalty
//
//  ============================================================

// the 140e-09 kg/J value is based on the 2009 German-wide
// average emissions factor of 506 g/kWh (note grams)

#endif // _GATE01_H_

//  end of file

