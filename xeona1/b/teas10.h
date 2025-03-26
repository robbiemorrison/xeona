//  XEONA ENERGY SYSTEMS MODELING ENVIRONMENT
//
//  file-create-name : teas10.h
//  file-create-date : Mon 04-Apr-2011 10:19 UTC
//  file-initiator   : Robbie Morrison <robbie@actrix.co.nz>
//
//  file-role        : concrete technical assets 10 / header
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
//  $URL: file:///home/robbie/svn-root/xeona/futz/trunk/xeona1/b/teas10.h $
//
//  GENERAL NOTES FOR THIS FILE
//
//  This unit contains dedicated storage entities.

//  HEADER GUARD

#ifndef _TEAS10_H_
#define _TEAS10_H_

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
//  CLASS           : TeasSimpleStorage <>
// ---------------------------------------------------------
//  Description  : simple storage
//  Role         : concrete entity
//  Techniques   : (nothing special)
//  Status       : complete
//
//  Design notes
//
//      This class is a simple storage entity -- meaning that
//      there is no complex use-of-storage policy in place.
//
//      Any forced domain surplus is either stored or,
//      alternatively, spilled iff full AND the spill flag is
//      set.  Conversely supply is offered at some predefined
//      (possibly zero) unit price -- which, of course, can only
//      bind under "fin" policy.  A binary variable prevents the
//      recharge and discharge modes from occurring within the
//      same period.
//
//      The modeler will need to confirm that spilling makes
//      sense for their particular application and set the spill
//      flag accordingly.
//
// ---------------------------------------------------------

class OpsStore_A;                            // necessary for OSP typedef below

template <typename C>                        // 'C' for commodity
class TeasSimpleStorage :
  public TechnicalAsset,
  public CostRegisterSRFin,
  public CostRegisterEmbFin
{
  // USING DECLARATIONS

protected:

  using Entity::s_logger;                    // place in common scope for this class

  // TYPEDEFS

private:

  typedef OpsStore_A OpsStore;               // used for switching implementations

  // DISABLED

private:

  TeasSimpleStorage();                                           // zero-argument ctor
  TeasSimpleStorage(const TeasSimpleStorage& orig);              // copy constructor
  TeasSimpleStorage& operator= (const TeasSimpleStorage& orig);  // copy assignment oper

  // CREATORS

public:

  explicit
  TeasSimpleStorage
  (const std::string entityId,
   Record&           record);

  virtual
  ~TeasSimpleStorage();

  // CALLS

public:

  virtual
  void
  establish();

  virtual
  const int                                  // discharge gol
  constrain
  (const xeona::DomainMode capacityMode);

  virtual
  void
  washup();

  virtual
  void
  conclude();

  // INSTANCE DATA

private:

  // tied quantities

  const std::string&                  d_commodity;     // for the 'create' interface calls
  const double&                       d_roundTripEffy;
  const double&                       d_halfLife;
  const double&                       d_storageCapacity;
  const double&                       d_openingStorage;
  const double&                       d_rechargeRate;
  const double&                       d_dischargeRate;
  const bool&                         d_spillFlag;
  const double&                       d_dischargeUnitCost;

  shared_ptr<std::vector<int> >       d_modes;
  shared_ptr<std::vector<double> >    d_charges;
  shared_ptr<std::vector<double> >    d_closingInventorys;

  const shared_ptr<Cable<C> >         d_inCm;
  const shared_ptr<Socket<C> >        d_outCm;

  // local quantities

  double                              d_openingInventory;

  shared_ptr<OpsStore>                d_ops; // specialization required

  CostSet                             d_dischargeSpecCosts;

};

//  ==== XEDOC =================================================
//
//  entity.teas-simple-storage-*-0
//
//        base commodity in {Cert,Cseq,Elec,Fund,Heat,Oxid,Thrm,Work}
//        or derived in {OxidBiocoal,OxidGas,OxidHydrogen,OxidNaturalGas}
//
//      class                                    > TeasSimpleStorage:Elec
//
//        non-proactive storage asset which either recharges
//        without cost or discharges under some predefined
//        (perhaps zero) unit cost penalty and which can 'spill'
//        iff full and this feature is enabled
//
//        the associated sourcing entities may need to be set
//        their internal-dump-flag's to zero to drive recharging
//
//        if pass thru behavior is required then combine this
//        entity with a JuncDemand2Sym junction
//
//        my socket label is 'sock-1'
//
//      builtin-remark s                         <
//
//      socket l                                 > "teas-supply-0.sock-1"
//
//        socket is my supplier (often an electricity busbar)
//
//      stored-commodity l                       > "cm-electricity-0"
//
//        stored-commodity defines the underlying commodity
//
//      round-trip-effy [-] f                    > 0.70
//      half-life [s] f                          > 604800
//
//        the round-trip-effy accounts for instantaneous storage
//        losses and the half-life accounts for inventory decay,
//        where zero codes for no decay
//
//      capacity [*] f                           > 8.0e+03
//      opening-storage [-] f                    > 0.90
//      recharge-rate [*/s] f                    > -1.0
//      discharge-rate [*/s] f                   > -1.0
//
//        the capacity and opening-storage on [0,1] are
//        self-explanatory, the two rates account for flow rate
//        restrictions, whereby -1.0 means omit
//
//      spill-flag b                             > 1
//      discharge-unit-cost [$/*] f              > 0.0
//
//        a spill-flag set to true (1) indicates spilling is
//        supported, the discharge-unit-cost (often zero) adds to
//        the financial cost for discharge but is not
//        consolidated into the final accounts
//
//      modes [-] i                              < 0 ..
//      charges [*/s] F                          < 0 ..
//      closing-inventorys [*] F                 < 0.0 ..
//
//        the modes {-1,0,+1} indicates discharge, no-change, or
//        recharge and the charges and closing-inventorys are
//        self-explanatory -- except positive charge may supply
//        spill, if allowed, and not inventory
//
//      nameplate-capacity [*/s] f               > 8.0e+03
//      duty-specific-cost-financial [$/*] f     > 100.0
//      size-specific-cost-financial [$/*/s] f   > 200.0
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

#endif // _TEAS10_H_

//  end of file

