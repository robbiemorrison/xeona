//  XEONA ENERGY SYSTEMS MODELING ENVIRONMENT
//
//  file-create-name : teas07.h
//  file-create-date : Thu 20-Jan-2011 16:07 UTC
//  file-initiator   : Robbie Morrison <robbie@actrix.co.nz>
//
//  file-role        : concrete technical assets 7 / header
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
//  $URL: file:///home/robbie/svn-root/xeona/futz/trunk/xeona1/b/teas07.h $
//
//  GENERAL NOTES FOR THIS FILE

//  HEADER GUARD

#ifndef _TEAS07_H_
#define _TEAS07_H_

//  LOCAL AND SYSTEM INCLUDES

#include "../b/teas.h"        // technical asset entity
#include "../b/costreg.h"     // cost registers

#include "../a/logger_fwd.h"  // some forward declarations from "logger.h"
#include "../c/smart_ptr.h"   // toggle between Boost and TR1 smart pointers

#include <string>             // C++ strings

//  FORWARD (PARTIAL) DECLARATIONS

class Record;                                // constructor argument
namespace svif { class SolverIf; }           // member function argument

template <typename C> class Cable;           // 'C' is of base class 'Commodity' etc
template <typename C> class Socket;

class CmElectricity;

class CxInflow;                              // reservoir inflow context
class HydroStatus;                           // hydro asset to operator data transfer

template class Statistics<double>;           // data member

//  CODE

// ---------------------------------------------------------
//  CLASS           : TeasHydroScheme
// ---------------------------------------------------------
//  Description  : hydro-electic scheme
//  Role         : concrete entity
//  Techniques   : 'OpsFac0Out1' source-style OSP
//  Status       : complete
// ---------------------------------------------------------

class OpsFac0Out1_A;                         // necessary for OSP typedef below

class TeasHydroScheme :
  public TechnicalAsset,
  public CostRegisterSRFin,
  public CostRegisterEmbFin
{
  // USING DECLARATIONS

protected:

  using Entity::s_logger;                    // place in common scope for this class

  // TYPEDEFS

private:

  typedef OpsFac0Out1_A OpsFac0Out1;         // used for switching implementations

  // DISABLED

private:

  TeasHydroScheme();                                         // zero-argument constructor
  TeasHydroScheme(const TeasHydroScheme& orig);              // copy constructor
  TeasHydroScheme& operator= (const TeasHydroScheme& orig);  // copy assignment operator

  // CREATORS

public:

  explicit
  TeasHydroScheme
  (const std::string entityId,
   Record&           record);

  virtual
  ~TeasHydroScheme();

  // CALLS

public:

  virtual
  void
  establish();

  virtual
  const int                                  // duty gol
  constrain
  (const xeona::DomainMode capacityMode);

  virtual
  void
  washup();

  virtual
  void
  conclude();

  // OPERATOR DIALOG

public:

  shared_ptr<HydroStatus>                    // call before operator OSP load
  getStatus() const;                         // facilitate data download by operator

  void                                       // call after operator OSP load
  putBidSet                                  // facilitate data upload by operator
  (shared_ptr<LmpBidSet> bidset);

  // UTILITY FUNCTIONS

protected:

  double
  calcIo                                     // input/output relationship [W/m3]
  (const double staticHead) const;           // adjusted for lake level

  double
  calcCapacityFactor                         // capacity factor [-] on range [0,1]
  (const double staticHead) const;           // adjusted for lake level

  // INSTANCE DATA

private:

  // tied quantities

  assign_ptr<CxInflow>                     d_inflowContext;

  const double&                            d_generatorCapacity;  // nominal capacity
  const double&                            d_storageVolume;
  const double&                            d_operatingDepth;
  const double&                            d_staticHead;
  const double&                            d_primaryEfficiency;  // nominal efficiency
  const double&                            d_openingStorage;

  shared_ptr<std::vector<double> >         d_potentialProductions;
  shared_ptr<std::vector<double> >         d_actualProductions;
  shared_ptr<std::vector<double> >         d_reservoirStorages;
  double&                                  d_closingInventory;
  double&                                  d_inventoryDelta;
  double&                                  d_meanInflow;
  double&                                  d_capacityFactor;
  double&                                  d_spillFactor;
  int&                                     d_spillCount;

  shared_ptr<std::vector<std::string> >    d_submittedBidsets;

  shared_ptr<Socket<CmElectricity> >       d_outElec;

  // local quantities

  double                                   d_io;                 // water to electricity
  double                                   d_capacity;           // current capacity
  double                                   d_openingInventory;
  double                                   d_availableInventory;
  double                                   d_inflowVol;
  double                                   d_inflowHistoricalVol;
  double                                   d_priorInventory;
  double                                   d_priorInflowVol;
  double                                   d_priorTakeVol;
  double                                   d_priorSpillVol;
  double                                   d_priorDispatch;      // zero first time
  double                                   d_spillTally;         // quantified in lost [W]
  Statistics<double>                       d_statInflows;        // on-the-fly statistics

  shared_ptr<std::vector<double> >         d_monthlyInflows;

  shared_ptr<OpsFac0Out1>                  d_ops;      // specialization required

  // STATIC DATA

  static const double                      s_waterMassDensity;   // [kg/m3]

};

//  ==== XEDOC =================================================
//
//  entity.teas-hydro-scheme-0
//
//      class                                    > TeasHydroScheme
//
//        a hydroelectric scheme with storage that receives
//        inflows and which can spill water -- note that the
//        opening and closing inventories are not required to
//        match
//
//        this entity is designed to be paired with
//        AsopLmpBidHydro1 when part of a nodal market
//
//        a scheme may represent more than one hydro station if
//        these form a chain and are managed collectively
//
//        my socket label is 'elec-1'
//
//      builtin-remark s                         <
//
//      socket-electricity-commodity l           > "cm-electricity-0"
//
//        socket-electricity-commodity defines the shared
//        electricity commodity
//
//      inflow-context l                         > "cx-inflow-0"
//
//        the context entity must be a sub-class of CxInflow
//
//      generator-capacity [W] f                 > 540e+06
//      storage-volume [m3] f                    > 74e+06
//      operating-depth [m] f                    > 0.80
//      static-head [m] f                        > 92
//      primary-efficiency [-] f                 > 0.82
//      opening-storage [-] f                    > 0.90
//
//        the above parameters are, where appropriate, nominal,
//        the storage-volume can be set to near zero to yield
//        run-of-river operation
//
//      potential-productions [W] F              < 0.0 ..
//      actual-productions [W] F                 < 0.0 ..
//      reservoir-storages [-] F                 < 0.0 ..
//      closing-inventory [m3] f                 < 0.0
//      inventory-delta [-] f                    < 0.0
//      mean-inflow [m3/s] f                     < 0.0
//      capacity-factor [-] f                    < 0.0
//      spill-factor [-] f                       < 0.0
//      spill-count [-] i                        < 0
//
//        reservoir-storages [0-1] are relative, inventory-delta
//        [0,1] is the relative change, capacity-factor [0,1] is
//        the ratio of actual output to generator capacity,
//        spill-factor is the ratio of discarded to potential
//        output
//
//      submitted-bidsets [W,$/J] X              < "0.0 0.0" ..
//
//        submitted-bidsets are provided by the operator, but
//        stored and reported from here
//
//      nameplate-capacity [W] f                 > 5000e+03
//      duty-specific-cost-financial [$/J] f     > 100.0
//      size-specific-cost-financial [$/W/s] f   > 200.0
//      standing-cost-financial [$/s] f          > 300.0
//
//      variable-costs-financial [$] F           < 0.0 ..
//      fixed-costs-financial [$] F              < 0.0 ..
//
//      annual-discount-rate-decimal [-] f       > 0.10
//      economic-life [y] i                      > 25
//      capex-initial [$] f                      > +120e+03
//      capex-terminal [$] f                     > -10e+03
//      current-age [y] i                        > 2
//
//        a negative capex-terminal indicates salvage income as
//        opposed to decommissioning cost
//
//      embedded-costs-financial [$] F           < 0.0 ..
//
//  ============================================================

#endif // _TEAS07_H_

//  end of file

