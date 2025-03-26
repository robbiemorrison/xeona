//  XEONA ENERGY SYSTEMS MODELING ENVIRONMENT
//
//  file-create-name : teas02.h
//  file-create-date : Wed 22-Apr-2009 12:40 UTC
//  file-initiator   : Robbie Morrison <robbie@actrix.co.nz>
//
//  file-role        : concrete technical assets 2 / header
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
//  $URL: file:///home/robbie/svn-root/xeona/futz/trunk/xeona1/b/teas02.h $
//
//  GENERAL NOTES FOR THIS FILE
//
//  This unit contains CCGT assets and similar.
//
//  More specifically, this unit contains a generic oxidizable
//  fuel to electricity asset and unabated and abated
//  combined-cycle gas turbine (CCGT) assets.
//
//  All assets support shutdown mode operation, some also support
//  ramp-rate restrictions and ancillary (shutdown mode) fuel
//  usage as well as technology-specific specializations.

//  HEADER GUARD

#ifndef _TEAS02_H_
#define _TEAS02_H_

//  LOCAL AND SYSTEM INCLUDES

#include "../b/teas.h"        // technical asset entity
#include "../b/costreg.h"     // cost registers

#include "../a/logger_fwd.h"  // some forward declarations from "logger.h"
#include "../c/smart_ptr.h"   // toggle between Boost and TR1 smart pointers

#include <string>             // C++ strings

#include <boost/tuple/tuple.hpp>             // n-tuples, ref(), cref()

//  FORWARD (PARTIAL) DECLARATIONS

class Record;                                // constructor argument
namespace svif { class SolverIf; }           // member function argument

template <typename C> class Cable;           // 'C' is of base class 'Commodity' etc
template <typename C> class Socket;

class CmElectricity;                         // 'CmElectricity' is from 'Commodity' etc
class CmOxidize;
class CmOxidGas;
class CmCarbonSeq;

class CxAmbientAir;

//  CODE

// ---------------------------------------------------------
//  CLASS           : TeasOxidToElec
// ---------------------------------------------------------
//  Description  : particularized two-port conversion asset
//  Role         : concrete entity
//  Techniques   : 'OpsFac1Out1'
//  Status       : complete
// ---------------------------------------------------------

class OpsFac1Out1_A;                         // necessary for OSP typedef below

class TeasOxidToElec :
  public TechnicalAsset,
  public CostRegisterSRFin,
  public CostRegisterEmbFin
{
  // USING DECLARATIONS

protected:

  using Entity::s_logger;                    // place in common scope for this class

  // TYPEDEFS

private:

  typedef OpsFac1Out1_A OpsFac1Out1;         // used for switching implementations

  // DISABLED

private:

  TeasOxidToElec();                                         // zero-argument constructor
  TeasOxidToElec(const TeasOxidToElec& orig);               // copy constructor
  TeasOxidToElec& operator= (const TeasOxidToElec& orig);   // copy assignment operator

  // CREATORS

public:

  explicit
  TeasOxidToElec
  (const std::string entityId,
   Record&           record);

  virtual
  ~TeasOxidToElec();

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

  virtual void conclude()  { }               // necessary but hollow redefinition

  // INSTANCE DATA

private:

  // tied quantities

  const double&                        d_prodLoBound;
  const double&                        d_prodHiBound;
  const double&                        d_marginalEfficiency;
  const double&                        d_fuelNoload;
  const double&                        d_fuelAncillary;
  const double&                        d_rampRestraintDown;
  const double&                        d_rampRestraintUp;

  shared_ptr<std::vector<double> >     d_productions;
  shared_ptr<std::vector<bool> >       d_shutdownStatuss;

  shared_ptr<Cable<CmOxidize> >        d_inOxid;
  shared_ptr<Socket<CmElectricity> >   d_outElec;

  // local quantities

  shared_ptr<CmOxidize>                d_oxid;
  shared_ptr<OpsFac1Out1>              d_ops;     // specialization required

}; // class 'TeasOxidToElec'

//  ==== XEDOC =================================================
//
//  entity.teas-oxid-to-elec-0
//
//      class                                    > TeasOxidToElec
//
//        a simple fuel to power (thermal, fuel cell, or
//        otherwise) power plant, with support for shutdown mode
//        operation and ramp rate restrictions -- but ancillary
//        electricity demand is not included
//
//        my socket label is 'elec-1'
//
//      builtin-remark s                         <
//
//      socket-oxidize l                         > "teas-supplier-0.oxid-1"
//      cable-oxidize-commodity l                > "cm-oxidize-0"
//
//        the socket-oxidize and cable-oxidize-commodity define
//        my supplier and their socket label and our common
//        oxidizable fuel commodity
//
//      socket-electricity-commodity l           > "cm-electricity-0"
//
//        the socket-electricity-commodity defines the common
//        electricity commodity
//
//      prod-lo-bound [W] f                      > 60.0
//      prod-hi-bound [W] f                      > 90.0
//      marginal-efficiency [-] f                > 0.5
//      fuel-noload [W] f                        > 10.0
//      fuel-ancillary [W] f                     > 5.0
//
//        the prod-hi-bound and prod-lo-bound set the production
//        capacity and the shutdown mode threshold respectively,
//        the marginal-efficiency and fuel-noload determine the
//        operating curve, and the fuel-ancillary sets the
//        shutdown fuel usage, the last two converted by
//        combustion enthalpy
//
//      ramp-restraint-down [-] f                > 0.5
//      ramp-restraint-up [-] f                  > 0.5
//
//        ramp-restraint-down and ramp-restraint-up restrict
//        output steps relative to nameplate-capacity -- use
//        values of prod-hi-bound/nameplate-capacity or more to
//        prevent binding
//
//      productions [W] F                        < 0.0 ..
//      shutdown-statuss [-] B                   < 0 ..
//
//        productions represent the actual output figures,
//        shutdown-statuss are true (1) if the plant ran
//
//      nameplate-capacity [W] f                 > 80.0
//      duty-specific-cost-financial [$/J] f     > 100.0
//      size-specific-cost-financial [$/W/s] f   > 200.0
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
//  CLASS           : TeasCcgt
// ---------------------------------------------------------
//  Description  : industrial-scale CCGT installation
//  Role         : concrete entity
//  Techniques   : 'OpsFac1Out1'
//  Status       : complete
// ---------------------------------------------------------

class OpsFac1Out1_A;                         // necessary for OSP typedef below

class TeasCcgt :
  public TechnicalAsset,
  public CostRegisterSRFin,
  public CostRegisterEmbFin
{
  // USING DECLARATIONS

protected:

  using Entity::s_logger;                    // place in common scope for this class

  // TYPEDEFS

private:

  typedef OpsFac1Out1_A OpsFac1Out1;         // used for switching implementations

  // DISABLED

private:

  TeasCcgt();                                     // zero-argument constructor
  TeasCcgt(const TeasCcgt& orig);                 // copy constructor
  TeasCcgt& operator= (const TeasCcgt& orig);     // copy assignment operator

  // CREATORS

public:

  explicit
  TeasCcgt
  (const std::string entityId,
   Record&           record);

  virtual
  ~TeasCcgt();

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

  virtual void conclude()  { }               // necessary but hollow redefinition

protected:

  boost::tuple
  <int,                                      // duty gol
   int>                                      // fuel gol
  constrainCore
  (const xeona::DomainMode capacityMode);

  // UTILITY FUNCTIONS

private:

  boost::tuple                               // note: fuel = a * product + b
  <double,                                   // coefficient 'a'
   double,                                   // coefficient 'b'
   double,                                   // temperature-adjusted cutout capacity
   double>                                   // temperature-adjusted on-design capacity
  characterize
  (const double airTemp);                    // current ambient air temperature

  // INSTANCE DATA

  // CAUTION: the fine-grain control over private and protected
  // shows which variables are accessed by subclasses

  // tied quantities

private:

  assign_ptr<CxAmbientAir>             d_ambientAirContext;

protected:

  const double&                        d_onDesignCapacity;
  const double&                        d_onDesignEfficiency;
  const double&                        d_cutoutCapacityFactor;
  const double&                        d_cutoutEfficiencyFactor;
  const double&                        d_fuelAncillary;

protected:

  shared_ptr<std::vector<double> >     d_productions;
  shared_ptr<std::vector<double> >     d_fuelDemands;
  shared_ptr<std::vector<double> >     d_capacitys;
  shared_ptr<std::vector<bool> >       d_shutdownStatuss;
  shared_ptr<std::vector<double> >     d_varCostsGhg;
  shared_ptr<std::vector<double> >     d_carbonEmissions;

protected:

  shared_ptr<Cable<CmOxidGas> >        d_inOxid;
  shared_ptr<Socket<CmElectricity> >   d_outElec;

  // local quantities

private:

  CostSet                              d_oxidSpecCosts;

protected:

  shared_ptr<CmOxidGas>                d_oxid;
  shared_ptr<OpsFac1Out1>              d_ops;     // specialization required

  // quantities that can be modified by CCGT specializations --
  // such as 'TeasCcgtCapture'

protected:

  double                               d_onDesignCapacity_M;
  double                               d_onDesignEfficiency_M;

}; // class 'TeasCcgt'

//  ==== XEDOC =================================================
//
//  entity.teas-ccgt-0
//
//        takes {OxidGas}
//
//      class                                    > TeasCcgt
//
//        industrial-scale CCGT installation featuring low-duty
//        cutout and the influence of ambient air temperature on
//        efficiency and capacity
//
//        the modeler is responsible for ensuring that the
//        parametrizations used here match the fuel type and
//        whether the fuel is characterized by LHV or HHV
//
//        the HHV of natural gas is about 54.0e+06 J/kg
//
//        the values provided cover the Alstom GT26 CCGT
//        plant installed in Taranaki, New Zealand in 1998
//        and presume HHV
//
//        my socket label is 'elec-1'
//
//      builtin-remark s                         <
//
//      socket-oxidize l                         > "teas-supplier-0.oxid-1"
//      cable-oxidize-commodity l                > "cm-oxidize-0"
//
//        the socket-oxidize and cable-oxidize-commodity define
//        my supplier and their socket label and our common
//        oxidizable fuel commodity
//
//      socket-electricity-commodity l           > "cm-electricity-0"
//
//        the socket-electricity-commodity defines the common
//        electricity commodity
//
//      on-design-capacity [W] f                 > 360e+06
//      on-design-efficiency [-] f               > 0.52
//      cutout-capacity-factor [-] f             > 0.40
//      cutout-efficiency-factor [-] f           > 0.80
//      fuel-ancillary [W] f                     > 36e+03
//
//        the on-design-capacity and on-design-efficiency [0,1]
//        are at 15C and 100kPa and based on net production, the
//        cutout-capacity-factor [0,1] is relative to the
//        adjusted capacity, the cutout-efficiency-factor [0,1]
//        is relative to the adjusted efficiency, and the
//        fuel-ancillary defines the shutdown fuel usage
//        converted by combustion enthalpy
//
//      ambient-air-context l                    > "cx-ambient-air-0"
//
//        the ambient-air-context provides temperature data,
//        no adjustment is made for pressure or humidity
//
//      productions [W] F                        < 0.0 ..
//      fuel-demands [kg/s] F                    < 0.0 ..
//      carbon-emissions [kg/s] F                < 0.0 ..
//      capacitys [W] F                          < 0.0 ..
//      shutdown-statuss [-] B                   < 0 ..
//
//        productions represent the actual output figures,
//        capacitys are the temperature-adjusted potentials,
//        carbon-emissions are CO2e, and shutdown-statuss are
//        true (1) if the plant ran
//
//      nameplate-capacity [W] f                 > 360e06
//      duty-specific-cost-financial [$/J] f     > 1.20e-09
//      size-specific-cost-financial [$/W/s] f   > 1.22e-09
//      standing-cost-financial [$/s] f          > 0.0
//
//      variable-costs-financial [$] F           < 0.0 ..
//      fixed-costs-financial [$] F              < 0.0 ..
//      variable-costs-greenhouse [kg] F         < 0.0 ..
//
//      annual-discount-rate-decimal [-] f       > 0.10
//      economic-life [y] i                      > 25
//      capex-initial [$] f                      > 605e+06
//      capex-terminal [$] f                     > 0.0
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
//  CLASS           : TeasCcgtCapture
// ---------------------------------------------------------
//  Description  : industrial-scale CCGT installation with carbon capture
//  Role         : concrete entity
//  Techniques   : (nothing special)
//  Status       : complete
//
//  Design notes
//
//      This class inherits from the basic 'TeasCcgt' model.  And
//      overwrites the following protected duplicated variables:
//
//          d_onDesignCapacity_X
//          d_onDesignEfficiency_X
//
//       Using the following:
//
//          d_efficiencyHit
//          d_capacityHit
//
//      In 'xeona', unfortunately, coding up logical collections
//      of entities is really not feasible -- hence the use of
//      inheritance instead.
//
// ---------------------------------------------------------

class OpsFuelToCseq_A;                       // necessary for OSP typedef below

class TeasCcgtCapture :
  public TeasCcgt
{
  // USING DECLARATIONS

protected:

  using Entity::s_logger;                    // place in common scope for this class

  // TYPEDEFS

private:

  typedef OpsFuelToCseq_A OpsFuelToCseq;     // used for switching implementations

  // DISABLED

private:

  TeasCcgtCapture();                                         // zero-argument constructor
  TeasCcgtCapture(const TeasCcgtCapture& orig);              // copy constructor
  TeasCcgtCapture& operator= (const TeasCcgtCapture& orig);  // copy assignment operator

  // CREATORS

public:

  explicit
  TeasCcgtCapture
  (const std::string entityId,
   Record&           record);

  virtual
  ~TeasCcgtCapture();

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

  virtual void conclude()  { }               // necessary but hollow redefinition

  // INSTANCE DATA

private:

  // tied quantities

  const double&                        d_carbonCaptureRate;
  const double&                        d_efficiencyHit;
  const double&                        d_capacityHit;

  shared_ptr<std::vector<double > >    d_carbonCaptures;

  shared_ptr<Cable<CmCarbonSeq> >      d_inCseq;

  // local quantities

  const double                         d_cseqLoBound;
  const double                         d_cseqHiBound;

  shared_ptr<CmCarbonSeq>              d_cseq;
  shared_ptr<OpsFuelToCseq>            d_ops;

}; // class 'TeasCcgtCapture'

//  ==== XEDOC =================================================
//
//  entity.teas-ccgt-capture-0
//
//        takes {OxidGas}
//
//      class                                    > TeasCcgtCapture
//
//        industrial-scale CCGT installation with carbon capture,
//        featuring low-duty cutout and the influence of ambient
//        air temperature on efficiency and capacity
//
//        this asset builds on the CCGT class by downgrading the
//        efficiency and capacity using fixed multipliers
//
//        the modeler is responsible for ensuring that the
//        parametrizations used here match the fuel type and
//        whether the fuel is characterized by LHV or HHV
//
//        the HHV of natural gas is about 54.0e+06 J/kg and
//        combustion produces 2.74x carbon dioxide by mass
//
//        my socket label is 'elec-1'
//
//      builtin-remark s                         <
//
//      socket-oxidize l                         > "teas-supplier-0.oxid-1"
//      socket-carbon-seq l                      > "teas-supplier-1.cseq-1"
//      cable-oxidize-commodity l                > "cm-oxidize-0"
//      cable-carbon-seq-commodity l             > "cm-carbon-seq-0"
//
//        the socket-oxidize and cable-oxidize-commodity define
//        my supplier and their socket label and our common
//        oxidizable fuel commodity, similarly for
//        socket-carbon-seq and cable-carbon-seq-commodity
//
//      socket-electricity-commodity l           > "cm-electricity-0"
//
//        the socket-electricity-commodity defines the common
//        electricity commodity
//
//      on-design-capacity [W] f                 > 360e+06
//      on-design-efficiency [-] f               > 0.52
//      cutout-capacity-factor [-] f             > 0.40
//      cutout-efficiency-factor [-] f           > 0.80
//      fuel-ancillary [W] f                     > 36e+03
//
//        the on-design-capacity and on-design-efficiency are
//        at 15C and 100kPa and based on net production, the
//        cutout-capacity-factor [0,1] is relative to the
//        adjusted capacity, the cutout-efficiency-factor [0,1]
//        is relative to the adjusted efficiency, and the
//        fuel-ancillary defines the shutdown fuel usage
//        converted by combustion enthalpy
//
//      carbon-capture-rate [-] f                > 0.90
//      efficiency-hit [-] f                     > 0.40
//      capacity-hit [-] f                       > 0.20
//
//        the carbon-capture-rate [0,1] is self-explanatory and
//        the efficiency-hit [0,1] and capacity-hit [0,1] are the
//        increase in fuel load and decrease in electrical output
//        which results from the carbon capture subsystem (of
//        unspecified technology) -- plant cost increases should
//        be factored in directly (see below)
//
//      ambient-air-context l                    > "cx-ambient-air-0"
//
//        the ambient-air-context provides temperature data,
//        no adjustment is made for pressure or humidity
//
//      productions [W] F                        < 0.0 ..
//      fuel-demands [kg/s] F                    < 0.0 ..
//      carbon-emissions [kg/s] F                < 0.0 ..
//      carbon-captures [kg/s] F                 < 0.0 ..
//      capacitys [W] F                          < 0.0 ..
//      shutdown-statuss [-] B                   < 0 ..
//
//        productions represent the actual output figures,
//        carbon-emissions are CO2e, carbon-captures represent
//        the carbon dioxide sent for sequestration, capacitys
//        are the temperature-adjusted potentials, and
//        shutdown-statuss are true (1) if the plant ran
//
//      nameplate-capacity [W] f                 > 360e06
//      duty-specific-cost-financial [$/J] f     > 1.20e-09
//      size-specific-cost-financial [$/W/s] f   > 1.22e-09
//      standing-cost-financial [$/s] f          > 0.0
//
//      variable-costs-financial [$] F           < 0.0 ..
//      fixed-costs-financial [$] F              < 0.0 ..
//      variable-costs-greenhouse [kg] F         < 0.0 ..
//
//      annual-discount-rate-decimal [-] f       > 0.10
//      economic-life [y] i                      > 25
//      capex-initial [$] f                      > 605.0e+06
//      capex-terminal [$] f                     > 0.0
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

#endif // _TEAS02_H_

//  end of file

