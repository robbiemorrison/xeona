//  XEONA ENERGY SYSTEMS MODELING ENVIRONMENT
//
//  file-create-name : teas1.h
//  file-create-date : Wed 15-Apr-2009 21:02 UTC
//  file-initiator   : Robbie Morrison <robbie@actrix.co.nz>
//
//  file-role        : concrete technical assets 1 / header
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
//  $URL: file:///home/robbie/svn-root/xeona/futz/trunk/xeona1/b/teas01.h $
//
//  GENERAL NOTES FOR THIS FILE

//  HEADER GUARD

#ifndef _TEAS1_H_
#define _TEAS1_H_

//  LOCAL AND SYSTEM INCLUDES

#include "../b/teas.h"        // technical asset entity
#include "../b/costreg.h"     // cost registers

#include "../a/logger_fwd.h"  // some forward declarations from "logger.h"
#include "../c/smart_ptr.h"   // toggle between Boost and TR1 smart pointers

#include <string>             // C++ strings
#include <sstream>            // string-streams

//  FORWARD (PARTIAL) DECLARATIONS

class Record;                                // constructor argument
namespace svif { class SolverIf; }           // member function argument

template <typename C> class Cable;           // 'C' is of base class 'Commodity' etc
template <typename C> class Socket;

class CmElectricity;

//  CODE

// ---------------------------------------------------------
//  CLASS           : TeasAcTransmission
// ---------------------------------------------------------
//  Description  : bidirectional high-voltage AC transmission
//  Role         : primarily for use in LMP wholesale electricity markets
//  Techniques   : enhanced DC power flow, linear discretization for quadratic losses
//  Status       : complete
//
//  Design notes
//
//      Note that the "enhanced DC power flow model" is for AC
//      transmission.
//
//      The default flow orientation is specified by the
//      direction from my socket to my cable.
//
//      This asset allows reverse or negative flow, unlike most
//      technical assets.
//
//  Typical line characterization
//
//      Purchala etal (2005 table 1) give typical
//      distance-specific impedance values (resistance 'r' and
//      reactance 'x') for the Belgium national grid at 380, 220,
//      150, and 70kV.
//
//  References
//
//      Purchala, Konrad Leonardo Meeus, Daniel Van Dommelen, and
//        Ronnie Belmans.  2005.  Usefulness of DC power flow for
//        active power flow analysis.  IEEE Power Engineering
//        Society General Meeting, 2005, v1 p454-459.
//
// ---------------------------------------------------------

class OpsLoadFlow_A;                         // necessary for OSP typedef below
class OpsLoadFlow_B;                         // necessary for OSP typedef below

class TeasAcTransmission :
  public TechnicalAsset,
  public CostRegisterSRFin,
  public CostRegisterEmbFin
{
  // USING DECLARATIONS

protected:

  using Entity::s_logger;                    // place in common scope for this class

  // TYPEDEFS

private:

  // CAUTION: note this simple way of swapping OSP
  // implementations, the "_X" postfix needs changing in this one
  // place only -- note also that an implementation change is
  // required to honor the same function names and signatures,
  // but not necessarily the behavior at large

  typedef OpsLoadFlow_A OpsLoadFlow;         // used for switching implementations

  // DISABLED

private:

  TeasAcTransmission();                                            // zero-argument ctor
  TeasAcTransmission(const TeasAcTransmission& orig);              // copy constructor
  TeasAcTransmission& operator= (const TeasAcTransmission& orig);  // copy assignment oper

  // CREATORS

public:

  explicit
  TeasAcTransmission
  (const std::string entityId,
   Record&           record);

  virtual
  ~TeasAcTransmission();

  // CALLS

public:

  virtual
  void
  establish();

  virtual
  const int                                  // returns zero (no duty coupling)
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

  const double&                         d_capacity;
  const double&                         d_resistancePerMetre;
  const double&                         d_reactancePerMetre;
  const double&                         d_length;
  const double&                         d_voltageAngleDeltaUpper;
  const int&                            d_discretizationSteps;

  const std::string&                    d_gridCommodity;    // 'create' interface calls

  shared_ptr<std::vector<bool> >        d_directions;
  shared_ptr<std::vector<double> >      d_injections;
  shared_ptr<std::vector<double> >      d_exits;
  shared_ptr<std::vector<double> >      d_relativeDutys;
  shared_ptr<std::vector<double> >      d_relativeLosss;
  shared_ptr<std::vector<double> >      d_voltageAngleDeltas;
  int&                                  d_capacitateCount;

  shared_ptr<Cable<CmElectricity> >     d_cable;
  shared_ptr<Socket<CmElectricity> >    d_socket;

  // local quantities

  double                                d_voltage;

  shared_ptr<OpsLoadFlow>               d_ops;              // specialization required

}; // class 'TeasAcTransmission'

//  ==== XEDOC =================================================
//
//  entity.teas-ac-transmission-0
//
//      class                                    > TeasAcTransmission
//
//        HV AC transmission line entity, based on enhanced DC
//        power flow (Motto etal 2002), and suitable for use
//        under nodal pricing -- the specific impedence values
//        are for 220kV (Purchala etal 2005 table 1)
//
//        my socket label is 'grid-1'
//
//      builtin-remark s                         <
//
//      socket-1 l                               > "node-0.grid-1"
//
//        socket-1 is my node entity
//
//      grid-commodity l                         > "cm-electricity-0"
//
//        grid-commodity defines the underlying commodity
//
//      capacity [W] f                           > 400e+06
//      resistance-per-metre [ohm/m] f           > 67e-06
//      reactance-per-metre [ohm/m] f            > 364e-06
//      length [m] f                             > 200e+03
//      voltage-angle-delta-upper [degrees] f    > 90.0
//      discretization-steps [-] i               > 4
//
//        self-explanatory except the quadratic loss parameter
//        voltage-angle-delta-upper on [0,90] where 90 is safe
//        but should be tuned downward after reviewing results
//        and the reactance-per-metre cannot be zero
//
//      directions B                             < 1 ..
//      injections [W] F                         < 0.0 ..
//      exits [W] F                              < 0.0 ..
//      relative-dutys [-] F                     < 0.0 ..
//      relative-losss [-] F                     < 0.0 ..
//      voltage-angle-deltas [degrees] F         < 0.0 ..
//      capacitate-count [-] i                   < 0
//
//        directions entry 1 indicates flow from my cable to my
//        socket -- while 0 indicates the opposite
//
//      nameplate-capacity [W] f                 > 400e+06
//      duty-specific-cost-financial [$/J] f     > 0.0
//      size-specific-cost-financial [$/W/s] f   > 8.0e-12
//      standing-cost-financial [$/s] f          > 0.0
//
//      variable-costs-financial [$] F           < 0.0 ..
//      fixed-costs-financial [$] F              < 0.0 ..
//
//      annual-discount-rate-decimal [-] f       > 0.08
//      economic-life [y] i                      > 30
//      capex-initial [$] f                      > 1.6e+09
//      capex-terminal [$] f                     > 0.4e+09
//      current-age [y] i                        > 10
//
//        a negative capex-terminal indicates salvage income as
//        opposed to decommissioning cost
//
//      embedded-costs-financial [$] F           < 0.0 ..
//
//        the 'embedded-costs-financial' are calculated using the
//        DCF annuity method over the economic life of the entity
//        in question
//
//  ============================================================

// ---------------------------------------------------------
//  CLASS           : TeasDcTransmission
// ---------------------------------------------------------
//  Description  : bidirectional high-voltage DC transmission
//  Role         : primarily for use in LMP wholesale electricity markets
//  Techniques   : linear discretization for quadratic losses
//  Status       : complete
//
//  Design notes
//
//      The default flow orientation is specified by the
//      direction from my socket to my cable.
//
//      This asset allows negative flow, unlike most technical
//      assets.
//
//  CAUTION: loss mechanism
//
//      Users should check the loss mechanism meets their needs.
//
//      If I recall correctly, the current systems "fills up" the
//      discretized capacities in order of increasing loss.  This
//      means that the average and marginal loss differ --
//      probably not what is required.
//
// ---------------------------------------------------------

class OpsDcTrans_A;                          // necessary for OSP typedef below

class TeasDcTransmission :
  public TechnicalAsset,
  public CostRegisterSRFin,
  public CostRegisterEmbFin
{
  // USING DECLARATIONS

protected:

  using Entity::s_logger;                    // place in common scope for this class

  // TYPEDEFS

private:

  // CAUTION: note this simple way of swapping OSP
  // implementations, the "_X" postfix needs changing in this one
  // place only -- note also that an implementation change is
  // required to honor the same function names and signatures,
  // but not necessarily the behavior at large

  typedef OpsDcTrans_A OpsTransmission;      // used for switching implementations

  // DISABLED

private:

  TeasDcTransmission();                                            // zero-argument ctor
  TeasDcTransmission(const TeasDcTransmission& orig);              // copy constructor
  TeasDcTransmission& operator= (const TeasDcTransmission& orig);  // copy assignment oper

  // CREATORS

public:

  explicit
  TeasDcTransmission
  (const std::string entityId,
   Record&           record);

  virtual
  ~TeasDcTransmission();

  // CALLS

public:

  virtual
  void
  establish();

  virtual
  const int                                  // returns zero (no duty coupling)
  constrain
  (const xeona::DomainMode capacityMode);

  virtual
  void
  washup();

  virtual void conclude()  { }               // necessary redefinition

  // INSTANCE DATA

private:

  // tied quantities

  const double&                         d_injectCapacity;
  const double&                         d_ohmsPerMetre;
  const double&                         d_length;
  const int&                            d_discretizationSteps;

  const std::string&                    d_gridCommodity;    // 'create' interface calls

  shared_ptr<std::vector<bool> >        d_directions;
  shared_ptr<std::vector<double> >      d_injections;
  shared_ptr<std::vector<double> >      d_exits;
  shared_ptr<std::vector<double> >      d_relativeDutys;
  shared_ptr<std::vector<double> >      d_relativeLosss;

  shared_ptr<Cable<CmElectricity> >     d_cable;
  shared_ptr<Socket<CmElectricity> >    d_socket;

  // local quantities

  double                                d_voltage;

  shared_ptr<OpsTransmission>           d_ops;              // specialization required

}; // class 'TeasDcTransmission'

//  ==== XEDOC =================================================
//
//  entity.teas-dc-transmission-0
//
//      class                                    > TeasDcTransmission
//
//        HV DC transmission line entity, based on I2R losses,
//        and suitable for use under nodal pricing
//
//        CAUTION: the loss mechanism may not meet your needs,
//        read the source code and comments for more information
//
//        my socket label is 'elec-1'
//
//      builtin-remark s                         <
//
//      socket-1 l                               > "node-0.grid-1"
//
//        socket-1 is my node entity
//
//      grid-commodity l                         > "cm-electricity-0"
//
//        grid-commodity defines the underlying commodity
//
//      inject-capacity [W] f                    >  400e+06
//      ohms-per-metre [ohm/m] f                 >   30e-06
//      length [m] f                             > 1000e+03
//      discretization-steps [-] i               > 4
//
//        the discretization-steps are the number of steps used
//        to approximate quadratic transmission losses
//
//      directions B                             < 1 ..
//      injections [W] F                         < 0.0 ..
//      exits [W] F                              < 0.0 ..
//      relative-dutys [-] F                     < 0.0 ..
//      relative-losss [-] F                     < 0.0 ..
//
//        directions entry 1 indicates flow from my cable to my
//        socket -- while 0 indicates the opposite
//
//      nameplate-capacity [W] f                 > 400e+06
//      duty-specific-cost-financial [$/J] f     > 100.0
//      size-specific-cost-financial [$/W/s] f   > 200.0
//      standing-cost-financial [$/s] f          > 300.0
//
//      variable-costs-financial [$] F           < 0.0 ..
//      fixed-costs-financial [$] F              < 0.0 ..
//
//      annual-discount-rate-decimal [-] f       > 0.10
//      economic-life [y] i                      > 20
//      capex-initial [$] f                      > 120e+03
//      capex-terminal [$] f                     >  40e+03
//      current-age [y] i                        > 2
//
//        a negative capex-terminal indicates salvage income as
//        opposed to decommissioning cost
//
//      embedded-costs-financial [$] F           < 0.0 ..
//
//        the 'embedded-costs-financial' are calculated using the
//        DCF annuity method over the economic life of the entity
//        in question
//
//  ============================================================

// ---------------------------------------------------------
//  CLASS           : TeasSubstation
// ---------------------------------------------------------
//  Description  : substation asset, used to step up or down line voltages
//  Role         : concrete entity
//  Techniques   : 'OpsFixedEffy'
//  Status       : complete
//
//  Design notes
//
//      The primary engineering component in a substation is the
//      transformer.  Various other elements that offer, for
//      example, protection or regulation are not considered
//      here.
//
//      Transformer losses can normally be divided into fixed
//      losses and load losses.  Fixed losses are dominated by
//      the transformer core: primarily through magnetic
//      hysteresis and eddy current heating.  Load losses are
//      dominated by the windings: primarily through resistive
//      heating.
//
//      The losses modeled here are split into a no-load absolute
//      loss and a linear efficiency -- taken to represent the
//      fixed and load losses (see above) respectively.
//
//      A better approximation would be to replace the linear
//      efficiency with quadratic losses -- as is done with the
//      transmission line model in 'xeona'.  But in defense of
//      linearization, large transformers normally operate with a
//      full-load efficiency of about 98% and don't really
//      warrant a more sophisticated approach.
//
// ---------------------------------------------------------

class OpsFixedEffy_A;                        // necessary for OSP typedef below

class TeasSubstation :
  public TechnicalAsset,
  public CostRegisterSRFin,
  public CostRegisterEmbFin
{
  // USING DECLARATIONS

protected:

  using Entity::s_logger;                    // place in common scope for this class

  // TYPEDEFS

private:

  typedef OpsFixedEffy_A OpsFixedEffy;       // used for switching implementations

  // DISABLED

private:

  TeasSubstation();                                         // zero-argument constructor
  TeasSubstation(const TeasSubstation& orig);               // copy constructor
  TeasSubstation& operator= (const TeasSubstation& orig);   // copy assignment operator

  // CREATORS

public:

  explicit
  TeasSubstation
  (const std::string entityId,
   Record&           record);

  virtual
  ~TeasSubstation();

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

  const double&                         d_outHiBound;
  const double&                         d_fixedEfficiency;
  const double&                         d_noLoadLoss;

  shared_ptr<std::vector<double> >      d_outputs;

  shared_ptr<Cable<CmElectricity> >     d_inElec;
  shared_ptr<Socket<CmElectricity> >    d_outElec;

  // local quantities

  shared_ptr<OpsFixedEffy>              d_ops;    // specialization required

}; // class 'TeasSubstation'

//  ==== XEDOC =================================================
//
//  entity.teas-substation-0
//
//      class                                    > TeasSubstation
//
//        substation asset used to step up or down line voltages,
//        which also forms losses and costs
//
//        my socket label is 'elec-1'
//
//      builtin-remark s                         <
//
//      socket-electricity l                     > "teas-supplier-0.elec-1"
//      cable-electricity-commodity l            > "cm-electricity-0"
//
//        the socket-electricity and cable-electricity-commodity
//        define my supplier and their socket label and our
//        common electricity commodity
//
//      socket-electricity-commodity l           > "cm-electricity-0"
//
//        the socket-electricity-commodity defines the common
//        electricity commodity shared with my demander
//
//      out-hi-bound [W] f                       > 5.0e+06
//      fixed-efficiency [-] f                   > 0.97
//      no-load-loss [W] f                       > 0.0
//
//        the out-hi-bound sets the substation capacity on
//        output, the low capacity being zero, fixed-efficiency
//        and no-load-loss are self-explanatory
//
//      outputs [W] F                            < 0.0 ..
//
//      nameplate-capacity [W] f                 > 5.0e+06
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

#endif // _TEAS1_H_

//  end of file

