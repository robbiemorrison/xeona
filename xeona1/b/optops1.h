//  XEONA ENERGY SYSTEMS MODELING ENVIRONMENT
//
//  file-create-name : optops.h
//  file-create-date : Fri 17-Oct-2008 14:40 UTC
//  file-initiator   : Robbie Morrison <robbie@actrix.co.nz>
//
//  file-role        : operations OSPs for technical assets / header
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
//  $URL: file:///home/robbie/svn-root/xeona/futz/trunk/xeona1/b/optops1.h $
//
//  GENERAL NOTES FOR THIS FILE
//
//  OSP class naming conventions
//
//      Take class "OpsDevice_A" for instance -- this refers to
//      implementation A of the OpsDevice optimization
//      sub-problem.
//
//      A second implementation B would retain the same interface
//      -- that is, all public function declarations would remain
//      identical -- and only the internals would change.
//
//      Moreover, there should be no requirement that
//      implementation B should mimic the behavior of
//      implementation A.  But the two behaviors should be
//      documented and preferably by difference if possible.

//  HEADER GUARD

#ifndef _OPTOPS1_H_
#define _OPTOPS1_H_

//  LOCAL AND SYSTEM INCLUDES

#include "../b/optprob.h"     // optimization sub-problem and key sub-classes

#include "../c/util3.h"       // free functions for floating point comparison
#include "../a/logger_fwd.h"  // some forward declarations from "logger.h"
#include "../c/smart_ptr.h"   // toggle between Boost and TR1 smart pointers

#include <string>             // C++ strings

//  CODE

// ---------------------------------------------------------
//  CLASS           : OpsDcTrans_A
// ---------------------------------------------------------
//  Description  : discretized two-way DC transmission line
//  Role         : 'TeasDcTransmission'
//  Techniques   : discretization
//  Status       : complete
// ---------------------------------------------------------

class OpsDcTrans_A :
  public OperationsOsp
{
  // TYPEDEFS

public:

  typedef boost::tuple                       // can be in either local or global cols
  <int,                                      // cable (single, bidirectional)
   int>                                      // socket (single, bidirectional)
  index_type;

  typedef boost::tuple
  <bool,                                     // flow direction flag, 'true' socket-2-cable
   double,                                   // cable flow [W]
   double,                                   // socket flow [W]
   double,                                   // relative duty [-]
   double>                                   // relative loss [-]
  results_type;

  // CREATORS

public:

  OpsDcTrans_A
  (shared_ptr<svif::SolverIf> solver,
   const xeona::DomainMode    commitmentMode);

  virtual
  ~OpsDcTrans_A();

  // FILL PROBLEM CALLS

public:

  // lower level processing (principal problem building call)
  index_type                                 // global cols
  uploadEngineering
  (const double injectCapacity,              // in unprefixed SI units [W]
   const double voltage,                     // in unprefixed SI units [V]
   const double ohmsPerMetre,                // in unprefixed SI units [ohm/m]
   const double length,                      // in unprefixed SI units [m]
   const int    discretization);             // number of discretization steps

  // higher level call (convenience call), see the implementation
  // file for more information
  index_type                                 // global cols
  uploadEngineering
  (const double injectCapacity,              // injection capacity
   const double maxRelLosses,                // relative losses at capacity (not percent)
   const int    discretization);             // number of discretization steps

  // DOWNLOAD SOLUTION CALLS

public:

  results_type                               // see local typedef
  downloadSolution() const;

  // INSTANCE DATA

private:

  index_type    d_cols;                      // local cols
  double        d_injectCapacity;

}; // class 'OpsDcTrans_A'

// ---------------------------------------------------------
//  CLASS           : OpsFac1Out1_A
// ---------------------------------------------------------
//  Description  : two-port conversion with constant marginal efficiency and shutdown mode
//  Role         : for instance, 'TeasOxidToElec'
//  Techniques   : binary variables
//  Status       : complete
// ---------------------------------------------------------

class OpsFac1Out1_A :
  public OperationsOsp
{
  // TYPEDEFS

public:

  typedef boost::tuple                       // can be in either local or global cols
  <int,                                      // fuel stream
   int,                                      // product stream (typically for coupling)
   int>                                      // trip status (nonsensical for coupling)
  index_type;

  typedef boost::tuple
  <double,                                   // fuel usage (factor) [W]
   double,                                   // product (output) [W]
   bool>                                     // trip status, 'true' = ran
  results_type;

  // CREATORS

public:

  OpsFac1Out1_A
  (shared_ptr<svif::SolverIf> solver,
   const xeona::DomainMode    commitmentMode);

  virtual
  ~OpsFac1Out1_A();

  // FILL PROBLEM CALLS

public:

  index_type                                 // global cols
  uploadEngineering                          // J-to-J form
  (const double prodLoBound,                 // lower output below which the asset trips
   const double prodHiBound,                 // upper output
   const double marginalEfficiency,          // slope (as decimal not percentage)
   const double fuelNoload,                  // fuel use on idle (input-axis intercept)
   const double fuelAncillary,               // fuel use on shutdown (for ancillaries)
   const double rampLoSize,                  // ramp down restriction ('inf' to ignore)
   const double rampHiSize);                 // ramp up restriction ('inf' to ignore)

  index_type                                 // global cols
  uploadEngineering                          // kg-to-J form
  (const double prodLoBound,                 // lower output below which the asset trips
   const double prodHiBound,                 // upper output
   const double marginalEfficiency,          // slope (as decimal not percentage)
   const double fuelNoload,                  // fuel use on idle (input-axis intercept)
   const double fuelAncillary,               // fuel use on shutdown (for ancillaries)
   const double rampLoSize,                  // ramp down restriction ('inf' to ignore)
   const double rampHiSize,                  // ramp up restriction ('inf' to ignore)
   const double specEnthalpy,                // kg->J conversion
   const double scaleFactor);                // for a better conditioned constraint matrix

  // DOWNLOAD SOLUTION CALLS

  results_type
  downloadSolution() const;

  // INSTANCE DATA

private:

  double        d_scaleFactor;               // to accommodate mass-specific enthalpy
  index_type    d_cols;                      // local cols

}; // class 'OpsFac1Out1'

// ---------------------------------------------------------
//  CLASS           : OpsFac0Out1_A
// ---------------------------------------------------------
//  Description  : harvest/extract/import source-style OSP
//  Role         : for instance, 'TeasWindfarm'
//  Techniques   : low (often zero) and high bounds
//  Status       : complete
//
//  Design notes
//
//      The concept of a "source" entity covers harvest,
//      extraction, and un-modeled import.
//
//      The concept also imply a "reservoir" in the thermodynamic
//      and economic senses -- that is, an invariant intensive
//      state is assumed, irrespective of rate of take.
//
// ---------------------------------------------------------

class OpsFac0Out1_A :
  public OperationsOsp
{
  // TYPEDEFS

public:

  typedef boost::tuple                       // can be local or global cols
  <int>                                      // output variable
  index_type;

  typedef boost::tuple
  <double>                                   // actual output [W]
  results_type;

  // CREATORS

public:

  OpsFac0Out1_A
  (shared_ptr<svif::SolverIf> solver,
   const xeona::DomainMode    commitmentMode);

  virtual
  ~OpsFac0Out1_A();

  // FILL PROBLEM CALLS

public:

  index_type                                 // global cols
  uploadEngineering
  (const double prodLoBound,                 // lower output (typically zero)
   const double prodHiBound);                // upper output

  // DOWNLOAD SOLUTION CALLS

  results_type                               // see local typedef
  downloadSolution() const;

  // INSTANCE DATA

private:

  index_type    d_cols;                      // local cols

}; // class 'OpsFac0Out1'

// ---------------------------------------------------------
//  CLASS           : OpsFac1Out0_A
// ---------------------------------------------------------
//  Description  : load/export sink-style OSP
//  Role         : for instance, 'TeasLoadElecTs'
//  Techniques   : low (often zero) and high bounds, single bound
//  Status       : complete
//
//  Design notes
//
//      The concept of a "sink" entity covers load and un-modeled
//      export.
//
//      The concept also imply a "reservoir" in the thermodynamic
//      and economic senses -- that is, an invariant intensive
//      state is assumed, irrespective of rate of disposal.
//
// ---------------------------------------------------------

class OpsFac1Out0_A :
  public OperationsOsp
{
  // TYPEDEFS

public:

  typedef boost::tuple                       // can be local or global cols
  <int>                                      // output variable
  index_type;

  typedef boost::tuple
  <double>                                   // actual input [W]
  results_type;

  // CREATORS

public:

  OpsFac1Out0_A
  (shared_ptr<svif::SolverIf> solver,
   const xeona::DomainMode    commitmentMode);

  virtual
  ~OpsFac1Out0_A();

  // FILL PROBLEM CALLS

public:

  index_type                                 // global cols
  uploadEngineering
  (const double takeFixed);                  // fixed input (can be zero)

  index_type                                 // global cols
  uploadEngineering
  (const double takeLoBound,                 // lower input (typically zero)
   const double takeHiBound);                // upper input

  // DOWNLOAD SOLUTION CALLS

  results_type                               // see local typedef
  downloadSolution() const;

  // INSTANCE DATA

private:

  index_type    d_cols;                      // local cols

}; // class 'OpsFac1Out0'

// ---------------------------------------------------------
//  CLASS           : OpsFixedEffy_A
// ---------------------------------------------------------
//  Description  : two-port non-conversion with fixed efficiency
//  Role         : for instance, 'TeasSubstation'
//  Techniques   : (nothing special)
//  Status       : complete
// ---------------------------------------------------------

class OpsFixedEffy_A :
  public OperationsOsp
{
  // TYPEDEFS

public:

  typedef boost::tuple                  // can be in either local or global cols
  <int,                                 // in (factor) stream
   int>                                 // out (product) stream (typically for coupling)
  index_type;

  typedef boost::tuple
  <double,                              // input (factor) [W]
   double>                              // output (product) [W]
  results_type;

  // CREATORS

public:

  OpsFixedEffy_A
  (shared_ptr<svif::SolverIf> solver,
   const xeona::DomainMode    commitmentMode);

  virtual
  ~OpsFixedEffy_A();

  // FILL PROBLEM CALLS

public:

  index_type                                 // global cols
  uploadEngineering                          // J-to-J form
  (const double outLoBound,                  // lower output (usually zero)
   const double outHiBound,                  // upper output
   const double fixedEfficiency,             // efficiency (as decimal not percentage) [1]
   const double noLoadLoss = 0.0);           // no load loss (as absolute)

  // [1] the 'fixedEfficiency' need not be dimensionless and hence may exceed unity

  // DOWNLOAD SOLUTION CALLS

  results_type
  downloadSolution() const;

  // INSTANCE DATA

private:

  index_type    d_cols;                      // local cols

}; // class 'OpsFixedEffy'

// ---------------------------------------------------------
//  CLASS           : OpsFuelToCseq_A
// ---------------------------------------------------------
//  Description  : two-port fuel to carbon sequestration coupling with fixed capture rate
//  Role         : for instance, with entity 'TeasCcgtCapture'
//  Techniques   : linear
//  Status       : complete
//
//  Design notes
//
//      The carbon stoichiometry variable 'specCarbonDioxide'
//      [kg/kg] in the range [0,3.7] and the dimensionless
//      capture rate 'carbonCaptureRate' [0,1] are required.
//
//      The worst case stoichiometry is pure carbon which has a
//      specific CO2 "content" of 3.664 and rounded here to 3.7.
//      The best case stoichiometry is say pure hydrogen with a
//      "content" of zero (not that capture would be fitted in
//      this case).  Anyhow, these two bounds are used for range
//      checking.
//
// ---------------------------------------------------------

class OpsFuelToCseq_A :
  public OperationsOsp
{
  // TYPEDEFS

public:

  typedef boost::tuple                       // can be in either local or global cols
  <int,                                      // fuel input
   int>                                      // cseq input
  index_type;

  typedef boost::tuple
  <double,                                   // fuel flow [kg/s]
   double,                                   // cseq flow [kg/s]
   double>                                   // co2e emissions [kg/s]
  results_type;

  // CREATORS

public:

  OpsFuelToCseq_A
  (shared_ptr<svif::SolverIf> solver,
   const xeona::DomainMode    commitmentMode);

  virtual
  ~OpsFuelToCseq_A();

  // FILL PROBLEM CALLS

public:

  index_type                                 // global cols
  uploadEngineering
  (const double cseqLoBound,                 // lower output (typically zero)
   const double cseqHiBound,                 // upper output (can be +inf)
   const double specCarbonDioxide,           // mass-specific [kg/kg] ranging [0,3.7]
   const double carbonCaptureRate);          // normalized [-] ranging [0,1]

  // DOWNLOAD SOLUTION CALLS

  results_type
  downloadSolution() const;

  // INSTANCE DATA

private:

  double        d_specCarbonDioxide;         // keep 'uploadEngineering' data for washup
  double        d_carbonCaptureRate;         // keep 'uploadEngineering' data for washup

  index_type    d_cols;                      // local cols

}; // class 'OpsFuelToCseq_A'

// ---------------------------------------------------------
//  CLASS           : OpsStore_A
// ---------------------------------------------------------
//  Description  : two-port storage with option for elective spill, efficiency, and decay
//  Role         : for instance, 'TeasSimpleStorage'
//  Techniques   : binary variables
//  Status       : complete
//
//  Design notes
//
//      The optional spill allows any forced supply not required
//      to be dumped.  In practice, this implies some associated
//      engineering components, for instance, an air cooled
//      resistor bank.
//
//      The round trip efficiency is split equally (using sqrt)
//      between the recharge and discharge flows.
//
//      The normalized decay term is applied exactly as received.
//      The client is responsible for calculating its value,
//      perhaps using exponential decay on a given half-life.
//
// ---------------------------------------------------------

class OpsStore_A :
  public OperationsOsp
{
  // TYPEDEFS

public:

  typedef boost::tuple                       // can be in either local or global cols
  <int,                                      // recharge stream
   int>                                      // discharge stream
  index_type;

  typedef boost::tuple                       // only one element can be non-zero (checked)
  <double,                                   // recharge (+ve) or discharge (-ve) [*/s]
   double,                                   // opening inventory [*]
   double>                                   // closing inventory [*]
  results_type;

  // CREATORS

public:

  OpsStore_A
  (shared_ptr<svif::SolverIf> solver,
   const xeona::DomainMode    commitmentMode);

  virtual
  ~OpsStore_A();

  // FILL PROBLEM CALLS

public:

  index_type
  uploadEngineering
  (const bool   spillFlag,                   // 'true' means spilling is allowed
   const double openingInventory,
   const double loCapacity,                  // usually zero
   const double hiCapacity,
   const double rechargeBound,
   const double dischargeBound,
   const double roundTripEffy,               // round-trip efficiency [-]
   const double normalizedDecay,             // one minus normalized remaining [-]
   const int    interval);                   // interval length [s]

  void
  uploadShortrunCosts
  (const CostSet& sizeSpecCosts);

  void
  uploadShortrunCosts
  (const CostSet& dutySpecCosts,
   const int      gol);                      // normally recharge or discharge stream

  // DOWNLOAD SOLUTION CALLS

  results_type
  downloadSolution() const;

  // INSTANCE DATA

private:

  double                    d_roundTripEffy;
  double                    d_normalizedDecay;
  double                    d_openingInventory;
  double                    d_hiCapacity;
  int                       d_interval;
  index_type                d_cols;          // local cols, exposed as usual
  index_type                d_colsInternal;  // local cols, for internal usage
  const xeona::Precision    d_roundZeroTrip; // note also 'xeona::numericalZero'
  const double              d_spillPenalty;  // arbitrary penalty to prejudice spill

}; // class 'OpsStore_A'

// ---------------------------------------------------------
//  CLASS           : OpsDummy_A
// ---------------------------------------------------------
//  Description  : dummy OSP that mimics a two-way transmission line
//  Role         : for 'TeasCapA|B'
//  Techniques   : discretization
//  Status       : complete
// ---------------------------------------------------------

class OpsDummy_A :
  public OperationsOsp
{
  // TYPEDEFS

public:

  typedef boost::tuple                       // can be in either local or global cols
  <int>                                      // cable or socket (single, bidirectional)
  index_type;

  typedef boost::tuple
  <double>                                   // cable or socket flow (should be zero)
  results_type;

  // CREATORS

public:

  OpsDummy_A
  (shared_ptr<svif::SolverIf> solver,
   const xeona::DomainMode    commitmentMode);

  virtual
  ~OpsDummy_A();

  // FILL PROBLEM CALLS

public:

  index_type                                 // global col
  uploadEngineering();

  // DOWNLOAD SOLUTION CALLS

public:

  results_type
  downloadSolution() const;

  // INSTANCE DATA

  index_type    d_cols;

}; // class 'OpsDummy_A'

#endif // _OPTOPS1_H_

//  end of file

