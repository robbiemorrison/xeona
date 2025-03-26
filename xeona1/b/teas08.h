//  XEONA ENERGY SYSTEMS MODELING ENVIRONMENT
//
//  file-create-name : teas08.h
//  file-create-date : Fri 04-Mar-2011 15:18 UTC
//  file-initiator   : Robbie Morrison <robbie@actrix.co.nz>
//
//  file-role        : concrete technical assets 8 / header
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
//  $URL: file:///home/robbie/svn-root/xeona/futz/trunk/xeona1/b/teas08.h $
//
//  GENERAL NOTES FOR THIS FILE

//  HEADER GUARD

#ifndef _TEAS08_H_
#define _TEAS08_H_

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

class CmOxidGas;

//  CODE

// ---------------------------------------------------------
//  CLASS           : TeasPipelineGas
// ---------------------------------------------------------
//  Description  : single-direction gas pipeline
//  Role         : originally introduced for natural gas transport
//  Techniques   : rather basic
//  Status       : complete
//
//  Design notes
//
//      The compressor stations are powered by gas-turbine units
//      using pipeline gas.
//
//      In this implementation, compressor demand is deemed
//      independent of carriage.
//
//      Gas leakage and compressor station usage are combined and
//      treated as an efficiency issue.
//
//  Model is basic
//
//      The model underpinning this pipeline is rather basic and
//      could certainly be improved.  A more sophisticated model
//      may need to calculate the Reynolds number and use a Moody
//      diagram to calculate pressure losses.  Alternatively, one
//      of the more empirical pipeline design methodologies might
//      suffice.
//
// ---------------------------------------------------------

class OpsFixedEffy_A;                        // necessary for OSP typedef below

class TeasPipelineGas :
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

  typedef OpsFixedEffy_A OpsFixedEffy;       // used for switching implementations

  // DISABLED

private:

  TeasPipelineGas();                                         // zero-argument constructor
  TeasPipelineGas(const TeasPipelineGas& orig);              // copy constructor
  TeasPipelineGas& operator= (const TeasPipelineGas& orig);  // copy assignment operator

  // CREATORS

public:

  explicit
  TeasPipelineGas
  (const std::string entityId,
   Record&           record);

  virtual
  ~TeasPipelineGas();

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

  const double&                       d_outHiBound;
  const double&                       d_pipeDiameter;
  const double&                       d_pipelineLength;
  const double&                       d_leakageFactor;
  const double&                       d_fixedCompressorUsage;

  double&                             d_absoluteLeakageRate;
  double&                             d_relativeLeakageRate;
  double&                             d_absoluteCompressorUsage;
  double&                             d_relativeCompressorUsage;

  shared_ptr<std::vector<double> >    d_inputs;
  shared_ptr<std::vector<double> >    d_outputs;

  shared_ptr<Cable<CmOxidGas> >       d_inFluid;
  shared_ptr<Socket<CmOxidGas> >      d_outFluid;

  // local quantities

  const double                        d_leakageRate;             // for entire pipe run
  const double                        d_compressorUsage;         // for entire pipe run

  double                              d_massSpecificCo2equiv;    // combustion
  double                              d_massSpecificGwp;         // leakage

  shared_ptr<OpsFixedEffy>            d_ops;     // specialization required

}; // class 'TeasPipelineGas'

//  ==== XEDOC =================================================
//
//  entity.teas-pipeline-gas-0
//
//        requires derived commodity in {OxidGas}
//
//      class                                    > TeasPipelineGas
//
//        an asset for transporting specialized gas commodities
//        of type oxidize, with self-powered compressor stations
//        and support for leakage
//
//        my socket label is 'oxid-1'
//
//      builtin-remark s                         <
//
//      socket-oxidize l                         > "teas-supplier1-0.oxid-1"
//      pipe-oxidize-commodity l                 > "cm-oxidize-0"
//
//        socket-oxidize defines my supplier and their socket
//        label and pipe-oxidize-commodity defines the
//        transported commodity
//
//      out-hi-bound [kg/s] f                    > 5.0
//      pipe-diameter [m] f                      > 0.40
//      pipeline-length [m] f                    > 100e+03
//      leakage-factor [kg/s/m] f                > 0.0
//      fixed-compressor-usage [kg/s/m] f        > 0.10
//
//        the out-hi-bound sets the pipeline capacity on output,
//        the low capacity being zero, pipe-diameter and
//        pipeline-length are self-explanatory, the
//        leakage-factor is the leakage rate normalized per metre
//        of length (perhaps 0.5% overall), and the
//        fixed-compressor-usage is deemed duty-independent
//
//      inputs [kg/s] F                          < 0.0 ..
//      outputs [kg/s] F                         < 0.0 ..
//      absolute-leakage-rate [kg/s] f           < 0.0
//      relative-leakage-rate [-] f              < 0.0
//      absolute-compressor-usage [kg/s] f       < 0.0
//      relative-compressor-usage [kg/s] f       < 0.0
//
//        the above should be self-explanatory
//
//      nameplate-capacity [kg/s] f              > 5.0
//      duty-specific-cost-financial [$/kg] f    > 100.0
//      size-specific-cost-financial [$/k/s/s] f > 200.0
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

#endif // _TEAS08_H_

//  end of file

