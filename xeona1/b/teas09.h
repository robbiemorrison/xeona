//  XEONA ENERGY SYSTEMS MODELING ENVIRONMENT
//
//  file-create-name : teas09.h
//  file-create-date : Thu 10-Mar-2011 12:58 UTC
//  file-initiator   : Robbie Morrison <robbie@actrix.co.nz>
//
//  file-role        : concrete technical assets 9 / header
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
//  $URL: file:///home/robbie/svn-root/xeona/futz/trunk/xeona1/b/teas09.h $
//
//  GENERAL NOTES FOR THIS FILE
//
//  This unit contains geological carbon capture and storage
//  (CCS) assets.

//  HEADER GUARD

#ifndef _TEAS09_H_
#define _TEAS09_H_

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

class CmElectricity;                         // 'CmElectricity' is from 'Commodity' etc
class CmCarbonSeq;

//  CODE

// ---------------------------------------------------------
//  CLASS           : TeasCcsGeological
// ---------------------------------------------------------
//  Description  : CCS geological storage facility
//  Role         : concrete entity
//  Techniques   : (nothing special)
//  Status       : complete
//
//  Design notes
//
//      Based on geological storage, but not very detailed.
//
//      The emissions factor for electricity generation from coal
//      is about 0.40kg/MJ, therefore a 1000MW plant produces
//      carbon dioxide at the rate of 400kg/s.  As a first cut,
//      one might guesstimate that 10MW is required to pump this
//      into the reserviour, thus requiring 2.5e05J/kg.
//
// ---------------------------------------------------------

class OpsFixedEffy_A;                        // necessary for OSP typedef below

class TeasCcsGeological :
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

  TeasCcsGeological();                                           // zero-argument ctor
  TeasCcsGeological(const TeasCcsGeological& orig);              // copy constructor
  TeasCcsGeological& operator= (const TeasCcsGeological& orig);  // copy assignment opor

  // CREATORS

public:

  explicit
  TeasCcsGeological
  (const std::string entityId,
   Record&           record);

  virtual
  ~TeasCcsGeological();

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

  // INSTANCE DATA

private:

  // tied quantities

  const int&                           d_divisor;
  const double&                        d_demandHiBound;
  const double&                        d_specificElectricityUsage;
  const double&                        d_carbonDioxideLeakageRate;

  shared_ptr<std::vector<double> >     d_burials;
  shared_ptr<std::vector<double> >     d_electricityDemands;

  double&                              d_annualLeakageContribution;

  shared_ptr<Cable<CmElectricity> >    d_inElec;
  shared_ptr<Socket<CmCarbonSeq> >     d_outCSeq;

  // local quantities

  const double                         d_demandLoBound;

  shared_ptr<CmCarbonSeq>              d_cseq;
  shared_ptr<OpsFixedEffy>             d_ops;     // specialization required

}; // class 'TeasCcsGeological'

//  ==== XEDOC =================================================
//
//  entity.teas-ccs-geological-0
//
//      class                                    > TeasCcsGeological
//
//        industrial-scale carbon dioxide geological store using
//        injection pumps powered by electricity
//
//        in modeling terms, the facility "produces"
//        supercritical carbon dioxide sequestration services on
//        demand -- with the carbon dioxide flowing (somewhat
//        paradoxically) parallel to that demand
//
//        any long-haul commitment to monitor leakage and
//        maintain capping beyond the active life of the facility
//        is not currently expressed
//
//        my socket label is 'cseq-1'
//
//      builtin-remark s                         <
//
//      socket-electricity l                     > "teas-supplier-0.elec-1"
//      cable-electricity-commodity l            > "cm-electricity-0"
//
//        the socket-electricity and cable-electricity-commodity
//        define my electricity supplier and their socket label
//        and our common electricity commodity
//
//      socket-carbon-seq-commodity l            > "cm-carbon-seq-0"
//
//        the socket-electricity-commodity defines the common
//        electricity commodity
//
//      divisor [-] i                            > 10
//      demand-hi-bound [kg/s] f                 > 400
//      specific-electricity-usage [J/kg] f      > 2.5e+05
//
//        the divisor is used to downsize a large capture
//        facility for use in a reduced-scope model (in the same
//        way that a building entity can be replicated by
//        specifying a count of two or more) -- but take care not
//        to choke the model
//
//        the demand-hi-bound defines the maximum sequestration
//        rate and the specific-electricity-usage defines the
//        electricity demand per unit sequestration
//
//      carbon-dioxide-leakage-rate [1/1000yr] f  > 0.01
//
//        the carbon-dioxide-leakage-rate per thousand years (1%
//        is low) is used to calculate the sequestration
//        contribution, assuming straight-line losses
//
//      burials [kg/s] F                         < 0.0 ..
//      electricity-demands [W] F                < 0.0 ..
//      annual-leakage-contribution [kg] f       < 0
//
//        burials represent the actual sequestration services,
//        electricity-demands are self-explanatory, and the
//        annual-leakage-contribution is described above
//
//      nameplate-capacity [kg/s] f              > 80.0
//      duty-specific-cost-financial [$/kg] f    > 100.0
//      size-specific-cost-financial [$/kg/s] f  > 200.0
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

#endif // _TEAS09_H_

//  end of file

