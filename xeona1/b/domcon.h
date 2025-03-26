//  XEONA ENERGY SYSTEMS MODELING ENVIRONMENT
//
//  file-create-name : domcon.h
//  file-create-date : Thu 07-Aug-2008 20:51 UTC
//  file-initiator   : Robbie Morrison <robbie@actrix.co.nz>
//
//  file-role        : domain controller entity / header
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
//  $URL: file:///home/robbie/svn-root/xeona/futz/trunk/xeona1/b/domcon.h $

//  HEADER GUARD

#ifndef _DOMCON_H_
#define _DOMCON_H_

//  LOCAL AND SYSTEM INCLUDES

#include "../f/ospmodes.h"    // domain mode enums (header only)
#include "../b/costreg.h"     // cost registers
#include "../b/actor.h"       // actor entity

#include "../a/logger_fwd.h"  // some forward declarations from "logger.h"
#include "../c/smart_ptr.h"   // toggle between Boost and TR1 smart pointers

#include <map>                // STL associative container
#include <string>             // C++ strings
#include <sstream>            // string-streams
#include <vector>             // STL sequence container

#include <boost/tuple/tuple.hpp>             // n-tuples, ref(), cref()

//  FORWARD (PARTIAL) DECLARATIONS

class AssetOperator;                         // data member
class DemandJunction;                        // data member
class Block;                                 // member function return
class Gateway;                               // member function return

class Record;                                // constructor argument
namespace svif { class SolverIf; }           // data member
class CostSet;                               // member function argument

//  CODE

// ---------------------------------------------------------
//  CLASS           : DomainController
// ---------------------------------------------------------
//  Description  : controlling entity for commitment domains
//  Role         : sits under overseer, contains 'capset' and 'transolve' calls
//  Techniques   : each 'DomainController' object contains its own MIP problem interface
//  Leading tag  : domcon
//  Status       : complete
//
//  Design notes
//
//      Commitment mode (aka control strategy)
//
//          The relevant 'domcon' sets the control strategy for
//          its domain via the commitment mode.  The commitment
//          mode itself is encoded in the enum
//          'xeona::DomainMode', as defined in 'f/ospmode.h'
//
//          Commitment modes are set as part of the
//          'DomainController' definition and, in some cases,
//          using read-in model data.
//
//          The commitment mode value is currently invariant, but
//          the design does allow for the commitment mode to be
//          reset partway thru a simulation.
//
//          Users of commitment modes (most notably OSPs) can
//          also encode an acceptable set of control strategies,
//          for instance:
//
//              int d_commitmentModeSum
//                = xeona::e_shortrunFin
//                | xeona::e_shortrunGhg;
//
//          Each recipient can check for commitment mode
//          miscibility (internal compatibility) as follows:
//
//              const tribool ret
//                = xeona::isTwoContained(offeredCommitmentMode,
//                                        d_commitmentModeSum)
//
//          Immiscibility errors should be logged as warnings.
//          Assuming properly coded modules, an immiscibility
//          error indicates a fault with the model (which the
//          modeler should fix).
//
//      "lowest" for member functions
//
//          For the member functions here, "lowest" means the
//          least-ranked (zero-based) index for the given gate:
//
//            selgates: in descending priority
//            buygates: in arbitrary but consistent order
//
//          Recall that selgates are given by the modeler,
//          whereas buygates are revealed by the
//          'xeona::registerGates' call.
//
// ---------------------------------------------------------

class DomainController :
  public Actor,
  public CostRegisterDomcon
{
  // TYPEDEFS

public:

  typedef boost::tuple<CostSet, CostSet, CostSet> costs_type;

  // DISABLED

private:

  DomainController();                                         // zero-argument constructor
  DomainController(const DomainController& orig);             // copy constructor
  DomainController& operator= (const DomainController& orig); // copy assignment operator

  // CREATORS

public:

  explicit
  DomainController
  (const std::string entityId,
   Record&           record);

  virtual
  ~DomainController();

  // ACCESSORS

public:

  int                                        // number of selgates (from input data)
  selgateCount() const;

  int                                        // number of buygates (needs DFS traversal)
  buygateCount() const;

  // OVERSEER POINTS OF ENTRY

  void
  establishDomain();                         // beginning of horizon call

  void
  restructureDomain();                       // structure change call

  void
  setStep                                    // beginning of interval "pre-call"
  (const int step);

  void
  initializeDomain();                        // beginning of interval call

  void
  washupDomain();                            // end of interval call

  void
  consolidateDomain                          // dedicated consolidate costs call
  (const int   step,
   costs_type& overseerCosts);

  void
  concludeDomain();                          // end of horizon call

  // REGISTER GATEWAYS FUNCTION POINTS OF ENTRY

  // used by helper function '::registerGateways' called in turn
  // by free function 'xeona::registerGates' called in turn by
  // 'xeona::simulate'

  std::map<shared_ptr<Gateway>, shared_ptr<DomainController> >
  mapGatesToDomain();

  std::map<shared_ptr<Gateway>, shared_ptr<Block> >
  mapGatesToBlocks();

  std::map<shared_ptr<Block>, shared_ptr<DomainController> >
  mapBlocksToDomain();

  int                                        // number of buygates following insertion
  addBuygate
  (shared_ptr<Gateway> buygate);

  std::vector<shared_ptr<Gateway> >
  retBuygates();

  std::vector<shared_ptr<Gateway> >
  retSelgates();

  // DOMAIN FUNCTIONS

  // used by class 'TravDepthFirst'

  bool getHyphen() const;
  bool markHyphen();
  void resetHyphen();

  // GET GATEWAY FUNCTIONS

  // used by class 'TravDepthFirst'

  shared_ptr<Gateway> lowestNoTildeSel();
  void                pushTilde(shared_ptr<Gateway> gate);
  shared_ptr<Gateway> popTilde();

  // used by functors derived from class 'CapTransAlg'

  // CAUTION: the commented out functions fit the functionality
  // pattern but are not required -- however they should remain
  // in the code because future CTA developments may need them

  shared_ptr<Gateway> lowestNoThetaBuy();
  shared_ptr<Gateway> lowestNoStarBuy();

  shared_ptr<Gateway> lowestNotTransBuy();
  shared_ptr<Gateway> lowestNotTransSel();

//shared_ptr<Gateway> lowestNotCapBuy();
//shared_ptr<Gateway> lowestNotCapSel();

  shared_ptr<Gateway> lowestUnderCapBuy();
  shared_ptr<Gateway> lowestUnderCapSel();

//shared_ptr<Gateway> lowestBuy();
  shared_ptr<Gateway> lowestSel();

  // RESET GATEWAY FUNCTIONS

  void resetTildeSelgates();

  // CAPSET AND TRANSOLVE FUNCTIONS

  // 'capset' uses minimum and maximum network flow problem
  // formulations to determine gateway capacities dynamically
  // (static capset is yet to be designed and implemented)

  bool                                       // return 'false' on failure
  capset
  (const int               step,
   shared_ptr<Gateway>     targetGateway,    // target gateway
   const xeona::DomainMode capacityMode);    // from overseer

  // 'transolve' uses a least cost network flow formulation to
  // determine a minimum shortrun cost solution

  bool                                       // return 'false' on failure
  transolve
  (const int               step,
   const xeona::DomainMode commitmentMode);

  // INTERNAL DATA

private:

  // tied quantities: informational

  const std::string&                         d_domainRole;

  // tied quantities : read-in GLPK settings

  const bool&                                d_initEmployScaling;
  const bool&                                d_initEmployAdvIniBasis;
  const bool&                                d_initSimplexPresolver;
  const bool&                                d_initIntegerPresolver;
  const bool&                                d_initNumericalZero;
  const int&                                 d_tripKktReportLevel;    // in { 0 1 2 3 }
  const int&                                 d_tripCoeffSpanLevel;    // in { 0 1 2 }

  // tied quantities and local quantities

  std::string&                               d_commitment_mode;
  xeona::DomainMode                          d_commitmentMode;

  std::string&                               d_ranked_selgates;
  std::vector<shared_ptr<Gateway> >          d_rankedSelgates;

  std::string&                               d_asset_operators;
  std::vector<shared_ptr<AssetOperator> >    d_assetOperators;

  std::string&                               d_demand_junctions;
  std::vector<shared_ptr<DemandJunction> >   d_demandJunctions;

  // local quantities

  int                                        d_step;
  shared_ptr<svif::SolverIf>                 d_solver;

  std::vector<shared_ptr<Gateway> >          d_randomBuygates;

  std::vector<shared_ptr<Gateway> >          d_tildePushPop;
  bool                                       d_hyphen;      // DFS revisit lock

}; // class 'DomainController'

//  ==== XEDOC =================================================
//
//  entity.domain-controller-0
//
//      class                                    > DomainController
//
//        a domain controller entity (the only one provided)
//        which can take one of a number of commitment modes --
//        but REQUIRES that the managed entities support the
//        elected mode
//
//      builtin-remark s                         <
//
//      domain-role s                            > "domain role"
//
//      init-scale-problem b                     > 1
//      init-use-advanced-initial-basis b        > 1
//      init-use-simplex-presolver b             > 1
//      init-use-mip-presolver b                 > 1
//      init-apply-numerical-zero b              > 1
//
//        these five GLPK solver behavior settings should
//        normally be set to true unless run-time tests indicate
//        otherwise -- numerical zero rounding applies to both
//        input coefficients and output values
//
//      trip-kkt-report-level i                  > 1
//      trip-coeff-span-level i                  > 1
//
//        trip-kkt-report-level in { 0 1 2 3 } for no check, then
//        report awful, close, and every, trip-coeff-span-level
//        in { 0 1 2 } for no check, then report bad and every
//        min and max abs coeff from original matrix
//
//      commitment-mode s                        > "fin"
//
//        supported commitment-mode values (lmp is nodal pricing):
//        fin | ghg | nox | dep | luc | lmp | merit | first
//
//      ranked-selgates L                        > "gate-2 gate-1"
//
//        the ranked-selgates (bridging the right side) must be
//        null, one, or listed in DESCENDING priority
//
//      asset-operators L                        > "asop-1 asop-2"
//
//        the asset-operators may be null, individual, or listed
//        in no particular order
//
//      demand-junctions L                       > "junc-demand-2-split-1"
//
//        the demand-junctions, which split and join demand, may
//        be null, individual, or listed in no particular order
//
//      variable-costs-financial [$] F           < 0.0 ..
//      fixed-costs-financial [$] F              < 0.0 ..
//
//        the following are consolidations for the entire domain
//
//      subtotal-financial [$] f                 < 0.0
//      subtotal-greenhouse [kg] f               < 0.0
//      subtotal-nox [kg] f                      < 0.0
//      subtotal-depletion [J] f                 < 0.0
//      subtotal-landuse [m2] f                  < 0.0
//
//  ============================================================

#endif // _DOMCON_H_

//  end of file

