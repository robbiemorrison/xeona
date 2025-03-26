//  XEONA ENERGY SYSTEMS MODELING ENVIRONMENT
//
//  file-create-name : costreg.h
//  file-create-date : Mon 20-Oct-2008 08:57 UTC
//  file-initiator   : Robbie Morrison <robbie@actrix.co.nz>
//
//  file-role        : cost registers / header
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
//  $URL: file:///home/robbie/svn-root/xeona/futz/trunk/xeona1/b/costreg.h $
//
//  GENERAL NOTES FOR THIS FILE

//  HEADER GUARD

#ifndef _COSTREG_H_
#define _COSTREG_H_

//  LOCAL AND SYSTEM INCLUDES

#include "../c/costs.h"       // cost sets and support

#include "../a/logger_fwd.h"  // some forward declarations from "logger.h"
#include "../c/smart_ptr.h"   // toggle between Boost and TR1 smart pointers

#include <string>             // C++ strings
#include <sstream>            // string-streams
#include <vector>             // STL sequence container

#include <boost/tuple/tuple.hpp>             // n-tuples, ref(), cref()

//  FORWARD (PARTIAL) DECLARATIONS

class CostSet;                               // see "costs.h"
class Record;                                // see "recset.h"

//  DOCUMENTATION

// ---------------------------------------------------------
//  Cost registers
// ---------------------------------------------------------
//
//  Terminology
//
//      Please note the terminology used in this unit is not
//      entirely self-evident and the material here should be
//      read IN CONJUNCTION WITH the one page cost and price
//      taxonomy table given in my (Robbie Morrison) PhD report
//      [and perhaps also available as file
//      'phd-costs-chart_000.odg|pdf'].
//
//  Timebase : per-second versus interval-based
//
//      For reasons of flexibility, the timebase underpinning
//      specific costs is per-second.
//
//      This approach naturally decouples specific costs data and
//      the selected horizon interval (for instance, 1800 versus
//      3600 seconds and so on).
//
//      Cost reporting, in contrast, is typically interval-based.
//      As a rule, the horizon interval is multiplied in at the
//      last possible point in the calculation chain.  And that
//      action should only occur within the 'CostRegister' class
//      hierarchy.
//
//      All other code should presume a per-second timebase in
//      relation to costs.
//
//  Class architecture
//
//      Inheritance (generalization in UML) and not composition
//      (aggregation in UML) is necessary due to the need to tie
//      back variables to the relevant record object (via the
//      'tieSingle' and 'tieTimeseries' calls).
//
//      Moreover, multiple virtual inheritance is required.  For
//      some background, see Stroustrup (1997 pp396-397).
//
//      Firstly, the base class 'CostRegister' is virtually
//      inherited by:
//
//          'TechnicalAsset'
//          'Actor'                and thus 'DomainController'
//          'Overseer'             does not inherit from 'Actor'
//          'Gateway'
//
//      and perhaps later by the currently speculative entities:
//
//          'Zone'
//          'EconomicUnit'
//
//      The cost register base class is:
//
//          'CostRegister'          provides interface but no XEM onus
//
//      This base class architecture provides concrete entities
//      with cost registers and cost consolidation calls -- but
//      does not provide support for tying registers to XEM
//      fields and/or for undertaking specialist cost
//      calculations.
//
//      Secondly, the base class 'CostRegister' is virtually
//      inherited by the following derived cost register classes.
//      These derived cost register classes can be non-virtually
//      inherited by (normally but not necessarily) concrete
//      entities to provide the following additional
//      functionality:
//
//          'CostRegisterSRFin'     financial short-run
//          'CostRegisterSRAll'     above plus remaining short-run
//
//          'CostRegisterEmbFin'    financial embedded (via discounted cash flow)
//          'CostRegisterEmbAll'    above plus remaining embedded
//
//      Entity authors should progressively shift to more
//      encompassing cost register variants as they develop their
//      code.
//
//      Thirdly, the following specialist sub-classes give
//      suitable additional support to the top layer entities on
//      the cost consolidation tree:
//
//          'CostRegisterOverseer'  overseer support
//          'CostRegisterDomcon'    domain controller support
//          'CostRegisterAsop'      asset operator support (standing)
//
//      The first two specializations are not normally of direct
//      interest to entity authors.  The third specialization
//      will be of interest to those developing asset operators.
//
//  Tie process for record entries and local variables
//
//      With regard to data members, shared_ptr<std::vector<T> >
//      types are used as these can naturally tie with record
//      entries.
//
//  Cost consolidation process
//
//      The process of cost consolidation -- that is, the
//      assigning of technical asset and gateway costs to asset
//      operators and then to domain controllers and then to the
//      overseer singleton -- is mostly handled within the
//      function 'DomainController::consolidateDomain'.
//
//      This unit provides the means to undertake this process
//      via the various 'importCosts' and 'exportCosts' calls.
//
//  Naming convention and related information
//
//      term                                type          units
//      ----------------------------------------------------------
//      financial-costs                     fin            UOA
//      revenues                            fin            UOA
//      ghgs                                ghg            kg
//      noxs                                nox            kg
//      depletable-energy-resource-usage    dep            J
//      land-usage                          luc            m^2
//      ----------------------------------------------------------
//      UOA = unit of account (for instance, US dollar or European euro)
//      the ^ character indicates "to the power of"
//
//      term         kind    comment                                  issue
//      -------------------------------------------------------------------------
//      variable-    var     duty (activity) cost plus factor cost    short-run
//      fixed-       fix     nominal size (nameplate capacity) cost   short-run
//      embedded-    emb     DCF annuity cost or embedded cost        long-run
//      -------------------------------------------------------------------------
//      DCF = discounted cash flow
//
//      The notion of a factor cost is simply a duty cost which
//      arises at a gateway and not a technical asset -- simply a
//      matter of definition and nothing more.
//
//  Not all combinations useful
//
//      All naming combinations are trivially supported for
//      reasons of completeness and symmetry.  However some
//      concepts make little sense are therefore not tied to the
//      model data, for instance, 'embLuc' for embedded land
//      usage.
//
//  Adding a new cost type
//
//      Should a new cost types be required (as an illustration,
//      "lab" for labor input), then this unit, the class
//      'CostSet', and the code for switching objective functions
//      will all need modification.  Furthermore, the relevant
//      XEDOC entries will need alignment.
//
//  CAUTION: timeseries indexing
//
//      The model horizon step count is ZERO-based, as are the
//      timeseries vectors.  This convention should therefore
//      lead to straightforward code, but nonetheless keep a
//      lookout for insidious "off-by-one" bugs.
//
// ---------------------------------------------------------

//  CODE

// ---------------------------------------------------------
//  CLASS           : CostRegister (concrete)
// ---------------------------------------------------------
//  Description  : costs container base class with standard interface and processing
//  Role         : provide standardized cost formation support to entities creating costs
//  Techniques   : multiple inheritance, member initialization, record entry ties
//  Status       : complete
//
//  Design notes
//
//      This class contains the basic interface but does not add
//      any XEM onus -- meaning that no data fields are required
//      by this class definition.
//
//      Indeed, no cost vectors are bound to the model io at this
//      point, so they are simply member initialized as "reset"
//      shared pointer vectors.  That said, 'CostRegister'
//      sub-classes can then tie these variables as required.
//
//      Instance data members
//
//          A cost register currently contains the 15 following
//          data members:
//
//            { var, fix, emb } x { fin, ghg, nox, dep, luc }
//
// ---------------------------------------------------------

class CostRegister
{
  // DISABLED

private:

  CostRegister();                                      // zero-argument constructor
  CostRegister(const CostRegister& orig);              // copy constructor
  CostRegister& operator= (const CostRegister& orig);  // copy assignment operator

  // CREATORS

public:

  CostRegister
  (Record& record);

#ifdef _XUTEST
  virtual ~CostRegister();                   // unit test instantiates an object
#else
  virtual ~CostRegister() = 0;               // create abstract class
#endif // _XUTEST

  // UPDATE CALLS - virtual

protected:

  virtual
  void
  updateShortrunCosts                        // excludes embedded costs financial
  (const int      step,                      // does, in theory, allow for reworking
   const CostSet& varCosts,
   const CostSet& fixCosts);

  virtual
  void
  updateRevenues
  (const int    step,
   const double revenues);                   // revenues per second, usually positive

  virtual
  void
  updatePurchases
  (const int    step,
   const double purchases);                  // purchases per second, usually positive

  virtual
  void
  updateEmbeddedCosts                        // simple transfer, no calcs undertaken
  (const int      step,                      // does, in theory, allow for reworking
   const CostSet& emb);                      // calculation left to client

  virtual
  void
  updateEmbeddedCosts                        // employs 'xeona::capitalRecovery'
  (const int step);                          // does, in theory, allow for reworking

  // COST CONSOLIDATION CALLS

  // these cost consolidation calls are used by member functions
  // 'Overseer::run' and 'DomainController::consolidateDomain'

public:

  void
  importCosts                                // utilizes "add-and-assign" operator
  (const int step,
   const boost::tuple
   <CostSet,                                 // accrued "var" variable costs
    CostSet,                                 // accrued "fix" fixed costs
    CostSet>& costs);                        // accrued "emb" embedded costs
                                             // pass-by-ref required
  boost::tuple
  <CostSet,                                  // accrued "var" variable costs
   CostSet,                                  // accrued "fix" fixed costs
   CostSet>                                  // accrued "emb" embedded costs
  exportCosts
  (const int step) const;

  void
  consolidateCosts                           // provided for cost consolidation
  (const int                      step,
   const shared_ptr<CostRegister> entity);   // normally a subsidiary entity

  // COST RESET CALL

  // this cost reset call is used in member functions
  // 'Overseer::run' and 'DomainController::establishDomain' and
  // also 'AssetOperator::establish'
  //
  // in hindsight, it would be better to traverse all entities
  // and make this call -- entities without meaningful registers
  // would simply return straight away

public:

  void
  resetRegister();                      // { var, fix, emb } x { fin, ghg, nox, dep, luc }

  // INSTANCE DATA

protected:

  // strictly local (non-tied) quantities

  const double    d_reset;                   // reset value set via constructor argument
  const int       d_interval;
  const int       d_horizon;

  CostSet         d_dutySpecCosts;           // for gateways, also known as unit price
  CostSet         d_sizeSpecCosts;           // normalized against nominal capacity
  CostSet         d_standingCosts;           // NOT normalized against either duty or size

  // initially local (non-tied) quantities -- which may later be
  // tied by cost register sub-classes as required -- via their
  // constructor blocks and NOT their member initialization lists

  shared_ptr<std::vector<double> >    d_varFins;  // variable costs financial
  shared_ptr<std::vector<double> >    d_fixFins;  // fixed costs financial
  shared_ptr<std::vector<double> >    d_embFins;  // capital recovery contribution
  shared_ptr<std::vector<double> >    d_varGhgs;  // variable costs greenhouse gas
  shared_ptr<std::vector<double> >    d_fixGhgs;  // fixed costs greenhouse gas
  shared_ptr<std::vector<double> >    d_embGhgs;  // embedded costs greenhouse gas
  shared_ptr<std::vector<double> >    d_varNoxs;  // variable costs NOx emissions
  shared_ptr<std::vector<double> >    d_fixNoxs;  // fixed costs NOx emissions
  shared_ptr<std::vector<double> >    d_embNoxs;  // embedded costs NOx emissions
  shared_ptr<std::vector<double> >    d_varDeps;  // variable costs depletable
  shared_ptr<std::vector<double> >    d_fixDeps;  // fixed costs depletable
  shared_ptr<std::vector<double> >    d_embDeps;  // embedded costs depletable
  shared_ptr<std::vector<double> >    d_varLucs;  // variable costs land usage
  shared_ptr<std::vector<double> >    d_fixLucs;  // fixed costs land usage
  shared_ptr<std::vector<double> >    d_embLucs;  // embedded costs land usage

//shared_ptr<std::vector<double> >    d_revenues; // revenues (like negative costs)

// STATIC DATA

protected:

  static logga::spLogger s_logger_costreg;   // shared_ptr to single logger object

}; // class 'CostRegister'

// ---------------------------------------------------------
//  CLASS           : CostRegisterOverseer
// ---------------------------------------------------------
//  Description  : builds on 'CostRegister' to add support for the overseer
//  Role         : use by the class 'Overseer' singleton
//  Techniques   : see overarching documentation
//  Status       : complete
//
//  XEM fields
//
//      variable-costs-financial [$] F           < 0.0 ..
//      fixed-costs-financial [$] F              < 0.0 ..
//      embedded-costs-financial [$] F           < 0.0 ..
//      variable-costs-greenhouse [kg] F         < 0.0 ..
//      fixed-costs-greenhouse [kg] F            < 0.0 ..
//      embedded-costs-greenhouse [kg] F         < 0.0 ..
//      variable-costs-nox [kg] F                < 0.0 ..
//      fixed-costs-nox [kg] F                   < 0.0 ..
//      embedded-costs-nox [kg] F                < 0.0 ..
//      variable-costs-depletion [J] F           < 0.0 ..
//      fixed-costs-depletion [J] F              < 0.0 ..
//      embedded-costs-depletion [J] F           < 0.0 ..
//      variable-costs-landuse [m2] F            < 0.0 ..
//      fixed-costs-landuse [m2] F               < 0.0 ..
//      embedded-costs-landuse [m2] F            < 0.0 ..
//
//      total-financial [$] f                    < 0.0
//      total-greenhouse [kg] f                  < 0.0
//      total-nox [kg] f                         < 0.0
//      total-depletion [J] f                    < 0.0
//      total-landuse [m2] f                     < 0.0
//
//      total-shortrun-financial [$] f           < 0.0
//      total-shortrun-greenhouse [kg] f         < 0.0
//      total-shortrun-nox [kg] f                < 0.0
//      total-shortrun-depletion [J] f           < 0.0
//      total-shortrun-landuse [m2] f            < 0.0
//
// ---------------------------------------------------------

class CostRegisterOverseer :
  public virtual CostRegister                // 'virtual' prevents double registers
{
  // DISABLED

private:

  CostRegisterOverseer();                                              // zero-arg ctor
  CostRegisterOverseer(const CostRegisterOverseer& orig);              // copy constructor
  CostRegisterOverseer& operator= (const CostRegisterOverseer& orig);  // copy ass opor

  // CREATORS

public:

  CostRegisterOverseer
  (Record& record);

  virtual ~CostRegisterOverseer() = 0;       // create abstract class

  // COST CONSOLIDATION CALLS

public:

  void
  importCosts                                // utilizes "add-and-assign" operator
  (const int step,
   const boost::tuple
   <CostSet,                                 // accrued "var" variable costs
    CostSet,                                 // accrued "fix" fixed costs
    CostSet>& costs);                        // accrued "emb" embedded costs,
                                             // pass-by-ref required
  // REPORTING CALLS

protected:

  std::string                                // newline included
  highPrecisionReport                        // the instance data below
  (const int indent) const;

  // INSTANCE DATA

protected:

  double&    d_totalFin;
  double&    d_totalGhg;
  double&    d_totalNox;
  double&    d_totalDep;
  double&    d_totalLuc;

  double&    d_totalShortrunFin;
  double&    d_totalShortrunGhg;
  double&    d_totalShortrunNox;
  double&    d_totalShortrunDep;
  double&    d_totalShortrunLuc;

}; // class 'CostRegisterOverseer'

// ---------------------------------------------------------
//  CLASS           : CostRegisterDomcon
// ---------------------------------------------------------
//  Description  : builds on 'CostRegister' to add support for domain controllers
//  Role         : use by class 'DomainController'
//  Techniques   : see overarching documentation
//  Status       : complete
//
//  XEM fields
//
//      variable-costs-financial [$] F           < 0.0 ..
//      fixed-costs-financial [$] F              < 0.0 ..
//
//      subtotal-financial [$] f                 < 0.0
//      subtotal-greenhouse [kg] f               < 0.0
//      subtotal-nox [kg] f                      < 0.0
//      subtotal-depletion [J] f                 < 0.0
//      subtotal-landuse [m2] f                  < 0.0
//
// ---------------------------------------------------------

class CostRegisterDomcon :
  public virtual CostRegister                // 'virtual' prevents double registers
{
  // DISABLED

private:

  CostRegisterDomcon();                                            // zero-arg ctor
  CostRegisterDomcon(const CostRegisterDomcon& orig);              // copy constructor
  CostRegisterDomcon& operator= (const CostRegisterDomcon& orig);  // copy ass opor

  // CREATORS

public:

  CostRegisterDomcon
  (Record& record);

  virtual ~CostRegisterDomcon() = 0;         // create abstract class

  // MAINTENANCE CALLS

  void
  updateMySubtotals
  (const int step);                          // horizon step

  // INSTANCE DATA

protected:

  double&    d_subtotalFin;
  double&    d_subtotalGhg;
  double&    d_subtotalNox;
  double&    d_subtotalDep;
  double&    d_subtotalLuc;

}; // class 'CostRegisterDomcon'

// ---------------------------------------------------------
//  CLASS           : CostRegisterAsop
// ---------------------------------------------------------
//  Description  : builds on 'CostRegister' to add standing financial support
//  Role         : drop-in replacement
//  Techniques   : see overarching documentation
//  Status       : complete
//
//  Overview - standing costs support (asset operators)
//
//      This class provides additional support for standing
//      costs.
//
//      Asset operators inheriting from this class should place
//      an update update call in their washup function.
//
//  XEM fields
//
//      standing-cost-financial [$/s] f          > 200.0
//      standing-cost-greenhouse [kg/s] f        > 200.0
//      standing-cost-nox [kg/s] f               > 200.0
//      standing-cost-depletion [J/s] f          > 200.0
//      standing-cost-landuse [m2/s] f           > 200.0
//
//      variable-costs-financial [$] F           < 0.0 ..
//      fixed-costs-financial [$] F              < 0.0 ..
//
// ---------------------------------------------------------

class CostRegisterAsop :
  public virtual CostRegister                // 'virtual' prevents double registers
{
  // DISABLED

private:

  CostRegisterAsop();                                          // zero-argument ctor
  CostRegisterAsop(const CostRegisterAsop& orig);              // copy constructor
  CostRegisterAsop& operator= (const CostRegisterAsop& orig);  // copy assignment operator

  // CREATORS

public:

  CostRegisterAsop
  (Record& record);

  virtual ~CostRegisterAsop() = 0;          // create abstract class

  // UPDATE CALLS - for the given horizon step

protected:

  virtual
  void
  updateStandingCosts                        // see documentation and implementation
  (const int step);                          // does, in theory, allow for reworking

   // INSTANCE DATA

 protected:

  // tied quantities

  const double&    d_standingFin;            // standing cost financial
  const double&    d_standingGhg;            // standing cost greenhouse
  const double&    d_standingNox;            // standing cost nox
  const double&    d_standingDep;            // standing cost depletion
  const double&    d_standingLuc;            // standing cost land use

}; // class 'CostRegistedAsop'

// ---------------------------------------------------------
//  CLASS           : CostRegisterSRFin
// ---------------------------------------------------------
//  Description  : builds on 'CostRegister' to add short-run financial support
//  Role         : drop-in replacement
//  Techniques   : see overarching documentation
//  Status       : complete
//
//  Overview - short-run financial support
//
//      This class provides additional support for short-run
//      (variable and fixed) financial costs.
//
//  XEM fields (where . = duty units x s, * = capacity units)
//
//      nameplate-capacity [*] f                 > 200
//      duty-specific-cost-financial [$/.] f     > 2.0
//      size-specific-cost-financial [$/*/s] f   > 3.0
//      standing-cost-financial [$/s] f          > 5.0
//
//      variable-costs-financial [$] F           < 0.0 ..
//      fixed-costs-financial [$] F              < 0.0 ..
//
//  CAUTION: upload functions
//
//      The various 'CostRegister::uploadShortrunCosts' functions
//      need to be repeated here.
//
// ---------------------------------------------------------

class CostRegisterSRFin :
  public virtual CostRegister                // 'virtual' prevents double registers
{
  // DISABLED

private:

  CostRegisterSRFin();                                           // zero-argument ctor
  CostRegisterSRFin(const CostRegisterSRFin& orig);              // copy constructor
  CostRegisterSRFin& operator= (const CostRegisterSRFin& orig);  // copy assignment opor

  // CREATORS

public:

  CostRegisterSRFin
  (Record& record);

  virtual ~CostRegisterSRFin() = 0;          // create abstract class

  // UPDATE CALLS - for the given horizon step

protected:

  virtual
  void
  updateShortrunCosts                        // excludes embedded costs financial
  (const int      step,                      // does, in theory, allow for reworking
   const CostSet& varCosts,
   const CostSet& fixCosts);

  // INSTANCE DATA

 protected:

  // tied quantities

  const double&    d_nameplateSize;          // for size cost calculations
  const double&    d_dutySpecFin;            // duty-specific cost financial
  const double&    d_sizeSpecFin;            // size-specific cost financial
  const double&    d_standingFin;            // standing cost financial

}; // class 'CostRegistedSRFin'

// ---------------------------------------------------------
//  CLASS           : CostRegisterSRAll
// ---------------------------------------------------------
//  Description  : builds on 'CostRegisterSRFin' to add short-run non-financial support
//  Role         : drop-in replacement
//  Techniques   : see overarching documentation
//  Status       : complete
//
//  Overview - short-run financial support
//
//      This class provides additional support for short-run
//      non-financial costs.
//
//  XEM fields (where . = duty units x s, * = capacity units)
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
// ---------------------------------------------------------

class CostRegisterSRAll :
  public CostRegisterSRFin
{
  // DISABLED

private:

  CostRegisterSRAll();                                           // zero-argument ctor
  CostRegisterSRAll(const CostRegisterSRAll& orig);              // copy constructor
  CostRegisterSRAll& operator= (const CostRegisterSRAll& orig);  // copy assignment opor

  // CREATORS

public:

  CostRegisterSRAll
  (Record& record);

  virtual ~CostRegisterSRAll() = 0;          // create abstract class

  // UPDATE CALLS - for the given horizon step

protected:

  virtual
  void
  updateShortrunCosts                        // excludes embedded costs
  (const int      step,                      // does, in theory, allow for reworking
   const CostSet& varCosts,
   const CostSet& fixCosts);

  // INSTANCE DATA

 protected:

  // tied quantities

  const double&    d_dutySpecGhg;            // duty-specific cost greenhouse
  const double&    d_sizeSpecGhg;            // size-specific cost greenhouse
  const double&    d_standingGhg;            // standing cost greenhouse
  const double&    d_dutySpecNox;            // duty-specific cost nox
  const double&    d_sizeSpecNox;            // size-specific cost nox
  const double&    d_standingNox;            // standing cost nox
  const double&    d_dutySpecDep;            // duty-specific cost depletion
  const double&    d_sizeSpecDep;            // size-specific cost depletion
  const double&    d_standingDep;            // standing cost depletion
  const double&    d_dutySpecLuc;            // duty-specific cost land usage
  const double&    d_sizeSpecLuc;            // size-specific cost land usage
  const double&    d_standingLuc;            // standing cost land usage

}; // class 'CostRegistedSRAll'

// ---------------------------------------------------------
//  CLASS           : CostRegisterEmbFin
// ---------------------------------------------------------
//  Description  : builds on 'CostRegister' to add DCF support
//  Role         : drop-in replacement
//  Techniques   : see overarching documentation
//  Status       : complete
//
//  Note
//
//      DCF is discounted cash flow (analysis)
//
//  Overview - additional DCF support
//
//      This class provides additional support for discounted
//      cash flow analysis.
//
//  XEM fields
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
//  CAUTION: upload functions
//
//      The various 'CostRegister::uploadEmbeddedCost' functions
//      need to be repeated here.
//
// ---------------------------------------------------------

class CostRegisterEmbFin :
  public virtual CostRegister
{
  // DISABLED

private:

  CostRegisterEmbFin();                                            // zero-argument ctor
  CostRegisterEmbFin(const CostRegisterEmbFin& orig);              // copy constructor
  CostRegisterEmbFin& operator= (const CostRegisterEmbFin& orig);  // copy assignment opor

  // CREATORS

public:

  CostRegisterEmbFin
  (Record& record);

  virtual ~CostRegisterEmbFin() = 0;         // create abstract class

  // UPDATE CALLS - for the given horizon step

  virtual
  void
  updateEmbeddedCosts                        // includes DCF financial cost calculations
  (const int step);                          // does, in theory, allow for reworking

  // INSTANCE DATA

protected:

  // tied quantities

  const double&    d_annualDiscountRate;     // risk-adjusted interest per annum (decimal)
  const int&       d_economicLife;           // economic life in years (not seconds)
  const double&    d_capitalInputInitial;    // capital input at start of economic life
  const double&    d_capitalInputTerminal;   // capital input at end of economic life
  const int&       d_currentAge;             // current age

}; // class 'CostRegisterEmbFin'

// ---------------------------------------------------------
//  CLASS           : CostRegisterEmbAll
// ---------------------------------------------------------
//  Description  :  builds on 'CostRegisterEmbFin' to add levelized non-financial support
//  Role         : drop-in replacement
//  Techniques   : see overarching documentation
//  Status       : complete
//
//  Note
//
//      DCF is discounted cash flow (analysis)
//
//  Overview - additional DCF support
//
//      This class provides additional support for discounted
//      cash flow analysis.
//
//  XEM fields
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
// ---------------------------------------------------------

class CostRegisterEmbAll :
  public CostRegisterEmbFin
{
  // DISABLED

private:

  CostRegisterEmbAll();                                            // zero-argument ctor
  CostRegisterEmbAll(const CostRegisterEmbAll& orig);              // copy constructor
  CostRegisterEmbAll& operator= (const CostRegisterEmbAll& orig);  // copy assignment opor

  // CREATORS

public:

  CostRegisterEmbAll
  (Record& record);

  virtual ~CostRegisterEmbAll() = 0;         // create abstract class

  // UPDATE CALLS - for the given horizon step

  virtual
  void
  updateEmbeddedCosts                        // includes DCF financial cost calculations
  (const int step);                          // does, in theory, allow for reworking

  // INSTANCE DATA

protected:

  // tied quantities

  const int&       d_physicalLife;           // physical life in years (not seconds)
  const double&    d_investGhg;              // greenhouse investment
  const double&    d_investNox;              // nox investment
  const double&    d_investDep;              // depletion investment
  const double&    d_investLuc;              // land use investment

}; // class 'CostRegisterEmbAll'

#endif // _COSTREG_H_

//  end of file

