//  XEONA ENERGY SYSTEMS MODELING ENVIRONMENT
//
//  file-create-name : optprob.h
//  file-create-date : Fri 17-Oct-2008 08:19 UTC
//  file-initiator   : Robbie Morrison <robbie@actrix.co.nz>
//
//  file-role        : optimization sub-problem (OSP) and key sub-classes / header
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
//  $URL: file:///home/robbie/svn-root/xeona/futz/trunk/xeona1/b/optprob.h $
//
//  GENERAL NOTES FOR THIS FILE

//  HEADER GUARD

#ifndef _OPTPROB_H_
#define _OPTPROB_H_

//  OVERVIEW
//
//  Design strategy
//
//      OSP means "optimization sub-problem" and refers to all
//      concrete classes derived from the 'OptimSubProb' abstract
//      base class.
//
//      On first view, the introduction of an OSP class hierarchy
//      may appear an unnecessary complication -- after all, the
//      equivalent code could be buried in the entities
//      themselves (whilst noting that that approach is not
//      precluded).  However the selected design should reduce
//      code repetition and duplicated testing.
//
//      The scheme given here therefore tries to provide a set of
//      relatively generic OSPs for use by entity authors with
//      the aim of ultimately reducing the associated coding and
//      commissioning overhead.
//
//  CAUTION: problem build requires EXCLUSIVE access
//
//      This code assumes each 'OptimSubProb' derived instance
//      has exclusive access to the relevant solver interface
//      'svif::SolverIf' object during problem loading.
//      Therefore DO NOT INTERLACE problem loading code.
//
//      Enforcement via build locking, for instance, is not
//      provided (but perhaps should be).
//
//      Further details are given in the implementation file.
//
//  Terminology and usage
//
//      The following naming convention for OSP member functions
//      is employed:
//
//          internal (protected) calls begin : "push" "repush" "recover"
//          external (public) calls begin    : "upload" "download"
//
//      The following indexing definitions and terminology are used:
//
//          columns represent variables (strictly speaking, structural variables)
//          rows represent constraints (strictly speaking, axillary variables)
//
//          col : local col using the 'd_colStart' offset
//          row : local row using the 'd_rowStart' offset
//          gol : global col
//          gow : global row
//
//      If need be, calls regarding variables should be named
//      thus:
//
//          getXxxx
//          setXxxx    also returns previous value
//          Xxxx       reference (alias) variant uses identical name
//
//      Problem building calls return:
//
//          void
//          boost::tuple<int>
//          boost::tuple<int, int, ..>
//
//      Problem building calls more specifically:
//
//          uploadEngineering      technical description
//          uploadShortrunCosts    duty and factor specific costs,
//                                   overwrite or increment "shift" *
//          uploadBidset           LMP (nodal pricing) bid set
//          uploadCeilingDuty      current upper capacity = maximum duty
//          uploadFloorDuty        current lower capacity = must-run duty
//
//          * overloaded function, meaning depends on signature
//
//      Solution recovery calls include:
//
//          boost::tuple<CostSet, CostSet> results = downloadShortrunCost
//          results_type                   results = downloadSolution
//
//  One and zero-based indexing
//
//     The rows and cols (and thereby gows and gols) utilize
//     ONE-based indexing.  This aligns with the conventions
//     employed by GLPK.
//
//     The various objective coefficient vectors use ZERO-based
//     indexing -- the 0th entry is for the so-called "shift"
//     constant term.
//
//     Boost.Tuple tuples use ZERO-based indexing.
//
//     For completeness, the step count is also ZERO-based.
//
//     In general, pay attention to the indexing base and look
//     for off-by-one errors!
//
//  Duty not for OSPs
//
//      OSPs do not support the notion of "duty" although they
//      must and do return the gols to their various input and
//      output streams.  Rather, duty and duty coupling are
//      concepts for the technical asset and/or asset operator
//      and, more particularly, must be resolved within their
//      'constrain' calls.
//
//  Tuple usage
//
//      Function arguments and function returns that could
//      potentially utilize more than one value are ALWAYS
//      declared as Boost.Tuple tuples.  This means, somewhat
//      awkwardly perhaps, the use of 1-tuple arguments and
//      1-tuple returns where only one value is required.  The
//      upside is that a consistent idiom can be maintained and
//      that the notion of multiple dimensionality is reinforced.
//
//  Scheme (may not be current)
//
//      OSP derivative        object prefix   unit *      typical host
//      -------------------------------------------------------------
//      CouplingOsp           **              optprob     couple ***
//      ConnectionOsp         con             optprob     Interface ****
//      sub-OperationsOsp     ops             optops      TechnicalAsset
//                                            optgate     Gateway
//                                            optjunc     DomainJunction
//                                            optnode     HvNode
//      sub-ControlOsp        ctl             optctl      AssetOperator
//      sub-QuantityOsp       qan             optgate     Gateway
//      sub-Offer             ofr             optgate     Gateway
//      -------------------------------------------------------------
//      "sub-" indicates a derived class hierarchy
//        * = translation unit (read "header") which holds concrete specializations
//       ** = use free function 'xeona::couple' in preference to dedicated ctor call
//      *** = unlike other examples, OSP only last for duration of call
//     **** = a grep search reveals that the  "con"  is probably not used
//
//  CLASS HIERARCHY (ongoing and possibly out-of-date)
//
//                                             unit
//      -------------------------------------------------------------
//      - OptimSubProb                    -+
//          + CouplingOsp                  |   optprob (here)
//          + ConnectionOsp               -+
//          - OperationsOsp                    optops
//              + OpsXxxx_X
//                ...
//          - ControlOsp                       optctl
//              + CtlXxxx_X
//                ...
//          - QuantityOsp                 -+
//              + QanTechCapacity_X        |
//              + QanObligToSupply_X       |   optgate
//          - OfferOsp                     |
//              + OfrTariffSet_X          -+
//          - JunctionOsp
//              + JncSplit2                    optjunc
//                ...
//          - LmpNodeOsp
//              + LmpCab1A                     optnode
//                ...
//              + Lmp2NodeOsp
//                + Lmp2Cab1A                  optnode2
//                ...
//      -------------------------------------------------------------
//        - = abstract  + = concrete

//  LOCAL AND SYSTEM INCLUDES

#include "../f/ospmodes.h"    // domain mode enums (header only)
#include "../d/siglp.h"       // semi-intelligent interface to GLPK MILP solver
#include "../c/costs.h"       // cost sets and support

#include "../a/logger_fwd.h"  // some forward declarations from "logger.h"
#include "../c/smart_ptr.h"   // toggle between Boost and TR1 smart pointers

#include <limits>             // numeric_limits<T>::infinity() and similar
#include <string>             // C++ strings
#include <vector>             // STL sequence container

#include <boost/tuple/tuple.hpp>        // n-tuples, ref(), cref()

//  FORWARD (PARTIAL) DECLARATIONS

class BandedTariffSet;                       // an 'uploadTariffSet' argument

//  CODE

// ---------------------------------------------------------
//  CLASS           : OptimSubProb (abstract)
// ---------------------------------------------------------
//  Description  : entity-local abstraction to the domain-specific solver interface
//  Role         : abstract base class for all '*Osp' classes
//  Techniques   : "push loading" (see below), sub-classes regularly use 'boost::tuple'
//  Status       : complete
// ---------------------------------------------------------

class OptimSubProb
{
  // DISABLED

private:

  OptimSubProb(const OptimSubProb& orig);              // copy constructor
  OptimSubProb& operator= (const OptimSubProb& orig);  // copy assignment operator

  // CREATORS

public:

  OptimSubProb();                            // zero-argument constructor

  // CAUTION: zero-argument constructor should only be used by
  // the base-class 'Interface' constructor

  OptimSubProb
  (shared_ptr<svif::SolverIf> solver,        // solver interface object
   const xeona::DomainMode    commitmentMode,
   const std::string&         ospDesc = ""); // hardcode for logs, see also 'loadOspLabel'

  virtual
  ~OptimSubProb() = 0;                       // create abstract class

  // HOST-RELATED CALLS - used by assets and operators

public:

  std::string
  loadOspLabel                               // optional OSP labeling
  (const std::string label);                 // sub-problem (and not solver) label

  // ACCESSORS

  int
  getRowCount() const;

  int
  getColCount() const;

  //  CALLS PROVIDED FOR TEST PURPOSES

public:

  // CAUTION: DO NOT USE these calls if interleaving OSPs

  bool                                       // should always be 'true'
  checkCounts();                             // tests a subset of 'reportBuildIntegrity'

  bool                                       // should always be 'true'
  reportBuildIntegrity                       // can call any time
  (const unsigned    failLevel,              // report on fail level
   const std::string comment = "");

  // PUSH CALLS (and similar)

protected:

  // virtual

  virtual
  void
  pushIncShift                               // increment objective function constant term
  (const double      shiftValue,             // non-short-run commitment mode
   const CostSet&    specCosts);

  virtual
  void
  pushIncShift                               // increment objective function constant term
  (const CostSet&    shiftValues);           // all commitment modes

  virtual
  const int                                  // the col index just employed
  pushObj
  (const double      objValue,               // for non-short-run commitment
   const CostSet&    specCosts,
   const std::string tag = "");

  virtual
  const int                                  // the col index just employed
  pushObj
  (const CostSet&    specCosts,
   const std::string tag = "");

  virtual
  const double                               // return the previous value, now overwritten
  repushObj
  (const int         col,
   const double      objValue,
   const CostSet&    specCosts);

  virtual
  const double                               // return the previous value, now overwritten
  repushObj
  (const int         col,
   const CostSet&    specCosts);

  // non-virtual

  const int                                  // the same col index for convenience
  markBinary                                 // in set { 0, 1 }
  (const int col);

  const int                                  // the same col index for convenience
  markInteger
  (const int col);

  const int                                  // the row index just employed
  pushRhs
  (const double                rhsValue,
   const svif::ConstraintSense conSense,     // { svif::L, E, G, R, N } from unit 'siglp'
   const std::string           tag = "");    // passed thru to solver

  const int                                  // number of local nonzero structural coeffs
  pushCof
  (const int    row,
   const int    col,
   const double coeffValue);

  const int                                  // always zero, 'd_cofCount' not incremented
  pushGof                                    // like 'pushCof' but with global row and col
  (const int    gow,                         // global row
   const int    gol,                         // global col
   const double coeffValue);

  int                                        // number of bound changes {0, 1, 2}
  openBnds                                   // set to [-inf, +inf]
  (const int    col);

  int                                        // number of bound changes {0, 1, 2}
  changeBnds                                 // CAUTION: not expected to be used
  (const int    col,
   const double lowerBnd,
   const double upperBnd);

  void
  resetBnds                                  // reset to defaults, see unit 'd/siglpk'
  (const int    col);

  // CALLS WHICH TRANSLATE OSP INDEXES

  // int variants

  const int
  globalcol                                  // add 'd_colStart'
  (const int localCol) const;

  const int
  globalrow                                  // add 'd_rowStart'
  (const int localRow) const;

  const int
  localcol                                   // subtract 'd_colStart'
  (const int globalCol) const;

  const int
  localrow                                   // subtract 'd_rowStart'
  (const int globalRow) const;

  // tuple variants covering one to nine indexes

  boost::tuple<int>
  globalcols(boost::tuple<int>) const;

  boost::tuple<int, int>
  globalcols(boost::tuple<int, int>) const;

  boost::tuple<int, int, int>
  globalcols(boost::tuple<int, int, int>) const;

  boost::tuple<int, int, int, int>
  globalcols(boost::tuple<int, int, int, int>) const;

  boost::tuple<int, int, int, int, int>
  globalcols(boost::tuple<int, int, int, int, int>) const;

  boost::tuple<int, int, int, int, int, int>
  globalcols(boost::tuple<int, int, int, int, int, int>) const;

  boost::tuple<int, int, int, int, int, int, int>
  globalcols(boost::tuple<int, int, int, int, int, int, int>) const;

  boost::tuple<int, int, int, int, int, int, int, int>
  globalcols(boost::tuple<int, int, int, int, int, int, int, int>) const;

  boost::tuple<int, int, int, int, int, int, int, int, int>
  globalcols(boost::tuple<int, int, int, int, int, int, int, int, int>) const;

  // PROBLEM GET CALLS - can be made anytime but returned value
  // may not be complete

  CostSet
  getShift() const;                          // current objective function "shift" term

  std::vector<CostSet>
  getObjs() const;

  // SOLUTION RECOVERY CALLS - note the individual and vector versions

public:

  // each recovery call tests "d_solver->isUsableSoln() == true"
  // and will complain and react accordingly

  double
  downloadVar                                // col primal value for underlying LP
  (const int col) const;

  std::vector<double>
  downloadVars() const;

  const bool                                 // 'true' means successful calculation
  downloadVarCosts                           // excludes "shift" term
  (CostSet& varCosts) const;

  const bool                                 // 'true' means successful calculation
  downloadFixCosts                           // supplies "shift" term
  (CostSet& fixCosts) const;

  CostSet
  downloadVarCosts() const;                  // excludes "shift" term

  CostSet
  downloadFixCosts() const;                  // supplies "shift" term

  // CAUTION: note possibly counter-intuitive order below: var then fix

  const bool                                 // 'true' means successful calculation
  downloadShortrunCosts                      // pass-by-reference form and also workhorse
  (CostSet& varCosts,                        // identical to 'downloadVarCosts'
   CostSet& fixCosts) const;                 // assign "shift" term

  boost::tuple
  <CostSet,                                  // "var" costs
   CostSet>                                  // "fix" costs
  downloadShortrunCosts() const;             // tuple form and also wrapper

  double
  downloadSlack                              // row dual value for underlying LP
  (const int row) const;

  std::vector<double>
  downloadSlacks() const;                    // slack values

  // UTILITY FUNCTIONS

protected:

  double
  getInf() const
  {
    return std::numeric_limits<double>::infinity();    // refer <limits>
  }

  double
  getNaN() const
  {
    return std::numeric_limits<double>::quiet_NaN();   // refer <limits>
  }

#if 0 // one answer to the question below

  // INSTANCE AND STATIC RESET CALLS
  // PONDER: which of these reset calls are needed?

protected:

  void
  resetSolver                                // will affect all other solver users
  (const std::string label = "");            // solver (and not sub-problem) label

  static
  void
  resetSolver                                // will affect all other solver users
  (shared_ptr<svif::SolverIf> solver,
   const std::string          label = "");   // solver (and not sub-problem) label

#endif // 0

  // INSTANCE DATA

protected:

  std::string            d_label;            // optional OSP label (for tags)
  const std::string      d_ospDesc;          // optional OSP description (for logs)

protected:

  // CAUTION: indexing: the sub-problem uses one-based indexing
  // for rows and cols while the various objective coefficient
  // vectors use zero-based indexing -- code utilizing the
  // objective vectors is not common, but do pay attention for
  // off-by-one errors!

  const int              d_rowStart;         // global and acquired from solver
  const int              d_colStart;

  int                    d_rowCount;         // local, initially zero
  int                    d_colCount;
  int                    d_cofCount;         // local nonzero structural coefficients

  int                    d_colTrack;         // local, ad-hoc index store, initially zero
  int                    d_rowTrack;

  // these containers are not necessarily used by all the various
  // sub-classes but they are all placed in the base class -- if
  // need be, some can be moved down the inheritance hierarchy
  // (although there is no compelling reason to do so)

  std::vector<double>    d_finObjs;          // also specific costs, note zero-based index
  std::vector<double>    d_ghgObjs;
  std::vector<double>    d_noxObjs;
  std::vector<double>    d_depObjs;
  std::vector<double>    d_lucObjs;

  std::vector<double>    d_bidObjs;          // bids
  std::vector<double>    d_mitObjs;          // merit order
  std::vector<double>    d_nulObjs;          // first feasible (always zero)

  // CAUTION: 'd_commitmentMode' and 'd_solver' should only be
  // used by protected 'push' calls and not by public
  // 'uploadProblem' calls -- unfortunately this requirement is
  // not enforced

  const xeona::DomainMode       d_commitmentMode;      // implies a cost type too
  shared_ptr<svif::SolverIf>    d_solver;              // interface to GLPK solver

  // STATIC DATA

protected:

  static logga::spLogger    s_logger;        // shared_ptr to single logger object

}; // class 'OptimSubProb'

// ---------------------------------------------------------
//  CLASS           : CouplingOsp
// ---------------------------------------------------------
//  Description  : concrete class for "coupling" two OSPs
//  Role         : used in 'constrain' calls from 'AssetOperator' instances and similar
//  Techniques   : concrete
//  Status       : complete
//  See also     : free function 'couple' (below)
// ---------------------------------------------------------

class CouplingOsp :
  public OptimSubProb
{
  // CREATORS

public:

  CouplingOsp
  (shared_ptr<svif::SolverIf> solver);

  virtual
  ~CouplingOsp();

  // PUBLIC FUNCTIONS

public:

  bool                                       // 'true' if coupling constraint added
  coupleGols
  (const int         golA,
   const int         golB,
   const std::string tag = "");

}; // class 'CouplingOsp'

// ---------------------------------------------------------
//  FREE FUNCTION   : xeona::couple
// ---------------------------------------------------------
//  Description  : "couple" two OSPs conveniently
//  Role         : used in 'constrain' calls from 'AssetOperator' instances and similar
//  Techniques   : free function wrapper to 'Coupling::coupleGols'
//  Status       : complete
// ---------------------------------------------------------

namespace xeona
{
  bool                                       // 'true' if coupling constraint added
  couple
  (shared_ptr<svif::SolverIf> solver,
   const int                  golA,          // usually from { ops }
   const int                  golB,          // usually from { ctl connection }
   const std::string          tag = "");

} // namespace 'xeona'

// ---------------------------------------------------------
//  CLASS           : ConnectionOsp
// ---------------------------------------------------------
//  Description  : concrete class for "connecting" block interfaces in an OSP context
//  Role         : used in 'constrain' calls from 'TechnicalAsset' instances and similar
//  Techniques   : concrete
//  Status       : complete
// ---------------------------------------------------------

class ConnectionOsp :
  public OptimSubProb
{
  // CREATORS

public:

  ConnectionOsp();                           // zero-argument constructor

  ConnectionOsp
  (shared_ptr<svif::SolverIf> solver,
   const int                  passCount = 0);

  virtual
  ~ConnectionOsp();

  // PUBLIC FUNCTIONS

public:

  int                                        // revised value
  incPassCount                               // increment pass count, starting zero
  (shared_ptr<svif::SolverIf> solver);

  int                                        // current value
  getPassCount() const;

  void
  resetPassCount();                          // reset to zero

  void
  storeGols
  (const std::vector<int> gols);

  std::vector<int>
  recoverGols() const;

  bool                                       // 'true' if all connection constraints added
  bindGols
  (const std::vector<int> gols1,
   const std::vector<int> gols2,
   const std::string      tag = "");

  // UTILITY FUNCTIONS

private:

  bool                                       // 'true' if connection constraint added
  bindGolz                                   // was 'bindGols' and is still the workhorse
  (const int         gol1,
   const int         gol2,
   const std::string tag = "");

  bool                                       // 'false' if problem detected
  checkSolverConsistency                     // used by 'incPassCount'
  (const int                  passCount,
   shared_ptr<svif::SolverIf> solver,
   std::string&               info);         // processing information for local use

  // INSTANCE DATA

private:

  std::vector<int>    d_gols;                // stored global cols
  int                 d_passCount;           // pass count in { 0, 1, 2 }
  std::string         d_lastSolverAddress;   // determined by stream insertion

}; // class 'ConnectionOsp'

// ---------------------------------------------------------
//  CLASS           : ControlOsp (abstract)
// ---------------------------------------------------------
//  Description  : abstract class covering penalty-based control (PBC)
//  Role         : parent for concrete "Ctl" sub-classes
//  Techniques   : pure virtual destructor
//  Status       : complete
// ---------------------------------------------------------

class ControlOsp :
  public OptimSubProb
{
  // CREATORS

public:

  ControlOsp
  (shared_ptr<svif::SolverIf> solver,
   const xeona::DomainMode    commitmentMode,
   const xeona::DomainMode    commitmentModeSum,
   const std::string&         ospDesc = "(not overwritten)");

  virtual
  ~ControlOsp() = 0;                         // create abstract class

  // FILL PROBLEM CALLS

public:

  void
  setFloorDuty
  (const double mustRunDuty,
   const int    teasDutyCol);

  void
  setCeilingDuty
  (const double maxDuty,
   const int    teasDutyCol);

  // INSTANCE DATA

protected:

  int    d_dutyCol;                          // local col

}; // class 'ControlOsp'

// ---------------------------------------------------------
//  CLASS           : OperationsOsp (abstract)
// ---------------------------------------------------------
//  Description  : abstract class covering operations
//  Role         : parent for concrete "Ops" sub-classes
//  Techniques   : pure virtual destructor
//  Status       : complete
// ---------------------------------------------------------

class OperationsOsp :
  public OptimSubProb
{
  // CREATORS

public:

  OperationsOsp
  (shared_ptr<svif::SolverIf> solver,
   const xeona::DomainMode    commitmentMode,
   const std::string&         ospDesc = "(not overwritten)");

  virtual
  ~OperationsOsp() = 0;                      // create abstract class

  // PROBLEM BUILDING CALLS (and similar)

public:

  virtual
  void
  uploadShortrunCosts                        // specific costs
  (const CostSet& dutySpecCosts,
   const int      gol);                      // zero means overwrite "shift" term

  virtual
  void
  uploadShortrunCosts                        // specific costs, increment "shift" term
  (const CostSet& shiftCosts);

  // SOLUTION RECOVERY CALLS - general

  virtual
  const double
  downloadSolnVar
  (const int gol) const;

  virtual
  const double
  downloadSolnSlack
  (const int gow) const;

  virtual
  const bool                                 // 'false' means not selected or tripped
  downloadRunStatus
  (const int gol) const;                     // trip global column

}; // class 'OperationsOsp'

// ---------------------------------------------------------
//  CLASS           : QuantityOsp (abstract)
// ---------------------------------------------------------
//  Description  : abstract class covering gateway quantities
//  Role         : parent for concrete "Qan" sub-classes
//  Techniques   : pure virtual destructor
//  Status       : complete
// ---------------------------------------------------------

class QuantityOsp :
  public OptimSubProb
{
  // CREATORS

public:

  QuantityOsp
  (shared_ptr<svif::SolverIf> solver,
   const xeona::DomainMode    commitmentMode,
   const std::string&         ospDesc = "(not overwritten)");

  virtual
  ~QuantityOsp() = 0;                        // create abstract class

  // PROBLEM BUILDING CALLS (and similar)

public:

  virtual
  void
  uploadShortrunCosts                        // specific costs
  (const CostSet& dutySpecCosts,
   const int      gol);                      // zero means overwrite "shift" term

  virtual
  void
  uploadShortrunCosts                        // specific costs, increment "shift" term
  (const CostSet& shiftCosts);

}; // class 'QuantityOsp'

// ---------------------------------------------------------
//  CLASS           : OfferOsp (abstract)
// ---------------------------------------------------------
//  Description  : abstract class covering gateway offers
//  Role         : parent for concrete "Ofr" sub-classes
//  Techniques   : pure virtual destructor
//  Status       : complete
// ---------------------------------------------------------

class OfferOsp :
  public OptimSubProb
{
  // CREATORS

public:

  OfferOsp
  (shared_ptr<svif::SolverIf> solver,
   const xeona::DomainMode    commitmentMode,
   const std::string&         ospDesc = "(not overwritten)");

  virtual
  ~OfferOsp() = 0;                           // create abstract class

  // MANIPULATORS

public:

  virtual
  boost::tuple
  <int>                                      // sale gol, connects to interface
  uploadTariffSet
  (const shared_ptr<BandedTariffSet> tariffset,
   const double                      capacity) = 0;

}; // class 'OfferOsp'

// ---------------------------------------------------------
//  CLASS           : JunctionOsp (abstract)
// ---------------------------------------------------------
//  Description  : abstract class for supporting demand splitting and joining
//  Role         : parent for concrete "Jnc" sub-classes
//  Techniques   : abstract
//  Status       : complete
//
//  Design notes
//
//      Junctions necessarily lack exergy destruction or cost
//      formation.
//
// ---------------------------------------------------------

class JunctionOsp :
  public OptimSubProb
{
  // CREATORS

public:

  JunctionOsp
  (shared_ptr<svif::SolverIf> solver,
   const xeona::DomainMode    commitmentMode,
   const std::string&         ospDesc = "(not overwritten)");

  virtual
  ~JunctionOsp() = 0;                        // create abstract class

}; // class 'JunctionOsp'

// ---------------------------------------------------------
//  CLASS           : LmpNodeOsp (abstract)
// ---------------------------------------------------------
//  Description  : abstract class for supporting LMP nodes
//  Role         : parent for concrete 'LmpCab*' 'LmpSoc*' 'LmpNul*' sub-classes
//  Techniques   : abstract
//  Status       : complete
//
//  Design notes
//
//      LMP nodes necessarily lack exergy destruction or cost
//      creation.
//
// ---------------------------------------------------------

class LmpNodeOsp :
  public OptimSubProb
{
  // CREATORS

public:

  LmpNodeOsp
  (shared_ptr<svif::SolverIf> solver,
   const xeona::DomainMode    commitmentMode,
   const std::string&         ospDesc = "(not overwritten)");

  virtual
  ~LmpNodeOsp() = 0;                         // create abstract class

}; // class 'LmpNodeOsp'

// ---------------------------------------------------------
//  CLASS           : Lmp2NodeOsp (abstract)
// ---------------------------------------------------------
//  Description  : abstract class for supporting LMP AC nodes
//  Role         : parent for concrete 'Lmp2Cab*' 'Lmp2Soc*' 'Lmp2Nul*' sub-classes
//  Techniques   : abstract, support for voltage angle theta
//  Status       : complete
//
//  Design notes
//
//      LMP nodes necessarily lack exergy destruction or cost
//      creation.
//
//      This particular LMP node OSP contains support for the AC
//      voltage angle theta.  In particular, 'pinTheta' allows
//      theta to be fixed.
//
//      The host is responsible for making sure ONLY ONE NODE in
//      the network is bound thus -- otherwise flows on some
//      parts of the network will be impossible or severely
//      restricted.
//
//      The 'd_thetaGol' will need to be UPDATED prior to a call
//      to function 'pinTheta'.
//
// ---------------------------------------------------------

class Lmp2NodeOsp :
  public LmpNodeOsp
{
  // CREATORS

public:

  Lmp2NodeOsp
  (shared_ptr<svif::SolverIf> solver,
   const xeona::DomainMode    commitmentMode,
   const std::string&         ospDesc = "(not overwritten)");

  virtual
  ~Lmp2NodeOsp() = 0;                        // create abstract class

  void
  pinTheta
  (const double thetaDegrees);               // reference voltage angle (usually zero)

protected:

  int    d_thetaGol;                         // voltage angle global col

}; // class 'Lmp2NodeOsp'

#endif // _OPTPROB_H_

//  end of file

