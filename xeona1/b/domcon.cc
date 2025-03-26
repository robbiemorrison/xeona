//  XEONA ENERGY SYSTEMS MODELING ENVIRONMENT
//
//  file-create-name : domcon.cc
//  file-create-date : Thu 07-Aug-2008 20:51 UTC
//  file-initiator   : Robbie Morrison <robbie@actrix.co.nz>
//
//  file-role        : domain controller entity / implementation
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
//  $URL: file:///home/robbie/svn-root/xeona/futz/trunk/xeona1/b/domcon.cc $

//  LOCAL AND SYSTEM INCLUDES

#include "domcon.h"           // companion header for this file (place first)

#include "../f/ospmodes.h"    // domain mode enums (header only)
#include "../d/siglp.h"       // semi-intelligent interface to GLPK MILP solver
#include "../c/recset.h"      // records and fields and also record-sets
#include "../b/teas.h"        // technical asset entity
#include "../b/node.h"        // LMP node entity
#include "../b/junc.h"        // demand split/join junction entity
#include "../b/gate.h"        // gateway entity
#include "../b/entity.h"      // entity base class plus lazy linking
#include "../b/block.h"       // abstract block entity
#include "../b/asop.h"        // asset operator entity
#include "../a/helpers.h"     // application helper functions

#include "../a/logger.h"      // standard logging functionality (as required)
#include "../c/smart_ptr.h"   // toggle between Boost and TR1 smart pointers
#include ".././common.h"      // common definitions for project (place last)

#include <deque>              // STL sequence container, double-ended vector
#include <sstream>            // string-streams
#include <string>             // C++ strings
#include <utility>            // STL pair, make_pair()

#include <boost/foreach.hpp>            // BOOST_FOREACH iteration macro
#include <boost/format.hpp>             // printf style formatting
#include <boost/tuple/tuple.hpp>        // n-tuples, ref(), cref()

//  CODE

// ---------------------------------------------------------
//  FREE FUNCTION   : ::visualize
// ---------------------------------------------------------
//  Description  : create and display solver interface as HTML
//  Role         : currently called within 'DomainController::transolve' under "--yeek 3"
//  Techniques   : optional one pass flag, 'GlpkViz' object
//  Status       : complete
//
//  Typical usage
//
//      ::visualize(d_solver, __func__, step);
//
//      where '__func__' is a compiler macro
//
// ---------------------------------------------------------

namespace
{
  void
  visualize
  (const shared_ptr<svif::SolverIf> solver,
   const std::string                caller,  // recommended to use __func__
   const int                        step)    // from 0 upward
  {
    // static variables
    static bool firstPass         = true;
    static logga::spLogger logger = logga::ptrLogStream();  // bind logger

    // initial reporting
    logger->repx(logga::adhc, "entering free function, first pass", firstPass);

    // one pass protection
    if ( firstPass ) firstPass = false;      // toggle the first pass flag
#if 0 // 0 = multiple visualization calls, 1 = one visualization call only
    else             return;
#endif // 0

    // active code
    logger->repx(logga::dbug, "will create and use GlpkViz object", caller);
    std::ostringstream oss;
    oss << solver->getObjectiveSenseStr() << " problem ";   // "maximize" or "minimize"
    if ( ! caller.empty() ) oss << "from function '" << caller << "'" << " at ";
    oss << "step " << step;

    // GlpkViz object
    GlpkViz webbrowse;                       // accept default browser from unit 'common'
    webbrowse(solver,                        // shared pointer (raw pointer also okay)
              oss.str(),                     // comment passed to web page
              4);                            // output precision

    // completion reporting
    logger->repx(logga::adhc, "leaving free function", "");
    // 'webbrowse' will shortly go out of scope

  } // free function 'xeona::visualize'

} // unnamed namespace

// ---------------------------------------------------------
//  FREE FUNCTION   : ::writeProblemInstance
// ---------------------------------------------------------
//  Description  : write out problem (perhaps CPLEX format) and solution (if appropriate)
//  Role         : currently called within 'DomainController::transolve' under "--yeek 4"
//  Techniques   : one pass flag, 'svif::SolverIf::printInfo'
//  Status       : complete
//
//  Typical usage
//
//      ::writeProblemInstance(d_solver, __func__, step);
//
//  Design notes
//
//      Refer to the function 'svif::Solver::printInfo' for
//      information about the exact action.
//
// ---------------------------------------------------------

namespace
{
  void
  writeProblemInstance
  (const shared_ptr<svif::SolverIf> solver,
   const std::string                caller,  // recommended to use __func__
   const int                        step)
  {
    // static variables
    static bool firstPass         = true;
    static logga::spLogger logger = logga::ptrLogStream();  // bind logger

    // initial reporting
    logger->repx(logga::adhc, "entering free function, first pass", firstPass);

    // one pass protection
    if ( firstPass ) firstPass = false;      // toggle the first pass flag
#if 0 // 0 = multiple write problem calls, 1 = one write problem call only
    else             return;
#endif // 0

    // active code
    logger->repx(logga::dbug, "will write GLPK problem instance", caller);
    solver->writeInfo();                     // see unit 'siglp' for details
    logger->repx(logga::adhc, "leaving free function", "");

  } // free function 'xeona::writeProblemInstance'

} // unnamed namespace

// ---------------------------------------------------------
//  FREE FUNCTION   : ::lowestNotGateway
// ---------------------------------------------------------
//  Description  : traverse buy or selgates and apply function 'func'
//  Role         : called by 'lowest*' functions
//  Techniques   : takes a callback function
//  Status       : complete
//
//  Design notes
//
//      Usage (two statements are recommended)
//
//          funcPtrVoid f = &Gateway::getHash;     // for instance
//          return ::lowestGateway(d_randomBuygates, f, __func__);
//
//  CAUTION: pointer function call syntax
//
//      The following syntax should work for raw pointers, but
//      would NOT with Boost.Smart_ptr smart pointers:
//
//          (gate->*func)()
//
//      The g++ 4.1.2 compiler chokes thus -- see the code proper
//      for a simple work around.
//
//  CAUTION: compiler support for BOOST_FOREACH
//
//      Regarding the local free functions '::lowestNotGateway'
//      and '::lowestGateway' and their traversals.
//
//      The Boost.Foreach macro 'BOOST_FOREACH' may be used to
//      iterate over an expression that returns a sequence by
//      value (that is, an rvalue).  However this can break older
//      compilers -- in which case make 'xeona::reverseSequence'
//      a stand-alone call.  Consult the portability section of
//      Boost.Foreach to see whether your compiler is listed as
//      problematic.
//
//      The foreach loop is never entered if the container is
//      empty, hence all shared_ptr's remain empty.
//
// ---------------------------------------------------------

#define XE_FORWARD 1 // 0 = reverse sequence, 1 = normal order

// The above preprocessor macro applies to the next two free
// functions.  It was added in commit r4563 for experimentation
// purposes and should be removed when the code is stable.

typedef bool (Gateway::* funcPtrVoid)() const;

// The above is a pointer-to-member function type, see Stroustrup
// (1997 p419), Stephens etal (2006 pp539-541), Loudon (2003
// pp26-27).  This typedef makes the subsequent syntax easier.

namespace
{
  shared_ptr<Gateway>
  lowestNotGateway
  (const std::vector<shared_ptr<Gateway> >& gateways,
   funcPtrVoid                              func,
   std::string                              caller)
  {
    // logger
    static logga::spLogger logger = logga::ptrLogStream();

    // active code
    std::string gateId = "(nonexistent)";
    shared_ptr<Gateway> returnme;            // empty smart pointer

#if (XE_FORWARD == 0)
    BOOST_FOREACH( shared_ptr<Gateway> gate,
                   xeona::reverseSequence(gateways) )
#elif (XE_FORWARD == 1)
    BOOST_FOREACH( shared_ptr<Gateway> gate,
                   gateways )
#endif // XE_FORWARD

      {
        if ( ! ((*gate).*func)() )           // CAUTION: syntax is correct, see above
          {
            returnme = gate;
            gateId   = gate->getIdAndKind();
            break;
          }
      }

    // final reporting
    logger->repx(logga::adhc, "caller: " + caller, gateId);

    // return
    return returnme;
  }

} // unnamed namespace

// ---------------------------------------------------------
//  FREE FUNCTION   : ::lowestGateway
// ---------------------------------------------------------
//  Description  : traverse buy or selgates and apply function 'func'
//  Role         : called by 'lowest*' functions
//  Techniques   : takes a callback function
//  Status       : complete
//
//  Design notes
//
//      See comments in '::lowestNotGateway'.
//
// ---------------------------------------------------------

namespace
{

  shared_ptr<Gateway>
  lowestGateway
  (const std::vector<shared_ptr<Gateway> >& gateways,
   funcPtrVoid                              func,
   std::string                              caller)
  {
    // logger
    static logga::spLogger logger = logga::ptrLogStream();

    // active code
    std::string gateId = "(nonexistent)";
    shared_ptr<Gateway> returnme;            // empty smart pointer

#if (XE_FORWARD == 0)
    BOOST_FOREACH( shared_ptr<Gateway> gate,
                   xeona::reverseSequence(gateways) )
#elif (XE_FORWARD == 1)
    BOOST_FOREACH( shared_ptr<Gateway> gate,
                   gateways )
#endif // XE_FORWARD

      {
        if ( ((*gate).*func)() )             // CAUTION: syntax is correct, see above
          {
            returnme = gate;
            gateId   = gate->getIdAndKind();
            break;
          }
      }

    // final reporting
    logger->repx(logga::adhc, "caller: " + caller, gateId);

    // return
    return returnme;
  }

} // unnamed namespace

#undef XE_FORWARD

// ---------------------------------------------------------
//  CLASS           : TestForCostRegister (local class)
// ---------------------------------------------------------
//  Description  : screen for cost entities lacking 'CostRegister' inheritance
//  Role         : used in 'establishDomain' call
//  Techniques   : 'dynamic_pointer_cast'
//  Status       : complete
// ---------------------------------------------------------

class TestForCostRegister
{
  // DISABLED

private:

  TestForCostRegister(const TestForCostRegister& orig);              // copy constructor
  TestForCostRegister& operator= (const TestForCostRegister& orig);  // copy assign opor

  // CREATORS

public:

  TestForCostRegister() :
    d_failLog()                              // initially empty
  {
    s_logger->repx(logga::xtra, "constructor call", "");
  }

  // MANIPULATORS

  bool
  operator()
  (shared_ptr<Entity> entity)
  {
    // initial reporting
    s_logger->repx(logga::adhc, "entering member function", "");

    // check first for empty/null shared pointers
    if ( ! entity )
      {
        s_logger->repx(logga::warn, "empty/null pointer supplied", entity);
        return false;
      }

    // report
    s_logger->repx(logga::adhc,
                   "cost register testing cost entity",
                   entity->getIdAndKind());

    // attempt an upcast -- while noting a null pointer indicates failure
    if ( dynamic_pointer_cast<CostRegister>(entity) == 0 )
      {
        d_failLog.push_back(entity->getIdAndKind());
        return false;
      }
    else
      {
        return true;
      }
  }

  // ACCESSORS

  int
  size() const
  {
    return d_failLog.size();
  }

  bool                                       // 'true' means no issues detected
  okay() const
  {
    return d_failLog.size() == 0 ? true : false;
  }

  std::string
  say() const
  {
    std::ostringstream oss;
    BOOST_FOREACH( std::string s, d_failLog )
      {
        oss << "    " << s << "\n";
      }
    return oss.str();
  }

  // INTERNAL DATA

private:

  std::vector<std::string>    d_failLog;     // vector of collected identifiers
  static logga::spLogger      s_logger;      // shared_ptr to single logger object

};

logga::spLogger TestForCostRegister::s_logger = logga::ptrLogStream();  // bind logger

// ---------------------------------------------------------
//  CLASS           : DomainController
// ---------------------------------------------------------
//
//  CAUTION: cyclic gateway references at the close of 'main'
//
//      A subtle pointer bug was fixed for commit r5525
//      (22-Nov-2010).  This bug involved the non-destruction of
//      entities at the close of 'main' in models with more than
//      one domain and at least one gateway.  This end of
//      function boundary was supposed to be the big curtain call
//      for all entities -- with each entity destructing and
//      releasing its memory and, with suitable code, any other
//      system resources it was holding.  But if, at this point,
//      a domain controller referenced one or more gateways and
//      vice-versa, then cyclic referencing would occur.  And the
//      domain controller/gateway pair would remain intact, each
//      with non-zero reference counts and, with them, most of
//      the remaining entities.  'valgrind' would duly report
//      leaks (first with modest reporting, second with lots):
//
//        $ valgrind --tool=memcheck --leak-check=full
//        $ valgrind --tool=memcheck --leak-check=full --show-reachable=yes
//
//      The current solution is to:
//
//      * place gate census clear calls at the end of the
//        'DomainController::concludeDomain' function, namely:
//
//            d_rankedSelgates.clear()
//            d_randomBuygates.clear()
//
//      Note that adding the same clear calls to the
//      'DomainController::~DomainController' destructor doesn't
//      work, because these destructors are never called, d'oh!"
//
//      The given solution was tested on submodels { 16 17 18 19 }.
//
//      If this solution proves unsatisfactory, then the two
//      census vectors should hold 'weak_ptr's instead.  This
//      would clearly be a cleaner solution and perhaps should be
//      revisited in due course.
//
//      Another alternative might be to write a static function
//      named 'Entity::killAll' to kill all entities (given that
//      this is possible -- in needs thought) and then expressly
//      call this function.
//
// ---------------------------------------------------------

// CREATORS

// ---------------------------------------------------------
//  MEMBER FUNCTION : DomainController
// ---------------------------------------------------------

DomainController::DomainController
(const std::string entityId,
 Record&           record) :
  CostRegister(record),
  Actor(entityId, record),
  CostRegisterDomcon(record),
  d_domainRole(record.tieSingle<std::string>("domain-role")),
  d_initEmployScaling(record.tieSingle<bool>("init-scale-problem")),
  d_initEmployAdvIniBasis(record.tieSingle<bool>("init-use-advanced-initial-basis")),
  d_initSimplexPresolver(record.tieSingle<bool>("init-use-simplex-presolver")),
  d_initIntegerPresolver(record.tieSingle<bool>("init-use-mip-presolver")),
  d_initNumericalZero(record.tieSingle<bool>("init-apply-numerical-zero")),
  d_tripKktReportLevel(record.tieSingle<int>("trip-kkt-report-level")),
  d_tripCoeffSpanLevel(record.tieSingle<int>("trip-coeff-span-level")),
  d_commitment_mode(record.tieSingle<std::string>("commitment-mode")),
  d_commitmentMode(xeona::e_modeNotSpecified),    // will derive from above
  d_ranked_selgates(record.tieSingle<std::string>("ranked-selgates")),
  d_rankedSelgates(),
  d_asset_operators(record.tieSingle<std::string>("asset-operators")),
  d_assetOperators(),                             // empty vector
  d_demand_junctions(record.tieSingle<std::string>("demand-junctions")),
  d_demandJunctions(),
  d_step(-1),                                     // nonsensical value
  d_solver(),                                     // empty object
  d_randomBuygates(),                             // empty vector
  d_tildePushPop(),                               // empty vector
  d_hyphen(false)                                 // initialize
{
  // initial reporting
  s_logger->repx(logga::xtra, "constructor call", getIdAndKind());

  // integrity checks
  if ( d_tripKktReportLevel < 0 || d_tripKktReportLevel > 3 )
    {
      s_logger->repx(logga::warn, "trip not 0 1 2 3", d_tripKktReportLevel);
    }
  else
    {
      s_logger->repx(logga::adhc, "trip in range", d_tripKktReportLevel);
    }
  if ( d_tripCoeffSpanLevel < 0 || d_tripCoeffSpanLevel > 2 )
    {
      s_logger->repx(logga::warn, "trip not 0 1 2 3", d_tripCoeffSpanLevel);
    }
  else
    {
      s_logger->repx(logga::adhc, "trip in range", d_tripCoeffSpanLevel);
    }

  // builtin remark
  d_builtinRemark = "beta";
}

// ---------------------------------------------------------
//  MEMBER FUNCTION : ~DomainController
// ---------------------------------------------------------

DomainController::~DomainController()
{
  s_logger->repx(logga::adhc, "destructor call", getIdAndKind());
}

// ACCESSORS

// ---------------------------------------------------------
//  MEMBER FUNCTION : selgateCount
// ---------------------------------------------------------

int                                          // number of selgates (from input data)
DomainController::selgateCount() const
{
  return d_rankedSelgates.size();
}

// ---------------------------------------------------------
//  MEMBER FUNCTION : buygateCount
// ---------------------------------------------------------

int                                          // number of buygates (from DFS traversal)
DomainController::buygateCount() const
{
  return d_randomBuygates.size();
}

// OVERSEER POINTS OF ENTRY

// ---------------------------------------------------------
//  MEMBER FUNCTION : establishDomain
// ---------------------------------------------------------
//  Description  : point of entry for 'establish' calls
//  Role         : beginning of horizon code
//  Techniques   : 'listToVec<>' conversions
//  Status       : complete
//
//  Design notes
//
//      DFS traversal has run prior to this call.
//
//      Could perhaps move some initialization tasks to 'ctor',
//      but not the 'FullEntity::listToVec' calls.  Otherwise a
//      "hanging association/s found" is reported, followed by an
//      'xeona::xem_data_issue' exception.
//
// ---------------------------------------------------------

void
DomainController::establishDomain()
{
  s_logger->repx(logga::info, "entering member function", "DomainController");

  // ---------------------------------
  //  initializations
  // ---------------------------------

  // call order integrity check
  if ( d_step != -1 )
    {
      s_logger->repx(logga::warn, "d_step not -1 as expected", d_step);
    }

  // commitment mode string to enum mapping
  if      ( d_commitment_mode == "fin"   ) d_commitmentMode = xeona::e_shortrunFin;
  else if ( d_commitment_mode == "ghg"   ) d_commitmentMode = xeona::e_shortrunGhg;
  else if ( d_commitment_mode == "nox"   ) d_commitmentMode = xeona::e_shortrunNox;
  else if ( d_commitment_mode == "dep"   ) d_commitmentMode = xeona::e_shortrunDep;
  else if ( d_commitment_mode == "luc"   ) d_commitmentMode = xeona::e_shortrunLuc;
  else if ( d_commitment_mode == "lmp"   ) d_commitmentMode = xeona::e_auctionLmp;
  else if ( d_commitment_mode == "merit" ) d_commitmentMode = xeona::e_adminMerit;
  else if ( d_commitment_mode == "first" ) d_commitmentMode = xeona::e_adminFirst;
  else
    {
      s_logger->repx(logga::warn, "invalid commitment mode", d_commitment_mode);
      s_logger->repx(logga::info, "commitment mode remains unset", d_commitmentMode);
    }

  // report
  s_logger->repx(logga::dbug, "commitment mode set to", d_commitmentMode);

  // asset operators
  d_assetOperators.clear();
  const int asopCnt
    = FullEntity::listToVec<AssetOperator>(d_asset_operators, d_assetOperators);
  s_logger->repx(logga::adhc, "asset operators count", asopCnt);

  // demand junctions
  d_demandJunctions.clear();
  const int juncCnt
    = FullEntity::listToVec<DemandJunction>(d_demand_junctions, d_demandJunctions);
  s_logger->repx(logga::adhc, "demand junctions count", juncCnt);

  // ranked sell gates
  d_rankedSelgates.clear();
  const int selgateCnt
    = FullEntity::listToVec<Gateway>(d_ranked_selgates, d_rankedSelgates);
  s_logger->repx(logga::adhc, "ranked selgates count", selgateCnt);

  // ---------------------------------
  //  cost register reset
  // ---------------------------------

  CostRegister::resetRegister();             // reset entire register

  // ---------------------------------
  //  establish sweep (domain)
  // ---------------------------------

  // step thru asset operators in no required order
  BOOST_FOREACH( shared_ptr<AssetOperator> ao, d_assetOperators )
    {
      ao->establish();
    }

  // step thru demand junctions
  BOOST_FOREACH( shared_ptr<DemandJunction> dj, d_demandJunctions )
    {
      dj->establish();
    }

  // step thru selgates in rank order, as it happens
  BOOST_FOREACH( shared_ptr<Gateway> sg, d_rankedSelgates )
    {
      shared_ptr<SelSide> sel = dynamic_pointer_cast<SelSide>(sg);
      sel->establish();
    }

  // step thru buygates in arbitrary order
  BOOST_FOREACH( shared_ptr<Gateway> bg, d_randomBuygates )
    {
      shared_ptr<BuySide> buy = dynamic_pointer_cast<BuySide>(bg);
      buy->establish();
    }

  // ---------------------------------
  //  entity authorship integrity
  // ---------------------------------

  // CAUTION: this code requires that all cost entities inherit
  // from 'CostRegister' or one of its sub-classes

  // CAUTION: the following test really only makes sense during
  // development, hence the conditional block

  if ( xeona::releaseStatus == false )
    {

      TestForCostRegister testForCostRegister;    // functor declared and defined above

      // step thru asset operators in no required order
      BOOST_FOREACH( shared_ptr<AssetOperator> ao, d_assetOperators )
        {
          testForCostRegister(ao);
          // grab and step thru technical assets in no required order
          std::vector<shared_ptr<TechnicalAsset> > technicalAssets;
          technicalAssets = ao->getTechnicalAssets();
          BOOST_FOREACH( shared_ptr<TechnicalAsset> ta, technicalAssets )
            {
              testForCostRegister(ta);
            }
#if 0 // 0 = LMP nodes do not have cost registers, 1 = otherwise
          // grab and step thru LMP nodes in no required order
          std::vector<shared_ptr<LmpNode> > lmpNodes;
          lmpNodes = ao->getLmpNodes();
          BOOST_FOREACH( shared_ptr<LmpNode> ln, lmpNodes )
            {
              testForCostRegister(ln);
            }
#endif // 0
        }

#if 0 // 0 = gateways do not have cost registers, 1 = otherwise
      // step thru sell gates in rank order, as it happens
      BOOST_FOREACH( shared_ptr<Gateway> sg, d_rankedSelgates )
        {
          testForCostRegister(sg);
        }

      // step thru buy gates in arbitrary order
      BOOST_FOREACH( shared_ptr<Gateway> bg, d_randomBuygates )
        {
          testForCostRegister(bg);
        }
#endif // 0

      // respond appropriately if problems encountered
      if ( testForCostRegister.okay() == false )
        {
          // report
          const int fails = testForCostRegister.size();
          s_logger->repx(logga::warn, "cost entities without registers", fails);
          std::ostringstream put;
          put << "  list of cost entities lacking cost registers (authorship error):"
              << "\n";
          put << testForCostRegister.say();  // contains trailing newline
          s_logger->putx(logga::warn, put);

          // abandon as required
          if ( xeona::nopro == false )       // meaning option '--krazy' not applied
            {
              s_logger->repx(logga::warn, "will throw xeona::bad_authorship", "");
              throw xeona::bad_authorship(fails);
            }
        }

    } // 'xeona::releaseStatus' conditional

} // member function 'establishDomain'

// ---------------------------------------------------------
//  MEMBER FUNCTION : restructureDomain
// ---------------------------------------------------------
//  Description  : point of entry for 'restructure' calls
//  Role         : structure change code
//  Techniques   : (nothing special)
//  Status       : complete
// ---------------------------------------------------------

void
DomainController::restructureDomain()
{
  // at the time of writing, 'd_commitmentMode' should have been
  // set by 'establishDomain' so complain if not
  if ( d_commitmentMode == xeona::e_modeNotSpecified )
    {
      s_logger->repx(logga::warn, "commitment mode not set properly", d_commitmentMode);
    }

  // ---------------------------------
  //  restructure sweep (domain)
  // ---------------------------------

  // step thru asset operators in no required order
  BOOST_FOREACH( shared_ptr<AssetOperator> ao, d_assetOperators )
    {
      ao->restructure(d_commitmentMode);
    }

  // step thru demand junctions
  BOOST_FOREACH( shared_ptr<DemandJunction> dj, d_demandJunctions )
    {
      dj->restructure(d_commitmentMode);
    }

  // step thru selgates in rank order, as it happens
  BOOST_FOREACH( shared_ptr<Gateway> sg, d_rankedSelgates )
    {
      shared_ptr<SelSide> sel = dynamic_pointer_cast<SelSide>(sg);
      sel->restructure(d_commitmentMode);
    }

  // step thru buygates in arbitrary order
  BOOST_FOREACH( shared_ptr<Gateway> bg, d_randomBuygates )
    {
      shared_ptr<BuySide> buy = dynamic_pointer_cast<BuySide>(bg);
      buy->restructure(d_commitmentMode);
    }

} // member function 'restructureDomain'

// ---------------------------------------------------------
//  MEMBER FUNCTION : setStep
// ---------------------------------------------------------
//  Description  : simple set step call
//  Role         : beginning of loop code
//  Techniques   : (nothing special)
//  Status       : complete
// ---------------------------------------------------------

void
DomainController::setStep
(const int step)                             // from 'overseer'
{
  // initial reporting
  s_logger->repx(logga::adhc, "entering member function", getIdAndKind());

  // update interval count
  d_step = step;

  // reporting
  s_logger->repx(logga::adhc, "explicitly setting step", step);
}

// ---------------------------------------------------------
//  MEMBER FUNCTION : initializeDomain
// ---------------------------------------------------------
//  Description  : point of entry for 'initialize' calls
//  Role         : beginning of loop code
//  Techniques   : (nothing special)
//  Status       : complete
//
//  Design notes
//
//      Function 'TicToc::initialize' is virtual, but is only
//      redefined in a few special cases.
//
// ---------------------------------------------------------

void
DomainController::initializeDomain()
{
  // ---------------------------------
  //  initializations
  // ---------------------------------

  // CAUTION: note that each domain gets just one solver
  // interface pointer and that this pointer persists for the
  // entire the simulation -- notwithstanding the underlying
  // solver interface object is "reset" quite often

  // interpret the current reporting level for transfer to solver
  const int reportLevel                      = s_logger->getReportLevelInt();
  const svif::ReportingLevel svifReportLevel = xeona::logRankToGlpkLevel(reportLevel);

  // process the problem label (the short form is provided for convenience)
  const std::string longform   = "domain-controller-";
  const std::string shortform  = "dc-";
  const std::string identifier = getIdAndKind();
  const int len                = longform.length();
  std::string label            = identifier;
  if ( label.substr(0, len) == longform )
    {
      std::ostringstream put;
      put << "  longform (from the entity identifier specified in model file) : "
          << label << "\n";
      label.replace(0, len, shortform);      // replace call
      put << "  shortform (as revised here)                                   : "
          << label << "\n";
      s_logger->repx(logga::adhc, "revised domain label adopted", "");
      s_logger->putx(logga::adhc, put);
    }
  else
    {
      std::ostringstream put;
      put << "  longform  : " << label << "\n";
      s_logger->repx(logga::adhc, "original domain label retained", "");
      s_logger->putx(logga::adhc, put);
    }

  // reestablish the solver
  d_solver.reset(new svif::SolverIf(label,   // now calls 'setProblemLabel' internally
                                    svifReportLevel));

  // set the numerical zero tolerance for testing and reset
  // structural coefficients and variable values as required
  if ( d_initNumericalZero )
    {
      d_solver->setZeroTol(xeona::numericalZero,  // defined in "common.cc"
                           true,                  // please modify coefficients
                           true);                 // please modify values
    }
  else
    {
      d_solver->setZeroTol(xeona::numericalZero,  // defined in "common.cc"
                           false,                 // do not modify coefficients
                           false);                // do not modify values
    }

  // rework defaults as required
  if ( d_initEmployScaling     ) d_solver->initEmployScaling();
  if ( d_initEmployAdvIniBasis ) d_solver->initEmployAdvBasis();
  if ( d_initSimplexPresolver  ) d_solver->initSimplexPresolver();
  if ( d_initIntegerPresolver  ) d_solver->initIntegerPresolver();
  d_solver->initCoeffSpan(d_tripCoeffSpanLevel);

  // CAUTION: there is no need to set the objective function
  // direction and label at this stage -- instead rely on the
  // default settings (namely, 'svif::sense_not_specified' and
  // "" at the time of writing)

  // ---------------------------------
  //  initialize sweep (domain)
  // ---------------------------------

  // CAUTION: the 'TicToc::initialize' calls are polymorphic --
  // moreover, the technical assets are called by their
  // respective operators and not here

  // step thru asset operators in no required order
  BOOST_FOREACH( shared_ptr<AssetOperator> ao, d_assetOperators )
    {
      ao->initialize(d_step, d_solver);
    }

  // step thru demand junctions
  BOOST_FOREACH( shared_ptr<DemandJunction> dj, d_demandJunctions )
    {
      dj->initialize(d_step, d_solver);
    }

  // step thru selgates in rank order, as it happens
  BOOST_FOREACH( shared_ptr<Gateway> sg, d_rankedSelgates )
    {
      shared_ptr<SelSide> sel = dynamic_pointer_cast<SelSide>(sg);
      sel->SelSide::initialize(d_step, d_solver);
    }

  // step thru buygates in arbitrary order
  BOOST_FOREACH( shared_ptr<Gateway> bg, d_randomBuygates )
    {
      shared_ptr<BuySide> buy = dynamic_pointer_cast<BuySide>(bg);
      buy->BuySide::initialize(d_step, d_solver);
    }

} // member function 'initializeDomain'

// ---------------------------------------------------------
//  MEMBER FUNCTION : constrainDomain
// ---------------------------------------------------------

// see r3038 / 08-Jul-2009 for the loop code, but note that
// constraining domains is now part of the CTA process

// ---------------------------------------------------------
//  MEMBER FUNCTION : washupDomain
// ---------------------------------------------------------
//  Description  : point of entry for 'washup' calls
//  Role         : end of loop code
//  Techniques   : (nothing special)
//  Status       : complete
// ---------------------------------------------------------

void
DomainController::washupDomain()
{
  // ---------------------------------
  //  washup sweep (domain)
  // ---------------------------------

  // step thru asset operators in no required order
  BOOST_FOREACH( shared_ptr<AssetOperator> ao, d_assetOperators )
    {
      ao->washup();
    }

  // step thru demand junctions
  BOOST_FOREACH( shared_ptr<DemandJunction> dj, d_demandJunctions )
    {
      dj->washup();
    }

  // step thru sell gates in rank order, as it happens
  BOOST_FOREACH( shared_ptr<Gateway> sg, d_rankedSelgates )
    {
      shared_ptr<SelSide> sel = dynamic_pointer_cast<SelSide>(sg);
      sel->SelSide::washup();
    }

  // step thru buy gates in arbitrary order
  BOOST_FOREACH( shared_ptr<Gateway> bg, d_randomBuygates )
    {
      shared_ptr<BuySide> buy = dynamic_pointer_cast<BuySide>(bg);
      buy->BuySide::washup();
    }

} // member function 'washupDomain'

// ---------------------------------------------------------
//  MEMBER FUNCTION : consolidateDomain
// ---------------------------------------------------------
//  Description  : point of entry for 'consolidate' calls
//  Role         : end of loop code cost consolidation
//  Techniques   : (nothing special)
//  Status       : complete
// ---------------------------------------------------------

void
DomainController::consolidateDomain          // dedicated consolidate costs call
(const int   step,
 costs_type& overseerCosts)
{
  // initial reporting
  s_logger->repx(logga::adhc, "entering member function", getIdAndKind());

  // CAUTION: code previously run from within 'establishDomain'
  // has already screened for cost entities that lack cost
  // registers (an issue of entity authorship).

  // ---------------------------------
  //  import costs sweep (domain) (consolidateDomain)
  // ---------------------------------

  // step thru asset operators in no required order
  BOOST_FOREACH( shared_ptr<AssetOperator> ao, d_assetOperators )
    {
      // grab and step thru technical assets in no required order
      std::vector<shared_ptr<TechnicalAsset> > technicalAssets;
      technicalAssets = ao->getTechnicalAssets();
      BOOST_FOREACH( shared_ptr<TechnicalAsset> ta, technicalAssets )
        {
          // consolidate to asset operator
          ao->importCosts(step, ta->exportCosts(step)); // import employs "add-and-assign"
        }
      // no need to step thru LMP nodes

      // consolidate to domain controller (note the explicit this->)
      this->importCosts(step, ao->exportCosts(step));
    }

  // step thru sell gates in rank order, as it happens
  BOOST_FOREACH( shared_ptr<Gateway> sg, d_rankedSelgates )
    {
      shared_ptr<SelSide> sel = dynamic_pointer_cast<SelSide>(sg);
      // consolidate to domain controller (note the explicit this->)
      this->importCosts(step, sel->exportCosts(step)); // import employs "add-and-assign"
    }

  // step thru buy gates in arbitrary order
  BOOST_FOREACH( shared_ptr<Gateway> bg, d_randomBuygates )
    {
      shared_ptr<BuySide> buy = dynamic_pointer_cast<BuySide>(bg);
      // consolidate to domain controller (note the explicit this->)
      this->importCosts(step, buy->exportCosts(step)); // import employs "add-and-assign"
    }

  // ---------------------------------
  //    update my subtotals
  // ---------------------------------

  updateMySubtotals(step);

  // ---------------------------------
  //  update overseer costs
  // ---------------------------------

  addCosts(overseerCosts, exportCosts(step));

} // function 'DomainController::consolidateDomain'

// ---------------------------------------------------------
//  MEMBER FUNCTION : concludeDomain
// ---------------------------------------------------------
//  Description  : point of entry for 'conclude' calls
//  Role         : end of horizon code
//  Techniques   : (nothing special)
//  Status       : complete
// ---------------------------------------------------------

void
DomainController::concludeDomain()
{
  // ---------------------------------
  //  conclude sweep (domain)
  // ---------------------------------

  // step thru asset operators in no required order
  BOOST_FOREACH( shared_ptr<AssetOperator> ao, d_assetOperators )
    {
      ao->conclude();
    }

  // step thru demand junctions
  BOOST_FOREACH( shared_ptr<DemandJunction> dj, d_demandJunctions )
    {
      dj->conclude();
    }

  // step thru sell gates in rank order, as it happens
  BOOST_FOREACH( shared_ptr<Gateway> sg, d_rankedSelgates )
    {
      shared_ptr<SelSide> sel = dynamic_pointer_cast<SelSide>(sg);
      sel->conclude();
    }

  // step thru buygates in arbitrary order
  BOOST_FOREACH( shared_ptr<Gateway> bg, d_randomBuygates )
    {
      shared_ptr<BuySide> buy = dynamic_pointer_cast<BuySide>(bg);
      buy->conclude();
    }

  // ---------------------------------
  //  block stats sweep (domain)
  // ---------------------------------

  // code execution based on current 'logga::Rank' or
  // 'xeona::yeek' values

  const logga::Rank currentRank = s_logger->getReportLevelRank();

  if ( currentRank >= logga::xtra
       ||
       xeona::yeek == 14                     // YEEK 14 CODE (set by '--yeek')
       ||
       xeona::yeek == 1 || xeona::yeek == 2 )
    {
      std::ostringstream put;

      const int indent = 4;                  // value used locally only,
                                             // should align with 'Block::s_indent'
      put << "  block summary" << "\n";

      put << std::string(indent, ' ')
          << "domain = "         << getIdAndKind()
          << "   steps = "       << Entity::getHorizonSteps()
          << "   interval = "    << Entity::getHorizonInterval() << " s"  // 's' = second
          << "\n";

      put << Block::formatStatsHeader();     // includes trailing newline

      // step thru asset operators in no required order
      BOOST_FOREACH( shared_ptr<AssetOperator> ao, d_assetOperators )
        {
          // grab and step thru technical assets in no required order
          std::vector<shared_ptr<TechnicalAsset> > technicalAssets;
          technicalAssets = ao->getTechnicalAssets();
          BOOST_FOREACH( shared_ptr<TechnicalAsset> ta, technicalAssets )
            {
              put << ta->formatStats("tech");
            }
          // grab and step thru LMP nodes in no required order
          std::vector<shared_ptr<LmpNode> > lmpNodes;
          lmpNodes = ao->getLmpNodes();
          BOOST_FOREACH( shared_ptr<LmpNode> ln, lmpNodes )
            {
              put << ln->formatStats("node");
            }
        }

      // step thru demand junctions
      BOOST_FOREACH( shared_ptr<DemandJunction> dj, d_demandJunctions )
        {
          put << dj->formatStats("junc");
        }

      // step thru sell gates (only) in rank order, as it happens
      BOOST_FOREACH( shared_ptr<Gateway> sg, d_rankedSelgates )
        {
          put << sg->formatStats("gate");
        }

      put << Block::formatStatsFooter();     // includes trailing newline

      // CAUTION; note use of 'currentRank'
      s_logger->repx(logga::info, "block duty/size reporting follows", "");
      s_logger->addSmartBlank(currentRank);
      s_logger->putx(currentRank, put);
    }

  // ---------------------------------
  //  clear gateway inventories
  // ---------------------------------

  // clear out gateway inventories on 'conclude', see the class
  // 'DomainController' documentation in this file for more
  // details

  d_rankedSelgates.clear();                  // holds 'shared_ptr's
  d_randomBuygates.clear();                  // holds 'shared_ptr's

} // member function 'concludeDomain'

// REGISTER GATEWAYS FUNCTION POINTS OF ENTRY

// the following three 'map*To*' calls are invoked once and once
// only by '::registerGateways'

// ---------------------------------------------------------
//  MEMBER FUNCTION : mapGatesToDomain
// ---------------------------------------------------------

// repackaging of 'd_rankedSelgates'

typedef std::map<shared_ptr<Gateway>,
                 shared_ptr<DomainController> > mapGD_type;

mapGD_type
DomainController::mapGatesToDomain()
{
  // initial reporting
  s_logger->repx(logga::dbug, "entering member function", "");

  // identify myself
  shared_ptr<DomainController const> meconst = this->retMe<DomainController>(); // [1]
  shared_ptr<DomainController>       me = const_pointer_cast<DomainController>(meconst);
  // [1] CAUTION: template instantiation must be explicit

  // declare buffer
  mapGD_type buffer;

  // process ranked selgates
  d_rankedSelgates.clear();
  FullEntity::listToVec<Gateway>(d_ranked_selgates, d_rankedSelgates);
  BOOST_FOREACH( shared_ptr<Gateway> gate, d_rankedSelgates )
    {
      if ( ! buffer.insert(std::make_pair(gate, me)).second )
        {
          s_logger->repx(logga::warn,
                         "gate insertion unexpectedly failed",
                         gate->getIdAndKind());
        }
    }

  // return
  return buffer;
}

// ---------------------------------------------------------
//  MEMBER FUNCTION : mapGatesToBlocks
// ---------------------------------------------------------

// working "up-demand" (often portrayed as leftwards)

typedef std::map<shared_ptr<Gateway>,
                 shared_ptr<Block> > mapGB_type;

mapGB_type
DomainController::mapGatesToBlocks()
{
  // initial reporting
  s_logger->repx(logga::dbug, "entering member function", "");

  // declare buffer
  mapGB_type buffer;

  // process ranked selgates
  d_rankedSelgates.clear();
  FullEntity::listToVec<Gateway>(d_ranked_selgates, d_rankedSelgates);
  BOOST_FOREACH( shared_ptr<Gateway> gate, d_rankedSelgates )
    {
      shared_ptr<Block> block = gate->getDemander();   // in { gate, junc, node, teas }
      if ( ! buffer.insert(std::make_pair(gate, block)).second )
        {
          s_logger->repx(logga::warn,
                         "gate insertion unexpectedly failed",
                         gate->getIdAndKind());
        }
    }

  // return
  return buffer;
}

// ---------------------------------------------------------
//  MEMBER FUNCTION : mapBlocksToDomain
// ---------------------------------------------------------

// working "down-coordination" (portrayed as downwards)

typedef std::map<shared_ptr<Block>,
                 shared_ptr<DomainController> > mapBD_type;

mapBD_type
DomainController::mapBlocksToDomain()
{
  // initial reporting
  s_logger->repx(logga::dbug, "entering member function", "");

  // identify myself
  shared_ptr<DomainController const> meconst = this->retMe<DomainController>(); // [1]
  shared_ptr<DomainController>       me = const_pointer_cast<DomainController>(meconst);
  // [1] CAUTION: template instantiation must be explicit

  // declare buffer
  mapBD_type buffer;

  // loop demand junctions
  d_demandJunctions.clear();
  FullEntity::listToVec<DemandJunction>(d_demand_junctions, d_demandJunctions);
  BOOST_FOREACH( shared_ptr<DemandJunction> junc, d_demandJunctions )
    {
      if ( ! buffer.insert(std::make_pair(junc, me)).second )
        {
          s_logger->repx(logga::warn,
                         "junc insertion unexpectedly failed",
                         junc->getIdAndKind());
        }
    }

  // loop asset operators
  d_assetOperators.clear();
  FullEntity::listToVec<AssetOperator>(d_asset_operators, d_assetOperators);
  BOOST_FOREACH( shared_ptr<AssetOperator> asop, d_assetOperators )
    {
      // loop technical assets [1]
      BOOST_FOREACH( shared_ptr<TechnicalAsset> teas, asop->getTechnicalAssets() )
        {
          if ( ! buffer.insert(std::make_pair(teas, me)).second )
            {
              s_logger->repx(logga::warn,
                             "teas insertion unexpectedly failed",
                             teas->getIdAndKind());
            }
        }
      // loop lmp nodes [1]
      BOOST_FOREACH( shared_ptr<LmpNode> node, asop->getLmpNodes() )
        {
          if ( ! buffer.insert(std::make_pair(node, me)).second )
            {
              s_logger->repx(logga::warn,
                             "node insertion unexpectedly failed",
                             node->getIdAndKind());
            }
        }
    }

  // [1] the 'getTechnicalAssets' and 'getLmpNodes' calls now
  // also do 'listToVec' processing

  // return
  return buffer;
}

// ---------------------------------------------------------
//  MEMBER FUNCTION : addBuygate
// ---------------------------------------------------------

int                                          // number of buygates following insertion
DomainController::addBuygate
(shared_ptr<Gateway> buygate)
{
  if ( buygate )                             // not empty nor holding null pointer
    {
      d_randomBuygates.push_back(buygate);
      s_logger->repx(logga::adhc, "added gateway", buygate->getIdAndKind());
    }
  else
    {
      s_logger->repx(logga::warn, "attempt to add bad pointer", buygate);
    }
  return d_randomBuygates.size();
}

// ---------------------------------------------------------
//  MEMBER FUNCTION : retBuygates
// ---------------------------------------------------------

std::vector<shared_ptr<Gateway> >
DomainController::retBuygates()
{
  return d_randomBuygates;
}

// ---------------------------------------------------------
//  MEMBER FUNCTION : retSelgates
// ---------------------------------------------------------

std::vector<shared_ptr<Gateway> >
DomainController::retSelgates()
{
  return d_rankedSelgates;
}

// DOMAIN FUNCTIONS

// ---------------------------------------------------------
//  MEMBER FUNCTION : getHyphen
//  MEMBER FUNCTION : markHyphen
//  MEMBER FUNCTION : resetHyphen
// ---------------------------------------------------------

bool DomainController::getHyphen() const {                   return d_hyphen; }
bool DomainController::markHyphen()      { d_hyphen = true;  return d_hyphen; }
void DomainController::resetHyphen()     { d_hyphen = false;                  }

// GET GATEWAY FUNCTIONS

// ---------------------------------------------------------
//  MEMBER FUNCTION : lowestNoTildeSel
// ---------------------------------------------------------
//  Description  : 'tilde' is the selgate visited flag
//  Role         : depth first search traversal
//  Techniques   : (nothing special)
//  Status       : complete
// ---------------------------------------------------------

shared_ptr<Gateway>
DomainController::lowestNoTildeSel()
{
  // additional reporting as appropriate
  // YEEK 27 CODE (set by '--yeek')
  if ( xeona::yeek == 27 || xeona::yeek == 26 || xeona::yeek == 1 || xeona::yeek == 2 )
    {
      std::ostringstream put;
      put << "  " << "ranked selgates: tilde statuses" << "\n";
      BOOST_FOREACH( shared_ptr<Gateway> gate, d_rankedSelgates )
        {
          put << "    "
              << std::setw(30) << std::left
              << gate->getIdAndKind() << " "
              << std::boolalpha
              << gate->getTilde() << "\n";
        }
      s_logger->repx(logga::dbug, "additional reporting follows, yeek", xeona::yeek);
      s_logger->putx(logga::dbug, put);
    }

  // active code
  funcPtrVoid f = &Gateway::getTilde;
  return ::lowestNotGateway(d_rankedSelgates, f, __func__);
}

// ---------------------------------------------------------
//  MEMBER FUNCTION : pushTilde
// ---------------------------------------------------------
//  Description  : push a gateway into 'd_tildePushPop'
//  Role         : depth first search traversal
//  Techniques   : (nothing special)
//  Status       : complete
//
//  Design notes
//
//      Prior unit 'f/gatesreg', this function also used to fill
//      'd_randomBuygates'.  That functionality is now contained
//      in 'DomainController::addBuygate' called indirectly from
//      free function 'xeona::registerGates'.
//
// ---------------------------------------------------------

void
DomainController::pushTilde
(shared_ptr<Gateway> gate)
{
  // initial reporting
  s_logger->repx(logga::adhc, "entering member function", "");

  // active code
  d_tildePushPop.push_back(gate);
  s_logger->repx(logga::adhc, "pushing tilde gateway", gate->getIdAndKind());
}

// ---------------------------------------------------------
//  MEMBER FUNCTION : popTilde
// ---------------------------------------------------------
//  Description  : 'tilde' is the selgate visited flag
//  Role         : depth first search traversal
//  Techniques   : (nothing special)
//  Status       : complete
// ---------------------------------------------------------

shared_ptr<Gateway>                          // return empty on failure
DomainController::popTilde()
{
  // initial reporting
  s_logger->repx(logga::adhc, "entering member function", "");

  // active code
  shared_ptr<Gateway> gate;                  // empty shared pointer
  if ( d_tildePushPop.empty() )
    {
      s_logger->repx(logga::dbug,
                     "cannot pop tilde gateway (oft okay)",
                     d_tildePushPop.size());
    }
  else
    {
      gate = d_tildePushPop.back();          // grab last element
      d_tildePushPop.pop_back();             // remove last element
      s_logger->repx(logga::adhc,
                     "popping tilde gateway",
                     gate->getIdAndKind());
    }
  return gate;
}

// ---------------------------------------------------------
//  MEMBER FUNCTION : lowestNoThetaBuy
// ---------------------------------------------------------
//  Description  : 'theta' is the capacity back-track lock
//  Role         : CTA algorithm
//  Techniques   : (nothing special)
//  Status       : complete
// ---------------------------------------------------------

shared_ptr<Gateway>
DomainController::lowestNoThetaBuy()
{
  funcPtrVoid f = &Gateway::getTheta;
  return ::lowestNotGateway(d_randomBuygates, f, __func__);
}

// ---------------------------------------------------------
//  MEMBER FUNCTION : lowestNoStarBuy
// ---------------------------------------------------------
//  Description  : 'star' is the transaction forward-track lock
//  Role         : CTA algorithm
//  Techniques   : (nothing special)
//  Status       : complete
// ---------------------------------------------------------

shared_ptr<Gateway>
DomainController::lowestNoStarBuy()
{
  funcPtrVoid f = &Gateway::getStar;
  return ::lowestNotGateway(d_randomBuygates, f, __func__);
}

// ---------------------------------------------------------
//  MEMBER FUNCTION : lowestNotTransBuy
// ---------------------------------------------------------
//  Description  : 'trans' is transaction
//  Role         : CTA algorithm
//  Techniques   : (nothing special)
//  Status       : complete
// ---------------------------------------------------------

shared_ptr<Gateway>
DomainController::lowestNotTransBuy()
{
  funcPtrVoid f = &Gateway::getTransacted;
  return ::lowestNotGateway(d_randomBuygates, f, __func__);
}

// ---------------------------------------------------------
//  MEMBER FUNCTION : lowestNotTransSel
// ---------------------------------------------------------
//  Description  : 'trans' is transaction
//  Role         : CTA algorithm
//  Techniques   : (nothing special)
//  Status       : complete
// ---------------------------------------------------------

shared_ptr<Gateway>
DomainController::lowestNotTransSel()
{
  funcPtrVoid f = &Gateway::getTransacted;
  return ::lowestNotGateway(d_rankedSelgates, f, __func__);
}

// ---------------------------------------------------------
//  MEMBER FUNCTION : lowestUnderCapBuy
// ---------------------------------------------------------
//  Description  : under-capacity means not capset
//  Role         : CTA algorithm
//  Techniques   : (nothing special)
//  Status       : complete
// ---------------------------------------------------------

shared_ptr<Gateway>
DomainController::lowestUnderCapBuy()
{
  funcPtrVoid f = &Gateway::getHash;
  return ::lowestNotGateway(d_randomBuygates, f, __func__);
}

// ---------------------------------------------------------
//  MEMBER FUNCTION : lowestUnderCapSel
// ---------------------------------------------------------
//  Description  : under-capacity means not capset
//  Role         : CTA algorithm
//  Techniques   : (nothing special)
//  Status       : complete
// ---------------------------------------------------------

shared_ptr<Gateway>
DomainController::lowestUnderCapSel()
{
  funcPtrVoid f = &Gateway::getHash;
  return ::lowestNotGateway(d_rankedSelgates, f, __func__);
}

// ---------------------------------------------------------
//  MEMBER FUNCTION : lowestSel
// ---------------------------------------------------------
//  Description  : 'sel' means selgate
//  Role         : CTA algorithm
//  Techniques   : (nothing special)
//  Status       : complete
// ---------------------------------------------------------

shared_ptr<Gateway>
DomainController::lowestSel()
{
  funcPtrVoid f = &Gateway::getHash;
  return ::lowestGateway(d_rankedSelgates, f, __func__);
}

// RESET GATEWAY FUNCTIONS

void
DomainController::resetTildeSelgates()
{
  BOOST_FOREACH( shared_ptr<Gateway> sg, d_rankedSelgates)
    {
      sg->unmarkTilde();
    }
}

// CAPSET AND TRANSOLVE FUNCTIONS

// ---------------------------------------------------------
//  MEMBER FUNCTION : capset
// ---------------------------------------------------------
//  Description  : set lower and upper domain capacities relative to target gateway
//  Role         : called by a 'CapTransAlg' sub-class (capset and transolve algorithm)
//  Techniques   : minimum and maximum network flow
//  Status       : complete
//
//  Design notes
//
//      Purpose
//
//          Determine the current technical capacity of a gateway
//          -- which can be materially greater than the offered
//          capacity, an economic process known as withholding.
//
//      Failure to capacitate
//
//          The 'capset' function is not likely to fail but this
//          could conceivably happen in a very badly formed
//          model.  Unless it can be shown that failure is not a
//          possible occurrence, failure needs to be handled in
//          the implementation.
//
//      Capacity mode
//
//          Capacity modes are defined in 'f/ospmodes.h'.  The
//          modes currently supported comprise normal operation,
//          that is, as encoded by the aggregate enumerator:
//
//              xeona::e_normalCapModes
//
//          At this point in development, the
//          'xeona::e_crisisOperation' mode is not supported.
//          This mode indicates that all non-mandatory
//          constraints should be relaxed when generating the
//          domain structure.
//
//      Problem structuring
//
//          The domain constraints are set as per the 'transolve'
//          process.  But in this case, the target selgate
//          contributes unity to the objective function and all
//          other variables contribute zero.  Minimization sets
//          the lower bound and maximization sets the upper
//          bound.
//
//      Further details
//
//          Refer to my (Robbie Morrison) PhD thesis for a
//          comprehensive description.
//
// ---------------------------------------------------------

bool                                         // return 'false' on failure
DomainController::capset
(const int               step,
 shared_ptr<Gateway>     targetGateway,      // target selgate (meaning sel-side gateway)
 const xeona::DomainMode capacityMode)       // capacity mode indirectly from overseer
{
  // ---------------------------------
  //  preamble
  // ---------------------------------

  // reporting stringstream
  std::ostringstream put;

  // determine aggregate capacity modes
  const xeona::DomainMode aggregateCapacityModes = xeona::e_normalCapModes;

  // undertake outset reporting
  s_logger->repx(logga::dbug, "entering member function", "");

  // function name
  const std::string func = XEONA_FUNC;

  put << "  function                 : " << func                            << "\n"
      << "  step (zero-based)        : " << step                            << "\n"
      << "  domain controller        : " << getIdAndKind()                  << "\n"
      << "  target identifier        : " << targetGateway->getIdAndKind()   << "\n"
      << "  given capacity mode      : " << capacityMode                    << "\n"
      << "  aggregate capacity modes : " << aggregateCapacityModes          << "\n";
  s_logger->putx(logga::xtra, put);

  // local quantities
  int targetGol = 0;                         // duty column index for target gateway

  // confirm the capacity mode, noting that argument two (the
  // aggregate) can be less than argument one (the pure)
  if ( ! xeona::isTwoContained(capacityMode, aggregateCapacityModes) )
    {
      std::ostringstream oss;
      oss << "details above if report " << logga::xtra;
      s_logger->repx(logga::warn, "immiscible capacity mode", oss.str());
      s_logger->repx(logga::xtra, "early return", false);
      return false;
    }

  // abandon gracefully if the target gateway is already capacitated
  if ( targetGateway->getHash() == true )    // set hash means "fully-capacitated"
    {
      s_logger->repx(logga::dbug,
                     "target gateway already marked hash",
                     targetGateway->getIdAndKind());
      return true;
    }

  // ---------------------------------
  //  constrain sweep (capset)
  // ---------------------------------

  // reestablish the solver 'd_solver'
  d_solver->resetGlpkProb();                 // nuke for everybody, very neighborly!

  // step thru asset operators
  BOOST_FOREACH( shared_ptr<AssetOperator> ao, d_assetOperators )
    {
      xeona::putxId(ao, "DomainController::capset asset operator loop");
      ao->constrain(capacityMode);
    }

  // step thru demand junctions
  BOOST_FOREACH( shared_ptr<DemandJunction> dj, d_demandJunctions )
    {
      xeona::putxId(dj, "DomainController::capset demand junction loop");
      dj->constrain(capacityMode);
    }

  // step thru selgates in rank order, as it happens
  BOOST_FOREACH( shared_ptr<Gateway> sg, d_rankedSelgates )
    {
      xeona::putxId(sg, "DomainController::capset sell gate loop");
      // CAUTION: the following action must be done in two statements
      shared_ptr<SelSide> selside = dynamic_pointer_cast<SelSide>(sg);
      const int gol = selside->SelSide::constrain(capacityMode);

      // integrity check
      const std::string selId = sg->getIdAndKind();
      switch ( gol )
        {
        case 0:
          s_logger->repx(logga::warn, "unset duty gol 0", selId);
          break;
        case -1:
          s_logger->repx(logga::warn, "invalid duty gol -1", selId);
          break;
        case -2:
          s_logger->repx(logga::warn, "coded duty gol -2, wrong constrain", selId);
          break;
        default:
          if ( gol < -2 )
            {
              std::clog << "** coding error 01 in source file " << __FILE__ << std::endl;
            }
          break;
        }

      // update column index if this particular selgate is the target
      if ( sg == targetGateway )             // based on 'get' result, can also be zero
        {
          s_logger->repx(logga::dbug, "target selgate gol found", gol);
          targetGol = gol;
        }
    }

  // step thru buygates in arbitrary order
  BOOST_FOREACH( shared_ptr<Gateway> bg, d_randomBuygates )
    {
      xeona::putxId(bg, "DomainController::capset buy gate loop");
      // CAUTION: the following action must be done in two statements
      shared_ptr<BuySide> buyside = dynamic_pointer_cast<BuySide>(bg);
      int gol = buyside->BuySide::constrain(capacityMode);
      static_cast<void>(gol); // temporary to quieten compiler
    }

  // check that we found our target
  if ( targetGol == 0 )
    {
      s_logger->repx(logga::warn, "no target selgate gol", targetGol);
      s_logger->repx(logga::xtra, "early return", false);
      return false;
    }

  // ---------------------------------
  //  replace objective function
  // ---------------------------------

  // grab and store existing objective function and tag
  const std::vector<double> priorObjFunc    = d_solver->getObjective();
  const std::string         priorObjFuncTag = d_solver->getObjectiveTag();

  // create new tag
  const std::string newObjFuncTag
    = "capset"
    + boost::str(boost::format("-%03d") % targetGol);
  s_logger->repx(logga::xtra, "zero-bar-me objective tag", newObjFuncTag);

  // define and install new objective function
  if ( ! d_solver->zeroBarMeObjective(targetGol, 1.0, newObjFuncTag) )
    {
      s_logger->repx(logga::warn, "new objective function load fail", "");
      s_logger->repx(logga::xtra, "early return", false);
      return false;
    }

  // ---------------------------------
  //  flow capacity bounds
  // ---------------------------------

  // first attempt to set the upper limit then the lower limit
  // the following defined array usage is from the 'Boost.Foreach' documentation

  const svif::ObjectiveSense objectives[] = { svif::maximize, svif::minimize };
  BOOST_FOREACH( svif::ObjectiveSense os, objectives )
    {
      // set objective sense
      d_solver->setObjectiveSense(os, newObjFuncTag);  // set tag again, else ""

      // solve
      d_solver->runSolver();                           // returns void

      // obtain solution statuses to assist reporting
      const bool usable  = d_solver->isUsableSoln();
      const bool optimal = d_solver->isOptimalSoln();

      // visualize as appropriate
      // YEEK 3 CODE (set by '--yeek')
      if ( xeona::yeek == 3 && step == 0 )
        {
          s_logger->repx(logga::dbug, "entering yeek code", xeona::yeek);
          ::visualize(d_solver, __func__, step);                 // note caller
        }

      //  write out GLPK problem instance as appropriate
      // YEEK 4 CODE (set by '--yeek')
      if ( xeona::yeek == 4 && step == 0 )
        {
          s_logger->repx(logga::dbug, "entering yeek code", xeona::yeek);
          ::writeProblemInstance(d_solver, __func__, step);      // note caller
        }

      // coefficients span reporting
      double span1 = 0.0;
      std::ostringstream put;
      if ( d_solver->reportCofs(span1, put) )
        {
          s_logger->repx(logga::dbug,
                         "additional reporting follows, trip",
                         d_tripCoeffSpanLevel);
          s_logger->putx(logga::dbug, put);
        }
      else
        {
          s_logger->repx(logga::adhc, "coefficient span", span1);
        }

      const std::string objLabel = d_solver->getObjectiveLabel();

      // confirm
      if ( ! optimal )
        {
          s_logger->repx(logga::info, "solver solution not optimal", objLabel);
        }
      if ( ! usable )
        {
          s_logger->repx(logga::warn, "solver solution not usable", objLabel);
          s_logger->repx(logga::warn, "current step", d_step);
          s_logger->repx(logga::warn, "solver solution unusable, 1=lo 2=up", os);
          s_logger->repx(logga::xtra, "early return", false);
          return false;
        }

      // recover capacity
      s_logger->repx(logga::adhc, "solver solution usable", "");
      const double capacity = d_solver->getVarValue(targetGol);

      // update data and also report
      switch ( os )
        {
        case svif::maximize:
          s_logger->repx(logga::dbug, "solver solution usable, upper", capacity);
          targetGateway->setUpperCapacity(capacity);
          break;
        case svif::minimize:
          s_logger->repx(logga::dbug, "solver solution usable, lower", capacity);
          targetGateway->setLowerCapacity(capacity);
          break;
        default:                             // compiler needs either default or all enums
          std::clog << "** coding error 02 in source file " << __FILE__ << std::endl;
          break;
        }
    }

  // ---------------------------------
  //  finishing up
  // ---------------------------------

  d_solver->renewObjective(priorObjFunc,               // reinstate objective function
                           priorObjFuncTag);

  // ---------------------------------
  //  identify full capacitation
  // ---------------------------------

  // A gateway can have two capacitation states:
  //
  //   * under-capacitation : the innate limits provided by the modeler
  //   * full-capacitation  : the potentially tighter limits provided by this algorithm
  //
  // That is not to say that the full-capacitation limits are
  // immutable -- they can be revised under the hop-relitigate
  // extension to the CTA algorithm

  const bool priorHash = targetGateway->markHash();    // mark fully-capacitated

  // 'priorHash' should be 'false'
  if ( priorHash == true )
    {
      s_logger->repx(logga::warn, "gateway was fully-capacitated", priorHash);
    }

  // ---------------------------------
  //  return
  // ---------------------------------

  // indicate success
  return true;

} // function 'DomainController::capset'

// ---------------------------------------------------------
//  MEMBER FUNCTION : transolve
// ---------------------------------------------------------
//  Description  : solve internal flows while honoring selgate obligations
//  Role         : called by the capset and transolve algorithm (CTA)
//  Techniques   : least cost network flow or some variant
//  Status       : complete
//
//  Design notes
//
//      Failure to solve
//
//          The 'transolve' function is likely to fail, that is,
//          fail to find a feasible solution.  Such failure needs
//          to be handled in the implementation.
//
//      Commitment modes (aka control strategies)
//
//          Commitment modes are defined in 'f/ospmodes.h'.  The
//          modes currently supported comprise:
//
//              shortrun-fin  : short-run financial cost minimization
//              shortrun-ghg  : short-run ghg contribution minimization
//              shortrun-nox  : short-run nox contribution minimization
//              shortrun-dep  : short-run depletable resource use minimization
//              shortrun-luc  : short-run land use minimization
//
//              auction-lmp   : locational marginal (nodal) pricing auction
//              admin-merit   : prescribed merit order
//              admin-first   : first feasible solution
//
// ---------------------------------------------------------

bool                                         // return 'false' on failure
DomainController::transolve
(const int               step,
 const xeona::DomainMode capacityMode)       // capacity mode indirectly from overseer
{
  // ---------------------------------
  //  preamble
  // ---------------------------------

  // reporting stringstream
  std::ostringstream put;

  // determine aggregate capacity modes
  const xeona::DomainMode aggregateCapacityModes = xeona::e_normalCapModes;

  // undertake outset reporting
  s_logger->repx(logga::dbug, "entering member function", "");

  // function name
  const std::string func = XEONA_FUNC;

  put << "  function                 : " << func                    << "\n"
      << "  step (zero-based)        : " << step                    << "\n"
      << "  stored commitment mode   : " << d_commitmentMode        << "\n"
      << "  given capacity mode      : " << capacityMode            << "\n"
      << "  aggregate capacity modes : " << aggregateCapacityModes  << "\n";
  s_logger->putx(logga::xtra, put);

  // confirm the capacity mode, noting that argument two (the
  // aggregate) can be less than argument one (the pure)
  if ( ! xeona::isTwoContained(capacityMode, aggregateCapacityModes) )
    {
      std::ostringstream oss;
      oss << "details above if report " << logga::xtra;
      s_logger->repx(logga::warn, "immiscible capacity mode", oss.str());
      s_logger->repx(logga::xtra, "early return", false);
      return false;
    }

  // any required integrity checks can go here

  // ---------------------------------
  //  constrain sweep (transolve)
  // ---------------------------------

  s_logger->repx(logga::adhc, "domain constrain starts", "");

  // reestablish the solver 'd_solver'
  d_solver->resetGlpkProb();                 // nuke for everybody, very neighborly!

  // step thru asset operators
  BOOST_FOREACH( shared_ptr<AssetOperator> ao, d_assetOperators )
    {
      xeona::putxId(ao, "DomainController::transolve asset operator loop");
      ao->constrain(capacityMode);
    }

  // step thru demand junctions
  BOOST_FOREACH( shared_ptr<DemandJunction> dj, d_demandJunctions )
    {
      xeona::putxId(dj, "DomainController::transolve demand junction loop");
      dj->constrain(capacityMode);
    }

  // step thru selgates in rank order, as it happens
  BOOST_FOREACH( shared_ptr<Gateway> sg, d_rankedSelgates )
    {
      xeona::putxId(sg, "DomainController::transolve sell gate loop");

      // CAUTION: better to cast once when loading
      // 'd_rankedSelgates' and not for each step, this would be
      // faster -- the same applies elsewhere too

      // CAUTION: the following action must be done in two statements
      // this code is buggy: dynamic_pointer_cast<SelSide>(sg)->washup()
      shared_ptr<SelSide> sel = dynamic_pointer_cast<SelSide>(sg);
      sel->SelSide::constrain(capacityMode);
    }

  // step thru buygates in arbitrary order
  BOOST_FOREACH( shared_ptr<Gateway> bg, d_randomBuygates )
    {
      xeona::putxId(bg, "DomainController::transolve buy gate loop");
      shared_ptr<BuySide> buy = dynamic_pointer_cast<BuySide>(bg);
      buy->BuySide::constrain(capacityMode);
    }

  // ---------------------------------
  //  cost-type-based directionality
  // ---------------------------------

  s_logger->repx(logga::adhc, "domain set directionality starts", "");

  // determine the optimization direction using the value of
  // 'd_commitmentMode'.

  svif::ObjectiveSense direction = svif::sense_not_specified;    // initial value
  std::string objFuncTag = "tsolve";

  switch ( d_commitmentMode )
    {
    case xeona::e_shortrunFin:
      direction = svif::minimize;
      objFuncTag += "-srfin";
      break;
    case xeona::e_shortrunGhg:
      direction = svif::minimize;
      objFuncTag += "-srghg";
      break;
    case xeona::e_shortrunNox:
      direction = svif::minimize;
      objFuncTag += "-srnox";
      break;
    case xeona::e_shortrunDep:
      direction = svif::minimize;
      objFuncTag += "-srdep";
      break;
    case xeona::e_shortrunLuc:
      direction = svif::minimize;
      objFuncTag += "-srluc";
      break;
    case xeona::e_auctionLmp:                // framed as least cost (see end of file)
      direction = svif::minimize;
      objFuncTag += "-nodal";
      break;
    case xeona::e_adminMerit:
      direction = svif::minimize;
      objFuncTag += "-merit";
      break;
    case xeona::e_adminFirst:                // direction not applied in this case
      direction = svif::minimize;
      objFuncTag += "-first";
      break;
    default:
      std::clog << "** coding error 03 in source file " << __FILE__ << std::endl;
      break;
    }

  const std::string oldObjFuncTag = d_solver->getObjectiveTag();
  if ( ! oldObjFuncTag.empty() )
    {
      s_logger->repx(logga::dbug, "object function tag not empty", oldObjFuncTag);
    }

  d_solver->setObjectiveSense(direction, objFuncTag);

  // ---------------------------------
  //  run solver
  // ---------------------------------

  s_logger->repx(logga::adhc, "domain run solver starts", "");

  // solve
  d_solver->runSolver();                          // returns 'void'

  // obtain solution statuses to assist reporting
  const bool usable  = d_solver->isUsableSoln();
  const bool optimal = d_solver->isOptimalSoln();

  // ---------------------------------
  //  solution reporting
  // ---------------------------------

  // visualize as appropriate
  // YEEK 3 CODE (set by '--yeek')
  if ( xeona::yeek == 3 && step == 0 )
    {
      s_logger->repx(logga::dbug, "entering yeek code", xeona::yeek);
      ::visualize(d_solver, __func__, step);                // note caller
    }

  //  write out GLPK problem instance as appropriate
  // YEEK 4 CODE (set by '--yeek')
  if ( xeona::yeek == 4 && step == 0 )
    {
      s_logger->repx(logga::dbug, "entering yeek code", xeona::yeek);
      ::writeProblemInstance(d_solver, __func__, step);     // note caller
    }

  // coefficients span reporting
  double span2 = 0.0;
  if ( d_solver->reportCofs(span2, put) )    // 'put' already declared
    {
      s_logger->repx(logga::dbug,
                     "additional reporting follows, trip",
                     d_tripCoeffSpanLevel);
      s_logger->putx(logga::dbug, put);
    }
  else
    {
      s_logger->repx(logga::adhc, "coefficient span", span2);
    }

  const std::string objLabel = d_solver->getObjectiveLabel();

  // ---------------------------------
  //  troubleshooting information
  // ---------------------------------

  if ( ! optimal )
    {
      s_logger->repx(logga::info, "solver solution not optimal", objLabel);
    }
  if ( ! usable )
    {
      // a 'false' return should lead to:
      //
      //    application exit : 51 = simulation call encountered
      //      infeasibility (solver fail or problem choke)

      s_logger->repx(logga::warn, "solver solution not usable", objLabel);
      s_logger->repx(logga::warn, "current step", d_step);

      // display model in web browser as appropriate
      // YEEK 1 CODE (set by '--yeek')
      if ( xeona::yeek == 1 )
        {
          s_logger->repx(logga::dbug, "troubleshooting visualization, yeek", xeona::yeek);
          ::visualize(d_solver, __func__, step);                 // note caller
        }

      // write out model in any case
      if ( true )
        {
          s_logger->repx(logga::dbug, "troubleshooting GLPK save", "hard-coded");
          ::writeProblemInstance(d_solver, __func__, step);      // note caller
        }
      s_logger->repx(logga::warn, "early return", false);
      return false;
    }
  else
    {
      s_logger->repx(logga::dbug, "solver solution usable", objLabel);
    }

  // ---------------------------------
  //  washup sweep (transolve)
  // ---------------------------------

  // this functionality is now in 'b/overseer.cc:349' within the
  // function 'run' -- so the washups now occur AFTER all the
  // 'transolve' calls (that code could perhaps be moved here if
  // needed)

  // ---------------------------------
  //  finishing up
  // ---------------------------------

  s_logger->repx(logga::adhc, "leaving member function, return", true);

  // indicate success (see above for early returns)
  return true;

} // 'DomainController::transolve'

// ---------------------------------------------------------
//  documentation   : nodal pricing
// ---------------------------------------------------------
//
//  LOCATIONAL MARGINAL PRICING
//
//  Locational marginal pricing (LMP) (also known as "nodal
//  pricing") is a form of wholesale electricity pricing which
//  uses the "reduced cost" (or "slack value") of each nodal flow
//  balance to determine the system unit price for that node.
//
//  LMP applies only to the energy component.  Use of the network
//  itself is treated as a separate issue and transmission
//  pricing methods are applied orthogonally (notwithstanding,
//  the question of a more integrated pricing methodology
//  remains).
//
//  LMP pricing is employed in the PJM (Pennsylvania-New
//  Jersey-Maryland), New York, and New England wholesale
//  electricity markets in the USA, in New Zealand, and
//  elsewhere.  Much of the underpinning theory is based on work
//  by William Hogan, Harvard University.  LMP markets have been
//  operating since the mid-1990s, with mixed success in the case
//  of New Zealand.
//
//  The reduced cost for each nodal flow balance is calculated
//  using a direct current (DC) power flow model and a linear
//  program (LP) (note that 'xeona' uses integer variables to set
//  the direction of flow, thereby creating a mixed-integer
//  program).  Under the normal LMP formulation, the value of
//  trading is maximized -- and, in the presence of perfect
//  competition, social welfare (noting that economic welfare
//  would read better) would also be maximized.  Notwithstanding,
//  'xeona' was coded with the objective function negated and
//  with minimization applied.  The resultant reduced cost for
//  each nodal flow balance sets the system unit price (or
//  "nodal" price) for that node.  If the bids are
//  cost-reflective, the nodal price represents the opportunity
//  cost of export.  However, bids need not be cost-reflective
//  and may indeed be highly strategic.
//
//  If a generator (or rather a bid) is:
//
//    * partially dispatched : nodal price = offer price
//    * fully dispatched     : nodal price > than offer price
//    * not dispatched       : nodal price < than offer price
//
//  In addition, system constraints can result in a lack of
//  competition.  Provoking constraints for the purpose of
//  profiteering is known as constraint gaming.  One form of
//  gaming tactic is to fake an unscheduled outage (used by Enron
//  during the Californian power crisis in 2000).
//
//  The market operator normally also acts as the system banker.
//  Under LMP pricing, a surplus normally accrues.  This then
//  needs to be redistributed, and is often handed to the
//  generators.  In some jurisdictions, this surplus is assigned
//  to the transmission operator -- a practice not generally
//  favored as it offers the transmission operator a perverse
//  incentive to increase grid losses.  Cash flows are not
//  modeled here but can be easily calculated by hand.
//
//  TRANSMISSION FLOWS AND LOSSES
//
//  Two forms of transmission and transmission loss are supported
//  within 'xeona'.
//
//  DC transmission losses are quadratic on power flow, which
//  means that the relative losses (defined as one - efficiency)
//  need to be stepwise discretized.  The LP problem formulation
//  adopted here assumes that power flows are non-negative.
//  Transmission lines are naturally bidirectional and hence the
//  'line' component characterization requires equations for both
//  forward (fwd) and back (bak) flow.
//
//  AC losses are based on the (misnamed) enhanced DC power flow
//  model -- described elsewhere in detail.  The AC transmission
//  model used here generates a mixed-integer problem containing
//  two binary variables for each transmission entity.  In
//  addition, the AC problem is designed for negative flows
//  (unlike its DC cousin).

//  end of file

