//  XEONA ENERGY SYSTEMS MODELING ENVIRONMENT
//
//  file-create-name : overseer.cc
//  file-create-date : Thu 07-Aug-2008 19:53 UTC
//  file-initiator   : Robbie Morrison <robbie@actrix.co.nz>
//
//  file-role        : top-level overseer entity (singleton) / implementation
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
//  $URL: file:///home/robbie/svn-root/xeona/futz/trunk/xeona1/b/overseer.cc $

//  LOCAL AND SYSTEM INCLUDES

#include "overseer.h"         // companion header for this file (place first)

#include "../f/trav.h"        // domain graph traversal class
#include "../f/ospmodes.h"    // domain mode enums (header only)
#include "../f/cta.h"         // captrans algorithm
#include "../e/context.h"     // abstract context entity
#include "../c/smart_ptr.h"   // toggle between Boost and TR1 smart pointers
#include "../c/recset.h"      // records and fields and also record-sets
#include "../c/costs.h"       // cost sets and support
#include "../b/domcon.h"      // domain controller entity
#include "../a/logger.h"      // standard logging functionality (as required)
#include ".././common.h"      // common definitions for project (place last)

#include <string>             // C++ strings
#include <sstream>            // string-streams
#include <vector>             // STL sequence container

#include <boost/foreach.hpp>  // BOOST_FOREACH iteration macro

//  CODE

// STATIC DEFINITIONS

unsigned Overseer::s_count = 0;

// CREATORS

// ---------------------------------------------------------
//  MEMBER FUNCTION : Overseer
// ---------------------------------------------------------

Overseer::Overseer
(const std::string entityId,
 Record&           record) :
  CostRegister(record),
  FullEntity(entityId, record),
  CostRegisterOverseer(record),
  d_captrans_algorithm(record.tieSingle<std::string>("captrans-algorithm")),
  d_ranked_orig_domains(record.tieSingle<std::string>("ranked-orig-domains")),
  d_rankedOrigDomains()                      // vector
{
  // initial reporting and integrity checks
  s_logger->repx(logga::dbug, "constructor call", getIdAndKind());
  ++s_count;                                 // instance count
  if ( s_count > 1 )
    {
      s_logger->repx(logga::warn, "single overseer condition violated", s_count);
    }

  // check for originating domains
  if ( d_ranked_orig_domains.empty() )
    {
      s_logger->repx(logga::rankJumpy,
                     "no ranked originating domains given",
                     d_ranked_orig_domains);
    }

  // built in remark
  d_builtinRemark = "mandatory entity";

  // completion reporting
  s_logger->repx(logga::adhc, "leaving member function", "");
}

// ---------------------------------------------------------
//  MEMBER FUNCTION : ~Overseer
// ---------------------------------------------------------

Overseer::~Overseer()
{
  s_logger->repx(logga::adhc, "destructor call", getIdAndKind());
}

// ---------------------------------------------------------
//  MEMBER FUNCTION : factoryInitialize
// ---------------------------------------------------------

void
Overseer::factoryInitialize()                // called by factory after construction
{
}

// SIMULATION POINT OF ENTRY

// ---------------------------------------------------------
//  MEMBER FUNCTION : run
// ---------------------------------------------------------
//  Description  : simulation point of entry
//  Role         : invoked by free function 'xeona::simulate'
//  Techniques   : string list to vector via templated 'listToVec'
//  Status       : complete
//
//  Design notes
//
//      Function return values
//
//          The following return statuses are supported:
//
//              premature 'false' : on solver infeasibility
//              full-term 'false' : as above but option '--again' deployed
//              full-term 'true'  : normal exit
//
//          The '--again' command-line option means do not quit
//          on bad solver returns.
//
//      Dynamic domain graphs
//
//          The addition of some more 'TravDepthFirst' reset
//          calls would enable the domain graph to be revised
//          between intervals, that is, the domain graph would
//          become a dynamic structure.  The ranked original
//          domains would also need an updating process too if
//          this were to be fully general.  The reset calls would
//          look something like this:
//
//              dfs.reset(new TravDepthFirst(d_rankedOrigDomains))
//
//      Yet to be implemented functionality
//
//          See code for comments and stubs for future
//          development.  This functionality is not essential to
//          the basic 'xeona' model and has been omitted during
//          the first development pass.
//
// ---------------------------------------------------------

bool
Overseer::run
(const int steps)
{
  // ---------------------------------
  //  preliminaries
  // ---------------------------------

  // initial logging
  s_logger->repx(logga::info, "entering member function, steps", steps);

  // set function return value
  bool runRet = true;                        // presume success

  // prepare ranked domain controller list
  d_rankedOrigDomains.clear();
  const int origDomainsCnt
    = FullEntity::listToVec<DomainController>(d_ranked_orig_domains, d_rankedOrigDomains);

  // report if empty
  if ( origDomainsCnt == 0 )
    {
      s_logger->repx(logga::rankJumpy,
                     "no ranked originating domains, cnt",
                     origDomainsCnt);
    }

  // report
  s_logger->repx(logga::adhc, "originating domains count", origDomainsCnt);

  // ---------------------------------
  //  domain graph traversal
  // ---------------------------------

  s_logger->repx(logga::adhc, "domain graph DFS calls start", "");

  // declare a depth first search object, used to invoke domain
  // functions 'establish' 'initialize' 'constrain' 'washup' and
  // 'conclude'

  shared_ptr<TravDepthFirst> dfs;            // empty shared pointer

  // the construction of a 'TravDepthFirst' object also invokes a
  // depth first search (DFS) traversal of the domain graph --
  // this object is latter passed various function pointers

  dfs.reset(new TravDepthFirst(d_rankedOrigDomains));  // domain graph also traversed

  // the search also counts the number of components in the
  // graph, whereby a component is a maximally connected
  // sub-graph

  const int componentCount = dfs->getComponentsCount();
  s_logger->repx(logga::adhc, "component count", componentCount);

  // ---------------------------------
  //  cost register reset
  // ---------------------------------

  CostRegister::resetRegister();             // reset entire register

  // ---------------------------------
  //  setup contexts
  // ---------------------------------

  // context entities are not creatures of domains and hence need
  // their own setup loop here -- but because class 'Context'
  // does not inherit from 'TicToc' that call is not named
  // 'establish' but rather 'setup'

  s_logger->repx(logga::adhc, "setup contexts sweep starts", "");

  std::vector<shared_ptr<Context> > buffer;

  //  the code below uses the following static function:
  //  const std::vector<shared_ptr<Entity> >& Entity::getCensusFull()

  BOOST_FOREACH( shared_ptr<Entity> e, Entity::getCensusFull() )
    {
      // attempt a downcast
      shared_ptr<Context> cx = dynamic_pointer_cast<Context>(e);
      if ( cx != 0 )
        {
          buffer.push_back(cx); // load the downcast version
        }
    }

  s_logger->repx(logga::dbug, "context count", buffer.size());

  // step thru contexts
  BOOST_FOREACH( shared_ptr<Context> cx, buffer )
    {
      cx->setup();
    }

  // ---------------------------------
  //  establish domains sweep
  // ---------------------------------

  // NOTE: cost register resets are also required for domain
  // controllers, asset operators, and technical assets as part
  // of their establishment loops -- indeed, this procedure is
  // somewhat brittle, it would be much better to have a full
  // entity tree traversal with both hollow and active reset
  // calls

  s_logger->repx(logga::adhc, "establish domains sweep starts", "");

  // CAUTION: for some unknown reason, the following two lines
  // could not be combined into one line -- so be careful!

  funcPtrVoid funcEstablishDomain = &DomainController::establishDomain;
  const int establishCount        = dfs->callDomains(funcEstablishDomain);
  s_logger->repx(logga::xtra, "establish sweep complete, domains", establishCount);

  // ---------------------------------
  //  restructure domains sweep
  // ---------------------------------

  s_logger->repx(logga::adhc, "restructure domains sweep starts", "");

  // CAUTION: for some unknown reason, the following two lines
  // could not be combined into one line -- so be careful!

  funcPtrVoid funcRestructureDomain = &DomainController::restructureDomain;
  const int restructureCount        = dfs->callDomains(funcRestructureDomain);
  s_logger->repx(logga::xtra, "restructure sweep complete, domains", restructureCount);

  // ---------------------------------
  //  step thru horizon
  // ---------------------------------

  s_logger->repx(logga::adhc, "step thru horizon starts, steps", steps);

  // CAUTION: the step count is ZERO-based

  // step thru the horizon steps
  for ( int step = 0; step < steps; ++step )
    {
      // ---------------------------------
      //  update step value sweep
      // ---------------------------------

      s_logger->repx(logga::adhc, "set step domains sweep starts", "");

      // call simply overwrites 'd_step'

      // CAUTION: for some unknown reason, the following two
      // lines could not be combined into one line -- so be
      // careful!

      funcPtrStep funcSetStep = &DomainController::setStep;
      const int initCount1    = dfs->callDomains(funcSetStep, step);
      s_logger->repx(logga::xtra, "set step sweep complete, domains", initCount1);

      // ---------------------------------
      //  responsive contexts sweep
      // ---------------------------------

      // update the responsive contexts (that is, contexts
      // responsive to other contexts and events) here in due
      // course

      // ---------------------------------
      //  thermal sub-net sweep
      // ---------------------------------

      // stub for a thermal sub-net (TSN) sweep

      // ---------------------------------
      //  initialize domains sweep
      // ---------------------------------

      s_logger->repx(logga::adhc, "initialize domains sweep starts", "");

      // each domain initializes its operators, assets, and
      // selgates, while also passing down 'step' and 'solver'
      // information in the process

      // CAUTION: for some unknown reason, the following two
      // lines could not be combined into one line -- so be
      // careful!

      funcPtrVoid funcInitializeDomain = &DomainController::initializeDomain;
      const int initCount2             = dfs->callDomains(funcInitializeDomain);
      s_logger->repx(logga::xtra, "initialize sweep complete, domains", initCount2);

      // ---------------------------------
      //  constrain domains sweep
      // ---------------------------------

      // code not required as constraint undertaken as part of
      // 'DomainController::capset' and
      // 'DomainController::transolve' calls, code last in r3030

      // ---------------------------------
      //  CTA preparations
      // ---------------------------------

      s_logger->repx(logga::adhc, "system CTA calls start", "");

      // the various domain and gateway flags are suitably
      // initialized on construction and are reset at the end of
      // each call

      // if the system fails to solve, successive levels of
      // constraint removal are employed -- however, supply
      // rationing is neither designed into 'xeona' nor
      // implemented

      shared_ptr<CapTransAlg> cta;           // empty shared pointer

      // create an appropriate CTA object
      if      ( d_captrans_algorithm == "fixed" )
        {
          cta.reset(new CtaFixed(step, d_rankedOrigDomains));
        }
      else if ( d_captrans_algorithm == "simple" )
        {
          cta.reset(new CtaSimple(step, d_rankedOrigDomains));
        }
      else if ( d_captrans_algorithm == "hop-relit" )
        {
          // TOFIX: hop-relit: reinstate after working up 'CtaHopRelit' in unit 'f/cta'
          // cta.reset(new CtaHopRelit(step, d_rankedOrigDomains));
        }
      else
        {
          const bool ret = false;
          s_logger->repx(logga::warn,
                         "unrecognized captrans algorithm name",
                         d_captrans_algorithm);
          s_logger->repx(logga::info, "abandoning member function, return", ret);
          return ret;
        }

      // ---------------------------------
      //  CTA attempts
      // ---------------------------------

      // attempt normal operation
      if ( ! cta->captrans(xeona::e_withholdOkay) ) // indeterminate (bad data) passes
        {
          if ( xeona::again == false )       // command-line option '--again' absent
            {
              s_logger->repx(logga::info, "abandoning run (else use --again)", "");
              return false;
            }

          // then ban withholding and retry
          if ( ! cta->captrans(xeona::e_withholdBan) )
            {
              // customer complaints can be expected!
              if ( xeona::nopro == false )    // command-line option '--krazy' absent
                {
                  s_logger->repx(logga::dbug, "abandoning run (else use --krazy)", "");
                  return false;              // abandon simulation
                }
              else
                {
                  if ( runRet == true )
                    {
                      s_logger->repx(logga::dbug, "return toggled to false", "");
                    }
                  runRet = false;            // reset return
                  continue;                  // abandon step and try to continue
                }
            }
        } // top level if

      // ---------------------------------
      //  abandon if yeek 28
      // ---------------------------------

      // additional action as appropriate
      // YEEK 28 CODE (set by '--yeek')
      if ( xeona::yeek == 28 )
        {
          std::string msg;
          msg += "part way thru function '";
          msg += __func__;
          msg += "'";
          msg += " using withholding okay";
          s_logger->repx(logga::warn, "will throw xeona::yeek_abandon", xeona::yeek);
          throw xeona::yeek_abandon(msg);
        }

      // ---------------------------------
      //  washup domains sweep
      // ---------------------------------

      s_logger->repx(logga::adhc, "washup domains sweep starts", "");

      funcPtrVoid funcWashupDomain = &DomainController::washupDomain;
      const int washupCount        = dfs->callDomains(funcWashupDomain);
      s_logger->repx(logga::xtra, "washup sweep complete, domains", washupCount);

      // ---------------------------------
      //  cost consolidation sweep
      // ---------------------------------

      s_logger->repx(logga::adhc, "consolidate sweep of domains starts", "");

      CostSet var(0.0);
      CostSet fix(0.0);
      CostSet emb(0.0);
      boost::tuple<CostSet, CostSet, CostSet> costs = boost::make_tuple(var, fix, emb);
      funcPtrStepCs3 funcConsolDomain = &DomainController::consolidateDomain;
      const int consolCount           = dfs->callDomains(funcConsolDomain,
                                                         step,
                                                         costs);
      importCosts(step, costs);       // load the new consolidated costs
      s_logger->repx(logga::xtra, "consolidate sweep complete, domains", consolCount);

    } // steps loop

  s_logger->repx(logga::adhc, "step thru horizon complete", "");

  // ---------------------------------
  //  conclude domains sweep
  // ---------------------------------

  s_logger->repx(logga::adhc, "conclude domains sweep starts", "");

  funcPtrVoid funcConcludeDomain = &DomainController::concludeDomain;
  const int concludeCount        = dfs->callDomains(funcConcludeDomain);
  s_logger->repx(logga::xtra, "conclude sweep complete, domains", concludeCount);

  // ---------------------------------
  //  additional reporting
  // ---------------------------------

  // additional reporting as appropriate
  // YEEK 58 CODE (set by '--yeek')
  if ( xeona::yeek == 58 || xeona::yeek == 1 || xeona::yeek == 2 )
    {
      const int indent = 4;
      std::ostringstream put;
      put << "  " << "function : " << XEONA_FUNC << "\n";
      put << CostRegisterOverseer::highPrecisionReport(indent);  // newline included
      s_logger->repx(logga::dbug, "additional reporting follows, yeek", xeona::yeek);
      s_logger->putx(logga::dbug, put);
    }

  // ---------------------------------
  //  return
  // ---------------------------------

  // return
  s_logger->repx(logga::adhc, "leaving member function, return", runRet);
  return runRet;

} // function 'Overseer::run'

// see r2151 for 'xeona::e_crisisOperation' nesting in CTA attempts code

//  end of file

