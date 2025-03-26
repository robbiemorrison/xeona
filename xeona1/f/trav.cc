//  XEONA ENERGY SYSTEMS MODELING ENVIRONMENT
//
//  file-create-name : trav.cc
//  file-create-date : Wed 11-Feb-2009 13:42 UTC
//  file-initiator   : Robbie Morrison <robbie@actrix.co.nz>
//
//  file-role        : domain graph traversal class / implementation
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
//  $URL: file:///home/robbie/svn-root/xeona/futz/trunk/xeona1/f/trav.cc $
//
//  GENERAL NOTES FOR THIS FILE

//  LOCAL AND SYSTEM INCLUDES

#include "trav.h"             // companion header for this file (place first)

#include "../b/domcon.h"      // domain controller entity
#include "../b/gate.h"        // gateway entity
#include "../c/util1.h"       // free functions which offer general utilities 1
#include "../c/util2.h"       // free functions which offer general utilities 2
#include "../a/logger.h"      // standard logging functionality (as required)
#include "../c/smart_ptr.h"   // toggle between Boost and TR1 smart pointers
#include ".././common.h"      // common definitions for project (place last)

#include <string>             // C++ strings
#include <sstream>            // string-streams

#include <boost/foreach.hpp>  // BOOST_FOREACH iteration macro

//  CODE

// STATIC DEFINITIONS - with and without explicit initialization

logga::spLogger TravDepthFirst::s_logger = logga::ptrLogStream();

// CREATORS

// ---------------------------------------------------------
//  MEMBER FUNCTION : TravDepthFirst
// ---------------------------------------------------------
//  Description  : constructor
//  Role         : services the 'Overseer' singleton
//  Techniques   : constructor contains internal call to DFS algorithm
//  Status       : complete
//
//  Design notes
//
//      Construction automatically invokes the DFS algorithm, due
//      to the call to private member function 'depthFirstTrav'.
//
//      The subsequent data recovery calls can simply return
//      the information recovered earlier.
//
// ---------------------------------------------------------

TravDepthFirst::TravDepthFirst
(const std::vector<shared_ptr<DomainController> >& domcons) :  // order significant
   d_originatingDomcons(domcons),
   d_allDomcons(),                           // empty vector
   d_componentCount(0)                       // initialize to zero
{
  s_logger->repx(logga::xtra,
                 "constructor call, orig domcon count",
                 d_originatingDomcons.size());

  depthFirstTrav();                          // sole call to this utility function
}

// ---------------------------------------------------------
//  MEMBER FUNCTION : ~TravDepthFirst
// ---------------------------------------------------------

TravDepthFirst::~TravDepthFirst()
{
  s_logger->repx(logga::adhc, "destructor call", "");
}

// ACCESSORS

// ---------------------------------------------------------
//  MEMBER FUNCTION : getOrigDomconsCount
// ---------------------------------------------------------

int                                          // number of originating domains
TravDepthFirst::getOrigDomconsCount() const
{
  return d_originatingDomcons.size();
}

// ---------------------------------------------------------
//  MEMBER FUNCTION : getAllDomconsCount
// ---------------------------------------------------------

int                                          // total number of domains
TravDepthFirst::getAllDomconsCount() const
{
  return d_allDomcons.size();
}

// ---------------------------------------------------------
//  MEMBER FUNCTION : getComponentsCount
// ---------------------------------------------------------

int                                          // number of maximally connected sub-graphs
TravDepthFirst::getComponentsCount() const
{
  return d_componentCount;
}

// ---------------------------------------------------------
//  MEMBER FUNCTION : getAllDomcons
// ---------------------------------------------------------

bool
TravDepthFirst::getAllDomcons
(std::vector<shared_ptr<DomainController> >& allDomcons) const // note non-const reference
{
  if ( ! allDomcons.empty() )
    {
      s_logger->repx(logga::warn, "load up container not empty", d_allDomcons.size());
      s_logger->repx(logga::xtra, "abandoning function without action", "");
      return false;
    }
  allDomcons = d_allDomcons;                 // copy assignment to non-const reference
  return true;
}

// MANIPULATORS

// ---------------------------------------------------------
//  MEMBER FUNCTION : callDomains
// ---------------------------------------------------------
//  Description  : traverse all domains and call 'func' on each domain
//  Role         : used by 'Overseer' to launch 'constrain' calls and similar
//  Techniques   : 'func' pointer-to-member function argument
//  Status       : complete
//
//  Design notes (which also mostly apply to the other overloads)
//
//      Typical applications
//
//          This is a matter for client code but some typical
//          applications are listed here nonetheless:
//
//          - horizon initialization traversal
//          - interval initialization traversal
//          - interval finalization traversal
//          - horizon finalization traversal
//
//      Depth first search on construction
//
//          The underpinning depth first search (DFS) is
//          undertaken on construction.  An update can be achieve
//          by invoking a shared pointer delete/new cycle,
//          namely:
//
//              dfs.reset(new TravDepthFirst(rankedOrigDomains))
//
//      Usage
//
//          The pointer-to-member function argument is explained
//          in the overarching documentation for class
//          'TravDepthFirst' in the header.
//
//      Pointer function call syntax
//
//          The following syntax should work for raw pointers,
//          but would NOT with Boost.Smart_ptr smart pointers:
//
//              (dc->*func)(step)
//
//          The g++ 4.1.2 compiler chokes thus -- see the code
//          proper for a simple work around:
//
//              error: no match for 'operator->*' in 'dc ->* func'
//
// ---------------------------------------------------------

const int                                    // number of visited domains
TravDepthFirst::callDomains
(funcPtrStep func,                           // see typedef in header
 const int   step)                           // duly passed thru to the 'func' call
{
  int count = 0;
  BOOST_FOREACH( shared_ptr<DomainController> dc, d_allDomcons )
    {
      ++count;
      ((*dc).*func)(step);                   // CAUTION: syntax is correct, see above
    }
  return count;
}

// ---------------------------------------------------------
//  MEMBER FUNCTION : callDomains
// ---------------------------------------------------------

const int                                    // number of visited domains
TravDepthFirst::callDomains
(funcPtrVoid func)                           // see typedef in header
{
  int count = 0;
  BOOST_FOREACH( shared_ptr<DomainController> dc, d_allDomcons )
    {
      ++count;
      ((*dc).*func)();                       // CAUTION: syntax is correct, see above
    }
  return count;
}

// ---------------------------------------------------------
//  MEMBER FUNCTION : callDomains
// ---------------------------------------------------------

const int
TravDepthFirst::callDomains
(funcPtrStepCs3                           func,
 const int                                step,
 boost::tuple<CostSet, CostSet, CostSet>& costs)
{
  int count = 0;
  BOOST_FOREACH( shared_ptr<DomainController> dc, d_allDomcons )
    {
      ++count;
      ((*dc).*func)(step, costs);            // CAUTION: syntax is correct, see above
    }
  return count;
}

// UTILITY FUNCTIONS

// ---------------------------------------------------------
//  MEMBER FUNCTION : depthFirstTrav
// ---------------------------------------------------------
//  Description  : traverse entire domain graph given only the originating domains
//  Role         : support for custom domain graph traversals
//  Techniques   : depth first search (DFS) using a non-recursive labeling scheme
//  Status       : complete
//
//  Design notes
//
//      Originating domains
//
//          Note that the 'd_originatingDomcons' originating
//          domains list can be interpreted as representing the
//          prioritized selgates from a single hypothetical
//          originating domain.  Indeed some DFS methods begin by
//          creating such a "genesis" node.
//
//      Push/pop buygate calls
//
//          The buygate ordering is arbitrary and -- unlike
//          selgates -- no priority ranking is required.  In this
//          case, the ordering is determined by the visitation
//          process.
//
//      Tilde and hyphen flags
//
//          The 'tilde' gateway and 'hyphen' domain flags are
//          reserved for this procedure.  For completeness, the
//          other gateway flags comprise 'theta' 'star' and
//          'hash' and are for use by the captrans algorithm
//          (CTA).
//
//      Topological sort order
//
//          One side effect of a DFS search is that the resulting
//          visitation order is topologically sorted.
//
//      Graph components (unconnected sub-graphs) count
//
//          The DFS method can also identify and count the number
//          of individual unconnected sub-graphs.  The result is,
//          at the time of writing, logged at the level of
//          'logga::info', but is not written to the XEM file as
//          output data (that however is a matter for the client
//          code and may change).
//
// ---------------------------------------------------------

bool
TravDepthFirst::depthFirstTrav()
{
  s_logger->repx(logga::dbug, "entering member function", "");

  // ---------------------------------
  //  input data integrity check
  // ---------------------------------

  if ( d_originatingDomcons.empty() )        // client supplied data, so may be empty
    {
      if ( xeona::releaseStatus == true )    // complain more
        {
          s_logger->repx(logga::warn, "no originating domain controllers", "");
        }
      else
        {
          s_logger->repx(logga::rankNoData, "no originating domain controllers", "");
        }
      return true;                           // not considered an error
    }

  // ---------------------------------
  //  multiple invocation protection
  // ---------------------------------

  // no need to repeat the traversal once done

  static bool firstInvocation = true;        // static member
  if ( firstInvocation == false )            // subsequent calls abandoned here
    {
      s_logger->repx(logga::dbug, "repeat invokation", "");
      s_logger->repx(logga::xtra, "abandoning function without action", "");
      return true;
    }
  firstInvocation = false;                   // update status

  // ---------------------------------
  //  member data integrity check
  // ---------------------------------

  if ( ! d_allDomcons.empty() )              // should never get here
    {
      const int nonzero = d_allDomcons.size();
      s_logger->repx(logga::warn, "domain controllers vector not empty", nonzero);
      s_logger->repx(logga::xtra, "abandoning traversal without action", "");
      return false;
    }

  // ---------------------------------
  //  preamble
  // ---------------------------------

  // for logging
  std::ostringstream put;

  // load the domain controllers vector into a more convenient popable container
  SmartPtrPopper<DomainController> origDomcons;   // simple popable container class
  origDomcons.load(d_originatingDomcons);         // shared pointers elements copied over

  // declare some entity variables -- the shared pointers are naturally empty
  shared_ptr<DomainController> dc;
  shared_ptr<Gateway>          gw;

  // declare some administrative variables
  bool crossEdgeFlag;                        // 'true' if a cross edge was found

  // ---------------------------------
  //  starting position
  // ---------------------------------

  dc               = origDomcons.pop();      // grab first originating domain, must exist
  d_componentCount = 1;                      // now unity, at least one vertex present [1]
  crossEdgeFlag    = false;                  // edge which connects two spanning trees [2]

  // [1] a component of a graph is a maximally connected
  // sub-graph -- in lay terms, this is the largest possible
  // navigable subset of the original graph
  //
  // [2] that is, an edge which connects the spanning trees which
  // derive from any two (necessarily distinct) originating
  // domains

  // ---------------------------------
  //  main loop
  // ---------------------------------

  // CAUTION: the single "=" (copy assignment operator) in the
  // various selection statements ('if' and perhaps 'switch' ) is
  // normally correct

  // [3] (see below) a hyphen represents a domain visited flag
  // [4] (see below) a tilde represents a selgate visited flag

  s_logger->repx(logga::dbug, "* DFS loop starts", "");

  while ( true )                             // a clear idiom but not the most compact
    {
      s_logger->repx(logga::adhc, "- loop start with domcon", dc->getIdAndKind());

      // ZERO: add the current domain if not marked, otherwise
      // note the presence of a cross edge

      if ( dc->getHyphen() )                 // [3]
        {
          s_logger->repx(logga::adhc, "0 domcon with hyphen", dc->getIdAndKind());
          crossEdgeFlag = true;
          s_logger->repx(logga::adhc, "0 cross-edge set", crossEdgeFlag);
        }
      else
        {
          s_logger->repx(logga::adhc, "0 domcon without hyphen", dc->getIdAndKind());
          dc->markHyphen();                  // duly mark
          s_logger->repx(logga::adhc, "0 domcon now hyphened", dc->getHyphen());
          d_allDomcons.push_back(dc);        // capture by copying in smart pointer
          s_logger->repx(logga::adhc, "0 all domcon added, size", d_allDomcons.size());
        }

      // FIRST: attempt to head deep

      if ( (gw = dc->lowestNoTildeSel()) )   // lowest not-tilde'd selgate [4]
        {
          s_logger->repx(logga::adhc, "1 lowest no tilde sel", gw->getIdAndKind());
          gw->markTilde();                   // mark tilde
          s_logger->repx(logga::adhc, "1 gateway tilded", gw->getIdAndKind());
          dc = gw->hop(dc);                  // buy-side hop
          s_logger->repx(logga::adhc, "1 hopped buyward", dc->getIdAndKind());
          dc->pushTilde(gw);                 // push the gateway, also collect the buygate
          s_logger->repx(logga::adhc, "1 domain push tilde", gw->getIdAndKind());
        }

      // SECOND: attempt to back-track

      else if ( (gw = dc->popTilde()) )      // pop earlier push
        {
          s_logger->repx(logga::adhc, "2 popped tilde", gw->getIdAndKind());
          dc = gw->hop(dc);                  // sel-side hop
          s_logger->repx(logga::adhc, "2 hopped selward", dc->getIdAndKind());
        }

      // THIRD: move to next buygate domain

      else if ( (dc = origDomcons.pop()) )   // simple next
        {
          // CAUTION: do nothing active is correct
          if ( crossEdgeFlag == false ) ++d_componentCount;
          crossEdgeFlag = false;             // reset flag
          s_logger->repx(logga::adhc, "3 next domcon", dc->getIdAndKind());
        }

      // FOURTH: termination

      else
        {
          s_logger->repx(logga::adhc, "4 termination", "");
          break;                             // from the 'while' block
        }
    } // while true

  s_logger->repx(logga::adhc, "* DFS loop finished", "");

  // ---------------------------------
  //  traversal reporting
  // ---------------------------------

  const std::string notes =
    "    notes: the originating domains are listed by decreasing importance\n"
    "           a component is an energy island, more formally, each largest"
    " possible navigable subset of the original domain graph";
  put << "  DFS (depth-first search): success: traversal complete"       << "\n"
      << "    originating domains count     : " << getOrigDomconsCount() << "\n"
      << "    all domains count             : " << getAllDomconsCount()  << "\n"
      << "    domain graph components count : " << getComponentsCount()  << "\n"
      << "\n"
      << notes                                                           << "\n";
  s_logger->repx(logga::dbug, "in-depth reporting follows", "");
  s_logger->putx(logga::dbug, put);

  // ---------------------------------
  //    flag resetting
  // ---------------------------------

  // reset the tilde and hyphen flags as a precaution

  BOOST_FOREACH( shared_ptr<DomainController> dc, d_allDomcons )
    {
      dc->resetTildeSelgates();              // 'DomainController's continue the resetting
      dc->resetHyphen();
    }

  // ---------------------------------
  //  abandon if yeek 23
  // ---------------------------------

  // additional action as appropriate
  // YEEK 23 CODE (set by '--yeek')
  if ( xeona::yeek == 23 )
    {
      std::string msg;
      msg += "at the end of function '";
      msg += __func__;
      msg += "'";
      s_logger->repx(logga::warn, "will throw xeona::yeek_abandon", xeona::yeek);
      throw xeona::yeek_abandon(msg);
    }

  // ---------------------------------
  //    return success
  // ---------------------------------

  s_logger->repx(logga::dbug, "normal exiting of member function", "");
  return true;                               // should always get here (yeek 23 aside)
}

//  end of file

