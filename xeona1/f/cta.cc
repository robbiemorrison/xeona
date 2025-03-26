//  XEONA ENERGY SYSTEMS MODELING ENVIRONMENT
//
//  file-create-name : cta.cc
//  file-create-date : Fri 06-Feb-2009 15:12 UTC
//  file-initiator   : Robbie Morrison <robbie@actrix.co.nz>
//
//  file-role        : captrans algorithm / implementation
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
//  $URL: file:///home/robbie/svn-root/xeona/futz/trunk/xeona1/f/cta.cc $

//  LOCAL AND SYSTEM INCLUDES

#include "cta.h"              // companion header for this file (place first)

#include "../c/util2.h"       // free functions which offer general utilities 2
#include "../c/util1.h"       // free functions which offer general utilities 1
#include "../b/gate.h"        // gateway entity
#include "../b/domcon.h"      // domain controller entity
#include "../a/recorder.h"    // summarizing recorder class

#include "../a/logger.h"      // standard logging functionality (as required)
#include "../c/smart_ptr.h"   // toggle between Boost and TR1 smart pointers
#include ".././common.h"      // common definitions for project (place last)

#include <string>             // C++ strings
#include <sstream>            // string-streams

#include <boost/foreach.hpp>  // BOOST_FOREACH iteration macro

//  CODE

// ---------------------------------------------------------
//  FREE FUNCTION   : ::formatLeader
// ---------------------------------------------------------
//  Description  : returns formatted leader string like "  CAPSET-01 (00)"
//  Role         : support reporting
//  Techniques   : string stream
//  Status       : complete
// ---------------------------------------------------------

namespace
{
  std::string
  formatLead
  (
   const std::string& label,
   const int          step,
   const int          count)
  {
    const int pad = 2;
    std::ostringstream oss;
    oss << "  "
        << label
        << "-"
        << std::setw(pad) << std::setfill('0') << count
        << " ("
        << std::setw(pad) << std::setfill('0') << step
        << ")";
    return oss.str();
  }

} // unnamed namespace

// ---------------------------------------------------------
//  CLASS           : CapTransAlg
// ---------------------------------------------------------

// STATIC DEFINITIONS - with and without explicit initialization

logga::spLogger CapTransAlg::s_logger = logga::ptrLogStream();

// MEMBER FUNCTIONS

// ---------------------------------------------------------
//  MEMBER FUNCTION : CapTransAlg
// ---------------------------------------------------------

CapTransAlg::CapTransAlg
(const int                                         step,
 const std::vector<shared_ptr<DomainController> >& domcons) :  // order significant
   d_step(step),
   d_originatingDomcons(domcons)
{
  s_logger->repx(logga::xtra,
                 "constructor call, orig domcon count",
                 d_originatingDomcons.size());
}

// ---------------------------------------------------------
//  MEMBER FUNCTION : ~CapTransAlg
// ---------------------------------------------------------

CapTransAlg::~CapTransAlg()
{
  s_logger->repx(logga::adhc, "destructor call", "");
}

// ---------------------------------------------------------
//  MEMBER FUNCTION : CtaFixed
// ---------------------------------------------------------

CtaFixed::CtaFixed
(const int                                         step,
 const std::vector<shared_ptr<DomainController> >& domcons) :
  CapTransAlg(step, domcons)
{
  s_logger->repx(logga::warn, "not fully implemented", "");
  std::ostringstream put;
  put << "  the \"fixed\" version of the captrans algorithm is not implemented"   << "\n"
      << "  try the \"simple\" or \"hop-relit\" versions instead"                 << "\n";
  s_logger->putx(logga::xtra, put);
}

// ---------------------------------------------------------
//  MEMBER FUNCTION : CtaFixed::captrans
// ---------------------------------------------------------

tribool                           // 'indeterminate' if no originating domains
CtaFixed::captrans
(const xeona::DomainMode capacityMode)
{
  const tribool ret = false;
  s_logger->repx(logga::warn, "not yet implemented, return", ret);
  return ret;
}

// ---------------------------------------------------------
//  MEMBER FUNCTION : CtaSimple
// ---------------------------------------------------------

CtaSimple::CtaSimple
(const int                                         step,
 const std::vector<shared_ptr<DomainController> >& domcons) :
  CapTransAlg(step, domcons)
{
  s_logger->repx(logga::dbug, "constructor call", "");
}

// ---------------------------------------------------------
//  MEMBER FUNCTION : ~CtaSimple
// ---------------------------------------------------------

CtaSimple::~CtaSimple()
{
  s_logger->repx(logga::adhc, "destructor call", "");
}

// ---------------------------------------------------------
//  MEMBER FUNCTION : captrans
// ---------------------------------------------------------
//  Description  : traverse domain graph and make 'capset' and 'transolve' calls
//  Role         : system solution, called by 'Overseer' singleton
//  Techniques   : graph traversal
//  Status       : complete -- but note the macro-controlled hop-relit code
//
//  Design notes
//
//      This function implements the "CapTrans" algorithm.
//
//      The commitment domain graph is implicit -- meaning that a
//      dedicated graph container is not used.
//
//  Briefly
//
//        capset     =  gateway capacity setting method
//        transolve  =  domain transaction solving method
//
//      The notion of under-capacitated means that the gateway in
//      question has not yet been capset, but which does of
//      course still possess innate limits as part of its
//      specification.
//
//  Logging and event recording optional
//
//      If need be, member function calls based on logger
//      instance 's_logger' and class 'EventRecorder' instance
//      'ctalog' can be deleted to make the code cleaner.
//
//  CAUTION: hop-relit code here a temporary measure
//
//      The 'XE_CTA' macro is a temporary measure being used
//      during algorithm development.  Later on, the code should
//      be split into two and added to each of the correct
//      classes.  That will thereby allow XEM-level control.
//
// ---------------------------------------------------------

// set the form of algorithm to be used during development
//
//   1 = without relitigation
//   2 = with hop-relitigation

#if !defined(XE_CTA)                         // allows external override via 'XE_CTA'
# define XE_CTA 1                            // set the default, see above for definitions
#endif

// protect against inappropriate macro values

#if   (XE_CTA == 1)
#elif (XE_CTA == 2)
#else
# error "XE_CTA macro not set to a supported value {1,2}"
#endif

tribool                                      // 'indeterminate' if no originating domains
CtaSimple::captrans
(const xeona::DomainMode capacityMode)
{
  // ---------------------------------
  //  preamble
  // ---------------------------------

  const logga::Rank reportLevel = s_logger->getReportLevelRank();

  int count = 0;                             // output counter
  std::ostringstream put;                    // for logging

  // ---------------------------------
  // event recorder (for reporting)
  // ---------------------------------

  EventRecorder ctalog("cta", d_step);       // see 'a/recorder.ut0.cc' for sample usage

  // ---------------------------------
  //  data integrity checks
  // ---------------------------------

  if ( ! xeona::isTwoContained(capacityMode, xeona::e_capacityModes) )
    {
      std::ostringstream oss;
      oss << capacityMode << " " << xeona::e_capacityModes;
      s_logger->repx(logga::warn, "inappropriate capacity mode", oss.str());
      return false;
    }

  if ( d_originatingDomcons.empty() )
    {
      s_logger->repx(logga::rankNoData, "no originating domain controllers", "");
      return indeterminate;
    }

  // ---------------------------------
  //  string format 'step'
  // ---------------------------------

  // for convenience
  const int step = d_step;

  // note the the 'Boost.Format' library does not
  // support the ANSI C 'printf' * width and
  // precision specifier

  const int span = Entity::getHorizonSteps();
  std::string formatString;                       // for 'boost::format' call
  if      ( span <   100 ) formatString = "%02d"; //     02 thru 99
  else if ( span < 10000 ) formatString = "%04d"; //   0002 thru 9999
  else                     formatString = "%06d"; // 000002 and above
  const std::string strStep = boost::str(boost::format(formatString) % step);

  // ---------------------------------
  //  preamble
  // ---------------------------------

  // load the domain controllers vector into a more convenient popable container
  SmartPtrPopper<DomainController> origDomcons;   // simple popable container container
  origDomcons.load(d_originatingDomcons);         // shared pointers elements copied over

  // declare some variables -- these shared pointers are naturally empty
  shared_ptr<DomainController> dc;
  shared_ptr<Gateway>          gw;

  // ---------------------------------
  // variation specific code
  // ---------------------------------

#if   (XE_CTA == 1) // without relitigation
#elif (XE_CTA == 2) // with hop-relitigation

  bool looseflag = false;

#endif // macro 'XE_CTA'

  // ---------------------------------
  //  CTA reset loop
  // ---------------------------------

  // There are two options here:
  //
  //   * deploy a domain graph traversal as seen in
  //     'Overseer::run'
  //
  //   * use looped calls as per 'xeona::registerGates' and its
  //     helper function
  //
  // The second method is currently used here -- but that
  // decision could be revisited.

  std::vector<shared_ptr<DomainController> > domcons = Entity::retDomains();
  BOOST_FOREACH( shared_ptr<DomainController> domcon, domcons )
    {
      std::vector<shared_ptr<Gateway> > gates = domcon->retSelgates();
      BOOST_FOREACH( shared_ptr<Gateway> gate, gates )
        {
          gate->reset();
        }
    }

  // ---------------------------------
  //  abandon if yeek 29
  // ---------------------------------

  // additional action as appropriate
  // YEEK 29 CODE (set by '--yeek')
  if ( xeona::yeek == 29 )
    {
      std::string msg;
      msg += "part way thru function '";
      msg += __func__;
      msg += "', main loop code next";
      s_logger->repx(logga::warn, "will throw xeona::yeek_abandon", xeona::yeek);
      throw xeona::yeek_abandon(msg);
    }

  // ---------------------------------
  //  CTA starting position
  // ---------------------------------

  // grab first originating domain
  dc = origDomcons.pop();
  ctalog.event("pop",
               "originating domcon " + dc->getIdAndKind(),
               "cta starting position");

  // ---------------------------------
  //  CTA main loop
  // ---------------------------------

  // CAUTION: the single "=" (copy assignment operator) in the
  // various selection statements ('if' and perhaps 'switch' ) is
  // normally correct

  while ( true )                             // a clear idiom but not the most compact
    {
      s_logger->repx(logga::adhc, "- loop start with domcon", dc->getIdAndKind());
      ctalog.event("loop", "entering main loop", "");

      // FIRST: try to back-track along the next (theta) unmarked buygate

      if ( (gw = dc->lowestNoThetaBuy()) )        // lowest not-theta'ed buygate
        {
          s_logger->repx(logga::adhc, "1 lowest no theta buy", gw->getIdAndKind());
          gw->markTheta();                        // mark theta
          s_logger->repx(logga::adhc, "1 buygate now theta'ed", gw->getTheta());
          shared_ptr<DomainController> from = dc; // used by 'ctalog'
          ctalog.mark("theta", gw, dc, "lowest no theta buy now theta'ed");
          dc = gw->hop(dc);                       // sel-side hop
          s_logger->repx(logga::adhc, "1 hopped selward", dc->getIdAndKind());
          ctalog.hop(gw, from, dc,  "lowest no theta buy");
        }

      // SECOND: try to back-track along the next unsold selgate, but capset en route

      else if ( (gw = dc->lowestNotTransSel()) )  // lowest transaction unsolved selgate
        {
          s_logger->repx(logga::adhc, "2 lowest not sold buy", gw->getIdAndKind());
          const std::string gate = gw->getIdAndKind();
          ctalog.event("select", "gateway " + gate, "lowest not transacted sel");

          if ( gw->getHash() == false )      // not fully capacitated
            {
              ctalog.capset(dc, gw);
              if ( dc->capset(step, gw, capacityMode) == false ) // set capacity bounds
                {
                  s_logger->repx(logga::adhc, "2 capset call failed", dc->getIdAndKind());

                  ctalog.alert("failure reason not available");
                  put << ctalog.output();    // see class 'EventRecorder'
                  s_logger->addSmartBlank(logga::dbug);
                  s_logger->putx(logga::dbug, put);

                  return false;
                }
              s_logger->repx(logga::adhc, "2 capset call succeeded", "");
              put << ::formatLead("CAPSET", step, ++count)
                  << " success : "
                  << gw->getIdAndKind() << " capacity set by "
                  << dc->getIdAndKind() << "\n";
              s_logger->putx(logga::adhc, put);

            }
          else
            {
              // no need to capacity set ('capset' would return
              // early in any case)
            }

          shared_ptr<DomainController> from = dc; // used in cta summary
          dc = gw->hop(dc);                       // buy-side hop
          s_logger->repx(logga::adhc, "2 hopped buyward", dc->getIdAndKind());
          ctalog.hop(gw, from, dc, "lowest no theta buy");

        }
      else
        {

          // THIRD: attempt to transolve

          ctalog.transolve(dc);
          if ( dc->transolve(step, capacityMode) == false )      // note side-effect
            {

#if   (XE_CTA == 1) // without relitigation

              s_logger->repx(logga::adhc, "3 transolve failed", dc->getIdAndKind());
              put << "  CTA (" << strStep << ")"
                  << ": could not find a feasible solution, that is, unable\n"
                  << "  to identify adequate or sufficiently flexible capacity"   << "\n"
                  << "      domain : " << dc->getIdentifier()                     << "\n"
                  << "      step   : " << step                                    << "\n";
              s_logger->putx(logga::warn, put);

              ctalog.alert("could not find feasible solution");
              put << ctalog.output();        // see class 'EventRecorder'
              s_logger->addSmartBlank(logga::dbug);
              s_logger->putx(logga::dbug, put);

              return false;

#elif (XE_CTA == 2) // with hop-relitigation (compiles as of r4634 and r4773)

  // tofix: 16-Jul-2010: hop-relit: does 'looseflag' ever need to be reset
  // .. or made domain-specific? -- important only when this code is actioned

              // "looseness" test, used only for reporting
              if ( dc->lowestUnderCapBuy() &&     // lowest not-fully-capped buygate
                   dc->lowestSel() )              // lowest (meaning any) selgate
                {
                  looseflag = true;               // potential problems ahead

                  put << "  CTA (" << strStep << ")"
                      << ": identified a structure which cannot be totally capacitated\n"
                      << "  and which may lead to a false infeasibility"          << "\n"
                      << "      domain : " << dc->getIdentifier()                 << "\n"
                      << "      step   : " << step                                << "\n";
                  s_logger->putx(logga::warn, put);

                  ctalog.alert("structure cannot be totally capacitated");
                }

              // hop-relitigate code
              if ( (gw = dc->lowestUnderCapSel()) )    // lowest not-fully-capped selgate
                {
                  while ( shared_ptr<Gateway> temp = dc->lowestUnderCapSel() )
                    {
                      temp->unsetTransaction();   // set transaction status to not-sold
                      ctalog.unmark("sold", gw, dc, "me and all higher selgates");
                    }
                  gw->unmarkStar();                    // set mark to false
                  ctalog.unmark("star", gw, dc, "the me gateway referred to above");
                  dc->capset(step, gw, capacityMode);  // set lower and upper capacities
                  shared_ptr<DomainController> from = dc;   // used by 'ctalog'
                  dc = gw->hop(dc);                    // hop right
                  ctalog.hop(gw, from, dc, "hop right");
                }
              // no more hop-relitigate possibilities exist
              else
                {
                  put << "  CTA (" << strStep << ")"
                      << ": could not find a feasible solution, that is, unable to\n"
                      << "  identify adequate or sufficiently flexible capacity"  << "\n"
                      << "      domain : " << dc->getIdentifier()                 << "\n"
                      << "      step   : " << step                                << "\n";
                  s_logger->putx(logga::warn, put);

                  if ( looseflag )
                    {
                      put << "  CTA (" << strStep << ")"
                          << ": capacity relitigation questionable"
                          << "\n";
                      s_logger->putx(logga::warn, put);
                    }
                  else
                    {
                      put << "  CTA (" << strStep << ")"
                          << ": capacity relitigation robust"
                          << "\n";
                      s_logger->putx(logga::warn, put);
                    }

                  ctalog.alert("unable to find feasible solution");
                  put << ctalog.output();    // see class 'EventRecorder'
                  s_logger->addSmartBlank(logga::dbug);
                  s_logger->putx(logga::dbug, put);

                  return false;
                }

#endif // macro 'XE_CTA'

            } // 'transolve' returned 'false'

          // FOURTH: 'transolve' returned 'true'

          else
            {
              s_logger->repx(logga::adhc, "4 transolve succeeded", dc->getIdAndKind());
              put << ::formatLead("TRANSOLVE", step, ++count)
                  << " success : "
                  << dc->getIdAndKind() << "\n";
              s_logger->putx(logga::adhc, put);

              // record transaction
              while ( gw = dc->lowestNotTransBuy() )
                {
                  // next call naturally creates a 'foreach'
                  // loop, moreover the gateway can recover its
                  // own transaction solution from the solver
                  shared_ptr<BuySide> temp = dynamic_pointer_cast<BuySide>(gw);
                  const double duty = temp->recordTransaction();

                  // reporting
                  const std::string id = gw->getIdAndKind();
                  s_logger->repx(logga::adhc, "4 transaction recorded", id);
                  s_logger->repx(logga::adhc, "4 transaction", duty);
                  ctalog.event("record", "transaction for " + gw->getIdAndKind(), duty);
                }

              // strictly optional block
              // YEEK 31 CODE (set by '--yeek')
              if ( xeona::yeek == 31 )
                {
                  s_logger->repx(logga::dbug, "optional code begins, yeek", xeona::yeek);

                  // actively undertake full capacitation --
                  // strictly optional and computationally
                  // expensive
                  while ( gw = dc->lowestUnderCapSel() )
                    {
                      s_logger->repx(logga::adhc,
                                     "4 undercapacitated gate to capset",
                                     gw->getIdAndKind());
                      dc->capset(step, gw, capacityMode);
                    }

                  s_logger->repx(logga::dbug, "optional code ends, yeek", xeona::yeek);
                } // YEEK 31

              // move to next buygate domain
              if ( (gw = dc->lowestNoStarBuy()) ) // lowest not-star'ed buygate
                {
                  s_logger->repx(logga::adhc, "4 lowest no star buy", gw->getIdAndKind());
                  gw->markStar();                 // mark star
                  s_logger->repx(logga::adhc, "4 starred", gw->getStar());
                  shared_ptr<DomainController> from = dc;   // used in cta summary
                  dc = gw->hop(dc);               // sel-side hop
                  s_logger->repx(logga::adhc, "4 hopped selward", dc->getIdAndKind());
                  ctalog.hop(gw, from, dc, "lowest no star buy");
                }
              else if ( (dc = origDomcons.pop()) )     // simple next
                {
                  s_logger->repx(logga::adhc, "4 popped domcon", dc->getIdAndKind());
                  const std::string domcon = dc->getIdAndKind();
                  ctalog.event("pop", "pop next originating domcon " + domcon, "");
                  // CAUTION: do nothing is correct
                }
              else
                {
                  // in some cases, tone down the amount of reporting
                  if ( reportLevel > logga::warn  // always if the reporting level is high
                       ||
                       step < 168                 // else up to one week as hours
                       ||
                       step % 10 == 0             // .. then every tenth step
                       ||
                       step >= span - 2 )         // .. and the last two steps
                    {
                      s_logger->repx(logga::adhc, "4 finishing", "");
                      put << "  CTA (" << strStep << ")"
                          << ": success: able to find a feasible solution set"
                          << "\n";
                      s_logger->putx(logga::warn, put);    // does not count as a "WARN"
                    }

                  ctalog.note("about to return success");
                  put << ctalog.output();              // see class 'EventRecorder'
                  s_logger->addSmartBlank(logga::dbug);
                  s_logger->putx(logga::dbug, put);

                  return true;
                }
            }

        }

    } // while true

} // function 'CtaSimple::captrans'

//  end of file

