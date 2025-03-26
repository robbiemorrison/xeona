//  XEONA ENERGY SYSTEMS MODELING ENVIRONMENT
//
//  file-create-name : overseer.h
//  file-create-date : Thu 07-Aug-2008 19:53 UTC
//  file-initiator   : Robbie Morrison <robbie@actrix.co.nz>
//
//  file-role        : top-level overseer entity (singleton) / header
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
//  $URL: file:///home/robbie/svn-root/xeona/futz/trunk/xeona1/b/overseer.h $

//  HEADER GUARD

#ifndef _OVERSEER_H_
#define _OVERSEER_H_

//  LOCAL AND SYSTEM INCLUDES

#include "../b/entity.h"      // entity base class plus lazy linking
#include "../b/costreg.h"     // cost registers

#include "../a/logger_fwd.h"  // some forward declarations from "logger.h"
#include "../c/smart_ptr.h"   // toggle between Boost and TR1 smart pointers

#include <string>             // C++ strings
#include <sstream>            // string-streams
#include <vector>             // STL sequence container

//  FORWARD (PARTIAL) DECLARATIONS

class Record;                                // constructor argument
class DomainController;                      // data member

//  CODE

// ---------------------------------------------------------
//  CLASS           : Overseer
// ---------------------------------------------------------
//  Description  : top level simulation entity
//  Role         : contains 'run' function invoked by free function 'xeona::simulate'
//  Techniques   : unenforced singleton
//  Status       : complete
//
//  Design notes
//
//      Unenforced mandatory singleton
//
//          This is an unenforced singleton, but warnings are
//          issued for multiple instantiations.
//
//          In terms of model requirements, this object is on the
//          mandatory list and must carry the identifier defined
//          by the 'xeona::overseer' string literal in
//          'common.cc' (probably set to "overseer").
//
//      Ranked originating domain controllers
//
//          First up, domain controllers are also known as
//          "commitment domains" or simply "domains" for short.
//
//          Only the so-called originating domains are specified
//          and then in descending rank order.  The remaining
//          domains are discovered naturally during the domain
//          graph traversal process.
//
//          The domain controllers, in total, form a directed
//          graph.  The various restrictions on this graph are
//          given in my (Robbie Morrison) PhD thesis and are not
//          listed here.
//
// ---------------------------------------------------------

class Overseer :                   // does NOT inherit from class 'TicToc' nor 'Actor'
  public FullEntity,
  public virtual CostRegister,     // provides the set of registers
  public CostRegisterOverseer
{
  // DISABLED

private:

  Overseer();                                     // zero-argument constructor
  Overseer(const Overseer& orig);                 // copy constructor
  Overseer& operator= (const Overseer& orig);     // copy assignment operator

  // CREATORS

public:

  explicit
  Overseer
  (const std::string entityId,
   Record&           record);

  virtual
  ~Overseer();

  // MANIPULATORS

public:

  virtual
  void
  factoryInitialize();                       // called by factory after construction

  // SIMULATION POINT OF ENTRY

public:

  bool
  run
  (const int steps);                         // run with full or restricted horizon

  // INTERNAL DATA

private:

  std::string&                                  d_captrans_algorithm; // selection
  std::string&                                  d_ranked_orig_domains;
  std::vector<shared_ptr<DomainController> >    d_rankedOrigDomains;  // descending rank

  // STATIC DATA

private:

  static unsigned    s_count;                // instance counter

};

//  ==== XEDOC =================================================
//
//  entity.overseer
//
//      class                                    > Overseer
//
//        the Overseer entity is REQUIRED and the 'overseer'
//        identifier is MANDATORY
//
//        the overseer does little more that invoke the various
//        originating domains in nominated order at each new
//        interval
//
//      builtin-remark s                         <
//
//      captrans-algorithm s                     > "simple"
//
//        captrans-algorithm takes 'fixed' | 'simple' | 'hop-relit'
//        but only 'simple' is currently implemented (this call
//        contains experimental macro-controlled hop-relit code)
//
//      ranked-orig-domains L                    > ""
//    # ranked-orig-domains L                    > "domain-controller-1"
//
//        the originating domain controllers must be given in
//        order of DESCENDING priority, any unannounced domains
//        will be discovered naturally during the various
//        traversals -- an originating domain must contain at
//        least one source entity (equivalently, demand sink)
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
//        the cost-type totals cover the entire horizon, with
//        first step truncation given by program.last-run.run-kind
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
//  ============================================================

#endif // _OVERSEER_H_

//  end of file

