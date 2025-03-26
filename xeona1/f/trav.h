//  XEONA ENERGY SYSTEMS MODELING ENVIRONMENT
//
//  file-create-name : trav.h
//  file-create-date : Wed 11-Feb-2009 13:42 UTC
//  file-initiator   : Robbie Morrison <robbie@actrix.co.nz>
//
//  file-role        : domain graph traversal class / header
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
//  $URL: file:///home/robbie/svn-root/xeona/futz/trunk/xeona1/f/trav.h $
//
//  GENERAL NOTES FOR THIS FILE
//
//  The code in this file is based on the unit 'cta' and in
//  particular code in the CTA reset procedure.

#ifndef _TRAV_H_
#define _TRAV_H_

//  LOCAL AND SYSTEM INCLUDES

#include "../a/logger_fwd.h"  // some forward declarations from "logger.h"
#include "../c/smart_ptr.h"   // toggle between Boost and TR1 smart pointers

#include <string>             // C++ strings
#include <vector>             // STL sequence container

#include <boost/tuple/tuple.hpp>                  // n-tuples, ref(), cref()

//  FORWARD (PARTIAL) DECLARATIONS

class DomainController;
class CostSet;

//  TYPEDEFS

typedef void (DomainController::* funcPtrVoid)();
typedef void (DomainController::* funcPtrStep)(const int);
typedef void (DomainController::* funcPtrStepCs3)
  (const int, boost::tuple<CostSet, CostSet, CostSet>&);

// The above are pointer-to-member function types, see Stroustrup
// (1997 p419), Stephens etal (2006 pp539-541), Loudon (2003
// pp26-27).  This typedef makes the subsequent syntax easier.

//  CODE

// ---------------------------------------------------------
//  CLASS           : TravDepthFirst
// ---------------------------------------------------------
//  Description  : depth first domain graph traversal, used to call passed-in functions
//  Role         : domain (and thereby selgate) visitation invoked by 'Overseer' singleton
//  Techniques   : depth first search, pointer-to-member function argument (not pretty)
//  Status       : complete
//
//  Design notes
//
//      The 'TravDepthFirst::call' calling syntax needs to be
//      split into TWO statements, as indicated in lines 2 and 3
//      below:
//
//          shared_ptr<TravDepthFirst> dfs(new TravDepthFirst(rankedOrigDomains));
//          funcPtr f = &DomainController::function;
//          const int fCount = dfs->callDomains(f, step);
//
//      The underlying function argument needs to be of the
//      following general type:
//
//          void DomainController::someFunc(const int)
//
//      A graph component is a maximally connected subgraph --
//      thus, in the case of 'xeona', an energy island.
//
// ---------------------------------------------------------

class TravDepthFirst
{
  // DISABLED

private:

  TravDepthFirst();                                         // zero-argument ctor
  TravDepthFirst(const TravDepthFirst& orig);               // copy constructor
  TravDepthFirst& operator= (const TravDepthFirst& orig);   // copy assignment

  // CREATORS

public:

  TravDepthFirst                                                 // calls 'depthFirstTrav'
  (const std::vector<shared_ptr<DomainController> >& domcons);   // ranked domains

  ~TravDepthFirst();

  // ACCESSORS

  int                                        // number of originating domains
  getOrigDomconsCount() const;

  int                                        // total number of domains
  getAllDomconsCount() const;

  int                                        // number of maximally connected sub-graphs
  getComponentsCount() const;

  bool                                       // 'true' indicates success
  getAllDomcons                              // all domains in topological sort order
  (std::vector<shared_ptr<DomainController> >& allDomcons) const;

  // MANIPULATORS - note overloading

  const int                                  // number of visited domains
  callDomains                                // interval-specific call
  (funcPtrStep func,                         // typedef, argument type must match
   const int   step);                        // step passed thru

  const int                                  // number of visited domains
  callDomains                                // start and end of run call
  (funcPtrVoid func);                        // typedef, argument type must match

  const int
  callDomains
  (funcPtrStepCs3                           func,
   const int                                step,
   boost::tuple<CostSet, CostSet, CostSet>& costs);

  // UTILITY FUNCTIONS

private:

  bool                                       // 'true' indicates success
  depthFirstTrav();                          // workhorse algorithm

  // INSTANCE DATA

private:

  const std::vector<shared_ptr<DomainController> >    d_originatingDomcons;

  std::vector<shared_ptr<DomainController> >          d_allDomcons;
  int                                                 d_componentCount;

  // STATIC DATA

protected:

  static logga::spLogger    s_logger;        // shared_ptr to single logger object

};

#endif // _TRAV_H_

//  end of file

