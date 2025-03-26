//  XEONA ENERGY SYSTEMS MODELING ENVIRONMENT
//
//  file-create-name : optctl.h
//  file-create-date : Fri 17-Oct-2008 14:36 UTC
//  file-initiator   : Robbie Morrison <robbie@actrix.co.nz>
//
//  file-role        : control OSPs for asset operators / header
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
//  $URL: file:///home/robbie/svn-root/xeona/futz/trunk/xeona1/b/optctl.h $
//
//  GENERAL NOTES FOR THIS FILE

//  HEADER GUARD

#ifndef _OPTCTL_H_
#define _OPTCTL_H_

//  LOCAL AND SYSTEM INCLUDES

#include "../b/optprob.h"     // optimization sub-problem and key sub-classes

#include "../a/logger_fwd.h"  // some forward declarations from "logger.h"
#include "../c/smart_ptr.h"   // toggle between Boost and TR1 smart pointers

#include <string>             // C++ strings

//  FORWARD (PARTIAL) DECLARATIONS

class LmpBidSet;

//  CODE

// ---------------------------------------------------------
//  CLASS           : CtlFirstFeasible_A
// ---------------------------------------------------------
//  Description  : implements first feasible solution
//  Role         : supports 'AsopFirstFeasible'
//  Techniques   : supports 'xeona::e_adminFirst'
//  Status       : complete
// ---------------------------------------------------------

class CtlFirstFeasible_A :
  public ControlOsp
{
  // DISABLED

private:

  CtlFirstFeasible_A();                                            // zero-argument ctor
  CtlFirstFeasible_A(const CtlFirstFeasible_A& orig);              // copy constructor
  CtlFirstFeasible_A& operator= (const CtlFirstFeasible_A& orig);  // copy assignment oper

  // CREATORS

public:

  CtlFirstFeasible_A
  (shared_ptr<svif::SolverIf> solver,
   const xeona::DomainMode    commitmentMode);

  virtual
  ~CtlFirstFeasible_A();

  // FILL PROBLEM CALLS

public:

  void
  uploadNullControl();

  // SPECIALIZED PUSH CALLS

protected:

  virtual
  const int
  pushObj                                    // no value required, simply loads zero
  (const std::string tag = "");

}; // class 'CtlFirstFeasible_A'

// ---------------------------------------------------------
//  CLASS           : CtlMeritOrder_A
// ---------------------------------------------------------
//  Description  : implements prescribed order control
//  Role         : supports 'AsopPrescribedOrder'
//  Techniques   : requires 'xeona::e_adminMerit'
//  Status       : complete -- except unit limit constraint needs adding
//
//  CAUTION: testing
//
//      This OSP needs operational testing.
//
// ---------------------------------------------------------

class CtlMeritOrder_A :
  public ControlOsp
{
  // TYPEDEFS

public:

  typedef boost::tuple                       // can be local or global cols
  <int>                                      // duty control
  index_type;

  // DISABLED

private:

  CtlMeritOrder_A();                                         // zero-argument constructor
  CtlMeritOrder_A(const CtlMeritOrder_A& orig);              // copy constructor
  CtlMeritOrder_A& operator= (const CtlMeritOrder_A& orig);  // copy assignment operator

  // CREATORS

public:

  CtlMeritOrder_A
  (shared_ptr<svif::SolverIf> solver,
   const xeona::DomainMode    commitmentMode);

  virtual
  ~CtlMeritOrder_A();

  // FILL PROBLEM CALLS

public:

  index_type                       // duty control gol
  uploadRank
  (const int    rank,              // a merit order from the series { 1, 2, 3, ..., 1024 }
   const double ceilingDuty);      // used to set a "big M"-style structural coefficient

  void
  uploadUnitLimit                  // limit the number of units that can be run at once
  (const int unitLimit);

  // SPECIALIZED PUSH CALLS

protected:

//    const int
//    pushObj
//    (const double objValue,
//     std::string  tag = "");

private:

  int                 d_unitLimit;           // optional limit
  std::vector<int>    d_rankings;            // ascending sorted rank values

}; // class 'CtlMeritOrder_A'

// ---------------------------------------------------------
//  CLASS           : CtlLmpBid_A
// ---------------------------------------------------------
//  Description  : implements LMP (nodal pricing) bidset
//  Role         : supports 'AsopLmpBidStated'
//  Techniques   : requires 'xeona::e_auctionLmp'
//  Status       : complete
// ---------------------------------------------------------

class CtlLmpBid_A :
  public ControlOsp
{
  // TYPEDEFS

public:

  typedef boost::tuple                       // can be local or global cols
  <int>                                      // duty control
  index_type;

  // DISABLED

private:

  CtlLmpBid_A();                                     // zero-argument constructor
  CtlLmpBid_A(const CtlLmpBid_A& orig);              // copy constructor
  CtlLmpBid_A& operator= (const CtlLmpBid_A& orig);  // copy assignment operator

  // CREATORS

public:

  CtlLmpBid_A
  (shared_ptr<svif::SolverIf> solver,
   const xeona::DomainMode    commitmentMode);

  virtual
  ~CtlLmpBid_A();

  // FILL PROBLEM CALLS

public:

  index_type                                 // duty control gol
  uploadBidSet
  (const shared_ptr<LmpBidSet> bidset);

}; // class 'CtlLmpBid_A'

// ---------------------------------------------------------
//  CLASS           : CtlLeastCost_A
// ---------------------------------------------------------
//  Description  : implements least short-run cost control
//  Role         : supports 'AsopLeastCost'
//  Techniques   : requires 'xeona::e_shortrunModes' -- with cost type supplied by domain
//  Status       : complete
//
//  Design notes
//
//      The 'uploadShortrunCosts' call is optional.  However good
//      style might suggest it be called with a zeroed cost set.
//
//      Note the various 'OptimSubProb::downloadShortrunCosts'
//      calls, which also serve our purposed here with
//      modification.
//
// ---------------------------------------------------------

class CtlLeastCost_A :
  public ControlOsp
{
  // DISABLED

private:

  CtlLeastCost_A();                                        // zero-argument constructor
  CtlLeastCost_A(const CtlLeastCost_A& orig);              // copy constructor
  CtlLeastCost_A& operator= (const CtlLeastCost_A& orig);  // copy assignment operator

  // CREATORS

public:

  CtlLeastCost_A
  (shared_ptr<svif::SolverIf> solver,
   const xeona::DomainMode    commitmentMode);

  virtual
  ~CtlLeastCost_A();

  // FILL PROBLEM CALLS

public:

  void
  uploadShortrunCosts
  (const CostSet& shiftCosts);

}; // class 'CtlLeastCost_A'

// ---------------------------------------------------------
//  CLASS           : CtlQuan_A
// ---------------------------------------------------------
//  Description  : implements demand quantity
//  Role         : supports 'AsopInelasticTs'
//  Techniques   : (nothing special)
//  Status       : complete
// ---------------------------------------------------------

class CtlQuan_A :
  public ControlOsp
{
  // TYPEDEFS

public:

  typedef boost::tuple                       // can be local or global cols
  <int>                                      // duty control
  index_type;

  // DISABLED

private:

  CtlQuan_A();                                    // zero-argument constructor
  CtlQuan_A(const CtlQuan_A& orig);               // copy constructor
  CtlQuan_A& operator= (const CtlQuan_A& orig);   // copy assignment operator

  // CREATORS

public:

  CtlQuan_A
  (shared_ptr<svif::SolverIf> solver,
   const xeona::DomainMode    commitmentMode);

  virtual
  ~CtlQuan_A();

  // FILL PROBLEM CALLS

public:

  index_type                                 // duty control gol
  uploadControl
  (const double demand);                     // demanded quantity

  // INSTANCE DATA

private:

  index_type    d_cols;                      // local cols

}; // class 'CtlQuan_A'

#endif // _OPTCTL_H_

//  end of file

