//  XEONA ENERGY SYSTEMS MODELING ENVIRONMENT
//
//  file-create-name : asop1.h
//  file-create-date : Wed 15-Apr-2009 21:52 UTC
//  file-initiator   : Robbie Morrison <robbie@actrix.co.nz>
//
//  file-role        : concrete asset operators 1 / header
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
//  $URL: file:///home/robbie/svn-root/xeona/futz/trunk/xeona1/b/asop01.h $
//
//  GENERAL NOTES FOR THIS FILE

//  HEADER GUARD

#ifndef _ASOP1_H_
#define _ASOP1_H_

//  LOCAL AND SYSTEM INCLUDES

#include "../b/auxs01.h"      // classes for auxiliary model data
#include "../b/asop.h"        // asset operator entity

#include "../a/logger_fwd.h"  // some forward declarations from "logger.h"
#include "../c/smart_ptr.h"   // toggle between Boost and TR1 smart pointers

#include <string>             // C++ strings

//  FORWARD (PARTIAL) DECLARATIONS

// CAUTION: forward declarations given in the code for convenience

//  CODE

// ---------------------------------------------------------
//  CLASS           : AsopBasic
// ---------------------------------------------------------
//  Description  : basic operator
//  Role         : concrete entity
//  Techniques   : intentionally lacks an embedded optimization sub-problem (OSP)
//  Status       : complete
//
//  Design notes
//
//      A null asset operator which contributes no
//      'OspControl'-based optimization sub-problem.
//
// ---------------------------------------------------------

class AsopBasic :
  public AssetOperator
{
  // USING DECLARATIONS

protected:

  using Entity::s_logger;                    // place in common scope for this class

  // DISABLED

private:

  AsopBasic();                                    // zero-argument constructor
  AsopBasic(const AsopBasic& orig);               // copy constructor
  AsopBasic& operator= (const AsopBasic& orig);   // copy assignment operator

  // CREATORS

public:

  explicit
  AsopBasic
  (const std::string entityId,
   Record&           record);

  virtual
  ~AsopBasic();

  // CALLS

public:

  virtual
  const int                                  // number of technical assets processesed
  constrain
  (const xeona::DomainMode capacityMode);

}; // class 'AsopBasic'

//  ==== XEDOC =================================================
//
//  entity.asop-basic-0
//
//      class                                    > AsopBasic
//
//        basic asset operator with a null control objective and
//        without internal cost formation
//
//      builtin-remark s                         <
//
//      technical-assets L                       > "teas-1 teas-2"
//
//        technical-assets in any order
//
//  ============================================================

// ---------------------------------------------------------
//  CLASS           : AsopPrescribedOrder
// ---------------------------------------------------------
//  Description  : prescribed order
//  Role         : concrete entity
//  Techniques   : (nothing special)
//  Status       : complete
//
//  Design notes
//
//      Prescribed merit order asset operator.
//
//      The 'technical-assets' list is given as model data in
//      DESCENDING priority.
//
// ---------------------------------------------------------

class CtlMeritOrder_A;                       // CAUTION: for class declaration typedef

class AsopPrescribedOrder :
  public AssetOperator,
  public AuxHeatLead
{
  // USING DECLARATIONS

protected:

  using Entity::s_logger;                    // place in common scope for this class

  // TYPEDEFS

private:

  // CAUTION: note this simple way of swapping OSP
  // implementations, the "_X" postfix needs changing in this one
  // place only -- note also that an implementation change is
  // required to honor the same function names and signatures,
  // but not necessarily the behavior at large

  typedef CtlMeritOrder_A CtlMeritOrder;     // used for switching implementations

  // DISABLED

  AsopPrescribedOrder();                                            // zero-argument ctor
  AsopPrescribedOrder(const AsopPrescribedOrder& orig);             // copy constructor
  AsopPrescribedOrder& operator= (const AsopPrescribedOrder& orig); // copy assignment opr

  // CREATORS

public:

  explicit
  AsopPrescribedOrder
  (const std::string entityId,
   Record&           record);

  virtual
  ~AsopPrescribedOrder();

  // CALLS

public:

  virtual
  const int                                  // number of technical assets processesed
  constrain
  (const xeona::DomainMode capacityMode);

  // INSTANCE DATA

private:

  shared_ptr<CtlMeritOrder>                  d_ctl;    // specialization required
  std::vector<shared_ptr<CtlMeritOrder> >    d_ctls;

}; // class 'AsopPrescribedOrder'

//  ==== XEDOC =================================================
//
//  entity.asop-prescribed-order-0
//
//      class                                    > AsopPrescribedOrder
//
//        asset operator who implements explicit prescribed order
//        operations
//
//      builtin-remark s                         <
//
//      technical-assets L                       > "teas-1 teas-2"
//
//        technical-assets given in DESCENDING priority
//
//      cogen-heat-lead-weighting [-] f          > 1.0
//
//        the cogen-lead-heat-weighting [0,1] is passed to any
//        co-generation assets to set their lead policy to heat
//        (1.0) or power (0.0) or some intermediate ratio
//
//  ============================================================

// ---------------------------------------------------------
//  CLASS           : AsopInternalCosts
// ---------------------------------------------------------
//  Description  : least cost operator
//  Role         : concrete entity
//  Techniques   : (nothing special)
//  Status       : complete
//
//  Design notes
//
//      Least-cost asset operator in which the cost, one of
//      either { fin, ghg, nox, dep, luc }, is determined by the
//      domain controller.
//
// ---------------------------------------------------------

class CtlLeastCost_A;                        // CAUTION: for class declaration typedef

class AsopInternalCosts :
  public AssetOperator,
  public CostRegisterAsop
{
  // USING DECLARATIONS

protected:

  using Entity::s_logger;                    // place in common scope for this class

  // TYPEDEFS

private:

  // CAUTION: note this simple way of swapping OSP
  // implementations, the "_X" postfix needs changing in this one
  // place only -- note also that an implementation change is
  // required to honor the same function names and signatures,
  // but not necessarily the behavior at large

  typedef CtlLeastCost_A CtlLeastCost;       // used for switching implementations

  // DISABLED

  AsopInternalCosts();                                           // zero-argument ctor
  AsopInternalCosts(const AsopInternalCosts& orig);              // copy constructor
  AsopInternalCosts& operator= (const AsopInternalCosts& orig);  // copy assignment opor

  // CREATORS

public:

  explicit
  AsopInternalCosts
  (const std::string entityId,
   Record&           record);

  virtual
  ~AsopInternalCosts();

  // CALLS

public:

  virtual
  const int                                  // number of technical assets processesed
  constrain
  (const xeona::DomainMode capacityMode);

  virtual
  void
  washup();

  // INSTANCE DATA

private:

  shared_ptr<CtlLeastCost>                  d_ctl;     // specialization required
  std::vector<shared_ptr<CtlLeastCost> >    d_ctls;

}; // class 'AsopInternalCosts'

//  ==== XEDOC =================================================
//
//  entity.asop-internal-costs-0
//
//      class                                    > AsopInternalCosts
//
//        basic asset operator with a null control objective and
//        with internal cost formation
//
//      builtin-remark s                         <
//
//      technical-assets L                       > "teas-1 teas-2"
//
//        the order of the technical-assets is not significant
//
//      standing-cost-financial [$/s] f          > 5.0e-03
//      standing-cost-greenhouse [kg/s] f        > 5.0e-03
//      standing-cost-nox [kg/s] f               > 0.0
//      standing-cost-depletion [J/s] f          > 0.0
//      standing-cost-landuse [m2/s] f           > 0.0
//
//      variable-costs-financial [$] F           < 0.0 ..
//      fixed-costs-financial [$] F              < 0.0 ..
//
//  ============================================================

#endif // _ASOP1_H_

//  end of file

