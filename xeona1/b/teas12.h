//  XEONA ENERGY SYSTEMS MODELING ENVIRONMENT
//
//  file-create-name : teas12.h
//  file-create-date : Fri 21-Oct-2011 10:31 UTC
//  file-initiator   : Robbie Morrison <robbie@actrix.co.nz>
//
//  file-role        : concrete technical assets 12 / header
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
//  $URL: file:///home/robbie/svn-root/xeona/futz/trunk/xeona1/b/teas12.h $
//
//  GENERAL NOTES FOR THIS FILE
//
//  This unit contains valve entities and related entities.
//  Valves can be used to enact rule-based control.

//  HEADER GUARD

#ifndef _TEAS12_H_
#define _TEAS12_H_

//  LOCAL AND SYSTEM INCLUDES

#include "../b/teas.h"        // technical asset entity

#include "../a/logger_fwd.h"  // some forward declarations from "logger.h"
#include "../c/smart_ptr.h"   // toggle between Boost and TR1 smart pointers

#include <string>             // C++ strings
#include <sstream>            // string-streams

//  FORWARD (PARTIAL) DECLARATIONS

class Record;                                // constructor argument
namespace svif { class SolverIf; }           // member function argument

template <typename C> class Cable;           // 'C' is of base class 'Commodity' etc
template <typename C> class Socket;

//  CODE

// ---------------------------------------------------------
//  CLASS           : TeasValveTs <>
// ---------------------------------------------------------
//  Description  : valve entity with prescribed settings
//  Role         : can be used to enact rule-based control
//  Techniques   : (nothing special)
//  Status       : complete
//
//  Design notes
//
//      The valve element is ideal.  It does not create costs,
//      nor does it generate losses, but simply capacitates the
//      flow.
//
//      This entity can be used to enact rule-based control, but
//      should be developed so that it takes its instructions
//      from a suitable operator.  In its current form, this
//      entity really only proves that such control is possible
//      within the 'xeona' framework.
//
// ---------------------------------------------------------

class OpsFixedEffy_A;                        // necessary for OSP typedef below

template <typename C>                        // 'C' for commodity
class TeasValveTs :
  public TechnicalAsset
{

  // USING DECLARATIONS

protected:

  using Entity::s_logger;                    // place in common scope for this class

  // TYPEDEFS

private:

  typedef OpsFixedEffy_A OpsFixedEffy;       // used for switching implementations

  // DISABLED

private:

  TeasValveTs();                                       // zero-argument constructor
  TeasValveTs(const TeasValveTs& orig);                // copy constructor
  TeasValveTs& operator= (const TeasValveTs& orig);    // copy assignment operator

  // CREATORS

public:

  explicit
  TeasValveTs
  (const std::string entityId,
   Record&           record);

  virtual
  ~TeasValveTs();

  // CALLS

public:

  virtual void establish() { }               // necessary but hollow redefinition

  virtual
  const int                                  // duty gol
  constrain
  (const xeona::DomainMode capacityMode);

  virtual
  void
  washup();

  virtual void conclude()  { }               // necessary but hollow redefinition

  // INSTANCE DATA

private:

  // tied quantities

  const double&                       d_capacity;
  double&                             d_maximumDuty;

  shared_ptr<std::vector<double> >    d_valveSettings;
  shared_ptr<std::vector<double> >    d_valveCapacitys;
  shared_ptr<std::vector<bool> >      d_valveBinds;

  const std::string&                  d_commodity;     // for the 'create' interface calls

  shared_ptr<Cable<C> >               d_inCommodity;
  shared_ptr<Socket<C> >              d_outCommodity;

  // local quantities

  double                              d_hiBound;  // current choke

  shared_ptr<OpsFixedEffy>            d_ops;      // specialization required

}; // class 'TeasValveTs'

//  ==== XEDOC =================================================
//
//  entity.teas-valve-ts-*-0
//
//        base commodity in {Cert,Cseq,Elec,Fund,Heat,Oxid,Thrm,Work}
//
//      class                                    > TeasValveTs:Elec
//
//        autonomous valve taking settings from a proscribed
//        timeseries (can extend to take instructions from a
//        suitable operator)
//
//        my socket label is 'sock-1'
//
//      builtin-remark s                         <
//
//      socket-1 l                               > "teas-upstream-1.sock-1"
//
//        socket-1 is my upstream supplier
//
//      common-commodity l                       > "cm-electricity-0"
//
//        common-commodity defines the underlying commodity
//
//      capacity [*/s] f                         > 3.0e+06
//      valve-settings [-] F                     > 0.0 0.5 1.0 0.5 ..
//
//        capacity is the maximum capacity, valve-settings are
//        relative to the capacity
//
//      valve-capacitys [*/s] F                  < 0.0 ..
//      valve-binds [-] b                        < 0 ..
//      maximum-duty [*/s] f                     < 0.0
//
//  ============================================================

// ---------------------------------------------------------
//  CLASS           : TeasFlowInhibitor <>
// ---------------------------------------------------------
//  Description  : flow inhibitor entity taking its settings from a suitable operator
//  Role         : can be used to enact rule-based control
//  Techniques   : (nothing special)
//  Status       : complete
//
//  Design notes
//
//      The flow inhibitor entity is an ideal element.  It does
//      not create cost-register costs, nor does it generate
//      commodity losses and/or degrade intensive commodity
//      parameters.  But it can either capacitate the flow or add
//      a non-consolidating penalty (as opposed to a cost) or do
//      both.
//
//      Moreover, negative penalties will give preference to the
//      associated commodity stream.
//
//      This entity can be used to enact rule-based control, when
//      coupled with a suitable operator.
//
// ---------------------------------------------------------

class OpsFixedEffy_A;                        // necessary for OSP typedef below

template <typename C>                        // 'C' for commodity
class TeasFlowInhibitor :
  public TechnicalAsset
{

  // USING DECLARATIONS

protected:

  using Entity::s_logger;                    // place in common scope for this class

  // TYPEDEFS

private:

  typedef OpsFixedEffy_A OpsFixedEffy;       // used for switching implementations

  // DISABLED

private:

  TeasFlowInhibitor();                                           // zero-argument ctor
  TeasFlowInhibitor(const TeasFlowInhibitor& orig);              // copy constructor
  TeasFlowInhibitor& operator= (const TeasFlowInhibitor& orig);  // copy assignment oper

  // CREATORS

public:

  explicit
  TeasFlowInhibitor
  (const std::string entityId,
   Record&           record);

  virtual
  ~TeasFlowInhibitor();

  // SPECIAL CALLS

public:

  void
  passSettings                               // used by operator to pass settings down
  (const double relativeValveSetting,
   const double operationalPenalty);

  // STANDARD CALLS

public:

  virtual void establish() { }               // necessary but hollow redefinition

  virtual
  const int                                  // duty gol
  constrain
  (const xeona::DomainMode capacityMode);

  virtual
  void
  washup();

  virtual void conclude()  { }               // necessary but hollow redefinition

  // INSTANCE DATA

private:

  // tied quantities

  const double&                       d_valveMaxCapacity;
  const bool&                         d_invertValveSettings;
  double&                             d_maximumDuty;

  shared_ptr<std::vector<double> >    d_valveCapacitys;
  shared_ptr<std::vector<bool> >      d_valveBinds;
  shared_ptr<std::vector<double> >    d_operationalPenaltys;

  const std::string&                  d_commodity;     // for the 'create' interface calls

  shared_ptr<Cable<C> >               d_inCommodity;
  shared_ptr<Socket<C> >              d_outCommodity;

  // local quantities

  double                              d_relativeValveSetting; // on [0,1]
  double                              d_hiBound;              // capacity choke
  double                              d_operationalPenalty;   // cost minimization penalty
  CostSet                             d_dutySpecPenalties;

  shared_ptr<OpsFixedEffy>            d_ops;      // specialization required

  // STATIC DATA

  static const double                 s_init;     // for initialization purposes

}; // class 'TeasFlowInhibitor'

//  ==== XEDOC =================================================
//
//  entity.teas-flow-inhibitor-*-0
//
//        base commodity in {Cert,Cseq,Elec,Fund,Heat,Oxid,Thrm,Work}
//
//      class                                    > TeasFlowInhibitor:Elec
//
//        flow inhibitor taking its valve and penalty settings
//        from an administrative operator
//
//        my socket label is 'sock-1'
//
//      builtin-remark s                         <
//
//      socket-1 l                               > "teas-upstream-1.sock-1"
//
//        socket-1 is my upstream supplier
//
//      common-commodity l                       > "cm-electricity-0"
//
//        common-commodity defines the underlying commodity
//
//      valve-max-capacity [*/s] f               > 3.0e+06
//      invert-valve-settings [-] b              > 0
//
//        the valve-max-capacity and the relative valve setting
//        determine the valve capacity, invert-valve-settings,
//        when true, means subtract setting from unity
//
//      valve-capacitys [*/s] F                  < 0.0 ..
//      valve-binds [-] b                        < 0 ..
//      operational-penaltys [+/s] F             < 0.0 ..
//      maximum-duty [*/s] f                     < 0.0
//
//        the operational-penaltys unit is + in {$,kg,J,m2}
//
//  ============================================================

#endif // _TEAS12_H_

//  end of file

