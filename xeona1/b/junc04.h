//  XEONA ENERGY SYSTEMS MODELING ENVIRONMENT
//
//  file-create-name : junc04.h
//  file-create-date : Thu 14-Apr-2011 09:58 UTC
//  file-initiator   : Robbie Morrison <robbie@actrix.co.nz>
//
//  file-role        : concrete faked demand and supply junctions / header
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
//  $URL: file:///home/robbie/svn-root/xeona/futz/trunk/xeona1/b/junc04.h $
//
//  GENERAL NOTES FOR THIS FILE
//
//  These junctions are intended for use during model
//  development.  They do not represent "real" entities.

//  HEADER GUARD

#ifndef _JUNC04_H_
#define _JUNC04_H_

//  LOCAL AND SYSTEM INCLUDES

#include "../b/junc.h"        // demand split/join junction entity

#include "../a/logger_fwd.h"  // some forward declarations from "logger.h"
#include "../c/smart_ptr.h"   // toggle between Boost and TR1 smart pointers

#include <string>             // C++ strings
#include <sstream>            // string-streams

//  FORWARD (PARTIAL) DECLARATIONS

class Record;                                // constructor argument

template <typename C> class Cable;           // 'C' is of base class 'Commodity' etc
template <typename C> class Socket;

//  CODE

// ---------------------------------------------------------
//  CLASS           : JuncDemandInvent <>
// ---------------------------------------------------------
//  Description  : demand creation junction with one normal cable
//  Role         : concrete entity, intended for use during model development
//  Techniques   : (nothing special)
//  Status       : complete
//
//  Design notes
//
//      Intended only for use during model development, this
//      entity has no costs and no associated asset operator.  It
//      will, however, need to be listed as a junction under the
//      correct enclosing domain controller, namely:
//
//         demand-junctions L    > "junc-demand-invent-1"
//
//  CAUTION: jumpy
//
//      This entity issues a 'logga::rankJumpy' log message on
//      construction -- which will translate into a WARN if the
//      'xeona' option '--jumpy' is applied.  Likewise, non-zero
//      demand is also logged under 'logga::rankJumpy'.
//
// ---------------------------------------------------------

class JncSuck;                               // forward (partial) declaration

template <typename C>                        // 'C' for commodity
class JuncDemandInvent :
  public DemandJunction
{
  // DISABLED

private:

  JuncDemandInvent();                                         // zero-argument constructor
  JuncDemandInvent(const JuncDemandInvent& orig);             // copy constructor
  JuncDemandInvent& operator= (const JuncDemandInvent& orig); // copy assignment operator

  // CREATORS

public:

  explicit
  JuncDemandInvent
  (const std::string entityId,
   Record&           record);

  virtual
  ~JuncDemandInvent();

  // CALLS

public:

  virtual void establish() { }               // necessary redefinition

  virtual
  const int                                  // returns zero (no duty coupling)
  constrain                                  // load OSP data
  (const xeona::DomainMode capacityMode);

  virtual
  void
  washup();

  virtual void conclude()  { }               // necessary redefinition

  // INSTANCE DATA

private:

  // tied quantities

  const std::string&             d_junctionCommodity;  // for the 'create' interface calls
  const double&                  d_constantDemand;

  const shared_ptr<Cable<C> >    d_cable;

  // local quantities

  shared_ptr<JncSuck>            d_ops;                // specialization required

}; // class 'JuncDemandInvent<>'

//  ==== XEDOC =================================================
//
//  entity.junc-demand-invent-*-0
//
//        base commodity in {Cert,Cseq,Elec,Fund,Heat,Oxid,Thrm,Work}
//        or derived in {OxidBiocoal,OxidGas,OxidHydrogen,OxidNaturalGas}
//
//      class                                    > JuncDemandInvent:Elec
//
//        fake demand creation junction with one normal cable and no costs
//        intended for use during model development only
//
//      builtin-remark s                         <
//
//      socket-1 l                               > "teas-supplier-1.sock-1"
//
//        socket-1 is my supplier
//
//      junction-commodity l                     > "cm-electricity-0"
//
//        junction-commodity defines the underlying commodity
//
//      constant-demand [*/s] f                  > 0.0
//
//        constant-demand is often set to zero
//
//  ============================================================

// ---------------------------------------------------------
//  CLASS           : JuncDemandService <>
// ---------------------------------------------------------
//  Description  : unlimited demand satisfying junction with one normal socket
//  Role         : concrete entity, intended for use during model development
//  Techniques   : (nothing special)
//  Status       : complete
//
//  Design notes
//
//      Intended only for use during model development, this
//      entity has no costs and no associated asset operator.  It
//      will, however, need to be listed as a junction under the
//      correct enclosing domain controller, namely:
//
//         demand-junctions L    > "junc-demand-quelle-1"
//
//  CAUTION: jumpy
//
//      This entity issues a 'logga::rankJumpy' log message on
//      construction -- which will translate into a WARN if the
//      'xeona' option '--jumpy' is applied.  Likewise, non-zero
//      demand is also logged under 'logga::rankJumpy'.
//
// ---------------------------------------------------------

class JncBlow;                               // forward (partial) declaration

template <typename C>                        // 'C' for commodity
class JuncDemandService :
  public DemandJunction
{
  // DISABLED

private:

  JuncDemandService();                                           // zero-argument ctor
  JuncDemandService(const JuncDemandService& orig);              // copy constructor
  JuncDemandService& operator= (const JuncDemandService& orig);  // copy assignment opor

  // CREATORS

public:

  explicit
  JuncDemandService
  (const std::string entityId,
   Record&           record);

  virtual
  ~JuncDemandService();

  // CALLS

public:

  virtual void establish() { }               // necessary redefinition

  virtual
  const int                                  // returns zero (no duty coupling)
  constrain                                  // load OSP data
  (const xeona::DomainMode capacityMode);

  virtual
  void
  washup();

  virtual void conclude()  { }               // necessary redefinition

  // INSTANCE DATA

private:

  // tied quantities

  const std::string&                  d_junctionCommodity; // for 'create' interface calls
  const double&                       d_loBound;
  const double&                       d_hiBound;

  shared_ptr<std::vector<double> >    d_supplys;

  const shared_ptr<Socket<C> >        d_socket;

  // local quantities

  double                              d_hiBoundReworked;

  shared_ptr<JncBlow>                 d_ops;      // specialization required

}; // class 'JuncDemandService<>'

//  ==== XEDOC =================================================
//
//  entity.junc-demand-service-*-0
//
//        base commodity in {Cert,Cseq,Elec,Fund,Heat,Oxid,Thrm,Work}
//        or derived in {OxidBiocoal,OxidGas,OxidHydrogen,OxidNaturalGas}
//
//      class                                    > JuncDemandService:Elec
//
//        fake commodity creation junction with one normal socket and no costs
//        intended for use during model development only
//
//        my socket label is 'sock-1'
//
//      builtin-remark s                         <
//
//      junction-commodity l                     > "cm-electricity-0"
//
//        junction-commodity defines the underlying commodity
//
//      lo-bound [*/s] f                         >  0.0
//      hi-bound [*/s] f                         > -1.0
//
//        lo-bound is the lower bound (normally zero) and
//        hi-bound is the upper bound (minus one to disable)
//
//      supplys [*/s] f                          < 0.0 ..
//
//        supplys contains the actual commodity flows
//
//  ============================================================

#endif // _JUNC04_H_

//  end of file

