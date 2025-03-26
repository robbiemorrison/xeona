//  XEONA ENERGY SYSTEMS MODELING ENVIRONMENT
//
//  file-create-name : teas11.h
//  file-create-date : Wed 19-Oct-2011 11:05 UTC
//  file-initiator   : Robbie Morrison <robbie@actrix.co.nz>
//
//  file-role        : concrete technical assets 11 / header
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
//  $URL: file:///home/robbie/svn-root/xeona/futz/trunk/xeona1/b/teas11.h $
//
//  GENERAL NOTES FOR THIS FILE
//
//  This unit contains node cap entities.

//  HEADER GUARD

#ifndef _TEAS11_H_
#define _TEAS11_H_

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
//  CLASS           : TeasCapA <>
// ---------------------------------------------------------
//  Description  : no-flow node cap with one bidirectional cable
//  Role         : used to cap node ports (typically when building up a model)
//  Techniques   : (nothing special)
//  Status       : complete
//
//  Design notes
//
//      The type A and B designations are derived from the node
//      entities: A indicates a surplus of cables over sockets
//      and B indicates the opposite.
//
//      The function 'TeasCapA::constrain' returns zero to
//      prevent duty gol coupling with the asset operator.
//
// ---------------------------------------------------------

class OpsDummy_A;                            // necessary for OSP typedef below

template <typename C>                        // 'C' for commodity
class TeasCapA :
  public TechnicalAsset
{
  // USING DECLARATIONS

protected:

  using Entity::s_logger;                    // place in common scope for this class

  // TYPEDEFS

private:

  typedef OpsDummy_A OpsDummy;               // used for switching implementations

  // DISABLED

private:

  TeasCapA();                                     // zero-argument constructor
  TeasCapA(const TeasCapA& orig);                 // copy constructor
  TeasCapA& operator= (const TeasCapA& orig);     // copy assignment operator

  // CREATORS

public:

  explicit
  TeasCapA
  (const std::string entityId,
   Record&           record);

  virtual
  ~TeasCapA();

  // CALLS

public:

  virtual void establish() { }                    // necessary redefinition

  virtual
  const int                                       // returns zero
  constrain
  (const xeona::DomainMode capacityMode);

  virtual
  void
  washup();

  virtual void conclude()  { }                    // necessary redefinition

  // INSTANCE DATA

private:

  // tied quantities

  const std::string&             d_commodity;     // for the 'create' interface calls

  const shared_ptr<Cable<C> >    d_inCommodity;

  // local quantities

  shared_ptr<OpsDummy>           d_ops;           // specialization required

};

//  ==== XEDOC =================================================
//
//  entity.teas-cap-a-*-0
//
//        base commodity in {Cert,Cseq,Elec,Fund,Heat,Oxid,Thrm,Work}
//
//      class                                    > TeasCapA:Elec
//
//        type A node cap entity with one bidirectional cable
//
//      builtin-remark s                         <
//
//      socket-1 l                               > "node-1.grid-1"
//
//        socket-1 is my associated node
//
//      cable-commodity l                        > "cm-electricity-0"
//
//        cable-commodity defines the underlying commodity
//
//  ============================================================

// ---------------------------------------------------------
//  CLASS           : TeasCapB <>
// ---------------------------------------------------------
//  Description  : no-flow node cap with one bidirectional socket
//  Role         : used to cap node ports (typically when building up a model)
//  Techniques   : (nothing special)
//  Status       : complete
//
//  Design notes
//
//      The type A and B designations are derived from the node
//      entities: A indicates a surplus of cables over sockets
//      and B indicates the opposite.
//
//      The function 'TeasCapB::constrain' returns zero to
//      prevent duty gol coupling with the asset operator.
//
// ---------------------------------------------------------

class OpsDummy_A;                            // necessary for OSP typedef below

template <typename C>                        // 'C' for commodity
class TeasCapB :
  public TechnicalAsset
{
  // USING DECLARATIONS

protected:

  using Entity::s_logger;                    // place in common scope for this class

  // TYPEDEFS

private:

  typedef OpsDummy_A OpsDummy;               // used for switching implementations

  // DISABLED

private:

  TeasCapB();                                     // zero-argument constructor
  TeasCapB(const TeasCapB& orig);                 // copy constructor
  TeasCapB& operator= (const TeasCapB& orig);     // copy assignment operator

  // CREATORS

public:

  explicit
  TeasCapB
  (const std::string entityId,
   Record&           record);

  virtual
  ~TeasCapB();

  // CALLS

public:

  virtual void establish() { }                    // necessary redefinition

  virtual
  const int                                       // returns zero
  constrain
  (const xeona::DomainMode capacityMode);

  virtual
  void
  washup();

  virtual void conclude()  { }                    // necessary redefinition

  // INSTANCE DATA

private:

  // tied quantities

  const std::string&              d_commodity;    // for the 'create' interface calls

  const shared_ptr<Socket<C> >    d_outCommodity;

  // local quantities

  shared_ptr<OpsDummy>            d_ops;          // specialization required

};

//  ==== XEDOC =================================================
//
//  entity.teas-cap-b-*-0
//
//        base commodity in {Cert,Cseq,Elec,Fund,Heat,Oxid,Thrm,Work}
//
//      class                                    > TeasCapB:Elec
//
//        type B node cap entity with one bidirectional socket
//
//        my bidirectional socket label is 'grid-l'
//
//      builtin-remark s                         <
//
//      socket-commodity l                       > "cm-electricity-0"
//
//        socket-commodity defines the underlying commodity
//
//  ============================================================

#endif // _TEAS11_H_

//  end of file

