//  XEONA ENERGY SYSTEMS MODELING ENVIRONMENT
//
//  file-create-name : node02.h
//  file-create-date : Wed 04-Nov-2009 11:11 UTC
//  file-initiator   : Robbie Morrison <robbie@actrix.co.nz>
//
//  file-role        :  concrete LMP DC nodes 2 / header
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
//  $URL: file:///home/robbie/svn-root/xeona/futz/trunk/xeona1/b/node02.h $
//
//  GENERAL NOTES FOR THIS FILE
//
//  Only some arrangements coded
//
//      This unit covers nodes that, in terms of grid
//      connectivity, support only one incoming and one outgoing
//      grid asset.  This is clearly a limitation -- although one
//      that would be straightforward to rectify.

//  HEADER GUARD

#ifndef _NODE02_H_
#define _NODE02_H_

//  LOCAL AND SYSTEM INCLUDES

#include "../b/node.h"        // LMP node entity

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
//  notes           : types A and B
// ---------------------------------------------------------
//
//  Type A nodes have more cables that sockets, conversely for
//  type B, when equal omit the type qualifier altogether.
//
//  The naming for the underlying OSPs uses the same convention.
//
// ---------------------------------------------------------

// ---------------------------------------------------------
//  CLASS           : Node2Nul <>
// ---------------------------------------------------------
//  Description  : bidirectional cable, bidirectional socket LMP node
//  Role         : connection between two grid entities
//  Techniques   : class template, 'LmpNul2'
//  Status       : complete
// ---------------------------------------------------------

class LmpNul2;                               // forward (partial) declaration

template <typename C>                        // 'C' for commodity
class Node2Nul :
  public LmpNode
{
  // DISABLED

private:

  Node2Nul();                                     // zero-argument ctor
  Node2Nul(const Node2Nul& orig);                 // copy constructor
  Node2Nul& operator= (const Node2Nul& orig);     // copy assignment operator

  // CREATORS

public:

  explicit
  Node2Nul
  (const std::string entityId,
   Record&           record);

  virtual
  ~Node2Nul();

  // CALLS

public:

  virtual
  const int                                  // returns zero (no duty coupling)
  constrain                                  // load OSP data
  (const xeona::DomainMode capacityMode);

  virtual
  void
  washup();                                  // recover OSP results

  // INSTANCE DATA

private:

  // tied quantities

  std::string&              d_nodeCommodity; // 'create' interface calls
  shared_ptr<Cable<C> >     d_cable;
  shared_ptr<Socket<C> >    d_socket;

  // local quantities

  shared_ptr<LmpNul2>       d_ops;           // specialization required

}; // class 'Node2Nul'

//  ==== XEDOC =================================================
//
//  entity.node-2-nul-*-0
//
//        base commodity in {Cert,Cseq,Elec,Fund,Heat,Oxid,Thrm,Work}
//
//      class                                    > Node2Nul:Elec
//
//        DC grid node to connect two grid assets
//
//        my sole bidirectional socket label is 'grid-1'
//
//      builtin-remark s                         <
//
//      socket-1 l                               > "teas-transmission-1.elec-1"
//
//        socket-1 is my associated bidirectional grid asset
//
//      node-commodity l                         > "cm-electricity-0"
//
//        node-commodity defines the underlying commodity
//
//      nodal-prices [$/J] F                     < 0.0 ..
//
//        to convert to the more usual [$/MWh], multiply the
//        given nodal-prices by 278
//
//  ============================================================

// ---------------------------------------------------------
//  CLASS           : Node2Inj <>
// ---------------------------------------------------------
//  Description  : bidirectional cable, bidirectional socket LMP node
//  Role         : connection between two grid entities plus injection
//  Techniques   : class template, 'LmpCab2'
//  Status       : complete
// ---------------------------------------------------------

class LmpCab2;                               // forward (partial) declaration

template <typename C>                        // 'C' for commodity
class Node2Inj :
  public LmpNode
{
  // DISABLED

private:

  Node2Inj();                                     // zero-argument ctor
  Node2Inj(const Node2Inj& orig);                 // copy constructor
  Node2Inj& operator= (const Node2Inj& orig);     // copy assignment operator

  // CREATORS

public:

  explicit
  Node2Inj
  (const std::string entityId,
   Record&           record);

  virtual
  ~Node2Inj();

  // CALLS

public:

  virtual
  const int                                  // returns zero (no duty coupling)
  constrain                                  // load OSP data
  (const xeona::DomainMode capacityMode);

  virtual
  void
  washup();                                  // recover OSP results

  // INSTANCE DATA

private:

  // tied quantities

  std::string&              d_nodeCommodity; // 'create' interface calls
  shared_ptr<Cable<C> >     d_cable1;
  shared_ptr<Cable<C> >     d_cable2;
  shared_ptr<Socket<C> >    d_socket;

  // local quantities

  shared_ptr<LmpCab2>       d_ops;           // specialization required

}; // class 'Node2Inj'

//  ==== XEDOC =================================================
//
//  entity.node-2-inj-*-0
//
//        base commodity in {Cert,Cseq,Elec,Fund,Heat,Oxid,Thrm,Work}
//
//      class                                    > Node2Inj:Elec
//
//        DC grid node with injection and support for two grid
//        assets
//
//        my sole bidirectional socket label is 'grid-1'
//
//      builtin-remark s                         <
//
//      socket-1 l                               > "teas-supplier-1.elec-1"
//      socket-2 l                               > "teas-transmission-1.elec-1"
//
//        socket-1 is my normal supplier (injector), socket-2 is
//        my associated bidirectional grid asset
//
//      node-commodity l                         > "cm-electricity-0"
//
//        node-commodity defines the underlying commodity
//
//      nodal-prices [$/J] F                     < 0.0 ..
//
//        to convert to the more usual [$/MWh], multiply the
//        given nodal-prices by 278
//
//  ============================================================

// ---------------------------------------------------------
//  CLASS           : Node2Xit <>
// ---------------------------------------------------------
//  Description  : bidirectional cable, bidirectional socket LMP node
//  Role         : connection between two grid entities plus exit
//  Techniques   : class template, 'LmpSoc2'
//  Status       : complete
// ---------------------------------------------------------

class LmpSoc2;                               // forward (partial) declaration

template <typename C>                        // 'C' for commodity
class Node2Xit :
  public LmpNode
{
  // DISABLED

private:

  Node2Xit();                                     // zero-argument ctor
  Node2Xit(const Node2Xit& orig);                 // copy constructor
  Node2Xit& operator= (const Node2Xit& orig);     // copy assignment operator

  // CREATORS

public:

  explicit
  Node2Xit
  (const std::string entityId,
   Record&           record);

  virtual
  ~Node2Xit();

  // CALLS

public:

  virtual
  const int                                  // returns zero (no duty coupling)
  constrain                                  // load OSP data
  (const xeona::DomainMode capacityMode);

  virtual
  void
  washup();                                  // recover OSP results

  // INSTANCE DATA

private:

  // tied quantities

  std::string&              d_nodeCommodity; // 'create' interface calls
  shared_ptr<Cable<C> >     d_cable;
  shared_ptr<Socket<C> >    d_socket1;
  shared_ptr<Socket<C> >    d_socket2;

  // local quantities

  shared_ptr<LmpSoc2>       d_ops;           // specialization required

}; // class 'Node2Xit'

//  ==== XEDOC =================================================
//
//  entity.node-2-xit-*-0
//
//        base commodity in {Cert,Cseq,Elec,Fund,Heat,Oxid,Thrm,Work}
//
//      class                                    > Node2Xit:Elec
//
//        DC grid node with exit and support for two grid assets
//
//        my normal socket label is 'sock-1'
//        my bidirectional socket label is 'grid-1'
//
//      builtin-remark s                         <
//
//      socket-1 l                               > "teas-transmission-1.elec-1"
//
//        socket-1 is my associated bidirectional grid asset
//
//      node-commodity l                         > "cm-electricity-0"
//
//        node-commodity defines the underlying commodity
//
//      nodal-prices [$/J] F                     < 0.0 ..
//
//        to convert to the more usual [$/MWh], multiply the
//        given nodal-prices by 278
//
//  ============================================================

#endif // _NODE02_H_

//  end of file

