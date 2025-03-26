//  XEONA ENERGY SYSTEMS MODELING ENVIRONMENT
//
//  file-create-name : node03.h
//  file-create-date : Tue 01-Mar-2011 18:31 UTC
//  file-initiator   : Robbie Morrison <robbie@actrix.co.nz>
//
//  file-role        : concrete LMP DC nodes 3 / header
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
//  $URL: file:///home/robbie/svn-root/xeona/futz/trunk/xeona1/b/node03.h $
//
//  GENERAL NOTES FOR THIS FILE

//  HEADER GUARD

#ifndef _NODE03_H_
#define _NODE03_H_

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
//  CLASS           : Node3NulA <>
// ---------------------------------------------------------
//  Description  : two bidirectional cables, bidirectional socket LMP node (type A)
//  Role         : connection between three grid entities
//  Techniques   : class template, 'LmpNul3A'
//  Status       : complete
// ---------------------------------------------------------

class LmpNul3A;                              // forward (partial) declaration

template <typename C>                        // 'C' for commodity
class Node3NulA :
  public LmpNode
{

  // DISABLED

private:

  Node3NulA();                                    // zero-argument constructor
  Node3NulA(const Node3NulA& orig);               // copy constructor
  Node3NulA& operator= (const Node3NulA& orig);   // copy assignment operator

  // CREATORS

public:

  explicit
  Node3NulA
  (const std::string entityId,
   Record&           record);

  virtual
  ~Node3NulA();

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

  shared_ptr<LmpNul3A>      d_ops;           // specialization required

}; // class 'Node3NulA'

//  ==== XEDOC =================================================
//
//  entity.node-3-nul-a-*-0
//
//        base commodity in {Cert,Cseq,Elec,Fund,Heat,Oxid,Thrm,Work}
//
//      class                                    > Node3NulA:Elec
//
//        type A DC grid node to connect three grid assets, two
//        of which are cables (not sockets)
//
//        my sole bidirectional socket label is 'grid-1'
//
//      builtin-remark s                         <
//
//      socket-1 l                               > "teas-transmission-1.elec-1"
//      socket-2 l                               > "teas-transmission-2.elec-1"
//
//        socket-1 and socket-2 are my associated bidirectional
//        grid assets
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
//  CLASS           : Node3NulB <>
// ---------------------------------------------------------
//  Description  : two bidirectional cables, bidirectional socket LMP node (type A)
//  Role         : connection between three grid entities
//  Techniques   : class template, 'LmpNul3B'
//  Status       : complete
// ---------------------------------------------------------

class LmpNul3B;                              // forward (partial) declaration

template <typename C>                        // 'C' for commodity
class Node3NulB :
  public LmpNode
{

  // DISABLED

private:

  Node3NulB();                                    // zero-argument constructor
  Node3NulB(const Node3NulB& orig);               // copy constructor
  Node3NulB& operator= (const Node3NulB& orig);   // copy assignment operator

  // CREATORS

public:

  explicit
  Node3NulB
  (const std::string entityId,
   Record&           record);

  virtual
  ~Node3NulB();

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

  shared_ptr<LmpNul3B>      d_ops;           // specialization required

}; // class 'Node3NulB'

//  ==== XEDOC =================================================
//
//  entity.node-3-nul-b-*-0
//
//        base commodity in {Cert,Cseq,Elec,Fund,Heat,Oxid,Thrm,Work}
//
//      class                                    > Node3NulB:Elec
//
//        type B DC grid node to connect three grid assets, only
//        one of which is a cable (not a socket)
//
//        my bidirectional socket labels are 'grid-1' 'grid-2'
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

#endif // _NODE03_H_

//  end of file

