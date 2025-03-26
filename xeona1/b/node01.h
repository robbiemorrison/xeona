//  XEONA ENERGY SYSTEMS MODELING ENVIRONMENT
//
//  file-create-name : node01.h
//  file-create-date : Tue 03-Nov-2009 12:19 UTC
//  file-initiator   : Robbie Morrison <robbie@actrix.co.nz>
//
//  file-role        : concrete LMP DC nodes 1 / header
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
//  $URL: file:///home/robbie/svn-root/xeona/futz/trunk/xeona1/b/node01.h $
//
//  GENERAL NOTES FOR THIS FILE

//  HEADER GUARD

#ifndef _NODE01_H_
#define _NODE01_H_

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
//  CLASS           : Node0InjXit <>
// ---------------------------------------------------------
//  Description  : normal cable, normal socket LMP node
//  Role         : no network node with injection and exit
//  Techniques   : class template, 'LmpCabSoc0'
//  Status       : complete
//
//  Design notes
//
//      With regard to the OSP class naming convention, a
//      trailing '_X' is not required or used.  That is because
//      there should be no need to develop different versions.
//
// ---------------------------------------------------------

class LmpCabSoc0;                            // forward (partial) declaration

template <typename C>                        // 'C' for commodity
class Node0InjXit :
  public LmpNode
{
  // DISABLED

private:

  Node0InjXit();                                       // zero-argument constructor
  Node0InjXit(const Node0InjXit& orig);                // copy constructor
  Node0InjXit& operator= (const Node0InjXit& orig);    // copy assignment operator

  // CREATORS

public:

  explicit
  Node0InjXit
  (const std::string entityId,
   Record&           record);

  virtual
  ~Node0InjXit();

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

  shared_ptr<LmpCabSoc0>    d_ops;           // specialization required

}; // class 'Node0InjXit'

//  ==== XEDOC =================================================
//
//  entity.node-0-injxit-*-0
//
//        base commodity in {Cert,Cseq,Elec,Fund,Heat,Oxid,Thrm,Work}
//
//      class                                    > Node0InjXit:Elec
//
//        non-network DC node with injection and exit -- which
//        lacks any transmission connection but offers a normal
//        socket and cable
//
//        my sole normal socket label is 'sock-1'
//
//      builtin-remark s                         <
//
//      socket-1 l                               > "teas-supplier-1.elec-1"
//
//        socket-1 is my normal supplier (injector)
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
//  CLASS           : Node1InjA <>
// ---------------------------------------------------------
//  Description  : normal cable, bidirectional cable LMP node
//  Role         : spur line node with injection
//  Techniques   : class template, 'LmpCab1A'
//  Status       : complete
//
//  Design notes
//
//      With regard to the OSP class naming convention, a
//      trailing '_X' is not required or used.  That is because
//      there should be no need to develop different versions.
//
// ---------------------------------------------------------

class LmpCab1A;                              // forward (partial) declaration

template <typename C>                        // 'C' for commodity
class Node1InjA :
  public LmpNode
{
  // DISABLED

private:

  Node1InjA();                                    // zero-argument ctor
  Node1InjA(const Node1InjA& orig);               // copy constructor
  Node1InjA& operator= (const Node1InjA& orig);   // copy assignment operator

  // CREATORS

public:

  explicit
  Node1InjA
  (const std::string entityId,
   Record&           record);

  virtual
  ~Node1InjA();

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

  // local quantities

  shared_ptr<LmpCab1A>      d_ops;           // specialization required

}; // class 'Node1InjA'

//  ==== XEDOC =================================================
//
//  entity.node-1-inj-a-*-0
//
//        base commodity in {Cert,Cseq,Elec,Fund,Heat,Oxid,Thrm,Work}
//
//      class                                    > Node1InjA:Elec
//
//        type A DC grid spur line node with injection which also
//        takes an incoming grid asset
//
//        there are no sockets, just cables
//
//      builtin-remark s                         <
//
//      socket-1 l                               > "teas-supplier-1.elec-1"
//      socket-2 l                               > "teas-transmission-1.elec-1"
//
//        socket-1 is my normal supplier (injector) and socket-2
//        is my associated bidirectional transmission asset
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
//  CLASS           : Node1InjB <>
// ---------------------------------------------------------
//  Description  : normal cable, bidirectional socket LMP node
//  Role         : spur line node with injection
//  Techniques   : class template, 'LmpCab1B'
//  Status       : complete
//
//  Design notes
//
//      With regard to the OSP class naming convention, a
//      trailing '_X' is not required or used.  That is because
//      there should be no need to develop different versions.
//
// ---------------------------------------------------------

class LmpCab1B;                              // forward (partial) declaration

template <typename C>                        // 'C' for commodity
class Node1InjB :
  public LmpNode
{
  // DISABLED

private:

  Node1InjB();                                    // zero-argument ctor
  Node1InjB(const Node1InjB& orig);               // copy constructor
  Node1InjB& operator= (const Node1InjB& orig);   // copy assignment operator

  // CREATORS

public:

  explicit
  Node1InjB
  (const std::string entityId,
   Record&           record);

  virtual
  ~Node1InjB();

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

  shared_ptr<LmpCab1B>      d_ops;           // specialization required

}; // class 'Node1InjB'

//  ==== XEDOC =================================================
//
//  entity.node-1-inj-b-*-0
//
//        base commodity in {Cert,Cseq,Elec,Fund,Heat,Oxid,Thrm,Work}
//
//      class                                    > Node1InjB:Elec
//
//        type B DC grid spur line node with injection
//
//        my sole bidirectional socket label is 'grid-1'
//
//      builtin-remark s                         <
//
//      socket-1 l                               > "teas-supplier-1.elec-1"
//
//        socket-1 is my normal supplier (injector)
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
//  CLASS           : Node1XitA <>
// ---------------------------------------------------------
//  Description  : bidirectional cable, normal socket LMP node
//  Role         : spur line node with exit
//  Techniques   : class template, 'LmpSoc1A'
//  Status       : complete
//
//  Design notes
//
// ---------------------------------------------------------

class LmpSoc1A;                              // forward (partial) declaration

template <typename C>                        // 'C' for commodity
class Node1XitA :
  public LmpNode
{
  // DISABLED

private:

  Node1XitA();                                    // zero-argument ctor
  Node1XitA(const Node1XitA& orig);               // copy constructor
  Node1XitA& operator= (const Node1XitA& orig);   // copy assignment operator

  // CREATORS

public:

  explicit
  Node1XitA
  (const std::string entityId,
   Record&           record);

  virtual
  ~Node1XitA();

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

  shared_ptr<LmpSoc1A>      d_ops;           // specialization required

}; // class 'Node1XitA'

//  ==== XEDOC =================================================
//
//  entity.node-1-xit-a-*-0
//
//        base commodity in {Cert,Cseq,Elec,Fund,Heat,Oxid,Thrm,Work}
//
//      class                                    > Node1XitA:Elec
//
//        type A DC grid spur line node with exit
//
//        my sole normal socket label is 'sock-1'
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
//  CLASS           : Node1XitB <>
// ---------------------------------------------------------
//  Description  : bidirectional socket, normal socket LMP node
//  Role         : spur line node with exit
//  Techniques   : class template, 'LmpSoc1B'
//  Status       : complete
//
//  Design notes
//
// ---------------------------------------------------------

class LmpSoc1B;                              // forward (partial) declaration

template <typename C>                        // 'C' for commodity
class Node1XitB :
  public LmpNode
{
  // DISABLED

private:

  Node1XitB();                                    // zero-argument ctor
  Node1XitB(const Node1XitB& orig);               // copy constructor
  Node1XitB& operator= (const Node1XitB& orig);   // copy assignment operator

  // CREATORS

public:

  explicit
  Node1XitB
  (const std::string entityId,
   Record&           record);

  virtual
  ~Node1XitB();

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
  shared_ptr<Socket<C> >    d_socket1;
  shared_ptr<Socket<C> >    d_socket2;

  // local quantities

  shared_ptr<LmpSoc1B>      d_ops;           // specialization required

}; // class 'Node1XitB'

//  ==== XEDOC =================================================
//
//  entity.node-1-xit-b-*-0
//
//        base commodity in {Cert,Cseq,Elec,Fund,Heat,Oxid,Thrm,Work}
//
//      class                                    > Node1XitB:Elec
//
//        type B DC grid spur line node with exit which also
//        takes an incoming grid asset
//
//        my sole bidirectional socket label is 'grid-1'
//        my sole normal socket label is 'sock-1'
//
//      builtin-remark s                         <
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

#endif // _NODE01_H_

//  end of file

