//  XEONA ENERGY SYSTEMS MODELING ENVIRONMENT
//
//  file-create-name : node04.h
//  file-create-date : Wed 11-Jan-2012 14:15 UTC
//  file-initiator   : Robbie Morrison <robbie@actrix.co.nz>
//
//  file-role        : concrete LMP AC nodes 1 / header
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
//  $URL: file:///home/robbie/svn-root/xeona/futz/trunk/xeona1/b/node04.h $
//
//  GENERAL NOTES FOR THIS FILE
//
//  For use with AC transmission lines.  Contains 'theta', the
//  voltage angle -- stored and processed in radians but reported
//  in degrees.
//
//  This unit is based on unit 'b/node01'.

//  HEADER GUARD

#ifndef _NODE04_H_
#define _NODE04_H_

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
//  CLASS           : NodeAc0InjXit <>
// ---------------------------------------------------------
//  Description  : normal cable, normal socket LMP node
//  Role         : no network node with injection and exit
//  Techniques   : class template, 'Lmp2CabSoc0'
//  Status       : complete
//
//  Design notes
//
//      With regard to the OSP class naming convention, a
//      trailing '_X' is not required or used.  That is because
//      there should be no need to develop different versions.
//
// ---------------------------------------------------------

class Lmp2CabSoc0;                           // forward (partial) declaration

template <typename C>                        // 'C' for commodity
class NodeAc0InjXit :
  public LmpNodeAc
{
  // DISABLED

private:

  NodeAc0InjXit();                                          // zero-argument constructor
  NodeAc0InjXit(const NodeAc0InjXit& orig);                 // copy constructor
  NodeAc0InjXit& operator= (const NodeAc0InjXit& orig);     // copy assignment operator

  // CREATORS

public:

  explicit
  NodeAc0InjXit
  (const std::string entityId,
   Record&           record);

  virtual
  ~NodeAc0InjXit();

  // CALLS

public:

  virtual
  const int                                  // returns zero (no duty coupling)
  constrain                                  // load OSP data
  (const xeona::DomainMode capacityMode);

  virtual
  void
  washup();                                  // recover OSP results

  virtual
  void
  fixTheta
  (const double thetaDegrees);               // reference voltage angle

  // INSTANCE DATA

private:

  // tied quantities

  std::string&              d_nodeCommodity; // 'create' interface calls
  shared_ptr<Cable<C> >     d_cable;
  shared_ptr<Socket<C> >    d_socket;

  // local quantities

  shared_ptr<Lmp2CabSoc0>   d_ops;           // specialization required

}; // class 'NodeAc0InjXit'

//  ==== XEDOC =================================================
//
//  entity.node-ac-0-injxit-*-0
//
//        base commodity in {Cert,Cseq,Elec,Fund,Heat,Oxid,Thrm,Work}
//
//      class                                    > NodeAc0InjXit:Elec
//
//        non-network AC node with injection and exit -- which
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
//      voltage-angles [degrees] F               < 0.0 ..
//
//        to convert to the more usual [$/MWh], multiply the
//        given nodal-prices by 278
//
//  ============================================================

// ---------------------------------------------------------
//  CLASS           : NodeAc1InjA <>
// ---------------------------------------------------------
//  Description  : normal cable, bidirectional cable LMP node
//  Role         : spur line node with injection
//  Techniques   : class template, 'Lmp2Cab1A'
//  Status       : complete
//
//  Design notes
//
//      With regard to the OSP class naming convention, a
//      trailing '_X' is not required or used.  That is because
//      there should be no need to develop different versions.
//
// ---------------------------------------------------------

class Lmp2Cab1A;                             // forward (partial) declaration

template <typename C>                        // 'C' for commodity
class NodeAc1InjA :
  public LmpNodeAc
{
  // DISABLED

private:

  NodeAc1InjA();                                    // zero-argument ctor
  NodeAc1InjA(const NodeAc1InjA& orig);               // copy constructor
  NodeAc1InjA& operator= (const NodeAc1InjA& orig);   // copy assignment operator

  // CREATORS

public:

  explicit
  NodeAc1InjA
  (const std::string entityId,
   Record&           record);

  virtual
  ~NodeAc1InjA();

  // CALLS

public:

  virtual
  const int                                  // returns zero (no duty coupling)
  constrain                                  // load OSP data
  (const xeona::DomainMode capacityMode);

  virtual
  void
  washup();                                  // recover OSP results

  virtual
  void
  fixTheta
  (const double thetaDegrees);               // reference voltage angle

  // INSTANCE DATA

private:

  // tied quantities

  std::string&              d_nodeCommodity; // 'create' interface calls
  shared_ptr<Cable<C> >     d_cable1;
  shared_ptr<Cable<C> >     d_cable2;

  // local quantities

  shared_ptr<Lmp2Cab1A>     d_ops;           // specialization required

}; // class 'NodeAc1InjA'

//  ==== XEDOC =================================================
//
//  entity.node-ac-1-inj-a-*-0
//
//        base commodity in {Cert,Cseq,Elec,Fund,Heat,Oxid,Thrm,Work}
//
//      class                                    > NodeAc1InjA:Elec
//
//        type A AC grid spur line node with injection which also
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
//      voltage-angles [degrees] F               < 0.0 ..
//
//        to convert to the more usual [$/MWh], multiply the
//        given nodal-prices by 278
//
//  ============================================================

// ---------------------------------------------------------
//  CLASS           : NodeAc1InjB <>
// ---------------------------------------------------------
//  Description  : normal cable, bidirectional socket LMP node
//  Role         : spur line node with injection
//  Techniques   : class template, 'Lmp2Cab1B'
//  Status       : complete
//
//  Design notes
//
//      With regard to the OSP class naming convention, a
//      trailing '_X' is not required or used.  That is because
//      there should be no need to develop different versions.
//
// ---------------------------------------------------------

class Lmp2Cab1B;                             // forward (partial) declaration

template <typename C>                        // 'C' for commodity
class NodeAc1InjB :
  public LmpNodeAc
{
  // DISABLED

private:

  NodeAc1InjB();                                       // zero-argument ctor
  NodeAc1InjB(const NodeAc1InjB& orig);                // copy constructor
  NodeAc1InjB& operator= (const NodeAc1InjB& orig);    // copy assignment operator

  // CREATORS

public:

  explicit
  NodeAc1InjB
  (const std::string entityId,
   Record&           record);

  virtual
  ~NodeAc1InjB();

  // CALLS

public:

  virtual
  const int                                  // returns zero (no duty coupling)
  constrain                                  // load OSP data
  (const xeona::DomainMode capacityMode);

  virtual
  void
  washup();                                  // recover OSP results

  virtual
  void
  fixTheta
  (const double thetaDegrees);               // reference voltage angle

  // INSTANCE DATA

private:

  // tied quantities

  std::string&              d_nodeCommodity; // 'create' interface calls
  shared_ptr<Cable<C> >     d_cable;
  shared_ptr<Socket<C> >    d_socket;

  // local quantities

  shared_ptr<Lmp2Cab1B>     d_ops;           // specialization required

}; // class 'NodeAc1InjB'

//  ==== XEDOC =================================================
//
//  entity.node-ac-1-inj-b-*-0
//
//        base commodity in {Cert,Cseq,Elec,Fund,Heat,Oxid,Thrm,Work}
//
//      class                                    > NodeAc1InjB:Elec
//
//        type B AC grid spur line node with injection
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
//      voltage-angles [degrees] F               < 0.0 ..
//
//        to convert to the more usual [$/MWh], multiply the
//        given nodal-prices by 278
//
//  ============================================================

// ---------------------------------------------------------
//  CLASS           : NodeAc1XitA <>
// ---------------------------------------------------------
//  Description  : bidirectional cable, normal socket LMP node
//  Role         : spur line node with exit
//  Techniques   : class template, 'Lmp2Soc1A'
//  Status       : complete
//
//  Design notes
//
// ---------------------------------------------------------

class Lmp2Soc1A;                             // forward (partial) declaration

template <typename C>                        // 'C' for commodity
class NodeAc1XitA :
  public LmpNodeAc
{
  // DISABLED

private:

  NodeAc1XitA();                                       // zero-argument ctor
  NodeAc1XitA(const NodeAc1XitA& orig);                // copy constructor
  NodeAc1XitA& operator= (const NodeAc1XitA& orig);    // copy assignment operator

  // CREATORS

public:

  explicit
  NodeAc1XitA
  (const std::string entityId,
   Record&           record);

  virtual
  ~NodeAc1XitA();

  // CALLS

public:

  virtual
  const int                                  // returns zero (no duty coupling)
  constrain                                  // load OSP data
  (const xeona::DomainMode capacityMode);

  virtual
  void
  washup();                                  // recover OSP results

  virtual
  void
  fixTheta
  (const double thetaDegrees);               // reference voltage angle

  // INSTANCE DATA

private:

  // tied quantities

  std::string&              d_nodeCommodity; // 'create' interface calls
  shared_ptr<Cable<C> >     d_cable;
  shared_ptr<Socket<C> >    d_socket;

  // local quantities

  shared_ptr<Lmp2Soc1A>     d_ops;           // specialization required

}; // class 'NodeAc1XitA'

//  ==== XEDOC =================================================
//
//  entity.node-ac-1-xit-a-*-0
//
//        base commodity in {Cert,Cseq,Elec,Fund,Heat,Oxid,Thrm,Work}
//
//      class                                    > NodeAc1XitA:Elec
//
//        type A AC grid spur line node with exit
//
//        my sole normal socket label is 'grid-1'
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
//      voltage-angles [degrees] F               < 0.0 ..
//
//        to convert to the more usual [$/MWh], multiply the
//        given nodal-prices by 278
//
//  ============================================================

// ---------------------------------------------------------
//  CLASS           : NodeAc1XitB <>
// ---------------------------------------------------------
//  Description  : bidirectional socket, normal socket LMP node
//  Role         : spur line node with exit
//  Techniques   : class template, 'Lmp2Soc1B'
//  Status       : complete
//
//  Design notes
//
// ---------------------------------------------------------

class Lmp2Soc1B;                             // forward (partial) declaration

template <typename C>                        // 'C' for commodity
class NodeAc1XitB :
  public LmpNodeAc
{
  // DISABLED

private:

  NodeAc1XitB();                                       // zero-argument ctor
  NodeAc1XitB(const NodeAc1XitB& orig);                // copy constructor
  NodeAc1XitB& operator= (const NodeAc1XitB& orig);    // copy assignment operator

  // CREATORS

public:

  explicit
  NodeAc1XitB
  (const std::string entityId,
   Record&           record);

  virtual
  ~NodeAc1XitB();

  // CALLS

public:

  virtual
  const int                                  // returns zero (no duty coupling)
  constrain                                  // load OSP data
  (const xeona::DomainMode capacityMode);

  virtual
  void
  washup();                                  // recover OSP results

  virtual
  void
  fixTheta
  (const double thetaDegrees);               // reference voltage angle

  // INSTANCE DATA

private:

  // tied quantities

  std::string&              d_nodeCommodity; // 'create' interface calls
  shared_ptr<Socket<C> >    d_socket1;
  shared_ptr<Socket<C> >    d_socket2;

  // local quantities

  shared_ptr<Lmp2Soc1B>     d_ops;           // specialization required

}; // class 'NodeAc1XitB'

//  ==== XEDOC =================================================
//
//  entity.node-ac-1-xit-b-*-0
//
//        base commodity in {Cert,Cseq,Elec,Fund,Heat,Oxid,Thrm,Work}
//
//      class                                    > NodeAc1XitB:Elec
//
//        type B AC grid spur line node with exit which also
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
//      voltage-angles [degrees] F               < 0.0 ..
//
//        to convert to the more usual [$/MWh], multiply the
//        given nodal-prices by 278
//
//  ============================================================

#endif // _NODE04_H_

//  end of file

