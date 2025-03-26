//  XEONA ENERGY SYSTEMS MODELING ENVIRONMENT
//
//  file-create-name : node05.h
//  file-create-date : Fri 03-Feb-2012 16:35 UTC
//  file-initiator   : Robbie Morrison <robbie@actrix.co.nz>
//
//  file-role        : concrete LMP AC nodes 2 / header
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
//  $URL: file:///home/robbie/svn-root/xeona/futz/trunk/xeona1/b/node05.h $
//
//  GENERAL NOTES FOR THIS FILE
//
//  For use with AC transmission lines.  Contains 'theta', the
//  voltage angle -- stored and processed in radians but reported
//  in degrees.
//
//  This unit is based on unit 'b/node02'.

//  HEADER GUARD

#ifndef _NODE05_H_
#define _NODE05_H_

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
//  CLASS           : NodeAc2Nul <>
// ---------------------------------------------------------
//  Description  : bidirectional cable, bidirectional socket LMP node
//  Role         : connection between two grid entities
//  Techniques   : class template, 'Lmp2Nul2'
//  Status       : complete
// ---------------------------------------------------------

class Lmp2Nul2;                              // forward (partial) declaration

template <typename C>                        // 'C' for commodity
class NodeAc2Nul :
  public LmpNodeAc
{
  // DISABLED

private:

  NodeAc2Nul();                                        // zero-argument ctor
  NodeAc2Nul(const NodeAc2Nul& orig);                  // copy constructor
  NodeAc2Nul& operator= (const NodeAc2Nul& orig);      // copy assignment operator

  // CREATORS

public:

  explicit
  NodeAc2Nul
  (const std::string entityId,
   Record&           record);

  virtual
  ~NodeAc2Nul();

  virtual
  void
  fixTheta
  (const double thetaDegrees);               // reference voltage angle

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

  shared_ptr<Lmp2Nul2>      d_ops;           // specialization required

}; // class 'NodeAc2Nul'

//  ==== XEDOC =================================================
//
//  entity.node-ac-2-nul-*-0
//
//        base commodity in {Cert,Cseq,Elec,Fund,Heat,Oxid,Thrm,Work}
//
//      class                                    > NodeAc2Nul:Elec
//
//        AC grid node to connect two grid assets
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
//      voltage-angles [degrees] F               < 0.0 ..
//
//        to convert to the more usual [$/MWh], multiply the
//        given nodal-prices by 278
//
//  ============================================================

// ---------------------------------------------------------
//  CLASS           : NodeAc2Inj <>
// ---------------------------------------------------------
//  Description  : bidirectional cable, bidirectional socket LMP node
//  Role         : connection between two grid entities plus injection
//  Techniques   : class template, 'Lmp2Cab2'
//  Status       : complete
// ---------------------------------------------------------

class Lmp2Cab2;                              // forward (partial) declaration

template <typename C>                        // 'C' for commodity
class NodeAc2Inj :
  public LmpNodeAc
{
  // DISABLED

private:

  NodeAc2Inj();                                        // zero-argument ctor
  NodeAc2Inj(const NodeAc2Inj& orig);                  // copy constructor
  NodeAc2Inj& operator= (const NodeAc2Inj& orig);      // copy assignment operator

  // CREATORS

public:

  explicit
  NodeAc2Inj
  (const std::string entityId,
   Record&           record);

  virtual
  ~NodeAc2Inj();

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
  shared_ptr<Socket<C> >    d_socket;

  // local quantities

  shared_ptr<Lmp2Cab2>      d_ops;           // specialization required

}; // class 'NodeAc2Inj'

//  ==== XEDOC =================================================
//
//  entity.node-ac-2-inj-*-0
//
//        base commodity in {Cert,Cseq,Elec,Fund,Heat,Oxid,Thrm,Work}
//
//      class                                    > NodeAc2Inj:Elec
//
//        AC grid node with injection and support for two grid
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
//      voltage-angles [degrees] F               < 0.0 ..
//
//        to convert to the more usual [$/MWh], multiply the
//        given nodal-prices by 278
//
//  ============================================================

// ---------------------------------------------------------
//  CLASS           : NodeAc2Xit <>
// ---------------------------------------------------------
//  Description  : bidirectional cable, bidirectional socket LMP node
//  Role         : connection between two grid entities plus exit
//  Techniques   : class template, 'Lmp2Soc2'
//  Status       : complete
// ---------------------------------------------------------

class Lmp2Soc2;                              // forward (partial) declaration

template <typename C>                        // 'C' for commodity
class NodeAc2Xit :
  public LmpNodeAc
{
  // DISABLED

private:

  NodeAc2Xit();                                        // zero-argument ctor
  NodeAc2Xit(const NodeAc2Xit& orig);                  // copy constructor
  NodeAc2Xit& operator= (const NodeAc2Xit& orig);      // copy assignment operator

  // CREATORS

public:

  explicit
  NodeAc2Xit
  (const std::string entityId,
   Record&           record);

  virtual
  ~NodeAc2Xit();

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
  shared_ptr<Socket<C> >    d_socket1;
  shared_ptr<Socket<C> >    d_socket2;

  // local quantities

  shared_ptr<Lmp2Soc2>      d_ops;           // specialization required

}; // class 'NodeAc2Xit'

//  ==== XEDOC =================================================
//
//  entity.node-ac-2-xit-*-0
//
//        base commodity in {Cert,Cseq,Elec,Fund,Heat,Oxid,Thrm,Work}
//
//      class                                    > NodeAc2Xit:Elec
//
//        AC grid node with exit and support for two grid assets
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
//      voltage-angles [degrees] F               < 0.0 ..
//
//        to convert to the more usual [$/MWh], multiply the
//        given nodal-prices by 278
//
//  ============================================================

#endif // _NODE05_H_

//  end of file

