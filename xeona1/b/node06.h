//  XEONA ENERGY SYSTEMS MODELING ENVIRONMENT
//
//  file-create-name : node06.h
//  file-create-date : Fri 03-Feb-2012 16:36 UTC
//  file-initiator   : Robbie Morrison <robbie@actrix.co.nz>
//
//  file-role        : concrete LMP AC nodes 3 / header
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
//  $URL: file:///home/robbie/svn-root/xeona/futz/trunk/xeona1/b/node06.h $
//
//  GENERAL NOTES FOR THIS FILE
//  For use with AC transmission lines.  Contains 'theta', the
//  voltage angle -- stored and processed in radians but reported
//  in degrees.
//
//  This unit is based on unit 'b/node03'.

//  HEADER GUARD

#ifndef _NODE06_H_
#define _NODE06_H_

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
//  CLASS           : NodeAc3NulA <>
// ---------------------------------------------------------
//  Description  : two bidirectional cables, bidirectional socket LMP node (type A)
//  Role         : connection between three grid entities
//  Techniques   : class template, 'Lmp2Nul3A'
//  Status       : complete
// ---------------------------------------------------------

class Lmp2Nul3A;                             // forward (partial) declaration

template <typename C>                        // 'C' for commodity
class NodeAc3NulA :
  public LmpNodeAc
{

  // DISABLED

private:

  NodeAc3NulA();                                       // zero-argument constructor
  NodeAc3NulA(const NodeAc3NulA& orig);                // copy constructor
  NodeAc3NulA& operator= (const NodeAc3NulA& orig);    // copy assignment operator

  // CREATORS

public:

  explicit
  NodeAc3NulA
  (const std::string entityId,
   Record&           record);

  virtual
  ~NodeAc3NulA();

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

  shared_ptr<Lmp2Nul3A>     d_ops;           // specialization required

}; // class 'NodeAc3NulA'

//  ==== XEDOC =================================================
//
//  entity.node-ac-3-nul-a-*-0
//
//        base commodity in {Cert,Cseq,Elec,Fund,Heat,Oxid,Thrm,Work}
//
//      class                                    > NodeAc3NulA:Elec
//
//        type A AC grid node to connect three grid assets, two
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
//      voltage-angles [degrees] F               < 0.0 ..
//
//        to convert to the more usual [$/MWh], multiply the
//        given nodal-prices by 278
//
//  ============================================================

// ---------------------------------------------------------
//  CLASS           : NodeAc3NulB <>
// ---------------------------------------------------------
//  Description  : two bidirectional cables, bidirectional socket LMP node (type A)
//  Role         : connection between three grid entities
//  Techniques   : class template, 'Lmp2Nul3B'
//  Status       : complete
// ---------------------------------------------------------

class Lmp2Nul3B;                             // forward (partial) declaration

template <typename C>                        // 'C' for commodity
class NodeAc3NulB :
  public LmpNodeAc
{

  // DISABLED

private:

  NodeAc3NulB();                                       // zero-argument constructor
  NodeAc3NulB(const NodeAc3NulB& orig);                // copy constructor
  NodeAc3NulB& operator= (const NodeAc3NulB& orig);    // copy assignment operator

  // CREATORS

public:

  explicit
  NodeAc3NulB
  (const std::string entityId,
   Record&           record);

  virtual
  ~NodeAc3NulB();

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

  shared_ptr<Lmp2Nul3B>     d_ops;           // specialization required

}; // class 'NodeAc3NulB'

//  ==== XEDOC =================================================
//
//  entity.node-ac-3-nul-b-*-0
//
//        base commodity in {Cert,Cseq,Elec,Fund,Heat,Oxid,Thrm,Work}
//
//      class                                    > NodeAc3NulB:Elec
//
//        type B AC grid node to connect three grid assets, only
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
//      voltage-angles [degrees] F               < 0.0 ..
//
//        to convert to the more usual [$/MWh], multiply the
//        given nodal-prices by 278
//
//  ============================================================

#endif // _NODE06_H_

//  end of file

