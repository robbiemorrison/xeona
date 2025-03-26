//  XEONA ENERGY SYSTEMS MODELING ENVIRONMENT
//
//  file-create-name : conex.h
//  file-create-date : Wed 16-Jul-2008 15:22 UTC
//  file-initiator   : Robbie Morrison <robbie@actrix.co.nz>
//
//  file-role        : create and connect block interfaces / header
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
//  $URL: file:///home/robbie/svn-root/xeona/futz/trunk/xeona1/c/conex.h $
//
//  GENERAL NOTES FOR THIS FILE

//  HEADER GUARD

#ifndef _CONEX_H_
#define _CONEX_H_

//  AD-HOC NOTES
//
//  See the file 'DOCS/CODE.txt' and, in particular, the section
//  on "Connection terminology" for a review of terms relating to
//  various forms of connections.

//  LOCAL AND SYSTEM INCLUDES

#include "../c/xeona_ptr.h"   // remappable counted pointer which mimics shared_ptr
#include "../b/entity.h"      // entity base class plus lazy linking

#include "../a/logger_fwd.h"  // some forward declarations from "logger.h"
#include "../c/smart_ptr.h"   // toggle between Boost and TR1 smart pointers

#include <string>             // C++ strings
#include <sstream>            // string-streams
#include <vector>             // STL sequence container

//  CODE

// ---------------------------------------------------------
//  CLASS           : Interface (abstract base class)
// ---------------------------------------------------------
//  Description  : abstract base class for 'Cable<C>' and 'Socket<C>' class templates
//  Role         : provide common static support for interface instantiations
//  Techniques   : 'weak_ptr', 'shared_ptr' casting, inheritance
//  See also     : 'Cable<C>' and 'Socket<C>'
//  Status       : complete
// ---------------------------------------------------------

// FORWARD (PARTIAL) DECLARATIONS

class Commodity;
class ConnectionOsp;
namespace svif { class SolverIf; }

template <typename C> class Cable;
template <typename C> class Socket;

// CLASS DECLARATION

class Interface
{
  // DISABLED

private:

  Interface();                                    // zero-argument constructor
  Interface(const Interface& orig);               // copy constructor
  Interface& operator= (const Interface& orig);   // copy assignment operator

  // CREATORS

protected:                                   // access for templated inheritance hierarchy

  Interface
  (const std::string& hostId,                // host is known at construction time
   const std::string& partnerId,             // empty string for sockets
   const std::string& ifQualifier,           // interface qualifier
   const std::string& commodityId);          // a common commodity is required

protected:

  // PROTECTED VIRTUAL DESTRUCTORS AND SMART POINTERS: first, the
  // use of 'virtual' destructors mean the correct destructors
  // are called.  Second, the use of 'protected' rather than
  // 'public' means that a raw pointer (extracted from a smart
  // pointer, say) cannot use 'delete', but (the original) smart
  // pointer still works fine -- see Karlsson (2006 pp23-24) for
  // an explanation and also Sutter and Alexandrescu (2005 pp90-91).
  //
  // Concrete classes need 'public' destructors and should also
  // be 'virtual' if further derived.

  virtual
  ~Interface() = 0;                          // create abstract class

  // FUNCTIONS FOR OPTIMIZATION SUB-PROBLEMS (OSP)

public:

  // the first two are just wrappers to the third

  bool                                       // 'true' if OSP constraint is completed
  bindOsp                                    // map OSP variables to block interfaces
  (shared_ptr<svif::SolverIf> solver,        // current solver interface instance
   const int                  gol);          // global col from relevant OSP call

  bool                                       // 'true' if OSP constraint is completed
  bindOsp                                    // map OSP variables to block interfaces
  (shared_ptr<svif::SolverIf> solver,        // current solver interface instance
   const int                  gol1,          // global col from relevant OSP call
   const int                  gol2);         // global col from relevant OSP call

  bool                                       // 'true' if OSP constraint is completed
  bindOsp                                    // map OSP variables to block interfaces
  (shared_ptr<svif::SolverIf> solver,        // current solver interface instance
   const std::vector<int>     gols);         // global col from relevant OSP call

  // FUNCTIONS FOR HOST SUPPORT

public:

  shared_ptr<Entity>
  getPartner();                              // dialog should be via the common commodity

  shared_ptr<Commodity>
  getCm()
    throw(xeona::bad_subentity_label);       // exception specification

  double&                                    // bind to reference or assign directly
  tieFlow();                                 // CAUTION: overloaded

  void
  tieFlow                                    // CAUTION: overloaded
  (double& flow);                            // set by argument

  void
  sayFlow();                                 // for testing purposes

  void
  sayAll                                     // for testing purposes
  (const std::string& name = "");            // optional name

  // CONNECTIVITY FUNCTIONS -- in order of calling

public:

  static
  bool
  connectAll();                              // point of entry

private:

  template <typename C>
  static
  shared_ptr<Interface>                      // zero on bad cast, empty on locate fail
  cableCast
  (const shared_ptr<Interface>& interface);

  template <typename C>                      // 'C' is restricted to Commodity sub-classes
  static                                     // NOTE: static
  shared_ptr<Socket<C> >                     // zero on bad cast, empty on locate fail
  locateSocket
  (const std::string& socketKey,             // with/without dot-qualification
   const std::string& cableId);

  template <typename C>                      // remember specialization can be employed
  static
  bool
  makeConnection
  (shared_ptr<Cable<C>  >& cable,
   shared_ptr<Socket<C> >& socket);

  // FUNCTIONS PROVIDED FOR UNIT TESTING

public:

  std::string
  getMyKey();

  bool
  isConnected();

  static
  void
  reset();

  static
  int
  getInterfaceCount();

  static
  int
  getConnectionCount();

  static
  bool
  isComplete();

  // UTILITY FUNCTIONS

private:

  void
  setConnected();

  // INSTANCE DATA

protected:                                   // used by 'Cable<C>' and 'Socket<C>' ctors

  std::string                  d_hostId;          // host block
  std::string                  d_partnerId;       // partner block
  std::string                  d_ifQualifier;     // interface qualifier
  std::string                  d_commodityId;     // associated commodity

  std::string                  d_myKey;           // dot-qualified key for me
  std::string                  d_counterKey;      // dot-qualified key for my counter

private:

  int                          d_gol1;            // duly stored first gol
  bool                         d_connected;       // 'true' if connection made

  shared_ptr<Commodity>        d_commodity;       // linked in commodity as intensities
  shared_ptr<double>           d_flow;            // flow in relevant units as extensity

  assign_ptr<ConnectionOsp>    d_cnn;             // optimization sub-problem

  // STATIC DATA

private:

  static int                   s_interfaceCount;                 // all interfaces
  static int                   s_connectionCount;                // bound interfaces
  static int                   s_connectAllCallCount;

  static std::vector<weak_ptr<Interface> > s_censusCables;       // used for traversals
  static std::vector<weak_ptr<Interface> > s_censusSockets;      // used for traversals

protected:

  static logga::spLogger       s_logger;          // shared_ptr to single logger object

}; // class 'Interface'

// ---------------------------------------------------------
//  CLASS           : Cable <>
// ---------------------------------------------------------
//  Description  : a block-to-block interface is a key 'xeona' abstraction
//  Role         : to provide type-safe connectivity while keeping data overheads low
//  Techniques   : restricted templates, static factory function, no data members
//  Status       : complete
// ---------------------------------------------------------

template <typename C>                        // specializations can also be defined
class Cable :
  public Interface
{
  // DISABLED

private:

  Cable();                                   // zero-argument constructor
  Cable(const Cable& orig);                  // copy constructor
  Cable& operator= (const Cable& orig);      // copy assignment operator

  // CREATORS - with private constructor accessed via public static factory function

private:

  Cable
  (const std::string& hostId,                // described for factory function (below)
   const std::string& partnerId,
   const std::string& ifQualifier,
   const std::string& commodityId);

public:

  static                                     // NOTE: static factory function
  shared_ptr<Cable<C> >
  create                                     // sub-id form (added later)
  (const std::string& hostId,                // me
   const std::string& targetId_ifQualifier,  // form with dot-separated socket qualifier
   const std::string& commodityId);          // associated commodity

}; // class 'Cable<>'

// ---------------------------------------------------------
//  CLASS           : Socket <>
// ---------------------------------------------------------

template <typename C>                        // specializations can also be defined
class Socket :
  public Interface
{
  // DISABLED

private:

  Socket();                                  // zero-argument constructor
  Socket(const Socket& orig);                // copy constructor
  Socket& operator= (const Socket& orig);    // copy assignment operator

  // CREATORS - with private constructor accessed via public static factory function

private:

  Socket
  (const std::string& hostId,                // described for factory function (below)
   const std::string& partnerId,
   const std::string& ifQualifier,
   const std::string& commodityId);

public:

  static                                     // NOTE: static factory function
  shared_ptr<Socket<C> >
  create
  (const std::string& hostId,                // me
   const std::string& ifQualifier,           // my socket qualifier
   const std::string& commodityId);          // associated commodity

}; // class 'Socket<>'

#endif // _CONEX_H_

//  end of file

