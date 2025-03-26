//  XEONA ENERGY SYSTEMS MODELING ENVIRONMENT
//
//  file-create-name : gateway.h
//  file-create-date : Thu 23-Oct-2008 11:45 UTC
//  file-initiator   : Robbie Morrison <robbie@actrix.co.nz>
//
//  file-role        : gateway entity which span commitment domains / header
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
//  $URL: file:///home/robbie/svn-root/xeona/futz/trunk/xeona1/b/gate.h $
//
//  GENERAL NOTES FOR THIS FILE
//
//  Gateway-related notation
//
//      To be read in conjunction with unit 'optgate'.
//
//      The following is based on 'GateStatedTariff'.  The
//      "in-side" is accessible to both the buy-side and
//      sel-side, hence the name.
//
//          side   role          OSP
//          -------------------------------------------
//          buy    technical     QanTechCapacity
//          in     offer         OfrTariffSet
//          sel    obligation    QanObligToSupply
//          sel    technical     QanTechCapacity
//          -------------------------------------------
//          abbreviations:
//          OSP = optimization sub-problem
//          ofr = offer, qan = quantity
//
//  Lead-up
//
//      Lead-up development in 'frag-xeona-gateway-6.cc'.
//
//  Diamond inheritance
//
//      Overview
//
//          The design of the gateway entity make use of a
//          diamond inheritance graph.  This necessitates, in
//          this case, virtual inheritance and thereby introduces
//          some additional considerations, including base class
//          initialization.
//
//          See Stroustrup (1997 pp396-397) for a similar 'Radio'
//          example which demonstrates "diamond-shaped
//          inheritance".
//
//      Virtual base classes
//
//          Some comments from Lischner (2003):
//
//              "When constructing a class that has virtual
//              base classes, the most-derived class's
//              constructors must initialize all the virtual
//              base classes.  If a constructor does not
//              explicitly initialize a virtual base class, the
//              virtual base class's default [zero-argument]
//              constructor is used.  Initializers for the
//              virtual base classes are ignored when all base
//              class constructors run."  (p165)
//
//              "Warning: You should design your virtual base
//              classes so they require only the default
//              constructor.  Otherwise, you impose a burden on
//              every derived class to initialize the virtual
//              base properly.  Any change to the parameters of
//              the virtual base constructor necessitates a
//              corresponding change to ever constructor for
//              every class that derives, directly or indirectly,
//              from the virtual base class."  (p166)
//
//          Indeed, Lischner's advice has NOT been taken and the
//          aforementioned burden applies.  That said, the base
//          class constructor will be (in the absence of
//          catastrophes) completely stable.
//
//      Prior code
//
//          From r2197 to r2000, two gateway implementations were
//          supported.  Refer to r2200 for the original basic
//          gateway implementation (note also my alias 'svcatr'):
//
//              $ svn cat --revision 2200 b/gateway.h  | less
//              $ svn cat --revision 2200 b/gateway.cc | less
//
//          See 'frag-diamond-inheritance-2.cc' for background
//          fragment code development in relation to "diamond"
//          inheritance.
//
//      References
//
//          Lischner, Ray.  2003.  C++ in a nutshell : a language
//            and library reference.  O'Reilly and Associates,
//            Sebastopol, California, USA.  ISBN 0-596-00298-X.
//
//          Stroustrup, Bjarne.  1997.  The C++ programming
//            language -- Third edition.  Addison-Wesley, Reading,
//            Massachusetts, USA.  ISBN-10 0-201-88954-4.

//  HEADER GUARD

#ifndef _GATE_H_
#define _GATE_H_

//  LOCAL AND SYSTEM INCLUDES

#include "../f/ospmodes.h"    // domain mode enums (header only)
#include "../b/tictoc.h"      // inherited interface for entities using common calls
#include "../b/costreg.h"     // cost registers
#include "../b/block.h"       // block entity

#include "../a/logger_fwd.h"  // some forward declarations from "logger.h"
#include "../c/smart_ptr.h"   // toggle between Boost and TR1 smart pointers

#include <string>             // C++ strings
#include <sstream>            // string-streams
#include <utility>            // STL pair, make_pair()

//  FORWARD (PARTIAL) DECLARATIONS

class Record;                                // 'Gateway' constructor argument
class DomainController;                      // member function usage and member data

class OfferOsp;                              // 'BuySide' and 'Selgate' member data
class OperationsOsp;                         // 'BuySide' and 'Selgate' member data
class QuantityOsp;                           // 'BuySide' member data

template <typename C> class Cable;           // 'GateCom' member data
template <typename C> class Socket;          // 'GateCom' member data

//  CODE

// ---------------------------------------------------------
//  CLASS           : Gateway (abstract)
// ---------------------------------------------------------
//  Description  : gateway base class
//  Role         : step in the entity inheritance web
//  Techniques   : virtual multiple inheritance, gateway-specific abstract functions
//  Status       : complete
//
//  Ad-hoc thoughts
//
//      Gates only know financial penalties so the objective
//      coefficients should be set iff 'e_shortrunFin' is set,
//      else zeroed.  The financial costs, of course, need to
//      later be calculated and stored.
//
//      The fact that gates only know financial penalties is in
//      keeping with the information destroying characteristic of
//      commodity markets.  Or could gray costs should be
//      supported under different arrangements?
//
//  Coding
//
//      The data members here are common, occur only once, and do
//      not require scope resolution to resolve ambiguity (due to
//      virtual inheritance).
//
// ---------------------------------------------------------

class Gateway :                              // the bottom of gateway tree
  public Block
{

  // TYPEDEFS

public:

  typedef std::pair
  <double,                                   // lower bound
   double>                                   // upper bound
  bounded_type;

  // DISABLED

private:

  Gateway();                                 // zero-argument constructor
  Gateway(const Gateway& orig);              // copy constructor
  Gateway& operator= (const Gateway& orig);  // copy assignment operator

  // CREATORS

public:

  explicit
  Gateway
  (const std::string entityId,
   Record&           record,
   const int         commitmentModeSum);     // passed thru to 'TicToc' constructor

  virtual ~Gateway() = 0;                    // create abstract class

public:

  // ACCESSORS

  // boolean label status for DFS and CTA graph algorithms

  bool getTilde() const;                     // depth-first search label
  bool getTheta() const;                     // capacity back-track lock
  bool getStar()  const;                     // transaction forward-track lock
  bool getHash()  const;                     // fully-capacitated flag

  // sale status

  bool   getTransacted()  const;             // 'true' if "sold"
  double getTransaction() const;
  double getTotalCost()   const;

  // navigation calls for DFS and CTA graph algorithms

  virtual
  shared_ptr<DomainController>               // my partner domain controller
  hop
  (const shared_ptr<DomainController> me)
    throw(xeona::hop_limit_reached,
          xeona::no_gateway_controller) = 0; // exception specifications

  // obtain associated blocks

  virtual
  shared_ptr<Block>
  getDemander() const = 0;                   // obtain demander -- the socket-side block

  virtual
  shared_ptr<Block>
  getSupplier() const = 0;                   // obtain supplier -- the cable-side block

  // MANIPULATORS

  void unsetTransaction();                   // hop-relitigation extension only

  // calls used by 'DomainController::capset' function

  void setLowerCapacity(const double lowerCta);
  void setUpperCapacity(const double upperCta);

  // flag calls for DFS and CTA graph algorithms -- these
  // functions also return their previous status

  bool markTilde();
  bool unmarkTilde();
  bool markTheta();
  bool unmarkTheta();
  bool markStar();
  bool unmarkStar();
  bool markHash();
  bool unmarkHash();

  // reset call for DFS and CTA graph algorithms

  void reset();

protected:

  // these calls are wrapped by normal 'initialize' calls, with
  // the latter defined in the next layer of specialization

  virtual void initializeBuySide(const int step, shared_ptr<svif::SolverIf> solver) = 0;
  virtual void initializeSelSide(const int step, shared_ptr<svif::SolverIf> solver) = 0;

  // these calls are wrapped by normal 'constrain' calls, with
  // the latter defined in the next layer of specialization

  virtual const int constrainBuySide(const xeona::DomainMode capacityMode) = 0;
  virtual const int constrainSelSide(const xeona::DomainMode capacityMode) = 0;

  // these calls are necessarily redefined in the various
  // concrete gateways

  virtual void washupBuySide() = 0;
  virtual void washupSelSide() = 0;

  // strictly for debugging

  void
  pushDebugHxTransacts
  (const int    step,
   const double one,
   const double two);

  void
  sayDebugHxTransacts
  (std::ostream& os);

  // INSTANCE DATA

protected:

  // the following may not be necessary or even convenient

  bounded_type           d_techCapacity;     // technical (lower, upper) capacity
  bounded_type           d_commCapacity;     // commercial (lower, upper) capacity

  // canonical cross-domain data (derived data such as marginal
  // price and total payment falls under the responsibility of
  // the concrete gateway class)

  bounded_type           d_capacity;         // sale (lower, upper) capacity
  double                 d_transaction;      // sale quantity
  double                 d_totalCost;        // cost of transaction

  int                    d_dutyGol;          // transaction global col

private:

  // strictly for debugging (could possible be removed now)

  std::vector<boost::tuple<int, double, double> >    d_debugHxTransacts;

  // graph algorithm boolean labels

  bool                   d_tilde;            // depth-first search label
  bool                   d_theta;            // capacity back-track lock
  bool                   d_star;             // transaction forward-track lock
  bool                   d_hash;             // fully-capacitated flag

  // STATIC DATA

protected:

  // for convenience

  static const double    s_inf;              // floating point infinity
  static const double    s_nil;              // nonsensical at NaN (see implementation)

}; // class 'Gateway'

// ---------------------------------------------------------
//  CLASS           : BuySide (abstract)
// ---------------------------------------------------------
//  Description  : add in buy-side functionality
//  Role         : step in the entity inheritance web
//  Techniques   : brings in the 'TicToc' interface and provides definitions
//  Status       : complete
//
//  Coding
//
//      Add dedicated buy-side members here -- these should be
//      unique by design (or introduced one layer down).
//
// ---------------------------------------------------------

class BuySide :
  public virtual Gateway,
  public TicToc,
  public CostRegister
{
  // CREATORS

public:

  explicit
  BuySide
  (const std::string entityId,
   Record&           record,
   const int         commitmentModeSum);

  virtual ~BuySide() = 0;

  // MANIPULATORS

public:

  void
  registerDomain
  (const std::string& actorId);

  void
  registerDomain
  (shared_ptr<DomainController> domcon);

  virtual
  void
  initialize
  (const int                  step,
   shared_ptr<svif::SolverIf> solver);       // solver instance passed thru

  virtual
  const int
  constrain                                  // set constraints
  (const xeona::DomainMode capacityMode);

  virtual
  void
  washup();                                  // calls 'washupBuySide'

  // store and unset calls for CTA graph algorithm

  double recordTransaction();                // also returns transaction for convenience

  // INSTANCE DATA

protected:

  shared_ptr<DomainController>    d_controller;   // controlling domain for this side

}; // class 'BuySide'

// ---------------------------------------------------------
//  CLASS           : SelSide (abstract)
// ---------------------------------------------------------
//  Description  : add in sel-side functionality
//  Role         : step in the entity inheritance web
//  Techniques   : brings in the 'TicToc' interface and provides definitions
//  Status       : complete
//
//  Coding
//
//      Add dedicated sel-side members here -- these should be
//      unique by design (or declared in 'Gateway').
//
// ---------------------------------------------------------

class SelSide :
  public virtual Gateway,
  public TicToc,
  public CostRegister
{
  // CREATORS

public:

  explicit
  SelSide
  (const std::string entityId,
   Record&           record,
   const int         commitmentModeSum);

  virtual ~SelSide() = 0;

  // MANIPULATORS

public:

  void
  registerDomain
  (const std::string& actorId);

  void
  registerDomain
  (shared_ptr<DomainController> domcon);

  virtual
  void
  initialize
  (const int                  step,
   shared_ptr<svif::SolverIf> solver);       // solver instance passed thru

  virtual
  const int
  constrain                                  // set constraints
  (const xeona::DomainMode capacityMode);

  virtual
  void
  washup();                                  // calls 'washupSelSide'

  // INSTANCE DATA

protected:

  shared_ptr<DomainController>    d_controller;   // controlling domain for this side

}; // class 'SelSide'

// ---------------------------------------------------------
//  CLASS           : GateCom <> (abstract)
// ---------------------------------------------------------
//  Description  : single templated gateway class
//  Role         : for use by concrete gateway classes
//  Techniques   : templated on 'C' for 'Commodity'
//  Status       : complete
//
//  Coding
//
//      Add undifferentiated single members here -- these are now
//      also clear of the multiple virtual ('TicToc' excepted)
//      inheritance web.
//
//      The 'cable' and 'socket' are defined here.
//
// ---------------------------------------------------------

class BandedTariffSet;

template <typename C>
class GateCom :
  public BuySide,                            // CAUTION: virtual inheritance not wanted
  public SelSide                             // CAUTION: virtual inheritance not wanted
{
  // CREATORS

public:

  explicit
  GateCom
  (const std::string entityId,
   Record&           record,
   const int         commitmentModeSum);

  virtual ~GateCom() = 0;

public:

  // FOR UNIT TESTING

  void unitTestSay();                        // strictly for testing

  // ACCESSORS

  virtual
  shared_ptr<DomainController>               // my partner domain controller
  hop
  (const shared_ptr<DomainController> me)
    throw(xeona::hop_limit_reached,
          xeona::no_gateway_controller);     // exception specifications

  virtual
  shared_ptr<Block>
  getDemander() const                        // obtain demander -- the socket-side block
    throw(xeona::no_gateway_demander);       // exception specification

  virtual
  shared_ptr<Block>
  getSupplier() const;                       // obtain supplier -- the cable-side block

  virtual
  shared_ptr<BandedTariffSet>
  obtainTariffSet() const = 0;

  // MANIPULATORS

  // CAUTION: the note below for 'constrain' may apply here also
  // -- but this requirement was not expressly checked for
  // 'initialize' -- alternatively the caution may be stale in
  // both cases (which I suspect it might be)

  virtual
  void
  initialize
  (const int                  step,
   shared_ptr<svif::SolverIf> solver)        // solver instance passed thru
  {
    s_logger->repx(logga::warn, "use upcast call instead", "");
  }

  // CAUTION: the compiler requires the call diamond to be
  // completed, hence this next 'constrain' declaration, also
  // defined here so no template instantiations

  virtual
  const int
  constrain
  (const xeona::DomainMode capacityMode)
  {
    s_logger->repx(logga::warn, "use upcast call instead", "");
    return -2;                               // nonsensical return
  }

  virtual
  void
  washup()
  {
    s_logger->repx(logga::warn, "use upcast call instead", "");
  }

  // PROVIDED FOR DEVELOPMENT ASSISTANCE

  void
  sayStatus
  (std::ostream& os);

  // INSTANCE DATA

protected:

  // tied quantities

  shared_ptr<Cable<C> >     d_cable;
  shared_ptr<Socket<C> >    d_socket;

}; // class 'GateCom<>'

#endif // _GATE_H_

//  end of file

