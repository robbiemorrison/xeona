//  XEONA ENERGY SYSTEMS MODELING ENVIRONMENT
//
//  file-create-name : gateway.cc
//  file-create-date : Thu 23-Oct-2008 11:45 UTC
//  file-initiator   : Robbie Morrison <robbie@actrix.co.nz>
//
//  file-role        : gateway entity which span commitment domains / implementation
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
//  $URL: file:///home/robbie/svn-root/xeona/futz/trunk/xeona1/b/gate.cc $
//
//  GENERAL NOTES FOR THIS FILE

//  LOCAL AND SYSTEM INCLUDES

#include "gate.h"             // companion header for this file (place first)

#include "../c/util3.h"       // free functions for floating point comparison
#include "../c/recset.h"      // records and fields and also record-sets
#include "../c/conex.h"       // create and connect block interfaces
#include "../b/optprob.h"     // optimization sub-problem and key sub-classes
#include "../b/optgate.h"     // various OSPs for gateways
#include "../b/domcon.h"      // domain controller entity
#include "../b/commods01.h"   // concrete commodities 1
#include "../b/commods.h"     // commodities hierarchy

#include "../a/logger.h"      // standard logging functionality (as required)
#include "../c/smart_ptr.h"   // toggle between Boost and TR1 smart pointers
#include ".././common.h"      // common definitions for project (place last)

#include <limits>             // numeric_limits<T>::infinity() and similar
#include <string>             // C++ strings
#include <sstream>            // string-streams

#include <boost/format.hpp>   // printf style formatting

//  FORWARD (PARTIAL) DECLARATIONS

namespace svif { class SolverIf; }           // member function argument

//  CODE

// ---------------------------------------------------------
//  FREE FUNCTION   : ::report
// ---------------------------------------------------------
//  Description  : stringifies a 'Gateway::bounded_type'
//  Role         : 'GateCom<C>::sayStatus' function
//  Techniques   : 'boost::tuple::get' member function
//  Status       : complete
// ---------------------------------------------------------

namespace
{
  std::string                                // "lower upper"
  report
  (const Gateway::bounded_type bounds)       // 'std::pair'
  {
    const std::string sep = "  ";            // separating string
    std::ostringstream oss;
    oss << boost::format("%8g") % bounds.first
        << sep
        << boost::format("%8g") % bounds.second;
    return oss.str();
  }
} // unnamed namespace

// ---------------------------------------------------------
//  CLASS           : Gateway
// ---------------------------------------------------------

//  STATIC DEFINITIONS

const double Gateway::s_inf = std::numeric_limits<double>::infinity();
const double Gateway::s_nil = std::numeric_limits<double>::quiet_NaN();

// CREATORS

Gateway::Gateway
(const std::string entityId,
 Record&           record,
 const int         commitmentModeSum) :
  Block(entityId, record),
  d_techCapacity(std::make_pair(s_nil , s_nil)),  // nonsensical value
  d_commCapacity(std::make_pair(s_nil , s_nil)),  // nonsensical value
  d_capacity(std::make_pair(s_nil, s_nil)),       // nonsensical value
  d_transaction(s_nil),                           // nonsensical value
  d_totalCost(s_nil),                             // nonsensical value
  d_dutyGol(-1),                                  // nonsensical value
  d_debugHxTransacts(),
  d_tilde(false),
  d_theta(false),
  d_star(false),
  d_hash(false)
{
  s_logger->repx(logga::dbug, "constructor call", getIdAndKind());
}

Gateway::~Gateway()
{
  s_logger->repx(logga::adhc, "destructor call", getIdAndKind());
}

// ACCESSORS

bool Gateway::getTilde() const { return d_tilde; }
bool Gateway::getTheta() const { return d_theta; }
bool Gateway::getStar()  const { return d_star;  }
bool Gateway::getHash()  const { return d_hash;  }

bool
Gateway::getTransacted() const
{
  const bool status = xeona::isNan(d_transaction) ? false : true;
  s_logger->repx(logga::adhc, "transaction status", status);
  return status;
}

double
Gateway::getTransaction() const
{
  return d_transaction;
}

double
Gateway::getTotalCost() const
{
  return d_totalCost;
}

// MANIPULATORS

void
Gateway::unsetTransaction()
{
  s_logger->repx(logga::dbug, "entering member function", "");
  d_transaction = s_nil;
}

void
Gateway::setLowerCapacity
(const double lowerCta )
{
  d_capacity.first  = lowerCta;
}

void
Gateway::setUpperCapacity
(const double upperCta )
{
  d_capacity.second = upperCta;
}

bool
Gateway::markTilde()
{
  const bool prior = d_tilde;
  d_tilde          = true;
  return prior;
}

bool
Gateway::unmarkTilde()
{
  const bool prior = d_tilde;
  d_tilde          = false;
  return prior;
}

bool
Gateway::markTheta()
{
  const bool prior = d_theta;
  d_theta          = true;
  return prior;
}

bool
Gateway::unmarkTheta()
{
  const bool prior = d_theta;
  d_theta          = false;
  return prior;
}

bool
Gateway::markStar()
{
  const bool prior = d_star;
  d_star           = true;
  return prior;
}

bool
Gateway::unmarkStar()
{
  const bool prior = d_star;
  d_star           = false;
  return prior;
}

bool
Gateway::markHash()
{
  const bool prior = d_hash;
  d_hash           = true;
  return prior;
}

bool
Gateway::unmarkHash()
{
  const bool prior = d_hash;
  d_hash           = false;
  return prior;
}

void
Gateway::reset()
{
  s_logger->repx(logga::adhc, "entering member function", getIdAndKind());

  d_capacity    = std::make_pair(0.0, s_inf);     // sale (lower, upper) capacity
  d_transaction = s_nil;                          // sale quantity

  d_dutyGol     = -1;

  d_tilde       = false;                     // depth-first search label
  d_theta       = false;                     // capacity back-track lock
  d_star        = false;                     // transaction forward-track lock
  d_hash        = false;                     // fully-capacitated flag
}

void
Gateway::pushDebugHxTransacts
(const int    step,
 const double one,
 const double two)
{
  d_debugHxTransacts.push_back(boost::make_tuple(step, one, two));
}

void
Gateway::sayDebugHxTransacts
(std::ostream& os)
{
  const std::string pad = "      ";          // hard-coded

  typedef boost::tuple
    <int,
    double,
    double>
    info_type;                               // CAUTION: typedef necessary

  BOOST_FOREACH( info_type t, d_debugHxTransacts )
    {
      const int         step  = boost::tuples::get<0>(t);
      const double      one   = boost::tuples::get<1>(t);
      const double      two   = boost::tuples::get<2>(t);
      const bool        ret   = xeona::almostEqual(one, two, xeona::numic);
      const std::string which = ret ? "okay" : "mismatch **";
      os << pad
         << boost::format("%5d")      % step
         << boost::format(" %30.15f") % one
         << boost::format(" %30.15f") % two
         << "    " << which << "\n";
    }
}

// ---------------------------------------------------------
//  CLASS           : BuySide
// ---------------------------------------------------------

BuySide::BuySide
(const std::string entityId,
 Record&           record,
 const int         commitmentModeSum) :
  Gateway(entityId, record, commitmentModeSum),
  TicToc(commitmentModeSum),
  CostRegister(record),
  d_controller()
{
  s_logger->repx(logga::xtra, "constructor call", "");
}

BuySide::~BuySide()
{
  s_logger->repx(logga::adhc, "destructor call", getIdAndKind());
}

void
BuySide::registerDomain
(const std::string& actorId)
{
  s_logger->repx(logga::dbug, "entering buy-side member function", actorId);

  // CAUTION: friendship required for 'retSharedPtr',
  // note also the need to downcast (increase specialization)
  d_controller = dynamic_pointer_cast<DomainController>(Entity::retSharedPtr(actorId));
}

void
BuySide::registerDomain
(shared_ptr<DomainController> domcon)
{
  s_logger->repx(logga::dbug,
                 "entering buy-side member function",
                 domcon->getIdAndKind());
  d_controller = domcon;
}

void
BuySide::initialize
(const int                  step,
 shared_ptr<svif::SolverIf> solver)          // solver instance passed thru
{
  s_logger->repx(logga::adhc, "entering member function", "");
  TicToc::initialize(step, solver);
  initializeBuySide(step, solver);
}

const int
BuySide::constrain                           // set constraints
(const xeona::DomainMode capacityMode)
{
  s_logger->repx(logga::adhc, "entering member function", "");
  const int buyDutyGol = constrainBuySide(capacityMode);
  d_dutyGol = buyDutyGol;
  return buyDutyGol;
}

void
BuySide::washup()
{
  s_logger->repx(logga::adhc, "entering member function", "");
  washupBuySide();
}

// ---------------------------------------------------------
//  MEMBER FUNCTION : recordTransaction
// ---------------------------------------------------------
//
//  Design notes
//
//      It might be better to make this function virtual and
//      service it from the various concrete classes -- instead
//      of capturing and then using 'd_dutyGol' directly with the
//      buy-side 'd_solver.'
//
// ---------------------------------------------------------

double
BuySide::recordTransaction()
{
  // initial reporting
  s_logger->repx(logga::adhc, "entering member function", "");

  // integrity checks
  if ( ! xeona::isNan(d_transaction) )
    {
      s_logger->repx(logga::warn, "transaction already set", d_transaction);
    }
  if ( d_dutyGol < 1 )
    {
      s_logger->repx(logga::warn, "nonsensical duty gol", d_dutyGol);
    }

  // obtain value from solver
  const double transaction = d_solver->getVarValue(d_dutyGol);

  s_logger->repx(logga::adhc, "transaction recorded", transaction);

  // update state
  d_transaction = transaction;

  // additional reporting as appropriate
  // YEEK 21 CODE (set by '--yeek')
  if ( xeona::yeek == 21 || xeona::yeek == 1 || xeona::yeek == 2 )
    {
      std::ostringstream put;
      put
        << "  gateway status" << "\n"
        << "    identifier  : " << getIdAndKind()                                 << "\n"
        << "    capacities  : " << d_capacity.first << " | " << d_capacity.second << "\n"
        << "    transaction : " << d_transaction                                  << "\n";
      s_logger->repx(logga::dbug, "additional reporting follows, yeek", xeona::yeek);
      s_logger->putx(logga::dbug, put);
    }

  // return
  return d_transaction;
}

// ---------------------------------------------------------
//  CLASS           : SelSide
// ---------------------------------------------------------

SelSide::SelSide
(const std::string entityId,
 Record&           record,
 const int         commitmentModeSum) :
  Gateway(entityId, record, commitmentModeSum),
  TicToc(commitmentModeSum),
  CostRegister(record),
  d_controller()
{
  s_logger->repx(logga::xtra, "constructor call", "");
}

SelSide::~SelSide()
{
  s_logger->repx(logga::adhc, "destructor call", getIdAndKind());
}

void
SelSide::registerDomain
(const std::string& actorId)
{
  s_logger->repx(logga::dbug, "entering sel-side member function", actorId);

  // CAUTION: friendship required for 'retSharedPtr',
  // note also the need to downcast (increase specialization)
  d_controller = dynamic_pointer_cast<DomainController>(Entity::retSharedPtr(actorId));
}

void
SelSide::registerDomain
(shared_ptr<DomainController> domcon)
{
  s_logger->repx(logga::dbug,
                 "entering buy-side member function",
                 domcon->getIdAndKind());
  d_controller = domcon;
}

void
SelSide::initialize
(const int                  step,
 shared_ptr<svif::SolverIf> solver)          // solver instance passed thru
{
  s_logger->repx(logga::adhc, "entering member function", "");
  TicToc::initialize(step, solver);
  initializeSelSide(step, solver);
}

const int
SelSide::constrain                           // set constraints
(const xeona::DomainMode capacityMode)
{
  s_logger->repx(logga::adhc, "entering member function", "");
  const int selDutyGol = constrainSelSide(capacityMode);
  d_dutyGol = selDutyGol;
  return selDutyGol;
}

void
SelSide::washup()
{
  s_logger->repx(logga::adhc, "entering member function", "");
  washupSelSide();
}

// ---------------------------------------------------------
//  CLASS           : GateCom <>
// ---------------------------------------------------------

template <typename C>
GateCom<C>::GateCom
(const std::string entityId,
 Record&           record,
 const int         commitmentModeSum) :
  Gateway(entityId, record, commitmentModeSum),      // CAUTION: virtual base class init
  BuySide(entityId, record, commitmentModeSum),
  SelSide(entityId, record, commitmentModeSum),
  // tied quantities
  d_cable(Cable<C>::create
          (entityId,                         // me
           record.tieSingle<std::string>("socket"),
           record.tieSingle<std::string>("common-commodity"))),
  d_socket(Socket<C>::create
           (entityId,                        // me
            "sock-1",                        // hard-coded socket label
            record.tieSingle<std::string>("common-commodity")))
  // internal quantities
{
  s_logger->repx(logga::dbug, "constructor call", "");
}

template <typename C>
GateCom<C>::~GateCom()
{
  // initial reporting
  s_logger->repx(logga::adhc, "destructor call", getIdAndKind());

  // additional reporting as appropriate
  // YEEK 54 CODE (set by '--yeek')
  if ( xeona::yeek == 54 || xeona::yeek == 1 || xeona::yeek == 2 )
    {
      // function name
      const std::string func = XEONA_FUNC;

      // create report
      std::ostringstream put;
      put << "  gateway destructor report"                                        << "\n"
          << "      function      : " << func                                     << "\n"
          << "      identity      : " << getIdAndKind()                           << "\n"
          << "    transactions history from selside : step offer obligation test" << "\n"
          << "\n";
      Gateway::sayDebugHxTransacts(put);     // stream this information too
      s_logger->repx(logga::info, "additional reporting follows, yeek", xeona::yeek);
      s_logger->addSmartBlank(logga::warn);
      s_logger->putx(logga::warn, put);     // CAUTION: 'logga::warn' is correct
    }
}

template <typename C>
void
GateCom<C>::unitTestSay()
{
#ifdef _XUTEST
  // no code is correct
#endif // _XUTEST
}

template <typename C>
shared_ptr<DomainController>
GateCom<C>::hop
(const shared_ptr<DomainController> me)
  throw(xeona::hop_limit_reached,
        xeona::no_gateway_controller)        // exception specifications
{
  // flow control trip
  static const int hopLimit = xeona::ctaHopLimit;      // per interval, set in "common.cc"

  // initial reporting
  s_logger->repx(logga::adhc, "entering member function, hop limit", hopLimit);

  // function name
  const std::string func = XEONA_FUNC;

  // check the call count
  static int callCount        =  0;
  static int previousCallStep = -1;               // minus one forces first reset
  const  int currentCallStep  = BuySide::d_step;  // either side would do
  if ( currentCallStep > previousCallStep )       // a new horizon interval detected
    {
      previousCallStep = currentCallStep;
      callCount        = 0;
    }
  callCount++;
  if ( hopLimit != 0 && callCount > hopLimit )    // zero means disable this trip
    {
      const std::string myId = getIdAndKind();
      std::ostringstream put;
      put << "  CTA hop limit exceeded"                                           << "\n"
          << "    function                  : " << func                           << "\n"
          << "    identity                  : " << myId                           << "\n"
          << "    preset xeona::ctaHopLimit : " << hopLimit                       << "\n"
          << "    current call count        : " << callCount                      << "\n"
          << "    current call step         : " << currentCallStep                << "\n"
          << "  possible causes:"                                                 << "\n"
          << "    * one or more originating domain not listed with overseer"      << "\n"
          << "  an originating domain possesses a commodity SOURCE of some form"  << "\n";
      s_logger->repx(logga::warn, "hop count exceeds limit", callCount);
      s_logger->putx(logga::dbug, put);

      // throw
      if ( xeona::nopro == false )           // meaning option '--krazy' not applied
        {
          s_logger->repx(logga::warn, "about to throw", "xeona::hop_limit_reached");
          throw xeona::hop_limit_reached(myId, func, callCount, hopLimit);
        }
      else
        {
          s_logger->repx(logga::warn, "would normally throw", "xeona::hop_limit_reached");
        }
   }

  // some protection against incomplete models
  if ( BuySide::d_controller == 0 )
    {
      // throw
      const std::string myId = getIdAndKind();
      if ( xeona::nopro == false )           // meaning option '--krazy' not applied
        {
          s_logger->repx(logga::warn,
                         "about to throw",
                         "xeona::no_gateway_controller / buyside");
          throw xeona::no_gateway_controller(myId, func, "buyside");
        }
      else
        {
          s_logger->repx(logga::warn,
                         "would normally throw",
                         "xeona::no_gateway_controller / buyside");
        }
    }
  if ( SelSide::d_controller == 0 )
    {
      // throw
      const std::string myId = getIdAndKind();
      if ( xeona::nopro == false )           // meaning option '--krazy' not applied
        {
          s_logger->repx(logga::warn,
                         "about to throw",
                         "xeona::no_gateway_controller / selside");
          throw xeona::no_gateway_controller(myId, func, "selside");
        }
      else
        {
          s_logger->repx(logga::warn,
                         "would normally throw",
                         "xeona::no_gateway_controller / selside");
        }
    }

  // additional reporting as appropriate
  // YEEK 19 CODE (set by '--yeek')
  if ( xeona::yeek == 19 || xeona::yeek == 26 || xeona::yeek == 1 || xeona::yeek == 2 )
    {
      std::string direction = "(null domain controller)";
      if ( me )
        {
          if ( SelSide::d_controller == me ) direction = "buyward";
          else                               direction = "selward";
        }
      const std::string func = XEONA_FUNC;   // preprocessor macro defined in 'common.h'
      std::ostringstream put;
      put << "  function " << func                                              << "\n"
          << "    host identifier : " << getIdAndKind()                         << "\n"
          << "    me (argument)   : " << me->getIdAndKind()                     << "\n"
          << "    buy-side domain : " << BuySide::d_controller->getIdAndKind()  << "\n"
          << "    sel-side domain : " << SelSide::d_controller->getIdAndKind()  << "\n"
          << "    direction       : " << direction                              << "\n";
      s_logger->repx(logga::dbug, "additional reporting follows, yeek", xeona::yeek);
      s_logger->putx(logga::dbug, put);
    }

  // active code
  if ( me )                                  // not empty or not holding null pointer
    {
      // simply return my counterparty
      if ( SelSide::d_controller == me )
        return BuySide::d_controller;
      else
        return SelSide::d_controller;
    }
  else                                       // empty or holding null pointer
    {
      // log warning and return an empty shared pointer
      s_logger->repx(logga::warn, "empty or null shared pointer given", "");
      return shared_ptr<DomainController>();
    }
}

template <typename C>
shared_ptr<Block>
GateCom<C>::getDemander() const
  throw(xeona::no_gateway_demander)          // exception specification
{
  // preamble
  const std::string myId = getIdAndKind();

  // attempt to recover
  shared_ptr<Entity> entity = d_socket->getPartner();
  if ( entity == 0 )
    {
      s_logger->repx(logga::warn, "get partner call failed", "");
      if ( xeona::nopro == false )           // meaning option '--krazy' not applied
        {
          s_logger->repx(logga::warn, "about to throw", "xeona::no_gateway_demander");
          throw xeona::no_gateway_demander(myId, XEONA_FUNC);
        }
      else
        {
          s_logger->repx(logga::warn,
                         "would normally throw",
                         "xeona::no_gateway_demander");
        }
    }

  // attemp to cast
  shared_ptr<Block>  block = dynamic_pointer_cast<Block>(entity);
  if ( block == 0 )
    {
      const std::string mySocketPartnerId = entity->getIdAndKind();
      s_logger->repx(logga::warn, "Block cast failed", "");
      if ( xeona::nopro == false )           // meaning option '--krazy' not applied
        {
          s_logger->repx(logga::warn, "about to throw", "xeona::no_gateway_demander");
          throw xeona::no_gateway_demander(myId, XEONA_FUNC, mySocketPartnerId);
        }
      else
        {
          s_logger->repx(logga::warn,
                         "would normally throw",
                         "xeona::no_gateway_demander");
        }
    }
  return block;

} // function 'GateCom<C>::getDemander'

template <typename C>
shared_ptr<Block>
GateCom<C>::getSupplier() const
{
  shared_ptr<Entity> entity = d_cable->getPartner();
  shared_ptr<Block>  block  = dynamic_pointer_cast<Block>(entity);
  if ( block == 0 ) s_logger->repx(logga::warn, "Block cast failed", "");
  return block;

} // function 'GateCom<C>::getSupplier'

  // PROVIDED FOR DEVELOPMENT ASSISTANCE

template <typename C>
void
GateCom<C>::sayStatus
(std::ostream& os)
{
  // function name
  const std::string func = XEONA_FUNC;

  // other data
  const std::string saled  = getTransacted() ? "complete" : "not sold **";
  const std::string tilded = getTilde()      ? "set"      : "not set";     // no asterisks
  const std::string hashed = getHash()       ? "fully"    : "not fully **";
  const std::string theted = getTheta()      ? "set"      : "not set **";
  const std::string stared = getStar()       ? "set"      : "not set **";

  // stream data
  os << "  gateway base status report"                                            << "\n"
     << "      identifier                    : " << getIdAndKind()                << "\n"
     << "      function                      : " << func                          << "\n"
     << "    interest"                                                            << "\n"
     << "      tech capacity                 : " << ::report(d_techCapacity)      << "\n"
     << "      comm capacity                 : " << ::report(d_commCapacity)      << "\n"
     << "    direct"                                                              << "\n"
     << "      sale capacity                 : " <<  ::report(d_capacity)         << "\n"
     << "      sale quantity (d_transaction) : " << d_transaction                 << "\n"
     << "      total cost                    : " << d_totalCost                   << "\n"
     << "      duty gol (transaction)        : " << d_dutyGol                     << "\n"
     << "    public calls"                                                        << "\n"
     << "      sale status                   : " << saled                         << "\n"
     << "      transaction                   : " << getTransaction()              << "\n"
     << "      total cost                    : " << getTotalCost()                << "\n"
     <<  "   DFS support"                                                         << "\n"
     << "      depth first search label  (~) : " << tilded                        << "\n"
     << "    CTA support"                                                         << "\n"
     << "      capacitated               (#) : " << hashed                        << "\n"
     << "      capacity backtrack lock   (0) : " << theted                        << "\n"
     << "      transaction fwdtrack lock (*) : " << stared                        << "\n";
}

// ---------------------------------------------------------
//  EXPLICIT TEMPLATE INSTANTIATIONS
// ---------------------------------------------------------

// CAUTION: explicit template instantiations must be placed at
// the very end of the implementation file
//
// For more information, see the excellent documentation in
// 'recset.cc' regarding explicit template instantiations.

// Commodity typedefs for convenience

typedef CmOxidize      Oxid;
typedef CmCarbonCert   Cert;
typedef CmCarbonSeq    Cseq;
typedef CmElectricity  Elec;
typedef CmWork         Work;
typedef CmHeat         Heat;
typedef CmThermalFluid Thrm;
typedef CmFunds        Fund;

// class 'GateCom<>'

template GateCom<Oxid>::GateCom(const std::string, Record&, const int);
template GateCom<Cert>::GateCom(const std::string, Record&, const int);
template GateCom<Cseq>::GateCom(const std::string, Record&, const int);
template GateCom<Elec>::GateCom(const std::string, Record&, const int);
template GateCom<Work>::GateCom(const std::string, Record&, const int);
template GateCom<Heat>::GateCom(const std::string, Record&, const int);
template GateCom<Thrm>::GateCom(const std::string, Record&, const int);
template GateCom<Fund>::GateCom(const std::string, Record&, const int);

template GateCom<Oxid>::~GateCom();
template GateCom<Cert>::~GateCom();
template GateCom<Cseq>::~GateCom();
template GateCom<Elec>::~GateCom();
template GateCom<Work>::~GateCom();
template GateCom<Heat>::~GateCom();
template GateCom<Thrm>::~GateCom();
template GateCom<Fund>::~GateCom();

template void GateCom<Oxid>::unitTestSay();
template void GateCom<Cert>::unitTestSay();
template void GateCom<Cseq>::unitTestSay();
template void GateCom<Elec>::unitTestSay();
template void GateCom<Work>::unitTestSay();
template void GateCom<Heat>::unitTestSay();
template void GateCom<Thrm>::unitTestSay();
template void GateCom<Fund>::unitTestSay();

template shared_ptr<Block> GateCom<Oxid>::getDemander() const;
template shared_ptr<Block> GateCom<Cert>::getDemander() const;
template shared_ptr<Block> GateCom<Cseq>::getDemander() const;
template shared_ptr<Block> GateCom<Elec>::getDemander() const;
template shared_ptr<Block> GateCom<Work>::getDemander() const;
template shared_ptr<Block> GateCom<Heat>::getDemander() const;
template shared_ptr<Block> GateCom<Thrm>::getDemander() const;
template shared_ptr<Block> GateCom<Fund>::getDemander() const;

template shared_ptr<Block> GateCom<Oxid>::getSupplier() const;
template shared_ptr<Block> GateCom<Cert>::getSupplier() const;
template shared_ptr<Block> GateCom<Cseq>::getSupplier() const;
template shared_ptr<Block> GateCom<Elec>::getSupplier() const;
template shared_ptr<Block> GateCom<Work>::getSupplier() const;
template shared_ptr<Block> GateCom<Heat>::getSupplier() const;
template shared_ptr<Block> GateCom<Thrm>::getSupplier() const;
template shared_ptr<Block> GateCom<Fund>::getSupplier() const;

template void GateCom<Oxid>::sayStatus(std::ostream&);
template void GateCom<Cert>::sayStatus(std::ostream&);
template void GateCom<Cseq>::sayStatus(std::ostream&);
template void GateCom<Elec>::sayStatus(std::ostream&);
template void GateCom<Work>::sayStatus(std::ostream&);
template void GateCom<Heat>::sayStatus(std::ostream&);
template void GateCom<Thrm>::sayStatus(std::ostream&);
template void GateCom<Fund>::sayStatus(std::ostream&);

// ---------------------------------
//  derived instantiations
// ---------------------------------

// for convenience

typedef CmOxidGas        OGas;
typedef CmOxidNaturalGas NatG;
typedef CmOxidBiocoal    BioC;
typedef CmOxidHydrogen   Htwo;

// class 'GateCom<>'

template GateCom<OGas>::GateCom(const std::string, Record&, const int);
template GateCom<NatG>::GateCom(const std::string, Record&, const int);
template GateCom<BioC>::GateCom(const std::string, Record&, const int);
template GateCom<Htwo>::GateCom(const std::string, Record&, const int);

template GateCom<OGas>::~GateCom();
template GateCom<NatG>::~GateCom();
template GateCom<BioC>::~GateCom();
template GateCom<Htwo>::~GateCom();

template void GateCom<OGas>::unitTestSay();
template void GateCom<NatG>::unitTestSay();
template void GateCom<BioC>::unitTestSay();
template void GateCom<Htwo>::unitTestSay();

template shared_ptr<Block> GateCom<OGas>::getDemander() const;
template shared_ptr<Block> GateCom<NatG>::getDemander() const;
template shared_ptr<Block> GateCom<BioC>::getDemander() const;
template shared_ptr<Block> GateCom<Htwo>::getDemander() const;

template shared_ptr<Block> GateCom<OGas>::getSupplier() const;
template shared_ptr<Block> GateCom<NatG>::getSupplier() const;
template shared_ptr<Block> GateCom<BioC>::getSupplier() const;
template shared_ptr<Block> GateCom<Htwo>::getSupplier() const;

template void GateCom<OGas>::sayStatus(std::ostream&);
template void GateCom<NatG>::sayStatus(std::ostream&);
template void GateCom<BioC>::sayStatus(std::ostream&);
template void GateCom<Htwo>::sayStatus(std::ostream&);

//  end of file

