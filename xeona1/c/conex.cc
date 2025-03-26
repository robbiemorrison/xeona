//  XEONA ENERGY SYSTEMS MODELING ENVIRONMENT
//
//  file-create-name : conex.cc
//  file-create-date : Wed 16-Jul-2008 15:22 UTC
//  file-initiator   : Robbie Morrison <robbie@actrix.co.nz>
//
//  file-role        : create and connect block interfaces / implementation
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
//  $URL: file:///home/robbie/svn-root/xeona/futz/trunk/xeona1/c/conex.cc $

//  LOCAL AND SYSTEM INCLUDES

#include "conex.h"            // companion header for this file (place first)

#include "../c/util1.h"       // free functions which offer general utilities 1
#include "../b/optprob.h"     // optimization sub-problem and key sub-classes
#include "../b/commods01.h"   // concrete commodities 1
#include "../b/commods.h"     // commodities hierarchy

#include "../a/logger.h"      // standard logging functionality (as required)
#include "../c/smart_ptr.h"   // toggle between Boost and TR1 smart pointers
#include ".././common.h"      // common definitions for project (place last)

#include <iomanip>            // setw() and family
#include <sstream>            // string-streams
#include <string>             // C++ strings
#include <typeinfo>           // run-time type information (RTTI)

#include <cctype>             // C-style char classification, case conversion

#include <boost/algorithm/string.hpp>   // string recasing, trimming, splitting
#include <boost/foreach.hpp>            // BOOST_FOREACH iteration macro

//  CODE

// ---------------------------------------------------------
//  CLASS           : Interface (abstract base class)
// ---------------------------------------------------------
//  Description  : base class for 'Cable<C>' and 'Socket<C>' class templates
//  Role         : provide common static support for interface instantiations
//  Techniques   : 'weak_ptr', 'shared_ptr' casting, inheritance
//  See also     : 'Cable<C>' and 'Socket<C>'
//  Status       : complete
// ---------------------------------------------------------
//
//  Design notes
//
//      The rationale for this abstract base class
//
//          Templates take priority over static members --
//          therefore, this abstract base class (ABC) provides
//          static support before the templates are applied to
//          create the required sub-classes.
//
//      Weak pointers
//
//          'weak_ptr' smart pointers are used in the 's_census*'
//          static data members of type
//          'std::vector<weak_ptr<Interface> >', in order that
//          the reference count remains correct.
//
//          Karlsson (2006 p49) writes ".. when you have a
//          weak_ptr that's observing some resource, you'll
//          eventually want to access that resource.  To do so,
//          the weak_ptr must be converted to a shared_ptr,
//          because the weak_ptr alone does not allow direct
//          access to the resource."
//
//          A weak pointer is created from a shared pointer.
//
//              weak_ptr<T>(sp)        construction from shared_ptr<T>
//
//          There are two ways of creating a shared_ptr from a
//          weak_ptr, noting that implicit template type
//          conversions can also apply:
//
//              shared_ptr<T> sp(wp);          // [1]
//              shared_ptr<T> sp = wp.lock();  // [2]
//
//            note 1 : throws 'std::tr1::bad_weak_ptr' if wp is empty
//            note 2 : sp is empty if wp is empty
//
//      New commodities
//
//          Two modifications are required should new commodity
//          sub-classes need to be added:
//
//              extend cast list in 'connectAll'
//              extend explicit template instantiations list
//
//  See also
//
//      The design notes for class templates 'Cable<C>' and
//      'Socket<C>'.
//
//  References
//
//      Becker, Pete.  2007.  The C++ Standard Library extensions :
//        a tutorial and reference.  Addison-Wesley, Upper Saddle
//        River, New Jersey, USA.  ISBN 0-321-41299-0.
//
//      Karlsson, Bjoern.  2006.  Beyond the C++ Standard Library :
//        an introduction to Boost.  Addison-Wesley, Upper Saddle
//        River, New Jersey, USA.  ISBN 0-321-13354-4.
//
//      Lischner, Ray.  2003.  C++ in a nutshell : a language and
//        library reference.  O'Reilly and Associates, Sebastopol,
//        California, USA.  ISBN 0-596-00298-X.
//
// ---------------------------------------------------------

// STATIC DEFINITIONS

int Interface::s_interfaceCount      = 0;
int Interface::s_connectionCount     = 0;
int Interface::s_connectAllCallCount = 0;

std::vector<weak_ptr<Interface> > Interface::s_censusSockets;
std::vector<weak_ptr<Interface> > Interface::s_censusCables;

logga::spLogger Interface::s_logger  = logga::ptrLogStream(); // bind logger on definition

// CREATORS

// ---------------------------------------------------------
//  MEMBER FUNCTION : Interface
// ---------------------------------------------------------

Interface::Interface
(const std::string& hostId,
 const std::string& partnerId,               // empty string for sockets
 const std::string& ifQualifier,
 const std::string& commodityId) :
  d_hostId(hostId),                          // host is known at construction time
  d_partnerId(partnerId),                    // partner is known by cables only
  d_ifQualifier(ifQualifier),                // interface qualifier
  d_commodityId(commodityId),                // a common commodity is required
  d_myKey(),
  d_counterKey(),
  d_gol1(-1),                                // nonsensical value
  d_connected(false),                        // interface is unbound initially
  d_commodity(),                             // contains intensities, linked on connection
  d_flow(new double(0.0)),                   // the characterizing extensity
  d_cnn(new ConnectionOsp())                 // real shared pointer, but OSP lacks solver
{
  // initial reporting
  s_logger->repx(logga::dbug, "constructor call, host id", d_hostId);
}

// ---------------------------------------------------------
//  MEMBER FUNCTION : ~Interface
// ---------------------------------------------------------

Interface::~Interface()
{
  // initial reporting
  s_logger->repx(logga::xtra, "destructor call", "");
}

// FUNCTIONS FOR OPTIMIZATION SUB-PROBLEMS (OSP)

// ---------------------------------------------------------
//  MEMBER FUNCTION : bindOsp (wrappers)
// ---------------------------------------------------------
//  Description  : map OSP variables to block interfaces and then to solver
//  Role         : used within derived 'TechnicalAsset::constrain' calls
//  Techniques   : 'xeona::assign_ptr::revamp'
//  Status       : complete
// ---------------------------------------------------------

bool                                         // 'true' if OSP constraint is completed
Interface::bindOsp                           // map OSP variables to block interfaces
(shared_ptr<svif::SolverIf> solver,          // current solver interface instance
 const int                  gol)             // global col from relevant OSP call
{
  // initial reporting
  s_logger->repx(logga::adhc, "entering member function, gol", gol);

  // load vector
  std::vector<int> vec;
  vec.push_back(gol);

  // main call
  return bindOsp(solver, vec);
}

bool                                         // 'true' if OSP constraint is completed
Interface::bindOsp                           // map OSP variables to block interfaces
(shared_ptr<svif::SolverIf> solver,          // current solver interface instance
 const int                  gol1,            // global col from relevant OSP call
 const int                  gol2)            // global col from relevant OSP call
{
  // initial reporting
  std::ostringstream oss;
  oss << gol1 << " " << gol2;
  s_logger->repx(logga::adhc, "entering member function, gols", oss.str());

  // load vector and continue
  std::vector<int> vec;
  vec.push_back(gol1);
  vec.push_back(gol2);

  // main call
  return bindOsp(solver, vec);
}

// ---------------------------------------------------------
//  MEMBER FUNCTION : bindOsp (principal)
// ---------------------------------------------------------
//  Description  : map OSP variables to block interfaces and then to solver
//  Role         : used within derived 'TechnicalAsset::constrain' calls
//  Techniques   : 'xeona::assign_ptr::revamp'
//  Status       : complete
// ---------------------------------------------------------

bool                                         // 'true' if OSP constraint is completed
Interface::bindOsp                           // map OSP variables to block interfaces
(shared_ptr<svif::SolverIf> solver,          // current solver interface instance
 const std::vector<int>     gols)            // global col from relevant OSP call
{
  // initial reporting
  const int size = gols.size();
  s_logger->repx(logga::adhc, "entering member function, gol size", size);

  // defensive programming
  if ( ! solver )
    {
      s_logger->repx(logga::warn, "solver pointer empty/null", solver);
    }
  if ( ! d_cnn )                             // should not get here
    {
      s_logger->repx(logga::warn, "ConnectionOsp pointer empty/null", d_cnn);
    }

  // process pass count, which is stored in the common
  // optimization sub-problem object -- the 'solver' argument is
  // simply used as an integrity check
  d_cnn->incPassCount(solver);               // initially zero
  const int pass = d_cnn->getPassCount();

  // can be used to visually confirm the solver is the same
  s_logger->repx(logga::adhc, "solver address", solver);

  // active code
  if ( pass == 1 )                           // first pass
    {
      // store 'gols' on first pass
      s_logger->repx(logga::adhc, "first pass, will store gols", "");
      d_cnn->storeGols(gols);                // store in volatile object
      return false;
    }
  else if ( pass == 2 )                      // second pass
    {
      // reestablish connection OSP on second pass, then process
      s_logger->repx(logga::adhc, "second pass, will bind gols", "");

      const std::vector<int> gols1 = d_cnn->recoverGols();
      const std::vector<int> gols2 = gols;

      std::ostringstream put;
      put << "  hollow connection OSP pointer : " << d_cnn.get() << "\n"; // [1]

      // CAUTION: [1] 'get' required, else 'assign_ptr' streams
      // as 'true' or "1" (the reason being an incomplete
      // implementation of 'assign_ptr')

      // pool replace the hollow object with a full object
      s_logger->repx(logga::adhc, "about to revamp", "");
      d_cnn.revamp(new ConnectionOsp(solver, // real instance
                                     0));    // make pass zero (the default in any case)

      put << "  live connection OSP pointer   : " << d_cnn.get() << "\n";
      s_logger->repx(logga::adhc, "reestablish connection OSP complete", "");
      s_logger->putx(logga::adhc, put);

      // bind call
      return d_cnn->bindGols(gols1, gols2);
    }
  else                                       // should not get here
    {
      s_logger->repx(logga::warn, "pass count not {1,2}", pass);
      return false;
    }
}

// FUNCTIONS FOR HOST SUPPORT

// ---------------------------------------------------------
//  MEMBER FUNCTION : getPartner
// ---------------------------------------------------------

shared_ptr<Entity>
Interface::getPartner()
{
  // initial reporting
  s_logger->repx(logga::adhc, "entering member function", "");

  if ( d_connected )
    {
      return Entity::retSharedPtr(d_partnerId);
    }
  else
    {
      s_logger->repx(logga::dbug, "returning empty shared pointer", d_partnerId);
      return shared_ptr<Entity>();
    }
}

// ---------------------------------------------------------
//  MEMBER FUNCTION : getCm
// ---------------------------------------------------------

shared_ptr<Commodity>
Interface::getCm()
  throw(xeona::bad_subentity_label)          // exception specification
{
  // initial reporting
  s_logger->repx(logga::adhc, "entering member function", "");

  // integrity check
  //
  // CAUTION: a null 'd_commodity' will probably segfault
  //
  // the most probable cause is a model entity with a faulty
  // sub-entity label, for instance: "teas-xx.elec-1" instead of
  // "teas-xx.sock-1"

  if ( ! d_commodity )
    {
      if ( xeona::nopro == false )           // meaning option '--krazy' not applied
        {
          s_logger->repx(logga::warn, "about to throw", "xeona::bad_subentity_label");
          throw xeona::bad_subentity_label(d_hostId, XEONA_FUNC);
        }
      else
        {
          std::ostringstream put;
          put << "  in function '" << XEONA_FUNC << "'"                        << "\n";
          put << "  looks like a sub-entity label is incorrect (fix model)"    << "\n"
              << "  a review of previous warn logs should reveal the culprit"  << "\n"
              << "  experience suggests a segfault is immanent"                << "\n"
              << ""                                                            << "\n";
          s_logger->repx(logga::warn, "problems ahead, commodity is null", d_commodity);
          s_logger->putx(logga::warn, put);
        }
    }

  // non-null return
  return d_commodity;
}

// ---------------------------------------------------------
//  MEMBER FUNCTION : tieFlow (overloaded)
// ---------------------------------------------------------

double&                                      // bind to reference or assign directly
Interface::tieFlow()
{
  // initial reporting
  s_logger->repx(logga::adhc, "entering member function", "");

  return *d_flow;
}

void
Interface::tieFlow
(double& flow)                               // set by argument
{
  // initial reporting
  s_logger->repx(logga::adhc, "entering member function", "");

  *d_flow = flow;
}

// ---------------------------------------------------------
//  MEMBER FUNCTION : sayFlow
// ---------------------------------------------------------

void
Interface::sayFlow()                         // for testing purposes
{
  // initial reporting
  s_logger->repx(logga::adhc, "entering member function", "");

  std::ostringstream put;
  put << "  flow (interface " << d_myKey << ")"
      << " = " << *d_flow << "\n";
  s_logger->putx(logga::dbug, put);
}

// ---------------------------------------------------------
//  MEMBER FUNCTION : sayAll
// ---------------------------------------------------------

void
Interface::sayAll                            // for testing purposes
(const std::string& name)
{
  // initial reporting
  s_logger->repx(logga::adhc, "entering member function", "");

  std::ostringstream put;
  put << std::boolalpha;
                        put << "  interface details"                            << "\n";
  if ( ! name.empty() ) put << "    local name (supplied)  : " << name          << "\n";
                        put << "    key                    : " << d_myKey       << "\n"
                            << "    connected              : " << d_connected   << "\n";
  if ( d_connected )    put << "    counter key            : " << d_counterKey  << "\n";
                        put << "    commodity id           : " << d_commodityId << "\n"
                            << "    current flow           : " << *d_flow       << "\n";
  s_logger->putx(logga::dbug, put);
}

// CONNECTIVITY FUNCTIONS

// ---------------------------------------------------------
//  MEMBER FUNCTION : connectAll (static)
// ---------------------------------------------------------
//  Description  : point of entry for making connections
//  Role         : loops cables, calls 'cableCast<C>' under various instantiations
//  Techniques   : static
//  Status       : complete
// ---------------------------------------------------------
//
//  Design notes
//
//      See code for treatment of multiple calls and of cables
//      which are already connected.  Depending on the
//      hash-conditional settings, both can trigger a warning
//      message and premature return.
//
// ---------------------------------------------------------

bool
Interface::connectAll()
{
  // initial reporting
  s_logger->repx(logga::info, "entering static member function", "");

  // preamble
  std::ostringstream put;
  bool ret = true;                           // return 'true' unless something fails

  // check call count -- and abandon if so coded and as required
  s_connectAllCallCount++;                   // initialized to zero
  if ( s_connectAllCallCount != 1 )
    {
      s_logger->repx(logga::warn, "repeat call, count", s_connectionCount);

#if 0 // 0 = allow multiple calls (to enable structural change), 1 = enforce single call
      return false;
#endif // 0

    }

  // integrity checks
  const int numCables  = s_censusCables.size();
  const int numSockets = s_censusSockets.size();

  std::ostringstream oss;
  oss << numCables << " : " << numSockets;
  s_logger->repx(logga::dbug, "cables : sockets", oss.str());

  if ( numCables != numSockets )
    {
      s_logger->repx(logga::warn, "cable/socket imbalance", oss.str());
      std::ostringstream oss;
      put << "  cable/socket imbalance detected"                              << "\n"
          << "    imbalance    : " << oss.str()                               << "\n"
          << "    explanation  : faulty model"                                << "\n"
          << "    likely cause : omitted or incorrect interface connections"  << "\n";
      s_logger->putx(logga::xtra, put);
    }

  // loop thru the cables
  int loopCount = 0;
  BOOST_FOREACH( weak_ptr<Interface> w, s_censusCables )
    {
      ++loopCount;                           // one-based counting
      s_logger->addSmartBlank(logga::xtra);
      put << "loop " << loopCount << "\n";
      s_logger->putx(logga::xtra, put);

      // check weak pointer
      if ( w.expired() )
        {
          s_logger->repx(logga::dbug, "expired weak pointer, cable index", loopCount);
          continue;
        }

      // attempt to recover a working pointer
      shared_ptr<Interface> s = w.lock();    // 'lock' will create empty from empty

      if ( s.get() == 0 )                    // empty or holding null
        {
          if ( s.use_count() == 0 )          // empty
            {
              s_logger->repx(logga::dbug, "empty shared pointer, cable index", loopCount);
            }
          else                               // therefore holding null
            {
              s_logger->repx(logga::dbug, "null shared pointer, cable index", loopCount);
            }
          ret = false;
          continue;                          // loop again
        }

      // skip cables which are already connected OR abandon task
      if ( s->isConnected() )
        {
          s_logger->repx(logga::warn, "cable already connected", s->d_myKey);

#if 0 // 0 = loop again, 1 = abandon task
          s_logger->repx(logga::warn, "abandoning task", "");
          return false;                      // abandon task
#else
          continue;                          // loop again
#endif // 0

        }

      // attempt to cast against a hard-coded list (any new
      // sub-commodities should therefore be added here)
      //
      // the 'cableCast' call, upon success, in turn calls to
      // 'makeConnection'

      if ( Interface::cableCast<CmOxidize>(s)        ) continue;
      if ( Interface::cableCast<CmCarbonSeq>(s)      ) continue;
      if ( Interface::cableCast<CmCarbonCert>(s)     ) continue;
      if ( Interface::cableCast<CmElectricity>(s)    ) continue;
      if ( Interface::cableCast<CmWork>(s)           ) continue;
      if ( Interface::cableCast<CmHeat>(s)           ) continue;
      if ( Interface::cableCast<CmThermalFluid>(s)   ) continue;
      if ( Interface::cableCast<CmFunds>(s)          ) continue;
      if ( Interface::cableCast<CmProductiveLand>(s) ) continue;

      if ( Interface::cableCast<CmOxidBiocoal>(s)    ) continue;
      if ( Interface::cableCast<CmOxidGas>(s)        ) continue;
      if ( Interface::cableCast<CmOxidHydrogen>(s)   ) continue;
      if ( Interface::cableCast<CmOxidNaturalGas>(s) ) continue;

      // failure and function end reporting
      std::ostringstream oss;
      oss << "more details if report " << logga::xtra;
      s_logger->repx(logga::warn, "all 'cableCast' calls failed", oss.str());

      std::ostringstream put;
      put << "  commodity casting problem"                                      << "\n"
          << "    likely cause, given the key was found : sought commodity is not on"
          << " the hardcoded 'Interface::cableCast<Commodity>'"                 << "\n"
          << "      call list in function 'Interface::connectAll', rerun with report "
          << logga::xtra
          << " to view the currently supported casts"                           << "\n"
          << "    otherwise disregard this message and fix earlier problems"
          << ", one of which may be an incorrect \":Elec\" on a class specifier"<< "\n";

      s_logger->putx(logga::dbug, put);
      s_logger->repx(logga::dbug, "leaving member function, returning", "false");

      ret = false;

    } // BOOST_FOREACH

  return ret;                                // 'true' if everything worked
}

// ---------------------------------------------------------
//  MEMBER FUNCTION : cableCast <> (static)
// ---------------------------------------------------------
//  Description  : cast a 'shared_ptr<Interface>' cable and proceed if successful
//  Role         : the next step in the connect chain
//  Techniques   : static, 'dynamic_pointer_cast' for smart pointer downcasting
//  Status       : complete
// ---------------------------------------------------------
//
//  CAUTION: smart pointer casts
//
//      'shared_ptr's have their own const, static, and dynamic
//      casts, namely:
//
//          const_pointer_cast
//          static_pointer_cast
//          dynamic_pointer_cast
//
// ---------------------------------------------------------

template <typename C>
shared_ptr<Interface>                        // zero on bad cast, empty on locate fail
Interface::cableCast
(const shared_ptr<Interface>& interface)
{
  // set the tabstop
  static const int tab = 47;

  // initial reporting
  std::ostringstream oss;
  oss << "entered function 'cableCast<"
      << xeona::trimLeadingDigits(typeid(C).name()) << ">'";
  std::ostringstream put;
  put << std::left << std::setw(tab) << oss.str();
  s_logger->putx(logga::xtra, put);

  // attempt cast based on supplied 'C'
  shared_ptr<Cable<C> > cable = dynamic_pointer_cast<Cable<C> >(interface);  // downcast
  if ( cable == 0 )                          // bad cast returns zero
    {
      put << " : fail cast 1" << "\n";
      s_logger->putx(logga::xtra, put);
      return cable;                          // zero, in other words
    }
  put << " : good cast 1" << "\n";
  s_logger->putx(logga::xtra, put);

  // attempt to locate socket
  std::string socketKey = cable->d_counterKey;
  std::string cableId   = cable->d_hostId;

  shared_ptr<Socket<C> > socket = locateSocket<C>(socketKey, cableId);
  if ( ! socket )                            // either zero or empty
    {
      put << "... unable to locate socket" << "\n";
      s_logger->putx(logga::xtra, put);
      return socket;
    }

  // attempt connection
  s_logger->repx(logga::adhc, "about to make connection using", "");
  put << "  cable pointer  : " << cable  << "\n"
      << "  socket pointer : " << socket << "\n";
  s_logger->putx(logga::adhc, put);

  // attempt connection
  makeConnection(cable, socket);

  return socket;
}

// ---------------------------------------------------------
//  MEMBER FUNCTION : locateSocket <> (static)
// ---------------------------------------------------------
//  Description  : attempt to locate a suitable socket
//  Role         : the next step in the connect chain
//  Techniques   : static, 'dynamic_pointer_cast' for smart pointer downcasting
//  Status       : complete
// ---------------------------------------------------------

template <typename C>                        // C is restricted to Commodity sub-classes
shared_ptr<Socket<C> >                       // zero on bad cast, empty on locate fail
Interface::locateSocket
(const std::string& socketKey,               // with/without dot-qualification
 const std::string& cableId)
{
  // initial reporting
  s_logger->repx(logga::adhc, "entering static member function", "");
  std::ostringstream put;
  put << "entered function 'locateSocket<"
      << xeona::trimLeadingDigits(typeid(C).name()) << ">"
      << "(" << socketKey << ", " << cableId << ")'" << "\n";
  s_logger->putx(logga::xtra, put);

  // preamble
  shared_ptr<Socket<C> > empty;              // used as a fail-to-find return value
  empty = typename shared_ptr<Socket<C> >::shared_ptr();

  // loop thru the sockets
  int loopCount = 0;
  BOOST_FOREACH( weak_ptr<Interface> w, s_censusSockets )
    {
      ++loopCount;

      // attempt to recover a working pointer
      shared_ptr<Interface> s = w.lock();    // 'lock' will create empty from empty
      if ( s.get() == 0 )
        {
          if ( s.use_count() == 0 )
            {
              const int temp = loopCount;
              s_logger->repx(logga::dbug, "empty shared pointer, socket index", temp);
            }
          else
            {
              s_logger->repx(logga::dbug, "null shared pointer, socket index", loopCount);
            }
          continue;                          // loop again
        }

      // attempt to cast against current template
      shared_ptr<Socket<C> > socket = dynamic_pointer_cast<Socket<C> >(s); // downcast
      if ( socket == 0 )
        {
          put << " : fail cast 2 (" << s->d_myKey << ")" << "\n";
          s_logger->putx(logga::xtra, put);
          continue;                          // loop again
        }
      else
        {
          put << " : good cast 2";
          s_logger->putx(logga::xtra, put);

          std::string currentKey = socket->d_myKey;

          if ( socketKey == currentKey )     // simple string match
            {
              put << " : key match worked (" << socketKey << ")";
              s_logger->putx(logga::xtra, put);
              if ( socket->isConnected() )   // most probably a model error
                {
                  s_logger->repx(logga::warn, "connection already bound", s->d_partnerId);
                  return empty;              // return empty pointer
                }
              put << " : caller should connect" << "\n";
              s_logger->putx(logga::xtra, put);
              return socket;                 // key found, return good pointer
            }
          else
            {
              put << " : key match failed ("
                  << "socket key = '"  << socketKey  << "'" << ", "
                  << "current key = '" << currentKey << "'"
                  << ")" << "\n";
            }
        }                                    // fall thru, key not found
    } // BOOST_FOREACH

  s_logger->putx(logga::xtra, put);
  s_logger->repx(logga::warn, "socket key not found and/or cast", socketKey);
  put << "  socket key (repeated) : " << socketKey << "\n"; // repeat as 'repx' truncates
  s_logger->putx(logga::dbug, put);
  s_logger->addSmartBlank(logga::dbug);
  return empty;                              // return empty pointer
  }

// ---------------------------------------------------------
//  MEMBER FUNCTION : makeConnection <> (static)
// ---------------------------------------------------------
//  Description  : bind the extensity
//  Role         : the final step in the connect chain
//  Techniques   : static, copy assign one pointer to the other
//  Status       : complete
// ---------------------------------------------------------
//
//  Design notes
//
//      integrity checks : connectedness bools, commodity ids
//      extra functions  : link commodity
//      main function    : bind extensity
//      housekeeping     : update necessary details
//
// ---------------------------------------------------------

template <typename C>                        // remember specialization can be employed
bool
Interface::makeConnection
(shared_ptr<Cable<C>  >& cable,
 shared_ptr<Socket<C> >& socket)
{
  // initial reporting
  s_logger->repx(logga::adhc, "entering static member function", "");
  std::ostringstream put;
  put << "entered function 'makeConnection<"
      << xeona::trimLeadingDigits(typeid(C).name()) << ">'" << "\n";
  s_logger->putx(logga::xtra, put);

  // check the connectedness bools (which should be fine in any case)
  if ( cable->d_connected || socket->d_connected )
    {
      s_logger->repx(logga::warn, "abandoning task, prior connections", "");
      std::ostringstream put;
      put << "  connectivity status (probable model problem)"    << "\n"
          << "    cable key           : " << cable->d_myKey      << "\n"
          << "    socket key          : " << socket->d_myKey     << "\n"
          << "    cable connectivity  : " << cable->d_connected  << "\n"
          << "    socket connectivity : " << socket->d_connected << "\n";
      s_logger->putx(logga::dbug, put);
      return false;
    }

  // check the commodity ids (which could well differ)
  std::string cableComId  =  cable->d_commodityId;
  std::string socketComId = socket->d_commodityId;
  if ( cableComId != socketComId )           // simple string match
    {
      s_logger->repx(logga::warn, "abandoning task, commodity mismatch", "");
      std::ostringstream put;
      put << "  commodity ids do not match (model problem)"      << "\n"
          << "    cable key           : " << cable->d_myKey      << "\n"
          << "    socket key          : " << socket->d_myKey     << "\n"
          << "    cable commodity id  : " << cableComId          << "\n"
          << "    socket commodity id : " << socketComId         << "\n";
      s_logger->putx(logga::dbug, put);
      return false;
    }

  // link in by recovering the pointers
  cable->d_commodity  = static_pointer_cast<Commodity>(Entity::retSharedPtr(cableComId));
  socket->d_commodity = static_pointer_cast<Commodity>(Entity::retSharedPtr(socketComId));

  // complete the identity information
  socket->d_partnerId  = cable->d_hostId ;   // update socket information
  socket->d_counterKey = cable->d_myKey ;    // update socket information

  s_logger->repx(logga::dbug, "about to bind extensity and OSP", "");

  // bind the extensity and the optimization sub-problem
  cable->d_flow = socket->d_flow;            // KAZAAP!
  cable->d_cnn  = socket->d_cnn;             // WOOMPF!

  // reporting
  put << "  socket key                            : " << socket->d_myKey << "\n"
      << "  socket connection OSP pointer (d_cnn) : " << socket->d_cnn   << "\n"
      << "  socket commodity                      : " << socketComId     << "\n"
      << "  cable key                             : " <<  cable->d_myKey << "\n"
      << "  cable connection OSP pointer (d_cnn)  : " <<  cable->d_cnn   << "\n"
      << "  cable commodity                       : " <<  cableComId     << "\n";
  s_logger->repx(logga::adhc, "low priority reporting follows", "");
  s_logger->putx(logga::adhc, put);

  // housekeeping
  cable->setConnected();
  socket->setConnected();
  ++s_connectionCount;

  put << " : connection complete" << "\n";
  s_logger->putx(logga::xtra, put);

  s_logger->repx(logga::adhc, "leaving member function", "success");
  return true;
}

// FUNCTIONS PROVIDED FOR UNIT TESTING

// ---------------------------------------------------------
//  MEMBER FUNCTION : getMyKey
// ---------------------------------------------------------

std::string
Interface::getMyKey()
{
  // initial reporting
  s_logger->repx(logga::adhc, "entering member function", "");

  return d_myKey;                          // always known
}

// ---------------------------------------------------------
//  MEMBER FUNCTION : isConnected
// ---------------------------------------------------------

bool
Interface::isConnected()
{
  // CAUTION: initial reporting omitted because it interferes
  // with the cast reporting

  return d_connected;
}

// ---------------------------------------------------------
//  MEMBER FUNCTION : reset (static)
// ---------------------------------------------------------

void
Interface::reset()
{
  // initial reporting
  s_logger->repx(logga::adhc, "entering static member function", "");

  s_logger->repx(logga::dbug, "zeroing 'Interface' static data", "");

  s_censusCables.clear();
  s_censusSockets.clear();

  s_interfaceCount      = 0;
  s_connectionCount     = 0;
  s_connectAllCallCount = 0;
}

// ---------------------------------------------------------
//  MEMBER FUNCTION : getInterfaceCount (static)
// ---------------------------------------------------------

int
Interface::getInterfaceCount()
{
  // initial reporting
  s_logger->repx(logga::adhc, "entering static member function", "");

  return s_interfaceCount;
}

// ---------------------------------------------------------
//  MEMBER FUNCTION : getConnectionCount (static)
// ---------------------------------------------------------

int
Interface::getConnectionCount()
{
  // initial reporting
  s_logger->repx(logga::adhc, "entering static member function", "");

  return s_connectionCount;
}

// ---------------------------------------------------------
//  MEMBER FUNCTION : isComplete (static)
// ---------------------------------------------------------

bool
Interface::isComplete()
{
  // initial reporting
  s_logger->repx(logga::adhc, "entering static member function", "");

  return ! (s_interfaceCount - 2 * s_connectionCount);
}

// UTILITY FUNCTIONS

// ---------------------------------------------------------
//  MEMBER FUNCTION : setConnected
// ---------------------------------------------------------

void
Interface::setConnected()
{
  // initial reporting
  s_logger->repx(logga::adhc, "entering member function", "");

  d_connected = true;
}

// ---------------------------------------------------------
//  CLASS           : Cable <>
//  CLASS           : Socket <>
// ---------------------------------------------------------
//  Description  : a block-to-block interface is a key 'xeona' abstraction
//  Role         : to provide type-safe connectivity while keeping data overheads low
//  Techniques   : restricted templates, static factory function, no data members
//  Status       : complete
// ---------------------------------------------------------
//
//  Design notes
//
//      First up, the concept and orientation of "demand flow"
//      needs to be stated.  Demand flow refers to the commodity
//      requests that are passed, by definition, from cable to
//      socket.  Negative demand flow is NOT SUPPORTED -- in
//      which case, bidirectional blocks need special treatment
//      (as discussed shortly).  'xeona' currently supports [kg],
//      [J], and [UOA] (units-of-account) as characterizing
//      extensities.  By convention, demand flow is sketched
//      right-to-left.
//
//      For completeness, "commodity flow", as represented by a
//      simultaneous reduction and increase in characterizing
//      extensity, is often, but NOT NECESSARILY, anti-parallel
//      to demand flow.  For instance, cooling services
//      characterized with enthalpy flow parallel.  In contrast,
//      exergy flow is anti-parallel in most, but NOT ALL, cases.
//
//      Some blocks, for instance, line HV transmission assets,
//      are bidirectional and need to be assigned an arbitrary
//      demand flow direction.  Bidirectional assets are handled
//      with a single interfaces to each node, but with negative
//      flow enabled.
//
//      The characterizing extensity need not be literal -- in
//      the sense that carbon certificates are naturally
//      quantified in [kg] although no mass transport takes place.
//
//      Socket and cable containing blocks automatically create
//      interface objects upon their own construction.
//
//      The static function 'Interface::connectAll' then
//      completes the connections.
//
//      The set of possible template instantiations is restricted
//      by the list provided in the implementation file.
//
//  Connection process (undertaken by the 'Interface' class)
//
//      Two interfaces are connected as shown.  Starting with:
//
//          Host<>-->Socket<>-->Commodity<--<>Cable<--<>Host
//
//      where <>--> indicates a UML aggregation "part of"
//                  relationship with navigability restricted
//                  as shown
//
//      At various states, confirm that:
//
//          - the same 'Commodity' id is used
//          - the'C' types match
//          - the connectedness bools are 'false'
//
//      Then set:
//
//          - from cable: d_flow = socket->d_flow
//          - the connectedness bools to 'true'
//
//      From a design perspective, the 'Commodity' contains any
//      intensive information and the 'flow' variable represents
//      the characterizing extensity.
//
//  Smart pointers and the static factory function
//
//      Smart pointers are fantastic but they cannot
//      (unfortunately) be created in constructors.  The
//      solution here is to use a static factory function --
//      as described in the Boost documentation:
//
//          Smart Pointers > Smart Pointer Programming
//          Techniques > Obtaining a shared_ptr (weak_ptr) to
//          this in a constructor
//
//      This material is available at:
//
//          file:///home/robbie/boost-build/boost_1_35_0/libs/
//          smart_ptr/sp_techniques.html#in_constructor
//
//          file:///usr/share/doc/libboost-doc/HTML/libs/
//          smart_ptr/sp_techniques.html#in_constructor
//
//      A quote from the cited documentation (Boost 1.35.0)
//
//          If 'X' is supposed to always live on the heap,
//          and be managed by a shared_ptr, use a static
//          factory function:
//
//              class X
//              {
//              private:
//                X() { ... }
//              public:
//                static
//                shared_ptr<X>
//                create()
//                {
//                  shared_ptr<X> px(new X);
//                  return px;
//                }
//              };
//
//  See also
//
//      The design notes for base class 'Interface'.
//
// ---------------------------------------------------------

// CREATORS

// ---------------------------------------------------------
//  MEMBER FUNCTION : Cable<C>::Cable
// ---------------------------------------------------------

template <typename C>
Cable<C>::Cable                              // template signature "<C>" is required here
(const std::string& hostId,                  // see factory function for explanations
 const std::string& partnerId,
 const std::string& ifQualifier,
 const std::string& commodityId) :
  Interface(hostId, partnerId, ifQualifier, commodityId)
{
  std::string typestr = xeona::trimLeadingDigits(typeid(C).name());
  std::ostringstream put;
  put << "  cable constructor call"                         << "\n"
      << "    class         : Cable<"   << typestr   << ">" << "\n"
      << "    host id       : "         << hostId           << "\n"
      << "    partner id    : "         << partnerId        << "\n"
      << "    qualifier     : "         << ifQualifier      << "\n"
      << "    commodity id  : "         << commodityId      << "\n";
  s_logger->putx(logga::xtra, put);

  d_myKey = d_hostId;
  if ( ! d_ifQualifier.empty() ) d_myKey += "." + d_ifQualifier;
  d_counterKey = d_partnerId;
  if ( ! d_ifQualifier.empty() ) d_counterKey += "." + d_ifQualifier;
}

// ---------------------------------------------------------
//  MEMBER FUNCTION : Cable<C>::create (static)
// ---------------------------------------------------------

template <typename C>
shared_ptr<Cable<C> >
Cable<C>::create                             // sub-id form (added later)
(const std::string& hostId,                  // me
 const std::string& targetId_ifQualifier,    // form with dot-separated socket qualifer
 const std::string& commodityId)             // associated commodity
{
  // split the "sub-id"
  std::string              targetId    = "";
  std::string              ifQualifier = "";
  std::vector<std::string> buffer;
  boost::split(buffer,
               targetId_ifQualifier,
               boost::is_any_of("."),
               boost::token_compress_off);   // every two separators delimit a token

  // now load or complain
  const int buffersize = buffer.size();
  std::string first = "";
  switch ( buffersize )
    {
    case 3:                                  // quite likely with a leading "entity."
      first       = buffer.at(0);
      s_logger->repx(logga::warn, "three dot value (fix)", targetId_ifQualifier);
      break;
    case 2:                                  // quite likely success
      first       = buffer.at(0);
      targetId    = buffer.at(0);            // entity identifier
      ifQualifier = buffer.at(1);            // sub-entity label
      s_logger->repx(logga::adhc, "well-formed target-id.label", targetId_ifQualifier);
      break;
    case 1:
      first       = buffer.at(0);
      if ( first.empty() )                   // empty string given
        {
          s_logger->repx(logga::warn, "empty string (fix)", targetId_ifQualifier);
        }
      else                                   // quite likely no label part
        {
          s_logger->repx(logga::warn, "no dot (check for label)", targetId_ifQualifier);
        }
      break;
    case 0:                                  // no value given (may never get here?)
      s_logger->repx(logga::warn, "empty string (fix)", targetId_ifQualifier);
      break;
    default:
      s_logger->repx(logga::warn, "multi-dot value (fix)", targetId_ifQualifier);
      break;
    }
  // check 'targetId'
  if ( first == "entity" )
    {
      s_logger->repx(logga::warn, "leading \"entity.\" present (fix)", first);
    }

  // continue as per original (four argument) code
  shared_ptr<Cable<C> > pi(new Cable<C>(hostId, targetId, ifQualifier, commodityId));
  s_censusCables.push_back(weak_ptr<Cable<C> >(pi));
  ++s_interfaceCount;
  return pi;                                 // return the shared pointer
}

// ---------------------------------------------------------
//  MEMBER FUNCTION : Socket<C>::Socket
// ---------------------------------------------------------

template <typename C>                        // specializations can also be defined
Socket<C>::Socket                            // template signature "<C>" is required here
(const std::string& hostId,                  // see factory function for explanations
 const std::string& partnerId,
 const std::string& ifQualifier,
 const std::string& commodityId) :
  Interface(hostId, partnerId, ifQualifier, commodityId)
{
  std::string typestr = xeona::trimLeadingDigits(typeid(C).name());
  std::ostringstream put;
  put << "  socket constructor call"                        << "\n"
      << "    class         : Socket<"  << typestr   << ">" << "\n"
      << "    host id       : "         << hostId           << "\n"
      << "    partner id    : "         << partnerId        << "\n"
      << "    qualifier     : "         << ifQualifier      << "\n"
      << "    commodity id  : "         << commodityId      << "\n";
  s_logger->putx(logga::xtra, put);

  d_myKey = d_hostId;
  if ( ! d_ifQualifier.empty() ) d_myKey += "." + d_ifQualifier;
}

// ---------------------------------------------------------
//  MEMBER FUNCTION : Socket<C>::create (static)
// ---------------------------------------------------------

template <typename C>
shared_ptr<Socket<C> >
Socket<C>::create
(const std::string& hostId,                  // my id
 const std::string& ifQualifier,             // my socket qualifier
 const std::string& commodityId)             // associated commodity
{
  shared_ptr<Socket<C> > pi(new Socket<C>(hostId, "", ifQualifier, commodityId));
  s_censusSockets.push_back(weak_ptr<Socket<C> >(pi));
  ++s_interfaceCount;
  return pi;                                 // return the shared pointer
}

// ---------------------------------------------------------
//  EXPLICIT TEMPLATE INSTANTIATIONS
// ---------------------------------------------------------

// CAUTION: explicit template instantiations must be placed at
// the very end of the implementation file
//
// For more information, see the excellent documentation in
// 'recset.cc' regarding explicit template instantiations.

// Commodity typedefs are used for convenience

// ---------------------------------
//  core instantiations
// ---------------------------------

typedef CmOxidize         Oxid;
typedef CmCarbonCert      Cert;
typedef CmCarbonSeq       Cseq;
typedef CmElectricity     Elec;
typedef CmWork            Work;
typedef CmHeat            Heat;
typedef CmThermalFluid    Thrm;
typedef CmFunds           Fund;
typedef CmProductiveLand  Land;

template Cable<Oxid>::Cable
  (const std::string&, const std::string&, const std::string&, const std::string&);
template Cable<Cseq>::Cable
  (const std::string&, const std::string&, const std::string&, const std::string&);
template Cable<Cert>::Cable
  (const std::string&, const std::string&, const std::string&, const std::string&);
template Cable<Elec>::Cable
  (const std::string&, const std::string&, const std::string&, const std::string&);
template Cable<Work>::Cable
  (const std::string&, const std::string&, const std::string&, const std::string&);
template Cable<Heat>::Cable
  (const std::string&, const std::string&, const std::string&, const std::string&);
template Cable<Thrm>::Cable
  (const std::string&, const std::string&, const std::string&, const std::string&);
template Cable<Fund>::Cable
  (const std::string&, const std::string&, const std::string&, const std::string&);
template Cable<Land>::Cable
  (const std::string&, const std::string&, const std::string&, const std::string&);

template shared_ptr<Cable<Oxid> > Cable<Oxid>::create
  (const std::string&, const std::string&, const std::string&);
template shared_ptr<Cable<Cseq> > Cable<Cseq>::create
  (const std::string&, const std::string&, const std::string&);
template shared_ptr<Cable<Cert> > Cable<Cert>::create
  (const std::string&, const std::string&, const std::string&);
template shared_ptr<Cable<Elec> > Cable<Elec>::create
  (const std::string&, const std::string&, const std::string&);
template shared_ptr<Cable<Work> > Cable<Work>::create
  (const std::string&, const std::string&, const std::string&);
template shared_ptr<Cable<Heat> > Cable<Heat>::create
  (const std::string&, const std::string&, const std::string&);
template shared_ptr<Cable<Thrm> > Cable<Thrm>::create
  (const std::string&, const std::string&, const std::string&);
template shared_ptr<Cable<Fund> > Cable<Fund>::create
  (const std::string&, const std::string&, const std::string&);
template shared_ptr<Cable<Land> > Cable<Land>::create
  (const std::string&, const std::string&, const std::string&);

template Socket<Oxid>::Socket
  (const std::string&, const std::string&, const std::string&, const std::string&);
template Socket<Cseq>::Socket
  (const std::string&, const std::string&, const std::string&, const std::string&);
template Socket<Cert>::Socket
  (const std::string&, const std::string&, const std::string&, const std::string&);
template Socket<Elec>::Socket
  (const std::string&, const std::string&, const std::string&, const std::string&);
template Socket<Work>::Socket
  (const std::string&, const std::string&, const std::string&, const std::string&);
template Socket<Heat>::Socket
  (const std::string&, const std::string&, const std::string&, const std::string&);
template Socket<Thrm>::Socket
  (const std::string&, const std::string&, const std::string&, const std::string&);
template Socket<Fund>::Socket
  (const std::string&, const std::string&, const std::string&, const std::string&);
template Socket<Land>::Socket
  (const std::string&, const std::string&, const std::string&, const std::string&);

template shared_ptr<Socket<Oxid> > Socket<Oxid>::create
  (const std::string&, const std::string&, const std::string&);
template shared_ptr<Socket<Cseq> > Socket<Cseq>::create
  (const std::string&, const std::string&, const std::string&);
template shared_ptr<Socket<Cert> > Socket<Cert>::create
  (const std::string&, const std::string&, const std::string&);
template shared_ptr<Socket<Elec> > Socket<Elec>::create
  (const std::string&, const std::string&, const std::string&);
template shared_ptr<Socket<Work> > Socket<Work>::create
  (const std::string&, const std::string&, const std::string&);
template shared_ptr<Socket<Heat> > Socket<Heat>::create
  (const std::string&, const std::string&, const std::string&);
template shared_ptr<Socket<Thrm> > Socket<Thrm>::create
  (const std::string&, const std::string&, const std::string&);
template shared_ptr<Socket<Fund> > Socket<Fund>::create
  (const std::string&, const std::string&, const std::string&);
template shared_ptr<Socket<Land> > Socket<Land>::create
  (const std::string&, const std::string&, const std::string&);

// ---------------------------------
//  derived instantiations
// ---------------------------------

typedef CmOxidGas         OGas;
typedef CmOxidNaturalGas  NatG;
typedef CmOxidBiocoal     BioC;
typedef CmOxidHydrogen    Htwo;

template Cable<OGas>::Cable
  (const std::string&, const std::string&, const std::string&, const std::string&);
template Cable<NatG>::Cable
  (const std::string&, const std::string&, const std::string&, const std::string&);
template Cable<BioC>::Cable
  (const std::string&, const std::string&, const std::string&, const std::string&);
template Cable<Htwo>::Cable
  (const std::string&, const std::string&, const std::string&, const std::string&);

template shared_ptr<Cable<OGas> > Cable<OGas>::create
  (const std::string&, const std::string&, const std::string&);
template shared_ptr<Cable<NatG> > Cable<NatG>::create
  (const std::string&, const std::string&, const std::string&);
template shared_ptr<Cable<BioC> > Cable<BioC>::create
  (const std::string&, const std::string&, const std::string&);
template shared_ptr<Cable<Htwo> > Cable<Htwo>::create
  (const std::string&, const std::string&, const std::string&);

template Socket<OGas>::Socket
  (const std::string&, const std::string&, const std::string&, const std::string&);
template Socket<NatG>::Socket
  (const std::string&, const std::string&, const std::string&, const std::string&);
template Socket<BioC>::Socket
  (const std::string&, const std::string&, const std::string&, const std::string&);
template Socket<Htwo>::Socket
  (const std::string&, const std::string&, const std::string&, const std::string&);

template shared_ptr<Socket<OGas> > Socket<OGas>::create
  (const std::string&, const std::string&, const std::string&);
template shared_ptr<Socket<NatG> > Socket<NatG>::create
  (const std::string&, const std::string&, const std::string&);
template shared_ptr<Socket<BioC> > Socket<BioC>::create
  (const std::string&, const std::string&, const std::string&);
template shared_ptr<Socket<Htwo> > Socket<Htwo>::create
  (const std::string&, const std::string&, const std::string&);

//  end of file

