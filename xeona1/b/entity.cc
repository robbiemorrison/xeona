//  XEONA ENERGY SYSTEMS MODELING ENVIRONMENT
//
//  file-create-name : entity.cc
//  file-create-date : Fri 15-Jun-2007 08:52 UTC
//  file-initiator   : Robbie Morrison <robbie@actrix.co.nz>
//
//  file-role        : entity base class plus lazy linking / implementation
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
//  $URL: file:///home/robbie/svn-root/xeona/futz/trunk/xeona1/b/entity.cc $

//  LOCAL AND SYSTEM INCLUDES

#include "entity.h"           // companion header for this file (place first)

#ifndef _XUTEST  // this greatly simplifies unit testing dependencies
# include "../b/asop.h"       // asset operator entity
# include "../b/domcon.h"     // domain controller entity
# include "../b/gate.h"       // gateway entity
# include "../b/junc.h"       // demand split/join junction entity
# include "../b/node.h"       // LMP node entity
# include "../b/overseer.h"   // top-level overseer entity (singleton)
# include "../b/teas.h"       // technical asset entity
#else
  class AssetOperator    { }; // hollow definition
  class DemandJunction   { }; // hollow definition
  class DomainController { }; // hollow definition
  class Gateway          { }; // hollow definition
  class Overseer         { }; // hollow definition
  class TechnicalAsset   { }; // hollow definition
  class LmpNode          { }; // hollow definition
#endif // _XUTEST

#include "../c/util2.h"       // free functions which offer general utilities 2
#include "../c/recset.h"      // record set support
#include "../c/factory.h"     // entity factory

#include "../a/logger.h"      // standard logging functionality (as required)
#include ".././common.h"      // common definitions for project (place last)

#include <algorithm>          // STL copying, searching, and sorting
#include <iterator>           // STL additional iterators, std::distance()
#include <sstream>            // string-streams
#include <typeinfo>           // run-time type information (RTTI)

#include <cctype>             // C-style char classification, case conversion

#include <boost/algorithm/string.hpp>   // string recasing, trimming, splitting
#include <boost/foreach.hpp>            // BOOST_FOREACH iteration macro

//  CODE

// ---------------------------------------------------------
//  CLASS           : Entity
// ---------------------------------------------------------
//  Description  : abstract base class for all entities
//  Role         : provide basic housekeeping and class-wide support
//  Techniques   : static censuses, shared pointers, RTTI (run-time type identification)
//  Status       : complete
//
//  Design notes
//
//      See header
//
//          Some introductory comments are also given in the
//          header file.
//
//      Raw census
//
//          A list (and not vector) is used for 's_censusRaw' for
//          ease of element removal.  In addition, one of the
//          Boost.Ptr_containers was considered.  But no
//          compelling advantages materialized, given the simple
//          usage here -- no polymorphic behavior, no null
//          pointers, and no small object efficiency
//          considerations.  Boost.Any objects were also
//          considered but again no clear advantages were likely
//          to accrue.
//
//          Unfortunately smart 'this' pointers cannot be used in
//          constructors (as documented in the code proper).
//
//          Stephens etal (2006 pp294-295) provide similar code
//          to that used here except that their destructor
//          searches for its own 'this' entry -- here the
//          'remove(value)' member function is used.
//
//      shared_from_this()
//
//          During development, this class variously used
//          'shared_from_this' and explicitly processed
//          'shared_ptr's from the entity factory.  Currently,
//          the shared pointers derive from calls to the
//          '::createType' function template.
//
//          The Boost documentation states: "the
//          shared_from_this() member function (actually there
//          are two, depending on constness) REQUIRES: .. there
//          must exist at least one shared_ptr instance .. that
//          owns .. [the underlying object]"
//
//          see: file:///usr/share/doc/libboost-doc/HTML/libs/
//          smart_ptr/enable_shared_from_this.html
//
//      Censuses
//
//          The primary census container 's_censusFull' contains
//          the canonical shared pointers.  The container
//          's_censusLink' contains "link" entity shared pointers
//          -- which will, in due course, be redirected to "full"
//          entities as a result of the 'linkAll' call.
//
//          The raw pointer census 's_censusRaw' is provided only
//          for development purposes.
//
//      Stream insertion
//
//          The 'xeona' object factory returns a (std::tr1:: or
//          boost::) shared_ptr<Entity> and it would be
//          convenient to correctly stream sub-entities without
//          additional code:
//
//              std::ostream << *subent;
//
//          The usual free function granted friendship approach
//          (see, for example, Stephens etal 2006 pp363-366) does
//          not allow for polymorphic behavior.
//
//          The helper variant used here is from Dattatri (2002
//          pp356-357 and surrounding text) with my contribution
//          being the addition of 'virtual' to facilitate
//          polymorphic behavior.  Note that most stream
//          insertions will call the free (non-member) overloaded
//          stream insertion operator:
//
//              ostream& operator<< (ostream& os, const Entity& en)
//
//          That said, sub-entities are also given there own
//          dedicated overloaded stream insertion operators.
//
//          Finally, Sutter and Alexandrescu (2005 p79) cover
//          this topic in a more general sense, stating: "add a
//          virtual member function to provide the virtual
//          behavior, and implement the nonmember [free function]
//          in terms of that [member function]".
//
// ---------------------------------------------------------

//  STATIC DEFINITIONS - with and without explicit initialization

int  Entity::s_horizonSteps     = -1;        // negative number means value not yet set
int  Entity::s_horizonInterval  = -1;        // negative number means value not yet set
int  Entity::s_horizonStartHour = -1;        // negative number means value not yet set
int  Entity::s_horizonStartDay  = -1;        // negative number means value not yet set
xeona::LeapYear   Entity::s_leapYear   = xeona::e_leapNotSet;
xeona::Hemisphere Entity::s_hemisphere = xeona::e_hemiNotSet;

int Entity::s_linkAllCallCount = 0;

std::list<const Entity*>          Entity::s_censusRaw;
std::vector<shared_ptr<Entity> >  Entity::s_censusFull;
std::vector<assign_ptr<Entity> >  Entity::s_censusLink;
std::vector<std::string>          Entity::s_identsLink;

std::vector<std::string> Entity::s_entityTypesWant;
std::vector<std::string> Entity::s_entityTypesGot;
std::vector<std::string> Entity::s_entityTypesDiff;

LinkLogger      Entity::s_linkLogger;
logga::spLogger Entity::s_logger = logga::ptrLogStream();

Record      Entity::s_recordStandIn;         // zero-argument constructed 'Record'
std::string Entity::s_stringEmpty = "";
double      Entity::s_doubleZero  = 0.0;
int         Entity::s_intZero     = 0;
bool        Entity::s_boolFalse   = false;

// CREATORS

// ---------------------------------------------------------
//  MEMBER FUNCTION : Entity
// ---------------------------------------------------------
//  Description  : constructor
//  Status       : complete
// ---------------------------------------------------------

Entity::Entity
(const std::string entityId,                 // enforced unique identifier
 Record&           record) :                 // CAUTION: pass-by-ref necessary
   d_identifier(entityId),
   d_record(record),
   d_typeStr()
{
  s_logger->repx(logga::dbug, "constructor call", d_identifier);

  const int idLength  = d_identifier.length();
  const int maxLength = 32;
  if (  idLength > maxLength )
    {
      std::ostringstream oss1;
      oss1 << "entity identifier exceeds " << maxLength << " chars";
      std::ostringstream oss2;
      oss2 << d_identifier << " " << idLength;
      s_logger->repx(logga::rankJumpy, oss1.str(), oss2.str());
    }

  // CAUTION: Becker (2007 p58 footnote 5) says NOT to use
  // shared_from_this() -- which provides support for shared
  // pointer 'this' pointers -- in constructor definitions.

  Entity::s_censusRaw.push_back(this);       // insert copy of pointer in raw census
}

// ---------------------------------------------------------
//  MEMBER FUNCTION : ~Entity
// ---------------------------------------------------------
//  Description  : destructor
//  Techniques   : virtual, protected
//  Status       : complete
// ---------------------------------------------------------

Entity::~Entity()
{
  s_logger->repx(logga::xtra, "destructor call", d_identifier);

  // Remove copy of 'this' pointer from the instance census
  // 's_censusRaw'.  Aggregate (potentially multiple identical)
  // element removal is straightforward with with a list but not
  // a vector -- see Stephens etal (2006 p230) for more
  // information (whilst noting that 'this' should be, by
  // definition, unique!).
  //
  // As a digression, Lischner (2003 p353) offers alternative
  // code for vectors, but data.erase() (rather than his typo
  // std::erase) is correct as far as I can tell.  Finally,
  // Josuttis (1999 p378) discusses the issue.

  Entity::s_censusRaw.remove(this);     // 'remove' is 'std::list' but not 'std::vector'
}

// ---------------------------------------------------------
//  MEMBER FUNCTION : factoryInitialize
// ---------------------------------------------------------
//  Description  : post-construction initialization by factory call
//  Role         : to support the set-up of entities
//  Caller       : one of the 'EntityFactory' '::createType<>' functions
//  Status       : complete
// ---------------------------------------------------------

void
Entity::factoryInitialize()
{
  s_logger->repx(logga::warn, "Entity instance initialize", "base class form");
}

// ACCESSORS

// ---------------------------------------------------------
//  MEMBER FUNCTION : getIdentifier
// ---------------------------------------------------------
//  Description  : returns unique entity identifier
//  Role         : accessor
//  Status       : complete
// ---------------------------------------------------------

std::string
Entity::getIdentifier() const
{
  return d_identifier;
}

// ---------------------------------------------------------
//  MEMBER FUNCTION : getIdAndKind
// ---------------------------------------------------------
//  Description  : returns unique entity identifier and if "link" entity
//  Role         : accessor
//  Status       : complete
// ---------------------------------------------------------

std::string
Entity::getIdAndKind                         // returns d_identifier and if link entity
(const std::string& auxMsg) const            // optional auxillary message, note default
{
  std::string buffer = d_identifier;
  if ( getTypeStr().empty() ) buffer += " (link)";   // proxy for link entity
  else                        buffer += "";          // could also be " (full)"
  if ( ! auxMsg.empty() )     buffer += " [" + auxMsg + "]";
  return buffer;
}

// ---------------------------------------------------------
//  MEMBER FUNCTION : getTypeStr
// ---------------------------------------------------------
//  Description  : returns RTTI (run-time type information) type string
//  Role         : accessor
//  Status       : complete
// ---------------------------------------------------------

std::string
Entity::getTypeStr() const
{
  return d_typeStr;
}

// ---------------------------------------------------------
//  MEMBER FUNCTION : getUseCount
// ---------------------------------------------------------
//  Description  : return number of shared pointer in existence including the census copy
//  Role         : accessor
//  On fail      : returns 0
//  Techniques   : 'shared_ptr::use_count'
//  Status       : complete
// ---------------------------------------------------------

int
Entity::getUseCount() const                  // includes copy in census
{
  const shared_ptr<Entity> me = retConstMe();
  if ( ! me )                                // 'retConstSharedPtr' returns empty on fail
    {
      s_logger->repx(logga::warn, "could not obtain my shared pointer", d_identifier);
      return 0;
    }
  const int useCount = me.use_count() - 1;   // minus one for 'me'
  return useCount;
}                                            // 'me' goes out of scope

// ---------------------------------------------------------
//  MEMBER FUNCTION : report
// ---------------------------------------------------------
//  Description  : prepare a report
//  Role         : debugging
//  Caller       : various
//  Techniques   : 'typeid' 'getIdentifier'
//  Status       : complete
// ---------------------------------------------------------

std::string
Entity::report
(const std::string msg) const                // note default
{
  s_logger->repx(logga::adhc, "entering member function", msg);

  const std::string etype = xeona::demangle(typeid(*this).name());
  std::ostringstream oss;
  oss << "  entity report (this-based):"                    << "\n"
      << "    caller message : " << msg                     << "\n"
      << "    address        : " << this                    << "\n"
      << "    id             : " << this->getIdentifier()   << "\n"
      << "    type construct : " << this->getTypeStr()      << "\n"
      << "    type current   : " << etype                   << "\n";
  return oss.str();
}

// STATIC MANIPULATORS -- for the "lazy" linking process

// ---------------------------------------------------------
//  MEMBER FUNCTION : linkAll (static)
// ---------------------------------------------------------
//  Description  : traverses and remaps referenced "link" pointers to "full" entities
//  Role         : final step in the entity linking process
//  Robustness   : repeat calls should not cause a problems
//  Techniques   : references for shared pointers (for the link holding entity)
//  Status       : complete
//
//  Design notes
//
//      An entity holding a link to another entity INITIALLY does
//      so by reference to a shared pointer containing a "link"
//      entity with the required identifier string.  The "link"
//      entities are duly placed in a "link" census.
//
//      This function simply remaps the "link" shared pointer to
//      its proper "full" entity.
//
//      The "link" census 'std::vector' needs to remain intact
//      after this function call because its shared pointer
//      contents are still required by the requiring-links
//      entities. (I had originally thought that 'clearing' the
//      "link" census after this remapping would be correct).
//
//      In any case, the "link" entities are duly deleted and
//      release their memory (not that this would amount to
//      much).
//
//      Regarding repeat calls, this function should be
//      dynamic-safe in the sense that new entities could be
//      constructed and linked -- that said, entity removal is
//      more problematic and is unlikely to be supported in the
//      near future.
//
// ---------------------------------------------------------

bool
Entity::linkAll()
{
  // initial reporting, also update number of calls
  ++s_linkAllCallCount;
  s_logger->repx(logga::dbug, "entering member function, call cnt", s_linkAllCallCount);

  // reset the link logger
  s_linkLogger.reset();                      // class 'LinkLogger'

  // traverse link census
  BOOST_FOREACH( assign_ptr<Entity> link, s_censusLink )
    {
      // do the linking, then log success or count failures
      const bool okay = link->polylink(link);
      s_linkLogger.insert(link, okay);
    }

  // grab some counts
  const int links = s_linkLogger.getLogCount();
  const int fails = s_linkLogger.getNullCount();

  // report the outcome
  s_logger->repx(logga::info, "link (informational) success count", links);
  if ( fails > 0 ) s_logger->repx(logga::warn, "link fail count", fails);
  else             s_logger->repx(logga::xtra, "link fail count", fails);
  std::ostringstream put;
  put << s_linkLogger.recover();
  s_logger->putx(logga::dbug, put);

  // return status
  if ( fails > 0 ) return false;
  else             return true;
}

// STATIC ACCESSORS

// ---------------------------------------------------------
//  MEMBER FUNCTION : getFullPopn (static)
// ---------------------------------------------------------
//  Description  : returns current population
//  Role         : static accessor for test purposes
//  Techniques   : 'std::vector::size' member function
//  Status       : complete
// ---------------------------------------------------------

int
Entity::getFullPopn()
{
  return s_censusFull.size();                // just the genuine entities
}

// ---------------------------------------------------------
//  MEMBER FUNCTION : getHorizonSteps (static)
// ---------------------------------------------------------
//  Description  : returns current simulation horizon steps
//  Role         : static accessor
//  On fail      : returns 0
//  Status       : complete
// ---------------------------------------------------------

int
Entity::getHorizonSteps()
{
  if ( s_horizonSteps < 0 )
    {
      s_logger->repx(logga::warn, "horizon steps not initialized", s_horizonSteps);
      return 0;
    }
  return s_horizonSteps;
}

// ---------------------------------------------------------
//  MEMBER FUNCTION : getHorizonInterval (static)
// ---------------------------------------------------------
//  Description  : returns current simulation horizon interval length
//  Role         : static accessor
//  On fail      : returns 0
//  Status       : complete
// ---------------------------------------------------------

int
Entity::getHorizonInterval()
{
  if ( s_horizonInterval < 0 )
    {
      s_logger->repx(logga::warn, "horizon interval not initialized", s_horizonInterval);
      return 0;
    }
  return s_horizonInterval;
}

// ---------------------------------------------------------
//  MEMBER FUNCTION : getHorizonIntsPerDay (static)
// ---------------------------------------------------------
//  Description  : returns current intervals-per-day, ranging 1 thru 288
//  Role         : static accessor
//  On fail      : returns 0
//  Status       : complete
// ---------------------------------------------------------

int
Entity::getHorizonIntsPerDay()
{
  if ( s_horizonInterval < 0 )
    {
      s_logger->repx(logga::warn, "horizon interval not initialized", s_horizonInterval);
      return 0;
    }

  // active code
  const double ipd = (24.0 * 3600.0) / static_cast<double>(s_horizonInterval);
  if ( ! xeona::isIntegerValued(ipd) )       // confirmation test
    {
      s_logger->repx(logga::warn, "intervals-per-day not integral", ipd);
      return 0;
    }
  return static_cast<int>(ipd);
}

// ---------------------------------------------------------
//  MEMBER FUNCTION : getHorizonStartHour (static)
// ---------------------------------------------------------
//  Description  : returns current simulation horizon start hour
//  Role         : static accessor
//  On fail      : returns 0
//  Status       : complete
// ---------------------------------------------------------

int
Entity::getHorizonStartHour()
{
  if ( s_horizonStartHour < 0 )
    {
      s_logger->repx(logga::warn, "horizon start hour uninitialized", s_horizonStartHour);
      return 0;
    }
  return s_horizonStartHour;
}

// ---------------------------------------------------------
//  MEMBER FUNCTION : getHorizonStartDay (static)
// ---------------------------------------------------------
//  Description  : returns current simulation horizon start day
//  Role         : static accessor
//  On fail      : returns 0
//  Status       : complete
// ---------------------------------------------------------

int
Entity::getHorizonStartDay()
{
  if ( s_horizonStartDay < 0 )
    {
      s_logger->repx(logga::warn, "horizon start day not initialized", s_horizonStartDay);
      return 0;
    }
  return s_horizonStartDay;
}

// ---------------------------------------------------------
//  MEMBER FUNCTION : getHorizonOffset (static)
// ---------------------------------------------------------
//  Description  : returns current simulation offset
//  Role         : static accessor
//  On fail      : returns 0
//  Status       : complete
// ---------------------------------------------------------

int
Entity::getHorizonOffset()
{
  if ( s_horizonStartDay < 0 || s_horizonStartHour < 0 || s_horizonInterval < 0 )
    {
      s_logger->repx(logga::warn, "horizon start conditions uninitialized", "");
      return 0;
    }

  // active code
  const double adjustment = 3600.0 / static_cast<double>(s_horizonInterval);
  const double hours      = 24 * (s_horizonStartDay - 1) + s_horizonStartHour;
  const double offset     = adjustment * hours;
  if ( ! xeona::isIntegerValued(offset) )
    {
      s_logger->repx(logga::warn, "horizon offset not integral", offset);
      return 0;
    }
  return static_cast<int>(offset);
}

// ---------------------------------------------------------
//  MEMBER FUNCTION : getLeapYear (static)
// ---------------------------------------------------------
//  Description  : returns current leap year status
//  Role         : static accessor
//  On fail      : return not useful and warning issued
//  Status       : complete
// ---------------------------------------------------------

bool
Entity::getLeapYear()
{
  if ( s_leapYear == xeona::e_leapNotSet )
    {
      s_logger->repx(logga::warn, "leap year uninitialized", s_leapYear);
      return false;
    }
  switch ( s_leapYear )
    {
    case xeona::e_normal: return false;
    case xeona::e_leap:   return true;
    default:
      s_logger->repx(logga::warn, "coding error, s_leapYear", s_leapYear);
      return false;
    }
}

// ---------------------------------------------------------
//  MEMBER FUNCTION : getHemisphere (static)
// ---------------------------------------------------------
//  Description  : returns current simulation horizon hemisphere
//  Role         : static accessor
//  On fail      : returns xeona::e_notSet (also 0)
//  Status       : complete
// ---------------------------------------------------------

const xeona::Hemisphere
Entity::getHemisphere()
{
  if ( s_hemisphere < xeona::e_hemiNotSet )
    {
      s_logger->repx(logga::warn,"horizon hemisphere uninitialized", s_hemisphere);
      return xeona::e_hemiNotSet;
    }
  return s_hemisphere;
}

// ---------------------------------------------------------
//  MEMBER FUNCTION : confirmIdentifier (static)
// ---------------------------------------------------------
//  Description  : hunts for sought identifier in "full" census
//  Role         : used by 'xeona::simulate'
//  On fail      : returns 'false'
//  Status       : complete
// ---------------------------------------------------------

const bool                                   // 'true' if in "full" census, else 'false'
Entity::confirmIdentifier
(const std::string& soughtId)
{
  BOOST_FOREACH( shared_ptr<Entity> s, s_censusFull )
    {
#ifdef _XUTEST
      std::ostringstream put;
      put << "    id : " << s->d_identifier << "\n";
      s_logger->putx(logga::dbug, put);
#endif // _XUTEST
      if ( s->d_identifier == soughtId )
        return true;
    }
  return false;
}

// ---------------------------------------------------------
//  MEMBER FUNCTION : retOverseer (static)
// ---------------------------------------------------------

// CAUTION: the following "downcast" is needed because 'Entity'
// and 'FullEntity' do not possess 'run' calls and similar --
// note also a shared pointer 'dynamic_pointer_cast' is required
// instead of the more normal raw pointer 'dynamic_cast.'

shared_ptr<Overseer>
Entity::retOverseer()
{
  // 'xeona::overseer' is defined in 'common.cc'
  return dynamic_pointer_cast<Overseer>(retSharedPtr(xeona::overseer));
}

// ---------------------------------------------------------
//  MEMBER FUNCTION : retDomains (static)
// ---------------------------------------------------------

std::vector<shared_ptr<DomainController> >
Entity::retDomains()
{
  std::vector<shared_ptr<DomainController> > buffer;
  BOOST_FOREACH( shared_ptr<Entity> entity, s_censusFull )
    {
      // attempt a downcast
      shared_ptr<DomainController> dom = dynamic_pointer_cast<DomainController>(entity);
      if ( dom != 0 )
        {
          buffer.push_back(dom); // load the downcast version
        }
    }
  return buffer;                             // can be empty
}

// STATIC ACCESSORS FOR TEST PURPOSES

// ---------------------------------------------------------
//  MEMBER FUNCTION : getLinkPopn (static)
// ---------------------------------------------------------
//  Description  : returns current "link" entity population
//  Role         : static accessor for test purposes
//  Techniques   : 'std::vector::size' member function
//  Status       : complete
// ---------------------------------------------------------

int
Entity::getLinkPopn()
{
  return s_censusLink.size();                // the number of (formed or unformed) links
}

// ---------------------------------------------------------
//  MEMBER FUNCTION : getRawPopn (static)
// ---------------------------------------------------------
//  Description  : returns current population
//  Role         : static accessor for test purposes
//  Techniques   : 'std::list::size' member function
//  Status       : complete
// ---------------------------------------------------------

int
Entity::getRawPopn()
{
  return s_censusRaw.size();                 // "full" and "link"(if any) entities
}

// ---------------------------------------------------------
//  MEMBER FUNCTION : getCensusRaw (static)
// ---------------------------------------------------------
//  Description  : returns 'const' reference to 's_censusRaw'
//  Role         : static accessor for test purposes
//  Status       : complete
//
//  CAUTION: on returning references
//
//      See Dattatri (2002 pp109-112) about returning references
//      from functions -- in particular, be careful that the
//      referenced object outlasts the reference holder.
//
// ---------------------------------------------------------

const std::list<const Entity*>&              // return a reference to a 'const' container
Entity::getCensusRaw()
{
  s_logger->repx(logga::dbug, "raw census reference, size", s_censusRaw.size());
  return s_censusRaw;
}

// ---------------------------------------------------------
//  MEMBER FUNCTION : traverseFullPopulation (static)
// ---------------------------------------------------------
//  Description  : returns string with entity identifiers
//  Role         : static accessor for test purposes
//  Techniques   : string-stream
//  Status       : complete
// ---------------------------------------------------------

std::string                                  // returns string for test purposes
Entity::traverseFullPopulation()
{
  std::ostringstream ss;
  ss << "traversal : |";
  BOOST_FOREACH( shared_ptr<Entity> s, s_censusFull )
    {
      ss << " " << s->d_identifier << " |";
    }
  ss << "\n";
  return ss.str();
}

// ---------------------------------------------------------
//  MEMBER FUNCTION : getCensusFull (static)
// ---------------------------------------------------------

const std::vector<shared_ptr<Entity> >&
Entity::getCensusFull()
{
  return s_censusFull;
}

// ---------------------------------------------------------
//  MEMBER FUNCTION : reportFullPopulation (static)
// ---------------------------------------------------------

std::string
Entity::reportFullPopulation()
{
  // initial reporting
  s_logger->repx(logga::adhc, "entering member function", "");

  // active code
  std::ostringstream oss;
  oss << "  no  identifier                      entity               use-count"   << "\n"
      << "  --------------------------------------------------------------------" << "\n";
  int i = 0;
  BOOST_FOREACH( shared_ptr<Entity> e, s_censusFull )
    {
      oss << boost::format("  %02d")  % ++i
          << boost::format("  %-30s") % e->getIdentifier()
          << boost::format("  %-25d") % e->getTypeStr()
          << boost::format("  %2d")   % e.use_count()
          << "\n";
    }

  // return string
  return oss.str();
}

// STATIC MANIPULATORS

// ---------------------------------------------------------
//  MEMBER FUNCTION : setHorizonSteps (static)
// ---------------------------------------------------------
//  Description  : sets simulation horizon steps
//  Role         : static manipulator
//  Techniques   : static data (to count calls), string-stream
//  Status       : complete
// ---------------------------------------------------------

bool
Entity::setHorizonSteps
(const int steps)
{
  bool ret = true;

  static bool firstCall = true;
  if ( firstCall == false )
    {
      s_logger->repx(logga::warn, "illegal reset, firstCall", firstCall);
      ret = false;
    }
  firstCall = false;

  if ( s_horizonSteps != -1 )
    {
      s_logger->repx(logga::warn, "illegal reset, s_horizonSteps", s_horizonSteps);
      ret = false;
    }

  const int was  = s_horizonSteps;           // briefly capture for reporting
  s_horizonSteps = steps;                    // reset static variable

  std::ostringstream oss;
  oss << was << " > " << s_horizonSteps;
  s_logger->repx(logga::xtra, "set s_horizonSteps, was > now", oss.str());

  return ret;
}

// ---------------------------------------------------------
//  MEMBER FUNCTION : setHorizonInterval (static)
// ---------------------------------------------------------

bool
Entity::setHorizonInterval
(const int interval)
{
  bool ret = true;

  static bool firstCall = true;
  if ( firstCall == false )
    {
      s_logger->repx(logga::warn, "illegal reset, firstCall", firstCall);
      ret = false;
    }
  firstCall = false;

  if ( s_horizonInterval != -1 )
    {
      s_logger->repx(logga::warn, "illegal reset, s_horizonInterval", s_horizonInterval);
      ret = false;
    }

  const int was     = s_horizonInterval;     // briefly capture for reporting
  s_horizonInterval = interval;              // reset static variable

  std::ostringstream oss;
  oss << was << " > " << s_horizonInterval;
  s_logger->repx(logga::xtra, "set s_horizonInterval, was > now", oss.str());

  return ret;
}

// ---------------------------------------------------------
//  MEMBER FUNCTION : setHorizonStartHour (static)
// ---------------------------------------------------------

bool
Entity::setHorizonStartHour
(const int startHour)
{
  bool ret = true;

  static bool firstCall = true;
  if ( firstCall == false )
    {
      s_logger->repx(logga::warn, "illegal reset, firstCall", firstCall);
      ret = false;
    }
  firstCall = false;

  if ( s_horizonStartHour != -1 )
    {
      s_logger->repx(logga::warn, "illegal reset, s_horizonStartHour",s_horizonStartHour);
      ret = false;
    }

  const int was      = s_horizonStartHour;   // briefly capture for reporting
  s_horizonStartHour = startHour;            // reset static variable

  std::ostringstream oss;
  oss << was << " > " << s_horizonStartHour;
  s_logger->repx(logga::xtra, "set s_horizonStartHour, was > now", oss.str());

  return ret;
}

// ---------------------------------------------------------
//  MEMBER FUNCTION : setHorizonStartDay (static)
// ---------------------------------------------------------

bool
Entity::setHorizonStartDay
(const int startDay)
{
  bool ret = true;

  static bool firstCall = true;
  if ( firstCall == false )
    {
      s_logger->repx(logga::warn, "illegal reset, firstCall", firstCall);
      ret = false;
    }
  firstCall = false;

  if ( s_horizonStartDay != -1 )
    {
      s_logger->repx(logga::warn, "illegal reset, s_horizonStartDay", s_horizonStartDay);
      ret = false;
    }

  const int was     = s_horizonStartDay;     // briefly capture for reporting
  s_horizonStartDay = startDay;              // reset static variable

  std::ostringstream oss;
  oss << was << " > " << s_horizonStartDay;
  s_logger->repx(logga::xtra, "set s_horizonStartDay, was > now", oss.str());

  return ret;
}

// ---------------------------------------------------------
//  MEMBER FUNCTION : setLeapYear (static)
// ---------------------------------------------------------

bool
Entity::setLeapYear
(const bool leapYear)
{
  bool ret = true;

  static bool firstCall = true;
  if ( firstCall == false )
    {
      s_logger->repx(logga::warn, "illegal reset, firstCall", firstCall);
      ret = false;
    }
  firstCall = false;

  if ( s_leapYear != xeona::e_leapNotSet )
    {
      s_logger->repx(logga::warn, "illegal reset, s_leapYear", s_leapYear);
      ret = false;
    }
  const int was = s_leapYear;                // briefly capture for reporting
  switch ( leapYear )
    {
    case true:  s_leapYear = xeona::e_leap;   break;
    case false: s_leapYear = xeona::e_normal; break;
    }

  std::ostringstream oss;
  oss << was << " > " << s_leapYear;
  s_logger->repx(logga::xtra, "set s_leapYear, was > now", oss.str());

  return ret;
}

// ---------------------------------------------------------
//  MEMBER FUNCTION : setHemisphere (static)
// ---------------------------------------------------------

bool
Entity::setHemisphere
(const xeona::Hemisphere hemisphere)
{
  bool ret = true;

  static bool firstCall = true;
  if ( firstCall == false )
    {
      s_logger->repx(logga::warn, "illegal reset, firstCall", firstCall);
      ret = false;
    }
  firstCall = false;

  if ( s_hemisphere != xeona::e_hemiNotSet )
    {
      s_logger->repx(logga::warn, "illegal reset, s_hemisphere", s_hemisphere);
      ret = false;
    }

  const int was = s_hemisphere;              // briefly capture for reporting
  s_hemisphere  = hemisphere;                // reset static variable

  std::ostringstream oss;
  oss << was << " > " << s_hemisphere;
  s_logger->repx(logga::xtra, "set s_hemisphere, was > now", oss.str());

  return ret;
}

// ---------------------------------------------------------
//  MEMBER FUNCTION : addWant (static)
// ---------------------------------------------------------
//  Description  : add a Want
//  Role         : add an entity to the mandatory list
//  Techniques   : pass-by-ref
//  Status       : complete
// ---------------------------------------------------------

void
Entity::addWant
(const std::string& entityType)
{
  s_logger->repx(logga::dbug, "adding wanted entity type", entityType);
  s_entityTypesWant.push_back(entityType);   // normal STL copy in
}

// ---------------------------------------------------------
//  MEMBER FUNCTION : checkWants (static)
// ---------------------------------------------------------
//  Description  : process Wants and Gots, pass out Diffs list
//  Role         : confirm that all mandatory entities exist
//  Techniques   : pass-by-ref argument, calls 'checkEntityTypesLists'
//  Status       : complete
// ---------------------------------------------------------

void
Entity::checkWants
(std::vector<std::string>& buffer)
{
  checkEntityTypesLists();                             // static utility function
  std::vector<std::string> local(s_entityTypesDiff);   // make copy
  buffer = local;                                      // bind copy
}

// UTILITY FUNCTIONS -- non-public

// ---------------------------------------------------------
//  MEMBER FUNCTION : isUniqueIdentifierFull
// ---------------------------------------------------------
//  Description  : check given "full" sub-entity identifier is unique
//  Role         : integrity checking during construction
//  On success   : return 'true'
//  Techniques   : simple string match
//  Status       : complete
// ---------------------------------------------------------

bool                                         // 'true' indicates unique
Entity::isUniqueIdentifierFull()
{
  BOOST_FOREACH( shared_ptr<Entity> s, s_censusFull )
    {
      if ( s->d_identifier == d_identifier ) // simple string match
        {
          s_logger->repx(logga::warn, "non-unique id, d_identifier", d_identifier);
          return false;
        }
    }
  return true;
}

// ---------------------------------------------------------
//  MEMBER FUNCTION : retConstMe
// ---------------------------------------------------------

const shared_ptr<Entity>
Entity::retConstMe() const                   // wrapper call to 'getConstSharedPtr'
{
  return retConstSharedPtr(d_identifier);
}

// ---------------------------------------------------------
//  MEMBER FUNCTION : retConstSharedPtr (static)
// ---------------------------------------------------------
//  Description  : effectively returns 'const' shared pointer to 'this'
//  Role         : accessor
//  On fail      : returns empty shared pointer
//  Techniques   : wrapper call to 'getNonConstSharedPtr'
//  Status       : complete
// ---------------------------------------------------------

const shared_ptr<Entity>
Entity::retConstSharedPtr
(const std::string& soughtId)
{
  return retSharedPtr(soughtId);             // 'const' added by return type
}

// ---------------------------------------------------------
//  MEMBER FUNCTION : retSharedPtr (static)
// ---------------------------------------------------------
//  Description  : effectively returns non-'const' shared pointer to 'this'
//  Role         : accessor and potential manipulator
//  On fail      : returns empty shared pointer
//  Techniques   : census search
//  Status       : complete
// ---------------------------------------------------------

shared_ptr<Entity>
Entity::retSharedPtr
(const std::string& soughtId)
{
  s_logger->repx(logga::xtra, "entering member function, sought id", soughtId);

  // SEARCHING: generally speaking, use of the 'std::find'
  // function template from <algorithm> would be considered
  // better style, but that approach also requires a custom
  // comparison functor (or free function) -- the following code
  // is therefore fine for our purposes (similar comments could
  // apply elsewhere in this unit)

  BOOST_FOREACH( shared_ptr<Entity> s, s_censusFull )
    {
      if ( s->d_identifier == soughtId )     // simple string match
        {
          return s;
        }
    } // BOOST_FOREACH

  // note fail
  s_logger->repx(logga::info, "returning empty, no match for id", soughtId);

  // provide a model debugging hint where relevant
  if ( boost::starts_with(soughtId, "entity.") )  // refer <boost/algorithm/string.hpp>
    {
      s_logger->repx(logga::warn, "remove the leading \"entity.\"", soughtId);
    }

  // finally return an empty shared pointer
  return shared_ptr<Entity>();
}

// ---------------------------------------------------------
//  MEMBER FUNCTION : checkEntityTypesLists (static)
// ---------------------------------------------------------
//  Description  : performs a set-theoretic Want / Got, returns 'true' for null set
//  Role         : checking for mandatory entities
//  Techniques   : 'std::set_difference' from <algorithm>
//  Status       : complete
// ---------------------------------------------------------

bool
Entity::checkEntityTypesLists()
{
  s_logger->repx(logga::dbug, "entering static member function", "");

  // create Got list
  BOOST_FOREACH( shared_ptr<Entity> s, s_censusFull )
    {
      const std::string classStr = (s->d_record).locateClass();
      s_logger->repx(logga::xtra, "registering entity type on got list", classStr);
      s_entityTypesGot.push_back(classStr);      // register on got list
    }

  // some convenient typedefs
  typedef std::vector<std::string> ets_list;                     // see class declaration
  typedef ets_list::iterator       ets_list_iter;

  // sort the input containers
  std::sort(s_entityTypesWant.begin(),                           // [1]
            s_entityTypesWant.end());

  std::sort(s_entityTypesGot.begin(),
            s_entityTypesGot.end());

  // "remove" consecutive duplicates
  ets_list_iter entityTypesWant_end =                            // new logical end
    std::unique(s_entityTypesWant.begin(),                       // [2]
                s_entityTypesWant.end());

  ets_list_iter entityTypesGot_end =
    std::unique(s_entityTypesGot.begin(),
                s_entityTypesGot.end());

  // take the set difference
  std::set_difference(s_entityTypesWant.begin(),                 // [3]
                      entityTypesWant_end,                       // use new logical end
                      s_entityTypesGot.begin(),
                      entityTypesGot_end,
                      std::back_inserter(s_entityTypesDiff));    // [4]

  // [1] sorting: the 'set_difference' function template from
  // <algorithm> requires sorted ranges -- moreover here the
  // range is the entire container -- and note also that the
  // 'sort' function template from <algorithm> returns 'void'
  //
  // [2] all duplicates: to "remove" all (and not just
  // consecutive) duplicates, the container must first be sorted
  // -- note too that the discarded values are simply moved to
  // the end of the container and hence the need to use the "new
  // logical end" iterator (or invoke the 'erase' member function
  // for a permanent change)
  //
  // [3] set difference: yields, in set notation: Want / Got
  //
  // [4] results container: Josuttis (1999 p420) writes "the
  // caller must ensure that the destination range is big enough
  // or that insert iterators are used" -- the latter approach is
  // used here as also indicated in Josuttis (1999 p272)

  // return status information
  if ( s_entityTypesDiff.empty() )
    {
      return true;                             // all wants were got
    }
  else
    {
      s_logger->repx(logga::warn, "ungot wants total", s_entityTypesDiff.size());
      return false;                            // not all wants were got
    }
}

// STREAM INSERTION SUPPORT

// ---------------------------------------------------------
//  MEMBER FUNCTION : streamOut
// ---------------------------------------------------------
//  Description  : workhorse for overloaded operator<<
//  Role         : for streaming entities
//  Techniques   : ostreams
//  Status       : complete
// ---------------------------------------------------------

std::ostream&
Entity::streamOut                            // support for overloaded operator<<
(std::ostream& os) const
{
  s_logger->repx(logga::warn, "stream insert helper not overridden", "");
  std::ios::fmtflags prior = os.flags();     // grab ostream state
  os << "    dummy output from an entity (not polymorphically overridden as expected)"
     << "\n";
  os.flags(prior);                           // reset ostream state
  return os;
}

// ---------------------------------------------------------
//  CLASS           : FullEntity
// ---------------------------------------------------------
//  Description  : stepping stone sub-entity
//  Role         : part of the full entity hierarchy
//  Techniques   : inheritance
//  Status       : complete
// ---------------------------------------------------------

// STATIC DEFINITIONS - with and without explicit initialization

int FullEntity::s_fullCount = 0;

// CREATORS

FullEntity::FullEntity
(const std::string entityId,
 Record&           record) :
  Entity(entityId, record),
  d_builtinRemark(record.tieSingle<std::string>("builtin-remark"))
{
  s_logger->repx(logga::adhc, "constructor call", getIdAndKind());
  isUniqueIdentifierFull();                  // char-by-char uniqueness
  ++s_fullCount;
  d_builtinRemark = "(not overwritten by entity author)";
}

FullEntity::~FullEntity()
{
  s_logger->repx(logga::xtra, "destructor call", getIdAndKind());
  --s_fullCount;
}

void
FullEntity::factoryInitialize()
{
  s_logger->repx(logga::xtra, "entity instance initialize", "full entity form");
}

// ---------------------------------------------------------
//  MEMBER FUNCTION : listToVec <> (static)
// ---------------------------------------------------------
//  Description  : converts sub-entity lists to sub-entity vectors
//  Role         : explicit (rather than "lazy") multiple sub-entity linking
//  Techniques   : templates, downcasting, 'Boost.String_algo' library
//  Requires     : friendship from 'Entity'
//  Status       : complete
//
//  Typical usage (for the hypothetical 'RedAnt' class):
//
//      const std::string&               d_red_ants;
//      std::vector<shared_ptr<RedAnt> > d_redAnts;
//
//      const int redAntsCnt
//        = FullEntity::listToVec<RedAnt>(d_red_ants, d_redAnts);
//
// ---------------------------------------------------------

template<typename E>                         // downcasting employed
int                                          // returns number of elements in 'vec'
FullEntity::listToVec
(const std::string&           list,          // order is significant
 std::vector<shared_ptr<E> >& vec)           // pass by non-const reference, note the 'E'
{
  // initial logging, note that the hash-ifdef removes a
  // link-time error if the unit containing 'E' is not available
  // -- as may well be the case when unit testing
  const std::string msg = "entering member function, E type";
#ifdef _XUTEST
  s_logger->repx(logga::xtra, msg, "(not sought in unit tests)");
#else
  const std::string etype = xeona::demangle(typeid(E).name());
  s_logger->repx(logga::dbug, msg, etype);
#endif // _XUTEST

  // possibly report if 'list' is empty, noting that
  // 'xeona::rankNoData' is set by option '--nodata'
  if ( list.empty() )
    {
      s_logger->repx(logga::rankNoData, "list argument is empty", "");
    }

  // an empty vector is expected
  if ( ! vec.empty() )
    {
      s_logger->repx(logga::rankJumpy, "vector argument is not empty", vec.size()); // [1]
      vec.clear();                           // remove all elements (but expecting empty)
    }
  // [1] CAUTION: this 'repx' used to be 'logga::warn' but the
  // gateway registeration process can make repeat calls to this
  // function -- if 'logga::rankJumpy' causes problems, simply
  // downgrade to 'logga::dbug'

  // CAUTION: an empty string (of size zero), when split,
  // produces a single empty element vector (of size one) -- this
  // is not helpful, hence the protection below keeps the split
  // list buffer empty in such cases

  // split the input, while noting the above caution
  std::vector<std::string> splitList;        // declare a split list buffer
  if ( ! list.empty() )
    {
      // split the list whilst compressing adjacent tokens
      boost::split(splitList, list, boost::is_any_of(" "), boost::token_compress_on);
    }

  // fill the vector
  BOOST_FOREACH( std::string s, splitList )
    {
      shared_ptr<Entity> sp;
      sp = Entity::retSharedPtr(s);          // requires friendship from 'Entity'
      if ( sp )                              // 'retSharedPtr' returns empty on failure
        {
          shared_ptr<E> sE;
          sE = dynamic_pointer_cast<E>(sp);  // downcast to 'E'
          vec.push_back(sE);                 // fill while maintaining order
        }
    }

  // integrity checks and reporting
  const int splitSize = splitList.size();    // input string duly split (unless empty)
  const int vecSize   = vec.size();          // resultant vector

  if ( splitSize > vecSize )                 // hanging association/s encountered
    {
      std::ostringstream oss;
      oss << splitSize << " : " << vecSize;
      s_logger->repx(logga::rankNoData, "size mismatch, list : vector", oss.str());

      std::ostringstream put;
      std::ostringstream oss2;
      put << "    hanging association/s found" << "\n";
      BOOST_FOREACH( std::string s, splitList )
        {
          put  << "      " << s << "\n";
          oss2 << s << " ";                  // trailing space is later removed
        }
      s_logger->putx(logga::rankNoData, put);
      s_logger->addSmartBlank(logga::rankNoData);

      // throw if appropriate
      if ( xeona::nopro == false )           // meaning option '--krazy' not applied
        {
          s_logger->repx(logga::warn, "will throw xeona::xem_data_issue", "");
          throw xeona::xem_data_issue("hanging association/s found",
                                      "offending identities",
                                      oss2.str());
        }
    }
  else if ( splitSize < vecSize )            // very strange result, should never be here
    {
      std::ostringstream oss;
      oss << splitSize << " : " << vecSize;
      s_logger->repx(logga::kill, "size mismatch, list : vector", oss.str());
    }

  // return vector size
  return vecSize;

} // function 'listToVec'

// ---------------------------------------------------------
//  FREE FUNCTION   : xeona::putxId
// ---------------------------------------------------------

namespace xeona
{
  int
  putxId
  (const shared_ptr<Entity> e,
   const std::string&       comment)         // note empty string default in header
  {
    static logga::spLogger logger = logga::ptrLogStream();
    static int s_passes           = 0;
    static int s_fails            = 0;
    if ( e )
      {
        std::string buf = "";
        if ( ! comment.empty() ) buf = "  " + comment;
        const std::string id   = "  " + e->getIdAndKind();
#if 0 // 0 = add type information, 1 = skip
        const std::string type = "";                                         // cheap
#else
        const std::string type = "  " + xeona::demangle(typeid(*e).name());  // expensive
#endif // 0
        const std::string tag = "LOOP-";
        std::ostringstream put;
        put << boost::format("      %s%02d%s%s%s\n") % tag % ++s_passes % type % id % buf;
        logger->addSmartBlank(logga::dbug);
        logger->putx(logga::dbug, put);
      }
    else
      {
        logger->repx(logga::warn, "passed an empty/null entity, count", ++s_fails);
      }
    return s_fails;
  }

} // namespace 'xeona'

// ---------------------------------------------------------
//  EXPLICIT TEMPLATE INSTANTIATIONS
// ---------------------------------------------------------
//
//  Place all required template instantiations here.  That is,
//  place all required (or potentially required) header-declared
//  class and member function template instantiations at the end
//  of the implementation file.
//
//  (Alternatively, move the relevant definitions to the header
//  file.)
//
//  For simple information on template coding strategies, read
//  Cline (2006) section 35.  If this is insufficient for your
//  needs, refer to Lischner (2003 pp195-199) and Dattatri (2002
//  pp456-461).
//
//  Failure to properly instantiate a template will typically
//  result in a link-time error whenever a (constructor or
//  function) call is made.  The linker usually says: "undefined
//  reference to ...".  If no call is made, then the program
//  should compile normally and run without a murmur.
//
//    Cline, Marshall.  2006.  C++ FAQ lite.
//    [www.parashift.com/c++-faq-lite/templates.html]

//  CAUTION: regarding the 'listToVec' instantiations, the 'E'
//  needs to be fully defined, that is, the relevant header needs
//  to be hash-included -- otherwise the following error results
//  (noting that the header path has been resolved here):
//
//      /usr/include/c++/4.1.2/tr1/boost_shared_ptr.h:599:
//      error:
//      cannot dynamic_cast '__r->std::tr1::shared_ptr<Entity>::_M_ptr'
//      (of type 'class Entity* const') to type 'struct Xxxx*'
//      (target is not pointer or reference to complete type)

template
int FullEntity::listToVec<AssetOperator>
(const std::string&, std::vector<shared_ptr<AssetOperator> >&);

template
int FullEntity::listToVec<DemandJunction>
(const std::string&, std::vector<shared_ptr<DemandJunction> >&);

template
int FullEntity::listToVec<DomainController>
(const std::string&, std::vector<shared_ptr<DomainController> >&);

template
int FullEntity::listToVec<Gateway>
(const std::string&, std::vector<shared_ptr<Gateway> >&);

template
int FullEntity::listToVec<LmpNode>
(const std::string&, std::vector<shared_ptr<LmpNode> >&);

template
int FullEntity::listToVec<TechnicalAsset>
(const std::string&, std::vector<shared_ptr<TechnicalAsset> >&);

//  end of file

