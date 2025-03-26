//  XEONA ENERGY SYSTEMS MODELING ENVIRONMENT
//
//  file-create-name : entity.h
//  file-create-date : Fri 15-Jun-2007 08:51 UTC
//  file-initiator   : Robbie Morrison <robbie@actrix.co.nz>
//
//  file-role        : entity base class plus lazy linking / header
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
//  $URL: file:///home/robbie/svn-root/xeona/futz/trunk/xeona1/b/entity.h $

//  HEADER GUARD

#ifndef _ENTITY_H_
#define _ENTITY_H_

//  LOCAL AND SYSTEM INCLUDES

#include "../c/xeona_ptr.h"   // remappable counted pointer which mimics shared_ptr
#include "../c/linklog.h"     // utility class to record entity linking results
#include "../a/logger.h"      // run-time logging functionality (as required)

#include "../c/util1.h"       // free functions which offer general utilities 1
#include "../a/logger_fwd.h"  // some forward declarations from "logger.h"
#include "../c/smart_ptr.h"   // toggle between TR1 and Boost smart pointers

#include <iostream>           // standard io, used here for overloaded stream inserters
#include <list>               // STL sequence container
#include <limits>             // numeric_limits<T>::infinity() and similar
#include <string>             // C++ strings
#include <vector>             // STL sequence container

#include <typeinfo>           // run-time type info, NOTE: passive reporting role only

//  NAMESPACE DIRECTIVES

using xeona::assign_ptr;                     // remappable counted pointer

//  FORWARD (PARTIAL) DECLARATIONS

class Overseer;                              // see "overseer.h"
class DomainController;                      // see "domcon.h"
class Record;                                // see "recset.h"

//  CODE

// ---------------------------------------------------------
//  ENUM            : xeona::Hemisphere
// ---------------------------------------------------------

namespace xeona
{
  enum Hemisphere
    {
      e_hemiNotSet = 0,
      e_north      = 1,
      e_south      = 2
    };
}

// ---------------------------------------------------------
//  ENUM            : xeona::LeapYear
// ---------------------------------------------------------

namespace xeona
{
  enum LeapYear
    {
      e_leapNotSet = 0,
      e_normal     = 1,
      e_leap       = 2
    };
}

// ---------------------------------------------------------
//  CLASS           : Entity (abstract base)
// ---------------------------------------------------------
//  Description  : abstract base class for all entities
//  Role         : provide basic housekeeping and class-wide support
//  Status       : complete
// ---------------------------------------------------------
//
//  Design notes
//
//      Overview
//
//          There are two programmatic issues that the 'Entity'
//          class needs to deal with at the base-class level:
//
//            * the factory construction of entities from model data
//            * the linking in of associated entities
//
//      Factory construction
//
//          Refer to documentation for the 'EntityFactory' class
//          and for the '::createType<>' free function template.
//
//      Lazy linking
//
//          The term "link" here refers an internal procedure for
//          remapping hollow entities to full entities and NOT to
//          the compile-time process of combining object files to
//          create an executable file.
//
//          Client sub-entities often need to link to service
//          sub-entities by way of embedded applied pointers.
//          These linkages cannot be established when the clients
//          are constructed because the respective service
//          sub-entities may not have been built yet.  The
//          process of linking, therefore, must wait until the
//          process of sub-entity construction is complete.
//
//          Applied pointers are remappable shared pointers.
//          Applied pointers are unique to 'xeona'.  See unit
//          'xeona_ptr' for details.
//
//          This code employs "lazy linking".  Lazy linking
//          allows an intuitive and economical syntax to be used
//          by entity authors when specifying these linkages --
//          because the actual mechanics of linking is handled
//          outside of the modules.
//
//          The service entity must be a "LLE" or "lazy-linkable"
//          entity.  There are two LLE requirements:
//
//              * a locally defined 'polylink' member function
//                which comprises one line of standard code with
//                the appropriate template argument
//
//              * a single-argument constructor which lacks the
//                'Record&' argument and which is supported up
//                (base-wise) the inheritance chain
//
//          The module syntax is as follows (see the ambient air
//          statement) where 'd_ambientAirContext' is of type
//          'assign_ptr<CxAmbientAir>':
//
//              SubEntity::SubEntity
//              (const std::string& entityId,
//               Record&            record) :
//                Entity(entityId, record),
//                d_coeff(record.tieSingle<double>("coeff")),
//                d_ambientAirContext(record.lazyLink<CxAmbientAir>
//                  ("ambient-air-context"))
//              {
//              }
//
//          The "link" entity CONSTRUCT chain is as follows:
//
//            client sub-entity constructor (see above)
//             |
//             + assign_ptr<Entity> Record::lazyLink<E>(linkname)  // name as record field
//                |
//                + static shared_ptr<Entity> makeLinkEntity(soughtId)  // recovered id
//                   |
//                   - assign_ptr<E> link(new E(soughtId))
//                   - s_censusLink.push_back(link)
//                   - return link
//
//          And the "link" entity LINKING chain is as follows:
//
//            simulation management
//             |
//             + static Entity::linkAll()
//
//      Function template 'polylink' in standard-form:
//
//          private:
//            bool
//            LLE::polylink(assign_ptr<Entity>& pass)
//            {
//              return pass.revamp(retFull<LLE>());    // key call
//            }
//
// ---------------------------------------------------------

class Entity
  : public enable_shared_from_this<Entity>
{
  // FRIENDS

  friend class TimeHorizon;                  // sub-entity sets horizon steps and interval
  friend class Interface;                    // for access to 'retSharedPtr'
  friend class BuySide;                      // for access to 'retSharedPtr', gateway
  friend class SelSide;                      // for access to 'retSharedPtr', gateway
  friend class LinkLogger;                   // for access to 'retSharedPtr'

  friend class FullEntity;                   // to retain private access specifiers

  // DISABLED

private:

  Entity();                                  // zero-argument construction
  Entity(const Entity& orig);                // copy constructor
  Entity& operator= (const Entity& orig);    // copy assignment operator

  // CREATORS

public:

  explicit
  Entity                                     // used by "link" factory ('makeLinkEntity')
  (const std::string entityId) :
    d_identifier(entityId),
    d_record(s_recordStandIn),
    d_typeStr()
  { }

  explicit                                   // disable implicit type conversions (string)
  Entity                                     // used by "full" factory (complicated)
  (const std::string entityId,               // enforced unique identifier on full list
   Record&           record);                // associated record

protected:

  // PROTECTED VIRTUAL DESTRUCTORS AND SMART POINTERS:
  //
  // First, the use of 'virtual' destructors means that the
  // correct destructors are called.
  //
  // Second, the use of 'protected' rather than 'public' means
  // that a raw pointer (extracted from a smart pointer, say)
  // cannot use 'delete', but (the original) smart pointer still
  // works fine -- see Karlsson (2006 pp23-24) for an explanation
  // and also Sutter and Alexandrescu (2005 pp90-91).
  //
  // Concrete classes need 'public' destructors and should also
  // be 'virtual' if further derived.

  virtual                                    // support for polymorphic destruction
  ~Entity() = 0;                             // create abstract class

  // LINKING CALL NON-OVERWRITE WARNING

protected:

  virtual
  bool
  polylink(assign_ptr<Entity>& pass)
  {
    // key call is "return pass.revamp(retFull<L>())" with L = lazy-linkable entity

    // logging messages only
    s_logger->repx(logga::warn, "function not overwritten as required", "Entity");
    std::ostringstream put;
    put << "  lazy-linkable sub-entities must overwrite the 'polylink' function" << "\n";
    s_logger->putx(logga::dbug, put);

    // return fail
    return false;
  }

  // INITIALIZERS

public:

  // INITIALIZATION: the object factory invokes the the
  // 'factoryInitialize' method directly after the non-default
  // constructor call, namely 'E(const std::string, Record&)'
  // where 'E' is the templated sub-entity -- see 'register.cc'
  // for the actual code.

  virtual
  void                                       // called by factory after construction
  factoryInitialize();

  // ACCESSORS

  std::string
  getIdentifier() const;                     // returns d_identifier

  std::string                                // primarily for use in ctor and dtor logging
  getIdAndKind                               // returns d_identifier and if link entity
  (const std::string& auxMsg = "") const;    // optional auxillary message appended in []

  std::string                                // provided for testing
  getTypeStr() const;                        // RTTI name, duly cleaned for g++

  int
  getUseCount() const;                       // includes copy in the respective census

  std::string
  report
  (const std::string msg = "(not given)") const;

  // INSTANCE MANIPULATORS -- for the set of 'EntityFactory' '::createType<>' functions

  template <typename E>                      // CAUTION: must be defined in header file
  void
  processFabricatedPtr
  (shared_ptr<E> me)                         // called with implicit instantiation
  {
    s_censusFull.push_back(me);              // copy into shared census

    std::string compilerName = typeid(E).name();  // 'name' returns 'const char*'
    d_typeStr = xeona::demangle(compilerName);    // cleaning substantive only for g++
    s_logger->repx(logga::adhc, "type string", getTypeStr());
  }

  // STATIC MANIPULATORS -- for the "lazy" linking process

  // ---------------------------------------------------------
  //  MEMBER FUNCTION : makeLinkEntity <> (static)
  // ---------------------------------------------------------

  // CAUTION: this function is defined in the header file to
  // allow for implicit template instantiation

  template <typename E>
  static
  assign_ptr<E>
  makeLinkEntity                             // static factory function
  (const std::string& soughtId)              // note sought id rather than own identifier
  {
    s_logger->repx(logga::dbug, "entering member function", soughtId);

    assign_ptr<E> link(new E(soughtId));     // "factory" call (no registration required)
    s_censusLink.push_back(link);            // upcast copy constructor called
    return link;
  }

  static
  bool
  linkAll();                                 // CAUTION: must follow entity construction

  // STATIC ACCESSORS -- general use

  static
  int                                        // return "full" census size
  getFullPopn();                             // static member funcs cannot be cv-qualified

  static
  int                                        // unitless
  getHorizonSteps();

  static
  int                                        // time in seconds
  getHorizonInterval();

  static
  int
  getHorizonIntsPerDay();                    // intervals per day, ranging 1 thru 288

  static
  int
  getHorizonStartHour();

  static
  int
  getHorizonStartDay();

  static
  int                                        // measured in "steps"
  getHorizonOffset();

  static
  bool
  getLeapYear();

  static
  const xeona::Hemisphere
  getHemisphere();

  static
  const bool                                 // 'true' if in "full" census, else 'false'
  confirmIdentifier
  (const std::string& soughtId);

  static
  shared_ptr<Overseer>
  retOverseer();                             // could equally classify as a manipulator

  static
  std::vector<shared_ptr<DomainController> >
  retDomains();                              // could equally classify as a manipulator

  // STATIC ACCESSORS FOR TEST PURPOSES

  static
  int                                        // the number of (formed or unformed) links
  getLinkPopn();

  static
  int                                        // return "raw" census size, links and fulls
  getRawPopn();

  static
  const std::list<const Entity*>&            // return a 'std::list' reference
  getCensusRaw();

  static
  std::string                                // return formatted identifiers
  traverseFullPopulation();                  // uses the "full" census

  static
  const std::vector<shared_ptr<Entity> >&
  getCensusFull();

  static
  std::string
  reportFullPopulation();

  // STATIC MANIPULATORS

  static
  bool                                       // 'true' unless call was inappropriate
  setHorizonSteps
  (const int steps);

  static
  bool                                       // 'true' unless call was inappropriate
  setHorizonInterval
  (const int interval);

  static
  bool                                       // 'true' unless call was inappropriate
  setHorizonStartHour
  (const int startHour);

  static
  bool                                       // 'true' unless call was inappropriate
  setHorizonStartDay
  (const int startDay);

  static
  bool                                       // 'true' unless call was inappropriate
  setLeapYear
  (const bool leapYear);

  static
  bool                                       // 'true' unless call was inappropriate
  setHemisphere
  (const xeona::Hemisphere hemisphere);

  static
  void
  addWant(const std::string& entityType);    // append to list of mandatory entities

  static
  void
  checkWants
  (std::vector<std::string>& buffer);        // confirm that all mandatory entities exist

  // STATIC MANIPULATORS FOR TEST PURPOSES

#ifdef _XUTEST                               // a compile-time failure under normal build

  static
  void
  setHorizonDirectly
  (const int steps,
   const int interval)
  {
    s_horizonSteps    = steps;               // unitless
    s_horizonInterval = interval;            // in seconds
  }

#endif // _XUTEST

  // INSTANCE UTILITY FUNCTIONS

protected:

  bool                                       // 'true' indicates unique
  isUniqueIdentifierFull();                  // "full" means the full census is used

private:

  const shared_ptr<Entity>
  retConstMe() const;                        // wrapper call to 'retConstSharedPtr'

  // ---------------------------------------------------------
  //  MEMBER FUNCTION : retFull <>
  // ---------------------------------------------------------
  //
  //  Design notes
  //
  //      This function can only return a "full" version.
  //
  // ---------------------------------------------------------

protected:

  template <typename E>
  shared_ptr<E>
  retFull() const
  {
    const std::string identifier = getIdentifier();
    shared_ptr<E> temp = dynamic_pointer_cast<E>(Entity::retSharedPtr(identifier));
    if ( temp == 0 )
      {
        const std::string etype = xeona::demangle(typeid(E).name());
        const std::string mtype = getTypeStr();
        s_logger->repx(logga::warn, "dynamic pointer cast failed", mtype);
        if ( xeona::nopro == false )         // meaning option '--krazy' not applied
          {
            s_logger->repx(logga::warn, "will throw xeona::full_link_fail", "");
            throw xeona::full_link_fail(identifier,    // identifier
                                        etype,         // template type
                                        mtype);        // my type
          }
      }
    return temp;
  }

  // ---------------------------------------------------------
  //  MEMBER FUNCTION : retMe
  // ---------------------------------------------------------
  //
  //  Design notes
  //
  //      The <Entity const> means that the controlled resource
  //      cannot be switched.
  //
  //      This function can return both "full" and "link"
  //      versions, depending on the host.
  //
  // ---------------------------------------------------------

  shared_ptr<Entity const>
  retMe() const
  {
    return retMe<Entity const>();
  }

  // ---------------------------------------------------------
  //  MEMBER FUNCTION : retMe <>
  // ---------------------------------------------------------
  //
  //  Design notes
  //
  //      The <E const> means that the controlled resource cannot
  //      be switched.
  //
  //      This function can return both "full" and "link"
  //      versions, depending on the host.
  //
  //      See also: Boost mailing list archive: From: Raffaele
  //      Romito / Date: 2006-01-10 10:58:23 / search-terms:
  //      "shared_ptr retrieve inherit base"
  //
  // ---------------------------------------------------------

  template <typename E>
  shared_ptr<E const>
  retMe() const
  {
    shared_ptr<E const> temp = dynamic_pointer_cast<E const>(shared_from_this());
    if ( temp == 0 )
      {
        const std::string type = xeona::demangle(typeid(E).name());
        s_logger->repx(logga::warn, "dynamic pointer cast failed", type);
      }
    return temp;
  }

  // STATIC UTILITY FUNCTIONS

private:                                     // CAUTION: note the friendships

  static
  const shared_ptr<Entity>
  retConstSharedPtr                          // wrapper call to 'retSharedPtr'
  (const std::string& soughtId);

  static
  shared_ptr<Entity>
  retSharedPtr                               // could equally classify as a manipulator
  (const std::string& soughtId);

  static
  bool                                       // 'true' if all "wanted entities" were got
  checkEntityTypesLists();

  // STREAM INSERTION SUPPORT

public:

  virtual                                    // CAUTION: needed for polymorphic behavior
  std::ostream&
  streamOut                                  // support for overloaded operator<<
  (std::ostream& os) const;

  // INSTANCE DATA

private:

  const std::string    d_identifier;         // unique identifier as given in model
  const Record&        d_record;             // used passively for housekeeping tasks
  std::string          d_typeStr;            // based on RTTI (run-time type information)

  // STATIC DATA

private:

  static int                  s_horizonSteps;          // unitless
  static int                  s_horizonInterval;       // in seconds
  static int                  s_horizonStartHour;      // [0,23]
  static int                  s_horizonStartDay;       // [1,365]
  static xeona::LeapYear      s_leapYear;
  static xeona::Hemisphere    s_hemisphere;            // {e_north, e_south}

  static int                  s_linkAllCallCount;      // 'linkAll' calls count

  static std::list<const Entity*>          s_censusRaw;     // all Entity instances
  static std::vector<shared_ptr<Entity> >  s_censusFull;    // "full" Entity census
  static std::vector<assign_ptr<Entity> >  s_censusLink;    // "link" Entity census
  static std::vector<std::string>          s_identsLink;    // parallel sought identifiers

  static std::vector<std::string>    s_entityTypesWant;     // list of mandatory types
  static std::vector<std::string>    s_entityTypesGot;      // list of types supplied
  static std::vector<std::string>    s_entityTypesDiff;     // list of types not got

  static LinkLogger                  s_linkLogger;          // monitor linking outcomes

protected:

  static logga::spLogger     s_logger;            // shared_ptr to single logger object

  // stand-in quantites for use in link entity constructors and similar

  static Record         s_recordStandIn;
  static std::string    s_stringEmpty;
  static double         s_doubleZero;
  static int            s_intZero;
  static bool           s_boolFalse;

  // UNIT TESTING METHODS -- defined here too for convenience

public:

#ifdef _XUTEST                               // specifically for class 'Datfact_UT'

  // CAUTION: must be public but DO NOT place the access
  // specifier within this preprocessor 'hash-ifdef'

  virtual
  double
  getX() const
  {
    return std::numeric_limits<double>::quiet_NaN();   // output streams as a "nan"
  }

  virtual
  void
  multiplyX(const double factor)
  { }

#endif // _XUTEST

}; // class 'Entity'

// ---------------------------------------------------------
//  FREE FUNCTION   : operator<< (std::ostream&, Entity&)
// ---------------------------------------------------------

// CAUTION: place after client class declaration

inline                                       // CAUTION: inline essential
std::ostream&
operator<<                                   // overloaded operator
(std::ostream& os,
const Entity&  en)
{
  return en.streamOut(os);                   // wrapper to streamOut member function
}

// ---------------------------------------------------------
//  CLASS           : FullEntity (abstract)
// ---------------------------------------------------------
//  Description  : stepping stone sub-entity
//  Role         : part of the full entity hierarchy
//  Techniques   : inheritance
//  Status       : complete
// ---------------------------------------------------------

class FullEntity
  : public Entity
{
  // DISABLED

private:

  FullEntity();                                        // zero-argument constructor
  FullEntity(const FullEntity& orig);                  // copy constructor
  FullEntity& operator= (const FullEntity& orig);      // copy assignment operator

  // CREATORS

public:

  explicit
  FullEntity(const std::string entityId) :
    Entity(entityId),
    d_builtinRemark(s_stringEmpty)
  { }

  explicit                                   // disable implicit type conversions (string)
  FullEntity
  (const std::string entityId,
   Record&           record);

  virtual
  ~FullEntity() = 0;                         // create abstract class

  // MANIPULATORS

  virtual
  void                                       // called by factory after construction
  factoryInitialize();

  // STATIC ACCESSORS

  template<typename E>                       // downcasting employed
  static
  int                                        // returns number of elements in 'vec'
  listToVec                                  // explicit multiple entity linking
  (const std::string&           list,        // order is significant
   std::vector<shared_ptr<E> >& vec);        // pass by non-const reference, note the 'E'

  // INTERNAL DATA

protected:

  std::string&    d_builtinRemark;           // normally re-assigned in derived classes

private:

  static int      s_fullCount;               // "full" entity con/destructor call count

}; // class 'FullEntity'

// ---------------------------------------------------------
//  FREE FUNCTION   : xeona::putxId
// ---------------------------------------------------------
//  Description  : conveniently add 'putx' output
//  Role         : called in various processing loops to improve logging readability
//  Techniques   : 'boost::format', static variables
//  Status       : complete
// ---------------------------------------------------------

namespace xeona
{
  int                                        // empty/null entities count passed to date
  putxId
  (const shared_ptr<Entity> e,               // full or link entity
   const std::string&       comment = "");   // optional comment

} // namespace 'xeona'

#endif // _ENTITY_H_

//  end of file

