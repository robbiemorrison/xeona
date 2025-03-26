//  XEONA ENERGY SYSTEMS MODELING ENVIRONMENT
//
//  file-create-name : factory.cc
//  file-create-date : Tue 22-May-2007 12:59 UTC
//  file-initiator   : Robbie Morrison <robbie@actrix.co.nz>
//
//  file-role        : entity sub-class factory / implementation
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
//  $URL: file:///home/robbie/svn-root/xeona/futz/trunk/xeona1/c/factory.cc $

//  LOCAL AND SYSTEM INCLUDES

#include "factory.h"          // companion header for this file (place first)

#include "../c/recset.h"      // records and fields and also record-sets
#include "../c/util1.h"       // free functions which offer general utilities 1
#include "../a/logger.h"      // run-time logging functionality (as required)
#include ".././common.h"      // common definitions for project (place last)

#include <sstream>            // string-streams

#include <boost/format.hpp>   // printf style formatting

//  CODE

// ---------------------------------------------------------
//  CLASS           : EntityFactory
// ---------------------------------------------------------
//  Description  : an enum-keyed entity object factory
//  Patterns     : concrete object factory, singleton
//  Status       : complete
//
//  Design notes
//
//      This class used two classic design patterns: the concrete
//      object factory and the singleton.  Code for both drew
//      heavily on Alexandrescu (2001).  Somewhat similar
//      material is presented in Stephens etal (2006) but is not
//      discussed as comprehensively.
//
//      The singleton is a build-on-first-request object, which,
//      in this case, lasts until some time after the end of
//      'main()'.  The static 's_instance' pointer relies on
//      "static initialization" meaning it is a type without
//      constructor initialized with a compile-time constant.
//      This strategy is, for reasons not give here, inherently
//      safe.
//
//      The object factory was originally coded with const
//      std::string type identifiers, but these were changed to
//      xeona::EntityType enums to enable compile-time checking.
//      Then r2209 reverted to the original approach.
//
//      The use of a smart (rather than raw) pointer for
//      's_instance' prevented a small memory leak.
//
//      The factory pattern will be required when
//      "deserialization" is introduced.  Deserialization is the
//      process of creating C++ objects from information stored
//      on disk.  Further thought will be required to implement
//      this functionality.
//
//      Finally, apologies to Alexandrescu for not taking his
//      advice and hand crafting this class to a much larger
//      degree than he might consider sensible.
//
//  Additional comment regarding singletons
//
//      With hindsight, it is not worth implementing formal
//      singletons.  It is quite sufficient to add a constructor
//      counter and log warnings on second and subsequent calls.
//
//  CAUTION: interacting singletons
//
//      Coordinating the life expectancy, and more specifically,
//      the destruction order of interacting singleton objects is
//      both subtle and difficult -- under the current code (and
//      build environment), the 's_logger' Logger object may be
//      destroyed before the 'EntityFactory' singleton is
//      destroyed.  Hence, 's_logger' calls placed in
//      ~EntityFactory() may result in unpredictable behavior.
//
//      Alexandrescu (2001 pp126-156) provides an excellent
//      description of the issues involved.  Read his chapter on
//      singletons if you need to know more.
//
//  References
//
//      Alexandrescu, Andrei.  2001.  Modern C++ design : generic
//        programming and design patterns applied.
//        Addison-Wesley, Boston, USA.  ISBN 0-201-70431-5.
//
//      Stephens, D Ryan, Christopher Diggins, Jonathan Turkanis,
//        and Jeff Cogswell.  2006.  C++ cookbook : solutions and
//        examples for C++ programmers.  O'Reilly Media,
//        Sebastopol, California, USA.  ISBN 0-596-00761-2.
//
// ---------------------------------------------------------

//  STATIC DEFINITIONS

EntityFactory::entityFactory_type
EntityFactory::s_instance = EntityFactory::entityFactory_type(); // [1]

// [1] creates an empty smart pointer (the following will also
// work = EntityFactory::entityFactory_type();) -- for more
// details on empty, null, single, and copied shared_ptr, see
// documentation for the free function 'xeona::reportShared_ptr'.

logga::spLogger
EntityFactory::s_logger = logga::ptrLogStream();  // bind logger on definition

// CREATORS

EntityFactory::EntityFactory() :             // CAUTION: private
  d_callbacks()
{
  s_logger->repx(logga::dbug, "constructor call", "singleton pattern");
}

EntityFactory::~EntityFactory()
{
  // CAUTION: 'Logger' object calls placed here will cause
  // trouble if the 'Logger' singleton has already been destroyed
  // -- at the time of writing, this is not currently a problem
  // but there are NO GUARANTEES (see the comments in factory
  // unit test for more information)

  // 0 = do nothing
  // 1 = call with Logger object
  // 2 = call without (not under logga::Rank control)

#if !defined(XE_FACTORY_DTOR_LOGGING)
# define XE_FACTORY_DTOR_LOGGING 0
#endif

#if   (XE_FACTORY_DTOR_LOGGING == 0)
  // do nothing
#elif (XE_FACTORY_DTOR_LOGGING == 1)
  s_logger->repx(logga::dbug, "destructor call", "placement okay?");
#elif (XE_FACTORY_DTOR_LOGGING == 2)
  xeona::logDirect(__FILE__, __LINE__, __func__, "destructor call", "logDirect report");
#endif // XE_FACTORY_DTOR_LOGGING
}

// SINGLE POINT OF ACCESS

// 'instance' could return an 'EntityFactory&' reference, but
// here 'instance' provides a dereferenced *s_instance -- and
// therefore indirection is required in the client code:
//
//     EntityFactory::instance()->someFn()

EntityFactory::entityFactory_type
EntityFactory::iface()
{
  if ( ! s_instance )                        // implicit boolean conversion
    {
      entityFactory_type smartptr(new EntityFactory());     // constructor call
      s_instance = smartptr;                                // store locally
      s_logger->repx(logga::dbug, "first time, s_instance ptr", s_instance);
    }
  else
    {
      s_logger->repx(logga::adhc, "subsequent, s_instance ptr", s_instance);
    }
  return s_instance;
}

// MANIPULATORS

bool                                         // true if successful
EntityFactory::registerEntity
(const std::string    entityRegn,
 createEntityCallback createfn)
{
  s_logger->repx(logga::xtra, "about to register entity type", entityRegn);

  // for sets and maps, insert() returns a std::pair<iterator,
  // bool> signalling position and success, and here 'second'
  // refers to this struct and NOT our make_pair struct (Josuttis
  // 1999 p183, Alexandrescu 2001 p205)

  return d_callbacks.insert(std::make_pair(entityRegn, createfn)).second;
}

bool                                         // true if previously registered
EntityFactory::unregisterEntity
(const std::string entityRegn)
{
  s_logger->repx(logga::dbug, "about to UNregister entity type", entityRegn);

  // member function 'erase()' returns number of elements removed
  return d_callbacks.erase(entityRegn) == 1;
}

shared_ptr<Entity>
EntityFactory::createEntityBind
(const std::string  entityRegn,
 const std::string& entityId,
 Record&            r)                       // live record
  throw(std::exception,                      // exception specification
        xeona::empty_wrap,
        xeona::non_registration)
{
  s_logger->repx(logga::xtra, "processing entity", entityId);
  callback_map::const_iterator pos;
  pos = d_callbacks.find(entityRegn);
  if ( pos == d_callbacks.end() )
    {
      s_logger->repx(logga::warn, "entity registration not found", entityRegn);

      // if '--krazy' return empty pointer and continue,
      // otherwise notify and exit

      if ( xeona::nopro )
        {
          std::ostringstream put;
          put << "** unregistered entity class requested"                         << "\n"
              << "       using '--krazy' code"                                    << "\n"
              << "       entity identifier   : " << entityId                      << "\n"
# ifndef _XUTEST // not unit testing [1]
              << "       requested class     : " << r.locateClass()               << "\n"
# endif // _XUTEST
              << "       entity registration : " << entityRegn                    << "\n"
              << "   about to return empty pointer of type 'shared_ptr<Entity>'"  << "\n"
              << "   this will almost certainly cause more problems downstream"   << "\n";
          s_logger->putx(logga::warn, put);

          return shared_ptr<Entity>();       // empty shared pointer
        }
      else
        {
# ifndef _XUTEST // not unit testing [1]
          const std::string requestedClass = r.locateClass();
# else
          const std::string requestedClass = "(not available for unit test)";
# endif // _XUTEST
          s_logger->repx(logga::warn, "will throw xeona::non_registration", "");
          throw xeona::non_registration(entityId, entityRegn, requestedClass);
        } // xeona::nopro

      // [1] gave rather puzzling link-time error for unit test:
      //     b/entity.o: In function `__tcf_13':
      //     collect2: ld returned 1 exit status

    } // if ( pos == d_callbacks.end() )

  // note the following public typedef declared within class
  // 'EntityFactory':
  //
  //   shared_ptr<Entity> (*createEntityCallback)(const std::string&, Record&)

  return (pos->second)(entityId, r);         // invoke creation function
}

//  end of file

