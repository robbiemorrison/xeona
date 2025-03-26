//  XEONA ENERGY SYSTEMS MODELING ENVIRONMENT
//
//  file-create-name : factory.h
//  file-create-date : Tue 22-May-2007 12:58 UTC
//  file-initiator   : Robbie Morrison <robbie@actrix.co.nz>
//
//  file-role        : sub-entity factory / header
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
//  $URL: file:///home/robbie/svn-root/xeona/futz/trunk/xeona1/c/factory.h $

//  HEADER GUARD

#ifndef _FACTORY_H_
#define _FACTORY_H_

//  LOCAL AND SYSTEM INCLUDES

#include "../b/register.h"    // sub-entity registrations
#include "../a/exapp.h"       // application exception classes
#include "../a/logger_fwd.h"  // some forward declarations from "logger.h"
#include "../c/smart_ptr.h"   // switch easily between TR1 and Boost smart pointers

#include <string>             // C++ strings
#include <map>                // STL associative container

//  FORWARD (PARTIAL) DECLARATIONS

class Entity;                                // from "entity.h"
class Record;                                // from "recset.h"

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
//      Note the earlier typedef: "shared_ptr<Entity> entity_type"
//
// ---------------------------------------------------------

class EntityFactory
{
  // TYPEDEFS

public:

  typedef shared_ptr<Entity> (*createEntityCallback)(const std::string&, Record&);

private:

  typedef shared_ptr<EntityFactory> entityFactory_type;
  typedef std::map<std::string, createEntityCallback> callback_map;

  // DISABLED

private:

  EntityFactory();                                          // zero-argument constructor
  EntityFactory(const EntityFactory& other);                // copy constructor
  EntityFactory& operator= (const EntityFactory& other);    // assignment operator

  // CREATORS

public:

  ~EntityFactory();      // CAUTION: shared_ptrs normally require a public destructor

  // SINGLE POINT OF ACCESS

public:

  static
  entityFactory_type
  iface();

  // MANIPULATORS

public:

  // NOTE: the term 'entity' in function names implies sub-classes too

  bool                                       // true if successful
  registerEntity                             // used in 'register.cc'
  (const std::string    entityRegn,          // string defined in "register.h"
   createEntityCallback createfn);

  bool                                       // true if previously registered
  unregisterEntity
  (const std::string entityRegn);

  shared_ptr<Entity>
  createEntityBind                           // note 'initialize()' member function
  (const std::string  entityRegn,
   const std::string& entityId,              // enforced unique identifier
   Record&            r)                     // associated record to bind
    throw(std::exception,                    // exception specification
          xeona::empty_wrap,
          xeona::non_registration);

  // INTERNAL DATA

private:

  callback_map    d_callbacks;     // std::map<std::string, createEntityCallback>

  // STATIC DATA

private:

  static entityFactory_type    s_instance;   // pointer to this singleton
  static logga::spLogger       s_logger;     // shared_ptr to single logger object

};

#endif // _FACTORY_H_

//  end of file

