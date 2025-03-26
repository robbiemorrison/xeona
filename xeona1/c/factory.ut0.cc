//  XEONA ENERGY SYSTEMS MODELING ENVIRONMENT
//
//  file-create-name : factory.test.cc
//  file-create-date : Tue 22-May-2007 12:59 UTC
//  file-initiator   : Robbie Morrison <robbie@actrix.co.nz>
//
//  file-role        : entity factory (using local 'createUnit' function) / unit test
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
//  $URL: file:///home/robbie/svn-root/xeona/futz/trunk/xeona1/c/factory.ut0.cc $
//
//  GENERAL NOTES FOR THIS FILE
//
//  This is a unit test file and hence not essential to the
//  application proper.

//  LOCAL AND SYSTEM INCLUDES

#include "factory.h"          // unit under test (place early)

#include "../a/logger.h"      // standard logging functionality (as required)
#include "../c/smart_ptr.h"   // toggle between Boost and TR1 smart pointers
#include ".././common.h"      // common definitions for project (place last)

//  CODE

//  *** WARNING: these two Entity and Unit classes are LOCAL to
//  *** this unit tester and are NOT used in xeona proper

//  *** WARNING: this Record class is for unit testing only

// ---------------------------------------------------------
//  CLASS           : Record (fake for this unit only)
// ---------------------------------------------------------
//
// this class definition exists to save adding 'recset.cc' and
// dependencies to the SOURCES list

class Record
{
public:
  Record()                                   // genuine Record is zero-argument too
  {
    logga::spLogger logger = logga::ptrLogStream();
    logger->repx(logga::kill, "fake Record class for test purposes", "");
  }
};

// ---------------------------------------------------------
//  CLASS           : Entity, Unit
// ---------------------------------------------------------

// The preprocessor hash-include "if 1" may cause SEGFAULTing
// with messages like "dbug: negative interval calculated:
// -130214124".  "if 0" enables Entity and Unit definitions with
// logging, which -- as a side-effect -- means the Logger
// singleton will probably outlast the EntityFactory singleton.
// See "factory.h" for more discussion.

#if 0 // 0 = safe

class Entity { public: virtual ~Entity() {}; };
class Unit : public Entity {};

#else

class Entity
{
public: Entity() { s_logger->repx(logga::dbug, "constructor call", ""); }
public: virtual ~Entity() {}                 // keep compiler quiet
private: static logga::spLogger  s_logger;
};

class Unit : public Entity
{
public: Unit() { s_logger->repx(logga::dbug, "constructor call", ""); }
private: static logga::spLogger  s_logger;
};

logga::spLogger
Entity::s_logger = logga::ptrLogStream();    // bind logger on definition

logga::spLogger
Unit::s_logger = logga::ptrLogStream();      // bind logger on definition

#endif // 0

// ---------------------------------------------------------
//  FREE FUNCTION   : ::createUnit()
// ---------------------------------------------------------

namespace
{
  shared_ptr<Entity>
  createUnit
  (const std::string& entityId,              // not used here
   Record&            r)                     // not used here
  {
    logga::spLogger logger = logga::ptrLogStream();
    logger->repx(logga::dbug, "creating a Unit", "");

    shared_ptr<Entity> smartptr(new Unit());   // CAUTION: local definition
    return smartptr;
  }

} // unnamed namespace

// ---------------------------------------------------------
//  MAIN FUNCTION   : main
// ---------------------------------------------------------

int
main(int argc, char* argv[])
{
  logga::spLogger logger = logga::ptrLogStream();  // main function logger
  logger->repx(logga::info, "beginning of main", "~~~~~~~~~~");
  logger->addSmartBlank();
  logger->setReportLevel(logga::adhc);       // maximum reporting

  // PRELIMINARY

  std::ostringstream put;
  Record r;                                  // generally available fake record

  // ---------------------------------------------------------
  // test ONE         : register Unit constructor
  // ---------------------------------------------------------

  logger->test(1);

  // the following code is sensitive to placement -- it did not
  // work when positioned prior to main()

  EntityFactory::iface()->registerEntity("Unit", &::createUnit);

  // note too that these 'registerEntity' calls are normally
  // placed in 'register.cc' and are called en masse via a single
  // invocation of 'registerEntyCreators'

  // ---------------------------------------------------------
  // test TWO         : request good Unit creation
  // ---------------------------------------------------------

  logger->test(2);

  // note that the (smart) pointer is of type 'Entity' but now
  // holds a specialization, namely 'Unit'

  shared_ptr<Entity> unitOne
    = EntityFactory::iface()
    ->createEntityBind("Unit", "real-unit", r);

  // ---------------------------------------------------------
  // test THREE       : request bad Entity creation
  // ---------------------------------------------------------

#if 0 // 0 = safe, 1 = compile-time error: "BUGGY" is not registered

  logger->test(3);

  shared_ptr<Entity> entityBuggy
    = EntityFactory::iface()
    ->createEntityBind("BUGGY", "buggy-entity", r);

#endif

  // ---------------------------------
  //  HOUSEKEEPING
  // ---------------------------------

  logger->test(0);                           // test end

  logger->repx(logga::info, "end of main", "~~~~~~~~~~");
  return xeona::exit_success;

} // main function

//  end of file

