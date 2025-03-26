//  XEONA ENERGY SYSTEMS MODELING ENVIRONMENT
//
//  file-create-name : tests.cc
//  file-create-date : Thu 24-Jan-2008 15:16 UTC
//  file-initiator   : Robbie Morrison <robbie@actrix.co.nz>
//
//  file-role        : test sub-entities for use with --inbuilt and such / implementation
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
//  $URL: file:///home/robbie/svn-root/xeona/futz/trunk/xeona1/b/tests.cc $

//  LOCAL AND SYSTEM INCLUDES

#include "tests.h"            // companion header for this file (place first)

#include "../c/conex.h"       // create and connect block interfaces
#include "../c/recset.h"      // record set support
#include "../a/logger.h"      // standard logging functionality (as required)
#include "../c/smart_ptr.h"   // toggle between Boost and TR1 smart pointers
#include ".././common.h"      // common definitions for project (place last)

#include <boost/foreach.hpp>  // BOOST_FOREACH iteration macro

//  CODE

// ---------------------------------------------------------
//  CLASS           : TestEntity0
// ---------------------------------------------------------
//  Purpose      : a representative first sub-class of FullEntity
//  Status       : complete
// ---------------------------------------------------------

// CREATORS

TestEntity0::TestEntity0
(const std::string entityId,                 // enforced unique identifier
 Record&           record) :
  FullEntity(entityId, record)
{
  s_logger->repx(logga::dbug, "constructor call", "");
}

TestEntity0::~TestEntity0()
{
  s_logger->repx(logga::xtra, "destructor call", "");
}

void
TestEntity0::factoryInitialize()
{
  s_logger->repx(logga::xtra, "TestEntity0 instance initialization", "");
}

// ---------------------------------------------------------
//  CLASS           : TestEntity1
// ---------------------------------------------------------
//  Purpose      : a representative first sub-class of FullEntity
//  Status       : complete
//
//  Design notes
//
//      See notes for the superclass Entity.
//
//  CAUTION -- creating and initializing TestEntity1 objects
//
//      A TestEntity1 object MUST be instantiated using a shared
//      pointer, either:
//
//          shared_ptr<Entity>
//          shared_ptr<TestEntity1>
//
//      This is because the 'init' member function generates its
//      own shared pointer using function 'shared_from_this'.
//      The restriction is unlikely to be an issue outside of
//      testing, because the entity factory generates shared
//      pointers.
//
//      By way of background, 'shared_from_this' requires at
//      least one other shared pointer to be in existence.  The
//      Boost documentation, found at:
//
//          file:///usr/share/doc/libboost-doc/HTML/libs/
//          smart_ptr/enable_shared_from_this.html
//
//      says that the 'shared_from_this' member function
//      (actually there are two, depending on constness)
//      "requires: .. there must exist at least one shared_ptr
//      instance .. that owns .. [the underlying object]"
//
//      If a TestEntity1 object is created without producing a
//      shared pointer, calling 'init' will result in the
//      following kind of run-time error:
//
// 226  entity.cc   initialize   00  00.0000s   dbug   TestEntity1 instance initialization
// terminate called after throwing an instance of 'std::tr1::bad_weak_ptr'
//   what():  tr1::bad_weak_ptr
// /hom/.../scripts/mach: line 1308: 30080 Aborted (core dumped) ./entity
//
// ---------------------------------------------------------

//  STATIC DEFINITIONS

int TestEntity1::s_ctorCount = 0;                      // with explicit initialization
std::list<const TestEntity1*> TestEntity1::s_census;   // without explicit initialization

// CREATORS

TestEntity1::TestEntity1
(const std::string entityId,                 // enforced unique identifier
 Record&           record) :
  FullEntity(entityId, record),

  d_userDescription(record.tieSingle<std::string>("user-description")),
  d_x(record.tieSingle<double>("x")),
  d_single(record.tieSingle<bool>("single")),
  d_timeseries(record.tieTimeseries<int>("timeseries")),

  d_electricalOutput(record.tieTimeseries<int>("electrical-output")),
  d_runTime(record.tieSingle<double>("run-time")),
  d_someState(record.tieTimeseries<bool>("some-state"))
{
  s_logger->repx(logga::dbug, "constructor call", "");
  TestEntity1::s_census.push_back(this);      // insert copy of pointer in instance census
  s_logger->repx(logga::dbug, "current population", popnSize());

  d_builtinRemark = "beta";

  // a little modification to fake some real recalculation
  const unsigned steps = Entity::getHorizonSteps();
  d_electricalOutput->at(steps-1) = 0;
}

TestEntity1::~TestEntity1()
{
  s_logger->repx(logga::xtra, "destructor call", "");
  TestEntity1::s_census.remove(this);
}

// for future reference (above), note the safe census code was:
// TestEntity1::s_safeCensus.remove(static_pointer_cast<TestEntity1>(shared_from_this()));

void
TestEntity1::factoryInitialize()
{
  // initial reporting
  s_logger->repx(logga::xtra, "TestEntity1 instance initialization", "");

  // a little modification to fake some real recalculation
  const unsigned steps = Entity::getHorizonSteps();
  d_builtinRemark = "some built in remark";
  for ( unsigned i = 0; i < steps; ++i) d_electricalOutput->at(i) = i * 2;
  d_runTime = 123456;                       // +1.23e+05
  bool load = true;
  if ( getIdentifier() == "testme-two") load = false;
  for ( unsigned i = 0; i < steps; ++i) d_someState->at(i) = load;
  d_someState->resize(steps, true);
}

// UTILITY FUNCTIONS

// STREAM INSERTION SUPPORT

std::ostream&
TestEntity1::streamOut                       // support for overloaded operator<<
(std::ostream& os) const
{
  std::ios_base::fmtflags prior = os.flags();
  os << "    dummy output from a derived entity" << "\n";
  os.flags(prior);
  return os;
}

// STATIC ACCESSORS

const std::list<const TestEntity1*>&         // return a const reference
TestEntity1::getCensus()
{
  return s_census;
}

int
TestEntity1::popnSize()
{
  return s_census.size();
}

std::string                                  // returns string for test purposes
TestEntity1::traverseFullPopulation()
{
  std::ostringstream ss;
  ss << "traversal : |";
  BOOST_FOREACH( const TestEntity1* s, s_census )
    {
      ss << " " << s->getIdentifier() << " |";
    }
  ss << "\n";
  return ss.str();
}

// ---------------------------------------------------------
//  CLASS           : TestEntity2
// ---------------------------------------------------------
//  Purpose      : a representative first sub-class of FullEntity
//  Status       : complete
//
//  Design notes
//
//      Contains a 'Socket'
//
// ---------------------------------------------------------

//  STATIC DEFINITIONS

// CREATORS

TestEntity2::TestEntity2
(const std::string entityId,                 // enforced unique identifier
 Record&           record) :
  FullEntity(entityId, record),
  d_socketWork(Socket<CmWork>::create(entityId, "one", "commodity-id"))
{
  s_logger->repx(logga::dbug, "constructor call", "");
  d_builtinRemark = "beta / with 'socket'";
}

TestEntity2::~TestEntity2()
{
  s_logger->repx(logga::xtra, "destructor call", "");
}

void
TestEntity2::factoryInitialize()
{
  s_logger->repx(logga::xtra, "TestEntity2 instance initialization", "");
}

//  end of file

