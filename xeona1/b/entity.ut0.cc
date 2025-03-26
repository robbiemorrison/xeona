//  XEONA ENERGY SYSTEMS MODELING ENVIRONMENT
//
//  file-create-name : entity.test.cc
//  file-create-date : Fri 15-Jun-2007 08:52 UTC
//  file-initiator   : Robbie Morrison <robbie@actrix.co.nz>
//
//  file-role        : entity base class plus lazy linking (own sub-classes) / unit test
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
//  $URL: file:///home/robbie/svn-root/xeona/futz/trunk/xeona1/b/entity.ut0.cc $
//
//  GENERAL NOTES FOR THIS FILE
//
//  This is a unit test file and hence not essential to the
//  application proper.

//  LOCAL AND SYSTEM INCLUDES

#include "entity.h"           // unit under test (place early)

#include "../c/recset.h"      // record set support
#include "../a/logger.h"      // standard logging functionality (as required)
#include "../c/smart_ptr.h"   // toggle between TR1 and Boost smart pointers
#include ".././common.h"      // common definitions for project (place last)

#include <iostream>           // standard io
#include <string>             // C++ strings
#include <sstream>            // string-streams

// NOTE: this unit test generates lots of warnings but probably
// not worth amending code to squash them

//  CODE

// ---------------------------------------------------------
//  CLASS           : SubEntity_UT
// ---------------------------------------------------------

class SubEntity_UT :
  public FullEntity
{
public:
  SubEntity_UT
  (const std::string entityId) :
    FullEntity(entityId),
    d_identifier(entityId)
  {
    s_logger->repx(logga::dbug, "constructor call (link version)", d_identifier);
  }

  SubEntity_UT
  (const std::string entityId,
   Record&           record) :
    FullEntity(entityId, record),
    d_identifier(entityId)
  {
    s_logger->repx(logga::dbug, "constructor call", d_identifier);
  }

  virtual void factoryInitialize()
  {
    s_logger->repx(logga::dbug, "initialize call", d_identifier);
  }

private:
  std::string  d_identifier;

};

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

  // ---------------------------------------------------------
  //  test ONE        : if Entity class is concrete
  // ---------------------------------------------------------

  logger->test(1);

  {

#if 0 // 0 = safe, 1 = only if Entity class is concrete (non-abstract)

    Record r;
    r.hackUnitTestRecord("test one");

    // Entity entity0;  // uncomment to test that 'Entity::Entity()' is private

    Entity entity1("unique for entity1", r);
    shared_ptr<Entity> entity2(new Entity("next for entity2 too", r));
    put << "population : " << entity2->census() << "\n";
    logger->putx(logga::dbug, put);
    put << entity1.traverseFullPopulation();
    logger->putx(logga::dbug, put);

#else

    put << "test inactive" << "\n";
    logger->putx(logga::dbug, put);

#endif // test one

  }

  // ---------------------------------------------------------
  //  test TWO        : two sub-classes of Entity defined above
  // ---------------------------------------------------------

  logger->test(2," instance two sub-entities, duly defined in this unit test file");

  {
    Record r;
    r.hackUnitTestRecord("test two");

    SubEntity_UT stack1("zero", r);                         // on stack
    shared_ptr<Entity> smart1(new SubEntity_UT("one", r));  // on heap (smart pointer)
    shared_ptr<Entity> smart2(new SubEntity_UT("two", r));  // on heap

    put << "    " << SubEntity_UT::traverseFullPopulation();
    logger->putx(logga::dbug, put);

    put << "    " << Entity::traverseFullPopulation();
    logger->putx(logga::dbug, put);

    smart1 = smart2;                         // the old 'smart1' is quietly dispatched!

    put << "    " << Entity::traverseFullPopulation();
    logger->putx(logga::dbug, put);
    put << ""                                            << "\n";

    put << "about to 'shared_ptr::reset()' smart2"       << "\n";
    logger->putx(logga::dbug, put);
    smart2.reset();                          // one pointer still exists so same census

    put << ""                                            << "\n";
    put << "    " << Entity::traverseFullPopulation();
    logger->putx(logga::dbug, put);

#if 0 // 0 = safe, 1 = delete conflict with protected destructor

    Entity* dumb = new SubEntity_UT("three", r);  // on heap (raw pointer)
    delete dumb;  // compiler: error: 'virtual Entity::~Entity()' is protected

#endif // 0

  }

  // ---------------------------------------------------------
  //  test THREE      : object initialization
  // ---------------------------------------------------------

  logger->test(3, "object initialization");

  {
    Record r;
    r.hackUnitTestRecord("test three");

    shared_ptr<Entity> smart3(new SubEntity_UT("four", r));
    put << "manual initialization: " << smart3->getIdentifier() << "\n";
    logger->putx(logga::dbug, put);
    smart3->factoryInitialize();
  }

  // ---------------------------------------------------------
  //  test FOUR       : polymorphic streaming
  // ---------------------------------------------------------

  // "dummy output from an entity (not polymorphically overridden as expected)"

  logger->test(4, "about to stream 'SubEntity_UT'");

  {
    Record r;
    r.hackUnitTestRecord("test four");

    shared_ptr<Entity> smart4(new SubEntity_UT("five", r));
    put << *smart4;                          // stream item
    logger->putx(logga::dbug, put);

    put << "\n";
    put << "** note that the \" not overridden\" comment above is okay in this case **"
        << "\n";
    logger->putx(logga::dbug, put);
  }

  // ---------------------------------------------------------
  //  test FIVE       : id uniqueness
  // ---------------------------------------------------------

  logger->test(5, "id uniqueness confirmation");

  {
    Record r;
    r.hackUnitTestRecord("test five");

    shared_ptr<Entity> smart5(new SubEntity_UT("me-first", r));
    shared_ptr<Entity> smart6(new SubEntity_UT("me-first", r));  // non-unique id
  }

  // ---------------------------------------------------------
  //  test SIX        : "link" entities and "lazy" linking
  // ---------------------------------------------------------

  logger->test(6, "\"link\" entities and  \"lazy\" linking");

  {
    Record r;
    r.hackUnitTestRecord("test six");

    shared_ptr<Entity> full(new SubEntity_UT("abc", r));
    full->processFabricatedPtr(full);             // approximate the entity factory func
    Entity::makeLinkEntity<SubEntity_UT>("abc");  // approximate 'Record::lazyLink'
    Entity::linkAll();
  }

  // ---------------------------------------------------------
  //  test SEVEN      : try to confirm identifier
  // ---------------------------------------------------------

  // for some reason, identifier "abc" is present and the
  // identifier for 'spShouldFind' is non-existent -- this is
  // clearly a bug (perhaps within 'SubEntity_UT') but I have not
  // bothered to track it down

  logger->test(7, "try to confirm identifier");

  {
    Record r;
    r.hackUnitTestRecord("test seven");

    shared_ptr<Entity> spShouldFind(new SubEntity_UT("should-find", r));

    put << std::boolalpha;
    put << "  hunt 'abc'         : " << Entity::confirmIdentifier("abc")         << "\n";
    put << "  hunt 'should-find' : " << Entity::confirmIdentifier("should-find") << "\n";
    put << "  hunt 'mia'         : " << Entity::confirmIdentifier("mia")         << "\n";
    logger->addSmartBlank(logga::dbug);
    logger->putx(logga::dbug, put);
  }

  // ---------------------------------------------------------
  //  test EIGHT      : identifier concating
  // ---------------------------------------------------------

  // trial preceding gateway design with "embedded entity composite"

  logger->test(8, "identifier concating");

  {
    Record r;
    r.hackUnitTestRecord("test eight");

    const std::string baseIdentifier = "passedthru";
    SubEntity_UT stack(baseIdentifier + "-fixed", r);  // note std::string concatenation

    put << "  identifier : " << stack.getIdentifier() << "\n";
    logger->putx(logga::dbug, put);
  }

  // ---------------------------------------------------------
  //  test NINE       : putx entity id
  // ---------------------------------------------------------

  logger->test(9, "putx entity id");

  {
    shared_ptr<Entity> entity;               // null entity
    xeona::putxId(entity, "null entity");
  }

  // ---------------------------------
  //  HOUSEKEEPING
  // ---------------------------------

  logger->test(0);                           // test end

  logger->repx(logga::info, "end of main", "~~~~~~~~~~");
  return xeona::exit_success;

} // main function

//  end of file

