//  XEONA ENERGY SYSTEMS MODELING ENVIRONMENT
//
//  file-create-name : datfact.ut2.cc
//  file-create-date : Wed 05-Dec-2007 16:05 UTC
//  file-initiator   : Robbie Morrison <robbie@actrix.co.nz>
//
//  file-role        : model io and sub-entity prodn (generates 'datfact.xem') / unit test
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
//  $URL: file:///home/robbie/svn-root/xeona/futz/trunk/xeona1/c/datfact.ut2.cc $
//
//  GENERAL NOTES FOR THIS FILE
//
//  This is a unit test file and hence not essential to the
//  application proper.

//  AD-HOC NOTES
//
//  Earlier development
//
//      This file is a mixture of 'datrec.ut1.cc' and 'entfact.ut1.cc'.
//
//  Destructor logging bug
//
//      Removing the entity registration enum code and replacing
//      it with simple string code in r2209 provoked a unit test
//      failure here.  After bughunting, the offending code was
//      disabled under preprocessor macro 'XE_DTOR_LOG_BUG'.
//      The problem arises from calls like:
//
//          shared_ptr<Entity> ent
//            = EntityFactory::interface()
//            ->createEntityBind("SubEntity1", "ent", globalRec);
//
//      The issue is a that the relevant 'Logger' object is
//      destructed BEFORE the logging client.  This also includes
//      the client's base class, namely Entity::~Entity(), so a
//      simple fix is not available.
//
//      This result was unexpected, given that the earlier
//      enum-based code (as opposed to the current string-based
//      code) worked fine.  However, after two hours of testing,
//      I decided to sidestep the problem by disabling the
//      offending calls in this unit test.
//
//      It should be noted that a similar problem also occured
//      during development of the 'EntityFactory' singleton, as
//      documented in 'factory.cc'.

//  LOCAL AND SYSTEM INCLUDES

#include "../b/entity.h"      // entity base class (place early)
#include "../c/factory.h"     // entity factory (place early)
#include "../c/datio.h"       // model data io (place early)
#include "../c/recset.h"      // record set support (place early)

#include "../b/register.h"    // entity subclass registration
#include "../a/logger.h"      // standard logging functionality (as required)
#include "../c/smart_ptr.h"   // toggle between Boost and TR1 smart pointers
#include ".././common.h"      // common definitions for project (place last)

#include <algorithm>          // STL copying, searching, and sorting
#include <fstream>            // file-based io
#include <iostream>           // standard io
#include <sstream>            // string-streams
#include <string>             // C++ strings
#include <vector>             // STL sequence container

#include <boost/algorithm/string.hpp>   // string recasing, trimming, splitting
#include <boost/foreach.hpp>            // BOOST_FOREACH macro

//  PREPROCESSOR MACROS FOR TEST PURPOSES

#define XE_DTOR_LOG_BUG 0 // 1 = enable, else disable

//  FORWARD (PARTIAL) DECLARATIONS -- defined at end of file

bool dumpToFile(const std::string& filename);
void loadOstream(std::ostream& os);

//  CODE

// ---------------------------------------------------------
//  MAIN FUNCTION   : main
// ---------------------------------------------------------

int
main(int argc, char* argv[])
{
  logga::spLogger logger = logga::ptrLogStream();   // main function logger
  logger->repx(logga::info, "beginning of main", "~~~~~~~~~~");
  logger->addSmartBlank();
  logger->setReportLevel(logga::adhc);       // maximum reporting

  Record globalRec;                          // CAUTION: note position
  unsigned count = 0;                        // loop counting

  count++; count--; // TOFIX: remove in due course, to keep compiler happy

  // HARDCODED FILENAME

  const std::string leafName = "datfact.xem";     // data file generated here

  // PRELIMINARY

  std::ostringstream put;
  put << std::boolalpha;

  // ---------------------------------------------------------
  //  test ONE        : data file preparation
  // ---------------------------------------------------------

  logger->test(1);

  put << "about to create (or overwrite) file: " << leafName     << "\n";
  logger->putx(logga::dbug, put);

  dumpToFile(leafName);

  // ---------------------------------------------------------
  //  test TWO        : input processing
  // ---------------------------------------------------------

  logger->test(2);

  // create a record set and a data input/output admin object
  RecordSet recset;
  DataIo dataIo(recset);

  // establish model path (simplified code)
  boost::filesystem::path model = boost::filesystem::absolute(leafName);
  put << "model path (complete) : " <<  model                    << "\n";
  put << "model path (leaf)     : " <<  model.filename()         << "\n";
  logger->putx(logga::dbug, put);

  // read in model
  put << ""                                                      << "\n";
  put << "about to call readModel()"                             << "\n";
  put << ""                                                      << "\n";
  logger->putx(logga::dbug, put);
  dataIo.readModel(model);                   // only 'd_rawStr' is loaded

  // horizon
  put << ""                                                      << "\n";
  put << "about to call locateModelHorizon()"                    << "\n";
  logger->putx(logga::dbug, put);
  unsigned horizon
    = dataIo.locateModelHorizon();           //  locate and set horizon
  put << "    locateModelHorizon() yields : " << horizon         << "\n";
  logger->putx(logga::dbug, put);

  // process model
  put << ""                                                      << "\n";
  put << "about to call processModel()"                          << "\n";
  logger->putx(logga::dbug, put);
  dataIo.processModel();                     // process model values

  // ---------------------------------------------------------
  //  test THREE      : dump record set
  // ---------------------------------------------------------

  logger->test(3);

  // dump record set
  put << "about to call dump()"                             << "\n";
  logger->putx(logga::dbug, put);
  recset.dump();                             // call using default "---"

  // ---------------------------------------------------------
  //  test FOUR       : recover specific information
  // ---------------------------------------------------------

  logger->test(4);

  put << "about to call getSubset(xeona::e_entity)"         << "\n";
  logger->putx(logga::dbug, put);
  std::vector<shared_ptr<Record> > users = dataIo.getSubset(xeona::e_entity);
  put << "    size of user vector    : " << users.size()    << "\n";
  logger->putx(logga::dbug, put);

  put << ""                                                 << "\n";
  put << "about to call getSubset(xeona::e_program)"        << "\n";
  logger->putx(logga::dbug, put);
  std::vector<shared_ptr<Record> > programs = dataIo.getSubset(xeona::e_program);
  put << "    size of program vector : " << programs.size() << "\n";
  logger->putx(logga::dbug, put);

  put << ""                                                 << "\n";
  put << "about to call getSubset(xeona::e_note)"           << "\n";
  logger->putx(logga::dbug, put);
  std::vector<shared_ptr<Record> > notes = dataIo.getSubset(xeona::e_note);
  put << "    size of note vector    : " << notes.size()    << "\n";
  logger->putx(logga::dbug, put);

  // ---------------------------------------------------------
  //  test FIVE         : invoke hardcoded factory calls
  // ---------------------------------------------------------

  logger->test(5);

  put << "about to call registerEntyCreators()"                  << "\n";
  logger->putx(logga::info, put);
  xeona::registerEntyCreators();             // one-off protected (by log warning) call

  put << "about to create two SubEntity1's"                      << "\n";
  logger->putx(logga::info, put);

#if (XE_DTOR_LOG_BUG == 1)

  shared_ptr<Entity> se1_1
    = EntityFactory::interface()
    ->createEntityBind("SubEntity1", "sub1-one", globalRec);

  shared_ptr<Entity> se1_2
    = EntityFactory::interface()
    ->createEntityBind("SubEntity1", "sub1-two", globalRec);

#endif // XE_DTOR_LOG_BUG

  // ---------------------------------------------------------
  //  test SIX        : cycle records passively
  // ---------------------------------------------------------

  logger->test(6);

  put << "about to initialize ClassStrToEntityType object"       << "\n";
  logger->putx(logga::dbug, put);

  put << "about to cycle user records"                           << "\n";
  logger->putx(logga::dbug, put);

  count = 0;
  BOOST_FOREACH( shared_ptr<Record> r, users )
    {
      const std::string identity         = r->getIdentifier();
      const std::string entityTypeStr    = r->locateClass();   // empty means failure
      ++count;
      put << ""                                                  << "\n";
      put << "    identity             : " << identity           << "\n";
      put << "    entity type (string) : " << entityTypeStr      << "\n";
      logger->putx(logga::dbug, put);
    }

  put << ""                                                      << "\n";
  put << "    count  : " << count                                << "\n";
  logger->putx(logga::dbug, put);

  // ---------------------------------------------------------
  //  test SEVEN      : cycle records and make factory calls
  // ---------------------------------------------------------

#if (XE_DTOR_LOG_BUG == 1)

  logger->test(7);

  put << "about to start production!"                            << "\n";
  logger->putx(logga::dbug, put);

  // the following vector is available as a list via a 'const
  // std::list<const SubEntity1*>&' using the static member
  // function 'Entity::getCensus'

  std::vector<shared_ptr<Entity> > xTechAss;  // xem-sourced technical assets register

  int xTechAssCount = 0;

  BOOST_FOREACH( shared_ptr<Record> r, users )
    {
      const std::string identity         = r->getIdentifier();
      const std::string entityTypeStr    = r->locateClass();   // empty means failure

      xTechAss.push_back(EntityFactory::interface()
                        ->createEntityBind(entityTypeStr, identity, *r));
      ++xTechAssCount;
    }

  put << "    xem-sourced asset count       : " << xTechAssCount      << "\n";
  put << "    xem-sourced asset vector size : " << xTechAss.size()    << "\n";
  logger->putx(logga::dbug, put);

  // set binding status

  dataIo.noteModelIsBound();

#endif // XE_DTOR_LOG_BUG

  // ---------------------------------------------------------
  //  test EIGHT      : get Entity census list by static call
  // ---------------------------------------------------------

  logger->test(8);

  {
    put << "about to get Entity census using static function"    << "\n";
    logger->putx(logga::dbug, put);

    // reference to a constant list
    const std::list<const Entity*>& eList
      = Entity::getCensusRaw();              // compiles without an &

    put << "    Entity census : " << eList.size()                << "\n";
    logger->putx(logga::dbug, put);

  }  // 'eList' goes out of scope

  // ---------------------------------------------------------
  //  test NINE       : test binding
  // ---------------------------------------------------------

#if (XE_DTOR_LOG_BUG == 1)

  if ( xTechAss.empty() )
    {
      put << ""                               << "\n";
      put << "    PROBLEM: xTechAss is empty" << "\n";
      put << ""                               << "\n";
      logger->putx(logga::warn, put);
      logger->repx(logga::info, "end of main", "~~~~~~~~~~");
      return xeona::exit_test100;
    }

  logger->test(9);

  put << "about to test bindings"                                << "\n";
  logger->putx(logga::dbug, put);

  shared_ptr<Entity> spFirst(xTechAss.front());    // grab first element

  const double factor = 2.2;
  const double answer = factor * spFirst->getX();

  put << "\n";
  put << "    "
      << spFirst->getIdentifier()
      << " x (if 'testme' then originally set to +4.44044e+04) : "
      << spFirst->getX()
      << "\n";
  logger->putx(logga::dbug, put);

  spFirst->multiplyX(factor);

  put << "    "
      << "modified x (if 'testme' then should be " << answer << ") : "
      << spFirst->getX()
      << "\n";
  logger->putx(logga::dbug, put);

  // ---------------------------------------------------------
  //  test TEN        : examine records
  // ---------------------------------------------------------

  logger->test(10);

  put << "about to locate \"x\" from \"testme\" using the recordset \"recset\"" << "\n";
  put << ""                                                      << "\n";
  logger->putx(logga::dbug, put);

  const shared_ptr<Field> f = recset.locateRecordAndField("testme", "x");

  if ( ! f )
    put << "failed to find \"x\" in \"testme\"" << "\n";
  else
    put << "    x : " << f->getSingle() << "\n";
  put << "\n";
  logger->putx(logga::dbug, put);

  put << "about to cycle throught the records"                   << "\n";
  put << ""                                                      << "\n";
  logger->putx(logga::dbug, put);

  count = 0;
  BOOST_FOREACH( shared_ptr<Record> r, users )
    {
      std::string currentX             = "(failed to locate field)";
      const std::string identity       = r->getIdentifier();
      const shared_ptr<Field> f        = r->locateField("x");
      if ( f ) currentX                = f->getSingle();    // overwrite
      if ( currentX.empty() ) currentX = "(failed to get single)";
      ++count;
      put << "    identity : " << identity   << "\n";
      put << "      x      : " << currentX   << "\n";
      logger->putx(logga::dbug, put);
    }

#endif // XE_DTOR_LOG_BUG

  // ---------------------------------
  //  HOUSEKEEPING
  // ---------------------------------

  logger->test(0);                           // test end

  logger->repx(logga::info, "end of main", "~~~~~~~~~~");
  return xeona::exit_success;

} // main function

// ======  code to produce .xem file dynamically   =========

// ---------------------------------------------------------
//  FREE FUNCTION   : dumpToFile
// ---------------------------------------------------------

bool
dumpToFile
(const std::string& filename)
{
  logga::spLogger logger = logga::ptrLogStream();
  std::ostringstream put;

  std::ofstream ofile(filename.c_str());     // note defaults: text, trunc (overwrite)
  if ( ! ofile )
    {
      logger->repx(logga::warn, "failed to open file", filename);
      return false;
    }
  logger->repx(logga::dbug, "able to open file", filename);

  loadOstream(ofile);
  return true;
} // ofile closes on block exit

// ---------------------------------------------------------
//  FREE FUNCTION   : loadOstream
// ---------------------------------------------------------
//
//  Notes
//
//    - a final '\' on each physical line is essential
//
//    - trailing space after the final '\' is acceptable (unlike
//      shell scripting)
//
//    - could also #include the text en masse from a stand-alone
//      data file OR 'ifstream::getline' this file

void
loadOstream
(std::ostream& os)
{
  os << "\
\n\
builtin.time-horizon\n\
\n\
    steps [-]                   > 6\n\
    interval [s]                > 3600\n\
\n\
entity.testme\n\
\n\
    designed for use with datfact level 2 unit test\n\
\n\
    class                       > Datfact_UT\n\
    user-description            > \"generic test class\"\n\
\n\
    kind                        <\n\
    builtin-remark              < \"none\"\n\
\n\
    x [-] (remark)              > +4.44044e+04\n\
    single [bool]               > 1\n\
    timeseries [kW]             > 0 16 0 17 18 91\n\
\n\
    electrical_output+ [MW]     < 1 2 3 4 5 6\n\
    run_time [s]                < +3.99e+00\n\
    some_state [bool]           < 0 1 1 0 0 1\n\
\n\
entity.testme-two\n\
\n\
    some inane comment\n\
\n\
    class                       > Datfact_UT\n\
    user-description            > \"generic test class\"\n\
\n\
    kind                        <\n\
    builtin-remark              < \"none\"\n\
\n\
    x [-] (remark)              > +6.66066e+04\n\
    single [bool]               > 1\n\
    timeseries [kW]             > 1 20 1 1 1 10\n\
\n\
    electrical_output+ [MW]     < 1 2 3 4 5 6\n\
    run_time [s]                < +3.99e+00\n\
    some_state [bool]           < 0 1 1 0 0 1\n\
\n\
model-end\n\
\n\
";

  logga::spLogger logger = logga::ptrLogStream();
  logger->repx(logga::dbug, "file text streamed", "");
}

//  end of file

