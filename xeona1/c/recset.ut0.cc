//  XEONA ENERGY SYSTEMS MODELING ENVIRONMENT
//
//  file-create-name : recset.ut0.cc
//  file-create-date : Tue 09-Oct-2007 17:14 UTC
//  file-initiator   : Robbie Morrison <robbie@actrix.co.nz>
//
//  file-role        : records and fields and also record-sets / unit test
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
//  $URL: file:///home/robbie/svn-root/xeona/futz/trunk/xeona1/c/recset.ut0.cc $
//
//  GENERAL NOTES FOR THIS FILE
//
//  This is a unit test file and hence not essential to the
//  application proper.

//  AD-HOC NOTES
//
//  The last time this test suite ran in its entirety was r1416.
//  Two blocks of code have been commented out to get it run
//  subsequently.

//  LOCAL AND SYSTEM INCLUDES

#include "recset.h"           // unit under test (place early)

#include "../a/logger.h"      // run-time logging functionality (as required)
#include "../c/smart_ptr.h"   // switch easily between TR1 and Boost smart pointers
#include ".././common.h"      // common definitions for project (place last)

#include <iomanip>            // setw() and family
#include <iostream>           // standard io
#include <string>             // C++ strings
#include <vector>             // STL sequence container

//  CODE

// ---------------------------------------------------------
//  FREE FUNCTION   : unittest::unlexTestSingle
// ---------------------------------------------------------

// unittest namespace required because one or more friendships
// are granted within 'recset.h'

namespace unittest
{
  template <typename T>
  void
  unlexTestSingle
  (const T input)
  {
    logga::spLogger logger = logga::ptrLogStream();
    std::ostringstream put;
    put << "    single (as original) : " << std::setw(30) << std::left << input;
    logger->putx(logga::dbug, put);
    single_type single = wrap<T>(input);     // CAUTION: cannot stream a single_type
    shared_ptr<Field> f(new Field());
    f->unlexSingle(single);                  // this function is accessed via friendship
    const std::vector<std::string> cvec = f->getSplitStr(); // returns const reference
    put << " (as unlex)    : " << cvec.front() << "\n";
    logger->putx(logga::dbug, put);
  }

} // unittest namespace

// ---------------------------------------------------------
//  CLASS           : Entity_UT
// ---------------------------------------------------------

class Entity_UT                              // represents simulation 'things'
{
public:
  explicit                                        // explicit for testing
  Entity_UT(Record& record) :                     // CAUTION: pass-by-reference necessary
    d_record(record),                             // for testing purposes
    d_coeff(record.tieSingle<double>("coeff")),   // intuitive syntax
    d_count(record.tieTimeseries<int>("count")),  // intuitive syntax
    d_local(0)
  {
    report();
  }

  void modifySingle(double d)
  {
    d_coeff = d;                             // normal syntax (no pointer derefs)
    report();                                // display this modification
  }

  void modifyTimeseries(int i)
  {
    d_count->front() = i;                    // is legal C++, front() returns a reference
    report();
  }

  void report()
  {
    const shared_ptr<Field> fCoeff     = d_record.locateField("coeff");
    const shared_ptr<Field> fCount     = d_record.locateField("count");
    const std::vector<std::string> vec = fCount->getTimeseries();

    logga::spLogger logger = logga::ptrLogStream();
    std::ostringstream put;
    put << "    Entity   : coeff : " << d_coeff             << "\n";
    put << "    Record   : coeff : " << fCoeff->getSingle() << "\n";
    put << "    Entity   : count : " << d_count->front()    << "\n";
    put << "    Record   : count : " << vec.front()         << "\n";
    logger->putx(logga::dbug, put);
  }

private:
  Record&                          d_record; // not strictly necessary
  double&                          d_coeff;  // watched variable
  shared_ptr<std::vector<int> >    d_count;  // watched pointer
  int                              d_local;  // ignored variable
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

  std::ostringstream put;

  // ---------------------------------------------------------
  //  test ONE        : trivial construction
  // ---------------------------------------------------------

  logger->test(1, "trivial construction");

  {
    RecordSet recset;
    Record record;
    Field field;
  }

  // ---------------------------------------------------------
  //  test TWO        : field trials
  // ---------------------------------------------------------

  logger->test(2, "field trials");

  {
    logga::Rank prior = logger->setReportLevel(logga::dbug);     // reduce reporting

    shared_ptr<Field> f1(new Field());

    // MANIPULATORS

    f1->addName("my name");                  // note the whitespace
    f1->addKind(xeona::e_input);
    f1->addEnabled(true);
    f1->addUnits("kg");
    f1->addRemark("some remark");

    std::string value = "123.4567";
    std::vector<std::string> vec;
    vec.push_back(value);

    f1->addRawStr(value);
    f1->addSplitStr(vec);

    // ACCESSORS

    put << std::boolalpha;
    put << "    name    : " << f1->getName()   << "\n";
    put << "    dataStr : " << f1->getRawStr() << "\n";
    put << "    count   : " << f1->getCount()  << "\n";
    put << "    empty   : " << f1->isEmpty()   << "\n";
    logger->putx(logga::dbug, put);

    const std::vector<std::string> constvec = f1->getSplitStr();

    // RECORD ACCESSIBLE FEATURES -- requires TEMPORARY 'public' access

    shared_ptr<std::vector<double> > dvec(new std::vector<double>());
    dvec->push_back(234.567);
    dvec->push_back(345.678);

    f1->lexValue<double>(dvec);  // template argument now set explicitly
    f1->lexValue(dvec);          // template argument (double) is deduced by compiler

    put << "    dvec size : " << dvec->size() << "\n";
    if ( dvec->size() > 0 )
      put << "    dvec val  : " << (*dvec)[0] << "\n";
    put << "\n";
    logger->putx(logga::dbug, put);

    // an explicit template argument overrides implicit instantiation

    unittest::unlexTestSingle(20);
    unittest::unlexTestSingle<double>(40);   // 40 is duly treated as a double
    unittest::unlexTestSingle(false);
    unittest::unlexTestSingle<tribool>(indeterminate);
    unittest::unlexTestSingle<std::string>("using ::unlexTestSingle"); // [1]

    // [1] template argument necessary in this case

//      shared_ptr<std::vector<int> > spInts(new std::vector<int>(10, 2)); // ten twos
//      timeseries_type tst = spInts;            // store as variant
//      f1->unlexTimeseries(tst);
//      const std::vector<std::string> splitstr
//        = f1->getSplitStr();                   // returns const reference
//      put << "    d_splitStr[9] : " << splitstr[9] << "\n";
//      logger->putx(logga::dbug, put);

    logger->setReportLevel(prior);           // return to maximum reporting
  }

  // ---------------------------------------------------------
  //  test THREE      : scope tests
  // ---------------------------------------------------------

  logger->test(3, "undertaking scope test with shared_ptr smart pointer");

  {
    shared_ptr<Record> r1(new Record());
  }

  // ---------------------------------------------------------
  //  test FOUR       : wrap tests
  // ---------------------------------------------------------

  logger->test(4, "wrap tests");

  {
    int init = 10;
    wrap<int> w(init);
    int& r = w;
    w++;
    r++;
    put << "    w          : " << w << "\n";
    put << "    r          : " << r << "\n";
    logger->putx(logga::dbug, put);

#if 0 // 0 = safe, 1 = check warnings from class 'wrap'

  wrap<double> pox;
  put << pox << "\n";  // should warn and then seg-fault!
  logger->putx(logga::dbug, put);

//  ** about to assign from an empty (default constructed) wrap object
//     will probably seg-fault on the next statement
//     check entity class member initialization code

#endif // 0

  }

  // ---------------------------------------------------------
  //  test FIVE       : entity binding
  // ---------------------------------------------------------

  logger->test(5, "entity bindings");

  std::ios::fmtflags prior = put.flags();
  put << std::fixed << std::setprecision(2);

  {
    put << "creating single data" << "\n";
    logger->putx(logga::dbug, put);
    std::string fSingle = "555.5555";        // sole "coeff" element
    std::vector<std::string> fSingleVec;
    fSingleVec.push_back(fSingle);

    put << "creating timeseries data" << "\n";
    logger->putx(logga::dbug, put);
    std::string timeseriesElement;
    timeseriesElement= "2.0";                // "2.0" will not numeric cast
    timeseriesElement= "2";                  // "2" will numeric cast
    std::vector<std::string> fTimeseriesVec(10, timeseriesElement);

    put << "creating a recset" << "\n";
    logger->putx(logga::dbug, put);
    shared_ptr<RecordSet>  rst(new RecordSet());

    put << "creating a single field"  << "\n";
    logger->putx(logga::dbug, put);

    shared_ptr<Field> fsi(new Field());

    fsi->addName("coeff");                   // a single of type double
    fsi->addKind(xeona::e_input);
    fsi->addEnabled(true);
    fsi->addUnits("");
    fsi->addRemark("");

    fsi->addRawStr(fSingle);
    fsi->addSplitStr(fSingleVec);

    put << "creating a timeseries field"  << "\n";
    logger->putx(logga::dbug, put);

    shared_ptr<Field> fts(new Field());

    fts->addName("count");                   // a timeseries of type int
    fts->addKind(xeona::e_input);
    fts->addEnabled(true);
    fts->addUnits("");
    fts->addRemark("");

    fts->addRawStr("(blank)");
    fts->addSplitStr(fTimeseriesVec);

    put << "creating a record"  << "\n";
    logger->putx(logga::dbug, put);

    shared_ptr<Record> rec(new Record());

    rec->addIdentifier("testme");
    rec->addField(fsi);                      // load single field
    rec->addField(fts);                      // load timeseries field

    rst->addRecord(rec);                     // load record

    shared_ptr<Field> loc;
    loc = rst->locateRecordAndField("testme", "coeff");
    put << "    fieldname          : " << loc->getName()  << "\n";
    put << "    split string count : " << loc->getCount() << "\n";
    logger->putx(logga::dbug, put);

    loc = rst->locateRecordAndField("testme", "count");
    put << "    fieldname          : " << loc->getName()  << "\n";
    put << "    split string count : " << loc->getCount() << "\n";
    logger->putx(logga::dbug, put);

    // NOTE: important call next

//      put << "\n";
//      put << "creating a (pseudo) entity" << "\n";
//      logger->putx(logga::dbug, put);
//      Entity_UT ent(*rec);                     // pass the entity its associated record
//
//      put << "\n";
//      put << "modifying the entity single" << "\n";
//      logger->putx(logga::dbug, put);
//      ent.modifySingle(0.01);
//
//      put << "\n";
//      put << "modifying the entity single" << "\n";
//      logger->putx(logga::dbug, put);
//      ent.modifySingle(2.0);
//
//      put << "\n";
//      put << "modifying the entity timeseries" << "\n";
//      logger->putx(logga::dbug, put);
//      ent.modifyTimeseries(8);
  }

  put.flags(prior);                          // reset format flags

  // ---------------------------------------------------------
  //  test SIX        : accessor tests
  // ---------------------------------------------------------

  logger->test(6, "accessor tests");

  {
    // Field accessors
    Field f;

    std::string                            f1 = f.getName();
    xeona::FieldKind                       f2 = f.getKind();
    tribool                                f3 = f.getEnabled();
    std::string                            f4 = f.getUnits();
    std::string                            f5 = f.getRemark();
    std::string                            f6 = f.getRawStr() ;
    const std::vector<std::string>&        f7 = f.getSplitStr();
    int                                    f8 = f.getCount();
    bool                                   f9 = f.isEmpty();

    // Record accessors
    Record r;

    std::string                            r1 = r.getIdentifier();
    xeona::RecordKind                      r2 = r.getKind();
    tribool                                r3 = r.getEnabled();
    const std::vector<std::string>&        r4 = r.getComments();
    const std::vector<shared_ptr<Field> >& r5 = r.getFields();

    // use (some of) the variables in order to stop the compiler
    // warning about an "unused variable"

    // f1 = "";
    f2 = xeona::e_notField;
    // f3 = false;
    // f4 = "";
    // f5 = "";
    // f6 = "";
    f7.empty();
    f8 = 0;
    f9 = false;

    // r1 = "";
    r2 = xeona::e_notRecord;
    // r3 = false;
    r4.empty();
    r5.empty();
  }

  // ---------------------------------------------------------
  //  test SEVEN      : add builtin remark test
  // ---------------------------------------------------------

  logger->test(7, "add builtin remark test");

  {
    Record r;                                // create on stack
    r.hackUnitTestRecord("some useful builtin remark");
  }

  // ---------------------------------
  //  HOUSEKEEPING
  // ---------------------------------

  logger->test(0);                           // test end

  logger->repx(logga::info, "end of main", "~~~~~~~~~~");
  return xeona::exit_success;

} // main function

//  end of file

