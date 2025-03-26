//  XEONA ENERGY SYSTEMS MODELING ENVIRONMENT
//
//  file-create-name : xeona_ptr.ut0.cc
//  file-create-date : Fri 31-Jul-2009 16:27 UTC
//  file-initiator   : Robbie Morrison <robbie@actrix.co.nz>
//
//  file-role        : remappable counted pointer which mimics shared_ptr / unit test
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
//  $URL: file:///home/robbie/svn-root/xeona/futz/trunk/xeona1/c/xeona_ptr.ut0.cc $
//
//  GENERAL NOTES FOR THIS FILE
//
//  This is a unit test file and hence not essential to the
//  application proper.
//
//  In line with '::deport' (see the associated header), most of
//  the console reporting here is to 'std::clog'.

//  LOCAL AND SYSTEM INCLUDES

#include "xeona_ptr.h"        // unit under test (place early)

#include "../a/logger.h"      // standard logging functionality (as required)
#include "../c/smart_ptr.h"   // toggle between Boost and TR1 smart pointers
#include ".././common.h"      // common definitions for project (place last)

#include <string>             // C++ strings
#include <sstream>            // string-streams

//  CODE

// ---------------------------------------------------------
//  CLASS           : Base
//  CLASS           : Next
//  CLASS           : Last
// ---------------------------------------------------------

class Base
{
public:
  Base(const int id) : d_id(id) { }
  virtual ~Base() { }
  virtual std::string say() { return "base"; }
  virtual int getIdentifier() { return d_id; }
protected:
  int    d_id;
};

class Next : public Base
{
public:
  Next(const int id) : Base(id) { }
  virtual int getIdentifier() { return d_id; }
  virtual std::string say() { return "next"; }
};

class Last : public Next
{
public:
  Last(const int id) : Next(id) { }
  virtual int getIdentifier() { return d_id; }
  virtual std::string say() { return "last"; }
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
  //  test ONE        : empty instantiation
  // ---------------------------------------------------------

  logger->test(1, "empty instantiation");

  {
    using xeona::assign_ptr;
    logger->addDumbBlank();

    assign_ptr<int> hollow;
  }

  // ---------------------------------------------------------
  //  test TWO        : Common instantiation
  // ---------------------------------------------------------

  logger->test(2, "Common instantiation");

  {
    using xeona::assign_ptr;
    logger->addDumbBlank();

    xeona::Common common;
  }

  // ---------------------------------------------------------
  //  test THREE      : general tests
  // ---------------------------------------------------------

  logger->test(3, "general tests (including revamp)");

  {
    using xeona::assign_ptr;
    logger->addDumbBlank();

    assign_ptr<Next> r1(new Next(1));
    std::clog << "  original (1) = " << r1->getIdentifier() << std::endl;

    assign_ptr<Next> r2(r1);
    std::clog << "   clients (2) = " << r2.client_count()   << std::endl;

    r1.revamp(new Next(7));
    std::clog << "    update (7) = " << r2->getIdentifier() << std::endl;

    assign_ptr<Base> r3 = r2;
    std::clog << "      next (7) = " << r3->getIdentifier() << std::endl;

    assign_ptr<Next> r4 = r2;
    std::clog << "   clients (4) = " << r4.client_count()   << std::endl;

    std::clog << std::boolalpha;
    std::clog << "   unique (no) = " << r4.unique()         << std::endl;

    std::clog << r4.report("r4", "", 30);
  }

  // ---------------------------------------------------------
  //  test FOUR       : downcast tests
  // ---------------------------------------------------------

  logger->test(4, "downcast tests");

  {
    using xeona::assign_ptr;
    using xeona::polymorphic_assign_cast;
    logger->addDumbBlank();

    assign_ptr<Base> x1(new Next(8));
    assign_ptr<Next> x2 = polymorphic_assign_cast<Next>(x1);
    std::clog << x1.report("x1");
    std::clog << x2.report("x2");
  }

  // ---------------------------------------------------------
  //  test FIVE       : revamp tests
  // ---------------------------------------------------------

  logger->test(5, "revamp (pool-wide) tests");

  {
    using xeona::assign_ptr;
    logger->addDumbBlank();

    assign_ptr<Next> next(new Last(1));
    assign_ptr<Base> base = next;

    std::clog << next.report("next", "before revamp");
    std::clog << base.report("base", "before revamp");

    base.revamp(new Last(2));

    std::clog << next.report("next", "after revamp 1");
    std::clog << base.report("base", "after revamp 1");

    base.revamp(new Next(3));

    std::clog << next.report("next", "after revamp 2");
    std::clog << base.report("base", "after revamp 2");

    // faulty calls

#if 0

    base.revamp(new Base(4));
    std::clog << base.report("base", "after revamp 3");
    std::clog << next.report("next", "after revamp 3");

#endif // 0

  }

  // ---------------------------------------------------------
  //  test SIX        : reset tests
  // ---------------------------------------------------------

  logger->test(6, "reset (individual-specific) tests");

  {
    using xeona::assign_ptr;
    logger->addDumbBlank();

    assign_ptr<Base> e1(new Next(12));
    assign_ptr<Base> e2 = e1;
    assign_ptr<Base> e3 = e1;
    e3.reset(new Next(13));                  // reset 'e3'
    std::clog << e1.report("e1");
    std::clog << e3.report("e3");
  }

  // ---------------------------------------------------------
  //  test SEVEN      : non-exclusivity tests
  // ---------------------------------------------------------

  logger->test(7, "external (non-exclusive) resources tests");

  {
    using xeona::assign_ptr;
    logger->addDumbBlank();

    shared_ptr<Base> sp(new Next(0));
    assign_ptr<Base> a1(sp);
    std::clog << a1.report("a1");

    assign_ptr<Base> a2(shared_ptr<Base>(new Next(1)));
    std::clog << a2.report("a2");
  }

  // ---------------------------------------------------------
  //  test EIGHT      : further report tests
  // ---------------------------------------------------------

  logger->test(8, "empty instantiation with report");

  {
    using xeona::assign_ptr;
    logger->addDumbBlank();

    assign_ptr<Base> hollow;
    std::clog << hollow.report("hollow");
  }

  // ---------------------------------------------------------
  //  test NINE       : pointer information tests
  // ---------------------------------------------------------

  logger->test(9, "pointer information tests");

  {
    using xeona::assign_ptr;
    logger->addDumbBlank();

    assign_ptr<Base> m1(new Next(7));
    std::clog << std::boolalpha
              << "   m1 stream (true) : " << m1              << std::endl;
    std::clog << "   m1.get()         : " << m1.get()        << std::endl;
    std::clog << "   m1.get_assign()  : " << m1.get_assign() << std::endl;
  }

  // ---------------------------------------------------------
  //  test TEN        : special
  // ---------------------------------------------------------

  logger->test(10, "special");

  {
    using xeona::assign_ptr;
    logger->addDumbBlank();

    assign_ptr<Next> next(new Last(1));
    assign_ptr<Base> base = next;
    std::clog << base.report("base", "original");
    std::clog << next.report("next", "original");

    base.revamp(new Last(2));
    next.revamp(new Last(3));

    std::clog << base.report("base", "revamp");
    std::clog << next.report("next", "revamp");

    base.revamp(new Base(4));                // [1]

    std::clog << base.report("base", "faulty");
    std::clog << next.report("next", "faulty");

    std::clog << "  base, faulty (4) = " << base->getIdentifier() << std::endl;
    std::clog << "  next, faulty (4) = " << next->getIdentifier() << std::endl;

    shared_ptr<Base> realbase(new Base(5));
    // shared_ptr<Next> next2 = realbase;    // compare with [1] which is type-unsafe
    shared_ptr<Base> base2 = realbase;
  }

  // ---------------------------------
  //  HOUSEKEEPING
  // ---------------------------------

  logger->test(0);                           // test end

  logger->repx(logga::info, "end of main", "~~~~~~~~~~");
  return xeona::exit_success;

} // main function

//  end of file

