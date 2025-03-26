//  XEONA ENERGY SYSTEMS MODELING ENVIRONMENT
//
//  file-create-name : conex.ut0.cc
//  file-create-date : Wed 16-Jul-2008 15:22 UTC
//  file-initiator   : Robbie Morrison <robbie@actrix.co.nz>
//
//  file-role        : create and connect block interfaces / unit test
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
//  $URL: file:///home/robbie/svn-root/xeona/futz/trunk/xeona1/c/conex.ut0.cc $
//
//  GENERAL NOTES FOR THIS FILE
//
//  This is a unit test file and hence not essential to the
//  application proper.

//  LOCAL AND SYSTEM INCLUDES

#include "conex.h"            // unit under test (place early)

#include "../b/commods.h"     // commodities hierarchy
#include "../c/recset.h"      // record set support
#include "../a/logger.h"      // standard logging functionality (as required)
#include "../c/smart_ptr.h"   // toggle between Boost and TR1 smart pointers
#include ".././common.h"      // common definitions for project (place last)

#include <string>             // C++ strings
#include <sstream>            // string-streams

//  CODE

Record r;                                    // global object!

// ---------------------------------------------------------
//  CLASS           : Asset
// ---------------------------------------------------------

class Asset :
  public FullEntity                   // 'Block' skipped for this exercise
{
public:

  Asset(const std::string id) :
    FullEntity(id, r),
    d_id(id)
  { }

  virtual ~Asset() { }

protected:

  std::string               d_id;

  static logga::spLogger    s_logger;
};

// STATIC DEFINITIONS

logga::spLogger Asset::s_logger = logga::ptrLogStream();

// ---------------------------------------------------------
//  FREE FUNCTION   : ::hi <>
// ---------------------------------------------------------

namespace
{
  void
  hi
  (const std::string&           interfaceTag,
   const shared_ptr<Interface>& interface)   // pass-by-ref to honor count
  {
    logga::spLogger logger = logga::ptrLogStream();
    logger->addSmartBlank();
    std::ostringstream put;
    put << "    interface details"                                 << "\n"
        << "    local name      : "     << interfaceTag            << "\n"
        << "    qualified name  : "     << interface->getMyKey()   << "\n"
        << "    use count       : "     << interface.use_count()   << "\n";
    logger->putx(logga::dbug, put);
  }
}

// ---------------------------------------------------------
//  CLASS           : Supply (with socket)
// ---------------------------------------------------------

class Supply :
  public Asset
{
public:

  Supply
  (const std::string id) :                   // omit record for current purposes
    Asset(id),
    i_workOut(Socket<CmWork>::create(id, "one", "commodity-id")),
    d_flowWork(new std::vector<double>(20, 10))   // set arbitrarily, 20 elements of ten
  {
    report();
  }

  void report()
  {
    ::hi("i_workOut", i_workOut);
  }

  void action()
  {
    std::ostringstream put;

    // GET COMMODITY POINTER TRIAL

    shared_ptr<Commodity> wcom = i_workOut->getCm();
    s_logger->addSmartBlank();
    put << "  i_work commodity (address) : " << wcom << "\n";
    s_logger->putx(logga::dbug, put);

    // FLOW SETTING TRIALS

    double val;                              // new flow value
    s_logger->addSmartBlank();
    i_workOut->sayFlow();                    // report existing value, zero

    // bind reference approach
    val = 10.1;
    put << "  setting flow to (bound reference) : " << val << "\n";
    s_logger->putx(logga::dbug, put);
    double& flow = i_workOut->tieFlow();     // bind to reference
    flow = val;
    i_workOut->sayFlow();

    // direct assign approach
    val = -20.2;
    put << "  setting flow to (direct assign)   : " << val << "\n";
    s_logger->putx(logga::dbug, put);
    i_workOut->tieFlow() = val;              // assign directly
    i_workOut->sayFlow();

    // argument approach
    val = 30.3;
    put << "  setting flow to (by argument)     : " << val << "\n";
    s_logger->putx(logga::dbug, put);
    i_workOut->tieFlow(val);                 // set by argument
    i_workOut->sayFlow();

    // SAY ALL TRIAL

    s_logger->addSmartBlank();
    i_workOut->sayAll("i_workOut");

    // GET PARTNER POINTER TRIAL

    s_logger->addSmartBlank();
    shared_ptr<Entity> partner = i_workOut->getPartner();
    put << "  i_work partner (address) : " << partner << "\n";
    s_logger->putx(logga::dbug, put);

    return;
  }

private:

  shared_ptr<Socket<CmWork> >         i_workOut;
  shared_ptr<std::vector<double> >    d_flowWork;

};

// ---------------------------------------------------------
//  CLASS           : Load (with cable)
// ---------------------------------------------------------

class Load :
  public Asset
{
public:

  Load
  (const std::string id) :                   // omit record for current purposes
    Asset(id),
    i_workIn(Cable<CmWork>::create(id, "mitte.one", "commodity-id")),
    d_flowWork(new std::vector<double>(20, 10))   // set arbitrarily, 20 elements of ten
  { }

private:

  shared_ptr<Cable<CmWork> >          i_workIn;
  shared_ptr<std::vector<double> >    d_flowWork;

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
  //  test ONE        : direct interface trials
  // ---------------------------------------------------------

  logger->test(1, "direct interface trials");

  {
    shared_ptr<Cable<CmHeat> >  spCabCmHeat;
    shared_ptr<Socket<CmHeat> > spSockCmHeat;
    shared_ptr<Cable<CmWork> >  spCabCmWork;
    shared_ptr<Socket<CmWork> > spSockCmWork;

    spCabCmHeat  = Cable<CmHeat>::create("hable-id", "hocket-id.qual", "com-id");
    spSockCmHeat = Socket<CmHeat>::create("hocket-id", "qual", "com-id");
    spCabCmWork  = Cable<CmWork>::create("wable-id", "wocket-id.qual", "com-id");
    spSockCmWork = Socket<CmWork>::create("wocket-id", "qual", "com-id");

    bool ret = Interface::connectAll();

    logger->addSmartBlank();
    put << std::boolalpha
        << "  connectAll return    : " << ret                             << "\n"
        << "  connections complete : " << Interface::isComplete()         << "\n"
        << "  connections count    : " << Interface::getConnectionCount() << "\n";
    logger->putx(logga::dbug, put);

    Interface::reset();
  }

  // ---------------------------------------------------------
  //  test TWO        : block-based trials
  // ---------------------------------------------------------

  logger->test(2, "block-based trials");

  {
    r.hackUnitTestRecord("block based remark");

    Supply supply("mitte");
    Load   load("charlottenburg");

    bool ret = Interface::connectAll();

    logger->addSmartBlank();
    put << std::boolalpha
        << "  connectAll return    : " << ret                             << "\n"
        << "  connections complete : " << Interface::isComplete()         << "\n"
        << "  connections count    : " << Interface::getConnectionCount() << "\n";
    logger->putx(logga::dbug, put);

    supply.action();                         // will display zero as pointer address

    Interface::reset();
  }

  // ---------------------------------
  //  HOUSEKEEPING
  // ---------------------------------

  logger->test(0);                           // test end

  logger->repx(logga::info, "end of main", "~~~~~~~~~~");
  return xeona::exit_success;

} // main function

//  end of file

