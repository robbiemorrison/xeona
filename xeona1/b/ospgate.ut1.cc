//  XEONA ENERGY SYSTEMS MODELING ENVIRONMENT
//
//  file-create-name : ospgate.ut1.cc
//  file-create-date : Fri 08-Jan-2010 06:38 UTC
//  file-initiator   : Robbie Morrison <robbie@actrix.co.nz>
//
//  file-role        :
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
//  $URL: file:///home/robbie/svn-root/xeona/futz/trunk/xeona1/b/ospgate.ut1.cc $
//
//  GENERAL NOTES FOR THIS FILE
//
//  This is a unit test file and hence not essential to the
//  application proper.
//
//  AD-HOC NOTES

//  LOCAL AND SYSTEM INCLUDES

#include "optgate.h"          // unit under test (place early)

#include "../d/glpkviz.h"     // HTML visualization of GLPK problem instances
#include "../b/bandtaf.h"     // banded tariff set and support

#include "../a/logger.h"      // standard logging functionality (as required)
#include "../c/smart_ptr.h"   // toggle between Boost and TR1 smart pointers
#include ".././common.h"      // common definitions for project (place last)

#include <string>             // C++ strings
#include <sstream>            // string-streams

#include <cstdlib>            // C-style exit(), getenv(), system(), NULL, EXIT_SUCCESS

//  CODE

// ---------------------------------------------------------
//  FREE FUNCTION   : ::clearFilename
// ---------------------------------------------------------

namespace
{
  void
  clearFilename
  (const std::string& file)
  {
    logga::spLogger logger = logga::ptrLogStream();

    // make call
    std::string call;
    call += "test -f " + file + " && ";
    call += "mv --force";                    // note also --suffix
    call += " " + file + " " + file + "~";
 // call += " 2>/dev/null";
    const int ret = system(call.c_str());    // utility call thru command interpreter

    // interpret exit status
    std::ostringstream put;
    put << "  call : " << call << "\n";
    if ( ret == 0 || ret == 256 )            // TOFIX: check 256
      {
        logger->repx(logga::dbug, "shell call succeeded, return", ret);
        logger->putx(logga::dbug, put);
      }
    else
      {
        logger->repx(logga::warn, "shell call failed, return", ret);
        logger->putx(logga::warn, put);
      }
  }

} // unnamed namespace

// ---------------------------------------------------------
//  FREE FUNCTION   : ::listFiles
// ---------------------------------------------------------

namespace
{
  void
  listFiles
  (const std::string& subdire,
   const std::string& screen)
  {

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

  // ---------------------------------------------------------
  //  test ONE        : OfrTariffset::uploadTariffSet
  // ---------------------------------------------------------

  logger->test(1, "OfrTariffset::uploadTariffSet test");

  typedef OfrTariffSet_A OfrTariffSet;       // CAUTION: note typedef

  {
    // create tariffset

    shared_ptr<BandedTariffSet> tariffs(new BandedTariffSet("test-one"));

    const std::string tariffStr
      = "4.4"                                // fixed charge
      "* 40e+06 28e-09"
      "* 30e+06 10e-09"
      "* 20e+06 80e-09";                     // decreasing cost -- non-convex
    const double capacity = 60.0e+06;

    tariffs->pushString(tariffStr);
    const bool ret = tariffs->truncate(capacity);

    put << "  " << "band sum     : " << tariffs->getBandSum()  << "\n"
        << "  " << "capacity     : " << tariffs->getCapacity() << "\n"
        << "  " << "return       : " << ret                    << "\n";
    logger->putx(logga::dbug, put);
    logger->addSmartBlank(logga::dbug);

    // create solver

    shared_ptr<svif::SolverIf> solver(new svif::SolverIf("opsgate-ut1"));
    const xeona::DomainMode mode = xeona::e_adminFirst;     // for instance

    // create and load gate OSP

    shared_ptr<OfrTariffSet> ofr(new OfrTariffSet(solver, mode, "ofr"));
    ofr->uploadTariffSet(tariffs, tariffs->getCapacity());

    // run solver

    const std::string stub = "optgate-test";
    solver->runSolver();

    // write to file

    const std::string subdir   = "./xeona-mach";
    const std::string fileprob = subdir + "/" + stub + ".prob";
    const std::string filesoln = subdir + "/" + stub + ".soln";

    ::clearFilename(fileprob);
    ::clearFilename(filesoln);

    solver->writeInfo(stub);

    // prepare webnote

    std::ostringstream note;
    note << "originated by function '" << __func__ << "'"
         << " in unit test '" << __FILE__ << "'"
         << " compiled on " << __DATE__  << " " << __TIME__ << " local";
    const std::string webnote = note.str();

    put << "  "<< "webnote : " << webnote << "\n";
    logger->putx(logga::dbug, put);

    // visualize problem

    GlpkViz webbrowse("firefox");            // also contains default webbrowser string
    webbrowse(solver, webnote, 4);

    // list files system call

    const std::string screen = "*-[0-9][0-9][0-9][0-9][0-9][0-9]-[0-9][0-9].html";
    ::listFiles(subdir, screen);

  }

  // ---------------------------------
  //  HOUSEKEEPING
  // ---------------------------------

  logger->test(0);                           // test end

  logger->repx(logga::info, "end of main", "~~~~~~~~~~");
  return xeona::exit_success;

} // main function

//  end of file

