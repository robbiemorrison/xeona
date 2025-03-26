//  XEONA ENERGY SYSTEMS MODELING ENVIRONMENT
//
//  file-create-name : linklog.ut0.cc
//  file-create-date : Thu 30-Jul-2009 21:26 UTC
//  file-initiator   : Robbie Morrison <robbie@actrix.co.nz>
//
//  file-role        : utility class to record entity linking results / unit test
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
//  $URL: file:///home/robbie/svn-root/xeona/futz/trunk/xeona1/c/linklog.ut0.cc $
//
//  GENERAL NOTES FOR THIS FILE
//
//  This is a unit test file and hence not essential to the
//  application proper.

//  LOCAL AND SYSTEM INCLUDES

#include "linklog.h"          // unit under test (place early)

#include "../c/xeona_ptr.h"   // remappable counted pointer which mimics shared_ptr
#include "../b/entity.h"      // entity base class plus lazy linking

#include "../a/logger.h"      // standard logging functionality (as required)
#include "../c/smart_ptr.h"   // toggle between Boost and TR1 smart pointers
#include ".././common.h"      // common definitions for project (place last)

#include <string>             // C++ strings
#include <sstream>            // string-streams

//  CODE

// ---------------------------------------------------------
//  CLASS           : Hack
// ---------------------------------------------------------

class Hack
  : public Entity
{
public:
  Hack() : Entity("my-identifier-1") { }
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
  //  test ONE        : link logger
  // ---------------------------------------------------------

  logger->test(1, "link logger");

  {
    LinkLogger log;
    put << log.recover();
    logger->putx(logga::dbug, put);

    xeona::assign_ptr<Entity> hack(new Hack());   // refer unit 'xeona_ptr'
    log.insert(hack, true);
    log.insert(hack, false);
    put << log.recover() << "\n";
    logger->putx(logga::dbug, put);

    put << "    count  : " << log.getLogCount()  << "\n"
        << "    fails  : " << log.getNullCount() << "\n";
    logger->putx(logga::dbug, put);

    log.reset();
  }

  // ---------------------------------
  //  HOUSEKEEPING
  // ---------------------------------

  logger->test(0);                           // test end

  logger->repx(logga::info, "end of main", "~~~~~~~~~~");
  return xeona::exit_success;

} // main function

//  end of file

