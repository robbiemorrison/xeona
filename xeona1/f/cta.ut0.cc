//  XEONA ENERGY SYSTEMS MODELING ENVIRONMENT
//
//  file-create-name : cta.ut0.cc
//  file-create-date : Fri 06-Feb-2009 15:12 UTC
//  file-initiator   : Robbie Morrison <robbie@actrix.co.nz>
//
//  file-role        : captrans algorithm / unit test
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
//  $URL: file:///home/robbie/svn-root/xeona/futz/trunk/xeona1/f/cta.ut0.cc $
//
//  GENERAL NOTES FOR THIS FILE
//
//  This is a unit test file and hence not essential to the
//  application proper.
//
//  AD-HOC NOTES

//  LOCAL AND SYSTEM INCLUDES

#include "cta.h"              // unit under test (place early)

#include "../a/logger.h"      // standard logging functionality (as required)
#include "../c/smart_ptr.h"   // toggle between Boost and TR1 smart pointers
#include ".././common.h"      // common definitions for project (place last)

#include <string>             // C++ strings
#include <sstream>            // string-streams

//  CODE

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
  //  test ONE        : mode report
  // ---------------------------------------------------------

  logger->test(1, "mode report");

  {
#if   (XE_CTA == 1)
    put << "     XE_CTA = " << XE_CTA << " : running WITHOUT hop-relitigate code" << "\n";
#elif (XE_CTA == 2)
    put << "     XE_CTA = " << XE_CTA << " : running with hop-relitigate code"    << "\n";
#else
    put << "     XE_CTA = (not set code-wide)" << "\n";
#endif
    logger->putx(logga::dbug, put);
  }

  // ---------------------------------------------------------
  //  test TWO        : simple creation
  // ---------------------------------------------------------

  logger->test(2, "simple creation (with empty domain controllers vector)");

  {
    const std::vector<shared_ptr<DomainController> > origDomcons;     // descending rank
    const int step = 3;                                               // interval

    CtaSimple ctaSimple(step, origDomcons);
    const tribool triret = ctaSimple.captrans(xeona::e_withholdOkay);

    // CAUTION: special test idiom for tribools
    if ( triret )        put << "   all okay"            << "\n";
    else if ( ! triret ) put << "   blackouts may occur" << "\n";
    else                 put << "   data problems"       << "\n";

    // stream 'triret' so that its identity is known
    put << std::boolalpha << "   triret : " << triret << "\n";

    // report
    logger->putx(logga::dbug, put);
  }

  // ---------------------------------
  //  HOUSEKEEPING
  // ---------------------------------

  logger->test(0);                           // test end

  logger->repx(logga::info, "end of main", "~~~~~~~~~~");
  return xeona::exit_success;

} // main function

//  end of file

