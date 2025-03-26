//  XEONA ENERGY SYSTEMS MODELING ENVIRONMENT
//
//  file-create-name : stats.ut0.cc
//  file-create-date : Wed 16-Sep-2009 13:34 UTC
//  file-initiator   : Robbie Morrison <robbie@actrix.co.nz>
//
//  file-role        : on-the-fly statistical calculations / unit test
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
//  $URL: file:///home/robbie/svn-root/xeona/futz/trunk/xeona1/c/stats.ut0.cc $
//
//  GENERAL NOTES FOR THIS FILE
//
//  This is a unit test file and hence not essential to the
//  application proper.

//  LOCAL AND SYSTEM INCLUDES

#include "stats.h"            // unit under test (place early)

#include "../a/logger.h"      // standard logging functionality (as required)
#include "../c/smart_ptr.h"   // toggle between Boost and TR1 smart pointers
#include ".././common.h"      // common definitions for project (place last)

#include <string>             // C++ strings
#include <sstream>            // string-streams
#include <vector>             // STL sequence container

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

  xeona::yeek = 1;
  std::ostringstream put;

  // ---------------------------------------------------------
  //  test ONE        : 'Statistics' instantiation
  // ---------------------------------------------------------

  logger->test(1, "'Statistics' instantiation");

  {
    Statistics<double> stat;
  }

  // ---------------------------------------------------------
  //  test TWO        : direct usage
  // ---------------------------------------------------------

  logger->test(2, "direct usage");

  {
    Statistics<double> stat;                 // functor

    put << "    status :";
    if ( stat ) put << " full"  << "\n";
    else        put << " empty" << "\n";
    logger->putx(logga::dbug, put);

    stat(1.0);                               // function call operator
    stat(2.0);

    put << "    status :";
    if ( stat ) put << " full"  << "\n";
    else        put << " empty" << "\n";
    logger->putx(logga::dbug, put);

    put << "    count  : " << stat.count() << "\n"
        << "    mean   : " << stat.mean()  << "\n";
    logger->putx(logga::dbug, put);
  }

  // ---------------------------------------------------------
  //  test THREE      : calculation and reporting
  // ---------------------------------------------------------

  logger->test(3, "calculation and reporting");

  {
    // series
    std::vector<double> vec;
    vec.push_back(0);
    vec.push_back(1);
    vec.push_back(2);
    vec.push_back(3);

    // calculate (key call)
    const Statistics<double> stat = xeona::fillStatistics(vec);

    // reporting
    std::ostringstream put;
    put << "    vector     :";
    BOOST_FOREACH( double d, vec ) put << " " << d;
    put << "\n";
    put << "\n";

    put << "    count      : " << stat.count()      << "\n"
        << "    mean       : " << stat.mean()       << "\n"
        << "    var        : " << stat.var()        << "\n"
        << "    sdev       : " << stat.sdev()       << "\n"
        << "    sdev2      : " << stat.sdev2()      << "\n"
        << "    sum        : " << stat.sum()        << "\n"
        << "    sumsq      : " << stat.sumsq()      << "\n"
        << "    min        : " << stat.min()        << "\n"
        << "    max        : " << stat.max()        << "\n"
        << "    range      : " << stat.range()      << "\n"
        << "    zeros      : " << stat.zeros()      << "\n"
        << "    nonzeros   : " << stat.nonzeros()   << "\n"
        << "    zerorat    : " << stat.zerorat()    << "\n"
        << "    nonzerorat : " << stat.nonzerorat() << "\n";
    logger->putx(logga::dbug, put);
  }

  // ---------------------------------------------------------
  //  test FOUR       : bulk loading 1
  // ---------------------------------------------------------

  logger->test(4, "bulk loading (STL vector)");

  {
    // series
    const int length = 4;
    std::vector<double> vec;
    for ( int i = 0; i < length; ++i )
      {
        vec.push_back(static_cast<double>(i));
      }

    // instantiate (key call)
    Statistics<double> stat;

    // fill
    stat(vec);

    // reporting (as previous test)
    put << "    count      : " << stat.count()      << "\n"
        << "    mean       : " << stat.mean()       << "\n"
        << "    var        : " << stat.var()        << "\n";
    logger->putx(logga::dbug, put);

  }

  // ---------------------------------------------------------
  //  test FIVE       : bulk loading 2
  // ---------------------------------------------------------

  logger->test(5, "bulk loading (shared pointer vector)");

  {
    // series
    const int length = 4;
    shared_ptr<std::vector<double> > vec(new std::vector<double>());
    for ( int i = 0; i < length; ++i )
      {
        vec->push_back(static_cast<double>(i));
      }

    // instantiate (key call)
    Statistics<double> stat;

    // fill
    stat(vec);

    // reporting (as previous test)
    put << "    count      : " << stat.count()      << "\n"
        << "    mean       : " << stat.mean()       << "\n"
        << "    var        : " << stat.var()        << "\n";
    logger->putx(logga::dbug, put);

  }

  // ---------------------------------------------------------
  //  test SIX        : constructor loading
  // ---------------------------------------------------------

  logger->test(6, "constructor loading (shared pointer vector)");

  {
    // series
    const int length = 4;
    shared_ptr<std::vector<double> > vec(new std::vector<double>());
    for ( int i = 0; i < length; ++i )
      {
        vec->push_back(static_cast<double>(i));
      }

    // instanciate and fill (key call)
    Statistics<double> stat(vec);

    // reporting (as previous test)
    put << "    count      : " << stat.count()      << "\n"
        << "    mean       : " << stat.mean()       << "\n"
        << "    var        : " << stat.var()        << "\n";
    logger->putx(logga::dbug, put);

  }

  // ---------------------------------------------------------
  //  test SEVEN      : operational mean
  // ---------------------------------------------------------

  logger->test(7, "operational mean");

  {
    // series
    std::vector<double> vec;
    vec.push_back(0.0);                      // first
    vec.push_back(3.0);
    vec.push_back(2.0);
    vec.push_back(0.0);
    vec.push_back(4.0);
    vec.push_back(0.0);
    vec.push_back(1.0);                      // last

    // calculate (key call)
    const Statistics<double> stat = xeona::fillStatistics(vec);

    // reporting
    std::ostringstream put;
    put << "    vector     :";
    BOOST_FOREACH( double d, vec ) put << " " << d;
    put << "\n";
    put << "\n";

    put << "    count      : " << stat.count()      << "\n"
        << "    first      : " << stat.first()      << "\n"
        << "    last       : " << stat.last()       << "\n"
        << "    mean       : " << stat.mean()       << "\n"
        << "    opmean *   : " << stat.opmean()     << "\n"
        << "    sum        : " << stat.sum()        << "\n"
        << "    zeros      : " << stat.zeros()      << "\n"
        << "    nonzeros   : " << stat.nonzeros()   << "\n"
        << "    zerorat    : " << stat.zerorat()    << "\n"
        << "    nonzerorat : " << stat.nonzerorat() << "\n";
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

