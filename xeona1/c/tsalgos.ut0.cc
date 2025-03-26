//  XEONA ENERGY SYSTEMS MODELING ENVIRONMENT
//
//  file-create-name : tsalgos.ut0.cc
//  file-create-date : Tue 24-May-2011 22:45 UTC
//  file-initiator   : Robbie Morrison <robbie@actrix.co.nz>
//
//  file-role        : custom STL algorithms for timeseries / unit test
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
//  $URL: file:///home/robbie/svn-root/xeona/futz/trunk/xeona1/c/tsalgos.ut0.cc $
//
//  GENERAL NOTES FOR THIS FILE
//
//  This is a unit test file and hence not essential to the
//  application proper.

//  LOCAL AND SYSTEM INCLUDES

#include "tsalgos.h"          // unit under test (place early)

#include "../a/logger.h"      // standard logging functionality (as required)
#include "../c/smart_ptr.h"   // toggle between Boost and TR1 smart pointers
#include ".././common.h"      // common definitions for project (place last)

#include <ostream>            // output streams
#include <string>             // C++ strings
#include <sstream>            // string-streams

#include <boost/assign/std/vector.hpp>  // assign operator+=() for std::vector

//  NAMESPACE DIRECTIVES

using namespace boost::assign;               // bring 'operator+=()' into scope

//  CODE

// ---------------------------------------------------------
//  FREE FUNCTION   : ::fillTs
// ---------------------------------------------------------

namespace
{
  unsigned
  fillTs
  (std::vector<double>& vec,
   const unsigned       size = 0)
  {
    vec += 1.2, 2.3, 4.5, 5.0, 8.7, 9.0, 12.2;                   // 'Boost.Assign' library
    if ( size != 0 && size < vec.size() ) vec.resize(size);      // truncate
    return vec.size();
  }
}

// ---------------------------------------------------------
//  FREE FUNCTION   : ::printVector <>
// ---------------------------------------------------------
//  Description  : print a vector
//  Role         : utility
//  Techniques   : template, 'std::copy', statistics
//  Status       : complete
//
//  Typical output: msg size mean values
//
//      v1         :   7    6.12857    1.200 2.300 4.500 5.000 8.700 9.000 12.200
//
//  CAUTION: adds a normally unwanted trailing space
//
// ---------------------------------------------------------

namespace
{
  template <typename T>
  void
  printVector
  (const std::string&    msg,                // prepended message
   const std::vector<T>& vec,                // source vector
   std::ostream&         os)
  {
    const int    tab   = 10;
    const int    count = vec.size();
    const double mean  = std::accumulate(vec.begin(), vec.end(), 0.0) / count;

    os << "  ";
    os << std::left << std::setw(tab) << msg << " : ";
    os << std::right << std::setw(3) << count;
    if ( ! vec.empty() )
      {
        os << "  ";
        os << std::fixed << std::setprecision(5) << std::setw(9) << mean << "    ";
        os << std::fixed << std::setprecision(3);
        std::copy(vec.begin(), vec.end(), std::ostream_iterator<T>(os, " "));
      }
    os << std::endl;
  }
}

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

  xeona::yeek = 1;                           // maximum yeeking
  std::ostringstream put;

  // ---------------------------------------------------------
  //  test ONE        : fill and print vectors
  // ---------------------------------------------------------

  logger->test(1, "fill and print vectors");

  {
    std::vector<double> v1;
    put << "  ret        : " << ::fillTs(v1, 100) << "\n";
    ::printVector("v1", v1, put);
    logger->putx(logga::dbug, put);
  }

  // ---------------------------------------------------------
  //  test TWO        : ResampleTsCompress
  // ---------------------------------------------------------

  logger->test(2, "ResampleTsCompress");

  {
    const int shrink = 3;
    put << "  shrink     : " << shrink << "\n";
    logger->putx(logga::dbug, put);

    std::vector<double> v1;
    std::vector<double> v2;
    ::fillTs(v1, 6);
    v2.reserve(v1.size());                   // CAUTION: optional but useful

    xeona::ResampleTsCompress functor(shrink);
    std::back_insert_iterator <std::vector<double> >
      ret = functor(v1.begin(), v1.end(), std::back_inserter(v2));    // see <iterator>

    ::printVector("v1", v1, put);
    ::printVector("v2", v2, put);

    logger->putx(logga::dbug, put);
  }

  // ---------------------------------------------------------
  //  test THREE      : ResampleTsCompress throw
  // ---------------------------------------------------------

  logger->test(3, "ResampleTsCompress throw");

  {
    const int shrink = 0;
    put << "  shrink     : " << shrink << "\n";
    logger->putx(logga::dbug, put);

    std::vector<double> v1;
    std::vector<double> v2;
    ::fillTs(v1, 6);
    v2.reserve(v1.size());                   // CAUTION: optional but useful

    try
      {
        xeona::ResampleTsCompress functor(shrink);
        functor(v1.begin(), v1.end(), std::back_inserter(v2));   // see <iterator>

        ::printVector("v1", v1, put);
        ::printVector("v2", v2, put);
        logger->putx(logga::dbug, put);
      }
    catch( const std::domain_error& e )      // this one
      {
        put << "  " << __FILE__ << ":" << __LINE__
            << ": caught std::domain_error: " << e.what()
            << "\n";
        logger->putx(logga::dbug, put);
      }
    catch( const std::exception& e )
      {
        std::cout << "  " << __FILE__ << ":" << __LINE__
                  << ": caught std::exception: " << e.what()
                  << "\n";
        logger->putx(logga::dbug, put);
      }
  }

  // ---------------------------------------------------------
  //  test FOUR       : ResampleTsLinear
  // ---------------------------------------------------------

  logger->test(4, "ResampleTsLinear");

  {
    const int grow = 3;
    put << "  grow       : " << grow << "\n";
    logger->putx(logga::dbug, put);

    std::vector<double> v1;
    std::vector<double> v2;
    ::fillTs(v1, 6);
    v2.reserve(grow * v1.size());            // CAUTION: optional but useful

    xeona::ResampleTsLinear functor(grow);
    std::back_insert_iterator <std::vector<double> >
      ret = functor(v1.begin(), v1.end(), std::back_inserter(v2));    // see <iterator>

    ::printVector("v1", v1, put);
    ::printVector("v2", v2, put);
    logger->putx(logga::dbug, put);
  }

  // ---------------------------------------------------------
  //  test FIVE       : ResampleTsDuplicate
  // ---------------------------------------------------------

  logger->test(5, "ResampleTsDuplicate");

  {
    const int grow = 3;
    put << "  grow       : " << grow << "\n";
    logger->putx(logga::dbug, put);

    std::vector<double> v1;
    std::vector<double> v2;
    ::fillTs(v1, 6);
    v2.reserve(grow * v1.size());            // CAUTION: optional but useful

    xeona::ResampleTsDuplicate functor(grow);
    std::back_insert_iterator <std::vector<double> >
      ret = functor(v1.begin(), v1.end(), std::back_inserter(v2));    // see <iterator>

    ::printVector("v1", v1, put);
    ::printVector("v2", v2, put);
    logger->putx(logga::dbug, put);
  }

  // ---------------------------------------------------------
  //  test SIX        : ModifyTsDeLeapYear
  // ---------------------------------------------------------

  logger->test(6, "ModifyTsDeLeapYear");

  {
    const unsigned ilen = 1800;
    put << "  ilen       : " << ilen << "\n";
    logger->putx(logga::dbug, put);

    std::vector<double> v1;
    std::vector<double> v2;
    ::fillTs(v1, 6);
    v2.reserve(v1.size());                   // CAUTION: optional but useful

    xeona::ModifyTsDeLeapYear functor(ilen);
    std::back_insert_iterator <std::vector<double> >
      ret = functor(v1.begin(), v1.end(), std::back_inserter(v2));    // see <iterator>

    ::printVector("v1", v1, put);
    ::printVector("v2", v2, put);
    logger->putx(logga::dbug, put);
  }

  // ---------------------------------------------------------
  //  test SEVEN      : ModifyTsReLeapYear
  // ---------------------------------------------------------

  logger->test(7, "ModifyTsReLeapYear");

  {
    const unsigned ilen = 7200;
    put << "  ilen       : " << ilen << "\n";
    logger->putx(logga::dbug, put);

    std::vector<double> v1;
    std::vector<double> v2;
    ::fillTs(v1, 6);
    v2.reserve(2 * v1.size());               // CAUTION: optional but useful

    xeona::ModifyTsReLeapYear functor(ilen);
    std::back_insert_iterator <std::vector<double> >
      ret = functor(v1.begin(), v1.end(), std::back_inserter(v2));    // see <iterator>

    ::printVector("v1", v1, put);
    ::printVector("v2", v2, put);
    logger->putx(logga::dbug, put);
  }

  // ---------------------------------------------------------
  //  test EIGHT      : ModifyTsReLeapYear throw
  // ---------------------------------------------------------

  logger->test(8, "ModifyTsReLeapYear throw");

  {
    const unsigned ilen = 7201;              // CAUTION: invalid value intentions
    put << "  ilen       : " << ilen << "\n";
    logger->putx(logga::dbug, put);

    std::vector<double> v1;
    std::vector<double> v2;
    ::fillTs(v1, 6);
    v2.reserve(2 * v1.size());               // CAUTION: optional but useful

    try
      {
        xeona::ModifyTsReLeapYear functor(ilen);
        functor(v1.begin(), v1.end(), std::back_inserter(v2));   // see <iterator>

        ::printVector("v1", v1, put);
        ::printVector("v2", v2, put);
        logger->putx(logga::dbug, put);
      }
    catch( const std::domain_error& e )
      {
        put << "  " << __FILE__ << ":" << __LINE__
            << ": caught std::domain_error: " << e.what()
            << "\n";
        logger->putx(logga::dbug, put);
      }
    catch( const std::exception& e )
      {
        put << "  " << __FILE__ << ":" << __LINE__
            << ": caught std::exception: " << e.what()
            << "\n";
        logger->putx(logga::dbug, put);
      }
  }

  // ---------------------------------------------------------
  //  test NINE       : Month2TsDuplicate
  // ---------------------------------------------------------

  logger->test(9, "Month2TsDuplicate");

  {
    const unsigned ilen = 24 * 3600;         // one day
    put << "  ilen       : " << ilen << "\n";
    logger->putx(logga::dbug, put);

    std::vector<double> v1;
    std::vector<double> v2;
    v1 += 50.0, 48.0, 46.0, 47.0, 49.0, 50.0, 48.0, 46.0, 47.0, 49.0, 50.0, 48.0;
    v2.reserve(31 * v1.size());              // CAUTION: optional but useful

    const bool leapYear = true;
    xeona::Month2TsDuplicate functor(ilen, leapYear);
    functor(v1.begin(), v1.end(), std::back_inserter(v2));  // see <iterator>

    ::printVector("v1", v1, put);
    ::printVector("v2", v2, put);
    logger->putx(logga::dbug, put);
  }

  // ---------------------------------------------------------
  //  test TEN        : OffsetTs
  // ---------------------------------------------------------

  // checks out fine

  logger->test(10, "OffsetTs");

  {
    // offset 3, wrap true/false
    std::vector<double> v1;
    std::vector<double> v2;
    v1 += 50.0, 48.0, 46.0, 47.0, 49.0, 50.0, 48.0, 46.0, 47.0, 49.0, 50.0, 48.0; // 12
    v2.reserve(v1.size());                   // CAUTION: optional but useful

    const int  offsetA = 3;
    bool loopableA;
    loopableA = false;
    loopableA = true;
    xeona::OffsetTs functorA(offsetA, loopableA);
    functorA(v1.begin(), v1.end(), std::back_inserter(v2));      // see <iterator>

    ::printVector("v1", v1, put);
    ::printVector("v2", v2, put);
    logger->putx(logga::dbug, put);

    // offset zero, wrap false
    std::vector<double> v3;
    std::vector<double> v4;
    v3 += 50.0, 48.0, 46.0, 47.0, 49.0, 50.0, 48.0, 46.0, 47.0, 49.0, 50.0, 48.0;
    v4.reserve(v1.size());                   // CAUTION: optional but useful

    const int  offsetB   = 0;
    const bool loopableB = false;
    xeona::OffsetTs functorB(offsetB, loopableB);
    functorB(v3.begin(), v3.end(), std::back_inserter(v4));      // see <iterator>

    ::printVector("v3", v3, put);
    ::printVector("v4", v4, put);
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

