//  XEONA ENERGY SYSTEMS MODELING ENVIRONMENT
//
//  file-create-name : tsops.ut0.cc
//  file-create-date : Wed 16-Sep-2009 16:58 UTC
//  file-initiator   : Robbie Morrison <robbie@actrix.co.nz>
//
//  file-role        : overloaded operators for timeseries / unit test
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
//  $URL: file:///home/robbie/svn-root/xeona/futz/trunk/xeona1/c/tsops.ut0.cc $
//
//  GENERAL NOTES FOR THIS FILE
//
//  This is a unit test file and hence not essential to the
//  application proper.

//  LOCAL AND SYSTEM INCLUDES

#include "tsops.h"            // unit under test (place early)

#include "../a/logger.h"      // standard logging functionality (as required)
#include "../c/smart_ptr.h"   // toggle between Boost and TR1 smart pointers
#include ".././common.h"      // common definitions for project (place last)

#include <string>             // C++ strings
#include <sstream>            // string-streams

#include <boost/assign/std/vector.hpp>       // assign operator+=() for std::vector

//  NAMESPACE DECLARATIONS

using namespace boost::assign;               // for: vec += 0, 1;

//  CODE

// ---------------------------------------------------------
//  FREE FUNCTION   : ::vector2TupleFill <>
// ---------------------------------------------------------

namespace
{
  template <typename T>
  shared_ptr<std::vector<boost::tuple<T, T> > >
  vector2TupleFill
  (const int size,
   const T   zero,                           // first element
   const T   one)                            // second element
  {
    typedef std::vector<boost::tuple<T, T> > vec2tuple_type;
    shared_ptr<vec2tuple_type> buffer(new vec2tuple_type());
    for ( int i = 0; i < size; ++i )
      {
        buffer->push_back(boost::make_tuple(zero, one));
      }
    return buffer;

  } // function '::vector2TupleFill'

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

  xeona::yeek = 1;
  std::ostringstream put;

  // ---------------------------------------------------------
  //  test ONE        : vectorSum
  // ---------------------------------------------------------

  logger->test(1, "vector sum");

  {
    shared_ptr<std::vector<double> > vec(new std::vector<double>(5, 1.5));

    put << "  vec sum  : " << xeona::vectorSum(vec) << "\n";
    logger->putx(logga::dbug, put);
  }

  // ---------------------------------------------------------
  //  test TWO        : operator+
  // ---------------------------------------------------------

  logger->test(2, "operator+");

  {
    shared_ptr<std::vector<double> > vec1(new std::vector<double>(5, 1.5));
    shared_ptr<std::vector<double> > vec2(new std::vector<double>(5, 0.5));
    shared_ptr<std::vector<double> > vec3;

    vec3 = vec1 + vec2;

    put << "  vec1 sum : " << xeona::vectorSum(vec1) << "\n"
        << "  vec2 sum : " << xeona::vectorSum(vec2) << "\n"
        << "  vec3 sum : " << xeona::vectorSum(vec3) << "\n";
    logger->putx(logga::dbug, put);
  }

  // ---------------------------------------------------------
  //  test THREE      : 2-tuple vector to simple vector
  // ---------------------------------------------------------

  logger->test(3, "2-tuple vector to simple vector");

  {
    typedef std::vector<boost::tuple<double, double> > v2td_type;
    typedef std::vector<double>                        vd_type;

    shared_ptr<v2td_type> input  = ::vector2TupleFill(24, 4.4, 8.8);   // fill call
    shared_ptr<vd_type>   output = xeona::vector2TupleExtract0(input); // extract call

    xeona::vectorPrint(output, "output", put);
    logger->putx(logga::dbug, put);
  }

  // ---------------------------------------------------------
  //  test FOUR       : diurnal vector
  // ---------------------------------------------------------

  logger->test(4, "diurnal vector");

  {
    // vector
    shared_ptr<std::vector<double> > fill(new std::vector<double>());

    // parameters
    double mean       = 15.5;
    double amplitude  = 3.5;
    int    offset     = 0;
    double randomness = 0.1;
    int    resolution = 24;                  // samples per day
    int    length     = 12;

    // principal call
    xeona::vectorFillDiurnal(fill,           // vector
                             mean,
                             amplitude,
                             offset,
                             randomness,
                             resolution,
                             length);        // timeseries length

    // report call
    logger->addSmartBlank(logga::dbug);
    xeona::vectorPrint(fill, "fill", put);
    logger->putx(logga::dbug, put);
  }

  // ---------------------------------------------------------
  //  test FIVE       : zip two vectors
  // ---------------------------------------------------------

  logger->test(5, "zip two vectors");

  {
    // vectors
    const int len = 8760;
    shared_ptr<std::vector<double> >  left(new std::vector<double>(len, 3.3));
    shared_ptr<std::vector<double> > right(new std::vector<double>(len, 6.6));

    // zip
    shared_ptr<std::vector<boost::tuple<double, double> > > zip
      = xeona::vector2TupleZip(left, right);

    // report
    if ( zip )
      {
        const double firstleft = zip->front().get<0>();
        put << "  first left value : " << firstleft << "\n";
        logger->putx(logga::dbug, put);
      }
    else
      {
        put << "  abandoning test, 'zip' failed boolean test" << "\n";
        logger->putx(logga::dbug, put);
      }
  }

  // ---------------------------------------------------------
  //  test SIX        : number string to numerical vector
  // ---------------------------------------------------------

  logger->test(6, "number string to numerical vector");

  {
    const std::string input = "1.2 3.4 5.6";
    const std::string sep   = " ";
    const std::vector<double> vec = xeona::stringParse<double>(input, sep);
    put << "  input string : " << input          << "\n"
        << "  separators   : \"" << sep << "\""  << "\n"
        << "  second value : " << vec.at(2-1)    << "\n";
    logger->putx(logga::dbug, put);
  }

  // ---------------------------------------------------------
  //  test SEVEN    : vectorMean
  // ---------------------------------------------------------

  logger->test(7, "vector mean");

  {
    // prepare vector
    const int length = 40;
    shared_ptr<std::vector<double> > vec(new std::vector<double>());
    xeona::vectorFillRandom(vec,             // vector
                            2.0,             // lower
                            3.0,             // upper
                            length);         // timeseries length

    // mean call
    const double mean = xeona::vectorMean(vec);

    // parallel calculation
    const int len2 = vec->size();
    double sum2(0);                          // initial value
    for ( int i = 0; i < len2; ++i ) sum2 += vec->at(i);
    const double mean2 = sum2 / len2;

    // report
    const int len = vec->size();
    put << "  length             : " << len        << "\n"
        << "  mean (accumulate)  : " << mean       << "\n"
        << "  mean (for loop)    : " << mean2      << "\n";
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

