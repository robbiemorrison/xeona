//  XEONA ENERGY SYSTEMS MODELING ENVIRONMENT
//
//  file-create-name : util4.cc
//  file-create-date : Fri 05-Mar-2010 09:10 UTC
//  file-initiator   : Robbie Morrison <robbie@actrix.co.nz>
//
//  file-role        : free functions for trigonometry and maths / implementation
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
//  $URL: file:///home/robbie/svn-root/xeona/futz/trunk/xeona1/c/util4.cc $
//
//  GENERAL NOTES FOR THIS FILE

//  LOCAL AND SYSTEM INCLUDES

#include "util4.h"            // companion header for this file (place first)

#include "../a/logger.h"      // standard logging functionality (as required)
#include "../c/smart_ptr.h"   // toggle between Boost and TR1 smart pointers
#include ".././common.h"      // common definitions for project (place last)

#include <iostream>           // standard io
#include <limits>             // numeric_limits<T>::infinity() and similar
#include <sstream>            // string-streams
#include <string>             // C++ strings

#include <cmath>              // C-style maths, abs(), ceil(), floor(), sqrt()

#include <boost/math/constants/constants.hpp>     // high-precision mathematical constants

// STATIC CONSTANTS

// CAUTION: static global variables are not normally used in
// 'xeona' but 'PI' below is an exception because this file might
// well be deployed elsewhere

static const double PI = boost::math::constants::pi<double>();   // "static" is file-local

//  CODE

namespace xeona
{
  // ---------------------------------------------------------
  //  FREE FUNCTION   : xeona::log2 (double) (binary logarithm)
  // ---------------------------------------------------------
  //  Description  : returns the base two logarithm, the ISO notation is 'lb'
  //  Role         : general use
  //  Techniques   : <cmath> library function OR local definition
  //  Status       : complete
  //
  //  Design notes
  //
  //      The 'log2' function from <cmath> is not universal.  It
  //      may have been introduced with C99.  It may also be part
  //      of TR1 and possibly the upcoming C++11.
  //
  //      The calculated version shows no apparent loss of
  //      precision, relative to the library version.
  //
  //      This function also works fine with explicit 'int'
  //      integers up to very large integers -- around
  //      2^significand (aka mantissa) or about 9e15 -- at which
  //      point the integer value cannot be exactly represented
  //      as a 'double' type (please check the threshold
  //      definition if it is important to be correct).
  //
  //      Normally "xeona::log2(0.0)" would return '-inf'
  //
  //  CAUTION: memory conflict with <cmath> 'log2' and the logger
  //
  //     The use of the <cmath> 'log2' variant and the logger code
  //     gave the following 'valgrind' error messages:
  //
  //       Stack overflow in thread 1: can't grow stack to 0x7fe801ff8
  //       ...
  //       Process terminating with default action of signal 11 (SIGSEGV): dumping core
  //       ...
  //       ERROR SUMMARY: 33 errors from 33 contexts (suppressed: 4 from 4)
  //
  // ---------------------------------------------------------

  double
  log2
  (const double n)
  {
    // range warning
    static logga::spLogger logger = logga::ptrLogStream();
    if      ( n == 0.0 ) logger->repx(logga::warn, "log_2 zero not defined, n",     n);
    else if ( n  < 0.0 ) logger->repx(logga::warn, "log_2 negative not defined, n", n);

    // calculation
#if 0 // 1 = use <cmath> inbuilt (see caution above), 0 = use log_10 work-around
    return log2(n);                          // refer <cmath>, not in 'std' namespace
#else
    return std::log(n) / std::log(2.0);      // log_2 is log_10(n)/log_10(2)
#endif // 0
  }

  // ---------------------------------------------------------
  //  FREE FUNCTION   : xeona::degree2radian
  // ---------------------------------------------------------
  //  Description  : convert degrees to radians
  //  Role         : general usage
  //  Techniques   : (nothing special)
  //  Status       : complete
  //
  //  Usage suggestion for positive outcomes
  //
  //      double deg = -30.0;
  //      double rad = xeona::normalizeTwoPi(xeona::degree2radian(deg));
  //
  // ---------------------------------------------------------

  double degree2radian(const double degree)
  {
    return degree * (PI / 180);
  }

  // ---------------------------------------------------------
  //  FREE FUNCTION   : xeona::radian2degree
  // ---------------------------------------------------------
  //  Description  : convert radians to degrees
  //  Role         : general usage
  //  Techniques   : (nothing special)
  //  Status       : complete
  //
  //  Usage suggestion for positive outcomes
  //
  //      double rad = -4.0 * 3.14159;
  //      double deg = xeona::radian2degree(xeona::normalizeTwoPi(rad));
  //
  // ---------------------------------------------------------

  double radian2degree(const double radian)
  {
    return radian * (180 / PI);
  }

  // ---------------------------------------------------------
  //  FREE FUNCTION   : xeona::normalizeZeroTwoPi
  // ---------------------------------------------------------
  //  Description  : return 'radian' in range 0-2pi : must be positive
  //  Role         : general usage
  //  Techniques   : 'fmod'
  //  Status       : complete
  //
  //  Source
  //
  //      Based on 'anp.c' from the International Astronomical
  //      Union's SOFA (Standards Of Fundamental Astronomy)
  //      software collection.  SOFA release 2009-12-31.
  //
  // ---------------------------------------------------------

  double normalizeZeroTwoPi(const double radian)
  {
    const double twopi = 2.0 * PI;
    double norm;                             // return value
    norm = fmod(radian, twopi);
    if ( norm < 0.0 )
      {
        norm += twopi;
      }
    return norm;
  }

  // ---------------------------------------------------------
  //  FREE FUNCTION   : xeona::normalizePlusMinusPi
  // ---------------------------------------------------------
  //  Description  : return 'radian' in range +/-pi : cannot exceed abs(pi)
  //  Role         : general use
  //  Techniques   : 'fmod' 'fabs' '?:'
  //  Status       : complete
  //
  //  Source
  //
  //      Based on 'anpm.c' from the International Astronomical
  //      Union's SOFA (Standards Of Fundamental Astronomy)
  //      software collection.  SOFA release 2009-12-31.
  //
  // ---------------------------------------------------------

  double normalizePlusMinusPi(const double radian)
  {
    const double twopi = 2.0 * PI;
    double norm;                             // return value
    norm = fmod(radian, twopi);
    if ( fabs(norm) >= twopi )
      {
        norm -= (radian < 0.0 ? -twopi : twopi);
      }
    return norm;
  }

  // ---------------------------------------------------------
  //  FREE FUNCTION   : xeona::normalizeZero360
  // ---------------------------------------------------------
  //  Description  : return 'degree' in range [0,360]
  //  Role         : general use
  //  Techniques   : wrapper
  //  Status       : complete (better to be directly implemented)
  // ---------------------------------------------------------

  double normalizeZero360(const double degree)
  {
    double norm;
    norm = radian2degree(normalizeZeroTwoPi(degree2radian(degree)));
    return norm;
  }

  // ---------------------------------------------------------
  //  FREE FUNCTION   : xeona::normalizePlusMinus180
  // ---------------------------------------------------------
  //  Description  : return 'degree' in range [-180,180]
  //  Role         : general use
  //  Techniques   : wrapper
  //  Status       : complete (better to be directly implemented)
  // ---------------------------------------------------------

  double normalizePlusMinus180(const double degree)
  {
    double norm;
    norm = radian2degree(normalizePlusMinusPi(degree2radian(degree)));
    return norm;
  }

}

//  end of file

