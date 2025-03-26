//  XEONA ENERGY SYSTEMS MODELING ENVIRONMENT
//
//  file-create-name : util3.cc
//  file-create-date : Thu 31-Dec-2009 00:18 UTC
//  file-initiator   : Robbie Morrison <robbie@actrix.co.nz>
//
//  file-role        : free functions for floating point comparison / implementation
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
//  $URL: file:///home/robbie/svn-root/xeona/futz/trunk/xeona1/c/util3.cc $
//
//  GENERAL NOTES FOR THIS FILE

//  LOCAL AND SYSTEM INCLUDES

#include "util3.h"            // companion header for this file (place first)

#include "../a/logger.h"      // standard logging functionality (as required)
#include "../c/smart_ptr.h"   // toggle between Boost and TR1 smart pointers
#include ".././common.h"      // common definitions for project (place last)

#include <iostream>           // standard io
#include <sstream>            // string-streams
#include <stdexcept>          // standard exception classes, runtime_error()
#include <string>             // C++ strings

#include <cmath>              // C-style maths, abs(), ceil(), floor(), sqrt()

#include <boost/math/special_functions/next.hpp>  // unit of last place precision calcs

//  PREPROCESSOR MACROS FOR TEST PURPOSES

#ifdef _XUTEST
# define XE_ALMOSTEQUAL 1 // only turn on when testing this unit
#else
# define XE_ALMOSTEQUAL 0 // 0 = silence, 1 = verbose
#endif

//  CODE

// ---------------------------------------------------------
//  FREE FUNCTION   : xeona::almostEqual <>
// ---------------------------------------------------------
//
//  Design notes
//
//      The user documentation in the header file describes the
//      role and characteristics of this function.
//
//      This code uses relative error.  For reference purposes,
//      the following commonly-encountered code implements a test
//      based on absolute error:
//
//          #include <cmath>
//          #include <limits>
//
//          bool areSimilar(const double a, const double b)
//          {
//            return std::abs(a - b) < std::numeric_limits<double>::epsilon();
//          }
//
//      The Boost library 'boost::math::float_distance' call
//      appears to be header-only.  No Boost linking directives
//      were made, nor were any run-time Boost dependencies
//      reported by 'ldd'.
//
//      The underlying implementation used here is very
//      straightforward.  Rather, it is the template structure,
//      unit test includes, and logging statements that make the
//      actual code look ratty.
//
//      ULP is "unit of last place".
//
//  CAUTION: comparison between -max double and max double
//
//      The 'boost::math::float_distance' call return can be
//      interpreted as a 'long' although strictly it is of type
//      'boost::math::FPT'.  In addition, the library
//      documentation states:
//
//        "If the distance is too great then it may not be able
//         to be represented as an exact integer by type FPT, but
//         in practice this is unlikely to be a issue."
//
//      In practice it was an issue!  The solution was to stay
//      with the floating point return.
//
//      FPT is "floating point type".
//
//  See also
//
//      http://en.wikipedia.org/wiki/Unit_in_the_last_place
//
//      http://www.cygnus-software.com/papers/comparingfloats/comparingfloats.htm
//
//      http://www.boost.org/doc/libs/1_33_1/libs/test/doc/
//      components/test_tools/floating_point_comparison.html
//
//      http://www.boost.org/doc/libs/1_36_0/libs/test/doc/
//      html/utf/testing-tools/floating_point_comparison.html
//
// ---------------------------------------------------------

namespace xeona
{
  template <typename F>
  bool
  almostEqual
  (const F                a,
   const F                b,
   const xeona::Precision precision)
  {
    // logging support
    static logga::spLogger logger = logga::ptrLogStream();
    static std::ostringstream put;

#if (XE_ALMOSTEQUAL == 0)

    // short-circuit if floating point equal
    if ( a == b ) return true;               // values are identical

#endif // 0

    // select the required numerical precision test tolerance
    F dif = 0.0;                             // CAUTION: must initialize
    switch ( precision )
      {
      case xeona::exact: dif =                   0.0; break;
      case xeona::numic: dif =          7600000000.0; break;
      case xeona::tight: dif =        760000000000.0; break;
      case xeona::loose: dif =      76000000000000.0; break;
      case xeona::zero0: dif = boost::math::float_distance<F>(F(0.0), F(1.0e-10)); break;
      case xeona::zero9: dif = boost::math::float_distance<F>(F(0.0), F(1.0e-09)); break;
      case xeona::zero8: dif = boost::math::float_distance<F>(F(0.0), F(1.0e-08)); break;
      case xeona::zero7: dif = boost::math::float_distance<F>(F(0.0), F(1.0e-07)); break;
      case xeona::zero6: dif = boost::math::float_distance<F>(F(0.0), F(1.0e-06)); break;
      case xeona::zero5: dif = boost::math::float_distance<F>(F(0.0), F(1.0e-05)); break;
      case xeona::zero4: dif = boost::math::float_distance<F>(F(0.0), F(1.0e-04)); break;
      case xeona::zero3: dif = boost::math::float_distance<F>(F(0.0), F(1.0e-03)); break;
      case xeona::zero2: dif = boost::math::float_distance<F>(F(0.0), F(1.0e-02)); break;
      default:
        std::clog << "** coding error 01 in source file " << __FILE__ << std::endl;
        break;
      }

    // for reference, 'xeona::zero8' yields 4487126258331716608

#if (XE_ALMOSTEQUAL == 1)

    // unit test reporting
    put << std::fixed << std::setprecision(20)
        << "  a         : " << a                               << "\n"
        << "  b         : " << b                               << "\n"
        << "  enum      : " << precision                       << "\n"
        << "  dif       : " << dif                             << "\n";
    logger->putx(logga::dbug, put);

#endif // XE_ALMOSTEQUAL

    F ulp;                                   // actual distance [1]
    try
      {
        ulp = std::abs(boost::math::float_distance<F>(a, b));
      }
    catch( const std::domain_error& e )      // 'inf' or 'NaN' encountered
      {
        logger->repx(logga::warn, "catching std::domain_error", "");
        put << "  " << e.what() << "\n";
        logger->putx(logga::dbug, put);
        logger->addSmartBlank(logga::dbug);
        return false;
      }
    // [1] CAUTION: must remain with 'F' type and not cast to 'long'

#if (XE_ALMOSTEQUAL == 1)

    // unit test reporting
    put << std::fixed << std::setprecision(20)
        << "  ulp       : " << ulp                             << "\n"
        << "  outcome   : " << (ulp > dif ? "false" : "true")  << "\n";
    logger->putx(logga::dbug, put);

#endif // XE_ALMOSTEQUAL

    if ( ulp > dif ) return false;           // treat values as different
    else             return true;            // treat values as equal
  }

} // namespace 'xeona'

// ---------------------------------------------------------
//  FREE FUNCTION   : nearZero <>
// ---------------------------------------------------------

namespace xeona
{
  template <typename F>                      // 'F' in { float double }
  bool
  nearZero
  (const F                a,
   const xeona::Precision precision)
  {
    return xeona::almostEqual(a, static_cast<F>(0.0), precision);
  }

} // namespace 'xeona'

// ---------------------------------------------------------
//  FREE FUNCTION   : xeona::roundZero <> (ULP version)
// ---------------------------------------------------------

namespace xeona
{
  template <typename F>                      // 'F' in { float double }
  bool
  roundZero
  (F&                     a,
   const xeona::Precision precision)
  {
    const F    zero     = static_cast<F>(0.0);
    const bool nearZero = xeona::almostEqual(zero, a, precision);
    if ( nearZero ) a = zero;                // make exact
    return nearZero;
  }

} // namespace 'xeona'

// ---------------------------------------------------------
//  FREE FUNCTION   : xeona::roundZero <> (trip version)
// ---------------------------------------------------------
//  Description  : round a floating point number to exact zero as appropriate
//  Role         : general use
//  On fail      : returns 'false', never throws
//  Techniques   : 'Boost.MathToolkit' library, templates, tolerates 'inf's and 'NaN's
//  Status       : complete
//
//  Design notes
//
//      Makes use of simple comparison.
//
// ---------------------------------------------------------

namespace xeona
{
  template <typename F>                      // 'F' in { float double }
  bool                                       // 'true' if rounded to exact zero
  roundZero
  (F&      a,
   const F threshold)
  {
    const bool nearZero = ( std::abs(a) < std::abs(threshold) );
    if ( nearZero ) a = static_cast<F>(0.0);
    return nearZero;
  }
}

// ---------------------------------------------------------
//  EXPLICIT TEMPLATE INSTANTIATIONS
// ---------------------------------------------------------

// these function templates will also accept the 'long double' type

template bool xeona::almostEqual(const float , const float , const xeona::Precision);
template bool xeona::almostEqual(const double, const double, const xeona::Precision);

template bool xeona::nearZero(const float , const xeona::Precision);
template bool xeona::nearZero(const double, const xeona::Precision);

template bool xeona::roundZero(float& , const xeona::Precision);
template bool xeona::roundZero(double&, const xeona::Precision);

template bool xeona::roundZero(float& , const float);
template bool xeona::roundZero(double&, const double);

//  end of file

