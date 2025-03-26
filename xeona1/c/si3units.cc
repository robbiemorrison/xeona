//  XEONA ENERGY SYSTEMS MODELING ENVIRONMENT
//
//  file-create-name : si3units.cc
//  file-create-date : Thu 17-Dec-2009 13:48 UTC
//  file-initiator   : Robbie Morrison <robbie@actrix.co.nz>
//
//  file-role        : support for engineering format and SI prefixes / implementation
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
//  $URL: file:///home/robbie/svn-root/xeona/futz/trunk/xeona1/c/si3units.cc $
//
//  GENERAL NOTES FOR THIS FILE

//  LOCAL AND SYSTEM INCLUDES

#include "si3units.h"         // companion header for this file (place first)

#include "../c/util2.h"       // free functions which offer general utilities 2

#include "../a/logger.h"      // standard logging functionality (as required)
#include "../c/smart_ptr.h"   // toggle between Boost and TR1 smart pointers
#include ".././common.h"      // common definitions for project (place last)

#include <iomanip>            // setw() and family
#include <limits>             // numeric_limits<T>::infinity() and similar
#include <string>             // C++ strings

#include <cmath>              // C-style maths, abs(), ceil(), floor(), sqrt()

//  CODE

namespace xeona
{
  // ---------------------------------------------------------
  //  FREE FUNCTION   : xeona::getSiPrefixStr
  // ---------------------------------------------------------

  std::string
  getSiPrefixStr
  (xeona::SiPrefix prefix)
  {
    switch ( prefix )
      {
      case xeona::yocto: return "y";
      case xeona::zepto: return "z";
      case xeona::atto:  return "a";
      case xeona::femto: return "f";
      case xeona::pico:  return "p";
      case xeona::nano:  return "n";
      case xeona::micro: return "u";         // as a substitute for \mu
      case xeona::milli: return "m";
      case xeona::none:  return " ";         // space char
      case xeona::kilo:  return "k";
      case xeona::mega:  return "M";
      case xeona::giga:  return "G";
      case xeona::tera:  return "T";
      case xeona::peta:  return "P";
      case xeona::exa:   return "E";
      case xeona::zetta: return "Z";
      case xeona::yotta: return "Y";

      case xeona::tooSmall:
      case xeona::tooLarge:
        {
          static logga::spLogger logger = logga::ptrLogStream();
          logger->repx(logga::warn, "unsupported SI prefix, enum", prefix);
          return "";
        }

      case xeona::notSet:
        {
          static logga::spLogger logger = logga::ptrLogStream();
          logger->repx(logga::warn, "SI prefix currently unset, enum", prefix);
          return "";
        }

      default:
        std::clog << "** coding error 01 in source file " << __FILE__ << std::endl;
        return "";

      } // function 'xeona::getSiPrefixStr'
  } // namespace 'xeona'

  // ---------------------------------------------------------
  //  FREE FUNCTION   : xeona::getEngineeringPrefix
  // ---------------------------------------------------------

  xeona::SiPrefix
  getEngineeringPrefix
  (const double quantity,
   const int    shift)                       // note default value
  {
    // threshold
    const double closeToZero = 1.0e-15;      // CAUTION: can reset here

    // bind logger
    static logga::spLogger logger = logga::ptrLogStream();

    // preamble
    double absol = std::abs(quantity);       // refer <cmath>

    // special cases
    if ( absol == 0.0 )                      // strictly floating point zero
      {
        return xeona::none;
      }
    else if ( absol < closeToZero )          // close-to-zero rounding
      {
        logger->repx(logga::adhc, "quantity supplied considered zero", quantity);
        return xeona::none;
      }
    else if ( absol == std::numeric_limits<double>::infinity() )      // refer <limits>
      {
        return xeona::none;
      }

    // apply shift such that 'shift' = 1 means 9999.9 maps to
    // itself and not 9.9999 k
    absol *= std::pow(10.0, -shift);

    // recover SI prefix -- note the difference between "<" and
    // "<=", the former blocks 1000 whereas the latter allows it
    xeona::SiPrefix scale = xeona::notSet;

    if ( absol < 1.0e-24 )
      {
        scale = xeona::tooSmall;
        logger->repx(logga::warn, "excessively small quantity supplied", quantity);
      }
    else if ( absol < 1.0e-21 ) scale = xeona::yocto;
    else if ( absol < 1.0e-18 ) scale = xeona::zepto;
    else if ( absol < 1.0e-15 ) scale = xeona::atto;
    else if ( absol < 1.0e-12 ) scale = xeona::femto;
    else if ( absol < 1.0e-09 ) scale = xeona::pico;
    else if ( absol < 1.0e-06 ) scale = xeona::nano;
    else if ( absol < 1.0e-03 ) scale = xeona::micro;
    else if ( absol < 1.0e+00 ) scale = xeona::milli;
    else if ( absol < 1.0e+03 ) scale = xeona::none;
    else if ( absol < 1.0e+06 ) scale = xeona::kilo;
    else if ( absol < 1.0e+09 ) scale = xeona::mega;
    else if ( absol < 1.0e+12 ) scale = xeona::giga;
    else if ( absol < 1.0e+15 ) scale = xeona::tera;
    else if ( absol < 1.0e+18 ) scale = xeona::peta;
    else if ( absol < 1.0e+21 ) scale = xeona::exa;
    else if ( absol < 1.0e+24 ) scale = xeona::zetta;
    else if ( absol < 1.0e+27 ) scale = xeona::yotta;
    else
      {
        scale = xeona::tooLarge;
        logger->repx(logga::warn, "excessively large quantity supplied", quantity);
      }

    return scale;

  } // function 'xeona::getEngineeringPrefix'

  // ---------------------------------------------------------
  //  FREE FUNCTION   : xeona::fmtEngineering
  // ---------------------------------------------------------

  std::string
  fmtEngineering
  (const double   quantity,
   const unsigned decimals)                  // note default value
  {
    const xeona::SiPrefix prefix = xeona::getEngineeringPrefix(quantity);
    return xeona::fmtEngineeringFix(quantity, prefix, decimals);
  }

  // ---------------------------------------------------------
  //  FREE FUNCTION   : xeona::fmtEngineeringFix
  // ---------------------------------------------------------

  std::string
  fmtEngineeringFix
  (double                quantity,
   const xeona::SiPrefix level,
   const unsigned        decimals,
   const double          closeToZero)
  {
    // constants
    const std::string unitSeparator = " ";   // CAUTION: can also be ""
    const std::string zero          = "0";   // CAUTION: can be "0.0"

    // rescale
    const std::string prefixStr = xeona::getSiPrefixStr(level);
    const double requantity     = quantity / std::pow(10.0, level);

    // close-to-zero rounding as appropriate
    if ( xeona::zero )
      {
        if ( std::abs(quantity) < closeToZero ) quantity = 0.0;
      }

    // stringification
    std::ostringstream oss;
    oss << std::fixed << std::setprecision(decimals) << requantity;
    std::string requantityStr = oss.str();

    // return strings
    if ( xeona::isInf(quantity) )                      // plus or minus inf
      {
        return requantityStr + unitSeparator + " ";    // skip prefix string
      }
    if ( xeona::isNan(quantity) )                      // NaN (the sign is meaningless)
      {
        return requantityStr + unitSeparator + " ";    // skip prefix string
      }
    else if ( quantity == 0.0 )                        // exact match
      {
        return zero + unitSeparator + " ";             // string 'zero' is defined above
      }
    else
      {
        return requantityStr + unitSeparator + prefixStr;
      }

  } // function 'xeona::fmtEngineeringFix'

} // namespace 'xeona'

//  end of file

