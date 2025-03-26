//  XEONA ENERGY SYSTEMS MODELING ENVIRONMENT
//
//  file-create-name : si3units.h
//  file-create-date : Thu 17-Dec-2009 13:48 UTC
//  file-initiator   : Robbie Morrison <robbie@actrix.co.nz>
//
//  file-role        : support for engineering format and SI prefixes / header
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
//  $URL: file:///home/robbie/svn-root/xeona/futz/trunk/xeona1/c/si3units.h $
//
//  GENERAL NOTES FOR THIS FILE
//
//  Heldoorn (2002) provides an excellent review of the SI
//  system, particularly in relation to usage and typesetting.
//
//    Heldoorn, Marcel.  2002.  The SIunits package : support
//      for the International System of Units [pdf file:
//      SIunits.pdf]

//  HEADER GUARD

#ifndef _SI3UNITS_H_
#define _SI3UNITS_H_

//  LOCAL AND SYSTEM INCLUDES

#include "../a/logger_fwd.h"  // some forward declarations from "logger.h"
#include "../c/smart_ptr.h"   // toggle between Boost and TR1 smart pointers

#include <string>             // C++ strings
#include <sstream>            // string-streams

//  CODE

  // ---------------------------------------------------------
  //  ENUM            : xeona::SiPrefix
  // ---------------------------------------------------------

namespace xeona
{
  enum SiPrefix
    {
      tooSmall     =  -1,                    // nonsensical value
      tooLarge     =  +1,                    // nonsensical value

      notSet       =  +2,                    // nonsensical value

      yocto        = -24,                    // y
      zepto        = -21,                    // z
      atto         = -18,                    // a
      femto        = -15,                    // f
      pico         = -12,                    // p
      nano         =  -9,                    // n
      micro        =  -6,                    // u (more properly \mu)
      milli        =  -3,                    // m
      none         =   0,                    //   (a space is currently used)
      kilo         =  +3,                    // k
      mega         =  +6,                    // M
      giga         =  +9,                    // G
      tera         = +12,                    // T
      peta         = +15,                    // P
      exa          = +18,                    // E
      zetta        = +21,                    // Z
      yotta        = +24                     // Y
    };

} // namespace 'xeona'

namespace xeona
{
  // ---------------------------------------------------------
  //  FREE FUNCTION   : xeona::getSiPrefixStr
  // ---------------------------------------------------------
  //  Description  : transform 'xeona::SiPrefix' to string
  //  Role         : general
  //  Techniques   : (nothing special)
  //  Status       : complete
  // ---------------------------------------------------------

  std::string
  getSiPrefixStr
  (xeona::SiPrefix prefix);

  // ---------------------------------------------------------
  //  FREE FUNCTION   : xeona::getEngineeringPrefix
  // ---------------------------------------------------------
  //  Description  : obtain engineering prefix for 'value'
  //  Role         : general, for instance 8000 returns 'xeona::kilo'
  //  Techniques   : (nothing special)
  //  Status       : complete
  // ---------------------------------------------------------

  xeona::SiPrefix
  getEngineeringPrefix
  (const double quantity,
   const int    shift = 0);                  // 1 means accept 4 digit numbers

  // ---------------------------------------------------------
  //  FREE FUNCTION   : xeona::fmtEngineering
  // ---------------------------------------------------------
  //  Description  : see example
  //  Role         : general
  //  Techniques   : relies on 'xeona::fmtEngineeringFix'
  //  Status       : complete
  //
  //  Examples
  //
  //      value  : 0.0000007700
  //      final  : 770.00 n
  //
  //      value  : -7.7
  //      final  : -7.70        (with two trailing spaces)
  //
  //      value  : -7777.7
  //      final  : -7.79 k
  //
  //      value  : -77777777777777.7
  //      final  : -77.79 T
  //
  // ---------------------------------------------------------

  std::string
  fmtEngineering
  (const double   quantity,                  // measured value
   const unsigned decimals = 2);             // number of decimals places

  // ---------------------------------------------------------
  //  FREE FUNCTION   : xeona::fmtEngineeringFix
  // ---------------------------------------------------------
  //  Description  : as above, but use given 'xeona::SiPrefix'
  //  Role         : general
  //  Techniques   : (nothing special)
  //  Status       : complete
  //
  //  Design notes
  //
  //      Read code regarding exact behavior.
  //
  // ---------------------------------------------------------

  std::string
  fmtEngineeringFix
  (double                quantity,           // measured value
   const xeona::SiPrefix level,              // such as 'xeona::kilo'
   const unsigned        decimals    = 2,    // number of decimal places
   const double          closeToZero = 1.0e-10);

} // namespace 'xeona'

#endif // _SI3UNITS_H_

//  end of file

