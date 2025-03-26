//  XEONA ENERGY SYSTEMS MODELING ENVIRONMENT
//
//  file-create-name : extunits.cc
//  file-create-date : Wed 02-Dec-2009 17:24 UTC
//  file-initiator   : Robbie Morrison <robbie@actrix.co.nz>
//
//  file-role        : quantifying extensity enums and interpretation / implementation
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
//  $URL: file:///home/robbie/svn-root/xeona/futz/trunk/xeona1/c/extunits.cc $
//
//  GENERAL NOTES FOR THIS FILE

//  LOCAL AND SYSTEM INCLUDES

#include "extunits.h"         // companion header for this file (place first)

#include "../a/logger.h"      // standard logging functionality (as required)
#include "../c/smart_ptr.h"   // toggle between Boost and TR1 smart pointers
#include ".././common.h"      // common definitions for project (place last)

#include <string>             // C++ strings
#include <sstream>            // string-streams

//  CODE

namespace xeona
{
  std::string
  interpretExtensity
  (const xeona::ExtensityUnit extensity)
  {
    std::string buf = "(not overwritten)";
    switch ( extensity )
      {
      case xeona::notSpecified: buf = "(not specified)"; break;
      case xeona::joule:        buf = "J";               break;
      case xeona::kilogram:     buf = "kg";              break;
      case xeona::uoa:          buf = "$";               break;
      case xeona::metreSq:      buf = "m2";              break;
      default: std::clog << "** coding error 01 in source file " << __FILE__ << std::endl;
      }
    return buf;
  }

} // namespace 'xeona'

//  end of file

