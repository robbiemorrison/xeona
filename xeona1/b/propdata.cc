//  XEONA ENERGY SYSTEMS MODELING ENVIRONMENT
//
//  file-create-name : propdata.cc
//  file-create-date : Thu 05-Feb-2009 11:43 UTC
//  file-initiator   : Robbie Morrison <robbie@actrix.co.nz>
//
//  file-role        : mass-based commodity property data / implementation
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
//  $URL: file:///home/robbie/svn-root/xeona/futz/trunk/xeona1/b/propdata.cc $

//  LOCAL AND SYSTEM INCLUDES

#include "propdata.h"         // companion header for this file (place first)

#include <limits>             // numeric_limits<T>::infinity() and similar

//  DEFINITIONS: some typical MASS-specific physico-chemical property values
//  source: Wikipedia, July 2008

namespace
{
  const double TOGET = std::numeric_limits<double>::quiet_NaN();
} // unnamed namespace

namespace xeona
{
  const double massCp_steam         = +2080;      // [J/kgC]
  const double massCp_ice           = +2050;      // [J/kgC]
  const double massCp_liquidWater   = +4181;      // [J/kgC]

  const double massHhv_hardCoal     = +24e6;      // [J/kg]
  const double massHhv_lignite      = ::TOGET;    // [J/kg]
  const double massHhv_natGas       = +54e6;      // [J/kg]
  const double massHhv_wood         = +15e6;      // [J/kg]

  const double massCO2_hardCoal     = 1.83;       // [kg/kg]
  const double massCO2_lignite      = ::TOGET;    // [kg/kg]
  const double massCO2_natGas       = ::TOGET;    // [kg/kg]
  const double massCO2_wood         = ::TOGET;    // [kg/kg]

  const double massGhg_hardCoal     = ::TOGET;    // [kg/kg]
  const double massGhg_lignite      = ::TOGET;    // [kg/kg]
  const double massGhg_natGas       = ::TOGET;    // [kg/kg]
  const double massGhg_wood         = ::TOGET;    // [kg/kg]

  const double massGray_hardCoal    = ::TOGET;    // [kg/kg]
  const double massGray_lignite     = ::TOGET;    // [kg/kg]
  const double massGray_natGas      = ::TOGET;    // [kg/kg]
  const double massGray_wood        = ::TOGET;    // [kg/kg]
} // namespace xeona

//  end of file

