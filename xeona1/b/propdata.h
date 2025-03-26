//  XEONA ENERGY SYSTEMS MODELING ENVIRONMENT
//
//  file-create-name : propdata.h
//  file-create-date : Thu 05-Feb-2009 11:42 UTC
//  file-initiator   : Robbie Morrison <robbie@actrix.co.nz>
//
//  file-role        : mass-based commodity property data / header
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
//  $URL: file:///home/robbie/svn-root/xeona/futz/trunk/xeona1/b/propdata.h $
//
//  GENERAL NOTES FOR THIS FILE
//
//  This file declares some commodity properties that should be
//  useful to entity authors.

//  HEADER GUARD

#ifndef _PROPDATA_H_
#define _PROPDATA_H_

//  LOCAL AND SYSTEM INCLUDES

//  no hash-includes required

//  DEFINITIONS: some typical MASS-specific physico-chemical property values

namespace xeona
{
  extern const double massCp_steam;          // constant pressure heat capacity
  extern const double massCp_ice;            // constant pressure heat capacity
  extern const double massCp_liquidWater;    // constant pressure heat capacity

  extern const double massHhv_hardCoal;      // enthalpy of combustion, condensed water
  extern const double massHhv_lignite;       // enthalpy of combustion, condensed water
  extern const double massHhv_natGas;        // enthalpy of combustion, condensed water
  extern const double massHhv_wood;          // enthalpy of combustion, condensed water

  extern const double massCO2_hardCoal;      // carbon-dioxide on oxidation
  extern const double massCO2_lignite;       // carbon-dioxide on oxidation
  extern const double massCO2_natGas;        // carbon-dioxide on oxidation
  extern const double massCO2_wood;          // carbon-dioxide on oxidation

  extern const double massGhg_hardCoal;      // co2-equiv on oxidation, direct
  extern const double massGhg_lignite;       // co2-equiv on oxidation, direct
  extern const double massGhg_natGas;        // co2-equiv on oxidation, direct
  extern const double massGhg_wood;          // co2-equiv on oxidation, direct

  extern const double massGray_hardCoal;     // co2-equiv on oxidation, upstream also
  extern const double massGray_lignite;      // co2-equiv on oxidation, upstream also
  extern const double massGray_natGas;       // co2-equiv on oxidation, upstream also
  extern const double massGray_wood;         // co2-equiv on oxidation, upstream also

} // namespace xeona

#endif // _PROPDATA_H_

//  end of file

