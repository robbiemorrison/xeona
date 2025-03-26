//  XEONA ENERGY SYSTEMS MODELING ENVIRONMENT
//
//  file-create-name : commods01.cc
//  file-create-date : Tue 05-May-2009 18:35 UTC
//  file-initiator   : Robbie Morrison <robbie@actrix.co.nz>
//
//  file-role        : concrete commodities 1 / implementation
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
//  $URL: file:///home/robbie/svn-root/xeona/futz/trunk/xeona1/b/commods01.cc $
//
//  GENERAL NOTES FOR THIS FILE

//  AD-HOC NOTES
//
//  See r2570 for use of retie code.

//  LOCAL AND SYSTEM INCLUDES

#include "commods01.h"        // companion header for this file (place first)

#include "../a/logger.h"      // standard logging functionality (as required)
#include "../c/smart_ptr.h"   // toggle between Boost and TR1 smart pointers
#include ".././common.h"      // common definitions for project (place last)

#include <string>             // C++ strings
#include <sstream>            // string-streams

//  CODE

// ---------------------------------------------------------
//  CLASS           : CmOxidBiocoal
// ---------------------------------------------------------

// ---------------------------------------------------------
//  MEMBER FUNCTION : CmOxidBiocoal
// ---------------------------------------------------------

// CREATORS

CmOxidBiocoal::CmOxidBiocoal
(const std::string entityId,
 Record&           record) :
  CmOxidize(entityId, record)
{
  // initial reporting
  s_logger->repx(logga::dbug, "constructor call", getIdAndKind());

  // integrity checks
  if ( d_specCombustionEnthalpy < 10.0e+06 || d_specCombustionEnthalpy > 30.0e+06 )
    {
      s_logger->repx(logga::info,
                     "odd specific combustion enthalpy",
                     d_specCombustionEnthalpy);
    }

  // builtin remark
  d_builtinRemark = "beta";
}

// ---------------------------------------------------------
//  MEMBER FUNCTION : ~CmOxidBiocoal
// ---------------------------------------------------------

CmOxidBiocoal::~CmOxidBiocoal()
{
  s_logger->repx(logga::adhc, "destructor call", "");
}

// ---------------------------------------------------------
//  CLASS           : CmOxidGas
// ---------------------------------------------------------

// ---------------------------------------------------------
//  MEMBER FUNCTION : CmOxidGas
// ---------------------------------------------------------

// CREATORS

CmOxidGas::CmOxidGas
(const std::string entityId,
 Record&           record) :
  CmOxidize(entityId, record),
  d_specGwp(record.tieSingle<double>("spec-gwp"))
{
  // initial reporting
  s_logger->repx(logga::dbug, "constructor call", getIdAndKind());

  // builtin remark
  d_builtinRemark = "beta";
}

// ---------------------------------------------------------
//  MEMBER FUNCTION : ~CmOxidGas
// ---------------------------------------------------------

CmOxidGas::~CmOxidGas()
{
  s_logger->repx(logga::adhc, "destructor call", "");
}

// ---------------------------------------------------------
//  MEMBER FUNCTION : ~CmOxidGas
// ---------------------------------------------------------

double
CmOxidGas::getSpecGwp() const
{
  return d_specGwp;
}

// ---------------------------------------------------------
//  CLASS           : CmOxidNaturalGas
// ---------------------------------------------------------
//  Description  : fossil natural gas
//  Role         : concrete commodity
//  Techniques   : (nothing special)
//  Status       : complete
//
//  Design notes
//
//      The specific carbon-dioxide content assumes pure methane
//      and complete combustion.  The specific CO2e value is,
//      indirectly, from the California Air Resources Board
//      (CARB) and is empirical.  The specific
//      global-warming-potential again assumes pure methane and
//      is from the IPCC (the GWPs are mass-specific in any
//      case).
//
//      Anders (2008) states: "The project team used the fuel
//      emissions factors for natural gas combustion provided by
//      CARB [California Air Resources Board]:
//
//        - CH4 - 1.00e-06 g/BTU
//        - CO2 - 0.053 g/BTU
//        - N2O - 1.00e-07 g/BTU"
//
//  References
//
//      Anders, Scott J.  2008.  San Diego County greenhouse gas
//          inventory : an analysis of regional emissions and
//          strategies to achieve AB 32 targets : natural gas
//          end-use report.
//
// ---------------------------------------------------------

// ---------------------------------------------------------
//  MEMBER FUNCTION : CmOxidNaturalGas
// ---------------------------------------------------------

// CREATORS

CmOxidNaturalGas::CmOxidNaturalGas
(const std::string entityId,
 Record&           record) :
  CmOxidGas(entityId, record)
{
  // initial reporting
  s_logger->repx(logga::dbug, "constructor call", getIdAndKind());

  // integrity checks
  if ( d_specCombustionEnthalpy < 30.0e+06 || d_specCombustionEnthalpy > 58.0e+06 )
    {
      s_logger->repx(logga::info,
                     "odd specific combustion enthalpy",
                     d_specCombustionEnthalpy);
    }

  // builtin remark
  d_builtinRemark = "beta";
}

// ---------------------------------------------------------
//  MEMBER FUNCTION : ~CmOxidNaturalGas
// ---------------------------------------------------------

CmOxidNaturalGas::~CmOxidNaturalGas()
{
  s_logger->repx(logga::adhc, "destructor call", "");
}

// ---------------------------------------------------------
//  CLASS           : CmOxidHydrogen
// ---------------------------------------------------------
//  Description  : hydrogen gas
//  Role         : concrete combustion
//  Techniques   : (nothing special)
//  Status       : complete
// ---------------------------------------------------------

// ---------------------------------------------------------
//  MEMBER FUNCTION : CmOxidHydrogen
// ---------------------------------------------------------

// CREATORS

CmOxidHydrogen::CmOxidHydrogen
(const std::string entityId,
 Record&           record) :
  CmOxidGas(entityId, record)
{
  // initial reporting
  s_logger->repx(logga::dbug, "constructor call", getIdAndKind());

  // integrity checks
  if ( d_specCombustionEnthalpy < 135.0e+06 || d_specCombustionEnthalpy > 145.0e+06 )
    {
      s_logger->repx(logga::info,
                     "odd specific combustion enthalpy",
                     d_specCombustionEnthalpy);
    }

  // builtin remark
  d_builtinRemark = "beta";
}

// ---------------------------------------------------------
//  MEMBER FUNCTION : ~CmOxidHydrogen
// ---------------------------------------------------------

CmOxidHydrogen::~CmOxidHydrogen()
{
  s_logger->repx(logga::adhc, "destructor call", "");
}

//  end of file

