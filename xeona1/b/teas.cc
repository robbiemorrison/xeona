//  XEONA ENERGY SYSTEMS MODELING ENVIRONMENT
//
//  file-create-name : asset.cc
//  file-create-date : Tue 26-Aug-2008 14:15 UTC
//  file-initiator   : Robbie Morrison <robbie@actrix.co.nz>
//
//  file-role        : technical asset entity / implementation
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
//  $URL: file:///home/robbie/svn-root/xeona/futz/trunk/xeona1/b/teas.cc $

//  LOCAL AND SYSTEM INCLUDES

#include "teas.h"             // companion header for this file (place first)

#include "../c/recset.h"      // records and fields and also record-sets
#include "../b/bandtaf.h"     // banded tariff set and support

#include "../a/logger.h"      // standard logging functionality (as required)
#include "../c/smart_ptr.h"   // toggle between Boost and TR1 smart pointers
#include ".././common.h"      // common definitions for project (place last)

#include <string>             // C++ strings
#include <sstream>            // string-streams

//  CODE

// ---------------------------------------------------------
//  MEMBER FUNCTION : TechnicalAsset
// ---------------------------------------------------------

TechnicalAsset::TechnicalAsset
(const std::string entityId,
 Record&           record,
 const int         commitmentModeSum) :
  CostRegister(record),
  Block(entityId, record),
  TicToc(commitmentModeSum),
  d_ceilingDuty(-1.0),                       // nonsensical value
  d_floorDuty(-1.0),                         // nonsensical value
  d_cogenHeatLeadWeight(0.0)                 // zero is power led
{
  s_logger->repx(logga::xtra, "constructor call", getIdAndKind());
}

// ---------------------------------------------------------
//  MEMBER FUNCTION : ~TechnicalAsset
// ---------------------------------------------------------

TechnicalAsset::~TechnicalAsset()
{
  s_logger->repx(logga::adhc, "destructor call", getIdAndKind());
}

// ---------------------------------------------------------
//  MEMBER FUNCTION : getCeilingDuty
// ---------------------------------------------------------
//  Description  : get current ceiling duty (conceptually equivalent to current capacity)
//  Role         : called by asset operators, most likely those preparing bid sets
//  Techniques   : (nothing special)
//  Status       : complete
//
//  Design notes
//
//      If required, 'd_ceilingDuty' should be set in the
//      relevant technical asset constrain call.
//
//      It is most likely that asset operators requiring this
//      information will be preparing LMP (nodal pricing) bid
//      sets.
//
// ---------------------------------------------------------

double
TechnicalAsset::getCeilingDuty() const
{
  // initial reporting
  s_logger->repx(logga::adhc, "entering member function", d_ceilingDuty);

  // integrity checks
  if ( d_ceilingDuty < 0 )
    {
      s_logger->repx(logga::warn, "ceiling duty not reset", getIdAndKind());
    }

  // return
  return d_ceilingDuty;
}

// ---------------------------------------------------------
//  MEMBER FUNCTION : getFloorDuty
// ---------------------------------------------------------
//
//  see documentation for 'getCeilingDuty'
//
// ---------------------------------------------------------

double
TechnicalAsset::getFloorDuty() const
{
  // initial reporting
  s_logger->repx(logga::adhc, "entering member function", d_floorDuty);

  // integrity checks
  if ( d_floorDuty < 0 )
    {
      s_logger->repx(logga::warn, "floor duty not reset", getIdAndKind());
    }

  // return
  return d_floorDuty;
}

// ---------------------------------------------------------
//  MEMBER FUNCTION : setCogenHeatWeight
// ---------------------------------------------------------

void
TechnicalAsset::setCogenHeatWeight
(const double cogenHeatLeadWeight)
{
  s_logger->repx(logga::adhc, "entering member function, weighting", cogenHeatLeadWeight);
  d_cogenHeatLeadWeight = cogenHeatLeadWeight;
}

// ---------------------------------------------------------
//  MEMBER FUNCTION : getCogenHeatWeight
// ---------------------------------------------------------

const double
TechnicalAsset::getCogenHeatWeight() const
{
  return d_cogenHeatLeadWeight;
}

// ---------------------------------------------------------
//  MEMBER FUNCTION : getPriorDuty
// ---------------------------------------------------------

double                                       // prior duty
TechnicalAsset::getPriorDuty() const         // 'NaN' for step 0
{
  return d_dutyStats.last();                 // 'NaN' if object not yet filled
}

// ---------------------------------------------------------
//  MEMBER FUNCTION : getPriorSize
// ---------------------------------------------------------

double                                       // prior size
TechnicalAsset::getPriorSize() const         // 'NaN' for step 0
{
  return d_sizeStats.last();                 // 'NaN' if object not yet filled
}

// ---------------------------------------------------------
//  MEMBER FUNCTION : obtainTariffSet
// ---------------------------------------------------------

shared_ptr<BandedTariffSet>                // current tariff set for adaptive behavior
TechnicalAsset::obtainTariffSet() const
{
  // warning
  s_logger->repx(logga::warn, "function should have been redefined", "");

  // active code
  return shared_ptr<BandedTariffSet>();
}

// ---------------------------------------------------------
//  MEMBER FUNCTION : passBuildingOccData
// ---------------------------------------------------------

void
TechnicalAsset::passBuildingOccData
(const double temperatureSetPoint,           // temperature set point [C] (Celsius)
 const double activityLoad,                  // load contribution but not electricity [W]
 const double electricityDemand)             // electricity demand [W]
{
  // warning
  s_logger->repx(logga::warn, "function should have been redefined", "");
}

//  end of file

