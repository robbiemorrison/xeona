//  XEONA ENERGY SYSTEMS MODELING ENVIRONMENT
//
//  file-create-name : commods.cc
//  file-create-date : Wed 30-Jul-2008 07:37 UTC
//  file-initiator   : Robbie Morrison <robbie@actrix.co.nz>
//
//  file-role        : commodities hierarchy / implementation
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
//  $URL: file:///home/robbie/svn-root/xeona/futz/trunk/xeona1/b/commods.cc $

//  LOCAL AND SYSTEM INCLUDES

#include "commods.h"          // companion header for this file (place first)

#include "../c/ghouse.h"      // global warming potential support
#include "../c/extunits.h"    // quantifying extensity enums and interpretation
#include "../b/entity.h"      // entity base class plus lazy linking
#include "../a/logger.h"      // standard logging functionality (as required)
#include "../c/smart_ptr.h"   // toggle between Boost and TR1 smart pointers
#include ".././common.h"      // common definitions for project (place last)

#include <limits>             // numeric_limits<T>::infinity() and similar
#include <string>             // C++ strings
#include <sstream>            // string-streams

//  CODE

// ---------------------------------------------------------
//  CLASS           : Commodity (abstract)
// ---------------------------------------------------------

Commodity::Commodity
(const std::string          entityId,
 Record&                    record,
 const xeona::ExtensityUnit quantifyingExtensity) :
  FullEntity(entityId, record),
  d_quantifyingExtensity(quantifyingExtensity)
{
  s_logger->repx(logga::xtra, "constructor call", getIdAndKind());
}

Commodity::~Commodity()
{
  s_logger->repx(logga::adhc, "destructor call", getIdAndKind());
}

int                                          // minus one indicates some problem
Commodity::getInterfacePairs() const
{
  const int interfaces = getUseCount() - 1;  // should be in {0,2,4,..}
  if ( interfaces == -1 )
    {
      s_logger->repx(logga::warn, "commodity not factory constructed", interfaces);
      return -1;
    }
  // odd/even test
  double check = static_cast<double>(interfaces)/2.0;
  if ( check != std::floor(check) )          // see <cmath>
    {                                        // 'interfaces' was odd
      s_logger->repx(logga::dbug, "interface count not properly paired", interfaces);
      return -1;
    }
  return interfaces / 2;
}

// ---------------------------------------------------------
//  CLASS           : CommodityJoule
// ---------------------------------------------------------

CommodityJoule::CommodityJoule
(const std::string entityId,
 Record&           record) :
  Commodity(entityId, record, xeona::joule)
{
  s_logger->repx(logga::xtra, "constructor call", getIdAndKind());
}

CommodityJoule::~CommodityJoule()
{
  s_logger->repx(logga::adhc, "destructor call", getIdAndKind());
}

// ACCESSORS

xeona::ExtensityUnit
CommodityJoule::getExtensity() const
{
  return d_quantifyingExtensity;
}

std::string
CommodityJoule::getExtensityStr() const
{
  return xeona::interpretExtensity(d_quantifyingExtensity);
}

// ---------------------------------------------------------
//  CLASS           : CommodityKilogram
// ---------------------------------------------------------

CommodityKilogram::CommodityKilogram
(const std::string entityId,
 Record&           record) :
  Commodity(entityId, record, xeona::kilogram)
{
  s_logger->repx(logga::xtra, "constructor call", getIdAndKind());
}

CommodityKilogram::~CommodityKilogram()
{
  s_logger->repx(logga::adhc, "destructor call", getIdAndKind());
}

// ACCESSORS

xeona::ExtensityUnit
CommodityKilogram::getExtensity() const
{
  return d_quantifyingExtensity;
}

std::string
CommodityKilogram::getExtensityStr() const
{
  return xeona::interpretExtensity(d_quantifyingExtensity);
}

// ---------------------------------------------------------
//  CLASS           : CommodityUOA
// ---------------------------------------------------------

CommodityUOA::CommodityUOA
(const std::string entityId,
 Record&           record) :
  Commodity(entityId, record, xeona::uoa)
{
  s_logger->repx(logga::xtra, "constructor call", getIdAndKind());
}

CommodityUOA::~CommodityUOA()
{
  s_logger->repx(logga::adhc, "destructor call", getIdAndKind());
}

// ACCESSORS

xeona::ExtensityUnit
CommodityUOA::getExtensity() const
{
  return d_quantifyingExtensity;
}

std::string
CommodityUOA::getExtensityStr() const
{
  return xeona::interpretExtensity(d_quantifyingExtensity);
}

// ---------------------------------------------------------
//  CLASS           : CommodityMetreSq
// ---------------------------------------------------------

CommodityMetreSq::CommodityMetreSq
(const std::string entityId,
 Record&           record) :
  Commodity(entityId, record, xeona::uoa)
{
  s_logger->repx(logga::xtra, "constructor call", getIdAndKind());
}

CommodityMetreSq::~CommodityMetreSq()
{
  s_logger->repx(logga::adhc, "destructor call", getIdAndKind());
}

// ACCESSORS

xeona::ExtensityUnit
CommodityMetreSq::getExtensity() const
{
  return d_quantifyingExtensity;
}

std::string
CommodityMetreSq::getExtensityStr() const
{
  return xeona::interpretExtensity(d_quantifyingExtensity);
}

// ---------------------------------------------------------
//  CLASS           : CmOxidize (quantified in kg)
// ---------------------------------------------------------

// CREATORS

CmOxidize::CmOxidize
(const std::string entityId,
 Record&           record) :
  CommodityKilogram(entityId, record),
  d_specCombustionEnthalpy(record.tieSingle<double>("spec-combustion-enthalpy")),
  d_specCarbonDioxide(record.tieSingle<double>("spec-carbon-dioxide")),
  d_specCo2equiv(record.tieSingle<double>("spec-co2-equiv"))
{
  s_logger->repx(logga::dbug, "constructor call", getIdAndKind());
  d_builtinRemark = "beta";
}

CmOxidize::~CmOxidize()
{
  s_logger->repx(logga::adhc, "destructor call", getIdAndKind());
}

// ACCESSORS

double
CmOxidize::getSpecCombEnthalpy() const
{
  return d_specCombustionEnthalpy;
}

double
CmOxidize::getSpecCarbonDioxide() const
{
  return d_specCarbonDioxide;
}

double
CmOxidize::getSpecCo2equiv() const
{
  return d_specCo2equiv;
}

Gwp100Bundle
CmOxidize::getSpecGhgs() const
{
  // initial reporting -- noting that more input data would be
  // needed to implement this function completely
  s_logger->repx(logga::warn, "function not properly implemented", "");

  // active code
  Gwp100Bundle gwps;
  gwps.co2 = d_specCarbonDioxide;
  return gwps;
}

// ---------------------------------------------------------
//  CLASS           : CmCarbonSeq (quantified in kg)
// ---------------------------------------------------------

CmCarbonSeq::CmCarbonSeq
(const std::string entityId,
 Record&           record) :
  CommodityKilogram(entityId, record),
  d_pressures(record.tieTimeseries<double>("pressures"))
{
  s_logger->repx(logga::dbug, "constructor call", getIdAndKind());
  d_builtinRemark = "beta";
}

CmCarbonSeq::~CmCarbonSeq()
{
  s_logger->repx(logga::adhc, "destructor call", getIdAndKind());
}

double
CmCarbonSeq::getPressure
(const int step) const
{
  return d_pressures->at(step);
}

void
CmCarbonSeq::setPressure
(const int    step,
 const double pressure)
{
  d_pressures->at(step) = pressure;
}

// ---------------------------------------------------------
//  CLASS           : CmCarbonCert (quantified in kg)
// ---------------------------------------------------------

CmCarbonCert::CmCarbonCert
(const std::string entityId,
 Record&           record) :
  CommodityKilogram(entityId, record),
  d_unitPrices(record.tieTimeseries<double>("unit-prices"))
{
  s_logger->repx(logga::dbug, "constructor call", getIdAndKind());
  d_builtinRemark = "beta";
}

CmCarbonCert::~CmCarbonCert()
{
  s_logger->repx(logga::adhc, "destructor call", getIdAndKind());
}

// ---------------------------------------------------------
//  CLASS           : CmElectricity (quantified in J)
// ---------------------------------------------------------

CmElectricity::CmElectricity
(const std::string entityId,
 Record&           record) :
  CommodityJoule(entityId, record),
  d_voltage(record.tieSingle<double>("voltage"))
{
  s_logger->repx(logga::dbug, "constructor call", getIdAndKind());
  d_builtinRemark = "beta";
}

CmElectricity::~CmElectricity()
{
  s_logger->repx(logga::adhc, "destructor call", getIdAndKind());
}

double
CmElectricity::getVoltage() const
{
  return d_voltage;
}

// ---------------------------------------------------------
//  CLASS           : CmWork (quantified in J)
// ---------------------------------------------------------

CmWork::CmWork
(const std::string entityId,
 Record&           record) :
  CommodityJoule(entityId, record)
{
  s_logger->repx(logga::dbug, "constructor call", getIdAndKind());
  d_builtinRemark = "beta";
}

CmWork::~CmWork()
{
  s_logger->repx(logga::adhc, "destructor call", getIdAndKind());
}

// ---------------------------------------------------------
//  CLASS           : CmHeat (quantified in J)
// ---------------------------------------------------------

CmHeat::CmHeat
(const std::string entityId,
 Record&           record) :
  CommodityJoule(entityId, record)
{
  s_logger->repx(logga::dbug, "constructor call", getIdAndKind());
  d_builtinRemark = "beta";
}

CmHeat::~CmHeat()
{
  s_logger->repx(logga::adhc, "destructor call", getIdAndKind());
}

// ---------------------------------------------------------
//  CLASS           : CmThermalFluid (quantified in J)
// ---------------------------------------------------------

CmThermalFluid::CmThermalFluid
(const std::string entityId,
 Record&           record) :
  CommodityJoule(entityId, record),
  d_specHeatCapacity(record.tieSingle<double>("spec-heat-capacity")),
  d_floTemps(record.tieTimeseries<double>("flo-temps")),
  d_retTemps(record.tieTimeseries<double>("ret-temps"))
{
  s_logger->repx(logga::dbug, "constructor call", getIdAndKind());
  d_builtinRemark = "beta";
}

CmThermalFluid::~CmThermalFluid()
{
  s_logger->repx(logga::adhc, "destructor call", getIdAndKind());
}

double
CmThermalFluid::getFloTemp
(const int step) const
{
  return d_floTemps->at(step);
}

double
CmThermalFluid::getRetTemp
(const int step) const
{
  return d_retTemps->at(step);
}

void
CmThermalFluid::setFloTemp
(const int    step,
 const double temp)
{
  if ( temp < xeona::absoluteZero )
    {
      s_logger->repx(logga::warn, "flo temp now aphysical", temp);
    }
  d_floTemps->at(step) = temp;
}

void
CmThermalFluid::setRetTemp
(const int    step,
 const double temp)
{
  if ( temp < xeona::absoluteZero )
    {
      s_logger->repx(logga::warn, "ret temp now aphysical", temp);
    }
  d_retTemps->at(step) = temp;
}

double&
CmThermalFluid::floTemp
(const int step)
{
  return d_floTemps->at(step);
}

double&
CmThermalFluid::retTemp
(const int step)
{
  return d_retTemps->at(step);
}

// ---------------------------------------------------------
//  CLASS           : CmFunds (quantified in UOA)
// ---------------------------------------------------------

CmFunds::CmFunds
(const std::string entityId,
 Record&           record) :
  CommodityUOA(entityId, record)
{
  s_logger->repx(logga::dbug, "constructor call", getIdAndKind());
  d_builtinRemark = "beta";
}

CmFunds::~CmFunds()
{
  s_logger->repx(logga::adhc, "destructor call", getIdAndKind());
}

// ---------------------------------------------------------
//  CLASS           : CmFission (quantified in kg)
// ---------------------------------------------------------

CmFission::CmFission
(const std::string entityId,
 Record&           record) :
  CommodityKilogram(entityId, record)
{
  s_logger->repx(logga::dbug, "constructor call", getIdAndKind());
  d_builtinRemark = "beta";
}

CmFission::~CmFission()
{
  s_logger->repx(logga::adhc, "destructor call", getIdAndKind());
}

// ---------------------------------------------------------
//  CLASS           : CmProductiveLand (quantified in m2)
// ---------------------------------------------------------

CmProductiveLand::CmProductiveLand
(const std::string entityId,
 Record&           record) :
  CommodityMetreSq(entityId, record)
{
  s_logger->repx(logga::dbug, "constructor call", getIdAndKind());
  d_builtinRemark = "beta";
}

CmProductiveLand::~CmProductiveLand()
{
  s_logger->repx(logga::adhc, "destructor call", getIdAndKind());
}

//  end of file

