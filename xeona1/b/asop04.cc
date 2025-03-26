//  XEONA ENERGY SYSTEMS MODELING ENVIRONMENT
//
//  file-create-name : asop04.cc
//  file-create-date : Thu 21-Oct-2010 22:12 UTC
//  file-initiator   : Robbie Morrison <robbie@actrix.co.nz>
//
//  file-role        : concrete asset operators 4 / implementation
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
//  $URL: file:///home/robbie/svn-root/xeona/futz/trunk/xeona1/b/asop04.cc $
//
//  GENERAL NOTES FOR THIS FILE

//  LOCAL AND SYSTEM INCLUDES

#include "asop04.h"           // companion header for this file (place first)

#include "../c/recset.h"      // records and fields and also record-sets
#include "../b/teas.h"        // technical asset entity

#include "../a/logger.h"      // standard logging functionality (as required)
#include "../c/smart_ptr.h"   // toggle between Boost and TR1 smart pointers
#include ".././common.h"      // common definitions for project (place last)

#include <string>             // C++ strings
#include <sstream>            // string-streams

#include <boost/foreach.hpp>  // BOOST_FOREACH iteration macro

//  CODE

// ---------------------------------------------------------
//  CLASS           : AsopOccupant
// ---------------------------------------------------------

// ---------------------------------------------------------
//  MEMBER FUNCTION : AsopOccupant
// ---------------------------------------------------------

AsopOccupant::AsopOccupant
(const std::string entityId,
 Record&           record) :
  CostRegister(record),
  AssetOperator(entityId, record, xeona::e_commitmentModes),
  d_temperatureSetPoints(record.tieTimeseries<double>("temperature-set-points")),
  d_activityLoads(record.tieTimeseries<double>("activity-loads")),
  d_electricityDemands(record.tieTimeseries<double>("electricity-demands"))
{
  s_logger->repx(logga::xtra, "constructor call", getIdAndKind());
  d_builtinRemark = "beta";
}

// ---------------------------------------------------------
//  MEMBER FUNCTION : ~AsopOccupant
// ---------------------------------------------------------

AsopOccupant::~AsopOccupant()
{
  s_logger->repx(logga::adhc, "destructor call", getIdAndKind());
}

// DOMAIN CONTROLLER POINTS OF ENTRY

// ---------------------------------------------------------
//  MEMBER FUNCTION : constrain
// ---------------------------------------------------------
//  Description  : constrain call
//  Role         : standard call
//  Techniques   : 'passBuildingOccData' but no OSP
//  Status       : complete
// ---------------------------------------------------------

const int                                    // number of technical assets processed
AsopOccupant::constrain
(const xeona::DomainMode capacityMode)
{
  // initial reporting
  s_logger->repx(logga::adhc, "entering member function", "");

  // preamble
  const std::string asopId = getIdentifier();

  // building occupancy values
  const double temperatureSetPoint = d_temperatureSetPoints->at(d_step);
  const double activityLoad        = d_activityLoads->at(d_step);
  const double electricityDemand   = d_electricityDemands->at(d_step);

  // declare some administration counters
  int teasLoops = 0;                         // number of technical assets processed

  // step thru technical assets in no required order
  BOOST_FOREACH( shared_ptr<TechnicalAsset> ta, d_technicalAssets )
    {
      // keep count
      ++teasLoops;
      xeona::putxId(ta, "AsopOccupant::constrain technical asset loop");

      // communicate building occupancy data
      ta->passBuildingOccData(temperatureSetPoint,
                              activityLoad,
                              electricityDemand);

      // constrain the associated technical asset
      const int opsDutyGol = ta->constrain(capacityMode);

      // bid coupling is required
      if ( opsDutyGol == 0 )
        {
          const std::string teasId = ta->getIdentifier();
          s_logger->repx(logga::warn, "hollow coupling encountered, gol", opsDutyGol);
          s_logger->repx(logga::xtra, "abandoning current asset processing", teasId);
          continue;
        }

      // store some information
      // nothing to store

    } // technical assets loop

  // return combined count
  return teasLoops;

} // function 'constrain'

// ---------------------------------------------------------
//  CLASS           : AsopOccupantParam
// ---------------------------------------------------------

// ---------------------------------------------------------
//  MEMBER FUNCTION : AsopOccupantParam
// ---------------------------------------------------------

AsopOccupantParam::AsopOccupantParam
(const std::string entityId,
 Record&           record) :
  CostRegister(record),
  AssetOperator(entityId, record, xeona::e_commitmentModes),
  d_occupantCount(record.tieSingle<int>("occupant-count")),
  d_dayStart(record.tieSingle<int>("day-start")),
  d_dayEnd(record.tieSingle<int>("day-end")),
  d_tempSetPoint(record.tieSingle<double>("temperature-set-point")),
  d_activityLoadDayPerson(record.tieSingle<double>("activity-load-day-person")),
  d_activityLoadNightPerson(record.tieSingle<double>("activity-load-night-person")),
  d_elecDemandDayBase(record.tieSingle<double>("electricity-demand-day-base")),
  d_elecDemandNightBase(record.tieSingle<double>("electricity-demand-night-base")),
  d_elecDemandDayPerson(record.tieSingle<double>("electricity-demand-day-person")),
  d_elecDemandNightPerson(record.tieSingle<double>("electricity-demand-night-person")),
  d_tempSetPoints(record.tieTimeseries<double>("temperature-set-points")),
  d_activityLoads(record.tieTimeseries<double>("activity-loads")),
  d_electDemands(record.tieTimeseries<double>("electricity-demands")),
  d_timerActivity(Entity::getHorizonInterval(), Entity::getHorizonStartHour())
{
  s_logger->repx(logga::xtra, "constructor call", getIdAndKind());
  d_builtinRemark = "beta";

  // integrity checks -- timing
  if ( d_dayStart < 0 || d_dayStart > 24 )
    {
      s_logger->repx(logga::warn, "day-start is not [0,24]", d_dayStart);
    }
  if ( d_dayEnd < 0 || d_dayEnd > 24 )
    {
      s_logger->repx(logga::warn, "day-end is not [0,24]", d_dayEnd);
    }
  if ( d_dayStart > d_dayEnd )
    {
      std::ostringstream oss;
      oss << d_dayStart << " : " << d_dayEnd;
      s_logger->repx(logga::warn, "day-start exceeds day-end", oss.str());
    }
  // integrity checks -- temperature
  if ( d_tempSetPoint < 5 || d_tempSetPoint > 30 )
    {
      s_logger->repx(logga::warn, "temperature set point not [5,30]", d_tempSetPoint);
    }
  // integrity checks -- activity loads
  const double activityLoadPersonUp = 500;   // upper limit, 500 W is beyond athletic
  if ( d_activityLoadDayPerson < 0 || d_activityLoadDayPerson > activityLoadPersonUp )
    {
      s_logger->repx(logga::warn,
                     "odd activity-load-day-per-person",
                     d_activityLoadDayPerson);
    }
  if ( d_activityLoadNightPerson < 0 || d_activityLoadNightPerson > activityLoadPersonUp )
    {
      s_logger->repx(logga::warn,
                     "odd activity-load-night-per-person",
                     d_activityLoadNightPerson);
    }
  // integrity checks -- electricity demands
  if ( d_elecDemandDayBase < 0 )
    {
      s_logger->repx(logga::warn,
                     "electricity-demand-day-base -ve",
                     d_elecDemandDayBase);
    }
  if ( d_elecDemandNightBase < 0 )
    {
      s_logger->repx(logga::warn,
                     "electricity-demand-night-base -ve",
                     d_elecDemandNightBase);
    }
  if ( d_elecDemandDayPerson < 0 )
    {
      s_logger->repx(logga::warn,
                     "electricity-demand-day-person -ve",
                     d_elecDemandDayPerson);
    }
  if ( d_elecDemandNightPerson < 0 )
    {
      s_logger->repx(logga::warn,
                     "electricity-demand-night-person -ve",
                     d_elecDemandNightPerson);
    }

  // set the operational schedule,see unit 'c/opssched' for details
  d_timerActivity.setSchedule(d_dayStart, d_dayEnd);
}

// ---------------------------------------------------------
//  MEMBER FUNCTION : ~AsopOccupantParam
// ---------------------------------------------------------

AsopOccupantParam::~AsopOccupantParam()
{
  s_logger->repx(logga::adhc, "destructor call", getIdAndKind());
}

// DOMAIN CONTROLLER POINTS OF ENTRY

// ---------------------------------------------------------
//  MEMBER FUNCTION : constrain
// ---------------------------------------------------------
//  Description  : constrain call
//  Role         : standard call
//  Techniques   : 'passBuildingOccData' but no OSP
//  Status       : complete
// ---------------------------------------------------------

const int                                    // number of technical assets processed
AsopOccupantParam::constrain
(const xeona::DomainMode capacityMode)
{
  // initial reporting
  s_logger->repx(logga::adhc, "entering member function", "");

  // preamble
  const std::string asopId = getIdentifier();

  // update timer as required and recover schedule
  d_timerActivity.bump(d_step);              // CAUTION: must precede recovery
  const bool isDayRoutine = d_timerActivity.isWorkingHour();     // 'true' means daytime

  // building occupancy variables
  double temperatureSetPoint = 0.0;
  double activityLoad        = 0.0;
  double electricityDemand   = 0.0;

  // calculate based on day routine or not
  if ( isDayRoutine )
    {
      const double activityLoadDayBase = 0.0;
      temperatureSetPoint = d_tempSetPoint;
      activityLoad        = calcTotal(activityLoadDayBase, d_activityLoadDayPerson);
      electricityDemand   = calcTotal(d_elecDemandDayBase, d_elecDemandDayPerson);
    }
  else
    {
      const double activityLoadNightBase = 0.0;
      temperatureSetPoint = d_tempSetPoint;
      activityLoad        = calcTotal(activityLoadNightBase, d_activityLoadNightPerson);
      electricityDemand   = calcTotal(d_elecDemandNightBase, d_elecDemandNightPerson);
    }

  // declare some administration counters
  int teasLoops = 0;                         // number of technical assets processed

  // step thru technical assets in no required order
  BOOST_FOREACH( shared_ptr<TechnicalAsset> ta, d_technicalAssets )
    {
      // keep count
      ++teasLoops;
      xeona::putxId(ta, "AsopOccupantParam::constrain technical asset loop");

      // communicate building occupancy data
      ta->passBuildingOccData(temperatureSetPoint,
                              activityLoad,
                              electricityDemand);

      // constrain the associated technical asset
      const int opsDutyGol = ta->constrain(capacityMode);

      // bid coupling is required
      if ( opsDutyGol == 0 )
        {
          const std::string teasId = ta->getIdentifier();
          s_logger->repx(logga::warn, "hollow coupling encountered, gol", opsDutyGol);
          s_logger->repx(logga::xtra, "abandoning current asset processing", teasId);
          continue;
        }

      // store some information
      // nothing to store

    } // technical assets loop

  // store my information
  d_tempSetPoints->at(d_step) = temperatureSetPoint;
  d_activityLoads->at(d_step) = activityLoad;
  d_electDemands->at(d_step)  = electricityDemand;

  // return combined count
  return teasLoops;

} // function 'constrain'

// ---------------------------------------------------------
//  MEMBER FUNCTION : AsopOccupantParam::washup
// ---------------------------------------------------------
//  Description  : development stub
//  Role         : not yet specified
//  Techniques   : polymorphic function call
//  Status       : complete
//
//  Design notes
//
//      Not often overloaded for operators.
//
// ---------------------------------------------------------

void
AsopOccupantParam::washup()
{
  // call base version first (see the documentation for 'AssetOperator')
  AssetOperator::washup();

  // nothing more to do at the present point in development
}

// UTILITY FUNCTIONS

// ---------------------------------------------------------
//  MEMBER FUNCTION : AsopOccupantParam::calcTotal
// ---------------------------------------------------------
//  Description  : calculates total for base and per-person values
//  Role         : support for calculations
//  Techniques   : (nothing special)
//  Status       : complete
// ---------------------------------------------------------

double
AsopOccupantParam::calcTotal
(const double base,
 const double perPerson)
{
  return base + d_occupantCount * perPerson;
}

//  end of file

