//  XEONA ENERGY SYSTEMS MODELING ENVIRONMENT
//
//  file-create-name : teas05.h
//  file-create-date : Thu 21-Oct-2010 22:17 UTC
//  file-initiator   : Robbie Morrison <robbie@actrix.co.nz>
//
//  file-role        : concrete technical assets 5 / header
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
//  $URL: file:///home/robbie/svn-root/xeona/futz/trunk/xeona1/b/teas05.h $
//
//  GENERAL NOTES FOR THIS FILE

//  HEADER GUARD

#ifndef _TEAS05_H_
#define _TEAS05_H_

//  LOCAL AND SYSTEM INCLUDES

#include "../c/opssched.h"    // class to track clock time and schedule plant
#include "../b/teas.h"        // technical asset entity
#include "../b/costreg.h"     // cost registers
#include "../a/exent.h"       // entity exception classes

#include "../a/logger_fwd.h"  // some forward declarations from "logger.h"
#include "../c/smart_ptr.h"   // toggle between Boost and TR1 smart pointers

#include <map>                // STL associative container
#include <string>             // C++ strings
#include <vector>             // STL sequence container

//  FORWARD (PARTIAL) DECLARATIONS

class Record;                                // constructor argument
namespace svif { class SolverIf; }           // member function argument

template <typename C> class Cable;           // 'C' is of base class 'Commodity' etc

class CmElectricity;                         // 'CmElectricity' is from 'Commodity' etc

class CxAmbientAir;
class CxAmbientSolar;

class BuildingFabricData;
class BuildingSim;

//  CODE

// ---------------------------------------------------------
//  CLASS           : TeasBuildingElec
// ---------------------------------------------------------
//  Description  : building which reacts to given temperature set point and its context
//  Role         : concrete entity
//  Techniques   : 'OpsFac1Out0' sink-style OSP, class 'BuildingSim' thermal performance
//  Status       : complete
//
//  Design notes
//
//      Setting the chiller and heater capacities
//
//          Please note that hysteresis is (currently) set to 5%
//          of the given chiller/heater capacity.  This means
//          that excessive values of 'chiller-thermal-capacity'
//          and 'heater-thermal-capacity' will result in
//          excessively high hysteresis trips and therefore be
//          completely counterproductive.  Perhaps the hysteresis
//          could be made dynamic on duty.
//
//          This leads to the broader question of designing
//          better HVAC operations protocols.  The present
//          implementation should be regarded as a first cut.
//
// ---------------------------------------------------------

class OpsFac1Out0_A;                         // necessary for OSP typedef below

class TeasBuildingElec :
  public TechnicalAsset,
  public CostRegisterSRFin,
  public CostRegisterEmbFin
{
  // USING DECLARATIONS

protected:

  using Entity::s_logger;                    // place in common scope for this class

  // TYPEDEFS

private:

  typedef OpsFac1Out0_A OpsFac1Out0;         // used for switching implementations

  // DISABLED

private:

  TeasBuildingElec();                                         // zero-argument constructor
  TeasBuildingElec(const TeasBuildingElec& orig);             // copy constructor
  TeasBuildingElec& operator= (const TeasBuildingElec& orig); // copy assignment operator

  // CREATORS

public:

  explicit
  TeasBuildingElec
  (const std::string entityId,
   Record&           record);

  virtual
  ~TeasBuildingElec();

  // CALLS

public:

  virtual
  void
  establish()                                // with low priority development reporting
    throw(xeona::entity_issue_2);            // exception specification

  virtual
  void
  initialize();

  virtual
  const int                                  // duty gol
  constrain
  (const xeona::DomainMode capacityMode);

  virtual
  void
  washup();

  virtual
  void
  conclude();

  virtual
  void
  passBuildingOccData
  (const double temperatureSetPoint,         // temperature set point [C] (Celsius)
   const double activityLoad,                // load contribution but not electricity [W]
   const double electricityDemand);          // electricity demand [W]

  // UTILITY FUNCTIONS

protected:

  virtual
  double
  calcBuildingDemand                         // basic building simulation
  (const int step,                           // horizon step
   const int count);

  // INSTANCE DATA

private:

  // tied quantities - in-data

  assign_ptr<CxAmbientAir>             d_ambientAirContext;
  assign_ptr<CxAmbientSolar>           d_ambientSolarContext;

  const int&                           d_count;
  const double&                        d_demandHiBound;
  const double&                        d_orientation;
  const double&                        d_floorArea;
  const double&                        d_floorAspectRatio;
  const double&                        d_wallHeight;
  const double&                        d_windowToWallRatio;
  const std::string&                   d_constructionType;
  const double&                        d_chillerCapacity;
  const double&                        d_heaterCapacity;
  const int&                           d_hvacOpsStart;
  const int&                           d_hvacOpsEnd;
  const double&                        d_hvacBacklogTrip;

  // tied quantities - out-data

  double&                              d_totalCapacity;     // simple arithmetic
  shared_ptr<std::vector<double> >     d_demands;           // meterbox electricity
  shared_ptr<std::vector<double> >     d_hvacDutys;         // heat loads, heating -ve

  double&                              d_floorPerformance;  // thermal performance metric
  double&                              d_chillerMaxDuty;
  double&                              d_heaterMaxDuty;

  shared_ptr<Cable<CmElectricity> >    d_inElec;            // cable

  // quantities set by 'AsopOccupant'

  double                               d_temperatureSetPoint;
  double                               d_activityLoad;
  double                               d_electricityDemand;

  // local quantities

  shared_ptr<BuildingFabricData>                            d_fabricData;
  std::map<std::string, shared_ptr<BuildingFabricData> >    d_fabricDatas;

  OpsScheduler                         d_timerHvac;    // see unit 'c/opssched'
  shared_ptr<BuildingSim>              d_buildingSim;
  Statistics<double>                   d_hvacElecDemandStats;
  Statistics<double>                   d_totalElecDemandStats;

  shared_ptr<OpsFac1Out0>              d_ops;          // specialization required

};

//  ==== XEDOC =================================================
//
//  entity.teas-building-elec-0
//
//      class                                    > TeasBuildingElec
//
//        a simple rectangular electrically cooled and heated
//        building which is coupled to an occupant operator for
//        indoor temperature set point and activity data
//
//        pair with either AsopOccupant or AsopOccupantParam
//
//      builtin-remark s                         <
//
//      socket-1 l                               > "teas-supplier-1.elec-1"
//
//        socket-1 is my supplier
//
//      cable-electricity-commodity l            > "cm-electricity-0"
//
//        cable-electricity-commodity defines the underlying
//        commodity
//
//      ambient-air-context l                    > "cx-ambient-air-0"
//      ambient-solar-context l                  > "cx-ambient-solar-0"
//
//        these context entities must be a sub-class of
//        CxAmbientAir and CxAmbientSolar respectively
//
//      count [-] i                              > 5
//      demand-hi-bound [W] f                    > 120.0e+03
//
//        count is the number of identical buildings, the
//        demand-hi-bound applies to one building
//
//      orientation [degrees] f                  > 30.0
//      floor-area [m2] f                        > 200
//      floor-aspect-ratio [-] f                 > 2.0
//      wall-height [m] f                        > 3.0
//      window-to-wall-ratio [-] f               > 0.40
//
//        orientation [-90,90] refers to the short-axis of the
//        building, the floor-area is the footprint, the
//        floor-aspect-ratio, wall-height, and
//        window-to-wall-ratio are self-evident
//
//      construction-type s                      > "average"
//
//        the construction-type is used to generate
//        representative thermal characteristics in
//        average | good
//
//      chiller-thermal-capacity [W] f           > 100.0e+03
//      heater-thermal-capacity [W] f            > 100.0e+03
//
//        chiller-thermal-capacity and heater-thermal-capacity
//        are self-evident, the chiller itself uses a Plank
//        refrigeration model with refrigerant R-134a, heating
//        is by electrical resistance
//
//      hvac-ops-start [h] i                     > 8
//      hvac-ops-end [h] i                       > 18
//      hvac-backlog-trip [-] f                  > 2.0
//
//        hvac-ops-start and hvac-ops-end [0,24] specify the HVAC
//        run schedule where always off is 0,0 and continuous on
//        is 0,24, hvac-backlog-trip [0,inf] sets the level
//        beyond which unmet HVAC carryover provokes warnings
//
//      total-capacity [W] f                     < 0.0
//      demands [W] F                            < 0.0 ..
//      hvac-dutys [W] F                         < 0.0 ..
//      floor-performance [W/m2] f               < 0.0
//      chiller-max-duty [W] f                   < 0.0
//      heater-max-duty [W] f                    < 0.0
//
//        demand-hi-bound is the maximum electricity demand for
//        each count house -- demands are the actual metered
//        demands combined for all houses, the hvac-duties are
//        negative for heating, floor-performance is the annual
//        average HVAC usage
//
//      nameplate-capacity [W] f                 > 200
//      duty-specific-cost-financial [$/J] f     > 2.0
//      size-specific-cost-financial [$/W/s] f   > 3.0
//      standing-cost-financial [$/s] f          > 5.0
//
//        short-run building costs are on a per-building basis
//        and should not be pre-multiplied by count
//
//      variable-costs-financial [$] F           < 0.0 ..
//      fixed-costs-financial [$] F              < 0.0 ..
//
//        the following capex information represents total costs
//        and must be pre-multiplied by count
//
//      annual-discount-rate-decimal [-] f       > 0.10
//      economic-life [y] i                      > 25
//      capex-initial [$] f                      > 120e+03
//      capex-terminal [$] f                     > -10e+03
//      current-age [y] i                        > 2
//
//        a negative capex-terminal indicates salvage income as
//        opposed to decommissioning cost
//
//      embedded-costs-financial [$] F           < 0.0 ..
//
//        the embedded-costs-financial are calculated using the
//        DCF annuity method over the economic life of the entity
//        in question
//
//  ============================================================

#endif // _TEAS05_H_

//  end of file

