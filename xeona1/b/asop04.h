//  XEONA ENERGY SYSTEMS MODELING ENVIRONMENT
//
//  file-create-name : asop04.h
//  file-create-date : Thu 21-Oct-2010 22:11 UTC
//  file-initiator   : Robbie Morrison <robbie@actrix.co.nz>
//
//  file-role        : concrete asset operators 4 / header
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
//  $URL: file:///home/robbie/svn-root/xeona/futz/trunk/xeona1/b/asop04.h $
//
//  GENERAL NOTES FOR THIS FILE

//  HEADER GUARD

#ifndef _ASOP04_H_
#define _ASOP04_H_

//  LOCAL AND SYSTEM INCLUDES

#include "../c/opssched.h"    // class to track clock time and schedule plant
#include "../b/asop.h"        // asset operator entity

#include "../a/logger_fwd.h"  // some forward declarations from "logger.h"
#include "../c/smart_ptr.h"   // toggle between Boost and TR1 smart pointers

#include <string>             // C++ strings

//  CODE

// ---------------------------------------------------------
//  CLASS           : AsopOccupant
// ---------------------------------------------------------
//  Description  : asset operator who acts as a building occupant
//  Role         : concrete entity
//  Techniques   : (nothing special)
//  Status       : complete
// ---------------------------------------------------------

class AsopOccupant :
  public AssetOperator
{
  // USING DECLARATIONS

protected:

  using Entity::s_logger;                    // place in common scope for this class

  // DISABLED

private:

  AsopOccupant();                                      // zero-argument constructor
  AsopOccupant(const AsopOccupant& orig);              // copy constructor
  AsopOccupant& operator= (const AsopOccupant& orig);  // copy assignment operator

  // CREATORS

public:

  explicit
  AsopOccupant
  (const std::string entityId,
   Record&           record);

  virtual
  ~AsopOccupant();

  // DOMAIN CONTROLLER POINTS OF ENTRY

public:

  virtual
  const int                                  // number of technical assets processed
  constrain
  (const xeona::DomainMode capacityMode);

  // INSTANCE DATA

private:

  // tied quantities

  const shared_ptr<std::vector<double> >    d_temperatureSetPoints;
  const shared_ptr<std::vector<double> >    d_activityLoads;
  const shared_ptr<std::vector<double> >    d_electricityDemands;

}; // class 'AsopOccupant'

//  ==== XEDOC =================================================
//
//  entity.asop-occupant-0
//
//      class                                    > AsopOccupant
//
//        asset operator who acts as a building occupant and
//        provides set point and activity information
//
//      builtin-remark s                         <
//
//      technical-assets L                       > "teas-building-0"
//
//        technical-assets in any order
//
//      temperature-set-points [C] F             > 18.0 ..
//      activity-loads [W] F                     > 180e+00 ..
//      electricity-demands [W] F                > 5.0e+03 ..
//
//        the temperature-set-points are used to determine cooling
//        and heating loads, the activity-loads are the heating
//        contribution over and above the direct electricity-demands
//
//  ============================================================

// ---------------------------------------------------------
//  CLASS           : AsopOccupantParam
// ---------------------------------------------------------
//  Description  : asset operator who acts as a building occupant
//  Role         : concrete entity
//  Techniques   : (nothing special)
//  Status       : complete
// ---------------------------------------------------------

class AsopOccupantParam :
  public AssetOperator
{
  // USING DECLARATIONS

protected:

  using Entity::s_logger;                    // place in common scope for this class

  // DISABLED

private:

  AsopOccupantParam();                                           // zero-argument ctor
  AsopOccupantParam(const AsopOccupantParam& orig);              // copy constructor
  AsopOccupantParam& operator= (const AsopOccupantParam& orig);  // copy assignment oper

  // CREATORS

public:

  explicit
  AsopOccupantParam
  (const std::string entityId,
   Record&           record);

  virtual
  ~AsopOccupantParam();

  // DOMAIN CONTROLLER POINTS OF ENTRY

public:

  virtual
  const int                                  // number of technical assets processed
  constrain
  (const xeona::DomainMode capacityMode);

  virtual
  void
  washup();                                  // not normally overloaded for operators

  // UTILITY FUNCTIONS

private:

  double
  calcTotal
  (const double base,
   const double perPerson);

  // INSTANCE DATA

private:

  // tied quantities

  const int&                          d_occupantCount;
  const int&                          d_dayStart;
  const int&                          d_dayEnd;
  const double&                       d_tempSetPoint;
  const double&                       d_activityLoadDayPerson;
  const double&                       d_activityLoadNightPerson;
  const double&                       d_elecDemandDayBase;
  const double&                       d_elecDemandNightBase;
  const double&                       d_elecDemandDayPerson;
  const double&                       d_elecDemandNightPerson;

  shared_ptr<std::vector<double> >    d_tempSetPoints;
  shared_ptr<std::vector<double> >    d_activityLoads;
  shared_ptr<std::vector<double> >    d_electDemands;

  // local quantities

  OpsScheduler                        d_timerActivity;      // see unit 'c/opssched'

}; // class 'AsopOccupantParam'

//  ==== XEDOC =================================================
//
//  entity.asop-occupant-param-0
//
//      class                                    > AsopOccupantParam
//
//        asset operator who acts as a building occupant and
//        provides set point and activity information based on
//        day and non-day parameters
//
//      builtin-remark s                         <
//
//      technical-assets L                       > "teas-building-0"
//
//        technical-assets in any order
//
//      occupant-count [-] i                     > 5
//
//        occupant-count is the number of people
//
//      day-start [h] i                          > 8
//      day-end [h] i                            > 20
//
//        day-start and day-end on [0,24] at beginning of hour
//
//      temperature-set-point [C] f              > 21.0
//      activity-load-day-person [W] f           > 140
//      activity-load-night-person [W] f         > 100
//      electricity-demand-day-base [W] f        > 3.0e+03
//      electricity-demand-night-base [W] f      > 0.0
//      electricity-demand-day-person [W] f      > 1.0e+03
//      electricity-demand-night-person [W] f    > 0.0
//
//        the temperature-set-point is used to determine cooling
//        and heating loads, the activity-loads are the heating
//        contribution over and above the direct
//        electricity-demands and are per person
//
//      temperature-set-points [C] F             < 0.0 ..
//      activity-loads [W] F                     < 0.0 ..
//      electricity-demands [W] F                < 0.0 ..
//
//  ============================================================

#endif // _ASOP04_H_

//  end of file

