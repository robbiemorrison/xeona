//  XEONA ENERGY SYSTEMS MODELING ENVIRONMENT
//
//  file-create-name : cxamb02.h
//  file-create-date : Thu 07-May-2009 13:52 UTC
//  file-initiator   : Robbie Morrison <robbie@actrix.co.nz>
//
//  file-role        : concrete ambient conditions contexts 2 / header
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
//  $URL: file:///home/robbie/svn-root/xeona/futz/trunk/xeona1/e/cxamb02.h $
//
//  GENERAL NOTES FOR THIS FILE
//
//  The contexts here cover ambient solar.

//  HEADER GUARD

#ifndef _CXAMB02_H_
#define _CXAMB02_H_

//  LOCAL AND SYSTEM INCLUDES

#include "../e/context.h"     // abstract context entity
#include "../c/wxinfo.h"      // weather data class for testing
#include "../c/tsset.h"       // timeseries classes for added functionality

#include "../a/logger_fwd.h"  // some forward declarations from "logger.h"
#include "../c/smart_ptr.h"   // toggle between Boost and TR1 smart pointers

#include <string>             // C++ strings
#include <sstream>            // string-streams
#include <vector>             // STL sequence container

#include <boost/tuple/tuple.hpp>             // n-tuples, ref(), cref()

//  FORWARD (PARTIAL) DECLARATIONS

class Record;                                // constructor argument

//  CODE

// ---------------------------------------------------------
//  CLASS           : CxAmbientSolar
// ---------------------------------------------------------
//  Description  : interface base for ambient solar contexts
//  Role         : step in the inheritance web
//  Techniques   : (nothing special)
//  Status       : complete
// ---------------------------------------------------------

class CxAmbientSolar :
  public AmbientConditions
{
  // DISABLED

private:

  CxAmbientSolar();                                         // zero-argument constructor
  CxAmbientSolar(const CxAmbientSolar& orig);               // copy constructor
  CxAmbientSolar& operator= (const CxAmbientSolar& orig);   // copy assignment operator

  // CREATORS

public:

  explicit
  CxAmbientSolar
  (const std::string entityId) :             // special constructor to facilitate linking
    AmbientConditions(entityId)
  { }

  explicit
  CxAmbientSolar
  (const std::string entityId,
   Record&           record);

  virtual
  ~CxAmbientSolar();

  // LINKING CALL

private:                                     // CAUTION: 'private' is correct

  virtual
  bool
  polylink(assign_ptr<Entity>& pass)         // CAUTION: 'Entity' is correct
  {
    s_logger->repx(logga::adhc, "entering virtual member function", "CxAmbientSolar");
    const bool okay = pass.revamp(retFull<CxAmbientSolar>());      // key call

    // low priority development reporting
    shared_ptr<CxAmbientSolar> full = retFull<CxAmbientSolar>();
    s_logger->repx(logga::adhc, "development reporting only", "");
    std::ostringstream put;
    put << "  full identifier : " << full->getIdentifier() << "\n"
        << "  full resource   : " << full.get()            << "\n";
    s_logger->putx(logga::adhc, put);

    return okay;                             // typically return directly from 'revamp'
  }

  // ACCESSORS

public:

  virtual
  boost::tuple
  <double,                                   // see proper definition
   double>
  getSolar
  (const int step) const { return boost::make_tuple(s_doubleZero, s_doubleZero); }

  virtual
  boost::tuple
  <double,                                   // see proper definition
   double>
  getSolarPeek
  (const int step,
   const int ahead) const { return boost::make_tuple(s_doubleZero, s_doubleZero); }

  virtual
  shared_ptr<std::vector<boost::tuple<double, double> > >
  getSolarDay
  (const int pivot,
   const int interval) const
  { return shared_ptr<std::vector<boost::tuple <double, double> > >(); }

  virtual
  boost::tuple
  <double,                                   // see proper definition
   double>
  getSunAngles
  (const int step) const  { return boost::make_tuple(s_doubleZero, s_doubleZero); }

  virtual
  double                                     // see proper definition
  getSiteElevation() const { return s_doubleZero; }

  virtual
  double                                     // see proper definition
  getLatitude() const { return s_doubleZero; }

  virtual
  double                                     // see proper definition
  getLongitude() const { return s_doubleZero; }

};

// ---------------------------------------------------------
//  CLASS           : CxAmbientSolarTs
// ---------------------------------------------------------
//  Description  : ambient solar context based on exogenous timeseries
//  Role         : ambient conditions support
//  Techniques   : std::vector, 'xeona::sunpos'
//  Status       : complete
// ---------------------------------------------------------

class CxAmbientSolarTs :
  public CxAmbientSolar
{
  // DISABLED

private:

  CxAmbientSolarTs();                                         // zero-argument constructor
  CxAmbientSolarTs(const CxAmbientSolarTs& orig);             // copy constructor
  CxAmbientSolarTs& operator= (const CxAmbientSolarTs& orig); // copy assignment operator

  // CREATORS

public:

  explicit
  CxAmbientSolarTs
  (const std::string entityId,
   Record&           record);

  virtual
  ~CxAmbientSolarTs();

  // CALLS

  virtual
  void
  setup();

  // ACCESSORS

public:

  // CAUTION: the following member functions do not make downward
  // adjustments based on the atmospheric clearness

  virtual
  boost::tuple
  <double,                                   // direct irradiation [W/m^2]
   double>                                   // diffuse irradiation [W/m^2]
  getSolar
  (const int step) const;

  virtual
  boost::tuple
  <double,
   double>
  getSolarPeek
  (const int step,
   const int ahead = 1) const;

  virtual
  shared_ptr<std::vector<boost::tuple
                         <double,            // direct irradation [W/m^2]
                          double> > >        // diffuse irradiation [W/m^2]
  getSolarDay                                // day of data, centered on 'pivot'
  (const int pivot,                          // zero-based, often the current step
   const int interval) const;                // interval length [s]

  virtual
  boost::tuple
  <double,                                   // zenith angle (phi) [degrees]
   double>                                   // azimuth angle (gamma) [degrees]
  getSunAngles
  (const int step) const;                    // also relies on 'TimeHorizon' data

  virtual
  double                                     // site elevation (altitude) [m]
  getSiteElevation() const;                  // typically used in airmass calculations

  virtual
  double
  getLatitude() const;

  virtual
  double
  getLongitude() const;

  // INSTANCE DATA

private:

  // tied quantities

  const double&                             d_latitude;
  const double&                             d_longitude;
  const double&                             d_elevation;
  const int&                                d_year;

  const bool&                               d_hasFeb29;
  const bool&                               d_isLoopable;

  const shared_ptr<std::vector<double> >    d_solarDirect;
  const shared_ptr<std::vector<double> >    d_solarDifuse;
  const shared_ptr<std::vector<double> >    d_clearnessIndex;

  // local quantities

  TsNormal                                  d_solarDirectTs;
  TsNormal                                  d_solarDifuseTs;
  TsNormal                                  d_clearnessIndexTs;

};

//  ==== XEDOC =================================================
//
//  entity.cx-ambient-solar-ts-0
//
//      class                                    > CxAmbientSolarTs
//
//        ambient solar context with exogenous timeseries and
//        which can also calculate zenith and azimuth angles --
//        thereby allowing assets to calculate the total solar
//        irradiation on surfaces of known orientation
//
//      builtin-remark s                         <
//
//      latitude [deg] f                         > 52.5
//      longitude [deg] f                        > 13.4
//      elevation [m] f                          > 50.0
//      year [-] i                               > 2010
//
//        latitude [-90,+90] and longitude [-180,+180] define
//        location and the timeseries start year is required for
//        sun angle calculations
//
//      has-feb-29 [-] b                         > 0
//      is-loopable [-] b                        > 0
//
//        has-feb-29 and is-loopable provide metadata
//
//      solar-direct [W/m^2] F                   > 500.0 ..
//      solar-diffuse [W/m^2] F                  > 500.0 ..
//      clearness-index [-] F                    > 1.0 ..
//
//        solar-direct and solar-diffuse are the solar
//        irradiation components, time-based to UTC
//
//        if the clearness-index [0,1] is set to unity, the
//        direct and diffuse values should include the effects of
//        cloudiness
//
//  ============================================================

// ---------------------------------------------------------
//  CLASS           : CxAmbientSolarTMY
// ---------------------------------------------------------
//  Description  : ambient solar context based on TMY data
//  Role         : ambient conditions support
//  Techniques   : std::vector, 'c/wxinfo02.cc'
//  Status       : complete
// ---------------------------------------------------------

class CxAmbientSolarTMY :
  public CxAmbientSolar
{
  // DISABLED

private:

  CxAmbientSolarTMY();                                           // zero-argument ctor
  CxAmbientSolarTMY(const CxAmbientSolarTMY& orig);              // copy constructor
  CxAmbientSolarTMY& operator= (const CxAmbientSolarTMY& orig);  // copy assignment oper

  // CREATORS

public:

  explicit
  CxAmbientSolarTMY
  (const std::string entityId,
   Record&           record);

  virtual
  ~CxAmbientSolarTMY();

  // CALLS

  virtual
  void
  setup();

  // ACCESSORS

public:

  // CAUTION: the following member functions do not make downward
  // adjustments based on the atmospheric clearness -- but
  // neither do they need to, they are based on measurement

  virtual
  boost::tuple
  <double,                                   // direct irradiation [W/m^2]
   double>                                   // diffuse irradiation [W/m^2]
  getSolar
  (const int step) const;

  virtual
  boost::tuple
  <double,
   double>
  getSolarPeek
  (const int step,
   const int ahead = 1) const;

  virtual
  shared_ptr<std::vector<boost::tuple
                         <double,            // direct irradation [W/m^2]
                          double> > >        // diffuse irradiation [W/m^2]
  getSolarDay                                // day of data, centered on 'pivot'
  (const int pivot,                          // zero-based, often the current step
   const int interval) const;                // interval length [s]

  virtual
  boost::tuple
  <double,                                   // zenith angle (phi) [degrees]
   double>                                   // azimuth angle (gamma) [degrees]
  getSunAngles
  (const int step) const;                    // also relies on 'TimeHorizon' data

  virtual
  double                                     // site elevation (altitude) [m]
  getSiteElevation() const;                  // typically used in airmass calculations

  virtual
  double
  getLatitude() const;

  virtual
  double
  getLongitude() const;

  // INSTANCE DATA

private:

  // tied quantities

  const int&                          d_hoursOffset;
  const int&                          d_daysOffset;

  double&                             d_latitude;
  double&                             d_longitude;
  double&                             d_elevation;

  shared_ptr<std::vector<double> >    d_solarDirect;
  shared_ptr<std::vector<double> >    d_solarDifuse;
  shared_ptr<std::vector<double> >    d_sunZenith;
  shared_ptr<std::vector<double> >    d_sunAzimuth;

  // local quantities

  const WeatherData                   d_wx;  // see unit 'c/wxinfo'
  TsNormal                            d_solarDirectTs;
  TsNormal                            d_solarDifuseTs;
  TsNormal                            d_sunZenithTs;
  TsNormal                            d_sunAzimuthTs;

};

//  ==== XEDOC =================================================
//
//  entity.cx-ambient-solar-tmy-0
//
//      class                                    > CxAmbientSolarTMY
//
//        ambient solar context which serves the NIWA TMY
//        (typical meteorological year) dataset for Otago, New
//        Zealand
//
//        resampling is supported
//
//      builtin-remark s                         <
//
//      latitude [deg] f                         < 0
//      longitude [deg] f                        < 0
//      elevation [m] f                          < 0
//
//      solar-direct [W/m^2] F                   < 0.0 ..
//      solar-diffuse [W/m^2] F                  < 0.0 ..
//      sun-zenith [degrees] F                   < 0.0 ..
//      sun-azimuth [degrees] F                  < 0.0 ..
//
//        solar-direct and solar-diffuse are the solar
//        irradiation components, time-based to UTC
//
//      hours-offset [-] i                       > 0
//      days-offset [-] i                        > 0
//
//        hours-offset and days-offset are used to vary the
//        starting point
//
//  ============================================================

#endif // _CXAMB02_H_

//  end of file

