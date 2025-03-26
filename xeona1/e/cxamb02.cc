//  XEONA ENERGY SYSTEMS MODELING ENVIRONMENT
//
//  file-create-name : cxamb02.cc
//  file-create-date : Thu 07-May-2009 13:52 UTC
//  file-initiator   : Robbie Morrison <robbie@actrix.co.nz>
//
//  file-role        : concrete ambient conditions contexts 2 / implementation
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
//  $URL: file:///home/robbie/svn-root/xeona/futz/trunk/xeona1/e/cxamb02.cc $
//
//  GENERAL NOTES FOR THIS FILE

//  LOCAL AND SYSTEM INCLUDES

#include "cxamb02.h"          // companion header for this file (place first)

#include "../c/yearcalc.h"    // free functions for calendar calculations
#include "../c/util4.h"       // free functions for trigonometry and maths
#include "../c/tsops.h"       // overloaded operators and similar for timeseries
#include "../c/suncalc.h"     // free functions for solar calculations
#include "../c/recset.h"      // records and fields and also record-sets

#include "../a/logger.h"      // standard logging functionality (as required)
#include "../c/smart_ptr.h"   // toggle between Boost and TR1 smart pointers
#include ".././common.h"      // common definitions for project (place last)

#include <limits>             // numeric_limits<T>::infinity() and similar
#include <string>             // C++ strings
#include <sstream>            // string-streams

//  CODE

// ---------------------------------------------------------
//  CLASS           : CxAmbientSolar
// ---------------------------------------------------------

// ---------------------------------------------------------
//  MEMBER FUNCTION : CxAmbientSolar
// ---------------------------------------------------------

CxAmbientSolar::CxAmbientSolar
(const std::string entityId,
 Record&           record) :
  AmbientConditions(entityId, record)
{
  s_logger->repx(logga::xtra, "constructor call", getIdAndKind());
  d_builtinRemark = "beta";
}

// ---------------------------------------------------------
//  MEMBER FUNCTION : ~CxAmbientSolar
// ---------------------------------------------------------

CxAmbientSolar::~CxAmbientSolar()
{
  s_logger->repx(logga::xtra, "destructor call", getIdAndKind());
}

// ---------------------------------------------------------
//  CLASS           : CxAmbientSolarTs
// ---------------------------------------------------------
//  Description  : ambient solar context which also calculates sun angles
//  Role         : support for solar gain, solar PV, and solar thermal calculations
//  Techniques   : (nothing special)
//  Status       : complete
//
//  Design notes
//
//      This class supplies solar irradiation information.
//
//      It uses timeseries for the irradiance.  And calculates
//      the position of the sun based on location (latitude and
//      longitude) and time (calender date and universal time)
//      data.  Member function 'CxAmbientSolar::getSunAngles'
//      recovers this information, after calling 'xeona::sunpos'.
//
//      The information on offer can be used to support building
//      heating and cooling calculations, solar PV performance
//      calculations, and solar thermal systems calculations.
//
//  Notation
//
//      The notation used here is consistent with that from NOAA
//      (National Oceanic and Atmospheric Administration), USA:
//
//        http://www.srrb.noaa.gov/highlights/sunrise/glossary.html
//
//  CAUTION: clearness not implemented
//
//      Member functions 'CxAmbientSolar::getSolar' and
//      'CxAmbientSolar::getSolarPeek' do NOT make use of
//      clearness data.
//
// ---------------------------------------------------------

CxAmbientSolarTs::CxAmbientSolarTs
(const std::string entityId,
 Record&           record) :
  CxAmbientSolar(entityId, record),
  d_latitude(record.tieSingle<double>("latitude")),
  d_longitude(record.tieSingle<double>("longitude")),
  d_elevation(record.tieSingle<double>("elevation")),
  d_year(record.tieSingle<int>("year")),
  d_hasFeb29(record.tieSingle<bool>("has-feb-29")),
  d_isLoopable(record.tieSingle<bool>("is-loopable")),
  d_solarDirect(record.tieTimeseries<double>("solar-direct")),
  d_solarDifuse(record.tieTimeseries<double>("solar-diffuse")),
  d_clearnessIndex(record.tieTimeseries<double>("clearness-index")),
  d_solarDirectTs("timeseries solar direct"),
  d_solarDifuseTs("timeseries solar difuse"),
  d_clearnessIndexTs("timeseries clearness index")
{
  // initial reporting
  s_logger->repx(logga::xtra, "constructor call", getIdAndKind());

  // integrity checks
  const xeona::Hemisphere hemisphere = Entity::getHemisphere();
  if ( d_latitude < -90.0 || d_latitude > +90.0 )
    {
      s_logger->repx(logga::warn, "invalid latitude", d_latitude);
    }
  if ( d_longitude < -180.0 || d_longitude > +180.0 )
    {
      s_logger->repx(logga::warn, "invalid longitude", d_longitude);
    }
  if ( d_latitude < 0.0 && hemisphere == xeona::e_north )
    {
      s_logger->repx(logga::warn, "northern hemisphere wrong, latitude", d_latitude);
    }
  if ( d_latitude > 0.0 && hemisphere == xeona::e_south )
    {
      s_logger->repx(logga::warn, "southern hemisphere wrong, latitude", d_latitude);
    }
}

CxAmbientSolarTs::~CxAmbientSolarTs()
{
  s_logger->repx(logga::xtra, "destructor call", getIdAndKind());
}

// ---------------------------------------------------------
//  MEMBER FUNCTION : setup
// ---------------------------------------------------------

void
CxAmbientSolarTs::setup()
{
  // initial reporting
  s_logger->repx(logga::adhc, "entering member function", "CxAmbientAirTs");

  // preamble
  const int interval = Entity::getHorizonInterval();
  const int steps    = Entity::getHorizonSteps();

  // reporting string stream
  std::ostringstream put;

  // load timeseries wrappers
  d_solarDirectTs.load(d_solarDirect,
                       interval,
                       d_hasFeb29,
                       d_isLoopable);
  d_solarDifuseTs.load(d_solarDifuse,
                       interval,
                       d_hasFeb29,
                       d_isLoopable);
  d_clearnessIndexTs.load(d_clearnessIndex,
                          interval,
                          d_hasFeb29,
                          d_isLoopable);

  // reporting
  d_solarDirectTs.setCaller( XEONA_FUNC );   // preprocessor macro defined in 'common.h'
  d_solarDifuseTs.setCaller( XEONA_FUNC );
  d_clearnessIndexTs.setCaller( XEONA_FUNC );
  d_solarDirectTs.report(put);
  d_solarDifuseTs.report(put);
  d_clearnessIndexTs.report(put);
  s_logger->addSmartBlank(logga::dbug);
  s_logger->putx(logga::dbug, put);

  // gnuplot visualization
  const int plotFirst = steps;               // adaptive
  TsGnuplot gp;                              // plotting object with default command
  gp.addCaller( __FILE__ , __LINE__ );       // less whitespace is okay
  gp.addTs(d_solarDirectTs);
  gp.addTs(d_solarDifuseTs);
  gp.plot("timeseries solar data", plotFirst, 'p');

} // function 'CxAmbientSolarTs::setup'

// ---------------------------------------------------------
//  MEMBER FUNCTION : getSolar
// ---------------------------------------------------------

boost::tuple
  <double,                                   // direct irradiation [W/m^2]
   double>                                   // diffuse irradiation [W/m^2]
CxAmbientSolarTs::getSolar
(const int step) const
{
  // initial reporting
  s_logger->repx(logga::adhc, "entering member function", "");

  // CAUTION: no adjustment is currently made on clearness

  return boost::make_tuple
    (d_solarDirectTs.value(step),
     d_solarDifuseTs.value(step));
}

// ---------------------------------------------------------
//  MEMBER FUNCTION : getSolarPeek
// ---------------------------------------------------------

boost::tuple
<double,                                     // direct component
 double>                                     // diffuse component
CxAmbientSolarTs::getSolarPeek
(const int step,
 const int ahead) const
{
  // initial reporting
  s_logger->repx(logga::adhc, "entering member function, ahead", ahead);

  // active code
  const int peekIndex  = step + ahead;
  try
    {
      return boost::make_tuple
        (d_solarDirectTs.value(peekIndex),
         d_solarDifuseTs.value(peekIndex));
    }
  catch( const std::out_of_range& e )        // if not loopable
    {
      s_logger->repx(logga::warn, "cannot now peek nonloopable", peekIndex);
      return boost::make_tuple(0.0,
                               0.0);
    }
}

// ---------------------------------------------------------
//  MEMBER FUNCTION : getSolarDay
// ---------------------------------------------------------

shared_ptr<std::vector<boost::tuple <double, double> > >
CxAmbientSolarTs::getSolarDay
(const int pivot,
 const int interval) const
{
  // initial reporting
  s_logger->repx(logga::adhc, "entering member function, pivot", pivot);

  // typedef for convenience
  typedef std::vector<boost::tuple <double, double> > v2td_type;

  // preparation
  shared_ptr<v2td_type> empty;
  shared_ptr<v2td_type> buffer(new v2td_type());
  shared_ptr<std::vector<double> > direct;
  shared_ptr<std::vector<double> > difuse;

  // active code
  direct = d_solarDirectTs.dayInfo(pivot, interval);
  difuse = d_solarDifuseTs.dayInfo(pivot, interval);
  if ( ! direct || ! difuse )
    {
      s_logger->repx(logga::warn, "'TsNormal::dayInfo' call(s) failed", "");
      return empty;
    }

  // load final vector
  buffer = xeona::vector2TupleZip(direct, difuse);

  // return
  return buffer;

} // function 'CxAmbientSolarTs::getSolarDay'

// ---------------------------------------------------------
//  MEMBER FUNCTION : getSunAngles
// ---------------------------------------------------------
//  Description  : returns the 'zenith' and 'azimuth' sun angles
//  Role         : provide sun position data to entities who require it
//  Techniques   : 'xeona::timeOffseToTimestamp' 'xeona::sunpos'
//  Status       : complete
//
//  Design notes
//
//      The zenith angle [0,180] documents the sun's elevation,
//      relative to a vertical rod.  The azimuth (angle)
//      [-180,180] documents the sun's direction relative to true
//      north (with east being positive).
//
//      The bulk of the work is undertaken by free functions
//      'xeona::timeOffseToTimestamp' and 'xeona::sunpos' --
//      these are both well documented.
//
//  CAUTION: degrees used
//
//      Unless otherwise indicated, degrees and not radians are
//      used for angular measure.  Note that 'xeona' has utility
//      functions that can convert to and normalize radian
//      values.
//
// ---------------------------------------------------------

boost::tuple
<double,                                     // zenith angle (phi) [degrees]
 double>                                     // azimuth angle (gamma) [degrees]
CxAmbientSolarTs::getSunAngles
(const int step) const                       // also relies on 'TimeHorizon' data
{
  // initial reporting
  s_logger->repx(logga::adhc, "entering member function", "");

  // for convenience
  const int startYear = d_year;

  // import 'TimeHorizon' data
  const int startDay  = Entity::getHorizonStartDay();
  const int startHour = Entity::getHorizonStartHour();
  const int interval  = Entity::getHorizonInterval();

  // ---------------------------------
  //  main call
  // ---------------------------------

  // calculate hourly offset
  const int hourOffset = (step + 0.5) * interval;      // note the 0.5 adjustment

  // convert 'ydh + offset' to 'ymdhms'
  xeona::timestamp_type tstamp;
  tstamp  = xeona::timeOffseToTimestamp(startYear,
                                        startDay,
                                        startHour,
                                        hourOffset);

  // load inputs (not sure why hours, minutes, seconds are not integral)
  xeona::cTime time;
  time.iYear    = tstamp.get<0>();
  time.iMonth   = tstamp.get<1>();
  time.iDay     = tstamp.get<2>();
  time.dHours   = static_cast<double>(tstamp.get<3>());
  time.dMinutes = static_cast<double>(tstamp.get<4>());
  time.dSeconds = static_cast<double>(tstamp.get<5>());

  xeona::cLocation location;
  location.dLatitude  = d_latitude;
  location.dLongitude = d_longitude;

  xeona::cSunCoordinates coordinates;
  coordinates.dZenithAngle =  0.0;
  coordinates.dAzimuth     =  0.0;

  // make call
  xeona::sunpos(time, location, &coordinates);

  // unload outputs
  const double zenith  = coordinates.dZenithAngle;
  const double azimuth = coordinates.dAzimuth;

  // return
  return boost::make_tuple(zenith, azimuth);

} // function 'CxAmbientSolarTs::getSunAngles'

// ---------------------------------------------------------
//  MEMBER FUNCTION : getSiteElevation
// ---------------------------------------------------------
//  Description  : return elevation (altitude) of weather station site
//  Role         : typically used in airmass calculations
//  Techniques   : (nothing special)
//  Status       : complete
// ---------------------------------------------------------

double
CxAmbientSolarTs::getSiteElevation() const
{
  return d_elevation;
}

// ---------------------------------------------------------
//  MEMBER FUNCTION : getLatitude
// ---------------------------------------------------------
//  Description  : return latitude of weather station site
//  Role         : typically used for information
//  Techniques   : (nothing special)
//  Status       : complete
// ---------------------------------------------------------

double
CxAmbientSolarTs::getLatitude() const
{
  return d_latitude;
}

// ---------------------------------------------------------
//  MEMBER FUNCTION : getLongitude
// ---------------------------------------------------------
//  Description  : return longitude of weather station site
//  Role         : typically used for information
//  Techniques   : (nothing special)
//  Status       : complete
// ---------------------------------------------------------

double
CxAmbientSolarTs::getLongitude() const
{
  return d_longitude;
}

// ---------------------------------------------------------
//  CLASS           : CxAmbientSolarTMY
// ---------------------------------------------------------

// ---------------------------------------------------------
//  MEMBER FUNCTION : CxAmbientSolarTMY
// ---------------------------------------------------------

CxAmbientSolarTMY::CxAmbientSolarTMY
(const std::string entityId,
 Record&           record) :
  CxAmbientSolar(entityId, record),
  d_hoursOffset(record.tieSingle<int>("hours-offset")),
  d_daysOffset(record.tieSingle<int>("days-offset")),
  d_latitude(record.tieSingle<double>("latitude")),
  d_longitude(record.tieSingle<double>("longitude")),
  d_elevation(record.tieSingle<double>("elevation")),
  d_solarDirect(record.tieTimeseries<double>("solar-direct")),
  d_solarDifuse(record.tieTimeseries<double>("solar-diffuse")),
  d_sunZenith(record.tieTimeseries<double>("sun-zenith")),
  d_sunAzimuth(record.tieTimeseries<double>("sun-azimuth")),
  d_wx(),                                    // 'WeatherData' object from unit 'e/cxamb02'
  d_solarDirectTs("TMY solar direct empty"),
  d_solarDifuseTs("TMY solar diffuse empty"),
  d_sunZenithTs("sun zenith empty"),
  d_sunAzimuthTs("sun azimuth empty")
{
  // initial reporting
  s_logger->repx(logga::xtra, "constructor call", getIdAndKind());
}

// ---------------------------------------------------------
//  MEMBER FUNCTION : CxAmbientSolarTMY
// ---------------------------------------------------------

CxAmbientSolarTMY::~CxAmbientSolarTMY()
{
  s_logger->repx(logga::xtra, "destructor call", getIdAndKind());
}

// ---------------------------------------------------------
//  MEMBER FUNCTION : setup
// ---------------------------------------------------------

void
CxAmbientSolarTMY::setup()
{
  // initial reporting
  s_logger->repx(logga::adhc, "entering member function", "CxAmbientSolarTMY");

  // preamble
  const int interval      = Entity::getHorizonInterval();
  const int steps         = Entity::getHorizonSteps();
  const int offset        = d_hoursOffset + d_daysOffset * 24;
  const int dataInterval  = d_wx.getInterval();
  const bool dataLoopable = d_wx.isLoopable();
  const bool dataFeb29    = d_wx.hasFeb29();

  // report 'WxInfo' metadata
  std::ostringstream put;
  d_wx.metadata(put);
  s_logger->putx(logga::dbug, put);

  // load scalars
  d_latitude  = d_wx.getLatitude();
  d_longitude = d_wx.getLongitude();
  d_elevation = d_wx.getElevation();

  // create block-local timeseries wrappers for unsampled data
  TsNormal solarDirectOrig(d_wx.getSolarDirect(),      // data, default settings accepted
                           dataInterval,               // interval in seconds
                           "TMY solar direct original",
                           dataFeb29,                  // leap year data
                           dataLoopable);              // Dec to Jan seamless
  TsNormal solarDifuseOrig(d_wx.getSolarDifuse(),
                           dataInterval,
                           "TMY solar difuse original",
                           dataFeb29,
                           dataLoopable);
  TsNormal sunZenithOrig(d_wx.getZenithAngles(),
                         dataInterval,
                         "sun zenith original",
                         dataFeb29,
                         dataLoopable);
  TsNormal sunAzimuthOrig(d_wx.getAzimuthAngles(),
                          dataInterval,
                          "sun azimuth original",
                          dataFeb29,
                          dataLoopable);

  // local integrity checks -- the callee will also log details on failure
  const bool directOkay  = solarDirectOrig.hasSufficientData(interval, steps, offset);
  const bool difuseOkay  = solarDifuseOrig.hasSufficientData(interval, steps, offset);
  const bool zenithOkay  = sunZenithOrig.hasSufficientData(interval, steps, offset);
  const bool azimuthOkay = sunAzimuthOrig.hasSufficientData(interval, steps, offset);
  if ( directOkay == false || difuseOkay == false ||   // unlikely to fail independently
       zenithOkay == false || azimuthOkay == false )
    {
      s_logger->repx(logga::warn, "timeseries wrapper/s failed to load", "");
    }

  // resample
  d_solarDirectTs = solarDirectOrig.sampleLinear("solar direct resample",
                                                 interval, offset);
  d_solarDifuseTs = solarDifuseOrig.sampleLinear("solar diffuse resample",
                                                 interval, offset);
  d_sunZenithTs   = sunZenithOrig.sampleLinear("sun zenith resample",
                                               interval, offset);
  d_sunAzimuthTs  = sunAzimuthOrig.sampleLinear("sun azimuth resample",
                                                interval, offset);
  // load
  d_solarDirectTs.copyfill(d_solarDirect, steps);      // CAUTION: must resize to 'steps'
  d_solarDifuseTs.copyfill(d_solarDifuse, steps);
  d_sunZenithTs.copyfill(d_sunZenith, steps);
  d_sunAzimuthTs.copyfill(d_sunAzimuth, steps);

  // reporting
  d_solarDirectTs.setCaller( XEONA_FUNC );   // preprocessor macro defined in 'common.h'
  d_solarDifuseTs.setCaller( XEONA_FUNC );
  d_sunZenithTs.setCaller( XEONA_FUNC );
  d_sunAzimuthTs.setCaller( XEONA_FUNC );

  d_solarDirectTs.report(put);
  d_solarDifuseTs.report(put);
  d_sunZenithTs.report(put);
  d_sunAzimuthTs.report(put);

  s_logger->addSmartBlank(logga::dbug);
  s_logger->putx(logga::dbug, put);

  // gnuplot visualization
  const int plotFirst = steps;               // adaptive
  TsGnuplot gp;                              // plotting object with default command
  gp.addCaller( __FILE__ , __LINE__ );       // less whitespace is okay
  gp.addTs(d_solarDirectTs);
  gp.addTs(d_solarDifuseTs);
  gp.addTs(d_sunZenithTs);
  gp.addTs(d_sunAzimuthTs);
  gp.plot("resampled solar data", plotFirst, 'p');
}

// ---------------------------------------------------------
//  MEMBER FUNCTION : getSolar
// ---------------------------------------------------------

boost::tuple
  <double,                                   // direct irradiation [W/m^2]
   double>                                   // diffuse irradiation [W/m^2]
CxAmbientSolarTMY::getSolar
(const int step) const
{
  // initial reporting
  s_logger->repx(logga::adhc, "entering member function", "");

  // return
  return boost::make_tuple
    (d_solarDirectTs.value(step),
     d_solarDifuseTs.value(step));
}

// ---------------------------------------------------------
//  MEMBER FUNCTION : getSolarPeek
// ---------------------------------------------------------
//  Description  : obtain some future value
//  Role         : used in calculations requiring look ahead
//  Techniques   : (nothing special)
//  Status       : complete
// ---------------------------------------------------------

boost::tuple
<double,                                     // direct component
 double>                                     // diffuse component
CxAmbientSolarTMY::getSolarPeek
(const int step,
 const int ahead) const
{
  // initial reporting
  s_logger->repx(logga::adhc, "entering member function, ahead", ahead);

  // active code
  const int peekIndex  = step + ahead;
  try
    {
      return boost::make_tuple
        (d_solarDirectTs.value(peekIndex),
         d_solarDifuseTs.value(peekIndex));
    }
  catch( const std::out_of_range& e )        // if not loopable
    {
      s_logger->repx(logga::warn, "cannot now peek nonloopable", peekIndex);
      return boost::make_tuple(0.0,
                               0.0);
    }
}

// ---------------------------------------------------------
//  MEMBER FUNCTION : getSolarDay
// ---------------------------------------------------------

shared_ptr<std::vector<boost::tuple <double, double> > >
CxAmbientSolarTMY::getSolarDay
(const int pivot,
 const int interval) const
{
  // initial reporting
  s_logger->repx(logga::adhc, "entering member function, pivot", pivot);

  // typedef for convenience
  typedef std::vector<boost::tuple <double, double> > v2td_type;

  // preparation
  shared_ptr<v2td_type> empty;
  shared_ptr<v2td_type> buffer(new v2td_type());
  shared_ptr<std::vector<double> > direct;
  shared_ptr<std::vector<double> > difuse;

  // active code
  direct = d_solarDirectTs.dayInfo(pivot, interval);
  difuse = d_solarDifuseTs.dayInfo(pivot, interval);
  if ( ! direct || ! difuse )
    {
      s_logger->repx(logga::warn, "'TsNormal::dayInfo' call(s) failed", "");
      return empty;
    }

  // load final vector
  buffer = xeona::vector2TupleZip(direct, difuse);

  // return
  return buffer;
}

// ---------------------------------------------------------
//  MEMBER FUNCTION : getSunAngles
// ---------------------------------------------------------
//  Description  : returns the 'zenith' and 'azimuth' sun angles
//  Role         : provide sun position data to entities who require it
//  Techniques   : (nothing special)
//  Status       : complete
//
//  Design notes
//
//      The zenith angle [0,180] documents the sun's elevation,
//      relative to a vertical stake.  The azimuth (angle)
//      [-180,180] documents the sun's direction relative to true
//      north (with east being positive).
//
// ---------------------------------------------------------

boost::tuple
<double,                                     // zenith angle (phi) [degrees]
 double>                                     // azimuth angle (gamma) [degrees]
CxAmbientSolarTMY::getSunAngles
(const int step) const                       // also relies on 'TimeHorizon' data
{
  // initial reporting
  s_logger->repx(logga::adhc, "entering member function", "");

  // return
  return boost::make_tuple
    (d_sunZenithTs.value(step),
     d_sunAzimuthTs.value(step));
}

// ---------------------------------------------------------
//  MEMBER FUNCTION : getSiteElevation
// ---------------------------------------------------------
//  Description  : return elevation (altitude) of weather station site
//  Role         : typically used in airmass calculations
//  Techniques   : (nothing special)
//  Status       : complete
// ---------------------------------------------------------

double
CxAmbientSolarTMY::getSiteElevation() const
{
  return d_elevation;
}

// ---------------------------------------------------------
//  MEMBER FUNCTION : getLatitude
// ---------------------------------------------------------
//  Description  : return latitude of weather station site
//  Role         : typically used for information
//  Techniques   : (nothing special)
//  Status       : complete
// ---------------------------------------------------------

double
CxAmbientSolarTMY::getLatitude() const
{
  return d_latitude;
}

// ---------------------------------------------------------
//  MEMBER FUNCTION : getLongitude
// ---------------------------------------------------------
//  Description  : return longitude of weather station site
//  Role         : typically used for information
//  Techniques   : (nothing special)
//  Status       : complete
// ---------------------------------------------------------

double
CxAmbientSolarTMY::getLongitude() const
{
  return d_longitude;
}

//  end of file

