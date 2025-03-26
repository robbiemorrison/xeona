//  XEONA ENERGY SYSTEMS MODELING ENVIRONMENT
//
//  file-create-name : cxamb01.h
//  file-create-date : Thu 07-May-2009 08:15 UTC
//  file-initiator   : Robbie Morrison <robbie@actrix.co.nz>
//
//  file-role        : concrete ambient conditions contexts 1 / header
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
//  $URL: file:///home/robbie/svn-root/xeona/futz/trunk/xeona1/e/cxamb01.h $
//
//  GENERAL NOTES FOR THIS FILE
//
//  The contexts here cover ambient air.

//  HEADER GUARD

#ifndef _CXAMB01_H_
#define _CXAMB01_H_

//  LOCAL AND SYSTEM INCLUDES

#include "../e/context.h"     // abstract context entity
#include "../c/wxinfo.h"      // weather data class for testing
#include "../c/tsset.h"       // timeseries classes for added functionality

#include "../a/logger_fwd.h"  // some forward declarations from "logger.h"
#include "../c/smart_ptr.h"   // toggle between Boost and TR1 smart pointers

#include <string>             // C++ strings
#include <sstream>            // string-streams
#include <vector>             // STL sequence container

#include <boost/random.hpp>                       // convenience header for Boost.Random
#include <boost/math/distributions/rayleigh.hpp>  // Rayleigh statistical distribution

//  FORWARD (PARTIAL) DECLARATIONS

class Record;                                // constructor argument

//  CODE

// ---------------------------------------------------------
//  CLASS (FUNCTOR) : xeona::SynWindMaker
// ---------------------------------------------------------
//  Description  : makes synthetic wind speed data
//  Role         : support for ambient air contexts based on internal simulation
//  Techniques   : functor, 'Boost.Math' library, 'Boost.Random' library
//  Status       : complete
//
//  Design notes
//
//      See caveats, particularly regarding volatility.
//
// ---------------------------------------------------------

namespace xeona
{
  class SynWindMaker
  {
    // DISABLED

  private:

    SynWindMaker();                                         // zero-argument constructor
    SynWindMaker& operator= (const SynWindMaker& orig);     // copy assignment operator

    // CREATORS

  public:

    SynWindMaker
    (const double mean);

    ~SynWindMaker();

    // MANIPULATORS

  public:

    double                                   // wind value in [m/s]
    operator()() const
      throw(std::domain_error, std::overflow_error);   // exception specification

    // INSTANCE DATA

  private:

    mutable int                                         d_callCount;  // call count [1]
    const double                                        d_mean;       // supplied mean
    const boost::math::rayleigh_distribution<double>    d_raydis;     // distribution

    // [1] necessary because operator() is 'const'

    // STATIC DATA

  private:

    static bool                           s_firstCall; // first call status
    static boost::mt19937                 s_engine;    // random generator engine
    static boost::uniform_real<double>    s_unidis;    // distribution
    static boost::variate_generator
    <
      boost::mt19937,
      boost::uniform_real<double>
    >                                     s_rng;       // later glue above for generator

    static logga::spLogger                s_logger;    // shared_ptr to single logger obj
  };

} // namespace 'xeona'

// ---------------------------------------------------------
//  CLASS           : CxAmbientAir
// ---------------------------------------------------------
//  Description  : interface base for ambient air contexts
//  Role         : step in the inheritance web
//  Techniques   : (nothing special)
//  Status       : complete
//
//  Design notes
//
//      This class cannot be abstract because it must be
//      instantiated during the entity linking process.
//      Therefore trivial definitions are needed for all virtual
//      functions, even though they will never be called.
//
// ---------------------------------------------------------

class CxAmbientAir :
  public AmbientConditions
{
  // DISABLED

private:

  CxAmbientAir();                                      // zero-argument constructor
  CxAmbientAir(const CxAmbientAir& orig);              // copy constructor
  CxAmbientAir& operator= (const CxAmbientAir& orig);  // copy assignment operator

  // CREATORS

public:

  explicit
  CxAmbientAir
  (const std::string entityId) :             // special constructor to facilitate linking
    AmbientConditions(entityId)
  { }

  explicit
  CxAmbientAir
  (const std::string entityId,
   Record&           record);

  virtual
  ~CxAmbientAir();

  // LINKING CALL

private:                                     // CAUTION: 'private' is correct

  virtual
  bool
  polylink(assign_ptr<Entity>& pass)         // CAUTION: 'Entity' is correct
  {
    s_logger->repx(logga::adhc, "entering virtual member function", "CxAmbientAir");
    const bool okay = pass.revamp(retFull<CxAmbientAir>());      // key call

    // low priority development reporting
    shared_ptr<CxAmbientAir> full = retFull<CxAmbientAir>();
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
  double                                     // units [m/s]
  getWindSpeed                               // see proper definition
  (const int step) const  { return s_doubleZero; }

  virtual
  double
  getWindSpeedPeek                           // see proper definition
  (const int step,
   const int ahead) const { return s_doubleZero; }

  virtual
  double                                     // units [C]
  getAirTemp                                 // see proper definition
  (const int step) const  { return s_doubleZero; }

  virtual
  double
  getAirTempPeek                             // see proper definition
  (const int step,
   const int ahead) const { return s_doubleZero; }

  virtual
  shared_ptr<std::vector<double> >
  getAirTempDay                              // see proper definition
  (const int pivot,
   const int interval) const { return shared_ptr<std::vector<double> >(); }

  virtual
  double
  getSiteElevation() const { return 0.0; }   // see proper definition

  virtual
  double
  getLatitude() const { return 0.0; }        // see proper definition

  virtual
  double
  getLongitude() const { return 0.0; }       // see proper definition

};

// ---------------------------------------------------------
//  CLASS           : CxAmbientAirTs
// ---------------------------------------------------------
//  Description  : ambient air context based on exogenous timeseries
//  Role         : ambient conditions support
//  Techniques   : 'std::vector' 'TsNormal'
//  Status       : complete
// ---------------------------------------------------------

class CxAmbientAirTs :
  public CxAmbientAir
{
  // DISABLED

private:

  CxAmbientAirTs();                                         // zero-argument constructor
  CxAmbientAirTs(const CxAmbientAirTs& orig);               // copy constructor
  CxAmbientAirTs& operator= (const CxAmbientAirTs& orig);   // copy assignment operator

  // CREATORS

public:

  explicit
  CxAmbientAirTs
  (const std::string entityId,
   Record&           record);

  virtual
  ~CxAmbientAirTs();

  // CALLS

  virtual
  void
  setup();

  // ACCESSORS

public:

  virtual
  double                                     // units [m/s]
  getWindSpeed
  (const int step) const;

  virtual
  double
  getWindSpeedPeek
  (const int step,
   const int ahead = 1) const;               // steps look ahead

  virtual
  double                                     // units [C]
  getAirTemp
  (const int step) const;

  virtual
  double
  getAirTempPeek
  (const int step,
   const int ahead = 1) const;               // steps look ahead

  virtual
  shared_ptr<std::vector<double> >
  getAirTempDay                              // 24 hours of data, centered on 'pivot'
  (const int pivot,                          // zero-based, often the current step
   const int interval) const;                // interval length [s]

  // INSTANCE DATA

private:

  // tied quanties

  const bool&                               d_hasFeb29;
  const bool&                               d_isLoopable;

  const shared_ptr<std::vector<double> >    d_windSpeeds;
  const shared_ptr<std::vector<double> >    d_airTemps;
  const double&                             d_tempShift;

  // local quantities

  TsNormal                                  d_windSpeedTs;
  TsNormal                                  d_airTempTs;

};

//  ==== XEDOC =================================================
//
//  entity.cx-ambient-air-ts-0
//
//      class                                    > CxAmbientAirTs
//
//        ambient air context based on exogenous timeseries
//
//      builtin-remark s                         <
//
//      has-feb-29 [-] b                         > 0
//      is-loopable [-] b                        > 0
//
//        has-feb-29 and is-loopable provide metadata
//
//      wind-speeds [m/s] F                      > 13.0 14.0 ..
//      air-temps [C] F                          > 10.0 11.0 ..
//      temperature-shift [C] f                  > 0.0
//
//        the temperature-shift is used to move the output value
//        accordingly
//
//  ============================================================

// ---------------------------------------------------------
//  CLASS           : CxAmbientAirSim
// ---------------------------------------------------------
//  Description  : ambient air context based on internal simulation
//  Role         : ambient conditions support
//  Techniques   : weather simulation
//  Status       : complete
// ---------------------------------------------------------

class CxAmbientAirSim :
  public CxAmbientAir
{
  // DISABLED

private:

  CxAmbientAirSim();                                         // zero-argument constructor
  CxAmbientAirSim(const CxAmbientAirSim& orig);              // copy constructor
  CxAmbientAirSim& operator= (const CxAmbientAirSim& orig);  // copy assignment operator

  // CREATORS

public:

  explicit
  CxAmbientAirSim
  (const std::string entityId,
   Record&           record);

  virtual
  ~CxAmbientAirSim();

  // CALLS

  virtual
  void
  setup();

  // ACCESSORS

public:

  virtual
  double                                     // units [m/s]
  getWindSpeed
  (const int step) const;

  virtual
  double
  getWindSpeedPeek
  (const int step,
   const int ahead = 1) const;               // steps look ahead

  virtual
  double                                     // units [C]
  getAirTemp
  (const int step) const;

  virtual
  double
  getAirTempPeek
  (const int step,
   const int ahead = 1) const;               // steps look ahead

  virtual
  shared_ptr<std::vector<double> >
  getAirTempDay                              // 24 hours of data, centered on 'pivot'
  (const int pivot,                          // zero-based, often the current step
   const int interval) const;                // interval length [s]

  // INSTANCE DATA

private:

  // tied quantities

  const double&                       d_meanWindSpeed;
  const double&                       d_constantAirTemp;    // constant
  shared_ptr<std::vector<double> >    d_windSpeeds;         // generated timeseries

  // local quantities

  TsNormal                            d_windSpeedTs;
  TsNormal                            d_airTempTs;

};

//  ==== XEDOC =================================================
//
//  entity.cx-ambient-air-sim-0
//
//      class                                    > CxAmbientAirSim
//
//        an ambient air context with wind speeds sampled from a
//        stateless Rayleigh distribution -- meaning that the
//        prior values do not influence the current value
//
//      builtin-remark s                         <
//
//      mean-wind-speed [m/s] f                  > 10.5
//      constant-air-temp [C] f                  > 15.0
//
//        mean-wind-speed is often measured at 10m above ground
//
//        constant-air-temp allows this entity to provide
//        temperature data in line with the CxAmbientAir
//        interface
//
//      wind-speeds [m/s] F                      < 0.0 ..
//
//        the wind-speeds are duly calculated
//
//  ============================================================

// ---------------------------------------------------------
//  CLASS           : CxAmbientAirTMY
// ---------------------------------------------------------
//  Description  : ambient air context based on TMY data
//  Role         : ambient conditions support
//  Techniques   : 'std::vector', 'c/wxinfo02.cc'
//  Status       : complete
// ---------------------------------------------------------

class CxAmbientAirTMY :
  public CxAmbientAir
{
  // DISABLED

private:

  CxAmbientAirTMY();                                         // zero-argument constructor
  CxAmbientAirTMY(const CxAmbientAirTMY& orig);              // copy constructor
  CxAmbientAirTMY& operator= (const CxAmbientAirTMY& orig);  // copy assignment operator

  // CREATORS

public:

  explicit
  CxAmbientAirTMY
  (const std::string entityId,
   Record&           record);

  virtual
  ~CxAmbientAirTMY();

  // CALLS

  virtual
  void
  setup();

  // ACCESSORS

public:

  virtual
  double                                     // units [m/s]
  getWindSpeed
  (const int step) const;

  virtual
  double
  getWindSpeedPeek
  (const int step,
   const int ahead = 1) const;               // steps look ahead

  virtual
  double                                     // units [C]
  getAirTemp
  (const int step) const;

  virtual
  double
  getAirTempPeek
  (const int step,
   const int ahead = 1) const;               // steps look ahead

  virtual
  shared_ptr<std::vector<double> >
  getAirTempDay                              // 24 hours of data, centered on 'pivot'
  (const int pivot,                          // zero-based, often the current step
   const int interval) const;                // interval length [s]

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
  const double&                       d_windSpeedScale;
  const double&                       d_tempShift;

  double&                             d_latitude;
  double&                             d_longitude;
  double&                             d_elevation;

  shared_ptr<std::vector<double> >    d_windSpeeds;
  shared_ptr<std::vector<double> >    d_airTemps;

  // local quantities

  const WeatherData                   d_wx;  // see unit 'c/wxinfo'
  TsNormal                            d_windSpeedTs;
  TsNormal                            d_airTempTs;

};

//  ==== XEDOC =================================================
//
//  entity.cx-ambient-air-tmy-0
//
//      class                                    > CxAmbientAirTMY
//
//        ambient air context which serves the NIWA TMY (typical
//        meteorological year) dataset for Otago, New Zealand
//
//        resampling is supported
//
//      builtin-remark s                         <
//
//      latitude [deg] f                         < 0
//      longitude [deg] f                        < 0
//      elevation [m] f                          < 0
//
//      hours-offset [-] i                       > 0
//      days-offset [-] i                        > 0
//      wind-speed-scale [-] f                   > 1.0
//      temperature-shift [C] f                  > 0.0
//
//        hours-offset and days-offset are used to vary the
//        starting point, the wind-speed-scale temperature-shift
//        are used to scale and move the output value accordingly
//
//      wind-speeds [m/s] F                      < 0.0 ..
//      air-temps [C] F                          < 0.0 ..
//
//  ============================================================

#endif // _CXAMB01_H_

//  end of file

