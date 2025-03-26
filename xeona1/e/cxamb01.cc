//  XEONA ENERGY SYSTEMS MODELING ENVIRONMENT
//
//  file-create-name : cxamb01.cc
//  file-create-date : Thu 07-May-2009 08:15 UTC
//  file-initiator   : Robbie Morrison <robbie@actrix.co.nz>
//
//  file-role        : concrete ambient conditions contexts 1 / implementation
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
//  $URL: file:///home/robbie/svn-root/xeona/futz/trunk/xeona1/e/cxamb01.cc $
//
//  GENERAL NOTES FOR THIS FILE

//  LOCAL AND SYSTEM INCLUDES

#include "cxamb01.h"          // companion header for this file (place first)

#include "../c/tsset.h"       // timeseries classes for added functionality
#include "../c/tsops.h"       // overloaded operators and similar for timeseries
#include "../c/recset.h"      // records and fields and also record-sets

#include "../a/logger.h"      // standard logging functionality (as required)
#include "../c/smart_ptr.h"   // toggle between Boost and TR1 smart pointers
#include ".././common.h"      // common definitions for project (place last)

#include <algorithm>          // STL copying, searching, and sorting
#include <limits>             // numeric_limits<T>::infinity() and similar
#include <numeric>            // STL numerical algorithms
#include <sstream>            // string-streams
#include <stdexcept>          // standard exception classes, runtime_error()
#include <string>             // C++ strings

#include <cmath>              // C-style maths, ceil(), floor(), sqrt()

#include <boost/format.hpp>                       // printf style formatting
#include <boost/math/constants/constants.hpp>     // high-precision mathematical constants

//  CODE

// ---------------------------------------------------------
//  CLASS (FUNCTOR) : ::SynWindMaker
// ---------------------------------------------------------
//  Description  : makes synthetic wind speed data
//  Role         : support for ambient air contexts based on internal simulation
//  Techniques   : functor, Boost.Math library, Boost.Random library
//  Status       : complete
//
//  Design notes
//
//      Caution:
//
//          See caveats, particularly regarding volatility.
//
//      Parameters:
//
//          mean  : mean wind speed (by convention in [m/s] at 10m mast height)
//
//      Output:
//
//          random wind speed which conforms to a Rayleigh distribution
//
//          Whether the output is repeatable depends on whether
//          or not the '--pepper' option is set.  In other words,
//          the random engine either relies on its default
//          internal state or is explicitly seeded on the FIRST
//          constructor call.
//
//      Typical usage:
//
//          std::vector<double> winds(10);     // timeseries of ten intervals
//          double averageWindSpeed = 10.5;    // see above for the underlying protocol
//          std::generate(winds.begin(),       // see <algorithm>
//                        winds.end(),
//                        ::SynWindMaker(averageWindSpeed));
//
//          Note carefully that the 'SynWindMaker' functor is
//          instantiated only once for the above task -- and is
//          not created anew for every element processed.
//
//  Caveats related to this implementation
//
//      Statistical issues
//
//          This method assumes independent sampling -- which is
//          readily visible in the fractured results and is
//          clearly not a good representation for real wind data
//          which is substantially attenuated by the prior wind
//          state.
//
//          That said, this code does represent a reasonable
//          first pass at synthetic wind and should also form a
//          reasonable basis for future development.
//
//          Indeed, this functor will give the correct annual
//          energy output, considering the highly nonlinear power
//          characteristics of wind turbines.
//
//          Note too that the 'Weibull' distribution, one of the
//          generalizations of the 'Rayleigh' distribution (as
//          used here), can better match real wind data -- but
//          also requires an additional characterizing parameter.
//
//      Software design
//
//          Random numbers conforming to a particular
//          distribution should not, as a rule, be generated by
//          "uniform sampling".  In fact, the Boost.Math
//          documentation states:
//
//             "Random numbers that approximate Quantiles of
//              Distributions
//
//              If you want random numbers that are distributed
//              in a specific way, for example in a uniform,
//              normal or triangular, see Boost.Random.
//
//              Whilst in principal there's nothing to prevent
//              you from using the quantile function to convert a
//              uniformly distributed random number to another
//              distribution, in practice there are much more
//              efficient algorithms available that are specific
//              to random number generation."
//
//          At the time of writing, Boost.Random did not support
//          the 'Rayleigh' distribution.
//
//  Further information
//
//      Wikipedia on wind energy and related topics.
//
//      Becker (2007 pp336-342) for use of the
//      'boost::variate_generator' class template specifically
//      and the Boost.Random library more generally.
//
//      Stephens etal (2006 pp407-410) on aspects of the software
//      design used here.
//
//      The Boost libraries documentation including Boost.Math >
//      Statistical Distributions Reference > Non-Member
//      Properties.
//
//  CAUTION: copy constructor
//
//     An accessible and working copy constructor is required.
//
// ---------------------------------------------------------

namespace xeona
{
  // CREATORS

  SynWindMaker::SynWindMaker
  (const double mean) :
    d_callCount(0),                          // initialize to zero
    d_mean(mean),
    d_raydis(d_mean * sqrt(2.0 / boost::math::constants::pi<double>()))    // [1]
  {
    s_logger->repx(logga::dbug, "constructor call, d_mean ", d_mean);
    s_logger->repx(logga::dbug, "Rayleigh distribution sigma", d_raydis.sigma());

    // first call seeding as required
    if ( s_firstCall )
      {
        if ( xeona::pepper == true )              // controls which seed to use
          {
            std::time_t stamp = std::time(0);     // CAUTION: std::time is not ideal
            boost::mt19937::result_type seed      // better than hardcoding 'unsigned'
              = static_cast<boost::mt19937::result_type>(stamp);
            s_rng.engine().seed(seed);            // CAUTION: note the form of this call
            s_firstCall = false;
            s_logger->repx(logga::dbug, "first call and seeding with", seed);
          }
        else
          {
            s_firstCall = false;
            s_logger->repx(logga::xtra, "first call but using default seed", "");
          }
      }
  }

  // [1] for the Rayleigh distribution:
  //                       ____
  //                      / pi
  //      mean = sigma   / ---
  //                   \/   2
  //
  // but here we want 'sigma', the sole distribution parameter

  SynWindMaker::~SynWindMaker()
  {
    s_logger->repx(logga::adhc, "destructor call, functor call count", d_callCount);
  }

  // MANIPULATORS

  double                                   // wind value in [m/s]
  SynWindMaker::operator()() const
    throw(std::domain_error, std::overflow_error)    // exception specification
  {
    ++d_callCount;

    // ---------------------------------
    //  generate uniform random number
    // ---------------------------------

    // generate uniform random number on interval [0,1)
    //
    // the following call does this, which is useful because
    // prob = 1.0 is not admissible for the Rayleigh
    // distribution, that is, no finite value exists for an
    // inverse cumulative probability of unity

    const double randomUniformProb = s_rng();

    // ---------------------------------
    //  generate Rayleigh-compliant data
    // ---------------------------------

    try
      {
        return boost::math::quantile(d_raydis, randomUniformProb);
      }
    catch( const std::domain_error& e )
      {
        // a probability outside [0,1] supplied (should never
        // be here as such values cannot be generated)
        s_logger->repx(logga::warn, "given probability not [0,1]", randomUniformProb);
        throw;                             // rethrow, CAUTION: usage is correct
      }
    catch( const std::overflow_error& e )
      {
        // no finite value for the specified probability
        // (should never be here as unity cannot be generated)
        s_logger->repx(logga::warn, "invalid Rayleigh probability", randomUniformProb);
        throw;                             // rethrow
      }
  }

  // STATIC DEFINITIONS

  bool SynWindMaker::s_firstCall = true;
  boost::mt19937 SynWindMaker::s_engine;
  boost::uniform_real<double> SynWindMaker::s_unidis;
  boost::variate_generator<boost::mt19937, boost::uniform_real<double> >
  SynWindMaker::s_rng(SynWindMaker::s_engine, SynWindMaker::s_unidis);

  logga::spLogger SynWindMaker::s_logger = logga::ptrLogStream();

} // unnamed namespace

// ---------------------------------------------------------
//  CLASS           : CxAmbientAir (abstract)
// ---------------------------------------------------------

// ---------------------------------------------------------
//  MEMBER FUNCTION : CxAmbientAir
// ---------------------------------------------------------

CxAmbientAir::CxAmbientAir
(const std::string entityId,
 Record&           record) :
  AmbientConditions(entityId, record)
{
  s_logger->repx(logga::xtra, "constructor call", getIdAndKind());
  d_builtinRemark = "beta";
}

// ---------------------------------------------------------
//  MEMBER FUNCTION : ~CxAmbientAir
// ---------------------------------------------------------

CxAmbientAir::~CxAmbientAir()
{
  s_logger->repx(logga::xtra, "destructor call", getIdAndKind());
}

// ---------------------------------------------------------
//  CLASS           : CxAmbientAirTs
// ---------------------------------------------------------

// ---------------------------------------------------------
//  MEMBER FUNCTION : CxAmbientAirTs
// ---------------------------------------------------------

CxAmbientAirTs::CxAmbientAirTs
(const std::string entityId,
 Record&           record) :
  CxAmbientAir(entityId, record),
  d_hasFeb29(record.tieSingle<bool>("has-feb-29")),
  d_isLoopable(record.tieSingle<bool>("is-loopable")),
  d_windSpeeds(record.tieTimeseries<double>("wind-speeds")),
  d_airTemps(record.tieTimeseries<double>("air-temps")),
  d_tempShift(record.tieSingle<double>("temperature-shift")),
  d_windSpeedTs("timeseries wind speed"),
  d_airTempTs("timeseries air temp")
{
  s_logger->repx(logga::xtra, "constructor call", getIdAndKind());
}

// ---------------------------------------------------------
//  MEMBER FUNCTION : ~CxAmbientAirTs
// ---------------------------------------------------------

CxAmbientAirTs::~CxAmbientAirTs()
{
  s_logger->repx(logga::xtra, "destructor call", getIdAndKind());
}

// ---------------------------------------------------------
//  MEMBER FUNCTION : setup
// ---------------------------------------------------------

void
CxAmbientAirTs::setup()
{
  // initial reporting
  s_logger->repx(logga::adhc, "entering member function", "CxAmbientAirTs");

  // preamble
  const int interval = Entity::getHorizonInterval();
  const int steps    = Entity::getHorizonSteps();

  // reporting string stream
  std::ostringstream put;

  // load timeseries wrappers
  d_windSpeedTs.load(d_windSpeeds,
                     interval,
                     d_hasFeb29,
                     d_isLoopable);
  d_airTempTs.load(d_airTemps,
                   interval,
                   d_hasFeb29,
                   d_isLoopable);

  // shift
  std::ostringstream oss;
  oss << "shift " << d_tempShift;            // for the sublabel
  d_airTempTs.reoffset(d_tempShift, oss.str());

  // reporting
  d_windSpeedTs.setCaller( XEONA_FUNC );     // preprocessor macro defined in 'common.h'
  d_airTempTs.setCaller( XEONA_FUNC );
  d_windSpeedTs.report(put);
  d_airTempTs.report(put);
  s_logger->addSmartBlank(logga::dbug);
  s_logger->putx(logga::dbug, put);

  // gnuplot visualization
  const int plotFirst = steps;               // adaptive
  TsGnuplot gp;                              // plotting object with default command
  gp.addCaller( __FILE__ , __LINE__ );       // less whitespace is okay
  gp.addTs(d_windSpeedTs);
  gp.addTs(d_airTempTs);
  gp.plot("timeseries weather data", plotFirst, 'p');

} // function 'CxAmbientAirTs::setup'

// ---------------------------------------------------------
//  MEMBER FUNCTION : getWindSpeed
// ---------------------------------------------------------

double
CxAmbientAirTs::getWindSpeed
(const int step) const
{
  return d_windSpeedTs.value(step);
}

// ---------------------------------------------------------
//  MEMBER FUNCTION : getWindSpeedPeek
// ---------------------------------------------------------

double
CxAmbientAirTs::getWindSpeedPeek
(const int step,
 const int ahead) const
{
  // initial reporting
  s_logger->repx(logga::adhc, "entering member function, ahead", ahead);

  // code
  const int peekIndex  = step + ahead;
  try
    {
      return d_windSpeedTs.value(peekIndex);
    }
  catch( const std::out_of_range& e )        // if not loopable
    {
      s_logger->repx(logga::warn, "cannot now peek nonloopable", peekIndex);
      return 0.0;
    }
}

// ---------------------------------------------------------
//  MEMBER FUNCTION : getAirTemp
// ---------------------------------------------------------

double
CxAmbientAirTs::getAirTemp
(const int step) const
{
  // initial reporting
  s_logger->repx(logga::adhc, "entering member function, shift", d_tempShift);

  // return
  return d_airTempTs.value(step);
}

// ---------------------------------------------------------
//  MEMBER FUNCTION : getAirTempPeek
// ---------------------------------------------------------

double
CxAmbientAirTs::getAirTempPeek
(const int step,
 const int ahead) const
{
  // initial reporting
  s_logger->repx(logga::adhc, "entering member function, ahead", ahead);

  // code
  const int peekIndex  = step + ahead;
  try
    {
      return d_airTempTs.value(peekIndex);
    }
  catch( const std::out_of_range& e )        // if not loopable
    {
      s_logger->repx(logga::warn, "cannot now peek nonloopable", peekIndex);
      return 0.0;
    }
}

// ---------------------------------------------------------
//  MEMBER FUNCTION : getAirTempDay
// ---------------------------------------------------------

shared_ptr<std::vector<double> >
CxAmbientAirTs::getAirTempDay                // 24 hours data, centered on 'pivot'
(const int pivot,                            // zero-based, often the current step
 const int interval) const                   // interval length [s]
{
  // initial reporting
  s_logger->repx(logga::adhc, "entering member function, pivot", pivot);

  // active code
  return d_airTempTs.dayInfo(pivot, interval);
}

// ---------------------------------------------------------
//  CLASS           : CxAmbientAirSim
// ---------------------------------------------------------

// ---------------------------------------------------------
//  MEMBER FUNCTION : CxAmbientAirSim
// ---------------------------------------------------------

CxAmbientAirSim::CxAmbientAirSim
(const std::string entityId,
 Record&           record) :
  CxAmbientAir(entityId, record),
  d_meanWindSpeed(record.tieSingle<double>("mean-wind-speed")),
  d_constantAirTemp(record.tieSingle<double>("constant-air-temp")),
  d_windSpeeds(record.tieTimeseries<double>("wind-speeds")),
  d_windSpeedTs("simulated wind speed"),
  d_airTempTs("simulated air temp")
{
  s_logger->repx(logga::xtra, "constructor call", getIdAndKind());
  d_builtinRemark = "beta";

  // generate synthetic timeseries
  std::generate(d_windSpeeds->begin(),
                d_windSpeeds->end(),
                xeona::SynWindMaker(d_meanWindSpeed));

  // check mean
  const int    length = d_windSpeeds->size();
  const double sum    = std::accumulate(d_windSpeeds->begin(),   // refer <numeric>
                                        d_windSpeeds->end(),
                                        0.0);     // initial value
  // report
  std::ostringstream oss;
  oss << length << "  " << d_meanWindSpeed << "  " << (sum / length) << "\n";
  s_logger->repx(logga::xtra, "size, read-in mean, calculated mean", oss.str());
}

// ---------------------------------------------------------
//  MEMBER FUNCTION : ~CxAmbientAirSim
// ---------------------------------------------------------

CxAmbientAirSim::~CxAmbientAirSim()
{
  s_logger->repx(logga::xtra, "destructor call", getIdAndKind());
}

// ---------------------------------------------------------
//  MEMBER FUNCTION : setup
// ---------------------------------------------------------

void
CxAmbientAirSim::setup()
{
  // initial reporting
  s_logger->repx(logga::adhc, "entering member function", "CxAmbientAirSim");

  // preamble
  const int interval = Entity::getHorizonInterval();
  const int steps    = Entity::getHorizonSteps();

  // reporting string stream
  std::ostringstream put;

  // also
  const bool hasFeb29   = true;
  const bool isLoopable = true;

  // load timeseries wrappers
  d_windSpeedTs.load(d_windSpeeds,
                     interval,
                     hasFeb29,
                     isLoopable);
  d_airTempTs.load(steps,                    // repetition load
                   d_constantAirTemp,
                   interval,
                   hasFeb29,
                   isLoopable);

  // reporting
  d_windSpeedTs.setCaller( XEONA_FUNC );     // preprocessor macro defined in 'common.h'
  d_airTempTs.setCaller( XEONA_FUNC );
  d_windSpeedTs.report(put);
  d_airTempTs.report(put);
  s_logger->addSmartBlank(logga::dbug);
  s_logger->putx(logga::dbug, put);

  // gnuplot visualization
  const int plotFirst = steps;               // adaptive
  TsGnuplot gp;                              // plotting object with default command
  gp.addCaller( __FILE__ , __LINE__ );       // less whitespace is okay
  gp.addTs(d_windSpeedTs);
  gp.addTs(d_airTempTs);
  gp.plot("simulated weather data", plotFirst, 'p');

} // function 'CxAmbientAirSim::setup'

// ---------------------------------------------------------
//  MEMBER FUNCTION : getWindSpeed
// ---------------------------------------------------------

double
CxAmbientAirSim::getWindSpeed
(const int step) const
{
  return d_windSpeedTs.value(step);
}

// ---------------------------------------------------------
//  MEMBER FUNCTION : getWindSpeedPeek
// ---------------------------------------------------------

double
CxAmbientAirSim::getWindSpeedPeek
(const int step,
 const int ahead) const
{
  // initial reporting
  s_logger->repx(logga::adhc, "entering member function, ahead", ahead);

  // code
  const int peekIndex  = step + ahead;
  try
    {
      return d_windSpeedTs.value(peekIndex);
    }
  catch( const std::out_of_range& e )        // if not loopable
    {
      s_logger->repx(logga::warn, "cannot now peek nonloopable", peekIndex);
      return 0.0;
    }
}

// ---------------------------------------------------------
//  MEMBER FUNCTION : getAirTemp
// ---------------------------------------------------------

double
CxAmbientAirSim::getAirTemp
(const int step) const
{
  return d_constantAirTemp;
}

// ---------------------------------------------------------
//  MEMBER FUNCTION : getAirTempPeek
// ---------------------------------------------------------

double
CxAmbientAirSim::getAirTempPeek
(const int step,
 const int ahead) const
{
  return d_constantAirTemp;
}

// ---------------------------------------------------------
//  MEMBER FUNCTION : getAirTempDay
// ---------------------------------------------------------

shared_ptr<std::vector<double> >
CxAmbientAirSim::getAirTempDay               // 24 hours data, centered on 'pivot'
(const int pivot,                            // zero-based, often the current step
 const int interval) const                   // interval length [s]
{
  // initial reporting
  s_logger->repx(logga::adhc, "entering member function, pivot", pivot);

  // active code
  return d_airTempTs.dayInfo(pivot, interval);
}

// ---------------------------------------------------------
//  CLASS           : CxAmbientAirTMY
// ---------------------------------------------------------

// ---------------------------------------------------------
//  MEMBER FUNCTION : CxAmbientAirTMY
// ---------------------------------------------------------

CxAmbientAirTMY::CxAmbientAirTMY
(const std::string entityId,
 Record&           record) :
  CxAmbientAir(entityId, record),
  d_hoursOffset(record.tieSingle<int>("hours-offset")),
  d_daysOffset(record.tieSingle<int>("days-offset")),
  d_windSpeedScale(record.tieSingle<double>("wind-speed-scale")),
  d_tempShift(record.tieSingle<double>("temperature-shift")),
  d_latitude(record.tieSingle<double>("latitude")),
  d_longitude(record.tieSingle<double>("longitude")),
  d_elevation(record.tieSingle<double>("elevation")),
  d_windSpeeds(record.tieTimeseries<double>("wind-speeds")),
  d_airTemps(record.tieTimeseries<double>("air-temps")),
  d_wx(),                                    // 'WeatherData' object from unit 'c/wxinfo'
  d_windSpeedTs("TMY wind speed empty"),
  d_airTempTs("TMY air temp empty")
{
  // initial reporting
  s_logger->repx(logga::xtra, "constructor call", getIdAndKind());

  // integrity checks
  if ( d_windSpeedScale < 0.0 )
    {
      s_logger->repx(logga::warn, "wind speed scale is negative", d_windSpeedScale);
    }
  if ( d_windSpeedScale == 0.0 )
    {
      s_logger->repx(logga::rankJumpy, "wind speed scale is zero", d_windSpeedScale);
    }
  if ( d_windSpeedScale > 10.0 )
    {
      s_logger->repx(logga::rankJumpy, "wind speed scale looks high", d_windSpeedScale);
    }
}

// ---------------------------------------------------------
//  MEMBER FUNCTION : CxAmbientAirTMY
// ---------------------------------------------------------

CxAmbientAirTMY::~CxAmbientAirTMY()
{
  s_logger->repx(logga::xtra, "destructor call", getIdAndKind());
}

// ---------------------------------------------------------
//  MEMBER FUNCTION : setup
// ---------------------------------------------------------

void
CxAmbientAirTMY::setup()
{
  // initial reporting
  s_logger->repx(logga::adhc, "entering member function", "CxAmbientAirTMY");

  // preamble
  const int  interval        = Entity::getHorizonInterval();
  const int  steps           = Entity::getHorizonSteps();
  const int  startOffset     = Entity::getHorizonOffset();
  const int  dataStartOffset = d_hoursOffset + d_daysOffset * 24;
  const int  dataInterval    = d_wx.getInterval();
  const bool dataLoopable    = d_wx.isLoopable();
  const bool dataFeb29       = d_wx.hasFeb29();

  // report 'WxInfo' metadata
  std::ostringstream put;
  d_wx.metadata(put);
  s_logger->putx(logga::dbug, put);

  // load scalars
  d_latitude  = d_wx.getLatitude();
  d_longitude = d_wx.getLongitude();
  d_elevation = d_wx.getElevation();

  // create block-local timeseries wrappers
  TsNormal windSpeedOrig(d_wx.getWindSpeeds(),     // data, default settings accepted
                         dataInterval,             // interval in seconds
                         "TMY wind speed original",
                         dataFeb29,                // leap year data
                         dataLoopable);            // Dec to Jan seamless
  TsNormal airTempOrig(d_wx.getAirTemps(),
                       dataInterval,
                       "TMY air temp original",
                       dataFeb29,
                       dataLoopable);

  // rescale the wind (there is no temperature scaling)
  windSpeedOrig.rescale(d_windSpeedScale, "wind rescale");

  // add the temp-shift (there is no wind shift)
  airTempOrig.reoffset(d_tempShift, "temp shift");

  // combine the offsets
  const double startOff = startOffset + dataStartOffset;

  // local integrity checks -- the callee will also log details on failure
  const bool windOkay = windSpeedOrig.hasSufficientData(interval, steps, startOff);
  const bool tempOkay =   airTempOrig.hasSufficientData(interval, steps, startOff);
  if ( windOkay == false || tempOkay == false )   // unlikely to fail independently
    {
      s_logger->repx(logga::warn, "timeseries wrapper/s failed to load", "");
    }

  // resample and load
  d_windSpeedTs = windSpeedOrig.sampleLinear("wind speed resample", interval, startOff);
  d_airTempTs   = airTempOrig.sampleLinear("air temp resample",     interval, startOff);
  d_windSpeedTs.copyfill(d_windSpeeds, steps);    // CAUTION: must resize to 'steps' [1]
  d_airTempTs.copyfill(d_airTemps, steps);

  // [1] the 'TsNormal::copyfill' function was developed because
  // simple (smart) pointer assignment worked during the
  // simulation but did not retain its data when written back at
  // the end of the run.  Instead it gave zeros.  Both options
  // ran clean under 'valgrind'.  I guess the answer lies in the
  // io code somewhere.

  // reporting
  d_windSpeedTs.setCaller( XEONA_FUNC );     // preprocessor macro defined in 'common.h'
  d_airTempTs.setCaller( XEONA_FUNC );
  d_windSpeedTs.report(put);
  d_airTempTs.report(put);
  s_logger->addSmartBlank(logga::dbug);
  s_logger->putx(logga::dbug, put);

  // gnuplot visualization
  const int plotFirst = steps;               // adaptive
  TsGnuplot gp;                              // plotting object with default command
  gp.addCaller( __FILE__ , __LINE__ );       // less whitespace is okay
  gp.addTs(d_windSpeedTs);
  gp.addTs(d_airTempTs);
  gp.plot("resampled weather data", plotFirst, 'p');

} // function 'CxAmbientAirTMY::setup'

// ---------------------------------------------------------
//  MEMBER FUNCTION : getWindSpeed
// ---------------------------------------------------------

double
CxAmbientAirTMY::getWindSpeed
(const int step) const
{
  // initial reporting
  s_logger->repx(logga::adhc, "entering member function", "");

  // return
  return d_windSpeedTs.value(step);
}

// ---------------------------------------------------------
//  MEMBER FUNCTION : getWindSpeedPeek
// ---------------------------------------------------------
//  Description  : obtain some future value
//  Role         : used in calculations requiring look ahead
//  Techniques   : (nothing special)
//  Status       : complete
// ---------------------------------------------------------

double
CxAmbientAirTMY::getWindSpeedPeek
(const int step,
 const int ahead) const
{
  // initial reporting
  s_logger->repx(logga::adhc, "entering member function, ahead", ahead);

  // code
  const int peekIndex  = step + ahead;
  try
    {
      return d_windSpeedTs.value(peekIndex);
    }
  catch( const std::out_of_range& e )        // if not loopable
    {
      s_logger->repx(logga::warn, "cannot now peek nonloopable", peekIndex);
      return 0.0;
    }
}

// ---------------------------------------------------------
//  MEMBER FUNCTION : getAirTemp
// ---------------------------------------------------------

double
CxAmbientAirTMY::getAirTemp
(const int step) const
{
  // initial reporting
  s_logger->repx(logga::adhc, "entering member function, shift", d_tempShift);

  // return
  return d_airTempTs.value(step);
}

// ---------------------------------------------------------
//  MEMBER FUNCTION : getAirTempPeek
// ---------------------------------------------------------
//  Description  : obtain some future value
//  Role         : used in calculations requiring look ahead
//  Techniques   : (nothing special)
//  Status       : complete
// ---------------------------------------------------------

double
CxAmbientAirTMY::getAirTempPeek
(const int step,
 const int ahead) const
{
  // initial reporting
  s_logger->repx(logga::adhc, "entering member function, ahead", ahead);

  // code
  const int peekIndex  = step + ahead;
  try
    {
      return d_airTempTs.value(peekIndex);
    }
  catch( const std::out_of_range& e )        // if not loopable
    {
      s_logger->repx(logga::warn, "cannot now peek nonloopable", peekIndex);
      return 0.0;
    }
}

// ---------------------------------------------------------
//  MEMBER FUNCTION : getAirTempDay
// ---------------------------------------------------------

shared_ptr<std::vector<double> >
CxAmbientAirTMY::getAirTempDay
(const int pivot,
 const int interval) const
{
  // initial reporting
  s_logger->repx(logga::adhc, "entering member function, pivot", pivot);

  // active code
  return d_airTempTs.dayInfo(pivot, interval);
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
CxAmbientAirTMY::getSiteElevation() const
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
CxAmbientAirTMY::getLatitude() const
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
CxAmbientAirTMY::getLongitude() const
{
  return d_longitude;
}

//  end of file

