//  XEONA ENERGY SYSTEMS MODELING ENVIRONMENT
//
//  file-create-name : sandia.cc
//  file-create-date : Mon 10-Jan-2011 12:15 UTC
//  file-initiator   : Robbie Morrison <robbie@actrix.co.nz>
//
//  file-role        : Sandia photovoltaic array model / implementation
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
//  $URL: file:///home/robbie/svn-root/xeona/futz/trunk/xeona1/h/sandia.cc $
//
//  GENERAL NOTES FOR THIS FILE

//  LOCAL AND SYSTEM INCLUDES

#include "sandia.h"           // companion header for this file (place first)

#include "../a/logger.h"      // standard logging functionality (as required)
#include "../c/smart_ptr.h"   // toggle between Boost and TR1 smart pointers
#include ".././common.h"      // common definitions for project (place last)

#include <iomanip>            // setw() and family
#include <iostream>           // standard io
#include <limits>             // numeric_limits<T>::infinity() and similar
#include <sstream>            // string-streams
#include <sstream>            // string-streams
#include <string>             // C++ strings

#include <cmath>              // C-style maths, abs(), ceil(), floor(), sqrt()

#include <boost/algorithm/string.hpp>             // string recasing, trimming, splitting
#include <boost/foreach.hpp>                      // BOOST_FOREACH iteration macro
#include <boost/format.hpp>                       // printf style formatting
#include <boost/io/ios_state.hpp>                 // stream io state savers
#include <boost/lexical_cast.hpp>                 // lexical_cast<> string to number conv
#include <boost/math/constants/constants.hpp>     // high-precision mathematical constants
#include <boost/tokenizer.hpp>                    // string tokenizer

//  PREPROCESSOR MACROS FOR CODE-SWITCH PURPOSES

// XE_ZERO_SPURIOUS_PMP: zero spurious PV panel output
// 0 = leave as-is, 1 = zero if spurious (negative or not-a-number)
// see the header file for a discussion

#define XE_ZERO_SPURIOUS_PMP 1

//  FILE-LOCAL GLOBAL CONSTANTS

namespace
{
  const double e           = boost::math::constants::e<double>();
  const double pi          = boost::math::constants::pi<double>();

  const double inf         = std::numeric_limits<double>::infinity();
  const double qan         = std::numeric_limits<double>::quiet_NaN();

  const double kBoltzmann  = +1.380658e-23;   // Boltzmann constant [J/K]
  const double qElem       = +1.602176e-19;   // elementary charge [coulomb]
  const double absolZero   = -273.15;         // absolute zero [C]
}

//  CODE

// ---------------------------------------------------------
//  CLASS           : DataValidation
// ---------------------------------------------------------

// ---------------------------------------------------------
//  MEMBER FUNCTION : DataValidation::Event::Event (nested)
// ---------------------------------------------------------

DataValidation::Event::Event
(const unsigned     count,
 const Severity     severity,
 const std::string& log,
 const std::string& message) :
  count(count),
  severity(severity),
  log(log),
  message(message)
{
}

// CREATORS

// ---------------------------------------------------------
//  MEMBER FUNCTION : DataValidation::DataValidation
// ---------------------------------------------------------

DataValidation::DataValidation() :
  d_eventCount(0),
  d_events()
{
}

// ---------------------------------------------------------
//  MEMBER FUNCTION : DataValidation::~DataValidation
// ---------------------------------------------------------

DataValidation::~DataValidation()
{
}

// ACCESSORS

// ---------------------------------------------------------
//  MEMBER FUNCTION : DataValidation::getEventCount
// ---------------------------------------------------------

unsigned
DataValidation::getEventCount() const
{
  return d_eventCount;
}

// ---------------------------------------------------------
//  MEMBER FUNCTION : DataValidation::printEvents
// ---------------------------------------------------------

unsigned
DataValidation::printEvents
(std::ostream& os) const
{
  unsigned printCount = 0;
  BOOST_FOREACH( Event e, d_events )
    {
      os << boost::format("  %02d") % e.count;
      os << boost::format("  %-7s") % interpretSeverity(e.severity);
      os << boost::format("  %s")   % e.log;;
      if (! e.message.empty() ) os << boost::format(" : %s") % e.message;
      os << "\n";                            // final newline
      ++printCount;
    }
  if ( d_events.empty() )
    {
      os << "  " << __func__ << " : no issues to report" << "\n";
    }
  return printCount;
}

// UTILITY FUNCTIONS

// ---------------------------------------------------------
//  MEMBER FUNCTION : DataValidation::interpretSeverity
// ---------------------------------------------------------

std::string
DataValidation::interpretSeverity
(const DataValidation::Severity severity) const
{
  switch( severity )
    {
    case DataValidation::e_notSpecified: return "unknown";
    case DataValidation::e_info:         return "info";
    case DataValidation::e_warn:         return "warn";
    case DataValidation::e_error:        return "error";
    }
}

// ---------------------------------------------------------
//  MEMBER FUNCTION : DataValidation::clearEvents
// ---------------------------------------------------------

unsigned                                   // previous event count
DataValidation::clearEvents()
{
  const unsigned priorCount = d_eventCount;
  d_eventCount = 0;
  d_events.clear();
  return priorCount;
}

// ---------------------------------------------------------
//  MEMBER FUNCTION : DataValidation::bounds <>
// ---------------------------------------------------------

// MANIPULATORS

template <typename T>
bool
DataValidation::bounds
(const T                        value,
 const T                        lowerBound,
 const T                        upperBound,
 const DataValidation::Severity severity,
 const std::string&             msg)         // optional message
{
  if ( value < lowerBound || value > upperBound )
    {
      std::ostringstream oss;
      oss << std::showpos;
      oss << "violated bounds : ";
      oss << value << " not [" << lowerBound << "," << upperBound << "]";
      Event event(++d_eventCount, severity, oss.str(), msg);
      d_events.push_back(event);
      return false;                          // effectively 0
    }
  else if ( std::isnan(value) )              // special treatment for 'nan's / see <cmath>
    {
      std::ostringstream oss;
      oss << "violated bounds : ";
      oss << value << " value encountered";
      Event event(++d_eventCount, severity, oss.str(), msg);
      d_events.push_back(event);
      return false;                          // effectively 0
    }
  else
    {
      return true;                           // effectively 1
    }
}

// ---------------------------------------------------------
//  MEMBER FUNCTION : DataValidation::summation <>
// ---------------------------------------------------------

template <typename T>
bool                                         // 'true' if within tolerance
DataValidation::sumation
(const T                        one,
 const T                        two,
 const T                        sum,
 const T                        tolerance,
 const DataValidation::Severity severity,
 const std::string&             msg)         // optional message
{
  const double dif = std::abs(one + two - sum);
  const double tol = std::abs(tolerance);
  if ( dif > tol )
    {
      std::ostringstream oss;
      oss << std::showpos;
      oss << "violated summation : ";
      oss << one << " + " << two << " != " << sum << " within " << tolerance;
      Event event(++d_eventCount, severity, oss.str(), msg);
      d_events.push_back(event);
      return false;                          // effectively 0
    }
  else if ( std::isnan(one)                  // see <cmath>
            ||
            std::isnan(two) )                // special treatment for 'nan's
    {
      std::ostringstream oss;
      oss << "violated summation : ";
      oss << "nan encountered " << one << " " << two;
      Event event(++d_eventCount, severity, oss.str(), msg);
      d_events.push_back(event);
      return false;                          // effectively 0
    }
  else
    {
      return true;                           // effectively 1
    }
}

// ---------------------------------------------------------
//  MEMBER FUNCTION : DataValidation::nostring
// ---------------------------------------------------------

bool
DataValidation::nostring
(const std::string&             string,
 const DataValidation::Severity severity,
 const std::string&             msg)         // optional message
{
  if ( string.empty() )                      // empty 'string'
    {
      std::ostringstream oss;
      oss << "empty string";
      Event event(++d_eventCount, severity, oss.str(), msg);
      d_events.push_back(event);
      return false;                          // effectively 0
    }
  else
    {
      return true;                           // effectively 1
    }
}

// ---------------------------------------------------------
//  MEMBER FUNCTION : DataValidation::caught
// ---------------------------------------------------------

bool
DataValidation::caught
(const std::exception& exception,
 const Severity        severity,
 const std::string&    msg)                  // optional message
{
  std::ostringstream oss;
  oss << "exception caught : ";
  oss << exception.what();
  Event event(++d_eventCount, severity, oss.str(), msg);
  d_events.push_back(event);
  return false;
}

// ---------------------------------------------------------
//  MEMBER FUNCTION : DataValidation::comment
// ---------------------------------------------------------

bool                                       // always 'false'
DataValidation::comment
(const Severity     severity,
 const std::string& msg)                     // optional message
{
  std::ostringstream oss;
  oss << "comment";
  Event event(++d_eventCount, severity, oss.str(), msg);
  d_events.push_back(event);
  return false;
}

//  EXPLICIT TEMPLATE SPECIFICATION

template
bool
DataValidation::bounds
(const int,
 const int,
 const int,
 const DataValidation::Severity,
 const std::string&);

template
bool
DataValidation::bounds
(const double,
 const double,
 const double,
 const DataValidation::Severity,
 const std::string&);

template
bool
DataValidation::sumation
(const int,
 const int,
 const int,
 const int,
 DataValidation::Severity,
 const std::string&);

template
bool
DataValidation::sumation
(const double,
 const double,
 const double,
 const double,
 DataValidation::Severity,
 const std::string&);

// ---------------------------------------------------------
//  FREE FUNCTION   : ::rad -- degrees to radians function
//  FREE FUNCTION   : ::deg -- radians to degrees function
// ---------------------------------------------------------

namespace
{
  double rad(const double degrees) { return degrees * ( ::pi / 180 ); }
  double deg(const double radians) { return radians * ( 180 / ::pi ); }
}

// ---------------------------------------------------------
//  FREE FUNCTION   : ::celsius -- kelvin to celsius function
//  FREE FUNCTION   : ::kelvin  -- celsius to kelvin function
// ---------------------------------------------------------

namespace
{
  double celsius(const double kelvin)  { return kelvin  - -::absolZero; }
  double kelvin (const double celsius) { return celsius + -::absolZero; }
}

// ---------------------------------------------------------
//  CLASS           : PvModule
// ---------------------------------------------------------
//  Description  : stand-alone Photovoltaic module simulation
//  Role         : support Photovoltaic installation entities
//  Techniques   : Sandia Photovoltaic Array Performance Model, current 2010
//  Status       : complete
//
//  Design notes
//
//      See the header file for an introduction.  See the member
//      functions for details, in particular, 'PvModule::calcSandia'.
//
//  Units representation
//
//      The use of straight ASCII presents some issues regarding
//      abbreviations for scientific units -- the following
//      convention is therefore adopted here:
//
//        [C]            degrees Celsius (also simply celsius)
//        [degrees]      angular measure
//        [coulomb]      unit of charge
//
//  Default initialization (s_init)
//
//      Quantities can be either initialized to zero or as a
//      not-a-number (NaN).  The former "looks" better when
//      reporting, but the latter is more revealing.  Note that a
//      'quiet_NaN' simply streams as "nan".
//
// ---------------------------------------------------------

// STATIC DEFINITIONS - with and without explicit initialization

logga::spLogger PvModule::s_logger = logga::ptrLogStream();

#if 1 // 0 = initialize to zero, 1 = initialize to NaN (arguably better)
const double PvModule::s_init = std::numeric_limits<double>::quiet_NaN();
#else
const double PvModule::s_init = 0.0;
#endif // 0

// ---------------------------------------------------------
//  MEMBER FUNCTION : PvModule::PvModule (constructor)
// ---------------------------------------------------------

PvModule::PvModule
(const std::string& identifier) :

  // administration
  d_identifier(identifier),
  d_loaded(false),
  d_callTracker(0),                          // tracker to ensure call order is honored
  d_valid(),

  // CSV data
  Model(),
  Vintage(),
  Area(),
  Material(),
  Series_Cells(0),
  Parallel_C_S(0),
  Isco(s_init),
  Voco(s_init),
  Impo(s_init),
  Vmpo(s_init),
  aIsc(s_init),
  aImp(s_init),
  C0(s_init),
  C1(s_init),
  BVoco(s_init),
  mBVoc(s_init),
  BVmpo(s_init),
  mBVmp(s_init),
  n(s_init),
  C2(s_init),
  C3(s_init),
  A0(s_init),
  A1(s_init),
  A2(s_init),
  A3(s_init),
  A4(s_init),
  B0(s_init),
  B1(s_init),
  B2(s_init),
  B3(s_init),
  B4(s_init),
  B5(s_init),
  dTc(s_init),
  fd(s_init),
  a(s_init),
  b(s_init),
  C4(s_init),
  C5(s_init),
  Ixo(s_init),
  Ixxo(s_init),
  C6(s_init),
  C7(s_init),

  // reference data
  T0(25.0),                                  // set the reference cell temperature here
  E0(1000.0),                                // set the reference solar irradiation here

  // other
  Zm(s_init),
  AZm(s_init),
  h(s_init),
  Zs(s_init),
  AZs(s_init),
  Edni(s_init),
  Ediff(s_init),
  Ta(s_init),
  WS(s_init),
  status(),

  // calculated intermediate values
  AM(s_init),
  AMa(s_init),
  AOI(s_init),
  E(s_init),
  Eb(s_init),
  Ee(s_init),
  PP0(s_init),
  Tc(s_init),
  Tm(s_init),
  deltaTc(s_init),
  f1(s_init),
  f2(s_init),
  Isc(s_init),
  Imp(s_init),
  Voc(s_init),
  Vmp(s_init),
  Pmp(s_init),
  FF(s_init),
  d_output(s_init),
  d_pd(s_init),
  d_effy(s_init)
{
  s_logger->repx(logga::adhc, "constructor call, identifier", d_identifier);
}

// ---------------------------------------------------------
//  MEMBER FUNCTION : PvModule::~PvModule (destructor)
// ---------------------------------------------------------

PvModule::~PvModule()
{
  s_logger->repx(logga::adhc, "destructor call", "");
}

// ---------------------------------------------------------
//  MEMBER FUNCTION : PvModule::calcSandia
// ---------------------------------------------------------
//  Description  : implementation of Sandia model
//  Role         : workhorse function
//  Techniques   : closed-form expressions, logical order of calculation
//  Status       : complete
//
//  Design notes
//
//      This treatment is based on King etal (2004) in the first
//      instance, and when that information is not sufficient,
//      then King (1997) is consulted.
//
//      Please note that the formulation of some equations in
//      King etal (2004) DIFFERS from those in King (1997), due
//      to revisions to both the underlying performance model and
//      the array characterization data format.
//
//      Please note that equations A.5 and A.6 from De Soto etal
//      (2006) are INCORRECT.  This was confirmed by Chris
//      Cameron <cpcamer@sandia.gov>, Sandia National
//      Laboratories, in an email to Robbie Morrison on
//      21-Jan-2011.
//
//      More than occasionally, the required equations are not
//      numbered, in which case the page number alone is
//      provided.
//
//      The Sandia model has some variations in terms of
//      implementation.  In this code, eq7 is used to find Ee,
//      the "effective" solar irradiance (King etal 2004, p10).
//
//      It is presumed that if the sun does not hit the module
//      surface, the output is zero -- calculations based on the
//      relative positioning of the module, the sun, and the
//      ground plane ensure that this assumption holds.
//
//      Two additional points on the I-V curve for the module,
//      designated 'x' and 'xx', are not calculated here.  But
//      they could be if an approximate I-V curve is required.
//
//      This code makes use of various 'DataValidation' member
//      functions.  Some calls use previously calculated values
//      in order to further refine the fidelity of the current
//      testing.
//
//  Dimensionality conventions
//
//      Radians used exclusively
//
//          All angles in this class are stored and processed as
//          radians.  Moreover, the standard library <cmath>
//          trigonometry functions take radians.
//
//          This practice may be contrary to that given in the
//          various King publications -- so be careful!
//
//      Celsius used exclusively
//
//          All temperatures in this class are stored and
//          processed in celsius.  If the absolute temperature in
//          kelvin is required, then that adjustment will be made
//          locally within the equation.  This treatment is
//          consistent with the various King publications, but
//          inconsistent with De Soto etal (2006) -- so be
//          careful!
//
//      Logarithm functions
//
//          The standard library <cmath> 'log' returns the
//          natural log and 'log10' returns the common log.
//
//  Publication-related matters
//
//      There are some inconsistencies arising from the various
//      publications that describe the Sandia Photovoltaic Array
//      Performance Model that users might like to take note of.
//      This section can be skipped on a first reading.
//
//      The following was confirmed by Chris Cameron
//      <cpcamer@sandia.gov>, Sandia National Laboratories in an
//      email to Robbie Morrison <robbie@actric.co.nz> on
//      20-Jan-2011:
//
//        * sometime before King etal (2004), the underlying
//          model and data format changed:
//
//            C0   was coefficient relating Isc to irradiance Ee [-]
//            C1   was coefficient relating Voc to irradiance Ee [-]
//
//            C0   now coefficient relating Imp to irradiance Ee [-]
//            C1   now coefficient relating Imp to irradiance Ee [-]
//
//        * the following fields were dropped:
//
//            Picture       was bitmap file-name, I guess
//            Description   was extended description string
//
//      Note also that:
//
//        * in King etal (2004), eq 24 is identical to eq 7,
//          which leads me (Robbie Morrison) to suspect eq 24 is
//          not set correctly
//
//        * neither King etal (2004) or King (1997) provide an
//          expression to calculate 'E', a term in eq 11 in King
//          etal (2004) -- in this code, the beam and diffuse
//          components are simply added, which seems intuitively
//          correct
//
//      Finally, more changes could occur as a result of a model
//      review being undertaken in 2011, but not, at the time of
//      writing, complete.
//
// ---------------------------------------------------------

void
PvModule::calcSandia()
{
  // ---------------------------------
  //  preamble
  // ---------------------------------

  // initial reporting
  s_logger->repx(logga::adhc, "entering member function", "");

  // ---------------------------------
  //  used in several places
  // ---------------------------------

  const double Ns = static_cast<double>(Series_Cells);

  // ---------------------------------
  //  first presume sun hits module
  // ---------------------------------

  status = "working (sun on module surface)";

  // ---------------------------------
  //  check for nighttime
  // ---------------------------------

  // check if the sun is below the ground plane and duly abort

  if ( Zs >= ::rad(90.0) )                   // CAUTION: [1]
    {
      d_output = 0.0;                        // assume zero production
      status = "night (sun below ground plane)";
      s_logger->repx(logga::adhc, "leaving member function, status", status);
      return;
    }

  // [1] Zs normally ranges [0,180] but some datasets (for
  // instance, New Zealand NIWA TMY timeseries) truncate at
  // 90 degrees -- hence nighttime conditionals must employ
  // greater-than or equal to 90.0

  // ---------------------------------
  //  AOI
  // ---------------------------------

  // AOI is solar angle-of-incidence on the array [radians]
  // note that Zm here is Tm in the papers
  //
  // King (1997, p9, eq9) (equation not given in King etal 2004)

  const double cZm = std::cos(Zm);
  const double cZs = std::cos(Zs);
  const double cAZ = std::cos(AZs-AZm);
  const double sZm = std::sin(Zm);
  const double sZs = std::sin(Zs);

  AOI = std::acos(cZm * cZs + sZm * sZs * cAZ);

  d_valid.bounds(AOI, 0.0, ::rad(180.0), dv::e_error, "invalid AOI (angle of incidence)");

  // the bounds data above is self-evident

  // ---------------------------------
  //  check that sun hits module
  // ---------------------------------

  // check if the sun is behind the module plane and duly abort

  if ( AOI >= ::rad(90.0) )
    {
      d_output = 0.0;                        // assume zero production
      status = "shaded (sun behind module surface)";
      s_logger->repx(logga::adhc, "leaving member function, status", status);
      return;
    }

  // ---------------------------------
  //  Eb and E
  // ---------------------------------

  // Eb is beam component of solar irradiance incident on the
  // module surface [W/m2]
  //
  // King etal (2004, p32, eq21)

  Eb = Edni * std::cos(AOI);

  // E is [total] solar irradiance on the module surface [W/m2]
  // E is used in cell temperature calculations only
  //
  // refer to King etal (2004, p17, eq11)

  E = Eb + Ediff;

  // CAUTION: I presume the above is correct -- the design of
  // pyranometers (as shown on wikipedia) indicates that the
  // instrument would respond to diffuse irradiance as well
  //
  // Some further descriptions of E follow:
  //
  // E is plane-of-array (POA) solar irradiance using broadband
  // pyranometer measurement corrected for angle-of-incidence
  // sensitivity [W/m2] (King 1997, p3)
  //
  // E is solar irradiance incident on module surface [W/m2]
  // (King etal 2004, p17, eq11)
  //
  // E is measured solar irradiance on module [W/m2] (King etal
  // 2004, p18, eq12)

  // ---------------------------------
  //  cell temperature
  // ---------------------------------

  // Tm is the back-surface module temperature [C]
  // WS is wind speed measured at 10m [m/s]
  // Ta is ambient air temperature [C]
  // a  is empirical coefficient for still sunny weather temperature upper limit [-]
  // b  is empirical coefficient for temperature drop from wind cooling [s/m]
  //
  // King etal (2004, p17, eq11)

  Tm = E * std::exp(a + b * WS) + Ta;

  // Tc  is the cell temperature [C]
  // Tm  (see above)
  // E   (see above)
  // E0  is the reference solar irradiation (= 1000) [W/m2]
  // dTc (d(Tc)) is cell and backing temp delta [C]
  //
  // King etal (2004, p18, eq12)

  Tc = Tm + E/E0 * dTc;

  d_valid.bounds(Tm, Ta, 70.0, dv::e_warn, "suspect Tm (back temperature)");
  d_valid.bounds(Tc, Ta, 70.0, dv::e_warn, "suspect Tc (cell temperature)");

  // the bounds data above is guesstimated: 70C seems
  // conservatively hot -- King (1997, p14, fig6) shows cell
  // temperatures up to 52C

  // ---------------------------------
  //  delta(Tc)
  // ---------------------------------

  // deltaTc is the "thermal voltage" per cell [V]

  // King etal (2004, p8, eq8)

  deltaTc = n * kBoltzmann * (::kelvin(Tc)) / qElem;

  // the units in the equation above naturally resolve to V

  d_valid.bounds(deltaTc, 0.0, 0.04, dv::e_warn, "suspect deltaTc (\"thermal voltage\")");

  // the bounds data above derives from a comment in King etal
  // (2004, p9) and from experience

  // ---------------------------------
  //  absolute air mass
  // ---------------------------------

  // AM is air mass [-] and ranges from unity to 10 or greater
  // near sunrise and sunset
  //
  // PP0 accounts for elevation effects
  //
  // AMa is absolute air mass [-], it is calculated from sun
  // elevation angle and site altitude, and it provides a
  // relative measure of the path length the sun must travel
  // through the atmosphere, AMa = 1 at sea level when the sun is
  // directly overhead (King etal 2004)
  //
  // King (1997, p7, eq6 eq7 eq8) (not given in King etal 2004)

  AM  = 1.0 / (std::cos(Zs) + 0.5057 * std::pow((96.080 - ::deg(Zs)), -1.634));
  PP0 = std::exp(-0.0001184 * h);
  AMa = PP0 * AM;

  // CAUTION: the second Zs term for AM is in degrees!
  //
  // see: http://en.wikipedia.org/wiki/Airmass#Interpolative_formulas
  //
  // the AM formula above is from Kasten, F, and AT Young.  1989.
  // Revised optical air mass tables and approximation formula.
  // Applied Optics.  v28 pp4735-4738.  (PDF not for free)
  //
  // the PP0 formula assumes a simple isothermal atmosphere
  //
  // AM ranges from unity looking straight up at sea level and,
  // for Kasten and Young, rises to approximately 38 at the
  // horizon -- gaining elevation will lead to sub-unity values
  // (wikipedia)

  d_valid.bounds(AM,  1.0, 40.0, dv::e_warn, "suspect AM (air mass)");
  d_valid.bounds(PP0, 0.0, 1.0,  dv::e_warn, "suspect PP0 (air mass adjustment)");
  d_valid.bounds(AMa, 0.5, 40.0, dv::e_warn, "suspect AMa (absolute air mass)");

  // the bounds data above is from King (1997, p7) and King etal
  // (2004, p13, fig6) and from wikipedia

  // ---------------------------------
  //  f1(AMa)
  // ---------------------------------

  // empirical function relating solar spectrum influence on Isc
  // to air mass variation [-]
  //
  // King etal (2004, p14) (equation not numbered)

  f1 =
    + A0
    + A1 * AMa
    + A2 * std::pow(AMa, 2)
    + A3 * std::pow(AMa, 3)
    + A4 * std::pow(AMa, 4);

  // f1 is strongly technology-type dependent and is bounded by
  // 1.1 and 0.6 and lower -- see King etal (2004, p13, fig6)

  d_valid.bounds(f1, 0.0, 1.1, dv::e_warn, "suspect f1 (Isc empirical AMa dependency)");

  // ---------------------------------
  //  f2(AOI)
  // ---------------------------------

  // empirical function relating optical influence of Isc to
  // solar angle of incidence [-]
  //
  // King etal (2004, p14) (equation not numbered)

  const double aoi = ::deg(AOI);             // conversion to degrees is correct

  f2 =
    + B0
    + B1 * aoi
    + B2 * std::pow(aoi, 2)
    + B3 * std::pow(aoi, 3)
    + B4 * std::pow(aoi, 4)
    + B5 * std::pow(aoi, 5);

  // f2 is bounded by 1.1 and 0.2 and lower -- see King etal
  // (2004, p13, fig7)

  d_valid.bounds(f2, 0.0, 1.1, dv::e_warn, "suspect f2 (Isc empirical AOI dependency)");

  // ---------------------------------
  //  Isc (a = alpha)
  // ---------------------------------

  // Isc is short-circuit current [A]
  // aIsc is the normalized temperature coefficient for Isc
  //
  // King etal (2004, p8, eq1) (see also King 1997, p3, eq1)

  Isc = Isco * f1 * ((Eb * f2 + fd * Ediff) / E0) * (1.0 + aIsc * (Tc - T0));

  d_valid.bounds(Isc, 0.0, 20.0, dv::e_warn, "suspect Isc (short-circuit current)");

  // the bounds data above is guesstimated, but might not apply for all modules

  // ---------------------------------
  //  Ee
  // ---------------------------------

  // Ee is "effective" solar irradiance [-]
  //
  // Ee is dimensionless but can be thought of as being measured
  // in "suns"

  // King etal (2004, p8, eq7)

  Ee = Isc / (Isco * (1.0 + aIsc * (Tc - T0)));

  // 'Ee' is the "effective" solar irradiance as previously
  // defined by equation (7).  This value describes the fraction
  // of the total solar irradiance incident on the module to
  // which the cells inside actually respond.  When tabulated
  // solar resource data are used in predicting module
  // performance, equation (7) is used directly.  When direct
  // measurements of solar resource variables are used, then
  // alternative procedures can be used for determining the
  // effective irradiance, as discussed later in this document.

  d_valid.bounds(Ee, 0.0,  1500/E0, dv::e_error, "invalid Ee (effective irradiance)");
  d_valid.bounds(Ee, 0.05, 1500/E0, dv::e_warn,  "suspect Ee (effective irradiance)");

  // the bounds data above is based on theoretical considerations
  // and on a comment from King etal (2004, p10) and the
  // associated plot (p11, fig3)

  // ---------------------------------
  //  betas (B = beta)
  // ---------------------------------

  // temperature dependent parameters
  //
  // King (2004, p16) states "normally the irradiance dependence
  // can be neglected", hence the second terms can be disabled

  // BVoc is temperature coefficient for Voc [V/C]
  // BVoco is open-circuit reference voltage [V/C]
  //
  // King etal (2004, p16) (equations not numbered)

  double BVoc;
  BVoc = BVoco + mBVoc * (1.0 - Ee);

  // BVmp is temperature coefficient for Vmp [V/C]
  // BVmpo is maximum-power reference voltage [V/C]
  //
  // King etal (2004, p16) (equations not numbered)

  double BVmp;
  BVmp = BVmpo + mBVmp * (1.0 - Ee);

  // for the Siemens Solar SM55 module, both second terms are
  // zero -- this is apparently often the case

  // ---------------------------------
  //  Imp
  // ---------------------------------

  // Imp is the maximum-power current [A]
  //
  // King etal (2004, p8, eq2)

  const double tmp1 =
    + C0 * Ee
    + C1 * std::pow(Ee, 2);

  Imp = Impo * tmp1 * (1.0 + aImp * (Tc - T0));

  d_valid.bounds(Imp, 0.0, Isc, dv::e_error, "invalid Imp (maximum power current)");

  // the bounds data above is self-evident

  // ---------------------------------
  //  Voc
  // ---------------------------------

  // Voc is the open-circuit voltage [V]
  //
  // King etal (2004, p8, eq3)
  //
  // CAUTION: De Soto etal (2006, p86, eqA.6) is INCORRECT

  const double tmp2 = deltaTc * std::log(Ee);

  Voc =
    + Voco
    + Ns * tmp2
    + BVoc * (Tc - T0);

  d_valid.bounds(Voc, 0.0, 40.0, dv::e_warn, "suspect Voc (open-circuit voltage)");

  // the bounds data above is guesstimated, but might not apply
  // for all modules

  // ---------------------------------
  //  Vmp
  // ---------------------------------

  // Vmp is the maximum-power voltage [V]
  //
  // King etal (2004, p8, eq4)
  //
  // CAUTION: De Soto etal (2006, p86, eqA.5) is INCORRECT

  const double tmp3 = deltaTc * std::log(Ee);     // same as 'tmp2'

  Vmp =
    + Vmpo
    + C2 * Ns * tmp2
    + C3 * Ns * std::pow(tmp3, 2)
    + BVmp * (Tc - T0);

  d_valid.bounds(Vmp, 0.0, Voc, dv::e_error, "invalid Vmp (maximum power voltage)");

  // the bounds data above is self-evident

  // ---------------------------------
  //  Pmp
  // ---------------------------------

  // Pmp is the maximum power
  //
  // King etal (2004, p8, eq5)

  Pmp = Imp * Vmp;

  // this is also known as Joule's law for a direct current
  // resistive circuit

  d_valid.bounds(Pmp,  0.0, ::inf, dv::e_error, "invalid Pmp (maximum power)");
  d_valid.bounds(Pmp,  0.0, 400.0, dv::e_warn,  "suspect Pmp (maximum power)");

  // the bounds data above is self-evident and guesstimated,
  // respectively

  // ---------------------------------
  //  FF
  // ---------------------------------

  // FF is the fill factor [-]
  //
  // refer to King etal (2004, p9, fig2) for some background, FF
  // effectively describes how "inflated" the I-V curve is
  //
  // King etal (2004, p8, eq6)

  FF = Pmp / (Isc * Voc);

  d_valid.bounds(FF, 0.0, 1.0, dv::e_error, "invalid FF (fill factor)");

  // the bounds data above is self-evident

  // ---------------------------------
  //  adjustment and export
  // ---------------------------------

  // the following default to 'xeona::dbug' reporting, but
  // increase to 'xeona::warn' under option '--jumpy'

  if ( std::isnan(Pmp) )
    {
      s_logger->repx(logga::rankJumpy, "Pmp (maximum power) yields NaN", Pmp);
    }
  else if ( Pmp < 0.0 )
    {
      std::ostringstream oss;
      oss << Pmp;
      s_logger->repx(logga::rankJumpy, "Pmp (maximum power) is negative", oss.str());
    }

#if   (XE_ZERO_SPURIOUS_PMP == 0)            // leave as-is

  d_output = Pmp;

#elif (XE_ZERO_SPURIOUS_PMP == 1)            // zero if spurious

  // this procedure, and the reasons for undertaking it, are
  // discussed in the class documentation

  if ( std::isnan(Pmp) )
    {
      d_output = 0.0;
      s_logger->repx(logga::dbug, "output set to zero", d_output);
    }
  else if ( Pmp < 0.0 )
    {
      d_output = 0.0;
      s_logger->repx(logga::dbug, "output set to zero", d_output);
    }
  else
    {
      d_output = Pmp;
    }

#endif // XE_ZERO_SPURIOUS_PMP

  // ---------------------------------
  //  additional metrics
  // ---------------------------------

  // d_pd is power density
  // d_effy is efficiency

  d_pd   = d_output / Area;

  if ( E == 0.0 ) d_effy = 0.0;              // div-by-zero protection
  else            d_effy = d_pd / E;

  // ---------------------------------
  //  housekeeping
  // ---------------------------------

  // completion reporting
  s_logger->repx(logga::adhc, "leaving member function, status", status);

} // function 'PvModule::calcSandia'

// ---------------------------------------------------------
//  MEMBER FUNCTION : PvModule::slurp
// ---------------------------------------------------------
//  Description  : convert Sandia CSV row to data member values
//  Role         : utility function, called on construction
//  Techniques   : 'Boost::Tokenizer' library, 'boot::lexical_cast<>'
//  Status       : complete
//  Note         : nicely coded
//
//  Design notes
//
//      This function reads in an entire CSV (comma-separated
//      variables row) as a string and assigns the values to its
//      data members.
//
//      The data format is that of the Sandia Photovoltaic Array
//      Performance Model database, as of 2010.
//
//      But note the need to convert any "" (two double-quotes)
//      sequences to \\\" (escaped escape plus escaped
//      double-quote) first.
//
//  'Boost::Tokinizer' library usage
//
//      First, see the official documentation for the
//      'boost::escaped_list_separator'.
//
//      The boost tokinizer code used here is premised on:
//
//        - fields are separated by commas
//
//        - a comma can exist within a field, but double quotes
//          must surround that field
//
//        - the following escape sequences are supported:
//          \" to " and \\ to \ and \n to newline
//
//      Under the CSV specification, the "" sequence may be used
//      to represent a single double quote within a field (to
//      indicate inch units, for instance).  Hence when an entire
//      CSV row also needs to be a valid C++ string (as here):
//
//        - any opening and closing " must be replaced by the
//          sequence \"
//
//        - any internal sequence "" must be replaced by the
//          sequence \\\"
//
// ---------------------------------------------------------

bool
PvModule::slurp
(const std::string& csv)
{
  // confirm string

  if ( csv.empty() )
    {
      s_logger->repx(logga::warn, "csv string empty", csv.length());
      return false;
    }

  // split the CSV string

  std::vector<std::string> vec;
  boost::tokenizer<boost::escaped_list_separator<char> > tk(csv);
  for ( boost::tokenizer<boost::escaped_list_separator<char> >::iterator i(tk.begin());
        i != tk.end();
        ++i )
    {
      vec.push_back(*i);
    }

  // test reporting

#if 0 // 0 = omit test reporting, 1 = include test reporting

  std::ostringstream put;
  put << "string input:" << "\n";
  BOOST_FOREACH( std::string s, vec )
    {
      if ( s.empty() ) s = "(empty)";
      put << "  " << s << "\n";
    }
  s_logger->putx(logga::adhc, put);

#endif // 0

  const int want = 42;                       // number of fields
  const int len  = vec.size();
  if ( len != want )
    {
      std::ostringstream oss;
      oss << want << " : " << len;
      s_logger->repx(logga::warn, "slurp size issue, want : got", oss.str());
      return false;
    }

  // attempt to cast and fill variables

  try
    {
      Model        = vec.at(0);
      Vintage      = vec.at(1);
      Area         = boost::lexical_cast<double>(vec.at(2));
      Material     = vec.at(3);
      Series_Cells = boost::lexical_cast<int>(vec.at(4));
      Parallel_C_S = boost::lexical_cast<int>(vec.at(5));
      Isco         = boost::lexical_cast<double>(vec.at(6));
      Voco         = boost::lexical_cast<double>(vec.at(7));
      Impo         = boost::lexical_cast<double>(vec.at(8));
      Vmpo         = boost::lexical_cast<double>(vec.at(9));
      aIsc         = boost::lexical_cast<double>(vec.at(10));
      aImp         = boost::lexical_cast<double>(vec.at(11));
      C0           = boost::lexical_cast<double>(vec.at(12));    // 2004 semantics
      C1           = boost::lexical_cast<double>(vec.at(13));    // 2004 semantics
      BVoco        = boost::lexical_cast<double>(vec.at(14));
      mBVoc        = boost::lexical_cast<double>(vec.at(15));
      BVmpo        = boost::lexical_cast<double>(vec.at(16));
      mBVmp        = boost::lexical_cast<double>(vec.at(17));
      n            = boost::lexical_cast<double>(vec.at(18));
      C2           = boost::lexical_cast<double>(vec.at(19));
      C3           = boost::lexical_cast<double>(vec.at(20));
      A0           = boost::lexical_cast<double>(vec.at(21));
      A1           = boost::lexical_cast<double>(vec.at(22));
      A2           = boost::lexical_cast<double>(vec.at(23));
      A3           = boost::lexical_cast<double>(vec.at(24));
      A4           = boost::lexical_cast<double>(vec.at(25));
      B0           = boost::lexical_cast<double>(vec.at(26));
      B1           = boost::lexical_cast<double>(vec.at(27));
      B2           = boost::lexical_cast<double>(vec.at(28));
      B3           = boost::lexical_cast<double>(vec.at(29));
      B4           = boost::lexical_cast<double>(vec.at(30));
      B5           = boost::lexical_cast<double>(vec.at(31));
      dTc          = boost::lexical_cast<double>(vec.at(32));
      fd           = boost::lexical_cast<double>(vec.at(33));
      a            = boost::lexical_cast<double>(vec.at(34));
      b            = boost::lexical_cast<double>(vec.at(35));
      C4           = boost::lexical_cast<double>(vec.at(36));
      C5           = boost::lexical_cast<double>(vec.at(37));
      Ixo          = boost::lexical_cast<double>(vec.at(38));
      Ixxo         = boost::lexical_cast<double>(vec.at(39));
      C6           = boost::lexical_cast<double>(vec.at(40));
      C7           = boost::lexical_cast<double>(vec.at(41));

    }
  catch( const boost::bad_lexical_cast& e )
    {
      d_valid.caught(e, dv::e_error, "boost::bad_lexical_cast exception");
      s_logger->repx(logga::warn, "boost::bad_lexical_cast exception", e.what());
      return false;
    }
  catch( const std::out_of_range& e )        // unlikely
    {
      d_valid.caught(e, dv::e_error, "std::out_of_range exception");
      s_logger->repx(logga::warn, "std::out_of_range exception", e.what());
      return false;
    }
  catch( ... )                               // unlikely
    {
      d_valid.comment(dv::e_error, "undifferentiated exception");
      s_logger->repx(logga::warn, "undifferentiated exception", "");
      return false;
    }

  // housekeeping

  d_loaded = true;
  return true;
}

// ---------------------------------------------------------
//  MEMBER FUNCTION : PvModule::setInstallation
// ---------------------------------------------------------
//  Description  : set the installation context
//  Role         : mandatory client-side call
//  Techniques   : '::rad' (degrees to radians function)
//  Status       : complete
// ---------------------------------------------------------

void
PvModule::setInstallation
(const double moduleTiltAngle,
 const double moduleAzimuthAngle,
 const double siteAltitude)
{
  // integrity checks
d_valid.bounds(moduleTiltAngle,       0.0, 90.0,   dv::e_error, "invalid module tilt");
d_valid.bounds(moduleTiltAngle,       0.0, 50.0,   dv::e_info,  "suspect module tilt");
d_valid.bounds(moduleAzimuthAngle, -180.0, 360.0,  dv::e_error, "invalid module azimuth");
d_valid.bounds(siteAltitude,          0.0, 5000.0, dv::e_info,  "suspect site altitude");

  // note that zenith angles are specified on [0,+90], otherwise the panel is angled down!
  // note that azimuth angles can be specified on either [-180,+180] or [0,+360]

  // it is unlikely, but not impossible, that a site elevation falls outside [0,5000]

  // set variables
  Zm  = ::rad(moduleTiltAngle);
  AZm = ::rad(moduleAzimuthAngle);
  h   = siteAltitude;

  // housekeeping
  d_callTracker += 1;                        // call order protection
}

// ---------------------------------------------------------
//  MEMBER FUNCTION : PvModule::setWeather
// ---------------------------------------------------------
//  Description  : set the prevailing weather
//  Role         : mandatory client-side call
//  Techniques   : '::rad' (degrees to radians function)
//  Status       : complete
// ---------------------------------------------------------

void
PvModule::setWeather
(const double directSolar,
 const double difuseSolar,
 const double solarZenithAngle,
 const double solarAzimuthAngle,
 const double airTemp,
 const double windSpeed)
{
  // integrity checks
  d_valid.bounds(directSolar,          0.0, 1500.0, dv::e_error, "invalid direct solar");
  d_valid.bounds(difuseSolar,          0.0, 1000.0, dv::e_error, "invalid diffuse solar");
  d_valid.bounds(solarZenithAngle,     0.0, 180.0,  dv::e_error, "invalid solar zenith");
  d_valid.bounds(solarAzimuthAngle, -180.0, 360.0,  dv::e_error, "invalid solar azimuth");
  d_valid.bounds(airTemp,            -0.90, 60.0,   dv::e_error, "invalid air temp");
  d_valid.bounds(windSpeed,            0.0, 55.0,   dv::e_error, "invalid wind speed");

  // note that zenith angles are specified on [0,+180] but may be truncated to [0,+90]
  // note that azimuth angles can be specified on either [-180,+180] or [0,+360]

  // the air temperature and wind speeds bounds are based on
  // extreme values from global weather records

  // set variables
  Edni  = directSolar;
  Ediff = difuseSolar;
  Zs    = ::rad(solarZenithAngle);
  AZs   = ::rad(solarAzimuthAngle);
  Ta    = airTemp;
  WS    = windSpeed;

  // housekeeping
  d_callTracker += 2;                        // call order protection
}

// ---------------------------------------------------------
//  MEMBER FUNCTION : PvModule::calculate
// ---------------------------------------------------------
//  Description  : check data status and call the Sandia model
//  Role         : client-side call, invoke Sandia model
//  Techniques   : predominantly protection code, 'calcSandia', 'systemLossFactor'
//  Status       : complete
//
//  Design notes
//
//      This function offers protection against missed
//      client-side calls.  For single errors, the error message
//      are correct, but for multiple errors, not all the nuances
//      are necessarily reported.
//
//      At present, under fault, this function returns zero and
//      keeps going.  This policy may need to be revisited for
//      some applications (the function could throw, for example).
//
// ---------------------------------------------------------

bool
PvModule::calculate
(double&      production,
 const double systemLossFactor)
{
  // initial reporting
  s_logger->repx(logga::adhc, "entering member function", "");

  // preamble
  production = 0.0;                          // default setting

  // integrity checks
  const double slf = systemLossFactor;
  d_valid.bounds(slf, 0.0, 1.0, dv::e_error, "invalid PV system loss factor");
  d_valid.bounds(slf, 0.5, 1.0, dv::e_info,  "suspect PV system loss factor");

  // confirm slurp
  if ( ! d_loaded )
    {
      std::ostringstream put;
      put << "  client call order issue"                                 << "\n"
          << "  explanation : 'slurp' call omitted"                      << "\n"
          << "  production set, in order to keep going : " << production << "\n";
      s_logger->repx(logga::warn, "call order issue", "");
      s_logger->putx(logga::info, put);
      d_callTracker += 4;                    // call order protection
      return false;
    }

  // confirm call order
  if ( d_callTracker != 3 )                  // means coding error in client code
    {
      std::string reason;
      switch ( d_callTracker )
        {
        case  7: reason = "'calculate' call repeated without resetting data";       break;
        case  2: reason = "'setInstallation' call omitted";                         break;
        case  1: reason = "'setWeather' call omitted";                              break;
        case  0: reason = "'setInstallation' and 'setWeather' calls omitted";       break;
        default: reason = "no simple explanation, examine the logging output";      break;
        }
      std::ostringstream put;
      put << "  client call order issue, tracker : "       << d_callTracker << "\n"
          << "  explanation : "                            << reason        << "\n"
          << "  production set, in order to keep going : " << production    << "\n";
      s_logger->repx(logga::warn, "call order issue, tracker not 3", d_callTracker);
      s_logger->putx(logga::info, put);
      d_callTracker += 4;                    // call order protection
      return false;
    }

  // run tests on Sandia module data
  //
  //   C0 + C1 = 1
  //   C4 + C5 = 1
  //   C6 + C7 = 1
  //   n is near unity

  d_valid.nostring(Model, dv::e_warn, "Model field");
  d_valid.sumation(C0, C1, 1.0, 0.01, dv::e_warn, "C0 C1 inputs");
  d_valid.sumation(C4, C5, 1.0, 0.01, dv::e_warn, "C4 C5 inputs");
  d_valid.sumation(C6, C7, 1.0, 0.01, dv::e_warn, "C6 C7 inputs");
  d_valid.bounds(n, 0.8, 1.3, dv::e_warn, "n (diode factor) input");

  // normal code
  calcSandia();                              // call the model
  production = systemLossFactor * d_output;  // export via pass-by-reference

  // completion reporting
  s_logger->repx(logga::adhc, "leaving member function, production", production);

  // housekeeping
  d_callTracker += 4;                        // call order protection
  return isNoDataIssues();                   // public function
}

// ---------------------------------------------------------
//  MEMBER FUNCTION : PvModule::reset
// ---------------------------------------------------------
//  Description  : reset data but retain module characterization
//  Role         : necessary for repeat calculations
//  Techniques   : (nothing special)
//  Status       : complete
// ---------------------------------------------------------

unsigned
PvModule::reset()
{
  // initial reporting
  s_logger->repx(logga::adhc, "entering member function, tracker", d_callTracker);

  // reset data (not strictly necessary)

  Zm       = s_init;
  AZm      = s_init;
  h        = s_init;
  Zs       = s_init;
  AZs      = s_init;
  Edni     = s_init;
  Ediff    = s_init;
  Ta       = s_init;
  WS       = s_init;
  status   = "";

  // reset intermediate values

  AM       = s_init;
  AMa      = s_init;
  AOI      = s_init;
  E        = s_init;
  Eb       = s_init;
  Ee       = s_init;
  PP0      = s_init;
  Tc       = s_init;
  Tm       = s_init;
  f1       = s_init;
  f2       = s_init;
  Isc      = s_init;
  Imp      = s_init;
  Voc      = s_init;
  Vmp      = s_init;
  Pmp      = s_init;
  FF       = s_init;
  d_output = s_init;

  // reset administration
  d_valid.clearEvents();                     // clear event log, reset counter
  const unsigned tmp = d_callTracker;
  d_callTracker      = 0;

  // return
  return tmp;
}

// ---------------------------------------------------------
//  MEMBER FUNCTION : PvModule::getIdentifier
// ---------------------------------------------------------
//  Description  : return object identifier if available
//  Role         : optional client-side call
//  Techniques   : (nothing special)
//  Status       : complete
// ---------------------------------------------------------

std::string
PvModule::getIdentifier() const
{
  return d_identifier;
}

// ---------------------------------------------------------
//  MEMBER FUNCTION : PvModule::getModel
// ---------------------------------------------------------
//  Description  : return "Model" field data if available
//  Role         : optional client-side call
//  Techniques   : (nothing special)
//  Status       : complete
// ---------------------------------------------------------

std::string
PvModule::getModel() const
{
  if ( ! d_loaded )         return "(no module data loaded)";
  else if ( Model.empty() ) return "(module data loaded but Model field empty)";
  else                      return Model;    // from the data set
}

// ---------------------------------------------------------
//  MEMBER FUNCTION : PvModule::printReport
// ---------------------------------------------------------
//  Description  : print report
//  Role         : optional client-side call
//  Techniques   : 'std::ostream', stream manipulators, 'Boost.Io_state' library
//  Status       : complete
// ---------------------------------------------------------

void
PvModule::printReport
(std::ostream& os) const
{
  // restore io state on exit in relation to 'std::showpos' and
  // any other locally modified ostream flags
  boost::io::ios_flags_saver ifs(os);

  // function name
  const std::string func = XEONA_FUNC;

  // intro

  os << "  Sandia Photovoltaic Array Performance Model Data\n"
     << "\n";
  os << "  client object identifier : " << d_identifier        << "\n"
     << "  function call            : " << func                << "\n"
     << std::showpos
     << "  default initialization   : " << s_init              << "\n"
     << std::noshowpos
     << "\n";

  // Sandia data : predetermined module coefficients

  os << "  ASCII representations of SI units\n"
     << "  C = degrees Celsius / coulomb = unit of charge / degrees = angular measure\n"
     << "\n";
  os << std::showpos
     << "  e       [-]         mathematical constant e         : " << ::e          << "\n"
     << "  pi      [-]         mathematical constant pi        : " << ::pi         << "\n"
     << "  k       [J/K]       Boltzmann constant              : " << ::kBoltzmann << "\n"
     << "  q       [coulomb]   elementary charge               : " << ::qElem      << "\n"
     << "          [C]         absolute zero                   : " << ::absolZero  << "\n"
     << std::noshowpos;
  os << "\n";
  os << std::showpos
     << "  T0      [C]         reference cell temperature      : " << T0           << "\n"
     << "  E0      [W/m2]      reference solar irradiation     : " << E0           << "\n"
     << std::noshowpos;
  os << "\n";
  os << "  Model               module identifier               : " << Model        << "\n"
     << "  Vintage             test year details               : " << Vintage      << "\n"
     << "  Area    [m2]        module area                     : " << Area         << "\n"
     << "  Material            module type                     : " << Material     << "\n"
     << "  Series_Cells (Ns)   no of cells in one cell-string  : " << Series_Cells << "\n"
     << "  Parallel_C-S (Np)   no of cell-strings in parallel  : " << Parallel_C_S << "\n"
     << std::showpos
     << "  Isco    [A]         short-circuit ref current       : " << Isco         << "\n"
     << "  Voco    [V]         open-circuit ref voltage        : " << Voco         << "\n"
     << "  Impo    [A]         max-power ref current           : " << Impo         << "\n"
     << "  Vmpo    [V]         max-power ref voltage           : " << Vmpo         << "\n"
     << "  aIsc    [1/C]       normalized temp coeff for Isc   : " << aIsc         << "\n"
     << "  aImp    [1/C]       normalized temp coeff for Imp   : " << aImp         << "\n"
     << "  C0      [-]         relating Imp to Ee              : " << C0           << "\n"
     << "  C1      [-]         relating Imp to Ee              : " << C1           << "\n"
     << "  BVoco   [V/C]       temp coeff for Voc              : " << BVoco        << "\n"
     << "  mBVoc   [V/C]       temp coeff for Voc and Ee       : " << mBVoc        << "\n"
     << "  BVmpo   [V/C]       temp coeff for Vmp              : " << BVmpo        << "\n"
     << "  mBVmp   [V/C]       temp coeff for Vmp and Ee       : " << mBVmp        << "\n"
     << "  n       [-]         diode factor                    : " << n            << "\n"
     << "  C2      [-]         relating Vmp to Ee              : " << C2           << "\n"
     << "  C3      [1/V]       relating Vmp to Ee              : " << C3           << "\n"
     << "  A0      [-]         f1 polynomial coeff             : " << A0           << "\n"
     << "  A1      [-]         f1 polynomial coeff             : " << A1           << "\n"
     << "  A2      [-]         f1 polynomial coeff             : " << A2           << "\n"
     << "  A3      [-]         f1 polynomial coeff             : " << A3           << "\n"
     << "  A4      [-]         f1 polynomial coeff             : " << A4           << "\n"
     << "  B0      [-]         f2 polynomial coeff             : " << B0           << "\n"
     << "  B1      [-]         f2 polynomial coeff             : " << B1           << "\n"
     << "  B2      [-]         f2 polynomial coeff             : " << B2           << "\n"
     << "  B3      [-]         f2 polynomial coeff             : " << B3           << "\n"
     << "  B4      [-]         f2 polynomial coeff             : " << B4           << "\n"
     << "  B5      [-]         f2 polynomial coeff             : " << B5           << "\n"
     << "  d(Tc)   [C]         cell and backing temp delta     : " << dTc          << "\n"
     << "  fd      [-]         diffuse fraction used           : " << fd           << "\n"
     << "  a       [-]         still sunny weather temp limit  : " << a            << "\n"
     << "  b       [s/m]       wind cooling dependency         : " << b            << "\n"
     << "  C4      [-]         relating Ix to Ee               : " << C4           << "\n"
     << "  C5      [-]         relating Ix to Ee               : " << C5           << "\n"
     << "  Ixo     [A]         I-V datapoint at Voc/2          : " << Ixo          << "\n"
     << "  Ixxo    [A]         I-V datapoint at (Voc-Vmp)/2    : " << Ixxo         << "\n"
     << "  C6      [-]         relating Ixx to Ee              : " << C6           << "\n"
     << "  C7      [-]         relating Ixx to Ee              : " << C7           << "\n"
     << std::noshowpos;

  // installation data

  os << "\n";
  os << std::showpos
     << "  Zm      [degrees]   module zenith angle             : " << ::deg(Zm)    << "\n"
     << "  Zm      [radians]   module zenith angle (was Tm)    : " << Zm           << "\n"
     << "  AZm     [degrees]   module azimuth angle (also Am)  : " << ::deg(AZm)   << "\n"
     << "  AZm     [radians]   module azimuth angle (also Am)  : " << AZm          << "\n"
     << "  h       [m]         altitude above mean sea level   : " << h            << "\n"
     << std::noshowpos;

  // solar resource and related weather data

  os << "\n";
  os << std::showpos
     << "  Zs      [degrees]   solar zenith angle              : " << ::deg(Zs)    << "\n"
     << "  Zs      [radians]   solar zenith angle              : " << Zs           << "\n"
     << "  AZs     [degrees]   solar azimuth angle (also As)   : " << ::deg(AZs)   << "\n"
     << "  AZs     [radians]   solar azimuth angle (also As)   : " << AZs          << "\n"
     << "  Edni    [W/m2]      direct normal irradiation       : " << Edni         << "\n"
     << "  Ediff   [W/m2]      diffuse irradiation             : " << Ediff        << "\n"
     << "  Ta      [C]         ambient air temperature         : " << Ta           << "\n"
     << "  WS      [m/s]       wind speed at 10m               : " << WS           << "\n"
     << std::noshowpos;

  // intermediate results

  os << "\n";
  os << std::showpos
     << "  AOI     [degrees]   angle of incidence from normal  : " << ::deg(AOI)   << "\n"
     << "  AOI     [radians]   angle of incidence from normal  : " << AOI          << "\n"
     << "  AM      [-]         air mass                        : " << AM           << "\n"
     << "  E       [W/m2]      total incident on module        : " << E            << "\n"
     << "  Eb      [W/m2]      beam  incident on module        : " << Eb           << "\n"
     << "  Ee      [-]         effective incident on module    : " << Ee           << "\n"
     << "  PP0     [-]         P/P0 ratio (inferred)           : " << PP0          << "\n"
     << "  AMa     [-]         absolute air mass               : " << AMa          << "\n"
     << "  Tm      [C]         back-surface module temperature : " << Tm           << "\n"
     << "  Tc      [C]         cell temperature                : " << Tc           << "\n"
     << "  deltaTc [V]         \"thermal voltage\" per cell      : " << deltaTc    << "\n"
     << "  f1      [-]         f1(AMa) empirical function      : " << f1           << "\n"
     << "  f2      [-]         f2(AOI) empirical function      : " << f2           << "\n"
     << "  Isc     [A]         short-circuit current           : " << Isc          << "\n"
     << "  Imp     [A]         max-power current               : " << Imp          << "\n"
     << "  Voc     [V]         open-circuit voltage            : " << Voc          << "\n"
     << "  Vmp     [V]         max-power voltage               : " << Vmp          << "\n"
     << std::noshowpos;

  // final results

  os << "\n";
  os << std::showpos
     << "  status              does sun hit module surface     : " << status       << "\n"
     << "  Pmp     [W]         max-power output                : " << Pmp          << "\n"
     << "  FF      [-]         fill factor                     : " << FF           << "\n"
     << "  pd      [W/m2]      power density (array area)      : " << d_pd         << "\n"
     << "  eff     [%]         conversion efficiency           : " << d_effy * 100 << "\n"
     << std::noshowpos;

  // housekeeping

  os << std::flush;
}

// ---------------------------------------------------------
//  MEMBER FUNCTION : PvModule::isNoDataIssues
// ---------------------------------------------------------
//  Description  : say if events have been encountered or not
//  Role         : optional client-side call
//  Techniques   : (nothing special)
//  Status       : complete
// ---------------------------------------------------------

bool
PvModule::isNoDataIssues() const
{
  if ( d_valid.getEventCount() == 0 ) return true;
  else                                return false;
}

// ---------------------------------------------------------
//  MEMBER FUNCTION : PvModule::printDataIssues
// ---------------------------------------------------------
//  Description  : print events log or issue "no data integrity issues revealed" message
//  Role         : optional client-side call
//  Techniques   : (nothing special)
//  Status       : complete
// ---------------------------------------------------------

void
PvModule::printDataIssues
(std::ostream& os) const
{
  // report on any data integrity issues to date
  if ( d_valid.getEventCount() != 0 )
    {
      d_valid.printEvents(os);
    }
  else
    {
      os << "  no data integrity issues revealed" << std::endl;
    }
}

//  PREPROCESSOR MACROS

#undef XE_ZERO_SPURIOUS_PMP

//  end of file

