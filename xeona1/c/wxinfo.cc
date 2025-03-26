//  XEONA ENERGY SYSTEMS MODELING ENVIRONMENT
//
//  file-create-name : wxinfo.cc
//  file-create-date : Fri 05-Nov-2010 11:29 UTC
//  file-initiator   : Robbie Morrison <robbie@actrix.co.nz>
//
//  file-role        : weather data class for testing / implementation
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
//  $URL: file:///home/robbie/svn-root/xeona/futz/trunk/xeona1/c/wxinfo.cc $
//
//  GENERAL NOTES FOR THIS FILE
//
//  AD-HOC NOTES
//
//  The horrible C-array initialization code currently used
//  should be replaced by the 'Boost::Assignment' library:
//
//      #include <boost/assign/std/vector.hpp>
//      using namespace boost::assign
//      std::vector<int> vec
//      vec += 1, 2, 3, 4, 5, 6, 7, 8, 9
//
//  Note that the upcoming C++11 standard, scheduled for late
//  2011, will support generalized initializer lists and this
//  feature should be used then.

//  LOCAL AND SYSTEM INCLUDES

#include "wxinfo.h"           // companion header for this file (place first)

#include "../a/logger.h"      // standard logging functionality (as required)
#include "../c/smart_ptr.h"   // toggle between Boost and TR1 smart pointers
#include ".././common.h"      // common definitions for project (place last)

#include <string>             // C++ strings
#include <sstream>            // string-streams

#include <algorithm>          // STL copying, searching, and sorting, max()
#include <iterator>           // STL additional iterators, std::distance()
#include <numeric>            // STL numerical algorithms

//  DATA

// ---------------------------------------------------------
//  FILE-LOCAL VARS : (various)
// ---------------------------------------------------------
//
//  Design notes
//
//      These data variables might be better implemented as a
//      struct, although the current design works well enough.
//
//      The values below are for testing.  Normally, a full-scale
//      data-set would be hash-included.
//
//  Transforming the data with emacs (a versatile text editor)
//
//      1 obtain data: download in raw form, place in text file
//        and give '.csv' extension (comma-separated variables)
//
//      2 load data: visit the CVS file with emacs, check for
//        "CSV" major mode (M-x describe mode), and enable the
//        menu bar if needed (M-x menu-bar-mode)
//
//      3 notate file: add background using the '#' comment char
//
//      4 unwrap: M-x toggle-truncate-lines (also perhaps S-F11)
//
//      5 transpose: transpose the data using M-x csv-transpose
//        (or the menu) after first placing the point inside the
//        table
//
//      6 add blank lines between rows, space out to col 6
//
//      7 manipulate signs, query-replace "," -> ", " and wrap to
//        col 90 (place point and then C-u C-x f)
//
//  CAUTION: array length in constructor must be accurate
//
//      The '::length' value must be accurate (aarrgh C-style
//      arrays!).  Note that the proposed C++11 standard will
//      allow direct initialization of STL vectors.
//
//  CAUTION: data changes not necessarily reflected
//
//      As currently programmed, changes to the data header are
//      not automatically picked up by the makefile.  So remember
//      to touch this file if you modify entries in the data
//      header.  This should be a rare occurrence in any case.
//
//  CAUTION: GNU C library 'timezone' integer
//
//      Section 21.4.8 of the GNU C library 'libc' states that
//      the library provides the long int 'timezone' for for
//      compatibility with System V Unix.  This variable holds
//      the UTC time difference in [s] with the sign reversed.
//
//      Therefore, '/usr/include/time.h' contains:
//
//          # if defined __USE_SVID || defined __USE_XOPEN
//          extern long int timezone;
//          # endif
//
//      Ambiguity problems then arise with the following local
//      code:
//
//          namespace { const double timezone = +0700; }
//          ...
//          std::cout << ::timezone;
//
//      Hence the name '::timeZone' is (now) used in this file!
//
// ---------------------------------------------------------

#define XE_WX_DATA 2

#if   (XE_WX_DATA == 1)
#  include "../g/wxdata01.h"                 // full-scale data for Colorado, USA
#elif (XE_WX_DATA == 2)
#  include "../g/wxdata02.h"                 // full-scale data for Central Otago, NZ
#else
#  ifdef __GNUG__                            // a GNU g++ compiler
#    warning "this build uses minimal test data"
#  endif

namespace
{
  const std::string description = "dummy data";
  const std::string file        = __FILE__;  // predefined C/C++ macro
  const int         interval    = 3600;      // [s]
  const int         year        = 2009;
  const double      latitude    = +8.88;
  const double      longitude   = +8.88;
  const double      elevation   = +88.8;     // [m] above mean sea level
  const double      timeZone    = +00.00;    // [h]
  const bool        loopable    = true;      // meaning Dec to Jan is seamless
  const bool        hasFeb29    = false;     // meaning not a leap year

  const double direct[]    = {   1,   6,   9,   2 };   // [W/m2] on horizontal plane
  const double difuse[]    = {   2,   2,   4,   1 };   // [W/m2] on horizontal plane
  const double zenith[]    = { 070, 020, 020, 070 };   // [degrees] on [0,180]
  const double azimuth[]   = { 300, 370, 020, 070 };   // [degrees] on [0,360]
  const double airtemp[]   = {   1,   8,  11,   7 };   // [C] dry bulb at about 1.5m
  const double windspeed[] = {   0,   8,  11,   7 };   // [m/s] speed on 6.7m or 10m mast

  const int length = sizeof(direct)/sizeof(*direct);

} // unnamed namespace

#endif // XE_WX_DATA

//  CODE

// ---------------------------------------------------------
//  CLASS           : WeatherData
// ---------------------------------------------------------

// STATIC DEFINITIONS - with and without explicit initialization

logga::spLogger WeatherData::s_logger = logga::ptrLogStream();

// CREATORS

WeatherData::WeatherData() :
  d_description(::description),
  d_file(::file),
  d_interval(::interval),
  d_length(::length),
  d_year(::year),
  d_latitude(::latitude),
  d_longitude(::longitude),
  d_elevation(::elevation),
  d_timeZone(::timeZone),
  d_loopable(::loopable),
  d_hasFeb29(::hasFeb29),
  d_solarDirect(transform(::direct)),
  d_solarDifuse(transform(::difuse)),
  d_zenithAngles(transform(::zenith)),
  d_azimuthAngles(transform(::azimuth)),
  d_airTemps(transform(::airtemp)),
  d_windSpeeds(transform(::windspeed))
{
  s_logger->repx(logga::adhc, "constructor call, description", d_description);
}

WeatherData::~WeatherData()
{
  s_logger->repx(logga::adhc, "destructor call", "");
}

//  UTILITY FUNCTIONS

// ---------------------------------------------------------
//  MEMBER FUNCTION : transform
// ---------------------------------------------------------

std::vector<double>
WeatherData::transform
(const double* array)
{
  return std::vector<double>(array, array + d_length);      // C-style array to STL vector
}

// ---------------------------------------------------------
//  MEMBER FUNCTION : rework
// ---------------------------------------------------------

bool
WeatherData::rework
(shared_ptr<std::vector<double> > ts,
 const int                        trunc,
 const int                        offset) const
{
  // return status
  bool ret = true;                           // presume success

  // integrity checks
  if ( ! ts )
    {
      s_logger->repx(logga::warn, "empty or null timeseries pointer", ts);
      return false;
    }
  if ( ts->empty() )
    {
      s_logger->repx(logga::warn, "empty timeseries", ts->size());
      return false;
    }
  if ( trunc == 0 )
    {
      s_logger->repx(logga::warn, "truncate value is zero", trunc);
      ret = false;
    }

  // metadata
  const unsigned len   = ts->size();         // underlying data length, never zero
  const unsigned open  = trunc + offset;
  const unsigned shift = offset % len;       // normalize the offset
  const double   div   = static_cast<double>(trunc) / static_cast<double>(len);
  const unsigned rep   = static_cast<unsigned>(std::ceil(div));  // round up

  // iterators, see <iterator>
  std::vector<double>::iterator beg = ts->begin();     // set iterator to beginning
  std::vector<double>::iterator end = ts->end();       // set iterator to ending
  std::vector<double>::iterator pos;                   // volatile

  // warn if insufficient data
  if ( d_loopable == false && open > len )
    {
      s_logger->repx(logga::warn, "insufficient data, require", open);
      s_logger->repx(logga::dbug, "timeseries will be padded with zeros", open - len);
      ret = false;
    }

  // process 'ts' timeseries, namely: rotate, serially append, and truncate (or add zeros)
  pos = beg + shift;                         // new start
  std::rotate(beg, pos, end);                // see <algorithm>
  for ( unsigned r = 0; r < rep; ++r )       // progressively append
    {
      // CAUTION: the iterators must be recalculated each time
      beg = ts->begin();                     // beginning of pattern
      end = beg + len;                       // ending of pattern
      pos = ts->end();                       // current ending
      ts->insert(pos, beg, end);             // 'std::vector<>::insert' returns 'void'
    }
  ts->resize(trunc);                         // truncate (or add zeros if under-length)

  // return status
  return ret;
}

//  ACCESSORS - NORMAL AND STATIC

// ---------------------------------------------------------
//  MEMBER FUNCTION : metadata
// ---------------------------------------------------------

void
WeatherData::metadata
(std::ostream& os) const
{
  const std::string comment = "solar isolation, sun angles, air temperature, wind speed";
  os << std::boolalpha
     << "  location                 : "  << d_description    << "\n"
     << "  contains                 : "  << comment          << "\n"
     << "  local data               : '" << d_file    << "'" << "\n"
     << "  year                     : "  << d_year           << "\n"
     << "  interval [s]             : "  << d_interval       << "\n"
     << "  entries [-]              : "  << d_length         << "\n"
     << std::showpos
     << "  latitude [degrees]       : "  << d_latitude       << "\n"
     << "  longitude [degrees]      : "  << d_longitude      << "\n"
     << "  elevation [m]            : "  << d_elevation      << "\n"
     << "  timezone (re UTC) [h]    : "  << d_timeZone       << "\n"
     << std::noshowpos
     << "  loopable (Dec/Jan break) : "  << d_loopable       << "\n"
     << "  leap year (29-Feb) data  : "  << d_hasFeb29       << "\n"
     << std::flush;
}

// ---------------------------------------------------------
//  MEMBER FUNCTION : summarizeTimeseries
// ---------------------------------------------------------

bool
WeatherData::summarizeTimeseries             // CAUTION: static function
(shared_ptr<std::vector<double> > input,
 const std::string&               tag,
 std::ostream&                    os,
 const int                        skip)      // note the default of zero
{
  // add blank lines as required
  for ( int i = 0; i < skip; ++i) os << "\n";

  // introductory tag (if supplied)
  if ( tag.empty() ) os << "  tag     : (not set)" << "\n";
  else               os << "  tag     : " << tag   << "\n";

  // protection
  if ( ! input )
    {
      os << "  FATAL   : empty or null pointer" << "\n"
         << std::flush;
      return false;
    }
  if ( input->empty() )
    {
      os << "  FATAL   : empty vector" << "\n"
         << std::flush;
      return false;
    }

  // calculations
  const int    length = input->size();       // CAUTION: 'd_length' is not available
  const double first  = input->front();
  const double last   = input->back();
  const double maxval = *std::max_element(input->begin(), input->end());
  const double minval = *std::min_element(input->begin(), input->end());
  const double range  = maxval - minval;
  const double total  = std::accumulate(input->begin(), input->end(), 0.0);
  const double mean   = total / length;

  // reporting (quick and dirty ios state treatment)
  os << "  length  : " << length   << "\n"
     << std::showpos
     << "  first   : " << first    << "\n"
     << "  last    : " << last     << "\n"
     << "  min     : " << minval   << "\n"
     << "  max     : " << maxval   << "\n"
     << "  range   : " << range    << "\n"
     << "  mean    : " << mean     << "\n"
     << std::noshowpos
     << std::flush;

  // return success
  return true;
}

// ---------------------------------------------------------
//  MEMBER FUNCTION : get scalar functions
// ---------------------------------------------------------

std::string
WeatherData::getDescription() const
{
  return d_description;
}

std::string
WeatherData::getFile() const
{
  return d_file;
}

int
WeatherData::getInterval() const
{
  return d_interval;
}

int
WeatherData::getLength() const
{
  return d_length;
}

int
WeatherData::getYear() const
{
  return d_year;
}

double
WeatherData::getLatitude() const
{
  return d_latitude;
}

double
WeatherData::getLongitude() const
{
  return d_longitude;
}

double
WeatherData::getElevation() const
{
  return d_elevation;
}

double
WeatherData::getTimezone() const
{
  return d_timeZone;
}

bool
WeatherData::isLoopable() const
{
  return d_loopable;
}

bool
WeatherData::hasFeb29() const
{
  return d_hasFeb29;
}

// ---------------------------------------------------------
//  MEMBER FUNCTION : get timeseries functions
// ---------------------------------------------------------

// FULL VECTORS

shared_ptr<std::vector<double> >
WeatherData::getSolarDirect() const
{
  shared_ptr<std::vector<double> > buffer(new std::vector<double>(d_solarDirect));
  return buffer;
}

shared_ptr<std::vector<double> >
WeatherData::getSolarDifuse() const
{
  shared_ptr<std::vector<double> > buffer(new std::vector<double>(d_solarDifuse));
  return buffer;
}

shared_ptr<std::vector<double> >
WeatherData::getZenithAngles() const
{
  shared_ptr<std::vector<double> > buffer(new std::vector<double>(d_zenithAngles));
  return buffer;
}

shared_ptr<std::vector<double> >
WeatherData::getAzimuthAngles() const
{
  shared_ptr<std::vector<double> > buffer(new std::vector<double>(d_azimuthAngles));
  return buffer;
}

shared_ptr<std::vector<double> >
WeatherData::getAirTemps() const
{
  shared_ptr<std::vector<double> > buffer(new std::vector<double>(d_airTemps));
  return buffer;
}

shared_ptr<std::vector<double> >
WeatherData::getWindSpeeds() const
{
  shared_ptr<std::vector<double> > buffer(new std::vector<double>(d_windSpeeds));
  return buffer;
}

// TRUNCATED VECTORS

shared_ptr<std::vector<double> >
WeatherData::getSolarDirect
(const int trunc,
 const int offset) const
{
  shared_ptr<std::vector<double> > buffer(new std::vector<double>(d_solarDirect));
  if ( rework(buffer, trunc, offset) != true )
    {
      s_logger->repx(logga::warn, "problems encountered (see above)", "");
    }
  return buffer;
}

shared_ptr<std::vector<double> >
WeatherData::getSolarDifuse
(const int trunc,
 const int offset) const
{
  shared_ptr<std::vector<double> > buffer(new std::vector<double>(d_solarDifuse));
  if ( rework(buffer, trunc, offset) != true )
    {
      s_logger->repx(logga::warn, "problems encountered (see above)", "");
    }
  return buffer;
}

shared_ptr<std::vector<double> >
WeatherData::getZenithAngles
(const int trunc,
 const int offset) const
{
  shared_ptr<std::vector<double> > buffer(new std::vector<double>(d_zenithAngles));
  if ( rework(buffer, trunc, offset) != true )
    {
      s_logger->repx(logga::warn, "problems encountered (see above)", "");
    }
  return buffer;
}

shared_ptr<std::vector<double> >
WeatherData::getAzimuthAngles
(const int trunc,
 const int offset) const
{
  shared_ptr<std::vector<double> > buffer(new std::vector<double>(d_azimuthAngles));
  if ( rework(buffer, trunc, offset) != true )
    {
      s_logger->repx(logga::warn, "problems encountered (see above)", "");
    }
  return buffer;
}

shared_ptr<std::vector<double> >
WeatherData::getAirTemps
(const int trunc,
 const int offset) const
{
  shared_ptr<std::vector<double> > buffer(new std::vector<double>(d_airTemps));
  if ( rework(buffer, trunc, offset) != true )
    {
      s_logger->repx(logga::warn, "problems encountered (see above)", "");
    }
  return buffer;
}

shared_ptr<std::vector<double> >
WeatherData::getWindSpeeds
(const int trunc,
 const int offset) const
{
  shared_ptr<std::vector<double> > buffer(new std::vector<double>(d_windSpeeds));
  if ( rework(buffer, trunc, offset) != true )
    {
      s_logger->repx(logga::warn, "problems encountered (see above)", "");
    }
  return buffer;
}

//  end of file

