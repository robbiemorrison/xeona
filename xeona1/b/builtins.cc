//  XEONA ENERGY SYSTEMS MODELING ENVIRONMENT
//
//  file-create-name : builtins.cc
//  file-create-date : Mon 28-Jan-2008 18:25 UTC
//  file-initiator   : Robbie Morrison <robbie@actrix.co.nz>
//
//  file-role        : time horizon entity / implementation
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
//  $URL: file:///home/robbie/svn-root/xeona/futz/trunk/xeona1/b/builtins.cc $

//  LOCAL AND SYSTEM INCLUDES

#include "builtins.h"         // companion header for this file (place first)

#include "../c/recset.h"      // record set support
#include "../a/logger.h"      // standard logging functionality (as required)
#include "../c/smart_ptr.h"   // toggle between Boost and TR1 smart pointers
#include ".././common.h"      // common definitions for project (place last)

#include <stdexcept>          // standard exception classes, runtime_error()

#include <boost/assign/std/vector.hpp>  // assign operator+=() for std::vector

//  NAMESPACE DIRECTIVES

using namespace boost::assign;

//  CODE

// ---------------------------------------------------------
//  CLASS           : TimeHorizon
// ---------------------------------------------------------

// STATIC DEFINITIONS - with and without explicit initialization

// 's_intervals' contains a list of valid intervals

#define S_INTERVALS_SIZE 12                  // CAUTION: must equal the number of elements
const int TimeHorizon::s_intervals[] =
  {
    5  *   60,    // sub-hour multiples [m * 60]
    10 *   60,
    15 *   60,
    20 *   60,
    30 *   60,
    1  * 3600,    // hour multiples [h * 3600]
    2  * 3600,
    3  * 3600,
    4  * 3600,
    6  * 3600,
    12 * 3600,
    24 * 3600
  };

// CREATORS

// ---------------------------------------------------------
//  MEMBER FUNCTION : TimeHorizon
// ---------------------------------------------------------

TimeHorizon::TimeHorizon
(const std::string entityId,                 // enforced unique identifier
 Record&           record) :
  Entity(entityId, record),
  d_builtinRemarkTh(record.tieSingle<std::string>("builtin-remark")),
  d_record(record),                          // for testing purposes
  d_steps(record.tieSingle<int>("steps")),
  d_interval(record.tieSingle<int>("interval")),
  d_startHour(record.tieSingle<int>("start-hour")),
  d_startDay(record.tieSingle<int>("start-day")),
  d_leapYear(record.tieSingle<bool>("leap-year")),
  d_hemisphere(record.tieSingle<std::string>("hemisphere")),
  d_enumLeapYear(xeona::e_leapNotSet),
  d_enumHemisphere(xeona::e_hemiNotSet)
{
  // initial reporting
  s_logger->repx(logga::dbug, "constructor call", "");

  // range checks
  if ( d_steps < 2 )
    {
      s_logger->repx(logga::warn, "steps not 2 or more", d_steps);
      std::ostringstream put;
      put
        << "  to run just one step, set the 'entity.time-horizon.step' field"    << "\n"
        << "  to 2 or more and then employ the command-line option \"--mode 6\"" << "\n";
      s_logger->putx(logga::dbug, put);
    }
  if ( d_startHour < 0 || d_startHour >  23 )
    {
      s_logger->repx(logga::warn, "start hour not [0,23]", d_startHour);
    }
  if ( d_startDay  < 1 || d_startDay  > 356 )
    {
      s_logger->repx(logga::warn, "start day not [1,365]", d_startDay);
    }

  // modify
  if ( d_leapYear == false )
    {
      d_enumLeapYear = xeona::e_normal;
    }
  if ( d_leapYear == true )
    {
      d_enumLeapYear = xeona::e_leap;
    }

  // modify
  if      ( d_hemisphere == "N" )
    {
      d_enumHemisphere = xeona::e_north;
    }
  else if ( d_hemisphere == "S" )
    {
      d_enumHemisphere = xeona::e_south;
    }
  else
    {
      s_logger->repx(logga::warn, "hemisphere not {N,S}", d_hemisphere);
    }

  // update the central horizon steps and interval information
  if ( Entity::s_horizonSteps != d_steps )
    {
      std::ostringstream oss;
      oss << Entity::s_horizonSteps << " : " << d_steps;
      s_logger->repx(logga::warn, "steps disagree, existing : mine", oss.str());
    }

  // note potential problems with commencement offsets and leap years
  if  ( d_startHour > 0 || d_startDay > 1 || d_leapYear == true )
    {
      std::ostringstream put;
      put << "  the following commencement offsets and leap year flag might"      << "\n"
          << "  not be properly implemented by internal timeseries providers"     << "\n"
          << "  -- the user should check the documentation provided by the"       << "\n"
          << "  relevant entity authors to confirm that this is the case"         << "\n"
          << ""                                                                   << "\n"
          << "      entity.time-horizon.start-hour : " << d_startHour             << "\n"
          << "      entity.time-horizon.start-day  : " << d_startDay              << "\n"
          << "      entity.time-horizon.leap-year  : " << d_leapYear              << "\n";
      s_logger->repx(logga::rankJumpy, "see message below", "");
      s_logger->putx(logga::rankJumpy, put);
    }

#if 1 // 1 = restrict intervals, 0 = omit test (option NOT recommended)

  // this code confirms that 'd_interval' is restricted to the
  // values given in 'TimeHorizon::s_intervals' (explicitly
  // initialized above)
  //
  // the wider question is whether to omit this test depends on which
  // entities are being run and whether they use timeseries resampling

  const bool okay = TimeHorizon::checkInterval(d_interval);
  switch ( okay )
    {
    case true:
      s_logger->repx(logga::info, "horizon interval is valid", d_interval);
      break;
    case false:
      if ( xeona::nopro == false )           // meaning option '--krazy' not applied
        {
          std::ostringstream put;
          put << "  supported values [s] : " << TimeHorizon::stringifyValidInts() << "\n";
          s_logger->repx(logga::warn, "invalid horizon interval value", d_interval);
          s_logger->putx(logga::warn, put);
#if 1 // 1 = STL exception, 0 = xeona exception (CAUTION: not caught for some reason)
          std::ostringstream oss;
          oss << "invalid horizon interval : " << d_interval << "\n";
          s_logger->repx(logga::warn, "about to throw", "std::domain_error");
          throw std::domain_error(oss.str());
#else
          s_logger->repx(logga::warn, "about to throw", "xeona::invalid_interval");
          throw xeona::invalid_interval(d_interval, TimeHorizon::stringifyValidInts());
#endif // 0
        }
      else
        {
          s_logger->repx(logga::rankJumpy, "horizon interval is invalid", d_interval);
          s_logger->repx(logga::dbug, "continuing under --krazy, okay", okay);
        }
      break;
    }

#endif // 0

  // builtin remark
  d_builtinRemarkTh = "mandatory entity";

  // transfer values
  // CAUTION: these provide copies and not references
  Entity::s_horizonSteps     = d_steps;
  Entity::s_horizonInterval  = d_interval;
  Entity::s_horizonStartHour = d_startHour;
  Entity::s_horizonStartDay  = d_startDay;
  Entity::s_leapYear         = d_enumLeapYear;
  Entity::s_hemisphere       = d_enumHemisphere;
}

// ---------------------------------------------------------
//  MEMBER FUNCTION : ~TimeHorizon
// ---------------------------------------------------------

TimeHorizon::~TimeHorizon()
{
  s_logger->repx(logga::xtra, "destructor call", "");
}

// ---------------------------------------------------------
//  MEMBER FUNCTION : factoryInitialize
// ---------------------------------------------------------

void
TimeHorizon::factoryInitialize()
{
  s_logger->repx(logga::xtra, "TimeHorizon instance initialization", "");
}

// STATIC ACCESSORS

// ---------------------------------------------------------
//  MEMBER FUNCTION : stringifyValidInts (static)
// ---------------------------------------------------------

std::string
TimeHorizon::stringifyValidInts()
{
  // CAUTION: array conversion cannot take place in the
  // constructor because there is no guarantee that the
  // constructor will have run

  // convert int array to STL vector
  std::vector<int> vec(s_intervals, s_intervals + S_INTERVALS_SIZE);

  // stringify
  const std::string separator = " ";                   // space-separated
  std::ostringstream oss;
  std::copy(vec.begin(),
            vec.end() - 1,
            std::ostream_iterator<int>(oss, separator.c_str()));
  std::copy(vec.end() - 1,
            vec.end(),
            std::ostream_iterator<int>(oss, ""));      // no trailing newline

  // return
  const std::string buffer = oss.str();
  return buffer;
}

// ---------------------------------------------------------
//  MEMBER FUNCTION : checkInterval (static)
// ---------------------------------------------------------

bool
TimeHorizon::checkInterval
(const int interval)                         // intervals length in seconds
{
  // convert int array to STL vector
  std::vector<int> vec(s_intervals, s_intervals + S_INTERVALS_SIZE);

  // search
  std::vector<int>::const_iterator pos;      // CAUTION: "const_" can cause problems
  pos = std::find(vec.begin(), vec.end(), interval);

  // interpret
  if ( pos == vec.end() ) return false;
  else                    return true;
}

// STREAM INSERTION SUPPORT

// ---------------------------------------------------------
//  MEMBER FUNCTION : streamOut
// ---------------------------------------------------------

std::ostream&
TimeHorizon::streamOut                       // support for overloaded operator<<
(std::ostream& os) const
{
  std::ios_base::fmtflags prior = os.flags();
  os << "    dummy output from a derived entity" << "\n";
  os.flags(prior);
  return os;
}

//  end of file

