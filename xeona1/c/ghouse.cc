//  XEONA ENERGY SYSTEMS MODELING ENVIRONMENT
//
//  file-create-name : greenhouse.cc
//  file-create-date : Mon 12-Oct-2009 09:10 UTC
//  file-initiator   : Robbie Morrison <robbie@actrix.co.nz>
//
//  file-role        : global warming potential support / implementation
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
//  $URL: file:///home/robbie/svn-root/xeona/futz/trunk/xeona1/c/ghouse.cc $
//
//  GENERAL NOTES FOR THIS FILE

//  LOCAL AND SYSTEM INCLUDES

#include "ghouse.h"           // companion header for this file (place first)

#include "../a/logger.h"      // standard logging functionality (as required)
#include "../c/smart_ptr.h"   // toggle between Boost and TR1 smart pointers
#include ".././common.h"      // common definitions for project (place last)

#include <string>             // C++ strings
#include <sstream>            // string-streams

#include <boost/format.hpp>   // printf style formatting

//  CODE

// ---------------------------------------------------------
//  CLASS           : Gwp100Bundle
// ---------------------------------------------------------
//
//  Abbreviations
//
//      co2      carbon dioxide
//      ch4      methane
//      n2o      nitrous oxide
//
//  100 year GWP (global warming potential)
//
//      Data from wikipedia page at
//      'http://en.wikipedia.org/wiki/Global_warming_potential'
//      quoting 2007 IPCC AR4 p212 (more specifically the 2007
//      IPCC Fourth Assessment Report by Working Group 1 and
//      Chapter 2 of that report "Changes in Atmospheric
//      Constituents and in Radiative Forcing" which contains
//      relevant GWP information).
//
// ---------------------------------------------------------

// STATIC DEFINITIONS

const double Gwp100Bundle::s_gwpCo2    =   1.0;   // necessarily unity
const double Gwp100Bundle::s_gwpCh4    =  25.0;
const double Gwp100Bundle::s_gwpN2o    = 298.0;

logga::spLogger Gwp100Bundle::s_logger = logga::ptrLogStream();

// CREATORS

Gwp100Bundle::Gwp100Bundle() :
  co2(0.0),
  ch4(0.0),
  n2o(0.0)
{ }

Gwp100Bundle::Gwp100Bundle                   // up-front constructor
(const double a_co2,
 const double a_ch4,
 const double a_n2o) :
  co2(a_co2),
  ch4(a_ch4),
  n2o(a_n2o)
{ }

Gwp100Bundle::Gwp100Bundle                   // tuple constructor
(const tupleD3 tup) :
  co2(tup.get<0>()),
  ch4(tup.get<1>()),
  n2o(tup.get<2>())
{ }

Gwp100Bundle::~Gwp100Bundle() { }            // destructor

// UNARY OPERATORS

Gwp100Bundle&
Gwp100Bundle::operator*=
(const double& other)
{
  co2 *= other;
  ch4 *= other;
  n2o *= other;
  return *this;
}

// ACCESSORS

tupleD3
Gwp100Bundle::tuple() const                  // export 'Gwp100Bundle' to six tuple
{
  return boost::make_tuple(co2, ch4, n2o);
}

double
Gwp100Bundle::totalMass() const
{
  const double mass
    = co2
    + ch4
    + n2o;
  return mass;
}

double
Gwp100Bundle::co2Equivalent() const
{
  const double co2e
    = s_gwpCo2   * co2
    + s_gwpCh4   * ch4
    + s_gwpN2o   * n2o;
  return co2e;
}

double
Gwp100Bundle::gwpEffective() const
{
  return co2Equivalent() / totalMass();
}

// STATIC ACCESSORS

double Gwp100Bundle::getGwpCo2() { return s_gwpCo2; }
double Gwp100Bundle::getGwpCh4() { return s_gwpCh4; }
double Gwp100Bundle::getGwpN2o() { return s_gwpN2o; }

// MANIPULATORS

void
Gwp100Bundle::reset
(const double value)                         // note the default (of zero)
{
  // reporting
  if ( xeona::releaseStatus == true )
    {
      s_logger->repx(logga::warn, "call intended for development only", "");
    }
  if ( value == 0.0 )
    {
      s_logger->repx(logga::adhc, "reset value", value);
    }
  else
    {
      s_logger->repx(logga::warn, "non-zero reset value", value);
    }

  // active code
  co2   = value;
  ch4   = value;
  n2o   = value;
}

void Gwp100Bundle::setCo2  (const double a_co2) { co2 = a_co2; }
void Gwp100Bundle::setCh4  (const double a_ch4) { ch4 = a_ch4; }
void Gwp100Bundle::setN2o  (const double a_n2o) { n2o = a_n2o; }

// DISPLAY CALLS

std::string
Gwp100Bundle::summarizeMe
(std::string msg) const
{
#if 1 // 0 = ignore code, 1 = enact code
  if ( msg.empty() ) msg = "(no message)";
#endif // 0

  std::ostringstream oss;
  oss << "  GWP gas bundle:" << "\n";
  oss << boost::format("    bundle [kg] : CO2 = %.3g") % co2
      << boost::format("    CH4 = %.3g")               % ch4
      << boost::format("    N2O = %.3g")               % n2o
      << "\n";
  if ( ! msg.empty() )
    {
      oss <<           "    message (options)   : " << msg << "\n";
    }
  oss << boost::format("    total mass [kg]     : %10.6g\n") % totalMass()
      << boost::format("    CO2 equivalent [kg] : %10.6g\n") % co2Equivalent()
      << boost::format("    effective GWP [-]   : %10.6g\n") % gwpEffective();
  return oss.str();
}

std::string
Gwp100Bundle::displayGwps()                     // static
{
  const std::string msg = "hardcoded IPCC 100-year global warming potentials [-]";
  std::ostringstream oss;
  oss << boost::format("  %s:\n")                                              % msg
      << boost::format("    CO2   (carbon dioxide) (unity)  : %10.6g\n") % s_gwpCo2
      << boost::format("    CH4   (methane)                 : %10.6g\n") % s_gwpCh4
      << boost::format("    N2O   (nitrous oxide)           : %10.6g\n") % s_gwpN2o;
  return oss.str();
}

// ---------------------------------------------------------
//  FREE FUNCTION   : operator* (const double&,       const Gwp100Bundle&);
//  FREE FUNCTION   : operator* (const Gwp100Bundle&, const double&);
// ---------------------------------------------------------

const Gwp100Bundle
operator*
(const double&       lhs,
 const Gwp100Bundle& rhs)
{
  Gwp100Bundle tmp;
  tmp.co2   = lhs * rhs.co2;
  tmp.ch4   = lhs * rhs.ch4;
  tmp.n2o   = lhs * rhs.n2o;
  return tmp;
}

const Gwp100Bundle
operator*
(const Gwp100Bundle& lhs,
 const double&       rhs)
{
  return operator*(rhs, lhs);
}

// ---------------------------------------------------------
//  FREE FUNCTION   : xeona::totalMass
// ---------------------------------------------------------

namespace xeona
{
  double
  totalMass(const double co2,
            const double ch4,
            const double n2o)
  {
    Gwp100Bundle gwp(co2, ch4, n2o);
    const double mass = gwp.totalMass();
    return mass;
  }
} // namespace 'xeona'

// ---------------------------------------------------------
//  FREE FUNCTION   : xeona::co2Equivalent
// ---------------------------------------------------------

namespace xeona
{
  double
  co2Equivalent(const double co2,
                const double ch4,
                const double n2o)
  {
    Gwp100Bundle gwp(co2, ch4, n2o);
    const double co2e = gwp.co2Equivalent();
    return co2e;
  }
} // namespace 'xeona'

// ---------------------------------------------------------
//  FREE FUNCTION   : xeona::gwpEffective
// ---------------------------------------------------------

namespace xeona
{
  double
  gwpEffective(const double co2,
               const double ch4,
               const double n2o)
  {
    Gwp100Bundle gwp(co2, ch4, n2o);
    const double gwpe = gwp.gwpEffective();
    return gwpe;
  }
} // namespace 'xeona'

//  end of file

