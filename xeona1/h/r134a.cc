//  XEONA ENERGY SYSTEMS MODELING ENVIRONMENT
//
//  file-create-name : r134a.cc
//  file-create-date : Thu 06-Jan-2011 16:15 UTC
//  file-initiator   : Robbie Morrison <robbie@actrix.co.nz>
//
//  file-role        : refrigerant R-134a / implementation
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
//  $URL: file:///home/robbie/svn-root/xeona/futz/trunk/xeona1/h/r134a.cc $
//
//  GENERAL NOTES FOR THIS FILE

//  LOCAL AND SYSTEM INCLUDES

#include "r134a.h"            // companion header for this file (place first)

#include "../a/logger.h"      // standard logging functionality (as required)
#include "../c/smart_ptr.h"   // toggle between Boost and TR1 smart pointers
#include ".././common.h"      // common definitions for project (place last)

#include <string>             // C++ strings
#include <sstream>            // string-streams

#include <cmath>              // C-style maths, abs(), ceil(), floor(), sqrt()

#include <boost/assign/list_of.hpp>               // assign list_of() map_list_of(), etc
#include <boost/math/special_functions/modf.hpp>  // integer and fractional split, modf()
#include <boost/math/special_functions/sign.hpp>  // sign manipulation, sign() etc

//  CODE

// ---------------------------------------------------------
//  CLASS           : R134a
// ---------------------------------------------------------
//
//  Data source
//
//      The data for the lookup tables was generated using the
//      Technical University of Denmark 'RefCalc' refrigerant
//      property calculator.
//
//      'RefCalc' 2.03 is the refrigerant properties calculator
//      that ships as part of 'CoolPack' 1.46.  The software is
//      distributed as a set of 32-bit Microsoft Windows
//      applications which will also run on the Linux Wine
//      compatibility layer.  The current release dates from 18
//      May 2001.
//
//      See Jakobsen etal (2001) and Skovrup (2001) for further
//      details.
//
//  Use of 'Boost.Assign' library
//
//      The 'Boost.Assign' library 'boost::assign::list_of'
//      syntax is a little odd, but the more normal overloaded
//      'operator+=' cannot be used to initialize variables --
//      for details, see the section titled "Function list_of()"
//      in the Boost library documentation.
//
//      Note that the upcoming C++11 standard, scheduled for late
//      2011, will support generalized initializer lists and
//      thereby make the present use of Boost.Assign obsolete.
//
//  References
//
//      Jakobsen, Arne, Bjarne Dindler Rasmussen, Morten Juel
//        Skovrup, and Simon Engedal Andersen.  2001.  CoolPack
//        tutorial -- Version 1.46.  Department of Energy
//        Engineering, Technical University of Denmark (DTU),
//        Denmark.
//
//      Skovrup, Morten Juel.  2001.  Thermodynamic and
//        thermophysical properties of refrigerants -- Technical
//        report.  Department of Energy Engineering, Technical
//        University of Denmark, Denmark.
//
// ---------------------------------------------------------

// STATIC DEFINITIONS - with and without explicit initialization

logga::spLogger R134a::s_logger = logga::ptrLogStream();

const std::map<int,double> R134a::d_h_vapor = boost::assign::map_list_of
  ( 5, 193.4)
  ( 6, 192.6)
  ( 7, 191.8)
  ( 8, 191.0)
  ( 9, 190.3)
  (10, 189.5)
  (11, 188.6)
  (12, 187.8)
  (13, 187.0)
  (14, 186.2)
  (15, 185.3);

const std::map<int,double> R134a::d_cp_satliq = boost::assign::map_list_of
  (25, 1.422)
  (26, 1.427)
  (27, 1.431)
  (28, 1.436)
  (29, 1.440)
  (30, 1.445)
  (31, 1.450)
  (32, 1.454)
  (33, 1.459)
  (34, 1.464)
  (35, 1.469)
  (36, 1.474)
  (37, 1.480)
  (38, 1.485)
  (39, 1.490)
  (40, 1.496)
  (41, 1.501)
  (42, 1.507)
  (43, 1.513)
  (44, 1.519)
  (45, 1.525)
  (46, 1.531)
  (47, 1.538)
  (48, 1.544)
  (49, 1.551)
  (50, 1.558);

const std::map<int,double> R134a::d_cp_satvap = boost::assign::map_list_of
  (25, 1.065)
  (26, 1.072)
  (27, 1.079)
  (28, 1.086)
  (29, 1.094)
  (30, 1.101)
  (31, 1.109)
  (32, 1.116)
  (33, 1.124)
  (34, 1.132)
  (35, 1.141)
  (36, 1.149)
  (37, 1.157)
  (38, 1.166)
  (39, 1.175)
  (40, 1.184)
  (41, 1.193)
  (42, 1.203)
  (43, 1.212)
  (44, 1.222)
  (45, 1.233)
  (46, 1.243)
  (47, 1.254)
  (48, 1.265)
  (49, 1.276)
  (50, 1.288);

const std::map<int,double> R134a::d_cp_super75 = boost::assign::map_list_of
  (25, 1.029)
  (26, 1.032)
  (27, 1.035)
  (28, 1.038)
  (29, 1.042)
  (30, 1.045)
  (31, 1.049)
  (32, 1.053)
  (33, 1.057)
  (34, 1.061)
  (35, 1.065)
  (36, 1.070)
  (37, 1.074)
  (38, 1.079)
  (39, 1.084)
  (40, 1.089)
  (41, 1.095)
  (42, 1.100)
  (43, 1.106)
  (44, 1.112)
  (45, 1.119)
  (46, 1.126)
  (47, 1.133)
  (48, 1.140)
  (49, 1.148)
  (50, 1.156);

// CREATORS

// ---------------------------------------------------------
//  MEMBER FUNCTION : R134a::R134a
// ---------------------------------------------------------

R134a::R134a()
{
  s_logger->repx(logga::adhc, "constructor call", "");
  std::ostringstream put;
  put << "  refrigerant R-134a one-dimensional lookup table lengths"  << "\n"
      << "    h  vaporization       : " << d_h_vapor.size()           << "\n"
      << "    Cp saturated liquid   : " << d_cp_satliq.size()         << "\n"
      << "    Cp saturated vapor    : " << d_cp_satvap.size()         << "\n"
      << "    Cp superheated to 75C : " << d_cp_super75.size()        << "\n";
  s_logger->putx(logga::adhc, put);
}

// ---------------------------------------------------------
//  MEMBER FUNCTION : R134a::~R134a
// ---------------------------------------------------------

R134a::~R134a()
{
  s_logger->repx(logga::adhc, "destructor call", "");
}

// ACCESSORS

// ---------------------------------------------------------
//  MEMBER FUNCTION : R134a::hVaporization
// ---------------------------------------------------------

double
R134a::hVaporization                         // specific enthalpy of vaporization [kJ/kg]
(const double celsius) const
  throw(std::domain_error)                   // exception specification
{
  try
    {
      return obtainValue(d_h_vapor, celsius);
    }
  catch( const int& e )                      // 'e' being the faulty lookup key
    {
      s_logger->repx(logga::dbug, "about to throw", "std::domain_error");
      std::ostringstream oss;
      oss << "lookup function '" << __func__ << "'"
          << " cannot process '" << e << "' from argument '" << celsius << "'";
      throw std::domain_error(oss.str());
    }
}

// ---------------------------------------------------------
//  MEMBER FUNCTION : R134a::cpSatLiq
// ---------------------------------------------------------

double                                       // constant-pressure heat capacity [kJ/kgK]
R134a::cpSatLiq                              // for saturated liquid (bubble)
(const double celsius) const
  throw(std::domain_error)                   // exception specification
{
  try
    {
      return obtainValue(d_cp_satliq, celsius);
    }
  catch( const int& e )                      // 'e' being the faulty lookup key
    {
      s_logger->repx(logga::dbug, "about to throw", "std::domain_error");
      std::ostringstream oss;
      oss << "lookup function '" << __func__ << "'"
          << " cannot process '" << e << "' from argument '" << celsius << "'";
      throw std::domain_error(oss.str());
    }
}

// ---------------------------------------------------------
//  MEMBER FUNCTION : R134a::cpSatVapor
// ---------------------------------------------------------

double                                       // constant-pressure heat capacity [kJ/kgK]
R134a::cpSatVapor                            // for saturated vapor (dew)
(const double celsius) const
  throw(std::domain_error)                   // exception specification
{
  try
    {
      return obtainValue(d_cp_satvap, celsius);
    }
  catch( const int& e )                      // 'e' being the faulty lookup key
    {
      s_logger->repx(logga::dbug, "about to throw", "std::domain_error");
      std::ostringstream oss;
      oss << "lookup function '" << __func__ << "'"
          << " cannot process '" << e << "' from argument '" << celsius << "'";
      throw std::domain_error(oss.str());
    }
}

// ---------------------------------------------------------
//  MEMBER FUNCTION : R134a::cpSuper75
// ---------------------------------------------------------

double                                       // constant-pressure heat capacity [kJ/kgK]
R134a::cpSuper75                             // for superheated vapor at 75C [kJ/kgK]
(const double celsius) const
  throw(std::domain_error)                   // exception specification
{
  try
    {
      return obtainValue(d_cp_super75, celsius);
    }
  catch( const int& e )                      // 'e' being the faulty lookup key
    {
      s_logger->repx(logga::dbug, "about to throw", "std::domain_error");
      std::ostringstream oss;
      oss << "lookup function '" << __func__ << "'"
          << " cannot process '" << e << "' from argument '" << celsius << "'";
      throw std::domain_error(oss.str());
    }
}

// ---------------------------------------------------------
//  MEMBER FUNCTION : R134a::obtainValue
// ---------------------------------------------------------
//  Description  : return value for given lookup using linear interpolation
//  Role         : property values
//  Techniques   : linear interpolation, 'boost::math::modf', 'boost::math::sign'
//  Status       : complete
//
//  Design notes
//
//      This function looks up a one-dimensional lookup table on
//      the basis that the lookup keys are integer-resolved.  The
//      function also applies simple linear interpolation where
//      needed.  It also provides the correct behavior if
//      negative keys are used.
//
//      The helper function 'R134a::lookupValue' is deployed for
//      the actual lookup and it is this function that detects
//      out-of-range keys.
//
//      Note that if 'lookup' is strictly integral, then the
//      'nextValue' lookup call is identical to the 'baseValue'
//      lookup call.
//
//  CAUTION: out-of-range lookup
//
//      This function rethrows a simple 'int' exception if the
//      supplied 'lookup' "key" is not supported.
//
// ---------------------------------------------------------

double
R134a::obtainValue
(const std::map<int,double>& series,         // one-dimensional lookup table
 const double                lookup) const   // non-integral "key"
{
  // split 'lookup' into integer and fractional parts, each of
  // which retains the original sign
  int intBuffer;                             // used because 'intLookup' should be 'const'
  const double fracLookup = boost::math::modf(lookup, &intBuffer);    // fractional part
  const int    intLookup  = intBuffer;                                // integer part

  // function 'boost::math::sign' returns int 1 if x > 0,
  // int -1 if x < 0, and int 0 if x is zero

  // look up the values
  try
    {
      const double baseValue = lookupValue(series, intLookup);
const double nextValue = lookupValue(series,
                                           intLookup + boost::math::sign(fracLookup));

      // undertake interpolation -- the 'abs' function is required
      // for negative 'lookup' keys
      const double value = baseValue + std::abs(fracLookup) * (nextValue - baseValue);

      // return interpolated value
      return value;
    }
  catch( const int& e )
    {
      throw e;                               // rethrow to the public caller
    }
}

// ---------------------------------------------------------
//  MEMBER FUNCTION : R134a::lookupValue
// ---------------------------------------------------------
//  Description  : return value for given key in a one-dimensional lookup table
//  Role         : support for member function 'obtainValue'
//  Techniques   : 'std::map', 'std::map::find'
//  Status       : complete
//
//  Design notes
//
//      Note that 'std::map' containers are required to be
//      implemented as binary trees and therefore offer fast
//      lookup.  This is certainly not an issue when only a few
//      tens of elements are present -- as is the case here.
//
//  CAUTION: out-of-range keys
//
//      This function throws a simple 'int' exception if the
//      supplied 'lookup' key cannot be found.
//
// ---------------------------------------------------------

double
R134a::lookupValue
(const std::map<int,double>& series,         // 'const' requires 'const_iterator'
 const int                   lookup) const   // integral key
{
  std::map<int, double>::const_iterator pos;
  pos = series.find(lookup);
  if ( pos == series.end() )
    {
      throw lookup;                          // 'lookup' is the faulty key
    }
  else
    {
      return pos->second;                    // the desired value
    }
}

//  end of file

