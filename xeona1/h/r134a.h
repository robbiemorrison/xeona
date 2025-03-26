//  XEONA ENERGY SYSTEMS MODELING ENVIRONMENT
//
//  file-create-name : r134a.h
//  file-create-date : Thu 06-Jan-2011 16:14 UTC
//  file-initiator   : Robbie Morrison <robbie@actrix.co.nz>
//
//  file-role        : refrigerant R-134a / header
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
//  $URL: file:///home/robbie/svn-root/xeona/futz/trunk/xeona1/h/r134a.h $
//
//  GENERAL NOTES FOR THIS FILE

//  HEADER GUARD

#ifndef _R134A_H_
#define _R134A_H_

//  LOCAL AND SYSTEM INCLUDES

#include "../a/logger_fwd.h"  // some forward declarations from "logger.h"
#include "../c/smart_ptr.h"   // toggle between Boost and TR1 smart pointers

#include <map>                // STL associative container
#include <sstream>            // string-streams
#include <stdexcept>          // standard exception classes, runtime_error()
#include <string>             // C++ strings

//  CODE

// ---------------------------------------------------------
//  CLASS           : R134a
//  ---------------------------------------------------------
//  Description  : provide thermodynamic data for refrigerant R-134a
//  Role         : support for the Plank refrigeration model
//  Techniques   : 'std::map', Boost.Assign initialization, lookup tables, exceptions
//  Status       : complete
//
//  Design notes
//
//      This class provides the following temperature-dependent
//      thermodynamic data for standard refrigerant R-134a:
//
//        * specific enthalpy of vaporization
//        * constant-pressure specific heat capacity for the saturated liquid
//        * constant-pressure specific heat capacity for the saturated vapor
//        * constant-pressure specific heat capacity for the superheated vapor to 75C
//
//      This implementation uses lookup tables with simple linear
//      interpolation.  The underlying data was obtained from the
//      'RefCalc' program -- part of the 'CoolPack' suite.
//
//      The Plank refrigeration model requires the first property
//      at the evaporator temperature, and the remaining three
//      properties at the condenser temperature.
//
//      Hence the data is limited to predetermined temperature
//      ranges.  If a particular range if violated, a C++
//      exception is thrown.
//
//      The implementation file contains more details.
//
//  Supporting material
//
//      Background material was simultaneously writen using
//      org-mode and LaTeX.  Ask Robbie Morrison for the
//      resulting PDFs.
//
//  CAUTION: this class is NOT an entity
//
//      This class provides support to other classes, but is not
//      a 'xeona' entity in its own right.
//
//  CAUTION: unsupported temperatures
//
//      The key functions throw a 'std::domain_error' if they
//      cannot fulfill the request -- most likely because the
//      supplied temperature is not supported.  The client code
//      should, of course, handle these exceptions.
//
// ---------------------------------------------------------

class R134a
{
  // DISABLED

private:

  R134a(const R134a& orig);                  // copy constructor
  R134a& operator= (const R134a& orig);      // copy assignment operator

  // CREATORS

public:

  R134a();                                   // zero-argument constructor
  ~R134a();                                  // destructor

  // ACCESSORS

public:

  double
  hVaporization                              // specific enthalpy of vaporization [kJ/kg]
  (const double celsius) const               // supported range 5 to 15C
    throw(std::domain_error);                // exception specification

  double                                     // constant-pressure heat capacity [kJ/kgK]
  cpSatLiq                                   // for saturated liquid (bubble)
  (const double celsius) const               // supported range 25 to 50C
    throw(std::domain_error);                // exception specification

  double                                     // constant-pressure heat capacity [kJ/kgK]
  cpSatVapor                                 // for saturated vapor (dew)
  (const double celsius) const               // supported range 25 to 50C
    throw(std::domain_error);                // exception specification

  double                                     // constant-pressure heat capacity [kJ/kgK]
  cpSuper75                                  // for superheated vapor at 75C
  (const double celsius) const               // supported range 25 to 50C
    throw(std::domain_error);                // exception specification

  // UTILITY FUNCTIONS

private:

  double
  obtainValue
  (const std::map<int,double>& series,            // one-dimensional lookup table
   const double                lookup) const;     // non-integral "key"

  double
  lookupValue                                     // helper for 'obtainValue'
  (const std::map<int,double>& series,            // one-dimensional lookup table
   const int                   lookup) const;     // integral key

  // INTERNAL DATA

private:

  static const std::map<int,double>    d_h_vapor;
  static const std::map<int,double>    d_cp_satliq;
  static const std::map<int,double>    d_cp_satvap;
  static const std::map<int,double>    d_cp_super75;

protected:

  static logga::spLogger    s_logger;        // shared_ptr to single logger object

};

#endif // _R134A_H_

//  end of file

