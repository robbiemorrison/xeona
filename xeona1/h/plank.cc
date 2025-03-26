//  XEONA ENERGY SYSTEMS MODELING ENVIRONMENT
//
//  file-create-name : plank.cc
//  file-create-date : Thu 06-Jan-2011 16:56 UTC
//  file-initiator   : Robbie Morrison <robbie@actrix.co.nz>
//
//  file-role        : chiller model / implementation
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
//  $URL: file:///home/robbie/svn-root/xeona/futz/trunk/xeona1/h/plank.cc $
//
//  GENERAL NOTES FOR THIS FILE

//  LOCAL AND SYSTEM INCLUDES

#include "plank.h"            // companion header for this file (place first)

#include "../a/logger.h"      // standard logging functionality (as required)
#include "../c/smart_ptr.h"   // toggle between Boost and TR1 smart pointers
#include ".././common.h"      // common definitions for project (place last)

#include <iomanip>            // setw() and family
#include <sstream>            // string-streams
#include <stdexcept>          // standard exception classes, runtime_error()
#include <string>             // C++ strings

#include <cmath>              // C-style maths, abs(), ceil(), floor(), sqrt()

//  FILE-LOCAL GLOBAL VARIABLES

namespace
{
  const double abz = 273.15;                 // zero Celsius in kelvin
}

//  CODE

// ---------------------------------------------------------
//  CLASS           : Chiller
// ---------------------------------------------------------

// STATIC DEFINITIONS - with and without explicit initialization

logga::spLogger Chiller::s_logger = logga::ptrLogStream();

// CREATORS

// ---------------------------------------------------------
//  MEMBER FUNCTION : Chiller::Chiller
// ---------------------------------------------------------

Chiller::Chiller
(const double fakeCop) :                     // note default argument
  d_fakeCop(fakeCop),
  d_refrigerant()
{
  s_logger->repx(logga::adhc, "constructor call, fakeCop", d_fakeCop);
}

// ---------------------------------------------------------
//  MEMBER FUNCTION : Chiller::~Chiller
// ---------------------------------------------------------

Chiller::~Chiller()
{
  s_logger->repx(logga::adhc, "destructor call", "");
}

// ACCESSORS

// ---------------------------------------------------------
//  MEMBER FUNCTION : Chiller::coeffOfPerformance
// ---------------------------------------------------------
//  Description  : coordination for R-134a properties, the Plank model, performance factor
//  Role         : public function
//  Techniques   : class 'R134a', private function 'plankModel'
//  Status       : complete
//
//  Design notes
//
//      This function simply checks and collects data before
//      calling the analytical model implemented in function
//      'Chiller::plankModel' See the documentation for that
//      function regarding the model itself.
//
//      See the documentation for class 'R134a' regarding the
//      thermodynamic properties calculations for R-134a.
//
//      The performance factor 'performFactor' represents a
//      catchall for unmodeled engineering losses.  It normally
//      ranges [0.7,0.8].
//
//      In this implementation, the 'cs' value assumes a constant
//      superheat of 75C.  The thermal expansion (TX) valve is
//      designed to maintain this temperature by varying the mass
//      flow of the refrigerant.  The degree of superheat for
//      R-134a system typically ranges [70,75].
//
//  CAUTION: treatment of invalid temperature values
//
//      Invalid temperature values are naturally detected by
//      class 'R134a', which then throws a 'std::domain_error'
//      exception.  This object also containing some details
//      regarding the cause.
//
// ---------------------------------------------------------

double
Chiller::coeffOfPerformance
(const double celsiusEvaporator,             // lower temperature
 const double celsiusCondenser,              // higher temperature
 const double performFactor) const           // note default value
{
  // initial reporting
  s_logger->repx(logga::adhc, "entering member function", "");

  // ---------------------------------
  //  preamble
  // ---------------------------------

  // for reporting
  std::ostringstream put;

  // integrity checks (errant temperatures are dealt with by class 'R134a')
  if ( performFactor < 0.0 || performFactor > 1.0 )
    {
      s_logger->repx(logga::warn, "aphysical performance factor", performFactor);
    }
  else if ( performFactor < 0.6 || performFactor > 0.9 )
    {
      s_logger->repx(logga::dbug, "questionable performance factor", performFactor);
    }

  if ( celsiusEvaporator > celsiusCondenser )     // [1]
    {
      std::ostringstream oss;
      oss << celsiusEvaporator << " : " << celsiusCondenser;
      s_logger->repx(logga::warn, "evaporator hotter then condenser", oss.str());
    }

  // [1] as at commit r5676, the two ranges do not overlap and
  // hence failing this conditional must also lead to look-up
  // problems later on

  // ---------------------------------
  //  main calculations
  // ---------------------------------

  try                                        // the refrigerant properties calls can throw
    {
      // STEP 1 : calculate required refrigerant properties
      //
      // r  : specific enthalpy of vaporization [kJ/kg]
      // c1 : constant-pressure heat capacity for saturated liquid (bubble) [kJ/kgK]
      // c2 : constant-pressure heat capacity for saturated vapor (dew) [kJ/kgK]
      // cs : constant-pressure heat capacity for superheated vapor at 75C [kJ/kgK]

      const double r  = d_refrigerant.hVaporization(celsiusEvaporator);
      const double c1 = d_refrigerant.cpSatLiq(celsiusCondenser);
      const double c2 = d_refrigerant.cpSatVapor(celsiusCondenser);
      const double cs = d_refrigerant.cpSuper75(celsiusCondenser);

      // report as required

      put << std::showpos;
      put << "  Plank refrigeration model"                                  << "\n"
          << "    refrigerant data:"                                        << "\n"
          << "      evaporator temperature [C]  : " << celsiusEvaporator    << "\n"
          << "      r   [kJ/kg]                 : " << r                    << "\n"
          << "      condenser temperature [C]   : " << celsiusCondenser     << "\n"
          << "      c'  [kJ/kgK]                : " << c1                   << "\n"
          << "      c'' [kJ/kgK]                : " << c2                   << "\n"
          << "      c_s [kJ/kgK] (75 superheat) : " << cs                   << "\n"
          << "      performance factor [-]      : " << performFactor        << "\n";
      s_logger->putx(logga::adhc, put);

      // STEP 2 : invoke the Plank model

      const double copFluid = plankModel(celsiusEvaporator,
                                         celsiusCondenser,
                                         r,
                                         c1,
                                         c2,
                                         cs);

      // STEP 3 : apply the selected machine performance factor

      const double copMachine = performFactor * copFluid;

      // report as required

      put << std::showpos;
      put << "    intermediates:"                                           << "\n"
          << "      CoP_fluid [-]               : " << copFluid             << "\n"
          << "    output:"                                                  << "\n"
          << "      CoP_machine [-]             : " << copMachine           << "\n";
      s_logger->putx(logga::adhc, put);

      // FINALLY : return on success

      s_logger->repx(logga::adhc, "leaving member function, final CoP", copMachine);
      return copMachine;
    }
  catch( const std::domain_error& e )        // invalid temperature value
    {
      std::ostringstream put;
      put << "  caught std::domain_error: " << e.what() << "\n";
      s_logger->repx(logga::warn, "exception caught, fake cop return", d_fakeCop);
      s_logger->putx(logga::warn, put);
      return d_fakeCop;
    }
}

// UTILITY FUNCTIONS

// ---------------------------------------------------------
//  MEMBER FUNCTION : Chiller::plankModel
// ---------------------------------------------------------
//  Description  : implements the Plank refrigeration model
//  Role         : support for client function 'coeffOfPerformance'
//  Techniques   : arithmetic
//  Status       : complete
//
//  Design notes
//
//      The model itself derives from German refrigeration
//      scientist Rudolf Plank.
//
//      This code is based on equation 5.3 as given in notes from
//      Tatjana in late-2010.
//
//      The equation accepts qualified SI units as long as they
//      are used consistently.  For instance, the common
//      deployment of "kJ" is fine.
//
// ---------------------------------------------------------

double
Chiller::plankModel
(const double celsiusEvaporator,
 const double celsiusCondenser,
 const double r,
 const double c1,
 const double c2,
 const double cs) const
{
  // convert to absolute temperature
  const double Tevap = celsiusEvaporator + ::abz;
  const double Tcond = celsiusCondenser  + ::abz;

  // create some useful sub-expressions
  const double Tdelta = Tcond - Tevap;     // temperature difference
  const double Tdelt2 = Tdelta * Tdelta;   // above squared
  const double Tevap2 = Tevap * Tevap;

  // split main equation by denominator and numerator
  const double den
    = r
    - c1 * Tdelta
    - c1 / (2.0 * Tevap) * Tdelta;
  const double num
    = (r - c1 * Tdelta) * (Tdelta / Tevap)
    + (c1 / Tevap) * (Tdelt2 / 2.0)
    + ((c2 * c2) / cs) * (Tcond / 2.0) * Tdelt2 / Tevap2;
  const double cop = den / num;

  // report
  std::ostringstream put;
  put << std::showpos;
  put << "    calculations:"                                 << "\n"
      << "      den                         : " << den       << "\n"
      << "      num                         : " << num       << "\n"
      << "      cop                         : " << cop       << "\n";
  s_logger->putx(logga::adhc, put);

  // return
  return cop;
}

//  end of file

