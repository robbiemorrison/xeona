//  XEONA ENERGY SYSTEMS MODELING ENVIRONMENT
//
//  file-create-name : plank.h
//  file-create-date : Thu 06-Jan-2011 16:56 UTC
//  file-initiator   : Robbie Morrison <robbie@actrix.co.nz>
//
//  file-role        : chiller model / header
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
//  $URL: file:///home/robbie/svn-root/xeona/futz/trunk/xeona1/h/plank.h $
//
//  GENERAL NOTES FOR THIS FILE
//
//  At the time of writing (Aug-2011), the Plank equation used
//  here is being rechecked for accuracy against Plank (1940).
//  The formulation currently used was based on notes from
//  Tatjana Morozyuk -- in which the final equations were
//  inconsistent and the "most likely" to be correct version was
//  selected.
//
//  That primary reference is:
//
//      Plank, Rudolf.  1940.  Zur thermodynamischen Bewertung
//        von Kaeltemitteln.  Zeitschrift fuer die gesamte
//        Kaelte-Industrie.  v6 p81-84.  June issue.  ISSN
//        0372-879X.  [On the thermodynamic evaluation of
//        refrigerants.  Journal for the entire refrigeration
//        industry.]
//
//  This journal is held in the TU-Berlin library, Berlin,
//  Germany with the Signatur 4Z412 (look in the up-to-1949
//  shelving).

//  HEADER GUARD

#ifndef _PLANK_H_
#define _PLANK_H_

//  LOCAL AND SYSTEM INCLUDES

#include "../a/logger_fwd.h"  // some forward declarations from "logger.h"
#include "../c/smart_ptr.h"   // toggle between Boost and TR1 smart pointers

#include "../h/r134a.h"       // refrigerant R134a

#include <string>             // C++ strings
#include <sstream>            // string-streams

//  CODE

// ---------------------------------------------------------
//  CLASS           : Chiller
// ---------------------------------------------------------
//  Description  : class encapsulating a simple refrigeration model
//  Role         : intended for building simulation air chiller calculations
//  Techniques   : R-134a properties from support class 'R134a', C++ exceptions
//  Status       : complete
//
//  Design notes
//
//      The chiller uses the vapor-compression refrigeration
//      cycle and refrigerant R-134a.  A thermal expansion value
//      regulates the process and maintains a constant superheat
//      of 75C.
//
//      The underlying analytical (closed-form) model derives
//      from German refrigeration scientist Rudolf Plank and is
//      further documented in public member function
//      'Chiller::plankModel'.
//
//      The thermodynamic property data for refrigerant R-134a
//      comes from support class 'R134a'.  The values were
//      sourced from a numerical refrigerant properties
//      calculator and are interpolated when needed.
//
//  Supporting material
//
//      Background material was simultaneously written using
//      org-mode and LaTeX.  Ask Robbie Morrison for the
//      resulting PDFs.
//
//      Prof Tatjana Morozyuk <morozyuk@iet.tu-berlin.de>
//      provided details of the model in late-2010.
//
//  CAUTION: this class is NOT an entity
//
//      This class provides support to other classes, but is not
//      a 'xeona' entity in its own right.
//
// ---------------------------------------------------------

class Chiller
{
  // DISABLED

private:

  Chiller(const Chiller& orig);              // copy constructor
  Chiller& operator= (const Chiller& orig);  // copy assignment operator

  // CREATORS

public:

  Chiller(const double fakeCop = 5.0);       // 'fakeCop' is used to "keep going" on error
  ~Chiller();                                // destructor

  // ACCESSORS

public:

  double
  coeffOfPerformance                         // will return 'fakeCop' on error
  (const double celsiusEvaporator,           // evaporator (low) temperature [C]
   const double celsiusCondenser,            // condenser (high) temperature [C]
   const double performFactor = 0.7) const;  // performance factor (note default) [-]

  // UTILITY FUNCTIONS

private:

  double
  plankModel                                 // (note: SHC = specific heat capacity)
  (const double celsiusEvaporator,           // evaporator (low) temperature [C]
   const double celsiusCondenser,            // condenser (high) temperature [C]
   const double r,                           // enthalpy of vaporization at T_evap [kJ/kg]
   const double c1,                          // saturated liquid SHC at T_cond [kJ/kgK]
   const double c2,                          // saturated vapor SHC at T_cond [kJ/kgK]
   const double cs) const;                   // superheated vapor SHC (given T2) [kJ/kgK]

private:

  // INSTANCE DATA

  double   d_fakeCop;                        // used by 'coeffOfPerformance' on error
  R134a    d_refrigerant;                    // property data for refrigerant R-134a

  // STATIC DATA

protected:

  static logga::spLogger    s_logger;        // shared_ptr to single logger object

};

#endif // _PLANK_H_

//  end of file

