//  XEONA ENERGY SYSTEMS MODELING ENVIRONMENT
//
//  file-create-name : greenhouse.h
//  file-create-date : Mon 12-Oct-2009 09:10 UTC
//  file-initiator   : Robbie Morrison <robbie@actrix.co.nz>
//
//  file-role        : global warming potential support / header
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
//  $URL: file:///home/robbie/svn-root/xeona/futz/trunk/xeona1/c/ghouse.h $
//
//  GENERAL NOTES FOR THIS FILE

//  HEADER GUARD

#ifndef _GHOUSE_H_
#define _GHOUSE_H_

//  LOCAL AND SYSTEM INCLUDES

#include "../a/logger_fwd.h"  // some forward declarations from "logger.h"
#include "../c/smart_ptr.h"   // toggle between Boost and TR1 smart pointers

#include <string>             // C++ strings
#include <sstream>            // string-streams

#include <boost/tuple/tuple.hpp>        // n-tuples, ref(), cref()

//  CODE

// ---------------------------------------------------------
//  TYPEDEF         : tupleD3
// ---------------------------------------------------------

typedef boost::tuple<double, double, double> tupleD3;

// ---------------------------------------------------------
//  CLASS           : Gwp100Bundle
// ---------------------------------------------------------
//  Description  : container for a global warming potential (GWP) gas bundle
//  Role         : to support 'ghg' cost establishment
//  Techniques   : struct, tuples
//  Status       : complete
//
//  Modeling issues
//
//      Carbon dioxide equivalent (CO2-e)
//
//          CO2-e here represents just three common greenhouse
//          gases weighted according to their 100-year global
//          warming potential (GWP):
//
//              gas                100-year GWP
//              --------------------------------
//              CO2  carbon dioxide       unity
//              CH4  methane                 25
//              N2O  nitrous oxide          298
//              --------------------------------
//              source: IPCC AR4 WG1 (Forster etal 2007)
//
//      Biofuels
//
//          In the case of biofuels, the sequestered CO2-e
//          inventory as a result of cropping can be subtracted,
//          given that the cropping regime did not necessitate
//          natural ecosystem conversion. However, any remote
//          joint-implementation greenhouse gas mitigation
//          exercise, including carbon sinking, associated with
//          particular assets should not be accounted within a
//          given domain, unless irrevocably linked. Ultimately,
//          decisions relating to CO2-e inclusion or exclusion
//          reside with the modeler.
//
//  Design notes
//
//      Class 'Gwp100Bundle' can be be used in two contexts (in a
//      manner similar to the usage of class 'CostSet'):
//
//          - the mass-specific GWP of a given gas bundle
//          - the carbon dioxide equivalent a given release
//
//  References
//
//      Forster, P, V Ramaswamy, etal.  2007.  Changes in
//        atmospheric constituents and in radiative forcing.
//        In: Climate change 2007 -- the physical science basis :
//        contribution of Working Group I to the Fourth
//        Assessment Report of the Intergovernmental Panel on
//        Climate Change.  Cambridge University Press, Cambridge,
//        United Kingdom and New York, NY, USA.
//
// ---------------------------------------------------------

class Gwp100Bundle
{
  // DISABLED

  // none

  // CREATORS

public:

  Gwp100Bundle();                            // also acts as a zero argument constructor

  Gwp100Bundle                               // up-front constructor
  (const double a_co2,                       // carbon dioxide
   const double a_ch4,                       // methane
   const double a_n2o);                      // nitrous oxide

  Gwp100Bundle                               // tuple constructor
  (const tupleD3 tup);

  ~Gwp100Bundle();                           // destructor

  // UNARY OPERATORS

  Gwp100Bundle& operator*= (const double&  other);     // common multiplier

  // ACCESSORS

  // NOTE: the data members are also public and directly accessible

  tupleD3 tuple() const;                     // export 'Gwp100Bundle' to six tuple

  double totalMass() const;
  double co2Equivalent() const;
  double gwpEffective() const;

  // STATIC ACCESSORS

  double static getGwpCo2();
  double static getGwpCh4();
  double static getGwpN2o();

  // MANIPULATORS

  void reset(const double value = 0.0);      // reset all values, note the default of zero

  void setCo2  (const double setCo2);        // equivalent to: gwp.co2 = setCo2
  void setCh4  (const double setCh4);
  void setN2o  (const double setN2o);

  // DISPLAY CALLS

  std::string
  summarizeMe
  (std::string msg = "") const;

  static
  std::string
  displayGwps();

  // INSTANCE DATA

public:                                      // CAUTION: public access is correct

  double    co2;                             // carbon dioxide
  double    ch4;                             // methane
  double    n2o;                             // nitrous oxide

  // STATIC DATA

private:

  static const double       s_gwpCo2;        // CO2 global warming potential (unity)
  static const double       s_gwpCh4;        // CH4 global warming potential
  static const double       s_gwpN2o;        // N2O global warming potential

  static logga::spLogger    s_logger;        // shared_ptr to single logger object

};

// ---------------------------------------------------------
//  FREE FUNCTION   : operator* (const double&,       const Gwp100Bundle&);
//  FREE FUNCTION   : operator* (const Gwp100Bundle&, const double&);
// ---------------------------------------------------------

const Gwp100Bundle operator* (const double&       lhs, const Gwp100Bundle& rhs);
const Gwp100Bundle operator* (const Gwp100Bundle& lhs, const double&       rhs);

// ---------------------------------------------------------
//  FREE FUNCTION   : xeona::totalMass
//  FREE FUNCTION   : xeona::co2Equivalent
//  FREE FUNCTION   : xeona::gwpEffective
// ---------------------------------------------------------

namespace xeona
{
  double
  totalMass(const double co2,
            const double ch4   = 0.0,
            const double n2o   = 0.0);

  double
  co2Equivalent(const double co2,
                const double ch4   = 0.0,
                const double n2o   = 0.0);

  double
  gwpEffective(const double co2,
               const double ch4   = 0.0,
               const double n2o   = 0.0);
} // namespace 'xeona'

#endif // _GHOUSE_H_

//  end of file

