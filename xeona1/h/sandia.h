//  XEONA ENERGY SYSTEMS MODELING ENVIRONMENT
//
//  file-create-name : sandia.h
//  file-create-date : Mon 10-Jan-2011 12:15 UTC
//  file-initiator   : Robbie Morrison <robbie@actrix.co.nz>
//
//  file-role        : Sandia photovoltaic array model / header
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
//  $URL: file:///home/robbie/svn-root/xeona/futz/trunk/xeona1/h/sandia.h $
//
//  GENERAL NOTES FOR THIS FILE
//
//  Separate usage
//
//      This unit has been written with a view to it being
//      extracted from the 'xeona' codebase and used separately.
//      In which case, don't forget the associated data file.
//
//  Model equations and data formats
//
//      This unit contains the Sandia Photovoltaic Array
//      Performance Model.
//
//      The implementation uses the array performance model
//      formulation, the associated array characterization data
//      format, and the array characterization dataset, current
//      at 2010.  Be aware that changes to both the model and the
//      data format occurred sometime between King (1997) and
//      King etal (2004) (see below for a list of references).
//      Relative to the old data format, the new format offers
//      different semantics for model parameters 'C0' and 'C1'
//      and also omits the 'Picture' and 'Description' fields.
//
//      Be aware that two of the model equations repeated in De
//      Soto (2006) are incorrect.  Chris Cameron
//      <cpcamer@sandia.gov>, Sandia National Laboratories,
//      confirmed this in an email to Robbie Morrison on
//      21-Jan-2011.  Details are given in the implementation
//      file.
//
//      It is quite possible that the model equations, data
//      format, and/or array characterizations will change again,
//      after a model review being undertaken in 2011 is
//      complete.  Improved documentation is also planned as part
//      of that exercise.
//
//  Development environment
//
//      This unit was written using GCC 4.4.3 on Linux.  The code
//      also makes extensive use of various Boost C++ libraries
//      and was developed using version 1.45.0.
//
//  Based on Siemens Solar SM55
//
//      This unit was tested using the Siemens Solar SM55
//      photovoltaic module.  No other array models are currently
//      supported by this code.
//
//      Please read file 'g/pvdata.h' for details on the primary
//      data source.
//
//      To deploy other modules, refer to the implementation file
//      for details on how to adjust Sandia CSV (comma-separated
//      values) row information for use in this code.  The CSV
//      row needs to be converted into a valid C++ string.
//
//      It would be a straightforward matter to store the entire
//      Sandia database in an associative array and offer users a
//      choice -- but that is not currently done.
//
//      In 2010, TUV Rheinland, Germany (the 'U' is umlauted) was
//      awarded a contract to update the Sandia database.  It may
//      be worth tracking down this new data in due course.
//
//  Data validation helper class
//
//      This unit uses data validation code provided by the
//      helper class 'DataValidation'.  Although there should be
//      little reason to do so, this class and its associated
//      calls can be removed without affecting the workings of
//      the core model in class 'PvModule'.
//
//  Console logging
//
//      This unit also uses the 'xeona' logging calls 'repx' and
//      'putx'.  These calls are fully passive and can be deleted
//      without problem if you do not wish to link to the 'xeona'
//      unit responsible for logging.

//  HEADER GUARD

#ifndef _SANDIA_H_
#define _SANDIA_H_

//  LOCAL AND SYSTEM INCLUDES

#include "../a/logger_fwd.h"  // some forward declarations from "logger.h"
#include "../c/smart_ptr.h"   // toggle between Boost and TR1 smart pointers

#include <ostream>            // output streams
#include <string>             // C++ strings
#include <vector>             // STL sequence container

//  CODE

// CAUTION: class 'DataValidation' must precede class 'PvModule'

// ---------------------------------------------------------
//  CLASS           : DataValidation
// ---------------------------------------------------------
//  Description  : data validation helper class
//  Role         : support for class 'PvModule'
//  Techniques   : nested class, enum covering severity
//  Status       : complete
//  Note         : quite nicely coded
//
//  Design notes
//
//      This class is provided to reduce the amount of data
//      validation code present in the client class and thereby
//      improve the readability of that class.
//
//      Each non-validation event is encapsulated in a nested
//      class 'Event' instance and duly stored in a 'std::vector'
//      for subsequent retrieval and reporting.
//
//      The test calls are 'bounds', 'summation', 'nostring', and
//      'comment'.  Further calls can be added as required.
//
//      IEEE 754 'inf's can be used to set open bounds (that is,
//      in the absence of more specialized functions).
//
// ---------------------------------------------------------

class DataValidation
{
  // LOCAL ENUMERATIONS

public:

  enum Severity                              // severity of the violation
    {
      e_notSpecified = 0,
      e_info         = 1,
      e_warn         = 2,
      e_error        = 3
    };

  // NESTED CLASSES

private:

  class Event
  {
  private:

    Event();                                 // zero-argument constructor

  public:

    Event
    (const unsigned     count,
     const Severity     severity,
     const std::string& log,
     const std::string& message);

  public:

    unsigned       count;                    // event counter
    Severity       severity;                 // info thru error
    std::string    log;                      // function-specific log
    std::string    message;                  // call-specific message (optional)
  };

  // DISABLED

private:

  DataValidation(const DataValidation& orig);               // copy constructor
  DataValidation& operator= (const DataValidation& orig);   // copy assignment operator

  // CREATORS

public:

  DataValidation();                          // zero-argument constructor
  ~DataValidation();                         // destructor

  // ACCESSORS

  unsigned
  getEventCount() const;

  unsigned
  printEvents
  (std::ostream& os) const;                  // leaves 'os' in its prior state

  // MANIPULATORS

  unsigned                                   // previous event count
  clearEvents();                             // clears all data

  template <typename T>
  bool                                       // 'true' if within bounds
  bounds
  (const T            value,                 // quantity being tested
   const T            lowerBound,
   const T            upperBound,
   const Severity     severity,              // see nested enum
   const std::string& msg = "");             // optional message

  template <typename T>
  bool                                       // 'true' if within tolerance
  sumation
  (const T            one,                   // first quantity
   const T            two,                   // second quantity
   const T            sum,                   // expected sum
   const T            tolerance,             // required +/- tolerance on sum
   const Severity     severity,              // see nested enum
   const std::string& msg = "");             // optional message

  bool                                       // 'true' if 'string' not empty
  nostring
  (const std::string& string,                // string to test
   const Severity     severity,              // see nested enum
   const std::string& msg = "");             // optional message

  bool                                       // always 'false'
  caught
  (const std::exception& exception,          // caught STL (base or derived) exception
   const Severity        severity,           // nested enum
   const std::string&    msg = "");          // optional message

  bool                                       // always 'false'
  comment
  (const Severity     severity,              // see nested enum
   const std::string& msg = "");             // optional message

  // UTILITY FUNCTIONS

private:

  std::string
  interpretSeverity                          // converts enum into string
  (const Severity severity) const;           // nested enum

private:

  // INSTANCE DATA

  unsigned                    d_eventCount;
  std::vector<Event>          d_events;

};

// ---------------------------------------------------------
//  CLASS           : PvModule
// ---------------------------------------------------------
//  Description  : stand-alone photovoltaic module simulation
//  Role         : support photovoltaic installation entities
//  Techniques   : Sandia Photovoltaic Array Performance Model, current 2010
//  Status       : complete
//
//  Design notes
//
//      This class models a single photovoltaic module.  It
//      assumes that the voltage can be set to maximize output,
//      yet does not account for the performance of the
//      associated wiring and power electronics.
//
//      In terms of nomenclature: photovoltaic modules (aka
//      panels) are built from individual solar cells,
//      photovoltaic arrays are collections of modules, and an
//      installation comprises one or more array plus wiring,
//      electronics, and storage.  An installation may also
//      include a grid-tie connection, perimeter fencing, and
//      such.
//
//      It is left to the client entity to capture the remainder
//      of the installation and to cost the entire facility.  It
//      is also the responsibility of client entity to supply
//      suitable weather data.
//
//      This class determines and reports the maximum power
//      available from the module given that its voltage can
//      float.  Some installations may be run sub-optimally,
//      because, for instance, their operating voltage is
//      determined by the battery bank and not by the module
//      controller.
//
//      The algebraic model and datasets used in this class come
//      from the Sandia Photovoltaic Array Performance Model,
//      current as of 2010.
//
//      The data variables used here are named in accordance with
//      King etal (2004) and King (1997) in the first instance.
//      Except that 'Tm' is now 'Zm', thereby freeing "T" for
//      temperature.  Plus some minor changes to respect C++
//      identifier requirements.  For non-Sandia quantities, the
//      usual 'xeona' "d_" and "s_" naming convention is
//      deployed.
//
//      This code returns all external ostreams to their original
//      state by virtue of the 'Boost.Io_state' library.
//
//  Model source
//
//      This code is based on King, Boyson, and Kratochvil
//      (2004).  Some ancillary equations are from King (1997),
//      but note carefully that, for King (1997):
//
//        * parameters 'C0' and 'C1' have DIFFERENT semantics
//
//      De Soto etal (2006), although presenting a different
//      model, also list the core Sandia model equations as an
//      appendix.  Note carefully that:
//
//        * equations A.5 and A.6 are INCORRECT
//
//      The references given below were, in early-2011, all
//      readily available, without charge, from the internet.
//
//  Spurious results zeroed
//
//      This model may give spurious negative or not-a-number
//      (NaN) results.  This may simply be because the model
//      equations (closed-form expressions), as given in the
//      various publications, are not expressed with bounds on
//      their legitimate use.  For instance, solar zenith angles
//      of 89 degrees may cause problems.  The treatment here is
//      to summarily convert spurious module output results to
//      zero in such circumstances.
//
//  Sandia data in string form
//
//      An individual Sandia CSV (comma-separated values) data
//      row, describing some particular PV module, will require
//      minor formatting changes before it can be added to the
//      client code.  The issue relates to the fact that the
//      entire row must now constitute a legitimate C++ string,
//      something not required by the CSV specification.  Refer
//      to the public member function 'PvModule::slurp' for
//      details on how to convert a primary Sandia data row to a
//      parsable C++ string.
//
//  CAUTION: radians used exclusively
//
//      All angles in this class are stored and manipulated using
//      radians, but are, nonetheless, exclusively entered in
//      degrees.  Angles are reported in either degrees or
//      radians as indicated by the given units.
//
//      This practice may be contrary to that given in the
//      various King publications -- so be careful!
//
//      In particular, some formulas expect raw degrees!
//
//      Note also that azimuth angles may be defined on either
//      range (nor is consistency required):
//
//            * [-180,+180]
//            * [ 0,  +360]
//
//  CAUTION: this class is NOT an entity
//
//      This class provides support to photovoltaic installation
//      entities, but is not a 'xeona' entity in its own right.
//
//  References
//
//      De Soto, W, SA Klein, and WA Beckman.  2006.  Improvement
//        and validation of a model for photovoltaic array
//        performance Solar Energy.  v80 no1 pp78-88.  ISSN
//        0038-092X.  doi:10.1016/j.solener.2005.06.010.
//
//      King, David L.  2010.  Sandia PV array performance
//        model - Presentation.  Sandia PV Modeling Workshop, 22
//        September 2010.  David King, DK Solar Works,
//        Albuquerque, USA.
//
//      King, David L, William E Boyson, and Jay A Kratochvil.
//        2004.  Photovoltaic array performance model -- Sandia
//        report SAND2004-3535.  Sandia Corporation, USA.*
//
//        * the page numbers quoted here assume that the title
//          and abstract page is THREE -- this aligns with the
//          various PDF versions of this paper, some of which do
//          not display typeset page numbers
//
//      King, David L.  1997.  Photovoltaic module and array
//        performance characterization methods for all system
//        operating conditions.  Proceeding of NREL/SNL
//        Photovoltaics Program Review Meeting, November
//        18-22,1996, Lakewood, CO, USA.  AIP Press (publisher),
//        New York, USA.
//
// ---------------------------------------------------------

class PvModule
{
  // TYPEDEFS

private:

  typedef DataValidation dv;                 // more convenient data validation calls

  // DISABLED

private:

  PvModule();                                     // zero-argument constructor
  PvModule(const PvModule& orig);                 // copy constructor
  PvModule& operator= (const PvModule& orig);     // copy assignment operator

  // CREATORS

public:

  PvModule
  (const std::string& identifier);           // useful identifier, no default provided

  ~PvModule();                               // destructor

  // MANIPULATORS

  bool                                       // 'true' means success
  slurp                                      // read PV module data
  (const std::string& csv);                  // Sandia CSV row as string

  void
  setInstallation
  (const double moduleTiltAngle,             // module tilt, horizontal is zero [degrees]
   const double moduleAzimuthAngle,          // module azimuth angle [degrees]
   const double siteAltitude);               // site altitude [m]

  void
  setWeather
  (const double directSolar,                 // direct component [W/m2]
   const double difuseSolar,                 // diffuse component [W/m2]
   const double solarZenithAngle,            // solar zenith angle [degrees]
   const double solarAzimuthAngle,           // solar azimuth angle [degrees]
   const double airTemp,                     // air temperature [C]
   const double windSpeed);                  // wind speed at 10m [m/s]

  bool
  calculate                                  // 'true' if no data issues
  (double&      production,                  // loss adjusted production from PV module
   const double systemLossFactor = 1.0);     // adjust for wiring and inverter losses

  unsigned                                   // return previous value
  reset();                                   // reset 'd_callTracker' and data

  // ACCESSORS

  std::string
  getIdentifier() const;                     // return object identifier

  std::string
  getModel() const;                          // return the module identifier if set

  void
  printReport                                // display known data to date
  (std::ostream& os) const;                  // leaves 'os' in its prior state

  bool                                       // 'false' means data issues
  isNoDataIssues() const;                    // the 'calculate' return is also similar

  void
  printDataIssues
  (std::ostream& os) const;                  // leaves 'os' in its prior state

  // UTILITY FUNCTIONS

private:

  void
  calcSandia();                              // run the Sandia model

private:

  // administration

  const std::string    d_identifier;         // object identifier, set on construction
  bool                 d_loaded;             // 'true' if CSV string successfully slurped
  unsigned             d_callTracker;        // track calls, see 'zeroCallTracker'
  DataValidation       d_valid;              // data validation helper class

  // original Sandia model CSV headers
  //
  //   Model Vintage Area Material Series_Cells Parallel_C-S Isco
  //   Voco Impo Vmpo aIsc aImp C0 C1 BVoco mBVoc BVmpo mBVmp n
  //   C2 C3 A0 A1 A2 A3 A4 B0 B1 B2 B3 B4 B5 d(Tc) fd a b C4 C5
  //   Ixo Ixxo C6 C7
  //
  // C++ identifier modifications
  //
  //   Parallel_C-S now 'Parallel_C_S'
  //   d(Tc) now 'dTc'
  //
  // the examples relate to the Siemens SM55 module
  // "c-Si" for (single crystal?) crystalline silicon

  std::string     Model;           // module model identifier
  std::string     Vintage;         // year manufactured (E = manufactures data used)
  double          Area;            // module area [m2]
  std::string     Material;        // solar cell material

  int             Series_Cells;    // Ns, no of cells in series in each cell-string [-]
  int             Parallel_C_S;    // Np, no of cell-strings in parallel [-]
  double          Isco;            // short-circuit reference current [A]
  double          Voco;            // open-circuit reference voltage [V]
  double          Impo;            // maximum-power reference current [A]
  double          Vmpo;            // maximum-power reference voltage [V]
  double          aIsc;            // (a = alpha) normalized temp coeff for Isc [1/C]
  double          aImp;            // (a = alpha) normalized temp coeff for Imp [1/C]
  double          C0;              // coefficient relating Imp to irradiance Ee [-]
  double          C1;              // coefficient relating Imp to irradiance Ee [-]
  double          BVoco;           // (B = beta) temperature coefficient for Voc [V/C]
  double          mBVoc;           // (B = beta) irradiance dependence for Voc [V/C]
  double          BVmpo;           // (B = beta) temperature coefficient for Vmp [V/C]
  double          mBVmp;           // (B = beta) irradiance dependence for Vmp [V/C]
  double          n;               // diode factor [-]
  double          C2;              // coefficient relating Vmp to irradiance Ee [-]
  double          C3;              // coefficient relating Vmp to irradiance Ee [1/V]
  double          A0;              // f1 spectral influence on Isc polynomial coeff [-]
  double          A1;              // f1 spectral influence on Isc polynomial coeff [-]
  double          A2;              // f1 spectral influence on Isc polynomial coeff [-]
  double          A3;              // f1 spectral influence on Isc polynomial coeff [-]
  double          A4;              // f1 spectral influence on Isc polynomial coeff [-]
  double          B0;              // f2 optical influence on AOI polynomial coeff [-]
  double          B1;              // f2 optical influence on AOI polynomial coeff [-]
  double          B2;              // f2 optical influence on AOI polynomial coeff [-]
  double          B3;              // f2 optical influence on AOI polynomial coeff [-]
  double          B4;              // f2 optical influence on AOI polynomial coeff [-]
  double          B5;              // f2 optical influence on AOI polynomial coeff [-]
  double          dTc;             // d(Tc), cell and module back temperature delta [C]
  double          fd;              // diffuse fraction used by module [-]
  double          a;               // still sunny weather temperature limit [-]
  double          b;               // wind cooling dependency [s/m]
  double          C4;              // coefficient relating Ix to Ee [-]
  double          C5;              // coefficient relating Ix to Ee [-]
  double          Ixo;             // related to I-V datapoint at V = Voc/2 [A]
  double          Ixxo;            // related to I-V datapoint at V = (Voc+Vmp)/2 [A]
  double          C6;              // coefficient relating Ixx to Ee [-]
  double          C7;              // coefficient relating Ixx to Ee [-]

  // model constants

  const double    T0;              // reference cell temperature [C]
  const double    E0;              // reference solar irradiation [W/m2]

  // inputs which define the operating conditions

  double          Zm;              // module zenith (tilt) angle [radians] [1]
  double          AZm;             // module azimuth angle [radians]
  double          h;               // site altitude [m]

  double          Zs;              // solar zenith angle [radians]
  double          AZs;             // solar azimuth angle [radians]
  double          Edni;            // direct normal irradiance [W/m2]
  double          Ediff;           // diffuse irradiance [W/m2]
  double          Ta;              // ambient temperature [C]
  double          WS;              // ambient wind speed at 10m [m/s]

  // [1] 'Zm' was 'Tm' in King (1997), see below for new 'Tm' meaning

  // calculated intermediate and final values

  std::string     status;          // night, day, sun behind module

  double          AM;              // air mass [-]
  double          AMa;             // absolute air mass [-]
  double          AOI;             // solar angle-of-incidence on the array [radians]
  double          E;               // total incident on module surface [W/m2]
  double          Eb;              // beam component incident on module surface [W/m2]
  double          Ee;              // "effective" solar irradiance [-]
  double          PP0;             // ratio of P to P0 = 101.325kPa [-]
  double          Tc;              // cell temperature [C]
  double          Tm;              // back-surface module temperature [C]
  double          deltaTc;         // "thermal voltage" per cell [V]

  double          f1;              // intermediate f1(AMa) empirical function value [-]
  double          f2;              // intermediate f2(AOI) empirical function value [-]

  double          Isc;             // short-circuit current [A]
  double          Imp;             // max-power current [A]
  double          Voc;             // open-circuit voltage [V]
  double          Vmp;             // max-power voltage [V]

  double          Pmp;             // max-power power [W]
  double          FF;              // fill factor [-]

  double          d_output;        // zero for nonsensical Pmp, else Pmp [W]
  double          d_pd;            // power density (array area) [W/m2]
  double          d_effy;          // efficiency (tilt-based) [-]

  // STATIC DATA

protected:

  static const double       s_init;     // for initialization purposes, either zero or NaN
  static logga::spLogger    s_logger;   // shared_ptr to single logger object

};

#endif // _SANDIA_H_

//  end of file

