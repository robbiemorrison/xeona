//  XEONA ENERGY SYSTEMS MODELING ENVIRONMENT
//
//  file-create-name : thrmperf.h
//  file-create-date : Mon 25-Oct-2010 14:39 UTC
//  file-initiator   : Robbie Morrison <robbie@actrix.co.nz>
//
//  file-role        : building thermal performance and HVAC model / header
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
//  $URL: file:///home/robbie/svn-root/xeona/futz/trunk/xeona1/h/thrmperf.h $
//
//  GENERAL NOTES FOR THIS FILE

//  HEADER GUARD

#ifndef _THRMPERF_H_
#define _THRMPERF_H_

//  LOCAL AND SYSTEM INCLUDES

#include "../a/logger_fwd.h"  // some forward declarations from "logger.h"
#include "../c/smart_ptr.h"   // toggle between Boost and TR1 smart pointers

#include <sstream>            // string-streams
#include <string>             // C++ strings
#include <vector>             // STL sequence container

#include <boost/tuple/tuple.hpp>             // n-tuples, ref(), cref()

//  FORWARD (PARTIAL) DECLARATIONS

class Chiller;                               // unit 'h/plank'

//  CODE

// ---------------------------------------------------------
//  CLASS           : BuildingSim
// ---------------------------------------------------------
//  Description  : building thermal performance and HVAC calculations
//  Role         : support for building entities like 'TeasBuildingElec'
//  Techniques   : embeds a simplified building model
//  Status       : complete
//
//  Celsius symbol
//
//      Please note that degrees Celsius is abbreviated [C] in
//      this documentation -- the degree symbol (176) is not part
//      of 7-bit ASCII and is therefore avoided.  The SI
//      abbreviation "C" is officially reserved for the coulomb
//      derived unit.
//
//  Design notes
//
//      This class:
//
//      * calculates heating and cooling loads for a simplified
//        box building with glazing
//
//      * implements an operational strategy for the heating and
//        cooling equipment
//
//      * calculates chiller performance using a simplified
//        refrigeration model
//
//      * must be reset using 'zeroCallTracker' and
//        re-provisioned with data for each new calculation --
//        however unfulfilled cooling and heating loads are
//        carried over
//
//      Cooling loads are normally more complicated than heating
//      loads because they involve solar gain and, in particular,
//      heat transmission thru glazing.
//
//      The calculation of heat transfer thru the opaque
//      structure involves approximating a dynamic system by
//      steady-state and transient responses -- sometimes
//      described in the literature as the "semi-empirical"
//      method.
//
//      Any internal loads are straightforward and can be summed
//      directly.
//
//      Representative calculations are given by ASHRAE (refer
//      Fundamentals volume) in the United States and CIBSE
//      (refer Guides A and J) in the United Kingdom.
//      Calculation methods are also captured in various ISO
//      norms.
//
//      This class closely FOLLOWS the treatment given in
//      Chadderton (1998a) which, in turn, relies on CIBSE.
//      Although a little dated, Chadderton provides a crisp and
//      relatively consistent account and is more than sufficient
//      for the design needs of this class.
//
//      It would be conceptually cleaner to model the HVAC
//      equipment as a connected stand-alone entity offering or
//      accepting 'CmHeat' as the commodity -- while noting that
//      that is not done here.
//
//      HVAC is "heating, ventilating, and air conditioning".
//
//  Heat and cooling loads
//
//      The heating and cooling load calculations used here
//      comprise the following aspects:
//
//      * heat transmission thru glazing
//      * heat gains thru opaque structures: steady and cyclic
//      * infiltration gains and losses are considered
//      * internal sources of heat
//
//      The following general simplifications apply:
//
//      * a one-room model is assumed
//      * the site has an unimpeded sky view
//      * some input parameters are hard-coded
//
//      The embedded building is basic:
//
//      * a simple box with unshaded windows, distributed equally
//        by area, on each face
//
//      The default orientation is northward relative to the
//      short-axis, but this can be varied.
//
//      Those aspects which depend on building geometry and sun
//      position (including glazing) must be calculated for each
//      building element and interval.
//
//      Some input parameters are hard-coded, whereas ideally
//      they should be read-in as data or looked up using
//      suitably encoded CIBSE design tables.  These occurrences
//      are normally commented in the code.
//
//      As noted, this class assumes a simple box building.
//      Defining a realistic building geometry and undertaking
//      the necessary calculations crosses into the realm of
//      realistic building thermal performance calculations.
//      There are many examples of calculators which can
//      undertake this task, including spreadsheets, websites,
//      and fully fledged GUI applications.
//
//      To reiterate, it is not the intention of this entity to
//      replicate such software.  Instead, a simple box building
//      is depicted and many of the construction parameters are
//      hard-coded.
//
//  Operations
//
//      The cross-over between heating and cooling depend on the
//      balance temperature.
//
//  Data requirements
//
//      Each simulation requires accurate interval-specific data
//      plus 24 hours of representative data.  Under the current
//      design, this information is supplied separately.
//
//  References
//
//      Chadderton, David V.  1998a.  Air conditioning : a
//        practical introduction -- Second edition.  E and FN
//        Spon, London, United Kingdom.  ISBN-10 0-419-22610-9
//        ISBN-13 978-0419226109.
//
//      Chadderton, David V.  1998b.  Building services
//        engineering spreadsheets.  E and FN Spon, London,
//        United Kingdom.  ISBN-10 0-419-22620-6 ISBN-13
//        978-0419226208.
//
//      CIBSE
//        http://www.cibse.org/
//
//  Alternative designs
//
//      It would be a reasonable idea to make this an abstract
//      base class and then derive different building types.
//      That path was not taken initially in the interests of
//      simplicity, but could well be worth revisiting.
//
//      The current design is based on the notion of a set of
//      design calculations -- as such, it obtains, processes,
//      and discards information that could otherwise be kept and
//      reused to advantage.  An alternative strategy would be
//      make this class behave more like a moving simulation.
//
//  Third-partly libraries
//
//      For future reference, I looked for a C/C++ building
//      thermal performance API library, but could not locate one
//      (FORTRAN is more common, reflecting the age and sociology
//      of the projects).  Some useful links:
//
//        http://www.wbdg.org/resources/energyanalysis.php
//        http://apps1.eere.energy.gov/buildings/tools_directory/about.cfm
//        http://www.esru.strath.ac.uk/software.htm
//
//      The University of Strathclyde Energy Systems Research
//      Unit 'ESP-r' software looked the best and has a GPL
//      license.  For an overview:
//
//        http://www.esru.strath.ac.uk/Documents/AppB_checklist.pdf
//
//      The other alternative is to continue custom development,
//      based on spreadsheet code from Chadderton (1989b) or
//      similar.
//
//  CAUTION: this class is NOT an entity
//
//      This class provides support to building entities like
//      'TeasBuildingElec', but is not an entity in its own
//      right.
//
// ---------------------------------------------------------

class BuildingFabricData;                    // forward (partial) declaration

class BuildingSim
{
  // LOCAL ENUMERATIONS

protected:

  enum elementKind                           // used to select opaque calculations
    {                                        // CAUTION: place above typedefs
      notSpecified = 0,
      solid        = 1,                      // conduction thru solid elements
      glass        = 2                       // conduction not related to irradiation [1]
    };

  // [1] glass is also treated as an opaque building element

  // FRIENDS

  friend class BuildingFabricData;

  // TYPEDEFS

protected:

  typedef boost::tuple <
    elementKind,                             // calculation type in { solid glass }
    double,                                  // element area              [m2]
    double,                                  // element U-value           [W/m2K]
    double,                                  // element Y-value           [W/m2K]
    double,                                  // element decrement factor  [-]
    unsigned,                                // element lag               [h]
    std::string                              // comment
    > element_type;                          // used to model building elements

  // DISABLED

private:

  BuildingSim();                                       // zero-argument constructor
  BuildingSim(const BuildingSim& orig);                // copy constructor
  BuildingSim& operator= (const BuildingSim& orig);    // copy assignment operator

  // CREATORS

public:

  BuildingSim                                // constructor
  (const int interval,                       // timeseries resolution [s]
   const int hourStart = 0);                 // hour start, zero means midnight

  ~BuildingSim();                            // destructor

  // DATA SET CALLS

  unsigned                                   // return previous value
  zeroCallTracker();                         // reset 'd_callTracker'

  void
  setHvacBacklogTrips                        // for warning messages, nothing else
  (const double chillerBacklogTrip,
   const double heaterBacklogTrip);

  void
  setBuildingData
  (const double demandHiBound,               // electrical supply capacity [W]
   const double buildingOrientation,         // building orientation [degrees] [1]
   const double floorArea,                   // floor area [m2]
   const double floorAspectRatio,            // floor aspect ratio [-] (from unity)
   const double wallHeight,                  // wall height [m2]
   const double windowToWall);               // window-to-wall ratio [0,1] [-]

  // [1] from north, based on the short-axis of the oblong building

  void
  setBuildingFabric
  (shared_ptr<BuildingFabricData> fabricData);    // specialist class

  void
  setPlantData
  (const double airChangesPerHour,           // room air changes per hour [1/h]
   const double chillerCapacity,             // chiller thermal capacity [W]
   const double chillerHysteresis,           // chiller hysteresis factor [-]
   const double heaterCapacity,              // heater thermal capacity [W]
   const double heaterHysteresis,            // heater hysteresis factor [-]
   const double ductLossFactor,              // non-chiller loss factor (chiller-mode) [-]
   const double airHxLmtd,                   // air-to-air hx LMTD [degrees C]
   const bool   runStatus);                  // HVAC run status, 'true' to run plant [2]

  // [2] the 'runStatus' is often set to 'false' outside of
  // business hours to reflect that operational practice

  // the "Day" vectors below should contain one day of data at
  // current resolution, centered on zero-based element 11 for
  // hourly data and so on

  void                                                 // wrapper version
  setAmbientConditions
  (const double                                            tempAirOutside,
   boost::tuple<double, double>                            solarIrradiation,
   boost::tuple<double, double>                            sunAngle,
   shared_ptr<std::vector<double> >                        tempAirOutsideDay,
   shared_ptr<std::vector<boost::tuple<double, double> > > solarIrradiationDay,
   shared_ptr<std::vector<boost::tuple<double, double> > > sunAnglesDay);

  void
  setAmbientConditions                                 // substantive function
  (const double                     tempAirOutside,    // dry bulb outside air temp [C]
   const double                     solarDirect,       // direct component [W/m2]
   const double                     solarDifuse,       // diffuse component [W/m2]
   const double                     zenithAngle,       // zenith angle [degrees]
   const double                     azimuthAngle,      // azimuth angle [degrees]
   shared_ptr<std::vector<double> > tempAirOutsideDay, // day form of above
   shared_ptr<std::vector<double> > solarDirectDay,    // day form of above
   shared_ptr<std::vector<double> > solarDifuseDay,    // day form of above
   shared_ptr<std::vector<double> > zenithAngleDay,    // day form of above
   shared_ptr<std::vector<double> > azimuthAngleDay);  // day form of above

  void
  setBuildingOccupancyData
  (const double                     tempSetPoint,      // temperature set point [C] [1]
   shared_ptr<std::vector<double> > tempSetPointDay,
   const double                     activityLoad,      // internal thermal load [W] [2]
   const double                     electDemand);      // internal electricity demand [W]

  // [1] this analysis assumes that the set point is achieved --
  // meaning the indoor air temperature is at the set point
  //
  // [2] this is the so-called sensible heat component, which
  // excludes the enthalpy in the moisture evaporated from people
  // -- a single office worker typically contributes 90W to the
  // activity load (Chadderton 1989a p102) -- this value also
  // excludes the contribution from electrical appliances

  // CALCULATE CALL (the data setting calls must precede this call)

  void calculate();

  // UPDATE CALL

  void saveState();                          // called by host DURING washup

  // RETRIEVAL CALLS (the calculate call must precede this call)

  int    getInterval() const;                // as set at construction time

  double getChillerDuty() const;
  double getHeaterDuty() const;

  double getHvacElecDemand()  const;         // HVAC electricity demand [W]
  double getTotalElecDemand() const;         // HVAC plus internal electricity demand [W]

  double getHvacMetric()  const;             // HVAC electricity use / floor area [W/m2]
  double getTotalMetric() const;             // total electricity use / floor area [W/m2]

  // REPORTING CALL (can be used anytime, adapts to the current data state)

  void report(std::ostream& os) const;       // formatted reporting

  // UTILITY FUNCTIONS

protected:

  double calcGlazingLoad();
  double calcOpaqueLoad();
  double calcInternalLoad();

  double
  calcHvac
  (const double hvacLoadAsk,                 // HVAC load request
   const bool   runStatus);                  // is the HVAC plant available or not

  double
  simulateGlazing
  (const double windowOrientation,           // outward normal [0,360] [degrees]
   const double glassArea,                   // glass area [m2]
   const double groundReflectFactor = 0.2);  // 0.2 for temperate regions, 0.5 for tropics

  double
  simulateOpaqueElems
  (const std::vector<element_type> elements,               // vector of elements
   const double                    internalVolume,         // internal volume [m3]
   const double                    airChanges);            // air changes per hour [-]

  double                                                   // wrapper function
  calcTempSolAir                                           // sol-air temperature [C]
  (const double                 tempAirOutside,            // outside air dry-bulb [C]
   boost::tuple<double, double> solarIrradiation,          // (direct, diffuse) [W/m^2]
   boost::tuple<double, double> sunAngle,                  // (zenith, azimuth) [degrees]
   const double                 longwaveRadiation = 93.0,  // longwave re-radiation [W/m2]
   const double                 thermResistSurf   = 0.07)  // thermal resistance [m2K/W]
  const;

  double
  calcTempSolAir                                           // sol-air temperature [C]
  (const double tempAirOutside,                            // outside air dry-bulb [C]
   const double solarDirect,                               // direct irradiation [W/m^2]
   const double solarDifuse,                               // diffuse irradiation [W/m^2]
   const double zenithAngle,                               // zenith angle [degrees]
   const double longwaveRadiation = 93.0,                  // longwave re-radiation [W/m2]
   const double thermResistSurf   = 0.07) const;           // thermal resistance [m2K/W]

  // the default 'longwaveRadiation' value represents a cloudless
  // sky during times of high solar gains, the default
  // 'thermResistSurf' is taken from Chadderton (1989a p93) and
  // is listed as the external surface film thermal resistance
  // 'R_so' [m2K/W]

  // administration

  std::string                                // for reporting purposes
  interpretCallTrack                         // call tracking ensures correct call order
  (const int callTrack) const;

  // INSTANCE DATA

private:

  // administration

  unsigned               d_callTracker;           // track calls, see 'zeroCallTracker'
  std::ostringstream     d_oss;                   // report buffer

  // constants set on construction

  const int              d_interval;              // interval length [s]
  const int              d_hourStart;             // hour start [h]

  // parameters set externally

  double                 d_demandHiBound;         // electrical supply capacity [W]
  double                 d_buildingOrientation;   // based on short-axis [0,90] [degrees]
  double                 d_floorArea;             // floor area [m2]
  double                 d_floorAspectRatio;
  double                 d_wallHeight;            // wall height [m] (flat roof assumed)
  double                 d_windowToWall;          // window-to-wall ratio [-]

  double                 d_airChangesPerHour;     // interior air changes per hour [-]
  double                 d_chillerCapacity;       // chiller thermal capacity [W]
  double                 d_chillerHysteresis;     // chiller hysteresis [-]
  double                 d_heaterCapacity;        // heater thermal capacity [W]
  double                 d_heaterHysteresis;      // heater hysteresis [-]
  double                 d_ductLossFactor;        // non-chiller losses [-]
  double                 d_airHxLmtd;             // heat exchanger LMTD [degrees C]

  double                 d_tempSetPoint;          // current set point [C]
  double                 d_activityLoad;          // sensible component [W]
  double                 d_electricityDemand;     // load from appliances and such

  double                 d_tempAirOutside;        // dry-bulb air temp [C]
  double                 d_solarDirect;           // [W/m2]
  double                 d_solarDifuse;           // [W/m2]
  double                 d_zenithAngle;           // zenith angle [degrees] [1]
  double                 d_azimuthAngle;          // azimuth angle [degrees]

  double                 d_chillerCarryOverTrip;  // trip for chiller backlog warnings
  double                 d_heaterCarryOverTrip;   // trip for heater backlog warnings

  // [1] normally ranges [0,180] but some datasets (for instance,
  // NIWA TMY timeseries) truncate at 90 degrees -- hence
  // daytime conditionals must employ a strict less-than:
  // "( d_zenithAngle < 90 )"

  shared_ptr<BuildingFabricData>    d_fabricData;

  // variables set or determined internally

  double                 d_tempSetPointDayMean;   // day-average set point [C]
  double                 d_tempAirOutsideDayMean; // day-average outside air [C]
  double                 d_tempSolAir;            // current sol-air temp [C]
  double                 d_tempSolAirDayMean;     // day-average sol-air temp [C]
  std::vector<double>    d_tempSolAirSwings12;    // increasing lag, relative to mean [C]

  bool                   d_runStatus;             // 'true' means run HVAC plant
  double                 d_shortDimension;        // from floor area and aspect ratio [m]
  double                 d_chillerDuty;
  double                 d_heaterDuty;
  double                 d_hvacElecDemand;        // main result of this class [W]
  double                 d_totalElecDemand;       // HVAC demand plus direct usage [W]

  // prior state variables -- only the HVAC obligation is treated thus

  double                 d_hvacLoadCarryOverTmp;  // when HVAC cannot fulfill obligation
  double                 d_hvacLoadCarryOver;     // when HVAC cannot fulfill obligation

  // support classes

  shared_ptr<Chiller>    d_chiller;               // Plank refrigeration model

  // STATIC DATA

protected:

  static const double       s_init;     // for initialization purposes, probably a NaN
  static logga::spLogger    s_logger;   // shared_ptr to single logger object

};

#endif // _THRMPERF_H_

//  end of file

