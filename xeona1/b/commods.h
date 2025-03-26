//  XEONA ENERGY SYSTEMS MODELING ENVIRONMENT
//
//  file-create-name : commods.h
//  file-create-date : Wed 30-Jul-2008 07:36 UTC
//  file-initiator   : Robbie Morrison <robbie@actrix.co.nz>
//
//  file-role        : commodities hierarchy / header
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
//  $URL: file:///home/robbie/svn-root/xeona/futz/trunk/xeona1/b/commods.h $
//
//  GENERAL NOTES FOR THIS FILE
//
//  This header defines various concrete commodity classes,
//  prefixed "Cm", and the abstract 'Commodity' class from which
//  they derived.
//
//  This concept of a commodity is very general, as can be seen
//  by the definitions below.  Commodities not currently
//  supported include local labor (perhaps useful when
//  considering the system influence on local employment).

//  HEADER GUARD

#ifndef _COMMODS_H_
#define _COMMODS_H_

//  LOCAL AND SYSTEM INCLUDES

#include "../c/recset.h"      // record set support (also dragged in by "entity.h")
#include "../c/ghouse.h"      // global warming potential support
#include "../c/extunits.h"    // quantifying extensity enums and interpretation
#include "../b/propdata.h"    // mass-based commodity property data
#include "../b/entity.h"      // entity base class plus lazy linking

#include "../a/logger_fwd.h"  // some forward declarations from "logger.h"
#include "../c/smart_ptr.h"   // toggle between Boost and TR1 smart pointers

#include <string>             // C++ strings
#include <sstream>            // string-streams

#include <cmath>              // C-style maths, ceil(), floor(), sqrt()

//  FORWARD (PARTIAL) DECLARATIONS

class Gwp100Bundle;                          // function return

//  CODE

// ---------------------------------------------------------
//  CLASS           : Commodity (abstract)
// ---------------------------------------------------------
//  Description  : super-class for other commodities
//  Role         : provide commodity-common support
//  Techniques   : inheritance, abstract
//  Status       : complete
//
//  Design notes
//
//      Commodities can only hold intensive state information
//      (for instance, specific heating value or current
//      temperature).
//
//      The extensive state information (flows and stocks) is
//      recorded within the various 'interface' objects.
//
//      See member function 'TeasOxidToElec::establish' for the
//      dynamic cast code required to use calls to
//      sub-commodities.
//
// ---------------------------------------------------------

class Commodity :
  public FullEntity
{
  // DISABLED

private:

  Commodity();                                    // zero-argument constructor
  Commodity(const Commodity& orig);               // copy constructor
  Commodity& operator= (const Commodity& orig);   // copy assignment operator

  // CREATORS

public:

  explicit
  Commodity
  (const std::string      entityId,
   Record&                record,
   const xeona::ExtensityUnit quantifyingExtensity);

  virtual
  ~Commodity() = 0;                          // create abstract class

  // ACCESSORS

  int                                        // minus one indicates some problem
  getInterfacePairs() const;                 // one interface pair equals one connection

  virtual
  xeona::ExtensityUnit                       // see unit 'c/extunits'
  getExtensity() const = 0;                  // abstract

  virtual
  std::string
  getExtensityStr() const = 0;               // abstract

  // INTERNAL DATA

private:

  // note: no internal data thus far, but could add a weak census if warranted

protected:

  const xeona::ExtensityUnit    d_quantifyingExtensity;

};

// ---------------------------------------------------------
//  CLASS           : CommodityJoule (abstract)
// ---------------------------------------------------------
//  Description  : J-quantified commodity
//  Role         : step in the commodity inheritance web
//  Techniques   : (nothing special)
//  Status       : complete
// ---------------------------------------------------------

class CommodityJoule :
  public Commodity
{
  // DISABLED

private:

  CommodityJoule();                                         // zero-argument constructor
  CommodityJoule(const CommodityJoule& orig);               // copy constructor
  CommodityJoule& operator= (const CommodityJoule& orig);   // copy assignment operator

  // CREATORS

public:

  explicit
  CommodityJoule
  (const std::string entityId,
   Record&           record);

  virtual
  ~CommodityJoule() = 0;                     // create abstract class

  virtual
  xeona::ExtensityUnit
  getExtensity() const;

  virtual
  std::string
  getExtensityStr() const;

}; // class 'CommodityJoule'

// ---------------------------------------------------------
//  CLASS           : CommodityKilogram (abstract)
// ---------------------------------------------------------
//  Description  : kg-quantified commodity
//  Role         : step in the commodity inheritance web
//  Techniques   : (nothing special)
//  Status       : complete
// ---------------------------------------------------------

class CommodityKilogram :
  public Commodity
{
  // DISABLED

private:

  CommodityKilogram();                                           // zero-argument ctor
  CommodityKilogram(const CommodityKilogram& orig);              // copy constructor
  CommodityKilogram& operator= (const CommodityKilogram& orig);  // copy assignment opor

  // CREATORS

public:

  explicit
  CommodityKilogram
  (const std::string entityId,
   Record&           record);

  virtual
  ~CommodityKilogram() = 0;                  // create abstract class

  // ACCESSORS

  virtual
  xeona::ExtensityUnit
  getExtensity() const;

  virtual
  std::string
  getExtensityStr() const;

}; // class 'CommodityKilogram'

// ---------------------------------------------------------
//  CLASS           : CommodityUOA (abstract)
// ---------------------------------------------------------
//  Description  : UOA-quantified commodity
//  Role         : step in the commodity inheritance web
//  Techniques   : (nothing special)
//  Status       : complete
// ---------------------------------------------------------

class CommodityUOA :
  public Commodity
{
  // DISABLED

private:

  CommodityUOA();                                      // zero-argument constructor
  CommodityUOA(const CommodityUOA& orig);              // copy constructor
  CommodityUOA& operator= (const CommodityUOA& orig);  // copy assignment operator

  // CREATORS

public:

  explicit
  CommodityUOA
  (const std::string entityId,
   Record&           record);

  virtual
  ~CommodityUOA() = 0;                       // create abstract class

  // ACCESSORS

  virtual
  xeona::ExtensityUnit
  getExtensity() const;

  virtual
  std::string
  getExtensityStr() const;

}; // class 'CommodityUOA'

// ---------------------------------------------------------
//  CLASS           : CommodityMetreSq (abstract)
// ---------------------------------------------------------
//  Description  : m2-quantified commodity
//  Role         : step in the commodity inheritance web
//  Techniques   : (nothing special)
//  Status       : complete
// ---------------------------------------------------------

class CommodityMetreSq :
  public Commodity
{
  // DISABLED

private:

  CommodityMetreSq();                                         // zero-argument constructor
  CommodityMetreSq(const CommodityMetreSq& orig);             // copy constructor
  CommodityMetreSq& operator= (const CommodityMetreSq& orig); // copy assignment operator

  // CREATORS

public:

  explicit
  CommodityMetreSq
  (const std::string entityId,
   Record&           record);

  virtual
  ~CommodityMetreSq() = 0;                   // create abstract class

  // ACCESSORS

  virtual
  xeona::ExtensityUnit
  getExtensity() const;

  virtual
  std::string
  getExtensityStr() const;

}; // class 'CommodityMetre2'

// ---------------------------------------------------------
//  CLASS           : CmOxidize ("oxid")
// ---------------------------------------------------------
//  Description  : oxidizable fuels commodity
//  Role         : state invariant concrete commodity
//  Quantified   : kg
//  Status       : complete
// ---------------------------------------------------------

class CmOxidize :
  public CommodityKilogram
{
  // DISABLED

private:

  CmOxidize();                                    // zero-argument constructor
  CmOxidize(const CmOxidize& orig);               // copy constructor
  CmOxidize& operator= (const CmOxidize& orig);   // copy assignment operator

public:

  // CREATORS

  explicit
  CmOxidize
  (const std::string entityId,
   Record&           record);

  virtual
  ~CmOxidize();

  // ACCESSORS

  virtual
  double
  getSpecCombEnthalpy() const;               // used in combination with efficiency

  virtual
  double
  getSpecCarbonDioxide() const;              // for carbon capture and later sequestration

  virtual
  double
  getSpecCo2equiv() const;                   // for release of combustion products

  virtual
  Gwp100Bundle
  getSpecGhgs() const;

  // INTERNAL DATA

protected:

  double    d_specCombustionEnthalpy;        // the modeler can elect to use LHV or HHV
  double    d_specCarbonDioxide;             // assuming capture
  double    d_specCo2equiv;                  // assuming release

};

//  ==== XEDOC =================================================
//
//  entity.cm-oxidize-0
//
//      class                                    > CmOxidize
//
//        kg-quantified state-invariant oxidizable commodity
//
//      builtin-remark s                         <
//
//      spec-combustion-enthalpy [J/kg] f        > 21.6e+06
//      spec-carbon-dioxide [kg/kg] f            > 3.7
//      spec-co2-equiv [kg/kg] f                 > 3.7
//
//        the specific combustion enthalpy given here is the AR
//        (as received) HHV (higher heating value) of biocoal
//        assuming 10% moisture -- the modeler can use other
//        protocols (and substance values) but the chosen
//        protocol must be consistent with the associated
//        technical assets
//
//        spec-carbon-dioxide informs technical assets utilizing
//        carbon capture and sequestration (CCS), spec-co2-equiv
//        informs assets releasing combustion products into the
//        atmosphere
//
//  ============================================================

// ---------------------------------------------------------
//  CLASS           : CmCarbonSeq ("cseq")
// ---------------------------------------------------------
//  Description  : carbon sequestration services carrying supercritical carbon-dioxide
//  Role         : state changeable concrete commodity
//  Quantified   : kg
//  Status       : complete
// ---------------------------------------------------------

class CmCarbonSeq :
  public CommodityKilogram
{
  // DISABLED

private:

  CmCarbonSeq();                                       // zero-argument constructor
  CmCarbonSeq(const CmCarbonSeq& orig);                // copy constructor
  CmCarbonSeq& operator= (const CmCarbonSeq& orig);    // copy assignment operator

  // CREATORS

public:

  explicit
  CmCarbonSeq
  (const std::string entityId,
   Record&           record);

  virtual
  ~CmCarbonSeq();

  // ACCESSORS

  virtual
  double
  getPressure
  (const int step) const;

  // MANIPULATORS

  virtual
  void
  setPressure
  (const int    step,
   const double pressure);

  // INTERNAL DATA

protected:

  shared_ptr<std::vector<double> >    d_pressures;     // transported pressure

};

//  ==== XEDOC =================================================
//
//  entity.cm-carbon-seq-0
//
//      class                                    > CmCarbonSeq
//
//        kg-quantified pressure state-changeable carbon dioxide
//        sequestration-service commodity
//
//        demand flow is anti-parallel to physical flow, but
//        otherwise the behavior is similar to that of a
//        conventional fuel (like coal)
//
//      builtin-remark s                         <
//
//      pressures [Pa] F                         < 0.0 ..
//
//        the carriage pressure is normally supercritical, around
//        10.0e+03 kPa
//
//  ============================================================

// ---------------------------------------------------------
//  CLASS           : CmCarbonCert ("cert")
// ---------------------------------------------------------
//  Description  : financial instrument
//  Role         : state changeable concrete commodity
//  Quantified   : kg
//  Status       : complete
// ---------------------------------------------------------

class CmCarbonCert :
  public CommodityKilogram
{
  // DISABLED

private:

  CmCarbonCert();                                      // zero-argument constructor
  CmCarbonCert(const CmCarbonCert& orig);              // copy constructor
  CmCarbonCert& operator= (const CmCarbonCert& orig);  // copy assignment operator

  // CREATORS

public:

  CmCarbonCert
  (const std::string entityId,
   Record&           record);

  virtual
  ~CmCarbonCert();

  // INTERNAL DATA

protected:

  shared_ptr<std::vector<double> >    d_unitPrices;

};

//  ==== XEDOC =================================================
//
//  entity.cm-carbon-cert-0
//
//      class                                    > CmCarbonCert
//
//        kg-quantified financial instrument granting a CO2e
//        emissions entitlement
//
//      builtin-remark s                         <
//
//      unit-prices [$/kg] F                     > 14.0 ..
//
//        unit prices are currently coded as exogenous data, but
//        that could be developed
//
//  ============================================================

// ---------------------------------------------------------
//  CLASS           : CmElectricity ("elec")
// ---------------------------------------------------------
//  Description  : DC (direct current) or AC (alternating current) electricity
//  Role         : concrete commodity
//  Quantified   : J (joules)
//  Techniques   : state invariant (fully specified in the model)
//  Status       : complete
//
//  CAUTION: simplistic design
//
//      This class should have been derived to produce two
//      concrete classes 'CmElectricityDc' and 'CmElectricityAc'
//      for use by transmission entities.  Note too that the DC
//      and AC transmission models both assume "flat voltage" so
//      both would embed the same 'voltage' intensity -- with one
//      representing DC voltage and the other RMS (root mean
//      square) AC voltage.
//
//      In contrast, the transacting of electricity between
//      technical assets need not be specific on whether DC or AC
//      power is employed.
//
// ---------------------------------------------------------

class CmElectricity :
  public CommodityJoule
{
private:

  CmElectricity();                                          // zero-argument constructor
  CmElectricity(const CmElectricity& orig);                 // copy constructor
  CmElectricity& operator= (const CmElectricity& orig);     // copy assignment operator

public:

  explicit
  CmElectricity
  (const std::string entityId,
   Record&           record);

  virtual
  ~CmElectricity();

  // ACCESSORS

public:

  virtual
  double
  getVoltage() const;

  // INTERNAL DATA

protected:

  const double&    d_voltage;                // intensity

};

//  ==== XEDOC =================================================
//
//  entity.cm-electricity-0
//
//      class                                    > CmElectricity
//
//        J-quantified electricity commodity
//
//        technical assets transacting electricity either adopt
//        DC-power flow with resistive losses or AC-power via the
//        (misnamed) enhanced DC load flow model -- this
//        commodity provides support for both
//
//      builtin-remark s                         <
//
//      voltage [V] f                            > 11.0e+03
//
//        for fixed-frequency AC power, use the RMS (root-mean-
//        square) voltage (as opposed to the peak-to-peak value)
//
//  ============================================================

// ---------------------------------------------------------
//  CLASS           : CmWork ("work")
// ---------------------------------------------------------
//  Description  : classic "W"
//  Role         : concrete commodity
//  Quantified   : J (joules)
//  Techniques   : state invariant (fully specified in the model)
//  Status       : complete
// ---------------------------------------------------------

class CmWork :
  public CommodityJoule
{
private:

  CmWork();                                  // zero-argument constructor
  CmWork(const CmWork& orig);                // copy constructor
  CmWork& operator= (const CmWork& orig);    // copy assignment operator

public:

  explicit
  CmWork
  (const std::string entityId,
   Record&           record);

  virtual
  ~CmWork();

  // INTERNAL DATA

protected:

};

//  ==== XEDOC =================================================
//
//  entity.cm-work-0
//
//      class                                    > CmWork
//
//        J-quantified state-invariant classic 'W' commodity,
//        covering, among other things, shaft power
//
//      builtin-remark s                         <
//
//  ============================================================

// ---------------------------------------------------------
//  CLASS           : CmHeat ("heat")
// ---------------------------------------------------------
//  Description  : classic "Q"
//  Role         : state invariant concrete commodity
//  Quantified   : J (joules)
//  Status       : complete
// ---------------------------------------------------------

class CmHeat :
  public CommodityJoule
{
  // DISABLED

private:

  CmHeat();                                  // zero-argument constructor
  CmHeat(const CmHeat& orig);                // copy constructor
  CmHeat& operator= (const CmHeat& orig);    // copy assignment operator

  // CREATORS

public:

  explicit
  CmHeat
  (const std::string entityId,
   Record&           record);

  virtual
  ~CmHeat();

  // INTERNAL DATA

protected:

};

//  ==== XEDOC =================================================
//
//  entity.cm-heat-0
//
//      class                                    > CmHeat
//
//        J-quantified classic 'Q' commodity
//
//        currently lacking intensive state, but could
//        specialized to embed the transfer temperature
//
//      builtin-remark s                         <
//
//        see also the CmThermalFluid commodity
//
//  ============================================================

// ---------------------------------------------------------
//  CLASS           : CmThermalFluid ("thrm")
// ---------------------------------------------------------
//  Description  : recirculating thermal fluid commodity
//  Role         : state changeable concrete commodity
//  Quantified   : J (joules)
//  Status       : incomplete -- development stub for future use
//
//  Design notes
//
//      Provides a development stub for future use.  Will also
//      need thermal sub-network traversal support.
//
// ---------------------------------------------------------

class CmThermalFluid :
  public CommodityJoule
{
  // DISABLED

private:

  CmThermalFluid();                                         // zero-argument constructor
  CmThermalFluid(const CmThermalFluid& orig);               // copy constructor
  CmThermalFluid& operator= (const CmThermalFluid& orig);   // copy assignment operator

  // CREATORS

public:

  explicit
  CmThermalFluid
  (const std::string entityId,
   Record&           record);

  virtual
  ~CmThermalFluid();

  // ACCESSORS

public:

  virtual
  double
  getFloTemp
  (const int step) const;

  virtual
  double
  getRetTemp
  (const int step) const;

  // MANIPULATORS

  virtual
  void
  setFloTemp
  (const int    step,
   const double temp);

  virtual
  void
  setRetTemp
  (const int    step,
   const double temp);

  virtual
  double&                                    // directly assignable
  floTemp
  (const int step);

  virtual
  double&                                    // directly assignable
  retTemp
  (const int step);

  // INTERNAL DATA

protected:

  const double&                       d_specHeatCapacity;

  shared_ptr<std::vector<double> >    d_floTemps; // flo line, anti-parallel to demand
  shared_ptr<std::vector<double> >    d_retTemps; // ret line, parallel to demand

};

//  ==== XEDOC =================================================
//
//  entity.cm-thermal-fluid-0
//
//      class                                    > CmThermalFluid
//
//        J-quantified temperature state-changeable recirculating
//        thermal fluid commodity
//
//        provided as a development stub to enable deeco-style
//        intensive attribute setting (IAS) at some future point
//
//      builtin-remark s                         <
//
//      spec-heat-capacity [J/kgC] f             > 4181
//
//      flo-temps [C] F                          < 0.0 ..
//      ret-temps [C] F                          < 0.0 ..
//
//        'flo' and 'ret' refer to the flow and return lines of
//        the recirculating fluid, say hot water or thermal oil
//
//  ============================================================

// ---------------------------------------------------------
//  CLASS           : CmFunds ("fund") (quantified in UOA)
// ---------------------------------------------------------
//  Description  : funds -- accounted by either the cash method or the accrual method
//  Role         : state invariant concrete commodity
//  Quantified   : $ (the dollar sign is used as a generic currency sign)
//  Status       : complete
// ---------------------------------------------------------

class CmFunds :
  public CommodityUOA
{
  // DISABLED

private:

  CmFunds();                                 // zero-argument constructor
  CmFunds(const CmFunds& orig);              // copy constructor
  CmFunds& operator= (const CmFunds& orig);  // copy assignment operator

  // CREATORS

public:

  explicit
  CmFunds
  (const std::string entityId,
   Record&           record);

  virtual
  ~CmFunds();

  // INTERNAL DATA

protected:

};

//  ==== XEDOC =================================================
//
//  entity.cm-funds-0
//
//      class                                    > CmFunds
//
//        funds are quantified in UOA (units of account) and can
//        be accounted using either the cash (actual transfers)
//        or accrual methods (transfers of legal obligation)
//
//      builtin-remark s                         <
//
//        complete but not tested
//
//  ============================================================

// ---------------------------------------------------------
//  CLASS           : CmFission ("fiss")
// ---------------------------------------------------------
//  Description  : fissionable fuels commodity
//  Role         : state invariant concrete commodity
//  Quantified   : kg
//  Status       : incomplete -- lacks intensive state support
// ---------------------------------------------------------

class CmFission :
  public CommodityKilogram
{
  // DISABLED

private:

  CmFission();                                    // zero-argument constructor
  CmFission(const CmFission& orig);               // copy constructor
  CmFission& operator= (const CmFission& orig);   // copy assignment operator

  // CREATORS

public:

  explicit
  CmFission
  (const std::string entityId,
   Record&           record);

  virtual
  ~CmFission();

  // INTERNAL DATA

protected:

};

//  ==== XEDOC =================================================
//
//  entity.cm-fission-0
//
//      class                                    > CmFission
//
//        kg-quantified state-invariant fissionable material
//        commodity
//
//        caution: not currently supported by technical entities
//
//      builtin-remark s                         <
//
//  ============================================================

// ---------------------------------------------------------
//  CLASS           : CmProductiveLand ("land")
// ---------------------------------------------------------
//  Description  : productive land commodity
//  Role         : state invariant concrete commodity
//  Quantified   : m2
//  Status       : incomplete -- lacks intensive state support
// ---------------------------------------------------------

class CmProductiveLand :
  public CommodityMetreSq
{
  // DISABLED

private:

  CmProductiveLand();                                         // zero-argument constructor
  CmProductiveLand(const CmProductiveLand& orig);             // copy constructor
  CmProductiveLand& operator= (const CmProductiveLand& orig); // copy assignment operator

  // CREATORS

public:

  explicit
  CmProductiveLand
  (const std::string entityId,
   Record&           record);

  virtual
  ~CmProductiveLand();

  // INTERNAL DATA

protected:

};

//  ==== XEDOC =================================================
//
//  entity.cm-productive-land-0
//
//      class                                    > CmProductiveLand
//
//        m2-quantified state-invariant land commodity
//
//        caution: not currently supported by technical entities
//
//      builtin-remark s                         <
//
//  ============================================================

#endif // _COMMODS_H_

//  end of file

