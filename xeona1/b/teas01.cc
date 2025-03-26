//  XEONA ENERGY SYSTEMS MODELING ENVIRONMENT
//
//  file-create-name : teas1.cc
//  file-create-date : Wed 15-Apr-2009 21:03 UTC
//  file-initiator   : Robbie Morrison <robbie@actrix.co.nz>
//
//  file-role        : concrete technical assets 1 / implementation
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
//  $URL: file:///home/robbie/svn-root/xeona/futz/trunk/xeona1/b/teas01.cc $
//
//  GENERAL NOTES FOR THIS FILE

//  LOCAL AND SYSTEM INCLUDES

#include "teas01.h"           // companion header for this file (place first)

#include "../c/util4.h"       // free functions for trigonometry and maths
#include "../c/recset.h"      // records and fields and also record-sets
#include "../c/label.h"       // helper class to format solver labels
#include "../c/conex.h"       // create and connect block interfaces
#include "../b/optops2.h"     // operate optimization sub-problems for technical assets 2
#include "../b/optops1.h"     // operate optimization sub-problems for technical assets 1
#include "../b/commods.h"     // commodities hierarchy

#include "../a/logger.h"      // standard logging functionality (as required)
#include "../c/smart_ptr.h"   // toggle between Boost and TR1 smart pointers
#include ".././common.h"      // common definitions for project (place last)

#include <string>             // C++ strings
#include <sstream>            // string-streams

#include <cmath>              // C-style maths, abs(), ceil(), floor(), sqrt()

//  CODE

// ---------------------------------------------------------
//  FREE FUNCTION   : ::loadFlow
// ---------------------------------------------------------
//  Description  : calculate transmission and lineloss after delta-theta is known
//  Role         : reporting support
//  Techniques   : (nothing special)
//  Status       : complete
// ---------------------------------------------------------

namespace
{
  boost::tuple
  <double,                                   // transmission [W]
   double>                                   // lineloss [W]
  loadFlow(const double voltage,             // V [V]
           const double resistance,          // R [ohm]
           const double reactance,           // X [ohm]
           const double thetaDegreesDelta)   // theta [deg]
  {
    // conductance and susceptance
    const double denom         = resistance * resistance + reactance * reactance;
    const double susceptance   = -reactance  / denom;  // B [S]
    const double conductance   = +resistance / denom;  // G [S]
    const double V2B           = voltage * voltage * susceptance;
    const double V2G           = voltage * voltage * conductance;
    // load flow calculations
    const double thetaRadDelta = xeona::degree2radian(thetaDegreesDelta);
    const double transmission  = +V2B * thetaRadDelta;
    const double lineloss      = +V2G * thetaRadDelta * thetaRadDelta;
    // return
    return boost::make_tuple(transmission, lineloss);

  } // function '::loadFlow'
}

// ---------------------------------------------------------
//  CLASS           : TeasAcTransmission
// ---------------------------------------------------------

// ---------------------------------------------------------
//  MEMBER FUNCTION : TeasAcTransmission
// ---------------------------------------------------------

TeasAcTransmission::TeasAcTransmission
(const std::string entityId,
 Record&           record) :
  CostRegister(record),
  TechnicalAsset(entityId, record, xeona::e_commitmentModes),
  CostRegisterSRFin(record),
  CostRegisterEmbFin(record),
  d_capacity(record.tieSingle<double>("capacity")),
  d_resistancePerMetre(record.tieSingle<double>("resistance-per-metre")),
  d_reactancePerMetre(record.tieSingle<double>("reactance-per-metre")),
  d_length(record.tieSingle<double>("length")),
  d_voltageAngleDeltaUpper(record.tieSingle<double>("voltage-angle-delta-upper")),
  d_discretizationSteps(record.tieSingle<int>("discretization-steps")),
  d_gridCommodity(record.tieSingle<std::string>("grid-commodity")),
  d_directions(record.tieTimeseries<bool>("directions")),
  d_injections(record.tieTimeseries<double>("injections")),
  d_exits(record.tieTimeseries<double>("exits")),
  d_relativeDutys(record.tieTimeseries<double>("relative-dutys")),
  d_relativeLosss(record.tieTimeseries<double>("relative-losss")),
  d_voltageAngleDeltas(record.tieTimeseries<double>("voltage-angle-deltas")),
  d_capacitateCount(record.tieSingle<int>("capacitate-count")),
  d_cable(Cable<CmElectricity>::create
          (entityId,                         // me
           record.tieSingle<std::string>("socket-1"),
           d_gridCommodity)),                // common value
  d_socket(Socket<CmElectricity>::create
           (entityId,                        // me
            "grid-1",                        // hard-coded socket label
            d_gridCommodity)),               // common value
  d_voltage(0.0),
  d_ops()                                    // empty pointer
{
  s_logger->repx(logga::xtra, "constructor call", getIdAndKind());
  d_builtinRemark = "beta";

  d_capacitateCount = 0;
}

// ---------------------------------------------------------
//  MEMBER FUNCTION : ~TeasAcTransmission
// ---------------------------------------------------------

TeasAcTransmission::~TeasAcTransmission()
{
  s_logger->repx(logga::adhc, "destructor call", getIdAndKind());
}

// ---------------------------------------------------------
//  MEMBER FUNCTION : establish
// ---------------------------------------------------------
//  Description  : sets some initial values
//  Role         : beginning-of-horizon call
//  Techniques   : 'dynamic_pointer_cast'
//  Status       : complete
// ---------------------------------------------------------

void
TeasAcTransmission::establish()
{
  // initial reporting
  s_logger->repx(logga::adhc, "entering member function", "TeasAcTransmission");

  // obtain electricity commodity and its voltage
  const shared_ptr<Commodity>     com  = d_cable->getCm();  // 'd_socket' would also do
  const shared_ptr<CmElectricity> elec = dynamic_pointer_cast<CmElectricity>(com);
  if ( ! elec )
    {
      s_logger->repx(logga::warn, "dynamic pointer cast failed, target", "CmElectricity");
    }
  else
    {
      d_voltage = elec->getVoltage();
      s_logger->repx(logga::adhc, "voltage (from cable)", d_voltage);
    }

  // report on unusual voltages
  const double threshold = 11.0e03;
  if ( d_voltage <= threshold )
    {
      s_logger->repx(logga::rankJumpy, "HV voltage seems low", d_voltage);
    }

} // function 'TeasAcTransmission::establish'

// ---------------------------------------------------------
//  MEMBER FUNCTION : constrain
// ---------------------------------------------------------
//  Description  : technical asset constrain call
//  Role         : characterize current engineering and short-run costs
//  Techniques   : (nothing special)
//  Status       : complete
// ---------------------------------------------------------

const int                                    // return zero
TeasAcTransmission::constrain
(const xeona::DomainMode capacityMode)
{
  // additional initial reporting as appropriate
  // YEEK 30 CODE (set by '--yeek')
  if ( xeona::yeek == 30 || xeona::yeek == 1 || xeona::yeek == 2 )
    {
      const std::string subtype = xeona::demangle(typeid(*this).name()); // CAUTION: deref
      s_logger->repx(logga::adhc, "entering member function", subtype);
    }
  else
    {
      s_logger->repx(logga::adhc, "entering member function", "TeasAcTransmission");
    }

  // create and fill a label object
  const std::string teasId = getIdentifier();
  Label lab(teasId);

  // recreate new operations OSP of the required type
  d_ops.reset(new OpsLoadFlow(d_solver, d_commitmentMode));
  d_ops->loadOspLabel(lab.str());

  // define some global cols for internal use
  int cabGol       = -1;
  int cabThetaGol  = -1;
  int socGol       = -1;
  int socThetaGol  = -1;
  int directionGol = -1;                     // not used in this function

  // upload the engineering -- in this case, the directionality
  // is defined by my cable-to-socket arrangement
  boost::tie(cabGol,                         // cable flow   (single, bidirectional)
             cabThetaGol,                    // cable theta  (voltage angle)
             socGol,                         // socket flow  (single, bidirectional)
             socThetaGol,                    // socket theta (voltage angle)
             directionGol)                   // flow direction
    = d_ops->uploadEngineering(d_capacity,
                               d_voltage,
                               d_resistancePerMetre,
                               d_reactancePerMetre,
                               d_length,
                               d_voltageAngleDeltaUpper,
                               d_discretizationSteps);

  // upload specific costs -- including increment the "shift"
  // term note also the 50/50 split on costs -- see r4150 and
  // back for previous call chain
  d_ops->uploadShortrunCosts(d_dutySpecCosts * 0.5, cabGol);
  d_ops->uploadShortrunCosts(d_dutySpecCosts * 0.5, socGol);
  d_ops->uploadShortrunCosts(d_sizeSpecCosts * d_nameplateSize + d_standingCosts);

  // bind global cols to the relevant interfaces
  d_cable ->bindOsp(d_solver, cabGol, cabThetaGol);
  d_socket->bindOsp(d_solver, socGol, socThetaGol);

  // store duty values
  d_ceilingDuty = +d_capacity;         // automatically decoupled from 'd_capacity'
  d_floorDuty   = -d_capacity;

  // additional reporting as appropriate
  // YEEK 13 CODE (set by '--yeek')
  if ( xeona::yeek == 13 || xeona::yeek == 1 || xeona::yeek == 2 )
    {
      const double resistance = d_resistancePerMetre * d_length;
      const double reactance  = d_reactancePerMetre  * d_length;

      std::ostringstream put;
      put << "  AC transmission engineering (note the use of prefixed units)"    << "\n"
          << std::fixed << std::setprecision(2)
          << "    line capacity       [MW] : " << ( d_capacity / 1.0e+06 )       << "\n"
          << "    voltage [kV]             : " << ( d_voltage  / 1.0e+03 )       << "\n"
          << "    resistance         [ohm] : " <<   resistance                   << "\n"
          << "    reactance          [ohm] : " <<   reactance                    << "\n"
          << "    length              [km] : " << ( d_length   / 1.0e+03 )       << "\n"
          << "    discretization steps [-] : " << d_discretizationSteps          << "\n"
          << std::fixed << std::setprecision(2)
          << std::showpos
          << "    ceiling duty (unscaled)  : " << d_ceilingDuty                  << "\n"
          << "    floor   duty (unscaled)  : " << d_floorDuty                    << "\n";
      s_logger->repx(logga::dbug, "additional reporting follows, yeek", xeona::yeek);
      s_logger->putx(logga::dbug, put);
    }

  // set duty to zero as not needed in this case, also capture return value
  const int dutyGol = 0;                     // zero here means no need to couple

  // return zero
  return dutyGol;

} // function 'TeasAcTransmission::constrain'

// ---------------------------------------------------------
//  MEMBER FUNCTION : washup
// ---------------------------------------------------------

void
TeasAcTransmission::washup()
{
  // initial reporting
  s_logger->repx(logga::dbug, "entering member function, step", d_step);

  // results recovery
  bool   directionFlag     = false;
  double flowCable         =  0.0;           // negative values are valid
  double flowSocket        =  0.0;           // negative values are valid
  double relativeDuty      = -1.0;           // nonsensical value
  double relativeLoss      = -1.0;           // nonsensical value
  double thetaDegreesDelta = -1.0;           // nonsensical value

  boost::tie(directionFlag,                  // flow direction, 'true' if cable to socket
             flowCable,                      // cable flow [W], can be negative
             flowSocket,                     // socket flow [W]
             relativeDuty,                   // duty relative to capacity [-]
             relativeLoss,                   // loss relative to injection [-]
             thetaDegreesDelta)              // voltage angle shift [degrees]
    = d_ops->downloadSolution();

  // store entity state information
  d_directions->at(d_step) = directionFlag;
  if ( directionFlag == true )               // flow orientation from cable to socket
    {
      d_injections->at(d_step)         = +flowCable;
      d_exits     ->at(d_step)         = +flowSocket;
      d_voltageAngleDeltas->at(d_step) = +thetaDegreesDelta;
    }
  else                                       // opposite flow orientation encountered
    {
      d_injections->at(d_step)         = -flowSocket;
      d_exits     ->at(d_step)         = -flowCable;
      d_voltageAngleDeltas->at(d_step) = -thetaDegreesDelta;
    }
  d_relativeDutys->at(d_step) = relativeDuty;
  d_relativeLosss->at(d_step) = relativeLoss;

  bool atCapacity   = false;                 // only for reporting
  const double duty = std::max(std::abs(flowCable), std::abs(flowSocket));
  if ( xeona::almostEqual(duty, d_capacity, xeona::numic) ) // third argument is threshold
    {
      ++d_capacitateCount;
      atCapacity = true;
    }

  // additional reporting as appropriate
  // YEEK 13 CODE (set by '--yeek')
  if ( xeona::yeek == 13 || xeona::yeek == 1 || xeona::yeek == 2 )
    {
      std::ostringstream put;

      // part one -- standard reporting
      put << "  AC transmission solution"                                << "\n"
          << "    identifier         : " << getIdAndKind()               << "\n"
          << "    direction {0,1}    : " << directionFlag                << "\n"
          << std::fixed << std::setprecision(0)
          << std::showpos
          << "    cable flow     [W] : " << flowCable                    << "\n"
          << "    socket flow    [W] : " << flowSocket                   << "\n"
          << std::fixed << std::setprecision(5)
          << std::noshowpos
          << "    relative duty  [%] : " << (relativeDuty * 100)         << "\n"
          << "    relative loss  [%] : " << (relativeLoss * 100)         << "\n"
          << "    at capacity    [-] : " << atCapacity                   << "\n"
          << std::fixed << std::setprecision(0)
          << std::showpos
          << "    injection      [W] : " << d_injections->at(d_step)     << "\n"
          << "    exit           [W] : " << d_exits->at(d_step)          << "\n"
          << "    duty (abs'ed)  [W] : " << duty                         << "\n";

      // part two -- additional calculations
      double transmission = 0.0;
      double lineloss     = 0.0;
      boost::tie(transmission,
                 lineloss)
        = ::loadFlow(d_voltage,
                     d_resistancePerMetre * d_length,
                     d_reactancePerMetre  * d_length,
                     thetaDegreesDelta);
      const double relDuty    = transmission / d_capacity;
      const double relLoss    = lineloss / transmission;

      // part three -- further reporting
      put << "\n";
      put << "  AC transmission calculated values"                       << "\n"
          << "    identifier         : " << getIdAndKind()               << "\n"
          << std::showpos
          << "    transmission   [W] : " << transmission                 << "\n"
          << "    line loss      [W] : " << lineloss                     << "\n"
          << "    difference     [W] : " << transmission - lineloss      << "\n"
          << std::fixed << std::setprecision(5)
          << "    delta theta  [deg] : " << thetaDegreesDelta            << "\n"
          << "    relative duty  [%] : " << (relDuty * 100)              << "\n"
          << "    relative loss  [%] : " << (relLoss * 100)              << "\n";

      // display
      s_logger->repx(logga::dbug, "additional reporting follows, yeek", xeona::yeek);
      s_logger->putx(logga::dbug, put);
    }

  // accrued costs processing
  CostSet varCosts(0.0);                               // transfer object
  CostSet fixCosts(0.0);                               // transfer object
  d_ops->downloadVarCosts(varCosts);                   // recover variable costs
  d_ops->downloadFixCosts(fixCosts);                   // recover fixed costs
  updateShortrunCosts(d_step, varCosts, fixCosts);     // utilize supplied data
  updateEmbeddedCosts(d_step);                         // utilize in-house calcs

  // store some on-the-fly statistics
  d_dutyStats(duty);                         // functor provided by class 'Block'
  d_sizeStats(d_capacity);                   // functor provided by class 'Block'

} // function 'TeasAcTransmission::washup'

// ---------------------------------------------------------
//  MEMBER FUNCTION : conclude
// ---------------------------------------------------------

void
TeasAcTransmission::conclude()
{
  // additional reporting as appropriate
  // YEEK 60 CODE (set by '--yeek')
  if ( xeona::yeek == 60 || xeona::yeek == 1 || xeona::yeek == 2 )
    {
      Statistics<double> vadstat(d_voltageAngleDeltas);     // smart pointer to vector
      const double minvad     = vadstat.min();
      const double maxvad     = vadstat.max();

      shared_ptr<std::vector<double> > abs = xeona::vectorAbs(d_voltageAngleDeltas);
      Statistics<double> vadstatAbs(abs);                   // smart pointer to vector
      const double minvadAbs  = vadstatAbs.min();
      const double maxvadAbs  = vadstatAbs.max();

      const double vadratio   = maxvadAbs / d_voltageAngleDeltaUpper;
      const double vadratioPc = vadratio * 100.0;
      const double couldTry   = std::ceil(maxvadAbs * 1.30);     // increase by 30%

      std::ostringstream put;
      put << "  AC transmission VAD (voltage angle delta) summary"                << "\n"
          << "    identifier         : " << getIdAndKind()                        << "\n"
          << "    VAD cap         [deg] : " << d_voltageAngleDeltaUpper           << "\n"
          << "    min VAD         [deg] : " << minvad                             << "\n"
          << "    max VAD         [deg] : " << maxvad                             << "\n"
          << "    min abs VAD     [deg] : " << minvadAbs                          << "\n"
          << "    max abs VAD     [deg] : " << maxvadAbs                          << "\n"
          << "    max / cap ratio   [%] : " << boost::format("%.0f") % vadratioPc << "\n";
      if ( vadratio > 0.95 )
        {
          put << "    WARNING               : " << "VAD set too low"              << "\n";
        }
      else if ( vadratio > 0.70 )
        {
          put << "    CAUTION               : " << "VAD set rather low"           << "\n";
        }
      if ( couldTry < d_voltageAngleDeltaUpper )
        {
          put << "    could try (30%) [deg] : " << couldTry                       << "\n";
        }
      s_logger->repx(logga::dbug, "additional reporting follows, yeek", xeona::yeek);
      s_logger->putx(logga::dbug, put);
    }

} // function 'TeasAcTransmission::conclude'

// ---------------------------------------------------------
//  CLASS           : TeasDcTransmission
// ---------------------------------------------------------

// ---------------------------------------------------------
//  MEMBER FUNCTION : TeasDcTransmission
// ---------------------------------------------------------

TeasDcTransmission::TeasDcTransmission
(const std::string entityId,
 Record&           record) :
  CostRegister(record),
  TechnicalAsset(entityId, record, xeona::e_commitmentModes),
  CostRegisterSRFin(record),
  CostRegisterEmbFin(record),
  d_injectCapacity(record.tieSingle<double>("inject-capacity")),
  d_ohmsPerMetre(record.tieSingle<double>("ohms-per-metre")),
  d_length(record.tieSingle<double>("length")),
  d_discretizationSteps(record.tieSingle<int>("discretization-steps")),
  d_gridCommodity(record.tieSingle<std::string>("grid-commodity")),
  d_directions(record.tieTimeseries<bool>("directions")),
  d_injections(record.tieTimeseries<double>("injections")),
  d_exits(record.tieTimeseries<double>("exits")),
  d_relativeDutys(record.tieTimeseries<double>("relative-dutys")),
  d_relativeLosss(record.tieTimeseries<double>("relative-losss")),
  d_cable(Cable<CmElectricity>::create
          (entityId,                         // me
           record.tieSingle<std::string>("socket-1"),
           d_gridCommodity)),                // common value
  d_socket(Socket<CmElectricity>::create
           (entityId,                        // me
            "elec-1",                        // hard-coded socket label
            d_gridCommodity)),               // common value
  d_voltage(0.0),
  d_ops()                                    // empty pointer
{
  s_logger->repx(logga::xtra, "constructor call", getIdAndKind());
  d_builtinRemark = "experimental";
}

// ---------------------------------------------------------
//  MEMBER FUNCTION : ~TeasDcTransmission
// ---------------------------------------------------------

TeasDcTransmission::~TeasDcTransmission()
{
  s_logger->repx(logga::adhc, "destructor call", getIdAndKind());
}

// ---------------------------------------------------------
//  MEMBER FUNCTION : establish
// ---------------------------------------------------------
//  Description  : sets some initial values
//  Role         : beginning-of-horizon call
//  Techniques   : 'dynamic_pointer_cast'
//  Status       : complete
// ---------------------------------------------------------

void
TeasDcTransmission::establish()
{
  // initial reporting
  s_logger->repx(logga::adhc, "entering member function", "TeasDcTransmission");

  // obtain electricity commodity and its voltage
  const shared_ptr<Commodity>     com  = d_cable->getCm();  // 'd_socket' would also do
  const shared_ptr<CmElectricity> elec = dynamic_pointer_cast<CmElectricity>(com);
  if ( ! elec )
    {
      s_logger->repx(logga::warn, "dynamic pointer cast failed, target", "CmElectricity");
    }
  else
    {
      d_voltage = elec->getVoltage();
      s_logger->repx(logga::adhc, "voltage (from cable)", d_voltage);
    }

  // report on unusual voltages
  const double threshold = 11.0e03;
  if ( d_voltage <= threshold )
    {
      s_logger->repx(logga::rankJumpy, "HV voltage seems low", d_voltage);
    }

} // function 'TeasDcTransmission::establish'

// ---------------------------------------------------------
//  MEMBER FUNCTION : constrain
// ---------------------------------------------------------
//  Description  : technical asset constrain call
//  Role         : characterize current engineering and short-run costs
//  Techniques   : (nothing special)
//  Status       : complete
// ---------------------------------------------------------

const int                                    // return zero
TeasDcTransmission::constrain
(const xeona::DomainMode capacityMode)
{
  // additional initial reporting as appropriate
  // YEEK 30 CODE (set by '--yeek')
  if ( xeona::yeek == 30 || xeona::yeek == 1 || xeona::yeek == 2 )
    {
      const std::string subtype = xeona::demangle(typeid(*this).name()); // CAUTION: deref
      s_logger->repx(logga::adhc, "entering member function", subtype);
    }
  else
    {
      s_logger->repx(logga::adhc, "entering member function", "TeasDcTransmission");
    }

  // create and fill a label object
  const std::string teasId = getIdentifier();
  Label lab(teasId);

  // recreate new operations OSP of the required type
  d_ops.reset(new OpsTransmission(d_solver, d_commitmentMode));
  d_ops->loadOspLabel(lab.str());

  // define some global cols for internal use
  int cabGol = -1;
  int socGol = -1;

  // upload the engineering -- in this case, the directionality
  // is defined by my cable-to-socket arrangement
  boost::tie(cabGol,
             socGol)
    = d_ops->uploadEngineering(d_injectCapacity,
                               d_voltage,
                               d_ohmsPerMetre,
                               d_length,
                               d_discretizationSteps);

  // upload specific costs -- including increment the "shift"
  // term note also the 50/50 split on costs -- see r4150 and
  // back for previous call chain
  d_ops->uploadShortrunCosts(d_dutySpecCosts * 0.5, cabGol);
  d_ops->uploadShortrunCosts(d_dutySpecCosts * 0.5, socGol);
  d_ops->uploadShortrunCosts(d_sizeSpecCosts * d_nameplateSize + d_standingCosts);

  // bind global cols to the relevant interfaces
  d_cable ->bindOsp(d_solver, cabGol);
  d_socket->bindOsp(d_solver, socGol);

  // store duty values
  d_ceilingDuty = +d_injectCapacity;         // automatically decoupled from 'd_capacity'
  d_floorDuty   = -d_injectCapacity;

  // additional reporting as appropriate
  // YEEK 13 CODE (set by '--yeek')
  if ( xeona::yeek == 13 || xeona::yeek == 1 || xeona::yeek == 2 )
    {
      std::ostringstream put;
      put << "  DC transmission engineering (note the use of prefixed units)"    << "\n"
          << std::fixed << std::setprecision(2)
          << "    inject capacity     [MW] : " << ( d_injectCapacity / 1.0e+06 ) << "\n"
          << "    voltage             [kV] : " << ( d_voltage        / 1.0e+03 ) << "\n"
          << "    resistance [micro-ohm/m] : " << ( d_ohmsPerMetre   / 1.0e-06 ) << "\n"
          << "    length              [km] : " << ( d_length         / 1.0e+03 ) << "\n"
          << "    discretization steps [-] : " << d_discretizationSteps          << "\n"
          << std::fixed << std::setprecision(2) << std::showpos
          << "    ceiling duty (unscaled)  : " << d_ceilingDuty                  << "\n"
          << "    floor   duty (unscaled)  : " << d_floorDuty                    << "\n";
      s_logger->repx(logga::dbug, "additional reporting follows, yeek", xeona::yeek);
      s_logger->putx(logga::dbug, put);
    }

  // set duty to zero as not needed in this case, also capture return value
  const int dutyGol = 0;                     // zero here means no need to couple

  // return zero
  return dutyGol;

} // function 'TeasDcTransmission::constrain'

// ---------------------------------------------------------
//  MEMBER FUNCTION : washup
// ---------------------------------------------------------

void
TeasDcTransmission::washup()
{
  // initial reporting
  s_logger->repx(logga::dbug, "entering member function, step", d_step);

  // results recovery
  bool   directionFlag = false;
  double flowCable     =  0.0;               // negative values are valid
  double flowSocket    =  0.0;               // negative values are valid
  double relativeDuty  = -1.0;               // nonsensical value
  double relativeLoss  = -1.0;               // nonsensical value

  boost::tie(directionFlag,                  // flow direction, 'true' if cable to socket
             flowCable,                      // cable flow [W], can be negative
             flowSocket,                     // socket flow [W]
             relativeDuty,                   // relative duty [-]
             relativeLoss)                   // relative size [-]
    = d_ops->downloadSolution();

  // store entity state information
  d_directions->at(d_step) = directionFlag;
  if ( directionFlag == true )               // flow orientation from cable to socket
    {
      d_injections->at(d_step) = +flowCable;
      d_exits     ->at(d_step) = +flowSocket;
    }
  else                                       // opposite flow orientation encountered
    {
      d_injections->at(d_step) = -flowSocket;
      d_exits     ->at(d_step) = -flowCable;
    }
  d_relativeDutys->at(d_step)  = relativeDuty;
  d_relativeLosss->at(d_step)  = relativeLoss;

  const double duty = std::max(std::abs(flowCable), std::abs(flowSocket));

  // additional reporting as appropriate
  // YEEK 13 CODE (set by '--yeek')
  if ( xeona::yeek == 13 || xeona::yeek == 1 || xeona::yeek == 2 )
    {
      std::ostringstream put;
      put << "  transmission solution"                                << "\n"
          << "    direction {0,1} : " << directionFlag                << "\n"
          << std::fixed << std::setprecision(0) << std::showpos
          << "    cable flow      : " << flowCable                    << "\n"
          << "    socket flow     : " << flowSocket                   << "\n"
          << std::fixed << std::setprecision(5) << std::noshowpos
          << "    relative duty   : " << relativeDuty                 << "\n"
          << "    relative loss   : " << relativeLoss                 << "\n"
          << std::fixed << std::setprecision(0) << std::showpos
          << "    injection       : " << d_injections->at(d_step)     << "\n"
          << "    exit            : " << d_exits->at(d_step)          << "\n"
          << "    duty (abs'ed)   : " << duty                         << "\n";
      s_logger->repx(logga::dbug, "additional reporting follows, yeek", xeona::yeek);
      s_logger->putx(logga::dbug, put);
    }

  // accrued costs processing
  CostSet varCosts(0.0);                               // transfer object
  CostSet fixCosts(0.0);                               // transfer object
  d_ops->downloadVarCosts(varCosts);                   // recover variable costs
  d_ops->downloadFixCosts(fixCosts);                   // recover fixed costs
  updateShortrunCosts(d_step, varCosts, fixCosts);     // utilize supplied data
  updateEmbeddedCosts(d_step);                         // utilize in-house calcs

  // store some on-the-fly statistics
  d_dutyStats(duty);                         // functor provided by class 'Block'
  d_sizeStats(d_injectCapacity);             // functor provided by class 'Block'

} // function 'TeasDcTransmission::washup'

// ---------------------------------------------------------
//  CLASS           : TeasSubstation
// ---------------------------------------------------------

// ---------------------------------------------------------
//  MEMBER FUNCTION : TeasSubstation
// ---------------------------------------------------------

TeasSubstation::TeasSubstation
(const std::string entityId,
 Record&           record) :
  CostRegister(record),
  TechnicalAsset(entityId, record, xeona::e_commitmentModes),
  CostRegisterSRFin(record),
  CostRegisterEmbFin(record),
  d_outHiBound(record.tieSingle<double>("out-hi-bound")),
  d_fixedEfficiency(record.tieSingle<double>("fixed-efficiency")),
  d_noLoadLoss(record.tieSingle<double>("no-load-loss")),
  d_outputs(record.tieTimeseries<double>("outputs")),
  d_inElec(Cable<CmElectricity>::create
           (entityId,                        // me
            record.tieSingle<std::string>("socket-electricity"),
            record.tieSingle<std::string>("cable-electricity-commodity"))),
  d_outElec(Socket<CmElectricity>::create
            (entityId,                       // me
             "elec-1",                       // hard-coded socket label
             record.tieSingle<std::string>("socket-electricity-commodity"))),
  d_ops()
{
  // initial reporting
  s_logger->repx(logga::xtra, "constructor call", getIdAndKind());

  // builtin remark
  d_builtinRemark = "beta";
}

// ---------------------------------------------------------
//  MEMBER FUNCTION : ~TeasSubstation
// ---------------------------------------------------------

TeasSubstation::~TeasSubstation()
{
  s_logger->repx(logga::adhc, "destructor call", getIdAndKind());
}

// ---------------------------------------------------------
//  MEMBER FUNCTION : establish
// ---------------------------------------------------------

void
TeasSubstation::establish()
{
  // initial reporting
  s_logger->repx(logga::adhc, "entering member function", "");
}

// ---------------------------------------------------------
//  MEMBER FUNCTION : constrain
// ---------------------------------------------------------
//  Description  : technical asset constrain call
//  Role         : characterize current engineering and short-run costs
//  Techniques   : 'boost::tuples::ignore' to ignore tuple ties
//  Status       : complete
// ---------------------------------------------------------

const int                                    // duty gol
TeasSubstation::constrain
(const xeona::DomainMode capacityMode)
{
  // initial reporting
  s_logger->repx(logga::adhc, "entering member function", "TeasSubstation");

  // create and fill a label object
  const std::string teasId = getIdentifier();
  Label lab(teasId);

  // recreate new operations OSP of the required type
  d_ops.reset(new OpsFixedEffy(d_solver, d_commitmentMode));
  d_ops->loadOspLabel(lab.str());

  // define global cols for internal use
  int  inGol = -1;                           // nonsensical value
  int outGol = -1;                           // nonsensical value

  const double outLoBound = 0.0;             // zero is reasonable

  // upload the engineering
  boost::tie(inGol,                          // input (factor) stream
             outGol)                         // output (product) stream
    = d_ops->uploadEngineering(outLoBound,
                               d_outHiBound,
                               d_fixedEfficiency,
                               d_noLoadLoss);

  // upload specific costs -- including increment the "shift" term
  d_ops->uploadShortrunCosts(d_dutySpecCosts, inGol);
  d_ops->uploadShortrunCosts(d_sizeSpecCosts * d_nameplateSize + d_standingCosts);

  // bind global cols to the relevant interfaces
  d_inElec ->bindOsp(d_solver,  inGol);
  d_outElec->bindOsp(d_solver, outGol);

  // store duty values
  d_floorDuty   =   outLoBound;
  d_ceilingDuty = d_outHiBound;

  // return the duty gol
  return outGol;
}

// ---------------------------------------------------------
//  MEMBER FUNCTION : washup
// ---------------------------------------------------------
//  Description  : technical asset washup call
//  Role         : recover and record solution
//  Techniques   : (nothing special)
//  Status       : complete
// ---------------------------------------------------------

void
TeasSubstation::washup()
{
  // initial reporting
  s_logger->repx(logga::dbug, "entering member function, step", d_step);

  // results recovery
  double input  = -1.0;                      // nonsensical value
  double output = -1.0;                      // nonsensical value
  boost::tie(input,
             output) = d_ops->downloadSolution();

  // store entity state information
  d_outputs->at(d_step) = output;

  // accrued costs processing
  CostSet varCosts(0.0);                               // transfer object
  CostSet fixCosts(0.0);                               // transfer object
  d_ops->downloadVarCosts(varCosts);                   // recover variable costs
  d_ops->downloadFixCosts(fixCosts);                   // recover fixed costs
  updateShortrunCosts(d_step, varCosts, fixCosts);     // utilize supplied data
  updateEmbeddedCosts(d_step);                         // utilize in-house calcs

  // store some on-the-fly statistics
  d_dutyStats(output);                       // functor provided by class 'Block'
  d_sizeStats(d_outHiBound);                 // functor provided by class 'Block'

}

//  end of file

