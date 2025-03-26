//  XEONA ENERGY SYSTEMS MODELING ENVIRONMENT
//
//  file-create-name : teas07.cc
//  file-create-date : Thu 20-Jan-2011 16:07 UTC
//  file-initiator   : Robbie Morrison <robbie@actrix.co.nz>
//
//  file-role        : concrete technical assets 7 / implementation
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
//  $URL: file:///home/robbie/svn-root/xeona/futz/trunk/xeona1/b/teas07.cc $
//
//  GENERAL NOTES FOR THIS FILE

//  LOCAL AND SYSTEM INCLUDES

#include "teas07.h"           // companion header for this file (place first)

#include "../e/cxamb03.h"     // concrete ambient conditions contexts 3
#include "../c/tsops.h"       // overloaded operators and similar for timeseries
#include "../c/stats.h"       // on-the-fly statistical calculations
#include "../c/recset.h"      // records and fields and also record-sets
#include "../c/label.h"       // helper class to format solver labels
#include "../c/hydro.h"       // hydro asset to operator data transfer
#include "../c/conex.h"       // create and connect block interfaces
#include "../b/optops1.h"     // operate optimization sub-problems for hard assets 1
#include "../b/lmpbid.h"      // LMP auction bidset
#include "../b/commods.h"     // commodities hierarchy
#include "../a/exent.h"       // entity exception classes

#include "../a/logger.h"      // standard logging functionality (as required)
#include "../c/smart_ptr.h"   // toggle between Boost and TR1 smart pointers
#include ".././common.h"      // common definitions for project (place last)

#include <string>             // C++ strings
#include <sstream>            // string-streams
#include <limits>             // numeric_limits<T>::infinity() and similar

#include <cmath>              // C-style maths, abs(), ceil(), floor(), sqrt()

#include <boost/lexical_cast.hpp>       // lexical_cast<> string to number conversions

//  CODE

// ---------------------------------------------------------
//  CLASS           : TeasHydroScheme
// ---------------------------------------------------------

// STATIC DEFINITIONS

// the dependence of mass density on temperature is insignificant
// from zero to 20C

const double TeasHydroScheme::s_waterMassDensity = 1000.0;  // [kg/m3]

// ---------------------------------------------------------
//  MEMBER FUNCTION : TeasHydroScheme
// ---------------------------------------------------------
//
//  Design notes
//
//      This asset is intended to be paired with the nodal market
//      operator 'AsopLmpBidHydro1'.
//
//      The water to electricity input/output relationship (aka
//      conversion factor) is captured by a simple multiplicative
//      formula, adjusted for any lowering of the lake surface.
//      See 'calcIo' for details.
//
//      The capacity is also a function of lake level.  See
//      'calcCapacityFactor' for more information.
//
//      The extremes also count too.  If there is too much inflow
//      to handle, water is spilled.  If too little storage, the
//      maximum output is adjusted accordingly.
//
//      There is a one way dialog by the operator.  The
//      information downloads by the operator are facilitated by
//      the class 'HydroStatus'.  The operator also uploads bid
//      information for end of simulation reporting, via the
//      class 'LmpBidSet'.
//
//  REFERENCES
//
//      Fink, Donald G, H Wayne Beaty, and John M Carroll.  1999.
//          Standard handbook for electrical engineers -- 14th
//          revised edition.  McGraw-Hill Publishing, New York,
//          USA.  ISBN-10 0070220050.  ISBN-13 978-0070220058.
//          (cited here for future reference, was recommended but
//          not sighted)
//
// ---------------------------------------------------------

TeasHydroScheme::TeasHydroScheme
(const std::string entityId,
 Record&           record) :
  CostRegister(record),
  TechnicalAsset(entityId, record, xeona::e_commitmentModes),
  CostRegisterSRFin(record),
  CostRegisterEmbFin(record),
  d_inflowContext(record.lazyLink<CxInflow>("inflow-context")),
  d_generatorCapacity(record.tieSingle<double>("generator-capacity")),
  d_storageVolume(record.tieSingle<double>("storage-volume")),
  d_operatingDepth(record.tieSingle<double>("operating-depth")),
  d_staticHead(record.tieSingle<double>("static-head")),
  d_primaryEfficiency(record.tieSingle<double>("primary-efficiency")),
  d_openingStorage(record.tieSingle<double>("opening-storage")),
  d_potentialProductions(record.tieTimeseries<double>("potential-productions")),
  d_actualProductions(record.tieTimeseries<double>("actual-productions")),
  d_reservoirStorages(record.tieTimeseries<double>("reservoir-storages")),
  d_closingInventory(record.tieSingle<double>("closing-inventory")),
  d_inventoryDelta(record.tieSingle<double>("inventory-delta")),
  d_meanInflow(record.tieSingle<double>("mean-inflow")),
  d_capacityFactor(record.tieSingle<double>("capacity-factor")),
  d_spillFactor(record.tieSingle<double>("spill-factor")),
  d_spillCount(record.tieSingle<int>("spill-count")),
  d_submittedBidsets(record.tieTimeseries<std::string>("submitted-bidsets")),
  d_outElec(Socket<CmElectricity>::create
            (entityId,                       // me
             "elec-1",                       // hard-coded socket label
             record.tieSingle<std::string>("socket-electricity-commodity"))),
  d_io(0.0),                                 // current generation efficiency [J/m3]
  d_capacity(0.0),                           // current generation capacity [W]
  d_openingInventory(0.0),
  d_availableInventory(0.0),
  d_inflowVol(0.0),
  d_inflowHistoricalVol(0.0),
  d_priorInventory(0.0),
  d_priorInflowVol(0.0),
  d_priorTakeVol(0.0),
  d_priorSpillVol(0.0),
  d_priorDispatch(0.0),
  d_spillTally(0.0),
  d_statInflows(),
  d_monthlyInflows(),
  d_ops()
{
  // initial reporting
  s_logger->repx(logga::xtra, "constructor call", getIdAndKind());

  // buildin remark
  d_builtinRemark = "beta";

  // integrity checks
  if ( d_staticHead < 0.0 )
    {
      s_logger->repx(logga::warn, "negative static head", d_staticHead);
    }
  else if ( d_staticHead == 0.0 )            // acceptable but weird
    {
      s_logger->repx(logga::rankJumpy, "zero static head", d_staticHead);
    }
  if ( d_primaryEfficiency < 0.0 || d_primaryEfficiency > 1.0 )
    {
      s_logger->repx(logga::warn, "aphysical primary efficiency", d_primaryEfficiency);
    }
  if ( d_openingStorage < 0.0 || d_openingStorage > 1.0 )
    {
      s_logger->repx(logga::warn, "opening storage not [0,1]", d_openingStorage);
    }
  if ( d_operatingDepth > d_staticHead )
    {
      std::ostringstream oss;
      oss << d_operatingDepth << " " << d_staticHead;
      s_logger->repx(logga::warn, "operating depth exceeds static head", oss.str());
    }

  // set opening inventory
  d_openingInventory = d_openingStorage * d_storageVolume;
  s_logger->repx(logga::adhc, "opening inventory", d_openingInventory);

  // zero spill count
  if ( d_spillCount != 0 )
    {
      s_logger->repx(logga::warn, "spill count not zero (reset model?)", d_spillCount);
      d_spillCount = 0;
    }

} // function 'TeasHydroScheme::TeasHydroScheme'

// ---------------------------------------------------------
//  MEMBER FUNCTION : ~TeasHydroScheme
// ---------------------------------------------------------

TeasHydroScheme::~TeasHydroScheme()
{
  s_logger->repx(logga::adhc, "destructor call", getIdAndKind());
}

// ---------------------------------------------------------
//  MEMBER FUNCTION : establish
// ---------------------------------------------------------
//  Description  : sets some initial values
//  Role         : beginning-of-horizon call
//  Techniques   : (nothing special)
//  Status       : complete
// ---------------------------------------------------------

void
TeasHydroScheme::establish()
{
  // initial reporting
  s_logger->repx(logga::adhc, "entering member function, step", d_step);

  // zero storage causes lots of div-by-zero problems
  if ( d_storageVolume == 0.0 )
    {
      if ( xeona::nopro == false )           // meaning option '--krazy' not applied
        {
          std::ostringstream oss;
          oss << "storage set to zero under in-data 'storage-volume' : "
              << d_storageVolume;
          s_logger->repx(logga::warn, "about to throw", "xeona::entity_issue_2");
          throw xeona::entity_issue_2("TeasHydroScheme",
                                      __func__,
                                      getIdAndKind(),
                                      oss.str());
        }
      else
        {
          s_logger->repx(logga::warn, "storage-volume cannot be zero", d_storageVolume);
        }
    }

  // preamble
  const int interval = Entity::getHorizonInterval();   // time in seconds

#if 1 // 1 = normal, 0 = development only
  const double init  = 0.0;
#else
#warning "development code using 'NaN's"
  const double init = std::numeric_limits<double>::quiet_NaN();
#endif // 0

  // calculate the maximum take and then the truncated take,
  // keeping the values +ve for now
  const double histInflow    = d_inflowContext->getHistorical(0);
  const double histInflowVol = histInflow * interval;
  const double relInv        = d_openingInventory / d_storageVolume;
  const double statHead      = d_staticHead - (1.0 - relInv) * d_operatingDepth;
  const double ioFactor      = calcIo(statHead);
  const double maxTakeVol    = (d_generatorCapacity / ioFactor) * interval;
  const double halfTakeVol   = maxTakeVol / 2.0;       // [3]
  const double truncTakeVol
    = ( d_openingInventory > halfTakeVol ) ? halfTakeVol : d_openingInventory;

  // create some initial values for the short-lived 'HydroStatus' object
  d_priorInventory = +d_openingInventory;    // [1]
  d_priorInflowVol = +histInflowVol;         // [2]
  d_priorTakeVol   = -truncTakeVol;          // [3]
  d_priorSpillVol  = -init;                  // [4]
  d_priorDispatch  = +init;                  // [5]

  // [1] correct
  // [2] in the absence of other information, use the long-run monthly mean
  // [3] in the absence of other information, use HALF the maximum capacity
  // [4] in the absence of other information, can set to 'NaN' to prove not used
  // [5] in the absence of other information, can set to 'NaN' to prove not used
}

// ---------------------------------------------------------
//  MEMBER FUNCTION : constrain
// ---------------------------------------------------------
//  Description  : constrain call
//  Role         : characterize current engineering and short-run costs
//  Techniques   : member functions 'calcIo' and 'calcCapacityFactor'
//  Status       : complete
//
//  Design notes
//
//      See the documentation for the class and for the utility
//      functions 'calcIo' and 'calcCapacityFactor'.
//
//      The influences of storage level on static head, and
//      thereby on efficiency and capacity, are accounted for
//      here.  Any spill is calculated in 'TeasHydroScheme::washup'.
//
//      This function also updates 'd_io' 'd_capacity'
//      'd_availableInventory' 'd_inflowVol'.
//
//  Currency choice
//
//      Water volume in [m3] -- constant flow integrated over
//      interval length -- is used in the dialog with the
//      operator and, later, for bid formation decisions.
//
//      Electricity generation in [W] -- at constant rate -- is
//      used when forming the optimization sub-problem (OSP) and
//      when communicating the solver.
//
//      Please be very cognizant of these differences.
//
// ---------------------------------------------------------

const int                                    // duty gol
TeasHydroScheme::constrain
(const xeona::DomainMode capacityMode)
{
  // initial reporting
  s_logger->repx(logga::adhc, "entering member function", "TeasHydroScheme");

  // preamble
  const int interval = Entity::getHorizonInterval();   // time in seconds

  // create and fill a label object
  const std::string teasId = getIdentifier();
  Label lab(teasId);

  // recreate new operations OSP of the required type
  d_ops.reset(new OpsFac0Out1(d_solver, d_commitmentMode));
  d_ops->loadOspLabel(lab.str());

  // define a global col for internal use
  int outGol = -1;                           // nonsensical value

  // get prevailing inflow information and integrate over interval
  const double inflow     = d_inflowContext->getInflow(d_step);
  const double inflowHist = d_inflowContext->getHistorical(d_step);
  d_inflowVol             = inflow     * interval;
  d_inflowHistoricalVol   = inflowHist * interval;

  // add 'inflow' to on-the-fly statistics
  d_statInflows(inflow);

  // calculate new available inventory -- meaning before any take and/or spill
  d_availableInventory = d_priorInventory + d_inflowVol;

  // calculate the current static head -- simple geometry
  const double relativeInventory = d_availableInventory / d_storageVolume;
  const double staticHead = d_staticHead - (1.0 - relativeInventory) * d_operatingDepth;

  // update the current capacity and conversion (efficiency) factors
  const double capacityFactor = calcCapacityFactor(staticHead);
  d_capacity                  = capacityFactor * d_generatorCapacity;
  d_io                        = calcIo(staticHead);

  // calculate available power in [W], while remembering to truncate on capacity
  double availpower = d_io * d_availableInventory / interval;
  if ( availpower > d_capacity ) availpower = d_capacity;

  // calculate power range
  const double loPower = 0.0;
  const double hiPower = availpower;
  s_logger->repx(logga::xtra, "potential production, hiPower", hiPower);

  // upload the engineering
  boost::tie(outGol) = d_ops->uploadEngineering(loPower, hiPower);

  // upload specific costs -- including increment the "shift" term
  d_ops->uploadShortrunCosts(d_dutySpecCosts, outGol);
  d_ops->uploadShortrunCosts(d_sizeSpecCosts * d_nameplateSize + d_standingCosts);

  // bind global cols to the relevant interfaces
  d_outElec->bindOsp(d_solver, outGol);

  // store values
  d_potentialProductions->at(d_step) = hiPower;

  // store duty values
  d_floorDuty   = loPower;
  d_ceilingDuty = hiPower;

  // additional reporting as appropriate
  // YEEK 43 CODE (set by '--yeek')
  if ( xeona::yeek == 43 || xeona::yeek == 1 || xeona::yeek == 2 )
    {
      std::ostringstream put;
      put << "  TeasHydroScheme constrain report"                               << "\n"
          << "    note: available inventory includes inflow but not outflow"    << "\n"
          << "      identifier                : " << getIdAndKind()             << "\n"
          << "      step                      : " << d_step                     << "\n"
          << "      interval              [s] : " << interval                   << "\n"
          << "    intermediate"                                                 << "\n"
          << "      nominal static head   [m] : " << d_staticHead               << "\n"
          << "      static head           [m] : " << staticHead                 << "\n"
          << "      io factor          [J/m3] : " << d_io                       << "\n"
          << "      capacity factor       [-] : " << capacityFactor             << "\n"
          << "      relative inventory    [-] : " << relativeInventory          << "\n"
          << "      available inventory  [m3] : " << d_availableInventory       << "\n"
          << "    inflow"                                                       << "\n"
          << std::showpos
          << "      inflow             [m3/s] : " << (d_inflowVol / interval)   << "\n"
          << "      inflow volume        [m3] : " << d_inflowVol                << "\n"
          << std::noshowpos
          << "    settings"                                                     << "\n"
          << "      lo power              [W] : " << loPower                    << "\n"
          << "      hi power              [W] : " << hiPower                    << "\n";
      s_logger->repx(logga::dbug, "additional reporting follows, yeek", xeona::yeek);
      s_logger->putx(logga::dbug, put);
    }

  // return the duty gol
  return outGol;

} // function 'TeasHydroScheme::constrain'

// ---------------------------------------------------------
//  MEMBER FUNCTION : washup
// ---------------------------------------------------------
//  Description  : recover the solution, calculate any spill, and record the result
//  Role         : technical asset end-of-interval washup call
//  Techniques   : (nothing special)
//  Status       : complete
//
//  Design notes
//
//      Also updates ''d_priorInventory' 'd_priorInflowVol'
//      d_priorTakeVol' 'd_priorSpillVol' 'd_priorDispatch'.
//
// ---------------------------------------------------------

void
TeasHydroScheme::washup()
{
  // initial reporting
  s_logger->repx(logga::dbug, "entering member function, step", d_step);

  // preamble
  const int interval = Entity::getHorizonInterval();   // time in seconds
  std::string status = "(not overwritten)";            // used for yeek reporting

  // results recovery
  double actualProduction      = -1.0;       // nonsensical value
  boost::tie(actualProduction) = d_ops->downloadSolution();

  // reverse engineer water take in [m3]
  const double waterTake = -1.0 * (actualProduction / d_io) * interval;

  // calculate the new inventory
  double inventory = d_availableInventory + waterTake;
  double spillVol  = 0.0;
  status           = "no water spillage";
  if ( inventory > d_storageVolume )                   // spillage occurs
    {
      spillVol      = d_storageVolume - inventory;     // necessarily -ve
      inventory     = d_storageVolume;
      d_spillTally += (spillVol * d_io) / interval;    // in [W] like production
      d_spillCount++;
      status = "water spillage";
    }

  // update dedicated quantities for 'getStatus' call for NEXT interval
  d_priorInventory = inventory;
  d_priorInflowVol = d_inflowVol;
  d_priorTakeVol   = waterTake;
  d_priorSpillVol  = spillVol;
  d_priorDispatch  = actualProduction;

  // store entity state information
  d_actualProductions->at(d_step) = actualProduction;
  d_reservoirStorages->at(d_step) = inventory / d_storageVolume;

  // accrued costs processing
  CostSet varCosts(0.0);                               // transfer object
  CostSet fixCosts(0.0);                               // transfer object
  d_ops->downloadVarCosts(varCosts);                   // recover variable costs
  d_ops->downloadFixCosts(fixCosts);                   // recover fixed costs
  updateShortrunCosts(d_step, varCosts, fixCosts);     // utilize supplied data
  updateEmbeddedCosts(d_step);                         // utilize in-house calcs

  // store some on-the-fly statistics
  const double potentialProduction = d_potentialProductions->at(d_step);
  d_dutyStats(actualProduction);             // functor provided by class 'Block'
  d_sizeStats(potentialProduction);          // functor provided by class 'Block'

  // additional reporting as appropriate
  // YEEK 43 CODE (set by '--yeek')
  if ( xeona::yeek == 43 || xeona::yeek == 1 || xeona::yeek == 2 )
    {
      std::ostringstream put;
      put << "  TeasHydroScheme washup report"                                  << "\n"
          << "      identifier                : " << getIdAndKind()             << "\n"
          << "      step                      : " << d_step                     << "\n"
          << "      interval              [s] : " << interval                   << "\n"
          << "    production results"                                           << "\n"
          << "      production            [W] : " << actualProduction           << "\n"
          << std::showpos
          << "      take outflow       [m3/s] : " << (waterTake / interval)     << "\n"
          << "      take volume          [m3] : " << waterTake                  << "\n"
          << std::noshowpos
          << "      inventory            [m3] : " << inventory                  << "\n"
          << "    spillage"                                                     << "\n"
          << std::showpos
          << "      spill outflow      [m3/s] : " << (spillVol / interval)      << "\n"
          << "      spill volume         [m3] : " << spillVol                   << "\n"
          << std::noshowpos
          << "      spill status              : " << status                     << "\n"
          << "      spill count to date       : " << d_spillCount               << "\n";
      s_logger->repx(logga::dbug, "additional reporting follows, yeek", xeona::yeek);
      s_logger->putx(logga::dbug, put);
    }

} // function 'TeasHydroScheme::washup'

// ---------------------------------------------------------
//  MEMBER FUNCTION : conclude
// ---------------------------------------------------------
//  Description  : calculate and load aggregated metrics
//  Role         : end-of-horizon call
//  Techniques   : class 'Statistics', div-by-zero protection
//  Status       : complete
//
//  Design notes
//
//      Definitions:
//
//        * availability     = actual production / generator capacity
//        * normalized spill = discarded production / actual production
//
//     Note that the div-by-zero protection can be removed by
//     utilizing option '--krazy'.
//
// ---------------------------------------------------------

void
TeasHydroScheme::conclude()
{
  // preparation
  const Statistics<double> statActualProductions(d_actualProductions);
  const double actualProductionsMean = statActualProductions.mean();
  const double actualProductionsSum  = statActualProductions.sum();
  const double inflowsMean           = d_statInflows.mean();

  // update metrics -- noting the div-by-zero protections
  d_closingInventory = d_reservoirStorages->back() * d_storageVolume;
  d_meanInflow       = inflowsMean;

  // inventory delta [-]
  if ( d_storageVolume != 0.0 || xeona::nopro == true )        // meaning option '--krazy'
    {
      d_inventoryDelta = (d_closingInventory - d_openingInventory) / d_storageVolume;
    }
  else
    {
      d_inventoryDelta = 0.0;
      s_logger->repx(logga::rankJumpy,
                     "inventory-delta div-by-zero, now",
                     d_inventoryDelta);
    }

  // capacity factor [-]
  if ( d_generatorCapacity != 0.0 || xeona::nopro == true ) // meaning option '--krazy'
    {
      d_capacityFactor = actualProductionsMean / d_generatorCapacity;
    }
  else
    {
      d_capacityFactor = 0.0;
      s_logger->repx(logga::rankJumpy,
                     "capacity factor div-by-zero, now",
                     d_capacityFactor);
    }

  // spill factor [-]
  if ( (actualProductionsSum + -d_spillTally) != 0.0   // div-by-zero protection
       ||
       xeona::nopro == true )                          // meaning option '--krazy'
    {
      d_spillFactor = -d_spillTally / (actualProductionsSum + -d_spillTally);   // in [W]
      d_spillFactor = std::abs(d_spillFactor);                                  // [1]
      // [1] the 'std::abs' avoids the somewhat strange -0.0 when 'd_spillTally' is zero
    }
  else
    {
      d_spillFactor = 0.0;
      s_logger->repx(logga::rankJumpy,
                     "spill div-by-zero, now",
                     d_spillFactor);
    }

} // function 'TeasHydroScheme::conclude'

//  OPERATOR DIALOG

// ---------------------------------------------------------
//  MEMBER FUNCTION : getStatus
// ---------------------------------------------------------
//  Description  : give status data to operator
//  Role         : operator download
//  Techniques   : class 'HydroStatus'
//  Status       : complete
//
//  CAUTION: use before operator optimization sub-problem (OSP) call
//
// ---------------------------------------------------------

// string  hydroIdentifier
// int     step
// double  io
// double  capacity
// double  storageVol
// double  inventory
// double  priorInventory
// double  inflowVol
// double  histInflowVol
// double  priorInflowVol
// double  priorTakeVol
// double  priorSpillVol
// double  priorDispatch

shared_ptr<HydroStatus>
TeasHydroScheme::getStatus() const
{
  // prepare 'HydroStatus' object
  shared_ptr<HydroStatus>
    status(new HydroStatus
           (getIdAndKind(),
            d_step,
            d_io,                            // varies for each period
            d_capacity,                      // varies for each period
            d_storageVolume,
            d_availableInventory,
            d_priorInventory,
            d_inflowVol,
            d_inflowHistoricalVol,
            d_priorInflowVol,
            d_priorTakeVol,
            d_priorSpillVol,
            d_priorDispatch));

  return status;
}

// ---------------------------------------------------------
//  MEMBER FUNCTION : putBidSet
// ---------------------------------------------------------
//  Description  : records current dialog
//  Role         : operator upload
//  Techniques   : 'LmpBidSet::stringify'
//  Status       : complete
//
//  CAUTION: use after operator optimization sub-problem (OSP) call
//
// ---------------------------------------------------------

void
TeasHydroScheme::putBidSet
(shared_ptr<LmpBidSet> bidset)
{
  const std::string bidsetstr    = bidset->stringify();
  d_submittedBidsets->at(d_step) = bidsetstr;
  s_logger->repx(logga::adhc, "bidset as string", bidsetstr);
}

// ---------------------------------------------------------
//  MEMBER FUNCTION : calcIo
// ---------------------------------------------------------
//  Description  : calculate water take to electricity conversion
//  Role         : internal support
//  Techniques   : simple multiplicative equation
//  Status       : complete
//
//  Design notes
//
//      This calculation is based on Bernoulli's steady-flow
//      equation for incompressible flow.
//
//      The simple multiplicative formula employed here relies on
//      the following assumptions either holding true or being
//      captured satisfactorily by the primary efficiency:
//
//        - kinetic energy at inlet and discharge is insignificant
//        - hydraulic losses assumed linear on flow
//        - evaporation and seepage as per primary efficiency
//        - primary efficiency can be regarded as constant
//        - no weather context is required
//
//      Taking the head change, in the case of the Benmore scheme
//      the static head is 92m and the reservoir level varies
//      within a 0.8m band.  Admittedly this is a downstream
//      station and a top station would experience more
//      variation.
//
//      Recall it is the solver that calculates the level of
//      generation and water use.  This function simply returns
//      the conversion factor.
//
//      Note that run-of-river stations can be modeled by setting
//      the storage to near zero and the head to a few metres.
//
//      In summary, this formula is considered quite representative.
//
// ---------------------------------------------------------

double                                       // resultant io factor [W/m3]
TeasHydroScheme::calcIo
(const double staticHead) const              // less than or equal to nominal static head
{
  // integrity checks
  if ( staticHead < 0.0 )
    {
      s_logger->repx(logga::warn, "negative static head given", staticHead);
    }
  else if ( staticHead > d_staticHead )
    {
      std::ostringstream oss;
      oss << staticHead << " " << d_staticHead;
      s_logger->repx(logga::rankJumpy, "static head above nominal given", oss.str());
    }

  // active code
  const double io
    = s_waterMassDensity                     // close to constant [kg/m3]
    * xeona::stdGravity                      // standard gravity, 9.81 [m/s2]
    * staticHead                             // not considered constant [m]
    * d_primaryEfficiency;                   // general factor, perhaps 0.8
  return io;                                 // about 700e3 for Benmore
}

// ---------------------------------------------------------
//  MEMBER FUNCTION : calcCapacityFactor
// ---------------------------------------------------------
//  Description  : calculate generator capacity factor
//  Role         : internal support
//  Techniques   : turbine curve
//  Status       : complete
//
//  Design notes
//
//      The following capacity/head relationship (or turbine
//      curve) is used here:
//
//                h       /   q   \ 2
//               ---   = |   ---   |
//              h_nom     \ q_nom /
//
//
//          for q in [0,q_nom] and where h is static head, q is
//          volumetric flow (in this case, the "take"), and "nom"
//          indicates nominal (maximum) conditions.
//
//      This equation is duly reworked:
//
//                       q       /  h    \ 1/2
//              f(h) =  ---   = |  ---    |
//                     q_nom     \ h_nom /
//
//          where f is the sought capacity factor.
//
//      The interplay of efficiency on the equation shown is
//      second-order and is not considered here.
//
// ---------------------------------------------------------

double                                       // capacity factor [-] on range [0,1]
TeasHydroScheme::calcCapacityFactor
(const double staticHead) const              // less than or equal to nominal static head
{
  // integrity checks
  if ( staticHead < 0.0 )
    {
      s_logger->repx(logga::warn, "negative static head given", staticHead);
    }
  else if ( staticHead > d_staticHead )
    {
      std::ostringstream oss;
      oss << staticHead << " " << d_staticHead;
      s_logger->repx(logga::rankJumpy, "given static head above nominal", oss.str());
    }

  // active code with div-by-zero protection
  if ( d_staticHead == 0.0 )
    {
      return 1.0;
    }
  else
    {
      const double capacityFactor = std::sqrt(staticHead / d_staticHead);
      return capacityFactor;                     // say 0.99 for Benmore at a low level
    }
}

//  end of file

