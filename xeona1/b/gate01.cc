//  XEONA ENERGY SYSTEMS MODELING ENVIRONMENT
//
//  file-create-name : gate1.cc
//  file-create-date : Wed 15-Apr-2009 21:34 UTC
//  file-initiator   : Robbie Morrison <robbie@actrix.co.nz>
//
//  file-role        : concrete gateways 1 - stated tariff / implementation
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
//  $URL: file:///home/robbie/svn-root/xeona/futz/trunk/xeona1/b/gate01.cc $
//
//  GENERAL NOTES FOR THIS FILE

//  LOCAL AND SYSTEM INCLUDES

#include "gate01.h"           // companion header for this file (place first)

#include "../c/util3.h"       // free functions for floating point comparison
#include "../c/recset.h"      // records and fields and also record-sets
#include "../c/label.h"       // helper class to format solver labels
#include "../c/costs.h"       // cost sets and support
#include "../c/conex.h"       // create and connect block interfaces
#include "../b/optprob.h"     // optimization sub-problem and key sub-classes
#include "../b/optgate.h"     // various OSPs for gateways
#include "../b/domcon.h"      // domain controller entity
#include "../b/commods01.h"   // concrete commodities 1
#include "../b/commods.h"     // commodities hierarchy
#include "../b/bandtaf.h"     // banded tariff set and support

#include "../a/logger.h"      // standard logging functionality (as required)
#include "../c/smart_ptr.h"   // toggle between Boost and TR1 smart pointers
#include ".././common.h"      // common definitions for project (place last)

#include <string>             // C++ strings
#include <sstream>            // string-streams
#include <utility>            // STL pair, make_pair()

#include <cmath>              // C-style maths, abs(), ceil(), floor(), sqrt()

#include <boost/format.hpp>   // printf style formatting

//  CODE

// ---------------------------------------------------------
//  CLASS           : GateStatedTariff <>
// ---------------------------------------------------------
//
//  CAUTION: name resolution within member functions
//
//      A 'using' declaration (as opposed to a 'using' directive)
//      can only be used at class scope and not at function scope
//      -- hence the need to use fully qualify certain
//      identifiers within member functions.  Most notably:
//
//          d_step
//          d_commitmentMode
//          d_solver
//
// ---------------------------------------------------------

template <typename C>
GateStatedTariff<C>::GateStatedTariff
(const std::string entityId,
 Record&           record,
 const int         commitmentModeSum) :
  Gateway(entityId, record, commitmentModeSum),   // CAUTION: virtual base class init
  GateCom<C>(entityId, record, commitmentModeSum),
  // tied quantities
  d_tariffsets(record.tieTimeseries<std::string>("tariffsets")),
  d_definedCapacitys(record.tieTimeseries<double>("defined-capacitys")),
  d_quantitys(record.tieTimeseries<double>("quantitys")),
  d_marginalPrices(record.tieTimeseries<double>("marginal-prices")),
  d_totalCosts(record.tieTimeseries<double>("total-costs")),
  // internal quantities
  d_tariffset(),                                  // empty shared pointer
  d_capSel(),
  d_capBuy(),
  d_ots(),
  d_ofr()
{
  // initial reporting
  s_logger->repx(logga::adhc, "constructor call", getIdAndKind());

  // built-in remark
  GateCom<C>::d_builtinRemark = "beta";           // CAUTION: note scope resolution
}

template <typename C>
GateStatedTariff<C>::~GateStatedTariff()
{
  // initial reporting
  s_logger->repx(logga::adhc, "destructor call", getIdAndKind());
}

// ---------------------------------------------------------
//  MEMBER FUNCTION : establish
// ---------------------------------------------------------

// the 'd_tariffset' variable is filled for each interval under
// 'initializeBuySide' -- so it does not need to be generated for
// the whole horizon here

template <typename C>
void
GateStatedTariff<C>::establish()
{
  // additional reporting as appropriate
  // YEEK 18 CODE (set by '--yeek')
  if ( xeona::yeek == 18 || xeona::yeek == 1 || xeona::yeek == 2 )
    {
      std::ostringstream put;
      put << "  gateway establish call"           << "\n"
          << "    type     : GateStatedTariff<C>" << "\n"
          << "    identity : " << getIdAndKind()  << "\n";
      s_logger->repx(logga::dbug, "additional reporting follows, yeek", xeona::yeek);
      s_logger->putx(logga::dbug, put);
    }
  else
    {
      s_logger->repx(logga::adhc, "entering member function", "nothing to do");
    }
}

// ---------------------------------------------------------
//  MEMBER FUNCTION : conclude
// ---------------------------------------------------------

template <typename C>
void
GateStatedTariff<C>::conclude()
{
      s_logger->repx(logga::adhc, "entering member function", "nothing to do");
}

// ---------------------------------------------------------
//  MEMBER FUNCTION : initializeBuySide
// ---------------------------------------------------------

template <typename C>
void
GateStatedTariff<C>::initializeBuySide
(const int                  step,
 shared_ptr<svif::SolverIf> solver)
{
  // initial reporting
  s_logger->repx(logga::adhc, "entering member function, step", step);

  // reset and then load the tariffset
  const std::string strTariffset = d_tariffsets->at(step);
  d_tariffset.reset(new BandedTariffSet());
  d_tariffset->pushString(strTariffset);

  // then label the tariffset
  std::string tariffSetLabel = "";
#if 0 // 0 = omit identifier because the OSP rows and cols also begin with this info
  tariffSetLabel += getIdAndKind();          // gateway identifier
  tariffSetLabel += ".";                     // dot separated
#endif // 0
  tariffSetLabel += "init-buy-side";         // my addition
  d_tariffset->setLabel(tariffSetLabel);     // load

  // load the commercial capacity
  const double lowerCommCapacity = 0.0;
  const double upperCommCapacity = d_tariffset->getCapacity();
  d_commCapacity = std::make_pair(lowerCommCapacity, upperCommCapacity);

  // integrity checks (previously added for bughunting)
  if ( strTariffset.empty() )                // tariffset string
    {
      s_logger->repx(logga::warn, "tariffset string empty", strTariffset);
    }
  if ( d_tariffset->empty() )                // banded tariffset
    {
      s_logger->repx(logga::warn, "banded tariffset empty", "");
    }

} // function 'GateStatedTariff<C>::initializeBuySide'

// ---------------------------------------------------------
//  MEMBER FUNCTION : initializeSelSide
// ---------------------------------------------------------

template <typename C>
void
GateStatedTariff<C>::initializeSelSide
(const int                  step,
 shared_ptr<svif::SolverIf> solver)
{
  s_logger->repx(logga::adhc, "entering member function", "");

  // do nothing is correct
}

// ---------------------------------------------------------
//  MEMBER FUNCTION : constrainBuySide
// ---------------------------------------------------------

// resets and fills 'd_tariffset' of class 'BandedTariffSet'
//
// resets and fills 'd_capBuy'    of class 'QanTechCapacity'
// resets and fills 'd_ofr'       of class 'OfrTariffSet'
//
// the tariff set is only used here and is not used by 'constrainSelSide'

template <typename C>
const int
GateStatedTariff<C>::constrainBuySide
(const xeona::DomainMode capacityMode)
{
  // initial reporting
  s_logger->repx(logga::adhc, "entering member function", "GateStatedTariff<C>");

  // define some local variables for convenience
  const int step                    = GateCom<C>::BuySide::d_step;
  shared_ptr<svif::SolverIf> solver = GateCom<C>::BuySide::d_solver;
  xeona::DomainMode commitmentMode  = GateCom<C>::BuySide::d_commitmentMode;

  // TECHNICAL CAPACITY OSP

  // create and fill a label object
  const std::string gateId = getIdentifier();
  Label capLabel(gateId);
  capLabel << "capset-capacity";

  // locate key values
  const double lower = 0.0;
  const double upper = d_definedCapacitys->at(step);
  d_capacity = std::make_pair(lower, upper);      // strictly 'Gateway::d_capacity'

  // recreate new operations OSP of the required type
  d_capBuy.reset(new QanTechCapacity(solver, commitmentMode, ""));
  d_capBuy->loadOspLabel(capLabel.str());

  // define global col/cols for internal use
  int buyGol = 0;

  // upload the engineering (a simple upper bound onto the buy-side)
  boost::tie(buyGol) = d_capBuy->uploadCapacity(d_capacity);

  // not required: upload specific costs
  // not required: store some values ('d_capacity' set by 'constrainSelSide')

  // TARIFFSET OSP

  // create and fill a label object
  Label ofrLabel(gateId);
  ofrLabel << "offer";

  // recreate new operations OSP of the required type
  d_ofr.reset(new OfrTariffSet(solver, commitmentMode, ""));
  d_ofr->loadOspLabel(ofrLabel.str());

  // define global col/cols for internal use
  int ofrGol = 0;

  // upload the tariffset
  Gateway::bounded_type capacity = d_capacity;  // CAUTION: cannot use "d_capacity.second"
  boost::tie(ofrGol) = d_ofr->uploadTariffSet(d_tariffset, capacity.second);

  // not required: upload specific costs
  // not required: bind global cols to the relevant interfaces
  // not required: store some values

  // BINDINGS

  // OSP couple call (from unit 'b/optprob')
  xeona::couple(solver,
                buyGol,
                ofrGol,
                "buy-side.couple");

  // bind global cols to the relevant interfaces
  d_socket->bindOsp(solver, buyGol);

  // RETURN

  // return the duty gol
  s_logger->repx(logga::dbug, "leaving member function, returning", buyGol);
  return buyGol;

} // function 'GateStatedTariff<C>::constrainBuySide'

// ---------------------------------------------------------
//  MEMBER FUNCTION : constrainSelSide
// ---------------------------------------------------------

// resets and fills 'd_capSel' of class 'QanTechCapacity'
// resets and fills 'd_ots'    of class 'QanObligToSupply'

template <typename C>
const int
GateStatedTariff<C>::constrainSelSide
(const xeona::DomainMode capacityMode)
{
  // initial reporting
  s_logger->repx(logga::adhc, "entering member function", "GateStatedTariff<C>");

  // define some local variables for convenience
  const int step                    = GateCom<C>::SelSide::d_step;
  shared_ptr<svif::SolverIf> solver = GateCom<C>::SelSide::d_solver;
  xeona::DomainMode commitmentMode  = GateCom<C>::SelSide::d_commitmentMode;

  // TECHNICAL CAPACITY OSP

  // create and fill a label object
  const std::string gateId = getIdentifier();
  Label opsLabel(gateId);
  opsLabel << "tech-capacity";

  // locate key values
  const double lower = 0.0;
  const double upper = d_definedCapacitys->at(step);
  d_capacity = std::make_pair(lower, upper); // strictly 'Gateway::d_capacity'

  // recreate new operations OSP of the required type
  d_capSel.reset(new QanTechCapacity(solver, commitmentMode, ""));
  d_capSel->loadOspLabel(opsLabel.str());

  // define global col/cols for internal use
  int selGol = 0;

  // upload the engineering (a simple upper bound onto the sel-side)
  boost::tie(selGol) = d_capSel->uploadCapacity(d_capacity);

  // not required: upload specific costs

  // store capacity value as a technical capacity
  d_techCapacity = d_capacity;               // strictly 'Gateway::d_techCapacity'

  // OBLIGATION OSP

  // create and fill a label object
  Label otsLabel(gateId);
  otsLabel << "tech-capacity";

  // recreate new operations OSP of the required type
  d_ots.reset(new QanObligToSupply(solver, commitmentMode, ""));
  d_ots->loadOspLabel(otsLabel.str());

  // define global col/cols for internal use
  int otsGol = 0;

  // upload the supply obligation
  boost::tie(otsGol) = d_ots->uploadObligation(d_transaction);

  // not required: upload specific costs
  // not required: bind global cols to the relevant interfaces
  // not required: store some values

  // BINDINGS

  // OSP couple call (from unit 'b/optprob')
  xeona::couple(solver,
                selGol,
                otsGol,
                "buy-side.couple");

  // bind global cols to the relevant interfaces
  d_cable->bindOsp(solver, selGol);

  // RETURN

  // return the duty gol
  s_logger->repx(logga::dbug, "leaving member function, returning", selGol);
  return selGol;

} // function 'GateStatedTariff<C>::constrainSelSide'

// ---------------------------------------------------------
//  MEMBER FUNCTION : washupBuySide
// ---------------------------------------------------------

template <typename C>
void
GateStatedTariff<C>::washupBuySide()
{
  s_logger->repx(logga::adhc, "entering member function", getIdAndKind());
  s_logger->repx(logga::adhc, "any tasks currently left to sel-side", "");

} // function 'GateStatedTariff<C>::washupBuySide'

// ---------------------------------------------------------
//  MEMBER FUNCTION : washupSelSide
// ---------------------------------------------------------

template <typename C>
void
GateStatedTariff<C>::washupSelSide()
{
  // call counter for reporting
  static int callCount = 0;
  ++callCount;

  // define some local variables for convenience
  const int step = GateCom<C>::SelSide::d_step;

  // interval (time span in seconds)
  const int interval = Entity::getHorizonInterval();

  // initial reporting
  s_logger->repx(logga::dbug, "entering member function, step", step);

  // additional reporting as required
  if ( xeona::releaseStatus == false )
    {
      // information
      const std::string id = getIdAndKind();
      static int callcount = 0;
      callcount++;

      // reporting
      s_logger->repx(logga::adhc, "gate identifier", id);
      s_logger->repx(logga::adhc, "call count", callcount);
    }

  // RESULTS PROCESSING

  // results recovery
  double marginalPrice     = -1.0;              // unlikely value
  double fixedComponent    = -1.0;              // unlikely value
  double variableComponent = -1.0;              // unlikely value
  double transaction       = -1.0;              // nonsensical value
  boost::tie(marginalPrice,
             fixedComponent,
             variableComponent,
             transaction) = d_ofr->downloadSolution(interval);
  double duty = -1.0;                        // nonsensical value
  boost::tie(duty) = d_capSel->downloadSolution();

  // calculate total price
  const double totalPrice = fixedComponent + variableComponent;

  // load Gateway registers
  d_transaction = transaction;
  d_totalCost   = totalPrice;

  // grab capacities -- previously set in 'GateStatedTariff<C>::constrainBuySide'
  const Gateway::bounded_type techCap = d_capacity;    // CAUTION: not "d_capacity.second"
  const double lowerTechCapacity      = techCap.first;
  const double upperTechCapacity      = techCap.second;

  // store entity state information
  d_quantitys->at(step)      = transaction;
  d_marginalPrices->at(step) = marginalPrice;
  d_totalCosts->at(step)     = totalPrice;

  // exclude code as appropriate -- athough NOT passing over
  // payments is considered to be a special case
  //
  // YEEK 56 CODE (set by '--yeek')
  if ( xeona::yeek == 56 )
    {
      s_logger->repx(logga::dbug, "running nonstandard code, yeek", xeona::yeek);
      s_logger->repx(logga::yeek, "omitting update purchases/revenues", "");
    }
  else
    {
      s_logger->repx(logga::dbug, "about to update purchases/revenues", "");
      // short-run costs processing -- revenue and purchases,
      // under this arrangement, are treated as negative and
      // positive costs, respectively
      GateCom<C>::BuySide::updatePurchases(step, totalPrice / interval);
      GateCom<C>::SelSide::updateRevenues (step, totalPrice / interval);
    }

  // store some on-the-fly statistics
  d_sizeStats(upperTechCapacity);            // functor provided by class 'Block'
  d_dutyStats(d_transaction);                // functor provided by class 'Block'

  // DEBUG AND INTEGRITY CHECK CODE - as at r6980, no longer required but not causing harm

  // function name
  const std::string func = XEONA_FUNC;

  // debug code : used later if mismatch detected
  Gateway::pushDebugHxTransacts(step,             // from 'SelSide'
                                transaction,      // recovered from offer OSP (ofr)
                                d_transaction);   // earlier set by obligation OSP (ots)

  // integrity check -- a strict equality test is too limiting
  std::ostringstream oss;
  oss << transaction << " : " << d_transaction;
  // comparisons in decreasing severity
  if ( transaction == d_transaction )
    {
      // strictly equal -- and this conditional obviates the need for further tests
      s_logger->repx(logga::adhc, "transactions bitwise equal", oss.str());
    }
  else if ( xeona::almostEqual(transaction, d_transaction, xeona::numic) )
    {
      // close to equal "numeric" threshold
      s_logger->repx(logga::adhc, "transactions close to equal", oss.str());
    }
  else if ( xeona::almostEqual(transaction, d_transaction, xeona::tight) )
    {
      // more lax threshold
      s_logger->repx(logga::warn, "transaction mismatch (try yeek 32)", oss.str());
      s_logger->repx(logga::dbug, "almostEqual fail level xeona::numic", "");
    }
  else
    {
      // totally unacceptable mismatch
      s_logger->repx(logga::warn, "transaction mismatch (try yeek 32)", oss.str());
      s_logger->repx(logga::dbug, "almostEqual fail level xeona::tight", "");

      // bug hunting code -- because multiples seem to be involved
      const double a = std::abs(transaction);
      const double b = std::abs(d_transaction);
      std::ostringstream oss;
      if ( a > b ) oss << boost::format("%.8f") % (a / b);  // parentheses essential
      else         oss << boost::format("%.8f") % (b / a);

      // create report
      std::ostringstream put;
      put << "  gateway problem report"                                           << "\n"
          << "      function      : " << func                                     << "\n"
          << "      identity      : " << getIdAndKind()                           << "\n"
          << "      step          : " << step                                     << "\n"
          << "    details"                                                        << "\n"
          << "      transaction   : " << boost::format("%.8f") %   transaction    << "\n"
          << "      d_transaction : " << boost::format("%.8f") % d_transaction    << "\n"
          << "      ratio         : " << oss.str()                                << "\n";
      s_logger->putx(logga::dbug, put);

      put << "    additional reporting"                                           << "\n";
      Gateway::sayDebugHxTransacts(put);
      s_logger->putx(logga::dbug, put);

      // potential exit
      if ( xeona::nopro == false )           // meaning option '--krazy' not applied
        {
          s_logger->repx(logga::dbug, "use --krazy to omit kill", "");
          s_logger->repx(logga::kill, "significant transaction mismatch", "");
        }
      else
        {
          s_logger->repx(logga::warn, "defensive code being omitted", "");
        }
    }

  // ADDITIONAL REPORTING

  // additional reporting as appropriate
  // YEEK 32 CODE (set by '--yeek')
  if ( xeona::yeek == 32 || xeona::yeek == 1 || xeona::yeek == 2 )
    {
      // stream the data
      std::ostringstream put;
      put << "  gateway information"                                       << "\n"
          << "      gate identifier               : " << getIdAndKind()    << "\n"
          << "      function                      : " << func              << "\n"
          << "      step                          : " << step              << "\n"
          << "      call count                    : " << callCount         << "\n"
          << "    recovered"                                               << "\n"
          << "      marginal price                : " << marginalPrice     << "\n"
          << "      fixed component               : " << fixedComponent    << "\n"
          << "      variable component            : " << variableComponent << "\n"
          << "      total price                   : " << totalPrice        << "\n"
          << "      OfrTariffSet transaction      : " << transaction       << "\n"
          << "      QanTechCapacity duty          : " << duty              << "\n"
          << "    recorded"                                                << "\n"
          << "      d_transaction                 : " << d_transaction     << "\n"
          << "      d_capacity.first              : " << lowerTechCapacity << "\n"
          << "      d_capacity.second             : " << upperTechCapacity << "\n";

      // report
      s_logger->repx(logga::dbug, "additional reporting follows, yeek", xeona::yeek);
      s_logger->putx(logga::dbug, put);

      // stream more data and report
      sayStatus(put);
      s_logger->repx(logga::dbug, "additional reporting follows, yeek", xeona::yeek);
      s_logger->putx(logga::dbug, put);
    }

} // member function 'GateStatedTariff<C>::washupSelSide'

// ---------------------------------------------------------
//  MEMBER FUNCTION : sayStatus
// ---------------------------------------------------------

template <typename C>
void
GateStatedTariff<C>::sayStatus
(std::ostream& os)
{
  // function name
  const std::string func = XEONA_FUNC;

  // define a local variable for convenience
  const int step = GateCom<C>::BuySide::d_step;

  // get related information
  const std::string tafFilled  = d_tariffset->unfilled() ? "no" : "yes";
  const std::string tafLabel   = d_tariffset->getLabel();
  const double      tafBandSum = d_tariffset->getBandSum();

  // load up from below
  GateCom<C>::sayStatus(os);

  // load up here
  os << "  gateway client status report"                                          << "\n"
     << "      function                      : " << func                          << "\n"
     << "    latest values"                                                       << "\n"
     << "      defined capacity              : " << d_definedCapacitys->at(step)  << "\n"
     << "      quantity                      : " << d_quantitys->at(step)         << "\n"
     << "      marginal price                : " << d_marginalPrices->at(step)    << "\n"
     << "      total cost                    : " << d_totalCosts->at(step)        << "\n"
     << "    tariff set details"                                                  << "\n"
     << "      filled                        : " << tafFilled                     << "\n"
     << "      label                         : " << tafLabel                      << "\n"
     << "      commercial capacity           : " << tafBandSum                    << "\n";
}

// ---------------------------------------------------------
//  CLASS           : GateStatedTariffEFac <>
// ---------------------------------------------------------

// ---------------------------------------------------------
//  MEMBER FUNCTION : GateStatedTariffEFac
// ---------------------------------------------------------

template <typename C>
GateStatedTariffEFac<C>::GateStatedTariffEFac
(const std::string entityId,
 Record&           record,
 const int         commitmentModeSum) :
  Gateway(entityId, record, commitmentModeSum),   // CAUTION: virtual base class init
  GateStatedTariff<C>(entityId, record, commitmentModeSum),
  // tied quantities
  d_ghgUnitPenalty(record.tieSingle<double>("ghg-unit-penalty")),
  d_noxUnitPenalty(record.tieSingle<double>("nox-unit-penalty")),
  d_depUnitPenalty(record.tieSingle<double>("dep-unit-penalty")),
  d_lucUnitPenalty(record.tieSingle<double>("luc-unit-penalty")),
  d_totalGhgPenaltys(record.tieTimeseries<double>("total-ghg-penaltys")),
  // internal quantities
  d_dutySpecCosts(0.0)                       // zero is the standard reset value
{
  // initial reporting
  s_logger->repx(logga::adhc, "constructor call", getIdAndKind());

  // built-in remark
  GateCom<C>::d_builtinRemark = "beta";      // CAUTION: note scope resolution

  // load the duty-specific costset
  d_dutySpecCosts.ghg = d_ghgUnitPenalty;
  d_dutySpecCosts.nox = d_noxUnitPenalty;
  d_dutySpecCosts.dep = d_depUnitPenalty;
  d_dutySpecCosts.luc = d_lucUnitPenalty;

  // additional reporting as appropriate (with "%g" formatting)
  // YEEK 38 CODE (set by '--yeek')
  if ( xeona::yeek == 38 || xeona::yeek == 1 || xeona::yeek == 2 )
    {
      std::ostringstream put;
      put << d_dutySpecCosts.summarizeMeG("duty-specific penalties") << "\n";
      s_logger->repx(logga::dbug, "additional reporting follows, yeek", xeona::yeek);
      s_logger->putx(logga::dbug, put);
    }

} // function 'GateStatedTariffEFac<C>::GateStatedTariffEFac'

// ---------------------------------------------------------
//  MEMBER FUNCTION : ~GateStatedTariffEFac
// ---------------------------------------------------------

template <typename C>
GateStatedTariffEFac<C>::~GateStatedTariffEFac()
{
  // initial reporting
  s_logger->repx(logga::adhc, "destructor call", getIdAndKind());
}

// ---------------------------------------------------------
//  MEMBER FUNCTION : GateStatedTariffEFac::initializeBuySide
// ---------------------------------------------------------

template <typename C>
void
GateStatedTariffEFac<C>::initializeBuySide
(const int                  step,
 shared_ptr<svif::SolverIf> solver)
{
  // call parent class version (if this is all, then can delete
  // the overwrite entirely)
  GateStatedTariff<C>::initializeBuySide(step, solver);
}

// ---------------------------------------------------------
//  MEMBER FUNCTION : GateStatedTariffEFac::constrainBuySide
// ---------------------------------------------------------

template <typename C>
const int
GateStatedTariffEFac<C>::constrainBuySide
(const xeona::DomainMode capacityMode)
{
  // initial reporting
  s_logger->repx(logga::adhc, "entering member function", "GateStatedTariffEFac<C>");

  // call parent class version (no need to repeat that code here)
  const int buyGol = GateStatedTariff<C>::constrainBuySide(capacityMode);

  // upload specific costs -- but in this case they are penalties
  // as they do not later get transferred to cost registers
  d_capBuy->uploadShortrunCosts(d_dutySpecCosts, buyGol);

  // return the same global column
  return buyGol;

} // function 'GateStatedTariffEFac<C>::constrainSelSide'

// ---------------------------------------------------------
//  MEMBER FUNCTION : GateStatedTariffEFac::washupBuySide
// ---------------------------------------------------------

template <typename C>
void
GateStatedTariffEFac<C>::washupBuySide()
{
  // initial reporting
  s_logger->repx(logga::adhc, "entering member function", getIdAndKind());
  s_logger->repx(logga::adhc, "most tasks currently left to sel-side", "");

  // define a local variable for convenience
  const int step = GateCom<C>::BuySide::d_step;

  // recover data and store values
  double duty = -1.0;                        // nonsensical value
  boost::tie(duty) = d_capBuy->downloadSolution();

  // process and store values
  const double ghgPenalty      = d_dutySpecCosts.ghg * duty;
  d_totalGhgPenaltys->at(step) = ghgPenalty;

  // completion reporting
  s_logger->repx(logga::adhc, "leaving member function, ghgPenalty", ghgPenalty);

} // function 'GateStatedTariffEFac<C>::washupBuySide'

// ---------------------------------------------------------
//  EXPLICIT TEMPLATE INSTANTIATIONS
// ---------------------------------------------------------

// CAUTION: explicit template instantiations must be placed at
// the very end of the implementation file
//
// For more information, see the excellent documentation in
// 'recset.cc' regarding explicit template instantiations.

// Commodity typedefs for convenience

typedef CmOxidize      Oxid;
typedef CmCarbonCert   Cert;
typedef CmCarbonSeq    Cseq;
typedef CmElectricity  Elec;
typedef CmWork         Work;
typedef CmHeat         Heat;
typedef CmThermalFluid Thrm;
typedef CmFunds        Fund;

// class 'GateStatedTariff<>'

template GateStatedTariff<Oxid>::GateStatedTariff(const std::string, Record&, const int);
template GateStatedTariff<Cert>::GateStatedTariff(const std::string, Record&, const int);
template GateStatedTariff<Cseq>::GateStatedTariff(const std::string, Record&, const int);
template GateStatedTariff<Elec>::GateStatedTariff(const std::string, Record&, const int);
template GateStatedTariff<Work>::GateStatedTariff(const std::string, Record&, const int);
template GateStatedTariff<Heat>::GateStatedTariff(const std::string, Record&, const int);
template GateStatedTariff<Thrm>::GateStatedTariff(const std::string, Record&, const int);
template GateStatedTariff<Fund>::GateStatedTariff(const std::string, Record&, const int);

template GateStatedTariff<Oxid>::~GateStatedTariff();
template GateStatedTariff<Cert>::~GateStatedTariff();
template GateStatedTariff<Cseq>::~GateStatedTariff();
template GateStatedTariff<Elec>::~GateStatedTariff();
template GateStatedTariff<Work>::~GateStatedTariff();
template GateStatedTariff<Heat>::~GateStatedTariff();
template GateStatedTariff<Thrm>::~GateStatedTariff();
template GateStatedTariff<Fund>::~GateStatedTariff();

template void      GateStatedTariff<Oxid>::establish();
template void      GateStatedTariff<Cert>::establish();
template void      GateStatedTariff<Cseq>::establish();
template void      GateStatedTariff<Elec>::establish();
template void      GateStatedTariff<Work>::establish();
template void      GateStatedTariff<Heat>::establish();
template void      GateStatedTariff<Thrm>::establish();
template void      GateStatedTariff<Fund>::establish();

template const int GateStatedTariff<Oxid>::constrainBuySide(const xeona::DomainMode);
template const int GateStatedTariff<Cert>::constrainBuySide(const xeona::DomainMode);
template const int GateStatedTariff<Cseq>::constrainBuySide(const xeona::DomainMode);
template const int GateStatedTariff<Elec>::constrainBuySide(const xeona::DomainMode);
template const int GateStatedTariff<Work>::constrainBuySide(const xeona::DomainMode);
template const int GateStatedTariff<Heat>::constrainBuySide(const xeona::DomainMode);
template const int GateStatedTariff<Thrm>::constrainBuySide(const xeona::DomainMode);
template const int GateStatedTariff<Fund>::constrainBuySide(const xeona::DomainMode);

template const int GateStatedTariff<Oxid>::constrainSelSide(const xeona::DomainMode);
template const int GateStatedTariff<Cert>::constrainSelSide(const xeona::DomainMode);
template const int GateStatedTariff<Cseq>::constrainSelSide(const xeona::DomainMode);
template const int GateStatedTariff<Elec>::constrainSelSide(const xeona::DomainMode);
template const int GateStatedTariff<Work>::constrainSelSide(const xeona::DomainMode);
template const int GateStatedTariff<Heat>::constrainSelSide(const xeona::DomainMode);
template const int GateStatedTariff<Thrm>::constrainSelSide(const xeona::DomainMode);
template const int GateStatedTariff<Fund>::constrainSelSide(const xeona::DomainMode);

template void      GateStatedTariff<Oxid>::washupBuySide();
template void      GateStatedTariff<Cert>::washupBuySide();
template void      GateStatedTariff<Cseq>::washupBuySide();
template void      GateStatedTariff<Elec>::washupBuySide();
template void      GateStatedTariff<Work>::washupBuySide();
template void      GateStatedTariff<Heat>::washupBuySide();
template void      GateStatedTariff<Thrm>::washupBuySide();
template void      GateStatedTariff<Fund>::washupBuySide();

template void      GateStatedTariff<Oxid>::washupSelSide();
template void      GateStatedTariff<Cert>::washupSelSide();
template void      GateStatedTariff<Cseq>::washupSelSide();
template void      GateStatedTariff<Elec>::washupSelSide();
template void      GateStatedTariff<Work>::washupSelSide();
template void      GateStatedTariff<Heat>::washupSelSide();
template void      GateStatedTariff<Thrm>::washupSelSide();
template void      GateStatedTariff<Fund>::washupSelSide();

template void      GateStatedTariff<Oxid>::sayStatus(std::ostream&);
template void      GateStatedTariff<Cert>::sayStatus(std::ostream&);
template void      GateStatedTariff<Cseq>::sayStatus(std::ostream&);
template void      GateStatedTariff<Elec>::sayStatus(std::ostream&);
template void      GateStatedTariff<Work>::sayStatus(std::ostream&);
template void      GateStatedTariff<Heat>::sayStatus(std::ostream&);
template void      GateStatedTariff<Thrm>::sayStatus(std::ostream&);
template void      GateStatedTariff<Fund>::sayStatus(std::ostream&);

// class 'GateStatedTariffEFac<>' (just the overwritten functions)

#define GSTEF GateStatedTariffEFac           // to reduce line lengths, omit double-quotes

template GSTEF<Oxid>::GateStatedTariffEFac(const std::string, Record&, const int);
template GSTEF<Cert>::GateStatedTariffEFac(const std::string, Record&, const int);
template GSTEF<Cseq>::GateStatedTariffEFac(const std::string, Record&, const int);
template GSTEF<Elec>::GateStatedTariffEFac(const std::string, Record&, const int);
template GSTEF<Work>::GateStatedTariffEFac(const std::string, Record&, const int);
template GSTEF<Heat>::GateStatedTariffEFac(const std::string, Record&, const int);
template GSTEF<Thrm>::GateStatedTariffEFac(const std::string, Record&, const int);
template GSTEF<Fund>::GateStatedTariffEFac(const std::string, Record&, const int);

template GSTEF<Oxid>::~GateStatedTariffEFac();
template GSTEF<Cert>::~GateStatedTariffEFac();
template GSTEF<Cseq>::~GateStatedTariffEFac();
template GSTEF<Elec>::~GateStatedTariffEFac();
template GSTEF<Work>::~GateStatedTariffEFac();
template GSTEF<Heat>::~GateStatedTariffEFac();
template GSTEF<Thrm>::~GateStatedTariffEFac();
template GSTEF<Fund>::~GateStatedTariffEFac();

template void      GSTEF<Oxid>::initializeBuySide(const int, shared_ptr<svif::SolverIf>);
template void      GSTEF<Cert>::initializeBuySide(const int, shared_ptr<svif::SolverIf>);
template void      GSTEF<Cseq>::initializeBuySide(const int, shared_ptr<svif::SolverIf>);
template void      GSTEF<Elec>::initializeBuySide(const int, shared_ptr<svif::SolverIf>);
template void      GSTEF<Work>::initializeBuySide(const int, shared_ptr<svif::SolverIf>);
template void      GSTEF<Heat>::initializeBuySide(const int, shared_ptr<svif::SolverIf>);
template void      GSTEF<Thrm>::initializeBuySide(const int, shared_ptr<svif::SolverIf>);
template void      GSTEF<Fund>::initializeBuySide(const int, shared_ptr<svif::SolverIf>);

template const int GSTEF<Oxid>::constrainBuySide(const xeona::DomainMode);
template const int GSTEF<Cert>::constrainBuySide(const xeona::DomainMode);
template const int GSTEF<Cseq>::constrainBuySide(const xeona::DomainMode);
template const int GSTEF<Elec>::constrainBuySide(const xeona::DomainMode);
template const int GSTEF<Work>::constrainBuySide(const xeona::DomainMode);
template const int GSTEF<Heat>::constrainBuySide(const xeona::DomainMode);
template const int GSTEF<Thrm>::constrainBuySide(const xeona::DomainMode);
template const int GSTEF<Fund>::constrainBuySide(const xeona::DomainMode);

template void      GSTEF<Oxid>::washupBuySide();
template void      GSTEF<Cert>::washupBuySide();
template void      GSTEF<Cseq>::washupBuySide();
template void      GSTEF<Elec>::washupBuySide();
template void      GSTEF<Work>::washupBuySide();
template void      GSTEF<Heat>::washupBuySide();
template void      GSTEF<Thrm>::washupBuySide();
template void      GSTEF<Fund>::washupBuySide();

#undef GSTEF                                 // was "GateStatedTariffEFac"

// ---------------------------------
//  derived instantiations
// ---------------------------------

// for convenience

typedef CmOxidGas        OGas;
typedef CmOxidNaturalGas NatG;
typedef CmOxidBiocoal    BioC;
typedef CmOxidHydrogen   Htwo;

// class 'GateStatedTariff<>'

template GateStatedTariff<OGas>::GateStatedTariff(const std::string, Record&, const int);
template GateStatedTariff<NatG>::GateStatedTariff(const std::string, Record&, const int);
template GateStatedTariff<BioC>::GateStatedTariff(const std::string, Record&, const int);
template GateStatedTariff<Htwo>::GateStatedTariff(const std::string, Record&, const int);

template GateStatedTariff<OGas>::~GateStatedTariff();
template GateStatedTariff<NatG>::~GateStatedTariff();
template GateStatedTariff<BioC>::~GateStatedTariff();
template GateStatedTariff<Htwo>::~GateStatedTariff();

template void      GateStatedTariff<OGas>::establish();
template void      GateStatedTariff<NatG>::establish();
template void      GateStatedTariff<BioC>::establish();
template void      GateStatedTariff<Htwo>::establish();

template const int GateStatedTariff<OGas>::constrainBuySide(const xeona::DomainMode);
template const int GateStatedTariff<NatG>::constrainBuySide(const xeona::DomainMode);
template const int GateStatedTariff<BioC>::constrainBuySide(const xeona::DomainMode);
template const int GateStatedTariff<Htwo>::constrainBuySide(const xeona::DomainMode);

template const int GateStatedTariff<OGas>::constrainSelSide(const xeona::DomainMode);
template const int GateStatedTariff<NatG>::constrainSelSide(const xeona::DomainMode);
template const int GateStatedTariff<BioC>::constrainSelSide(const xeona::DomainMode);
template const int GateStatedTariff<Htwo>::constrainSelSide(const xeona::DomainMode);

template void      GateStatedTariff<OGas>::washupBuySide();
template void      GateStatedTariff<NatG>::washupBuySide();
template void      GateStatedTariff<BioC>::washupBuySide();
template void      GateStatedTariff<Htwo>::washupBuySide();

template void      GateStatedTariff<OGas>::washupSelSide();
template void      GateStatedTariff<NatG>::washupSelSide();
template void      GateStatedTariff<BioC>::washupSelSide();
template void      GateStatedTariff<Htwo>::washupSelSide();

template void      GateStatedTariff<OGas>::sayStatus(std::ostream&);
template void      GateStatedTariff<NatG>::sayStatus(std::ostream&);
template void      GateStatedTariff<BioC>::sayStatus(std::ostream&);
template void      GateStatedTariff<Htwo>::sayStatus(std::ostream&);

// class 'GateStatedTariffEFac<>' (just the overwritten functions)

#define GSTEF GateStatedTariffEFac           // to reduce line lengths, omit double-quotes

template GSTEF<OGas>::GateStatedTariffEFac(const std::string, Record&, const int);
template GSTEF<NatG>::GateStatedTariffEFac(const std::string, Record&, const int);
template GSTEF<BioC>::GateStatedTariffEFac(const std::string, Record&, const int);
template GSTEF<Htwo>::GateStatedTariffEFac(const std::string, Record&, const int);

template GSTEF<OGas>::~GateStatedTariffEFac();
template GSTEF<NatG>::~GateStatedTariffEFac();
template GSTEF<BioC>::~GateStatedTariffEFac();
template GSTEF<Htwo>::~GateStatedTariffEFac();

template void      GSTEF<OGas>::initializeBuySide(const int, shared_ptr<svif::SolverIf>);
template void      GSTEF<NatG>::initializeBuySide(const int, shared_ptr<svif::SolverIf>);
template void      GSTEF<BioC>::initializeBuySide(const int, shared_ptr<svif::SolverIf>);
template void      GSTEF<Htwo>::initializeBuySide(const int, shared_ptr<svif::SolverIf>);

template const int GSTEF<OGas>::constrainBuySide(const xeona::DomainMode);
template const int GSTEF<NatG>::constrainBuySide(const xeona::DomainMode);
template const int GSTEF<BioC>::constrainBuySide(const xeona::DomainMode);
template const int GSTEF<Htwo>::constrainBuySide(const xeona::DomainMode);

template void      GSTEF<OGas>::washupBuySide();
template void      GSTEF<NatG>::washupBuySide();
template void      GSTEF<BioC>::washupBuySide();
template void      GSTEF<Htwo>::washupBuySide();

#undef GSTEF                                 // was "GateStatedTariffEFac"

// define a preprocessor macro for convenience when making
// explicit template instantiations
//
// CAUTION: Lischner (2003 p276) says "A backslash (\) at the end
// of the line continues the directive onto the subsequent line."

#define ARGS_1 \
shared_ptr<svif::SolverIf>, \
const xeona::DomainMode, \
const xeona::DomainMode, \
const std::string

// EXPLANATION: at one point, long template instantiations were
// shortened using this macro -- this facility, however, is no
// longer needed but the macro code remains for the time being

// macro no longer needed!
#undef ARGS_1 // see this file

//  end of file

