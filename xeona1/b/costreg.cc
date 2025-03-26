//  XEONA ENERGY SYSTEMS MODELING ENVIRONMENT
//
//  file-create-name : costreg.cc
//  file-create-date : Mon 20-Oct-2008 08:57 UTC
//  file-initiator   : Robbie Morrison <robbie@actrix.co.nz>
//
//  file-role        : cost registers / implementation
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
//  $URL: file:///home/robbie/svn-root/xeona/futz/trunk/xeona1/b/costreg.cc $
//
//  GENERAL NOTES FOR THIS FILE

//  LOCAL AND SYSTEM INCLUDES

#include "costreg.h"          // companion header for this file (place first)

#include "../c/recset.h"      // records and fields and also record-sets
#include "../c/fincalc.h"     // support for discounted cash flow analysis
#include "../c/costs.h"       // cost sets and support

#include "../a/logger.h"      // standard logging functionality (as required)
#include "../c/smart_ptr.h"   // toggle between Boost and TR1 smart pointers
#include ".././common.h"      // common definitions for project (place last)

#include <string>             // C++ strings
#include <sstream>            // string-streams

#include <boost/format.hpp>   // printf style formatting

//  CODE

// STATIC DEFINITIONS

logga::spLogger CostRegister::s_logger_costreg = logga::ptrLogStream();  // bind

// ---------------------------------------------------------
//  CLASS           : CostRegister
// ---------------------------------------------------------

// ---------------------------------------------------------
//  MEMBER FUNCTION : CostRegister
// ---------------------------------------------------------

CostRegister::CostRegister
(Record& record) :
  d_reset(0.0),                              // default set here
  d_interval(Entity::getHorizonInterval()),
  d_horizon(Entity::getHorizonSteps()),
  d_dutySpecCosts(d_reset),
  d_sizeSpecCosts(d_reset),
  d_standingCosts(d_reset),
  d_varFins(new std::vector<double>(d_horizon, d_reset)),
  d_fixFins(new std::vector<double>(d_horizon, d_reset)),
  d_embFins(new std::vector<double>(d_horizon, d_reset)),
  d_varGhgs(new std::vector<double>(d_horizon, d_reset)),
  d_fixGhgs(new std::vector<double>(d_horizon, d_reset)),
  d_embGhgs(new std::vector<double>(d_horizon, d_reset)),
  d_varNoxs(new std::vector<double>(d_horizon, d_reset)),
  d_fixNoxs(new std::vector<double>(d_horizon, d_reset)),
  d_embNoxs(new std::vector<double>(d_horizon, d_reset)),
  d_varDeps(new std::vector<double>(d_horizon, d_reset)),
  d_fixDeps(new std::vector<double>(d_horizon, d_reset)),
  d_embDeps(new std::vector<double>(d_horizon, d_reset)),
  d_varLucs(new std::vector<double>(d_horizon, d_reset)),
  d_fixLucs(new std::vector<double>(d_horizon, d_reset)),
  d_embLucs(new std::vector<double>(d_horizon, d_reset))

//d_revenues(new std::vector<double>(d_horizon, d_reset))  // intentionally disabled
{
  // initial reporting
  s_logger_costreg->repx(logga::dbug, "constructor call, reset value", d_reset);

  // defensive programming -- worth testing but should never fail
  if ( d_horizon < 2 )
    {
      s_logger_costreg->repx(logga::warn, "horizon problem", d_horizon);
    }
}

// ---------------------------------------------------------
//  MEMBER FUNCTION : ~CostRegister
// ---------------------------------------------------------

CostRegister::~CostRegister()
{
  s_logger_costreg->repx(logga::dbug, "destructor call", "");
}

// ---------------------------------------------------------
//  MEMBER FUNCTION : updateShortrunCosts
// ---------------------------------------------------------
//  Description  : update all shortrun costs
//  Role         : typically used in technical asset washup calls
//  Techniques   : principal code (other calls are wrappers)
//  Status       : complete
// ---------------------------------------------------------

void
CostRegister::updateShortrunCosts
(const int      step,
 const CostSet& varCosts,
 const CostSet& fixCosts)
{
  // recovery and loading
  d_varFins->at(step) = varCosts.fin * d_interval;
  d_fixFins->at(step) = fixCosts.fin * d_interval;
  d_varGhgs->at(step) = varCosts.ghg * d_interval;
  d_fixGhgs->at(step) = fixCosts.ghg * d_interval;
  d_varNoxs->at(step) = varCosts.nox * d_interval;
  d_fixNoxs->at(step) = fixCosts.nox * d_interval;
  d_varDeps->at(step) = varCosts.dep * d_interval;
  d_fixDeps->at(step) = fixCosts.dep * d_interval;
  d_varLucs->at(step) = varCosts.luc * d_interval;
  d_fixLucs->at(step) = fixCosts.luc * d_interval;
}

// ---------------------------------------------------------
//  MEMBER FUNCTION : updateRevenues
// ---------------------------------------------------------

void
CostRegister::updateRevenues
(const int    step,
 const double revenues)                           // treated as negative costs
{
  // initial reporting
  s_logger_costreg->repx(logga::adhc, "entering member function", "");

  // integrity check
  if( revenues < 0.0 )
    {
      s_logger_costreg->repx(logga::rankJumpy, "negative revenues submitted", revenues);
    }

  // calculations
  d_varFins->at(step) += -revenues * d_interval;  // note the unary negative
}

// ---------------------------------------------------------
//  MEMBER FUNCTION : updatePurchases
// ---------------------------------------------------------

void
CostRegister::updatePurchases
(const int    step,
 const double purchases)                          // treated as positive costs
{
  // initial reporting
  s_logger_costreg->repx(logga::adhc, "entering member function", "");

  // integrity check
  if( purchases < 0.0 )
    {
      s_logger_costreg->repx(logga::rankJumpy, "negative purchase submitted", purchases);
    }

  // calculations
  d_varFins->at(step) += +purchases * d_interval; // note the unary positive
}

// ---------------------------------------------------------
//  MEMBER FUNCTION : updateEmbeddedCosts (external)
// ---------------------------------------------------------

void
CostRegister::updateEmbeddedCosts
(const int      step,
 const CostSet& emb)
{
  // initial reporting
  s_logger_costreg->repx(logga::adhc, "entering member function", "");

  // calculations
  d_varFins->at(step) = emb.fin * d_interval;
  d_varGhgs->at(step) = emb.ghg * d_interval;
  d_varNoxs->at(step) = emb.nox * d_interval;
  d_varDeps->at(step) = emb.dep * d_interval;
  d_varLucs->at(step) = emb.luc * d_interval;
}

// ---------------------------------------------------------
//  MEMBER FUNCTION : updateEmbeddedCosts (calculated)
// ---------------------------------------------------------

// the more specialized versions apply if the more specialized
// classes have been inherited -- hence the null calculations are
// fine

void
CostRegister::updateEmbeddedCosts
(const int step)
{
  // undertake null calculations in any case
  d_varFins->at(step) = 0.0 * d_interval;
  d_varGhgs->at(step) = 0.0 * d_interval;
  d_varNoxs->at(step) = 0.0 * d_interval;
  d_varDeps->at(step) = 0.0 * d_interval;
  d_varLucs->at(step) = 0.0 * d_interval;
}

// ---------------------------------------------------------
//  MEMBER FUNCTION : resetRegister
// ---------------------------------------------------------
//  Description  : reset all tied standard cost vectors
//  Role         : standard usage
//  Techniques   : 'std::vector' member functions
//  Status       : complete
//
//  Design notes
//
//      The given code tests for tied timeseries and then resets
//      these to 'd_reset' (zero by default).  Note that a prior
//      'clear' call not needed.
//
//      Under g++, the 'std::vector<>::size_type' returned by
//      'std::vector' member function 'size' is 'unsigned'.
//
//      From commit r4913, this call is now considered part of
//      standard usage.  Prior to r4910, it was tagged
//      developmental.
//
//  CAUTION: operates entire horizon
//
//      This function nukes the entire horizon and not just the
//      current 'step'.
//
// ---------------------------------------------------------

void
CostRegister::resetRegister()
{
  s_logger_costreg->repx(logga::dbug, "entering member function, reset to", d_reset);

  // member function 'void std::vector<T>::assign(size_type n,
  // const T& value)' "erases all the elements of the vector,
  // then inserts 'n' copies of 'value'" (Lischner 2003 p724)
  //
  // note also the protection against empty/null shared pointers

  d_varFins->assign(d_varFins->size(), d_reset);
  d_fixFins->assign(d_fixFins->size(), d_reset);
  d_embFins->assign(d_embFins->size(), d_reset);
  d_varGhgs->assign(d_varGhgs->size(), d_reset);
  d_fixGhgs->assign(d_fixGhgs->size(), d_reset);
  d_embGhgs->assign(d_embGhgs->size(), d_reset);
  d_varNoxs->assign(d_varNoxs->size(), d_reset);
  d_fixNoxs->assign(d_fixNoxs->size(), d_reset);
  d_embNoxs->assign(d_embNoxs->size(), d_reset);
  d_varDeps->assign(d_varDeps->size(), d_reset);
  d_fixDeps->assign(d_fixDeps->size(), d_reset);
  d_embDeps->assign(d_embDeps->size(), d_reset);
  d_varLucs->assign(d_varLucs->size(), d_reset);
  d_fixLucs->assign(d_fixLucs->size(), d_reset);
  d_embLucs->assign(d_embLucs->size(), d_reset);
}

// ---------------------------------------------------------
//  MEMBER FUNCTION : importCosts
// ---------------------------------------------------------

void
CostRegister::importCosts
(const int step,
 const boost::tuple
 <CostSet,                                   // accrued "var" variable costs
  CostSet,                                   // accrued "fix" fixed costs
  CostSet>& costs)                           // accrued "emb" embedded costs,
{                                            // pass-by-ref required
  // initial reporting
  s_logger_costreg->repx(logga::adhc, "entering member function", "");

  // calculations
  CostSet var = costs.get<0>();              // CAUTION: zero-based tuple indexing
  CostSet fix = costs.get<1>();
  CostSet emb = costs.get<2>();

  d_varFins->at(step) += var.fin;
  d_fixFins->at(step) += fix.fin;
  d_embFins->at(step) += emb.fin;
  d_varGhgs->at(step) += var.ghg;
  d_fixGhgs->at(step) += fix.ghg;
  d_embGhgs->at(step) += emb.ghg;
  d_varNoxs->at(step) += var.nox;
  d_fixNoxs->at(step) += fix.nox;
  d_embNoxs->at(step) += emb.nox;
  d_varDeps->at(step) += var.dep;
  d_fixDeps->at(step) += fix.dep;
  d_embDeps->at(step) += emb.dep;
  d_varLucs->at(step) += var.luc;
  d_fixLucs->at(step) += fix.luc;
  d_embLucs->at(step) += emb.luc;
}

// ---------------------------------------------------------
//  MEMBER FUNCTION : exportCosts
// ---------------------------------------------------------

boost::tuple
<CostSet,
 CostSet,
 CostSet>
CostRegister::exportCosts
(const int step) const
{
  // initial reporting
  s_logger_costreg->repx(logga::adhc, "entering member function", "");

  // calculations
  double varFin = d_reset; varFin = d_varFins->at(step);
  double varGhg = d_reset; varGhg = d_varGhgs->at(step);
  double varNox = d_reset; varNox = d_varNoxs->at(step);
  double varDep = d_reset; varDep = d_varDeps->at(step);
  double varLuc = d_reset; varLuc = d_varLucs->at(step);

  double fixFin = d_reset; fixFin = d_fixFins->at(step);
  double fixGhg = d_reset; fixGhg = d_fixGhgs->at(step);
  double fixNox = d_reset; fixNox = d_fixNoxs->at(step);
  double fixDep = d_reset; fixDep = d_fixDeps->at(step);
  double fixLuc = d_reset; fixLuc = d_fixLucs->at(step);

  double embFin = d_reset; embFin = d_embFins->at(step);
  double embGhg = d_reset; embGhg = d_embGhgs->at(step);
  double embNox = d_reset; embNox = d_embNoxs->at(step);
  double embDep = d_reset; embDep = d_embDeps->at(step);
  double embLuc = d_reset; embLuc = d_embLucs->at(step);

  return boost::make_tuple                   // tuple holds copies of its args
    (CostSet(varFin, varGhg, varNox, varDep, varLuc),
     CostSet(fixFin, fixGhg, fixNox, fixDep, fixLuc),
     CostSet(embFin, embGhg, embNox, embDep, embLuc));
}

// ---------------------------------------------------------
//  MEMBER FUNCTION : consolidateCosts
// ---------------------------------------------------------

// CAUTION: not currently deployed at the time of writing

void
CostRegister::consolidateCosts
(const int                      step,
 const shared_ptr<CostRegister> entity)      // normally a subsidiary entity
{
  // initial reporting
  s_logger_costreg->repx(logga::adhc, "entering member function", "");

  // calculations
  importCosts(step, entity->exportCosts(step));
}

// ---------------------------------------------------------
//  CLASS           : CostRegisterOverseer
// ---------------------------------------------------------

// ---------------------------------------------------------
//  MEMBER FUNCTION : CostRegisterOverseer
// ---------------------------------------------------------

CostRegisterOverseer::CostRegisterOverseer
(Record& record) :
  CostRegister(record),
  d_totalFin(record.tieSingle<double>("total-financial")),
  d_totalGhg(record.tieSingle<double>("total-greenhouse")),
  d_totalNox(record.tieSingle<double>("total-nox")),
  d_totalDep(record.tieSingle<double>("total-depletion")),
  d_totalLuc(record.tieSingle<double>("total-landuse")),
  d_totalShortrunFin(record.tieSingle<double>("total-shortrun-financial")),
  d_totalShortrunGhg(record.tieSingle<double>("total-shortrun-greenhouse")),
  d_totalShortrunNox(record.tieSingle<double>("total-shortrun-nox")),
  d_totalShortrunDep(record.tieSingle<double>("total-shortrun-depletion")),
  d_totalShortrunLuc(record.tieSingle<double>("total-shortrun-landuse"))
{
  // initial reporting
  s_logger_costreg->repx(logga::dbug, "constructor call", "");

  // reset totals
  d_totalFin         = d_reset;
  d_totalGhg         = d_reset;
  d_totalNox         = d_reset;
  d_totalDep         = d_reset;
  d_totalLuc         = d_reset;
  d_totalShortrunFin = d_reset;
  d_totalShortrunGhg = d_reset;
  d_totalShortrunNox = d_reset;
  d_totalShortrunDep = d_reset;
  d_totalShortrunLuc = d_reset;

  // tie inherited quantities in the constructor (as opposed to
  // the member initialization list)
  d_varFins = record.tieTimeseries<double>("variable-costs-financial");
  d_fixFins = record.tieTimeseries<double>("fixed-costs-financial");
  d_embFins = record.tieTimeseries<double>("embedded-costs-financial");
  d_varGhgs = record.tieTimeseries<double>("variable-costs-greenhouse");
  d_fixGhgs = record.tieTimeseries<double>("fixed-costs-greenhouse");
  d_embGhgs = record.tieTimeseries<double>("embedded-costs-greenhouse");
  d_varNoxs = record.tieTimeseries<double>("variable-costs-nox");
  d_fixNoxs = record.tieTimeseries<double>("fixed-costs-nox");
  d_embNoxs = record.tieTimeseries<double>("embedded-costs-nox");
  d_varDeps = record.tieTimeseries<double>("variable-costs-depletion");
  d_fixDeps = record.tieTimeseries<double>("fixed-costs-depletion");
  d_embDeps = record.tieTimeseries<double>("embedded-costs-depletion");
  d_varLucs = record.tieTimeseries<double>("variable-costs-landuse");
  d_fixLucs = record.tieTimeseries<double>("fixed-costs-landuse");
  d_embLucs = record.tieTimeseries<double>("embedded-costs-landuse");

} // function 'CostRegisterOverseer::CostRegisterOverseer'

// ---------------------------------------------------------
//  MEMBER FUNCTION : ~CostRegisterOverseer
// ---------------------------------------------------------

CostRegisterOverseer::~CostRegisterOverseer()
{
  s_logger_costreg->repx(logga::dbug, "destructor call", "");
}

// ---------------------------------------------------------
//  MEMBER FUNCTION : importCosts
// ---------------------------------------------------------

void
CostRegisterOverseer::importCosts            // based on "add-and-assign" operator
(const int step,
 const boost::tuple
 <CostSet,                                   // accrued "var" variable costs
  CostSet,                                   // accrued "fix" fixed costs
  CostSet>& costs)                           // accrued "emb" embedded costs,
{                                            // pass-by-ref required
  // recovery
  CostSet var = costs.get<0>();              // CAUTION: zero-based tuple indexing
  CostSet fix = costs.get<1>();
  CostSet emb = costs.get<2>();

  // update
  d_varFins->at(step) += var.fin;
  d_fixFins->at(step) += fix.fin;
  d_embFins->at(step) += emb.fin;
  d_varGhgs->at(step) += var.ghg;
  d_fixGhgs->at(step) += fix.ghg;
  d_embGhgs->at(step) += emb.ghg;
  d_varNoxs->at(step) += var.nox;
  d_fixNoxs->at(step) += fix.nox;
  d_embNoxs->at(step) += emb.nox;
  d_varDeps->at(step) += var.dep;
  d_fixDeps->at(step) += fix.dep;
  d_embDeps->at(step) += emb.dep;
  d_varLucs->at(step) += var.luc;
  d_fixLucs->at(step) += fix.luc;
  d_embLucs->at(step) += emb.luc;

  // CAUTION: 'operator+=' of lower precedence than 'operator+'
  d_totalFin += var.fin + fix.fin + emb.fin;
  d_totalGhg += var.ghg + fix.ghg + emb.ghg;
  d_totalNox += var.nox + fix.nox + emb.nox;
  d_totalDep += var.dep + fix.dep + emb.dep;
  d_totalLuc += var.luc + fix.luc + emb.luc;

  d_totalShortrunFin += var.fin + fix.fin;
  d_totalShortrunGhg += var.ghg + fix.ghg;
  d_totalShortrunNox += var.nox + fix.nox;
  d_totalShortrunDep += var.dep + fix.dep;
  d_totalShortrunLuc += var.luc + fix.luc;

} // function 'CostRegisterOverseer::importCosts'

// ---------------------------------------------------------
//  FREE FUNCTION   : ::fmtCost
// ---------------------------------------------------------

namespace
{
  std::string
  fmtCost
  (const int          indent,                // usually 2
   const int          tab,                   // at least the longest 'description'
   const std::string& description,
   const double       value)
  {
    const std::string pad(indent, ' ');
    std::ostringstream fmt;                  // boost format string
    fmt << "%-" << tab << "s"                // description
        << " : "                             // colon
        << "%30.10f"                         // value
        << "    ";                           // separator
    if ( value == 0.0 ) fmt <<  "%9.2g";     // simple zero with 'g'
    else                fmt << "%+9.2e";     // value
    std::ostringstream oss;                  // to be returned as string
    oss << pad << boost::format(fmt.str()) % description % value % value;
    return oss.str();
  }

} // unnamed namespace

// ---------------------------------------------------------
//  MEMBER FUNCTION : highPrecisionReport
// ---------------------------------------------------------

std::string
CostRegisterOverseer::highPrecisionReport
(const int indent) const
{
  const int tab = 29;                        // description space
  const std::string pad(indent, ' ');

  std::ostringstream oss;                    // to be returned as string

  oss << pad << "high precision report for overseer final values"                << "\n";
  oss << pad << "function : " << XEONA_FUNC                                      << "\n";
  oss                                                                            << "\n";
  oss << ::fmtCost(indent, tab, "total financial",           d_totalFin)         << "\n";
  oss << ::fmtCost(indent, tab, "total greenhouse",          d_totalGhg)         << "\n";
  oss << ::fmtCost(indent, tab, "total nox",                 d_totalNox)         << "\n";
  oss << ::fmtCost(indent, tab, "total depletion",           d_totalDep)         << "\n";
  oss << ::fmtCost(indent, tab, "total land usage",          d_totalLuc)         << "\n";
  oss                                                                            << "\n";

  oss << ::fmtCost(indent, tab, "total shortrun financial",  d_totalShortrunFin) << "\n";
  oss << ::fmtCost(indent, tab, "total shortrun greenhouse", d_totalShortrunGhg) << "\n";
  oss << ::fmtCost(indent, tab, "total shortrun nox",        d_totalShortrunNox) << "\n";
  oss << ::fmtCost(indent, tab, "total shortrun depletion",  d_totalShortrunDep) << "\n";
  oss << ::fmtCost(indent, tab, "total shortrun land usage", d_totalShortrunLuc) << "\n";
  oss                                                                            << "\n";

  const double difFin = d_totalFin - d_totalShortrunFin;
  const double difGhg = d_totalGhg - d_totalShortrunGhg;
  const double difNox = d_totalNox - d_totalShortrunNox;
  const double difDep = d_totalDep - d_totalShortrunDep;
  const double difLuc = d_totalLuc - d_totalShortrunLuc;

  oss << ::fmtCost(indent, tab, "total difference financial",  difFin)           << "\n";
  oss << ::fmtCost(indent, tab, "total difference greenhouse", difGhg)           << "\n";
  oss << ::fmtCost(indent, tab, "total difference nox",        difNox)           << "\n";
  oss << ::fmtCost(indent, tab, "total difference depletion",  difDep)           << "\n";
  oss << ::fmtCost(indent, tab, "total difference land usage", difLuc)           << "\n";

  return oss.str();

} // function 'CostRegisterOverseer::highPrecisionReport'

// ---------------------------------------------------------
//  CLASS           : CostRegisterDomcon
// ---------------------------------------------------------

// ---------------------------------------------------------
//  MEMBER FUNCTION : CostRegisterDomcon
// ---------------------------------------------------------

CostRegisterDomcon::CostRegisterDomcon
(Record& record) :
  CostRegister(record),
  d_subtotalFin(record.tieSingle<double>("subtotal-financial")),
  d_subtotalGhg(record.tieSingle<double>("subtotal-greenhouse")),
  d_subtotalNox(record.tieSingle<double>("subtotal-nox")),
  d_subtotalDep(record.tieSingle<double>("subtotal-depletion")),
  d_subtotalLuc(record.tieSingle<double>("subtotal-landuse"))
{
  // initial reporting
  s_logger_costreg->repx(logga::dbug, "constructor call", "");

  // reset totals
  d_subtotalFin = d_reset;
  d_subtotalGhg = d_reset;
  d_subtotalNox = d_reset;
  d_subtotalDep = d_reset;
  d_subtotalLuc = d_reset;

  // tie some inherited quantities in the constructor (as opposed
  // to the member initialization list)
  d_varFins = record.tieTimeseries<double>("variable-costs-financial");
  d_fixFins = record.tieTimeseries<double>("fixed-costs-financial");
}

// ---------------------------------------------------------
//  MEMBER FUNCTION : ~CostRegisterDomcon
// ---------------------------------------------------------

CostRegisterDomcon::~CostRegisterDomcon()
{
  s_logger_costreg->repx(logga::dbug, "destructor call", "");
}

// ---------------------------------------------------------
//  MEMBER FUNCTION : updateMySubtotals
// ---------------------------------------------------------
//  Description  : update my subtotals
//  Role         : final call function 'DomainController::consolidateDomain'
//  Techniques   : (nothing special)
//  Status       : complete
// ---------------------------------------------------------

void
CostRegisterDomcon::updateMySubtotals
(const int step)                             // horizon step
{
  // CAUTION: 'operator+=' of lower precedence than 'operator+'
  d_subtotalFin += d_varFins->at(step) + d_fixFins->at(step) + d_embFins->at(step);
  d_subtotalGhg += d_varGhgs->at(step) + d_fixGhgs->at(step) + d_embGhgs->at(step);
  d_subtotalNox += d_varNoxs->at(step) + d_fixNoxs->at(step) + d_embNoxs->at(step);
  d_subtotalDep += d_varDeps->at(step) + d_fixDeps->at(step) + d_embDeps->at(step);
  d_subtotalLuc += d_varLucs->at(step) + d_fixLucs->at(step) + d_embLucs->at(step);
}

// ---------------------------------------------------------
//  CLASS           : CostRegisterAsop
// ---------------------------------------------------------

// ---------------------------------------------------------
//  MEMBER FUNCTION : CostRegisterAsop
// ---------------------------------------------------------

CostRegisterAsop::CostRegisterAsop
(Record& record) :
  CostRegister(record),
  d_standingFin(record.tieSingle<double>("standing-cost-financial")),
  d_standingGhg(record.tieSingle<double>("standing-cost-greenhouse")),
  d_standingNox(record.tieSingle<double>("standing-cost-nox")),
  d_standingDep(record.tieSingle<double>("standing-cost-depletion")),
  d_standingLuc(record.tieSingle<double>("standing-cost-landuse"))
{
  // initial reporting
  s_logger_costreg->repx(logga::dbug, "constructor call", "");

  // tie some inherited quantities in the constructor (as opposed
  // to the member initialization list)
  d_varFins = record.tieTimeseries<double>("variable-costs-financial");
  d_fixFins = record.tieTimeseries<double>("fixed-costs-financial");

  // load the cost set variants
  d_standingCosts.fin = d_standingFin;
  d_standingCosts.ghg = d_standingGhg;
  d_standingCosts.nox = d_standingNox;
  d_standingCosts.dep = d_standingDep;
  d_standingCosts.luc = d_standingLuc;
}

// ---------------------------------------------------------
//  MEMBER FUNCTION : ~CostRegisterAsop
// ---------------------------------------------------------

CostRegisterAsop::~CostRegisterAsop()
{
  s_logger_costreg->repx(logga::dbug, "destructor call", "");
}

// ---------------------------------------------------------
//  MEMBER FUNCTION : updateStandingCosts
// ---------------------------------------------------------

// CAUTION: do nothing with 'd_varFins' is correct

void
CostRegisterAsop::updateStandingCosts
(const int step)
{
  // warning
  s_logger_costreg->repx(logga::rankJumpy, "function need checking", "");

  // calculations
  d_fixFins->at(step) = d_standingCosts.fin * d_interval;
  d_fixGhgs->at(step) = d_standingCosts.ghg * d_interval;
  d_fixNoxs->at(step) = d_standingCosts.nox * d_interval;
  d_fixDeps->at(step) = d_standingCosts.dep * d_interval;
  d_fixLucs->at(step) = d_standingCosts.luc * d_interval;
}

// ---------------------------------------------------------
//  CLASS           : CostRegisterSRFin
// ---------------------------------------------------------

// ---------------------------------------------------------
//  MEMBER FUNCTION : CostRegisterSRFin
// ---------------------------------------------------------

CostRegisterSRFin::CostRegisterSRFin
(Record& record) :
  CostRegister(record),
  d_nameplateSize(record.tieSingle<double>("nameplate-capacity")),
  d_dutySpecFin(record.tieSingle<double>("duty-specific-cost-financial")),
  d_sizeSpecFin(record.tieSingle<double>("size-specific-cost-financial")),
  d_standingFin(record.tieSingle<double>("standing-cost-financial"))
{
  // initial reporting
  s_logger_costreg->repx(logga::dbug, "constructor call", "");

  // tie some inherited quantities in the constructor (as opposed
  // to the member initialization list)
  d_varFins = record.tieTimeseries<double>("variable-costs-financial");
  d_fixFins = record.tieTimeseries<double>("fixed-costs-financial");

  // load the cost set variants
  d_dutySpecCosts.fin = d_dutySpecFin;
  d_sizeSpecCosts.fin = d_sizeSpecFin;
  d_standingCosts.fin = d_standingFin;
}

// ---------------------------------------------------------
//  MEMBER FUNCTION : ~CostRegisterSRFin
// ---------------------------------------------------------

CostRegisterSRFin::~CostRegisterSRFin()
{
  s_logger_costreg->repx(logga::dbug, "destructor call", "");
}

// MANIPULATORS

// ---------------------------------------------------------
//  MEMBER FUNCTION : updateShortrunCosts
// ---------------------------------------------------------
//  Description  : update all shortrun costs
//  Role         : typically used in technical asset washup calls
//  Techniques   : wrapper to avoid fully resolving the call in client code
//  Status       : complete
// ---------------------------------------------------------

void
CostRegisterSRFin::updateShortrunCosts
(const int      step,
 const CostSet& varCosts,
 const CostSet& fixCosts)
{
  // wrapper
  CostRegister::updateShortrunCosts(step, varCosts, fixCosts);
}

// ---------------------------------------------------------
//  CLASS           : CostRegisterSRAll
// ---------------------------------------------------------

// ---------------------------------------------------------
//  MEMBER FUNCTION : CostRegisterSRAll
// ---------------------------------------------------------

CostRegisterSRAll::CostRegisterSRAll
(Record& record) :
  CostRegister(record),
  CostRegisterSRFin(record),
  d_dutySpecGhg(record.tieSingle<double>("duty-specific-cost-greenhouse")),
  d_sizeSpecGhg(record.tieSingle<double>("size-specific-cost-greenhouse")),
  d_standingGhg(record.tieSingle<double>("standing-cost-greenhouse")),
  d_dutySpecNox(record.tieSingle<double>("duty-specific-cost-nox")),
  d_sizeSpecNox(record.tieSingle<double>("size-specific-cost-nox")),
  d_standingNox(record.tieSingle<double>("standing-cost-nox")),
  d_dutySpecDep(record.tieSingle<double>("duty-specific-cost-depletion")),
  d_sizeSpecDep(record.tieSingle<double>("size-specific-cost-depletion")),
  d_standingDep(record.tieSingle<double>("standing-cost-depletion")),
  d_dutySpecLuc(record.tieSingle<double>("duty-specific-cost-landuse")),
  d_sizeSpecLuc(record.tieSingle<double>("size-specific-cost-landuse")),
  d_standingLuc(record.tieSingle<double>("standing-cost-landuse"))
{
  // initial reporting
  s_logger_costreg->repx(logga::dbug, "constructor call", "");

  // tie some inherited quantities in the constructor (as opposed
  // to the member initialization list)
  d_varGhgs = record.tieTimeseries<double>("variable-costs-greenhouse");
  d_fixGhgs = record.tieTimeseries<double>("fixed-costs-greenhouse");
  d_varNoxs = record.tieTimeseries<double>("variable-costs-nox");
  d_fixNoxs = record.tieTimeseries<double>("fixed-costs-nox");
  d_varDeps = record.tieTimeseries<double>("variable-costs-depletion");
  d_fixDeps = record.tieTimeseries<double>("fixed-costs-depletion");
  d_varLucs = record.tieTimeseries<double>("variable-costs-landuse");
  d_fixLucs = record.tieTimeseries<double>("fixed-costs-landuse");

// load the cost set variants
  d_dutySpecCosts.ghg = d_dutySpecGhg;
  d_sizeSpecCosts.ghg = d_sizeSpecGhg;
  d_standingCosts.ghg = d_standingGhg;
  d_dutySpecCosts.nox = d_dutySpecNox;
  d_sizeSpecCosts.nox = d_sizeSpecNox;
  d_standingCosts.nox = d_standingNox;
  d_dutySpecCosts.dep = d_dutySpecDep;
  d_sizeSpecCosts.dep = d_sizeSpecDep;
  d_standingCosts.dep = d_standingDep;
  d_dutySpecCosts.luc = d_dutySpecLuc;
  d_sizeSpecCosts.luc = d_sizeSpecLuc;
  d_standingCosts.luc = d_standingLuc;
}

// ---------------------------------------------------------
//  MEMBER FUNCTION : ~CostRegisterSRAll
// ---------------------------------------------------------

CostRegisterSRAll::~CostRegisterSRAll()
{
  s_logger_costreg->repx(logga::dbug, "destructor call", "");
}

// MANIPULATORS

// ---------------------------------------------------------
//  MEMBER FUNCTION : updateShortrunCosts
// ---------------------------------------------------------
//  Description  : update all shortrun costs
//  Role         : typically used in technical asset washup calls
//  Techniques   : wrapper to avoid fully resolving the call in client code
//  Status       : complete
// ---------------------------------------------------------

void
CostRegisterSRAll::updateShortrunCosts
(const int      step,
 const CostSet& varCosts,
 const CostSet& fixCosts)
{
  // wrapper
  CostRegister::updateShortrunCosts(step, varCosts, fixCosts);
}

// ---------------------------------------------------------
//  CLASS           : CostRegisterEmbFin
// ---------------------------------------------------------

// ---------------------------------------------------------
//  MEMBER FUNCTION : CostRegisterEmbFin
// ---------------------------------------------------------

CostRegisterEmbFin::CostRegisterEmbFin
(Record& record) :
  CostRegister(record),
  d_annualDiscountRate(record.tieSingle<double>("annual-discount-rate-decimal")),
  d_economicLife(record.tieSingle<int>("economic-life")),
  d_capitalInputInitial(record.tieSingle<double>("capex-initial")),
  d_capitalInputTerminal(record.tieSingle<double>("capex-terminal")),
  d_currentAge(record.tieSingle<int>("current-age"))
{
  // initial reporting
  s_logger_costreg->repx(logga::dbug, "constructor call", "");

  // integrity checks
  if ( d_economicLife == 0 )
    {
      s_logger_costreg->repx(logga::rankJumpy, "economic life is zero", d_economicLife);
    }

  // tie some inherited quantities in the constructor (as opposed
  // to the member initialization list)
  d_embFins = record.tieTimeseries<double>("embedded-costs-financial");
}

// ---------------------------------------------------------
//  MEMBER FUNCTION : ~CostRegisterEmbFin
// ---------------------------------------------------------

CostRegisterEmbFin::~CostRegisterEmbFin()
{
  s_logger_costreg->repx(logga::dbug, "destructor call", "");
}

// MANIPULATORS

// ---------------------------------------------------------
//  MEMBER FUNCTION : updateEmbeddedCosts
// ---------------------------------------------------------

void
CostRegisterEmbFin::updateEmbeddedCosts
(const int step)                             // horizon step
{
  // initial reporting
  s_logger_costreg->repx(logga::xtra, "entering member function", "");

  // calculations
  // CAUTION: 'xeona::capitalRecovery' return is interval length adjusted
  d_embFins->at(step) = xeona::capitalRecovery(d_annualDiscountRate,
                                               d_economicLife,
                                               d_capitalInputInitial,
                                               d_capitalInputTerminal,
                                               d_currentAge);
}

// ---------------------------------------------------------
//  CLASS           : CostRegisterEmbAll
// ---------------------------------------------------------

// ---------------------------------------------------------
//  MEMBER FUNCTION : CostRegisterEmbAll
// ---------------------------------------------------------

CostRegisterEmbAll::CostRegisterEmbAll
(Record& record) :
  CostRegister(record),
  CostRegisterEmbFin(record),
  d_physicalLife(record.tieSingle<int>("physical-life")),
  d_investGhg(record.tieSingle<double>("investment-greenhouse")),
  d_investNox(record.tieSingle<double>("investment-nox")),
  d_investDep(record.tieSingle<double>("investment-depletion")),
  d_investLuc(record.tieSingle<double>("investment-landuse"))
{
  // initial reporting
  s_logger_costreg->repx(logga::dbug, "constructor call", "");

  // integrity checks
  if ( d_physicalLife == 0 )
    {
      s_logger_costreg->repx(logga::rankJumpy, "physical life is zero", d_physicalLife);
    }

  // tie some inherited quantities in the constructor (as opposed
  // to the member initialization list)
  d_embGhgs = record.tieTimeseries<double>("embedded-costs-greenhouse");
  d_embNoxs = record.tieTimeseries<double>("embedded-costs-nox");
  d_embDeps = record.tieTimeseries<double>("embedded-costs-depletion");
  d_embLucs = record.tieTimeseries<double>("embedded-costs-landuse");
}

// ---------------------------------------------------------
//  MEMBER FUNCTION : ~CostRegisterEmbAll
// ---------------------------------------------------------

CostRegisterEmbAll::~CostRegisterEmbAll()
{
  s_logger_costreg->repx(logga::dbug, "destructor call", "");
}

// MANIPULATORS

// ---------------------------------------------------------
//  MEMBER FUNCTION : updateEmbeddedCosts
// ---------------------------------------------------------

void
CostRegisterEmbAll::updateEmbeddedCosts
(const int step)                             // horizon step
{
  // initial reporting
  s_logger_costreg->repx(logga::xtra, "entering member function", "");

  // calculations
  // CAUTION: 'xeona::capitalRecovery' return is interval length adjusted
  d_embFins->at(step) = xeona::capitalRecovery(d_annualDiscountRate,
                                               d_economicLife,
                                               d_capitalInputInitial,
                                               d_capitalInputTerminal,
                                               d_currentAge);

  // CAUTION: physical life is given in years
  if ( d_physicalLife == 0 )                 // div-by-zero protection
    {
      const int divByZeroValue = 0.0;        // selected return value
      d_embGhgs->at(step) = divByZeroValue;
      d_embNoxs->at(step) = divByZeroValue;
      d_embDeps->at(step) = divByZeroValue;
      d_embLucs->at(step) = divByZeroValue;
    }
  else
    {
      // CAUTION: 'operator*' and 'operator/' associate leftward in C++
      const int spy = xeona::secondsPerYear;
      d_embGhgs->at(step) = d_investGhg / d_physicalLife / spy * d_interval;
      d_embNoxs->at(step) = d_investNox / d_physicalLife / spy * d_interval;
      d_embDeps->at(step) = d_investDep / d_physicalLife / spy * d_interval;
      d_embLucs->at(step) = d_investLuc / d_physicalLife / spy * d_interval;
    }
}

//  end of file

