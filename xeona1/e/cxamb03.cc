//  XEONA ENERGY SYSTEMS MODELING ENVIRONMENT
//
//  file-create-name : cxamb03.cc
//  file-create-date : Thu 03-Feb-2011 12:11 UTC
//  file-initiator   : Robbie Morrison <robbie@actrix.co.nz>
//
//  file-role        : concrete ambient conditions contexts 3 / implementation
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
//  $URL: file:///home/robbie/svn-root/xeona/futz/trunk/xeona1/e/cxamb03.cc $
//
//  GENERAL NOTES FOR THIS FILE

//  LOCAL AND SYSTEM INCLUDES

#include "cxamb03.h"          // companion header for this file (place first)

#include "../c/tsops.h"       // overloaded operators and similar for timeseries
#include "../c/stats.h"       // on-the-fly statistical calculations
#include "../c/recset.h"      // records and fields and also record-sets
#include "../c/flowset.h"     // lake inflow dataset support
#include "../a/exent.h"       // entity exception classes

#include "../a/logger.h"      // standard logging functionality (as required)
#include "../c/smart_ptr.h"   // toggle between Boost and TR1 smart pointers
#include ".././common.h"      // common definitions for project (place last)

#include <algorithm>          // STL copying, searching, and sorting, max()
#include <iterator>           // STL additional iterators, std::distance()
#include <string>             // C++ strings
#include <sstream>            // string-streams

//  CODE

// ---------------------------------------------------------
//  CLASS           : CxInflow
// ---------------------------------------------------------

// ---------------------------------------------------------
//  MEMBER FUNCTION : CxInflow::CxInflow
// ---------------------------------------------------------

CxInflow::CxInflow
(const std::string entityId,
 Record&           record) :
  AmbientConditions(entityId, record)
{
  s_logger->repx(logga::xtra, "constructor call", getIdAndKind());
  d_builtinRemark = "beta";
}

// ---------------------------------------------------------
//  MEMBER FUNCTION : CxInflow::~CxInflow
// ---------------------------------------------------------

CxInflow::~CxInflow()
{
  s_logger->repx(logga::xtra, "destructor call", getIdAndKind());
}

// ---------------------------------------------------------
//  CLASS           : CxInflowSets
// ---------------------------------------------------------

// STATIC DEFINITIONS - with and without explicit initialization

typedef std::map<std::string, shared_ptr<InflowSet> > inflowsets_type;
const shared_ptr<inflowsets_type> CxInflowSets::d_flowsets = xeona::loadInflowSets();

// CREATORS

// ---------------------------------------------------------
//  MEMBER FUNCTION : CxInflowSets::CxInflowSets
// ---------------------------------------------------------
//  Description  : constructor
//  Role         : entity
//  Techniques   : (nothing special)
//  Status       : complete
//
//  CAUTION: 'entity.time-horizon' at top of model file
//
//      The mandatory 'TimeHorizon' entity with the mandatory
//      identifier 'entity.time-horizon' MUST precede the entry
//      for this entity.
//
//      Otherwise a floating point exception will result,
//      preceded by a warning like this or similar:
//
//  506   b/entity.cc   getHorizonInterval   WARN   horizon interval not initialized   -1
//
//      Moreover, do not move the "load dataset" code into the
//      'CxInflowSets::setup' function.
//
// ---------------------------------------------------------

CxInflowSets::CxInflowSets
(const std::string entityId,
 Record&           record)
  throw (xeona::entity_issue_2) :
  CxInflow(entityId, record),
  d_flowsetId(record.tieSingle<std::string>("flowset-id")),
  d_scale(record.tieSingle<double>("scale")),
  d_flowsetDescription(record.tieSingle<std::string>("flowset-description")),
  d_flowsetMean(record.tieSingle<double>("flowset-mean")),
  d_flowsetMinimum(record.tieSingle<double>("flowset-minimum")),
  d_flowsetMaximum(record.tieSingle<double>("flowset-maximum")),
  d_flowset(record.tieTimeseries<double>("flowset")),
  d_description(),
  d_year(0),
  d_days(),
  d_months(),
  d_inflows(),
  d_historicals(),
  d_inflowMonthlyTs("monthly inflows empty"),
  d_inflowDailyTs("daily inflows empty")
{
  // initial reporting
  s_logger->repx(logga::adhc, "constructor call", getIdAndKind());

  // load dataset
  const bool ret = loadFlowset(d_flowsetId); // utility function
  if ( ret == false )                        // key most likely false
    {
      s_logger->repx(logga::warn, "inflow set not found", d_flowsetId);
      if ( xeona::nopro == false )           // meaning option '--krazy' not applied
        {
          s_logger->repx(logga::warn, "about to throw", "xeona::entity_issue_2");
          throw xeona::entity_issue_2("CxInflowSets",
                                      __func__,
                                      getIdAndKind(),
                                      "invalid inflow set identifier (fix model)");
        }
      else
        {
          s_logger->repx(logga::warn, "will probably segfault very soon", "");
        }
    }

  // builtin remark
  d_builtinRemark = "beta";

  // completion reporting
  s_logger->repx(logga::adhc, "leaving member function", "");
}

// ---------------------------------------------------------
//  MEMBER FUNCTION : CxInflowSets::~CxInflowSets
// ---------------------------------------------------------

CxInflowSets::~CxInflowSets()
{
  s_logger->repx(logga::adhc, "destructor call", getIdAndKind());
}

// ---------------------------------------------------------
//  MEMBER FUNCTION : CxInflowSets::setup
// ---------------------------------------------------------
//  Description  : setup call
//  Role         : part of the simulation call sequence
//  Techniques   : 'loadFlowset' 'calcStats'
//  Status       : complete
//
//  Design notes
//
//      The 'calcStats' could equally go in the constructor,
//      although the entity is not linked at that point.
//
// ---------------------------------------------------------

void
CxInflowSets::setup()
{
  // initial reporting
  s_logger->repx(logga::adhc, "entering member function", "");

  // active code
  calcStats();                               // utility function

  // copy over dataset for out-data purposes
  *d_flowset = d_inflows;
}

// ACCESSORS

// ---------------------------------------------------------
//  MEMBER FUNCTION : CxInflowSets::getDescription
// ---------------------------------------------------------

std::string
CxInflowSets::getDescription() const
{
  return d_description;
}

// ---------------------------------------------------------
//  MEMBER FUNCTION : CxInflowSets::getDescription
// ---------------------------------------------------------

int
CxInflowSets::getYear() const
{
  return d_year;
}

// ---------------------------------------------------------
//  MEMBER FUNCTION : CxInflowSets::getInflow
// ---------------------------------------------------------

double
CxInflowSets::getInflow
(const int step) const
{
  return d_inflows.at(step);
}

// ---------------------------------------------------------
//  MEMBER FUNCTION : CxInflowSets::getHistorical
// ---------------------------------------------------------

double
CxInflowSets::getHistorical
(const int step) const
{
  // defensive programming
  if ( d_historicals.empty() )
    {
      s_logger->repx(logga::warn, "vector still empty, requested step", step);
    }

  // active code
  return d_historicals.at(step);
}

// ---------------------------------------------------------
//  MEMBER FUNCTION : CxInflowSets::getHistoricalSeries
// ---------------------------------------------------------

std::vector<double>
CxInflowSets::getHistoricalSeries() const
{
  return d_historicals;
}

// ---------------------------------------------------------
//  MEMBER FUNCTION : CxInflowSets::getAnnualMinimum
// ---------------------------------------------------------

double
CxInflowSets::getAnnualMinimum() const
{
  return d_flowsetMinimum;
}

// ---------------------------------------------------------
//  MEMBER FUNCTION : CxInflowSets::getAnnualMaximum
// ---------------------------------------------------------

double
CxInflowSets::getAnnualMaximum() const
{
  return d_flowsetMinimum;
}

// UTILITY FUNCTIONS

// ---------------------------------------------------------
//  MEMBER FUNCTION : CxInflowSets::loadFlowset
// ---------------------------------------------------------
//  Description  : load flowset data into this object
//  Role         : used by constructor
//  Techniques   : 'TsNormal' 'TsMonthly'
//  Status       : complete
// ---------------------------------------------------------

bool                                         // 'true' if key found, else 'false'
CxInflowSets::loadFlowset
(const std::string& flowsetId)               // flowset identifier
{
  // initial reporting
  s_logger->repx(logga::adhc, "entering member function, flowset", flowsetId);

  // preamble
  std::ostringstream put;

  // preliminary
  const int  interval    = Entity::getHorizonInterval();
  const int  steps       = Entity::getHorizonSteps();
  const int  startOffset = Entity::getHorizonOffset();
  const bool leapYear    = Entity::getLeapYear();

  // hunt thru map
  std::map<std::string, shared_ptr<InflowSet> >::const_iterator pos;
  pos = d_flowsets->find(flowsetId);
  if ( pos == d_flowsets->end() )          // the key is faulty
    {
      s_logger->repx(logga::warn, "failed to find flowset, key", flowsetId);
      return false;
    }

  // carry on and unload the data -- unit 'c/flowset'
  shared_ptr<InflowSet> inflowset;           // block-scope
  inflowset     = pos->second;               // recover the 'InflowSet' object
  d_description = inflowset->description;    // as a string
  d_year        = inflowset->year;           // as an integer
  d_days        = inflowset->days;
  d_months      = inflowset->months;

  const int  daysLen  = d_days.size();
  const bool hasFeb29 = ( daysLen == 366 ) ? true : false;  // reverse engineer
  const bool loopable = false;

  s_logger->repx(logga::dbug, "daily inflow data length", daysLen);

  // load block-local monthseries and timeseries wrappers
  TsMonthly inflowOrigTm(d_months,
                         "monthly inflows original");

  TsNormal inflowOrigTs(d_days,
                        24 * 3600,
                        "daily inflows original",
                        hasFeb29,
                        loopable);

  // report
  inflowOrigTm.setCaller( XEONA_FUNC );      // preprocessor macro defined in 'common.h'
  inflowOrigTs.setCaller( XEONA_FUNC );
  inflowOrigTm.report(put);
  inflowOrigTs.report(put);
  s_logger->addSmartBlank(logga::dbug);
  s_logger->putx(logga::dbug, put);

  // resample
  const std::string msg = " resampled but not resized";
  d_inflowMonthlyTs = inflowOrigTm.sampleDuplicate("monthly inflows" + msg,
                                                   interval,
                                                   startOffset,
                                                   leapYear,
                                                   d_scale,
                                                   0.0);         // no offsetting in-data

  d_inflowDailyTs = inflowOrigTs.sampleLinear("daily inflows" + msg,
                                              interval,
                                              startOffset,
                                              leapYear,
                                              d_scale,
                                              0.0);

  // write back to the primary data members
  const int historicalsSize = d_inflowMonthlyTs.copyfill(d_historicals, steps);
  const int dailysSize      = d_inflowDailyTs.copyfill(d_inflows, steps);

  // integrity checks
  if ( historicalsSize == 0 )
    {
      s_logger->repx(logga::warn, "d_historicals remains empty", historicalsSize);
    }
  else
    {
      s_logger->repx(logga::dbug, "historicals size", historicalsSize);
    }
  if ( dailysSize == 0 )
    {
      s_logger->repx(logga::warn, "d_inflows remains empty", dailysSize);
    }
  else
    {
      s_logger->repx(logga::dbug, "dailys size", dailysSize);
    }

#if 0 // 0 = omit, 1 = additional information

  // dbug printout
  put << "  historicals : " << d_historicals.size() << "    ";
  std::copy(d_historicals.begin(), d_historicals.end(),
            std::ostream_iterator<double>(put, " "));
  put << "\n";
  s_logger->repx(logga::dbug, "printing out historicals", "");
  s_logger->putx(logga::dbug, put);

#endif // 0

  // report
  d_inflowMonthlyTs.setCaller( XEONA_FUNC ); // preprocessor macro defined in 'common.h'
  d_inflowDailyTs.setCaller( XEONA_FUNC );
  d_inflowMonthlyTs.report(put);
  d_inflowDailyTs.report(put);
  s_logger->addSmartBlank(logga::dbug);
  s_logger->putx(logga::dbug, put);

  // gnuplot visualization
  int plotUntil = steps;                     // adaptive
  TsGnuplot gp;                              // plotting object with default command
  gp.addCaller( __FILE__ , __LINE__ );       // less whitespace is okay
  gp.addTs(d_inflowMonthlyTs);
  gp.addTs(d_inflowDailyTs);
  gp.plot("resampled weather data", plotUntil, 'p');

  // return
  return true;                               // meaning the inflow set was found

} // function 'CxInflowSets::loadFlowset'

// ---------------------------------------------------------
//  MEMBER FUNCTION : CxInflowSets::calcStats
// ---------------------------------------------------------
//  Description  : update statistics
//  Role         : called by 'setup'
//  Techniques   : class 'Statistics<>'
//  Status       : complete
// ---------------------------------------------------------

void
CxInflowSets::calcStats()
{
  s_logger->repx(logga::adhc, "entering member function", "");
  Statistics<double> flowstats(d_days);
  d_flowsetDescription = d_description;      // assign from internal to tied quantity
  d_flowsetMean        = flowstats.mean();
  d_flowsetMinimum     = flowstats.min();
  d_flowsetMaximum     = flowstats.max();
  const double count   = flowstats.count();  // local reporting only

  // additional reporting as appropriate
  // YEEK 44 CODE (set by '--yeek')
  if ( xeona::yeek == 44 || xeona::yeek == 1 || xeona::yeek == 2 )
    {
      std::ostringstream put;
      put << "  flowset calculate statistics report"                              << "\n"
          << "      identity    : " << getIdAndKind()                             << "\n"
          << "    flowset"                                                        << "\n"
          << "      count       : " << count                                      << "\n"
          << "      description : " << d_flowsetDescription                       << "\n"
          << "      mean        : " << d_flowsetMean                              << "\n"
          << "      minimum     : " << d_flowsetMinimum                           << "\n"
          << "      maximum     : " << d_flowsetMaximum                           << "\n";
      s_logger->repx(logga::dbug, "additional reporting follows, yeek", xeona::yeek);
      s_logger->putx(logga::dbug, put);
    }
}

//  end of file

