//  XEONA ENERGY SYSTEMS MODELING ENVIRONMENT
//
//  file-create-name : cxamb03.h
//  file-create-date : Thu 03-Feb-2011 12:11 UTC
//  file-initiator   : Robbie Morrison <robbie@actrix.co.nz>
//
//  file-role        : concrete ambient conditions contexts 3 / header
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
//  $URL: file:///home/robbie/svn-root/xeona/futz/trunk/xeona1/e/cxamb03.h $
//
//  GENERAL NOTES FOR THIS FILE
//
//  The contexts here cover hydro reservoir inflow.

//  HEADER GUARD

#ifndef _CXAMB03_H_
#define _CXAMB03_H_

//  LOCAL AND SYSTEM INCLUDES

#include "../e/context.h"     // abstract context entity
#include "../c/tsset.h"       // timeseries classes for added functionality
#include "../a/exent.h"       // entity exception classes

#include "../a/logger_fwd.h"  // some forward declarations from "logger.h"
#include "../c/smart_ptr.h"   // toggle between Boost and TR1 smart pointers

#include <string>             // C++ strings
#include <sstream>            // string-streams

#include <vector>             // STL sequence container
#include <map>                // STL associative container

//  FORWARD (PARTIAL) DECLARATIONS

class Record;                                // constructor argument
class InflowSet;

//  CODE

// ---------------------------------------------------------
//  CLASS           : CxInflow
// ---------------------------------------------------------
//  Description  : interface base for inflow contexts
//  Role         : step in the inheritance web
//  Techniques   : (nothing special)
//  Status       : complete
// ---------------------------------------------------------

class CxInflow :
  public AmbientConditions
{

  // DISABLED

private:

  CxInflow();                                     // zero-argument constructor
  CxInflow(const CxInflow& orig);                 // copy constructor
  CxInflow& operator= (const CxInflow& orig);     // copy assignment operator

  // CREATORS

public:

  explicit
  CxInflow
  (const std::string entityId) :             // special constructor to facilitate linking
    AmbientConditions(entityId)
  { }

  explicit
  CxInflow
  (const std::string entityId,
   Record&           record);

  virtual
  ~CxInflow();

  // LINKING CALL

private:                                     // CAUTION: 'private' is correct

  virtual
  bool
  polylink(assign_ptr<Entity>& pass)         // CAUTION: 'Entity' is correct
  {
    s_logger->repx(logga::adhc, "entering virtual member function", "CxInflow");
    const bool okay = pass.revamp(retFull<CxInflow>());      // key call
    return okay;                             // typically return directly from 'revamp'
  }

  // ACCESSORS

public:

  virtual
  std::string                                // see proper definition
  getDescription() const { return ""; }

  virtual
  int                                        // see proper definition
  getYear() const { return s_intZero; }

  virtual
  double                                     // see proper definition
  getInflow
  (const int step) const { return s_doubleZero; }

  virtual
  double                                     // see proper definition
  getHistorical
  (const int step) const { return s_doubleZero; }

  virtual
  std::vector<double>
  getHistoricalSeries() const { return std::vector<double>(); }

  virtual
  double                                     // see proper definition
  getAnnualMean() const { return s_doubleZero; }

  virtual
  double                                     // see proper definition
  getAnnualMinimum() const { return s_doubleZero; }

  virtual
  double                                     // see proper definition
  getAnnualMaximum() const { return s_doubleZero; }

};

// ---------------------------------------------------------
//  CLASS           : CxInflowSets
// ---------------------------------------------------------
//  Description  : inflow context with hard-coded annual daily timeseries
//  Role         : ambient conditions support
//  Techniques   : 'std::vector' 'std::map', 'g/ifdata01.h' data
//  Status       : complete
// ---------------------------------------------------------

class CxInflowSets :
  public CxInflow
{
  // DISABLED

private:

  CxInflowSets();                                      // zero-argument constructor
  CxInflowSets(const CxInflowSets& orig);              // copy constructor
  CxInflowSets& operator= (const CxInflowSets& orig);  // copy assignment operator

  // CREATORS

public:

  explicit
  CxInflowSets
  (const std::string entityId,
   Record&           record)
    throw (xeona::entity_issue_2);

  virtual
  ~CxInflowSets();

  // CALLS

  virtual
  void
  setup();

  // ACCESSORS

public:

  virtual
  std::string
  getDescription()   const;

  virtual
  int
  getYear()          const;

  virtual
  double
  getInflow
  (const int step)   const;

  virtual
  double
  getHistorical
  (const int step)   const;

  virtual
  std::vector<double>
  getHistoricalSeries() const;               // entire timeseries

  virtual
  double
  getAnnualMinimum() const;

  virtual
  double
  getAnnualMaximum() const;

  // UTILITY FUNCTIONS

private:

  bool                                       // 'true' if key found, else 'false'
  loadFlowset
  (const std::string& flowsetId);            // flowset identifier

  void
  calcStats();

  // INSTANCE DATA

private:

  // tied quantities

  const std::string&                  d_flowsetId;
  const double&                       d_scale;

  std::string&                        d_flowsetDescription;
  double&                             d_flowsetMean;
  double&                             d_flowsetMinimum;
  double&                             d_flowsetMaximum;
  shared_ptr<std::vector<double> >    d_flowset;

  // local quantities

  std::string                         d_description;
  int                                 d_year;
  std::vector<double>                 d_days;
  std::vector<double>                 d_months;

  std::vector<double>                 d_inflows;       // 'd_days' expended
  std::vector<double>                 d_historicals;   // 'd_months' expanded

  TsNormal                            d_inflowMonthlyTs;    // normal type is correct
  TsNormal                            d_inflowDailyTs;

  // STATIC DATA

  static const shared_ptr<std::map<std::string, shared_ptr<InflowSet> > >    d_flowsets;

};

//  ==== XEDOC =================================================
//
//  entity.cx-inflow-sets-0
//
//      class                                    > CxInflowSets
//
//        ambient inflow context which serves embedded data --
//        currently limited to the 1983 Benmore flowset
//        distributed by the New Zealand Electricity Commission
//
//        this context resamples for intervals other than 3600s
//
//      builtin-remark s                         <
//
//      flowset-id s                             > "benmore1983"
//      scale [-] f                              > 1.0
//
//        flowset-id is the identifier of an embedded flowset,
//        scale [0,inf] rescales the entire timeseries
//
//      flowset-description s                    < "_"
//      flowset-mean [m3/s] f                    < 0.0
//      flowset-minimum [m3/s] f                 < 0.0
//      flowset-maximum [m3/s] f                 < 0.0
//      flowset [m3/s] F                         < 0.0 ..
//
//  ============================================================

#endif // _CXAMB03_H_

//  end of file

