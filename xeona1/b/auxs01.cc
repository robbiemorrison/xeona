//  XEONA ENERGY SYSTEMS MODELING ENVIRONMENT
//
//  file-create-name : auxs.cc
//  file-create-date : Thu 16-Jul-2009 21:49 UTC
//  file-initiator   : Robbie Morrison <robbie@actrix.co.nz>
//
//  file-role        : classes for auxiliary model data / implementation
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
//  $URL: file:///home/robbie/svn-root/xeona/futz/trunk/xeona1/b/auxs01.cc $
//
//  GENERAL NOTES FOR THIS FILE

//  LOCAL AND SYSTEM INCLUDES

#include "auxs01.h"           // companion header for this file (place first)

#include "../c/recset.h"      // records and fields and also record-sets
#include "../b/lmpbid.h"      // LMP auction bidset

#include "../a/logger.h"      // standard logging functionality (as required)
#include "../c/smart_ptr.h"   // toggle between Boost and TR1 smart pointers
#include ".././common.h"      // common definitions for project (place last)

#include <string>             // C++ strings
#include <sstream>            // string-streams

//  CODE

// ---------------------------------------------------------
//  CLASS           : AuxBidSets
// ---------------------------------------------------------

// STATIC DEFINITIONS

logga::spLogger AuxBidSets::s_logger = logga::ptrLogStream();

// CREATORS

AuxBidSets::AuxBidSets
(Record& record):
  d_lmpBidsets(record.tieTimeseries<std::string>("lmp-bidsets"))
{
  s_logger->repx(logga::xtra, "constructor call", "");
}

AuxBidSets::~AuxBidSets()
{
  s_logger->repx(logga::adhc, "destructor call", "");
}

// ACCESSORS (INTERPRETED RETURN)

const shared_ptr<LmpBidSet>
AuxBidSets::getBidSet
(const int step) const
{
  // if need be, could place the 'at' call in a try block and
  // then return an empty pointer on failure

  const std::string sBidset = d_lmpBidsets->at(step);
  shared_ptr<LmpBidSet> bidset(new LmpBidSet("auxbidset"));
  if ( bidset->pushString(sBidset) != 1 )
    {
      s_logger->repx(logga::warn, "bidset string parse issue", sBidset);
    }
  return bidset;
}

// ---------------------------------------------------------
//  CLASS           : AuxHeatLead
// ---------------------------------------------------------

// STATIC DEFINITIONS

logga::spLogger AuxHeatLead::s_hl_logger = logga::ptrLogStream();

// CREATORS

AuxHeatLead::AuxHeatLead
(Record& record):
  d_cogenHeatLeadWeight(record.tieSingle<double>("cogen-heat-lead-weighting"))
{
  s_hl_logger->repx(logga::xtra, "constructor call", "");

  // range checking
  if ( d_cogenHeatLeadWeight < 0.0 || d_cogenHeatLeadWeight > 1.0 )
    {
      const double dcog = d_cogenHeatLeadWeight;
      s_hl_logger->repx(logga::warn, "cogen-heat-lead-weighting not [0,1]", dcog);
    }
}

AuxHeatLead::~AuxHeatLead()
{
  s_hl_logger->repx(logga::adhc, "destructor call", "");
}

// ACCESSORS

const double
AuxHeatLead::getCogenHeatLeadWeight() const
{
  return d_cogenHeatLeadWeight;
}

// MANIPULATORS

void
AuxHeatLead::setCogenHeatLeadWeight
(const double cogenHeatLeadWeight)
{
  s_hl_logger->repx(logga::adhc, "entering member function, weight", cogenHeatLeadWeight);

  // integrity checks
  if ( cogenHeatLeadWeight < 0.0 || cogenHeatLeadWeight > 1.0 )
    {
      s_hl_logger->repx(logga::warn, "weighting out of range [0,1]", cogenHeatLeadWeight);
      s_hl_logger->repx(logga::dbug, "retaining prior value", d_cogenHeatLeadWeight);
      return;
    }

  // update
  d_cogenHeatLeadWeight = cogenHeatLeadWeight;
}

//  end of file

