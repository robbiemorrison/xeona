//  XEONA ENERGY SYSTEMS MODELING ENVIRONMENT
//
//  file-create-name : flowset.cc
//  file-create-date : Thu 03-Feb-2011 10:45 UTC
//  file-initiator   : Robbie Morrison <robbie@actrix.co.nz>
//
//  file-role        : lake inflow dataset support / implementation
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
//  $URL: file:///home/robbie/svn-root/xeona/futz/trunk/xeona1/c/flowset.cc $
//
//  GENERAL NOTES FOR THIS FILE

//  LOCAL AND SYSTEM INCLUDES

#include "flowset.h"          // companion header for this file (place first)

#include "../a/logger.h"      // standard logging functionality (as required)
#include "../c/smart_ptr.h"   // toggle between Boost and TR1 smart pointers
#include ".././common.h"      // common definitions for project (place last)

#include <string>             // C++ strings
#include <sstream>            // string-streams
#include <vector>             // STL sequence container

#include <boost/assign/std/vector.hpp>  // assign operator+=() for std::vector

//  NAMESPACE DIRECTIVES

using namespace boost::assign;               // to resolve the += operator

//  CODE

// ---------------------------------------------------------
//  CLASS           : InflowSet
// ---------------------------------------------------------

// STATIC DEFINITIONS

logga::spLogger InflowSet::s_logger = logga::ptrLogStream();     // bind logger

// ---------------------------------------------------------
//  MEMBER FUNCTION : InflowSet
// ---------------------------------------------------------

InflowSet::InflowSet
(const std::string&         a_description,
 const int                  a_year,
 const std::vector<double>& a_days,
 const std::vector<double>& a_months) :
    description(a_description),
    year(a_year),
    days(a_days),                            // copy assign all elements to target
    months(a_months)
  {
    // integrity checks
    if ( description.empty() )
      {
        s_logger->repx(logga::warn, "no description given", description);
      }
    const int len1 = days.size();
    if ( len1 != 365 && len1 != 366 )        // one standard year or leap year
      {
        s_logger->repx(logga::warn, "daily inflow set not 365 or 366", len1);
      }
    const int len2 = months.size();
    if ( len2 != 12 )                        // twelve months
      {
        s_logger->repx(logga::warn, "long-run months inflow set not 12", len2);
      }
  }

// ---------------------------------------------------------
//  FREE FUNCTION   : xeona::loadInflowSets
// ---------------------------------------------------------

namespace xeona
{
  shared_ptr<xeona::inflowsets_type>
  loadInflowSets()
  {
    shared_ptr<inflowsets_type> inflowsets(new inflowsets_type());

    // load the data -- while noting the data files contain some C++ code too

#include "../g/ifdata01.h"    // lake inflow data file

    return inflowsets;
  }
}

//  end of file

