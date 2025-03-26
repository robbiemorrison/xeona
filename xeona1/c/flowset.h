//  XEONA ENERGY SYSTEMS MODELING ENVIRONMENT
//
//  file-create-name : flowset.h
//  file-create-date : Thu 03-Feb-2011 10:45 UTC
//  file-initiator   : Robbie Morrison <robbie@actrix.co.nz>
//
//  file-role        : lake inflow dataset support / header
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
//  $URL: file:///home/robbie/svn-root/xeona/futz/trunk/xeona1/c/flowset.h $
//
//  GENERAL NOTES FOR THIS FILE

//  HEADER GUARD

#ifndef _FLOWSET_H_
#define _FLOWSET_H_

//  LOCAL AND SYSTEM INCLUDES

#include "../a/logger_fwd.h"  // some forward declarations from "logger.h"
#include "../c/smart_ptr.h"   // toggle between Boost and TR1 smart pointers

#include <map>                // STL associative container
#include <string>             // C++ strings
#include <vector>             // STL sequence container

//  CODE

// ---------------------------------------------------------
//  CLASS           : InflowSet
// ---------------------------------------------------------

class InflowSet
{
  // DISABLED

private:

  InflowSet();                                    // zero-argument constructor
  InflowSet(const InflowSet& orig);               // copy constructor
  InflowSet& operator= (const InflowSet& orig);   // copy assignment operator

  // CREATORS

public:

  InflowSet
  (const std::string&         a_description,
   const int                  a_year,
   const std::vector<double>& a_days,
   const std::vector<double>& a_months);

  // INSTANCE DATA

public:

  const std::string            description;
  const int                    year;
  const std::vector<double>    days;
  const std::vector<double>    months;

  // STATIC DATA

private:

  static logga::spLogger       s_logger;     // shared_ptr to single logger object

};

// ---------------------------------------------------------
//  FREE FUNCTION   : xeona::loadInflowSets
// ---------------------------------------------------------
//  Description  : loads and returns a map with 'InflowSet' objects
//  Role         : support for 'CxInflowSets' context
//  Techniques   : 'std::map' 'Boost.Assign' library
//  Status       : complete
//
//  Usage
//
//      shared_ptr<xeona::inflowsets_type> inflowsets
//      inflowsets = xeona::loadInflowSets()
//
// ---------------------------------------------------------

namespace xeona
{
  // for convenience
  typedef std::map<std::string, shared_ptr<InflowSet> > inflowsets_type;

  shared_ptr<std::map<std::string, shared_ptr<InflowSet> > >
  loadInflowSets();
}

#endif // _FLOWSET_H_

//  end of file

