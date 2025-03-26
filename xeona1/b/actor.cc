//  XEONA ENERGY SYSTEMS MODELING ENVIRONMENT
//
//  file-create-name : actor.cc
//  file-create-date : Mon 25-Aug-2008 10:35 UTC
//  file-initiator   : Robbie Morrison <robbie@actrix.co.nz>
//
//  file-role        : abstract actor entity / implementation
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
//  $URL: file:///home/robbie/svn-root/xeona/futz/trunk/xeona1/b/actor.cc $

//  LOCAL AND SYSTEM INCLUDES

#include "actor.h"            // companion header for this file (place first)

#include "../c/recset.h"      // records and fields and also record-sets
#include "../a/logger.h"      // standard logging functionality (as required)
#include "../c/smart_ptr.h"   // toggle between Boost and TR1 smart pointers
#include ".././common.h"      // common definitions for project (place last)

#include <string>             // C++ strings
#include <sstream>            // string-streams

//  CODE

// CREATORS

// ---------------------------------------------------------
//  MEMBER FUNCTION : Actor
// ---------------------------------------------------------

Actor::Actor
(const std::string entityId,
 Record&           record) :
  CostRegister(record),
  FullEntity(entityId, record)
{
  s_logger->repx(logga::xtra, "constructor call", getIdAndKind());
}

// ---------------------------------------------------------
//  MEMBER FUNCTION : ~Actor
// ---------------------------------------------------------

Actor::~Actor()
{
  s_logger->repx(logga::adhc, "destructor call", getIdAndKind());
}

//  end of file

