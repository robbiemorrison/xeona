//  XEONA ENERGY SYSTEMS MODELING ENVIRONMENT
//
//  file-create-name : debug.cc
//  file-create-date : Mon 22-Nov-2010 04:39 UTC
//  file-initiator   : Robbie Morrison <robbie@actrix.co.nz>
//
//  file-role        : debugging support in namespace 'xeona::debug' / implementation
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
//  $URL: file:///home/robbie/svn-root/xeona/futz/trunk/xeona1/c/debug.cc $
//
//  GENERAL NOTES FOR THIS FILE

//  LOCAL AND SYSTEM INCLUDES

#include "debug.h"            // companion header for this file (place first)

#include "../b/entity.h"      // entity base class plus lazy linking

#include "../a/logger.h"      // standard logging functionality (as required)
#include "../c/smart_ptr.h"   // toggle between Boost and TR1 smart pointers
#include ".././common.h"      // common definitions for project (place last)

#include <string>             // C++ strings
#include <sstream>            // string-streams

#include <boost/foreach.hpp>  // BOOST_FOREACH iteration macro
#include <boost/format.hpp>   // printf style formatting

//  CODE

namespace xeona {
  namespace debug {

    // ---------------------------------------------------------
    //  FREE FUNCTION   : xeona::debug::entityReport
    // ---------------------------------------------------------

    void
    entityReport()
    {
#ifdef _XDEBUG

      static logga::spLogger logger = logga::ptrLogStream();
      std::ostringstream put;
      put << Entity::reportFullPopulation();
      logger->repx(logga::dbug, "entity report follows", "");
      logger->putx(logga::dbug, put);

#endif // _XDEBUG
    }

  }
} // namespace 'xeona::debug'

//  end of file

