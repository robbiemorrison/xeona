//  XEONA ENERGY SYSTEMS MODELING ENVIRONMENT
//
//  file-create-name : floatstat.cc
//  file-create-date : Mon 20-Jul-2009 10:48 UTC
//  file-initiator   : Robbie Morrison <robbie@actrix.co.nz>
//
//  file-role        : floating-point environment management / implementation
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
//  $URL: file:///home/robbie/svn-root/xeona/futz/trunk/xeona1/a/floatstat.cc $
//
//  GENERAL NOTES FOR THIS FILE

//  LOCAL AND SYSTEM INCLUDES

#include "floatstat.h"        // companion header for this file (place first)

#include "../a/logger.h"      // standard logging functionality (as required)
#include "../c/smart_ptr.h"   // toggle between Boost and TR1 smart pointers
#include ".././common.h"      // common definitions for project (place last)

#include <string>             // C++ strings
#include <sstream>            // string-streams

//  CODE

// ---------------------------------------------------------
//  CLASS           : xeona::SaveClearResetFeFlag
// ---------------------------------------------------------

namespace xeona
{
  // CREATORS

  SaveClearResetFeFlag::SaveClearResetFeFlag
  (const int feFlag) :                       // macros as defined in <fenv.h>, can be |'ed
    d_feFlag(feFlag),
    d_feStat()                               // default construction
  {
    fegetexceptflag(&d_feStat, d_feFlag);    // obtain status
    feclearexcept(d_feFlag);                 // duly clear status
  }

  SaveClearResetFeFlag::~SaveClearResetFeFlag()
  {
    fesetexceptflag(&d_feStat, d_feFlag);    // restore call
  }

  // ACCESSORS

  bool
  SaveClearResetFeFlag::tripped() const
  {
    if ( fetestexcept(d_feFlag) ) return true;    // returns 'int', see manpage
    else                          return false;
  }

  int
  SaveClearResetFeFlag::getFlag() const
  {
    return d_feFlag;
  }

} // namespace 'xeona'

//  end of file

