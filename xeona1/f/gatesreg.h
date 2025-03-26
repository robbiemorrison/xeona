//  XEONA ENERGY SYSTEMS MODELING ENVIRONMENT
//
//  file-create-name : gatesreg.h
//  file-create-date : Sat 20-Feb-2010 19:48 UTC
//  file-initiator   : Robbie Morrison <robbie@actrix.co.nz>
//
//  file-role        : find and register gateways / header
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
//  $URL: file:///home/robbie/svn-root/xeona/futz/trunk/xeona1/f/gatesreg.h $
//
//  GENERAL NOTES FOR THIS FILE

//  HEADER GUARD

#ifndef _GATESREG_H_
#define _GATESREG_H_

//  LOCAL AND SYSTEM INCLUDES

#include "../a/logger_fwd.h"  // some forward declarations from "logger.h"
#include "../c/smart_ptr.h"   // toggle between Boost and TR1 smart pointers

#include <string>             // C++ strings
#include <sstream>            // string-streams

//  CODE

// ---------------------------------------------------------
//  FREE FUNCTION   : xeona::registerGates
// ---------------------------------------------------------
//  Description  : finalize reciprocal information for gateway and domain controllers
//  Role         : point of contact for registering gateways, called by 'xeona::simulate'
//  Note         : one pass protection
//  Status       : complete
// ---------------------------------------------------------

namespace xeona
{
  bool                                       // 'true' means ran to completion
  registerGates();
}

#endif // _GATESREG_H_

//  end of file

