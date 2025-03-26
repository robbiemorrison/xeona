//  XEONA ENERGY SYSTEMS MODELING ENVIRONMENT
//
//  file-create-name : registr.h
//  file-create-date : Wed 20-Jun-2007 13:25 UTC
//  file-initiator   : Robbie Morrison <robbie@actrix.co.nz>
//
//  file-role        : sub-entity registrations (modify this and 'register.cc') / header
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
//  $URL: file:///home/robbie/svn-root/xeona/futz/trunk/xeona1/b/register.h $

//  HEADER GUARD

#ifndef _REGISTR_H_
#define _REGISTR_H_

//  LOCAL AND SYSTEM INCLUDES

#include "../a/logger_fwd.h"  // some forward declarations from "logger.h"

//  CODE

namespace xeona
{
  // ---------------------------------------------------------
  //  FREE FUNCTION   : xeona::registerEntyCreators
  // ---------------------------------------------------------
  //  Description  : called by main to register Entity creators
  // ---------------------------------------------------------

  void
  registerEntyCreators();

} // namespace xeona

#endif // _REGISTR_H_

//  end of file

