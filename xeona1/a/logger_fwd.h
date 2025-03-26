//  XEONA ENERGY SYSTEMS MODELING ENVIRONMENT
//
//  file-create-name : logger_fwd.h
//  file-create-date : Mon 16-Jul-2007 19:30 UTC
//  file-initiator   : Robbie Morrison <robbie@actrix.co.nz>
//
//  file-role        : subset of "logger.h" (avoid hash-including entire header) / header
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
//  $URL: file:///home/robbie/svn-root/xeona/futz/trunk/xeona1/a/logger_fwd.h $
//
//  GENERAL NOTES FOR THIS FILE
//
//  This file is used to avoid hash-including the entire logging unit header.

//  HEADER GUARD

#ifndef _LOGGER_FWD_H_
#define _LOGGER_FWD_H_
#ifndef _LOGGER_H_            // skip if "../a/logger.h"

//  LOCAL AND SYSTEM INCLUDES

#include "../c/smart_ptr.h"   // switch easily between TR1 and Boost smart pointers

//  FORWARD (PARTIAL) DECLARATIONS

namespace logga               // forward declaration for logger object
{
  class Logger;
  typedef shared_ptr<Logger> spLogger;
}

#endif // _LOGGER_H_
#endif // _LOGGER_FWD_H_

//  end of file

