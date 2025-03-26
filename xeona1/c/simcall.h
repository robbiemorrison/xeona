//  XEONA ENERGY SYSTEMS MODELING ENVIRONMENT
//
//  file-create-name : rundev.h
//  file-create-date : Wed 25-Jul-2007 07:32 UTC
//  file-initiator   : Robbie Morrison <robbie@actrix.co.nz>
//
//  file-role        : principal simulation call / header
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
//  $URL: file:///home/robbie/svn-root/xeona/futz/trunk/xeona1/c/simcall.h $
//
//  GENERAL NOTES FOR THIS FILE
//
//  Principal simulation call.

//  HEADER GUARD

#ifndef _SIMCALL_H_
#define _SIMCALL_H_

//  LOCAL AND SYSTEM INCLUDES

#include <string>             // C++ strings

//  CODE

namespace xeona
{

  // ---------------------------------------------------------
  //  ENUM            : xeona::SimKind
  // ---------------------------------------------------------
  //  Description  : describe the kind of simulation to undertake
  //  Role         : argument to 'simulate'
  // ---------------------------------------------------------

  enum SimKind
  {
    e_notSpecified       = 0,
    e_hollowCall         = 1,
    e_identifyFile       = 2,
    e_parseModel         = 3,
    e_invokeFactory      = 4,
    e_linkAndConnect     = 5,
    e_firstStepRun       = 6,
    e_fullRun            = 7,
    e_yearRun            = 8,
    e_resampleRun        = 9
  };

  // ---------------------------------------------------------
  //  ENUM            : xeona::SimRet
  // ---------------------------------------------------------
  //  Description  : encode the 'simulate' exist status
  //  Role         : end-of-application reporting
  // ---------------------------------------------------------

  enum SimRet
  {
    e_statusNotKnown     = 0,                // indeterminate state
    e_success            = 1,
    e_modelFileFault     = 2,
    e_infeasibility      = 3,                // thru solver failing or proven badness
    e_errantSimulation   = 4,
    e_testCodeUsed       = 8,                // meaning 'hash-ifdef' code and similar used
    e_other              = 9
  };

} // namespace 'xeona'

// ---------------------------------------------------------
//  FREE FUNCTION   : simulate
// ---------------------------------------------------------

xeona::SimRet                                // simulation return
simulate
(const std::string&   modelName,             // from command-line or default
 const xeona::SimKind simKind);              // from '--mode' option or default

#endif // _SIMCALL_H_

//  end of file

