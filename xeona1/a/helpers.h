//  XEONA ENERGY SYSTEMS MODELING ENVIRONMENT
//
//  file-create-name : helpers.h
//  file-create-date : Mon 18-May-2009 11:08 UTC
//  file-initiator   : Robbie Morrison <robbie@actrix.co.nz>
//
//  file-role        : application helper functions / header
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
//  $URL: file:///home/robbie/svn-root/xeona/futz/trunk/xeona1/a/helpers.h $
//
//  GENERAL NOTES FOR THIS FILE
//
//  This unit contains functions and classes for use by the
//  application but which don't sensibly belong elsewhere.

//  HEADER GUARD

#ifndef _HELPERS_H_
#define _HELPERS_H_

//  LOCAL AND SYSTEM INCLUDES

#include "../d/siglp.h"       // semi-intelligent interface to GLPK MILP solver

#include "../a/logger_fwd.h"  // some forward declarations from "logger.h"
#include "../c/smart_ptr.h"   // toggle between Boost and TR1 smart pointers

#include <string>             // C++ strings
#include <sstream>            // string-streams

//  CODE

// ---------------------------------------------------------
//  FREE FUNCTION   : xeona::logRankToGlpkLevel
// ---------------------------------------------------------
//  Description  : logging rank (via '--report' option) to solver noise policy
//  Role         : optional use prior to creating new 'SolverIf' objects
//  Techniques   : switch statement
//  Status       : complete
//
//  Design notes
//
//      'svif::ReportingLevel' enum members
//      (taken from unit 'siglp' documentation)
//
//          not_specified = 0
//          silent        = 1    GLPK no output (although leakage may occur)
//          low           = 2    GLPK errors and warning
//          medium        = 3    GLPK normal             + problem info
//          high          = 4    GLPK normal plus info   + problem info plus GLPK report
//
//      These values equate to ONE more than the new GLPK
//      'msg_lev' integers GLP_MSG_{OFF,ERR,ON,ALL} (and the
//      undocumented GLP_MSG_DBG = 4) and the old 'LPX_K_MSGLEV'
//      parameter values.
//
//
//  CAUTION: required header
//
//      "d/siglp.h"       // semi-intelligent interface to GLPK MILP solver
//
// ---------------------------------------------------------

namespace xeona
{
  const svif::ReportingLevel
  logRankToGlpkLevel
  (const unsigned loggerRank);               // fundamentally 'logga::Rank'

} // namespace 'xeona'

#endif // _HELPERS_H_

//  end of file

