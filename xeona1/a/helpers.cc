//  XEONA ENERGY SYSTEMS MODELING ENVIRONMENT
//
//  file-create-name : helpers.cc
//  file-create-date : Mon 18-May-2009 11:08 UTC
//  file-initiator   : Robbie Morrison <robbie@actrix.co.nz>
//
//  file-role        : application helper functions / implementation
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
//  $URL: file:///home/robbie/svn-root/xeona/futz/trunk/xeona1/a/helpers.cc $
//
//  GENERAL NOTES FOR THIS FILE

//  LOCAL AND SYSTEM INCLUDES

#include "helpers.h"          // companion header for this file (place first)
#include "../a/logger.h"      // standard logging functionality (as required)
#include "../c/smart_ptr.h"   // toggle between Boost and TR1 smart pointers
#include ".././common.h"      // common definitions for project (place last)
#include <string>             // C++ strings
#include <sstream>            // string-streams

//  CODE

// ---------------------------------------------------------
//  FREE FUNCTION   : xeona::logRankToGlpkLevel
// ---------------------------------------------------------

namespace xeona
{
  const svif::ReportingLevel
  logRankToGlpkLevel
  (const unsigned loggerRank)                // fundamentally 'logga::Rank'
  {
    // logging support
    static logga::spLogger logger = logga::ptrLogStream();
    logger->repx(logga::xtra, "entering member function", "");

    // declare return variable
    svif::ReportingLevel glpkLevel = svif::not_specified;

    // set the policy here
    switch ( loggerRank )
      {
      case logga::yeek: glpkLevel = svif::silent; break;
      case logga::kill: glpkLevel = svif::low;    break;
      case logga::warn: glpkLevel = svif::low;    break;
      case logga::info: glpkLevel = svif::high;   break;
      case logga::dbug: glpkLevel = svif::high;   break;
      case logga::xtra: glpkLevel = svif::high;   break;
      case logga::adhc: glpkLevel = svif::high;   break;
      default:
        std::clog << "** coding error 01 in source file " << __FILE__ << std::endl;
        break;
      }

    // for reporting purposes
    std::string glpkInterpretation;
    switch ( glpkLevel )
      {
      case svif::silent: glpkInterpretation = "no output (leakage may occur)"; break;
      case svif::low:    glpkInterpretation = "errors and warnings";           break;
      case svif::medium: glpkInterpretation = "normal (without added info)";   break;
      case svif::high:   glpkInterpretation = "all (including added info)";    break;
      default:
        std::clog << "** coding error 02 in source file " << __FILE__ << std::endl;
        break;
      }

    // additional reporting as appropriate
    // YEEK 16 CODE (set by '--yeek')
    if ( xeona::yeek == 16 || xeona::yeek == 1 || xeona::yeek == 2 )
      {
        std::ostringstream put;
        put << "  log rank to GLPK level"                 << "\n"
            << "    logger rank : " << loggerRank         << "\n"
            << "    GLPK level  : " << glpkInterpretation << "\n";
        logger->repx(logga::dbug, "additional reporting follows, yeek", xeona::yeek);
        logger->putx(logga::dbug, put);
      }
    else                                     // normal code
      {
        // completion reporting
        logger->repx(logga::xtra, "using current logger rank"    , loggerRank);
        logger->repx(logga::xtra, "returning GLPK interpretation", glpkInterpretation);
      }

    // return value
    return glpkLevel;
  }
}

//  end of file

