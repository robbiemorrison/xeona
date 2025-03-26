//  XEONA ENERGY SYSTEMS MODELING ENVIRONMENT
//
//  file-create-name : tictoc.cc
//  file-create-date : Thu 13-Nov-2008 20:26 UTC
//  file-initiator   : Robbie Morrison <robbie@actrix.co.nz>
//
//  file-role        : inherited interface for entities using common calls / implementat_n
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
//  $URL: file:///home/robbie/svn-root/xeona/futz/trunk/xeona1/b/tictoc.cc $
//
//  GENERAL NOTES FOR THIS FILE

//  LOCAL AND SYSTEM INCLUDES

#include "tictoc.h"           // companion header for this file (place first)

#include "../a/logger.h"      // standard logging functionality (as required)
#include "../c/smart_ptr.h"   // toggle between Boost and TR1 smart pointers
#include ".././common.h"      // common definitions for project (place last)

#include <string>             // C++ strings
#include <sstream>            // string-streams

#include <boost/logic/tribool.hpp>      // three state boolean
#include <boost/logic/tribool_io.hpp>   // CAUTION: essential for std::boolalpha reporting

//  CODE

// ---------------------------------------------------------
//  CLASS           : TicToc
// ---------------------------------------------------------
//  Description  : provide a call interface for common calls
//  Role         : to support a consistent usage across hosts classes
//  Techniques   : multiple inheritance, call to 'xeona::isTwoContained'
//  Status       : complete
//
//  Design notes
//
//      The 'TicToc' class provides a consistent simulation call
//      interface.  The class is inherited by the following
//      entities (noting that some of the sub-classes may not yet
//      actually exist):
//
//          'Block' sub-classes { 'TechnicalAsset' 'Gateway' 'Multibus' 'Environment' }
//          'Actor' sub-classes { 'AssetOperator' } but not 'DomainController'
//
// ---------------------------------------------------------

// STATIC DEFINITIONS

logga::spLogger TicToc::s_logger_tictoc = logga::ptrLogStream();

// CREATORS

TicToc::TicToc
(const int commitmentModeSum) :
  d_commitmentMode(xeona::e_modeNotSpecified),
  d_commitmentModeSum(commitmentModeSum),
  d_step(-1),                                // nonsensical value
  d_solver()                                 // empty smart pointer
{
  // CAUTION: good to log the commitment strategy sum at the debug threshold
  s_logger_tictoc->repx(logga::dbug,
                        "constructor call, commit mode sum",
                        d_commitmentModeSum);
}

TicToc::~TicToc()
{
  s_logger_tictoc->repx(logga::adhc, "destructor call", "");
}

// ---------------------------------------------------------
//  MEMBER FUNCTION : restructure
// ---------------------------------------------------------

void
TicToc::restructure
(const xeona::DomainMode commitmentMode)
{
  // initial reporting
  s_logger_tictoc->repx(logga::dbug, "entering member function", "");
  std::ostringstream put;
  put << "  restructure call arguments"                           << "\n"
      << "  commitment mode        : " << commitmentMode          << "\n";
  s_logger_tictoc->putx(logga::adhc, put);

  // update data members
  d_commitmentMode = commitmentMode;
}

// ---------------------------------------------------------
//  MEMBER FUNCTION : initialize
// ---------------------------------------------------------

void
TicToc::initialize
(const int                  step,
 shared_ptr<svif::SolverIf> solver)
{
  // initial reporting
  s_logger_tictoc->repx(logga::dbug, "entering member function", "");
  std::ostringstream put;
  put << "  initialize call arguments"                            << "\n"
      << "  step                   : " << step                    << "\n"
#ifndef _XUTEST // to avoid the need to link to unit 'siglp' when unit testing
      << "  solver optional tag    : " << solver->getProblemTag() << "\n"
#endif // _XUTEST
      << "  solver (pointer value) : " << solver                  << "\n";
  s_logger_tictoc->putx(logga::adhc, put);

  // update data members
  d_step           = step;
  d_solver         = solver;
}

// ---------------------------------------------------------
//  MEMBER FUNCTION : checkCommitmentMode
// ---------------------------------------------------------
//  Description  : tests for pure/sum miscibility and produces interpreted logging
//  Role         : provide sub-classes with a simple test
//  Techniques   : relies on 'xeona::isTwoContained'
//  Status       : complete
//
//  Design notes
//
//      Usage example
//
//          if ( ! checkCommitmentMode( d_commitmentMode, d_commitmentModeSum )
//          {
//            s_logger->repx(logga::warn, "commitment mode problem", "");
//          }
//
// ---------------------------------------------------------

bool                                         // 'false' is problematic
TicToc::checkCommitmentMode                  // non-virtual function
(const xeona::DomainMode pure,               // value under test
 const xeona::DomainMode sum)                // miscible values aggregate
{
  // establish input details for later reporting
  std::ostringstream oss;
  oss << pure << " : " << sum;

  // short circuit if test value not specified
  if ( pure == xeona::e_modeNotSpecified )
    {
      s_logger_tictoc->repx(logga::warn, "commitment unspecified, test : sum", oss.str());
      return false;
    }

  // call test function
  const tribool ret = xeona::isTwoContained(pure, sum);

  // CAUTION: three-state testing of a 'boost::logic::tribool'
  // value is NOT intuitive -- in particular, see the relevant
  // documentation file 'DOCS/CODE.txt'

  if ( ret )                                 // 'true'
    {
      s_logger_tictoc->repx(logga::adhc, "miscible commitment, test : sum", oss.str());
      return true;
    }
  else if ( !ret )                           // 'false' means commitment immiscibility
    {
      s_logger_tictoc->repx(logga::dbug, "immiscible commitment, test : sum", oss.str());
      return false;
    }
  else                                       // 'indeterminate' means data problem
    {
      s_logger_tictoc->repx(logga::dbug, "data problem, test : sum", oss.str());
      return false;
    }
}

//  end of file

