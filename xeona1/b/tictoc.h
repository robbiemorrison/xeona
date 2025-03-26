//  XEONA ENERGY SYSTEMS MODELING ENVIRONMENT
//
//  file-create-name : tictoc.h
//  file-create-date : Thu 13-Nov-2008 20:26 UTC
//  file-initiator   : Robbie Morrison <robbie@actrix.co.nz>
//
//  file-role        : inherited interface for entities using common calls / header
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
//  $URL: file:///home/robbie/svn-root/xeona/futz/trunk/xeona1/b/tictoc.h $
//
//  GENERAL NOTES FOR THIS FILE

//  HEADER GUARD

#ifndef _TICTOC_H_
#define _TICTOC_H_

//  LOCAL AND SYSTEM INCLUDES

#include "../a/logger_fwd.h"  // some forward declarations from "logger.h"
#include "../c/smart_ptr.h"   // toggle between Boost and TR1 smart pointers

#include "../f/ospmodes.h"    // domain mode enums (header only)
#include "../b/optprob.h"     // optimization sub-problem and key sub-classes
#include "../c/costs.h"       // cost sets and support
#include "../c/util2.h"       // free functions which offer general utilities 2

#include <string>             // C++ strings

//  FORWARD (PARTIAL) DECLARATIONS

class CostSet;                               // "c/costs.h"
namespace svif { class SolverIf; }           // "d/siglp.h"

//  CODE

// ---------------------------------------------------------
//  CLASS           : TicToc
// ---------------------------------------------------------
//  Description  : provide a call interface for common calls
//  Role         : to support a consistent usage across hosts classes
//  Techniques   : multiple inheritance, call to 'xeona::isTwoContained'
//  Status       : complete
//
//  CAUTION: see the design notes in the implementation file too.
//
//      This class supplies the protected member functions and data:
//
//          'establish'            beginning of horizon
//          'initialize'           beginning of interval
//          'constrain'            following shortly after 'initialize' call
//          'washup'               end of interval
//          'conclude'             end of horizon
//
//          'd_step'               current interval count
//          'd_solver'             current solver interface object
//
//      and also supplies and sets the data members:
//
//          'd_commitmentMode'     current commitment mode (aka control strategy)
//          'd_commitmentModeSum'  supported commitment modes
//
//      The clients of this class are as follows:
//
//          currently inherited by:
//
//              'AssetOperator'
//              'Gateway'
//              'TechnicalAsset'
//
//          to be inherited by perhaps:
//
//              'Environment'
//              'Multibus'
//
//          but NOT INHERITED by:
//
//              'DomainController' which implements its own versions
//
//          and NOT REQUIRED by:
//
//              'Overseer' singleton which just contains 'run'
//
// ---------------------------------------------------------

class TicToc
{
  // DISABLED

private:

  TicToc();                                  // zero-argument constructor
  TicToc(const TicToc& orig);                // copy constructor
  TicToc& operator= (const TicToc& orig);    // copy assignment operator

  // CREATORS

public:

  TicToc
  (const int commstratSum);                  // fixed on construction

  virtual
  ~TicToc();                                 // destructor

  // BEGINNING OF HORIZON CALL - ABSTRACT

  virtual
  void
  establish() = 0;

  // STRUCTURE CHANGE CALL - PRIMARILY DEFINED HERE

  virtual                                    // because 'AssetOperator::restructure' loops
  void
  restructure
  (const xeona::DomainMode commitmentMode);

  // LOOP CALLS - PRIMARILY DEFINED HERE

  virtual                                    // because 'AssetOperator::initialize' loops
  void
  initialize
  (const int                  step,          // CAUTION: the step count is ZERO-based
   shared_ptr<svif::SolverIf> solver);       // solver instance passed thru

  // LOOP CALLS - ABSTRACT

  virtual
  const int                                  // return interpretation is host-dependent
  constrain                                  // set constraints
  (const xeona::DomainMode capacityMode) = 0;

  virtual
  void
  washup() = 0;

  // END OF HORIZON CALL - ABSTRACT

  virtual
  void
  conclude() = 0;

  // UTILITY FUNCTIONS

protected:

  bool                                       // 'false' is problematic
  checkCommitmentMode                        // non-virtual function
  (const xeona::DomainMode pure,             // value under test
   const xeona::DomainMode sum);             // miscible values aggregate

  // INSTANCE DATA

protected:

  xeona::DomainMode             d_commitmentMode;      // current commitment strategy
  const int                     d_commitmentModeSum;   // supported commitment strategies

  int                           d_step;                // interval count used by clients
  shared_ptr<svif::SolverIf>    d_solver;              // solver object used by clients

  // STATIC DATA

private:

  static logga::spLogger s_logger_tictoc;    // [1] CAUTION

  // [1] alternative logger name: the trailing "tictoc" is to
  // avoid a namespace collision with the logger from the other
  // partner

};

#endif // _TICTOC_H_

//  end of file

