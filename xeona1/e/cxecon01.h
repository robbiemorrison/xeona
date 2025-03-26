//  XEONA ENERGY SYSTEMS MODELING ENVIRONMENT
//
//  file-create-name : cxecon01.h
//  file-create-date : Thu 07-May-2009 08:16 UTC
//  file-initiator   : Robbie Morrison <robbie@actrix.co.nz>
//
//  file-role        : concrete economic contexts 1 / header
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
//  $URL: file:///home/robbie/svn-root/xeona/futz/trunk/xeona1/e/cxecon01.h $
//
//  GENERAL NOTES FOR THIS FILE

//  HEADER GUARD

#ifndef _CXECON01_H_
#define _CXECON01_H_

//  LOCAL AND SYSTEM INCLUDES

#include "../e/context.h"     // abstract context entity

#include "../a/logger_fwd.h"  // some forward declarations from "logger.h"
#include "../c/smart_ptr.h"   // toggle between Boost and TR1 smart pointers

#include <string>             // C++ strings
#include <sstream>            // string-streams

//  FORWARD (PARTIAL) DECLARATIONS

class Record;                                // constructor argument

//  CODE

// ---------------------------------------------------------
//  CLASS           : CxCommercial (abstract)
// ---------------------------------------------------------
//  Description  : interface base for commercial contexts
//  Role         : step in the inheritance web
//  Techniques   : pure virtual functions
//  Status       : incomplete
// ---------------------------------------------------------

class CxCommercial :
  public EconomicContext
{
  // DISABLED

private:

  CxCommercial();                                      // zero-argument constructor
  CxCommercial(const CxCommercial& orig);              // copy constructor
  CxCommercial& operator= (const CxCommercial& orig);  // copy assignment operator

  // CREATORS

public:

  explicit
  CxCommercial
  (const std::string entityId,
   Record&           record);

  virtual
  ~CxCommercial() = 0;                       // create abstract class

  // ACCESSORS

public:

  virtual
  double                                     // decimal form unit [1/y], say 0.2
  getComInterestRate() const = 0;

};

// ---------------------------------------------------------
//  CLASS           : CxCommercialFix
// ---------------------------------------------------------

class CxCommercialFix :
  public CxCommercial
{
  // DISABLED

  CxCommercialFix();                                        // zero-argument constructor
  CxCommercialFix(const CxCommercialFix& orig);             // copy constructor
  CxCommercialFix& operator= (const CxCommercialFix& orig); // copy assignment operator

  // CREATORS

public:

  explicit
  CxCommercialFix
  (const std::string entityId,
   Record&           record);

  virtual
  ~CxCommercialFix();

  // ACCESSORS

public:

  virtual
  double                                     // decimal form unit [1/y], say 0.2
  getComInterestRate() const;

  // INSTANCE DATA

private:

  const double&    d_comInterestRate;

};

//  ==== XEDOC =================================================
//
//  entity.cx-commercial-fix-0
//
//      class                                    > CxCommercialFix
//
//        commercial context using fixed data
//
//      builtin-remark s                         <
//
//      commercial-interest-rate [1/y] f         > 0.2
//
//  ============================================================

#endif // _CXECON01_H_

//  end of file

