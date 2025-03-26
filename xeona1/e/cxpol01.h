//  XEONA ENERGY SYSTEMS MODELING ENVIRONMENT
//
//  file-create-name : cxpubpol01.h
//  file-create-date : Thu 07-May-2009 08:27 UTC
//  file-initiator   : Robbie Morrison <robbie@actrix.co.nz>
//
//  file-role        : concrete public policy contexts 1 / header
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
//  $URL: file:///home/robbie/svn-root/xeona/futz/trunk/xeona1/e/cxpol01.h $
//
//  GENERAL NOTES FOR THIS FILE
//
//  The contexts here cover public policy measures.

//  HEADER GUARD

#ifndef _CXPUBPOL01_H_
#define _CXPUBPOL01_H_

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
//  CLASS           : CxPublicPolicyA
// ---------------------------------------------------------

class CxPublicPolicyA :
  public PublicPolicyContext
{
  // DISABLED

private:

  CxPublicPolicyA();                                        // zero-argument constructor
  CxPublicPolicyA(const CxPublicPolicyA& orig);             // copy constructor
  CxPublicPolicyA& operator= (const CxPublicPolicyA& orig); // copy assignment operator

  // CREATORS

public:

  explicit
  CxPublicPolicyA
  (const std::string entityId,
   Record&           record);

  virtual
  ~CxPublicPolicyA() = 0;                    // create abstract class

};

//  ==== xedoc =================================================
//
//  entity.cx-public-policy-a-0
//
//      class                                    > CxPublicPolicyA
//
//        public policy context
//
//      builtin-remark s                         <
//
//  ============================================================

#endif // _CXPUBPOL01_H_

//  end of file

