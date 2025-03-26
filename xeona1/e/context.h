//  XEONA ENERGY SYSTEMS MODELING ENVIRONMENT
//
//  file-create-name : context.h
//  file-create-date : Wed 06-May-2009 21:11 UTC
//  file-initiator   : Robbie Morrison <robbie@actrix.co.nz>
//
//  file-role        : abstract context entity / header
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
//  $URL: file:///home/robbie/svn-root/xeona/futz/trunk/xeona1/e/context.h $
//
//  GENERAL NOTES FOR THIS FILE

//  HEADER GUARD

#ifndef _CONTEXT_H_
#define _CONTEXT_H_

//  AD-HOC NOTES

//  LOCAL AND SYSTEM INCLUDES

#include "../b/entity.h"      // entity base class plus lazy linking

#include "../a/logger_fwd.h"  // some forward declarations from "logger.h"
#include "../c/smart_ptr.h"   // toggle between Boost and TR1 smart pointers

#include <string>             // C++ strings
#include <sstream>            // string-streams

//  FORWARD (PARTIAL) DECLARATIONS

class Record;                                // constructor argument

//  CODE

// ---------------------------------------------------------
//  CLASS           : Context (abstract proto-context class)
// ---------------------------------------------------------
//  Description  : abstract class for context entities
//  Role         : step in the inheritance web
//  Techniques   : pure virtual destructor
//  Status       : complete
//
//  Design notes
//
//      This class provides for the following abstract classes:
//
//          - 'AmbientConditions'
//          - 'EconomicContext'
//          - 'PublicPolicyContext'
//
// ---------------------------------------------------------

class Context :
  public FullEntity
{
  // DISABLED

private:

  Context();                                 // zero-argument constructor
  Context(const Context& orig);              // copy constructor
  Context& operator= (const Context& orig);  // copy assignment operator

  // CREATORS

public:

  explicit
  Context(const std::string entityId) :
    FullEntity(entityId)
  { }

  explicit
  Context
  (const std::string entityId,
   Record&           record);

  virtual
  ~Context() = 0;                            // create abstract class

  // CALL

  virtual
  void
  setup();                                   // like 'establish' call for other entities

  // INTERNAL DATA

protected:

};

// ---------------------------------------------------------
//  CLASS           : AmbientConditions
// ---------------------------------------------------------

class AmbientConditions :
  public Context
{
  // DISABLED

private:

  AmbientConditions(const AmbientConditions& orig);              // copy constructor
  AmbientConditions& operator= (const AmbientConditions& orig);  // copy assignment oper

  // CREATORS

public:

  explicit
  AmbientConditions(const std::string entityId) :
    Context(entityId)
  { }

  explicit
  AmbientConditions
  (const std::string entityId,
   Record&           record);

  virtual
  ~AmbientConditions() = 0;                  // create abstract class

};

// ---------------------------------------------------------
//  CLASS           : EconomicContext
// ---------------------------------------------------------

class EconomicContext :
  public Context
{
  // DISABLED

private:

  EconomicContext();                                             // zero-argument ctor
  EconomicContext(const EconomicContext& orig);                  // copy constructor
  EconomicContext& operator= (const EconomicContext& orig);      // copy assignment oper

  // CREATORS

public:

  explicit
  EconomicContext
  (const std::string entityId,
   Record&           record);

  virtual
  ~EconomicContext() = 0;                    // create abstract class

};

// ---------------------------------------------------------
//  CLASS           : PublicPolicyContext
// ---------------------------------------------------------

class PublicPolicyContext :
  public Context
{
  // DISABLED

private:

  PublicPolicyContext();                                            // zero-argument ctor
  PublicPolicyContext(const PublicPolicyContext& orig);             // copy constructor
  PublicPolicyContext& operator= (const PublicPolicyContext& orig); // copy assignment ope

  // CREATORS

public:

  explicit
  PublicPolicyContext
  (const std::string entityId,
   Record&           record);

  virtual
  ~PublicPolicyContext() = 0;                // create abstract class

};

#endif // _CONTEXT_H_

//  end of file

