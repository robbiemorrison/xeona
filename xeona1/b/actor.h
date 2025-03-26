//  XEONA ENERGY SYSTEMS MODELING ENVIRONMENT
//
//  file-create-name : actor.h
//  file-create-date : Mon 25-Aug-2008 10:34 UTC
//  file-initiator   : Robbie Morrison <robbie@actrix.co.nz>
//
//  file-role        : abstract actor entity / header
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
//  $URL: file:///home/robbie/svn-root/xeona/futz/trunk/xeona1/b/actor.h $

//  HEADER GUARD

#ifndef _ACTOR_H_
#define _ACTOR_H_

//  LOCAL AND SYSTEM INCLUDES

#include "../b/entity.h"      // entity base class plus lazy linking
#include "../b/costreg.h"     // cost registers

#include "../a/logger_fwd.h"  // some forward declarations from "logger.h"
#include "../c/smart_ptr.h"   // toggle between Boost and TR1 smart pointers

#include <string>             // C++ strings
#include <sstream>            // string-streams

//  FORWARD (PARTIAL) DECLARATIONS

class Record;                                // constructor argument

//  CODE

// ---------------------------------------------------------
//  CLASS           : Actor (abstract almost-base)
// ---------------------------------------------------------
//  Description  : abstract class for all actors
//  Role         : step in the inheritance web
//  Techniques   : pure virtual destructor
//  Status       : complete
//
//  Design notes
//
// ---------------------------------------------------------

class Actor :
  public FullEntity,
  public virtual CostRegister                // provides the set of registers
{
  // DISABLED

private:

  Actor();                                   // zero-argument constructor
  Actor(const Actor& orig);                  // copy constructor
  Actor& operator= (const Actor& orig);      // copy assignment operator

  // CREATORS

public:

  explicit
  Actor
  (const std::string entityId,
   Record&           record);

  virtual
  ~Actor() = 0;                              // create abstract class

};

#endif // _ACTOR_H_

//  end of file

