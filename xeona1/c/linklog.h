//  XEONA ENERGY SYSTEMS MODELING ENVIRONMENT
//
//  file-create-name : linklog.h
//  file-create-date : Thu 30-Jul-2009 21:25 UTC
//  file-initiator   : Robbie Morrison <robbie@actrix.co.nz>
//
//  file-role        : utility class to record entity linking results / header
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
//  $URL: file:///home/robbie/svn-root/xeona/futz/trunk/xeona1/c/linklog.h $
//
//  GENERAL NOTES FOR THIS FILE

//  HEADER GUARD

#ifndef _LINKLOG_H_
#define _LINKLOG_H_

//  LOCAL AND SYSTEM INCLUDES

#include "../c/xeona_ptr.h"   // remappable counted pointer which mimics shared_ptr
#include "../c/util1.h"       // free functions which offer general utilities 1

#include "../a/logger_fwd.h"  // some forward declarations from "logger.h"
#include "../c/smart_ptr.h"   // toggle between Boost and TR1 smart pointers

#include <iostream>           // standard io
#include <sstream>            // string-streams
#include <string>             // C++ strings
#include <typeinfo>           // run-time type information (RTTI)
#include <vector>             // STL sequence container

#include <boost/tuple/tuple.hpp>        // n-tuples, ref(), cref()

//  FORWARD (PARTIAL) DECLARATIONS

class Entity;

//  CODE

// ---------------------------------------------------------
//  CLASS           : LinkLogger
// ---------------------------------------------------------
//  Description  : small utility class to recover and hold entity linking details
//  Role         : used by static function 'Entity::linkAll' to create record
//  Techniques   : Boost.Tuple library, Boost.Format library, 'typeid'
//  Status       : complete
//
//  Design notes
//
//      This class represents a central place for recording
//      lazy-to-full link call details for later display.
//
//      The object is held as:
//
//          class Entity
//          private: static LinkLogger s_linkLog
//
//      This class does not report to the console.
//
//  CAUTION: tuple member function 'get<0>' not g++ 4.1.2
//
//      See note in code regarding this call.
//
// ---------------------------------------------------------

class LinkLogger
{
  // TYPEDEFS

private:

  typedef boost::tuple<std::string,               // entity identifier
                       std::string,               // entity type on construction
                       std::string,               // requested type cast on linking
                       std::string,               // raw address (zeros recorded)
                       std::string> entry_type;   // 'revamp' return

  // DISABLED

private:

  LinkLogger(const LinkLogger& orig);             // copy constructor
  LinkLogger& operator= (const LinkLogger& orig); // copy assignment operator

  // CREATORS

public:

  LinkLogger();
  ~LinkLogger();

  // ACCESSORS

  std::string                                // newline terminated
  recover                                    // dump database
  (const int tab = 4) const;                 // left indent

  int                                        // number of failed entries
  getNullCount() const;

  int                                        // number of successful entries
  getLogCount() const;

  // MANIPULATORS

  void
  reset();                                   // clear database, reestablish header

  bool                                       // simply return 'okay' value
  insert
  (const xeona::assign_ptr<Entity>& link,    // link entity
   const bool                       okay);   // return from 'revamp' call

  // INSTANCE DATA

private:

  unsigned                   d_linkNulls;    // number of null inserts
  std::vector<entry_type>    d_linkLog;      // database of live inserts

};

#endif // _LINKLOG_H_

//  end of file

