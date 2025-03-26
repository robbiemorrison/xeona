//  XEONA ENERGY SYSTEMS MODELING ENVIRONMENT
//
//  file-create-name : exitstat.h
//  file-create-date : Thu 14-May-2009 07:11 UTC
//  file-initiator   : Robbie Morrison <robbie@actrix.co.nz>
//
//  file-role        : exit status database / header
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
//  $URL: file:///home/robbie/svn-root/xeona/futz/trunk/xeona1/a/exitstat.h $
//
//  GENERAL NOTES FOR THIS FILE

//  HEADER GUARD

#ifndef _EXITSTAT_H_
#define _EXITSTAT_H_

//  LOCAL AND SYSTEM INCLUDES

#include "../a/logger_fwd.h"  // some forward declarations from "logger.h"
#include "../c/smart_ptr.h"   // toggle between Boost and TR1 smart pointers

#include <map>                // STL associative container
#include <string>             // C++ strings
#include <sstream>            // string-streams

//  CODE

// ---------------------------------------------------------
//  CLASS           : ExitStatus
// ---------------------------------------------------------
//  Description  : stand-alone class holding application return interpretations
//  Role         : used by option '--output'
//  Techniques   : (nothing special)
//  Status       : complete
// ---------------------------------------------------------

class ExitStatus
{
  // TYPEDEFS

private:

  typedef std::map<int, std::string> database_type;

  // DISABLED

private:

  ExitStatus(const ExitStatus& orig);                  // copy constructor
  ExitStatus& operator= (const ExitStatus& orig);      // copy assignment operator

  // CREATORS

public:

  ExitStatus
  (const std::string notPresentMessage = "(not supported)");

  ~ExitStatus();

  // ACCESSORS

  bool                                       // return 'false' if not present
  operator()
  (const int    exitStatus,
   std::string& exitInterpretation) const;

  std::string
  notPresentMessage() const;

  int
  size() const;

  // UTILITY FUNCTIONS

private:

  void
  add
  (const int         exitStatus,
   const std::string exitInterpretation);

  // INSTANCE DATA

private:

  database_type              d_data;
  const std::string          d_notPresentMessage;

  // INSTANCE DATA

private:

  static logga::spLogger     s_logger;            // shared_ptr to single logger object

};

#endif // _EXITSTAT_H_

//  end of file

