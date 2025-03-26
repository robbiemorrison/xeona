//  XEONA ENERGY SYSTEMS MODELING ENVIRONMENT
//
//  file-create-name : app_except.cc
//  file-create-date : Tue 28-Apr-2009 10:15 UTC
//  file-initiator   : Robbie Morrison <robbie@actrix.co.nz>
//
//  file-role        : entity exception classes / implementation
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
//  $URL: file:///home/robbie/svn-root/xeona/futz/trunk/xeona1/a/exent.cc $
//
//  GENERAL NOTES FOR THIS FILE

//  LOCAL AND SYSTEM INCLUDES

#include "exent.h"            // companion header for this file (place first)

#include "../c/smart_ptr.h"   // toggle between Boost and TR1 smart pointers
#include ".././common.h"      // common definitions for project (place last)

#include <string>             // C++ strings
#include <sstream>            // string-streams

#include <boost/algorithm/string.hpp>   // string recasing, trimming, splitting

//  CODE

namespace xeona
{
  // ---------------------------------------------------------
  //  CLASS           : xeona::ent_exception (abstract)
  // ---------------------------------------------------------

  ent_exception::ent_exception() :
    exception(xeona::exit_entity_fail)       // only this exit code is supported
  {
  }

  ent_exception::~ent_exception()            // definition necessary
  {
  }

  // ---------------------------------------------------------
  //  CLASS           : xeona::entity_issue
  // ---------------------------------------------------------

  entity_issue::entity_issue
  (const std::string msgExpl)      // should be suitably formatted with trailing newline
  {
    std::ostringstream oss;
    oss
      << "** xeona entity issue exception (variant 1)"                          << "\n"
      << "   "                         << msgExpl                               << "\n"
      << "   hardcoded exit status : " << d_code                                << "\n";

    d_stringExpl = oss.str();
    d_stringTell = "xeona::entity_issue exception";
  }

  // ---------------------------------------------------------
  //  CLASS           : xeona::entity_issue_2
  // ---------------------------------------------------------

  entity_issue_2::entity_issue_2
  (const std::string klass,
   const std::string function,
   const std::string identifier,
   const std::string msgLine)
  {
    std::ostringstream oss;
    oss
      << "** xeona entity issue exception (variant 2)"                          << "\n"
      << "       class      : " << klass                                        << "\n"
      << "       function   : " << function                                     << "\n"
      << "       identifier : " << identifier                                   << "\n"
      << "       message    : " << msgLine                                      << "\n"
      << "   hardcoded exit status : " << d_code                                << "\n";

    d_stringExpl = oss.str();
    d_stringTell = "xeona::entity_issue exception_2";
  }

  // ---------------------------------------------------------
  //  CLASS           : xeona::bidset_selections
  // ---------------------------------------------------------

  bidset_selections::bidset_selections
  (const std::string klass,
   const std::string function,
   const std::string myId,
   const int         min,
   const int         max,
   const int         count)
  {
    std::ostringstream oss;
    oss
      << "** xeona bidset selections invalid"                                   << "\n"
      << "       indentifier : " << myId                                        << "\n"
      << "       class       : " << klass                                       << "\n"
      << "       function    : " << function                                    << "\n"
      << "       min         : " << min                                         << "\n"
      << "       max         : " << max                                         << "\n"
      << "       count       : " << count                                       << "\n"
      << "   hardcoded exit status : " << d_code                                << "\n";

    d_stringExpl = oss.str();
    d_stringTell = "xeona::bidset_selection";
  }

} // namespace 'xeona'

//  end of file

