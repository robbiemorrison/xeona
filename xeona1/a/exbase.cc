//  XEONA ENERGY SYSTEMS MODELING ENVIRONMENT
//
//  file-create-name : exbase.cc
//  file-create-date : Wed 24-Jun-2009 05:49 UTC
//  file-initiator   : Robbie Morrison <robbie@actrix.co.nz>
//
//  file-role        : xeona exception base class / implementation
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
//  $URL: file:///home/robbie/svn-root/xeona/futz/trunk/xeona1/a/exbase.cc $
//
//  GENERAL NOTES FOR THIS FILE

//  LOCAL AND SYSTEM INCLUDES

#include "exbase.h"           // companion header for this file (place first)

#include "../a/logger.h"      // standard logging functionality (as required)
#include "../c/smart_ptr.h"   // toggle between Boost and TR1 smart pointers
#include ".././common.h"      // common definitions for project (place last)

#include <string>             // C++ strings
#include <sstream>            // string-streams

//  CODE

namespace xeona
{
  // ---------------------------------------------------------
  //  CLASS           : xeona::exception (abstract base)
  // ---------------------------------------------------------

  exception::exception
  (const int exitcode) :
    d_code(exitcode),
    d_stringExpl(),
    d_stringTell()
  {
#if 1 // 1 = flush ostreams on construction, 0 = omit
    std::cout << std::flush;
    std::clog << std::flush;
#endif // 0
  }

  const std::string exception::expl() const { return d_stringExpl; }
  const std::string exception::tell() const { return d_stringTell; }
  const int         exception::code() const { return d_code;       }

  exception::~exception() { }                // definition necessary

} // namespace 'xeona'

//  end of file

