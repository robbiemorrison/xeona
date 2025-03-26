//  XEONA ENERGY SYSTEMS MODELING ENVIRONMENT
//
//  file-create-name : exbase.h
//  file-create-date : Wed 24-Jun-2009 05:49 UTC
//  file-initiator   : Robbie Morrison <robbie@actrix.co.nz>
//
//  file-role        : xeona exception base class / header
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
//  $URL: file:///home/robbie/svn-root/xeona/futz/trunk/xeona1/a/exbase.h $
//
//  GENERAL NOTES FOR THIS FILE
//
//  See file 'a/exapp.h' for overview documentation.

//  HEADER GUARD

#ifndef _EXBASE_H_
#define _EXBASE_H_

//  LOCAL AND SYSTEM INCLUDES

#include "../a/logger_fwd.h"  // some forward declarations from "logger.h"
#include "../c/smart_ptr.h"   // toggle between Boost and TR1 smart pointers

#include <string>             // C++ strings
#include <sstream>            // string-streams

//  CODE

namespace xeona
{
  // ---------------------------------------------------------
  //  CLASS           : xeona::exception (abstract base)
  // ---------------------------------------------------------
  //  Description  : abstract base class for exceptions
  //  Role         : codebase-wide exception handling
  //  Techniques   : exception class, inheritance tree
  //  Status       : complete
  // ---------------------------------------------------------

  class exception
  {
  private:

    exception();
    exception& operator= (const exception& orig);      // copy assignment operator

  public:

    exception
    (const int exitcode);

    virtual ~exception() = 0;                // create abstract class

    const std::string expl() const;
    const std::string tell() const;
    const int         code() const;

  protected:

    const int      d_code;              // hardcoded in constructors
    std::string    d_stringExpl;        // formatted on construction, returned by 'expl'
    std::string    d_stringTell;        // formatted on construction, returned by 'tell'

  };

} // namespace 'xeona'

#endif // _EXBASE_H_

//  end of file

