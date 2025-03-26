//  XEONA ENERGY SYSTEMS MODELING ENVIRONMENT
//
//  file-create-name : yeek.h
//  file-create-date : Tue 17-Nov-2009 11:32 UTC
//  file-initiator   : Robbie Morrison <robbie@actrix.co.nz>
//
//  file-role        : yeek (for running extra code) value interpretation / header
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
//  $URL: file:///home/robbie/svn-root/xeona/futz/trunk/xeona1/a/yeek.h $
//
//  GENERAL NOTES FOR THIS FILE

//  HEADER GUARD

#ifndef _YEEK_H_
#define _YEEK_H_

//  LOCAL AND SYSTEM INCLUDES

#include "../a/logger_fwd.h"  // some forward declarations from "logger.h"
#include "../c/smart_ptr.h"   // toggle between Boost and TR1 smart pointers

#include <string>             // C++ strings
#include <sstream>            // string-streams

//  CODE

// ---------------------------------------------------------
//  FREE FUNCTION   : xeona::yeekInterpret
// ---------------------------------------------------------
//  Description  : provide interpretation for given a 'xeona::yeek' value
//  Role         : called from function 'main' if option '--yeek' is deployed
//  Techniques   : function-like preprocessor macro, 'switch' construct
//  Status       : complete
// ---------------------------------------------------------

namespace xeona
{
  const std::string                          // empty string if 'yeekNo' not present
  yeekInterpret
  (const unsigned yeekNo);
}

// ---------------------------------------------------------
//  FREE FUNCTION   : xeona::yeekSummarize
// ---------------------------------------------------------
//  Description  : provide formatted summary of all supported 'xeona::yeek' values
//  Role         : called from function 'main' if a false '--yeek' option is deployed
//  Techniques   : cycles thru 'xeona::yeekInterpret' calls
//  Status       : complete
// ---------------------------------------------------------

namespace xeona
{
  const std::string                          // contains trailing newline
  yeekSummarize
  (const int indent);

} // namespace 'xeona'

#endif // _YEEK_H_

//  end of file

