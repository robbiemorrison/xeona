//  XEONA ENERGY SYSTEMS MODELING ENVIRONMENT
//
//  file-create-name : inbuilt.h
//  file-create-date : Mon 14-Jan-2008 15:28 UTC
//  file-initiator   : Robbie Morrison <robbie@actrix.co.nz>
//
//  file-role        : compiled-in test file generation / header
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
//  $URL: file:///home/robbie/svn-root/xeona/futz/trunk/xeona1/c/inbuilt.h $

//  HEADER GUARD

#ifndef _INBUILT_H_
#define _INBUILT_H_

//  LOCAL AND SYSTEM INCLUDES

#include "../a/logger_fwd.h"  // some forward declarations from "logger.h"
#include "../c/smart_ptr.h"   // toggle between Boost and TR1 smart pointers

#include <fstream>            // file-based io
#include <string>             // C++ strings

//  CODE

namespace xeona
{
  std::string                                // often leaf but directory part will remain
  createModelName
  (const std::string& stub);                 // empty string uses inbuilt default

  bool                                       // 'false' means file open failed
  dumpToFile                                 // calls 'loadOstream' internally
  (const std::string& filename,              // will overwrite file, maybe with zero bytes
   const unsigned     steps);                // horizon steps

  void
  loadOstream                                // helper func, contains model in text form
  (std::ostream&  os,
   const unsigned steps);                    // horizon steps

} // namespace xeona

#endif // _INBUILT_H_

//  end of file

