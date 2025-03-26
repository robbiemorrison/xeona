//  XEONA ENERGY SYSTEMS MODELING ENVIRONMENT
//
//  file-create-name : files.h
//  file-create-date : Fri 09-Nov-2007 13:38 UTC
//  file-initiator   : Robbie Morrison <robbie@actrix.co.nz>
//
//  file-role        : free functions for regular files / header
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
//  $URL: file:///home/robbie/svn-root/xeona/futz/trunk/xeona1/c/files.h $

//  GENERAL NOTES FOR THIS FILE
//
//  The functions here are (inadvertent mistakes aside)
//  POSIX-compliant.  They should therefore be acceptable on
//  Windows (Win32 API and better) as well as on UNIX and Linux.

//  HEADER GUARD

#ifndef _FILES_H_
#define _FILES_H_

//  LOCAL AND SYSTEM INCLUDES

#include "../a/logger_fwd.h"  // some forward declarations from "logger.h"
#include "../c/smart_ptr.h"   // toggle between Boost and TR1 smart pointers

#include <string>             // C++ strings
#include <sstream>            // string-streams

#include <boost/filesystem.hpp>         // path objects, iterators, useful operations

#include <boost/logic/tribool.hpp>      // three state boolean
#include <boost/logic/tribool_io.hpp>   // CAUTION: essential for std::boolalpha reporting

//  NAMESPACE DECLARATIONS

using boost::logic::tribool;            // three state boolean
using boost::logic::indeterminate;      // allows 'indeterminate' and 'indeterminate()'

//  CODE

namespace xeona
{
  // ---------------------------------------------------------
  //  FREE FUNCTION   : xeona::testWritable
  // ---------------------------------------------------------
  //  Description  : check if a given Boost.Filesystem path is writable
  // ---------------------------------------------------------

  tribool                                    // indeterminate means status not resolved
  testWritable
  (boost::filesystem::path regularFile);     // this function assumes the file is regular

  // ---------------------------------------------------------
  //  FREE FUNCTION   : xeona::establishModelFile
  // ---------------------------------------------------------
  //  Description  : build path and confirm model file or show failure
  // ---------------------------------------------------------

  boost::filesystem::path                    // empty path indicates failure
  establishModelFile                         // assemble path and confirm model file
  (const std::string& modelStub);            // from command-line or "" to use default

  // ---------------------------------------------------------
  //  FREE FUNCTION   : xeona::readonly
  // ---------------------------------------------------------
  //  Description  : chmod given 'filename' to 'newMode' (0440 set in implementation)
  //  Role         : general use (but often after various GLPK file creation calls)
  //  Techniques   : POSIX 'chmod'
  //  Status       : complete
  // ---------------------------------------------------------

  bool                                       // 'false' if unsuccessful
  readonly                                   // wrapper function
  (const char* filename);

  bool                                       // 'false' if unsuccessful
  readonly                                   // principal call
  (const std::string& filename);

  // ---------------------------------------------------------
  //  FREE FUNCTION   : xeona::temporaryFilename
  // ---------------------------------------------------------
  //  Description  : create temporary filename
  //  Role         : general use (used to create gnuplot names)
  //  Techniques   : Boost.Filesystem V3 'boost::filesystem::unique_path'
  //  Status       : complete
  // ---------------------------------------------------------

  const std::string
  temporaryFilename
  (const std::string& extension = "",        // note default
   const std::string& stub      = "");       // note default

} // namespace xeona

#endif // _FILES_H_

//  end of file

