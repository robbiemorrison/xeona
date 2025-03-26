//  XEONA ENERGY SYSTEMS MODELING ENVIRONMENT
//
//  file-create-name : xemrun.h
//  file-create-date : Tue 12-Aug-2008 13:31 UTC
//  file-initiator   : Robbie Morrison <robbie@actrix.co.nz>
//
//  file-role        : class to write and run XEM unit test models / header
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
//  $URL: file:///home/robbie/svn-root/xeona/futz/trunk/xeona1/c/xemrun.h $
//
//  GENERAL NOTES FOR THIS FILE
//
//  This unit is not intended to be part of the application
//  proper (but could nonetheless be included and used).

//  HEADER GUARD

#ifndef _XEMRUN_H_
#define _XEMRUN_H_

//  LOCAL AND SYSTEM INCLUDES

#include "../a/logger_fwd.h"  // some forward declarations from "logger.h"
#include "../c/smart_ptr.h"   // toggle between Boost and TR1 smart pointers

#include <iostream>           // standard io
#include <sstream>            // string-streams
#include <string>             // C++ strings

#include <boost/filesystem.hpp>         // path objects, iterators, useful operations

//  CODE

namespace
{
  const std::string xemrunBinaryName   = "xeona.mach";
  const std::string xemrunLocalDirName = "xeona";
  const std::string xemrunBaseDirName  = "/home/robbie/synk/xeona/svn2/futz/trunk";
  // was for 'sojus:' "/home/robbie/synk/xeona/svn/futz"
}

// ---------------------------------------------------------
//  CLASS           : XemRun
// ---------------------------------------------------------
//  Description  : write XEM string to file and call nominated 'xeona' binary
//  Role         : unit test support
//  Robustness   : quite a lot of fault tolerance and reporting provided
//  Techniques   : Boost.Filesystem library
//  Status       : complete
//
//  Design notes
//
//      The class 'XemRun' allows unit test files to write out
//      and run test models during unit testing.  A 'XemRun'
//      object would normally be used in conjunction with a
//      'XemGenerator' object, as follows.
//
//          XemGenerator x;
//          ..
//          xr.write(x.string());
//
//      The intention is to call the constructor with the
//      preprocessor macro, as follows, so that the caller file
//      name can be embedded in the model file name:
//
//          XemRun xr(__FILE__);
//
//      See the unit test file for more details regarding usage.
//
// ---------------------------------------------------------

class XemRun
{
public:

  XemRun
  (std::string file = "xemrun");             // the intention is to call with __FILE__

  ~XemRun();

  bool
  setBinary                                            // also tests file status
  (std::string binaryName   = ::xemrunBinaryName,
   std::string localDirName = ::xemrunLocalDirName,    // omit trailing digit
   std::string baseDirName  = ::xemrunBaseDirName);

  std::string
  setOptions
  (const std::string options);

  std::string                                // generated XEM file name
  write                                      // write 'model' to generated filename
  (const std::string& model);

  std::string                                // generated call string
  run                                        // call 'xeona'
  (std::string options = "");                // note default

  void
  dump                                       // dump the XEM model
  (std::ostream& os         = std::cout,     // pass in output stream
   const int     blankLines = 0);

private:

  void
  makeXemPath();

private:

  std::string                d_thisFile;     // from local __FILE__
  boost::filesystem::path    d_binary;       // xeona binary path
  boost::filesystem::path    d_xem;          // generated XEM file path
  std::string                d_options;      // nominated options
  std::string                d_call;         // generated 'xeona' call string

  static logga::spLogger     s_logger;       // shared_ptr to single logger object

};

#endif // _XEMRUN_H_

//  end of file

