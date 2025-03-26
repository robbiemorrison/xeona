//  XEONA ENERGY SYSTEMS MODELING ENVIRONMENT
//
//  file-create-name : appinfo.h
//  file-create-date : Mon 16-Apr-2007 12:07 UTC
//  file-initiator   : Robbie Morrison <robbie@actrix.co.nz>
//
//  file-role        : start and end-of-run reporting / header
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
//  $URL: file:///home/robbie/svn-root/xeona/futz/trunk/xeona1/a/appinfo.h $

//  HEADER GUARD

#ifndef _APPINFO_H_
#define _APPINFO_H_

//  LOCAL AND SYSTEM INCLUDES

#include "../a/logger_fwd.h"  // some forward declarations from "logger.h"
#include "../c/simcall.h"     // main simulation loop

#include <string>

#include <boost/filesystem.hpp>         // path objects, iterators, useful operations
#include <boost/date_time/posix_time/ptime.hpp>   // limited header

//  CODE

// ---------------------------------------------------------
//  CLASS           : AppInfo
// ---------------------------------------------------------

class AppInfo
{
  // DISABLED

private:

  AppInfo();                                 // prevent usage

  // CREATORS

public:

  AppInfo
  (int                           argc,       // from main()
   char**                        argv,       // from main()
   const boost::filesystem::path initial);   // from main()

  ~AppInfo();

  // ACCESSORS

  const std::string
  getProgramName() const;

  const std::string
  getBuildDate() const;

  const std::string
  getBuildTime() const;

  const std::string
  getCodebaseStatus() const;

  const std::string
  getTitle() const;

  const std::string
  getCopyright() const;

  const unsigned
  getSvnRevision() const;

  const std::string
  getGccVersion() const;

  const std::string
  getGlpkVersion() const;

  const std::string
  getBoostVersion() const;

  const std::string
  getOsInfo() const;

  const std::string      // say C, POSIX, en_US.iso885915, de_DE.utf8v, see $ locale -a
  getLocale() const;

  const bool
  getReleaseStatus() const;

  const std::string
  showSplash() const;

  const std::string
  showInfo
  (const xeona::SimKind simKind,
   const unsigned       beepMode,
   const unsigned       exitTrip) const;

  const std::string
  showHelp
  (std::string desc) const;

  const std::string
  showLegalMessage() const;

  const std::string
  showXemRules() const;

  const std::string
  showParse() const;

  const std::string
  showFinal
  (xeona::SimRet simRet,
   const int     exitStatus) const;

  const std::string
  getLogname() const;

  const std::string
  getCurrentDate() const;

  const std::string
  getCurrentTime() const;

  const std::string
  getReportLevel() const;

  const std::string                          // will be empty if not set
  getCommandLineModelName() const;

  // MANIPULATORS

  void
  setCommandLineReportLevel
  (const unsigned reportLevel);

  void
  setCommandLineModelName
  (const std::string& modelName);

  // UTILITY FUNCTIONS

private:

  const boost::filesystem::path
  pathFromString
  (const std::string& path) const;           // takes char* too without problem

  const std::string
  extractLeaf                                // the leaf is the final part of the path
  (const boost::filesystem::path path) const;

  const std::string
  extractPath
  (const boost::filesystem::path path) const;

  bool                                       // fail also sets field to ""
  splitString                                // split string and give nominated field
  (const std::string& record,                // input space-separated string
   std::string&       field,                 // selected output field
   const unsigned int index) const;          // field index, zero-based

  std::string
  zeroPadMe
  (const std::string& number,
   const int          padlevel) const;

  std::string
  argProcess
  (int    argc,                              // from main()
   char** argv) const;                       // from main()

  // PRIVATE DATA

private:

  boost::posix_time::ptime         d_tic;         // start time_point
  const boost::filesystem::path    d_progPath;    // complete (absolute) program path
  const boost::filesystem::path    d_startPath;   // complete (absolute) start directory
  std::string                      d_fullCmdLine; // "as typed" less extraneous whitespace
  unsigned                         d_clReportLevel;
  std::string                      d_clModelName;

  // STATIC DATA

private:

  static logga::spLogger s_logger;           // shared_ptr to single logger object

};

#endif // _APPINFO_H_

//  end of file

