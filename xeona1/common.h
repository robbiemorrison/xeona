//  XEONA ENERGY SYSTEMS MODELING ENVIRONMENT
//
//  file-create-name : common.h
//  file-create-date : Thu 05-Apr-2007 11:34 UTC
//  file-initiator   : Robbie Morrison <robbie@actrix.co.nz>
//
//  file-role        : useful definitions for the entire codebase / header
//  file-status      : ongoing
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
//  $URL: file:///home/robbie/svn-root/xeona/futz/trunk/xeona1/common.h $
//
//  GENERAL NOTES FOR THIS FILE
//
//  This file declares common items that may be needed throughout
//  the application.  These various items are duly defined in
//  common.cc.
//
//  The physical design adopted removes the need for complete
//  recompilation when a variable definition is changed --
//  instead only common.o needs to be rebuilt and relinked.
//
//  This file should be hash-included in each implementation file
//  unless there is a compelling reason not to do so.  Remember
//  also to namespace-qualify these variables, for example:
//  xeona::exit_success.

//  HEADER GUARD

#ifndef _COMMON_H_
#define _COMMON_H_

//  LOCAL AND SYSTEM INCLUDES

#include <string>             // C++ strings
#include <unistd.h>           // POSIX sleep(), usleep(), access(), chown()

// ---------------------------------------------------------
//  NAMESPACE       : xeona
// ---------------------------------------------------------

namespace xeona
{
  //  RUN-TIME FLAGS

  extern       bool        again;       // set via '--again' to continue after bad solve
  extern       bool        nopro;       // set via '--krazy' to exclude defensive coding
  extern       bool        pepper;      // set via '--pepper' for random reseeding
  extern       std::string tout;        // set via '--tout' for runtime visualization
  extern       bool        zero;        // set via '--zero' for disabling close-to-zero
  extern       unsigned    yeek;        // set via '--yeek' for selecting test code

  //  DEFINITIONS: compile-time debug flag

  extern const bool DBUG;               // set via xeona _XDEBUG macro
  extern const bool NBUG;               // set via official NDEBUG macro

  //  DEFINITIONS: current process id

  extern       pid_t pid;               // underlying 'unsigned', refer <unistd.h>

  //  DEFINITIONS: application exit statuses

  extern const int exit_success;        // for unit testers only
  extern const int exit_fail;           // for unit testers only

  extern const int ret_not_overwritten;

  extern const int ret_success;
  extern const int ret_message;
  extern const int ret_noclass;
  extern const int ret_usage;
  extern const int ret_fail;

  extern const int exit_kill_on_log;
  extern const int exit_empty_wrap;
  extern const int exit_non_registration;
  extern const int exit_short_timeseries;
  extern const int exit_file_not_found;
  extern const int exit_xem_data_issue;
  extern const int exit_lazy_link_fail;
  extern const int exit_cannot_run_guard;
  extern const int exit_bad_authorship;
  extern const int exit_full_link_fail;
  extern const int exit_empty_field_on_write;
  extern const int exit_timeseries_not_found;
  extern const int exit_yeek_abandon;
  extern const int exit_bad_submodel;
  extern const int exit_hour_resolution_only;
  extern const int exit_invalid_interval;
  extern const int exit_bad_subentity_label;
  extern const int exit_no_gateway_demander;
  extern const int exit_no_gateway_controller;
  extern const int exit_hop_limit_reached;

  extern const int exit_bidset_selections;
  extern const int exit_entity_fail;

  extern const int exit_std_out_of_range;
  extern const int exit_std_domain_error;
  extern const int exit_std_logic_error;
  extern const int exit_std_bad_alloc;
  extern const int exit_std_exception;
  extern const int exit_bad_assign_cast;
  extern const int exit_boost_exception;
  extern const int exit_unknown_exception;

  extern const int ret_model_file_fault;
  extern const int ret_infeasibility;
  extern const int ret_errant_simulation;
  extern const int ret_test_code_used;
  extern const int ret_other;
  extern const int ret_status_not_known;

  extern const int exit_test99;         // special  99 return for test code
  extern const int exit_test100;        // special 100 return for test code

  extern const int exitTripLevelDefault;

  //  DEFINITIONS: build information

  extern const std::string buildCopyright;
  extern const std::string buildTitle;
  extern const std::string codebaseStatus;

  extern const int svnRev;              // build script macro _SVNVER or defaults to 0

  extern const int gccVerMajor;         // derived from compiler macro __GNUC__
  extern const int gccVerMinor;         // derived from compiler macro __GNUC_MINOR__
  extern const int gccPatchLevel;       // derived from compiler macro __GNUC_PATCHLEVEL__

  extern const int glpkVerMajor;        // dervived from <glpk.h> macros
  extern const int glpkVerMinor;

  extern const int boostVersionNum;     // derived from <boost/version.hpp> macros
  extern const std::string boostVersionStr;

  extern const std::string osName;      // derived from compiler macros like __linux__

  extern const bool releaseStatus;      // true if _XRELEASE is defined

  extern const std::string buildDate;   // derived from compiler macro __DATE__
  extern const std::string buildTime;   // derived from compiler macro __TIME__

  //  DEFINITIONS: run information

  extern       std::string startDate;   // set by 'AppInfo' object at startup
  extern       std::string startTime;   // set by 'AppInfo' object at startup

  //  DEFINITIONS: system-specific literals

  extern const char osSlash;            // pathname separator, a '/' on Linux

  //  DEFINITIONS: miscellaneous constants

  extern const std::string iso4217;          // ISO 4217 three character currency code
  extern const std::string logfileDefault;   // hard-coded logfile name instead of console

  //  DEFINITIONS: mathematical and physical constants

  extern const double mathsPi;               // maths constant 'pi'
  extern const double mathsE;                // maths constant 'e'

  extern const double stdAtmosphere;         // physics constant 'atm' (~101kPa)
  extern const double stdGravity;            // physics constant 'g'   (~9.81m/s2)
  extern const double absoluteZero;          // absolute zero          (-273.15 degrees C)

  extern const int secondsPerYear;           // seconds in 365 day year

  //  DEFINITIONS: model files

  extern const std::string backupTag;             // append tag for file backup
  extern const std::string modelExt;              // xeona model extension
  extern const std::string modelStubDefault;      // default model name
  extern const std::string modelInbuiltDefault;   // default inbuilt model name
  extern const std::string modelGuardTag;         // tag indicating guard file

  extern const std::string modelDisableChar;      // record or field disable string
  extern const std::string modelTsRepeater;       // timeseries repeat indicator
  extern const std::string modelStringDelim;      // pairwise delimiter for string values
  extern const std::string modelBidDelim;         // nodal bid separator
  extern const std::string modelBidSetSep;        // bidset separator
  extern const std::string modelEndMarker;        // model end marker

  extern const unsigned    modelFieldIndent;      // field line indent
  extern const unsigned    modelAngleIndent;      // minimum field angle char allowance

  //  DEFINITIONS: optimization problem (OSP) labeling

  extern const std::string ospTagSep;        // separator string used by class 'Label'

  //  DEFINITIONS: mandatory entity identifiers

  extern const std::string timehorizon;      // 'TimeHorizon' entity
  extern const std::string overseer;         // 'Overseer' entity

  //  DEFINITIONS: reporting precision for writing data and for logging

  extern const std::string writeFloatFmtStr; // 'boost::format' for writing floats
  extern const int         loggerRepxPrec;   // 'std::iostream' precision for floats

  //  DEFINITIONS: run-time behavior

  extern const unsigned reportLevelOpening;  // opening reporting level
  extern const unsigned reportLevelDefault;  // default reporting level
  extern const unsigned beepModeDefault;     // default beep behavior
  extern const unsigned sameLogLimit;        // limit repetitive log messages
  extern const unsigned consoleWidth;        // macro set via CPPFLAGS

  //  DEFINITIONS: hard-coded model data, flow control trips, solver parameters

  extern const int    mode6steps;            // used by "--mode 6"
  extern const int    mode8hours;            // used by "--mode 8"
  extern const int    mode9seconds;          // used by "--mode 9"
  extern const int    ctaHopLimit;           // CTA hop limit per interval
  extern const double numericalZero;         // for use by solver
  extern const double coeffSpanTol;          // reporting alert threshold

  //  DEFINITIONS: external programs

  extern const std::string webbrowser;       // invocation string with options
  extern const std::string gnuplot;          // invocation string with options

  //  DEFINITIONS: xedocs text file

  extern const std::string xedocsFileName;        // xedocs file name
  extern const std::string xedocsFileContents;    // xedocs file contents

} // namespace xeona

//  CODE-WIDE PREPROCESSOR MACROS

// used to switch between standard and compiler-specific macros
// (noting that these are not actually macros these days)

// compiler-specific code to obtain the name of this function
#ifdef __GNUG__                              // a GNU g++ compiler
# define XEONA_FUNC __PRETTY_FUNCTION__
#else
# define XEONA_FUNC  __func__
#endif

//  TEMPORARY CODE-WIDE PREPROCESSOR MACROS

// #define XE_BUGHUNT_SOME_ISSUE 1           // not currently in use

#endif // _COMMON_H_

// end of file

