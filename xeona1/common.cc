//  XEONA ENERGY SYSTEMS MODELING ENVIRONMENT
//
//  file-create-name : common.cc
//  file-create-date : Sun 08-Apr-2007 05:43 UTC
//  file-initiator   : Robbie Morrison <robbie@actrix.co.nz>
//
//  file-role        : provide useful definitions to the entire codebase / implementation
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
//  $URL: file:///home/robbie/svn-root/xeona/futz/trunk/xeona1/common.cc $
//
//  GENERAL NOTES FOR THIS FILE
//
//  This file defines common items that may be needed throughout
//  the application.
//
//  Assuming the normal makefile is used, this file is always
//  "touched" in order to ensure it is remade and therefore
//  fully current.
//
//  The compiler macros '__linux__' and '__unix__' are used
//  exclusively in this codebase.  A more robust approach would
//  be to utilize instead:
//
//  // #ifdef __linux__
// #if defined(linux) || defined(__linux) || defined(__linux__)

//  LOCAL AND SYSTEM INCLUDES

#include "common.h"           // companion header for this file (place first)

#include "a/logger.h"         // run-time logging functionality (as required)

#include <string>             // C++ strings

// specific sources of information

#include <boost/math/constants/constants.hpp>     // high-precision mathematical constants
#include <boost/version.hpp>                      // Boost release information
#include <cstdlib>                                // EXIT_SUCCESS macro
#include <glpk.h>                                 // GNU GLPK version information

//  NAMESPACE : xeona

namespace xeona
{
  //  RUN-TIME FLAGS

#ifdef _XUTEST
  bool nopro = true;                    // unit tests require 'true'
#else
  bool nopro = false;                   // can be reset via '--krazy' to 'true'
#endif

  bool        again  = false;           // set via '--again' to continue after bad solve
  bool        pepper = false;           // set via '--pepper' for random reseeding
#if 0 // 0 = no default, 1 = 'dumb' terminal default
  std::string tout   = "dumb";          // set via '--tout' for runtime visualization
#else
  std::string tout   = "";              // set via '--tout' for runtime visualization
#endif // 0

  bool        zero   = true;            // set via '--zero' for disabling close-to-zero
  unsigned    yeek   = 0;               // set via '--yeek' for selecting test code

  //  DEFINITIONS: debug flag
  //  controlled by compile-time _XDEBUG macro

#ifdef _XDEBUG                               // xeona macro
  const bool DBUG = true;
#else
  const bool DBUG = false;
#endif

  //  controlled by compile-time NDEBUG macro

#ifdef NDEBUG                                // official macro
  const bool NBUG = true;
#else
  const bool NBUG = false;
#endif

  //  DEFINITIONS: process id

  pid_t pid = 0;                             // underlying 'unsigned', refer <unistd.h>

  //  DEFINITIONS : application exit statuses
  //  consider existing user expectations when modifying

  const int exit_success              =   0; // for unit testers only
  const int exit_fail                 =   1; // for unit testers only

  const int ret_not_overwritten       = 255; // highest possible return integer

  const int ret_success               =   0; // general success
  const int ret_message               =   0; // display and quit returns
  const int ret_noclass               =   8; // class not found
  const int ret_usage                 =   9; // command-line usage error
  const int ret_fail                  = 255; // serious failure

  const int exit_kill_on_log          =  10; // see 'a/exitstat.cc' for descriptions
  const int exit_empty_wrap           =  11;
  const int exit_non_registration     =  12;
  const int exit_short_timeseries     =  13;
  const int exit_file_not_found       =  14;
  const int exit_xem_data_issue       =  15;
  const int exit_lazy_link_fail       =  16;
  const int exit_cannot_run_guard     =  17;
  const int exit_bad_authorship       =  18;
  const int exit_full_link_fail       =  19;
  const int exit_empty_field_on_write =  20;
  const int exit_timeseries_not_found =  21;
  const int exit_yeek_abandon         =  22;
  const int exit_bad_submodel         =  23;
  const int exit_hour_resolution_only =  24;
  const int exit_invalid_interval     =  25;
  const int exit_bad_subentity_label  =  26;
  const int exit_no_gateway_demander  =  27;
  const int exit_no_gateway_controller=  28;
  const int exit_hop_limit_reached    =  29;

  const int exit_bidset_selections    =  30;
  const int exit_entity_fail          =  31;

  const int exit_bad_assign_cast      =  40;
  const int exit_boost_exception      =  41;
  const int exit_std_out_of_range     =  42;
  const int exit_std_domain_error     =  43;
  const int exit_std_logic_error      =  44;
  const int exit_std_bad_alloc        =  45;
  const int exit_std_exception        =  46;
  const int exit_unknown_exception    =  47;

  const int ret_model_file_fault      =  50;
  const int ret_infeasibility         =  51;
  const int ret_errant_simulation     =  52;
  const int ret_test_code_used        =  53;
  const int ret_other                 =  58;
  const int ret_status_not_known      =  60;

  const int exit_test99               =  99;
  const int exit_test100              = 100; // special 100 return for test code

  const int exitTripLevelDefault   = logga::kill;      // success means no KILL logging

  //  DEFINITIONS: hand-set build information
  //  update as appropriate

  const std::string buildCopyright = "2007 - 2012 Robbie Morrison";
  const std::string buildTitle     = "xeona energy systems modeling environment";
  const std::string codebaseStatus = "first-pass complete and in testing phase";

  //  DEFINITIONS : auto-set build information
  //  controlled by compile-time macros

#ifdef _XSVNREV
  const int svnRev = _XSVNREV;
#else
  const int svnRev = 0;
#endif

  const int gccVerMajor     = __GNUC__;
  const int gccVerMinor     = __GNUC_MINOR__;
  const int gccPatchLevel   = __GNUC_PATCHLEVEL__;

  const int glpkVerMajor    = GLP_MAJOR_VERSION;
  const int glpkVerMinor    = GLP_MINOR_VERSION;

  const int boostVersionNum         = BOOST_VERSION;        // 103401
  const std::string boostVersionStr = BOOST_LIB_VERSION;    // "1_34_1"

#if   defined (__linux__)
  const std::string osName  = "Linux";       // Linux
#elif defined (__unix__)
  const std::string osName  = "UNIX";        // generic UNIX
# else
  const std::string osName  = "(not Linux or UNIX)";
#endif

#ifdef _XRELEASE                             // xeona macro
  const bool releaseStatus = true;
#else
  const bool releaseStatus = false;
#endif

  //  DEFINITIONS: compiler macros
  //  compiler macro __DATE__ yields Mar  8 2007
  //  compiler macro __TIME__ yields 23:00:59

  // CAUTION: omit double quotes on __DATE__ and __TIME__
  const std::string buildDate = __DATE__;
  const std::string buildTime = __TIME__;

  //  DEFINITIONS: run information

  std::string startDate = "(not yet set)";     // set by 'AppInfo' object at startup
  std::string startTime = "(not yet set)";     // set by 'AppInfo' object at startup

  //  DEFINITIONS : system-specific literals

  // CAUTION: this code has NOT BEEN TESTED on either 32-bit or 64-bit Windows
#if defined (WIN32) || defined (_WIN32) || defined (__WIN32__) || defined (_WIN64)
  const char osSlash = '\\';                 // CAUTION: escaping is needed
#else
  const char osSlash = '/';
#endif

  //  DEFINITIONS: miscellaneous constants

  //  GENERAL : define the units used in reporting -- these are
  //  SHALLOW settings which affect the printed output but which
  //  have NO INFLUENCE on the underlying calculations

  const std::string iso4217 = "EUR";    // ISO 4217 three char code (for example, EUR USD)
  const std::string logfileDefault = "hardcoded.log";  // hard-coded logfile name

  //  DEFINITIONS: mathematical and physical constants

  // pi = 3.1415926535897932384...
  // e  = 2.71828182845904523536...

  const double mathsPi          = boost::math::constants::pi<double>();
  const double mathsE           = boost::math::constants::e<double>();

  //  source: Wikipedia, July 2008

  const double stdAtmosphere    = +101325;        // [Pa]
  const double stdGravity       = +9.80665;       // [m/s2]
  const double absoluteZero     = -273.15;        // [degrees C]

  const int secondsPerYear      = 8760 * 3600;    // seconds in 365 day year

  //  DEFINITIONS: model files

  const std::string backupTag           = "~";         // append tag for file backup
  const std::string modelExt            = ".xem";      // xeona model extension
  const std::string modelStubDefault    = "test";      // default model name
  const std::string modelInbuiltDefault = "inbuilt";   // default inbuilt model name
  const std::string modelGuardTag       = ".guard";    // tag indicating guard file

  const std::string modelDisableChar = "#";  // record or field disable string
  const std::string modelTsRepeater  = ".."; // timeseries repeat indicator
  const std::string modelStringDelim = "\""; // pairwise delimiter for string values [1]
  const std::string modelBidDelim    = "*";  // nodal bid separator
  const std::string modelBidSetSep   = "/";  // bidset separator, single char! [2]
  const std::string modelEndMarker   = "model-end";    // model end marker

  const unsigned    modelFieldIndent =  4;   // field line indent
  const unsigned    modelAngleIndent = 45;   // minimum field angle char allowance

  // [1] successfully tested with "\"" and "'" whilst noting that "" is illegal
  // [2] used for splitting (set of chars), concatenation (short string), number casting

  //  DEFINITIONS: optimization problem (OSP) labeling

  const std::string ospTagSep = ".";         // separator string used by class 'Label' [1]

  // [1] a dot '.' will conflict with float representations while a "::" uses more room

  //  DEFINITIONS: mandatory entity identifiers

  const std::string timehorizon = "time-horizon";      // 'TimeHorizon' entity
  const std::string overseer    = "overseer";          // 'Overseer' entity

  //  DEFINITIONS: reporting precision for writing data and for logging

#if 0 // 0 = 4 sigfigs and 3 decplaces, 1 = 3 sigfigs and 2 decplaces
  const std::string writeFloatFmtStr = "%+.2e"; // 'boost::format' for writing floats
  const int         loggerRepxPrec   = 2;       // 'std::iostream' precision for floats
#else
  const std::string writeFloatFmtStr = "%+.3e"; // 'boost::format' for writing floats
  const int         loggerRepxPrec   = 3;       // 'std::iostream' precision for floats
#endif

  //  DEFINITIONS: run-time behavior

#ifdef _XDEBUG                               // xeona macro
  const unsigned reportLevelOpening  = 4;    // opening reporting before cmd-line parsing
  const unsigned reportLevelDefault  = 6;    // default reporting 0-6 (quiet - verbose)
  const unsigned beepModeDefault     = 0;    // default beep behavior 0-2 (silent - loud)
#else
  const unsigned reportLevelOpening  = 2;
  const unsigned reportLevelDefault  = 2;
  const unsigned beepModeDefault     = 2;
#endif
  const unsigned sameLogLimit        = 3;    // limit repetitive log messages
  const unsigned consoleWidth   = _XTCOLS;   // macro set via CPPFLAGS [1]

  // [1] 'sojus.fb10.tu-berlin.de', with a scroll bar, supports 143 chars
  // to check $ stty --all # take columns value OR $ stty size # take second value
  // to test: $ for i in $(seq 200); do printf "%0${i}d\n" $i; done

  //  DEFINITIONS: hard-coded model data, flow control trips, solver parameters

#if 1 // 0 = test status using sub-year, 1 = normal year
  const int    mode6steps         =    1;    // used by "--mode 6" (not used)
  const int    mode8hours         = 8760;    // used by "--mode 8"
  const int    mode9seconds       = 1800;    // used by "--mode 9"
#else
#  ifdef __GNUG__                            // a GNU g++ compiler
#    warning "'xeona::mode8hours' set to one week (normally 8760)"
#   endif
  const int    mode6steps         =    1;    // used by "--mode 6" (not used)
  const int    mode8hours         =  168;    // used by "--mode 8"
  const int    mode9seconds       = 1800;    // used by "--mode 9"
#endif // 0
  const int    ctaHopLimit        = 100;     // CTA hop limit per step (zero to disable)
  const double numericalZero      = 1.0e-10; // for use by solver
  const double coeffSpanTol       = 1.0e+08; // reporting alert threshold

  //  DEFINITIONS: external programs

  const std::string webbrowser = "firefox";  // invocation string with options [1]
  const std::string gnuplot    = "gnuplot";  // invocation string with options [2]

  // [1] used by 'GlpkViz' class to display GLPK problem instances
  // [2] used by 'TsGnuplot' class to display timeseries data

  //  DEFINITIONS: xedocs text file

#ifndef _XUTEST
# define XE_XEDOCS_FILE "a/xedocs.txt"
#else
# define XE_XEDOCS_FILE "a/xedocs.ut.txt"
#endif

  const std::string xedocsFileName = XE_XEDOCS_FILE;
  const std::string xedocsFileContents =
#include XE_XEDOCS_FILE                      // CAUTION: file must exist at compile time
  ;                                          // CAUTION: final semicolon is necessary

#undef XE_XEDOCS_FILE                        // undefine macro for defensive programming

} // namespace xeona

//  end of file

