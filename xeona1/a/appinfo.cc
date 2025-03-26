//  XEONA ENERGY SYSTEMS MODELING ENVIRONMENT
//
//  file-create-name : appinfo.cc
//  file-create-date : Mon 16-Apr-2007 12:07 UTC
//  file-initiator   : Robbie Morrison <robbie@actrix.co.nz>
//
//  file-role        : start and end-of-run reporting / implementation
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
//  $URL: file:///home/robbie/svn-root/xeona/futz/trunk/xeona1/a/appinfo.cc $

//  LOCAL AND SYSTEM INCLUDES

#include "appinfo.h"          // companion header for this file (place first)

#include "../c/util1.h"       // free functions which offer general utilities 1
#include "../a/yeek.h"        // yeek (for running extra code) value interpretation
#include "../a/exitstat.h"    // exit status database

#include "../a/logger.h"      // standard logging functionality (as required)
#include ".././common.h"      // common definitions for project (place last)

#include <locale>             // locale specific information
#include <sstream>            // string-streams
#include <string>             // C++ strings
#include <vector>             // STL sequence container

#include <cstdlib>            // C-style exit(), getenv(), system(), NULL, EXIT_SUCCESS
#include <unistd.h>           // POSIX sleep(), usleep(), access(), chown()

#include <boost/algorithm/string.hpp>        // string recasing, trimming, splitting
#include <boost/format.hpp>                  // printf style formatting
#include <boost/lexical_cast.hpp>            // lexical_cast<>()
#include <boost/tokenizer.hpp>               // string tokenizer
#include <boost/foreach.hpp>                 // BOOST_FOREACH iteration macro

// CAUTION: 'boost/date_time/posix_time/posix_time' DOES require
// the -lboost_date_time linking directive in the makefile
// (although some uses of this header may not)

#include <boost/date_time/posix_time/posix_time.hpp>   // plus io

//  CODE

// ---------------------------------------------------------
//  STRING CONSTANT : ::licenseText
// ---------------------------------------------------------
//  See Becker (2007 p350) for a similar example.
// ---------------------------------------------------------

namespace
{
  const std::string licenseText =
#include "license.h"               // contains double-quoted newline-terminated text
  ;                                // final semicolon
}

// ---------------------------------------------------------
//  CLASS           : AppInfo
// ---------------------------------------------------------
//  Purpose      : supply build and run information
//  Status       : complete
//
//  Design notes
//
//      Volatile information is held in 'common.cc'.  And
//      'common.cc' is either touched or modified (for a release
//      build) during make and is thus always current.
//
//      Some formatting routines could be better refactored using
//      Boost library calls.
//
//      Utility function beginning 'show' generally contain
//      newlines, whereas utility functions beginning 'get'
//      generally do not.
//
//      The terminology relating to files derives from
//      Boost.Filesystem.  Read that documentation for more
//      information.
//
// ---------------------------------------------------------

// CREATORS

AppInfo::AppInfo
(int                           argc,         // from main()
 char**                        argv,         // from main()
 const boost::filesystem::path initial) :    // from main()
   d_tic(boost::posix_time::microsec_clock::universal_time()),
   d_progPath(pathFromString(argv[0])),      // utility function
   d_startPath(initial),
   d_fullCmdLine(argProcess(argc, argv)),    // utility function
   d_clReportLevel(0),                       // set later by Boost.Program_options
   d_clModelName()                           // set later by Boost.Program_options
{
  // initial reporting
  s_logger->repx(logga::dbug, "constructor call, argv[0]", argv[0]);
  s_logger->repx(logga::dbug, "d_progPath as leaf", extractLeaf(d_progPath));

  // recover process id
  xeona::pid = getpid();                     // refer <unistd.h>
  s_logger->repx(logga::dbug, "getpid call", xeona::pid);

  // path (including leaf) reporting
  std::ostringstream put;
  put << "  full path (stream) : " << d_progPath              << "\n"
      << "  full path (native) : " << extractPath(d_progPath) << "\n";
  s_logger->putx(logga::dbug, put);
}

AppInfo::~AppInfo()
{
  s_logger->repx(logga::dbug, "destructor call", "");
}

// ACCESSORS

const std::string
AppInfo::getProgramName() const
{
  return extractLeaf(d_progPath);
}

const std::string
AppInfo::getBuildDate() const
{
  // CAUTION: this function was originally written using
  // 'boost::posix_time' and 'boost::gregorian' objects -- but
  // there were memory problems, as detected by valgrind, when
  // using the relevant streaming operators.

  std::string date = xeona::buildDate;  // from compiler macro __DATE__ set in common.cc

  // tokenize and create new date string

  // NOTE: the 'boost::tokenizer' class has a fairly limited
  // interface and, in particular, no subscript or [] operator --
  // hence the need to create an iterator to gain access to the
  // newly found tokens

  boost::tokenizer<> tok(date);                             // defaults used
  boost::tokenizer<>::iterator grab = tok.begin();
  std::string smonth =   *grab;                             // Mar
  std::string sday   = *++grab;                             // 8
  std::string syear  = *++grab;                             // 2007

  std::string zmonth;
  if ( smonth == "Jan" ) zmonth = "01";
  if ( smonth == "Feb" ) zmonth = "02";
  if ( smonth == "Mar" ) zmonth = "03";
  if ( smonth == "Apr" ) zmonth = "04";
  if ( smonth == "May" ) zmonth = "05";
  if ( smonth == "Jun" ) zmonth = "06";
  if ( smonth == "Jul" ) zmonth = "07";
  if ( smonth == "Aug" ) zmonth = "08";
  if ( smonth == "Sep" ) zmonth = "09";
  if ( smonth == "Oct" ) zmonth = "10";
  if ( smonth == "Nov" ) zmonth = "11";
  if ( smonth == "Dec" ) zmonth = "12";

  std::string zday   = zeroPadMe(sday, 2);
  std::string output = syear + "-" + zmonth + "-" + zday;
  return output;
}

const std::string
AppInfo::getBuildTime() const
{
  return xeona::buildTime;         // from compiler macro __TIME__ set in common.cc
}

const std::string
AppInfo::getCodebaseStatus() const
{
  return xeona::codebaseStatus;              // defined in common.cc
}

const std::string
AppInfo::getTitle() const
{
  return xeona::buildTitle;                  // defined in common.cc
}

const std::string
AppInfo::getCopyright() const
{
  return xeona::buildCopyright;              // defined in common.cc
}

const unsigned
AppInfo::getSvnRevision() const
{
  return xeona::svnRev;                      // from 'common.cc'
}

const std::string
AppInfo::getGccVersion() const
{
  const std::string gcc = boost::str(boost::format("%d.%d.%d")
                                      % xeona::gccVerMajor     // say  4
                                      % xeona::gccVerMinor     // say  1
                                      % xeona::gccPatchLevel); // say  2
  return gcc;
}

const std::string
AppInfo::getGlpkVersion() const
{
  // CAUTION: the following string should really be set
  // intelligently, probably via a preprocessor macro computed in
  // the makefile

#if 1 // 0 = run-time, 1 = static
  const std::string link = "static";         // statically linked
#else
  const std::string link = "run-time";       // dynamically linked
#endif // 0

  const std::string glpk = boost::str(boost::format("%d.%d %s")
                                      % xeona::glpkVerMajor    // say  4
                                      % xeona::glpkVerMinor    // say 19
                                      % link);
  return glpk;
}

const std::string
AppInfo::getBoostVersion() const
{
  // documentation from <boost/version.hpp> header
  //
  //  BOOST_VERSION % 100 is the sub-minor version
  //  BOOST_VERSION / 100 % 1000 is the minor version
  //  BOOST_VERSION / 100000 is the major version
  //
  // thus 103401 -> "1.34.1"

  const int boost_version = xeona::boostVersionNum;
  const int major    = boost_version / 100000;
  const int minor    = boost_version / 100 % 1000;
  const int subminor = boost_version % 100;

  // determine the linking strategy in relation to the
  // preprocessor macro '_XRELEASE'

  // CAUTION: the logic here must align with that used in the
  // 'mach' build script and/or the makefile

  const std::string linking = xeona::releaseStatus ? "static" : "dynamic";

  // build
  std::ostringstream oss;
  oss << major << "." << minor << "." << subminor << " " << linking;

  return oss.str();
}

const std::string
AppInfo::getOsInfo() const
{
  const std::string os = xeona::osName;
  return os;
}

// for information on the class 'std::locale', see Lischner (2003
// pp578-581) and, more generally, Josuttis (1999 pp692-726)

const std::string
AppInfo::getLocale() const
{
  // grab native locale
  std::locale local = std::locale();         // native locale, refer <locale>
                                             // as used by Boost.String_algo by default
  // check against "C" locale
  if ( local == std::locale::classic() )     // 'classic' is the "C" locale
    {
      return local.name();                   // type 'basic_string<char>'
    }
  else
    {
      return local.name() + " (\"C\" locale recommend)";
    }
}

const bool                                   // 'true' when built using 'mach -r' script
AppInfo::getReleaseStatus() const
{
  return xeona::releaseStatus;               // set via _XRELEASE preprocessor macro
}

const std::string
AppInfo::showSplash() const
{
  std::ostringstream ss;
  ss   << ""                                                    << "\n"
       << "  xeona"                                             << "\n"
       << ""                                                    << "\n"
       << "     " << getTitle()                                 << "\n"
       << "     " << "copyright (c) " << getCopyright()         << "\n";
  if ( getReleaseStatus() )
    ss << "     " << "show GPL v3 license with option --legal"  << "\n";
  ss   << "     " << "all timestamps in UTC time"               << "\n";

  s_logger->addSmartBlank();                 // next logger call should add a blank line
  s_logger->addFinalStdoutBlank();           // add a final blank to stdout

  return ss.str();
}

const std::string
AppInfo::showInfo
(const xeona::SimKind simKind,
 const unsigned       beepMode,
 const unsigned       exitTrip) const
{
  std::string buildStatus               = "non-release";
  if ( getReleaseStatus() ) buildStatus = "release";

  std::string debugInfo                 = "unset";
  if ( xeona::DBUG ) debugInfo          = "set";

  std::string ndebugInfo                = "unset";
  if ( xeona::NBUG ) ndebugInfo         = "set";

  std::string modelName;
  if ( d_clModelName.empty() )
    modelName = xeona::modelStubDefault + xeona::modelExt + " (defaulting)";
  else
    modelName = d_clModelName + "";

  std::string svnRevision;
  const unsigned rev = getSvnRevision();
  if ( rev == 0 ) svnRevision = "(not-in-sync)";
  else            svnRevision = boost::lexical_cast<std::string>(rev);

  std::string simulationMode = "(not overwritten)";
  std::ostringstream ossSimMode;
  switch ( simKind )
    {
    case xeona::e_notSpecified:
      ossSimMode << "(not specified)";
      break;
    case xeona::e_hollowCall:
      ossSimMode << "return after processing command-line";
      break;
    case xeona::e_identifyFile:
      ossSimMode << "identify file and quit";
      break;
    case xeona::e_parseModel:
      ossSimMode << "parse and shallow overwrite model";
      break;
    case xeona::e_invokeFactory:
      ossSimMode << "construct entities and deep overwrite model";
      break;
    case xeona::e_linkAndConnect:
      ossSimMode << "link and connect entities";
      break;
    case xeona::e_firstStepRun:
      ossSimMode << "first-step run";
      break;
    case xeona::e_fullRun:
      ossSimMode << "full run";
      break;
    case xeona::e_yearRun:
      ossSimMode << "year run"
                 << " (" << xeona::mode8hours << "h)";
      break;
    case xeona::e_resampleRun:
      ossSimMode << "resample year run"
                 << " (" << xeona::mode9seconds << "s "<< xeona::mode8hours << "h)";
      break;
    default: std::clog << "** coding error 01 in source file " << __FILE__ << std::endl;
    }
  ossSimMode << " (" << simKind << ")";
  simulationMode = ossSimMode.str();

  std::string noproStatus         = "exit cleanly on fault";
  if ( xeona::nopro ) noproStatus = "run to completion or failure (and core dump)";

  std::string beeping = "(not overwritten)";
  std::ostringstream ossBeeping;
  switch ( beepMode )
    {
    case 0: ossBeeping << "silent";                break;
    case 1: ossBeeping << "on completion";         break;
    case 2: ossBeeping << "on displayed warnings"; break;
    default: std::clog << "** coding error 02 in source file " << __FILE__ << std::endl;
    }
  ossBeeping << " (" << beepMode << ")";
  beeping = ossBeeping.str();

  std::string triggerSource = s_logger->getTrigger();
  if ( triggerSource.empty() ) triggerSource = "(none)";

  std::string exitTripLevel = "(not overwritten)";
  std::ostringstream ossExitTripLevel;
  switch ( exitTrip )
    {
    case logga::yeek: ossExitTripLevel << "report trips not enabled";    break;
    case logga::kill: ossExitTripLevel << "on kill calls";               break;
    case logga::warn: ossExitTripLevel << "on warning calls upward";     break;
    case logga::info: ossExitTripLevel << "on information calls upward"; break;
    case logga::dbug: ossExitTripLevel << "on debug calls upward";       break;
    case logga::xtra: ossExitTripLevel << "on extra calls upward";       break;
    case logga::adhc: ossExitTripLevel << "on any report call";          break;
    default: std::clog << "** coding error 03 in source file " << __FILE__ << std::endl;
    }
  ossExitTripLevel << " (0 thru " << exitTrip << ")";
  exitTripLevel = ossExitTripLevel.str();

  std::string emptyData = "(not overwritten)";
  switch ( logga::rankNoData )
    {
    case logga::warn: emptyData = "warn if empty fields encountered"; break;
    case logga::dbug: emptyData = "(no special action)";              break;
    default: std::clog << "** coding error 04 in source file " << __FILE__ << std::endl;
    }

  // load global variables
  xeona::startDate = getCurrentDate();       // overwrite "(not yet set)"
  xeona::startTime = getCurrentTime();       // overwrite "(not yet set)"

  std::ostringstream ss;
  ss << ""                                                  << "\n"
     << "  build information"                               << "\n"
     << ""                                                  << "\n"
     << "     compile date      :  " << getBuildDate()      << "\n"
     << "     operating system  :  " << getOsInfo()         << "\n"
     << "     g++ compiler      :  " << getGccVersion()     << "\n"
     << "     glpk solver       :  " << getGlpkVersion()    << "\n"
     << "     boost c++ libs    :  " << getBoostVersion()   << "\n"
     << "     build context     :  " << buildStatus         << "\n"
     << "     codebase status   :  " << getCodebaseStatus() << "\n"
     << "     terminal width    :  " << xeona::consoleWidth << "\n"
     << "     _XDEBUG app       :  " << debugInfo           << "\n"
     << "     NDEBUG c++ libs   :  " << ndebugInfo          << "\n"
     << "     svn revision      :  " << svnRevision         << "\n"
     << ""                                                  << "\n"
     << "  run information"                                 << "\n"
     << ""                                                  << "\n"
     << "     binary name       :  " << getProgramName()    << "\n"
     << "     logname           :  " << getLogname()        << "\n"
     << "     locale            :  " << getLocale()         << "\n"
     << "     start date        :  " << xeona::startDate    << "\n"
     << "     start time (UTC)  :  " << xeona::startTime    << "\n"
     << "     process ID        :  " << xeona::pid          << "\n"
     << ""                                                  << "\n"
     << "  command-line settings"                           << "\n"
     << ""                                                  << "\n"
     << "     command-line      :  " << d_fullCmdLine       << "\n"
     << "     defensive coding  :  " << noproStatus         << "\n"
     << "     beep behavior     :  " << beeping             << "\n"
     << "     reporting level   :  " << getReportLevel()    << "\n"
     << "     no data           :  " << emptyData           << "\n"
     << "     watch regex       :  " << triggerSource       << "\n"
     << "     nonzero exit trip :  " << exitTripLevel       << "\n"
     << "     simulation mode   :  " << simulationMode      << "\n"
     << "     given model name  :  " << modelName           << "\n";

  s_logger->addSmartBlank();                 // next logger call should add blank line
  s_logger->addFinalStdoutBlank();           // add a final blank to stdout

  return ss.str();
}

const std::string
AppInfo::showHelp
(std::string strDesc) const
{
  // create string summary of exit statuses
  ExitStatus exitstatus;                     // accept default not-present message
  std::ostringstream buffer;
  for ( int i = 0; i <= 100; ++i )
    {
      std::string interpretation;
      if ( exitstatus(i, interpretation) )   // 'true' if present
        {
          buffer << boost::format("   %3d = %s") % i % interpretation << "\n";
        }
    }
  const std::string exitstatii = buffer.str();

  // principal string-stream
  std::ostringstream ss;
  ss
<< "\n"

<<"usage: "
<< getProgramName()
<< "  [options]  [model[.xem]]"
<< "\n"

<< "\n"

<< strDesc                              // generated by 'Boost.Program_options'

<< "\n"

<< "numeric arguments (defaults given above):\n"
<< "  beep\n"
<< "      0 = silent\n"
<< "      1 = on completion\n"
<< "      2 = plus on displayed warnings\n"
<< "  report\n"
<< "      0 = no output\n"
<< "      1 = show kills\n"
<< "      2 = plus warnings\n"
<< "      3 = plus information\n"
<< "      4 = plus debug\n"
<< "      5 = plus extra\n"
<< "      6 = plus adhoc\n"
<< "      7 = as 6 but do not truncate logging to fit terminal (ignore _XCOLS macro)\n"
<< "  mode\n"
<< "      1 = process command-line\n"
<< "      2 = plus identify model file\n"
<< "      3 = plus parse file with shallow (unprocessed string) overwrite\n"
<< "      4 = plus construct entities with deep (lexically cast and recast) overwrite\n"
<< "      5 = plus link (informational) and connect (physical) entities as required\n"
<< "      6 = plus undertake the first simulation step only\n"
<< "      7 = plus undertake the remaining simulation steps\n"
<< "      8 = mode 7 but horizon forced to "
<< xeona::mode8hours
<< " hours (requires sufficient data)\n"
<< "      9 = mode 8 but interval forced to "
<< xeona::mode9seconds
<< " seconds (requires resampling)\n"

<< "\n"

<< "default model names:\n"
<< "      normal  = "
<< xeona::modelStubDefault << xeona::modelExt
<< "\n"
<< "      inbuilt = "
<< xeona::modelInbuiltDefault << xeona::modelExt
<< "\n"
<< "      guard   = "
<< xeona::modelStubDefault << xeona::modelGuardTag << xeona::modelExt
<< "\n"

<< "\n"

<< "options generally:\n"
<< "    options are processed in order listed above\n"
<< "    console reporting increases user/kernel context switching and slows execution\n"

<< "\n"

<< "options specifically:\n"
<< "    option --class + displays all documented entity class names"
    " (+ is special character)\n"
<< "    option --class . displays all documented entity class data layouts (. is regex)\n"
<< "    option --inbuilt requires a step argument of 2 or more\n"
<< "    option --exittrip increases the sensitivity of failure based on reporting level\n"
<< "    option --inbuilt and option --guard cannot be used together\n"
<< "    option --krazy is provided for testing and can yield useful insights\n"
<< "    option --yeek is documented below\n"

<< "\n"

<< "output streams:\n"
<< "    stdout (file descriptor 1) = splash screens, help and usage messages"
    ", caught exception notifications\n"
<< "    stderr (file descriptor 2) = logging output, caught exception explanations\n"

<< "\n"

<< "compiled-in literals which affect application behavior (see also --data):\n"
<< "    console width (set via _XCOLS macro)   : " << xeona::consoleWidth     << "\n"

<< "\n"

<< "command-line examples:\n"

<< "  run using all defaults including the default model name '"
<< xeona::modelStubDefault << xeona::modelExt
<< "':\n"
<< "    $ " << getProgramName() << "\n"
<< "  generate and run the compiled-in test model using the default name '"
<< xeona::modelInbuiltDefault << xeona::modelExt
<< "':\n"
<< "    $ " << getProgramName() << "  --inbuilt 24\n"
<< "  run '"
<< "mymodel" << xeona::modelExt
<< "' in restricted mode five and silent except for 'main.cc' log calls:\n"
<< "    $ " << getProgramName() << "  --mode 5  --report 0  --watch main  mymodel\n"
<< "  run '"
<< "mymodel" << xeona::modelExt
<< "' without defensive coding to completion or failure (and core dump):\n"
<< "    $ " << getProgramName() << "  --krazy  mymodel\n"
<< "  copy the guard model "
<< "'trial" << xeona::modelGuardTag << xeona::modelExt
<< "' to "
<< "'trial" << xeona::modelExt
<< "' first and run that:\n"
<< "    $ " << getProgramName() << "  --guard  trial\n"
<< "  run with enough reporting to track simulation progress:\n"
<< "    $ " << getProgramName() << "  --report 2  --watch overseer  --inbuilt 24\n"
<< "  minimal run which generates a representative two-step model file '"
<< xeona::modelInbuiltDefault << xeona::modelExt
<< "':\n"
<< "    $ " << getProgramName() << "  --mode 1  --inbuilt 2\n"
<< "  run using all defaults, but exit "
<< logga::kill << " if kill calls occur and exit " << logga::warn
<< " if warn calls occur:\n"
<< "    $ " << getProgramName() << "  --exittrip " << logga::warn << "\n"
<< "  minimal run which displays useful splash-screen information:\n"
<< "    $ " << getProgramName() << "  --report 0  --mode 1\n"
<< "  display all supported entity names, along with their respective headers:\n"
<< "    $ " << getProgramName() << "  --class +\n"
<< "  hunt for a particular entity name using the 'grep' pattern matching utility:\n"
<< "    $ " << getProgramName() << "  --class +  |  grep  --ignore-case  substring\n"
<< "  convenient way to read usage message using the 'less' pager:\n"
<< "    $ " << getProgramName() << "  --usage  |  less\n"
<< "  convenient way to read and search run-time output"
    " using the 'less' pager:\n"
<< "    $ " << getProgramName() << "  --report 6  mymodel  2>&1  |  less [+/WARN]\n"

<< "\n"

<< "entity name prefixes, useful with the --class option regex (can add leading ^):\n"
<< "    Asop = asset operator\n"
<< "    Cm   = commodity\n"
<< "    Cx   = context\n"
<< "    Gate = gateway\n"
<< "    Junc = demand join/split\n"
<< "    Node = grid node (nodal pricing and two-way flow)\n"
<< "    Teas = technical asset\n"

<< "\n"

<< "subdirectory roles, useful with the --watch option:\n"
<< "    ./ = core files\n"
<< "    a/ = logging, reporting, and xeona exceptions\n"
<< "    b/ = entity library (excluding contexts)\n"
<< "    c/ = workhorse code\n"
<< "    d/ = optimization solver interface code\n"
<< "    e/ = context library\n"
<< "    f/ = graph traversal routines, CTA capset and transolve algorithm, and similar\n"
<< "    g/ = compiled-in timeseries (model development)\n"
<< "    h/ = engineering modules (PV model, HVAC chiller model, CIBSE building model)\n"

<< "\n"

<< "option --yeek values:\n"
<< xeona::yeekSummarize(4)                   // note the indent argument

<< "\n"

<< "exit codes generated by the application:\n"
<< exitstatii                                // string from beginning of function block

<< "\n"

<< "exit codes due to operating system POSIX signals and some common causes:\n"
<< "   130 = user-initiated ^C (^ = ctrl) interrupt (not trapped)  SIGINT  =  2\n"
<< "   134 = glibc detects memory corruption during malloc call    SIGABRT =  6\n"
<< "   134 = glibc detects invalid pointer during free call        SIGABRT =  6\n"
<< "   134 = terminate on uncaught throw                           SIGABRT =  6\n"
<< "   134 = GLPK abort call                                       SIGABRT =  6\n"
<< "   137 = user-initiated hard kill (not trapped)                SIGKILL =  9\n"
<< "   139 = invalid memory use causing a segmentation fault       SIGSEGV = 11\n"
<< "   143 = user-initiated mild kill (not trapped)                SIGTERM = 15\n"

<< "\n"

<< "useful terminal control keys (else see $ stty --all)\n"
<< "    ^C  interrupt             not trapped                      SIGINT  =  2\n"
<< "    ^\\  quit                  not trapped, core dump           SIGQUIT =  3\n"
<< "    ^S  stop     ^Q  start\n"
<< "    ^Z  suspend  $ fg         pending output lost\n"

<< "\n"

<< "useful external programs (both have manpages):\n"
<< "  display resource usage information on completion (the options are correct)\n"
<< "    $ /usr/bin/time  --quiet  --verbose  " << getProgramName() << "\n"
<< "  check for memory leaks and faults (requires valgrind package)\n"
<< "    $ valgrind  --leak-check=full  " << getProgramName() << "\n"

<< "\n"

<< "command-line processing caution:\n"
<< "  options processed in the order given by --help and NOT in the order submitted\n"

<< "\n"

<< "software: "
<< getTitle()
<< "\n"

<< "notice: "
<< "distributed under the GNU General Public License version 3 and\n"
<< "  without warranty to the extent permitted by applicable law"
<< "\n"

<< "copyright: (c) "
<< getCopyright()
<< "\n";

  // note that Robbins (2005 pp255-300) devotes a chapter to POSIX signals

  s_logger->addSmartBlank();                 // next logger call should add a blank line
  s_logger->addFinalStdoutBlank();           // add a final blank to stdout

  return ss.str();
}

const std::string
AppInfo::showLegalMessage() const
{
  std::ostringstream buf;
  buf
<< ""
<< "  Software  : " << getTitle()                                                 << "\n"
<< ""                                                                             << "\n"
<< "  Copyright : (c) " << getCopyright() << "."                                  << "\n"
<< ""                                                                             << "\n"
<< "  Request   : This software is distributed with the request that you forward" << "\n"
<< "              any modifications you make to the xeona project for possible"   << "\n"
<< "              inclusion in the main codebase."                                << "\n"
<< ""                                                                             << "\n"
<< "  License   : This software is distributed under the license which follows."  << "\n";

  std::ostringstream ss;
  ss << ""                             << "\n"
     << buf.str()                      << "\n"
     << ::licenseText                  << "\n"    // file-local string literal
     << "  End of xeona legal notice." << "\n"
     << ""                             << "\n";

  return ss.str();
}

const std::string
AppInfo::showXemRules() const
{
  std::string output;                        // variable to be returned
  output.reserve(10000);                     // estimate via $ xeona  --data  | wc --chars

  output +=                                  // intro line

    "\n"
    "xeona data rules\n";

  output +=                                  // rules

    "\n"

    "text alignment\n"
    "    column 0 has no leading space, column 4 has four leading spaces\n"
    "    record identifiers must start in column 0, data fields and comments may "
    "start anywhere\n"
    "    the same alignment rule applies to the record and field disable character "
    "'" + xeona::modelDisableChar + "'\n"
    "    by convention, data fields are placed in column 4 and comments in column 6\n"
    "\n"

    "xeona model file\n"
    "    a XEM file is a set of records\n"
    "    the record order has no semantic significance\n"
    "\n"

    "records\n"
    "    each record begins with an identifier placed in column 0 (otherwise the line is "
    "deemed a comment!)\n"
    "    the leading dot-separated part of each identifier indicates the "
    "record kind (see below)\n"
    "    processing ceases when the special '" + xeona::modelEndMarker + "' "
    "record identifier is met\n"
    "    blank lines are not intrinsically significant\n"
    "    column alignment is not significant, except for the placement of the "
    "record identifier\n"
    "    a note record is treated as set of comment lines, with multiple blank "
    "lines removed on output\n"
    "    records are written out in the order that they are read in\n"
    "\n"

    "record kinds (with identifiers):\n"
    "    note         'note'         : verbatim text "
    "(except multiple blank lines are squeezed)\n"
    "    program      'program.*'    : run information\n"
    "    entity       'entity.*'     : model entities\n"
    "    model end    'model-end'    : processing stops (remaining text is lost)\n"
    "\n"

    "fields\n"
    "    a record is made up of zero or more data fields and/or comments\n"
    "    any field containing a single angle bracket is a data field, otherwise it "
    "is a comment\n"
    "    each data field has a leading identifier, followed optionally by \"[unit]\" "
    "and optionally by a remark\n"
    "    a comment may possess matched angle brackets (for instance, "
    "<email@address> is legal in a comment)\n"
    "\n"

    "data\n"
    "    a data field may hold a single value, a timeseries, an individual "
    "entity identifier, or a list of identifiers\n"
    "    single or timeseries values may be of data-type "
    "string, integer, float, or boolean\n"
    "    a string value is set in double quotes, a boolean value is either 0 (false) "
    "or 1 (true)\n"
    "    a connectivity list comprises zero or more space-separated entity "
    "identifiers placed within one set of double quotes\n"
    "    entities may disallow the use of null entity identifiers in some cases "
    "(as documented)\n"
    "\n"

    "disabling records and fields\n"
    "    a record can be disabled by placing a leading '" + xeona::modelDisableChar + "'"
    " in column 0 (and nowhere else)\n"
    "    a data field can be disabled by placing a leading "
    "'" + xeona::modelDisableChar + "' anywhere (but best to avoid column 0)\n"
    "    a disabled field must appear AFTER its respective enabled field\n"
    "    a leading '" + xeona::modelDisableChar + "' on a comment line is "
    "automatically removed and not replaced (think of this as a feature)\n"
    "\n"

    "reformatting on write out\n"
    "    xeona reformats the XEM file on write out\n"
    "    most notably, in-data, out-data, and comments are grouped together "
    "in that order\n"
    "    additional support is provided for emacs text editor local variables "
    "if originally present\n"
    "\n"

    "field kinds:\n"
    "    comment     : no angle bracket (or alternatively matched angle brackets)\n"
    "    in-data     : >\n"
    "    out-data    : <\n"
    "\n"

    "data values\n"
    "    floats may be input in either decimal or exponent format: "
    "-0.01234 or -12.34e-03\n"
    "    a trailing \"" + xeona::modelTsRepeater + "\" on timeseries input data "
    "means repeat that pattern as often as needed\n"
    "    xeona holds floats using the 8-byte 'double' arithmetic type"
    " (assuming IEEE 754 support)\n"
    "\n"

    "mandatory entities with preset identifiers:\n"
    "    TimeHorizon   entity.time-horizon\n"
    "    Overseer      entity.overseer\n";

  output +=                                  // field value prompts

    "\n"
    "field value data-type prompts "
    "(not actively parsed but provided to assist data preparation):\n"
    "        0  null for completeness\n"
    "    entity connectivity (in-data only, both can be null in some circumstances)\n"
    "        l  individual entity identifier within double quotes\n"
    "        L  space-separated list of entity identifiers within one set of "
    "double quotes\n"
    "    normal data (both in-data and out-data)\n"
    "        s  single string in double quotes\n"
    "        S  timeseries of strings with each element in double quotes\n"
    "        i  single integer\n"
    "        I  timeseries of integers\n"
    "        f  single floating-point number\n"
    "        F  timeseries of floating-point numbers\n"
    "        b  single boolean\n"
    "        B  timeseries of booleans\n"
    "    reinterpreted data (entity-specific in-data)\n"
    "        x  single character stream within double quotes "
    "(for instance, bidset data)\n"
    "        X  timeseries of above with each element in double quotes\n";

  output +=                                  // field units

    "\n"
    "field units (enclosed in square brackets):\n"
    "    physical quantities: unprefixed SI units are presumed "
    "(unless otherwise documented)\n"
    "        time: s\n"
    "        raw SI units: kg m ohm V etc\n"
    "        derived SI units: J Pa W etc\n"
    "        temperature: C or K (C preferred unless calculation utilizes K)\n"
    "    economic quantities:\n"
    "        time: y (year can be more intuitive than second in certain cases, "
    "for instance, rate of interest)\n"
    "        generic currency: $ (representing, for example, EUR or USD)\n"
    "    quantities lacking dimensionality:\n"
    "        unitless: - (normally preferred over %)\n"
    "        percentage: %\n"
    "    embedded costs:\n"
    "        reported $ per-interval (not $/s)\n";

  output +=                                  // entity documentation

    "\n"
    "entity documentation:\n"
    "    entity documentation can be obtained through xeona option --class +|regex\n"
    "    commodity-templated entities often use generic documentation, such that:\n"
    "        base commodity in { Cert, Cseq, Elec, Fiss, Fund, Heat, Oxid, Thrm, Work }\n"
    "        stand-in quantifying extensity field"
    " unit: * in { kg, J, $ } as appropriate\n";

  output +=                                  // compiled-in literals

    "\n"
    "compiled-in literals relating to data usage (set in unit 'common'):\n"
    "    ISO 4217 currency code                 : " + xeona::iso4217          + "\n"
    "    record or field disable string         : " + xeona::modelDisableChar + "\n"
    "    timeseries repeat indicator            : " + xeona::modelTsRepeater  + "\n"
    "    pairwise delimiter for string values   : " + xeona::modelStringDelim + "\n"
    "    nodal bid and tariff entry separator   : " + xeona::modelBidDelim    + "\n"
    "    model end marker                       : " + xeona::modelEndMarker   + "\n"
    "    separator string for OSP tag reporting : " + xeona::ospTagSep        + "\n";

  output +=                                  // abbreviations

    "\n"
    "data-related abbreviations:\n"
    "    EUR     euro\n"
    "    OSP     optimization sub-problem\n"
    "    XEM     xeona model\n";

  const std::string trial    = "trial";
  const std::string trialxem = trial + xeona::modelGuardTag + xeona::modelExt;

  output +=                                  // useful command-lines

    "\n"
    "some useful command-lines for developing XEM files:\n"
    "    $ " + getProgramName() + "  --inbuilt 2\n"
    "    $ cp  " + xeona::modelInbuiltDefault + xeona::modelExt + xeona::backupTag + "  "
    + trialxem + "\n"
    "    $ emacs  " + trialxem + "  &   # or your preferred editor\n"
    "    $ " + getProgramName() + "  --guard " + trial + "  "
    "&&  cat  " + trial + xeona::modelExt + "\n";

  output +=                                  // final caution

    "\n"
    "final caution:\n"
    "    entity authors may not have respected these guidelines in their entirety\n"
    "    that said, non-standard practice should be signaled via the"
    " builtin-remark field\n";

  output += "\n";                            // blank line

  // return the output
  return output;
}

const std::string
AppInfo::showParse() const
{
  std::string FS = " : ";                    // field separator

  std::string debugInfo                 = "0";
  if ( xeona::DBUG ) debugInfo          = "1";

  std::string ndebugInfo                = "0";
  if ( xeona::NBUG ) ndebugInfo         = "1";

  std::string buildStatus               = "non-release";
  if ( getReleaseStatus() ) buildStatus = "release";

  std::ostringstream ss;
  ss << "invoked-name"
     << FS
     << getProgramName()
     << "\n"

     << "compile-date"
     << FS
     << getBuildDate()
     << "\n"

     << "build-status"
     << FS
     << buildStatus
     << "\n"

     << "codebase-status"
     << FS
     << getCodebaseStatus()
     << "\n"

     << "_XDEBUG-app"
     << FS
     << debugInfo
     << "\n"

     << "NDEBUG-libs"
     << FS
     << ndebugInfo
     << "\n"

     << "svn-revision"
     << FS
     << getSvnRevision()
     << "\n";

  return ss.str();
}

// Note: representative output and script parsing examples using
// awk (also gawk, nawk) AND "begin" should read "BEGIN" (duly
// changed to prevent '/usr/bin/file' from misreporting this
// file)
//
// $ xeona-mach --version
// invoked-name : xeona-mach
// code-status : work-in-progress
// debug-info : omitted
// svn-date : 2007-04-17
// svn-revision : 347
//
// $xeona-mach --version [backslash]
//   | awk 'begin { FS = " : " } $1 ~ /svn-revision/ { print $2 }'
// 10
// note that the default RS is a newline

const std::string
AppInfo::showFinal
(xeona::SimRet simRet,
 const int     exitStatus) const
{
  // process logger rank for this stream
  const std::string logCallList = s_logger->getAllTriggerStr();

  // process xeona::SimRet enum
  std::string simInterp = "(string not overwritten)";
  switch ( simRet )
    {
    case xeona::e_statusNotKnown:   simInterp = "indeterminate";                break;
    case xeona::e_success:          simInterp = "simulate() returned success";  break;
    case xeona::e_modelFileFault:   simInterp = "faulty model file";            break;
    case xeona::e_infeasibility:    simInterp = "infeasibility encountered";    break;
    case xeona::e_errantSimulation: simInterp = "problems encountered";         break;
    case xeona::e_testCodeUsed:     simInterp = "downrated test code utilized"; break;
    case xeona::e_other:            simInterp = "(other)";                      break;
    default:
      s_logger->repx(logga::warn, "uncoded simRet", simRet);
    }

  // stop the clock!
  //
  // There is no real loss of accuracy having the application
  // timer located within the 'AppInfo' class rather than within
  // some global class.  A full run with a non-optimized binary,
  // with _XDEBUG set, and an deliberately incorrect model file
  // name takes under 10 milliseconds on my circa 2004 32-bit
  // laptop (gogol) with 1.4GHz Intel Celeron M processor.
  // Hence:
  //
  //     $ /usr/bin/time --portability xeona.mach --report 0 xxx
  //
  //     Command exited with non-zero status 10
  //     real 0.01
  //     user 0.00
  //     sys 0.00

  boost::posix_time::ptime toc(boost::posix_time::microsec_clock::universal_time());
  boost::posix_time::time_duration delta = toc - d_tic;

  std::string exitInterp;
  {                                          // local block to force early dtor logging
    ExitStatus exitstatus("(coding error, rerun at --report 2)"); // not-present message
    if ( ! exitstatus(exitStatus, exitInterp) )
      {
        s_logger->repx(logga::warn, "exit status not in database", exitStatus);
      }
  }

  // prepare the output
  std::ostringstream ss;
  ss << ""                                                                      << "\n"
     << "  final information"                                                   << "\n"
     << ""                                                                      << "\n"
     << "     command-line      :  " << d_fullCmdLine                           << "\n"
     << "     start directory   :  " << d_startPath                             << "\n"
     << "     elapsed time      :  " << xeona::formatDuration(delta)            << "\n"
     << "     log calls to date :  " << logCallList                             << "\n"
     << "     simulation state  :  " << simInterp                               << "\n"
     << "     application exit  :  " << exitStatus << " = " << exitInterp       << "\n";

  s_logger->addSmartBlank();                 // next logger call should add a blank line
  s_logger->addFinalStdoutBlank();           // add a final blank to stdout

  return ss.str();
}

const std::string
AppInfo::getLogname() const
{
  const char* logname = getenv("LOGNAME");   // <cstdlib>
  if ( logname ) return logname;
  else           return "(not determined)";
}

const std::string                            // format: 2007-04-16
AppInfo::getCurrentDate() const
{
  boost::posix_time::ptime now
    = boost::posix_time::second_clock::universal_time();
  return boost::gregorian::to_iso_extended_string(now.date());
}

const std::string                            // format: 23:59:59
AppInfo::getCurrentTime() const
{
  boost::posix_time::ptime now
    = boost::posix_time::second_clock::universal_time();
  return boost::posix_time::to_simple_string(now.time_of_day());
}

const std::string
AppInfo::getReportLevel() const
{
  std::ostringstream ss;
  ss << s_logger->getReportLevelStr()
     << " ("
     << s_logger->getReportLevelInt()
     << ")";
  return ss.str();
}

const std::string
AppInfo::getCommandLineModelName() const
{
  return d_clModelName;
}

// MANIPULATORS

void
AppInfo::setCommandLineReportLevel
(const unsigned reportLevel)
{
  d_clReportLevel = reportLevel;
}

void
AppInfo::setCommandLineModelName
(const std::string& modelName)
{
  d_clModelName = modelName;
}

// PRIVATE FUNCTIONS

bool                                         // fail also sets field to ""
AppInfo::splitString
(const std::string& record,                  // input space-separated string
 std::string&       field,                   // selected output field
 const unsigned int index) const             // field index, zero-based
{
  std::vector<std::string> output;                     // buffer
  boost::split(output, record, boost::is_any_of(" ")); // useful 'split' function
  if ( index < output.size() )
    {
      field = output.at(index);              // will throw if out of range
      return true;
    }
  else
    {
      field = "";
      return false;
    }
}

const boost::filesystem::path
AppInfo::pathFromString
(const std::string& path) const
{
  // create buffer string
  std::string buff(path);                    // CAUTION: also converts to non-const

  // strip any leading "./" or ".\\"
  const char native = xeona::osSlash;        // native path separator set in 'common.cc'
  const std::string slash(1, native);        // CAUTION: note char constructor syntax
  const std::string dot(1, '.');
  const std::string junk = dot + slash;      // the offending non-leaf path
  if ( boost::starts_with(buff, junk) )
    buff = buff.substr(2);                   // remove first two chars

  // form complete (absolute) path
  return boost::filesystem::absolute(buff); // note the default second argument [1]

  // [1] boost::filesystem::absolute() has a default second
  // argument of boost::filesystem::initial_path(), this being
  // the current directory at the time of entry into main().
  //
  // CAUTION: system_complete has different behavior so read the
  // documentation if you wish to change from POSIX conformance
  // to operating system-based format rules
}

const std::string
AppInfo::extractLeaf                         // the leaf is the final part of the path
(const boost::filesystem::path path) const
{
  boost::filesystem::path temp
    = d_progPath.filename();                 // grab leaf
  return temp.string();                      // 'file_string' outputs os native format
}

const std::string
AppInfo::extractPath
(const boost::filesystem::path path) const
{
  return d_progPath.string();                // 'file_string' outputs os native format
}

// pad numbers
std::string
AppInfo::zeroPadMe
(const std::string& number,
 const int         padlevel) const
{
  int num = boost::lexical_cast<int>(number);
  std::ostringstream ossBuf;                 // formatting buffer
  ossBuf << std::setw(padlevel)              // define overall width
         << std::setfill('0')                // define zero pad character
         << std::fixed                       // adopt fixed formatting
         << num;
  return ossBuf.str();
}

std::string
AppInfo::argProcess
(int    argc,
 char** argv) const
{
  std::vector<std::string> argValues;
  std::string argConcat;

  for ( int i = 0; i < argc; ++i )
    argValues.push_back(argv[i]);

  BOOST_FOREACH( std::string s, argValues )
    argConcat += s + " ";

  boost::trim(argConcat);                    // trim trailing space
  return argConcat;
}

// STATIC DATA

logga::spLogger
AppInfo::s_logger = logga::ptrLogStream();   // bind logger on definition

//  end of file

