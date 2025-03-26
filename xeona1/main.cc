//  XEONA ENERGY SYSTEMS MODELING ENVIRONMENT
//
//  file-create-name : main.cc
//  file-create-date : Mon 16-Apr-2007 11:45 UTC
//  file-initiator   : Robbie Morrison <robbie@actrix.co.nz>
//
//  file-role        : contains 'main' function, includes command-line parsing / main
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
//  $URL: file:///home/robbie/svn-root/xeona/futz/trunk/xeona1/main.cc $
//
//  GENERAL NOTES FOR THIS FILE
//
//  This is the main file for the xeona application.

//  LOCAL AND SYSTEM INCLUDES

#include "a/logger.h"         // standard logging functionality (as required) [1]

// [1] CAUTION: "a/logger.h" should be first -- there were early
// problems with the logger destructor '~Logger' but that could
// well have been due to the used of 'exit()' calls -- which no
// longer applies

#include "i/gnuplot.h"        // gnuplot interface, originally by Daniel Stahlke
#include "c/xeona_ptr.h"      // remappable counted pointer which mimics shared_ptr
#include "c/simcall.h"        // main simulation call
#include "c/inbuilt.h"        // compiled-in test file generation
#include "a/yeek.h"           // yeek (for running extra code) value interpretation
#include "a/xemopt.h"         // skeleton xem model generator
#include "a/xedocs.h"         // processing of 'xedoc' entity documentation
#include "a/exitstat.h"       // exit status database
#include "a/exapp.h"          // application exception classes
#include "a/appinfo.h"        // generates version info, splash screens, and so forth

#include "./common.h"         // common definitions for project (place last)

#include <iostream>           // standard io
#include <sstream>            // string-streams
#include <stdexcept>          // standard exception classes, runtime_error()
#include <string>             // C++ strings

#include <cstdio>             // C-style io, remove(), EOF, perror()

#include <unistd.h>           // POSIX sleep(), usleep(), access(), chown()

#include <boost/algorithm/string.hpp>   // string recasing, trimming, splitting
#include <boost/filesystem.hpp>         // path objects, iterators, useful operations
#include <boost/format.hpp>             // printf style formatting
#include <boost/program_options.hpp>    // command-line parser and more
#include <boost/weak_ptr.hpp>           // Boost weak pointer

// CAUTION: Boost.Program_options needs a -lboost_program_options linking directive

//  COMPILE-TIME ASSERTIONS

#include "./sasserts.h"       // compile-time assertions (header only)

//  FORWARD (PARTIAL) DECLARATIONS

class Entity                                 // unit 'b/entity'
{
public:
  static std::string reportFullPopulation();
};

//  CODE

// ---------------------------------------------------------
//  MAIN FUNCTION   : main
// ---------------------------------------------------------

int
main
(int   argc,                                 // CAUTION: do not use 'const' here
 char* argv[])                               // CAUTION: do not use 'const' here
{
  // ---------------------------------
  //  preamble
  // ---------------------------------

  // declare some returns, these values should ALWAYS be overwritten
  xeona::SimRet sret = xeona::e_statusNotKnown;        // return enum from 'simulate'
  int returnStatus   = xeona::ret_not_overwritten;     // application return status

  // main function logger
  logga::spLogger logger = logga::ptrLogStream();      // bind logger
  logger->repx(logga::dbug, "start of main", "~~~~~~~~~~");

  // grab initial current directory
  const boost::filesystem::path initial = boost::filesystem::current_path();  // [1]

  // [1] this functionality used to be provided by
  // 'boost::filesystem::initial_path' but that function was
  // withdrawn with Boost.Filesystem version 3 under 1.44.0

  // create application information object
  AppInfo appInfo(argc, argv, initial);      // pass thru command-line information

  // declare variables which may have been set on the command-line
  unsigned beepMode       = 0;               // changeable via --beep
  unsigned reportLevel    = 0;               // changeable via --report
  unsigned simulationMode = 0;               // changeable via --mode
  unsigned inbuiltSteps   = 0;               // changeable via --inbuilt
  unsigned exitTripLevel  = 0;               // changeable via --exittrip
  unsigned outputQuery    = 1000;            // set via --output, value is outside [0,255]
  unsigned yeekValue      = 0;               // see via --yeek
  std::string cmdLineModelName;              // single word or several if in quotes
  std::string logTrigger;                    // single word without whitespace for --watch
  std::string xedocClass;                    // single word without whitespace for --class
  std::string xemPart     = "invalid";       // one of several supported strings for --xem
  std::string gnuplotTerm = "invalid";       // one of several supported string for --tout

  // declare required default values (others are also set in "common.h")
  xeona::SimKind simKind = xeona::e_fullRun; // simulation mode (as enum)

  // declare some flags
  bool flag_generateInbuiltFile    = false;  // reset by --inbuilt
  bool flag_refreshModelIfPossible = false;  // reset by --guard

  // =================================
  //  parse command-line using Boost
  // =================================

  //  Boost.Program_options parses as follows:
  //
  //   * options are displayed and processed in the order given
  //     under 'add_options' function
  //
  //   * the first error encountered prevents further parsing

  namespace po = boost::program_options;     // for convenience

  logger->repx(logga::dbug, "command-line parsing, argc", argc);

  std::ostringstream ossBeepMsg;             // for use in --help message
  ossBeepMsg
    << "set beep behavior "
    << "0-2"
    << " (silent to noisy) (default "
    << boost::format("%d") % xeona::beepModeDefault         // set in 'common.cc'
    << ")";
  std::string sBeepMsg = ossBeepMsg.str();
  const char* cbeepMsg = sBeepMsg.c_str();

  std::ostringstream ossReportMsg;           // for use in --help message
  ossReportMsg
    << "set report level "
    << boost::format("%d-%d") % logga::yeek % (logga::adhc + 1)
    << " (nil to verbose) (default "
    << boost::format("%d") % xeona::reportLevelDefault      // set in 'common.cc'
    << ")";
  std::string sReportMsg = ossReportMsg.str();
  const char* creportMsg = sReportMsg.c_str();

  std::ostringstream ossModeMsg;             // for use in --help message
  ossModeMsg
    << "set run mode "
    << boost::format("%d-%d") % xeona::e_hollowCall % xeona::e_resampleRun
    << " (reduced to extended) (default "
    << boost::format("%d") % xeona::e_fullRun
    << ")";
  std::string sModeMsg = ossModeMsg.str();
  const char* cmodeMsg = sModeMsg.c_str();

  std::ostringstream ossExitTripMsg;          // for use in --help message
  ossExitTripMsg
    << "set nonzero exit trip to ARG level logs (default "
    << boost::format("%d") % xeona::exitTripLevelDefault    // set in 'common.cc'
    << ")"
    << "\n";
  std::string sExitTripMsg = ossExitTripMsg.str();
  const char* cexitTripMsg = sExitTripMsg.c_str();

  // ---------------------------------
  //  command-line 'try' block
  // ---------------------------------

  try
    {
      // specify the options displayed under --help, note use of newlines
      const unsigned optionsWidthDefault = po::options_description::m_default_line_length;
      const unsigned optionsWidth        = 100;   // effective iff greater than default
      logger->repx(logga::dbug, "program_options default width", optionsWidthDefault);
      logger->repx(logga::dbug, "program_options given width"  , optionsWidth);
      const char* cOptionsMsg =
        "options (processed in order shown, can be abbreviated)";
      po::options_description desc(cOptionsMsg, optionsWidth);
      desc.add_options()
        ("help",     "display command-line options and quit")
        ("usage",    "display longer usage message and quit")
        ("output",
         po::value<unsigned>(&outputQuery), // default value not provided
         "describe application return code ARG and quit\n")

        ("legal",    "display copyright notice and license and quit")
        ("svn",      "display embedded source code revision number and quit")
        ("version",  "display version details in structured form and quit\n")

        ("data",  "display xem file data rules and quit")
        ("class",
         po::value<std::string>(&xedocClass),
         "display fields for entity class regex ARG and quit")
        ("xem",
         po::value<std::string>(&xemPart),
         "display ARG head|mand|tail|comb xem file parts and quit\n")

        ("quiet",    "silence all output except the above (release builds)\n")

        ("beep",
         po::value<unsigned>(&beepMode),
         cbeepMsg)                           // CAUTION: must be a const C-string
        ("report",
         po::value<unsigned>(&reportLevel),
         creportMsg)
        ("nodata",   "empty data fields generate warnings not debugs")
        ("jumpy",    "range failures and such generate warnings not debugs")
        ("watch",
         po::value<std::string>(&logTrigger),
         "report fully from sources matching ARG regex")
        ("exittrip",
         po::value<unsigned>(&exitTripLevel),
         cexitTripMsg)

        ("mode",
         po::value<unsigned>(&simulationMode),
         cmodeMsg)
        ("pepper",   "reseed random number generators using time() return")
        ("again",    "continue when choked or hard problems encountered")
        ("krazy",    "omit defensive coding and try to run to failure")
        ("yeek",
         po::value<unsigned>(&yeekValue),
         "run non-standard code based on ARG (changeable)")
        ("zero",     "disable close-to-zero solver rounding (overrides models)\n")

        ("inbuilt",
         po::value<unsigned>(),
         "generate and run inbuilt test model using ARG steps")
        ("guard",    "regenerate xem file from guard file if present")
        ("tout",
         po::value<std::string>(&gnuplotTerm),
         "graph timeseries in gnuplot terminal ARG dumb|x11|wxt|svg")
        ; // final semicolon

      // specify options not displayed under --help
      po::options_description hidden("hidden options");
      hidden.add_options()
        ("filename",                         // CAUTION: must be processed after "inbuilt"
         po::value<std::string>(&cmdLineModelName),
         "model name");

      // combine the above
      po::options_description cmdline_options;
      cmdline_options.add(desc).add(hidden);

      // specify treatment of stand-alone argument
      po::positional_options_description pod;
      pod.add("filename", -1);

      // final admin
      po::variables_map vm;
      po::store
        (po::command_line_parser(argc, argv).     // CAUTION: not 'parse_command_line'
         options(cmdline_options).positional(pod).run(), vm);
      po::notify(vm);                             // CAUTION: necessary call

      // ---------------------------------
      //  process command-line options
      // ---------------------------------

      if ( vm.count("help") )
        {
          logger->setReportLevel(0);         // reduce further logging to a minimum
          std::clog << std::flush;
          std::cout << desc;                 // Boost.Program_options
          return xeona::ret_message;
        }

      if ( vm.count("usage") )
        {
          logger->setReportLevel(0);         // reduce further logging to a minimum
          std::clog << std::flush;
          std::ostringstream ssDesc;
          ssDesc << desc;                    // to make 'desc' available to AppInfo
          std::cout << appInfo.showHelp(ssDesc.str());
          return xeona::ret_message;
        }

      if ( vm.count("output") )
        {
          logger->setReportLevel(0);         // reduce further logging to a minimum
          std::clog << std::flush;
          ExitStatus exitstatus;             // accept default not-present message
          std::string interpretation;        // fill by reference
          if ( exitstatus(outputQuery, interpretation) )   // 'true' if present
            {
              std::cout << interpretation << "\n";
              std::cout << std::flush;
            }
          return xeona::ret_message;
        }

      if ( vm.count("legal") )
        {
          std::clog << std::flush;
          std::cout << appInfo.showLegalMessage();
          return xeona::ret_message;
        }

      if ( vm.count("svn") )
        {
          std::clog << std::flush;
          std::cout << appInfo.getSvnRevision() << std::endl;
          return xeona::ret_message;
        }

      if ( vm.count("version") )
        {
          std::clog << std::flush;
          std::cout << appInfo.showParse();
          return xeona::ret_message;
        }

      if ( vm.count("class") && ! xedocClass.empty() )
        {
          if ( boost::find_token(xedocClass, boost::is_space()) )
            {
              std::ostringstream oss;
              oss << "'class' argument cannot contain whitespace: "
                  << xedocClass;
              throw std::domain_error(oss.str()); // see Lischner (2003 p658)
            }
          else if ( xedocClass == "+" )           // not expanded by the shell
            {
              std::string plusStr = "\"" + xedocClass + "\"";
              logger->repx(logga::info, "class list requested, argument", plusStr);
              std::string classes = "";
              Xedocs xedocs;
              const int numberClasses = xedocs.dumpClassNames(classes);
              std::clog << std::flush;
              if ( numberClasses == 0 )
                {
                  std::cout << appInfo.getProgramName()
                            << ": entity class database not refreshed at compile-time"
                            << " (probably built with 'make' call and not 'mach' script)"
                            << std::endl;
                }
              else
                {
                  std::cout << "\n"
                            << classes
                            << std::endl;
                }
              return xeona::ret_message;
            }
          else
            {
              std::string regexStr = "\"" + xedocClass + "\"";
              logger->repx(logga::info, "xedocs requested for regex", regexStr);
              std::string xedoc = "";
              Xedocs xedocs;
              Xedocs::FindStatus findStatus = xedocs.findXedocForRegex(xedocClass, xedoc);
              std::clog << std::flush;

              switch ( findStatus )
                {
                case Xedocs::e_classFound:
                  {
                    std::cout << "\n" << xedoc << "\n" << std::endl;
                    return xeona::ret_success;
                  }
                case Xedocs::e_classNotFound:
                  {
                    std::cout << "case-sensitive regex search for '"
                              << xedocClass
                              << "' failed (try --class + for full listing)"
                              << std::endl;
                    return xeona::ret_noclass;
                  }
                case Xedocs::e_emptyDatabase:
                  {                          // CAUTION: local block necessary
                    std::cout << appInfo.getProgramName()
                              << ": entity class database not refreshed at compile-time"
                              << " (probably built with 'makefile' make call"
                              << " and not 'mach' build script)"
                              << std::endl;
                    return xeona::ret_noclass;
                  }
                default:
                  {
                    std::clog << "** coding error 01 in source file " << __FILE__
                              << std::endl;
                    return xeona::ret_fail;
                  }
                } // 'switch'
            }
        }

      if ( vm.count("xem") )
        {
          logger->flush();
          Xem xem(std::cout,                 // print to stream
                  xeona::svnRev,             // svn revision
                  45);                       // alignment tab (typically 45 or 50)
          if      ( xemPart == "head" )
            {
              xem.head();
              xem.blank();
            }
          else if ( xemPart == "mand" )
            {
              xem.mand(6);                   // number of steps
              xem.blank();
            }
          else if ( xemPart == "tail" )
            {
              xem.tail();
              xem.blank();
            }
          else if ( xemPart == "comb" )
            {
              xem.head();
              xem.mand(6);
              xem.more("a");                 // add a domain controller
              xem.blok("work in progress", "(domain 1 entities go here)");
              xem.tail();
            }
          else if ( xemPart == "string-delim" )
            {
              std::cout << xeona::modelStringDelim << std::endl;
            }
          else if ( xemPart == "disable-char" )
            {
              std::cout << xeona::modelDisableChar << std::endl;
            }
          else
            {
              std::ostringstream oss;
              oss << "'xem' argument not supported: " << xemPart;
              throw std::domain_error(oss.str());      // see Lischner (2003 p658)
            }
          xem.flush();
          logger->repx(logga::dbug, "--xem option code complete", xemPart);
          return xeona::ret_message;
        }

      if ( vm.count("quiet") )
        {
          // CAUTION: this block may well be UNIX-specific code

          // simply close 'stdout' and 'stderr'

          // CAUTION: the following might not cross-compile to
          // Windows -- in which case place under conditional
          // compilation

          // NOTE: there will be some leakage (around 16 console
          // lines) for non-release builds because of logging
          // output before this code is reached.

          std::cout << std::flush;
          std::clog << std::flush;

          if ( close(STDOUT_FILENO) == -1 )    // macro defined in <unistd.h>
            {
              std::clog << "** quiet code close stdout failed" << std::endl;
              perror("perror: failed to close stdout / system message");
            }
          if ( close(STDERR_FILENO) == -1 )    // macro defined in <unistd.h>
            {
              std::clog << "** quiet code close stderr failed" << std::endl;
              perror("perror: failed to close stderr / system message");
            }

          std::cout << std::flush;
          std::clog << std::flush;
        }

      if ( vm.count("data") )
        {
          std::clog << std::flush;
          std::cout << appInfo.showXemRules();
          return xeona::ret_message;
        }

      // CAUTION: the options above return, the options below do not

      if ( vm.count("again") )
        {
          const bool was = xeona::again;               // set 'false' in 'common.cc'
          if ( xeona::nopro ) xeona::again = false;    // toggle under '--again'
          else                xeona::again = true;
          std::ostringstream oss;
          oss << std::boolalpha;                       // use labels
          oss << was << " > " << xeona::again;
          logger->repx(logga::dbug, "xeona::again reset from > to", oss.str());
        }

      if ( vm.count("beep") )
        {
          switch ( beepMode )
            {
            case 2:
              logger->setBeepOnOrAbove(logga::warn);   // logga::yeek excluded
            case 1:
              logger->setBeepOnCompletion();
              logger->enableBeeping();
            case 0:                                    // do nothing
              break;
            default:
              {
                logger->repx(logga::dbug, "beep mode not supported", beepMode);
                std::ostringstream oss;
                oss << "'beep' " << beepMode << " is not supported";
                throw std::domain_error(oss.str());    // see Lischner (2003 p658)
              }
              break;
            }
        }

      if ( vm.count("exittrip") )
        {
          switch ( exitTripLevel )
            {
            case logga::yeek:
            case logga::kill:
            case logga::warn:
            case logga::info:
            case logga::dbug:
            case logga::xtra:
            case logga::adhc:
              {                              // CAUTION: keep any variables local
                logger->repx(logga::dbug,
                             "exit trip on log level set or reset",
                             exitTripLevel);
              }
              break;
            default:
              {
                logger->repx(logga::dbug,
                             "exit trip on log level unsupported",
                             exitTripLevel);
                std::ostringstream oss;
                oss << "exit trip on log level " << exitTripLevel << " is not supported";
                throw std::domain_error(oss.str());    // see Lischner (2003 p658)
              }
              break;
            }
        }

      // CAUTION: the following would not compile:
      // << vm["filename"].as<std::vector<std::string> >()
      // although the string version would

      if ( vm.count("filename") )            // quite possibly an empty string
        {
          appInfo.setCommandLineModelName(cmdLineModelName);     // just for reporting
        }

      if ( vm.count("guard") )
        {
          if ( flag_generateInbuiltFile )    // --inbuilt already processed
            {
              std::string msg = "cannot use --inbuilt and --guard together";
              throw std::invalid_argument(msg); // see Lischner (2003 p658)
            }
          flag_refreshModelIfPossible = true;
        }

      if ( vm.count("inbuilt") )
        {
          if ( cmdLineModelName.empty() )
            {
              cmdLineModelName = xeona::modelInbuiltDefault;
            }
          inbuiltSteps = vm["inbuilt"].as<unsigned>();

          if ( inbuiltSteps < 2 )
            {
              logger->repx(logga::dbug, "steps below 2 not supported", inbuiltSteps);
              std::ostringstream oss;
              oss << "'inbuilt' " << inbuiltSteps
                  << " is not legal (must be 2 steps or more)";
              throw std::domain_error(oss.str());      // see Lischner (2003 p658)
            }
          flag_generateInbuiltFile = true;   // flag used to delay code execution
        }

      if ( vm.count("krazy") )
        {
          const bool was = xeona::nopro;               // set 'false' in 'common.cc'
          if ( xeona::nopro ) xeona::nopro = false;    // toggle under '--krazy'
          else                xeona::nopro = true;
          std::ostringstream oss;
          oss << std::boolalpha;                       // use labels
          oss << was << " > " << xeona::nopro;
          logger->repx(logga::dbug, "xeona::nopro reset from > to", oss.str());
        }

      if ( vm.count("mode") )
        {
          if ( simulationMode == 0           // meaning less than 'xeona::e_hollowCall'
               ||
               simulationMode  > xeona::e_resampleRun )
            {
              logger->repx(logga::dbug, "simulation mode not supported", simulationMode);
              std::ostringstream oss;
              oss << "'mode' " << simulationMode << " is not supported";
              throw std::domain_error(oss.str());      // see Lischner (2003 p658)
            }

          // note that 'xeona::e_yearRun' will be duly processed
          // in 'c/simcall.cc' so there is nothing to do here

          simKind = static_cast<xeona::SimKind>(simulationMode);
        }

      if ( vm.count("nodata") )
        {
          // the following removes the need for conditionals when logging
          const logga::Rank wasRank = logga::rankNoData;    // logga::dbug
          logga::rankNoData         = logga::warn;
          std::ostringstream oss;
          oss << wasRank << " > " << logga::rankNoData;
          logger->repx(logga::dbug, "logga::rankNoData reset from > to", oss.str());
        }

      if ( vm.count("jumpy") )
        {
          // the following removes the need for conditionals when logging
          const logga::Rank wasRank = logga::rankJumpy;     // logga::dbug
          logga::rankJumpy          = logga::warn;
          std::ostringstream oss;
          oss << wasRank << " > " << logga::rankJumpy;
          logger->repx(logga::dbug, "logga::rankJumpy reset from > to", oss.str());
        }

      if ( vm.count("pepper") )
        {
          const bool was = xeona::pepper;              // set 'false' in 'common.cc'
          if ( xeona::pepper ) xeona::pepper = false;  // toggle under '--pepper'
          else                 xeona::pepper = true;
          std::ostringstream oss;
          oss << std::boolalpha;                       // use labels
          oss << was << " > " << xeona::pepper;
          logger->repx(logga::dbug, "xeona::pepper reset from > to", oss.str());
        }

      if ( vm.count("report") )
        {
          const unsigned clip                     // clip value
            = static_cast<unsigned>(logga::adhc) + 1;
          if ( reportLevel > clip )
            {
              reportLevel = clip;
              logger->repx(logga::dbug, "clipping --report arg back to", clip);
            }
          if ( reportLevel == clip )
            {
              logga::Logger::disableConsoleTruncation();
              logger->repx(logga::dbug, "console log truncation disabled", "");
              reportLevel = clip - 1;
            }
          logger->repx(logga::dbug,
                       "will set report level to value",
                       reportLevel);
          logger->setReportLevel(reportLevel);    // active call
        }
      else                                        // otherwise reset to 'common' default
        {
          logger->repx(logga::dbug,
                       "will reset report level to default",
                       xeona::reportLevelDefault);
          logger->setReportLevel(xeona::reportLevelDefault);
        }

      if ( vm.count("tout") )                // set gnuplot terminal
        {
          // check option argument, if valid continue, else throw
          switch ( xeona::okayGnuplotTerm(gnuplotTerm) )
            {
            case true:
              xeona::tout = gnuplotTerm;
              logger->repx(logga::dbug, "gnuplot terminal set to", gnuplotTerm);
              break;
            case false:                      // not currently supported
              {
                std::ostringstream oss;
                oss << "'tout' argument (gnuplot terminal) not supported: "
                    << gnuplotTerm;
                throw std::domain_error(oss.str());    // see Lischner (2003 p658)
              }
            }
        }

      if ( vm.count("watch") && ! logTrigger.empty() )
        {
          if ( boost::find_token(logTrigger, boost::is_space()) )
            {
              std::ostringstream oss;
              oss << "'watch' argument cannot contain whitespace: "
                  << logTrigger;
              throw std::domain_error(oss.str()); // see Lischner (2003 p658)
            }
          else
            {
              logger->repx(logga::info, "watch option set", logTrigger);
              logger->setTrigger(logTrigger);     // enable specific reporting
            }
        }

      if ( vm.count("yeek") )
        {
          const unsigned was = xeona::yeek;            // as set in 'common.cc'
          xeona::yeek        = yeekValue;              // update
          std::ostringstream oss;
          oss << was << " > " << xeona::yeek;
          logger->repx(logga::dbug, "xeona::yeek reset from > to", oss.str());

          // interpretation reporting
          std::string msg = xeona::yeekInterpret(yeekValue);
          if ( msg.empty() )
            {
              msg = "(not in database)";
              logger->repx(logga::warn, "yeek value not in database", yeekValue);
            }
          std::ostringstream put;
          put << "  yeek value : " << yeekValue << " = " << msg << "\n";
          logger->putx(logga::xtra, put);
        }

      if ( vm.count("zero") )
        {
          const bool was = xeona::zero;                // now set 'true' in 'common.cc'
          if ( xeona::zero ) xeona::zero = false;      // toggle under '--zero'
          else               xeona::zero = true;
          std::ostringstream oss;
          oss << std::boolalpha;                       // use labels
          oss << was << " > " << xeona::zero;
          logger->repx(logga::dbug, "xeona::zero reset from > to", oss.str());
        }

    } // command-line 'try' block

  // ---------------------------------
  //  command-line 'catch' blocks
  // ---------------------------------

  // CAUTION: 'stdout' and not 'stderr' used for direct reporting
  // as file descriptor 2 may well be redirected to '/dev/null'

  catch ( const std::exception& e )
    {
      std::clog << std::flush;
      std::cout
        << appInfo.getProgramName()
        << ": incorrect usage (try --help): "
        << e.what()
        << std::endl;
      logger->disableBeeping();              // no need to beep
      return xeona::ret_usage;               // return
    }
  catch ( ... )
    {
      std::clog << std::flush;
      std::cout
        << appInfo.getProgramName()
        << ": boost::program_options exception of unknown type"
        << std::endl;
    }

  // ---------------------------------
  //  display splash and info screens
  // ---------------------------------

  logger->repx(logga::dbug, "splash and info screen calls", "stdout");

  std::clog << std::flush;
  std::cout << appInfo.showSplash();         // first six lines, including blanks
  std::cout << appInfo.showInfo(simKind, beepMode, exitTripLevel);
  std::cout << std::flush;

  // =================================
  //  simulation call 'try' block
  // =================================

  // NOTE: the 'xeona::' variables are set in 'common.cc'

  try
    {
      std::string modelName = "";            // set via '--inbuilt' or '--guard' or null

      // ---------------------------------
      //  generate inbuilt model
      // ---------------------------------

      if ( flag_generateInbuiltFile )        // set by '--inbuilt'
        {
          // unit 'inbuilt'
          modelName = xeona::createModelName(cmdLineModelName);
          logger->repx(logga::info, "about to create test model", modelName);
          logger->repx(logga::info, "inbuilt steps set to", inbuiltSteps);
          const bool ret = xeona::dumpToFile(modelName, inbuiltSteps);
          if ( ret )
            logger->repx(logga::dbug, "write to file succeeded", ret);
          else
            logger->repx(logga::warn, "write to file failed", ret);
        }

      // ---------------------------------
      //  regenerate external test model
      // ---------------------------------

      else if ( flag_refreshModelIfPossible )          // set by '--guard'
        {
          // obtain stub
          std::string stub(cmdLineModelName);     // block local copy
          if ( stub.empty() )
            {
              stub = xeona::modelStubDefault;
            }
          if ( boost::ends_with(stub, xeona::modelExt) )         // trim ".xem"
            {
              boost::erase_tail(stub, xeona::modelExt.length());
            }
          if ( boost::ends_with(stub, xeona::modelGuardTag) )    // trim ".guard"
            {
              boost::erase_tail(stub, xeona::modelGuardTag.length());
            }

          // build file names
          std::string modelname = stub;
          modelname            += xeona::modelExt;          // add ".xem"
          std::string guardname = stub;
          guardname            += xeona::modelGuardTag;     // add ".guard"
          guardname            += xeona::modelExt;          // add ".xem"

          // create paths
          boost::filesystem::path model = boost::filesystem::absolute(modelname);
          boost::filesystem::path guard = boost::filesystem::absolute(guardname);

          // copy over file
          if ( exists(guard) )
            {
              logger->repx(logga::dbug, "reinstating model from", guard.filename());
              boost::filesystem::remove(model);  // copy_file will not overwrite
              boost::filesystem::copy_file(guard, model);
            }
          else
            {
              logger->repx(logga::warn, "guard file not found", guard.filename());
              logger->repx(logga::info, "not possible to reinstate model", "");
              throw xeona::file_not_found(guard.filename().string(), "guard file");
            }

          modelName = modelname;
        }

      // ---------------------------------
      //  straight name
      // ---------------------------------

      else
        {
          // test for presence of ".guard"
          std::string stub(cmdLineModelName);                    // block local copy
          if ( boost::ends_with(stub, xeona::modelExt) )         // trim ".xem"
            {
              boost::erase_tail(stub, xeona::modelExt.length());
            }
          if ( boost::ends_with(stub, xeona::modelGuardTag) )    // trim ".guard"
            {
              throw xeona::cannot_run_guard(cmdLineModelName);   // choke
            }
          modelName = cmdLineModelName;
        }

      // ---------------------------------
      //  run simulation, see unit 'simcall'
      // ---------------------------------

      const std::string value = '"' + modelName + '"';
      logger->repx(logga::dbug, "about to call simulate using", value);

      sret = simulate(modelName,             // from command-line or default
                      simKind);              // from '--mode' option or default

      // ---------------------------------
      //  reprocess 'sret'
      // ---------------------------------

      int contextBasedReturn = -1;                                 // nonsensical value
      if ( xeona::releaseStatus == true ) contextBasedReturn = xeona::ret_test_code_used;
      else                                contextBasedReturn = 0;  // ease of development

      switch ( sret )                        // simulation call return
        {
  case xeona::e_success:          returnStatus = xeona::ret_success;           break;
  case xeona::e_modelFileFault:   returnStatus = xeona::ret_model_file_fault;  break;
  case xeona::e_errantSimulation: returnStatus = xeona::ret_errant_simulation; break;
  case xeona::e_infeasibility:    returnStatus = xeona::ret_infeasibility;     break;
  case xeona::e_testCodeUsed:     returnStatus = contextBasedReturn;           break;
  case xeona::e_other:            returnStatus = xeona::ret_other;             break;
  case xeona::e_statusNotKnown:   returnStatus = xeona::ret_status_not_known;  break;
  default: std::clog << "** coding error 02 in source file " << __FILE__ << std::endl;
        } // 'switch' block

    } // 'try' block

  // ---------------------------------
  //  simulation call 'catch' blocks
  // ---------------------------------

  // CAUTION: 'stdout' and not 'stderr' used for direct reporting
  // as file descriptor 2 may well be redirected to '/dev/null'

  catch ( const xeona::kill_on_log& x )
    {
      logger->repx(logga::info, "xeona::kill_on_log caught", "");
      logger->flush();
      returnStatus = x.code();
    }
  catch ( const xeona::file_not_found& x )
    {
      logger->repx(logga::info, "xeona::file_not_found caught", "");
      logger->flush();
      returnStatus = x.code();
    }
  catch ( const xeona::invalid_interval& x )
    {
      logger->repx(logga::info, "xeona::invalid_interval caught", "");
      logger->flush();
      returnStatus = x.code();
    }
  catch ( const xeona::bad_assign_cast& a )  // inherits direct from 'std::exception'
    {
      logger->repx(logga::info, "xeona::bad_assign_cast caught", "");
      logger->flush();
      std::cout
        << "\n"
        << "** xeona::bad_assign_cast: " << a.what()                       << "\n"
        << "   for more information, rerun model with maximum reporting"   << "\n"
        << std::flush;
      logger->addSmartBlank();
      returnStatus = xeona::exit_bad_assign_cast;
    }
  catch ( const xeona::exception& x )
    {
      logger->repx(logga::info, "polymorphic xeona::exception caught", "");
      std::ostringstream put;
      put << x.expl();
      logger->putx(logga::info, put);
      logger->flush();
      std::cout
        << "\n"
        << "** " << x.tell() << " caught, execution abandoned"             << "\n"
        << std::flush;
      logger->addSmartBlank();
      returnStatus = x.code();
    }
  catch ( const boost::bad_weak_ptr& p )     // place before 'std::exception'
    {
      logger->repx(logga::info, "boost::bad_weak_ptr caught", "");
      logger->flush();
      std::cout
        << "\n"
        << "** boost::bad_weak_ptr: " << p.what()                          << "\n"
        << "   for more information, rerun model with maximum reporting"   << "\n"
        << std::flush;
      logger->addSmartBlank();
      returnStatus = xeona::exit_boost_exception;
    }
  catch ( const std::out_of_range& e )
    {
      logger->repx(logga::info, "std::out_of_range caught", "");
      logger->flush();
      std::cout
        << "\n"
        << "** std::out_of_range: " << e.what()                                  << "\n"
        << "   possible cause: inappropriate 'at' call by 'std::vector' object"  << "\n"
        << "   for more information, rerun model with maximum reporting"         << "\n"
        << std::flush;
      logger->addSmartBlank();
      returnStatus = xeona::exit_std_out_of_range;
    }
  catch ( const std::domain_error& e )
    {
      logger->repx(logga::info, "std::domain_error caught", "");
      logger->flush();
      std::cout
        << "\n"
        << "** std::domain_error: " << e.what()                                  << "\n"
        << "   for more information, rerun model with maximum reporting"         << "\n"
        << std::flush;
      logger->addSmartBlank();
      returnStatus = xeona::exit_std_domain_error;
    }
  catch ( const std::logic_error& e )
    {
      logger->repx(logga::info, "std::logic_error caught", "");
      logger->flush();
      std::cout
        << "\n"
        << "** std::logic_error: " << e.what()                                   << "\n"
        << "   for more information, rerun model with maximum reporting"         << "\n"
        << std::flush;
      logger->addSmartBlank();
      returnStatus = xeona::exit_std_logic_error;
    }
  catch ( const std::bad_alloc& m )
    {
      logger->repx(logga::info, "std::bad_alloc caught", "");
      logger->flush();
      std::cout
        << "\n"
        << "** std::bad_alloc: " << m.what()                                     << "\n"
        << "   cause: insufficient heap (mid-simulation) memory"                 << "\n"
        << "   for more information, rerun model using a memory profiler"        << "\n"
        << std::flush;
      logger->addSmartBlank();
      returnStatus = xeona::exit_std_bad_alloc;
    }
  catch ( const std::exception& e )
    {
      logger->repx(logga::info, "std::exception caught", "");
      logger->flush();
      std::cout
        << "\n"
        << "** std::exception: " << e.what()                               << "\n"
        << "   for more information, rerun model with maximum reporting"   << "\n"
        << std::flush;
      logger->addSmartBlank();
      returnStatus = xeona::exit_std_exception;
    }
  catch ( ... )
    {
      logger->repx(logga::warn, "unknown exception caught", "");  // note 'logga::warn'
      logger->flush();
      std::cout
        << "\n"
        << "** unknown (...) exception"                                    << "\n"
        << "   for more information, rerun model with maximum reporting"   << "\n"
        << std::flush;
      logger->addSmartBlank();
      returnStatus = xeona::exit_unknown_exception;
    }

  // =================================
  //  conclusion
  // =================================

  // integrity check

  if ( returnStatus == xeona::ret_not_overwritten )
    {
      logger->repx(logga::warn, "main function return not overwritten", returnStatus);
    }

  // ---------------------------------
  //  reprocess return if --exittrip
  // ---------------------------------

  // CAUTION: program execution may not get here due to 'return'
  // statements in the above code, however 'returnStatus' should
  // aways be overwritten -- that is also tested here

  // CAUTION: this code cannot consider logging from object
  // destructors -- indeed this problem is difficult because
  // objects are not destructed until 'main' returns, by which
  // point the return status has been set

  const logga::Rank highestRankLogCall = logger->getHighestRankAll();
  const unsigned highestRankLogCallInt = static_cast<unsigned>(highestRankLogCall);

  logger->repx(logga::adhc, "highest rank log call (int)",  highestRankLogCallInt);

  if ( returnStatus == xeona::ret_success )
    {
      if ( highestRankLogCallInt <= exitTripLevel )
        {
          returnStatus = highestRankLogCallInt;
          logger->repx(logga::adhc, "return status (from exittrip)", returnStatus);
        }
      else
        {
          logger->repx(logga::adhc, "return status (from earlier)", returnStatus);
        }
    }

  // ---------------------------------
  //  pass return status to logger
  // ---------------------------------

  // the reason for this call is that several screens-full of
  // output can follow the final report splash screen -- this
  // information reported when the Logger instance finally
  // destructs.

  if ( returnStatus == 0 )
    {
      logger->updateReturnStatus(returnStatus, "success");  // short form message
    }
  else
    {
      std::string interpretation;            // fill by reference
      ExitStatus exitstatus;                 // accept default not-present message
      exitstatus(returnStatus, interpretation);
      logger->updateReturnStatus(returnStatus, interpretation);
    }

  // ---------------------------------
  //  display final report
  // ---------------------------------

  std::clog << std::flush;
  std::cout << appInfo.showFinal(sret, returnStatus);

  std::cout << std::flush;
  std::clog << std::flush;

  // ---------------------------------
  //  exit with an appropriate code
  // ---------------------------------

  std::ostringstream put;
  put << Entity::reportFullPopulation();
  logger->repx(logga::dbug, "entity report follows", "");
  logger->putx(logga::dbug, put);

  logger->repx(logga::dbug, "end of main", "~~~~~~~~~~");
  return returnStatus;

} // main

//  end of file

