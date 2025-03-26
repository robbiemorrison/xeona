//  XEONA ENERGY SYSTEMS MODELING ENVIRONMENT
//
//  file-create-name : logger.cc
//  file-create-date : Thu 05-Apr-2007 11:34 UTC
//  file-initiator   : Robbie Morrison <robbie@actrix.co.nz>
//
//  file-role        : run-time logging functionality / implementation
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
//  $URL: file:///home/robbie/svn-root/xeona/futz/trunk/xeona1/a/logger.cc $
//  LOCAL AND SYSTEM INCLUDES

#include "logger.h"           // companion header for this file (place first)

#include "../c/smart_ptr.h"   // switch easily between TR1 and Boost smart pointers
#include ".././common.h"      // common definitions for project (place last)

#include <fstream>            // file-based io (in case of logging to file)
#include <iomanip>            // setw() and family
#include <iostream>           // standard io
#include <map>                // STL associative container
#include <sstream>            // string-streams
#include <string>             // C++ strings

#include <unistd.h>           // sleep(), usleep()

// CAUTION: this unit does not require the -lboost_date_time linking
// directive in the makefile (but some uses of the following header do)

#include <boost/date_time/posix_time/posix_time.hpp>   // plus io
#include <boost/algorithm/string.hpp>        // string recasing, trimming, splitting
#include <boost/algorithm/string_regex.hpp>  // additional regex support
#include <boost/format.hpp>             // printf style formatting
#include <boost/lexical_cast.hpp>       // lexical_cast<> string to number conversions

// PREPROCESSOR MACROS

#if !defined(XE_OUTPUT)
# define XE_OUTPUT 2 // 1 = log to file, 2 = stdlog (std::clog), 3 = stderr (std::cerr)
#endif

// ---------------------------------------------------------
//  notes           : logger concept
// ---------------------------------------------------------
//
//  Free function returning reference to logger object
//
//  Design notes
//
//      The design concept draws on Meyers (1998, pp 219-223)
//      and, in particular, item 47 entitled "Ensure that
//      non-local static objects are initialized before they are
//      used".
//
//      The design goal is to give any number of class
//      constructors -- some of which are instantiated before the
//      main function -- and the main function itself access to
//      universal logging.  Ideally then, only one Logger object
//      should be created and it should be properly initialized
//      before first use.  Moreover, it is not possible to
//      determine or mandate the order in which calls to such an
//      object might take place.
//
//      The solution is to create a free function in the logga
//      namespace containing a static instance of the Logger
//      class.  This function returns a reference to a
//      necessarily unique Logger instance -- with the first such
//      call giving rise to the correct initialization of the
//      Logger instance.  Cout reporting shows that the Logger
//      constructor and destructor are called only once -- as
//      required.
//
//  CAUTION
//
//      A full (as opposed to forward) class declaration for Logger
//      MUST PRECEDE the free function declaration/definition for
//      refLogStream().
//
//  Usage
//
//      0. Note the typedef within logger.h:
//
//           typdef shared_ptr<Logger> pLogger
//
//      1. Within free and member functions as automatic variable:
//
//         {
//           logga::pLogger logger = logga::ptrLogStream();
//           logger->repx(logga::info, 1, "from function");
//         }
//
//      2. Within classes proper as per object instances:
//
//          class SomeClass
//          {
//          public:
//            SomeClass() :
//              d_logger(logga::ptrLogStream())
//            {
//              d_logger->repx(logga::info, 101, "per object approach");
//            }
//          private:
//            logga::pLogger d_logger;
//          };
//
//      3. Within classes proper as a per class instance:
//
//          class AnotherClass
//          {
//          public:
//            AnotherClass()
//            {
//              s_logger->repx(logga::info, 101, per class approach");
//            }
//          private:
//            static logga:pLogger s_logger;
//          };
//
//          logga::pLogger
//          AnotherClass::s_logger = logga::ptrLogStream();
//
//      See the unit test file for more examples of usage.
//
//  --------------------------------------------------------
//   Additional information regarding compiler macros
//  --------------------------------------------------------
//
//  Design notes
//
//      The member function repx() is hash-define overwritten to
//      additionally report the compiler macros __FILE__, __LINE__,
//      and__func__.
//
//  Control
//
//      This feature can be disabled by modifying the hash-if
//      directive at the bottom of this file.
//
//  --------------------------------------------------------
//   Use of function templates and link-time errors
//  --------------------------------------------------------
//
//  Background
//
//      For a quick synopsis of function templates (aka template
//      functions), see Loudon (2003, pp 101-102).  For a more
//      detailed coverage, consult Lischner (2003, pp 180-184).
//
//  Link-time errors
//
//      It was found that link-time errors would occur when the
//      three-box approach was adopted (using .h and .cc files),
//      for instance:
//
//        "undefined reference to `bool
//         logga::Logger::repx<int>(logga::Rank, int, int)'"
//
//      Oualline (1995, p445) suggests, instead, to place
//      function template definitions in the header file and to
//      also inline them (noting that the inline keyword is a
//      compiler hint only).
//
//      The Oualline approach worked here.  Strangely, C-style
//      strings and string literals were okay with the three-box
//      model, but ints, doubles, and std::strings failed to link.
//
//  --------------------------------------------------------
//   References
//  --------------------------------------------------------
//
//      Loudon, Kyle.  2003.  C++ pocket reference.  O'Reilly and
//        Associates, Sebastopol, USA.  ISBN 0-596-00496-6.
//
//      Meyers, Scott.  1998.  Effective C++ : 50 specific ways to
//        improve your programs and design --- Second edition.
//        Addison-Wesley, Boston, USA.  ISBN 0-201-92488-9.
//
//      Oualline, Steve.  1995.  Practical C++ programming -- First
//        edition.  O'Reilly and Associates, Sebastopol, USA.
//        ISBN 1-56592-139-9.
//
//  --------------------------------------------------------

//  CODE

namespace logga
{
  // ---------------------------------------------------------
  //  VARIABLE        : logga::rankNoData
  //  VARIABLE        : logga::rankJumpy
  // ---------------------------------------------------------

  Rank rankNoData = dbug;
  Rank rankJumpy  = dbug;

  // ---------------------------------------------------------
  //  CLASS           : logga::Logger
  // ---------------------------------------------------------

  //  STATIC DEFINITIONS - with explicit initialization

  bool Logger::s_truncate = true;            // console truncation flag

  // CREATORS

  // ---------------------------------------------------------
  //  MEMBER FUNCTION : Logger::Logger
  // ---------------------------------------------------------

  Logger::Logger
  (std::ostream& os) :
    d_os(os),
    d_was(boost::posix_time::microsec_clock::universal_time()),
    d_lastCall(e_none),
    d_repCount(0),
    d_reportLevel(static_cast<Rank>(xeona::reportLevelOpening)),
    d_enableBeep(false),
    d_beepCompletion(false),
    d_beepThreshold(logga::yeek),            // and beeping on yeek is disabled
    d_triggerStr(),                          // empty string
    d_triggerRegex(),                        // zero argument construction
    d_finalBlank(false),

    d_returnStatus(-1),                      // nonsensical value
    d_returnInterp(),                        // empty string

    d_yeekNoFirst(0),
    d_killNoFirst(0),
    d_warnNoFirst(0),

    d_yeekAllCount(0),
    d_killAllCount(0),
    d_warnAllCount(0),
    d_infoAllCount(0),
    d_dbugAllCount(0),
    d_xtraAllCount(0),
    d_adhcAllCount(0),

    d_yeekFixedCount(0),
    d_killFixedCount(0),
    d_warnFixedCount(0),
    d_infoFixedCount(0),
    d_dbugFixedCount(0),
    d_xtraFixedCount(0),
    d_adhcFixedCount(0),

    d_yeekResetCount(0),
    d_killResetCount(0),
    d_warnResetCount(0),
    d_infoResetCount(0),
    d_dbugResetCount(0),
    d_xtraResetCount(0),
    d_adhcResetCount(0)
  {
    this->repx(logga::dbug, "constructor call", "using this->repx");
#if 0 // 0 = ignore, 1 = test "delta" timings using a 200ms delay
    this->repx(logga::dbug, "delay set", "200ms");
    usleep(200000);                          // <unistd.h>
#endif // 0
    this->repx(logga::dbug, "timestamp", timestampUTC());
  }

  // ---------------------------------------------------------
  //  MEMBER FUNCTION : Logger::~Logger
  // ---------------------------------------------------------

  Logger::~Logger()
  {
    this->repx(logga::dbug, "destructor call", "using this->repx");

    // print final rule

    if ( d_repCount > 0 )
      {
        printRule(0);                        // zero blank lines to follow
        this->addSmartBlank();
      }

    // display log call print tally if -- at various report
    // levels (noting that 'yeek' is officially silent) -- any
    // log messages were written out OR kill calls were made OR
    // warn calls were made -- note too this logging policy can
    // be readily changed to suit circumstances

    if ( ( d_repCount     > 0 && d_reportLevel >= info )    // logs were printed
         ||
         ( d_killAllCount > 0 && d_reportLevel >= yeek )    // kills were called
         ||
         ( d_warnAllCount > 0 && d_reportLevel >= kill ) )  // warns were called
      {
        std::ostringstream put;
        put << "    Logger destructor call data"     << "\n";
        put << ""                                    << "\n";

        put << summarizeRankCounts();        // normal trailing newline not required
        this->putx(logga::yeek, put);        // always print if here
        this->addSmartBlank();               // set subsequent logging behavior
      }

    // display application return status, if known

    if ( d_returnStatus >= 0 )               // that is, overwritten construction value
      {
        std::ostringstream put;
        put << "    application exit : " << d_returnStatus
            << " = " << d_returnInterp           << "\n";
        this->putx(logga::yeek, put);        // always print if here
        this->addSmartBlank();               // set subsequent logging behavior
      }

    // this next statement couples with 'std::cout' calls in
    // class 'AppInfo' (the logger stream itself is set in the
    // free function 'logga::ptrLogStream' and need not be
    // 'std::clog') -- the approach is not especially clean but
    // is acceptable, given that '~Logger' is the last hand-coded
    // destructor called

    if ( d_finalBlank )
      {
        d_os << std::flush;
        std::cout << "\n";         // add newline to stdout, even if no logs
        std::cout << std::flush;
      }
    else if ( d_repCount > 0 )
      {
        d_os << "\n";              // add newline to stream, if not above and if logs
        d_os << std::flush;
      }

    if ( d_beepCompletion )
      this->beep(1);               // final beep, comes after 'main' returns

  } // ~Logger

  // LOGGING FUNCTIONS - most are defined in "logger.h" to protect from the CPP macro

  // ---------------------------------------------------------
  //  MEMBER FUNCTION : Logger::test (overloaded)
  // ---------------------------------------------------------

  bool
  Logger::test                               // insert a TEST COUNT COMMENCING line
  (const int testCount)
  {
    d_os << "\n";
    if ( testCount == 0 )                    // special case of final line
      d_os << "   TESTS COMPLETE" << "\n";
    else
      d_os << "   TEST " << testCount << " COMMENCING" << "\n";
    d_os << "\n";
    d_lastCall = e_none;                     // no special treatment from next call
    return true;
  }

  bool
  Logger::test
  (const int         testCount,
   const std::string putMsg)                 // 'put'-type message without "\n"
  {
    test(testCount);
    d_os << "   ++ " << putMsg << "\n";      // note leading "   ++ "
    d_lastCall = e_addblank;
    return true;
  }

  // ---------------------------------------------------------
  //  MEMBER FUNCTION : Logger::addSmartBlank (overloaded)
  // ---------------------------------------------------------

  void
  Logger::addSmartBlank()                    // add blank line in a relatively smart way
  {
    d_lastCall = e_addblank;                 // simply modify the last call status
  }

  void
  Logger::addSmartBlank                      // ditto, but only if rank-appropriate
  (const logga::Rank rank)
  {
    if ( rank > d_reportLevel )              // check verbosity
      {
        return;
      }
    addSmartBlank();                         // action
  }

  // ---------------------------------------------------------
  //  MEMBER FUNCTION : Logger::addDumbBlank (overloaded)
  // ---------------------------------------------------------

  void
  Logger::addDumbBlank()
  {
    d_os << "\n";                            // call ensures consistent ostream usage
  }

  void
  Logger::addDumbBlank                       // ditto, but only if rank-appropriate
  (const logga::Rank rank)
  {
    if ( rank > d_reportLevel )              // check verbosity
      {
        return;
      }
    addDumbBlank();                          // action
  }

  // ---------------------------------------------------------
  //  MEMBER FUNCTION : Logger::addFinalStdoutBlank
  // ---------------------------------------------------------

  void
  Logger::addFinalStdoutBlank()
  {
    d_finalBlank = true;
  }

  // ---------------------------------------------------------
  //  MEMBER FUNCTION : Logger::flush
  // ---------------------------------------------------------

  void
  Logger::flush()                            // explicitly flush the output buffer
  {
    d_os << std::flush;
  }

  // ACCESSORS

  // ---------------------------------------------------------
  //  MEMBER FUNCTION : getHighestRankAll
  // ---------------------------------------------------------

  logga::Rank
  Logger::getHighestRankAll() const
  {
    if      ( d_yeekAllCount > 0 ) return logga::yeek;
    else if ( d_killAllCount > 0 ) return logga::kill;
    else if ( d_warnAllCount > 0 ) return logga::warn;
    else if ( d_infoAllCount > 0 ) return logga::info;
    else if ( d_dbugAllCount > 0 ) return logga::dbug;
    else if ( d_xtraAllCount > 0 ) return logga::xtra;
    else if ( d_adhcAllCount > 0 ) return logga::adhc;
    else                           return logga::none;
  }

  // ---------------------------------------------------------
  //  MEMBER FUNCTION : Logger::getReportLevelRank
  // ---------------------------------------------------------

  logga::Rank
  Logger::getReportLevelRank() const         // return an enum
  {
    return d_reportLevel;
  }

  // ---------------------------------------------------------
  //  MEMBER FUNCTION : Logger::getReportLevelInt
  // ---------------------------------------------------------

  unsigned
  Logger::getReportLevelInt() const          // return an integer
  {
    return static_cast<unsigned>(d_reportLevel);
  }

  // ---------------------------------------------------------
  //  MEMBER FUNCTION : Logger::getReportLevelStr
  // ---------------------------------------------------------

  std::string                                // used by 'AppInfo' object
  Logger::getReportLevelStr() const          // return an interpretation
  {
    std::string interpretation = "(not overwritten)";
    switch( d_reportLevel )
      {
      case yeek: interpretation = "no output";    break;
      case kill: interpretation = "kills";        break;
      case warn: interpretation = "warnings";     break;
      case info: interpretation = "information";  break;
      case dbug: interpretation = "debug";        break;
      case xtra: interpretation = "extra";        break;
      case adhc: interpretation = "all";          break;
      default: std::clog << "** coding error 01 in source file " << __FILE__ << std::endl;
        // CAUTION: do not place 'repx' call here, else 'const' problems
      }
    return interpretation;
  }

  // ---------------------------------------------------------
  //  MEMBER FUNCTION : Logger::getAllTriggerStr
  // ---------------------------------------------------------

  const std::string
  Logger::getAllTriggerStr() const
  {
    std::string buf;

    buf += formatTriggerStr(yeek, d_yeekAllCount);
    buf += formatTriggerStr(kill, d_killAllCount);
    buf += formatTriggerStr(warn, d_warnAllCount);
    buf += formatTriggerStr(info, d_infoAllCount);
    buf += formatTriggerStr(dbug, d_dbugAllCount);
    buf += formatTriggerStr(xtra, d_xtraAllCount);
    buf += formatTriggerStr(adhc, d_adhcAllCount);

    if ( ! buf.empty() )                   // nonzero length
      buf = buf.substr(1);                 // trim leading char, in this case, a space
    else
      buf = "(no log calls)";

    return buf;
  }

  // ---------------------------------------------------------
  //  MEMBER FUNCTION : Logger::summarizeRankCounts
  // ---------------------------------------------------------

  const std::string                          // has an uncharacteristic trailing newline
  Logger::summarizeRankCounts() const
  {
    unsigned sumAllCount
      = d_yeekAllCount
      + d_killAllCount
      + d_warnAllCount
      + d_infoAllCount
      + d_dbugAllCount
      + d_xtraAllCount
      + d_adhcAllCount;
    unsigned sumFixedCount
      = d_yeekFixedCount
      + d_killFixedCount
      + d_warnFixedCount
      + d_infoFixedCount
      + d_dbugFixedCount
      + d_xtraFixedCount
      + d_adhcFixedCount;
    unsigned sumResetCount
      = d_yeekResetCount
      + d_killResetCount
      + d_warnResetCount
      + d_infoResetCount
      + d_dbugResetCount
      + d_xtraResetCount
      + d_adhcResetCount;

    std::ostringstream oss;
#if 0 // 0 = support one billion log calls, 1 = support one million log calls (two places)
    oss << "    repx()      call        print           reset" << "\n";
    oss << "    ---------------------------------------------" << "\n";
#else
    oss << "    repx()          call           print                reset" << "\n";
    oss << "    ---------------------------------------------------------" << "\n";
#endif // 0

    if ( d_yeekFixedCount > 0 )              // conditional reporting
      {
 formatRankCount(oss, calcRank(yeek), d_yeekAllCount, d_yeekFixedCount, d_yeekResetCount);
      }
 formatRankCount(oss, calcRank(kill), d_killAllCount, d_killFixedCount, d_killResetCount);
 formatRankCount(oss, calcRank(warn), d_warnAllCount, d_warnFixedCount, d_warnResetCount);
 formatRankCount(oss, calcRank(info), d_infoAllCount, d_infoFixedCount, d_infoResetCount);
 formatRankCount(oss, calcRank(dbug), d_dbugAllCount, d_dbugFixedCount, d_dbugResetCount);
 formatRankCount(oss, calcRank(xtra), d_xtraAllCount, d_xtraFixedCount, d_xtraResetCount);
 formatRankCount(oss, calcRank(adhc), d_adhcAllCount, d_adhcFixedCount, d_adhcResetCount);
 formatRankCount(oss, "",             sumAllCount,    sumFixedCount,    sumResetCount);

    return oss.str();
  }

  // ---------------------------------------------------------
  //  MEMBER FUNCTION : Logger::getTrigger
  // ---------------------------------------------------------

  const std::string
  Logger::getTrigger() const
  {
    return d_triggerStr;
  }

  // ---------------------------------------------------------
  //  MEMBER FUNCTION : Logger::jumpyIsWarn
  // ---------------------------------------------------------

  bool                                       // 'true' if '--jumpy'
  Logger::jumpyIsWarn() const
  {
    return ( logga::rankJumpy == logga::warn );
  }

  // STATIC MANIPULATORS

  // ---------------------------------------------------------
  //  MEMBER FUNCTION : Logger::enableConsoleTruncation
  // ---------------------------------------------------------

  bool
  Logger::enableConsoleTruncation()
  {
    s_truncate = true;
    return s_truncate;
  }

  // ---------------------------------------------------------
  //  MEMBER FUNCTION : Logger::disableConsoleTruncation
  // ---------------------------------------------------------

  bool
  Logger::disableConsoleTruncation()
  {
    s_truncate = false;
    return s_truncate;
  }

  // MANIPULATORS

  // ---------------------------------------------------------
  //  MEMBER FUNCTION : Logger::enableBeeping
  // ---------------------------------------------------------

  bool
  Logger::enableBeeping()
  {
    d_enableBeep = true;
    return d_enableBeep;
  }

  // ---------------------------------------------------------
  //  MEMBER FUNCTION : Logger::disableBeeping
  // ---------------------------------------------------------

  bool
  Logger::disableBeeping()
  {
    d_enableBeep = false;
    return d_enableBeep;
  }

  // ---------------------------------------------------------
  //  MEMBER FUNCTION : Logger::setBeepOnCompletion
  // ---------------------------------------------------------

  void
  Logger::setBeepOnCompletion()
  {
    d_beepCompletion = true;
  }

  // ---------------------------------------------------------
  //  MEMBER FUNCTION : Logger::setBeepOnOrAbove
  // ---------------------------------------------------------

  void
  Logger::setBeepOnOrAbove
  (logga::Rank rank)
  {
    d_beepThreshold = rank;
  }

  // ---------------------------------------------------------
  //  MEMBER FUNCTION : Logger::setTrigger
  // ---------------------------------------------------------

  void
  Logger::setTrigger
  (const std::string& trigger)
  {
    d_triggerStr = trigger;
    boost::regex temp(d_triggerStr);
    d_triggerRegex = temp;

    // CAUTION: place logging statements after the trigger
    // setting process is complete -- otherwise a
    // "/usr/local/include/boost-1_34_1/boost/regex/v4/basic_regex.hpp"
    // assertion may fail on '0 != m_pimpl.get()' and the
    // application is duly "aborted" and core dumped!

    this->repx(logga::dbug, "trigger string set", d_triggerStr);
    this->repx(logga::dbug, "trigger regex set", d_triggerRegex);  // streamable
  }

  // ---------------------------------------------------------
  //  MEMBER FUNCTION : Logger::resetRankCounts
  // ---------------------------------------------------------

  void
  Logger::resetRankCounts                    // at given level and noisier
  (const unsigned reportLevel)               // default setting given in header
  {
    Rank level = static_cast<logga::Rank>(reportLevel);
    switch ( level )
      {
      case yeek: d_yeekResetCount = 0;       // CAUTION: no 'break' statement is correct
      case kill: d_killResetCount = 0;
      case warn: d_warnResetCount = 0;
      case info: d_infoResetCount = 0;
      case dbug: d_dbugResetCount = 0;
      case xtra: d_xtraResetCount = 0;
      case adhc: d_adhcResetCount = 0; break;
      default: std::clog << "** coding error 02 in source file " << __FILE__ << std::endl;
      }
  }

  // ---------------------------------------------------------
  //  MEMBER FUNCTION : Logger::setReportLevel
  // ---------------------------------------------------------

  Rank                                       // return previous report level
  Logger::setReportLevel
  (const unsigned reportLevel)
  {
    // note that out-of-range casting is okay
    Rank newReportLevel = static_cast<Rank>(reportLevel);

    // range check sought level
    if ( newReportLevel < logga::yeek || newReportLevel > logga::adhc )     // not [0,6]
      {
        // may also produce compiler warning: character constant too long for its type
        this->repx(logga::warn, "out-of-range argument, check caller", newReportLevel);
      }

    // capture current (but soon to be previous) value
    Rank oldReportLevel = d_reportLevel;

    // reset and report
    std::ostringstream oss;                  // for reporting
    oss << d_reportLevel << " > "  << newReportLevel;

    if ( newReportLevel > oldReportLevel )   // noisier, 'repx' after resetting
      {
        d_reportLevel = newReportLevel;
        this->repx(logga::dbug, "report level reset from > to", oss.str());
      }
    else                                     // same or quieter, 'repx' before resetting
      {
        this->repx(logga::dbug, "report level reset from > to", oss.str());
        d_reportLevel = newReportLevel;
      }

    resetRankCounts(newReportLevel);         // CAUTION: note call to reset triggers
    return oldReportLevel;
  }

  // UTILITY FUNCTIONS

  // ---------------------------------------------------------
  //  MEMBER FUNCTION : Logger::shouldLog
  // ---------------------------------------------------------

  bool                                       // 'true' means "should log"
  Logger::shouldLog
  (const std::string& file)
  {
    if ( d_triggerStr.empty() )              // no need to continue
      return false;

    if ( boost::find_regex(file, d_triggerRegex) )
      return true;
    else
      return false;
  }

  // ---------------------------------------------------------
  //  MEMBER FUNCTION : Logger::firstCall
  // ---------------------------------------------------------

  void
  Logger::firstCall()
  {
    std::cout << std::flush;                 // ensure synchronization
    d_os << "\n";                            // initial opening line
    printHeader();
    printRule(0);                            // zero blank lines to follow
  }

  // CAUTION: relatively trivial bug: 'printHeader' does not
  // respond if the preprocessor macros are "switched off" using
  // the 'hash-if' at the end of "logger.h"

  // ---------------------------------------------------------
  //  MEMBER FUNCTION : Logger::printHeader
  // ---------------------------------------------------------

  void
  Logger::printHeader()
  {
    std::ostringstream ssAt;                 // for line, file, and func information
    std::string        strAt;                // string version of above
    int                tab;                  // setw allowance in main stringstream

    ssAt << ""
         << std::setw(s_TAB1 + s_MAR) << std::right << "line"
         << std::setw(         s_GUT) << ""
         << std::setw(s_TAB2 + s_OVR) << std::left  << "source"
         << std::setw(s_TAB3 + s_GUT) << std::left  << "call";

    strAt = ssAt.str();
    tab   = s_TAB1 + s_MAR + s_GUT + s_TAB2 + s_TAB3 + s_GUT;
    boost::trim_right(strAt);                // trim trailing spaces in-situ

    d_os << std::setw(tab)            << std::left  << strAt
         << std::setw(s_TAB4 + s_OVR) << std::right << "no"
         << std::setw(         s_GUT) << ""
         << std::setw(s_TAB5 + s_GUT) << std::left  << "delta-t"
         << std::setw(s_TAB6 + s_GUT) << std::left  << "rank"
         << std::setw(s_TAB7 + s_OVR) << std::left  << "message"
         <<                              std::left  << "comment/value"
         << "\n";

    d_os << std::left;                       // reset to default
    d_repCount = 1;                          // originally initialized to zero
    d_lastCall = e_none;                     // ensure no gap is left
  }

  // ---------------------------------------------------------
  //  MEMBER FUNCTION : Logger::printRule
  // ---------------------------------------------------------

  void
  Logger::printRule
  (const int skip)
  {
    // add blank line as needed
    if ( d_lastCall == e_putx )
      d_os << "\n";                          // insert newline
    if ( d_lastCall == e_addblank )
      d_os << "\n";

    // print rule
    d_os << std::string(s_RULE, '.')         // line of dots
         << std::string(skip, '\n')          // series of newlines (usually zero or one)
         << "\n";
    d_lastCall = e_none;                     // ensure no additional gap is left
  }

  // ---------------------------------------------------------
  //  MEMBER FUNCTION : Logger::incrementRankCounts
  // ---------------------------------------------------------

  void
  Logger::incrementRankCounts
  (unsigned rank)                            // will also accept a logga::Rank
  {
    Rank reportLevel = static_cast<Rank>(rank);

    switch ( reportLevel )
      {
      case yeek: ++d_yeekAllCount; break;
      case kill: ++d_killAllCount; break;
      case warn: ++d_warnAllCount; break;
      case info: ++d_infoAllCount; break;
      case dbug: ++d_dbugAllCount; break;
      case xtra: ++d_xtraAllCount; break;
      case adhc: ++d_adhcAllCount; break;
      default: std::clog << "** coding error 03 in source file " << __FILE__ << std::endl;
      }

    // count only printed to console calls
    if ( reportLevel > d_reportLevel ) return;

    switch ( reportLevel )
      {
      case yeek: ++d_yeekFixedCount; ++d_yeekResetCount; break;
      case kill: ++d_killFixedCount; ++d_killResetCount; break;
      case warn: ++d_warnFixedCount; ++d_warnResetCount; break;
      case info: ++d_infoFixedCount; ++d_infoResetCount; break;
      case dbug: ++d_dbugFixedCount; ++d_dbugResetCount; break;
      case xtra: ++d_xtraFixedCount; ++d_xtraResetCount; break;
      case adhc: ++d_adhcFixedCount; ++d_adhcResetCount; break;
      default: std::clog << "** coding error 04 in source file " << __FILE__ << std::endl;
      }

    // update no (number) of first call

    if ( reportLevel == yeek && d_yeekNoFirst == 0 )
      d_yeekNoFirst = d_repCount;
    if ( reportLevel == kill && d_killNoFirst == 0 )
      d_killNoFirst = d_repCount;
    if ( reportLevel == warn && d_warnNoFirst == 0 )
      d_warnNoFirst = d_repCount;
  }

  // ---------------------------------------------------------
  //  MEMBER FUNCTION : Logger::formatTriggerStr
  // ---------------------------------------------------------

  std::string
  Logger::formatTriggerStr
  (logga::Rank rank,
   unsigned    count) const
  {
    if ( count == 0 )
      return "";

    // boost::to_lower(term);
    return boost::str(boost::format(" %s (%d)") % calcRank(rank) % count);
  }

  // ---------------------------------------------------------
  //  MEMBER FUNCTION : Logger::formatRankCount
  // ---------------------------------------------------------

  void
  Logger::formatRankCount                    // used by 'summarizeRankCounts'
  (std::ostringstream& os,
   const std::string&  term,
   unsigned            call,                 // logger called count
   unsigned            print,                // logger printed count
   unsigned            reset) const          // logger printed count after reset
  {
    std::string addn;                        // highlight string for print
    if ( term == calcRank(yeek) && print > 0 )
      addn = boost::str(boost::format("< %d") % d_yeekNoFirst);
    if ( term == calcRank(kill) && print > 0 )
      addn = boost::str(boost::format("< %d") % d_killNoFirst);
    if ( term == calcRank(warn) && print > 0 )
      addn = boost::str(boost::format("< %d") % d_warnNoFirst);
    // boost::to_lower(term);                // downcase alpha characters
    os << "    ";                            // left indent
    os << boost::format("%-4s     ") % term; // print tag
#if 0 // 0 = support one billion log calls, 1 = support one million log calls (2 places)
    os << boost::format("%6d       %6d  %-7s %6d") % call % print % addn % reset;
#else
    os << boost::format("%10d       %9d  %-10s %9d") % call % print % addn % reset;
#endif // 0
    os << "\n";                              // add trailing newline
  }

  // ---------------------------------------------------------
  //  MEMBER FUNCTION : Logger::endOfLogCall
  // ---------------------------------------------------------

  void
  Logger::endOfLogCall                       // only reacts to first kill
  (const logga::Rank rank)
    throw(std::exception,                    // exception specification
          xeona::kill_on_log)
  {
    // static member
    static bool firstKill = true;            // assignment at initialization only

    // beep behavior
    if ( rank <= d_beepThreshold && rank != logga::yeek )
      this->beep(2);

    // kill behavior
    if ( rank != kill ) return;              // not a kill
    if ( firstKill == false ) return;        // subsequent calls do nothing
    firstKill = false;                       // update status

    // exit on kill if not '--krazy'

    if ( xeona::nopro == true )
      {
        this->repx(logga::info, "skipping xeona::kill_on_log throw", "");
      }
    else
      {
        this->repx(logga::warn, "will throw xeona::kill_on_log", "");

        // CAUTION: this use of throw could be considered an
        // abuse of the exception handling system.  That said, it
        // does shut the application down in a succinct and clean
        // manner.

        throw xeona::kill_on_log();
      }
  }

  // ---------------------------------------------------------
  //  MEMBER FUNCTION : Logger::numToPad
  // ---------------------------------------------------------

  std::string
  Logger::numToPad  // could be better implemented using Boost.Format library
  (double input,
   int    LEFT,     // zero-padded digits (default 2) left of decimal point
   int    RIGHT)    // digits (default 4) right of decimal point
  {
    //  Purpose     : convert doubles to padded truncated strings
    //  Note        : default values for LEFT and RIGHT provided in logger.h
    //  Used by     : calcInterval()
    //
    //          0         ->  00.0000
    //          0.0       ->  00.0000
    //          5.003699  ->  05.0037
    //         -0.003699  ->  -0.0037

    std::ostringstream ossBuf;               // formatting buffer
    ossBuf << std::setw(LEFT + RIGHT + 1)    // define overall width, including point
           << std::setfill('0')              // define zero pad character
           << std::fixed                     // adopt fixed formatting
           << std::setprecision(RIGHT)       // define decimal place count
           << input;
    return ossBuf.str();
  }

  // ---------------------------------------------------------
  //  MEMBER FUNCTION : Logger::calcInterval
  // ---------------------------------------------------------

  std::string
  Logger::calcInterval()
  {
    //  Purpose     : return a string in the following formats
    //  Note        : left and right padding can be adjusted here
    //
    //          00.0000s
    //          00.0000m
    //          00.0000h

    std::string interval;                    // return value in one of several formats
    double      input;                       // input for numToPad() function

    // calculate the interval since the last call as a simple Boost time_duration
    // object -- whilst noting that there is no need to use the more complex Boost
    // time_period object that contains end point information thus [start, end)

    boost::posix_time::ptime now;            // current point in time
    boost::posix_time::ptime was;            // point in time of previous call
    boost::posix_time::time_duration dur;    // difference between the above

    now   = boost::posix_time::microsec_clock::universal_time();
    was   = d_was;                           // retrieve value of 'was'
    d_was = now;                             // reset stored 'was' to 'now'
    dur   = now - was;                       // calculate a simple duration

    long hours             = dur.hours();              // normalized hours
    long totalMicroSeconds = dur.total_microseconds(); // total microseconds

    // process string representation of this interval based on interval length

    if ( totalMicroSeconds < 0 )                       // negative, an error
      {
        this->repx(logga::dbug, "negative interval calculated", totalMicroSeconds);
        return "";
      }
    else if ( totalMicroSeconds < 1e6 * 60 )           // sub minute
      {
        input    = static_cast<double>(totalMicroSeconds)/(1e6);
        interval = numToPad(input) + "s";
        return interval;
      }
    else if ( totalMicroSeconds < 1e6 * 60 * 60 )      // sub hour
      {
        input    = static_cast<double>(totalMicroSeconds)/(1e6 * 60);
        interval = numToPad(input) + "m";
        return interval;
      }
    else if ( totalMicroSeconds < 1e6 * 60 * 60 * 100 ) // sub one hundred hours
      {
        input    = static_cast<double>(totalMicroSeconds)/(1e6 * 60 * 60);
        interval = numToPad(input) + "h";
        return interval;
      }
    else                                               // more than one hundred hours
      {
        this->repx(logga::info, "interval exceeds 100 hours", hours);
        return "";
      }
  }

  // ---------------------------------------------------------
  //  MEMBER FUNCTION : Logger::calcRank
  // ---------------------------------------------------------

  std::string
  Logger::calcRank
  (const logga::Rank rank) const
  {
    switch ( rank )
      {
      case yeek: return "YEEK";              // for use during development
      case kill: return "KILL";
      case warn: return "WARN";
      case info: return "info";
      case dbug: return "dbug";
      case xtra: return "xtra";
      case adhc: return "adhc";
      default:   return "????";              // should never be here
        std::clog << "** coding error 05 in source file " << __FILE__ << std::endl;
      }
  }

  // ---------------------------------------------------------
  //  MEMBER FUNCTION : Logger::timestampUTC
  // ---------------------------------------------------------
  //  Description  : returns truncated timestamp
  //  Role         : reporting
  //  Techniques   : Boost.Date_time library
  //  Status       : complete
  //
  //  Design notes
  //
  //      The various Boost time objects can be injected into
  //      stringstreams or processed as strings -- with the
  //      latter approach selected here.
  //
  //      The microsec_clock yields 00:15:20.502428 which is then
  //      simply truncated to 00:15:20.50
  //
  //  Boost library resolution and timezone options
  //
  //        second_clock, microsec_clock
  //        local_time, universal_time
  //
  // ---------------------------------------------------------

  std::string
  Logger::timestampUTC()
  {
    boost::posix_time::ptime now;              // current point in time
    boost::posix_time::time_duration time;     // the non-day-date part
    std::string timestamp;                     // return string

    now       = boost::posix_time::microsec_clock::universal_time();
    time      = now.time_of_day();
    timestamp = to_simple_string(time);        // stringify
    timestamp.resize(11);                      // simple truncation to 0.00 seconds
    return timestamp;
  }

  // ---------------------------------------------------------
  //  MEMBER FUNCTION : Logger::resetOSS
  // ---------------------------------------------------------
  //  Description  : restore default ostringstream manipulators
  //  Role         : anytime manipulators have been hand set
  //  Techniques   : low-level calls (unfortunately)
  //  Status       : complete and tested for std::boolalpha
  //
  //  Design notes -- considered but not used
  //
  //      Note 'basic_ios' member function 'init' -- which I could
  //      not get to work (but did not spend much time either).
  //
  //      Note 'copyfmt(stream)' described in Josuttis (1999 p615)
  //      -- which I have not used.
  //
  //  References
  //
  //      Josuttis, Nicolai M.  1999.  The C++ Standard Library :
  //        a tutorial and reference.  Addison-Wesley, Boston, USA.
  //        ISBN // 0-201-37926-0.
  //
  //      Lischner, Ray.  2003.  C++ in a nutshell : a language and
  //        library reference.  O'Reilly and Associates, Sebastopol,
  //        California, USA.  ISBN 0-596-00298-X.
  //
  // ---------------------------------------------------------

  bool
  Logger::resetOSS
  (std::ostringstream& oss)                  // user-supplied string-stream to be reset
  {
    if ( ! oss.good() )                      // defensive programming
      {
        this->repx(logga::warn, "stringstream read state not good", "");
        return false;
      }
    else
      {
        this->repx(logga::xtra, "stringstream read state good", "");
      }

    // Lischner (2003 p205) lists the following io stream defaults:

    oss.exceptions(std::ios::goodbit);
    oss.fill(' ');
    oss.flags(std::ios::skipws|std::ios::dec); // see below
    oss.precision(6);
    oss.width(0);

    // Lischner (2003 p516) describes the set of io stream flags:
    //
    //     alignment      : left, internal, right
    //     base           : dec, hex, oct
    //     float format   : fixed, scientific
    //     other format   : boolalpha, showbase, showpoint, showpos, uppercase
    //     input          : skipws (skip whitespace before input)
    //     flush behavior : unitbuf (flush after each operation)

    return true;
  }

  // ---------------------------------------------------------
  //  MEMBER FUNCTION : updateReturnStatus
  // ---------------------------------------------------------
  //  Description  : gives ~Logger return information for console reporting
  //  Role         : invoked near end of 'main' function
  //  Techniques   : (nothing special)
  //  Status       : complete
  //
  //  Could considered using an alias but this might be brittle.
  //
  // ---------------------------------------------------------

  void
  Logger::updateReturnStatus
  (const int         returnStatus,
   const std::string returnInterpetation)    // defaults to ""
  {
    this->repx(logga::xtra, "supplied return status", returnStatus);
    d_returnStatus = returnStatus;
    d_returnInterp = returnInterpetation;
  }

  // ---------------------------------------------------------
  //  MEMBER FUNCTION : Logger::beep
  // ---------------------------------------------------------
  //  Description  : beep functionality
  //  Role         : anywhere appropriate
  //  Techniques   : tries 'beep' from the beep package first
  //
  //  beep package
  //
  //      The more exotic beep requires the 'beep (beep the pc
  //      speaker any number of ways)' Linux package -- currently
  //      tested with version 1.2.2.
  //
  //      Could also use 'Xlib' and 'XBell' on systems supporting
  //      the X Window System (X11).
  //
  //  Design notes
  //
  //      This function uses a static 'firstcall' variable to
  //      remember its first call state.
  //
  //  CAUTION: <cstdlib> 'system' call return
  //
  //      See comments in code regarding portability issues.
  //
  // ---------------------------------------------------------

  void
  Logger::beep
  (int type)                                 // magic numbers are not usually recommended
  {
    static bool notLogged = true;            // assignment at initialization only

    int cycle = 0;
    std::string args;

    switch ( type )
      {
      case 1: cycle = 1; args = "-f  400 -l 150"; break;
      case 2: cycle = 1; args = "-f 1100 -l 150"; break;
      default: std::clog << "** coding error 06 in source file " << __FILE__ << std::endl;
      }

    std::string call  = "beep " + args + " 2>/dev/null";
    const char* ccall = call.c_str();

    // flush 'stdout' and 'stdlog' streams to ensure temporal alignment
    this->flush();
    std::cout << std::flush;

    int i = 0;                               // loop counter
    while ( d_enableBeep )
      {
        ++i;
        int ret = system(ccall);             // utility call thru command interpreter
        if ( ret != 0 )                      // CAUTION: guess that system call failed [1]
          {
            // [1] but this is implementation-dependent rather than
            // mandatory C++ behavior which has only been tested using g++

            std::cout << "\007"              // revert to bell character
                      << std::flush;
            if ( notLogged )
              this->repx(logga::info, "system call to beep utility failed", ret);
            notLogged = false;               // update first call state
          }
        if ( i >= cycle ) break;             // halt criteria
        usleep(1000000);                     // sleep 1000ms
      }
    usleep(100000);                          // sleep 200ms
  }

  // ---------------------------------------------------------
  //  FREE FUNCTION   : logga::refLogStream (DEPRECIATED)
  // ---------------------------------------------------------
  //  Category      : free function in xeona namespace
  //  Purpose       : provide a reference to a single logging object
  //  Status        : *depreciated*
  //
  //  Design notes
  //
  //      This free function 'replaces' normal Logger object
  //      construction.
  //
  //      Consult the associated header file for more details.
  //
  // ---------------------------------------------------------

  Logger&
  refLogStream()
  {
    // refLogStream is depreciated so issue a suitable reminder!
    std::clog <<"PLEASE CHANGE FROM refLogStream() TO ptrLogStream()" << "\n";

    // NOTE: activate following code to log to file
    //
    // useful improvements: add timestamp to filename, place file
    // open code in try block

#if (XE_OUTPUT == 1)

    // file

    const char* filename = "hardcoded.log";  // note problems with 'common.cc' definition
    // 'text' mode is the default, 'out' is writable, 'app' is append
    static std::ofstream logfile(filename, std::ios::out|std::ios::app);
    static Logger s_file(logfile);      // define and initialize a local static object
    s_file.repx(logga::adhc, "binding", filename);
    return s_file;

#elif (XE_OUTPUT == 2)

    // std::clog

//  static Logger s_clog(std::clog);    // define and initialize a local static object
//  s_clog.repx(logga::adhc, "binding", "stdlog");
//  return s_clog;                      // return a reference to it

    static shared_ptr<Logger> s_clog(new Logger(std::clog));
    s_clog->repx(logga::adhc, "binding", "stdlog");
    return *s_clog;                      // return a reference to it

#elif (XE_OUTPUT == 3)

    // not supported

#endif // XE_OUTPUT
  }

  // ---------------------------------------------------------
  //  FREE FUNCTION   : logga::ptrLogStream
  // ---------------------------------------------------------
  //  Category      : free function in xeona namespace
  //  Purpose       : provide a shared pointer to a single logging object
  //  Status        : complete, concept proven
  //
  //  Design notes
  //
  //      This free function 'replaces' normal Logger object
  //      construction.
  //
  //      Consult the associated header file for more details.
  //
  //  Remark
  //
  //      Note the typedef in the header:
  //
  //          typedef shared_ptr<Logger> spLogger
  //
  // ---------------------------------------------------------

  shared_ptr<Logger>
  ptrLogStream()
  {
    // NOTE: to log to file, activate the following code
    //
    // useful improvements: add timestamp to filename, place file
    // open code in try block

#if (XE_OUTPUT == 1)

    // text file

    const char* filename = "hardcoded.log";  // note problems with 'common.cc' definition
    // 'text' mode is the default, 'out' is writable, 'app' is append
    static std::ofstream logfile(filename, std::ios::out|std::ios::app);
    static shared_ptr<Logger> s_file(new Logger(logfile));
    // additional reporting as appropriate
    // YEEK 22 CODE (set by '--yeek')
    if ( xeona::yeek == 22 || xeona::yeek == 1 || xeona::yeek == 2 )
      {
        s_file->repx(logga::adhc, "binding", filename);
      }
    return s_file;                      // return the shared pointer

#elif (XE_OUTPUT == 2)

    // std::clog

    static shared_ptr<Logger> s_log(new Logger(std::clog));
    // additional reporting as appropriate
    // YEEK 22 CODE (set by '--yeek')
    if ( xeona::yeek == 22 || xeona::yeek == 1 || xeona::yeek == 2 )
      {
        s_log->repx(logga::adhc, "binding", "stdlog");
      }
    return s_log;                      // return the shared pointer

#elif (XE_OUTPUT == 3)

    // std::cerr

    static shared_ptr<Logger> s_log(new Logger(std::cerr));
    // additional reporting as appropriate
    // YEEK 22 CODE (set by '--yeek')
    if ( xeona::yeek == 22 || xeona::yeek == 1 || xeona::yeek == 2 )
      {
        s_log->repx(logga::adhc, "binding", "stderr");
      }
    return s_log;                      // return the shared pointer

#endif // XE_OUTPUT
  }

  // ---------------------------------------------------------
  //  FREE FUNCTION   : logga::checkReportLevel
  // ---------------------------------------------------------

  bool                                       // 'true' if 'trip' would print
  checkReportLevel
  (const logga::Rank trip)
  {
    logga::spLogger logger  = logga::ptrLogStream();
    logga::Rank reportLevel = logger->getReportLevelRank();
    if ( trip > reportLevel ) return false;
    else                      return true;
  }

} // namespace 'logga'

//  end of file

