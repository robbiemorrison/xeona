//  XEONA ENERGY SYSTEMS MODELING ENVIRONMENT
//
//  file-create-name : logger.h
//  file-create-date : Thu 05-Apr-2007 11:33 UTC
//  file-initiator   : Robbie Morrison <robbie@actrix.co.nz>
//
//  file-role        : run-time logging functionality / header
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
//  $URL: file:///home/robbie/svn-root/xeona/futz/trunk/xeona1/a/logger.h $

//  HEADER GUARD

#ifndef _LOGGER_H_
#define _LOGGER_H_

//  LOCAL AND SYSTEM INCLUDES

#include "../a/exapp.h"       // application exception classes
#include "../c/smart_ptr.h"   // switch easily between TR1 and Boost smart pointers

#include <map>                // STL associative container
#include <ostream>            // output streams
#include <sstream>            // string-streams
#include <string>             // C++ strings

#include <boost/date_time/posix_time/posix_time.hpp>  // plus io
#include <boost/algorithm/string_regex.hpp>           // additional regex support

// CAUTION: more system includes than would be usual are required
// because the 'repx' function templates are also defined in this
// file

#include <iomanip>            // setw() and family
#include <iostream>           // standard io

#include <boost/algorithm/string.hpp>   // string recasing, trimming, splitting
#include <boost/lexical_cast.hpp>       // lexical_cast<> string to number conversions
#include <boost/regex.hpp>              // regular expression support

//  FORWARD (PARTIAL) DECLARATIONS

// 'xeona::loggerRepxPrec' sets 'std::iostream' precision for floats
namespace xeona { extern const int loggerRepxPrec; }   // see unit 'common'

//  CODE

// ---------------------------------------------------------
//  notes           : TR1 or Boost shared pointers
// ---------------------------------------------------------
//
//  Until and including r2211, this unit was hard coded for
//  'std::tr1::shared_ptr'.  From r2212, the local 'smart_ptr.h'
//  header was included, which controls whether TR1 or Boost code
//  is used, and the declarations here duly modified.
//
//  For other applications, this unit can be reverted to the
//  original TR1 code.
//
// ---------------------------------------------------------
//  notes           : Implementation code in header
// ---------------------------------------------------------
//
//  Some of the implementation code is placed in this header.
//
//  The reason is that the final lines of this file contain two
//  preprocessor hash-define macros which act on the member
//  functions 'repx' and 'putx'.  If enabled, these macros make
//  __FILE__, __LINE__, and __func__ information available these
//  functions and thus enable this information to be logged to
//  the selected output stream.
//
//  Moreover the 'repx' function is also templated and a full
//  list of instantiations cannot be fully identified in advance.
//  This fact alone would make this function a candidate for
//  definition in the header file.
//
//  Note also that the two member functions and each variant
//  (four functions in total) need to 'inlined' in order to avoid
//  a link-time "multiple definition error".
//
// ---------------------------------------------------------
//  notes           : log call timings
// ---------------------------------------------------------
//
//  The Boost.Chrono library was released after this unit was
//  written.  The library might provide a better design for
//  calculating the duration between consecutive log calls.
//
// ---------------------------------------------------------

namespace logga
{
  // ---------------------------------------------------------
  //  TYPEDEF         : logga::spLogger
  // ---------------------------------------------------------
  //  Purpose       : simply for coding convenience
  // ---------------------------------------------------------

  class Logger;                              // forward (partial) declaration
  typedef shared_ptr<logga::Logger> spLogger;

  // ---------------------------------------------------------
  //  ENUM            : logga::Rank
  // ---------------------------------------------------------
  //  Purpose       : set of enums to influence reporting
  //  Application   : in function arguments
  //  Status        : complete
  // ---------------------------------------------------------

  // CAUTION: these enums are not proceeded by "e_" in the interests of brevity

  enum Rank
    {
      yeek = 0,                              // for use during development
      kill = 1,
      warn = 2,
      info = 3,
      dbug = 4,
      xtra = 5,                              // extra reporting, say from destructors
      adhc = 6,                              // ad-hoc and insignificant reporting
      none = 7                               // no log calls made
    };

  // ---------------------------------------------------------
  //  VARIABLE        : logga::rankNoData
  //  VARIABLE        : logga::rankJumpy
  // ---------------------------------------------------------

  extern Rank rankNoData;                    // set by '--nodata'
  extern Rank rankJumpy;                     // set by '--jumpy'

  // ---------------------------------------------------------
  //  CLASS           : logga::Logger
  // ---------------------------------------------------------
  //  Purpose       : universal logging object for each 'ostream'
  //  Status        : complete
  //  Comment       : the formatting code might be better expressed using Boost.Format
  //
  //  CAUTION: unusual code placement
  //
  //      Certain function are defined in this header file due to
  //      the preprocessor macro.  See elsewhere for details.
  //
  //  CAUTION: use of 'repx' in 'const' member functions
  //
  //     Calls to 'this->repx(..)' cannot be used in member
  //     functions deemed 'const' because 'repx' does not embed
  //     this guarantee.
  //
  // ---------------------------------------------------------

  class Logger
  {
    // PRIVATE ENUMS

  private:

    enum CallType                            // used to control additional blank lines
      {
        e_none     = 0,                      // as an initial value
        e_repx     = 1,                      // previous call was 'repx'
        e_putx     = 2,                      // previous call was 'putx'
        e_addblank = 3                       // add blank line before next logging output
      };

    // DISABLED

  private:

    Logger();                                // prevent instantiation without an ostream
    Logger(const Logger& orig);              // copy constructor
    Logger& operator= (const Logger& orig);  // copy assignment operator

    // CREATORS

  public:

    Logger
    (std::ostream& os);

    ~Logger();

    // the 'repx' suite of functions are suitable for
    // preprocessor overwrite, in which case the file/line/func
    // versions are called

    // LOGGING FUNCTIONS

    template <typename T>
    inline
    bool                                     // 'true' means message was displayed
    repx                                     // normal single line logging
    (const logga::Rank rank,                 // the "logga::" is, in fact, not needed
     const std::string text,                 // user supplied text
     const T&          value);               // implicit instantiation used

    template <typename T>
    inline
    bool
    repx
    (const logga::Rank rank,
     const std::string text,                 // user supplied text
     const T&          value,                // implicit instantiation used
     const std::string file,                 // from preprocessor macro __FILE__
     const int         line,                 // from preprocessor macro __LINE__
     const std::string func);                // from preprocessor macro __func__

    bool
    putx                                     // string-stream multi-line logging
    (const logga::Rank   rank,
     std::ostringstream& oss);               // users string-stream which is also nulled

    bool
    putx
    (const logga::Rank   rank,
     std::ostringstream& oss,                // users string-stream which is also nulled
     const std::string   file,               // from preprocessor macro __FILE__
     const int           line,               // from preprocessor macro __LINE__
     const std::string   func);              // from preprocessor macro __func__

    bool
    dotx                                     // output a row of dots or similar
    (const logga::Rank  rank,
     const std::string& text);

    bool
    dotx                                     // output a row of dots or similar
    (const logga::Rank  rank,
     const std::string& text,
     const std::string  file,                // from preprocessor macro __FILE__
     const int          line,                // from preprocessor macro __LINE__
     const std::string  func);               // from preprocessor macro __func__

    bool
    test                                     // insert a TEST COUNT COMMENCING line
    (const int testCount);                   // 'testCount' is the desired test count

    bool
    test
    (const int         testCount,
     const std::string putMsg);              // 'put'-type message without "\n"

    void
    addSmartBlank();                         // add blank line in a relatively smart way

    void
    addSmartBlank                            // ditto, but only if rank-appropriate
    (const logga::Rank rank);

    void
    addDumbBlank();                          // add blank line without question

    void
    addDumbBlank                             // ditto, but only if rank-appropriate
    (const logga::Rank rank);

    void
    addFinalStdoutBlank();

    void
    flush();                                 // explicitly flush the output buffer,
                                             //   but with no other action
    void
    beep                                     // using system utility or bell character
    (int type);                              // controls type of beep

    // ACCESSORS

    logga::Rank
    getHighestRankAll() const;               // eg: return 'logga::warn' if warnings made

    unsigned getYeekAllCount() const { return d_yeekAllCount; }
    unsigned getKillAllCount() const { return d_killAllCount; }
    unsigned getWarnAllCount() const { return d_warnAllCount; }
    unsigned getInfoAllCount() const { return d_infoAllCount; }
    unsigned getDbugAllCount() const { return d_dbugAllCount; }
    unsigned getXtraAllCount() const { return d_xtraAllCount; }
    unsigned getAdhcAllCount() const { return d_adhcAllCount; }

    unsigned getYeekFixedCount() const { return d_yeekFixedCount; }
    unsigned getKillFixedCount() const { return d_killFixedCount; }
    unsigned getWarnFixedCount() const { return d_warnFixedCount; }
    unsigned getInfoFixedCount() const { return d_infoFixedCount; }
    unsigned getDbugFixedCount() const { return d_dbugFixedCount; }
    unsigned getXtraFixedCount() const { return d_xtraFixedCount; }
    unsigned getAdhcFixedCount() const { return d_adhcFixedCount; }

    unsigned getYeekResetCount() const { return d_yeekResetCount; }
    unsigned getKillResetCount() const { return d_killResetCount; }
    unsigned getWarnResetCount() const { return d_warnResetCount; }
    unsigned getInfoResetCount() const { return d_infoResetCount; }
    unsigned getDbugResetCount() const { return d_dbugResetCount; }
    unsigned getXtraResetCount() const { return d_xtraResetCount; }
    unsigned getAdhcResetCount() const { return d_adhcResetCount; }

    logga::Rank
    getReportLevelRank() const;

    unsigned
    getReportLevelInt() const;

    std::string
    getReportLevelStr() const;

    const std::string                        // all space-separated ranks
    getAllTriggerStr() const;

    const std::string                        // has an uncharacteristic trailing newline
    summarizeRankCounts() const;             // makes small table

    const std::string
    getTrigger() const;                      // as set by '--watch' option

    bool                                     // 'true' if '--jumpy'
    jumpyIsWarn() const;

    // STATIC MANIPULATORS

    static
    bool
    enableConsoleTruncation();               // on by default, set using '_XCOLS' macro

    static
    bool
    disableConsoleTruncation();              // do not truncate output to suit '_XCOLS'

    // MANIPULATORS

    bool
    enableBeeping();                         // general activation of beeping

    bool
    disableBeeping();                        // general suppression of beeping

    void
    setBeepOnCompletion();                   // beep call made from '~Logger'

    void
    setBeepOnOrAbove                         // run-time beep trigger
    (logga::Rank rank);

    void
    setTrigger                               // as set by --trigger option
    (const std::string& trigger);

    void
    resetRankCounts                          // at given level and noisier
    (const unsigned reportLevel = 1);        // defaults to logga::kill

    Rank                                     // return previous report level
    setReportLevel                           // also resets triggers based on reportLevel
    (const unsigned reportLevel);            // will also accept a logga::Rank

    bool                                     // 'false' indicates 'oss.good()' failed
    resetOSS                                 // restore string-stream after manip'ing
    (std::ostringstream& oss);               // user-supplied string-stream to be reset

    void
    updateReturnStatus
    (const int         returnStatus,
     const std::string returnInterpetation = "(not forwarded)");

    // UTILITY FUNCTIONS

  private:

    bool
    shouldLog
    (const std::string& file);

    void
    firstCall();                             // print space, header, and rule

    void
    printHeader();                           // print reporting header

    void
    printRule                                // print reporting rule
    (const int skip);                        // number of blank lines to follow rule

    void
    incrementRankCounts                      // update counters
    (unsigned rank);                         // will also accept a logga::Rank

    std::string
    formatTriggerStr                         // used by 'getAllTriggerStr'
    (logga::Rank rank,
     unsigned    count) const;

    void
    formatRankCount                          // used by 'summarizeRankCounts'
    (std::ostringstream& os,                 // passed-in string-stream
     const std::string&  term,
     unsigned            all,
     unsigned            fixed,
     unsigned            reset) const;

    void
    endOfLogCall                             // only reacts to first kill
    (const logga::Rank rank)
      throw(std::exception,                  // exception specification
            xeona::kill_on_log);

    std::string                              // current timestamp, truncated to 0.00 secs
    timestampUTC();

    std::string               // zero-padded truncated number, say, 5.003699 -> 05.0037
    numToPad                                 // used in 'calcInterval'
    (double input,
     int    LEFT  = 2,                       // zero-padded digits left of decimal point
     int    RIGHT = 4);                      // digits right of decimal point

    std::string                              // return was-now interval in forms 00.0000s,
    calcInterval();                          //   00.0000m, or 00.0000h, or complain

    std::string                              // four character string describing rank
    calcRank                                 // obtain rank term for use in reporting
    (const Rank  rank) const;

    // INSTANCE DATA

  private:

    std::ostream&               d_os;             // can be either console or file ostream
    boost::posix_time::ptime    d_was;            // point in time object
    CallType                    d_lastCall;       // track last call type
    unsigned                    d_repCount;       // report call count
    Rank                        d_reportLevel;    // can be reset on command-line
    bool                        d_enableBeep;     // controls beeping
    bool                        d_beepCompletion; // defines beeping behavior
    Rank                        d_beepThreshold;  // defines beeping behavior
    std::string                 d_triggerStr;     // string trigger for added reporting
    boost::regex                d_triggerRegex;   // regex trigger for added reporting
    bool                        d_finalBlank;     // add final blank line to stdout

    int                         d_returnStatus;   // supplied at end of main
    std::string                 d_returnInterp;   // supplied at end of main

    unsigned d_yeekNoFirst;                  // close-of-application reporting
    unsigned d_killNoFirst;                  // for recording no (= number) of first call
    unsigned d_warnNoFirst;

    unsigned d_yeekAllCount;                 // close-of-application reporting
    unsigned d_killAllCount;                 // a simple count of calls
    unsigned d_warnAllCount;
    unsigned d_infoAllCount;
    unsigned d_dbugAllCount;
    unsigned d_xtraAllCount;
    unsigned d_adhcAllCount;

    unsigned d_yeekFixedCount;               // close-of-application reporting
    unsigned d_killFixedCount;               // a simple count of prints
    unsigned d_warnFixedCount;
    unsigned d_infoFixedCount;
    unsigned d_dbugFixedCount;
    unsigned d_xtraFixedCount;
    unsigned d_adhcFixedCount;

    unsigned d_yeekResetCount;               // for test purposes
    unsigned d_killResetCount;               // simple count of reset prints
    unsigned d_warnResetCount;
    unsigned d_infoResetCount;
    unsigned d_dbugResetCount;
    unsigned d_xtraResetCount;
    unsigned d_adhcResetCount;

    // STATIC DATA

  private:

    static bool               s_truncate;         // console truncation flag

    // in-class initialization of static constant integers is acceptable

    static const unsigned int s_MAR  =       0;   // left margin
    static const unsigned int s_GUT  =       3;   // standard gutter
    static const unsigned int s_OVR  =       2;   // overlength string separation
    static const unsigned int s_RULE =     130;   // rule length
    static const unsigned int s_TERM = _XTCOLS;   // external macro set via CPPFLAGS

    static const unsigned int s_TAB1 =       4;   // file line number field
    static const unsigned int s_TAB2 =    12+3;   // file name field, was 12+
    static const unsigned int s_TAB3 =      20;   // function name field
    static const unsigned int s_TAB4 =       6;   // report counter (was originally 4)
    static const unsigned int s_TAB5 =       8;   // delta-t field
    static const unsigned int s_TAB6 =       4;   // rank field
    static const unsigned int s_TAB7 =      35;   // message field

  };

  // ---------------------------------------------------------
  //  MEMBER FUNCTION : logga::Logger::repx (without compiler macros)
  // ---------------------------------------------------------

  template <typename T>
  inline
  bool
  Logger::repx
  (const logga::Rank rank,
   const std::string text,
   const T&          value)
  {
    // check verbosity
    if ( rank > d_reportLevel )
      {
        incrementRankCounts(rank);           // update rank register
        return false;
      }
    else
      {
        if ( d_repCount == 0 ) firstCall();  // note first call
        incrementRankCounts(rank);           // update rank register
      }

    // local ostringstream buffer
    std::ostringstream ssBuf;

    // use bool (and boost::logic::tribool) terms and not digits
    ssBuf << std::boolalpha;

    // 'setprecision' normally controls the number of digits on
    // both side of the decimal point for floating point numbers,
    // but placed after 'fixed' it controls the number of digits
    // on the right side of the decimal point -- namely:
    // 5500000.00

    ssBuf << std::fixed
          << std::setprecision(xeona::loggerRepxPrec);      // 2 or 3 set in unit 'common'

    ssBuf << std::setw(s_TAB4 + s_MAR) << std::right << d_repCount
          << std::setw(         s_GUT) << ""
          << std::setw(s_TAB5        ) << std::left  << calcInterval()
          << std::setw(         s_GUT) << ""
          << std::setw(s_TAB6 + s_GUT) << std::left  << calcRank(rank)
          << std::setw(s_TAB7        ) << std::left  << text
          << std::setw(         s_OVR) << ""
          <<                              std::left  << value;

    // the main stringstream is truncated to avoid line wrapping
    // on the console

    std::string strBuf = ssBuf.str();             // stringify
    if ( strBuf.length() > s_TERM && s_truncate)  // truncate if necessary
      {
        strBuf.resize(s_TERM - 2);
        strBuf += " >";                           // truncation symbol
      }
    boost::trim_right(strBuf);                    // trim trailing spaces in-situ

    // FOURTH: unrelated tasks

    // add blank line as needed
    if ( d_lastCall == e_putx )
      d_os << "\n";                          // insert newline
    if ( d_lastCall == e_addblank )
      d_os << "\n";

    // update status
    d_lastCall = e_repx;

    // write out main message
    d_os << strBuf << "\n";
    ++d_repCount;

    // housekeeping
    endOfLogCall(rank);
    return true;
  }

  // ---------------------------------------------------------
  //  MEMBER FUNCTION : logga::Logger::repx (with compiler macros)
  // ---------------------------------------------------------

  // CAUTION: code quality: this code may be better reworked
  // using Boost.Format library calls for at least some of the
  // functionality

  template <typename T>
  inline
  bool
  Logger::repx
  (const logga::Rank rank,
   const std::string text,
   const T&          value,
   const std::string file,                   // from preprocessor macro __FILE__
   const int         line,                   // from preprocessor macro __LINE__
   const std::string func)                   // from preprocessor macro __func__
  {
    // check verbosity
    if ( rank > d_reportLevel                // 'rank' exceeds 'd_reportLevel' threshold
         && ! shouldLog(file) )
      {
        incrementRankCounts(rank);           // update rank register
        return false;
      }
    else
      {
        if ( d_repCount == 0 ) firstCall();  // note first call
        incrementRankCounts(rank);           // update rank register
      }

    // FIRST: the line (line), file (source), func (call)
    // information is processed and then turned into a local
    // string -- this constrains long filenames by allowing them
    // to push into the call column but no further

    std::ostringstream ssTmp;                // for line, file, and func information
    std::string        strTmp;               // string version of above
    int                tab;                  // setw allowance in main stringstream

    // ALSO: process the 'file' string to remove valuless path information

    std::string path(file);                  // also removes 'const' status

#if 0 // _XRELEASE

    // conditional compilation to shorten link-time for
    // non-release builds -- CAUTION: must be conditional
    // compilation and not an 'xeona::releaseStatus' if block in
    // order to do so

    // EXTREME CAUTION: leave this first macro block disabled --
    // it causes segfaults, perhaps in 'logga::Logger::~Logger',
    // with more complex models like 'submodel.11' under
    // '--report 3' or greater -- see commit r4240 and earlier
    // for more details

    // leftmost longest rule and repetition apply: "a/../b/../c/header.h" -> "c/header.h"

    const std::string rgxstr = "([[:alnum:]]+/\\.\\./)";
    const std::string patstr = "";                  // \\1 is 'rgxstr' match, try "\\1--"
    boost::regex pattern(rgxstr);                   // make a regular expression
    path = boost::regex_replace                     // replace variant
      (path,                                        // so-called target sequence
       pattern,                                     // regular expression
       patstr,                                      // format string
       boost::match_default | boost::format_sed);   // search options, 'sed' is simplest

#else // use a more basic alternative (added later, in case you're wondering)

    while ( path.substr(1, 4) == "/../" )    // assume SINGLE character directories
      {
        path.erase(0, 5);
      }

#endif // _XRELEASE

    // carry on as before with 'path' instead of 'file'

    ssTmp << std::setw(s_TAB1 + s_MAR) << std::right << line
          << std::setw(         s_GUT) << ""
          << std::setw(s_TAB2        ) << std::left  << path     // was 'file'
          << std::setw(         s_OVR) << ""
          << std::setw(s_TAB3 + s_GUT) << std::left  << func;

    strTmp = ssTmp.str();
    tab    = s_TAB1 + s_MAR + s_GUT + s_TAB2 + s_TAB3 + s_GUT;
    boost::trim_right(strTmp);               // trim trailing spaces in-situ

    // SECOND: the main stringstream is built

    std::ostringstream ssBuf;                // main stringstream

    // use bool (and boost::logic::tribool) terms and not digits
    ssBuf << std::boolalpha;

    // 'setprecision' normally controls the number of digits on
    // both side of the decimal point for floating point numbers,
    // but placed after 'fixed' it controls the number of digits
    // on the right side of the decimal point -- namely:
    // 5500000.00

    ssBuf << std::fixed
          << std::setprecision(xeona::loggerRepxPrec);      // 2 or 3 set in unit 'common'

    ssBuf << std::setw(tab)            << std::left  << strTmp   // line, file, func
          << std::setw(         s_OVR) << ""
          << std::setw(s_TAB4        ) << std::right << d_repCount
          << std::setw(         s_GUT) << ""
          << std::setw(s_TAB5        ) << std::left  << calcInterval()
          << std::setw(         s_GUT) << ""
          << std::setw(s_TAB6 + s_GUT) << std::left  << calcRank(rank)
          << std::setw(s_TAB7        ) << std::left  << text
          << std::setw(         s_OVR) << ""
          <<                              std::left  << value;

    // THIRD: the main stringstream is truncated to avoid line
    // wrapping on the console

    std::string strBuf = ssBuf.str();             // stringify
    if ( strBuf.length() > s_TERM && s_truncate)  // truncate if necessary
      {
        strBuf.resize(s_TERM - 2);
        strBuf += " >";                           // truncation symbol
      }
    boost::trim_right(strBuf);                    // trim trailing spaces in-situ

    // FOURTH: unrelated tasks

    // add blank line as needed
    if ( d_lastCall == e_putx )
      d_os << "\n";                          // insert newline
    if ( d_lastCall == e_addblank )
      d_os << "\n";

    // update status
    d_lastCall = e_repx;

    // write out main message
    d_os << strBuf << "\n";
    ++d_repCount;

    // housekeeping
    endOfLogCall(rank);
    return true;
  }

  // ---------------------------------------------------------
  //  MEMBER FUNCTION : logga::Logger::putx (with/without compiler macros)
  // ---------------------------------------------------------

  inline                                     // CAUTION: protects against link errors
  bool
  Logger::putx
  (const logga::Rank   rank,
   std::ostringstream& oss)                  // users string-stream which is also nulled
  {
    // check if needed
    if ( oss.str().empty() )
      return false;                          // simply quit if 'oss' is empty

    // check verbosity
    if ( rank > d_reportLevel )
      {
        oss.str("");                         // empty the non-const o-string-stream [1]
        return false;
      }

    // [1] emptying an ostringsteam: 'str(const std::string s)'
    // will set the buffer contents to 's', see Lischner (2003
    // p653) and elsewhere

    // add blank line as needed
    if ( d_lastCall == e_repx )
      d_os << "\n";                          // insert newline
    if ( d_lastCall == e_addblank )
      d_os << "\n";

    // update status
    d_lastCall = e_putx;

    // write out main message
    d_os << oss.str();
    oss.str("");                             // empty the non-const o-string-stream [1]

    // housekeeping
    endOfLogCall(rank);
    return true;
  }

  inline
  bool
  Logger::putx
  (const logga::Rank   rank,
   std::ostringstream& oss,                  // users string-stream which is also nulled
   const std::string  file,                  // from preprocessor macro __FILE__
   const int          line,                  // from preprocessor macro __LINE__
   const std::string  func)                  // from preprocessor macro __func__
  {
    // check if needed
    if ( oss.str().empty() )
      return false;                          // simply quit if 'oss' is empty

    // check verbosity
    if ( rank > d_reportLevel                // 'rank' exceeds 'd_reportLevel' threshold
         && ! shouldLog(file) )
      {
        oss.str("");                         // empty the non-const o-string-stream [1]
        return false;
      }

    // [1] emptying an ostringsteam: 'str(const std::string s)'
    // will set the buffer contents to 's', see Lischner (2003
    // p653) and elsewhere

    // add blank line as needed
    if ( d_lastCall == e_repx )
      d_os << "\n";                          // add a newline
    if ( d_lastCall == e_addblank )
      d_os << "\n";

    // update status
    d_lastCall = e_putx;

    // write out main message
    d_os << oss.str();                       // I think this is the best (and only?) way
    oss.str("");                             // empty the non-const o-string-stream [1]

    // housekeeping
    endOfLogCall(rank);
    return true;
  }

  // ---------------------------------------------------------
  //  MEMBER FUNCTION : logga::Logger::dotx (with/without compiler macros)
  // ---------------------------------------------------------

  inline
  bool
  Logger::dotx
  (const logga::Rank  rank,
   const std::string& text)
  {
    // process information
    char c = ' ';
    switch ( rank )
      {
      case yeek: c = '*'; break;
      case kill: c = '-'; break;
      case warn: c = '-'; break;
      case info: c = '-'; break;
      case dbug: c = '-'; break;
      case xtra: c = '-'; break;
      case adhc: c = '-'; break;
      default: std::clog << "** coding error 01 in source file " << __FILE__ << std::endl;
      }
    std::ostringstream oss;
    oss << "  "
        << "  " << std::string(2, c)
        << "  " << calcRank(rank)
        << "  " << std::string(20, c)
        << "  " << text
        << "\n";
    return putx(rank, oss);                  // simplifies the coding!
  }

  inline
  bool
  Logger::dotx
  (const logga::Rank  rank,
   const std::string& text,
   const std::string  file,                  // from preprocessor macro __FILE__
   const int          line,                  // from preprocessor macro __LINE__
   const std::string  func)                  // from preprocessor macro __func__
  {
    const unsigned len = 60;                 // hard-coded length of output string

    // process information
    char c = '\0';                           // ASCII 0 null character
    switch ( rank )
      {
      case yeek: c = '*'; break;
      case kill: c = '-'; break;
      case warn: c = '-'; break;
      case info: c = '-'; break;
      case dbug: c = '-'; break;
      case xtra: c = '-'; break;
      case adhc: c = '-'; break;
      default: std::clog << "** coding error 02 in source file " << __FILE__ << std::endl;
      }

    const std::string macroSep = "  ";       // reporting separator
    std::ostringstream tmp;
    tmp << " "
        << " " << std::string(2, c)
        << " " << calcRank(rank)
        << " " << std::string(4, c)
        << " " << line << macroSep << file << macroSep << func
        << " " << std::string(len, c);
    std::string str = tmp.str();
    if ( str.length() > len )                // strictly unnecessary
      str.erase(len);                        // truncate
    std::ostringstream oss;
    oss << str << " " << text << "\n";
    return putx(rank, oss);                  // simplifies the coding!
  }

  // ---------------------------------------------------------
  //  FREE FUNCTION   : logga::refLogStream
  // ---------------------------------------------------------
  //  Status       : obsolete, used ptrLogStream instead
  //
  //  CAUTION: code placement
  //
  //      Must be preceded by full Logger declaration.
  //
  //  Note
  //
  //      See implementation (.cc) file for design analysis.
  //
  // ---------------------------------------------------------

  Logger&
  refLogStream();

  // ---------------------------------------------------------
  //  FREE FUNCTION   : logga::ptrLogStream
  // ---------------------------------------------------------
  //  Status       : use in preference to refLogStream
  //
  //  CAUTION: code placement
  //
  //      Must be preceded by full Logger declaration.
  //
  //  Note
  //
  //      See implementation (.cc) file for design analysis.
  //
  // ---------------------------------------------------------

  shared_ptr<Logger>
  ptrLogStream();

  // ---------------------------------------------------------
  //  FREE FUNCTION   : logga::checkReportLevel
  // ---------------------------------------------------------

  bool                                       // 'true' if 'trip' would print
  checkReportLevel
  (const logga::Rank trip);

} // namespace 'logga'

// ---------------------------------------------------------
//  CPP MACROS      : logga::repx, logga::putx
// ---------------------------------------------------------

// Preprocessor macro to gain additional reporting under debug
// compilation, noting that this macro degrades gracefully if the
// hash-define is not made.  The use of brackets around first
// three arguments is also recommended.

#if 1 // 0 is deactivated, 1 is activated
#  define repx(a, b, c) repx((a), (b), (c), __FILE__, __LINE__, __func__)
#  define putx(a, b)    putx((a), (b),      __FILE__, __LINE__, __func__)
#  define dotx(a, b)    dotx((a), (b),      __FILE__, __LINE__, __func__)
#endif

#endif // _LOGGER_H_

//  end of file

