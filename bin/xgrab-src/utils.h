//  XEONA ENERGY SYSTEMS MODELING ENVIRONMENT
//
//  file-create-name : utils.h
//  file-create-date : Mon 07-Nov-2011 10:20 UTC
//  file-initiator   : Robbie Morrison <robbie@actrix.co.nz>
//
//  file-role        : utilities including fault handling / header
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
//  Copyright : This software is copyright (c) 2007 - 2011 Robbie Morrison.
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
//  $URL: file:///home/robbie/svn-root/xeona/futz/trunk/bin/xgrab-src/utils.h $
//
//  GENERAL NOTES FOR THIS FILE

//  HEADER GUARD

#ifndef _UTILS_H_
#define _UTILS_H_

//  LOCAL AND SYSTEM INCLUDES

#include "smart_ptr.h"        // switch easily between TR1 and Boost smart pointers
#include "common.h"           // common definitions for project (place last)

#include <iostream>           // standard io
#include <sstream>            // string-streams
#include <string>             // C++ strings

//  CODE

// ---------------------------------------------------------
//  CLASS           : ExitCodes (static-only)
// ---------------------------------------------------------
//  Description  : store current and defined exit codes
//  Role         : fault handling
//  Techniques   : static-only class, only 'ExitCodes::current' is defined elsewhere
//  Status       : complete
// ---------------------------------------------------------

struct ExitCodes
{
  static std::string interpretExitCode(const int code);

  static const int success     =  0;         // success
  static const int failure     =  1;         // generic (unspecified) failure
  static const int usage       =  2;         // command-line usage
  static const int codingError =  3;         // probable coding error
  static const int noXem       = 11;         // unable to open XEM file
  static const int mtXem       = 12;         // no lines read from XEM file
  static const int badXem      = 13;         // parse failures on XEM file
  static const int badRecKind  = 21;         // invalid record kind
  static const int badFqf      = 22;         // sought field badly formed
  static const int noRec       = 23;         // sought record not found
  static const int noFld       = 24;         // sought field not found
  static const int mtFqf       = 25;         // attempt to search out-data in unrun model
  static const int noFqf       = 26;         // sought target not found
  static const int badExt      = 27;         // invalid file extension
  static const int wasRun      =  0;         // from '--wasrun'
  static const int wasNotRun   =  1;         // from '--wasrun'
};

// ---------------------------------------------------------
//  CLASS           : Deport
// ---------------------------------------------------------

class Deport
{
public:

  template <typename T>                      // explicit instantiations in implementation
  static void log(const std::string  func,
                  const int          level,
                  const std::string& key,    // limit to 16 chars (but read definition)
                  const T            value); // CAUTION: best to pass-by-copy than by-ref

  static void log(const std::string  func,
                  const int          level,
                  const std::string& msg);

  static std::string interpretLevel(const int level);

private:

  static const std::string    s_intro;       // lead-in for reporting, can be ""
  static       int            s_count;       // call counter

};

// ---------------------------------------------------------
//  CLASS           : Flags (static-only)
// ---------------------------------------------------------
//  Description  : contains flags set during command-line processing
//  Role         : application-wide variable
//  Techniques   : static
//  Status       : complete
// ---------------------------------------------------------

struct Flags
{
  static void recite(const int level)
  {
    if ( failuse ) Deport::log(FUNC, level, "failmsg", failmsg);      // given message
    else           Deport::log(FUNC, level, "failmsg", failuse);      // 'false'
    Deport::log(FUNC, level, "indata",    indata);
    Deport::log(FUNC, level, "jettison",  jettison);
    Deport::log(FUNC, level, "kind",      kind);
    Deport::log(FUNC, level, "names",     names);
    Deport::log(FUNC, level, "outdata",   outdata);
    Deport::log(FUNC, level, "quiet",     quiet);
    Deport::log(FUNC, level, "summary",   summary);
    Deport::log(FUNC, level, "units",     units);
    Deport::log(FUNC, level, "unquote",   unquote);
    Deport::log(FUNC, level, "xemstub",   xemstub);
    Deport::log(FUNC, level, "free args", freeArgs);
  }

  static bool        debug;
  static bool        failuse;
  static std::string failmsg;
  static bool        indata;
  static bool        jettison;
  static bool        kind;
  static bool        names;
  static bool        outdata;
  static bool        quiet;
  static bool        summary;
  static bool        units;
  static bool        unquote;
  static bool        wasrun;                 // CAUTION: may not be accurate
  static bool        xemstub;
  static int         freeArgs;
};

// ---------------------------------------------------------
//  FREE FUNCTION   : unquoteString
// ---------------------------------------------------------

std::string                                  // returns either original or modified string
unquoteString                                // acts just on enclosing double-quotes
(const std::string& value);

// ---------------------------------------------------------
//  CLASS           : localException (abstract base)
// ---------------------------------------------------------
//  Description  : abstract base class for local exceptions
//  Role         : application-wide fault state handling
//  Techniques   : exception class, inheritance tree
//  Status       : complete
// ---------------------------------------------------------

class localException
{
private:

  localException();                                         // zero-argument constructor
  localException& operator= (const localException& orig);   // copy assignment operator

public:

  localException
  (const std::string func,
   const int         exitcode);

  virtual ~localException() = 0;             // create abstract class

  const int         code() const;
  const std::string tell() const;

  static void setProgram(const std::string& program);

protected:

  const int             d_code;              // hardcoded in constructors
  std::string           d_tell;              // formatted on construction

  static std::string    s_program;

};

// ---------------------------------------------------------
//  CLASS           : codingError (coding error)
// ---------------------------------------------------------

class codingError :
  public localException
{
public:

  codingError
  (const std::string func,
   const std::string explanation);

};

// ---------------------------------------------------------
//  CLASS           : noXem (cannot open XEM file)
// ---------------------------------------------------------

class noXem :
  public localException
{
public:

  noXem
  (const std::string func,
   const std::string xemfile);

};

// ---------------------------------------------------------
//  CLASS           : mtXem (empty XEM file)
// ---------------------------------------------------------

class mtXem :
  public localException
{
public:

  mtXem
  (const std::string func,
   const std::string xemfile);

};

// ---------------------------------------------------------
//  CLASS           : badXem (XEM parse failure)
// ---------------------------------------------------------

class badXem :
  public localException
{
public:

  badXem
  (const std::string func,
   const std::string explanation);

};

// ---------------------------------------------------------
//  CLASS           : badRecKind (record kind find failure)
// ---------------------------------------------------------

class badRecKind :
  public localException
{
public:

  badRecKind
  (const std::string func,
   const std::string recKind);

};

// ---------------------------------------------------------
//  CLASS           : noRec (record find failure)
// ---------------------------------------------------------

class noRec :
  public localException
{
public:

  noRec
  (const std::string func,
   const std::string rec);

};

// ---------------------------------------------------------
//  CLASS           : noFld (field find failure)
// ---------------------------------------------------------

class noFld :
  public localException
{
public:

  noFld
  (const std::string func,
   const std::string fld);

};

// ---------------------------------------------------------
//  CLASS           : noFqf (target find failure)
// ---------------------------------------------------------

// CAUTION: not currently used

class noFqf :
  public localException
{
public:

  noFqf
  (const std::string func,
   const std::string fqf);

};

// ---------------------------------------------------------
//  CLASS           : mtFqf (target empty)
// ---------------------------------------------------------

class mtFqf :
  public localException
{
public:

  mtFqf
  (const std::string func,
   const std::string fqf);

};

// ---------------------------------------------------------
//  CLASS           : badFqf (badly formed target)
// ---------------------------------------------------------

class badFqf :
  public localException
{
public:

  badFqf
  (const std::string func,
   const std::string fqf,
   const std::string explanation);

};

// ---------------------------------------------------------
//  CLASS           : badArg (invalid argument)
// ---------------------------------------------------------

class badArg :
  public localException
{
public:

  badArg
  (const std::string func,
   const std::string arg);

};

// ---------------------------------------------------------
//  CLASS           : badExt (invalid file extension)
// ---------------------------------------------------------

class badExt :
  public localException
{
public:

  badExt
  (const std::string func,
   const std::string arg);

};

#endif // _UTILS_H_

//  end of file

