//  XEONA ENERGY SYSTEMS MODELING ENVIRONMENT
//
//  file-create-name : xedocs.h
//  file-create-date : Tue 23-Sep-2008 06:28 UTC
//  file-initiator   : Robbie Morrison <robbie@actrix.co.nz>
//
//  file-role        : processing of 'xedoc' entity documentation / header
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
//  $URL: file:///home/robbie/svn-root/xeona/futz/trunk/xeona1/a/xedocs.h $
//
//  GENERAL NOTES FOR THIS FILE
//
//      Brittle behavior: the process of scanning and building
//      xedoc information breaks in the face of multiple blank
//      lines in a xedoc block -- one fix is to modify the
//      'xedox' script to "squeeze" the input, another is to
//      check here that each entry finishes with header
//      information.
//
//  This unit requires that the text file defined by
//  'XEDOCS_FILE' in 'common.cc' be present.  Moreover the
//  contents of this text file (even if lacking information)
//  must:
//
//      * be set in double quotes ("")
//      * comprise a single line
//      * replace genuine newline chars (ASCII 10 for UNIX) with "\n" substrings
//      * replace genuine double quote chars with "\"" substrings
//
//  That said, leading and trailing blank lines are okay.
//
//  Additional requirements for the structuring of data are
//  discussed elsewhere.  But here is a simple example:
//
//      "entity.one\n\n    class  > One\n\n\nentity.two\n\n    class  > Two\n"

//  HEADER GUARD

#ifndef _XEDOCS_H_
#define _XEDOCS_H_

//  LOCAL AND SYSTEM INCLUDES

#include "../a/logger_fwd.h"  // some forward declarations from "logger.h"
#include "../c/smart_ptr.h"   // toggle between Boost and TR1 smart pointers

#include <string>             // C++ strings
#include <map>                // STL associative container

//  CODE

// ---------------------------------------------------------
//  CLASS           : Xedocs
// ---------------------------------------------------------
//  Description  : usually short-lived and single object called from within 'main.cc'
//  Role         : support for the '--class arg' option
//  Techniques   : 'std::map' key/value database, underlying data embedded at compile-time
//  Status       : complete
//
//  Design notes
//
//      The underlying data is held in the string
//      'xeona::xedocsFileContent' from the 'common' unit and is
//      "loaded" at compile-time using a preprocessor
//      hash-include directive.  The hash-included filename is
//      held in the string 'xeona::xedocsFileName', also from the
//      'common' unit.
//
//      The hash-included file is generated (and later nullified)
//      by the 'mach' build script.  Hence, the binary must be
//      built using a call to 'mach' and not 'make'.
//
//      The 'mach' script calls the 'xedoc' script to refresh the
//      said hash-included file.
//
// ---------------------------------------------------------

class Xedocs
{
  // LOCAL ENUMERATIONS

public:

  enum FindStatus
    {
      e_unknown = 0,
      e_classFound,
      e_classNotFound,
      e_emptyDatabase
    };

  // DISABLED

private:

  Xedocs(const Xedocs& orig);                // copy constructor
  Xedocs& operator= (const Xedocs& orig);    // copy assignment operator

  // CREATORS

public:

  Xedocs();                                  // zero-argument constructor

  ~Xedocs();                                 // destructor

  // ACCESSORS

  const FindStatus                           // status enum
  findXedocForClass
  (const std::string soughtClass,            // sought entity class name
   std::string&      result);                // documentation if successful, else empty

  const FindStatus                           // status enum
  findXedocForRegex
  (const std::string regex,                  // sought entity class regular expression
   std::string&      result);                // documentation if successful, else empty

  const int                                  // number of classes
  dumpClassNames
  (std::string& result);                     // newline-separated and sorted list

  // UTILITY FUNCTIONS

private:

  bool
  makeDatabase();                            // called by the constructor

  // INSTANCE DATA

private:

  const std::string                     d_fileName;         // as given in macro
  const std::string                     d_fileContents;     // as read in
  std::map<std::string, std::string>    d_database;         // key/value database

  // STATIC DATA

private:

  static logga::spLogger    s_logger;        // shared_ptr to single logger object

};

#endif // _XEDOCS_H_

//  end of file

