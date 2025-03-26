//  XEONA ENERGY SYSTEMS MODELING ENVIRONMENT
//
//  file-create-name : usage.h
//  file-create-date : Mon 07-Nov-2011 10:21 UTC
//  file-initiator   : Robbie Morrison <robbie@actrix.co.nz>
//
//  file-role        : usage messages and similar / header
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
//  $URL: file:///home/robbie/svn-root/xeona/futz/trunk/bin/xgrab-src/usage.h $
//
//  GENERAL NOTES FOR THIS FILE

//  HEADER GUARD

#ifndef _USAGE_H_
#define _USAGE_H_

//  LOCAL AND SYSTEM INCLUDES

#include "smart_ptr.h"        // switch easily between TR1 and Boost smart pointers
#include "common.h"           // common definitions for project (place last)

#include <iostream>           // standard io
#include <string>             // C++ strings
#include <vector>             // STL sequence container

//  FORWARD (PARTIAL) DECLARATIONS

class Field;
class Record;
class RecordSet;

//  CODE

// ---------------------------------------------------------
//  FREE FUNCTION   : displayUsage
// ---------------------------------------------------------
//  Description  : generates usage message
//  Role         : called with '--help'
//  Techniques   : io stream manipulators
//  Status       : complete
// ---------------------------------------------------------

void
displayUsage
(std::ostream&      os,
 const std::string& program,
 const std::string& xemext);

// ---------------------------------------------------------
//  FREE FUNCTION   : summarizeXem
// ---------------------------------------------------------
//  Description  : summarizes the given 'xemfile'
//  Role         : used by '--summarize' when no target is given
//  Techniques   : formatting functions 'formatSummarizeGen' 'formatSummarizeXem'
//  Status       : complete
// ---------------------------------------------------------

void
summarizeXem
(std::ostream&      os,
 RecordSet          xem,                     // compiler disliked a constant reference
 const std::string& xemfile);

// ---------------------------------------------------------
//  FREE FUNCTION   : summarizeTarget
// ---------------------------------------------------------
//  Description  : summarizes the given 'target'
//  Role         : used by '--summarize' with a given target
//  Techniques   : formatting function 'formatSummarizeGen'
//  Status       : complete
// ---------------------------------------------------------

void
summarizeTarget
(std::ostream&      os,
 shared_ptr<Field>  field,
 const std::string& target);

// ---------------------------------------------------------
//  FREE FUNCTION   : tellExitCode
// ---------------------------------------------------------
//  Description  : cast and interpret 'code' command-line argument
//  Role         : called with '--exitcode'
//  Techniques   : 'Boost.Conversion'
//  Status       : complete
// ---------------------------------------------------------

bool                                         // 'false' if lexical cast fails
tellExitCode
(std::ostream&      os,
 const std::string& code);

// ---------------------------------------------------------
//  FREE FUNCTION   : displayLegal
// ---------------------------------------------------------
//  Description  : display legal information
//  Role         : called with '--legal'
//  Techniques   : (nothing special)
//  Status       : complete
// ---------------------------------------------------------

void
displayLegal
(std::ostream&      os,
 const std::string& copyrightYears);

// ---------------------------------------------------------
//  FREE FUNCTION   : displayVersion
// ---------------------------------------------------------
//  Description  : display version information
//  Role         : called with '--version'
//  Techniques   : (nothing special)
//  Status       : complete
// ---------------------------------------------------------

void
displayVersion
(std::ostream&      os,
 const std::string& version);

// ---------------------------------------------------------
//  FREE FUNCTION   : dumpXem
// ---------------------------------------------------------
//  Description  : interface to function 'dump'
//  Role         : called with --jettison
//  Techniques   : 'std::copy' for vector traversal
//  Status       : complete
// ---------------------------------------------------------

int                                          // number of fields printed
dumpXem
(std::ostream&      os,
 RecordSet          xem,
 const bool         indata,
 const bool         outdata,
 const std::string& prepend = "");           // note default

// ---------------------------------------------------------
//  FREE FUNCTION   : dumpXemNames
// ---------------------------------------------------------
//  Description  : interface to function 'dump'
//  Role         : called with --names
//  Techniques   : 'std::copy' for vector traversal
//  Status       : complete
// ---------------------------------------------------------

int                                          // number of fields printed
dumpXemNames
(std::ostream&      os,
 RecordSet          xem,
 const bool         indata,
 const bool         outdata,
 const std::string& prepend = "");           // note default

// ---------------------------------------------------------
//  FREE FUNCTION   : dump
// ---------------------------------------------------------
//  Description  : dumps entire xem structure
//  Role         : services function 'dumpXem'
//  Techniques   : friendship from classes 'RecordSet' 'Record' 'Field', 'Boost.Foreach'
//  Status       : complete
// ---------------------------------------------------------

int                                          // number of fields dumped
dump
(std::vector<std::string>& data,
 RecordSet                 xem,
 const bool                indata,           // 'false' is omit in-data
 const bool                outdata,          // 'false' is omit out-data
 const bool                entire,           // 'false' is omit field kinds and values
 const std::string&        prepend);         // prepend string, can be ""

// ---------------------------------------------------------
//  FREE FUNCTION   : rejig
// ---------------------------------------------------------
//  Description  : rejig single four dot command-line argument
//  Role         : support for command-line processing
//  Techniques   : 'Boost.String_Algo'
//  Status       : complete
//
//  Design notes
//
//      If 'arg1' is "study.scenario.kind.recordId.fieldName" and
//      'arg2' is empty, then 'arg1' becomes "study.scenario.xem"
//      and 'arg2' becomes "kind.recordId.fieldName".
//
// ---------------------------------------------------------

bool                                         // 'true' if altered, else 'false'
rejig
(std::string&      arg1,                     // may become filename
 std::string&      arg2,                     // may become target
 const std::string ext);                     // file extension

// ---------------------------------------------------------
//  FREE FUNCTION   : removeFileExt
// ---------------------------------------------------------

bool                                         // 'true'if altered, else 'false'
removeFileExt
(std::string&       filename,                // filename to alter
 const std::string& ext);                    // extension WITHOUT dot

#endif // _USAGE_H_

//  end of file

