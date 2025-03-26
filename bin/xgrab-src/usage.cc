//  XEONA ENERGY SYSTEMS MODELING ENVIRONMENT
//
//  file-create-name : usage.cc
//  file-create-date : Mon 07-Nov-2011 10:21 UTC
//  file-initiator   : Robbie Morrison <robbie@actrix.co.nz>
//
//  file-role        : usage messages and similar / implementation
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
//  $URL: file:///home/robbie/svn-root/xeona/futz/trunk/bin/xgrab-src/usage.cc $
//
//  GENERAL NOTES FOR THIS FILE

//  LOCAL AND SYSTEM INCLUDES

#include "usage.h"            // companion header for this file (place first)

#include "xemtree.h"          // XEM recordset tree
#include "common.h"           // common definitions for project (place last)

#include <algorithm>          // STL copying, searching, and sorting, max()
#include <iomanip>            // setw() and family
#include <iostream>           // standard io
#include <sstream>            // string-streams
#include <string>             // C++ strings

#include <boost/foreach.hpp>  // BOOST_FOREACH iteration macro
#include <boost/format.hpp>   // printf style formatting

#include <boost/lexical_cast.hpp>       // lexical_cast<> string to number conversions
#include <boost/algorithm/string.hpp>   // string recasing, trimming, splitting

//  CODE

// ---------------------------------------------------------
//  FREE FUNCTION   : ::formatExitCode
// ---------------------------------------------------------

namespace
{
  std::string
  formatExitCode
  (const int code)
  {
    std::ostringstream oss;
    oss << std::setw(2) << code << " = " << ExitCodes::interpretExitCode(code);
    return oss.str();
  }
}

// ---------------------------------------------------------
//  FREE FUNCTION   : ::tab
// ---------------------------------------------------------

namespace
{
  std::string
  tab
  (const std::string tag = "")
  {
    std::ostringstream oss;
    if ( tag.empty() ) oss << boost::format("%19s") % "";
    else               oss << boost::format("%17s: ") % tag;
    return oss.str();
  }
}

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
 const std::string& xemext)
{
  Deport::log(FUNC, 1, "about to display usage message");
  const std::string p = program;

  os << "\n";

  os << ::tab("usage")        << p << " [genopts] [opts] <xemfile>             "
    "parse 'xemfile' but do nothing more\n"
     << ::tab()               << " --jettison                                  "
    "dump the 'xemfile'\n"
     << ::tab()               << " --names                                     "
    "list all the fully-qualified field names\n"
     << ::tab()               << " --summary                                   "
    "summarize the 'xemfile'\n"
     << ::tab()               << " --wasrun                                    "
    "exit 0 if 'xemfile' has run, else exit 1\n"

     << ::tab()               << p << " [genopts] [opts] <xemfile> <target>    "
    "search 'xemfile' for fully-qualified 'target'\n"
     << ::tab()               << " --kind                                      "
    "print \">\" for in-data and \"<\" for out-data\n"
     << ::tab()               << " --measure                                   "
    "recover unit of measure (if present)\n"
     << ::tab()               << " --summary                                   "
    "summarize 'target'\n"

     << ::tab()               << p << " [genopts] [opts] <xemstub.target>      "
    "rework 'xemstub' and the fully-qualified 'target'\n"
     << ::tab()               << " --kind                                      "
    "print \">\" for in-data and \"<\" for out-data\n"
     << ::tab()               << " --measure                                   "
    "recover unit of measure (if present)\n"
     << ::tab()               << " --summary                                   "
    "summarize 'target'\n"

     << ::tab()               << p << " --exitcode <code>                      "
    "describe exit code 'code' and quit\n"
     << ::tab()               << p << " --help                                 "
    "display this message and quit\n"
     << ::tab()               << p << " --legal                                "
    "display license message and quit\n"
     << ::tab()               << p << " --version                              "
    "display version string and quit\n"

     << ::tab("data opts")    << " --indata                                    "
    "limit output to in-data (inputs)\n"
     << ::tab()               << " --outdata                                   "
    "limit output to out-data (results)\n"
     << ::tab()               << " --quiet                                     "
    "do not report identified faults (unresolved exceptions)\n"
     << ::tab()               << " --unquote                                   "
    "remove enclosing quotes before returning value\n"
     << ::tab()               << " --xemstub                                   "
    "append 'xemstub' where relevant (requires correct file extension)\n"

     << ::tab("general opts") << " --debug                                     "
    "active debug mode (noisy)\n"
     << ::tab()               << " --failmsg <msg>                             "
    "print 'msg' on failure (can be \"\")\n"
     << ::tab()               << " --quiet                                     "
    "do not report identified faults (unresolved exceptions)\n"

     << ::tab("purpose")      << "obtain and print fully-qualified field values"
    " from a given XEM file, also report more generally\n"

     << ::tab("notes")        << "string values retain their double-quotes, hen"
    "ce \"\" indicates an empty string and not an absent value\n"
     << ::tab()               << "record and field name duplication is not chec"
    "ked (\"xeona --mode 3\" will do that)\n"
     << ::tab()               << "'xemstub' must be in the form 'study.scenario"
    "' where 'scenario' is usually [+a-z]\n"

     << ::tab("output")       << "regular messages to 'stdout', debug messages "
    "to 'stderr'\n"

     << ::tab("exit codes")   << ::formatExitCode(ExitCodes::success)     << "\n"
     << ::tab()               << ::formatExitCode(ExitCodes::failure)     << "\n"
     << ::tab()               << ::formatExitCode(ExitCodes::usage)       << "\n"
     << ::tab()               << ::formatExitCode(ExitCodes::codingError) << "\n"
     << ::tab()               << ::formatExitCode(ExitCodes::noXem)       << "\n"
     << ::tab()               << ::formatExitCode(ExitCodes::mtXem)       << "\n"
     << ::tab()               << ::formatExitCode(ExitCodes::badXem)      << "\n"
     << ::tab()               << ::formatExitCode(ExitCodes::badRecKind)  << "\n"
     << ::tab()               << ::formatExitCode(ExitCodes::badFqf)      << "\n"
     << ::tab()               << ::formatExitCode(ExitCodes::noRec)       << "\n"
     << ::tab()               << ::formatExitCode(ExitCodes::noFld)       << "\n"
     << ::tab()               << ::formatExitCode(ExitCodes::mtFqf)       << "\n"
     << ::tab()               << ::formatExitCode(ExitCodes::noFqf)       << "\n"
     << ::tab()               << ::formatExitCode(ExitCodes::badExt)      << "\n"

     << ::tab("examples")     << "$ " << p << " --failmsg \"fail\"     "
    "run." << xemext << " \"entity.time-horizon.steps\"\n"
     << ::tab()               << "$ " << p << " --quiet              "
    "run." << xemext << " \"duh.duh.duh\" || " << p << " --exitcode $?\n"
     << ::tab()               << "$ " << p << " --quiet              "
    "raw." << xemext << " \"entity.overseer.total-financial\"\n"
     << ::tab()               << "$ " << p << " --wasrun             "
    "raw." << xemext << "\n"
     << ::tab()               << "$ " << p << " --unquote            "
    "run.xem \"program.r-processing.r-title\"\n"
     << ::tab()               << "$ " << p << " --unquote --summary  "
    "study.scenario.entity.time-horizon.steps\n"
     << ::tab()               << "$ " << p << " --jettison --xemstub "
    "run.xem\n";

  os << "\n";
}

// ---------------------------------------------------------
//  FREE FUNCTION   : ::formatSummarizeGen <>
// ---------------------------------------------------------
//  Description  : formatting utility
//  Role         : support for 'summarizeXem' and 'summarizeTarget'
//  Techniques   : function template, 'Boost.Format'
//  Status       : complete
// ---------------------------------------------------------

namespace
{
  template <typename T>
  void
  formatSummarizeGen
  (std::ostream&      os,
   const std::string& key,
   const T            value)
  {
    // debug reporting
    Deport::log(FUNC, 1, "general version", value);

    // stream the information
    os << std::boolalpha
       << "  "
       << boost::format("%-25s") % key
       << " : "
       << value
       << "\n";
  }

  template <>                                // specialized for 'std::string'
  void
  formatSummarizeGen
  (std::ostream&      os,
   const std::string& key,
   const std::string  value)
  {
    // debug reporting
    Deport::log(FUNC, 1, "string version", value);

      // stream the information
    os << std::boolalpha
       << "  "
       << boost::format("%-25s") % key
       << " : ";
    if ( Flags::unquote ) os << unquoteString(value);
    else                  os << value;
    os << "\n";
  }

  template <>                                // specialized for 'char*'
  void
  formatSummarizeGen
  (std::ostream&      os,
   const std::string& key,
   const char*        value)
  {
    // debug reporting
    Deport::log(FUNC, 1, "char* version", value);

      // stream the information
    os << std::boolalpha
       << "  "
       << boost::format("%-25s") % key
       << " : ";
    if ( Flags::unquote ) os << unquoteString(value);
    else                  os << value;
    os << "\n";
  }
}

// EXPLICIT TEMPLATE INSTANTIATIONS

namespace
{
  template void formatSummarizeGen(std::ostream&, const std::string&, const bool);
  template void formatSummarizeGen(std::ostream&, const std::string&, const double);
  template void formatSummarizeGen(std::ostream&, const std::string&, const int);
}

// ---------------------------------------------------------
//  FREE FUNCTION   : ::formatSummarizeXem
// ---------------------------------------------------------
//  Description  : formatting utility
//  Role         : support for 'summarizeXem'
//  Techniques   : 'Boost.Format'
//  Status       : complete
// ---------------------------------------------------------

namespace
{
  void
  formatSummarizeXem
  (std::ostream&      os,
   const RecordSet&   xem,
   const std::string& fqfname)
  {
    // split on dot separator and check integrity
    std::vector<std::string> output;
    boost::split(output, fqfname, boost::is_any_of("."));
    const int split = output.size();
    if ( split != 3 ) return;
    std::string key = output.back();
    // hunt for value
    std::string value = "";
    try
      {
        value = xem.value(fqfname);
      }
    catch ( const noRec& ex )
      {
        Deport::log(FUNC, 1, "noRec exception caught (see next)");
      }
    catch ( const noFld& ex )
      {
        Deport::log(FUNC, 1, "noFld exception caught (see next)");
      }

    // stream the information
    os << "  "
       << boost::format("%-25s") % key
       << " : ";
    if ( Flags::unquote ) os << unquoteString(value);
    else                  os << value;
    os << "\n";
  }
}

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
 const std::string& xemfile)
{
  formatSummarizeGen(os, "xemfile", xemfile);
  ::formatSummarizeXem(os, xem, "program.study-description.study-name");
  ::formatSummarizeXem(os, xem, "program.study-description.scenario-name");
  ::formatSummarizeXem(os, xem, "program.study-description.scenario-author");
  ::formatSummarizeXem(os, xem, "program.study-description.scenario-leta");
  ::formatSummarizeXem(os, xem, "entity.time-horizon.steps");
  ::formatSummarizeXem(os, xem, "program.last-run.timestamp-start");
  formatSummarizeGen(os, "was-run",        (xem.wasRun() ? "yes" : "no"));
  formatSummarizeGen(os, "entity count",   xem.entities());
  formatSummarizeGen(os, "in-data count",  xem.inDatas());
  formatSummarizeGen(os, "out-data count", xem.outDatas());
}

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
 const std::string& target)
{
  formatSummarizeGen(os, "fully-qualified target", target);
  formatSummarizeGen(os, "field identifier",       field->identifier());
  formatSummarizeGen(os, "field kind",             Field::interpretKind(field->kind()));
  formatSummarizeGen(os, "units",                  field->units());
  formatSummarizeGen(os, "value",                  field->value());
  formatSummarizeGen(os, "length",                 field->length());
  formatSummarizeGen(os, "finalized",              (field->finalized() ? "yes" : "no"));
}

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
 const std::string& code)
{
  Deport::log(FUNC, 1, "code as string", code);
  int i = -1;                                // nonsensical value
  try
    {
      i =  boost::lexical_cast<int>(code);
      Deport::log(FUNC, 1, "boost::lexical_cast<int> success");
      Deport::log(FUNC, 1, "code as int", i);
    }
  catch( const boost::bad_lexical_cast& e )
    {
      Deport::log(FUNC, 3, "boost::lexical_cast<int> fail");
      return false;
    }
  const std::string exitcode = ExitCodes::interpretExitCode(i);
  os << i << " = " <<  exitcode << std::endl;
  return true;
}

// ---------------------------------------------------------
//  FREE FUNCTION   : displayLegal
// ---------------------------------------------------------

void
displayLegal
(std::ostream&      os,
 const std::string& copyrightYears)
{
  os << "software: extract information from xeona model files"                   << "\n";
  os << "notice: distributed under the GNU General Public License version 3 and" << "\n"
     << "without warranty to the extent permitted by applicable law"             << "\n";
  os << "copyright: (c) " << copyrightYears << " Robbie Morrison"                << "\n";
}

// ---------------------------------------------------------
//  FREE FUNCTION   : displayVersion
// ---------------------------------------------------------

void
displayVersion
(std::ostream&      os,
 const std::string& version)
{
  os << version << "\n";
}

// ---------------------------------------------------------
//  FREE FUNCTION   : dumpXem
// ---------------------------------------------------------

int
dumpXem
(std::ostream&      os,
 RecordSet          xem,
 const bool         indata,
 const bool         outdata,
 const std::string& prepend)
{
  std::vector<std::string> data;
  dump(data, xem, indata, outdata, true, prepend);     // free function (see below)
  Deport::log(FUNC, 1, "about to print dumped xem file");
  std::copy(data.begin(), data.end(), std::ostream_iterator<std::string>(os, "\n"));
  return data.size();
}

// ---------------------------------------------------------
//  FREE FUNCTION   : dumpXemNames
// ---------------------------------------------------------

int
dumpXemNames
(std::ostream&      os,
 RecordSet          xem,
 const bool         indata,
 const bool         outdata,
 const std::string& prepend)
{
  std::vector<std::string> data;
  dump(data, xem, indata, outdata, false, prepend);    // free function (see below)
  Deport::log(FUNC, 1, "about to print dumped xem file");
  std::copy(data.begin(), data.end(), std::ostream_iterator<std::string>(os, "\n"));
  return data.size();
}

// ---------------------------------------------------------
//  FREE FUNCTION   : dump
// ---------------------------------------------------------

int
dump
(std::vector<std::string>& data,
 RecordSet                 xem,
 const bool                indata,           // 'false' is omit in-data
 const bool                outdata,          // 'false' is omit out-data
 const bool                entire,           // 'false' is omit field kinds and values
 const std::string&        prepend)          // prepend string, can be ""
{
  Deport::log(FUNC, 1, "about to dump xem file");

  Field::Kind fldkind = Field::e_notKnown;
  std::string recKind;
  std::string recId;
  std::string fldId;
  std::string fqf;
  std::string fldKind;
  std::string val;
  std::string buf;

  BOOST_FOREACH( shared_ptr<Record> rec, xem.d_records )
    {
      recKind = Record::interpretKind(rec->kind());
      recId   = rec->identifier();

      BOOST_FOREACH( shared_ptr<Field> fld, rec->d_fields )
        {
          fldId   = fld->identifier();
          fldkind = fld->kind();
          if ( fldkind == Field::e_inData  && indata  == false ) continue;
          if ( fldkind == Field::e_outData && outdata == false ) continue;
          fldKind = Field::interpretKind(fldkind);
          val     = fld->value();
          fqf     = "";                                      // reset
          if ( ! prepend.empty() ) fqf += prepend + ".";     // extended-fqf form
          fqf    += recKind + "." +recId + "." + fldId;
          if ( entire ) buf = boost::str(boost::format("%s %s %s") % fqf % fldKind % val);
          else          buf = boost::str(boost::format("%s") % fqf);
          data.push_back(buf);
        }
    }
  return data.size();
}

// ---------------------------------------------------------
//  FREE FUNCTION   : rejig
// ---------------------------------------------------------

bool                                         // 'true' if altered, else 'false'
rejig
(std::string&      arg1,                     // may become filename
 std::string&      arg2,                     // may become target
 const std::string ext)                      // file extension
{
  // preamble
  const std::string sep = ".";               // separator dot

  // integrity checks
  int errors = 0;
  if ( arg1.empty() )                        // 'arg1' must not be empty to proceed
    {
      Deport::log(FUNC, 1, "arg 1 empty", arg1);
      ++errors;
    }
  if ( ! arg2.empty() )                      // 'arg2' must be empty to proceed
    {
      Deport::log(FUNC, 1, "arg 2 not empty", arg2);
      ++errors;
    }
  if ( errors == 0 )
    {
      Deport::log(FUNC, 1, "input errors", errors);
    }
  else
    {
      Deport::log(FUNC, 1, "input errors", errors);
      Deport::log(FUNC, 1, "no action taken");
      return false;
    }

  // active code
  std::vector<std::string> output;
  boost::split(output, arg1, boost::is_any_of(sep));
  const unsigned size = output.size();
  Deport::log(FUNC, 1, "split size", size);
  switch ( size )
    {
    case 5:
      {
        arg1 = output.at(0) + sep + output.at(1) + "." + ext;
        arg2 = output.at(2) + sep + output.at(3) + sep + output.at(4);
        Deport::log(FUNC, 1, "action taken");
      }
      return true;
    default:
      Deport::log(FUNC, 1, "no action taken", "split not 5-way");
      return false;
    }
}

// ---------------------------------------------------------
//  FREE FUNCTION   : removeFileExt
// ---------------------------------------------------------

//  note also the following STL code if not fussy about 'ext'
//
//    std::string::size_type idx = filename.rfind('.');
//    if ( idx != std::string::npos ) std::string extension = filename.substr(idx + 1);

bool                                         // 'true'if altered, else 'false'
removeFileExt
(std::string&       filename,                // filename to alter
 const std::string& ext)                     // extension without dot
{
  const std::string extn = "." + ext;        // prepend a dot
  if ( boost::ends_with(filename, extn) )
    {
      const int len = filename.length() - extn.length();
      filename = filename.substr(0, len);
      return true;
    }
  else
    {
      return false;
    }
}

//  end of file

