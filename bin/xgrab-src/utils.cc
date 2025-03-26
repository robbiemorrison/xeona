//  XEONA ENERGY SYSTEMS MODELING ENVIRONMENT
//
//  file-create-name : utils.cc
//  file-create-date : Mon 07-Nov-2011 10:21 UTC
//  file-initiator   : Robbie Morrison <robbie@actrix.co.nz>
//
//  file-role        : utilities including fault handling / implementation
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
//  $URL: file:///home/robbie/svn-root/xeona/futz/trunk/bin/xgrab-src/utils.cc $
//
//  GENERAL NOTES FOR THIS FILE

//  LOCAL AND SYSTEM INCLUDES

#include "utils.h"            // companion header for this file (place first)

#include "common.h"           // common definitions for project (place last)

#include <sstream>            // string-streams

#include <boost/format.hpp>             // printf style formatting
#include <boost/algorithm/string.hpp>   // string recasing, trimming, splitting

//  CODE

// ---------------------------------------------------------
//  CLASS           : ExitCodes
// ---------------------------------------------------------

// ---------------------------------------------------------
//  MEMBER FUNCTION : ExitCodes::interpretExitCode (static)
// ---------------------------------------------------------
//  Description  : interpret given exit code
//  Role         : support for options '--help' and '--exitcodes'
//  Techniques   : note overloaded semantics for 0,1
//  Status       : complete
// ---------------------------------------------------------

std::string
ExitCodes::interpretExitCode
(const int code)
{
  Deport::log(FUNC, 0, "code", code);
  switch ( code )
    {
      // my exit codes
    case 0:           return "success / XEM model was run (--wasrun)";
    case 1:           return "generic failure / XEM model was not run (--wasrun)";
    case usage:       return "invalid command-line usage";
    case codingError: return "probable coding error";
    case noXem:       return "unable to open XEM file";
    case mtXem:       return "no lines read from XEM file";
    case badXem:      return "XEM file parse failures";
    case badRecKind:  return "invalid record kind (not 'program' or 'entity')";
    case badFqf:      return "sought target badly formed (two dots expected)";
    case noRec:       return "sought record not found";
    case noFqf:       return "sought target not found";
    case mtFqf:       return "attempt to search for out-data in unrun model";
    case noFld:       return "sought field not found";
    case badExt:      return "invalid file extension";
      // POSIX exit codes
    case 130:         return "user-initiated Ctrl-C SIGINT interrupt (not trapped)";
    case 134:         return "SIGABRT abort from library call or uncaught throw";
    case 137:         return "user-initiated SIGKILL kill (not trapped)";
    case 139:         return" invalid memory use causing a SIGSEGV segmentation fault";
    case 143:         return "user-initiated SIGTERM kill (not trapped)";
      // default action
    default: return "(exit code not supported)";  // could perhaps throw custom exception
    }
}

// ---------------------------------------------------------
//  CLASS           : Deport
// ---------------------------------------------------------

const std::string Deport::s_intro = "  debug  ";
int               Deport::s_count = 0;

template <typename T>
void Deport::log(const std::string  func,
                 const int          level,
                 const std::string& key,
                 const T            value)
{
  if ( level < 0 || level > 3 ) throw codingError(FUNC, "level not 0,1,2,3");
  std::ostringstream oss;
  oss << boost::format("%-16s") % key << " : " << value;    // set the width
  Deport::log(func, level, oss.str());                      // underlying call
}

void Deport::log(const std::string  func,
                 const int          level,
                 const std::string& msg)
{
  if ( level < 0 || level > 3 ) throw codingError(FUNC, "level not 0,1,2,3");
  if ( level == 0 ) return;                  // level zero to disable output
  if ( Flags::debug )
    {
      std::string modfunc(func);
#if 1 // 0 = no func name processing, 1 = compress extended  __PRETTY_FUNCTION__ output
      const std::string bad1  = "std::basic_string<char, std::char_traits<char>, "
        "std::allocator<char> >, std::allocator<std::basic_string<char, "
        "std::char_traits<char>, std::allocator<char> > >";
      const std::string good1 = "std::string";
      const std::string bad2  = "<std::string >";
      const std::string good2 = "<std::string>";
      const std::string bad3  = "<unnamed>";
      const std::string good3 = " ";
      boost::replace_all(modfunc, bad1, good1);
      boost::replace_all(modfunc, bad2, good2);
      boost::replace_all(modfunc, bad3, good3);
#endif // 0

      std::cerr << s_intro
                <<        boost::format("%02d") % ++s_count
                << " " << Deport::interpretLevel(level)
                << " " << boost::format(" %-40s") % msg
                << " " << modfunc
                << std::endl;
    }
}

std::string Deport::interpretLevel(const int level)
{
  switch ( level )
    {
    case 1: return " ";
    case 2: return "~";
    case 3: return ">";
    default: throw codingError(FUNC, "case not handled");
    }
}

// EXPLICIT TEMPLATE INSTANTIATIONS

// the "const char*" variant is required for string literals

#define string std::string                      // for convenience

template void Deport::log(const string, const int, const string& key, const bool);
template void Deport::log(const string, const int, const string& key, const string);
template void Deport::log(const string, const int, const string& key, const char*);
template void Deport::log(const string, const int, const string& key, const int);
template void Deport::log(const string, const int, const string& key, const unsigned);
template void Deport::log(const string, const int, const string& key, const double);

#undef string

// ---------------------------------------------------------
//  CLASS           : Flags
// ---------------------------------------------------------

bool        Flags::debug    = false;
bool        Flags::failuse  = false;
std::string Flags::failmsg  = "";
bool        Flags::indata   = true;
bool        Flags::jettison = false;
bool        Flags::kind     = false;
bool        Flags::names    = false;
bool        Flags::outdata  = true;
bool        Flags::quiet    = false;
bool        Flags::summary  = false;
bool        Flags::units    = false;
bool        Flags::unquote  = false;
bool        Flags::wasrun   = false;
bool        Flags::xemstub  = false;
int         Flags::freeArgs = -1;

// ---------------------------------------------------------
//  FREE FUNCTION   : unquoteString
// ---------------------------------------------------------

std::string
unquoteString
(const std::string& value)
{
  std::string buffer(value);
  if ( boost::starts_with(value, "\"") && boost::ends_with(value, "\"") )
    {
#if 0 // 0 = turn off (often interrupts 'std::cout' stream)
      Deport::log(FUNC, 1, "enclosing quotes identified");
#endif // 0
      return buffer.substr(1, buffer.length() - 2); // takes: zero-based start, length
    }
  else
    {
      return buffer;
    }
}

// ---------------------------------------------------------
//  CLASS           : localException (abstract base)
// ---------------------------------------------------------

std::string localException::s_program = "(coding error)";

localException::localException
(const std::string func,
 const int         exitcode) :
  d_code(exitcode),
  d_tell("(coding error)")
{
  std::cout << std::flush;
  std::clog << std::flush;

  const std::string codeInt = ExitCodes::interpretExitCode(d_code);

  std::stringstream oss;
  oss << s_program << ": custom exception thrown by: " << func      << "\n";
  oss << s_program << ": will exit: " << d_code << " = " << codeInt << "\n";
  d_tell = oss.str();
}

localException::~localException()
{
}

const int
localException::code() const
{
  return d_code;
}

const std::string
localException::tell() const
{
  return d_tell;
}

void
localException::setProgram
(const std::string& program)
{
  s_program = program;
}

// ---------------------------------------------------------
//  CLASS           : codingError (coding error)
// ---------------------------------------------------------

codingError::codingError
(const std::string func,
 const std::string explanation) :
  localException(func,
                 ExitCodes::codingError)
{
  std::ostringstream oss;
  if ( explanation.empty() ) oss << s_program << ": coding error" << "\n";
  else                       oss << s_program << ": coding error: " << explanation <<"\n";
  d_tell += oss.str();
}

// ---------------------------------------------------------
//  CLASS           : noXem (cannot open XEM file)
// ---------------------------------------------------------

noXem::noXem
(const std::string func,
 const std::string xemfile) :
  localException(func,
                 ExitCodes::noXem)
{
  std::ostringstream oss;
  oss << s_program << ": cannot open xem file: '" << xemfile << "'" << "\n";
  d_tell += oss.str();
}

// ---------------------------------------------------------
//  CLASS           : mtXem (empty XEM file)
// ---------------------------------------------------------

mtXem::mtXem
(const std::string func,
 const std::string xemfile) :
  localException(func,
                 ExitCodes::mtXem)
{
  std::ostringstream oss;
  oss << s_program << ": no lines read from xem file: '" << xemfile << "'" << "\n";
  d_tell += oss.str();
}

// ---------------------------------------------------------
//  CLASS           : badXem (XEM parse failure)
// ---------------------------------------------------------

badXem::badXem
(const std::string func,
 const std::string explanation) :
  localException(func,
                 ExitCodes::badXem)
{
  std::ostringstream oss;
  oss << s_program << ": parse failure on xem file"                      << "\n";
  if ( ! explanation.empty() ) oss << s_program << ": " << explanation   << "\n";
  d_tell += oss.str();
}

// ---------------------------------------------------------
//  CLASS           : badRecKind (record kind find failure)
// ---------------------------------------------------------

badRecKind::badRecKind
(const std::string func,
 const std::string recKind) :
  localException(func,
                 ExitCodes::badRecKind)
{
  std::ostringstream oss;
  oss << s_program << ": sought record kind invalid (not 'program' or 'entity'): '"
      << recKind << "'" << "\n";
  d_tell += oss.str();
}

// ---------------------------------------------------------
//  CLASS           : noRec (record find failure)
// ---------------------------------------------------------

noRec::noRec
(const std::string func,
 const std::string rec) :
  localException(func,
                 ExitCodes::noRec)
{
  std::ostringstream oss;
  oss << s_program << ": sought record not found: '" << rec << "'" << "\n";
  d_tell += oss.str();
}

// ---------------------------------------------------------
//  CLASS           : noFld (field find failure)
// ---------------------------------------------------------

noFld::noFld
(const std::string func,
 const std::string fld) :
  localException(func,
                 ExitCodes::noFld)
{
  std::ostringstream oss;
  oss << s_program << ": sought field not found: '" << fld << "'" << "\n";
  d_tell += oss.str();
}

// ---------------------------------------------------------
//  CLASS           : noFqf (target find failure)
// ---------------------------------------------------------

noFqf::noFqf
(const std::string func,
 const std::string fqf) :
  localException(func,
                 ExitCodes::noFqf)
{
  std::ostringstream oss;
  oss << s_program << ": sought identifier not found: '" << fqf << "'" << "\n";
  d_tell += oss.str();
}

// ---------------------------------------------------------
//  CLASS           : mtFqf (target empty)
// ---------------------------------------------------------

mtFqf::mtFqf
(const std::string func,
 const std::string fqf) :
  localException(func,
                 ExitCodes::mtFqf)
{
  std::ostringstream oss;
  oss << s_program << ": in relation to field: '" << fqf << "'" << "\n";
  d_tell += oss.str();
}

// ---------------------------------------------------------
//  CLASS           : badFqf (badly formed target)
// ---------------------------------------------------------

badFqf::badFqf
(const std::string func,
 const std::string fqf,
 const std::string explanation) :
  localException(func,
                 ExitCodes::badFqf)
{
  std::ostringstream oss;
  oss << s_program << ": badly formed field sought: '" << fqf << "'"   << "\n";
  if ( ! explanation.empty() ) oss << s_program << ": " << explanation << "\n";
  d_tell += oss.str();
}

// ---------------------------------------------------------
//  CLASS           : badArg (invalid argument)
// ---------------------------------------------------------

badArg::badArg
(const std::string func,
 const std::string arg):
  localException(func,
                 ExitCodes::usage)
{
  std::ostringstream oss;
  oss << s_program << ": invalid argument (try --help): '" << arg << "'" << "\n";
  d_tell = oss.str();                        // CAUTION: not "+=" in this case

}

// ---------------------------------------------------------
//  CLASS           : badExt (invalid file extension)
// ---------------------------------------------------------

badExt::badExt
(const std::string func,
 const std::string ext):
  localException(func,
                 ExitCodes::badExt)
{
  std::ostringstream oss;
  oss << s_program << ": file extension not '" << ext << "' as required" << "\n";
  d_tell += oss.str();

}

//  end of file

