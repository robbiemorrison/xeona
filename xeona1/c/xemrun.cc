//  XEONA ENERGY SYSTEMS MODELING ENVIRONMENT
//
//  file-create-name : xemrun.cc
//  file-create-date : Tue 12-Aug-2008 13:31 UTC
//  file-initiator   : Robbie Morrison <robbie@actrix.co.nz>
//
//  file-role        : class to write and run XEM unit test models / implementation
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
//  $URL: file:///home/robbie/svn-root/xeona/futz/trunk/xeona1/c/xemrun.cc $
//
//  GENERAL NOTES FOR THIS FILE
//
//  This file contains UNIX-specific code -- which could be
//  portable to non-UNIX POSIX-compliant systems, although this
//  has not been confirmed.

//  LOCAL AND SYSTEM INCLUDES

#include "xemrun.h"           // companion header for this file (place first)

#include "../a/logger.h"      // standard logging functionality (as required)
#include "../c/smart_ptr.h"   // toggle between Boost and TR1 smart pointers
#include ".././common.h"      // common definitions for project (place last)

#include <fstream>       // file-based io
#include <iomanip>       // setw() and family
#include <iostream>      // standard io
#include <sstream>       // string-streams
#include <string>        // C++ strings

#include <cerrno>        // C-style error codes, errno object
#include <cstdlib>       // C-style exit(), getenv(), system(), NULL, EXIT_SUCCESS
#include <cstring>       // C-style string handing, strcpy(), basename(), strerror(errno)
#include <sys/stat.h>    // C-style POSIX file characteristics, stat()
#include <unistd.h>      // POSIX sleep(), usleep(), access(), chown()

#include <boost/filesystem.hpp>         // path objects, iterators, useful operations

//  CODE

// ---------------------------------------------------------
//  FILE-LOCAL VAR  : ::rule
// ---------------------------------------------------------

namespace
{
  const std::string rule(20, '=');           // to demarcate output
} // unnamed namespace

// ---------------------------------------------------------
//  CLASS           : XemRun
// ---------------------------------------------------------
//  Description  : write XEM string to file and call nominated 'xeona' binary
//  Role         : unit test support
//  On fail      : some fault tolerance provided
//  Techniques   : Boost.Filesystem library
//  Status       : complete
//
//  Design notes
//
//      The header notes discuss usage, whereas these notes
//      discuss implementation.
//
//      Standard Library <fstream> calls were used, although the
//      Boost.Filesystem header <boost/filesystem/fstream.hpp>
//      provides equivalent equivalent calls taking path objects
//      (as opposed to 'const char*'s).
//
//  CAUTION: portability and robustness
//
//      This code has not been written to be especially portable
//      beyond 'Linux'.  Nor to be especially robust in relation to
//      exception states.
//
//  References
//
//      Boost.Filesystem documentation.
//
//      Lischner, Ray.  2003.  C++ in a nutshell : a language and
//        library reference.  O'Reilly and Associates, Sebastopol,
//        California, USA.  ISBN 0-596-00298-X.
//
// ---------------------------------------------------------

// STATIC DEFINITIONS

logga::spLogger XemRun::s_logger = logga::ptrLogStream();  // bind logger

// CREATORS

XemRun::XemRun
(std::string file) :                         // the intention is to call with __FILE__
  d_thisFile(file),
  d_binary(),
  d_xem(),
  d_options(),
  d_call()
{
  // the following is very unlikely to fail but it is still worth
  // testing for and reporting on

  if ( system(NULL) == 0 )
    s_logger->repx(logga::warn, "command interpreter not available", "");
  else
    s_logger->repx(logga::dbug, "command interpreter available", "");
}

XemRun::~XemRun()
{
  s_logger->repx(logga::dbug, "destructor call", "");
  std::ostringstream put;

  std::string xemName_wiggle = d_xem.string() + xeona::backupTag;
  boost::filesystem::path xem_wiggle(xemName_wiggle);

  try
    {
      boost::filesystem::remove(d_xem);
    }
  catch( const std::exception& e )           // [1] CAUTION
    {
      put << "  cannot delete file, what: " << e.what() << ": " << d_xem << "\n";
      s_logger->putx(logga::warn, put);
    }
  // [1] CAUTION: 'boost::filesystem::basic_filesystem_error'
  // would not work -- "file not present" does not 'throw' and an
  // empty path (!p.empty()) is supposed to 'throw' but does not
  // on my system.

  try
    {
      boost::filesystem::remove(xem_wiggle);
    }
  catch( const std::exception& e )       // [1]
    {
      put << "  cannot delete file, what: " << e.what() << ": " << xem_wiggle << "\n";
      s_logger->putx(logga::warn, put);
    }
}

// MANIPULATORS

bool
XemRun::setBinary
(std::string binaryName,
 std::string localDirName,                   // omit trailing digit
 std::string baseDirName)
{
  // CAUTION: this first part uses the Boost.Filesystem library
  // and should be fully portable -- given the presence of
  // suitable Boost run-time support!

  std::ostringstream put;

  // overwrite relevant data member with empty path

  boost::filesystem::path empty;
  d_binary = empty;

  // search for a suitable binary based on either supplied or
  // default arguments

  boost::filesystem::path local;                  // local directory with trailing digit
  boost::filesystem::path base(baseDirName);      // base directory
  boost::filesystem::path dir;                    // potential directory path for binary
  boost::filesystem::path leaf(binaryName);       // binary leaf
  boost::filesystem::path fullBinary;             // complete path for binary

  // search for suitable directory using the so-called trailing
  // digit format

  for (int i = 9; i > 0; --i)                // count backwards from "localDirName9"
    {
      std::ostringstream oss;
      oss << localDirName << i;
      local = boost::filesystem::path(oss.str());
      dir   = boost::filesystem::path(base/local);     // concatenate with 'operator/'
      if ( boost::filesystem::is_directory(dir) )
        break;                                         // sought directory found
    }
  if ( ! boost::filesystem::is_directory(dir) )        // a repeat test is necessary
    {
      put << "  directory not found, final try: " << dir << "\n";
      s_logger->putx(logga::warn, put);
      return false;                                    // binary remains empty
    }

  // test for presence of binary

  fullBinary = boost::filesystem::path(dir/leaf);      // concatenate with 'operator/'
  if ( ! boost::filesystem::exists(fullBinary) )
    {
      put << "  binary not found: " << fullBinary << "\n";
      s_logger->putx(logga::warn, put);
      return false;                                    // binary remains empty
    }

  put << "  complete binary name : " << fullBinary << "\n";
  s_logger->putx(logga::dbug, put);

  // CAUTION: the remaining code uses the C run-time function
  // 'stat' from <sys/stat.h>, which not all compilers supply

  const char* cbinary = fullBinary.string().c_str();
  struct stat statbuf;
  if ( stat(cbinary, &statbuf) == -1 )
    {
      put << "  'stat' call from <sys/stat.h> failed"
          << ": errno reports: " << strerror(errno) << "\n";
      s_logger->putx(logga::warn, put);
      return false;
    }

  if ( (statbuf.st_mode & S_IXUSR) == 0 )    // bitwise logical and, requires integer
    {
      std::ostringstream oss;
      oss << std::oct << statbuf.st_mode;    // note octal conversion
      put << "  file not executable, octal mode: '" << oss.str() << "'" << "\n";
      s_logger->putx(logga::dbug, put);
      return false;
    }

  // success

  d_binary = fullBinary;                     // update data member
  std::string leafName = d_binary.filename().string();
  s_logger->repx(logga::dbug, "binary present and user executable", leafName);
  return true;
}

std::string
XemRun::setOptions
(const std::string options)
{
  d_options = options;
  return d_options;
}

std::string
XemRun::write
(const std::string& model)
{
  makeXemPath();                             // sets 'd_xem'

  const char* cxemName = d_xem.string().c_str();
  std::ofstream o;
  o.open(cxemName, std::ios_base::out | std::ios_base::trunc);
  if ( o )                                   // write-only truncate open succeeded
    {
      o << model;                            // add model
      o.close();
    }

  return d_xem.string();
}

std::string
XemRun::run
(std::string options)
{
  std::ostringstream put;

  if ( d_xem.empty() )
    {
      s_logger->repx(logga::warn, "refusing to run, model path empty", "");
      put << "  'XemRun::write' call probably omitted" << "\n";
      s_logger->putx(logga::warn, put);
      s_logger->flush();
      return "";
    }

  std::string call;
  call += d_binary.string();                 // regular file OS-specific format string
  call += " ";
  call += d_options;                         // 'xeona' options
  call += " ";
  call += d_xem.string();                    // XEM model
  d_call = call;

  put << "  call : " << d_call << "\n";
  s_logger->putx(logga::dbug, put);

#if 1 // 0 = disable system call, 1 = enable

  s_logger->addDumbBlank();
  std::clog << ::rule << std::endl;
  const int ret = system(d_call.c_str());    // const char* command
  std::clog << ::rule << std::endl;
  s_logger->addDumbBlank();
  if ( ret != 0 )
    {
      put << "  'system' returned fail: " << ret << "\n";
      s_logger->putx(logga::warn, put);
    }

#else

  s_logger->addDumbBlank();
  std::clog << ::rule << std::endl;
  std::clog << ::rule << std::endl;
  s_logger->addDumbBlank();

#endif // 0

  return d_call;
}

// UTILITY FUNCTIONS

void
XemRun::makeXemPath()
{
  boost::filesystem::path cwd;               // current path
  boost::filesystem::path leaf;              // xem leaf

  // obtain file stub
  std::string stub = d_thisFile;             // works with empty string too
  std::string::size_type idx;                // CAUTION: do not use 'int' or 'unsigned'
  idx = stub.find(".");                      // forward find, 'idx' is zero-based
  if ( idx != std::string::npos ) stub.erase(idx, stub.length());

  // obtain process id
  const int padlevel = 6;
  std::ostringstream pid;
  pid << std::setw(padlevel) << std::setfill('0')
      << getpid();                           // refer <unistd.h>
                                             // returns 'pid_t', currently 'unsigned'

  // assemble XEM
  std::string xemName;
  xemName += stub;
  xemName += "-";                            // separator string
  xemName += pid.str();
  xemName += xeona::modelExt;                // includes the "dot"
  leaf = boost::filesystem::path(xemName);

  // grab current directory
  cwd = boost::filesystem::current_path();

  // make and store path
  d_xem = boost::filesystem::path(cwd/leaf);
}

void
XemRun::dump
(std::ostream& os,
 const int     blankLines)
{
  s_logger->flush();

  const char* cxemName = d_xem.string().c_str();
  std::ifstream i;
  i.open(cxemName, std::ios_base::in);
  if ( i )                                   // read-only open succeeded
    {
      os << ::rule << "\n" << std::string(blankLines, '\n');
      os << i.rdbuf();                // stream model, see Lischner (2003 p238)
      os << ::rule << "\n" << std::string(blankLines, '\n');
      os << std::flush;
      i.close();
    }
  else
    {
      s_logger->repx(logga::warn, "file open failed", cxemName);
    }
  s_logger->flush();

}

//  end of file

