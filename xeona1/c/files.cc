//  XEONA ENERGY SYSTEMS MODELING ENVIRONMENT
//
//  file-create-name : files.cc
//  file-create-date : Fri 09-Nov-2007 13:38 UTC
//  file-initiator   : Robbie Morrison <robbie@actrix.co.nz>
//
//  file-role        : free functions for regular files / implementation
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
//  $URL: file:///home/robbie/svn-root/xeona/futz/trunk/xeona1/c/files.cc $

//  AD-HOC NOTES
//
//  Boost.Filesystem
//
//      Here are some Boost.Filesystem portability issues which
//      were not covered well in the Boost documentation at the
//      time of coding.
//
//  Path stringification functions
//
//      string()
//
//          returns a POSIX compliant string with no distinction
//          between regular files and directories -- and (for
//          most normal characters, including " " and "\\") no
//          different from native Linux
//
//      file_string()
//
//          returns an operating system format string for regular
//          files
//
//      directory_string()
//
//          returns an operating system format string for
//          directories -- noticeable only on systems (VMS but
//          not Linux or Windows) for which the formatting rules
//          for regular files and directories differ
//
//      stream insertion via operator<<
//
//          stream insertion mimics string()
//
//      native_*_string()
//
//          depreciated
//
//  Path completion functions
//
//      complete()
//
//          assumes POSIX compliant input
//
//      system_complete()
//
//          assumes operating system format input
//
//  Portability conclusions
//
//      input : all input assumes POSIX path conventions
//
//          complete() is used, even where user input applies,
//          which implies user input should be POSIX compliant
//
//      reporting : all reporting uses OS path conventions
//
//          file_string() is used to stringify paths for
//          reporting (unless local circumstances dictate
//          otherwise)
//
// ---------------------------------------------------------

//  LOCAL AND SYSTEM INCLUDES

#include "files.h"            // companion header for this file (place first)

#include "../a/logger.h"      // standard logging functionality (as required)
#include "../c/smart_ptr.h"   // toggle between Boost and TR1 smart pointers
#include ".././common.h"      // common definitions for project (place last)

#include <fstream>            // file-based io
#include <iomanip>            // setw() and family
#include <iostream>           // standard io
#include <sstream>            // string-streams
#include <string>             // C++ strings

#include <cerrno>             // C-style error codes, errno object

#include <sys/stat.h>         // C-style POSIX file characteristics, stat()
#include <sys/types.h>        // C-style POSIX primitive system data types

#include <boost/algorithm/string.hpp>   // string recasing, trimming, splitting
#include <boost/any.hpp>                // type heterogeneous storage
#include <boost/cast.hpp>               // numeric_cast<> number to number conversions
#include <boost/format.hpp>             // printf style formatting
#include <boost/lexical_cast.hpp>       // lexical_cast<> string to number conversions

//  CODE

namespace xeona
{
  // ---------------------------------------------------------
  //  FREE FUNCTION   : xeona::testWritable
  // ---------------------------------------------------------
  //  Description  : check if a given Boost.Filesystem path is writable
  //  Techniques   : relies exclusively on C++ Standard Library calls
  //  Status       : complete
  //
  //  Design notes
  //
  //      This function uses only C++ Standard Library calls from
  //      <fstream>.  It does not, for instance, rely on:
  //
  //          * the C run-time function stat() from <sys/stat.h>,
  //            which not all compilers supply
  //
  //          * the Boost.Filesystem library, which currently has
  //            no "boost::filesystem::is_writable()" function anyway
  //
  //          * the Unix-specific access() function from <unistd.h>
  //
  //          * OS-specific support from Visual C++ and other
  //            dedicated Windows compilers
  //
  //      Lischner (2003), as always, is good on the specifics of
  //      Standard Library headers.
  //
  //  References
  //
  //      Lischner, Ray.  2003.  C++ in a nutshell : a language and
  //        library reference, O'Reilly and Associates, Sebastopol,
  //        California, USA.  ISBN 0-596-00298-X.
  //
  // ---------------------------------------------------------

  tribool                                    // indeterminate means status not resolved
  testWritable
  (boost::filesystem::path regularFile)      // this function assumes the file is regular
  {
    // bind logger object
    // static logga::spLogger logger = logga::ptrLogStream();

    // recover path name
    std::string pathname  = regularFile.string();      // use native format
    const char* cpathname = pathname.c_str();          // C-style string

    // some subtle logic is required for the writability test
    // because first the 'ifstream' call and then the 'ofstream'
    // call must succeed -- the 'ifstream' test is required
    // because it fails because the file in question is absent,
    // the 'ofstream' test will simply create the missing file
    // and return success (not what is wanted)

    tribool writable = false;                // set default return variable
    std::ifstream i;
    i.open(cpathname, std::ios_base::binary | std::ios_base::in);
    if ( i )                                 // read-only binary open succeeded
      {
        i.close();
        std::ofstream o;
        o.open(cpathname, std::ios::binary | std::ios::out | std::ios::app); // [1]
        if ( o )                             // write-only binary append open succeeded
          {
            o.close();
            writable = true;
          }
      }

    // CAUTION: [1] file open mode: ofstream "app" append mode is
    // essential, otherwise the default "trunc" truncate mode
    // will nuke the existing file

    return writable;
  }

  // ---------------------------------------------------------
  //  FREE FUNCTION   : xeona::establishModelFile
  // ---------------------------------------------------------
  //  Description  : build path and confirm model file or show failure
  //  Takes        : the model name or alternatively an empty string
  //  Status       : complete
  //
  //  Design notes
  //
  //      If passed an empty string, this function uses the
  //      built-in default name (model) from 'common.cc'
  //
  //      Takes this name and concatenates the built-in extension
  //      (.xem) from 'common.cc'.  And then converts this to a
  //      Boost.Filesystem path and, in the process, adds the
  //      initial directory from which the application was invoked.
  //
  //      Checks the path is a regular file.  Then calls
  //      'testWritable()' to confirm write status.  And finally
  //      backs up the file using the backup tag (~) set in
  //      'common.cc'.
  //
  //      Returns an empty path on failure, otherwise the actual
  //      path.  An empty path tests true with path.empty().
  //
  //      Regarding terminology, a "path" can be a regular file or
  //      a directory.  A "leaf" is the path with the parent
  //      directory and such omitted.  A file "extension" contains
  //      the dot as well, thereby allowing a trailing dot and no
  //      extension to be distinguished.  A "complete" path means
  //      an absolute path.
  //
  //  CAUTION: file path format conventions used in this code
  //
  //      input     : all input assumes POSIX path conventions
  //      reporting : all reporting uses OS path conventions
  //
  //  See also
  //
  //      Notes on Boost.Filesystem elsewhere in this file.
  //
  //  References
  //
  //      Boost.Filesystem documentation.
  //
  // ---------------------------------------------------------

  boost::filesystem::path                    // an empty path object indicates failure
  establishModelFile                         // build path and confirm model file
  (const std::string& modelStub)             // from command-line or "" to use default
  {
    static logga::spLogger logger = logga::ptrLogStream();
    logger->repx(logga::dbug, "entering free function", "");

    // CAUTION: advice on the hardcoding of paths (including
    // incomplete paths): the solution adopted here is to use legal
    // Linux pathnames and avoid the use of escaped characters --
    // these paths (and subpaths), then, will be automatically
    // POSIX conforming.

    // adopt a simplified namespace alias for convenience
    namespace fs = boost::filesystem;

    // create empty return object in the event of failure, thus empty.empty() returns true
    fs::path empty;        // default (zero-argument) construction creates an empty object

    // use the default model name from 'common.cc' if an empty string was passed
    std::string modelName;
    if ( modelStub.empty() )                 // 'modelStub' is a function argument
      modelName = xeona::modelStubDefault;   // set in 'common.cc'
    else
      modelName = modelStub;

    // form the leaf names for the model file and the backup file
    std::string modelLeaf;
    std::string backupLeaf;
    modelLeaf   = modelName;                 // say : subdir/model2
    modelLeaf  += xeona::modelExt;           // thus: subdir/model2.xem
    backupLeaf  = modelLeaf;
    backupLeaf += xeona::backupTag;          // thus: subdir/model2.xem~

    // form complete (absolute) paths for the model file and the backup file
    fs::path model  = fs::absolute(modelLeaf);   // note the default second argument [1]
    fs::path backup = fs::absolute(backupLeaf);

    // [1] boost::filesystem::absolute() has a default second
    // argument of boost::filesystem::initial_path(), this being
    // the current directory at the time of entry into main().
    //
    // CAUTION: system_complete has different behavior so read the
    // documentation if you wish to change from POSIX conformance
    // to operating system-based format rules

    // regenerate the leaf names in native format for use in local reporting
    std::string natModelLeaf;
    std::string natBackupLeaf;
    fs::path temp;
    temp          = model.filename();
    natModelLeaf  = temp.string();
    temp          = backup.filename();
    natBackupLeaf = temp.string();

    // check for the presence of the model file
    if ( ! fs::is_regular_file(model) )      // path exists and is a regular file [2]
      {
        logger->repx(logga::warn, "model file not found", natModelLeaf);
        return empty;
      }

    // [2] boost::filesystem::is_regular() is similar to the bash
    // builtin: test -f FILE and boost::filesystem::exists() is
    // similar to: test -e FILE (in this case FILE includes regular
    // files, directories, symlinks, and specials)

    // confirm file is writable with C++ <fstream> calls -- while
    // noting that there is currently no such support in the
    // Boost.Filesystem library

    if ( xeona::testWritable(model) != true )     // free function from this file
      {
        logger->repx(logga::warn, "model file not proved writable", "");
        return empty;
      }

    // remove any existing backup file -- an existence test is not
    // necessary because non-existence does not create an exception
    // although the call itself will return 'false'

    try
      {
        fs::remove(backup);
      }
    catch( const std::exception& e )
      {
        logger->repx(logga::warn, "boost::filesystem::remove() fail", natBackupLeaf);
        std::clog << std::flush;
        std::cout
          << "\n"
          <<"** boost::filesystem::remove(" << backup.string() << "): "
          << e.what()
          << "\n"
          << std::endl;
        return empty;
      }

    // back-up the current model file

    try
      {
        fs::copy_file(model, backup);        // CAUTION: 'backup' MUST NOT exist
        logger->repx(logga::info, "model file now backed up", "");
      }
    catch( const std::exception& e )
      {
        logger->repx(logga::warn, "boost::filesystem::copy_file fail", "");
        std::clog << std::flush;
        std::cout
          << "\n"
          <<"** boost::filesystem::copy_file("
          << model.string()  << ", "
          << backup.string() << "): "
          << e.what()
          << "\n"
          << std::endl;
        return empty;
      }

    // additional reporting, CAUTION: leave as std::cout because
    // this is to be treated as application reporting and not
    // logging

    logger->addSmartBlank();                 // next logger call should add blank line
    std::cout
      << ""                                                   << "\n"
      << "  model details (backup made)"                      << "\n"
      << ""                                                   << "\n"
      << "     model file        :  " << model.string()       << "\n"
      << "     backup file       :  " << backup.string()      << "\n"
      << std::flush;

    // make a substantive (rather than empty) path return
    logger->repx(logga::dbug, "returning a useful model path", natModelLeaf);
    return model;
  }

  // ---------------------------------------------------------
  //  FREE FUNCTION   : xeona::readonly
  // ---------------------------------------------------------
  //  Description  : chmod given 'filename' to 'newMode' (see implementation)
  //  Role         : general use (but often after various GLPK file creation calls)
  //  Headers      : <sys/stat.h> <sys/types>
  //  Techniques   : POSIX 'chmod'
  //  Status       : complete
  //
  //  Design notes
  //
  //      The 'chmod' call conforms to POSIX.
  //
  //      See primarily $ man 2 chmod.  See also Robbins and
  //      Robbins (2003 p105) for a list of symbolic file
  //      permission names and 'errno' codes.
  //
  //  References
  //
  //      Robbins, Kay A and Steven Robbins.  2003 UNIX systems
  //        programming : communication, concurrency, and threads --
  //        Second edition.  Prentice Hall PTR, Upper Saddle River,
  //        New Jersey, USA.  ISBN 0-13-042411-0.
  //
  // ---------------------------------------------------------

  bool                                       // 'false' if unsuccessful
  readonly                                   // wrapper function
  (const char* filename)
  {
    if ( filename == NULL )                  // protect against NULL char*
      {
        return readonly("");
      }
    else
      {
        const std::string buf(filename);     // C-string conversion
        return readonly(buf);                // simple wrapper to the std::string variant
      }
  }

  bool                                       // 'false' if unsuccessful
  readonly                                   // workhorse
  (const std::string& filename)
  {

    // skip active code if unit testing
#ifdef _XUTEST
    return true;                             // simply pretend the function worked
#endif

    //  CAUTION: the POSIX symbolic names S_IRGRP and S_IROTH are
    //  not defined in Windows as there is no way to deal with
    //  file permissions for groups and others.  The following
    //  code protects against undefined macros.  (This fix from a
    //  posting by Aleksander Morgado on 17-Apr-2008, recovered
    //  on 16-Mar-2010.  See also Robbins and Robbins (2003 p105)
    //  for a complete list of POSIX file permissions.)

    // define new mode using POSIX symbolic file permission names
    mode_t newMode = S_IRUSR;                // UNIX u=r or Windows read-only
#ifdef S_IRGRP
    newMode |= S_IRGRP;                      // add UNIX g=r
#endif

    // bind logger and prepare 'modestr'
    static logga::spLogger logger = logga::ptrLogStream();
    std::ostringstream oss;
    oss << std::setw(4) << std::setfill('0') << std::oct << newMode;
    const std::string modestr = oss.str();

    // active code
    logger->repx(logga::adhc, "chmod file to " + modestr, filename);
    if ( chmod(filename.c_str(), newMode ) == -1 )     // POSIX system call
      {
        // call failure
        std::ostringstream put;
        put << "  xeona::readonly call failed"  << "\n"
            << "    filename    : " << filename << "\n"
            << "    new mode    : " << modestr  << "\n"
            << "    POSIX call  : " << "chmod"  << "\n"
            << "    POSIX errno : " << errno    << "\n";  // refer <cerrno>
        logger->repx(logga::warn, "chmod call failed, details follow", "");
        logger->putx(logga::warn, put);
        logger->addSmartBlank(logga::warn);
        return false;
      }
    return true;
  }

  // ---------------------------------------------------------
  //  FREE FUNCTION   : xeona::temporaryFilename
  // ---------------------------------------------------------
  //  Description  : create temporary filename
  //  Role         : general use
  //  Techniques   : Boost.Filesystem V3 'boost::filesystem::unique_path'
  //  Status       : complete
  //
  //  Design notes
  //
  //      Requires version V3 of the 'Boost:Filesystem' library.
  //
  //      The function 'unique_path' defaults to "%%%%-%%%%-%%%%-%%%%"
  //
  //  CAUTION: deprecated functions
  //
  //      Do not use 'std::tmpnam' from 'cstdlib'.
  //
  // ---------------------------------------------------------

  const std::string
  temporaryFilename
  (const std::string& extension,
   const std::string& stub)
  {
    const boost::filesystem::path pat("%%%%%%%%%%%%%%%%");
    const boost::filesystem::path temp = boost::filesystem::unique_path(pat);
    std::string tempname;
    if ( extension.empty() )
      tempname = stub + temp.native();       // return native string
    else
      tempname = stub + temp.native() + "." + extension;
    return tempname;
  }

} // namespace xeona

//  end of file

