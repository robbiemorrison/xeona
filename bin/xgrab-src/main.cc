//  XEONA ENERGY SYSTEMS MODELING ENVIRONMENT
//
//  file-create-name : main.cc
//  file-create-date : Mon 07-Nov-2011 10:20 UTC
//  file-initiator   : Robbie Morrison <robbie@actrix.co.nz>
//
//  file-role        : utility to parse XEM files
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
//  $URL: file:///home/robbie/svn-root/xeona/futz/trunk/bin/xgrab-src/main.cc $
//
//  GENERAL NOTES FOR THIS FILE
//
//  Update '::copyrightYear' and '::version' as required.

//  LOCAL AND SYSTEM INCLUDES

#include "smart_ptr.h"        // switch easily between TR1 and Boost smart pointers
#include "xemtree.h"          // XEM recordset tree
#include "usage.h"            // usage message and summary preparation
#include "utils.h"            // support utilities
#include "common.h"           // common definitions for project (place last)

#include <iostream>           // standard io
#include <sstream>            // string-streams
#include <stdexcept>          // standard exception classes, runtime_error()
#include <string>             // C++ strings
#include <vector>             // STL sequence container

#include <cstring>            // C-style string handing, strcpy(), basename()
#include <getopt.h>           // C-style GNU libc normal short and long options

#include <boost/format.hpp>   // printf style formatting

//  FILE-LOCAL GLOBAL VARIABLES

namespace
{
  const std::string version        = "08";   // used in '--version'
  const std::string copyrightYears = "2011"; // used in '--legal'
  const std::string xemExt         = "xem";  // used generally

} // unnamed namespace

//  CODE

// ---------------------------------------------------------
//  PROGRAM         : utility to parse XEM files
// ---------------------------------------------------------
//  Description  : utility to parse XEM files
//  Role         : written to be invoked from bash scripts and similar
//  Techniques   : GNU 'getopt_long', Boost C++ libraries (only header-only)
//  Status       : complete
//
//  Portability
//
//      The Boost C++ libraries are the only compile-time
//      dependency.  Header-only libraries have been used
//      exclusively, so there should be no special run-time
//      dependencies.
//
//      This code was written to be portable.  If the function
//      'basename' causes problems, try excluding header
//      <cstring> for the GNU version and including <libgen.h>
//      for the POSIX version instead.  For more information, see
//      the official GNU 'libc' documentation.
//
//      The 'glibc' library used for development was 2.11.1.
//      Sample code for 'getopt_long' from 2.14.1 proved faulty.
//      I guess the new version is backward compatible, else the
//      command-line parsing code here may need to be updated for
//      newer environments.
//
//  Design and code quality
//
//     The XEM model parsing code (unit 'xemtree') works well,
//     the user interface is pretty well resolved, and the use of
//     custom exceptions offers a clean solution.  Overall, I am
//     happy with the overall design and implementation.
//
// ---------------------------------------------------------

// ---------------------------------------------------------
//  MAIN FUNCTION   : main
// ---------------------------------------------------------
//  Description  : main function
//  Role         : main function
//  Techniques   : GNU 'getopt_long'
//  Status       : complete
//
//  Regarding command-line parsing with GNU 'getopt_long', see:
//
//      notes at beginning of this code
//      http://en.wikipedia.org/wiki/Getopt
//      man getopt_long
//
// ---------------------------------------------------------

int
main
(int   argc,                                 // argument count
 char* argv[])                               // argument values
{

  Deport::log(FUNC, 1, "entered main function");  // iff 'Flags::debug' defaults to 'true'

  try                                             // entire 'main' is in a try block
    {
      // set program name

      const std::string program(basename(argv[0]));    // CAUTION: POSIX version modifies
      localException::setProgram(program);             // update the local exception base
      Deport::log(FUNC, 1, "program", program);        // (see note above about defaults)

      // command line parsing

      int c;
      while ( true )
        {
          int option_index = 0;
          static struct option long_options[] =
            {
              // const char* name : long-option name
              // int has_arg      : 0 = no, 1 = yes, 2 = optional argument
              // int* flag        : specifies how results are returned, if 0 returns 'val'
              // int val          : value to return or load to variable pointed by 'flag'

              {"debug",    0, 0, 'd'},
              {"exitcode", 1, 0, 'e'},
              {"failmsg",  1, 0, 'f'},
              {"help",     0, 0, 'h'},
              {"indata",   0, 0, 'i'},
              {"jettison", 0, 0, 'j'},
              {"kind",     0, 0, 'k'},
              {"legal",    0, 0, 'l'},
              {"measure",  0, 0, 'm'},
              {"names",    0, 0, 'n'},
              {"outdata",  0, 0, 'o'},
              {"quiet",    0, 0, 'q'},
              {"summary",  0, 0, 's'},
              {"unquote",  0, 0, 'u'},
              {"version",  0, 0, 'v'},
              {"wasrun",   0, 0, 'w'},
              {"xemstub",  0, 0, 'x'},
              {0, 0, 0, 0}
            };

          c = getopt_long(argc, argv, "de:f:hijklmnoqsuvwx",
                          long_options, &option_index);
          if ( c == -1 )
            break;

          switch ( c )
            {
            case 'd':                        // --debug
              Flags::debug = true;           // application-wide variable
              Deport::log(FUNC, 1, "debug flag set", Flags::debug);
              break;

            case 'e':                        // --exitcode
              {
                const std::string code = optarg;
                if ( tellExitCode(std::cout, code) )
                  {
                    Deport::log(FUNC, 1, "about to return", ExitCodes::success);
                    return ExitCodes::success;
                  }
                else
                  {
                    throw badArg(FUNC, code);
                  }
              }

            case 'f':                        // --failmsg
              Flags::failuse = true;
              Flags::failmsg = optarg;
              break;

            case 'h':                        // --help
              {
                displayUsage(std::cout, program, ::xemExt);
                Deport::log(FUNC, 1, "about to return", ExitCodes::success);
                return ExitCodes::success;
              }

            case 'i':                        // --indata
              Flags::outdata = false;
              break;

            case 'j':                        // --jettison
              Flags::jettison = true;
              break;

            case 'k':                        // --kind
              Flags::kind = true;
              break;

            case 'l':                        // --legal
              {
                displayLegal(std::cout, ::copyrightYears);
                Deport::log(FUNC, 1, "copyright years", ::copyrightYears);
                Deport::log(FUNC, 1, "about to return", ExitCodes::success);
                return ExitCodes::success;
              }

            case 'm':                        // --measure
              Flags::units = true;
              break;

            case 'n':                        // --names
              Flags::names = true;
              break;

            case 'o':                        // --outdata
              Flags::indata = false;
              break;

            case 'q':                        // --quiet
              Flags::quiet = true;
              break;

            case 's':                        // --summary
              Flags::summary = true;
              break;

            case 'u':                        // --unquote
              Flags::unquote = true;
              break;

            case 'v':                        // --version
              displayVersion(std::cout, ::version);
              Deport::log(FUNC, 1, "version string",  ::version);
              Deport::log(FUNC, 1, "about to return", ExitCodes::success);
              return ExitCodes::success;

            case 'w':                        // --wasrun
              Flags::wasrun = true;
              break;

            case 'x':                        // --xemstub
              Flags::xemstub = true;
              break;

            default:                         // "unrecognized options" arrive here
              std::ostringstream oss;
              oss << "'getopt_long' returned char " << boost::format("'0%o'") % c;
              Deport::log(FUNC, 1, oss.str());
              // std::cerr << program << ": usage problem (try --help)" << std::endl;
              return ExitCodes::usage;
            }
        }

      // comment on program name

      Deport::log(FUNC, 1, "program name", program);

      // harvest remaining arguments

      Flags::freeArgs = argc - optind;
      Deport::log(FUNC, 1, "free args (orig)", Flags::freeArgs);

      std::string xemfile = "";
      std::string target  = "";
      if ( optind < argc )
        {
          xemfile = argv[optind++];
        }
      if ( optind < argc )
        {
          target = argv[optind++];
        }

      // split one argument into two (rejig) as required

      if ( rejig(xemfile, target, ::xemExt) )
        {
          Flags::freeArgs = 2;               // else unity, which is not correct
          Deport::log(FUNC, 1, "command-line arguments now rejigged");
        }

      // debug reporting

      Deport::log(FUNC, 1, "xemfile", xemfile);
      Deport::log(FUNC, 1, "target",  target);

      // free argument integrity checks

      if ( optind < argc )
        {
          Deport::log(FUNC, 1, "free arguments", Flags::freeArgs);
          Deport::log(FUNC, 1, "more than two free arguments given");
          std::cerr << program << ": usage problem (try --help)" << std::endl;
          return ExitCodes::usage;
        }
      if ( xemfile.empty() )
        {
          Deport::log(FUNC, 1, "free arguments", Flags::freeArgs);
          Deport::log(FUNC, 1, "name of xemfile remains empty string");
          std::cerr << program << ": usage problem (try --help)" << std::endl;
          return ExitCodes::usage;
        }

      // debug reporting

      Flags::recite(1);
      Deport::log(FUNC, 1, "xem file read code commencing");

      // load the XEM file

      Text text;
      const int lineCount = text.slurp(xemfile);
      Deport::log(FUNC, 1, "slurp lines", lineCount);

      // parse the file

      RecordSet xem;
      while ( ! text.empty() ) xem.parseLine( text.pop() );
      const int xemsize = xem.size();
      Deport::log(FUNC, 1, "xem size", xemsize);
      if ( xemsize == 0 ) throw mtXem(FUNC, xemfile);

      // set last run

      xem.determineWasRun("program.last-run.used-svn");     // proxy for "was run"

      // code below conditional on given options

      switch ( Flags::freeArgs )
        {
        case 1:                              // one free argument
          if ( Flags::wasrun )               // hence '--wasrun' takes precedence
            {
              if ( xem.wasRun() )
                {
                  if ( ! Flags::quiet ) std::cout << "was run" << std::endl;
                  return ExitCodes::wasRun;
                }
              else
                {
                  if ( ! Flags::quiet ) std::cout << "not run" << std::endl;
                  return ExitCodes::wasNotRun;
                }
            }
          else if ( Flags::summary )         // next '--summary'
            {
              summarizeXem(std::cout, xem, xemfile);
            }
          else if ( Flags::names )
            {
              if ( Flags::xemstub )
                {
                  std::string xemstub(xemfile);        // extension must match '::xemExt'
                  if ( ! removeFileExt(xemstub, ::xemExt) ) throw badExt(FUNC, ::xemExt);
                  dumpXemNames(std::cout,
                               xem,
                               Flags::indata,
                               Flags::outdata,
                               xemstub);               // prepend 'filestub' with dot
                }
              else
                {
                  dumpXemNames(std::cout,
                               xem,
                               Flags::indata,
                               Flags::outdata);        // not so fussy
                }
            }
          else if ( Flags::jettison )
            {
              if ( Flags::xemstub )
                {
                  std::string xemstub(xemfile);        // extension must match '::xemExt'
                  if ( ! removeFileExt(xemstub, ::xemExt) ) throw badExt(FUNC, ::xemExt);
                  dumpXem(std::cout,
                          xem,
                          Flags::indata,
                          Flags::outdata,
                          xemstub);                    // prepend 'filestub' with dot
                }
              else
                {
                  dumpXem(std::cout,
                          xem,
                          Flags::indata,
                          Flags::outdata);             // not so fussy
                }
            }
          else                               // no special mode flags
            {
              // the model parsing is enough
            }
          break;

        case 2:                              // two free arguments
          if ( Flags::kind )                 // hence '--kind' takes precedence
            {
              switch ( xem.kind(target) )
                {
                case Field::e_inData:  std::cout << ">" << std::endl; break;
                case Field::e_outData: std::cout << "<" << std::endl; break;
                default: throw noFqf(FUNC, target);
                }
            }
          else if ( Flags::units )           // next '--units'
            {
              shared_ptr<Field> field = xem.field(target);
              if ( field ) std::cout << field->units() << std::endl;
              else         throw noFqf(FUNC, target);
            }
          else if ( Flags::summary )         // next '--summary'
            {
              shared_ptr<Field> field = xem.field(target);
              if ( field ) summarizeTarget(std::cout, field, target);
              else         throw noFqf(FUNC, target);
            }
          else                               // no special mode flags
            {
              const std::string value = xem.value(target);
              const Field::Kind fkind = xem.kind(target);
              if ( ! xem.wasRun() && fkind == Field::e_outData )
                {
                  Deport::log(FUNC, 1, "seeking out-data from unrun model");
                  Deport::log(FUNC, 1, "value (fyi)", value);
                  throw mtFqf(FUNC, target);
                }
              else if ( value.empty() )
                {
                  Deport::log(FUNC, 1, "value", value);
                  throw noFqf(FUNC, target);
                }
              else
                {
                  if ( Flags::unquote )
                    {
                      std::cout << unquoteString(value) << std::endl;
                    }
                  else
                    {
                      std::cout << value << std::endl;
                    }
                }
            }
          break;

        default:                             // should never get here
          std::cerr << program << ": usage problem (try --help)" << std::endl;
          return ExitCodes::usage;
          break;
        }
    }
  catch ( const localException& e )
    {
      if ( ! Flags::quiet ) std::cerr << e.tell() << std::flush;
      if ( Flags::failuse ) std::cout << Flags::failmsg << std::endl;
      Deport::log(FUNC, 1, "about to return", e.code());
      return e.code();
    }
  catch ( const std::exception& e )
    {
      if ( ! Flags::quiet ) std::cerr << "unexpected std::exception caught" << std::endl;
      if ( ! Flags::quiet ) std::cerr << e.what() << std::endl;
      if ( Flags::failuse ) std::cout << Flags::failmsg << std::endl;
      Deport::log(FUNC, 1, "about to return", ExitCodes::failure);
      return ExitCodes::failure;
    }
  catch ( ... )
    {
      if ( ! Flags::quiet ) std::cerr << "unexpected std exception caught" << std::endl;
      if ( Flags::failuse ) std::cout << Flags::failmsg << std::endl;
      return ExitCodes::failure;
    }

  // return success

  Deport::log(FUNC, 1, "about to return", ExitCodes::success);
  return ExitCodes::success;

}

//  end of file

