//  XEONA ENERGY SYSTEMS MODELING ENVIRONMENT
//
//  file-create-name : main.cc
//  file-create-date : Tue 15-Nov-2011 12:18 UTC
//  file-initiator   : Robbie Morrison <robbie@actrix.co.nz>
//
//  file-role        : utility to summarize numerical timeseries via the command-line
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
//  $URL: file:///home/robbie/svn-root/xeona/futz/trunk/bin/xstat-src/main.cc $
//
//  GENERAL NOTES FOR THIS FILE

//  AD-HOC NOTES
//
//  The design could be a little more sensitive to integers,
//  instead of casting them to 'double' without a second thought.

//  LOCAL AND SYSTEM INCLUDES

#include "smart_ptr.h"        // switch easily between TR1 and Boost smart pointers

#include "stats.h"            // on-the-fly statistical calculations
#include "common.h"           // common definitions for project (place last)

#include <algorithm>          // STL copying, searching, and sorting, max()
#include <deque>              // STL sequence container, double-ended vector
#include <iostream>           // standard io
#include <sstream>            // string-streams
#include <string>             // C++ strings
#include <vector>             // STL sequence container

#include <unistd.h>           // POSIX sysconf()

#include <boost/algorithm/string.hpp>   // string recasing, trimming, splitting
#include <boost/format.hpp>             // printf style formatting
#include <boost/lexical_cast.hpp>       // lexical_cast<> string to number conversions

// ---------------------------------------------------------
//  documentation   : Command-line argument limitations
// ---------------------------------------------------------
//
//  Introduction
//
//      This utility passes relatively large timeseries via the
//      command-line.  On new systems and in most cases, this
//      should not present a problem.
//
//      See also the tests in 'xstats.sh'.  Multiple arguments
//      supported 201480 elements or 23 * 8760.
//
//  Argument limitations
//
//      Argument size limits CAN derive from two sources:
//
//          * that set by the 'bash' shell
//          * that set dynamically by C-style function 'execve'
//
//      Bash shell
//
//          'ARG_MAX' is the number of bytes of args + environ
//
//          $ hostname --long
//            hinau.fb10.tu-berlin.de
//          $ uname -rms
//            Linux 2.6.32-35-generic x86_64
//          $ getconf ARG_MAX
//            2097152
//          $ getconf -a | grep --color "ARG_MAX"
//            # gives ARG_MAX and _POSIX_ARG_MAX
//          $ echo $(( $(getconf ARG_MAX) - $(env | wc --bytes) ))
//            2094402
//
//          There is some suggestion that this limit is not due
//          to 'bash' and that 'bash' is simply reporting the
//          limit set by the operating system.
//
//      POSIX function 'execve'
//
//          $ man 2 execve    # see section "Limits on size of arguments and environment"
//
//         "Most Unix implementations impose some limit on the
//          total size of the command-line argument (argv) and
//          environment (envp) strings that may be passed to a
//          new program."
//
//          Linux kernels after 2.6.23 set the floor and ceiling
//          size ('argv' plus 'envp') limits dynamically.  The
//          limit is large and is available at run-time using:
//
//              #include <iostream>           // standard io
//              #include <unistd.h>           // POSIX sysconf()
//              int main(int argc, char* argv[])
//              {
//                const long argMax = sysconf(_SC_ARG_MAX);
//                std::cout << "argMax : " << argMax << std::endl;
//                return 0;
//               }
//
//          On 'hinau' this yields: 2097152.
//
//      String restrictions
//
//          Other restrictions apply to the number and size of
//          strings.  But is seems that:
//
//              $ getconf PAGE_SIZE
//                4096
//
//          Which, together with:
//
//              $ cat /usr/include/linux/binfmts.h
//
//          Implies 'MAX_ARG_STRLEN' is 4096 * 32 or: 131072
//          Also 'MAX_ARG_STRINGS' is 0x7FFFFFFF or: 2147483647
//
//  Typical timeseries argument size
//
//      First, assume each entry is >+0.00e+00 < or 10 chars.
//      Hence 5 minute sampling for one year gives: 8760 * (60/5)
//      * 10 or: 1051200.  On 'hinau' this leaves a safety margin
//      of about 2-fold if multiple arguments are used.
//
//  Tests show that multiple arguments work better:
//
//      Relative to 8760 by 9-char space separated arguments:
//
//          * multiple arguments   : 201480 (23-fold) okay (but not 24-fold)
//          * stringified argument : 17520 (twice) NOT okay
//
//      Error is: "Argument list too long".
//
//  Storage requirements
//
//      8760 * 12 cast to 8-byte double ~ 0.8MiB
//      8760 * 23 cast to 8-byte double ~ 1.5MiB
//
//  Resources
//
//      http://www.in-ulm.de/~mascheck/various/argmax/
//      http://stackoverflow.com/questions/4185017
//      appropriate manpages and headers
//
// ---------------------------------------------------------

//  FILE-LOCAL GLOBAL VARIABLES

namespace
{
  bool     flagQuiet  = false;               // set to 'true' by option '--quiet'
  unsigned flagDigits = 3;                   // default, overridden by option '--digits'
}

//  CODE

// ---------------------------------------------------------
//  FREE FUNCTION   : reportStatistic
// ---------------------------------------------------------
//  Description  : take 'stat' and 'data' and returns appropriate statistic
//  Role         : called by 'main'
//  Techniques   : 'std::deque', 'Boost.Conversion' library, template class 'Statistics'
//  Status       : complete
//
//  Design notes
//
//      Only 'double' casting is supported -- this is quite
//      satisfactory, but not especially user friendly.
//
// ---------------------------------------------------------

bool
reportStatistic
(std::ostream&            os,
 const std::string&       stat,
 std::deque<std::string>& data )
{
  // bail out if empty
  if ( data.empty() )
    {
      os << "no data given";
      return false;
    }

  // loop and capture
  Statistics<double> stats;                  // see unit 'stats'
  while ( ! data.empty() )
    {
      const std::string elem = data.front(); // does not "pop"
      try
        {
          const double value = boost::lexical_cast<double>(elem);
          stats(value);
        }
      catch ( const boost::bad_lexical_cast& e )
        {
          // e.what() =
          // "bad lexical cast: source type value could not be interpreted as target"

          os << "bad lexical cast: string element could not be reinterpreted as double"
             << ": " << "\"" << elem << "\"";
          return false;
        }
      data.pop_front();                      // returns 'void'
    }

  // harvest and format
  const std::string z = "%d";                // integer formatting
//const std::string r = "%+.2e";             // real (floating-point) formatting

  std::ostringstream oss;
  oss << "%+." << (::flagDigits - 1) << "e"; // "%+.0e" where 0 is placeholder
  const std::string r = oss.str();

  if ( false ) { }                           // just for legibility
  else if ( stat == "count"         ) { os << boost::format(z) % stats.count();      }
  else if ( stat == "first"         ) { os << boost::format(r) % stats.first();      }
  else if ( stat == "last"          ) { os << boost::format(r) % stats.last();       }
  else if ( stat == "sum"           ) { os << boost::format(r) % stats.sum();        }
  else if ( stat == "sumsq"         ) { os << boost::format(r) % stats.sumsq();      }
  else if ( stat == "mean"          ) { os << boost::format(r) % stats.mean();       }
  else if ( stat == "var"           ) { os << boost::format(r) % stats.var();        }
  else if ( stat == "sdev"          ) { os << boost::format(r) % stats.sdev();       }
  else if ( stat == "var2"          ) { os << boost::format(r) % stats.var2();       }
  else if ( stat == "sdev2"         ) { os << boost::format(r) % stats.sdev2();      }
  else if ( stat == "min"           ) { os << boost::format(r) % stats.min();        }
  else if ( stat == "max"           ) { os << boost::format(r) % stats.max();        }
  else if ( stat == "range"         ) { os << boost::format(r) % stats.range();      }
  else if ( stat == "zeros"         ) { os << boost::format(z) % stats.zeros();      }
  else if ( stat == "nonzeros"      ) { os << boost::format(z) % stats.nonzeros();   }
  else if ( stat == "zero-ratio"    ) { os << boost::format(r) % stats.zerorat();    }
  else if ( stat == "nonzero-ratio" ) { os << boost::format(r) % stats.nonzerorat(); }
  else if ( stat == "opmean"        ) { os << boost::format(r) % stats.opmean();     }
  else if ( stat == "strict-pos"    ) { os << boost::format(z) % stats.strictPos();  }
  else if ( stat == "strict-neg"    ) { os << boost::format(z) % stats.strictNeg();  }
  else if ( stat == "zero-pos"      ) { os << boost::format(z) % stats.zeroPos();    }
  else if ( stat == "zero-neg"      ) { os << boost::format(z) % stats.zeroNeg();    }
  else if ( stat == "summary"       )
    {
      os << boost::format(z) % stats.count()
         << " "
         << boost::format(r) % stats.min()
         << " "
         << boost::format(r) % stats.mean()
         << " "
         << boost::format(r) % stats.max();
    }
  else if ( boost::starts_with(stat, "integral=") )
    {
      const int strlen = std::string("integral=").size();
      const std::string width = boost::erase_head_copy(stat, strlen);
      try
        {
          const int     value    = boost::lexical_cast<int>(width);
          const double  integral = value * stats.sum();
          os << boost::format(r) % integral;
        }
      catch ( const boost::bad_lexical_cast& e )
        {
          // e.what() =
          // "bad lexical cast: source type value could not be interpreted as target"

          os << "option integral"
             << ": bad lexical cast: string could not be reinterpreted as integer"
             << ": " << "\"" << width << "\"";
          return false;
        }
    }
  else
    {
      os << "statistic not found: '" << stat << "'";
      return false;
    }

  // return success
  return true;
}

// ---------------------------------------------------------
//  FREE FUNCTION   : dumpTimeseries
// ---------------------------------------------------------
//  Description  : take 'data', reformat timeseres, and load ostream
//  Role         : called by 'main'
//  Techniques   : 'std::deque', 'Boost.Conversion' library, 'std::copy' from <algorithm>
//  Status       : complete
//
//  Design notes
//
//      Only 'double' casting is supported -- this is quite
//      satisfactory, but not especially user friendly.
//
// ---------------------------------------------------------

bool
dumpTimeseries
(std::ostream&            os,
 std::deque<std::string>& data )
{
  // bail out if empty
  if ( data.empty() )
    {
      os << "no data given";
      return false;
    }

  // set format
  const std::string r = "%+.2e";             // real (floating-point) formatting

  // loop and capture
  std::vector<std::string> ts;
  while ( ! data.empty() )
    {
      const std::string elem = data.front(); // does not "pop"
      try
        {
          const double value = boost::lexical_cast<double>(elem);
          ts.push_back(boost::str(boost::format(r) %  value));
        }
      catch ( const boost::bad_lexical_cast& e )
        {
          // e.what() =
          // "bad lexical cast: source type value could not be interpreted as target"

          os << "bad lexical cast: string element could not be reinterpreted as double"
             << ": " << "\"" << elem << "\"";
          return false;
        }
      data.pop_front();                      // returns 'void'
    }

  // create output
  // CAUTION: the 'std::copy' call appends a trailing space which is later removed
  std::copy(ts.begin(), ts.end(), std::ostream_iterator<std::string>(os, " "));

  // return success
  return true;
}

// ---------------------------------------------------------
//  FREE FUNCTION   : fmt1
// ---------------------------------------------------------

std::string
fmt1
(const std::string& program,
 const std::string& option,
 const std::string& description,
 const std::string& tag = "")
{
  std::ostringstream oss;
  if ( tag.empty() ) oss << boost::format("%17s")  % "";
  else               oss << boost::format("%16s:") % tag;
  oss << " " << boost::format("%s")    % program
      << " " << "[opts]"
      << " " << boost::format("%-21s") % option
      << " " << "<timeseries>" << "    " << "print " << description
      << "\n";
  return oss.str();
}

// ---------------------------------------------------------
//  FREE FUNCTION   : fmt2
// ---------------------------------------------------------

std::string fmt2
(const std::string& line,
 const std::string& tag = "")
{
  std::ostringstream oss;
  if ( tag.empty() ) oss << boost::format("%17s")  % "";
  else               oss << boost::format("%16s:") % tag;
  oss << " " << line
      << "\n";
  return oss.str();
}

// ---------------------------------------------------------
//  FREE FUNCTION   : displayUsage
// ---------------------------------------------------------

void
displayUsage
(std::ostream&     os,
 const std::string program)
{
  const long        argMax    = sysconf(_SC_ARG_MAX);
  const double      argmax    = static_cast<double>(argMax);
  const double      times     = argmax / (8760 * 10);
  const std::string strArgMax = boost::str(boost::format("%d")   % argMax);
  const std::string strTimes  = boost::str(boost::format("%.1f") % times);

  os << "\n";

  // usage
  os << fmt1(program, "--count",              "count", "usage");
  os << fmt1(program, "--first",              "first entry");
  os << fmt1(program, "--last",               "last entry");
  os << fmt1(program, "--sum",                "sum (same as --integral=1)");
  os << fmt1(program, "--sumsq",              "sum squared");
  os << fmt1(program, "--integral=<seconds>", "integral using bin width 'seconds'");
  os << fmt1(program, "--mean",               "mean");
  os << fmt1(program, "--var",                "variation (sample)");
  os << fmt1(program, "--sdev",               "standard deviation (sample)");
  os << fmt1(program, "--var2",               "variation (population)");
  os << fmt1(program, "--sdev2",              "standard deviation (population)");
  os << fmt1(program, "--min",                "minimum value");
  os << fmt1(program, "--max",                "maximum value");
  os << fmt1(program, "--range",              "range");
  os << fmt1(program, "--zeros",              "number of zeros");
  os << fmt1(program, "--nonzeros",           "number of nonzeros");
  os << fmt1(program, "--zero-ratio",         "ratio of zeros/count");
  os << fmt1(program, "--nonzero-ratio",      "ratio of nonzeros/count");
  os << fmt1(program, "--opmean",             "operational mean (ignore zeros)");
  os << fmt1(program, "--strict-pos",         "true (1) if (0,+inf], else false (0)");
  os << fmt1(program, "--strict-neg",         "true (1) if [-inf,0), else false (0)");
  os << fmt1(program, "--zero-pos",           "true (1) if [-inf,0], else false (0)");
  os << fmt1(program, "--zero-neg",           "true (1) if [-inf,0], else false (0)");
  os << fmt1(program, "--range01",            "true (1) if on [0,1], else false (0)");
  os << fmt1(program, "--summary",            "count min mean max");
  os << fmt1(program, "--dump",      "original timeseries using standard formatting");

  std::ostringstream oss1;
  oss1 << program << " --help                                       "
       << "display help message and exit";
  os << fmt2(oss1.str());

  std::ostringstream oss0;
  oss0 << " --digits <int>                             "
       << "output 'int' digits (1 thru 8 valid, default " << ::flagDigits << ")";
  os << fmt2(oss0.str(), "options");

  os << fmt2(" --quiet                                    "
             "do not print on encountering data faults");

  // purpose
  os << fmt2("summarize numeric timeseries using data passed via the command-line",
             "purpose");

  // notes
  os << fmt2("internal calculations use double precision"
             ", output limited to 3 significant figures", "notes");
  os << fmt2("routine uses 'xeona' template class Statistics");

  // system limits
  os << fmt2("ARG_MAX           = " + strArgMax
             + " chars (maximum command-line on current system)",
             "system limits");
  os << fmt2("timeseries metric = " + strTimes + " times"
             + " (assuming 8760 elems, 9 char values, and use of multiple arguments)");

  // timeseries
  os << fmt2("best entered as multiple arguments, but can be single argument set in quote"
             "-marks", "timeseries");

  // troubleshooting
  os << fmt2("an \"Argument list too long\" error may indicate your Linux kernel is too o"
             "ld (see 'execve' manpage)", "troubleshooting");

  // exit codes
  os << fmt2("  0 = success", "exit codes");
  os << fmt2("  1 = data fault");
  os << fmt2("  2 = invalid usage (try --help)");
  os << fmt2("255 = coding error (advise the author)");

  // examples
  std::ostringstream oss2;
  oss2 << "$ " << program << " --mean           +2.00e+00 +8.00e+00       "
       << "# output = +5.00e+00";
  os << fmt2(oss2.str(), "examples");

  std::ostringstream oss3;
  oss3 << "$ " << program << " --integral=3600  +2.00e+00 +8.00e+00       "
       << "# 3600 is the bin width";
  os << fmt2(oss3.str());

  std::ostringstream oss4;
  oss4 << "$ " << program << " --summary        +2.00e+00 +8.00e+00       "
       << "# output = 2 +2.00e+00 +5.00e+00 +8.00e+00";
  os << fmt2(oss4.str());

  std::ostringstream oss5;
  oss5 << "$ " << program << " --sdev           +2.00e+00                 "
       << "# scalar data is okay";
  os << fmt2(oss5.str());

  std::ostringstream oss6;
  oss6 << "$ " << program << " --sum            2 3. 4.0 5E0 +6.0000e+00  "
       << "# all standard number formats are acceptable";
  os << fmt2(oss6.str());

  std::ostringstream oss7;
  oss7 << "$ " << program << " --sdev           +2.00e+00 aa              "
       << "# faulty data, print error message and return 1";
  os << fmt2(oss7.str());

  std::ostringstream oss8;
  oss8 << "$ " << program << " --quiet --sdev   +2.00e+00 aa              "
       << "# faulty data, omit error message and return 1";
  os << fmt2(oss8.str());

  // see also -- with example
  std::ostringstream oss9;
  oss9 << "$ xgrab trial-008.+.entity.node-2-xit-elec-a01.nodal-prices | xargs "
       << program << " --mean";
  os << fmt2("'xgrab' utility, example using 'xargs'", "see also");
  os << fmt2(oss9.str());

  // space
  os << "\n";
}

// ---------------------------------------------------------
//  MAIN FUNCTION   : main
// ---------------------------------------------------------
//  Description  : main function
//  Role         : main function
//  Techniques   : arguments parsed using multiple 'else if' blocks
//  Status       : complete
// ---------------------------------------------------------

int
main
(int   argc,
 char* argv[])
{
  std::string program   = "(not set)";

  try                                                  // entire 'main' is in a try block
    {
      // counter and stop for traversing the command-line

      int optind       = -1;
      const int optmax = argc - 1;

      // set program name

      const std::string temp0(basename(argv[++optind]));  // CAUTION: POSIX version mods
      program = temp0;

      // ---------------------------------
      //  command-line parsing : process the option
      // ---------------------------------

      if ( optind == optmax )
        {
          std::cerr << program << ": invalid usage (try --help)" << std::endl;
          return 2;
        }

      std::string option;
      const std::string temp1(argv[++optind]);
      option = temp1;

      if ( boost::equals(option, "--help") ||
           boost::equals(option, "--hel" ) ||
           boost::equals(option, "--he"  ) ||
           boost::equals(option, "--h"   ) ||
           boost::equals(option, "-h"    ) )
        {
          displayUsage(std::cout, program);
          return 0;
        }

      if ( boost::equals(option, "--quiet") )
        {
          ::flagQuiet = true;
          const std::string temp2(argv[++optind]);     // nibble again
          option = temp2;
        }

      if ( boost::equals(option, "--digits") )
        {
          const std::string temp3(argv[++optind]);     // nibble again
          try
            {
              ::flagDigits = boost::lexical_cast<unsigned>(temp3);
              const std::string temp4(argv[++optind]);     // nibble again
              option = temp4;
            }
          catch ( const boost::bad_lexical_cast& e )
            {
              std::cerr << program
                        << ": --digits argument not integer 1 thru 8 (try --help): "
                        << temp3 << std::endl;
              return 2;
            }
          if ( ::flagDigits < 1 || ::flagDigits > 8 )
            {
              std::cerr << program
                        << ": --digits argument not integer 1 thru 8 (try --help): "
                        << temp3 << std::endl;
              return 2;
            }
        }

      if ( boost::equals(option, "--quiet") )          // try '--quiet' again!
        {
          ::flagQuiet = true;
          const std::string temp2(argv[++optind]);     // nibble again
          option = temp2;
        }

      if ( ! boost::starts_with(option, "--") )
        {
          std::cerr << program << ": valid option required (try --help)" << std::endl;
          return 2;
        }

      const std::string stat = boost::erase_first_copy(option, "--");

      // ---------------------------------
      //  commmand-line parsing : load the data
      // ---------------------------------

      std::deque<std::string> data;          // double-ended vector
      switch ( optmax - optind )
        {
        case 0:                              // no data
          data.clear();
          break;
        case 1:                              // stringified or single arg
          {
            std::string rest(argv[++optind]);
            boost::erase_first(rest, "\"");
            boost::erase_last(rest, "\"");
            boost::split(data, rest, boost::is_any_of(" "));
            if ( data.front().empty() ) data.clear();
          }
          break;
        default:                             // multiple args
          while ( optind < optmax )          // loop and fill
            {
              std::string elem(argv[++optind]);
              boost::erase_first(elem, "\"");
              boost::erase_last(elem, "\"");
              data.push_back(elem);
            }
          break;
        }

      // ---------------------------------
      //  key calls
      // ---------------------------------

      if ( stat == "dump" )
        {
          std::stringstream oss1;
          if ( dumpTimeseries(oss1, data) )            // key call
            {
              std::cout << boost::trim_copy(oss1.str()) << std::endl;
              return 0;
            }
          else
            {
              if ( ! ::flagQuiet )
                {
                  std::cerr << program << ": " << oss1.str() << std::endl;
                }
              return 1;
            }
          }
      else
        {
          std::stringstream oss2;
          if ( reportStatistic(oss2, stat, data) )     // key call
            {
              std::cout << oss2.str() << std::endl;
              return 0;
            }
          else
            {
              if ( ! ::flagQuiet )
                {
                  std::cerr << program << ": " << oss2.str() << std::endl;
                }
              return 1;
            }
        }
    }
  catch ( ... )
    {
      std::cout << program << ": C++ exception caught" << std::endl;
    }

  // should not be here

  if ( ! ::flagQuiet ) std::cerr << program << ": coding error" << std::endl;
  return 255;
}

//  end of file

