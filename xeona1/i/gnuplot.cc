//  XEONA ENERGY SYSTEMS MODELING ENVIRONMENT
//
//  file-create-name : gnuplot.cc
//  file-create-date : Tue 24-May-2011 21:43 UTC
//  file-initiator   : Robbie Morrison <robbie@actrix.co.nz>
//
//  file-role        : gnuplot interface, originally by Daniel Stahlke / implementation
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
//  $URL: file:///home/robbie/svn-root/xeona/futz/trunk/xeona1/i/gnuplot.cc $

//  LOCAL AND SYSTEM INCLUDES

#include "gnuplot.h"          // companion header for this file (place first)

#include "../a/logger.h"      // standard logging functionality (as required)
#include "../c/smart_ptr.h"   // toggle between Boost and TR1 smart pointers
#include ".././common.h"      // common definitions for project (place last)

#include <algorithm>          // STL copying, searching, and sorting, max()
#include <map>                // STL associative container
#include <iomanip>            // setw() and family
#include <sstream>            // string-streams
#include <string>             // C++ strings
#include <utility>            // STL pair, make_pair()
#include <vector>             // STL sequence containe

#include <boost/format.hpp>             // printf style formatting
#include <boost/algorithm/string.hpp>   // string recasing, trimming, splitting

//  PREPROCESSOR MACROS

#if defined(WIN32) || defined(_WIN32) || defined(__WIN32__) || defined(__TOS_WIN__)
# define PCLOSE _pclose
# define POPEN  _popen
# define FILENO _fileno
#elif defined(unix) || defined(__unix) || defined(__unix__) || defined(__APPLE__)
# define PCLOSE pclose
# define POPEN  popen
# define FILENO fileno
#else
# error "unsupported or unknown operating system"
#endif

// compiler-specific code to obtain the name of this function
#ifdef __GNUG__                              // a GNU g++ compiler
# define MYFUNC __PRETTY_FUNCTION__
#else
# define MYFUNC  __func__
#endif

//  CODE

// ---------------------------------------------------------
//  MEMBER FUNCTION : logme (overloaded)
// ---------------------------------------------------------

// CAUTION: these two functions must be defined before the
// preprocessor macro that overwrites their calls

void
Gnuplot::logme
(const std::string msg)
{
  if ( ! say ) return;
  std::clog << msg << std::endl;
}

void
Gnuplot::logme
(const std::string msg,
 const std::string file,
 const int         line,
 const std::string func)
{
  if ( ! say ) return;
  std::clog << file << ":"
            << line << ": "
            << func << ": "
            << msg << std::endl;
}

// special CPP macro to add extra information at the point of invocation

#if 1 // 0 is extra information deactivated, 1 is activated
# define logme(msg) logme((msg), __FILE__, __LINE__, MYFUNC)
#endif

// ---------------------------------------------------------
//  STATIC DEF      : ctorCount
// ---------------------------------------------------------

int Gnuplot::ctorCount = 0;

// ---------------------------------------------------------
//  FREE FUNCTION   : xeona::okayGnuplotTerm
// ---------------------------------------------------------

namespace xeona
{
  bool
  okayGnuplotTerm
  (const std::string& term)
  {
    // logger
    static logga::spLogger logger = logga::ptrLogStream();  // local usage

    // declare container
    typedef std::map<std::string, std::string> data_type;
    static data_type terms;

    // load data
    terms.insert(std::make_pair("dumb",     "print ASCII-art to terminal"));
    terms.insert(std::make_pair("x11",      "interactive display using 'X11' libraries"));
    terms.insert(std::make_pair("wxt",      "interactive display using 'wxWidgets' set"));
    terms.insert(std::make_pair("svg",      "SVG output"));

#if 0 // potential extensions, need to update class 'TsGnuplot' to use

    terms.insert(std::make_pair("aqua",     "interactive display for Mac OS"));
    terms.insert(std::make_pair("windows",  "interactive display for Microsoft Windows"));
    terms.insert(std::make_pair("xterm",    "print ASCII-art using semi-smart terminal"));

    terms.insert(std::make_pair("pdfcairo", "PDF output using Cairo library"));
    terms.insert(std::make_pair("png",      "PNG output"));
    terms.insert(std::make_pair("pngcairo", "PNG output using Cairo library"));

#endif // 0

    // look for match
    data_type::iterator pos;
    pos = terms.find(term);                  // member function
    if ( pos == terms.end() )
      {
        logger->repx(logga::warn, "term not matched", term);
        return false;
      }
    else
      {
        const std::string msg = pos->second;
        logger->repx(logga::dbug, "term successfully matched", term);
        logger->repx(logga::adhc, "interpretation", msg);
        return true;
      }

  } // function 'xeona::okayGnuplotTerm'
} // namespace 'xeona'

// ---------------------------------------------------------
//  MEMBER FUNCTION : Gnuplot
// ---------------------------------------------------------
//  Description  : constructor
//  Role         : client usage
//  Techniques   : io manipulators
//  Status       : complete
//
//  Design notes
//
//      The call 'popen' means pipe a stream to a process.
//
//      Changing "w" to "r" echos 'gnuplot' terminal output to
//      the console:
//
//        "r" is open text file for reading
//        "w" is truncate file to zero length or create text file for writing
//
//
//      It is expected that 'cmd' is set to something sensible.
//
// ---------------------------------------------------------

Gnuplot::Gnuplot
(const std::string       cmd,
 const Gnuplot::LogLevel level) :            // note the default value
  boost::iostreams::stream<boost::iostreams::file_descriptor_sink>
  (FILENO(pout = POPEN(cmd.c_str(), "w")),   // see above for hash-defines
   boost::iostreams::never_close_handle),    // remain open after program exits
  pout(pout),                                // keeps '-Weff++' quiet
  gnuplotTerm(),                             // empty string
  say(level),
  objectNo(boost::str(boost::format("%02d") % ++ctorCount))
{
  logme("beginning gnuplot session " + objectNo);
  logme("cmd = '" + cmd + "', log level = " + (level ? "on" : "off"));

  *this << std::scientific << std::setprecision(18);   // refer <iomanip>
}

// ---------------------------------------------------------
//  MEMBER FUNCTION : ~Gnuplot
// ---------------------------------------------------------

Gnuplot::~Gnuplot()
{
  logme("ending gnuplot session " + objectNo);

  // The 'never_close_handle' argument means the handle cannot be
  // closed here, even if 'close()' or some other technique is
  // attempted.  On the other hand, calling 'pclose' on the FILE
  // pointer 'pout' prevents a memory leak.  In addition,
  // flushing the ostream first seems like good practice.

  *this << std::flush;
  fflush(pout);
  // std::ostream::flush();

  if ( PCLOSE(pout) )
    {
      // This error also occurs when 'gnuplot' encounters a bad
      // statement -- on this basis, knowledge of this error
      // could be valuable.

      std::cerr << __FILE__ << ": WARNING: 'pclose' returned error" << std::endl;
    }
}

// ---------------------------------------------------------
//  MEMBER FUNCTION : setTerminal
// ---------------------------------------------------------
//  Description  : set gnuplot terminal type
//  Role         : client usage
//  Techniques   : gnuplot "set terminal"
//  Status       : complete
//
//  CAUTION: this call MUST precede all other calls
//
//      This requirement is not explicitly checked here, but
//     'gnuplot' will complain if the call is omitted or placed
//     late.
//
// ---------------------------------------------------------

void
Gnuplot::setTerminal(const std::string& term)     // "dumb" "x11" "wxt"
{
  const bool supported = ( term == "dumb" ||
                           term == "x11"  ||
                           term == "wxt"  ||
                           term == "svg" );
  if ( supported ) logme("term = '" + term + "'");
  else             logme("WARNING: gnuplot terminal type not tested: " + term);

  gnuplotTerm = term;                        // store value

  *this << "set terminal " << gnuplotTerm << std::endl;
}

// ---------------------------------------------------------
//  MEMBER FUNCTION : setPersist
// ---------------------------------------------------------
//  Description  : make interactive terminals persist after 'xeona' exists
//  Role         : client usage
//  Techniques   : (nothing special)
//  Status       : complete
// ---------------------------------------------------------

void
Gnuplot::setPersist()                        // plot window remains open
{
  logme("plot windows should persist (keypress 'q' to quit)");

  *this << "set terminal " << gnuplotTerm << " persist" << std::endl;
}

// ---------------------------------------------------------
//  MEMBER FUNCTION : setPlotWindowTitle
// ---------------------------------------------------------
//  Description  : set interactive plot window title, including an object counter
//  Role         : client usage
//  Techniques   : gnuplot "set window title"
//  Status       : complete
// ---------------------------------------------------------

void
Gnuplot::setPlotWindowTitle
(const std::string& windowTitle)             // plot window title
{
  const std::string joiner = " | ";          // adjust here
  std::string buffer  = objectNo;
  if ( ! windowTitle.empty() )
    {
      buffer += joiner;
      buffer += windowTitle;
    }
  boost::to_lower(buffer);                   // downcase
  logme("plot window titled: " + buffer);

  *this << "set terminal " << gnuplotTerm << " title '" << buffer << "'" << std::endl;
}

// ---------------------------------------------------------
//  MEMBER FUNCTION : setPlotWindowSize
// ---------------------------------------------------------
//  Description  : set plot window or dumb terminal size
//  Role         : client usage
//  Techniques   : gnuplot "set terminal size"
//  Status       : complete
//
//  Design notes
//
//      See : http://stackoverflow.com/questions/4820647
//
//      This did not work with "wxt" (but probably would with the
//      "x11" terminal):
//
//          ~/.Xresources
//          add: gnuplot*geometry: 600x600
//          update: $ xrdb ~/.Xresources
//
//  Usage examples
//
//     The 'width' and 'height' values given may need to be
//     tweaked to suit your hardware.  The 'size' option for
//     "wxt" requires gnuplot version 4.4.
//
//          Gnuplot gp("gnuplot", "dumb");
//          gp.setPlotWindowSize(80, 25);     // width (cols) x height (lines)
//
//          Gnuplot gp("gnuplot", "x11");
//          gp.setPlotWindowSize(1000, 600);  // width (px) x height (px)
//
//          Gnuplot gp("gnuplot", "wxt");
//          gp.setPlotWindowSize(1100, 600);  // width (px) x height (px)
//
// ---------------------------------------------------------

void
Gnuplot::setPlotWindowSize
(const int width,
 const int height)
{
  std::ostringstream oss;
  oss << width << "," << height;
  const std::string size = oss.str();

  if ( gnuplotTerm == "wxt" )
    logme("plot window resized (requires version 4.4): " + size);
  else
    logme("plot window resized: " + size);

  *this << "set terminal " << gnuplotTerm << " size " << size << std::endl;
}

// ---------------------------------------------------------
//  MEMBER FUNCTION : setOutput
// ---------------------------------------------------------
//  Description  : set ouput, currently just for SVG
//  Role         : client usage, client to confirm filename
//  Techniques   : (nothing special)
//  Status       : complete
// ---------------------------------------------------------

void
Gnuplot::setOutput(const std::string& output)     // output file
{
  if ( gnuplotTerm == "svg" )
    logme("will write SVG to file: " + output);
  else
    logme("faulty call, can only be used with svg terminal: " + output);

  *this << "set output " << "\"" << output << "\"" << std::endl;
}

// ---------------------------------------------------------
//  MEMBER FUNCTION : setFinalize
// ---------------------------------------------------------
//  Description  : finalize ouput, currently just for SVG
//  Role         : client usage, completes writeout of file
//  Techniques   : (nothing special)
//  Status       : complete
// ---------------------------------------------------------

void
Gnuplot::setFinalize()                       // final call
{
  if ( gnuplotTerm == "svg" )
    logme("finalizing the SVG file");
  else
    logme("faulty call, can only be used with svg terminal");

  *this << "set output" << std::endl;
}

// ---------------------------------------------------------
//  CPP UNDEFINES   : various
// ---------------------------------------------------------

#undef MYFUNC
#undef logme                                 // omit the "extra tokens"

#undef PCLOSE
#undef POPEN
#undef FILENO

//  end of file

