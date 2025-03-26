//  XEONA ENERGY SYSTEMS MODELING ENVIRONMENT
//
//  file-create-name : gnuplot.h
//  file-create-date : Tue 24-May-2011 21:43 UTC
//  file-initiator   : Robbie Morrison <robbie@actrix.co.nz>
//
//  file-role        : gnuplot interface, originally by Daniel Stahlke / header
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
//  $URL: file:///home/robbie/svn-root/xeona/futz/trunk/xeona1/i/gnuplot.h $
//
//  GENERAL NOTES FOR THIS FILE
//
//  Original author
//
//    Daniel Stahlke <dstahlke@gmail.com>
//
//  MIT license
//
//    The 'gnuplot-iostream' header is licensed under the MIT
//    free software license.  The MIT license is permissive and
//    fully compatible with the GNU GPL license (according to
//    wikipedia).
//
//  Code modifications
//
//    This unit is based on the original 'gnuplot-iostream.h'
//    file.  The conditional compilation code for
//    'GNUPLOT_ENABLE_PTY' and 'GNUPLOT_ENABLE_BLITZ' has been
//    removed, as neither feature is needed here.  The iostream
//    manipulators now stream directly.  The run-time logging was
//    improved.  Several behavior set calls have been added,
//    including the ability to select the terminal type.
//
//  System requirements
//
//    * Gnuplot must be installed
//    * the Boost C++ libraries must be present
//
//  Link-time requirements
//
//    -lboost_iostreams
//
//  Tested
//
//      gnuplot 4.2.6 / Linux : works but "wxt" plot window cannot be resized thru code
//      gnuplot 4.4.3 / Linux : recommended, "wxt" plot window refresh button also works
//
//  Some external resources
//
//    web page    : http://www.stahlke.org/dan/gnuplot-iostream/
//    git         : git clone git://gitorious.org/gnuplot-iostream/gnuplot-iostream.git
//    MIT license : http://en.wikipedia.org/wiki/MIT_License

//  -------------------------------------------------------------------------------------
//
//  ORIGINAL COPYRIGHT NOTICE
//
//  Copyright (c) 2009 Daniel Stahlke
//
//  Permission is hereby granted, free of charge, to any person obtaining a copy
//  of this software and associated documentation files (the "Software"), to deal
//  in the Software without restriction, including without limitation the rights
//  to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
//  copies of the Software, and to permit persons to whom the Software is
//  furnished to do so, subject to the following conditions:
//
//  The above copyright notice and this permission notice shall be included in
//  all copies or substantial portions of the Software.
//
//  THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
//  IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
//  FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
//  AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
//  LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
//  OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
//  THE SOFTWARE.
//
//   -------------------------------------------------------------------------------------

//  HEADER GUARD

#ifndef _GNUPLOT_H_
#define _GNUPLOT_H_

//  LOCAL AND SYSTEM INCLUDES

#include "../a/logger_fwd.h"  // some forward declarations from "logger.h"

#include <iostream>           // standard io
#include <string>             // C++ strings

#include <cstdio>             // C-style io, remove(), EOF, perror()

#include <boost/iostreams/stream.hpp>                  // stream template for io
#include <boost/iostreams/device/file_descriptor.hpp>  // filesystem access

//  CODE

// ---------------------------------------------------------
//  FREE FUNCTION   : xeona::okayGnuplotTerm
// ---------------------------------------------------------
//  Description  : confirm whether a gnuplot 'term' is supported by 'xeona' or not
//  Role         : support for command-line processing in 'main.cc'
//  Techniques   : 'std::find'
//  Status       : complete
//
//  Design notes
//
//      See implementation for supported 'term' strings.
//
// ---------------------------------------------------------

namespace xeona
{
  bool
  okayGnuplotTerm
  (const std::string& term);
}

// ---------------------------------------------------------
//  CLASS           : Gnuplot
// ---------------------------------------------------------
//  Description  : class to establish gnuplot and then pipe streamed commands
//  Role         : utility
//  Techniques   : UNIX pipe via 'popen', 'Boost.Iostreams' library, template functions
//  Status       : complete
//
//  Design notes
//
//      Class 'Gnuplot' is based on code by Daniel Stahlke.
//
//      The conditional compilation code for 'GNUPLOT_ENABLE_PTY'
//      and 'GNUPLOT_ENABLE_BLITZ' was removed, as neither
//      feature is needed here.  The various iostream
//      manipulators (like 'std::setprecision') now stream
//      directly.  The run-time logging was improved.
//
//      New public member function 'Gnuplot::setPersist' was
//      added, as was data member 'Gnuplot::gnuplotTerm' to code
//      for the Gnuplot terminal device.  Later two new
//      'Gnuplot::setPlotWindow*' functions were added.
//
//      The various templated functions (such as
//      "Gnuplot::sendEntry") remain in the header to enable
//      implicit template instantiation.
//
//      Janert (2010) covers Gnuplot comprehensively, but does
//      not discuss compiled language interfaces (such as this
//      class).
//
//      The "e" that is sometimes streamed MAY invoke the gnuplot
//      command 'builtin-replot'.
//
//  Potential 'mingw' port issue
//
//      On 24-Aug-2011, the gnuplot mailing list
//      <gnuplot-info@lists.sourceforge.net> has a new thread
//      entitled "call gnuplot from c++ (windows) (Mathias
//      Anselmann)" which details problems porting running Linux
//      code to 'mingw' under Windows.
//
//  Formfeed
//
//      The 'dumb' terminal outputs a formfeed (^L or \f)
//      character prior to printing.  This is only noticeable in
//      a text editor like emacs.
//
//  Example
//
//        std::map<double, double> xyPts;
//        for ( double x = -2.0; x < 2.0; x += 0.01 ) xyPts[x] = x * x * x;
//
//        Gnuplot gp("gnuplot");                // your 'gnuplot' call
//        gp.setPersist();                      // plot window remains open
//        gp << "set xrange [-2:2]"                    << "\n"
//           << "set yrange [-2:2]"                    << "\n";
//        gp << "set title \"Cubic plot\""             << "\n";
//        gp << "plot '-' with lines title 'cubic'"    << "\n";
//        gp.send(xyPts);
//
//      A 'std::map' is naturally key-ordered.  The '-' is a
//      special filename which indicates that the data should be
//      read from the same device as the original command.
//
//      The keypress 'q' will close each plot window in turn --
//      this can be rather convenient.
//
//  Reference
//
//      Janert, Philipp K.  2010.  Gnuplot in action :
//        understanding data with graphs.  Manning Publications,
//        Connecticut, USA.  ISBN-13 978-1-933988-39-9.
//
// ---------------------------------------------------------

class Gnuplot :
  public boost::iostreams::stream<boost::iostreams::file_descriptor_sink>
{
  // LOCAL ENUMERATIONS

public:

  enum LogLevel
    {
      e_debugLogsOff = 0,                    // treat as 'false'
      e_debugLogsOn  = 1                     // treat as 'true'
    };

  // DISABLED

private:

  Gnuplot();                                 // zero-argument constructor
  Gnuplot(const Gnuplot& orig);              // copy constructor
  Gnuplot& operator= (const Gnuplot& orig);  // copy assignment operator

  // CREATORS

public:

  explicit
  Gnuplot
  (const std::string       cmd,
   const Gnuplot::LogLevel level = Gnuplot::e_debugLogsOff);

  ~Gnuplot();

  // BEHAVIOR SET FUNCTIONS

public:

  void setTerminal(const std::string& term);                  // "dumb" "x11" "wxt" "svg"
  void setPersist();                                          // plot window remains
  void setPlotWindowTitle(const std::string& windowTitle);    // plot window title
  void setPlotWindowSize(const int width, const int height);  // plot window size
  void setOutput(const std::string& output);                  // filename, SVG only
  void setFinalize();                                         // finalize, SVG only

  // DATA LOAD FUNCTIONS

public:

  template <class T>
  Gnuplot& send(T p, T last)                      // used for one STL container
  {
    while ( p != last )
      {
        sendEntry(*p);                            // private member function
        *this << "\n";
        ++p;
      }
    *this << "e" << std::endl;
    return *this;
  }

  template <class T, class U>
  Gnuplot& send(T x, T x_last, U y, U y_last)     // used for two STL containers
  {
    while (x != x_last && y != y_last )
      {
        sendEntry(*x, *y);                        // private member function
        *this << "\n";
        ++x;
        ++y;
      }
    assert ( x == x_last && y == y_last );        // assert inputs same size
    *this << "e" << std::endl;
    return *this;
  }

  template <class Iter>
  Gnuplot& send (Iter arr)                        // wrapper for both STL containers
  {
    send(arr.begin(), arr.end());                 // wrapped call
    return *this;
  }

  // UTILITY FUNCTIONS

private:

  template <class T>
  void sendEntry(T v)                             // workhorse call
  {
    *this << v << " ";
  }

  template <class T, class U>
  void sendEntry(std::pair<T, U> v)               // wrapper to wrapper
  {
    sendEntry(v.first, v.second);
  }

  template <class T, class U>
  void sendEntry(T t, U u)                        // wrapper
  {
    sendEntry(t);
    sendEntry(u);
  }

  void
  logme
  (const std::string msg);

  void
  logme
  (const std::string msg,
   const std::string file,
   const int         line,
   const std::string func);

  // DATA

private:

  FILE*                pout;                 // C-style stdlib io
  std::string          gnuplotTerm;          // Gnuplot terminal device
  const bool           say;                  // 'true' means debug report

  const std::string    objectNo;             // unique for every object
  static int           ctorCount;            // class-wide counter

}; // class 'Gnuplot'

#endif // _GNUPLOT_H_

//  end of file

