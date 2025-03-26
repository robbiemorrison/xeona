//  XEONA ENERGY SYSTEMS MODELING ENVIRONMENT
//
//  file-create-name : gnuplot.ut0.cc
//  file-create-date : Tue 24-May-2011 21:44 UTC
//  file-initiator   : Robbie Morrison <robbie@actrix.co.nz>
//
//  file-role        : gnuplot interface, originally by Daniel Stahlke / unit test
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
//  $URL: file:///home/robbie/svn-root/xeona/futz/trunk/xeona1/i/gnuplot.ut0.cc $
//
//  GENERAL NOTES FOR THIS FILE
//
//  This is a unit test file and hence not essential to the
//  application proper.
//
//  AD-HOC NOTES
//
//  Note preprocessor macro 'GNUPLOT_TERM' to control the plot
//  type.  This is normally set to "dumb" so that the unit test
//  scripts do not require manual intervention to close
//  interactive windows and still record some evidence of
//  plotting.  The other option would be to omit the window
//  persist call.

//  LOCAL AND SYSTEM INCLUDES

#include "gnuplot.h"          // unit under test (place early)

#include "../a/logger.h"      // standard logging functionality (as required)
#include "../c/smart_ptr.h"   // toggle between Boost and TR1 smart pointers
#include ".././common.h"      // common definitions for project (place last)

#include <string>             // C++ strings
#include <sstream>            // string-streams

//  PREPROCESSOR MACROS FOR CODE-SWITCH PURPOSES

#define GNUPLOT_TERM 1 // 1 "dumb" terminal, 2 "x11" terminal, 3 "wxt" terminal

//  CODE

// ---------------------------------------------------------
//  FILE-LOCAL VAR  : ::gnuplot
// ---------------------------------------------------------

namespace
{
  const std::string gnuplot = "gnuplot";     // gnuplot invocation string
}

// ---------------------------------------------------------
//  MAIN FUNCTION   : main
// ---------------------------------------------------------

int
main(int argc, char* argv[])
{
  logga::spLogger logger = logga::ptrLogStream();  // main function logger
  logger->repx(logga::info, "beginning of main", "~~~~~~~~~~");
  logger->addSmartBlank();
  logger->setReportLevel(logga::adhc);       // maximum reporting

  // PRELIMINARY

  xeona::yeek = 1;                           // maximum yeeking
  std::ostringstream put;

  // ---------------------------------------------------------
  //  test ONE        : basic instantiation
  // ---------------------------------------------------------

  logger->test(1, "basic instantiation");

  {
    logger->addDumbBlank(logga::dbug);

    Gnuplot gp(::gnuplot, Gnuplot::e_debugLogsOn);

  } // 'gp' goes out of scope here

  // ---------------------------------------------------------
  //  test TWO        : original test example
  // ---------------------------------------------------------

  logger->test(2, "original test example");

  {
    logger->addDumbBlank(logga::dbug);

    std::map<double, double> xyPts;
    for ( double x = -2.0; x < 2.0; x += 0.01 )
      {
        xyPts[x] = x * x * x;
      }

#if (GNUPLOT_TERM == 1)

    Gnuplot gp(::gnuplot, Gnuplot::e_debugLogsOn);
    gp.setTerminal("dumb");
    gp.setPlotWindowSize(80, 25);            // width (cols) x height (lines)

#elif (GNUPLOT_TERM == 2)

    Gnuplot gp(::gnuplot, Gnuplot::e_debugLogsOn);
    gp.setTerminal("x11");
    gp.setPersist();
    gp.setPlotWindowSize(1000, 600);         // width (px) x height (px)
    gp.setPlotWindowTitle("test two");       // gets confused under 'valgrind'

#elif (GNUPLOT_TERM == 3)

    Gnuplot gp(::gnuplot, Gnuplot::e_debugLogsOn);
    gp.setTerminal("wxt");
    gp.setPersist();                         // plot window remains open
    gp.setPlotWindowSize(1100, 600);         // width (px) x height (px)
    gp.setPlotWindowTitle("test two");

#endif // 'GNUPLOT_TERM'

    gp << "set xrange [-2:2]"                    << "\n"
       << "set yrange [-2:2]"                    << "\n";
    gp << "set title \"cubic plot\""             << "\n";
    gp << "plot '-' with lines title 'cubic'"    << "\n";
    gp.send(xyPts);

  }

  // ---------------------------------------------------------
  //  test THREE      : okayGnuplotTerm test
  // ---------------------------------------------------------

  logger->test(3, "okayGnuplotTerm test");

  {
    put << std::boolalpha;
    put << "  " << "dumb : " << xeona::okayGnuplotTerm("dumb") << "\n";
    put << "  " << "daft : " << xeona::okayGnuplotTerm("daft") << "\n";
    logger->putx(logga::dbug, put);
  }

  // ---------------------------------
  //  HOUSEKEEPING
  // ---------------------------------

  logger->test(0);                           // test end

  logger->repx(logga::info, "end of main", "~~~~~~~~~~");
  return xeona::exit_success;

} // main function

//  PREPROCESSOR MACROS UNDEFINE

#undef GNUPLOT_TERM

//  end of file

