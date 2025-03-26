//  XEONA ENERGY SYSTEMS MODELING ENVIRONMENT
//
//  file-create-name : tsset.ut0.cc
//  file-create-date : Tue 24-May-2011 23:58 UTC
//  file-initiator   : Robbie Morrison <robbie@actrix.co.nz>
//
//  file-role        : timeseries classes for added functionality / unit test
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
//  $URL: file:///home/robbie/svn-root/xeona/futz/trunk/xeona1/c/tsset.ut0.cc $
//
//  GENERAL NOTES FOR THIS FILE
//
//  This is a unit test file and hence not essential to the
//  application proper.
//
//  LOCAL AND SYSTEM INCLUDES

#include "tsset.h"            // unit under test (place early)

#include "../a/logger.h"      // standard logging functionality (as required)
#include "../c/smart_ptr.h"   // toggle between Boost and TR1 smart pointers
#include ".././common.h"      // common definitions for project (place last)

#include <algorithm>          // STL copying, searching, and sorting, max()
#include <iterator>           // STL additional iterators, std::distance()
#include <numeric>            // STL numerical algorithms
#include <ostream>            // output streams
#include <sstream>            // string-streams
#include <string>             // C++ strings

#include <cstdlib>            // C-style exit(), getenv(), system(), NULL, EXIT_SUCCESS

#include <boost/assign/std/vector.hpp>  // assign operator+=() for std::vector

//  PREPROCESSOR MACROS FOR TEST PURPOSES

#define GNUPLOT_TERM "dumb"                  // dumb | wxt

// NAMESPACE DIRECTIVES

using namespace boost::assign;               // for: vec += 0, 1;

//  CODE

// ---------------------------------------------------------
//  FREE FUNCTION   : ::randomVector
// ---------------------------------------------------------
//  Description  : fill a vector with random numbers
//  Role         : basic code for unit tests
//  Techniques   : 'rand' from <cstdlib>
//  Status       : complete
//
//  Design notes
//
//      'std::generate_n'    is <algorithm>
//      'std;:back_inserter' is <iterator>
//      'rand'               is <cstdlib>
//
// ---------------------------------------------------------

namespace
{
  class Rand                                 // functor
  {
  public:
    Rand
    (const double lower,                     // lower bound
     const double upper) :                   // upper bound
      d_lower(lower),
      d_factor((upper - lower) / static_cast<double>(RAND_MAX))
    { }
    double operator()() { return d_lower + d_factor * rand(); }
  private:
    const double d_lower;
    const double d_factor;
  };

  std::vector<double>
  randomVector
  (const int    items,                       // vector size
   const double lower = 0.0,                 // lower bound
   const double upper = 1.0)                 // upper bound
  {
    std::vector<double> x;
    x.reserve(items);
    std::generate_n(std::back_inserter(x), items, Rand(lower, upper));
    return x;
  }
}

// ---------------------------------------------------------
//  FREE FUNCTION   : ::printVector <>
// ---------------------------------------------------------
//  Description  : print a vector
//  Role         : utility
//  Techniques   : template, 'std::copy', statistics
//  Status       : complete
//
//  Typical output: msg size mean values
//
//      v1         :   7    6.12857    1.200 2.300 4.500 5.000 8.700 9.000 12.200
//
//  CAUTION: adds a normally unwanted trailing space
//
// ---------------------------------------------------------

namespace
{
  template <typename T>
  void
  printVector
  (const std::string&    msg,                // prepended message
   const std::vector<T>& vec,                // source vector
   std::ostream&         os)
  {
    const int    tab   = 10;
    const int    count = vec.size();
    const double mean  = std::accumulate(vec.begin(), vec.end(), 0.0) / count;

    os << "  ";
    os << std::left << std::setw(tab) << msg << " : ";
    os << std::right << std::setw(3) << count;
    if ( ! vec.empty() )
      {
        os << "  ";
        os << std::fixed << std::setprecision(5) << std::setw(9) << mean << "    ";
        os << std::fixed << std::setprecision(3);
        std::copy(vec.begin(), vec.end(), std::ostream_iterator<T>(os, " "));
      }
    os << std::endl;
  }
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

  // set gnuplot plot control

  xeona::tout = GNUPLOT_TERM;

  // SPECIAL FOR THIS UNIT TEST

  const int fakeSteps    = 8760;
  const int fakeInterval = 1800;

  // declaration/definition only present under '_XUTEST'
  Entity::setHorizonDirectly(fakeSteps, fakeInterval);

  // ---------------------------------------------------------
  //  test ONE        : static calls to TsBase
  // ---------------------------------------------------------

  logger->test(1, "static calls to TsBase");

  {
    // static calls -- will both persist and affect program wide behavior
    TsBase::checksOn();
    TsBase::checksOff();
    TsBase::checksOn();
  }

  // ---------------------------------------------------------
  //  test TWO        : TsNormal instantiation
  // ---------------------------------------------------------

  logger->test(2, "TsNormal instantiation");

  {
    // load vector and create timeseries
    shared_ptr<std::vector<double> > data(new std::vector<double>());
    *data +=  2,  4,  8, 10, 99,  1;
    TsNormal ndata(data, 3600, "test 2");    // timeseries wrapper
  }

  // ---------------------------------------------------------
  //  test THREE      : TsMonthly instantiation
  // ---------------------------------------------------------

  logger->test(3, "TsMonthly instantiation");

  {
    // load vector and create monthseries
    shared_ptr<std::vector<double> > data(new std::vector<double>());
    *data +=  5,  3, 34, 20, 30, 28, 16, 39, 34, 50, 38, 29;
    TsMonthly mdata(data, "test 3");         // monthseries wrapper
  }

  // ---------------------------------------------------------
  //  test FOUR       : TsNormal manipulation
  // ---------------------------------------------------------

  logger->test(4, "TsNormal manipulation");

  {
    // 'data1' example from Stephens etal (2006 p405)
    // values: 2,4,8,10,99,1
    // count = 6, sum = 124, mean = 20.6667, variance = 1237.22, stddev = 35.1742

    // load vector
    shared_ptr<std::vector<double> > data(new std::vector<double>());
    *data +=  2,  4,  8, 10, 99,  1;

    // print vector
    ::printVector("data", *data, put);
    logger->putx(logga::dbug, put);

    // create timeseries
    TsNormal ndata(data, 3600, "stevens 2006");
    ndata.setCaller(__func__);

    // report
    ndata.report(put);
    logger->addSmartBlank(logga::dbug);
    logger->putx(logga::dbug, put);

    // recover and print vector
    shared_ptr<std::vector<double> > out1 = ndata.pointer();
    ::printVector("out1", *out1, put);
    logger->addSmartBlank(logga::dbug);
    logger->putx(logga::dbug, put);

    // rescale timeseries
    ndata.rescale(1.2, "120%");
    ndata.report(put);
    logger->addSmartBlank(logga::dbug);
    logger->putx(logga::dbug, put);

    // recover and print a second vector
    shared_ptr<std::vector<double> > out2 = ndata.pointer();
    ::printVector("out2", *out2, put);
    logger->addSmartBlank(logga::dbug);
    logger->putx(logga::dbug, put);
  }

  // ---------------------------------------------------------
  //  test FIVE       : TsMonthly manipulation
  // ---------------------------------------------------------

  logger->test(5, "TsMonthly manipulation");

  {
    // load vector
    shared_ptr<std::vector<double> > data(new std::vector<double>());
    *data +=  5,  3, 34, 20, 30, 28, 16, 39, 34, 50, 38, 29;

    // create timeseries
    TsMonthly mdata(data, "test 5");

    // report
    mdata.report(put);
    logger->putx(logga::dbug, put);

    // resample
    const int    startOffset = 0;
    const bool   leapYear    = true;
    const double scaleFactor = 1.3;
    const double offset      = 0.0;

    TsNormal ndata = mdata.sampleDuplicate("new",
                                           7200,
                                           startOffset,
                                           leapYear,
                                           scaleFactor,
                                           offset);

    // report
    ndata.report(put);
    logger->addSmartBlank(logga::dbug);
    logger->putx(logga::dbug, put);
  }

  // ---------------------------------------------------------
  //  test SIX        : copy construction and copy assignment
  // ---------------------------------------------------------

  logger->test(6, "copy construction and copy assignment");

  {
    // load vector
    shared_ptr<std::vector<double> > data(new std::vector<double>());
    *data +=  2,  4,  8, 10, 99,  1;

    // class 'TsNormal'
    TsNormal ndata1(data, 3600, "original");

    // copy
    TsNormal ndata2(ndata1);                 // copy construction
    TsNormal ndata3 = ndata2;                // copy assignment

    // test and report
    put << std::boolalpha
        << "  are 'ndata1' and 'ndata2' equal : " << (ndata1 == ndata2) << "\n"
        << "  are 'ndata1' and 'ndata3' equal : " << (ndata1 == ndata3) << "\n"
        << "  are 'ndata2' and 'ndata3' equal : " << (ndata2 == ndata3) << "\n";
    logger->putx(logga::dbug, put);

    // class 'TsMonthly'
    TsMonthly mdata1(data, "original");

    // copy
    TsMonthly mdata2(mdata1);                 // copy construction
    TsMonthly mdata3 = mdata2;                // copy assignment

    // test and report
    put << std::boolalpha
        << "  are 'mdata1' and 'mdata2' equal : " << (mdata1 == mdata2) << "\n"
        << "  are 'mdata1' and 'mdata3' equal : " << (mdata1 == mdata3) << "\n"
        << "  are 'mdata2' and 'mdata3' equal : " << (mdata2 == mdata3) << "\n";
    logger->addSmartBlank(logga::dbug);
    logger->putx(logga::dbug, put);
  }

  // ---------------------------------------------------------
  //  test SEVEN      : further manipulation and public calls
  // ---------------------------------------------------------

  logger->test(7, "further manipulation and public calls");

  {
    // create vector
    shared_ptr<std::vector<double> > data(new std::vector<double>());
    *data +=  4,  8, 16, 20,  4;

    // create timeseries
    TsNormal ndata(data, 3600, "original");

    // gather information, undertake addition
    const double oldmean = ndata.getMean();
    const bool   ret     = ndata.add(ndata, "self added");      // self-addition is legal
    const double newmean = ndata.getMean();

    put << std::boolalpha
        << "  metadata was compatible : " << ret     << "\n"
        << "  old mean                : " << oldmean << "\n"
        << "  new mean                : " << newmean << "\n";
    logger->putx(logga::dbug, put);
  }

  // ---------------------------------------------------------
  //  test EIGHT      : sampling
  // ---------------------------------------------------------

  logger->test(8, "sampling");

  {
    // load vector
    const int    items = 365;                // one non-leap year
    const double lower = -50;
    const double upper = +100;
    std::vector<double> random = ::randomVector(items, lower, upper);

    // create timeseries
    const bool hasFeb29  = false;
    const bool wrappable = false;
    TsNormal ydata(random, 24 * 3600, "yearly", hasFeb29, wrappable);

    // resample
    const int    offset      = 0;
    const bool   leapYear    = false;
    const double scaleFactor = 1.0;
    TsNormal resam1 = ydata.sampleLinear("default");  // default 'TimeHorizon' interval
    TsNormal resam2 = ydata.sampleLinear("speced", 1800, offset, leapYear, scaleFactor);

    // report
    ydata.report(put);
    resam1.report(put);
    resam2.report(put);
    logger->putx(logga::dbug, put);
  }

  // ---------------------------------------------------------
  //  test NINE       : gnuplot 1
  // ---------------------------------------------------------

  logger->test(9, "gnuplot 1");

  {
    // load vectors
    shared_ptr<std::vector<double> > data1(new std::vector<double>());
    shared_ptr<std::vector<double> > data2(new std::vector<double>());
    shared_ptr<std::vector<double> > data3(new std::vector<double>());
    *data1 +=  2,  4,  8, 10, 99,  1;
    *data2 +=  2,  1, 50, 49;
    *data3 +=  5,  3, 34, 20, 30, 28, 16, 39, 34, 50, 38, 29;

    // create timeseries
    TsNormal ndata1(data1, 3600, "data 1");
    TsNormal ndata2(data2, 3600, "data 2");
    TsNormal ndata3(data3, 1800, "data 3");

    // gnuplot
    TsGnuplot gp;                            // plotting object with default command
    gp.addCaller(__FILE__,__LINE__);         // more whitespace is okay
    gp.addTs(ndata1);
    gp.addTs(ndata2);
    gp.addTs(ndata3);
    gp.plot("test 8 plot", 0, 'p');          // last two arguments are optional

  }

  // ---------------------------------------------------------
  //  test TEN        : gnuplot 2
  // ---------------------------------------------------------

  logger->test(10, "gnuplot 2");

  {
    // load vector
    const int    items = 365;                // one non-leap year
    const double lower = -50;
    const double upper = +100;
    std::vector<double> random = ::randomVector(items, lower, upper);

    // create timeseries
    const bool hasFeb29  = false;
    const bool wrappable = false;
    TsNormal ydata(random, 24 * 3600, "yearly", hasFeb29, wrappable);
    ydata.setCaller(__func__);

    // resample
    const int hourly = 12;                   // can drop to one
    TsNormal resam = ydata.sampleLinear("new", hourly * 3600);
    resam.setCaller(__func__);

    // report
    ydata.report(put);
    resam.report(put);
    logger->putx(logga::dbug, put);

    // gnuplot
    TsGnuplot gp;                            // plotting object with default command
    gp.addCaller( __FILE__ , __LINE__ );     // less whitespace is okay
    gp.addTs(ydata);
    gp.addTs(resam);
    gp.plot("test 10 plot", 0, 'p');         // last two arguments are optional

    // summarize
    const int tab = ydata.getLabel().size(); // perhaps a little over the top
    put << "  " << ydata.summary(tab) << "\n";
    put << "  " << resam.summary(tab) << "\n";
    logger->addSmartBlank(logga::dbug);
    logger->putx(logga::dbug, put);
  }

  // ---------------------------------------------------------
  //  test ELEVEN     : gnuplot monthly
  // ---------------------------------------------------------

  logger->test(11, "gnuplot monthly");

  {
    // load vector and create monthseries
    shared_ptr<std::vector<double> > data(new std::vector<double>());
    *data +=  5,  3, 34, 20, 30, 28, 16, 39, 34, 50, 38, 29;
    TsMonthly mdata(data, "test 11");         // monthseries wrapper

    TsGnuplot gp;
    gp.addTs(mdata);
    gp.addCaller( __FILE__ , __LINE__ );     // less whitespace is okay
    gp.plot("test 11 plot", 0, 's');
  }

  // ---------------------------------------------------------
  //  test TWELVE     : TsNormal copyfill
  // ---------------------------------------------------------

  logger->test(12, "TsNormal copyfill");

  {
    // preamble
    std::vector<double> data;
    data +=  5,  3, 34, 20, 30, 28, 16, 39, 34, 50, 38, 29;
    TsNormal ndata(data, 3600, "test12");

    // copy fill
    const int cut = 3;
    std::vector<double> fill;
    fill += 888, 176;
    const int got = ndata.copyfill(fill, cut);

    // print vectors
    put << "  " << "cut = " << cut << "   got = " << got << "\n";
    ::printVector("data", data, put);
    ::printVector("fill",  fill, put);
    logger->addSmartBlank(logga::dbug);
    logger->putx(logga::dbug, put);
  }

  // ---------------------------------------------------------
  //  test THIRTEEN  : TsMonthly copyfill
  // ---------------------------------------------------------

  logger->test(13, "TsMonthly copyfill");

  {
    // preamble
    std::vector<double> data;
    data +=  5,  3, 34, 20, 30, 28, 16, 39, 34, 50, 38, 29;
    TsMonthly mdata(data, "test12");

    // copy fill
    const int cut = 3;
    std::vector<double> fill;
    const int got = mdata.copyfill(fill, cut);

    // print vectors
    put << "  " << "cut = " << cut << "   got = " << got << "\n";
    ::printVector("data", data, put);
    ::printVector("fill",  fill, put);
    logger->addSmartBlank(logga::dbug);
    logger->putx(logga::dbug, put);
  }

  // ---------------------------------------------------------
  //  test FOURTEEN   : dayInfo
  // ---------------------------------------------------------

  logger->test(14, "dayInfo");

  {
    // simulation data
    const int ipd      = 12;                  // only some values supported
    const int pivot    = 85;
    int sampleInterval = 3600;

    // preamble
    const int interval = (3600 * 24) / ipd;
    if ( sampleInterval == 0 ) sampleInterval = interval;

    // load vector
    const int    items = 365 * ipd;
    const double lower = -50;
    const double upper = +100;
    std::vector<double> random = ::randomVector(items, lower, upper);

    // create timeseries
    const bool hasFeb29  = false;
    const bool wrappable = false;
    TsNormal ydata(random, interval, "year of data", hasFeb29, wrappable);

    // day sample
    shared_ptr<std::vector<double> > dayinfo = ydata.dayInfo(pivot, interval);

    // print vector
    put << "  " << "original ipd = " << ipd
        << "   pivot = "             << pivot
        << "   sample interval = "   << sampleInterval
        << "\n";
    ::printVector("dayinfo", *dayinfo, put);
    logger->addSmartBlank(logga::dbug);
    logger->putx(logga::dbug, put);
  }

  // ---------------------------------------------------------
  //  test FIFTEEN    : repetition tests
  // ---------------------------------------------------------

  logger->test(15, "repetition tests");

  {
    // preamble
    const int steps    = 22;
    const int interval = 3600;
    const double value = 88.88;

    // create repetition timeseries
    TsNormal ndata1(steps, value, interval, "repetition constructor");
    ndata1.setCaller(__func__);

    // report
    ndata1.report(put);
    logger->addSmartBlank(logga::dbug);
    logger->putx(logga::dbug, put);

    // recover and print vector
    shared_ptr<std::vector<double> > out1 = ndata1.pointer();
    ::printVector("out1", *out1, put);
    logger->addSmartBlank(logga::dbug);
    logger->putx(logga::dbug, put);

    // create and repetition timeseries
    TsNormal ndata2("repetition load");
    ndata2.setCaller(__func__);
    ndata2.load(steps, value / 2.0, interval);

    // report
    ndata2.report(put);
    logger->addSmartBlank(logga::dbug);
    logger->putx(logga::dbug, put);

    // recover and print vector
    shared_ptr<std::vector<double> > out2 = ndata2.pointer();
    ::printVector("out2", *out2, put);
    logger->addSmartBlank(logga::dbug);
    logger->putx(logga::dbug, put);

  }

  // ---------------------------------
  //  HOUSEKEEPING
  // ---------------------------------

  logger->test(0);                           // test end

  logger->repx(logga::info, "end of main", "~~~~~~~~~~");
  return xeona::exit_success;

} // main function

#undef GNUPLOT_TERM

//  end of file

