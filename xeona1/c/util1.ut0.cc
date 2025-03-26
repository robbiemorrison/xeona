//  XEONA ENERGY SYSTEMS MODELING ENVIRONMENT
//
//  file-create-name : utils.test.cc
//  file-create-date : Thu 14-Jun-2007 16:02 UTC
//  file-initiator   : Robbie Morrison <robbie@actrix.co.nz>
//
//  file-role        : free functions which offer general utilities 1 / unit test
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
//  $URL: file:///home/robbie/svn-root/xeona/futz/trunk/xeona1/c/util1.ut0.cc $
//
//  GENERAL NOTES FOR THIS FILE
//
//  This is a unit test file and hence not essential to the
//  application proper.

//  LOCAL AND SYSTEM INCLUDES

#include "util1.h"            // unit under test (place early)

#include "../a/logger.h"      // standard logging functionality (as required)
#include ".././common.h"      // common definitions for project (place last)

#include <cstdlib>            // C-style exit(), getenv(), system(), NULL, EXIT_SUCCESS
#include <deque>              // STL sequence container, double-ended vector
#include <iostream>           // standard io
#include <sstream>            // string-streams
#include <string>             // C++ strings
#include <typeinfo>           // run-time type information (RTTI)

#include <boost/algorithm/string.hpp>   // string recasing, trimming, splitting
#include <boost/cast.hpp>               // numeric_cast<>()
#include <boost/foreach.hpp>            // BOOST_FOREACH macro
#include <boost/format.hpp>             // printf style formatting

#include <boost/logic/tribool.hpp>      // three state boolean
#include <boost/logic/tribool_io.hpp>   // CAUTION: essential for std::boolalpha reporting

//  NAMESPACE DECLARATIONS

using boost::logic::tribool;            // three state boolean
using boost::logic::indeterminate;      // allows 'indeterminate' and 'indeterminate()'

//  FILE-SCOPE GLOBAL VARIABLE

namespace
{

#if 0 // 0 = routine testing, 1 = occasional testing
  const int throwCount = 1000000;
#else
  const int throwCount = 10000;
#endif

} // unnamed namespace

//  CODE

// ---------------------------------------------------------
//  FREE FUNCTION   : ::unitFormatedOutput
// ---------------------------------------------------------
//  Description  : provide additional reporting
//  Calls        : xeona::fmtQuantity
//  Role         : to support unit testing (leave here)
// ---------------------------------------------------------

namespace
{
  template<typename T>
  void
  unitFormatedOutput
  (const T           num,
   const std::string unit)
  {
    // some Boost.Format format specifier: g = general float
    // format, [minus] = left align, '[space]'= space for omitted
    // +, [00] is min width, no precision requirements are set
    // here (but would follow a [point])

    logga::spLogger logger = logga::ptrLogStream();

    std::string strInput
      = boost::str(boost::format("%' 'g %s") % num % unit);

    std::ostringstream put;
    put << "    original/formatted"
        << boost::format("  %-15s") % strInput
        << xeona::fmtQuantity(num, unit)     // test call
        << "\n";
    logger->putx(logga::dbug, put);
  }

} // unnamed namespace

// ---------------------------------------------------------
//  CLASS           : NineClass
//  CLASS           : CmWork
//  CLASS           : TemClass <>
//  CLASS           : Class
// ---------------------------------------------------------

class NineClass { };                         // basic definition

class CmThermalFluid { };                    // basic definition

template <typename T>
class TemClass { };                          // basic definition

class Class { public: Class(std::string str){ } };

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

  std::ostringstream put;

  // ---------------------------------------------------------
  //  test ONE        : random number functions
  // ---------------------------------------------------------

  logger->test(1, "random number functions");

  {
    const int range = 15;

    put << "  repeatedly calling getRandom(0, 9)"           << "\n";
    put << "  ";
    for (int i = 0; i < range; ++i)
      put << xeona::getRandom(0, 9) << " ";
    put << "\n";
    logger->putx(logga::dbug, put);

    put << ""                                               << "\n";
    put << "  repeatedly calling getRandom(0, 5)"           << "\n";
    put << "  ";
    for (int i = 0; i < range; ++i)
      put << xeona::getRandom(0, 5) << " ";
    put << "\n";
    logger->putx(logga::dbug, put);

    put << "  repeatedly calling getRandom(0.0, 5.0)"       << "\n";
    put << "  ";
    for (int i = 0; i < range; ++i)
      put << xeona::getRandom(0.0, 5.0) << " ";
    put << "\n";
    logger->putx(logga::dbug, put);

    int total = 0;
    int count = throwCount;
    put << "\n" << "  finding average getRandom(1, 6)"      << "\n";
    for (int i = 0; i < count; ++i)
      total += xeona::getRandom(1, 6);
    put << "  average (3.5) after " << count << " throws : "
        << boost::numeric_cast<double>(total)/count
        << "\n";
    logger->putx(logga::dbug, put);

    total = 0;
    count = throwCount;
    put << "\n" << "  finding average getRandom(1, 99)"     << "\n";
    for (int i = 0; i < count; ++i)
      total += xeona::getRandom(1, 99);
    put << "  average (50) after " << count << " throws : "
        << boost::numeric_cast<double>(total)/count
        << "\n";
    logger->putx(logga::dbug, put);
  }

  // ---------------------------------------------------------
  //  test TWO        : unit formatted output
  // ---------------------------------------------------------

  logger->test(2, "unit formatted output");

  logga::Rank prior = logger->setReportLevel(logga::dbug);  // reduce reporting

  {
    put << "  double calls" << "\n" << "\n";
    logger->putx(logga::dbug, put);

    ::unitFormatedOutput<double>(0.0001,     "W");     //  l00 uW
    ::unitFormatedOutput<double>(-200.0,     "W");     // -200 W
    ::unitFormatedOutput<double>(1000.2,     "W");     //  1.0002 kW
    ::unitFormatedOutput<double>(999000.0,   "W");     //  999 kW
    ::unitFormatedOutput<double>(50e+06,     "W");     //  50 MW
    ::unitFormatedOutput<double>(0.0,        "W");     //  0 kW
    ::unitFormatedOutput<double>(0.0,        "D");     //  0 D
    ::unitFormatedOutput<double>(1300000,    "kg");    //  1.3 Mkg (ktonne)

    put << "\n" << "  int calls" << "\n" << "\n";
    logger->putx(logga::dbug, put);

    ::unitFormatedOutput<int>(0,          "I");        //  0 I
    ::unitFormatedOutput<int>(1200,       "kg");       //  1.2 kkg (tonne)
    ::unitFormatedOutput<int>(1300000,    "kg");       //  1.3 Mkg (ktonne)
    ::unitFormatedOutput<int>(14000000,   "kg");       //  13  Mkg (ktonne)
  }

  logger->setReportLevel(prior);             // return to maximum reporting

  // ---------------------------------------------------------
  //  test THREE      : price rates
  // ---------------------------------------------------------

  logger->test(3, "price rates");

  {
    put << "    " << xeona::fmtPriceRate(0.000031, "W")  << "\n";
    put << "    " << xeona::fmtPriceRate(40,       "W")  << "\n";
    put << "    " << xeona::fmtPriceRate(-0.550,   "kg") << "\n";
  logger->putx(logga::dbug, put);
  }

  // ---------------------------------------------------------
  //  test FOUR        : horizonize
  // ---------------------------------------------------------

  logger->test(4, "horizonize");

  {
    const int horizon = 7;
    std::vector<std::string> pattern;
    std::vector<std::string> timeseries(horizon);
    pattern.push_back("aa");
    pattern.push_back("bb");
    pattern.push_back("cc");
    xeona::vectorRepeat(pattern, timeseries);

    put << "    pattern length    : " << pattern.size()    << "\n";
    put << "    timeseries length : " << timeseries.size() << "\n";
    put << ""                                              << "\n";
    logger->putx(logga::dbug, put);

    xeona::vectorReport(pattern);
    xeona::vectorReport(timeseries);
  }

  // ---------------------------------------------------------
  //  test FIVE        : empty smart pointers
  // ---------------------------------------------------------

  logger->test(5, "empty smart pointers");

  {
    // empty
    shared_ptr<int> spInt;
    put << "    " << xeona::reportShared_ptr(spInt) << "\n";     // implicit instantiation
    logger->putx(logga::dbug, put);

    // null
    spInt = xeona::makeNullShared_ptr<int>();  // = shared_ptr<int>(static_cast<int*>(0));
    put << "    " << xeona::reportShared_ptr(spInt) << "\n";
    logger->putx(logga::dbug, put);

    // single
    spInt = shared_ptr<int>(new int(5));
    put << "    " << xeona::reportShared_ptr(spInt) << "\n";
    logger->putx(logga::dbug, put);

    // multiple(3)
    shared_ptr<int> spInt2(spInt);           // copy construct
    shared_ptr<int> spInt3 = spInt;          // copy assign
    put << "    " << xeona::reportShared_ptr(spInt) << "\n";
    logger->putx(logga::dbug, put);

    // empty
    spInt.reset();
    put << "    " << xeona::reportShared_ptr(spInt) << "\n";
    logger->putx(logga::dbug, put);
  }

  // ---------------------------------------------------------
  //  test SIX        : non-Logger logging
  // ---------------------------------------------------------

  logger->test(6, "non-Logger logging");

  {
    put << "  about to make 'xeona::logDirect' call"  << "\n";
    put << ""                                         << "\n";
    logger->putx(logga::xtra, put);

    xeona::logDirect(__FILE__, __LINE__, __func__, "test call", 1.23456789);
  }

  // ---------------------------------------------------------
  //  test SEVEN      : reverseSequence
  // ---------------------------------------------------------

  logger->test(7, "reverseSequence");

  {
    std::deque<int> deck;

    xeona::reverseSequence(deck);            // trial empty container

    deck.push_back(1);
    deck.push_back(2);
    deck.push_back(3);
    deck.push_back(4);
    deck.push_back(5);
    deck.push_back(6);

    put << "    original deck        :";
    BOOST_FOREACH( int i, deck )
      put << " " << i;
    put << "\n";

    put << "    reversed deck        :";
    BOOST_FOREACH( int i, xeona::reverseSequence(deck) )
      put << " " << i;
    put << "\n";

    logger->putx(logga::xtra, put);
  }

  // ---------------------------------------------------------
  //  test EIGHT      : tailCombine
  // ---------------------------------------------------------

  // CAUTION: requires explicit template instantiation in source file

  logger->test(8, "tailCombine");

  {
    put << "  about to make 'xeona::tailCombine' call"
        << " with 'std::vector<int>' specialization"   << "\n";
    put << ""                                          << "\n";
    logger->putx(logga::xtra, put);

    typedef std::vector<int> vint;

    vint com;                                // modified vector
    vint add;                                // vector to be added to tail

    com.push_back(  1);
    com.push_back( 11);
    com.push_back(111);
    add.push_back(  2);
    add.push_back( 22);
    add.push_back(222);

    put << "    size (before) : " << com.size() << "\n";

    xeona::tailCombine(com, add);            // main call

    put << "    size (after)  : " << com.size() << "\n";
    logger->putx(logga::xtra, put);

    put << "    com           :";
    BOOST_FOREACH( int i, com )
      put << " " << i;
    put << "\n";
    logger->putx(logga::xtra, put);
  }

  // ---------------------------------------------------------
  //  test NINE       : compiler name demangling
  // ---------------------------------------------------------

  logger->test(9, "compiler name demangling");

  {
    std::string str;

    str = typeid(NineClass).name();
    put << "  class       : " << str << " > " << xeona::demangle(str) << "\n";
    logger->putx(logga::dbug, put);

    str = typeid(TemClass<CmThermalFluid>).name();
    put << "  class <>    : " << str << " > " << xeona::demangle(str) << "\n";
    logger->putx(logga::dbug, put);

    str = typeid(std::vector<double>).name();
    put << "  fundamental : " << str << " > " << xeona::demangle(str) << "\n";
    logger->putx(logga::dbug, put);
  }

  // ---------------------------------------------------------
  //  test TEN        : mapCombine
  // ---------------------------------------------------------

  typedef std::map<shared_ptr<Class>, shared_ptr<Class> > map_type;

  logger->test(10, "mapCombine");

  {
    map_type map1;                           // combined map
    map1.insert(std::make_pair(shared_ptr<Class>(new Class("a")),
                               shared_ptr<Class>(new Class("1"))));
    map1.insert(std::make_pair(shared_ptr<Class>(new Class("c")),
                               shared_ptr<Class>(new Class("3"))));

    map_type map2;                           // addition map
    map2.insert(std::make_pair(shared_ptr<Class>(new Class("b")),
                               shared_ptr<Class>(new Class("2"))));
    map2.insert(std::make_pair(shared_ptr<Class>(new Class("d")),
                               shared_ptr<Class>(new Class("4"))));
    map2.insert(std::make_pair(shared_ptr<Class>(new Class("e")),
                               shared_ptr<Class>(new Class("5"))));

    put << "  map1 size before : " << map1.size() << "\n"
        << "  map2 size before : " << map2.size() << "\n";

    xeona::mapCombine(map1, map2);           // key call

    put << "  map1 size after  : " << map1.size() << "\n"
        << "  map2 size after  : " << map2.size() << "\n";

    logger->putx(logga::dbug, put);
  }

  // ---------------------------------------------------------
  //  test ELEVEN     : vector absolute
  // ---------------------------------------------------------

  logger->test(11, "vector absolute");

  {
    shared_ptr<std::vector<double> > raw(new std::vector<double>());
    raw->push_back(-2.01);
    raw->push_back(-1.01);
    raw->push_back(+0.00);
    raw->push_back(+1.02);
    raw->push_back(+2.02);

    shared_ptr<std::vector<double> > abs = xeona::vectorAbs(raw);

    put << std::showpos;
    put << "  raw front : " << raw->front() << "\n"
        << "  abs front : " << abs->front() << "\n"
        << "  raw back  : " << raw->back()  << "\n"
        << "  abs back  : " << abs->back()  << "\n";
    logger->putx(logga::dbug, put);
  }

  // ---------------------------------
  //  HOUSEKEEPING
  // ---------------------------------

  logger->test(0);                           // test end

  logger->repx(logga::info, "end of main", "~~~~~~~~~~");
  return xeona::exit_success;

} // main function

//  end of file

