//  XEONA ENERGY SYSTEMS MODELING ENVIRONMENT
//
//  file-create-name : utils.cc
//  file-create-date : Thu 14-Jun-2007 15:45 UTC
//  file-initiator   : Robbie Morrison <robbie@actrix.co.nz>
//
//  file-role        : free functions which offer general utilities 1 / implementation
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
//  $URL: file:///home/robbie/svn-root/xeona/futz/trunk/xeona1/c/util1.cc $

//  LOCAL AND SYSTEM INCLUDES

#include "util1.h"            // companion header for this file (place first)

#include "../a/logger.h"      // standard logging functionality (as required)
#include ".././common.h"      // common definitions for project (place last)

#include <algorithm>          // STL copying, searching, and sorting
#include <fstream>            // file-based io
#include <iomanip>            // setw() and family
#include <iostream>           // standard io
#include <iterator>           // STL additional iterators, std::distance()
#include <string>             // C++ strings

#include <cctype>             // C-style char classification, case conversion
#include <cmath>              // C-style maths, ceil(), floor(), sqrt()
#include <cstdlib>            // C-style exit(), getenv(), system(), NULL, EXIT_SUCCESS
#include <ctime>              // C-style time and date functions

#include <boost/random/mersenne_twister.hpp>    // random engine
#include <boost/random/uniform_int.hpp>         // random distribution
#include <boost/random/uniform_real.hpp>        // random distribution
#include <boost/random/variate_generator.hpp>   // random generator

#include <boost/algorithm/string.hpp>   // string recasing, trimming, splitting
#include <boost/cast.hpp>               // numeric_cast<>()
#include <boost/foreach.hpp>            // BOOST_FOREACH iteration macro
#include <boost/format.hpp>             // printf style formatting

//  FORWARD (PARTIAL) DECLARATIONS

class Record;                                // for 'tailCombine'
class Block;                                 // for 'mapCombine'
class Class;                                 // for 'mapCombine'
class DomainController;                      // for 'mapCombine'
class Entity;                                // for 'mapCombine'
class Gateway;                               // for 'mapCombine'

//  CODE

namespace xeona
{

  // ---------------------------------------------------------
  //  FREE FUNCTION   : xeona::getRandom (int, int)
  // ---------------------------------------------------------
  //  Description  : retrieve random ints in required range
  //  Status       : preliminary testing complete
  //
  //  Design notes
  //
  //      The Boost.Random library has been accepted into the TR1
  //      but is not explicitly supported by g++ 4.1.  This code
  //      relies on the Ubuntu package libboost-dev (Boost C++
  //      Libraries development files) for the necessary
  //      Boost.Random headers.  That said, explicit linking to
  //      say libboost_random is not required.
  //
  //      Reseeding
  //
  //      The global variable 'xeona::pepper', if true, creates a
  //      different seed for every run.  Otherwise the random
  //      engine default is used which leads to repeatability
  //      between runs.
  //
  //  CAUTIONS
  //
  //      See issues highlighted in the code proper.
  //
  //  See also
  //
  //      Boost.Random documentation.
  //
  //      Becker (2007 ch 13) devotes a chapter to the
  //      Boost.Random library.  But note that Karlsson (2006)
  //      does *not* cover the library at all.
  //
  //  References
  //
  //      Becker, Pete.  2007.  The C++ Standard Library
  //        extensions : a tutorial and reference.  Addison-Wesley,
  //        Upper Saddle River, New Jersey, USA.  ISBN
  //        0-321-41299-0.
  //
  // ---------------------------------------------------------

  int                                        // retrieve a random number
  getRandom
  (int lower,                                // inclusive lower bound
   int upper)                                // inclusive upper bound
  {
    // logger disabled due to high number of calls
    static logga::spLogger logger = logga::ptrLogStream();
    //std::string str = boost::str(boost::format("%1%, %2%") % lower % upper);
    //logger->repx(logga::dbug, "arguments lower, upper", str);

    // select a predefined pseudo-random number engine, in
    // this case based on the 'mersenne_twister' algorithm
    static boost::mt19937 eng;               // CAUTION: use static [1], do not seed [2]

    // create a variate_generator object
    boost::variate_generator                 // CAUTION: omit static [3]
      <
      boost::mt19937&,                       // CAUTION: ampersand required [4]
      boost::uniform_int<>                   // select uniform integer distribution
      >
      rint                                   // variate_generator object name
      (eng,                                  // bind generator
       boost::uniform_int<>(lower, upper));  // .. and distribution

    // [1] the engine should persist between function calls
    // [2] the engine should not be reseeded on each function call
    // [3] static will fix the range lower, upper on first call, not what
    //     is wanted (although static should be faster)
    // [4] see Becker (2007 p337)
    // [5] std::time(0) is not a very good random seed -- when
    //     called in rapid succession, it will return the same value
    //     and hence the same random number sequences will ensue
    // [6] to ensure run to run repeatability

    static bool firstCall = true;                 // note first call status
    if ( firstCall )
      {
        if ( xeona::pepper == true )              // controls which seed to use
          {
            std::time_t stamp = std::time(0);     // CAUTION: std::time is not ideal [5]
            boost::mt19937::result_type seed      // better than hardcoding 'unsigned'
              = static_cast<boost::mt19937::result_type>(stamp);
            eng.seed(seed);
            firstCall = false;
            logger->repx(logga::info, "integer form first call, time seed", seed);
          }
        else
          {
            firstCall = false;
            logger->repx(logga::info, "integer first call, default seed", "");
          }
      }

    return rint();                                // return random number
  }

  // ---------------------------------------------------------
  //  FREE FUNCTION   : xeona::getRandom (double, double)
  // ---------------------------------------------------------
  //  Description  : retrieve random doubles in required range
  //  Status       : preliminary testing complete
  //
  //  Design notes
  //
  //      See getRandom (int, int).
  //
  // ---------------------------------------------------------

  double                                     // retrieve a random number
  getRandom
  (double lower,                             // inclusive lower bound
   double upper)                             // inclusive upper bound
  {
    // logger disabled due to high number of calls
    static logga::spLogger logger = logga::ptrLogStream();
    //std::string str = boost::str(boost::format("%1%, %2%") % lower % upper);
    //logger->repx(logga::dbug, "arguments lower, upper", str);

    // select a predefined pseudo-random number engine, in
    // this case based on the 'mersenne_twister' algorithm
    static boost::mt19937 eng;               // CAUTION: use static [1], do not seed [2]

    // create a variate_generator object
    boost::variate_generator                 // CAUTION: omit static [3]
      <
      boost::mt19937&,                       // CAUTION: ampersand required [4]
      boost::uniform_real<>                  // select uniform real distribution
      >
      rdouble                                // variate_generator object name
      (eng,                                  // bind generator
       boost::uniform_real<>(lower, upper)); // .. and distribution

    // [1] the engine should persist between function calls
    // [2] the engine should not be reseeded on each function call
    // [3] static will fix the range lower, upper on first call, not what
    //     is wanted (although static should be faster)
    // [4] see Becker (2007 p337)
    // [5] std::time(0) is not a very good random seed -- when
    //     called in rapid succession, it will return the same value
    //     and hence the same random number sequences will ensue
    // [6] to ensure run to run repeatability

    static bool firstCall = true;                 // note first call status
    if ( firstCall )
      {
        if ( xeona::pepper == true )              // controls which seed to use
          {
            std::time_t stamp = std::time(0);     // CAUTION: std::time is not ideal [5]
            boost::mt19937::result_type seed      // better than hardcoding 'unsigned'
              = static_cast<boost::mt19937::result_type>(stamp);
            eng.seed(seed);
            firstCall = false;
            logger->repx(logga::info, "real form first call, time seed", seed);
          }
        else
          {
            eng.seed();                          // use default internal state [6]
            firstCall = false;
            logger->repx(logga::info, "real first call, default seed", "");
          }
      }

    return rdouble();                             // return random number
  }

  // ---------------------------------------------------------
  //  FREE FUNCTION   : xeona::fmtQuantity
  // ---------------------------------------------------------
  //  Purpose      : return a quantity in resolved SI format
  //  Status       : working
  //
  //  Design notes
  //
  //      Format %d for (double) 1000000 -> 1.0e+06.
  //      Format %d for    (int) 1000000 -> 1000000.
  //
  //      The SI micro prefix 'u' is a stand-in for 'mu' (unicode
  //      U+00B5, ascii dec 181 or octal 265 , LaTeX \mu).  One
  //      can use char '\256' but this needs extended-ASCII
  //      support on your (bash) shell -- to check if this is
  //      possible, try:
  //
  //          $ printf "%b\n" "\256" # (R) registered trademark symbol
  //
  //  Examples using 'W'
  //
  //          200      ->  0.2 kW
  //          3.3e+05  ->  330 kW
  //          -4.4e+06 -> -4.4 MW
  //
  //  Note on 'tonne' unit
  //
  //      The base SI mass unit is 'kg'.  Hence clients passing
  //      't' must do their own local conversion first:
  //
  //          double mass = 300;              // 300 kg
  //          fmtQuantity(mass/1000, "t");    // 0.3 t
  //
  //  Limitations (which arise from the SI system)
  //
  //          1000 kg  ->  1 kkg (t)
  //          0.001 kg ->  1 mkg (g)
  //          0.1 t    ->  100 mt (kg)
  //
  // ---------------------------------------------------------

  std::string
  fmtQuatity
  (const int         value,                  // wrapper for double version
   const std::string baseUnit)
  {
    return fmtQuantity(boost::numeric_cast<double>(value), baseUnit);
  }

  std::string
  fmtQuantity
  (const double      value,                  // numerical value
   const std::string baseUnit)               // for instance, 'W' for Watts
  {
    std::string pre;                         // SI prefix
    std::string pmv;                         // prefix-modified value
    std::string unt;                         // new unit
    std::string buf;                         // buffer to return

    if ( value == 0.0 )                      // strictly zero in floating point arithmetic
      {
        pre = "";                            // generally no prefix
        if ( baseUnit == "W" ) pre = "k";    // kW is an exception
        pmv = boost::str(boost::format("%' 'd") % 0);
      }
    else if ( std::abs(value) < 1.0e-06 )    // below one micro-baseUnit
      {
        pre = "n";                           // 'nano'
        pmv = boost::str(boost::format("%' 'd") % (value * 1.0e+09));
      }
    else if ( std::abs(value) < 1.0e-03 )    // below one milli-baseUnit
      {
        pre = "u";                           // 'micro' stand-in
     // pre = "\265";                        // 'micro' in extended ASCII (see note above)
        pmv = boost::str(boost::format("%' 'd") % (value * 1.0e+06));
      }
    else if ( std::abs(value) < 1.0e+00 )    // below one baseUnit
      {
        pre = "m";                           // 'milli'
        pmv = boost::str(boost::format("%' 'd") % (value * 1.0e+03));
      }
    else if ( std::abs(value) < 1.0e+03 )    // below one kilo-baseUnit
      {
        pre = "";                            // nothing
        pmv = boost::str(boost::format("%' 'd") % value);
      }
    else if ( std::abs(value) < 1.0e+06 )    // below one mega-baseUnit
      {
        pre = "k";                           // 'kilo'
        pmv = boost::str(boost::format("%' 'd") % (value / 1.0e+03));
      }
    else if ( std::abs(value) < 1.0e+09 )    // below one giga-baseUnit
      {
        pre = "M";                           // 'mega'
        pmv = boost::str(boost::format("%' 'd") % (value / 1.0e+06));
      }
    else if ( std::abs(value) < 1.0e+12 )    // below one tera-baseUnit
      {
        pre = "G";                           // 'giga'
        pmv = boost::str(boost::format("%' 'd") % (value / 1.0e+09));
      }
    else if ( std::abs(value) < 1.0e+15 )    // below one peta-baseUnit
      {
        pre = "T";                           // 'tera'
        pmv = boost::str(boost::format("%' 'd") % (value / 1.0e+12));
      }
    else                                     // remaining
      {
        pre = "P";                           // 'peta'
        pmv = boost::str(boost::format("%' 'd") % (value / 1.0e+15));
      }

    unt = pre + baseUnit;

    // for some units of mass, append a useful equivalent in parentheses

    if ( unt == "ukg" )   unt += " (mg)";    // micro-kg, the 'u' version
    if ( unt == "\256kg") unt += " (mg)";    // micro-kg, the 'mu' version
    if ( unt == "mkg" )   unt += " (g)";     // milli-kg
    if ( unt == "kkg" )   unt += " (t)";     // kilo-kg
    if ( unt == "Mkg" )   unt += " (kt)";    // mega-kg
    if ( unt == "Gkg" )   unt += " (Mt)";    // giga-kg
    if ( unt == "Tkg" )   unt += " (Gt)";    // tera-kg

    if ( unt == "ut" )    unt += " (g)";     // micro-tonne, the 'u' version
    if ( unt == "\256t")  unt += " (mg)";    // micro-tonne, the 'mu' version
    if ( unt == "mt" )    unt += " (kg)";    // milli-tonne

    // create final string

    buf = pmv + " " + unt;
    return buf;
  }

  // ---------------------------------------------------------
  //  FREE FUNCTION   : xeona::fmtPriceRate
  // ---------------------------------------------------------
  //  Purpose      : return a price rate in readable 'kilo' format
  //
  //  Design notes
  //
  //      The functions fmtPriceRate(..) and fmtQuantity(..)
  //      provide more readable output when streaming UnitBid
  //      objects.  If they prove more generally useful, they
  //      could be extended and moved to xeona:: scope.
  //
  //      Regarding the monetary currency unit, "$" is now
  //      hard-coded from commit r3951;
  //
  //  Examples ($ hard-coded)
  //
  //          0.2e-03   W  ->  0.20 $/kW
  //          0.201e-03 W  ->  0.201 $/kW
  //          550       kg ->  550000.00 $/kkg ($/t)

  //
  //  Limitations
  //
  //      The 'kilo' prefix is the only one currently supported.
  //
  // ---------------------------------------------------------

  std::string
  fmtPriceRate
  (const int         priceRate,              // wrapper for double version
   const std::string baseUnit)               // for instance, 'W' for Watts
  {
    return fmtPriceRate(boost::numeric_cast<double>(priceRate), baseUnit);
  }

  std::string
  fmtPriceRate
  (const double      priceRate,              // numerical value
   const std::string baseUnit)               // for instance, 'W' for Watts
  {
    std::string buf;

    std::string unit = "$/k" + baseUnit;
    if ( baseUnit == "kg" ) unit += " ($/t)";  // for "kkg (t)"
    std::string two  = boost::str(boost::format("%' '.2f") % (1000 * priceRate));
    std::string all  = boost::str(boost::format("%' 'd")   % (1000 * priceRate));
    if ( two.length() > all.length() )
      {
        buf = two + " " + unit;              // trailing zero fixed (.2f) form
      }
    else
      {
        buf = all + " " + unit;              // straight decimal (d) form
      }
    return buf;
  }

  // ---------------------------------------------------------
  //  FREE FUNCTION   : xeona::vectorRepeat <>
  // ---------------------------------------------------------
  //  Description  : create a vector from a repeating pattern
  //  Role         : used to complete incomplete timeseries
  //  Techniques   : STL 'std::copy'
  //  Status       : complete
  //
  //  Usage
  //
  //      const int horizon = 7;
  //      std::vector<std::string> pattern;
  //      std::vector<std::string> timeseries(horizon);
  //      pattern.push_back("aa");
  //      pattern.push_back("bb");
  //      pattern.push_back("cc");
  //      xeona::vectorRepeat(pattern, timeseries);
  //
  //  Design notes
  //
  //      This function is quite robust.  It works for any value
  //      of 'horizon', be it shorter, equal, or longer than the
  //      length of the 'pattern' vector and also for zero
  //      (although not useful).
  //
  //      See Lischner (2003 p331) for details on 'std::copy'.
  //
  // ---------------------------------------------------------

  template <typename T>                      // meaning a std::vector<T> container
  bool                                       // returns false if pattern is empty
  vectorRepeat
  (const std::vector<T>& pattern,            // pattern vector, must not be empty
   std::vector<T>&       fullset)            // output vector must be correct length
  {
    static logga::spLogger logger = logga::ptrLogStream();
    logger->repx(logga::xtra, "entering member function", "");

    // complain if pattern is empty, because nothing sensible can be done
    if ( pattern.empty() )
      {
        logger->repx(logga::warn, "pattern vector is empty", pattern.size());
        return false;
      }

    // determine output length
    int horizon = fullset.size();

    // calculate the number of loops required
    int patternLength = pattern.size();      // can be longer than the horizon
    double div                               // CAUTION: use floating point arithmetic
      = static_cast<double>(horizon)
      / static_cast<double>(patternLength);
    double ceiling = std::ceil(div);         // round up to next integer as required
    int loops = static_cast<int>(ceiling);   // 'ceiling' is "integer-valued" anyway

    // prepare the output vector
    fullset.clear();                         // empty the container
    fullset.resize(patternLength * loops);   // ", x" can be useful for testing

    // loop zero or more times as the case may be
    typename std::vector<T>::iterator pos;   // CAUTION: 'typename' essential
    pos = fullset.begin();                   // set fullset iterator to beginning
    for ( int count = 0; count < loops; ++count )
      {
        pos = std::copy                      // CAUTION: must update iterator
          (pattern.begin(),
           pattern.end(),
           pos);
      }

    // truncate if necessary
    fullset.resize(horizon);                 // when horizon < patternLength * loops

    return true;
  }

  // ---------------------------------------------------------
  //  FREE FUNCTION   : xeona::getSystemTimerResol
  // ---------------------------------------------------------

  long                                       // guaranteed at least 2147483647
  getSystemTimerResol()
  {
    const boost::posix_time::time_duration::tick_type tps   // integral type
      = boost::posix_time::time_duration::ticks_per_second();
    return static_cast<long>(tps);
  }

  // ---------------------------------------------------------
  //  FREE FUNCTION   : xeona::formatDuration
  // ---------------------------------------------------------
  //
  //  CAUTION: fractional seconds (displaying subseconds)
  //
  //      Fractional seconds depend on the resolution of the
  //      system timer and special considerations are needed to
  //      write portable code.  This code attempts to do that.
  //      See the reference below for background.
  //
  //  References
  //
  //      The Boost C++ Libraries (BoostBook Subset)
  //        > Boost.Date_Time
  //          > Posix Time
  //            > Time Duration
  //              >Introduction
  //                > discussion on 'ticks_per_second()'
  //
  // ---------------------------------------------------------

  std::string
  formatDuration
  (const boost::posix_time::time_duration& delta)
  {
    boost::posix_time::time_duration onesec(0, 0, 1);
    if ( delta < onesec )
      {
#if 1 // 1 = report milliseconds, 0 = simply output a comment
        const long   totalMillisecs = delta.total_milliseconds();     // [1]
        const double totalSeconds   = static_cast<double>(totalMillisecs) / 1000.0;
        std::ostringstream buf;
        buf << boost::format("%.3f") % totalSeconds;
        return buf.str();
        // [1] call uses truncation and not rounding
#else
        return "(under one second)";
#endif // 0
      }
    else
      {
        std::ostringstream buf;
        buf << boost::format("%02f")  % delta.hours()  // set to two or three digits
            << boost::format(":%02f") % delta.minutes()
            << boost::format(":%02f") % delta.seconds();
        return buf.str();
      }
  }

  // ---------------------------------------------------------
  //  FREE FUNCTION   : xeona::tailCombine <>
  // ---------------------------------------------------------
  //  Description  : append sequence container 'tail' to 'combined' and thus modify
  //  Role         : general, specifically used to combine record subsets
  //  Techniques   : explicit template instantiations in source file
  //  Headers      : requires <algorithm>, <iterator>
  //  Status       : complete
  //
  //  Design notes
  //
  //      The term "combine" was chosen to describe the processes
  //      of making one vector from two.  Regarding other
  //      definitions, the term "merge" is restricted to sorted
  //      sequences and is thus not appropriate.  The term
  //      "concatenate" could be used, but is often applied just to
  //      strings and that practice is maintained here.  The term
  //      "addition" has mathematical meanings.
  //
  //      For the application in mind, the two sequence containers
  //      have no common elements singly or together, that is, no
  //      two elements would equate.  This however is not a
  //      requirement for the following code to work.
  //
  //      In most implementations, the 'merge' function template
  //      from <algorithm> could work.  Usage by unsorted sequences
  //      is not formally supported by the C++ standard and
  //      Josuttis (1999 p417) notes that "for unsorted ranges you
  //      should call copy() twice, instead of merge(), to be
  //      portable".
  //
  //      The 'copy' function template from <algorithm> is used
  //      with the 'back_inserter' function template from
  //      <iterator>.  This application is demonstrated in Josuttis
  //      (1999 p364) with the comment "use back_inserter to insert
  //      instead of overwrite".  For more information on the
  //      'back_inserter' see Lischner (2003 p538) and Stephens
  //      etal (2006 pp266-267).
  //
  //      If this function template is used exclusively by vectors,
  //      the following line should speed up the code, but is not
  //      essential (change the function name too):
  //
  //          combined.reserve(combined.size() + tail.size());
  //
  //  CAUTION: supported sequence containers
  //
  //      tested using     : vector, list, deque
  //      not tested using : valarray, string (both pseudo containers anyway)
  //
  // ---------------------------------------------------------

  template <typename S>                      // 'S' is a specialized sequence container
  void
  tailCombine
  (S&       combined,                        // resultant container, need not be empty
   const S& tail)                            // container to be "back inserted"
  {
    static logga::spLogger logger = logga::ptrLogStream();
    logger->repx(logga::xtra, "entering free function, tail size", tail.size());

    std::copy(tail.begin(),                  // <algorithm>
              tail.end(),
              std::back_inserter(combined)); // <iterator>
  }

  // ---------------------------------------------------------
  //  FREE FUNCTION   : xeona::mapCombine <>
  // ---------------------------------------------------------
  //  Description  : merge map 'addition' with map 'combined' and thus modify the latter
  //  Role         : general, specifically written for registering gateways
  //  Techniques   : explicit template instantiations in source file
  //  Caution      : requires UNIQUE keys, warnings issued for duplicates
  //  Headers      : requires <map> and probably <utility>
  //  Status       : complete
  //
  //  Design notes
  //
  //      The implementation use here is not particularly
  //      STL-like.  However attempts to use 'std::copy' and
  //      'std::inserter' failed, as did attempts to use
  //      'std::merge'.
  //
  //      See Lischner (2003 p604) for code to modify a key in an
  //      existing map.
  //
  //  Reference
  //
  //      Lischner, Ray.  2003.  C++ in a nutshell : a language and
  //        library reference.  O'Reilly and Associates, Sebastopol,
  //        California, USA.  ISBN 0-596-00298-X.
  //
  // ---------------------------------------------------------

  template <typename S>
  bool
  mapCombine
  (S&       combined,
   const S& addition)
  {
    // logging support
    static logga::spLogger logger = logga::ptrLogStream();

    // active code
    bool status = true;
    const int orig = combined.size();        // just for reporting
    typename S::const_iterator pos;          // CAUTION: 'typename' and 'const_' essential
    for ( pos  = addition.begin();
          pos != addition.end();
          ++pos)
      {
        if ( ! combined.insert(std::make_pair(pos->first, pos->second)).second )
          {
            logger->repx(logga::warn, "map insert failed, key", pos->first);
            status = false;
          }
#if 1 // 1 = extended reporting, 0 = normal reporting
        else                                 // debug reporting follows
          {
            logger->repx(logga::adhc, "map insert succeeded, key", pos->first);
          }
#endif // 0
      }
    const int delta = combined.size() - orig;
    logger->repx(logga::adhc, "number of added elements", delta);

    // return the status
    return status;
  }

  // ---------------------------------------------------------
  //  FREE FUNCTION   : xeona::trimLeadingDigits
  // ---------------------------------------------------------
  //  Description  : trims leading digits from given string
  //  Role         : various, including use by 'xeona::demangle'
  //  Techniques   : C-style 'std::isdigit' from <cctype>
  //  Status       : complete
  //
  //  CAUTION: do not convert 'str' to pass-by-ref
  //
  //      This function is sometimes passed a char-star and
  //      pass-by-ref does not work.
  //
  // ---------------------------------------------------------

  std::string
  trimLeadingDigits
  (std::string str)
  {
    if ( ! str.empty() )                     // protection needed
      while ( std::isdigit(str.at(0)) )      // see <cctype>
        str.erase(0, 1);                     // remove first char
    return str;
  }

  // ---------------------------------------------------------
  //  FREE FUNCTION   : xeona::demangle
  // ---------------------------------------------------------
  //  Description  : attempts to reinterpret straightforward g++ typenames
  //  Role         : repairing typeid names from GCC RTTI
  //  Techniques   : simple reverse engineering
  //  Status       : complete
  //
  //  CAUTION: contains g++ compiler specific code
  //
  //      The C preprocessor will issue a warning if this file is
  //      not compiled using GNU g++.
  //
  //  Design notes
  //
  //      This function attempts to "demangle" a compiler
  //      decorated name.
  //
  //      Ideally, this function should be a wrapper to a
  //      compiler or demangling library API.  However such APIs
  //      do not appear to exist for GCC (although they do for
  //      other compilers, Hewlett Packard HPUX for example).
  //
  //      Note also that the 'c++filt' command-line utility does
  //      exactly what is required.
  //
  //      For user-defined classes, g++ typeid(C).name() reports
  //      in the form give below.  This code simply strips off
  //      the leading character count in such cases.
  //
  //          1A
  //          3Now
  //          15MyLongClassName
  //
  //      Note that a leading "P" indicates a raw pointer or (in
  //      my experience) a 'shared_ptr'.  For user-defined types,
  //      this case is dealt with in the code -- however, for
  //      user-defined derived types, only the base class is
  //      revealed by 'typeid'.  The following approach is thus
  //      more informative (assuming member function code):
  //
  //          std::string subtypeName;
  //          subtypeName = xeona::demangle(typeid(*this).name())
  //
  //      Run-time type info is expensive so it may be worth
  //      placing the calling code in a debug only block.
  //
  //      See also my test file: 'frag-rtti-2.cc'.
  //
  //      Fog (2008) describes name mangling in general and GNU
  //      3.* and later name mangling more specifically.
  //
  //  References
  //
  //      Fog, Agner.  2008.  Calling conventions for different
  //        C++ compilers and operating systems.  Copenhagen
  //        University College of Engineering.  Last updated
  //        2008-06-29.  [calling_conventions.pdf].  See in
  //        particular, section 8.5 : Gnu3 name mangling.
  //
  // ---------------------------------------------------------

  std::string                                // this return may be streamed
  demangle                                   // GCC-specific for fundamental typenames
  (std::string tid)                          // most often 'tid' is a 'const char*'
  {
    // skip if empty string
    if ( tid.empty() ) return "";

    // try some common fundamental types
    if      ( tid == typeid(void        ).name() ) return "void";     // GCC "v"
    else if ( tid == typeid(std::string ).name() ) return "string";   // GCC "Ss"
    else if ( tid == typeid(int         ).name() ) return "int";      // GCC "i"
    else if ( tid == typeid(unsigned    ).name() ) return "unsigned"; // GCC "j"
    else if ( tid == typeid(double      ).name() ) return "double";   // GCC "d"
    else if ( tid == typeid(bool        ).name() ) return "bool";     // GCC "b"
    else if ( tid == typeid(char        ).name() ) return "char";     // GCC "c"
    else if ( tid == typeid(char*       ).name() ) return "char*";    // GCC "Pc"
    else if ( tid == typeid(std::vector<std::string>).name() ) return "vector<string>";
    else if ( tid == typeid(std::vector<int>        ).name() ) return "vector<int>";
    else if ( tid == typeid(std::vector<unsigned>   ).name() ) return "vector<unsigned>";
    else if ( tid == typeid(std::vector<double>     ).name() ) return "vector<double>";
    else if ( tid == typeid(std::vector<bool>       ).name() ) return "vector<bool>";
    else if ( tid == typeid(std::vector<char>       ).name() ) return "vector<char>";

    // then assume, perhaps falsely, a user-defined type

#ifdef __GNUG__                              // CAUTION: g++ compiler specific code block

    else
      {
        if ( tid.substr(0, 1) == "P" )       // probably a raw or shared pointer
          {
            // need to dereference pointer before making 'typeid' call
            static logga::spLogger logger = logga::ptrLogStream();
            logger->repx(logga::warn, "dereference prior to 'typeid' call", tid);
            // active code
            const std::string comment = "base class pointer";
            tid.erase(0, 1);                 // strip the leading "P"
            return comment + " " + xeona::trimLeadingDigits(tid);
          }
        else if ( tid.substr(tid.length() - 1, 1) == "E" )  // avoid unnecessary effort
          {
            // this code is based on the observation that:
            //
            //     "5Class<Type>" mangles to "5ClassI4TypeE"
            //
            // this code is also specific to 'xeona' although a
            // more general regex pattern match could easily be
            // written
            //
            // some calls in this block use the 'Boost.Foreach'
            // and 'Boost.String_algo' header-only libraries --
            // which need to be present (or instead factor out
            // the offending calls using the STL library)

            std::vector<std::pair<std::string, std::string> > names;
            // base commodities
            names.push_back(std::make_pair("CmOxidize"        , "Oxid"));
            names.push_back(std::make_pair("CmCarbonCert"     , "Cert"));
            names.push_back(std::make_pair("CmCarbonSeq"      , "Cseq"));
            names.push_back(std::make_pair("CmElectricity"    , "Elec"));
            names.push_back(std::make_pair("CmWork"           , "Work"));
            names.push_back(std::make_pair("CmHeat"           , "Heat"));
            names.push_back(std::make_pair("CmThermalFluid"   , "Thrm"));
            names.push_back(std::make_pair("CmFunds"          , "Fund"));
            // base commodities only partially implemented
            names.push_back(std::make_pair("CmProductiveLand" , "Land"));
            // derived commodities
            names.push_back(std::make_pair("CmOxidBiocoal"    , "Biocoal"));
            names.push_back(std::make_pair("CmOxidGas"        , "OxidGas"));
            names.push_back(std::make_pair("CmOxidNaturalGas" , "NaturalGas"));
            // loop thru 'names'
            std::pair<std::string, std::string> pair;  // [1]
            // [1] CAUTION: BOOST_FOREACH expects just one comma,
            // hence the use of a predeclared loop variable 'pair'
            BOOST_FOREACH( pair, names )
              {
                const std::string tag = boost::str(boost::format("I%d%sE")
                                                   % pair.first.length()
                                                   % pair.first );
                const std::string rep = ":" + pair.second;   // note colon
                if ( boost::ends_with(tid, tag) )
                  {
                    tid = xeona::trimLeadingDigits(tid);
                    boost::replace_last(tid, tag, rep);
                    break;
                  }
              }
            return tid;
          }
        else
          {
            return xeona::trimLeadingDigits(tid);
          }
      }

#else
# warning "g++ specific code omitted: function 'xeona::demangle' now ineffective"
    // this is only an issue of aesthetics, not substance --
    // whilst noting also that '#warning' is a GCC extension!

    else return tid;

#endif // def __GNUG__

  }

  // ---------------------------------------------------------
  //  EXPLICIT TEMPLATE INSTANTIATIONS
  // ---------------------------------------------------------

  //  See Cline (2006, section 35) for a simple explanation.  The
  //  topic is also covered more comprehensively by Lischner
  //  (2003 pp195-199).
  //
  //  Failure to instantiate template will typically result in a
  //  link-time error whenever a call is made.  The error usually
  //  says: "undefined reference to ...".  If no such call is
  //  ever made, then the program should compile and run without
  //  a murmur.
  //
  //    Cline, Marshall.  2006.  C++ FAQ lite.
  //      [www.parashift.com/c++-faq-lite/templates.html].
  //
  //    Lischner, Ray.  2003.  C++ in a nutshell : a language and
  //      library reference.  O'Reilly and Associates, Sebastopol,
  //      California, USA.  ISBN 0-596-00298-X.

  // FUNCTION 'xeona::vectorRepeat'

  template bool vectorRepeat<std::string>
  (const std::vector<std::string>&, std::vector<std::string>&);

  // FUNCTION 'xeona::tailCombine'

  // the following is just for unit test purposes
  typedef std::vector<int> vec0_type;
  template void tailCombine<vec0_type>(vec0_type&, const vec0_type&);

  typedef std::vector<shared_ptr<Record> > vec1_type;
  template void tailCombine<vec1_type>(vec1_type&, const vec1_type&);

  // FUNCTION 'xeona::mapCombine'

  // the following is just for unit test purposes
  typedef std::map<shared_ptr<Class>,
                   shared_ptr<Class> > map0_type;
  template bool mapCombine<map0_type>(map0_type&, const map0_type&);

  typedef std::map<shared_ptr<Entity>,
                   shared_ptr<Entity> > map1_type;
  template bool mapCombine<map1_type>(map1_type&, const map1_type&);

  typedef std::map<shared_ptr<Gateway>,
                   shared_ptr<DomainController> > map2_type;
  template bool mapCombine<map2_type>(map2_type&, const map2_type&);

  typedef std::map<shared_ptr<Gateway>,
                   shared_ptr<Block> > map3_type;
  template bool mapCombine<map3_type>(map3_type&, const map3_type&);

  typedef std::map<shared_ptr<Block>,
                   shared_ptr<DomainController> > map4_type;
  template bool mapCombine<map4_type>(map4_type&, const map4_type&);

  // CAUTION: must place new code above the template instantiations

} // namespace xeona

//  end of file

