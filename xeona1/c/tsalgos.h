//  XEONA ENERGY SYSTEMS MODELING ENVIRONMENT
//
//  file-create-name : tsalgos.h
//  file-create-date : Tue 24-May-2011 22:44 UTC
//  file-initiator   : Robbie Morrison <robbie@actrix.co.nz>
//
//  file-role        : custom STL algorithms for timeseries / header
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
//  $URL: file:///home/robbie/svn-root/xeona/futz/trunk/xeona1/c/tsalgos.h $
//
//  GENERAL NOTES FOR THIS FILE
//
//  Purpose
//
//    This unit holds custom algorithm functors for 'std::vector'
//    timeseries.  Note also the timeseries classes 'TsNormal'
//    'TsMonthly' and 'TsGnuplot' to embed meta-information and
//    offer further statistical and visualization functionality.
//
//  Design
//
//    In terms of design, it seems that an STL algorithm plus
//    custom function would not work -- because the source and
//    target are traversed at different speeds (maybe I missed
//    something).
//
//    Only those STL containers which offer the 'push_back'
//    member function can be used with these custom algorithm
//    functors.
//
//  INTERVAL LENGTHS
//
//        ipd     secs    hrs   mins
//      --------------------------------
//        288      300             5
//        144      600            10
//         96      900            15
//         72     1200            20
//         48     1800            30
//         24     3600      1
//         12     7200      2
//          8    10800      3
//          6    14400      4
//          4    21600      6
//          2    43200     12
//          1    86400     24

//  HEADER GUARD

#ifndef _TSALGOS_H_
#define _TSALGOS_H_

//  LOCAL AND SYSTEM INCLUDES

#include "../a/logger.h"      // run-time logging functionality (as required)
#include "../c/smart_ptr.h"   // toggle between Boost and TR1 smart pointers

#include <deque>              // STL sequence container, double-ended vector
#include <iostream>           // standard io
#include <sstream>            // string-streams
#include <stdexcept>          // standard exception classes, runtime_error()
#include <string>             // C++ strings
#include <vector>             // STL sequence container

#include <iterator>           // STL additional iterators, std::distance()
#include <numeric>            // STL numerical algorithms

#include <cmath>              // C-style maths, abs(), ceil(), floor(), sqrt()

#include <boost/assign/std/vector.hpp>  // assign operator+=() for std::vector

//  PREPROCESSOR MACRO

// used to switch between standard and compiler-specific macros
// (noting that these are not actually macros these days)

// compiler-specific code to obtain the name of this function
#ifdef __GNUG__                              // a GNU g++ compiler
# define XEONA_FUNC __PRETTY_FUNCTION__
#else
# define XEONA_FUNC  __func__
#endif

//  NAMESPACE DIRECTIVES

using namespace boost::assign;               // bring 'operator+=()' into scope

//  CODE

// ---------------------------------------------------------
//  CLASS (FUNCTOR) : xeona::ResampleTsCompress
// ---------------------------------------------------------
//  Description  : contract resample a timeseries by average value
//  Role         : custom STL algorithm
//  Techniques   : functor
//  Status       : complete
//
//  Design notes
//
//      This version stores the span history in a 'std::vector'.
//      And although the statistical mean can be calculated
//      on-the-fly, this approach, by keeping all the
//      information, offers greater development flexibility.
//
//      The functor will throw a 'std::domain_error' if a 'span'
//      of zero is submitted.  This otherwise avoids a subsequent
//      floating point exception.
//
//      Information loss takes place if the source length is not
//      a multiple of 'span'.  Perhaps this should be checked,
//      but it cannot be done on construction.
//
//      See Josuttis (1999 pp271-275) for a discussion on
//      inserters.  And Lischner (2003 p275) on custom algorithms
//      and a 'copy_if' implementation.
//
//  Example
//
//      #include <iterator>
//      #include <numeric>
//
//      std::vector<double> v1;
//      // fill 'v1'
//      std::vector<double> v2;
//      v2.reserve(v1.size());                  // optional but useful
//      xeona::ResampleTsCompress resample(3);  // functor
//      resample(v1.begin(), v1.end(), std::back_inserter(v2));
//
//  CAUTION: 'push_back' required
//
//      Only those STL containers which offer the 'push_back'
//      member function can be used with 'ResampleTsCompress'.
//      This includes 'std::vector'.
//
//  References
//
//      Josuttis, Nicolai M.  1999.  The C++ Standard Library : a
//        tutorial and reference.  Addison-Wesley, Boston, USA.
//        ISBN 0-201-37926-0.
//
//      Lischner, Ray.  2003.  C++ in a nutshell : a language and
//        library reference.  O'Reilly and Associates, Sebastopol,
//        California, USA.  ISBN 0-596-00298-X.
//
// ---------------------------------------------------------

namespace xeona
{
  class ResampleTsCompress
  {
  private:

    ResampleTsCompress();                    // zero-argument constructor

  public:

    ResampleTsCompress
    (const unsigned& n)                      // number of elements to combine
      throw (std::domain_error) :
      span(n),
      store()
    {
      if ( n == 0 )
        {
          std::ostringstream oss;
          oss << __func__ << " functor" << ": span value cannot be zero" << ": " << n;
          throw std::domain_error(oss.str());
        }
    }

    template <typename InIter, typename BackInser>
    BackInser
    operator()
    (InIter    first,
     InIter    last,
     BackInser result)
    {
      unsigned loop = 0;
      for ( ; first != last; ++first )
        {
          store.push_back(*first);           // 'store' is a private vector
          if ( ++loop % span == 0 )          // modulo (remainder) operator
            {
              result = mean(store);          // 'mean' is a private function
              store.clear();
            }
        }
      return result;
    }

  private:

    double mean(const std::vector<double>& vec)
    {
      const double total = std::accumulate(vec.begin(), vec.end(), 0.0); // see <numeric>
      const double mean  = total / vec.size();
      return mean;
    }

  private:

    const unsigned         span;             // grouping
    std::vector<double>    store;            // reusable store

  }; // class 'ResampleTsCompress'
}

// ---------------------------------------------------------
//  CLASS (FUNCTOR) : xeona::ResampleTsLinear
// ---------------------------------------------------------
//  Description  : resample and expand a timeseries by linear interpolation
//  Role         : custom STL algorithm
//  Techniques   : functor
//  Status       : complete
//
//  Design notes
//
//      This version offers three lead in and lead out treatments
//      -- the choice of which is controlled by the preprocessor
//      macro 'OPTION'.
//
//      The functor will throw a 'std::domain_error' if a 'grow'
//      of zero is submitted.  This otherwise avoids a subsequent
//      floating point exception.
//
//      Information loss in terms of timeseries truncation can
//      never take place.
//
//      See Josuttis (1999 pp271-275) for a discussion on
//      inserters.  And Lischner (2003 p275) on custom algorithms
//      and a 'copy_if' implementation.
//
//  Example
//
//      #include <iterator>
//
//      std::vector<double> v1;
//      // fill 'v1'
//      std::vector<double> v2;
//      const int grow = 4;                      // expansion factor
//      v2.reserve(grow * v1.size());            // optional but useful
//      xeona::ResampleTsLinear resample(grow);  // functor
//      resample(v1.begin(), v1.end(), std::back_inserter(v2));
//
//  CAUTION: 'push_back' required
//
//      Only those STL containers which offer the 'push_back'
//      member function can be used with 'ResampleTsLinear'.
//      This includes 'std::vector'.
//
//  References
//
//      Josuttis, Nicolai M.  1999.  The C++ Standard Library : a
//        tutorial and reference.  Addison-Wesley, Boston, USA.
//        ISBN 0-201-37926-0.
//
//      Lischner, Ray.  2003.  C++ in a nutshell : a language and
//        library reference.  O'Reilly and Associates, Sebastopol,
//        California, USA.  ISBN 0-596-00298-X.
//
// ---------------------------------------------------------

namespace xeona
{
  class ResampleTsLinear
  {
  private:

    ResampleTsLinear();                      // zero-argument constructor

  public:

    ResampleTsLinear
    (const unsigned& m)                      // number of expansions per element
      throw (std::domain_error) :
      grow(m),
      even(( m % 2 == 0 ))                   // 'true' if even
    {
      if ( m == 0 )
        {
          std::ostringstream oss;
          oss << __func__ << " functor" << ": expansion value cannot be zero"
              << ": " << m;
          throw std::domain_error(oss.str());
        }
    }

    template <typename InIter, typename BackInser>
    BackInser
    operator()
    (InIter    first,
     InIter    last,
     BackInser result)
    {
      // preamble

      double hi;                             // right-side interval
      double lo;                             // left-side interval

      // corner cases

      if ( first == last )                   // empty vector
        {
          return result;
        }

      if ( first == (last - 1) )             // single element vector
        {
          lo = *first;
          for ( unsigned j = 0; j < grow; ++j ) result++ = lo;
          return result;
        }

      // lead-in and lead-out treatments

      // 'OPTION' values
      //
      // 1 = lead-extrapolated without adjustment
      // 2 = lead-extrapolated with mean-preserving adjustment
      // 3 = flat, also mean-preserving

#define OPTION 3

      int ono;
      std::string omsg;

#if   OPTION == 1 // stable

      ono  = 1;
      omsg += "linear interpolation lead-ins and lead-outs are simply extrapolated";
      omsg += " without adjustment, not mean-preserving";

      double inHi      = *first;
      double inNext    = *(first + 1);
      double outLo     = *(last  - 1);
      double outPrior  = *(last  - 2);

      double inDelta   = inNext - inHi;
      double outDelta  = outLo  - outPrior;
      double inLo      = inHi   - inDelta;
      double outHi     = outLo  + outDelta;

#elif OPTION == 2 // beta

      ono  = 2;
      omsg += "linear interpolation lead-ins and lead-outs are extrapolated";
      omsg += " but with a mean-preserving adjustment";

      double inHi      = *first;
      double inNext    = *(first + 1);
      double outLo     = *(last  - 1);
      double outPrior  = *(last  - 2);

      double inDelta   = inNext - inHi;
      double outDelta  = outLo  - outPrior;
      double halfDelta = (outDelta - inDelta) / 2.0;
      double inLo      = inHi   - (inDelta  + halfDelta);
      double outHi     = outLo  + (outDelta - halfDelta);

#elif OPTION == 3 // stable

      ono  = 3;
      omsg += "linear interpolation lead-ins and lead-outs are flat";
      omsg += " and also mean-preserving";

      double inHi      = *first;
      double outLo     = *(last - 1);

      double inLo      = inHi;
      double outHi     = outLo;

#endif // OPTION

      // reporting
      std::ostringstream put;
      put << "    " << "option " << ono << ": " << omsg << "\n";
      logga::spLogger logger = logga::ptrLogStream();
      logger->repx(logga::dbug, "ResampleTsLinear macro OPTION", OPTION);
      logger->putx(logga::adhc, put);
      logger->addSmartBlank(logga::dbug);

      // main code

      // lead-in
      hi = inHi;
      lo = inLo;
      for ( unsigned j = (grow + 1) / 2; j < grow; ++j ) result++ = dot(lo, hi, j);
      // main loop
      for ( ; first != (last - 1); ++first )
        {
          lo = *(first + 0);
          hi = *(first + 1);
          for ( unsigned j = 0; j < grow; ++j ) result++ = dot(lo, hi, j);
        }
      // lead-out
      lo = outLo;
      hi = outHi;
      for ( unsigned j = 0; j < (grow + 1) / 2; ++j ) result++ = dot(lo, hi, j);

      // housekeeping
      return result;
    }

    // UTILITY FUNCTIONS

  private:

    double
    dot                                      // the sub-interval value
    (const double lo,
     const double hi,
     const unsigned j)
    {
      const double dj  = static_cast<double>(j);
      const double dm  = static_cast<double>(grow);
      double fac;
      if ( even ) fac  = (1.0 + 2.0 * dj) / (2.0 * dm);
      else        fac  = dj / dm;
      return lo + (fac * (hi - lo));
    }

    // INSTANCE DATA

  private:

    const unsigned    grow;                  // expansion factor
    const bool        even;

  }; // class 'ResampleTsLinear'
}

// ---------------------------------------------------------
//  CLASS (FUNCTOR) : xeona::ResampleTsDuplicate
// ---------------------------------------------------------
//  Description  : contract resample a timeseries by average value
//  Role         : custom STL algorithm
//  Techniques   : functor
//  Status       : complete
//
//  Design notes
//
//      The functor will throw a 'std::domain_error' if a 'grow'
//      of zero is submitted.  This otherwise avoids a subsequent
//      floating point exception.
//
//      Information loss in terms of timeseries truncation can
//      never take place.
//
//      See Josuttis (1999 pp271-275) for a discussion on
//      inserters.  And Lischner (2003 p275) on custom algorithms
//      and a 'copy_if' implementation.
//
//  Example
//
//      #include <iterator>
//
//      std::vector<double> v1;
//      // fill 'v1'
//      std::vector<double> v2;
//      const int grow = 4;                         // expansion factor
//      v2.reserve(grow * v1.size());               // optional but useful
//      xeona::ResampleTsDuplicate resample(grow);  // functor
//      resample(v1.begin(), v1.end(), std::back_inserter(v2));
//
//  CAUTION: 'push_back' required
//
//      Only those STL containers which offer the 'push_back'
//      member function can be used with 'ResampleTsDuplicate'.
//      This includes 'std::vector'.
//
//  References
//
//      Josuttis, Nicolai M.  1999.  The C++ Standard Library : a
//        tutorial and reference.  Addison-Wesley, Boston, USA.
//        ISBN 0-201-37926-0.
//
//      Lischner, Ray.  2003.  C++ in a nutshell : a language and
//        library reference.  O'Reilly and Associates, Sebastopol,
//        California, USA.  ISBN 0-596-00298-X.
//
// ---------------------------------------------------------

namespace xeona
{
  class ResampleTsDuplicate
  {
  private:

    ResampleTsDuplicate();                   // zero-argument constructor

  public:

    ResampleTsDuplicate
    (const unsigned& m)                      // number of expansions per element
      throw (std::domain_error) :
      grow(m)                                // stored as 'unsigned'
    {
      if ( m == 0 )
        {
          std::ostringstream oss;
          oss << __func__ << " functor" << ": expansion value cannot be zero"
              << ": " << m;
          throw std::domain_error(oss.str());
        }
    }

    template <typename InIter, typename BackInser>
    BackInser
    operator()
    (InIter    first,
     InIter    last,
     BackInser result)
    {
      // preamble

      double me;                             // current interval

      // corner cases

      if ( first == last )                   // empty vector
        {
          return result;
        }

      // main code

      for ( ; first != last; ++first )
        {
          me = *first;
          for ( unsigned j = 0; j < grow; ++j ) result++ = me;
        }

      // housekeeping
      return result;
    }

     // INSTANCE DATA

  private:

    const unsigned    grow;                  // expansion factor

  }; // class 'ResampleTsDuplicate'
}

// ---------------------------------------------------------
//  CLASS (FUNCTOR) : xeona::ModifyTsDeLeapYear
// ---------------------------------------------------------
//  Description  : remove values associated with 29-Feb
//  Role         : custom STL algorithm
//  Techniques   : functor
//  Status       : complete
//
//  Design notes
//
//      This functor copies any elements associated with 28-Feb
//      to 29-Feb.  To do so, the constructor needs to be passed
//      the interval length in [s].  The caller is responsible
//      for knowing that the vector indeed contains leap year
//      data.
//
//      The functor will throw a 'std::domain_error' if an 'i' of
//      zero is submitted.  This otherwise avoids subsequent
//      problems.  The functor will throw a 'std::domain_error'
//      if the 'i' is not a divisor of 24 * 3600.
//
//      See Josuttis (1999 pp271-275) for a discussion on
//      inserters.  And Lischner (2003 p275) on custom algorithms
//      and a 'copy_if' implementation.
//
//  Example
//
//      #include <iterator>
//
//      std::vector<double> v1;
//      // fill 'v1'
//      std::vector<double> v2;
//      const int interval = 7200;                   // interval in seconds
//      v2.reserve(v1.size());                       // optional but useful
//      xeona::ModifyTsDeLeapYear modify(interval);  // functor
//      modify(v1.begin(), v1.end(), std::back_inserter(v2));
//
//  CAUTION: 'push_back' required
//
//      Only those STL containers which offer the 'push_back'
//      member function can be used with 'ModifyTsDeLeapYear'.
//      This includes 'std::vector'.
//
//  References
//
//      Josuttis, Nicolai M.  1999.  The C++ Standard Library : a
//        tutorial and reference.  Addison-Wesley, Boston, USA.
//        ISBN 0-201-37926-0.
//
//      Lischner, Ray.  2003.  C++ in a nutshell : a language and
//        library reference.  O'Reilly and Associates, Sebastopol,
//        California, USA.  ISBN 0-596-00298-X.
//
// ---------------------------------------------------------

namespace xeona
{
  class ModifyTsDeLeapYear
  {
  private:

    ModifyTsDeLeapYear();                              // zero-argument constructor

  public:

    ModifyTsDeLeapYear
    (const unsigned& i)                                // interval length in seconds
      throw (std::domain_error) :
      ipd((24.0 * 3600.0) / static_cast<double>(i))    // intervals per day
    {
      // integrity checks on construction
      if ( i == 0 )
        {
          std::ostringstream oss;
          oss << __func__ << " functor" << ": interval value cannot be zero" << ": " << i;
          throw std::domain_error(oss.str());
        }
      if ( 24 * 3600 % i != 0 )                        // 'i' should not be zero
        {
          std::ostringstream oss;
          oss << __func__ << " functor" << ": interval value not a divisor of 24 * 3600"
              << ": " << i;
          throw std::domain_error(oss.str());
        }
    }

    template <typename InIter, typename BackInser>
    BackInser
    operator()
    (InIter    first,
     InIter    last,
     BackInser result)
    {
      // day 29-Feb has a zero-based index of 59
      unsigned loop = 0;
      for ( ; first != last; ++first )
        {
          if ( loop >= 59 * ipd && loop < 60 * ipd )
            {
              continue;
            }
          result++ = *first;
          ++loop;
        }
      return result;
    }

  private:

    const unsigned    ipd;                   // intervals per day

  }; // class 'ModifyTsDeLeapYear'
}

// ---------------------------------------------------------
//  CLASS (FUNCTOR) : xeona::ModifyTsReLeapYear
// ---------------------------------------------------------
//  Description  : copy values associated with 28-Feb to 29-Feb
//  Role         : custom STL algorithm
//  Techniques   : functor, 'std::deque'
//  Status       : complete
//
//  Design notes
//
//      This functor copies any elements associated with 28-Feb
//      to 29-Feb.  To do so, the constructor needs to be passed
//      the interval length in [s].  The caller is responsible
//      for knowing that the vector indeed needs leap year data.
//
//      The functor will throw a 'std::domain_error' if an 'i' of
//      zero is submitted.  This otherwise avoids subsequent
//      problems.  The functor will throw a 'std::domain_error'
//      if the 'i' is not a divisor of 24 * 3600.
//
//      See Josuttis (1999 pp271-275) for a discussion on
//      inserters.  And Lischner (2003 p275) on custom algorithms
//      and a 'copy_if' implementation.
//
//  Example
//
//      #include <iterator>
//
//      std::vector<double> v1;
//      // fill 'v1'
//      std::vector<double> v2;
//      const int interval = 7200;                   // interval in seconds
//      v2.reserve(2 * v1.size());                   // optional but useful
//      xeona::ModifyTsReLeapYear modify(interval);  // functor
//      modify(v1.begin(), v1.end(), std::back_inserter(v2));
//
//  CAUTION: 'push_back' required
//
//      Only those STL containers which offer the 'push_back'
//      member function can be used with 'ModifyTsReLeapYear'.
//      This includes 'std::vector'.
//
//  References
//
//      Josuttis, Nicolai M.  1999.  The C++ Standard Library : a
//        tutorial and reference.  Addison-Wesley, Boston, USA.
//        ISBN 0-201-37926-0.
//
//      Lischner, Ray.  2003.  C++ in a nutshell : a language and
//        library reference.  O'Reilly and Associates, Sebastopol,
//        California, USA.  ISBN 0-596-00298-X.
//
// ---------------------------------------------------------

namespace xeona
{
  class ModifyTsReLeapYear
  {
  private:

    ModifyTsReLeapYear();                              // zero-argument constructor

  public:

    ModifyTsReLeapYear
    (const unsigned& i)                                // interval length in seconds
      throw (std::domain_error) :
      ipd((24.0 * 3600.0) / static_cast<double>(i)),   // intervals per day
      feb28()                                          // collect 28-Feb values for 29-Feb
    {
      // integrity checks on construction
      if ( i == 0 )
        {
          std::ostringstream oss;
          oss << __func__ << " functor" << ": interval value cannot be zero" << ": " << i;
          throw std::domain_error(oss.str());
        }
      if ( 24 * 3600 % i != 0 )                        // 'i' should not be zero
        {
          std::ostringstream oss;
          oss << __func__ << " functor" << ": interval value not a divisor of 24 * 3600"
              << ": " << i;
          throw std::domain_error(oss.str());
        }
    }

    template <typename InIter, typename BackInser>
    BackInser
    operator()
    (InIter    first,
     InIter    last,
     BackInser result)
    {
      // day 29-Feb has a zero-based index of 59
      unsigned loop = 0;
      for ( ; first != last; ++first )
        {
          if ( loop >= 58 * ipd && loop < 59 * ipd )
            {
              result++ = *first;
              feb28.push_back(*first);       // store the value too
            }
          else if ( loop >= 59 * ipd && loop < 60 * ipd )
            {
              result++ = feb28.front();      // grab first deque element
              feb28.pop_front();             // then remove it
              --first;                       // hold still
            }
          else
            {
              result++ = *first;
            }
          ++loop;
        }
      return result;
    }

  private:

    const unsigned        ipd;               // intervals per day
    std::deque<double>    feb28;             // values for 28-Feb

  }; // class 'ModifyTsReLeapYear'
}

// ---------------------------------------------------------
//  CLASS (FUNCTOR) : xeona::OffsetTs
// ---------------------------------------------------------
//  Description  : offset a timeseries, rolling around if need be
//  Role         : custom STL algorithm
//  Techniques   : functor
//  Status       : complete
//
//  Design notes
//
//      This functor offsets the given timeseries.  It also
//      rolls around the timeseries if it is marked as wrappable.
//
//  Example
//
//      #include <iterator>
//
//      std::vector<double> v1;
//      // fill 'v1' with monthly values
//      std::vector<double> v2;
//      const int offset     = 48;                  // offset
//      const bool wrappable = true;
//      v2.reserve(v1.size());                      // optional but useful
//      xeona::OffsetTs offset(offset, wrappable);  // functor
//      offset(v1.begin(), v1.end(), std::back_inserter(v2));
//
//  CAUTION: 'push_back' required
//
//      Only those STL containers which offer the 'push_back'
//      member function can be used with 'OffsetTs'.  This
//      includes 'std::vector'.
//
//  References
//
//      Josuttis, Nicolai M.  1999.  The C++ Standard Library : a
//        tutorial and reference.  Addison-Wesley, Boston, USA.
//        ISBN 0-201-37926-0.
//
//      Lischner, Ray.  2003.  C++ in a nutshell : a language and
//        library reference.  O'Reilly and Associates, Sebastopol,
//        California, USA.  ISBN 0-596-00298-X.
//
// ---------------------------------------------------------

namespace xeona
{
  class OffsetTs
  {
  private:

    OffsetTs();                              // zero-argument constructor

  public:

    OffsetTs
    (const unsigned& offsetCount,            // zero is valid
     const bool      canWrap = false)
      throw (std::domain_error) :
      offset(offsetCount),                   // offset count
      wrappable(canWrap)                     // can loop the vector
    { }

    template <typename InIter, typename BackInser>
    BackInser
    operator()
    (InIter    first1,
     InIter    last1,
     BackInser result)
      throw (std::domain_error)
    {
      // preliminary
      const unsigned size = last1 - first1;

      // integrity checks on processing
      if ( wrappable == false && offset > size )
        {
          std::ostringstream oss;
          oss << "OffsetTs" << " functor"
              << ": offset exceeds length and vector cannot be wrapped" << ": " << size;
          throw std::domain_error(oss.str());
        }

      // active code
      switch ( wrappable )
        {
        case true:
          {
            InIter first2 = first1;          // keep for second loop
            InIter last2  = last1;           // keep for second loop
            const unsigned wrapoffset = offset % size;
            first1 += wrapoffset;            // advance
            for ( ; first1 != last1; ++first1 )
              {
                result++ = *first1;          // last part
              }
            last2 -= (size - wrapoffset);    // backtrack
            for ( ; first2  != last2; ++first2 )
              {
                result++ = *first2;          // first part
              }
          }
          break;
        case false:
          {
            first1 += offset;                 // advance
            for ( ; first1 != last1; ++first1 )
              {
                result++ = *first1;
              }
          }
          break;
        }
      return result;
    }

  private:

    const unsigned    offset;                // offset count
    const bool        wrappable;             // request to add leap year

  }; // class 'OffsetTs'

}

// ---------------------------------------------------------
//  CLASS (FUNCTOR) : xeona::Month2TsDuplicate
// ---------------------------------------------------------
//  Description  : convert a monthly series to equal duplicates
//  Role         : custom STL algorithm
//  Techniques   : functor
//  Status       : complete
//
//  Design notes
//
//      This functor changes a monthseries into a timeseries.  To
//      do so, the constructor needs to be passed the interval
//      length in [s].  The caller is responsible for knowing if
//      the output needs leap year data.
//
//      The functor will throw a 'std::domain_error' if an 'i' of
//      zero is submitted.  This otherwise avoids subsequent
//      problems.  The functor will throw a 'std::domain_error'
//      if the 'i' is not a divisor of 24 * 3600.
//
//      See Josuttis (1999 pp271-275) for a discussion on
//      inserters.  And Lischner (2003 p275) on custom algorithms
//      and a 'copy_if' implementation.
//
//  Example
//
//      #include <iterator>
//
//      std::vector<double> v1;
//      // fill 'v1' with monthly values
//      std::vector<double> v2;
//      const int interval = 7200;                   // interval in seconds
//      v2.reserve(2 * v1.size());                   // optional but useful
//      xeona::Month2TsDuplicate convert(interval);  // functor
//      convert(v1.begin(), v1.end(), std::back_inserter(v2));
//
//  CAUTION: 'push_back' required
//
//      Only those STL containers which offer the 'push_back'
//      member function can be used with 'ModifyTsReLeapYear'.
//      This includes 'std::vector'.
//
//  References
//
//      Josuttis, Nicolai M.  1999.  The C++ Standard Library : a
//        tutorial and reference.  Addison-Wesley, Boston, USA.
//        ISBN 0-201-37926-0.
//
//      Lischner, Ray.  2003.  C++ in a nutshell : a language and
//        library reference.  O'Reilly and Associates, Sebastopol,
//        California, USA.  ISBN 0-596-00298-X.
//
// ---------------------------------------------------------

namespace xeona
{
  class Month2TsDuplicate
  {
  private:

    Month2TsDuplicate();                               // zero-argument constructor

  public:

    Month2TsDuplicate
    (const unsigned& i,                                // interval length in seconds
     const bool      leapYear = false)
      throw (std::domain_error) :
      ipd((24.0 * 3600.0) / static_cast<double>(i)),   // intervals per day
      addFeb29(leapYear),                              // duly adjust 'months'
      months()                                         // days per each month
    {
      // integrity checks on construction
      if ( i == 0 )
        {
          std::ostringstream oss;
          oss << __func__ << " functor" << ": interval value cannot be zero" << ": " << i;
          throw std::domain_error(oss.str());
        }
      if ( 24 * 3600 % i != 0 )                        // 'i' should not be zero
        {
          std::ostringstream oss;
          oss << __func__ << " functor" << ": interval value not a divisor of 24 * 3600"
              << ": " << i;
          throw std::domain_error(oss.str());
        }

      // load the days per month values
      months += 31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31;
      if ( addFeb29 ) ++months.at(1);        // adjust the second month
    }

    template <typename InIter, typename BackInser>
    BackInser
    operator()
    (InIter    first,
     InIter    last,
     BackInser result)
      throw (std::domain_error)
    {
      // integrity checks on processing
      const unsigned size = last - first;
      if ( size < 1 || size > 12 )
        {
          std::ostringstream oss;
          oss << "Month2TsDuplicate" << " functor"
              << ": source length outside [1,12]" << ": " << size;
          throw std::domain_error(oss.str());
        }

      // active code
      int loop = 0;
      for ( ; first != last; ++first )                 // loop given monthly values
        {
          for ( int j = 0; j < months.at(loop); ++j )  // loop days
            {
              for ( int k = 0; k < ipd; ++k )          // loop intervals per day
                {
                  result++ = *first;
                }
            }
          loop++;
        }
      return result;
    }

  private:

    const int           ipd;                 // intervals per day
    const bool          addFeb29;            // request to add leap year
    std::vector<int>    months;              // list of days per month

  }; // class 'Month2TsDuplicate'

}

#undef XEONA_FUNC

#endif // _TSALGOS_H_

//  end of file

