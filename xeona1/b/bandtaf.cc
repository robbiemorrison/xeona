//  XEONA ENERGY SYSTEMS MODELING ENVIRONMENT
//
//  file-create-name : bandtaf.cc
//  file-create-date : Tue 18-Nov-2008 09:37 UTC
//  file-initiator   : Robbie Morrison <robbie@actrix.co.nz>
//
//  file-role        : banded tariff set and support / implementation
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
//  $URL: file:///home/robbie/svn-root/xeona/futz/trunk/xeona1/b/bandtaf.cc $

//  LOCAL AND SYSTEM INCLUDES

#include "bandtaf.h"          // companion header for this file (place first)

#include "../a/logger.h"      // standard logging functionality (as required)
#include "../c/smart_ptr.h"   // toggle between Boost and TR1 smart pointers
#include ".././common.h"      // common definitions for project (place last)

#include <algorithm>          // STL copying, searching, and sorting
#include <limits>             // numeric_limits<T>::infinity() and similar
#include <numeric>            // STL numerical algorithms
#include <sstream>            // string-streams
#include <string>             // C++ strings
#include <utility>            // STL pair, make_pair()
#include <vector>             // STL sequence container

#include <boost/algorithm/string.hpp>   // string recasing, trimming, splitting
#include <boost/foreach.hpp>            // BOOST_FOREACH iteration macro
#include <boost/format.hpp>             // printf style formatting
#include <boost/lexical_cast.hpp>       // lexical_cast<> string to number conversions

//  CODE

// ---------------------------------------------------------
//  FREE FUNCTION   : std::pair<double, double> ::operator+
// ---------------------------------------------------------
//  Description  : 'std::pair<double, double>' addition
//  Role         : required by 'std::accumulate'
//  Techniques   : free function, operator overloading
//  Status       : complete
//
//  CAUTION: file scope
//
//      This operator is currently set in file scope, but that
//      could be relaxed if deemed useful.  The given semantics
//      should meet most expectations.
//
// ---------------------------------------------------------

namespace
{
  const std::pair<double, double>
  operator+
  (const std::pair<double, double>& lhs,
   const std::pair<double, double>& rhs)
  {
    std::pair<double, double> tmp(lhs.first  + rhs.first,
                                  lhs.second + rhs.second);
    return tmp;
  }
}

// ---------------------------------------------------------
//  FUNCTION        : ::tariff_less
// ---------------------------------------------------------
//  Description  : custom STL comparison predicate
//  Role         : for 'std::max_element' and similar
//  Techniques   : STL algorithms, unnamed namespace (file local scope)
//  Status       : complete
// ---------------------------------------------------------

namespace
{
  bool
  tariff_less
  (std::pair<double, double> one,
   std::pair<double, double> two)
  {
    return one.second < two.second;          // compares second element
  }
} // unnamed namespace

// ---------------------------------------------------------
//  CLASS           : BandedTariffSet
// ---------------------------------------------------------
//  Description  : contract-to-supply tariff set abstraction
//  Role         : used to prepare, hold, and unpack a variety of banded tariff sets
//  Techniques   : stand-alone class, 'std::pair', 'std::stable_sort'
//  Status       : complete
//
//  Design notes
//
//      Overview
//
//          A banded tariff is an UNSORTED collection of (band,
//          price) 'std::pair's held in a 'std::vector.'  Unlike
//          an LMP bidset, the ORDER OF INSERTION is significant.
//
//          In this file, the term 'price' more particularly
//          means 'unit price' in [CUR/J].  A band is specified
//          in [W].
//
//          A 'specific fixed charge' in [CUR/Ws] may also be
//          specified, otherwise the hard-coded default of zero
//          will be used (note that "Ws" is Watt x second).
//
//          A given 'unit price' and 'specific fixed charge' may
//          be strictly positive, zero, or strictly negative.  A
//          'band' must be strictly positive and any
//          non-complying 'band' will be skipped during parsing.
//
//      Input format
//
//          In terms of input, the 'xeona' model more generally
//          expects:
//
//            * a commodity band in [W]
//            * a unit price in [CUR/J]
//
//          In addition, a fixed charge can be given.  If not,
//          the hard-coded default of zero will be used:
//
//            * a specific fixed charge in [CUR/Ws]
//              (or equivalently [CUR/s/W])
//
//          Moreover, the fixed charge can only be given once.
//          Any additional specification is skipped and a warning
//          is logged.
//
//          The 'load' call expects a single string with the
//          following formatting rules:
//
//            * tariff pair as given above: (band, price)
//            * special case single field: fixed charge
//            * tariff separator as defined by 'xeona::modelBidDelim'
//            * band/price separator is space character
//
//          The individual tariffs are inserted in the order given.
//
//          String insertion is supported via 'pushString' in the
//          prescribed format, for instance:
//
//              "8.00e-6 * 40.00e+06 28.00e-09 * 20.00e+06 40.00e-09"
//
//      Interpretation
//
//          The following interpretations are supported under
//          'summarizeAll':
//
//              "null data or not loaded"
//              "free supply"
//              "simple"
//              "take-or-pay"
//              "standard"
//
// ---------------------------------------------------------

// STATIC DEFINITIONS

logga::spLogger BandedTariffSet::s_logger = logga::ptrLogStream();

// CREATORS

// ---------------------------------------------------------
//  MEMBER FUNCTION : BandedTariffSet
// ---------------------------------------------------------

BandedTariffSet::BandedTariffSet
(std::string label) :
  d_label(label),
  d_fixed(0.0),
  d_tariffset_pop(),
  d_tariffset_fix(),
  d_capacity(std::numeric_limits<double>::infinity())
{
  s_logger->repx(logga::xtra, "constructor call, label", d_label);
}

// ---------------------------------------------------------
//  MEMBER FUNCTION : ~BandedTariffSet
// ---------------------------------------------------------

BandedTariffSet::~BandedTariffSet()
{
  s_logger->repx(logga::adhc, "destructor call", "");
}

// ---------------------------------------------------------
//  MEMBER FUNCTION : copy constructor
// ---------------------------------------------------------

// explicit definition required because of friendship declaration,
// also now public

BandedTariffSet::BandedTariffSet
(const BandedTariffSet& orig) :
  d_label(orig.d_label),
  d_fixed(orig.d_fixed),
  d_tariffset_pop(orig.d_tariffset_pop),
  d_tariffset_fix(orig.d_tariffset_fix),
  d_capacity(orig.d_capacity)
{
  s_logger->repx(logga::adhc, "copy constructor call", d_label);
}

// ---------------------------------------------------------
//  MEMBER FUNCTION : copy assignment operator
// ---------------------------------------------------------

BandedTariffSet&
BandedTariffSet::operator=
(const BandedTariffSet& orig)
{
  // initial reporting
  s_logger->repx(logga::adhc, "copy assignment operator call", d_label);

  // complaint if self assignment
  if ( this == &orig )
    {
      s_logger->repx(logga::warn, "copy assignment of self (a = a)", "");
      return *this;
    }

  // copy and return
  this->d_label         = orig.d_label;
  this->d_fixed         = orig.d_fixed;
  this->d_tariffset_pop = orig.d_tariffset_pop;
  this->d_tariffset_fix = orig.d_tariffset_fix;
  this->d_capacity      = orig.d_capacity;

  return *this;
}

// MANIPULATORS

// ---------------------------------------------------------
//  MEMBER FUNCTION : clear
// ---------------------------------------------------------

void
BandedTariffSet::clear()                     // remove tariffs and zero the fixed charge
{
  s_logger->repx(logga::adhc, "entering member function", "");
  d_tariffset_pop.clear();
  d_tariffset_fix.clear();
  d_fixed = 0.0;
}

// ---------------------------------------------------------
//  MEMBER FUNCTION : setLabel
// ---------------------------------------------------------

std::string                                  // return old label
BandedTariffSet::setLabel                    // set new label
(const std::string& label)
{
  std::string temp = d_label;
  d_label = label;
  return temp;
}

// ---------------------------------------------------------
//  MEMBER FUNCTION : pushString
// ---------------------------------------------------------
//  Description  : tariffset parsing code for say "50 * 40.00e+06 28.00e-09"
//  Role         : loading a 'BandedTariffSet' instance using model input data
//  Techniques   : Boost.String_algo, calls 'pushTariff' for insertion in 'd_tariffset_*'
//  Status       : complete
//
//  Design notes
//
//      As currently coded, this member function can be called
//      multiple times.
//
//      This code could have used the Boost.Tokenizer library,
//      but 'boost::algorithm::split' from the Boost.String_algo
//      library is employed instead.
//
//      The "sName" prefix is used to indicate a string variable.
//
//      Note that 'continue' and 'break' are supported by the
//      'Boost.Foreach' library.
//
// ---------------------------------------------------------

int                                          // number of tariffs loaded this time
BandedTariffSet::pushString                  // parses input, uses 'pushTariff' to load
(const std::string sTariffset)
{
  // initial reporting
  s_logger->repx(logga::adhc, "entering member function, strlen", sTariffset.length());
  int loopCount  = 0;                        // also the return value
  int fixedCount = 0;

  // defensive programming
  if ( sTariffset.empty() )
    {
      s_logger->repx(logga::warn, "empty tariff set string", "");
      return loopCount;
    }

  // note: insert 'clear' call here to reset each time

  // split input string into tariffs
  std::vector<std::string> sTariffs;              // split on tariff separator
  const std::string bs = xeona::modelBidDelim;    // bid separator set in 'common.cc'
  boost::algorithm::split(sTariffs,
                          sTariffset,
                          boost::algorithm::is_any_of(bs),  // tariff separator
                          boost::algorithm::token_compress_off);

  // process individual tariffs
  std::vector<std::string> fields;           // split on price-quantity separator
  double fixed;                              // double for assignment
  tariff_type tariff;                        // tariff for insertion
  BOOST_FOREACH( std::string sTariff, sTariffs )
    {
      // split individual tariffs into fields
      fields.clear();                        // remove all elements
      boost::algorithm::trim(sTariff);       // CAUTION: 'token_compress_on' insufficient
      boost::algorithm::split(fields,
                              sTariff,
                              boost::algorithm::is_any_of(" "),  // field separator
                              boost::algorithm::token_compress_on);
      // switch on structure
      const int elementCount = fields.size();
      switch ( elementCount )
        {
        case 1:
          // attempt a string to double cast
          try
            {
              fixed = boost::lexical_cast<double>(fields.at(0));
            }
          catch( const boost::bad_lexical_cast& eblc)
            {
              // what "bad lexical cast: source type value could
              // not be interpreted as target"
              std::ostringstream put;
              put << "  " << "tariff set string to double cast failure" << "\n"
                  << "  " << "split string: " << fields.at(0) << "\n"
                  << "  " << "what: " << eblc.what() << "\n";
              s_logger->putx(logga::warn, put);
              continue;
            }
          // load data
          ++fixedCount;
          if ( fixedCount == 1 )             // load data if first time
            {
              setSpecificFixedCharge(fixed); // member function call
            }
          else                               // more than one fixed charges present
            {
              std::ostringstream oss;
              oss << fixedCount << " " << fixed;
              s_logger->repx(logga::warn, "additional fixed charge ignored", oss.str());
            }
          break;

        case 2:
          // attempt a string to double cast
          try
            {
              tariff.first  = boost::lexical_cast<double>(fields.at(0));
              tariff.second = boost::lexical_cast<double>(fields.at(1));
            }
          catch( const boost::bad_lexical_cast& eblc)
            {
              // what "bad lexical cast: source type value could
              // not be interpreted as target"
              std::ostringstream put;
              put << "  " << "tariff set string to double cast failure" << "\n"
                  << "  " << "split strings: " << fields.at(0)
                  << " " << fields.at(1) << "\n"
                  << "  " << "what: " << eblc.what() << "\n";
              s_logger->putx(logga::warn, put);
              continue;
            }
          // load data
          if ( pushTariff(tariff) > 0 )      // member function for sorted insertion
            {
              ++loopCount;                   // zero indicates failure to insert
            }
          break;

        default:
          s_logger->repx(logga::warn, "invalid tariff field size", elementCount);
          break;
        }
    }

  // update capacity
  d_capacity = getBandSum();

  // return
  return loopCount;
}

// ---------------------------------------------------------
//  MEMBER FUNCTION : pushTariff
// ---------------------------------------------------------
//  Description  : addes a new tariff
//  Role         : direct use or via 'pushString'
//  Techniques   : (nothing special)
//  Status       : complete
// ---------------------------------------------------------

int                                          // current number of tariffs or zero on fail
BandedTariffSet::pushTariff
(const tariff_type tariff)
{
  // initial reporting
  std::ostringstream oss;
  oss << tariff.first << " " << tariff.second;
  s_logger->repx(logga::adhc, "entering member function, tariff", oss.str());

  // integrity checks
  if ( tariff.first < 0.0 )
    {
      s_logger->repx(logga::warn, "negative tariff band unacceptable", tariff.first);
      return 0;
    }
  else if ( tariff.first == 0.0 )
    {
      s_logger->repx(logga::dbug, "zero tariff band skipped", tariff.first);
      return 0;
    }

  // load the tariff
  d_tariffset_pop.push_back(tariff);         // unsorted insertion
  d_tariffset_fix.push_back(tariff);         // unsorted insertion

  // update capacity
  d_capacity = getBandSum();

  // return
  return d_tariffset_fix.size();             // excludes any popping
}

// ---------------------------------------------------------
//  MEMBER FUNCTION : setSpecificFixedCharge
// ---------------------------------------------------------

void
BandedTariffSet::setSpecificFixedCharge
(const double specFixedCharge)
{
  d_fixed = specFixedCharge;                 // overwrite action
}

// ---------------------------------------------------------
//  MEMBER FUNCTION : popLast
// ---------------------------------------------------------

BandedTariffSet::tariff_type
BandedTariffSet::popLast()                   // pop newest tariff
{
  // initial reporting
  s_logger->repx(logga::adhc, "entering member function", "");

  // defensive programming
  if ( d_tariffset_pop.empty() )
    {
      s_logger->repx(logga::warn,
                     "attempt to pop an empty tariff set",
                     d_tariffset_pop.size());
      return tariff_type(0.0, 0.0);
    }

  // active code
  tariff_type temp;
  temp = d_tariffset_pop.back();             // CAUTION: 'back' requires element to exist
  d_tariffset_pop.pop_back();                // CAUTION: 'pop_back' doesn't return element
  return temp;
}

// ---------------------------------------------------------
//  MEMBER FUNCTION : popFirst
// ---------------------------------------------------------

BandedTariffSet::tariff_type
BandedTariffSet::popFirst()                  // pop oldest tariff
{
  // initial reporting
  s_logger->repx(logga::adhc, "entering member function", "");

  // defensive programming
  if ( d_tariffset_pop.empty() )
    {
      s_logger->repx(logga::warn,
                     "attempt to pop an empty tariff set",
                     d_tariffset_pop.size());
      return tariff_type(0.0, 0.0);
    }

  tariff_type temp;
  temp = d_tariffset_pop.front();                 // CAUTION: 'front' element must exist
  d_tariffset_pop.erase(d_tariffset_pop.begin()); // CAUTION: 'erase' element must exist
  return temp;
}

// ---------------------------------------------------------
//  MEMBER FUNCTION : truncate
// ---------------------------------------------------------

bool                                       // 'true' indicates the 'capacity' binds
BandedTariffSet::truncate
(const double capacity)
{
  // initial reporting
  s_logger->repx(logga::adhc, "leaving member function, capacity", capacity);

  // defensive programming
  if ( capacity < 0.0 )
    {
      s_logger->repx(logga::warn, "negative capacity rejected", capacity);
      return false;
    }

  // active code
  if ( capacity > getBandSum() )
    {
      s_logger->repx(logga::dbug, "capacity is non-binding", capacity);
      d_capacity = capacity;
      return false;
    }
  else
    {
      s_logger->repx(logga::dbug, "capacity is non-binding", capacity);
      d_capacity = capacity;
      return true;
    }
}

// ACCESSORS

// ---------------------------------------------------------
//  MEMBER FUNCTION : getLabel
// ---------------------------------------------------------

std::string
BandedTariffSet::getLabel() const
{
  return d_label;
}

// ---------------------------------------------------------
//  MEMBER FUNCTION : getSpecificFixedCharge
// ---------------------------------------------------------

double
BandedTariffSet::getSpecificFixedCharge() const
{
  return d_fixed;
}

// ---------------------------------------------------------
//  MEMBER FUNCTION : getFirstOnTariff
// ---------------------------------------------------------

BandedTariffSet::tariff_type
BandedTariffSet::getFirstOnTariff() const
{
  if ( d_tariffset_fix.size() == 0 )
    {
      return tariff_type(0.0, 0.0);
    }
  else
    {
      return d_tariffset_fix.front();
    }
}

// ---------------------------------------------------------
//  MEMBER FUNCTION : getLastOnTariff
// ---------------------------------------------------------

BandedTariffSet::tariff_type
BandedTariffSet::getLastOnTariff() const
{
  if ( d_tariffset_fix.size() == 0 )
    {
      return tariff_type(0.0, 0.0);
    }
  else
    {
      return d_tariffset_fix.back();
    }
}

// ---------------------------------------------------------
//  MEMBER FUNCTION : getHighestTariff
// ---------------------------------------------------------

BandedTariffSet::tariff_type
BandedTariffSet::getHighestTariff() const
{
  if ( d_tariffset_fix.size() == 0 )
    {
      return tariff_type(0.0, 0.0);
    }
  else
    {
      return *std::max_element(d_tariffset_fix.begin(),     // refer <algorithm>
                               d_tariffset_fix.end(),
                               &::tariff_less);             // refer this file
    }
}

// ---------------------------------------------------------
//  MEMBER FUNCTION : getLowestTariff
// ---------------------------------------------------------

BandedTariffSet::tariff_type
BandedTariffSet::getLowestTariff() const
{
  if ( d_tariffset_fix.size() == 0 )
    {
      return tariff_type(0.0, 0.0);
    }
  else
    {
      return *std::min_element(d_tariffset_fix.begin(),     // refer <algorithm>
                               d_tariffset_fix.end(),
                               &::tariff_less);             // refer this file
    }
}

// ---------------------------------------------------------
//  MEMBER FUNCTION : getBandSum
// ---------------------------------------------------------

double
BandedTariffSet::getBandSum() const
{
  tariff_type initialValue(0.0, 0.0);
  tariff_type sum = std::accumulate(d_tariffset_fix.begin(),
                                    d_tariffset_fix.end(),
                                    initialValue,
                                    &::operator+);     // CAUTION: arg must be explicit
  return sum.first;
}

// ---------------------------------------------------------
//  MEMBER FUNCTION : getCapacity
// ---------------------------------------------------------

double                                       // could be infinity
BandedTariffSet::getCapacity() const
{
  return d_capacity;
}

// ---------------------------------------------------------
//  MEMBER FUNCTION : isNonConvex
// ---------------------------------------------------------

// design allows for strictly negative prices

bool
BandedTariffSet::isNonConvex() const
{
  // shuffle thru and check for a following lower unit price

  const double bigneg = -std::numeric_limits<double>::max();
  tariff_type comp(0.0, bigneg);             // only second element is used
  BOOST_FOREACH( tariff_type t, d_tariffset_fix )
    {
      if ( t.second < comp.second )          // strictly less
        {
          return true;
        }
      comp = t;
    }
  return false;
}

// ---------------------------------------------------------
//  MEMBER FUNCTION : isConvex
// ---------------------------------------------------------

bool
BandedTariffSet::isConvex() const
{
  return ( isNonConvex() == false );
}

// ---------------------------------------------------------
//  MEMBER FUNCTION : empty
// ---------------------------------------------------------

bool
BandedTariffSet::empty() const
{
  return ( d_tariffset_pop.size() == 0 );
}

// ---------------------------------------------------------
//  MEMBER FUNCTION : unfilled
// ---------------------------------------------------------

bool
BandedTariffSet::unfilled() const            // default fixed charge and no tariff data
{
  return ( d_fixed == 0.0                    // default value
           &&
           d_tariffset_fix.size() == 0 );    // no tariff data
}

// ---------------------------------------------------------
//  MEMBER FUNCTION : size
// ---------------------------------------------------------

int                                          // current number of tariffs
BandedTariffSet::size() const
{
  return  d_tariffset_pop.size();
}

// ---------------------------------------------------------
//  MEMBER FUNCTION : bands
// ---------------------------------------------------------

int                                          // current number of tariffs
BandedTariffSet::bands() const
{
  return  d_tariffset_fix.size();
}

// ---------------------------------------------------------
//  MEMBER FUNCTION : summarizeAll
// ---------------------------------------------------------
//  Description  : output 'd_tariffset_fix' in human readable form
//  Role         : development purposes
//  Techniques   : 'Boost.Format'
//  Status       : complete
//
//  Design notes
//
//      Typical output
//
//          The following is typical.  The first part has
//          suppressed precision, whereas the the part in
//          brackets does not.  The currency string is now "$".
//
//    tariff set label: for-unit-testing
//       4.40e-00 $/s  (4.4)
//       40.00e+06 W  28.00e-09 $/J  (4e+07, 2.8e-08)
//       30.00e+06 W  10.00e-09 $/J  (3e+07, 1e-08)
//       20.00e+06 W  80.00e-09 $/J  (2e+07, 8e-08)
//       3 bands totaling: 90.00e+06 W
//       price range: 10.00e-09 - 80.00e-09 $/J  first/last: 28.00e-09, 80.00e-09 $/J
//       summary: non-convex (at least once) with nonzero fixed charge
//
// ---------------------------------------------------------

std::string
BandedTariffSet::summarizeAll() const
{
  // initial reporting
  s_logger->repx(logga::adhc, "entering member function", "");

  std::ostringstream oss;                    // for return as string

  // for convenience
  const int bandz = bands();

  // summarize label
  std::string tag = getLabel();
  if ( tag.empty() ) tag = "(not set)";
  oss << "  " << "tariff set label: " << tag << "\n"
      << "  " << "  loaded data"             << "\n";

  // summarized fixed
  oss << "  " << "   " << boost::format("% .2fe-00 $/s") % d_fixed
      << "  " << "(" << d_fixed << ")" << "\n";

  // loop and summarize tariffs
  // CAUTION: BOOST_FOREACH requires typedef
  BOOST_FOREACH( tariff_type tariff, d_tariffset_fix )
    {
      oss << "  "
          << "   "
          << boost::format("% 6.2fe+06 W")   % (tariff.first  / 1e6)
          << " "
          << boost::format("% 6.2fe-09 $/J") % (tariff.second * 1e9)
          << "  " << "(" << tariff.first << ", " << tariff.second << ")"
          << "\n";
    }

  // add subheading
  oss << "    " << "summary" << "\n";

  // summarize band sum
  if ( bandz == 1 ) oss << "      " << "1 band";
  else              oss << "      " << bandz << " bands";
  oss << " totaling:" << boost::format("% .2fe+06 W") % (getBandSum() / 1e6)
      << "\n";

  // summarize prices
  oss << "      " << "price range:"
      << boost::format("% .2fe-09")     % (getLowestTariff().second  * 1e9)
      << " to"
      << boost::format("% .2fe-09 $/J") % (getHighestTariff().second * 1e9)
      << "  " << "first/last:"
      << boost::format("% .2fe-09")     % (getFirstOnTariff().second * 1e9)
      << " and"
      << boost::format("% .2fe-09 $/J") % (getLastOnTariff().second  * 1e9)
      << "\n";

  // interpret the tariff set
  std::string sy;

  if ( getSpecificFixedCharge() == 0.0 )     // with zero fixed charge
    if      ( bandz == 0 )  sy = "empty with zero fixed charge (null data or not loaded)";
    else if ( bandz == 1 )
      if ( getHighestTariff().second == 0.0 )
                            sy = "zero price with zero fixed charge "
                              "(\"free supply\")";
      else                  sy = "single (non-banded) with zero fixed charge "
                              "(a \"simple\" contract)";
    else
      if ( isNonConvex() )  sy = "non-convex (at least once) with zero fixed charge";
      else                  sy = "convex (never decreasing) with zero fixed charge";
  else if ( getSpecificFixedCharge() > 0.0 ) // with positive fixed charge
    if      ( bandz == 0 )  sy = "empty with nonzero fixed charge";
    else if ( bandz == 1 )
      if ( getHighestTariff().second == 0.0 )
                            sy = "zero price with nonzero fixed charge "
                              "(a \"take-or-pay\" contract)";
      else                  sy = "single (non-banded) with nonzero fixed charge"
                              " (a \"standard\" contract)";
    else
      if ( isNonConvex() )  sy = "non-convex (at least once) with nonzero fixed charge";
      else                  sy = "convex (never decreasing) with nonzero fixed charge";
  else if ( getSpecificFixedCharge() < 0.0 ) // with negative fixed charge
    if      ( bandz == 0 )  sy = "empty with negative fixed charge";
    else if ( bandz == 1 )
      if ( getHighestTariff().second == 0.0 )
                            sy = "zero price with negative fixed charge";
      else                  sy = "single (non-banded) with negative fixed charge";
    else
      if ( isNonConvex() )  sy = "non-convex (at least once) with negative fixed charge";
      else                  sy = "convex (never decreasing) with negative fixed charge";
  else                                       // should not be here
    std::clog << "** coding error 01 in source file " << __FILE__ << std::endl;

  oss << "      " << "contract type: "<< sy << "\n";

  return oss.str();

} // member function 'BandedTariffSet::summarizeAll'

// ---------------------------------------------------------
//  MEMBER FUNCTION : getMarginalPrice
// ---------------------------------------------------------
//  Description  : returns the marginal price for a given quantity
//  Role         : general use
//  Techniques   : simplification of function 'interpretSale'
//  Status       : complete
// ---------------------------------------------------------

double                                       // marginal price ($/quantity)
BandedTariffSet::getMarginalPrice
(const double sale) const                    // transaction (quantity)
{
  // initial reporting
  s_logger->repx(logga::adhc, "entering member function", "");

  // comment on input
  if ( sale == 0)
    {
      s_logger->repx(logga::adhc, "sale is zero", sale);
    }
  else if ( sale < 0 )
    {
      s_logger->repx(logga::warn, "sale is unexpectedly negative", sale);
    }

  // opening state
  double cumulativeBand = 0.0;
  double marginalPrice  = 0.0;
  int    activeTariffs  = 0;

  // loop thru tariffs
  BOOST_FOREACH( tariff_type t, d_tariffset_fix )
    {
      ++activeTariffs;
      double currentBand  = t.first;         // current band
      double currentPrice = t.second;        // current price

      cumulativeBand     += currentBand;     // too high, reduced on loop exit

      if ( sale >= cumulativeBand )          // indicates flooded band
        {
          // loop again
        }
      else                                   // indicates marginal band
        {
          marginalPrice    = currentPrice;   // by definition
          break;                             // bail out
        }
    }

  // return
  return marginalPrice;
}

// ---------------------------------------------------------
//  MEMBER FUNCTION : interpretSale
// ---------------------------------------------------------
//  Description  : interprets tariff set in relation to a sale
//  Role         : used by OSP 'OfrTariffSet::downloadSolution'
//  Techniques   : discrete integration
//  Status       : complete
// ---------------------------------------------------------

boost::tuple                                 // see 'OfrTariffSet::results_type' typedef
<double,                                     // marginal price ($/quantity)
 double,                                     // fixed component ($)
 double,                                     // total price ($)
 double>                                     // transaction (quantity)
BandedTariffSet::interpretSale
(const double size,
 const double sale,
 const int    interval) const
{
  // initial reporting
  s_logger->repx(logga::adhc, "entering member function", "");

  // comment on input
  if ( size == 0)
    {
      s_logger->repx(logga::adhc, "size is zero", size);
    }
  else if ( size < 0 )
    {
      s_logger->repx(logga::warn, "size is unexpectedly negative", size);
    }

  // opening state
  double cumulativeBand = 0.0;
  double fixedComponent = 0.0;
  double totalVarCost   = 0.0;               // thru discrete integration
  double marginalPrice  = 0.0;
  double totalCost      = 0.0;
  int    activeTariffs  = 0;

  // fixed component
  fixedComponent = d_fixed * interval;

  // loop thru tariffs
  BOOST_FOREACH( tariff_type t, d_tariffset_fix )
    {
      ++activeTariffs;
      double currentBand  = t.first;         // current band
      double currentPrice = t.second;        // current price

      cumulativeBand     += currentBand;     // too high, reduced on loop exit

      if ( sale >= cumulativeBand )          // indicates flooded band
        {
          totalVarCost    += currentBand * currentPrice;
        }
      else                                   // indicates marginal band
        {
          totalVarCost    += (sale - (cumulativeBand - currentBand)) * currentPrice;
          cumulativeBand   = sale;
          marginalPrice    = currentPrice;   // by definition
          break;                             // bail out
        }
    }
  totalVarCost = totalVarCost * interval;

  // total cost
  totalCost = fixedComponent + totalVarCost;

  // completion reporting
  std::ostringstream oss;
  oss << activeTariffs << " of " << d_tariffset_fix.size();
  s_logger->repx(logga::adhc, "complete, active tariffs of all tariffs", oss.str());

  // additional reporting as appropriate
  // YEEK 34 CODE (set by '--yeek')
  if ( xeona::yeek == 34 || xeona::yeek == 1 || xeona::yeek == 2 )
    {
      std::ostringstream put;
      put << summarizeAll()                  // trailing newline not required
          << ""                                                         << "\n"
          << "  interpret sale"                                         << "\n"
          << "    inputs"                                               << "\n"
          << "      size     : " << size                     << " W"    << "\n"
          << "      sale     : " << sale                     << " J"    << "\n"
          << "      interval : " << interval                 << " s"    << "\n"
          << "    return values"                                        << "\n"
          << "      marginal price     : " << marginalPrice  << " $/J"  << "\n"
          << "      fixed component    : " << fixedComponent << " $"    << "\n"
          << "      variable component : " << totalVarCost   << " $"    << "\n"
          << "      total cost         : " << totalCost      << " $"    << "\n"
          << "      transaction        : " << cumulativeBand << " J"    << "\n"
          << ""                                                         << "\n"
          << "  note: this reporting uses J (and W) but should more "
        "generally use . as the generic duty unit and * as the "
        "generic capacity unit"                                         << "\n"
          << "        see 'b/costreg.h' for a discussion on the "
        "treatment and taxonomy of costs"                               << "\n";
      s_logger->repx(logga::dbug, "additional reporting follows, yeek", xeona::yeek);
      s_logger->putx(logga::dbug, put);
    }

  // return en-masse
  return boost::make_tuple(marginalPrice,
                           fixedComponent,
                           totalVarCost,
                           cumulativeBand);

} // member function 'BandedTariffSet::interpretSale'

//  end of file

