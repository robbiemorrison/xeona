//  XEONA ENERGY SYSTEMS MODELING ENVIRONMENT
//
//  file-create-name : lmpbid.cc
//  file-create-date : Fri 17-Oct-2008 12:53 UTC
//  file-initiator   : Robbie Morrison <robbie@actrix.co.nz>
//
//  file-role        : LMP auction bidset and support / implementation
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
//  $URL: file:///home/robbie/svn-root/xeona/futz/trunk/xeona1/b/lmpbid.cc $
//
//  GENERAL NOTES FOR THIS FILE

//  LOCAL AND SYSTEM INCLUDES

#include "lmpbid.h"           // companion header for this file (place first)

#include "../a/logger.h"      // standard logging functionality (as required)
#include "../c/smart_ptr.h"   // toggle between Boost and TR1 smart pointers
#include ".././common.h"      // common definitions for project (place last)

#include <algorithm>          // STL copying, searching, and sorting
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
//  FUNCTION        : ::bid_less
// ---------------------------------------------------------
//  Description  : custom STL comparison predicate
//  Role         : sort insertion of individual bids within a given bidset
//  Techniques   : STL algorithms, unnamed namespace (file local scope)
//  Status       : complete
// ---------------------------------------------------------

namespace
{
  bool
  bid_less
  (std::pair<double, double> one,
   std::pair<double, double> two)
  {
    return one.second < two.second;          // compares second element
  }
} // unnamed namespace

// ---------------------------------------------------------
//  CLASS           : LmpBidSet
// ---------------------------------------------------------
//  Description  : bidset abstraction for LMP (nodal) auctions
//  Role         : used to prepare, hold, and unpack a bidset
//  Techniques   : stand-alone class, 'std::pair', 'std::upper_bound'
//  Status       : complete
//
//  Design notes
//
//      Overview
//
//          A bidset is a sorted collection of (band, price)
//          'std::pair's held in a 'std::vector.'
//
//          The sort order is on ascending price.  If two bid
//          pairs have equal price, then their order of
//          appearance is preserved.
//
//      Default negation values
//
//          The 'd_defaultNegatePrice' and 'd_defaultNegateBands'
//          are provided for LMP pricing development purposes --
//          both should normally be set to 'false'.
//
//      Input format
//
//          In terms of input, the 'xeona' model more generally
//          expects:
//
//            * a commodity band in [W]
//            * a unit price in [CUR/J]
//
//          The 'load' call expects a single string with the
//          following formatting rules:
//
//            * field order as given above
//            * bid separator as defined by 'xeona::modelBidDelim'
//            * band/price separator is space character
//
//          The individual bids need not be sorted in any way.
//          Typical input might be (the first bid equates to 4OMW
//          prices at about 10c/kWh):
//
//              "40.00e+06 28.00e-09 * 20.00e+06 40.00e-09"
//
// ---------------------------------------------------------

// STATIC DEFINITIONS

logga::spLogger LmpBidSet::s_logger = logga::ptrLogStream();

// CREATORS

// ---------------------------------------------------------
//  MEMBER FUNCTION : LmpBidSet
// ---------------------------------------------------------

LmpBidSet::LmpBidSet
(std::string label) :
  d_label(label),
  d_bidset(),
  d_defaultNegatePrice(false),               // 'false' is normal
  d_defaultNegateBands(false),               // 'false' is normal
  d_negatePrice(d_defaultNegatePrice),
  d_negateBands(d_defaultNegateBands)
{
  s_logger->repx(logga::xtra, "constructor call, label", d_label);
}

// ---------------------------------------------------------
//  MEMBER FUNCTION : ~LmpBidSet
// ---------------------------------------------------------

LmpBidSet::~LmpBidSet()
{
  s_logger->repx(logga::adhc, "destructor call", d_label);
}

// ---------------------------------------------------------
//  MEMBER FUNCTION : copy constructor
// ---------------------------------------------------------

// explicit definition required because of friendship declaration,
// also now public

LmpBidSet::LmpBidSet
(const LmpBidSet& orig) :
  d_label(orig.d_label),
  d_bidset(orig.d_bidset),
  d_defaultNegatePrice(orig.d_defaultNegatePrice),
  d_defaultNegateBands(orig.d_defaultNegateBands),
  d_negatePrice(orig.d_negatePrice),
  d_negateBands(orig.d_negateBands)
{
  s_logger->repx(logga::adhc, "copy constructor call", d_label);
}

// ---------------------------------------------------------
//  MEMBER FUNCTION : copy assignment operator
// ---------------------------------------------------------

LmpBidSet&
LmpBidSet::operator=
(const LmpBidSet& orig)
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
  this->d_label              = orig.d_label;
  this->d_bidset             = orig.d_bidset;
  this->d_defaultNegatePrice = orig.d_defaultNegatePrice;
  this->d_defaultNegateBands = orig.d_defaultNegateBands;
  this->d_negatePrice        = orig.d_negatePrice;
  this->d_negateBands        = orig.d_negateBands;
  return *this;
}

// UNARY OPERATORS

// ---------------------------------------------------------
//  MEMBER FUNCTION : operator+= (price increment)
// ---------------------------------------------------------

LmpBidSet&
LmpBidSet::operator+=
(const double& other)
{
  BOOST_FOREACH( bid_type& bid, d_bidset )   // CAUTION: typedef and reference essential
    {
      bid.second += other;
    }
  return *this;
}

// ---------------------------------------------------------
//  MEMBER FUNCTION : operator-= (price decrement)
// ---------------------------------------------------------

LmpBidSet&
LmpBidSet::operator-=
(const double& other)
{
  BOOST_FOREACH( bid_type& bid, d_bidset )   // CAUTION: typedef and reference essential
    {
      bid.second -= other;
    }
  return *this;
}

// ---------------------------------------------------------
//  MEMBER FUNCTION : operator*= (price multiplier)
// ---------------------------------------------------------

LmpBidSet&
LmpBidSet::operator*=
(const double& other)
{
  BOOST_FOREACH( bid_type& bid, d_bidset )   // CAUTION: typedef and reference essential
    {
      bid.second *= other;
    }
  return *this;
}

// MANIPULATORS

// ---------------------------------------------------------
//  MEMBER FUNCTION : clear
// ---------------------------------------------------------

void
LmpBidSet::clear()                           // remove all elements
{
  s_logger->repx(logga::adhc, "entering member function", "");
  d_bidset.clear();
}

// ---------------------------------------------------------
//  MEMBER FUNCTION : relabel
// ---------------------------------------------------------

std::string                                  // previous label
LmpBidSet::relabel
(const std::string label)                    // new label
{
  std::ostringstream oss;
  oss << "'" << label << "'" << " was " << "'" << d_label << "'";
  s_logger->repx(logga::adhc, "entering member function", oss.str());
  const std::string oldLabel = d_label;
  d_label                    = label;
  return oldLabel;
}

// ---------------------------------------------------------
//  MEMBER FUNCTION : pushString
// ---------------------------------------------------------
//  Description  : bidset parsing code for say "40.00e+06 28.00e-09 * 20.00e+06 40.00e-09"
//  Role         : loading a 'BidSet' instance using model input data
//  Techniques   : Boost.String_algo, calls 'pushBid' for actual insertion into 'd_bidset'
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
//      The "sCase" prefix is used to indicate a string variable.
//
//      Note that 'continue' and 'break' are supported by the
//      'Boost.Foreach' library.
//
// ---------------------------------------------------------

int                                          // number of bids loaded this time
LmpBidSet::pushString                        // parses input, uses 'pushBid' to load data
(const std::string sBidset)
{
  s_logger->repx(logga::adhc, "entering member function, strlen", sBidset.length());
  int loopCount = 0;                         // also the return value

  // defensive programming
  if ( sBidset.empty() )
    {
      s_logger->repx(logga::warn, "empty bidset string", "");
      return loopCount;
    }

#if 0 // 0 = multiple push calls supported, 1 = 'd_bidset' cleared each time

  clear();                                   // member function

#endif // 0

  // split input string into bids
  std::vector<std::string> sBids;                 // split on bid separator
  const std::string bs = xeona::modelBidDelim;    // bid separator set in 'common.cc'
  boost::algorithm::split(sBids,
                          sBidset,
                          boost::algorithm::is_any_of(bs),  // bid separator
                          boost::algorithm::token_compress_off);

  // process individual bids
  std::vector<std::string> fields;           // split on price-quantity separator
  bid_type bid;                              // bid for insertion
  BOOST_FOREACH( std::string sBid, sBids )
    {
      // split individual bids into fields
      fields.clear();                        // remove all elements
      boost::algorithm::trim(sBid);          // CAUTION: 'token_compress_on' insufficient
      boost::algorithm::split(fields,
                              sBid,
                              boost::algorithm::is_any_of(" "),  // field separator
                              boost::algorithm::token_compress_on);
      // confirm structure
      const int elementCount = fields.size();
      if ( elementCount != 2 )
        {
          s_logger->repx(logga::warn, "bid field size not two", elementCount);
          std::ostringstream put;
          put << "  problematic bid string"                                       << "\n"
              << "    label              :  "         << d_label                  << "\n"
              << "    element count      :  "         << elementCount             << "\n"
              << "    trimmed bid string : " << "\"" << sBid          << "\""     << "\n";
          int loop = 0;
          BOOST_FOREACH( std::string s, fields )
            {
              put << "    element " << ++loop << "          :  " << s << "\n";
            }
          s_logger->putx(logga::xtra, put);
          s_logger->repx(logga::xtra, "abandoning bid processing", "");
          continue;                          // could also be 'break'
        }
      // attempt a string to double cast
      try
        {
          bid.first  = boost::lexical_cast<double>(fields.at(0));
          bid.second = boost::lexical_cast<double>(fields.at(1));
        }
      catch( const boost::bad_lexical_cast& eblc)
        {
          // what "bad lexical cast: source type value could not be interpreted as target"
          std::ostringstream put;
          put << "  " << "bidset string to double cast failure" << "\n"
              << "  " << "split strings: " << fields.at(0) << " " << fields.at(1) << "\n"
              << "  " << "what: " << eblc.what() << "\n";
          s_logger->putx(logga::warn, put);
          continue;
        }

      pushBid(bid);                          // member function for sorted insertion
      ++loopCount;
    }

  return loopCount;
}

// ---------------------------------------------------------
//  MEMBER FUNCTION : pushBid
// ---------------------------------------------------------

// the 'std::upper_bound' algorithm expects a sorted sequence
// -- for which a 'std::vector' container is suitable as long
// as it remains sorted

int                                          // current number of bids
LmpBidSet::pushBid
(const bid_type bid)
{
  // initial reporting
  s_logger->repx(logga::adhc, "entering member function", "");

  // range checking
  const double price = bid.second;
  if ( price < 0.0 )
    {
      s_logger->repx(logga::rankJumpy, "negative unit price encountered", price);
    }

  // active code
  d_bidset.insert                            // refer <vector>
    (std::upper_bound(d_bidset.begin(),      // refer <algorithm>
                      d_bidset.end(),
                      bid,
                      &::bid_less),          // comparison predicate, defined in this file
     bid);                                   // value to insert
  return size();                             // member function
}

// ---------------------------------------------------------
//  MEMBER FUNCTION : negatePrice
// ---------------------------------------------------------

void
LmpBidSet::negatePrice()                     // multiply all unit prices by minus one
{
  d_negatePrice = (! d_defaultNegatePrice);
}

// ---------------------------------------------------------
//  MEMBER FUNCTION : unnegatePrice
// ---------------------------------------------------------

void
LmpBidSet::unnegatePrice()                   // cancel any earlier 'negate' 'call'
{
  d_negatePrice = (! d_defaultNegatePrice);
}

// ---------------------------------------------------------
//  MEMBER FUNCTION : negateBands
// ---------------------------------------------------------

void
LmpBidSet::negateBands()                     // multiply all bands by minus one
{
  d_negateBands = (! d_defaultNegateBands);
}

// ---------------------------------------------------------
//  MEMBER FUNCTION : unnegateBands
// ---------------------------------------------------------

void
LmpBidSet::unnegateBands()                   // cancel any earlier 'negate' 'call'
{
  d_negateBands = (! d_defaultNegateBands);
}

// ---------------------------------------------------------
//  MEMBER FUNCTION : popBig
// ---------------------------------------------------------

LmpBidSet::bid_type
LmpBidSet::popBig()                          // pop in sorted order
{
  s_logger->repx(logga::adhc, "entering member function", "");

  // defensive programming
  if ( d_bidset.empty() )
    {
      s_logger->repx(logga::warn, "attempt to pop an empty bidset", size());
      return bid_type(0.0, 0.0);
    }

  // active code
  bid_type temp;
  temp = d_bidset.back();                    // CAUTION: 'back' requires element to exist
  d_bidset.pop_back();                       // CAUTION: 'pop_back' doesn't return element
  if ( d_negateBands ) temp.first  *= -1;
  if ( d_negatePrice ) temp.second *= -1;
  return temp;
}

// ---------------------------------------------------------
//  MEMBER FUNCTION : popSmall
// ---------------------------------------------------------

LmpBidSet::bid_type
LmpBidSet::popSmall()                        // pop in sorted order
{
  s_logger->repx(logga::adhc, "entering member function", "");

  // defensive programming
  if ( d_bidset.empty() )
    {
      s_logger->repx(logga::warn, "attempt to pop an empty bidset", size());
      return bid_type(0.0, 0.0);
    }

  // active code
  bid_type temp;
  temp = d_bidset.front();                   // CAUTION: 'front' requires element to exist
  d_bidset.erase(d_bidset.begin());          // CAUTION: code requires element to exist
  if ( d_negateBands ) temp.first  *= -1;
  if ( d_negatePrice ) temp.second *= -1;
  return temp;
}

// ---------------------------------------------------------
//  MEMBER FUNCTION : rescalePrices
// ---------------------------------------------------------

void
LmpBidSet::rescalePrices
(const double factor)
{
  if ( factor != 1.0 ) *this *= factor;
}

// ---------------------------------------------------------
//  MEMBER FUNCTION : rescaleWeightedPrice
// ---------------------------------------------------------

double                                       // the rescaling factor used
LmpBidSet::rescaleWeightedPrice
(const double newPrice)
{
  const double was    = getWeightedPrice();
  const double factor = newPrice / was;
  if ( factor != 1.0 ) *this *= factor;
  return factor;
}

// ACCESSORS

// ---------------------------------------------------------
//  MEMBER FUNCTION : label
// ---------------------------------------------------------

std::string
LmpBidSet::label() const
{
  s_logger->repx(logga::adhc, "entering member function, label", d_label);
  return d_label;
}

// ---------------------------------------------------------
//  MEMBER FUNCTION : size
// ---------------------------------------------------------

int                                          // current number of bids
LmpBidSet::size() const
{
  const int size = d_bidset.size();
  s_logger->repx(logga::adhc, "entering member function, size", size);
  return size;
}

// ---------------------------------------------------------
//  MEMBER FUNCTION : getLoQuantity
// ---------------------------------------------------------

double
LmpBidSet::getLoQuantity() const
{
  return 0.0;                                // by definition
}

// ---------------------------------------------------------
//  MEMBER FUNCTION : getHiQuantity
// ---------------------------------------------------------

double
LmpBidSet::getHiQuantity() const
{
  double sum = 0.0;
  BOOST_FOREACH( bid_type bid, d_bidset )    // CAUTION: BOOST_FOREACH requires typedef
    {
      sum += bid.first;
    }
  return sum;
}

// ---------------------------------------------------------
//  MEMBER FUNCTION : getWeightedPrice
// ---------------------------------------------------------
//  Description  : returns weighted average unit price
//  Role         : general use
//  Techniques   : (nothing special)
//  Status       : complete
//
//  Design notes
//
//                        sum( q_i * p_i )
//      weighted-price = ------------------  for i in 1 .. bid-count
//                           sum( q_i )
//
//      returns explicit 'nan' if 'd_bidset' is empty
//
//      returns calculated 'nan' if zero hi-quantity (see above)
//
// ---------------------------------------------------------

double
LmpBidSet::getWeightedPrice() const
{
  // empty bidset case
  if ( d_bidset.empty() )
    {
      return std::numeric_limits<double>::quiet_NaN(); // empty bidset
    }

  // active code
  double quan      = 0.0;
  double priceQuan = 0.0;
  BOOST_FOREACH( bid_type bid, d_bidset )    // CAUTION: BOOST_FOREACH requires typedef
    {
      quan      +=  bid.first;               // sum( q_i )
      priceQuan += (bid.first * bid.second); // sum( q_i * p_i )
    }
  return ( priceQuan / quan );               // can also be 'NaN'
}

// ---------------------------------------------------------
//  MEMBER FUNCTION : wouldCapacitate
// ---------------------------------------------------------
//  Description  : indicates whether 'duty' would be met by the current bid
//  Role         : general use
//  Techniques   : floating-point comparison
//  Status       : complete
//
//  Design notes
//
//      Uses floating-point comparison to exclude false positive
//      returns.
//
// ---------------------------------------------------------

bool                                         // true means 'duty' would not be met
LmpBidSet::wouldCapacitate                   // uses floating-point comparison
(const double           duty,
 double&                hiQuantity,
 const xeona::Precision test) const          // note default
{
  hiQuantity = getHiQuantity();
  if ( xeona::almostEqual(duty, hiQuantity, test) ) return false;
  else if ( duty < hiQuantity ) return false;
  else return true;
}

// ---------------------------------------------------------
//  MEMBER FUNCTION : stringify
// ---------------------------------------------------------
//
//  Design notes
//
//      The semantics of formatting floating point values by C++
//      i/o streams depends somewhat on context.
//
//      In the case of 'std::scientific', precision refers to the
//      number of decimal places displayed.
//
//      Josuttis (1999 p624 table 13.26) gives some key examples.
//
//  References
//
//      Josuttis, Nicolai M.  1999.  The C++ Standard Library : a
//        tutorial and reference.  Addison-Wesley, Boston, USA.
//        ISBN 0-201-37926-0.
//
// ---------------------------------------------------------

std::string                                  // 'modelBidDelim'-separated for output
LmpBidSet::stringify() const
{
  std::ostringstream buffer;
#ifndef _XUTEST
  buffer << std::showpos;                    // useful if negative bids are used
  buffer << std::scientific;
  buffer << std::setprecision(4);            // default is 6
#else
  buffer << std::showpos;
  buffer << std::scientific;
  buffer << std::setprecision(2);            // useful for unit testing
#endif // _XUTEST

  const int bids = d_bidset.size();

  int loop = 0;                              // loop counter
  BOOST_FOREACH( bid_type bid, d_bidset )    // CAUTION: BOOST_FOREACH requires typedef
    {
      buffer << bid.first << " " << bid.second;
      if ( ++loop == bids ) break;
      buffer << " " << xeona::modelBidDelim << " ";    // add the intervening separator
    }

  return buffer.str();
}

// ---------------------------------------------------------
//  MEMBER FUNCTION : summarizeAll
// ---------------------------------------------------------
//  Description  : output 'd_bidset' in human readable form
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
//          brackets does not.  The currency string is now "$."
//
//              40.00e+06 W  28.00e-09 $/J  (4e+07, 2.8e-08)
//
// ---------------------------------------------------------

std::string
LmpBidSet::summarizeAll
(const int indent) const
{
  s_logger->repx(logga::adhc, "entering member function", "");

  const std::string tab(indent, ' ');
  std::ostringstream oss;
  oss << tab << "bidset label: " << d_label << "\n";
  BOOST_FOREACH( bid_type bid, d_bidset )    // CAUTION: BOOST_FOREACH requires typedef
    {
      oss << tab << "  "
          << boost::format("% 6.2fe+06 W")    % (bid.first  / 1e6)
          << " "
          << boost::format("% 6.2fe-09 $/J")  % (bid.second * 1e9)
          << "  " << "(" << bid.first << ", " << bid.second << ")"
          << "\n";
    }
  return oss.str();
}

// ---------------------------------------------------------
//  FREE FUNCTION   : operator== (LmpBidSet&, LmpBidSet&)
// ---------------------------------------------------------
//  Description  : operates on two 'LmpBidSet' objects
//  Role         : general use
//  Techniques   : member data comparison
//  Status       : complete
// ---------------------------------------------------------

bool
operator==
(const LmpBidSet& lhs,
 const LmpBidSet& rhs)
{
  return (lhs.d_label       == rhs.d_label  &&
          lhs.d_bidset      == rhs.d_bidset &&    // no need for 'std::equal' <algorithm>
          lhs.d_negatePrice == rhs.d_negatePrice,
          lhs.d_negateBands == rhs.d_negateBands);
}

// ---------------------------------------------------------
//  FREE FUNCTION   : operator!= (LmpBidSet&, LmpBidSet&)
// ---------------------------------------------------------
//  Description  : operates on two 'LmpBidSet' objects
//  Role         : general use
//  Techniques   : member data comparison
//  Status       : complete
// ---------------------------------------------------------

bool
operator!=
(const LmpBidSet& lhs,
 const LmpBidSet& rhs)
{
  if ( lhs == rhs ) return false;
  else              return true;
}

// ---------------------------------------------------------
//  FREE FUNCTION   : xeona::stringifyBidSets
// ---------------------------------------------------------
//  Description  : stringifies a vector of bidsets, currently separated thus " / "
//  Role         : general use
//  Techniques   : 'boost::foreach'
//  Status       : complete
// ---------------------------------------------------------

namespace xeona
{
  std::string
  stringifyBidSets
  (const std::vector<LmpBidSet>& bidsets)
  {
    static const std::string sep
      = " "
      + xeona::modelBidSetSep                     // see unit 'common', usually "/"
      + " ";

    std::string buffer;
    BOOST_FOREACH( LmpBidSet s, bidsets )
      {
        const std::string bidsetstr = s.stringify();
        buffer += bidsetstr + sep;
      }
    buffer.erase(buffer.length() - sep.length()); // trim final bidset sep string
    return buffer;
  }

  std::string
  stringifyBidSets
  (const std::vector<shared_ptr<LmpBidSet> >& bidsets)
  {
    static const std::string sep
      = " "
      + xeona::modelBidSetSep                     // see unit 'common', usually "/"
      + " ";

    std::string buffer;
    BOOST_FOREACH( shared_ptr<LmpBidSet> s, bidsets )
      {
        const std::string bidsetstr = s->stringify();
        buffer += bidsetstr + sep;
      }
    buffer.erase(buffer.length() - sep.length()); // trim final bidset sep string
    return buffer;
  }
}

//  end of file

