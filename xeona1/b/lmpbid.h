//  XEONA ENERGY SYSTEMS MODELING ENVIRONMENT
//
//  file-create-name : lmpbid.h
//  file-create-date : Fri 17-Oct-2008 12:53 UTC
//  file-initiator   : Robbie Morrison <robbie@actrix.co.nz>
//
//  file-role        : LMP auction bidset and support / header
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
//  $URL: file:///home/robbie/svn-root/xeona/futz/trunk/xeona1/b/lmpbid.h $
//
//  GENERAL NOTES FOR THIS FILE

//  HEADER GUARD

#ifndef _LMPBID_H_
#define _LMPBID_H_

//  LOCAL AND SYSTEM INCLUDES

#include "../c/util3.h"       // free functions for floating point comparison

#include "../a/logger_fwd.h"  // some forward declarations from "logger.h"
#include "../c/smart_ptr.h"   // toggle between Boost and TR1 smart pointers

#include <sstream>            // string-streams
#include <string>             // C++ strings
#include <utility>            // STL pair, make_pair()
#include <vector>             // STL sequence container

//  CODE

// ---------------------------------------------------------
//  CLASS           : LmpBidSet
// ---------------------------------------------------------
//  Description  : bidset abstraction for LMP (nodal) auctions
//  Role         : used to prepare, hold, and unpack a bidset
//  Techniques   : stand-alone class, 'std::pair', 'std::upper_bound'
//  Status       : complete
// ---------------------------------------------------------

class LmpBidSet
{
  // TYPEDEFS

private:

  typedef std::pair<double, double> bid_type;     // namely (band, price)

  // FRIENDS

  friend class CtlLmpBid_A;                       // to access copy constructor [1]

  // [1] the copy constructor and copy assignment operator were
  // later made public but this declaration was nonetheless
  // retrained

  // CAUTION: bidset labels also compared and must be identical
  friend bool operator== (const LmpBidSet&, const LmpBidSet&);
  friend bool operator!= (const LmpBidSet&, const LmpBidSet&);

  // CREATORS

public:

  LmpBidSet
  (std::string label = "");                       // optional label

  ~LmpBidSet();

  // CAUTION: the old bidset label is overwritten by the new label
  LmpBidSet(const LmpBidSet& orig);               // copy constructor
  LmpBidSet& operator= (const LmpBidSet& orig);   // copy assignment operator

  // UNARY OPERATORS

public:

  LmpBidSet& operator+= (const double& other);    // price increment
  LmpBidSet& operator-= (const double& other);    // price decrement
  LmpBidSet& operator*= (const double& other);    // price multiplier

  // MANIPULATORS

public:

  void
  clear();                                   // remove all elements

  std::string                                // previous label
  relabel
  (const std::string label);                 // new label

  int                                        // number of bids loaded this time
  pushString                                 // parses input, uses 'pushBid' to load data
  (const std::string sBidset);

  int                                        // current number of bids
  pushBid
  (const bid_type bid);

  // ---------------------------------------------------------
  //  documentation   : Nodal pricing auction practice
  // ---------------------------------------------------------
  //
  //  Purpose of XE_NEGATION
  //
  //     The following conditional compilation allows one to
  //     experiment with the nodal pricing auction:
  //
  //       0 do nothing : disable negation call functionality
  //
  //       1 price negate : the rationale is that demand-side
  //         curves naturally slope down, therefore the bidset
  //         unit prices -- which represent the slope piecewise
  //         -- need to be multiplied by minus one
  //
  //       2 quantity negate : simply for completeness
  //
  //  Explanation
  //
  //      Take eqn (1) from Motto etal (2002) and simplify
  //      (remove the terms) not used here:
  //
  //          max : sum bidsets_demand - sum bidsets_supply
  //
  //      This code equivalently uses, with particular reference
  //      the demand-side bidding entities in 'b/asop02':
  //
  //          min : sum bidsets_supply + sum -bidsets_demand
  //
  //      These two formulations are therefore equivalent (simply
  //      multiply thru by negative one).
  //
  //  References
  //
  //      Motto, Alexis L, Fransisco D Galiana, Antonio J Conejo,
  //        and Jose M Arroyo.  2002.  Network-constrained
  //        multiperiod auction for a pool-based electricity
  //        market.  IEEE Transactions on Power Systems v17 no3
  //        p646-653.  doi:10.1109/TPWRS.2002.800909
  //
  // ---------------------------------------------------------

#define XE_NEGATION 1                        // 1 is correct (see above)
#if (XE_NEGATION == 0)
  void  negate() { }                         // disabled
  void unegate() { }
#elif (XE_NEGATION == 1)
  void  negate() {   negatePrice(); }        // price negation
  void unegate() { unnegatePrice(); }
#elif (XE_NEGATION == 2)
  void  negate() {   negateBands(); }        // quantity negation
  void unegate() { unnegateBands(); }
#else
  #warning "bidset negation macro XE_NEGATION not valid"
#endif // XE_NEGATION

  void
  negatePrice();                             // negate all unit prices on recovery

  void
  unnegatePrice();                           // cancel any earlier 'negatePrice' call

  void
  negateBands();                             // negate all bands on recovery

  void
  unnegateBands();                           // cancel any earlier 'negateBands' call

  bid_type
  popBig();                                  // pop biggest, meaning current highest price

  bid_type
  popSmall();                                // pop smallest, meaning current lowest price

  void
  rescalePrices
  (const double factor);

  double                                     // return the rescaling factor used
  rescaleWeightedPrice
  (const double newPrice);

  // ACCESSORS

  std::string
  label() const;

  int                                        // current number of bids
  size() const;

  double
  getLoQuantity() const;

  double
  getHiQuantity() const;

  double
  getWeightedPrice() const;

  bool                                       // true means 'duty' would not be met
  wouldCapacitate                            // uses floating-point comparison
  (const double           duty,
   double&                hiQuantity,        // duly loaded
   const xeona::Precision test = xeona::numic) const;

  std::string                                // 'modelBidDelim'-separated for output
  stringify() const;

  std::string
  summarizeAll                               // trailing newline not required
  (const int indent) const;                  // left margin

  // INSTANCE DATA

private:

  std::string              d_label;               // optional
  std::vector<bid_type>    d_bidset;              // sorted
  bool                     d_defaultNegatePrice;  // default negate value for price
  bool                     d_defaultNegateBands;  // default negate value for bands
  bool                     d_negatePrice;         // defaults as above
  bool                     d_negateBands;         // defaults as above

  // STATIC DATA

private:

  static logga::spLogger    s_logger;        // shared_ptr to single logger object

}; // class 'LmpBidSet'

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
 const LmpBidSet& rhs);

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
 const LmpBidSet& rhs);

// ---------------------------------------------------------
//  FREE FUNCTION   : xeona::stringifyBidSets
// ---------------------------------------------------------
//  Description  : stringifies a vector of bidsets, currently separated thus " / "
//  Role         : general use
//  Techniques   : 'boost::foreach'
//  Status       : complete
// ---------------------------------------------------------

class LmpBidSet;                             // forward (partial) declaration

namespace xeona
{
  std::string
  stringifyBidSets
  (const std::vector<LmpBidSet>& bidsets);

  std::string
  stringifyBidSets
  (const std::vector<shared_ptr<LmpBidSet> >& bidsets);
}

#endif // _LMPBID_H_

//  end of file

