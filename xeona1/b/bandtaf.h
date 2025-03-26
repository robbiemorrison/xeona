//  XEONA ENERGY SYSTEMS MODELING ENVIRONMENT
//
//  file-create-name : bandtaf.h
//  file-create-date : Tue 18-Nov-2008 09:37 UTC
//  file-initiator   : Robbie Morrison <robbie@actrix.co.nz>
//
//  file-role        : banded tariff set and support / header
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
//  $URL: file:///home/robbie/svn-root/xeona/futz/trunk/xeona1/b/bandtaf.h $

//  HEADER GUARD

#ifndef _BANDTAF_H_
#define _BANDTAF_H_

//  AD-HOC NOTES
//
//  Design based quite closely on previously coded class
//  'LmpBidSet'.

//  LOCAL AND SYSTEM INCLUDES

#include "../a/logger_fwd.h"  // some forward declarations from "logger.h"
#include "../c/smart_ptr.h"   // toggle between Boost and TR1 smart pointers

#include <string>             // C++ strings
#include <sstream>            // string-streams
#include <utility>            // STL pair, make_pair()
#include <vector>             // STL sequence container

#include <boost/tuple/tuple.hpp>                  // n-tuples, ref(), cref()

//  CODE

// ---------------------------------------------------------
//  CLASS           : BandedTariffSet
// ---------------------------------------------------------
//  Description  : contract-to-supply tariff set abstraction
//  Role         : used to prepare, hold, and unpack a variety of banded tariff sets
//  Techniques   : stand-alone class, 'std::pair', 'std::accumulation'
//  Status       : complete
//
//  Design notes
//
//      The actual tariffs themselves could have been implemented
//      as a stand-alone (nested) class, but are instead
//      represented by the private typedef 'BandedTariffSet::tariff_type'.
//
//      This class maintains two tariff set registers, one
//      poppable and the other not.  In particular:
//
//          'BandedTariffSet::clear'       empties both
//
//          'BandedTariffSet::pushString'  pushes both
//          'BandedTariffSet::pushTariff'  pushes both
//
//          'BandedTariffSet::popFirst'    pops poppable
//          'BandedTariffSet::popLast'     pops poppable
//
//          'BandedTariffSet::empty'       tests poppable size
//          'BandedTariffSet::unfilled'    tests unpoppable size and fixed charge
//
//          'BandedTariffSet::size'        returns poppable size
//          'BandedTariffSet::bands'       returns unpoppable size
//
//      If not otherwise stated, accessors offer unpoppable
//      (fixed) information.  'BandedTariffSet::size' is the only
//      exception.
//
//      In hindsight, a single labeled register may have resulted
//      in a better design.
//
// ---------------------------------------------------------

class BandedTariffSet
{
  // TYPEDEFS

private:

  typedef std::pair<double, double> tariff_type;  // namely (band, price)

  // CREATORS

public:

  BandedTariffSet
  (std::string label = "");                  // optional label

  ~BandedTariffSet();

  // CAUTION: the old tariffset label is overwritten by the new label
  BandedTariffSet(const BandedTariffSet& orig);              // copy constructor
  BandedTariffSet& operator= (const BandedTariffSet& orig);  // copy assignment operator

  // MANIPULATORS

  void
  clear();                                   // remove tariffs and zero the fixed charge

  std::string                                // return old label
  setLabel                                   // set new label
  (const std::string& label = "");

  int                                        // number of tariffs successfully loaded
  pushString                                 // parses input, uses 'pushTariff' to load
  (const std::string sBandedTariffSet);

  int                                        // number of pushed tariffs or zero on fail
  pushTariff                                 // order of insertion significant
  (const tariff_type tariff);

  void
  setSpecificFixedCharge                     // in [CUR/Ws]
  (const double specFixedCharge);

  tariff_type
  popLast();                                 // pop newest tariff

  tariff_type
  popFirst();                                // pop oldest tariff

  bool                                       // 'true' indicates the 'capacity' binds
  truncate
  (const double capacity);

  // ACCESSORS

  // Clients can also access the band and price values using the
  // 'std::pair' 'first' and 'second' fields -- for example
  //
  //    double highestUnitPrice = getHighestTariff().second;

  std::string
  getLabel() const;

  double
  getSpecificFixedCharge() const;            // return fixed

  tariff_type
  getFirstOnTariff() const;

  tariff_type
  getLastOnTariff() const;

  tariff_type
  getLowestTariff() const;                   // lowest defined in terms of price

  tariff_type
  getHighestTariff() const;                  // highest defined in terms of price

  double
  getBandSum() const;                        // the contractual "capacity"

  double                                     // could be infinity
  getCapacity() const;

  bool                                       // 'true' indicates non-convex, but excluding
  isNonConvex() const;                       // .. any consideration of the fixed charge

  bool                                       // 'true' indicates not non-convex
  isConvex() const;                          // wrapper to ( isNonConvex == false )

  bool
  empty() const;                             // includes popping

  bool
  unfilled() const;                          // default fixed charge and no tariff data

  int                                        // current number of nontrivial bands
  size() const;                              // includes popping

  int                                        // current number of nontrivial bands
  bands() const;                             // excludes popping

  std::string
  summarizeAll() const;                      // for unit testing or yeek reporting

  double                                     // marginal price ($/quantity)
  getMarginalPrice
  (const double sale) const;                 // transaction (quantity)

  boost::tuple                               // see 'OfrTariffSet::results_type' typedef
  <double,                                   // marginal price ($/quantity)
   double,                                   // fixed component ($)
   double,                                   // total price ($)
   double>                                   // transaction (quantity)
  interpretSale
  (const double size,
   const double sale,
   const int    interval) const;

  // INSTANCE DATA

private:

  std::string                 d_label;            // optional
  double                      d_fixed;            // defaults to zero
  std::vector<tariff_type>    d_tariffset_pop;    // insertion order maintained,
  std::vector<tariff_type>    d_tariffset_fix;    // insertion order maintained
  double                      d_capacity;         // additional constraint

  // STATIC DATA

private:

  static logga::spLogger    s_logger;        // shared_ptr to single logger object

};

#endif // _BANDTAF_H_

//  end of file

