//  XEONA ENERGY SYSTEMS MODELING ENVIRONMENT
//
//  file-create-name : node.cc
//  file-create-date : Tue 03-Nov-2009 12:18 UTC
//  file-initiator   : Robbie Morrison <robbie@actrix.co.nz>
//
//  file-role        : LMP node entity / implementation
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
//  $URL: file:///home/robbie/svn-root/xeona/futz/trunk/xeona1/b/node.cc $
//
//  GENERAL NOTES FOR THIS FILE

//  LOCAL AND SYSTEM INCLUDES

#include "node.h"             // companion header for this file (place first)

#include "../c/stats.h"       // on-the-fly statistical calculations
#include "../c/recset.h"      // records and fields and also record-sets

#include "../a/logger.h"      // standard logging functionality (as required)
#include "../c/smart_ptr.h"   // toggle between Boost and TR1 smart pointers
#include ".././common.h"      // common definitions for project (place last)

#include <iomanip>            // setw() and family
#include <string>             // C++ strings
#include <sstream>            // string-streams

#include <boost/format.hpp>   // printf style formatting

//  CODE

// ---------------------------------------------------------
//  MEMBER FUNCTION : LmpNode
// ---------------------------------------------------------

LmpNode::LmpNode
(const std::string entityId,
 Record&           record) :
  Block(entityId, record),
  TicToc(xeona::e_auctionLmp),               // general: 'xeona::e_commitmentModes'
  d_nodalPrices(record.tieTimeseries<double>("nodal-prices"))
{
  s_logger->repx(logga::xtra, "constructor call", getIdAndKind());
}

// ---------------------------------------------------------
//  MEMBER FUNCTION : ~LmpNode
// ---------------------------------------------------------
//  Description  : LmpNode destructor
//  Role         : contains troubleshooting code
//  Techniques   : Statistics functor
//  Status       : complete
//
//  Design notes
//
//      The reporting code is placed here and not in
//      'LmpNode::conclude' because the latter is not called when
//      the solver chokes.
//
// ---------------------------------------------------------

LmpNode::~LmpNode()
{
  s_logger->repx(logga::adhc, "destructor call", getIdAndKind());

  // initial reporting
  s_logger->repx(logga::adhc, "entering member function", "");

  // set some trip values -- modify as required
  const double priceCap   =   50.0;          // adjusting natural gas at 0.5 USD/kg by 100
  const double priceError = 1000.0;          // highly unlikely for even kg-quantified

  // statistics
  Statistics<double> statPrices;             // nodal price statistics
  statPrices(d_nodalPrices);
  const double count      = statPrices.count();
  const double minPrice   = statPrices.min();
  const double maxPrice   = statPrices.max();
  const double meanPrice  = statPrices.mean();
  const double rangePrice = statPrices.range();

  // cautions and warnings
  if ( minPrice < 0.0 )
    {
      const std::string minprice = boost::str(boost::format("%g") % minPrice);
      s_logger->repx(logga::warn, "negative minimum nodal price", minprice);
    }
  if ( maxPrice > priceError )
    {
      const std::string maxprice = boost::str(boost::format("%g") % maxPrice);
      s_logger->repx(logga::warn, "extremely high maximum nodal price", maxprice);
    }
  else if ( maxPrice > priceCap )
    {
      const std::string maxprice = boost::str(boost::format("%g") % maxPrice);
      s_logger->repx(logga::rankJumpy, "rather high maximum nodal price", maxprice);
    }

  // additional reporting as appropriate
  // YEEK 53 CODE (set by '--yeek')
  if ( xeona::yeek == 53 || xeona::yeek == 1 || xeona::yeek == 2 )
    {
      const std::string func = XEONA_FUNC;   // function name
      std::ostringstream put;
      put << "  node report"                                       << "\n"
          << "    identifier        : " << getIdAndKind()          << "\n"
          << "    function          : " << func                    << "\n"
          << "    count             : " << count                   << "\n"
          << std::showpoint << std::setprecision(3)
          << "    minimum LMP price : " << minPrice                << "\n"
          << "    maximum LMP price : " << maxPrice                << "\n"
          << "    mean LMP price    : " << meanPrice               << "\n"
          << "    LMP price range   : " << rangePrice              << "\n";
      s_logger->repx(logga::dbug, "additional reporting follows, yeek", xeona::yeek);
      s_logger->putx(logga::dbug, put);
    }
}

// ---------------------------------------------------------
//  MEMBER FUNCTION : LmpNode::conclude
// ---------------------------------------------------------
//  Description  : standard but optional virtual function
//  Role         : end-of-horizon code
//  Techniques   : class 'Statistics'
//  Status       : complete
//
//  Design notes
//
//      Negative price code placed here because destructor
//      warning do not affect the '--exittrip'
//
// ---------------------------------------------------------

void
LmpNode::conclude()
{
  // initial reporting
  s_logger->repx(logga::adhc, "entering member function", getIdAndKind());

  // statistics
  Statistics<double> statPrices;             // nodal price statistics
  statPrices(d_nodalPrices);
  const double minPrice = statPrices.min();

  // cautions and warnings
  if ( minPrice < 0.0 )
    {
      const std::string minprice = boost::str(boost::format("%g") % minPrice);
      s_logger->repx(logga::warn, "negative minimum nodal price", minprice);
    }
}

// ---------------------------------------------------------
//  MEMBER FUNCTION : LmpNodeAc
// ---------------------------------------------------------

LmpNodeAc::LmpNodeAc
(const std::string entityId,
 Record&           record) :
  LmpNode(entityId, record),
  d_voltageAngles(record.tieTimeseries<double>("voltage-angles"))
{
  s_logger->repx(logga::xtra, "constructor call", getIdAndKind());
}

// ---------------------------------------------------------
//  MEMBER FUNCTION : ~LmpNodeAc
// ---------------------------------------------------------
//  Description  : LmpNodeAc destructor
//  Role         : contains troubleshooting code
//  Techniques   : Statistics functor
//  Status       : complete
//
//  Design notes
//
//      The reporting code is placed here and not in
//      'LmpNodeAc::conclude' because the latter is not called
//      when the solver chokes.
//
//      In addition, this class inherits from class 'LmpNode' and
//      YEEK 53 code is contained there also.
//
// ---------------------------------------------------------

LmpNodeAc::~LmpNodeAc()
{
  s_logger->repx(logga::adhc, "destructor call", getIdAndKind());

  // additional reporting as appropriate
  // YEEK 53 CODE (set by '--yeek')
  if ( xeona::yeek == 53 || xeona::yeek == 1 || xeona::yeek == 2 )
    {
      const std::string func = XEONA_FUNC;   // function name
      const double voltageAngle = d_voltageAngles->at(d_step);

      std::ostringstream put;
      put << "  AC node report"                                    << "\n"
          << "    identifier        : " << getIdAndKind()          << "\n"
          << "    function          : " << func                    << "\n"
          << "    voltage angle [d] : " << voltageAngle            << "\n";
      s_logger->repx(logga::dbug, "additional reporting follows, yeek", xeona::yeek);
      s_logger->putx(logga::dbug, put);
    }

}

//  end of file

