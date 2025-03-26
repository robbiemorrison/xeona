//  XEONA ENERGY SYSTEMS MODELING ENVIRONMENT
//
//  file-create-name : lmpbid.ut0.cc
//  file-create-date : Fri 17-Oct-2008 12:54 UTC
//  file-initiator   : Robbie Morrison <robbie@actrix.co.nz>
//
//  file-role        : LMP auction bidset and support / unit test
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
//  $URL: file:///home/robbie/svn-root/xeona/futz/trunk/xeona1/b/lmpbid.ut0.cc $
//
//  GENERAL NOTES FOR THIS FILE
//
//  This is a unit test file and hence not essential to the
//  application proper.

//  LOCAL AND SYSTEM INCLUDES

#include "lmpbid.h"           // unit under test (place early)

#include "../a/logger.h"      // standard logging functionality (as required)
#include "../c/smart_ptr.h"   // toggle between Boost and TR1 smart pointers
#include ".././common.h"      // common definitions for project (place last)

#include <string>             // C++ strings
#include <sstream>            // string-streams

#include <utility>            // STL pair, make_pair()

#include <boost/foreach.hpp>  // BOOST_FOREACH iteration macro

//  CODE

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

  std::ostringstream put;

  // ---------------------------------------------------------
  //  test ONE        : create a bidset
  // ---------------------------------------------------------

  logger->test(1, "create a bidset");

  {
    LmpBidSet bids("test-1");
  }

  // ---------------------------------------------------------
  //  test TWO        : fill, report, and pop a bidset
  // ---------------------------------------------------------

  logger->test(2, "fill, report, and pop a bidset (empty bidset warning expected)");

  {
    LmpBidSet bids("test-2");

    std::string bidStr;
    bidStr = "40.00e+06 28.00e-09 * 30.00e+06 12.00e-09 * 20.00e+06 12.00e-09";
    put << "  " << "bid string : " << bidStr << "\n";
    logger->putx(logga::dbug, put);
    logger->addSmartBlank(logga::dbug);

    bids.pushString(bidStr);
    bids.pushString("");

    const int indent = 2;
    put << bids.summarizeAll(indent);          // trailing newline not required
    logger->putx(logga::dbug, put);
    logger->addSmartBlank(logga::dbug);

    put << "  " << "hi quantity : " << bids.getHiQuantity() << "\n";
    logger->putx(logga::dbug, put);
    logger->addSmartBlank(logga::dbug);

    std::pair<double, double> bid1 = bids.popBig();
    put << "  " << "quantity : " << bid1.first  << "\n"
        << "  " << "price    : " << bid1.second << "\n";
    logger->putx(logga::dbug, put);
    logger->addSmartBlank(logga::dbug);

    std::pair<double, double> bid2 = bids.popSmall();
    put << "  " << "quantity : " << bid2.first  << "\n"
        << "  " << "price    : " << bid2.second << "\n";
    logger->putx(logga::dbug, put);
    logger->addSmartBlank(logga::dbug);

    put << "  " << "hi quantity : " << bids.getHiQuantity() << "\n";
    logger->putx(logga::dbug, put);
  }

  // ---------------------------------------------------------
  //  test THREE      : make, double, and stringify
  // ---------------------------------------------------------

  logger->test(3, "make, double, and stringify a bidset");

  {
    LmpBidSet bids("test-3");

    std::string bidStr;
    bidStr = "40.00e+06 10.00e-09 * 30.00e+06 11.00e-09 * 20.00e+06 12.00e-09";
    put << "  " << "bid string in    : " << bidStr << "\n";
    logger->putx(logga::dbug, put);
    logger->addSmartBlank(logga::dbug);

    bids.pushString(bidStr);

    put << "  " << "bid string out           : " << bids.stringify() <<"\n";
    logger->putx(logga::dbug, put);
    bids *= 2.0;
    put << "  " << "bid string out * 2       : " << bids.stringify() <<"\n";
    bids += 0.5e-08;
    put << "  " << "bid string out + 0.5e-08 : " << bids.stringify() <<"\n";
    logger->putx(logga::dbug, put);
  }

  // ---------------------------------------------------------
  //  test FOUR       : weighted price
  // ---------------------------------------------------------

  logger->test(4, "weighted price");

  {
    LmpBidSet bids("test-4");

    std::string bidStr;
    bidStr = "40.00e+06 10.00e-09 * 20.00e+06 20.00e-09";
    put << "  " << "bid string in             : " << bidStr                  << "\n";
    logger->putx(logga::dbug, put);

    bids.pushString(bidStr);

    put << "  " << "weighted price (13.3e-09) : " << bids.getWeightedPrice() <<"\n";
    logger->putx(logga::dbug, put);
  }

  // ---------------------------------------------------------
  //  test FIVE       : push bidset
  // ---------------------------------------------------------

  // note that 'std::make_pair' required <utility>

  logger->test(5, "push bidset");

  {
    const double band  = 40.0e+06;           // capacity band [*/s], where * = J
    const double price = 20.0e-09;           // unit price [$/*]

    LmpBidSet bids("test-5");
    bids.pushBid(std::make_pair(1.0 * band, 1.0 * price));
    bids.pushBid(std::make_pair(0.5 * band, 2.0 * price));

    put << "  " << "bid string out           : " << bids.summarizeAll(2);
    logger->putx(logga::dbug, put);
  }

  // ---------------------------------------------------------
  //  test SIX        : copy construction
  // ---------------------------------------------------------

  logger->test(6, "copy construction");

  {
    shared_ptr<LmpBidSet> bidsA(new LmpBidSet("test-6"));

    const std::string bidStr = "40.00e+06 10.00e-09 * 20.00e+06 20.00e-09";
    bidsA->pushString(bidStr);

    shared_ptr<LmpBidSet> bidsB(new LmpBidSet(*bidsA));
  }

  // ---------------------------------------------------------
  //  test SEVEN      : copy assignment
  // ---------------------------------------------------------

  logger->test(7, "copy assignment");

  {
    shared_ptr<LmpBidSet> bidsA(new LmpBidSet("test-7a"));

    const std::string bidStr = "40.00e+06 10.00e-09 * 20.00e+06 20.00e-09";
    bidsA->pushString(bidStr);

    shared_ptr<LmpBidSet> bidsB(new LmpBidSet("test-7b"));  // [1]
    *bidsB = *bidsA;                                        // [2]

    // [1] must allocate memory
    // [2] must dereference
  }

  // ---------------------------------------------------------
  //  test EIGHT      : equality tests (can differ by label)
  // ---------------------------------------------------------

  logger->test(8, "equality tests");

  {
    shared_ptr<LmpBidSet> bidsA(new LmpBidSet("test-8a"));

    const std::string bidStr = "40.00e+06 10.00e-09 * 20.00e+06 20.00e-09";
    bidsA->pushString(bidStr);

    shared_ptr<LmpBidSet> bidsB(new LmpBidSet("test-8b"));
    *bidsB = *bidsA;                         // overwrites label

    put << "  " << "bidsA label : " << bidsA->label() << "\n";
    put << "  " << "bidsB label : " << bidsB->label() << "\n";
    put << "\n";

    if ( *bidsA == *bidsB )
      {
        put << "  " << "bids considered equal"     << "\n";
      }
    else
      {
        put << "  " << "bids not considered equal" << "\n";

      }
    logger->putx(logga::dbug, put);
  }

  // ---------------------------------------------------------
  //  test NINE       : relabel
  // ---------------------------------------------------------

  logger->test(9, "relabel");

  {
    shared_ptr<LmpBidSet> bids(new LmpBidSet("test-9-old"));

    const std::string bidStr = "40.00e+06 10.00e-09 * 20.00e+06 20.00e-09";
    bids->pushString(bidStr);

    const std::string oldLab = bids->relabel("test-9-new");
    const std::string newLab = bids->label();

    put << "  " << "old label : " << oldLab << "\n";
    put << "  " << "new label : " << newLab << "\n";
    logger->putx(logga::dbug, put);
  }

  // ---------------------------------------------------------
  //  test TEN        : stringify several bidsets
  // ---------------------------------------------------------

  logger->test(10, "stringify several bidsets");

  {
    shared_ptr<LmpBidSet> bidsA(new LmpBidSet("test-10-common"));

    const std::string bidStr = "40.00e+06 10.00e-09 * 20.00e+06 20.00e-09";
    bidsA->pushString(bidStr);

    // copy construct two new bidsets
    shared_ptr<LmpBidSet> bidsB(new LmpBidSet(*bidsA));
    shared_ptr<LmpBidSet> bidsC(new LmpBidSet(*bidsA));

    std::vector<shared_ptr<LmpBidSet> > bidsets;

    bidsets.push_back(bidsA);
    bidsets.push_back(bidsB);
    bidsets.push_back(bidsC);

    int count = 0;
    BOOST_FOREACH( shared_ptr<LmpBidSet> bs, bidsets )
      {
        std::ostringstream oss;
        oss << "test-10-" << std::setw(2) << std::setfill('0') << ++count;
        bs->relabel(oss.str());
        *bs *= static_cast<double>(count);   // count from 1 .. 3
      }

    put << "  " << "stringifaction : " << xeona::stringifyBidSets(bidsets) << "\n";
    logger->putx(logga::dbug, put);
  }

  // ---------------------------------------------------------
  //  test ELEVEN     : price hike with operator*=
  // ---------------------------------------------------------

  logger->test(11, "price hike with operator*=");

  {
    shared_ptr<LmpBidSet> bids(new LmpBidSet("test-11"));

    const std::string bidStr = "40.00e+06 10.00e-09 * 20.00e+06 20.00e-09";
    bids->pushString(bidStr);

    const double hike = 8.0;
    put << "  " << "hike     : " << hike << " x" << "\n";

    put << "  " << "original : " << bids->stringify() << "\n";
    *bids *= hike;
    put << "  " << "modified : " << bids->stringify() << "\n";

    logger->putx(logga::dbug, put);
  }

  // ---------------------------------------------------------
  //  test TWELVE     : rescaling on factor
  // ---------------------------------------------------------

  logger->test(12, "rescaling on factor");

  {
    shared_ptr<LmpBidSet> bids(new LmpBidSet("test-12"));

    const std::string bidStr = "40.00e+06 10.00e-09 * 20.00e+06 20.00e-09";
    bids->pushString(bidStr);

    const double factor  = 2.4;
    const double wp1     = bids->getWeightedPrice();
    bids->rescalePrices(factor);
    const double wp2     = bids->getWeightedPrice();

    put << "  " << "original weighted : " << wp1 << "\n"
        << "  " << "modified weighted : " << wp2 << "\n";
    put << "\n";
    put << "  " << "factor     : " << factor  << "\n"
        << "  " << "calculated : " << wp2/wp1 << "\n";

    logger->putx(logga::dbug, put);
  }

  // ---------------------------------------------------------
  //  test THIRTEEN   : rescaling on weighted price
  // ---------------------------------------------------------

  logger->test(13, "rescaling on weighted price");

  {
    shared_ptr<LmpBidSet> bids(new LmpBidSet("test-13"));

    const std::string bidStr = "40.00e+06 10.00e-09 * 20.00e+06 20.00e-09";
    bids->pushString(bidStr);

    const double factor  = 1.4;
    const double wp1     = bids->getWeightedPrice();
    const double rescale = bids->rescaleWeightedPrice(wp1 * factor);
    const double wp2     = bids->getWeightedPrice();

    put << "  " << "original weighted : " << wp1 << "\n"
        << "  " << "modified weighted : " << wp2 << "\n";
    put << "\n";
    put << "  " << "factor     : " << factor  << "\n"
        << "  " << "rescale    : " << rescale << "\n"
        << "  " << "calculated : " << wp2/wp1 << "\n";

    logger->putx(logga::dbug, put);
  }

  // ---------------------------------------------------------
  //  test FOURTEEN   : would capacitate trials
  // ---------------------------------------------------------

  logger->test(14, "would capacitate trials");

  {
    shared_ptr<LmpBidSet> bids(new LmpBidSet("test-14"));

    const std::string bidStr = "40.00e+06 10.00e-09 * 20.00e+06 20.00e-09";
    bids->pushString(bidStr);

    const double hi = bids->getHiQuantity();

    double hiQuantity1 = 0.0;
    const double hi1   = hi;
    const bool would1  = bids->wouldCapacitate(hi1, hiQuantity1);

    put << "\n";                             // the floating-point test reports directly
    put << std::boolalpha
        << "  " << "hi1 (original)       : " << hi1         << "\n"
        << "  " << "hiQuantity1 (loaded) : " << hiQuantity1 << "\n"
        << "  " << "would capacitate *   : " << would1      << "\n";

    put << "\n";                             // the floating-point test reports directly
    logger->putx(logga::dbug, put);

    double hiQuantity2 = 0.0;
    const double hi2   = hi * (1.0 + 1.0e-6);     // 1.0e-7 is okay, e-06 would capacitate
    const bool would2  = bids->wouldCapacitate(hi2, hiQuantity2);

    put << "\n";                             // the floating-point test reports directly
    put << std::boolalpha
        << "  " << "hi2 (original)       : " << hi2         << "\n"
        << "  " << "hiQuantity2 (loaded) : " << hiQuantity2 << "\n"
        << "  " << "would capacitate *   : " << would2      << "\n";

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

