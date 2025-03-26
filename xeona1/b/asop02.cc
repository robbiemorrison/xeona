//  XEONA ENERGY SYSTEMS MODELING ENVIRONMENT
//
//  file-create-name : asop02.cc
//  file-create-date : Thu 09-Jul-2009 15:44 UTC
//  file-initiator   : Robbie Morrison <robbie@actrix.co.nz>
//
//  file-role        : concrete asset operators 2 / implementation
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
//  $URL: file:///home/robbie/svn-root/xeona/futz/trunk/xeona1/b/asop02.cc $
//
//  GENERAL NOTES FOR THIS FILE

//  LOCAL AND SYSTEM INCLUDES

#include "asop02.h"           // companion header for this file (place first)

#include "../c/tsset.h"       // timeseries classes for added functionality
#include "../c/tsops.h"       // overloaded operators and similar for timeseries
#include "../c/stats.h"       // on-the-fly statistical calculations
#include "../c/recset.h"      // records and fields and also record-sets
#include "../c/label.h"       // helper class to format solver labels
#include "../c/hydro.h"       // hydro asset to operator data transfer
#include "../b/teas07.h"      // concrete technical assets 7
#include "../b/teas.h"        // technical asset entity
#include "../b/optctl.h"      // control optimization sub-problems for asset operators
#include "../b/node.h"        // LMP node entity
#include "../b/lmpbid.h"      // LMP auction bidset
#include "../b/entity.h"      // entity base class plus lazy linking
#include "../a/exent.h"       // entity exception classes

#include "../a/logger.h"      // standard logging functionality (as required)
#include ".././common.h"      // common definitions for project (place last)

#include <iomanip>            // setw() and family
#include <iostream>           // standard io
#include <sstream>            // string-streams
#include <string>             // C++ strings
#include <utility>            // STL pair, make_pair()

#include <boost/algorithm/string.hpp>   // string recasing, trimming, splitting
#include <boost/foreach.hpp>            // BOOST_FOREACH iteration macro
#include <boost/lexical_cast.hpp>       // lexical_cast<> string to number conversions

//  CODE

// ---------------------------------------------------------
//  FREE FUNCTION   : xeona::obtainBidsetDialog
// ---------------------------------------------------------

namespace xeona
{
  std::string
  obtainBidsetDialog
  (const double       capacity,              // current capacity
   const std::string& intro)                 // optional intro text with trailing newline
  {
    // initial reporting
    static logga::spLogger logger = logga::ptrLogStream();
    logger->repx(logga::dbug, "entering free function, capacity", capacity);
    logger->addDumbBlank();

    // user-defined constants
    const std::string rule  = "-----\n";     // used to demark this function
    const double scaleBand  = 1.0e+06;       // band scale factor
    const double scalePrice = 1.0e-09;       // price scale factor

    // bid delimiter
    const std::string bd    = xeona::modelBidDelim;

    // declare the read buffers
    std::string bufBand;                     // capacity band buffer
    std::string bufPrice;                    // unit price buffer

    // scaled capacity as string
    std::ostringstream tmp;
    tmp << capacity / scaleBand;
    const std::string cap = tmp.str();

    // create standard messages
    std::ostringstream msg;
    msg << "create a bidset by entering multiple space-separated band/price pairs"
        << ", finish early with Ctrl-D if required"                 << "\n"
        << "order not significant, scientific notation supported"
        << ", \"cap\" as alias for current capacity : " << capacity << "\n"
        << "\n";
    const std::string msgstr = msg.str();
    std::ostringstream ask;
    ask << "enter band (" << scaleBand << ") and price (" << scalePrice << ")";
    const std::string askstr = ask.str();

    // terminal output starts
    std::cerr << rule;
    if ( ! intro.empty() ) std::cerr << intro << "\n";
    std::cerr << msgstr;
    std::cerr << std::flush;

    // bidset string-stream
    std::ostringstream bidset;

    // main loop
    double totalBand = 0.0;                  // running total
    while ( true )
      {
        // input request
        std::cerr << "  " << askstr << " : ";
        std::cin  >> bufBand >> bufPrice;    // two buffers

        // abandon on ctrl-d
        if ( ! std::cin )                    // meaning ctrl-d received
          {
            std::cerr << "complete" << "\n"; // newline also required
            std::cerr << "\n";
            std::cerr << std::flush;
            break;
          }

        // capacity swap on "cap"
        if ( bufBand == "cap" )              // meaning "cap" entered
          {
            bufBand = cap;
          }

        // cast and reject if necessary
        double band  = -1.0;
        double price = -1.0;
        try
          {
            band  = boost::lexical_cast<double>(bufBand);
            price = boost::lexical_cast<double>(bufPrice);
          }
        catch( const boost::bad_lexical_cast& e )
          {
            std::cerr << "  invalid entry rejected" << "\n";
            std::cerr << std::flush;
            continue;
          }

        // check if negative and reject
        if ( band < 0.0 || price < 0.0 )
          {
            std::cerr << "  negative entry rejected" << "\n";
            std::cerr << std::flush;
            continue;
          }

        // rescale
        band  *= scaleBand;
        price *= scalePrice;

        // incrementally construct the bidset
        if ( ! bidset.str().empty() )        // meaning prior bids exist
          {
            bidset << " " << bd << " ";
          }
        bidset << band << " " << price;

        // break on sufficient capacity
        totalBand += band;
        if ( totalBand >= capacity )
          {
            std::cerr << "\n" << "break on sufficient capacity : " << totalBand << "\n";
            std::cerr << std::flush;
            break;
          }

      } // while true block

    // rework an empty bidset
    std::string bidsetstr = bidset.str();
    if ( bidsetstr.empty() )
      {
        bidsetstr = "0.0 0.0";
      }

    //  terminal output completes
    std::cerr << "capacity ratio : " << totalBand / capacity << "\n"; // div-zero is 'nan'
    std::cerr << "bidset string  : " << "\"" << bidsetstr << "\"" << "\n";
    std::cerr << rule;
    std::cerr << std::flush;

    logger->addSmartBlank();

    return bidsetstr;
  }

} // namespace 'xeona'

// ---------------------------------------------------------
//  FREE FUNCTION   : ::splitBidsetsString
// ---------------------------------------------------------
//  Description  : splits single string of +-separated bidsets into vector of 'LmpBidSet's
//  Role         : used by 'AsopLmpBidParam'
//  Techniques   : 'boost::algorithm::split' string split function
//  Status       : complete
// ---------------------------------------------------------

namespace
{
  int                                        // bidsets count
  splitBidsetsString
  (const std::string&                   bidsetsString,
   std::vector<shared_ptr<LmpBidSet> >& bidsets,
   const std::string                    bidsetSep)     // usually "+"
  {
    // bind logger and report
    static logga::spLogger logger = logga::ptrLogStream();  // local usage
    logger->repx(logga::adhc, "entering free function, separator", bidsetSep);

    // require that 'bidsets' be empty
    if ( ! bidsets.empty() )
      {
        const int elements = bidsets.size();
        logger->repx(logga::warn, "bidsets vector not empty as needed", elements);
        bidsets.clear();                     // remove all elements
      }

    // split into bidset strings
    std::vector<std::string> vecBidsets;
    boost::algorithm::split
      (vecBidsets,
       bidsetsString,
       boost::algorithm::is_any_of(bidsetSep),    // bidset separator
       boost::algorithm::token_compress_off);     // no adjacent token merging

    // unpack 'vecBidsets' and add label "bidset 00"
    int count = 0;
    BOOST_FOREACH( std::string s, vecBidsets )
      {
        const std::string label = boost::str(boost::format("bidset %02d") % ++count);
        shared_ptr<LmpBidSet> bidset(new LmpBidSet(label));
        bidset->pushString(s);               // create from string
        bidsets.push_back(bidset);           // fill the container
      }

    // return number of bidsets
    const int bidsetCount = bidsets.size();
    logger->repx(logga::adhc, "entering free function, bidsets cnt", bidsetCount);
    return bidsetCount;

  } // function '::splitBidsetsString'
} // unnamed namespace

// ---------------------------------------------------------
//  CLASS           : AsopGrid
// ---------------------------------------------------------

// ---------------------------------------------------------
//  MEMBER FUNCTION : AsopGrid
// ---------------------------------------------------------

AsopGrid::AsopGrid
(const std::string entityId,
 Record&           record) :
  CostRegister(record),
  AssetOperator(entityId, record, xeona::e_auctionLmp),
  d_lmp_nodes(record.tieSingle<std::string>("lmp-nodes")),
  d_voltageAngleReference(record.tieSingle<double>("voltage-angle-reference"))
{
  s_logger->repx(logga::xtra, "constructor call", getIdAndKind());
  d_builtinRemark = "beta";
}

// ---------------------------------------------------------
//  MEMBER FUNCTION : ~AsopGrid
// ---------------------------------------------------------

AsopGrid::~AsopGrid()
{
  s_logger->repx(logga::adhc, "destructor call", getIdAndKind());
}

// DOMAIN CONTROLLER POINTS OF ENTRY

// ---------------------------------------------------------
//  MEMBER FUNCTION : getLmpNodes
// ---------------------------------------------------------

std::vector<shared_ptr<LmpNode> >
AsopGrid::getLmpNodes()
{
  s_logger->repx(logga::adhc, "entering member function", "");
  d_lmpNodes.clear();
  FullEntity::listToVec<LmpNode>(d_lmp_nodes, d_lmpNodes);
  return d_lmpNodes;
}

// ---------------------------------------------------------
//  MEMBER FUNCTION : establish
// ---------------------------------------------------------

void
AsopGrid::establish()
{
  // initial reporting
  s_logger->repx(logga::dbug, "entering member function", "AsopGrid");

  // call the base function for the underlying work
  AssetOperator::establish();

  // load LMP nodes and report (will shortly be undertaken for
  // technical assets by class 'AssetOperator')
  d_lmpNodes.clear();
  const int lmpNodeCnt = FullEntity::listToVec<LmpNode>(d_lmp_nodes, d_lmpNodes);
  s_logger->repx(logga::adhc, "lmp node count", lmpNodeCnt);
}

// ---------------------------------------------------------
//  MEMBER FUNCTION : constrain
// ---------------------------------------------------------
//  Description  : constrain call
//  Role         : standard call
//  Techniques   : duty coupling
//  Status       : complete
// ---------------------------------------------------------

const int                          // number of technical assets and LMP nodes processed
AsopGrid::constrain
(const xeona::DomainMode capacityMode)
{
  // initial reporting
  s_logger->repx(logga::adhc, "entering member function", "AsopGrid");

  // preamble
  const std::string asopId = getIdentifier();

  // declare some administration counters
  int teasLoops = 0;                         // number of technical assets processed
  int nodeLoops = 0;                         // number of LMP nodes processed

  // step thru technical assets in no required order
  BOOST_FOREACH( shared_ptr<TechnicalAsset> ta, d_technicalAssets )
    {
      // keep count
      ++teasLoops;
      xeona::putxId(ta, "AsopGrid::constrain technical asset loop");

      // constrain the associated technical asset
      const int ret = ta->constrain(capacityMode);

      // CAUTION: transmission assets are not bid coupled and
      // hence return zero in order to "stay out of the loop" --
      // complain if meaningful coupling is indicated
      if ( ret != 0 )
        {
          s_logger->repx(logga::warn, "meaningful coupling encountered", ret);
        }

    } // technical assets loop

  // step thru LMP nodes in no required order
  BOOST_FOREACH( shared_ptr<LmpNode> ln, d_lmpNodes )
    {
      // keep count
      ++nodeLoops;
      xeona::putxId(ln, "AsopGrid::constrain LMP node loop");

      // constrain the associated node
      const int ret = ln->constrain(capacityMode);

      // CAUTION: LMP nodes are not bid coupled and hence return
      // zero in order to "stay out of the loop" -- complain if
      // meaningful coupling is indicated
      if ( ret != 0 )
        {
          s_logger->repx(logga::warn, "meaningful coupling encountered", ret);
        }

    } // LMP nodes loop

  // set AC reference voltage angle if nodes exists and inherits
  // from class 'LmpNodeAc' -- and then simply use the first
  if ( ! d_lmpNodes.empty() )
    {
      shared_ptr<LmpNode>   first    = d_lmpNodes.front();
      shared_ptr<LmpNodeAc> downcast = dynamic_pointer_cast<LmpNodeAc>(first);
      if ( downcast )
        {
          downcast->fixTheta(d_voltageAngleReference);
          s_logger->repx(logga::dbug,
                         "just fixed the voltage angle theta",
                         d_voltageAngleReference);
        }
    }

  // return combined count
  return teasLoops + nodeLoops;

} // function 'AsopGrid::constrain'

// ---------------------------------------------------------
//  CLASS           : AsopLmpBidStatedTs1
// ---------------------------------------------------------

// ---------------------------------------------------------
//  MEMBER FUNCTION : AsopLmpBidStatedTs1
// ---------------------------------------------------------

AsopLmpBidStatedTs1::AsopLmpBidStatedTs1
(const std::string entityId,
 Record&           record) :
  CostRegister(record),
  AssetOperator(entityId,
                record,
                xeona::e_auctionLmp),        // commitment mode sum
  CostRegisterAsop(record),
  d_market_side(record.tieSingle<std::string>("market-side")),
  d_bidsets_1(record.tieTimeseries<std::string>("bidsets-1")),
  d_marketSide(xeona::e_notSpecified),
  d_bidsets1(),
  d_ctl(),
  d_ctls()
{
  s_logger->repx(logga::xtra, "constructor call", getIdAndKind());
  d_builtinRemark = "beta";

  // record the market side or else complain about given 'd_market_side' value
  if      ( d_market_side == "demand" ) d_marketSide = xeona::e_demandSide;
  else if ( d_market_side == "supply" ) d_marketSide = xeona::e_supplySide;
  if ( d_marketSide == xeona::e_notSpecified )
    {
      s_logger->repx(logga::warn, "market side string not valid", d_market_side);
    }

  // load bidset vector at outset
  BOOST_FOREACH( std::string s, *d_bidsets_1 )
    {
      shared_ptr<LmpBidSet> bidset(new LmpBidSet("AsopLmpBidStatedTs1"));
      if ( bidset->pushString(s) == 0 )
        {
          s_logger->repx(logga::warn, "bidset string parse issue, input", s);
        }
      if ( d_marketSide == xeona::e_demandSide )
        {
          bidset->negate();                  // CAUTION: see code in unit 'b/lmpbid'
        }
      d_bidsets1.push_back(bidset);

      // additional reporting as appropriate
      // YEEK 5 CODE (set by '--yeek')
      if ( xeona::yeek == 5 || xeona::yeek == 1 || xeona::yeek == 2 )
        {
          std::ostringstream put;
          shared_ptr<LmpBidSet> temp = d_bidsets1.back();
          put << "  bidset, market side: " << d_market_side << "\n";
          put << temp->summarizeAll(2);
          s_logger->repx(logga::dbug, "additional reporting follows, yeek", xeona::yeek);
          s_logger->putx(logga::dbug, put);
        }
    }
}

// ---------------------------------------------------------
//  MEMBER FUNCTION : ~AsopLmpBidStatedTs1
// ---------------------------------------------------------

AsopLmpBidStatedTs1::~AsopLmpBidStatedTs1()
{
  s_logger->repx(logga::adhc, "destructor call", getIdAndKind());
}

// ---------------------------------------------------------
//  MEMBER FUNCTION : constrain
// ---------------------------------------------------------
//  Description  : constrain call
//  Role         : standard call
//  Techniques   : duty coupling
//  Status       : complete
// ---------------------------------------------------------

const int                                    // number of technical assets processed
AsopLmpBidStatedTs1::constrain
(const xeona::DomainMode capacityMode)
{
  // initial reporting
  s_logger->repx(logga::adhc, "entering member function", "AsopLmpBidStatedTs1");

  // preamble
  const std::string asopId = getIdentifier();

  // clear previous OSPs because each invocation produces a new set of OSPs
  d_ctls.clear();                            // strictly necessary

  // declare some administration counters
  int teasLoops = 0;                         // number of technical assets processed

  // step thru technical assets in no required order
  BOOST_FOREACH( shared_ptr<TechnicalAsset> ta, d_technicalAssets )
    {
      // keep count
      ++teasLoops;
      xeona::putxId(ta, "AsopLmpBidStatedTs1::constrain technical asset loop");

      // useful messaging data
      const std::string aoid = getIdAndKind();
      const std::string taid = ta->getIdAndKind();

      // create and fill a label object
      Label lab(asopId);

      // constrain the associated technical asset (also set floor and ceiling duties)
      const int opsDutyGol = ta->constrain(capacityMode);

      // check for non-meaningful coupling (transmissions assets)
      // then complain and loop again
      if ( opsDutyGol == 0 )
        {
          s_logger->repx(logga::warn, "uncoupled asset encountered", taid);
          std::ostringstream put;
          put << "  LMP asset operator encountered technical asset that"
              << " does not require duty coupling"                         << "\n"
              << "    asset operator class  : " << "AsopLmpBidStatedTs1"   << "\n"
              << "    asset operator id     : " << aoid                    << "\n"
              << "    technical operator id : " << taid                    << "\n";
          s_logger->putx(logga::xtra, put);
          s_logger->repx(logga::adhc, "will abandon current loop, count", teasLoops);
          continue;
        }

      // recreate and label new control OSP of the required type
      d_ctl.reset(new CtlLmpBid(d_solver, d_commitmentMode));
      d_ctl->loadOspLabel(lab.str("lmp-stated-ts1"));

      // grab current bidset
      shared_ptr<LmpBidSet> bidset = d_bidsets1.at(d_step);

      // test that bidset
      if ( ! bidset )                        // empty bidset
        {
          s_logger->repx(logga::warn, "empty bidset encountered, loop", teasLoops);
          std::ostringstream put;
          put << "  unexpected empty bidset"                      << "\n"
              << "    bidset pointer     : " << bidset            << "\n"
              << "    step               : " << d_step            << "\n"
              << "    asset operator  id : " << aoid              << "\n"
              << "    technical asset id : " << taid              << "\n";
          s_logger->putx(logga::dbug, put);
          s_logger->repx(logga::adhc, "will abandon current loop, count", teasLoops);
          continue;
        }

      // obtain technical asset information
      const double ceilingDuty     = ta->getCeilingDuty();  // from 'TechnicalAsset'

      // check for asset capacitation through insufficient
      // bidding, using floating-point comparision
      double hiQuantity = 0.0;               // load thru pass-by-reference
      if ( bidset->wouldCapacitate(ceilingDuty, hiQuantity) )
        {
          std::ostringstream oss;
          oss << ceilingDuty << " : " << hiQuantity;
          s_logger->repx(logga::rankJumpy, "asset capacitation, asset : bid", oss.str());
        }

      // OSP upload bidset call
      int ctlDutyGol;
      boost::tie(ctlDutyGol) = d_ctl->uploadBidSet(bidset);

      // OSP couple call (from unit 'b/optprob')
      xeona::couple(d_solver,
                    opsDutyGol,
                    ctlDutyGol,
                    lab.str("couple"));

      // store some information
      d_ctls.push_back(d_ctl);

    } // technical assets loop

  // return
  return teasLoops;

} // function 'AsopLmpBidStatedTs1::constrain'

// ---------------------------------------------------------
//  MEMBER FUNCTION : AsopLmpBidStatedTs1::washup
// ---------------------------------------------------------

void
AsopLmpBidStatedTs1::washup()
{
  // initial reporting
  s_logger->repx(logga::dbug, "entering member function, step", d_step);

  // base class call
  AssetOperator::washup();

  // standing costs processing
  updateStandingCosts(d_step);
}

// ---------------------------------------------------------
//  CLASS           : AsopLmpBidParam
// ---------------------------------------------------------

// ---------------------------------------------------------
//  MEMBER FUNCTION : AsopLmpBidParam
// ---------------------------------------------------------

AsopLmpBidParam::AsopLmpBidParam
(const std::string entityId,
 Record&           record) :
  CostRegister(record),
  AssetOperator(entityId,
                record,
                xeona::e_auctionLmp),        // commitment mode sum
  CostRegisterAsop(record),
  d_market_side(record.tieSingle<std::string>("market-side")),
  d_bidset_list(record.tieSingle<std::string>("bidset-list")),
  d_bidPriceMultiplier(record.tieSingle<double>("bid-price-multiplier")),
  d_bidsetSelections(record.tieTimeseries<int>("bidset-selections")),
  d_submittedBidsets(record.tieTimeseries<std::string>("submitted-bidsets")),
  d_marketSide(xeona::e_notSpecified),
  d_bidsetZero(new LmpBidSet("bidset zero")),
  d_bidset(),
  d_bidsets(),
  d_ctl(),
  d_ctls()
{
  // initial reporting
  s_logger->repx(logga::xtra, "constructor call", getIdAndKind());
  d_builtinRemark = "beta";

  // define the bidset separator char (or string of chars)
  const std::string bidsetSep = xeona::modelBidSetSep;      // currently "/"
  s_logger->repx(logga::dbug, "using bidset separator", bidsetSep);

  // record the market side or else complain about given 'd_market_side' value
  if      ( d_market_side == "demand" ) d_marketSide = xeona::e_demandSide;
  else if ( d_market_side == "supply" ) d_marketSide = xeona::e_supplySide;
  if ( d_marketSide == xeona::e_notSpecified )
    {
      s_logger->repx(logga::warn, "market side string not valid", d_market_side);
    }

  // create zero bidset -- for when the bidset selection is also zero
  d_bidsetZero->pushString("0.0 0.0");

  // load given bidsets
  const int count = ::splitBidsetsString(d_bidset_list,     // +-separated string
                                         d_bidsets,         // must be empty
                                         bidsetSep);        // the '/' separator

  // "demand-side" these bidsets as required
  if ( d_marketSide == xeona::e_demandSide )
    {
      BOOST_FOREACH( shared_ptr<LmpBidSet> bs, d_bidsets )
        {
          bs->negate();                           // CAUTION: see code in unit 'b/lmpbid'
        }
      s_logger->repx(logga::adhc, "bidsets now 'demand-sided'", "");
    }

  // reprice these bidsets
  BOOST_FOREACH( shared_ptr<LmpBidSet> bs, d_bidsets )
    {
      bs->rescalePrices(d_bidPriceMultiplier);
    }
  s_logger->repx(logga::adhc, "bidsets repriced using", d_bidPriceMultiplier);

  // confirm the bidset selections
  Statistics<int> selstat(d_bidsetSelections);    // smart pointer to vector
  const int min = selstat.min();
  const int max = selstat.max();

  // integrity checks 1
  if ( d_bidPriceMultiplier < 0.0 )
    {
      s_logger->repx(logga::warn, "negative bid price multiplier", d_bidPriceMultiplier);
    }

  // integrity checks 2
  bool okay = true;
  if ( min < 0 )
    {
      s_logger->repx(logga::warn, "bidset selections negative, min", min);
      okay = false;
    }
  if ( max > count )
    {
      std::ostringstream oss;
      oss << max << " > " << count;
      s_logger->repx(logga::warn, "bidset selection value excessive", oss.str());
      okay = false;
    }
  // additional reporting as appropriate
  // YEEK 6 CODE (set by '--yeek')
  if ( xeona::yeek == 6 || xeona::yeek == 1 || xeona::yeek == 2 || okay == false )
    {
      std::ostringstream put;
      put << "  selection statistics"                    << "\n"
          << "    length     : " << selstat.count()      << "\n"
          << "    min        : " << selstat.min()        << "\n"
          << "    max        : " << selstat.max()        << "\n"
          << "    mean       : " << selstat.mean()       << "\n"
          << "    var        : " << selstat.var()        << "\n"
          << "  bidset statistics"                       << "\n"
          << "    count      : " << count                << "\n"
          << "  status"                                  << "\n"
          << std::boolalpha
          << "    okay       : " << okay                 << "\n";
      s_logger->repx(logga::dbug, "additional reporting follows, yeek", xeona::yeek);
      s_logger->putx(logga::dbug, put);
    }

  if ( okay == false )
    {
      if ( xeona::nopro == false )           // meaning option '--krazy' not applied
        {
          s_logger->repx(logga::warn, "about to throw", "xeona::bidset_selections");
          throw xeona::bidset_selections("AsopLmpBidParam",
                                         __func__,
                                         getIdAndKind(),
                                         min, max, count);
        }
    }

} // function 'AsopLmpBidParam::AsopLmpBidParam'

// ---------------------------------------------------------
//  MEMBER FUNCTION : ~AsopLmpBidParam
// ---------------------------------------------------------

AsopLmpBidParam::~AsopLmpBidParam()
{
  s_logger->repx(logga::adhc, "destructor call", getIdAndKind());
}

// ---------------------------------------------------------
//  MEMBER FUNCTION : establish
// ---------------------------------------------------------
//  Description  : beginning of horizon code
//  Role         : standard call
//  Techniques   : (nothing special)
//  Status       : complete
// ---------------------------------------------------------

void
AsopLmpBidParam::establish()
{
  // initial reporting
  s_logger->repx(logga::adhc, "entering member function", "");

  // call the base function for the underlying work
  AssetOperator::establish();

  // multiple asset caution
  const int teasCnt = d_technicalAssets.size();
  s_logger->repx(logga::adhc, "technical assets count", teasCnt);
  if ( teasCnt > 1 )
    {
      std::ostringstream put;
      put << "  " << "as currently coded, '" << XEONA_FUNC << "'"
          << " does not support multiple assets" << "\n"
          << "  " << "replicate the code in 'AsopLmpBidAdaptive1'"
          << " if you want this functionality" << "\n";
      s_logger->repx(logga::warn, "multiple assets not supported", teasCnt);
      s_logger->putx(logga::dbug, put);
    }
}

// ---------------------------------------------------------
//  MEMBER FUNCTION : constrain
// ---------------------------------------------------------
//  Description  : constrain call
//  Role         : standard call
//  Techniques   : duty coupling
//  Status       : complete
// ---------------------------------------------------------

const int                                    // number of technical assets processed
AsopLmpBidParam::constrain
(const xeona::DomainMode capacityMode)
{
  // initial reporting
  s_logger->repx(logga::adhc, "entering member function", "AsopLmpBidParam");

  // local constant for tweaking successive bids
  const double successiveBidsTweak = 0.001;

  // preamble
  const std::string asopId = getIdentifier();

  // clear previous OSPs because each invocation produces a new set of OSPs
  d_ctls.clear();                            // strictly necessary

  // declare some administration counters
  int teasLoops = 0;                         // number of technical assets processed

  // step thru technical assets in no required order
  BOOST_FOREACH( shared_ptr<TechnicalAsset> ta, d_technicalAssets )
    {
      // keep count
      ++teasLoops;
      xeona::putxId(ta, "AsopLmpBidParam::constrain technical asset loop");

      // useful messaging data
      const std::string aoid = getIdAndKind();
      const std::string taid = ta->getIdAndKind();

      // create and fill a label object
      Label lab(asopId);

      // constrain the associated technical asset (also set floor and ceiling duties)
      const int opsDutyGol = ta->constrain(capacityMode);

      // check for non-meaningful coupling (transmissions assets)
      // then complain and loop again
      if ( opsDutyGol == 0 )
        {
          s_logger->repx(logga::warn, "uncoupled asset encountered", taid);
          std::ostringstream put;
          put << "  LMP asset operator encountered technical asset that"
              << " does not require duty coupling"                         << "\n"
              << "    asset operator class  : " << "AsopLmpBidParam"   << "\n"
              << "    asset operator id     : " << aoid                    << "\n"
              << "    technical operator id : " << taid                    << "\n";
          s_logger->putx(logga::xtra, put);
          s_logger->repx(logga::adhc, "will abandon current loop, count", teasLoops);
          continue;
        }

      // recreate and label new control OSP of the required type
      d_ctl.reset(new CtlLmpBid(d_solver, d_commitmentMode));
      d_ctl->loadOspLabel(lab.str("lmp-stated-ts1"));

      // grab the appropriate bidset
      const int select = d_bidsetSelections->at(d_step);
      if ( select == 0 )
        {
          d_bidset = d_bidsetZero;                // zero bid requested
        }
      else
        {
          d_bidset = d_bidsets.at(select - 1);    // one-based to zero-based adjustment
        }

      // test that bidset
      if ( ! d_bidset )                      // empty bidset
        {
          s_logger->repx(logga::warn, "empty bidset encountered, loop", teasLoops);
          std::ostringstream put;
          put << "  unexpected empty bidset"                      << "\n"
              << "    bidset pointer     : " << d_bidset          << "\n"
              << "    step               : " << d_step            << "\n"
              << "    asset operator  id : " << aoid              << "\n"
              << "    technical asset id : " << taid              << "\n";
          s_logger->putx(logga::dbug, put);
          s_logger->repx(logga::adhc, "will abandon current loop, count", teasLoops);
          continue;
        }

      // obtain technical asset information
      const double ceilingDuty     = ta->getCeilingDuty();  // from 'TechnicalAsset'

      // increase the price a tad on each loop, 'successiveBidsTweak' is function local
      const double bidTweak
        = 1.0 + (successiveBidsTweak * static_cast<double>(teasLoops - 1));
      *d_bidset *= bidTweak;             // price multiplier operator
      std::ostringstream oss;
      oss << std::fixed << std::setprecision(6) << bidTweak;
      s_logger->repx(logga::adhc, "bid tweak applied", getIdAndKind());
      s_logger->repx(logga::xtra, "bid tweak applied", oss.str());

      // check for asset capacitation through insufficient
      // bidding, using floating-point comparision
      double hiQuantity = 0.0;               // load thru pass-by-reference
      if ( d_bidset->wouldCapacitate(ceilingDuty, hiQuantity) )
        {
          std::ostringstream oss;
          oss << ceilingDuty << " : " << hiQuantity;
          s_logger->repx(logga::rankJumpy, "asset capacitation, asset : bid", oss.str());
        }

      // OSP upload bidset call
      int ctlDutyGol;
      boost::tie(ctlDutyGol) = d_ctl->uploadBidSet(d_bidset);

      // OSP couple call (from unit 'b/optprob')
      xeona::couple(d_solver,
                    opsDutyGol,
                    ctlDutyGol,
                    lab.str("couple"));

      // store some information
      d_ctls.push_back(d_ctl);

    } // technical assets loop

  // store bidset in string form
  const std::string bidsetstr    = d_bidset->stringify();
  d_submittedBidsets->at(d_step) = bidsetstr;
  s_logger->repx(logga::adhc, "bidset as string", bidsetstr);

  // return
  s_logger->repx(logga::dbug, "leaving member function, teas loops", teasLoops);
  return teasLoops;

} // function 'AsopLmpBidParam::constrain'

// ---------------------------------------------------------
//  MEMBER FUNCTION : AsopLmpBidParam::washup
// ---------------------------------------------------------

void
AsopLmpBidParam::washup()
{
  // initial reporting
  s_logger->repx(logga::dbug, "entering member function, step", d_step);

  // base class call
  AssetOperator::washup();

  // standing costs processing
  updateStandingCosts(d_step);
}

// ---------------------------------------------------------
//  CLASS           : AsopLmpBidDialog
// ---------------------------------------------------------

// ---------------------------------------------------------
//  MEMBER FUNCTION : AsopLmpBidDialog
// ---------------------------------------------------------

AsopLmpBidDialog::AsopLmpBidDialog
(const std::string entityId,
 Record&           record) :
  CostRegister(record),
  AssetOperator(entityId,
                record,
                xeona::e_auctionLmp),        // commitment mode sum
  CostRegisterAsop(record),
  d_market_side(record.tieSingle<std::string>("market-side")),
  d_marketSide(xeona::e_notSpecified),
  // d_bidsets1(),
  d_ctl(),
  d_ctls()
{
  s_logger->repx(logga::xtra, "constructor call", getIdAndKind());
  d_builtinRemark = "beta";

  // record the market side or else complain about given 'd_market_side' value
  if      ( d_market_side == "demand" ) d_marketSide = xeona::e_demandSide;
  else if ( d_market_side == "supply" ) d_marketSide = xeona::e_supplySide;
  if ( d_marketSide == xeona::e_notSpecified )
    {
      s_logger->repx(logga::warn, "market side string not valid", d_market_side);
    }
}

// ---------------------------------------------------------
//  MEMBER FUNCTION : ~AsopLmpBidDialog
// ---------------------------------------------------------

AsopLmpBidDialog::~AsopLmpBidDialog()
{
  s_logger->repx(logga::adhc, "destructor call", getIdAndKind());
}

// ---------------------------------------------------------
//  MEMBER FUNCTION : constrain
// ---------------------------------------------------------
//  Description  : constrain call
//  Role         : standard call
//  Techniques   : duty coupling, 'xeona::obtainBidsetDialog'
//  Status       : complete
// ---------------------------------------------------------

const int                                    // number of technical assets processed
AsopLmpBidDialog::constrain
(const xeona::DomainMode capacityMode)
{
  // initial reporting
  s_logger->repx(logga::adhc, "entering member function", "AsopLmpBidDialog");

  // preamble
  const std::string asopId = getIdentifier();

  // clear previous OSPs because each invocation produces a new set of OSPs
  d_ctls.clear();                            // strictly necessary

  // declare some administration counters
  int teasLoops = 0;                         // number of technical assets processed

  // step thru technical assets in no required order
  BOOST_FOREACH( shared_ptr<TechnicalAsset> ta, d_technicalAssets )
    {
      // keep count
      ++teasLoops;
      xeona::putxId(ta, "AsopLmpBidDialog::constrain technical asset loop");

      // useful messaging data
      const std::string aoid = getIdAndKind();
      const std::string taid = ta->getIdAndKind();

      // create and fill a label object
      Label lab(asopId);

      // constrain the associated technical asset (also set floor and ceiling duties)
      const int opsDutyGol = ta->constrain(capacityMode);

      // check for non-meaningful coupling (transmissions assets)
      // then complain and loop again
      if ( opsDutyGol == 0 )
        {
          s_logger->repx(logga::warn, "uncoupled asset encountered", taid);
          std::ostringstream put;
          put << "  LMP asset operator encountered technical asset that"
              << " does not require duty coupling"                         << "\n"
              << "    asset operator class  : " << "AsopLmpBidDialog"      << "\n"
              << "    asset operator id     : " << aoid                    << "\n"
              << "    technical operator id : " << taid                    << "\n";
          s_logger->putx(logga::xtra, put);
          s_logger->repx(logga::adhc, "will abandon current loop, count", teasLoops);
          continue;
        }

      // recreate and label new control OSP of the required type
      d_ctl.reset(new CtlLmpBid(d_solver, d_commitmentMode));
      d_ctl->loadOspLabel(lab.str("lmp-stated-ts1"));

      // create intro string for bidset dialog
      std::ostringstream intro;
      intro << "call from"
            << " AsopLmpBidDialog operator " << "'" << aoid << "'"
            << " concerning asset "          << "'" << taid << "'"
            << "\n";

      // grab bid
      const double size           = ta->getCeilingDuty();   // current value
      const std::string bidsetstr = xeona::obtainBidsetDialog(size, intro.str());

      const std::string bidsetLabel = "AsopLmpBidDialog interactive";      // improve
      shared_ptr<LmpBidSet> bidset(new LmpBidSet(bidsetLabel));
      bidset->pushString(bidsetstr);

      // test that bidset
      if ( ! bidset )                        // empty bidset
        {
          s_logger->repx(logga::warn, "empty bidset encountered, loop", teasLoops);
          std::ostringstream put;
          put << "  unexpected empty bidset"                      << "\n"
              << "    bidset pointer     : " << bidset            << "\n"
              << "    step               : " << d_step            << "\n"
              << "    asset operator  id : " << aoid              << "\n"
              << "    technical asset id : " << taid              << "\n";
          s_logger->putx(logga::dbug, put);
          s_logger->repx(logga::adhc, "will abandon current loop, count", teasLoops);
          continue;
        }

      // obtain technical asset information
      const double ceilingDuty     = ta->getCeilingDuty();  // from 'TechnicalAsset'

      // check for asset capacitation through insufficient
      // bidding, using floating-point comparision
      double hiQuantity = 0.0;               // load thru pass-by-reference
      if ( bidset->wouldCapacitate(ceilingDuty, hiQuantity) )
        {
          std::ostringstream oss;
          oss << ceilingDuty << " : " << hiQuantity;
          s_logger->repx(logga::rankJumpy, "asset capacitation, asset : bid", oss.str());
        }

      // OSP upload bidset call
      int ctlDutyGol;
      boost::tie(ctlDutyGol) = d_ctl->uploadBidSet(bidset);

      // OSP couple call (from unit 'b/optprob')
      xeona::couple(d_solver,
                    opsDutyGol,
                    ctlDutyGol,
                    lab.str("couple"));

      // store some information
      d_ctls.push_back(d_ctl);

    } // technical assets loop

  // return
  return teasLoops;

} // function 'AsopLmpBidDialog::constrain'

// ---------------------------------------------------------
//  MEMBER FUNCTION : AsopLmpBidDialog::washup
// ---------------------------------------------------------

void
AsopLmpBidDialog::washup()
{
  // initial reporting
  s_logger->repx(logga::dbug, "entering member function, step", d_step);

  // base class call
  AssetOperator::washup();

  // standing costs processing
  updateStandingCosts(d_step);
}

// ---------------------------------------------------------
//  CLASS           : AsopLmpBidAdaptive1
// ---------------------------------------------------------

// ---------------------------------------------------------
//  MEMBER FUNCTION : AsopLmpBidAdaptive1
// ---------------------------------------------------------
//  Description  : constructor
//  Role         : standard call
//  Techniques   : (nothing special)
//  Status       : complete
// ---------------------------------------------------------

AsopLmpBidAdaptive1::AsopLmpBidAdaptive1
(const std::string entityId,
 Record&           record) :
  CostRegister(record),
  AssetOperator(entityId,
                record,
                xeona::e_auctionLmp),        // commitment mode sum
  CostRegisterAsop(record),
  d_market_side(record.tieSingle<std::string>("market-side")),
  d_openingBidset(record.tieSingle<std::string>("opening-bidset")),
  d_floorPrice(record.tieSingle<double>("floor-price")),
  d_targetCommitment(record.tieSingle<double>("target-commitment")),
  d_relativeHysteresis(record.tieSingle<double>("relative-hysteresis")),
  d_priceFactor(record.tieSingle<double>("price-factor")),
  d_priceDelta(record.tieSingle<double>("price-delta")),
  d_priceCapMultiplier(record.tieSingle<double>("price-cap-multiplier")),
  d_multiAssetPriceTweak(record.tieSingle<double>("multi-asset-price-tweak")),
  d_submittedBidsets(record.tieTimeseries<std::string>("submitted-bidsets")),
  d_finalBidsets(record.tieSingle<std::string>("final-bidsets")),
  d_currentStep(-1),                          // the logic depends on this being negative
  d_priceCap(0.0),
  d_marketSide(xeona::e_notSpecified),
  d_bidset(new LmpBidSet("asop-ctor")),
  d_bidsets(),
  d_ctl(),
  d_ctls()
{
  s_logger->repx(logga::xtra, "constructor call", getIdAndKind());
  d_builtinRemark = "beta";

  // record the market side or else complain about given 'd_market_side' value
  if      ( d_market_side == "demand" ) d_marketSide = xeona::e_demandSide;
  else if ( d_market_side == "supply" ) d_marketSide = xeona::e_supplySide;
  if ( d_marketSide == xeona::e_notSpecified )
    {
      s_logger->repx(logga::warn, "market side string not valid", d_market_side);
    }

  // range check adaptive parameters
  if ( d_relativeHysteresis < 0.0 )
    {
      s_logger->repx(logga::rankJumpy,
                     "negative relative hysteresis",
                     d_relativeHysteresis);
    }
  if ( d_priceFactor < 0.0 )
    {
      s_logger->repx(logga::rankJumpy,
                     "negative price factor",
                     d_priceFactor);
    }
  if ( d_priceDelta < 0.0 )
    {
      s_logger->repx(logga::rankJumpy,
                     "negative price delta",
                     d_priceDelta);
    }

  // check multi-asset price tweak
  if ( d_multiAssetPriceTweak < 0.0 )
    {
      s_logger->repx(logga::warn,
                     "multi-asset priced tweak negative",
                     d_multiAssetPriceTweak);
    }
  else if ( d_multiAssetPriceTweak > 0.50 )
    {
      s_logger->repx(logga::rankJumpy,
                     "multi-asset priced tweak is high",
                     d_multiAssetPriceTweak);
    }
}

// ---------------------------------------------------------
//  MEMBER FUNCTION : ~AsopLmpBidAdaptive1
// ---------------------------------------------------------

AsopLmpBidAdaptive1::~AsopLmpBidAdaptive1()
{
  s_logger->repx(logga::adhc, "destructor call", getIdAndKind());
}

// ---------------------------------------------------------
//  MEMBER FUNCTION : establish
// ---------------------------------------------------------
//  Description  : beginning of horizon code
//  Role         : standard call
//  Techniques   : (nothing special)
//  Status       : complete
// ---------------------------------------------------------

void
AsopLmpBidAdaptive1::establish()
{
  // initial reporting
  s_logger->repx(logga::dbug, "entering member function", "");

  // call the base function for the underlying work
  AssetOperator::establish();

  // create opening bidset
  const std::string tag = "opening";
  d_bidset.reset(new LmpBidSet(tag));
  d_bidset->pushString(d_openingBidset);     // load the bidset

  // test opening bidset
  if ( ! d_bidset )                          // empty bidset
    {
      s_logger->repx(logga::warn, "empty opening bidset encountered", "");
      std::ostringstream put;
      put << "  unexpected empty bidset"                      << "\n"
          << "    comment            : " << d_bidset->label() << "\n"
          << "    bidset pointer     : " << d_bidset          << "\n"
          << "    step               : " << d_step            << "\n";
      s_logger->putx(logga::dbug, put);
    }

  // calculate the cap
  const double weightedPrice = d_bidset->getWeightedPrice();
  d_priceCap                 = d_priceCapMultiplier * weightedPrice;

  // integrity check agains floor price
  if ( d_floorPrice > weightedPrice )
    {
      std::ostringstream oss;
      oss << d_floorPrice << " : " << weightedPrice;
      s_logger->repx(logga::warn, "floor price fails, floor : weighted", oss.str());
    }

  // additional reporting as appropriate
  // YEEK 17 CODE (set by '--yeek')
  if ( xeona::yeek == 17 || xeona::yeek == 1 || xeona::yeek == 2 )
    {
      const std::string func = XEONA_FUNC;   // preprocessor macro defined in 'common.h'
      std::ostringstream put;
      put << "  yeek 17 reporting"                                       << "\n"
          << "    function                  : " << func                  << "\n"
          << "    opening bidset"                                        << "\n"
          << "      opening-bidset string   : " << d_openingBidset       << "\n"
          << "      label                   : " << d_bidset->label()     << "\n"
          << "      stringified             : " << d_bidset->stringify() << "\n"
          << "    adaptation parameters"                                 << "\n"
          << "      floor price             : " << d_floorPrice          << "\n"
          << "      target commitment       : " << d_targetCommitment    << "\n"
          << "      relative hysteresis     : " << d_relativeHysteresis  << "\n"
          << "      price factor            : " << d_priceFactor         << "\n"
          << "      price delta             : " << d_priceDelta          << "\n"
          << "      price cap multiplier    : " << d_priceCapMultiplier  << "\n"
          << "      price cap (weighted)    : " << d_priceCap            << "\n";
      s_logger->repx(logga::dbug, "additional reporting follows, yeek", xeona::yeek);
      s_logger->putx(logga::dbug, put);
      s_logger->addSmartBlank(logga::dbug);
    }
}

// ---------------------------------------------------------
//  MEMBER FUNCTION : constrain
// ---------------------------------------------------------
//  Description  : constrain call (follows 'initialize')
//  Role         : standard call
//  Techniques   : duty coupling
//  Status       : complete
//
//  Design notes
//
//      This logic contained in this function assumes that the
//      multiple assets (often called "units" in this situation)
//      are similar -- although they may vary slightly in terms
//      of capacity, efficiency, and trip point (more properly,
//      the cutout capacity factor).  In particular, the trip for
//      the end-of-line plant could be set lower.
//
//      That said, the dispatch ranking is dynamic.  So when an
//      end-of-line plant runs at capacity it could well become
//      first-of-line (see the code block marked "reorder the
//      technical assets").
//
//      Assets essentially receive the same adapted 'd_bidset',
//      but lower ranked asset have their bids raised by the
//      'd_multiAssetPriceTweak' factor.
//
//      The adaptation process is described under the code block
//      marked "adapt the base bidset".
//
//      The definition of prior duty on the line marked "see
//      design notes" can be experimented with -- at the time of
//      writing, it was set to the so-called operational mean
//      (the sum divided by the number of nonzero entries).
//
// ---------------------------------------------------------

const int                                    // number of technical assets processed
AsopLmpBidAdaptive1::constrain
(const xeona::DomainMode capacityMode)
{
  // initial reporting
  s_logger->repx(logga::adhc, "entering member function", "AsopLmpBidAdaptive1");

  // preamble
  const std::string asopId = getIdentifier();

  // clear the bidsets stored from the previous step
  d_bidsets.clear();

  // clear previous OSPs because each invocation produces a new set of OSPs
  d_ctls.clear();                            // strictly necessary

  // ---------------------------------
  //  loop 1 - get prior duties
  // ---------------------------------

  // prior duties -- use to reorder the technical assets
  std::vector<double> priorDutys;

  // step thru technical assets in no required order
  BOOST_FOREACH( shared_ptr<TechnicalAsset> ta, d_technicalAssets )
    {
      // obtain technical asset information and store result
      const double priorDuty = ta->getPriorDuty();       // 'NaN' on step 0
      priorDutys.push_back(priorDuty);
    }

  // ---------------------------------
  //  reorder the technical assets
  // ---------------------------------

  std::vector<shared_ptr<TechnicalAsset> > technicalAssets;
  technicalAssets = d_technicalAssets;       // copy assignment if fine
  if ( d_step != 0 )                         // skip the first step, no prior info
    {
      // free function 'xeona::reorderUpwards' is defined is 'c/util2'
      if ( ! xeona::reorderUpwards(priorDutys,         // ordering
                                   technicalAssets) )  // data
        {
          s_logger->repx(logga::warn, "xeona::reorderUpwards returned fail", "");
        }
      std::reverse(technicalAssets.begin(), technicalAssets.end());   // see <algorithm>
      // the technical assets are now in order of decreasing prior duty
    }

  // some prior duty statistics are also useful to know
  Statistics<double> priorDutyStats(priorDutys);
  const double minPriorDuty    = priorDutyStats.min();
  const double maxPriorDuty    = priorDutyStats.max();
  const double sumPriorDuty    = priorDutyStats.sum();
  const double meanPriorDuty   = priorDutyStats.mean();
  const double opmeanPriorDuty = priorDutyStats.opmean();   // [1]

  // [1] function 'opmean' returns the "operational mean" -- this
  // is the sum divided by the number of nonzero entries

  // some reporting (also prevents the compiler complaining about non-usage)
  std::ostringstream oss;
  oss << boost::format("%.2e %.2e %.2e  %.2e % .2e")
    % sumPriorDuty % maxPriorDuty % opmeanPriorDuty % meanPriorDuty % minPriorDuty;
  s_logger->repx(logga::adhc, "prior duty: sum max opmean mean min", oss.str());

  // ---------------------------------
  //  adapt the base bidset at start of each step
  // ---------------------------------

  // this code block previously ran multiple times for each
  // capacitiation and solution -- the following conditional
  // prevents that

  if ( d_step > d_currentStep )              // first time for this step
    {
      // update the trip
      d_currentStep = d_step;

      // undertake adaptation: first skip step 0, then screen on
      // hysteresis band (if any), then adjust the bid prices by
      // first multiplying and then adding (or subtracting)

      // define prior duty -- other values could be used
      const double priorDuty = opmeanPriorDuty;  // see design notes

      // relabel bidset
      const std::string tag = "base";
      d_bidset->relabel(tag);

      // reporting string
      std::string process = "(coding error)";

      // adaptation code 1 : adapt using given rules
      if ( d_step == 0 )
        {
          // no adaptation necessary or possible
          process = "first pass with opening bidset";
        }
      else if ( d_floorPrice == d_bidset->getWeightedPrice() )
        {
          process = "floor price threshold met so same";
        }
      else if ( priorDuty < d_targetCommitment * (1.0 - d_relativeHysteresis) )
        {
          // CAUTION: dereferencing of 'd_bidset' is necessary
          *d_bidset *= (1.0 - d_priceFactor);     // decrease prices
          *d_bidset -= d_priceDelta;              // decrease prices
          process = "bidset price decreased adaptively";
        }
      else if ( d_priceCap < d_bidset->getWeightedPrice() )
        {
          process = "price cap met so bidset as before";
        }
      else if ( priorDuty > d_targetCommitment * (1.0 + d_relativeHysteresis) )
        {
          *d_bidset *= (1.0 + d_priceFactor);     // increase prices
          *d_bidset += d_priceDelta;              // increase prices
          process = "bidset price increased adaptively";
        }
      else
        {
          process = "bidset remains unchanged";
          // nothing to do
        }

      // adaptation code 2 : check for a below-the-floor bid and
      // adjust upward as required
      if ( d_step == 0 )
        {
          // nothing to do
        }
      else if ( d_floorPrice > d_bidset->getWeightedPrice() )
        {
          d_bidset->rescaleWeightedPrice(d_floorPrice);
          process = "enforcing the floor price";
        }

      // now report the given action
      const double wp       = d_bidset->getWeightedPrice();
      const std::string swp = boost::str(boost::format("%e") % wp);
      s_logger->repx(logga::adhc, process, d_step);
      s_logger->repx(logga::adhc, "weighted price", swp);

    } // end of start-of-step conditional

  // ---------------------------------
  //  loop 2
  // ---------------------------------

  // declare an administration counter
  int teasLoops  = 0;

  // step thru technical assets in prior duty downwards order
  BOOST_FOREACH( shared_ptr<TechnicalAsset> ta, technicalAssets )
    {
      xeona::putxId(ta, "AsopLmpBidAdaptive1::constrain technical asset loop 2");

      // useful messaging data
      const std::string aoid = getIdAndKind();
      const std::string taid = ta->getIdAndKind();

      // create and fill a label object
      Label lab(asopId);

      // copy construct a distinct bidset and then relabel it
      shared_ptr<LmpBidSet> bidset(new LmpBidSet(*d_bidset));
      const std::string tag = boost::str(boost::format("teas-%02d") % teasLoops);
      bidset->relabel(tag);

      // constrain the associated technical asset (also set floor and ceiling duties)
      const int opsDutyGol = ta->constrain(capacityMode);

      // check for non-meaningful coupling (transmissions assets)
      // then complain and loop again
      if ( opsDutyGol == 0 )
        {
          s_logger->repx(logga::warn, "uncoupled asset encountered", taid);
          std::ostringstream put;
          put << "  LMP asset operator encountered technical asset that"
              << " does not require duty coupling"                         << "\n"
              << "    asset operator class  : " << "AsopLmpBidAdaptive1"   << "\n"
              << "    asset operator id     : " << aoid                    << "\n"
              << "    technical operator id : " << taid                    << "\n";
          s_logger->putx(logga::xtra, put);
          s_logger->repx(logga::adhc, "will abandon current loop, count", teasLoops);
          continue;
        }

      // recreate and label new control OSP of the required type
      d_ctl.reset(new CtlLmpBid(d_solver, d_commitmentMode));
      d_ctl->loadOspLabel(lab.str("lmp-adaptive-1"));

      // obtain technical asset information
      const double ceilingDuty = ta->getCeilingDuty();     // from class 'TechnicalAsset'
      const double priorDuty   = ta->getPriorDuty();       // 'NaN' on step 0

      // reporting
      std::ostringstream oss1;
      oss1 << ceilingDuty << " " << priorDuty << "\n";
      s_logger->repx(logga::dbug, "current ceiling duty, prior duty", oss1.str());

      // increase the price a tad on each asset loop, noting that
      // "multi-asset-price-tweak" is read from the XEM file and
      // is typically 0.1%
      const double bidTweak = 1.0 + (d_multiAssetPriceTweak * teasLoops);
      *bidset *= bidTweak;                   // price multiplier operator
      std::ostringstream oss;
      oss << std::fixed << std::setprecision(6) << bidTweak;
      s_logger->repx(logga::adhc, "bid tweak applied", getIdAndKind());
      s_logger->repx(logga::xtra, "bid tweak applied", oss.str());

      // store current bidset -- noting the asset mapping can change
      d_bidsets.push_back(bidset);

      // check for asset capacitation through insufficient
      // bidding, using floating-point comparision
      double hiQuantity = 0.0;               // load thru pass-by-reference
      if ( bidset->wouldCapacitate(ceilingDuty, hiQuantity) )
        {
          std::ostringstream oss;
          oss << ceilingDuty << " : " << hiQuantity;
          s_logger->repx(logga::rankJumpy, "asset capacitation, asset : bid", oss.str());
        }

      // OSP upload bidset call
      int ctlDutyGol;
      boost::tie(ctlDutyGol) = d_ctl->uploadBidSet(bidset);

      // OSP couple call (from unit 'b/optprob')
      xeona::couple(d_solver,
                    opsDutyGol,
                    ctlDutyGol,
                    lab.str("couple"));

      // store some information
      d_ctls.push_back(d_ctl);

      // additional reporting as appropriate
      // YEEK 17 CODE (set by '--yeek')
      if ( xeona::yeek == 17 || xeona::yeek == 1 || xeona::yeek == 2 )
        {
          std::string priorstr = "(no prior bidset)";
          const std::string bidsetstr = bidset->stringify();
          if ( d_step > 0 ) priorstr  = d_submittedBidsets->at(d_step - 1);     // <<<<<<
          const double weightedPrice  = bidset->getWeightedPrice();
          const std::string func = XEONA_FUNC; // preprocessor macro defined in 'common.h'
          std::ostringstream put;
          put << "  yeek 17 reporting"                                    << "\n"
              << "    function               : " << func                  << "\n"
              << "    step                   : " << d_step                << "\n"
              << "    bidset pointer         : " << bidset                << "\n"
              << "    OSP pointer            : " << d_ctl                 << "\n"
              << "    asset operator  id     : " << aoid                  << "\n"
              << "    technical asset id     : " << taid                  << "\n"
              << "    ops OSP duty gol       : " << opsDutyGol            << "\n"
              << "    ctl OPS duty gol       : " << ctlDutyGol            << "\n"
              << "    current ceiling duty   : " << ceilingDuty           << "\n"
              << "    prior actual duty      : " << priorDuty             << "\n"
              << "    hi bound               : " << hiQuantity            << "\n"
              << "    prior bidset string    : " << priorstr              << "\n"
              << "    current bidset string  : " << bidsetstr             << "\n"
              << "    weighted price         : " << weightedPrice         << "\n";
          s_logger->repx(logga::dbug, "additional reporting follows, yeek", xeona::yeek);
          s_logger->putx(logga::dbug, put);
          s_logger->addSmartBlank(logga::dbug);
        }

      // keep count
      ++teasLoops;

    } // technical assets loop

  // integrity check
  if ( teasLoops == 0 )
    {
      s_logger->repx(logga::warn, "no asset loops made", teasLoops);
    }

  // write back non-tweaked bidset for next time
  d_bidset = d_bidsets.front();              // element must exist

  // store non-tweaked bidset
  const std::string bidsetstr    = d_bidset->stringify();
  d_submittedBidsets->at(d_step) = bidsetstr;

  // process current final bidsets as string -- undertaken here
  // and not in 'conclude' because then the value stays current
  d_finalBidsets = xeona::stringifyBidSets(d_bidsets);      // see 'c/util2'

  // return
  s_logger->repx(logga::dbug, "leaving member function, teas loops", teasLoops);
  return teasLoops;

} // function 'AsopLmpBidAdaptive1::constrain'

// ---------------------------------------------------------
//  MEMBER FUNCTION : AsopLmpBidAdaptive1::washup
// ---------------------------------------------------------

void
AsopLmpBidAdaptive1::washup()
{
  // initial reporting
  s_logger->repx(logga::dbug, "entering member function, step", d_step);

  // base class call
  AssetOperator::washup();

  // standing costs processing
  updateStandingCosts(d_step);
}

// ---------------------------------------------------------
//  CLASS           : AsopLmpBidHydro1
// ---------------------------------------------------------

// ---------------------------------------------------------
//  MEMBER FUNCTION : AsopLmpBidHydro1
// ---------------------------------------------------------

AsopLmpBidHydro1::AsopLmpBidHydro1
(const std::string entityId,
 Record&           record) :
  CostRegister(record),
  AssetOperator(entityId,
                record,
                xeona::e_auctionLmp),        // commitment mode sum
  CostRegisterAsop(record),
  d_inflowWeighting(record.tieSingle<double>("inflow-weighting")),
  d_basicBidPrice(record.tieSingle<double>("basic-bid-price")),
  d_premiumBidPrice(record.tieSingle<double>("premium-bid-price")),
  d_lakeTargets(record.tieSingle<std::string>("lake-targets")),
  d_monthlyTargets(),
  d_monthlyTimeseries(new std::vector<double>()), // CAUTION: allocate storage
  d_ctl(),
  d_ctls()
{
  // initial reporting
  s_logger->repx(logga::xtra, "constructor call", getIdAndKind());
  d_builtinRemark = "beta";

  // integrity checks
  if ( d_inflowWeighting < 0.0 || d_inflowWeighting > 1.0 )
    {
      s_logger->repx(logga::warn, "inflow weighting not [0,1]", d_inflowWeighting);
    }
  if ( d_basicBidPrice < 0.0 )
    {
      std::ostringstream oss;                // otherwise small values display as 0.00
      oss << std::showpos
          << std::scientific
          << std::setprecision(6);           // means here 'n' decimal places, defaults 6
      oss << d_basicBidPrice;
      s_logger->repx(logga::warn, "negative basic price", oss.str());
    }
  if ( d_premiumBidPrice < 0.0 )
    {
      std::ostringstream oss;                // otherwise small values display as 0.00
      oss << std::showpos
          << std::scientific
          << std::setprecision(6);           // means here 'n' decimal places, defaults 6
      oss << d_premiumBidPrice;
      s_logger->repx(logga::warn, "negative premium price", oss.str());
    }
  if ( d_basicBidPrice > d_premiumBidPrice )
    {
      std::ostringstream oss;                // otherwise small values display as 0.00
      oss << std::showpos
          << std::scientific
          << std::setprecision(6);           // means here 'n' decimal places, defaults 6
      oss << d_basicBidPrice << " : "<< d_premiumBidPrice;
      s_logger->repx(logga::warn, "basic price above premium price", oss.str());
    }
  if ( d_lakeTargets.empty() )
    {
      s_logger->repx(logga::warn, "lake-targets in-data empty", d_lakeTargets.size());
    }
}

// ---------------------------------------------------------
//  MEMBER FUNCTION : ~AsopLmpBidHydro1
// ---------------------------------------------------------

AsopLmpBidHydro1::~AsopLmpBidHydro1()
{
  s_logger->repx(logga::adhc, "destructor call", getIdAndKind());
}

// ---------------------------------------------------------
//  MEMBER FUNCTION : establish
// ---------------------------------------------------------

void
AsopLmpBidHydro1::establish()
{
  // initial reporting
  s_logger->repx(logga::dbug, "entering member function", "");

  // preamble
  const int interval    = Entity::getHorizonInterval();
  const int steps       = Entity::getHorizonSteps();
  const int startOffset = Entity::getHorizonOffset();
  const bool leapYear   = Entity::getLeapYear();

  // unpack 'd_lakeTargets'
  try
    {
      const std::string separators = ", ";   // support also comma-separated variables
      d_monthlyTargets = xeona::stringParseSP<double>(d_lakeTargets, separators);
    }
  catch( boost::bad_lexical_cast& e )
    {
      std::ostringstream put;
      put << "  faulty string : \"" << d_lakeTargets << "\"" << "\n";
      s_logger->repx(logga::warn, "lake-targets not castable", "boost::bad_lexical_cast");
      s_logger->putx(logga::dbug, put);
      if ( xeona::nopro == false )           // meaning option '--krazy' not applied
        {
          s_logger->repx(logga::warn, "about to throw", "xeona::entity_issue_2");
          throw xeona::entity_issue_2("AsopLmpBidHydro1",
                                      __func__,
                                      getIdAndKind(),
                                      "lake-targets not castable (check data)");
        }
    } // try block

  // further integrity checks
  Statistics<double> statMonthlyTargets(d_monthlyTargets);
  const int  count = statMonthlyTargets.count();
  const bool range = statMonthlyTargets.range01();     // 'true' if within [0,1]
  if ( count != 12 )
    {
      s_logger->repx(logga::warn, "lake-targets elements not 12", count);
      if ( xeona::nopro == false )           // meaning option '--krazy' not applied
        {
          s_logger->repx(logga::warn, "about to throw", "xeona::entity_issue_2");
          throw xeona::entity_issue_2("AsopLmpBidHydro1",
                                      __func__,
                                      getIdAndKind(),
                                      "lake-targets elements not 12 (check data)");
        }
    }
  if ( ! range )
    {
      s_logger->repx(logga::warn, "lake-targets elements not [0,1]", range);
    }

  // preparation
  const double scaleFactor = 1.0;            // scaling not supported by entity
  const double offset      = 0.0;            // offsetting not supported by entity

  // load block-local monthseries wrapper
  TsMonthly targetsTm(d_monthlyTargets, "monthly inflow targets original");

  // resample
  TsNormal  targetsTs = targetsTm.sampleDuplicate("monthly inflow targets resampled",
                                                  interval,
                                                  startOffset,
                                                  leapYear,
                                                  scaleFactor,
                                                  offset);

  // write back to the primary data member
  const int monthlyOkay = targetsTs.copyfill(d_monthlyTimeseries, steps);

  // integrity check
  if ( monthlyOkay == 0 )
    {
      s_logger->repx(logga::warn, "d_monthlyTimeseries remains empty", monthlyOkay);
    }

  // report
  std::ostringstream put;
  targetsTm.setCaller( XEONA_FUNC );         // preprocessor macro defined in 'common.h'
  targetsTs.setCaller( XEONA_FUNC );
  targetsTm.report(put);
  targetsTs.report(put);
  s_logger->repx(logga::adhc, "timeseries reports follow", "");
  s_logger->putx(logga::adhc, put);

  // gnuplot visualization
  int plotUntil = steps;                     // adaptive
  TsGnuplot gp;                              // plotting object with default command
  gp.addCaller( __FILE__ , __LINE__ );       // less whitespace is okay
  gp.addTs(targetsTs);
  gp.plot("monthly inflow targets", plotUntil, 'p');

  // call the base function for the underlying work
  AssetOperator::establish();

} // function 'AsopLmpBidHydro1::establish'

// ---------------------------------------------------------
//  MEMBER FUNCTION : constrain
// ---------------------------------------------------------
//  Description  : constrain call
//  Role         : standard call
//  Techniques   : duty coupling
//  Status       : complete
// ---------------------------------------------------------

const int                                    // number of technical assets processed
AsopLmpBidHydro1::constrain
(const xeona::DomainMode capacityMode)
{
  // initial reporting
  s_logger->repx(logga::adhc, "entering member function", "AsopLmpBidHydro1");

  // preamble
  const std::string asopId = getIdentifier();

  // clear previous OSPs because each invocation produces a new set of OSPs
  d_ctls.clear();                            // strictly necessary

  // declare some administration counters
  int teasLoops = 0;                         // number of technical assets processed

  // get lake target
  const double inventoryTarget = d_monthlyTimeseries->at(d_step);

  // step thru technical assets in no required order
  BOOST_FOREACH( shared_ptr<TechnicalAsset> ta, d_technicalAssets )
    {
      // keep count
      ++teasLoops;
      xeona::putxId(ta, "AsopLmpBidHydro1::constrain technical asset loop");

      // useful messaging data
      const std::string aoid = getIdAndKind();
      const std::string taid = ta->getIdAndKind();

      // create and fill a label object
      Label lab(asopId);

      // downcast (cast towards specialization)
      shared_ptr<TeasHydroScheme> hs = dynamic_pointer_cast<TeasHydroScheme>(ta);
      if ( hs == 0 )                          // bad cast returns zero
        {
          s_logger->repx(logga::warn, "downcast to TeasHydroScheme failed", hs);
          std::ostringstream put;
          put << "  LMP hydro asset operator encountered technical asset that"
              << " is not a hydro scheme"                               << "\n"
              << "    asset operator class  : " << "AsopLmpBidHydro1"   << "\n"
              << "    asset operator id     : " << aoid                 << "\n"
              << "    technical operator id : " << taid                 << "\n"
              << "  the model may need fixing"                          << "\n";
          s_logger->putx(logga::xtra, put);
          s_logger->repx(logga::adhc, "will abandon current loop, count", teasLoops);
          continue;
        }

      // constrain the associated technical asset (also set floor
      // and ceiling duties) -- this call must occur BEFORE the
      // 'getStatus' call
      const int opsDutyGol = ta->constrain(capacityMode);

      // get hydro status using a 'HydroStatus' object for the data transfer
      shared_ptr<HydroStatus> status = hs->getStatus();
      if ( status->step != d_step )
        {
          s_logger->repx(logga::warn, "step mismatch with hydro status", status->step);
        }

      // prepare bidset
      std::ostringstream tag;
      tag << "hydro scheme / " << taid << " / step " << d_step;
      shared_ptr<LmpBidSet> bidset = prepareBidSet(status, inventoryTarget, tag.str());

      // recreate and label new control OSP of the required type
      d_ctl.reset(new CtlLmpBid(d_solver, d_commitmentMode));
      d_ctl->loadOspLabel(lab.str("lmp-hydro-1"));

      // test current bidset
      if ( ! bidset )                        // empty bidset pointer
        {
          s_logger->repx(logga::warn, "empty bidset encountered", "");
          s_logger->repx(logga::adhc, "will abandon current loop, count", teasLoops);
          continue;
        }

      // obtain technical asset information
      const double ceilingDuty = ta->getCeilingDuty();     // from 'TechnicalAsset'
      const double priorDuty   = ta->getPriorDuty();       // 'NaN' on step 0

      // reporting
      std::ostringstream oss;
      oss << ceilingDuty << " " << priorDuty << "\n";
      s_logger->repx(logga::dbug, "current ceiling duty, prior duty", oss.str());

      // check for asset capacitation through insufficient
      // bidding, using floating-point comparision
      double hiQuantity = 0.0;               // load thru pass-by-reference
      if ( bidset->wouldCapacitate(ceilingDuty, hiQuantity) )
        {
          std::ostringstream oss;
          oss << ceilingDuty << " : " << hiQuantity;
          s_logger->repx(logga::rankJumpy, "asset capacitation, asset : bid", oss.str());
        }

      // OSP upload bidset call
      int ctlDutyGol;
      boost::tie(ctlDutyGol) = d_ctl->uploadBidSet(bidset);

      // OSP couple call (from unit 'b/optprob')
      xeona::couple(d_solver,
                    opsDutyGol,
                    ctlDutyGol,
                    lab.str("couple"));

      // store some information
      d_ctls.push_back(d_ctl);

      // upload bidset to hydro scheme for storage
      hs->putBidSet(bidset);

      // additional reporting as appropriate
      // YEEK 42 CODE (set by '--yeek')
      if ( xeona::yeek == 42 || xeona::yeek == 1 || xeona::yeek == 2 )
        {
          std::ostringstream put;
          put << "  AsopLmpBidHydro1 constrain report"                            << "\n"
              << "    current step               : " << d_step                    << "\n"
              << "    operator identifier        : " << aoid                      << "\n"
              << "    technical asset identifier : " << taid                      << "\n"
              << "    bidset stored              : " << "yes"                     << "\n";
          s_logger->repx(logga::dbug, "additional reporting follows, yeek", xeona::yeek);
          s_logger->putx(logga::dbug, put);
        }

    } // technical assets loop

  // completion reporting
  s_logger->repx(logga::dbug, "leaving member function, teas loops", teasLoops);

  // return
  return teasLoops;

} // function 'AsopLmpBidHydro1::constrain'

// ---------------------------------------------------------
//  MEMBER FUNCTION : prepareBidSet
// ---------------------------------------------------------
//  Description  : prepares a suitable LMP bidset, while taking note of water availability
//  Role         : support function 'constrain'
//  Techniques   : class 'LmpBidSet', class 'HydroStatus'
//  Status       : complete
//
//  Design notes
//
//      Bidset preparation needs to take note of water
//      availability.  Low water is managed by 'TeasHydroScheme'
//      and accounted for here by creating different kinds of
//      bids.  Spill, in contrast, is not a consideration here,
//      and is handled solely by 'TeasHydroScheme'.
//
//  Improvements
//
//      The current implementation is probably somewhat twitchy.
//      The use of smoothing techniques, rolling averages for
//      instance, might offer one avenue for improvement.
//
//  CAUTION: currency
//
//      Individual bids require their generation bands to be
//      expressed as electrical power [W].  However the primary
//      inputs to 'HydroStatus' are expressed in water volumes
//      [m3], derived earlier from water flows [m3/s] by
//      integrating over the interval length [s].  Here, the
//      conversion factor 'fac' "converts" interval-integrated
//      water volume to interval-averaged electrical power.
//
//  CAUTION: water take is positive here and only here!
//
//      Particular care is required regarding signing.  Outflows,
//      comprising take and spill, are normally negative by
//      convention.  In this code, positive quantities are
//      require by the bids.  Hence take is locally positive
//      here.
//
// ---------------------------------------------------------

shared_ptr<LmpBidSet>
AsopLmpBidHydro1::prepareBidSet
(shared_ptr<HydroStatus> status,             // from the associated hydro scheme
 const double            target,             // from this operator, range [0,1]
 const std::string&      label)
{
  // preamble
  const double a           = d_inflowWeighting;   // inflow weighting factor [-]
  const double io          = status->io;          // conversion [J/m3]
  const double interval    = status->interval;    // horizon interval [s]
  const double fac         = io / interval;       // factor [W/m3]

  // "flows" in [W]
//const double inflowNow   = +fac * status->inflowVol;      // originally +ve
  const double inflowPrior = +fac * status->priorInflowVol; // originally +ve
  const double inflowHist  = +fac * status->histInflowVol;  // originally +ve
  const double takeTrunc   = -fac * status->truncTakeVol;   // originally -ve

  // "storage" in [J]
  const double inventPrior = +io * status->priorInventory;
  const double storage     = +io * status->storageVol;

  // calculated desired take, based on inflow weighting factor 'a' ranging [0,1]
  const double vol1        = inflowPrior;                        // inflow persistence
  const double goal        = target * storage;
  const double temp        = (inventPrior - goal) + inflowHist;  // historical target
  const double vol2        = ( temp < 0.0 ) ? 0.0 : temp;        // truncate, see [1]
  const double takeDesired = a * vol1 + (1 - a) * vol2;          // CAUTION: take now +ve

  // [1] a normal hydro station cannot pump water back into the reservoir

  // create bidset
  shared_ptr<LmpBidSet> bidset(new LmpBidSet(label));

  // load some bids -- the format is (band, price):
  std::string type = "(not overwritten)";
  if ( takeTrunc == 0 )                  // no capacity
    {
      type = "no capacity";
      bidset->pushBid(std::make_pair(0.0, 0.0));
    }
  else if ( takeDesired > takeTrunc )    // insufficient capacity
    {
      type = "insufficient capacity to meet desired generation";
      bidset->pushBid(std::make_pair(takeTrunc, d_basicBidPrice));
    }
  else                                     // normal capacity
    {
      type = "sufficient capacity to meet desired generation";
      // zero-priced up to desired take and then premium priced for the remainder
      bidset->pushBid(std::make_pair(takeDesired, d_basicBidPrice));
      bidset->pushBid(std::make_pair(takeTrunc - takeDesired, d_premiumBidPrice));
    }

  // additional reporting as appropriate
  // YEEK 42 CODE (set by '--yeek')
  if ( xeona::yeek == 42 || xeona::yeek == 1 || xeona::yeek == 2 )
    {
      const std::string note = "(0 = historical, 1 = prior)";
      std::ostringstream put;
      put << "  AsopLmpBidHydro1 bid preparation report / all flow treated +ve"   << "\n"
          << "    key: + = direct value, - = factored to electrical energy/power" << "\n"
          << "      identifier                : " << getIdAndKind()               << "\n"
          << "      step                      : " << d_step                       << "\n"
          << "    context"                                                        << "\n"
          << "    + io factor          [J/m3] : " << io                           << "\n"
          << "    + interval              [s] : " << interval                     << "\n"
          << "    aspirations"                                                    << "\n"
          << "      inflow weighting (a)  [-] : " << a << " " << note             << "\n"
          << "      lake target           [-] : " << target                       << "\n"
          << "      storage goal          [J] : " << goal                         << "\n"
          << "    status information"                                             << "\n"
          << "    - max storage           [J] : " << storage                      << "\n"
          << "    - inflow historical     [W] : " << inflowHist                   << "\n"
          << "    - inflow prior          [W] : " << inflowPrior                  << "\n"
          << "    - take truncated        [W] : " << takeTrunc                    << "\n"
          << "    outcomes"                                                       << "\n"
          << "      take desired          [W] : " << takeDesired                  << "\n"
          << "      water state               : " << type                         << "\n";
      put << "\n"
          << bidset->summarizeAll(2);        // trailing newline not required
      s_logger->repx(logga::dbug, "additional reporting follows, yeek", xeona::yeek);
      s_logger->putx(logga::dbug, put);
    }

  // return
  return bidset;

} // function 'AsopLmpBidHydro1::prepareBidSet'

// ---------------------------------------------------------
//  MEMBER FUNCTION : AsopLmpBidHydro1::washup
// ---------------------------------------------------------

void
AsopLmpBidHydro1::washup()
{
  // initial reporting
  s_logger->repx(logga::dbug, "entering member function, step", d_step);

  // base class call
  AssetOperator::washup();

  // standing costs processing
  updateStandingCosts(d_step);
}

//  end of file

