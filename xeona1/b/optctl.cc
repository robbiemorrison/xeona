//  XEONA ENERGY SYSTEMS MODELING ENVIRONMENT
//
//  file-create-name : optctl.cc
//  file-create-date : Fri 17-Oct-2008 14:36 UTC
//  file-initiator   : Robbie Morrison <robbie@actrix.co.nz>
//
//  file-role        : control OSPs for asset operators / implementation
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
//  $URL: file:///home/robbie/svn-root/xeona/futz/trunk/xeona1/b/optctl.cc $
//
//  GENERAL NOTES FOR THIS FILE

//  LOCAL AND SYSTEM INCLUDES

#include "optctl.h"           // companion header for this file (place first)

#include "../f/ospmodes.h"    // domain mode enums (header only)
#include "../d/siglp.h"       // semi-intelligent interface to GLPK MILP solver
#include "../c/util2.h"       // free functions which offer general utilities 2
#include "../c/label.h"       // helper class to format solver labels
#include "../c/costs.h"       // cost sets and support
#include "../b/lmpbid.h"      // LMP auction bidset

#include "../a/logger.h"      // standard logging functionality (as required)
#include "../c/smart_ptr.h"   // toggle between Boost and TR1 smart pointers
#include ".././common.h"      // common definitions for project (place last)

#include <string>             // C++ strings
#include <sstream>            // string-streams

#include <boost/format.hpp>             // printf style formatting
#include <boost/logic/tribool.hpp>      // three state boolean
#include <boost/logic/tribool_io.hpp>   // CAUTION: essential for std::boolalpha reporting

//  NAMESPACE DECLARATIONS

using boost::logic::tribool;            // three state boolean
using boost::logic::indeterminate;      // allows 'indeterminate' and 'indeterminate()'

//  CODE

// ---------------------------------------------------------
//  CLASS           : CtlFirstFeasible_A
// ---------------------------------------------------------

// ---------------------------------------------------------
//  MEMBER FUNCTION : CtlFirstFeasible_A
// ---------------------------------------------------------

CtlFirstFeasible_A::CtlFirstFeasible_A
(shared_ptr<svif::SolverIf> solver,
 const xeona::DomainMode    commitmentMode) :
  ControlOsp(solver, commitmentMode, xeona::e_adminFirst, "ctl-firstfeas-a")
{
  // initial reporting
  s_logger->repx(logga::xtra, "constructor call", "");
}

// ---------------------------------------------------------
//  MEMBER FUNCTION : ~CtlFirstFeasible_A
// ---------------------------------------------------------

CtlFirstFeasible_A::~CtlFirstFeasible_A()
{
  s_logger->repx(logga::xtra, "destructor call", "");
}

// FILL PROBLEM CALLS

// ---------------------------------------------------------
//  MEMBER FUNCTION : uploadNullControl
// ---------------------------------------------------------
//  Description  : fill the solver using intermediate calls
//  Role         : host usage
//  Techniques   : (nothing special)
//  Status       : complete
// ---------------------------------------------------------

void
CtlFirstFeasible_A::uploadNullControl()
{
  // initial reporting
  s_logger->repx(logga::adhc, "entering member function", "");

  // ACTIVE CODE

#if 0 // 0 = no active code, 1 = some active code

  // process label
  Label lab(d_label);

  // col calls
  pushObj(lab.str("null"));

#endif // 0

}

// SPECIALIZED PUSH CALLS

// ---------------------------------------------------------
//  MEMBER FUNCTION : pushObj
// ---------------------------------------------------------

const int
CtlFirstFeasible_A::pushObj
(const std::string tag)
{
  // initial reporting
  s_logger->repx(logga::adhc, "entering member function", "CtlFirstFeasible");

  // ACTIVE CODE

  const double objValue = 0.0;

  // store the objective value
  d_nulObjs.push_back(objValue);

  // load the solver
  ++d_colCount;
  d_solver->loadObj(globalcol(d_colCount), objValue, tag);
  return d_colCount;
}

// ---------------------------------------------------------
//  CLASS           : CtlMeritOrder_A
// ---------------------------------------------------------

// ---------------------------------------------------------
//  MEMBER FUNCTION : CtlMeritOrder_A
// ---------------------------------------------------------

CtlMeritOrder_A::CtlMeritOrder_A
(shared_ptr<svif::SolverIf> solver,
 const xeona::DomainMode    commitmentMode) :
  ControlOsp(solver, commitmentMode, xeona::e_adminMerit, "ctl-merit-a"),
  d_unitLimit(0),                            // zero means no limit
  d_rankings()                               // empty vector
{
  // initial reporting
  s_logger->repx(logga::xtra, "constructor call", "");
}

// ---------------------------------------------------------
//  MEMBER FUNCTION : ~CtlMeritOrder_A
// ---------------------------------------------------------

CtlMeritOrder_A::~CtlMeritOrder_A()
{
  s_logger->repx(logga::adhc, "destructor call", "");
}

// FILL PROBLEM CALLS

// ---------------------------------------------------------
//  MEMBER FUNCTION : uploadRank
// ---------------------------------------------------------
//  Description  : fill the solver using intermediate calls
//  Role         : host usage
//  Techniques   : sorted vectors
//  Status       : complete
//
//  Design notes
//
//      Terminology
//
//          This function implements "prescribed merit order".
//
//          The qualifier "prescribed" is added to
//          differentiate it from terminology used to describe
//          (somewhat optimistically) supply curves in
//          wholesale electricity markets (in Germany at
//          least).
//
//      Merit order ranking
//
//          The following assumes a 'shift' of unity.  Otherwise
//          see comments in the code.  (CAUTION: this shift is
//          different from the GLPK objective "shift" or constant
//          term)
//
//          The merit order ranking must be an integer between
//          unity and 1024 inclusive and the ordering is from
//          best to worst.  This means that a rank 1 asset will
//          always be fully dispatched in preference to any
//          other asset -- and so on down the merit order
//          chain.
//
//          Rankings above 20 are not recommended because
//          optimization scaling issues may occur.  Twenty
//          merit order assets means that the objective
//          function coefficients will range from 1 to 524288
//          (it might be useful to change the 'shift' from 1 to
//          5 say to improve the spread, relative to
//          coefficients from other sources).
//
//    Upper bound 'up'
//
//         The variable 'up' is set so that its associated
//         constraint will (hopefully) never bind.  It is
//         therefore like the "big M" concept in mathematical
//         programming (although that concept had several
//         interpretations).
//
//    Sorted vector 'd_rankings"
//
//          The individual ranks are held in the sorted vector
//          'd_rankings'.  The sorting code comes from Karlsson
//          (2006 ch12).
//
// ---------------------------------------------------------

CtlMeritOrder_A::index_type
CtlMeritOrder_A::uploadRank
(const int    rank,                // a merit order from the series { 1, 2, 3, ..., 1024 }
 const double ceilingDuty)         // used to set a "big M"-style structural coefficient
{
  // initial reporting
  s_logger->repx(logga::adhc, "entering member function", "");

  // PRELIMINARY CHECKS

  // defensive programming
  if ( rank < 1 )
    {
      s_logger->repx(logga::warn, "rank value must be one or greater", rank);
      return 0;
    }
  else if ( rank > 1024 )                    // assuming (see below) 'shift' >= 1
    {
      s_logger->repx(logga::warn, "rank value exceeds 1024", rank);
      return 0;
    }
  else if ( rank > 20 )
    {
      s_logger->repx(logga::info, "rank value above 20 inadvisable", rank);
    }

  if ( ceilingDuty < 0.0 )
    {
      s_logger->repx(logga::warn, "negative ceilingDuty not legal", ceilingDuty);
      return 0;
    }

  // check to see if the 'rank' has already been used
  //
  // 'std::binary_search' is a non-modifying STL searching
  // algorithm for locating certain values in sorted ranges
  //
  // note also that 'd_rankings' is declared in this class and
  // is not inherited

  if ( std::binary_search                                             // refer <algorithm>
       (d_rankings.begin(), d_rankings.end(), rank) )
    {
      // not entirely sure how this could happen but the check remains useful
      s_logger->repx(logga::warn, "given rank previously registered", rank);
      return 0;
    }

  // save the current 'rank' at the appropriate location in 'd_ranks'
  //
  // the 'std::lower_bound' algorithm gives this location, but
  // also expects a sorted sequence, for which a 'std::vector'
  // container is suitable -- 'd_rankings' is sorted by virtue of
  // the previous code

  d_rankings.insert                                                   // refer <vector>
    (std::lower_bound(d_rankings.begin(), d_rankings.end(), rank),    // refer <algorithm>
     rank);                                                           // value to insert

  // ACTIVE CODE

  // create and fill a label object
  Label lab(d_label);
  lab << boost::format("upload-rank-%02d") % rank;          // 'boost::format' streamable

  // convert rank value to mapped binary series
  //
  // for instance, if shift = 1, then { 1, 2, 3, 4 } maps to { 1, 2, 4, 8 }
  const int shift     = 1;                                  // see design notes
  const double objVal = std::pow(2.0, rank - shift);        // refer <cmath>

  // col calls
  const int col = pushObj(objVal, lab.str("dispatch"));     // add dispatch binary var
  markBinary(col);                                          // mark as such
  const int dutyCol = pushObj(0.0, lab.str("dutycol"));     // add duty col

  // first bump the 'ceilingDuty' upwards a bit to make sure it
  // never binds -- noting that duty is in [W] and is unlikely
  // to be less than 1000 (but we cannot guarantee that) and
  // also zero stays at zero
  const double up = 2.0 * ceil(ceilingDuty);

  // constraint calls
  const int row = pushRhs(0.0, svif::L, lab.str("constraint"));  // set up constraint
  pushCof(row, dutyCol, +1.0);               // couple to duty variable
  pushCof(row, col,     -up);                // add binary variable coefficient

  // housekeeping
  d_dutyCol = dutyCol;                       // store duty col for possible later use
  return globalcol(d_dutyCol);               // return duty gol for coupling purposes
}

// ---------------------------------------------------------
//  MEMBER FUNCTION : uploadUnitLimit
// ---------------------------------------------------------

void
CtlMeritOrder_A::uploadUnitLimit
(const int unitLimit)
{
  // initial reporting
  s_logger->repx(logga::xtra, "entering member function, unitlimit", unitLimit);

  // PRELIMINARY CHECKS

  if ( d_dutyCol == 0 )
    {
      s_logger->repx(logga::warn, "must follow uploadRank call", d_dutyCol);
      return;
    }

  // ACTIVE CODE

  d_unitLimit       = unitLimit;
  // TOFIX: merit order control: add unit limit constraint in due course
  // const int dutyCol = d_dutyCol;
  s_logger->repx(logga::warn, "unsupported feature, ignoring call", "");
}

// ---------------------------------------------------------
//  CLASS           : CtlLmpBid_A
// ---------------------------------------------------------

// ---------------------------------------------------------
//  MEMBER FUNCTION : CtlLmpBid_A
// ---------------------------------------------------------

CtlLmpBid_A::CtlLmpBid_A
(shared_ptr<svif::SolverIf> solver,
 const xeona::DomainMode    commitmentMode) :
  ControlOsp(solver, commitmentMode, xeona::e_auctionLmp, "ctl-lmpbid-a")
{
  // initial reporting
  s_logger->repx(logga::xtra, "constructor call, commitment mode", commitmentMode);
}

// ---------------------------------------------------------
//  MEMBER FUNCTION : ~CtlLmpBid_A
// ---------------------------------------------------------

CtlLmpBid_A::~CtlLmpBid_A()
{
  s_logger->repx(logga::adhc, "destructor call", "");
}

// FILL PROBLEM CALLS

// ---------------------------------------------------------
//  MEMBER FUNCTION : uploadBidSet
// ---------------------------------------------------------
//  Description  : uploads an LMP (nodal pricing) bidset
//  Role         : host usage
//  Techniques   : class 'LmpBidSet' for bidsets, includes friendship
//  Status       : complete
// ---------------------------------------------------------

CtlLmpBid_A::index_type                      // duty control gol
CtlLmpBid_A::uploadBidSet
(const shared_ptr<LmpBidSet> bidset)
{
  // initial reporting
  s_logger->repx(logga::adhc, "entering member function", "");

  // ACTIVE CODE

  // CAUTION: first make a copy of 'bidset' because the pop calls
  // later on destroy the bidset -- whilst noting that class
  // 'CtlLmpBid_A' is a friend of class 'LmpBidSet'

  shared_ptr<LmpBidSet> tempBidset(new LmpBidSet(*bidset));

  // declare some administration counters
  int bidCount = 0;                          // bids processed

  // create and initialize a label object
  Label lab(d_label);

  // create a stand-in specific costs cost set
  const CostSet zeroSpecCosts(0.0);

  // first create a bidset balance
  const int setCol = pushObj(0.0, zeroSpecCosts, lab.str("bidset"));
  const int setRow = pushRhs(0.0, svif::E, lab.str("bidset-bal"));
  d_cofCount       = pushCof(setRow, setCol, -1.0);

  // then loop thru the bids
  while ( tempBidset->size() )
    {
      // update bid count
      ++bidCount;

      // set up label
      lab << boost::format("bid-%02d") %  bidCount;

      // grab biggest bid, meaning current highest unit price
      const std::pair<double, double> bid = tempBidset->popBig();
      const double deltaQuantity = bid.first;
      const double unitPrice     = bid.second;

      // additional reporting as appropriate
      // YEEK 12 CODE (set by '--yeek')
      if ( xeona::yeek == 12 || xeona::yeek == 1 || xeona::yeek == 2 )
        {
          std::ostringstream put;
          put << "  bid details"                                      << "\n"
              << "    bid number     : "
              << std::setw(2) << std::setfill('0') << bidCount        << "\n"
              << "    delta quantity : " << deltaQuantity             << "\n"
              << "    unit price     : " << unitPrice                 << "\n";
          s_logger->repx(logga::dbug, "additional reporting follows, yeek", xeona::yeek);
          s_logger->putx(logga::dbug, put);
        }

      // define blockette bounds
      const double lowerBnd = 0;
      const double upperBnd = deltaQuantity;

      // set up blockette variables, just the "out" in this case
      const int bidCol   = pushObj(unitPrice, zeroSpecCosts, lab.str(""));  // "" is okay

      // add blockette variable to common balance
      d_cofCount         = pushCof(setRow, bidCol, +1.0);

      // add constraint for lower and upper bounds
      const int lowerRow = pushRhs(lowerBnd, svif::G, lab.str("lo-band"));
      d_cofCount         = pushCof(lowerRow, bidCol, +1.0);
      const int upperRow = pushRhs(upperBnd, svif::L, lab.str("hi-band"));
      d_cofCount         = pushCof(upperRow, bidCol, +1.0);

      // NOTE: blockette input/output relationships are not required

      lab.trim(1);                           // remove the last "bid-00" labelette
    }

  // completion reporting
  s_logger->repx(logga::adhc, "bid processing complete, count", bidCount);

  // housekeeping
  d_dutyCol = setCol;
  return globalcol(d_dutyCol);

} // function 'CtlLmpBid_A::uploadBidSet'

// ---------------------------------------------------------
//  CLASS           : CtlLeastCost_A
// ---------------------------------------------------------

// ---------------------------------------------------------
//  MEMBER FUNCTION : CtlLeastCost_A
// ---------------------------------------------------------

CtlLeastCost_A::CtlLeastCost_A
(shared_ptr<svif::SolverIf> solver,
 const xeona::DomainMode    commitmentMode) :
  ControlOsp(solver, commitmentMode, xeona::e_shortrunModes, "ctl-leastcost-a")
{
  // initial reporting
  s_logger->repx(logga::xtra, "constructor call", "");
}

// ---------------------------------------------------------
//  MEMBER FUNCTION : ~CtlLeastCost_A
// ---------------------------------------------------------

CtlLeastCost_A::~CtlLeastCost_A()
{
  s_logger->repx(logga::adhc, "destructor call", "");
}

// ---------------------------------------------------------
//  MEMBER FUNCTION : uploadShortrunCosts
// ---------------------------------------------------------
//  Description  : upload standing costs (duty and size independent costs)
//  Role         : host usage
//  Techniques   : increments objective "shift" term
//  Status       : complete
//
//  Design notes
//
//      This call loads the short-run non-duty costs of the
//      asset operator.
//
// ---------------------------------------------------------

void
CtlLeastCost_A::uploadShortrunCosts
(const CostSet& shiftCosts)
{
  // initial reporting
  s_logger->repx(logga::adhc, "entering member function", "");

  // ACTIVE CODE

  // push increment "shift" term
  pushIncShift(shiftCosts);                  // no label required or used
}

// ---------------------------------------------------------
//  CLASS           : CtlQuan_A
// ---------------------------------------------------------

// ---------------------------------------------------------
//  MEMBER FUNCTION : CtlQuan_A
// ---------------------------------------------------------

CtlQuan_A::CtlQuan_A
(shared_ptr<svif::SolverIf> solver,
 const xeona::DomainMode    commitmentMode) :
  ControlOsp(solver, commitmentMode, xeona::e_commitmentModes, "ctl-quan-a"),
  d_cols()
{
  // initial reporting
  s_logger->repx(logga::xtra, "constructor call", "");
}

// ---------------------------------------------------------
//  MEMBER FUNCTION : ~CtlQuan_A
// ---------------------------------------------------------

CtlQuan_A::~CtlQuan_A()
{
  s_logger->repx(logga::adhc, "destructor call", "");
}

// ---------------------------------------------------------
//  MEMBER FUNCTION : uploadControl
// ---------------------------------------------------------
//  Description  : uploads demand quantity
//  Role         : host usage
//  Techniques   : (nothing special)
//  Status       : complete
// ---------------------------------------------------------

CtlQuan_A::index_type                        // duty control gol
CtlQuan_A::uploadControl
(const double demand)                        // demanded quantity
{
  // initial reporting
  s_logger->repx(logga::adhc, "entering member function", "");

  // PREAMBLE

  // create and fill a label object
  Label lab(d_label);                        // entity identifier

  // create a stand-in specific costs cost set
  const CostSet zeroSpecCosts(0.0);

  // INTEGRITY CHECKS

  // very basic range checking -- with negative demand being
  // considered invalid
  if ( demand < 0.0 )
    {
      s_logger->repx(logga::rankJumpy,
                     "negative demand encountered",
                     demand);
    }

  // EXPOSED VARIABLE

  // create one variable
  const int ctlCol  = pushObj(zeroSpecCosts, lab.str("control"));

  // load the local index into a 1-tuple
  d_cols            = boost::make_tuple(ctlCol);

  // DEMAND EQUALITY

  // create an input balance and add a coefficient
  const int quanRow = pushRhs(demand, svif::E, lab.str("demand"));
  d_cofCount        = pushCof(quanRow, ctlCol, +1.0);

  // GLOBALIZE AND RETURN

  // note that external usage requires global indexing
  return globalcols(d_cols);
}

//  end of file

