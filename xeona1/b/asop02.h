//  XEONA ENERGY SYSTEMS MODELING ENVIRONMENT
//
//  file-create-name : asop02.h
//  file-create-date : Thu 09-Jul-2009 15:43 UTC
//  file-initiator   : Robbie Morrison <robbie@actrix.co.nz>
//
//  file-role        : concrete asset operators 2 / header
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
//  $URL: file:///home/robbie/svn-root/xeona/futz/trunk/xeona1/b/asop02.h $
//
//  GENERAL NOTES FOR THIS FILE
//
//  Contains asset operators needed for LMP auctions.

//  HEADER GUARD

#ifndef _ASOP02_H_
#define _ASOP02_H_

//  LOCAL AND SYSTEM INCLUDES

#include "../b/auxs01.h"      // classes for auxiliary model data
#include "../b/asop.h"        // asset operator entity

#include "../a/logger_fwd.h"  // some forward declarations from "logger.h"
#include "../c/smart_ptr.h"   // toggle between Boost and TR1 smart pointers

#include <string>             // C++ strings

//  CODE

// CAUTION: forward declarations given in the code for convenience

// ---------------------------------------------------------
//  FREE FUNCTION   : xeona::obtainBidsetDialog
// ---------------------------------------------------------
//  Description  : prompt user for bid information and return a bidset string
//  Role         : used by interactive bid operator 'AsopLmpBidDialog'
//  Techniques   : 'std::cin' 'boost::lexical_cast'
//  Status       : complete
//
//  Testing
//
//      This free function can be interactively tested by
//      enabling TEST 1 in the accompanying unit test.
//
// ---------------------------------------------------------

namespace xeona
{
  std::string
  obtainBidsetDialog
  (const double       capacity,              // current capacity
   const std::string& intro = "");           // optional intro text with trailing newline
}

// ---------------------------------------------------------
//  CLASS           : AsopGrid
// ---------------------------------------------------------
//  Description  : asset operator which controls LMP nodes and transmission assets
//  Role         : concrete entity
//  Techniques   : (nothing special)
//  Status       : complete
//
//  Design notes
//
//      Will log a warning if duty-coupling technical assets are
//      encountered.
//
// ---------------------------------------------------------

// no OSP needed

class AsopGrid :
  public AssetOperator
{
  // USING DECLARATIONS

protected:

  using Entity::s_logger;                    // place in common scope for this class

  // DISABLED

private:

  AsopGrid();                                     // zero-argument constructor
  AsopGrid(const AsopGrid& orig);                 // copy constructor
  AsopGrid& operator= (const AsopGrid& orig);     // copy assignment operator

  // CREATORS

public:

  explicit
  AsopGrid
  (const std::string entityId,
   Record&           record);

  virtual
  ~AsopGrid();

  // DOMAIN CONTROLLER POINTS OF ENTRY

public:

  virtual                                    // virtual because 'd_lmp_nodes' in not base
  std::vector<shared_ptr<LmpNode> >
  getLmpNodes();

  virtual
  void
  establish();

  virtual
  const int                                  // number of technical assets processed
  constrain
  (const xeona::DomainMode capacityMode);

  // INSTANCE DATA

private:

  // tied quantities

  const std::string&    d_lmp_nodes;
  const double&         d_voltageAngleReference;

}; // class 'AsopGrid'

//  ==== XEDOC =================================================
//
//  entity.asop-grid-0
//
//      class                                    > AsopGrid
//
//        asset operator who controls LMP nodes and transmission
//        assets
//
//      builtin-remark s                         <
//
//      lmp-nodes L                              > "node-1 node-2"
//      technical-assets L                       > "teas-1 teas-2"
//
//        the technical-assets should be limited to transmission
//        assets and node caps,
//
//      voltage-angle-reference [degree] f       > 0.0
//
//        when AC assets are used, the voltage-angle-reference is
//        applied to the first item in lmp-nodes
//
//  ============================================================

// ---------------------------------------------------------
//  CLASS           : AsopLmpBidStatedTs1
// ---------------------------------------------------------
//  Description  : nodal pricing (LMP) operator using read-in bids
//  Role         : concrete entity
//  Techniques   : (nothing special)
//  Status       : complete
//
//  Design notes
//
//      This operator utilizes a read-in bidset timeseries (the
//      "Ts") but just one (the "1").  If more than one technical
//      asset is present, the same bid is applied to both.
//
//      This class could be the starting point for another class
//      'AsopLmpBidAdaptive' which reacts adaptively based on its
//      sales history.
//
// ---------------------------------------------------------

class CtlLmpBid_A;                           // CAUTION: for class declaration typedef
class LmpBidSet;                             // data member

class AsopLmpBidStatedTs1 :
  public AssetOperator,
  public CostRegisterAsop
{
  // USING DECLARATIONS

protected:

  using Entity::s_logger;                    // place in common scope for this class

  // TYPEDEFS

private:

  // CAUTION: note this simple way of swapping OSP
  // implementations, the "_X" postfix needs changing in this one
  // place only -- note also that an implementation change is
  // required to honor the same function names and signatures,
  // but not necessarily the behavior at large

  typedef CtlLmpBid_A CtlLmpBid;             // used for switching implementations

  // DISABLED

private:

  AsopLmpBidStatedTs1();                                             // zero-argument ctor
  AsopLmpBidStatedTs1(const AsopLmpBidStatedTs1& orig);              // copy constructor
  AsopLmpBidStatedTs1& operator= (const AsopLmpBidStatedTs1& orig);  // copy assign opor

  // CREATORS

public:

  explicit
  AsopLmpBidStatedTs1
  (const std::string entityId,
   Record&           record);

  virtual
  ~AsopLmpBidStatedTs1();

  // DOMAIN CONTROLLER POINTS OF ENTRY

public:

  virtual
  const int                                  // number of technical assets processed
  constrain
  (const xeona::DomainMode capacityMode);

  virtual
  void
  washup();

  // INSTANCE DATA

private:

  // tied quantities

  const std::string&                       d_market_side;
  shared_ptr<std::vector<std::string> >    d_bidsets_1;

  // local quantities

  xeona::MarketSide                        d_marketSide;
  std::vector<shared_ptr<LmpBidSet> >      d_bidsets1;

  shared_ptr<CtlLmpBid>                    d_ctl;      // specialization required
  std::vector<shared_ptr<CtlLmpBid> >      d_ctls;

}; // class 'AsopLmpBidStatedTs1'

//  ==== XEDOC =================================================
//
//  entity.asop-lmp-bid-stated-ts1-0
//
//      class                                    > AsopLmpBidStatedTs1
//
//        asset operator using a single read-in (as apposed to
//        stochastic or adaptive) nodal bidding timeseries
//
//      builtin-remark s                         <
//
//      technical-assets L                       > "teas-1 teas-2"
//
//        the order of the technical-assets is not significant
//
//      market-side s                            > "supply"
//
//        the consumer or producer status of the operator is
//        determined by market-side in {demand,supply} respectively
//
//      bidsets-1 [W,$/J] X            > "30.0e+06 28.00e-09 * 50.0e+06 40.00e-09" ..
//
//        bidsets-1 is the sole bidset timeseries in the form of
//        (quantity band, unit price) bids in any order
//
//      standing-cost-financial [$/s] f          > 5.0e-03
//      standing-cost-greenhouse [kg/s] f        > 5.0e-03
//      standing-cost-nox [kg/s] f               > 0.0
//      standing-cost-depletion [J/s] f          > 0.0
//      standing-cost-landuse [m2/s] f           > 0.0
//
//      variable-costs-financial [$] F           < 0.0 ..
//      fixed-costs-financial [$] F              < 0.0 ..
//
//  ============================================================

// ---------------------------------------------------------
//  CLASS           : AsopLmpBidParam
// ---------------------------------------------------------
//  Description  : nodal pricing (LMP) operator using parametrized bids
//  Role         : concrete entity
//  Techniques   : string splitting
//  Status       : complete
//
//  Design notes
//
//      This operator utilizes several read-in bidsets and a
//      selection list is used to choose the prevailing bid.  If
//      more than one technical asset is present, the same bid is
//      applied to all.
//
//  CAUTION: multiple assets not supported
//
//      Unlike 'AsopLmpBidAdaptive1', this entity does not
//      currently support more than one technical asset.  This
//      could be easily fixed by aligning this code, particularly
//      that in 'AsopLmpBidParam::constrain' with
//      'AsopLmpBidAdaptive1::constrain'.
//
// ---------------------------------------------------------

class CtlLmpBid_A;                           // CAUTION: for class declaration typedef
class LmpBidSet;                             // data member

class AsopLmpBidParam :
  public AssetOperator,
  public CostRegisterAsop
{
  // USING DECLARATIONS

protected:

  using Entity::s_logger;                    // place in common scope for this class

  // TYPEDEFS

private:

  // CAUTION: note this simple way of swapping OSP
  // implementations, the "_X" postfix needs changing in this one
  // place only -- note also that an implementation change is
  // required to honor the same function names and signatures,
  // but not necessarily the behavior at large

  typedef CtlLmpBid_A CtlLmpBid;             // used for switching implementations

  // DISABLED

private:

  AsopLmpBidParam();                                        // zero-argument constructor
  AsopLmpBidParam(const AsopLmpBidParam& orig);             // copy constructor
  AsopLmpBidParam& operator= (const AsopLmpBidParam& orig); // copy assignment operator

  // CREATORS

public:

  explicit
  AsopLmpBidParam
  (const std::string entityId,
   Record&           record);

  virtual
  ~AsopLmpBidParam();

  // DOMAIN CONTROLLER POINTS OF ENTRY

public:

  virtual
  void
  establish();

  virtual
  const int                                  // number of technical assets processed
  constrain
  (const xeona::DomainMode capacityMode);

  virtual
  void
  washup();

  // INSTANCE DATA

private:

  // tied quantities

  const std::string&                       d_market_side;
  const std::string&                       d_bidset_list;   // separated list
  const double&                            d_bidPriceMultiplier;
  shared_ptr<std::vector<int> >            d_bidsetSelections;
  shared_ptr<std::vector<std::string> >    d_submittedBidsets;

  // local quantities

  xeona::MarketSide                        d_marketSide;

  shared_ptr<LmpBidSet>                    d_bidsetZero;    // zero bidset
  shared_ptr<LmpBidSet>                    d_bidset;        // current bidset
  std::vector<shared_ptr<LmpBidSet> >      d_bidsets;

  shared_ptr<CtlLmpBid>                    d_ctl;      // specialization required
  std::vector<shared_ptr<CtlLmpBid> >      d_ctls;

}; // class 'AsopLmpBidParam'

//  ==== XEDOC =================================================
//
//  entity.asop-lmp-bid-param-0
//
//      class                                    > AsopLmpBidParam
//
//        asset operator using a single read-in (as apposed to
//        stochastic or adaptive) nodal bidding timeseries, but
//        simplified thru parametrization
//
//      builtin-remark s                         <
//
//      technical-assets L                       > "teas-1 teas-2"
//
//        the order of the technical-assets is not significant
//
//      market-side s                            > "supply"
//
//        the consumer or producer status of the operator is
//        determined by market-side in {demand,supply} respectively
//
//      bidset-list [W,$/J] x                    > "0.0 0.0 * 0.0 0.0 / 0.0 0.0 * 0.0 0.0"
//      bidset-selections [-] I                  > 0 1 2 1 0 ..
//      bid-price-multiplier [-] f               > 1.0
//
//        the bidset-list is a single double-quoted string of
//        slash-separated bidsets, which comprise the usual
//        star-separated (quantity band, unit price) pairs -- the
//        one-based bidset-selections list identifies which
//        bidset to use, and zero means simply bid '0.0 0.0'
//
//        a non-unity bid-price-multiplier will adjust the bidset
//
//      submitted-bidsets [W,$/J] X              < "0.0 0.0" ..
//
//        submitted-bidsets records the bidsets used
//
//      standing-cost-financial [$/s] f          > 5.0e-03
//      standing-cost-greenhouse [kg/s] f        > 5.0e-03
//      standing-cost-nox [kg/s] f               > 0.0
//      standing-cost-depletion [J/s] f          > 0.0
//      standing-cost-landuse [m2/s] f           > 0.0
//
//      variable-costs-financial [$] F           < 0.0 ..
//      fixed-costs-financial [$] F              < 0.0 ..
//
//  ============================================================

// ---------------------------------------------------------
//  CLASS           : AsopLmpBidDialog
// ---------------------------------------------------------
//  Description  : nodal pricing (LMP) operator using interactive bids
//  Role         : concrete entity
//  Techniques   : interactive, 'xeona::obtainBidsetDialog'
//  Status       : complete
//
//  Design notes
//
//      This operative is interactive.
//
//      At present, no record of the submitted bidsets are kept
//      and reported -- this really should be remedied.
//
//      This class could also interact with a nominated remote
//      terminal, duly hard-coded in the data and passed to
//      function 'xeona::obtainBidsetDialog'.  See Robbins and
//      Robbins (2003 ch6 pp183-224) on UNIX special files as a
//      starting point for this exercise.
//
// ---------------------------------------------------------

class CtlLmpBid_A;                           // CAUTION: for class declaration typedef
class LmpBidSet;                             // data member

class AsopLmpBidDialog :
  public AssetOperator,
  public CostRegisterAsop
{
  // USING DECLARATIONS

protected:

  using Entity::s_logger;                    // place in common scope for this class

  // TYPEDEFS

private:

  // CAUTION: note this simple way of swapping OSP
  // implementations, the "_X" postfix needs changing in this one
  // place only -- note also that an implementation change is
  // required to honor the same function names and signatures,
  // but not necessarily the behavior at large

  typedef CtlLmpBid_A CtlLmpBid;             // used for switching implementations

  // DISABLED

private:

  AsopLmpBidDialog();                                         // zero-argument constructor
  AsopLmpBidDialog(const AsopLmpBidDialog& orig);             // copy constructor
  AsopLmpBidDialog& operator= (const AsopLmpBidDialog& orig); // copy assignment operator

  // CREATORS

public:

  explicit
  AsopLmpBidDialog
  (const std::string entityId,
   Record&           record);

  virtual
  ~AsopLmpBidDialog();

  // DOMAIN CONTROLLER POINTS OF ENTRY

public:

  virtual
  const int                                  // number of technical assets processed
  constrain
  (const xeona::DomainMode capacityMode);

  virtual
  void
  washup();

  // INSTANCE DATA

private:

  // tied quantities

  const std::string&                     d_market_side;

  // local quantities

  xeona::MarketSide                      d_marketSide;
//std::vector<shared_ptr<LmpBidSet> >    d_bidsets1;

  shared_ptr<CtlLmpBid>                  d_ctl;   // specialization required
  std::vector<shared_ptr<CtlLmpBid> >    d_ctls;

}; // class 'AsopLmpBidDialog'

//  ==== XEDOC =================================================
//
//  entity.asop-lmp-bid-dialog-0
//
//      class                                    > AsopLmpBidDialog
//
//        asset operator who interactively seeks a new bidset for
//        each interval
//
//      builtin-remark s                         <
//
//      technical-assets L                       > "teas-1 teas-2"
//
//        the order of the technical-assets is not significant
//
//      market-side s                            > "supply"
//
//        the consumer or producer status of the operator is
//        determined by market-side in {demand,supply} respectively
//
//      standing-cost-financial [$/s] f          > 5.0e-03
//      standing-cost-greenhouse [kg/s] f        > 5.0e-03
//      standing-cost-nox [kg/s] f               > 0.0
//      standing-cost-depletion [J/s] f          > 0.0
//      standing-cost-landuse [m2/s] f           > 0.0
//
//      variable-costs-financial [$] F           < 0.0 ..
//      fixed-costs-financial [$] F              < 0.0 ..
//
//  ============================================================

// ---------------------------------------------------------
//  CLASS           : AsopLmpBidAdaptive1
// ---------------------------------------------------------
//  Description  : adaptive nodal pricing (LMP) operator
//  Role         : concrete entity
//  Techniques   : (nothing special)
//  Status       : complete
//
//  Design notes
//
//      This operator is adaptive.  The adaptation strategy is
//      not particularly smart, while the floor-price does set a
//      lower limit.  It is intended as a development stub on
//      which to base more sophisticated measures.
//
// ---------------------------------------------------------

class CtlLmpBid_A;                           // CAUTION: for class declaration typedef
class LmpBidSet;                             // data member

class AsopLmpBidAdaptive1 :
  public AssetOperator,
  public CostRegisterAsop
{
  // USING DECLARATIONS

protected:

  using Entity::s_logger;                    // place in common scope for this class

  // TYPEDEFS

private:

  // CAUTION: note this simple way of swapping OSP
  // implementations, the "_X" postfix needs changing in this one
  // place only -- note also that an implementation change is
  // required to honor the same function names and signatures,
  // but not necessarily the behavior at large

  typedef CtlLmpBid_A CtlLmpBid;             // used for switching implementations

  // DISABLED

private:

  AsopLmpBidAdaptive1();                                             // zero-argument ctor
  AsopLmpBidAdaptive1(const AsopLmpBidAdaptive1& orig);              // copy constructor
  AsopLmpBidAdaptive1& operator= (const AsopLmpBidAdaptive1& orig);  // copy assign oper

  // CREATORS

public:

  explicit
  AsopLmpBidAdaptive1
  (const std::string entityId,
   Record&           record);

  virtual
  ~AsopLmpBidAdaptive1();

  // DOMAIN CONTROLLER POINTS OF ENTRY

public:

  virtual
  void
  establish();

  virtual
  const int                                  // number of technical assets processed
  constrain
  (const xeona::DomainMode capacityMode);

  virtual
  void
  washup();

  // INSTANCE DATA

private:

  // tied quantities

  const std::string&                       d_market_side;
  const std::string&                       d_openingBidset;
  const double&                            d_floorPrice;
  const double&                            d_targetCommitment;
  const double&                            d_relativeHysteresis;
  const double&                            d_priceFactor;
  const double&                            d_priceDelta;
  const double&                            d_priceCapMultiplier;
  const double&                            d_multiAssetPriceTweak;
  shared_ptr<std::vector<std::string> >    d_submittedBidsets;
  std::string&                             d_finalBidsets;

  // local quantities

  int                                      d_currentStep;
  double                                   d_priceCap; // acts on weighted price
  xeona::MarketSide                        d_marketSide;
  shared_ptr<LmpBidSet>                    d_bidset;   // opening bidset
  std::vector<shared_ptr<LmpBidSet> >      d_bidsets;  // per technical asset

  shared_ptr<CtlLmpBid>                    d_ctl;      // specialization required
  std::vector<shared_ptr<CtlLmpBid> >      d_ctls;

}; // class 'AsopLmpBidAdaptive1'

//  ==== XEDOC =================================================
//
//  entity.asop-lmp-bid-adaptive1-0
//
//      class                                    > AsopLmpBidAdaptive1
//
//        asset operator with automated nodal bidding based on
//        the preceding and desired commitments and predetermined
//        price change rules (read source code for details)
//
//      builtin-remark s                         <
//
//      technical-assets L                       > "teas-1"
//
//        technical-assets share the same opening bid and other
//        adaptation parameters -- they should be technically
//        similar
//
//      market-side s                            > "supply"
//
//        the consumer or producer status of the operator is
//        determined by market-side in {demand,supply} respectively
//
//      opening-bidset [W,$/J] x            > "30.0e+06 28.00e-09 * 50.0e+06 40.00e-09"
//      floor-price [$/J] f                 > 10.0e-09
//      target-commitment [W] f             > 40.0e+06
//      relative-hysteresis [-] f           > 0.1
//      price-factor [-] f                  > 0.1
//      price-delta [$/J] f                 > 1.00e-09
//      price-cap-multiplier [-] f          > 5.0
//
//        opening-bidset is the starting bidset in the form of
//        (quantity band, unit price) bids in any order,
//        floor-price applies to the weighted-average price,
//        target-commitment is the desired duty,
//        relative-hysteresis defines the commitment zone within
//        which no adaptation occurs (zero to disable this zone),
//        price-factor is the (non-negative) price multiplier
//        applied first (zero to disable), price-delta is the
//        (non-negative) price increment/decrement applied second
//        (zero to disable), and price-cap-multiplier sets a cap
//        on the weighted average price
//
//        for example, if a bid price rise is required, then:
//        bidset = bidset_previous * (1 + price-factor) + price-delta
//
//      multi-asset-price-tweak [-] f            > 0.001
//
//        the multi-asset-price-tweak can be used to initially
//        prioritize multiple assets by applying a small (perhaps
//        0.1%) price tweak (zero to disable) to the
//        technical-assets in their given order
//
//      submitted-bidsets [W,$/J] X              < "0.0 0.0" ..
//      final-bidsets  [W,$/J] x                 < "0.0 0.0 / 0.0 0.0"
//
//        submitted-bidsets are the actual bidsets for the
//        asset-to-report starting with the opening-bidset,
//        final-bidsets are the final step bidsets, duly
//        slash-separated
//
//      standing-cost-financial [$/s] f          > 5.0e-03
//      standing-cost-greenhouse [kg/s] f        > 5.0e-03
//      standing-cost-nox [kg/s] f               > 0.0
//      standing-cost-depletion [J/s] f          > 0.0
//      standing-cost-landuse [m2/s] f           > 0.0
//
//      variable-costs-financial [$] F           < 0.0 ..
//      fixed-costs-financial [$] F              < 0.0 ..
//
//  ============================================================

// ---------------------------------------------------------
//  CLASS           : AsopLmpBidHydro1
// ---------------------------------------------------------
//  Description  : specialized adaptive nodal pricing (LMP) hydro scheme operator
//  Role         : concrete entity
//  Techniques   : integration with hydro scheme classes
//  Status       : complete
//
//  Design notes
//
//      This operator is adaptive -- the operational strategy is
//      based on maintaining preset inventories and is not
//      particularly sophisticated.  See the section on bidset
//      formation below for more details.
//
//      That said, hydro schemes often consist of a head
//      reservoir and a chain of stations.  The head reservoir
//      typically has substantial storage and is managed
//      seasonally, whereas the downstream stations have limited
//      storage are managed for daily and weekly fluctuations.
//
//      This implementation does not consider that stored water
//      has an opportunity cost -- developing an operator that
//      values water in this way could be provide for an
//      interesting extension.
//
//  Bidset formation
//
//      The bid strategy is based on inventory management.  The
//      aim is to keep the lakes at a given monthly level, based
//      on the long-run average.  Two bids are then made:
//
//        * a zero-cost bid up to the desired outflow
//        * a premium bid above this and up to the maximum take
//
//      This strategy assumes:
//
//        * that the station is never marginal (largely self-fulfilling)
//        * that monthly targets exist
//
// ---------------------------------------------------------

class CtlLmpBid_A;                           // CAUTION: for class declaration typedef
class LmpBidSet;                             // data member
class HydroStatus;                           // hydro asset to operator data transfer

class AsopLmpBidHydro1 :
  public AssetOperator,
  public CostRegisterAsop
{
  // USING DECLARATIONS

protected:

  using Entity::s_logger;                    // place in common scope for this class

  // TYPEDEFS

private:

  // CAUTION: note this simple way of swapping OSP
  // implementations, the "_X" postfix needs changing in this one
  // place only -- note also that an implementation change is
  // required to honor the same function names and signatures,
  // but not necessarily the behavior at large

  typedef CtlLmpBid_A CtlLmpBid;             // used for switching implementations

  // DISABLED

private:

  AsopLmpBidHydro1();                                         // zero-argument constructor
  AsopLmpBidHydro1(const AsopLmpBidHydro1& orig);             // copy constructor
  AsopLmpBidHydro1& operator= (const AsopLmpBidHydro1& orig); // copy assignment operator

  // CREATORS

public:

  explicit
  AsopLmpBidHydro1
  (const std::string entityId,
   Record&           record);

  virtual
  ~AsopLmpBidHydro1();

  // DOMAIN CONTROLLER POINTS OF ENTRY

public:

  void
  establish();

  virtual
  const int                                  // number of technical assets processed
  constrain
  (const xeona::DomainMode capacityMode);

  virtual
  void
  washup();

  // UTILITY FUNCTIONS

private:

  shared_ptr<LmpBidSet>                      // prepared bidset
  prepareBidSet
  (shared_ptr<HydroStatus> status,           // from hydro scheme
   const double            target,           // from the operator
   const std::string&      label);

  // INSTANCE DATA

private:

  // tied quantities

  const double&                          d_inflowWeighting;
  const double&                          d_basicBidPrice;
  const double&                          d_premiumBidPrice;
  const std::string&                     d_lakeTargets;     // 12 entries

  // local quantities

  shared_ptr<std::vector<double> >       d_monthlyTargets;     // from 'd_lakeTargets'
  shared_ptr<std::vector<double> >       d_monthlyTimeseries;  // from 'd_monthlyTargets'

  shared_ptr<CtlLmpBid>                  d_ctl;             // specialization required
  std::vector<shared_ptr<CtlLmpBid> >    d_ctls;

}; // class 'AsopLmpBidHydro1'

//  ==== XEDOC =================================================
//
//  entity.asop-lmp-bid-hydro1-0
//
//      class                                    > AsopLmpBidHydro1
//
//        an asset operator for managing one or more hydro-
//        electric schemes within a nodal market
//
//        although the associated TeasHydroScheme asset forwards
//        information on its status, the bid formation algorithm
//        and settings are held here
//
//        two bids are made: a basic-price bid with the quantity
//        set to track the inventory target and a premium-price
//        bid offering the remaining capacity at high price --
//        the first should more-or-less ensure dispatch and the
//        second should more-or-less mean last in line
//
//      builtin-remark s                         <
//
//      technical-assets L                       > "teas-1"
//
//        technical-assets share the same strategy parameters
//
//      inflow-weighting [-] f                   > 0.5
//      basic-bid-price [$/J] f                  > 0.0e-09
//      premium-bid-price [$/J] f                > 100.0e-09
//
//        an expectation of current inflow is needed to calculate
//        the basic-price bid quantity and the inflow-weighting
//        [0,1] controls the estimate: 0 for actual previous
//        inflow (persistence) and 1 for historical monthly
//        inflow (average monthly) -- the basic-bid matches this
//        target and the premium-bid, if appropriate, offers the
//        remaining available capacity
//
//      lake-targets [-] s > "0.70 0.73 0.73 0.71 0.67 0.63 0.58 0.53 0.50 0.52 0.55 0.62"
//
//        the relative monthly lake-targets [0,1] are used,
//        together with status information from the hydro asset,
//        to manage the bid formation -- 12 values are needed
//        (space-separated and in double-quotes)
//
//      standing-cost-financial [$/s] f          > 5.0e-03
//      standing-cost-greenhouse [kg/s] f        > 5.0e-03
//      standing-cost-nox [kg/s] f               > 0.0
//      standing-cost-depletion [J/s] f          > 0.0
//      standing-cost-landuse [m2/s] f           > 0.0
//
//      variable-costs-financial [$] F           < 0.0 ..
//      fixed-costs-financial [$] F              < 0.0 ..
//
//  ============================================================

#endif // _ASOP02_H_

//  end of file

