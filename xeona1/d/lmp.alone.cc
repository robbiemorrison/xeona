//  XEONA ENERGY SYSTEMS MODELING ENVIRONMENT
//
//  file-create-name : siviz.alone-lmp.cc
//  file-create-date : Mon 23-Jun-2008 15:05 UTC
//  file-initiator   : Robbie Morrison <robbie@actrix.co.nz>
//
//  file-role        : auxiliary LMP (locational marginal pricing) code / stand-alone
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
//  $URL: file:///home/robbie/svn-root/xeona/futz/trunk/xeona1/d/lmp.alone.cc $
//
//  GENERAL NOTES FOR THIS FILE
//
//  This file is not an essential part of 'xeona', but was
//  written to code and test the LMP pricing method (see below).
//
//  BUILDING AND RUNNING THIS CODE
//
//  Build and run this test via the appropriately modified bash script:
//
//     $ siviz.alone.sh
//
//  This script, in turn, invokes the generic 'xeona' 'makefile'
//  with the correct settings.
//
//  LOCATIONAL MARGINAL PRICING
//
//  Locational marginal pricing (LMP) (also known as "nodal
//  pricing") is a form of wholesale electricity pricing which
//  uses the "reduced cost" (or "slack value") of each nodal flow
//  balance to determine the system unit price for that node.
//
//  LMP applies only to the energy component.  Use of the network
//  itself is treated as a separate issue and transmission
//  pricing methods are applied orthogonally (notwithstanding,
//  the question of a more integrated pricing methodology
//  remains).
//
//  LMP pricing is employed in the PJM (Pennsylvania-New
//  Jersey-Maryland), New York, and New England wholesale
//  electricity markets in the USA, in New Zealand, and
//  elsewhere.  Much of the underpinning theory is based on work
//  by William Hogan, Harvard University.  LMP markets have been
//  operating since the mid-1990s, with mixed success in the case
//  of New Zealand.
//
//  The reduced cost for each nodal flow balance is calculated
//  using a direct current (DC) power flow model and a linear
//  program (LP).  The total cost of supply and demand is
//  minimized using the bid unit prices.  The resultant reduced
//  cost for each nodal flow balance sets the system unit price
//  (or "nodal" price) for that node.  If the bids are
//  cost-reflective, the nodal price represents the opportunity
//  cost of export.  However, bids need not be cost-reflective
//  and may indeed be highly strategic.
//
//  If a generator (or rather a bid) is:
//
//    * partially dispatched : nodal price = offer price
//    * fully dispatched     : nodal price > than offer price
//    * not dispatched       : nodal price < than offer price
//
//  In addition, system constraints can result in a lack of
//  competition.  Provoking constraints for the purpose of
//  profiteering is known as constraint gaming.  One form of
//  gaming tactic is to fake an unscheduled outage (used by Enron
//  during the Californian power crisis in 2000).
//
//  The market operator normally also acts as the system banker.
//  Under LMP pricing, a surplus normally accrues.  This then
//  needs to be redistributed, and is often handed to the
//  generators.  In some jurisdictions, this surplus is assigned
//  to the transmission operator -- a practice not generally
//  favored as it offers the transmission operator a perverse
//  incentive to increase grid losses.  Cash flows are not
//  modeled here but can be easily calculated by hand.
//
//  The algorithm implemented here employs full calculation.
//  Faster algorithms exist, including the reference node method.
//
//  Four network components are defined here (these all derived
//  from the 'component' base class):
//
//    * generator
//    * load
//    * line
//    * node
//
//  Transmission losses are quadratic on power flow, which means
//  that the relative losses (defined as one - efficiency) need
//  to be stepwise discretized.  Generator bids (and also
//  demander bids) likewise need to be stepwise.
//
//  The LP problem formulation adopted here assumes that power
//  flows are non-negative.  Transmission lines are naturally
//  bi-directional and hence the 'line' component
//  characterization requires equations for both forward (fwd)
//  and back (bak) flow.
//
//  The 'load' component implemented here is "silent on price" --
//  in other words, complete price-inelasticity (insensitive) on
//  the part of demanders is assumed.
//
//  UNITS AND TIMEBASE
//
//  Non-prefixed SI units are generally assumed.  Currency can be
//  "prefixed" so the following practice is adopted here:
//
//    * prices are set in $/MJ   (2.8 $/MJ equals 10c/kWh)
//    * capacities are set in J  (100e+6 equals 100MW)
//
//  For ease of coding, this file assumes an interval length of
//  one second.  Thus, given half-hourly market clearance, the
//  objective value needs to be multiplied by 1800s.  Similar
//  adjustments are needed for the calculation of nodal payments.
//
//  DESIGN ISSUES
//
//  Each "flow" constitutes a non-negative continuous structural
//  variable.  Each j-th flow may be associated with:
//
//    * a specific cost formation parameter (which becomes an objective coefficient)
//    * lower and upper capacities (which become inequality constraints)
//
//  The term "blockette" refers to the most reduced optimization
//  element (drawn as little white blocks).  A blockette normally
//  contains:
//
//    * at least one input/output relationship (which becomes an equality constraint)
//    * at least one structural variable, typically without external visibility
//
//  A blockette may contain:
//
//    * one or more binary variables
//
//  A host "component" is made up of:
//
//    * at least one blockette
//    * at least one structural variable for external usage [1]
//    * an internal balance for each external variable
//
//  Regarding 'xeona' proper, normally one external structural
//  variable is selected as the "duty variable".

//  LOCAL AND SYSTEM INCLUDES

#include "../d/siglp.h"       // GLPK solver interface in 'svif' namespace (current APIs)
#include "../d/glpkviz.h"     // GLPK problem instance visualizer using HTML
#include "../a/logger.h"      // standard logging functionality (as required)
#include ".././common.h"      // common definitions for project (place last)

#include <algorithm>          // STL copying, searching, and sorting
#include <iostream>           // standard io
#include <limits>             // numeric_limits<T>::infinity() and similar
#include <sstream>            // string-streams
#include <string>             // C++ strings
#include <utility>            // STL pair, make_pair()
#include <vector>             // STL sequence container

//  PREPROCESSOR MACROS FOR TEST PURPOSES

// select test suite from 1 (basic) to 4 (diamond diagram)

#ifndef XE_LMP_TEST                          // allows external definition
# define XE_LMP_TEST 4
#endif

//  CODE

// ---------------------------------------------------------
//  CLASS           : Component (abstract base class)
// ---------------------------------------------------------
//  Description  : abstract base class for the various LMP components
//  Role         : mostly to provide universal access to the solver interface
//  Techniques   : destructor is pure virtual
//  Status       : working
//
//  CAUTION: static order definitions
//
//      See the caution below regarding the order of static
//      definitions.
//
// ---------------------------------------------------------

class Component
{
public:

  typedef shared_ptr<svif::SolverIf> solver_type;

public:

  Component(std::string tag) :
    d_tag(tag)
  { }

  virtual ~Component() = 0;                  // abstract base class

  static
  void
  setSolver                                  // required at outset
  (const solver_type solver)
  {
    s_mip = solver;
  }

  static
  void
  setMarketInterval                          // not currently used
  (const int marketInterval)
  {
    s_interval = marketInterval;
  }

protected:

  std::string               d_tag;           // individual identifier
  static logga::spLogger    s_logger;        // shared_ptr to single logger object
  static solver_type        s_mip;           // common solver interface object
  static int                s_interval;      // market interval (default set below)

};

Component::~Component() { }                  // CAUTION: some definition is required

//  STATIC DEFINITIONS

logga::spLogger Component::s_logger     = logga::ptrLogStream();      // bind logger
Component::solver_type Component::s_mip = Component::solver_type::shared_ptr();
int Component::s_interval               = 1;      // defaults to one second

// ---------------------------------------------------------
// CAUTION: it is essential that the 'Logger' definition precede
// the 'solver_type' definition -- otherwise a core dump with
// the following output will result, depending on how the binary
// was invoked (direct call, via script, under valgrind):
//
// *** glibc detected *** ./siviz.alone: malloc(): memory corruption: 0xb7d371bf ***
//
// ./siviz.alone.sh: line 127: 28506 Segmentation fault      (core dumped) ./$BINARY
//
// ==28140== Invalid read of size 4
// ==28140==    at 0x807B578: logga::Logger::incrementRankCounts(unsigned) (logger.cc:634)
// ==28140==    by 0x8059F49: bool logga::Logger::repx<char[1]>(logga::Rank, [wrap]
//              std::string, char[1] const&, std::string, int, std::string) (logger.h:527)
// ==28140==    by 0x805E336: svif::SolverIf::utilDeleteGlpkProb() (siglp.cc:2470)
// ---------------------------------------------------------

// ---------------------------------------------------------
//  FUNCTION        : ::bid_more
// ---------------------------------------------------------
//  Description  : custom STL sort predicate
//  Role         : sorting of individual bids within a given bidset
//  Techniques   : STL algorithms
//  Status       : working
// ---------------------------------------------------------

namespace
{
  bool
  bid_more
  (std::pair<double, double> p,
   std::pair<double, double> q)
  {
    return p.second > q.second;              // sorts on second pair element
  }
} // unnamed namespace

// ---------------------------------------------------------
//  CLASS           : BidSet
// ---------------------------------------------------------
//  Description  : bidset abstraction
//  Role         : for preparing and passing a bidset to a generator
//  Techniques   : 'std::vector', 'std::pair', 'std::stable_sort'
//  Status       : complete
//
//  Design notes
//
//      An ordered insertion for 'push' would be more elegant
//      than resorting after each 'push_back'.
//
// ---------------------------------------------------------

class BidSet
{
public:

  BidSet() :
    d_bidset()
  { }

  void
  clear()
  {
    d_bidset.clear();                        // remove all elements
  }

  const
  unsigned                                   // current size
  push
  (const double band,
   const double price)
  {
    d_bidset.push_back(std::make_pair(band, price));
    std::stable_sort                         // stable_sort retains equal element ordering
      (d_bidset.begin(),
       d_bidset.end(),
       &::bid_more);                         // custom STL sort predicate defined above
    return static_cast<unsigned>(d_bidset.size());
  }

  const
  unsigned                                   // current size
  size() const
  {
    return static_cast<unsigned>(d_bidset.size());
  }

  std::pair<double, double>
  pop()
  {
    if ( d_bidset.empty() )
      {
        s_logger->repx(logga::warn, "attempt to pop empty bidset", d_bidset.size());
        return std::make_pair(0.0, 0.0);
      }
    std::pair<double, double> bid;
    bid = d_bidset.back();                   // return last element
    d_bidset.pop_back();                     // remove last element
    return bid;
  }

private:

  std::vector<std::pair<double, double> > d_bidset;
  static logga::spLogger                  s_logger;  // shared_ptr to single logger object

};

//  STATIC DEFINITIONS

logga::spLogger BidSet::s_logger = logga::ptrLogStream();      // bind logger

// ---------------------------------------------------------
//  CLASS           : Generator
// ---------------------------------------------------------
//  Description  : bidset representing a generator
//  Role         : model building
//  Techniques   : STL
//  Status       : working
//
//  Design notes
//
//      Naming this a generator is perhaps a little misleading --
//      essentially it is just a set of discrete bids.
//
//  CAUTION: call order
//
//      Calls to 'setMustRun' and 'setMaxCapacity' must be made
//      after 'addBidset' (or else a more sophisticated
//      implementation is needed).
//
// ---------------------------------------------------------

class Generator :
  public Component
{
public:

  Generator(std::string tag) :
    Component(tag),
    d_varOut(0)
  {
    s_logger->repx(logga::dbug, "constructor call", d_tag);
  }

  void
  addBidset
  (BidSet bidset)                            // entire bidset
  {
    // preamble (zero is a nonsensical row and col index)

    int varIndex = 0;                        // row index
    int conIndex = 0;                        // col index

    int conOut   = 0;                        // out balance

    // note also that 'd_varOut' is a data member

    // establish context

    // process row and col markers

    const int varBegin = s_mip->getVarCount() + 1;
    const int conBegin = s_mip->getConCount() + 1;

    d_varOut = varBegin;             // out balance variable, used externally
    conOut   = conBegin;             // out balance constraint, used internally

    // create out balance using 'd_varOut'

    s_mip->loadObj(        d_varOut,  0.0,          d_tag + "-out");
    s_mip->loadRhs(conOut,            0.0, svif::E, d_tag + "-out-bal");
    s_mip->loadCof(conOut, d_varOut, -1.0);

    varIndex = varBegin;
    conIndex = conBegin;

    // loop thru bids

    double delta     = 0;                    // pair element one
    double unitPrice = 0;                    // pair element two

    const unsigned bidsetSize = bidset.size();
    for ( unsigned bidCount = 1;
          bidCount <= bidsetSize;            // CAUTION: do not put 'size' call here
          ++bidCount )
      {
        // unload vector

        std::pair<double, double> bid = bidset.pop();
        delta     = bid.first;
        unitPrice = bid.second;

        // process bid

        double lower = 0;
        double upper = delta;

        std::ostringstream oss;
        oss << d_tag << "-bid" << bidCount;
        std::string tag = oss.str();

        const int varOut   = ++varIndex;
        const int conLower = ++conIndex;
        const int conUpper = ++conIndex;

        // set blockette variable (just out in this case)

        s_mip->loadObj(          varOut,  unitPrice, tag + "-out");

        // add to local out balance

        s_mip->loadCof(conOut, varOut, +1.0);

        // lower and upper constraints

        s_mip->loadRhs(conLower,          lower, svif::G, tag + "-lower");
        s_mip->loadCof(conLower, varOut, +1.0);
        s_mip->loadRhs(conUpper,          upper, svif::L, tag + "-upper");
        s_mip->loadCof(conUpper, varOut, +1.0);

        // NOTE: input/output relationships are not required
      }
  }

  void
  setMustRun
  (const double mustRunCapacity)
  {
    if ( d_varOut == 0 )
      {
        s_logger->repx(logga::dbug, "bid call must precede this call", mustRunCapacity);
        return;
      }

    const int conMustRun = s_mip->getConCount() + 1;

    s_mip->loadRhs(conMustRun,            mustRunCapacity, svif::G, d_tag + "-mustrun");
    s_mip->loadCof(conMustRun, d_varOut, +1.0);
  }

  void
  setMaxCapacity
  (const double maxCapacity)
  {
    if ( d_varOut == 0 )
      {
        s_logger->repx(logga::dbug, "bid call must precede this call", maxCapacity);
        return;
      }

    const int conMustRun = s_mip->getConCount() + 1;

    s_mip->loadRhs(conMustRun,            maxCapacity, svif::L, d_tag + "-maxcap");
    s_mip->loadCof(conMustRun, d_varOut, +1.0);
  }

  int out() { return d_varOut; }

  // RESULTS RECOVERY

  double
  getCommitment()
  {
    return s_mip->getVarValue(d_varOut);
  }

private:

  int    d_varOut;                           // col number

};

// ---------------------------------------------------------
//  CLASS           : Load
// ---------------------------------------------------------
//  Description  : price-inelastic (insensitive) demand
//  Role         : model building
//  Techniques   : STL
//  Status       : working
//
//  Design notes
//
//      This particular implementation of a demander is silent on
//      price.
//
// ---------------------------------------------------------

class Load :
  public Component
{
public:

  Load
  (std::string tag) :
    Component(tag),
    d_varIn(0)
  {
    s_logger->repx(logga::dbug, "constructor call", d_tag);
  }

  void
  setDemand
  (double demand)
  {
    const int varBegin = s_mip->getVarCount() + 1;
    const int conBegin = s_mip->getConCount() + 1;

    d_varIn     = varBegin;                  // in balance variable, used externally
    int varIn   = varBegin + 1;              // demand blockette variable
    int conIn   = conBegin;                  // in balance constraint, used internally
    int conDem  = conBegin + 1;              // demand equality

    // create in balance using 'd_varIn'

    s_mip->loadObj(        d_varIn,  0.0,          d_tag + "-in");
    s_mip->loadRhs(conIn,            0.0, svif::E, d_tag + "-in-bal");
    s_mip->loadCof(conIn,  d_varIn, +1.0);

    // single blockette balance

    s_mip->loadObj(        varIn,    0.0,             d_tag + "-demand");
    s_mip->loadRhs(conDem,           demand, svif::E, d_tag + "-demand");
    s_mip->loadCof(conDem, varIn,   +1.0);
    s_mip->loadCof(conIn,  varIn,   -1.0);
  }

  int in() { return d_varIn; }

  // RESULTS RECOVERY

  double
  getDemand()
  {
    return s_mip->getVarValue(d_varIn);
  }

private:

  int d_varIn;                               // col number

};

// ---------------------------------------------------------
//  CLASS           : Line
// ---------------------------------------------------------
//  Description  : bidirectional transmission line with stepwise relative losses
//  Role         : model building
//  Techniques   : STL
//  Status       : working
//
//  Design notes
//
//      This transmission line component is bidirectional and
//      symmetric.
//
//      The 'characterize' function is overloaded.
//
// ---------------------------------------------------------

class Line :
  public Component
{
public:

  Line
  (std::string tag) :
    Component(tag),
    d_varInFwd(0),
    d_varInBak(0),
    d_varOutFwd(0),
    d_varOutBak(0)
  {
    s_logger->repx(logga::dbug, "constructor call", d_tag);
  }

  void
  characterize
  (const double inputCapacity,               // must be unprefixed SI units (W)
   const double voltage,                     // must be unprefixed SI units (V)
   const double ohmsPerMetre,                // must be unprefixed SI units (ohm/m)
   const double length,                      // must be unprefixed SI units (m)
   const int    steps)                       // discretization steps
  {
    // ---------------------------------------------------------
    //  DC power flow model used
    //
    //                R       I
    //      +   o---VVVVVV------o
    //      |
    //    V |
    //      |       V                 electromotive force [V]
    //      =       I                 current [A]
    //              R                 resistance [ohm]
    //              P      = V.I      power carried in conductor
    //              Ploss  = R.I^2    power loss in resistor
    //              deltaV = R.I      voltage drop across resistor
    //
    //  the relative losses Ploss/P at capacity are used here
    //
    //                R
    //     Ploss/P = ---      at capacity P = V.Icap
    //                V^2
    //
    //  A fixed frequency AC power flow model has been employed
    //  for LMP pricing, but this is complicated -- for instance,
    //  see work by University of New South Wales, Australia
    //
    //  Typical specific resistance values
    //
    //      voltage      ohms/m
    //      -------------------
    //      500kV        30e-06
    //      230kV       100e-06
    //      115kV       170e-06
    //
    //  Source: Phillips, Drew.  2004.  Nodal pricing basics --
    //  OHPs.  IMO independent electricity market operator.
    //  LMP_NodalBasics_2004jan14.pdf [file downloaded from web
    //  in Feb-2008].  Reverse engineered from slide 12.  See
    //  also my spreadsheet (*line-losses*.ods).
    //
    // ---------------------------------------------------------

    // process arguments

    double ohms         = ohmsPerMetre * length;       // DC resistance
    double slope        = ohms / (voltage * voltage);  // R/V^2
    double maxRelLosses = slope * inputCapacity;       // y upper (y lower is zero)
    if ( maxRelLosses >= 1.0 )
      {

        //  check to see if the voltage drop exceeds the input voltage

        std::ostringstream put;
        put << "line characterization aphysical : " << d_tag << "\n"
            << "  inputCapacity    : " << inputCapacity      << "\n"
            << "  voltage          : " << voltage            << "\n"
            << "  ohmsPerMetre     : " << ohmsPerMetre       << "\n"
            << "  length           : " << length             << "\n"
            << "  maxRelLosses     : " << maxRelLosses       << "\n";
        s_logger->putx(logga::info, put);
      }

    // call principal function

    characterize(inputCapacity, maxRelLosses, steps);
  }

  void
  characterize
  (const double inputCapacity,               // input and not output capacity
   const double maxRelLosses,                // relative losses at capacity
   const int    steps)                       // discretization steps
  {
    // preamble (zero is a nonsensical row and col index)

    int varIndex = 0;                        // row index
    int conIndex = 0;                        // col index

    int conIn    = 0;                        // in balance
    int conOut   = 0;                        // out balance

    // note also that 'd_varIn/Out x Fwd/Bak' are data members

    // process arguments

    double tread = inputCapacity / steps;    // x-axis discretization
    double riser = maxRelLosses / steps;     // y-axis discretization

    // process row and col markers

    const int varBegin = s_mip->getVarCount() + 1;
    const int conBegin = s_mip->getConCount() + 1;

    varIndex = varBegin;                     // set to first variable
    conIndex = conBegin;                     // set to first constraint

    d_varInFwd  =   varIndex;                // in balance variable, used externally
    d_varInBak  = ++varIndex;                // in balance variable, used externally
    d_varOutFwd = ++varIndex;                // out balance variable, used externally
    d_varOutBak = ++varIndex;                // out balance variable, used externally

    conIn    =   conIndex;                   // in balance constraint, used internally
    conOut   = ++conIndex;                   // out balance constraint, used internally

    // create in and out balances using 'd_varIn/Out x Fwd/Bak'

    s_mip->loadObj(        d_varInFwd,   0.0, d_tag + "-in-fwd");
    s_mip->loadObj(        d_varInBak,   0.0, d_tag + "-in-bak");
    s_mip->loadObj(        d_varOutFwd,  0.0, d_tag + "-out-fwd");
    s_mip->loadObj(        d_varOutBak,  0.0, d_tag + "-out-bak");
    s_mip->loadRhs(conIn,                0.0, svif::E, d_tag + "-in-bal");
    s_mip->loadRhs(conOut,               0.0, svif::E, d_tag + "-out-bal");

    // add external variables

    s_mip->loadCof(conIn,  d_varInFwd,  +1.0);
    s_mip->loadCof(conIn,  d_varInBak,  -1.0);
    s_mip->loadCof(conOut, d_varOutFwd, -1.0);
    s_mip->loadCof(conOut, d_varOutBak, +1.0);

    // FORWARD BLOCKETTES

    std::string tag;                         // for labeling
    double loss   = 0;                       // normalized loss, y-value
    double lower  = 0;                       // current lower capacity , x-value
    double upper  = 0;                       // current upper capacity, x-value

    for ( int i = 1;                         // loop the blockettes
          i <= steps;
          ++i )
      {
        loss  = (i - 0.5) * riser;           // forms 1/2, 3/2, 5/2, ..
        lower = 0.0;
        upper = tread;

        std::ostringstream oss;
        oss << d_tag << "-fwd" << i;
        tag = oss.str();

        const int varIn    = ++varIndex;
        const int varOut   = ++varIndex;
        const int conLower = ++conIndex;
        const int conUpper = ++conIndex;
        const int conIor   = ++conIndex;

        // two blockette variables

        s_mip->loadObj(          varIn,   0.0, tag + "-in");
        s_mip->loadObj(          varOut,  0.0, tag + "-out");

        // add to local balances

        s_mip->loadCof(conIn,    varIn,  -1.0);
        s_mip->loadCof(conOut,   varOut, +1.0);

        // lower and upper constraints on input

        s_mip->loadRhs(conLower,          lower, svif::G, tag + "-lower");
        s_mip->loadCof(conLower, varIn,  +1.0);
        s_mip->loadRhs(conUpper,          upper, svif::L, tag + "-upper");
        s_mip->loadCof(conUpper, varIn,  +1.0);

        // input/output relationship

        s_mip->loadRhs(conIor,            0.0, svif::E, tag + "-ior");
        s_mip->loadCof(conIor,   varIn,   1.0 - loss);
        s_mip->loadCof(conIor,   varOut, -1.0);

      }

    // BACKWARD BLOCKETTES

    loss   = 0;                              // reset
    lower  = 0;                              // reset
    upper  = 0;                              // reset

    for ( int i = 1;                         // loop the blockettes
          i <= steps;
          ++i )
      {
        loss  = (i - 0.5) * riser;           // forms 1/2, 3/2, 5/2
        lower = 0.0;
        upper = tread;

        std::ostringstream oss;
        oss << d_tag << "-bak" << i;
        tag = oss.str();

        const int varIn    = ++varIndex;
        const int varOut   = ++varIndex;
        const int conLower = ++conIndex;
        const int conUpper = ++conIndex;
        const int conIor   = ++conIndex;

        // two blockette variables

        s_mip->loadObj(          varIn,   0.0, tag + "-in");
        s_mip->loadObj(          varOut,  0.0, tag + "-out");

        // add to local balances

        s_mip->loadCof(conIn,    varOut, +1.0);
        s_mip->loadCof(conOut,   varIn,  -1.0);

        // lower and upper constraints on input

        s_mip->loadRhs(conLower,          lower, svif::G, tag + "-lower");
        s_mip->loadCof(conLower, varIn,  +1.0);
        s_mip->loadRhs(conUpper,          upper, svif::L, tag + "-upper");
        s_mip->loadCof(conUpper, varIn,  +1.0);

        // input/output relationship

        s_mip->loadRhs(conIor,            0.0, svif::E, tag + "-ior");
        s_mip->loadCof(conIor,   varIn,  +1.0 - loss);
        s_mip->loadCof(conIor,   varOut, -1.0);

      }
  }

  std::pair<int,int> in()  { return std::make_pair(d_varInFwd,  d_varInBak);  }
  std::pair<int,int> out() { return std::make_pair(d_varOutFwd, d_varOutBak); }

private:

  int d_varInFwd;                            // col number
  int d_varInBak;                            // col number
  int d_varOutFwd;                           // col number
  int d_varOutBak;                           // col number

};

// ---------------------------------------------------------
//  CLASS           : Node
// ---------------------------------------------------------
//  Description  : basic node
//  Role         : model building
//  Techniques   : nothing of note
//  Status       : working
// ---------------------------------------------------------

class Node :
  public Component
{
public:

  Node(std::string tag) :
    Component(tag),
    d_varMe(0),
    d_conMe(0)
  {
    s_logger->repx(logga::dbug, "constructor call", d_tag);
  }

  void
  inject
  (int var)
  {
    initMe();                                     // initialization as required
    s_mip->loadCof(d_conMe, var,         +1.0);   // patch in var
  }

  void
  inject
  (std::pair<int, int> vars)
  {
    initMe();
    s_mip->loadCof(d_conMe, vars.first,  +1.0);   // patch in var
    s_mip->loadCof(d_conMe, vars.second, -1.0);   // patch in var
  }

  void
  extract
  (int var)
  {
    initMe();                                     // initialization as required
    s_mip->loadCof(d_conMe, var,         -1.0);   // patch in var
  }

  void
  extract
  (std::pair<int, int> vars)
  {
    initMe();
    s_mip->loadCof(d_conMe, vars.first,  -1.0);     // patch in var
    s_mip->loadCof(d_conMe, vars.second, +1.0);     // patch in var
  }

  // RESULTS RECOVERY

  double
  getLmpPrice()
  {
//      return s_mip->getRedRowValue(d_conMe);
    return s_mip->getSlackValue(d_conMe);
  }

private:

  void
  initMe()                                        // initialize this node
  {
    if ( d_varMe > 0 ) return;

    const int varBegin = s_mip->getVarCount() + 1;
    const int conBegin = s_mip->getConCount() + 1;

    d_varMe = varBegin;
    d_conMe = conBegin;

    s_mip->loadRhs(d_conMe,               0.0, svif::E, d_tag + "-bal");
  }

private:

  int d_varMe;                               // col number
  int d_conMe;                               // row number

};

// ---------------------------------------------------------
//  MAIN FUNCTION   : main
// ---------------------------------------------------------

int
main(int argc, char* argv[])
{
  logga::spLogger logger = logga::ptrLogStream();  // main function logger
  logger->repx(logga::info, "beginning of main", "~~~~~~~~~~");
  logger->addSmartBlank();
  logger->setReportLevel(logga::xtra);       // maximum reporting

  // PRELIMINARY

  std::ostringstream put;

#if   (XE_LMP_TEST == 1)

  // ---------------------------------------------------------
  //  test ONE        : basic model (without transmission)
  // ---------------------------------------------------------

  logger->test(1);

  {
    put << "basic model" << "\n";
    logger->putx(logga::dbug, put);

    // PREAMBLE

    const std::string model = "basic";       // model name

    GlpkViz webbrowse("firefox -new-tab");   // 'new-tab' seems to be is implied

    shared_ptr<svif::SolverIf>
      lmp(new svif::SolverIf
          (model,                            // mandatory
           svif::high));                     // default set via _XDEBUG

    Component::setSolver(lmp);               // set static variable in abstract base class

    lmp->initUseSimpPresolver();
    // lmp->initSetPrefLPSolver(svif::solver_interior);

    lmp->setObjectiveSense(svif::minimize, "nodal-pricing");

    // BUILD COMPONENTS (2.8 $/MJ equals 10c/kWh)

    Generator geni1("geni1");
    BidSet bidset1;
    bidset1.push(200e+6, 2.8);               // 000 MW x $/MJ
    bidset1.push(100e+6, 4.0);

    geni1.addBidset(bidset1);
    geni1.setMustRun(50e+6);                 // must run value

    webbrowse(lmp, "just geni1", 4, true);

    Load load1("load1");
    load1.setDemand(250e6);

    // PATCH TOGETHER

    Node nodeA("nodeA");
    nodeA.inject(  geni1.out() );
    nodeA.extract( load1.in()  );

    //  SOLVE PROBLEM

    lmp->runSolver();

    //  VISUALIZATION

    webbrowse(lmp, "final view", 4, true);

    // HOUSEKEEPING

    // free memory -- although the only instance is about to go
    // out of scope in any case

    lmp.reset();                             // redundant in the circumstances

  }

#elif (XE_LMP_TEST == 2)

  // ---------------------------------------------------------
  //  test TWO        : line diagram (with transmission)
  // ---------------------------------------------------------
  //
  //  Based on the hydro-spill problem in New Zealand.
  //
  //  If the 10GW hydro capacity ('geni1') bids reasonably
  //  (5c/kWh), then the rather huge 5GW combined-cycle gas
  //  turbine in Auckland can "guzump it" with a large
  //  zero-priced bid and still collect.  Water will be spilled
  //  if storage and inflows are high.  This has happened!
  //
  //  It the hydro operator then bids zero for dispatch, then the
  //  price dives, but the dispatch solution remains.  No doubt
  //  some kind of cat-and-mouse bidding game ensues, until the
  //  story hits the media and the energy minister steps in.
  //
  // ---------------------------------------------------------

  logger->test(2);

  {
    put << "line diagram" << "\n";
    logger->putx(logga::dbug, put);

    // PREAMBLE

    const std::string model = "line";

    GlpkViz webbrowse("firefox");

    shared_ptr<svif::SolverIf>
      lmp(new svif::SolverIf
          (model,                            // mandatory
           svif::high));                     // default set via _XDEBUG

    Component::setSolver(lmp);               // set static variable in abstract base class

    lmp->initUseSimpPresolver();
    // lmp->initSetPrefLPSolver(svif::solver_interior);

    lmp->setObjectiveSense(svif::minimize, "nodal-pricing");

    // BUILD COMPONENTS

    const double zbid = 0.0e-00;             // behaves okay with zero bids in THIS case

    Generator geni1("geni1");
    BidSet bidset1;
//  bidset1.push(10000e+6, zbid);            // 000 MW x $/MJ
    bidset1.push(10000e+6, 1.4);             // 000 MW x $/MJ
    geni1.addBidset(bidset1);

    webbrowse(lmp, "just geni1", 4, true);

    Generator geni2("geni2");
    BidSet bidset2;
    bidset2.push(4900e+6, zbid);             // 000 MW x $/MJ
    bidset2.push( 100e+6, 28.0);
    geni2.addBidset(bidset2);

    Line line1("line1");
    line1.characterize(10000e+6,             // 10000 MW
                       0.20,                 // relative loss at capacity
                       2);                   // 2 steps

    Load load1("load1");
    load1.setDemand(5000e6);                 // 5000 MW

    // PATCH TOGETHER

    Node nodeA("nodeA");                     // South Island node (Benmore)
    nodeA.inject(  geni1.out() );            // hydro production
    nodeA.extract( line1.in()  );            // national grid

    Node nodeB("nodeB");                     // Auckland node (Otahuhu)
    nodeB.inject(  geni2.out() );            // local thermal
    nodeB.inject(  line1.out() );            // national grid
    nodeB.extract( load1.in()  );            // domestic, commercial, and industrial load

    //  SOLVE PROBLEM

    lmp->runSolver();

    //  VISUALIZATION

    webbrowse(lmp, "final view", 4, true);

    //  ADDITIONAL REPORTING

    lmp->setGlpkRounding(true);              // note: can turn off and set precision to 20

    const double scale1 = 1.0;               // currency scale
    const double scale2 = 1.0e-6;            // flow scale
    std::ostringstream put;
    put << std::showpoint
        << "  scaling"                                                << "\n"
        << "    currency    : " << 1/scale1                           << "\n"
        << "    flow        : " << 1/scale2                           << "\n"
        << std::noshowpoint
        << std::fixed << std::setprecision(2)
        << "  LMPs [$/MJ]"                                            << "\n"
        << "    node a      : " << nodeA.getLmpPrice() * scale1       << "\n"
        << "    node b      : " << nodeB.getLmpPrice() * scale1       << "\n"
        << "  unit commitments [MW]"                                  << "\n"
        << "    generator 1 : " << geni1.getCommitment() * scale2     << "\n"
        << "    generator 2 : " << geni2.getCommitment() * scale2     << "\n"
        << "  demand [MW]"                                            << "\n"
        << "    load 1      : " << load1.getDemand() * scale2         << "\n"
        << std::flush;
    logger->putx(logga::dbug, put);

    // HOUSEKEEPING

    // free memory -- although the only instance is about to go
    // out of scope in any case

    lmp.reset();                             // redundant in the circumstances

  }

#elif (XE_LMP_TEST == 3)

  // ---------------------------------------------------------
  //  test THREE      : triangle diagram
  // ---------------------------------------------------------

  logger->test(3);

  {
    put << "triangle diagram" << "\n";
    logger->putx(logga::dbug, put);

    // PREAMBLE

    const std::string model = "triangle";

    GlpkViz webbrowse("firefox");

    shared_ptr<svif::SolverIf>
      lmp(new svif::SolverIf
          (model,                            // mandatory
           svif::high));                     // default set via _XDEBUG

    Component::setSolver(lmp);               // set static variable in abstract base class

    lmp->initUseSimpPresolver();
    // lmp->initSetPrefLPSolver(svif::solver_interior);

    lmp->setObjectiveSense(svif::minimize, "nodal-pricing");

    // BUILD COMPONENTS

    Generator geni1("geni1");
    BidSet bidset1;
    bidset1.push(200e+6, 2.5);               // 000 MW x $/MJ
    bidset1.push(100e+6, 4.5);
    geni1.addBidset(bidset1);

    webbrowse(lmp, "just geni1", 4, true);

    Generator geni2("geni2");
    BidSet bidset2;
    bidset2.push(100e+6, 4.0);               // intentionally out of order
    bidset2.push(200e+6, 3.0);
    geni2.addBidset(bidset2);

    Line line1("line1");
    line1.characterize(400e+6,               // 400 MW
                       0.20,                 // relative loss at capacity
                       2);                   // 2 steps

    Line line2("line2");
    line2.characterize(400e+6,
                       0.20,
                       2);

    Line line3("line3");
    line3.characterize(400e+6,
                       0.10,
                       2);

    Load load1("load1");
    load1.setDemand(400e6);                  // 400 MW

    // PATCH TOGETHER

    Node nodeA("nodeA");
    nodeA.inject(  geni1.out() );
    nodeA.extract( line1.in()  );
    nodeA.extract( line3.in()  );

    Node nodeB("nodeB");
    nodeB.inject(  geni2.out() );
    nodeB.inject(  line1.out() );
    nodeB.extract( line2.in()  );

    Node nodeC("nodeC");
    nodeC.inject(  line2.out() );
    nodeC.inject(  line3.out() );
    nodeC.extract( load1.in()  );

    //  SOLVE PROBLEM

    lmp->runSolver();

    //  VISUALIZATION

    webbrowse(lmp, "final view", 4, true);

    // HOUSEKEEPING

    // free memory -- although the only instance is about to go
    // out of scope in any case

    lmp.reset();                             // redundant in the circumstances

  }

#elif (XE_LMP_TEST == 4)

  // ---------------------------------------------------------
  //  test FOUR       : diamond diagram
  // ---------------------------------------------------------
  //
  //  CAUTION: documented test problem
  //
  //      This is a documented test problem.  Do NOT modify the
  //      underlying data.
  //
  //  Some trials with the 'sublines' parameter as a quick way of
  //  building big models!
  //
  //    * negative goal value are due to objective toggle
  //    * subline above 200 timed with /usr/bin/time and user recorded
  //
  //    sublines   rowxcol      iters   goal        hh:mm:ss   comment
  //    ----------------------------------------------------------------
  //       0       28 x    26       4   0          under 1s   proved nonfeasible by GLPK
  //       1       52 x    42      35  -9.864e+08  under 1s
  //       2       76 x    58      42  -9.257e+08  under 1s
  //       3      100 x    74      51  -9.042e+08  under 1s   [1]
  //      20      500 x   400     227  -8.892e+08  under 1s   will display with GLPK viz
  //     200     4828 x  3226    2090  -8.891e+08  00:00:34
  //     500    12028 x  8026    5198  -8.891e+08  00:05:10
  //     700    16828 x 11226    7262  +8.891e+08  00:10:28   identical with toggle
  //    1000    24028 x 16026   10367  +8.891e+08  00:22:17
  //    1500    36028 x 24026   15539  +8.891e+08  00:51:36
  //
  //    memory usage was never observed to be above 6% = 30MB
  //
  //    note [1]: every combination of presolver, max/min, and
  //    'valgrind' (or not) was tried and everything looked fine
  //
  // ---------------------------------------------------------

    logger->test(4);

  {
    put << "diamond diagram" << "\n";
    logger->putx(logga::dbug, put);

    // PREAMBLE

    const int sublines = 3;                  // quadratic loss discretization steps (3)

    const std::string model = "diamond";

    GlpkViz webbrowse("firefox");            // visualizer object

    shared_ptr<svif::SolverIf>
      lmp(new svif::SolverIf
          (model,                            // mandatory
           svif::high));                     // default set via _XDEBUG

    Component::setSolver(lmp);               // set static variable in abstract base class

    lmp->initSimplexPresolver();
    // lmp->initSetPrefLPSolver(svif::solver_interior);

    lmp->setObjectiveSense(svif::minimize, "nodal-pricing");

    // BUILD COMPONENTS (2.8 $/MJ equals 10c/kWh)

    logga::Rank prior;
    prior = logger->setReportLevel(logga::info);  // reduce reporting

    Generator geni1("geni1");
    BidSet bidset1;
    bidset1.push(200e+6, 1.0);               // 000 MW x $/MJ
    bidset1.push(100e+6, 4.0);
    bidset1.push(100e+6, 6.0);
    geni1.addBidset(bidset1);

    webbrowse(lmp, "just geni1", 4, true);

    Generator geni2("geni2");
    BidSet bidset2;
    bidset2.push(200e+6, 2.5 );
    bidset2.push(100e+6, 4.0);               // the price setting bid
    bidset2.push(100e+6, 5.0);
    geni2.addBidset(bidset2);

    Line line1("line1");                     // with 25% marginal losses
    line1.characterize(400e+6,               // 400 MW
                       220e+3,               // 220 kV
                       30e-6,                // ohms/metre
                       1000e+3,              // 1000 km
                       sublines);            // 3 steps

    Line line2("line2");
    line2.characterize(400e+6,
                       220e+3,
                       30e-6,
                       1000e+3,
                       sublines);

    Line line3("line3");
    line3.characterize(400e+6,
                       220e+3,
                       30e-6,
                       1000e+3,
                       sublines);

    Line line4("line4");                     // differs from others
    line4.characterize(100e+6,               // 100 MW
                       220e+3,
                       50e-6,
                       1000e+3,
                       sublines);

    Load load1("load1");
    load1.setDemand(400e6);                  // 400 MW

    // PATCH TOGETHER

    Node nodeA("nodeA");
    nodeA.inject(  geni1.out() );
    nodeA.extract( line1.in()  );
    nodeA.extract( line4.in()  );

    Node nodeB("nodeB");
    nodeB.inject(  line1.out() );
    nodeB.extract( line2.in()  );

    Node nodeC("nodeC");
    nodeC.inject(  geni2.out() );
    nodeC.inject(  line2.out() );
    nodeC.extract( line3.in()  );

    Node nodeD("nodeD");
    nodeD.inject(  line3.out() );
    nodeD.inject(  line4.out() );
    nodeD.extract( load1.in()  );

    logger->setReportLevel(prior);           // return to normal reporting

    //  WORK-AROUND FOR EARLIER GLPK DIRECTIONALITY PROBLEM

#if 0 // 1 = use toggle objective, 0 is run as intended
    // CAUTION: do NOT run under 'valgrind' with these settings
    lmp->setObjectiveSense(svif::maximize, "toggled-objective");
    lmp->toggleObjective();
    std::string objToggle = "toggled (negative)";
#else
    std::string objToggle ="normal";
#endif // 0

    //  SOLVE PROBLEM

    lmp->runSolver();

    //  VISUALIZATION

    if ( lmp->getConCount() < 200 )         // protect against large problems
      {
        webbrowse(lmp, "final view", 4, true);
      }

    //  ADDITIONAL REPORTING

    lmp->setGlpkRounding(true);              // note: can turn off and set precision to 20

    const double scale1 = 1.0;               // currency scale
    const double scale2 = 1.0e-6;            // flow scale
    std::ostringstream put;
    put << std::showpoint
        << "  scaling"                                                << "\n"
        << "    currency    : " << 1/scale1                           << "\n"
        << "    flow        : " << 1/scale2                           << "\n"
        << std::noshowpoint
        << std::fixed << std::setprecision(2)
        << "  problem settings"                                       << "\n"
        << "    sublines    : " << sublines                           << "\n"
        << "    objective   : " << objToggle                          << "\n"
        << "  LMPs [$/MJ]"                                            << "\n"
        << "    node a      : " << nodeA.getLmpPrice() * scale1       << "\n"
        << "    node b      : " << nodeB.getLmpPrice() * scale1       << "\n"
        << "    node c      : " << nodeC.getLmpPrice() * scale1       << "\n"
        << "    node d      : " << nodeD.getLmpPrice() * scale1       << "\n"
        << "  unit commitments [MW]"                                  << "\n"
        << "    generator 1 : " << geni1.getCommitment() * scale2     << "\n"
        << "    generator 2 : " << geni2.getCommitment() * scale2     << "\n"
        << "  demand [MW]"                                            << "\n"
        << "    load 1      : " << load1.getDemand() * scale2         << "\n"
        << std::flush;
    logger->putx(logga::dbug, put);

    // HOUSEKEEPING

    // free memory -- although the only instance is about to go
    // out of scope in any case

    lmp.reset();                             // redundant in the circumstances

  }

#endif // XE_LMP_TEST

  // HOUSEKEEPING

  logger->test(0);                           // test end

  logger->repx(logga::info, "end of main", "~~~~~~~~~~");
  return xeona::exit_success;

} // main function

//  end of file

