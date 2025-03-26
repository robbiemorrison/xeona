//  XEONA ENERGY SYSTEMS MODELING ENVIRONMENT
//
//  file-create-name : optops.cc
//  file-create-date : Fri 17-Oct-2008 14:41 UTC
//  file-initiator   : Robbie Morrison <robbie@actrix.co.nz>
//
//  file-role        : operations OSPs for technical assets / implementation
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
//  $URL: file:///home/robbie/svn-root/xeona/futz/trunk/xeona1/b/optops1.cc $

//  LOCAL AND SYSTEM INCLUDES

#include "optops1.h"          // companion header for this file (place first)

#include "../c/label.h"       // helper class to format solver labels
#include "../c/costs.h"       // cost sets and support
#include "../c/util3.h"       // free functions for floating point comparison
#include "../c/util2.h"       // free functions which offer general utilities 2

#include "../a/logger.h"      // standard logging functionality (as required)
#include "../c/smart_ptr.h"   // toggle between Boost and TR1 smart pointers
#include ".././common.h"      // common definitions for project (place last)

#include <limits>             // numeric_limits<T>::infinity() and similar
#include <string>             // C++ strings
#include <sstream>            // string-streams

#include <cmath>              // C-style maths, ceil(), floor(), sqrt()

#include <boost/format.hpp>   // printf style formatting
#include <boost/ref.hpp>      // pass references to STL algorithms and such

//  CODE

// ---------------------------------------------------------
//  notes           : Operations optimization sub-problems
// ---------------------------------------------------------
//
//  Design notes generally applicable to operations OSPs
//
//      Use of input and output balances
//
//          Setting up explicit balances to handle input and
//          output is clearly redundant for a one blockette OSP.
//
//          Notwithstanding, this practice should be honored
//          because the resulting code will be easier to read,
//          maintain, and extend if written in a consistent way.
//
//      Return capture variables
//
//          The following base class 'OptimSubProb' variables may
//          be used to capture push returns (and similar) in
//          statements where no subsequent usage is envisaged:
//
//              d_colTrack
//              d_rowTrack
//              d_cofCount
//
//      Separation of engineering and specific costs uploads
//
//          For most OSPs, the upload of specific costs and of
//          the engineering characterizations are separated and
//          should be processed in this order:
//
//              uploadEngineering   - engineering characterization
//              uploadShortrunCosts - specific cost data (optional)
//
//          As indicated, 'OperationsOsp::uploadShortrunCosts' is
//          normally relied upon for loading specific costs, but
//          its use is optional -- in which case the
//          'OperationsOsp::uploadEngineering' default (zero by
//          convention) will be employed.
//
//          More complex designs are possible but they would
//          require both aspects to be treated in the same
//          routine.  In addition, the user-supplied model data
//          overhead would need to increase as a result.
//
//      Duty
//
//          The definition of duty is left to the host entity
//          (and perhaps ultimately the modeler), as is the
//          explicit coupling of duty between the technical asset
//          and the asset operator.  This is an important
//          concept!
//
//          For a transformation block entity with just one
//          output stream, duty invariably equates to that
//          output.  In other words, the concept of elected duty
//          only becomes a model choice issue for multi-product
//          assets (such as combined heat and power).
//
//      Boost tuples and 'boost::make_tuple' usage
//
//          By default, 'boost::make_tuple' holds non-const,
//          non-reference values -- even if the argument in
//          question is a reference or a const reference.
//
//          In this file, we may want to hold const references in
//          tuples -- which requires the use of 'boost::cref'
//          from header <boost/ref.hpp>.
//
//          See Karlsson (2006 pp216-217) for more details on
//          'cref.'  Becker (2007 pp3-22) also covers tuples.
//
//      Standard labels for use in 'uploadEngineering' definitions
//
//          OSP authors should deploy the following labels where
//          appropriate:
//
//          // initial reporting
//          // PREAMBLE
//          // INTEGRITY CHECKS
//          // EXPOSED VARIABLES
//          // FACTOR AND OUTPUT BALANCES
//          // BLOCKETTES, etc
//          // GLOBALIZE AND RETURN
//
// ---------------------------------------------------------

// ---------------------------------------------------------
//  CLASS           : OpsDcTrans_A (DC transmission)
// ---------------------------------------------------------

// CREATORS

// ---------------------------------------------------------
//  MEMBER FUNCTION : OpsDcTrans_A
// ---------------------------------------------------------

OpsDcTrans_A::OpsDcTrans_A
(shared_ptr<svif::SolverIf> solver,
 const xeona::DomainMode    commitmentMode) :
  OperationsOsp(solver, commitmentMode, "ops-transmission-a"),
  d_cols(),                                  // tuple ctor calls default ctors
  d_injectCapacity(0.0)
{
  s_logger->repx(logga::adhc, "constructor call, description", d_ospDesc);
}

// ---------------------------------------------------------
//  MEMBER FUNCTION : ~OpsDcTrans_A
// ---------------------------------------------------------

OpsDcTrans_A::~OpsDcTrans_A()
{
  s_logger->repx(logga::adhc, "destructor call, description", d_ospDesc);
}

// FILL PROBLEM CALLS

// ---------------------------------------------------------
//  MEMBER FUNCTION : uploadEngineering (pre-processing)
// ---------------------------------------------------------
//  Description  : problem building, higher-level wrapper to principal call
//  Role         : host call, convenience
//  Techniques   : physics
//  Status       : complete
//
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
//     Ploss/P = ---      and at capacity P = V.Icap
//                V^2
//
//  In passing, note also that a fixed frequency AC power flow
//  model has been developed for LMP pricing, but this is much
//  more complicated.  For instance, see work by University of
//  New South Wales, Australia.
//
//  Typical specific resistance values
//
//      voltage      ohms/m
//      -------------------
//      500kV        30e-06
//      230kV       100e-06
//      115kV       170e-06
//
//  Source: Phillips, Drew.  2004.  Nodal pricing basics -- OHPs.
//  IMO independent electricity market operator.
//  LMP_NodalBasics_2004jan14.pdf [file downloaded from web in
//  Feb-2008].  Reverse engineered from slide 12.  See also my
//  OpenOffice spreadsheet (*line-losses*.ods).
//
// ---------------------------------------------------------

OpsDcTrans_A::index_type                     // global cols
OpsDcTrans_A::uploadEngineering
(const double injectCapacity,                // in unprefixed SI units [W]
 const double voltage,                       // in unprefixed SI units [V]
 const double ohmsPerMetre,                  // in unprefixed SI units [ohm/m]
 const double length,                        // in unprefixed SI units [m]
 const int    discretization)                // number of discretization steps
{
  // initial reporting
  s_logger->repx(logga::xtra, "entering member function", "");

  // process arguments
  const double ohms         = ohmsPerMetre * length;        // DC resistance
  const double slope        = ohms / std::pow(voltage, 2);  // R/V^2, refer <cmath>
  const double maxRelLosses = slope * injectCapacity;       // y upper (y lower is zero)

  // integrity checks
  const double trip = 0.3;                   // 30% losses
  if ( maxRelLosses >= 1.0 )
    {
      // could also check to see if voltage drop exceeds input voltage
      s_logger->repx(logga::warn, "aphysical line characterization", "");
      const std::string func = XEONA_FUNC;   // preprocessor macro defined in 'common.h'
      std::ostringstream put;
      put << "  line characterization aphysical (+ indicates primary data)"       << "\n"
          << "    relative loss at capacity exceeds unity (undersized conductor)" << "\n"
          << "      function             : " << func                              << "\n"
          << "      label                : " << d_label                           << "\n"
          << "    + discretization steps : " << discretization                    << "\n"
          << "    + inject capacity      : " << injectCapacity                    << "\n"
          << "    + voltage              : " << voltage                           << "\n"
          << "    + ohms per metre       : " << ohmsPerMetre                      << "\n"
          << "    + length               : " << length                            << "\n"
          << "      max rel losses       : " << maxRelLosses                      << "\n";
      s_logger->putx(logga::dbug, put);
    }
  else if ( maxRelLosses >= trip )
    {
      s_logger->repx(logga::rankJumpy, "poor line characterization, trip", trip);
      const std::string func = XEONA_FUNC;   // preprocessor macro defined in 'common.h'
      std::ostringstream put;
      put << "  line characterization poor (+ indicates primary data)"            << "\n"
          << "    relative loss at capacity exceeds "
          << trip
          <<  " (undersized conductor)"                                           << "\n"
          << "      function             : " << func                              << "\n"
          << "      label                : " << d_label                           << "\n"
          << "    + discretization steps : " << discretization                    << "\n"
          << "    + inject capacity      : " << injectCapacity                    << "\n"
          << "    + voltage              : " << voltage                           << "\n"
          << "    + ohms per metre       : " << ohmsPerMetre                      << "\n"
          << "    + length               : " << length                            << "\n"
          << "      max rel losses       : " << maxRelLosses                      << "\n";
      s_logger->putx(logga::dbug, put);
    }

  // call principal function
  return uploadEngineering(injectCapacity,
                           maxRelLosses,
                           discretization);
}

// ---------------------------------------------------------
//  MEMBER FUNCTION : uploadEngineering (principal call)
// ---------------------------------------------------------
//  Description  : engineering characterization
//  Role         : host call
//  Techniques   : step-wise discretization
//  Status       : complete
// ---------------------------------------------------------

OpsDcTrans_A::index_type                     // global cols
OpsDcTrans_A::uploadEngineering
(const double injectCapacity,                // input capacity
 const double maxRelLosses,                  // relative losses at capacity
 const int    discretization)                // number of discretization steps
{
  // initial reporting
  s_logger->repx(logga::xtra, "entering member function", "");

  // PREAMBLE

  // create and fill a label object
  Label lab(d_label);

  // create a stand-in specific costs cost set
  const CostSet zeroSpecCosts(0.0);

  // set persistent variables (from this class 'OpsDcTrans_A')
  d_injectCapacity = injectCapacity;

  // process arguments (note the staircase analogy)
  const double tread = injectCapacity / discretization;     // x-axis discretization
  const double riser = maxRelLosses   / discretization;     // y-axis discretization

  // EXPOSED VARIABLES

  // create two variables covering input and output -- the zero
  // specific costs can later be rectified by
  // 'uploadShortrunCosts' but, in all likelihood, will simply
  // remain zero
  const int cabCol = pushObj(zeroSpecCosts, lab.str("cable-bi"));
  const int socCol = pushObj(zeroSpecCosts, lab.str("socket-bi"));

  // load the local indexes into a 2-tuple
  d_cols = boost::make_tuple(cabCol,
                             socCol);

  // CABLE AND SOCKET BALANCES

  // bidirectionalize the cable and socket
  int bndChanges = -1;                       // nonsensical value
  bndChanges     = openBnds(cabCol);
  bndChanges     = openBnds(socCol);

  // create cable and socket balances
  const int cabRow = pushRhs(0.0, svif::E, lab.str("cable-bal"));
  const int socRow = pushRhs(0.0, svif::E, lab.str("socket-bal"));

  // add the exposed variables
  d_cofCount = pushCof(cabRow, cabCol, +1.0);
  d_cofCount = pushCof(socRow, socCol, -1.0);

  // FORWARD BLOCKETTES

  double loss   = 0;                         // normalized loss, y-value
  double lower  = 0;                         // current lower capacity, x-value
  double upper  = 0;                         // current upper capacity, x-value

  for ( int i = 1;                           // loop the blockettes
        i    <= discretization;
        ++i )
    {
      loss  = (i - 0.5) * riser;             // forms 1/2, 3/2, 5/2, ..
      lower = 0.0;
      upper = tread;

      lab << boost::format("%s%02d") % "fwd" % i;      // "fwd00"

      // two blockette variables
      const int  inCol = pushObj(0.0, lab.str("in"));
      const int outCol = pushObj(0.0, lab.str("out"));

      // connect to the input and output balances
      d_cofCount = pushCof(cabRow,  inCol, -1.0);
      d_cofCount = pushCof(socRow, outCol, +1.0);

      // define the lower and upper constraints on input
      const int lowerRow = pushRhs(lower, svif::G, lab.str("lower"));
      const int upperRow = pushRhs(upper, svif::L, lab.str("upper"));
      d_cofCount         = pushCof(lowerRow, inCol, +1.0);
      d_cofCount         = pushCof(upperRow, inCol, +1.0);

      // define the input/output equality
      const int iorRow = pushRhs(0.0, svif::E, lab.str("ior"));
      d_cofCount       = pushCof(iorRow,  inCol, +1.0 - loss);
      d_cofCount       = pushCof(iorRow, outCol, -1.0);

      lab.trim(1);                           // remove the last labelette
    }

  // BACKWARD BLOCKETTES

  loss   = 0;                                // reset
  lower  = 0;                                // reset
  upper  = 0;                                // reset

  for ( int i = 1;                           // loop the blockettes
        i    <= discretization;
        ++i )
    {
      loss  = (i - 0.5) * riser;             // forms 1/2, 3/2, 5/2, ..
      lower = 0.0;
      upper = tread;

      lab << boost::format("%s%02d") % "bak" % i;      // "bak00"

      // two blockette variables
      const int inCol  = pushObj(0.0, lab.str("in"));
      const int outCol = pushObj(0.0, lab.str("out"));

      // connect to the input and output balances
      d_cofCount = pushCof(cabRow, outCol, +1.0);
      d_cofCount = pushCof(socRow,  inCol, -1.0);

      // define the lower and upper constraints on input
      const int lowerRow = pushRhs(lower, svif::G, lab.str("lower"));
      const int upperRow = pushRhs(upper, svif::L, lab.str("upper"));
      d_cofCount         = pushCof(lowerRow, inCol, +1.0);
      d_cofCount         = pushCof(upperRow, inCol, +1.0);

      // define the input/output equality
      const int iorRow = pushRhs(0.0, svif::E, lab.str("ior"));
      d_cofCount       = pushCof(iorRow,  inCol, 1.0 - loss);
      d_cofCount       = pushCof(iorRow, outCol, -1.0);

      lab.trim(1);                           // remove the last labelette
    }

  // additional reporting as appropriate
  // YEEK 24 CODE (set by '--yeek')
  if ( xeona::yeek == 24 || xeona::yeek == 1 || xeona::yeek == 2 )
    {
      s_logger->repx(logga::dbug, "additional reporting follows, yeek", xeona::yeek);
      const std::string func = XEONA_FUNC;   // a preprocessor macro
      reportBuildIntegrity(logga::dbug, func + " (principal call)");
    }

  // GLOBALIZE AND RETURN

  // note that external usage requires global indexing
  return globalcols(d_cols);

} // function 'OpsDcTrans_A::uploadEngineering'

// DOWNLOAD SOLUTION CALLS

// ---------------------------------------------------------
//  MEMBER FUNCTION : downloadSolution
// ---------------------------------------------------------
//  Description  : solution recovery
//  Role         : host call
//  Techniques   : directionality calculations
//  Status       : complete
// ---------------------------------------------------------

OpsDcTrans_A::results_type
OpsDcTrans_A::downloadSolution() const
{
  // initial reporting
  s_logger->repx(logga::xtra, "entering member function", "");

  // identify transmission results
  const double cab = downloadVar(d_cols.get<0>());     // bidirectional
  const double soc = downloadVar(d_cols.get<1>());     // bidirectional

  // integrity check
  if ( (cab * soc) < 0.0 )                   // one and only one is negative
    {
      s_logger->repx(logga::warn, "integrity check failure", "unmatched signs");
      std::ostringstream put;
      put << "  cable flow  : " << std::showpos << cab << "\n"
          << "  socket flow : " << std::showpos << soc << "\n";
      s_logger->putx(logga::xtra, put);
    }

  // process secondary quantities
  const bool direction = cab < 0 ? false : true;
  double inject = -1.0;                      // current injection, always positive
  double exit   = -1.0;                      // current exit, always positive
  if ( direction == true )                   // positive commodity flow encountered
    {
      inject = +cab;
      exit   = +soc;
    }
  else                                       // negative commodity flow encountered
    {
      inject = -soc;
      exit   = -cab;
    }

  // calculate with div-by-zero protection
  double relativeDuty = 0.0;
  double relativeLoss = 0.0;
  if ( d_injectCapacity != 0.0 ) relativeDuty = inject / d_injectCapacity;
  if ( inject           != 0.0 ) relativeLoss = (inject - exit) / inject;

  // additional reporting as appropriate
  // YEEK 11 CODE (set by '--yeek')
  if ( xeona::yeek == 11 || xeona::yeek == 1 || xeona::yeek == 2 )
    {
      std::ostringstream put;
      put << "  transmission OSP values:"                                  << "\n"
          << "    cable         : " << cab                                 << "\n"
          << "    socket        : " << soc                                 << "\n"
          << "    inject flow   : " << inject                              << "\n"
          << "    exit flow     : " << exit                                << "\n"
          << "    relative duty : " << relativeDuty                        << "\n"
          << "    relative loss : " << relativeLoss                        << "\n";
      s_logger->repx(logga::dbug, "additional reporting follows, yeek", xeona::yeek);
      s_logger->putx(logga::dbug, put);
    }

  // return
  return boost::make_tuple(direction,
                           cab,
                           soc,
                           relativeDuty,
                           relativeLoss);

} // function 'downloadSolution'

// ---------------------------------------------------------
//  CLASS           : OpsFac1Out1_A (shutdown mode)
// ---------------------------------------------------------

// CREATORS

// ---------------------------------------------------------
//  MEMBER FUNCTION : OpsFac1Out1_A
// ---------------------------------------------------------

OpsFac1Out1_A::OpsFac1Out1_A
(shared_ptr<svif::SolverIf> solver,
 const xeona::DomainMode    commitmentMode) :
  OperationsOsp(solver, commitmentMode, "ops-f1o1-shutdown-a"),
  d_scaleFactor(1.0),                        // default value
  d_cols()                                   // tuple ctor calls default ctors
{
  s_logger->repx(logga::adhc, "constructor call, description", d_ospDesc);
}

// ---------------------------------------------------------
//  MEMBER FUNCTION : ~OpsFac1Out1_A
// ---------------------------------------------------------

OpsFac1Out1_A::~OpsFac1Out1_A()
{
  s_logger->repx(logga::adhc, "destructor call, description", d_ospDesc);
}

// FILL PROBLEM CALLS

// ---------------------------------------------------------
//  MEMBER FUNCTION : uploadEngineering (overloaded)
// ---------------------------------------------------------
//  Description  : engineering characterization
//  Role         : host call
//  Techniques   : binary variable for shutdown mode, output dimension scaling
//  Status       : complete
//
//  Design notes
//
//      Shutdown mode operation.
//
//          This OSP supports shutdown mode operation, as tripped
//          by the 'prodLoBound' value.  It also employs constant
//          marginal efficiency.  See my (Robbie Morrison) PhD
//          thesis for the underlying equations.
//
//      kg-to-J wrapper
//
//          Note this wrapper which also requires mass-specific
//          enthalpy data.
//
//          Because the mass-specific enthalpy of combustion can
//          be in the order of 1.0e+08, the caller must offer a
//          'scaleFactor' in order to produce a better
//          conditioned constraint matrix.
//
//      Abnormal conditions
//
//          As an extension, one could relax the trip condition
//          for times when the original "tripped" problem is
//          infeasible.  This is not currently done.
//
// ---------------------------------------------------------

// kg-to-J wrapper

OpsFac1Out1_A::index_type                    // global cols
OpsFac1Out1_A::uploadEngineering             // kg-to-J form
(const double prodLoBound,                   // lower output below which the asset trips
 const double prodHiBound,                   // upper output
 const double marginalEfficiency,            // slope (as decimal not percentage)
 const double fuelNoload,                    // fuel usage on idle (input-axis intercept)
 const double fuelAncillary,                 // fuel usage on shutdown (for ancillaries)
 const double rampLoSize,                    // ramp down restriction ('inf' to ignore)
 const double rampHiSize,                    // ramp up restriction ('inf' to ignore)
 const double specEnthalpy,                  // kg->J conversion
 const double scaleFactor)                   // for a better conditioned constraint matrix
{
  // initial reporting
  s_logger->repx(logga::xtra, "entering member function", "kg-to-J form");

  // for debugging
  s_logger->repx(logga::dbug, "prodHiBound value", prodHiBound);

  // INTEGRITY CHECKS

  // very basic range checking -- with both zero and unity
  // efficiency being valid (also caters for most cases when a
  // percentage is mistakenly used)
  if ( marginalEfficiency < 0.0 || marginalEfficiency > 1.0 )
    {
      s_logger->repx(logga::rankJumpy,
                     "aphysical marginal efficiency",
                     marginalEfficiency);
    }
  // HHV-based specific enthalpy typically ranges from 6e06 for
  // wet peat to 142e06 for hydrogen -- delete or modify
  // following values as appropriate
  if ( specEnthalpy < 5.0e06 || specEnthalpy > 145.0e06)
    {
      s_logger->repx(logga::rankJumpy,
                     "surprising specific enthalpy value",
                     specEnthalpy);
    }

  // PRINCIPAL CALL

  // revise scaling factor, construction-time default is 1.0;
  d_scaleFactor = scaleFactor;

  // under the current data specification, the last two arguments
  // do not need to be be multiplied by 'specEnthalpy'
  return uploadEngineering(prodLoBound,
                           prodHiBound,
                           marginalEfficiency * specEnthalpy,
                           fuelNoload         / specEnthalpy,
                           fuelAncillary      / specEnthalpy,
                           rampLoSize,
                           rampHiSize);

} // function 'uploadEngineering' (wrapper)

// J-to-J form

OpsFac1Out1_A::index_type                    // global cols
OpsFac1Out1_A::uploadEngineering             // J-to-J form
(const double prodLoBound,                   // lower output below which the asset trips
 const double prodHiBound,                   // upper output
 const double marginalEfficiency,            // slope (as decimal not percentage)
 const double fuelNoload,                    // fuel usage on idle (input-axis intercept)
 const double fuelAncillary,                 // fuel usage on shutdown (for ancillaries)
 const double rampLoSize,                    // ramp down restriction ('inf' to ignore)
 const double rampHiSize)                    // ramp up restriction ('inf' to ignore)
{
  // initial reporting
  s_logger->repx(logga::xtra, "entering member function", "J-to-J form");

  // for debugging
  s_logger->repx(logga::dbug, "prodHiBound value", prodHiBound);

  // PREAMBLE

  // create and fill a label object
  Label lab(d_label);

  // create a stand-in specific costs cost set
  const CostSet zeroSpecCosts(0.0);

  // INTEGRITY CHECKS

  // very basic range checking, noting that the marginal
  // efficiency may be multiplied by the specific enthalpy prior
  // to this call
  const double threshold = 0.000001;
  if ( marginalEfficiency < 0.0 )
    {
      s_logger->repx(logga::rankJumpy,
                     "aphysical marginal efficiency",
                     marginalEfficiency);
    }
  else if ( marginalEfficiency == 0 )
    {
      s_logger->repx(logga::warn,
                     "zero marginal efficiency",
                     marginalEfficiency);
      std::ostringstream put;
      put << "  GLPK will not accept a div-by-zero -inf (infinity) and"
        " will shortly error out, check the code in the calling entity"    << "\n"
          << "  note that optimization sub-problem 'OpsFixedEffy' will"
        " accept zero efficiency"                                          << "\n";
      s_logger->putx(logga::dbug, put);
    }
  else if ( marginalEfficiency < threshold )
    {
      s_logger->repx(logga::warn,
                     "tiny marginal efficiency",
                     marginalEfficiency);
    }

  // advisory message
  if ( fuelAncillary > fuelNoload )
    {
      std::ostringstream oss;
      oss << fuelAncillary << " : " << fuelNoload;
      s_logger->repx(logga::rankJumpy,
                     "advise fuel ancillary > fuel noload",
                     oss.str());
    }

  // informational message
  if ( d_scaleFactor != 1.0 )
    {
      s_logger->repx(logga::adhc, "revised scale factor in use", d_scaleFactor);
    }

  // EXPOSED VARIABLES

  // create two variables covering input and output -- the zero
  // specific costs can later be rectified by 'uploadShortrunCosts'
  const int fac1Col  = pushObj(zeroSpecCosts, lab.str("factor"));
  const int out1Col  = pushObj(zeroSpecCosts, lab.str("output"));

  // create a binary variable covering shutdown mode, whereby
  // value 0 (implicit 'false') = shutdown
  const int tripCol  = pushObj(zeroSpecCosts, lab.str("trip-if-zero"));
  d_colTrack         = markBinary(tripCol);

  // load the local col indexes into a 3-tuple
  d_cols = boost::make_tuple(fac1Col,
                             out1Col,
                             tripCol);

  // FACTOR AND OUTPUT BALANCES

  // create input and output balances
  const int fac1Row   = pushRhs(0.0, svif::E, lab.str("factor-bal"));
  const int out1Row   = pushRhs(0.0, svif::E, lab.str("output-bal"));

  // add the exposed variables
  d_cofCount          = pushCof(fac1Row, fac1Col, +1.0);
  d_cofCount          = pushCof(out1Row, out1Col, -1.0);

  // BLOCKETTE IO
  // note that blockette Io is the sole blockette in this OSP

  // create in and out variables for blockette Io
  const int  inIoCol  = pushObj(zeroSpecCosts, lab.str("block-io-in"));
  const int outIoCol  = pushObj(zeroSpecCosts, lab.str("block-io-out"));

  // connect to the input and output balances
  d_cofCount          = pushCof(fac1Row,  inIoCol, -1.0);
  d_cofCount          = pushCof(out1Row, outIoCol, +d_scaleFactor);

  // define the input/output equality for blockette Io
  const int effRow    = pushRhs(fuelAncillary, svif::E, lab.str("scaled-efficiency"));
  d_cofCount          = pushCof(effRow,  inIoCol, +1.0);
  d_cofCount          = pushCof(effRow, outIoCol, -d_scaleFactor / marginalEfficiency);
  d_cofCount          = pushCof(effRow,  tripCol, fuelAncillary - fuelNoload);

  // define the lo and hi bounds for blockette Io -- note that
  // these calls are  complicated by the shutdown mode logic
  const int loRow     = pushRhs(0.0, svif::G, lab.str("lo-bound"));
  d_cofCount          = pushCof(loRow,  tripCol, -prodLoBound / d_scaleFactor);
  d_cofCount          = pushCof(loRow, outIoCol, +1.0);

  const int hiRow     = pushRhs(0.0, svif::L, lab.str("hi-bound"));
  d_cofCount          = pushCof(hiRow,  tripCol, -prodHiBound / d_scaleFactor);
  d_cofCount          = pushCof(hiRow, outIoCol, +1.0);

  // add the ramp rate restrictions as required
  if ( ! std::isinf(rampLoSize) )            // refer <cmath>
    {
      const std::string sLo = "lo-ramp-restraint";
      const int rampLoRow   = pushRhs(rampLoSize / d_scaleFactor, svif::G, lab.str(sLo));
      d_cofCount            = pushCof(rampLoRow, outIoCol, +1.0);
    }
  if ( ! std::isinf(rampHiSize) )
    {
      const std::string sHi = "hi-ramp-restraint";
      const int rampHiRow   = pushRhs(rampHiSize / d_scaleFactor, svif::L, lab.str(sHi));
      d_cofCount            = pushCof(rampHiRow, outIoCol, +1.0);
    }

  // YEEK 24 CODE (set by '--yeek')
  if ( xeona::yeek == 24 || xeona::yeek == 1 || xeona::yeek == 2 )
    {
      s_logger->repx(logga::dbug, "additional reporting follows, yeek", xeona::yeek);
      const std::string func = XEONA_FUNC;   // a preprocessor macro
      reportBuildIntegrity(logga::dbug, func + " (J-to-J call)");
    }

  // GLOBALIZE AND RETURN

  // note that external usage requires global indexing
  return globalcols(d_cols);

} // function 'uploadEngineering' (workhorse)

// DOWNLOAD SOLUTION CALLS

// ---------------------------------------------------------
//  MEMBER FUNCTION : downloadSolution
// ---------------------------------------------------------
//  Description  : solution recovery
//  Role         : host call
//  Techniques   : 'boost::make_tuple'
//  Status       : complete
// ---------------------------------------------------------

OpsFac1Out1_A::results_type
OpsFac1Out1_A::downloadSolution() const
{
  // YEEK 15 CODE (set by '--yeek')
  if ( xeona::yeek == 15 || xeona::yeek == 1 || xeona::yeek == 2 )
    {
      s_logger->repx(logga::dbug, "entering member function", "");
      const int    gol = d_cols.get<2>();
      const double var = downloadVar(gol);
      const bool   bol = static_cast<bool>(var);
      std::ostringstream put;
      put << "  gol    : " << gol << "\n"
          << "  double : " << var << "\n"
          << "  bool   : " << bol << "\n"
          << std::boolalpha
          << "  bool   : " << bol << "\n";
      s_logger->putx(logga::dbug, put);
    }

  return boost::make_tuple(downloadVar(d_cols.get<0>()),
                           downloadVar(d_cols.get<1>()),
                           static_cast<bool>(downloadVar(d_cols.get<2>()))); // [1]

  // [1] a 'double' { 0.0, 1.0 } to 'bool' { 'false', 'true' } conversion

} // function 'downloadSolution'

// ---------------------------------------------------------
//  CLASS           : OpsFac0Out1_A (source)
// ---------------------------------------------------------

// CREATORS

// ---------------------------------------------------------
//  MEMBER FUNCTION : OpsFac0Out1_A
// ---------------------------------------------------------

OpsFac0Out1_A::OpsFac0Out1_A
(shared_ptr<svif::SolverIf> solver,
 const xeona::DomainMode    commitmentMode) :
  OperationsOsp(solver, commitmentMode, "ops-f0o1-a"),
  d_cols()                                   // tuple ctor calls default ctors
{
  s_logger->repx(logga::adhc, "constructor call, description", d_ospDesc);
}

// ---------------------------------------------------------
//  MEMBER FUNCTION : ~OpsFac0Out1_A
// ---------------------------------------------------------

OpsFac0Out1_A::~OpsFac0Out1_A()
{
  s_logger->repx(logga::adhc, "destructor call, description", d_ospDesc);
}

// FILL PROBLEM CALLS

// ---------------------------------------------------------
//  MEMBER FUNCTION : uploadEngineering
// ---------------------------------------------------------
//  Description  : engineering characterization
//  Role         : host call
//  Techniques   : (nothing special)
//  Status       : complete
// ---------------------------------------------------------

OpsFac0Out1_A::index_type
OpsFac0Out1_A::uploadEngineering
(const double prodLoBound,                   // lower output (typically zero)
 const double prodHiBound)                   // upper output
{
  // initial reporting
  s_logger->repx(logga::xtra, "entering member function", "");

  // PREAMBLE

  // create and fill a label object
  Label lab(d_label);                        // entity identifier

  // create a stand-in specific costs cost set
  const CostSet zeroSpecCosts(0.0);

  // INTEGRITY CHECKS

  // very basic integrity checking
  if ( prodLoBound > prodHiBound ||
       prodLoBound < 0.0 )
    {
      std::ostringstream oss;
      oss << prodLoBound << " : " << prodHiBound;
      s_logger->repx(logga::warn, "aphysical factor bounds, lo : hi", oss.str());
    }

  // EXPOSED VARIABLES

  // create one variable covering output (and thus the only
  // choice on offer for duty) -- the zero specific costs can
  // later be rectified by 'uploadShortrunCosts'
  const int prodCol = pushObj(zeroSpecCosts, lab.str("output"));

  // load the local index into a 1-tuple
  d_cols            = boost::make_tuple(prodCol);

  // OUTPUT BALANCE

  // create an output balance
  const int outRow  = pushRhs(0.0, svif::E, lab.str("output-bal"));

  // add the exposed variable
  d_cofCount        = pushCof(outRow, prodCol, -1.0);

  // SOURCE BLOCKETTE

  // note that the source blockette is the sole blockette in this
  // OSP -- now create an out variable for the source blockette
  const int outCol  = pushObj(zeroSpecCosts, lab.str("source-out"));

  // connect to the output balance
  d_cofCount        = pushCof(outRow, outCol, +1.0);

  // define the input/output equality for the source blockette
  // (not required)

  // define the lo and hi bounds for the source blockette
  const int loRow   = pushRhs(prodLoBound, svif::G, lab.str("lo-bound"));
  d_cofCount        = pushCof(loRow, outCol, +1.0);

  const int hiRow   = pushRhs(prodHiBound, svif::L, lab.str("hi-bound"));
  d_cofCount        = pushCof(hiRow, outCol, +1.0);

  // additional reporting as appropriate
  // YEEK 24 CODE (set by '--yeek')
  if ( xeona::yeek == 24 || xeona::yeek == 1 || xeona::yeek == 2 )
    {
      s_logger->repx(logga::dbug, "additional reporting follows, yeek", xeona::yeek);
      const std::string func = XEONA_FUNC;   // a preprocessor macro
      reportBuildIntegrity(logga::dbug, func + " (lo/hi output call)");
    }

  // GLOBALIZE AND RETURN

  // note that external usage requires global indexing
  return globalcols(d_cols);

} // function 'OpsFac0Out1_A::uploadEngineering'

// DOWNLOAD SOLUTION CALLS

// ---------------------------------------------------------
//  MEMBER FUNCTION : downloadSolution
// ---------------------------------------------------------
//  Description  : solution recovery
//  Role         : host call
//  Techniques   : 'boost::make_tuple'
//  Status       : complete
// ---------------------------------------------------------

OpsFac0Out1_A::results_type
OpsFac0Out1_A::downloadSolution() const
{
  // additional reporting
  const int col      = d_cols.get<0>();
  const double value = downloadVar(col);
  std::ostringstream oss;
  oss << "value for col " << col;
  s_logger->repx(logga::adhc, oss.str(), value);

  // main call
  return boost::make_tuple(downloadVar(d_cols.get<0>()));
}

// ---------------------------------------------------------
//  CLASS           : OpsFac1Out0_A (sink)
// ---------------------------------------------------------

// CREATORS

// ---------------------------------------------------------
//  MEMBER FUNCTION : OpsFac1Out0_A
// ---------------------------------------------------------

OpsFac1Out0_A::OpsFac1Out0_A
(shared_ptr<svif::SolverIf> solver,
 const xeona::DomainMode    commitmentMode) :
  OperationsOsp(solver, commitmentMode, "ops-f1o0-a"),
  d_cols()                                   // tuple ctor calls default ctors
{
  s_logger->repx(logga::adhc, "constructor call, description", d_ospDesc);
}

// ---------------------------------------------------------
//  MEMBER FUNCTION : ~OpsFac1Out0_A
// ---------------------------------------------------------

OpsFac1Out0_A::~OpsFac1Out0_A()
{
  s_logger->repx(logga::adhc, "destructor call, description", d_ospDesc);
}

// FILL PROBLEM CALLS

// ---------------------------------------------------------
//  MEMBER FUNCTION : uploadEngineering (overloaded, one)
// ---------------------------------------------------------
//  Description  : engineering characterization
//  Role         : host call
//  Techniques   : (nothing special)
//  Status       : complete
// ---------------------------------------------------------

OpsFac1Out0_A::index_type
OpsFac1Out0_A::uploadEngineering
(const double takeFixed)                     //  fixed input (can be zero)
{
  // initial reporting
  s_logger->repx(logga::xtra, "entering member function", "");

  // PREAMBLE

  // create and fill a label object
  Label lab(d_label);                        // entity identifier

  // create a stand-in specific costs cost set
  const CostSet zeroSpecCosts(0.0);

  // INTEGRITY CHECKS

  // very basic integrity checking
  if ( takeFixed < 0.0 )
    {
      s_logger->repx(logga::warn, "aphysical factor bound, fixed", takeFixed);
    }

  // EXPOSED VARIABLES

  // create one variable covering input (and thus the only choice
  // on offer for duty) -- the zero specific costs can later be
  // rectified by 'uploadShortrunCosts'
  const int fuelCol = pushObj(zeroSpecCosts, lab.str("factor"));

  // load the local index into a 1-tuple
  d_cols            = boost::make_tuple(fuelCol);

  // FACTOR BALANCE

  // create an input balance
  const int inRow   = pushRhs(0.0, svif::E, lab.str("factor-bal"));

  // add the exposed variable
  d_cofCount        = pushCof(inRow, fuelCol, -1.0);

  // SINK BLOCKETTE

  // note that the sink blockette is the sole blockette in this
  // OSP -- now create an in variable for the sink blockette
  const int sinkCol   = pushObj(zeroSpecCosts, lab.str("sink-in"));

  // connect to the input balance
  d_cofCount        = pushCof(inRow, sinkCol, +1.0);

  // define the input/output equality for the source blockette
  // (not required)

  // define the fixed bound for the source blockette
  const int fixRow  = pushRhs(takeFixed, svif::E, lab.str("fixed-bound"));
  d_cofCount        = pushCof(fixRow, sinkCol, +1.0);

  // YEEK 24 CODE (set by '--yeek')
  if ( xeona::yeek == 24 || xeona::yeek == 1 || xeona::yeek == 2 )
    {
      s_logger->repx(logga::dbug, "additional reporting follows, yeek", xeona::yeek);
      const std::string func = XEONA_FUNC;   // a preprocessor macro
      reportBuildIntegrity(logga::dbug, func + " (fixed take call)");
    }

  // GLOBALIZE AND RETURN

  // note that external usage requires global indexing
  return globalcols(d_cols);

} // function 'uploadEngineering'

// ---------------------------------------------------------
//  MEMBER FUNCTION : uploadEngineering (overloaded, two)
// ---------------------------------------------------------
//  Description  : engineering characterization
//  Role         : host call
//  Techniques   : (nothing special)
//  Status       : complete
// ---------------------------------------------------------

OpsFac1Out0_A::index_type
OpsFac1Out0_A::uploadEngineering
(const double takeLoBound,                   // lower input (typically zero)
 const double takeHiBound)                   // upper input
{
  // initial reporting
  s_logger->repx(logga::xtra, "entering member function", "");

  // PREAMBLE

  // create and fill a label object
  Label lab(d_label);                        // entity identifier

  // create a stand-in specific costs cost set
  const CostSet zeroSpecCosts(0.0);

  // INTEGRITY CHECKS

  // very basic integrity checking
  if ( takeLoBound < 0.0 )
    {
      s_logger->repx(logga::warn, "aphysical factor bound, lo", takeLoBound);
    }
  if ( takeLoBound > takeHiBound )
    {
      std::ostringstream oss;
      oss << takeLoBound << " : " << takeHiBound;
      s_logger->repx(logga::warn, "aphysical factor bounds, lo : hi", oss.str());
    }

  // EXPOSED VARIABLES

  // create one variable covering input (and thus the only choice
  // on offer for duty) -- the zero specific costs can later be
  // rectified by 'uploadShortrunCosts'
  const int inCol   = pushObj(zeroSpecCosts, lab.str("factor"));

  // load the local index into a 1-tuple
  d_cols            = boost::make_tuple(inCol);

  // FACTOR BALANCE

  // create an input balance
  const int inRow   = pushRhs(0.0, svif::E, lab.str("factor-bal"));

  // add the exposed variable
  d_cofCount        = pushCof(inRow, inCol, -1.0);

  // SINK BLOCKETTE

  // note that the sink blockette is the sole blockette in this
  // OSP -- now create an in variable for the sink blockette
  const int sinkCol = pushObj(zeroSpecCosts, lab.str("sink-in"));

  // connect to the input balance
  d_cofCount        = pushCof(inRow, sinkCol, +1.0);

  // define the input/output equality for the source blockette
  // (not required)

  // define the bounds for the sink blockette
  const int loRow   = pushRhs(takeLoBound, svif::G, lab.str("lo-bound"));
  d_cofCount        = pushCof(loRow, sinkCol, +1.0);
  const int hiRow   = pushRhs(takeHiBound, svif::L, lab.str("hi-bound"));
  d_cofCount        = pushCof(hiRow, sinkCol, +1.0);

  // YEEK 24 CODE (set by '--yeek')
  if ( xeona::yeek == 24 || xeona::yeek == 1 || xeona::yeek == 2 )
    {
      s_logger->repx(logga::dbug, "additional reporting follows, yeek", xeona::yeek);
      const std::string func = XEONA_FUNC;   // a preprocessor macro
      reportBuildIntegrity(logga::dbug, func + " (lo/hi take call)");
    }

  // GLOBALIZE AND RETURN

  // note that external usage requires global indexing
  return globalcols(d_cols);

} // function 'uploadEngineering'

// DOWNLOAD SOLUTION CALLS

// ---------------------------------------------------------
//  MEMBER FUNCTION : downloadSolution
// ---------------------------------------------------------
//  Description  : solution recovery
//  Role         : host call
//  Techniques   : 'boost::make_tuple'
//  Status       : complete
// ---------------------------------------------------------

OpsFac1Out0_A::results_type
OpsFac1Out0_A::downloadSolution() const
{
  return boost::make_tuple(downloadVar(d_cols.get<0>()));
}

// ---------------------------------------------------------
//  CLASS           : OpsFixedEffy_A (shutdown mode)
// ---------------------------------------------------------

// CREATORS

// ---------------------------------------------------------
//  MEMBER FUNCTION : OpsFixedEffy_A
// ---------------------------------------------------------

OpsFixedEffy_A::OpsFixedEffy_A
(shared_ptr<svif::SolverIf> solver,
 const xeona::DomainMode    commitmentMode) :
  OperationsOsp(solver, commitmentMode, "ops-f1o1-shutdown-a"),
  d_cols()                                   // tuple ctor calls default ctors
{
  s_logger->repx(logga::adhc, "constructor call, description", d_ospDesc);
}

// ---------------------------------------------------------
//  MEMBER FUNCTION : ~OpsFixedEffy_A
// ---------------------------------------------------------

OpsFixedEffy_A::~OpsFixedEffy_A()
{
  s_logger->repx(logga::adhc, "destructor call, description", d_ospDesc);
}

// FILL PROBLEM CALLS

// ---------------------------------------------------------
//  MEMBER FUNCTION : uploadEngineering
// ---------------------------------------------------------
//  Description  : engineering characterization
//  Role         : host call
//  Techniques   : (nothing special)
//  Status       : complete
//
//  Design notes
//
//      The 'fixedEfficiency' need not be dimensionless and hence
//      may exceed unity.
//
// ---------------------------------------------------------

OpsFixedEffy_A::index_type                   // global cols
OpsFixedEffy_A::uploadEngineering
  (const double outLoBound,                  // lower output (usually zero)
   const double outHiBound,                  // upper output
   const double fixedEfficiency,             // efficiency (as decimal not percentage)
   const double noLoadLoss)                  // no load loss (as absolute), note default
{
  // initial reporting
  s_logger->repx(logga::xtra, "entering member function", "");

  // INTEGRITY CHECKS

  // very basic range checking -- with both zero and unity
  // efficiency being valid (also caters for most cases when a
  // percentage is mistakenly used)

  if ( fixedEfficiency < 0.0 )
    {
      s_logger->repx(logga::rankJumpy, "negative fixed efficiency", fixedEfficiency);
    }
  if ( outLoBound < 0.0 )
    {
      s_logger->repx(logga::rankJumpy, "negative bound encountered, lo", outLoBound);
    }
  if ( outHiBound < 0.0 )
    {
      s_logger->repx(logga::rankJumpy, "negative bound encountered, hi", outHiBound);
    }
  if ( outLoBound > outHiBound )
    {
      std::ostringstream oss;
      oss << outLoBound << " : " << outHiBound;
      s_logger->repx(logga::rankJumpy, "impossible bounds, lo : hi", oss.str());
    }

  // PREAMBLE

  // create and fill a label object
  Label lab(d_label);

  // create a stand-in specific costs cost set
  const CostSet zeroSpecCosts(0.0);

  // EXPOSED VARIABLES

  // create two variables covering input and output -- the zero
  // specific costs can later be rectified by
  // 'uploadShortrunCosts' (but that is not needed this case)
  const int fac1Col   = pushObj(zeroSpecCosts, lab.str("input"));
  const int out1Col   = pushObj(zeroSpecCosts, lab.str("output"));

  // load the local col indexes into a 2-tuple
  d_cols = boost::make_tuple(fac1Col,
                             out1Col);

  // FACTOR AND OUTPUT BALANCES

  // create input and output balances
  const int fac1Row   = pushRhs(0.0, svif::E, lab.str("input-bal"));
  const int out1Row   = pushRhs(0.0, svif::E, lab.str("output-bal"));

  // add the exposed variables
  d_cofCount          = pushCof(fac1Row, fac1Col, +1.0);
  d_cofCount          = pushCof(out1Row, out1Col, -1.0);

  // IO BLOCKETTE
  // note that blockette Io is the sole blockette in this OSP

  // create in and out variables for blockette Io
  const int  inCol    = pushObj(zeroSpecCosts, lab.str("block-io-in"));
  const int outCol    = pushObj(zeroSpecCosts, lab.str("block-io-out"));

  // connect to the input and output balances
  d_cofCount          = pushCof(fac1Row,  inCol, -1.0);
  d_cofCount          = pushCof(out1Row, outCol, +1.0);

  // omit io entry if fixed efficiency is infinite, meaning this
  // particular factor is not required to create the product
  if ( xeona::isInf(fixedEfficiency) )
    {
      s_logger->repx(logga::rankJumpy, "infinite fixed efficiency", fixedEfficiency);
    }
  else
    {
      // define the input/output equality for blockette Io
      // note the no-load loss term 'noload', defined in terms of input
      const double noLoad = fixedEfficiency * noLoadLoss;  // input adjustment necessary
      const int    effRow = pushRhs(noLoad, svif::E, lab.str("efficiency"));
      d_cofCount          = pushCof(effRow,  inCol, +fixedEfficiency);
      d_cofCount          = pushCof(effRow, outCol, -1.0);
    }

  // define the lo and hi bounds for blockette Io
  const int loRow     = pushRhs(outLoBound, svif::G, lab.str("lo-bound"));
  d_cofCount          = pushCof(loRow, outCol, +1.0);

  const int hiRow     = pushRhs(outHiBound, svif::L, lab.str("hi-bound"));
  d_cofCount          = pushCof(hiRow, outCol, +1.0);

  // GLOBALIZE AND RETURN

  // note that external usage requires global indexing
  return globalcols(d_cols);

} // function 'OpsFixedEffy_A::uploadEngineering'

// DOWNLOAD SOLUTION CALLS

// ---------------------------------------------------------
//  MEMBER FUNCTION : downloadSolution
// ---------------------------------------------------------
//  Description  : solution recovery
//  Role         : host call
//  Techniques   : 'boost::make_tuple'
//  Status       : complete
// ---------------------------------------------------------

OpsFixedEffy_A::results_type
OpsFixedEffy_A::downloadSolution() const
{
  return boost::make_tuple(downloadVar(d_cols.get<0>()),
                           downloadVar(d_cols.get<1>()));

} // function 'OpsFixedEffy_A::downloadSolution'

// ---------------------------------------------------------
//  CLASS           : OpsFuelToCseq_A
// ---------------------------------------------------------

// CREATORS

// ---------------------------------------------------------
//  MEMBER FUNCTION : OpsFuelToCseq_A
// ---------------------------------------------------------

OpsFuelToCseq_A::OpsFuelToCseq_A
(shared_ptr<svif::SolverIf> solver,
 const xeona::DomainMode    commitmentMode) :
  OperationsOsp(solver, commitmentMode, "ops-fuel-cseq-a"),
  d_specCarbonDioxide(0.0),
  d_carbonCaptureRate(0.0),
  d_cols()                                   // tuple ctor calls default ctors
{
  s_logger->repx(logga::adhc, "constructor call, description", d_ospDesc);
}

// ---------------------------------------------------------
//  MEMBER FUNCTION : ~OpsFuelToCseq_A
// ---------------------------------------------------------

OpsFuelToCseq_A::~OpsFuelToCseq_A()
{
  s_logger->repx(logga::adhc, "destructor call, description", d_ospDesc);
}

// FILL PROBLEM CALLS

// ---------------------------------------------------------
//  MEMBER FUNCTION : uploadEngineering
// ---------------------------------------------------------
//  Description  : engineering characterization
//  Role         : host call
//  Techniques   : binary variable for shutdown mode
//  Status       : complete
//
//  Design notes
//
//      This implementation is a little circular in terms of
//      addtional rows and cols.  It could be reworked a little
//      but there is no pressing need.
//
// ---------------------------------------------------------

OpsFuelToCseq_A::index_type
OpsFuelToCseq_A::uploadEngineering
(const double cseqLoBound,
 const double cseqHiBound,
 const double specCarbonDioxide,
 const double carbonCaptureRate)
{
  // initial reporting
  s_logger->repx(logga::xtra, "entering member function", "");

  // SPECIAL

  // used in washup
  d_specCarbonDioxide = specCarbonDioxide;
  d_carbonCaptureRate = carbonCaptureRate;

  // PREAMBLE

  // create and fill a label object
  Label lab(d_label);                        // entity identifier

  // create a stand-in specific costs cost set
  const CostSet zeroSpecCosts(0.0);

  // INTEGRITY CHECKS

  // very basic integrity checking
  if ( cseqLoBound < 0.0 )
    {
      s_logger->repx(logga::warn, "aphysical cseq bound, lo", cseqLoBound);
    }
  if ( cseqLoBound > cseqHiBound )
    {
      std::ostringstream oss;
      oss << cseqLoBound << " : " << cseqHiBound;
      s_logger->repx(logga::warn, "aphysical cseq bounds, lo : hi", oss.str());
    }
  if ( specCarbonDioxide == 0.0 )
    {
      s_logger->repx(logga::rankJumpy,
                     "specific carbon dioxide is zero",
                     specCarbonDioxide);
    }
  else if ( specCarbonDioxide < 0.0 )
    {
      s_logger->repx(logga::warn,
                     "specific carbon dioxide is negative",
                     specCarbonDioxide);
    }
  if ( carbonCaptureRate == 0.0 )
    {
      s_logger->repx(logga::dbug, "zero carbon capture rate", carbonCaptureRate);
    }
  else if ( carbonCaptureRate < 0.0 )
    {
      s_logger->repx(logga::warn, "aphysical carbon capture rate", carbonCaptureRate);
    }

  // EXPOSED VARIABLES

  // create two variables covering input -- the zero specific
  // costs can later be rectified by 'uploadShortrunCosts'
  const int fuelCol  = pushObj(zeroSpecCosts, lab.str("fuel"));
  const int cseqCol  = pushObj(zeroSpecCosts, lab.str("cseq"));

  // load the local index into a 2-tuple
  d_cols             = boost::make_tuple(fuelCol,
                                        cseqCol);

  // FACTOR BALANCES

  // create two input factor balances
  const int fuelRow  = pushRhs(0.0, svif::E, lab.str("fuel-bal"));
  const int cseqRow  = pushRhs(0.0, svif::E, lab.str("cseq-bal"));

  // add the exposed variables
  d_cofCount         = pushCof(fuelRow, fuelCol, +1.0);
  d_cofCount         = pushCof(cseqRow, cseqCol, +1.0);

  // COUPLING BLOCKETTE

  // first determine the coupling parameter
  const double joint = carbonCaptureRate * specCarbonDioxide;

  // note that the coupling blockette is the sole blockette in
  // this OSP -- now create two in variables for the sink blockette
  const int in1Col   = pushObj(zeroSpecCosts, lab.str("joint-fuel"));
  const int in2Col   = pushObj(zeroSpecCosts, lab.str("joint-cseq"));

  // connect the input balances
  d_cofCount         = pushCof(fuelRow, in1Col, -1.0);
  d_cofCount         = pushCof(cseqRow, in2Col, -1.0);

  // define the input/output equality for the source blockette
  // (not required)

  // define the relationship between 'fuel' and 'cseq' or set
  // 'cseq' to zero
  if ( joint == 0.0 )
    {
      // report
      s_logger->repx(logga::adhc, "special \"zero-cseq\" constraint", joint);
      // simply set 'cseq' to zero
      const int jointRow = pushRhs(0.0, svif::E, lab.str("zero-cseq"));
      d_cofCount         = pushCof(jointRow, in2Col, +1.0);
    }
  else
    {
      // define the relationship between 'fuel' and 'cseq'
      const int jointRow = pushRhs(0.0, svif::E, lab.str("joint"));
      d_cofCount         = pushCof(jointRow, in1Col, +joint);
      d_cofCount         = pushCof(jointRow, in2Col, -1.0);
    }

  // define the bounds for the sink blockette
  const int loRow   = pushRhs(cseqLoBound, svif::G, lab.str("lo-bound"));
  d_cofCount        = pushCof(loRow, cseqCol, +1.0);

  if ( xeona::isInf(cseqHiBound) )
    {
      // do nothing (GLPK will not accept infs)
    }
  else
    {
      const int hiRow   = pushRhs(cseqHiBound, svif::L, lab.str("hi-bound"));
      d_cofCount        = pushCof(hiRow, cseqCol, +1.0);
    }

  // YEEK 47 CODE (set by '--yeek')
  if ( xeona::yeek == 47 || xeona::yeek == 1 || xeona::yeek == 2 )
    {
      s_logger->repx(logga::dbug, "additional reporting follows, yeek", xeona::yeek);
      const std::string func = XEONA_FUNC;   // a preprocessor macro
      reportBuildIntegrity(logga::dbug, func + " (lo/hi cseq call)");
    }

  // GLOBALIZE AND RETURN

  // note that external usage requires global indexing
  return globalcols(d_cols);

} // function 'OpsFuelToCseq_A::uploadEngineering'

// DOWNLOAD SOLUTION CALLS

// ---------------------------------------------------------
//  MEMBER FUNCTION : downloadSolution
// ---------------------------------------------------------
//  Description  : solution recovery
//  Role         : host call
//  Techniques   : 'boost::make_tuple', somewhat unusual in that calculations occur
//  Status       : complete
// ---------------------------------------------------------

OpsFuelToCseq_A::results_type
OpsFuelToCseq_A::downloadSolution() const
{
  const double fuelFlow = downloadVar(d_cols.get<0>());
  const double cseqFlow = downloadVar(d_cols.get<1>());
  const double co2eEmit = (1.0 - d_carbonCaptureRate) * d_specCarbonDioxide * fuelFlow;
  return boost::make_tuple(fuelFlow,
                           cseqFlow,
                           co2eEmit);

} // function 'OpsFuelToCseq_A::downloadSolution'

// ---------------------------------------------------------
//  CLASS           : OpsStore_A (with mode switching)
// ---------------------------------------------------------
//
//  Design notes
//
//      This class contains two modifiable constants.  The
//      'd_roundZeroTrip' was used during development with bad
//      code and is now effectively disabled.  It could probably
//      be removed.  The 'd_spillPenalty' is necessary -- read
//      the comments below for more information.
//
// ---------------------------------------------------------

// CREATORS

// ---------------------------------------------------------
//  MEMBER FUNCTION : OpsStore_A
// ---------------------------------------------------------

OpsStore_A::OpsStore_A
(shared_ptr<svif::SolverIf> solver,
 const xeona::DomainMode    commitmentMode) :
  OperationsOsp(solver, commitmentMode, "ops-store-a"),
  d_roundTripEffy(1.0),
  d_normalizedDecay(0.0),
  d_openingInventory(0.0),
  d_hiCapacity(0.0),
  d_interval(0),
  d_cols(),                                  // tuple ctor calls default ctors
  d_colsInternal(),
  // modifiable constants (should probably be client set)
  d_roundZeroTrip(xeona::zero0),             // see unit 'c/util3' [1]
  d_spillPenalty(1.0)                        // arbitrary penalty to prejudice spill [2]
{
  s_logger->repx(logga::adhc, "constructor call, description", d_ospDesc);
}

// [1] 'xeona::exact' to disable, 'xeona::zero0' for 1.0e-10
// [2] set at 1.0 for no good reason, anything that scales well should suffice

// ---------------------------------------------------------
//  MEMBER FUNCTION : ~OpsStore_A
// ---------------------------------------------------------

OpsStore_A::~OpsStore_A()
{
  s_logger->repx(logga::adhc, "destructor call, description", d_ospDesc);
}

// FILL PROBLEM CALLS

// ---------------------------------------------------------
//  MEMBER FUNCTION : uploadEngineering
// ---------------------------------------------------------
//  Description  : engineering characterization
//  Role         : host call
//  Techniques   : binary variable for mode toggle
//  Status       : complete
//
//  Design notes
//
//      The mode lockout is a bit tricky.  Please see my (Robbie
//      Morrison) PhD report.  The constraint matrix is either
//      8x8 or 9x8, the latter if the 'spillFlag' if 'false' to
//      prevent unwanted recharge from spilling.
//
//      This OSP uses the labels "inflow" and "outflow" in a
//      special way -- in this case, they refer to internal flows
//      with the inefficiency and decay split off.
//
//      On the programming side, this OSP is a little unusual
//      because it uses internal cols to transfer information to
//      the 'downloadSolution' code, namely the
//      non-externally-exposed variables "inflow" and "outflow".
//
//      Both the 'normalizedDecay' and the 'roundTripEffy' are
//      fully accounted for in this OSP.  No further tweaking is
//      required in the client code.  That said, the client needs
//      to calculate the normalized decay.  See function
//      'TeasSimpleStorage::constrain' code that assumes
//      exponential decay.
//
// ---------------------------------------------------------

OpsStore_A::index_type
OpsStore_A::uploadEngineering
(const bool   spillFlag,
 const double openingInventory,
 const double loCapacity,
 const double hiCapacity,
 const double rechargeBound,
 const double dischargeBound,
 const double roundTripEffy,
 const double normalizedDecay,
 const int    interval)
{
  // initial reporting
  s_logger->repx(logga::xtra, "entering member function", "");

  // INTEGRITY CHECKS

  // data integrity is intended to be the responsibility of the
  // client code -- however some checks can be useful

  if ( openingInventory > hiCapacity )
    {
      std::ostringstream oss;
      oss << openingInventory << " : " << hiCapacity;
      s_logger->repx(logga::rankJumpy, "opening inventory exceeds capacity", oss.str());
    }
  if ( roundTripEffy == 0.0 )                // div-by-zero problems will arise
    {
      s_logger->repx(logga::warn, "zero round trip efficiency", roundTripEffy);
    }
  else if ( roundTripEffy < 0.0 )            // 'std::sqrt' problems will arise
    {
      s_logger->repx(logga::warn, "negative round trip efficiency", roundTripEffy);
    }
  if ( normalizedDecay < 0.0 || normalizedDecay > 1.0 )
    {
      s_logger->repx(logga::warn, "normalized decay not [0,1]", normalizedDecay);
    }
  else if ( normalizedDecay > 0.1 )
    {
      s_logger->repx(logga::rankJumpy, "normalized decay above 10%", normalizedDecay);
    }

  // PREAMBLE

  // create and fill a label object
  Label lab(d_label);

  // create a 'spillPenalty' set to some arbitrary 'value' or to
  // zero, depending on 'spillFlag'
  const double  spillPenalty = spillFlag ? d_spillPenalty : 0.0;

  // create stand-in and spill specific costs cost set
  const CostSet zeroSpecCosts(0.0);
  const CostSet spillSpecCosts(spillPenalty);

  // update data member for use in 'downloadSolution'
  d_roundTripEffy    = roundTripEffy;
  d_normalizedDecay  = normalizedDecay;
  d_openingInventory = openingInventory;
  d_hiCapacity       = hiCapacity;
  d_interval         = interval;             // type 'int'

  // round down close-to-zeros (depending on the value of
  // 'd_roundZeroTrip' and in addition to 'xeona::numericalZero'
  // which is used by the solver interface)
  const bool opi0 = xeona::roundZero(d_openingInventory, d_roundZeroTrip);

  // miscellaneous
  const double period = static_cast<double>(interval);

  // INTEGRITY CHECKS

  // check capacities
  if ( loCapacity > hiCapacity )
    {
      std::ostringstream oss;
      oss << loCapacity << " " << hiCapacity;
      s_logger->repx(logga::warn, "invalid capacity bounds", oss.str());
    }
  if ( roundTripEffy < 0.0 || roundTripEffy > 1.0 )
    {
      s_logger->repx(logga::warn, "aphysical round-trip efficiency", roundTripEffy);
    }

  // EXPOSED VARIABLES

  // create two variables covering recharge and discharge -- the
  // zero specific costs can later be rectified by
  // 'uploadShortrunCosts'
  const int recCol  = pushObj(zeroSpecCosts, lab.str("recharge"));
  const int disCol  = pushObj(zeroSpecCosts, lab.str("discharge"));

  // create a binary variable covering the mode of charge,
  // whereby value 0 (implicit 'false') = discharge
  const int modeCol = pushObj(zeroSpecCosts, lab.str("discharge-if-zero"));
  d_colTrack        = markBinary(modeCol);

  // load the local col indexes into a 2-tuple
  d_cols = boost::make_tuple(recCol,
                             disCol);

  // INPUT AND OUTPUT BALANCES

  // calculate the required decay flow, later provided by the
  // 'outflow' -- also split the round trip efficiency for
  // subsequent use too
  const double decayFlow = (d_openingInventory * normalizedDecay) / period;
  const double splitEffy = std::sqrt(roundTripEffy);

  // create input and output balances
  const int recRow = pushRhs(+0.0, svif::E, lab.str("recharge-bal"));
  const int disRow = pushRhs(+0.0, svif::E, lab.str("discharge-bal"));

  // add the exposed variables and adjust for inefficiency
  d_cofCount       = pushCof(recRow, recCol, +splitEffy);
  d_cofCount       = pushCof(disRow, disCol, -1.0 / splitEffy);

  // BLOCKETTES
  // note that blockette Store is the sole blockette in this OSP

  // create in and out variables for blockette Store
  const int  inflowCol = pushObj(zeroSpecCosts,  lab.str("inflow"));
  const int outflowCol = pushObj(zeroSpecCosts,  lab.str("outflow"));
  const int   spillCol = pushObj(spillSpecCosts, lab.str("spillflow"));
  const int    recKCol = pushObj(zeroSpecCosts,  lab.str("recharge-decayflow"));
  const int    disKCol = pushObj(zeroSpecCosts,  lab.str("discharge-decayflow"));

  // load the local col indexes into a 2-tuple for internal use
  d_colsInternal = boost::make_tuple(inflowCol,
                                     outflowCol);

  // connect to the recharge and discharge balances (these are
  // normally called the input and output balances but not in
  // this OSP)
  d_cofCount           = pushCof(recRow,  inflowCol, -1.0);
  d_cofCount           = pushCof(recRow,   spillCol, -1.0);      // may be later zeroed
  d_cofCount           = pushCof(recRow,    recKCol, -1.0);

  d_cofCount           = pushCof(disRow, outflowCol, +1.0);
  d_cofCount           = pushCof(disRow,    disKCol, -1.0);

  // the inflow and outflow conditions require a binary variable
  // to prevent simultaneous recharge and discharge (admittedly a
  // little tricky) -- nonetheless the calculation of the
  // respective capacities is straightforward
  const double   inflowcap = (hiCapacity - d_openingInventory) / period;
  const double  outflowcap = d_openingInventory / period;

  const int      inRow = pushRhs(+0.0, svif::L, lab.str("inputs"));
  d_cofCount           = pushCof(inRow,     modeCol, -inflowcap);
  d_cofCount           = pushCof(inRow,   inflowCol, +1.0);

  const int     outRow = pushRhs(+outflowcap, svif::L, lab.str("outputs"));
  d_cofCount           = pushCof(outRow,    modeCol, +outflowcap);
  d_cofCount           = pushCof(outRow, outflowCol, +1.0);

  // apply decay flow condition using the same binary variable
  // used above -- note that if 'decayFlow' is zero, the OSP will
  // omit the coefficient altogether, leading to a correct and
  // trivial definitions of zero for the relevant variables
  const int    recKRow = pushRhs(+0.0, svif::E, lab.str("recharge-decay-bal"));
  d_cofCount           = pushCof(recKRow, modeCol, -decayFlow);
  d_cofCount           = pushCof(recKRow, recKCol, +1.0);

  const int    disKRow = pushRhs(+decayFlow, svif::E, lab.str("discharge-decay-bal"));
  d_cofCount           = pushCof(disKRow, modeCol, +decayFlow);
  d_cofCount           = pushCof(disKRow, disKCol, +1.0);

  // apply spill conditions as required
  if ( spillFlag == false )                  // spill not tolerated
    {
      // add a new equality constraint and set it to zero
      const int spillRow = pushRhs(+0.0, svif::E, lab.str("no-spill"));
      d_cofCount         = pushCof(spillRow, spillCol, +1.0);
    }

  // add the ramp rate restrictions as required
  if ( ! std::isinf(rechargeBound) )            // refer <cmath>
    {
      const int recBndRow = pushRhs(+rechargeBound, svif::L, lab.str("recharge-bnd"));
      d_cofCount          = pushCof(recBndRow, inflowCol, +1.0);
    }
  if ( ! std::isinf(dischargeBound) )
    {
      const int disBndRow = pushRhs(+dischargeBound, svif::L, lab.str("discharge-bnd"));
      d_cofCount          = pushCof(disBndRow, outflowCol, +1.0);
    }

  // additional reporting as appropriate
  // YEEK 50 CODE (set by '--yeek')
  if ( xeona::yeek == 50 || xeona::yeek == 1 || xeona::yeek == 2 )
    {
      const double storage = d_openingInventory / hiCapacity;
      const double ratio   = outflowcap / (inflowcap + outflowcap);
      std::ostringstream put;
      put << "  storage OSP reporting (create)"                                   << "\n"
          << "      label                               : " << d_label            << "\n"
          << std::boolalpha
          << "    given"                                                          << "\n"
          << "      spill flag                          : " << spillFlag          << "\n"
          << "      opening inventory               [*] : " << d_openingInventory << "\n"
          << "      lo capacity                     [*] : " << loCapacity         << "\n"
          << "      hi capacity                     [*] : " << hiCapacity         << "\n"
          << "      recharge bound                [*/s] : " << rechargeBound      << "\n"
          << "      discharge bound               [*/s] : " << dischargeBound     << "\n"
          << "      round trip efficiency           [-] : " << roundTripEffy      << "\n"
          << "      normalized decay over interval  [-] : " << normalizedDecay    << "\n"
          << "      interval                        [s] : " << interval           << "\n"
          << "    calculated"                                                     << "\n"
          << "      inflow capacity               [*/s] : " << inflowcap          << "\n"
          << "      outflow capacity              [*/s] : " << outflowcap         << "\n"
          << "      decay flow                    [*/s] : " << decayFlow          << "\n"
          << "      outcap : incap + outcap         [-] : "
          << boost::format("%.1f%%") % (100.0 * ratio)                            << "\n"
          << "      opening : hi capacity           [-] : "
          << boost::format("%.1f%%") % (100.0 * storage)                          << "\n"
          << "      split efficiency                [-] : " << splitEffy          << "\n";

      put << "   zeroed roundings :";
      if ( opi0 ) put << " opening inventory";
      if ( ! (opi0) ) put << " (none)";
      put << "\n";

      s_logger->repx(logga::dbug, "additional reporting follows, yeek", xeona::yeek);
      s_logger->putx(logga::dbug, put);
    }

  // GLOBALIZE AND RETURN

  // note that external usage requires global indexing
  return globalcols(d_cols);

} // function 'OpsStore_A::uploadEngineering'

// ---------------------------------------------------------
//  MEMBER FUNCTION : uploadShortrunCosts (overloaded)
// ---------------------------------------------------------
//  Description  : cost characterization
//  Role         : host call
//  Techniques   : overloaded function
//  Status       : complete
//
//  Design notes
//
//      This function uses the underlying function
//      'OperationsOsp::uploadShortrunCosts', but performs some
//      integrity checks first.
//
// ---------------------------------------------------------

void
OpsStore_A::uploadShortrunCosts
(const CostSet& shiftCosts)
{
  // initial reporting
  s_logger->repx(logga::adhc, "entering member function", "dumb wrapper");

  // pass thru
  OperationsOsp::uploadShortrunCosts(shiftCosts);
}

void
OpsStore_A::uploadShortrunCosts
(const CostSet& dutySpecCosts,
 const int      gol)
{
  // initial reporting
  s_logger->repx(logga::adhc, "entering member function", "smart wrapper");

  // obtain some gols
  int golRecharge  = -1;                     // nonsensical value
  int golDischarge = -1;                     // nonsensical value
  boost::tie(golRecharge, golDischarge) = globalcols(d_cols);

  // integrity check and active code
  if ( gol == golRecharge )
    {
      s_logger->repx(logga::adhc, "recharge gol identified", gol);
      OperationsOsp::uploadShortrunCosts(dutySpecCosts, gol);
    }
  if ( gol == golDischarge )
    {
      s_logger->repx(logga::adhc, "discharge gol identified", gol);
      OperationsOsp::uploadShortrunCosts(dutySpecCosts, gol);
    }
  else
    {
      // function name
      const std::string func = XEONA_FUNC;

      // reporting
      std::ostringstream put;
      put << "  problematic gol"                                                  << "\n"
          << "    function         : " << func                                    << "\n"
          << "    supplied gol     : " << gol                                     << "\n"
          << "    gol not          : " << golRecharge << ", " << golDischarge     << "\n"
          << "    costset details  : " << dutySpecCosts                           << "\n";
      s_logger->repx(logga::warn, "supplied gol not known", gol);
      s_logger->putx(logga::dbug, put);
    }

} // function 'OpsStore_A::uploadShortrunCosts'

// DOWNLOAD SOLUTION CALLS

// ---------------------------------------------------------
//  MEMBER FUNCTION : downloadSolution
// ---------------------------------------------------------
//  Description  : solution recovery
//  Role         : host call
//  Techniques   : 'boost::make_tuple'
//  Status       : complete
//
//  Design notes
//
//      This function also calculates the storage 'delta' and
//      'closing' inventory.
//
//      The 'round-trip-effy' and 'normalizedDecay' now form part
//      of the OSP (optimization sub-problem).
//
//      There is some defensive programming to check for
//      unexpected values and duly log warnings.
//
// ---------------------------------------------------------

OpsStore_A::results_type
OpsStore_A::downloadSolution() const
{
  // initial reporting
  s_logger->repx(logga::dbug, "entering member function", "");

  // preamble
  const double period  = static_cast<double>(d_interval);

  // obtain external stream data
  double recharge      = downloadVar(d_cols.get<0>());      // +ve as coded
  double discharge     = downloadVar(d_cols.get<1>());      // +ve as coded

  // round down close-to-zeros (depending on the value of
  // 'd_roundZeroTrip' and in addition to 'xeona::numericalZero'
  // which is used by the solver interface)
  const bool rec0 = xeona::roundZero(recharge,  d_roundZeroTrip);
  const bool dis0 = xeona::roundZero(discharge, d_roundZeroTrip);

  // obtain internal stream data
  double inflow  = downloadVar(d_colsInternal.get<0>());   // +ve as coded
  double outflow = downloadVar(d_colsInternal.get<1>());   // +ve as coded

  // more rounding down
  const bool inf0 = xeona::roundZero(inflow,  d_roundZeroTrip);
  const bool out0 = xeona::roundZero(outflow, d_roundZeroTrip);

  // simple arithmetic prevails
  const double fliff   = inflow - outflow;   // internal flow difference
  const double delta   = fliff * period;     // integrated over the interval
  const double opening = d_openingInventory;
  double       closing = opening + delta;

  // this test is motivated by figures like +4.77e-07 / +4.00e+09 = 1.19e-16
  bool clo0 =false;                          // used for reporting purposes
  if ( xeona::nearZero(closing / d_hiCapacity, d_roundZeroTrip) )
    {
      closing = 0.0;
      clo0    = true;
    }

  // integrity checks
  if ( recharge > 0.0 && discharge > 0.0 )   // confirm only one external flow is non-zero
    {
      std::ostringstream oss;
      oss << recharge << " " << discharge;
      s_logger->repx(logga::warn, "concurrent recharge AND discharge", oss.str());
    }
  if ( closing < 0.0 )                       // negative inventories not supported
    {
      std::ostringstream oss;
      oss << closing;
      s_logger->repx(logga::warn, "negative inventory (possible bug)", oss.str());
    }

  // calculate the 'charge', noting only one value can be non-zero
  const double charge = ( discharge > 0.0 ) ? -discharge : +recharge;

  // additional reporting as appropriate
  // YEEK 50 CODE (set by '--yeek')
  // note that 'Label' objects stream as a simple string without a trailing newline
  if ( xeona::yeek == 50 || xeona::yeek == 1 || xeona::yeek == 2 )
    {
      std::ostringstream put;
      put << "  storage OSP reporting (recover)"                                 << "\n"
          << "    note \"zeroed\" means 'roundZero' applied"                     << "\n"
          << "      label                               : " << d_label           << "\n"
          << "      interval                        [s] : " << period            << "\n"
          << "    values"                                                        << "\n"
          << "      round-trip efficiency           [-] : " << d_roundTripEffy   << "\n"
          << "      normalized decay over interval  [-] : " << d_normalizedDecay << "\n"
          << "      internal inflow               [*/s] : " << inflow            << "\n"
          << "      internal outflow              [*/s] : " << outflow           << "\n"
          << "      external recharge  (zeroed)   [*/s] : " << recharge          << "\n"
          << "      external discharge (zeroed)   [*/s] : " << discharge         << "\n"
          << std::showpos
          << "      internal flow difference      [*/s] : " << fliff             << "\n"
          << "      inventory delta                 [*] : " << delta             << "\n"
          << std::noshowpos
          << "    exports"                                                       << "\n"
          << "      charge (recharge is positive) [*/s] : "
          << boost::format("%+.5e") % charge                                     << "\n"
          << "      opening inventory (straight)    [*] : "
          << boost::format("%.5e") % opening                                     << "\n"
          << "      closing inventory (zeroed)      [*] : "
          << boost::format("%.5e") % closing                                     << "\n";

      put << "   zeroed roundings :";
      if ( rec0 ) put << " recharge";
      if ( dis0 ) put << " discharge";
      if ( inf0 ) put << " inflow";
      if ( out0 ) put << " outflow";
      if ( clo0 ) put << " closing";
      if ( ! (rec0 + dis0 + inf0 + out0 + clo0) ) put << " (none)";
      put << "\n";

      s_logger->repx(logga::dbug, "additional reporting follows, yeek", xeona::yeek);
      s_logger->putx(logga::dbug, put);
    }

  return boost::make_tuple(charge, opening, closing);

} // function 'OpsStore_A::downloadSolution'

// ---------------------------------------------------------
//  CLASS           : OpsDummy_A (zero flow)
// ---------------------------------------------------------

// CREATORS

// ---------------------------------------------------------
//  MEMBER FUNCTION : OpsDummy_A
// ---------------------------------------------------------

OpsDummy_A::OpsDummy_A
(shared_ptr<svif::SolverIf> solver,
 const xeona::DomainMode    commitmentMode) :
  OperationsOsp(solver, commitmentMode, "ops-dummy-a"),
  d_cols()                                   // tuple ctor calls default ctors
{
  s_logger->repx(logga::adhc, "constructor call, description", d_ospDesc);
}

// ---------------------------------------------------------
//  MEMBER FUNCTION : ~OpsDummy_A
// ---------------------------------------------------------

OpsDummy_A::~OpsDummy_A()
{
  s_logger->repx(logga::adhc, "destructor call, description", d_ospDesc);
}

// OTHER CALLS

// ---------------------------------------------------------
//  MEMBER FUNCTION : OpsDummy_A::uploadEngineering
// ---------------------------------------------------------
//  Description  : engineering characterization (kind of!)
//  Role         : host call, originally for 'TeasCapA|B' entities
//  Techniques   : 'boost::make_tuple'
//  Status       : complete
// ---------------------------------------------------------

OpsDummy_A::index_type                      // global cols
OpsDummy_A::uploadEngineering()
{
  // initial reporting
  s_logger->repx(logga::xtra, "entering member function", "");

  // PREAMBLE

  // create and fill a label object
  Label lab(d_label);

  // create a stand-in specific costs cost set
  const CostSet zeroSpecCosts(0.0);

  // EXPOSED VARIABLE

  // create exposed variable
  const int cabsocCol = pushObj(zeroSpecCosts, lab.str("cabsoc-bi"));

  // load the local indexes into a 1-tuple
  d_cols = boost::make_tuple(cabsocCol);

  // CABLE OR SOCKET BALANCE

  // bidirectionalize the cable or socket
  int bndChanges      = -1;                  // nonsensical value
  bndChanges          = openBnds(cabsocCol);

  // create the cable or socket balance
  const int cabsocRow = pushRhs(0.0, svif::E, lab.str("cabsoc-bal"));

  // add the exposed variables
  d_cofCount          = pushCof(cabsocRow, cabsocCol, +1.0);

  // BLOCKETTES

  // note that the zero blockette is the sole blockette in this
  // OSP -- now create an out variable for the zero blockette
  const int zeroCol = pushObj(zeroSpecCosts, lab.str("zero"));
  bndChanges        = openBnds(zeroCol);

  // connect to the cable or socket balance
  d_cofCount        = pushCof(cabsocRow, zeroCol, -1.0);

  // define a zero flow for the blockette (nothing else required)
  const double flow = 0.0;                   // defined flow [1]
  const int zeroRow = pushRhs(flow, svif::E, lab.str("zero-quantity"));
  d_cofCount        = pushCof(zeroRow, zeroCol, +1.0);

  // [1] may be negative, zero, or positive

  // GLOBALIZE AND RETURN

  // note that external usage requires global indexing
  return globalcols(d_cols);

} // function 'OpsDummy_A::uploadEngineering'

// DOWNLOAD SOLUTION CALLS

// ---------------------------------------------------------
//  MEMBER FUNCTION : downloadSolution
// ---------------------------------------------------------
//  Description  : solution recovery
//  Role         : host call
//  Techniques   : 'boost::make_tuple'
//  Status       : complete
// ---------------------------------------------------------

OpsDummy_A::results_type
OpsDummy_A::downloadSolution() const
{
  return boost::make_tuple(downloadVar(d_cols.get<0>()));
}

//  end of file

