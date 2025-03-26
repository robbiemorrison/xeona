//  XEONA ENERGY SYSTEMS MODELING ENVIRONMENT
//
//  file-create-name : optops2.cc
//  file-create-date : Tue 10-Jan-2012 13:13 UTC
//  file-initiator   : Robbie Morrison <robbie@actrix.co.nz>
//
//  file-role        : operations OSPs for technical assets 2 / implementation
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
//  $URL: file:///home/robbie/svn-root/xeona/futz/trunk/xeona1/b/optops2.cc $
//
//  GENERAL NOTES FOR THIS FILE

//  LOCAL AND SYSTEM INCLUDES

#include "optops2.h"          // companion header for this file (place first)

#include "../c/label.h"       // helper class to format solver labels
#include "../c/costs.h"       // cost sets and support
#include "../c/util4.h"       // free functions for trigonometry and maths
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

//  PREPROCESSOR MACROS FOR CODE-SWITCH PURPOSES

// XE_OPS_LOAD_FLOW_ABS_HI
//
// 0 = exclude hi bound setting, 1 = action hi bound setting
// Used within member function 'OpsLoadFlow_A::uploadEngineering'.
//
// Extensive tests show that this set of constraints is not
// needed and that its omission leads to a noticeable speed up
// (37% in the case of the proof-of-concept model).
//
// If searching the PDF documentation, try: "In all likelihood,
// (25) and (26) are not required because the solver has ever
// reason to keep [theta] on its lower bound."

#define XE_OPS_LOAD_FLOW_ABS_HI 0

//  CODE

// ---------------------------------------------------------
//  CLASS           : OpsLoadFlow_A (HV transmission)
// ---------------------------------------------------------
//
//  Overview
//
//      This OSP implements the (misnamed) enhanced DC load flow
//      model (the term "power flow" model is also synonymous).
//      Under this model, power transmission and power loss are
//      functions of the voltage angle difference over the line.
//
//  Single bidirectional edge
//
//      This class is based on a single bidirectional edge.
//
//  OSP diagram
//
//      See latest version of file 'osp-grid_00.pdf' (at least 14).
//
//  Flow orientation
//
//      There are two seperate flow orientation metrics: "flow
//      orientation" for reporting and "forward flow" for the
//      mathematics.  Flow orientation for reporting has the
//      following semantics:
//
//         0 indicates cable > socket flow
//         1 indicates socket > cable flow
//
//      Forward flow maps to flow orientation zero.
//
//  More details
//
//      See any documentation for the various member functions.
//
//      Further details are also given in my (Robbie Morrison)
//      PhD report and elsewhere.
//
// ---------------------------------------------------------

// CREATORS

// ---------------------------------------------------------
//  MEMBER FUNCTION : OpsLoadFlow_A
// ---------------------------------------------------------

OpsLoadFlow_A::OpsLoadFlow_A
(shared_ptr<svif::SolverIf> solver,
 const xeona::DomainMode    commitmentMode) :
  OperationsOsp(solver, commitmentMode, "ops-load-flow-a"),
  d_cols(),                                  // tuple ctor calls default ctors
  d_capacity(0.0),
  d_maxShiftDegrees(0.0)
{
  s_logger->repx(logga::adhc, "constructor call, description", d_ospDesc);
}

// ---------------------------------------------------------
//  MEMBER FUNCTION : ~OpsLoadFlow_A
// ---------------------------------------------------------

OpsLoadFlow_A::~OpsLoadFlow_A()
{
  s_logger->repx(logga::adhc, "destructor call, description", d_ospDesc);
}

// FILL PROBLEM CALLS

// ---------------------------------------------------------
//  MEMBER FUNCTION : uploadEngineering
// ---------------------------------------------------------
//  Description  : engineering characterization
//  Role         : host call
//  Techniques   : piecewise discretization
//  Status       : complete
//
//  Enhanced DC power flow model
//
//      This OSP implements a simplified form of static
//      fixed-frequency AC power flow analysis.  It uses the
//      so-called enhanced "DC" load flow model described by
//      Motto etal (2002).  The details of recasting the Motto
//      formulation into a format suitable for coding here are
//      given in my (Robbie Morrison) PhD report (and other
//      supporting documentation).  You will also need to know
//      some power systems analysis to understand this procedure.
//      See, for instance, Andersson (2008).
//
//      This OSP also enacts the bi-direction feature required by
//      transmission lines.  It does this by allowing negative
//      flows and not by using two anti-parallel but otherwise
//      symmetric characterizations.
//
//      The 'k' and 'm' refer to nodes in the original model,
//      where 'k' is the focus of interest (the sender node) and
//      'm' identifies the particular line (via the receiver
//      node) under consideration.
//
//      The capacity constraint is applied at both ends of the
//      line -- while noting that only the injection end would
//      count.
//
//  Arguments
//
//      The engineering characterization takes impedance values,
//      while the calculations are performed using admittance.
//
//  References
//
//      Andersson, Goeran.  2008.  Modelling and analysis of
//        electric power systems : power flow analysis fault
//        analysis power systems dynamics and stability.  ETH,
//        Zurich, Switzerland.  Lecture 227-0526-00, ITET ETH
//        Zurich.  [download 'modelling_hs08_script_m02.pdf']
//
//      Motto, Alexis L, Fransisco D Galiana, Antonio J Conejo,
//        and Jose M Arroyo.  2002.  Network-constrained
//        multiperiod auction for a pool-based electricity
//        market.  IEEE Transactions on Power Systems v17 no3
//        p646-653.  doi:10.1109/TPWRS.2002.800909
//
// ---------------------------------------------------------

OpsLoadFlow_A::index_type                    // global cols
OpsLoadFlow_A::uploadEngineering
(const double capacity,                      // in unprefixed SI units [W]
 const double voltage,                       // in unprefixed SI units [V]
 const double resistancePerMetre,            // in unprefixed SI units [ohm/m]
 const double reactancePerMetre,             // in unprefixed SI units [ohm/m]
 const double length,                        // in unprefixed SI units [m]
 const double maxShiftDegrees,               // in degrees [deg]
 const int    discretization)                // number of discretization steps
{
  // initial reporting
  s_logger->repx(logga::xtra, "entering member function", "");

  // ---------------------------------
  //  PREAMBLE
  // ---------------------------------

  // create and fill a label object
  Label lab(d_label);

  // INTEGRITY CHECKS

  if ( resistancePerMetre < 0.0 )            // can be zero though
    {
      s_logger->repx(logga::warn, "negative per-metre resistance (r)", reactancePerMetre);
    }
  if ( reactancePerMetre == 0.0 )
    {
      s_logger->repx(logga::warn, "zero per-metre reactance (x)", reactancePerMetre);
    }
  else if ( reactancePerMetre < 0.0 )        // cannot be zero though
    {
      s_logger->repx(logga::warn, "negative per-metre reactance (x)", reactancePerMetre);
    }
  if ( maxShiftDegrees <= 0.0 )
    {
      s_logger->repx(logga::warn, "max shift degrees not positive", maxShiftDegrees);
    }
  if ( discretization <= 0 )
    {
      s_logger->repx(logga::warn, "invalid discretization value", discretization);
    }

  // PREAMBLE

  // create a stand-in specific costs cost set
  const CostSet zeroSpecCosts(0.0);

  // set variables also required by function 'washup'
  d_capacity        = capacity;
  d_maxShiftDegrees = maxShiftDegrees;

  // set the 'delThetaBlok' interval used in discretization code
  const double maxShiftRadians = xeona::degree2radian(maxShiftDegrees);
  const double delthetaBlok    = maxShiftRadians / discretization;

  // PHYSICAL COEFFICIENTS

  const double R     = resistancePerMetre * length;    // resistance [ohm]
  const double X     = reactancePerMetre  * length;    // reactance  [ohm]
  const double denom = R * R + X * X;
  double G = std::numeric_limits<double>::infinity();  // conductance [S]
  double B = std::numeric_limits<double>::infinity();  // susceptance [S]
  if ( denom != 0 )                          // div-by-zero protection
    {
      G              = +R / denom;           // conductance [S]
      B              = -X / denom;           // susceptance [S]
    }
  else
    {
      s_logger->repx(logga::warn, "R and X both zero", denom);
    }
  const double V2B   = voltage * voltage * B;
  const double V2G   = voltage * voltage * G;

  // additional reporting as appropriate
  // YEEK 59 CODE (set by '--yeek')
  if ( xeona::yeek == 59 || xeona::yeek == 1 || xeona::yeek == 2 )
    {
      const double kms = length/1000.0;
      std::ostringstream put;
      put << "  line characterization OSP values (AC transmission):"               << "\n"
          << std::showpos
          << "    kilometres               [km] : " << boost::format("%.1f") % kms << "\n"
          << "    resistance per metre  [ohm/m] : " << resistancePerMetre          << "\n"
          << "    reactance per metre   [ohm/m] : " << reactancePerMetre           << "\n"
          << "    resistance  (R)         [ohm] : " << R                           << "\n"
          << "    reactance   (X)         [ohm] : " << X                           << "\n"
          << "    conductance (G)           [S] : " << G                           << "\n"
          << "    susceptance (B) (-ve)     [S] : " << B                           << "\n"
          << std::noshowpos;
      s_logger->repx(logga::dbug, "additional reporting follows, yeek", xeona::yeek);
      s_logger->putx(logga::dbug, put);
    }

  // ---------------------------------
  //  EXPOSED VARIABLES
  // ---------------------------------

  // FLOWS AND ANGLES

  // create four variables covering input and output -- the zero
  // specific costs can later be rectified by
  // 'uploadShortrunCosts' but, in all likelihood, will simply
  // remain zero
  const int cabColFlow  = pushObj(zeroSpecCosts, lab.str("cable-flow-bi"));
  const int cabColTheta = pushObj(zeroSpecCosts, lab.str("cable-theta"));
  const int socColFlow  = pushObj(zeroSpecCosts, lab.str("socket-flow-bi"));
  const int socColTheta = pushObj(zeroSpecCosts, lab.str("socket-theta"));

  // bidirectionalize the cable and socket for both 'flow' and 'theta'
  int bndChanges = -1;                       // nonsensical value
  bndChanges     = openBnds(cabColFlow);
  bndChanges     = openBnds(socColFlow);
  bndChanges     = openBnds(cabColTheta);
  bndChanges     = openBnds(socColTheta);

  // ---------------------------------
  //  NON-LOOP CHARACTERIZATIONS
  // ---------------------------------
  //
  // positive flow    : soc -> cab
  // equation mapping : 'k' = cab and 'm' = soc

  // FLOW RELATIONSHIPS

  // create cable and socket balances
  const int cabRowFlow   = pushRhs(0.0, svif::E, lab.str("cable-flow-bal"));
  const int socRowFlow   = pushRhs(0.0, svif::E, lab.str("socket-flow-bal"));

  // add the exposed variable -- the cable end and the socket end
  d_cofCount             = pushCof(cabRowFlow, cabColFlow, +1.0);
  d_cofCount             = pushCof(socRowFlow, socColFlow, -1.0);

  // theta constraint -- note the orientation
  const int thetaRowSum  = pushRhs(0.0, svif::E, lab.str("theta-sum"));
  d_cofCount             = pushCof(thetaRowSum, cabColTheta, +1.0);
  d_cofCount             = pushCof(thetaRowSum, socColTheta, -1.0);

  // FORWARD AND REVERSE LOCK-OUT

  // create binary variables
  const int zCol1        = pushObj(zeroSpecCosts, lab.str("z-reverse"));
  const int zCol2        = pushObj(zeroSpecCosts, lab.str("z-forward"));
  markBinary(zCol1);
  markBinary(zCol2);

  // binary variable lock-out constraint
  const int zRowToggle   = pushRhs(1.0, svif::E, lab.str("flow-dir-toggle"));
  d_cofCount             = pushCof(zRowToggle, zCol1, +1.0);
  d_cofCount             = pushCof(zRowToggle, zCol2, +1.0);

  // FLOW CAPACITIES

  // set capacity constraints -- no loop entries required and
  // note the symmetry (unlike the loss term elsewhere which
  // always applies)

  // cable end
  const int cabRowUpperA   = pushRhs(0.0, svif::G, lab.str("cable-upper-a"));
  const int cabRowUpperB   = pushRhs(0.0, svif::L, lab.str("cable-upper-b"));
  d_cofCount               = pushCof(cabRowUpperA, cabColFlow, +1.0);
  d_cofCount               = pushCof(cabRowUpperA, zCol1,      +capacity);
  d_cofCount               = pushCof(cabRowUpperB, cabColFlow, +1.0);
  d_cofCount               = pushCof(cabRowUpperB, zCol2,      -capacity);

  // socket end
  const int socRowUpperA   = pushRhs(0.0, svif::G, lab.str("socket-upper-a"));
  const int socRowUpperB   = pushRhs(0.0, svif::L, lab.str("socket-upper-b"));
  d_cofCount               = pushCof(socRowUpperA, socColFlow, +1.0);
  d_cofCount               = pushCof(socRowUpperA, zCol1,      +capacity);
  d_cofCount               = pushCof(socRowUpperB, socColFlow, +1.0);
  d_cofCount               = pushCof(socRowUpperB, zCol2,      -capacity);

  // LOAD LOCAL INDEXES

  // load the local indexes into a 5-tuple
  d_cols = boost::make_tuple(cabColFlow,     // active power flow [W]
                             cabColTheta,    // voltage angle [rad]
                             socColFlow,
                             socColTheta,
                             zCol2);         // 'one' if socket to cable [1]

  // [1] in this case, 'zCol2' is the correct variable to use
  // because it is unity if the flow is from socket to cable

  // ---------------------------------
  //  DISCRETIZED CHARACTERIZATIONS
  // ---------------------------------

  for ( int i = 1;                                // 'ell' in documentation
        i    <= discretization;                   // 'L' in documentation
        ++i )                                     // loop the blockettes
    {
      // add to the label
      lab << boost::format("%s%02d") % "" % i;    // "01"

      // ENGINEERING

      // calculate coefficients
      const double slopeFactor = (2 * i - 1) * delthetaBlok;   // CAUTION: note the 'i'
      const double V2GL        = V2G * slopeFactor;

      // two new blockette variables
      const int delthetaCol    = pushObj(0.0, lab.str("del-theta"));       // signed
      const int delthetaColAbs = pushObj(0.0, lab.str("del-theta-abs"));   // absolute
      bndChanges               = openBnds(delthetaCol);

      // CORE

      // add del-theta entry
      d_cofCount               = pushCof(thetaRowSum, delthetaCol, -1.0);

      // add cable and socket balances
      d_cofCount               = pushCof(cabRowFlow, delthetaCol,    +V2B);
      d_cofCount               = pushCof(socRowFlow, delthetaCol,    -V2B);
      d_cofCount               = pushCof(cabRowFlow, delthetaColAbs, -0.5 * V2GL);
      d_cofCount               = pushCof(socRowFlow, delthetaColAbs, -0.5 * V2GL);

      // integrity checks
      if ( std::abs(V2GL) > std::abs(V2B) )
        {
          std::ostringstream oss1;
          std::ostringstream oss2;
          std::ostringstream oss3;
          oss1 << "V2GL trumps V2B on l = " << i;
          oss2 << V2GL << " " << V2B;
          oss3 << resistancePerMetre << " " << reactancePerMetre;
          s_logger->repx(logga::warn, oss1.str(), oss2.str());
          s_logger->repx(logga::warn, "check length-specific R and X values", oss3.str());
        }

      // DEL-THETA ABSOLUTE

      const int dthaRowFwdLo = pushRhs(0.0, svif::G, lab.str("del-theta-abs-fwd-lo"));
      const int dthaRowRevLo = pushRhs(0.0, svif::G, lab.str("del-theta-abs-rev-lo"));

#if (XE_OPS_LOAD_FLOW_ABS_HI == 1)

      const double C         = 2.0 * maxShiftRadians;      // twice the cap value

      const int dthaRowFwdHi = pushRhs(0.0, svif::L, lab.str("del-theta-abs-fwd-hi"));
      const int dthaRowRevHi = pushRhs(0.0, svif::L, lab.str("del-theta-abs-rev-hi"));
      d_cofCount             = pushCof(dthaRowFwdHi, zCol1, -C);
      d_cofCount             = pushCof(dthaRowRevHi, zCol2, -C);

#endif // XE_OPS_LOAD_FLOW_ABS_HI

      // DEL-THETA ABSOLUTE

      d_cofCount               = pushCof(dthaRowFwdLo, delthetaCol,    -1.0);
      d_cofCount               = pushCof(dthaRowFwdLo, delthetaColAbs, +1.0);
      d_cofCount               = pushCof(dthaRowRevLo, delthetaCol,    +1.0);
      d_cofCount               = pushCof(dthaRowRevLo, delthetaColAbs, +1.0);

#if (XE_OPS_LOAD_FLOW_ABS_HI == 1)

      d_cofCount               = pushCof(dthaRowFwdHi, delthetaCol,    -1.0);
      d_cofCount               = pushCof(dthaRowFwdHi, delthetaColAbs, +1.0);
      d_cofCount               = pushCof(dthaRowRevHi, delthetaCol,    +1.0);
      d_cofCount               = pushCof(dthaRowRevHi, delthetaColAbs, +1.0);

#endif // XE_OPS_LOAD_FLOW_ABS_HI

      // DEL-THETA BOUNDS

      // del-theta bounds -- also used in the loop
      const int delthetaRowFwd = pushRhs(0.0, svif::L, lab.str("del-theta-fwd"));
      const int delthetaRowRev = pushRhs(0.0, svif::G, lab.str("del-theta-rev"));

      d_cofCount               = pushCof(delthetaRowFwd, zCol2, -delthetaBlok);
      d_cofCount               = pushCof(delthetaRowRev, zCol1, +delthetaBlok);

      // add deltheta bounds
      d_cofCount               = pushCof(delthetaRowFwd, delthetaCol, +1.0);
      d_cofCount               = pushCof(delthetaRowRev, delthetaCol, +1.0);

#if 0 // 1 to activate the explicit upper bound del-theta_blok, 0 otherwise

      // del-theta absolute
      const int delthetaRowAbs = pushRhs(delthetaBlok, svif::L, lab.str("del-theta-abs"));
      d_cofCount               = pushCof(delthetaRowAbs, delthetaColAbs, +1.0);

#endif // 0

      // scrap the beginning of block "00" part
      lab.trim(1);                           // remove the last labelette

    } // discretization loop

   // ---------------------------------
  //  GLOBALIZE AND RETURN
  // ---------------------------------

  // note that external usage requires global indexing
  return globalcols(d_cols);

} // function 'OpsLoadFlow_A::uploadEngineering'

// DOWNLOAD SOLUTION CALLS

// ---------------------------------------------------------
//  MEMBER FUNCTION : downloadSolution
// ---------------------------------------------------------
//  Description  : solution recovery
//  Role         : host call
//  Techniques   : directionality calculations
//  Status       : complete
// ---------------------------------------------------------

OpsLoadFlow_A::results_type
OpsLoadFlow_A::downloadSolution() const
{
  // initial reporting
  s_logger->repx(logga::xtra, "entering member function", "");

  // identify transmission results
  const double cabFlow         = downloadVar(d_cols.get<0>());   // bidirectional
  const double socFlow         = downloadVar(d_cols.get<2>());   // bidirectional
  const double cabThetaDegrees = xeona::radian2degree(downloadVar(d_cols.get<1>()));
  const double socThetaDegrees = xeona::radian2degree(downloadVar(d_cols.get<3>()));
  const int    direction01     = downloadVar(d_cols.get<4>());
  const bool   directionFlag   = static_cast<bool>(direction01); // one is 'true'

  // integrity check
  if ( (cabFlow * socFlow) < 0.0 )           // one and only one is negative
    {
      s_logger->repx(logga::warn, "integrity check failure", "unmatched signs");
      std::ostringstream put;
      put << "  cable flow  : " << std::showpos << cabFlow << "\n"
          << "  socket flow : " << std::showpos << socFlow << "\n";
      s_logger->putx(logga::xtra, put);
    }

  // process secondary quantities -- used only for relative duty
  // and loss calculations and some additional reporting

  double inject = -1.0;                      // current injection, always positive
  double exit   = -1.0;                      // current exit, always positive
  if ( directionFlag == true )               // flow orientation from socket to cable
    {
      inject = +cabFlow;
      exit   = +socFlow;
    }
  else                                       // opposite flow orientation encountered
    {
      inject = -socFlow;
      exit   = -cabFlow;
    }

  // calculate with div-by-zero protection
  double relativeDuty = 0.0;
  double relativeLoss = 0.0;
  if ( d_capacity != 0.0 ) relativeDuty = inject / d_capacity;
  if ( inject     != 0.0 ) relativeLoss = (inject - exit) / inject;

  // calculate theta delta
  const double thetaDegreesDelta = cabThetaDegrees - socThetaDegrees;

  // integrity checks
  if ( xeona::almostEqual(thetaDegreesDelta, d_maxShiftDegrees, xeona::tight) )
    {
      std::ostringstream oss;
      oss << thetaDegreesDelta << " " << d_maxShiftDegrees;
      s_logger->repx(logga::warn, "insufficient max shift degrees", oss.str());
      s_logger->repx(logga::dbug, "comment regarding above", "need to relax model");
    }
  else if ( thetaDegreesDelta > d_maxShiftDegrees )
    {
      // CAUTION: if you end up here and the values are close,
      // try using 'xeona::loose' above instead
      std::ostringstream oss;
      oss << thetaDegreesDelta << " " << d_maxShiftDegrees;
      const std::string msg = "theta delta exceeds max shift degrees value";
      s_logger->repx(logga::warn, "insufficient max shift degrees", oss.str());
      s_logger->repx(logga::warn, "probable program logic error", msg);
    }
  if ( xeona::nearZero(relativeDuty, xeona::zero5) )   // within 1.0e-05
    {
      // catch trickle flows of about one millionth, do nothing is correct
    }
  else if ( relativeDuty < 0.0 )
    {
      s_logger->repx(logga::warn, "negative relative duty", relativeDuty);
    }
  else if ( relativeDuty > 1.0 )
    {
      s_logger->repx(logga::warn, "relative duty above unity", relativeDuty);
    }

  if ( xeona::nearZero(relativeDuty, xeona::zero5) )   // within 1.0e-05
    {
      // catch trickle flows of about one millionth, do nothing is correct
    }
  else if ( relativeLoss < 0.0 )
    {
      s_logger->repx(logga::warn, "negative relative loss (line gain)", relativeLoss);
    }
  else if ( relativeLoss > 1.0 )
    {
      s_logger->repx(logga::warn, "relative loss above unity", relativeLoss);
    }

  // additional reporting as appropriate
  // YEEK 59 CODE (set by '--yeek')
  if ( xeona::yeek == 59 || xeona::yeek == 1 || xeona::yeek == 2 )
    {
      std::ostringstream put;
      put << "  load flow OSP values (AC transmission):"                         << "\n"
          << std::showpos
          << "    cable flow      [W] : " << cabFlow                             << "\n"
          << "    socket flow     [W] : " << socFlow                             << "\n"
          << "    inject flow     [W] : " << inject                              << "\n"
          << "    exit flow       [W] : " << exit                                << "\n"
          << std::noshowpos
          << "    flow orientation    : " << directionFlag                       << "\n"
          << "    relative duty   [-] : " << relativeDuty                        << "\n"
          << "    relative loss   [-] : " << relativeLoss                        << "\n"
          << std::showpos
          << "    cable theta   [deg] : " << cabThetaDegrees                     << "\n"
          << "    socket theta  [deg] : " << socThetaDegrees                     << "\n"
          << "    theta delta   [deg] : " << thetaDegreesDelta                   << "\n";
      s_logger->repx(logga::dbug, "additional reporting follows, yeek", xeona::yeek);
      s_logger->putx(logga::dbug, put);
    }

  // return
  return boost::make_tuple(directionFlag,
                           cabFlow,
                           socFlow,
                           relativeDuty,
                           relativeLoss,
                           thetaDegreesDelta);

} // function 'downloadSolution'

//  end of file

