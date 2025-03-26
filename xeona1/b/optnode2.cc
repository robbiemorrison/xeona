//  XEONA ENERGY SYSTEMS MODELING ENVIRONMENT
//
//  file-create-name : optnode2.cc
//  file-create-date : Tue 10-Jan-2012 19:28 UTC
//  file-initiator   : Robbie Morrison <robbie@actrix.co.nz>
//
//  file-role        : node optimization sub-problems for LMP nodes 2 / implementation
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
//  $URL: file:///home/robbie/svn-root/xeona/futz/trunk/xeona1/b/optnode2.cc $
//
//  GENERAL NOTES FOR THIS FILE

//  LOCAL AND SYSTEM INCLUDES

#include "optnode2.h"         // companion header for this file (place first)

#include "../c/label.h"       // helper class to format solver labels
#include "../c/costs.h"       // cost sets and support

#include "../c/util4.h"       // free functions for trigonometry and maths
#include "../a/logger.h"      // standard logging functionality (as required)
#include "../c/smart_ptr.h"   // toggle between Boost and TR1 smart pointers
#include ".././common.h"      // common definitions for project (place last)

#include <string>             // C++ strings
#include <sstream>            // string-streams

//  CODE

// ---------------------------------------------------------
//  CLASS           : Lmp2CabSoc0
// ---------------------------------------------------------

// ---------------------------------------------------------
//  MEMBER FUNCTION : Lmp2CabSoc0
// ---------------------------------------------------------

Lmp2CabSoc0::Lmp2CabSoc0
(shared_ptr<svif::SolverIf> solver,
 const xeona::DomainMode    commitmentMode) :
  Lmp2NodeOsp(solver, commitmentMode, "lmp2-cable+socket"),
  d_cols(),                                  // tuple ctor calls default ctors
  d_row(0)
{
  s_logger->repx(logga::adhc, "constructor call, description", d_ospDesc);
}

// ---------------------------------------------------------
//  MEMBER FUNCTION : ~Lmp2CabSoc0
// ---------------------------------------------------------

Lmp2CabSoc0::~Lmp2CabSoc0()
{
  s_logger->repx(logga::adhc, "destructor call", "");
}

// ---------------------------------------------------------
//  MEMBER FUNCTION : uploadEngineering
// ---------------------------------------------------------

Lmp2CabSoc0::index_type
Lmp2CabSoc0::uploadEngineering()
{
  // initial reporting
  s_logger->repx(logga::xtra, "entering member function", "");

  // PREAMBLE

  // create and fill a label object
  Label lab(d_label);

  // create a stand-in specific costs cost set
  const CostSet zeroSpecCosts(0.0);

  // EXPOSED VARIABLES

  // create some variables covering input and output -- the zero
  // specific costs are, of course, fine
  const int cabCol = pushObj(zeroSpecCosts, lab.str("cable"));
  const int socCol = pushObj(zeroSpecCosts, lab.str("socket"));

  // load the local col indexes into a 2-tuple
  d_cols = boost::make_tuple(cabCol,
                             socCol);

  // NODE BALANCE

  // no need to bidirectionalize anything

  // create the node balance
  const int nodeRow = pushRhs(0.0, svif::E, lab.str("node-bal"));

  // load the local row index into an 'int'
  d_row      = nodeRow;

  // add the exposed variables to the flow balance
  d_cofCount = pushCof(nodeRow, cabCol, +1.0);
  d_cofCount = pushCof(nodeRow, socCol, -1.0);

  // GLOBALIZE AND RETURN

  // note that external usage requires global indexing
  return globalcols(d_cols);
}

// ---------------------------------------------------------
//  MEMBER FUNCTION : downloadSolution
// ---------------------------------------------------------

Lmp2CabSoc0::results_type
Lmp2CabSoc0::downloadSolution() const
{
  // initial reporting
  s_logger->repx(logga::xtra, "entering member function", "");

  // active code
  return boost::make_tuple(downloadVar(d_cols.get<0>()),
                           downloadVar(d_cols.get<1>()),
                           downloadSlack(d_row));
}

// ---------------------------------------------------------
//  CLASS           : Lmp2Cab1A
// ---------------------------------------------------------

// ---------------------------------------------------------
//  MEMBER FUNCTION : Lmp2Cab1A
// ---------------------------------------------------------

Lmp2Cab1A::Lmp2Cab1A
(shared_ptr<svif::SolverIf> solver,
 const xeona::DomainMode    commitmentMode) :
  Lmp2NodeOsp(solver, commitmentMode, "lmp2-cable+bilateral"),
  d_cols(),                                  // tuple ctor calls default ctors
  d_row(0)
{
  s_logger->repx(logga::adhc, "constructor call, description", d_ospDesc);
}

// ---------------------------------------------------------
//  MEMBER FUNCTION : ~Lmp2Cab1A
// ---------------------------------------------------------

Lmp2Cab1A::~Lmp2Cab1A()
{
  s_logger->repx(logga::adhc, "destructor call", "");
}

// ---------------------------------------------------------
//  MEMBER FUNCTION : uploadEngineering
// ---------------------------------------------------------

Lmp2Cab1A::index_type
Lmp2Cab1A::uploadEngineering()
{
  // initial reporting
  s_logger->repx(logga::xtra, "entering member function", "");

  // PREAMBLE

  // create and fill a label object
  Label lab(d_label);

  // create a stand-in specific costs cost set
  const CostSet zeroSpecCosts(0.0);

  // EXPOSED VARIABLES

  // create some variables covering input and output -- the zero
  // specific costs are, of course, fine
  const int cab1Col  = pushObj(zeroSpecCosts, lab.str("cable"));
  const int cab2Col  = pushObj(zeroSpecCosts, lab.str("cable-bi"));
  const int thetaCol = pushObj(zeroSpecCosts, lab.str("theta"));

  // load the local col indexes into a 3-tuple
  d_cols = boost::make_tuple(cab1Col,
                             cab2Col,
                             thetaCol);

  // allow 'theta' to be negative
  openBnds(thetaCol);

  // NODE BALANCE

  // bidirectionalize the cable
  const int bndChanges = openBnds(cab2Col);

  // defensive programming (for this and 'LmpSoc1')
  switch ( bndChanges )
    {
    case 0:
      s_logger->repx(logga::warn, "bidirectionalize failed, changes", bndChanges);
      break;
    case 1:                                  // hopefully 0.0 to -inf
      // do nothing okay
      break;
    case 2:
      s_logger->repx(logga::warn, "bidirectionalize failed, changes", bndChanges);
      break;
    default:
      std::clog << "** coding error 01 in source file " << __FILE__ << std::endl;
      break;
    }

  // create the node balance
  const int nodeRow = pushRhs(0.0, svif::E, lab.str("node-bal"));

  // load the local row and theta col indexes into 'int's
  d_row      = nodeRow;
  d_thetaGol = globalcol(thetaCol);

  // add the exposed variables to the flow balance
  d_cofCount = pushCof(nodeRow, cab1Col, +1.0);
  d_cofCount = pushCof(nodeRow, cab2Col, +1.0);

  // GLOBALIZE AND RETURN

  // note that external usage requires global indexing
  return globalcols(d_cols);
}

// ---------------------------------------------------------
//  MEMBER FUNCTION : downloadSolution
// ---------------------------------------------------------

Lmp2Cab1A::results_type
Lmp2Cab1A::downloadSolution() const
{
  // initial reporting
  s_logger->repx(logga::xtra, "entering member function", "");

  // active code
  const double voltageAngleDeg = xeona::radian2degree(downloadVar(d_cols.get<2>()));
  return boost::make_tuple(downloadVar(d_cols.get<0>()),
                           voltageAngleDeg,
                           downloadSlack(d_row));
}

// ---------------------------------------------------------
//  CLASS           : Lmp2Cab1B
// ---------------------------------------------------------

// ---------------------------------------------------------
//  MEMBER FUNCTION : Lmp2Cab1B
// ---------------------------------------------------------

Lmp2Cab1B::Lmp2Cab1B
(shared_ptr<svif::SolverIf> solver,
 const xeona::DomainMode    commitmentMode) :
  Lmp2NodeOsp(solver, commitmentMode, "lmp2-cable+bilateral"),
  d_cols(),                                  // tuple ctor calls default ctors
  d_row(0)
{
  s_logger->repx(logga::adhc, "constructor call, description", d_ospDesc);
}

// ---------------------------------------------------------
//  MEMBER FUNCTION : ~Lmp2Cab1B
// ---------------------------------------------------------

Lmp2Cab1B::~Lmp2Cab1B()
{
  s_logger->repx(logga::adhc, "destructor call", "");
}

// ---------------------------------------------------------
//  MEMBER FUNCTION : uploadEngineering
// ---------------------------------------------------------

Lmp2Cab1B::index_type
Lmp2Cab1B::uploadEngineering()
{
  // initial reporting
  s_logger->repx(logga::xtra, "entering member function", "");

  // PREAMBLE

  // create and fill a label object
  Label lab(d_label);

  // create a stand-in specific costs cost set
  const CostSet zeroSpecCosts(0.0);

  // EXPOSED VARIABLES

  // create some variables covering input and output -- the zero
  // specific costs are, of course, fine
  const int cab1Col  = pushObj(zeroSpecCosts, lab.str("cable"));
  const int soc1Col  = pushObj(zeroSpecCosts, lab.str("socket-bi"));
  const int thetaCol = pushObj(zeroSpecCosts, lab.str("theta"));

  // load the local col indexes into a 3-tuple
  d_cols = boost::make_tuple(cab1Col,
                             soc1Col,
                             thetaCol);

  // allow 'theta' to be negative
  openBnds(thetaCol);

  // NODE BALANCES

  // bidirectionalize the socket
  const int bndChanges = openBnds(soc1Col);

  // defensive programming
  switch ( bndChanges )
    {
    case 0:
      s_logger->repx(logga::warn, "bidirectionalize failed, changes", bndChanges);
      break;
    case 1:                                  // hopefully 0.0 to -inf
      // do nothing okay
      break;
    case 2:
      s_logger->repx(logga::warn, "bidirectionalize failed, changes", bndChanges);
      break;
    default:
      std::clog << "** coding error 01 in source file " << __FILE__ << std::endl;
      break;
    }

  // create the node balance
  const int flowRow = pushRhs(0.0, svif::E, lab.str("flow-bal"));

  // load the local row and theta col indexes into 'int's
  d_row      = flowRow;
  d_thetaGol = globalcol(thetaCol);

  // add the exposed variables to the flow balance
  d_cofCount = pushCof(flowRow, cab1Col, +1.0);
  d_cofCount = pushCof(flowRow, soc1Col, -1.0);

  // GLOBALIZE AND RETURN

  // note that external usage requires global indexing
  return globalcols(d_cols);
}

// ---------------------------------------------------------
//  MEMBER FUNCTION : downloadSolution
// ---------------------------------------------------------

Lmp2Cab1B::results_type
Lmp2Cab1B::downloadSolution() const
{
  // initial reporting
  s_logger->repx(logga::xtra, "entering member function", "");

  // active code
  const double voltageAngleDeg = xeona::radian2degree(downloadVar(d_cols.get<2>()));
  return boost::make_tuple(downloadVar(d_cols.get<0>()),    // 0 = injection
                           voltageAngleDeg,
                           downloadSlack(d_row));
}

// ---------------------------------------------------------
//  CLASS           : Lmp2Soc1A
// ---------------------------------------------------------

// ---------------------------------------------------------
//  MEMBER FUNCTION : Lmp2Soc1A
// ---------------------------------------------------------

Lmp2Soc1A::Lmp2Soc1A
(shared_ptr<svif::SolverIf> solver,
 const xeona::DomainMode    commitmentMode) :
  Lmp2NodeOsp(solver, commitmentMode, "lmp2-socket+bilateral"),
  d_cols(),                                  // tuple ctor calls default ctors
  d_row(0)
{
  s_logger->repx(logga::adhc, "constructor call, description", d_ospDesc);
}

// ---------------------------------------------------------
//  MEMBER FUNCTION : ~Lmp2Soc1A
// ---------------------------------------------------------

Lmp2Soc1A::~Lmp2Soc1A()
{
  s_logger->repx(logga::adhc, "destructor call", "");
}

// ---------------------------------------------------------
//  MEMBER FUNCTION : uploadEngineering
// ---------------------------------------------------------

Lmp2Soc1A::index_type
Lmp2Soc1A::uploadEngineering()
{
  // initial reporting
  s_logger->repx(logga::xtra, "entering member function", "");

  // PREAMBLE

  // create and fill a label object
  Label lab(d_label);

  // create a stand-in specific costs cost set
  const CostSet zeroSpecCosts(0.0);

  // EXPOSED VARIABLES

  // create some variables covering input and output -- the zero
  // specific costs are, of course, fine
  const int cab1Col = pushObj(zeroSpecCosts, lab.str("cable-bi"));
  const int soc1Col = pushObj(zeroSpecCosts, lab.str("socket"));
  const int thetaCol = pushObj(zeroSpecCosts, lab.str("theta"));

  // load the local col indexes into a 3-tuple
  d_cols = boost::make_tuple(cab1Col,
                             soc1Col,
                             thetaCol);

  // allow 'theta' to be negative
  openBnds(thetaCol);

  // NODE BALANCE

  // bidirectionalize the cable
  const int bndChanges = openBnds(cab1Col);

  // defensive programming
  switch ( bndChanges )
    {
    case 0:
      s_logger->repx(logga::warn, "bidirectionalize failed, changes", bndChanges);
      break;
    case 1:                                  // hopefully 0.0 to -inf
      // do nothing okay
      break;
    case 2:
      s_logger->repx(logga::warn, "bidirectionalize failed, changes", bndChanges);
      break;
    default:
      std::clog << "** coding error 02 in source file " << __FILE__ << std::endl;
      break;
    }

  // create the node balance
  const int flowRow = pushRhs(0.0, svif::E, lab.str("node-bal"));

  // load the local row and theta col indexes into 'int's
  d_row      = flowRow;
  d_thetaGol = globalcol(thetaCol);

  // add the exposed variables to the flow balance
  d_cofCount = pushCof(flowRow, cab1Col, +1.0);
  d_cofCount = pushCof(flowRow, soc1Col, -1.0);

  // GLOBALIZE AND RETURN

  // note that external usage requires global indexing
  return globalcols(d_cols);
}

// ---------------------------------------------------------
//  MEMBER FUNCTION : downloadSolution
// ---------------------------------------------------------

Lmp2Soc1A::results_type
Lmp2Soc1A::downloadSolution() const
{
  // initial reporting
  s_logger->repx(logga::xtra, "entering member function", "");

  // active code
  return boost::make_tuple(downloadVar(d_cols.get<1>()),    // 1 = off-take
                           xeona::radian2degree(downloadVar(d_cols.get<2>())),
                           downloadSlack(d_row));
}

// ---------------------------------------------------------
//  CLASS           : Lmp2Soc1B
// ---------------------------------------------------------

// ---------------------------------------------------------
//  MEMBER FUNCTION : Lmp2Soc1B
// ---------------------------------------------------------

Lmp2Soc1B::Lmp2Soc1B
(shared_ptr<svif::SolverIf> solver,
 const xeona::DomainMode    commitmentMode) :
  Lmp2NodeOsp(solver, commitmentMode, "lmp2-socket+bilateral"),
  d_cols(),                                  // tuple ctor calls default ctors
  d_row(0)
{
  s_logger->repx(logga::adhc, "constructor call, description", d_ospDesc);
}

// ---------------------------------------------------------
//  MEMBER FUNCTION : ~Lmp2Soc1B
// ---------------------------------------------------------

Lmp2Soc1B::~Lmp2Soc1B()
{
  s_logger->repx(logga::adhc, "destructor call", "");
}

// ---------------------------------------------------------
//  MEMBER FUNCTION : uploadEngineering
// ---------------------------------------------------------

Lmp2Soc1B::index_type
Lmp2Soc1B::uploadEngineering()
{
  // initial reporting
  s_logger->repx(logga::xtra, "entering member function", "");

  // PREAMBLE

  // create and fill a label object
  Label lab(d_label);

  // create a stand-in specific costs cost set
  const CostSet zeroSpecCosts(0.0);

  // EXPOSED VARIABLES

  // create some variables covering input and output -- the zero
  // specific costs are, of course, fine
  const int soc1Col  = pushObj(zeroSpecCosts, lab.str("socket-bi"));
  const int soc2Col  = pushObj(zeroSpecCosts, lab.str("socket"));
  const int thetaCol = pushObj(zeroSpecCosts, lab.str("theta"));

  // load the local col indexes into a 3-tuple
  d_cols = boost::make_tuple(soc1Col,
                             soc2Col,
                             thetaCol);

  // allow 'theta' to be negative
  openBnds(thetaCol);

  // NODE BALANCE

  // bidirectionalize the socket
  const int bndChanges = openBnds(soc1Col);

  // defensive programming
  switch ( bndChanges )
    {
    case 0:
      s_logger->repx(logga::warn, "bidirectionalize failed, changes", bndChanges);
      break;
    case 1:                                  // hopefully 0.0 to -inf
      // do nothing okay
      break;
    case 2:
      s_logger->repx(logga::warn, "bidirectionalize failed, changes", bndChanges);
      break;
    default:
      std::clog << "** coding error 01 in source file " << __FILE__ << std::endl;
      break;
    }

  // create the node balance
  const int nodeRow = pushRhs(0.0, svif::E, lab.str("node-bal"));

  // load the local row and theta col indexes into 'int's
  d_row      = nodeRow;
  d_thetaGol = globalcol(thetaCol);

  // add the exposed variables to the flow balance
  d_cofCount = pushCof(nodeRow, soc1Col, -1.0);
  d_cofCount = pushCof(nodeRow, soc2Col, -1.0);

  // GLOBALIZE AND RETURN

  // note that external usage requires global indexing
  return globalcols(d_cols);
}

// ---------------------------------------------------------
//  MEMBER FUNCTION : downloadSolution
// ---------------------------------------------------------

Lmp2Soc1B::results_type
Lmp2Soc1B::downloadSolution() const
{
  // initial reporting
  s_logger->repx(logga::xtra, "entering member function", "");

  // active code
  const double voltageAngleDeg = xeona::radian2degree(downloadVar(d_cols.get<2>()));
  return boost::make_tuple(downloadVar(d_cols.get<0>()),
                           voltageAngleDeg,
                           downloadSlack(d_row));
}

// ---------------------------------------------------------
//  CLASS           : Lmp2Nul2
// ---------------------------------------------------------

// ---------------------------------------------------------
//  MEMBER FUNCTION : Lmp2Nul2
// ---------------------------------------------------------

Lmp2Nul2::Lmp2Nul2
(shared_ptr<svif::SolverIf> solver,
 const xeona::DomainMode    commitmentMode) :
  Lmp2NodeOsp(solver, commitmentMode, "lmp2-two-bilateral"),
  d_cols(),                                  // tuple ctor calls default ctors
  d_row(0)
{
  s_logger->repx(logga::adhc, "constructor call, description", d_ospDesc);
}

// ---------------------------------------------------------
//  MEMBER FUNCTION : ~Lmp2Nul2
// ---------------------------------------------------------

Lmp2Nul2::~Lmp2Nul2()
{
  s_logger->repx(logga::adhc, "destructor call", "");
}

// ---------------------------------------------------------
//  MEMBER FUNCTION : uploadEngineering
// ---------------------------------------------------------

Lmp2Nul2::index_type
Lmp2Nul2::uploadEngineering()
{
  // initial reporting
  s_logger->repx(logga::xtra, "entering member function", "");

  // PREAMBLE

  // create and fill a label object
  Label lab(d_label);

  // create a stand-in specific costs cost set
  const CostSet zeroSpecCosts(0.0);

  // EXPOSED VARIABLES

  // create some variables covering input and output -- the zero
  // specific costs are, of course, fine
  const int cab1Col  = pushObj(zeroSpecCosts, lab.str("cable-bi"));
  const int soc1Col  = pushObj(zeroSpecCosts, lab.str("socket-bi"));
  const int thetaCol = pushObj(zeroSpecCosts, lab.str("theta"));

  // load the local col indexes into a 3-tuple
  d_cols = boost::make_tuple(cab1Col,
                             soc1Col,
                             thetaCol);

  // allow 'theta' to be negative
  openBnds(thetaCol);

  // NODE BALANCE

  // bidirectionalize the cable and socket
  openBnds(cab1Col);
  openBnds(soc1Col);

  // create the node balance
  const int nodeRow = pushRhs(0.0, svif::E, lab.str("node-bal"));

  // load the local row and theta col indexes into 'int's
  d_row      = nodeRow;
  d_thetaGol = globalcol(thetaCol);

  // add the exposed variables to the flow balance
  d_cofCount = pushCof(nodeRow, cab1Col, +1.0);
  d_cofCount = pushCof(nodeRow, soc1Col, -1.0);

  // GLOBALIZE AND RETURN

  // note that external usage requires global indexing
  return globalcols(d_cols);
}

// ---------------------------------------------------------
//  MEMBER FUNCTION : downloadSolution
// ---------------------------------------------------------

Lmp2Nul2::results_type
Lmp2Nul2::downloadSolution() const
{
  // initial reporting
  s_logger->repx(logga::xtra, "entering member function", "");

  // active code
  const double voltageAngleDeg = xeona::radian2degree(downloadVar(d_cols.get<2>()));
  return boost::make_tuple(downloadVar(d_cols.get<0>()),    // either would do [1]
                           voltageAngleDeg,
                           downloadSlack(d_row));

  // [1] (added later) but if you swap to <1> then check the sign
  // remains correct
}

// ---------------------------------------------------------
//  CLASS           : Lmp2Cab2
// ---------------------------------------------------------

// ---------------------------------------------------------
//  MEMBER FUNCTION : Lmp2Cab2
// ---------------------------------------------------------

Lmp2Cab2::Lmp2Cab2
(shared_ptr<svif::SolverIf> solver,
 const xeona::DomainMode    commitmentMode) :
  Lmp2NodeOsp(solver, commitmentMode, "lmp2-cable+two-bilateral"),
  d_cols(),                                  // tuple ctor calls default ctors
  d_row(0)
{
  s_logger->repx(logga::adhc, "constructor call, description", d_ospDesc);
}

// ---------------------------------------------------------
//  MEMBER FUNCTION : ~Lmp2Cab2
// ---------------------------------------------------------

Lmp2Cab2::~Lmp2Cab2()
{
  s_logger->repx(logga::adhc, "destructor call", "");
}

// ---------------------------------------------------------
//  MEMBER FUNCTION : uploadEngineering
// ---------------------------------------------------------

Lmp2Cab2::index_type
Lmp2Cab2::uploadEngineering()
{
  // initial reporting
  s_logger->repx(logga::xtra, "entering member function", "");

  // PREAMBLE

  // create and fill a label object
  Label lab(d_label);

  // create a stand-in specific costs cost set
  const CostSet zeroSpecCosts(0.0);

  // EXPOSED VARIABLES

  // create some variables covering input and output -- the zero
  // specific costs are, of course, fine
  const int cab1Col  = pushObj(zeroSpecCosts, lab.str("cable-1"));
  const int cab2Col  = pushObj(zeroSpecCosts, lab.str("cable-2-bi"));
  const int soc1Col  = pushObj(zeroSpecCosts, lab.str("socket-bi"));
  const int thetaCol = pushObj(zeroSpecCosts, lab.str("theta"));

  // load the local col indexes into a 4-tuple
  d_cols = boost::make_tuple(cab1Col,
                             cab2Col,
                             soc1Col,
                             thetaCol);

  // allow 'theta' to be negative
  openBnds(thetaCol);

  // NODE BALANCE

  // bidirectionalize the cable and socket
  openBnds(cab2Col);
  openBnds(soc1Col);

  // create the node balance
  const int nodeRow = pushRhs(0.0, svif::E, lab.str("node-bal"));

  // load the local row and theta col indexes into 'int's
  d_row      = nodeRow;
  d_thetaGol = globalcol(thetaCol);

  // add the exposed variables to the flow balance
  d_cofCount = pushCof(nodeRow, cab1Col, +1.0);
  d_cofCount = pushCof(nodeRow, cab2Col, +1.0);
  d_cofCount = pushCof(nodeRow, soc1Col, -1.0);

  // GLOBALIZE AND RETURN

  // note that external usage requires global indexing
  return globalcols(d_cols);
}

// ---------------------------------------------------------
//  MEMBER FUNCTION : downloadSolution
// ---------------------------------------------------------

Lmp2Cab2::results_type
Lmp2Cab2::downloadSolution() const
{
  // initial reporting
  s_logger->repx(logga::xtra, "entering member function", "");

  // active code
  const double voltageAngleDeg = xeona::radian2degree(downloadVar(d_cols.get<3>()));
  return boost::make_tuple(downloadVar(d_cols.get<0>()),
                           voltageAngleDeg,
                           downloadSlack(d_row));
}

// ---------------------------------------------------------
//  CLASS           : Lmp2Soc2
// ---------------------------------------------------------

// ---------------------------------------------------------
//  MEMBER FUNCTION : Lmp2Soc2
// ---------------------------------------------------------

Lmp2Soc2::Lmp2Soc2
(shared_ptr<svif::SolverIf> solver,
 const xeona::DomainMode    commitmentMode) :
  Lmp2NodeOsp(solver, commitmentMode, "lmp2-socket+two-bilateral"),
  d_cols(),                                  // tuple ctor calls default ctors
  d_row(0)
{
  s_logger->repx(logga::adhc, "constructor call, description", d_ospDesc);
}

// ---------------------------------------------------------
//  MEMBER FUNCTION : ~Lmp2Soc2
// ---------------------------------------------------------

Lmp2Soc2::~Lmp2Soc2()
{
  s_logger->repx(logga::adhc, "destructor call", "");
}

// ---------------------------------------------------------
//  MEMBER FUNCTION : uploadEngineering
// ---------------------------------------------------------

Lmp2Soc2::index_type
Lmp2Soc2::uploadEngineering()
{
  // initial reporting
  s_logger->repx(logga::xtra, "entering member function", "");

  // PREAMBLE

  // create and fill a label object
  Label lab(d_label);

  // create a stand-in specific costs cost set
  const CostSet zeroSpecCosts(0.0);

  // EXPOSED VARIABLES

  // create some variables covering input and output -- the zero
  // specific costs are, of course, fine
  const int cab1Col  = pushObj(zeroSpecCosts, lab.str("cable-bi"));
  const int soc1Col  = pushObj(zeroSpecCosts, lab.str("socket-1"));
  const int soc2Col  = pushObj(zeroSpecCosts, lab.str("socket-2-bi"));
  const int thetaCol = pushObj(zeroSpecCosts, lab.str("theta"));

  // load the local col indexes into a 4-tuple
  d_cols = boost::make_tuple(cab1Col,
                             soc1Col,
                             soc2Col,
                             thetaCol);

  // allow 'theta' to be negative
  openBnds(thetaCol);

  // NODE BALANCE

  // bidirectionalize the cable and socket
  openBnds(cab1Col);
  openBnds(soc2Col);

  // create the node balance
  const int nodeRow = pushRhs(0.0, svif::E, lab.str("node-bal"));

  // load the local row and theta col indexes into 'int's
  d_row      = nodeRow;
  d_thetaGol = globalcol(thetaCol);

  // add the exposed variables to the flow balance
  d_cofCount = pushCof(nodeRow, cab1Col, +1.0);
  d_cofCount = pushCof(nodeRow, soc1Col, -1.0);
  d_cofCount = pushCof(nodeRow, soc2Col, -1.0);

  // GLOBALIZE AND RETURN

  // note that external usage requires global indexing
  return globalcols(d_cols);
}

// ---------------------------------------------------------
//  MEMBER FUNCTION : downloadSolution
// ---------------------------------------------------------

Lmp2Soc2::results_type
Lmp2Soc2::downloadSolution() const
{
  // initial reporting
  s_logger->repx(logga::xtra, "entering member function", "");

  // active code
  const double voltageAngleDeg = xeona::radian2degree(downloadVar(d_cols.get<3>()));
  return boost::make_tuple(downloadVar(d_cols.get<1>()),
                           voltageAngleDeg,
                           downloadSlack(d_row));
}

// ---------------------------------------------------------
//  CLASS           : Lmp2Nul3A
// ---------------------------------------------------------

// ---------------------------------------------------------
//  MEMBER FUNCTION : Lmp2Nul3A
// ---------------------------------------------------------

Lmp2Nul3A::Lmp2Nul3A
(shared_ptr<svif::SolverIf> solver,
 const xeona::DomainMode    commitmentMode) :
  Lmp2NodeOsp(solver, commitmentMode, "lmp2-three-type-a"),
  d_cols(),                                  // tuple ctor calls default ctors
  d_row(0)
{
  s_logger->repx(logga::adhc, "constructor call, description", d_ospDesc);
}

// ---------------------------------------------------------
//  MEMBER FUNCTION : ~Lmp2Nul3A
// ---------------------------------------------------------

Lmp2Nul3A::~Lmp2Nul3A()
{
  s_logger->repx(logga::adhc, "destructor call", "");
}

// ---------------------------------------------------------
//  MEMBER FUNCTION : uploadEngineering
// ---------------------------------------------------------

Lmp2Nul3A::index_type
Lmp2Nul3A::uploadEngineering()
{
  // initial reporting
  s_logger->repx(logga::xtra, "entering member function", "");

  // PREAMBLE

  // create and fill a label object
  Label lab(d_label);

  // create a stand-in specific costs cost set
  const CostSet zeroSpecCosts(0.0);

  // EXPOSED VARIABLES

  // create some variables covering input and output -- the zero
  // specific costs are, of course, fine
  const int cab1Col  = pushObj(zeroSpecCosts, lab.str("cable-1-bi"));
  const int cab2Col  = pushObj(zeroSpecCosts, lab.str("cable-2-bi"));
  const int soc1Col  = pushObj(zeroSpecCosts, lab.str("socket-bi"));
  const int thetaCol = pushObj(zeroSpecCosts, lab.str("theta"));

  // load the local col indexes into a 4-tuple
  d_cols = boost::make_tuple(cab1Col,
                             cab2Col,
                             soc1Col,
                             thetaCol);

  // allow 'theta' to be negative
  openBnds(thetaCol);

  // NODE BALANCE

  // bidirectionalize the cables and socket
  openBnds(cab1Col);
  openBnds(cab2Col);
  openBnds(soc1Col);

  // create the node balance
  const int nodeRow = pushRhs(0.0, svif::E, lab.str("node-bal"));

  // load the local row and theta col indexes into 'int's
  d_row      = nodeRow;
  d_thetaGol = globalcol(thetaCol);

  // add the exposed variables to the flow balance
  d_cofCount = pushCof(nodeRow, cab1Col, +1.0);
  d_cofCount = pushCof(nodeRow, cab2Col, +1.0);
  d_cofCount = pushCof(nodeRow, soc1Col, -1.0);

  // GLOBALIZE AND RETURN

  // note that external usage requires global indexing
  return globalcols(d_cols);
}

// ---------------------------------------------------------
//  MEMBER FUNCTION : downloadSolution
// ---------------------------------------------------------

Lmp2Nul3A::results_type
Lmp2Nul3A::downloadSolution() const
{
  // initial reporting
  s_logger->repx(logga::xtra, "entering member function", "");

  // active code
  const double voltageAngleDeg = xeona::radian2degree(downloadVar(d_cols.get<3>()));
  return boost::make_tuple(downloadVar(d_cols.get<0>()),
                           downloadVar(d_cols.get<1>()),
                           downloadVar(d_cols.get<2>()),
                           voltageAngleDeg,
                           downloadSlack(d_row));
}

// ---------------------------------------------------------
//  CLASS           : Lmp2Nul3B
// ---------------------------------------------------------

// ---------------------------------------------------------
//  MEMBER FUNCTION : Lmp2Nul3B
// ---------------------------------------------------------

Lmp2Nul3B::Lmp2Nul3B
(shared_ptr<svif::SolverIf> solver,
 const xeona::DomainMode    commitmentMode) :
  Lmp2NodeOsp(solver, commitmentMode, "lmp2-three-type-b"),
  d_cols(),                                  // tuple ctor calls default ctors
  d_row(0)
{
  s_logger->repx(logga::adhc, "constructor call, description", d_ospDesc);
}

// ---------------------------------------------------------
//  MEMBER FUNCTION : ~Lmp2Nul3B
// ---------------------------------------------------------

Lmp2Nul3B::~Lmp2Nul3B()
{
  s_logger->repx(logga::adhc, "destructor call", "");
}

// ---------------------------------------------------------
//  MEMBER FUNCTION : uploadEngineering
// ---------------------------------------------------------

Lmp2Nul3B::index_type
Lmp2Nul3B::uploadEngineering()
{
  // initial reporting
  s_logger->repx(logga::xtra, "entering member function", "");

  // PREAMBLE

  // create and fill a label object
  Label lab(d_label);

  // create a stand-in specific costs cost set
  const CostSet zeroSpecCosts(0.0);

  // EXPOSED VARIABLES

  // create some variables covering input and output -- the zero
  // specific costs are, of course, fine
  const int cabCol   = pushObj(zeroSpecCosts, lab.str("cable-bi"));
  const int soc1Col  = pushObj(zeroSpecCosts, lab.str("socket-1-bi"));
  const int soc2Col  = pushObj(zeroSpecCosts, lab.str("socket-2-bi"));
  const int thetaCol = pushObj(zeroSpecCosts, lab.str("theta"));

  // load the local col indexes into a 4-tuple
  d_cols = boost::make_tuple(cabCol,
                             soc1Col,
                             soc2Col,
                             thetaCol);

  // allow 'theta' to be negative
  openBnds(thetaCol);

  // NODE BALANCE

  // bidirectionalize the cable and sockets
  openBnds(cabCol);
  openBnds(soc1Col);
  openBnds(soc2Col);

  // create the node balance
  const int nodeRow = pushRhs(0.0, svif::E, lab.str("node-bal"));

  // load the local row and theta col indexes into 'int's
  d_row      = nodeRow;
  d_thetaGol = globalcol(thetaCol);

  // add the exposed variables to the flow balance
  d_cofCount = pushCof(nodeRow, cabCol,  +1.0);
  d_cofCount = pushCof(nodeRow, soc1Col, +1.0);
  d_cofCount = pushCof(nodeRow, soc2Col, -1.0);

  // GLOBALIZE AND RETURN

  // note that external usage requires global indexing
  return globalcols(d_cols);
}

// ---------------------------------------------------------
//  MEMBER FUNCTION : downloadSolution
// ---------------------------------------------------------

Lmp2Nul3B::results_type
Lmp2Nul3B::downloadSolution() const
{
  // initial reporting
  s_logger->repx(logga::xtra, "entering member function", "");

  // active code
  const double voltageAngleDeg = xeona::radian2degree(downloadVar(d_cols.get<3>()));
  return boost::make_tuple(downloadVar(d_cols.get<0>()),
                           downloadVar(d_cols.get<1>()),
                           downloadVar(d_cols.get<2>()),
                           voltageAngleDeg,
                           downloadSlack(d_row));
}

//  end of file

