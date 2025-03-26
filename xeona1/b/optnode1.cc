//  XEONA ENERGY SYSTEMS MODELING ENVIRONMENT
//
//  file-create-name : optnode.cc
//  file-create-date : Tue 03-Nov-2009 19:49 UTC
//  file-initiator   : Robbie Morrison <robbie@actrix.co.nz>
//
//  file-role        : node optimization sub-problems for LMP nodes / implementation
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
//  $URL: file:///home/robbie/svn-root/xeona/futz/trunk/xeona1/b/optnode.cc $
//
//  GENERAL NOTES FOR THIS FILE

//  LOCAL AND SYSTEM INCLUDES

#include "optnode1.h"         // companion header for this file (place first)

#include "../c/label.h"       // helper class to format solver labels
#include "../c/costs.h"       // cost sets and support

#include "../a/logger.h"      // standard logging functionality (as required)
#include "../c/smart_ptr.h"   // toggle between Boost and TR1 smart pointers
#include ".././common.h"      // common definitions for project (place last)

#include <string>             // C++ strings
#include <sstream>            // string-streams

//  CODE

// ---------------------------------------------------------
//  CLASS           : LmpCabSoc0
// ---------------------------------------------------------

// ---------------------------------------------------------
//  MEMBER FUNCTION : LmpCabSoc0
// ---------------------------------------------------------

LmpCabSoc0::LmpCabSoc0
(shared_ptr<svif::SolverIf> solver,
 const xeona::DomainMode    commitmentMode) :
  LmpNodeOsp(solver, commitmentMode, "lmp-cable+socket"),
  d_cols(),                                  // tuple ctor calls default ctors
  d_row(0)
{
  s_logger->repx(logga::adhc, "constructor call, description", d_ospDesc);
}

// ---------------------------------------------------------
//  MEMBER FUNCTION : ~LmpCabSoc0
// ---------------------------------------------------------

LmpCabSoc0::~LmpCabSoc0()
{
  s_logger->repx(logga::adhc, "destructor call", "");
}

// ---------------------------------------------------------
//  MEMBER FUNCTION : uploadEngineering
// ---------------------------------------------------------

LmpCabSoc0::index_type
LmpCabSoc0::uploadEngineering()
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

  // load the local row index into an int
  d_row = nodeRow;

  // add the exposed variables
  d_cofCount        = pushCof(nodeRow, cabCol, +1.0);
  d_cofCount        = pushCof(nodeRow, socCol, -1.0);

  // GLOBALIZE AND RETURN

  // note that external usage requires global indexing
  return globalcols(d_cols);
}

// ---------------------------------------------------------
//  MEMBER FUNCTION : downloadSolution
// ---------------------------------------------------------

LmpCabSoc0::results_type
LmpCabSoc0::downloadSolution() const
{
  // initial reporting
  s_logger->repx(logga::xtra, "entering member function", "");

  // active code
  return boost::make_tuple(downloadVar(d_cols.get<0>()),
                           downloadVar(d_cols.get<1>()),
                           downloadSlack(d_row));
}

// ---------------------------------------------------------
//  CLASS           : LmpCab1A
// ---------------------------------------------------------

// ---------------------------------------------------------
//  MEMBER FUNCTION : LmpCab1A
// ---------------------------------------------------------

LmpCab1A::LmpCab1A
(shared_ptr<svif::SolverIf> solver,
 const xeona::DomainMode    commitmentMode) :
  LmpNodeOsp(solver, commitmentMode, "lmp-cable+bilateral"),
  d_cols(),                                  // tuple ctor calls default ctors
  d_row(0)
{
  s_logger->repx(logga::adhc, "constructor call, description", d_ospDesc);
}

// ---------------------------------------------------------
//  MEMBER FUNCTION : ~LmpCab1A
// ---------------------------------------------------------

LmpCab1A::~LmpCab1A()
{
  s_logger->repx(logga::adhc, "destructor call", "");
}

// ---------------------------------------------------------
//  MEMBER FUNCTION : uploadEngineering
// ---------------------------------------------------------

LmpCab1A::index_type
LmpCab1A::uploadEngineering()
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
  const int cab1Col = pushObj(zeroSpecCosts, lab.str("cable"));
  const int cab2Col = pushObj(zeroSpecCosts, lab.str("cable-bi"));

  // load the local col indexes into a 2-tuple
  d_cols = boost::make_tuple(cab1Col,
                             cab2Col);

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

  // load the local row index into an int
  d_row = nodeRow;

  // add the exposed variables
  d_cofCount        = pushCof(nodeRow, cab1Col, +1.0);
  d_cofCount        = pushCof(nodeRow, cab2Col, +1.0);

  // GLOBALIZE AND RETURN

  // note that external usage requires global indexing
  return globalcols(d_cols);
}

// ---------------------------------------------------------
//  MEMBER FUNCTION : downloadSolution
// ---------------------------------------------------------

LmpCab1A::results_type
LmpCab1A::downloadSolution() const
{
  // initial reporting
  s_logger->repx(logga::xtra, "entering member function", "");

  // active code
  return boost::make_tuple(downloadVar(d_cols.get<0>()),
                           downloadSlack(d_row));
}

// ---------------------------------------------------------
//  CLASS           : LmpCab1B
// ---------------------------------------------------------

// ---------------------------------------------------------
//  MEMBER FUNCTION : LmpCab1B
// ---------------------------------------------------------

LmpCab1B::LmpCab1B
(shared_ptr<svif::SolverIf> solver,
 const xeona::DomainMode    commitmentMode) :
  LmpNodeOsp(solver, commitmentMode, "lmp-cable+bilateral"),
  d_cols(),                                  // tuple ctor calls default ctors
  d_row(0)
{
  s_logger->repx(logga::adhc, "constructor call, description", d_ospDesc);
}

// ---------------------------------------------------------
//  MEMBER FUNCTION : ~LmpCab1B
// ---------------------------------------------------------

LmpCab1B::~LmpCab1B()
{
  s_logger->repx(logga::adhc, "destructor call", "");
}

// ---------------------------------------------------------
//  MEMBER FUNCTION : uploadEngineering
// ---------------------------------------------------------

LmpCab1B::index_type
LmpCab1B::uploadEngineering()
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
  const int cab1Col = pushObj(zeroSpecCosts, lab.str("cable"));
  const int soc1Col = pushObj(zeroSpecCosts, lab.str("socket-bi"));

  // load the local col indexes into a 2-tuple
  d_cols = boost::make_tuple(cab1Col,
                             soc1Col);

  // NODE BALANCE

  // bidirectionalize the socket
  const int bndChanges = openBnds(soc1Col);

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

  // load the local row index into an int
  d_row = nodeRow;

  // add the exposed variables
  d_cofCount        = pushCof(nodeRow, cab1Col, +1.0);
  d_cofCount        = pushCof(nodeRow, soc1Col, -1.0);

  // GLOBALIZE AND RETURN

  // note that external usage requires global indexing
  return globalcols(d_cols);
}

// ---------------------------------------------------------
//  MEMBER FUNCTION : downloadSolution
// ---------------------------------------------------------

LmpCab1B::results_type
LmpCab1B::downloadSolution() const
{
  // initial reporting
  s_logger->repx(logga::xtra, "entering member function", "");

  // active code
  return boost::make_tuple(downloadVar(d_cols.get<0>()),
                           downloadSlack(d_row));
}

// ---------------------------------------------------------
//  CLASS           : LmpSoc1A
// ---------------------------------------------------------

// ---------------------------------------------------------
//  MEMBER FUNCTION : LmpSoc1A
// ---------------------------------------------------------

LmpSoc1A::LmpSoc1A
(shared_ptr<svif::SolverIf> solver,
 const xeona::DomainMode    commitmentMode) :
  LmpNodeOsp(solver, commitmentMode, "lmp-socket+bilateral"),
  d_cols(),                                  // tuple ctor calls default ctors
  d_row(0)
{
  s_logger->repx(logga::adhc, "constructor call, description", d_ospDesc);
}

// ---------------------------------------------------------
//  MEMBER FUNCTION : ~LmpSoc1A
// ---------------------------------------------------------

LmpSoc1A::~LmpSoc1A()
{
  s_logger->repx(logga::adhc, "destructor call", "");
}

// ---------------------------------------------------------
//  MEMBER FUNCTION : uploadEngineering
// ---------------------------------------------------------

LmpSoc1A::index_type
LmpSoc1A::uploadEngineering()
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

  // load the local col indexes into a 2-tuple
  d_cols = boost::make_tuple(cab1Col,
                             soc1Col);

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
      std::clog << "** coding error 01 in source file " << __FILE__ << std::endl;
      break;
    }

  // create the node balance
  const int nodeRow = pushRhs(0.0, svif::E, lab.str("node-bal"));

  // load the local row index into an int
  d_row = nodeRow;

  // add the exposed variables
  d_cofCount        = pushCof(nodeRow, cab1Col, +1.0);
  d_cofCount        = pushCof(nodeRow, soc1Col, -1.0);

  // GLOBALIZE AND RETURN

  // note that external usage requires global indexing
  return globalcols(d_cols);
}

// ---------------------------------------------------------
//  MEMBER FUNCTION : downloadSolution
// ---------------------------------------------------------

LmpSoc1A::results_type
LmpSoc1A::downloadSolution() const
{
  // initial reporting
  s_logger->repx(logga::xtra, "entering member function", "");

  // active code
  return boost::make_tuple(downloadVar(d_cols.get<0>()),
                           downloadSlack(d_row));
}

// ---------------------------------------------------------
//  CLASS           : LmpSoc1B
// ---------------------------------------------------------

// ---------------------------------------------------------
//  MEMBER FUNCTION : LmpSoc1B
// ---------------------------------------------------------

LmpSoc1B::LmpSoc1B
(shared_ptr<svif::SolverIf> solver,
 const xeona::DomainMode    commitmentMode) :
  LmpNodeOsp(solver, commitmentMode, "lmp-socket+bilateral"),
  d_cols(),                                  // tuple ctor calls default ctors
  d_row(0)
{
  s_logger->repx(logga::adhc, "constructor call, description", d_ospDesc);
}

// ---------------------------------------------------------
//  MEMBER FUNCTION : ~LmpSoc1B
// ---------------------------------------------------------

LmpSoc1B::~LmpSoc1B()
{
  s_logger->repx(logga::adhc, "destructor call", "");
}

// ---------------------------------------------------------
//  MEMBER FUNCTION : uploadEngineering
// ---------------------------------------------------------

LmpSoc1B::index_type
LmpSoc1B::uploadEngineering()
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
  const int soc1Col = pushObj(zeroSpecCosts, lab.str("socket-bi"));
  const int soc2Col = pushObj(zeroSpecCosts, lab.str("socket"));

  // load the local col indexes into a 2-tuple
  d_cols = boost::make_tuple(soc1Col,
                             soc2Col);

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

  // load the local row index into an int
  d_row = nodeRow;

  // add the exposed variables
  d_cofCount        = pushCof(nodeRow, soc1Col, -1.0);
  d_cofCount        = pushCof(nodeRow, soc2Col, -1.0);

  // GLOBALIZE AND RETURN

  // note that external usage requires global indexing
  return globalcols(d_cols);
}

// ---------------------------------------------------------
//  MEMBER FUNCTION : downloadSolution
// ---------------------------------------------------------

LmpSoc1B::results_type
LmpSoc1B::downloadSolution() const
{
  // initial reporting
  s_logger->repx(logga::xtra, "entering member function", "");

  // active code
  return boost::make_tuple(downloadVar(d_cols.get<0>()),
                           downloadSlack(d_row));
}

// ---------------------------------------------------------
//  CLASS           : LmpNul2
// ---------------------------------------------------------

// ---------------------------------------------------------
//  MEMBER FUNCTION : LmpNul2
// ---------------------------------------------------------

LmpNul2::LmpNul2
(shared_ptr<svif::SolverIf> solver,
 const xeona::DomainMode    commitmentMode) :
  LmpNodeOsp(solver, commitmentMode, "lmp-two-bilateral"),
  d_cols(),                                  // tuple ctor calls default ctors
  d_row(0)
{
  s_logger->repx(logga::adhc, "constructor call, description", d_ospDesc);
}

// ---------------------------------------------------------
//  MEMBER FUNCTION : ~LmpNul2
// ---------------------------------------------------------

LmpNul2::~LmpNul2()
{
  s_logger->repx(logga::adhc, "destructor call", "");
}

// ---------------------------------------------------------
//  MEMBER FUNCTION : uploadEngineering
// ---------------------------------------------------------

LmpNul2::index_type
LmpNul2::uploadEngineering()
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
  const int soc1Col = pushObj(zeroSpecCosts, lab.str("socket-bi"));

  // load the local col indexes into a 2-tuple
  d_cols = boost::make_tuple(cab1Col,
                             soc1Col);

  // NODE BALANCE

  // bidirectionalize the cable and socket
  openBnds(cab1Col);
  openBnds(soc1Col);

  // create the node balance
  const int nodeRow = pushRhs(0.0, svif::E, lab.str("node-bal"));

  // load the local row index into an int
  d_row = nodeRow;

  // add the exposed variables
  d_cofCount        = pushCof(nodeRow, cab1Col, +1.0);
  d_cofCount        = pushCof(nodeRow, soc1Col, -1.0);

  // GLOBALIZE AND RETURN

  // note that external usage requires global indexing
  return globalcols(d_cols);
}

// ---------------------------------------------------------
//  MEMBER FUNCTION : downloadSolution
// ---------------------------------------------------------

LmpNul2::results_type
LmpNul2::downloadSolution() const
{
  // initial reporting
  s_logger->repx(logga::xtra, "entering member function", "");

  // active code
  return boost::make_tuple(downloadVar(d_cols.get<0>()),    // either would do [1]
                           downloadSlack(d_row));

  // [1] (added later) but if you swap to <1> then check the sign
  // remains correct
}

// ---------------------------------------------------------
//  CLASS           : LmpCab2
// ---------------------------------------------------------

// ---------------------------------------------------------
//  MEMBER FUNCTION : LmpCab2
// ---------------------------------------------------------

LmpCab2::LmpCab2
(shared_ptr<svif::SolverIf> solver,
 const xeona::DomainMode    commitmentMode) :
  LmpNodeOsp(solver, commitmentMode, "lmp-cable+two-bilateral"),
  d_cols(),                                  // tuple ctor calls default ctors
  d_row(0)
{
  s_logger->repx(logga::adhc, "constructor call, description", d_ospDesc);
}

// ---------------------------------------------------------
//  MEMBER FUNCTION : ~LmpCab2
// ---------------------------------------------------------

LmpCab2::~LmpCab2()
{
  s_logger->repx(logga::adhc, "destructor call", "");
}

// ---------------------------------------------------------
//  MEMBER FUNCTION : uploadEngineering
// ---------------------------------------------------------

LmpCab2::index_type
LmpCab2::uploadEngineering()
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
  const int cab1Col = pushObj(zeroSpecCosts, lab.str("cable-1"));
  const int cab2Col = pushObj(zeroSpecCosts, lab.str("cable-2-bi"));
  const int soc1Col = pushObj(zeroSpecCosts, lab.str("socket-bi"));

  // load the local col indexes into a 3-tuple
  d_cols = boost::make_tuple(cab1Col,
                             cab2Col,
                             soc1Col);

  // NODE BALANCE

  // bidirectionalize the cable and socket
  openBnds(cab2Col);
  openBnds(soc1Col);

  // create the node balance
  const int nodeRow = pushRhs(0.0, svif::E, lab.str("node-bal"));

  // load the local row index into an int
  d_row = nodeRow;

  // add the exposed variables
  d_cofCount        = pushCof(nodeRow, cab1Col, +1.0);
  d_cofCount        = pushCof(nodeRow, cab2Col, +1.0);
  d_cofCount        = pushCof(nodeRow, soc1Col, -1.0);

  // GLOBALIZE AND RETURN

  // note that external usage requires global indexing
  return globalcols(d_cols);
}

// ---------------------------------------------------------
//  MEMBER FUNCTION : downloadSolution
// ---------------------------------------------------------

LmpCab2::results_type
LmpCab2::downloadSolution() const
{
  // initial reporting
  s_logger->repx(logga::xtra, "entering member function", "");

  // active code
  return boost::make_tuple(downloadVar(d_cols.get<0>()),
                           downloadSlack(d_row));
}

// ---------------------------------------------------------
//  CLASS           : LmpSoc2
// ---------------------------------------------------------

// ---------------------------------------------------------
//  MEMBER FUNCTION : LmpSoc2
// ---------------------------------------------------------

LmpSoc2::LmpSoc2
(shared_ptr<svif::SolverIf> solver,
 const xeona::DomainMode    commitmentMode) :
  LmpNodeOsp(solver, commitmentMode, "lmp-socket+two-bilateral"),
  d_cols(),                                  // tuple ctor calls default ctors
  d_row(0)
{
  s_logger->repx(logga::adhc, "constructor call, description", d_ospDesc);
}

// ---------------------------------------------------------
//  MEMBER FUNCTION : ~LmpSoc2
// ---------------------------------------------------------

LmpSoc2::~LmpSoc2()
{
  s_logger->repx(logga::adhc, "destructor call", "");
}

// ---------------------------------------------------------
//  MEMBER FUNCTION : uploadEngineering
// ---------------------------------------------------------

LmpSoc2::index_type
LmpSoc2::uploadEngineering()
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
  const int soc1Col = pushObj(zeroSpecCosts, lab.str("socket-1"));
  const int soc2Col = pushObj(zeroSpecCosts, lab.str("socket-2-bi"));

  // load the local col indexes into a 3-tuple
  d_cols = boost::make_tuple(cab1Col,
                             soc1Col,
                             soc2Col);

  // NODE BALANCE

  // bidirectionalize the cable and socket
  openBnds(cab1Col);
  openBnds(soc2Col);

  // create the node balance
  const int nodeRow = pushRhs(0.0, svif::E, lab.str("node-bal"));

  // load the local row index into an int
  d_row = nodeRow;

  // add the exposed variables
  d_cofCount        = pushCof(nodeRow, cab1Col, +1.0);
  d_cofCount        = pushCof(nodeRow, soc1Col, -1.0);
  d_cofCount        = pushCof(nodeRow, soc2Col, -1.0);

  // GLOBALIZE AND RETURN

  // note that external usage requires global indexing
  return globalcols(d_cols);
}

// ---------------------------------------------------------
//  MEMBER FUNCTION : downloadSolution
// ---------------------------------------------------------

LmpSoc2::results_type
LmpSoc2::downloadSolution() const
{
  // initial reporting
  s_logger->repx(logga::xtra, "entering member function", "");

  // active code
  return boost::make_tuple(downloadVar(d_cols.get<1>()),
                           downloadSlack(d_row));
}

// ---------------------------------------------------------
//  CLASS           : LmpNul3A
// ---------------------------------------------------------

// ---------------------------------------------------------
//  MEMBER FUNCTION : LmpNul3A
// ---------------------------------------------------------

LmpNul3A::LmpNul3A
(shared_ptr<svif::SolverIf> solver,
 const xeona::DomainMode    commitmentMode) :
  LmpNodeOsp(solver, commitmentMode, "lmp-three-type-a"),
  d_cols(),                                  // tuple ctor calls default ctors
  d_row(0)
{
  s_logger->repx(logga::adhc, "constructor call, description", d_ospDesc);
}

// ---------------------------------------------------------
//  MEMBER FUNCTION : ~LmpNul3A
// ---------------------------------------------------------

LmpNul3A::~LmpNul3A()
{
  s_logger->repx(logga::adhc, "destructor call", "");
}

// ---------------------------------------------------------
//  MEMBER FUNCTION : uploadEngineering
// ---------------------------------------------------------

LmpNul3A::index_type
LmpNul3A::uploadEngineering()
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
  const int cab1Col = pushObj(zeroSpecCosts, lab.str("cable-1-bi"));
  const int cab2Col = pushObj(zeroSpecCosts, lab.str("cable-2-bi"));
  const int soc1Col = pushObj(zeroSpecCosts, lab.str("socket-bi"));

  // load the local col indexes into a 3-tuple
  d_cols = boost::make_tuple(cab1Col,
                             cab2Col,
                             soc1Col);

  // NODE BALANCE

  // bidirectionalize the cables and socket
  openBnds(cab1Col);
  openBnds(cab2Col);
  openBnds(soc1Col);

  // create the node balance
  const int nodeRow = pushRhs(0.0, svif::E, lab.str("node-bal"));

  // load the local row index into an int
  d_row = nodeRow;

  // add the exposed variables
  d_cofCount        = pushCof(nodeRow, cab1Col, +1.0);
  d_cofCount        = pushCof(nodeRow, cab2Col, +1.0);
  d_cofCount        = pushCof(nodeRow, soc1Col, -1.0);

  // GLOBALIZE AND RETURN

  // note that external usage requires global indexing
  return globalcols(d_cols);
}

// ---------------------------------------------------------
//  MEMBER FUNCTION : downloadSolution
// ---------------------------------------------------------

LmpNul3A::results_type
LmpNul3A::downloadSolution() const
{
  // initial reporting
  s_logger->repx(logga::xtra, "entering member function", "");

  // active code
  return boost::make_tuple(downloadVar(d_cols.get<0>()),
                           downloadVar(d_cols.get<1>()),
                           downloadVar(d_cols.get<2>()),
                           downloadSlack(d_row));
}

// ---------------------------------------------------------
//  CLASS           : LmpNul3B
// ---------------------------------------------------------

// ---------------------------------------------------------
//  MEMBER FUNCTION : LmpNul3B
// ---------------------------------------------------------

LmpNul3B::LmpNul3B
(shared_ptr<svif::SolverIf> solver,
 const xeona::DomainMode    commitmentMode) :
  LmpNodeOsp(solver, commitmentMode, "lmp-three-type-b"),
  d_cols(),                                  // tuple ctor calls default ctors
  d_row(0)
{
  s_logger->repx(logga::adhc, "constructor call, description", d_ospDesc);
}

// ---------------------------------------------------------
//  MEMBER FUNCTION : ~LmpNul3B
// ---------------------------------------------------------

LmpNul3B::~LmpNul3B()
{
  s_logger->repx(logga::adhc, "destructor call", "");
}

// ---------------------------------------------------------
//  MEMBER FUNCTION : uploadEngineering
// ---------------------------------------------------------

LmpNul3B::index_type
LmpNul3B::uploadEngineering()
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
  const int cabCol  = pushObj(zeroSpecCosts, lab.str("cable-bi"));
  const int soc1Col = pushObj(zeroSpecCosts, lab.str("socket-1-bi"));
  const int soc2Col = pushObj(zeroSpecCosts, lab.str("socket-2-bi"));

  // load the local col indexes into a 3-tuple
  d_cols = boost::make_tuple(cabCol,
                             soc1Col,
                             soc2Col);

  // NODE BALANCE

  // bidirectionalize the cable and sockets
  openBnds(cabCol);
  openBnds(soc1Col);
  openBnds(soc2Col);

  // create the node balance
  const int nodeRow = pushRhs(0.0, svif::E, lab.str("node-bal"));

  // load the local row index into an int
  d_row = nodeRow;

  // add the exposed variables
  d_cofCount        = pushCof(nodeRow, cabCol,  +1.0);
  d_cofCount        = pushCof(nodeRow, soc1Col, +1.0);
  d_cofCount        = pushCof(nodeRow, soc2Col, -1.0);

  // GLOBALIZE AND RETURN

  // note that external usage requires global indexing
  return globalcols(d_cols);
}

// ---------------------------------------------------------
//  MEMBER FUNCTION : downloadSolution
// ---------------------------------------------------------

LmpNul3B::results_type
LmpNul3B::downloadSolution() const
{
  // initial reporting
  s_logger->repx(logga::xtra, "entering member function", "");

  // active code
  return boost::make_tuple(downloadVar(d_cols.get<0>()),
                           downloadVar(d_cols.get<1>()),
                           downloadVar(d_cols.get<2>()),
                           downloadSlack(d_row));
}

//  end of file

