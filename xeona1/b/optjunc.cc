//  XEONA ENERGY SYSTEMS MODELING ENVIRONMENT
//
//  file-create-name : optjunc.cc
//  file-create-date : Mon 26-Oct-2009 10:26 UTC
//  file-initiator   : Robbie Morrison <robbie@actrix.co.nz>
//
//  file-role        : operations OSPs for junctions / implementation
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
//  $URL: file:///home/robbie/svn-root/xeona/futz/trunk/xeona1/b/optjunc.cc $
//
//  GENERAL NOTES FOR THIS FILE

//  LOCAL AND SYSTEM INCLUDES

#include "optjunc.h"          // companion header for this file (place first)

#include "../c/label.h"       // helper class to format solver labels
#include "../c/costs.h"       // cost sets and support

#include "../a/logger.h"      // standard logging functionality (as required)
#include "../c/smart_ptr.h"   // toggle between Boost and TR1 smart pointers
#include ".././common.h"      // common definitions for project (place last)

#include <string>             // C++ strings
#include <sstream>            // string-streams

#include <cmath>              // C-style maths, abs(), ceil(), floor(), sqrt()

//  CODE

// ---------------------------------------------------------
//  CLASS           : JncSplit2
// ---------------------------------------------------------

// ---------------------------------------------------------
//  MEMBER FUNCTION : JncSplit2
// ---------------------------------------------------------

JncSplit2::JncSplit2
(shared_ptr<svif::SolverIf> solver,
 const xeona::DomainMode    commitmentMode) :
  JunctionOsp(solver, commitmentMode, "jnc-2-cable"),
  d_cols()                                   // tuple ctor calls default ctors
{
  s_logger->repx(logga::adhc, "constructor call, description", d_ospDesc);
}

// ---------------------------------------------------------
//  MEMBER FUNCTION : ~JncSplit2
// ---------------------------------------------------------

JncSplit2::~JncSplit2()
{
  s_logger->repx(logga::adhc, "destructor call", "");
}

// ---------------------------------------------------------
//  MEMBER FUNCTION : uploadEngineering
// ---------------------------------------------------------

JncSplit2::index_type
JncSplit2::uploadEngineering()
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
  const int sockCol  = pushObj(zeroSpecCosts, lab.str("socket"));
  const int cab1Col  = pushObj(zeroSpecCosts, lab.str("cable-1"));
  const int cab2Col  = pushObj(zeroSpecCosts, lab.str("cable-2"));

  // load the local col indexes into a 3-tuple
  d_cols = boost::make_tuple(sockCol,
                             cab1Col,
                             cab2Col);

  // JUNCTION BALANCE

  // create the junction balance
  const int juncRow  = pushRhs(0.0, svif::E, lab.str("junction-bal"));

  // add the exposed variables
  d_cofCount         = pushCof(juncRow, sockCol, +1.0);
  d_cofCount         = pushCof(juncRow, cab1Col, -1.0);
  d_cofCount         = pushCof(juncRow, cab2Col, -1.0);

  // GLOBALIZE AND RETURN

  // note that external usage requires global indexing
  return globalcols(d_cols);
}

// ---------------------------------------------------------
//  MEMBER FUNCTION : downloadSolution
// ---------------------------------------------------------

JncSplit2::results_type
JncSplit2::downloadSolution() const
{
  // initial reporting
  s_logger->repx(logga::xtra, "entering member function", "");

  // active code
  return boost::make_tuple(downloadVar(d_cols.get<0>()));
}

// ---------------------------------------------------------
//  CLASS           : JncSplit3
// ---------------------------------------------------------

// ---------------------------------------------------------
//  MEMBER FUNCTION : JncSplit3
// ---------------------------------------------------------

JncSplit3::JncSplit3
(shared_ptr<svif::SolverIf> solver,
 const xeona::DomainMode    commitmentMode) :
  JunctionOsp(solver, commitmentMode, "jnc-3-cable"),
  d_cols()                                   // tuple ctor calls default ctors
{
  s_logger->repx(logga::adhc, "constructor call, description", d_ospDesc);
}

// ---------------------------------------------------------
//  MEMBER FUNCTION : ~JncSplit3
// ---------------------------------------------------------

JncSplit3::~JncSplit3()
{
  s_logger->repx(logga::adhc, "destructor call", "");
}

// ---------------------------------------------------------
//  MEMBER FUNCTION : uploadEngineering
// ---------------------------------------------------------

JncSplit3::index_type
JncSplit3::uploadEngineering()
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
  const int sockCol  = pushObj(zeroSpecCosts, lab.str("socket"));
  const int cab1Col  = pushObj(zeroSpecCosts, lab.str("cable-1"));
  const int cab2Col  = pushObj(zeroSpecCosts, lab.str("cable-2"));
  const int cab3Col  = pushObj(zeroSpecCosts, lab.str("cable-3"));

  // load the local col indexes into a 4-tuple
  d_cols = boost::make_tuple(sockCol,
                             cab1Col,
                             cab2Col,
                             cab3Col);

  // JUNCTION BALANCE

  // create the junction balance
  const int juncRow  = pushRhs(0.0, svif::E, lab.str("junction-bal"));

  // add the exposed variables
  d_cofCount         = pushCof(juncRow, sockCol, +1.0);
  d_cofCount         = pushCof(juncRow, cab1Col, -1.0);
  d_cofCount         = pushCof(juncRow, cab2Col, -1.0);
  d_cofCount         = pushCof(juncRow, cab3Col, -1.0);

  // GLOBALIZE AND RETURN

  // note that external usage requires global indexing
  return globalcols(d_cols);
}

// ---------------------------------------------------------
//  MEMBER FUNCTION : downloadSolution
// ---------------------------------------------------------

JncSplit3::results_type
JncSplit3::downloadSolution() const
{
  // initial reporting
  s_logger->repx(logga::xtra, "entering member function", "");

  // active code
  return boost::make_tuple(downloadVar(d_cols.get<0>()));
}

// ---------------------------------------------------------
//  CLASS           : JncJoin2
// ---------------------------------------------------------

// ---------------------------------------------------------
//  MEMBER FUNCTION : JncJoin2
// ---------------------------------------------------------

JncJoin2::JncJoin2
(shared_ptr<svif::SolverIf> solver,
 const xeona::DomainMode    commitmentMode) :
  JunctionOsp(solver, commitmentMode, "jnc-2-socket"),
  d_cols()                                   // tuple ctor calls default ctors
{
  s_logger->repx(logga::adhc, "constructor call, description", d_ospDesc);
}

// ---------------------------------------------------------
//  MEMBER FUNCTION : ~JncJoin2
// ---------------------------------------------------------

JncJoin2::~JncJoin2()
{
  s_logger->repx(logga::adhc, "destructor call", "");
}

// ---------------------------------------------------------
//  MEMBER FUNCTION : uploadEngineering
// ---------------------------------------------------------

JncJoin2::index_type
JncJoin2::uploadEngineering()
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
  const int cableCol  = pushObj(zeroSpecCosts, lab.str("cable"));
  const int sock1Col  = pushObj(zeroSpecCosts, lab.str("socket-1"));
  const int sock2Col  = pushObj(zeroSpecCosts, lab.str("socket-2"));

  // load the local col indexes into a 3-tuple
  d_cols = boost::make_tuple(cableCol,
                             sock1Col,
                             sock2Col);

  // JUNCTION BALANCE

  // create the junction balance
  const int juncRow  = pushRhs(0.0, svif::E, lab.str("junction-bal"));

  // add the exposed variables
  d_cofCount         = pushCof(juncRow, cableCol, -1.0);
  d_cofCount         = pushCof(juncRow, sock1Col, +1.0);
  d_cofCount         = pushCof(juncRow, sock2Col, +1.0);

  // GLOBALIZE AND RETURN

  // note that external usage requires global indexing
  return globalcols(d_cols);
}

// ---------------------------------------------------------
//  MEMBER FUNCTION : downloadSolution
// ---------------------------------------------------------

JncJoin2::results_type
JncJoin2::downloadSolution() const
{
  // initial reporting
  s_logger->repx(logga::xtra, "entering member function", "");

  // active code
  return boost::make_tuple(downloadVar(d_cols.get<0>()));
}

// ---------------------------------------------------------
//  CLASS           : JncSym2
// ---------------------------------------------------------

// ---------------------------------------------------------
//  MEMBER FUNCTION : JncSym2
// ---------------------------------------------------------

JncSym2::JncSym2
(shared_ptr<svif::SolverIf> solver,
 const xeona::DomainMode    commitmentMode) :
  JunctionOsp(solver, commitmentMode, "jnc-2-symmetric"),
  d_cols()                                   // tuple ctor calls default ctors
{
  s_logger->repx(logga::adhc, "constructor call, description", d_ospDesc);
}

// ---------------------------------------------------------
//  MEMBER FUNCTION : ~JncSym2
// ---------------------------------------------------------

JncSym2::~JncSym2()
{
  s_logger->repx(logga::adhc, "destructor call", "");
}

// ---------------------------------------------------------
//  MEMBER FUNCTION : uploadEngineering
// ---------------------------------------------------------

JncSym2::index_type
JncSym2::uploadEngineering()
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
  const int cab2Col = pushObj(zeroSpecCosts, lab.str("cable-2"));
  const int soc1Col = pushObj(zeroSpecCosts, lab.str("socket-1"));
  const int soc2Col = pushObj(zeroSpecCosts, lab.str("socket-2"));

  // load the local col indexes into a 4-tuple
  d_cols = boost::make_tuple(cab1Col,
                             cab2Col,
                             soc1Col,
                             soc2Col);

  // JUNCTION BALANCE

  // create the junction balance
  const int juncRow = pushRhs(0.0, svif::E, lab.str("junction-bal"));

  // add the exposed variables
  d_cofCount        = pushCof(juncRow, cab1Col, -1.0);
  d_cofCount        = pushCof(juncRow, cab2Col, -1.0);
  d_cofCount        = pushCof(juncRow, soc1Col, +1.0);
  d_cofCount        = pushCof(juncRow, soc2Col, +1.0);

  // GLOBALIZE AND RETURN

  // note that external usage requires global indexing
  return globalcols(d_cols);
}

// ---------------------------------------------------------
//  MEMBER FUNCTION : downloadSolution
// ---------------------------------------------------------

JncSym2::results_type
JncSym2::downloadSolution() const
{
  // initial reporting
  s_logger->repx(logga::xtra, "entering member function", "");

  // active code
  const double throughput
    = downloadVar(d_cols.get<0>())
    + downloadVar(d_cols.get<1>());

  // debug reporting (could be deleted)
  if ( throughput < 0.0 ) s_logger->repx(logga::warn, "throughput negative", throughput);
  else                    s_logger->repx(logga::adhc, "throughput", throughput);

  // return
  return boost::make_tuple(throughput);
}

// ---------------------------------------------------------
//  CLASS           : JncSuck
// ---------------------------------------------------------

// ---------------------------------------------------------
//  MEMBER FUNCTION : JncSuck
// ---------------------------------------------------------

JncSuck::JncSuck
(shared_ptr<svif::SolverIf> solver,
 const xeona::DomainMode    commitmentMode) :
  JunctionOsp(solver, commitmentMode, "jnc-suck"),
  d_cols()                                   // tuple ctor calls default ctors
{
  s_logger->repx(logga::adhc, "constructor call, description", d_ospDesc);
}

// ---------------------------------------------------------
//  MEMBER FUNCTION : ~JncSuck
// ---------------------------------------------------------

JncSuck::~JncSuck()
{
  s_logger->repx(logga::adhc, "destructor call", "JncSuck");
}

// ---------------------------------------------------------
//  MEMBER FUNCTION : uploadEngineering
// ---------------------------------------------------------

JncSuck::index_type
JncSuck::uploadEngineering
(const double fixedFlow)                     // often zero
{
  // initial reporting
  s_logger->repx(logga::xtra, "entering member function", "JncSuck");

  // integrity check
  if ( fixedFlow < 0.0 )
    {
      s_logger->repx(logga::warn, "negative flow invalid", fixedFlow);
    }

  // PREAMBLE

  // create and fill a label object
  Label lab(d_label);

  // create a stand-in specific costs cost set
  const CostSet zeroSpecCosts(0.0);

  // EXPOSED VARIABLES

  // create exposed variable
  const int cabCol = pushObj(zeroSpecCosts, lab.str("cable"));

  // load the local col indexes into a 1-tuple
  d_cols = boost::make_tuple(cabCol);

  // JUNCTION BALANCE

  // create the junction balance
  const int juncRow = pushRhs(0.0, svif::E, lab.str("junction-bal"));

  // add the exposed variables
  d_cofCount        = pushCof(juncRow, cabCol, +1.0);

  // BLOCKETTES (unusual for a junction)

  // note that the flow blockette is the sole blockette in this
  // OSP -- now create an in variable for the flow blockette
  const int inCol    = pushObj(zeroSpecCosts, lab.str("in-flow"));

  // connect to the cable or socket balance
  d_cofCount         = pushCof(juncRow, inCol, -1.0);

  // add the fixed flow to the blockette (nothing else required)
  const int fixedRow = pushRhs(fixedFlow, svif::E, lab.str("fixed-flow"));
  d_cofCount         = pushCof(fixedRow, inCol, +1.0);

  // GLOBALIZE AND RETURN

  // note that external usage requires global indexing
  return globalcols(d_cols);
}

// ---------------------------------------------------------
//  MEMBER FUNCTION : downloadSolution
// ---------------------------------------------------------

JncSuck::results_type
JncSuck::downloadSolution() const
{
  // initial reporting
  s_logger->repx(logga::xtra, "entering member function", "JncSuck");

  // active code
  return boost::make_tuple(downloadVar(d_cols.get<0>()));
}

// ---------------------------------------------------------
//  CLASS           : JncBlow
// ---------------------------------------------------------

// ---------------------------------------------------------
//  MEMBER FUNCTION : JncBlow
// ---------------------------------------------------------

JncBlow::JncBlow
(shared_ptr<svif::SolverIf> solver,
 const xeona::DomainMode    commitmentMode) :
  JunctionOsp(solver, commitmentMode, "jnc-source"),
  d_cols()                                   // tuple ctor calls default ctors
{
  s_logger->repx(logga::adhc, "constructor call, description", d_ospDesc);
}

// ---------------------------------------------------------
//  MEMBER FUNCTION : ~JncBlow
// ---------------------------------------------------------

JncBlow::~JncBlow()
{
  s_logger->repx(logga::adhc, "destructor call", "JncBlow");
}

// ---------------------------------------------------------
//  MEMBER FUNCTION : uploadEngineering
// ---------------------------------------------------------

JncBlow::index_type
JncBlow::uploadEngineering
  (const double loBound,
   const double hiBound)
{
  // initial reporting
  s_logger->repx(logga::xtra, "entering member function", "JncBlow");

  // integrity checks
  if ( loBound < 0.0 )
    {
      s_logger->repx(logga::warn, "negative low bound invalid", loBound);
    }
  if ( loBound > hiBound )
    {
      std::ostringstream oss;
      oss << loBound << " : " << hiBound;
      s_logger->repx(logga::warn, "low bound exceeds high bound", oss.str());
    }

  // PREAMBLE

  // create and fill a label object
  Label lab(d_label);

  // create a stand-in specific costs cost set
  const CostSet zeroSpecCosts(0.0);

  // EXPOSED VARIABLES

  // create exposed variable
  const int socCol = pushObj(zeroSpecCosts, lab.str("socket"));

  // load the local col indexes into a 1-tuple
  d_cols = boost::make_tuple(socCol);

  // JUNCTION BALANCE

  // create the junction balance
  const int juncRow = pushRhs(0.0, svif::E, lab.str("junction-bal"));

  // add the exposed variables
  d_cofCount        = pushCof(juncRow, socCol, +1.0);

  // BLOCKETTES (unusual for a junction)

  // note that the blow blockette is the sole blockette in this
  // OSP -- now create an out variable for the blow blockette
  const int outCol  = pushObj(zeroSpecCosts, lab.str("out-flow"));

  // connect to the cable or socket balance
  d_cofCount        = pushCof(juncRow, outCol, -1.0);

  // define the lo and hi bounds for the blow blockette
  if ( loBound > 0.0 )
    {
      const int loRow = pushRhs(loBound, svif::G, lab.str("lo-bound"));
      d_cofCount      = pushCof(loRow, outCol, +1.0);
    }
  if ( ! std::isinf(hiBound) )            // refer <cmath>
    {
      const int hiRow = pushRhs(hiBound, svif::L, lab.str("hi-bound"));
      d_cofCount      = pushCof(hiRow, outCol, +1.0);
    }

  // GLOBALIZE AND RETURN

  // note that external usage requires global indexing
  return globalcols(d_cols);
}

// ---------------------------------------------------------
//  MEMBER FUNCTION : downloadSolution
// ---------------------------------------------------------

JncBlow::results_type
JncBlow::downloadSolution() const
{
  // initial reporting
  s_logger->repx(logga::xtra, "entering member function", "JncBlow");

  // active code
  return boost::make_tuple(downloadVar(d_cols.get<0>()));
}

//  end of file

