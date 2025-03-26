//  XEONA ENERGY SYSTEMS MODELING ENVIRONMENT
//
//  file-create-name : optprob.cc
//  file-create-date : Fri 17-Oct-2008 08:20 UTC
//  file-initiator   : Robbie Morrison <robbie@actrix.co.nz>
//
//  file-role        : optimization sub-problem (OSP) and key sub-classes / implementation
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
//  $URL: file:///home/robbie/svn-root/xeona/futz/trunk/xeona1/b/optprob.cc $
//
//  GENERAL NOTES FOR THIS FILE

//  LOCAL AND SYSTEM INCLUDES

#include "optprob.h"          // companion header for this file (place first)

#include "../f/ospinfo.h"     // domain mode interpretation
#include "../d/siglp.h"       // semi-intelligent interface to GLPK MILP solver
#include "../c/util4.h"       // free functions for trigonometry and maths
#include "../c/util2.h"       // free functions which offer general utilities 2
#include "../c/util1.h"       // free functions which offer general utilities 1
#include "../c/label.h"       // helper class to format solver labels

#include "../a/logger.h"      // standard logging functionality (as required)
#include "../c/smart_ptr.h"   // toggle between Boost and TR1 smart pointers
#include ".././common.h"      // common definitions for project (place last)

#include <algorithm>          // STL copying, searching, and sorting, max()
#include <iomanip>            // setw() and family
#include <iostream>           // standard io
#include <sstream>            // string-streams
#include <string>             // C++ strings
#include <vector>             // STL sequence container

#include <typeinfo>           // run-time type info, NOTE: passive reporting role only

#include <boost/algorithm/string.hpp>   // string recasing, trimming, splitting

//  CODE

// ---------------------------------------------------------
//  CLASS           : OptimSubProb
// ---------------------------------------------------------
//  Description  : sub-entity-local abstraction to the domain-local solver interface
//  Role         : abstract base class for classes 'Operations' and 'Control'
//  Techniques   : "push loading" (see below), sub-classes also use 'boost::tuple'
//  Status       : complete
//
//  Design notes
//
//      Provides for intuitive usage for module developers
//
//          Instances of this class are held by 'TechnicalAsset'
//          and 'AssetOperator' sub-classes (and perhaps others).
//
//          One purpose of this class is to shield the host
//          entity from the underlying derived OSP (optimization
//          sub-problem) and its internal formulation.  Moreover,
//          meaningful and (hopefully) consistent function names
//          can be used for the host-related calls.
//
//      Exclusive sequential solver interface access assumed but
//      not enforced
//
//          The solver interface is held in the instance variable
//          'd_solver' of type 'shared_ptr<svif::SolverIf>',
//          passed in at construction time.
//
//          The design assumes that each 'OptimSubProb' has
//          exclusive sequential access to the underlying
//          'svif::SolverIf' solver interface object during the
//          loading of its sub-problem (conversely, solution
//          recovery is read-only).  However, no attempt is made
//          here to use resource locks to prevent parallel
//          access.  Rather the underlying program-wide thread of
//          control should provide for the orderly use of the
//          solver (otherwise there will be major problems
//          elsewhere as well).
//
//          Furthermore, it is assumed that a solver is reset at
//          the appropriate time.  This action, however, is not
//          the responsibility of this class.
//
//      Push loading of the optimization sub-problem
//
//          The constraint and variable loading calls do NOT take
//          row and col indexes respectively, but rather use and
//          return the next available index.  All such calls
//          begin with the phrase "push".
//
//      Commitment modes and subsequent action
//
//          The various commitment modes -- encoded by the
//          enum 'xeona::DomainMode' -- require different
//          objective function loading processes.
//
//          Where feasible, the correct behavior is achieved
//          using polymorphism.  However, in some cases, it is
//          simpler and cleaner to switch on the value of the
//          'd_commitmentMode' flag.
//
//          In terms of the inheritance hierarchy, operations
//          pertains to assets and control pertains to operators
//          (hopefully not as confusing as it looks).
//
//          The documentation for this class is not the right
//          place to discuss the details of the various
//          commitment modes and how the objective function is
//          actually loaded.
//
//          Note however, that the solver only ever receives one
//          set of objective function values and that, once
//          loaded, these values are not modified.  Therefore any
//          pre or post "manipulation" of object function values
//          need to be undertaken in this class.
//
//      Coupling of the operations and control (or tariff) OSPs
//
//          Coupling means adding an equality constraint to
//          connect the operations duty col (or operations sale
//          col) and the control duty col (or tariff sale col).
//          Defining duty/sale and making this coupling is the
//          responsibility of the operator host and NOT the
//          various OSPs.
//
//          The most direct way to couple is via the free
//          function:
//
//              couple(shared_ptr<svif::SolverIf> solver,
//                     const int golA,
//                     const int golB,
//                     const std::string tag)
//
//      Constraint sense for 'pushRhs' calls.
//
//          This enum is defined in the solver interface class
//          'svif::SolverIf'.
//
//      Background information on nomenclature
//
//          item                       short-name   formal description
//          ..................................................................
//          variable value             variable     primal structural variable
//          variable reduced cost      recost       dual structural variable
//          constraint value                        primal auxiliary variable
//          constraint shadow price    shadow       dual auxiliary variable
//          ..................................................................
//
//  CAUTION: 'std::vector' containers and memory reallocation
//
//      STL vectors automatically reshuffle their internal memory
//      when their current allocation becomes insufficient.  This
//      process duly invalidates all references, pointers, and/or
//      iterators for elements of the given vector.  Josuttis
//      (1999 pp149-150) discusses the associated issues in some
//      detail.  This code does not rely on any of the above
//      mechanisms and normal 'std::vector' capacity increments
//      should occur without problem.
//
// ---------------------------------------------------------

// STATIC DEFINITIONS

logga::spLogger OptimSubProb::s_logger = logga::ptrLogStream();

// CREATORS

// ---------------------------------------------------------
//  MEMBER FUNCTION : OptimSubProb (zero-argument)
// ---------------------------------------------------------
//  Description  : zero-argument constructor
//  Role         : indirectly used in the class 'Interface' constructor
//  Techniques   : (nothing special)
//  Status       : complete
// ---------------------------------------------------------

OptimSubProb::OptimSubProb() :
  d_label(),
  d_ospDesc(),

  d_rowStart(),
  d_colStart(),
  d_rowCount(),
  d_colCount(),
  d_cofCount(),
  d_colTrack(),
  d_rowTrack(),

  d_finObjs(),                               // empty vector
  d_ghgObjs(),
  d_noxObjs(),
  d_depObjs(),
  d_lucObjs(),
  d_bidObjs(),
  d_mitObjs(),
  d_nulObjs(),

  d_commitmentMode(),                        // xeona enum
  d_solver()                                 // empty shared pointer
{
  // initial reporting
  s_logger->repx(logga::xtra, "constructor call, zero-argument", "");
}

// ---------------------------------------------------------
//  MEMBER FUNCTION : OptimSubProb (proper)
// ---------------------------------------------------------

OptimSubProb::OptimSubProb
(shared_ptr<svif::SolverIf> solver,          // solver interface object
 const xeona::DomainMode    commitmentMode,
 const std::string&         ospDesc) :

  d_label(""),                               // default label is set here
  d_ospDesc(ospDesc),

  d_rowStart(solver->getConCount()),         // global indexing and also 'const'-qualified
  d_colStart(solver->getVarCount()),
  d_rowCount(0),                             // local indexing and changeable
  d_colCount(0),
  d_cofCount(0),                             // number of nonzero structural coeffs
  d_colTrack(0),                             // provided for ad-hoc use
  d_rowTrack(0),

  d_finObjs(),                               // empty vector
  d_ghgObjs(),
  d_noxObjs(),
  d_depObjs(),
  d_lucObjs(),
  d_bidObjs(),
  d_mitObjs(),
  d_nulObjs(),

  d_commitmentMode(commitmentMode),          // xeona enum
  d_solver(solver)                           // smart pointer to 'xeona::SolverIf' object
{
  // logging stringstream
  std::ostringstream put;

  // headline reporting
  static int yeekLoop          = 0;
  static const std::string tag = "OSP-";
  put << "      ";
  put << tag << std::setw(2) << std::setfill('0') << ++yeekLoop << "  ";
  put << "optimization sub-problem establishment" << "\n";
  s_logger->addSmartBlank(logga::adhc);
  s_logger->putx(logga::adhc, put);

  // initial reporting
  s_logger->repx(logga::xtra, "constructor call, commitment mode", d_commitmentMode);

  // confirm the commitment mode is legitimate
  if ( d_commitmentMode == xeona::e_modeNotSpecified)
    {
      // assume this setting is intentional, but comment anyway
      s_logger->repx(logga::xtra, "unspecified commitment mode in use", d_commitmentMode);
    }
  else
    {
      const tribool okay = xeona::isTwoContained(d_commitmentMode,
                                                 xeona::e_commitmentModes);
      if ( okay )                            // strictly 'true'
        {
        }
      else if ( ! okay )                     // strictly 'false'
        {
          s_logger->repx(logga::warn, "entity coding issue", okay);
        }
      else                                   // otherwise 'indeterminate'
        {
          s_logger->repx(logga::warn, "unexpected commitment mode problem", okay);
        }
    }

  // CAUTION: add a zero "shift" to the various objective vectors
  // -- necessary to keep the indexing correct
  d_finObjs.push_back(0.0);
  d_ghgObjs.push_back(0.0);
  d_noxObjs.push_back(0.0);
  d_depObjs.push_back(0.0);
  d_lucObjs.push_back(0.0);

  // low priority reporting
  s_logger->repx(logga::adhc, "low priority reporting follows", "");
  put << "  OSP row and col settings"           << "\n";
  put << "  solver address : " << d_solver      << "\n";
  if ( ! d_label.empty()   ) put << "  optional label : " << d_label   << "\n";
  if ( ! d_ospDesc.empty() ) put << "  description    : " << d_ospDesc << "\n";
  put << "      row count  : " << d_rowCount    << "\n"
      << "      row offset : " << d_rowStart    << "\n"
      << "      col count  : " << d_colCount    << "\n"
      << "      col offset : " << d_colStart    << "\n";
  s_logger->putx(logga::adhc, put);

} // proper constructor

// ---------------------------------------------------------
//  MEMBER FUNCTION : ~OptimSubProb
// ---------------------------------------------------------

OptimSubProb::~OptimSubProb()                // destructor definition is mandatory
{
  if ( d_label.empty() )
    {
      s_logger->repx(logga::xtra, "destructor call, no OSP label", "");
    }
  else
    {
      // label could be simply "(not set)", see constructor code
      s_logger->repx(logga::adhc, "destructor call, OSP label", d_label);
    }
}

// HOST-RELATED CALLS - used by assets and operators

  // ---------------------------------------------------------
  //  MEMBER FUNCTION : loadOspLabel
  // ---------------------------------------------------------

std::string
OptimSubProb::loadOspLabel                   // optional OSP labeling
(const std::string label)                    // sub-problem (and not solver) label
{
  const std::string previous = d_label;
  d_label                    = label;
  s_logger->repx(logga::xtra, "resetting OSP label, now", d_label);
  s_logger->repx(logga::adhc, "resetting OSP label, was", previous);
  return previous;
}

// ACCESSORS

// ---------------------------------------------------------
//  MEMBER FUNCTION : getRowCount
// ---------------------------------------------------------

int
OptimSubProb::getRowCount() const
{
  return d_rowCount;
}

// ---------------------------------------------------------
//  MEMBER FUNCTION : getColCount
// ---------------------------------------------------------

int
OptimSubProb::getColCount() const
{
  return d_colCount;
}

//  CALLS PROVIDED FOR TEST PURPOSES

// ---------------------------------------------------------
//  MEMBER FUNCTION : checkCounts
// ---------------------------------------------------------
//
//  Useful idiom for reporting bad counts
//
//      if ( ! checkCounts() ) reportBuildIntegrity("my location")
//
//
//  CAUTION: care required in gateways
//
//      Gateways normally run more than one 'OptimSubProb' object
//      at constrain time and hence the this test may prove wrong,
//      particular for solver gow count.
//
// ---------------------------------------------------------

bool
OptimSubProb::checkCounts()
{
  // start and count checks (same code as 'reportBuildIntegrity')
  bool ret = true;                           // presume counts integrity at outset
  const int gowCountSolver = d_solver->getConCount();
  const int golCountSolver = d_solver->getVarCount();
  if ( gowCountSolver != d_rowStart + d_rowCount ) ret = false;
  if ( golCountSolver != d_colStart + d_colCount ) ret = false;
  return ret;
}

// ---------------------------------------------------------
//  MEMBER FUNCTION : reportBuildIntegrity
// ---------------------------------------------------------

bool                                         // should always be true
OptimSubProb::reportBuildIntegrity           // can call any time
(const unsigned    failLevel,
 const std::string comment)                  // note default value
{
  s_logger->repx(logga::adhc, "entering member function", "");

  // start and count checks
  bool ret = true;                           // presume build integrity at outset
  ret = checkCounts();                       // see Gateway caution above

  // for reporting purposes
  const int gowCountSolver = d_solver->getConCount();
  const int golCountSolver = d_solver->getVarCount();

  // set integrity string
  const std::string okay
    = ret ? "start and count checks passed" : "start and count checks failed";

  // run-time type information (expensive)
  const std::string subtype = xeona::demangle(typeid(*this).name());  // CAUTION: deref

  // process the commitment mode
  std::string sCommitmentMode          = "(not overwritten)";
  const std::vector<std::string> cmode = xeona::infoDomainModeLong(d_commitmentMode);
  if ( cmode.size() == 1 )
    {
      sCommitmentMode = cmode.at(0);              // indirectly 'DomainModeInfo::longform'
      boost::algorithm::trim(sCommitmentMode);    // remove special alignment
    }
  else
    {
      const std::string sep = " ";
      std::ostringstream oss;
      oss << d_commitmentMode << " = " << xeona::reducedVector(d_commitmentMode, sep);
      sCommitmentMode = oss.str();
      ret = false;
    }

  std::ostringstream buf;
  buf << "  OSP build integrity report"                                         << "\n";
  if ( ! comment.empty() ) buf << "    call-time comment   : " << comment       << "\n";
  buf << "    OSP optional label  : " << d_label                                << "\n"
      << "    OSP sub-class       : " << subtype                                << "\n"
      << "    build integrity     : " << okay                                   << "\n"
      << "    commitment mode     : " << sCommitmentMode                        << "\n"
      << "    solver gow count    : " << gowCountSolver                         << "\n"
      << "    OSP row start       : " << d_rowStart                             << "\n"
      << "    OSP row count       : " << d_rowCount                             << "\n"
      << "    solver gol count    : " << golCountSolver                         << "\n"
      << "    OSP col start       : " << d_colStart                             << "\n"
      << "    OSP col count       : " << d_colCount                             << "\n"
      << "    OSP non-zero coeffs : " << d_cofCount                             << "\n"
      << "    local summary       : " << d_rowCount << " x " << d_colCount      << "\n";

  if ( ret == true )
    {
      std::ostringstream put;
      put << buf.str();
      s_logger->putx(logga::dbug, put);
    }
  else
    {
      const logga::Rank failRank = static_cast<logga::Rank>(failLevel);
      s_logger->repx(failRank, "OSP build failure", "");
      std::ostringstream put;
      put << "  optimization sub-problem build integrity FAILURE" << "\n"
          << buf.str();
      s_logger->putx(failRank, put);
    }
  return ret;
}

// PUSH CALLS (and similar)

// ---------------------------------------------------------
//  MEMBER FUNCTION : pushIncShift (dual documentation)
// ---------------------------------------------------------
//  Description  : push increment the objective "shift"
//  Role         : problem building by sub-classes
//  Techniques   : virtual functions with substantive definitions, 'd_commitmentMode'
//  Status       : complete
//
//  Design notes
//
//      The "shift" is the constant term in the objective
//      function.  The "shift" does not influence short-run or
//      other optimization, but it does get added to the final
//      objective value.
//
//      The 'double' variant if for non-short-run commitment
//      modes, in contrast to the 'CostSet' variant.
//
//      At the time of writing, the two 'repushShift'
//      non-incremental "shift" value overwriting functions are
//      not provided but could be easily added.
//
//  CAUTION: which objectives to implement
//
//      See below for a caution and warning regarding coding
//      design.
//
// ---------------------------------------------------------

// DOUBLE VARIANT

void                                         // in effect, col zero
OptimSubProb::pushIncShift                   // increment objective function constant term
(const double        shiftValue,             // non-short-run commitment mode
 const CostSet&      specCosts)
{
  // CAUTION: it remain an open question as to whether the
  // 'specCosts' argument should increment the various general
  // objective vectors -- nonetheless, as at commit r5226
  // (30-Sep-2010), there are no calls to this function

  // emit a warning
  s_logger->repx(logga::warn, "unresolved coding design issue", "");
  std::ostringstream put1;
  put1 << "  this function increments the shifts for both the selected"           << "/n"
       << "  commitment objective vector and the various general objective"       << "\n"
       << "  vectors"                                                             << "\n"
       << ""                                                                      << "\n"
       << "  the question of whether it should do the latter remains unresolved"  << "\n"
       << ""                                                                      << "\n"
       << "  if you see this message, fix the issue"                              << "\n";
  if ( specCosts.isZero() )
    {
      put1 << ""                                                                  << "\n"
           << "  but note in this particular case the general increment is zero " << "\n";
    }
  s_logger->putx(logga::warn, put1);

  // increment the various objective vectors as required
  d_finObjs.at(0) += specCosts.fin;
  d_ghgObjs.at(0) += specCosts.ghg;
  d_noxObjs.at(0) += specCosts.nox;
  d_depObjs.at(0) += specCosts.dep;
  d_lucObjs.at(0) += specCosts.luc;

  // increment the various objective vectors as required
  switch ( d_commitmentMode )
    {
    case xeona::e_auctionLmp: d_bidObjs.at(0) += shiftValue; break;
    case xeona::e_adminMerit: d_mitObjs.at(0) += shiftValue; break;
    case xeona::e_adminFirst: d_nulObjs.at(0) += shiftValue; break;
    case xeona::e_shortrunFin:
    case xeona::e_shortrunGhg:
    case xeona::e_shortrunNox:
    case xeona::e_shortrunDep:
    case xeona::e_shortrunLuc:
      {
        s_logger->repx(logga::warn, "wrong call, entity design issue", "");
        std::ostringstream put2;
        put2 << "  should be passing a 'CostSet' and not a 'double' coefficient" << "\n";
        s_logger->putx(logga::dbug, put2);
      }
      break;
    default:
      std::clog << "** coding error 01 in source file " << __FILE__ << std::endl;
      break;
    }

  // modify the solver
  d_solver->incShift(shiftValue);
}

// COSTSET VARIANT

void                                         // in effect, col zero
OptimSubProb::pushIncShift                   // increment objective function constant term
(const CostSet&      shiftIncValues)         // all commitment modes
{
  // increment the various objective vectors as required
  d_finObjs.at(0) += shiftIncValues.fin;
  d_ghgObjs.at(0) += shiftIncValues.ghg;
  d_noxObjs.at(0) += shiftIncValues.nox;
  d_depObjs.at(0) += shiftIncValues.dep;
  d_lucObjs.at(0) += shiftIncValues.luc;

  // recover the required "shift" value
  double shiftval = 0.0;
  switch ( d_commitmentMode )
    {
    case xeona::e_shortrunFin: shiftval = shiftIncValues.fin; break;
    case xeona::e_shortrunGhg: shiftval = shiftIncValues.ghg; break;
    case xeona::e_shortrunNox: shiftval = shiftIncValues.nox; break;
    case xeona::e_shortrunDep: shiftval = shiftIncValues.dep; break;
    case xeona::e_shortrunLuc: shiftval = shiftIncValues.luc; break;
    case xeona::e_auctionLmp:
    case xeona::e_adminMerit:
    case xeona::e_adminFirst:
      {
        s_logger->repx(logga::adhc, "details below (call seems correct)", "");
        std::vector<std::string> modes = xeona::infoDomainModeLong(d_commitmentMode);
        std::string mode = "(some problem)";           // default value
        if ( modes.size() == 1 ) mode = modes.front(); // unique mode encountered
        const std::string func = XEONA_FUNC; // preprocessor macro defined in 'common.h'
        std::ostringstream put;
        put << "  call details" << "\n"
            << "    function        : " << func                            << "\n"
            << "    commitment mode : " << mode                            << "\n"
            << "    value           : " << shiftIncValues                  << "\n";
        // the 'specCosts' supports streaming
        s_logger->putx(logga::adhc, put);
      }
      break;
    default:
      std::clog << "** coding error 02 in source file " << __FILE__ << std::endl;
      break;
    }

  // modify the solver
  d_solver->incShift(shiftval);
}

// ---------------------------------------------------------
//  MEMBER FUNCTION : pushObj (dual documentation)
// ---------------------------------------------------------
//  Description  : push load objective function values
//  Role         : problem building by sub-classes
//  Techniques   : virtual functions with substantive definitions, 'd_commitmentMode'
//  Status       : complete
//
//  Design notes
//
//      The 'double' variant if for non-short-run commitment
//      modes, in contrast to the 'CostSet' variant.
//
// ---------------------------------------------------------

// DOUBLE VARIANT

const int                                    // the col index just employed
OptimSubProb::pushObj
(const double      objValue,
 const CostSet&    specCosts,
 const std::string tag)                      // defaults to empty string
{
  // initial reporting
  s_logger->repx(logga::adhc, "entering member function", "double version");

  // load the various objective vectors
  d_finObjs.push_back(specCosts.fin);
  d_ghgObjs.push_back(specCosts.ghg);
  d_noxObjs.push_back(specCosts.nox);
  d_depObjs.push_back(specCosts.dep);
  d_lucObjs.push_back(specCosts.luc);

  // add another element as required
  switch ( d_commitmentMode )
    {
    case xeona::e_auctionLmp: d_bidObjs.push_back(objValue); break;
    case xeona::e_adminMerit: d_mitObjs.push_back(objValue); break;
    case xeona::e_adminFirst: d_nulObjs.push_back(objValue); break;
    case xeona::e_shortrunFin:
    case xeona::e_shortrunGhg:
    case xeona::e_shortrunNox:
    case xeona::e_shortrunDep:
    case xeona::e_shortrunLuc:
      {
        s_logger->repx(logga::warn, "wrong call, entity design issue", "");
        std::ostringstream put;
        put << "  should be passing a 'CostSet' and not a 'double' coefficient" << "\n";
        s_logger->putx(logga::dbug, put);
      }
      break;
    default:
      s_logger->repx(logga::warn, "coding error 03, commitment mode", d_commitmentMode);
      std::clog << "** coding error 03 in source file " << __FILE__ << std::endl;
      break;
    }

  // load the solver and return col index
  ++d_colCount;
  d_solver->loadObj(globalcol(d_colCount), objValue, tag);
  return d_colCount;
}

// COSTSET VARIANT

const int                                    // the col index just employed
OptimSubProb::pushObj
(const CostSet&    specCosts,
 const std::string tag)
{
  // initial reporting
  s_logger->repx(logga::adhc, "entering member function", "CostSet version");

  // load the various objective vectors
  d_finObjs.push_back(specCosts.fin);
  d_ghgObjs.push_back(specCosts.ghg);
  d_noxObjs.push_back(specCosts.nox);
  d_depObjs.push_back(specCosts.dep);
  d_lucObjs.push_back(specCosts.luc);

  // recover the correct objective value
  double objval = 0.0;
  switch ( d_commitmentMode )
    {
    case xeona::e_shortrunFin: objval = specCosts.fin; break;
    case xeona::e_shortrunGhg: objval = specCosts.ghg; break;
    case xeona::e_shortrunNox: objval = specCosts.nox; break;
    case xeona::e_shortrunDep: objval = specCosts.dep; break;
    case xeona::e_shortrunLuc: objval = specCosts.luc; break;
    case xeona::e_auctionLmp:
    case xeona::e_adminMerit:
    case xeona::e_adminFirst:
      {
        s_logger->repx(logga::adhc, "cost set obj load omitted", "");
        s_logger->repx(logga::adhc, "details below (call seems correct)", "");
        std::vector<std::string> modes = xeona::infoDomainModeLong(d_commitmentMode);
        std::string mode = "(some problem)";           // default value
        if ( modes.size() == 1 ) mode = modes.front(); // unique mode encountered
        const std::string func = XEONA_FUNC; // preprocessor macro defined in 'common.h'
        std::ostringstream put;
        put << "  call details" << "\n"
            << "    function        : " << func                            << "\n"
            << "    commitment mode : " << mode                            << "\n"
            << "    tag             : " << tag                             << "\n"
            << "    value           : " << specCosts                       << "\n";
        // the 'specCosts' supports streaming
        s_logger->putx(logga::adhc, put);
      }
      break;
    default:
      {
        std::clog << "** coding error 04 in source file " << __FILE__ << std::endl;
        break;
      }
    }

  // load the solver and return col index
  ++d_colCount;
  d_solver->loadObj(globalcol(d_colCount), objval, tag);
  return d_colCount;
}

// ---------------------------------------------------------
//  MEMBER FUNCTION : repushObj (dual documentation)
// ---------------------------------------------------------
//  Description  : repush load objective function values
//  Role         : problem building by sub-classes
//  Techniques   : (nothing special)
//  Status       : complete
//
//  Design notes
//
//       Two complementary forms
//
//           The 'CostSet' form is for least-cost commitment
//           modes, whereas the 'double' form is for the other
//           modes.  Incorrect invocation triggers a warning.
//
//      Subsequent use
//
//          These two 'repushObj' functions are intended for
//          subsequent use only -- meaning that 'pushObj' should
//          have already been called.
//
// ---------------------------------------------------------

// DOUBLE VARIANT

const double                                 // return the previous value, now overwritten
OptimSubProb::repushObj
(const int         col,
 const double      objValue,
 const CostSet&    specCosts)
{
  // initial reporting
  s_logger->repx(logga::adhc, "entering member function", "subsequent use only");

  // integrity checks
  if ( col <= 0 )                            // most probably zero
    {
      s_logger->repx(logga::warn, "requested col zero or less", col);
    }
  else if ( col > d_colCount )
    {
      std::ostringstream oss;
      oss << col << " : " << d_colCount;
      s_logger->repx(logga::warn, "requested col exceeds current count", oss.str());
    }

  // update the various objective vectors
  d_finObjs.at(col) = specCosts.fin;
  d_ghgObjs.at(col) = specCosts.ghg;
  d_noxObjs.at(col) = specCosts.nox;
  d_depObjs.at(col) = specCosts.dep;
  d_lucObjs.at(col) = specCosts.luc;

  // remove and replace nominated element as required
  switch ( d_commitmentMode )
    {
    case xeona::e_auctionLmp: d_bidObjs.at(col) = objValue; break;
    case xeona::e_adminMerit: d_mitObjs.at(col) = objValue; break;
    case xeona::e_adminFirst: d_nulObjs.at(col) = objValue; break;
    case xeona::e_shortrunFin:
    case xeona::e_shortrunGhg:
    case xeona::e_shortrunNox:
    case xeona::e_shortrunDep:
    case xeona::e_shortrunLuc:
      {
        s_logger->repx(logga::warn, "wrong call, entity design issue", "");
        std::ostringstream put;
        put << "  should be passing a 'double' and not a 'CostSet' coefficient" << "\n";
        s_logger->putx(logga::dbug, put);
      }
      break;
    default:
      std::clog << "** coding error 05 in source file " << __FILE__ << std::endl;
      break;
    }

  // load the solver and return col index
  return d_solver->reviseObj(globalcol(col), objValue);
}

// COSTSET VARIANT

const double                                 // return the previous value, now overwritten
OptimSubProb::repushObj
(const int         col,
 const CostSet&    specCosts)
{
  // initial reporting
  s_logger->repx(logga::adhc, "entering member function", "subsequent use only");

  // integrity checks
  if ( col > d_colCount )
    {
      std::ostringstream oss;
      oss << col << " : " << d_colCount;
      s_logger->repx(logga::warn, "requested col exceeds current count", oss.str());
    }

  // update the various objective vectors
  d_finObjs.at(col) = specCosts.fin;
  d_ghgObjs.at(col) = specCosts.ghg;
  d_noxObjs.at(col) = specCosts.nox;
  d_depObjs.at(col) = specCosts.dep;
  d_lucObjs.at(col) = specCosts.luc;

  // recover the correct objective value
  double objval = 0.0;
  switch ( d_commitmentMode )
    {
    case xeona::e_shortrunFin: objval = specCosts.fin; break;
    case xeona::e_shortrunGhg: objval = specCosts.ghg; break;
    case xeona::e_shortrunNox: objval = specCosts.nox; break;
    case xeona::e_shortrunDep: objval = specCosts.dep; break;
    case xeona::e_shortrunLuc: objval = specCosts.luc; break;
    case xeona::e_auctionLmp:
    case xeona::e_adminMerit:
    case xeona::e_adminFirst:
      {
        s_logger->repx(logga::adhc, "cost set obj load omitted", "");
        s_logger->repx(logga::adhc, "details below (call seems correct)", "");
        std::vector<std::string> modes = xeona::infoDomainModeLong(d_commitmentMode);
        std::string mode        = "(some problem)";    // default value
        const std::string label = d_solver->getVarLabel(globalcol(col));
        if ( modes.size() == 1 ) mode = modes.front(); // unique mode encountered
        const std::string func = XEONA_FUNC; // preprocessor macro defined in 'common.h'
        std::ostringstream put;
        put << "  call details" << "\n"
            << "    function        : " << func                                 << "\n"
            << "    commitment mode : " << mode                                 << "\n"
            << "    col (gol)       : " << col << " (" << globalcol(col) << ")" << "\n"
            << "    label           : " << label                                << "\n"
            << "    value           : " << specCosts                            << "\n";
        // the 'specCosts' supports streaming
        s_logger->putx(logga::adhc, put);
      }
      break;
    default:
      std::clog << "** coding error 06 in source file " << __FILE__ << std::endl;
      break;
    }

  // load the solver and return prior value
  return d_solver->reviseObj(globalcol(col), objval);
}

// ---------------------------------------------------------
//  MEMBER FUNCTION : markBinary
// ---------------------------------------------------------

const int
OptimSubProb::markBinary
(const int col)
{
  s_logger->repx(logga::adhc, "entering member function", "");

  d_solver->markVarBinary(globalcol(col));
  return col;
}

// ---------------------------------------------------------
//  MEMBER FUNCTION : markInteger
// ---------------------------------------------------------

const int
OptimSubProb::markInteger
(const int col)
{
  s_logger->repx(logga::adhc, "entering member function", "");

  d_solver->markVarInteger(globalcol(col));
  return col;
}

// ---------------------------------------------------------
//  MEMBER FUNCTION : pushRhs
// ---------------------------------------------------------

const int                                    // the row index just employed
OptimSubProb::pushRhs
(const double                rhsValue,
 const svif::ConstraintSense conSense,       // {svif::L, E, G, R, N} from unit 'siglp'
 const std::string           tag)            // passed thru to solver
{
  s_logger->repx(logga::adhc, "entering member function", "");

  ++d_rowCount;                              // ratchet the row count
  d_solver->loadRhs(globalrow(d_rowCount),   // global indexing
                    rhsValue,
                    conSense,
                    tag);
  return d_rowCount;
}

// ---------------------------------------------------------
//  MEMBER FUNCTION : pushCof
// ---------------------------------------------------------
//
//      As far as the solver interface is concerned, the row and
//      col indexes need not exist.  However the initial code for
//      this function tests to confirm that these indexes do
//      exist as far as this class is concerned.
//
// ---------------------------------------------------------

const int                                    // number of local nonzero structural coeffs
OptimSubProb::pushCof
(const int    row,
 const int    col,
 const double coeffValue)
{
  s_logger->repx(logga::adhc, "entering member function", "");

  // defensive programming
  bool failflag = false;
  // local row check
  if ( row < 1 || row > d_rowCount )
    {
      std::ostringstream buf;
      buf << row << " outside 1 thru " << d_rowCount;
      s_logger->repx(logga::dbug, "out of bounds col index", buf.str());
      failflag = true;
    }
  // local col check
  if ( col < 1 || col > d_colCount )
    {
      std::ostringstream buf;
      buf << col << " outside 1 thru " << d_colCount;
      s_logger->repx(logga::dbug, "out of bounds col index", buf.str());
      failflag = true;
    }
  if ( failflag == true )
    {
      std::ostringstream oss;
      oss << "details above if report " << logga::dbug;
      s_logger->repx(logga::warn, "out of range indexing", oss.str());
      std::ostringstream put;
      put
        << "  must abandon structural coefficient insertion" << "\n"
        << "  else GLPK 'glp_load_matrix' may complain of \"column index out of range\""
        << " and then exit on POSIX signal 6" << "\n";
      s_logger->putx(logga::info, put);
      s_logger->addSmartBlank(logga::info);
      return 0;                              // CAUTION: early return is essential
    }

  // active code
  if ( coeffValue != 0.0 )                   // action only nonzero values
    {
      ++d_cofCount;                          // ratchet the nonzero coefficient count
      d_solver->loadCof(globalrow(row),      // global indexing
                        globalcol(col),      // global indexing
                        coeffValue);
    }
  return d_cofCount;
}

// ---------------------------------------------------------
//  MEMBER FUNCTION : pushGof (global indexing)
// ---------------------------------------------------------

const int                                    // number of global nonzero structural coeffs
OptimSubProb::pushGof                        // that is, pushCof using global row and col
(const int    gow,
 const int    gol,
 const double coeffValue)
{
  // initial reporting
  s_logger->repx(logga::adhc, "entering member function", "");

  // defensive programming
  bool failflag = false;
  // global row check
  if ( gow < 1 || gow > d_solver->getConCount() )
    {
      std::ostringstream buf;
      buf << gow << " outside 1 thru " << d_solver->getConCount();
      s_logger->repx(logga::dbug, "out of bounds col index", buf.str());
      failflag = true;
    }
  // global col check
  if ( gol < 1 || gol > d_solver->getVarCount() )
    {
      std::ostringstream buf;
      buf << gol << " outside 1 thru " << d_solver->getVarCount();
      s_logger->repx(logga::dbug, "out of bounds col index", buf.str());
      failflag = true;
    }
  if ( failflag == true )
    {
      std::ostringstream oss;
      oss << "details above if report " << logga::dbug;
      s_logger->repx(logga::warn, "out of range indexing", oss.str());
      std::ostringstream put;
      put
        << "  must abandon structural coefficient insertion" << "\n"
        << "  else GLPK 'glp_load_matrix' may complain of \"column index out of range\""
        << " and then exit on POSIX signal 6" << "\n";
      s_logger->putx(logga::info, put);
      s_logger->addSmartBlank(logga::info);
      return 0;                              // CAUTION: early return is essential
    }

  // active code
  if ( coeffValue != 0.0 )                   // action only nonzero values
    {
      d_solver->loadCof(gow,                 // global indexing
                        gol,                 // global indexing
                        coeffValue);
    }
  const int conCount = d_solver->getConCount();
  return conCount;
}

// ---------------------------------------------------------
//  MEMBER FUNCTION : openBnds
// ---------------------------------------------------------

int                                          // number of bound changes {0, 1, 2}
OptimSubProb::openBnds
(const int col)
{
  // initial reporting
  s_logger->repx(logga::adhc, "entering member function", "");

  // get old bounds
  const double oldLower = d_solver->getLowerBnd(globalcol(col));
  const double oldUpper = d_solver->getUpperBnd(globalcol(col));

  // define new bounds - negative and positive flow
  const double newLower = -d_solver->getInf();
  const double newUpper = +d_solver->getInf();

  // process changes
  int ret = 2;
  if ( oldLower == newLower ) --ret;
  if ( oldUpper == newUpper ) --ret;

  // additional reporting as appropriate
  // YEEK 52 CODE (set by '--yeek')
  if ( xeona::yeek == 52 || xeona::yeek == 1 || xeona::yeek == 2 )
    {
      std::ostringstream put;
      put << std::showpos;
      put << "  lower old/new : " << std::setw(4) << oldLower << " " << newLower << "\n"
          << "  upper old/new : " << std::setw(4) << oldUpper << " " << newUpper << "\n";
      s_logger->repx(logga::dbug, "additional reporting follows, yeek", xeona::yeek);
      s_logger->putx(logga::dbug, put);
    }

  // active code
  d_solver->reviseBnds(globalcol(col), newLower, newUpper);

  // return
  return ret;
}

// ---------------------------------------------------------
//  MEMBER FUNCTION : changeBnds
// ---------------------------------------------------------

int                                          // number of bound changes {0, 1, 2}
OptimSubProb::changeBnds
(const int    col,
 const double lowerBnd,
 const double upperBnd)
{
  // initial reporting
  s_logger->repx(logga::info, "entering member function", "");

  // get old bounds
  const double oldLowerBnd = d_solver->getLowerBnd(globalcol(col));
  const double oldUpperBnd = d_solver->getUpperBnd(globalcol(col));

  // process changes - user defined
  int ret = 2;
  if ( oldLowerBnd == lowerBnd ) --ret;
  if ( oldUpperBnd == upperBnd ) --ret;

  // active code
  d_solver->reviseBnds(globalcol(col), lowerBnd, upperBnd);

  // return
  return ret;
}

// ---------------------------------------------------------
//  MEMBER FUNCTION : resetBnds
// ---------------------------------------------------------

void
OptimSubProb::resetBnds                      // reset to defaults, see unit 'd/siglpk'
(const int col)
{
  // initial reporting
  s_logger->repx(logga::info, "entering member function", "");

  // active code
  d_solver->resetDefaultBnds(globalcol(col));     // wrapper to 'utilSetDefaultBnds'
}

// CALLS WHICH TRANSLATE OSP INDEXES

// ---------------------------------------------------------
//  MEMBER FUNCTION : globalcol
// ---------------------------------------------------------
//  Description  : convert local col index to global col index
//  Role         : enable interchange between various OSP instances holding same solver
//  Techniques   : routine
//  Status       : complete
//
//  Design notes
//
//      Some input integrity checks are also included.
//
// ---------------------------------------------------------

const int
OptimSubProb::globalcol
(const int localCol) const
{
  if ( d_colCount == 0 )
    {
      s_logger->repx(logga::warn, "col count is zero", d_colCount);
    }
  if ( localCol < 1 ||
       localCol > d_colCount )
    {
      std::ostringstream oss;
      oss << localCol << " outside 1 thru " << d_colCount;
      s_logger->repx(logga::warn, "out of range local col", oss.str());
      if ( xeona::nopro == false )           // meaning option '--krazy' not applied
        {
          s_logger->repx(logga::dbug, "GLPK may object shortly", "");
          return 0;
        }
    }
  return localCol + d_colStart;
}

// ---------------------------------------------------------
//  MEMBER FUNCTION : globalrow
// ---------------------------------------------------------
//  Description  : convert local row index to global row index
//  Role         : enable interchange between various OSP instances holding same solver
//  Techniques   : routine
//  Status       : complete
// ---------------------------------------------------------

const int
OptimSubProb::globalrow
(const int localRow) const
{
  if ( d_rowCount == 0 )
    {
      s_logger->repx(logga::warn, "row count is zero", d_rowCount);
    }
  if ( localRow < 1 ||
       localRow > d_rowCount )
    {
      std::ostringstream oss;
      oss << localRow << " outside 1 thru " << d_rowCount;
      s_logger->repx(logga::warn, "out of range local row", oss.str());
      if ( xeona::nopro == false )           // meaning option '--krazy' not applied
        {
          s_logger->repx(logga::dbug, "GLPK may object shortly", "");
          return 0;
        }
    }
  return localRow + d_rowStart;
}

// ---------------------------------------------------------
//  MEMBER FUNCTION : localcol
// ---------------------------------------------------------
//  Description  : convert global col index to local col index
//  Role         : enable interchange between various OSP instances holding same solver
//  Techniques   : routine
//  Status       : complete
// ---------------------------------------------------------

const int
OptimSubProb::localcol
(const int globalCol) const
{
  const int localCol = globalCol - d_colStart;

  if ( localCol < 1 ||
       localCol > d_colCount )
    {
      std::ostringstream oss;
      oss << localCol << " outside 1 thru " << d_colCount;
      s_logger->repx(logga::warn, "out of range converted col", oss.str());
      return 0;
    }
  else
    {
      return localCol;
    }
}

// ---------------------------------------------------------
//  MEMBER FUNCTION : localrow
// ---------------------------------------------------------
//  Description  : convert global row index to local row index
//  Role         : enable interchange between various OSP instances holding same solver
//  Techniques   : routine
//  Status       : complete
// ---------------------------------------------------------

const int
OptimSubProb::localrow
(const int globalRow) const
{
  const int localRow = globalRow - d_rowStart;

  if ( localRow < 1 ||
       localRow > d_rowCount )
    {
      std::ostringstream oss;
      oss << localRow << " outside 1 thru " << d_rowCount;
      s_logger->repx(logga::warn, "out of range converted row", oss.str());
      return 0;
    }
  else
    {
      return localRow;
    }
}

// ---------------------------------------------------------
//  MEMBER FUNCTION : globalcols (1-tuple thru 9-tuple)
// ---------------------------------------------------------

boost::tuple<int>
OptimSubProb::globalcols
(boost::tuple<int> localCols) const
{
  return boost::make_tuple(globalcol(localCols.get<0>()));
}

boost::tuple<int, int>
OptimSubProb::globalcols
(boost::tuple<int, int> localCols) const
{
  return boost::make_tuple(globalcol(localCols.get<0>()),
                           globalcol(localCols.get<1>()));
}

boost::tuple<int, int, int>
OptimSubProb::globalcols
(boost::tuple<int, int, int> localCols) const
{
  return boost::make_tuple(globalcol(localCols.get<0>()),
                           globalcol(localCols.get<1>()),
                           globalcol(localCols.get<2>()));
}

boost::tuple<int, int, int, int>
OptimSubProb::globalcols
(boost::tuple<int, int, int, int> localCols) const
{
  return boost::make_tuple(globalcol(localCols.get<0>()),
                           globalcol(localCols.get<1>()),
                           globalcol(localCols.get<2>()),
                           globalcol(localCols.get<3>()));
}

boost::tuple<int, int, int, int, int>
OptimSubProb::globalcols
(boost::tuple<int, int, int, int, int> localCols) const
{
  return boost::make_tuple(globalcol(localCols.get<0>()),
                           globalcol(localCols.get<1>()),
                           globalcol(localCols.get<2>()),
                           globalcol(localCols.get<3>()),
                           globalcol(localCols.get<4>()));
}

boost::tuple<int, int, int, int, int, int>
OptimSubProb::globalcols
(boost::tuple<int, int, int, int, int, int> localCols) const
{
  return boost::make_tuple(globalcol(localCols.get<0>()),
                           globalcol(localCols.get<1>()),
                           globalcol(localCols.get<2>()),
                           globalcol(localCols.get<3>()),
                           globalcol(localCols.get<4>()),
                           globalcol(localCols.get<5>()));
}

boost::tuple<int, int, int, int, int, int, int>
OptimSubProb::globalcols
(boost::tuple<int, int, int, int, int, int, int> localCols) const
{
  return boost::make_tuple(globalcol(localCols.get<0>()),
                           globalcol(localCols.get<1>()),
                           globalcol(localCols.get<2>()),
                           globalcol(localCols.get<3>()),
                           globalcol(localCols.get<4>()),
                           globalcol(localCols.get<5>()),
                           globalcol(localCols.get<6>()));
}

boost::tuple<int, int, int, int, int, int, int, int>
OptimSubProb::globalcols
(boost::tuple<int, int, int, int, int, int, int, int> localCols) const
{
  return boost::make_tuple(globalcol(localCols.get<0>()),
                           globalcol(localCols.get<1>()),
                           globalcol(localCols.get<2>()),
                           globalcol(localCols.get<3>()),
                           globalcol(localCols.get<4>()),
                           globalcol(localCols.get<5>()),
                           globalcol(localCols.get<6>()),
                           globalcol(localCols.get<7>()));
}

boost::tuple<int, int, int, int, int, int, int, int, int>
OptimSubProb::globalcols
(boost::tuple<int, int, int, int, int, int, int, int, int> localCols) const
{
  return boost::make_tuple(globalcol(localCols.get<0>()),
                           globalcol(localCols.get<1>()),
                           globalcol(localCols.get<2>()),
                           globalcol(localCols.get<3>()),
                           globalcol(localCols.get<4>()),
                           globalcol(localCols.get<5>()),
                           globalcol(localCols.get<6>()),
                           globalcol(localCols.get<7>()),
                           globalcol(localCols.get<8>()));
}

// PROBLEM GET CALLS - can be made anytime but result may not be complete

// ---------------------------------------------------------
//  MEMBER FUNCTION : getShift
// ---------------------------------------------------------

CostSet
OptimSubProb::getShift() const               // meaning the current "shift" term
{
  CostSet buf;
  buf.fin = d_finObjs.at(0);
  buf.ghg = d_ghgObjs.at(0);
  buf.nox = d_noxObjs.at(0);
  buf.dep = d_depObjs.at(0);
  buf.luc = d_lucObjs.at(0);
  return buf;
}

// ---------------------------------------------------------
//  MEMBER FUNCTION : getObjs
// ---------------------------------------------------------

std::vector<CostSet>
OptimSubProb::getObjs() const
{
  std::vector<CostSet> bufs;
  for ( unsigned i = 0;
        i          < d_finObjs.size();
        ++i)
    {
      CostSet buf;
      buf.fin = d_finObjs.at(i);
      buf.ghg = d_ghgObjs.at(i);
      buf.nox = d_noxObjs.at(i);
      buf.dep = d_depObjs.at(i);
      buf.luc = d_lucObjs.at(i);
      bufs.push_back(buf);
    }

  return bufs;
}

// SOLUTION RECOVERY CALLS (note individual and vector versions)

// ---------------------------------------------------------
//  MEMBER FUNCTION : downloadVar
// ---------------------------------------------------------

double
OptimSubProb::downloadVar                    // col primal value for underlying LP
(const int col) const
{
  s_logger->repx(logga::adhc, "entering member function", "");

  // defensive programming
  if ( ! d_solver->isUsableSoln() )          // confirm usable solution
    {
      s_logger->repx(logga::warn, "no usable solution", "debug the call order");
      return 0.0;
    }

  return d_solver->getVarValue(globalcol(col));
}

// ---------------------------------------------------------
//  MEMBER FUNCTION : downloadVars
// ---------------------------------------------------------

std::vector<double>
OptimSubProb::downloadVars() const
{
  s_logger->repx(logga::adhc, "entering member function", "");

  std::vector<double> varVals;               // empty vector

  // defensive programming
  if ( ! d_solver->isUsableSoln() )          // confirm usable solution
    {
      s_logger->repx(logga::warn, "no usable solution", "debug the call order");
      return varVals;
    }

  // active code
  varVals.push_back(0.0);                    // index 0 is simply a placeholder
  for ( int col  = 1;
        col     <= d_colCount;
        ++col )
    {
      varVals.push_back(downloadVar(col));   // see above for 'downloadVar'
    }
  return varVals;
}

// ---------------------------------------------------------
//  MEMBER FUNCTION : downloadVarCosts
// ---------------------------------------------------------

const bool
OptimSubProb::downloadVarCosts
(CostSet& varCosts) const
{
  // initial reporting
  s_logger->repx(logga::adhc, "entering member function", "");

  // defensive programming
  if ( ! d_solver->isUsableSoln() )          // confirm usable solution
    {
      s_logger->repx(logga::warn, "no usable solution", "debug the call order");
      varCosts.reset(0.0);                   // overwrite current values
      return false;
    }

  // calculations
  const std::vector<double> vars = downloadVars();
  varCosts.fin = xeona::coeffProduct(d_finObjs, vars);
  varCosts.ghg = xeona::coeffProduct(d_ghgObjs, vars);
  varCosts.nox = xeona::coeffProduct(d_noxObjs, vars);
  varCosts.dep = xeona::coeffProduct(d_depObjs, vars);
  varCosts.luc = xeona::coeffProduct(d_lucObjs, vars);

  // defensive programming: test for infs (most likely) and nans
  if ( ! varCosts.isFinite() )
    {
      s_logger->repx(logga::warn, "variable costs not finite", "");
      // stay with current values however
      return false;
    }

  // success
  return true;
}

// ---------------------------------------------------------
//  MEMBER FUNCTION : downloadFixCosts
// ---------------------------------------------------------

const bool
OptimSubProb::downloadFixCosts
(CostSet& fixCosts) const
{
  // initial reporting
  s_logger->repx(logga::adhc, "entering member function", "");

  // defensive programming
  if ( ! d_solver->isUsableSoln() )          // confirm usable solution
    {
      s_logger->repx(logga::warn, "no usable solution", "debug the call order");
      fixCosts.reset(0.0);                   // overwrite current values
      return false;
    }

  // calculations
  fixCosts = getShift();

  // defensive programming: test for infs (most likely) and nans
  if ( ! fixCosts.isFinite() )
    {
      s_logger->repx(logga::warn, "fixed costs not finite", "");
      // stay with current values however
      return false;
    }

  // success
  return true;
}

// ---------------------------------------------------------
//  MEMBER FUNCTION : downloadVarCosts (return CostSet)
// ---------------------------------------------------------

CostSet
OptimSubProb::downloadVarCosts() const
{
  // preamble
  CostSet varCosts(0.0);

  // load
  if ( ! downloadVarCosts(varCosts) )
    {
      s_logger->repx(logga::warn, "var costs load failed", "");
    }

  // return
  return varCosts;
}

// ---------------------------------------------------------
//  MEMBER FUNCTION : downloadFixCosts (return CostSet)
// ---------------------------------------------------------

CostSet
OptimSubProb::downloadFixCosts() const
{
  // preamble
  CostSet fixCosts(0.0);

  // load
  if ( ! downloadFixCosts(fixCosts) )
    {
      s_logger->repx(logga::warn, "fix costs load failed", "");
    }

  // return
  return fixCosts;
}

// ---------------------------------------------------------
//  MEMBER FUNCTION : downloadShortrunCosts (pass-by-reference)
// ---------------------------------------------------------

const bool
OptimSubProb::downloadShortrunCosts
(CostSet& varCosts,
 CostSet& fixCosts) const
{
  // initial reporting
  s_logger->repx(logga::adhc, "entering member function", "two CostSet by ref");

  // defensive programming
  if ( ! d_solver->isUsableSoln() )          // confirm usable solution
    {
      s_logger->repx(logga::warn, "no usable solution", "debug the call order");
      varCosts.reset(0.0);                   // overwrite current values
      fixCosts.reset(0.0);                   // overwrite current values
      return false;
    }

  // calculations (existing values overwritten in call)
  downloadVarCosts(varCosts);                // load (short-run) variable costs [1]
  downloadFixCosts(fixCosts);                // load (short-run) fixed costs [1]
  // [1] no need to test for success

  // success
  return true;
}

// ---------------------------------------------------------
//  MEMBER FUNCTION : downloadShortrunCosts (tuple return)
// ---------------------------------------------------------

boost::tuple
<CostSet,
 CostSet>
OptimSubProb::downloadShortrunCosts() const
{
  // initial reporting
  s_logger->repx(logga::adhc, "entering member function", "tuple CostSet return");

  // preamble
  CostSet varCosts(0.0);
  CostSet fixCosts(0.0);

  // defensive programming
  if ( ! d_solver->isUsableSoln() )          // confirm usable solution
    {
      s_logger->repx(logga::warn, "no usable solution", "debug the call order");
    }

  // active code
  downloadShortrunCosts(varCosts, fixCosts); // wrapper call
  return boost::make_tuple(varCosts, fixCosts);
}

// ---------------------------------------------------------
//  MEMBER FUNCTION : downloadSlack (for LMP unit price)
// ---------------------------------------------------------

double
OptimSubProb::downloadSlack                  // row dual value for underlying LP
(const int row) const
{
  s_logger->repx(logga::adhc, "entering member function", "");

  // defensive programming
  if ( ! d_solver->isUsableSoln() )          // confirm usable solution
    {
      s_logger->repx(logga::warn, "no usable solution", "debug the call order");
      return 0;
    }

  // additional reporting as appropriate
  // YEEK 7 CODE (set by '--yeek')
  if ( xeona::yeek == 7 || xeona::yeek == 1 || xeona::yeek == 2 )
    {
      std::ostringstream put;
      put << "  slack call" << "\n"
          << "    local row  : " << row                                     << "\n"
          << "    global row : " << globalrow(row)                          << "\n"
          << "    value      : " << d_solver->getSlackValue(globalrow(row)) << "\n";
      s_logger->putx(logga::yeek, put);      // 'yeek' is acceptable in this case
    }

  // active code
  return d_solver->getSlackValue(globalrow(row));
}

// ---------------------------------------------------------
//  MEMBER FUNCTION : downloadSlacks
// ---------------------------------------------------------

std::vector<double>
OptimSubProb::downloadSlacks() const         // slack values
{
  s_logger->repx(logga::adhc, "entering member function", "");

  std::vector<double> slackVals;

  // defensive programming
  if ( ! d_solver->isUsableSoln() )          // confirm usable solution
    {
      s_logger->repx(logga::warn, "no usable solution", "debug the call order");
      return slackVals;
    }

  // active code
  for ( int row  = 1;
        row     <= d_rowCount;
        ++row )
    {
      slackVals.push_back(downloadSlack(row));    // see above for 'downloadSlack'
    }
  return slackVals;
}

#if 0 // one answer to the question below

// INSTANCE AND STATIC RESET CALLS
// PONDER: which of these reset calls are needed?

// ---------------------------------------------------------
//  MEMBER FUNCTION : resetSolver
// ---------------------------------------------------------

void
OptimSubProb::resetSolver                    // will affect all other solver users
(const std::string label)                    // solver label, note default
{
  // initial reporting
  s_logger->repx(logga::adhc, "entering normal member function", label);

  // active code
  d_solver->resetProblem();
  if ( ! label.empty() )
    {
      d_solver->setProblemLabel(label);      // otherwise retain preexisting label
    }
}

// ---------------------------------------------------------
//  MEMBER FUNCTION : resetSolver (static)
// ---------------------------------------------------------

void
OptimSubProb::resetSolver                    // will affect all other solver users
(shared_ptr<svif::SolverIf> solver,
 const std::string          label)           // solver label, note default
{
  // initial reporting
  s_logger->repx(logga::adhc, "entering static member function", label);

  // active code
  solver->resetProblem();
  if ( ! label.empty() )
    {
      solver->setProblemLabel(label);        // relabel as appropriate
    }
}

#endif // 0

// ---------------------------------------------------------
//  CLASS           : CouplingOsp
// ---------------------------------------------------------
//  Description  : concrete class for "coupling" two OSPs
//  Role         : used in 'constrain' calls from 'AssetOperator' instances and similar
//  Techniques   : concrete
//  Status       : complete
// ---------------------------------------------------------

// CREATORS

CouplingOsp::CouplingOsp
(shared_ptr<svif::SolverIf> solver) :
  OptimSubProb(solver,
               xeona::e_modeNotSpecified,
               "coupling")
{
  s_logger->repx(logga::xtra, "constructor call", "");
}

CouplingOsp::~CouplingOsp()
{
  s_logger->repx(logga::adhc, "destructor call", "");
}

// ---------------------------------------------------------
//  MEMBER FUNCTION : coupleGols
// ---------------------------------------------------------
//  Description  : adds coupling constraint row and structural coefficients
//  Role         : called by asset operator 'constrain' function
//  Techniques   : solver interface calls
//  Status       : complete
// ---------------------------------------------------------

bool                                         // 'true' if coupling constraint added
CouplingOsp::coupleGols
(const int         golA,
 const int         golB,
 const std::string tag)                      // note default
{
  // initial reporting
  std::ostringstream oss;
  oss << golA << " " << golB;
  s_logger->repx(logga::xtra, "entering member function, gols", oss.str());

  // integrity checks
  bool okay = true;                          // presumption of data integrity
  if ( golA == golB ) okay = false;
  if ( golA <= 0 )    okay = false;
  if ( golB <= 0 )    okay = false;
  if ( okay == false )
    {
      std::ostringstream oss;
      oss << golA << " : " << golB;
      s_logger->repx(logga::warn, "problematic global col values", oss.str());
      s_logger->repx(logga::dbug, "early return", "failure");
      return false;
    }

  // process the solver contribution
  Label label(tag);
  label << "osp-coupling";

  int cofCount  = 0;                         // not actually used for anything
  const int row = pushRhs(0.0, svif::E, label.str());
  const int gow = globalrow(row);
  cofCount      = pushGof(gow, golA, +1.0);
  cofCount      = pushGof(gow, golB, -1.0);
  return true;
}

// ---------------------------------------------------------
//  FREE FUNCTION   : xeona::couple
// ---------------------------------------------------------
//  Description  : concrete class for "coupling" two OSPs
//  Role         : used in 'constrain' calls from 'AssetOperator' instances and similar
//  Techniques   : free function wrapper to 'Coupling::coupleGols'
//  Status       : complete
// ---------------------------------------------------------

namespace xeona
{
  bool                                       // 'true' if coupling constraint added
  couple
  (shared_ptr<svif::SolverIf> solver,
   const int                  golA,          // usually from { ops }
   const int                  golB,          // usually from { ctl con }
   const std::string          tag)           // defaults to ""
  {
    static logga::spLogger logger = logga::ptrLogStream();  // free function logger
    std::ostringstream oss;
    oss << golA << " : " << golB;
    logger->repx(logga::xtra, "entering free function, gols", oss.str());
    if ( golA == 0 || golB == 0 )
      {
        logger->repx(logga::warn, "at least one gol is zero", oss.str());
      }

    CouplingOsp couple(solver);
    return couple.coupleGols(golA, golB, tag);
  }
}

// ---------------------------------------------------------
//  CLASS           : ConnectionOsp
// ---------------------------------------------------------
//  Description  : concrete class for "connecting" block interfaces in an OSP context
//  Role         : used in 'constrain' calls from 'TechnicalAsset' instances and similar
//  Techniques   : concrete
//  Status       : complete
// ---------------------------------------------------------

// CREATORS

ConnectionOsp::ConnectionOsp() :             // zero-argument constructor
  OptimSubProb(),
  d_gols(),                                  // empty vector
  d_passCount(),
  d_lastSolverAddress("")
{
  s_logger->repx(logga::xtra, "constructor call, zero-argument", "");
}

ConnectionOsp::ConnectionOsp
(shared_ptr<svif::SolverIf> solver,
 const int                  passCount) :     // note default
  OptimSubProb(solver,
               xeona::e_modeNotSpecified,    // see unit 'f/ospmodes'
               "connection"),
  d_gols(),                                  // empty vector
  d_passCount(passCount),
  d_lastSolverAddress("")
{
  s_logger->repx(logga::xtra, "constructor call, normal", "");
  getPassCount();                            // for its range checking side-effects
}

ConnectionOsp::~ConnectionOsp()
{
  s_logger->repx(logga::adhc, "destructor call", "");
}

// PUBLIC FUNCTIONS

// ---------------------------------------------------------
//  MEMBER FUNCTION : incPassCount
// ---------------------------------------------------------

int
ConnectionOsp::incPassCount
(shared_ptr<svif::SolverIf> solver)
{
  // initial reporting
  s_logger->repx(logga::adhc, "entering member function", "");

  // active code
  d_passCount++;
  const int passCount = getPassCount();      // 'getPassCount' contains range checking

  // check for solver mismatch (added to assist a bug hunt)
  std::string msg = "(not overwritten)";
  if ( checkSolverConsistency(passCount, solver, msg) )
    {
      s_logger->repx(logga::adhc, "connection OSP solver okay", msg);      // okay
    }
  else
    {
      std::ostringstream put;
      put << "  likely causes for this warning:"                         << "\n"
          << "    * a technical asset not listed by an asset operator"   << "\n"
          << "    * an asset operator not listed by a domain controller" << "\n"
          << "    * a junction entity not listed by a domain controller" << "\n";
      s_logger->repx(logga::warn, "connection OSP solver issue", msg);     // problem
      s_logger->putx(logga::dbug, put);
    }

  // return
  return passCount;
}

// ---------------------------------------------------------
//  MEMBER FUNCTION : getPassCount
// ---------------------------------------------------------

int
ConnectionOsp::getPassCount() const
{
  // initial reporting
  s_logger->repx(logga::adhc, "entering member function, count", d_passCount);

  // active code
  if ( d_passCount < 0 )
    {
      s_logger->repx(logga::warn, "pass count strictly negative ", d_passCount);
    }
  else if ( d_passCount > 2 )
    {
      s_logger->repx(logga::warn, "pass count exceeds two", d_passCount);
    }
  return d_passCount;
}

// ---------------------------------------------------------
//  MEMBER FUNCTION : resetPassCount
// ---------------------------------------------------------

void
ConnectionOsp::resetPassCount()
{
  // initial reporting
  s_logger->repx(logga::adhc, "entering member function, count", "");

  d_passCount = 0;
}

// ---------------------------------------------------------
//  MEMBER FUNCTION : storeGols
// ---------------------------------------------------------

void
ConnectionOsp::storeGols
(const std::vector<int> gols)
{
  d_gols = gols;
}

// ---------------------------------------------------------
//  MEMBER FUNCTION : recoverGols
// ---------------------------------------------------------

std::vector<int>
ConnectionOsp::recoverGols() const
{
  return d_gols;
}

// ---------------------------------------------------------
//  MEMBER FUNCTION : bindGols
// ---------------------------------------------------------

bool
ConnectionOsp::bindGols
(const std::vector<int> gols1,
 const std::vector<int> gols2,
 const std::string      tag)
{
  // initial reporting
  s_logger->repx(logga::adhc, "entering member function, tag", tag);

  // failures counter
  int fails = 0;

  // integrity checks
  const int size1 = gols1.size();
  const int size2 = gols2.size();
  if ( size1 == 0 )
    {
      s_logger->repx(logga::warn, "gol1 vector empty", size1);
    }
  if ( size2 == 0 )
    {
      s_logger->repx(logga::warn, "gol2 vector empty", size2);
    }
  if ( size1 != size2 )
    {
      ++fails;                               // fail flag update
      std::ostringstream oss;
      oss << size1 << " " << size2;
      s_logger->repx(logga::warn, "gol vectors differ in size", oss.str());
    }

  // loop thru calling 'bindGolz'
  const int size  = std::min(size1, size2);  // fault tolerant for looping
  for ( int i = 0; i < size; ++i )
    {
      const bool ret = bindGolz(gols1.at(i), gols2.at(i), tag);
      if ( ret == false )
        {
          ++fails;                           // fail flag update
          s_logger->repx(logga::warn, "bindGolz call (scalar) fail, return", ret);
        }
    }

  // return
  if ( fails == 0 ) return true;
  else              return false;

} // function 'ConnectionOsp::bindGols'

// ---------------------------------------------------------
//  MEMBER FUNCTION : bindGolz (workhorse)
// ---------------------------------------------------------
//  Description  : bind two global cols in order to maintain equality
//  Role         : connect interface process, couple duty process
//  Techniques   : 'svif::SolverIf' calls
//  Status       : complete
//
//  Terminology
//
//      "gol" = global col
//      "gow" = global row
//
// ---------------------------------------------------------

bool                                         // 'true' if connection constraint added
ConnectionOsp::bindGolz
(const int         gol1,
 const int         gol2,
 const std::string tag)                      // note default
{
  // initial reporting
  std::ostringstream put;
  s_logger->repx(logga::adhc, "entering member function", "");
  put << "  connection OSP pointer (me) : " << this << "\n"
      << "  given global col one        : " << gol1 << "\n"
      << "  given global col two        : " << gol2 << "\n";
  s_logger->putx(logga::adhc, put);

  // integrity checks
  if ( gol1 == gol2 )
    {
      std::ostringstream oss;
      oss << gol1 << " : " << gol2;
      s_logger->repx(logga::warn, "identical gol values", oss.str());
      std::ostringstream put;
      put << "  likely causes for this warning:"                         << "\n"
          << "    * a technical asset not listed by an asset operator"   << "\n";
      put << "  speculative causes for this warning:"                    << "\n"
          << "    * an asset operator not listed by a domain controller" << "\n"
          << "    * a junction entity not listed by a domain controller" << "\n";
      s_logger->putx(logga::dbug, put);
    }

  // integrity checks
  bool okay = true;                          // presumption of data integrity
  if ( gol1 == gol2 ) okay = false;
  if ( gol1 <= 0 )    okay = false;
  if ( gol2 <= 0 )    okay = false;
  if ( okay == false )
    {
      std::ostringstream oss;
      oss << gol1 << " : " << gol2;
      s_logger->repx(logga::warn, "problematic global col values", oss.str());
      s_logger->repx(logga::dbug, "early return", "failure");
      return false;
    }

  // process the solver contribution
  Label lab(d_label);
  lab << "iface-bal";                        // hard-coded

  int cofCount  = 0;                         // not actually used for much
  const int row = pushRhs(0.0, svif::E, lab.str());
  const int gow = globalrow(row);

  put << "  local row          : " << row  << "\n"
      << "  global row         : " << gow  << "\n"
      << "  global col storage : " << gol1 << "\n"
      << "  global col given   : " << gol2 << "\n";
  s_logger->putx(logga::adhc, put);

  cofCount      = pushGof(gow, gol1, +1.0);
  cofCount      = pushGof(gow, gol2, -1.0);

  s_logger->repx(logga::adhc, "leaving member function", "success");
  return true;                               // meaning solver row entered
}

// UTILITY FUNCTIONS

// ---------------------------------------------------------
//  MEMBER FUNCTION : checkSolverConsistency
// ---------------------------------------------------------
//  Description  : check for solver consistency, no explicit effect on code logic
//  Role         : added to chase a bug involving solver consistency
//  Techniques   : shared pointer stream insertion, 'd_lastSolverAddress'
//  Status       : complete
// ---------------------------------------------------------

bool                                         // 'false' if problem detected
ConnectionOsp::checkSolverConsistency        // used by 'incPassCount'
(const int                  passCount,
 shared_ptr<svif::SolverIf> solver,
 std::string&               info)            // processing information for local use
{
  // process solver address
  std::ostringstream oss;
  oss << solver;                                  // inserts "solver.get()"
  const std::string solverAddress = oss.str();    // stringify

  // undertake integrity checks and similar as required
  switch ( passCount )
    {
    case 1:
      info                = solverAddress + " : (premature)";
      d_lastSolverAddress = solverAddress;        // update stored address string
      return true;
    case 2:
      if ( d_lastSolverAddress == solverAddress ) // string-wise comparison
        {
          info                = d_lastSolverAddress + " : " + solverAddress;
          d_lastSolverAddress = solverAddress;    // update stored address string
          return true;
        }
      else
        {
          info = d_lastSolverAddress + " : " + solverAddress;
          return false;
        }
    default:
      s_logger->repx(logga::warn, "unexpected invalid pass count", passCount);
      info = "coding problem";
      return false;
    }

} // member function 'ConnectionOsp::checkSolverConsistency'

// ---------------------------------------------------------
//  CLASS           : ControlOsp (penalty-based control)
// ---------------------------------------------------------
//  Description  : abstract class covering penalty-based control (PBC)
//  Role         : parent for concrete "Ctl" sub-classes
//  Techniques   : (nothing special)
//  Status       : complete
//
//  Design notes
//
//  CAUTION: code order
//
//      This definition must precede the definition for class
//      'Operations'.
//
// ---------------------------------------------------------

// CREATORS

ControlOsp::ControlOsp
(shared_ptr<svif::SolverIf> solver,
 const xeona::DomainMode    commitmentMode,
 const xeona::DomainMode    commitmentModeSum,
 const std::string&         ospDesc) :
  OptimSubProb(solver, commitmentMode),
  d_dutyCol(0)
{
  s_logger->repx(logga::xtra, "constructor call", "");

  // integrity checks
  const tribool okay = xeona::isTwoContained(commitmentMode,
                                             commitmentModeSum);

  // CAUTION: the following 'boost::tribool' tests work as
  // expected, however not all constructs behave intuitively
  if ( okay )                                // strictly 'true'
    {
      std::ostringstream oss;
      oss << commitmentMode << " two-contained in " << commitmentModeSum;
      s_logger->repx(logga::adhc, "commitment mode fits", oss.str());
    }
  else if ( ! okay )                         // strictly 'false'
    {
      std::ostringstream oss;
      oss << commitmentMode << " not two-contained in " << commitmentModeSum;
      s_logger->repx(logga::warn, "commitment mode mismatch", oss.str());
      const std::string fix = "check 'DomainController::commitment-mode' string value";
      std::ostringstream put;
      put << "  commitment mode mismatch"                                          << "\n"
          << "    OSP description : " << ospDesc                                   << "\n"
          << "    likely cause    : " << fix                                       << "\n"
          << "    probably wrong  : " << xeona::infoDomainModePure(commitmentMode) << "\n"
          << "    looking for one of the following:"  << "\n"                      << "\n"
          << xeona::infoDomainModeLong(commitmentModeSum, 2);    // no trailing newline
      s_logger->putx(logga::dbug, put);
    }
  else                                       // strictly 'indeterminate'
    {
      std::ostringstream oss;
      oss << commitmentMode << " : " << commitmentModeSum;
      s_logger->repx(logga::warn, "commitment mode data issue", oss.str());
    }
}

// ---------------------------------------------------------
//  MEMBER FUNCTION : ~ControlOsp
// ---------------------------------------------------------

ControlOsp::~ControlOsp()
{
  s_logger->repx(logga::adhc, "destructor call", "");
}

// FILL PROBLEM CALLS

// ---------------------------------------------------------
//  MEMBER FUNCTION : ControlOsp::setFloorDuty
// ---------------------------------------------------------

void
ControlOsp::setFloorDuty
(const double mustRunDuty,
 const int    teasDutyCol)
{
  Label lab(d_label);
  lab << "set-duty";
  const int floorRow = pushRhs(mustRunDuty, svif::G, lab.str("floor"));
  d_cofCount         = pushCof(floorRow, teasDutyCol, +1.0);
}

// ---------------------------------------------------------
//  MEMBER FUNCTION : ControlOsp::setCeilingDuty
// ---------------------------------------------------------

void
ControlOsp::setCeilingDuty
(const double maxDuty,
 const int    teasDutyCol)
{
  Label lab(d_label);
  lab << "set-duty";
  const int ceilingRow = pushRhs(maxDuty, svif::L, lab.str("ceiling"));
  d_cofCount           = pushCof(ceilingRow, teasDutyCol, +1.0);
}

// ---------------------------------------------------------
//  CLASS           : OperationsOsp
// ---------------------------------------------------------
//  Description  : abstract class covering operations
//  Role         : parent for concrete "Ops" sub-classes
//  Techniques   :
//  Status       : complete
//
//  Design notes
//
//      OSP coupling
//
//          The constraint equation which couples the operations
//          and control OSPs is the first call to 'pushRhs' for
//          this class -- in other words, it will have a local
//          row index of unity.
//
//      Base class members
//
//          Note the need to scope qualify the call to
//          'OptimSubProb::pushZeroObj'.
//
//      Use of 'd_commitmentMode' flag
//
//          This function uses the 'd_commitmentMode' flag to
//          make the right choices -- and while the use of a flag
//          in this manner is counter to object-oriented design,
//          it is hard to get the information across using
//          inheritance.
//
//  CAUTION: code layout
//
//      The definition for class 'Operations' MUST precede this
//      definition.
//
// ---------------------------------------------------------

// CREATORS

OperationsOsp::OperationsOsp
(shared_ptr<svif::SolverIf> solver,
 const xeona::DomainMode    commitmentMode,
 const std::string&         ospDesc) :
  OptimSubProb(solver, commitmentMode, ospDesc)
{
  s_logger->repx(logga::xtra, "constructor call", "");
}

OperationsOsp::~OperationsOsp()
{
  s_logger->repx(logga::adhc, "destructor call", "");
}

// PROBLEM BUILDING CALLS (and similar)

void
OperationsOsp::uploadShortrunCosts
(const CostSet& dutySpecCosts,
 const int      gol)                         // zero is valid
{
  // filter on index zero
  if ( gol == 0 )
    {
      // initial reporting
      s_logger->repx(logga::xtra, "entering member function, shift use", gol);
      s_logger->repx(logga::dbug, "shift term overwrite will occur", "");
      repushObj(gol, dutySpecCosts);         // zero means zero
    }
  else
    {
      // initial reporting
      const int col = localcol(gol);
      std::ostringstream oss;
      oss << col << " (" << gol << ")";
      s_logger->repx(logga::adhc, "entering member function, col (gol)", oss.str());
      s_logger->repx(logga::dbug, "normal overwrite will occur", "");
      repushObj(col, dutySpecCosts);     // CAUTION: local col index required
    }
}

void
OperationsOsp::uploadShortrunCosts
(const CostSet& shiftCosts)
{
  // initial reporting
  s_logger->repx(logga::xtra, "entering member function", "no gol version");
  s_logger->repx(logga::dbug, "shift term increment will occur", "");

  // active code
  pushIncShift(shiftCosts);
}

// SOLUTION RECOVERY CALLS

const double
OperationsOsp::downloadSolnVar
(const int gol) const
{
  return downloadVar(localcol(gol));
}

const double
OperationsOsp::downloadSolnSlack
(const int gow) const
{
  return downloadSlack(localrow(gow));
}

const bool                                   // 'false' means not selected or tripped
OperationsOsp::downloadRunStatus
(const int gol) const                        // trip global column
{
  return d_solver->getBinaryVarValue(gol);
}

// ---------------------------------------------------------
//  CLASS           : QuantityOsp
// ---------------------------------------------------------

QuantityOsp::QuantityOsp
(shared_ptr<svif::SolverIf> solver,
 const xeona::DomainMode    commitmentMode,
 const std::string&         ospDesc) :
  OptimSubProb(solver, commitmentMode, ospDesc)
{
  s_logger->repx(logga::dbug, "constructor call", "");
}

QuantityOsp::~QuantityOsp()
{
  s_logger->repx(logga::adhc, "destructor call", "");
}

// PROBLEM BUILDING CALLS (and similar)

void
QuantityOsp::uploadShortrunCosts
(const CostSet& dutySpecCosts,
 const int      gol)                         // zero is valid
{
  // filter on index zero
  if ( gol == 0 )
    {
      // initial reporting
      s_logger->repx(logga::xtra, "entering member function, shift use", gol);
      s_logger->repx(logga::dbug, "shift term overwrite will occur", "");
      repushObj(gol, dutySpecCosts);         // zero means zero
    }
  else
    {
      // initial reporting
      const int col = localcol(gol);
      std::ostringstream oss;
      oss << col << " (" << gol << ")";
      s_logger->repx(logga::adhc, "entering member function, col (gol)", oss.str());
      s_logger->repx(logga::dbug, "normal overwrite will occur", "");
      repushObj(col, dutySpecCosts);     // CAUTION: local col index required
    }
}

void
QuantityOsp::uploadShortrunCosts
(const CostSet& shiftCosts)
{
  // initial reporting
  s_logger->repx(logga::xtra, "entering member function", "no gol version");
  s_logger->repx(logga::dbug, "shift term increment will occur", "");

  // active code
  pushIncShift(shiftCosts);
}

// ---------------------------------------------------------
//  CLASS           : OfferOsp
// ---------------------------------------------------------

OfferOsp::OfferOsp
(shared_ptr<svif::SolverIf> solver,
 const xeona::DomainMode    commitmentMode,
 const std::string&         ospDesc) :
  OptimSubProb(solver, commitmentMode, ospDesc)
{
  s_logger->repx(logga::xtra, "constructor call", "");
}

OfferOsp::~OfferOsp()
{
  s_logger->repx(logga::adhc, "destructor call", "");
}

// ---------------------------------------------------------
//  CLASS           : JunctionOsp
// ---------------------------------------------------------

JunctionOsp::JunctionOsp
(shared_ptr<svif::SolverIf> solver,
 const xeona::DomainMode    commitmentMode,
 const std::string&         ospDesc) :
  OptimSubProb(solver,
               commitmentMode,
               ospDesc)
{
  s_logger->repx(logga::xtra, "constructor call", "");
}

JunctionOsp::~JunctionOsp()
{
  s_logger->repx(logga::adhc, "destructor call", "");
}

// ---------------------------------------------------------
//  CLASS           : LmpNodeOsp
// ---------------------------------------------------------

LmpNodeOsp::LmpNodeOsp
(shared_ptr<svif::SolverIf> solver,
 const xeona::DomainMode    commitmentMode,
 const std::string&         ospDesc) :
  OptimSubProb(solver,
               commitmentMode,
               ospDesc)
{
  s_logger->repx(logga::xtra, "constructor call", "");
}

LmpNodeOsp::~LmpNodeOsp()
{
  s_logger->repx(logga::adhc, "destructor call", "");
}

// ---------------------------------------------------------
//  CLASS           : Lmp2NodeOsp
// ---------------------------------------------------------

Lmp2NodeOsp::Lmp2NodeOsp
(shared_ptr<svif::SolverIf> solver,
 const xeona::DomainMode    commitmentMode,
 const std::string&         ospDesc) :
  LmpNodeOsp(solver,
             commitmentMode,
             ospDesc),
  d_thetaGol(0)
{
  s_logger->repx(logga::xtra, "constructor call", "");
}

Lmp2NodeOsp::~Lmp2NodeOsp()
{
  s_logger->repx(logga::adhc, "destructor call", "");
}

// ---------------------------------------------------------
//  MEMBER FUNCTION : Lmp2NodeOsp::pinTheta
// ---------------------------------------------------------

void
Lmp2NodeOsp::pinTheta
(const double thetaDegrees)                  // reference voltage angle (usually zero)
{
  const double thetaRadians = xeona::degree2radian(thetaDegrees);
  if ( d_thetaGol == 0 )
    {
      s_logger->repx(logga::warn, "invalid voltage angle theta col", d_thetaGol);
      s_logger->repx(logga::dbug, "no action taken, thetaDegrees", thetaDegrees);
      return;
    }

  // quoting from above: "the design assumes that each
  // 'OptimSubProb' has exclusive sequential access to the
  // underlying 'svif::SolverIf' solver interface object during
  // the loading of its sub-problem"
  //
  // that assumption is reasonable in this case
  //
  // but it may have been better to implement locking as some
  // level, probably for OSPs

  // low level code -- meaning direct calls to the solver
  const int pinRow = d_solver->getConCount() + 1;
  d_solver->loadRhs(pinRow, thetaRadians, svif::E, "pinned-value");
  d_solver->loadCof(pinRow, d_thetaGol, +1.0);

  s_logger->repx(logga::adhc, "theta pinned, thetaDegrees", thetaDegrees);
}

//  end of file

