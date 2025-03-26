//  XEONA ENERGY SYSTEMS MODELING ENVIRONMENT
//
//  file-create-name : costs.cc
//  file-create-date : Fri 17-Oct-2008 12:00 UTC
//  file-initiator   : Robbie Morrison <robbie@actrix.co.nz>
//
//  file-role        : cost sets and support / implementation
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
//  $URL: file:///home/robbie/svn-root/xeona/futz/trunk/xeona1/c/costs.cc $
//
//  GENERAL NOTES FOR THIS FILE

//  LOCAL AND SYSTEM INCLUDES

#include "costs.h"            // companion header for this file (place first)

#include "../a/logger.h"      // standard logging functionality (as required)
#include "../c/smart_ptr.h"   // toggle between Boost and TR1 smart pointers
#include ".././common.h"      // common definitions for project (place last)

#include <string>             // C++ strings
#include <sstream>            // string-streams

#include <boost/format.hpp>             // printf style formatting
#include <boost/tuple/tuple.hpp>        // n-tuples, ref(), cref()

//  CODE

// ---------------------------------------------------------
//  CLASS           : CostSet
// ---------------------------------------------------------
//  Description  : container for multiple cost types, be they intensive or extensive
//  Role         : to support 'OptimSubProb' instances and for cost aggregation
//  Techniques   : struct, overloaded operators, tuples
//  Status       : complete
//
//  Supported arithmetic and comparison operators
//
//          typedef tuple<double, double, double, double> tupleD5
//
//          add                             : cs1  = cs2 + cs3
//          subtract                        : cs1  = cs2 - cs3
//          add and assign                  : cs1 += cs2
//          subtract and assign             : cs1 -= cs2
//          scalar multiply                 : cs1  = double * cs2
//          scalar multiply                 : cs1  = cs2 * double
//          scalar multiply and assign      : cs1 *= double
//
//          tupleD5 multiply                : cs1  = tupleD5 * cs2
//          tupleD5 multiply                : cs1  = cs2 * tupleD5
//          tupleD5 multiply and assign     : cs1 *= tupleD5
//
//          strictly equal to (no epsilon)  : cs1 == cs2
//          strictly not equal to           : cs1 != cs2
//
//          Note also the 'tupleD5' can be used in constructors
//          and can be exported using the 'tuple' member
//          function.
//
//  Design notes
//
//      Public data
//
//          This class is indeed a 'struct'.  It has public data,
//          which means direct assignment and recovery is legal:
//
//              cs1.fin = 111.1;
//              std::cout << cs1.ghg << std::endl;
//
//      Free binary operators
//
//          The overloaded binary operators used here are
//          implemented as free functions.  They could equally be
//          member functions.  See Stephens etal (2006 p327) for
//          a discussion on the relative merits.
//
//          The free binary operators are not granted friendship
//          because they do not need it -- the data is public.
//
//      Copy constructor and copy assignment operator
//
//          The compiler supplied copy constructor and copy
//          assignment operator will do just fine:
//
//             CostSet(const CostSet& orig);              // copy constructor
//             CostSet& operator= (const CostSet& orig);  // copy assignment operator
//
//  References
//
//      Stephens, D Ryan, Christopher Diggins, Jonathan Turkanis,
//        and Jeff Cogswell.  2006.  C++ cookbook : solutions and
//        examples for C++ programmers.  O'Reilly Media,
//        Sebastopol, California, USA.  ISBN 0-596-00761-2.
//
// ---------------------------------------------------------

// STATIC DEFINITIONS - with and without explicit initialization

logga::spLogger CostSet::s_logger = logga::ptrLogStream();  // bind

// CREATORS

CostSet::CostSet
(const double init) :                        // the default of zero is set in the header
  fin(init),
  ghg(init),
  nox(init),
  dep(init),
  luc(init)
{
  if ( ! this->isFinite() )
    {
      s_logger->repx(logga::rankJumpy, "costs not finite", "common argument ctor");
    }
}

CostSet::CostSet                             // up-front constructor
(const double a_fin,
 const double a_ghg,
 const double a_nox,
 const double a_dep,
 const double a_luc) :
  fin(a_fin),
  ghg(a_ghg),
  nox(a_nox),
  dep(a_dep),
  luc(a_luc)
{
  if ( ! this->isFinite() )
    {
      s_logger->repx(logga::rankJumpy, "costs not finite", "individual argument ctor");
    }
}

CostSet::CostSet                             // tuple constructor
(const tupleD5 tup) :
  fin(tup.get<0>()),
  ghg(tup.get<1>()),
  nox(tup.get<2>()),
  dep(tup.get<3>()),
  luc(tup.get<4>())
{
  if ( ! this->isFinite() )
    {
      s_logger->repx(logga::rankJumpy, "costs not finite", "tuple argument ctor");
    }
}

CostSet::~CostSet()                          // destructor
{
  if ( ! this->isFinite() )
    {
      s_logger->repx(logga::warn, "costs not finite", "");
    }
}

// UNARY OPERATORS

// Dattatri (2002) covers the design of overloaded operators in
// chapter 8.
//
// Dattatri, Kayshav.  2002.  C++ : effective-object oriented
//   software construction : concepts, principles, industrial
//   strategies and practices -- Second edition.  Prentice Hall
//   PTR, Upper Saddle River, New Jersey, USA.  ISBN
//   0-13-086769-1.

CostSet
CostSet::operator-() const
{
  CostSet cs;
  cs.fin = -fin;
  cs.ghg = -ghg;
  cs.nox = -nox;
  cs.dep = -dep;
  cs.luc = -luc;
  return cs;
}

CostSet&
CostSet::operator+=
(const CostSet& other)
{
  fin += other.fin;
  ghg += other.ghg;
  nox += other.nox;
  dep += other.dep;
  luc += other.luc;
  return *this;
}

CostSet&
CostSet::operator-=
(const CostSet& other)
{
  fin -= other.fin;
  ghg -= other.ghg;
  nox -= other.nox;
  dep -= other.dep;
  luc -= other.luc;
  return *this;
}

CostSet&
CostSet::operator*=
(const double& other)
{
  fin *= other;
  ghg *= other;
  nox *= other;
  dep *= other;
  luc *= other;
  return *this;
}

CostSet&
CostSet::operator*=
(const tupleD5& other)
{
  fin *= other.get<0>();
  ghg *= other.get<1>();
  nox *= other.get<2>();
  dep *= other.get<3>();
  luc *= other.get<4>();
  return *this;
}

CostSet&
CostSet::operator/=
(const double& other)
{
  fin /= other;
  ghg /= other;
  nox /= other;
  dep /= other;
  luc /= other;
  return *this;
}

CostSet&
CostSet::operator/=
(const tupleD5& other)
{
  fin /= other.get<0>();
  ghg /= other.get<1>();
  nox /= other.get<2>();
  dep /= other.get<3>();
  luc /= other.get<4>();
  return *this;
}

// ACCESSORS

tupleD5
CostSet::tuple() const
{
  return boost::make_tuple(fin, ghg, nox, dep, luc);
}

// PREDICATES

bool
CostSet::isZero() const
{
  if ( fin == 0.0 &&
       ghg == 0.0 &&
       nox == 0.0 &&
       dep == 0.0 &&
       luc == 0.0)
    return true;
  else
    return false;
}

bool
CostSet::isNearZero
(xeona::Precision trip) const
{
  if ( xeona::nearZero(fin, trip) &&
       xeona::nearZero(ghg, trip) &&
       xeona::nearZero(nox, trip) &&
       xeona::nearZero(dep, trip) &&
       xeona::nearZero(luc, trip))
    return true;
  else
    return false;
}

bool
CostSet::isFinite() const
{
  if ( ! xeona::isNotFinite(fin) &&          // note the double negation
       ! xeona::isNotFinite(ghg) &&
       ! xeona::isNotFinite(nox) &&
       ! xeona::isNotFinite(dep) &&
       ! xeona::isNotFinite(luc))
    return true;
  else
    return false;
}

// MANIPULATORS

void CostSet::reset(const double value)
{ fin = value; ghg = value; nox = value; dep = value; luc = value; }

void CostSet::setFin(const double setFin) { fin = setFin; }
void CostSet::setGhg(const double setGhg) { ghg = setGhg; }
void CostSet::setNox(const double setNox) { nox = setNox; }
void CostSet::setDep(const double setDep) { dep = setDep; }
void CostSet::setLuc(const double setLuc) { luc = setLuc; }

void CostSet::plusFin(const double plusFin) { fin += plusFin; }
void CostSet::plusGhg(const double plusGhg) { ghg += plusGhg; }
void CostSet::plusNox(const double plusNox) { nox += plusNox; }
void CostSet::plusDep(const double plusDep) { dep += plusDep; }
void CostSet::plusLuc(const double plusLuc) { luc += plusLuc; }

void CostSet::multFin(const double multFin) { fin *= multFin; }
void CostSet::multGhg(const double multGhg) { ghg *= multGhg; }
void CostSet::multNox(const double multNox) { nox *= multNox; }
void CostSet::multDep(const double multDep) { dep *= multDep; }
void CostSet::multLuc(const double multLuc) { luc *= multLuc; }

// DISPLAY CALLS

std::string
CostSet::summarizeMeG                        // general float format
(std::string msg) const
{
  if ( msg.empty() ) msg = "(no msg)";
  std::ostringstream oss;
  oss << boost::format("  %15s :") %  msg
      << boost::format("   fin = %10.3g") % fin
      << boost::format("   ghg = %10.3g") % ghg
      << boost::format("   nox = %10.3g") % nox
      << boost::format("   dep = %10.3g") % dep
      << boost::format("   luc = %10.3g") % luc;
  return oss.str();
}

std::string
CostSet::summarizeMeF                        // fixed float format
(std::string msg) const
{
  if ( msg.empty() ) msg = "(no msg)";
  std::ostringstream oss;
  oss << boost::format("  %15s :") %  msg
      << boost::format("   fin = %10.2f") % fin
      << boost::format("   ghg = %10.2f") % ghg
      << boost::format("   nox = %10.2f") % nox
      << boost::format("   dep = %10.2f") % dep
      << boost::format("   luc = %10.2f") % luc;
  return oss.str();
}

// ---------------------------------------------------------
//  MEMBER FUNCTION : streamOut
// ---------------------------------------------------------
//  Description  : workhorse for overloaded operator<<
//  Role         : for streaming cost sets
//  Techniques   : 'std::ostream'
//  Status       : working
// ---------------------------------------------------------

std::ostream&
CostSet::streamOut                            // support for overloaded operator<<
(std::ostream& os) const
{
  std::ios::fmtflags prior = os.flags();     // grab ostream state
  os << boost::format(  "fin = %.3g") % fin
     << boost::format("  ghg = %.3g") % ghg
     << boost::format("  nox = %.3g") % nox
     << boost::format("  dep = %.3g") % dep
     << boost::format("  luc = %.3g") % luc;
  os.flags(prior);                           // reset ostream state
  return os;
}

// ---------------------------------------------------------
//  FREE FUNCTION   : operator+ (CostSet&, CostSet&)
// ---------------------------------------------------------

const CostSet
operator+
(const CostSet& lhs,
 const CostSet& rhs)
{
  CostSet tmp;
  tmp.fin = lhs.fin + rhs.fin;
  tmp.ghg = lhs.ghg + rhs.ghg;
  tmp.nox = lhs.nox + rhs.nox;
  tmp.dep = lhs.dep + rhs.dep;
  tmp.luc = lhs.luc + rhs.luc;
  return tmp;
}

// ---------------------------------------------------------
//  FREE FUNCTION   : operator- (CostSet&, CostSet&)
// ---------------------------------------------------------

const CostSet
operator-
(const CostSet& lhs,
 const CostSet& rhs)
{
  CostSet tmp;
  tmp.fin = lhs.fin - rhs.fin;
  tmp.ghg = lhs.ghg - rhs.ghg;
  tmp.nox = lhs.nox - rhs.nox;
  tmp.dep = lhs.dep - rhs.dep;
  tmp.luc = lhs.luc - rhs.luc;
  return tmp;
}

// ---------------------------------------------------------
//  FREE FUNCTION   : operator* (double&,  CostSet&)
// ---------------------------------------------------------

const CostSet
operator*
(const double&  lhs,
 const CostSet& rhs)
{
  CostSet tmp;
  tmp.fin = lhs * rhs.fin;
  tmp.ghg = lhs * rhs.ghg;
  tmp.nox = lhs * rhs.nox;
  tmp.dep = lhs * rhs.dep;
  tmp.luc = lhs * rhs.luc;
  return tmp;
}

// ---------------------------------------------------------
//  FREE FUNCTION   : operator* (CostSet&, double&)
// ---------------------------------------------------------

const CostSet
operator*
(const CostSet& lhs,
 const double&  rhs)
{
  CostSet tmp;
  tmp.fin = lhs.fin * rhs;
  tmp.ghg = lhs.ghg * rhs;
  tmp.nox = lhs.nox * rhs;
  tmp.dep = lhs.dep * rhs;
  tmp.luc = lhs.luc * rhs;
  return tmp;
}

// ---------------------------------------------------------
//  FREE FUNCTION   : operator* (CostSet&, tupleD5&)
// ---------------------------------------------------------

// supports distinct multipliers

const CostSet
operator*
(const CostSet& lhs,
 const tupleD5& rhs)
{
  CostSet tmp;
  tmp.fin = lhs.fin * rhs.get<0>();
  tmp.ghg = lhs.ghg * rhs.get<1>();
  tmp.nox = lhs.nox * rhs.get<2>();
  tmp.dep = lhs.dep * rhs.get<3>();
  tmp.luc = lhs.luc * rhs.get<4>();
  return tmp;
}

// ---------------------------------------------------------
//  FREE FUNCTION   : operator/ (CostSet&, double&)
// ---------------------------------------------------------

const CostSet
operator/
(const CostSet& lhs,
 const double&  rhs)
{
  CostSet tmp;
  tmp.fin = lhs.fin / rhs;
  tmp.ghg = lhs.ghg / rhs;
  tmp.nox = lhs.nox / rhs;
  tmp.dep = lhs.dep / rhs;
  tmp.luc = lhs.luc / rhs;
  return tmp;
}

// ---------------------------------------------------------
//  FREE FUNCTION   : operator/ (CostSet&, tupleD5&)
// ---------------------------------------------------------

// supports distinct divisors

const CostSet
operator/
(const CostSet& lhs,
 const tupleD5& rhs)
{
  CostSet tmp;
  tmp.fin = lhs.fin / rhs.get<0>();
  tmp.ghg = lhs.ghg / rhs.get<1>();
  tmp.nox = lhs.nox / rhs.get<2>();
  tmp.dep = lhs.dep / rhs.get<3>();
  tmp.luc = lhs.luc / rhs.get<4>();
  return tmp;
}

// ---------------------------------------------------------
//  FREE FUNCTION   : operator== (CostSet&, CostSet&)
// ---------------------------------------------------------

bool
operator==
(const CostSet& lhs,
 const CostSet& rhs)
{
  return (lhs.fin == rhs.fin &&
          lhs.ghg == rhs.ghg &&
          lhs.nox == rhs.nox &&
          lhs.dep == rhs.dep &&
          lhs.luc == rhs.luc);
}

// ---------------------------------------------------------
//  FREE FUNCTION   : operator!= (CostSet&, CostSet&)
// ---------------------------------------------------------

bool
operator!=
(const CostSet& lhs,
 const CostSet& rhs)
{
  return ! (lhs == rhs);
}

// ---------------------------------------------------------
//  FREE FUNCTION   : operator- (boost::tuple<CostSet, CostSet>)
// ---------------------------------------------------------

boost::tuple <CostSet, CostSet>              // normally "var" and "fix" costs
operator-                                    // unary minus (not subtraction)
(const boost::tuple <CostSet, CostSet>& other)
{
  // reporting
  static logga::spLogger logger = logga::ptrLogStream();
  logger->repx(logga::xtra, "tuple<CostSet, CostSet> unary minus", "");

  // active code
  return boost::make_tuple(-other.get<0>(),  // unary minus on recovered 'CostSet' object
                           -other.get<1>());
}

// ---------------------------------------------------------
//  FREE FUNCTION   : addCosts
// ---------------------------------------------------------

void
addCosts
(boost::tuple<CostSet, CostSet, CostSet>&       base,
 const boost::tuple<CostSet, CostSet, CostSet>& next)
{
  // reporting
  static logga::spLogger logger = logga::ptrLogStream();
  logger->repx(logga::xtra, "tuple<CostSet, CostSet, CostSet> add", "");

  // active code
  base.get<0>() += next.get<0>();
  base.get<1>() += next.get<1>();
  base.get<2>() += next.get<2>();
}

//  end of file

