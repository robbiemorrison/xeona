//  XEONA ENERGY SYSTEMS MODELING ENVIRONMENT
//
//  file-create-name : costs.h
//  file-create-date : Fri 17-Oct-2008 12:00 UTC
//  file-initiator   : Robbie Morrison <robbie@actrix.co.nz>
//
//  file-role        : cost sets and support / header
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
//  $URL: file:///home/robbie/svn-root/xeona/futz/trunk/xeona1/c/costs.h $
//
//  GENERAL NOTES FOR THIS FILE

//  AD-HOC NOTES
//
//  Code and unit tests for class 'CostSetR' can be found in
//  r2160.  This material has subsequently been deleted.

//  HEADER GUARD

#ifndef _COSTS_H_
#define _COSTS_H_

//  LOCAL AND SYSTEM INCLUDES

#include "../a/logger_fwd.h"  // some forward declarations from "logger.h"
#include "../c/smart_ptr.h"   // toggle between Boost and TR1 smart pointers

#include "../c/util3.h"       // free functions for floating point comparison
#include "../c/util2.h"       // free functions which offer general utilities 2

#include <string>             // C++ strings
#include <sstream>            // string-streams

#include <boost/tuple/tuple.hpp>        // n-tuples, ref(), cref()

// CAUTION: forward declaration of enums is supported in C++11
// but not before, hence the hash-inclusion of 'c/util3.h'

//  CODE

// ---------------------------------------------------------
//  TYPEDEF         : tupleD5
// ---------------------------------------------------------

typedef boost::tuple<double, double, double, double, double> tupleD5;

// ---------------------------------------------------------
//  CLASS           : CostSet
// ---------------------------------------------------------
//  Description  : container for multiple cost types, be they intensive or extensive
//  Role         : to support 'OptimSubProb' instances and for cost aggregation
//  Techniques   : struct, overloaded operators, tuples
//  Status       : complete
//
//  Design notes
//
//      Cost sets support the (currently five) literal cost types
//      as public data members.
//
//      Cost sets can be used in several contexts to hold:
//
//         - cost formation parameters - namely specific costs
//
//         - accrued costs - or their various contributions such as
//             - variable
//             - fixed
//             - embedded
//
//        - unit costs - the cost of unit output
//             - average
//             - marginal
//
//      Cost sets do not apply to prices, which are limited to
//      the financial cost type and often held as 'double's.
//      That said, there is no reason preventing incomplete cost
//      sets being used to hold prices.
//
//  Further information
//
//      Please refer to my (Robbie Morrison) PhD thesis for more
//      information.
//
// ---------------------------------------------------------

class CostSet
{
  // CAUTION: the various overloaded free function operators do
  // not need to be granted friendship as the data members are
  // public

  // DISABLED

  // none

  // CREATORS

public:

  CostSet                                    // also acts as a zero argument constructor
  (const double init = 0.0);

  CostSet                                    // up-front constructor
  (const double a_fin,
   const double a_ghg,
   const double a_nox,
   const double a_dep,
   const double a_luc);

  CostSet                                    // tuple constructor
  (const tupleD5 tup);

  ~CostSet();                                // destructor

  // UNARY OPERATORS

  CostSet  operator- () const;                    // unary minus (not subtraction)
  CostSet& operator+= (const CostSet& other);
  CostSet& operator-= (const CostSet& other);
  CostSet& operator*= (const double&  other);     // common multiplier
  CostSet& operator*= (const tupleD5& other);     // supports distinct multipliers
  CostSet& operator/= (const double&  other);     // common divisor
  CostSet& operator/= (const tupleD5& other);     // supports distinct divisors

  // ACCESSORS

  // NOTE: the data members are also public and directly accessible

  tupleD5 tuple() const;                     // export 'CostSet' to five tuple

  // PREDICATES

  bool isZero()                          const;   // 'true' if all entries are zero
  bool isNearZero(xeona::Precision trip) const;   // 'true' if all entries are near zero
  bool isFinite()                        const;   // 'true' if all entries are finite

  // MANIPULATORS

  void reset(const double value = 0.0);      // reset all values, note the default of zero

  void setFin(const double setFin);          // equivalent to: cs.fin = setFin
  void setGhg(const double setGhg);
  void setNox(const double setNox);
  void setDep(const double setDep);
  void setLuc(const double setLuc);

  void plusFin(const double plusFin);        // equivalent to: cs.fin += plusFin
  void plusGhg(const double plusGhg);
  void plusNox(const double plusNox);
  void plusDep(const double plusDep);
  void plusLuc(const double plusLuc);

  void multFin(const double multFin);        // equivalent to: cs.fin *= multFin
  void multGhg(const double multGhg);
  void multNox(const double multNox);
  void multDep(const double multDep);
  void multLuc(const double multLuc);

  // DISPLAY CALLS

  std::string
  summarizeMeG                               // general float format using %10.3g
  (std::string msg = "") const;

  std::string
  summarizeMeF                               // fixed float format using %10.2f
  (std::string msg = "") const;

  // STREAM INSERTION SUPPORT

  std::ostream&
  streamOut                                  // support for overloaded operator<<
  (std::ostream& os) const;

  // INSTANCE DATA

public:                                      // CAUTION: public access is correct

  double    fin;                             // financial cost type
  double    ghg;                             // greenhouse gas cost type
  double    nox;                             // NOx cost type
  double    dep;                             // depletable resource use cost type
  double    luc;                             // land use cost type

  // STATIC DATA

private:

  static logga::spLogger    s_logger;        // shared_ptr to single logger object

};

// ---------------------------------------------------------
//  FREE FUNCTION   : operator<< (std::ostream&, CostSet&)
// ---------------------------------------------------------
//
//  CAUTION: place after client class declaration
//
// ---------------------------------------------------------

inline                                       // CAUTION: inline essential
std::ostream&
operator<<                                   // overloaded operator
(std::ostream& os,
const CostSet& cs)
{
  return cs.streamOut(os);                   // wrapper to 'streamOut' member function
}

// FREE FUNCTION BINARY OPERATORS - arithmetic, comparison

// ---------------------------------------------------------
//  FREE FUNCTION   : operator+ (CostSet&, CostSet&)
//  FREE FUNCTION   : operator- (CostSet&, CostSet&)
//  FREE FUNCTION   : operator* (double&,  CostSet&)
//  FREE FUNCTION   : operator* (CostSet&, double&)
//  FREE FUNCTION   : operator* (CostSet&, tupleD5&)
//  FREE FUNCTION   : operator/ (CostSet&, double&)
//  FREE FUNCTION   : operator/ (CostSet&, tupleD5&)
// ---------------------------------------------------------

const CostSet operator+ (const CostSet& lhs, const CostSet& rhs);
const CostSet operator- (const CostSet& lhs, const CostSet& rhs);
const CostSet operator* (const double&  lhs, const CostSet& rhs);
const CostSet operator* (const CostSet& lhs, const double&  rhs);
const CostSet operator* (const CostSet& lhs, const tupleD5& rhs);
const CostSet operator/ (const CostSet& lhs, const double&  rhs);
const CostSet operator/ (const CostSet& lhs, const tupleD5& rhs);

// ---------------------------------------------------------
//  FREE FUNCTION   : operator== (CostSet&, CostSet&)
//  FREE FUNCTION   : operator!= (CostSet&, CostSet&)
// ---------------------------------------------------------

bool operator== (const CostSet& lhs, const CostSet& rhs);
bool operator!= (const CostSet& lhs, const CostSet& rhs);

// ---------------------------------------------------------
//  FREE FUNCTION   : operator- (boost::tuple<CostSet, CostSet>)
// ---------------------------------------------------------
//  Description  : unary minus (not subtraction)
//  Role         : intended for turning costs into revenues thru sign-change
//  Techniques   : (nothing special)
//  Status       : complete
// ---------------------------------------------------------

boost::tuple <CostSet, CostSet>              // normally "var" and "fix" costs
operator-                                    // unary minus (not subtraction)
(const boost::tuple <CostSet, CostSet>& other);

// ---------------------------------------------------------
//  FREE FUNCTION   : addCosts
// ---------------------------------------------------------
//  Description  : add 'next' to 'base'
//  Role         : update (var, fix, emb) cost set triples
//  Techniques   : (nothing special)
//  Status       : complete
// ---------------------------------------------------------

void
addCosts
(boost::tuple<CostSet, CostSet, CostSet>&       base,
 const boost::tuple<CostSet, CostSet, CostSet>& next);

#endif // _COSTS_H_

//  end of file

