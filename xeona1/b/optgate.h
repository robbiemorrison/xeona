//  XEONA ENERGY SYSTEMS MODELING ENVIRONMENT
//
//  file-create-name : optgate.h
//  file-create-date : Wed 25-Mar-2009 21:08 UTC
//  file-initiator   : Robbie Morrison <robbie@actrix.co.nz>
//
//  file-role        : various OSPs for gateways / header
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
//  $URL: file:///home/robbie/svn-root/xeona/futz/trunk/xeona1/b/optgate.h $
//
//  GENERAL NOTES FOR THIS FILE

//  HEADER GUARD

#ifndef _OPTGATE_H_
#define _OPTGATE_H_

//  LOCAL AND SYSTEM INCLUDES

#include "../b/optprob.h"     // optimization sub-problem and key sub-classes

#include "../a/logger_fwd.h"  // some forward declarations from "logger.h"
#include "../c/smart_ptr.h"   // toggle between Boost and TR1 smart pointers

#include <string>             // C++ strings

//  FORWARD (PARTIAL) DECLARATIONS

class BandedTariffSet;                       // an 'uploadTariffSet' argument

//  CODE

// ---------------------------------------------------------
//  CLASS           : QanTechCapacity_A
// ---------------------------------------------------------
//  Description  : create a technical capacity upper bound OSP
//  Role         : concrete gateways
//  Techniques   : (nothing special)
//  Status       : complete
// ---------------------------------------------------------

class QanTechCapacity_A :
  public QuantityOsp
{
  // USING DECLARATIONS

protected:

  using OptimSubProb::s_logger;              // place in common scope for this class

  // TYPEDEFS

public:

  typedef boost::tuple                       // can be in either local or global cols
  <int>                                      // coupling index for internal use
  index_type;

  typedef boost::tuple
  <double>                                   // technical duty (recovered)
  results_type;

  // CREATORS

public:

  QanTechCapacity_A
  (shared_ptr<svif::SolverIf> solver,
   const xeona::DomainMode    commitmentMode,
   const std::string          tag);

  virtual
  ~QanTechCapacity_A();

  // UPLOAD CALLS

public:

  index_type
  uploadCapacity                             // wrapper to two argument version
  (const std::pair<double, double> capacity);

  index_type
  uploadCapacity
  (const double loCapacity,
   const double hiCapacity);

  // DOWNLOAD SOLUTION CALLS

  results_type
  downloadSolution() const;

  // INSTANCE DATA

private:

  index_type    d_cols;                      // local cols
  double        d_capacity;                  // stored so solution can be interpreted

}; // class 'QanTechCapacity_A'

// ---------------------------------------------------------
//  CLASS           : OfrTariffSet_A
// ---------------------------------------------------------
//  Description  : create a tariff set offer OSP
//  Role         : concrete gateways
//  Techniques   : (nothing special)
//  Status       : complete
// ---------------------------------------------------------

class OfrTariffSet_A :
  public OfferOsp
{
  // USING DECLARATIONS

protected:

  using OptimSubProb::s_logger;              // place in common scope for this class

  // TYPEDEFS

public:

  typedef boost::tuple                       // can be in either local or global cols
  <int>                                      // coupling index for internal use
  index_type;

  typedef boost::tuple
  <double,                                   // marginal price ($/quantity)
   double,                                   // fixed component ($)
   double,                                   // variable componet ($)
   double>                                   // transaction (quantity)
  results_type;

private:

  typedef std::pair
  <double,                                   // band
   double>                                   // price
  tariff_type;

  // DISABLED

private:

  OfrTariffSet_A();                                         // zero-argument constructor
  OfrTariffSet_A(const OfrTariffSet_A& orig);               // copy constructor
  OfrTariffSet_A& operator= (const OfrTariffSet_A& orig);   // copy assignment operator

  // CREATORS

public:

  OfrTariffSet_A
  (shared_ptr<svif::SolverIf> solver,
   const xeona::DomainMode    commitmentMode,
   const std::string          tag);

  // UPLOAD CALLS

public:

  index_type
  uploadTariffSet
  (const shared_ptr<BandedTariffSet> original,    // copied ctored before emptying
   const double                      capacity);

  // DOWNLOAD SOLUTION CALLS

  results_type
  downloadSolution
  (const int interval) const;                // horizon interval

  // INSTANCE DATA

private:

  index_type                     d_cols;          // local cols
  shared_ptr<BandedTariffSet>    d_tariffset;     // stored so solution can be interpreted
  double                         d_capacity;      // ditto

}; // class 'OfrTariffSet_A'

// ---------------------------------------------------------
//  CLASS           : QanObligToSupply_A
// ---------------------------------------------------------
//  Description  : create obligation to suppy as an equality constraint OSP
//  Role         : concrete gateways
//  Techniques   : (nothing special)
//  Status       : complete
// ---------------------------------------------------------

class QanObligToSupply_A :
  public QuantityOsp
{
  // USING DECLARATIONS

protected:

  using OptimSubProb::s_logger;              // place in common scope for this class

  // TYPEDEFS

public:

  typedef boost::tuple                       // can be in either local or global cols
  <int>                                      // coupling index for internal use
  index_type;

  typedef boost::tuple
  <double>                                   // obligation (recovered)
  results_type;

  // DISABLED

private:

  QanObligToSupply_A();                                           // zero-argument ctor
  QanObligToSupply_A(const QanObligToSupply_A& orig);             // copy constructor
  QanObligToSupply_A& operator= (const QanObligToSupply_A& orig); // copy assignment op

  // CREATORS

public:

  QanObligToSupply_A
  (shared_ptr<svif::SolverIf> solver,
   const xeona::DomainMode    commitmentMode,
   const std::string          tag);

  // UPLOAD CALLS

  index_type
  uploadObligation
  (const double supplyObligation);

  // DOWNLOAD SOLUTION CALLS

  results_type
  downloadSolution() const;

  // INSTANCE DATA

private:

  index_type    d_cols;                      // local cols

}; // class 'QanObligToSupply_A'

#endif // _OPTGATE_H_

//  end of file

