//  XEONA ENERGY SYSTEMS MODELING ENVIRONMENT
//
//  file-create-name : asop03.h
//  file-create-date : Thu 26-Nov-2009 13:22 UTC
//  file-initiator   : Robbie Morrison <robbie@actrix.co.nz>
//
//  file-role        : concrete asset operators 3 / header
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
//  $URL: file:///home/robbie/svn-root/xeona/futz/trunk/xeona1/b/asop03.h $
//
//  GENERAL NOTES FOR THIS FILE

//  HEADER GUARD

#ifndef _ASOP03_H_
#define _ASOP03_H_

//  LOCAL AND SYSTEM INCLUDES

#include "../b/auxs01.h"      // classes for auxiliary model data
#include "../b/asop.h"        // asset operator entity

#include "../a/logger_fwd.h"  // some forward declarations from "logger.h"
#include "../c/smart_ptr.h"   // toggle between Boost and TR1 smart pointers

#include <string>             // C++ strings

//  CODE

// CAUTION: forward declarations given in the code for convenience

// ---------------------------------------------------------
//  CLASS           : AsopInelasticTs
// ---------------------------------------------------------
//  Description  : asset operator with stated demand and thereby price-inelastic behavior
//  Role         : concrete entity
//  Techniques   : (nothing special)
//  Status       : complete
// ---------------------------------------------------------

class CtlQuan_A;                             // necessary for OSP typedef below

class AsopInelasticTs :
  public AssetOperator
{
  // USING DECLARATIONS

protected:

  using Entity::s_logger;                    // place in common scope for this class

  // TYPEDEFS

private:

  typedef CtlQuan_A CtlQuan;                 // used for switching implementations

  // DISABLED

private:

  AsopInelasticTs();                                        // zero-argument constructor
  AsopInelasticTs(const AsopInelasticTs& orig);             // copy constructor
  AsopInelasticTs& operator= (const AsopInelasticTs& orig); // copy assignment operator

  // CREATORS

public:

  explicit
  AsopInelasticTs
  (const std::string entityId,
   Record&           record);

  virtual
  ~AsopInelasticTs();

  // DOMAIN CONTROLLER POINTS OF ENTRY

public:

  virtual
  const int                                  // number of technical assets processed
  constrain
  (const xeona::DomainMode capacityMode);

  // INSTANCE DATA

private:

  // tied quantities

  const shared_ptr<std::vector<double> >    d_demands;

  // local quantities

  shared_ptr<CtlQuan>                       d_ctl;     // specialization required
  std::vector<shared_ptr<CtlQuan> >         d_ctls;

}; // class 'AsopInelasticTs'

//  ==== XEDOC =================================================
//
//  entity.asop-inelastic-ts-0
//
//        quantifying extensity * in {J,kg,$} as appropriate
//
//      class                                    > AsopInelasticTs
//
//        asset operator with stated demand and thereby
//        price-inelastic behavior
//
//      builtin-remark s                         <
//
//      technical-assets L                       > "teas-1 teas-2"
//
//        the technical-assets should have the same quantifying
//        extensity
//
//      demands [*/s] F                          > 5.0e+06 ..
//
//        the demand needs to be extensity-compatible with the
//        associated assets (not checked), in addition each asset
//        gets the same value
//
//  ============================================================

// ---------------------------------------------------------
//  CLASS           : AsopInelasticParam
// ---------------------------------------------------------
//  Description  : asset operator with stated demand and thereby price-inelastic behavior
//  Role         : concrete entity
//  Techniques   : 'xeona::vectorFillDiurnal' to generate demand timeseries
//  Status       : complete
// ---------------------------------------------------------

class CtlQuan_A;                             // necessary for OSP typedef below

class AsopInelasticParam :
  public AssetOperator
{
  // USING DECLARATIONS

protected:

  using Entity::s_logger;                    // place in common scope for this class

  // TYPEDEFS

private:

  typedef CtlQuan_A CtlQuan;                 // used for switching implementations

  // DISABLED

private:

  AsopInelasticParam();                                           // zero-argument ctor
  AsopInelasticParam(const AsopInelasticParam& orig);             // copy constructor
  AsopInelasticParam& operator= (const AsopInelasticParam& orig); // copy assignment oper

  // CREATORS

public:

  explicit
  AsopInelasticParam
  (const std::string entityId,
   Record&           record);

  virtual
  ~AsopInelasticParam();

  // DOMAIN CONTROLLER POINTS OF ENTRY

public:

  virtual
  const int                                  // number of technical assets processed
  constrain
  (const xeona::DomainMode capacityMode);

  // INSTANCE DATA

private:

  // tied quantities

  const double&                             d_demandMean;
  const double&                             d_demandAmplitude;
  const int&                                d_temporalShift;
  const double&                             d_demandRandomness;

  // local quantities

  shared_ptr<std::vector<double> >          d_demands; // generated timeseries

  shared_ptr<CtlQuan>                       d_ctl;     // specialization required
  std::vector<shared_ptr<CtlQuan> >         d_ctls;

}; // class 'AsopInelasticParam'

//  ==== XEDOC =================================================
//
//  entity.asop-inelastic-param-0
//
//        quantifying extensity * in {J,kg,$} as appropriate
//
//      class                                    > AsopInelasticParam
//
//        asset operator with generated diurnal demand and
//        thereby price-inelastic behavior
//
//      builtin-remark s                         <
//
//      technical-assets L                       > "teas-1 teas-2"
//
//        the technical-assets should have the same quantifying
//        extensity
//
//      demand-mean [*/s] f                      > 9.0e+06
//      demand-amplitude [*/s] f                 > 2.0e+06
//      temporal-shift [h] i                     > 6
//      demand-randomness [-] f                  > 0.1
//
//        demand-mean is the mean value before noise,
//        demand-amplitude is peak-to-peak measure,
//        temporal-shift is relative to midnight,
//        demand-randomness is the maximum noise relative to the
//        amplitude, use zero to disable
//
//  ============================================================

// ---------------------------------------------------------
//  CLASS           : AsopAdaptiveTs
// ---------------------------------------------------------
//  Description  : asset operator with stated demand and thereby price-inelastic behavior
//  Role         : concrete entity
//  Techniques   : (nothing special)
//  Status       : complete
//
//  Design notes
//
//      This entity could be the forerunner for adaptive
//      end-users.
//
// ---------------------------------------------------------

class CtlQuan_A;                             // necessary for OSP typedef below

class AsopAdaptiveTs :
  public AssetOperator
{
  // USING DECLARATIONS

protected:

  using Entity::s_logger;                    // place in common scope for this class

  // TYPEDEFS

private:

  typedef CtlQuan_A CtlQuan;                 // used for switching implementations

  // DISABLED

private:

  AsopAdaptiveTs();                                         // zero-argument constructor
  AsopAdaptiveTs(const AsopAdaptiveTs& orig);               // copy constructor
  AsopAdaptiveTs& operator= (const AsopAdaptiveTs& orig);   // copy assignment operator

  // CREATORS

public:

  explicit
  AsopAdaptiveTs
  (const std::string entityId,
   Record&           record);

  virtual
  ~AsopAdaptiveTs();

  // DOMAIN CONTROLLER POINTS OF ENTRY

public:

  virtual
  const int                                  // number of technical assets processed
  constrain
  (const xeona::DomainMode capacityMode);

  // UTILITY FUNCTIONS

private:

  bool                                       // 'true' if 'demand' was modified
  adapt1
  (shared_ptr<TechnicalAsset> ta,            // associated technical asset
   double&                    demand);       // original demand

  // INSTANCE DATA

private:

  // tied quantities

  const shared_ptr<std::vector<double> >    d_demands;
  const double&                             d_unitPriceThreshold;
  const double&                             d_adaptFactor;

  shared_ptr<std::vector<bool> >            d_curtailments;

  // local quantities

  shared_ptr<CtlQuan>                       d_ctl;     // specialization required
  std::vector<shared_ptr<CtlQuan> >         d_ctls;

}; // class 'AsopAdaptiveTs'

//  ==== XEDOC =================================================
//
//  entity.asop-adaptive-ts-0
//
//        quantifying extensity * in {J,kg,$} as appropriate
//
//      class                                    > AsopAdaptiveTs
//
//        asset operator with stated demand and with
//        price-adaptive behavior -- so that partial curtainment
//        without backlogging may occur
//
//      builtin-remark s                         <
//
//      technical-assets L                       > "teas-1 teas-2"
//
//        the technical-assets should have the same quantifying
//        extensity
//
//      demands [*/s] F                          > 5.0e+06 ..
//      unit-price-threshold [$/*] f             > 30.0e-09
//      adapt-factor [-] f                       > 0.9
//
//        the adapt-factor is envoked when the prevailing
//        marginal price trips the unit-price-threshold, this
//        causes the current curtailment to become true
//
//      curtailments [-] B                       < 0 ..
//
//        the demand needs to be extensity-compatible with the
//        associated assets (not checked), in addition each asset
//        gets the same value
//
//  ============================================================

#endif // _ASOP03_H_

//  end of file

