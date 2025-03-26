//  XEONA ENERGY SYSTEMS MODELING ENVIRONMENT
//
//  file-create-name : teas13.h
//  file-create-date : Thu 17-Nov-2011 22:12 UTC
//  file-initiator   : Robbie Morrison <robbie@actrix.co.nz>
//
//  file-role        : concrete technical assets 13 / header
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
//  $URL: file:///home/robbie/svn-root/xeona/futz/trunk/xeona1/b/teas13.h $
//
//  GENERAL NOTES FOR THIS FILE
//
//  This unit contains windfarm entities.

//  HEADER GUARD

#ifndef _TEAS13_H_
#define _TEAS13_H_

//  AD-HOC NOTES

//  LOCAL AND SYSTEM INCLUDES

#include "../b/teas.h"        // technical asset entity
#include "../b/costreg.h"     // cost registers

#include "../a/logger_fwd.h"  // some forward declarations from "logger.h"
#include "../c/smart_ptr.h"   // toggle between Boost and TR1 smart pointers

#include <string>             // C++ strings

#include <string>             // C++ strings

#include <boost/tuple/tuple.hpp>             // n-tuples, ref(), cref()

//  FORWARD (PARTIAL) DECLARATIONS

class Record;                                // constructor argument
namespace svif { class SolverIf; }           // member function argument

template <typename C> class Cable;           // 'C' is of base class 'Commodity' etc
template <typename C> class Socket;

class CmElectricity;                         // 'CmElectricity' is from 'Commodity' etc

class CxAmbientAir;

//  CODE

// ---------------------------------------------------------
//  CLASS           : TeasWindfarm
// ---------------------------------------------------------
//  Description  : windfarm comprising one or more identical turbines
//  Role         : concrete entity
//  Techniques   : 'OpsFac0Out1' source-style OSP
//  Status       : complete
// ---------------------------------------------------------

class OpsFac0Out1_A;                         // necessary for OSP typedef below

class TeasWindfarm :
  public TechnicalAsset,
  public CostRegisterSRFin,
  public CostRegisterEmbFin
{
  // USING DECLARATIONS

protected:

  using Entity::s_logger;                    // place in common scope for this class

  // TYPEDEFS

private:

  typedef OpsFac0Out1_A OpsFac0Out1;         // used for switching implementations

  // DISABLED

private:

  TeasWindfarm();                                      // zero-argument constructor
  TeasWindfarm(const TeasWindfarm& orig);              // copy constructor
  TeasWindfarm& operator= (const TeasWindfarm& orig);  // copy assignment operator

  // CREATORS

public:

  explicit
  TeasWindfarm
  (const std::string entityId,
   Record&           record);

  virtual
  ~TeasWindfarm();

  // CALLS

public:

  virtual
  void
  establish();                               // with low priority development reporting

  virtual
  const int                                  // duty gol
  constrain
  (const xeona::DomainMode capacityMode);

  virtual
  void
  washup();

  virtual
  void
  conclude();

  // UTILITY FUNCTIONS

protected:

  double                                     // resultant power [W]
  calcTurbinePower                           // single turbine
  (const double windSpeed);                  // known wind speed [m/s]

  // INSTANCE DATA

private:

  // tied quantities

  assign_ptr<CxAmbientAir>             d_ambientAirContext;

  const int&                           d_count;
  const double&                        d_turbineRating;
  const double&                        d_loCutSpeed;
  const double&                        d_hiCutSpeed;

  shared_ptr<std::vector<double> >     d_potentialProductions;
  shared_ptr<std::vector<double> >     d_actualProductions;
  double&                              d_availability;
  double&                              d_spill;

  shared_ptr<Socket<CmElectricity> >   d_outElec;

  // local quantities

  shared_ptr<OpsFac0Out1>              d_ops;     // specialization required

};

//  ==== XEDOC =================================================
//
//  entity.teas-windfarm-0
//
//      class                                    > TeasWindfarm
//
//        a windfarm comprising one or more identical turbines,
//        which can also spill wind
//
//        my socket label is 'elec-1'
//
//      builtin-remark s                         <
//
//      count [-] i                              > 50
//      turbine-rating [W] f                     > 3000e+03
//      lo-cut-speed [m/s] f                     > 5.0
//      hi-cut-speed [m/s] f                     > 14.0
//
//        the turbine-rating applies at the cut-out-speed and can
//        be multiplied by count to calculate the windfarm capacity
//
//      socket-electricity-commodity l           > "cm-electricity-0"
//
//        socket-electricity-commodity defines the shared
//        electricity commodity
//
//      ambient-air-context l                    > "cx-ambient-air-0"
//
//        the context entity must be a sub-class of CxAmbientAir
//
//      potential-productions [W] F              < 0.0 ..
//      actual-productions [W] F                 < 0.0 ..
//      availability [-] f                       < 0.0
//      spill [-] f                              < 0.0
//
//        availability [0,1] is the ratio of potential output to
//        turbine-rating, spill [0,1] is the ratio of discarded
//        to potential output
//
//      nameplate-capacity [W] f                 > 3000e+03
//      duty-specific-cost-financial [$/J] f     > 1.4e-09
//      size-specific-cost-financial [$/W/s] f   > 0.0
//      standing-cost-financial [$/s] f          > 0.0
//
//      variable-costs-financial [$] F           < 0.0 ..
//      fixed-costs-financial [$] F              < 0.0 ..
//
//      annual-discount-rate-decimal [-] f       > 0.10
//      economic-life [y] i                      > 25
//      capex-initial [$] f                      > +7.80e+06
//      capex-terminal [$] f                     > -0.78e+06
//      current-age [y] i                        > 2
//
//        a negative capex-terminal indicates salvage income as
//        opposed to decommissioning cost
//
//      embedded-costs-financial [$] F           < 0.0 ..
//
//  ============================================================

#endif // _TEAS13_H_

//  end of file

