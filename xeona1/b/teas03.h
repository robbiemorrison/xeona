//  XEONA ENERGY SYSTEMS MODELING ENVIRONMENT
//
//  file-create-name : teas03.h
//  file-create-date : Sat 16-May-2009 12:24 UTC
//  file-initiator   : Robbie Morrison <robbie@actrix.co.nz>
//
//  file-role        : concrete technical assets 3 / header
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
//  $URL: file:///home/robbie/svn-root/xeona/futz/trunk/xeona1/b/teas03.h $
//
//  GENERAL NOTES FOR THIS FILE

//  HEADER GUARD

#ifndef _TEAS03_H_
#define _TEAS03_H_

//  LOCAL AND SYSTEM INCLUDES

#include "../b/teas.h"        // technical asset entity
#include "../b/costreg.h"     // cost registers

#include "../a/logger_fwd.h"  // some forward declarations from "logger.h"
#include "../c/smart_ptr.h"   // toggle between Boost and TR1 smart pointers

#include <string>             // C++ strings
#include <sstream>            // string-streams

//  FORWARD (PARTIAL) DECLARATIONS

class Record;                                // constructor argument
namespace svif { class SolverIf; }           // member function argument

template <typename C> class Cable;           // 'C' is of base class 'Commodity' etc
template <typename C> class Socket;

class CmElectricity;                         // 'CmElectricity' is from 'Commodity' etc
class CmOxidize;                             // 'CmOxidize' is from 'Commodity' etc

//  CODE

// ---------------------------------------------------------
//  CLASS           : TeasLoadElecTs
// ---------------------------------------------------------
//  Description  : timeseries prescribed electricity load
//  Role         : concrete entity
//  Techniques   : 'OpsFac1Out0' sink-style OSP
//  Status       : complete
// ---------------------------------------------------------

class OpsFac1Out0_A;                         // necessary for OSP typedef below

class TeasLoadElecTs :
  public TechnicalAsset,
  public CostRegisterSRFin,
  public CostRegisterEmbFin
{
  // USING DECLARATIONS

protected:

  using Entity::s_logger;                    // place in common scope for this class

  // TYPEDEFS

private:

  typedef OpsFac1Out0_A OpsFac1Out0;         // used for switching implementations

  // DISABLED

private:

  TeasLoadElecTs();                                         // zero-argument constructor
  TeasLoadElecTs(const TeasLoadElecTs& orig);               // copy constructor
  TeasLoadElecTs& operator= (const TeasLoadElecTs& orig);   // copy assignment operator

  // CREATORS

public:

  explicit
  TeasLoadElecTs
  (const std::string entityId,
   Record&           record);

  virtual
  ~TeasLoadElecTs();

  // CALLS

public:

  virtual void establish() { }               // necessary redefinition

  virtual
  const int                                  // duty gol
  constrain
  (const xeona::DomainMode capacityMode);

  virtual
  void
  washup();

  virtual void conclude()  { }               // necessary redefinition

  // INSTANCE DATA

private:

  // tied quantities

  const shared_ptr<std::vector<double> >    d_loads;
  shared_ptr<Cable<CmElectricity> >         d_inElec;

  // local quantities

  shared_ptr<OpsFac1Out0>                   d_ops;     // specialization required

};

//  ==== XEDOC =================================================
//
//  entity.teas-load-elec-ts-0
//
//      class                                    > TeasLoadElecTs
//
//        prescribed electricity load, lacking both flexibility
//        and context dependency
//
//      builtin-remark s                         <
//
//      socket-electricity l                     > "teas-supplier-0.elec-1"
//      cable-electricity-commodity l            > "cm-electricity-0"
//
//        the above fields define my supplier and their socket
//        label and our common electricity commodity
//
//      loads [W] F                              > 1.0e+06 0.5e+06 ..
//
//        the loads are simply specified
//
//      nameplate-capacity [W] f                 > 2.0e+06
//      duty-specific-cost-financial [$/J] f     > 100.0
//      size-specific-cost-financial [$/W/s] f   > 200.0
//      standing-cost-financial [$/s] f          > 300.0
//
//      variable-costs-financial [$] F           < 0.0 ..
//      fixed-costs-financial [$] F              < 0.0 ..
//
//      annual-discount-rate-decimal [-] f       > 0.10
//      economic-life [y] i                      > 25
//      capex-initial [$] f                      > 120e+03
//      capex-terminal [$] f                     > -10e+03
//      current-age [y] i                        > 2
//
//        a negative capex-terminal indicates salvage income as
//        opposed to decommissioning cost
//
//      embedded-costs-financial [$] F           < 0.0 ..
//
//        the embedded-costs-financial are calculated using the
//        DCF annuity method over the economic life of the entity
//        in question
//
//  ============================================================

// ---------------------------------------------------------
//  CLASS           : TeasMineOxid
// ---------------------------------------------------------
//  Description  : oxidizable commodity source
//  Role         : concrete entity
//  Techniques   : 'OpsFac0Out1' source-style OSP
//  Status       : complete
// ---------------------------------------------------------

class OpsFac0Out1_A;                         // necessary for OSP typedef below

class TeasMineOxid :
  public TechnicalAsset,
  public CostRegisterSRFin
{
  // USING DECLARATIONS

protected:

  using Entity::s_logger;                    // place in common scope for this class

  // TYPEDEFS

private:

  typedef OpsFac0Out1_A OpsFac0Out1;         // used for switching implementations

  // DISABLED

private:

  TeasMineOxid();                                      // zero-argument constructor
  TeasMineOxid(const TeasMineOxid& orig);              // copy constructor
  TeasMineOxid& operator= (const TeasMineOxid& orig);  // copy assignment operator

  // CREATORS

public:

  explicit
  TeasMineOxid
  (const std::string entityId,
   Record&           record);

  virtual
  ~TeasMineOxid();

  // CALLS

public:

  virtual void establish() { }               // necessary redefinition

  virtual
  const int                                  // duty gol
  constrain
  (const xeona::DomainMode capacityMode);

  virtual
  void
  washup();

  virtual void conclude()  { }               // necessary redefinition

  // INSTANCE DATA

private:

  // tied quantities

  const double&                             d_extractLoBound;
  const double&                             d_extractHiBound;

  const shared_ptr<std::vector<double> >    d_extractions;
  shared_ptr<Socket<CmOxidize> >            d_outOxid;

  // local quantities

  shared_ptr<OpsFac0Out1>                   d_ops;     // specialization required

};

//  ==== XEDOC =================================================
//
//  entity.teas-mine-oxid-0
//
//      class                                    > TeasMineOxid
//
//        flexible oxidizable commodity source -- somewhat
//        similar to a stockpile
//
//        this entity is provided for model development purposes
//
//      builtin-remark s                         <
//
//      socket-oxide-commodity l                 > "cm-oxidize-0"
//
//        socket-oxide-commodity defines the shared commodity
//
//      extract-lo-bound [J/s] f                 > 0.0
//      extract-hi-bound [J/s] f                 > 8.0e+03
//
//        extract-hi-bound is required by the simulation whereas
//        nameplate-capacity informs the financial calculations
//
//      extractions [kg] F                       < 0.0 ..
//
//        financial data
//
//      nameplate-capacity [kg/s] f              > 10.0
//      duty-specific-cost-financial [$/kg/s] f  > 100.0
//      size-specific-cost-financial [$/kg/s] f  > 200.0
//      standing-cost-financial [$/s] f          > 300.0
//
//      variable-costs-financial [$] F           < 0.0 ..
//      fixed-costs-financial [$] F              < 0.0 ..
//
//  ============================================================

// ---------------------------------------------------------
//  CLASS           : TeasMineElec
// ---------------------------------------------------------
//  Description  : electricity commodity source
//  Role         : concrete entity
//  Techniques   : 'OpsFac0Out1' source-style OSP
//  Status       : complete
// ---------------------------------------------------------

class OpsFac0Out1_A;                         // necessary for OSP typedef below

class TeasMineElec :
  public TechnicalAsset,
  public CostRegisterSRFin
{
  // USING DECLARATIONS

protected:

  using Entity::s_logger;                    // place in common scope for this class

  // TYPEDEFS

private:

  typedef OpsFac0Out1_A OpsFac0Out1;         // used for switching implementations

  // DISABLED

private:

  TeasMineElec();                                      // zero-argument constructor
  TeasMineElec(const TeasMineElec& orig);              // copy constructor
  TeasMineElec& operator= (const TeasMineElec& orig);  // copy assignment operator

  // CREATORS

public:

  explicit
  TeasMineElec
  (const std::string entityId,
   Record&           record);

  virtual
  ~TeasMineElec();

  // CALLS

public:

  virtual void establish() { }               // necessary redefinition

  virtual
  const int                                  // duty gol
  constrain
  (const xeona::DomainMode capacityMode);

  virtual
  void
  washup();

  virtual void conclude()  { }               // necessary redefinition

  // INSTANCE DATA

private:

  // tied quantities

  const double&                             d_extractLoBound;
  const double&                             d_extractHiBound;

  const shared_ptr<std::vector<double> >    d_extractions;
  shared_ptr<Socket<CmElectricity> >        d_outElec;

  // local quantities

  shared_ptr<OpsFac0Out1>                   d_ops;     // specialization required

};

//  ==== XEDOC =================================================
//
//  entity.teas-mine-elec-0
//
//      class                                    > TeasMineElec
//
//        flexible (and oversimplified) electricity commodity
//        source -- somewhat similar to a battery
//
//        this entity is provided for model development purposes
//
//      builtin-remark s                         <
//
//      socket-electricity-commodity l           > "cm-electricity-0"
//
//        socket-electricity-commodity defines the shared
//        commodity
//
//      extract-lo-bound [kJ/s] f                > 0.0
//      extract-hi-bound [kJ/s] f                > 1000.0
//
//        extract-hi-bound is required by the simulation whereas
//        nameplate-capacity informs the financial calculations
//
//      extractions [kJ] F                       < 0.0 ..
//
//        financial data
//
//      nameplate-capacity [kJ/s] f              > 10.0
//      duty-specific-cost-financial [$/kJ/s] f  > 100.0
//      size-specific-cost-financial [$/kJ/s] f  > 200.0
//      standing-cost-financial [$/s] f          > 300.0
//
//      variable-costs-financial [$] F           < 0.0 ..
//      fixed-costs-financial [$] F              < 0.0 ..
//
//  ============================================================

#endif // _TEAS03_H_

//  end of file

