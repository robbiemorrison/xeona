//  XEONA ENERGY SYSTEMS MODELING ENVIRONMENT
//
//  file-create-name : teas12.cc
//  file-create-date : Fri 21-Oct-2011 10:31 UTC
//  file-initiator   : Robbie Morrison <robbie@actrix.co.nz>
//
//  file-role        : concrete technical assets 12 / implementation
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
//  $URL: file:///home/robbie/svn-root/xeona/futz/trunk/xeona1/b/teas12.cc $
//
//  GENERAL NOTES FOR THIS FILE

//  LOCAL AND SYSTEM INCLUDES

#include "teas12.h"           // companion header for this file (place first)

#include "../c/recset.h"      // records and fields and also record-sets
#include "../c/label.h"       // helper class to format solver labels
#include "../c/conex.h"       // create and connect block interfaces
#include "../b/optops1.h"     // operate optimization sub-problems for hard assets 1
#include "../b/commods01.h"   // concrete commodities 1
#include "../b/commods.h"     // commodities hierarchy

#include "../a/logger.h"      // standard logging functionality (as required)
#include "../c/smart_ptr.h"   // toggle between Boost and TR1 smart pointers
#include ".././common.h"      // common definitions for project (place last)

#include <string>             // C++ strings
#include <sstream>            // string-streams

//  CODE

// ---------------------------------------------------------
//  CLASS           : TeasValveTs <>
// ---------------------------------------------------------

// ---------------------------------------------------------
//  MEMBER FUNCTION : TeasValveTs <>
// ---------------------------------------------------------

template <typename C>                        // 'C' for commodity
TeasValveTs<C>::TeasValveTs
(const std::string entityId,
 Record&           record) :
  CostRegister(record),
  TechnicalAsset(entityId, record, xeona::e_commitmentModes),    // no restriction
  d_capacity(record.tieSingle<double>("capacity")),
  d_maximumDuty(record.tieSingle<double>("maximum-duty")),
  d_valveSettings(record.tieTimeseries<double>("valve-settings")),
  d_valveCapacitys(record.tieTimeseries<double>("valve-capacitys")),
  d_valveBinds(record.tieTimeseries<bool>("valve-binds")),
  d_commodity(record.tieSingle<std::string>("common-commodity")),
  d_inCommodity(Cable<C>::create
                (entityId,                   // me
                 record.tieSingle<std::string>("socket-1"),
                 d_commodity)),              // common value

  d_outCommodity(Socket<C>::create
           (entityId,                        // me
            "sock-1",                        // hard-coded socket label
            d_commodity)),                   // common value
  d_hiBound(0.0),
  d_ops()
{
  // initial reporting
  s_logger->repx(logga::xtra, "constructor call", getIdAndKind());

  // zero the maximum duty
  d_maximumDuty = 0.0;

  // builtin remark
  d_builtinRemark = "beta";
}

// ---------------------------------------------------------
//  MEMBER FUNCTION : ~TeasValveTs <>
// ---------------------------------------------------------

template <typename C>
TeasValveTs<C>::~TeasValveTs()
{
  // initial reporting
  s_logger->repx(logga::xtra, "destructor call", getIdAndKind());
}

// ---------------------------------------------------------
//  MEMBER FUNCTION : constrain <>
// ---------------------------------------------------------
//  Description  : engineering characterization
//  Role         : host call
//  Techniques   : (nothing special)
//  Status       : complete
//
//  Design notes
//
//      This code sets the hi bound on capacity based on the
//      prevailing valve setting, nothing more.
//
// ---------------------------------------------------------

template <typename C>
const int                                    // duty gol
TeasValveTs<C>::constrain
(const xeona::DomainMode capacityMode)
{
  // additional initial reporting as appropriate
  s_logger->repx(logga::adhc, "entering member function", "TeasValveTs");

  // create and fill a label object
  const std::string teasId = getIdentifier();
  Label lab(teasId);

  // recreate new operations OSP of the required type
  d_ops.reset(new OpsFixedEffy(d_solver, d_commitmentMode));
  d_ops->loadOspLabel(lab.str());

  // define global cols for internal use
  int  inGol = -1;                           // nonsensical value
  int outGol = -1;                           // nonsensical value

  // calculate the lo and hi bounds
  const double valveSetting = d_valveSettings->at(d_step);
  const double loBound      = 0.0;           // a reasonable value
  d_hiBound                 = valveSetting * d_capacity;

  // set the efficiencies
  const double fixedEfficiency = 1.0;        // no conversion losses
  const double noLoadLoss      = 0.0;        // no leaks

  // upload the engineering
  boost::tie(inGol,                          // input (factor) stream
             outGol)                         // output (product) stream
    = d_ops->uploadEngineering(loBound,
                               d_hiBound,
                               fixedEfficiency,
                               noLoadLoss);

  // bind global cols to the relevant interfaces
  d_inCommodity ->bindOsp(d_solver,  inGol);
  d_outCommodity->bindOsp(d_solver, outGol);

  // store duty values
  d_floorDuty   = loBound;
  d_ceilingDuty = d_hiBound;

#if 0 // 0 = return zero to prevent any duty gol coupling with the asset operator,
      // 1 = return the duty gol
  return outGol;
#else
  return 0;
#endif // 0

}
// ---------------------------------------------------------
//  MEMBER FUNCTION : washup <>
// ---------------------------------------------------------
//  Description  : technical asset washup call
//  Role         : recover and record solution
//  Techniques   : (nothing special)
//  Status       : complete
// ---------------------------------------------------------

template <typename C>
void
TeasValveTs<C>::washup()
{
  // initial reporting
  s_logger->repx(logga::dbug, "entering member function, step", d_step);

  // results recovery
  double input  = -1.0;                      // nonsensical value
  double output = -1.0;                      // nonsensical value
  boost::tie(input,
             output) = d_ops->downloadSolution();

  // establish the bind status (this rather indirect method will
  // suffice for now, but is would be better to interrogate the
  // solver directly using the appropriate gol)
  bool bind = false;
  if ( xeona::almostEqual(output, d_hiBound, xeona::numic) )
    {
      bind = true;
    }
  else if ( output > d_hiBound )
    {
      // should never be here
      s_logger->repx(logga::warn, "output exceeds hi bound (bug?)", "");
      bind = true;
    }
  else
    {
      bind = false;
    }

  // store entity state information
  d_valveCapacitys->at(d_step) = d_hiBound;
  d_valveBinds->at(d_step)     = bind;

  // ratchet up the maximum duty as required
  if ( output > d_maximumDuty ) d_maximumDuty = output;

  // store some on-the-fly statistics
  d_dutyStats(output);                       // functor provided by class 'Block'
  d_sizeStats(d_hiBound);                    // functor provided by class 'Block'

} // function 'TeasValveTs::washup'

// ---------------------------------------------------------
//  CLASS           : TeasFlowInhibitor <>
// ---------------------------------------------------------

// STATIC DEFINITIONS - with and without explicit initialization

template <typename C>
const double TeasFlowInhibitor<C>::s_init = std::numeric_limits<double>::quiet_NaN();

// ---------------------------------------------------------
//  MEMBER FUNCTION : TeasFlowInhibitor <>
// ---------------------------------------------------------

template <typename C>                        // 'C' for commodity
TeasFlowInhibitor<C>::TeasFlowInhibitor
(const std::string entityId,
 Record&           record) :
  CostRegister(record),
  TechnicalAsset(entityId, record, xeona::e_commitmentModes),    // no restriction
  d_valveMaxCapacity(record.tieSingle<double>("valve-max-capacity")),
  d_invertValveSettings(record.tieSingle<bool>("invert-valve-settings")),
  d_maximumDuty(record.tieSingle<double>("maximum-duty")),
  d_valveCapacitys(record.tieTimeseries<double>("valve-capacitys")),
  d_valveBinds(record.tieTimeseries<bool>("valve-binds")),
  d_operationalPenaltys(record.tieTimeseries<double>("operational-penaltys")),
  d_commodity(record.tieSingle<std::string>("common-commodity")),
  d_inCommodity(Cable<C>::create
                (entityId,                   // me
                 record.tieSingle<std::string>("socket-1"),
                 d_commodity)),              // common value

  d_outCommodity(Socket<C>::create
           (entityId,                        // me
            "sock-1",                        // hard-coded socket label
            d_commodity)),                   // common value
  d_relativeValveSetting(s_init),
  d_hiBound(s_init),
  d_operationalPenalty(s_init),
  d_dutySpecPenalties(0.0),                  // was 's_init'
  d_ops()
{
  // initial reporting
  s_logger->repx(logga::xtra, "constructor call", getIdAndKind());

  // zero the maximum duty
  d_maximumDuty = 0.0;

  // builtin remark
  d_builtinRemark = "beta";
}

// ---------------------------------------------------------
//  MEMBER FUNCTION : ~TeasFlowInhibitor <>
// ---------------------------------------------------------

template <typename C>
TeasFlowInhibitor<C>::~TeasFlowInhibitor()
{
  // initial reporting
  s_logger->repx(logga::xtra, "destructor call", getIdAndKind());
}

// class 'TeasFlowInhibitor'

// ---------------------------------------------------------
//  MEMBER FUNCTION : TeasFlowInhibitor::passSettings
// ---------------------------------------------------------

template <typename C>
void
TeasFlowInhibitor<C>::passSettings
(const double relativeValveSetting,
 const double operationalPenalty)
{
  d_relativeValveSetting = relativeValveSetting;
  d_operationalPenalty   = operationalPenalty;
}

// ---------------------------------------------------------
//  MEMBER FUNCTION : constrain <>
// ---------------------------------------------------------
//  Description  : engineering characterization
//  Role         : host call
//  Techniques   : (nothing special)
//  Status       : complete
//
//  Design notes
//
//      This code sets the hi bound on capacity based on the
//      prevailing valve setting and the penality if nonzero.
//
//      The same value is applied to all minimized cost types,
//      meaning the modeler is expected to change the value if
//      they change the commitment mode.
//
// ---------------------------------------------------------

template <typename C>
const int                                    // duty gol
TeasFlowInhibitor<C>::constrain
(const xeona::DomainMode capacityMode)
{
  // additional initial reporting as appropriate
  s_logger->repx(logga::adhc, "entering member function", "TeasFlowInhibitor");

  // create and fill a label object
  const std::string teasId = getIdentifier();
  Label lab(teasId);

  // recreate new operations OSP of the required type
  d_ops.reset(new OpsFixedEffy(d_solver, d_commitmentMode));
  d_ops->loadOspLabel(lab.str());

  // define global cols for internal use
  int  inGol = -1;                           // nonsensical value
  int outGol = -1;                           // nonsensical value

  // calculate the lo and hi bounds
  const double loBound = 0.0;                // a reasonable value
  double valveSetting  = 0.0;                // dummy value
  switch ( d_invertValveSettings )
    {
    case false: valveSetting = d_relativeValveSetting;         break;
    case true:  valveSetting = (1.0 - d_relativeValveSetting); break;
    }
  d_hiBound                 = valveSetting * d_valveMaxCapacity;

  // set the efficiencies
  const double fixedEfficiency = 1.0;        // no conversion losses
  const double noLoadLoss      = 0.0;        // no leaks

  // upload the engineering
  boost::tie(inGol,                          // input (factor) stream
             outGol)                         // output (product) stream
    = d_ops->uploadEngineering(loBound,
                               d_hiBound,
                               fixedEfficiency,
                               noLoadLoss);

  // the same value is applied to all penalties
  d_dutySpecPenalties.reset(d_operationalPenalty);

  // upload specific costs
  d_ops->uploadShortrunCosts(d_dutySpecPenalties, outGol);

  // bind global cols to the relevant interfaces
  d_inCommodity ->bindOsp(d_solver,  inGol);
  d_outCommodity->bindOsp(d_solver, outGol);

  // store duty values
  d_floorDuty   = loBound;
  d_ceilingDuty = d_hiBound;

  // housekeeping
#if 0 // 0 = return zero to prevent any duty gol coupling with the asset operator,
      // 1 = return the duty gol
  return outGol;
#else
  return 0;
#endif // 0

}
// ---------------------------------------------------------
//  MEMBER FUNCTION : washup <>
// ---------------------------------------------------------
//  Description  : technical asset washup call
//  Role         : recover and record solution
//  Techniques   : (nothing special)
//  Status       : complete
// ---------------------------------------------------------

template <typename C>
void
TeasFlowInhibitor<C>::washup()
{
  // initial reporting
  s_logger->repx(logga::dbug, "entering member function, step", d_step);

  // results recovery
  double input  = -1.0;                      // nonsensical value
  double output = -1.0;                      // nonsensical value
  boost::tie(input,
             output) = d_ops->downloadSolution();

  // establish the bind status (this rather indirect method will
  // suffice for now, but is would be better to interrogate the
  // solver directly using the appropriate gol)
  bool bind = false;
  if ( xeona::almostEqual(output, d_hiBound, xeona::numic) )
    {
      bind = true;
    }
  else if ( output > d_hiBound )
    {
      // should never be here
      s_logger->repx(logga::warn, "output exceeds hi bound (bug?)", "");
      bind = true;
    }
  else
    {
      bind = false;
    }

  // store entity state information
  d_valveCapacitys->at(d_step) = d_hiBound;
  d_valveBinds->at(d_step)     = bind;

  // ratchet up the maximum duty as required
  if ( output > d_maximumDuty ) d_maximumDuty = output;

  // store some on-the-fly statistics
  d_dutyStats(output);                       // functor provided by class 'Block'
  d_sizeStats(d_hiBound);                    // functor provided by class 'Block'

} // function 'TeasFlowInhibitor::washup'

// ---------------------------------------------------------
//  EXPLICIT TEMPLATE INSTANTIATIONS
// ---------------------------------------------------------

// CAUTION: explicit template instantiations must be placed at
// the very end of the implementation file
//
// For more information, see the excellent documentation in
// 'recset.cc' regarding explicit template instantiations.

// class 'Commodity' typedefs for convenience

typedef CmOxidize      Oxid;
typedef CmCarbonCert   Cert;
typedef CmCarbonSeq    Cseq;
typedef CmElectricity  Elec;
typedef CmWork         Work;
typedef CmHeat         Heat;
typedef CmThermalFluid Thrm;
typedef CmFunds        Fund;

// class 'TeasValveTs'

template TeasValveTs<Oxid>::TeasValveTs(const std::string, Record&);
template TeasValveTs<Cert>::TeasValveTs(const std::string, Record&);
template TeasValveTs<Cseq>::TeasValveTs(const std::string, Record&);
template TeasValveTs<Elec>::TeasValveTs(const std::string, Record&);
template TeasValveTs<Work>::TeasValveTs(const std::string, Record&);
template TeasValveTs<Heat>::TeasValveTs(const std::string, Record&);
template TeasValveTs<Thrm>::TeasValveTs(const std::string, Record&);
template TeasValveTs<Fund>::TeasValveTs(const std::string, Record&);

template TeasValveTs<Oxid>::~TeasValveTs();
template TeasValveTs<Cert>::~TeasValveTs();
template TeasValveTs<Cseq>::~TeasValveTs();
template TeasValveTs<Elec>::~TeasValveTs();
template TeasValveTs<Work>::~TeasValveTs();
template TeasValveTs<Heat>::~TeasValveTs();
template TeasValveTs<Thrm>::~TeasValveTs();
template TeasValveTs<Fund>::~TeasValveTs();

template const int TeasValveTs<Oxid>::constrain(const xeona::DomainMode);
template const int TeasValveTs<Cert>::constrain(const xeona::DomainMode);
template const int TeasValveTs<Cseq>::constrain(const xeona::DomainMode);
template const int TeasValveTs<Elec>::constrain(const xeona::DomainMode);
template const int TeasValveTs<Work>::constrain(const xeona::DomainMode);
template const int TeasValveTs<Heat>::constrain(const xeona::DomainMode);
template const int TeasValveTs<Thrm>::constrain(const xeona::DomainMode);
template const int TeasValveTs<Fund>::constrain(const xeona::DomainMode);

template void TeasValveTs<Oxid>::washup();
template void TeasValveTs<Cert>::washup();
template void TeasValveTs<Cseq>::washup();
template void TeasValveTs<Elec>::washup();
template void TeasValveTs<Work>::washup();
template void TeasValveTs<Heat>::washup();
template void TeasValveTs<Thrm>::washup();
template void TeasValveTs<Fund>::washup();

// class 'TeasFlowInhibitor'

template TeasFlowInhibitor<Oxid>::TeasFlowInhibitor(const std::string, Record&);
template TeasFlowInhibitor<Cert>::TeasFlowInhibitor(const std::string, Record&);
template TeasFlowInhibitor<Cseq>::TeasFlowInhibitor(const std::string, Record&);
template TeasFlowInhibitor<Elec>::TeasFlowInhibitor(const std::string, Record&);
template TeasFlowInhibitor<Work>::TeasFlowInhibitor(const std::string, Record&);
template TeasFlowInhibitor<Heat>::TeasFlowInhibitor(const std::string, Record&);
template TeasFlowInhibitor<Thrm>::TeasFlowInhibitor(const std::string, Record&);
template TeasFlowInhibitor<Fund>::TeasFlowInhibitor(const std::string, Record&);

template TeasFlowInhibitor<Oxid>::~TeasFlowInhibitor();
template TeasFlowInhibitor<Cert>::~TeasFlowInhibitor();
template TeasFlowInhibitor<Cseq>::~TeasFlowInhibitor();
template TeasFlowInhibitor<Elec>::~TeasFlowInhibitor();
template TeasFlowInhibitor<Work>::~TeasFlowInhibitor();
template TeasFlowInhibitor<Heat>::~TeasFlowInhibitor();
template TeasFlowInhibitor<Thrm>::~TeasFlowInhibitor();
template TeasFlowInhibitor<Fund>::~TeasFlowInhibitor();

template void TeasFlowInhibitor<Oxid>::passSettings(const double, const double);
template void TeasFlowInhibitor<Cert>::passSettings(const double, const double);
template void TeasFlowInhibitor<Cseq>::passSettings(const double, const double);
template void TeasFlowInhibitor<Elec>::passSettings(const double, const double);
template void TeasFlowInhibitor<Work>::passSettings(const double, const double);
template void TeasFlowInhibitor<Heat>::passSettings(const double, const double);
template void TeasFlowInhibitor<Thrm>::passSettings(const double, const double);
template void TeasFlowInhibitor<Fund>::passSettings(const double, const double);

template const int TeasFlowInhibitor<Oxid>::constrain(const xeona::DomainMode);
template const int TeasFlowInhibitor<Cert>::constrain(const xeona::DomainMode);
template const int TeasFlowInhibitor<Cseq>::constrain(const xeona::DomainMode);
template const int TeasFlowInhibitor<Elec>::constrain(const xeona::DomainMode);
template const int TeasFlowInhibitor<Work>::constrain(const xeona::DomainMode);
template const int TeasFlowInhibitor<Heat>::constrain(const xeona::DomainMode);
template const int TeasFlowInhibitor<Thrm>::constrain(const xeona::DomainMode);
template const int TeasFlowInhibitor<Fund>::constrain(const xeona::DomainMode);

template void TeasFlowInhibitor<Oxid>::washup();
template void TeasFlowInhibitor<Cert>::washup();
template void TeasFlowInhibitor<Cseq>::washup();
template void TeasFlowInhibitor<Elec>::washup();
template void TeasFlowInhibitor<Work>::washup();
template void TeasFlowInhibitor<Heat>::washup();
template void TeasFlowInhibitor<Thrm>::washup();
template void TeasFlowInhibitor<Fund>::washup();

// ---------------------------------
//  derived instantiations
// ---------------------------------

typedef CmOxidGas        OGas;
typedef CmOxidNaturalGas NatG;
typedef CmOxidBiocoal    BioC;
typedef CmOxidHydrogen   Htwo;

// class 'TeasValveTs'

template TeasValveTs<OGas>::TeasValveTs(const std::string, Record&);
template TeasValveTs<NatG>::TeasValveTs(const std::string, Record&);
template TeasValveTs<BioC>::TeasValveTs(const std::string, Record&);
template TeasValveTs<Htwo>::TeasValveTs(const std::string, Record&);

template TeasValveTs<OGas>::~TeasValveTs();
template TeasValveTs<NatG>::~TeasValveTs();
template TeasValveTs<BioC>::~TeasValveTs();
template TeasValveTs<Htwo>::~TeasValveTs();

template const int TeasValveTs<OGas>::constrain(const xeona::DomainMode);
template const int TeasValveTs<NatG>::constrain(const xeona::DomainMode);
template const int TeasValveTs<BioC>::constrain(const xeona::DomainMode);
template const int TeasValveTs<Htwo>::constrain(const xeona::DomainMode);

template void TeasValveTs<OGas>::washup();
template void TeasValveTs<NatG>::washup();
template void TeasValveTs<BioC>::washup();
template void TeasValveTs<Htwo>::washup();

// class 'TeasFlowInhibitor'

template TeasFlowInhibitor<OGas>::TeasFlowInhibitor(const std::string, Record&);
template TeasFlowInhibitor<NatG>::TeasFlowInhibitor(const std::string, Record&);
template TeasFlowInhibitor<BioC>::TeasFlowInhibitor(const std::string, Record&);
template TeasFlowInhibitor<Htwo>::TeasFlowInhibitor(const std::string, Record&);

template TeasFlowInhibitor<OGas>::~TeasFlowInhibitor();
template TeasFlowInhibitor<NatG>::~TeasFlowInhibitor();
template TeasFlowInhibitor<BioC>::~TeasFlowInhibitor();
template TeasFlowInhibitor<Htwo>::~TeasFlowInhibitor();

template void TeasFlowInhibitor<OGas>::passSettings(const double, const double);
template void TeasFlowInhibitor<NatG>::passSettings(const double, const double);
template void TeasFlowInhibitor<BioC>::passSettings(const double, const double);
template void TeasFlowInhibitor<Htwo>::passSettings(const double, const double);

template const int TeasFlowInhibitor<OGas>::constrain(const xeona::DomainMode);
template const int TeasFlowInhibitor<NatG>::constrain(const xeona::DomainMode);
template const int TeasFlowInhibitor<BioC>::constrain(const xeona::DomainMode);
template const int TeasFlowInhibitor<Htwo>::constrain(const xeona::DomainMode);

template void TeasFlowInhibitor<OGas>::washup();
template void TeasFlowInhibitor<NatG>::washup();
template void TeasFlowInhibitor<BioC>::washup();
template void TeasFlowInhibitor<Htwo>::washup();

//  end of file

