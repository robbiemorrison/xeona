//  XEONA ENERGY SYSTEMS MODELING ENVIRONMENT
//
//  file-create-name : teas03.cc
//  file-create-date : Sat 16-May-2009 12:24 UTC
//  file-initiator   : Robbie Morrison <robbie@actrix.co.nz>
//
//  file-role        : concrete technical assets 3 / implementation
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
//  $URL: file:///home/robbie/svn-root/xeona/futz/trunk/xeona1/b/teas03.cc $
//
//  GENERAL NOTES FOR THIS FILE

//  LOCAL AND SYSTEM INCLUDES

#include "teas03.h"           // companion header for this file (place first)

#include "../c/util3.h"       // free functions for floating point comparison
#include "../c/recset.h"      // records and fields and also record-sets
#include "../c/label.h"       // helper class to format solver labels
#include "../c/conex.h"       // create and connect block interfaces
#include "../b/optops1.h"     // operate optimization sub-problems for hard assets 1

#include "../a/logger.h"      // standard logging functionality (as required)
#include "../c/smart_ptr.h"   // toggle between Boost and TR1 smart pointers
#include ".././common.h"      // common definitions for project (place last)

#include <string>             // C++ strings
#include <sstream>            // string-streams

//  CODE

// ---------------------------------------------------------
//  CLASS           : TeasLoadElecTs
// ---------------------------------------------------------

// ---------------------------------------------------------
//  MEMBER FUNCTION : TeasLoadElecTs
// ---------------------------------------------------------

TeasLoadElecTs::TeasLoadElecTs
(const std::string entityId,
 Record&           record) :
  CostRegister(record),
  TechnicalAsset(entityId, record, xeona::e_commitmentModes),
  CostRegisterSRFin(record),
  CostRegisterEmbFin(record),
  d_loads(record.tieTimeseries<double>("loads")),
  d_inElec(Cable<CmElectricity>::create
           (entityId,                        // me
            record.tieSingle<std::string>("socket-electricity"),
            record.tieSingle<std::string>("cable-electricity-commodity"))),
  d_ops()
{
  s_logger->repx(logga::xtra, "constructor call", getIdAndKind());
  d_builtinRemark = "beta / cost output inactive / superseded, try TeasLoad:Elec";
  s_logger->repx(logga::rankJumpy,
                 "superseded entity, try",
                 "TeasLoad:Elec + AsopInelasticTs");
}

// ---------------------------------------------------------
//  FREE FUNCTION   : ~TeasLoadElecTs
// ---------------------------------------------------------

TeasLoadElecTs::~TeasLoadElecTs()
{
  s_logger->repx(logga::adhc, "destructor call", getIdAndKind());
}

// ---------------------------------------------------------
//  MEMBER FUNCTION : constrain
// ---------------------------------------------------------
//  Description  : engineering characterization
//  Role         : host call
//  Techniques   : (nothing special)
//  Status       : complete
// ---------------------------------------------------------

const int                                    // duty gol
TeasLoadElecTs::constrain
(const xeona::DomainMode capacityMode)
{
  // initial reporting
  s_logger->repx(logga::adhc, "entering member function", "TeasLoadElecTs");

  // create and fill a label object
  const std::string teasId = getIdentifier();
  Label lab(teasId);

  // recreate new operations OSP of the required type
  d_ops.reset(new OpsFac1Out0(d_solver, d_commitmentMode));
  d_ops->loadOspLabel(lab.str());

  // define a global col for internal use
  int inGol = -1;                            // nonsensical value

  // get prevailing load
  const double load = d_loads->at(d_step);
  s_logger->repx(logga::xtra, "current load", load);

  // upload the engineering (using single argument inflexible call)
  boost::tie(inGol) = d_ops->uploadEngineering(load);

  // upload specific costs -- including increment the "shift" term
  d_ops->uploadShortrunCosts(d_dutySpecCosts, inGol);
  d_ops->uploadShortrunCosts(d_sizeSpecCosts * d_nameplateSize + d_standingCosts);

  // bind global cols to the relevant interfaces
  d_inElec->bindOsp(d_solver, inGol);

  // store duty values
  d_floorDuty   = load;
  d_ceilingDuty = load;

  // return the duty gol
  return inGol;
}

// ---------------------------------------------------------
//  MEMBER FUNCTION : washup
// ---------------------------------------------------------
//  Description  : washup call
//  Role         : obtain and process results
//  Techniques   : (nothing special)
//  Status       : complete
// ---------------------------------------------------------

void
TeasLoadElecTs::washup()
{
  // initial reporting
  s_logger->repx(logga::dbug, "entering member function, step", d_step);

  // grab requested load
  const double currentLoad      = d_loads->at(d_step);

  // results recovery
  double actualConsumption      = -1.0;      // nonsensical value
  boost::tie(actualConsumption) = d_ops->downloadSolution();

  // preform an integrity check using bounded equality
  if ( ! xeona::almostEqual(actualConsumption, currentLoad, xeona::numic) )
    {
      std::ostringstream oss;
      oss << actualConsumption << " : " << currentLoad;
      s_logger->repx(logga::warn, "consumption : load mismatch", oss.str());
    }

  // accrued costs processing
  CostSet varCosts(0.0);                               // transfer object
  CostSet fixCosts(0.0);                               // transfer object
  d_ops->downloadVarCosts(varCosts);                   // recover variable costs
  d_ops->downloadFixCosts(fixCosts);                   // recover fixed costs
  updateShortrunCosts(d_step, varCosts, fixCosts);     // utilize supplied data
  updateEmbeddedCosts(d_step);                         // utilize in-house calcs

  // store some on-the-fly statistics
  d_dutyStats(actualConsumption);            // functor provided by class 'Block'
  d_sizeStats(currentLoad);                  // functor provided by class 'Block'
}

// ---------------------------------------------------------
//  CLASS           : TeasMineOxid
// ---------------------------------------------------------

// ---------------------------------------------------------
//  MEMBER FUNCTION : TeasMineOxid
// ---------------------------------------------------------

TeasMineOxid::TeasMineOxid
(const std::string entityId,
 Record&           record) :
  CostRegister(record),
  TechnicalAsset(entityId, record, xeona::e_commitmentModes),
  CostRegisterSRFin(record),
  d_extractLoBound(record.tieSingle<double>("extract-lo-bound")),
  d_extractHiBound(record.tieSingle<double>("extract-hi-bound")),
  d_extractions(record.tieTimeseries<double>("extractions")),
  d_outOxid(Socket<CmOxidize>::create
            (entityId,                       // me
             "oxid-1",                       // hard-coded socket label
             record.tieSingle<std::string>("socket-oxide-commodity"))),
  d_ops()
{
  s_logger->repx(logga::xtra, "constructor call", getIdAndKind());
  d_builtinRemark = "beta / superseded, try TeasSource:Oxid";
  s_logger->repx(logga::rankJumpy,
                 "superseded entity, try",
                 "TeasSource:Oxid + AsopInelasticTs");
}

// ---------------------------------------------------------
//  FREE FUNCTION   : ~TeasMineOxid
// ---------------------------------------------------------

TeasMineOxid::~TeasMineOxid()
{
  s_logger->repx(logga::adhc, "destructor call", getIdAndKind());
}

// ---------------------------------------------------------
//  MEMBER FUNCTION : constrain
// ---------------------------------------------------------
//  Description  : engineering characterization
//  Role         : host call
//  Techniques   : (nothing special)
//  Status       : complete
// ---------------------------------------------------------

const int                                    // duty gol
TeasMineOxid::constrain
(const xeona::DomainMode capacityMode)
{
  // initial reporting
  s_logger->repx(logga::adhc, "entering member function", "TeasMineOxid");

  // create and fill a label object
  const std::string teasId = getIdentifier();
  Label lab(teasId);

  // recreate new operations OSP of the required type
  d_ops.reset(new OpsFac0Out1(d_solver, d_commitmentMode));
  d_ops->loadOspLabel(lab.str());

  // define a global col for internal use
  int outGol = -1;                            // nonsensical value

  // upload the engineering
  boost::tie(outGol) = d_ops->uploadEngineering(d_extractLoBound,
                                                d_extractHiBound);

  // upload specific costs -- including increment the "shift" term
  d_ops->uploadShortrunCosts(d_dutySpecCosts, outGol);
  d_ops->uploadShortrunCosts(d_sizeSpecCosts * d_nameplateSize + d_standingCosts);

  // store duty values
  d_floorDuty   = d_extractLoBound;
  d_ceilingDuty = d_extractHiBound;

  // bind global cols to the relevant interfaces
  d_outOxid->bindOsp(d_solver, outGol);

  // return the duty gol
  return outGol;
}

// ---------------------------------------------------------
//  MEMBER FUNCTION : washup
// ---------------------------------------------------------
//  Description  : washup call
//  Role         : obtain and process results
//  Techniques   : (nothing special)
//  Status       : complete
// ---------------------------------------------------------

void
TeasMineOxid::washup()
{
  // initial reporting
  s_logger->repx(logga::dbug, "entering member function, step", d_step);

  // results recovery
  double actualExtraction      = -1.0;      // nonsensical value
  boost::tie(actualExtraction) = d_ops->downloadSolution();

  // accrued costs processing
  CostSet varCosts(0.0);                               // transfer object
  CostSet fixCosts(0.0);                               // transfer object
  d_ops->downloadVarCosts(varCosts);                   // recover variable costs
  d_ops->downloadFixCosts(fixCosts);                   // recover fixed costs
  updateShortrunCosts(d_step, varCosts, fixCosts);     // utilize supplied data
  updateEmbeddedCosts(d_step);                         // utilize in-house calcs

  // ad-hoc reporting
  s_logger->repx(logga::adhc, "variable ghg cost [kg/s]", varCosts.ghg);

  // store some on-the-fly statistics
  d_dutyStats(actualExtraction);             // functor provided by class 'Block'
  d_sizeStats(d_extractHiBound);             // functor provided by class 'Block'
}

// ---------------------------------------------------------
//  CLASS           : TeasMineElec
// ---------------------------------------------------------

// ---------------------------------------------------------
//  MEMBER FUNCTION : TeasMineElec
// ---------------------------------------------------------

TeasMineElec::TeasMineElec
(const std::string entityId,
 Record&           record) :
  CostRegister(record),
  TechnicalAsset(entityId, record, xeona::e_commitmentModes),
  CostRegisterSRFin(record),
  d_extractLoBound(record.tieSingle<double>("extract-lo-bound")),
  d_extractHiBound(record.tieSingle<double>("extract-hi-bound")),
  d_extractions(record.tieTimeseries<double>("extractions")),
  d_outElec(Socket<CmElectricity>::create
            (entityId,                       // me
             "elec-1",                       // hard-coded socket label
             record.tieSingle<std::string>("socket-electricity-commodity"))),
  d_ops()
{
  s_logger->repx(logga::xtra, "constructor call", getIdAndKind());
  d_builtinRemark = "beta / superseded, try TeasSource:Elec";
  s_logger->repx(logga::rankJumpy,
                 "superseded entity, try",
                 "TeasSource:Elec + AsopInelasticTs");
}

// ---------------------------------------------------------
//  FREE FUNCTION   : ~TeasMineElec
// ---------------------------------------------------------

TeasMineElec::~TeasMineElec()
{
  s_logger->repx(logga::adhc, "destructor call", getIdAndKind());
}

// ---------------------------------------------------------
//  MEMBER FUNCTION : constrain
// ---------------------------------------------------------
//  Description  : engineering characterization
//  Role         : host call
//  Techniques   : (nothing special)
//  Status       : complete
// ---------------------------------------------------------

const int                                    // duty gol
TeasMineElec::constrain
(const xeona::DomainMode capacityMode)
{
  // initial reporting
  s_logger->repx(logga::adhc, "entering member function", "TeasMineElec");

  // create and fill a label object
  const std::string teasId = getIdentifier();
  Label lab(teasId);

  // recreate new operations OSP of the required type
  d_ops.reset(new OpsFac0Out1(d_solver, d_commitmentMode));
  d_ops->loadOspLabel(lab.str());

  // define a global col for internal use
  int outGol = -1;                            // nonsensical value

  // upload the engineering
  boost::tie(outGol) = d_ops->uploadEngineering(d_extractLoBound,
                                                d_extractHiBound);

  // upload specific costs -- including increment the "shift" term
  d_ops->uploadShortrunCosts(d_dutySpecCosts, outGol);
  d_ops->uploadShortrunCosts(d_sizeSpecCosts * d_nameplateSize + d_standingCosts);

  // store duty values
  d_floorDuty   = d_extractLoBound;
  d_ceilingDuty = d_extractHiBound;

  // bind global cols to the relevant interfaces
  d_outElec->bindOsp(d_solver, outGol);

  // return the duty gol
  return outGol;
}

// ---------------------------------------------------------
//  MEMBER FUNCTION : washup
// ---------------------------------------------------------
//  Description  : washup call
//  Role         : obtain and process results
//  Techniques   : (nothing special)
//  Status       : complete
// ---------------------------------------------------------

void
TeasMineElec::washup()
{
  // initial reporting
  s_logger->repx(logga::dbug, "entering member function, step", d_step);

  // results recovery
  double actualExtraction      = -1.0;      // nonsensical value
  boost::tie(actualExtraction) = d_ops->downloadSolution();

  // accrued costs processing
  CostSet varCosts(0.0);                               // transfer object
  CostSet fixCosts(0.0);                               // transfer object
  d_ops->downloadVarCosts(varCosts);                   // recover variable costs
  d_ops->downloadFixCosts(fixCosts);                   // recover fixed costs
  updateShortrunCosts(d_step, varCosts, fixCosts);     // utilize supplied data
  updateEmbeddedCosts(d_step);                         // utilize in-house calcs

  // store some on-the-fly statistics
  d_dutyStats(actualExtraction);             // functor provided by class 'Block'
  d_sizeStats(d_extractHiBound);             // functor provided by class 'Block'
}

//  end of file

