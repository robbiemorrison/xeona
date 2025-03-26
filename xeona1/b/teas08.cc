//  XEONA ENERGY SYSTEMS MODELING ENVIRONMENT
//
//  file-create-name : teas08.cc
//  file-create-date : Fri 04-Mar-2011 15:18 UTC
//  file-initiator   : Robbie Morrison <robbie@actrix.co.nz>
//
//  file-role        : concrete technical assets 8 / implementation
//  file-status      : complete
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
//  $URL: file:///home/robbie/svn-root/xeona/futz/trunk/xeona1/b/teas08.cc $
//
//  GENERAL NOTES FOR THIS FILE

//  LOCAL AND SYSTEM INCLUDES

#include "teas08.h"           // companion header for this file (place first)

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

#include <cmath>              // C-style maths, abs(), ceil(), floor(), sqrt()

//  CODE

// ---------------------------------------------------------
//  CLASS           : TeasPipelineGas
// ---------------------------------------------------------

// ---------------------------------------------------------
//  MEMBER FUNCTION : TeasPipelineGas
// ---------------------------------------------------------

TeasPipelineGas::TeasPipelineGas
(const std::string entityId,
 Record&           record) :
  CostRegister(record),
  TechnicalAsset(entityId, record, xeona::e_commitmentModes),
  CostRegisterSRFin(record),
  CostRegisterEmbFin(record),
  d_outHiBound(record.tieSingle<double>("out-hi-bound")),
  d_pipeDiameter(record.tieSingle<double>("pipe-diameter")),
  d_pipelineLength(record.tieSingle<double>("pipeline-length")),
  d_leakageFactor(record.tieSingle<double>("leakage-factor")),
  d_fixedCompressorUsage(record.tieSingle<double>("fixed-compressor-usage")),
  d_absoluteLeakageRate(record.tieSingle<double>("absolute-leakage-rate")),
  d_relativeLeakageRate(record.tieSingle<double>("relative-leakage-rate")),
  d_absoluteCompressorUsage(record.tieSingle<double>("absolute-compressor-usage")),
  d_relativeCompressorUsage(record.tieSingle<double>("relative-compressor-usage")),
  d_inputs(record.tieTimeseries<double>("inputs")),
  d_outputs(record.tieTimeseries<double>("outputs")),
  d_inFluid(Cable<CmOxidGas>::create
            (entityId,                       // me
             record.tieSingle<std::string>("socket-oxidize"),
             record.tieSingle<std::string>("pipe-oxidize-commodity"))),
  d_outFluid(Socket<CmOxidGas>::create
             (entityId,                      // me
              "oxid-1",                      // hard-coded socket label
              record.tieSingle<std::string>("pipe-oxidize-commodity"))),
  d_leakageRate(d_pipelineLength * d_leakageFactor),
  d_compressorUsage(d_pipelineLength * d_fixedCompressorUsage),
  d_massSpecificCo2equiv(0.0),
  d_massSpecificGwp(0.0),
  d_ops()
{
  // initial reporting
  s_logger->repx(logga::xtra, "constructor call", getIdAndKind());

  // integrity checks
  if ( d_leakageFactor < 0.0 )
    {
      s_logger->repx(logga::warn, "negative leakage factor", d_leakageFactor);
    }
  if ( d_pipeDiameter < 0.050 || d_pipeDiameter > 1.500 )
    {
      s_logger->repx(logga::rankJumpy,
                     "pipe diameter normally 0.05 to 1.50",
                     d_pipeDiameter);
    }
  if ( d_pipelineLength > 5.0e06 )
    {
      s_logger->repx(logga::rankJumpy,
                     "pipe length normally under 5000 km",
                     d_pipelineLength);
    }
  if ( d_leakageRate / d_outHiBound > 0.05 )
    {
      s_logger->repx(logga::rankJumpy,
                     "leakage rate looks high",
                     d_leakageRate / d_outHiBound);
    }
  if ( d_compressorUsage / d_outHiBound > 0.10 )
    {
      s_logger->repx(logga::rankJumpy,
                     "compressor usage looks high",
                     d_compressorUsage / d_outHiBound);
    }

  // builtin remark
  d_builtinRemark = "beta";
}

// ---------------------------------------------------------
//  MEMBER FUNCTION : ~TeasPipelineGas
// ---------------------------------------------------------

TeasPipelineGas::~TeasPipelineGas()
{
  s_logger->repx(logga::adhc, "destructor call", getIdAndKind());
}

// ---------------------------------------------------------
//  MEMBER FUNCTION : establish
// ---------------------------------------------------------

void
TeasPipelineGas::establish()
{
  // initial reporting
  s_logger->repx(logga::adhc, "entering member function", "TeasPipelineGas");

  // obtain fuel commodity and some intensive properties
  const shared_ptr<Commodity> com = d_outFluid->getCm();
  const shared_ptr<CmOxidGas> gas = dynamic_pointer_cast<CmOxidGas>(com);
  if ( ! gas )
    {
      s_logger->repx(logga::warn, "dynamic pointer cast failed, target", "CmOxidGas");
    }
  else
    {
      d_massSpecificCo2equiv = gas->getSpecCo2equiv();
      d_massSpecificGwp      = gas->getSpecGwp();
    }

  // reporting
  s_logger->repx(logga::adhc,
                 "specific CO2e of burnt gas [kg/kg]",
                 d_massSpecificCo2equiv);
  s_logger->repx(logga::adhc,
                 "GWP of leaked gas [-]",
                 d_massSpecificGwp);
}

// ---------------------------------------------------------
//  MEMBER FUNCTION : constrain
// ---------------------------------------------------------
//  Description  : technical asset constrain call
//  Role         : characterize current engineering and short-run costs
//  Techniques   : 'boost::tuples::ignore' to ignore tuple ties
//  Status       : complete
// ---------------------------------------------------------

const int                                    // duty gol
TeasPipelineGas::constrain
(const xeona::DomainMode capacityMode)
{
  // initial reporting
  s_logger->repx(logga::adhc, "entering member function", "TeasPipelineGas");

  // create and fill a label object
  const std::string teasId = getIdentifier();
  Label lab(teasId);

  // recreate new operations OSP of the required type
  d_ops.reset(new OpsFixedEffy(d_solver, d_commitmentMode));
  d_ops->loadOspLabel(lab.str());

  // define global cols for internal use
  int  inGol = -1;                           // nonsensical value
  int outGol = -1;                           // nonsensical value

  const double outLoBound      = 0.0;        // zero is reasonable
  const double fixedEfficiency = 1.0;        // all losses are independent of duty
  const double noLoadLoss      = d_leakageRate + d_compressorUsage;

  // upload the engineering
  boost::tie(inGol,                          // input (factor) stream
             outGol)                         // output (product) stream
    = d_ops->uploadEngineering(outLoBound,
                               d_outHiBound,
                               fixedEfficiency,
                               noLoadLoss);

  // work-up the GHG costs -- first is combusted, second is fugitive
  const double  ghgCompressor = d_massSpecificCo2equiv * d_compressorUsage;
  const double  ghgLeakage    = d_massSpecificGwp      * d_leakageRate;
  const CostSet ghgCosts(0.0,                // fin
                         ghgCompressor + ghgLeakage,
                         0.0,                // nox
                         0.0,                // dep
                         0.0);               // luc

  // upload specific costs -- including increment the "shift" term
  d_ops->uploadShortrunCosts(d_dutySpecCosts, inGol);
  d_ops->uploadShortrunCosts(d_sizeSpecCosts * d_nameplateSize
                             + d_standingCosts
                             + ghgCosts);

  // bind global cols to the relevant interfaces
  d_inFluid ->bindOsp(d_solver,  inGol);
  d_outFluid->bindOsp(d_solver, outGol);

  // store duty values
  d_floorDuty   =   outLoBound;
  d_ceilingDuty = d_outHiBound;

  // return the duty gol
  return outGol;

} // function 'TeasPipelineGas::constrain'

// ---------------------------------------------------------
//  MEMBER FUNCTION : washup
// ---------------------------------------------------------
//  Description  : technical asset washup call
//  Role         : recover and record solution
//  Techniques   : (nothing special)
//  Status       : complete
// ---------------------------------------------------------

void
TeasPipelineGas::washup()
{
  // initial reporting
  s_logger->repx(logga::dbug, "entering member function, step", d_step);

  // results recovery
  double input  = -1.0;                      // nonsensical value
  double output = -1.0;                      // nonsensical value
  boost::tie(input,
             output) = d_ops->downloadSolution();

  // store entity state information
  d_inputs ->at(d_step) = input;
  d_outputs->at(d_step) = output;

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
  d_dutyStats(output);                       // functor provided by class 'Block'
  d_sizeStats(d_outHiBound);                 // functor provided by class 'Block'

} // function 'TeasPipelineGas::washup'

// ---------------------------------------------------------
//  MEMBER FUNCTION : conclude
// ---------------------------------------------------------
//  Description  : technical asset conclude call
//  Role         : calculate and record horizon-long metrics
//  Techniques   : (nothing special)
//  Status       : complete
// ---------------------------------------------------------

void
TeasPipelineGas::conclude()
{
  s_logger->repx(logga::adhc, "entering member function", "TeasPipelineGas");

  // get the average carriage rate
  const double averageDuty = d_dutyStats.mean();

  // update metrics
  d_absoluteLeakageRate     = d_leakageRate;
  d_absoluteCompressorUsage = d_compressorUsage;
  if ( averageDuty == 0.0 )                  // div-by-zero protection
    {
      d_relativeLeakageRate     = 0.0;
      d_relativeCompressorUsage = 0.0;
    }
  else
    {
      d_relativeLeakageRate     = d_leakageRate     / averageDuty;
      d_relativeCompressorUsage = d_compressorUsage / averageDuty;
    }

  // additional reporting as appropriate
  // YEEK 46 CODE (set by '--yeek')
  if ( xeona::yeek == 46 || xeona::yeek == 1 || xeona::yeek == 2 )
    {
      const double absoluteTotal = d_absoluteCompressorUsage + d_absoluteLeakageRate;
      const double relativeTotal = d_relativeCompressorUsage + d_relativeLeakageRate;
      std::ostringstream put;
      put << "  gas pipeline reporting"                                   << "\n"
          << "      carriage rate metrics"                                << "\n"
          << "          minimum   [kg/s] : " << d_dutyStats.min()         << "\n"
          << "          average   [kg/s] : " << d_dutyStats.mean()        << "\n"
          << "          maximum   [kg/s] : " << d_dutyStats.max()         << "\n"
          << "          zeros        [-] : " << d_dutyStats.zeros()       << "\n"
          << "      carriage aggregate metrics"                           << "\n"
          << "          sum         [kg] : " << d_dutyStats.sum()         << "\n"
          << "      compressor usage"                                     << "\n"
          << "          absolute  [kg/s] : " << d_absoluteCompressorUsage << "\n"
          << "          relative     [-] : " << d_relativeCompressorUsage << "\n"
          << "      gas leakage"                                          << "\n"
          << "          absolute  [kg/s] : " << d_absoluteLeakageRate     << "\n"
          << "          relative     [-] : " << d_relativeLeakageRate     << "\n"
          << "      loss totals"                                          << "\n"
          << "          absolute  [kg/s] : " << absoluteTotal             << "\n"
          << "          relative     [-] : " << relativeTotal             << "\n";
      s_logger->repx(logga::dbug, "additional reporting follows, yeek", xeona::yeek);
      s_logger->putx(logga::dbug, put);
    }

} // function 'TeasPipelineGas::conclude'

//  end of file

