//  XEONA ENERGY SYSTEMS MODELING ENVIRONMENT
//
//  file-create-name : yeek.cc
//  file-create-date : Tue 17-Nov-2009 11:32 UTC
//  file-initiator   : Robbie Morrison <robbie@actrix.co.nz>
//
//  file-role        : yeek (for running extra code) value interpretation / implementation
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
//  $URL: file:///home/robbie/svn-root/xeona/futz/trunk/xeona1/a/yeek.cc $
//
//  GENERAL NOTES FOR THIS FILE

//  LOCAL AND SYSTEM INCLUDES

#include "yeek.h"             // companion header for this file (place first)

#include "../a/logger.h"      // standard logging functionality (as required)
#include "../c/smart_ptr.h"   // toggle between Boost and TR1 smart pointers
#include ".././common.h"      // common definitions for project (place last)

#include <iomanip>            // setw() and family
#include <string>             // C++ strings
#include <sstream>            // string-streams

//  CODE

// ---------------------------------------------------------
//  FREE FUNCTION   : xeona::interpret
// ---------------------------------------------------------
//
//  'CASE' preprocessor macro
//
//      'CASE' is simply a programming convenience.  'CASE' is
//      known as a function-like preprocessor macro, see Lischner
//      (2003 p279) for further discussion.
//
//      Hence the statement:
//
//          CASE( 1, "some text");
//
//      maps to:
//
//          case 1: "some text"; break;
//
//      The first argument 'a' (see hash-definition) must be an
//      integer.  The second argument 'b' must be a string
//      literal and may contain matched or unmatched single (')
//      and escaped double quotes (\").
//
//  Example usage
//
//      // comment
//      // YEEK 99 CODE (set by '--yeek')
//      if ( xeona::yeek == 99 )
//        {
//          s_logger->repx(logga::dbug, "entering yeek code", xeona::yeek);
//          ...
//        }
//
//  CAUTION: retiring of yeek numbers
//
//      Retired yeek numbers should be retired and not recycled.
//      This is because the originating code my be rolled back
//      too.
//
//      In the absence of other considerations, simply comment
//      out the CASE line.
//
//  References
//
//      Lischner, Ray.  2003.  C++ in a nutshell : a language and
//        library reference.  O'Reilly and Associates, Sebastopol,
//        California, USA.  ISBN 0-596-00298-X.
//
// ---------------------------------------------------------

#define CASE(a,b) case a: buf = b; break;    // see above comments

namespace xeona
{
  const std::string
  yeekInterpret
  (const unsigned yeekNo)
  {
    std::string buf = "";
    switch (yeekNo)
      {
// special calls
 CASE(  0, "inactive (provided for convenience)"                                        );
 CASE(  1, "provide maximum reporting plus show first non-feasible OSP (if any)"        );
 CASE(  2, "provide maximum reporting"                                                  );
 CASE(  3, "show step 0 domain-specific OSPs (optimization sub-problems) in web browser");
 CASE(  4, "copy step 0 GLPK transolve problem objects to file"
        " via 'svif::SolverIf::writeInfo'"                                              );
 // normal calls
 CASE(  5, "bidset summary from 'AsopLmpBidStatedTs1' constructor"                      );
 CASE(  6, "bidset summary from 'AsopLmpBidParam' constructor"                          );
 CASE(  7, "report from 'OptimSubProb::downloadSlack' call"                             );
 CASE(  8, "special test for entity throw code, requires 'AsopBasic' entity"            );
 CASE(  9, "report from 'xeona::geometricProgression' call"                             );
 CASE( 10, "report from 'xeona::isTwoContained' call"                                   );
 CASE( 11, "report from 'OpsDcTrans_A::downloadSolution' call"                          );
 CASE( 12, "report from 'CtlLmpBid_A::uploadBidSet' call"                               );
 CASE( 13, "report from 'TeasAc|DcTransmission::constrain/washup' calls"                );
 CASE( 14, "enable block summary report irrespective of --report"                       );
 CASE( 15, "report from 'OpsFac1Out1_A::downloadSolution' call"                         );
 CASE( 16, "report from 'xeona::logRankToGlpkLevel' call"                               );
 CASE( 17, "report from 'AsopLmpBidAdaptive1' member functions"                         );
 CASE( 18, "report from 'GateStatedTariff::establish' call"                             );
 CASE( 19, "report from 'GateCom<C>::hop' call"                                         );
 CASE( 20, "report from '::registerGateways' call"                                      );
 CASE( 21, "report from 'Gateway::recordTransaction' call"                              );
 CASE( 22, "log from 'logga::ptrLogStream' call (normally suppressed)"                  );
 CASE( 23, "abandon early after DFS traversal"                                          );
 CASE( 24, "report from 'uploadEngineering' calls"                                      );
 CASE( 25, "report from 'svif::SolverIf::resetGlpkProb' call"                           );
 CASE( 26, "report from DFS code = 19 + 27"                                             );
 CASE( 27, "report from 'DomainController::lowestNoTildeSel' call"                      );
 CASE( 28, "abandon early during CTA traversal"                                         );
 CASE( 29, "abandon before main loop in 'CtaSimple::captrans' call"                     );
 CASE( 30, "report from 'TeasAc|DcTransmission|TeasLoad*<C>|TeasSource*<C>::constrain'"
       " calls"                                                                         );
 CASE( 31, "undertake full capacitation in CTA (optional and expensive)"                );
 CASE( 32, "report from 'GateStatedTariff<>::washupSelSide' and "
       "'QanTechCapacity' and 'OfrTariffSet_A::downloadSolution' calls"                 );
 CASE( 33, "add \"(some problem)\" to blank output under "
       "'DataIo::getFieldValue' call and also warn"                                     );
 CASE( 34, "report from 'BandedTariffSet::interpretSale' call"                          );
 CASE( 35, "report from 'AsopAdaptiveTs::adapt1' call"                                  );
 CASE( 36, "report from 'TeasBuildingElec::calcBuildingDemand' call, includes summary"  );
 CASE( 37, "gnuplot CCGT characteristic curve from 'TeasCcgt::characterize' call"       );
 CASE( 38, "report from 'GateStatedTariffEFac' constructor, including emissions factor" );
 CASE( 39, "report from all 'BuildingSim' non-entity class calls"                       );
 CASE( 40, "report from 'TeasPvInstallation::constrain' call (with full Sandia data)"   );
 CASE( 41, "report from 'TeasCcgt::characterize' call (CCGT entity)"                    );
 CASE( 42, "report from 'AsopLmpBidHydro1::constrain' call and utility calls"           );
 CASE( 43, "report from 'TeasHydroScheme::constrain' and 'washup' calls"                );
 CASE( 44, "report from 'CxInflowSets::calcStats' call"                                 );
 CASE( 45, "report from 'HydroStatus' constructor call"                                 );
 CASE( 46, "report from 'TeasPipelineGas::conclude' call"                               );
 CASE( 47, "report from 'OpsFuelToCseq_A::uploadEngineering' call"                      );
 CASE( 48, "report from 'TeasCcgtCapture::constrain' call"                              );
 CASE( 49, "report from 'TeasCcsGeological::conclude' call"                             );
 CASE( 50, "report from 'OpsStore_A' calls"                                             );
 CASE( 51, "report from 'BuildingSim::setAmbientConditions' call"                       );
 CASE( 52, "report from 'OptimSubProb::openBnds' calls"                                 );
 CASE( 53, "report from 'LmpNode' 'LmpNodeAc' dtors to summarize nodal prices and such" );
 CASE( 54, "report from 'GateCom<C>::~GateCom' call showing historical transactions"    );
 CASE( 55, "report from 'AsopAdminStated::constrain'"                                   );
 CASE( 56, "exclude 'updatePurchases' and 'updateRevenues' calls from"
       " 'GateStatedTariff<>::washupSelSide' call"                                      );
 CASE( 57, "report from 'TeasCcsGeological::washup' call"                               );
 CASE( 58, "report high precision cost data from 'Overseer::run' call"                  );
 CASE( 59, "report from 'OpsLoadFlow_A::uploadEngineering' and 'downloadSolution' calls");
 CASE( 60, "report maximum voltage angle shift from 'TeasAcTransmission::conclude' call");
      }
    return buf;
  }
} // namespace 'xeona'

// ---------------------------------------------------------
//  FREE FUNCTION   : xeona::yeekSummarize
// ---------------------------------------------------------

namespace xeona
{
  const std::string                          // contains trailing newline
  yeekSummarize
  (const int indent)
  {
    const int lower =  0;                    // lower attempted yeek number
    const int upper = 99;                    // upper attempted yeek number

    std::ostringstream oss;
    std::string tab(indent, ' ');
    for (int i  = lower;
         i     <= upper;
         ++i)
      {
        const std::string interpretation = yeekInterpret(i);
        if ( interpretation.empty() ) continue;
        oss << tab
            << std::setw(2) << std::setfill(' ') << i
            << " = " << interpretation << "\n";
      }
    return oss.str();
  }
} // namespace 'xeona'

//  end of file

