//  XEONA ENERGY SYSTEMS MODELING ENVIRONMENT
//
//  file-create-name : asop05.cc
//  file-create-date : Sat 22-Oct-2011 19:35 UTC
//  file-initiator   : Robbie Morrison <robbie@actrix.co.nz>
//
//  file-role        : concrete asset operators 5 / implementation
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
//  $URL: file:///home/robbie/svn-root/xeona/futz/trunk/xeona1/b/asop05.cc $
//
//  GENERAL NOTES FOR THIS FILE

//  LOCAL AND SYSTEM INCLUDES

#include "asop05.h"           // companion header for this file (place first)

// the following three headers may be able to be replace by forward declarations
#include "../b/optops1.h"     // operate optimization sub-problems for technical assets 1
#include "../b/commods01.h"   // concrete commodities 1
#include "../b/teas12.h"      // concrete technical assets 12

#include "../a/logger.h"      // standard logging functionality (as required)
#include "../c/smart_ptr.h"   // toggle between Boost and TR1 smart pointers
#include ".././common.h"      // common definitions for project (place last)

#include <string>             // C++ strings
#include <sstream>            // string-streams

#include "../c/recset.h"      // records and fields and also record-sets
#include "../b/teas.h"        // technical asset entity

#include <boost/foreach.hpp>  // BOOST_FOREACH iteration macro

//  CODE

// ---------------------------------------------------------
//  CLASS           : AsopAdminStated
// ---------------------------------------------------------

// ---------------------------------------------------------
//  MEMBER FUNCTION : AsopAdminStated
// ---------------------------------------------------------

AsopAdminStated::AsopAdminStated
(const std::string entityId,
 Record&           record) :
  CostRegister(record),
  AssetOperator(entityId, record, xeona::e_shortrunModes),
  CostRegisterAsop(record),
  d_relativeValveSettings(record.tieTimeseries<double>("relative-valve-settings")),
  d_operationalPenaltys(record.tieTimeseries<double>("operational-penaltys"))
{
  s_logger->repx(logga::xtra, "constructor call", getIdAndKind());
  d_builtinRemark = "beta";
}

// ---------------------------------------------------------
//  MEMBER FUNCTION : ~AsopAdminStated
// ---------------------------------------------------------

AsopAdminStated::~AsopAdminStated()
{
  s_logger->repx(logga::adhc, "destructor call", getIdAndKind());
}

// ---------------------------------------------------------
//  MEMBER FUNCTION : AsopAdminStated::constrain
// ---------------------------------------------------------
//  Description  : constrain call
//  Role         : standard call
//  Techniques   : downcasting cascade
//  Status       : complete
//
//  Design notes
//
//      The use of downcasting here is a hack!  The flow inhibitors should
//      have their own inheritance tree and 'passSettings' should be
//      polymorphic.  It may be useful to clean the design up in due
//      course, but for now the present code is satisfactory.  The main
//      problem is extensibility.
//
// ---------------------------------------------------------

const int                                    // number of technical assets processed
AsopAdminStated::constrain
(const xeona::DomainMode capacityMode)
{
  // initial reporting
  s_logger->repx(logga::adhc, "entering member function", "AsopAdminStated");

  // preamble
  const std::string asopId = getIdentifier();

  // declare some administration counters
  int teasLoops = 0;                         // number of technical assets processed

  // get current parameters
  const double valveSetting       = d_relativeValveSettings->at(d_step);
  const double operationalPenalty = d_operationalPenaltys->at(d_step);

  // step thru technical assets in no required order
  BOOST_FOREACH( shared_ptr<TechnicalAsset> ta, d_technicalAssets )
    {
      // keep count
      ++teasLoops;
      xeona::putxId(ta, "AsopAdminStated::constrain technical asset loop");

      // useful messaging data
      const std::string aoid = getIdAndKind();
      const std::string taid = ta->getIdAndKind();

      // define capture variable
      int opsDutyGol = -1;                   // nonsensical value

      // downcast (cast towards specialization)
#if 0
      shared_ptr<TechnicalAsset> fi = 0;     // CAUTION: [1]
      // [1] this is often bad practice -- it is recommended by the Boost
      // authors (see URL below) that every 'shared_ptr' declaration by
      // accompanied by a 'new':
      //
      //    shared_ptr<MyClass> me(new MyClass(..))
      //
      // this requiredment is omitted in this case, because 'fi' is only
      // used within this block and is not passed outside
      //
      // http://www.boost.org/doc/libs/1_47_0/libs/smart_ptr/shared_ptr.htm#BestPractices
#else
      shared_ptr<TechnicalAsset> fi(static_cast<TechnicalAsset*>(0));  // carries raw null
#endif // 0

      // CAUTION: the single "=" next are correct
      if      ( shared_ptr<TeasFlowInhibitor<CmOxidize> > fi
                = dynamic_pointer_cast<TeasFlowInhibitor<CmOxidize> >(ta) )
        {
          fi->passSettings(valveSetting, operationalPenalty);    // pass down data
          opsDutyGol = fi->constrain(capacityMode);              // constrain the asset
        }
      else if ( shared_ptr<TeasFlowInhibitor<CmCarbonCert> > fi
                = dynamic_pointer_cast<TeasFlowInhibitor<CmCarbonCert> >(ta) )
        {
          fi->passSettings(valveSetting, operationalPenalty);
          opsDutyGol = fi->constrain(capacityMode);
        }
      else if ( shared_ptr<TeasFlowInhibitor<CmCarbonSeq> > fi
                = dynamic_pointer_cast<TeasFlowInhibitor<CmCarbonSeq> >(ta) )
        {
          fi->passSettings(valveSetting, operationalPenalty);
          opsDutyGol = fi->constrain(capacityMode);
        }
      else if ( shared_ptr<TeasFlowInhibitor<CmElectricity> > fi
                = dynamic_pointer_cast<TeasFlowInhibitor<CmElectricity> >(ta) )
        {
          fi->passSettings(valveSetting, operationalPenalty);
          opsDutyGol = fi->constrain(capacityMode);
        }
      else if ( shared_ptr<TeasFlowInhibitor<CmWork> > fi
                = dynamic_pointer_cast<TeasFlowInhibitor<CmWork> >(ta) )
        {
          fi->passSettings(valveSetting, operationalPenalty);
          opsDutyGol = fi->constrain(capacityMode);
        }
      else if ( shared_ptr<TeasFlowInhibitor<CmHeat> > fi
                = dynamic_pointer_cast<TeasFlowInhibitor<CmHeat> >(ta) )
        {
          fi->passSettings(valveSetting, operationalPenalty);
          opsDutyGol = fi->constrain(capacityMode);
        }
      else if ( shared_ptr<TeasFlowInhibitor<CmThermalFluid> > fi
                = dynamic_pointer_cast<TeasFlowInhibitor<CmThermalFluid> >(ta) )
        {
          fi->passSettings(valveSetting, operationalPenalty);
          opsDutyGol = fi->constrain(capacityMode);
        }
      else if ( shared_ptr<TeasFlowInhibitor<CmFunds> > fi
                = dynamic_pointer_cast<TeasFlowInhibitor<CmFunds> >(ta) )
        {
          fi->passSettings(valveSetting, operationalPenalty);
          opsDutyGol = fi->constrain(capacityMode);
        }
      else if ( shared_ptr<TeasFlowInhibitor<CmOxidGas> > fi
                = dynamic_pointer_cast<TeasFlowInhibitor<CmOxidGas> >(ta) )
        {
          fi->passSettings(valveSetting, operationalPenalty);
          opsDutyGol = fi->constrain(capacityMode);
        }
      else if ( shared_ptr<TeasFlowInhibitor<CmOxidNaturalGas> > fi
                = dynamic_pointer_cast<TeasFlowInhibitor<CmOxidNaturalGas> >(ta) )
        {
          fi->passSettings(valveSetting, operationalPenalty);
          opsDutyGol = fi->constrain(capacityMode);
        }
      else if ( shared_ptr<TeasFlowInhibitor<CmOxidBiocoal> > fi
                = dynamic_pointer_cast<TeasFlowInhibitor<CmOxidBiocoal> >(ta) )
        {
          fi->passSettings(valveSetting, operationalPenalty);
          opsDutyGol = fi->constrain(capacityMode);
        }
      else if ( shared_ptr<TeasFlowInhibitor<CmOxidHydrogen> > fi
                = dynamic_pointer_cast<TeasFlowInhibitor<CmOxidHydrogen> >(ta) )
        {
          fi->passSettings(valveSetting, operationalPenalty);
          opsDutyGol = fi->constrain(capacityMode);
        }
      else                                   // bad cast returns zero
        {
          s_logger->repx(logga::warn, "downcast to TeasFlowInhibitor failed", fi);
          std::ostringstream put;
          put << "  administered control operator encountered technical asset that"
              << " is not a flow inhibitor"                             << "\n"
              << "    asset operator class  : " << "AsopAdminStated"    << "\n"
              << "    asset operator id     : " << aoid                 << "\n"
              << "    technical operator id : " << taid                 << "\n"
              << "  the model may need fixing"                          << "\n";
          s_logger->putx(logga::xtra, put);
          s_logger->repx(logga::adhc, "will abandon current loop, count", teasLoops);
          continue;
        }

      // check the 'opsDutyGol'
      if ( opsDutyGol != 0 )                 // should be zero
        {
          s_logger->repx(logga::warn, "return from teas constrain not zero", opsDutyGol);
        }

      // additional reporting as appropriate
      // YEEK 55 CODE (set by '--yeek')
      if ( xeona::yeek == 55 || xeona::yeek == 1 || xeona::yeek == 2 )
        {
          std::ostringstream put;
          put << "  AsopAdminStated constrain report"                             << "\n"
              << "    current step               : " << d_step                    << "\n"
              << "    operator identifier        : " << aoid                      << "\n"
              << "    technical asset identifier : " << taid                      << "\n";
          s_logger->repx(logga::dbug, "additional reporting follows, yeek", xeona::yeek);
          s_logger->putx(logga::dbug, put);
        }

    } // technical assets loop

  // completion reporting
  s_logger->repx(logga::dbug, "leaving member function, teas loops", teasLoops);

  // return
  return teasLoops;

} // function 'AsopAdminStated::constrain'

// ---------------------------------------------------------
//  MEMBER FUNCTION : AsopAdminStated::washup
// ---------------------------------------------------------

void
AsopAdminStated::washup()
{
  // initial reporting
  s_logger->repx(logga::dbug, "entering member function, step", d_step);

  // base class call
  AssetOperator::washup();

  // standing costs processing
  updateStandingCosts(d_step);
}

//  end of file

