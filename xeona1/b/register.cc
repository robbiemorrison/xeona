//  XEONA ENERGY SYSTEMS MODELING ENVIRONMENT
//
//  file-create-name : registr.cc
//  file-create-date : Wed 20-Jun-2007 13:25 UTC
//  file-initiator   : Robbie Morrison <robbie@actrix.co.nz>
//
//  file-role        : entity sub-class registrations / implementation
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
//  $URL: file:///home/robbie/svn-root/xeona/futz/trunk/xeona1/b/register.cc $

//  ADDING SUB-ENTITY CLASSES
//
//  To add a new sub-entity class, see the guidelines in: SUBENTITY_AUTHORING.txt

//  LOCAL AND SYSTEM INCLUDES

#include "register.h"         // companion header for this file (place first)

#include "../c/factory.h"     // entity factory
#include "../b/commods.h"     // commodities hierarchy
#include "../b/entity.h"      // entity base class

// ~~~~~ step one of two : add corresponding header ~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#ifndef _XUTEST                              // unit testing code

# include "../b/builtins.h"             // time horizon entity
# include "../b/tests.h"                // generic test sub-entities for --inbuilt

# include "../b/overseer.h"             // top-level overseer entity (singleton)
# include "../b/domcon.h"               // domain controller entity

# include "../b/commods.h"              // commodities hierarchy
# include "../b/commods01.h"            // concrete commodities 1

# include "../b/asop01.h"               // concrete asset operators 1
# include "../b/asop02.h"               // concrete asset operators 2
# include "../b/asop03.h"               // concrete asset operators 3
# include "../b/asop04.h"               // concrete asset operators 4
# include "../b/asop05.h"               // concrete asset operators 5
# include "../b/gate01.h"               // concrete gateways 1
# include "../b/junc01.h"               // concrete demand split junctions
# include "../b/junc02.h"               // concrete demand join junctions
# include "../b/junc03.h"               // concrete symmetric demand junctions
# include "../b/junc04.h"               // concrete faked demand and supply junctions
# include "../b/node01.h"               // concrete LMP DC nodes 1
# include "../b/node02.h"               // concrete LMP DC nodes 2
# include "../b/node03.h"               // concrete LMP DC nodes 3
# include "../b/node04.h"               // concrete LMP AC nodes 1
# include "../b/node05.h"               // concrete LMP AC nodes 2
# include "../b/node06.h"               // concrete LMP AC nodes 3
# include "../b/teas01.h"               // concrete technical assets 1
# include "../b/teas02.h"               // concrete technical assets 2
# include "../b/teas03.h"               // concrete technical assets 3
# include "../b/teas04.h"               // concrete technical assets 4
# include "../b/teas05.h"               // concrete technical assets 5
# include "../b/teas06.h"               // concrete technical assets 6
# include "../b/teas07.h"               // concrete technical assets 7
# include "../b/teas08.h"               // concrete technical assets 8
# include "../b/teas09.h"               // concrete technical assets 9
# include "../b/teas10.h"               // concrete technical assets 10
# include "../b/teas11.h"               // concrete technical assets 11
# include "../b/teas12.h"               // concrete technical assets 12
# include "../b/teas13.h"               // concrete technical assets 13
# include "../b/teasdev.h"              // concrete technical assets development

# include "../e/cxamb01.h"              // concrete ambient conditions contexts 1
# include "../e/cxamb02.h"              // concrete ambient conditions contexts 2
# include "../e/cxamb03.h"              // concrete ambient conditions contexts 3
# include "../e/cxecon01.h"             // concrete economic contexts 1
# include "../e/cxpol01.h"              // concrete public policy contexts 1

#endif // _XUTEST

// ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#include "../a/logger.h"      // standard logging functionality (as required)
#include ".././common.h"      // common definitions for project (place last)

#include <cctype>             // C-style char classification, case conversion
#include <string>             // C++ strings
#include <typeinfo>           // run-time type info, NOTE: passive reporting role only
#include <utility>            // STL pair, make_pair()

//  CODE

namespace // unnamed
{
  // ---------------------------------------------------------
  //  FREE FUNCTION   : ::createType <>
  // ---------------------------------------------------------
  //  Description  : free function
  //  Role         : provide 'EntityFactory' create functions
  //  Scope        : this file only
  //  Techniques   : RTTI (run-time type identification), templates, smart pointers
  //  Status       : working
  // ---------------------------------------------------------
  //
  //  Design notes
  //
  //       This function is called from
  //       'EntityFactory::createEntityBind' after being
  //       registered earlier with explicit template
  //       instantiation.
  //
  //  CAUTION: shared pointers
  //
  //      First, a raw pointer must only be used once to create a
  //      (bunch of) shared pointers -- this is done in the call
  //      marked '#' below.
  //
  //      Second, that shared pointers to 'this' cannot be
  //      created in constructors and can only be employed after
  //      the constructor has returned.  Hence the
  //      'processFabricatedPtr' call.
  //
  // ---------------------------------------------------------

  template <typename E>                      // E is a sub-class of Entity
  shared_ptr<Entity>                         // CAUTION: <E> creates a compile-time error
  createType
  (const std::string& entityId,              // unique identifier
   Record&            r)                     // associated record
  {
    static logga::spLogger logger = logga::ptrLogStream();

    std::string compilerName;                // cleaned up type name
    compilerName = typeid(E).name();         // name() returns const char*
    compilerName = xeona::demangle(compilerName);

    logger->repx(logga::dbug, "new sub-entity, typeid(E) yields", compilerName);

    logger->repx(logga::adhc, "about to call new", entityId);
    shared_ptr<E> sp(new E(entityId, r));    // constructor call (# see caution above)

    logger->repx(logga::adhc, "about to process my shared pointer", entityId);
    sp->processFabricatedPtr(sp);            // called with implicit instantiation

    logger->repx(logga::adhc, "about to initialize", entityId);
    sp->factoryInitialize();                 // factory initialization call

    return sp;
  }

} // unnamed namespace

namespace xeona
{

  // ---------------------------------------------------------
  //  FREE FUNCTION   : xeona::registerEntyCreators
  // ---------------------------------------------------------
  //  Description  : support for entity factory
  //  Role         : called by the appropriate function to register Entity creators
  //  Techniques   : function-like preprocessor macro
  //  Status       : working
  //
  //  Design notes
  //
  //      Protection is provided against repeat calls.  And a
  //      warning is also logged.
  //
  //      This function is invoked by the 'xeona::simulate' free
  //      function (which is, in turn, duly invoked by the main
  //      function).
  //
  // ---------------------------------------------------------

  void
  registerEntyCreators()
  {
    // protection against repeat calls
    static bool firstCall = true;
    static logga::spLogger logger = logga::ptrLogStream();
    if ( ! firstCall )
      {
        logger->repx(logga::warn, "duplicate call", "check client code");
        return;
      }
    firstCall = false;
    logger->repx(logga::dbug, "first time", "");

// ~~~~~ step two of two : add corresponding creation call ~~~~~~~~~~~~~~~~~~~~~

    // 'EFIRE' is simply a programming convenience.  'EFIRE' is
    // known as a function-like preprocessor macro, see Lischner
    // (2003 p279) for further discussion.  Note that no
    // additional space is needed, somewhat surprisingly, in the
    // "<b>" definition ('cpp' must contain some smarts!).

#define EFIRE(a,b) EntityFactory::iface()->registerEntity(a, &::createType<b>)

    // note my emacs macro 'robbie-xeona-efire' : f10 S-z .. TestClass

#ifndef _XUTEST                              // for unit testing only

    //  entity prefixes
    //
    //  Asop = asset operator
    //  Cm   = commodity
    //  Cx   = context
    //  Gate = gateway
    //  Teas = technical asset

    //  commodity abbreviations, with quantifying extensity
    //
    //  Cert = CmCarbonCert        kg
    //  Cseq = CmCarbonSeq         kg
    //  Elec = CmElectricity        J
    //  Fund = CmFunds              $ (generic unit of account)
    //  Heat = CmHeat               J
    //  Oxid = CmOxidize           kg
    //  Thrm = CmThermalFluid       J
    //  Work = CmWork               J
    //  Fiss = CmFission           kg (not generally supported)
    //  Land = CmProductiveLand    m2 (not generally supported)

    // test entities

    EFIRE("TestEntity0"                        , TestEntity0                         );
    EFIRE("TestEntity1"                        , TestEntity1                         );
    EFIRE("TestEntity2"                        , TestEntity2                         );

    // high-level entities

    EFIRE("TimeHorizon"                        , TimeHorizon                         );
    EFIRE("Overseer"                           , Overseer                            );
    EFIRE("DomainController"                   , DomainController                    );

    // operators

    EFIRE("AsopAdaptiveTs"                     , AsopAdaptiveTs                      );
    EFIRE("AsopAdminStated"                    , AsopAdminStated                     );
    EFIRE("AsopBasic"                          , AsopBasic                           );
    EFIRE("AsopGrid"                           , AsopGrid                            );
    EFIRE("AsopInelasticParam"                 , AsopInelasticParam                  );
    EFIRE("AsopInelasticTs"                    , AsopInelasticTs                     );
    EFIRE("AsopInternalCosts"                  , AsopInternalCosts                   );
    EFIRE("AsopLmpBidAdaptive1"                , AsopLmpBidAdaptive1                 );
    EFIRE("AsopLmpBidDialog"                   , AsopLmpBidDialog                    );
    EFIRE("AsopLmpBidHydro1"                   , AsopLmpBidHydro1                    );
    EFIRE("AsopLmpBidParam"                    , AsopLmpBidParam                     );
    EFIRE("AsopLmpBidStatedTs1"                , AsopLmpBidStatedTs1                 );
    EFIRE("AsopOccupant"                       , AsopOccupant                        );
    EFIRE("AsopOccupantParam"                  , AsopOccupantParam                   );
    EFIRE("AsopPrescribedOrder"                , AsopPrescribedOrder                 );

    // commodities

    EFIRE("CmCarbonCert"                       , CmCarbonCert                        );
    EFIRE("CmCarbonSeq"                        , CmCarbonSeq                         );
    EFIRE("CmElectricity"                      , CmElectricity                       );
    EFIRE("CmFunds"                            , CmFunds                             );
    EFIRE("CmHeat"                             , CmHeat                              );
    EFIRE("CmOxidize"                          , CmOxidize                           );
    EFIRE("CmThermalFluid"                     , CmThermalFluid                      );
    EFIRE("CmWork"                             , CmWork                              );
    EFIRE("CmFission"                          , CmFission                           );
    EFIRE("CmProductiveLand"                   , CmProductiveLand                    );

    EFIRE("CmOxidBiocoal"                      , CmOxidBiocoal                       );
    EFIRE("CmOxidGas"                          , CmOxidGas                           );
    EFIRE("CmOxidHydrogen"                     , CmOxidHydrogen                      );
    EFIRE("CmOxidNaturalGas"                   , CmOxidNaturalGas                    );

    // gates

    EFIRE("GateStatedTariff:Cert"              , GateStatedTariff<CmCarbonCert>      );
    EFIRE("GateStatedTariff:Cseq"              , GateStatedTariff<CmCarbonSeq>       );
    EFIRE("GateStatedTariff:Elec"              , GateStatedTariff<CmElectricity>     );
    EFIRE("GateStatedTariff:Fund"              , GateStatedTariff<CmFunds>           );
    EFIRE("GateStatedTariff:Heat"              , GateStatedTariff<CmHeat>            );
    EFIRE("GateStatedTariff:Oxid"              , GateStatedTariff<CmOxidize>         );
    EFIRE("GateStatedTariff:Thrm"              , GateStatedTariff<CmThermalFluid>    );
    EFIRE("GateStatedTariff:Work"              , GateStatedTariff<CmWork>            );

    EFIRE("GateStatedTariff:OxidBiocoal"       , GateStatedTariff<CmOxidBiocoal>     );
    EFIRE("GateStatedTariff:OxidGas"           , GateStatedTariff<CmOxidGas>         );
    EFIRE("GateStatedTariff:OxidHydrogen"      , GateStatedTariff<CmOxidHydrogen>    );
    EFIRE("GateStatedTariff:OxidNaturalGas"    , GateStatedTariff<CmOxidNaturalGas>  );

    EFIRE("GateStatedTariffEFac:Cert"          , GateStatedTariffEFac<CmCarbonCert>  );
    EFIRE("GateStatedTariffEFac:Cseq"          , GateStatedTariffEFac<CmCarbonSeq>   );
    EFIRE("GateStatedTariffEFac:Elec"          , GateStatedTariffEFac<CmElectricity> );
    EFIRE("GateStatedTariffEFac:Fund"          , GateStatedTariffEFac<CmFunds>       );
    EFIRE("GateStatedTariffEFac:Heat"          , GateStatedTariffEFac<CmHeat>        );
    EFIRE("GateStatedTariffEFac:Oxid"          , GateStatedTariffEFac<CmOxidize>     );
    EFIRE("GateStatedTariffEFac:Thrm"          , GateStatedTariffEFac<CmThermalFluid>);
    EFIRE("GateStatedTariffEFac:Work"          , GateStatedTariffEFac<CmWork>        );

    EFIRE("GateStatedTariffEFac:OxidBiocoal"   , GateStatedTariffEFac<CmOxidBiocoal> );
    EFIRE("GateStatedTariffEFac:OxidGas"       , GateStatedTariffEFac<CmOxidGas>     );
    EFIRE("GateStatedTariffEFac:OxidHydrogen"  , GateStatedTariffEFac<CmOxidHydrogen>);
    EFIRE("GateStatedTariffEFac:OxidNaturalGas", GateStatedTariffEFac<CmOxidNaturalGas> );

    // technical assets

    EFIRE("TeasSource:Cert"                    , TeasSource<CmCarbonCert>            );
    EFIRE("TeasSource:Cseq"                    , TeasSource<CmCarbonSeq>             );
    EFIRE("TeasSource:Elec"                    , TeasSource<CmElectricity>           );
    EFIRE("TeasSource:Fund"                    , TeasSource<CmFunds>                 );
    EFIRE("TeasSource:Heat"                    , TeasSource<CmHeat>                  );
    EFIRE("TeasSource:Oxid"                    , TeasSource<CmOxidize>               );
    EFIRE("TeasSource:Thrm"                    , TeasSource<CmThermalFluid>          );
    EFIRE("TeasSource:Work"                    , TeasSource<CmWork>                  );

    EFIRE("TeasSource:OxidBiocoal"             , TeasSource<CmOxidBiocoal>           );
    EFIRE("TeasSource:OxidGas"                 , TeasSource<CmOxidGas>               );
    EFIRE("TeasSource:OxidHydrogen"            , TeasSource<CmOxidHydrogen>          );
    EFIRE("TeasSource:OxidNaturalGas"          , TeasSource<CmOxidNaturalGas>        );

    EFIRE("TeasSourceFin:Cert"                 , TeasSourceFin<CmCarbonCert>         );
    EFIRE("TeasSourceFin:Cseq"                 , TeasSourceFin<CmCarbonSeq>          );
    EFIRE("TeasSourceFin:Elec"                 , TeasSourceFin<CmElectricity>        );
    EFIRE("TeasSourceFin:Fund"                 , TeasSourceFin<CmFunds>              );
    EFIRE("TeasSourceFin:Heat"                 , TeasSourceFin<CmHeat>               );
    EFIRE("TeasSourceFin:Oxid"                 , TeasSourceFin<CmOxidize>            );
    EFIRE("TeasSourceFin:Thrm"                 , TeasSourceFin<CmThermalFluid>       );
    EFIRE("TeasSourceFin:Work"                 , TeasSourceFin<CmWork>               );

    EFIRE("TeasSourceFin:OxidBiocoal"          , TeasSourceFin<CmOxidBiocoal>        );
    EFIRE("TeasSourceFin:OxidGas"              , TeasSourceFin<CmOxidGas>            );
    EFIRE("TeasSourceFin:OxidHydrogen"         , TeasSourceFin<CmOxidHydrogen>       );
    EFIRE("TeasSourceFin:OxidNaturalGas"       , TeasSourceFin<CmOxidNaturalGas>     );

    EFIRE("TeasSourceAll:Cert"                 , TeasSourceAll<CmCarbonCert>         );
    EFIRE("TeasSourceAll:Cseq"                 , TeasSourceAll<CmCarbonSeq>          );
    EFIRE("TeasSourceAll:Elec"                 , TeasSourceAll<CmElectricity>        );
    EFIRE("TeasSourceAll:Fund"                 , TeasSourceAll<CmFunds>              );
    EFIRE("TeasSourceAll:Heat"                 , TeasSourceAll<CmHeat>               );
    EFIRE("TeasSourceAll:Oxid"                 , TeasSourceAll<CmOxidize>            );
    EFIRE("TeasSourceAll:Thrm"                 , TeasSourceAll<CmThermalFluid>       );
    EFIRE("TeasSourceAll:Work"                 , TeasSourceAll<CmWork>               );

    EFIRE("TeasSourceAll:OxidBiocoal"          , TeasSourceAll<CmOxidBiocoal>        );
    EFIRE("TeasSourceAll:OxidGas"              , TeasSourceAll<CmOxidGas>            );
    EFIRE("TeasSourceAll:OxidHydrogen"         , TeasSourceAll<CmOxidHydrogen>       );
    EFIRE("TeasSourceAll:OxidNaturalGas"       , TeasSourceAll<CmOxidNaturalGas>     );

    EFIRE("TeasLoad:Cert"                      , TeasLoad<CmCarbonCert>              );
    EFIRE("TeasLoad:Cseq"                      , TeasLoad<CmCarbonSeq>               );
    EFIRE("TeasLoad:Elec"                      , TeasLoad<CmElectricity>             );
    EFIRE("TeasLoad:Fund"                      , TeasLoad<CmFunds>                   );
    EFIRE("TeasLoad:Heat"                      , TeasLoad<CmHeat>                    );
    EFIRE("TeasLoad:Oxid"                      , TeasLoad<CmOxidize>                 );
    EFIRE("TeasLoad:Thrm"                      , TeasLoad<CmThermalFluid>            );
    EFIRE("TeasLoad:Work"                      , TeasLoad<CmWork>                    );

    EFIRE("TeasLoad:OxidBiocoal"               , TeasLoad<CmOxidBiocoal>             );
    EFIRE("TeasLoad:OxidGas"                   , TeasLoad<CmOxidGas>                 );
    EFIRE("TeasLoad:OxidHydrogen"              , TeasLoad<CmOxidHydrogen>            );
    EFIRE("TeasLoad:OxidNaturalGas"            , TeasLoad<CmOxidNaturalGas>          );

    EFIRE("TeasLoadFin:Cert"                   , TeasLoadFin<CmCarbonCert>           );
    EFIRE("TeasLoadFin:Cseq"                   , TeasLoadFin<CmCarbonSeq>            );
    EFIRE("TeasLoadFin:Elec"                   , TeasLoadFin<CmElectricity>          );
    EFIRE("TeasLoadFin:Fund"                   , TeasLoadFin<CmFunds>                );
    EFIRE("TeasLoadFin:Heat"                   , TeasLoadFin<CmHeat>                 );
    EFIRE("TeasLoadFin:Oxid"                   , TeasLoadFin<CmOxidize>              );
    EFIRE("TeasLoadFin:Thrm"                   , TeasLoadFin<CmThermalFluid>         );
    EFIRE("TeasLoadFin:Work"                   , TeasLoadFin<CmWork>                 );

    EFIRE("TeasLoadFin:OxidBiocoal"            , TeasLoadFin<CmOxidBiocoal>          );
    EFIRE("TeasLoadFin:OxidGas"                , TeasLoadFin<CmOxidGas>              );
    EFIRE("TeasLoadFin:OxidHydrogen"           , TeasLoadFin<CmOxidHydrogen>         );
    EFIRE("TeasLoadFin:OxidNaturalGas"         , TeasLoadFin<CmOxidNaturalGas>       );

    EFIRE("TeasLoadAll:Cert"                   , TeasLoadAll<CmCarbonCert>           );
    EFIRE("TeasLoadAll:Cseq"                   , TeasLoadAll<CmCarbonSeq>            );
    EFIRE("TeasLoadAll:Elec"                   , TeasLoadAll<CmElectricity>          );
    EFIRE("TeasLoadAll:Fund"                   , TeasLoadAll<CmFunds>                );
    EFIRE("TeasLoadAll:Heat"                   , TeasLoadAll<CmHeat>                 );
    EFIRE("TeasLoadAll:Oxid"                   , TeasLoadAll<CmOxidize>              );
    EFIRE("TeasLoadAll:Thrm"                   , TeasLoadAll<CmThermalFluid>         );
    EFIRE("TeasLoadAll:Work"                   , TeasLoadAll<CmWork>                 );

    EFIRE("TeasLoadAll:OxidBiocoal"            , TeasLoadAll<CmOxidBiocoal>          );
    EFIRE("TeasLoadAll:OxidGas"                , TeasLoadAll<CmOxidGas>              );
    EFIRE("TeasLoadAll:OxidHydrogen"           , TeasLoadAll<CmOxidHydrogen>         );
    EFIRE("TeasLoadAll:OxidNaturalGas"         , TeasLoadAll<CmOxidNaturalGas>       );

    EFIRE("TeasSimpleStorage:Cert"             , TeasSimpleStorage<CmCarbonCert>     );
    EFIRE("TeasSimpleStorage:Cseq"             , TeasSimpleStorage<CmCarbonSeq>      );
    EFIRE("TeasSimpleStorage:Elec"             , TeasSimpleStorage<CmElectricity>    );
    EFIRE("TeasSimpleStorage:Fund"             , TeasSimpleStorage<CmFunds>          );
    EFIRE("TeasSimpleStorage:Heat"             , TeasSimpleStorage<CmHeat>           );
    EFIRE("TeasSimpleStorage:Oxid"             , TeasSimpleStorage<CmOxidize>        );
    EFIRE("TeasSimpleStorage:Thrm"             , TeasSimpleStorage<CmThermalFluid>   );
    EFIRE("TeasSimpleStorage:Work"             , TeasSimpleStorage<CmWork>           );

    EFIRE("TeasSimpleStorage:OxidBiocoal"      , TeasSimpleStorage<CmOxidBiocoal>    );
    EFIRE("TeasSimpleStorage:OxidGas"          , TeasSimpleStorage<CmOxidGas>        );
    EFIRE("TeasSimpleStorage:OxidHydrogen"     , TeasSimpleStorage<CmOxidHydrogen>   );
    EFIRE("TeasSimpleStorage:OxidNaturalGas"   , TeasSimpleStorage<CmOxidNaturalGas> );

    EFIRE("TeasAcTransmission"                 , TeasAcTransmission                  );
    EFIRE("TeasBuildingElec"                   , TeasBuildingElec                    );
    EFIRE("TeasCcgt"                           , TeasCcgt                            );
    EFIRE("TeasCcgtCapture"                    , TeasCcgtCapture                     );
    EFIRE("TeasCcsGeological"                  , TeasCcsGeological                   );
    EFIRE("TeasDcTransmission"                 , TeasDcTransmission                  );
    EFIRE("TeasHydroScheme"                    , TeasHydroScheme                     );
    EFIRE("TeasLoadElecTs"                     , TeasLoadElecTs                      );
    EFIRE("TeasMineElec"                       , TeasMineElec                        );
    EFIRE("TeasMineOxid"                       , TeasMineOxid                        );
    EFIRE("TeasOxidToElec"                     , TeasOxidToElec                      );
    EFIRE("TeasPipelineGas"                    , TeasPipelineGas                     );
    EFIRE("TeasPvInstallation"                 , TeasPvInstallation                  );
    EFIRE("TeasSubstation"                     , TeasSubstation                      );
    EFIRE("TeasWindfarm"                       , TeasWindfarm                        );

    // valve and flow inhibitor assets

    EFIRE("TeasValveTs:Cert"                  , TeasValveTs<CmCarbonCert>            );
    EFIRE("TeasValveTs:Cseq"                  , TeasValveTs<CmCarbonSeq>             );
    EFIRE("TeasValveTs:Elec"                  , TeasValveTs<CmElectricity>           );
    EFIRE("TeasValveTs:Fund"                  , TeasValveTs<CmFunds>                 );
    EFIRE("TeasValveTs:Heat"                  , TeasValveTs<CmHeat>                  );
    EFIRE("TeasValveTs:Oxid"                  , TeasValveTs<CmOxidize>               );
    EFIRE("TeasValveTs:Thrm"                  , TeasValveTs<CmThermalFluid>          );
    EFIRE("TeasValveTs:Work"                  , TeasValveTs<CmWork>                  );

    EFIRE("TeasValveTs:OxidBiocoal"           , TeasValveTs<CmOxidBiocoal>           );
    EFIRE("TeasValveTs:OxidGas"               , TeasValveTs<CmOxidGas>               );
    EFIRE("TeasValveTs:OxidHydrogen"          , TeasValveTs<CmOxidHydrogen>          );
    EFIRE("TeasValveTs:OxidNaturalGas"        , TeasValveTs<CmOxidNaturalGas>        );

    EFIRE("TeasFlowInhibitor:Cert"            , TeasFlowInhibitor<CmCarbonCert>      );
    EFIRE("TeasFlowInhibitor:Cseq"            , TeasFlowInhibitor<CmCarbonSeq>       );
    EFIRE("TeasFlowInhibitor:Elec"            , TeasFlowInhibitor<CmElectricity>     );
    EFIRE("TeasFlowInhibitor:Fund"            , TeasFlowInhibitor<CmFunds>           );
    EFIRE("TeasFlowInhibitor:Heat"            , TeasFlowInhibitor<CmHeat>            );
    EFIRE("TeasFlowInhibitor:Oxid"            , TeasFlowInhibitor<CmOxidize>         );
    EFIRE("TeasFlowInhibitor:Thrm"            , TeasFlowInhibitor<CmThermalFluid>    );
    EFIRE("TeasFlowInhibitor:Work"            , TeasFlowInhibitor<CmWork>            );

    EFIRE("TeasFlowInhibitor:OxidBiocoal"     , TeasFlowInhibitor<CmOxidBiocoal>     );
    EFIRE("TeasFlowInhibitor:OxidGas"         , TeasFlowInhibitor<CmOxidGas>         );
    EFIRE("TeasFlowInhibitor:OxidHydrogen"    , TeasFlowInhibitor<CmOxidHydrogen>    );
    EFIRE("TeasFlowInhibitor:OxidNaturalGas"  , TeasFlowInhibitor<CmOxidNaturalGas>  );

    // AC nodes (just electricity and no node caps)

    EFIRE("NodeAc0InjXit:Elec"                , NodeAc0InjXit<CmElectricity>         );
    EFIRE("NodeAc1InjA:Elec"                  , NodeAc1InjA<CmElectricity>           );
    EFIRE("NodeAc1InjB:Elec"                  , NodeAc1InjB<CmElectricity>           );
    EFIRE("NodeAc1XitA:Elec"                  , NodeAc1XitA<CmElectricity>           );
    EFIRE("NodeAc1XitB:Elec"                  , NodeAc1XitB<CmElectricity>           );
    EFIRE("NodeAc2Nul:Elec"                   , NodeAc2Nul<CmElectricity>            );
    EFIRE("NodeAc2Inj:Elec"                   , NodeAc2Inj<CmElectricity>            );
    EFIRE("NodeAc2Xit:Elec"                   , NodeAc2Xit<CmElectricity>            );
    EFIRE("NodeAc3NulA:Elec"                  , NodeAc3NulA<CmElectricity>           );
    EFIRE("NodeAc3NulB:Elec"                  , NodeAc3NulB<CmElectricity>           );

    // DC node caps

    EFIRE("TeasCapA:Cert"                     , TeasCapA<CmCarbonCert>               );
    EFIRE("TeasCapA:Cseq"                     , TeasCapA<CmCarbonSeq>                );
    EFIRE("TeasCapA:Elec"                     , TeasCapA<CmElectricity>              );
    EFIRE("TeasCapA:Fund"                     , TeasCapA<CmFunds>                    );
    EFIRE("TeasCapA:Heat"                     , TeasCapA<CmHeat>                     );
    EFIRE("TeasCapA:Oxid"                     , TeasCapA<CmOxidize>                  );
    EFIRE("TeasCapA:Thrm"                     , TeasCapA<CmThermalFluid>             );
    EFIRE("TeasCapA:Work"                     , TeasCapA<CmWork>                     );

    EFIRE("TeasCapB:Cert"                     , TeasCapB<CmCarbonCert>               );
    EFIRE("TeasCapB:Cseq"                     , TeasCapB<CmCarbonSeq>                );
    EFIRE("TeasCapB:Elec"                     , TeasCapB<CmElectricity>              );
    EFIRE("TeasCapB:Fund"                     , TeasCapB<CmFunds>                    );
    EFIRE("TeasCapB:Heat"                     , TeasCapB<CmHeat>                     );
    EFIRE("TeasCapB:Oxid"                     , TeasCapB<CmOxidize>                  );
    EFIRE("TeasCapB:Thrm"                     , TeasCapB<CmThermalFluid>             );
    EFIRE("TeasCapB:Work"                     , TeasCapB<CmWork>                     );

    // DC nodes

    EFIRE("Node0InjXit:Cert"                   , Node0InjXit<CmCarbonCert>           );
    EFIRE("Node0InjXit:Cseq"                   , Node0InjXit<CmCarbonSeq>            );
    EFIRE("Node0InjXit:Elec"                   , Node0InjXit<CmElectricity>          );
    EFIRE("Node0InjXit:Fund"                   , Node0InjXit<CmFunds>                );
    EFIRE("Node0InjXit:Heat"                   , Node0InjXit<CmHeat>                 );
    EFIRE("Node0InjXit:Oxid"                   , Node0InjXit<CmOxidize>              );
    EFIRE("Node0InjXit:Thrm"                   , Node0InjXit<CmThermalFluid>         );
    EFIRE("Node0InjXit:Work"                   , Node0InjXit<CmWork>                 );

    EFIRE("Node1InjA:Cert"                     , Node1InjA<CmCarbonCert>             );
    EFIRE("Node1InjA:Cseq"                     , Node1InjA<CmCarbonSeq>              );
    EFIRE("Node1InjA:Elec"                     , Node1InjA<CmElectricity>            );
    EFIRE("Node1InjA:Fund"                     , Node1InjA<CmFunds>                  );
    EFIRE("Node1InjA:Heat"                     , Node1InjA<CmHeat>                   );
    EFIRE("Node1InjA:Oxid"                     , Node1InjA<CmOxidize>                );
    EFIRE("Node1InjA:Thrm"                     , Node1InjA<CmThermalFluid>           );
    EFIRE("Node1InjA:Work"                     , Node1InjA<CmWork>                   );

    EFIRE("Node1XitA:Cert"                     , Node1XitA<CmCarbonCert>             );
    EFIRE("Node1XitA:Cseq"                     , Node1XitA<CmCarbonSeq>              );
    EFIRE("Node1XitA:Elec"                     , Node1XitA<CmElectricity>            );
    EFIRE("Node1XitA:Fund"                     , Node1XitA<CmFunds>                  );
    EFIRE("Node1XitA:Heat"                     , Node1XitA<CmHeat>                   );
    EFIRE("Node1XitA:Oxid"                     , Node1XitA<CmOxidize>                );
    EFIRE("Node1XitA:Thrm"                     , Node1XitA<CmThermalFluid>           );
    EFIRE("Node1XitA:Work"                     , Node1XitA<CmWork>                   );

    EFIRE("Node1InjB:Cert"                     , Node1InjB<CmCarbonCert>             );
    EFIRE("Node1InjB:Cseq"                     , Node1InjB<CmCarbonSeq>              );
    EFIRE("Node1InjB:Elec"                     , Node1InjB<CmElectricity>            );
    EFIRE("Node1InjB:Fund"                     , Node1InjB<CmFunds>                  );
    EFIRE("Node1InjB:Heat"                     , Node1InjB<CmHeat>                   );
    EFIRE("Node1InjB:Oxid"                     , Node1InjB<CmOxidize>                );
    EFIRE("Node1InjB:Thrm"                     , Node1InjB<CmThermalFluid>           );
    EFIRE("Node1InjB:Work"                     , Node1InjB<CmWork>                   );

    EFIRE("Node1XitB:Cert"                     , Node1XitB<CmCarbonCert>             );
    EFIRE("Node1XitB:Cseq"                     , Node1XitB<CmCarbonSeq>              );
    EFIRE("Node1XitB:Elec"                     , Node1XitB<CmElectricity>            );
    EFIRE("Node1XitB:Fund"                     , Node1XitB<CmFunds>                  );
    EFIRE("Node1XitB:Heat"                     , Node1XitB<CmHeat>                   );
    EFIRE("Node1XitB:Oxid"                     , Node1XitB<CmOxidize>                );
    EFIRE("Node1XitB:Thrm"                     , Node1XitB<CmThermalFluid>           );
    EFIRE("Node1XitB:Work"                     , Node1XitB<CmWork>                   );

    EFIRE("Node2Nul:Cert"                      , Node2Nul<CmCarbonCert>              );
    EFIRE("Node2Nul:Cseq"                      , Node2Nul<CmCarbonSeq>               );
    EFIRE("Node2Nul:Elec"                      , Node2Nul<CmElectricity>             );
    EFIRE("Node2Nul:Fund"                      , Node2Nul<CmFunds>                   );
    EFIRE("Node2Nul:Heat"                      , Node2Nul<CmHeat>                    );
    EFIRE("Node2Nul:Oxid"                      , Node2Nul<CmOxidize>                 );
    EFIRE("Node2Nul:Thrm"                      , Node2Nul<CmThermalFluid>            );
    EFIRE("Node2Nul:Work"                      , Node2Nul<CmWork>                    );

    EFIRE("Node2Inj:Cert"                      , Node2Inj<CmCarbonCert>              );
    EFIRE("Node2Inj:Cseq"                      , Node2Inj<CmCarbonSeq>               );
    EFIRE("Node2Inj:Elec"                      , Node2Inj<CmElectricity>             );
    EFIRE("Node2Inj:Fund"                      , Node2Inj<CmFunds>                   );
    EFIRE("Node2Inj:Heat"                      , Node2Inj<CmHeat>                    );
    EFIRE("Node2Inj:Oxid"                      , Node2Inj<CmOxidize>                 );
    EFIRE("Node2Inj:Thrm"                      , Node2Inj<CmThermalFluid>            );
    EFIRE("Node2Inj:Work"                      , Node2Inj<CmWork>                    );

    EFIRE("Node2Xit:Cert"                      , Node2Xit<CmCarbonCert>              );
    EFIRE("Node2Xit:Cseq"                      , Node2Xit<CmCarbonSeq>               );
    EFIRE("Node2Xit:Elec"                      , Node2Xit<CmElectricity>             );
    EFIRE("Node2Xit:Fund"                      , Node2Xit<CmFunds>                   );
    EFIRE("Node2Xit:Heat"                      , Node2Xit<CmHeat>                    );
    EFIRE("Node2Xit:Oxid"                      , Node2Xit<CmOxidize>                 );
    EFIRE("Node2Xit:Thrm"                      , Node2Xit<CmThermalFluid>            );
    EFIRE("Node2Xit:Work"                      , Node2Xit<CmWork>                    );

    EFIRE("Node3NulA:Cert"                     , Node3NulA<CmCarbonCert>             );
    EFIRE("Node3NulA:Cseq"                     , Node3NulA<CmCarbonSeq>              );
    EFIRE("Node3NulA:Elec"                     , Node3NulA<CmElectricity>            );
    EFIRE("Node3NulA:Fund"                     , Node3NulA<CmFunds>                  );
    EFIRE("Node3NulA:Heat"                     , Node3NulA<CmHeat>                   );
    EFIRE("Node3NulA:Oxid"                     , Node3NulA<CmOxidize>                );
    EFIRE("Node3NulA:Thrm"                     , Node3NulA<CmThermalFluid>           );
    EFIRE("Node3NulA:Work"                     , Node3NulA<CmWork>                   );

    EFIRE("Node3NulB:Cert"                     , Node3NulB<CmCarbonCert>             );
    EFIRE("Node3NulB:Cseq"                     , Node3NulB<CmCarbonSeq>              );
    EFIRE("Node3NulB:Elec"                     , Node3NulB<CmElectricity>            );
    EFIRE("Node3NulB:Fund"                     , Node3NulB<CmFunds>                  );
    EFIRE("Node3NulB:Heat"                     , Node3NulB<CmHeat>                   );
    EFIRE("Node3NulB:Oxid"                     , Node3NulB<CmOxidize>                );
    EFIRE("Node3NulB:Thrm"                     , Node3NulB<CmThermalFluid>           );
    EFIRE("Node3NulB:Work"                     , Node3NulB<CmWork>                   );

    // junctions

    EFIRE("JuncDemand2Split:Cert"              , JuncDemand2Split<CmCarbonCert>      );
    EFIRE("JuncDemand2Split:Cseq"              , JuncDemand2Split<CmCarbonSeq>       );
    EFIRE("JuncDemand2Split:Elec"              , JuncDemand2Split<CmElectricity>     );
    EFIRE("JuncDemand2Split:Fund"              , JuncDemand2Split<CmFunds>           );
    EFIRE("JuncDemand2Split:Heat"              , JuncDemand2Split<CmHeat>            );
    EFIRE("JuncDemand2Split:Oxid"              , JuncDemand2Split<CmOxidize>         );
    EFIRE("JuncDemand2Split:Thrm"              , JuncDemand2Split<CmThermalFluid>    );
    EFIRE("JuncDemand2Split:Work"              , JuncDemand2Split<CmWork>            );

    EFIRE("JuncDemand2Split:OxidBiocoal"       , JuncDemand2Split<CmOxidBiocoal>     );
    EFIRE("JuncDemand2Split:OxidGas"           , JuncDemand2Split<CmOxidGas>         );
    EFIRE("JuncDemand2Split:OxidHydrogen"      , JuncDemand2Split<CmOxidHydrogen>    );
    EFIRE("JuncDemand2Split:OxidNaturalGas"    , JuncDemand2Split<CmOxidNaturalGas>  );

    EFIRE("JuncDemand2Join:Cert"               , JuncDemand2Join<CmCarbonCert>       );
    EFIRE("JuncDemand2Join:Cseq"               , JuncDemand2Join<CmCarbonSeq>        );
    EFIRE("JuncDemand2Join:Elec"               , JuncDemand2Join<CmElectricity>      );
    EFIRE("JuncDemand2Join:Fund"               , JuncDemand2Join<CmFunds>            );
    EFIRE("JuncDemand2Join:Heat"               , JuncDemand2Join<CmHeat>             );
    EFIRE("JuncDemand2Join:Oxid"               , JuncDemand2Join<CmOxidize>          );
    EFIRE("JuncDemand2Join:Thrm"               , JuncDemand2Join<CmThermalFluid>     );
    EFIRE("JuncDemand2Join:Work"               , JuncDemand2Join<CmWork>             );

    EFIRE("JuncDemand2Join:OxidBiocoal"        , JuncDemand2Join<CmOxidBiocoal>      );
    EFIRE("JuncDemand2Join:OxidGas"            , JuncDemand2Join<CmOxidGas>          );
    EFIRE("JuncDemand2Join:OxidHydrogen"       , JuncDemand2Join<CmOxidHydrogen>     );
    EFIRE("JuncDemand2Join:OxidNaturalGas"     , JuncDemand2Join<CmOxidNaturalGas>   );

    EFIRE("JuncDemand2Sym:Cert"                , JuncDemand2Sym<CmCarbonCert>        );
    EFIRE("JuncDemand2Sym:Cseq"                , JuncDemand2Sym<CmCarbonSeq>         );
    EFIRE("JuncDemand2Sym:Elec"                , JuncDemand2Sym<CmElectricity>       );
    EFIRE("JuncDemand2Sym:Fund"                , JuncDemand2Sym<CmFunds>             );
    EFIRE("JuncDemand2Sym:Heat"                , JuncDemand2Sym<CmHeat>              );
    EFIRE("JuncDemand2Sym:Oxid"                , JuncDemand2Sym<CmOxidize>           );
    EFIRE("JuncDemand2Sym:Thrm"                , JuncDemand2Sym<CmThermalFluid>      );
    EFIRE("JuncDemand2Sym:Work"                , JuncDemand2Sym<CmWork>              );

    EFIRE("JuncDemand2Sym:OxidBiocoal"         , JuncDemand2Sym<CmOxidBiocoal>       );
    EFIRE("JuncDemand2Sym:OxidGas"             , JuncDemand2Sym<CmOxidGas>           );
    EFIRE("JuncDemand2Sym:OxidHydrogen"        , JuncDemand2Sym<CmOxidHydrogen>      );
    EFIRE("JuncDemand2Sym:OxidNaturalGas"      , JuncDemand2Sym<CmOxidNaturalGas>    );

    // fake junctions (intended for model development only)

    EFIRE("JuncDemandInvent:Cert"              , JuncDemandInvent<CmCarbonCert>      );
    EFIRE("JuncDemandInvent:Cseq"              , JuncDemandInvent<CmCarbonSeq>       );
    EFIRE("JuncDemandInvent:Elec"              , JuncDemandInvent<CmElectricity>     );
    EFIRE("JuncDemandInvent:Fund"              , JuncDemandInvent<CmFunds>           );
    EFIRE("JuncDemandInvent:Heat"              , JuncDemandInvent<CmHeat>            );
    EFIRE("JuncDemandInvent:Oxid"              , JuncDemandInvent<CmOxidize>         );
    EFIRE("JuncDemandInvent:Thrm"              , JuncDemandInvent<CmThermalFluid>    );
    EFIRE("JuncDemandInvent:Work"              , JuncDemandInvent<CmWork>            );

    EFIRE("JuncDemandInvent:OxidBiocoal"       , JuncDemandInvent<CmOxidBiocoal>     );
    EFIRE("JuncDemandInvent:OxidGas"           , JuncDemandInvent<CmOxidGas>         );
    EFIRE("JuncDemandInvent:OxidHydrogen"      , JuncDemandInvent<CmOxidHydrogen>    );
    EFIRE("JuncDemandInvent:OxidNaturalGas"    , JuncDemandInvent<CmOxidNaturalGas>  );

    EFIRE("JuncDemandService:Cert"             , JuncDemandService<CmCarbonCert>     );
    EFIRE("JuncDemandService:Cseq"             , JuncDemandService<CmCarbonSeq>      );
    EFIRE("JuncDemandService:Elec"             , JuncDemandService<CmElectricity>    );
    EFIRE("JuncDemandService:Fund"             , JuncDemandService<CmFunds>          );
    EFIRE("JuncDemandService:Heat"             , JuncDemandService<CmHeat>           );
    EFIRE("JuncDemandService:Oxid"             , JuncDemandService<CmOxidize>        );
    EFIRE("JuncDemandService:Thrm"             , JuncDemandService<CmThermalFluid>   );
    EFIRE("JuncDemandService:Work"             , JuncDemandService<CmWork>           );

    EFIRE("JuncDemandService:OxidBiocoal"      , JuncDemandService<CmOxidBiocoal>    );
    EFIRE("JuncDemandService:OxidGas"          , JuncDemandService<CmOxidGas>        );
    EFIRE("JuncDemandService:OxidHydrogen"     , JuncDemandService<CmOxidHydrogen>   );
    EFIRE("JuncDemandService:OxidNaturalGas"   , JuncDemandService<CmOxidNaturalGas> );

    // contexts

    EFIRE("CxAmbientAirSim"                    , CxAmbientAirSim                     );
    EFIRE("CxAmbientAirTMY"                    , CxAmbientAirTMY                     );
    EFIRE("CxAmbientAirTs"                     , CxAmbientAirTs                      );
    EFIRE("CxAmbientSolarTMY"                  , CxAmbientSolarTMY                   );
    EFIRE("CxAmbientSolarTs"                   , CxAmbientSolarTs                    );
    EFIRE("CxCommercialFix"                    , CxCommercialFix                     );
    EFIRE("CxInflowSets"                       , CxInflowSets                        );

#endif // _XUTEST

#undef EFIRE                                 // just the identifier is sufficient

// ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

  } // function 'registerEntyCreators'

} // namespace 'xeona'

//  end of file

