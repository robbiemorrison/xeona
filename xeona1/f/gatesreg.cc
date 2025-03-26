//  XEONA ENERGY SYSTEMS MODELING ENVIRONMENT
//
//  file-create-name : gatesreg.cc
//  file-create-date : Sat 20-Feb-2010 19:48 UTC
//  file-initiator   : Robbie Morrison <robbie@actrix.co.nz>
//
//  file-role        : find and register gateways / implementation
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
//  $URL: file:///home/robbie/svn-root/xeona/futz/trunk/xeona1/f/gatesreg.cc $
//
//  GENERAL NOTES FOR THIS FILE

//  LOCAL AND SYSTEM INCLUDES

#include "gatesreg.h"         // companion header for this file (place first)

#include "../b/teas.h"        // technical asset entity
#include "../b/node.h"        // LMP node entity
#include "../b/junc.h"        // demand split/join junction entity
#include "../b/gate.h"        // gateway entity
#include "../b/entity.h"      // entity base class plus lazy linking
#include "../b/domcon.h"      // domain controller entity
#include "../b/asop.h"        // asset operator entity

#include "../a/logger.h"      // standard logging functionality (as required)
#include "../c/smart_ptr.h"   // toggle between Boost and TR1 smart pointers
#include ".././common.h"      // common definitions for project (place last)

#include <map>                // STL associative container
#include <string>             // C++ strings
#include <sstream>            // string-streams

//  CODE

class DomainController;

// ---------------------------------------------------------
//  FREE FUNCTION   : ::padInteger
// ---------------------------------------------------------
//  Description  : zero pad an integer
//  Role         : reporting calls
//  Techniques   : 'std::ostringstream' (rather than 'Boost::Format')
//  Status       : complete
// ---------------------------------------------------------

namespace
{
  std::string
  padInteger
  (const int integer,
   const int pad = 2)                        // note default
  {
    std::ostringstream oss;
    oss << std::setw(pad) << std::setfill('0') << integer;
    return oss.str();
  }
} // unnamed namespace

// ---------------------------------------------------------
//  FREE FUNCTION   : ::registerGateways
// ---------------------------------------------------------
//  Description  : record domain buy-gates / register both domains in gateways
//  Role         : called by 'xeona::registerGate'
//  Techniques   : traversal, 'dynamic_pointer_cast', 'std::map', 'std::map::find'
//  Status       : complete
//
//  Design notes
//
//      The following post-conditions should be fulfilled on exit:
//
//        - domain controllers have their 'd_randomBuygates' vectors filled
//        - gateways have their abutting domain controllers registered
//
//      Achieving these post-conditions requires a somewhat
//      involved matching process, based on the following:
//
//        - the "demander block" is in { gateway, demand
//          junction, lmp node, technical asset } (note that
//          gateways can connect to gateways to implement
//          arbitrage)
//
//        - knowledge of the demander block for each gateway
//          using domain-specific traversals of 'd_rankedSelgates'
//
//        - a constructed map of potential demander blocks and
//          their domain controllers
//
//      The matching process is straightforward, once the above
//      information has been obtained.
//
//      The code in this function is well documented and the
//      description just given should be self-evident.
//
//  Coding issues
//
//      [1] emptying an ostringsteam: 'str(const std::string s)'
//      will set the buffer contents to 's', see Lischner (2003
//      p653) and elsewhere
//
// ---------------------------------------------------------

namespace
{
  bool                                       // 'true' means runs to completion
  registerGateways
  (std::vector<shared_ptr<DomainController> > domains)
  {
    // initial reporting
    static logga::spLogger logger = logga::ptrLogStream();  // bind logger
    logger->repx(logga::adhc, "entering free function", "");

    // string stream for optional yeek report
    std::ostringstream put;

    // set return flag under the presumption of success
    bool returnFlag = true;

    // ---------------------------------
    //  1 - domain controllers
    // ---------------------------------

    // abandon if required
    if ( domains.empty() )
      {
        logger->repx(logga::dbug, "leaving free function early", "no domains");
        return returnFlag;
      }

    // ---------------------------------
    //  2 - prepare some 'std::map's
    // ---------------------------------

    // for convenience
    typedef std::map<shared_ptr<Gateway>, shared_ptr<DomainController> > mapGD_type;
    typedef std::map<shared_ptr<Gateway>, shared_ptr<Block>            > mapGB_type;
    typedef std::map<shared_ptr<Block>  , shared_ptr<DomainController> > mapBD_type;

    // declare some maps
    mapGD_type mapGatesToDomains;            // entire system (hence both plurals)
    mapGB_type mapGatesToBlocks;             // entire system (hence both plurals)
    mapBD_type mapBlocksToDomains;           // entire system (hence both plurals)

    // declare some iterators
    mapGD_type::iterator pos1;
    mapGB_type::iterator pos2;
    mapBD_type::iterator pos3;

    // ---------------------------------
    //  3 - form the mappings
    // ---------------------------------

    // CAUTION: the 'xeona::mapCombine' calls need template
    // instantiations in unit 'c/util1'

    BOOST_FOREACH( shared_ptr<DomainController> domain, domains )
      {
        // capture the ranked selgate data
        mapGD_type gatesToDomain = domain->mapGatesToDomain();
        const bool ret1 = xeona::mapCombine(mapGatesToDomains, gatesToDomain);

        // drill upstream to demand block
        mapGB_type gatesToBlocks = domain->mapGatesToBlocks();
        const bool ret2 = xeona::mapCombine(mapGatesToBlocks, gatesToBlocks);

        // drill down to demand block
        mapBD_type blocksToDomain = domain->mapBlocksToDomain();
        const bool ret3 = xeona::mapCombine(mapBlocksToDomains, blocksToDomain);

        // report any failures
        if ( ! ret1 || ! ret2 || ! ret3 )
          {
            const std::string domainId = domain->getIdAndKind();
            std::ostringstream put;
            put << "  gateway mapping problem"                                    << "\n";
            if ( ! ret1 ) put << "    * map combine failure on ranked selgates"   << "\n";
            if ( ! ret2 ) put << "    * map combine failure on drill upstream"    << "\n";
            if ( ! ret3 ) put << "    * map combine failure on drill downstream"  << "\n";
            put << "  likely cause: "                                             << "\n"
                << "    * multiple entity insertions attempted, indicating"
                << " multiple ownership"                                          << "\n";
            logger->repx(logga::warn, "gateway registration problem", domainId);
            logger->putx(logga::dbug, put);
          }
      }

    // ---------------------------------
    //  4 - process the sel-side
    // ---------------------------------

    int lineCount1 = 0;                      // used for 'put' reporting
    put << "  " << "gateway sel-side processing" << "\n";

    for ( pos1  = mapGatesToDomains.begin();
          pos1 != mapGatesToDomains.end();
          ++pos1)
      {
        // obtain pointers
        shared_ptr<Gateway>          gate   = pos1->first;
        shared_ptr<DomainController> domain = pos1->second;

        // register domain on the sel-side
        shared_ptr<SelSide> sel = dynamic_pointer_cast<SelSide>(gate);
        sel->registerDomain(domain);

        put << "    "
            << "SEL-" << ::padInteger(++lineCount1) << "  "
            << std::setw(30+4+30) << std::left << gate->getIdAndKind() << " >  "
            << domain->getIdAndKind() << "\n";
      }

    // ---------------------------------
    //  5 - process the buy-side
    // ---------------------------------

    int lineCount2 = 0;                      // used for 'put' reporting
    put << "\n"
        << "  " << "gateway buy-side processing" << "\n";

    // loop gateways
    for ( pos2  = mapGatesToBlocks.begin();
          pos2 != mapGatesToBlocks.end();
          ++pos2)
      {
        // obtain pointers
        shared_ptr<Gateway> gate  = pos2->first;
        shared_ptr<Block>   block = pos2->second;

        put << "    "
            << "BUY-" << ::padInteger(++lineCount2) << "  "
            << std::setw(30) << std::left <<  gate->getIdAndKind() << " >  "
            << std::setw(30) << std::left << block->getIdAndKind() << " >  ";

        // attempt to find block
        pos3 = mapBlocksToDomains.find(block);    // based on shared pointer comparison
        if ( pos3 == mapBlocksToDomains.end() )
          {
            logger->repx(logga::warn, "no domain match for block", block->getIdAndKind());
            returnFlag = false;

            put << "(no domain match)" << "\n";
          }
        else
          {
            // obtain pointer
            shared_ptr<DomainController> domain = pos3->second;

            // update 'd_randomBuygates'
            domain->addBuygate(gate);
            // register the domain on the buy-side
            shared_ptr<BuySide> buy = dynamic_pointer_cast<BuySide>(gate);
            buy->registerDomain(domain);

            put << domain->getIdAndKind() << "\n";
          }
      }

    // ---------------------------------
    //  6 - mop-up
    // ---------------------------------

    // additional reporting as appropriate
    // YEEK 20 CODE (set by '--yeek')
    if ( xeona::yeek == 20 || xeona::yeek == 1 || xeona::yeek == 2 )
      {
        logger->repx(logga::dbug, "additional reporting follows, yeek", xeona::yeek);
        logger->putx(logga::dbug, put);
      }

    // completion reporting
    logger->repx(logga::adhc, "leaving free function, return", returnFlag);

    // return
    return returnFlag;
  }

} // unnamed namespace

// ---------------------------------------------------------
//  FREE FUNCTION   : xeona::registerGates
// ---------------------------------------------------------
//  Description  : finalize reciprocal information for gateway and domain controllers
//  Role         : point of contact for registering gateways
//  Techniques   : 'Entity::getFullPopn', '::registerGateways', one pass protection
//  Status       : complete
//
//  Motivation
//
//      One might wonder about the complexity of the code in this
//      unit.  It would clearly be simpler to ask the modeler to
//      define 'd_randomBuygates'.  That however would add to
//      data redundancy and make 'xeona' models more brittle.
//      Hence the approach taken here.
//
//  Design notes
//
//      After confirming that there are full entities, this
//      function calls '::registerGateways.'  See the notes for
//      '::registerGateways' for details.
//
// ---------------------------------------------------------

namespace xeona
{
  bool                                       // 'true' means ran to completion
  registerGates()
  {
    // logger
    static logga::spLogger logger = logga::ptrLogStream();  // bind logger

    // one pass protection
    static unsigned passCount = 0;
    if ( ++passCount > 1 )
      {
        logger->repx(logga::warn, "repeat call, abandoning, pass count", passCount);
        return false;
      }
    else
      {
        logger->repx(logga::adhc, "entering free function, pass count", passCount);
      }

    // check for entities, otherwise return
    if ( Entity::getFullPopn() == 0 )
      {
        logger->repx(logga::info, "no full entities present", "abandoning search");
        return true;                         // considered a success
      }

    // active code
    std::vector<shared_ptr<DomainController> > domains = Entity::retDomains();
    const bool ret = ::registerGateways(domains);

    // return
    logger->repx(logga::dbug, "leaving free function, return", ret);
    return ret;
  }

} // namespace 'xeona'

//  end of file

