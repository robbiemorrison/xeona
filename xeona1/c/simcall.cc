//  XEONA ENERGY SYSTEMS MODELING ENVIRONMENT
//
//  file-create-name : rundev.cc
//  file-create-date : Wed 25-Jul-2007 07:32 UTC
//  file-initiator   : Robbie Morrison <robbie@actrix.co.nz>
//
//  file-role        : principal simulation call / implementation
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
//  $URL: file:///home/robbie/svn-root/xeona/futz/trunk/xeona1/c/simcall.cc $
//
//  GENERAL NOTES FOR THIS FILE
//
//  Principal simulation call.

//  LOCAL AND SYSTEM INCLUDES

#include "simcall.h"          // companion header for this file (place first)

#include "../f/gatesreg.h"    // find and register gateways
#include "../c/datio.h"       // model data io
#include "../c/factory.h"     // entity factory
#include "../c/files.h"       // file path processing
#include "../c/recset.h"      // record set support
#include "../b/entity.h"      // entity base class plus lazy linking
#include "../b/overseer.h"    // top-level overseer entity (singleton)
#include "../c/conex.h"       // create and connect block interfaces
#include "../b/register.h"    // entity sub-class registrations
#include "../c/util1.h"       // free functions which offer general utilities 1
#include "../a/logger.h"      // standard logging functionality (as required)
#include "../c/smart_ptr.h"   // toggle between Boost and TR1 smart pointers
#include ".././common.h"      // common definitions for project (place last)

#include <iostream>           // standard io
#include <sstream>            // string-streams
#include <stdexcept>          // standard exception classes, runtime_error()
#include <string>             // C++ strings
#include <vector>             // STL sequence container

#include <boost/algorithm/string.hpp>   // string recasing, trimming, splitting
#include <boost/filesystem.hpp>         // path objects, iterators, useful operations
#include <boost/foreach.hpp>            // BOOST_FOREACH iteration macro
#include <boost/format.hpp>             // printf style formatting

#include <boost/logic/tribool.hpp>      // three state boolean
#include <boost/logic/tribool_io.hpp>   // CAUTION: essential for std::boolalpha reporting

//  NAMESPACE DECLARATIONS

using boost::logic::tribool;       // three state boolean
using boost::logic::indeterminate; // allows 'indeterminate' and 'indeterminate()'

//  CODE

// ---------------------------------------------------------
//  FREE FUNCTION   : ::interpreteSimKind
// ---------------------------------------------------------

namespace
{
  std::string
  interpretSimKind
  (const xeona::SimKind simKind)
  {
    std::string buffer = "(not overwritten)";
    switch ( simKind )
{
 case xeona::e_notSpecified:   buffer = "not specified";                          break;
 case xeona::e_hollowCall:     buffer = "hollow call";                            break;
 case xeona::e_identifyFile:   buffer = "identify model file - no overwrite";     break;
 case xeona::e_parseModel:     buffer = "parse model file - shallow overwrite";   break;
 case xeona::e_invokeFactory:  buffer = "construct objects - deep overwrite";     break;
 case xeona::e_linkAndConnect: buffer = "link and connect - deep overwrite";      break;
 case xeona::e_firstStepRun:   buffer = "first simulation step - deep overwrite"; break;
 case xeona::e_fullRun:        buffer = "full simulation - deep overwrite";       break;
 case xeona::e_yearRun:        buffer = "extended simulation - deep overwrite";   break;
 case xeona::e_resampleRun:    buffer = "resampled simulation - deep overwrite";  break;
 default: std::clog << "** coding error 01 in source file " << __FILE__ << std::endl;
}
    buffer += " ";
    buffer += boost::str(boost::format("(%d)") % simKind);
    return buffer;
  }
} // unnamed namespace

// ---------------------------------------------------------
//  FREE FUNCTION   : ::interpreteSimRet
// ---------------------------------------------------------

namespace
{
  std::string
  interpretSimRet
  (const xeona::SimRet simRet)
  {
    std::string buffer = "(not overwritten)";
    switch ( simRet )
      {
      case xeona::e_statusNotKnown:   buffer = "status not known";  break;
      case xeona::e_success:          buffer = "success";           break;
      case xeona::e_modelFileFault:   buffer = "model file fault";  break;
      case xeona::e_infeasibility:    buffer = "hit infeasibility"; break;
      case xeona::e_errantSimulation: buffer = "errant simulation"; break;
      case xeona::e_testCodeUsed:     buffer = "test code used";    break;
      case xeona::e_other:            buffer = "other";             break;
      default: std::clog << "** coding error 02 in source file " << __FILE__ << std::endl;
      }
    buffer += " ";
    buffer += boost::str(boost::format("(%d)") % simRet);
    return buffer;
  }
} // unnamed namespace

// ---------------------------------------------------------
//  FREE FUNCTION   : xeona::simulate
// ---------------------------------------------------------
//  Description  : principal simulation call
//  Role         : runs the simulation to the required 'SimKind' "depth"
//  Caller       : 'main'
//  Status       : complete
//
//  Design notes
//
//      The services provided by the code within this unit are
//      mostly procedural, which means a "function-driven
//      programming paradigm" is the most appropriate (Alhir
//      1998, pp43-45).  Hence this free function named
//      'xeona::simulate'.
//
//      'xeona::simulate' also relies for support on local
//      (prefixed "::" in unnamed namespace) free functions,
//      because there seemed little reason to code and deploy
//      stateless singleton objects solely for their member
//      function calls.
//
//      This function also undertakes some integrity checks and
//      responds accordingly, usually by downgrading the
//      simulation kind.  Unless, that is, 'xeona::nopro' has
//      been set to 'true' by the '--krazy' command-line option.
//
//      Logger calls are best made from the most appropriate
//      location -- hence there are quite a number 'repx' calls
//      within this function definition.
//
//  References
//
//      Alhir, Sinan Si.  1998.  UML in a nutshell : a desktop
//        quick reference.  O'Reilly and Associates, Sebastopol,
//        California, USA.  ISBN: 1-56592-448-7.
//
// ---------------------------------------------------------

xeona::SimRet                                // simulation return enum
simulate
(const std::string&   modelName,             // from command-line or 'main' func default
 const xeona::SimKind simKind)               // from command-line or 'main' func default
{
  // ---------------------------------
  //  set up logging
  // ---------------------------------

  static logga::spLogger logger = logga::ptrLogStream();  // bind logger
  std::ostringstream put;                                 // used for 'logger->putx' calls

  // ---------------------------------
  //  declare some local variables
  // ---------------------------------

  xeona::SimRet  simret;                     // return value for this function
  xeona::SimKind simkind;                     // modifiable version of 'simKind'

  simret  = xeona::e_success;                // set to success, may be overwritten later
  simkind = simKind;                         // assign original value

  // ---------------------------------
  //  report some initial information
  // ---------------------------------

  logger->repx(logga::dbug, "entering free function", "");
  logger->repx(logga::dbug, "simulation model name", modelName);
  logger->repx(logga::dbug, "system timer resolution", xeona::getSystemTimerResol());
  logger->repx(logga::dbug, "write float format string", xeona::writeFloatFmtStr);

  // close-to-zero setting and behavior
  std::ostringstream numZero;
  numZero << boost::format("%.12f") % xeona::numericalZero;
  logger->repx(logga::dbug, "numerical zero to 12 places", numZero.str());
  switch ( xeona::zero )
    {
    case true:
      logger->repx(logga::dbug, "close-to-zero rounding on",  xeona::zero);
      break;
    case false:                              // note 'logga::info'
      logger->repx(logga::info, "close-to-zero rounding off", xeona::zero);
      break;
    }

  // structural coefficients span tolerance
  std::ostringstream coeffSpanTol;
  coeffSpanTol << boost::format("%.2e") % xeona::coeffSpanTol;
  logger->repx(logga::info, "constraint coeffs span tolerance", coeffSpanTol.str());

  std::string msg = "requested simulation kind";
  switch ( simKind )                         // note the different reporting levels
    {
    case xeona::e_notSpecified:   logger->repx(logga::warn, msg, "notSpecified");   break;
    case xeona::e_hollowCall:     logger->repx(logga::dbug, msg, "hollowCall");     break;
    case xeona::e_identifyFile:   logger->repx(logga::dbug, msg, "identifyFile");   break;
    case xeona::e_parseModel:     logger->repx(logga::dbug, msg, "parseModel");     break;
    case xeona::e_invokeFactory:  logger->repx(logga::dbug, msg, "invokeFactory");  break;
    case xeona::e_linkAndConnect: logger->repx(logga::dbug, msg, "linkAndConnect"); break;
    case xeona::e_firstStepRun:   logger->repx(logga::dbug, msg, "firstStepRun");   break;
    case xeona::e_fullRun:        logger->repx(logga::dbug, msg, "fullRun");        break;
    case xeona::e_yearRun:        logger->repx(logga::dbug, msg, "extendedRun");    break;
    case xeona::e_resampleRun:    logger->repx(logga::dbug, msg, "resampledRun");   break;
    default: std::clog << "** coding error 03 in source file " << __FILE__ << std::endl;
    }

  // ---------------------------------
  //  * identify the XEM file              // the asterisk means a simkind switch
  // ---------------------------------

  if ( simkind >= xeona::e_identifyFile )  // enum given in "simcall.h"
    {
      logger->repx(logga::dbug, "executing nested code for", "identifyFile");

      // ---------------------------------
      //  establish model path
      // ---------------------------------

      // try to establish the model path as a regular file
      // after removing the pre-defined model file extension
      // 'xeona::modelExt' if present -- in particular, note
      // the call to 'xeona::establishModelFile' which confirms
      // the existence of the file or returns an empty path on
      // failure

      std::string modelStub(modelName);      // construct a local copy
      if ( boost::ends_with(modelStub, xeona::modelExt) )
        {
          boost::erase_tail(modelStub, xeona::modelExt.length());
        }
      boost::filesystem::path model = xeona::establishModelFile(modelStub);

      if ( model.empty() )                   // no path established
        {
          logger->repx(logga::warn, "empty model path", model.empty());
          logger->repx(logga::dbug, "simulation downgraded to", "e_identifyFile");
          simkind = xeona::e_identifyFile;   // downgrade simulation kind
          simret  = xeona::e_modelFileFault;
        }
      else
        {
          logger->repx(logga::dbug, "model path (complete)", model);  // streamable
          logger->repx(logga::dbug, "model path (leaf)", model.filename());
        }

      // ---------------------------------
      //  * parse model
      // ---------------------------------

      if ( simkind >= xeona::e_parseModel )
        {
          logger->repx(logga::dbug, "executing nested code for", "parseModel");

          // ---------------------------------
          //  make record set and io object
          // ---------------------------------

          RecordSet recset;                  // create empty record set
          DataIo dataIo(recset);             // create data input/output admin object

          // ---------------------------------
          //  read and process model
          // ---------------------------------

          put << "   ++ about to read in model" << "\n";
          logger->addSmartBlank(logga::dbug);
          logger->putx(logga::dbug, put);
          logger->addSmartBlank(logga::dbug);

          // read in model, 'd_rawStr' is loaded but not processed
          logger->repx(logga::dbug, "about to call readModel", "");
          dataIo.readModel(model);

          put << "   ++ about to undertake post-read processing" << "\n";
          logger->addSmartBlank(logga::dbug);
          logger->putx(logga::dbug, put);
          logger->addSmartBlank(logga::dbug);

          // confirm that model format and svn revision align
          dataIo.confirmModelSvnAlign();

          // the following sneaky modifications mean that the XEM
          // file will reflect the actual values used under the
          // mode options 8 and 9 and not those originally given
          // -- this automatically alleviates problems with
          // downstream data processing
          //
          // CAUTION: cannot reset horizon steps to unity here
          // when mode option 6

          // sneakily modify the TimeHorizon data if running under "--mode 8"
          if ( simkind == xeona::e_yearRun )
            {
              logger->repx(logga::dbug, "about to reset hours", xeona::mode8hours);
              recset.modifyHours(xeona::mode8hours);
            }

          // sneakily modify the TimeHorizon data if running under "--mode 9"
          if ( simkind == xeona::e_resampleRun )
            {
              logger->repx(logga::dbug, "about to reset seconds", xeona::mode9seconds);
              logger->repx(logga::dbug, "about to reset hours", xeona::mode8hours);
              recset.modifySecondsHours(xeona::mode9seconds, xeona::mode8hours);
            }

          // locate horizon information -- necessary for the
          // 'processModel' call and ALSO for the production of
          // the TimeHorizon entity 'time-horizon'
          dataIo.snoopHorizonDetails();      // updates 'Entity::s_horizonSteps' and such

          const int horizonSteps = Entity::getHorizonSteps();
          put << "  horizon steps : " << horizonSteps << "\n";
          logger->putx(logga::xtra, put);
          logger->addSmartBlank(logga::xtra);

          // process 'd_rawStr' to yield 'd_splitStr' string vectors
          logger->repx(logga::dbug, "about to call processModel", "");
          dataIo.processModel();

          // defensive programming if horizon steps less than two
          if ( ! horizonSteps >= 2 )
            {
              logger->repx(logga::warn, "time-horizon.steps not 2 or more", horizonSteps);
              if ( xeona::nopro )
                {
                  logger->repx(logga::info, "defensive coding omitted", "");
                  simret  = xeona::e_modelFileFault;
                }
              else
                {
                  logger->repx(logga::dbug, "simulation downgraded to", "e_parseModel");
                  simkind = xeona::e_parseModel;  // downgrade simulation kind
                  simret  = xeona::e_modelFileFault;
                }
            }

          // ---------------------------------
          //  * invoke entity factory
          // ---------------------------------

          boost::posix_time::ptime           // start timing
            tic(boost::posix_time::microsec_clock::universal_time());

          if ( simkind >= xeona::e_invokeFactory )
            {
              logger->repx(logga::dbug, "executing nested code for", "invokeFactory");

              // ---------------------------------
              //  register creators en-masse
              // ---------------------------------

              put << "   ++ "
                  << "about to register entity creators"
                  << "\n";
              logger->addSmartBlank(logga::dbug);
              logger->putx(logga::dbug, put);
              logger->addSmartBlank(logga::dbug);

              logger->repx(logga::dbug, "about to call registerEntyCreators", "");
              xeona::registerEntyCreators(); // one-off call, protected by a warning

              // ---------------------------------
              //  set mandatory entities
              // ---------------------------------

              // the functionality here is duplicated later on
              // (search on "mandatories") when a list of
              // mandatory identifiers is confirmed -- the code
              // here is considerably more complicated and
              // should probably be ripped out at some point

              put << "   ++ about to set the mandatory entities" << "\n";
              logger->addSmartBlank(logga::dbug);
              logger->putx(logga::dbug, put);
              logger->addSmartBlank(logga::dbug);

              // mandatory entity types listed here
              Entity::addWant("TimeHorizon");
              Entity::addWant("Overseer");

# if 0 // 0 = skip code (safe option), 1 = induce fake complaint if not a release build
              if ( xeona::releaseStatus == false )
                {
                  Entity::addWant("FakeForTesting");
                }
# endif // 0

              // ---------------------------------
              //  formulate the production list
              // ---------------------------------

              put << "   ++ about to formulate the production list" << "\n";
              logger->addSmartBlank(logga::dbug);
              logger->putx(logga::dbug, put);
              logger->addSmartBlank(logga::dbug);

              // process entity instances

              typedef std::vector<shared_ptr<Record> > RexSet;   // note also 'RecordSet'

              const RexSet entities = dataIo.getSubset(xeona::e_entity);   // "entity."

              RexSet productionList;
              xeona::tailCombine(productionList, entities);

              put << "    production breakdown"                     << "\n";
              put << "    entity.*  entities : " << entities.size() << "\n";
              logger->putx(logga::xtra, put);
              logger->addSmartBlank(logga::xtra);

              // ---------------------------------
              //  undertake object production
              // ---------------------------------

              put << "   ++ about to start production" << "\n";
              logger->addSmartBlank(logga::dbug);
              logger->putx(logga::dbug, put);
              logger->addSmartBlank(logga::dbug);

              // the following vector should also remain
              // identical to 'Entity::s_censusFull'

              std::vector<shared_ptr<Entity> > xemFull;     // xem-sourced register

              const std::string tag = "FAC-";     // lead tag

              unsigned facLoop      = 0;
              unsigned xemFullCount = 0;

              BOOST_FOREACH( shared_ptr<Record> r, productionList )
                {
                  const std::string id       = r->getIdentifier(); // uniqueness is tested
                  const tribool enabled      = r->getEnabled();  // 'non-true' is disabled
                  const std::string eTypeStr = r->locateClass(); // empty means failure

                  if ( ! enabled )
                    {
                      logger->repx(logga::info, "skipping disabled record", id);
                      continue;
                    }

                  const std::string separator = "  ";  // separator
                  std::ostringstream put;              // new for each loop
                  std::ostringstream oss;              // new for each loop
                  oss << "    ";
                  oss << tag << std::setw(2) << std::setfill('0') << ++facLoop << "  ";
                  oss << eTypeStr
                      << separator
                      << id
                      << "\n";
                  logger->addSmartBlank(logga::xtra);
                  logger->putx(logga::xtra, oss);
                  logger->addSmartBlank(logga::xtra);

                  try
                    {
  // main production call
  xemFull.push_back(EntityFactory::iface()->createEntityBind(eTypeStr, id, *r));

                      // some housekeeping
                      ++xemFullCount;
                      if ( xemFull.size() != xemFullCount )      // defensive programming
                        {
                          logger->repx(logga::warn, "xemFull.size != xemFullCount", "");
                        }
                    }
                  catch( const std::out_of_range& e )
                    {
  logger->repx(logga::warn, "'std::out_of_range' caught", "details below");
  const std::string what = e.what();         // CAUTION: cannot string == directly
  put   << "    event : std::out_of_range exception caught"                << "\n";
  put   << "    what  : " << what                                          << "\n";
  if ( what == "vector::_M_range_check" )    // also occurs with NDEBUG set
    {
      put << "    check : faulty 'std::vector<>::at' call within an entity?" << "\n";
    }
  put   << "    next  : will rethrow, application should exit cleanly"     << "\n";
  logger->putx(logga::warn, put);
  throw;                                     // rethrow
                    }
                  catch( const std::exception& e )
                    {
                      logger->repx(logga::warn, "std::exception caught, rethrown", "");
                      throw;                 // rethrow
                    }
                } // boost_foreach production list loop

              // ---------------------------------
              //  set deep write requirement
              // ---------------------------------

              // inform the data io object that the model is
              // now bound -- this will provoke a "deep"
              // (rather than "shallow") write later on

              logger->repx(logga::xtra, "about to call noteModelIsBound", "");
              dataIo.noteModelIsBound(); // 'DataIo::d_records' now 'RecordSet::e_bound'

              // ---------------------------------
              //  conduct post-production checks
              // ---------------------------------

              put << "   ++ about to conduct post-production checks" << "\n";
              logger->addSmartBlank(logga::dbug);
              logger->putx(logga::dbug, put);
              logger->addSmartBlank(logga::dbug);

              put << "    horizon steps (as used)    : "
                  << Entity::getHorizonSteps()
                  << "\n"
                  << "    horizon interval (as used) : "
                  << Entity::getHorizonInterval()
                  << "\n";
              logger->addSmartBlank(logga::dbug);
              logger->putx(logga::dbug, put);
              logger->addSmartBlank(logga::dbug);

              put << "    xem-sourced entity count       : " << xemFullCount   << "\n";
              put << "    xem-sourced entity vector size : " << xemFull.size() << "\n";
              logger->addSmartBlank(logga::dbug);
              logger->putx(logga::dbug, put);
              logger->addSmartBlank(logga::dbug);

              // confirm mandatory types

              std::vector<std::string> unfulfilled;
              Entity::checkWants(unfulfilled);
              if ( unfulfilled.empty() )
                {
                  logger->repx(logga::dbug, "all mandatory entity types present", "");
                }
              else
                {
                  int size = unfulfilled.size();
                  logger->repx(logga::warn, "unfulfilled mandatory entity types", size);
                  put << "    " << "list of unfulfilled mandatory entity types" << "\n";
                  BOOST_FOREACH( std::string s, unfulfilled )
                    put << "      " << s << "\n";
                  logger->addSmartBlank(logga::dbug);
                  logger->putx(logga::dbug, put);
                  logger->addSmartBlank(logga::dbug);
                }

              // ---------------------------------
              //  list mandatory objects
              // ---------------------------------

              // this is a good point to test for mandatory
              // entity ids (noting that "time-horizon" which
              // has already been found) here and drop back if
              // any are absent
              //
              // the identifier strings themselves are
              // hard-coded in 'common.cc'

              std::vector<std::string> mandatories;
              mandatories.push_back(xeona::timehorizon);    // 'TimeHorizon' entity type
              mandatories.push_back(xeona::overseer);       // 'Overseer' entity type

              // ---------------------------------
              //  confirm mandatory objects
              // ---------------------------------

              int missingCount = 0;
              BOOST_FOREACH( std::string s, mandatories )
                {
                  if ( Entity::confirmIdentifier(s) == false )
                    {
                      put << "      " << s << "\n";
                      ++missingCount;
                    }
                }
              if ( missingCount == 0 )
                {
                  logger->repx(logga::dbug, "all mandatory entities found", "");
                }
              else
                {
                  logger->repx(logga::warn, "mandatory entities not found", missingCount);
                  std::ostringstream oss;
                  oss << "    " << "list of mandatory entities not found" << "\n";
                  logger->addSmartBlank(logga::dbug);
                  logger->putx(logga::dbug, oss);
                  logger->putx(logga::dbug, put);
                  logger->addSmartBlank(logga::dbug);

                  if ( xeona::nopro )
                    {
                      logger->repx(logga::info, "defensive coding omitted", "");
                      simret  = xeona::e_modelFileFault;
                    }
                  else
                    {
                      logger->repx(logga::dbug,
                                   "simulation downgraded to",
                                   "e_invokeFactory");
                      simkind = xeona::e_invokeFactory;     // downgrade simulation kind
                      simret  = xeona::e_modelFileFault;
                    }

                } // missingCount conditional

              // ---------------------------------
              //  * link and connect
              // ---------------------------------

              if ( simkind >= xeona::e_linkAndConnect  )
                {
                  logger->repx(logga::dbug,
                               "executing nested code for",
                               "link and connect");

                  // ---------------------------------
                  //  link entities via embedded pointers
                  // ---------------------------------

                  put << "   ++ about to link entities (information flows)" << "\n";
                  logger->addSmartBlank(logga::dbug);
                  logger->putx(logga::dbug, put);
                  logger->addSmartBlank(logga::dbug);

                  bool linkRet;
                  linkRet = Entity::linkAll();    // repeat call never problematic

                  logger->repx(logga::dbug, "Entity::linkAll return", linkRet);

                  // ---------------------------------
                  //  connect cables to sockets
                  // ---------------------------------

                  put << "   ++ about to connect interfaces (cables to sockets)" << "\n";
                  logger->addSmartBlank(logga::dbug);
                  logger->putx(logga::dbug, put);
                  logger->addSmartBlank(logga::dbug);

                  bool connectRet;
                  connectRet = Interface::connectAll();  // repeat call can be problematic

                  // a repeat call to 'Interface::connectAll' may
                  // be problematic depending on the setting of two
                  // hash-conditional in 'connec.cc' -- at the time
                  // of writing, a repeat call would be okay

                  logger->repx(logga::dbug, "Interface::connectAll return", connectRet);

                  // ---------------------------------
                  //  register gateways
                  // ---------------------------------

                  put << "   ++ about to locate gateways and complete their registration"
                      << "\n";
                  logger->addSmartBlank(logga::dbug);
                  logger->putx(logga::dbug, put);
                  logger->addSmartBlank(logga::dbug);

                  bool registerGatesRet;
                  registerGatesRet = xeona::registerGates();

                  logger->repx(logga::dbug,
                               "xeona::registerGates return",
                               registerGatesRet);

                  // ---------------------------------
                  //  * run simulation
                  // ---------------------------------

                  if ( simkind >= xeona::e_firstStepRun )
                    {
                      logger->repx(logga::dbug,
                                   "executing nested code for",
                                   "run (first or full)");

                      unsigned span = 0;         // initialize with nonsensical value

                      // ---------------------------------
                      //  set steps
                      // ---------------------------------

                      if ( simKind == xeona::e_firstStepRun )
                        {
                          span = 1;              // just one step

                          put << "   ++ about to simulate just one step" << "\n";
                          logger->addSmartBlank(logga::dbug);
                          logger->putx(logga::dbug, put);
                          logger->addSmartBlank(logga::dbug);
                        }
                      else if ( simkind >= xeona::e_fullRun )
                        {
                          span = Entity::getHorizonSteps();  // XEM file value, 2 or more

                          put << "   ++ about to simulate the entire horizon using span: "
                              << span << "\n";
                          logger->addSmartBlank(logga::dbug);
                          logger->putx(logga::dbug, put);
                          logger->addSmartBlank(logga::dbug);
                        }
                      else
                        {
                          std::clog << "** coding error 04 in source file " << __FILE__
                                    << std::endl;
                        }

                      // ---------------------------------
                      //  invoke 'Overseer::run'
                      // ---------------------------------

                      // the 'Overseer' identifier "overseer" is
                      // hard-coded in 'common.cc'

                      // CAUTION: the following call returns an
                      // 'Overseer' "downcast" -- needed because
                      // 'Entity' and 'FullEntity' do not possess
                      // 'run' calls.

                      shared_ptr<Overseer> overseer = Entity::retOverseer();

                      bool runRet = overseer->run(span);   // WOW: run the simulation!

                      if ( runRet == true )
                        {
                          logger->repx(logga::info, "Overseer::run call returned",runRet);
                        }
                      else
                        {
                          logger->repx(logga::warn, "Overseer::run call returned",runRet);
                          simret = xeona::e_infeasibility;  // assume infeasibility
                        }

                      // CAUTION: cannot reset horizon steps to
                      // unity here when mode option 6

                    } // 'xeona::e_firstStepRun' and above nesting, inc 'xeona::e_fullRun'

                } // 'xeona::e_linkAndConnect' nesting

            } // xeona::e_bindModel nesting

          // CAUTION: note that entities are not destructed
          // until after the 'main' function returns

          // ---------------------------------
          // model last-run information
          // ---------------------------------

          boost::posix_time::ptime           // stop timing
            toc(boost::posix_time::microsec_clock::universal_time());

          put << "   ++ about to update model last-run information" << "\n";
          logger->addSmartBlank(logga::dbug);
          logger->putx(logga::dbug, put);
          logger->addSmartBlank(logga::dbug);

          const std::string ret
            = xeona::modelStringDelim
            + ::interpretSimRet(simret)
            + xeona::modelStringDelim;
          const std::string kind
            = xeona::modelStringDelim
            + ::interpretSimKind(simkind)
            + xeona::modelStringDelim;

          put << "    model last run information" << "\n"
              << "    return : " << ret           << "\n"
              << "    kind   : " << kind          << "\n";
          logger->putx(logga::xtra, put);
          logger->addSmartBlank(logga::xtra);

          boost::posix_time::time_duration delta = toc - tic;
          const std::string simulateTime = xeona::formatDuration(delta);

          logger->repx(logga::info, "simulate time (hh::mm::ss or s.s)", simulateTime);

          dataIo.updateModelRunTime(ret, kind, simulateTime);

          // ---------------------------------
          // write model
          // ---------------------------------

          // the behavior of 'writeModel' depends on whether
          // 'DataIo::noteModelIsBound' has been called or not
          // -- if not then a shallow write will result,
          // otherwise a deep write will be used
          //
          // a shallow write can be enforced through the use of
          // "--mode 3" on the command-line (but no simulation
          // will be run)

          put << "   ++ about to write out model" << "\n";
          logger->addSmartBlank(logga::dbug);
          logger->putx(logga::dbug, put);
          logger->addSmartBlank(logga::dbug);

          dataIo.writeModel(model);          // takes a 'boost::filesystem::path' object

        } // xeona::e_parseModel nesting

    } // xeona::e_identifyFile nesting

  // ---------------------------------
  //  end of function return
  // ---------------------------------

  logger->repx(logga::xtra, "leaving free function, returning", simret);
  return simret;
}

//  end of file

