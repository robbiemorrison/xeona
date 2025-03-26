//  XEONA ENERGY SYSTEMS MODELING ENVIRONMENT
//
//  file-create-name : app_except.cc
//  file-create-date : Tue 28-Apr-2009 10:15 UTC
//  file-initiator   : Robbie Morrison <robbie@actrix.co.nz>
//
//  file-role        : application exception classes / implementation
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
//  $URL: file:///home/robbie/svn-root/xeona/futz/trunk/xeona1/a/exapp.cc $
//
//  GENERAL NOTES FOR THIS FILE

//  LOCAL AND SYSTEM INCLUDES

#include "exapp.h"            // companion header for this file (place first)

#include "../c/smart_ptr.h"   // toggle between Boost and TR1 smart pointers
#include ".././common.h"      // common definitions for project (place last)

#include <string>             // C++ strings
#include <sstream>            // string-streams

#include <boost/algorithm/string.hpp>   // string recasing, trimming, splitting

//  CODE

namespace xeona
{
  // ---------------------------------------------------------
  //  CLASS           : xeona::app_exception (abstract)
  // ---------------------------------------------------------

  app_exception::app_exception
  (const int exitcode) :
    exception(exitcode)
  { }

  app_exception::~app_exception()
  { }                                        // definition necessary

  // ---------------------------------------------------------
  //  CLASS           : xeona::kill_on_log
  // ---------------------------------------------------------

  kill_on_log::kill_on_log() :
    app_exception(xeona::exit_kill_on_log)
  {
    d_stringExpl = "";                       // no newline is correct
    d_stringTell = "xeona::kill_on_log exception";

  } // 'xeona::kill_on_log'

  // ---------------------------------------------------------
  //  CLASS           : xeona::non_registration
  // ---------------------------------------------------------

  non_registration::non_registration
  (const std::string entityIdentifier,       // 'entityId'
   const std::string entityRegistration,     // 'entityRegn'
   const std::string requestedClass) :       // 'r.locateClass()'
     app_exception(xeona::exit_non_registration)
  {
    std::ostringstream oss;
    oss
      << "** xeona non-registration exception"                                    << "\n"
      << "   unregistered entity class requested"                                 << "\n"
      << "       entity identifier   : " << entityIdentifier                      << "\n"
      << "       requested class     : " << requestedClass                        << "\n"
      << "       entity registration : " << entityRegistration                    << "\n"
      << "   likely causes"                                                       << "\n"
      << "       modeler misspelled entity class name in model file"              << "\n"
      << "       entity author failed to register entity class properly"          << "\n"
      << "   hardcoded exit status : " << d_code                                  << "\n";

    d_stringExpl = oss.str();
    d_stringTell = "xeona::non_registration exception";

  } // 'xeona::non_registration'

  // ---------------------------------------------------------
  //  CLASS           : xeona::empty_wrap
  // ---------------------------------------------------------

  empty_wrap::empty_wrap
  (const std::string type) :                 // 'xeona::demangle(typeid(T).name())'
     app_exception(xeona::exit_empty_wrap)
  {
    std::ostringstream oss;
    oss
      << "** xeona empty wrap exception"                                          << "\n"
      << "   about to assign from an empty (default constructed) wrap<> object"   << "\n"
      << "       parameterization : " << type                                     << "\n"
      << "   likely causes"                                                       << "\n"
      << "       input  : mismatch of supplied and required model data"           << "\n"
      << "       output : entity failed to produce suitably structured output"    << "\n"
      << "   for more information (including field name) see recent logging"      << "\n"
      << "   also try --class CLASSNAME for required model data format"           << "\n"
      << "   hardcoded exit status : " << d_code                                  << "\n";

    d_stringExpl = oss.str();
    d_stringTell = "xeona::empty_wrap exception";

  } // 'xeona::empty_wrap'

  // ---------------------------------------------------------
  //  CLASS           : xeona::short_timeseries
  // ---------------------------------------------------------

  short_timeseries::short_timeseries
  (const std::string fieldName,              // 'getName()'
   const int         shortfall) :            // 'int delta = horizon - len'
     app_exception(xeona::exit_short_timeseries)
  {
    std::ostringstream oss;
    oss
      << "** xeona short timeseries exception"                                    << "\n"
      << "   short timeseries encountered"                                        << "\n"
      << "       field name : " << fieldName                                      << "\n"
      << "       shortfall  : " << shortfall                                      << "\n"
      << "   likely causes"                                                       << "\n"
      << "       insufficient data (try adding trailing "
      << xeona::modelTsRepeater << ")"                                            << "\n"
      << "   for more information see recent logging output"                      << "\n"
      << "   hardcoded exit status : " << d_code                                  << "\n";

    d_stringExpl = oss.str();
    d_stringTell = "xeona::short_timeseries exception";

  } // 'xeona::short_timeseries'

  // ---------------------------------------------------------
  //  CLASS           : xeona::file_not_found
  // ---------------------------------------------------------

  file_not_found::file_not_found
  (const std::string filename,
   const std::string comment) :
    app_exception(xeona::exit_file_not_found)
  {
    std::ostringstream oss;
    oss
      << "** xeona file not found exception"                                      << "\n"
      << "       file name : " << filename                                        << "\n"
      << "         comment : " << comment                                         << "\n"
      << "   hardcoded exit status : " << d_code                                  << "\n";

    d_stringExpl = oss.str();
    d_stringTell = "xeona::file_not_found exception";

  } // 'xeona::file_not_found'

  // ---------------------------------------------------------
  //  CLASS           : xeona::xem_data_issue
  // ---------------------------------------------------------

  xem_data_issue::xem_data_issue
  (const std::string comment,
   const std::string name,
   const std::string valueStr) :
    app_exception(xeona::exit_xem_data_issue)
  {
    // 'value' can be 1000s of chars long so it needs truncating
    std::string trunc(valueStr);
    const unsigned width    = xeona::consoleWidth;  // often set to 142
    const unsigned strwidth = width - 20;           // CAUTION: unsigned for comparison
    if ( trunc.length() > strwidth ) trunc.erase(strwidth);
    boost::trim(trunc);                      // trim trailing space

    std::ostringstream oss;
    oss
      << "** xeona xem data issue exception"                                      << "\n"
      << "         comment : " << comment                                         << "\n"
      << "            name : " << name                                            << "\n"
      << "    value string : " << trunc                                           << "\n"
      << "    value length : " << valueStr.length()                               << "\n"
      << "   hardcoded exit status : " << d_code                                  << "\n";

    d_stringExpl = oss.str();
    d_stringTell = "xeona::xem_data_issue exception";

  } // 'xeona::xem_data_issue'

  // ---------------------------------------------------------
  //  CLASS           : xeona::lazy_link_fail
  // ---------------------------------------------------------

  lazy_link_fail::lazy_link_fail
  (const std::string comment,
   const std::string linkname) :
    app_exception(xeona::exit_lazy_link_fail)
  {
    std::ostringstream oss;
    oss
      << "** xeona lazy link fail exception"                                      << "\n"
      << "         comment : " << comment                                         << "\n"
      << "       link name : " << linkname                                        << "\n"
      << "   hardcoded exit status : " << d_code                                  << "\n";

    d_stringExpl = oss.str();
    d_stringTell = "xeona::lazy_link_fail exception";

  } // 'xeona::lazy_link_fail'

  // ---------------------------------------------------------
  //  CLASS           : xeona::cannot_run_guard
  // ---------------------------------------------------------

  cannot_run_guard::cannot_run_guard
  (const std::string filename) :
    app_exception(xeona::exit_cannot_run_guard)
  {
    std::ostringstream oss;
    oss
      << "** xeona cannot run guard file exception"                               << "\n"
      << "       file name : " << filename                                        << "\n"
      << "   likely fix"                                                          << "\n"
      << "       add option --guard to the command-line and rerun"                << "\n"
      << "   hardcoded exit status : " << d_code                                  << "\n";

    d_stringExpl = oss.str();
    d_stringTell = "xeona::cannot_run_file exception";

  } // 'xeona::cannot_run_guard'

  // ---------------------------------------------------------
  //  CLASS           : xeona::bad_authorship
  // ---------------------------------------------------------

  bad_authorship::bad_authorship
  (const int numberFailedEntities) :
    app_exception(xeona::exit_bad_authorship)
  {
    std::ostringstream oss;
    oss
      << "** xeona bad authorship exception"                                      << "\n"
      << "       number of failed entities : " << numberFailedEntities            << "\n"
      << "   likely fix"                                                          << "\n"
      << "       correct the problematic entities"                                << "\n"
      << "   hardcoded exit status : " << d_code                                  << "\n";

    d_stringExpl = oss.str();
    d_stringTell = "xeona::bad_authorship exception";

  } // xeona::bad_authorship'

  // ---------------------------------------------------------
  //  CLASS           : xeona::full_link_fail
  // ---------------------------------------------------------

  full_link_fail::full_link_fail
  (const std::string identifier,
   const std::string etype,
   const std::string mtype) :
    app_exception(xeona::exit_full_link_fail)
  {
    std::ostringstream oss;
    oss
      << "** xeona full link entity (information flows) fail exception"           << "\n"
      << "       identifier       : " << identifier                               << "\n"
      << "       cast (sub-)type  : " << etype                                    << "\n"
      << "       my type          : " << mtype                                    << "\n"
      << "   likely fix"                                                          << "\n"
      << "       mismatch of supplied and required model data"                    << "\n"
      << "       link problems include (but are not limited to) context entities" << "\n";
    if ( identifier.empty() )
      {
        oss << "       in this case, provide a non-empty link identifier"         << "\n";
      }
    else
      {
        oss << "       in this case, check presence of named link as an entity"   << "\n";
      }
    oss
      << "   hardcoded exit status : " << d_code                                  << "\n";

    d_stringExpl = oss.str();
    d_stringTell = "xeona::full_link_fail exception";
  }

  // ---------------------------------------------------------
  //  CLASS           : xeona::empty_field_on_write
  // ---------------------------------------------------------

  empty_field_on_write::empty_field_on_write
  (const std::string fieldType,
   const std::string fieldname) :
    app_exception(xeona::exit_empty_field_on_write)
  {
    std::ostringstream oss;
    oss
      << "** xeona empty field on write out exception"                            << "\n"
      << "       field type : " << fieldType                                      << "\n"
      << "       field name : " << fieldname                                      << "\n"
      << "   likely fix"                                                          << "\n"
      << "       mismatch of supplied and required field name"                    << "\n"
      << "       this problem should have also produced prior warnings"           << "\n"
      << "   hardcoded exit status : " << d_code                                  << "\n";

    d_stringExpl = oss.str();
    d_stringTell = "xeona::empty_field_on_write exception";

  } // 'xeona::empty_field_on_write'

  // ---------------------------------------------------------
  //  CLASS           : xeona::timeseries_not_found
  // ---------------------------------------------------------

  timeseries_not_found::timeseries_not_found
  (const std::string fieldname,
   const std::string templateType) :
    app_exception(xeona::exit_timeseries_not_found)
  {
    // process information
    const std::string utype = "shared_ptr<vector<" + templateType + "> >";
    std::string       fvdt  = "(not set)";           // field value data-type
    if      ( templateType == "bool"   ) fvdt = "B";
    else if ( templateType == "int"    ) fvdt = "I";
    else if ( templateType == "double" ) fvdt = "F";
    else if ( templateType == "string" ) fvdt = "S,L,X";    // CAUTION: not "std::string"

    std::ostringstream oss;
    oss
      << "** xeona timeseries not found exception"                                << "\n"
      << "       sought field name : " << fieldname                               << "\n"
      << "       underlying type   : " << utype                                   << "\n"
      << "       interpretation    : " << fvdt                                    << "\n"
      << "   likely fix"                                                          << "\n"
      << "       mismatch of supplied and required field name"                    << "\n"
      << "       this problem should have also produced prior warnings"           << "\n"
      << "       check previous reporting for associated entity identifier"       << "\n"
      << "   hardcoded exit status : " << d_code                                  << "\n";

    d_stringExpl = oss.str();
    d_stringTell = "xeona::timeseries_not_found exception";

#if 1

    // CAUTION: for some (completely mysterious) reason, this
    // exception was not being caught in 'main', so report
    // directly at construction-time in anticipation of a later
    // forced abort

    std::clog << std::flush
              << "\n"
              << " ++++ special construction-time report because"
              << " this exception was not being caught ++++" << "\n"
              << "\n"
              << d_stringExpl
              << std::flush;

    std::cout << std::flush
              << "\n"
              << "** " << d_stringTell << "\n"
              << "\n"
              << std::flush;

#endif // 0

  } // 'xeona::timeseries_not_found'

  // ---------------------------------------------------------
  //  CLASS           : xeona::yeek_abandon
  // ---------------------------------------------------------

  yeek_abandon::yeek_abandon
  (const std::string explanation) :
    app_exception(xeona::exit_yeek_abandon)
  {
    std::ostringstream oss;
    oss
      << "** xeona yeek abandon exception"                                        << "\n"
      << "       yeek value  : " << xeona::yeek                                   << "\n"
      << "       explanation : " << explanation                                   << "\n"
      << "   purpose"                                                             << "\n"
      << "       this exception does not indicate a fault state"                  << "\n"
      << "       it is used to trip the application under supported yeek values"  << "\n"
      << "   hardcoded exit status : " << d_code                                  << "\n";

    d_stringExpl = oss.str();
    d_stringTell = "xeona::yeek_abandon exception";

  } // 'xeona::yeek_abandon'

  // ---------------------------------------------------------
  //  CLASS           : xeona::bad_submodel
  // ---------------------------------------------------------

  bad_submodel::bad_submodel
  (const std::string submodel,
   const std::string explanation) :
    app_exception(xeona::exit_bad_submodel)
  {
    std::ostringstream oss;
    oss
      << "** xeona bad submodel exception"                                        << "\n"
      << "       submodel    : " << submodel                                      << "\n"
      << "       explanation : " << explanation                                   << "\n"
      << "   purpose"                                                             << "\n"
      << "       used to trip the application when a submodel evidently fails"    << "\n"
      << "   likely fix"                                                          << "\n"
      << "       debug the submodel"                                              << "\n"
      << "   hardcoded exit status : " << d_code                                  << "\n";

    d_stringExpl = oss.str();
    d_stringTell = "xeona::bad_submodel exception";

   } // 'xeona::bad_submodel'

  // ---------------------------------------------------------
  //  CLASS           : xeona::hour_resolution_only
  // ---------------------------------------------------------

  hour_resolution_only::hour_resolution_only
  (const std::string entity,
   const std::string identity,
   const std::string comment,
   const unsigned    interval) :
    app_exception(xeona::exit_hour_resolution_only)
  {
    std::ostringstream oss;
    oss
      << "** xeona hour resolution only exception"                                << "\n"
      << "       entity       : " << entity                                       << "\n"
      << "       identity     : " << identity                                     << "\n"
      << "       comment      : " << comment                                      << "\n"
      << "       interval [s] : " << interval                                     << "\n"
      << "   purpose"                                                             << "\n"
      << "       used to trip the application when given interval not supported"  << "\n"
      << "   likely fixes"                                                        << "\n"
      << "       revert to 3600s interval under 'entity.time-horizon.steps'"      << "\n"
      << "       use another entity where applicable"                             << "\n"
      << "       recode the offending entity"                                     << "\n"
      << "   hardcoded exit status : " << d_code                                  << "\n";

    d_stringExpl = oss.str();
    d_stringTell = "xeona::hour_resolution_only exception";

   } // 'xeona::hour_resolution_only'

  // ---------------------------------------------------------
  //  CLASS           : xeona::invalid_interval
  // ---------------------------------------------------------

  invalid_interval::invalid_interval
  (const int         invalid,
   const std::string valids) :               // space-separated string
    app_exception(xeona::exit_invalid_interval)
  {
    std::ostringstream oss;
    oss
      << "** xeona invalid interval value exception"                              << "\n"
      << "       entity        : " << "TimeHorizon"                               << "\n"
      << "       identity      : " << xeona::timehorizon                          << "\n"
      << "       invalid value : " << invalid                                     << "\n"
      << "       valid values  : " << valids                                      << "\n"
      << "   purpose"                                                             << "\n"
      << "       trip application when an invalid horizon interval is given"      << "\n"
      << "   likely fixes"                                                        << "\n"
      << "       first note only certain multiples of one hour are supported"     << "\n"
      << "       replace the current value of 'entity.time-horizon.interval'"     << "\n"
      << "       can also run using option --krazy to omit this test"             << "\n"
      << "   hardcoded exit status : " << d_code                                  << "\n";

    d_stringExpl = oss.str();
    d_stringTell = "xeona::invalid_interval exception";

   } // 'xeona::invalid_interval'

  // ---------------------------------------------------------
  //  CLASS           : xeona::bad_subentity_label
  // ---------------------------------------------------------

  bad_subentity_label::bad_subentity_label
  (const std::string hostId,
   const std::string function) :
    app_exception(xeona::exit_bad_subentity_label)
  {
    std::ostringstream oss;
    oss
      << "** xeona bad sub-entity (cable or socket) label exception"              << "\n"
      << "       host identity : " << hostId                                      << "\n"
      << "       thrown by     : " << function                                    << "\n"
      << "   purpose"                                                             << "\n"
      << "       trip application when a bad cable or socket label is provided"   << "\n"
      << "   likely fixes"                                                        << "\n"
      << "       looks like a sub-entity label is incorrect (fix model)"          << "\n"
      << "       a review of previous warn logs should reveal the culprit"        << "\n"
      << "       can also run using option --krazy to omit this test"             << "\n"
      << "   additional background"                                               << "\n"
      << "       the most probable cause is a model entity with a faulty"         << "\n"
      << "         sub-entity label, for instance: \"teas-xx.elec-1\" instead"    << "\n"
      << "         of \"teas-xx.sock-1\""                                         << "\n"
      << "   hardcoded exit status : " << d_code                                  << "\n";

    d_stringExpl = oss.str();
    d_stringTell = "xeona::bad_subentity_label exception";

  } // 'xeona::bad_subentity_label'

  // ---------------------------------------------------------
  //  CLASS           : xeona::no_gateway_demander
  // ---------------------------------------------------------

  no_gateway_demander::no_gateway_demander
  (const std::string myId,
   const std::string function,
   const std::string mySocketPartnerId) :
    app_exception(xeona::exit_no_gateway_demander)
  {
    std::ostringstream oss;
    oss
      << "** xeona no gateway demander exception"                                 << "\n"
      << "       my identity                : " << myId                           << "\n"
      << "       thrown by                  : " << function                       << "\n"
      << "       my socket partner identity : " << mySocketPartnerId              << "\n"
      << "   purpose"                                                             << "\n"
      << "       trip application when gate cannot resolve the demander"          << "\n"
      << "   likely fixes (provisional)"                                          << "\n"
      << "       naming issue if partner id not known (fix model)"                << "\n"
      << "       commodity class mismatch if partner id known (fix model)"        << "\n"
      << "   hardcoded exit status : " << d_code                                  << "\n";

    d_stringExpl = oss.str();
    d_stringTell = "xeona::no_gateway_demander exception";

  } // 'xeona::no_gateway_demander'

  // ---------------------------------------------------------
  //  CLASS           : xeona::no_gateway_controller
  // ---------------------------------------------------------

  no_gateway_controller::no_gateway_controller
  (const std::string myId,
   const std::string function,
   const std::string side) :
    app_exception(xeona::exit_no_gateway_controller)
  {
    std::ostringstream oss;
    oss
      << "** xeona no gateway controller exception"                               << "\n"
      << "       my identity    : " << myId                                       << "\n"
      << "       thrown by      : " << function                                   << "\n"
      << "       operator side  : " << side                                       << "\n"
      << "   purpose"                                                             << "\n"
      << "       gateway operator cannot be resolved"                             << "\n"
      << "   possible cause:"                                                     << "\n"
      << "     * model error (fix model)"                                         << "\n"
      << "   hardcoded exit status : " << d_code                                  << "\n";
    d_stringExpl = oss.str();
    d_stringTell = "xeona::no_gateway_controller exception";

  } // 'xeona::no_gateway_controller'

  // ---------------------------------------------------------
  //  CLASS           : xeona::hop_limit_reached
  // ---------------------------------------------------------

  hop_limit_reached::hop_limit_reached
  (const std::string myId,
   const std::string function,
   const int         callCount,
   const int         hopLimit) :
    app_exception(xeona::exit_hop_limit_reached)
  {
    std::ostringstream oss;
    oss
      << "** xeona CTA algorithm hop limit reached exception"                     << "\n"
      << "       my identity       : " << myId                                    << "\n"
      << "       thrown by         : " << function                                << "\n"
      << "       call count        : " << callCount                               << "\n"
      << "       preset hop limit  : " << hopLimit                                << "\n"
      << "   purpose"                                                             << "\n"
      << "       prevent endless hopping"                                         << "\n"
      << "   possible cause:"                                                     << "\n"
      << "     * one or more originating domain not listed with overseer"
      << " (fix model)"                                                           << "\n"
      << "       an originating domain possesses a commodity SOURCE of some form" << "\n"
      << "     * DAG (directed acyclic graph) requirement violated (fix model)"   << "\n"
      << "   hardcoded exit status : " << d_code                                  << "\n";
    d_stringExpl = oss.str();
    d_stringTell = "xeona::hop_limit_reached exception";

  } // 'xeona::hop_limit_reached'

} // namespace 'xeona'

//  end of file

