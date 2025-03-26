//  XEONA ENERGY SYSTEMS MODELING ENVIRONMENT
//
//  file-create-name : exitstat.cc
//  file-create-date : Thu 14-May-2009 07:11 UTC
//  file-initiator   : Robbie Morrison <robbie@actrix.co.nz>
//
//  file-role        : exit status database / implementation
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
//  $URL: file:///home/robbie/svn-root/xeona/futz/trunk/xeona1/a/exitstat.cc $
//
//  GENERAL NOTES FOR THIS FILE

//  LOCAL AND SYSTEM INCLUDES

#include "exitstat.h"         // companion header for this file (place first)

#include "../a/logger.h"      // standard logging functionality (as required)
#include "../c/smart_ptr.h"   // toggle between Boost and TR1 smart pointers
#include ".././common.h"      // common definitions for project (place last)

#include <numeric>            // STL numerical algorithms
#include <sstream>            // string-streams
#include <string>             // C++ strings
#include <utility>            // STL pair, make_pair()

//  CODE

//  STATIC DEFINITIONS

logga::spLogger ExitStatus::s_logger = logga::ptrLogStream();

// ---------------------------------------------------------
//  MEMBER FUNCTION : ExitStatus
// ---------------------------------------------------------

// COMMENT: modify option '--output' text here

ExitStatus::ExitStatus
(const std::string notPresentMessage) :
  d_data(),                                  // empty map
  d_notPresentMessage(notPresentMessage)
{
  s_logger->repx(logga::dbug, "constructor call", "");

  // data load on construction (can break string lines with intervening " pair)

  add(
      //  0
      xeona::ret_success,
      "success (relative to prevailing exit trip, the default is to accept warnings)"
      );
  add(
      //  1
      logga::kill,
      "kill report calls made (requires exit trip 1 or greater)"
      );
  add(
      //  2
      logga::warn,
      "warning report calls made (requires exit trip 2 or greater)"
      );
  add(
      //  3
      logga::info,
      "information report calls made (requires exit trip 3 or greater)"
      );
  add(
      //  4
      logga::dbug,
      "debug report calls made (requires exit trip 4 or greater)"
      );
  add(
      //  5
      logga::xtra,
      "extra report calls made (requires exit trip 5 or greater)"
      );
  add(
      //  6
      logga::adhc,
      "ad-hoc report calls made (requires exit trip 6)"
      );
  add(
      //  8
      xeona::ret_noclass,
      "no match for given --class arg"
      );
  add(
      //  9
      xeona::ret_usage,
      "command-line usage issue (refer to the displayed explanation)"
      );
  add(
      // 10
      xeona::exit_kill_on_log,
      "xeona exception on printed KILL log call (where such action is enabled)"
      );
  add(
      // 11
      xeona::exit_empty_wrap,
      "xeona exception on empty wrap object (check entity data/class match)"
      );
  add(
      // 12
      xeona::exit_non_registration,
      "xeona exception on failure to find entity class name (check class string)"
      );
  add(
      // 13
      xeona::exit_short_timeseries,
      "xeona exception on short timeseries (check data length vs time-horizon.steps)"
      );
  add(
      // 14
      xeona::exit_file_not_found,
      "xeona exception on file not found"
      );
  add(
      // 15
      xeona::exit_xem_data_issue,
      "xeona exception on faulty xem data (duplicate name or hanging association)"
      );
  add(
      // 16
      xeona::exit_lazy_link_fail,
      "xeona exception on lazy link fail (check link name)"
      );
  add(
      // 17
      xeona::exit_cannot_run_guard,
      "xeona exception on attempt to directly run guard file (use option --guard)"
      );
  add(
      // 18
      xeona::exit_bad_authorship,
      "xeona exception on badly authored entities (coding problem)"
      );
  add(
      // 19
      xeona::exit_full_link_fail,
      "xeona exception on full link entity fail (data problem)"
      );
  add(
      // 20
      xeona::exit_empty_field_on_write,
      "xeona exception on empty field on write out (data problem)"
      );
  add(
      // 21
      xeona::exit_timeseries_not_found,
      "xeona exception on missing timeseries field (data problem)"
      );
  add(
      // 22
      xeona::exit_yeek_abandon,
      "xeona exception on optional application yeek code"
      );
  add(
      // 23
      xeona::exit_bad_submodel,
      "xeona exception on bad submodel (faulty results)"
      );
  add(
      // 24
      xeona::exit_hour_resolution_only,
      "xeona exception on resolution other than one hour (3600s)"
      );
  add(
      // 25
      xeona::exit_invalid_interval,
      "xeona exception on invalid horizon interval value"
      );
  add(
      // 26
      xeona::exit_bad_subentity_label,
      "xeona exception on bad subentity label or cast (for cable or socket)"
      );
  add(
      // 27
      xeona::exit_no_gateway_demander,
      "xeona exception on failure to resolve gateway demander"
      );
  add(
      // 28
      xeona::exit_no_gateway_controller,
      "xeona exception on failure to resolve gateway controller"
      );
  add(
      // 29
      xeona::exit_hop_limit_reached,
      "xeona exception because preset hop limit reached"
      );
  add(
      // 30
      xeona::exit_bidset_selections,
      "xeona exception thrown by 'AsopLmpBidParam' on invalid bidset selections"
      );
  add(
      // 31
      xeona::exit_entity_fail,
      "xeona exception thrown by entity author (read associated message)"
      );
  add(
      // 40
      xeona::exit_bad_assign_cast,
      "xeona::assign_ptr cast failed"
      );
  add(
      // 41
      xeona::exit_boost_exception,
      "unexpected boost library exception"
      );
  add(
      // 42
      xeona::exit_std_out_of_range,
      "unexpected standard out-of-range exception (read associated message)"
      );
  add(
      // 43
      xeona::exit_std_domain_error,
      "unexpected standard domain error (read associated message)"
      );
  add(
      // 44
      xeona::exit_std_logic_error,
      "unexpected standard logic error (read associated message)"
      );
  add(
      // 45
      xeona::exit_std_bad_alloc,
      "insufficient heap (mid-simulation) memory"
      );
  add(
      // 46
      xeona::exit_std_exception,
      "unexpected standard library exception"
      );
  add(
      // 47
      xeona::exit_unknown_exception,
      "unexpected unknown exception"
      );
  add(
      // 50
      xeona::ret_model_file_fault,
      "simulation call encountered faulty model file (may not be writable)"
      );
  add(
      // 51
      xeona::ret_infeasibility,
      "simulation call encountered infeasibility (solver fail or problem choke)"
      );
  add(
      // 52
      xeona::ret_errant_simulation,
      "simulation call returned non-differentiated fault status"
      );
  add(
      // 53
      xeona::ret_test_code_used,
      "simulation call encountered test code within a release build (coding mistake)"
      );
  add(
      // 58
      xeona::ret_other,
      "simulation call returned other"
      );
  add(
      // 59
      xeona::ret_status_not_known,
      "simulation call returned unknown status (dummy value not overwritten)"
      );
  add(
      // 99
      xeona::exit_test99,
      "reserved for code development purposes"
      );

  // from here, supported under option '--output' but not '--usage'

  add(
      // 130
      130,
      "user-initiated ^C"
      );
  add(
      // 134
      134,
      "bad malloc or free / uncaught throw / GLPK abort"
      );
  add(
      // 139
      139,
      "segfault on invalid memory use"
      );
  add(
      // 255
      xeona::ret_fail,
      "coding error or similar"
      );
}

// ---------------------------------------------------------
//  MEMBER FUNCTION : ~ExitStatus
// ---------------------------------------------------------

ExitStatus::~ExitStatus()
{
  s_logger->repx(logga::adhc, "destructor call", "");
}

// ---------------------------------------------------------
//  MEMBER FUNCTION : operator() (bool return)
// ---------------------------------------------------------

bool                                         // return 'false' if not present
ExitStatus::operator()
(const int    exitStatus,
 std::string& exitInterpretation) const
{
  // Josuttis (1999 p212) describes the various search options
  database_type::const_iterator pos = d_data.find(exitStatus);
  if ( pos == d_data.end() )
    {
      exitInterpretation = d_notPresentMessage;
      return false;
    }
  else
    {
      exitInterpretation = pos->second;
      return true;
    }
}

// ---------------------------------------------------------
//  MEMBER FUNCTION : notPresentMessage
// ---------------------------------------------------------

std::string
ExitStatus::notPresentMessage() const
{
  return d_notPresentMessage;
}

// ---------------------------------------------------------
//  MEMBER FUNCTION : size
// ---------------------------------------------------------

int
ExitStatus::size() const
{
  return static_cast<int>(d_data.size());    // cast from 'std::map<>::size_type'
}

// ---------------------------------------------------------
//  MEMBER FUNCTION : add
// ---------------------------------------------------------

void
ExitStatus::add
(const int         exitStatus,
 const std::string exitInterpretation)
{
  // Josuttis (1999 p203) describes the "second" insertion success test
  if ( ! d_data.insert(std::make_pair(exitStatus, exitInterpretation)).second )
    {
      s_logger->repx(logga::warn, "duplicate key insertion attempted", exitStatus);
    }
}

//  end of file

