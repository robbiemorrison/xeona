//  XEONA ENERGY SYSTEMS MODELING ENVIRONMENT
//
//  file-create-name : hydro.cc
//  file-create-date : Fri 04-Feb-2011 12:43 UTC
//  file-initiator   : Robbie Morrison <robbie@actrix.co.nz>
//
//  file-role        : hydro asset to operator data transfer / implementation
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
//  $URL: file:///home/robbie/svn-root/xeona/futz/trunk/xeona1/c/hydro.cc $
//
//  GENERAL NOTES FOR THIS FILE

//  LOCAL AND SYSTEM INCLUDES

#include "hydro.h"            // companion header for this file (place first)

#if _XUTEST                   // to reduce unit test dependencies
class Entity { public: static int getHorizonInterval() { return 7200; } };
#else
#include "../b/entity.h"      // entity base class plus lazy linking
#endif // _XUTEST

#include "../a/logger.h"      // standard logging functionality (as required)
#include "../c/smart_ptr.h"   // toggle between Boost and TR1 smart pointers
#include ".././common.h"      // common definitions for project (place last)

#include <iomanip>            // setw() and family
#include <string>             // C++ strings
#include <sstream>            // string-streams

#include <boost/format.hpp>   // printf style formatting

//  CODE

// ---------------------------------------------------------
//  CLASS           : HydroStatus
// ---------------------------------------------------------

// STATIC DEFINITIONS

logga::spLogger HydroStatus::s_logger = logga::ptrLogStream();     // bind logger

// ---------------------------------------------------------
//  MEMBER FUNCTION : HydroStatus
// ---------------------------------------------------------

HydroStatus::HydroStatus
(const std::string& a_hydroIdentifier,
 const int          a_step,
 const double       a_io,
 const double       a_capacity,
 const double       a_storageVol,
 const double       a_inventory,
 const double       a_priorInventory,
 const double       a_inflowVol,
 const double       a_histInflowVol,
 const double       a_priorInflowVol,
 const double       a_priorTakeVol,
 const double       a_priorSpillVol,
 const double       a_priorDispatch) :
  // primary
  hydroIdentifier(a_hydroIdentifier),
  step(a_step),
  io(a_io),
  capacity(a_capacity),
  storageVol(a_storageVol),
  inventory(a_inventory),
  priorInventory(a_priorInventory),
  inflowVol(a_inflowVol),
  histInflowVol(a_histInflowVol),
  priorInflowVol(a_priorInflowVol),
  priorTakeVol(a_priorTakeVol),
  priorSpillVol(a_priorSpillVol),
  priorDispatch(a_priorDispatch),
  // secondary
  interval(Entity::getHorizonInterval()),
  maxDutyTakeVol(-capacity * interval / io), // -ve
  truncTakeVol((inventory < -maxDutyTakeVol) ? -inventory : maxDutyTakeVol),
  relChange((priorInventory - inventory) / inventory),
  d_warnCount(0)
{
  // integrity checks
  if ( step < 0 )
    {
      ++d_warnCount;
      s_logger->repx(logga::warn, "step failed integrity check", step);
    }
  if ( inventory > storageVol )
    {
      if ( s_logger->jumpyIsWarn() ) ++d_warnCount;
      std::ostringstream oss;
      oss << inventory << " " << storageVol;
      s_logger->repx(logga::rankJumpy,
                     "inventory/volume mismatch",
                     oss.str());
    }
  if ( inflowVol < 0.0 )
    {
      ++d_warnCount;
      s_logger->repx(logga::warn,
                     "negative inflow (outflow -ve)",
                     inflowVol);
    }
  if ( histInflowVol < 0.0 )
    {
      if ( s_logger->jumpyIsWarn() ) ++d_warnCount;
      s_logger->repx(logga::rankJumpy,
                     "negative hist inflow (outflow -ve)",
                     histInflowVol);
    }
  if ( priorInventory > storageVol )
    {
      if ( s_logger->jumpyIsWarn() ) ++d_warnCount;
      std::ostringstream oss;
      oss << priorInventory << " " << storageVol;
      s_logger->repx(logga::rankJumpy,
                     "prior inventory/volume mismatch",
                     oss.str());
    }
  if ( priorInflowVol < 0.0 )
    {
      if ( s_logger->jumpyIsWarn() ) ++d_warnCount;
      s_logger->repx(logga::rankJumpy,
                     "negative prior inflow",
                     priorInflowVol);
    }
  if ( priorTakeVol > 0.0 )                  // pumped storage is best treated separately
    {
      if ( s_logger->jumpyIsWarn() ) ++d_warnCount;
      s_logger->repx(logga::rankJumpy,
                     "positive take (outflow -ve)",
                     priorTakeVol);
    }
  if ( priorSpillVol > 0.0 )                 // can never be strictly positive
    {
      if ( s_logger->jumpyIsWarn() ) ++d_warnCount;
      s_logger->repx(logga::rankJumpy,
                     "positive spill (outflow -ve)",
                     priorSpillVol);
    }
  if ( priorDispatch < 0.0 )
    {
      if ( s_logger->jumpyIsWarn() ) ++d_warnCount;
      s_logger->repx(logga::rankJumpy,
                     "negative dispatch",
                     priorDispatch);
    }

  // warn count reporting
  s_logger->repx(logga::adhc, "warn count", d_warnCount);

  // additional reporting as appropriate
  // YEEK 45 CODE (set by '--yeek')
  if ( xeona::yeek == 45 || xeona::yeek == 1 || xeona::yeek == 2 )
    {
      std::ostringstream put;
      put << summarize(2);                   // see below, major reporting
      s_logger->repx(logga::dbug, "additional reporting follows, yeek", xeona::yeek);
      s_logger->putx(logga::dbug, put);
    }

} // function 'HydroStatus' constructor

// ---------------------------------------------------------
//  MEMBER FUNCTION : ~HydroStatus
// ---------------------------------------------------------

HydroStatus::~HydroStatus()
{
  s_logger->repx(logga::adhc, "destructor call", "");
}

// ---------------------------------------------------------
//  MEMBER FUNCTION : summarize
// ---------------------------------------------------------

std::string
HydroStatus::summarize
(const int indent) const
{
  // using declaration
  using boost::format;                       // for convenience

  // initial reporting
  s_logger->repx(logga::adhc, "entering member function, warns", d_warnCount);

  // balance
  const double calcInventory
    = priorInventory
    + priorInflowVol
    + priorTakeVol
    + priorSpillVol;
  const double calcAvailInventory
    = calcInventory
    + inflowVol;
  const double currRelInv  = inventory / storageVol;
  const double priorRelInv = priorInventory / storageVol;

  // stream data
  const std::string tab(indent, ' ');
  std::ostringstream oss;
  if ( d_warnCount == 1 )
    {
      oss << tab << format("    ** %d WARNING ISSUED **\n")  % d_warnCount;
    }
  else if ( d_warnCount > 1 )
    {
      oss << tab << format("    ** %d WARNINGS ISSUED **\n")  % d_warnCount;
    }
  oss
    << tab << format("hydro status summary / outflow is -ve\n")
    << tab << format("  key: * = depends on prevailing lake level\n")
    << tab << format("    asset identifier       : %s\n")         % hydroIdentifier
    << tab << format("    current step           : %d\n")         % step
    << tab << format("    interval (called)  [s] : %s\n")         % interval
    << tab << format("  generation\n")
    << tab << format("  * io factor       [J/m3] : %.0f\n")       % io
    << tab << format("  * capacity           [W] : %9.0f\n")      % capacity
    << tab << format("    prior dispatch     [W] : %9.0f\n")      % priorDispatch
    << tab << format("  lake\n")
    << tab << format("    storage volume    [m3] : %11.0f\n")     % storageVol
    << tab << format("  current\n")
    << tab << format("    avail inventory   [m3] : %11.0f\n")     % inventory
    << tab << format("    inflow            [m3] : %+11.0f\n")    % inflowVol
    << tab << format("    inflow historic   [m3] : %+11.0f\n")    % histInflowVol
    << tab << format("  prior\n")
    << tab << format("    start inventory   [m3] : %11.0f\n")     % priorInventory
    << tab << format("    inflow            [m3] : %+11.0f\n")    % priorInflowVol
    << tab << format("    take              [m3] : %+11.0f\n")    % priorTakeVol
    << tab << format("    spill             [m3] : %+11.0f\n")    % priorSpillVol
    << tab << format("  calculated\n")
    << tab << format("    start inventory   [m3] : %11.0f\n")     % calcInventory
    << tab << format("    avail invetory    [m3] : %11.0f\n")     % calcAvailInventory
    << tab << format("  generation usage\n")
    << tab << format("    unrestricted take [m3] : %11.0f\n")     % maxDutyTakeVol
    << tab << format("    truncated take    [m3] : %11.0f\n")     % truncTakeVol
    << tab << format("  some metrics\n")
    << tab << format("    current            [-] : %+6.2f %%\n")  % (100 * currRelInv)
    << tab << format("    prior              [-] : %+6.2f %%\n")  % (100 * priorRelInv)
    << tab << format("    change             [-] : %+6.2f %%\n")  % (100 * relChange);

  // return string
  return oss.str();
}

//  end of file

