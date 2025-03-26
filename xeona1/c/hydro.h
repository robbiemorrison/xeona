//  XEONA ENERGY SYSTEMS MODELING ENVIRONMENT
//
//  file-create-name : hydro.h
//  file-create-date : Fri 04-Feb-2011 12:43 UTC
//  file-initiator   : Robbie Morrison <robbie@actrix.co.nz>
//
//  file-role        : hydro asset to operator data transfer / header
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
//  $URL: file:///home/robbie/svn-root/xeona/futz/trunk/xeona1/c/hydro.h $
//
//  GENERAL NOTES FOR THIS FILE

//  HEADER GUARD

#ifndef _HYDRO_H_
#define _HYDRO_H_

//  LOCAL AND SYSTEM INCLUDES

#include "../a/logger_fwd.h"  // some forward declarations from "logger.h"
#include "../c/smart_ptr.h"   // toggle between Boost and TR1 smart pointers

#include <string>             // C++ strings

//  CODE

// ---------------------------------------------------------
//  CLASS           : HydroStatus
// ---------------------------------------------------------
//  Description  : self-checking data-bundle containing hydro scheme status
//  Role         : hydro scheme to operator data transfer
//  Techniques   : some integrity checks
//  Status       : complete
//
//  Design notes
//
//      This data-bundle -- transferred to the operator before
//      they have bid -- provides status information necessary
//      for the bid-formation process.
//
//  CAUTION: note the sign convention
//
//        * inflows are positive
//        * outflows are negative
//
//  CAUTION: flow volumes are time-integrated
//
//      To provide for simple arithmetic, the flow volumes used
//      here have been integrated over the horizon interval.  As
//      dot{V} is constant for any given period means that, for
//      interval k:
//
//          flow_volume_k = dot{V}_k * interval_length
//
// ---------------------------------------------------------

class HydroStatus
{
  // DISABLED

private:

  HydroStatus();                                       // zero-argument constructor
  HydroStatus(const HydroStatus& orig);                // copy constructor
  HydroStatus& operator= (const HydroStatus& orig);    // copy assignment operator

  // CREATORS

public:

  HydroStatus
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
   const double       a_priorDispatch);

  ~HydroStatus();

  // ACCESSORS

public:

  std::string
  summarize                                  // trailing newline not required
  (const int indent) const;                  // left margin

  // INSTANCE DATA

public:

  // primary

  const std::string    hydroIdentifier;
  const int            step;
  const double         io;
  const double         capacity;
  const double         storageVol;
  const double         inventory;
  const double         priorInventory;
  const double         inflowVol;
  const double         histInflowVol;
  const double         priorInflowVol;
  const double         priorTakeVol;
  const double         priorSpillVol;
  const double         priorDispatch;

  // secondary

  const double         interval;
  const double         maxDutyTakeVol;
  const double         truncTakeVol;
  const double         relChange;

  // administration

private:

  int                  d_warnCount;

  // STATIC DATA

private:

  static logga::spLogger    s_logger;        // shared_ptr to single logger object

};

#endif // _HYDRO_H_

