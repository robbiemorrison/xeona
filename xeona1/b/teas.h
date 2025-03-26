//  XEONA ENERGY SYSTEMS MODELING ENVIRONMENT
//
//  file-create-name : asset.h
//  file-create-date : Tue 26-Aug-2008 14:15 UTC
//  file-initiator   : Robbie Morrison <robbie@actrix.co.nz>
//
//  file-role        : technical asset entity / header
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
//  $URL: file:///home/robbie/svn-root/xeona/futz/trunk/xeona1/b/teas.h $

//  HEADER GUARD

#ifndef _TEAS_H_
#define _TEAS_H_

//  LOCAL AND SYSTEM INCLUDES

#include "../b/tictoc.h"      // inherited interface for entities using common calls
#include "../b/costreg.h"     // cost registers
#include "../b/block.h"       // block entity

#include "../a/logger_fwd.h"  // some forward declarations from "logger.h"
#include "../c/smart_ptr.h"   // toggle between Boost and TR1 smart pointers

#include <string>             // C++ strings
#include <vector>             // STL sequence container

//  FORWARD (PARTIAL) DECLARATIONS

class LmpBidSet;                             // member function return
class Record;                                // constructor argument

//  CODE

// ---------------------------------------------------------
//  CLASS           : TechnicalAsset
// ---------------------------------------------------------
//  Description  : technical asset base class
//  Role         : step in the entity inheritance web
//  Techniques   : inheritance
//  Status       : complete
//
//  Design notes
//
//      Cost support
//
//          Cost support comes from whichever cost register is
//          inherited from.
//
//      Ceiling and floor duties
//
//          The data members 'd_ceilingDuty' and 'd_floorDuty'
//          should be reset in concrete technical assets as
//          required.
//
// ---------------------------------------------------------

class TechnicalAsset :
  public Block,
  public TicToc,
  public virtual CostRegister                // provides the set of registers
{
  // DISABLED

private:

  TechnicalAsset();                                         // zero-argument constructor
  TechnicalAsset(const TechnicalAsset& orig);               // copy constructor
  TechnicalAsset& operator= (const TechnicalAsset& orig);   // copy assignment operator

  // CREATORS

public:

  explicit
  TechnicalAsset
  (const std::string entityId,
   Record&           record,
   const int         commitmentModeSum);     // passed thru to 'TicToc' constructor

  virtual
  ~TechnicalAsset() = 0;                     // create abstract class

  // ASSET OPERATOR-RELATED ROUTINES

public:

  double                                     // prevailing upper bound on duty
  getCeilingDuty() const;

  double                                     // prevailing lower bound on duty
  getFloorDuty() const;

  void
  setCogenHeatWeight                         // CAUTION: similar class 'AuxHeatLead' calls
  (const double cogenHeatLeadWeight);

  const double
  getCogenHeatWeight() const;                // CAUTION: similar class 'AuxHeatLead' calls

  double                                     // prior duty
  getPriorDuty() const;                      // 'NaN' for step 0

  double                                     // prior size
  getPriorSize() const;                      // 'NaN' for step 0

  // specialist calls (warnings issued if not redefined)

  virtual
  shared_ptr<BandedTariffSet>                // current tariff set for adaptive behavior
  obtainTariffSet() const;

  virtual
  void
  passBuildingOccData
  (const double temperatureSetPoint,         // temperature set point [C] (Celsius)
   const double activityLoad,                // load contribution but not electricity [W]
   const double electricityDemand);          // electricity demand [W]

  // INSTANCE DATA

protected:

  double                       d_ceilingDuty;         // upper capacity in terms of duty
  double                       d_floorDuty;           // lower capacity in terms of duty
  double                       d_cogenHeatLeadWeight; // heat lead weighting [0,1]

};

#endif // _TEAS_H_

//  end of file

