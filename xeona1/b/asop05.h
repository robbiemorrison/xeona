//  XEONA ENERGY SYSTEMS MODELING ENVIRONMENT
//
//  file-create-name : asop05.h
//  file-create-date : Sat 22-Oct-2011 19:35 UTC
//  file-initiator   : Robbie Morrison <robbie@actrix.co.nz>
//
//  file-role        : concrete asset operators 5 / header
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
//  $URL: file:///home/robbie/svn-root/xeona/futz/trunk/xeona1/b/asop05.h $
//
//  GENERAL NOTES FOR THIS FILE

//  HEADER GUARD

#ifndef _ASOP05_H_
#define _ASOP05_H_

//  LOCAL AND SYSTEM INCLUDES

#include "../b/asop.h"        // asset operator entity

#include "../a/logger_fwd.h"  // some forward declarations from "logger.h"
#include "../c/smart_ptr.h"   // toggle between Boost and TR1 smart pointers

#include <string>             // C++ strings

//  CODE

// ---------------------------------------------------------
//  CLASS           : AsopAdminStated
// ---------------------------------------------------------
//  Description  : administered control operator
//  Role         : concrete entity
//  Techniques   : integration with flow inhibitor classes
//  Status       : complete
//
//  Design notes
//
//      Intended to be coupled with flow inhibitor assets,
//      including class 'TeasFlowInhibitor'.
//
//      This class does not posses its own optimization
//      sub-problem (OSP).
//
// ---------------------------------------------------------

class AsopAdminStated :
  public AssetOperator,
  public CostRegisterAsop
{
  // USING DECLARATIONS

protected:

  using Entity::s_logger;                    // place in common scope for this class

  // DISABLED

private:

  AsopAdminStated();                                         // zero-argument constructor
  AsopAdminStated(const AsopAdminStated& orig);              // copy constructor
  AsopAdminStated& operator= (const AsopAdminStated& orig);  // copy assignment operator

  // CREATORS

public:

  explicit
  AsopAdminStated
  (const std::string entityId,
   Record&           record);

  virtual
  ~AsopAdminStated();

  // CALLS

public:

  virtual
  const int                                  // number of technical assets processesed
  constrain
  (const xeona::DomainMode capacityMode);

  virtual
  void
  washup();

  // INSTANCE DATA

private:

  // tied quanties

  shared_ptr<std::vector<double> >    d_relativeValveSettings;
  shared_ptr<std::vector<double> >    d_operationalPenaltys;

}; // class 'AsopAdminStated'

//  ==== XEDOC =================================================
//
//  entity.asop-admin-stated-0
//
//      class                                    > AsopAdminStated
//
//        an administered control operator, designed to be
//        coupled with flow inhibitor assets
//
//      builtin-remark s                         <
//
//      technical-assets L                       > "teas-flow-inhibitor-0"
//
//        technical-assets share the same administrative
//        parameters
//
//      relative-valve-settings [-] F            > 0.0 0.5 1.0 0.5 ..
//      operational-penaltys [+/s]               > 0.0 ..
//
//        the relative-valve-settings are self-explanatory, the
//        operational-penaltys are + in {$,J,kg,m2} as determined
//        by the commitment mode for the domain and do not
//        contribute to consolidated costs
//
//      standing-cost-financial [$/s] f          > 5.0e-03
//      standing-cost-greenhouse [kg/s] f        > 5.0e-03
//      standing-cost-nox [kg/s] f               > 0.0
//      standing-cost-depletion [J/s] f          > 0.0
//      standing-cost-landuse [m2/s] f           > 0.0
//
//      variable-costs-financial [$] F           < 0.0 ..
//      fixed-costs-financial [$] F              < 0.0 ..
//
//  ============================================================

#endif // _ASOP05_H_

//  end of file

