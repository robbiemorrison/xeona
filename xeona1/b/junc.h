//  XEONA ENERGY SYSTEMS MODELING ENVIRONMENT
//
//  file-create-name : junc.h
//  file-create-date : Mon 26-Oct-2009 12:44 UTC
//  file-initiator   : Robbie Morrison <robbie@actrix.co.nz>
//
//  file-role        : demand split/join junction entity / header
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
//  $URL: file:///home/robbie/svn-root/xeona/futz/trunk/xeona1/b/junc.h $
//
//  GENERAL NOTES FOR THIS FILE

//  HEADER GUARD

#ifndef _JUNC_H_
#define _JUNC_H_

//  LOCAL AND SYSTEM INCLUDES

#include "../b/tictoc.h"      // inherited interface for entities using common calls
#include "../b/block.h"       // block entity

#include "../a/logger_fwd.h"  // some forward declarations from "logger.h"
#include "../c/smart_ptr.h"   // toggle between Boost and TR1 smart pointers

#include <string>             // C++ strings
#include <sstream>            // string-streams

//  CODE

// ---------------------------------------------------------
//  CLASS           : DemandJunction (abstract)
// ---------------------------------------------------------
//  Description  : junction base class
//  Role         : step in the inheritance web (intentionally sidesteps 'TechnicalAsset')
//  Techniques   : inheritance
//  Status       : complete
//
//  Design notes
//
//      Constructor
//
//          Unlike 'TechnicalAsset', this class constructor does
//          not take the following third argument: const int
//          commitmentModeSum.  That instead is hard-coded in the
//          constructor initialization list, namely:
//
//              xeona::e_commitmentModes
//
//      Explicit listing
//
//          These entities are explicitly listed in the domain
//          controller XEM record, namely:
//
//              demand-junctions L > "teas-demand-2-split-0"
//
//          The underlying reason for this approach was to
//          maintain a clean design during development.
//
//      Alternative code for automatic location (suggestion)
//
//          These entities could, perhaps, remain implicit and be
//          located under the appropriate 'restructure' call -
//          noting that connections are fixed prior to the first
//          'restructure' call and that the relevant solver is
//          also known to that call.
//
//          Therefore it may be possible to locate junctions
//          automatically.  Moreover, as junctions do not form
//          costs, they need not be part of the entity management
//          hierarchy.
//
//          Possibly, add near end of 'Interface::bindOsp':
//
//          const bool ret                  = d_cnn->bindGols(...);
//          shared_ptr<Entity> part         = getPartner();
//          shared_ptr<DemandJunction> junc = dynamic_pointer_cast<DemandJunction>(part);
//          if ( junc ) junc->constrain(capacityMode);
//
// ---------------------------------------------------------

class DemandJunction :
  public Block,
  public TicToc
{
  // DISABLED

private:

  DemandJunction();                                         // zero-argument constructor
  DemandJunction(const DemandJunction& orig);               // copy constructor
  DemandJunction& operator= (const DemandJunction& orig);   // copy assignment operator

  // CREATORS

public:

  explicit
  DemandJunction
  (const std::string entityId,
   Record&           record);

  virtual
  ~DemandJunction() = 0;                     // create abstract class

  // CALLS

public:

  virtual void establish() { }               // necessary redefinition
  virtual void conclude()  { }               // necessary redefinition

}; // class 'DemandJunction'

#endif // _JUNC_H_

//  end of file

