//  XEONA ENERGY SYSTEMS MODELING ENVIRONMENT
//
//  file-create-name : node.h
//  file-create-date : Tue 03-Nov-2009 12:18 UTC
//  file-initiator   : Robbie Morrison <robbie@actrix.co.nz>
//
//  file-role        : LMP node entity / header
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
//  $URL: file:///home/robbie/svn-root/xeona/futz/trunk/xeona1/b/node.h $
//
//  GENERAL NOTES FOR THIS FILE

//  HEADER GUARD

#ifndef _NODE_H_
#define _NODE_H_

//  LOCAL AND SYSTEM INCLUDES

#include "../b/tictoc.h"      // inherited interface for entities using common calls
#include "../b/block.h"       // block entity

#include "../a/logger_fwd.h"  // some forward declarations from "logger.h"
#include "../c/smart_ptr.h"   // toggle between Boost and TR1 smart pointers

#include <string>             // C++ strings
#include <sstream>            // string-streams

//  CODE

// ---------------------------------------------------------
//  CLASS           : LmpNode (abstract)
// ---------------------------------------------------------
//  Description  : LMP node base class
//  Role         : step in the inheritance web (intentionally sidesteps 'TechnicalAsset')
//  Techniques   : (nothing special)
//  Status       : complete
//
//  Design notes
//
//      Similarities to class 'DemandJunction'
//
// ---------------------------------------------------------

class LmpNode :
  public Block,
  public TicToc
{
  // DISABLED

private:

  LmpNode();                                 // zero-argument constructor
  LmpNode(const LmpNode& orig);              // copy constructor
  LmpNode& operator= (const LmpNode& orig);  // copy assignment operator

  // CREATORS

public:

  explicit
  LmpNode
  (const std::string entityId,
   Record&           record);

  virtual
  ~LmpNode() = 0;                             // create abstract class

  // CALLS

public:

  virtual void establish() { }               // necessary redefinition
  virtual void conclude();                   // logs warning on negative prices

  // INSTANCE DATA

protected:

  // tied quantities

  shared_ptr<std::vector<double> >    d_nodalPrices;

}; // class 'LmpNode'

// ---------------------------------------------------------
//  CLASS           : LmpNodeAc (abstract)
// ---------------------------------------------------------
//  Description  : LMP node base class for AC nodes
//  Role         : step in the inheritance web (intentionally sidesteps 'TechnicalAsset')
//  Techniques   : contains 'theta' for holding voltage angle
//  Status       : complete
//
//  Design notes
//
//      Similarities to class 'DemandJunction'
//
// ---------------------------------------------------------

class LmpNodeAc :
  public LmpNode
{
  // DISABLED

private:

  LmpNodeAc();                                    // zero-argument constructor
  LmpNodeAc(const LmpNodeAc& orig);               // copy constructor
  LmpNodeAc& operator= (const LmpNodeAc& orig);   // copy assignment operator

  // CREATORS

public:

  explicit
  LmpNodeAc
  (const std::string entityId,
   Record&           record);

  virtual
  ~LmpNodeAc() = 0;                             // create abstract class

  // CALLS

public:

  virtual void establish() { }               // necessary redefinition
  virtual void conclude()  { }               // necessary redefinition

  virtual void fixTheta(const double thetaDegrees) = 0;

  // INSTANCE DATA

protected:

  // tied quantities

  shared_ptr<std::vector<double> >    d_voltageAngles;

}; // class 'LmpNodeAc'

#endif // _NODE_H_

//  end of file

