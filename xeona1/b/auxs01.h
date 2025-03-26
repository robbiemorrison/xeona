//  XEONA ENERGY SYSTEMS MODELING ENVIRONMENT
//
//  file-create-name : auxs.h
//  file-create-date : Thu 16-Jul-2009 21:48 UTC
//  file-initiator   : Robbie Morrison <robbie@actrix.co.nz>
//
//  file-role        : classes for auxiliary model data / header
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
//  $URL: file:///home/robbie/svn-root/xeona/futz/trunk/xeona1/b/auxs01.h $
//
//  GENERAL NOTES FOR THIS FILE
//
//  Not all auxiliary model data classes are located in this unit
//  -- for example, those related to costs are elsewhere.
//
//  These classes also hold their data as private and therefore
//  need to provide suitable accessor and manipulator calls.

//  HEADER GUARD

#ifndef _AUXS01_H_
#define _AUXS01_H_

//  LOCAL AND SYSTEM INCLUDES

#include "../a/logger_fwd.h"  // some forward declarations from "logger.h"
#include "../c/smart_ptr.h"   // toggle between Boost and TR1 smart pointers

#include <string>             // C++ strings
#include <vector>             // STL sequence container

//  CODE

//  FORWARD (PARTIAL) DECLARATIONS

class LmpBidSet;
class Record;

// ---------------------------------------------------------
//  CLASS           : AuxBidSets
// ---------------------------------------------------------
//  Description  : support for read-in bidset timeseries
//  Role         : auxiliary data class
//  Techniques   : multiple inheritance
//  Status       : complete
//
//  Usage
//
//      header file
//
//          #include "../b/auxs.h"
//          class SubEntity :
//            ...
//            public AuxBidSet
//          { };
//
//      implementation file
//
//          SubEntity::SubEntity
//          (const std::string entityId,
//           Record&           record) :
//            ...
//            AuxBidSet(record),
//            ...
//          { }
//
//
//  Xedoc entry
//
//      lmp-bidsets X                       > "40.0e+06 28.0e-09 * 30.0e+06 12.0e-09" ..
//
//        lmp-bidsets holds star-separated bidsets comprising
//        space-separated pairwise quantity/price [J $] offers in
//        no particular order
//
// ---------------------------------------------------------

class AuxBidSets
{
  // DISABLED

private:

  AuxBidSets();                                        // zero-argument constructor
  AuxBidSets(const AuxBidSets& orig);                  // copy constructor
  AuxBidSets& operator= (const AuxBidSets& orig);      // copy assignment operator

  // CREATORS

public:

  AuxBidSets
  (Record& record);

#ifndef _XUTEST
  virtual ~AuxBidSets() = 0;
#else
  virtual ~AuxBidSets();                     // concrete version for unit testing
#endif // _XUTEST

  // ACCESSORS (INTERPRETED RETURN)

public:

  const shared_ptr<LmpBidSet>
  getBidSet                                  // string parsed on demand
  (const int step) const;

  // INSTANCE DATA

private:

  // tied quantities

  shared_ptr<std::vector<std::string> >    d_lmpBidsets;

  // STATIC DATA

private:

  static logga::spLogger                   s_logger;   // shared_ptr to single logger obj

}; // class 'AuxBidSets'

// ---------------------------------------------------------
//  CLASS           : AuxHeatLead
// ---------------------------------------------------------
//  Description  : support for read-in heat lead weighting
//  Role         : auxiliary data class
//  Techniques   : multiple inheritance
//  Status       : complete
//
//  Usage
//
//      Header file
//
//          #include "../b/auxs.h"
//          class SubEntity :
//            ...
//            public AuxHeatLead
//          { };
//
//      Implementation file
//
//          SubEntity::SubEntity
//          (const std::string entityId,
//           Record&           record) :
//            ...
//            AuxHeatLead(record),
//            ...
//          { }
//
//
//  Xedoc entry
//
//      cogen-heat-lead-weighting [-] f          > 1.0
//
//        the cogen-lead-heat-weighting [0,1] is passed to any
//        co-generation assets to set their lead policy to heat
//        (1.0) or power (0.0) or some intermediate ratio
//
// ---------------------------------------------------------

class AuxHeatLead
{
  // DISABLED

private:

  AuxHeatLead();                                       // zero-argument constructor
  AuxHeatLead(const AuxHeatLead& orig);                // copy constructor
  AuxHeatLead& operator= (const AuxHeatLead& orig);    // copy assignment operator

  // CREATORS

public:

  AuxHeatLead
  (Record& record);

#ifndef _XUTEST
  virtual ~AuxHeatLead() = 0;
#else
  virtual ~AuxHeatLead();                     // concrete version for unit testing
#endif // _XUTEST

  // ACCESSORS

public:

  const double
  getCogenHeatLeadWeight() const;

  // MANIPULATORS

  void
  setCogenHeatLeadWeight
  (const double cogenHeatLeadWeighting);

  // INSTANCE DATA

private:

  // tied quantities

  double&                   d_cogenHeatLeadWeight;

  // STATIC DATA

private:

  static logga::spLogger    s_hl_logger;     // shared_ptr to single logger obj

}; // class 'AuxHeatLead'

#endif // _AUXS01_H_

//  end of file

