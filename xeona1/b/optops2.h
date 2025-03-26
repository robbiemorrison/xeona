//  XEONA ENERGY SYSTEMS MODELING ENVIRONMENT
//
//  file-create-name : optops2.h
//  file-create-date : Tue 10-Jan-2012 13:12 UTC
//  file-initiator   : Robbie Morrison <robbie@actrix.co.nz>
//
//  file-role        : operations OSPs for technical assets 2 / header
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
//  $URL: file:///home/robbie/svn-root/xeona/futz/trunk/xeona1/b/optops2.h $
//
//  GENERAL NOTES FOR THIS FILE
//
//  See unit 'b/optops' for general background.

//  HEADER GUARD

#ifndef _OPTOPS2_H_
#define _OPTOPS2_H_

//  LOCAL AND SYSTEM INCLUDES

#include "../b/optprob.h"     // optimization sub-problem and key sub-classes

#include "../c/util3.h"       // free functions for floating point comparison
#include "../a/logger_fwd.h"  // some forward declarations from "logger.h"
#include "../c/smart_ptr.h"   // toggle between Boost and TR1 smart pointers

#include <string>             // C++ strings

//  CODE

// ---------------------------------------------------------
//  CLASS           : OpsLoadFlow_A
// ---------------------------------------------------------
//  Description  : discretized two-way AC transmission line
//  Role         : 'TeasAcTransmission'
//  Techniques   : discretization
//  Status       : complete
//
//  Design notes
//
//      Loss discretization parameters
//
//          maxShiftDegrees : maximum voltage shift difference
//              [degrees] -- set as high as 90 initially,
//              although the model breaks down over 45, while
//              field values above 25 are unusual -- this shift
//              also increases linearly to first-order with flow
//              until the line capacity is reached
//
//          discretization : number of discretization steps (also
//              called "blocks" in some of the background
//              documentation)
//
//          Regarding the interaction between the two --
//          unnecessarily high but safe values for the former
//          will require higher values for the latter.
//
// ---------------------------------------------------------

class OpsLoadFlow_A :
  public OperationsOsp
{
  // TYPEDEFS

public:

  typedef boost::tuple                       // can be in either local or global cols
  <int,                                      // cable flow   (single, bidirectional)
   int,                                      // cable theta  (voltage angle)
   int,                                      // socket flow  (single, bidirectional)
   int,                                      // socket theta (voltage angle)
   int>                                      // flow direction
  index_type;

  typedef boost::tuple
  <bool,                                     // flow direction flag, 'true' socket-2-cable
   double,                                   // cable flow [W]
   double,                                   // socket flow [W]
   double,                                   // relative duty [-]
   double,                                   // relative loss [-]
   double>                                   // voltage angle shift [degrees]
  results_type;

  // CREATORS

public:

  OpsLoadFlow_A
  (shared_ptr<svif::SolverIf> solver,
   const xeona::DomainMode    commitmentMode);

  virtual
  ~OpsLoadFlow_A();

  // FILL PROBLEM CALLS

public:

  index_type                                 // global cols
  uploadEngineering
  (const double capacity,                    // in unprefixed SI units [W]
   const double voltage,                     // in unprefixed SI units [V]
   const double resistancePerMetre,          // in unprefixed SI units [ohm/m]
   const double reactancePerMetre,           // in unprefixed SI units [ohm/m]
   const double length,                      // in unprefixed SI units [m]
   const double maxShiftDegrees,             // upper limit on discretization [degrees]
   const int    discretization);             // number of discretization steps

  // DOWNLOAD SOLUTION CALLS

public:

  results_type                               // see local typedef
  downloadSolution() const;

  // INSTANCE DATA

private:

  index_type    d_cols;                      // local cols
  double        d_capacity;
  double        d_maxShiftDegrees;

}; // class 'OpsLoadFlow_A'

#endif // _OPTOPS2_H_

//  end of file

