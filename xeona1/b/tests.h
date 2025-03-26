//  XEONA ENERGY SYSTEMS MODELING ENVIRONMENT
//
//  file-create-name : tests.h
//  file-create-date : Thu 24-Jan-2008 15:16 UTC
//  file-initiator   : Robbie Morrison <robbie@actrix.co.nz>
//
//  file-role        : test sub-entities for use with --inbuilt and such / header
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
//  $URL: file:///home/robbie/svn-root/xeona/futz/trunk/xeona1/b/tests.h $
//
//  GENERAL NOTES FOR THIS FILE
//
//  Supports the '--inbuilt' command-line option.

//  HEADER GUARD

#ifndef _TESTS_H_
#define _TESTS_H_

//  LOCAL AND SYSTEM INCLUDES

#include "../a/logger_fwd.h"  // some forward declarations from "logger.h"
#include "../b/entity.h"      // entity base class
#include "../c/smart_ptr.h"   // toggle between Boost and TR1 smart pointers

#include <iostream>           // standard io, used here for overloaded stream inserters
#include <list>               // STL sequence container
#include <string>             // C++ strings
#include <vector>             // STL sequence container

//  FORWARD (PARTIAL) DECLARATIONS

class Record;                                // see "recset.h"

template <typename C> class Socket;          // see "conex.h"
class CmWork;                                // see "commods.h"

//  CODE

// ---------------------------------------------------------
//  CLASS           : TestEntity0
// ---------------------------------------------------------
//  Purpose      : an oversimplified first sub-class of FullEntity
//  Status       : complete
// ---------------------------------------------------------

class TestEntity0 :
  public FullEntity
{
private:

  // DISABLED

  TestEntity0();                                  // prevent zero-argument construction
  TestEntity0& operator= (const TestEntity0&);    // prevent assignment

public:

  // CREATORS

  TestEntity0
  (const std::string entityId,               // enforced unique identifier
   Record&           record);                // associated record

  virtual                                    // support for polymorphic destruction
  ~TestEntity0();

  void
  factoryInitialize();

}; // class TestEntity0

// ---------------------------------------------------------
//  CLASS           : TestEntity1
// ---------------------------------------------------------
//  Purpose      : a representative first sub-class of FullEntity
//  Status       : complete
// ---------------------------------------------------------

class TestEntity1 :
  public FullEntity
{
private:

  // DISABLED

  TestEntity1();                                  // prevent zero-argument construction
  TestEntity1& operator= (const TestEntity1&);    // prevent assignment

public:

  // CREATORS

  TestEntity1
  (const std::string entityId,               // enforced unique identifier
   Record&           record);                // associated record

  virtual                                    // support for polymorphic destruction
  ~TestEntity1();

  virtual
  void
  factoryInitialize();

  // STREAM INSERTION SUPPORT

  virtual                                    // CAUTION: needed for polymorphic behavior
  std::ostream&
  streamOut                                  // support for overloaded operator<<
  (std::ostream& os) const;

  // STATIC ACCESSORS

  static
  const std::list<const TestEntity1*>&        // return const reference
  getCensus();

  static
  int
  popnSize();

  static
  std::string
  traverseFullPopulation();

  // UTILITY FUNCTIONS

private:

  // INTERNAL DATA

private:

  // input >
  std::string&                      d_userDescription;
  double&                           d_x;
  bool&                             d_single;
  shared_ptr<std::vector<int> >     d_timeseries;

  // output <
  shared_ptr<std::vector<int> >     d_electricalOutput;
  double&                           d_runTime;
  shared_ptr<std::vector<bool> >    d_someState;

  // STATIC DATA

private:

  static int                              s_ctorCount;
  static std::list<const TestEntity1*>    s_census;    // list of TestEntity1 instances

}; // class TestEntity1

// ---------------------------------------------------------
//  CLASS           : TestEntity2
// ---------------------------------------------------------
//  Purpose      : a representative first sub-class of FullEntity
//  Status       : complete
// ---------------------------------------------------------

class TestEntity2 :
  public FullEntity
{
private:

  // DISABLED

  TestEntity2();                                  // prevent zero-argument construction
  TestEntity2& operator= (const TestEntity2&);    // prevent assignment

public:

  // CREATORS

  TestEntity2
  (const std::string entityId,               // enforced unique identifier
   Record&           record);                // associated record

  virtual                                    // support for polymorphic destruction
  ~TestEntity2();

  // MANIPULATORS

  virtual
  void
  factoryInitialize();

  // INTERNAL DATA

private:

  // input >
  // NONE

  // output <
  // NONE

  // other
  shared_ptr<Socket<CmWork> >    d_socketWork;

}; // class TestEntity2

#endif // _TESTS_H_

//  end of file

