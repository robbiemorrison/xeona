
//  XEONA ENERGY SYSTEMS MODELING ENVIRONMENT
//
//  file-create-name : optnode.h
//  file-create-date : Tue 03-Nov-2009 19:48 UTC
//  file-initiator   : Robbie Morrison <robbie@actrix.co.nz>
//
//  file-role        : node optimization sub-problems for LMP nodes / header
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
//  $URL: file:///home/robbie/svn-root/xeona/futz/trunk/xeona1/b/optnode.h $
//
//  GENERAL NOTES FOR THIS FILE

//  HEADER GUARD
//
//  This unit is for use with DC transmission.

#ifndef _OPTNODE_H_
#define _OPTNODE_H_

//  LOCAL AND SYSTEM INCLUDES

#include "../b/optprob.h"     // optimization sub-problem (OSP) class and key sub-classes

#include "../a/logger_fwd.h"  // some forward declarations from "logger.h"
#include "../c/smart_ptr.h"   // toggle between Boost and TR1 smart pointers

#include <string>             // C++ strings
#include <sstream>            // string-streams

//  CODE

// ---------------------------------------------------------
//  CLASS           : LmpCabSoc0
// ---------------------------------------------------------
//  Description  : LMP node with one normal cable and one normal socket
//  Role         : for instance, 'Node0InjXit' (non-network pass-thru node)
//  Techniques   : (nothing special)
//  Status       : complete
// ---------------------------------------------------------

class LmpCabSoc0 :
  public LmpNodeOsp
{
  // TYPEDEFS

public:

  typedef boost::tuple                       // can be in either local or global cols
  <int,                                      // cable  (single, normal)
   int>                                      // socket (single, normal)
  index_type;

  typedef boost::tuple
  <double,                                   // injection quantity
   double,                                   // exit quantity
   double>                                   // nodal price (reduced cost)
  results_type;

  // CREATORS

public:

  LmpCabSoc0
  (shared_ptr<svif::SolverIf> solver,
   const xeona::DomainMode    commitmentMode);

  virtual
  ~LmpCabSoc0();

  // FILL PROBLEM CALLS

public:

  index_type                                 // global cols
  uploadEngineering();

  // DOWNLOAD SOLUTION CALLS

  results_type
  downloadSolution() const;

  // INSTANCE DATA

private:

  index_type    d_cols;                      // local cols
  int           d_row;                       // local row

}; // class 'LmpCabSoc0'

// ---------------------------------------------------------
//  CLASS           : LmpCab1A
// ---------------------------------------------------------
//  Description  : LMP node with one normal cable and one bidirectional cable
//  Role         : for instance, 'Node1InjA' (spur line injection node)
//  Techniques   : (nothing special)
//  Status       : complete
// ---------------------------------------------------------

class LmpCab1A :
  public LmpNodeOsp
{
  // TYPEDEFS

public:

  typedef boost::tuple                       // can be in either local or global cols
  <int,                                      // cable    (single, normal)
   int>                                      // cable-bi (single, bidirectional)
  index_type;

  typedef boost::tuple
  <double,                                   // injection quantity
   double>                                   // nodal price (reduced cost)
  results_type;

  // CREATORS

public:

  LmpCab1A
  (shared_ptr<svif::SolverIf> solver,
   const xeona::DomainMode    commitmentMode);

  virtual
  ~LmpCab1A();

  // FILL PROBLEM CALLS

public:

  index_type                                 // global cols
  uploadEngineering();

  // DOWNLOAD SOLUTION CALLS

  results_type
  downloadSolution() const;

  // INSTANCE DATA

private:

  index_type    d_cols;                      // local cols
  int           d_row;                       // local row

}; // class 'LmpCab1A'

// ---------------------------------------------------------
//  CLASS           : LmpCab1B
// ---------------------------------------------------------
//  Description  : LMP node with one normal cable and one bidirectional socket
//  Role         : for instance, 'Node1InjB' (spur line injection node)
//  Techniques   : (nothing special)
//  Status       : complete
// ---------------------------------------------------------

class LmpCab1B :
  public LmpNodeOsp
{
  // TYPEDEFS

public:

  typedef boost::tuple                       // can be in either local or global cols
  <int,                                      // cable     (single, normal)
   int>                                      // socket-bi (single, bidirectional)
  index_type;

  typedef boost::tuple
  <double,                                   // injection quantity
   double>                                   // nodal price (reduced cost)
  results_type;

  // CREATORS

public:

  LmpCab1B
  (shared_ptr<svif::SolverIf> solver,
   const xeona::DomainMode    commitmentMode);

  virtual
  ~LmpCab1B();

  // FILL PROBLEM CALLS

public:

  index_type                                 // global cols
  uploadEngineering();

  // DOWNLOAD SOLUTION CALLS

  results_type
  downloadSolution() const;

  // INSTANCE DATA

private:

  index_type    d_cols;                      // local cols
  int           d_row;                       // local row

}; // class 'LmpCab1B'

// ---------------------------------------------------------
//  CLASS           : LmpSoc1A
// ---------------------------------------------------------
//  Description  : LMP node with one normal socket and one bidirectional cable
//  Role         : for instance, 'Node1XitA' (spur line exit node)
//  Techniques   : (nothing special)
//  Status       : complete
// ---------------------------------------------------------

class LmpSoc1A :
  public LmpNodeOsp
{
  // TYPEDEFS

public:

  typedef boost::tuple                       // can be in either local or global cols
  <int,                                      // cable-bi (single, bidirectional)
   int>                                      // socket   (single, normal)
  index_type;

  typedef boost::tuple
  <double,                                   // exit quantity
   double>                                   // nodal price (reduced cost)
  results_type;

  // CREATORS

public:

  LmpSoc1A
  (shared_ptr<svif::SolverIf> solver,
   const xeona::DomainMode    commitmentMode);

  virtual
  ~LmpSoc1A();

  // FILL PROBLEM CALLS

public:

  index_type                                 // global cols
  uploadEngineering();

  // DOWNLOAD SOLUTION CALLS

  results_type
  downloadSolution() const;

  // INSTANCE DATA

private:

  index_type    d_cols;                      // local cols
  int           d_row;                       // local row

}; // class 'LmpSoc1A'

// ---------------------------------------------------------
//  CLASS           : LmpSoc1B
// ---------------------------------------------------------
//  Description  : LMP node with one normal socket and one bidirectional socket
//  Role         : for instance, 'Node1XitB' (spur line exit node)
//  Techniques   : (nothing special)
//  Status       : complete
// ---------------------------------------------------------

class LmpSoc1B :
  public LmpNodeOsp
{
  // TYPEDEFS

public:

  typedef boost::tuple                       // can be in either local or global cols
  <int,                                      // socket-bi (single, bidirectional)
   int>                                      // socket    (single, normal)
  index_type;

  typedef boost::tuple
  <double,                                   // exit quantity
   double>                                   // nodal price (reduced cost)
  results_type;

  // CREATORS

public:

  LmpSoc1B
  (shared_ptr<svif::SolverIf> solver,
   const xeona::DomainMode    commitmentMode);

  virtual
  ~LmpSoc1B();

  // FILL PROBLEM CALLS

public:

  index_type                                 // global cols
  uploadEngineering();

  // DOWNLOAD SOLUTION CALLS

  results_type
  downloadSolution() const;

  // INSTANCE DATA

private:

  index_type    d_cols;                      // local cols
  int           d_row;                       // local row

}; // class 'LmpSoc1B'

// ---------------------------------------------------------
//  CLASS           : LmpNul2
// ---------------------------------------------------------
//  Description  : LMP node two bidirectional connections
//  Role         : for instance, 'Node2Nul' (node)
//  Techniques   : (nothing special)
//  Status       : complete
// ---------------------------------------------------------

class LmpNul2 :
  public LmpNodeOsp
{
  // TYPEDEFS

public:

  typedef boost::tuple                       // can be in either local or global cols
  <int,                                      // cable-bi  (single, bidirectional)
   int>                                      // socket-bi (single, bidirectional)
  index_type;

  typedef boost::tuple
  <double,                                   // throughput
   double>                                   // nodal price (reduced cost)
  results_type;

  // CREATORS

public:

  LmpNul2
  (shared_ptr<svif::SolverIf> solver,
   const xeona::DomainMode    commitmentMode);

  virtual
  ~LmpNul2();

  // FILL PROBLEM CALLS

public:

  index_type                                 // global cols
  uploadEngineering();

  // DOWNLOAD SOLUTION CALLS

  results_type
  downloadSolution() const;

  // INSTANCE DATA

private:

  index_type    d_cols;                      // local cols
  int           d_row;                       // local row

}; // class 'LmpNul2'

// ---------------------------------------------------------
//  CLASS           : LmpCab2
// ---------------------------------------------------------
//  Description  : LMP node with one normal cable and two bidirectional connections
//  Role         : for instance, 'Node2Inj' (node)
//  Techniques   : (nothing special)
//  Status       : complete
// ---------------------------------------------------------

class LmpCab2 :
  public LmpNodeOsp
{
  // TYPEDEFS

public:

  typedef boost::tuple                       // can be in either local or global cols
  <int,                                      // cable-1    (normal)
   int,                                      // cable-2-bi (bidirectional)
   int>                                      // socket-bi  (single, bidirectional)
  index_type;

  typedef boost::tuple
  <double,                                   // throughput
   double>                                   // nodal price (reduced cost)
  results_type;

  // CREATORS

public:

  LmpCab2
  (shared_ptr<svif::SolverIf> solver,
   const xeona::DomainMode    commitmentMode);

  virtual
  ~LmpCab2();

  // FILL PROBLEM CALLS

public:

  index_type                                 // global cols
  uploadEngineering();

  // DOWNLOAD SOLUTION CALLS

  results_type
  downloadSolution() const;

  // INSTANCE DATA

private:

  index_type    d_cols;                      // local cols
  int           d_row;                       // local row

}; // class 'LmpCab2'

// ---------------------------------------------------------
//  CLASS           : LmpSoc2
// ---------------------------------------------------------
//  Description  : LMP node with one normal socket and two bidirectional connections
//  Role         : for instance, 'Node2Xit' (node)
//  Techniques   : (nothing special)
//  Status       : complete
// ---------------------------------------------------------

class LmpSoc2 :
  public LmpNodeOsp
{
  // TYPEDEFS

public:

  typedef boost::tuple                       // can be in either local or global cols
  <int,                                      // cable-bi    (bidirectional)
   int,                                      // socket-1    (normal)
   int>                                      // socket-2-bi (single, bidirectional)
  index_type;

  typedef boost::tuple
  <double,                                   // exit quantity
   double>                                   // nodal price (reduced cost)
  results_type;

  // CREATORS

public:

  LmpSoc2
  (shared_ptr<svif::SolverIf> solver,
   const xeona::DomainMode    commitmentMode);

  virtual
  ~LmpSoc2();

  // FILL PROBLEM CALLS

public:

  index_type                                 // global cols
  uploadEngineering();

  // DOWNLOAD SOLUTION CALLS

  results_type
  downloadSolution() const;

  // INSTANCE DATA

private:

  index_type    d_cols;                      // local cols
  int           d_row;                       // local row

}; // class 'LmpSoc2'

// ---------------------------------------------------------
//  CLASS           : LmpNul3A
// ---------------------------------------------------------
//  Description  : LMP node of type A with three bidirectional connections
//  Role         : for instance, 'Node3NulA' (node)
//  Techniques   : (nothing special)
//  Status       : complete
// ---------------------------------------------------------

class LmpNul3A :
  public LmpNodeOsp
{
  // TYPEDEFS

public:

  typedef boost::tuple                       // can be in either local or global cols
  <int,                                      // cable-1-bi (one, bidirectional)
   int,                                      // cable-2-bi (two, bidirectional)
   int>                                      // socket-bi  (single, bidirectional)
  index_type;

  typedef boost::tuple
  <double,                                   // input to cable 1
   double,                                   // input to cable 2
   double,                                   // output to socket
   double>                                   // nodal price (reduced cost)
  results_type;

  // CREATORS

public:

  LmpNul3A
  (shared_ptr<svif::SolverIf> solver,
   const xeona::DomainMode    commitmentMode);

  virtual
  ~LmpNul3A();

  // FILL PROBLEM CALLS

public:

  index_type                                 // global cols
  uploadEngineering();

  // DOWNLOAD SOLUTION CALLS

  results_type
  downloadSolution() const;

  // INSTANCE DATA

private:

  index_type    d_cols;                      // local cols
  int           d_row;                       // local row

}; // class 'LmpNul3A'

// ---------------------------------------------------------
//  CLASS           : LmpNul3B
// ---------------------------------------------------------
//  Description  : LMP node of type B with three bidirectional connections
//  Role         : for instance, 'Node3NulB' (node)
//  Techniques   : (nothing special)
//  Status       : complete
// ---------------------------------------------------------

class LmpNul3B :
  public LmpNodeOsp
{
  // TYPEDEFS

public:

  typedef boost::tuple                       // can be in either local or global cols
  <int,                                      // cable-bi (one, bidirectional)
   int,                                      // socket-1-bi (two, bidirectional)
   int>                                      // socket-2-bi (single, bidirectional)
  index_type;

  typedef boost::tuple
  <double,                                   // input to cable
   double,                                   // output to socket 1
   double,                                   // output to socket 2
   double>                                   // nodal price (reduced cost)
  results_type;

  // CREATORS

public:

  LmpNul3B
  (shared_ptr<svif::SolverIf> solver,
   const xeona::DomainMode    commitmentMode);

  virtual
  ~LmpNul3B();

  // FILL PROBLEM CALLS

public:

  index_type                                 // global cols
  uploadEngineering();

  // DOWNLOAD SOLUTION CALLS

  results_type
  downloadSolution() const;

  // INSTANCE DATA

private:

  index_type    d_cols;                      // local cols
  int           d_row;                       // local row

}; // class 'LmpNul3B'

#endif // _OPTNODE_H_

//  end of file

