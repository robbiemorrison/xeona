//  XEONA ENERGY SYSTEMS MODELING ENVIRONMENT
//
//  file-create-name : optnode2.h
//  file-create-date : Tue 10-Jan-2012 19:28 UTC
//  file-initiator   : Robbie Morrison <robbie@actrix.co.nz>
//
//  file-role        : node optimization sub-problems for LMP nodes 2 / header
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
//  $URL: file:///home/robbie/svn-root/xeona/futz/trunk/xeona1/b/optnode2.h $
//
//  GENERAL NOTES FOR THIS FILE
//
//  This unit is for use with AC transmission.  The OSP class
//  names begin "Lmp2".

//  HEADER GUARD

#ifndef _OPTNODE2_H_
#define _OPTNODE2_H_

//  LOCAL AND SYSTEM INCLUDES

#include "../b/optprob.h"     // optimization sub-problem (OSP) class and key sub-classes

#include "../a/logger_fwd.h"  // some forward declarations from "logger.h"
#include "../c/smart_ptr.h"   // toggle between Boost and TR1 smart pointers

#include <string>             // C++ strings
#include <sstream>            // string-streams

//  CODE

// ---------------------------------------------------------
//  CLASS           : Lmp2CabSoc0
// ---------------------------------------------------------
//  Description  : LMP node with one normal cable and one normal socket
//  Role         : for instance, 'NodeAc0InjXit' (non-network pass-thru node)
//  Techniques   : (nothing special)
//  Status       : complete
//  CAUTION      : no 'theta' required or supplied
// ---------------------------------------------------------

class Lmp2CabSoc0 :
  public Lmp2NodeOsp
{
  // TYPEDEFS

public:

  typedef boost::tuple                       // can be in either local or global cols
  <int,                                      // cable  (single, normal)
   int>                                      // socket (single, normal)
  index_type;                                // CAUTION: no theta col required

  typedef boost::tuple
  <double,                                   // injection quantity
   double,                                   // exit quantity
   double>                                   // nodal price (reduced cost)
  results_type;

  // CREATORS

public:

  Lmp2CabSoc0
  (shared_ptr<svif::SolverIf> solver,
   const xeona::DomainMode    commitmentMode);

  virtual
  ~Lmp2CabSoc0();

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

}; // class 'Lmp2CabSoc0'

// ---------------------------------------------------------
//  CLASS           : Lmp2Cab1A
// ---------------------------------------------------------
//  Description  : LMP node with one normal cable and one bidirectional cable
//  Role         : for instance, 'NodeAc1InjA' (spur line injection node)
//  Techniques   : (nothing special)
//  Status       : complete
// ---------------------------------------------------------

class Lmp2Cab1A :
  public Lmp2NodeOsp
{
  // TYPEDEFS

public:

  typedef boost::tuple                       // can be in either local or global cols
  <int,                                      // cable    (single, normal)
   int,                                      // cable-bi (single, bidirectional)
   int>                                      // theta
  index_type;

  typedef boost::tuple
  <double,                                   // injection quantity
   double,                                   // voltage angle (degrees)
   double>                                   // nodal price (reduced cost)
  results_type;

  // CREATORS

public:

  Lmp2Cab1A
  (shared_ptr<svif::SolverIf> solver,
   const xeona::DomainMode    commitmentMode);

  virtual
  ~Lmp2Cab1A();

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

}; // class 'Lmp2Cab1A'

// ---------------------------------------------------------
//  CLASS           : Lmp2Cab1B
// ---------------------------------------------------------
//  Description  : LMP node with one normal cable and one bidirectional socket
//  Role         : for instance, 'NodeAc1InjB' (spur line injection node)
//  Techniques   : (nothing special)
//  Status       : complete
// ---------------------------------------------------------

class Lmp2Cab1B :
  public Lmp2NodeOsp
{
  // TYPEDEFS

public:

  typedef boost::tuple                       // can be in either local or global cols
  <int,                                      // cable     (single, normal)
   int,                                      // socket-bi (single, bidirectional)
   int>                                      // theta
  index_type;

  typedef boost::tuple
  <double,                                   // injection quantity
   double,                                   // voltage angle (degrees)
   double>                                   // nodal price (reduced cost)
  results_type;

  // CREATORS

public:

  Lmp2Cab1B
  (shared_ptr<svif::SolverIf> solver,
   const xeona::DomainMode    commitmentMode);

  virtual
  ~Lmp2Cab1B();

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

}; // class 'Lmp2Cab1B'

// ---------------------------------------------------------
//  CLASS           : Lmp2Soc1A
// ---------------------------------------------------------
//  Description  : LMP node with one normal socket and one bidirectional cable
//  Role         : for instance, 'NodeAc1XitA' (spur line exit node)
//  Techniques   : (nothing special)
//  Status       : complete
// ---------------------------------------------------------

class Lmp2Soc1A :
  public Lmp2NodeOsp
{
  // TYPEDEFS

public:

  typedef boost::tuple                       // can be in either local or global cols
  <int,                                      // cable-bi (single, bidirectional)
   int,                                      // socket   (single, normal)
   int>                                      // theta
  index_type;

  typedef boost::tuple
  <double,                                   // exit quantity
   double,                                   // voltage angle (degrees)
   double>                                   // nodal price (reduced cost)
  results_type;

  // CREATORS

public:

  Lmp2Soc1A
  (shared_ptr<svif::SolverIf> solver,
   const xeona::DomainMode    commitmentMode);

  virtual
  ~Lmp2Soc1A();

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

}; // class 'Lmp2Soc1A'

// ---------------------------------------------------------
//  CLASS           : Lmp2Soc1B
// ---------------------------------------------------------
//  Description  : LMP node with one normal socket and one bidirectional socket
//  Role         : for instance, 'NodeAc1XitB' (spur line exit node)
//  Techniques   : (nothing special)
//  Status       : complete
// ---------------------------------------------------------

class Lmp2Soc1B :
  public Lmp2NodeOsp
{
  // TYPEDEFS

public:

  typedef boost::tuple                       // can be in either local or global cols
  <int,                                      // socket-bi (single, bidirectional)
   int,                                      // socket    (single, normal)
   int>                                      // theta
  index_type;

  typedef boost::tuple
  <double,                                   // exit quantity
   double,                                   // voltage angle (degrees)
   double>                                   // nodal price (reduced cost)
  results_type;

  // CREATORS

public:

  Lmp2Soc1B
  (shared_ptr<svif::SolverIf> solver,
   const xeona::DomainMode    commitmentMode);

  virtual
  ~Lmp2Soc1B();

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

}; // class 'Lmp2Soc1B'

// ---------------------------------------------------------
//  CLASS           : Lmp2Nul2
// ---------------------------------------------------------
//  Description  : LMP node two bidirectional connections
//  Role         : for instance, 'NodeAc2Nul' (node)
//  Techniques   : (nothing special)
//  Status       : complete
// ---------------------------------------------------------

class Lmp2Nul2 :
  public Lmp2NodeOsp
{
  // TYPEDEFS

public:

  typedef boost::tuple                       // can be in either local or global cols
  <int,                                      // cable-bi  (single, bidirectional)
   int,                                      // socket-bi (single, bidirectional)
   int>                                      // theta
  index_type;

  typedef boost::tuple
  <double,                                   // throughput
   double,                                   // voltage angle (degrees)
   double>                                   // nodal price (reduced cost)
  results_type;

  // CREATORS

public:

  Lmp2Nul2
  (shared_ptr<svif::SolverIf> solver,
   const xeona::DomainMode    commitmentMode);

  virtual
  ~Lmp2Nul2();

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

}; // class 'Lmp2Nul2'

// ---------------------------------------------------------
//  CLASS           : Lmp2Cab2
// ---------------------------------------------------------
//  Description  : LMP node with one normal cable and two bidirectional connections
//  Role         : for instance, 'NodeAc2Inj' (node)
//  Techniques   : (nothing special)
//  Status       : complete
// ---------------------------------------------------------

class Lmp2Cab2 :
  public Lmp2NodeOsp
{
  // TYPEDEFS

public:

  typedef boost::tuple                       // can be in either local or global cols
  <int,                                      // cable-1    (normal)
   int,                                      // cable-2-bi (bidirectional)
   int,                                      // socket-bi  (single, bidirectional)
   int>                                      // theta
  index_type;

  typedef boost::tuple
  <double,                                   // throughput
   double,                                   // voltage angle (degrees)
   double>                                   // nodal price (reduced cost)
  results_type;

  // CREATORS

public:

  Lmp2Cab2
  (shared_ptr<svif::SolverIf> solver,
   const xeona::DomainMode    commitmentMode);

  virtual
  ~Lmp2Cab2();

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

}; // class 'Lmp2Cab2'

// ---------------------------------------------------------
//  CLASS           : Lmp2Soc2
// ---------------------------------------------------------
//  Description  : LMP node with one normal socket and two bidirectional connections
//  Role         : for instance, 'NodeAc2Xit' (node)
//  Techniques   : (nothing special)
//  Status       : complete
// ---------------------------------------------------------

class Lmp2Soc2 :
  public Lmp2NodeOsp
{
  // TYPEDEFS

public:

  typedef boost::tuple                       // can be in either local or global cols
  <int,                                      // cable-bi    (bidirectional)
   int,                                      // socket-1    (normal)
   int,                                      // socket-2-bi (single, bidirectional)
   int>                                      // theta
  index_type;

  typedef boost::tuple
  <double,                                   // exit quantity
   double,                                   // voltage angle (degrees)
   double>                                   // nodal price (reduced cost)
  results_type;

  // CREATORS

public:

  Lmp2Soc2
  (shared_ptr<svif::SolverIf> solver,
   const xeona::DomainMode    commitmentMode);

  virtual
  ~Lmp2Soc2();

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

}; // class 'Lmp2Soc2'

// ---------------------------------------------------------
//  CLASS           : Lmp2Nul3A
// ---------------------------------------------------------
//  Description  : LMP node of type A with three bidirectional connections
//  Role         : for instance, 'NodeAc3NulA' (node)
//  Techniques   : (nothing special)
//  Status       : complete
// ---------------------------------------------------------

class Lmp2Nul3A :
  public Lmp2NodeOsp
{
  // TYPEDEFS

public:

  typedef boost::tuple                       // can be in either local or global cols
  <int,                                      // cable-1-bi (one, bidirectional)
   int,                                      // cable-2-bi (two, bidirectional)
   int,                                      // socket-bi  (single, bidirectional)
   int>                                      // theta
  index_type;

  typedef boost::tuple
  <double,                                   // input to cable 1
   double,                                   // input to cable 2
   double,                                   // output to socket
   double,                                   // voltage angle (degrees)
   double>                                   // nodal price (reduced cost)
  results_type;

  // CREATORS

public:

  Lmp2Nul3A
  (shared_ptr<svif::SolverIf> solver,
   const xeona::DomainMode    commitmentMode);

  virtual
  ~Lmp2Nul3A();

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

}; // class 'Lmp2Nul3A'

// ---------------------------------------------------------
//  CLASS           : Lmp2Nul3B
// ---------------------------------------------------------
//  Description  : LMP node of type B with three bidirectional connections
//  Role         : for instance, 'NodeAc3NulB' (node)
//  Techniques   : (nothing special)
//  Status       : complete
// ---------------------------------------------------------

class Lmp2Nul3B :
  public Lmp2NodeOsp
{
  // TYPEDEFS

public:

  typedef boost::tuple                       // can be in either local or global cols
  <int,                                      // cable-bi (one, bidirectional)
   int,                                      // socket-1-bi (two, bidirectional)
   int,                                      // socket-2-bi (single, bidirectional)
   int>                                      // theta
  index_type;

  typedef boost::tuple
  <double,                                   // input to cable
   double,                                   // output to socket 1
   double,                                   // output to socket 2
   double,                                   // voltage angle (degrees)
   double>                                   // nodal price (reduced cost)
  results_type;

  // CREATORS

public:

  Lmp2Nul3B
  (shared_ptr<svif::SolverIf> solver,
   const xeona::DomainMode    commitmentMode);

  virtual
  ~Lmp2Nul3B();

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

}; // class 'Lmp2Nul3B'

#endif // _OPTNODE2_H_

//  end of file

