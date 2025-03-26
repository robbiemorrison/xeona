//  XEONA ENERGY SYSTEMS MODELING ENVIRONMENT
//
//  file-create-name : optjunc.h
//  file-create-date : Mon 26-Oct-2009 10:26 UTC
//  file-initiator   : Robbie Morrison <robbie@actrix.co.nz>
//
//  file-role        : operations OSPs for junctions / header
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
//  $URL: file:///home/robbie/svn-root/xeona/futz/trunk/xeona1/b/optjunc.h $
//
//  GENERAL NOTES FOR THIS FILE

//  HEADER GUARD

#ifndef _OPTJUNC_H_
#define _OPTJUNC_H_

//  LOCAL AND SYSTEM INCLUDES

#include "../b/optprob.h"     // optimization sub-problem (OSP) and key sub-classes

#include "../a/logger_fwd.h"  // some forward declarations from "logger.h"
#include "../c/smart_ptr.h"   // toggle between Boost and TR1 smart pointers

#include <string>             // C++ strings
#include <sstream>            // string-streams

//  CODE

// ---------------------------------------------------------
//  CLASS           : JncSplit2
// ---------------------------------------------------------
//  Description  : demand splitter lacking exergy destruction or cost formation
//  Role         : for instance, 'JuncDemand2Split'
//  Techniques   : (nothing special)
//  Status       : complete
// ---------------------------------------------------------

class JncSplit2 :
  public JunctionOsp
{
  // TYPEDEFS

public:

  typedef boost::tuple                       // can be in either local or global cols
  <int,                                      // socket (single)
   int,                                      // cable-1
   int>                                      // cable-2
  index_type;

  typedef boost::tuple
  <double>                                   // socket output
  results_type;

  // CREATORS

public:

  JncSplit2
  (shared_ptr<svif::SolverIf> solver,
   const xeona::DomainMode    commitmentMode);

  virtual
  ~JncSplit2();

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

}; // class 'JncSplit2'

// ---------------------------------------------------------
//  CLASS           : JncSplit3
// ---------------------------------------------------------
//  Description  : demand splitter lacking exergy destruction or cost formation
//  Role         : for instance, 'JuncDemand3Split'
//  Techniques   : (nothing special)
//  Status       : complete
// ---------------------------------------------------------

class JncSplit3 :
  public JunctionOsp
{
  // TYPEDEFS

public:

  typedef boost::tuple                       // can be in either local or global cols
  <int,                                      // socket (single)
   int,                                      // cable-1
   int,                                      // cable-2
   int>                                      // cable-3
  index_type;

  typedef boost::tuple
  <double>                                   // socket output
  results_type;

  // CREATORS

public:

  JncSplit3
  (shared_ptr<svif::SolverIf> solver,
   const xeona::DomainMode    commitmentMode);

  virtual
  ~JncSplit3();

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

}; // class 'JncSplit3'

// ---------------------------------------------------------
//  CLASS           : JncJoin2
// ---------------------------------------------------------
//  Description  : demand joiner lacking exergy destruction or cost formation
//  Role         : for instance, 'JuncDemand2SJoin'
//  Techniques   : (nothing special)
//  Status       : complete
// ---------------------------------------------------------

class JncJoin2 :
  public JunctionOsp
{
  // TYPEDEFS

public:

  typedef boost::tuple                       // can be in either local or global cols
  <int,                                      // cable (single)
   int,                                      // socket-1
   int>                                      // socket-2
  index_type;

  typedef boost::tuple
  <double>                                   // cable input
  results_type;

  // CREATORS

public:

  JncJoin2
  (shared_ptr<svif::SolverIf> solver,
   const xeona::DomainMode    commitmentMode);

  virtual
  ~JncJoin2();

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

}; // class 'JncJoin2'

// ---------------------------------------------------------
//  CLASS           : JncSym2
// ---------------------------------------------------------
//  Description  : demand joiner lacking exergy destruction or cost formation
//  Role         : for instance, 'JuncDemand2Sym'
//  Techniques   : (nothing special)
//  Status       : complete
// ---------------------------------------------------------

class JncSym2 :
  public JunctionOsp
{
  // TYPEDEFS

public:

  typedef boost::tuple                       // can be in either local or global cols
  <int,                                      // cable-1
   int,                                      // cable-2
   int,                                      // socket-1
   int>                                      // socket-2
  index_type;

  typedef boost::tuple
  <double>                                   // throughput (combined)
  results_type;

  // CREATORS

public:

  JncSym2
  (shared_ptr<svif::SolverIf> solver,
   const xeona::DomainMode    commitmentMode);

  virtual
  ~JncSym2();

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

}; // class 'JncSym2'

// ---------------------------------------------------------
//  CLASS           : JncSuck
// ---------------------------------------------------------
//  Description  : demand source lacking exergy destruction or cost formation
//  Role         : for instance, 'JuncDemandInvent' (intended for model development only)
//  Techniques   : (nothing special)
//  Status       : complete
// ---------------------------------------------------------

class JncSuck :
  public JunctionOsp
{
  // TYPEDEFS

public:

  typedef boost::tuple                       // can be in either local or global cols
  <int>                                      // cable
  index_type;

  typedef boost::tuple
  <double>                                   // cable input
  results_type;

  // CREATORS

public:

  JncSuck
  (shared_ptr<svif::SolverIf> solver,
   const xeona::DomainMode    commitmentMode);

  virtual
  ~JncSuck();

  // FILL PROBLEM CALLS

public:

  index_type                                 // global cols
  uploadEngineering
  (const double fixedFlow);                  // demand or supply, non-negative, often zero

  // DOWNLOAD SOLUTION CALLS

  results_type
  downloadSolution() const;

  // INSTANCE DATA

private:

  index_type    d_cols;                      // local cols

}; // class 'JncSuck'

// ---------------------------------------------------------
//  CLASS           : JncBlow
// ---------------------------------------------------------
//  Description  : commodity source lacking exergy destruction or cost formation
//  Role         : for instance, 'JuncDemandService' (intended for model development only)
//  Techniques   : (nothing special)
//  Status       : complete
// ---------------------------------------------------------

class JncBlow :
  public JunctionOsp
{
  // TYPEDEFS

public:

  typedef boost::tuple                       // can be in either local or global cols
  <int>                                      // socket
  index_type;

  typedef boost::tuple
  <double>                                   // socket output
  results_type;

  // CREATORS

public:

  JncBlow
  (shared_ptr<svif::SolverIf> solver,
   const xeona::DomainMode    commitmentMode);

  virtual
  ~JncBlow();

  // FILL PROBLEM CALLS

public:

  index_type                                 // global cols
  uploadEngineering
  (const double loBound,                     // lower bound (normally zero)
   const double hiBound);                    // upper bound ('inf' to ignore)

  // DOWNLOAD SOLUTION CALLS

  results_type
  downloadSolution() const;

  // INSTANCE DATA

private:

  index_type    d_cols;                      // local cols

}; // class 'JncBlow'

#endif // _OPTJUNC_H_

//  end of file

