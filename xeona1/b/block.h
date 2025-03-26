//  XEONA ENERGY SYSTEMS MODELING ENVIRONMENT
//
//  file-create-name : block.h
//  file-create-date : Tue 26-Aug-2008 14:21 UTC
//  file-initiator   : Robbie Morrison <robbie@actrix.co.nz>
//
//  file-role        : abstract block entity / header
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
//  $URL: file:///home/robbie/svn-root/xeona/futz/trunk/xeona1/b/block.h $

//  HEADER GUARD

#ifndef _BLOCK_H_
#define _BLOCK_H_

//  LOCAL AND SYSTEM INCLUDES

#include "../c/stats.h"       // on-the-fly statistical calculations
#include "../b/entity.h"      // entity base class plus lazy linking

#include "../a/logger_fwd.h"  // some forward declarations from "logger.h"
#include "../c/smart_ptr.h"   // toggle between Boost and TR1 smart pointers

#include <string>             // C++ strings
#include <sstream>            // string-streams

//  FORWARD (PARTIAL) DECLARATIONS

class Record;                                // constructor argument

//  CODE

// ---------------------------------------------------------
//  CLASS           : Block (abstract almost-base)
// ---------------------------------------------------------
//  Description  : abstract class for gateway and technical asset entities
//  Role         : step in the inheritance web
//  Techniques   : pure virtual destructor
//  Status       : complete
//
//  Design notes
//
//      Definitions for duty and size
//
//          Duty covers:
//
//              - technical asset level of activity
//              - gateway sales quantity
//
//          Size covers:
//
//              - technical asset nominal capacity
//
// ---------------------------------------------------------

class Block :
  public FullEntity
{
public:

  // DISABLED

private:

  Block();                                   // zero-argument constructor
  Block(const Block& orig);                  // copy constructor
  Block& operator= (const Block& orig);      // copy assignment operator

  // CREATORS

public:

  explicit
  Block
  (const std::string entityId,
   Record&           record);

#ifndef _XUTEST
  virtual
  ~Block () = 0;                             // create abstract class
#else
  virtual
  ~Block ();                                 // allow direct usage in unit tests
#endif // _XUTEST

  // ACCESSORS

public:

  const Statistics<double>&
  getDutyStats() const;

  const Statistics<double>&
  getSizeStats() const;

  // REPORTING AND STATIC REPORTING

  const std::string
  formatStats
  (const std::string& trailingComment = "") const;

  static                                     // CAUTION: note 'static'
  const std::string
  formatStatsHeader();                       // note 'static' cannot be 'const'

  static                                     // CAUTION: note 'static'
  const std::string
  formatStatsFooter();                       // note 'static' cannot be 'const'

  std::string
  reportDutyAndSize() const;                 // for debugging

  // INSTANCE DATA

protected:

  Statistics<double>    d_dutyStats;         // on-the-fly duty statistics
  Statistics<double>    d_sizeStats;         // on-the-fly size statistics

  // STATIC DATA

  static const int      s_indent;            // universal indent
  static const int      s_identifierLength;  // identifier length
  static const int      s_ruleLength;        // dashed line

};

#endif // _BLOCK_H_

//  end of file

