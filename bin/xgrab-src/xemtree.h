//  XEONA ENERGY SYSTEMS MODELING ENVIRONMENT
//
//  file-create-name : xemtree.h
//  file-create-date : Mon 07-Nov-2011 10:20 UTC
//  file-initiator   : Robbie Morrison <robbie@actrix.co.nz>
//
//  file-role        : XEM file parsing code / header
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
//  Copyright : This software is copyright (c) 2007 - 2011 Robbie Morrison.
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
//  $URL: file:///home/robbie/svn-root/xeona/futz/trunk/bin/xgrab-src/xemtree.h $
//
//  GENERAL NOTES FOR THIS FILE

//  HEADER GUARD

#ifndef _XEMTREE_H_
#define _XEMTREE_H_

//  LOCAL AND SYSTEM INCLUDES

#include "smart_ptr.h"        // switch easily between TR1 and Boost smart pointers
#include "utils.h"            // support utilities
#include "common.h"           // common definitions for project (place last)

#include <deque>              // STL sequence container, double-ended vector
#include <iostream>           // standard io
#include <string>             // C++ strings
#include <vector>             // STL sequence container

//  FORWARD (PARTIAL) DECLARATIONS

class RecordSet;                             // for the 'dump' friendships

//  CODE

// ---------------------------------------------------------
//  CLASS           : Field
// ---------------------------------------------------------
//  Description  : holds in-data and out-data information
//  Role         : key/value abstraction
//  Techniques   : enums, 'Boost.String_Algo'
//  Status       : complete
// ---------------------------------------------------------

class Field
{

  friend int dump(std::vector<std::string>&,
                  RecordSet,
                  const bool,
                  const bool,
                  const bool,
                  const std::string&);

public:

  enum Kind
    {
      e_notKnown   = 0,
      e_inData     = 1,
      e_outData    = 2
    };

public:

  Field(const Kind         kind,
        const std::string& line);

  Kind        kind()       const;
  std::string identifier() const;
  std::string value()      const;
  std::string units()      const;
  int         length()     const;
  bool        finalized()  const;

  static Kind        convertKind(const std::string& kind);
  static std::string interpretKind(const Kind& kind);

private:

  void parseFieldLine(const std::string& line);

private:

  Kind           d_kind;
  std::string    d_key;
  std::string    d_value;
  std::string    d_units;
  int            d_length;
  bool           d_finalized;

};

// ---------------------------------------------------------
//  CLASS           : Record
// ---------------------------------------------------------
//  Description  : holds fields
//  Role         : record abstraction
//  Techniques   : enums, 'Boost.String_Algo'
//  Status       : complete
// ---------------------------------------------------------

class Record
{

  friend int dump(std::vector<std::string>&,
                  RecordSet,
                  const bool,
                  const bool,
                  const bool,
                  const std::string&);

public:

  enum Kind
    {
      e_notKnown    = 0,
      e_note        = 1,
      e_program     = 2,
      e_entity      = 3,
      e_modelEnd    = 4
    };

public:

  Record(const std::string& line);

  void insertField(shared_ptr<Field> field);

  bool        isModelEnd() const;
  Kind        kind()       const;
  std::string identifier() const;

  void setFinalized();

  shared_ptr<Field> findField(const std::string fiKey);

  static Kind        convertKind(const std::string& kind);
  static std::string interpretKind(const Kind kind);

private:

  void parseRecordLine(const std::string& line);

private:

  bool                               d_finalized;
  Kind                               d_kind;
  std::string                        d_identifier;
  std::vector<shared_ptr<Field> >    d_fields;

};

// ---------------------------------------------------------
//  CLASS           : RecordSet
// ---------------------------------------------------------
//  Description  : holds records
//  Role         : 'xeona' model abstraction
//  Techniques   : enums, 'Boost.String_Algo'
//  Status       : complete
// ---------------------------------------------------------

class RecordSet
{

  friend int dump(std::vector<std::string>&,
                  RecordSet,
                  const bool,
                  const bool,
                  const bool,
                  const std::string&);

public:

  RecordSet();

  bool wasRun()   const;
  int  size()     const;
  int  entities() const;
  int  programs() const;
  int  inDatas()  const;
  int  outDatas() const;

  void parseLine(const std::string& line);

  std::string       value(const std::string& fqfname) const;
  std::string       units(const std::string& fqfname) const;
  Field::Kind       kind (const std::string& fqfname) const;
  shared_ptr<Field> field(const std::string& fqfname) const;

  bool determineWasRun(const std::string& fqf);

private:

  void insertRecord(shared_ptr<Record> record);

  std::string findValue(const Record::Kind kind,
                        const std::string  recId,
                        const std::string  fiKey) const;

  shared_ptr<Field> findRecordField(const Record::Kind kind,
                                    const std::string  recId,
                                    const std::string  fiKey) const;

  shared_ptr<Record> findRecord(const Record::Kind kind,
                                const std::string  recId) const;
private:

  std::vector<shared_ptr<Record> >   d_records;
  shared_ptr<Record>                 d_record;    // current record
  bool                               d_finalized;
  bool                               d_wasRun;
  int                                d_programCount;
  int                                d_entityCount;
  int                                d_inDataCount;
  int                                d_outDataCount;

};

// ---------------------------------------------------------
//  CLASS           : Text
// ---------------------------------------------------------
//  Description  : slurps and stores text from text file
//  Role         : bridge between text file and record set
//  Techniques   : C++ file io
//  Status       : complete
// ---------------------------------------------------------

class Text
{
public:

  Text();

  int                                        // number of lines stored
  slurp
  (const std::string filename);

  int  size()  const;
  bool empty() const;

  std::string pop();

private:

  void loadLine(const std::string& line);

private:

  std::deque<std::string>    d_lines;

};

#endif // _XEMTREE_H_

//  end of file

