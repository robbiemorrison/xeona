//  XEONA ENERGY SYSTEMS MODELING ENVIRONMENT
//
//  file-create-name : xemtree.cc
//  file-create-date : Mon 07-Nov-2011 10:20 UTC
//  file-initiator   : Robbie Morrison <robbie@actrix.co.nz>
//
//  file-role        : XEM file parsing code / implementation
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
//  $URL: file:///home/robbie/svn-root/xeona/futz/trunk/bin/xgrab-src/xemtree.cc $
//
//  GENERAL NOTES FOR THIS FILE

//  AD-HOC NOTES

//  LOCAL AND SYSTEM INCLUDES

#include "xemtree.h"          // companion header for this file (place first)

#include "utils.h"            // support utilities
#include "common.h"           // common definitions for project (place last)

#include <deque>              // STL sequence container, double-ended vector
#include <fstream>            // file-based io
#include <iomanip>            // setw() and family
#include <iostream>           // standard io
#include <sstream>            // string-streams
#include <string>             // C++ strings
#include <vector>             // STL sequence container

#include <boost/foreach.hpp>  // BOOST_FOREACH iteration macro
#include <boost/format.hpp>   // printf style formatting

#include <boost/algorithm/string.hpp>   // string recasing, trimming, splitting

// ---------------------------------------------------------
//  CLASS           : Field
// ---------------------------------------------------------
//  Description  : holds in-data and out-data information
//  Role         : key/value abstraction
//  Techniques   : enums, 'Boost.String_Algo'
//  Status       : complete
// ---------------------------------------------------------

Field::Field(const Kind         kind,
             const std::string& line) :
  d_kind(kind),
  d_key(""),
  d_value(""),
  d_units(""),
  d_length(0),
  d_finalized(false)
{
  parseFieldLine(line);
}

Field::Kind Field::kind()       const { return d_kind;      }
std::string Field::identifier() const { return d_key;       }
std::string Field::value()      const { return d_value;     }
std::string Field::units()      const { return d_units;     }
int         Field::length()     const { return d_length;    }
bool        Field::finalized()  const { return d_finalized; }

Field::Kind Field::convertKind(const std::string& kind)
{
  if      ( kind == ">" ) return e_inData;
  else if ( kind == "<" ) return e_outData;
  else
    {
      std::stringstream oss;
      oss << "invalid field kind supplied: " << kind;
      throw codingError(FUNC, oss.str());
    }
}

std::string Field::interpretKind(const Kind& kind)
{
  switch ( kind )
    {
    case e_inData:   return ">";
    case e_outData:  return "<";
    case e_notKnown: return "(not known)";
    default: throw codingError(FUNC, "switch case not considered");
    }
}

void Field::parseFieldLine(const std::string& line)
{
  // expecting something like: "   key-name [units] X  > value"
  std::string buff1(line);                   // to remove constness
  // split on angle separator and check integrity
  std::vector<std::string> output1;
  boost::split(output1, buff1, boost::is_any_of("<>"));
  const int split1 = output1.size();
  if ( split1 != 2 )
    {
      std::ostringstream oss;
      oss << "split not 2 as required: " << split1;
      throw badXem(FUNC, oss.str());
    }
  std::string buff2 = boost::trim_copy(output1[0]);
  d_value           = boost::trim_copy(output1[1]);
  // split on space
  std::vector<std::string> output2;
  boost::split(output2, buff2, boost::is_any_of(" "));
  const int split2 = output2.size();
  if ( split2 == 0 )
    {
      std::ostringstream oss;
      oss << "split not 1 or more as required: " << split2;
      throw badXem(FUNC, oss.str());
    }
  d_key  = boost::trim_copy(output2[0]);
  // attempt to recover units of measurement, must be enclosed in "[]"
  if ( split2 > 1 )
    {
      const std::string posunits = boost::trim_copy(output2[1]);
      if ( boost::starts_with(posunits, "[") && boost::ends_with(posunits, "]") )
        {
          d_units = posunits;
        }
    }
  // identify length, split on space
  std::vector<std::string> output3;
  boost::split(output3, d_value, boost::is_any_of(" "));
  d_length = output3.size();

  // finish up
  d_finalized = true;
  return;
}

// ---------------------------------------------------------
//  CLASS           : Record
// ---------------------------------------------------------
//  Description  : holds fields
//  Role         : record abstraction
//  Techniques   : enums, 'Boost.String_Algo'
//  Status       : complete
// ---------------------------------------------------------

Record::Record(const std::string& line) :
  d_finalized(false),
  d_kind(e_notKnown),
  d_identifier(""),
  d_fields()
{
  parseRecordLine(line);
}

void Record::insertField(shared_ptr<Field> field)
{
  if ( ! d_finalized ) d_fields.push_back(field);
}

bool         Record::isModelEnd() const { return ( d_kind == Record::e_modelEnd ); }
Record::Kind Record::kind()       const { return d_kind;                           }
std::string  Record::identifier() const { return d_identifier;                     }

void Record::setFinalized() { d_finalized = true; }

shared_ptr<Field> Record::findField(const std::string fiKey)
{
  BOOST_FOREACH( shared_ptr<Field> f, d_fields )
    {
      if ( f->identifier() == fiKey )
        {
          return f;
        }
    }
  throw noFld(FUNC, fiKey);
}

Record::Kind Record::convertKind(const std::string& kind)
{
  if      ( kind == "note"      ) return e_note;
  else if ( kind == "program"   ) return e_program;
  else if ( kind == "entity"    ) return e_entity;
  else if ( kind == "model-end" ) return e_modelEnd;
  else throw badRecKind(FUNC, kind);
}

std::string Record::interpretKind(const Kind kind)
{
  switch ( kind )
    {
    case e_note:     return "note";
    case e_program:  return "program";
    case e_entity:   return "entity";
    case e_modelEnd: return "model-end";
    case e_notKnown: return "(not known)";
    default: throw codingError(FUNC, "switch case not considered");
    }
}

void Record::parseRecordLine(const std::string& line)
{
  // expecting something like: "entity.teas-technology-a01"

  // split on dot separator and check integrity
  std::vector<std::string> output;
  boost::split(output, line, boost::is_any_of("."));
  const int split = output.size();
  switch ( split )
    {
    case 2:
      d_identifier = output[1];
      // CAUTION: no "break" is correct
    case 1:
      d_kind = Record::convertKind(output[0]);
      return;
    default:
      std::stringstream oss;
      oss << "split not valid: " << split;
      throw codingError(FUNC, oss.str());
    }
}

// ---------------------------------------------------------
//  CLASS           : RecordSet
// ---------------------------------------------------------
//  Description  : holds records
//  Role         : 'xeona' model abstraction
//  Techniques   : enums, 'Boost.String_Algo'
//  Status       : complete
// ---------------------------------------------------------

RecordSet::RecordSet() :
  d_records(),
  d_record(),
  d_finalized(false),
  d_wasRun(false),
  d_programCount(0),
  d_entityCount(0),
  d_inDataCount(0),
  d_outDataCount(0)
{ }

bool RecordSet::wasRun()   const { return d_wasRun;         }
int  RecordSet::size()     const { return d_records.size(); }
int  RecordSet::entities() const { return d_entityCount;    }
int  RecordSet::programs() const { return d_programCount;   }
int  RecordSet::inDatas()  const { return d_inDataCount;    }
int  RecordSet::outDatas() const { return d_outDataCount;   }

void RecordSet::parseLine(const std::string& line)
{
  // empty and blank lines should not have been stored so they
  // are not considered here -- the lines should also have been
  // right trimmed but not left trimmed

  if ( d_finalized )
    {
      // do nothing is correct, model-end has been reached
    }
  else if ( boost::equals(line, "model-end") )
    {
      if ( d_record ) d_record->setFinalized();   // current record finalized
      d_finalized = true;                         // record set finalized
    }
  else if ( boost::equals(line, "note") )
    {
      // do nothing is correct
    }
  else if ( boost::starts_with(line, "program") )
    {
      if ( d_record ) d_record->setFinalized();   // previous record finalized
      shared_ptr<Record> record(new Record(line));
      d_record = record;                          // update the currently held record
      insertRecord(record);
      ++d_programCount;
    }
  else if ( boost::starts_with(line, "entity") )
    {
      if ( d_record ) d_record->setFinalized();   // current record finalized
      shared_ptr<Record> record(new Record(line));
      d_record = record;                          // update the currently held record
      insertRecord(record);
      ++d_entityCount;
    }
  else if ( boost::contains(line, "<")
            &&
            boost::contains(line, ">") )
    {
      // do nothing is correct, angle brackets pairs are acceptable
    }
  else if ( boost::contains(line, ">")
            &&
            ! boost::starts_with(boost::trim_copy(line), "#") )  // skip if disabled
    {
      shared_ptr<Field> field(new Field(Field::e_inData, line));
      d_record->insertField(field);
      ++d_inDataCount;
    }
  else if ( boost::contains(line, "<")
            &&
            ! boost::starts_with(boost::trim_copy(line), "#") )  // skip if disabled
    {
      shared_ptr<Field> field(new Field(Field::e_outData, line));
      d_record->insertField(field);
      ++d_outDataCount;
    }
  else if ( boost::starts_with(line, "#"))        // disabled record encountered
    {
      if ( d_record ) d_record->setFinalized();   // no further inserts now possible
    }
  else
    {
      // do nothing is correct
    }
}

std::string RecordSet::value(const std::string& fqfname) const
{
  shared_ptr<Field> field = this->field(fqfname);      // CAUTION: 'this' pointer required
  if ( ! field ) return "";
  const std::string value = field->value();
  return value;
}

std::string RecordSet::units(const std::string& fqfname) const
{
  shared_ptr<Field> field = this->field(fqfname);      // CAUTION: 'this' pointer required
  if ( ! field ) return "";
  const std::string units = field->units();
  return units;
}

Field::Kind RecordSet::kind(const std::string& fqfname) const
{
  shared_ptr<Field> field = this->field(fqfname);      // CAUTION: 'this' pointer required
  if ( ! field ) return Field::e_notKnown;
  const Field::Kind fkind = field->kind();
  return fkind;
}

shared_ptr<Field> RecordSet::field(const std::string& fqfname) const
{
  // split on dot separator and check integrity
  std::vector<std::string> output;
  boost::split(output, fqfname, boost::is_any_of("."));
  const int split = output.size();
  if ( split != 3 )
    {
      std::ostringstream oss;
      oss << "split not 3 as required: " << split;
      throw badXem(FUNC, oss.str());
    }

  // recover values
  const std::string  reckind = output[0];
  const std::string  recId   = output[1];
  const std::string  fiKey   = output[2];
  const Record::Kind recKind = Record::convertKind(reckind);
  shared_ptr<Field>  field   = findRecordField(recKind, recId, fiKey);
  return field;
}

bool RecordSet::determineWasRun(const std::string& fqf)
{
  const std::string value = this->value(fqf);     // CAUTION: 'this' pointer required
  if ( value.empty() ) d_wasRun = false;
  else                 d_wasRun = true;
  Deport::log(FUNC, 1, "was run", d_wasRun);
  return d_wasRun;
}

void RecordSet::insertRecord(shared_ptr<Record> record)
{
  d_records.push_back(record);
}

std::string RecordSet::findValue(const Record::Kind kind,
                                 const std::string  recId,
                                 const std::string  fiKey) const
{
  shared_ptr<Field> field = findRecordField(kind, recId, fiKey);
  if ( field ) return field->value();
  else         return "";
}

shared_ptr<Field> RecordSet::findRecordField(const Record::Kind kind,
                                             const std::string  recId,
                                             const std::string  fiKey) const
{
  shared_ptr<Record> record = findRecord(kind, recId);
  if ( record ) return record->findField(fiKey);
  else          return shared_ptr<Field>();
}

shared_ptr<Record> RecordSet::findRecord(const Record::Kind kind,
                                         const std::string  recId) const
{
  BOOST_FOREACH( shared_ptr<Record> r, d_records )
    {
      if ( r->kind() == kind && r->identifier() == recId )
        {
          return r;
        }
    }
  throw noRec(FUNC, Record::interpretKind(kind) + "." + recId);
}

// ---------------------------------------------------------
//  CLASS           : Text
// ---------------------------------------------------------
//  Description  : slurps and stores text from text file
//  Role         : bridge between text file and record set
//  Techniques   : C++ file io
//  Status       : complete
// ---------------------------------------------------------

Text::Text() : d_lines() { }

int Text::slurp(const std::string filename)
{
  Deport::log(FUNC, 1, "filename", filename);
  if ( ! d_lines.empty() )
    {
      std::stringstream oss;
      oss << "lines vector not empty: " << d_lines.size();
      throw codingError(FUNC, oss.str());
    }
  Deport::log(FUNC, 1, "will now attempt file open");
  std::string line;                          // line shuttle
  std::ifstream textfile(filename.c_str());
  if ( textfile.is_open() )
    {
      while ( textfile.good() )
        {
          std::getline (textfile, line);
          loadLine(line);
        }
      textfile.close();
      return d_lines.size();
    }
  else
    {
      throw noXem(FUNC, filename);
    }
}

int  Text::size()  const { return d_lines.size();  }
bool Text::empty() const { return d_lines.empty(); }

std::string Text::pop()
{
  if ( empty() )                             // poor coding if this is 'true'
    {
      throw codingError(FUNC, "invalid request to pop empty vector");
    }
  const std::string front = d_lines.front();
  d_lines.pop_front();
  return front;
}

void Text::loadLine(const std::string& line)
{
  std::string buff(line);                    // to remove constness
  boost::trim_right(buff);                   // any left padding is significant
  if ( buff.empty() ) return;
  d_lines.push_back(buff);                   // copied in
}

//  end of file

