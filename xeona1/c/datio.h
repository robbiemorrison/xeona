//  XEONA ENERGY SYSTEMS MODELING ENVIRONMENT
//
//  file-create-name : datio.h
//  file-create-date : Mon 01-Oct-2007 12:24 UTC
//  file-initiator   : Robbie Morrison <robbie@actrix.co.nz>
//
//  file-role        : model file read, process, and write functionality / header
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
//  $URL: file:///home/robbie/svn-root/xeona/futz/trunk/xeona1/c/datio.h $

//  HEADER GUARD

#ifndef _DATIO_H_
#define _DATIO_H_

//  LOCAL AND SYSTEM INCLUDES

#include "recset.h"           // record set support
#include "../a/logger_fwd.h"  // some forward declarations from "logger.h"
#include "../c/smart_ptr.h"   // toggle between Boost and TR1 smart pointers

#include <vector>             // STL sequence container

#include <boost/any.hpp>                // safe type heterogeneous storage
#include <boost/filesystem.hpp>         // path objects, iterators, useful operations
#include <boost/logic/tribool.hpp>      // three state boolean
#include <boost/logic/tribool_io.hpp>   // CAUTION: essential for std::boolalpha reporting

//  NAMESPACE DECLARATIONS

using boost::logic::tribool;            // three state boolean
using boost::logic::indeterminate;      // allows 'indeterminate' and 'indeterminate()'

//  CODE

// ---------------------------------------------------------
//  CLASS           : DataIo
// ---------------------------------------------------------
//  Description  : management of model data input and output
//  Role         : initially read and later overwrite the XEM model file
// ---------------------------------------------------------

class DataIo
{
  // LOCAL ENUMERATIONS

private:

  enum FieldWriteDepth                       // controls field writing
    {
      e_notSet        = 0,                   // set on construction
      e_rawString     = 1,                   // write simply echos back the raw string
      e_boundValue    = 2                    // write unlex the bound values
    };

  // DISABLED

private:

  DataIo();                                  // zero-argument constructor
  DataIo(const DataIo& orig);                // copy constructor
  DataIo& operator= (const DataIo& orig);    // copy assignment operator

  // CREATORS

public:

  DataIo
  (RecordSet& recset);                       // need to explicitly supply a record-set

  ~DataIo();                                 // destructor

  // ACCESSORS

  boost::filesystem::path                    // complete (absolute) path
  getModelFilePath();

  unsigned
  getRecordCount();

  // STATIC ACCESSORS

  static
  unsigned
  getTabset();

  // MANIPULATORS (identifiers contain the phrase "model")

  // normally called in the order given

  void
  readModel
  (boost::filesystem::path modelFile);       // model file path

  bool                                       // 'true' is model format and svn rev align
  confirmModelSvnAlign();

#ifdef _XUTEST

  unsigned                                   // returns horizon -- or zero on failure
  locateModelHorizon();                      // looks for 'steps' in 'time-horizon'

#endif // _XUTEST

  int                                        // returns count
  snoopHorizonDetails()                      // looks for various things in 'time-horizon'
    throw (xeona::xem_data_issue);

  void
  processModel();                            // turn raw strings into split strings

  void
  noteModelIsBound();

  void
  updateModelRunTime
  (const std::string& currentSimRet,
   const std::string& currentSimKind,
   const std::string& simulateTime);

  std::vector<shared_ptr<Record> >           // can be an empty vector
  getSubset                                  // subset based on record kind
  (const xeona::RecordKind recKind);         // must come after 'processModel'

  void
  writeModel                                 // defaults to empty path for 'stdout'
  (boost::filesystem::path modelFile = boost::filesystem::path());

  void
  setFieldWriteDepth
  (FieldWriteDepth fwd);                     // normally reset to 'DataIo::e_rawString'

  // UTILITY FUNCTIONS (identifiers often contain the phrase "data")

private:

  bool                                       // returns 'true' on success
  loadDataLine
  (const char* cline);                       // C-string input line (from getline)

  bool
  updateProgramOutput
  (const std::string& recordName,            // sought record name
   const std::string& fieldName,             // sought field name
   const std::string& overwriteStr);         // overwrite if field exists and is output

  void
  writeData                                  // used by 'writeModel'
  (std::ostream& os);

  std::string
  recordKindToLead
  (xeona::RecordKind kind) const;

  std::string
  getFieldValue
  (const shared_ptr<Field> f,
   FieldWriteDepth         fwd = e_notSet);  // default given

  void
  modifyEmacsTabstops
  (std::string& commentLine) const;

  template <typename T>
  T                                          // value of sought field
  getTimeHorizonValue
  (const std::string& fname)                 // name of sought field
    throw (xeona::xem_data_issue);

  // INTERNAL DATA

private:

  RecordSet&                 d_records;
  boost::filesystem::path    d_model;             // model file complete path

  shared_ptr<Record>         d_curRecord;         // current record
  shared_ptr<Field>          d_curField;          // current field

  FieldWriteDepth            d_fieldWriteDepth;   // controls how field values are derived
  unsigned                   d_lineCount;         // model file line count
  unsigned                   d_recCount;          // pushed-back record count
  bool                       d_flag_initialRecord;
  bool                       d_flag_endMarker;

  // STATIC DATA

private:

  static unsigned            s_tabset;            // tab setting for aligning "<>" output
  static logga::spLogger     s_logger;            // shared_ptr to single logger object

};

#endif // _DATIO_H_

//  end of file

