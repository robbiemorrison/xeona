//  XEONA ENERGY SYSTEMS MODELING ENVIRONMENT
//
//  file-create-name : recset.h
//  file-create-date : Tue 09-Oct-2007 17:13 UTC
//  file-initiator   : Robbie Morrison <robbie@actrix.co.nz>
//
//  file-role        : records and fields and also record-sets / header
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
//  $URL: file:///home/robbie/svn-root/xeona/futz/trunk/xeona1/c/recset.h $

//  HEADER GUARD

#ifndef _RECSET_H_
#define _RECSET_H_

//  LOCAL AND SYSTEM INCLUDES

#include "../c/xeona_ptr.h"   // remappable counted pointer which mimics shared_ptr
#include "../c/util1.h"       // free functions which offer general utilities 1
#include "../a/exapp.h"       // application exception classes
#include "../a/logger.h"      // standard logging functionality (as required)
#include "../c/smart_ptr.h"   // toggle between Boost and TR1 smart pointers
#include "../b/entity.h"      // entity base class

#include <ostream>            // output streams
#include <sstream>            // string-streams
#include <stdexcept>          // standard exception classes, runtime_error()
#include <typeinfo>           // run-time type information (RTTI)
#include <vector>             // STL sequence container

#include <boost/variant.hpp>            // safe specified type heterogeneous storage
#include <boost/logic/tribool.hpp>      // three state boolean
#include <boost/logic/tribool_io.hpp>   // CAUTION: essential for std::boolalpha reporting

//  NAMESPACE DECLARATIONS

using boost::logic::tribool;            // three state boolean
using boost::logic::indeterminate;      // allows 'indeterminate' and 'indeterminate()'

using xeona::assign_ptr;                // remappable counted pointer

//  FORWARD (PARTIAL) DECLARATIONS

namespace xeona { extern bool nopro; }       // for function 'wrap<T>::wrapExit()'

//  CODE

// ---------------------------------------------------------
//  ENUM            : xeona::RecordKind
// ---------------------------------------------------------
//  Description  : set of possible record kinds
//  Role         : used during file parsing and writing to control parsing logic
// ---------------------------------------------------------

namespace xeona
{
  enum RecordKind
    {
      e_notRecord = 0,   // default is 0 anyway
      e_note,            // verbatim multi-line note
      e_program,         // program configuration information
      e_entity,          // model-specific entities
      e_end              // end of model marker (not exactly a 'record' as such)
    };

} // namespace xeona

// ---------------------------------------------------------
//  ENUM            : xeona::FieldKind
// ---------------------------------------------------------
//  Description  : set of possible field kinds
//  Role         : used during file parsing to control parsing logic
// ---------------------------------------------------------

namespace xeona
{
  enum FieldKind
    {
      e_notField = 0,
      e_comment,         // comment line (cf remark within the field left side)
      e_input,           // externally supplied data
      e_output           // internally generated data
    };

} // namespace xeona

// ---------------------------------------------------------
//  CLASS           : wrap <>
// ---------------------------------------------------------
//  Description  : wraps a "single" so that it becomes a "non-temporary"
//  Role         : initializing reference data members in entity objects
//  Techniques   : operator T&, one form of the type-conversion operator
//  Status       : complete
//
//  Design notes
//
//      This utility class is used here to "wrap" a type which
//      the compiler has identified as a "temporary" and thereby
//      make it "acceptable" for binding to a reference:
//
//          int init = 10;      // 'init' seen as temporary by the compiler
//          wrap<int> w(init);  // create a class 'wrap' instance
//          // ...
//          int& r = w;   // bind 'w' to non-const reference 'r'
//          int  s = w;   // straight copy assignment, no cast necessary
//          w++;          // r,w = 11, s = 10, init = 10
//          r++;          // r,w = 12, s = 10, init = 10
//          s--;          // r,w = 12, s =  9, init = 10
//
//      The following statements are also usable:
//
//          wrap<int> copy1 = wrap<int>(3);
//          wrap<int> copy2 = w;
//          wrap<int> copy3 = static_cast<wrap<int> >(r);
//          wrap<int> copy4(w);
//
//      This class has the desired effect and no known
//      side-effects -- beyond making the type specifications
//      more complicated than they would otherwise be (for
//      instance, some functions return 'wrap<T>' rather than
//      straight 'T').
//
//      The idea draws on code in Lischner (2003 p119).  Please
//      study example 5-21 ("Binding references to conversion
//      lvalues") for further insights.
//
//      Note that the 'type-conversion operator' is not the same
//      as the 'address-of operator', although the signatures can
//      look quite similar.  The type-conversion operator has no
//      return type, not even 'void' or 'void*'.
//
//  CAUTION: default constructed object usage
//
//      Do NOT try to assign from a zero-argument (default)
//      constructed wrap object.  If you do, you should get a
//      short warning message explaining the problem before the
//      program seg-faults or exits() (depending on the code).
//
//  CAUTION: fully defined in header
//
//      This class is fully defined in the header file to avoid
//      template instantiation issues.
//
//  CAUTION: forward declaration
//
//      The correct way to forward declare this class is:
//
//          template <typename T> class wrap;
//
// ---------------------------------------------------------

template <typename T>
class wrap                                   // just for the type-conversion operator T&
{
public:
  explicit wrap() : d_value() { }            // mostly to member initialize 'd_single'
  explicit wrap(T v) : d_value(new T(v)) { }
  operator T& ()
  {
    if ( ! d_value ) wrapExit();             // else seg-faults on the return statement

    // reporting
    static logga::spLogger logger = logga::ptrLogStream();
    logger->repx(logga::adhc, "wrap return dereferenced d_value", *d_value);

    return *d_value;                         // return aliasable value
  }
private:
  void wrapExit()                            // warning message and exit
    throw(std::exception,                    // exception specification
          xeona::empty_wrap);
  shared_ptr<T>    d_value;                  // pointer usage is possibly essential [1]
};

// [1] pointer usage: things appear to work okay if 'd_value' is
// a straight T, but then I didn't test all the copy construction
// and copy assignment permutations.  That said, the shared_ptr
// design just felt like the more robust option in these other
// circumstances.  Moreover, the pointer approach default
// constructs an unusable object (an empty smart pointer),
// whereas the full type approach default constructs a 'T' with a
// usable but spurious value (often zero).

template <typename T>
void
wrap<T>::wrapExit()
  throw(std::exception,                      // exception specification
        xeona::empty_wrap)
{
  static logga::spLogger logger = logga::ptrLogStream();
  logger->repx(logga::dbug, "entering member function", "");
  const std::string type = xeona::demangle(typeid(T).name());
  if ( xeona::nopro == false )               // meaning option '--krazy' not applied
    {
      logger->repx(logga::warn, "will throw xeona::empty_wrap", "");
      throw xeona::empty_wrap(type);
    }
  else
    {
      logger->repx(logga::dbug, "continuing under --krazy, type", type);
    }
}

// ---------------------------------------------------------
//  TYPEDEF         : single_type
//  TYPEDEF         : timeseries_type
// ---------------------------------------------------------
//  Description  : enable multiple value types to be stored together
//  Role         : used to hold record data also mirrored with entities
//  Techniques   : Boost.Variant library
//
//  Design notes
//
//      As described elsewhere, a "single" value has one element,
//      whereas a "timeseries" value has a "horizon" of elements.
//      In terms of algebra, these would map roughly to 'scalar'
//      and 'vector'.
//
//      The "single_type" requires the 'wrap' class template so
//      that it can be used to initialize reference data members
//      in entity objects.  See class 'wrap' for more details.
//
// ---------------------------------------------------------

typedef boost::variant<
  wrap<int>,
  wrap<double>,
  wrap<bool>,
  wrap<tribool>,
  wrap<std::string>
> single_type;

typedef boost::variant<
  shared_ptr<std::vector<int> >,
  shared_ptr<std::vector<double> >,
  shared_ptr<std::vector<bool> >,
  shared_ptr<std::vector<tribool> >,
  shared_ptr<std::vector<std::string> >
> timeseries_type;

// ---------------------------------------------------------
//  CLASS           : Field
// ---------------------------------------------------------
//  Description  : holds field information, broken into various parts
//  Role         : used by Record objects
// ---------------------------------------------------------

#ifdef _XUTEST                                // normally defined when unit testing
namespace unittest { template <typename T> void unlexTestSingle(const T input); }
#endif // _XUTEST

class Field
{
  // FRIENDS

  friend class Record;                       // for access to private utility functions

#ifdef _XUTEST
  friend int main(int, char**);              // unit test access to private functions
  template <typename T> friend void unittest::unlexTestSingle(const T); // CAUTION: [1]
#endif // _XUTEST

  // [1] function template friendship: note unusual syntax, for
  // which Lischner (2003 pp188-190) provides a full description.

  //  DISABLED

private:

  Field(const Field& orig);                  // copy constructor
  Field& operator= (const Field& orig);      // copy assignment operator

  // CREATORS

public:

  Field();

  // MANIPULATORS -- for loading data

public:

  void addName(std::string fieldname);
  void addKind(xeona::FieldKind kind);
  void addEnabled(tribool enabled);
  void addUnits(std::string units);
  void addRemark(const std::string& remark);
  void addRawStr(const std::string& rawStr);
  void addSplitStr(const std::vector<std::string>& splitStr);

  template <typename T> void addSingle(wrap<T>& single);
  template <typename T> void addTimeseries(shared_ptr<std::vector<T> > tseries);

// ACCESSORS

public:

  std::string                      getName()     const;     // d_name
  xeona::FieldKind                 getKind()     const;     // d_kind
  tribool                          getEnabled()  const;     // d_enabled
  std::string                      getUnits()    const;     // d_units
  std::string                      getRemark()   const;     // d_remark

  std::string                      getRawStr()   const;     // d_rawStr
  const std::vector<std::string>&  getSplitStr() const;     // d_splitStr (const ref)
  int                              getCount()    const;     // d_splitStr.size()
  bool                             isEmpty()     const;     // d_splitStr.empty()

  // SPECIAL MANIPULATORS -- deploy with care BEFORE the raw string processing has occured

public:

  void overwriteRawStr(const std::string& rawStr);

  // ACCESSORS -- after binding has occurred

  const std::string              getSingle();
  const std::vector<std::string> getTimeseries()
    throw(xeona::empty_field_on_write);      // exception specification
  const std::string              getTimeseries(const std::string& sep);

  // MANIPULATORS -- for use by friendly Record objects

private:

  void
  splitRawStr()                              // process into split string
    throw(std::exception,                    // exception specification
          xeona::short_timeseries);

  template <typename T>
  void
  lexValue                                   // convert from string to nominated type
  (shared_ptr<std::vector<T> > output);

  void
  unlexSingle                                // convert back to split string vector
  (const single_type& input);                // CAUTION: & is necessary

  void
  unlexTimeseries                            // convert back to split string vector
  (const timeseries_type input);

  // INTERNAL DATA

private:

  std::string                 d_name;        // name of field
  xeona::FieldKind            d_kind;        // either input, output, or comment
  tribool                     d_enabled;     // indeterminate indicates incomplete
  std::string                 d_units;       // physical units, empty indicates dim'less
  std::string                 d_remark;      // in-line remark
  std::string                 d_rawStr;      // value as raw string (not used)
  std::vector<std::string>    d_splitStr;    // value as split string

  // safe storage of specified types using the Boost.Variant library

  single_type                 d_single;
  timeseries_type             d_timeseries;

  // STATIC DATA

private:

  static logga::spLogger      s_logger;      // shared_ptr to single logger object

}; // class 'Field'

// HEADER-SIDE IMPLEMENTATIONS

template <typename T>
void
Field::addSingle
(wrap<T>& single)
{
  d_single = single;
}

template <typename T>
void
Field::addTimeseries
(shared_ptr<std::vector<T> > tseries)
{
  d_timeseries = tseries;
}

// ---------------------------------------------------------
//  CLASS           : Record
// ---------------------------------------------------------
//  Description  : holds a record, comprising metainfo and fields
//  Role         : used by RecordSet
// ---------------------------------------------------------

class Record
{
  // FRIENDS

  friend class RecordSet;                    // for access to private utility functions

#ifdef _XUTEST
  friend class Entity_UT;                    // for 'recset.ut0.cc'
#endif // _XUTEST

  // DISABLED

private:

  Record(const Record& orig);                // copy constructor
  Record& operator= (const Record& orig);    // copy assignment operator

  // CREATORS

public:

  Record();

  ~Record();

  // MANIPULATORS -- for loading data

public:

  void addIdentifier(std::string recordId);
  void addKind(xeona::RecordKind recordKind);
  void addEnabled(tribool recordEnabled);
  void addComment(const std::string& comment);
  void addField(shared_ptr<Field> field)
    throw(std::exception, xeona::xem_data_issue);      // exception specification

  // ACCESSORS

  const std::string                       getIdentifier() const;      // d_identifier
  xeona::RecordKind                       getKind()       const;      // d_kind
  tribool                                 getEnabled()    const;      // d_enabled

  const std::vector<std::string>&         getComments()   const;      // d_comments
  const std::vector<shared_ptr<Field> >&  getFields()     const;      // d_fields

  const std::string                       locateClass()   const;

  // BINDING FUNCTIONS -- for use by entities

public:

  template <typename T>                      // T must be supported by single_type
  wrap<T>                                    // see design notes for class 'wrap'
  tieSingle
  (const std::string& fieldname);

  template <typename T>                      // T in {int,double,bool,tribool,std::string}
  wrap<T>                                    // see design notes for class 'wrap'
  tieConstant
  (const T constantValue);

  template <typename T>                      // T must be supported by timeseries_type
  shared_ptr<std::vector<T> >                // only the contained type need be specified
  tieTimeseries
  (const std::string& fieldname)
    throw(xeona::timeseries_not_found);      // exception specification

  template <typename E>                      // CAUTION: defined in header file
  assign_ptr<E>
  lazyLink                                   // gets id, calls 'Entity' factory function
  (const std::string& linkname)
    throw (xeona::lazy_link_fail)
  {
    // locate the required field
    shared_ptr<Field> f;                     // field of interest
    f = locateField(linkname);               // called function warns if not found
    if ( ! f )
      {
        s_logger->repx(logga::warn, "field not found", linkname);
        if ( xeona::nopro == false )         // meaning option '--krazy' not applied
          {
            s_logger->repx(logga::warn, "will throw xeona::lazy_link_fail", "");
            throw xeona::lazy_link_fail("link name not found", linkname);
          }
      }

    // experience shows the both the lazy linking and the write
    // out fails if the string is not properly lexed and stored
    // -- hence the following is identical to the 'tieSingle'
    // code with T = std::string ALBEIT with a different return
    // type.

    // first, lex 'splitStr' as 'std::string'
    shared_ptr<std::vector<std::string> > lexed(new std::vector<std::string>());
    f->lexValue<std::string>(lexed);         // load temp with type T values

    // then, create an aliasable single as in 'tieSingle'
    std::string value = lexed->front();      // grab the first and only element
    wrap<std::string> single(value);         // create "wrapped" form
    f->addSingle<std::string>(single);       // copy assign by reference to the recordset

    // do not return 'single' here as we need a entity (smart)
    // pointer and not a T reference!

    const std::string id = value;            // CAUTION: 'getRawStr()' returns quoted form

    // reporting
    s_logger->repx(logga::dbug, "linkname (key)", linkname);
    s_logger->repx(logga::dbug, "recovered id (value)", id);

    // CAUTION: the 'makeLinkEntity' call needs "entity.h" 'hash-include'

    // finally invoke the factory function and return
    return Entity::makeLinkEntity<E>(id);

  } // member function 'lazyLink<>'

  // MANIPULATORS -- for use by friendly RecordSet objects

private:

#ifdef _XUTEST                               // .. and also for use by unit testd
public:                                      // CAUTION: access specifier in 'hash-ifdef'
#endif // _XUTEST

  const shared_ptr<Field>                    // returns empty shared_ptr if not found
  locateField
  (const std::string& fieldname) const;

  const shared_ptr<Field>                    // returns empty shared_ptr if not found
  locateFieldQuietly                         // non-logging version of above
  (const std::string& fieldname) const;

  void
  processFields();                           // cycles thru splitRawStr() calls

public:

  // MANIPULATORS -- for unit tests which make entities but do not read from a model file

  void
  hackUnitTestRecord                         // CAUTION: hollow function unless '_XUTEST'
  (const std::string builtinRemark,
   const int         steps = 2);             // note default

  // INTERNAL DATA

private:

  std::string                       d_identifier; // unique (tested later)
  xeona::RecordKind                 d_kind;       // enumeration
  tribool                           d_enabled;    // indeterminate indicates incomplete
  std::vector<shared_ptr<Field> >   d_fields;     // principal data
  std::vector<std::string>          d_comments;   // vector supports multi-line comments

  // STATIC DATA

private:

  static logga::spLogger            s_logger;     // shared_ptr to single logger object

}; // class 'Record'

// ---------------------------------------------------------
//  CLASS           : RecordSet
// ---------------------------------------------------------
//  Description  : holds a set of Records
//  Role         : provide a single point of entry
// ---------------------------------------------------------

class RecordSet
{
  // LOCAL ENUMERATIONS

public:

  enum Status
    {
      e_empty        = 0,                    // as constructed
      e_loaded       = 1,                    // the field value string is raw
      e_processed    = 2,                    // the field value string is processed
      e_bound        = 3,                    // the fields are typed and bound
      e_written      = 4                     // the records have been written out
    };

  // CREATORS

public:

  RecordSet();

  ~RecordSet();

  // MANIPULATORS -- for loading data and such

  void setStatus(Status status);
  void addRecord(shared_ptr<Record> record)
    throw(std::exception, xeona::xem_data_issue);      // exception specification
  void processRecords();

  // BOUND SUBSETS

  std::vector<shared_ptr<Record> >           // may be an empty vector
  copySet();

  std::vector<shared_ptr<Record> >           // may be an empty vector
  copySubset
  (const xeona::RecordKind recKind);

  // ACCESSORS

  Status                                  getStatus();
  unsigned                                getCount();
  const std::vector<shared_ptr<Record> >& getRecords();

  const shared_ptr<Record>
  locateRecord
  (const std::string& recordIdentifier) const;

  const shared_ptr<Record>
  locateRecordQuietly
  (const std::string& recordIdentifier) const;

  const bool
  existsEnabledRecord
  (const std::string& recordIdentifier) const;

  const shared_ptr<Field>
  locateRecordAndField
  (const std::string& recordIdentifier,
   const std::string& fieldname) const;

#ifdef _XUTEST // unit testing only

  void
  dump
  (std::string marker = "---");              // optional marker to insert

#endif // _XUTEST

  // MANIPULATORS -- for external use

  bool                                       // 'true' if changes made
  modifySteps                                // called from unit 'c/simcall'
  (const int steps);                         // required 'steps', probably unity

  bool                                       // 'true' if changes made
  modifyHours                                // called from unit 'c/simcall'
  (const int hours);                         // required hours

  bool                                       // 'true' if changes made
  modifySecondsHours                         // called from unit 'c/simcall'
  (const int seconds,                        // required seconds
   const int hours);                         // required hours

  // INTERNAL DATA

private:

  Status                              d_status;
  std::vector<shared_ptr<Record> >    d_records;

  // STATIC DATA

private:

  static logga::spLogger              s_logger;     // shared_ptr to single logger object

}; // class 'RecordSet'

#endif // _RECSET_H_

//  end of file

