//  XEONA ENERGY SYSTEMS MODELING ENVIRONMENT
//
//  file-create-name : datio.cc
//  file-create-date : Mon 01-Oct-2007 12:24 UTC
//  file-initiator   : Robbie Morrison <robbie@actrix.co.nz>
//
//  file-role        : model file read, process, and write functionality / implementation
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
//  $URL: file:///home/robbie/svn-root/xeona/futz/trunk/xeona1/c/datio.cc $

//  LOCAL AND SYSTEM INCLUDES

#include "datio.h"            // companion header for this file (place first)

#include "../c/recset.h"      // record set support
#include "../b/entity.h"      // entity base class
#include "../c/util1.h"       // free functions which offer general utilities 1
#include "../a/logger.h"      // standard logging functionality (as required)
#include "../c/smart_ptr.h"   // toggle between Boost and TR1 smart pointers
#include ".././common.h"      // common definitions for project (place last)

#include <fstream>            // file-based io
#include <iomanip>            // setw() and family
#include <locale>             // locale specific information
#include <sstream>            // string-streams
#include <string>             // C++ strings
#include <vector>             // STL sequence container

#include <boost/algorithm/string.hpp>   // string recasing, trimming, splitting
#include <boost/any.hpp>                // type heterogeneous storage
#include <boost/cast.hpp>               // numeric_cast<> number to number conversions
#include <boost/foreach.hpp>            // BOOST_FOREACH iteration macro
#include <boost/format.hpp>             // printf style formatting
#include <boost/lexical_cast.hpp>       // lexical_cast<> string to number conversions

// LOCAL DEFINITIONS

// Set the size of the getline data retrieval buffer using the
// integer ::BUFFSIZE_GETLINE -- this can be extremely generous
// because it only persists for the data retrieval period (note
// that 33 * 8760 equals about 0.3MiB, assuming worst case 32
// char decimal numbers within an annual hourly timeseries).

namespace // unnamed
{
#if 0 // 0 = safe, 1 = for testing with an inadequate buffsize and 'datrec.ut1.cc'
  const int BUFFSIZE_GETLINE = 150;   // at time of writing, needs 161 + 1 characters
# warning "getline buffer being deliberately undersized for test purposes"
#else
  const int BUFFSIZE_GETLINE = 1024 * 1024 / 1;   // 1.0MiB
#endif // 0
} // unnamed namespace

//  CODE

// ---------------------------------------------------------
//  CLASS           : DataIo
// ---------------------------------------------------------
//  Description  : management of model data input and output
//  Role         : initially read and later overwrite the XEM model file
//  Status       : complete
//
//  Text file formatting issues
//
//      record  : must have no leading whitespace
//      field   : should have (ideally four) leading whitespaces
//      comment : determined through a process of elimination
//
//  Design notes
//
//      'DataIo' is an unenforced singleton.
//
//      Whitespace characters for Boost.String_algo and STL libraries
//
//          The STL classification function 'std::isspace' is
//          locale-specific.  The Boost.String_algo
//          classification predicates (for instance,
//          'boost::is_space') simply wrap the functionality of
//          the STL classification functions (in this case
//          'std::isspace').  This means identical conclusions
//          must arise irrespective of which library calls are
//          made.
//
//          Hence, the value of 'std::locale' is used to
//          establish the whitespace chars for 'boost::trim_*'.
//
//          For the "C" locale, space characters comprise
//          (Lischner 2003 pp573-574) and are defined (Lischner
//          2003 p6) thus:
//
//              ' '     space
//              '\f'    form feed
//              '\n'    newline
//              '\r'    carriage return
//              '\t'    tab
//              '\v'    vertical tab
//
//          If problems arise, it may be necessary to run 'xeona'
//          under "LANG=C" or "LC_CTYPE=C" to enforce the above
//          definitions.
//
//      Possible design extensions
//
//          Note that 'xeona::e_end' could be treated as a
//          comments-only record kind.
//
//          In retrospect, I would have looked carefully at using
//          the 'Boost.Spirit' parser library for the processing
//          of XEM files.  This would probably have also required
//          an EBNF-style grammar specification for the XEM
//          structure.
//
//      Connectivity information
//
//          Connectivity information (for better or worse) is
//          currently embedded within entity specifications.
//
// ---------------------------------------------------------

//  STATIC DEFINITIONS

unsigned DataIo::s_tabset = xeona::modelAngleIndent;   // set in 'common.cc'

logga::spLogger DataIo::s_logger
  = logga::ptrLogStream();                   // bind logger on definition

// CREATORS

DataIo::DataIo
(RecordSet& recset) :
  d_records(recset),                         // bind by reference
  d_model(),                                 // empty path
  d_curRecord(new Record()),                 // empty current record object
  d_curField(new Field()),                   // empty current field object
  d_fieldWriteDepth(e_notSet),               // default
  d_lineCount(0),                            // zero line count
  d_recCount(0),                             // zero pushed-back record count
  d_flag_initialRecord(true),                // necessary for processing logic
  d_flag_endMarker(false)                    // necessary for integrate verification
{
  s_logger->repx(logga::dbug, "constructor call", "");
}

DataIo::~DataIo()
{
  s_logger->repx(logga::dbug, "destructor call", "");

  unsigned indent = xeona::modelFieldIndent; // set in 'common.cc'
  unsigned tabset = indent + s_tabset + 1;   // determined here
  std::ostringstream put;
  put << "    DataIo destructor call data"             << "\n";
  put << "    record-set count : " << getRecordCount() << "\n";
  put << "    indent (col)     : " << indent           << "\n";
  put << "    tabset (col)     : " << tabset           << "\n";
  s_logger->putx(logga::xtra, put);
}

// ACCESSORS

boost::filesystem::path
DataIo::getModelFilePath()
{
  return d_model;
}

unsigned
DataIo::getRecordCount()
{
  return d_records.getCount();
}

  // STATIC ACCESSORS

unsigned
DataIo::getTabset()
{
  return s_tabset;
}

// MANIPULATORS (public and with identifiers that contain the phrase "model")

// ---------------------------------------------------------
//  MEMBER FUNCTION : DataIo::readModel
// ---------------------------------------------------------
//  Description  : open the file, cycle getlines, call 'loadDataLine'
//  Role         : public point of access for reading model into record set
//  Precondition : model path should be valid
//  Status       : complete
// ---------------------------------------------------------

void
DataIo::readModel
(boost::filesystem::path modelFile)          // model file path
{
  // initial reporting
  s_logger->repx(logga::dbug, "entering member function", "");

  // process object name
  std::string filename  = modelFile.string();          // in operating system format [1]
  const char* cfilename = filename.c_str();            // as C-string for fstream objects

  // CAUTION: [1] boost::filesystem::native_file_string() is now depreciated
  // (despite still being present in the official demo files)

  // open file in read-only mode
  std::ifstream infile(cfilename);           // defaults to text and std::ios::in
  if ( ! infile )
    {
      std::cout << std::flush;
      std::clog
        << "\n"
        << "** unsuccessful ifstream construction on: " << cfilename << "\n"
        << "   (earlier filename and write status checks may have also failed)"
        << "\n"
        << std::endl;
    }

  while ( true )                             // for use with 'break' command
    {
      // for information on getline(), see Josuttis (1999 p608)
      // and Lischner (2003 p531) -- note also the char array
      // initialization, filled with null chars, as recommended
      // by Sutter and Alexandrescu (2005 p37)

      char cline[::BUFFSIZE_GETLINE] = {'\0'};    // C-style buffer for getline()
      infile.getline(cline, ::BUFFSIZE_GETLINE);  // read until newline or BUFFSIZE - 1

      // abandon loop if end of file
      if ( infile.eof() ) break;

      // for information on iostate literals (such as
      // 'std::ios::failbit'), see Lischner (2003 p517) -- and
      // for information on 'std::fstream' member functions (such
      // as 'rdstate'), see Lischner (2003 pp505-508)

      // check stream status, looking for 'failbit', but not 'badbit' or 'eofbit'
      if ( infile.rdstate() & std::ios::failbit )     // CAUTION: '&' is correct
        {
          std::cout << std::flush;
          std::clog
            << "\n"
            << "** unsuccessful getline() operation, std::ios::failbit set"
            << std::endl;
          std::cout << std::flush;
          std::clog
            << "** maximum line length may have exceeded character buffer capacity of : "
            << ::BUFFSIZE_GETLINE - 1
            << std::endl;

#ifdef __unix__ // UNIX-specific code, additional reporting only

          std::string shellCommand                     // ensure sufficient blanks
            = "wc --max-line-length < " + filename;    // std::string version of filename

          std::cout << std::flush;
          std::clog << "** " << shellCommand << " gives : " << std::flush;
          const int ret = system(shellCommand.c_str());
          std::clog << std::endl;                      // additional to 'system' newline
          if ( ret != 0 )                              // unlikely to trigger
            {
              s_logger->repx(logga::warn, "system (shell) returned nonzero", ret);
            }

#else
//  #  warning "UNIX-specific additional reporting being omitted"
#endif // __unix__

          s_logger->repx(logga::kill,
                         "getline() fail, undersize buffer?",
                         ::BUFFSIZE_GETLINE);
          return;
        };

      if ( infile.rdstate() & std::ios::badbit )     // CAUTION: '&' is correct
        {
          s_logger->repx(logga::kill, "getline() fail: system-level error?", "");
          std::cout << std::flush;
          std::clog
            << "\n"
            << "** unsuccessful getline() operation, std::ios::badbit set" << "\n"
            << "   (system-level input/output error?)"
            << "\n"
            << std::endl;
          return;
        };

      // CAUTION: note that due to the way 'getline' works, a
      // line has no final 'newline' char and therefore may be of
      // zero length

      // process the line
      if ( loadDataLine(cline) == false )    // principal call
        {
          s_logger->repx(logga::kill, "loadDataLine call failed", false);
          return;
        }

    } // on block exit, 'cline' releases its memory and 'infile' closes

  d_records.setStatus(RecordSet::e_loaded);  // update status
}

// ---------------------------------------------------------
//  MEMBER FUNCTION : DataIo::confirmModelSvnAlign
// ---------------------------------------------------------
//  Description  : obtain minimum model format and compare against svn revision
//  Role         : used to confirm model/application match
//  Precondition : in-sync svn revision (although not-in-sync tolerant)
//  Status       : complete
// ---------------------------------------------------------

bool                                       // 'true' is model format and svn rev align
DataIo::confirmModelSvnAlign()
{
  // check for not-in-sync svn
  const int currentSvn = xeona::svnRev;
  if ( currentSvn == 0 )                     // codebase not-in-sync
    {
      s_logger->repx(logga::dbug, "unable to confirm model/svn match", "");
      return false;
    }

  // obtain model data
  const shared_ptr<Field> f
    = d_records.locateRecordAndField("data-format", "minimum-svn");
  if ( ! f )
    {
      s_logger->repx(logga::dbug, "unable to confirm model/svn match", "");
      return false;
    }

  // check for mismatch
  const std::string minimumSvnStr = f->getRawStr();
  const int minimumSvn            = boost::lexical_cast<int>(minimumSvnStr);
  if ( minimumSvn > currentSvn )
    {
      std::stringstream oss1;
      oss1 << minimumSvn << " > " << currentSvn;
      s_logger->repx(logga::warn, "model/svn mismatch", oss1.str());
      return false;
    }
  else if ( minimumSvn == currentSvn )
    {
      std::stringstream oss2;
      oss2 << minimumSvn << " = " << currentSvn;
      s_logger->repx(logga::info, "model/svn match confirmed", oss2.str());
      return true;
    }
  else
    {
      std::stringstream oss3;
      oss3 << minimumSvn << " < " << currentSvn;
      s_logger->repx(logga::info, "model/svn backwards compatibility", oss3.str());
      return true;
    }
}

// UTILITY FUNCTIONS (private and with identifiers that often contain the phrase "data")

// ---------------------------------------------------------
//  MEMBER FUNCTION : DataIo::loadDataLine
// ---------------------------------------------------------
//  Description  : process input and load records
//  Role         : called by DataIo::readModel()
//  Status       : complete
//
//  Design notes
//
//      Parses each line on its merits alone and does not
//      consider its wider context, for instance, is it part of a
//      'note' record or an 'entity' record (I suppose this is a
//      feature).
//
//      Some of the record kinds supported here are now obsolete.
//      The fact that parsing code is still included should not
//      present a problem.  For example:
//
//          current  : note, program, entity, model-end
//
//      The output from option '--data' should yield the latest
//      data rules.
//
// ---------------------------------------------------------

bool                                         // returns 'true' on success
DataIo::loadDataLine
(const char* cline)                          // C-string input line (from getline)
{
  // PREAMBLE

  // update the line count
  ++d_lineCount;

  // create some buffers
  std::string              line;             // read in text line
  std::string              buffer;           // general buffer
  std::vector<std::string> split;            // boost::split() results

  // create and initialize some variables
  tribool           lineEnabled = indeterminate;       // 'true' indicates enabled
  xeona::RecordKind recordKind  = xeona::e_notRecord;  // thus record header if reset
  xeona::FieldKind  fieldKind   = xeona::e_notField;   // thus field line if reset

  // LOAD LINE INTO STRING BUFFER

  line = cline;                              // C-string to std::string conversion
  boost::trim_right(line);                   // remove any trailing whitespaces
  const std::string oline = line;            // trimmed original line for comments

  // DEAL WITH TAB CHARS : embedded tabs in XEM files are highly discouraged!

  if ( boost::find_first(line, "\t") )       // iterator_range is convertible to bool
    // CAUTION: must use 'boost::find_first' and not
    // 'boost::find' above (could also use 'boost::contains' predicate)
    {
      s_logger->repx(logga::warn, "tab found in line", d_lineCount);
      if ( ! xeona::DBUG )
        {
          boost::replace_all(line, "\t", " ");    // replace tabs if not _XDEBUG
        }
    }

  // DETERMINE DISABLED STATUS

  // look for and then note and remove the 'xeona::modelDisableChar'
  // disable character if present -- defined as a '#' at the time
  // of writing

  std::string padding;                       // any leading whitespace characters
  std::string content;                       // remainder of line
  content = boost::trim_left_copy(line);     // could be an empty string
  padding = boost::erase_tail_copy(line, content.length());

  if ( boost::starts_with(content, xeona::modelDisableChar) )
    {
      content = content.substr(1);           // disabled, remove leading char
      boost::trim_left(content);             // remove any intervening whitespaces
      line  = padding + content;             // copy back without disable character
      lineEnabled = false;                   // set status
    }
  else if ( ! content.empty() )
    {
      lineEnabled = true;
    }

  // DETERMINE THE LINE TYPE

  // a record must have no leading whitespace whereas a field
  // ideally should -- in addition, a comment is determined later
  // on through a process of elimination

  // CAUTION: read Josuttis (1999 pp495-496) VERY CAREFULLY if
  // you opt for STL string rather that Boost.String_algo search
  // functions -- particularly in regard to return types, failure
  // to find detection, and 'std::string::npos'!

  // first scan for record headers, 'fieldKind' remains zeroed
  if ( boost::equals(line, "note"))                    // equals() returns bool
    recordKind = xeona::e_note;
  else if ( boost::starts_with(line, "entity.") )      // starts_with() returns bool
    recordKind = xeona::e_entity;
  else if ( boost::starts_with(line, "program.") )
    recordKind = xeona::e_program;
  else if ( boost::starts_with(line, xeona::modelEndMarker) )  // defined in 'common.cc'
    recordKind = xeona::e_end;          // a special record kind containing only comments

  // and then scan for field lines, 'recordKind' remains zeroed
  else if ( boost::contains(line, ">") )     // contains() returns bool
    fieldKind = xeona::e_input;
  else if ( boost::contains(line, "<") )
    fieldKind = xeona::e_output;

  // then deal with the special case of comments containing both
  // '<' and '>' (perhaps an email address like: <name@provider>)
  if ( boost::contains(line, "<") && boost::contains(line, ">") )
    fieldKind = xeona::e_notField;

  // next complain if a field is not indented
  if ( fieldKind )                           // CAUTION: this test must be staged
    if ( ! std::isspace(line.at(0)) )        // test first char, requires <locale>
      {
        s_logger->repx(logga::warn, "non-indented field at line", d_lineCount);
      }

  // and finally ignore and discard everything after the
  // 'xeona::modelEndMarker' marker is encountered (note: could
  // later treat these lines as comments in a special record
  // kind, but that special record would need to be pushed back
  // as an afterthought)

  if ( d_flag_endMarker == true )
    {
      recordKind = xeona::e_notRecord;
      fieldKind  = xeona::e_notField;
    }

  // PROCESS A RECORD HEADER LINE

  if ( recordKind )
    {
      // create some variables
      std::string type;                      // record type (as string not enum)
      std::string identifier;                // record identifier

      // if appropriate, close the old record and begin a new record
      if ( d_flag_initialRecord == true )    // first time a record header has been seen
        {
          d_flag_initialRecord = false;      // toggle so future records can be closed
          s_logger->repx(logga::dbug,
                         "toggled initialRecord flag, now",
                         d_flag_initialRecord);
        }
      else
        {
          ++d_recCount;
          std::ostringstream put;
          put << "\n";                       // to cope with 'logga::xtra' reporting
          s_logger->putx(logga::xtra, put);
          put << "  pushing back curRecord " << d_recCount << "\n";
          s_logger->putx(logga::dbug, put);
          s_logger->addSmartBlank(logga::xtra);   // 'logga::xtra' is correct

          d_records.addRecord(d_curRecord);  // STL container elements are copied in
          d_curRecord.reset(new Record());   // reload smart pointer for reuse here
        }

      // note if special 'model-end' record header is encountered
      if ( recordKind == xeona::e_end )
        {
          s_logger->repx(logga::dbug,
                         "record-set end-marker encountered",
                         xeona::modelEndMarker);
          d_flag_endMarker = true;           // reset flag
          s_logger->repx(logga::dbug,
                         "toggled endMarker flag, now",
                         d_flag_endMarker);
        }

      split.clear();
      boost::split(split, line, boost::is_any_of("."));
      switch ( split.size() )
        {
        case 2:
          identifier = split[1];
          // CAUTION: no break because fall-thru is correct
        case 1:
          type       = split[0];
          break;
        default:
          identifier = "(not set)";
          type       = "(not set)";
          s_logger->repx(logga::warn, "record header not split into two", split.size());
        }
      boost::trim(type);
      boost::trim(identifier);

      // complain if record header lacks information
      if ( identifier.empty()
           && recordKind != xeona::e_note         // was && type != "note"
           && type != xeona::modelEndMarker )     // set to "model-end" at time of writing
        {
          s_logger->repx(logga::warn, "incomplete record header at line", d_lineCount);
          std::ostringstream put;
          put << "problem line : '" << oline << "'\n";
          s_logger->putx(logga::dbug, put);
          return false;
        }

      // load new information
      d_curRecord->addIdentifier(identifier);
      d_curRecord->addKind(recordKind);
      d_curRecord->addEnabled(lineEnabled);  // bool value
    }

  // PROCESS A FIELD LINE

  else if ( fieldKind )
    {
      // split into that part to left of '<' or '>' and that part
      // to right of same
      std::string left;                      // left side part
      std::string right;                     // right side part

      split.clear();
      boost::split(split, line, boost::is_any_of("<>"));    // multiple delimiters used
      if ( split.size() >= 3 )                              // should only be two parts
        s_logger->repx(logga::warn, "multi-way field split", split.size());
      left  = split[0];
      right = split[1];

      boost::trim(left);
      boost::trim(right);

      // undertake some integrity checks
      bool flag_problems = false;            // presume okay

      // complain if an enabled input line lacks a value
      if ( fieldKind == xeona::e_input
           && lineEnabled == true
           && right.empty() )
        {
          s_logger->repx(logga::warn, "input field lacks value at line", d_lineCount);
          flag_problems = true;
        }

      // complain if an enabled output line lacks a name
      else if ( fieldKind == xeona::e_output
                && lineEnabled == true
                && left.empty() )
        {
          s_logger->repx(logga::warn, "output field lacks name at line", d_lineCount);
          flag_problems = true;
        }

      // halt if required
      if ( flag_problems == true )
        {
          std::ostringstream put;
          put << "problem line : '" << oline << "'\n";
          s_logger->putx(logga::dbug, put);
          return false;
        }

      // PROCESS THE LEFT PART OF A FIELD LINE

      std::string name;
      std::string units;
      std::string remark;

      split.clear();
      boost::split(split, left, boost::is_any_of("[]"));    // multiple delimiters used
      switch ( split.size() )
        {
        case 1:                              // no units given, try to split again
          split.clear();
          boost::split(split, left, boost::is_any_of(" "), boost::token_compress_on);
          name   = split[0];
          boost::erase_first(left, name);
          remark = left;
          break;
        case 2:                              // units given, but no remark
          name   = split[0];
          units  = split[1];
          break;
        case 3:                              // units and remark given
          name   = split[0];
          units  = split[1];
          remark = split[2];
          break;
        default:
          s_logger->repx(logga::warn, "invalid split, size", split.size());
          break;
        }

      boost::trim(name);
      boost::trim(units);
      boost::trim(remark);

      // determine overall string length and bump 's_tabset' if necessary
      unsigned length = 0;
      if ( ! name.empty()   ) length += name.size()   + 1;
      if ( ! units.empty()  ) length += units.size()  + 1 + 2; // + 2 for the "[]"
      if ( ! remark.empty() ) length += remark.size() + 1;
      length += xeona::modelDisableChar.length()      + 1;     // for any "# "
      if ( length > s_tabset ) s_tabset = length; // then bump

      // load general information
      d_curField->addKind(fieldKind);
      d_curField->addEnabled(lineEnabled);

      // load new left part information
      d_curField->addName(name);
      d_curField->addUnits(units);
      d_curField->addRemark(remark);

      // SIMPLY LOAD THE RIGHT PART OF A FIELD LINE

       // load right part information as it stands
      d_curField->addRawStr(right);          // will be processed later in unit 'recset'

      // FINALIZE FIELD OBJECT

      d_curRecord->addField(d_curField);     // load current field
      d_curField.reset(new Field());         // reset current field
    }

  // PROCESS A COMMENT LINE

  else
    {
      if ( d_flag_initialRecord == false )   // a record header has been seen
        {
          // complain if earlier processed as a disabled line
          if ( lineEnabled == false )
            {
              std::string msg = "comment with leading '";
              msg += xeona::modelDisableChar;
              msg += "' at line" ;
              s_logger->repx(logga::warn, msg, d_lineCount);
            }
          d_curRecord->addComment(line);
        }
      else if ( ! line.empty() )             // meaning substantive pre-model material
        {
          s_logger->repx(logga::warn,
                         "pre-model material ignored",
                         boost::trim_left_copy(line));
          return false;
        }
    }

  // ADDITIONAL REPORTING

  std::string lineType;
  if      ( recordKind ) lineType = "record";
  else if ( fieldKind  ) lineType = "field";
  else                   lineType = "comment";

  std::ostringstream put;
  put << "  "
      << std::setw(8) << std::left  << lineType
      << std::setw(4) << std::right << d_lineCount
      << " : "
      << oline                               // could also use simply 'line'
      << "\n";
  s_logger->putx(logga::xtra, put);

  return true;                     // meaning no return false statements were encountered

} // function 'DataIo::loadDataLine'

// ---------------------------------------------------------
//  MEMBER FUNCTION : locateModelHorizon
// ---------------------------------------------------------

#ifdef _XUTEST

unsigned                                     // returns horizon -- or zero on failure
DataIo::locateModelHorizon()                  // looks for 'steps' in 'time-horizon'
{
  s_logger->repx(logga::xtra, "entering member function", "");

  unsigned steps = 0;                        // setting value

  const shared_ptr<Field> f
    = d_records.locateRecordAndField
    (xeona::timehorizon,                     // the "builtin." has been dropped,
     "steps");                               //   'xeona::timehorizon' is in 'common.cc'
  if ( ! f )
    {
      s_logger->repx(logga::warn, "horizon field not found, shared_ptr", f);
      return 0;
    }
  std::string rawHorizon = f->getRawStr();
  if ( rawHorizon.empty() )
    {
      s_logger->repx(logga::warn, "horizon string empty", "");
      return 0;
    }
  try
    {
      steps = boost::lexical_cast<unsigned>(rawHorizon);
    }
  catch( const boost::bad_lexical_cast& eblc )
    {
      s_logger->repx(logga::warn, "horizon string has bad lexical cast", rawHorizon);
      return 0;
    }
  Entity::setHorizonSteps(steps);
  return Entity::getHorizonSteps();

} // function 'DataIo::locateModelHorizon'

#endif // _XUTEST

// ---------------------------------------------------------
//  MEMBER FUNCTION : DataIo::snoopHorizonDetails
// ---------------------------------------------------------

int
DataIo::snoopHorizonDetails()
  throw (xeona::xem_data_issue)
{
  // initial reporting
  s_logger->repx(logga::xtra, "entering member function", "");

  // preamble
  const int cnt = 6;
  int ret       = 0;                         // C99 says 'true' is unity, 'false' is zero

  // horizon steps
  const int steps = getTimeHorizonValue<int>("steps");
  ret += Entity::setHorizonSteps(steps);

  // horizon interval
  const int interval = getTimeHorizonValue<int>("interval");
  ret += Entity::setHorizonInterval(interval);

  // horizon start hour
  const int startHour = getTimeHorizonValue<int>("start-hour");
  ret += Entity::setHorizonStartHour(startHour);

  // horizon start day
  const int startDay = getTimeHorizonValue<int>("start-day");
  ret += Entity::setHorizonStartDay(startDay);

  // horizon leap year
  const bool leapYear = getTimeHorizonValue<bool>("leap-year");
  ret += Entity::setLeapYear(leapYear);

  // horizon hemisphere
  xeona::Hemisphere hemisphere = xeona::e_hemiNotSet;
  const std::string strHemisphere = getTimeHorizonValue<std::string>("hemisphere");
  if      ( strHemisphere == "\"N\"" ) hemisphere = xeona::e_north;
  else if ( strHemisphere == "\"S\"" ) hemisphere = xeona::e_south;
  else
    {
      s_logger->repx(logga::warn, "hemisphere value not supported", strHemisphere);
      // throw
      if ( xeona::nopro == false )           // meaning option '--krazy' not applied
        {
          s_logger->repx(logga::warn, "about to throw", "xeona::xem_data_issue");
          throw xeona::xem_data_issue("hemisphere value not supported",
                                      "strHemisphere",
                                      strHemisphere);
        }
    }
  ret += Entity::setHemisphere(hemisphere);

  // check return
  if ( ret != cnt )
    {
      s_logger->repx(logga::warn, "setting problems encountered", cnt - ret);
      return 0;
    }
  else
    {
      return cnt;
    }

} // function 'DataIo::snoopHorizonDetails'

// ---------------------------------------------------------
//  MEMBER FUNCTION : DataIo::processModel
// ---------------------------------------------------------

void
DataIo::processModel()
{
  if ( d_records.getStatus() != RecordSet::e_loaded )  // confirm call order
    {
      std::ostringstream oss;
      oss << d_records.getStatus()  << " but wanting " << RecordSet::e_loaded;
      s_logger->repx(logga::warn, "data io call order error, status", oss.str());
      return;
    }
  d_records.processRecords();                          // process records
  d_records.setStatus(RecordSet::e_processed);         // update status
}

// ---------------------------------------------------------
//  MEMBER FUNCTION : DataIo::noteModelIsBound
// ---------------------------------------------------------

void
DataIo::noteModelIsBound()
{
  s_logger->repx(logga::dbug, "about to update d_records status", RecordSet::e_bound);
  d_records.setStatus(RecordSet::e_bound);
}

// ---------------------------------------------------------
//  MEMBER FUNCTION : DataIo::updateModelRunTime
// ---------------------------------------------------------

void
DataIo::updateModelRunTime
(const std::string& currentSimRet,
 const std::string& currentSimKind,
 const std::string& simulateTime)
{
  const std::string processid = boost::lexical_cast<std::string>(xeona::pid);
  const std::string svnrev    = boost::lexical_cast<std::string>(xeona::svnRev);
#if 1 // 1 = with enclosing double-quotes, else 0 = without
  const std::string timestamp = "\"" + xeona::startDate + " " + xeona::startTime + "\"";
#else
  const std::string timestamp = xeona::startDate + " " + xeona::startTime;
#endif // 0

  updateProgramOutput("last-run", "process-id",      processid);
  updateProgramOutput("last-run", "used-svn",        svnrev);
  updateProgramOutput("last-run", "simulate-return", currentSimRet);
  updateProgramOutput("last-run", "timestamp-start", timestamp);
  updateProgramOutput("last-run", "simulate-time",   simulateTime);
  updateProgramOutput("last-run", "run-kind",        currentSimKind);
}

// ---------------------------------------------------------
//  MEMBER FUNCTION : DataIo::getSubset
// ---------------------------------------------------------

std::vector<shared_ptr<Record> >             // can be an empty vector
DataIo::getSubset                            // must come after 'processModel'
(const xeona::RecordKind recKind)            // subset based on record kind
{
  if ( d_records.getStatus() < RecordSet::e_processed )     // confirm call order
    {
      std::ostringstream oss;
      oss << d_records.getStatus()  << " but wanting " << RecordSet::e_processed;
      s_logger->repx(logga::warn, "data io call order error, status", oss.str());
      std::vector<shared_ptr<Record> > empty;
      return empty;
    }
  return d_records.copySubset(recKind);      // process request
}

// ---------------------------------------------------------
//  MEMBER FUNCTION : DataIo::writeModel
// ---------------------------------------------------------

void
DataIo::writeModel
(boost::filesystem::path modelFile)          // defaults to empty path
{
  s_logger->repx(logga::dbug, "entering member function", "");

  // CONFIRM CALL ORDER AND WRITE DEPTH

  switch ( d_records.getStatus() )
    {
    case RecordSet::e_bound:
      d_fieldWriteDepth = e_boundValue;
      s_logger->repx(logga::dbug, "proceeding with deep write", "");
      break;
    case RecordSet::e_processed:
      d_fieldWriteDepth = e_rawString;
      s_logger->repx(logga::dbug, "proceeding with shallow write", "");
      break;
    default:
      std::ostringstream oss;
      oss << d_records.getStatus()  << " but wanting " << RecordSet::e_bound;
      s_logger->repx(logga::warn, "data io call order error, status", oss.str());
      s_logger->repx(logga::warn, "abandoning model write", "");
      return;
    }

  if ( d_fieldWriteDepth == e_notSet )
    {
      s_logger->repx(logga::warn, "write depth not set", d_fieldWriteDepth);
    }

  // MAKE APPROPRIATE WRITE-OUT CALL

  if ( ! modelFile.empty() )
    {
      // process object name
      std::string filename  = modelFile.string();      // in operating system format
      const char* cfilename = filename.c_str();        // as C-string for fstream objects

      // open file in write mode and truncate mode, that is WITHOUT
      // specifying std::ios::app
      //
      // CAUTION: overwrite: the simple act of opening the file under
      // these conditions will cause its contents to be lost
      std::ofstream outfile(cfilename);        // defaults to text and std::ios::out
      if ( ! outfile )
        {
          std::cout << std::flush;
          std::clog
            << "\n"
            << "** unsuccessful ofstream construction on: " << cfilename << "\n"
            << "\n"
            << std::endl;
        }
      else
        {
          s_logger->repx(logga::dbug, "about to overwrite file", modelFile.filename());
          writeData(outfile);
        }
    } // on block exit, 'outfile' closes
  else
    {
      // write to console instead, most probably for testing purposes
      s_logger->repx(logga::dbug, "no path so will write to console", "stdlog");
      writeData(std::clog);
    }

  // UPDATE STATUS

  d_records.setStatus(RecordSet::e_written); // update status
}

// ---------------------------------------------------------
//  MEMBER FUNCTION : DataIo::setFieldWriteDepth
// ---------------------------------------------------------
//  Description  : modifies the field write depth
//  Role         : test purposes
// ---------------------------------------------------------

void
DataIo::setFieldWriteDepth
(FieldWriteDepth fwd)
{
#ifndef _XUTEST
  s_logger->repx(logga::kill, "call intended for unit testing only", "");
#endif // _XUTEST

  FieldWriteDepth prior = d_fieldWriteDepth;
  d_fieldWriteDepth     = fwd;

  std::ostringstream oss;
  oss << prior << " > " << d_fieldWriteDepth;
  s_logger->repx(logga::dbug, "field write depth reset from > to", oss.str());
}

// ---------------------------------------------------------
//  MEMBER FUNCTION : DataIo::updateProgramOutput
// ---------------------------------------------------------
//  Description  : updates various program fields
// ---------------------------------------------------------

bool
DataIo::updateProgramOutput
(const std::string& recordName,              // sought record name
 const std::string& fieldName,               // sought field name
 const std::string& overwriteStr)            // overwrite if field exists and is output
{
  const shared_ptr<Field> f = d_records.locateRecordAndField(recordName, fieldName);
  if ( ! f )
    {
      std::ostringstream oss;
      oss << recordName << " " << fieldName;
      s_logger->repx(logga::dbug, "unable to find field", oss.str());
      return false;
    }
  const xeona::FieldKind kind = f->getKind();
  if ( kind != xeona::e_output )
    {
      std::ostringstream oss;
      oss << xeona::e_output << " | " << kind;
      s_logger->repx(logga::warn, "wrong field kind, wanted | got", oss.str());
      return false;
    }
  f->addRawStr(overwriteStr);
  return true;
}

// ---------------------------------------------------------
//  MEMBER FUNCTION : DataIo::writeData
// ---------------------------------------------------------

void
DataIo::writeData                            // used by 'writeModel'
(std::ostream& os)
{
  s_logger->repx(logga::dbug, "entering member function", "");
  s_logger->repx(logga::dbug, "about to write out model", "");
  s_logger->flush();

  bool early = true;                         // used to control logging

  enum LastField                             // controls input and output field blanklines
    {
      e_unknown = 0,
      e_input   = 1,
      e_output  = 2
    };

  const FieldWriteDepth fwd = d_fieldWriteDepth;  // record for future reference

  const std::string disableChar = xeona::modelDisableChar;
  const unsigned fieldIndent    = xeona::modelFieldIndent;

  os << "\n";                                // leading blank line

  // cycle thru records: const std::vector<shared_ptr<Record> >&
  BOOST_FOREACH( shared_ptr<Record> r, d_records.getRecords() )
    {
      // first pass logging
      if ( early ) s_logger->repx(logga::adhc, "processing early record at", r.get());

      // useful check
      if ( ! r ) s_logger->repx(logga::warn, "record pointer is null or empty", r);

      // grab values
      xeona::RecordKind recKind = r->getKind();
      bool recEnabled           = r->getEnabled();
      std::string recIdentifier = r->getIdentifier();

      // useful reporting
      s_logger->repx(logga::xtra, "record name", recIdentifier);

      // process header line (the entity identifier or similar)
      std::string header;
      if ( recEnabled == false )       header += disableChar + " ";
      header                                  += recordKindToLead(recKind);     // [1]
      if ( ! recIdentifier.empty() )   header += "." + recIdentifier;

      // [1] might be better implemented as a dictionary

      os << header;
      os << "\n";                            // final newline

      // set shallow for non-entities and disabled entities
      if ( recKind == xeona::e_program
           ||
           recEnabled == false )
        d_fieldWriteDepth = e_rawString;     // opt for shallow write
      else
        d_fieldWriteDepth = fwd;             // revert to stored value

      bool flag1 = true;                     // controls blank line squeezing in comments
      bool flag2 = true;                     // controls printing of final blank line

      // cycle thru fields: const std::vector<shared_ptr<Field> >&
      LastField lastField = e_unknown;
      bool addBlank       = true;            // controls blanks between input and output
      BOOST_FOREACH( shared_ptr<Field> f, r->getFields() )
        {
          // first pass logging
          if ( early ) s_logger->repx(logga::adhc, "processing first field at", f.get());

          // useful check
          if ( ! f ) s_logger->repx(logga::warn, "field pointer is null or empty", r);

          // grab values
          std::string      fiName    = f->getName();
          xeona::FieldKind fiKind    = f->getKind();
          tribool          fiEnabled = f->getEnabled();
          std::string      fiUnits   = f->getUnits();
          std::string      fiRemark  = f->getRemark();
          std::string      fiRawStr  = f->getRawStr();

          // process field line left
          std::string left;
          if ( fiEnabled == false )    left += disableChar + " ";
          left                              += fiName;
          if ( ! fiUnits.empty() )     left += " [" + fiUnits + "]";
          if ( ! fiRemark.empty() )    left += " "  + fiRemark;

          // process field line right
          std::string right;

          if ( fiName == "class" )      // meaning class name rather than entity value
            {
              right                         += " >";
              right                         += getFieldValue(f, e_rawString);
            }
          else if ( fiKind == xeona::e_input )
            {
              right                         += " >";
              if ( fiEnabled )
                right                       += getFieldValue(f);      // important call
              else                                                    // shallow recovery
                right                       += getFieldValue(f, e_rawString);
              if ( lastField == e_output ) addBlank = true;
              lastField = e_input;
            }
          else if ( fiKind == xeona::e_output )
            {
              right                         += " <";
              if ( fiEnabled )
                right                       += getFieldValue(f);      // important call
              else                                                    // shallow recovery
                right                       += getFieldValue(f, e_rawString);
              if ( lastField == e_input ) addBlank = true;
              lastField = e_output;
            }
          else
            {
              s_logger->repx(logga::warn, "field kind not class, input, output", fiKind);
            }

          // add blank and update state toggle
          if ( addBlank )
            {
              addBlank = false;
              os << "\n";
            }

          // reduce indent for disabled fields in order to
          // improve alignment (some of the emacs xeona editing
          // major modes, including 'xem-mode', expect this)
          unsigned indent = fieldIndent;
          unsigned tabset = s_tabset;
          const int shift = 2;               // length of "# " string
          if ( fiEnabled == false && indent >= 4 )
            {
              indent = indent - shift;
              tabset = tabset + shift;
            }

          // write out current field
          os << boost::format("%1%") % boost::io::group(std::setw(indent), "")
             << boost::format("%1%") % boost::io::group(std::left, std::setw(tabset),left)
             << boost::format("%1%") % right
             << "\n";                                // final newline

          if ( fiName == "class" ) addBlank = true;

          flag2 = true;
          early = false;                     // reset logging flag

        } // FOREACH shared_ptr<Field>

      // cycle thru multi-line comments: const std::vector<std::string>&
      BOOST_FOREACH( std::string s, r->getComments() )
        {
          if ( s.empty() && flag1 )
            {
              os << s << "\n";
              flag1 = false;
              flag2 = false;                 // used shortly
            }
          else if ( ! s.empty() )
            {
              // modify "tab-stop-list"
              modifyEmacsTabstops(s);        // added after main code tested
              os << s << "\n";
              flag1 = true;
            }
        } // FOREACH std::string

      if ( flag2 )                           // set in comment loop about 60 lines above
        {
          os << "\n";                        // end-of-record blank line as required
        }

    } // FOREACH shared_ptr<Record>

  os << xeona::modelEndMarker;
  os << "\n";                                // end-of-file blank line
  os << std::endl;
}

// ---------------------------------------------------------
//  MEMBER FUNCTION : DataIo::recordKindToLead
// ---------------------------------------------------------

// must align with code in 'DataIo::loadDataLine'

std::string
DataIo::recordKindToLead
(xeona::RecordKind kind) const
{
  std::string lead;
  switch ( kind )
    {
    case xeona::e_note:    lead = "note";    break;
    case xeona::e_program: lead = "program"; break;
    case xeona::e_entity:  lead = "entity";  break;
    case xeona::e_end:     lead = "end";     break;
    default: std::clog << "** coding error 01 in source file " << __FILE__ << std::endl;
    }
  return lead;
}

// ---------------------------------------------------------
//  MEMBER FUNCTION : DataIo::getFieldValue
// ---------------------------------------------------------

std::string                                  // formatted for writing out
DataIo::getFieldValue
(const shared_ptr<Field> f,
 FieldWriteDepth         fwd)                // default 'e_notSet' given
{
  const std::string sep = " ";               // locally hard-coded timeseries separator
  std::string buf;                           // holds the output

  // additional reporting as appropriate
  // YEEK 33 CODE (set by '--yeek')
  //
  // prior to commit r4814, empty output automatically reported
  // "(some problem)" -- this however prevented a possibly
  // incomplete model from being rerun without prior cleaning
  // (for instance using something like: sed 's/(some problem)//')
  //
  // this code makes "" the default and allows the previous
  // behavior available via --yeek 33

  if ( xeona::yeek == 33 )
    {
      buf = xeona::modelStringDelim
        + "(some problem)"
        + xeona::modelStringDelim;

      const std::string name = f->getName();
      s_logger->repx(logga::dbug, "additional reporting follows, yeek", xeona::yeek);
      s_logger->repx(logga::warn, "added (some problem) to output", name);
    }
  else
    {
      buf = xeona::modelStringDelim
        + xeona::modelStringDelim;
    }

  if ( fwd == e_notSet )                     // most likely no second argument given
    {
      fwd = d_fieldWriteDepth;               // thus opt for the current recorded state
    }
  switch ( fwd )
    {
    case e_rawString:                        // shallow write for test purposes only
      {
        buf = f->getRawStr();                // simple grab raw string
      }
      break;

    case e_boundValue:                       // deep write
      {                                      // CAUTION: a local block is necessary [1]
        const int length = f->getCount();
        s_logger->repx(logga::xtra, "field name", f->getName());
        s_logger->repx(logga::xtra, "field length", length);

        // DESIGN CONSIDERATION: as coded, the only thing that
        // distinguishes singles from timeseries is length,
        // meaning that timeseries have a element count value
        // greater than unity.  However, if some other attribute
        // was used (say a trailing + on the field name) then
        // timeseries of length unity could be used and a horizon
        // steps value of one would be valid.  Development
        // testing confirms that next 'switch' block is the ONLY
        // place that code changes would need to be made.

        switch ( length )
          {
          case 0:
            {                                // CAUTION: a local block is necessary [1]
              std::ostringstream oss;
              oss << "details above if report " << logga::xtra;
              s_logger->repx(logga::rankNoData, "bound value is empty", oss.str());
            }
            break;
          case 1:
            {
              buf = f->getSingle();          // const std::string
              s_logger->repx(logga::xtra, "single as string", buf);
            }
            break;
          default:                           // longer
            {
              buf = f->getTimeseries(sep);   // const std::string
              s_logger->repx(logga::xtra, "timeseries as string", buf);
            }
            break;
          } // inner 'switch' statement
      }
      break;

      // [1] else error messages like (which puzzle me):
      // c/datio.cc:1072: error: jump to case label
      // c/datio.cc:1057: error:   crosses initialization of 'const int length'

    default:
      {
        s_logger->repx(logga::warn, "coding error, d_fieldWriteDepth", d_fieldWriteDepth);
      }
      break;
    } // outer 'switch' statement

  if ( ! buf.empty() )
    {
      buf = " " + buf;                       // append a leading space
    }

  return buf;
}

// ---------------------------------------------------------
//  MEMBER FUNCTION : DataIo::modifyEmacsTabstops
// ---------------------------------------------------------
//  Description  : updates 'tab-stop-list' value if present
//  Role         : used on model comments write to update 'emacs' local variables settings
//  Techniques   : Boost.String_algo library
//  Status       : complete
//
//  Design notes
//
//      typical entry: '    tab-stop-list: (04 45 47)'
//
//  CAUTION: late addition
//
//      Be aware that 'modifyEmacsTabstops' was added some months
//      after the original coding and testing of this unit.
//
// ---------------------------------------------------------

void
DataIo::modifyEmacsTabstops
(std::string& commentLine) const
{
  // define trigger phrase
  const std::string emacsSymbol = "tab-stop-list";

  // preamble
  std::ostringstream put;

  // look for trigger phrase
  if ( boost::contains(commentLine, emacsSymbol) )
    {
      std::string buffer(commentLine);       // to be copied later to 'commentLine'
      put << "  emacs symbol found : "  << emacsSymbol   << "\n"
          << "    original line    : '" << buffer << "'" << "\n";
      s_logger->putx(logga::dbug, put);

      // calculations as per '~DataIo' in this file
      int tab1 = xeona::modelFieldIndent;    // set in 'common.cc'
      int tab2 = tab1 + s_tabset + 1;        // determined here
      int tab3 = tab2 + 2;

      // format new tab-stop-list
      std::ostringstream ssBuffer;
      ssBuffer << "(" << boost::format("%02d") % tab1
               << " " << boost::format("%02d") % tab2
               << " " << boost::format("%02d") % tab3
               << ")";
      std::string tabstoplist = ssBuffer.str();

      // modify existing line
      const std::string listOpen = "(";      // denotes beginning of emacs list
      std::string::size_type index = buffer.find(listOpen);
      if ( index != std::string::npos )
        {
          buffer.erase(index, buffer.length());   // chop
          buffer += tabstoplist;                  // append
          put << "    replacement line : '" << buffer << "'" << "\n";
          s_logger->putx(logga::dbug, put);
          commentLine = buffer;              // overwrite function input
        }
      else
        {
          s_logger->repx(logga::info,
                         "emacs " + emacsSymbol + " lacks \"(\"",
                         "");
        }
    }
  else
    {
#ifdef _XUTEST                               // unit test reporting
      std::ostringstream put;
      put << "  emacs symbol NOT found : " << emacsSymbol << "\n";
      s_logger->putx(logga::dbug, put);
#endif // _XUTEST
    }
}

// ---------------------------------------------------------
//  MEMBER FUNCTION : getTimeHorizonValue <>
// ---------------------------------------------------------

template <typename T>                        // 'int' or 'bool'
T                                            // value of sought field
DataIo::getTimeHorizonValue
(const std::string& fname)                   // name of sought field
  throw (xeona::xem_data_issue)
{
  // initial reporting
  s_logger->repx(logga::dbug, "entering member function, fieldname", fname);

  // preamble
  T buffer = T();                            // explicit default construction

  // locate field
  const shared_ptr<Field> f
    = d_records.locateRecordAndField
    (xeona::timehorizon,                     // defined in 'common.cc'
     fname);
  if ( ! f )
    {
      s_logger->repx(logga::warn, "horizon field not found, shared_ptr", f);
      // throw
      if ( xeona::nopro == false )           // meaning option '--krazy' not applied
        {
          s_logger->repx(logga::warn, "about to throw", "xeona::xem_data_issue");
          throw xeona::xem_data_issue("horizon field not found",
                                      fname,
                                      "");
        }
    }

  // recover and process field
  const std::string raw = f->getRawStr();
  if ( raw.empty() )
    {
      s_logger->repx(logga::warn, "raw string empty", "");
      // throw
      if ( xeona::nopro == false )           // meaning option '--krazy' not applied
        {
          s_logger->repx(logga::warn, "about to throw", "xeona::xem_data_issue");
          throw xeona::xem_data_issue("recovered horizon field raw string is empty",
                                      fname,
                                      raw);
        }
    }
  try
    {
      buffer = boost::lexical_cast<T>(raw);
    }
  catch( const boost::bad_lexical_cast& eblc )
    {
      s_logger->repx(logga::warn, "horizon string has bad lexical cast", raw);
      // throw
      if ( xeona::nopro == false )           // meaning option '--krazy' not applied
        {
          s_logger->repx(logga::warn, "about to throw", "xeona::xem_data_issue");
          throw xeona::xem_data_issue("recovered horizon field raw string failed cast",
                                      fname,
                                      eblc.what());
        }
    }

  return buffer;
}

// EXPLICIT TEMPLATE INSTANTIATIONS

template int         DataIo::getTimeHorizonValue(const std::string& fname);
template bool        DataIo::getTimeHorizonValue(const std::string& fname);
template double      DataIo::getTimeHorizonValue(const std::string& fname);
template std::string DataIo::getTimeHorizonValue(const std::string& fname);

//  $Source: /home/robbie/synk/xeona/fragments/RCS/frag-boost-lexical-1.cc,v $
//  end of file

