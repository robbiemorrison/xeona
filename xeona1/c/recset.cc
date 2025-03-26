//  XEONA ENERGY SYSTEMS MODELING ENVIRONMENT
//
//  file-create-name : recset.cc
//  file-create-date : Tue 09-Oct-2007 17:14 UTC
//  file-initiator   : Robbie Morrison <robbie@actrix.co.nz>
//
//  file-role        : records and fields and also record-sets / implementation
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
//  $URL: file:///home/robbie/svn-root/xeona/futz/trunk/xeona1/c/recset.cc $

//  LOCAL AND SYSTEM INCLUDES

#include "recset.h"           // companion header for this file (place first)

#include "../c/util1.h"       // free functions which offer general utilities 1
#include "../b/entity.h"      // entity base class
#include "../a/exapp.h"       // application exception classes
#include "../a/logger.h"      // standard logging functionality (as required)
#include "../c/smart_ptr.h"   // toggle between Boost and TR1 smart pointers
#include ".././common.h"      // common definitions for project (place last)

#include <algorithm>          // STL copying, searching, and sorting
#include <sstream>            // string-streams
#include <string>             // C++ strings

#include <boost/algorithm/string.hpp>   // string recasing, trimming, splitting
#include <boost/foreach.hpp>            // BOOST_FOREACH iteration macro
#include <boost/format.hpp>             // printf style formatting
#include <boost/lexical_cast.hpp>       // lexical_cast<> string to number conversions

//  CODE

// ---------------------------------------------------------
//  notes           : Data in general
// ---------------------------------------------------------
//
//  Record sets
//
//      A 'record set' is a collection of records.
//
//      Currently, only one record set is supported -- that
//      created at start-up.  However, an interactive simulation
//      would need to create and process additional record sets
//      during run-time.
//
//  Values
//
//      Values are the primitives that underpin much of this, or
//      for that matter, any simulation model.  In this
//      documentation, 'values' break down into 'singles' and
//      'timeseries'.  And consequently timeseries contain
//      'elements' and not values.
//
//      No special naming convention in the fieldname
//      distinguishes singles from timeseries during data input.
//      And a horizon of unity is legal.  However the
//      'modelTsRepeater' string literal (defined as ".." at the
//      time of writing) enables repetition:
//
//          my_single      > 1
//          my_timeseries  > 2 ..
//
//      A value is typically read in as a single trimmed string.
//      This string is then split on whitespace (meaning ONE OR
//      MORE space chars) and stored in a vector:
//
//          input --+-- raw string   : std::string
//                  |
//                  +-- split string : std::vector<std::string>
//
//      If the split string is not of the required length, it may
//      be truncated or repeated (or repeated and truncated) to
//      suit (similar to vector "recycling" in the R language).
//
//      For reasons of simplicity, vector's are often used to
//      hold values.  Therefore, a single is a special case
//      timeseries with just one element (not an unreasonable
//      abstraction).
//
//          value --+-- single     : single element std::vector
//                  |
//                  +-- timeseries : multi-element std::vector
//
//      The entity itself is responsible for determining the
//      underlying type and can elect to use anything supported
//      by boost::lexical_cast<> and able to be stringified by
//      boost::format or the stream inserter operator<<
//      (including, for instance, a long double).  In addition,
//      the relevant typedefs must also be modified.
//
//      The following value data types are supported in this
//      simulation model:
//
//          value --+-- number  : int, double
//                  |
//                  +-- boolean : bool, boost::logic::tribool
//                  |
//                  +-- text    : std::string
//
//      Values may also play one of two roles, thus:
//
//          value --+-- input  : externally supplied
//                  |
//                  +-- output : internally generated
//
//      Singles objects also need to be "wrapped" so that they
//      can initialize entity data members held as references.
//      See the comments pertaining to class 'wrap' for details.
//
// ---------------------------------------------------------

// ---------------------------------------------------------
//  CLASS (FUNCTOR) : ::UnlexElement <>
// ---------------------------------------------------------
//  Description  : interface to 'UnlexSingle' with 'wrap' transformation added
//  Supports     : 'TimeseriesUnlex_Helper'
//  Uses         : 'UnlexSingle'
//  Status       : complete
// ---------------------------------------------------------

namespace
{
  class UnlexSingle;                         // forward (partial) declaration

  template <typename T>
  class UnlexElement
  {
  public:

    std::string
    operator()
    (T element)                              // T must be supported by single_type
    {
      ::UnlexSingle um;                      // functor
      return um(wrap<T>(element));           // uses 'single_type', hence the "conversion"
    }

  };

} // unnamed namespace

// ---------------------------------------------------------
//  FREE FUNCTION   : ::TimeseriesUnlex_Helper <>
// ---------------------------------------------------------
//  Description  : provides generic 'timeseries' traversal routines
//  Supports     : 'TimeseriesUnlex'
//  Uses         : 'UnlexElement'
//  Status       : complete
// ---------------------------------------------------------

namespace
{
  template <typename T>
  std::vector<std::string>
  TimeseriesUnlex_Helper
  (const shared_ptr<std::vector<T> > input)
  {
    unsigned horizon = Entity::getHorizonSteps(); // grab horizon steps
    std::vector<std::string> output(horizon);

    std::transform                           // copy and modify, requires <algorithm>
      (input->begin(),                       // source range start
       input->end(),                         // source range end
       output.begin(),                       // destination range start
       ::UnlexElement<T>());                 // unitary operation, '::' optional

    return output;
  }

} // unnamed namespace

// ---------------------------------------------------------
//  CLASS (FUNCTOR) : ::UnlexTimeseries_VariantVisitor <>
// ---------------------------------------------------------
//  Description  : boost::variant functor for unlexing 'timeseries'
//  Supports     : 'unlexTimeseries'
//  Uses         : 'TimeseriesUnlex_Helper'
//  Techniques   : Boost.Variant visitor
//  Status       : complete
// ---------------------------------------------------------

namespace
{

  class UnlexTimeseries_VariantVisitor :
    public boost::static_visitor<std::vector<std::string> >
  {

    typedef std::vector<std::string> return_type;  // CAUTION: same as line above

  public:

    //int, double, bool, boost::logic::tribool, std::string

    return_type operator() (const shared_ptr<std::vector<int> > input) const
    {
      return ::TimeseriesUnlex_Helper<int>(input);
    }

    return_type operator() (const shared_ptr<std::vector<double> > input) const
    {
      return ::TimeseriesUnlex_Helper<double>(input);
    }

    return_type operator() (const shared_ptr<std::vector<bool> > input) const
    {
      return ::TimeseriesUnlex_Helper<bool>(input);
    }

    return_type operator() (const shared_ptr<std::vector<tribool> > input) const
    {
      return ::TimeseriesUnlex_Helper<tribool>(input);
    }

    return_type operator() (const shared_ptr<std::vector<std::string> > input) const
    {
      return ::TimeseriesUnlex_Helper<std::string>(input);
    }

  };

} // unnamed namespace

// ---------------------------------------------------------
//  CLASS (FUNCTOR) : ::UnlexElement_VariantVisitor
// ---------------------------------------------------------
//  Description  : boost::variant functor returning formatted string
//  Role         : processing of boost::variant fields for output
//  Note         : formats output for both 'singles' and 'timeseries'
//  Supports     : 'UnlexSingle'
//  Uses         : call chain end
//  Techniques   : Boost.Variant visitor and Boost.Format library
//  Status       : complete
//
//  Design notes
//
//      The use of call-by-reference (rather than call-by-value)
//      in operator() prevents implicit conversions, for instance
//      from char to int (see Karlsson 2006).
//
//  Boost.Format
//
//      Boost.Format syntax (with the positional format
//      specification omitted)
//
//          %[flags][width][.precision]type-char
//
//          flags     : - + _ + # 0 [spc]
//          type-char : px, o, e, f, g, diu, s, c, % (also some uppercase)
//
//      Output formatting options used here
//
//          double  : %e    7 sig-figs, always scientific
//                    %g    6 sig-figs, scientific or raw (default)
//                    %.2e  3 sig-figs, scientific
//          int     : %i    straight output (equivalent to %d)
//
//          bool    : std::noboolalpha  0 or 1
//          tribool : std::noboolalpha  0, 1, 2
//
//          string  : %s  (can set precision first chars, also flags and width)
//
//  CAUTION: setting tribools
//
//     Set tribools with key-words (false, true, indeterminate)
//     and not integer literals (0, 1, 2).
//
//  See also
//
//      Karlsson (2006 pp191-207) describes the Boost.Variant
//      library.
//
//      The Boost.Format library is not covered in text books as
//      far as I know.  See the Boost documentation instead:
//
//        file:///path-to-boost/boost_1_34_1/libs/format/index.html
//
// ---------------------------------------------------------

// NOTE: this functor works equally with 'int' and 'wrap<int>'
// objects without modification

namespace
{
  class UnlexElement_VariantVisitor :
    public boost::static_visitor<std::string>
  {
  public:

    //int, double, bool, boost::logic::tribool, std::string

    std::string operator() (const int& i) const
    {
      return boost::str(boost::format("%i") % i);
    }

    std::string operator() (const double& d) const
    {
      // 'xeona::writeFloatFmtStr' set in 'common.cc'
      const std::string fmt = xeona::writeFloatFmtStr;
      return boost::str(boost::format(fmt) % d);
    }

    std::string operator() (const bool& b) const
    {
      std::ostringstream os;
      os << std::noboolalpha << b;
      return os.str();
    }

    std::string operator() (const tribool& t) const
    {
      std::ostringstream os;
      os << std::noboolalpha << t;
      return os.str();
    }

    std::string operator() (const std::string& s) const
    {
      const std::string del = xeona::modelStringDelim;
      return boost::str(boost::format("%s%s%s") % del % s % del);
    }

  };

} // unnamed namespace

// ---------------------------------------------------------
//  CLASS (FUNCTOR) : ::UnlexSingle
// ---------------------------------------------------------
//  Description  : format a variant type and return a std::string
//  Role         : unitary operator for STL container algorithm
//  Supports     : 'unlexSingle', 'UnlexElement'
//  Uses         : 'UnlexElement_VariantVisitor'
//  Techniques   : Boost.Variant library
//  Status       : complete
//
//  Design notes
//
//      Please note that 'UnlexElement_VariantVisitor' could not
//      be applied directly because of the boost::apply_visitor
//      syntax.  Perhaps I got this wrong but I tried pretty hard
//      to rationalize the call chains used here.  Hence the need
//      for this wrapper functor (which is analogous to
//      'LexElement' in any case).  [If interested in developing
//      this code, check out Karlsson (2006 p205)].
//
// ---------------------------------------------------------

namespace
{
  class UnlexSingle
  {
    // CAUTION: 'single_type' must be visible here (probably
    // because it has been declared as a global 'typedef' in the
    // header file).

  public:

    std::string
    operator()
    (single_type element)                         // CAUTION: 'const' caused problems
    {
      ::UnlexElement_VariantVisitor sf;           // formatting functor
      return boost::apply_visitor(sf, element);   // automatically selects correct type
    }

  };

} // unnamed namespace

// ---------------------------------------------------------
//  CLASS (FUNCTOR) : ::LexElement <>
// ---------------------------------------------------------
//  Description  : cast from std::string to nominated type T
//  Role         : unitary operator for STL container algorithm
//  Supports     : 'lexValue'
//  Uses         : call chain end
//  Techniques   : Boost.Conversion library
//  Status       : complete
// ---------------------------------------------------------

namespace
{
  template <typename T>
  class LexElement
  {
  public:

    LexElement() : d_loggerCalls(0) { }      // explicitly initialize 'd_loggerCalls'

    T                                        // functor return type is T
    operator()                               // CAUTION: template keyword not required
    (const std::string& element)
    {
    try
      {
        return boost::lexical_cast<T>(element);   // Boost.Conversion library
      }
    catch( const boost::bad_lexical_cast& eblc )
      {
        if ( d_loggerCalls < xeona::sameLogLimit )
          {
            const std::string quoteElem = '"' + element + '"';
            std::ostringstream put;
            put << "    lexical cast failure: " << eblc.what()                    << "\n"
                << "    in function       : "   << XEONA_FUNC                     << "\n"
                << "    value (set in \"\") : " << quoteElem                      << "\n"
                << "      if the above value looks reasonable (is the 'e' present),"
                << " check that the input file"                                   << "\n"
                << "      is strictly ASCII and, in particular, that any minus character"
                << " is indeed ASCII decimal 045"                                 << "\n";
            s_logger->putx(logga::dbug, put);
            s_logger->repx(logga::warn, "numeric cast type incompatibility", quoteElem);
          }
        else if ( d_loggerCalls == xeona::sameLogLimit )
          {
            s_logger->repx(logga::info, "same log limit exceeded", xeona::sameLogLimit);
          }
        d_loggerCalls++;
        return T();                          // zero-argument (default) construction
      }
    }

  private:

    unsigned                  d_loggerCalls; // to prevent a deluge of log messages
    static logga::spLogger    s_logger;      // shared_ptr to single logger object

  };

  //  STATIC DEFINITIONS

  template <typename T>
  logga::spLogger
  LexElement<T>::s_logger = logga::ptrLogStream();   // bind logger on definition

} // unnamed namespace

// ---------------------------------------------------------
//  CLASS           : Field
// ---------------------------------------------------------
//  Description  : holds field information
//  Role         : used directly or by friendly Record objects
//  Status       : complete
//
//  Design notes
//
//      A Field object, after being loaded from a model file,
//      usually holds either one string element or an entire time
//      horizon of separated string elements in 'd_splitStr'.
//
//      These strings, however, are not lexically converted until
//      the data has been requested by the associated entity --
//      at which time the underlying type is known.  This
//      conversion process is managed by the host Record object
//      but the modification code is provided by this class.
//
//      The converted data is bound by reference or shared_ptr to
//      the relevant entity.  This means that the record set and
//      the associated entity hold the same data.
//
//      The original string input is retained in 'd_rawStr'.  In
//      actuality (at the time of writing), 'd_rawStr' plays no
//      role after initialization and could be omitted in the
//      interests of memory conservation.
//
//      This class also relies on a number of helper functors,
//      also defined in this file.
//
//  Note on empty objects
//
//      An empty shared_ptr object returns member function
//      'use_count()' = 0 (whereas a shared_ptr holding a NULL of
//      the correct type yields 'use_count()' = 1 and 'get()' =
//      0).  The statement 'if ( sp )' can be used to determine
//      if the pointer holds anything useful.
//
//      In regards boost::variant, the member function 'empty()'
//      ALWAYS returns false (Karlsson 2006 p195).  This is
//      because the zero-argument (default) boost::variant
//      constructor zero-argument (default) constructs the first
//      type in the list.
//
// ---------------------------------------------------------

//  STATIC DEFINITIONS

logga::spLogger
Field::s_logger = logga::ptrLogStream();     // bind logger on definition

// CREATORS

Field::Field() :
  d_name(),
  d_kind(xeona::e_notField),
  d_enabled(indeterminate),
  d_units(),
  d_remark(),
  d_rawStr(),
  d_splitStr(),
  d_single(),
  d_timeseries()
{ }

// MANIPULATORS -- for loading data

void Field::addName(std::string fieldname)         { d_name       = fieldname; }
void Field::addKind(xeona::FieldKind kind)         { d_kind       = kind;      }
void Field::addEnabled(tribool enabled)            { d_enabled    = enabled;   }
void Field::addUnits(std::string units)            { d_units      = units;     }
void Field::addRemark(const std::string& remark)   { d_remark     = remark;    }
void Field::addRawStr(const std::string& rawStr)   { d_rawStr     = rawStr;    }
void Field::addSplitStr(const std::vector<std::string>& splitStr)
                                                   { d_splitStr   = splitStr;  }
// ACCESSORS

std::string                     Field::getName()     const { return d_name;    }
xeona::FieldKind                Field::getKind()     const { return d_kind;    }
tribool                         Field::getEnabled()  const { return d_enabled; }
std::string                     Field::getUnits()    const { return d_units;   }
std::string                     Field::getRemark()   const { return d_remark;  }

std::string                     Field::getRawStr()   const { return d_rawStr;           }
const std::vector<std::string>& Field::getSplitStr() const { return d_splitStr;         }
int                             Field::getCount()    const { return d_splitStr.size();  }
bool                            Field::isEmpty()     const { return d_splitStr.empty(); }

// SPECIAL MANIPULATORS -- deploy with care BEFORE the raw string processing has occured

void
Field::overwriteRawStr        // the client is entirely responsible for the timing!
(const std::string& rawStr)
{
  // initial reporting
  s_logger->repx(logga::adhc, "entering member function, argument", rawStr);

  // active code
  const std::string was = d_rawStr;
  d_rawStr              = rawStr;

  // report
  if ( was != d_rawStr )
    {
      s_logger->repx(logga::dbug, "significant change made", "");
    }
  else
    {
      s_logger->repx(logga::dbug, "non significant change made", "");
    }
}

// ACCESSORS -- after binding has occurred

const std::string
Field::getSingle()
{
  unlexSingle(d_single);
  return d_splitStr.front();
}

const std::vector<std::string>
Field::getTimeseries()
  throw(xeona::empty_field_on_write)         // exception specification
{
  // protect against 'd_timeseries' remaining default constructed
  // -- meaning that no subsequent reassignment has occurred (the
  // most likely explanation is that dataset never contained this
  // field)
  try
    {
      typedef shared_ptr<std::vector<int> > default_type;
      default_type test = boost::get<default_type>(d_timeseries);
      if ( test == 0 )
        {
          // 'boost::get' succeeded but shared pointer null or
          // empty -- a aforementioned problem thus identified
          s_logger->repx(logga::warn, "non-reassigned timeseries", d_name);
          if ( xeona::nopro == false )       // meaning option '--krazy' not applied
            {
              s_logger->repx(logga::warn, "will throw xeona::empty_field_on_write", "");
              throw xeona::empty_field_on_write("timeseries", d_name);
            }
        }
    }
  catch( const boost::bad_get& e )
    {
      // do nothing is correct -- to be here means that
      // 'd_timeseries' is no longer default constructed
    }

// active code
  unlexTimeseries(d_timeseries);
  return d_splitStr;
}

const std::string
Field::getTimeseries
(const std::string& sep)                     // 'sep' overloaded form of 'getTimeseries'
{
  std::string buf;
  BOOST_FOREACH( std::string s, getTimeseries() )
    {
      buf += sep;
      buf += s;
    }
  if ( ! buf.empty() )
    buf = buf.substr(sep.length());          // trim leading 'sep' string (often a space)
  return buf;
}

// MANIPULATORS -- for use by friendly Record objects

void
Field::splitRawStr()                         // process raw string into split string
  throw(std::exception,                      // exception specification
        xeona::short_timeseries)
{
  // PREAMBLE

  // obtain horizon and string delimiter from elsewhere
  unsigned horizon = Entity::getHorizonSteps();   // grab horizon steps
  const char* delim = xeona::modelStringDelim.c_str();      // [1]

  // CAUTION: the Boost.Tokenizer boost::char_separator<char>
  // 'separator' would only accept a C-string -- and not a
  // std::string or char (as might be expected)
  //
  // [1] model string delimiter: the string delimiter 'delim' is
  // defined in "common.cc" and was (at the time of writing) set
  // to an escaped double quote (\") but could equally be a
  // single quote (') or similar.

  // define a string vector buffer
  std::vector<std::string> split;

  // CHECK IF PAIRWISE STRING DELIMITERS ARE PRESENT

  boost::find_all(split, d_rawStr, delim);   // temporarily borrow 'split'
  unsigned delims = split.size();            // number of delims found, if any
  split.clear();                             // return 'split' as found

  if ( delims > 0 && (delims % 2) != 0 )     // inbuilt remainder operator
    {
      std::string msg = getName() + " delims unbalanced";
      s_logger->repx(logga::warn, msg, delims);
      return;
    }

  // UNDERTAKE SPLIT USING APPROPRIATE APPROACH

  if ( delims == 0 )                         // process as non-strings
    {
      // CAUTION: 'boost::token_compress_on' means "adjacent
      // separators are merged together", otherwise (by default)
      // every pair of separators delimits a token, either empty
      // or full as the case may be

      boost::split(split, d_rawStr, boost::is_any_of(" "), boost::token_compress_on);
    }
  else if ( delims > 0 )                     // process as strings
    {
      // CAUTION: "no parsing is actually done upon construction
      // -- parsing is done on demand as the tokens are accessed
      // via the iterator" (Boost.Tokenizer documentation,
      // 'Tokenizer Class' page)
      //
      // The general approach: the code in the 'for' block is
      // deliberately step-by-step so it can be readily modified

      typedef boost::char_separator<char> separator;
      typedef boost::tokenizer<separator> tokenizer;
      typedef tokenizer::iterator         iterator;

      separator sep(delim);
      tokenizer tokens(d_rawStr, sep);
      for ( iterator it = tokens.begin();
            it         != tokens.end();
            ++it )
        {
          std::string tok(*it);              // grab current token
          if ( tok.empty() ) continue;       // the next line would also catch this
          if ( boost::all(tok, boost::is_space()) ) continue;    // [1]
          boost::trim(tok);                  // trim whitespace from both ends
          split.push_back(tok);              // useful data (we hope)
        }

      // [1] the outer predicate 'boost::all' holds if all
      // elements (chars in this case) satisfy the inner
      // predicate 'boost::is_space'
    }

  // SPECIAL TREATMENT FOR BUILT-IN REMARK

  // this code allows for the non-standard "builtin-remark"
  // values of { null "" "string" }

  if ( getName() == "builtin-remark" )
    {
      split.clear();
      split.push_back("");
    }

  // CHECK AND COMMENT IF DATA IS NOT WELL-FORMED

  if ( split.empty() )
    {
      if ( getEnabled() )                    // skip disabled fields
        {
          s_logger->repx(logga::rankNoData, "enabled field value is empty", getName());
        }
      return;
    }
  else if ( split.front() == xeona::modelTsRepeater )
    {
      std::string repeater = split.front();
      s_logger->repx(logga::warn, "field value is repeat indicator", repeater);
      return;
    }

  // GENERATE A FULL TIMESERIES IF REQUESTED

  if ( split.back() == xeona::modelTsRepeater )
    {
      split.pop_back();                           // remove the repeat indicator
      std::vector<std::string> pattern(split);    // copy construct
      split.empty();
      split.resize(horizon);                      // set split to required length
      xeona::vectorRepeat(pattern, split);        // implicit template instantiation
    }

  // CHECK FOR AN UNDERSIZE TIMESERIES [1]

  unsigned len = split.size();
  if ( len > 1 && len < horizon )
    {
      int delta = horizon - len;
      std::string message = "'" + getName() + "' short by";
      s_logger->repx(logga::warn, message, delta);

      if ( xeona::nopro == false )
        {
          s_logger->repx(logga::warn, "will throw xeona::short_timeseries", "");
          throw xeona::short_timeseries(getName(), delta);
        }
    }

  // [1] singles versus timeseries: it is NOT POSSIBLE, at this
  // point, to distinguish between a trivial timeseries with only
  // one element and a genuine single -- the discriminating
  // information is held by entities and any discrepancies will
  // become apparent during entity creation (even in the event
  // that the horizon is set to unity)

  // LOAD INFO

  d_splitStr = split;         // early returns necessarily leave 'd_splitStr' untouched
}

template <typename T>
void
Field::lexValue                              // string to type T conversion
(shared_ptr<std::vector<T> > output)         // an empty shared_ptr cannot be passed in
{
  // Josuttis (1999 pp367-368) discusses std::transform().  Note
  // that "the caller must ensure the destination range is big
  // enough or that insert iterators are used" (p367).

  unsigned horizon = Entity::getHorizonSteps();   // grab horizon steps
  output->clear();                           // empty the container (if not already so)
  output->resize(horizon);                   // grow using default elements (or shrink)

  std::transform                             // copy and modify, requires <algorithm>
    (d_splitStr.begin(),                     // source range start
     d_splitStr.end(),                       // source range end
     output->begin(),                        // destination range start
     ::LexElement<T>());                     // unitary operation, '::' optional
}

void
Field::unlexSingle
(const single_type& input)
{
  d_splitStr.clear();                        // remove all elements
  ::UnlexSingle um;                          // functor wrapper to formatting functor
  d_splitStr.push_back(um(input));           // the one and only element
}

void
Field::unlexTimeseries
(const timeseries_type input)
{
  d_splitStr.clear();                        // remove all elements
  ::UnlexTimeseries_VariantVisitor tf;
  d_splitStr = boost::apply_visitor(tf, input);
}

// ---------------------------------------------------------
//  CLASS           : Record
// ---------------------------------------------------------
//  Description  : holds a record, comprising metainfo and fields
//  Role         : used by RecordSet
//  Status       : complete
//
// 'Entity' constructor usage for 'tieSingle' and 'tieTimeseries':
//
//      Entity(Record& record) :
//        d_record(record),
//        d_coeff(record.tieSingle<double>("coeff")),    // single value
//        d_count(record.tieTimeseries<int>("count")),   // timeseries value
//        d_local(false)
//      { }
//
//      Record&           d_record;                 // not strictly necessary
//      double&           d_coeff;
//      std::vector<int>  d_count;
//      bool              d_local;
//
// ---------------------------------------------------------

//  STATIC DEFINITIONS

logga::spLogger Record::s_logger
  = logga::ptrLogStream();                   // bind logger on definition

// CREATORS

Record::Record() :
  d_identifier(),
  d_kind(xeona::e_notRecord),
  d_enabled(indeterminate),
  d_fields(),
  d_comments()
{
  // CAUTION: unsatisfactory interactions between two static
  // objects -- one a static data member of type 'Record' defined
  // by the 'Entity' class and the other the static data member
  // 'Record::s_logger copy assigned from the
  // 'logga::shared_ptr<Logger>' instance held by free function
  // 'logga::ptrLogStream' -- can lead to 'Record::s_logger' not
  // being satisfactorily initialized and hence null.  The order
  // of events must be important -- however I was not able to
  // rectify the problem directly and so the following protection
  // is applied:

  if ( s_logger )                            // a shared pointer
    {
      s_logger->repx(logga::xtra, "constructor call", "");
    }
  else                                       // used in the case just cited
    {
      logga::spLogger logger = logga::ptrLogStream();
      logger->repx(logga::dbug, "constructor call, my s_logger", s_logger.get());
    }
}

Record::~Record()
{
  // CAUTION: 'Logger' object calls placed here will cause
  // trouble if the 'Logger' singleton has already been
  // destroyed, hence the following protection is required (a
  // similar problem exists for the constructor):

  if ( s_logger )                            // a shared pointer
    {
      s_logger->repx(logga::xtra, "destructor call", "");
    }
#ifdef _XUTEST
  else
    {
      // CAUTION: this code will produce reporting after the
      // closing log rule on the console (the final row of dots)
      // and that doesn't look that great on an application build

      logga::spLogger logger = logga::ptrLogStream();
      logger->repx(logga::adhc, "destructor call, my s_logger", s_logger.get());
    }
#endif // 0
}

// MANIPULATORS -- for loading data

void Record::addIdentifier(std::string recordId)    { d_identifier = recordId;       }
void Record::addKind(xeona::RecordKind recordKind)  { d_kind       = recordKind;     }
void Record::addEnabled(tribool recordEnabled)      { d_enabled    = recordEnabled;  }
void Record::addComment(const std::string& comment) { d_comments.push_back(comment); }

void Record::addField(shared_ptr<Field> field)
  throw(std::exception,                      // exception specification
        xeona::xem_data_issue)
{
  // check for duplicate enabled class or data field and throw if encountered
  const std::string name = field->getName();
  if ( field->getEnabled()                   // CAUTION: tribool is 'true'
       &&                                    // .. meaning skip disabled fields
       locateFieldQuietly(name) )            // tests 'true' if already exists
    {
      const std::string value = field->getRawStr();
      if ( name == "class" )                 // special case of: class >
        {
          s_logger->repx(logga::warn, "duplicate class field encountered", name);
          if ( xeona::nopro == false )       // meaning option '--krazy' not applied
            {
              s_logger->repx(logga::warn, "will throw xeona::xem_data_issue", "");
              throw xeona::xem_data_issue("duplicate class field encountered",
                                          name,
                                          value);
            }
        }
      else
        {
          s_logger->repx(logga::warn, "duplicate data field encountered", name);
          if ( xeona::nopro == false )
            {
              s_logger->repx(logga::warn, "will throw xeona::xem_data_issue", "");
              throw xeona::xem_data_issue("duplicate data field encountered",
                                          name,
                                          value);
            }
        }
    }

  // perform update
  d_fields.push_back(field);
}

// ACCESSORS

const std::string               Record::getIdentifier() const { return d_identifier; }
xeona::RecordKind               Record::getKind()       const { return d_kind;       }
tribool                         Record::getEnabled()    const { return d_enabled;    }

const std::vector<std::string>&        Record::getComments() const { return d_comments; }
const std::vector<shared_ptr<Field> >& Record::getFields()   const { return d_fields;   }

const std::string
Record::locateClass() const
{
  const shared_ptr<Field> f = locateField("class");
  if ( ! f )
    {
      s_logger->repx(logga::warn, "unable to locate 'class' for record", d_identifier);
      return "";
    }
  std::string str = f->getRawStr();
  if ( str.empty() )
    {
      s_logger->repx(logga::warn, "empty 'class' string for record", d_identifier);
      return "";
    }
  return str;
}

// BINDING FUNCTIONS -- for use by entities

template <typename T>                        // T must be supported by 'single_type'
wrap<T>                                      // see design notes for class 'wrap'
Record::tieSingle
(const std::string& fieldname)
{
  // locate the required field
  shared_ptr<Field> f;                       // field of interest
  f = locateField(fieldname);                // called function warns if not found
  if ( ! f )
    return wrap<T>();                        // empty wrap object

  // lex 'splitStr' now that T is known
  shared_ptr<std::vector<T> > lexed(new std::vector<T>());
  f->lexValue<T>(lexed);                     // load temp with type T values

  // create and distribute an aliasable single
  T value = lexed->front();                  // grab the first and only element
  wrap<T> single(value);                     // create "wrapped" form
  f->addSingle<T>(single);                   // copy assign by reference to the recordset
  return single;                             // return an aliasable object to the entity
}

template <typename T>                        // T in {int,double,bool,tribool,std::string}
wrap<T>                                      // see design notes for class 'wrap'
Record::tieConstant
(const T constantValue)
{
  // create and distribute an aliasable constant
  T value = constantValue;                   // grab the given value
  wrap<T> constant(value);                   // create "wrapped" form
  return constant;                           // return an aliasable object to the entity
}

template <typename T>
shared_ptr<std::vector<T> >                  // only the contained type T is specified
Record::tieTimeseries
(const std::string& fieldname)
  throw(xeona::timeseries_not_found)         // exception specification
{
  // locate the required field
  shared_ptr<Field> f;                       // field of interest
  f = locateField(fieldname);                // called function warns if not found
  if ( ! f )
    {
      s_logger->repx(logga::warn, "timeseries field not found", fieldname);
      if ( xeona::nopro == false )       // meaning option '--krazy' not applied
        {
          // CAUTION: the following exception is NOT caught
          // despite the presence of suitable catch blocks,
          // moreover this is also a problem if
          // 'std::runtime_error' it thrown instead (a complete
          // mystery)

          const std::string templateType = xeona::demangle(typeid(T).name());
          s_logger->repx(logga::warn, "will throw xeona::timeseries_not_found", "");
          throw xeona::timeseries_not_found(fieldname, templateType);
        }
      else
        {
          s_logger->repx(logga::dbug, "now running unprotected code", "");
        }

#if 1 // 1 = returns zero-length vector, 0 = returns empty shared pointer (original code)
      s_logger->repx(logga::warn, "probable out-of-range throw shortly", "");
      shared_ptr<std::vector<T> > zeroLengthVector(new std::vector<T>());
      return zeroLengthVector;
#else
      s_logger->repx(logga::warn, "probable segfault shortly", "");
      return typename shared_ptr<std::vector<T> >::shared_ptr();  // empty smart pointer
#endif // 0
    }

  // lex 'splitStr' now T is known
  shared_ptr<std::vector<T> > lexed(new std::vector<T>());
  f->lexValue<T>(lexed);                     // load temp with type T values

  // distribute the smart pointer object
  f->addTimeseries<T>(lexed);                // smart pointer copy to the recordset
  return lexed;                              // return a shared pointer to the entity
}

const shared_ptr<Field>                      // returns empty smart pointer if not found,
Record::locateField                          //   also logs a warning
(const std::string& fieldname) const
{
  shared_ptr<Field> ret = locateFieldQuietly(fieldname);
  if ( ! ret )                               // test for failure
    {
      s_logger->repx(logga::warn, "field name not found", fieldname);
    }
  else
    {
      s_logger->repx(logga::adhc, "field name found", fieldname);
    }
  return ret;                                // empty or full smart pointer
}

const shared_ptr<Field>                      // returns empty smart pointer if not found,
Record::locateFieldQuietly                   //   but no logging occurs
(const std::string& fieldname) const
{
  BOOST_FOREACH( shared_ptr<Field> f, d_fields )
    if ( f->getName() == fieldname )
      return f;                              // full smart pointer
  return shared_ptr<Field>();                // empty shared pointer
}

void
Record::processFields()
{
  BOOST_FOREACH( shared_ptr<Field> f, d_fields )
    {
      f->splitRawStr();
    }
}

// MANIPULATORS -- for unit tests which make entities but do not read from a '.xem' file

// ---------------------------------------------------------
//  MEMBER FUNCTION : Reccord::hackUnitTestRecord
// ---------------------------------------------------------
//  Description  : modify a dummy 'Record' object to be okay for a 'FullEntity' instance
//  Role         : unit tests which do not read from a model (*.xem) file
//  Requires     : the '_XUTEST' macro must be set for a meaningful call
//  Caller       : 'Record' instance within a unit test test block
//  Status       : complete
// ---------------------------------------------------------

void
Record::hackUnitTestRecord                   // CAUTION: hollow function unless '_XUTEST'
(const std::string builtinRemark,
 const int         horizonSteps)
{
#ifndef _XUTEST

  s_logger->repx(logga::warn, "hollow function as not '_XUTEST'", "");
  return;

#else

  // hard-coded constants
  const std::string fieldname       = "builtin-remark";     // given in 'FullEntity'
  const int         horizonInterval = 3600;                 // usual one hour

  // some reporting
  std::ostringstream oss;
  oss << "\"" << builtinRemark << "\"";
  s_logger->repx(logga::dbug, "entering member function with remark", oss.str());
  if ( builtinRemark.empty() )
    s_logger->repx(logga::info, "empty remark supplied", builtinRemark);

  // main work
  shared_ptr<Field> fBir(new Field());       // new 'Field' instance
  fBir->addName(fieldname);
  fBir->addKind(xeona::e_output);
  fBir->addEnabled(true);
  fBir->addRawStr(builtinRemark);            // from the function argument
  addField(fBir);                            // insert field

  // set some necessary values directly
  Entity::setHorizonDirectly(horizonSteps, horizonInterval);     // [1]
  //[1]: entire func in '_XUTEST'

  // CAUTION: the following call should not be used: fBir->splitRawStr();

  // some testing and reporting
  const shared_ptr<Field> fRem = locateField(fieldname);
  if (! fRem )
    {
      s_logger->repx(logga::warn, "field not located, fieldname", fieldname);
    }
  else
    {
      std::ostringstream put;
      put << std::boolalpha
          << "  field satisfactorily located"                               << "\n"
          << "    name      : "   << fRem->getName()                        << "\n"
          << "    kind      : "   << fRem->getKind()                        << "\n"
          << "    enabled   : "   << fRem->getEnabled()                     << "\n"
          << "    empty     : "   << fRem->isEmpty()                        << "\n"
          << "    value     : \"" << fRem->getRawStr() << "\""              << "\n"
          << "    split cnt : "   << fRem->getCount()                       << "\n"
          << "  horizon details"                                            << "\n"
          << "    steps     : "   << Entity::getHorizonSteps()              << "\n"
          << "    interval  : "   << Entity::getHorizonInterval()           << "\n"
        ; // trailing semicolon
      s_logger->putx(logga::adhc, put);      // reporting threshold set high
    }
  return;

#endif // _XUTEST
}

// ---------------------------------------------------------
//  CLASS (FUNCTOR) : ::IfRecKind
// ---------------------------------------------------------
//  Description  : boolean functor for 'RecordSet'
//  Role         : 'std::remove_copy_if' algorithm predicate in 'copySubset'
//  Status       : complete
//
//  Design notes
//
//      Note carefully that the predicate logic is "remove if
//      true" and NOT "copy if true" -- for some reason there is
//      no 'std::copy_if' algorithm.
//
// ---------------------------------------------------------

namespace
{
  class IfRecKind
  {
  private:                                   // public copy constructor required

    IfRecKind();                                       // zero-argument constructor
    IfRecKind& operator= (const IfRecKind& orig);      // copy assignment operator

  public:

    IfRecKind
    (const xeona::RecordKind recKind) :
      d_recKind(recKind)
    {
      logga::spLogger logger = logga::ptrLogStream();
      logger->repx(logga::dbug, "constructor call (functor)", "");
    }

    bool operator()
      (const shared_ptr<Record> rec)
    {
      if ( rec->getKind() == d_recKind )
        return false;                        // meaning please copy!
      else
        return true;
    }

  private:

    xeona::RecordKind    d_recKind;

  };

} // unnamed namespace

// ---------------------------------------------------------
//  CLASS           : RecordSet
// ---------------------------------------------------------
//  Description  : holds a set of Records
//  Role         : provide a single point of entry
// ---------------------------------------------------------

//  STATIC DEFINITIONS

logga::spLogger
RecordSet::s_logger = logga::ptrLogStream(); // bind logger on definition

// CREATORS

RecordSet::RecordSet() :
  d_status(e_empty),
  d_records()                                // empty vector
{
  s_logger->repx(logga::dbug, "constructor call", "");
}

RecordSet::~RecordSet()
{
  s_logger->repx(logga::dbug, "destructor call", "");
}

// MANIPULATORS -- for loading data and such

void RecordSet::setStatus(RecordSet::Status status)  { d_status = status;           }

void RecordSet::addRecord(shared_ptr<Record> record)
  throw(std::exception, xeona::xem_data_issue)    // exception specification
{
  // check for duplicate enabled record identifier and throw if encountered
  const std::string identifier = record->getIdentifier();
  if ( record->getKind() != xeona::e_note    // duplicate notes allowed
       &&
       record->getEnabled()                  // CAUTION: tribool is 'true'
       &&                                    // .. meaning skip disabled records
       existsEnabledRecord(identifier) )     // tests 'true' if already exists
    {
      s_logger->repx(logga::warn, "duplicate record encountered", identifier);
      if ( xeona::nopro == false )           // meaning option '--krazy' not applied
        {
          s_logger->repx(logga::warn, "will throw xeona::xem_data_issue", "");
          throw xeona::xem_data_issue("duplicate record encountered",
                                      "(record identifier)",
                                      identifier);
        }
    }

  // perform update
  d_records.push_back(record);
}

void
RecordSet::processRecords()
{
  BOOST_FOREACH( shared_ptr<Record> r, d_records )
    r->processFields();
}

// BOUND SUBSETS

std::vector<shared_ptr<Record> >             // may be an empty vector
RecordSet::copySet()
{
  typedef std::vector<shared_ptr<Record> > vpr_type;   // for convenience

  vpr_type fullset(d_records.size());        // CAUTION: must be of sufficient size
  vpr_type::iterator pos;                    // CAUTION: cannot be const_iterator

  pos = std::copy                            // from <algorithm>, 'pos' is not used here
    (d_records.begin(),                      // source container
     d_records.end(),
     fullset.begin());                       // destination container

  return fullset;                            // could be an empty vector
}

std::vector<shared_ptr<Record> >             // may be an empty vector
RecordSet::copySubset
(const xeona::RecordKind recKind)
{
  // CAUTION: note the "copy if false" logic for the
  // 'std::remove_copy_if' predicate

  typedef std::vector<shared_ptr<Record> > vpr_type;   // for convenience

  vpr_type subset(d_records.size());         // CAUTION: must be of sufficient size
  vpr_type::iterator pos;                    // CAUTION: cannot be const_iterator

  pos = std::remove_copy_if                  // from <algorithm>
    (d_records.begin(),                      // source container
     d_records.end(),
     subset.begin(),                         // destination container
     ::IfRecKind(recKind));                  // functor as temporary
  subset.erase(pos, subset.end());           // CAUTION: must trim the trailing junk

  return subset;                             // could be an empty vector
}

// ACCESSORS

RecordSet::Status                       RecordSet::getStatus()  { return d_status;       }
unsigned                                RecordSet::getCount() { return d_records.size(); }
const std::vector<shared_ptr<Record> >& RecordSet::getRecords() { return d_records;      }

const shared_ptr<Record>
RecordSet::locateRecord
(const std::string& recordIdentifier) const
{
  shared_ptr<Record> ret = locateRecordQuietly(recordIdentifier);
  if ( ! ret )
    s_logger->repx(logga::warn, "record identifier not found", recordIdentifier);
  return ret;
}

const shared_ptr<Record>
RecordSet::locateRecordQuietly
(const std::string& recordIdentifier) const
{
  BOOST_FOREACH( shared_ptr<Record> r, d_records )
    if ( r->getIdentifier() == recordIdentifier )
      return r;
  return shared_ptr<Record>();               // empty shared pointer
}

const bool
RecordSet::existsEnabledRecord
(const std::string& recordIdentifier) const
{
  BOOST_FOREACH( shared_ptr<Record> r, d_records )
    if ( r->getIdentifier() == recordIdentifier
         &&
         r->getEnabled() )
      {
        return true;
      }
  return false;
}

const shared_ptr<Field>
RecordSet::locateRecordAndField
(const std::string& recordIdentifier,
 const std::string& fieldname) const
{
  // CAUTION: the "=" are correct
  if ( shared_ptr<Record>  r = locateRecord(recordIdentifier) )  // implied "this->"
    if ( shared_ptr<Field> f = r->locateField(fieldname) )
      return f;
  s_logger->repx(logga::warn, "problems finding record and field", "");
  return shared_ptr<Field>();                // empty shared pointer
}

#ifdef _XUTEST // unit testing only

void
RecordSet::dump
(std::string marker)                         // optional marker to insert
{
  std::ostringstream put;
  put << std::boolalpha;
  put << marker << "\n";
  s_logger->putx(logga::dbug, put);

  BOOST_FOREACH( shared_ptr<Record> r, d_records )
    {
      put << r->getIdentifier()                << "\n";
      put << ""                                << "\n";
      put << "  enabled : " << r->getEnabled() << "\n";
      put << "  kind    : " << r->getKind()    << "\n";
      put << ""                                << "\n";

      BOOST_FOREACH( shared_ptr<Field> f, r->d_fields )
        {
          std::string raw(f->getRawStr());
          unsigned size = xeona::consoleWidth - 22 - 3;     // hard-coded in 'common.cc'
          if ( raw.length() > size )
            {
              raw.resize(size);
              raw += " >";
            }

          put << "  " << std::setw(20) << std::left
              << f->getName()
              << raw << "\n";
        }
      put << "\n";
      s_logger->putx(logga::dbug, put);      // print at record conclusion
    }

  put << marker << "\n";
  s_logger->putx(logga::dbug, put);
}

#endif // _XUTEST

// MANIPULATORS

// ---------------------------------------------------------
//  MEMBER FUNCTION : modifySteps
// ---------------------------------------------------------
//
//  CAUTION: use of unity causes faults on write out
//
//      Intended for use under "--mode 6" for one step only, but
//      DOES NOT work.  It causes faults when writing out
//      timeseries.
//
//      Hence this function has no current usage.
//
// ---------------------------------------------------------

bool
RecordSet::modifySteps
(const int steps)                            // warns if not unity
{
  // initial reporting
  s_logger->repx(logga::dbug, "entering member function, steps", steps);

  // preamble
  bool ret = false;                          // presume no change

  // integrity checks
  if ( steps <= 0 )
    {
      s_logger->repx(logga::warn, "steps value invalid", steps);
    }
  else if ( steps == 1 )
    {
      s_logger->repx(logga::warn, "steps value of unity no supported", steps);
    }

  // basic metadata
  const std::string sTimehorizon = xeona::timehorizon;
  const std::string sSteps       = "steps";

  // search (for reporting side-effects, recovered data not used in this case)
  shared_ptr<Record> recTimehorizon = locateRecord(sTimehorizon);
  if ( ! recTimehorizon )
    {
      s_logger->repx(logga::warn, "could not locate record", sTimehorizon);
      return false;
    }
  shared_ptr<Field> fieldSteps    = recTimehorizon->locateField(sSteps);
  if ( ! fieldSteps )
    {
      s_logger->repx(logga::warn, "could not locate field", sSteps);
      return false;
    }

  // update
  const std::string strSteps = boost::str(boost::format("%d") % steps);
  fieldSteps->overwriteRawStr(strSteps);
  ret = true;

  // reporting
  s_logger->repx(logga::dbug, "steps value updated using string", strSteps);

  // return
  return ret;

} // function 'RecordSet::modifySteps'

// ---------------------------------------------------------
//  MEMBER FUNCTION : modifyHours
// ---------------------------------------------------------

bool
RecordSet::modifyHours
(const int hours)
{
  // preamble
  bool ret = false;                          // presume no change

  // integrity checks
  if ( hours <= 0 )
    {
      s_logger->repx(logga::warn, "hours value invalid", hours);
      return ret;
    }

  // basic metadata
  const std::string sTimehorizon = xeona::timehorizon;
  const std::string sSteps       = "steps";
  const std::string sInterval    = "interval";

  // search
  shared_ptr<Record> recTimehorizon = locateRecord(sTimehorizon);
  if ( ! recTimehorizon )
    {
      s_logger->repx(logga::warn, "could not locate record", sTimehorizon);
      return false;
    }
  shared_ptr<Field> fieldSteps    = recTimehorizon->locateField(sSteps);
  shared_ptr<Field> fieldInterval = recTimehorizon->locateField(sInterval);
  if ( ! fieldSteps )
    {
      s_logger->repx(logga::warn, "could not locate field", sSteps);
      return false;
    }
  if ( ! fieldInterval )
    {
      s_logger->repx(logga::warn, "could not locate field", sInterval);
      return false;
    }
  const std::string strSteps    = fieldSteps->getRawStr();
  const std::string strInterval = fieldInterval->getRawStr();

  // conversion
  int steps = -1;                            // nonsensical value
  try
    {
      steps = boost::lexical_cast<int>(strSteps);
    }
  catch( boost::bad_lexical_cast& ae )
    {
      s_logger->repx(logga::warn, "lexical cast failed", ae.what());
    }

  int interval = -1;                         // nonsensical value
  try
    {
      interval = boost::lexical_cast<int>(strInterval);
    }
  catch( boost::bad_lexical_cast& ae )
    {
      s_logger->repx(logga::warn, "lexical cast failed", ae.what());
    }

  // calculate new step
  int newSteps = steps;
  if ( interval == 3600 )
    {
      newSteps = hours;                      // simple
    }
  else if ( interval != 0 )
    {
      // CAUTION: best avoid integer arithmetic
      const double temp
        = static_cast<double>(hours)
        * (3600.0 / static_cast<double>(interval));
      newSteps = static_cast<int>(std::floor(temp));   // or 'std::ceil' as required
    }
  else
    {
      s_logger->repx(logga::warn, "zero interval", interval);
    }

  // update as required
  if ( newSteps != steps )
    {
      const std::string strNewSteps = boost::str(boost::format("%d") % newSteps);
      fieldSteps->overwriteRawStr(strNewSteps);
      ret = true;
    }

  // reporting
  const bool exact = ( newSteps * interval == hours * 3600 );
  const std::string exactYesNo = exact ? "yes" : "no";
  std::ostringstream put;
  put << "  modify hours mode 8 report"                        << "\n"
      << "    sought hours      [h] : " << hours               << "\n"
      << "    exising interval  [s] : " << interval            << "\n"
      << "    existing steps    [-] : " << steps               << "\n"
      << "    new steps         [-] : " << newSteps            << "\n"
      << "    exact horizon         : " << exactYesNo          << "\n";
  s_logger->repx(logga::info, "additional reporting follows", "");
  s_logger->putx(logga::info, put);
  s_logger->flush();                         // the flush aided debugging

  // return
  return ret;

} // function 'RecordSet::modifyHours'

// ---------------------------------------------------------
//  MEMBER FUNCTION : modifySecondsHours
// ---------------------------------------------------------

bool
RecordSet::modifySecondsHours
(const int seconds,
 const int hours)
{
  // preamble
  bool ret = false;                          // presume no change

  // integrity checks
  if ( seconds <= 0 || hours <= 0 )
    {
      std::ostringstream oss;
      oss << seconds << " " << hours;
      s_logger->repx(logga::warn, "seconds and/or hours value invalid", oss.str());
      return ret;
    }

  // basic metadata
  const std::string sTimehorizon = xeona::timehorizon;
  const std::string sSteps       = "steps";
  const std::string sInterval    = "interval";

  // search
  shared_ptr<Record> recTimehorizon = locateRecord(sTimehorizon);
  if ( ! recTimehorizon )
    {
      s_logger->repx(logga::warn, "could not locate record", sTimehorizon);
      return false;
    }
  shared_ptr<Field> fieldSteps    = recTimehorizon->locateField(sSteps);
  shared_ptr<Field> fieldInterval = recTimehorizon->locateField(sInterval);
  if ( ! fieldSteps )
    {
      s_logger->repx(logga::warn, "could not locate field", sSteps);
      return false;
    }
  if ( ! fieldInterval )
    {
      s_logger->repx(logga::warn, "could not locate field", sInterval);
      return false;
    }
  const std::string strSteps    = fieldSteps->getRawStr();
  const std::string strInterval = fieldInterval->getRawStr();

  // conversion
  int steps = -1;                            // nonsensical value
  try
    {
      steps = boost::lexical_cast<int>(strSteps);
    }
  catch( boost::bad_lexical_cast& ae )
    {
      s_logger->repx(logga::warn, "lexical cast failed", ae.what());
    }

  int interval = -1;                         // nonsensical value
  try
    {
      interval = boost::lexical_cast<int>(strInterval);
    }
  catch( boost::bad_lexical_cast& ae )
    {
      s_logger->repx(logga::warn, "lexical cast failed", ae.what());
    }

  // "calculate" new interval
  const int newInterval = seconds;

  // update interval as required
  if ( newInterval != interval )
    {
      const std::string strNewInterval = boost::str(boost::format("%d") % newInterval);
      fieldInterval->overwriteRawStr(strNewInterval);
      ret = true;
    }

  // calculate new step
  int newSteps = steps;
  if ( seconds == 3600 )
    {
      newSteps = hours;                      // simple
    }
  else
    {
      const double temp                      // CAUTION: best avoid integer arithmetic
        = static_cast<double>(hours)
        * (static_cast<double>(interval) / static_cast<double>(seconds));
      newSteps = static_cast<int>(std::floor(temp));   // or 'std::ceil' as required
    }

  // update as required
  if ( newSteps != steps )
    {
      const std::string strNewSteps = boost::str(boost::format("%d") % newSteps);
      fieldSteps->overwriteRawStr(strNewSteps);
      ret = true;
    }

  // reporting
  const bool exact = ( newSteps * newInterval == steps * interval );
  const std::string exactYesNo = exact ? "yes" : "no";
  std::ostringstream put;
  put << "  modify seconds and hours mode 9 report"            << "\n"
      << "    sought seconds    [s] : " << seconds             << "\n"
      << "    sought hours      [h] : " << hours               << "\n"
      << "    existing interval [s] : " << interval            << "\n"
      << "    existing steps    [-] : " << steps               << "\n"
      << "    new interval      [s] : " << newInterval         << "\n"
      << "    new steps         [-] : " << newSteps            << "\n"
      << "    exact horizon         : " << exactYesNo          << "\n";
  s_logger->repx(logga::info, "additional reporting follows", "");
  s_logger->putx(logga::info, put);
  s_logger->flush();                         // the flush aided debugging

  // return
  return ret;

} // function 'RecordSet::modifySecondsHours'

// ---------------------------------------------------------
//  EXPLICIT TEMPLATE INSTANTIATIONS
// ---------------------------------------------------------
//
//  Place all required template instantiations here.  That is,
//  place all required (or potentially required) header-declared
//  class and member function template instantiations at the end
//  of the implementation file.
//
//  (Alternatively, move the relevant definitions to the header
//  file.  This latter approach would require the unnamed
//  namespace functors to be moved there as well -- thereby
//  making the header more complex than it need be.)
//
//  For simple information on template coding strategies, read
//  Cline (2006) section 35.  If this is insufficient for your
//  needs, refer to Lischner (2003 pp195-199) and Dattatri (2002
//  pp456-461).
//
//  Failure to properly instantiate a template will typically
//  result in a link-time error whenever a (constructor or
//  function) call is made.  The linker usually says: "undefined
//  reference to ...".  If no call is made, then the program
//  should compile normally and run without a murmur.
//
//    Cline, Marshall.  2006.  C++ FAQ lite.
//    [www.parashift.com/c++-faq-lite/templates.html]

template void Field::lexValue(shared_ptr<std::vector<int        > >);
template void Field::lexValue(shared_ptr<std::vector<double     > >);
template void Field::lexValue(shared_ptr<std::vector<bool       > >);
template void Field::lexValue(shared_ptr<std::vector<tribool    > >);
template void Field::lexValue(shared_ptr<std::vector<std::string> >);

template wrap<int        > Record::tieSingle(const std::string&);
template wrap<double     > Record::tieSingle(const std::string&);
template wrap<bool       > Record::tieSingle(const std::string&);
template wrap<tribool    > Record::tieSingle(const std::string&);
template wrap<std::string> Record::tieSingle(const std::string&);

template wrap<int        > Record::tieConstant(const int        );
template wrap<double     > Record::tieConstant(const double     );
template wrap<bool       > Record::tieConstant(const bool       );
template wrap<tribool    > Record::tieConstant(const tribool    );
template wrap<std::string> Record::tieConstant(const std::string);

template shared_ptr<std::vector<int        > > Record::tieTimeseries(const std::string&);
template shared_ptr<std::vector<double     > > Record::tieTimeseries(const std::string&);
template shared_ptr<std::vector<bool       > > Record::tieTimeseries(const std::string&);
template shared_ptr<std::vector<tribool    > > Record::tieTimeseries(const std::string&);
template shared_ptr<std::vector<std::string> > Record::tieTimeseries(const std::string&);

//  end of file

