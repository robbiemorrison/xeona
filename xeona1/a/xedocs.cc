//  XEONA ENERGY SYSTEMS MODELING ENVIRONMENT
//
//  file-create-name : xedocs.cc
//  file-create-date : Tue 23-Sep-2008 06:29 UTC
//  file-initiator   : Robbie Morrison <robbie@actrix.co.nz>
//
//  file-role        : processing of 'xedoc' entity documentation / implementation
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
//  $URL: file:///home/robbie/svn-root/xeona/futz/trunk/xeona1/a/xedocs.cc $

//  LOCAL AND SYSTEM INCLUDES

#include "xedocs.h"           // companion header for this file (place first)

#include "../a/logger.h"      // standard logging functionality (as required)
#include "../c/smart_ptr.h"   // toggle between Boost and TR1 smart pointers
#include ".././common.h"      // common definitions for project (place last)

#include <sstream>            // string-streams
#include <string>             // C++ strings
#include <utility>            // STL pair, make_pair()

#include <boost/algorithm/string.hpp>        // string recasing, trimming, splitting
#include <boost/algorithm/string_regex.hpp>  // additional regex support
#include <boost/foreach.hpp>                 // BOOST_FOREACH iteration macro
#include <boost/format.hpp>                  // printf style formatting
#include <boost/regex.hpp>                   // regular expression support

//  CODE

// ---------------------------------------------------------
//  FREE FUNCTION   : ::match
// ---------------------------------------------------------
//  Description  : simple interface for regex/host matching
//  Role         : Xedox searching
//  Techniques   : Boost::String_algo library, unnamed namespace
//  Status       : complete
//
//  Examples
//
//      ::match("ab" , "abc") is 'true'
//      ::match("^ab", "abc") is 'true'
//      ::match("^ab", "zab") is 'false'
//
//      also
//
//      ::match(""   , "abc") is 'false' (typical behavior is true)
//      ::match("ab" , ""   ) is 'false'
//
//  CAUTION: regex library
//
//      This function uses the Boost.String_alg library for regex
//      (regular expression) support and not the Boost.Regex
//      library directly.
//
// ---------------------------------------------------------

namespace
{
  bool                                       // 'true' if 'regex' matches, else 'false'
  match
  (const std::string& regex,                 // regex string (eg "^ab")
   const std::string& host)                  // host string  (eg "abc")
  {
    if ( regex.empty() ) return false;       // override "match found" default behavior
    boost::regex pattern(regex);
    return boost::find_regex(host, pattern);
  }

} // unnamed namespace

// ---------------------------------------------------------
//  FREE FUNCTION   : ::extractHeader
// ---------------------------------------------------------
//  Description  : extract header details from XEDOC entry using a regex
//  Role         : reporting, also acts as a check on XEDOC integrity
//  Techniques   : Boost.Regex library
//  Status       : complete
//
//  Design notes
//
//      The general pattern is: "    header: h/header.h"
//
//      A failure to find this pattern will often indicate a
//      poorly formed XEDOC.
//
//  See also
//
//      Becker (2007 pp347-504) devotes several chapters to the
//      Boost.Regex library.
//
//  CAUTION: script integration
//
//      This code must integrate with the '../scripts/xedoc'
//      script.
//
// ---------------------------------------------------------

namespace
{
  bool
  extractHeader
  (const std::string& xedoc,
   std::string&       header)
  {
    // local logging support
    static logga::spLogger logger = logga::ptrLogStream();  // logger bind
    logger->repx(logga::adhc, "entering free function", "");

    // some alternatives regular expressions are offered
    std::string rgxstr;
    rgxstr = "header: ([[:lower:][:digit:]/]+\\.h)";   // thus {a-z     0-9 /} plus ".h"
    rgxstr = "header: ([[:alnum:]/]+\\.h)";            // thus {a-z A-Z 0-9 /} plus ".h"

    // declare dedicated container for search results, 'smatch' is
    // typedef'ed to 'match_results<std::string::const_iterator>'
    boost::smatch match;

    // both the 'regex' constructor and the 'regex_search' function can throw
    try
      {
        boost::regex pattern(rgxstr);        // optional second arg: boost::regex::icase
        if ( boost::regex_search             // partial match variant, else 'regex_match'
             (xedoc,                         // so-called target sequence
              match,                         // search results
              pattern,
              boost::match_default) )        // search options
          {
            header = match[1];               // also known as sub-expression $1
            return true;
          }
        else
          {
            header = "";                     // empty the string
            return false;
          }
      }
    catch (const boost::regex_error& r )
      {
        logger->repx(logga::warn, "will rethrow exception", "");
        logger->flush();
        std::cout << "** boost::regex_error exception caught and rethrown: "
                  << r.what() << "\n"
                  << std::flush;
        throw;                               // rethrow
      }
    catch ( ... )
      {
        logger->repx(logga::warn, "will rethrow exception", "");
        logger->flush();
        std::cout << "** unspecified exception caught and rethrown" << "\n"
                  << std::flush;
        throw;                               // rethrow
      }
  } // function 'extractHeader'

} // unnamed namespace

// ---------------------------------------------------------
//  CLASS           : Xedocs
// ---------------------------------------------------------

// STATIC DATA

logga::spLogger Xedocs::s_logger = logga::ptrLogStream();   // bind logger on definition

// CREATORS

// ---------------------------------------------------------
//  MEMBER FUNCTION : Xedocs
// ---------------------------------------------------------

Xedocs::Xedocs() :
  d_fileName(xeona::xedocsFileName),         // set in 'common.cc'
  d_fileContents(xeona::xedocsFileContents), // "loaded" in 'common.cc'
  d_database()
{
  s_logger->repx(logga::dbug, "constructor call", "");
  s_logger->repx(logga::dbug, "xedocs filename", d_fileName);
  s_logger->repx(logga::dbug, "recovered xedocs char count", d_fileContents.length());

#ifdef _XUTEST
  std::ostringstream put;
  put << d_fileContents << "\n";
  s_logger->putx(logga::dbug, put);
#endif // _XUTEST

  if ( makeDatabase() == false )
    {
      s_logger->repx(logga::warn, "makeDatabase returned false", "");
    }
}

// ---------------------------------------------------------
//  MEMBER FUNCTION : ~Xedocs
// ---------------------------------------------------------

Xedocs::~Xedocs()
{
  s_logger->repx(logga::dbug, "destructor call", "");
}

// ACCESSORS

// ---------------------------------------------------------
//  MEMBER FUNCTION : findXedocForClass
// ---------------------------------------------------------
//  Description  : searches for entity class documentation based on class name
//  Role         : direct support for the '--class arg' option
//  Techniques   : returns enum giving find status, provides result by reference
//  Status       : complete -- but taken out of service for commit r2402
// ---------------------------------------------------------

const Xedocs::FindStatus
Xedocs::findXedocForClass
(const std::string soughtClass,
 std::string&      result)
{
  s_logger->repx(logga::dbug, "entering member function seeking", soughtClass);
  result = "";

  // abandon if database is empty
  if ( d_database.empty() )
    {
      return e_emptyDatabase;
    }

  // hunt using 'std::map::find' member function
  std::map<std::string, std::string>::iterator pos;
  pos = d_database.find(soughtClass);
  if ( pos != d_database.end() )
    {
      s_logger->repx(logga::dbug, "class found", soughtClass);
      result = pos->second;
      return e_classFound;
    }
  else
    {
      s_logger->repx(logga::warn, "class not found", soughtClass);
      return e_classNotFound;
    }
}

// ---------------------------------------------------------
//  MEMBER FUNCTION : findXedocForRegex
// ---------------------------------------------------------
//  Description  : searches for entity class documentation based on regex
//  Role         : direct support for the '--class arg' option
//  Techniques   : returns enum giving find status, provides result by reference
//  Status       : complete
//
//  Design notes
//
//      Call now placed in function 'main'.
//
// ---------------------------------------------------------

const Xedocs::FindStatus
Xedocs::findXedocForRegex
(const std::string regex,
 std::string&      result)
{
  s_logger->repx(logga::dbug, "entering member function seeking", "\"" + regex + "\"");
  result = "";

  // abandon if database is empty
  if ( d_database.empty() )
    {
      return e_emptyDatabase;
    }

  // loop database
  int count = 0;
  typedef std::pair<std::string, std::string> record;  // CAUTION: for BOOST_FOREACH
  BOOST_FOREACH( record rec, d_database )
    {
      if ( ::match(regex, rec.first) )       // function 'match' defined above
        {
          s_logger->repx(logga::xtra, "regex match found", regex);
          result += rec.second;
          result += "\n\n";                  // add trailing newline plus blank line
          ++count;
          if ( xeona::releaseStatus == false )
            {
              // undertake additional reporting
              std::ostringstream put;
              put << rec.second << "\n";
              s_logger->putx(logga::adhc, put);
            }
        }
    }

  // housekeeping
  if ( count > 0 )
    {
      result.erase(result.length() - 2);     // remove final blank line
      s_logger->repx(logga::info, "regex matches, count", count);
      return e_classFound;
    }
  else
    {
      s_logger->repx(logga::info, "regex not matched", regex);
      return e_classNotFound;
    }
}

// ---------------------------------------------------------
//  MEMBER FUNCTION : dumpClassNames
// ---------------------------------------------------------
//  Description  : gives naturally sorted list of map keys
//  Role         : direct support for the '--class *' option
//  Techniques   : simple traverse
//  Status       : complete
//
//  Design notes
//
//      Maps maintain sorted keys, see Josuttis (1999 p194).
//
// ---------------------------------------------------------

const int                                    // number of classes
Xedocs::dumpClassNames
(std::string& result)                        // newline-separated and sortedlist
{
#if 1 // 0 = omit header information, 1 = add header information

  s_logger->repx(logga::dbug, "entering member function", "with headers");
  result = "";
  std::map<std::string, std::string>::iterator pos;
  for ( pos  = d_database.begin();
        pos != d_database.end();
        ++pos)
    {
      std::string headerName;
      if ( ::extractHeader(pos->second, headerName) == false )
        {
          s_logger->repx(logga::warn, "no header match (check XEDOC)", "");
          headerName = "(not found)";
        }
      else
        {
          s_logger->repx(logga::adhc, "header name", headerName);
        }
      result += boost::str(boost::format("  %-40s %15s") % pos->first % headerName);
      result += "\n";
    }
  return d_database.size();

#else

  s_logger->repx(logga::dbug, "entering member function", "without headers");
  result = "";
  std::map<std::string, std::string>::iterator pos;
  for ( pos  = d_database.begin();
        pos != d_database.end();
        ++pos)
    {
      result += pos->first;
      result += "\n";
    }
  return d_database.size();

#endif // 0
}

// UTILITY FUNCTIONS

// ---------------------------------------------------------
//  MEMBER FUNCTION : makeDatabase
// ---------------------------------------------------------
//  Description  : fills the xedoc database
//  Role         : indirect support for the '--class arg' option, called by constructor
//  Techniques   : data is collected at compile-time (hence no ancillary file at run-time)
//  Status       : complete
//
//  Design notes
//
//      Map insert call based on Josuttis (1999 p203).
//
// ---------------------------------------------------------

bool
Xedocs::makeDatabase()
{
  s_logger->repx(logga::dbug, "entering member function", "");

  std::string buffer(d_fileContents);        // modifiable buffer

  // confirm container is empty
  if ( ! d_database.empty() )
    {
      s_logger->repx(logga::dbug, "database container not empty", "");
      return false;
    }

  // check for information
  if ( buffer.empty() )
    {
      s_logger->repx(logga::dbug, "'xedocs' file empty", "");
      return false;
    }

  // swap record separators for tabs after confirming no tabs
  if ( boost::algorithm::contains(buffer, "\t") )
    {
      s_logger->repx(logga::dbug, "'xedocs' file contains tab chars", "");
      return false;
    }
  boost::algorithm::replace_all(buffer, "\n\n\n", "\t");

  // spit into entity documentation
  std::vector<std::string> xedocs;
  boost::algorithm::split(xedocs,
                          buffer,
                          boost::algorithm::is_any_of("\t"),
                          boost::algorithm::token_compress_on);

  // split into lines
  BOOST_FOREACH( std::string xedoc, xedocs )
    {
      std::vector<std::string> lines;
      boost::algorithm::trim(xedoc);
      boost::algorithm::split(lines,
                              xedoc,
                              boost::algorithm::is_any_of("\n"),
                              boost::algorithm::token_compress_on);

      // split into words
      BOOST_FOREACH( std::string line, lines )
        {
          std::vector<std::string> words;
          boost::algorithm::trim(line);
          boost::algorithm::split(words,
                                  line,
                                  boost::algorithm::is_any_of(" "),
                                  boost::algorithm::token_compress_on);

          // load database as appropriate
          if ( words.size() == 3 )
            {
              if (    words.at(0) == "class"
                   && words.at(1) == ">"
                   && words.at(2).size() > 0 )
                {
                  std::string key   = words.at(2);
                  std::string value = xedoc;
                  if ( d_database.insert(std::make_pair(key, value)).second )
                    {
                      // successful insert
                    }
                  else
                    {
                      // key already exists
                      s_logger->repx(logga::warn, "duplicate class name present", key);
                      return false;
                    }
                }
            }
        }
    }
  s_logger->repx(logga::dbug, "leaving member function, entries", d_database.size());
  return true;
}

//  end of file

