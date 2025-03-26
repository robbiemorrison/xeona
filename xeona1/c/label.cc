//  XEONA ENERGY SYSTEMS MODELING ENVIRONMENT
//
//  file-create-name : label.cc
//  file-create-date : Fri 24-Oct-2008 12:18 UTC
//  file-initiator   : Robbie Morrison <robbie@actrix.co.nz>
//
//  file-role        : helper class for solver labels  / implementation
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
//  $URL: file:///home/robbie/svn-root/xeona/futz/trunk/xeona1/c/label.cc $

//  LOCAL AND SYSTEM INCLUDES

#include "label.h"            // companion header for this file (place first)

#include "../a/logger.h"      // standard logging functionality (as required)
#include "../c/smart_ptr.h"   // toggle between Boost and TR1 smart pointers
#include ".././common.h"      // common definitions for project (place last)

#include <string>             // C++ strings
#include <sstream>            // string-streams

#include <boost/foreach.hpp>  // BOOST_FOREACH iteration macro

//  CODE

// ---------------------------------------------------------
//  CLASS           : Label
// ---------------------------------------------------------
//  Description  : helper class to collect and format OSP and solver labels
//  Role         : simplify label processing in OSP upload calls
//  Techniques   : overloaded function call operator, output streaming
//  Status       : complete
// ---------------------------------------------------------

// CREATORS

Label::Label
(const std::string& str) :
  d_separator(xeona::ospTagSep),             // separator string set here
  d_ctorArg(str),
  d_labelettes()                             // empty vector
{
  if ( ! str.empty() ) d_labelettes.push_back(str);
}

Label::~Label()
{
}

// PUBLIC CALLS

void
Label::trim
(int num)
{
  while ( num-- )
    {
      if ( empty() ) break;                  // member function 'empty'
      d_labelettes.pop_back();               // 'pop_back' does not return the element
    }
}

void
Label::resetToCtor()
{
  d_labelettes.clear();
  if ( ! d_ctorArg.empty() ) d_labelettes.push_back(d_ctorArg);
}

bool                                         // 'true' if added
Label::padd                                  // zero-pad the integer
(const unsigned in,
 const unsigned pad)                         // pad level, note default
{
  std::ostringstream oss;
  oss << std::setfill('0') << std::setw(pad) << in;
  const std::string sin = oss.str();
  return add(sin);
}

std::string
Label::str() const                           // principal stringify function
{
  std::string buffer = "";
  BOOST_FOREACH( std::string s, d_labelettes )
    {
      buffer += s;
      buffer += d_separator;
    }
  int buflen = buffer.length();
  int seplen = d_separator.length();
  if ( buflen > 0 ) buffer.erase(buflen - seplen);     // the remove final separator
  return buffer;
}

std::string
Label::padstr                                // zero-pad the integer
(const unsigned in,
 const unsigned pad)                         // pad level, note default
{
  const bool added   = padd(in, pad);        // element inserted
  std::string buffer = str();                // overloaded member function 'str()'
  if ( added ) trim(1);                      // element removed
  return buffer;
}

bool
Label::empty() const
{
  return d_labelettes.empty();
}

// EXPLICIT TEMPLATE SPECIALIZATIONS (TYPE DEDUCED)

// These are type-deduced template specializations, as opposed to
// explicit template instantiations (noting that the latter are
// more often used in this codebase).

template <>
void
Label::operator<<                            // streams as per any default ostream
(const int& in)
{
  padd(in);                                  // padded add
}

template <>
void
Label::operator<<                            // streams as per any default ostream
(const unsigned& in)
{
  padd(in);                                  // padded add
}

// STREAMING SUPPORT

// ---------------------------------------------------------
//  FREE FUNCTION   : operator<< (std::ostream&, Label&)
// ---------------------------------------------------------

std::ostream&
operator<<
(std::ostream& os,
 const Label&  label)
{
  os << label.str();
  return os;
}

//  end of file

