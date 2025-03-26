//  XEONA ENERGY SYSTEMS MODELING ENVIRONMENT
//
//  file-create-name : label.h
//  file-create-date : Fri 24-Oct-2008 12:18 UTC
//  file-initiator   : Robbie Morrison <robbie@actrix.co.nz>
//
//  file-role        : helper class for solver labels  / header
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
//  $URL: file:///home/robbie/svn-root/xeona/futz/trunk/xeona1/c/label.h $

//  HEADER GUARD

#ifndef _LABEL_H_
#define _LABEL_H_

//  LOCAL AND SYSTEM INCLUDES

#include "../a/logger_fwd.h"  // some forward declarations from "logger.h"
#include "../c/smart_ptr.h"   // toggle between Boost and TR1 smart pointers

#include <string>             // C++ strings
#include <sstream>            // string-streams
#include <vector>             // STL sequence container

//  CODE

// ---------------------------------------------------------
//  CLASS           : Label
// ---------------------------------------------------------
//  Description  : helper class to collect and format OSP and solver labels
//  Role         : simplify label processing in OSP upload calls
//  Techniques   : overloaded function call operator, output streaming
//  Status       : complete
//
//  Example
//
//      Label lab1;                         // no data
//
//      int a = 4;
//      Label lab2("yes");                  // data added
//      lab2 << "hello";
//      lab2 << "world";
//      lab2 << a;                          // earlier set to 4
//      std::cout << "lab2 (as string) : " << lab2.str()        << std::endl;
//      std::cout << "lab2 (streaming) : " << lab2              << std::endl;
//      std::cout << "lab2 (extra)     : " << lab2.str("extra") << std::endl;
//
//      lab2 (as string) : yes-hello-world-4
//      lab2 (streaming) : yes-hello-world-4
//      lab2 (extra)     : yes-hello-world-4-extra
//
//  CAUTION: stream chaining
//
//      Chaining is not (currently) supported, for instance the
//      following will not compile:
//
//          lab1 << "first" << "second";
//
//  See also
//
//      See the unit test file for more examples.
//
// ---------------------------------------------------------

class Label
{
  // FRIENDS

  friend std::ostream& operator<< (std::ostream& os, const Label& label);

  // CREATORS

public:

  Label
  (const std::string& str = "");

  ~Label();

  // PUBLIC CALLS

public:

  void
  trim                                       // multiple pop without returning elements
  (int num = 1);                             // defaults to one pop

  void
  resetToCtor();                             // reset to construction-time state

  // CAUTION: the 'add' function template is defined in this
  // header to facilitate implicit template instantiation

  template <typename T>
  bool                                       // 'true' if added
  add
  (const T& in)
  {
    std::ostringstream oss;
    oss << std::boolalpha;                   // modify the default ios flags
    oss << in;                               // stream the input
    const std::string sin = oss.str();
    if ( sin.empty() ) return false;
    d_labelettes.push_back(sin);
    return true;
  }

  bool                                       // 'true' if added
  padd
  (const unsigned in,
   const unsigned pad = 2);                  // pad level, note the general default

  // CAUTION: the 'operator<<' function template is defined in
  // this header to facilitate implicit template instantiation
  //
  // CAUTION: regarding 'operator<<', see the implementation file
  // for explicit specializations covering 'int' and 'unsigned'
  // -- these call 'padd(in)' instead

  template <typename T>
  void
  operator<<                                 // streams as per any default ostream
  (const T& in)
  {
    add(in);
  }

  // CAUTION: the 'str(in)' function template is defined in this
  // header to facilitate implicit template instantiation

  template <typename T>
  std::string
  str                                        // stringify with temporary 'in' added
  (const T& in)                              // CAUTION: 'in' is not stored
  {
    const bool added   = add(in);            // element inserted
    std::string buffer = str();              // overloaded member function 'str()'
    if ( added ) trim(1);                    // element removed
    return buffer;
  }

  std::string
  padstr                                     // zero-pad the integer
  (const unsigned in,
   const unsigned pad = 2);                  // pad level, note default

  std::string
  str() const;                               // principal stringify function

  bool                                       // 'true' if empty
  empty() const;

  // INSTANCE DATA

private:

  const std::string           d_separator;   // separator string (a dot or underscore?)
  const std::string           d_ctorArg;     // argument supplied at construction-time
  std::vector<std::string>    d_labelettes;  // stack of substrings

};

// STREAMING SUPPORT

// ---------------------------------------------------------
//  FREE FUNCTION   : operator<< (std::ostream&, Label&)
// ---------------------------------------------------------

std::ostream&
operator<<
(std::ostream& os,
 const Label&  label);

#endif // _LABEL_H_

//  end of file

