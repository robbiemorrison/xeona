//  XEONA ENERGY SYSTEMS MODELING ENVIRONMENT
//
//  file-create-name : app_except.h
//  file-create-date : Tue 28-Apr-2009 10:14 UTC
//  file-initiator   : Robbie Morrison <robbie@actrix.co.nz>
//
//  file-role        : entity exception classes / header
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
//  $URL: file:///home/robbie/svn-root/xeona/futz/trunk/xeona1/a/exent.h $
//
//  GENERAL NOTES FOR THIS FILE
//
//  This unit implements a hierarchy of custom exception classes
//  for application use.  This hierarchy does not derive from
//  'std::exception'.

//  HEADER GUARD

#ifndef _EXENT_H_
#define _EXENT_H_

//  LOCAL AND SYSTEM INCLUDES

#include "../a/exbase.h"      // xeona exception base class

#include "../c/smart_ptr.h"   // toggle between Boost and TR1 smart pointers

#include <iostream>           // standard io
#include <sstream>            // string-streams
#include <string>             // C++ strings

//  CAUTION: the '<stdexcept>' header is not needed here but is
//  useful in client code which employs exception specifications
//  in member function declarations: type func(..) throw(..);

#include <stdexcept>          // standard exception classes, runtime_error()

//  CODE

namespace xeona
{
  // ---------------------------------------------------------
  //  CLASS           : xeona::ent_exception (abstract)
  // ---------------------------------------------------------
  //  Description  : abstract base class for entity exceptions
  //  Role         : use by entity authors within concrete entities
  //  Techniques   : exception class, inheritance tree
  //  Note         : can (and probably should) be further specialized
  //  Status       : complete
  //
  //  Design notes
  //
  //      Users will need to include "a/ent_except.h" in their
  //      unit header and add 'a/ent_except.cc' to their unit
  //      dependencies in 'machunit'.
  //
  // ---------------------------------------------------------

  class ent_exception :
    public exception
  {
  private:

    ent_exception& operator= (const ent_exception& orig);   // copy assignment operator

  public:

    ent_exception();

    virtual ~ent_exception() = 0;            // create abstract class

  };

  // ---------------------------------------------------------
  //  CLASS           : xeona::entity_issue
  // ---------------------------------------------------------
  //  Description  : generalized entity exception
  //  Role         : use by entity authors within concrete entities
  //  Techniques   : exception class, inheritance tree
  //  Status       : complete
  // ---------------------------------------------------------

  class entity_issue :
    public ent_exception
  {
  private:

    entity_issue();                                         // zero-argument ctor
    entity_issue& operator= (const entity_issue& orig);     // copy assignment operator

  public:

    entity_issue
    (const std::string msgExpl);   // should be suitably formatted with trailing newline

  };

  // ---------------------------------------------------------
  //  CLASS           : xeona::entity_issue_2
  // ---------------------------------------------------------
  //  Description  : formatted entity exception
  //  Role         : use by entity authors within concrete entities
  //  Techniques   : exception class, inheritance tree
  //  Status       : complete
  // ---------------------------------------------------------

  class entity_issue_2 :
    public ent_exception
  {
  private:

    entity_issue_2();                                        // zero-argument ctor
    entity_issue_2& operator= (const entity_issue_2& orig);  // copy assignment operator

  public:

    entity_issue_2
    (const std::string klass,                // "k" becase "class" is a reserved word
     const std::string function,             // try __func__ or __PRETTY_FUNCTION__ macro
     const std::string identifier,
     const std::string msgLine);             // one line without trailing newline
  };

  // ---------------------------------------------------------
  //  CLASS           : xeona::bidset_selections
  // ---------------------------------------------------------
  //  Description  : bidset selections invalid
  //  Role         : function  'AsopLmpBidParam::AsopLmpBidParam'
  //  Techniques   : exception class, inheritance tree
  //  Status       : complete
  // ---------------------------------------------------------

  class bidset_selections :
    public ent_exception
  {
  private:

    bidset_selections();                                              // zero-arg ctor
    bidset_selections& operator= (const bidset_selections& orig );    // copy assgn oper

  public:

    bidset_selections
    (const std::string klass,
     const std::string function,
     const std::string myId,
     const int         min,
     const int         max,
     const int         count);
  };

} // namespace 'xeona'

#endif // _EXENT_H_

//  end of file

