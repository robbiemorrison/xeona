//  XEONA ENERGY SYSTEMS MODELING ENVIRONMENT
//
//  file-create-name : xemgen.h
//  file-create-date : Mon 11-Aug-2008 14:46 UTC
//  file-initiator   : Robbie Morrison <robbie@actrix.co.nz>
//
//  file-role        : class to generate well-formatted XEM models / header
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
//  $URL: file:///home/robbie/svn-root/xeona/futz/trunk/xeona1/c/xemgen.h $

//  HEADER GUARD

#ifndef _XEMGEN_H_
#define _XEMGEN_H_

//  LOCAL AND SYSTEM INCLUDES

#include "../a/logger_fwd.h"  // some forward declarations from "logger.h"
#include "../c/smart_ptr.h"   // toggle between Boost and TR1 smart pointers
#include ".././common.h"      // common definitions for project (place last)

#include <string>             // C++ strings
#include <sstream>            // string-streams

//  CODE

// ---------------------------------------------------------
//  CLASS           : XemGenerator
// ---------------------------------------------------------
//  Description  : generate well-formatted XEM model data
//  Role         : used by command-line options '--xem' and '--inbuilt' via class 'Xem'
//  Techniques   : Boost.Format library
//  Status       : complete
//
//  CAUTION: string arguments
//
//      These should remain "const std::string" and not be "const
//      std::string&" -- otherwise string literals cannot be
//      placed in function calls.
//
// ---------------------------------------------------------

class XemGenerator
{
private:

  enum EntryKind
    {
      e_unknown    = 0,
      e_record,
      e_input,                               // with '>'
      e_output,                              // with '<'
      e_comment,
      e_rule,                                // covers standard rule and domain rule
      e_ident,                               // $Key: $
      e_meta,                                // xem-role:
      e_verbatim
    };

private:

  XemGenerator(const XemGenerator& orig);              // copy constructor
  XemGenerator& operator= (const XemGenerator& orig);  // copy assignment operator

public:

  XemGenerator                               // CAUTION: cannot disable zero-argument ctor
  (const int tab = 45);                      // note default

  ~XemGenerator();

  // RECORD LEVEL CALLS

  void
  entity
  (const std::string klass,
   const std::string identifier,
   const std::string preamble = "entity");   // else "inbuilt"

  void
  special
  (const std::string special);

  void
  note();                                    // usually followed by comment calls

  // FIELD LEVEL CALLS

  void
  in
  (const std::string key,
   const std::string value);

  void
  out
  (const std::string key,
   const std::string value);

  void
  inq
  (const std::string key,
   const std::string quote);                 // quote as single string

  void
  outq
  (const std::string key,
   const std::string quote);

  void
  inQ                                        // multi-quote variant
  (const std::string key,
   const std::string quote);                 // quote as timeseries string

  void
  outQ                                       // multi-quote variant
  (const std::string key,
   const std::string quote);                 // quote as timeseries string

  // PACKAGED CALLS

  void
  horizon
  (const int steps,
   const int interval = 3600);               // note default

  void
  overseer();

  void
  domainController
  (const std::string domconId                // omit the "entity."
   = "domain-controller-0");

  void
  emacs();                                   // insert emacs settings in note block

  // OTHER CALLS

  void
  com();                                     // blank line with in a set of comment

  void
  com                                        // comment
  (const std::string comment,
   const unsigned    pad
   = xeona::modelFieldIndent + 2);           // defaults to standard comment padding

  void
  hed                                        // header: with correct indent
  (const std::string header,
   const unsigned    pad
   = xeona::modelFieldIndent);

  void
  ident                                      // "Revision" to "$Revision: 8750 $"
  (const std::string key,
   const unsigned    pad
   = xeona::modelFieldIndent);               // defaults to standard alignment

  void
  meta                                       // "role" to "xem-role:"
  (const std::string key,
   const unsigned    pad
   = xeona::modelFieldIndent);               // defaults to standard alignment

  void
  verbatim
  (const std::string verbatim);

  void
  rule                                       // standard rule
  (const std::string header);

  void
  dule                                       // domain rule
  (const std::string tag,                    // domain tag, maintains case
   const std::string remark = "");           // optional end of line remark

  void
  end();                                     // end marker

  void
  blanks
  (const unsigned count);

  // OUTPUT

  void
  print
  (std::ostream& os);

  std::string
  string();

private:

  void
  field
  (std::string key,
   std::string angle,
   std::string value);

private:

  EntryKind                  d_last;         // previous entry kind
  const int                  d_tab;          // alignment tab for '<' and '>'
  std::ostringstream         d_oss;          // internal buffer

  static logga::spLogger     s_logger;       // shared_ptr to single logger object

};

#endif // _XEMGEN_H_

//  end of file

