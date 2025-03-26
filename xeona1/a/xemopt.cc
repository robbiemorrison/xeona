//  XEONA ENERGY SYSTEMS MODELING ENVIRONMENT
//
//  file-create-name : xemopt.cc
//  file-create-date : Wed 20-May-2009 16:32 UTC
//  file-initiator   : Robbie Morrison <robbie@actrix.co.nz>
//
//  file-role        : skeleton xem model generator / implementation
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
//  $URL: file:///home/robbie/svn-root/xeona/futz/trunk/xeona1/a/xemopt.cc $
//
//  GENERAL NOTES FOR THIS FILE

//  LOCAL AND SYSTEM INCLUDES

#include "xemopt.h"           // companion header for this file (place first)

#include "../a/logger.h"      // standard logging functionality (as required)
#include "../c/smart_ptr.h"   // toggle between Boost and TR1 smart pointers
#include ".././common.h"      // common definitions for project (place last)

#include <string>             // C++ strings
#include <sstream>            // string-streams

#include <boost/format.hpp>   // printf style formatting

//  CODE

// STATIC DEFINITIONS

logga::spLogger Xem::s_logger = logga::ptrLogStream();

// ---------------------------------------------------------
//  MEMBER FUNCTION : Xem
// ---------------------------------------------------------

Xem::Xem
(std::ostream& os,                           // can be either console or file ostream
 const int     svnRev,                       // usually 'xeona::svnRev'
 const int     tab) :                        // angle bracket alignment, note default
   d_os(os),
   d_svnRev(svnRev),                         // minimum svn revision
   d_xemgen(tab)                             // passed thru
{
  s_logger->repx(logga::dbug, "constructor call", "");
}

// ---------------------------------------------------------
//  MEMBER FUNCTION : ~Xem
// ---------------------------------------------------------

Xem::~Xem()
{
  s_logger->repx(logga::dbug, "destructor call", "");
}

// ---------------------------------------------------------
//  MEMBER FUNCTION : head
// ---------------------------------------------------------
//  Description  : adds premable, including 'ident' info
//  Role         : client call
//  Techniques   : (nothing special)
//  Status       : complete
//
//  Design notes
//
//      See the manpage for 'ident' for details.  Useful for
//      'subversion' but not 'git' -- although the latter version
//      control system will not mind.
//
// ---------------------------------------------------------

void
Xem::head()
{
  XemGenerator& x = d_xemgen;                // for convenience

  x.note();
  x.ident("Revision");
  x.ident("Date");
  x.ident("Author");
  x.ident("URL");
  x.meta("role");
  x.meta("status");

  x.rule("program admin");

  // CAUTION: must generate a "" for empty in-strings, hence "\"\""
  // but empty out-strings are okay as simply ""

  x.special("program.last-run");
  x.out("process-id"                            , "");
  x.out("run-kind"                              , "");
  x.out("used-svn"                              , "");
  x.out("timestamp-start (UTC)"                 , "");
  x.out("simulate-time (hh::mm::ss or s.s)"     , "");
  x.out("simulate-return (not application exit)", "");

  x.special("program.data-format");
  x.in("minimum-svn", boost::str(boost::format("%d") % d_svnRev));

  x.special("program.run-script-settings");
  x.in("script-model-status s"   , "\"incomplete\"");
  x.in("script-run-me b"         , "0");
  x.in("script-option-exittrip i", "1");
  x.in("script-option-nodata b"  , "0");
  x.in("script-option-jumpy b"   , "0");

  x.special("program.study-description");
  x.in("study-name s"            , "\"\"");
  x.in("scenario-name s"         , "\"\"");
  x.in("scenario-author s"       , "\"\"");
  x.in("scenario-leta s (+,a-z)" , "\"\"");

  x.special("program.r-processing");
  x.in("r-policy i"   , "31");
  x.in("r-title s"    , "\"generated\"");
  x.in("r-plot-list s", "\"\"");
  x.in("r-highlight-output s", "\"\"");

  x.special("program.post-processing");
  x.in("summarize-list s", "\"\"");
  x.in("report-list s",    "\"\"");

  x.print(d_os);
}

// ---------------------------------------------------------
//  MEMBER FUNCTION : mand
// ---------------------------------------------------------
//  Description  : add the mandatory 'TimeHorizon' and 'Overseer' entities
//  Role         : client call
//  Techniques   : (nothing special)
//  Status       : complete
// ---------------------------------------------------------

void
Xem::mand
(const int steps)
{
  XemGenerator& x = d_xemgen;                // for convenience

  x.rule("mandatory entities");

  x.horizon(steps);
  x.overseer();

  x.print(d_os);
}

// ---------------------------------------------------------
//  MEMBER FUNCTION : more
// ---------------------------------------------------------
//  Description  : add a non-mandatory 'DomainController' entity
//  Role         : client call
//  Techniques   : (nothing special)
//  Status       : complete
//
//  Design notes
//
//      Note that 'domconId' may need the value
//      "domain-controller-x" to align with the 'Overseer'
//      entity.
//
// ---------------------------------------------------------

void
Xem::more
(const std::string& tag)                     // note the default value
{
  XemGenerator& x = d_xemgen;                // for convenience

  const std::string TAG      = boost::to_upper_copy(tag);
  const std::string domconId = boost::str(boost::format("domain-controller-%s") % tag);

  x.dule(TAG, "first domain");               // produce a domain rule
  x.domainController(domconId);

  x.print(d_os);
}

// ---------------------------------------------------------
//  MEMBER FUNCTION : blok
// ---------------------------------------------------------
//  Description  : add a body skeleton
//  Role         : client call
//  Techniques   : (nothing special)
//  Status       : complete
// ---------------------------------------------------------

void
Xem::blok
(const std::string& annot,
 const std::string& remark)
{
  XemGenerator& x = d_xemgen;                // for convenience

  x.rule(annot);
  x.com(remark);

  x.print(d_os);
}

// ---------------------------------------------------------
//  MEMBER FUNCTION : tail
// ---------------------------------------------------------
//  Description  : add some trailing information
//  Role         : client call
//  Techniques   : (nothing special)
//  Status       : complete
// ---------------------------------------------------------

void
Xem::tail()
{
  XemGenerator& x = d_xemgen;                // for convenience

  x.rule("tail");

  x.note();
  x.ident("Id");

  x.note();
  x.emacs();

  x.end();
  x.blanks(1);

  x.print(d_os);
}

// ---------------------------------------------------------
//  MEMBER FUNCTION : rule
// ---------------------------------------------------------
//  Description  : add a rule with annotation -- which is technically a comment
//  Role         : client call, currently used by unit 'c/inbuilt'
//  Techniques   : (nothing special)
//  Status       : complete
// ---------------------------------------------------------

void
Xem::rule
(const std::string& annotation)
{
  XemGenerator& x = d_xemgen;                // for convenience

  x.rule(annotation);

  x.print(d_os);
}

// ---------------------------------------------------------
//  MEMBER FUNCTION : blank
// ---------------------------------------------------------
//  Description  : add a blank line
//  Role         : client call
//  Techniques   : (nothing special)
//  Status       : complete
// ---------------------------------------------------------

void
Xem::blank()
{
  XemGenerator& x = d_xemgen;                // for convenience

  x.blanks(1);

  x.print(d_os);
}

// ---------------------------------------------------------
//  MEMBER FUNCTION : flush
// ---------------------------------------------------------
//  Description  : explicitly flush underlying stream
//  Role         : client call
//  Techniques   : (nothing special)
//  Status       : complete
// ---------------------------------------------------------

void
Xem::flush()
{
  d_os << std::flush;
}

//  end of file

