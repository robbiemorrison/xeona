//  XEONA ENERGY SYSTEMS MODELING ENVIRONMENT
//
//  file-create-name : ospinfo.h
//  file-create-date : Mon 19-Oct-2009 11:04 UTC
//  file-initiator   : Robbie Morrison <robbie@actrix.co.nz>
//
//  file-role        : domain mode interpretation / header
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
//  $URL: file:///home/robbie/svn-root/xeona/futz/trunk/xeona1/f/ospinfo.h $
//
//  GENERAL NOTES FOR THIS FILE

//  HEADER GUARD

#ifndef _OSPINFO_H_
#define _OSPINFO_H_

//  LOCAL AND SYSTEM INCLUDES

#include "../f/ospmodes.h"    // domain mode enums (header only)

#include "../a/logger_fwd.h"  // some forward declarations from "logger.h"
#include "../c/smart_ptr.h"   // toggle between Boost and TR1 smart pointers

#include <sstream>            // string-streams
#include <string>             // C++ strings
#include <vector>             // STL sequence container

//  CODE

// ---------------------------------------------------------
//  CLASS           : DomainModeInfo
// ---------------------------------------------------------

class DomainModeInfo
{
  // FRIENDS

  friend class DomainModeDatabase;

  // CREATORS

private:

  DomainModeInfo
  (const xeona::DomainMode mode,
   const std::string       shortMsg,
   const std::string       longMsg);

  // ACCESSORS

private:

  std::string
  shortformat() const;

  std::string
  longformat() const;

  // INSTANCE DATA

private:

  xeona::DomainMode    d_mode;               // mode
  std::string          d_shortMsg;           // based on the enum identifier
  std::string          d_longMsg;            // long-based message
  std::string          d_modeKind;           // set here

};

// ---------------------------------------------------------
//  CLASS           : DomainModeDatabase
// ---------------------------------------------------------

class DomainModeDatabase
{
public:

  DomainModeDatabase();

  std::vector<std::string>
  longform
  (const int domainModes) const;

  std::string
  getInfo
  (const int domainMode) const;

  void
  test
  (std::ostringstream& oss) const;

  // INTERNAL DATA

private:

  std::vector<DomainModeInfo>    d_infos;    // database
  static logga::spLogger         s_logger;   // shared_ptr to single logger object

};

// ---------------------------------------------------------
//  FREE FUNCTION   : xeona::infoDomainModeLong (twice)
// ---------------------------------------------------------
//  Description  : interpret 'xeona::DomainMode' or integer-equivalent values
//  Role         : use in logging messages
//  Techniques   : (nothing special)
//  Status       : complete
//
//  Usage
//
//      The following example assumes that the domain mode value
//      captures just one domain mode.
//
//          std::vector<std::string> modes = xeona::infoDomainModeLong(d_commitmentMode);
//          std::string mode = "(some problem)";           // default value
//          if ( modes.size() == 1 ) mode = modes.front(); // unique mode encountered
//
// ---------------------------------------------------------

namespace xeona
{
  std::vector<std::string>
  infoDomainModeLong
  (const int domainModes);                   // can be non-pure

  std::string
  infoDomainModeLong
  (const int domainModes,                    // can be non-pure
   const int padding);

} // namespace 'xeona'

// ---------------------------------------------------------
//  FREE FUNCTION   : xeona::infoDomainModePure (twice)
// ---------------------------------------------------------
//  Description  : interpret 'xeona::DomainMode' or integer-equivalent values
//  Role         : use in logging messages
//  Techniques   : 'boost::trim'
//  Status       : complete
//
//  Usage
//
//      This function assumes 'domainModes' is pure, otherwise it
//      returns the string:
//
//          "mode found but mixed"
//
//      In any case, a non-empty string is always returned.
//
//      The second form is specifically for
//      'xeona::geometricProgression' in order to avoid an
//      "endless ping-pong of embedded calls [which] will rapidly
//      segfault"
//
// ---------------------------------------------------------

namespace xeona
{
  std::string                                // long interpretation if pure, else info
  infoDomainModePure                         // trailing newline NOT added
  (const int domainModes);                   // one-line report if pure (non-mixed)

  std::string                                // long interpretation if pure, else info
  infoDomainModePure                         // trailing newline NOT added
  (const std::vector<int>& domainModes);     // one-line report if pure (non-mixed)

} // namespace 'xeona'

#endif // _OSPINFO_H_

//  end of file

