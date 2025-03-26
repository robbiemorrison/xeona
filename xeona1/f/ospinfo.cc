//  XEONA ENERGY SYSTEMS MODELING ENVIRONMENT
//
//  file-create-name : ospinfo.cc
//  file-create-date : Mon 19-Oct-2009 11:04 UTC
//  file-initiator   : Robbie Morrison <robbie@actrix.co.nz>
//
//  file-role        : domain mode interpretation / implementation
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
//  $URL: file:///home/robbie/svn-root/xeona/futz/trunk/xeona1/f/ospinfo.cc $
//
//  GENERAL NOTES FOR THIS FILE

//  LOCAL AND SYSTEM INCLUDES

#include "ospinfo.h"          // companion header for this file (place first)

#include "../c/util2.h"       // free functions which offer general utilities 2
#include "../c/util1.h"       // free functions which offer general utilities 1

#include "../a/logger.h"      // standard logging functionality (as required)
#include "../c/smart_ptr.h"   // toggle between Boost and TR1 smart pointers
#include ".././common.h"      // common definitions for project (place last)

#include <string>             // C++ strings
#include <sstream>            // string-streams

#include <boost/algorithm/string.hpp>   // string recasing, trimming, splitting
#include <boost/foreach.hpp>            // BOOST_FOREACH iteration macro
#include <boost/format.hpp>             // printf style formatting

//  CODE

// ---------------------------------------------------------
//  CLASS           : DomainModeInfo
// ---------------------------------------------------------

DomainModeInfo::DomainModeInfo
(const xeona::DomainMode    mode,
 const std::string          shortMsg,
 const std::string          longMsg) :
  d_mode(mode),
  d_shortMsg(shortMsg),
  d_longMsg(longMsg),
  d_modeKind()
{
  // set mode kind string based on 'd_mode'
  if      ( d_mode  < xeona::e_modeNotSpecified ) d_modeKind = "out-of-range";
  else if ( d_mode == xeona::e_modeNotSpecified ) d_modeKind = "invalid";
  else if ( d_mode <= xeona::e_capacityModes    ) d_modeKind = "capacity";
  else if ( d_mode <= xeona::e_commitmentModes  ) d_modeKind = "commitment";
  else if ( d_mode <= xeona::e_maxAggregate     ) d_modeKind = "invalid";
  else                                            d_modeKind = "out-of-range";
}

std::string
DomainModeInfo::shortformat() const
{
  std::ostringstream oss;
  oss << boost::format("%5d %s") % d_mode % d_shortMsg;
  return oss.str();
}

std::string
DomainModeInfo::longformat() const
{
  std::ostringstream oss;
  oss << boost::format("%5d = %s : %s") % d_mode % d_modeKind % d_longMsg;
  return oss.str();
}

// ---------------------------------------------------------
//  CLASS           : DomainModeDatabase
// ---------------------------------------------------------

logga::spLogger DomainModeDatabase::s_logger = logga::ptrLogStream();

DomainModeDatabase::DomainModeDatabase() :
  d_infos()
{
  // initial reporting
  s_logger->repx(logga::xtra, "constructor call", "");

  // NOT-SET VALUE
  d_infos.push_back(DomainModeInfo(xeona::e_modeNotSpecified,    // 0
                                   "modeNotSpecified",
                                   "mode not specified"));
  // CONSTRAIN MODES
  d_infos.push_back(DomainModeInfo(xeona::e_usePresets,          // 1
                                   "usePresets",
                                   "use XEM data capacities, no run-time calculation"));
  d_infos.push_back(DomainModeInfo(xeona::e_withholdOkay,        // 2
                                   "withholdOkay",
                                   "withholding acceptable"));
  d_infos.push_back(DomainModeInfo(xeona::e_withholdBan,         // 4
                                   "withholdBan",
                                   "withholding banned, so normal technical capacity"));
  d_infos.push_back(DomainModeInfo(xeona::e_crisisOperation,     // 8
                                   "crisisOperation",
                                   "relax all further non-mandatory constraints"));
  // COMMITMENT MODES
  d_infos.push_back(DomainModeInfo(xeona::e_shortrunFin,         // 32
                                   "shortrunFin",
                                   "short-run financial cost minimization"));
  d_infos.push_back(DomainModeInfo(xeona::e_shortrunGhg,         // 64
                                   "shortrunGhg",
                                   "short-run GHG contribution minimization"));
  d_infos.push_back(DomainModeInfo(xeona::e_shortrunNox,         // 128
                                   "shortrunNox",
                                   "short-run NOx contribution minimization"));
  d_infos.push_back(DomainModeInfo(xeona::e_shortrunDep,         // 256
                                   "shortrunDep",
                                   "short-run depletable resource use minimization"));
  d_infos.push_back(DomainModeInfo(xeona::e_shortrunLuc,         // 512
                                   "shortrunLuc",
                                   "short-run land use minimization"));
  d_infos.push_back(DomainModeInfo(xeona::e_auctionLmp,          // 2048
                                   "auctionLmp",
                                   "locational marginal pricing auction"));
  d_infos.push_back(DomainModeInfo(xeona::e_adminMerit,          // 4096
                                   "adminMerit",
                                   "prescribed merit order"));
  d_infos.push_back(DomainModeInfo(xeona::e_adminFirst,          // 8192
                                   "adminFirst",
                                   "first feasible solution"));
  // completion reporting
  const int infos = d_infos.size();
  s_logger->repx(logga::adhc, "constructor call finish, infos size", infos);
}

std::string
DomainModeDatabase::getInfo
(const int domainMode) const
{
  BOOST_FOREACH( DomainModeInfo d, d_infos )
    {
      if ( d.d_mode == domainMode )
        {
          const std::string buffer = d.longformat();
          s_logger->repx(logga::adhc, "returning", boost::trim_copy(buffer));
          return buffer;
        }
    }
  s_logger->repx(logga::adhc, "returning", "empty string");
  return "";
}

std::vector<std::string>
DomainModeDatabase::longform
(const int domainModes) const
{
  // initial reporting
  s_logger->repx(logga::adhc, "entering member function, mode", domainModes);

  // active code
  std::vector<std::string> buffer;
  std::vector<int> reds = xeona::geometricProgression(domainModes);
  s_logger->repx(logga::adhc, "reds vector size", reds.size());

  // loop the 'reds'
  BOOST_FOREACH( int i, reds )
    {
      if ( i == 0 ) continue;                // skip the zeros
      std::string buf = getInfo(i);
      if ( ! buf.empty() ) buffer.push_back(buf);
    }

  // return
  return buffer;
}

void
DomainModeDatabase::test
(std::ostringstream& oss) const
{
  BOOST_FOREACH( DomainModeInfo d, d_infos )
    {
      oss << "  " << d.longformat() << "\n";
    }
}

namespace xeona
{

  // ---------------------------------------------------------
  //  FREE FUNCTION   : xeona::infoDomainModeLong (twice)
  // ---------------------------------------------------------

  std::vector<std::string>
  infoDomainModeLong
  (const int domainModes)                    // can be non-pure (mixed)
  {
    static DomainModeDatabase database;      // CAUTION: this variable is static
    return database.longform(domainModes);   // calls 'xeona::geometricProgression'
  }

  std::string
  infoDomainModeLong
  (const int domainModes,                    // can be non-pure (mixed)
   const int padding)
  {
    std::vector<std::string> interprets = infoDomainModeLong(domainModes);
    std::ostringstream oss;
    const std::string leftMargin(padding, ' ');
    BOOST_FOREACH( std::string s, interprets )
      {
        oss << leftMargin << s << "\n";
      }
    return oss.str();
  }

  // ---------------------------------------------------------
  //  FREE FUNCTION   : xeona::infoDomainModePure (twice)
  // ---------------------------------------------------------

  std::string
  infoDomainModePure
  (const int domainModes)
  {
    std::vector<std::string> results = infoDomainModeLong(domainModes);
    std::string msg = "(not overwritten)";
    switch ( results.size() )
      {
      case 0:  msg = "mode not found";       break;
      case 1:  msg = results.front();        break;
      default: msg = "mode found but mixed"; break;
      }
    boost::trim(msg);    // remove locale-specific blank spaces, both left and right
    return msg;
  }

  std::string
  infoDomainModePure
  (const std::vector<int>& domainModes)      // for 'xeona::geometricProgression'
  {
    // static variables
    static DomainModeDatabase database;

    // process reduced vector
    std::vector<int> buffer = domainModes;   // copy over vector
    const int zero = 0;                      // value to remove
    buffer.erase(std::remove(buffer.begin(),
                                  buffer.end(),
                                  zero),
                      buffer.end());

    // interrogate database and process are required
    std::string msg = "(not overwritten)";
    switch ( buffer.size() )
      {
      case 0:  msg = "mode not found";                 break;
      case 1:  msg = database.getInfo(buffer.front()); break;
      default: msg = "mode found but mixed";           break;
      }
    boost::trim(msg);
    return msg;
  }

} // namespace 'xeona'

//  end of file

