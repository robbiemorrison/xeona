//  XEONA ENERGY SYSTEMS MODELING ENVIRONMENT
//
//  file-create-name : block.cc
//  file-create-date : Tue 26-Aug-2008 14:21 UTC
//  file-initiator   : Robbie Morrison <robbie@actrix.co.nz>
//
//  file-role        : abstract block entity / implementation
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
//  $URL: file:///home/robbie/svn-root/xeona/futz/trunk/xeona1/b/block.cc $

//  LOCAL AND SYSTEM INCLUDES

#include "block.h"            // companion header for this file (place first)

#include "../c/si3units.h"    // support for engineering format and SI prefixes

#include "../a/logger.h"      // standard logging functionality (as required)
#include "../c/smart_ptr.h"   // toggle between Boost and TR1 smart pointers
#include ".././common.h"      // common definitions for project (place last)

#include <algorithm>          // STL copying, searching, and sorting, max()
#include <string>             // C++ strings
#include <sstream>            // string-streams

#include <boost/format.hpp>   // printf style formatting

//  CODE

// ---------------------------------------------------------
//  CLASS           : Block
// ---------------------------------------------------------

// STATIC DEFINITIONS - with and without explicit initialization

const int Block::s_indent           =   4;
const int Block::s_identifierLength =  35;   // originally 32
const int Block::s_ruleLength       = 130;

// CREATORS

// ---------------------------------------------------------
//  MEMBER FUNCTION : Block
// ---------------------------------------------------------

Block::Block
(const std::string entityId,
 Record&           record) :
  FullEntity(entityId, record),
  d_dutyStats(),
  d_sizeStats()
{
  s_logger->repx(logga::xtra, "constructor call", getIdAndKind());
}

// ---------------------------------------------------------
//  MEMBER FUNCTION : ~Block
// ---------------------------------------------------------

Block::~Block()
{
  s_logger->repx(logga::adhc, "destructor call", getIdAndKind());
}

// ---------------------------------------------------------
//  MEMBER FUNCTION : getDutyStats
// ---------------------------------------------------------

const Statistics<double>&
Block::getDutyStats() const
{
  return d_dutyStats;
}

// ---------------------------------------------------------
//  MEMBER FUNCTION : getSizeStats
// ---------------------------------------------------------

const Statistics<double>&
Block::getSizeStats() const
{
  return d_sizeStats;
}

// ---------------------------------------------------------
//  MEMBER FUNCTION : formatStats
// ---------------------------------------------------------
//  Description  : line formats current 'duty' and 'size' statistics
//  Role         : end of simulation reporting from 'DomainController::concludeDomain'
//  Techniques   : 'Boost.Format'
//  Status       : complete
//
//  Design notes
//
//      at the time of writing, only called under report level 5
//      or higher or YEEK 14
//
//  Input limits (may need to confirm currency)
//
//      identifier : should be 's_identifierLength' chars or less
//      duty/size  : "%10.0f" meaning up from 10GW
//
// ---------------------------------------------------------

const std::string
Block::formatStats
(const std::string& trailingComment) const   // note default
{
  // process left indent
  const std::string leftTab(s_indent, ' ');

  // process trailing comment
  const int noncommentWidth = s_indent + 118;  // right side of size "count"
  std::string buffer;
  if ( ! trailingComment.empty() )
    {
      const int length = xeona::consoleWidth - noncommentWidth;
      // defensive programming
      if ( length < 0 )
        {
          std::ostringstream oss;
          oss << noncommentWidth << " : " << xeona::consoleWidth;
          s_logger->repx(logga::warn, "console too narrow, width : _XTCOLS", oss.str());
          const std::string msg
            = leftTab
            + "(console width problem, noncommentWidth : xeona::consoleWidth "
            + oss.str() + ")";
          return msg;
        }
      else                                   // '_XTCOLS' set low
        {
          buffer = "    " + trailingComment;
          buffer = buffer.substr(0, length); // trim
        }
    }

  // integrity checks on sub-block entity programming
  if ( d_dutyStats.count() == 0 || d_sizeStats.count() == 0 )
    {
      std::ostringstream put;
      put << "  identifier : "            << getIdAndKind()                     << "\n"
          << "  class                 : " << getTypeStr()                       << "\n"
          << "  problem               : duty and/or size statistics empty"      << "\n"
          << "  likely cause          : faulty sub-block programming"           << "\n"
          << "  duty statistics count : " << d_dutyStats.count()                << "\n"
          << "  size statistics count : " << d_sizeStats.count()                << "\n";
      s_logger->repx(logga::rankJumpy,
                     "duty and/or size stats empty",
                     getIdAndKind());
      s_logger->putx(logga::dbug, put);
    }
  else if ( d_dutyStats.count() != d_sizeStats.count() )
    {
      std::ostringstream put;
      put << "  identifier : "            << getIdAndKind()                     << "\n"
          << "  class                 : " << getTypeStr()                       << "\n"
          << "  problem               : duty and size statistics counts differ" << "\n"
          << "  likely cause          : faulty sub-block programming"           << "\n"
          << "  duty statistics count : " << d_dutyStats.count()                << "\n"
          << "  size statistics count : " << d_sizeStats.count()                << "\n";
      s_logger->repx(logga::rankJumpy,
                     "duty and size stats counts differ",
                     getIdAndKind());
      s_logger->putx(logga::dbug, put);
    }

  // from commit r4105, the code relies on
  // 'xeona::fmtEngineeringFix' in unit 'c/si3units', this
  // function adds a " ", "k", "M", and so on, while ignoring
  // inf's and nan's

  const double maxDuty = d_dutyStats.max();
  const double maxSize = d_sizeStats.max();

#if 0 // 0 = normal (9999.9 unchanged), 1 = more aggressive (value maps to 9.9999 k)
  const xeona::SiPrefix prefixDuty = xeona::getEngineeringPrefix(maxDuty);
  const xeona::SiPrefix prefixSize = xeona::getEngineeringPrefix(maxSize);
  const xeona::SiPrefix temp   = std::max(prefixDuty, prefixSize);
  const xeona::SiPrefix prefix = static_cast<xeona::SiPrefix>(temp - 3);
  const unsigned decimals = 2;               // number of decimal places
#else
  const xeona::SiPrefix prefixDuty = xeona::getEngineeringPrefix(maxDuty, 1);
  const xeona::SiPrefix prefixSize = xeona::getEngineeringPrefix(maxSize, 1);
  const xeona::SiPrefix prefix = std::max(prefixDuty, prefixSize);
  const unsigned decimals = 2;               // number of decimal places
#endif // 0

  // note on 'Boost.Format' formatting: '-' is left-align,
  // '10.0f' means ten width, zero decimal places, fixed format
  // (could also try "e") -- obsolete comment now

//const std::string f    = "   %10.0f";     // CAUTION: any "% " space is significant
  const std::string f     = " %12s";
  const std::string idfmt = boost::str(boost::format("%%-%ds") % s_identifierLength);
  const std::string stfmt = "   %5d";

  const std::string formstr = "%s" + idfmt + f + f + f + f + f + f + stfmt + "%s" + "\n";
  return boost::str(boost::format(formstr)
                    % leftTab
                    % getIdAndKind()
                    % xeona::fmtEngineeringFix(d_dutyStats.min() , prefix, decimals)
                    % xeona::fmtEngineeringFix(d_dutyStats.max() , prefix, decimals)
                    % xeona::fmtEngineeringFix(d_dutyStats.mean(), prefix, decimals)
                    % xeona::fmtEngineeringFix(d_sizeStats.min() , prefix, decimals)
                    % xeona::fmtEngineeringFix(d_sizeStats.max() , prefix, decimals)
                    % xeona::fmtEngineeringFix(d_sizeStats.mean(), prefix, decimals)
                    % d_dutyStats.count()
                    % buffer);
}

// ---------------------------------------------------------
//  MEMBER FUNCTION : formatStatsHeader (static)
// ---------------------------------------------------------

const std::string
Block::formatStatsHeader()
{
  const std::string leftTab(s_indent, ' ');
  const std::string rule(s_ruleLength, '-');
  const std::string formstr    = boost::str(boost::format("%%-%ds") % s_identifierLength);
  const std::string identifier = boost::str(boost::format(formstr) % "identifier");
  return leftTab
    + identifier
    + "      duty lo      duty hi      duty av"
    + "      size lo      size hi      size av"
    + "   count    role"
    + "\n"
    + leftTab
    + rule
    + "\n";
}

// ---------------------------------------------------------
//  MEMBER FUNCTION : formatStatsFooter (static)
// ---------------------------------------------------------

const std::string
Block::formatStatsFooter()
{
  const std::string leftTab(s_indent, ' ');
  return "\n"
    + leftTab
    + "SI prefixes: "
    + "E exa (18)  P peta (15)  T tera (12)  G giga (9)  M mega (6)  k kilo (3)  --  "
    + "m milli (-3)  u micro (-6)  n nano (-9)"
    +"\n"
    + leftTab
    + "notes: 'size' is the step-specific UPPER capacity"
    + ", 'inf' indicates no limit"
    + ", '0' means (close-to) zero"
    + ", 'count' is the data count"
    + "\n";
}

// ---------------------------------------------------------
//  MEMBER FUNCTION : reportDutyAndSize
// ---------------------------------------------------------

std::string
Block::reportDutyAndSize() const
{
  s_logger->repx(logga::xtra, "entering member function", "");

  std::ostringstream oss;
  oss << "  duty (typically production or sales)"                << "\n"
      << "    current : " << d_dutyStats.last()                  << "\n"
      << "    hi      : " << d_dutyStats.max()                   << "\n"
      << "    lo      : " << d_dutyStats.min()                   << "\n"
      << "  size (usually actual but may be nominal capacity)"   << "\n"
      << "    current : " << d_sizeStats.last()                  << "\n"
      << "    hi      : " << d_sizeStats.max()                   << "\n"
      << "    lo      : " << d_sizeStats.min()                   << "\n";
  return oss.str();

}

//  end of file

