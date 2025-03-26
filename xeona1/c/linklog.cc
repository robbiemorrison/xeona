//  XEONA ENERGY SYSTEMS MODELING ENVIRONMENT
//
//  file-create-name : linklog.cc
//  file-create-date : Thu 30-Jul-2009 21:25 UTC
//  file-initiator   : Robbie Morrison <robbie@actrix.co.nz>
//
//  file-role        : utility class to record entity linking results / implementation
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
//  $URL: file:///home/robbie/svn-root/xeona/futz/trunk/xeona1/c/linklog.cc $
//
//  GENERAL NOTES FOR THIS FILE

//  LOCAL AND SYSTEM INCLUDES

#include "linklog.h"          // companion header for this file (place first)

#include "../b/entity.h"      // entity base class plus lazy linking

#include "../a/logger.h"      // standard logging functionality (as required)
#include "../c/smart_ptr.h"   // toggle between Boost and TR1 smart pointers
#include ".././common.h"      // common definitions for project (place last)

#include <string>             // C++ strings
#include <sstream>            // string-streams

#include <boost/format.hpp>   // printf style formatting
#include <boost/foreach.hpp>  // BOOST_FOREACH iteration macro

//  CODE

// ---------------------------------------------------------
//  MEMBER FUNCTION : LinkLogger
// ---------------------------------------------------------

LinkLogger::LinkLogger() :
  d_linkNulls(0),
  d_linkLog()                                // empty vector
{
  reset();
}

// ---------------------------------------------------------
//  MEMBER FUNCTION : ~LinkLogger
// ---------------------------------------------------------

LinkLogger::~LinkLogger()
{
}

// ---------------------------------------------------------
//  MEMBER FUNCTION : recover
// ---------------------------------------------------------

std::string
LinkLogger::recover
(const int tab) const
{
  if ( getLogCount() == 0 )                  // bail out if no entries
    {
      return "";
    }

  // CAUTION: tuple member function 'get<0>' works for Boost
  // library 1.35.0 and better, but not for g++ 4.1.2

  std::string buffer;                        // return buffer
  const std::string padding(tab, ' ');       // padding
  BOOST_FOREACH( entry_type entry, d_linkLog )
    {
      buffer += boost::str(boost::format("%s%-30s  %-20s  %-20s  %-15s  %s\n")
                           % padding
                           % entry.get<0>()
                           % entry.get<1>()
                           % entry.get<2>()
                           % entry.get<3>()
                           % entry.get<4>());
    }
  return buffer;
}

// ---------------------------------------------------------
//  MEMBER FUNCTION : getNullCount
// ---------------------------------------------------------

int
LinkLogger::getNullCount() const
{
  return d_linkNulls;
}

// ---------------------------------------------------------
//  MEMBER FUNCTION : getLogCount
// ---------------------------------------------------------

int
LinkLogger::getLogCount() const
{
  return d_linkLog.size() - 1;               // minus one for header
}

// ---------------------------------------------------------
//  MEMBER FUNCTION : reset
// ---------------------------------------------------------

void
LinkLogger::reset()
{
  d_linkLog.clear();

  // add header using the same formatting calls as entries
  d_linkLog.push_back(boost::make_tuple("identifier",
                                        "full",
                                        "link",
                                        "resource",
                                        "remap"));
}

// ---------------------------------------------------------
//  MEMBER FUNCTION : insert
// ---------------------------------------------------------

bool
LinkLogger::insert
(const xeona::assign_ptr<Entity>& link,
 const bool                       okay)
{
  // count failures
  if ( okay == false ) d_linkNulls++;

  // harvest information
  const std::string identifier    = link->getIdentifier();

#ifndef _XUTEST
  const shared_ptr<Entity> entity = Entity::retSharedPtr(identifier);
  const std::string asBuiltType   = entity->getTypeStr();
  std::ostringstream oss;
  oss << entity;                             // stream to recover address
  const std::string resource = oss.str();
#else
  const std::string asBuiltType   = "(not under unit test)";
  const std::string resource      = "(not under unit test)";
#endif // _XUTEST

  const std::string linkType      = xeona::demangle(typeid(*link).name());
  const std::string output        = okay ? "okay" : "FAIL";

  // insert data
  d_linkLog.push_back(boost::make_tuple(identifier,
                                        asBuiltType,
                                        linkType,
                                        resource,
                                        output));
  return okay;
}

//  end of file

