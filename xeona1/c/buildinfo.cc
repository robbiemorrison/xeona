//  XEONA ENERGY SYSTEMS MODELING ENVIRONMENT
//
//  file-create-name : buildinfo.cc
//  file-create-date : Tue 12-Apr-2011 10:24 UTC
//  file-initiator   : Robbie Morrison <robbie@actrix.co.nz>
//
//  file-role        : building asset to simulation data transfer / implementation
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
//  $URL: file:///home/robbie/svn-root/xeona/futz/trunk/xeona1/c/buildinfo.cc $
//
//  GENERAL NOTES FOR THIS FILE

//  LOCAL AND SYSTEM INCLUDES

#include "buildinfo.h"        // companion header for this file (place first)

#include "../a/logger.h"      // standard logging functionality (as required)
#include "../c/smart_ptr.h"   // toggle between Boost and TR1 smart pointers
#include ".././common.h"      // common definitions for project (place last)

#include <iomanip>            // setw() and family
#include <ostream>            // output streams
#include <sstream>            // string-streams
#include <string>             // C++ strings

//  CODE

// ---------------------------------------------------------
//  CLASS           : BuildingFabricData
// ---------------------------------------------------------

BuildingFabricData::GlassElementProps::GlassElementProps() :
  transDirect(),
  transDifuse(),
  absorptance(),
  transGeneral(),
  surfaceHeatTferCoeffOutside(),
  surfaceHeatTferCoeffInside()
{
}

BuildingFabricData::OpaqueElementProps::OpaqueElementProps
(const BuildingSim::elementKind a_kind,
 const std::string&             a_note) :
  note(a_note),
  kind(a_kind),
  uvalue(),
  yvalue(),
  decf(),
  lag()
{
}

BuildingFabricData::BuildingFabricData
(const std::string& annotation) :
  d_annotation(annotation),
  d_complete(false),
  glazing(),
  walls  (BuildingSim::solid, "walls"),
  roof   (BuildingSim::solid, "roof"),
  windows(BuildingSim::glass, "windows")
{
}

bool
BuildingFabricData::isComplete() const
{
  return d_complete;
}

std::string
BuildingFabricData::getAnnotation() const
{
  return "\"" + d_annotation + "\"";         // set in double quotes
}

void
BuildingFabricData::report
(std::ostream& os,
 const int     indent) const                 // note the default argument
{
  const std::string pad(indent, ' ');
  os << std::boolalpha
     << std::fixed << std::setprecision(2)
     << pad << "building fabric data summary"                                   << "\n"
     << pad << "  complete   : " << d_complete                                  << "\n"
     << pad << "  annotation : " << d_annotation                                << "\n"
     << pad << "  heat transmission thru glazing"                               << "\n"
     << pad << "    glazing"                                                    << "\n"
     << pad << "      solar direct transmission factor                  [-] : "
     << glazing.transDirect                                                     << "\n"
     << pad << "      solar diffuse transmission factor                 [-] : "
     << glazing.transDifuse                                                     << "\n"
     << pad << "      solar absorption factor                           [-] : "
     << glazing.absorptance                                                     << "\n"
     << pad << "      solar transmission factor                         [-] : "
     << glazing.transGeneral                                                    << "\n"
     << pad << "      surface heat transfer coefficient / inside    [W/m2K] : "
     << glazing.surfaceHeatTferCoeffOutside                                     << "\n"
     << pad << "      surface heat transfer coefficient / outside   [W/m2K] : "
     << glazing.surfaceHeatTferCoeffInside                                      << "\n"
     << pad << "  heat gain thru opaque structure"                              << "\n"
     << pad << "    walls"                                                      << "\n"
     << pad << "      element kind                                          : "
     << say(walls.kind)                                                         << "\n"
     << pad << "      area-averaged U-value (24h response)          [W/m2K] : "
     << walls.uvalue                                                            << "\n"
     << pad << "      area-averaged Y-value (transient response)    [W/m2K] : "
     << walls.yvalue                                                            << "\n"
     << pad << "      decrement factor (thermal mass)                   [-] : "
     << walls.decf                                                              << "\n"
     << pad << "      lag (time delay)                                  [h] : "
     << walls.lag                                                               << "\n"
     << pad << "    roof"                                                       << "\n"
     << pad << "      element kind                                          : "
     << say(roof.kind)                                                          << "\n"
     << pad << "      area-averaged U-value (24h response)          [W/m2K] : "
     << roof.uvalue                                                             << "\n"
     << pad << "      area-averaged Y-value (transient response)    [W/m2K] : "
     << roof.yvalue                                                             << "\n"
     << pad << "      decrement factor (thermal mass)                   [-] : "
     << roof.decf                                                               << "\n"
     << pad << "      lag (time delay)                                  [h] : "
     << roof.lag                                                                << "\n"
     << pad << "    windows (opaque behavior)"                                  << "\n"
     << pad << "      element kind                                          : "
     << say(windows.kind)                                                       << "\n"
     << pad << "      area-averaged U-value (24h response)          [W/m2K] : "
     << windows.uvalue                                                          << "\n"
     << pad << "      area-averaged Y-value (transient response)    [W/m2K] : "
     << windows.yvalue                                                          << "\n"
     << pad << "      decrement factor (thermal mass)                   [-] : "
     << windows.decf                                                            << "\n"
     << pad << "      lag (time delay)                                  [h] : "
     << windows.lag                                                             << "\n";
  os << std::flush;
}

std::string
BuildingFabricData::say                      // used for reporting
(BuildingSim::elementKind kind) const
{
  switch ( kind )
    {
    case 0: return "not specified";
    case 1: return "solid";
    case 2: return "glass";
    default: return "";
    }
}

void
BuildingFabricData::setComplete()
{
  d_complete = true;
}

//  end of file

