//  XEONA ENERGY SYSTEMS MODELING ENVIRONMENT
//
//  file-create-name : buildinfo.h
//  file-create-date : Tue 12-Apr-2011 10:24 UTC
//  file-initiator   : Robbie Morrison <robbie@actrix.co.nz>
//
//  file-role        : building asset to simulation data transfer / header
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
//  $URL: file:///home/robbie/svn-root/xeona/futz/trunk/xeona1/c/buildinfo.h $
//
//  GENERAL NOTES FOR THIS FILE

//  HEADER GUARD

#ifndef _BUILDINFO_H_
#define _BUILDINFO_H_

//  LOCAL AND SYSTEM INCLUDES

#include "../h/thrmperf.h"    // building thermal performance and HVAC model

#include "../a/logger_fwd.h"  // some forward declarations from "logger.h"
#include "../c/smart_ptr.h"   // toggle between Boost and TR1 smart pointers

#include <string>             // C++ strings
#include <sstream>            // string-streams

//  CODE

// ---------------------------------------------------------
//  CLASS           : BuildingFabricData
// ---------------------------------------------------------
//  Description  : standalone class to transfer building fabric data
//  Role         : transfer data from client entities to 'BuildingSim' objects
//  Techniques   : private nested classes with public members, friendship
//  Status       : complete
//
//  Design notes
//
//      Used to transfer data from entities like
//      'TeasBuildingElec' to the 'BuildingSim' objects.
//
// ---------------------------------------------------------

class BuildingFabricData
{
  // NESTED CLASSES

private:

  class GlassElementProps
  {
  public:

    GlassElementProps();

  public:

    double    transDirect;                   // solar direct transmission factor [-]
    double    transDifuse;                   // solar diffuse transmission factor [-]
    double    absorptance;                   // solar absorption factor [-]
    double    transGeneral;                  // solar transmission factor [-]
    double    surfaceHeatTferCoeffOutside;   // glazing heat transfer coefficient, outside
    double    surfaceHeatTferCoeffInside;    // glazing heat transfer coefficient, inside

  };

  class OpaqueElementProps
  {
  public:

    OpaqueElementProps
    (const BuildingSim::elementKind a_kind,
     const std::string&             a_note);

  public:

    const std::string                 note;       // note
    const BuildingSim::elementKind    kind;       // in { solid glass }
    double                            uvalue;     // area-averaged U-value [W/m2K]
    double                            yvalue;     // area-averaged Y-value [W/m2K]
    double                            decf;       // decrement factor [-]
    int                               lag;        // time lag [h]

  };

  // DISABLED

private:

  BuildingFabricData();                      // zero-argument constructor

  // CREATORS

public:

  BuildingFabricData
  (const std::string& annotation);

  // ACCESSORS

  // note too that this class and its nested classes act as
  // structs, allowing direct access to the underlying data

  bool
  isComplete() const;

  std::string
  getAnnotation() const;

  void
  report
  (std::ostream& os,
   const int     indent = 2) const;          // left margin

  std::string
  say                                        // used for reporting
  (BuildingSim::elementKind kind) const;

  // MANIPULATORS

  void
  setComplete();

  // INTERNAL DATA

private:

  const std::string     d_annotation;
  bool                  d_complete;

public:

  GlassElementProps     glazing;
  OpaqueElementProps    walls;
  OpaqueElementProps    roof;
  OpaqueElementProps    windows;

};

#endif // _BUILDINFO_H_

//  end of file

