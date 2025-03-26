//  XEONA ENERGY SYSTEMS MODELING ENVIRONMENT
//
//  file-create-name : extunits.h
//  file-create-date : Wed 02-Dec-2009 11:24 UTC
//  file-initiator   : Robbie Morrison <robbie@actrix.co.nz>
//
//  file-role        : quantifying extensity enums and interpretation / header
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
//  $URL: file:///home/robbie/svn-root/xeona/futz/trunk/xeona1/c/extunits.h $

//  HEADER GUARD

#ifndef _EXTUNITS_H_
#define _EXTUNITS_H_

//  LOCAL AND SYSTEM INCLUDES

#include <string>             // C++ strings

//  CODE

namespace xeona
{

  // ---------------------------------------------------------
  //  ENUM            : xeona::ExtensityUnit
  // ---------------------------------------------------------

  // CAUTION: these enums are not proceeded by "e_" in the
  // interests of brevity

  enum ExtensityUnit
    {
      notSpecified   = 0,
      joule          = 1,
      kilogram       = 2,
      uoa            = 3,
      metreSq        = 4
    };

  // ---------------------------------------------------------
  //  FREE FUNCTION   : xeona::interpretExtensity
  // ---------------------------------------------------------

  std::string
  interpretExtensity
  (const xeona::ExtensityUnit extensity);

} // namespace 'xeona'

#endif // _EXTUNITS_H_

//  end of file

