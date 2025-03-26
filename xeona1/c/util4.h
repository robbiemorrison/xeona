//  XEONA ENERGY SYSTEMS MODELING ENVIRONMENT
//
//  file-create-name : util4.h
//  file-create-date : Fri 05-Mar-2010 09:10 UTC
//  file-initiator   : Robbie Morrison <robbie@actrix.co.nz>
//
//  file-role        : free functions for trigonometry and maths / header
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
//  $URL: file:///home/robbie/svn-root/xeona/futz/trunk/xeona1/c/util4.h $
//
//  GENERAL NOTES FOR THIS FILE
//
//  This unit provides free functions to support maths calculations:
//
//    maths                   : binary logarithm
//    conversion              : degrees - radians
//    trigonometry            : normalize angles outside [0,2pi] and [-pi,+pi] radians

//  HEADER GUARD

#ifndef _UTIL4_H_
#define _UTIL4_H_

//  LOCAL AND SYSTEM INCLUDES

#include "../a/logger_fwd.h"  // some forward declarations from "logger.h"
#include "../c/smart_ptr.h"   // toggle between Boost and TR1 smart pointers

#include <string>             // C++ strings
#include <sstream>            // string-streams

//  CODE

namespace xeona
{
  // ---------------------------------------------------------
  //  FREE FUNCTION   : xeona::log2 (binary logarithm)
  // ---------------------------------------------------------

  double log2 (const double n);              // base 2 logarithm (ISO notation is 'lb')

  // ---------------------------------------------------------
  //  FREE FUNCTION   : xeona::degree2radian
  //  FREE FUNCTION   : xeona::radian2degree
  // ---------------------------------------------------------

  double degree2radian(const double degree);
  double radian2degree(const double radian);

  // ---------------------------------------------------------
  //  FREE FUNCTION   : xeona::normalizeZeroTwoPi
  //  FREE FUNCTION   : xeona::normalizePlusMinusPi
  //  FREE FUNCTION   : xeona::normalizeZero360
  //  FREE FUNCTION   : xeona::normalizePlusMinus180
  // ---------------------------------------------------------

  double normalizeZeroTwoPi(const double radian);
  double normalizePlusMinusPi(const double radian);

  double normalizeZero360(const double degree);
  double normalizePlusMinus180(const double degree);
}

#endif // _UTIL4_H_

//  end of file

