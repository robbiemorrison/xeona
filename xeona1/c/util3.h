//  XEONA ENERGY SYSTEMS MODELING ENVIRONMENT
//
//  file-create-name : util3.h
//  file-create-date : Thu 31-Dec-2009 00:17 UTC
//  file-initiator   : Robbie Morrison <robbie@actrix.co.nz>
//
//  file-role        : free functions for floating point comparison / header
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
//  $URL: file:///home/robbie/svn-root/xeona/futz/trunk/xeona1/c/util3.h $
//
//  GENERAL NOTES FOR THIS FILE
//
//  This unit provides enums and free functions to support:
//
//    precision specification : enumeration 'xeona::Precision'
//    predicates              : almost equal, near zero (one form)
//    active functions        : round to zero (two forms)
//
//  AD-HOC NOTES
//
//  The previous implementation was transferred to 'c/util3a' and
//  then deleted in commit r5472.  That code was based on Dawson
//  (2006) "Comparing floating point numbers".  To view the
//  relevant files, run: $ svn cat --revision 5468 c/util3.{h,cc}
//
//  HEADER GUARD

#ifndef _UTIL3_H_
#define _UTIL3_H_

//  LOCAL AND SYSTEM INCLUDES

#include "../a/logger_fwd.h"  // some forward declarations from "logger.h"
#include "../c/smart_ptr.h"   // toggle between Boost and TR1 smart pointers

//  CODE

// ---------------------------------------------------------
//  ENUM            : xeona::Precision
// ---------------------------------------------------------
//  Description  : enum to describe levels of "almost equal" precision
//  Role         : used in functions 'xeona::almostEqual' and 'xeona::nearZero'
//  Note         : the actual threshold values are hard-coded in 'xeona::almostEqual'
//  Status       : complete
// ---------------------------------------------------------

namespace xeona
{
  enum Precision
    {
      exact     =  1,                        // identical
      numic     =  2,                        // numerically equal
      tight     =  3,                        // tightly equal
      loose     =  4,                        // loosely equal
      zero0     =  5,                        // for close to zero comparisons
      zero9     =  6,                        // for close to zero comparisons
      zero8     =  7,                        // for close to zero comparisons
      zero7     =  8,                        // for close to zero comparisons
      zero6     =  9,                        // for close to zero comparisons
      zero5     = 10,                        // for close to zero comparisons
      zero4     = 11,                        // for close to zero comparisons
      zero3     = 12,                        // for close to zero comparisons
      zero2     = 13                         // for close to zero comparisons
    };

} // namespace 'xeona'

// ---------------------------------------------------------
//  FREE FUNCTION   : xeona::almostEqual <>
// ---------------------------------------------------------
//  Description  : compare two floating point numbers to a predefined relative error
//  Role         : general use
//  On fail      : returns 'false', never throws
//  Techniques   : 'Boost.MathToolkit' library, templates, tolerates 'inf's and 'NaN's
//  Status       : complete
//
//  Design notes
//
//      This function implements a relative equality test at four
//      predefined thresholds.  The particular level is given by
//      the selected 'xeona::Precision' enum as follows:
//
//      reference    : 3.333333333
//      xeona::exact : only 3.333333333 passes           identical
//      xeona::numic : 3.33333  passes  3.3333  fails    +/- 0.0001%
//      xeona::tight : 3.333    passes  3.33    fails    +/- 0.01%
//      xeona::loose : 3.3      passes  3.0     fails    +/- 10%
//
//      NOTE: the percentages are only accurate for given example
//      -- they can vary within a factor of two for other
//      comparisons.
//
//      The floating point identical requirement 'xeona::exact'
//      is provided for completeness -- however, a normal "=="
//      equality test will be much faster in failure mode.
//
//  Usage as a conditional
//
//      double A, B;
//      ..
//      if ( xeona::almostEqual(A, B, xeona::tight) )
//        {
//          // equal to within about 0.01%
//        }
//
//  See also
//
//      The comments and code in the implementation file.
//
// ---------------------------------------------------------

namespace xeona
{
  template <typename F>                      // 'F' in { float double }
  bool
  almostEqual
  (const F                a,
   const F                b,
   const xeona::Precision precision);
}

// ---------------------------------------------------------
//  FREE FUNCTION   : xeona::nearZero <>
// ---------------------------------------------------------
//  Description  : compare a floating point number to zero to a predefined relative error
//  Role         : general use
//  On fail      : returns 'false', never throws
//  Techniques   : 'Boost.MathToolkit' library, templates, tolerates 'inf's and 'NaN's
//  Status       : complete
//
//  Design notes
//
//      A wrapper function to 'xeona::almostEqual'.
//
// ---------------------------------------------------------

namespace xeona
{
  template <typename F>                      // 'F' in { float double }
  bool
  nearZero
  (const F                a,
   const xeona::Precision precision);
}

// ---------------------------------------------------------
//  FREE FUNCTION   : xeona::roundZero <> (ULP version)
// ---------------------------------------------------------
//  Description  : round a floating point number to exact zero as appropriate
//  Role         : general use
//  On fail      : returns 'false', never throws
//  Techniques   : 'Boost.MathToolkit' library, templates, tolerates 'inf's and 'NaN's
//  Status       : complete
//
//  Design notes
//
//      Makes use of function to 'xeona::almostEqual'.
//
// ---------------------------------------------------------

namespace xeona
{
  template <typename F>                      // 'F' in { float double }
  bool                                       // 'true' if rounded to exact zero
  roundZero
  (F&                     a,
   const xeona::Precision precision);
}

// ---------------------------------------------------------
//  FREE FUNCTION   : xeona::roundZero <> (trip version)
// ---------------------------------------------------------
//  Description  : round a floating point number to exact zero as appropriate
//  Role         : general use
//  On fail      : returns 'false', never throws
//  Techniques   : simple comparision
//  Status       : complete
//
//  Design notes
//
//      Makes use of simple comparison.
//
// ---------------------------------------------------------

namespace xeona
{
  template <typename F>                      // 'F' in { float double }
  bool                                       // 'true' if rounded to exact zero
  roundZero
  (F&      a,
   const F threshold);
}

#endif // _UTIL3_H_

//  end of file

