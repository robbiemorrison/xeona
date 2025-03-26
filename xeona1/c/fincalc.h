//  XEONA ENERGY SYSTEMS MODELING ENVIRONMENT
//
//  file-create-name : fincalc.h
//  file-create-date : Fri 17-Oct-2008 14:58 UTC
//  file-initiator   : Robbie Morrison <robbie@actrix.co.nz>
//
//  file-role        : support for discounted cash flow analysis / header
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
//  $URL: file:///home/robbie/svn-root/xeona/futz/trunk/xeona1/c/fincalc.h $
//
//  GENERAL NOTES FOR THIS FILE
//
//  The implementation file contains a full description of the
//  formulas used and similar matters.

//  HEADER GUARD

#ifndef _FINCALC_H_
#define _FINCALC_H_

//  LOCAL AND SYSTEM INCLUDES

// none required

//  CODE

namespace xeona
{

  // ---------------------------------------------------------
  //  FREE FUNCTION   : xeona::presentValue
  // ---------------------------------------------------------

  double
  presentValue                // PV, year-zero present value
  (double futureValue,        // FV, projected future value
   int    timePeriod,         // t, annual with zero-based count
   double interestRate);      // i, interest rate per annum and suitably risk-adjusted

  // ---------------------------------------------------------
  //  FREE FUNCTION   : xeona::annuityValue
  // ---------------------------------------------------------

  double
  annuityValue                // A, annuity value
  (double   presentValue,     // PV, year-zero present value
   unsigned paymentsCount,    // n, number of annual payments
   double   interestRate);    // i, interest rate per annum and suitably risk-adjusted

  // ---------------------------------------------------------
  //  FREE FUNCTION   : xeona::capitalRecovery
  // ---------------------------------------------------------
  //  Description  : calculates the capital recovery
  //  Role         : used for accrued costs reporting
  //  Techniques   : uses 'presentValue' and 'annuityValue' functions
  //  Status       : complete
  // ---------------------------------------------------------

  double
  capitalRecovery                 // relative to the length of the time horizon interval
  (double   discountRate,         // risk-adjusted interest per annum (decimal not %age)
   unsigned economicLife,         // economic life in years (not seconds)
   double   capitalInputInitial,  // capital input at beginning of economic life
   double   capitalInputTerminal, // capital input at end of economic life (positive if a
   unsigned currentAge = 0);      //                                          liability)

} // namespace xeona

#endif // _FINCALC_H_

//  end of file

