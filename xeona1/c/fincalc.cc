//  XEONA ENERGY SYSTEMS MODELING ENVIRONMENT
//
//  file-create-name : fincalc.cc
//  file-create-date : Fri 17-Oct-2008 14:59 UTC
//  file-initiator   : Robbie Morrison <robbie@actrix.co.nz>
//
//  file-role        : support for discounted cash flow analysis / implementation
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
//  $URL: file:///home/robbie/svn-root/xeona/futz/trunk/xeona1/c/fincalc.cc $
//
//  GENERAL NOTES FOR THIS FILE
//
//  Note
//
//      In particular, see the test comparisons at the end.
//
//  Source of financial equations
//
//      wikipedia page title: Time value of money
//      retrieved from: http://en.wikipedia.org/wiki/Time_value_of_money
//      page last modified : 26 March 2008 at 19:36
//
//  See also
//
//      xeona design notes on costs and prices
//      currently file: xeona-commentary_11.odt

//  LOCAL AND SYSTEM INCLUDES

#include "fincalc.h"          // companion header for this file (place first)

#ifndef _XUTEST
#include "../b/entity.h"      // entity base class plus lazy linking
#endif // XE_XUTEST

#include "../c/smart_ptr.h"   // toggle between Boost and TR1 smart pointers
#include "../a/logger.h"      // standard logging functionality (as required)
#include ".././common.h"      // common definitions for project (place last)

#include <cmath>              // C-style maths, ceil(), floor(), sqrt()
#include <limits>             // numeric_limits<T>::infinity() and similar
#include <sstream>            // string-streams
#include <string>             // C++ strings

#include <iostream>           // standard io
#include <iomanip>            // setw() and family

//  CODE

namespace xeona
{
  // ---------------------------------------------------------
  //  FREE FUNCTION   : xeona::presentValue
  // ---------------------------------------------------------

  //          FV
  //  PV = --------
  //              t
  //       (1 + i)
  //
  // The timebase depends on specification of i.

  double
  presentValue                // PV, year-zero present value
  (double futureValue,        // FV, projected future value
   int    timePeriod,         // t, annual with zero-based count
   double interestRate)       // i, interest rate per annum and suitably risk-adjusted
  {
    return futureValue / std::pow((1 + interestRate), timePeriod);
  }

  // ---------------------------------------------------------
  //  FREE FUNCTION   : xeona::annuityValue
  // ---------------------------------------------------------

  //           PV * i
  //  A = ---------------
  //               1
  //      1 -  --------
  //                   n
  //            (1 + i)
  //
  // The timebase depends on specification of i.

  double
  annuityValue                // A, annuity value
  (double   presentValue,     // PV, year-zero present value
   unsigned paymentsCount,    // n, number of annual payments
   double   interestRate)     // i, interest rate per annum and suitably risk-adjusted
  {
    static logga::spLogger logger = logga::ptrLogStream();

    // defensive programming
    if ( paymentsCount == 0 )
      {
        logger->repx(logga::warn, "invalid payments count, returning 0", paymentsCount);
        const double nan = std::numeric_limits<double>::quiet_NaN();
        return nan;
      }

    // active code
    if ( interestRate == 0 )  // special case of zero interest, otherwise a NaN results
      {
        return presentValue
          / paymentsCount;
      }
    else
      {
        // CAUTION: 'paymentsCount' is 'unsigned', which is unacceptable to 'std::pow'
        return presentValue * interestRate
          / (1 - ( 1 / std::pow((1 + interestRate), static_cast<int>(paymentsCount))));
      }
  }

  // ---------------------------------------------------------
  //  FREE FUNCTION   : xeona::capitalRecovery
  // ---------------------------------------------------------
  //  Description  : calculates the capital recovery
  //  Role         : used for accrued costs reporting
  //  Techniques   : uses 'presentValue' and 'annuityValue' functions
  //  Status       : complete
  //
  //  Design notes
  //
  //      The behavior of this call is identical to the spreadsheet
  //      function PMT(rate, periods, investment value, future
  //      value).
  //
  //      The (optional argument) current age will set the cost
  //      stream to zero if the current age is equal to or greater
  //      than the economic life.
  //
  //  Financial equations sourced from Wikipedia
  //
  //      wikipedia page title: Time value of money
  //      retrieved from: http://en.wikipedia.org/wiki/Time_value_of_money
  //      page last modified: 26-Mar-2008 at 19:36
  //      page still present: 31-Jul-2010
  //
  //  Comparative testing
  //
  //      OpenOffice.org Calc 2.0
  //      OpenOffice.org Calc 3.2
  //
  //      PMT : regular payments : returns the periodic payment of an annuity,
  //      based on regular payments and a fixed periodic interest rate
  //
  //      PMT(0.1;         # Rate          : the rate of interest per period
  //          3;           # NPER          : payment period
  //          100;         # PV            : present value
  //          -10)         # FV (optional) : future value
  //
  //      -37.19033233
  //
  //      This uses the default optional Type of 0 meaning
  //      payment occurs at the end of each period.
  //
  // ---------------------------------------------------------

  double
  capitalRecovery                  // relative to the length of the time horizon interval
  (double   discountRate,          // risk-adjusted interest per annum (decimal not %age)
   unsigned economicLife,          // economic life in years (not seconds)
   double   capitalInputInitial,   // capital input at beginning of economic life
   double   capitalInputTerminal,  // capital input at end of economic life (positive if a
   unsigned currentAge)            //                                          liability)
  {
    static logga::spLogger logger = logga::ptrLogStream();
    logger->repx(logga::adhc, "entering member function", "");

    const double nan       = std::numeric_limits<double>::quiet_NaN();
    double capitalRecovery = nan;            // initially set to nonsensical value

    if ( economicLife == 0 )                 // xeona convention for disabling the
      {                                      //    calculation, this test also prevents
        capitalRecovery = 0.0;               //   'annuityValue' from choking
      }
    else if ( currentAge >= economicLife )   // asset has been paid off
      {
        capitalRecovery = 0.0;
      }
    else                                     // normal calculation
      {
        // CAUTION: period counts are zero-based and hence the
        // economic life value was originally decremented when used
        // as an index.  However this lead to behavior different to
        // the spreadsheet function PMT and was removed.  In other
        // words, the terminal payment has no associated trailing
        // year or annuity payment.

        double PV
          = presentValue( capitalInputInitial,  0,            discountRate)
          + presentValue( capitalInputTerminal, economicLife, discountRate);

        double A = annuityValue(PV, economicLife, discountRate);

        capitalRecovery = A;
      }

#ifndef _XUTEST
    const int length                = Entity::getHorizonInterval();
#else                                        // to reduce dependencies
    const int length                = 1800;  // half hour
#endif // XE_XUTEST

    double capitalRecoveryPerSecond = capitalRecovery / (60 * 60 * 8760);
    double capitalRecoveryPerInt    = capitalRecoveryPerSecond * length;

    // report at the lowest priority
    // a 'width' of 14 aligns up to one thousand million at precision 3

#ifndef _XUTEST
    const int width     = 14;
    const int precision =  3;
#else
    const int width     = 13;
    const int precision =  8;                // for testing purposes
#endif // XE_XUTEST

      std::ostringstream put;
    put << std::fixed << std::setprecision(precision);
    put << "  capital recovery calculations"                          << "\n"
        << "    project discount rate [1/y]                   : "
        << std::setw(width) << std::right << discountRate             << "\n"
        << "    current age   [y]                             : "
        << std::setw(width) << std::right << currentAge               << "\n"
        << "    economic life [y]                             : "
        << std::setw(width) << std::right << economicLife             << "\n"
        << "    capital inflow initial  [$] (investment)      : "
        << std::setw(width) << std::right << capitalInputInitial      << "\n"
        << "    capital inflow terminal [$] (decommissioning) : "
        << std::setw(width) << std::right << capitalInputTerminal     << "\n"
        << "    interval length [s]                           : "
        << std::setw(width) << std::right << length                   << "\n"
        << "    capital recovery per annum [$/y]              : "
        << std::setw(width) << std::right << capitalRecovery          << "\n"
        << "    capital recovery per interval [$/interval]    : "
        << std::setw(width) << std::right << capitalRecoveryPerInt    << "\n";

    logger->repx(logga::adhc, "capital recovery values follow", "");
    logger->putx(logga::adhc, put);
    logger->addSmartBlank(logga::adhc);

    return capitalRecoveryPerInt;
  }

} // namespace xeona

//  end of file

