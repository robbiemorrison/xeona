//  XEONA ENERGY SYSTEMS MODELING ENVIRONMENT
//
//  file-create-name : sasserts.h
//  file-create-date : Sat 21-May-2011 08:23 UTC
//  file-initiator   : Robbie Morrison <robbie@actrix.co.nz>
//
//  file-role        : compile-time asserts (static asserts) / header (only)
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
//  $URL: file:///home/robbie/svn-root/xeona/futz/trunk/xeona1/sasserts.h $
//
//  GENERAL NOTES FOR THIS FILE
//
//  This is a "header-only" file.

//  HEADER GUARD

#ifndef _SASSERTS_H_
#define _SASSERTS_H_

//  LOCAL AND SYSTEM INCLUDES

#include <limits>                  // numeric_limits<T>::infinity() and similar

#include <boost/static_assert.hpp> //compile-time assertions

//  COMPILE-TIME ASSERTIONS

//  This code uses the 'BOOST_STATIC_ASSERT' feature from the
//  'Boost.Static_Assert' library.  It would be useful to migrate
//  this code to the C++11 'static_assert' call when that
//  standard is published.  Alexandrescu (2001, p23-26) covers
//  STATIC_ASSERT techniques in detail.
//
//  The following compile-time assertion confirms that the 'int'
//  type is not less than 31 digits -- in other words, 4-bytes.
//  C++ only guarantees a minimum 2-bytes, although this storage
//  is not common these days.
//
//  Regarding integer types: http://en.wikipedia.org/wiki/Integer_%28computer_science%29
//
//  A typical error message would be (wrapped here):
//
//    ./sasserts.h:00: error: invalid application of 'sizeof' to incomplete type
//    'boost::STATIC_ASSERTION_FAILURE<false>'
//
//  REFERENCES
//
//      Alexandrescu, Andrei.  2001.  Modern C++ design : generic
//        programming and design patterns applied.  Addison-Wesley,
//        Boston, USA.  ISBN 0-201-70431-5.

namespace                                    // unnamed namespace
{
  BOOST_STATIC_ASSERT(std::numeric_limits<int>::digits >= 31);
}

#endif // _SASSERTS_H_

//  end of file

