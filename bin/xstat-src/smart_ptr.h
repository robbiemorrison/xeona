//  XEONA ENERGY SYSTEMS MODELING ENVIRONMENT
//
//  file-create-name : smart_ptr.h
//  file-create-date : Tue 15-Nov-2011 12:44 UTC
//  file-initiator   : Robbie Morrison <robbie@actrix.co.nz>
//
//  file-role        : switch easily between TR1 and Boost smart pointers / header
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
//  Copyright : This software is copyright (c) 2007 - 2011 Robbie Morrison.
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
//  $URL: file:///home/robbie/svn-root/xeona/futz/trunk/bin/xstat-src/smart_ptr.h $
//
//  GENERAL NOTES FOR THIS FILE
//
//  Header-only file.

//  HEADER GUARD

#ifndef _SMART_PTR_H_
#define _SMART_PTR_H_

// ---------------------------------------------------------
//  SETUP           : smart pointer library
// ---------------------------------------------------------
//
//  Design notes
//
//      This file allows alternative shared pointer libraries
//      and/or headers to be switched in and out without
//      significant code changes.
//
//      The current setting (at the time of writing) uses TR1
//      shared pointers in the std::tr1:: namespace as included
//      by way of the <tr1/memory> header.
//
//      TR1 stands for Technical Report 1 and indicates libraries
//      that are very likely to be included in the next round of
//      C++ ISO standards process.
//
//      The shared pointer implementation can also be set to
//      Boost shared pointers (one reason might be that the
//      Boost.Serialization 1.34.1 (de)serialization library does
//      not support TR1 shared pointers).
//
//      The <tr1/memory> header may well migrate to <memory> in
//      the not too distant future.  If so, one can speculate
//      that a move to the std:: namespace would also occur.
//
//  Header usage
//
//      Use this file as follows:
//
//      #include "smart_ptr.h"  // toggle between TR1 and Boost smart pointers
//      #include "common.h"     // common definitions for project (place last)
//
//  Compile-time overwriting of the _XEONA_SHARED_PTR macro
//
//      The _XEONA_SHARED_PTR macro can be explicitly set and
//      passed to the preprocessor at compile time -- otherwise
//      the default defined in this file will be utilized.
//
//  CAUTION: namespace qualification should not be used
//
//      Do not namespace qualify shared_ptr in the codebase --
//      UNLESS the flexibility given here is expressly not
//      wanted.  Hence the preferred syntax is:
//
//          shared_ptr<T> spT0;
//
//  CAUTION: TR1 smart pointers and Boost.Serialization cannot be
//  mixed
//
//      See r920 of this file for further information.
//
//  References
//
//      Consult the GNU GCC compiler documentation for details of
//      the move from <tr1/memory> to <memory>.
//
//      The 'Boost - Users' email list is a good source of
//      information regarding the current status of Boost
//      libraries.
//
// ---------------------------------------------------------

//  set the smart pointer libraries to be used
//
//  0 = TR1 std::tr1::shared_ptr and similar (conflicts with Boost.Serialization)
//  1 = Boost.Smart_ptr boost::shared_ptr and similar

#if !defined(_XEONA_SHARED_PTR)
# define _XEONA_SHARED_PTR 0                      // default to TR1 version
#endif

#if   (_XEONA_SHARED_PTR == 0)
# include <tr1/memory>                            // TR1 shared pointer
  using std::tr1::shared_ptr;
  using std::tr1::weak_ptr;
  using std::tr1::enable_shared_from_this;
  using std::tr1::static_pointer_cast;
  using std::tr1::bad_weak_ptr;                   // exception class
#elif (_XEONA_SHARED_PTR == 1)
# include <boost/serialization/shared_ptr.hpp>    // shared_ptr serialization
# include <boost/shared_ptr.hpp>                  // Boost shared pointer (serializable)
# include <boost/weak_ptr.hpp>                    // Boost weak pointer (serializable)
# include <boost/enable_shared_from_this.hpp>     // Boost smart pointer support
  using boost::shared_ptr;
  using boost::weak_ptr;
  using boost::enable_shared_from_this;
  using boost::static_pointer_cast;
  using boost::bad_weak_ptr;
#else
# error "_XEONA_SHARED_PTR macro not set to a supported value (0,1)"
#endif

#endif // _SMART_PTR_H_

//  end of file

