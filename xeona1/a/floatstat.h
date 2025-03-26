//  XEONA ENERGY SYSTEMS MODELING ENVIRONMENT
//
//  file-create-name : floatstat.h
//  file-create-date : Mon 20-Jul-2009 10:48 UTC
//  file-initiator   : Robbie Morrison <robbie@actrix.co.nz>
//
//  file-role        : floating-point environment management / header
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
//  $URL: file:///home/robbie/svn-root/xeona/futz/trunk/xeona1/a/floatstat.h $
//
//  GENERAL NOTES FOR THIS FILE

//  HEADER GUARD

#ifndef _FLOATSTAT_H_
#define _FLOATSTAT_H_

//  LOCAL AND SYSTEM INCLUDES

#include "../a/logger_fwd.h"  // some forward declarations from "logger.h"
#include "../c/smart_ptr.h"   // toggle between Boost and TR1 smart pointers

#include <string>             // C++ strings

#include <fenv.h>             // C-style C99 and TR1 floating point environment

//  CODE

// ---------------------------------------------------------
//  CLASS           : xeona::SaveClearResetFeFlag
// ---------------------------------------------------------
//  Description  : save, clear, restore floating-point status flags
//  Role         : floating-point numerics
//  Techniques   : block scope, <fenv.h> functionality as per TR1 (also C99)
//  Status       : complete
//
//  Design notes
//
//      Requires header <fenv.h> (which may migrate to <cfenv>).
//
//      The design is based on code in Becker (2007 pp250-251) --
//      see also (p255) and (p575-576).  Becker is also the best
//      background reference on this subject.
//
//      The class was originally coded with a default constructor
//      argument of 'FE_ALL_EXCEPT', but this was removed because
//      it could lead to somewhat confusing client-side code.
//
//      Note that floating-point exceptions are completely
//      different from C++ exceptions.
//
//  Usage
//
//      Requires block scope, often, but not exclusively, a
//      function definition:
//
//          {
//            xeona::SaveClearResetFeFlag ouflow(FE_OVERFLOW | FE_UNDERFLOW);
//            // numerical code
//            if ( ouflow.tripped() )
//              // problem resolving code
//            else
//              // okay code
//          }                           // 'status' is destructed on block exit
//
//  CAUTION: 'valgrind' ignores floating-point status flags
//
//      The 'valgrind' memory checker seems to treat all
//      floating-point status flags as unset.  For instance, even
//      the most basic 'valgrind' call will behave as if no
//      floating-point status flags have been set.
//
//          $ ./program                         # floating-point exception tests honored
//          $ valgrind --tool=none ./program    # floating-point exception tests fail
//
//  References
//
//      Becker, Pete.  2007.  The C++ Standard Library extensions :
//        a tutorial and reference.  Addison-Wesley, Upper Saddle
//        River, New Jersey, USA.  ISBN 0-321-41299-0.
//
// ---------------------------------------------------------

namespace xeona
{
  class SaveClearResetFeFlag
  {
    // DISABLED

  private:

    SaveClearResetFeFlag();                                              // zero-arg ctor
    SaveClearResetFeFlag(const SaveClearResetFeFlag& orig);              // copy ctor
    SaveClearResetFeFlag& operator= (const SaveClearResetFeFlag& orig);  // copy assn oper

    // CREATORS

  public:

    SaveClearResetFeFlag
    (const int feFlag);                      // macros as defined in <fenv.h>, can be |'ed

    ~SaveClearResetFeFlag();

    // ACCESSORS

  public:

    bool                                     // 'true' if flag has been reset
    tripped() const;

    int                                      // return constructor argument
    getFlag() const;

    // INSTANCE DATA

  private:

    const int    d_feFlag;                   // refer <fenv.h>
    fexcept_t    d_feStat;                   // type representing floating-point status
  };

} // namespace 'xeona'

#endif // _FLOATSTAT_H_

//  end of file

