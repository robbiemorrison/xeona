//  XEONA ENERGY SYSTEMS MODELING ENVIRONMENT
//
//  file-create-name : inbuilt.cc
//  file-create-date : Mon 14-Jan-2008 15:29 UTC
//  file-initiator   : Robbie Morrison <robbie@actrix.co.nz>
//
//  file-role        : compiled-in test file generation / implementation
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
//  $URL: file:///home/robbie/svn-root/xeona/futz/trunk/xeona1/c/inbuilt.cc $

//  LOCAL AND SYSTEM INCLUDES

#include "inbuilt.h"          // companion header for this file (place first)

#include "../c/xemgen.h"      // class to generate well-formatted XEM models
#include "../a/xemopt.h"      // skeleton xem model generator
#include "../a/logger.h"      // standard logging functionality (as required)
#include "../c/smart_ptr.h"   // toggle between Boost and TR1 smart pointers
#include ".././common.h"      // common definitions for project (place last)

#include <fstream>            // file-based io
#include <string>             // C++ strings
#include <sstream>            // string-streams

//  CODE

// ---------------------------------------------------------
//  FREE FUNCTION   : ::quot
// ---------------------------------------------------------

namespace
{
  std::string
  quot
  (const std::string& str)
  {
    return xeona::modelStringDelim + str + xeona::modelStringDelim;
  }
} // unnamed namespace

namespace xeona
{
  // ---------------------------------------------------------
  //  FREE FUNCTION   : xeona::createModelName
  // ---------------------------------------------------------

  std::string                                // often leaf but directory part will remain
  createModelName
  (const std::string& stub)                  // empty string uses inbuilt default
  {
    std::string buf(stub);
    if ( boost::ends_with(buf, xeona::modelExt) )
      {
        boost::erase_tail(buf, xeona::modelExt.length());
      }
    if ( buf.empty() )
      {
        buf = xeona::modelInbuiltDefault;    // as set in "common.cc"
      }
    buf += xeona::modelExt;
    return buf;
  }

  // ---------------------------------------------------------
  //  FREE FUNCTION   : xeona::dumpToFile
  // ---------------------------------------------------------

  bool                                       // 'false' means file open failed
  dumpToFile                                 // calls 'loadOstream' internally
  (const std::string& filename,              // will overwrite file, maybe with zero bytes
   const unsigned     steps)                 // horizon steps
  {
    static logga::spLogger logger = logga::ptrLogStream();

    std::ofstream ofile(filename.c_str());   // note defaults: text, trunc (overwrite)
    if ( ! ofile )
      {
        logger->repx(logga::warn, "failed to open file for writing", filename);
        return false;
      }
    logger->repx(logga::dbug, "opened file for writing", filename);

    xeona::loadOstream(ofile, steps);        // defined below
    return true;

  } // 'ofile' closes on block exit

  // ---------------------------------------------------------
  //  FREE FUNCTION   : xeona::loadOstream
  // ---------------------------------------------------------
  //
  // prior to r2765 this function used to contain:
  //
  //    * a preprocessor macro 'XE_SIMPLIFIED' to control model depth
  //    * a set of outstream to build the model
  //
  // ---------------------------------------------------------

  void
  loadOstream                                // helper, func, contains model in text form
  (std::ostream&  os,
   const unsigned steps)                     // horizon steps
  {
    static logga::spLogger logger = logga::ptrLogStream();

    // create and fill a 'Xem' object, see unit 'a/xemopt'
    Xem xem(os,                              // print to stream
            xeona::svnRev,                   // svn revison
            xeona::modelAngleIndent);        // alignment tab

    xem.head();
    xem.mand(6);                             // number of steps
    xem.rule("model");
    xem.more();
    xem.tail();
    xem.flush();

    logger->repx(logga::dbug, "text streamed to file", "");
  }

} // namespace xeona

//  end of file

