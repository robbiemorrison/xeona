//  XEONA ENERGY SYSTEMS MODELING ENVIRONMENT
//
//  file-create-name : xemopt.h
//  file-create-date : Wed 20-May-2009 16:32 UTC
//  file-initiator   : Robbie Morrison <robbie@actrix.co.nz>
//
//  file-role        : skeleton xem model generator / header
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
//  $URL: file:///home/robbie/svn-root/xeona/futz/trunk/xeona1/a/xemopt.h $
//
//  GENERAL NOTES FOR THIS FILE
//
//  For use with option '--xem'.

//  HEADER GUARD

#ifndef _XEMOPT_H_
#define _XEMOPT_H_

//  LOCAL AND SYSTEM INCLUDES

#include "../c/xemgen.h"      // class to generate well-formatted XEM models

#include "../a/logger_fwd.h"  // some forward declarations from "logger.h"
#include "../c/smart_ptr.h"   // toggle between Boost and TR1 smart pointers

#include <string>             // C++ strings
#include <sstream>            // string-streams

#include <ostream>            // output streams

//  CODE

// ---------------------------------------------------------
//  CLASS           : Xem
// ---------------------------------------------------------
//  Description  : generate a skeleton xem model
//  Role         : used by command-line options '--xem' and '--inbuilt'
//  Techniques   : contains (by composition) and uses a 'XemGenerator' object
//  Status       : complete
//
//  Design notes
//
//      See unit 'c/xemgen' for details of class 'XemGenerator',
//      from whence most of the entity details are set.
//
// ---------------------------------------------------------

class Xem
{
  // DISABLED

private:

  Xem();                                     // zero-argument constructor
  Xem(const Xem& orig);                      // copy constructor
  Xem& operator= (const Xem& orig);          // copy assignment operator

  // CREATORS

public:

  Xem
  (std::ostream& os,                         // can be either console or file ostream
   const int     svnRev,                     // usually 'xeona::svnRev'
   const int     tab = 45);                  // angle bracket alignment (usually 45 or 50)

  ~Xem();

  // MANIPULATORS

public:

  void
  head();                                    // initial material

  void
  mand                                       // mandatory entities
  (const int steps);                         // used by horizon entity

  void
  more                                       // non-mandatory entities
  (const std::string& tag = "a");            // see implementation documentation

  void
  blok
  (const std::string& annot,                 // rule annotation
   const std::string& remark);               // added as a comment

  void
  tail();                                    // final material

  void
  rule
  (const std::string& annotation);

  void
  blank();                                   // blank line

  void
  flush();                                   // flush underlying stream

  // INTERNAL DATA

private:

  std::ostream&             d_os;
  const int                 d_svnRev;
  XemGenerator              d_xemgen;

  static logga::spLogger    s_logger;        // shared_ptr to single logger object

};

#endif // _XEMOPT_H_

//  end of file

