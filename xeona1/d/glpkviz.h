//  XEONA ENERGY SYSTEMS MODELING ENVIRONMENT
//
//  file-create-name : glpkviz.h
//  file-create-date : Thu 05-Jun-2008 14:02 UTC
//  file-initiator   : Robbie Morrison <robbie@actrix.co.nz>
//
//  file-role        : HTML visualization of GLPK problem instances / header
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
//  $URL: file:///home/robbie/svn-root/xeona/futz/trunk/xeona1/d/glpkviz.h $
//
//  GENERAL NOTES FOR THIS FILE
//
//  This 'GlpkViz' class is coded for two roles: for use within
//  'xeona' and as a stand-alone unit.
//
//    the 'xeona' variant:
//
//      * takes a 'SolverIf' object (raw or shared_ptr)
//      * requires 'XE_GLPKVIZ_ALONE' = 0
//      * unit tests with 'glpkviz.ut0.cc'
//      * builds with the bash script 'mach'
//
//    the stand-alone variant:
//
//      * takes a GLPK 'glp_prob' instance
//      * requires 'XE_GLPKVIZ_ALONE' = 1
//      * unit tests with 'glpkviz.alone.ut.cc'
//      * builds with the bash script 'glpkviz.alone.sh'
//      * does not require units 'common' or 'logger' EXCEPT
//        for unit testing
//
//  Both require the same generic 'xeona' 'makefile' to be
//  present.
//
//  BROWSER PERFORMANCE VERSUS PROBLEM SIZE
//
//  GLPK viz cannot display large problems.  It was written to
//  assist the debugging of small-scale test problems.
//
//  For instance, using a modest system as follows:
//
//    system   : Linux 2.6.17 / Ubuntu GNU/Linux 6.10 (edgy) x86 / released Oct-2006
//    hardware : 1.4GHz Intel Celeron M / 512MiB RAM / 40GB HDD / 1024x768 / new Aug-2004
//    browser  : firefox 2.0.0
//
//  A 500x400 problem takes firefox about 40 seconds to display
//  and consumes an extra 110MB of memory, that is, over and
//  above an empty browser.

//  HEADER GUARD

#ifndef _GLPKVIZ_H_
#define _GLPKVIZ_H_

//  INTERNAL MACRO SETTING

#if !defined(XE_GLPKVIZ_ALONE)               // gives possibilty of external override
# define XE_GLPKVIZ_ALONE 0                  // 1 = stand-alone code, 0 = xeona
#endif

// check the macro setting (and allow for additional statements)

#if   (XE_GLPKVIZ_ALONE == 0)
#elif (XE_GLPKVIZ_ALONE == 1)
# warning "XE_GLPKVIZ_ALONE macro set to stand-alone (this may be what you want)"
#else
# error "XE_GLPKVIZ_ALONE macro not set to a supported value {0,1}"
#endif

//  LOCAL AND SYSTEM INCLUDES

#if   (XE_GLPKVIZ_ALONE == 0)
#include "../a/logger_fwd.h"  // some forward declarations from "logger.h"
#include "../c/smart_ptr.h"   // toggle between Boost and TR1 smart pointers
#include ".././common.h"      // common definitions for project (place last)
#endif // XE_GLPKVIZ_ALONE

#include <iostream>           // standard io
#include <sstream>            // string-streams
#include <string>             // C++ strings

#include <glpk.h>             // GNU GLPK mixed integer linear (MILP) solver

//  GLPK VERSION CHECK

// the following check assumes that a change to major version 5
// may also break this unit
#if ( GLP_MAJOR_VERSION == 4 && GLP_MINOR_VERSION < 38 )
#warning "GlpkViz requires GLPK 4.37 or better (check <glpk.h>)"
#endif

namespace svif
{
  class SolverIf;                            // grants friendship
}

//  CODE

// ---------------------------------------------------------
//  CLASS           : GlpkViz
// ---------------------------------------------------------
//  Description  : visualize a GLPK problem instance using HTML
//  Role         : as required by the client code
//  Techniques   : GLPK APIs, hand crafted mark-up creation
//  Status       : working
//
//  Design notes
//
//      The following (quite modest) standards apply:
//
//          mark-up          : HTML 4.0 Transitional
//          styles           : CSS 1
//          non-ASCII chars  : HTML named entities, XML decimal entities
//          colors           : RRGGBB
//
//      Closing tags are not required for <tr> and <td>.
//
//      The generated mark-up has been run thru the HTML 'tidy'
//      utility (01-Sep-2005 version).  The utility warns about
//      (nothing serious, this is the 'infinity' glyph, which
//      renders fine in 'firefox' 2.0):
//
//          - unknown entity "&#8734"
//
//  Quick introduction -- stand-alone variant
//
//      int main()
//      {
//        // GlpkViz stack object
//        GlpkViz firefox("firefox");         // browser call required
//
//        // create empty GLPK problem instance
//        glp_prob* prob = glp_create_prob();
//        firefox(prob, "empty problem");     // comment passed to web page
//      }
//
// ---------------------------------------------------------

class GlpkViz
{
  // LOCAL ENUMERATIONS

private:

  enum Embellishment          // to help set the appropriate CSS class attribute
    {
      none    = 0,            // means do nothing

      // <td> classes
      narrow,                 // omit left and right padding
      label_c,                // for name strings from GLPK, centred
      label_l,                // for name strings from GLPK, left aligned
      kind_iv,                // integer-valued
      kind_bv,                // binary-valued
      zero,                   // zero structural coefficient
      meta,                   // row and col information
      meta_2,                 // ditto but spanning
      result,                 // solver values
      goal,                   // solver objective function value
      optimal,                // optimality reached
      extra,                  // extra reporting including reduced costs
      left,                   // left align
      right,                  // right align
      span_2,                 // span two cells, also adds a 'colspan' attribute
      error,                  // potential error or omission

      // <p> classes
      helptext,               // help text
      endtext                 // final text
    };

#if   (XE_GLPKVIZ_ALONE == 1)                // stand-alone role

  // DISABLED

private:

  GlpkViz();                                 // zero-argument constructor
  GlpkViz(const GlpkViz& orig);              // copy constructor
  GlpkViz& operator= (const GlpkViz& orig);  // copy assignment operator

  // CREATORS

public:

  GlpkViz
  (const std::string browserInvoke);           // can pass in browser options as well

  ~GlpkViz();

  // FUNCTION CALL OPERATOR

  void
  operator()
  (glp_prob*         prob,                   // GLPK problem instance
   const std::string comment         = "",
   const int         outputPrecision = 6);

#elif (XE_GLPKVIZ_ALONE == 0)                // 'xeona' role

  // DISABLED

private:

  // CAUTION: zero-argument constructor cannot be disabled
  GlpkViz(const GlpkViz& orig);              // copy constructor
  GlpkViz& operator= (const GlpkViz& orig);  // copy assignment operator

  // CREATORS

public:

  GlpkViz
  (const std::string browserInvoke = xeona::webbrowser);    // thus a zero-argument ctor

  ~GlpkViz();

  // FUNCTION CALL OPERATORS

  void
  operator()
  (const svif::SolverIf* si,                           // solver interface object
   const std::string     comment         = "",
   const int             outputPrecision = 6);

  void
  operator()
  (const shared_ptr<svif::SolverIf> si,                // solver interface object
   const std::string                comment         = "",
   const int                        outputPrecision = 6);

#endif // XE_GLPKVIZ_ALONE

  // UTILITY FUNCTIONS

private:

  // main calls taken approximately in the order shown

  void resetData();
  void prepareFilename();
  void prepareTitle();
  void createHtml();
  void createHelp();                         // extended form of the normal file
  void saveHtml(const std::string filename);
  void displayHtml(const std::string filename);

  // HTML helpers (the char* variants are to protect against char* NULL returns)

  void raw(const std::string line);                              // inserts raw text
  void rem(const std::string comment);                           // <!-- .. -->
  void title(const std::string info);                            // <title> .. </title>
  void title(const char* info);
  void h1(const std::string header);                             // <h1> .. </h1>
  void h1(const char* header);
  void rule();                                                   // <hr>
  void p(const Embellishment e = none);                          // <p></p>
  void p(const char* copy, const Embellishment e = none);
  void p(const std::string copy, const Embellishment e = none);
  void line();                                                   // <tr>
  void cell(const Embellishment e = none);                       // <td>
  void cell(const char* label, const Embellishment e = none);
  void li(const std::string item);                               // <li> .. </li>
  template <typename T> void cell (const T data, const Embellishment e = none);

  // "absent" GLPK glp functions

  double getConCoef(int row, int col);       // because no 'glp_get_mat_val'

  // utility functions

  std::string timestamp();                   // current time, duly formatted

  // INTERNAL DATA

private:

  std::string           d_browser;           // browser invocation string
  glp_prob*             d_prob;              // pointer to GLPK C struct
  std::ostringstream    d_html;              // HTML buffer before writing to file
  std::string           d_comment;           // optional from function call operator
  std::string           d_filename1;         // filename for normal version
  std::string           d_filename2;         // filename for extended help version
  std::string           d_title;             // for <h1> and also page <title>
  bool                  d_round;             // use LPX_K_ROUND near-to-zero rounding
  std::string           d_timestamp;         // single consistent timestamp for each html

  static std::string    s_helpExt;           // help file sub-extension
  static std::string    s_htmlExt;           // HTML file extension
  static int            s_callCount;         // track functor calls

#if   (XE_GLPKVIZ_ALONE == 0)                // 'xeona' role

  static logga::spLogger s_logger;           // shared_ptr to single logger object

#endif // XE_GLPKVIZ_ALONE

};

#endif // _GLPKVIZ_H_

//  end of file

