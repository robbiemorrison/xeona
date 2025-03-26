//  XEONA ENERGY SYSTEMS MODELING ENVIRONMENT
//
//  file-create-name : glpkviz.alone.cc
//  file-create-date : Wed 11-Jun-2008 07:50 UTC
//  file-initiator   : Robbie Morrison <robbie@actrix.co.nz>
//
//  file-role        : auxillary testing of 'GlpkViz' / stand-alone
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
//  $URL: file:///home/robbie/svn-root/xeona/futz/trunk/xeona1/d/glpkviz.alone.cc $
//
//  GENERAL NOTES FOR THIS FILE
//
//  This file is not an essential part of 'xeona', but can be
//  used to test 'GlpkViz' in a stand-alone context.
//
//  Invoke this unit test via: 'glpkviz.alone.sh'.
//
//  This particular bash script, in turn, invokes the generic
//  'makefile' with the correct settings.

//  LOCAL AND SYSTEM INCLUDES

#include "glpkviz.h"          // unit under test (place early)
#include "../a/logger.h"      // standard logging functionality (as required)
#include ".././common.h"      // common definitions for project (place last)

#include <string>             // C++ strings
#include <sstream>            // string-streams

//  CODE

// ---------------------------------------------------------
//  MAIN FUNCTION   : main
// ---------------------------------------------------------

int
main(int argc, char* argv[])
{
  logga::spLogger logger = logga::ptrLogStream();  // main function logger
  logger->repx(logga::info, "beginning of main", "~~~~~~~~~~");
  logger->addSmartBlank();
  logger->setReportLevel(logga::xtra);       // maximum reporting

  // PRELIMINARY

  std::ostringstream put;

  // ---------------------------------------------------------
  //  test ONE        : simple instantiation
  // ---------------------------------------------------------

  logger->test(1);

  {
    put << "simple instantiation on the stack" << "\n";
    logger->putx(logga::dbug, put);

    GlpkViz shortlived("firefox");           // browser call bound to functor instance
  }

  // ---------------------------------------------------------
  //  test TWO        : tiny problem
  // ---------------------------------------------------------

  logger->test(2);

  {
    put << "tiny problem" << "\n" << "\n";
    logger->putx(logga::dbug, put);

    // GlpkViz stack object
    GlpkViz firefox("firefox");              // browser call bound to functor instance

    // tiny problem
    glp_prob* rawprob = glp_create_prob();
    firefox(rawprob, "problem is entirely empty in every respect");

    glp_set_prob_name(rawprob, "tiny");
    glp_set_obj_dir(rawprob, GLP_MAX);
    glp_add_rows(rawprob, 1);
    glp_set_row_name(rawprob, 1, "p");
    glp_set_row_bnds(rawprob, 1, GLP_UP, 0.0, 100.0);
    glp_add_cols(rawprob, 1);
    glp_set_col_name(rawprob, 1, "x");
    glp_set_col_bnds(rawprob, 1, GLP_LO, 0.0, 0.0);
    glp_set_obj_coef(rawprob, 1, 10.0);
    int ia[2], ja[2];
    double ar[2];
    ia[1] = 1, ja[1] = 1, ar[1] =  1.0;        // a[1,1] =  1
    glp_load_matrix(rawprob, 1, ia, ja, ar);
    glp_simplex(rawprob, NULL);
    glp_intopt(rawprob, NULL);
    firefox(rawprob, "finally a sensible problem");

    glp_delete_prob(rawprob);

    // free call for tiny problem
    glp_free_env();
  }

  // HOUSEKEEPING

  logger->test(0);                           // test end

  logger->repx(logga::info, "end of main", "~~~~~~~~~~");
  return xeona::exit_success;

} // main function

//  end of file

