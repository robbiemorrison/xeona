//  XEONA ENERGY SYSTEMS MODELING ENVIRONMENT
//
//  file-create-name : glpkviz.singlefile.cc
//  file-create-date : Sat 07-Jun-2008 21:35 UTC
//  file-initiator   : Robbie Morrison <robbie@actrix.co.nz>
//
//  file-role        : test single file version / single file
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
//  $URL: file:///home/robbie/svn-root/xeona/futz/trunk/xeona1/d/glpkviz.singlefile.cc $
//
//  GENERAL NOTES FOR THIS FILE
//
//  This file is not an essential part of 'xeona', but can be
//  used to test 'GlpkViz' in a stand-alone context.
//
//  Typical build call
//
//      $ g++ -Wall -DXE_GLPKVIZ_ALONE=1 -o glpkviz.single glpkviz.singlefile.cc -lglpk
//
//  Requirements
//
//      glpkviz.h       'GlpkViz' header
//      glpkviz.cc      'GlpkViz' implementation
//      <this file>
//      GLPK library, either static (compile-time linkage) or dynamic (run-time linkage)
//
//  See also (which supports logging and uses the application makefile)
//
//      glpkviz.alone.sh
//      glpkviz.alone.cc
//
//  Vectors instead of arrays
//
//     note the use of STL vectors instead of C-style arrays for
//     building GLPK sparse matrix vectors

//  LOCAL AND SYSTEM INCLUDES

#include "glpkviz.cc"    // GLPK problem instance visualizer using HTML / implementation

#include <string>        // C++ strings
#include <vector>        // STL sequence container

//  CODE

// ---------------------------------------------------------
//  MAIN FUNCTION   : main
// ---------------------------------------------------------

int
main(int argc, char* argv[])
{
  GlpkViz firefox("firefox");                // with browser invocation string

  printf("GLPK %s\n\n", glp_version());      // added run-time version call

  glp_prob *lp;

  std::vector<int>    ia(1, 0);              // was: int ia[1+1000]
  std::vector<int>    ja(1, 0);              // was: int ja[1+1000]
  std::vector<double> ar(1, 1.0);            // was: double ar[1+1000]

  double Z;
  double x1;
  double x2;
  double x3;

  lp = glp_create_prob();

  firefox(lp, "completely empty problem");   // view empty problem

  glp_set_prob_name(lp, "sample");
  glp_set_obj_dir(lp, GLP_MAX);
  glp_add_rows(lp, 3);
  glp_set_row_name(lp, 1, "p");
  glp_set_row_bnds(lp, 1, GLP_UP, 0.0, 100.0);
  glp_set_row_name(lp, 2, "q");
  glp_set_row_bnds(lp, 2, GLP_UP, 0.0, 600.0);
  glp_set_row_name(lp, 3, "r");
  glp_set_row_bnds(lp, 3, GLP_UP, 0.0, 300.0);
  glp_add_cols(lp, 3);
  glp_set_col_name(lp, 1, "x1");
  glp_set_col_bnds(lp, 1, GLP_LO, 0.0, 0.0);
  glp_set_obj_coef(lp, 1, 10.0);
  glp_set_col_name(lp, 2, "x2");
  glp_set_col_bnds(lp, 2, GLP_LO, 0.0, 0.0);
  glp_set_obj_coef(lp, 2, 6.0);
  glp_set_col_name(lp, 3, "x3");
  glp_set_col_bnds(lp, 3, GLP_LO, 0.0, 0.0);
  glp_set_obj_coef(lp, 3, 4.0);
  ia.push_back(1), ja.push_back(1), ar.push_back( 1.0);     // a[1,1] =  1
  ia.push_back(1), ja.push_back(2), ar.push_back( 1.0);     // a[1,2] =  1
  ia.push_back(1), ja.push_back(3), ar.push_back( 1.0);     // a[1,3] =  1
  ia.push_back(2), ja.push_back(1), ar.push_back(10.0);     // a[2,1] = 10
  ia.push_back(3), ja.push_back(1), ar.push_back( 2.0);     // a[3,1] =  2
  ia.push_back(2), ja.push_back(2), ar.push_back( 4.0);     // a[2,2] =  4
  ia.push_back(3), ja.push_back(2), ar.push_back( 2.0);     // a[3,2] =  2
  ia.push_back(2), ja.push_back(3), ar.push_back( 5.0);     // a[2,3] =  5
  ia.push_back(3), ja.push_back(3), ar.push_back( 6.0);     // a[3,3] =  6

  const int size = ar.size() - 1;
  glp_load_matrix(lp, size, &ia[0], &ja[0], &ar[0]);

  glp_simplex(lp, NULL);
  glp_intopt(lp, NULL);

  Z = glp_get_obj_val(lp);
  x1 = glp_get_col_prim(lp, 1);
  x2 = glp_get_col_prim(lp, 2);
  x3 = glp_get_col_prim(lp, 3);
  printf("\nZ = %g; x1 = %g; x2 = %g; x3 = %g\n", Z, x1, x2, x3);

  firefox(lp, "problem as per manual");      // view solver-submitted problem

  // additional trials

  glp_set_col_kind(lp, 2, GLP_IV);
  glp_simplex(lp, NULL);
  glp_intopt(lp, NULL);
  firefox(lp, "col 2 now integer-valued");   // view solver-submitted problem

  glp_set_col_bnds(lp, 3, GLP_DB, -20.0, 20.0);
  glp_simplex(lp, NULL);
  glp_intopt(lp, NULL);
  firefox(lp, "col 3 now double bounded");   // view solver-submitted problem

  // housekeeping

  glp_delete_prob(lp);                       // delete problem instance
  glp_free_env();                            // added to prevent 140 byte memory leak

  return 0;
}

//  end of file

