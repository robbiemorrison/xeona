//  XEONA ENERGY SYSTEMS MODELING ENVIRONMENT
//
//  file-create-name : siglp.h
//  file-create-date : Tue 22-Apr-2008 14:37 UTC
//  file-initiator   : Robbie Morrison <robbie@actrix.co.nz>
//
//  file-role        : semi-intelligent interface to GLPK MILP solver / header
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
//  $URL: file:///home/robbie/svn-root/xeona/futz/trunk/xeona1/d/siglp.h $
//  HEADER GUARD

//  GENERAL NOTES FOR THIS FILE
//
//  This unit has evolved quite a bit and would benefit from refactoring.

#ifndef _SIGLP_H_
#define _SIGLP_H_

//  LOCAL AND SYSTEM INCLUDES

#include "../a/logger_fwd.h"  // some forward declarations from "logger.h"
#include "../d/glpkviz.h"     // GLPK problem instance visualizer using HTML

#include <iostream>           // standard io
#include <map>                // STL associative container
#include <string>             // C++ strings
#include <vector>             // STL sequence container

#include <glpk.h>             // GNU GLPK mixed integer linear (MILP) solver

//  GLPK VERSION CHECK

// * this check assumes that major version 5 will also break the unit
// * the macros used are defined in 'glpk.h'
#if ( GLP_MAJOR_VERSION == 4 && GLP_MINOR_VERSION < 42 )
#warning "GLPK solver interface requires GLPK 4.42 or better (check <glpk.h>)"
#endif

//  MUST-KNOW INFORMATION
//
//      * semi-intelligent interface to the GNU GLPK optimization library
//      * tested with GLPK version 4.47 (09-Sep-2011)
//      * should be backward compatible to GLPK version 4.42 (13-Jan-2010)
//      * development began with GLPK version 4.25 (19-Dec-2007)
//      * read file 'DOCS/CODE.txt' for more information on GLPK interoperation
//      * employs one-based row and col indexing
//      * uses only the GLPK APIs documented in the relevant manual
//      * set in C++ namespace 'svif'
//      * search on "user-modifiable" for useful hard-coded settings
//      * structural variables are probably bounded to be non-negative
//      * 'stdout' for class reporting and GLPK output, 'stderr' for logging
//      * output suspend and resume calls could be useful when debugging
//
//  OVERARCHING NOTES
//
//  Purpose
//
//      The class 'SolverIf' offers a semi-smart interface to the
//      GNU Linear Programming Kit (GLPK) linear and
//      mixed-integer programming library.
//
//      The problem building and solver call methods provide a
//      higher-level interface than do the equivalent GLPK APIs.
//
//      A GLPK problem instance can be built on-the-fly.  In
//      other words, it is not necessary to specify the problem
//      size (row and column counts and coefficients estimate) or
//      problem klass (LP or MIP) in advance.
//
//      A number of data integrity checks are performed during
//      building and prior to the solver calls.
//
//      The appropriate solver calls are duly made and
//      interpreted.
//
//      While this code was written as part of the 'xeona'
//      application, it has been designed to be used as a
//      stand-alone unit and with or without the logging support
//      from other 'xeona' classes.
//
//  Extensiblity
//
//      The interface is meant to be easily extended to include
//      new problem building calls.
//
//  GLPK solver library required
//
//      This unit was tested using a statically linked GLPK
//      library archive.  The static archive was built using the
//      '-disable-dl' configure option.
//
//          $ cd glpk-0.00
//          $./configure --disable-dl
//
//  New-style GLPK APIs adopted
//
//      This unit uses the GLPK APIs as documented in the current
//      reference manual.  These are a mix of old- and new-style
//      and can be recognizable thus:
//
//          'lpx' functions migrating to 'glp'
//          'LPX' constants (often macros) migrating to 'GLP'
//
//      The new APIs are being progressively introduced from GLPK
//      release 4.17 (26-May-2007) (to the best of my knowledge)
//      and the process is still not complete as of 4.28
//      (25-Mar-2008).
//
//  Boost libraries are not used in this unit
//
//      For reasons of portability, this implementation does NOT
//      use Boost (nor TR1) libraries.  In particular, this means
//      no 'boost::shared_ptr<>' smart pointers.
//
//      The 'siglp' unit test for this unit does, however, use
//      Boost smart pointers, but it should be a straightforward
//      matter to revert these to raw pointers.
//
//      More importantly, the logger unit defined in "logger.h"
//      and 'logger_fwd.h' employs several Boost libraries.  If
//      the required libraries are not available then a new
//      logger unit will need to be created with either
//      substituted functionality or just plain disabled.
//
//      Consult the world-wide-web for more information on Boost
//      libraries.
//
//  Removal of support for 'logger' and 'common'
//
//      If need be, the headers "logger.h", "logger_fwd.h", and
//      "common.h" can omitted and the resulting compiler
//      warnings used to cleanse the code.  Alternatively, search
//      and delete lines matching 'logga' and then check the
//      surrounding construction.
//
//      A fake logger suite was written early on in this project.
//      The files 'logger_fake.h' and 'logger_fake.cc' would make
//      a good starting point for the development of a disabled
//      logger.
//
//  Problem labeling
//
//      The following labeling prefixes are currently hard-coded
//      at the start of 'siglp.cc':
//
//          pro-    problem
//          obj-    objective function
//          con0-   constraint equation
//          var0-   strictly real-valued (continuous) structural variable
//          vaz0-   strictly integer-valued (integer) structural variable
//          vab0-   strictly 0-1-valued (binary) structural variable
//
//  Terminal output, redirection, and suppression
//
//      Note that standard output or 'stdout' is represented by
//      UNIX file descriptor 'STDOUT_FILENO' (set to 1) and
//      standard error or 'stderr' is represented by UNIX file
//      descriptor 'STDERR_FILENO' (set to 2).  And that, for C++
//      ouput streams, 'std::cout' maps to 'stdout' and
//      'std::cerr' and 'std::clog' both map to 'stderr'.
//      Moreover, 'std::clog' is the buffered form of 'std::cerr'
//      and 'std::cout is also buffered.  In addition, the
//      insertion of 'std::endl' and 'std::flush' both flush the
//      streams to which they apply.
//
//      GLPK output > 's_os' > cout > stdout
//
//      GLPK writes to 'stdout' by default, but this can be
//      changed by defining a custom hook function and then
//      calling 'glp_term_hook'.  This is done here, with hook
//      function named 'termHook'.  The new stream (a pointer in
//      fact) is named 's_os' and initially maps to 'std::cout'.
//
//      Class native reporting > 's_os' > cout > stdout
//
//      Class native reporting is sent to the same 's_os' as
//      GLPK output.
//
//      Logger output > 'd_os' > clog > stderr
//
//      The logger currently outputs to 'std::clog'.  This
//      behavior can be overridden by explicitly defining the
//      internal-use preprocessor macro 'XE_OUTPUT', set in
//      'logger.cc'.
//
//      'xeona' output > cout > stdout
//
//      For completeness, the initial 'xeona' splash screen
//      and final information screen are written to
//      'std::cout'.
//
//      Redirection
//
//      The 's_os' stream can be simply dumped using the
//      'outputSuspend' and 'outputResume' calls.  Or
//      redirected to a log file by modifying the relevant
//      code in the free function 'svif::refDumpStream' in
//      'siglp.cc'.
//
//      The call 'stdoutRedirect' can also be used, but it
//      effect is application-wide and irreversible (as
//      currently implemented).  This call simply and crudely
//      closes UNIX file descriptor 'STDOUT_FILENO'.
//
//      Motivation
//
//      The reason that effort has been put into controlling
//      both the level of reporting and the output direction
//      is that the amount of text produced can easily become
//      astronomical.
//
//  File organization
//
//      This file is organized roughly as follows:
//
//        * public enumerations
//        * constructor and destructor
//        * GLPK clean up (free GLPK environment)
//        * output controls
//        * solver preference setting
//        * problem building
//        * solver invocation
//        * solution recovery
//        * general information
//        * miscellaneous
//        * utility (private) functions
//        * instance internal data
//        * class-wide (static) internal data
//
//  Preferred terminology (because it varies so much)
//
//      objective function (not goal function)
//      constraint matrix (not structural matrix)
//      mixed-integer linear program (not mixed-integer program)
//      right-hand side bound
//      problem klass (intentionally 'k' to distinguish it from the C++ keyword)
//      GLPK problem instance (not GLPK problem object)
//
//  GLPK integer solvers
//
//          glp_intopt() : prior optimal LP relaxation required
//

//  ADDITIONAL NOTES (r2718) : GLPK now has new reporting controls as follows:
//
//  Solver level control
//
//      Three forms
//
//          smcp  = simplex solver control parameters
//          iptcp = interior-point solver control parameters
//          iocp  = integer optimization [branch and cut] control parameters
//
//      Data and functions
//
//          structs:
//
//              glp_smcp
//              glp_iptcp
//              glp_ipcp
//
//          reporting level data member:
//
//              all contain data member 'msg_lev', defaulting to 'GLP_MSG_ALL'
//
//          struct intitialization calls:
//
//              int  glp_init_smcp (&parm)
//              int  glp_init_iptcp(&parm)
//              void glp_init_iocp (&parm)
//
//          solver calls:
//
//              int glp_simplex (glp_prob*  lp, const glp_smcp*  parm)
//              int glp_exact   (glp_prob*  lp, const glp_smcp*  parm)
//              int glp_interior(glp_prob*   P, const glp_iptcp* parm)
//              int glp_intopt  (glp_prob* mip, const glp_iocp*  parm)
//
//          macro definitions for use with 'msg_lev:'
//
//              GLP_MSG_OFF : no output
//              GLP_MSG_ERR : error and warning messages only
//              GLP_MSG_ON  : normal output
//              GLP_MSG_ALL : full output (including informational messages)
//
//      Representative code:
//
//          glp_smcp parm;                // simplex solver control parameters
//          glp_init_smcp(&parm);         // initialize with defaults
//          parm.meth    = GLP_DUAL;
//          parm.msg_lev = GLP_MSG_ERR;   // modify setting
//          glp_simplex(P, &parm);
//
//  Environment-level control
//
//      API routines:
//
//          glp_term_out  : enable/disable terminal output
//          glp_term_hook : intercept terminal output
//
//  Note also (does not control reporting)
//
//      bfcp = basic factorization control parameters

//  CODE

namespace svif                               // CAUTION: note namespace
{
  // ---------------------------------------------------------
  //  CONSTANT        : DBUG
  // ---------------------------------------------------------

  extern const bool DBUG;                    // set via _XDEBUG macro in 'siglp.cc'

  // ---------------------------------------------------------
  //  ENUM            : ProblemStatus
  //  ENUM            : ObjectiveSense
  //  ENUM            : SolverType
  //  ENUM            : ProblemKlass
  //  ENUM            : ColumnKind
  //  ENUM            : ConstraintSense
  //  ENUM            : SolutionStatus
  // ---------------------------------------------------------
  //  Description  : supporting enums, often close to GLPK macros
  //  Role         : to improve code safety and readability
  //  Status       : complete
  //
  //  Design notes
  //
  //     These enumerations are mostly self-explanatory.  The
  //     'other' values are provided for testing purposes.  Some
  //     further clarification is given below:
  //
  //       ColumnKind
  //
  //           the 'ColumnKind' enum is no longer required after
  //           GLPK introduced 'GLP_BV', but is nonetheless
  //           retained for completeness
  //
  //       ObjectiveSense
  //
  //           although GLPK defaults to maximize, this class
  //           requires the objective sense to be set explicitly
  //
  //       soln_not_usable
  //
  //           means the current solution is infeasible, but has
  //           not been proven to be generally so (this latter
  //           condition is sometimes known as 'nonfeasible')
  //
  //       soln_proven_bad
  //
  //           means the current problem has been proven to have
  //           no feasible solution (that is, it is 'nonfeasible')
  //
  // ---------------------------------------------------------

  enum ProblemStatus               // meaning the GLPK problem instance status
    {
      status_not_specified = 0,
      status_incomplete    = 1,    // condition below does not hold
      status_empty         = 2,    // objective sense set but still empty
      status_solvable      = 3,    // objective sense and at least one row, col, and coeff
      status_submitted     = 4,    // submitted to solver
      status_other         = 9
    };

  enum ObjectiveSense              // termed "objective direction" in GLPK
    {
      sense_not_specified  = 0,
      minimize             = 1,    // GLP_MIN
      maximize             = 2     // GLP_MAX (maximize is the GLPK default)
    };

  enum SolverType
    {
      solver_not_specified = 0,
      solver_simplex       = 1,    // 'glp_simplex' simplex method
      solver_network       = 2,    //  CAUTION: network simplex planned for late-2008
      solver_interior      = 3,    // 'glp_interior' interior point method
      solver_integer       = 4,    // 'glp_intopt' integer method
      solver_other         = 9
    };

  enum ProblemKlass                // LPX_LP, LPX_MIP, 'lpx_get_class' no longer supported
    {
      prob_not_specified   = 0,
      prob_linear          = 1,    // GLP_CV
      prob_mixed_integer   = 2,    // GLP_CV + GLP_IV (+ GLP_BV optionally)
      prob_mixed_zero_one  = 3,    // GLP_CV           + GLP_BV
      prob_pure_integer    = 4,    //          GLP_IV (+ GLP_BV optionally)
      prob_pure_zero_one   = 5,    //                    GLP_BV
      prob_other           = 9
    };

  enum ColumnKind                  // these classifications are mutually exclusive
    {
      col_not_specified    = 0,
      col_continuous       = 1,    // GLP_CV, strictly continuous
      col_integer          = 2,    // GLP_IV, strictly integer
      col_binary           = 3,    // GLP_BV, strictly binary
      col_other            = 9
    };

  enum ConstraintSense             // same terminology as the IBM OSI project
    {
      con_not_specified    = 0,
      L                    = 1,    // <= constraint
      E                    = 2,    //  = constraint
      G                    = 3,    // >= constraint
      R                    = 4,    // ranged constraint
      N                    = 5     // free constraint
    };

  enum SolutionStatus
    {
      soln_undefined       = 0,
      soln_optimal         = 1,    // solution optimal
      soln_feasible        = 2,    // solution usable but not optimal
      soln_not_usable      = 3,    // solution not usable but problem not proven bad
      soln_proven_bad      = 4,    // problem proven bad
      soln_faulty          = 5,
      soln_other           = 9
    };

  // ---------------------------------------------------------
  //  ENUM            : ReportingLevel
  // ---------------------------------------------------------
  //  Description  : supporting enum to control reporting
  //  Status       : work in progress
  //
  //  Design notes
  //
  //      First be aware that logging verbosity is controlled via
  //      the 'Logger' class and is thus not managed by this
  //      class.  That said, much of the code here contains
  //      'logga::Logger::repx' calls.
  //
  //      GLPK native output control works on three levels:
  //
  //          * at the GLPK solver call level via the control
  //          parameter 'msg_lev' which defaults to 'GLP_MSG_ALL'
  //          -- as of 4.33, supported calls comprise
  //          'glp_simplex', 'glp_intopt', and 'glp_interior'
  //
  //          * at the GLPK problem instance level via
  //          'lpx_set_int_parm' as follows:
  //
  //              lpx_set_int_parm(d_prob, LPX_K_MSGLEV, value)
  //
  //          * at the GLPK library environment level, using the
  //          calls 'glp_term_out' and 'glp_term_hook' -- for
  //          example, 'glp_term_out(GLP_OFF)' completely blocks
  //          all normal terminal output (but NOT any intercepted
  //          and then redirected output) and applies globally
  //
  //      These last two levels are also mirrored in the
  //      'SolverIf' class as follows:
  //
  //          * the value of object variable 'd_noise' of type
  //          'ReportingLevel' determines how the GLPK instance
  //          is initiated -- 'd_noise' is optionally set in the
  //          constructor, otherwise it defaults to svif::low and
  //          svif::high depending on whether svif::DBUG is false
  //          or true
  //
  //          * output suspend is controlled via the static
  //          functions 'outputSuspend' and 'outputResume' and
  //          the current status is recorded by 's_noise'
  //
  //      Some class native reporting -- mostly in the form of
  //      information blocks -- also exists and controlled via
  //      'd_noise' and sometimes dependent as well on the
  //      solution status.
  //
  //      The GLPK settings (old and new style) are as follows:
  //
  //          LPX_K_MSGLEV = 0 or GLP_MSG_OFF - no output (although leakage may occur)
  //          LPX_K_MSGLEV = 1 or GLP_MSG_ERR - errors and warnings only
  //          LPX_K_MSGLEV = 2 or GLP_MSG_ON  - normal
  //          LPX_K_MSGLEV = 3 or GLP_MSG_ALL - normal plus information
  //
  //      The static function 'stdoutRedirect' simply redirects
  //      or disables 'stdout', depending on whether the call was
  //      passed a logfile name or not.  As currently
  //      implemented, this call is irreversible.  Deploy it with
  //      care (indeed, this function should probably not be
  //      offered).
  //
  //  Summary
  //
  //      The 'ReportingLevel' emum applies only to 'SolverIf'
  //      instances and controls both GLPK output and class
  //      native reporting.
  //
  //      The static calls 'outputSuspend/Resume()' disable and
  //      enable GLPK output and class native reporting on a
  //      class-wide basis.
  //
  //      Logging verbosity is controlled via the 'Logger' class
  //      and is not managed here.
  //
  //      'Stdout' across the entire application can be
  //      permanently redirected or closed by calling
  //      'stdoutRedirect'.
  //
  // ---------------------------------------------------------

  enum ReportingLevel
    {
      not_specified  = 0,
      silent         = 1,     // GLPK no output (although leakage may occur)
      low            = 2,     // GLPK errors and warning
      medium         = 3,     // GLPK normal             + problem info
      high           = 4      // GLPK normal plus info   + problem info plus GLPK report
    };

  enum CofSpan                // coefficient span testing thresholds
    {
      e_inactive     = 0,     // do nothing
      e_say_fail     = 1,     // test, print on fail
      e_say_each     = 2      // test, print all
    };

  // ---------------------------------------------------------
  //  FREE FUNCTION   : refDumpStream
  // ---------------------------------------------------------

  std::ofstream&
  refDumpStream();                           // used by 'outputSuspend' and 'outputResume'

  // ---------------------------------------------------------
  //  CLASS           : SolverIf
  // ---------------------------------------------------------
  //  Description  : semi-intelligent interface to GLPK solver
  //  Role         : offers higher-level calls when building and solving MIP problems
  //  Status       : complete (more or less)
  //
  //  Design notes
  //
  //      Row and col indexing is one-based, that is, the
  //      indexing starts from one (however, the objective
  //      function 'shift' is assigned index zero).
  //
  //      Significant additional notes are contained in the code.
  //
  //  CAUTION: friendship and included headers
  //
  //      Be aware that friendship introduces some tighter
  //      requirements on the order in which the compiler see
  //      things.
  //
  //  CAUTION: freeing the GLPK environment
  //
  //      Normally a call to 'freeGlpkEnvironment' is not needed
  //      because the final 'SolverIf' destructor call will do
  //      this.
  //
  //      If required, the static function 'freeGlpkEnvironment'
  //      should only be invoked near the end of the 'main'
  //      function because the call will dump any and all
  //      existing GLPK problem instances.
  //
  //  References (GLPK 4.25)
  //
  //      Makhorin, Andrew.  2007.  GNU linear programming kit :
  //        reference manual version 4.25 -- Draft edition
  //        (December 2007).  Free Software Foundation, Boston, MA,
  //        USA.  [author affiliation: Department for Applied
  //        Informatics, Moscow Aviation Institute, Moscow, Russia]
  //        (distributed with the GLPK source code as LaTeX, DVI,
  //        and PostScript)
  //
  // ---------------------------------------------------------

  class SolverIf
  {
    // ---------------------------------------------------------
    // TYPEDEFS

  public:

    typedef std::pair
    <int,                                    // row index
     int>                                    // col index
    ij_type;

  private:

    typedef std::pair
    <double,                                 // value, unmodified or absolute
     ij_type>                                // location
    cof_type;

    typedef std::multimap
    <double,                                 // value, unmodified or absolute
     ij_type>                                // location
    cofmap_type;

    // ---------------------------------------------------------
    // FRIENDS

  private:

    friend void GlpkViz::operator() (const SolverIf*, std::string, int);

    // ---------------------------------------------------------
    // DISABLED

  private:

    SolverIf();                                   // zero-argument constructor
    SolverIf(const SolverIf& orig);               // copy constructor
    SolverIf& operator= (const SolverIf& orig);   // copy assignment operator

    // ---------------------------------------------------------
    // CREATORS

  public:

    SolverIf
    (std::string    tag,                               // problem name tag, mandatory
     ReportingLevel noise = svif::not_specified);      // default chosen by _XDEBUG

    ~SolverIf();                             // no need to be virtual

    // ---------------------------------------------------------
    // GLPK CLEAN-UP : a class-wide call, CAUTION: no longer
    // needed in client code because it is called when the
    // 'SolverIf' destructor is invoked on the last instance --
    // but if employed, use only near the end of the 'main'
    // function because any existing GLPK problem instances will
    // be unceremoniously "invalidated"

    static
    bool                                     // 'false' if problems encountered
    freeGlpkEnvironment();                   // calls 'glp_free_env', warns if instances

    // ---------------------------------------------------------
    // REPORTING CONTROL : class-wide calls controlling GLPK
    // output and native reporting -- as opposed to xeona-style
    // logging
    //
    // these calls may be made before any 'SolverIf' objects have
    // be created

    // HEALTH WARNING: 'stdoutRedirect' is application-wide and permanent!

    static
    void
    stdoutRedirect                           // CAUTION: see above health warning
    (const std::string logname = "");        // default will simply close 'stdout'

    static
    void
    outputSuspend();                         // disable all GLPK and class native output

    static
    void
    outputResume();                          // re-enable all GLPK and class native output

    // ---------------------------------------------------------
    // SOLVER PREFERENCES
    //
    // CAUTION: first note that the concepts of "simplex
    // presolve" and "MIP initial LP solve" are quite different
    // and distinct -- the function 'initUseSimpPresolver'
    // declared below covers the former

    void
    initSetPrefLPSolver                      // advisory only
    (SolverType solverType);                 // preferred LP solver

    void
    initEmployScaling();                     // the GLPK default is not to scale

    void
    initEmployAdvBasis();                    // the GLPK default is not to construct this

    void
    initSimplexPresolver();                  // the GLPK default is not to presolve

    void
    initIntegerPresolver();                  // the GLPK default is not to presolve

    void
    initCoeffSpan                            // the default here is not to test
    (const int level);                       // in { 0 1 2 }

    // ---------------------------------------------------------
    // PROBLEM BUILDING : underlying model, calls, and rules
    //
    //  Users should read this comment block carefully.
    //
    //  This implementation uses dynamic containers, tracks the
    //  problem klass (LP or MIP) "on-the-fly", and
    //  automatically determines which calls and checks are to
    //  make at solve-time.  No up-front problem size and klass
    //  specifications and estimates are required.
    //
    //  In relation to the constraint matrix and associated
    //  objective and rhs vectors:
    //
    //      i is the one-based row count
    //      j is the one-based col count
    //
    //  But note also that the 'shift' objective coefficient
    //  resides at index 0.
    //
    //  These calls assume the problem is given in 'ordinary'
    //  rather than 'standard' form (see GLPK documentation
    //  chapter 1 "Introduction" and appendix C "CPLEX LP
    //  format").  Thus for structural variable x_j and
    //  constraint equation i (or simply row i and col j):
    //
    //      obj_j                            // objective coefficient j
    //      coeff_ij                         // constraint coefficient i,j
    //      <=|=|>=                          // constraint sense i
    //      rhs_i                            // rhs bound i
    //      x_j optionally in { int, bin }   // additional restriction
    //
    //  plus (as a 'SolverIf' default) a global condition that
    //  structural variables be non-negative (this requirement
    //  derives from 'xeona' and not GLPK)
    //
    //      x_j   >= 0                       // global non-negativity condition
    //
    //  and also note that, unless otherwise specified:
    //
    //      'value' is of fundamental type double and can be zero
    //          or strictly negative
    //
    //  The default col bounds are controlled by the internal
    //  macro 'XE_BNDS_DEFAULT'.  This macro is set in the
    //  implementation file.  If a single col requires finer
    //  bounds then two approaches exist.  Add a unitary entry
    //  constraint row for each requisite bound.  Or call
    //  'reviseBnds' directly.  Unlike the first method, a call
    //  to 'reviseBnds' allows the existing col bounds to be
    //  widened.  The choice of method depends somewhat on
    //  whether the "restriction" arises as a normal problem
    //  constraint or as something more fundamental.
    //
    //  The 'markVarBinary' call sets the col bounds to [0,1] and
    //  the other mark calls return the bounds to the default set
    //  by 'XE_BNDS_DEFAULT'.  Hence, col bounds are consistent
    //  and universal (aside from the toggling effect for binary
    //  variables).
    //
    //  A row, col, objective, or problem 'label' is formed from
    //  the appropriate fixed-character prefix (defined in the
    //  implementation file), any index data (or the timestamp,
    //  in the case of a problem label), and the optionally
    //  supplied 'tag' if given.
    //
    //  Label-setting is also used to enforce call order and to
    //  detect overwrite conditions.
    //
    //  A 'complete' problem must have at least one row, one col,
    //  and one constraint coefficient set, and have had an
    //  explicit call to 'setObjectiveSense' (in other words,
    //  unlike GLPK, there is no "maximize" default).  Otherwise
    //  the problem will be deemed 'incomplete' and a call to
    //  'runSolver' will see it abandon its task after some
    //  preliminary checks.
    //
    //  The following problem building functions are provided:
    //
    //      setProblemLabel
    //
    //          * takes    : tag (mandatory for this call)
    //          * returns  : void
    //          * logs     : nothing
    //          * relabels : problem (first set by the constructor)
    //
    //      setObjectiveSense
    //
    //          * takes    : objective sense {maximize, minimize}
    //          * optional : tag
    //          * returns  : void
    //          * action   : sets objective sense
    //          * logs     : overwrite calls
    //          * required : for problem completeness (else solve-time failure)
    //          * labels   : associated objective function
    //
    //      setObjectiveLabel
    //
    //          * takes    : tag (mandatory for this call)
    //          * returns  : void
    //          * logs     : nothing
    //          * relabels : objective function
    //
    //      incShift
    //
    //          * takes    : const term and loads incremementally
    //          * returns  : void
    //          * logs     : nothing
    //          * labels   : nothing
    //
    //      loadObj
    //
    //          * takes    : col (zero for "shift"), value (can be zero)
    //          * optional : tag
    //          * returns  : void
    //          * action   : loads obj_j
    //          * action   : applies structural variable 'XE_BNDS_DEFAULT' condition
    //          * logs     : non-progressive calls, overwrite calls
    //          * required : one call for problem completeness (else solve-time failure)
    //          * note     : unset cols are deemed zero by GLPK
    //          * labels   : associated structural variable
    //
    //      loadRhs
    //
    //          * takes    : row, value (can be negative), constraint sense {L, E, G}
    //          * optional : tag
    //          * returns  : void
    //          * action   : loads rhs_i
    //          * action   : sets the constraint sense i thus:
    //                           svif::L is <=
    //                           svif::E is  =
    //                           svif::G is >=
    //          * logs     : non-progressive calls, overwrite calls
    //          * required : one call for problem completeness (else solve-time failure)
    //          * note     : ranged constraints require two separate calls (like CPLEX)
    //          * labels   : associated constraint equation
    //
    //      loadCof
    //
    //          * takes    : row, col, value (zero is legal but rejected)
    //          * returns  : 'false' if zero value, otherwise 'true'
    //          * action   : loads coeff_ij
    //          * ignores  : zero values (which GLPK will also ignore)
    //          * logs     : zero values, overwrite calls
    //          * required : one call for problem completeness (else solve-time failure)
    //          * labels   : nothing to label
    //
    //      reviseBnds
    //
    //          * takes    : col, lower value, upper value
    //          * returns  : void
    //          * action   : sets bounds for the nominated structural variable
    //          * logs     : premature calls, overwrite calls
    //          * required : strictly optional
    //          * note     : will take 'inf' or '-inf' as an IEEE 754 (IEC 60559) infinity
    //          * note     : 'XE_BNDS_DEFAULT' actioned under 'loadObj'
    //          * labels   : nothing to label
    //
    //      markVarInteger
    //
    //          * takes    : col
    //          * returns  : void
    //          * action   : marks structural variable j as integer-valued
    //          * action   : returns col bounds as per 'loadObj'
    //          * logs     : premature calls, overwrite calls
    //          * warns    : non-integer bounds (GLPK will later reject problem)
    //          * relabels : associated structural variable
    //
    //      markVarBinary
    //
    //          * takes    : col
    //          * returns  : void
    //          * action   : marks col j as 0-1 binary
    //          * logs     : premature calls, overwrite calls
    //          * relabels : associated structural variable
    //
    //      markVarContinuous
    //
    //          * takes    : col
    //          * returns  : void
    //          * action   : marks col j as continuous (the default in any case)
    //          * action   : returns col bounds as per 'loadObj'
    //          * logs     : premature calls, overwrite calls
    //          * note     : the default is continuous and this call is not normally used
    //          * relabels : associated structural variable
    //
    //      reviseObj
    //          * takes    : col (zero for "shift"), value (can be zero)
    //          * omits    : tag (current value is retained)
    //          * returns  : previous objective value
    //          * action   : reloads obj_j
    //          * logs     : may or may not log all calls (read the code)
    //          * required : no
    //          * note     : should follow 'loadObj' call
    //          * note     : does not change tag, column kind, col bounds
    //          * labels   : associated structural variable
    //
    //      resetGlpkProb
    //
    //          * takes    : nothing
    //          * returns  : void
    //          * action   : removes built data and warm start info, retains problem label
    //          * logs     : none
    //
    //      recreateProblem (provided for test purposes only)
    //
    //          * optional : tag
    //          * returns  : void
    //          * action   : GLPK problem object is deleted and recreated
    //          * logs     : none
    //          * relabels : if passed tag, relabels problem, else recycles existing label
    //
    //  The solver call also performs some problem build
    //  integrity checks.
    //
    //  The following call and call order rules apply:
    //
    //      * the above calls -- except the three variable
    //      marking calls and 'reviseBnds' -- can be made in any
    //      order, need not be row and col incremental (termed
    //      "progressive" here), and can repeat earlier calls
    //      with either identical or different data (termed
    //      "overwrite" here)
    //
    //      * notwithstanding, GLPK requires a complete problem
    //      at solve-time
    //
    //      * a call to 'markVarInteger' or 'markVarBinary' (or
    //      'markVarContinuous' for that matter) MUST be made
    //      after the relevant 'loadObj' call and may NOT be made
    //      in advance.
    //
    //      * a call to 'reviseBnds' or 'reviseObj' MUST be made
    //      after the relevant 'loadObj' call and may NOT be made
    //      in advance.
    //
    //      * a non-progressive call to 'loadRhs' or 'loadObj' is
    //      legal, but will generate a log message regarding a
    //      'skip' or 'backfill' occurrence as appropriate
    //
    //      * an overwrite call (with identical or different
    //      data) is legal, but also will generate debug output
    //
    //      * if a structural variable is declared binary, any
    //      prior or subsequent bounds settings will be ignored
    //
    //      * if a structural variable is declared integer, any
    //      prior or subsequent bounds settings must be strictly
    //      integer-valued (value == floor(value))
    //
    //      * on completion, all rows and columns must be
    //      explicitly specified -- there cannot be "gaps"
    //
    //  The minimum specification is the constraint sense and (at
    //  least) one row, one col, and one constraint coefficient.
    //
    //  As currently implemented, warm starting is not supported.
    //  A call to 'resetGlpkProb' will remove all row and col LP
    //  status information.  An alternative method 'zeroProblem'
    //  which retained such information was considered but
    //  rejected because of the amount of new code needed.
    //  Hence, this solver interface does not presently offer
    //  warm start functionality.  Unless huge problems are being
    //  modeled, is unlikely that warm starting would provide for
    //  significant speed gains.

    void
    setProblemLabel                          // overwrite label set on construction
    (const std::string     tag);             // problem name tag, no default provided

    void
    setObjectiveSense
    (const ObjectiveSense  objSense,         // {minimize, maximize}
     const std::string     tag = "");        // objective function name tag, note default

    void
    setObjectiveLabel
    (const std::string     tag);

    void
    incShift                                 // increment the "shift"
    (const double          constTermValue);  // objective constant term value increment

    void
    loadObj
    (const int             col,              // col index (can be zero)
     const double          objValue,         // objective coefficient value
     const std::string     tag = "");        // structural variable name tag, note default

    void
    loadRhs
    (const int             row,              // row index
     const double          rhsValue,         // RHS constant value
     const ConstraintSense conSense,         // constraint sense {L, E, G}
     const std::string     tag = "");        // constraint equation name tag, note default

    bool                                     // 'false' indicates 'coeffValue' was zero
    loadCof
    (const int             row,              // row index
     const int             col,              // col index
     const double          coeffValue);      // structural coefficient value

    double                                   // return previous value
    setZeroTol                               // used to reset numerical zeros
    (const double numericalZero,             // 0.0 to disable
     const bool   resetCof,                  // 'true' is modify cof, 'false' is report
     const bool   resetVal);                 // 'true' is modify val, 'false' is report

    void
    reviseBnds
    (const int             col,              // col index
     const double          lowerValue,       // lower bound, can be '-inf'
     const double          upperValue);      // upper bound, can be 'inf'

    void
    resetDefaultBnds
    (const int             col);             // col index

    void
    markVarInteger
    (const int             col);             // col index

    void
    markVarBinary                            // uses GLP_BV (introduced in GLPK 4.16)
    (const int             col);             // col index

    void
    markVarContinuous                        // the default, only required if revising
    (const int             col);             // col index

    double                                   // prior value
    reviseObj
    (const int             col,              // col index (can be zero)
     const double          objValue);        // objective coefficient value

    void
    resetGlpkProb();                         // empty the problem instance

    void
    recreateProblem                          // delete/recreate problem instance
    (const std::string     tag = "");        // recycles prior label if tag is empty

    // ---------------------------------------------------------
    // METHODS INTENDED FOR TESTING

    void
    convertToLP();                           // downgrade to LP if currently MIP

    void
    toggleObjective();                       // multiply current objective by minus one

    glp_prob*                                // dangerous after 'SolverIf' host destructed
    obtainGlpProbPtr();                      // to provide direct access

    // ---------------------------------------------------------
    // PROBLEM INFORMATION : give the information as it stands,
    // calls can made before or during the problem build or
    // before or after the solver call

    std::string
    reportGlpkProblem();                     // using GLPK calls as far as is possible

    std::string                              // returns "" if label not set
    getProblemLabel() const;

    ProblemKlass                             // an 'svif::' enumeration
    getProblemKlass() const;                 // problem klass

    ObjectiveSense                           // an 'svif::' enumeration
    getObjectiveSense() const;               // objective sense, if set

    std::string
    getObjectiveSenseStr() const;            // interpreted

    std::string                              // with "obj-" (or similar) prefix
    getObjectiveLabel() const;

    std::string                              // without "obj-" (or similar) prefix
    getObjectiveTag() const;                 // tag for use in future 'SolverIf' calls

    ColumnKind
    getVarKind
    (const int col) const;

    std::string                              // with "va[rzb]-" (or similar) prefix
    getVarLabel
    (const int col) const;

    std::string                              // without "va[rzb]-" (or similar) prefix
    getVarTag                                // tag for use in future 'SolverIf' calls
    (const int col) const;

    int
    getVarCount() const;                     // number of variables, includes blank cols

    std::string
    getConLabel
    (const int row) const;

    std::string
    getConTag
    (const int row) const;

    int
    getConCount() const;                     // number of constraints, includes blank rows

    double                                   // automatically zero if not found
    getOrigCof                               // from input data, not the problem instance
    (const int row,
     const int col) const;

    double
    getProbCof                               // from the problem instance
    (const int row,
     const int col) const;

    int
    getNonZeroCoeffCount() const;            // number of non-zero structural coefficients

    bool
    isUseSimpPresolver() const;              // note the default is set on construction

    bool
    isLP() const;                            // all variables are continuous

    double
    getLowerBnd                              // zero by default
    (const int col) const;

    double
    getUpperBnd                              // inf by default
    (const int col) const;

    std::string                              // no trailing newline
    reportCof
    (const int          row,
     const int          col,
     const std::string& comment = "",
     const int          indent = 4) const;

    std::multimap<double, ij_type>           // elements thus (absolute_value, location)
    getAbsCofs() const;

    bool                                     // 'true' if streamed
    reportCofs
    (double&       span,                     // max_abs / min_abs
     std::ostream& os) const;

//      void
//      getFormattedProblem() const;             // not implemented, use 'GlpkViz' instead

    // ---------------------------------------------------------
    // POST-BUILD MANIPULATION

    std::vector<double>                      // return current objective function
    getObjective() const;

    bool                                     // 'true' for success
    renewObjective
    (const std::vector<double> objFunc,      // entire function
     const std::string         tag = "");    // objective function name tag, note default

    bool                                     // 'true' for success
    zeroBarMeObjective
    (const int         nonzeroGol,           // nominate the only non-zero column
     const double      value,                // objective coefficient value (often 1.0)
     const std::string tag = "");            // new objective function name, note default

    // ---------------------------------------------------------
    // SOLVER INVOCATION

    void
    runSolver();                             // sole point of entry

    // ---------------------------------------------------------
    // SOLUTION RECOVERY : usage must follow solver call

    double
    getObjectiveValue() const;               // the objective function value

    double
    getVarValue                              // a structural variable value
    (const int col) const;

    int
    getIntegerVarValue                       // a structural variable value
    (const int col) const;

    bool                                     // 'false' = 0 and 'true' = 1 (like C++)
    getBinaryVarValue                        // a structural variable value
    (const int col) const;

    double
    getObjCoeff                              // an objective coefficient
    (const int col) const;

    double
    getObjCoeffXVarValue                     // getObjCoeff * getVarValue
    (const int col) const;

    double
    getSlackValue                            // reduced cost of an auxiliary variable
    (const int row) const;

    double
    getShadowValue                           // reduced cost of a structural variable
    (const int col) const;

    bool
    isUsableSoln() const;                    // is feasible plus other usefulness tests

    bool
    isOptimalSoln() const;                   // proven optimal

    std::string
    getFormattedResults                      // reports Z and x_j using formatted text
    (const std::string objectiveTag = "Z",
     const std::string variableTag  = "x");

    // ---------------------------------------------------------
    // GENERAL INFORMATION

    const std::string                        // CAUTION: consider also 'getProblemLabel'
    getProblemTag() const;                   // as submitted on construction or revised

    int
    getAlertCount() const;                   // number of alerts

    void
    writeInfo() const;                       // wrapper to take current label as filestem

    void
    writeInfo                                // wrapper to take a C-string
    (const char* filestem) const;            // filestem name, extensions added later

    void
    writeInfo                                // working function, calls GLPK info routines
    (const std::string filestem) const;      // filestem name, extensions added later

    std::string                              // gives "yes" or "no"
    yesOrNo
    (const bool trueOrFalse) const;          // used to reinterpret bool-returning calls

    static
    const std::string                        // string format 0.00
    getGlpkVersion();                        // calls 'glp_version'

    // ---------------------------------------------------------
    // MISCELLANEOUS

    double
    getInf() const;                          // returns IEEE 754 (IEC 60559) infinity

    double
    getNaN() const;                          // returns IEEE 754 (IEC 60559) not-a-number

    // ---------------------------------------------------------
    // KARUSH-KUHN-TUCKER (KKT) REPORTING

  private:

    void
    reportKkt() const;

    void
    createKktReport
    (std::ostream& os,
     const double  ae_max,
     const int     ae_ind,
     const double  re_max,
     const int     re_ind,
     const int     indent = 2) const;        // left margin

    // ---------------------------------------------------------
    // UTILITY FUNCTIONS

  private:

    // problem creation methods -- called in the sequence given

    void
    utilCreateGlpkProb
    (const std::string tag = "");

    void
    utilSetGlpkDefaults();

    const std::string                        // recover the problem label
    utilDeleteGlpkProb();

    // support methods

    void
    utilSetDefaultBnds                       // controlled by 'XE_BNDS_DEFAULT' macro
    (int col);

    void
    utilResetCoeffVectors();                 // reset the three sparse matrix vectors

    void
    utilResetObjectData();                   // reset the relevant status variables

    // solver methods

    void
    solverCheckProb();                       // only issues logging messages

    void
    solverInvokeSolver();

    void
    solverAnalyzeSoln();

    std::string
    utilReportOnProb();                      // passive and hence optional

    std::string
    utilReportOnSolver();                    // passive and hence optional

    std::string
    utilReportOnSoln();                      // passive and hence optional

    // housekeeping methods

    int                                      // current alert count
    incrementAlertCount();                   // alerts mostly arise from data issues

    // output formatting methods used by reporting utilities

    template <typename T>                    // implicit instantiation usually okay
    inline                                   // compiler hint
    std::string                              // output format depends on implementation
    numToString                              // number to string conversion
    (const T number) const;                  // usually int or double

    inline                                   // compiler hint
    std::string                              // output format depends on implementation
    indexToString                            // index to string conversion
    (const int index) const;

    void
    formatLineOutput                         // for standard reporting of text
    (std::ostringstream& ssOut,
     const std::string   tag,
     const std::string   value);             // used for strings

    template <typename T>                    // implicit instantiation usually okay
    void
    formatLineOutput                         // for standard reporting of numbers
    (std::ostringstream& ssOut,
     const std::string   tag,
     const T             value,              // used for ints and doubles
     const std::string   extra = "");        // optional trailing string

    void
    updateProbStatus();                      // update 'd_probStatus' data member

    void
    updateProbKlass();                       // update 'd_probKlass' data member

    // numerical zero methods

    double
    zeroTripCof                              // intercept input and fix numerical zeros
    (const double cof) const;                // coefficient

    double
    zeroTripVal                              // intercept output and fix numerical zeros
    (const double val) const;                // value

    // =========================================================
    // INTERNAL DATA

  private:

    // problem information

    std::string            d_probTag;        // set in constructor, can be overwritten

    // status variables

    bool                   d_employScaling;  // call 'glp_scale_prob'
    bool                   d_employAdvBasis; // call 'glp_adv_basis'
    bool                   d_simplexPresol;  // set 'glp_smcp::presolve'
    bool                   d_integerPresol;  // set 'glp_iocp::presolve'
    svif::CofSpan          d_cofSpanLevel;   // controls coefficient span tests

    ProblemStatus          d_probStatus;     // problem status
    ObjectiveSense         d_objSense;       // minimize or maximize
    ProblemKlass           d_probKlass;      // problem klass
    SolverType             d_solverType;     // solver type, either advisory or actual
    SolutionStatus         d_solnStatus;
    int                    d_solverRet;      // if set, GLPK three-digit integer, else 0
    int                    d_solnRet;        // if set, GLPK three-digit integer, else 0

    // problem administration information

    ReportingLevel         d_noise;          // also dependent on output suppression
    int                    d_solverCalls;    // solver call count
    int                    d_alertCount;     // local alert count

    // sparse matrix vectors used during problem construction
    // (naming as per GLPK tutorial)
    //
    // Josuttis (1999 p155) writes: "whenever you need an array
    // of type T for any reason (such as for an existing C
    // library) you can use a std::vector<T> and pass it the
    // address of the first element".  Namely &vec[0].  The
    // reason this works is that std::vector elements, like C
    // array elements, are required to be held in contiguous
    // memory.

    std::vector<int>       d_ia;             // constraint coefficient i index
    std::vector<int>       d_ja;             // constraint coefficient j index
    std::vector<double>    d_ar;             // constraint coefficient value

    // numerical zero tolerances

    double                 d_zeroTol;        // numerical zero tolerance used by 'loadCof'
    bool                   d_zeroCof;        // controls numerical zero cof modification
    bool                   d_zeroVal;        // controls numerical zero val modification

    // GLPK problem instance (a typedef to a C struct)

    glp_prob*              d_prob;           // initialized GLPK problem instance

    glp_smcp               d_parmSimplex;    // simplex parameters
    glp_iptcp              d_parmInterior;   // interior parameters
    glp_iocp               d_parmInteger;    // integer parameters

    // ---------------------------------------------------------
    // STATIC DATA

    // CAUTION: 's_os' is a stream pointer and not a stream reference

    static std::ostream*   s_os;             // combined class native and GLPK output
    static bool            s_noise;          // 'true' means output enabled

    static int             s_instanceCount;  // instance count, used for warnings only
    static int             s_probLabelCount; // problem label count
    static const int       s_glpkMargin = 1; // GLPK does not often utilize array index 0
    static logga::spLogger s_logger;         // shared_ptr to single logger object

    // formatted output, for use in std::setw() and std::setprecision()

    static const int s_indexPad = 2;         // 0 to disable, 2 or more to zero-pad

    static const int s_W0 =  3;              // indent
    static const int s_W1 = 33;              // description
    static const int s_W2 =  2;              // ":"
    static const int s_W3 =  7;              // number alignment
    static const int s_P1 =  4;              // significant figures

  }; // class 'svif::SolverIf'

} // namespace svif

#endif // _SIGLP_H_

//  end of file

