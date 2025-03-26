//  XEONA ENERGY SYSTEMS MODELING ENVIRONMENT
//
//  file-create-name : siglp.cc
//  file-create-date : Tue 22-Apr-2008 14:37 UTC
//  file-initiator   : Robbie Morrison <robbie@actrix.co.nz>
//
//  file-role        : semi-intelligent interface to GLPK MILP solver / implementation
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
//  $URL: file:///home/robbie/svn-root/xeona/futz/trunk/xeona1/d/siglp.cc $
//
//  GENERAL NOTES FOR THIS FILE
//
//  List of depreciated GLPK APIs:
//
//      4.18: lpx_simplex
//
//      4.20: lpx_integer          / see: glp_iotopt
//
//      4.29: lpx_read_mps
//            lpx_read_freemps
//            lpx_write_mps
//            lpx_write_freemps
//            lpx_read_cpxlp
//            lpx_write_cpxlp
//
//      4.31: lpx_scale_prob
//            lpx_std_basis
//            lpx_adv_basis
//            lpx_cpx_basis
//
//      4.32: lpx_integer
//            lpx_intopt
//
//      4.33: lpx_exact
//            lpx_get_ray_info     / see: glp_get_unbnd_ray
//            lpx_interior
//            lpx_read_model
//
//      4.37: lpx_print_sol
//            lpx_print_ips        / see: glp_print_ipt
//            lpx_print_mip
//            lpx_print_prob       / see: glp_print_lp
//
//      4.38: glp_ipt_status       / not depreciated but two new return macros added
//
//      4.41: lpx_transform_row
//            lpx_transform_col
//            lpx_prim_ratio_test
//            lpx_dual_ratio_test
//
//      4.42: lpx_print_sens_bnds  / glp_print_ranges
//            glp_write_prob       / new call
//
//  AD-HOC NOTES
//
//  The 'xeona::readonly' calls are non-essential can be disabled
//  if need be.

//  LOCAL AND SYSTEM INCLUDES

#include "siglp.h"            // companion header for this file (place first)

#include "../c/files.h"       // free functions for regular files

#include "../a/logger.h"      // standard logging functionality (as required)
#include ".././common.h"      // common definitions for project (place last)

#include <algorithm>          // STL copying, searching, and sorting
#include <fstream>            // file-based io
#include <iomanip>            // setw() and family
#include <limits>             // numeric_limits<T>::infinity() and similar
#include <ostream>            // output streams
#include <sstream>            // string-streams
#include <string>             // C++ strings

#include <cfloat>             // C-style DBL_MAX (1.79769e+308)
#include <cmath>              // C-style maths, abs(), ceil(), floor(), sqrt()
#include <cstdio>             // C-style io, remove(), EOF
#include <cstdlib>            // C-style exit(), getenv(), system(), NULL, EXIT_SUCCESS
#include <ctime>              // C-style time and date functions

#include <fcntl.h>            // C-style file control, manipulate file descriptor flags
#include <fenv.h>             // C-style C99 and TR1 floating point environment
#include <sys/stat.h>         // C-style POSIX file characteristics, stat()
#include <unistd.h>           // POSIX sleep(), usleep(), access(), chown()

#include <boost/algorithm/string.hpp>             // string recasing, trimming, splitting
#include <boost/algorithm/string_regex.hpp>       // additional regex support
#include <boost/format.hpp>                       // printf style formatting
#include <boost/foreach.hpp>                      // BOOST_FOREACH iteration macro

//  INTERNAL MACRO SETTING

#define STD_OSTREAM std::clog           // this aligns with current logging practice [1]

// [1] this should really be a static variable and not a pre-processor macro

#if !defined(XE_BNDS_DEFAULT)           // gives possibilty of external override
# define XE_BNDS_DEFAULT 1              // 0 = free, 1 = non-negative / user-modifiable
#endif

#if   (XE_BNDS_DEFAULT == 0)
# warning "XE_BNDS_DEFAULT macro set to free (which may be correct)"
#elif (XE_BNDS_DEFAULT == 1)
#else
# error "XE_BNDS_DEFAULT macro not set to a supported value {0,1}"
#endif

// MACRO-SET CONSTANT

namespace svif
{
  //  DEFINITIONS: debug flag
  //  controlled by compile-time _XDEBUG macro

#ifdef _XDEBUG                               // xeona macro
  const bool DBUG = true;
#else
  const bool DBUG = false;
#endif

} // namespace svif

//  CODE

namespace
{
  // ---------------------------------------------------------
  //  LOCAL CONSTANT  : ::len
  //  LOCAL CONSTANT  : ::PRO
  //  LOCAL CONSTANT  : ::OBJ
  //  LOCAL CONSTANT  : ::CON
  //  LOCAL CONSTANT  : ::VAR
  //  LOCAL CONSTANT  : ::VAZ
  //  LOCAL CONSTANT  : ::VAB
  //  LOCAL CONSTANT  : ::SEP
  // ---------------------------------------------------------
  //  Description  : string constants for use in standardized labeling
  //  Role         : support
  //  Note         : modifiable by users, set to empty string to disable
  //
  //  CAUTION: all prefix strings must be of length 'len'
  //
  // ---------------------------------------------------------

  const int         len = 3;          // CAUTION: fixed string length required
  const std::string PRO = "pro";      // problem prefix / user-modifiable
  const std::string OBJ = "obj";      // objective function prefix / user-modifiable
  const std::string CON = "con";      // constraint equation prefix / user-modifiable
  const std::string VAR = "var";      // real-valued variable prefix / user-modifiable
  const std::string VAZ = "vaz";      // integer-valued variable prefix / user-modifiable
  const std::string VAB = "vab";      // 0-1-valued prefix (binary) / user-modifiable
  const std::string SEP = ".";        // separating string (say . or -) / user-modifiable

  // ---------------------------------------------------------
  //  FREE FUNCTION   : ::trimPrefixPlus
  // ---------------------------------------------------------
  //  Description  : for example, left trim "var01." from "var01.more.details"
  //  Role         : local helper function
  //  Techniques   : Boost.String_alg library and 'regex'
  //  Status       : complete
  // ---------------------------------------------------------

  bool                                       // 'true' indicates change made
  trimPrefixPlus
  (std::string& tag)                         // non-const pass-by-ref
  {
    // logging support
    static logga::spLogger logger = logga::ptrLogStream();  // main function logger

    // preamble
    const std::string original = tag;        // keep for later comparison

    // hard-coded prefix list
    const std::string prefixes[] = { ::PRO, ::OBJ, ::CON, ::VAR, ::VAZ, ::VAB };
    std::string separator        = ::SEP;

    // modify the separator if need be: a plain dot has special
    // meaning and needs to be escaped -- and the first escape
    // slash needs to be escaped again for C++ (read the manual!)
    if ( separator == "." ) separator = "\\.";

    // active code
    BOOST_FOREACH( std::string prefix, prefixes )
      {
        std::ostringstream oss;
        oss << "^" << prefix << "[[:digit:]]*" << separator;
        const boost::regex pattern(oss.str());
        boost::algorithm::erase_regex(tag, pattern);
        if ( original != tag )
          {
#if 1 // 0 = no reporting, 1 = use reporting
            logger->repx(logga::adhc, "current pattern", pattern);  // 'regex's stream
#endif // 0
            return true;
          }
      }

    // no match code
#if 0 // 0 = no reporting, 1 = use reporting
    logger->repx(logga::adhc, "no trim applied", original);
#endif // 0

    return false;

  } // function '::trimPrefixPlus'

  // ---------------------------------------------------------
  //  FREE FUNCTION   : ::termHook
  // ---------------------------------------------------------
  //  Description  : insert GLPK message into our nominated std::ostream
  //  Role         : used in combination with 'glp_term_hook'
  //  Techniques   : C hook function, void* (really!)
  //  Status       : complete
  // ---------------------------------------------------------

  int
  termHook
  (void*       info,                         // pointer to ostream as it happens
   const char* s)                            // information to be captured as C-string
  {
    std::ostream* os = static_cast<std::ostream*>(info);    // cast from void*
    *os << s;                                // need to dereference os
    return 1;                                // non-zero to suppress normal GLPK output
  }

  // ---------------------------------------------------------
  //  FREE FUNCTION   : ::seq
  // ---------------------------------------------------------
  //  Description  : creates a sequence of integers for use in GLPK APIs
  //  Role         : use in 'resetGlpkProb'
  //  Techniques   : std::vector
  //  Status       : complete
  //
  //  Design notes
  //
  //      The call 'seq(2, 4)' returns a vector containing:
  //
  //          { 2, 3, 4 }
  //
  //      STL 'std::vector' vectors can be used as C arrays thus.
  //      If a function takes a C array pointer, simply pass in
  //      the address of the first element of the vector instead:
  //
  //          &vec[0]
  //
  //      See also
  //
  //          Lischner (2003 pp338-339) shows how to code the
  //          following using the 'std::generate' algorithm.
  //
  // ---------------------------------------------------------

  std::vector<int>                           // can be an empty vector
  seq
  (const int lower,                          // lower element
   const int upper)                          // upper element
  {
    static logga::spLogger logger = logga::ptrLogStream();  // bind logger

    // passive integrity check
    if ( lower >= upper )
      {
        std::ostringstream oss;
        oss << lower << " : " << upper;
        logger->repx(logga::warn, "mismatch, lower : upper", oss.str());
      }

    // active code
    std::vector<int> vec;
    for ( int i = lower;                     // start value
          i <= upper;                        // end value
          ++i )                              // simple progression
      {
        vec.push_back(i);
      }
    return vec;
  }

  // ---------------------------------------------------------
  //  FREE FUNCTION   : ::isIntegerValued
  // ---------------------------------------------------------
  //  Description  : test whether a variable bound is integer-valued
  //  Role         : integrity check
  //  Techniques   : 'floor'
  //  Status       : complete
  //
  //  Design notes
  //
  //      Uses the 'floor' test from <cmath>.  This gave the same
  //      apparent behavior as the following double cast test:
  //
  //          x == double(int(x))  // but C-style casts not recommended
  //
  //     Note the following result:
  //
  //         double eps = std::numeric_limits<double>::epsilon();
  //         isIntegerValued(3.0 + eps) returns 'true'
  //         isIntegerValued(3.0 + 1.1 * eps) returns 'false'
  //
  //    On IA32 systems (at least on mine), eps is 2.22045e-16.
  //
  //    It is necessary to use the absolute value to a prevent
  //    failure with negative numbers.  Note too that 'trunc' is
  //    a Microsoft addition and not generally available.
  //
  // ---------------------------------------------------------

  bool                                       // output, 'true' means integer-valued
  isIntegerValued
  (double x)                                 // input as double
  {
    return ( x == std::floor(std::abs(x)) ) ? true : false;
  }

} // unnamed namespace

namespace svif
{
  // ---------------------------------------------------------
  //  FREE FUNCTION   : svif::refDumpStream
  // ---------------------------------------------------------
  //  Description  : used to dump common ouput
  //  Role         : used by 'outputSuspend' and 'outputResume'
  //  Techniques   : free function with static data
  //  Status       : complete
  //
  //  Design notes
  //
  //      Curiously, one can write to a closed stream as a way of
  //      disposing of data.  For instance, the following is
  //      seems fully acceptable:
  //
  //          std::ofstream null("");
  //          null.is_open();              // returns 'false'
  //          null << "hi" << std::endl;
  //
  //      The use of static data means that the same stream is
  //      always returned.
  //
  // ---------------------------------------------------------

  std::ofstream&
  refDumpStream()
  {
#if 1 // 1 = dump to waste, 0 = redirect to file / user-modifiable
    static std::ofstream s_stream("");
#else
    const std::string filename = "sos-dump.log";
    static std::ofstream s_stream(filename.c_str(), std::ios::out|std::ios::trunc);
    if ( !s_stream )
      {
        std::cout << std::flush;
        std::clog << "** file open failed : " << s_stream << std::endl;
      }
#endif // 0
    return s_stream;
  }

  //  STATIC DEFINITIONS

  std::ostream*   SolverIf::s_os             = &STD_OSTREAM;
  bool            SolverIf::s_noise          = true;                  // mostly passive
  int             SolverIf::s_probLabelCount = 0;
  int             SolverIf::s_instanceCount  = 0;                     // initially zero
  logga::spLogger SolverIf::s_logger         = logga::ptrLogStream(); // bind

  // CREATORS

  // ---------------------------------------------------------
  //  CONSTRUCTOR     : SolverIf
  // ---------------------------------------------------------
  //  Description  : construct solver interface object for client code
  //  Role         : creator
  //  Techniques   : calls 'utilCreateGlpkProb' to make GLPK problem instance
  //  Status       : complete
  //
  //  Design notes
  //
  //      Sparse matrix vectors 'd_ia', 'd_ja', and 'd_ar' are
  //      initialized here.  The index-zero (first) element is
  //      not used in GLPK (excepting obj_0 which is the
  //      objective 'shift'), hence the rather unusual
  //      initialization calls.  See also comments in the code.
  //
  // ---------------------------------------------------------

  SolverIf::SolverIf
  (std::string    tag,                       // problem name tag
   ReportingLevel noise) :

    // set fixed member variables
    d_probTag(tag),

    // set defaults for status variables
    d_employScaling(false),                  // scale problem
    d_employAdvBasis(false),                 // use advanced initial LP basis
    d_simplexPresol(false),                  // simplex presolver
    d_integerPresol(false),                  // MIP presolver
    d_cofSpanLevel(svif::e_inactive),        // controls coefficient span tests (default)

    d_probStatus(status_not_specified),      // determined in this code
    d_objSense(sense_not_specified),         // must be set in client code
    d_probKlass(prob_not_specified),         // determined in this code
    d_solverType(solver_other),              // acceptable default behavior is provided
    d_solnStatus(soln_undefined),            // usefulness of solution
    d_solverRet(0),                          // if set, GLPK three-digit int
    d_solnRet(0),                            // if set, GLPK three-digit int

    // set problem administration information
    d_noise(noise),                          // currently no provision to reset
    d_solverCalls(0),
    d_alertCount(0),                         // interface thru 'incrementAlertCount'

    // prepare sparse matrix vectors -- pad the start of matrix
    // with zeros as required, zeros are fine because they are
    // illegal as indices and ineffective as coefficient values
    d_ia(s_glpkMargin, 0),
    d_ja(s_glpkMargin, 0),
    d_ar(s_glpkMargin, 0.0),

    // numerical zero tolerances
    d_zeroTol(0.0),                          // a true zero will disable
    d_zeroCof(false),                        // the default is not too modify
    d_zeroVal(false),                        // the default is not too modify

    // set GLPK problem instance pointer to null
    d_prob(NULL),
    d_parmSimplex(),                         // struct holding parameters
    d_parmInterior(),
    d_parmInteger()

  {
#if 0 // 1 = reserve space in constructor, 0 = omit
    const int SIZE = 1000;
    d_ia.reserve(SIZE);
    d_ja.reserve(SIZE);
    d_ar.reserve(SIZE);
    s_logger->repx(logga::info, "set reserve space in 3 arrays", SIZE);
#endif // 0

    // set reporting level as required (whilst noting that
    // logging verbosity is CONTROLLED by the app)
    if ( d_noise == svif::not_specified )    // meaning that the constructor defaulted
      {
        d_noise                    = svif::low;   // user-modifiable
        if ( svif::DBUG ) d_noise  = svif::high;  // user-modifiable
      }

    // initial reporting, including debug status
    s_logger->repx(logga::dbug, "constructor call", "");

    // default initialize the GLPK parameter structs
    glp_init_smcp (&d_parmSimplex);
    glp_init_iptcp(&d_parmInterior);
    glp_init_iocp (&d_parmInteger);

    // bind GLOBAL GLPK output to our nominated std::ostream 's_os'
    glp_term_hook(::termHook, s_os);        // void* type

    // create GLPK problem
    utilCreateGlpkProb(d_probTag);

    // overwrite GLPK defaults with the values hard-coded in this
    // file
    //
    // CAUTION: the outcome can depend on the value of
    // 'svif::DBUG' so read the code for the following call
    //
    utilSetGlpkDefaults();

    // defensive programming (later on, even addressing 'd_prob'
    // can segfault)
    if ( d_prob == NULL )
      {
        s_logger->repx(logga::warn, "NULL problem unexpected, d_prob", d_prob);
      }

    // completion reporting
    s_logger->repx(logga::adhc, "leaving member function", "");

  } // constuctor 'SolverIf'

  // ---------------------------------------------------------
  //  DESTRUCTOR      : ~SolverIf (non-virtual)
  // ---------------------------------------------------------
  //  Description  : destructor
  //  Role         : (un)creator
  //  Techniques   : calls 'utilDeleteGlpkProb' to clean up GLPK problem instance
  //  Status       : complete
  //
  //  CAUTION: free GLPK environment
  //
  //      This destructor assumes that there will be no
  //      non-SolverIf (rogue) GLPK problem instances in
  //      existence.  Otherwise disable the 'freeGlpkEnvironment'
  //      call and manage the issue elsewhere.
  //
  // ---------------------------------------------------------

  SolverIf::~SolverIf()
  {
    // intial reporting
    s_logger->repx(logga::adhc, "destructor call", "");

    // flush local reporting stream
    *s_os << std::flush;                     // CAUTION: different from the logging stream

    // delete GLPK problem
    utilDeleteGlpkProb();                    // decrements GLPK problem instance count

#if 1 // 1 = free GLPK environment, 0 = disable / user-modifiable

    // free GLPK environment by calling a public member function
    // which then calls 'glp_free_env'
    if ( s_instanceCount == 0 )
      {
        s_logger->repx(logga::dbug, "about to call freeGlpkEnvironment", "");
        freeGlpkEnvironment();
      }
#endif // 0

    // report appropriately
    if ( d_alertCount == 0 )
      {
        s_logger->repx(logga::dbug, "destructor call, alert count", d_alertCount);
      }
    else
      {
        s_logger->repx(logga::info, "destructor call, alert count", d_alertCount);
      }

  } // destructor 'SolverIf'

  // APPLICATION CLEAN-UP

  // ---------------------------------------------------------
  //  MEMBER FUNCTION : freeGlpkEnvironment (static)
  // ---------------------------------------------------------
  //  Description  : frees the GLPK library environment
  //  Role         : application clean-up
  //  Techniques   : 'glp_free_env'
  //  Status       : complete
  //
  //  Design notes
  //
  //      A memory leak of 140 bytes currently results on app
  //      exit if not called.  'valgrind' notes:
  //
  //          "_glp_lib_init_env (glplib04.c:65)"
  //          "still reachable: 140 bytes in 1 blocks"
  //
  //  CAUTION: call placement
  //
  //      Called when the 'SolverIf' destructor is invoked on the
  //      last instance.
  //
  //      But if employed, use only near the end of the 'main'
  //      function because any existing GLPK problem instances
  //      will be unceremoniously "invalidated".
  //
  // ---------------------------------------------------------

  bool                                       // 'false' if problems encountered
  SolverIf::freeGlpkEnvironment()            // calls 'glp_free_env', warns if instances
  {
    bool ret = true;                         // return value
    s_logger->repx(logga::info, "about to call GLPK glp_free_env", "");
    if ( s_instanceCount != 0 )
      {
        s_logger->repx(logga::warn, "will clobber problem instances", s_instanceCount);
        ret = false;
      }
    glp_free_env();                          // GLPK API, void return
    return ret;
  }

  // REPORTING CONTROL : class-wide calls controlling native GLPK
  // output -- as opposed to xeona-style logging

  // ---------------------------------------------------------
  //  MEMBER FUNCTION : stdoutRedirect (static)
  // ---------------------------------------------------------
  //  Description  : permanently redirect stdout
  //  Role         : application-wide reporting control
  //  Techniques   : C-style UNIX code
  //  Status       : complete
  //
  //  Design notes
  //
  //      This call is a hack!  Ideally the block on/off
  //      functions should be implemented using 's_os.'
  //
  //      The code is based on Robbins and Robbins (2003 p131).
  //      Who, in turn, highly recommend Stevens (1992) for UNIX
  //      io issues.
  //
  //      All the system calls employed are POSIX-conforming.
  //
  //  CAUTION: global and permanent
  //
  //      The final splash screen will be affected.
  //
  //  References
  //
  //      Robbins, Kay A and Steven Robbins.  2003 UNIX systems
  //        programming : communication, concurrency, and threads
  //        -- Second edition.  Prentice Hall PTR, Upper Saddle
  //        River, New Jersey, USA.  ISBN 0-13-042411-0.
  //
  //      Stevens, Richard W.  1992.  Advanced programming in the
  //        UNIX environment.  Addison-Wesley, Reading,
  //        Massachusetts, USA.
  //
  // ---------------------------------------------------------

  void
  SolverIf::stdoutRedirect
  (const std::string logname)
  {
    //  CAUTION: the POSIX macros S_IRGRP and S_IROTH are not
    //  defined in Windows as there is no way to deal with
    //  permissions for group and others.

    s_logger->repx(logga::dbug, "entering member function", "");

    if ( logname.empty() )
      {
        // simply close stdout
        if ( close(STDOUT_FILENO) == -1 )    // macro defined in <unistd.h>
          {
            perror("perror: failed to close stdout / system message");
            return;
          }
      }
    else // logname is not empty
      {
        // open file
        int fd;                              // POSIX UNIX-style file descriptor
        fd = open(logname.c_str(),           // POSIX UNIX-style io call
                  O_WRONLY | O_CREAT | O_APPEND,  // file create flags
                  S_IRUSR | S_IWUSR);             // file create mode
        if ( fd == -1 )                      // on fail, 'open' also sets 'errno'
          {
            std::ostringstream oss;
            oss << "perror: "
                << "failed to open file: "
                << logname
                << "  / system message";
            perror(oss.str().c_str());       // POSIX extension CX from <cstdio>
            return;
          }
        else
          {
            std::ostringstream oss;
            oss << "opened file descriptor " << fd;
            s_logger->repx(logga::dbug, oss.str(), logname);
          }

        // close stdout (optional, 'dup2' does this anyway, but also omits error messages)
        if ( close(STDOUT_FILENO) == -1 )    // macro defined in <unistd.h>
          {
            perror("perror: failed to close stdout / system message");
            return;
          }

        // remap stdout to file
        if ( dup2(fd, STDOUT_FILENO) == -1 ) // POSIX call
          {
            std::ostringstream oss;
            oss << "perror: "
                << "failed to redirect stdout to: "
                << logname
                << "  / system message";
            perror(oss.str().c_str());       // POSIX extension CX from <cstdio>
            return;
          }
        else
          {
            std::ostringstream oss;
            oss << "redirecting stdout " << STDOUT_FILENO << " to";
            s_logger->repx(logga::info, oss.str(), logname);
          }

      } // logname not empty

    return;

  } // function 'SolverIf::stdoutRedirect'

  // ---------------------------------------------------------
  //  MEMBER FUNCTION : outputSuspend (static)
  //  MEMBER FUNCTION : outputResume  (static)
  // ---------------------------------------------------------
  //  Description  : toggle GLPK output and native reporting
  //  Role         : class-wide reporting control
  //  Techniques   : 'glp_term_out', 's_noise' (as opposed to 'd_noise')
  //  Status       : complete
  //
  //  Design notes
  //
  //      These static functions toggle:
  //
  //          * the GLPK output which results from GLPK API calls
  //          * native reporting results from local calls
  //
  //      but do not affect:
  //
  //          * normal logging
  //
  //      The actions are global and affect all 'SolverIf'
  //      instances.
  //
  //      However, normal logging is controlled by the app in
  //      association with the 'logger' unit.
  //
  //  CAUTION: GLPK environment needed
  //
  //      The call to 'glp_term_out' requires an existing GLPK
  //      library environment but not necessarily any live GLPK
  //      problem instances.  Hence the protection below and the
  //      delayed call in 'utilCreateGlpkProb' when needed.
  //
  // ---------------------------------------------------------

  void
  SolverIf::outputSuspend()
  {
    // CAUTION: 's_os' is a stream pointer and not a stream reference
    s_os = &svif::refDumpStream();           // free function with static data
    s_logger->repx(logga::info, "common out bound to closed stream", s_os);
    s_noise = false;

    // the following should only be seen by the dump stream
    *s_os << "** new redirect order" << std::endl;
  }

  void
  SolverIf::outputResume()
  {
    // CAUTION: 's_os' is a stream pointer and not a stream reference
    s_os = &STD_OSTREAM;
    s_logger->repx(logga::info, "common out bound to STD_OSTREAM", s_os);
    s_noise = true;
  }

  // SOLVER PREFERENCES : instance-based

  // ---------------------------------------------------------
  //  MEMBER FUNCTION : initSetPrefLPSolver
  // ---------------------------------------------------------
  //  Description  : choose between simplex and interior point solver
  //  Role         : initialization method, optional
  //  Techniques   : internal enum
  //  Status       : complete
  // ---------------------------------------------------------

  void
  SolverIf::initSetPrefLPSolver
  (SolverType solverType)
  {
    // initial reporting
    s_logger->repx(logga::adhc, "entering member function", "");

    d_solverType = solverType;
    s_logger->repx(logga::dbug, "preferred solver set, solver type", d_solverType);
  }

  // ---------------------------------------------------------
  //  MEMBER FUNCTION : initEmployScaling
  // ---------------------------------------------------------
  //  Description  : elect to use problem scaling
  //  Role         : initialization method, optional
  //  Techniques   : 'd_employScaling'
  //  Status       : complete
  // ---------------------------------------------------------

  void
  SolverIf::initEmployScaling()
  {
    d_employScaling = true;
    s_logger->repx(logga::dbug, "employ scaling set", d_employScaling);
  }

  // ---------------------------------------------------------
  //  MEMBER FUNCTION : initEmployAdvBasis
  // ---------------------------------------------------------
  //  Description  : elect to use advanced initial LP basis
  //  Role         : initialization method, optional
  //  Techniques   : 'd_employAdvBasis'
  //  Status       : complete
  // ---------------------------------------------------------

  void
  SolverIf::initEmployAdvBasis()
  {
    d_employAdvBasis = true;
    s_logger->repx(logga::dbug, "employ advanced initial bassis set", d_employAdvBasis);
  }

  // ---------------------------------------------------------
  //  MEMBER FUNCTION : initSimplexPresolver
  // ---------------------------------------------------------
  //  Description  : elect to use integer presolver
  //  Role         : initialization method, optional
  //  Techniques   : 'glp_smcp::presolve'
  //  Status       : complete
  // ---------------------------------------------------------

  void
  SolverIf::initSimplexPresolver()
  {
    d_parmSimplex.presolve = GLP_ON;         // simplex presolver
    d_simplexPresol = true;                  // used when reporting
    s_logger->repx(logga::dbug, "simplex presolver set", d_parmSimplex.presolve);
  }

  // ---------------------------------------------------------
  //  MEMBER FUNCTION : initIntegerPresolver
  // ---------------------------------------------------------
  //  Description  : elect to use integer presolver
  //  Role         : initialization method, optional
  //  Techniques   : 'glp_iocp::presolve'
  //  Status       : complete
  // ---------------------------------------------------------

  void
  SolverIf::initIntegerPresolver()
  {
    d_parmInteger.presolve = GLP_ON;         // mixed-integer presolver
    d_integerPresol = true;                  // used when reporting
    s_logger->repx(logga::dbug, "integer presolver set", d_parmInteger.presolve);
  }

  // ---------------------------------------------------------
  //  MEMBER FUNCTION : initCoeffSpan
  // ---------------------------------------------------------
  //  Description  : set the coefficients span level of analysis
  //  Role         : initialization method, optional
  //  Techniques   : 'switch'
  //  Status       : complete
  // ---------------------------------------------------------

  void
  SolverIf::initCoeffSpan
  (const int level)
  {
    switch ( level )
      {
      case 0: d_cofSpanLevel = svif::e_inactive; break;
      case 1: d_cofSpanLevel = svif::e_say_fail; break;
      case 2: d_cofSpanLevel = svif::e_say_each; break;
      default:
        s_logger->repx(logga::kill, "coding error, level not supported", level);
        break;
      }
    s_logger->repx(logga::dbug, "KKT report set", d_cofSpanLevel);
  }

  // PROBLEM BUILDING : entirely dynamic now, no ahead-of-time
  // size and klass estimates are solicited

  // ---------------------------------------------------------
  //  MEMBER FUNCTION : setProblemLabel
  // ---------------------------------------------------------
  //  Description  : relabel problem
  //  Role         : problem building
  //  Techniques   : 'glp_set_prob_name'
  //  Status       : complete
  //
  //  CAUTION: label uniqueness
  //
  //      When using the timestamp style, the generated part of
  //      the label is not currently unique due to the one second
  //      resolution.  The optional supplied part should assist
  //      uniqueness.  However it may be useful to redesign this
  //      method.
  //
  // ---------------------------------------------------------

  void
  SolverIf::setProblemLabel
  (const std::string tag)                    // problem name tag
  {
    // initial reporting
    s_logger->repx(logga::adhc, "entering member function, tag", tag);

    // update instance data
    d_probTag = tag;

    // add or overwrite meta information
    std::string sPrefix = ::PRO;             // set at beginning of this file
    std::string sSep    = ::SEP;             // set at beginning of this file
    std::string sLabel;                      // full label

#if 0 // 0 = count style, 1 = timestamp style

    // sleep one to ensure unique name (no longer a smart idea)
 // sleep(1);                                // refer <unistd.h>

    // produce sTimeStamp: 00000000z0000
    // NOTE: C rather that Boost time functions were chosen for portability
    const size_t MAX = 1024;                 // define buffer length
    const time_t now = time(0);              // seconds since 1970
    const tm* utc    = gmtime(&now);         // convert to tm struct in UTC time
    char timeStamp[MAX];

    strftime(timeStamp, MAX, "%H%M%S", utc);           // format time as string, short
 // strftime(timeStamp, MAX, "%Y%m%dz%H%M%S", utc);    // format time as string, long

    std::string sTimeStamp(timeStamp);       // convert to std::string

    // construct name
    if ( ! sPrefix.empty() ) sLabel += sPrefix;        // pro
    sLabel                          += sTimeStamp;     // 00000000z0000
    if ( ! tag.empty() )     sLabel += sSep + tag;     // -tag

#else

    // create padded count based on calls to this function
    std::ostringstream oss;
    oss << boost::format("%05d") % ++s_probLabelCount;
    const std::string sCount = oss.str();

    // construct name
    if ( ! sPrefix.empty() ) sLabel += sPrefix;        // pro
    sLabel                          += sCount;         // 00000
    if ( ! tag.empty() )     sLabel += sSep + tag;     // -tag

#endif // 0

    // load
    glp_set_prob_name(d_prob, sLabel.c_str());         // GLPK API

    // update problem status
    updateProbStatus();

    // report supplied tag
    std::string sRecoverLabel = "(not recovered)";
    sRecoverLabel             = getProblemLabel();     // call protects against GLPK NULL
    if ( tag.empty() )
      {
        s_logger->repx(logga::dbug, "supplied problem tag", "(empty)");
      }
    else
      {
        s_logger->repx(logga::dbug, "supplied problem tag", tag);
      }

    // completion reporting
    s_logger->repx(logga::adhc, "recovered problem tag",   getProblemTag());
    s_logger->repx(logga::dbug, "recovered problem label", getProblemLabel());

  } // function 'SolverIf::setProblemLabel'

  // ---------------------------------------------------------
  //  MEMBER FUNCTION : setObjectiveSense
  // ---------------------------------------------------------
  //  Description  : set objective sense, label objective function
  //  Role         : problem building
  //  Techniques   : 'glp_set_obj_dir'
  //  Status       : complete
  // ---------------------------------------------------------

  void
  SolverIf::setObjectiveSense
  (const ObjectiveSense objSense,            // objective sense
   const std::string     tag)                // objective function name tag
  {
    // initial reporting
    s_logger->repx(logga::adhc, "entering member function", "");

    // set private data
    d_objSense = objSense;

    // add meta information
    setObjectiveLabel(tag);

    // set objective sense
    switch ( d_objSense )
      {
      case maximize:
        glp_set_obj_dir(d_prob, GLP_MAX);
        s_logger->repx(logga::dbug, "about to call GLPK glp_set_obj_dir", GLP_MAX);
        break;
      case minimize:
        s_logger->repx(logga::dbug, "about to call GLPK glp_set_obj_dir", GLP_MIN);
        glp_set_obj_dir(d_prob, GLP_MIN);
        break;
      default:
        s_logger->repx(logga::warn, "unsupported objective sense", d_objSense);
        std::clog << "** coding error 01 in source file " << __FILE__
                  << ": unsupported objective sense" << std::endl;
        break;
      }

    // update problem status
    updateProbStatus();

    // report with recovered name and sense
    std::string sRecoverLabel = glp_get_obj_name(d_prob);
    const int sense           = glp_get_obj_dir(d_prob);
    std::string buf;
    switch ( sense )
      {
      case GLP_MIN: buf = "minimize";                                               break;
      case GLP_MAX: buf = "maximize";                                               break;
      default:
        s_logger->repx(logga::warn, "unsupported GLPK objective direction", sense);
        std::clog << "** coding error 02 in source file " << __FILE__
                  << ": unsupported GLP constant" << std::endl;
        break;
      }

    s_logger->repx(logga::dbug, "objective function label", sRecoverLabel);
    s_logger->repx(logga::dbug, "objective sense set",      buf);

  } // function 'SolverIf::setObjectiveSense'

  // ---------------------------------------------------------
  //  MEMBER FUNCTION : setObjectiveLabel
  // ---------------------------------------------------------
  //  Description  : label objective function
  //  Role         : problem building
  //  Techniques   : 'glp_set_obj_name', strips and reapplies standard label
  //  Status       : complete
  // ---------------------------------------------------------

  void
  SolverIf::setObjectiveLabel
  (const std::string tag)
  {
    // initial reporting
    s_logger->repx(logga::adhc, "entering member function", "");

    // create new string
    std::string buf = tag;

    // clean up 'buf'
    ::trimPrefixPlus(buf);

    // add meta information
    std::string sPrefix = ::OBJ;             // set at beginning of this file
    std::string sSep    = ::SEP;             // set at beginning of this file
    std::string sLabel;                      // full label
    if ( ! sPrefix.empty() ) sLabel += sPrefix;        // obj
    if ( ! buf.empty()     ) sLabel += sSep + buf;     // -buf
    glp_set_obj_name(d_prob, sLabel.c_str());          // GLPK API

  } // function 'SolverIf::setObjectiveLabel'

  // ---------------------------------------------------------
  //  MEMBER FUNCTION : loadRhs
  // ---------------------------------------------------------
  //  Description  : load RHS value and constraint sense, label row
  //  Role         : problem building
  //  Techniques   : 'glp_add_rows' and 'glp_set_row_bnds'
  //  Status       : complete
  //
  //  Design notes
  //
  //      The RHS can be zero and may be negative.  However a
  //      strictly negative value can readily generate
  //      infeasibilities and is thus flagged for further
  //      consideration.
  //
  //  CAUTION: constraint senses {R, N} not currently supported
  //
  //      Constraint senses {R, N} (ranged and free) are not
  //      supported here (although it would not be difficult to
  //      add the code, although that would require overloading
  //      'loadRhs').
  //
  // ---------------------------------------------------------

  void
  SolverIf::loadRhs
  (const int             row,                // row index
   const double          rhsValue,           // RHS value
   const ConstraintSense conSense,           // constraint sense as enum
   const std::string     tag)                // constraint equation name tag
  {
    // row number in string form for later reporting
    std::string sRow = indexToString(row);

    // check for negative rhsValue, this is advisory
    if ( rhsValue < 0.0 )
      {
        std::string sWarn = "negative row " + sRow + " RHS";
        s_logger->repx(logga::dbug, sWarn, rhsValue);
        incrementAlertCount();
      }

    // numerical zero processing
    const double rhsValueTrip = zeroTripCof(rhsValue);

    // add row or rows as required -- comment strongly if this
    // action is non-progressive, that is, will either skip rows
    // or backfill a previously loaded row
    int nRows    = glp_get_num_rows(d_prob); // GLPK API
    int deltaRow = row - nRows - 1;          // note minus one
    if ( deltaRow == 0 )                     // progressive addition
      {
        glp_add_rows(d_prob, 1);             // GLPK API
      }
    else if ( deltaRow > 0 )                 // skipping case
      {
        glp_add_rows(d_prob, deltaRow + 1);
        s_logger->repx(logga::info, "note row " + sRow + " will skip past", deltaRow);
      }
    else                                     // backfilling case
      {
        s_logger->repx(logga::info, "note row " + sRow + " will backfill by", deltaRow);
      }

    // comment strongly if this action will overwrite an existing row
    if ( glp_get_row_name(d_prob, row) != NULL )  // overwrite case
      {
        s_logger->repx(logga::info, "note row " + sRow + " will overwrite", "");
      }

    // add meta information
    std::string sPrefix = ::CON;             // set at beginning of this file
    std::string sSep    = ::SEP;             // set at beginning of this file
    std::string sLabel;                      // full label
    if ( ! sPrefix.empty() ) sLabel += sPrefix + sRow;      // con0
    if ( ! tag.empty()     ) sLabel += sSep + tag;          // -tag
    glp_set_row_name(d_prob, row, sLabel.c_str());          // GLPK API

    // process RHS value based on constraint sense
    //
    //  * intuitive form :                     eqn <= | = | >= rhs
    //  * Numerical Recipes terminology :      M1, M3, M2
    //  * IBM OSI terminology, adopted here :  L, E, G
    //  * old GLPK terminology :               LPX_UP, LPX_FX, LPX_LO
    //  * new GLPK terminology :               GLP_UP, GLP_FX, GLP_LO
    //
    // technically, the GLPK terms refer to the associated
    // +auxiliary+ variable, when the equations are in standard
    // form -- in practice, one can view these as applying
    // directly to the constraints in ordinary form (see earlier)
    //
    // the final arguments in the GLPK calls are 'lower bound'
    // and 'upper bound'

    switch ( conSense )
      {
        // eqn <= rhs, L, GLP_UP
      case L:
        glp_set_row_bnds(d_prob, row, GLP_UP, 0.0, rhsValueTrip);
        break;
        // eqn  = rhs, E, GLP_FX
      case E:
        glp_set_row_bnds(d_prob, row, GLP_FX, rhsValueTrip, rhsValueTrip);      // [1]
        break;
        // eqn >= rhs, G, GLP_LO
      case G:
        glp_set_row_bnds(d_prob, row, GLP_LO, rhsValueTrip, 0.0);               // [1]
        break;
      case R:
        s_logger->repx(logga::warn, "unsupported constraint sense", "R (ranged)");
        break;
      case N:
        s_logger->repx(logga::warn, "unsupported constraint sense", "N (free)");
        break;
      default:
        s_logger->repx(logga::warn, "unsupported constraint sense", conSense);
        std::clog << "** coding error 03 in source file " << __FILE__
                  << ": unsupported enum" << std::endl;
        break;

        // [1] upper bound not used
      }

    // update problem status
    updateProbStatus();

    // report with recovered name and value
    std::string sRecLabel = glp_get_row_name(d_prob, row);
    const double lb = glp_get_row_lb(d_prob, row);
    const double ub = glp_get_row_ub(d_prob, row);
    const int type  = glp_get_row_type(d_prob, row);

    // interpretation
    std::ostringstream buf;
    switch ( type )
      {
      case GLP_FR: buf << "" << " free";                                            break;
      case GLP_LO: buf << lb << " lower";                                           break;
      case GLP_UP: buf << ub << " upper";                                           break;
      case GLP_DB: buf << lb << " " << ub << " range";                              break;
      case GLP_FX: buf << lb << " fixed";                                           break;
      default:
        s_logger->repx(logga::warn, "unsupported GLPK row type", type);
        std::clog << "** coding error 04 in source file " << __FILE__
                  << ": unsupported GLP constant" << std::endl;
        break;
      }
    // often over-length so concat into just one string for
    // formatting consistency and thus readability
    const std::string msg = "load row " + sRow + " : " + sRecLabel + " " + buf.str();
    s_logger->repx(logga::dbug, msg, "");

  } // function 'SolverIf::loadRhs'

  // ---------------------------------------------------------
  //  MEMBER FUNCTION : loadObj
  // ---------------------------------------------------------
  //  Description  : load objective coefficient, label structural variable
  //  IMPORTANT    : may also add a standard non-negativity condition for that variable
  //  Role         : problem building
  //  Techniques   : 'glp_add_cols', 'glp_set_obj_coef', and 'glp_set_col_bnds'
  //  Status       : complete
  //
  //  Design notes
  //
  //      An objective coefficient may be zero or strictly
  //      negative.
  //
  //      A structural variable cannot be strictly negative under
  //      this implementation (see the caution below and check
  //      the conditional compilation).
  //
  //  CAUTION: standard non-negativity or free condition required
  //
  //      It is important to note that 'glp_add_cols' behaves as
  //      if the following API was also invoked:
  //
  //          glp_set_col_bnds(d_prob, col, GLP_FX, 0.0, 0.0)
  //
  //      Hence it is necessary to expressly call one of the
  //      following APIs (noting that unrequired bound values are
  //      ignored and can be happily set to zero):
  //
  //          glp_set_col_bnds(d_prob, col, GLP_LO, 0.0, 0.0)
  //          glp_set_col_bnds(d_prob, col, GLP_FR, 0.0, 0.0)
  //
  //      The choice of API is is controlled by conditional
  //      compilation in the code below.
  //
  // ---------------------------------------------------------

  void
  SolverIf::loadObj
  (const int         col,                    // col index (can be zero)
   const double      objValue,               // objective coefficient value
   const std::string tag)                    // objective function name tag
  {
    // numerical zero process
    const double objValueTrip = zeroTripCof(objValue);

    // check for "shift" and respond appropriately
    if ( col == 0 )
      {
        glp_set_obj_coef(d_prob, col, objValueTrip);
        s_logger->repx(logga::dbug, "objective \"shift\" set, value", objValueTrip);
        if ( ! tag.empty() )
          {
            s_logger->repx(logga::info, "tag string for \"shift\" ignored", tag);
          }
        return;
      }

    // create new string
    std::string buf = tag;

    // clean up 'buf'
    ::trimPrefixPlus(buf);

    // col number in string form for later reporting
    std::string sCol = indexToString(col);

#if 0 // 0 = suppress report, 1 = make report

    // check for zero-valued objective coefficient values and
    // issue a comment -- notwithstanding that GLPK allows
    // objective coefficients to be zero
    if ( objValue == 0.0 )                   // straight comparison, no EPS test
      {
        s_logger->repx(logga::adhc, "note col " + sCol + " zero-valued", "");
      }
    else if ( objValueTrip == 0.0 )
      {
        s_logger->repx(logga::adhc, "note col " + sCol + " zero-tripped", "");
      }

#endif // 0

    // add column or columns as required -- comment strongly if
    // this action is non-progressive, that is, will either skip
    // cols or backfill a previously loaded col
    int nCols    = glp_get_num_cols(d_prob); // GLPK API
    int deltaCol = col - nCols - 1;          // note minus one
    if ( deltaCol == 0 )                     // progressive addition
      {
        glp_add_cols(d_prob, 1);             // GLPK API
      }
    else if ( deltaCol > 0 )                 // skipping case
      {
        glp_add_cols(d_prob, deltaCol + 1);
        s_logger->repx(logga::info, "note col " + sCol + " will skip past", deltaCol);
      }
    else                                     // backfilling case
      {
#if 0 // 0 = suppress report, 1 = make report
        s_logger->repx(logga::info, "note col " + sCol + " will backfill by", deltaCol);
#endif // 0
      }

    // comment strongly if this action will overwrite an existing col
    if ( glp_get_col_name(d_prob, col) != NULL )  // overwrite case
      {
#if 0 // 0 = suppress report, 1 = make report
        s_logger->repx(logga::info, "note col " + sCol + " will overwrite", "");
#endif
      }

    // add meta information
    std::string sPrefix = ::VAR;             // set at beginning of this file
    std::string sSep    = ::SEP;             // set at beginning of this file
    std::string sLabel;                      // full label
    if ( ! sPrefix.empty() ) sLabel += sPrefix + sCol;      // var0
    if ( ! buf.empty()     ) sLabel += sSep + buf;          // -buf
    glp_set_col_name(d_prob, col, sLabel.c_str());          // GLPK API

    // note that these bounds should be integer-valued (such that
    // val == round(val) holds) to accomodate 'markVarInteger'
    utilSetDefaultBnds(col);

    // load objective coefficient
    glp_set_obj_coef(d_prob, col, objValueTrip);

    // update problem status
    updateProbStatus();

    // report with recovered name and value
    std::string sRecLabel = glp_get_col_name(d_prob, col);
    double      recValue  = glp_get_obj_coef(d_prob, col);
    std::ostringstream buf2;
    buf2 << recValue;                         // stream the 'double'
    const std::string msg = "load col " + sCol + " : " + sRecLabel + " " + buf2.str();
    s_logger->repx(logga::dbug, msg, "");

  } // function 'SolverIf::loadObj'

  // ---------------------------------------------------------
  //  MEMBER FUNCTION : incShift (increment "shift" term)
  // ---------------------------------------------------------
  //  Description  : incrementally load objective constant term
  //  Role         : problem building
  //  Techniques   : 'glp_get_obj_coef' and 'glp_set_obj_coef'
  //  Status       : complete
  //
  //  Design notes
  //
  //      This function is designed to stop multiple calls
  //      clobbering earlier information.
  //
  //      If you do want to overwrite the current value, use
  //      'loadCoef' and nominate col zero.
  //
  // ---------------------------------------------------------

  void
  SolverIf::incShift
  (const double constTermValue)              // objective constant term value increment
  {
    // initial reporting
    static int callCount = 0;                // intended for first step debugging
    s_logger->repx(logga::adhc, "entering member function, call cnt", ++callCount);

    // numerical zero processing
    const double constTermValueTrip = zeroTripCof(constTermValue);

    // CAUTION: column zero is correct in this case
    const double currentConstTermValue = glp_get_obj_coef(d_prob, 0);
    const double total = currentConstTermValue + constTermValueTrip;
    glp_set_obj_coef(d_prob, 0, total);

    // report
    std::ostringstream buf;
    buf << currentConstTermValue << " + " << constTermValueTrip << " = " << total;
    const std::string msg = "incr col 0 : " + buf.str();
    s_logger->repx(logga::dbug, msg, "");

  } // function 'SolverIf::incShift'

  // ---------------------------------------------------------
  //  MEMBER FUNCTION : loadCof
  // ---------------------------------------------------------
  //  Description  : load non-zero constraint matrix coefficient
  //  Role         : problem building
  //  Techniques   : sparse matrix vectors, 'glp_load_matrix'
  //  Status       : complete
  //
  //  Design notes
  //
  //      GLPK will accept and silently ignore constraint
  //      coefficients set to zero.  However, zero values are
  //      rejected in this code and are therefore never seen by
  //      GLPK.
  //
  //      The 'glp_load_matrix' call is made each time to keep
  //      the problem instance current.  This may be an expensive
  //      call but I suspect not.  If need be the load statement
  //      can be moved further down the formulation chain.
  //
  // ---------------------------------------------------------

  bool
  SolverIf::loadCof
  (const int    row,                         // row index
   const int    col,                         // col index
   const double coeffValue)                  // coefficient value
  {
    // declare function exit
    bool ret = false;                        // presume value not loaded

    // row and col numbers in string form for later reporting
    std::string sRow = indexToString(row);
    std::string sCol = indexToString(col);

    // numerical zero processing
    const double coeffValueTrip = zeroTripCof(coeffValue);

    // check for a zero-valued constraint coefficient --
    // zero-valued constraint coefficients are allowed in GLPK
    // but are not stored

    // active code

    if ( coeffValue == 0.0 )                           // straight comparison, no EPS test
      {
        std::string sWarn =  "note row " + sRow + " col " + sCol + " coeff zero-valued";
        s_logger->repx(logga::info, sWarn, coeffValue);
        incrementAlertCount();
        ret = false;
      }
    else if ( coeffValueTrip == 0.0 )                  // tripped numerical zero
      {
        std::string sWarn =  "note row " + sRow + " col " + sCol + " coeff zero-tripped";
        s_logger->repx(logga::info, sWarn, coeffValue);
        incrementAlertCount();
        ret = false;
      }
    else                                               // normal value
      {
        // insert coefficient into the sparse matrix vectors
        d_ia.push_back(row);
        d_ja.push_back(col);
        d_ar.push_back(coeffValueTrip);

        // load matrix (see design note above)
        const int coeffCount = d_ar.size() - s_glpkMargin;
        glp_load_matrix(d_prob, coeffCount, &d_ia[0], &d_ja[0], &d_ar[0]);  // void return

        // update problem status
        updateProbStatus();

        // update return code
        ret = true;

        // report with col and name information
        std::ostringstream buf;
        buf << std::showpos << coeffValueTrip;    // stream the 'double'
        const std::string msg  = "load cof " + sRow + " " + sCol + " : " + buf.str();
        s_logger->repx(logga::dbug, msg, "");
      }

    // finally report if exact or numerical zero, irrespective of whether loaded or not
    if ( coeffValue == 0.0 )
      {
        std::ostringstream put;
        put << reportCof(row, col, "exact zero", 4) << "\n";
        s_logger->putx(logga::dbug, put);
      }
    else if ( std::abs(coeffValue) <= d_zeroTol )
      {
        std::ostringstream put;
        put << reportCof(row, col, "numerical zero", 4) << "\n";
        s_logger->putx(logga::dbug, put);
      }

    // return the outcome
    return ret;

  } // function 'SolverIf::loadCof'

  // ---------------------------------------------------------
  //  MEMBER FUNCTION : setZeroTol
  // ---------------------------------------------------------
  //  Description  : update private data member 'd_zeroTol', exact zero by default
  //  Role         : client use, underlying values used internally by 'loadCof'
  //  Techniques   : some integrity checking
  //  Status       : complete
  // ---------------------------------------------------------

  double
  SolverIf::setZeroTol
  (const double numericalZero,
   const bool   resetCof,
   const bool   resetVal)
  {
    // reporting
    s_logger->repx(logga::adhc, "entering member function", numericalZero);

    // integrity checks
    if ( numericalZero < 0.0 )
      {
        s_logger->repx(logga::warn, "refusing to accept negative value", numericalZero);
        return d_zeroTol;
      }
    static const double trip = 1.0e-01;
    if ( numericalZero > trip )
      {
        s_logger->repx(logga::rankJumpy, "numerical zero > integrity trip", trip);
      }

    // set data members
    const double priorZeroTol = d_zeroTol;
    const bool   priorZeroCof = d_zeroCof;
    const bool   priorZeroVal = d_zeroVal;
    d_zeroTol                 = numericalZero;
    d_zeroCof                 = resetCof;
    d_zeroVal                 = resetVal;

    // report
    std::ostringstream put;
    put << std::boolalpha
        << "  numerical zero coefficient report"         << "\n"
        << "    current tolerance : " << d_zeroTol       << "\n"
        << "    current active    : " << d_zeroCof       << "\n"
        << "    current active    : " << d_zeroVal       << "\n"
        << "    prior tolerance   : " << priorZeroTol    << "\n"
        << "    prior active      : " << priorZeroCof    << "\n"
        << "    prior active      : " << priorZeroVal    << "\n";
    s_logger->repx(logga::dbug, "additional reporting follows", "");
    s_logger->putx(logga::dbug, put);

    // return prior tolerance
    return priorZeroTol;

  } // function 'SolverIf::setZeroTol'

  // ---------------------------------------------------------
  //  MEMBER FUNCTION : reviseBnds
  // ---------------------------------------------------------
  //  Description  : revise variable bounds
  //  Role         : problem building
  //  Techniques   : 'glp_set_col_bnds'
  //  Status       : complete
  // ---------------------------------------------------------

  void
  SolverIf::reviseBnds
  (const int    col,                         // col index
   const double lowerValue,                  // lower bound
   const double upperValue)                  // upper bound
  {
    // col number in string form for later reporting
    std::string sCol = indexToString(col);

    // check col is within range and already loaded
    int nCols = glp_get_num_cols(d_prob);
    if ( col <= 0 || col > nCols )           // out of range
      {
        s_logger->repx(logga::warn, "note col " + sCol + " illegal and ignored", nCols);
        return;                              // otherwise will signal SIGABRT and exit 134
      }
    else if ( glp_get_col_name(d_prob, col) == NULL )  // not already loaded
      {
        s_logger->repx(logga::info, "note col " + sCol + " not loaded", "");
        return;                              // otherwise will signal SIGABRT and exit 134
      }

    // revise bounds
    const double inf = getInf();             // IEEE 754 (IEC 60559) infinity

    if ( lowerValue > upperValue )           // nonsensical
      {
        s_logger->repx(logga::info, "range setting faulty", "");
      }
    else if ( lowerValue == upperValue )     // equal
      {
        glp_set_col_bnds(d_prob, col, GLP_FX, lowerValue, upperValue);
      }
    else if ( lowerValue == -inf )
      {
        if ( upperValue == +inf )
          {
            glp_set_col_bnds(d_prob, col, GLP_FR, 0.0, 0.0);
          }
        else                                 // upper value finite
          {
            glp_set_col_bnds(d_prob, col, GLP_UP, 0.0, upperValue);
          }
      }
    else                                     // lower value finite
      {
        if ( upperValue == +inf )
          {
            glp_set_col_bnds(d_prob, col, GLP_LO, lowerValue, 0.0);
          }
        else                                 // upper value finite too
          {
            glp_set_col_bnds(d_prob, col, GLP_DB, lowerValue, upperValue);
          }
      }

    // report with recovered name and value

    //  Makhorin states "the routine 'glp_get_col_ub' returns the
    //  upper bound of 'j'-th column, that is the upper bound of
    //  corresponding structural variable -- however, if the
    //  column has no upper bound, the routine returns
    //  '+DBL_MAX'" and similar for 'glp_get_col_lb'

    const std::string sRecLabel = glp_get_col_name(d_prob, col);
    double            recLower  = glp_get_col_lb(d_prob, col);
    double            recUpper  = glp_get_col_ub(d_prob, col);

    // transform to IEEE 754 (IEC 60559) -/+inf (rather than -/+1.79769e+308)
    if ( recLower == -DBL_MAX ) recLower = -inf;
    if ( recUpper == +DBL_MAX ) recUpper = +inf;

    std::ostringstream buf1;
    std::ostringstream buf2;
    buf1 << std::showpos << recLower;        // stream the 'double'
    buf2 << std::showpos << recUpper;        // stream the 'double'
    const std::string msg
      = "load bnd " + sCol + " : " + sRecLabel + " " + buf1.str() + " " + buf2.str();
    s_logger->repx(logga::dbug, msg, "");

  } // function 'SolverIf::reviseBnds'

  // ---------------------------------------------------------
  //  MEMBER FUNCTION : resetDefaultBnds
  // ---------------------------------------------------------
  //  Description  : reset to pre-defined bounds
  //  Role         : problem building
  //  Techniques   : utility function 'utilSetDefaultBnds'
  //  Status       : complete
  // ---------------------------------------------------------

  void
  SolverIf::resetDefaultBnds
  (const int col)
  {
    utilSetDefaultBnds(col);

  } // function 'SolverIf::resetDefaultBnds'

  // ---------------------------------------------------------
  //  MEMBER FUNCTION : markVarInteger
  // ---------------------------------------------------------
  //  Description  : mark structural variable as integer-valued, revises label
  //  Role         : problem building
  //  Techniques   : 'glp_set_col_kind'
  //  Status       : complete
  //
  //  Design notes
  //
  //      'GLP_IV' variables are strictly integer under GLPK,
  //      meaning they cannot additionally be binary.
  //
  //  Background
  //
  //      Robbie: MIP: Note on numerical thresholds.
  //
  //      This code indirectly uses the 'floor' test from
  //      <cmath>.  This compares (on my system, IA32) an epsilon
  //      value of 2.22045e-16.
  //
  //      It might be necessary to rework this code using the
  //      same threshold that GLPK uses.  This test will be as
  //      strict or more strict that GLPK, so acceptance here
  //      will confirm the problem is okay.  However, rejection
  //      may also be okay.
  //
  //      Andrew Makhorin, GLPK maintainer, later writes:
  //
  //      "Bounds of variables marked as integer must be exact
  //      integer numbers with no tolerance, i.e. the condition
  //      bnd == floor(bnd) must be satisfied, where bnd is a
  //      lower/upper bound.
  //
  //      However, to check if a node solution is integer
  //      feasible, [an]other criterion is used, namely, the
  //      current value of a variable is considered as integral,
  //      if it differs from the nearest integer at most by 1e-6
  //      (this is [the] *absolute* tolerance used by default)."
  //
  //      See also Stephens etal (2006, section 3.4, pp 129-131)
  //      on comparing floating-point numbers with bounded
  //      accuracy.
  //
  // ---------------------------------------------------------

  void
  SolverIf::markVarInteger
  (const int col)                            // col index
  {
    // initial reporting
    s_logger->repx(logga::adhc, "entering member function", "");

    // col number in string form for later reporting
    std::string sCol = indexToString(col);

    // check col is within range and already loaded
    int nCols = glp_get_num_cols(d_prob);
    if ( col <= 0 || col > nCols )           // out of range
      {
        s_logger->repx(logga::warn, "note col " + sCol + " illegal and ignored", nCols);
        return;                              // otherwise will signal SIGABRT and exit 134
      }
    else if ( glp_get_col_name(d_prob, col) == NULL )  // not already loaded
      {
        s_logger->repx(logga::info, "note col " + sCol + " not loaded", "");
        return;                              // otherwise will signal SIGABRT and exit 134
      }

    // overwrite meta information as appropriate
    std::string sColName = glp_get_col_name(d_prob, col);   // GLPK API
    if ( sColName.substr(0, ::len) == ::VAR ||              // first ::len chars
         sColName.substr(0, ::len) == ::VAB )
      {
        sColName.replace(0, ::len, ::VAZ);                  // replacement
        glp_set_col_name(d_prob, col, sColName.c_str());    // GLPK API
      }
    else                                     // repeat call
      {
        s_logger->repx(logga::info, "note col " + sCol + " is repeat call", "");
      }

    // check bounds are indeed integer, also a GLPK requirement
    double ub = glp_get_col_ub(d_prob, col);      // upper bound on variable
    if ( ! ::isIntegerValued(ub) )                // test for integer bound
      {
        const std::string msg = "col " + indexToString(col) + " upper bound not integer";
        s_logger->repx(logga::dbug, msg, ub);
      }
    double lb = glp_get_col_lb(d_prob, col);      // lower bound on variable
    if ( ! ::isIntegerValued(lb) )                // test for integer bound
      {
        const std::string msg = "col " + indexToString(col) + " lower bound not integer";
        s_logger->repx(logga::dbug, msg, lb);
      }

    // mark column as integer
    glp_set_col_kind(d_prob, col, GLP_IV);   // IV is integer variable

    // update problem klass
    updateProbKlass();                       // may have been 'prob_linear'

    // report
    std::string sLog = "mark col " + sCol + " : integer";
    s_logger->repx(logga::dbug, sLog, "");

  } // member function 'SolverIf::markVarInteger'

  // ---------------------------------------------------------
  //  MEMBER FUNCTION : markVarBinary
  // ---------------------------------------------------------
  //  Description  : mark structural variable as 0-1-valued, revises label
  //  Role         : problem building
  //  Techniques   : 'glp_set_col_kind'
  //  Status       : complete
  // ---------------------------------------------------------

  void
  SolverIf::markVarBinary
  (const int col)                            // col index
  {
    // initial reporting
    s_logger->repx(logga::adhc, "entering member function", "");

    // col number in string form for later reporting
    std::string sCol = indexToString(col);

    // check col is within range and already loaded
    int nCols = glp_get_num_cols(d_prob);
    if ( col <= 0 || col > nCols )           // out of range
      {
        s_logger->repx(logga::warn, "note col " + sCol + " illegal and ignored", nCols);
        return;                              // otherwise will signal SIGABRT and exit 134
      }
    else if ( glp_get_col_name(d_prob, col) == NULL )  // not already loaded
      {
        s_logger->repx(logga::info, "note col " + sCol + " not loaded", "");
        return;                              // otherwise will signal SIGABRT and exit 134
      }

    // overwrite meta information as appropriate
    std::string sColName = glp_get_col_name(d_prob, col);   // GLPK API
    if ( sColName.substr(0, ::len) == ::VAR ||              // first ::len chars
         sColName.substr(0, ::len) == ::VAZ )
      {
        sColName.replace(0, ::len, ::VAB);                  // replacement
        glp_set_col_name(d_prob, col, sColName.c_str());    // GLPK API
      }
    else                                     // repeat call
      {
        s_logger->repx(logga::info, "note col " + sCol + " is repeat call", "");
      }

    // mark column as integer
    glp_set_col_kind(d_prob, col, GLP_BV);   // BV is binary variable

    // update problem klass
    updateProbKlass();                       // may have been 'prob_linear'

    // report
    std::string sLog = "mark col " + sCol + " : binary";
    s_logger->repx(logga::dbug, sLog, "");

  } // member function 'SolverIf::markVarBinary'

  // ---------------------------------------------------------
  //  MEMBER FUNCTION : markVarContinuous
  // ---------------------------------------------------------
  //  Description  : mark structural variable as real-valued, revises label
  //  Role         : problem building
  //  Techniques   : 'glp_set_col_kind'
  //  Status       : complete
  // ---------------------------------------------------------

  void
  SolverIf::markVarContinuous
  (const int col)                            // col index
  {
    // initial reporting
    s_logger->repx(logga::adhc, "entering member function", "");

    // col number in string form for later reporting
    std::string sCol = indexToString(col);

    // check col is within range and already loaded
    int nCols = glp_get_num_cols(d_prob);
    if ( col <= 0 || col > nCols )           // out of range
      {
        s_logger->repx(logga::warn, "note col " + sCol + " illegal and ignored", nCols);
        return;                              // otherwise will signal SIGABRT and exit 134
      }
    else if ( glp_get_col_name(d_prob, col) == NULL )  // not already loaded
      {
        s_logger->repx(logga::info, "note col " + sCol + " not loaded", "");
        return;                              // otherwise will signal SIGABRT and exit 134
      }

    // overwrite meta information as appropriate
    std::string sColName = glp_get_col_name(d_prob, col);   // GLPK API
    if ( sColName.substr(0, ::len) == ::VAZ ||              // first ::len chars
         sColName.substr(0, ::len) == ::VAB )
      {
        sColName.replace(0, ::len, ::VAR);                  // replacement
        glp_set_col_name(d_prob, col, sColName.c_str());
      }
    else                                     // repeat call
      {
        s_logger->repx(logga::info, "note col " + sCol + " is repeat call", "");
      }

    // mark column as integer
    glp_set_col_kind(d_prob, col, GLP_CV);   // CV is continuous variable

    // reset bounds, otherwise these remain at [0,1], which seems
    // a bit counter-intuitive
    utilSetDefaultBnds(col);

    // update problem klass
    updateProbKlass();                       // may go back to 'prob_linear'

    // report
    std::string sLog = "mark col " + sCol + " : continuous";
    s_logger->repx(logga::dbug, sLog, "");

  } // member function 'SolverIf::markVarContinuous'

  // ---------------------------------------------------------
  //  MEMBER FUNCTION : reviseObj
  // ---------------------------------------------------------
  //  Description  : reload objective value
  //  Role         : problem building
  //  Techniques   : 'glp_set_obj_coef'
  //  Status       : complete
  //
  //  Design notes
  //
  //      The column tag, column kind, and column bounds remain
  //      as before.  No additional checks are made.
  //
  // ---------------------------------------------------------

  double
  SolverIf::reviseObj
  (const int    col,                         // col index (can be zero)
   const double objValue)                    // objective coefficient value
  {
    // initial reporting
    s_logger->repx(logga::adhc, "entering member function", "");

    // numerical zero processing
    const double objValueTrip = zeroTripCof(objValue);

    // active code
    const double prior = glp_get_obj_coef(d_prob, col);
    glp_set_obj_coef(d_prob, col, objValueTrip);
    return prior;

  } // member function 'SolverIf::reviseObj'

  // ---------------------------------------------------------
  //  MEMBER FUNCTION : resetGlpkProb
  // ---------------------------------------------------------
  //  Description  : empty the GLPK problem instance of external data
  //  Role         : problem building
  //  Techniques   : remove all rows and cols
  //  Status       : complete but not tested
  //
  //  Design notes
  //
  //      The old problem label remains.
  //
  //      This method does not retain internal information and
  //      thereby does not allow warm starting.
  //
  //  CAUTION: unexplained segfault bug
  //
  //      Under some circumstances, the first attempt to use
  //      'd_prob' in this function, even if just in comparison
  //      to NULL, causes a segfault.  I do not understand how
  //      this would happen, as 'd_prob' is set to NULL in the
  //      constructor member initialization list and should
  //      always be valid in straight (non-dereferenced) form.
  //
  //      In particular, 'submodel.16.guard.xem' as at commit
  //      r4554 will trigger a segfault.
  //
  //      As of commit r6570, this problem seems to have resolved
  //      itself some time ago.
  //
  //      As of commit r7008 and the buggy 'poc.002'
  //      proof-of-concept modelette, this problem has returned.
  //      After quite a lot of testing and contemplation , it was
  //      decided that the segfault cannot be easily avoided.  It
  //      appears to only arise from faulty models which had
  //      previously printed 'logga::warn' loggings indicating
  //      bad structure.
  //
  //      For completeness, 'd_prob' was reassigned the illegal
  //      address '0x98' somehow.  This occurred under both the
  //      -ggdb and -O3 compiler options.
  //
  // ---------------------------------------------------------

  void
  SolverIf::resetGlpkProb()
  {
    // initial reporting
    s_logger->repx(logga::dbug, "entering member function", "");

#if 0 // 0 = ignore, 1 = extended testing
    // test code
    STD_OSTREAM << std::flush;               // 'STD_OSTREAM' macro defined in this file
    STD_OSTREAM << "  ** d_prob = ";
    STD_OSTREAM << std::flush;
    STD_OSTREAM << d_prob;                   // CAUTION: fails here if going to fail
    STD_OSTREAM << "\n";
    STD_OSTREAM << std::flush;
#endif // 0

    // defensive programming (see caution given above)
    if ( d_prob == NULL )
      {
        // should never reach here
        s_logger->repx(logga::warn, "d_prob is NULL", d_prob);
      }
    else
      {
        // additional reporting as appropriate
        // YEEK 25 CODE (set by '--yeek')
        if ( xeona::yeek == 25 || xeona::yeek == 1 || xeona::yeek == 2 )
          {
            s_logger->repx(logga::dbug, "d_prob is valid", d_prob);
            s_logger->repx(logga::dbug, "will report on problem", "");
            std::ostringstream put;
            put << reportGlpkProblem();
            s_logger->putx(logga::dbug, put);
          }
      }

    const int nRows = glp_get_num_rows(d_prob);   // GLPK API
    if ( nRows > 0 )                              // protection against no rows
      {
        std::vector<int> vRows = ::seq(0, nRows); // local function, the zero is necessary
        glp_del_rows(d_prob, nRows, &vRows[0]);   // GLPK API
      }

    const int nCols = glp_get_num_cols(d_prob);
    if ( nCols > 0 )                              // protection against no cols
      {
        std::vector<int> vCols = ::seq(0, nCols);
        glp_del_cols(d_prob, nCols, &vCols[0]);
      }

    // reset data
    utilResetCoeffVectors();                 // reset the three sparse matrix vectors
    utilResetObjectData();                   // reset the various data members

    // report outcome
    int rows = glp_get_num_rows(d_prob);
    int cols = glp_get_num_cols(d_prob);
    std::string info = indexToString(rows) + " x " + indexToString(cols);
    s_logger->repx(logga::info, "problem reset, rows x cols", info);

    // completion reporting (useful for debugging)
    s_logger->repx(logga::adhc, "leaving member function", "");

  } // function 'SolverIf::resetGlpkProb'

  // ---------------------------------------------------------
  //  MEMBER FUNCTION : recreateProblem
  // ---------------------------------------------------------
  //  Description  : indirectly delete and create GLPK problem instance
  //  Role         : problem building, probably limited to testing
  //  Techniques   : 'utilDeleteGlpkProb' and 'utilCreateGlpkProb'
  //  Status       : complete
  //
  //  Design notes
  //
  //      If a new tag is supplied, it will be used.  Otherwise
  //      the old label will be recycled.
  //
  // ---------------------------------------------------------

  void
  SolverIf::recreateProblem                  // delete/recreate problem instance
  (const std::string tag)                    // prior label recycled if tag is empty
  {
    // initial reporting
    s_logger->repx(logga::dbug, "entering member function", "");

    // delete GLPK problem instance and also recover label
    const std::string label = utilDeleteGlpkProb();

    // reset object data
    utilResetCoeffVectors();
    utilResetObjectData();                   // reset the various data members

    // recreate GLPK problem instance
    std::string buf        = tag;
    if ( buf.empty() ) buf = label;        // replace with recovered label if need be
    utilCreateGlpkProb(buf);

    s_logger->repx(logga::dbug, "problem recreated", "");

  } // function 'SolverIf::recreateProblem'

  // METHODS INTENDED FOR TESTING

  // ---------------------------------------------------------
  //  MEMBER FUNCTION : convertToLP
  // ---------------------------------------------------------
  //  Description  : downgrade to LP if currently MIP
  //  Role         : testing
  //  Status       : disabled, underlying GLPK API has been removed
  // ---------------------------------------------------------

  void
  SolverIf::convertToLP()
  {
    // initial reporting
    s_logger->repx(logga::adhc, "entering member function", "");

    s_logger->repx(logga::warn, "call not implemented", "");
  }

  // ---------------------------------------------------------
  //  MEMBER FUNCTION : toggleObjective
  // ---------------------------------------------------------
  //  Description  : multiply current objective function by minus one
  //  Role         : convert minimize problem to maximize (and vice-versa)
  //  Comment      : provided as a work-around to the GLPK 4.25 directionality bug
  //  Status       : complete
  // ---------------------------------------------------------

  void
  SolverIf::toggleObjective()
  {
    // initial reporting
    s_logger->repx(logga::dbug, "entering member function", "");

    const int nCols = glp_get_num_cols(d_prob);
    double cof   = 0;
    int    j     = 0;                             // col counter
    int    count = 0;                             // number of toggles performed
    for ( j = 0; j <= nCols; ++j )                // j = 0 is correct, includes 'shift'
      {
        cof = glp_get_obj_coef(d_prob, j);        // recover coeffient
        if ( cof != 0.0 )                         // to prevent -0's being displayed
          {
            glp_set_obj_coef(d_prob, j, -cof);    // reload
            ++count;
          }
      }
    s_logger->repx(logga::warn, "objective toggled, non-zeros", count);
  }

  // ---------------------------------------------------------
  //  MEMBER FUNCTION : obtainGlpProbPtr
  // ---------------------------------------------------------
  //  Description  : provides direct access to 'd_prob'
  //  Role         : strictly for unit testing
  //  Techniques   : returns a 'glp_prob*'
  //  Status       : complete
  //
  //  CAUTION: dangling pointer possible
  //
  //      Pointer will become dangerous when the 'SolverIf' host
  //      is destructed.  Clients should therefore ensure the
  //      host exists before passing the pointer to a GLPK
  //      function!
  //
  // ---------------------------------------------------------

  glp_prob*
  SolverIf::obtainGlpProbPtr()
  {
#ifndef _XUTEST                              // warn as not intended for application usage
    s_logger->repx(logga::kill, "for unit testing only", "");
#endif
    if ( d_prob == NULL )                    // defensive programming
      {
        s_logger->repx(logga::warn, "returning a NULL pointer", d_prob);
      }
    return d_prob;
  }

  // PROBLEM INFORMATION : give the information as it stands,
  // calls can made before or during the problem build or before
  // or after the solver call

  // ---------------------------------------------------------
  //  MEMBER FUNCTION : reportGlpkProblem
  // ---------------------------------------------------------
  //  Description  : wrapper to 'utilReportOnProb'
  //  Role         : problem information
  //  Status       : complete
  // ---------------------------------------------------------

  std::string
  SolverIf::reportGlpkProblem()
  {
    // initial reporting
    s_logger->repx(logga::adhc, "entering member function", "");

    return utilReportOnProb();
  }

  // ---------------------------------------------------------
  //  MEMBER FUNCTION : getProblemLabel
  //  MEMBER FUNCTION : getProblemKlass
  //  MEMBER FUNCTION : getObjectiveSense
  //  MEMBER FUNCTION : getObjectiveSenseStr
  //  MEMBER FUNCTION : getObjectiveLabel
  //  MEMBER FUNCTION : getObjectiveTag
  //  MEMBER FUNCTION : getVarLabel
  //  MEMBER FUNCTION : getVarTag
  //  MEMBER FUNCTION : getVarCount
  //  MEMBER FUNCTION : getConLabel
  //  MEMBER FUNCTION : getConTag
  //  MEMBER FUNCTION : getConCount
  //  MEMBER FUNCTION : getOrigCof
  //  MEMBER FUNCTION : getProbCof
  //  MEMBER FUNCTION : getNonZeroCoeffCount
  // ---------------------------------------------------------
  //  Description  : get problem related information
  //  Role         : problem information
  //  Techniques   : various
  // ---------------------------------------------------------

  ProblemKlass
  SolverIf::getProblemKlass() const
  {
    return d_probKlass;                      // 'setObjectiveSense' may not yet be called
  }

  std::string
  SolverIf::getProblemLabel() const
  {
    const char* cLabel = glp_get_prob_name(d_prob);
    if ( cLabel == NULL )                    // protect against "not set" return
      {
        return "";
      }
    else
      {
        const std::string label(cLabel);
        return label;
      }
  }

  ObjectiveSense
  SolverIf::getObjectiveSense() const
  {
    if ( d_objSense == sense_not_specified ) // 'setObjectiveSense' not yet called
      {
        s_logger->repx(logga::info, "objective sense not specified", d_objSense);
      }
    return d_objSense;
  }

  std::string
  SolverIf::getObjectiveSenseStr() const     // interpreted
  {
    switch ( d_objSense )
      {
      case sense_not_specified: return "sense not specified";
      case minimize:            return "minimize";
      case maximize:            return "maximize";
      default:
        std::clog << "** coding error 05 in source file " << __FILE__ << std::endl;
        return "(coding error)";
      }
  }

  std::string                                // with "obj-" prefix
  SolverIf::getObjectiveLabel() const
  {
    std::string objlabel;
    const char* cname = glp_get_obj_name(d_prob);
    if ( cname  == NULL ) objlabel = "";      // NULL is normally zero
    else                  objlabel = std::string(cname);
    return objlabel;
  }

  std::string                                // without "obj-" prefix
  SolverIf::getObjectiveTag() const          // tag for use in future 'SolverIf' calls
  {
    std::string objtag = getObjectiveLabel();
    const std::string prefix = ::OBJ + "-";  // '::OBJ' set in this file, probably "obj"
    const int         prelen = prefix.length();   // same as: ::len + 1
    if ( objtag.substr(0, prelen) == prefix ) objtag.erase(0, prelen);
    return objtag;
  }

  ColumnKind
  SolverIf::getVarKind
  (const int col) const
  {
    // CAUTION: 'glp_get_col_type' is altogether different

    const int kind = glp_get_col_kind(d_prob, col);
    switch ( kind )
      {
      case GLP_CV: return col_continuous;
      case GLP_IV: return col_integer;
      case GLP_BV: return col_binary;
      default:
        s_logger->repx(logga::warn, "unsupported GLPK col kind", kind);
        return col_other;
      }
  }

  std::string                                // with "va[rzb]-" (or similar) prefix
  SolverIf::getVarLabel
  (const int col) const
  {
    // protect against a "shift" term call (ideally such a call
    // should not be made)
    if ( col <= 0)
      {
        s_logger->repx(logga::xtra, "protecting GLPK glp_get_col_name", col);
        return "";
      }

    std::string varlabel;
    const char* cname = glp_get_col_name(d_prob, col);
    if ( cname  == NULL ) varlabel = "";      // NULL is normally zero
    else                  varlabel = std::string(cname);
    return varlabel;
  }

  std::string                                // without "va[rzb]-" (or similar) prefix
  SolverIf::getVarTag
  (const int col) const
  {
    std::string vartag     = getVarLabel(col);
    std::string prefixes[] = { ::VAR, ::VAZ, ::VAB };  // probably { "var", "vaz", "vab" }
    BOOST_FOREACH( std::string p, prefixes )
      {
        const std::string prefix = p + "-";
        const int         prelen = prefix.length();
        if ( vartag.substr(0, prelen) == prefix )
          {
            vartag.erase(0, prelen);
            break;
          }
      }
    return vartag;
  }

  int
  SolverIf::getVarCount() const
  {
    return glp_get_num_cols(d_prob);         // GLPK API
  }

  std::string
  SolverIf::getConLabel
  (const int row) const
  {
    std::string conlabel;
    const char* cname = glp_get_row_name(d_prob, row);
    if ( cname  == NULL ) conlabel = "";      // NULL is normally zero
    else                  conlabel = std::string(cname);
    return conlabel;
  }

  std::string
  SolverIf::getConTag
  (const int row) const
  {
    std::string contag     = getConLabel(row);
    std::string prefixes[] = { ::CON  };  // probably { "con" }
    BOOST_FOREACH( std::string p, prefixes )
      {
        const std::string prefix = p + "-";
        const int         prelen = prefix.length();
        if ( contag.substr(0, prelen) == prefix )
          {
            contag.erase(0, prelen);
            break;
          }
      }
    return contag;
  }

  int
  SolverIf::getConCount() const
  {
    return glp_get_num_rows(d_prob);         // GLPK API
  }

  double
  SolverIf::getOrigCof
  (const int row,
   const int col) const
  {
    // loop code required because there is no 'glp_get_mat_coef'
    // API routine, rather the sparse matrix vectors are interrogated
    //
    // a zero coefficient may be submitted but never stored by GLPK

    const int len = d_ia.size();
    for ( int k = 0;
          k < len;                           // 'k' should never be out-of-range
          ++k)
      {
        const int i = d_ia.at(k);
        const int j = d_ja.at(k);
        if ( i == row && j == col )
          {
            return d_ar.at(k);
          }
      }
    return 0.0;
  }

  double
  SolverIf::getProbCof
  (const int row,
   const int col) const
  {
#if 0 // 0 = GLPK 4.45 implemention, 1 = use the new proposed GLPK API

    return glp_get_aij(d_prob, row, col);

#else
    // CAUTION: this version is expensive when used iteratively

    // declare and size two STL vectors for row index and value data
    const int cols  = glp_get_num_cols(d_prob);
    const int vsize = cols + 1;                   // upper bound on vector size [1]
    std::vector<int>    inds;                     // column index map
    std::vector<double> vals;                     // respective values
    inds.resize(vsize);
    vals.resize(vsize);

    // [1] plus one because entry zero is not used by GLPK

    // recover the required row
    const int nzCols = glp_get_mat_row(d_prob,
                                       row,       // sought row
                                       &inds[0],  // CAUTION: not "inds.begin()"
                                       &vals[0]);

    // defensive programming, should never be here
    if ( nzCols  > cols )
      {
        std::ostringstream oss;
        oss << nzCols << " : " << cols;
        s_logger->repx(logga::warn, "overfilled vectors, got : upper bnd", oss.str());
      }

    // early return if no nonzero entries
    if ( inds.empty() )
      {
        return 0.0;
      }

    // else attempt to locate
    std::vector<int>::const_iterator pos;
    pos = std::find (inds.begin(),
                     inds.end(),
                     col);                   // sought col
    if ( pos == inds.end() )                 // no non-zero entry
      {
        return 0.0;
      }

    // cross-reference and return
    const std::vector<int>::size_type index = pos - inds.begin();
    const double                      value = vals.at(index);
    return value;

#endif // 0
  }

  int
  SolverIf::getNonZeroCoeffCount() const
  {
    // 'glp_get_num_nz' happily returns zero if the sparse matrix
    // vectors (smv) have yet to be loaded (alternatively the
    // sparse matrix vectors may have been loaded but empty)

    int smv = d_ar.size() - s_glpkMargin;
    int glp = glp_get_num_nz(d_prob);        // GLPK API, non-zero coefficients

    if ( glp == 0 )                          // sparse matrix vectors
      {
        return smv;
      }
    else
      {
        return glp;
      }
  }

  // ---------------------------------------------------------
  //  MEMBER FUNCTION : isLP
  // ---------------------------------------------------------
  //  Description  : reports whether the problem is LP or not
  //  Role         :
  //  Techniques   : various
  //  Status       : complete
  // ---------------------------------------------------------

  bool
  SolverIf::isLP() const              // all variables are continuous
  {
    switch ( d_probKlass )
      {
      case prob_linear:
        return true;
      default:
        return false;
      }
  }

  // ---------------------------------------------------------
  //  MEMBER FUNCTION : getLowerBnd
  // ---------------------------------------------------------

  double
  SolverIf::getLowerBnd
  (const int col) const
  {
    // initial reporting
    s_logger->repx(logga::adhc, "entering member function", "");

    // active code -- and, if necessary, transform to IEEE 754
    // (IEC 60559) -infinity (rather than -1.79769e+308)
    double lower = glp_get_col_lb(d_prob, col);
    if ( lower == -DBL_MAX )                 // preprocessor macro, refer <cfloat>
      {
        lower = -getInf();
      }
    return lower;
  }

  // ---------------------------------------------------------
  //  MEMBER FUNCTION : getUpperBnd
  // ---------------------------------------------------------

  double
  SolverIf::getUpperBnd
  (const int col) const
  {
    // initial reporting
    s_logger->repx(logga::adhc, "entering member function", "");

    // active code -- and, if necessary, transform to IEEE 754
    // (IEC 60559) +infinity (rather than +1.79769e+308)
    double upper = glp_get_col_ub(d_prob, col);
    if ( upper == +DBL_MAX )                 // preprocessor macro, refer <cfloat>
      {
        upper = +getInf();
      }
    return upper;
  }

  // ---------------------------------------------------------
  //  MEMBER FUNCTION : reportCof
  // ---------------------------------------------------------
  //  Design notes
  //
  //      Setting the width or precision to an asterisk (*) is
  //      NOT currently (as of 1.48.0) supported by Boost.Format.
  //
  // ---------------------------------------------------------

  std::string                                // no trailing newline
  SolverIf::reportCof
  (const int          row,
   const int          col,
   const std::string& comment,               // note default value
   const int          indent) const          // note default value
  {
    // create padding
    const std::string pad(indent, ' ');

    // integrity checks
    if ( row > getConCount() || col > getVarCount() )
      {
        // out-of-range error
        std::ostringstream oss;
        oss << pad << boost::format("row %02d") % row
            << "  x "
            << boost::format("col %02d") % col
            << " out of range"                                                    << "\n";
        if ( ! comment.empty() ) oss << pad << "comment : " << comment            << "\n";
        return oss.str();
      }

    // report
    const std::string rowLabel = getConLabel(row);
    const std::string colLabel = getVarLabel(col);
    const double      cof      = getOrigCof(row, col);  // from sparse matrix vectors

#if 0 // 0 = one-line report, 1 = four-line report
    // four-line report
    std::ostringstream oss;
    oss << pad << "row     : " << rowLabel                                  << "\n"
        << pad << "col     : " << colLabel                                  << "\n";
    oss << pad << "value   : ";
    if      ( cof == 0.0 )           oss << boost::format("%.1f")  % cof    << "\n";
    else if ( std::abs(cof) <= 0.1 ) oss << boost::format("%.20f") % cof    << "\n";
    else                             oss << boost::format("%g")    % cof    << "\n";
    if ( ! comment.empty() )         oss << pad << "comment : " << comment  << "\n";
    return oss.str();
#else
    // one-line report
    std::ostringstream oss1;                 // coefficient
    if      ( cof == 0.0 )           oss1 << boost::format("%.1f")  % cof;
# if 0
    else if ( std::abs(cof) <= 0.1 ) oss1 << boost::format("%.15f") % cof;
# endif // 0
    else                             oss1 << boost::format("%g")    % cof;
    std::ostringstream oss2;                 // entire row
    oss2 << pad
         << boost::format("%-54s") % rowLabel
         << " "
         << boost::format("%-54s") % colLabel
         << " "
         << boost::format("%+19s") % oss1.str();  // precision + "0." + two spaces
    if ( ! comment.empty() ) oss2 << "   " << comment;
    // oss2 << "\n";
    return oss2.str();
#endif // 0
  }

  // ---------------------------------------------------------
  //  MEMBER FUNCTION : getAbsCofs
  // ---------------------------------------------------------
  //  Description  : return multimap with coefficient as key and location as value
  //  Role         : support role
  //  Techniques   : 'std::multimap' which is naturally sorted on the keys
  //  Status       : complete
  //
  //  Design notes
  //
  //      Later on, this function could be modified to simply
  //      iterate over the matrix using the proposed GLPK API
  //      'glp_get_aij'.
  //
  // ---------------------------------------------------------

  std::multimap<double, SolverIf::ij_type>   // absolute value, location
  SolverIf::getAbsCofs() const
  {
    // initial reporting
    s_logger->repx(logga::adhc, "entering member function", "");

    // determine the constraint matrix size
    const int cols = glp_get_num_cols(d_prob);
    const int rows = glp_get_num_rows(d_prob);

    // declare a multimap to store the sorted information
    std::multimap<double, ij_type> cofmap;

    // declare and size two STL vectors for row index and value data
    const int vsize = cols + 1;              // upper bound on vector size [1]
    std::vector<int>    inds;                // column index map
    std::vector<double> vals;                // respective values

    // [1] plus one because entry zero is not used by GLPK

    // iterate over rows
    for ( int row = 1;                       // entry zero not used
          row <= rows;
          ++row )
      {
        // first clear and resize the row vectors
        inds.clear();
        inds.resize(vsize);
        vals.clear();
        vals.resize(vsize);

        // recover the required row
        const int colsNonzero = glp_get_mat_row(d_prob,
                                                row,
                                                &inds[0],   // CAUTION: not "inds.begin()"
                                                &vals[0]);

        // iterate over the non-zero columns
        for ( int ind = colsNonzero;         // lengths may vary
              ind > 0;
              --ind)
          {
            const int     col      = inds.at(ind);
            const double  val      = vals.at(ind);
            const double  absVal   = std::abs(val);
            const ij_type location = std::make_pair(row, col);

#if 1 // 0 = silent, 1 = debug reporting
            std::ostringstream oss;
            oss << boost::format("%3d")  % row
                << " "
                << boost::format("%3d")  % col
                << "   "
                << boost::format("%+.2e") % val;
            s_logger->repx(logga::adhc, "matrix entry", oss.str());
#endif // 0

            // load the multimap
            cofmap.insert(std::make_pair(absVal, location));     // [2]

            // [2] unlike a 'std::map', a 'std::multimap' cannot
            // fail to insert because of a key-space crash --
            // hence this function does not return a 'bool' for
            // success or fail
          }
      }

    // final reporting
    const int cofmapSize = cofmap.size();
    s_logger->repx(logga::adhc, "leaving member function, elements", cofmapSize);

    // return
    return cofmap;
  }

  // ---------------------------------------------------------
  //  MEMBER FUNCTION : reportCofs
  // ---------------------------------------------------------
  //  Description  : generate report on absolute minimum and maximum coefficients
  //  Role         : optimization problem improvement reporting
  //  Techniques   : 'std::multimap' and member functions 'lower_bound' 'upper_bound'
  //  Status       : complete
  // ---------------------------------------------------------

  bool
  SolverIf::reportCofs
  (double&       span,
   std::ostream& os) const
  {
    // short-circuit if required
    if ( d_cofSpanLevel == svif::e_inactive )
      {
        return false;
      }

    // initial reporting
    s_logger->repx(logga::adhc, "entering member function", "");

    // load the first line of reporting
    const int leftMargin = 1;                // controls the indenting
    std::ostringstream oss;

    // load the first line of reporting
    oss << "  minimum and maximum absolute coefficients report" << "\n";
    oss << "\n";

    // load the constraint matrix coefficient map
    cofmap_type cofmap = getAbsCofs();       // naturally sorted

    // skip if empty (besides the max code will cause memory errors)
    if ( cofmap.empty() )
      {
        oss << "  no coefficients present" << "\n";
        span = 0.0;                          // load the double argument
        switch ( d_cofSpanLevel )
          {
          case svif::e_say_each:
            s_logger->repx(logga::dbug, "no coefficients to assess", "");
            os << oss.str() << std::flush;   // load the ostream argument
            return true;
          case svif::e_say_fail:
            return false;
          case svif::e_inactive:             // not strictly necessary
            return false;
          default:
            s_logger->repx(logga::kill, "coding error", "");
            return false;
          }
      }

    // preamble
    const double spanTol = xeona::coeffSpanTol;
    int count = 0;

    //declare some iterators
    cofmap_type::const_iterator pos0;
    cofmap_type::const_iterator pos1;
    cofmap_type::const_iterator pos2;

    count = 0;

    // process the minimum absolute coefficients
    pos1                  = cofmap.begin();
    const cof_type begin  = *pos1;
    const double   minAbs = begin.first;
    pos2                  = cofmap.upper_bound(minAbs);
    for ( cofmap_type::const_iterator pos = pos1;
          pos != pos2;
          ++pos )
      {
        const double row  = pos->second.first;
        const double col  = pos->second.second;
        oss << reportCof(row, col, "min abs", leftMargin) << "\n";
      }

    count = 0;

    // process the maximum absolute coefficients
    pos0                  = --cofmap.end();  // back up one
    pos1                  =   cofmap.end();
    const cof_type end    = *pos0;
    const double   maxAbs = end.first;
    pos2                  = cofmap.lower_bound(maxAbs);
    for ( cofmap_type::const_iterator pos = pos2;
          pos != pos1;
          ++pos )
      {
        const double row   = pos->second.first;
        const double col   = pos->second.second;
        oss << reportCof(row, col, "max abs", leftMargin) << "\n";
      }

    // calculate span
    const double spanCal = maxAbs / minAbs;
    oss << "\n";
    oss << "    trip = " << boost::format("%.3e") % spanTol << "\n";
    oss << "    span = " << boost::format("%.3e") % spanCal << "\n";

    // some preparation
    std::ostringstream oss2;
    oss2 << spanCal << " " << spanTol;
    const std::string out = oss2.str();

    // load ostream on trip as required, note the very different logging levels
    span = spanCal;                          // load the double argument
    switch ( d_cofSpanLevel )
      {
      case svif::e_say_each:                 // full report for all
        if ( spanCal >= spanTol )            // tripped
          {
            s_logger->repx(logga::warn, "coeffs tripped tolerance, span trip", out);
            os << oss.str() << std::flush;   // load the ostream argument
            return true;
          }
        else
          {
            s_logger->repx(logga::dbug, "coeff within tolerance, span trip", out);
            os << oss.str() << std::flush;   // load the ostream argument
            return true;
          }
      case svif::e_say_fail:                 // full report on trips
        if ( spanCal >= spanTol )            // tripped
          {
            s_logger->repx(logga::rankJumpy, "coeffs tripped tolerance, span trip", out);
            os << oss.str() << std::flush;   // load the ostream argument
            return true;
          }
        else
          {
            s_logger->repx(logga::adhc, "coeffs within tolerance, span trip", out);
            return false;
          }
      case svif::e_inactive:                 // not strictly necessary
        if ( spanCal >= spanTol )            // tripped
          {
            s_logger->repx(logga::rankJumpy, "coeffs tripped tolerance, span trip", out);
            return false;
          }
        else
          {
            s_logger->repx(logga::adhc, "coeffs within tolerance, span trip", out);
            return false;
          }
      default:
        s_logger->repx(logga::kill, "coding error", "");
        return false;
      }

  } // function 'SolverIf::reportCofs'

  // ---------------------------------------------------------
  //  MEMBER FUNCTION : getFormattedResults
  // ---------------------------------------------------------

  std::string
  SolverIf::getFormattedResults
  (const std::string objectiveTag,
   const std::string variableTag)
  {
    // initial reporting
    s_logger->repx(logga::adhc, "entering member function", "");

    const int tab         = 8;               // fixed indent
    const std::string sep = "  : ";          // tag-value separator

    std::ostringstream oss;
    std::ostringstream tag;

    oss << std::setw(tab) << std::right << objectiveTag << sep
        << getObjectiveValue()
        << "\n";

    const int vars = getVarCount();
    for (int var = 1; var <= vars; var++)
      {
        tag.str("");                         // empty the non-const string-stream
        tag << variableTag << var;
        oss << std::setw(tab) << std::right << tag.str() << sep
            << getVarValue(var)
            << "\n";
      }

    return oss.str();
  }

  // POST-BUILD MANIPULATION

  // ---------------------------------------------------------
  //  MEMBER FUNCTION : getObjective
  // ---------------------------------------------------------
  //  Description  : return current objective function as vector
  //  Role         : called from 'DomainController::capset'
  //  Techniques   : 'std::vector'
  //  Status       : complete
  //
  //  Note use of 'getObjectiveTag' to recover the tag.
  //
  //  CAUTION: post-build
  //
  //      Caller to ensure the call is post-build if so required!
  //
  //  CAUTION: shift
  //
  //      In GLPK, the 0th objective coefficient is the "shift".
  //
  // ---------------------------------------------------------

  std::vector<double>                        // return current objective function
  SolverIf::getObjective() const
  {
    // initial reporting
    s_logger->repx(logga::adhc, "entering member function", "");

    std::vector<double> objFunc;             // vector to load
    const int vars = getVarCount();

    // load vector
    for ( int var = 0;                       // CAUTION: zero-based index due to "shift"
          var <= vars;                       // problem cols
          var++ )
      {
        objFunc.push_back(glp_get_obj_coef(d_prob, var));
      }
    return objFunc;
  }

  // ---------------------------------------------------------
  //  MEMBER FUNCTION : renewObjective
  // ---------------------------------------------------------
  //  Description  : replace objective function
  //  Role         : called from 'DomainController::capset'
  //  Techniques   : 'std::vector'
  //  Status       : complete
  //
  //  CAUTION: post-build
  //
  //      Caller to ensure the call is post-build if so required!
  //
  //  CAUTION: shift
  //
  //      In GLPK, the 0th objective coefficient is the "shift".
  //
  //  CAUTION: revise not load
  //
  //      Code now utilizes 'reviseObj' and not 'loadObj' as the
  //      latter defaults the bounds and probably other unwanted
  //      side-effects (see commit r6235 for faulty code).
  //
  // ---------------------------------------------------------

  bool
  SolverIf::renewObjective
  (const std::vector<double> objFunc,        // entire function
   const std::string         tag)            // objective function name tag, note default
  {
    // initial reporting
    s_logger->repx(logga::xtra, "entering member function, length", objFunc.size());

    // confirm length alignment
    const int objs = objFunc.size();         // supplied vector, includes the "shift"
    const int vars = getVarCount();          // current build
    if      ( objs > vars + 1 )
      {
        s_logger->repx(logga::warn, "given objective function too long", objs);
        return false;
      }
    else if ( objs < vars + 1 )
      {
        s_logger->repx(logga::warn, "given objective function too short", objs);
        return false;
      }

    // rename the objective function
    setObjectiveLabel(tag);

    // first, add the 0th coefficient, the unlabeled "shift"
    reviseObj(0, objFunc.at(0));

    // then, unload the rest of the vector while simultaneously recycling existing labels
    std::string collabel;
    for ( int idx = 1;                       // one-based index, having hopped the "shift"
          idx <= vars;                       // problem cols
          idx++ )
      {
        reviseObj(idx, objFunc.at(idx));     // existing label kept
      }
    return true;

  } // function 'SolverIf::renewObjective'

  // ---------------------------------------------------------
  //  MEMBER FUNCTION : zeroBarMeObjective
  // ---------------------------------------------------------
  //  Description  : create special objective function
  //  Role         : called from 'DomainController::capset'
  //  Techniques   : calls 'renewObjective'
  //  Status       : complete
  //
  //  Note also use of 'getObjectiveTag' to recover tag.
  //
  //  CAUTION: post-build
  //
  //      Caller to ensure the call is post-build if so required!
  //
  // ---------------------------------------------------------

  bool
  SolverIf::zeroBarMeObjective
  (const int         nonzeroGol,             // nominate the only non-zero column
   const double      value,                  // objective coefficient value
   const std::string tag)                    // new objective function name, note default
  {
    // initial reporting
    s_logger->repx(logga::adhc, "entering member function", "");

    // confirm gol within range
    const int vars = getVarCount();
    if ( nonzeroGol > vars || nonzeroGol < 1 )    // 0th index not a sensible choice
      {
        std::ostringstream oss;
        oss << nonzeroGol << " : " << vars;
        s_logger->repx(logga::warn, "invalid gol, gol : var (col) count", oss.str());
        return false;
      }

    // create new objective function
    std::vector<double> objFunc(vars + 1, 0.0);   // fill with zeros, allow for 0th index
    objFunc.at(nonzeroGol) = value;               // overwrite the nominated col

    // call member function
    if ( ! renewObjective(objFunc, tag) )
      {
        s_logger->repx(logga::warn, "renew objective call failed", "");
        return false;
      }
    return true;

  } // function 'SolverIf::zeroBarMeObjective'

  // SOLVER INVOCATION

  // ---------------------------------------------------------
  //  MEMBER FUNCTION : runSolver
  // ---------------------------------------------------------
  //  Description  : public point of entry for invoking solution process
  //  Role         : solver invocation, public
  //  Techniques   : (nothing special)
  //  Status       : complete
  // ---------------------------------------------------------

  void
  SolverIf::runSolver()
  {
    // initial reporting
    s_logger->repx(logga::xtra, "entering member function", "");

    // screen the problem build
    if ( d_probStatus < status_empty )       // was 'status_solvable'
      {
        std::ostringstream put;
        put << utilReportOnProb();
        s_logger->putx(logga::dbug, put);
        s_logger->repx(logga::warn, "problem incomplete and abandoning", d_probStatus);
        incrementAlertCount();
        return;                              // CAUTION: note the return
      }
    else if ( d_noise >= svif::medium )
      {
        *s_os << "\n"
              << utilReportOnProb()          // passive
              << "\n"
              << std::flush;
      }

    solverCheckProb();                       // checks integrity and sets 'd_probStatus'
    if ( d_probStatus < status_empty )       // was 'status_solvable'
      {
        s_logger->repx(logga::dbug, "abandoning before solver call", d_probStatus);
        return;
      }
    if ( d_probStatus == status_empty )
      {
        s_logger->repx(logga::rankJumpy, "empty problem (no rows and no cols)", "");
      }

    solverInvokeSolver();                    // invokes solver, set d_solverRet
    solverAnalyzeSoln();                     // sets d_solnRet and d_solnStatus

    // housekeeping
    d_solverCalls++;

    // log the status
    switch ( d_solnStatus )
      {
      case svif::soln_optimal:
        break;
      case svif::soln_feasible:
        break;
      case svif::soln_not_usable:
        s_logger->repx(logga::warn, "solution not usable, soln status", d_solnStatus);
        incrementAlertCount();
        break;
      default:
        s_logger->repx(logga::warn, "unsupported solution status", d_solnStatus);
        std::clog << "** coding error 06 in source file " << __FILE__ << std::endl;
        break;
      }

    // undertake duplicated reporting if required
    if ( d_noise >= svif::high )             // equal or above 'high'
      {
        *s_os << "\n"
              << utilReportOnProb()          // passive
              << "\n"
              << utilReportOnSolver()        // passive
              << "\n"
              << utilReportOnSoln()          // passive
              << std::flush;

        s_logger->addSmartBlank();
      }

    // KKT reporting
    reportKkt();

    // completion reporting
    s_logger->repx(logga::adhc, "leaving member function", "");

  } // function 'SolverIf::runSolver'

  // ---------------------------------------------------------
  //  MEMBER FUNCTION : solverCheckProb (private)
  // ---------------------------------------------------------
  //  Description  : checks whether problem is fully built and acceptable to GLPK
  //  Role         : called prior to 'solverInvokeSolver'
  //  Techniques   : sets 'd_probStatus'
  //  Status       : complete
  //
  //  Design notes
  //
  //      This method is more comprehensive than
  //      'updateProbStatus'.
  //
  // ---------------------------------------------------------

  void
  SolverIf::solverCheckProb()
  {
    // initial reporting
    s_logger->repx(logga::adhc, "entering member function", "");

    ProblemStatus status = status_not_specified;  // initial value

    const int nVars   = glp_get_num_rows(d_prob);      // all variables
    const int nCons   = glp_get_num_cols(d_prob);      // all constraints
    const int nCoeffs = glp_get_num_nz(d_prob);        // loaded coefficients

    if ( nVars   < 1 ) s_logger->repx(logga::dbug, "insufficient cols, count",   nVars);
    if ( nCons   < 1 ) s_logger->repx(logga::dbug, "insufficient rows, count",   nCons);
    if ( nCoeffs < 1 ) s_logger->repx(logga::dbug, "insufficient coeffs, count", nCoeffs);

    if ( d_objSense == sense_not_specified )
      {
        s_logger->repx(logga::dbug, "objective sense not set, value", d_objSense);
      }

    // hunt for unfilled cols
    int colErrors = 0;
    std::string colsBad;

    for ( int j = 1; j <= nCons; j++ )
      {
        const char* clabel = glp_get_col_name(d_prob, j);
        if ( clabel == NULL )                // name not set
          {
            colErrors++;
            colsBad += " ";
            colsBad += indexToString(j);
          }
      }
    if ( ! colsBad.empty() ) colsBad.erase(0, 1); // remove leading space
    if ( colErrors )
      {
        s_logger->repx(logga::dbug, "unfilled cols found", colsBad);
      }

    // hunt for unfilled rows
    int rowErrors = 0;
    std::string rowsBad;

    for ( int i = 1; i <= nVars; ++i )
      {
        const char* clabel = glp_get_row_name(d_prob, i);
        if ( clabel == NULL )                // name not set
          {
            rowErrors++;
            rowsBad += " ";
            rowsBad += indexToString(i);
          }
      }
    if ( ! rowsBad.empty() ) rowsBad.erase(0, 1);      // remove leading space
    if ( rowErrors )
      {
        s_logger->repx(logga::dbug, "unfilled rows found", rowsBad);
      }

    // wash up (see commit r3718 for version which rejects empty problems)
    if      ( nVars   == 0                      &&  // no structural variables
              nCons   == 0                      &&  // no constraints
              nCoeffs == 0                      &&  // no constraint matrix entries
              d_objSense > sense_not_specified  &&  // objective sense not explicity set
              colErrors == 0                    &&  // unfilled cols
              rowErrors == 0 )                      // unfilled rows
      {
        status = status_empty;
      }
    else if ( nVars   < 1                       ||  // no structural variables
              nCons   < 1                       ||  // no constraints
              nCoeffs < 1                       ||  // no constraint matrix entries
              d_objSense == sense_not_specified ||  // objective sense not explicity set
              colErrors > 0                     ||  // unfilled cols
              rowErrors > 0 )                       // unfilled rows
      {
        status = status_incomplete;
      }
    else
      {
        status = status_solvable;
      }
    d_probStatus = status;

  } // function 'SolverIf::solverCheckProb'

  // ---------------------------------------------------------
  //  MEMBER FUNCTION : solverInvokeSolver (private)
  // ---------------------------------------------------------
  //  Description  : match solver to klass, invoke solver, and check exit status
  //  Role         : solver invocation
  //  Techniques   : (nothing special)
  //  Status       : complete
  //
  //  Design notes
  //
  //      Invoke solver and check the solver exit status.  The
  //      matching of solver type to problem klass is also
  //      undertaken.
  //
  //      The logic in this function is quite involved and users
  //      should confirm that it meets their needs.
  //
  //      A problem which is MIP will override any preference to
  //      use the interior point solver.
  //
  //      Note that the solution is analyzed later.  At this
  //      point, only solver issues are checked.  This means that
  //      solution issues like feasibility and optimality, are
  //      NOT examined here.
  //
  //      Solver success returns now vary!  LPX_E_OK is 200,
  //      while success for a glp-prefixed call is simply zero.
  //
  //      The actual solver calls are "wrapped" in s_os newline
  //      and flush calls.
  //
  //   'glpsol' API call logic (for comparison)
  //
  //      'glpsol' is the stand-alone command-line GLPK solver.
  //
  //      'glpsol' contains a similar API call logic to that used
  //      below -- for GLPK 4.42, refer to:
  //
  //          source   : src/glpapi20.c
  //          line     : 1033
  //          function : 'glp_main'
  //          search   : "/* solve the problem */"
  //
  //      This note was added long after the 'xeona' solver
  //      interface was coded -- nonetheless this information
  //      could be useful if there is a need to fix or refine the
  //      API call logic used here.
  //
  //      In particular, calls to 'glp_set_bfcp' -- change basis
  //      factorization control parameters -- might be required.
  //      That said, the control settings employed appear rather
  //      arcane -- hence the code here simply accepts the
  //      defaults.
  //
  //      Another issue is that 'glp_intopt' is smarter than the
  //      original MIP call logic used here.  That said, the
  //      previous "two call" logic has been retained -- for no
  //      other reason than it works.
  //
  // ---------------------------------------------------------

  void
  SolverIf::solverInvokeSolver()
  {
    // initial reporting
    s_logger->repx(logga::xtra, "entering member function", "");

    // set local trip level
    const logga::Rank trip = logga::dbug;

    // update problem klass and status
    updateProbKlass();
    updateProbStatus();

    // ---------------------------------
    //  ONE: scale the problem as appropriate
    // ---------------------------------

    // for more information, see the GLPK manual and also the
    // GLPK source files "glpk.h" and 'src/glpscl.c'

    // determine the scale flag
    int scaleFlag = -1;                      // nonsensical value, GLPK should error
    switch ( d_employScaling )
      {
      case true:
        // elective scaling
        scaleFlag = GLP_SF_GM | GLP_SF_EQ | GLP_SF_SKIP;    // GLP_SF_AUTO at v4.45
        s_logger->repx(logga::dbug, "full scaling", "");
        break;
      case false:
        // default non-scaling (which may paradoxically involve scaling)
#if 1 // 0 = no scaling, 1 = equilibration scaling when needed
        scaleFlag = GLP_SF_EQ | GLP_SF_SKIP;
#else
        scaleFlag = 0;
#endif // 0
        s_logger->repx(logga::dbug, "basic scaling", "");
        break;
      }

    // report
    std::ostringstream oss;
    oss << std::hex << scaleFlag;            // in 0x format
    s_logger->repx(logga::dbug, "GLPK glp_scale_prob call", oss.str());

    // apply scaling
    if ( ! logga::checkReportLevel(trip) ) glp_term_out(GLP_OFF);
    s_logger->addDumbBlank(trip);
    glp_scale_prob(d_prob, scaleFlag);
    s_logger->addDumbBlank(trip);
    if ( ! logga::checkReportLevel(trip) ) glp_term_out(GLP_ON);

    // ---------------------------------
    //  TWO: construct the basis as appropriate
    // ---------------------------------

    const int basisFlag = 0;                 // only 0 is currently supported

    switch ( d_employAdvBasis )
      {
      case true:
        s_logger->repx(logga::dbug, "GLPK glp_adv_basis call", basisFlag);
        if ( ! logga::checkReportLevel(trip) ) glp_term_out(GLP_OFF);
        s_logger->addDumbBlank(trip);
        glp_adv_basis(d_prob, basisFlag);
        s_logger->addDumbBlank(trip);
        if ( ! logga::checkReportLevel(trip) ) glp_term_out(GLP_ON);
        break;
      case false:
        s_logger->repx(logga::adhc, "GLPK construct adv basis omitted", "");
        break;
      }

    // ---------------------------------
    // THREE: invoke solver based on problem klass and any client
    //        code preferences regarding solver type
    // ---------------------------------

    switch ( d_probKlass )
      {
        // ---------------------------------
        //  linear problem
        // ---------------------------------

      case prob_linear:
        switch ( d_solverType )
          {
          case solver_other:                           // no preference from client code
          case solver_simplex:                         // elected
          case solver_integer:                         // cannot be explicitly elected
            s_logger->repx(logga::dbug, "GLPK glp_simplex call", "simplex solver");
            d_solverType = solver_simplex;             // to enable correct reporting
            s_logger->addDumbBlank(trip);
            d_solverRet  = glp_simplex(d_prob,         // simplex solver call
                                       &d_parmSimplex);
            s_logger->addDumbBlank(trip);
            break;
          case solver_interior:                        // elected
            s_logger->repx(logga::dbug, "GLPK glp_interior call","interior point solver");
            d_solverType = solver_interior;            // to enable correct reporting
            s_logger->addDumbBlank(trip);
            d_solverRet  = glp_interior(d_prob,        // interior point solver call
                                        &d_parmInterior);
            s_logger->addDumbBlank(trip);
            break;
          default:
            s_logger->repx(logga::warn, "unsupported solver type", d_solverType);
            std::clog << "** coding error 07 in source file " << __FILE__ << std::endl;
            break;
          } // solver type code
        // report on solver return
        if ( d_solverRet == 0 )                        // covers all solvers
          {
            s_logger->repx(logga::dbug,
                           "main solver success, solver return",
                           d_solverRet);
          }
        else
          {
            s_logger->repx(logga::warn,
                           "main solver FAIL, solver return",
                           d_solverRet);
          }
        break; // linear problem

        // ---------------------------------
        //  non-linear problem
        // ---------------------------------

      case prob_mixed_integer:
      case prob_mixed_zero_one:
      case prob_pure_integer:
      case prob_pure_zero_one:
        s_logger->repx(logga::dbug, "GLPK glp_simplex call", "simplex solver first");
        d_solverType = solver_simplex;                 // to enable correct reporting
        s_logger->addDumbBlank(trip);
        d_solverRet  = glp_simplex(d_prob,             // simplex solver call FIRST
                                   &d_parmSimplex);
        s_logger->addDumbBlank(trip);
        if ( d_solverRet == 0 )                        // check return
          // strictly speaking, solution success and not solver
          // success should be checked because, as currently
          // coded, nonfeasble (proven infeasible) problems go on
          // to call the MIP solver -- this however is not a
          // significant problem because 'glp_intopt' complains
          // straightaway
          {
            s_logger->repx(logga::dbug, "GLPK glp_intopt call", "integer solver second");
            d_solverType = solver_integer;             // to enable correct reporting
            s_logger->addDumbBlank(trip);
            d_solverRet  = glp_intopt(d_prob,          // integer solver call SECOND
                                      &d_parmInteger);
            s_logger->addDumbBlank(trip);
            if ( d_solverRet == 0 )
              {
                s_logger->repx(logga::dbug,
                               "MIP solver success, solver return",
                               d_solverRet);
              }
            else
              {
                s_logger->repx(logga::dbug,
                               "MIP solver FAIL, solver return",
                               d_solverRet);
              }
          }
        else
          {
            s_logger->repx(logga::dbug,
                           "LP relaxation FAIL, solver return",
                           d_solverRet);
          }
        break; // non-linear problem

      default:
        s_logger->repx(logga::warn, "unsupported problem klass", d_probKlass);
        std::clog << "** coding error 08 in source file " << __FILE__ << std::endl;
        break;
      } // problem klass

    // ---------------------------------
    //  housekeeping
    // ---------------------------------

    // revise problem status
    d_probStatus = status_submitted;         // moreover, a solver must have run

  } // function 'SolverIf::solverInvokeSolver'

  // ---------------------------------------------------------
  //  MEMBER FUNCTION : solverAnalyzeSoln (private)
  // ---------------------------------------------------------
  //  Description  :
  //  Role         : solver invocation
  //  Techniques   : (nothing special)
  //  Status       : complete
  // ---------------------------------------------------------

  void
  SolverIf::solverAnalyzeSoln()
  {
    // initial reporting
    s_logger->repx(logga::adhc, "entering member function", "");

    // obtain solution status
    switch ( d_solverType )
      {
      case solver_simplex:
        d_solnRet = glp_get_status(d_prob);
        break;
      case solver_interior:
        d_solnRet = glp_ipt_status(d_prob);
        break;
      case solver_integer:
        d_solnRet = glp_mip_status(d_prob);
        break;
      default:
        s_logger->repx(logga::warn, "unsupported solver type", d_solverType);
        std::clog << "** coding error 09 in source file " << __FILE__ << std::endl;
        break;
      }

    // process solution status (based on assumption that GLPK
    // encapsulates the solver status in these returns)

    switch ( d_solverType )
      {
      case solver_simplex:
        switch ( d_solnRet )
          {
          case GLP_OPT:
            d_solnStatus = soln_optimal;
            break;
          case GLP_FEAS:
            d_solnStatus = soln_feasible;
            break;
          case GLP_INFEAS:
          case GLP_NOFEAS:
          case GLP_UNBND:
          case GLP_UNDEF:
            d_solnStatus = soln_not_usable;
            break;
          default:
            s_logger->repx(logga::warn, "unsupported GLPK solver return", d_solnRet);
            std::clog << "** coding error 10 in source file " << __FILE__ << std::endl;
            break;
          }
        break;
      case solver_interior:
        switch ( d_solnRet )
          {
          case GLP_OPT:
            d_solnStatus = soln_optimal;
            break;
          case GLP_UNDEF:
            d_solnStatus = soln_not_usable;
            break;
          default:
            s_logger->repx(logga::warn, "unsupported GLPK solver return", d_solnRet);
            std::clog << "** coding error 11 in source file " << __FILE__ << std::endl;
            break;
          }
        break;
      case solver_integer:
        switch ( d_solnRet )
          {
          case GLP_OPT:
            d_solnStatus = soln_optimal;
            break;
          case GLP_FEAS:
            d_solnStatus = soln_feasible;
            break;
          case GLP_NOFEAS:
          case GLP_UNDEF:
            d_solnStatus = soln_not_usable;
            break;
          default:
            std::clog << "** coding error 12 in source file " << __FILE__ << std::endl;
            break;
          }
        break;
      default:
        std::clog << "** coding error 13 in source file " << __FILE__ << std::endl;
        break;
      }

  } // function 'SolverIf::solverAnalyzeSoln'

  // SOLUTION RECOVERY : usage must follow solver call

  // ---------------------------------------------------------
  //  MEMBER FUNCTION : getObjectiveValue
  // ---------------------------------------------------------
  //  Description  : return final objective value, which may or may not be optimal
  //  Role         : solution recovery
  //  Techniques   : 'glp_get_obj_val' and similar
  //  Status       : complete
  // ---------------------------------------------------------

  double
  SolverIf::getObjectiveValue() const
  {
    // note out of order call
    if ( d_probStatus < status_submitted )
      s_logger->repx(logga::warn, "problem not submitted to solver", "");

    // process call based on solver type
    double objective = getNaN();
    switch ( d_solverType )
      {
      case solver_simplex:
        objective = glp_get_obj_val(d_prob); // also valid part way thru solution process
        break;
      case solver_interior:
        objective = glp_ipt_obj_val(d_prob);
        break;
      case solver_integer:
        objective = glp_mip_obj_val(d_prob);
        break;
      default:
        std::clog << "** coding error 14 in source file " << __FILE__ << std::endl;
        break;
      }

    // numerical zero processing
    const double objectiveTrip = zeroTripVal(objective);

    // return possibly modified value
    return objectiveTrip;

  } // function 'SolverIf::getObjectiveValue'

  // ---------------------------------------------------------
  //  MEMBER FUNCTION : getVarValue
  // ---------------------------------------------------------
  //  Description  : return final structural var value, which may or may not be optimal
  //  Role         : solution recovery
  //  Techniques   : 'glp_get_col_val' and similar
  //  Status       : complete
  // ---------------------------------------------------------

  double
  SolverIf::getVarValue
  (const int col) const
  {
    // note out of order call
    if ( d_probStatus < status_submitted )
      s_logger->repx(logga::warn, "problem not submitted to solver", "");

    // process call based on solver type
    double value = getNaN();
    switch ( d_solverType )
      {
      case solver_simplex:
        value = glp_get_col_prim(d_prob, col);
        break;
      case solver_interior:
        value = glp_ipt_col_prim(d_prob, col);
        break;
      case solver_integer:
        value = glp_mip_col_val(d_prob, col);
        break;
      default:
        std::clog << "** coding error 15 in source file " << __FILE__ << std::endl;
        break;
      }

    // numerical zero processing
    const double valueTrip = zeroTripVal(value);

    // return possibly modified value
    return valueTrip;

  } // function 'SolverIf::getVarValue'

  // ---------------------------------------------------------
  //  MEMBER FUNCTION : getIntegerVarValue
  // ---------------------------------------------------------
  //  Description  : return final integer var value, which may or may not be optimal
  //  Role         : solution recovery
  //  Techniques   : 'glp_get_col_val' and similar
  //  Status       : complete
  //
  //  Design notes
  //
  //      See Becker (2007, ch12) regarding floating-point
  //      exception code.  This code complies with TR1 and C99.
  //
  // ---------------------------------------------------------

  int
  SolverIf::getIntegerVarValue
  (const int col) const
  {
    // check if binary
    const int colkind = glp_get_col_kind(d_prob, col);
    if ( colkind != GLP_IV )
      {
        s_logger->repx(logga::warn, "value not integer, GLPK col kind", colkind);
      }

    // recover value
    const double value = getVarValue(col);

    // convert to integer (some elements may move to 'std::tr1::' under <cfenv>)
    fexcept_t feStatus;
    const int feFlag = FE_INEXACT;
    fegetexceptflag(&feStatus, feFlag);      // obtain current floating-point status
    feclearexcept(feFlag);                   // duly clear status
    const double dint = rint(value);         // refer <cmath>
    if ( fetestexcept(feFlag) )              // check for changed status
      {
        std::ostringstream oss;
        oss << value << " : " << dint;
        s_logger->repx(logga::warn, "inexact integer conversion", oss.str());
      }
    fesetexceptflag(&feStatus, feFlag);      // reinstate floating-point status

    // cast to integer and return
    const int iint = static_cast<int>(dint);
    return iint;

  } // function 'SolverIf::getIntegerVarValue'

  // ---------------------------------------------------------
  //  MEMBER FUNCTION : getBinaryVarValue
  // ---------------------------------------------------------
  //  Description  : return final binary var value, which may or may not be optimal
  //  Role         : solution recovery
  //  Techniques   : 'glp_get_col_val' and similar
  //  Status       : complete
  // ---------------------------------------------------------

  bool                                       // 'false' = 0 and 'true' = 1 (like C++)
  SolverIf::getBinaryVarValue
  (const int col) const
  {
    // check if binary
    const int colkind = glp_get_col_kind(d_prob, col);
    if ( colkind != GLP_BV )
      {
        s_logger->repx(logga::warn, "value not binary, GLPK col kind", colkind);
      }

    // process, noting both zero and unity are accurately represented
    const double value = getVarValue(col);
    if      ( value == 0.0 ) return false;
    else if ( value == 1.0 ) return true;
    else
      {
        s_logger->repx(logga::warn, "problem identifying binary", "");
        return true;                         // need something (or else go to 'tribool')
      }

  } // function 'SolverIf::getBinaryVarValue'

  // ---------------------------------------------------------
  //  MEMBER FUNCTION : getObjCoeff
  // ---------------------------------------------------------
  //  Description  : get objective coefficient
  //  Role         : solution recovery
  //  Techniques   : 'glp_get_obj_coeff'
  //  Status       : complete
  // ---------------------------------------------------------

  double                                     // simple retrieval
  SolverIf::getObjCoeff                      // an objective coefficient value
  (const int col) const
  {
    return glp_get_obj_coef(d_prob, col);    // col = 0 is legal and returns 'shift'
  }

  // ---------------------------------------------------------
  //  MEMBER FUNCTION : getObjCoeffXVarValue
  // ---------------------------------------------------------
  //  Description  : get objective coefficient x variable value
  //  Role         : solution recovery
  //  Techniques   : local calls
  //  Status       : complete
  // ---------------------------------------------------------

  double
  SolverIf::getObjCoeffXVarValue             // getObjCoeff x getVarValue
  (const int col) const
  {
    // note out of order call
    if ( d_probStatus < status_submitted )
      {
        s_logger->repx(logga::warn, "problem not submitted to solver", "");
      }

    return getObjCoeff(col) * getVarValue(col);

  } // function 'SolverIf::getObjCoeffXVarValue'

  // ---------------------------------------------------------
  //  MEMBER FUNCTION : getSlackValue
  // ---------------------------------------------------------
  //  Description  : get LP slack value
  //  Role         : solution recovery
  //  Techniques   : (nothing special)
  //  Status       : complete, needs confirmation
  // ---------------------------------------------------------

  double
  SolverIf::getSlackValue
  (const int row) const
  {
    // complain if out of order call
    if ( d_probStatus < status_submitted )
      s_logger->repx(logga::warn, "problem not submitted to solver", "");

    // process call based on solver type
    double slack = 0.0;
    switch ( d_solverType )
      {
      case solver_simplex:
      case solver_integer:                        // LP relaxation
        slack = glp_get_row_dual(d_prob, row);    // reduced cost of auxiliary variable
        break;
      case solver_interior:
        slack = glp_ipt_row_dual(d_prob, row);    // reduced cost of axillary variable
        break;
      default:
        return getNaN();                          // CAUTION: do not feed to 'zeroTripVal'
        break;
      }

    // numerical zero processing
    const double slackTrip = zeroTripVal(slack);

    // return possibly modified value
    return slackTrip;

  } // function 'SolverIf::getSlackValue'

  // ---------------------------------------------------------
  //  MEMBER FUNCTION : getShadowValue
  // ---------------------------------------------------------
  //  Description  : get LP shadow value
  //  Role         : solution recovery
  //  Techniques   : (nothing special)
  //  Status       : complete, needs confirmation
  // ---------------------------------------------------------

  double
  SolverIf::getShadowValue
  (const int col) const
  {
    // complain if out of order call
    if ( d_probStatus < status_submitted )
      s_logger->repx(logga::warn, "problem not submitted to solver", "");

    // process call based on solver type
    double shadow = 0.0;
    switch ( d_solverType )
      {
      case solver_simplex:
      case solver_integer:                        // LP relaxation
        shadow = glp_get_col_dual(d_prob, col);   // reduced cost of structural variable
        break;
      case solver_interior:
        shadow = glp_ipt_col_dual(d_prob, col);   // reduced cost of structural variable
        break;
      default:
        return getNaN();                          // CAUTION: do not feed to 'ZeroTripVal'
        break;
      }

    // numerical zero processing
    const double shadowTrip = zeroTripVal(shadow);

    // return possibly modified value
    return shadowTrip;

  } // function 'SolverIf::getShadowValue'

  // ---------------------------------------------------------
  //  MEMBER FUNCTION : isUsableSoln
  // ---------------------------------------------------------
  //  Description  : return true or false based on solution status
  //  Role         :
  //  Techniques   : (nothing special)
  //  Status       : complete
  // ---------------------------------------------------------

  bool
  SolverIf::isUsableSoln() const             // feasible plus other usefulness tests
  {
    // note out of order call
    if ( d_probStatus < status_submitted )
      {
        s_logger->repx(logga::warn, "problem not submitted to solver", "");
      }

    switch ( d_solnStatus )
      {
      case soln_optimal:           // solution optimal
      case soln_feasible:          // solution usable but not optimal
        return true;

      case soln_not_usable:        // solution not usable but problem not proven bad
      case soln_proven_bad:        // problem proven bad
        return false;

      case soln_faulty:
      case soln_other:
        return false;

      case soln_undefined:
        s_logger->repx(logga::warn, "undefined solution, soln status", d_solnStatus);
        return false;

      default:
        std::clog << "** coding error 16 in source file " << __FILE__ << std::endl;
        return false;
      }

  } // function 'SolverIf::isUsableSoln'

  // ---------------------------------------------------------
  //  MEMBER FUNCTION : isOptimalSoln
  // ---------------------------------------------------------
  //  Description  : return true or false based on solution status
  //  Role         :
  //  Techniques   : (nothing special)
  //  Status       : complete
  // ---------------------------------------------------------

  bool
  SolverIf::isOptimalSoln() const            // proven optimal
  {
    // note out of order call
    if ( d_probStatus < status_submitted )
      s_logger->repx(logga::warn, "problem not submitted to solver", "");

    switch ( d_solnStatus )
      {
      case soln_optimal:
        return true;
      default:
        return false;
      }

  } // function 'SolverIf::isOptimalSoln'

  // ---------------------------------------------------------
  //  MEMBER FUNCTION : writeInfo
  // ---------------------------------------------------------
  //  Description  : invokes appropriate GLPK print calls
  //  Role         : diagnostics
  //  Techniques   : 'glp_print_*'
  //  Status       : complete
  //
  //  CAUTION: file overwrite
  //
  //      This function may overwrite any previous files.
  //
  //  CAUTION: spaces to underscores
  //
  //      This function will convert any space characters (' ')
  //      in the supplied filestem to underscore characters ('_').
  //
  // ---------------------------------------------------------

  void
  SolverIf::writeInfo() const
  {
    const std::string label = getProblemLabel();  // call protects against GLPK NULL
    writeInfo(label);
  }

  void
  SolverIf::writeInfo                        // wrapper to std::string function
  (const char* filestem) const               // filestem name, extensions added later
  {
    if ( filestem == NULL )                  // protect against NULL char*
      {
        writeInfo();
      }
    else
      {
        const std::string buf(filestem);     // C-string conversion
        writeInfo(buf);                      // simple wrapper to the std::string variant
      }
  }

  void
  SolverIf::writeInfo                        // workhorse
  (const std::string filestem) const
  {
    // initial reporting
    s_logger->repx(logga::adhc, "entering member function, filestem", filestem);

    // note out of order call
    if ( d_probStatus < status_submitted )
      {
        s_logger->repx(logga::warn, "problem not submitted to solver", "");
      }

    // count calls and trip if necessary
    const  int callCountTrip = 2000;
    static int callCount     = 0;
    ++callCount;
    if ( callCount > callCountTrip )
      {
        s_logger->repx(logga::adhc, "call count trip exceeded", callCount);
        return;
      }

    // process filestem
    std::string sFilestem = filestem;                            // remove const
    std::replace(sFilestem.begin(), sFilestem.end(), ' ', '_');  // spaces to underscores

    // prepend the current PID (process ID) and call count
    const int padding1 = 6;                   // zero pad level for PID
    const int padding2 = 4;                   // zero pad level for call count
    std::ostringstream oss;
    oss << std::setw(padding1) << std::setfill('0') << xeona::pid << "-";   // PID
    oss << std::setw(padding2) << std::setfill('0') << callCount  << "-";   // call count
    sFilestem = oss.str() + sFilestem;       // prepend the "00000-0000-"

    // concatenate extensions
    std::string sProbFilename = sFilestem + ".prob";
    std::string sGlpkFilename = sFilestem + ".glpk";
    std::string sSolnFilename = sFilestem + ".soln";
    std::string sSensFilename = sFilestem + ".sens";

    // convert to non-const char* for GLPK interface
    char* cProbFilename = const_cast<char*>(sProbFilename.c_str());
    char* cGlpkFilename = const_cast<char*>(sGlpkFilename.c_str());
    char* cSolnFilename = const_cast<char*>(sSolnFilename.c_str());
    char* cSensFilename = const_cast<char*>(sSensFilename.c_str());

    // call various GLPK print utilities, intended for visual analysis
    *s_os << "\n" << std::flush;                  // for tidy output
    glp_write_lp(d_prob, NULL, cProbFilename);    // problem details in CPLEX format
    glp_write_prob(d_prob, 0, cGlpkFilename);     // problem details in GLPK format
    *s_os << "\n" << std::flush;                  // for tidy output
    xeona::readonly(cProbFilename);
    xeona::readonly(cGlpkFilename);

    if ( d_solnRet != 0 )     // status routines have returned a three-digit int
      {
        switch ( d_solverType )
          {
          case solver_simplex:
            { // CAUTION: braces here prevent "crosses initialization" errors

              // solution details
              *s_os << "\n" << std::flush;   // for tidy output
              const int retSol = glp_print_sol(d_prob, cSolnFilename);
              *s_os << "\n" << std::flush;   // for tidy output
              if ( retSol == 0 ) xeona::readonly(cSolnFilename);
              else s_logger->repx(logga::warn, "glp_print_sol returned fail", retSol);

              // print ranges is more complicated because the
              // basis factorization must exist -- this is always
              // the case when the simplex presolver is NOT
              // deployed
              switch ( glp_bf_exists(d_prob) )
                {
                case 0:                      // factorization required
                  switch ( glp_factorize(d_prob) )
                    {
                    case 0:
                      s_logger->repx(logga::dbug, "factorize success", "");
                      break;
                    case GLP_EBADB:
                      s_logger->repx(logga::warn,
                                     "factorize fail, basis invalid",
                                     "GLP_EBADB");
                      break;
                    case GLP_ESING:
                      s_logger->repx(logga::warn,
                                     "factorize fail, basis singular",
                                     "GLP_ESING");
                      break;
                    case GLP_ECOND:
                      s_logger->repx(logga::warn,
                                     "factorize fail, ill-conditioned",
                                     "GLP_ECOND");
                      break;
                    }
                  break;
                default:                     // factorization not required
                  s_logger->repx(logga::dbug, "factorize not required", "");
                  break;
                }

              // can fall thru because 'glp_print_ranges' is tolerant
              *s_os << "\n" << std::flush;   // for tidy output
              const int retRng = glp_print_ranges(d_prob, 0, NULL, 0, cSensFilename);
              *s_os << "\n" << std::flush;   // for tidy output
              if ( retRng == 0 ) xeona::readonly(cSensFilename);
              else s_logger->repx(logga::warn, "glp_print_ranges returned fail", retRng);
            }
            break;
          case solver_interior:
            {
              // solution details
              *s_os << "\n" << std::flush;   // for tidy output
              const int retIpt = glp_print_ipt(d_prob, cSolnFilename);
              *s_os << "\n" << std::flush;   // for tidy output
              if ( retIpt == 0 )xeona::readonly(cSolnFilename);
              else s_logger->repx(logga::warn, "glp_print_ipt returned fail", retIpt);
            }
            break;
          case solver_integer:
            {
              // solution details
              *s_os << "\n" << std::flush;   // for tidy output
              const int retMip = glp_print_mip(d_prob, cSolnFilename);
              *s_os << "\n" << std::flush;   // for tidy output
              if ( retMip == 0 ) xeona::readonly(cSolnFilename);
              else s_logger->repx(logga::warn, "glp_print_mip returned fail", retMip);
            }
            break;
          default:
            std::clog << "** coding error 17 in source file " << __FILE__ << std::endl;
            break;
          }
      }
    else
      {
        s_logger->repx(logga::warn, "cannot glp_print_* to file", d_solnRet);
      }

    // some reporting, readily middle-button mouse pasted onto the command-line
    *s_os << "\n";
    *s_os << "  to view output: cat "
          << sFilestem << ".{prob,soln,sens,glpk}"
          << " | less"
          << "\n"
          << std::flush;

    *s_os << "  to view output: for file in "
          << sFilestem << ".{prob,soln,sens,glpk}"
          << "; do printf \"\\nfile : $file\\n\\n\"; cat $file; done | less +/\"file :\""
          << "\n"
          << "\n"
          << std::flush;

  } // function 'SolverIf::writeInfo'

  // ---------------------------------------------------------
  //  MEMBER FUNCTION : yesOrNo
  // ---------------------------------------------------------
  //  Description  : gives "yes" or "no"
  //  Role         : used to reinterpret bool-returning calls
  //  Status       : complete
  // ---------------------------------------------------------

  std::string
  SolverIf::yesOrNo
  (const bool trueOrFalse) const
  {
    return trueOrFalse ? "yes" : "no";
  }

  // MISCELLANEOUS

  // ---------------------------------------------------------
  //  MEMBER FUNCTION : getInf
  //  MEMBER FUNCTION : getNaN
  // ---------------------------------------------------------
  //  Description  : return non-a-number (quiet form)
  //  Role         : miscellaneous
  //  Techniques   : <limits>
  //  Status       : complete
  // ---------------------------------------------------------

  double
  SolverIf::getInf() const
  {
    return std::numeric_limits<double>::infinity();    // <limits>
  }

  double
  SolverIf::getNaN() const
  {
    return std::numeric_limits<double>::quiet_NaN();   // <limits>
  }

  // KARUSH-KUHN-TUCKER (KKT) REPORTING

  // ---------------------------------------------------------
  //  MEMBER FUNCTION : reportKkt (private)
  // ---------------------------------------------------------
  //  Description  : organizes a KKT report as required
  //  Role         : point of entry
  //  Techniques   : (nothing special)
  //  Status       : complete
  // ---------------------------------------------------------

  void
  SolverIf::reportKkt() const
  {
    // initial reporting
    s_logger->repx(logga::adhc, "entering member function", "");

    // GLPK hardcodes
    int sol;                                 // specifies which solution:
    switch ( isLP() )                        // .. {GLP_SOL, GLP_IPT, GLP_MIP}
      {
      case true:  sol = GLP_SOL; break;
      case false: sol = GLP_MIP; break;
      }
    int cond = GLP_KKT_PE;                   // specifies which condition:
                                             // {GLP_KKT_PE, GLP_KKT_PB, GLP_KKT_DE,
                                             // GLP_KKT_DB}

    // GLPK fill variables
    double ae_max;                           // largest absolute error
    int    ae_ind;                           // context-dependent index for above
    double re_max;                           // largest relative error
    int    re_ind;                           // context-dependent index for above

    // GLPK API call
    glp_check_kkt(d_prob,
                  sol,
                  cond,
                  &ae_max,                       // CAUTION: address-of operator
                  &ae_ind,
                  &re_max,
                  &re_ind);

    // create the report
    std::ostringstream put;
    createKktReport(put, ae_max, ae_ind, re_max, re_ind, 2);
    s_logger->flush();
    s_logger->putx(logga::info, put);
    s_logger->addSmartBlank(logga::info);
  }

  // ---------------------------------------------------------
  //  MEMBER FUNCTION : createKktReport (private)
  // ---------------------------------------------------------
  //  Description  : create the actual KKT report
  //  Role         : support for 'reportKkt' point of entry
  //  Techniques   : (nothing special)
  //  Status       : complete
  // ---------------------------------------------------------

  void
  SolverIf::createKktReport
  (std::ostream& os,
   const double  ae_max,
   const int     ae_ind,
   const double  re_max,
   const int     re_ind,
   const int     indent) const               // note default value
  {
    const std::string pad(indent, ' ');
    const int rows = glp_get_num_rows(d_prob);

    std::string title;
    switch ( isLP() )
      {
      case true:  title = "KKT (Karush-Kuhn-Tucker) optimality conditions report"; break;
      case false: title = "Integer feasibility conditions report";                 break;
      }

    os << pad << title << "\n";
    os << "\n";
    os << pad << boost::format("  max abs error = %.2e on index %3d\n") % ae_max % ae_ind
       << pad << boost::format("  max rel error = %.2e on index %3d\n") % re_max % re_ind
       << "\n";
  }

  // UTILITY FUNCTIONS

  // ---------------------------------------------------------
  //  MEMBER FUNCTION : utilCreateGlpkProb (private)
  // ---------------------------------------------------------
  //  Description  : create GLPK problem instance and add label
  //  Role         : utility function
  //  Techniques   : (nothing special)
  //  Status       : complete
  // ---------------------------------------------------------

  void
  SolverIf::utilCreateGlpkProb
  (const std::string tag)
  {
    // create empty GLPL problem instance
    s_logger->repx(logga::dbug, "about to call GLPK glp_create_prob", "");
    d_prob = glp_create_prob();

    // confirm empty GLPK problem instance
    if ( d_prob == NULL )
      {
        s_logger->repx(logga::warn, "GLPK glp_create_prob gave NULL", d_prob);
        incrementAlertCount();
        return;                              // CAUTION: note early return
      }

    // label the problem
    setProblemLabel(tag);

    // update problem klass
    updateProbKlass();                       // an empty problem is deemed linear!

    // update problem status
    updateProbStatus();

    // update instance count
    s_instanceCount++;
    s_logger->repx(logga::xtra, "SolverIf and glp_prob instances", s_instanceCount);

  } // function 'SolverIf::utilCreateGlpkProb'

  // ---------------------------------------------------------
  //  MEMBER FUNCTION : utilSetGlpkDefaults (private)
  // ---------------------------------------------------------
  //  Description  : set construction-time defaults
  //  Role         : utility function
  //  Techniques   : 'glp_*cp' structs
  //  Status       : complete
  //
  //  Design notes
  //
  //      GLPK parameters are initially set here.
  //
  //      These values could also be reset in the client code if
  //      suitable public member functions are written.
  //
  //      See the GLPK manual for details.
  //
  // ---------------------------------------------------------

  void
  SolverIf::utilSetGlpkDefaults()
  {
    // determine appropriate message level from 'd_noise' setting
    int msglev = GLP_MSG_ALL;                // set high
    switch ( d_noise )                       // set in constructor
      {
      case svif::not_specified:              // not overridden, a coding error
        std::clog << "** coding error 18 in source file " << __FILE__ << std::endl;
        break;
      case svif::silent:
        msglev = GLP_MSG_OFF;                // no output (but leakage may occur)
        break;
      case svif::low:
        msglev = GLP_MSG_ERR;                // errors and warnings
        break;
      case svif::medium:
        msglev = GLP_MSG_ON;                 // normal
        break;
      case svif::high:
        msglev = GLP_MSG_ALL;                // normal plus information
        break;
      default:
        std::clog << "** coding error 19 in source file " << __FILE__ << std::endl;
      }

    // determine time limit in milliseconds
    int tmlim;
    tmlim                    = 500 * 1000;   // 500s = 08:20:00
    tmlim                    = INT_MAX;      // 2147483647ms = 2147483s = 596:31:23 ~= 24d
    if ( svif::DBUG ) tmlim  = INT_MAX;      // user modifiable

    // SIMPLEX CONTROL PARAMETERS

    d_parmSimplex.msg_lev    = msglev;       // message level, see above
    d_parmSimplex.tm_lim     = tmlim;        // searching time limit in milliseconds
    d_parmSimplex.out_frq    = 100;          // output frequency in iterations (200) [1]
    d_parmSimplex.out_dly    = 0;            // output delay in milliseconds (0) [2]
    d_parmSimplex.presolve   = GLP_OFF;      // LP presolver (GLP_OFF)

    d_parmSimplex.meth       = GLP_PRIMAL;   // simplex method option (GLP_PRIMAL)
    d_parmSimplex.pricing    = GLP_PT_PSE;   // pricing technique (GLP_PT_PSE)
    d_parmSimplex.r_test     = GLP_RT_HAR;   // ratio test technique (GLP_RT_HAR)
    d_parmSimplex.it_lim     = INT_MAX;      // simplex iteration limit (INT_MAX)

    // INTERIOR POINT CONTROL PARAMETERS

    d_parmInterior.msg_lev   = msglev;       // message level, see above
    d_parmInterior.ord_alg   = GLP_ORD_AMD;  // ordering algorithm prior to Cholesky

    // MIXED-INTEGER CONTROL PARAMETERS

    d_parmInteger.msg_lev    = msglev;       // message level, see above
    d_parmInteger.tm_lim     = tmlim;        // searching time limit in milliseconds
    d_parmInteger.out_frq    = 1000;         // output frequency in milliseconds (5000)[3]
    d_parmInteger.out_dly    = 0;            // output delay in milliseconds (10000) [2]
    d_parmInteger.presolve   = GLP_OFF;      // mixed-integer presolver (GLP_OFF)

    d_parmInteger.br_tech    = GLP_BR_DTH;   // branching technique
    d_parmInteger.bt_tech    = GLP_BT_BLB;   // backtracking technique
    d_parmInteger.pp_tech    = GLP_PP_ALL;   // preprocessing technique
    d_parmInteger.fp_heur    = GLP_OFF;      // feasibility pump heuristic technique
    d_parmInteger.gmi_cuts   = GLP_OFF;      // mixed-integer cut technique
    d_parmInteger.mir_cuts   = GLP_OFF;      // mixed-integer rounding technique
    d_parmInteger.cov_cuts   = GLP_OFF;      // enable/disable mixed cover cuts
    d_parmInteger.clq_cuts   = GLP_OFF;      // enable/disable generating clique cuts
    d_parmInteger.br_tech    = GLP_BR_DTH;   // branching technique
    d_parmInteger.binarize   = GLP_OFF;      // binarization option [4]

    // GLPK 4.41 defaults sometimes recorded in round brackets
    //
    // [1] output frequency in iterations : this parameter
    // specifies how frequently the solver sends information
    // about the solution process to the terminal
    //
    // [2] output delay in milliseconds : this parameter
    // specifies how long the solver should delay sending
    // information about the solution process to the terminal
    //
    // [3] output frequency in milliseconds : this parameter
    // specifies how frequently the solver sends information
    // about the solution process to the terminal
    //
    // [4] binarization option (used only if the presolver is
    // enabled) : replace general integer variables by binary
    // ones, do not use binarization
    //
    // (source: GLPK 4.41 manual)

    // reporting preparation
    const int decimalPlaces = 1;
    std::ostringstream oss;
    oss << std::fixed << std::setprecision(decimalPlaces)
        << (static_cast<double>(tmlim) / 1000.0);
    const std::string tmlimSeconds = oss.str();

    // report
    s_logger->repx(logga::xtra, "GLPK msg_lev message level",    msglev);
    s_logger->repx(logga::xtra, "GLPK tm_lim timeout (seconds)", tmlimSeconds);

  } // function 'SolverIf::utilSetGlpkDefaults'

  // ---------------------------------------------------------
  //  MEMBER FUNCTION : utilDeleteGlpkProb (private)
  // ---------------------------------------------------------
  //  Description  : delete GLPK problem instance, recover label
  //  Role         : utility function, supports the destructor and reset
  //  Techniques   : (nothing special)
  //  Status       : complete
  // ---------------------------------------------------------

  const std::string
  SolverIf::utilDeleteGlpkProb()
  {
    // recover label
    std::string label;                       // empty string

    // free GLPK problem instance memory -- but not GLPK library environment memory
    if ( d_prob != NULL )
      {
        s_logger->repx(logga::dbug, "about to call GLPK glp_delete_prob", "");
        label = getProblemLabel();           // call protects against GLPK NULL
        glp_delete_prob(d_prob);             // also release GLPK problem instance memory
        d_prob = NULL;                       // null-out the pointer
        s_instanceCount--;                   // update instance count
      }
    else
      {
        s_logger->repx(logga::warn, "problem instance unexpectedly NULL", d_prob);
      }

    // return recovered label
    return label;

  } // function 'SolverIf::utilDeleteGlpkProb'

  // ---------------------------------------------------------
  //  MEMBER FUNCTION : utilSetDefaultBnds
  // ---------------------------------------------------------
  //  Description  : set the default bounds on variables
  //  Role         : utility function, used when creating new columns
  //  Techniques   : (nothing special)
  //  Status       : complete
  //
  //  Design notes
  //
  //      GLP_FR is free (both numeric arguments ignored in this
  //      case).  GLP_LO is set lower bound using next argument
  //      (final numeric argument ignored in this case).
  //
  // ---------------------------------------------------------

  void
  SolverIf::utilSetDefaultBnds
  (int col)
  {

#if   (XE_BNDS_DEFAULT == 0)

    // apply the free non-constraint -- which might also
    // generate "PROBLEM HAS UNBOUNDED SOLUTION" output from GLPK

    glp_set_col_bnds(d_prob, col, GLP_FR, 0.0, 0.0);   // zero bounds okay for FR

#elif (XE_BNDS_DEFAULT == 1)

    // apply the standard non-negativity constraint for a structural variable
    //
    //  * hence :                        x >= 0
    //  * in GLPK terminology :          GLP_LO
    //
    // the final arguments in the GLPK call are 'lower bound' and 'upper bound'

    glp_set_col_bnds(d_prob, col, GLP_LO, 0.0, 0.0);   // zero upper bound okay for LO

#endif // XE_BNDS_DEFAULT

  } // function 'SolverIf::utilSetDefaultBnds'

  // ---------------------------------------------------------
  //  MEMBER FUNCTION : utilResetCoeffVectors (private)
  // ---------------------------------------------------------
  //  Description  : reset vectors using 'clear' and then 'resize'
  //  Role         : utility function
  //  Status       : complete
  // ---------------------------------------------------------

  void
  SolverIf::utilResetCoeffVectors()
  {
    // initial reporting
    s_logger->repx(logga::adhc, "entering member function", "");

    // reset vectors using 'clear' and then 'resize'
    d_ia.clear();
    d_ja.clear();
    d_ar.clear();

    d_ia.resize(s_glpkMargin, 0);            // see notes for constructor
    d_ja.resize(s_glpkMargin, 0);
    d_ar.resize(s_glpkMargin, 0.0);

  } // function 'SolverIf::utilResetCoeffVectors'

  // ---------------------------------------------------------
  //  MEMBER FUNCTION : utilResetObjectData (private)
  // ---------------------------------------------------------
  //  Description  : reset the relevant status variables
  //  Role         : utility function
  //  Status       : complete
  // ---------------------------------------------------------

  void
  SolverIf::utilResetObjectData()
  {
    // initial reporting
    s_logger->repx(logga::adhc, "entering member function", "");

    // CAUTION: retained: 'd_simplexPresol'

    d_probStatus = status_not_specified;
    d_objSense   = sense_not_specified;
    d_probKlass  = prob_not_specified;
    d_solverType = solver_other;
    d_solnStatus = soln_undefined;
    d_solverRet  = 0;
    d_solnRet    = 0;

    d_solverCalls = 0;
    d_alertCount  = 0;

  } // function 'SolverIf::utilResetObjectData'

  // ---------------------------------------------------------
  //  MEMBER FUNCTION : utilReportOnProb (private)
  // ---------------------------------------------------------
  //  Description  : reports on problem
  //  Role         : passive utility function -- can be omitted without side-effect
  //  Techniques   : read code
  //  Status       : complete
  //
  //  Design notes
  //
  //      These "utilReportOn.." functions should probably take
  //      in an ostream object, rather than return a string.
  //
  // ---------------------------------------------------------

  std::string
  SolverIf::utilReportOnProb()
  {
    // initial reporting
    s_logger->repx(logga::adhc, "entering member function", "");

    std::ostringstream ssBuff;
    std::string buf;

    //    EXAMPLE:
    //    problem name                     : prob-some-name
    //    problem klass                    : linear
    //    objective sense                  : maximize
    //    problem status                   : solvable
    //    alert count                      :       99      <
    //    solver call count                :        1
    //    strictly binary variables        :        0
    //    strictly integer variables       :        0
    //    strictly continuous variables    :        4
    //    all variables                    :        4
    //    all constraints                  :        4
    //    all non-zero coefficients        :       11
    //    coefficients/constraints         :     2.75
    //    values numerical zero trip       :    1e-10
    //    coefficients numerical zero trip :    1e-10

    buf = "(faulty code)";
    buf = getProblemLabel();                 // call protects against GLPK NULL
    formatLineOutput(ssBuff, "problem name", buf);

    // previous code used switch (lpx_get_class(d_prob) ..
    // this API is depreciated, but it seems sensible to run an update
    updateProbKlass();
    buf = "(faulty code)";                   // in case this is not overwritten
    switch ( d_probKlass )
      {
      case prob_not_specified:  buf = "not specified";                              break;
      case prob_linear:         buf = "linear";                                     break;
      case prob_mixed_integer:  buf = "mixed integer";                              break;
      case prob_mixed_zero_one: buf = "mixed 0-1";                                  break;
      case prob_pure_integer:   buf = "pure integer";                               break;
      case prob_pure_zero_one:  buf = "pure 0-1";                                   break;
      default:
        std::clog << "** coding error 20 in source file " << __FILE__ << std::endl;
        break;
      }
    formatLineOutput(ssBuff, "problem klass", buf);

    buf = "(faulty code)";                   // in case this is not overwritten
    switch ( glp_get_obj_dir(d_prob) )
      {
      case GLP_MIN: buf = "minimize";                                               break;
      case GLP_MAX: buf = "maximize";                                               break;
      }
    formatLineOutput(ssBuff, "objective sense", buf);

    buf = "(faulty code)";                   // in case this is not overwritten
    switch ( d_probStatus )
      {
      case status_not_specified: buf = "not specified";                             break;
      case status_incomplete:    buf = "incomplete";                                break;
      case status_empty:         buf = "empty (no rows and no cols)";               break;
      case status_solvable:      buf = "solvable";                                  break;
      case status_submitted:     buf = "submitted";                                 break;
      default:
        std::clog << "** coding error 21 in source file " << __FILE__ << std::endl;
        break;
      }
    formatLineOutput(ssBuff, "problem status", buf);

    std::string extra = "";
    if ( d_alertCount > 0 ) extra = "      <";
    formatLineOutput(ssBuff, "alert count", d_alertCount, extra);

    formatLineOutput(ssBuff, "solver call count", d_solverCalls);

    int nBins  = glp_get_num_bin(d_prob);    // number of integer columns ranged [0,1]
    int nInts  = glp_get_num_int(d_prob);    // number of integer columns

    int nCols  = glp_get_num_cols(d_prob);   // number of columns
    int nRows  = glp_get_num_rows(d_prob);   // number of rows
    int nCoefs = glp_get_num_nz(d_prob);     // number of constraint coefficients
    double div = static_cast<double>(nCoefs)/static_cast<double>(nRows);

    formatLineOutput(ssBuff, "strictly binary variables",     nBins);
    formatLineOutput(ssBuff, "strictly integer variables",    nInts - nBins);
    formatLineOutput(ssBuff, "strictly continuous variables", nCols - nInts);
    formatLineOutput(ssBuff, "all variables",                 nCols);
    formatLineOutput(ssBuff, "all constraints",               nRows);
    formatLineOutput(ssBuff, "all non-zero coefficients",     nCoefs);
    if ( nRows == 0 )
      formatLineOutput(ssBuff, "coefficients/constraints",      "-"); // otherwise 'nan'
    else
      formatLineOutput(ssBuff, "coefficients/constraints",      div);

    const double zeroVal = d_zeroVal ? d_zeroTol : 0.0;
    const double zeroCof = d_zeroCof ? d_zeroTol : 0.0;
    formatLineOutput(ssBuff, "values numerical zero trip",       zeroVal);
    formatLineOutput(ssBuff, "coefficients numerical zero trip", zeroCof);

    return ssBuff.str();

  } // function 'SolverIf::utilReportOnProb'

  // ---------------------------------------------------------
  //  MEMBER FUNCTION : utilReportOnSolver (private)
  // ---------------------------------------------------------
  //  Description  : reports on solver
  //  Role         : passive utility function -- can be omitted without side-effect
  //  Techniques   : read code
  //  Status       : complete
  // ---------------------------------------------------------

  std::string
  SolverIf::utilReportOnSolver()
  {
    // initial reporting
    s_logger->repx(logga::adhc, "entering member function", "");

    std::ostringstream ssBuff;
    std::string buf = "";

    //    EXAMPLE:
    //    solver type used                : mixed integer (with simplex)
    //    simplex presolver               : yes
    //    solver exit                     : some explanation (GLP_ECODE)

    buf = "(faulty code)";                   // in case this is not overwritten
    switch ( d_solverType )
      {
      case solver_simplex:  buf = "simplex";                                        break;
      case solver_interior: buf = "interior point";                                 break;
      case solver_integer:  buf = "mixed integer (with simplex)";                   break;
      default:
        break;
      }

    formatLineOutput(ssBuff, "solver type used", buf);

    buf = "(faulty code)";                   // in case this is not overwritten
    switch ( d_simplexPresol )
      {
      case true:  buf = "yes";                                                      break;
      case false: buf = "no";                                                       break;
      default:
        break;
      }
    switch ( d_solverType )
      {
      case solver_interior:  buf = "not supported as presolver";                    break;
      default:
        break;
      }

    formatLineOutput(ssBuff, "simplex presolver set", buf);

    buf = "(faulty code)";                   // in case this is not overwritten
    switch ( d_solverType )
      {
      case solver_simplex:                   // 'glp_simplex' call documentation
        switch ( d_solverRet )
          {
          case 0:          buf = "solver completed (return 0)";                     break;
          case GLP_EBADB:  buf = "no start as basis invalid (GLP_EBADB)";           break;
          case GLP_ESING:  buf = "no start as basis singular (GLP_ESING)";          break;
          case GLP_ECOND:  buf = "no start as basis ill-conditioned (GLP_ECOND)";   break;
          case GLP_EBOUND: buf = "no start due to bad bounds (GLP_EBOUND)";         break;
          case GLP_EFAIL:  buf = "unspecified solver failure (GLP_EFAIL)";          break;
          case GLP_EOBJLL: buf = "search end as z decreasing (GLP_EOBJLL)";         break;
          case GLP_EOBJUL: buf = "search end as z increasing (GLP_EOBJUL)";         break;
          case GLP_EITLIM: buf = "iteration limit reached (GLP_EITLIM)";            break;
          case GLP_ETMLIM: buf = "time limit reached (GLP_ETMLIM)";                 break;
          case GLP_ENOPFS: buf = "(presolver used) no LP primal (GLP_ENOPFS)";      break;
          case GLP_ENODFS: buf = "(presolver used) no LP dual (GLP_ENODFS";         break;
          default:
            std::clog << "** coding error 22 in source file " << __FILE__ << std::endl;
            break;
          }
        break;
      case solver_interior:                  // 'glp_interior' call documentation
        switch ( d_solverRet )
          {
          case 0:           buf = "solver completed (return 0)";                    break;
          case GLP_EFAIL:   buf = "bad data (GLP_EFAIL)";                           break;
          case GLP_ENOCVG:  buf = "slow convergence or divergence (GLP_ENOCVG)";    break;
          case GLP_EITLIM:  buf = "iteration limit exceeded (GLP_EITLIM)";          break;
          case GLP_EINSTAB: buf = "numerical instability (GLP_EINSTAB)";            break;
          default:
            std::clog << "** coding error 23 in source file " << __FILE__ << std::endl;
            break;
          }
        break;
      case solver_integer:                   // 'glp_intopt' call documentation
        switch ( d_solverRet )
          {
          case 0:          buf = "solver completed (returned 0)";                   break;
          case GLP_EBOUND: buf = "no start due to bad bounds (GLP_EBOUND)";         break;
          case GLP_EROOT:  buf = "no start as no initial LP relax (GLP_EROOT)";     break;
          case GLP_EFAIL:  buf = "unspecified solver failure (GLP_EFAIL)";          break;
          case GLP_ETMLIM: buf = "time limit reached (GLP_ETMLIM)";                 break;
          case GLP_ESTOP:  buf = "search terminated by application (GLP_ESTOP)";    break;
          default:
            std::clog << "** coding error 24 in source file " << __FILE__ << std::endl;
            break;
          }
        break;
      default:
        break;
      }

    formatLineOutput(ssBuff, "exit status", buf);

    return ssBuff.str();

  } // function 'SolverIf::utilReportOnSolver'

  // ---------------------------------------------------------
  //  MEMBER FUNCTION : utilReportOnSoln (private)
  // ---------------------------------------------------------
  //  Description  : reports on solution
  //  Role         : passive utility function -- can be omitted without side-effect
  //  Techniques   : (nothing special)
  //  Status       : complete
  // ---------------------------------------------------------

  std::string
  SolverIf::utilReportOnSoln()
  {
    // initial reporting
    s_logger->repx(logga::adhc, "entering member function", "");

    std::ostringstream ssBuff;
    std::string buf = "";

    //    EXAMPLE:
    //    solution status                 : optimal solution
    //    objective                       :     2.99e+002

    buf = "(faulty code)";                   // in case this is not overwritten
    switch ( d_solverType )
      {
      case solver_simplex:                   // 'glp_get_status' call documentation
        switch ( d_solnRet )
          {
          case GLP_OPT:    buf = "optimal soln (GLP_OPT)";                          break;
          case GLP_FEAS:   buf = "feasible soln (GLP_FEAS)";                        break;
          case GLP_INFEAS: buf = "soln infeasible but not proven bad (GLP_INFEAS)"; break;
          case GLP_NOFEAS: buf = "problem proven nonfeasible (GLP_NOFEAS)";         break;
          case GLP_UNBND:  buf = "problem unbounded (GLP_UNBND)";                   break;
          case GLP_UNDEF:  buf = "soln undefined (GLP_UNDEF)";                      break;
          default:
            std::clog << "** coding error 25 in source file " << __FILE__ << std::endl;
            break;
          }
        break;
      case solver_interior:                  // 'glp_ipt_status' call documentation
        switch ( d_solnRet )
          {
          case GLP_OPT:   buf = "optimal soln (GLP_OPT)";                           break;
          case GLP_UNDEF: buf = "soln undefined (GLP_UNDEF)";                       break;
            // two new returns added in GLPK 4.38
#if ( GLP_MAJOR_VERSION == 4 && GLP_MINOR_VERSION >= 38 )
          case GLP_INFEAS: buf = "soln infeasible but not proven bad (GLP_INFEAS)"; break;
          case GLP_NOFEAS: buf = "problem proven nonfeasible (GLP_NOFEAS)";         break;
#endif
          default:
            std::clog << "** coding error 26 in source file " << __FILE__ << std::endl;
            break;
          }
        break;
      case solver_integer:                   // 'glp_mip_status' call documentation
        switch ( d_solnRet )
          {
          case GLP_OPT:    buf = "integer optimal soln (GLP_OPT)";                  break;
          case GLP_FEAS:   buf = "integer feasible soln, maybe optimal (GLP_FEAS)"; break;
          case GLP_NOFEAS: buf = "problem proven infeasible (GLP_NOFEAS)";          break;
          case GLP_UNDEF:  buf = "soln undefined (GLP_UNDEF)";                      break;
          default:
            std::clog << "** coding error 27 in source file " << __FILE__ << std::endl;
            break;
          }
        break;
      default:
        break;
      }
    formatLineOutput(ssBuff, "solution status", buf);

    double objective = getObjectiveValue();
    formatLineOutput(ssBuff, "final objective value", objective);

    return ssBuff.str();

  } // function 'SolverIf::utilReportOnSoln'

  // ---------------------------------------------------------
  //  MEMBER FUNCTION : indexToString <> (private)
  // ---------------------------------------------------------
  //  Description  : convert a number to a string
  //  Role         : utility function
  //  Techniques   : ostream formatting
  //  Status       : complete
  //
  //  Design notes
  //
  //      Should non-default formatting be required, Stephens
  //      etal (2006) cover the formatting of floating point
  //      output in recipe 10.2 (pp 356--359).
  //
  //      Alternatively, implement (for yourself) code using the
  //      Boost library Boost.Conversion and
  //      boost::lexical_cast<std::string>(number).
  //
  // ---------------------------------------------------------

  template<typename T>
  std::string
  SolverIf::numToString
  (const T number) const
  {
    std::ostringstream ssBuf;
    ssBuf << number;                         // specify additional formatting here
    return ssBuf.str();
  }

  // ---------------------------------------------------------
  //  MEMBER FUNCTION : indexToString (private)
  // ---------------------------------------------------------
  //  Description  : index to string conversion
  //  Role         : reporting support
  //  Techniques   : C++ streams
  //  Status       : complete
  //
  //  Design notes
  //
  //       The int 's_indexPad' is set in the class definition.
  //       A value of 0 (or 1) suppress padding, larger values
  //       enable zero padding.
  //
  //  CAUTION: negative indexes
  //
  //      The padding routine requires non-negative input
  //      (otherwise it can output strings like: 00-22).  The
  //      Boost.Format library provides a more robust solution
  //      (or a more sophisticated routine can be hand coded).
  //
  // ---------------------------------------------------------

  std::string
  SolverIf::indexToString
  (const int index) const
  {
    std::ostringstream ssBuf;
    ssBuf << std::setfill('0') << std::setw(s_indexPad) << index;
    return ssBuf.str();
  }

  // ---------------------------------------------------------
  //  MEMBER FUNCTION : getProblemTag
  // ---------------------------------------------------------
  //  Description  : return problem tag as submitted on construction
  //  Role         : general information
  //  Status       : complete
  //  CAUTION      : consider also 'getProblemLabel'
  // ---------------------------------------------------------

  const std::string
  SolverIf::getProblemTag() const
  {
    return d_probTag;
  }

  // ---------------------------------------------------------
  //  MEMBER FUNCTION : getAlertCount
  // ---------------------------------------------------------
  //  Description  : return number of alerts
  //  Role         : general information
  //  Status       : complete
  // ---------------------------------------------------------

  int
  SolverIf::getAlertCount() const
  {
    return d_alertCount;
  }

  // ---------------------------------------------------------
  //  MEMBER FUNCTION : incrementAlertCount (private)
  // ---------------------------------------------------------
  //  Description  : increments alert count and offers additional features
  //  Role         : use in preference to 'd_alertCount++'
  //  Techniques   : utility function
  //  Status       : complete
  // ---------------------------------------------------------

  int
  SolverIf::incrementAlertCount()
  {
    d_alertCount++;                          // this should be a nested class ideally
    if ( ! svif::DBUG )
      {
        s_logger->repx(logga::info, "incrementing alerts, alert count", d_alertCount);
      }

    return d_alertCount;
  }

  // ---------------------------------------------------------
  //  MEMBER FUNCTION : formatLineOutput (private)
  //  MEMBER FUNCTION : formatLineOutput <> (private)
  // ---------------------------------------------------------
  //  Description  : provide formatted output for use in 'utilReportOn*' functions
  //  Role         : utility function
  //  Techniques   : string-stream
  //  Status       : complete
  // ---------------------------------------------------------

  void
  SolverIf::formatLineOutput
  (std::ostringstream& ssOut,
   std::string         tag,
   std::string         value)
  {
    std::ios_base::fmtflags was = ssOut.flags();
    ssOut << std::setw(s_W0)
          << ""
          << std::setw(s_W1)
          << std::left
          << tag
          << std::setw(s_W2)
          << ":"
          << value
          << "\n";
    ssOut.flags(was);                        // reset formatting flags
  }

  template <typename T>
  void
  SolverIf::formatLineOutput
  (std::ostringstream& ssOut,
   std::string         tag,
   T                   value,                // int or double expected
   std::string         extra)                // optional trailing string
  {
    std::ios_base::fmtflags was = ssOut.flags();
    ssOut << std::setprecision(s_P1);
    ssOut << std::setw(s_W0)
          << ""
          << std::setw(s_W1)
          << std::left
          << tag
          << std::setw(s_W2)
          << ":"
          << std::setw(s_W3)
          << std::right
          << value
          << extra
          << "\n";
    ssOut.flags(was);                        // reset formatting flags
  }

  // ---------------------------------------------------------
  //  MEMBER FUNCTION : getGlpkVersion (static)
  // ---------------------------------------------------------
  //  Description  : returns GLPK version string in form 0.00
  //  Role         : general information
  //  Techniques   : 'glp_version'
  //  Status       : complete
  // ---------------------------------------------------------

  const std::string
  SolverIf::getGlpkVersion()
  {
    return glp_version();                    // const char* converted to std::string
  }

  // ---------------------------------------------------------
  //  MEMBER FUNCTION : updateProbKlass (private)
  // ---------------------------------------------------------
  //  Description  : update the problem klass based on the current build information
  //  Role         : internal
  //  Techniques   : 'd_probKlass'
  //  Status       : complete
  //
  //  CAUTION: terminology
  //
  //      The term 'integer' here excludes binaries -- and thus
  //      differs from from the problem classification!
  //
  // ---------------------------------------------------------

  void
  SolverIf::updateProbKlass()
  {
    // code
    int nVars     = 0;                       // variables
    int nCons     = 0;                       // continuous variables
    int nNonCons  = 0;                       // integer and binary variables
    int nInts     = 0;                       // integer (excluding binary) variables
    int nBins     = 0;                       // binary variables

    nVars    = glp_get_num_cols(d_prob);     // continuous, integer, and binary variables
    nNonCons = glp_get_num_int(d_prob);      // integer and binary variables
    nBins    = glp_get_num_bin(d_prob);      // just binary variables

    nCons = nVars    - nNonCons;
    nInts = nNonCons - nBins;

    ProblemKlass klass = prob_not_specified; // initial value

    if ( nVars == 0 )                        // no variables loaded
      {
        klass = prob_linear;                 // an empty problem is deemed linear!
      }
    else                                     // a substantial problem
      {
        if ( nCons == 0 )                    // pure not mixed nonlinear
          {
            if ( nInts == 0 )                // no integers present
              klass = prob_pure_zero_one;
            else                             // integers present
              klass = prob_pure_integer;
          }
        else if ( nNonCons == 0 )            // linear
          {
            klass = prob_linear;
          }
        else                                 // mixed nonlinear
          {
            if ( nInts == 0 )                // no integers present
              klass = prob_mixed_zero_one;
            else                             // integers present
              klass = prob_mixed_integer;
          }
      }

    // report
    std::string buf = "(faulty code)";       // in case this is not overwritten
    switch ( klass )
      {
      case prob_not_specified:  buf = "not specified";                              break;
      case prob_linear:         buf = "linear";                                     break;
      case prob_mixed_integer:  buf = "mixed integer";                              break;
      case prob_mixed_zero_one: buf = "mixed 0-1";                                  break;
      case prob_pure_integer:   buf = "pure integer";                               break;
      case prob_pure_zero_one:  buf = "pure 0-1";                                   break;
      default:
        std::clog << "** coding error 28 in source file " << __FILE__ << std::endl;
        break;
      }
    s_logger->repx(logga::xtra, "klass = " + buf, "");

    // return
    d_probKlass = klass;

  } // function 'SolverIf::updateProbKlass'

  // ---------------------------------------------------------
  //  MEMBER FUNCTION : updateProbStatus (private)
  // ---------------------------------------------------------
  //  Description  : update the problem status based on the current build information
  //  Role         : internal
  //  Techniques   : 'd_probStatus'
  //  Status       : complete
  // ---------------------------------------------------------

  void
  SolverIf::updateProbStatus()
  {
    // initial reporting omitted as not helpful

    int nVars   = glp_get_num_rows(d_prob);  // all variables
    int nCons   = glp_get_num_cols(d_prob);  // all constraints
    int nCoeffs = glp_get_num_nz(d_prob);    // loaded coefficients

    ProblemStatus status = status_not_specified;  // initial value

    // process (see commit r3718 for version which rejects empty problems)
    if      ( nVars      > 0 &&
              nCons      > 0 &&
              nCoeffs    > 0 &&
              d_objSense > sense_not_specified )  // overwrite is by 'setConstraintSense'
      {
        status = status_solvable;
        s_logger->repx(logga::xtra, "status = solvable", "");
      }
    else if ( nVars      == 0 &&
              nCons      == 0 &&
              nCoeffs    == 0 &&
              d_objSense > sense_not_specified )  // overwrite is by 'setConstraintSense'
      {
        status = status_empty;
        s_logger->repx(logga::xtra, "status = empty", "");
      }
    else
      {
        status = status_incomplete;
        s_logger->repx(logga::adhc, "status = incomplete", "");
      }

    d_probStatus = status;                   // update

  } // function 'SolverIf::updateProbStatus'

  // ---------------------------------------------------------
  //  MEMBER FUNCTION : zeroTripCof (private)
  // ---------------------------------------------------------
  //  Description  : intercept input and make numerical zeros exact
  //  Role         : load coefficients (including RHS) support
  //  Techniques   : (nothing special)
  //  Status       : complete
  //
  //  Design notes
  //
  //      If the global variable 'xeona::zero' is set to 'false'
  //      then tripped zero rounding should not occur.
  //
  //      The command-line option '--zero' toggles the default
  //      behavior.
  //
  //      That default is set in 'common.cc' and, at the time of
  //      writing, was 'true'.
  //
  // ---------------------------------------------------------

  double
  SolverIf::zeroTripCof
  (const double cof) const
  {
    if ( xeona::zero == false )                                                return cof;
    if ( d_zeroCof == true && d_zeroTol != 0.0 && std::abs(cof) <= d_zeroTol ) return 0.0;
    else                                                                       return cof;
  }

  // ---------------------------------------------------------
  //  MEMBER FUNCTION : zeroTripVal (private)
  // ---------------------------------------------------------
  //  Description  : intercept output and make numerical zeros exact
  //  Role         : get values (including slack and shadow values) support
  //  Techniques   : (nothing special)
  //  Status       : complete
  // ---------------------------------------------------------

  double
  SolverIf::zeroTripVal
  (const double val) const
  {
    if ( d_zeroVal == true && d_zeroTol != 0.0 && std::abs(val) <= d_zeroTol ) return 0.0;
    else                                                                       return val;
  }

} // namespace 'svif'

//  end of file
