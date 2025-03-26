//  XEONA ENERGY SYSTEMS MODELING ENVIRONMENT
//
//  file-create-name : glpkviz.cc
//  file-create-date : Thu 05-Jun-2008 14:03 UTC
//  file-initiator   : Robbie Morrison <robbie@actrix.co.nz>
//
//  file-role        : HTML visualization of GLPK problem instances / implementation
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
//  $URL: file:///home/robbie/svn-root/xeona/futz/trunk/xeona1/d/glpkviz.cc $
//
//  GENERAL NOTES FOR THIS FILE

//  LOCAL AND SYSTEM INCLUDES

#include "glpkviz.h"          // companion header for this file (place first)

#if   (XE_GLPKVIZ_ALONE == 0)
#include "../d/siglp.h"       // semi-intelligent interface to GLPK MILP solver
#include "../c/files.h"       // free functions for regular files
#include "../a/logger.h"      // standard logging functionality (as required)
#include ".././common.h"      // common definitions for project (place last)
#endif // XE_GLPKVIZ_ALONE

#include <algorithm>          // STL copying, searching, and sorting
#include <fstream>            // file-based io
#include <iomanip>            // setw() and family
#include <iostream>           // standard io
#include <sstream>            // string-streams
#include <string>             // C++ strings
#include <vector>             // STL sequence container

#include <cstdio>             // C-style io, remove(), EOF
#include <cstdlib>            // C-style exit(), getenv(), system(), NULL, EXIT_SUCCESS
#include <ctime>              // C-style time and date functions

#include <unistd.h>           // POSIX sleep(), usleep(), access(), chown()

#include <glpk.h>             // GNU GLPK mixed integer linear (MILP) solver

//  FILE-LOCAL TEST MACRO

//#define _DEBUG // table boundaries shown in light blue, system calls echoed

//  CODE

//  STATIC DEFINITIONS

std::string GlpkViz::s_helpExt   = "help";
std::string GlpkViz::s_htmlExt   = "html";
int         GlpkViz::s_callCount = 0;

#if   (XE_GLPKVIZ_ALONE == 1)                // stand-alone

  // CREATORS

GlpkViz::GlpkViz
(const std::string browserInvoke) :
  d_browser(browserInvoke),
  d_prob(NULL),
  d_html(),
  d_comment(),
  d_filename1(),
  d_filename2(),
  d_title(),
  d_round(true),
  d_timestamp()
{ }

GlpkViz::~GlpkViz()
{
}

// FUNCTION CALL OPERATORS

// ---------------------------------------------------------
//  MEMBER FUNCTION : operator()  (1 of 3)
// ---------------------------------------------------------
//  Description  : principal call for GLPK instances
//  Role         :
//  Signature    : 'glp_prob'
//  Techniques   :
//  Status       : complete
// ---------------------------------------------------------

void
GlpkViz::operator()
(glp_prob*         prob,                     // GLPK problem instance
 const std::string comment,
 const int         outputPrecision)
{
  // CAUTION: it is not possible to make a logger call here

  // reset data members
  resetData();                               // utility call

  // grab problem
  d_prob = prob;
  if ( !d_prob ) return;

  // store comment
  d_comment = comment;

  // set output precision for floats
  d_html << std::setprecision(outputPrecision);

  // stamp the time
  d_timestamp = timestamp();                 // utility call

  // main action
  prepareFilename();
  prepareTitle();
  createHtml();
  saveHtml(d_filename1);
  createHelp();
  saveHtml(d_filename2);
  displayHtml(d_filename1);
  }

#elif (XE_GLPKVIZ_ALONE == 0)                // 'xeona'

logga::spLogger GlpkViz::s_logger = logga::ptrLogStream();  // bind

// CREATORS

GlpkViz::GlpkViz
(const std::string browserInvoke) :            // thus a zero-argument ctor
  d_browser(browserInvoke),
  d_prob(NULL),
  d_html(),
  d_comment(),
  d_filename1(),
  d_filename2(),
  d_title(),
  d_round(true),
  d_timestamp()
{
  s_logger->repx(logga::dbug, "constructor call, browser string", d_browser);
}

GlpkViz::~GlpkViz()
{
  s_logger->repx(logga::dbug, "destructor call", "");
}

// ---------------------------------------------------------
//  MEMBER FUNCTION : operator()  (2 of 3)
// ---------------------------------------------------------
//  Description  : principal call for solver interface instances
//  Role         :
//  Signature    : SolverIf*
//  Techniques   :
//  Status       : complete
// ---------------------------------------------------------

void
GlpkViz::operator()
(const svif::SolverIf* si,
 const std::string     comment,
 const int             outputPrecision)
{
  s_logger->repx(logga::xtra, "entering function for raw pointer", "");

  // reset data members
  resetData();                               // utilty call

  // grab problem
  d_prob = si->d_prob;                       // this class has been granted friendship
  if ( !d_prob ) return;

  // store comment
  d_comment = comment;

  // set output precision for floats
  d_html << std::setprecision(outputPrecision);

  // stamp the time
  d_timestamp = timestamp();                 // utility call

  // main action
  prepareFilename();
  prepareTitle();
  createHtml();
  saveHtml(d_filename1);
  createHelp();
  saveHtml(d_filename2);
  displayHtml(d_filename1);

  // completion reporting
  s_logger->repx(logga::adhc, "leaving function for raw pointer", "");

  // finish
  return;
}

// ---------------------------------------------------------
//  MEMBER FUNCTION : operator()  (3 of 3)
// ---------------------------------------------------------
//  Description  : wrapper to the SolverIf* principal
//  Role         :
//  Signature    : taking shared_ptr<svif::SolverIf>
//  Techniques   : 'shared_ptr::get'
//  Status       : complete
//
//  Design notes
//
//      Simple wrapper to the raw pointer version.  The
//      shared_ptr 'get' member function returns a raw pointer to
//      its controlled resource.
//
// ---------------------------------------------------------

void
GlpkViz::operator()
(const shared_ptr<svif::SolverIf> si,        // solver interface object
 const std::string                comment,
 const int                        outputPrecision)
{
  s_logger->repx(logga::xtra, "entering wrapper for smart pointer", "");
  operator()(si.get(), comment, outputPrecision);
}

#endif // XE_GLPKVIZ_ALONE

// UTILITY FUNCTIONS

void
GlpkViz::raw
(const std::string line)
{
  d_html << line << "\n";                    // simple insertion
}

void
GlpkViz::rem
(const std::string comment)
{
  d_html << "<!-- " << comment << " -->" << "\n";
}

void
GlpkViz::title
(const std::string pageTitle)
{
  std::string preamble;
#if 0 // 1 = use, otherwise 0
  preamble = "GLPK viz : ";
#endif // 0
  if ( pageTitle.empty() )
    d_html << "<title>" << preamble << "unnamed" << "</title>" << "\n";
  else
    d_html << "<title>" << preamble << pageTitle << "</title>" << "\n";
}

void
GlpkViz::title
(const char* info)                           // wrapper
{
  if ( info == NULL )                        // GLPK function returned char* NULL
    {
      title("");
    }
  else
    {
      std::string temp(info);                // convert to string
      title(temp);                           // make underlying call
    }
}

void
GlpkViz::h1
(const std::string header)
{
  if ( header.empty() )
    d_html << "<h1>" << "(unnamed)" << "</h1>" << "\n";
  else
    d_html << "<h1>" << header << "</h1>" << "\n";
}

void
GlpkViz::h1
(const char* header)                         // wrapper
{
  if ( header == NULL )                      // GLPK function returned char* NULL
    {
      h1("");
    }
  else
    {
      std::string temp(header);              // convert to string
      h1(temp);                              // make underlying call
    }
}

void
GlpkViz::rule()
{
  d_html << "<hr noshade size=1 align=\"left\" width=\"80%\">" << "\n";
}

void
GlpkViz::p
(const std::string   copy,
 const Embellishment e)
{
  std::string buf;
  switch ( e )
    {
    case none:     buf = "";                                break;
    case helptext: buf = " class=\"helptext\"";             break;
    case endtext:  buf = " class=\"endtext\"";              break;
    default:
      std::clog << "** coding error 01 in source file " << __FILE__
                << ": given enum not supported on 'p' call: "
                << e
                << std::endl;
      break;
    }
  if ( copy.empty() )
    d_html << "<p" << buf << ">" << "&nbsp;" << "</p>" << "\n";
  else
    d_html << "\n" << "<p" << buf << ">" << "\n" << copy << "</p>" << "\n";
}

void
GlpkViz::p
(const char*         copy,                   // wrapper
 const Embellishment e)
{
  if ( copy == NULL )                        // GLPK function returned char* NULL
    {
      p("", e);
    }
  else
    {
      std::string temp(copy);                // convert to string
      p(temp, e);                            // make underlying call
    }
}

void
GlpkViz::p
(const Embellishment e)                      // wrapper
{
  p("", e);
}

void
GlpkViz::line()
{
  d_html << "\n";                            // note leading newline
  d_html << "<tr>" << "\n";
}

void
GlpkViz::cell                                // blank cell wrapper
(const Embellishment e)                      // defaults to 'none'
{
  cell("&nbsp;", e);                         // note non-breaking space character entity
}

void
GlpkViz::cell                                // specialization to protect from char* NULL
(const char*         label,                  // char* wrapper
 const Embellishment e)                      // defaults to 'none'
{
  if ( label == NULL )                       // GLPK function returned char* NULL
    {
#if 1 // 1 = highlight unnamed row or col, 0 = do not highlight / user-modifiable
      cell(error);                           // only char* NULL and not empty string
#else
      cell();
#endif // 0
    }
  else
    {
      std::string temp(label);               // convert to string
      cell(temp, e);                         // make underlying call
    }
}

template <typename T>
void
GlpkViz::cell                                // real cell
(const T             data,
 const Embellishment e)                      // defaults to 'none'
{
  std::string buf;
  switch ( e )
    {
    case none:    buf = "";                                 break;
    case narrow:  buf = " class=\"narrow\"";                break;
    case label_c: buf = " class=\"label_c\"";               break;
    case label_l: buf = " class=\"label_l\"";               break;
    case kind_iv: buf = " class=\"kind-iv\"";               break;
    case kind_bv: buf = " class=\"kind-bv\"";               break;
    case meta:    buf = " class=\"meta\"";                  break;
    case meta_2:  buf = " class=\"meta_2\" colspan=\"2\"";  break;
    case zero:    buf = " class=\"zero\"";                  break;
    case result:  buf = " class=\"result\"";                break;
    case goal:    buf = " class=\"goal\"";                  break;
    case optimal: buf = " class=\"optimal\"";               break;
    case extra:   buf = " class=\"extra\"";                 break;
    case left:    buf = " class=\"left\"";                  break;
    case right:   buf = " class=\"right\"";                 break;
    case span_2:  buf = " class=\"stub\" colspan=\"2\"";    break;
    case error:   buf = " class=\"error\"";                 break;
    default:
      std::clog << "** coding error 02 in source file " << __FILE__
                << ": given enum not supported on 'cell' call: "
                << e
                << std::endl;
      break;
    }
  d_html << "<td" << buf << ">" << data << "\n";
}

void
GlpkViz::li
(const std::string item)
{
  if ( ! item.empty() )
    d_html << "<li>" << item << "</li>" << "\n";
}

void
GlpkViz::resetData()
{
  d_html.str("");                            // empty the non-const string-stream
  d_comment.clear();
  d_filename1.clear();
  d_filename2.clear();
  d_title.clear();
  d_timestamp.clear();
}

void
GlpkViz::prepareFilename()
{
  // inputs : GLPK problem name = "prob name", PID = 1234, call count = 2
  // ouput  : prob_name-001234-02.html

  const std::string htmlExt = "html";

  // CAUTION: the padding routines coded here require a positive
  // integer, otherwise the following can result: 000-22.  The
  // Boost.Format library provides a more robust solution (or a
  // more sophisticated routine could be hand coded).

  const int pidpad = 6;                      // NOTE: will NOT truncate bigger ints
  pid_t pid = getpid();                      // see <unistd.h>
  std::ostringstream ossPid;
  ossPid << std::setfill('0') << std::setw(pidpad) << static_cast<int>(pid);
  std::string paddedPid = ossPid.str();

  const int filepad = 2;
  std::ostringstream ossCnt;
  ossCnt << std::setfill('0') << std::setw(filepad) << ++s_callCount;
  std::string paddedCnt = ossCnt.str();

  std::string buf;
  const char* probname = glp_get_prob_name(d_prob);
  if ( probname != NULL )
    buf += probname;
  else
    buf += "unnamed";                        // not named default
  buf += "-" + paddedPid;
  buf += "-" + paddedCnt;

  // clean up the filename stub
  std::replace(buf.begin(), buf.end(), ' ', '_');      // space to underscore
  std::replace(buf.begin(), buf.end(), '.', '_');      // dot to underscore

  d_filename1  = buf;
  d_filename1 += "." + s_htmlExt;;

  d_filename2  = buf;
  d_filename2 += "." + s_helpExt;
  d_filename2 += "." + s_htmlExt;;
}

void
GlpkViz::prepareTitle()
{
  // output: 00 : prob name

  std::string probName;
  const char* probname = glp_get_prob_name(d_prob);
  if ( probname == NULL )
    probName = "(unnamed)";
  else
    probName = probname;

  // CAUTION: seen note elsewhere about this padding routine

  std::ostringstream oss;
  oss << std::setfill('0') << std::setw(2) << s_callCount;
  std::string paddedCallCount = oss.str();

  d_title += paddedCallCount;
  d_title += " : ";
  d_title += probName;
}

void
GlpkViz::createHelp()
{
  d_html.str("");                            // reset out-string-stream

  // -- start html -----------------------------------------

  raw("<!DOCTYPE HTML PUBLIC \"-//W3C//DTD HTML 4.0 Transitional//EN\">");

  raw("");
  std::ostringstream ossGlpkVer; ossGlpkVer << glp_version();    // returns char*
  std::ostringstream ossFile; ossFile << __FILE__;
  rem("GLPK viz help screen");
  rem("HTML creation " + d_timestamp);
  rem("C++ class 'GlpkViz' from file '" + ossFile.str() + "'");
  rem("GNU GLPK version " + ossGlpkVer.str());

  raw("");
  raw("<html>");
  raw("<head>");

  raw("");
  raw("<meta http-equiv=\"content-type\" content=\"text/html; charset=iso-8859-1\">");
  raw("<meta name=\"generator\" content=\"GLPK viz - 2008\">");
  raw("<meta name=\"description\" content=\"GLPK problem and solution visualization\">");

  raw("");
  title("GLPK viz : help");

  // STYLE

  raw("");
  raw("<style type=\"text/css\">");
  raw("/* HTML 4 tags */");
  raw("body          {font-family: sans-serif}");
  raw("h1            {color: #6c6753}");               // warm charcoal
  raw("h1            {padding-left: 2.0em}");
  raw("ul            {list-style-type: square}");
  raw("li            {padding-top: 1.0ex}");
  raw("tt            {font-weight: bold}");
  raw("tt            {color: #d54400}");               // dark orange
  raw("/* CSS1 anchor tag pseudo-classes */");
  raw("a:link        {color: #ffa500}");               // orange
  raw("a:visited     {color: #708090}");               // slate
  raw("/* CSS 1 classes */");
  raw("p.helptext    {font-size: normal; padding-top: 2.0ex}");
  raw("</style>");

  raw("");
  raw("</head>");

  // BODY

  raw("<body>");
  raw("");

  h1("GLPK viz : help");

  p("<b>Display format</b>", helptext);

  raw("");
  raw("<ul>");
  li("GLPK viz displays the problem in 'convenient' rather than 'standard' \
form &mdash; that is, without explicit auxiliary variables");
  raw("</ul>");

  p("<b>Main table</b>", helptext);

  raw("");
  raw("<ul>");
  li("all values are floats but decimal points are only displayed when needed");
  li("missing or suspect entries are indicated with an orange box");
  li("the <b>caller is responsible</b> for ensuring that the correct solvers \
have run and that the solution is current");
  li("results from the interior point solver <tt>glp_interior</tt> cannot be displayed");
  li("the objective 'shift' coefficient is shown in the \
<font class=\"nowrap\">(OBJ, RHS)</font> cell");
  li("any unboundedness comments derive from <tt>glp_get_unbnd_ray</tt>");
  raw("</ul>");

  p("<b>Abbreviations</b>", helptext);

  raw("");
  raw("<ul>");
  li("con = continuous structural variable");
  li("OBJ = objective function");
  li("BNDS = col bounds");
  li("recosts = LP reduced costs (dual values)");
  li("nan = not-a-number (zero divide zero) float");
  li("inf = infinity (divide zero) float");
  raw("</ul>");

  p("<b>Printing</b>", helptext);

  raw("");
  raw("<ul>");
  li("activate your browser 'print background' option to retain colors when printing");
  raw("</ul>");

  p("<b>About GLPK viz</b>", helptext);

  raw("");
  raw("<ul>");
  li("GLPK viz was written by Robbie Morrison");
  li("GLPK viz is licensed under <font class=\"nowrap\">GNU GPLv3</font>");
  raw("</ul>");

  raw("");
  p();
  rule();
  p();

  raw("");
  raw("</body>");
  raw("</html>");

  // -- end html -------------------------------------------
}

void
GlpkViz::createHtml()
{
  d_html.str("");                            // reset out-string-stream

  const int cols    = glp_get_num_cols(d_prob);   // col count
  const int rows    = glp_get_num_rows(d_prob);   // row count
  const int nzs     = glp_get_num_nz(d_prob);     // non-zero coefficients count
  const int unbnded = glp_get_unbnd_ray(d_prob);  // 0 or range [1, rows+cols]

  // -- start html -----------------------------------------

  // HEAD

  raw("<!DOCTYPE HTML PUBLIC \"-//W3C//DTD HTML 4.0 Transitional//EN\">");

  raw("");
  std::ostringstream ossGlpkVer; ossGlpkVer << glp_version();    // returns char*
  std::ostringstream ossFile; ossFile << __FILE__;
  rem("file generated automatically by interrogating a GLPK 'glp_prob' problem instance");
  rem("HTML creation " + d_timestamp);
  rem("C++ class 'GlpkViz' from file '" + ossFile.str() + "'");
  rem("GNU GLPK version " + ossGlpkVer.str());

  raw("");
  raw("<html>");
  raw("<head>");

  raw("");
  raw("<meta http-equiv=\"content-type\" content=\"text/html; charset=iso-8859-1\">");
  raw("<meta name=\"generator\" content=\"GLPK viz - 2008\">");
  raw("<meta name=\"description\" content=\"GLPK problem and solution visualization\">");

  raw("");
  title(d_title);                            // in format: [GLPK viz :] 00 : problem name

  // STYLE

  raw("");
  raw("<style type=\"text/css\">");
  raw("/* HTML 4 tags */");
  raw("body          {font-family: sans-serif}");
  raw("h1            {color: #6c6753}");               // warm charcoal
  raw("h1            {padding-left: 2.0em}");
  raw("table         {}");
  raw("tr            {}");
  raw("td            {text-align: center; white-space: nowrap}");
  raw("td            {padding-left: 1.0em; padding-right: 1.0em}");
  raw("td            {border-style: solid; border-width: thin; border-color: white}");
#ifdef _DEBUG
  raw("td            {border-color: cyan}");           // very useful for testing
#endif // 0
  raw("/* CSS1 anchor tag pseudo-classes */");
  raw("a:link        {color: #ffa500}");               // orange
  raw("a:visited     {color: #708090}");               // slate
  raw("/* CSS 1 classes */");
  raw("p.helptext    {font-size: smaller; padding-top: 2.0ex}");
  raw("p.endtext     {font-size: 65%; white-space: nowrap}");
  raw("font.nobr     {white-space: nowrap}");
  raw("table.first   {border-style: solid; border-width: thin; border-color: #cccccc}");
  raw("table.first   {padding: 10px}");
  raw("table.second  {font-size: smaller; padding-top: 0.0ex; padding-bottom: 2.0ex}");
  raw("td.narrow     {padding: 0.0em;}");
  raw("td.label_c    {font-size: smaller}");
  raw("td.label_l    {font-size: smaller; text-align: left}");
  raw("td.meta       {background-color: #c8beb7}");    // warm gray
  raw("td.meta_2     {background-color: #c8beb7}");    // warm gray
  raw("td.zero       {background-color: #f2f2f2}");    // 5% gray
  raw("td.kind-bv    {background-color: #ccff00}");    // lime green
  raw("td.kind-iv    {background-color: #9ACD32}");    // mid-green
  raw("td.result     {background-color: #fbec5d}");    // gold
  raw("td.goal       {background-color: #6c6753; color: white; font-weight: bold}");
  raw("td.optimal    {background-color: #ff9955}");    // orange
  raw("td.extra      {background-color: #dde9af}");    // light green
  raw("td.left       {text-align: left}");
  raw("td.right      {text-align: right}");
  raw("td.stub       {text-align: left; font-weight: bold; padding-top: 2.0em}");
  raw("td.error      {border-color: #ffa500}");        // orange
  raw("</style>");

  raw("");
  raw("</head>");

  // BODY

  raw("<body>");
  raw("");
  h1(d_title);

  // TABLE ONE -- main data

  // start table
  raw("");
  raw("<table class=\"first\" summary=\"GLPK problem visualization\">");

  // optimization sense, col labels
  line();
  switch ( glp_get_obj_dir(d_prob) )         // optimization sense
    {
    case GLP_MAX: cell("maximize", goal); break;
    case GLP_MIN: cell("minimize", goal); break;
    default: cell(error); break;
    }
  for (int col = 1; col <= cols; col++)
    {
      cell(glp_get_col_name(d_prob, col), label_c);    // col name
    }
  cell();
  cell();
  cell(narrow);
  cell();
  cell();
  cell();
  std::ostringstream ossHelp;
  ossHelp << "<a href=\"" << d_filename2 << "\">help</a>";
  cell();
  cell(ossHelp.str());

  // col headers C1 to RHS
  line();
  std::ostringstream ossRxC1;
  ossRxC1 << rows << " &times; " << cols;
  cell(ossRxC1.str()); //  cell(glp_get_prob_name(d_prob), label_c);
  for (int col = 1; col <= cols; col++)
    {
      std::ostringstream osscol;
      osscol << "C" << col;
      cell(osscol.str(), meta);
    }
  cell("RHS", meta_2);
  cell(narrow);
  cell("LP solution", label_c);
  cell("MIP solution", label_c);
  cell("LP recosts", label_c);
  cell();
  cell();

  // col kinds
  line();
  cell(nzs);
  for (int col = 1; col <= cols; col++)
    {
      switch ( glp_get_col_kind(d_prob, col) )
        {
        case GLP_CV: cell("con");           break;
        case GLP_IV: cell("int", kind_iv);  break;
        case GLP_BV: cell("bin", kind_bv);  break;
        }
    }
  cell();
  cell("&mdash;");
  cell(narrow);
  cell();
  cell();
  cell("&larr; col kinds", label_c);
  cell();
  cell();

  // objective function
  line();
  cell("OBJ", meta);
  for (int col = 1; col <= cols; col++)
    {
      double objcoef = glp_get_obj_coef(d_prob, col);  // objective function coefficient
      if ( objcoef == 0.0 )
        cell(objcoef, error);
      else
        cell(objcoef);
    }
  cell();
  cell(glp_get_obj_coef(d_prob, 0));         // objective function 'shift' at index 0
  cell(narrow);
  cell(glp_get_obj_val(d_prob), goal);       // LP Z value
  cell(glp_mip_obj_val(d_prob), goal);       // MIP Z value
  cell("&larr; Z&nbsp;values", label_c);
  cell(glp_get_obj_name(d_prob), label_l);   // objective function name
  cell();

  // constraint matrix
  for (int row = 1; row <= rows; row++)
    {
      line();
      std::ostringstream ossrow;
      ossrow << "R" << row;
      cell(ossrow.str(), meta);
      for (int col = 1; col <= cols; col++)
        {
          double coeff = getConCoef(row, col);
          if ( coeff == 0.0 )
            {
              // select the treatment of cells containing zero / user-modifiable
              // cell()                      // do nothing
              // cell("&ndash");             // n-dash
              cell(zero);                    // CSS class 'td.zero'
            }
          else
            {
              cell(coeff);
            }
        }
      std::ostringstream ossrhs;
      std::string space = "&nbsp;&nbsp;";
      switch ( glp_get_row_type(d_prob, row) )
        {
        case GLP_LO:                         // glyph for >=
          cell("&ge;");
          cell(glp_get_row_lb(d_prob, row));
          break;
        case GLP_UP:                         // glyph for <=
          cell("&le;");
          cell(glp_get_row_ub(d_prob, row));
          break;
        case GLP_FX:                         // glyph for =
          cell("=");
          cell(glp_get_row_lb(d_prob, row)); // lower needed
          break;
        case GLP_FR:
          cell("free", error);
          cell();
          break;
        case GLP_DB:
          cell("[&nbps;]");
          ossrhs << glp_get_row_lb(d_prob, row)   // lower first
                 << space
                 << glp_get_row_ub(d_prob, row) ;
          cell(ossrhs.str());
          break;
        default:
          cell(error);
          break;
        }
      if ( row == unbnded )                  // uses 'glp_get_unbnd_ray'
        {
          cell("unboundedness", error);
        }
      else
        {
          cell(narrow);
        }
      cell(glp_get_row_prim(d_prob, row), result);     // LP solution
      cell(glp_mip_row_val(d_prob, row), result);      // MIP solution
      cell(glp_get_row_dual(d_prob, row), extra);      // LP reduced costs
      cell(glp_get_row_name(d_prob, row), label_l);    // row label
      cell(ossrow.str(), label_l);
    }

  // variable bounds
  line();
  cell("BNDS", meta);
  for (int col = 1; col <= cols; col++)
    {
      std::string space = "&nbsp;&nbsp;";
      std::ostringstream oss;
      switch ( glp_get_col_type(d_prob, col) )
        {
#if 1 // 1 is [] range style, 0 is < comparison style / user-modifiable
        case GLP_LO:
          oss << "["
              << glp_get_col_lb(d_prob, col)
              << ",&nbsp;"
              << "&#8734;"                   // infinity
              << "]";
          cell(oss.str());
          break;
        case GLP_UP:
          oss << "["
              << "&minus;&#8734;"            // minus infinity
              << ",&nbsp;"
              << glp_get_col_ub(d_prob, col)
              << "]";
          cell(oss.str());
          break;
        case GLP_FX:
          oss << "["
              << glp_get_col_lb(d_prob, col)
              << ",&nbsp;"
              << glp_get_col_ub(d_prob, col)
              << "]";
          cell(oss.str(), error);
          break;
        case GLP_FR:
          cell("free", error);
          break;
        case GLP_DB:
          oss << "["
              << glp_get_col_lb(d_prob, col) // lower first
              << ",&nbsp;"
              << glp_get_col_ub(d_prob, col)
              << "]";
          cell(oss.str());
          break;
#else
        case GLP_LO:                         // glyph for >=
          oss << "&ge;" << space << glp_get_col_lb(d_prob, col);
          cell(oss.str());
          break;
        case GLP_UP:                         // glyph for <=
          oss << "&le;" << space << glp_get_col_ub(d_prob, col);
          cell(oss.str());
          break;
        case GLP_FX:                         // glyph for =
          oss << "=" << space << glp_get_col_lb(d_prob, col);
          cell(oss.str(), error);
          break;
        case GLP_FR:
          cell("free", error);
          break;
        case GLP_DB:
          oss << glp_get_col_lb(d_prob, col) << space  // lower first
              << "&le;" << space
              << glp_get_col_ub(d_prob, col);
          cell(oss.str());
          break;
#endif // 0
        default:
          cell(error);
          break;
        }
    }
  cell();
  cell("&mdash;");
  cell(narrow);
  cell();
  cell();
  cell();
  cell();
  cell();

  // often row
  line();
  cell();
  for (int col = 1; col <= cols; col++)
    {
      if ( col == (unbnded - rows) )         // uses 'glp_get_unbnd_ray'
        {
          cell("unboundness", error);
        }
      else
        {
          cell();
        }
    }
  cell();
  cell();
  cell(narrow);
  cell();
  cell();
  cell();
  cell();
  cell();

  // results / linear
  line();
  cell("LP solution", label_c);
  for (int col = 1; col <= cols; col++)
    {
      cell(glp_get_col_prim(d_prob, col), result);     // structural variable value
    }
  cell();
  cell();
  cell(narrow);
  switch ( glp_get_status(d_prob) )
    {
    case GLP_OPT:    cell("LP optimal", optimal);      break;    // optimal
    case GLP_FEAS:   cell("LP feasible");              break;    // feasible
    case GLP_INFEAS: cell("LP INFEAS", error);         break;    // solution infeasible
    case GLP_NOFEAS: cell("LP NOFEAS", error);         break;    // proven not feasible
    case GLP_UNBND:  cell("LP UNBND",  error);         break;    // unbounded
    case GLP_UNDEF:  cell("LP undefined",  error);     break;    // probably not run
    default: cell(glp_get_status(d_prob), error);      break;
    }
  cell();
  cell();
  cell();
  cell();

  // results / non-linear
  line();
  cell("MIP solution", label_c);
  for (int col = 1; col <= cols; col++)
    {
      cell(glp_mip_col_val(d_prob, col), result); // structural variable value
    }
  cell();
  cell();
  cell(narrow);
  cell();
  switch ( glp_mip_status(d_prob) )
    {
    case GLP_OPT:    cell("MIP optimal", optimal);     break;    // optimal
    case GLP_FEAS:   cell("MIP feasible");             break;    // feasible
    case GLP_NOFEAS: cell("MIP NOFEAS", error);        break;    // proven not feasible
    case GLP_UNDEF:  cell("MIP undefined",  error);    break;    // probably not run
    default: cell(glp_mip_status(d_prob), error);      break;
    }
  cell();
  cell();
  cell();

  // results / shadow
  line();
  cell("LP recosts", label_c);
  for (int col = 1; col <= cols; col++)
    {
      cell(glp_get_col_dual(d_prob, col), extra); // LP reduced cost
    }
  cell();
  cell();
  cell(narrow);
  cell();
  cell();
  cell();
  cell();
  cell();

  // col labels again
  line();
  cell();
  for (int col = 1; col <= cols; col++)
    {
      cell(glp_get_col_name(d_prob, col), label_c);    // col name
    }
  cell();
  cell();
  cell(narrow);
  cell();
  cell();
  cell();
  cell();
  cell();

  // col headers C1 to Ccols
  line();
  cell();
  for (int col = 1; col <= cols; col++)
    {
      std::ostringstream osscol;
      osscol << "C" << col;
      cell(osscol.str(), label_c);
    }
  cell();
  cell();
  cell(narrow);
  cell();
  cell();
  cell();
  cell();
  cell();

  // complete the table
  raw("");
  raw("</table>");

  // TABLE TWO -- further information

  raw("");
  raw("<table class=\"second\" summary=\"GLPK problem further information\">");

  line();
  cell("Additional GLPK details", span_2);   // only one cell is correct

  line();
  cell("simplex iterations", right);
  // cell(lpx_get_int_parm(d_prob, LPX_K_ITCNT), left);    // lpx is correct
  cell(0, left);                                           // disabled

  line();
  cell("problem name", right);
  cell(glp_get_prob_name(d_prob), left);

  std::stringstream ossPtr; ossPtr << d_prob;
  line();
  cell("pointer address", right);
  cell(ossPtr.str(), left);

  // std::string rounding;
  // switch ( lpx_get_int_parm(d_prob, LPX_K_ROUND) )
  //   {
  //   case 0: rounding = "no";  break;
  //   case 1: rounding = "yes"; break;
  //   }
  line();
  // cell("close&ndash;to&ndash;zero rounding (LPX_K_ROUND)", right);
  cell("close&ndash;to&ndash;zero rounding", right);
  cell("no", left);

  std::ostringstream ossSize;
  ossSize << rows << " &times; " << cols;
  line();
  cell("constraint matrix (rows x cols)", right);
  cell(ossSize.str(), left);

  line();
  cell("non-zero coefficients", right);
  cell(nzs, left);

  double ratio1 =
    static_cast<double>(nzs)/static_cast<double>(rows);
  line();
  cell("non-zero coefficients/rows", right);
  cell(ratio1, left);

  double ratio2
    = static_cast<double>(nzs)/(static_cast<double>(rows) * static_cast<double>(cols));
  line();
  cell("non-zero coefficients/all coefficients", right);
  cell(ratio2, left);

  line();
  cell("GLPK version", right);
  cell(glp_version(), left);

  line();
  cell("HTML creation information", span_2);

  line();
  cell("associated comment", right);
  if ( !d_comment.empty() )
    cell(d_comment, left);
  else
    cell("(none)", left);

  line();
  cell("output (std::ostream) precision", right);
  cell(d_html.precision(), left);

  line();
  cell("saved filename (name + PID + 00)", right);
  cell(d_filename1, left);

  line();
  cell("HTML creation", right);
  cell(d_timestamp, left);

  raw("");
  raw("</table>");

  // FINAL RULE AND COMMENT

  raw("");
  rule();
  std::string final;
  final += "Machine generated HTML&nbsp; &bull;&nbsp; GLPK viz&nbsp; &bull;&nbsp; 2008";
  p(final, endtext);

  raw("</body>");
  raw("</html>");

  // -- end html -------------------------------------------

  return;
}

std::string
GlpkViz::timestamp()
{
  // option A produces: 2008-Jun-02 15:28:46 UTC
  // NOTE: C rather that Boost time functions were chosen for portability
  const size_t MAX = 1024;                   // define buffer length
  const time_t now = time(0);                // seconds since 1970
  const tm* utc    = gmtime(&now);           // convert to tm struct in UTC time
  char timeStamp[MAX];

#if 1 // 0 = option B, 1 = option A / user-modifiable
  strftime(timeStamp, MAX, "%Y-%b-%d %H:%M:%S UTC", utc);
#else
  strftime(timeStamp, MAX, "%Y-%m-%d %H%:M:%S UTC", utc);   // ISO 8601
#endif // 0

  std::string buff(timeStamp);               // convert to std::string
  return buff;
}

double
GlpkViz::getConCoef
(int row,
 int col)
{
  // because there is no 'double glp_get_mat_val(glp_prob* lp, int i, int j)'

  int count = glp_get_mat_row(d_prob, row, NULL, NULL);     // non-zeros in given row

  const int offset = 1;                      // GLPK does not use index zero
  std::vector<int>    index(count + offset); // a 'reserve' call is not sufficient
  std::vector<double> value(count + offset);

  // Josuttis (1999 p155) writes: "whenever you need an array of
  // type T for any reason (such as for an existing C library)
  // you can use a std::vector<T> and pass it the address of the
  // first element".  Namely &vec[0].  The reason this works is
  // that std::vector elements, like C array elements, are
  // required to be held in contiguous memory.
  //
  //    Josuttis, Nicolai M.  1999.  The C++ Standard Library :
  //      a tutorial and reference.  Addison-Wesley, Boston, USA.
  //      ISBN 0-201-37926-0.

  glp_get_mat_row(d_prob, row, &index[0], &value[0]);

  // the following code is very STL-ish

  std::vector<int>::iterator        pos;
  std::vector<int>::difference_type idx;     // probably typedef'ed to int
  pos = std::find(index.begin(), index.end(), col);
  if ( pos != index.end() )                  // 'col' was found
    {
      idx = std::distance(index.begin(), pos);
      return value.at(idx);
    }
  else
    {
      return 0.0;
    }
}

void
GlpkViz::saveHtml
(const std::string filename)
{
  // write to file using truncate mode
  std::ofstream ofile(filename.c_str(), std::ios::out|std::ios::trunc);
  if ( !ofile )
    {
      std::clog << "** std::ofstream failed : " << filename << std::endl;
      return;
    }
  ofile << d_html.str();
  ofile.close();

#if   (XE_GLPKVIZ_ALONE == 0)                // not stand-alone
  xeona::readonly(filename);                 // optional call, defined in unit 'c/util4'
#endif // XE_GLPKVIZ_ALONE

}

void
GlpkViz::displayHtml
(const std::string filename)
{
  // ---------------------------------------------------------
  //
  //  The following comments are based on the 'firefox'
  //  web-browser for Linux -- versions 2.0.0.13 and 3.6.6.
  //
  //  CAUTION: start 'firefox' first!
  //
  //  I tried 'firefox' options: (none), -safe-mode, and
  //  -no-remote on a dummy first call basis and nothing worked.
  //
  //  The 'firefox' warning dialog box says:
  //
  //     "Firefox is already running, but is not responding. To
  //     open a new window, you must first close the existing
  //     Firefox process, or restart your system."
  //
  //  The following made no difference also:
  //
  //      if ( s_callCount > 1 ) sleep(5);
  //
  //  I also experimented with the 'fork' function, but exiting
  //  cleanly is a problem.  And 'eval'ing under 'system':
  //
  //      eval "firefox sample-003442-01.html &"
  //
  // ---------------------------------------------------------

  // a one second delay means that the HTML pages open in order
  // (a distinct advantage) -- tested using a circa 2010 4-core
  // Intel Core i5 laptop

  const int delay = 1;                       // zero to disable
  if ( delay > 0 )
    {
      if ( s_callCount > 1 )
        {

#if (XE_GLPKVIZ_ALONE == 0)                // 'xeona'
          s_logger->repx(logga::adhc, "adding delay (to ensure HTML order)", delay);
#endif

          sleep(delay);
        }
    }

#if _XUTEST // 1 = disable browser call, 0 = enable
  return;
#endif // 0

  std::string browserCall;
  browserCall = d_browser + " \"" + filename + "\" &";      // filename set in soft quotes

#ifdef _DEBUG
  std::cout << "shell call : " << browserCall << std::endl;
#endif // 0

  int ret = system(browserCall.c_str());            // see <cstdlib>
  if ( ret != 0 )
    {
      std::clog << "** shell call failed : " << browserCall << std::endl;
    }
  return;
}

#undef _DEBUG // file local debug macro

//  end of file

