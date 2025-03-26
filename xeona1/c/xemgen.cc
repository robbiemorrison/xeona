//  XEONA ENERGY SYSTEMS MODELING ENVIRONMENT
//
//  file-create-name : xemgen.cc
//  file-create-date : Mon 11-Aug-2008 14:46 UTC
//  file-initiator   : Robbie Morrison <robbie@actrix.co.nz>
//
//  file-role        : class to generate well-formatted XEM models / implementation
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
//  $URL: file:///home/robbie/svn-root/xeona/futz/trunk/xeona1/c/xemgen.cc $

//  LOCAL AND SYSTEM INCLUDES

#include "xemgen.h"           // companion header for this file (place first)

#include "../a/logger.h"      // standard logging functionality (as required)
#include "../c/smart_ptr.h"   // toggle between Boost and TR1 smart pointers
#include ".././common.h"      // common definitions for project (place last)

#include <sstream>            // string-streams
#include <string>             // C++ strings
#include <vector>             // STL sequence container

#include <boost/algorithm/string.hpp>   // string recasing, trimming, splitting
#include <boost/foreach.hpp>            // BOOST_FOREACH iteration macro
#include <boost/format.hpp>             // printf style formatting

//  CODE

// ---------------------------------------------------------
//  CLASS           : XemGenerator
// ---------------------------------------------------------
//  Description  : generate well-formatted XEM model data
//  Role         : used by '--inbuilt' and various model testers
//  Techniques   : Boost.Format library
//  Status       : complete
// ---------------------------------------------------------

// STATIC DEFINITIONS

logga::spLogger XemGenerator::s_logger = logga::ptrLogStream();  // bind logger

// CREATORS

XemGenerator::XemGenerator
(const int tab) :                            // note default in header
  d_last(e_unknown),
  d_tab(tab),
  d_oss()
{
  s_logger->repx(logga::xtra, "constructor call, d_tab", d_tab);
}

XemGenerator::~XemGenerator()
{
  s_logger->repx(logga::adhc, "destructor call", "");
}

// RECORD LEVEL CALLS

void
XemGenerator::entity
(const std::string klass,
 const std::string identifier,
 const std::string preamble)                 // defaults to "entity"
{
  d_oss << "\n";                             // always a blank line above
  d_oss << preamble << "." << identifier << "\n";
  d_last = e_record;
  in("class", klass);
  d_last = e_record;                         // override the value set by 'in'
}

void
XemGenerator::special
(const std::string special)
{
  d_oss << "\n";                             // always a blank line above
  d_oss << special << "\n";
  d_last = e_record;
}

void
XemGenerator::note()                         // usually followed by comment calls
{
  special("note");
}

// FIELD LEVEL CALLS

void
XemGenerator::in
(const std::string key,
 const std::string value)
{
  if ( d_last != e_input ) d_oss << "\n";
  d_last = e_input;
  field(key, ">", value);
}

void
XemGenerator::out
(const std::string key,
 const std::string value)
{
  if ( d_last != e_output ) d_oss << "\n";
  d_last = e_output;
  field(key, "<", value);
}

void
XemGenerator::inq
(const std::string key,
 const std::string quote)                          // quote as single string
{
  in(key, "\"" + quote + "\"");
}

void
XemGenerator::outq
(const std::string key,
 const std::string quote)
{
  out(key, "\"" + quote + "\"");
}

void
XemGenerator::inQ                            // multi-quote
(const std::string key,
 const std::string quote)                    // quote as timeseries string
{
  std::string buffer;
  std::vector<std::string> split;
  boost::split(split, quote, boost::is_any_of(" "), boost::token_compress_on);
  BOOST_FOREACH( std::string s, split )
    buffer += " \"" + s + "\"";                        // concatenate with quotes
  if ( ! buffer.empty() ) buffer = buffer.substr(1);   // chomp the first character
  in(key, buffer);                                     // secondary call
}

void
XemGenerator::outQ                           // multi-quote
(const std::string key,
 const std::string quote)                          // quote as timeseries string
{
  std::string buffer;
  std::vector<std::string> split;
  boost::split(split, quote, boost::is_any_of(" "), boost::token_compress_on);
  BOOST_FOREACH( std::string s, split )
    buffer += " \"" + s + "\"";                        // concatenate with quotes
  if ( ! buffer.empty() ) buffer = buffer.substr(1);   // chomp the first character
  out(key, buffer);                                    // secondary call
}

// PACKAGED CALLS

void
XemGenerator::horizon
(const int steps,
 const int interval)                         // note default in header
{
  entity("TimeHorizon" , xeona::timehorizon); // defined in 'common.cc'
  com("the TimeHorizon entity is REQUIRED and the");
  com("'time-horizon' identifier is MANDATORY");
  out("builtin-remark s", "");
  in("steps [-] i"     , boost::str(boost::format("%d") % steps));
  in("interval [s] i"  , boost::str(boost::format("%d") % interval));
  in("start-hour [-] i", "0");
  in("start-day [-] i" , "1");
  in("leap-year [-] b" , "0" );
  com("the interval [300,86400] is further restricted to");
  com("common multiples of one hour ranging 5 mins to 24 hours");
  com();
  com("the start-hour begins midnight local time and ranges");
  com("[0,23] and the start-day begins 01-Jan and ranges");
  com("[1,365] -- used to align internal and model timeseries");
  com("when the latter commences after 00:00 on 01 January");
  com();
  com("when leap-year is true then 29 February is presumed to");
  com("exist (although not all entities will support this)");
  inq("hemisphere s"   , "N");
  com("the hemisphere is {N,S} for north and south");
  com();
  com("the modeler should ensuring that timeseries data given");
  com("elsewhere aligns with the specification given here");
  com();
  hed("b/builtins.h");
}

void
XemGenerator::overseer()
{
  entity("Overseer", xeona::overseer);
  com("the Overseer entity is REQUIRED and the 'overseer'");
  com("identifier is MANDATORY");
  com();
  com("the overseer does little more that invoke the various");
  com("originating domains in nominated order at each new");
  com("interval");
  out("builtin-remark s", "");
  inq("captrans-algorithm s","simple");
  com("captrans-algorithm takes 'fixed' | 'simple' | 'hop-relit'");
  com("but only 'simple' is currently implemented (this call");
  com("contains experimental macro-controlled hop-relit code)");
#if 1 // domain controllers list
  inq("ranked-orig-domains L", "domain-controller-a");
#else
 inq("ranked-orig-domains L", "");
#endif // 0
  com("the originating domain controllers must be given in");
  com("order of DESCENDING priority, any unannounced domains");
  com("will be discovered naturally during the various");
  com("traversals -- an originating domain must contain at");
  com("least one source entity");
  out("total-financial [$] f"             , "0.0");
  out("total-greenhouse [kg] f"           , "0.0");
  out("total-nox [kg] f"                  , "0.0");
  out("total-depletion [J] f"             , "0.0");
  out("total-landuse [m2] f"              , "0.0");
  com();
  out("total-shortrun-financial [$] f"    , "0.0");
  out("total-shortrun-greenhouse [kg] f"  , "0.0");
  out("total-shortrun-nox [kg] f"         , "0.0");
  out("total-shortrun-depletion [J] f"    , "0.0");
  out("total-shortrun-landuse [m2] f"     , "0.0");
  com("the cost-type totals cover the entire horizon, with");
  com("first step truncation given by program.last-run.run-kind");
  out("variable-costs-financial [$] F"    , "0.0 ..");
  out("fixed-costs-financial [$] F"       , "0.0 ..");
  out("embedded-costs-financial [$] F"    , "0.0 ..");
  out("variable-costs-greenhouse [kg] F"  , "0.0 ..");
  out("fixed-costs-greenhouse [kg] F"     , "0.0 ..");
  out("embedded-costs-greenhouse [kg] F"  , "0.0 ..");
  out("variable-costs-nox [kg] F"         , "0.0 ..");
  out("fixed-costs-nox [kg] F"            , "0.0 ..");
  out("embedded-costs-nox [kg] F"         , "0.0 ..");
  out("variable-costs-depletion [J] F"    , "0.0 ..");
  out("fixed-costs-depletion [J] F"       , "0.0 ..");
  out("embedded-costs-depletion [J] F"    , "0.0 ..");
  out("variable-costs-landuse [m2] F"     , "0.0 ..");
  out("fixed-costs-landuse [m2] F"        , "0.0 ..");
  out("embedded-costs-landuse [m2] F"     , "0.0 ..");
  hed("b/overseer.h");
}

void
XemGenerator::domainController
(const std::string domconId)                 // note the default value
{
  entity("DomainController", domconId);
  com("a domain controller entity (the only one provided)");
  com("which can take one of a number of commitment modes --");
  com("but REQUIRES that the managed entities support the");
  com("elected mode");
  out("builtin-remark s", "");
  inq("domain-role s", "domain role");
  com();
  in("init-scale-problem b"               , "1");
  in("init-use-advanced-initial-basis b"  , "1");
  in("init-use-simplex-presolver b"       , "1");
  in("init-use-mip-presolver b"           , "1");
  in("init-apply-numerical-zero b"        , "1");
  com("these five GLPK solver behavior settings should");
  com("normally be set to true unless run-time tests indicate");
  com("otherwise -- numerical zero rounding applies to both");
  com("input coefficients and output values");
  in("trip-kkt-report-level i"            , "1");
  in("trip-coeff-span-level i"            , "1");
  com("trip-kkt-report-level in { 0 1 2 3 } for no check, then");
  com("report awful, close, and every, trip-coeff-span-level");
  com("in { 0 1 2 } for no check, then report bad and every");
  com("min and max abs coeff from original matrix");
  inq("commitment-mode s", "fin");
  com("supported commitment-mode values (lmp is nodal pricing):");
  com("fin | ghg | nox | dep | luc | lmp | merit | first");
#if 0 // gateways list
  inQ("ranked-selgates L", "gate-1");
#else
  inq("ranked-selgates L", "");
#endif // 0
  com("the ranked-selgates (bridging the right side) must be");
  com("null, one, or listed in DESCENDING priority");
#if 0 // asset operators list
  inQ("asset-operators L", "asop-1");
#else
  inq("asset-operators L", "");
#endif // 0
  com("the asset-operators may be null, individual, or listed");
  com("in no particular order");
#if 0 // demand junctions list
  inQ("demand-junctions L", "teas-demand-2-split-0");
#else
  inq("demand-junctions L", "");
#endif // 0
  com("the demand-junctions, which split and join demand, may");
  com("be null, individual, or listed in no particular order");
  out("variable-costs-financial [$] F"  , "0.0 ..");
  out("fixed-costs-financial [$] F"     , "0.0 ..");
  // new subtotals
  com("the following are consolidations for the entire domain");
  out("subtotal-financial [$] f",   "0.0");
  out("subtotal-greenhouse [kg] f", "0.0");
  out("subtotal-nox [kg] f",        "0.0");
  out("subtotal-depletion [J] f",   "0.0");
  out("subtotal-landuse [m2] f",    "0.0");
  // header
  hed("b/domcon.h");
}

void
XemGenerator::emacs()
{
  std::string fill(xeona::modelFieldIndent, ' ');
  const int fieldIndent = xeona::modelFieldIndent;
  const int angleIndent = fieldIndent + d_tab + 1;     // plus one as field indent is 4
  const int finalIndent = angleIndent + 2;             // plus two for aesthetics
  std::ostringstream otabs;
  otabs << "(" << boost::format("%02d") % fieldIndent
        << " " << boost::format("%02d") % angleIndent
        << " " << boost::format("%02d") % finalIndent
        << ")";
  if ( d_last == e_record ) d_oss << "\n";
  d_last = e_comment;
  d_oss << fill << "useful emacs text editor settings"    << "\n"
        << fill << "local variables:"                     << "\n"
        << fill << "  mode: xem"                          << "\n"
        << fill << "  tab-stop-list: " << otabs.str()     << "\n"
        << fill << "  truncate-lines: t"                  << "\n"
        << fill << "end:"                                 << "\n";
}

// OTHER CALLS

void
XemGenerator::com()
{
  if ( d_last == e_comment ) d_oss << "\n";  // else do nothing
}

void
XemGenerator::com
(const std::string comment,
 const unsigned    pad)
{
  if ( d_last != e_comment ) d_oss << "\n";
  d_last = e_comment;
  d_oss << std::string(pad, ' ') << comment << "\n";
}

void
XemGenerator::hed
(const std::string header,
 const unsigned    pad)
{
  if ( d_last != e_comment ) d_oss << "\n";
  d_last = e_comment;
  d_oss << std::string(pad, ' ') << "header: " << header << "\n";
}

void
XemGenerator::ident                          // "Revision" to "    $Revision: 8750 $"
(const std::string key,
 const unsigned    pad)
{
  if ( d_last != e_ident ) d_oss << "\n";
  d_last = e_ident;
  d_oss << std::string(pad, ' ') << "$" << key << ": $" << "\n";
}

void
XemGenerator::meta                           // "role" to "xem-role:"
(const std::string key,
 const unsigned    pad)
{
  const int metalen = 6;                     // adjust as required
  if ( d_last != e_meta ) d_oss << "\n";
  d_last = e_meta;
  d_oss << std::string(pad, ' ');
  d_oss << "xem-" << std::setw(metalen) << std::left <<  key << " :" << "\n";
}

void
XemGenerator::verbatim
(const std::string verbatim)
{
  if ( d_last != e_verbatim ) d_oss << "\n";
  d_last = e_verbatim;
  d_oss << verbatim << "\n";
}

void
XemGenerator::rule
(const std::string header)
{
  if ( d_last != e_rule ) d_oss << "\n";
  d_last = e_rule;
  const int space = 12;
  const int dashs = d_tab + space - header.length();
  d_oss << "  " << std::string(dashs, '-') << " " << header << "\n";
}

void
XemGenerator::dule
(const std::string tag,
 const std::string remark)
{
  // produces "  ----------- DOMAIN A ------------------------------ remark"

  if ( d_last != e_rule ) d_oss << "\n";
  d_last = e_rule;
  const std::string label = "DOMAIN " + tag;
  const int lead  = 11;                      // number of lead-in dashes
  d_oss << "  " << std::string(lead, '-') << " " << label << " ";
  if ( remark.empty() )
    {
      int dashs = d_tab + 11 - lead - label.length();
      if ( dashs < 0 ) dashs = 2;
      d_oss << std::string(dashs, '-') << "\n";
    }
  else
    {
      int dashs = d_tab + 11 - lead - label.length() - 1 - remark.length();
      if ( dashs < 0 ) dashs = 2;
      d_oss << std::string(dashs, '-') << " " << remark << "\n";
    }
}

void
XemGenerator::end()
{
  d_oss << "\n";
  d_oss << xeona::modelEndMarker << "\n";
  d_last = e_record;
}

void
XemGenerator::blanks
(const unsigned count)
{
  for ( unsigned int i = 0; i < count; ++i )
    {
      d_oss << "\n";
    }
}

// OUTPUT

void
XemGenerator::print
(std::ostream& os)
{
  os << d_oss.str()
     << std::flush;

  d_oss.str("");                             // purge local buffer
  d_last = e_unknown;                        // reset to unknown
}

std::string
XemGenerator::string()
{
  return d_oss.str();
}

// UTILITY FUNCTIONS

void
XemGenerator::field
(const std::string key,
 const std::string angle,
 const std::string value)
{
  std::ostringstream fmtss;                  // format string with embedded tab stop
  fmtss << "%|4t|%1% %|" << d_tab << "t|%2%";
  d_oss << boost::format(fmtss.str()) % key % angle;
  if ( ! value.empty() ) d_oss << " " << value;
  d_oss << "\n";
}

//  end of file

