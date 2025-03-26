//  XEONA ENERGY SYSTEMS MODELING ENVIRONMENT
//
//  file-create-name : recorder.cc
//  file-create-date : Mon 26-Apr-2010 12:10 UTC
//  file-initiator   : Robbie Morrison <robbie@actrix.co.nz>
//
//  file-role        : summarizing recorder class / implementation
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
//  $URL: file:///home/robbie/svn-root/xeona/futz/trunk/xeona1/a/recorder.cc $
//
//  GENERAL NOTES FOR THIS FILE

//  LOCAL AND SYSTEM INCLUDES

#include "recorder.h"         // companion header for this file (place first)

#ifndef _XUTEST               // omit for unit testing
# include "../b/domcon.h"     // domain controller entity
# include "../b/gate.h"       // gateway entity
#endif // _XUTEST

#include "../a/logger.h"      // standard logging functionality (as required)
#include "../c/smart_ptr.h"   // toggle between Boost and TR1 smart pointers
#include ".././common.h"      // common definitions for project (place last)

#include <string>             // C++ strings
#include <sstream>            // string-streams

#include <boost/format.hpp>   // printf style formatting

//  CODE

#ifdef _XUTEST                // include for unit testing (declarations in header file)

class DomainController { public: std::string getIdAndKind(); };
class Gateway          { public: std::string getIdAndKind(); };

std::string DomainController::getIdAndKind(){ return "domcon-ut-1"; }
std::string Gateway::getIdAndKind(){ return "gate--ut-1"; }

#endif // _XUTEST

// ---------------------------------------------------------
//  CLASS           : EventRecorder::Event (nested class)
// ---------------------------------------------------------

EventRecorder::Event::Event
(const std::string& event,
 const std::string& details,
 const std::string& remark) :
  d_alert(false),
  d_event(event),
  d_details(details),
  d_remark(remark)
{
}

void
EventRecorder::Event::addAlert
(const std::string& remark)
{
  d_alert = true;
  if ( ! remark.empty() )
    {
      if ( ! d_remark.empty() ) d_remark += " / ";
      d_remark                           += remark;
    }
}

std::string                                  // right trimmed output
EventRecorder::Event::outputEvent
(const unsigned count) const
{
  std::ostringstream oss;
  if ( d_alert ) oss << "  * ";
  else           oss << "    ";
  oss << ""    << std::setw(s_tab1) << std::right << count
      << "   " << std::setw(s_tab2) << std::left  << d_event
      << "   " << std::setw(s_tab3) << std::left  << d_details
      << "   "                                    << d_remark;
  return boost::trim_right_copy(oss.str());  // also trim trailing spaces
}

// ---------------------------------------------------------
//  CLASS           : EventRecorder
// ---------------------------------------------------------

EventRecorder::EventRecorder
(const std::string title,
 const unsigned    step) :
  d_title(title),
  d_step(step),
  d_events()                                 // empty vector
{
  // record the construction
  const std::string event   = "construction";
  const std::string details = "EventRecorder construction";
  const std::string remark  = d_title;
  d_events.push_back(Event(event, details, remark));
}

EventRecorder::~EventRecorder()
{
}

void
EventRecorder::event
(const std::string event,
 const std::string details,
 const std::string remark)
{
  d_events.push_back(Event(event, details, remark));
}

void
EventRecorder::event
(const std::string event,
 const std::string details,
 const double      value)
{
  const std::string svalue = boost::str(boost::format("%g") % value);
  d_events.push_back(Event(event, details, svalue));
}

void
EventRecorder::event
(const std::string event,
 const std::string details,
 const int         value)
{
  const std::string svalue = boost::str(boost::format("%d") % value);
  d_events.push_back(Event(event, details, svalue));
}

void
EventRecorder::cta
(const std::string klass,
 const std::string remark)
{
  const std::string event   = "cta";
  d_events.push_back(Event(event, klass, remark));
}

void
EventRecorder::mark
(const std::string            symbol,
 shared_ptr<Gateway>          gate,
 shared_ptr<DomainController> domcon,
 const std::string            remark)
{
  const std::string event = "mark " + symbol;
  const std::string details
    = gate->getIdAndKind()
    + " from " + domcon->getIdAndKind();
  d_events.push_back(Event(event, details, remark));
}

void
EventRecorder::unmark
(const std::string            symbol,
 shared_ptr<Gateway>          gate,
 shared_ptr<DomainController> domcon,
 const std::string            remark)
{
  const std::string event = "unmark " + symbol;
  const std::string details
    = gate->getIdAndKind()
    + " from " + domcon->getIdAndKind();
  d_events.push_back(Event(event, details, remark));
}

void
EventRecorder::hop
(shared_ptr<Gateway>          gate,
 shared_ptr<DomainController> fromDomcon,
 shared_ptr<DomainController> toDomcon,
 const std::string            remark)
{
  const std::string event = "hop";
  const std::string details
    = fromDomcon->getIdAndKind()
    + " to "
    + toDomcon->getIdAndKind()
    + " via "
    + gate->getIdAndKind();
  d_events.push_back(Event(event, details, remark));
}

void
EventRecorder::capset
(shared_ptr<DomainController> domcon,
 shared_ptr<Gateway>          targetGate,
 const std::string            remark)
{
  const std::string event   = "capset";
  const std::string details
    = targetGate->getIdAndKind()
    + " from "
    + domcon->getIdAndKind();
  // get capacitities
  d_events.push_back(Event(event, details, remark));
}

void
EventRecorder::transolve
(shared_ptr<DomainController> domcon,
 const std::string            remark)
{
  const std::string event   = "transolve";
  const std::string details = domcon->getIdAndKind();
  d_events.push_back(Event(event, details, remark));
}

void
EventRecorder::note
(const std::string note)
{
  const std::string event   = "note";
  const std::string details = note;
  const std::string remark  = "";
  d_events.push_back(Event(event, details, remark));
}

void
EventRecorder::alert
(const std::string remark)
{
  if ( d_events.empty() )
    {
      return;
    }
  Event last = d_events.back();
  last.addAlert(remark);
  d_events.pop_back();
  d_events.push_back(last);
}

std::string
EventRecorder::output() const
{
  // CAUTION: currently leaves 'd_events' intact, but could
  // equally well clear the vector

  std::ostringstream header;
  header << ""    << std::setw(s_tab1) << std::right << "cnt"
         << "   " << std::setw(s_tab2) << std::left  << "event"
         << "   " << std::setw(s_tab3) << std::left  << "details"
         << "   "                                    << "remark";

  const int tab = s_tab1 + s_tab2 + s_tab3 + 27;
  const std::string rule(tab, '-');

  std::ostringstream oss;
  oss << "  " << d_title << " summary"   << "\n"
      << "    step = " << d_step         << "\n"
      << "    " << header.str()          << "\n"
      << "    " << rule                  << "\n";

  int count = 0;                             // event counter
  BOOST_FOREACH( Event event, d_events )
    {
      oss << event.outputEvent(count++) << "\n";
    }
  return oss.str();
}

//  end of file

