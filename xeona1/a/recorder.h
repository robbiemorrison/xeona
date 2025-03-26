//  XEONA ENERGY SYSTEMS MODELING ENVIRONMENT
//
//  file-create-name : recorder.h
//  file-create-date : Mon 26-Apr-2010 12:10 UTC
//  file-initiator   : Robbie Morrison <robbie@actrix.co.nz>
//
//  file-role        : summarizing recorder class / header
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
//  $URL: file:///home/robbie/svn-root/xeona/futz/trunk/xeona1/a/recorder.h $
//
//  GENERAL NOTES FOR THIS FILE
//
//  Class 'EventRecorder' was originally written to help debug
//  the CTA algorithm.  It can be used more generally however.

//  HEADER GUARD

#ifndef _RECORDER_H_
#define _RECORDER_H_

//  LOCAL AND SYSTEM INCLUDES

#include "../a/logger_fwd.h"  // some forward declarations from "logger.h"
#include "../c/smart_ptr.h"   // toggle between Boost and TR1 smart pointers

#include <string>             // C++ strings
#include <sstream>            // string-streams

//  CODE

#include <iomanip>            // setw() and family
#include <iostream>           // standard io
#include <sstream>            // string-streams
#include <string>             // C++ strings
#include <vector>             // STL sequence container

#include <boost/algorithm/string.hpp>   // string recasing, trimming, splitting
#include <boost/foreach.hpp>            // BOOST_FOREACH iteration macro

//  FORWARD (PARTIAL) DECLARATIONS

class DomainController;
class Gateway;

// ---------------------------------------------------------
//  CLASS           : EventRecorder
// ---------------------------------------------------------
//  Description  : event recorder which produces a formatted summary on demand
//  Role         : originally written for tracking the CTA algorithm
//  Techniques   : nested class, 'std::vector', streaming
//  Status       : complete
//
//  Design notes
//
//      Intended for recording events within the CTA algorithm.
//
//  Sample output
//
//      cta summary
//        step = 10
//        cnt   event             details                               remark
//        --------------------------------------------------------------------------------
//          0   EventRecorder     event recorder under construction     cta
//          1   cta               CtaSimple
//          2   event             details                               remark
//      *   3   capset            gate-1 (from domcon-1)                capset / reason
//      *   4   note              some long note                        uh-ohh!
//
// ---------------------------------------------------------

class EventRecorder
{
  // NESTED CLASSES

private:

  // ---------------------------------------------------------
  //  CLASS           : EventRecorder::Event (private)
  // ---------------------------------------------------------

  class Event
  {
    // DISABLED

  private:

    Event();                                 // zero-argument constructor

    // CREATORS

  public:

    Event
    (const std::string& event,
     const std::string& details,
     const std::string& remark);

    // MANIPULATORS

    void
    addAlert
    (const std::string& remark);

    // ACCESSORS

    std::string
    outputEvent
    (const unsigned count) const;

    // INSTANCE DATA

  private:

    bool           d_alert;
    std::string    d_event;
    std::string    d_details;
    std::string    d_remark;

  };

  // ---------------------------------------------------------
  //  CLASS           : EventRecorder (public interface))
  // ---------------------------------------------------------

  // DISABLED

private:

  EventRecorder();                                          // zero-argument constructor
  EventRecorder(const EventRecorder& orig);                 // copy constructor
  EventRecorder& operator= (const EventRecorder& orig);     // copy assignment operator

  // CREATORS

public:

  EventRecorder
  (const std::string title,
   const unsigned    step);

  ~EventRecorder();

  // MANIPULATORS

public:

  void
  event                                      // general reporting
  (const std::string event,
   const std::string details,
   const std::string remark);

  void
  event                                      // general reporting
  (const std::string event,
   const std::string details,
   const double      value);

  void
  event                                      // general reporting
  (const std::string event,
   const std::string details,
   const int         value);

  void
  cta                                        // specialized reporting
  (const std::string klass,                  // for example 'CtaSimple'
   const std::string remark = "");

  void
  mark
  (const std::string            symbol,      // for example, "theta"
   shared_ptr<Gateway>          gate,
   shared_ptr<DomainController> domcon,
   const std::string            remark = "");

  void
  unmark
  (const std::string            symbol,      // for example, "theta"
   shared_ptr<Gateway>          gate,
   shared_ptr<DomainController> domcon,
   const std::string            remark = "");

  void
  hop
  (shared_ptr<Gateway>          gate,
   shared_ptr<DomainController> fromDomcon,
   shared_ptr<DomainController> toDomcon,
   const std::string            remark = "");

  void
  capset                                     // specialized reporting
  (shared_ptr<DomainController> domcon,
   shared_ptr<Gateway>          targetGate,
   const std::string            remark = "");

  void
  transolve                                  // specialized reporting
  (shared_ptr<DomainController> domcon,
   const std::string            remark = "");

  void
  note                                       // general reporting
  (const std::string note);

  void
  alert                                      // add alert information subsequently
  (const std::string remark = "");

  // ACCESSORS

  std::string                                // final newline not required
  output() const;                            // output a summary

  // INSTANCE DATA

private:

  const std::string     d_title;
  const int             d_step;
  std::vector<Event>    d_events;

  // STATIC DATA

private:

#if 0  // 0 = narrow, 1 = wide
  static const int s_tab1 =  3;
  static const int s_tab2 = 15;
  static const int s_tab3 = 50;
#else
  static const int s_tab1 =  3;
  static const int s_tab2 = 15;
  static const int s_tab3 = 80;
#endif // 0

};

#endif // _RECORDER_H_

//  end of file

