//  XEONA ENERGY SYSTEMS MODELING ENVIRONMENT
//
//  file-create-name : cta.h
//  file-create-date : Fri 06-Feb-2009 15:11 UTC
//  file-initiator   : Robbie Morrison <robbie@actrix.co.nz>
//
//  file-role        : captrans algorithm / header
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
//  $URL: file:///home/robbie/svn-root/xeona/futz/trunk/xeona1/f/cta.h $
//
//  GENERAL NOTES FOR THIS FILE
//
//  The "captrans" algorithm is described in detail in my (Robbie
//  Morrison) PhD thesis.

//  HEADER GUARD

#ifndef _CTA_H_
#define _CTA_H_

//  LOCAL AND SYSTEM INCLUDES

#include "../f/ospmodes.h"    // domain mode enums (header only)
#include "../a/logger_fwd.h"  // some forward declarations from "logger.h"
#include "../c/smart_ptr.h"   // toggle between Boost and TR1 smart pointers

#include <string>             // C++ strings
#include <vector>             // STL sequence container

#include <boost/logic/tribool.hpp>      // three state boolean
#include <boost/logic/tribool_io.hpp>   // CAUTION: essential for std::boolalpha reporting

//  NAMESPACE DECLARATIONS

using boost::logic::tribool;            // three state boolean
using boost::logic::indeterminate;      // allows 'indeterminate' and 'indeterminate()'

//  FORWARD (PARTIAL) DECLARATIONS

class DomainController;

//  CODE

// ---------------------------------------------------------
//  CLASS           : CapTransAlg (abstract base class)
// ---------------------------------------------------------
//  Description  : base class for various "captrans" algorithms (CTA)
//  Role         : traverse domain graph and make 'capset' and 'transolve' calls
//  Techniques   : pure virtual, re-invoked by 'Overseer' each interval
//  Status       : complete
//
//  Design notes
//
//      Capacity mode
//
//          The 'capacityMode' is (depending on the derived class
//          implementation) taken by the function 'captrans',
//          range checked, and then passed on to the 'capset' and
//          'transolve' calls as these occur -- hence, the
//          'capacityMode' value has no direct influence on
//          course of the "captrans" algorithm.
//
//      Algorithm description
//
//          The CTA (including its simple and hop-relitate
//          variants) is NOT described here.  Refer instead to my
//          (Robbie Morrison) PhD thesis for more information.
//          The CTA is relatively complicated!
//
// ---------------------------------------------------------

class CapTransAlg
{
  // DISABLED

private:

  CapTransAlg();                                       // zero-argument ctor
  CapTransAlg(const CapTransAlg& orig);                // copy constructor
  CapTransAlg& operator= (const CapTransAlg& orig);    // copy assignment

  // CREATORS

public:

  CapTransAlg
  (const int                                         step,       // current step
   const std::vector<shared_ptr<DomainController> >& domcons);   // ranked

  virtual
  ~CapTransAlg();

  // MANIPULATORS

  virtual
  tribool                                    // 'indeterminate' if no originating domains
  captrans
  (const xeona::DomainMode capacityMode)
    = 0;

  // INSTANCE DATA

protected:

  const int                                           d_step;
  const std::vector<shared_ptr<DomainController> >    d_originatingDomcons;

  // STATIC DATA

protected:

  static logga::spLogger    s_logger;        // shared_ptr to single logger object

};

// ---------------------------------------------------------
//  CLASS           : CtaFixed
// ---------------------------------------------------------
//  Description  : stub for static capacity setting
//  Role         : <none>
//  Techniques   : <none>
//  Status       : incomplete -- reports noisily if construction attempted
//
//  CAUTION: development status
//
//      This class is currently hollow -- and will likely stay
//      that way for quite some time.
//
// ---------------------------------------------------------

class CtaFixed :
  public CapTransAlg
{
public:

  CtaFixed
  (const int                                         step,
   const std::vector<shared_ptr<DomainController> >& domcons);

  tribool                                    // 'indeterminate' if no originating domains
  captrans
  (const xeona::DomainMode capacityMode);

};

// ---------------------------------------------------------
//  CLASS           : CtaSimple
// ---------------------------------------------------------
//  Description  : derived class
//  Role         : see 'CapTransAlg'
//  Techniques   : see 'CtaSimple::captrans'
//  Status       : complete
//
//  CAUTION: dual-implementation (temporary measure)
//
//      This class currently contains code for both the "simple"
//      and "hop-relit" variants selected by way of the 'XE_CTA'
//      cpp macro.
//
// ---------------------------------------------------------

class CtaSimple :
  public CapTransAlg
{
public:

  CtaSimple
  (const int                                         step,
   const std::vector<shared_ptr<DomainController> >& domcons);

  ~CtaSimple();

  tribool                                    // 'indeterminate' if no originating domains
  captrans
  (const xeona::DomainMode capacityMode);

};

// ---------------------------------------------------------
//  CLASS           : CtaHopRelit
// ---------------------------------------------------------

// CAUTION: this class contains private creators for the time
// being, but should re-instigate later to allow XEM-level
// selection between 'simple' and 'hop-relit' variants -- note
// also there is no allied code in the implementation file at
// present

class CtaHopRelit :
  public CapTransAlg
{
private:                                     // fully disabled for now
  CtaHopRelit();                                       // zero-argument ctor
  CtaHopRelit(const CtaHopRelit& orig);                // copy constructor
  CtaHopRelit& operator= (const CtaHopRelit& orig);    // copy assignment
  ~CtaHopRelit();                                      // destructor
};

#endif // _CTA_H_

//  end of file

