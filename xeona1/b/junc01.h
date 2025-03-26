//  XEONA ENERGY SYSTEMS MODELING ENVIRONMENT
//
//  file-create-name : junc01.h
//  file-create-date : Mon 26-Oct-2009 12:45 UTC
//  file-initiator   : Robbie Morrison <robbie@actrix.co.nz>
//
//  file-role        : concrete demand split junctions / header
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
//  $URL: file:///home/robbie/svn-root/xeona/futz/trunk/xeona1/b/junc01.h $
//
//  GENERAL NOTES FOR THIS FILE

//  HEADER GUARD

#ifndef _JUNC01_H_
#define _JUNC01_H_

//  LOCAL AND SYSTEM INCLUDES

#include "../b/junc.h"        // demand split/join junction entity

#include "../a/logger_fwd.h"  // some forward declarations from "logger.h"
#include "../c/smart_ptr.h"   // toggle between Boost and TR1 smart pointers

#include <string>             // C++ strings
#include <sstream>            // string-streams

//  FORWARD (PARTIAL) DECLARATIONS

class Record;                                // constructor argument

template <typename C> class Cable;           // 'C' is of base class 'Commodity' etc
template <typename C> class Socket;

//  CODE

// ---------------------------------------------------------
//  CLASS           : JuncDemand2Split <>
// ---------------------------------------------------------
//  Description  : a two cable junction
//  Role         : demand splitting
//  Techniques   : class template, 'JncSplit2'
//  Status       : complete
//
//  Design notes
//
//      The various typename 'C' template specializations take
//      place in 'b/register.cc', more specifically within the
//      EFIRE macro statements.
//
//      With regard to the OSP class naming convention, a
//      trailing '_X' is not required or used.  That is because
//      there should be no need to develop different versions.
//
// ---------------------------------------------------------

class JncSplit2;                             // forward (partial) declaration

template <typename C>                        // 'C' for commodity
class JuncDemand2Split :
  public DemandJunction
{
  // DISABLED

private:

  JuncDemand2Split();                                          // zero-argument ctor
  JuncDemand2Split(const JuncDemand2Split& orig);              // copy constructor
  JuncDemand2Split& operator= (const JuncDemand2Split& orig);  // copy assignment operator

  // CREATORS

public:

  explicit
  JuncDemand2Split
  (const std::string entityId,
   Record&           record);

  virtual
  ~JuncDemand2Split();

  // CALLS

public:

  virtual
  const int                                  // returns zero (no duty coupling)
  constrain                                  // load OSP data
  (const xeona::DomainMode capacityMode);

  virtual
  void
  washup();                                  // recover OSP results

  // INSTANCE DATA

private:

  // tied quantities

  std::string&              d_junctionCommodity;  // for the 'create' interface calls
  shared_ptr<Cable<C> >     d_cable1;
  shared_ptr<Cable<C> >     d_cable2;
  shared_ptr<Socket<C> >    d_socket;

  // local quantities

  shared_ptr<JncSplit2>     d_ops;           // specialization required

}; // class 'JuncDemand2Split<>'

//  ==== XEDOC =================================================
//
//  entity.junc-demand-2-split-*-0
//
//        base commodity in {Cert,Cseq,Elec,Fund,Heat,Oxid,Thrm,Work}
//        or derived in {OxidBiocoal,OxidGas,OxidHydrogen,OxidNaturalGas}
//
//      class                                    > JuncDemand2Split:Elec
//
//        a two cable demand splitting junction for given commodity
//
//        my sole socket label is 'sock-1'
//
//      builtin-remark s                         <
//
//      socket-1 l                               > "teas-supplier-1.elec-1"
//      socket-2 l                               > "teas-supplier-2.elec-1"
//
//        socket-1 and socket-2 are my two potential suppliers
//
//      junction-commodity l                     > "cm-electricity-0"
//
//        junction-commodity defines the underlying commodity
//
//  ============================================================

#endif // _JUNC01_H_

//  end of file

