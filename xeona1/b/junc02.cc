//  XEONA ENERGY SYSTEMS MODELING ENVIRONMENT
//
//  file-create-name : junc02.cc
//  file-create-date : Tue 27-Oct-2009 09:18 UTC
//  file-initiator   : Robbie Morrison <robbie@actrix.co.nz>
//
//  file-role        : concrete demand join junctions / implementation
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
//  $URL: file:///home/robbie/svn-root/xeona/futz/trunk/xeona1/b/junc02.cc $
//
//  GENERAL NOTES FOR THIS FILE

//  LOCAL AND SYSTEM INCLUDES

#include "junc02.h"           // companion header for this file (place first)

#include "../c/recset.h"      // records and fields and also record-sets
#include "../c/label.h"       // helper class to format solver labels
#include "../c/conex.h"       // create and connect block interfaces
#include "../b/optjunc.h"     // operations OSPs for junctions
#include "../b/commods01.h"   // concrete commodities 1
#include "../b/commods.h"     // commodities hierarchy

#include "../a/logger.h"      // standard logging functionality (as required)
#include "../c/smart_ptr.h"   // toggle between Boost and TR1 smart pointers
#include ".././common.h"      // common definitions for project (place last)

#include <string>             // C++ strings
#include <sstream>            // string-streams

//  CODE

// ---------------------------------------------------------
//  MEMBER FUNCTION : JuncDemand2Join
// ---------------------------------------------------------

template <typename C>
JuncDemand2Join<C>::JuncDemand2Join
(const std::string entityId,
 Record&           record) :
  DemandJunction(entityId, record),
  d_junctionCommodity(record.tieSingle<std::string>("junction-commodity")),
  d_cable(Cable<C>::create
          (entityId,                         // me
           record.tieSingle<std::string>("socket-1"),
           d_junctionCommodity)),            // common value
  d_socket1(Socket<C>::create
            (entityId,                       // me
             "sock-1",                       // hard-coded socket label
             d_junctionCommodity)),          // common value
  d_socket2(Socket<C>::create
            (entityId,                       // me
             "sock-2",                       // hard-coded socket label
             d_junctionCommodity)),          // common value
  d_ops()
{
  // initial reporting
  s_logger->repx(logga::xtra, "constructor call", getIdAndKind());

  // builtin remark
  d_builtinRemark = "beta";
}

// ---------------------------------------------------------
//  MEMBER FUNCTION : ~JuncDemand2Join
// ---------------------------------------------------------

template <typename C>
JuncDemand2Join<C>::~JuncDemand2Join()
{
  // initial reporting
  s_logger->repx(logga::xtra, "destructor call", getIdAndKind());
}

// ---------------------------------------------------------
//  MEMBER FUNCTION : constrain
// ---------------------------------------------------------

template <typename C>
const int                                    // zero
JuncDemand2Join<C>::constrain
(const xeona::DomainMode capacityMode)       // not used
{
  // initial reporting
  s_logger->repx(logga::adhc, "entering member function", "JuncDemand2Join");

  // create and fill a label object
  const std::string junctionId = getIdentifier();
  Label lab(junctionId);

  // recreate new operations OSP of the required type
  d_ops.reset(new JncJoin2(d_solver, d_commitmentMode));
  d_ops->loadOspLabel(lab.str());

  // define global cols for internal use
  int factorGol   = -1;                      // nonsensical value
  int output1Gol  = -1;                      // nonsensical value
  int output2Gol  = -1;                      // nonsensical value

  // upload the engineering
  boost::tie(factorGol,
             output1Gol,
             output2Gol)
    = d_ops->uploadEngineering();

  // bind global cols to the relevant interfaces
  d_cable  ->bindOsp(d_solver, factorGol);
  d_socket1->bindOsp(d_solver, output1Gol);
  d_socket2->bindOsp(d_solver, output2Gol);

  // return zero
  return 0;
}

// ---------------------------------------------------------
//  MEMBER FUNCTION : washup
// ---------------------------------------------------------

template <typename C>
void
JuncDemand2Join<C>::washup()
{
  // initial reporting
  s_logger->repx(logga::adhc, "entering member function, step", d_step);

  // results recovery
  double output      = -1.0;                 // nonsensical value
  boost::tie(output) = d_ops->downloadSolution();

  // store some on-the-fly statistics
  d_dutyStats(output);                       // functor provided by class 'Block'
  d_sizeStats();                             // functor provided by class 'Block'
}

// ---------------------------------------------------------
//  EXPLICIT TEMPLATE INSTANTIATIONS
// ---------------------------------------------------------

// CAUTION: explicit template instantiations must be placed at
// the very end of the implementation file
//
// For more information, see the excellent documentation in
// 'recset.cc' regarding explicit template instantiations.

// class 'Commodity' typedefs for convenience

typedef CmOxidize      Oxid;
typedef CmCarbonCert   Cert;
typedef CmCarbonSeq    Cseq;
typedef CmElectricity  Elec;
typedef CmWork         Work;
typedef CmHeat         Heat;
typedef CmThermalFluid Thrm;
typedef CmFunds        Fund;

// class 'JuncDemand2Join<>'

template JuncDemand2Join<Oxid>::JuncDemand2Join(const std::string, Record&);
template JuncDemand2Join<Cert>::JuncDemand2Join(const std::string, Record&);
template JuncDemand2Join<Cseq>::JuncDemand2Join(const std::string, Record&);
template JuncDemand2Join<Elec>::JuncDemand2Join(const std::string, Record&);
template JuncDemand2Join<Work>::JuncDemand2Join(const std::string, Record&);
template JuncDemand2Join<Heat>::JuncDemand2Join(const std::string, Record&);
template JuncDemand2Join<Thrm>::JuncDemand2Join(const std::string, Record&);
template JuncDemand2Join<Fund>::JuncDemand2Join(const std::string, Record&);

template JuncDemand2Join<Oxid>::~JuncDemand2Join();
template JuncDemand2Join<Cert>::~JuncDemand2Join();
template JuncDemand2Join<Cseq>::~JuncDemand2Join();
template JuncDemand2Join<Elec>::~JuncDemand2Join();
template JuncDemand2Join<Work>::~JuncDemand2Join();
template JuncDemand2Join<Heat>::~JuncDemand2Join();
template JuncDemand2Join<Thrm>::~JuncDemand2Join();
template JuncDemand2Join<Fund>::~JuncDemand2Join();

template const int JuncDemand2Join<Oxid>::constrain(const xeona::DomainMode);
template const int JuncDemand2Join<Cert>::constrain(const xeona::DomainMode);
template const int JuncDemand2Join<Cseq>::constrain(const xeona::DomainMode);
template const int JuncDemand2Join<Elec>::constrain(const xeona::DomainMode);
template const int JuncDemand2Join<Work>::constrain(const xeona::DomainMode);
template const int JuncDemand2Join<Heat>::constrain(const xeona::DomainMode);
template const int JuncDemand2Join<Thrm>::constrain(const xeona::DomainMode);
template const int JuncDemand2Join<Fund>::constrain(const xeona::DomainMode);

template void JuncDemand2Join<Oxid>::washup();
template void JuncDemand2Join<Cert>::washup();
template void JuncDemand2Join<Cseq>::washup();
template void JuncDemand2Join<Elec>::washup();
template void JuncDemand2Join<Work>::washup();
template void JuncDemand2Join<Heat>::washup();
template void JuncDemand2Join<Thrm>::washup();
template void JuncDemand2Join<Fund>::washup();

// ---------------------------------
//  derived instantiations
// ---------------------------------

typedef CmOxidGas        OGas;
typedef CmOxidNaturalGas NatG;
typedef CmOxidBiocoal    BioC;
typedef CmOxidHydrogen   Htwo;

template JuncDemand2Join<OGas>::JuncDemand2Join(const std::string, Record&);
template JuncDemand2Join<NatG>::JuncDemand2Join(const std::string, Record&);
template JuncDemand2Join<BioC>::JuncDemand2Join(const std::string, Record&);
template JuncDemand2Join<Htwo>::JuncDemand2Join(const std::string, Record&);

template JuncDemand2Join<OGas>::~JuncDemand2Join();
template JuncDemand2Join<NatG>::~JuncDemand2Join();
template JuncDemand2Join<BioC>::~JuncDemand2Join();
template JuncDemand2Join<Htwo>::~JuncDemand2Join();

template const int JuncDemand2Join<OGas>::constrain(const xeona::DomainMode);
template const int JuncDemand2Join<NatG>::constrain(const xeona::DomainMode);
template const int JuncDemand2Join<BioC>::constrain(const xeona::DomainMode);
template const int JuncDemand2Join<Htwo>::constrain(const xeona::DomainMode);

template void JuncDemand2Join<OGas>::washup();
template void JuncDemand2Join<NatG>::washup();
template void JuncDemand2Join<BioC>::washup();
template void JuncDemand2Join<Htwo>::washup();

//  end of file

