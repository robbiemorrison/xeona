//  XEONA ENERGY SYSTEMS MODELING ENVIRONMENT
//
//  file-create-name : junc01.cc
//  file-create-date : Mon 26-Oct-2009 12:45 UTC
//  file-initiator   : Robbie Morrison <robbie@actrix.co.nz>
//
//  file-role        : concrete demand split junctions / implementation
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
//  $URL: file:///home/robbie/svn-root/xeona/futz/trunk/xeona1/b/junc01.cc $
//
//  GENERAL NOTES FOR THIS FILE

//  LOCAL AND SYSTEM INCLUDES

#include "junc01.h"           // companion header for this file (place first)

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
//  CLASS           : JuncDemand2Split <>
// ---------------------------------------------------------

// ---------------------------------------------------------
//  MEMBER FUNCTION : JuncDemand2Split
// ---------------------------------------------------------

template <typename C>
JuncDemand2Split<C>::JuncDemand2Split
(const std::string entityId,
 Record&           record) :
  DemandJunction(entityId, record),
  d_junctionCommodity(record.tieSingle<std::string>("junction-commodity")),
  d_cable1(Cable<C>::create
           (entityId,                        // me
            record.tieSingle<std::string>("socket-1"),
            d_junctionCommodity)),           // common value
  d_cable2(Cable<C>::create
           (entityId,                        // me
            record.tieSingle<std::string>("socket-2"),
            d_junctionCommodity)),           // common value
  d_socket(Socket<C>::create
           (entityId,                        // me
            "sock-1",                        // hard-coded socket label
            d_junctionCommodity)),           // common value
  d_ops()
{
  // initial reporting
  s_logger->repx(logga::xtra, "constructor call", getIdAndKind());

  // builtin remark
  d_builtinRemark = "beta";
}

// ---------------------------------------------------------
//  MEMBER FUNCTION : ~JuncDemand2Split
// ---------------------------------------------------------

template <typename C>
JuncDemand2Split<C>::~JuncDemand2Split()
{
  // initial reporting
  s_logger->repx(logga::xtra, "destructor call", getIdAndKind());
}

// ---------------------------------------------------------
//  MEMBER FUNCTION : constrain
// ---------------------------------------------------------

template <typename C>
const int                                    // zero
JuncDemand2Split<C>::constrain
(const xeona::DomainMode capacityMode)       // not used
{
#if 0 // 0 = normal reporting, 1 = extended reporting
  // initial extended reporting
  const std::string subtype = xeona::demangle(typeid(*this).name());  // CAUTION: deref
  s_logger->repx(logga::adhc, "entering member function", subtype);
#else
  // initial reporting
  s_logger->repx(logga::adhc, "entering member function", "JuncDemand2Split");
#endif // 0

  // create and fill a label object
  const std::string junctionId = getIdentifier();
  Label lab(junctionId);

  // recreate new operations OSP of the required type
  d_ops.reset(new JncSplit2(d_solver, d_commitmentMode));
  d_ops->loadOspLabel(lab.str());

  // define global cols for internal use
  int outputGol  = -1;                   // nonsensical value
  int factor1Gol = -1;                   // nonsensical value
  int factor2Gol = -1;                   // nonsensical value

  // upload the engineering
  boost::tie(outputGol,
             factor1Gol,
             factor2Gol)
    = d_ops->uploadEngineering();

  // bind global cols to the relevant interfaces
  d_cable1->bindOsp(d_solver, factor1Gol);
  d_cable2->bindOsp(d_solver, factor2Gol);
  d_socket->bindOsp(d_solver, outputGol);

  // return zero
  return 0;
}

// ---------------------------------------------------------
//  MEMBER FUNCTION : washup
// ---------------------------------------------------------

template <typename C>
void
JuncDemand2Split<C>::washup()
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

// class 'JuncDemand2Split<>'

template JuncDemand2Split<Oxid>::JuncDemand2Split(const std::string, Record&);
template JuncDemand2Split<Cert>::JuncDemand2Split(const std::string, Record&);
template JuncDemand2Split<Cseq>::JuncDemand2Split(const std::string, Record&);
template JuncDemand2Split<Elec>::JuncDemand2Split(const std::string, Record&);
template JuncDemand2Split<Work>::JuncDemand2Split(const std::string, Record&);
template JuncDemand2Split<Heat>::JuncDemand2Split(const std::string, Record&);
template JuncDemand2Split<Thrm>::JuncDemand2Split(const std::string, Record&);
template JuncDemand2Split<Fund>::JuncDemand2Split(const std::string, Record&);

template JuncDemand2Split<Oxid>::~JuncDemand2Split();
template JuncDemand2Split<Cert>::~JuncDemand2Split();
template JuncDemand2Split<Cseq>::~JuncDemand2Split();
template JuncDemand2Split<Elec>::~JuncDemand2Split();
template JuncDemand2Split<Work>::~JuncDemand2Split();
template JuncDemand2Split<Heat>::~JuncDemand2Split();
template JuncDemand2Split<Thrm>::~JuncDemand2Split();
template JuncDemand2Split<Fund>::~JuncDemand2Split();

template const int JuncDemand2Split<Oxid>::constrain(const xeona::DomainMode);
template const int JuncDemand2Split<Cert>::constrain(const xeona::DomainMode);
template const int JuncDemand2Split<Cseq>::constrain(const xeona::DomainMode);
template const int JuncDemand2Split<Elec>::constrain(const xeona::DomainMode);
template const int JuncDemand2Split<Work>::constrain(const xeona::DomainMode);
template const int JuncDemand2Split<Heat>::constrain(const xeona::DomainMode);
template const int JuncDemand2Split<Thrm>::constrain(const xeona::DomainMode);
template const int JuncDemand2Split<Fund>::constrain(const xeona::DomainMode);

template void JuncDemand2Split<Oxid>::washup();
template void JuncDemand2Split<Cert>::washup();
template void JuncDemand2Split<Cseq>::washup();
template void JuncDemand2Split<Elec>::washup();
template void JuncDemand2Split<Work>::washup();
template void JuncDemand2Split<Heat>::washup();
template void JuncDemand2Split<Thrm>::washup();
template void JuncDemand2Split<Fund>::washup();

// ---------------------------------
//  derived instantiations
// ---------------------------------

typedef CmOxidGas        OGas;
typedef CmOxidNaturalGas NatG;
typedef CmOxidBiocoal    BioC;
typedef CmOxidHydrogen   Htwo;

// class 'JuncDemand2Split<>'

template JuncDemand2Split<OGas>::JuncDemand2Split(const std::string, Record&);
template JuncDemand2Split<NatG>::JuncDemand2Split(const std::string, Record&);
template JuncDemand2Split<BioC>::JuncDemand2Split(const std::string, Record&);
template JuncDemand2Split<Htwo>::JuncDemand2Split(const std::string, Record&);

template JuncDemand2Split<OGas>::~JuncDemand2Split();
template JuncDemand2Split<NatG>::~JuncDemand2Split();
template JuncDemand2Split<BioC>::~JuncDemand2Split();
template JuncDemand2Split<Htwo>::~JuncDemand2Split();

template const int JuncDemand2Split<OGas>::constrain(const xeona::DomainMode);
template const int JuncDemand2Split<NatG>::constrain(const xeona::DomainMode);
template const int JuncDemand2Split<BioC>::constrain(const xeona::DomainMode);
template const int JuncDemand2Split<Htwo>::constrain(const xeona::DomainMode);

template void JuncDemand2Split<OGas>::washup();
template void JuncDemand2Split<NatG>::washup();
template void JuncDemand2Split<BioC>::washup();
template void JuncDemand2Split<Htwo>::washup();

//  end of file

