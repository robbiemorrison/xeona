//  XEONA ENERGY SYSTEMS MODELING ENVIRONMENT
//
//  file-create-name : junc04.cc
//  file-create-date : Thu 14-Apr-2011 09:58 UTC
//  file-initiator   : Robbie Morrison <robbie@actrix.co.nz>
//
//  file-role        : concrete faked demand and supply junctions / implementation
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
//  $URL: file:///home/robbie/svn-root/xeona/futz/trunk/xeona1/b/junc04.cc $
//
//  GENERAL NOTES FOR THIS FILE

//  LOCAL AND SYSTEM INCLUDES

#include "junc04.h"           // companion header for this file (place first)

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

#include <limits>             // numeric_limits<T>::infinity() and similar

//  CODE

// ---------------------------------------------------------
//  CLASS           : JuncDemandInvent <>
// ---------------------------------------------------------

// ---------------------------------------------------------
//  MEMBER FUNCTION : JuncDemandInvent
// ---------------------------------------------------------

template <typename C>
JuncDemandInvent<C>::JuncDemandInvent
(const std::string entityId,
 Record&           record) :
  DemandJunction(entityId, record),
  d_junctionCommodity(record.tieSingle<std::string>("junction-commodity")),
  d_constantDemand(record.tieSingle<double>("constant-demand")),
  d_cable(Cable<C>::create
          (entityId,                        // me
           record.tieSingle<std::string>("socket-1"),
           d_junctionCommodity)),           // common value
  d_ops()
{
  // initial reporting
  s_logger->repx(logga::xtra, "constructor call", getIdAndKind());

  // issue some jumpy cautions
  s_logger->repx(logga::rankJumpy,
                 "not intended for working models",
                 getIdAndKind());
  if ( d_constantDemand < 0.0 )
    {
      s_logger->repx(logga::warn, "constant demand is negative", d_constantDemand);
    }
  else if ( d_constantDemand > 0.0 )
    {
      s_logger->repx(logga::rankJumpy, "constant demand is non-zero", d_constantDemand);
    }

  // builtin remark
  d_builtinRemark = "beta / model development only";
}

// ---------------------------------------------------------
//  MEMBER FUNCTION : ~JuncDemandInvent
// ---------------------------------------------------------

template <typename C>
JuncDemandInvent<C>::~JuncDemandInvent()
{
  // initial reporting
  s_logger->repx(logga::xtra, "destructor call", getIdAndKind());
}

// ---------------------------------------------------------
//  MEMBER FUNCTION : constrain
// ---------------------------------------------------------

template <typename C>
const int                                    // zero
JuncDemandInvent<C>::constrain
(const xeona::DomainMode capacityMode)       // not used
{
  // initial reporting
  s_logger->repx(logga::adhc, "entering member function", "JuncDemandInvent");

  // create and fill a label object
  const std::string junctionId = getIdentifier();
  Label lab(junctionId);

  // recreate new operations OSP of the required type
  d_ops.reset(new JncSuck(d_solver, d_commitmentMode));
  d_ops->loadOspLabel(lab.str());

  // define global cols for internal use
  int factorGol = -1;                        // nonsensical value

  // upload the engineering
  boost::tie(factorGol) = d_ops->uploadEngineering(d_constantDemand);

  // bind global cols to the relevant interfaces
  d_cable->bindOsp(d_solver, factorGol);

  // return zero
  return 0;
}

// ---------------------------------------------------------
//  MEMBER FUNCTION : washup
// ---------------------------------------------------------

template <typename C>
void
JuncDemandInvent<C>::washup()
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
//  CLASS           : JuncDemandService <>
// ---------------------------------------------------------

// ---------------------------------------------------------
//  MEMBER FUNCTION : JuncDemandService
// ---------------------------------------------------------

template <typename C>
JuncDemandService<C>::JuncDemandService
(const std::string entityId,
 Record&           record) :
  DemandJunction(entityId, record),
  d_junctionCommodity(record.tieSingle<std::string>("junction-commodity")),
  d_loBound(record.tieSingle<double>("lo-bound")),
  d_hiBound(record.tieSingle<double>("hi-bound")),
  d_supplys(record.tieTimeseries<double>("supplys")),
  d_socket(Socket<C>::create
           (entityId,                        // me
            "sock-1",                        // hard-coded socket label
            d_junctionCommodity)),           // common value
  d_hiBoundReworked(0.0),
  d_ops()
{
  // initial reporting
  s_logger->repx(logga::xtra, "constructor call", getIdAndKind());

  // issue some jumpy cautions
  s_logger->repx(logga::rankJumpy,
                 "not intended for working models",
                 getIdAndKind());

  // rework hi bound, minus one is disable
  if ( d_hiBound == -1.0 ) d_hiBoundReworked = std::numeric_limits<double>::infinity();
  else                     d_hiBoundReworked = d_hiBound;

  // integrity checks
  if ( d_loBound < 0.0 )
    {
      s_logger->repx(logga::warn, "low bound is negative", d_loBound);
    }
  else if ( d_loBound > d_hiBoundReworked )
    {
      std::ostringstream oss;
      oss << d_loBound << " : " << d_hiBoundReworked;
      s_logger->repx(logga::rankJumpy, "low bound exceeds high bound", oss.str());
    }

  // builtin remark
  d_builtinRemark = "beta / model development only";
}

// ---------------------------------------------------------
//  MEMBER FUNCTION : ~JuncDemandService
// ---------------------------------------------------------

template <typename C>
JuncDemandService<C>::~JuncDemandService()
{
  // initial reporting
  s_logger->repx(logga::xtra, "destructor call", getIdAndKind());
}

// ---------------------------------------------------------
//  MEMBER FUNCTION : constrain
// ---------------------------------------------------------

template <typename C>
const int                                    // zero
JuncDemandService<C>::constrain
(const xeona::DomainMode capacityMode)       // not used
{
  // initial reporting
  s_logger->repx(logga::adhc, "entering member function", "JuncDemandService");

  // create and fill a label object
  const std::string junctionId = getIdentifier();
  Label lab(junctionId);

  // recreate new operations OSP of the required type
  d_ops.reset(new JncBlow(d_solver, d_commitmentMode));
  d_ops->loadOspLabel(lab.str());

  // define global cols for internal use
  int productGol = -1;                        // nonsensical value

  // upload the engineering
  boost::tie(productGol) = d_ops->uploadEngineering(d_loBound,
                                                    d_hiBoundReworked);

  // bind global cols to the relevant interfaces
  d_socket->bindOsp(d_solver, productGol);

  // return zero
  return 0;
}

// ---------------------------------------------------------
//  MEMBER FUNCTION : washup
// ---------------------------------------------------------

template <typename C>
void
JuncDemandService<C>::washup()
{
  // initial reporting
  s_logger->repx(logga::adhc, "entering member function, step", d_step);

  // results recovery
  double output      = -1.0;                 // nonsensical value
  boost::tie(output) = d_ops->downloadSolution();

  // store entity state information
  d_supplys->at(d_step) = output;

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

// class 'JuncDemandInvent<>'

template JuncDemandInvent<Oxid>::JuncDemandInvent(const std::string, Record&);
template JuncDemandInvent<Cert>::JuncDemandInvent(const std::string, Record&);
template JuncDemandInvent<Cseq>::JuncDemandInvent(const std::string, Record&);
template JuncDemandInvent<Elec>::JuncDemandInvent(const std::string, Record&);
template JuncDemandInvent<Work>::JuncDemandInvent(const std::string, Record&);
template JuncDemandInvent<Heat>::JuncDemandInvent(const std::string, Record&);
template JuncDemandInvent<Thrm>::JuncDemandInvent(const std::string, Record&);
template JuncDemandInvent<Fund>::JuncDemandInvent(const std::string, Record&);

template JuncDemandInvent<Oxid>::~JuncDemandInvent();
template JuncDemandInvent<Cert>::~JuncDemandInvent();
template JuncDemandInvent<Cseq>::~JuncDemandInvent();
template JuncDemandInvent<Elec>::~JuncDemandInvent();
template JuncDemandInvent<Work>::~JuncDemandInvent();
template JuncDemandInvent<Heat>::~JuncDemandInvent();
template JuncDemandInvent<Thrm>::~JuncDemandInvent();
template JuncDemandInvent<Fund>::~JuncDemandInvent();

template const int JuncDemandInvent<Oxid>::constrain(const xeona::DomainMode);
template const int JuncDemandInvent<Cert>::constrain(const xeona::DomainMode);
template const int JuncDemandInvent<Cseq>::constrain(const xeona::DomainMode);
template const int JuncDemandInvent<Elec>::constrain(const xeona::DomainMode);
template const int JuncDemandInvent<Work>::constrain(const xeona::DomainMode);
template const int JuncDemandInvent<Heat>::constrain(const xeona::DomainMode);
template const int JuncDemandInvent<Thrm>::constrain(const xeona::DomainMode);
template const int JuncDemandInvent<Fund>::constrain(const xeona::DomainMode);

template void JuncDemandInvent<Oxid>::washup();
template void JuncDemandInvent<Cert>::washup();
template void JuncDemandInvent<Cseq>::washup();
template void JuncDemandInvent<Elec>::washup();
template void JuncDemandInvent<Work>::washup();
template void JuncDemandInvent<Heat>::washup();
template void JuncDemandInvent<Thrm>::washup();
template void JuncDemandInvent<Fund>::washup();

// class 'JuncDemandService<>'

template JuncDemandService<Oxid>::JuncDemandService(const std::string, Record&);
template JuncDemandService<Cert>::JuncDemandService(const std::string, Record&);
template JuncDemandService<Cseq>::JuncDemandService(const std::string, Record&);
template JuncDemandService<Elec>::JuncDemandService(const std::string, Record&);
template JuncDemandService<Work>::JuncDemandService(const std::string, Record&);
template JuncDemandService<Heat>::JuncDemandService(const std::string, Record&);
template JuncDemandService<Thrm>::JuncDemandService(const std::string, Record&);
template JuncDemandService<Fund>::JuncDemandService(const std::string, Record&);

template JuncDemandService<Oxid>::~JuncDemandService();
template JuncDemandService<Cert>::~JuncDemandService();
template JuncDemandService<Cseq>::~JuncDemandService();
template JuncDemandService<Elec>::~JuncDemandService();
template JuncDemandService<Work>::~JuncDemandService();
template JuncDemandService<Heat>::~JuncDemandService();
template JuncDemandService<Thrm>::~JuncDemandService();
template JuncDemandService<Fund>::~JuncDemandService();

template const int JuncDemandService<Oxid>::constrain(const xeona::DomainMode);
template const int JuncDemandService<Cert>::constrain(const xeona::DomainMode);
template const int JuncDemandService<Cseq>::constrain(const xeona::DomainMode);
template const int JuncDemandService<Elec>::constrain(const xeona::DomainMode);
template const int JuncDemandService<Work>::constrain(const xeona::DomainMode);
template const int JuncDemandService<Heat>::constrain(const xeona::DomainMode);
template const int JuncDemandService<Thrm>::constrain(const xeona::DomainMode);
template const int JuncDemandService<Fund>::constrain(const xeona::DomainMode);

template void JuncDemandService<Oxid>::washup();
template void JuncDemandService<Cert>::washup();
template void JuncDemandService<Cseq>::washup();
template void JuncDemandService<Elec>::washup();
template void JuncDemandService<Work>::washup();
template void JuncDemandService<Heat>::washup();
template void JuncDemandService<Thrm>::washup();
template void JuncDemandService<Fund>::washup();

// ---------------------------------
//  derived instantiations
// ---------------------------------

typedef CmOxidGas        OGas;
typedef CmOxidNaturalGas NatG;
typedef CmOxidBiocoal    BioC;
typedef CmOxidHydrogen   Htwo;

// class 'JuncDemandInvent<>'

template JuncDemandInvent<OGas>::JuncDemandInvent(const std::string, Record&);
template JuncDemandInvent<NatG>::JuncDemandInvent(const std::string, Record&);
template JuncDemandInvent<BioC>::JuncDemandInvent(const std::string, Record&);
template JuncDemandInvent<Htwo>::JuncDemandInvent(const std::string, Record&);

template JuncDemandInvent<OGas>::~JuncDemandInvent();
template JuncDemandInvent<NatG>::~JuncDemandInvent();
template JuncDemandInvent<BioC>::~JuncDemandInvent();
template JuncDemandInvent<Htwo>::~JuncDemandInvent();

template const int JuncDemandInvent<OGas>::constrain(const xeona::DomainMode);
template const int JuncDemandInvent<NatG>::constrain(const xeona::DomainMode);
template const int JuncDemandInvent<BioC>::constrain(const xeona::DomainMode);
template const int JuncDemandInvent<Htwo>::constrain(const xeona::DomainMode);

template void JuncDemandInvent<OGas>::washup();
template void JuncDemandInvent<NatG>::washup();
template void JuncDemandInvent<BioC>::washup();
template void JuncDemandInvent<Htwo>::washup();

// class 'JuncDemandService<>'

template JuncDemandService<OGas>::JuncDemandService(const std::string, Record&);
template JuncDemandService<NatG>::JuncDemandService(const std::string, Record&);
template JuncDemandService<BioC>::JuncDemandService(const std::string, Record&);
template JuncDemandService<Htwo>::JuncDemandService(const std::string, Record&);

template JuncDemandService<OGas>::~JuncDemandService();
template JuncDemandService<NatG>::~JuncDemandService();
template JuncDemandService<BioC>::~JuncDemandService();
template JuncDemandService<Htwo>::~JuncDemandService();

template const int JuncDemandService<OGas>::constrain(const xeona::DomainMode);
template const int JuncDemandService<NatG>::constrain(const xeona::DomainMode);
template const int JuncDemandService<BioC>::constrain(const xeona::DomainMode);
template const int JuncDemandService<Htwo>::constrain(const xeona::DomainMode);

template void JuncDemandService<OGas>::washup();
template void JuncDemandService<NatG>::washup();
template void JuncDemandService<BioC>::washup();
template void JuncDemandService<Htwo>::washup();

//  end of file

