//  XEONA ENERGY SYSTEMS MODELING ENVIRONMENT
//
//  file-create-name : teas11.cc
//  file-create-date : Wed 19-Oct-2011 11:05 UTC
//  file-initiator   : Robbie Morrison <robbie@actrix.co.nz>
//
//  file-role        : concrete technical assets 11 / implementation
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
//  $URL: file:///home/robbie/svn-root/xeona/futz/trunk/xeona1/b/teas11.cc $
//
//  GENERAL NOTES FOR THIS FILE

//  LOCAL AND SYSTEM INCLUDES

#include "teas11.h"           // companion header for this file (place first)

#include "../c/recset.h"      // records and fields and also record-sets
#include "../c/label.h"       // helper class to format solver labels
#include "../c/conex.h"       // create and connect block interfaces
#include "../b/optops1.h"     // operate optimization sub-problems for hard assets 1
#include "../b/commods01.h"   // concrete commodities 1
#include "../b/commods.h"     // commodities hierarchy

#include "../a/logger.h"      // standard logging functionality (as required)
#include "../c/smart_ptr.h"   // toggle between Boost and TR1 smart pointers
#include ".././common.h"      // common definitions for project (place last)

#include <limits>             // numeric_limits<T>::infinity() and similar
#include <string>             // C++ strings
#include <sstream>            // string-streams

//  CODE

// ---------------------------------------------------------
//  CLASS           : TeasCapA <>
// ---------------------------------------------------------

// ---------------------------------------------------------
//  MEMBER FUNCTION : TeasCapA<C>
// ---------------------------------------------------------

template <typename C>                        // 'C' for commodity
TeasCapA<C>::TeasCapA
(const std::string entityId,
 Record&           record) :
  CostRegister(record),
  TechnicalAsset(entityId, record, xeona::e_commitmentModes),
  d_commodity(record.tieSingle<std::string>("cable-commodity")),
  d_inCommodity(Cable<C>::create
                (entityId,                   // me
                 record.tieSingle<std::string>("socket-1"),
                 d_commodity)),              // common value
  d_ops()
{
  // initial reporting
  s_logger->repx(logga::xtra, "constructor call", getIdAndKind());

  // builtin remark
  d_builtinRemark = "beta";
}

// ---------------------------------------------------------
//  MEMBER FUNCTION : ~TeasCapA<C>
// ---------------------------------------------------------

template <typename C>
TeasCapA<C>::~TeasCapA()
{
  // initial reporting
  s_logger->repx(logga::xtra, "destructor call", getIdAndKind());
}

// ---------------------------------------------------------
//  MEMBER FUNCTION : constrain
// ---------------------------------------------------------
//  Description  : engineering characterization
//  Role         : host call
//  Techniques   : (nothing special)
//  Status       : complete
//
//  Design notes
//
//      The factor equality constraint is set by my operator.
//
// ---------------------------------------------------------

template <typename C>
const int                                    // duty gol
TeasCapA<C>::constrain
(const xeona::DomainMode capacityMode)
{
  // additional initial reporting as appropriate
  s_logger->repx(logga::adhc, "entering member function", "TeasCapA");

  // create and fill a label object
  const std::string teasId = getIdentifier();
  Label lab(teasId);

  // recreate new operations OSP of the required type
  d_ops.reset(new OpsDummy(d_solver, d_commitmentMode));
  d_ops->loadOspLabel(lab.str());

  // define a global col for internal use
  int inGol = -1;                            // nonsensical value

  // upload the engineering (using the double argument flexible call)
  boost::tie(inGol) = d_ops->uploadEngineering();

  // upload specific costs -- including increment the "shift" term
  // do nothing

  // bind global cols to the relevant interfaces
  d_inCommodity->bindOsp(d_solver, inGol);

  // store duty values
  d_floorDuty   = 0.0;
  d_ceilingDuty = 0.0;

  // return zero to prevent duty gol coupling with the asset operator
  return 0;                                  // CAUTION: not 'inGol'
}

// ---------------------------------------------------------
//  MEMBER FUNCTION : washup
// ---------------------------------------------------------
//  Description  : washup call
//  Role         : obtain and process results
//  Techniques   : (nothing special)
//  Status       : complete
// ---------------------------------------------------------

template <typename C>
void
TeasCapA<C>::washup()
{
  // initial reporting
  s_logger->repx(logga::adhc, "entering member function, step", d_step);

  // results recovery
  d_ops->downloadSolution();

  // store some on-the-fly statistics
  d_dutyStats(0.0);                          // functor provided by class 'Block'
  d_sizeStats(0.0);                          // functor provided by class 'Block'
}

// ---------------------------------------------------------
//  CLASS           : TeasCapB <>
// ---------------------------------------------------------

// ---------------------------------------------------------
//  MEMBER FUNCTION : TeasCapB<C>
// ---------------------------------------------------------

template <typename C>                        // 'C' for commodity
TeasCapB<C>::TeasCapB
(const std::string entityId,
 Record&           record) :
  CostRegister(record),
  TechnicalAsset(entityId, record, xeona::e_commitmentModes),
  d_commodity(record.tieSingle<std::string>("socket-commodity")),
  d_outCommodity(Socket<C>::create
                 (entityId,                  // me
                  "grid-1",                  // hard-coded socket label
                  d_commodity)),             // common value
  d_ops()
{
  // initial reporting
  s_logger->repx(logga::xtra, "constructor call", getIdAndKind());

  // builtin remark
  d_builtinRemark = "beta";
}

// ---------------------------------------------------------
//  MEMBER FUNCTION : ~TeasCapB<C>
// ---------------------------------------------------------

template <typename C>
TeasCapB<C>::~TeasCapB()
{
  // initial reporting
  s_logger->repx(logga::xtra, "destructor call", getIdAndKind());
}

// ---------------------------------------------------------
//  MEMBER FUNCTION : constrain
// ---------------------------------------------------------
//  Description  : engineering characterization
//  Role         : host call
//  Techniques   : (nothing special)
//  Status       : complete
//
//  Design notes
//
//      The factor equality constraint is set by my operator.
//
// ---------------------------------------------------------

template <typename C>
const int                                    // duty gol
TeasCapB<C>::constrain
(const xeona::DomainMode capacityMode)
{
  // additional initial reporting as appropriate
  s_logger->repx(logga::adhc, "entering member function", "TeasCapB");

  // create and fill a label object
  const std::string teasId = getIdentifier();
  Label lab(teasId);

  // recreate new operations OSP of the required type
  d_ops.reset(new OpsDummy(d_solver, d_commitmentMode));
  d_ops->loadOspLabel(lab.str());

  // define a global col for internal use
  int outGol = -1;                           // nonsensical value

  // upload the engineering (using the double argument flexible call)
  boost::tie(outGol) = d_ops->uploadEngineering();

  // upload specific costs -- including increment the "shift" term
  // do nothing

  // bind global cols to the relevant interfaces
  d_outCommodity->bindOsp(d_solver, outGol);

  // store duty values
  d_floorDuty   = 0.0;
  d_ceilingDuty = 0.0;

  // return zero to prevent duty gol coupling with the asset operator
  return 0;                                  // CAUTION: not 'outGol'
}

// ---------------------------------------------------------
//  MEMBER FUNCTION : washup
// ---------------------------------------------------------
//  Description  : washup call
//  Role         : obtain and process results
//  Techniques   : (nothing special)
//  Status       : complete
// ---------------------------------------------------------

template <typename C>
void
TeasCapB<C>::washup()
{
  // initial reporting
  s_logger->repx(logga::adhc, "entering member function, step", d_step);

  // results recovery
  d_ops->downloadSolution();

  // store some on-the-fly statistics
  d_dutyStats(0.0);                          // functor provided by class 'Block'
  d_sizeStats(0.0);                          // functor provided by class 'Block'
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

// class 'TeasCapA'

template TeasCapA<Oxid>::TeasCapA(const std::string, Record&);
template TeasCapA<Cert>::TeasCapA(const std::string, Record&);
template TeasCapA<Cseq>::TeasCapA(const std::string, Record&);
template TeasCapA<Elec>::TeasCapA(const std::string, Record&);
template TeasCapA<Work>::TeasCapA(const std::string, Record&);
template TeasCapA<Heat>::TeasCapA(const std::string, Record&);
template TeasCapA<Thrm>::TeasCapA(const std::string, Record&);
template TeasCapA<Fund>::TeasCapA(const std::string, Record&);

template TeasCapA<Oxid>::~TeasCapA();
template TeasCapA<Cert>::~TeasCapA();
template TeasCapA<Cseq>::~TeasCapA();
template TeasCapA<Elec>::~TeasCapA();
template TeasCapA<Work>::~TeasCapA();
template TeasCapA<Heat>::~TeasCapA();
template TeasCapA<Thrm>::~TeasCapA();
template TeasCapA<Fund>::~TeasCapA();

template const int TeasCapA<Oxid>::constrain(const xeona::DomainMode);
template const int TeasCapA<Cert>::constrain(const xeona::DomainMode);
template const int TeasCapA<Cseq>::constrain(const xeona::DomainMode);
template const int TeasCapA<Elec>::constrain(const xeona::DomainMode);
template const int TeasCapA<Work>::constrain(const xeona::DomainMode);
template const int TeasCapA<Heat>::constrain(const xeona::DomainMode);
template const int TeasCapA<Thrm>::constrain(const xeona::DomainMode);
template const int TeasCapA<Fund>::constrain(const xeona::DomainMode);

template void TeasCapA<Oxid>::washup();
template void TeasCapA<Cert>::washup();
template void TeasCapA<Cseq>::washup();
template void TeasCapA<Elec>::washup();
template void TeasCapA<Work>::washup();
template void TeasCapA<Heat>::washup();
template void TeasCapA<Thrm>::washup();
template void TeasCapA<Fund>::washup();

// class 'TeasCapB'

template TeasCapB<Oxid>::TeasCapB(const std::string, Record&);
template TeasCapB<Cert>::TeasCapB(const std::string, Record&);
template TeasCapB<Cseq>::TeasCapB(const std::string, Record&);
template TeasCapB<Elec>::TeasCapB(const std::string, Record&);
template TeasCapB<Work>::TeasCapB(const std::string, Record&);
template TeasCapB<Heat>::TeasCapB(const std::string, Record&);
template TeasCapB<Thrm>::TeasCapB(const std::string, Record&);
template TeasCapB<Fund>::TeasCapB(const std::string, Record&);

template TeasCapB<Oxid>::~TeasCapB();
template TeasCapB<Cert>::~TeasCapB();
template TeasCapB<Cseq>::~TeasCapB();
template TeasCapB<Elec>::~TeasCapB();
template TeasCapB<Work>::~TeasCapB();
template TeasCapB<Heat>::~TeasCapB();
template TeasCapB<Thrm>::~TeasCapB();
template TeasCapB<Fund>::~TeasCapB();

template const int TeasCapB<Oxid>::constrain(const xeona::DomainMode);
template const int TeasCapB<Cert>::constrain(const xeona::DomainMode);
template const int TeasCapB<Cseq>::constrain(const xeona::DomainMode);
template const int TeasCapB<Elec>::constrain(const xeona::DomainMode);
template const int TeasCapB<Work>::constrain(const xeona::DomainMode);
template const int TeasCapB<Heat>::constrain(const xeona::DomainMode);
template const int TeasCapB<Thrm>::constrain(const xeona::DomainMode);
template const int TeasCapB<Fund>::constrain(const xeona::DomainMode);

template void TeasCapB<Oxid>::washup();
template void TeasCapB<Cert>::washup();
template void TeasCapB<Cseq>::washup();
template void TeasCapB<Elec>::washup();
template void TeasCapB<Work>::washup();
template void TeasCapB<Heat>::washup();
template void TeasCapB<Thrm>::washup();
template void TeasCapB<Fund>::washup();

//  end of file

