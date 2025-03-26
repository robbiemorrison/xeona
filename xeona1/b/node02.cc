//  XEONA ENERGY SYSTEMS MODELING ENVIRONMENT
//
//  file-create-name : node02.cc
//  file-create-date : Wed 04-Nov-2009 11:12 UTC
//  file-initiator   : Robbie Morrison <robbie@actrix.co.nz>
//
//  file-role        : concrete LMP DC nodes 2 / implementation
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
//  $URL: file:///home/robbie/svn-root/xeona/futz/trunk/xeona1/b/node02.cc $
//
//  GENERAL NOTES FOR THIS FILE

//  LOCAL AND SYSTEM INCLUDES

#include "node02.h"           // companion header for this file (place first)

#include "../c/recset.h"      // records and fields and also record-sets
#include "../c/label.h"       // helper class to format solver labels
#include "../c/conex.h"       // create and connect block interfaces
#include "../b/optnode1.h"    // node optimization sub-problems for LMP nodes 1
#include "../b/commods.h"     // commodities hierarchy

#include "../a/logger.h"      // standard logging functionality (as required)
#include "../c/smart_ptr.h"   // toggle between Boost and TR1 smart pointers
#include ".././common.h"      // common definitions for project (place last)

#include <string>             // C++ strings
#include <sstream>            // string-streams

#include <cmath>              // C-style maths, abs(), ceil(), floor(), sqrt()

//  CODE

// ---------------------------------------------------------
//  CLASS           : Node2Nul <>
// ---------------------------------------------------------

// ---------------------------------------------------------
//  MEMBER FUNCTION : Node2Nul
// ---------------------------------------------------------

template <typename C>
Node2Nul<C>::Node2Nul
(const std::string entityId,
 Record&           record) :
  LmpNode(entityId, record),
  d_nodeCommodity(record.tieSingle<std::string>("node-commodity")),
  d_cable(Cable<C>::create
          (entityId,                         // me
           record.tieSingle<std::string>("socket-1"),
           d_nodeCommodity)),                // common value
  d_socket(Socket<C>::create
           (entityId,                        // me
            "grid-1",                        // hard-coded socket label
            d_nodeCommodity)),               // common value
  d_ops()
{
  // initial reporting
  s_logger->repx(logga::xtra, "constructor call", getIdAndKind());

  // builtin remark
  d_builtinRemark = "beta";
}

// ---------------------------------------------------------
//  MEMBER FUNCTION : ~Node2Nul
// ---------------------------------------------------------

template <typename C>
Node2Nul<C>::~Node2Nul()
{
  // initial reporting
  s_logger->repx(logga::xtra, "destructor call", getIdAndKind());
}

// ---------------------------------------------------------
//  MEMBER FUNCTION : constrain
// ---------------------------------------------------------

template <typename C>
const int                                    // zero
Node2Nul<C>::constrain
(const xeona::DomainMode capacityMode)       // not used
{

  // initial reporting
  s_logger->repx(logga::adhc, "entering member function", "Node2Nul");

  // create and fill a label object
  const std::string junctionId = getIdentifier();
  Label lab(junctionId);

  // recreate new operations OSP of the required type
  d_ops.reset(new LmpNul2(d_solver, d_commitmentMode));
  d_ops->loadOspLabel(lab.str());

  // define global cols for internal use
  int cab1Gol = -1;                          // nonsensical value
  int soc1Gol = -1;                          // nonsensical value

  // upload the engineering
  boost::tie(cab1Gol,                        // bidirectional
             soc1Gol)                        // bidirectional
    = d_ops->uploadEngineering();

  // bind global cols to the relevant interfaces
  d_cable ->bindOsp(d_solver, cab1Gol);
  d_socket->bindOsp(d_solver, soc1Gol);

  // return zero
  return 0;

} // function 'constrain'

// ---------------------------------------------------------
//  MEMBER FUNCTION : washup
// ---------------------------------------------------------

template <typename C>
void
Node2Nul<C>::washup()
{
  // initial reporting
  s_logger->repx(logga::adhc, "entering member function, step", d_step);

  // results recovery
  double throughputQuantity = -1.0;          // nonsensical value
  double nodalPrice         = -1.0;          // nonsensical value
  boost::tie(throughputQuantity, nodalPrice) = d_ops->downloadSolution();

  // store entity state information
  d_nodalPrices->at(d_step) = nodalPrice;

  // store some on-the-fly statistics
  d_dutyStats(std::abs(throughputQuantity)); // functor provided by class 'Block'
  d_sizeStats();                             // functor provided by class 'Block'
}

// ---------------------------------------------------------
//  CLASS           : Node2Inj <>
// ---------------------------------------------------------

// ---------------------------------------------------------
//  MEMBER FUNCTION : Node2Inj
// ---------------------------------------------------------

template <typename C>
Node2Inj<C>::Node2Inj
(const std::string entityId,
 Record&           record) :
  LmpNode(entityId, record),
  d_nodeCommodity(record.tieSingle<std::string>("node-commodity")),
  d_cable1(Cable<C>::create
           (entityId,                        // me
            record.tieSingle<std::string>("socket-1"),
            d_nodeCommodity)),               // common value
  d_cable2(Cable<C>::create
           (entityId,                        // me
            record.tieSingle<std::string>("socket-2"),
            d_nodeCommodity)),               // common value
  d_socket(Socket<C>::create
           (entityId,                        // me
            "grid-1",                        // hard-coded socket label
            d_nodeCommodity)),               // common value
  d_ops()
{
  // initial reporting
  s_logger->repx(logga::xtra, "constructor call", getIdAndKind());

  // builtin remark
  d_builtinRemark = "beta";
}

// ---------------------------------------------------------
//  MEMBER FUNCTION : ~Node2Inj
// ---------------------------------------------------------

template <typename C>
Node2Inj<C>::~Node2Inj()
{
  // initial reporting
  s_logger->repx(logga::xtra, "destructor call", getIdAndKind());
}

// ---------------------------------------------------------
//  MEMBER FUNCTION : constrain
// ---------------------------------------------------------

template <typename C>
const int                                    // zero
Node2Inj<C>::constrain
(const xeona::DomainMode capacityMode)       // not used
{

  // initial reporting
  s_logger->repx(logga::adhc, "entering member function", "Node2Inj");

  // create and fill a label object
  const std::string junctionId = getIdentifier();
  Label lab(junctionId);

  // recreate new operations OSP of the required type
  d_ops.reset(new LmpCab2(d_solver, d_commitmentMode));
  d_ops->loadOspLabel(lab.str());

  // define global cols for internal use
  int cab1Gol = -1;                          // nonsensical value
  int cab2Gol = -1;                          // nonsensical value
  int soc1Gol = -1;                          // nonsensical value

  // upload the engineering
  boost::tie(cab1Gol,                        // normal
             cab2Gol,                        // bidirectional
             soc1Gol)                        // bidirectional
    = d_ops->uploadEngineering();

  // bind global cols to the relevant interfaces
  d_cable1->bindOsp(d_solver, cab1Gol);
  d_cable2->bindOsp(d_solver, cab2Gol);
  d_socket->bindOsp(d_solver, soc1Gol);

  // return zero
  return 0;

} // function 'constrain'

// ---------------------------------------------------------
//  MEMBER FUNCTION : washup
// ---------------------------------------------------------

template <typename C>
void
Node2Inj<C>::washup()
{
  // initial reporting
  s_logger->repx(logga::adhc, "entering member function, step", d_step);

  // results recovery
  double injectionQuantity = -1.0;           // nonsensical value
  double nodalPrice        = -1.0;           // nonsensical value
  boost::tie(injectionQuantity, nodalPrice) = d_ops->downloadSolution();

  // store entity state information
  d_nodalPrices->at(d_step) = nodalPrice;

  // store some on-the-fly statistics
  d_dutyStats(std::abs(injectionQuantity));  // functor provided by class 'Block'
  d_sizeStats();                             // functor provided by class 'Block'
}

// ---------------------------------------------------------
//  CLASS           : Node2Xit <>
// ---------------------------------------------------------

// ---------------------------------------------------------
//  MEMBER FUNCTION : Node2Xit
// ---------------------------------------------------------

template <typename C>
Node2Xit<C>::Node2Xit
(const std::string entityId,
 Record&           record) :
  LmpNode(entityId, record),
  d_nodeCommodity(record.tieSingle<std::string>("node-commodity")),
  d_cable(Cable<C>::create
          (entityId,                         // me
           record.tieSingle<std::string>("socket-1"),
           d_nodeCommodity)),                // common value
  d_socket1(Socket<C>::create
            (entityId,                       // me
             "sock-1",                       // hard-coded socket label
             d_nodeCommodity)),              // common value
  d_socket2(Socket<C>::create
            (entityId,                       // me
             "grid-1",                       // hard-coded socket label
             d_nodeCommodity)),              // common value
  d_ops()
{
  // initial reporting
  s_logger->repx(logga::xtra, "constructor call", getIdAndKind());

  // builtin remark
  d_builtinRemark = "beta";
}

// ---------------------------------------------------------
//  MEMBER FUNCTION : ~Node2Xit
// ---------------------------------------------------------

template <typename C>
Node2Xit<C>::~Node2Xit()
{
  // initial reporting
  s_logger->repx(logga::xtra, "destructor call", getIdAndKind());
}

// ---------------------------------------------------------
//  MEMBER FUNCTION : constrain
// ---------------------------------------------------------

template <typename C>
const int                                    // zero
Node2Xit<C>::constrain
(const xeona::DomainMode capacityMode)       // not used
{

  // initial reporting
  s_logger->repx(logga::adhc, "entering member function", "Node2Xit");

  // create and fill a label object
  const std::string junctionId = getIdentifier();
  Label lab(junctionId);

  // recreate new operations OSP of the required type
  d_ops.reset(new LmpSoc2(d_solver, d_commitmentMode));
  d_ops->loadOspLabel(lab.str());

  // define global cols for internal use
  int cab1Gol = -1;                           // nonsensical value
  int soc1Gol = -1;                           // nonsensical value
  int soc2Gol = -1;                           // nonsensical value

  // upload the engineering
  boost::tie(cab1Gol,                         // bidirectional
             soc1Gol,                         // normal
             soc2Gol)                         // bidirectional
    = d_ops->uploadEngineering();

  // bind global cols to the relevant interfaces
  d_cable  ->bindOsp(d_solver, cab1Gol);
  d_socket1->bindOsp(d_solver, soc1Gol);
  d_socket2->bindOsp(d_solver, soc2Gol);

  // return zero
  return 0;

} // function 'constrain'

// ---------------------------------------------------------
//  MEMBER FUNCTION : washup
// ---------------------------------------------------------

template <typename C>
void
Node2Xit<C>::washup()
{
  // initial reporting
  s_logger->repx(logga::adhc, "entering member function, step", d_step);

  // results recovery
  double exitQuantity = -1.0;                // nonsensical value
  double nodalPrice   = -1.0;                // nonsensical value
  boost::tie(exitQuantity, nodalPrice) = d_ops->downloadSolution();

  // store entity state information
  d_nodalPrices->at(d_step) = nodalPrice;

  // store some on-the-fly statistics
  d_dutyStats(std::abs(exitQuantity));       // functor provided by class 'Block'
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

// class 'Node2Nul<>'

template Node2Nul<Oxid>::Node2Nul(const std::string, Record&);
template Node2Nul<Cert>::Node2Nul(const std::string, Record&);
template Node2Nul<Cseq>::Node2Nul(const std::string, Record&);
template Node2Nul<Elec>::Node2Nul(const std::string, Record&);
template Node2Nul<Work>::Node2Nul(const std::string, Record&);
template Node2Nul<Heat>::Node2Nul(const std::string, Record&);
template Node2Nul<Thrm>::Node2Nul(const std::string, Record&);
template Node2Nul<Fund>::Node2Nul(const std::string, Record&);

template Node2Nul<Oxid>::~Node2Nul();
template Node2Nul<Cert>::~Node2Nul();
template Node2Nul<Cseq>::~Node2Nul();
template Node2Nul<Elec>::~Node2Nul();
template Node2Nul<Work>::~Node2Nul();
template Node2Nul<Heat>::~Node2Nul();
template Node2Nul<Thrm>::~Node2Nul();
template Node2Nul<Fund>::~Node2Nul();

template const int Node2Nul<Oxid>::constrain(const xeona::DomainMode);
template const int Node2Nul<Cert>::constrain(const xeona::DomainMode);
template const int Node2Nul<Cseq>::constrain(const xeona::DomainMode);
template const int Node2Nul<Elec>::constrain(const xeona::DomainMode);
template const int Node2Nul<Work>::constrain(const xeona::DomainMode);
template const int Node2Nul<Heat>::constrain(const xeona::DomainMode);
template const int Node2Nul<Thrm>::constrain(const xeona::DomainMode);
template const int Node2Nul<Fund>::constrain(const xeona::DomainMode);

template void Node2Nul<Oxid>::washup();
template void Node2Nul<Cert>::washup();
template void Node2Nul<Cseq>::washup();
template void Node2Nul<Elec>::washup();
template void Node2Nul<Work>::washup();
template void Node2Nul<Heat>::washup();
template void Node2Nul<Thrm>::washup();
template void Node2Nul<Fund>::washup();

// class 'Node2Inj<>'

template Node2Inj<Oxid>::Node2Inj(const std::string, Record&);
template Node2Inj<Cert>::Node2Inj(const std::string, Record&);
template Node2Inj<Cseq>::Node2Inj(const std::string, Record&);
template Node2Inj<Elec>::Node2Inj(const std::string, Record&);
template Node2Inj<Work>::Node2Inj(const std::string, Record&);
template Node2Inj<Heat>::Node2Inj(const std::string, Record&);
template Node2Inj<Thrm>::Node2Inj(const std::string, Record&);
template Node2Inj<Fund>::Node2Inj(const std::string, Record&);

template Node2Inj<Oxid>::~Node2Inj();
template Node2Inj<Cert>::~Node2Inj();
template Node2Inj<Cseq>::~Node2Inj();
template Node2Inj<Elec>::~Node2Inj();
template Node2Inj<Work>::~Node2Inj();
template Node2Inj<Heat>::~Node2Inj();
template Node2Inj<Thrm>::~Node2Inj();
template Node2Inj<Fund>::~Node2Inj();

template const int Node2Inj<Oxid>::constrain(const xeona::DomainMode);
template const int Node2Inj<Cert>::constrain(const xeona::DomainMode);
template const int Node2Inj<Cseq>::constrain(const xeona::DomainMode);
template const int Node2Inj<Elec>::constrain(const xeona::DomainMode);
template const int Node2Inj<Work>::constrain(const xeona::DomainMode);
template const int Node2Inj<Heat>::constrain(const xeona::DomainMode);
template const int Node2Inj<Thrm>::constrain(const xeona::DomainMode);
template const int Node2Inj<Fund>::constrain(const xeona::DomainMode);

template void Node2Inj<Oxid>::washup();
template void Node2Inj<Cert>::washup();
template void Node2Inj<Cseq>::washup();
template void Node2Inj<Elec>::washup();
template void Node2Inj<Work>::washup();
template void Node2Inj<Heat>::washup();
template void Node2Inj<Thrm>::washup();
template void Node2Inj<Fund>::washup();

// class 'Node2Xit<>'

template Node2Xit<Oxid>::Node2Xit(const std::string, Record&);
template Node2Xit<Cert>::Node2Xit(const std::string, Record&);
template Node2Xit<Cseq>::Node2Xit(const std::string, Record&);
template Node2Xit<Elec>::Node2Xit(const std::string, Record&);
template Node2Xit<Work>::Node2Xit(const std::string, Record&);
template Node2Xit<Heat>::Node2Xit(const std::string, Record&);
template Node2Xit<Thrm>::Node2Xit(const std::string, Record&);
template Node2Xit<Fund>::Node2Xit(const std::string, Record&);

template Node2Xit<Oxid>::~Node2Xit();
template Node2Xit<Cert>::~Node2Xit();
template Node2Xit<Cseq>::~Node2Xit();
template Node2Xit<Elec>::~Node2Xit();
template Node2Xit<Work>::~Node2Xit();
template Node2Xit<Heat>::~Node2Xit();
template Node2Xit<Thrm>::~Node2Xit();
template Node2Xit<Fund>::~Node2Xit();

template const int Node2Xit<Oxid>::constrain(const xeona::DomainMode);
template const int Node2Xit<Cert>::constrain(const xeona::DomainMode);
template const int Node2Xit<Cseq>::constrain(const xeona::DomainMode);
template const int Node2Xit<Elec>::constrain(const xeona::DomainMode);
template const int Node2Xit<Work>::constrain(const xeona::DomainMode);
template const int Node2Xit<Heat>::constrain(const xeona::DomainMode);
template const int Node2Xit<Thrm>::constrain(const xeona::DomainMode);
template const int Node2Xit<Fund>::constrain(const xeona::DomainMode);

template void Node2Xit<Oxid>::washup();
template void Node2Xit<Cert>::washup();
template void Node2Xit<Cseq>::washup();
template void Node2Xit<Elec>::washup();
template void Node2Xit<Work>::washup();
template void Node2Xit<Heat>::washup();
template void Node2Xit<Thrm>::washup();
template void Node2Xit<Fund>::washup();

//  end of file

