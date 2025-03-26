//  XEONA ENERGY SYSTEMS MODELING ENVIRONMENT
//
//  file-create-name : node01.cc
//  file-create-date : Tue 03-Nov-2009 12:19 UTC
//  file-initiator   : Robbie Morrison <robbie@actrix.co.nz>
//
//  file-role        : concrete LMP DC nodes 1 / implementation
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
//  $URL: file:///home/robbie/svn-root/xeona/futz/trunk/xeona1/b/node01.cc $
//
//  GENERAL NOTES FOR THIS FILE

//  LOCAL AND SYSTEM INCLUDES

#include "node01.h"           // companion header for this file (place first)

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
//  CLASS           : Node0InjXit <>
// ---------------------------------------------------------

// ---------------------------------------------------------
//  MEMBER FUNCTION : Node0InjXit
// ---------------------------------------------------------

template <typename C>
Node0InjXit<C>::Node0InjXit
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
            "sock-1",                        // hard-coded socket label
            d_nodeCommodity)),               // common value
  d_ops()
{
  // initial reporting
  s_logger->repx(logga::xtra, "constructor call", getIdAndKind());

  // builtin remark
  d_builtinRemark = "beta";
}

// ---------------------------------------------------------
//  MEMBER FUNCTION : ~Node0InjXit
// ---------------------------------------------------------

template <typename C>
Node0InjXit<C>::~Node0InjXit()
{
  // initial reporting
  s_logger->repx(logga::xtra, "destructor call", getIdAndKind());
}

// ---------------------------------------------------------
//  MEMBER FUNCTION : constrain
// ---------------------------------------------------------

template <typename C>
const int                                    // zero
Node0InjXit<C>::constrain
(const xeona::DomainMode capacityMode)       // not used
{
  // initial reporting
  s_logger->repx(logga::adhc, "entering member function", "Node0InjXit");

  // create and fill a label object
  const std::string junctionId = getIdentifier();
  Label lab(junctionId);

  // recreate new operations OSP of the required type
  d_ops.reset(new LmpCabSoc0(d_solver, d_commitmentMode));
  d_ops->loadOspLabel(lab.str());

  // define global cols for internal use
  int cabGol = -1;                           // nonsensical value
  int socGol = -1;                           // nonsensical value

  // upload the engineering
  boost::tie(cabGol,                         // normal
             socGol)                         // normal
    = d_ops->uploadEngineering();

  // bind global cols to the relevant interfaces
  d_cable ->bindOsp(d_solver, cabGol);
  d_socket->bindOsp(d_solver, socGol);

  // return zero
  return 0;
}

// ---------------------------------------------------------
//  MEMBER FUNCTION : washup
// ---------------------------------------------------------

template <typename C>
void
Node0InjXit<C>::washup()
{
  // initial reporting
  s_logger->repx(logga::adhc, "entering member function, step", d_step);

  // results recovery
  double injectionQuantity = -1.0;           // nonsensical value
  double exitQuantity      = -1.0;           // nonsensical value
  double nodalPrice        = -1.0;           // nonsensical value
  boost::tie(injectionQuantity, exitQuantity, nodalPrice) = d_ops->downloadSolution();

  // store entity state information
  d_nodalPrices->at(d_step) = nodalPrice;

  // store some on-the-fly statistics
  d_dutyStats(std::abs(injectionQuantity));  // functor provided by class 'Block'
  d_sizeStats();                             // functor provided by class 'Block'
}

// ---------------------------------------------------------
//  CLASS           : Node1InjA <>
// ---------------------------------------------------------

// ---------------------------------------------------------
//  MEMBER FUNCTION : Node1InjA
// ---------------------------------------------------------

template <typename C>
Node1InjA<C>::Node1InjA
(const std::string entityId,
 Record&           record) :
  LmpNode(entityId, record),
  d_nodeCommodity(record.tieSingle<std::string>("node-commodity")),
  d_cable1(Cable<C>::create
          (entityId,                         // me
           record.tieSingle<std::string>("socket-1"),
           d_nodeCommodity)),                // common value
  d_cable2(Cable<C>::create
          (entityId,                         // me
           record.tieSingle<std::string>("socket-2"),
           d_nodeCommodity)),                // common value
  d_ops()
{
  // initial reporting
  s_logger->repx(logga::xtra, "constructor call", getIdAndKind());

  // builtin remark
  d_builtinRemark = "beta";
}

// ---------------------------------------------------------
//  MEMBER FUNCTION : ~Node1InjA
// ---------------------------------------------------------

template <typename C>
Node1InjA<C>::~Node1InjA()
{
  // initial reporting
  s_logger->repx(logga::xtra, "destructor call", getIdAndKind());
}

// ---------------------------------------------------------
//  MEMBER FUNCTION : constrain
// ---------------------------------------------------------

template <typename C>
const int                                    // zero
Node1InjA<C>::constrain
(const xeona::DomainMode capacityMode)       // not used
{
  // initial reporting
  s_logger->repx(logga::adhc, "entering member function", "Node1InjA");

  // create and fill a label object
  const std::string junctionId = getIdentifier();
  Label lab(junctionId);

  // recreate new operations OSP of the required type
  d_ops.reset(new LmpCab1A(d_solver, d_commitmentMode));
  d_ops->loadOspLabel(lab.str());

  // define global cols for internal use
  int cab1Gol = -1;                          // nonsensical value
  int cab2Gol = -1;                          // nonsensical value

  // upload the engineering
  boost::tie(cab1Gol,                        // normal
             cab2Gol)                        // bidirectional
    = d_ops->uploadEngineering();

  // bind global cols to the relevant interfaces
  d_cable1->bindOsp(d_solver, cab1Gol);
  d_cable2->bindOsp(d_solver, cab2Gol);

  // return zero
  return 0;
}

// ---------------------------------------------------------
//  MEMBER FUNCTION : washup
// ---------------------------------------------------------

template <typename C>
void
Node1InjA<C>::washup()
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
//  CLASS           : Node1InjB <>
// ---------------------------------------------------------

// ---------------------------------------------------------
//  MEMBER FUNCTION : Node1InjB
// ---------------------------------------------------------

template <typename C>
Node1InjB<C>::Node1InjB
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
//  MEMBER FUNCTION : ~Node1InjB
// ---------------------------------------------------------

template <typename C>
Node1InjB<C>::~Node1InjB()
{
  // initial reporting
  s_logger->repx(logga::xtra, "destructor call", getIdAndKind());
}

// ---------------------------------------------------------
//  MEMBER FUNCTION : constrain
// ---------------------------------------------------------

template <typename C>
const int                                    // zero
Node1InjB<C>::constrain
(const xeona::DomainMode capacityMode)       // not used
{
  // initial reporting
  s_logger->repx(logga::adhc, "entering member function", "Node1InjB");

  // create and fill a label object
  const std::string junctionId = getIdentifier();
  Label lab(junctionId);

  // recreate new operations OSP of the required type
  d_ops.reset(new LmpCab1B(d_solver, d_commitmentMode));
  d_ops->loadOspLabel(lab.str());

  // define global cols for internal use
  int cabGol = -1;                           // nonsensical value
  int socGol = -1;                           // nonsensical value

  // upload the engineering
  boost::tie(cabGol,                         // normal
             socGol)                         // bidirectional
    = d_ops->uploadEngineering();

  // bind global cols to the relevant interfaces
  d_cable ->bindOsp(d_solver, cabGol);
  d_socket->bindOsp(d_solver, socGol);

  // return zero
  return 0;
}

// ---------------------------------------------------------
//  MEMBER FUNCTION : washup
// ---------------------------------------------------------

template <typename C>
void
Node1InjB<C>::washup()
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
//  CLASS           : Node1XitA <>
// ---------------------------------------------------------

// ---------------------------------------------------------
//  MEMBER FUNCTION : Node1XitA
// ---------------------------------------------------------

template <typename C>
Node1XitA<C>::Node1XitA
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
            "sock-1",                        // hard-coded socket label
            d_nodeCommodity)),               // common value
  d_ops()
{
  // initial reporting
  s_logger->repx(logga::xtra, "constructor call", getIdAndKind());

  // builtin remark
  d_builtinRemark = "beta";
}

// ---------------------------------------------------------
//  MEMBER FUNCTION : ~Node1XitA
// ---------------------------------------------------------

template <typename C>
Node1XitA<C>::~Node1XitA()
{
  // initial reporting
  s_logger->repx(logga::xtra, "destructor call", getIdAndKind());
}

// ---------------------------------------------------------
//  MEMBER FUNCTION : constrain
// ---------------------------------------------------------

template <typename C>
const int                                    // zero
Node1XitA<C>::constrain
(const xeona::DomainMode capacityMode)       // not used
{
  // initial reporting
  s_logger->repx(logga::adhc, "entering member function", "Node1Xit");

  // create and fill a label object
  const std::string junctionId = getIdentifier();
  Label lab(junctionId);

  // recreate new operations OSP of the required type
  d_ops.reset(new LmpSoc1A(d_solver, d_commitmentMode));
  d_ops->loadOspLabel(lab.str());

  // define global cols for internal use
  int cabGol = -1;                           // nonsensical value
  int socGol = -1;                           // nonsensical value

  // upload the engineering
  boost::tie(cabGol,                         // normal
             socGol)                         // bidirectional
    = d_ops->uploadEngineering();

  // bind global cols to the relevant interfaces
  d_cable ->bindOsp(d_solver, cabGol);
  d_socket->bindOsp(d_solver, socGol);

  // return zero
  return 0;
}

// ---------------------------------------------------------
//  MEMBER FUNCTION : washup
// ---------------------------------------------------------

template <typename C>
void
Node1XitA<C>::washup()
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
//  CLASS           : Node1XitB <>
// ---------------------------------------------------------

// ---------------------------------------------------------
//  MEMBER FUNCTION : Node1XitB
// ---------------------------------------------------------

template <typename C>
Node1XitB<C>::Node1XitB
(const std::string entityId,
 Record&           record) :
  LmpNode(entityId, record),
  d_nodeCommodity(record.tieSingle<std::string>("node-commodity")),
  d_socket1(Socket<C>::create
           (entityId,                        // me
            "grid-1",                        // hard-coded socket label
            d_nodeCommodity)),               // common value
  d_socket2(Socket<C>::create
           (entityId,                        // me
            "sock-1",                        // hard-coded socket label
            d_nodeCommodity)),               // common value
  d_ops()
{
  // initial reporting
  s_logger->repx(logga::xtra, "constructor call", getIdAndKind());

  // builtin remark
  d_builtinRemark = "beta";
}

// ---------------------------------------------------------
//  MEMBER FUNCTION : ~Node1XitB
// ---------------------------------------------------------

template <typename C>
Node1XitB<C>::~Node1XitB()
{
  // initial reporting
  s_logger->repx(logga::xtra, "destructor call", getIdAndKind());
}

// ---------------------------------------------------------
//  MEMBER FUNCTION : constrain
// ---------------------------------------------------------

template <typename C>
const int                                    // zero
Node1XitB<C>::constrain
(const xeona::DomainMode capacityMode)       // not used
{
  // initial reporting
  s_logger->repx(logga::adhc, "entering member function", "Node1Xit");

  // create and fill a label object
  const std::string junctionId = getIdentifier();
  Label lab(junctionId);

  // recreate new operations OSP of the required type
  d_ops.reset(new LmpSoc1B(d_solver, d_commitmentMode));
  d_ops->loadOspLabel(lab.str());

  // define global cols for internal use
  int soc1Gol = -1;                          // nonsensical value
  int soc2Gol = -1;                          // nonsensical value

  // upload the engineering
  boost::tie(soc1Gol,                        // bidirectional
             soc2Gol)                        // normal
    = d_ops->uploadEngineering();

  // bind global cols to the relevant interfaces
  d_socket1->bindOsp(d_solver, soc1Gol);
  d_socket2->bindOsp(d_solver, soc2Gol);

  // return zero
  return 0;
}

// ---------------------------------------------------------
//  MEMBER FUNCTION : washup
// ---------------------------------------------------------

template <typename C>
void
Node1XitB<C>::washup()
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

// class 'Node0InjXit<>'

template Node0InjXit<Oxid>::Node0InjXit(const std::string, Record&);
template Node0InjXit<Cert>::Node0InjXit(const std::string, Record&);
template Node0InjXit<Cseq>::Node0InjXit(const std::string, Record&);
template Node0InjXit<Elec>::Node0InjXit(const std::string, Record&);
template Node0InjXit<Work>::Node0InjXit(const std::string, Record&);
template Node0InjXit<Heat>::Node0InjXit(const std::string, Record&);
template Node0InjXit<Thrm>::Node0InjXit(const std::string, Record&);
template Node0InjXit<Fund>::Node0InjXit(const std::string, Record&);

template Node0InjXit<Oxid>::~Node0InjXit();
template Node0InjXit<Cert>::~Node0InjXit();
template Node0InjXit<Cseq>::~Node0InjXit();
template Node0InjXit<Elec>::~Node0InjXit();
template Node0InjXit<Work>::~Node0InjXit();
template Node0InjXit<Heat>::~Node0InjXit();
template Node0InjXit<Thrm>::~Node0InjXit();
template Node0InjXit<Fund>::~Node0InjXit();

template const int Node0InjXit<Oxid>::constrain(const xeona::DomainMode);
template const int Node0InjXit<Cert>::constrain(const xeona::DomainMode);
template const int Node0InjXit<Cseq>::constrain(const xeona::DomainMode);
template const int Node0InjXit<Elec>::constrain(const xeona::DomainMode);
template const int Node0InjXit<Work>::constrain(const xeona::DomainMode);
template const int Node0InjXit<Heat>::constrain(const xeona::DomainMode);
template const int Node0InjXit<Thrm>::constrain(const xeona::DomainMode);
template const int Node0InjXit<Fund>::constrain(const xeona::DomainMode);

template void Node0InjXit<Oxid>::washup();
template void Node0InjXit<Cert>::washup();
template void Node0InjXit<Cseq>::washup();
template void Node0InjXit<Elec>::washup();
template void Node0InjXit<Work>::washup();
template void Node0InjXit<Heat>::washup();
template void Node0InjXit<Thrm>::washup();
template void Node0InjXit<Fund>::washup();

// class 'Node1InjA<>'

template Node1InjA<Oxid>::Node1InjA(const std::string, Record&);
template Node1InjA<Cert>::Node1InjA(const std::string, Record&);
template Node1InjA<Cseq>::Node1InjA(const std::string, Record&);
template Node1InjA<Elec>::Node1InjA(const std::string, Record&);
template Node1InjA<Work>::Node1InjA(const std::string, Record&);
template Node1InjA<Heat>::Node1InjA(const std::string, Record&);
template Node1InjA<Thrm>::Node1InjA(const std::string, Record&);
template Node1InjA<Fund>::Node1InjA(const std::string, Record&);

template Node1InjA<Oxid>::~Node1InjA();
template Node1InjA<Cert>::~Node1InjA();
template Node1InjA<Cseq>::~Node1InjA();
template Node1InjA<Elec>::~Node1InjA();
template Node1InjA<Work>::~Node1InjA();
template Node1InjA<Heat>::~Node1InjA();
template Node1InjA<Thrm>::~Node1InjA();
template Node1InjA<Fund>::~Node1InjA();

template const int Node1InjA<Oxid>::constrain(const xeona::DomainMode);
template const int Node1InjA<Cert>::constrain(const xeona::DomainMode);
template const int Node1InjA<Cseq>::constrain(const xeona::DomainMode);
template const int Node1InjA<Elec>::constrain(const xeona::DomainMode);
template const int Node1InjA<Work>::constrain(const xeona::DomainMode);
template const int Node1InjA<Heat>::constrain(const xeona::DomainMode);
template const int Node1InjA<Thrm>::constrain(const xeona::DomainMode);
template const int Node1InjA<Fund>::constrain(const xeona::DomainMode);

template void Node1InjA<Oxid>::washup();
template void Node1InjA<Cert>::washup();
template void Node1InjA<Cseq>::washup();
template void Node1InjA<Elec>::washup();
template void Node1InjA<Work>::washup();
template void Node1InjA<Heat>::washup();
template void Node1InjA<Thrm>::washup();
template void Node1InjA<Fund>::washup();

// class 'Node1XitA<>'

template Node1XitA<Oxid>::Node1XitA(const std::string, Record&);
template Node1XitA<Cert>::Node1XitA(const std::string, Record&);
template Node1XitA<Cseq>::Node1XitA(const std::string, Record&);
template Node1XitA<Elec>::Node1XitA(const std::string, Record&);
template Node1XitA<Work>::Node1XitA(const std::string, Record&);
template Node1XitA<Heat>::Node1XitA(const std::string, Record&);
template Node1XitA<Thrm>::Node1XitA(const std::string, Record&);
template Node1XitA<Fund>::Node1XitA(const std::string, Record&);

template Node1XitA<Oxid>::~Node1XitA();
template Node1XitA<Cert>::~Node1XitA();
template Node1XitA<Cseq>::~Node1XitA();
template Node1XitA<Elec>::~Node1XitA();
template Node1XitA<Work>::~Node1XitA();
template Node1XitA<Heat>::~Node1XitA();
template Node1XitA<Thrm>::~Node1XitA();
template Node1XitA<Fund>::~Node1XitA();

template const int Node1XitA<Oxid>::constrain(const xeona::DomainMode);
template const int Node1XitA<Cert>::constrain(const xeona::DomainMode);
template const int Node1XitA<Cseq>::constrain(const xeona::DomainMode);
template const int Node1XitA<Elec>::constrain(const xeona::DomainMode);
template const int Node1XitA<Work>::constrain(const xeona::DomainMode);
template const int Node1XitA<Heat>::constrain(const xeona::DomainMode);
template const int Node1XitA<Thrm>::constrain(const xeona::DomainMode);
template const int Node1XitA<Fund>::constrain(const xeona::DomainMode);

template void Node1XitA<Oxid>::washup();
template void Node1XitA<Cert>::washup();
template void Node1XitA<Cseq>::washup();
template void Node1XitA<Elec>::washup();
template void Node1XitA<Work>::washup();
template void Node1XitA<Heat>::washup();
template void Node1XitA<Thrm>::washup();
template void Node1XitA<Fund>::washup();

// class 'Node1InjB<>'

template Node1InjB<Oxid>::Node1InjB(const std::string, Record&);
template Node1InjB<Cert>::Node1InjB(const std::string, Record&);
template Node1InjB<Cseq>::Node1InjB(const std::string, Record&);
template Node1InjB<Elec>::Node1InjB(const std::string, Record&);
template Node1InjB<Work>::Node1InjB(const std::string, Record&);
template Node1InjB<Heat>::Node1InjB(const std::string, Record&);
template Node1InjB<Thrm>::Node1InjB(const std::string, Record&);
template Node1InjB<Fund>::Node1InjB(const std::string, Record&);

template Node1InjB<Oxid>::~Node1InjB();
template Node1InjB<Cert>::~Node1InjB();
template Node1InjB<Cseq>::~Node1InjB();
template Node1InjB<Elec>::~Node1InjB();
template Node1InjB<Work>::~Node1InjB();
template Node1InjB<Heat>::~Node1InjB();
template Node1InjB<Thrm>::~Node1InjB();
template Node1InjB<Fund>::~Node1InjB();

template const int Node1InjB<Oxid>::constrain(const xeona::DomainMode);
template const int Node1InjB<Cert>::constrain(const xeona::DomainMode);
template const int Node1InjB<Cseq>::constrain(const xeona::DomainMode);
template const int Node1InjB<Elec>::constrain(const xeona::DomainMode);
template const int Node1InjB<Work>::constrain(const xeona::DomainMode);
template const int Node1InjB<Heat>::constrain(const xeona::DomainMode);
template const int Node1InjB<Thrm>::constrain(const xeona::DomainMode);
template const int Node1InjB<Fund>::constrain(const xeona::DomainMode);

template void Node1InjB<Oxid>::washup();
template void Node1InjB<Cert>::washup();
template void Node1InjB<Cseq>::washup();
template void Node1InjB<Elec>::washup();
template void Node1InjB<Work>::washup();
template void Node1InjB<Heat>::washup();
template void Node1InjB<Thrm>::washup();
template void Node1InjB<Fund>::washup();

// class 'Node1XitB<>'

template Node1XitB<Oxid>::Node1XitB(const std::string, Record&);
template Node1XitB<Cert>::Node1XitB(const std::string, Record&);
template Node1XitB<Cseq>::Node1XitB(const std::string, Record&);
template Node1XitB<Elec>::Node1XitB(const std::string, Record&);
template Node1XitB<Work>::Node1XitB(const std::string, Record&);
template Node1XitB<Heat>::Node1XitB(const std::string, Record&);
template Node1XitB<Thrm>::Node1XitB(const std::string, Record&);
template Node1XitB<Fund>::Node1XitB(const std::string, Record&);

template Node1XitB<Oxid>::~Node1XitB();
template Node1XitB<Cert>::~Node1XitB();
template Node1XitB<Cseq>::~Node1XitB();
template Node1XitB<Elec>::~Node1XitB();
template Node1XitB<Work>::~Node1XitB();
template Node1XitB<Heat>::~Node1XitB();
template Node1XitB<Thrm>::~Node1XitB();
template Node1XitB<Fund>::~Node1XitB();

template const int Node1XitB<Oxid>::constrain(const xeona::DomainMode);
template const int Node1XitB<Cert>::constrain(const xeona::DomainMode);
template const int Node1XitB<Cseq>::constrain(const xeona::DomainMode);
template const int Node1XitB<Elec>::constrain(const xeona::DomainMode);
template const int Node1XitB<Work>::constrain(const xeona::DomainMode);
template const int Node1XitB<Heat>::constrain(const xeona::DomainMode);
template const int Node1XitB<Thrm>::constrain(const xeona::DomainMode);
template const int Node1XitB<Fund>::constrain(const xeona::DomainMode);

template void Node1XitB<Oxid>::washup();
template void Node1XitB<Cert>::washup();
template void Node1XitB<Cseq>::washup();
template void Node1XitB<Elec>::washup();
template void Node1XitB<Work>::washup();
template void Node1XitB<Heat>::washup();
template void Node1XitB<Thrm>::washup();
template void Node1XitB<Fund>::washup();

//  end of file

