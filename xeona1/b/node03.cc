//  XEONA ENERGY SYSTEMS MODELING ENVIRONMENT
//
//  file-create-name : node03.cc
//  file-create-date : Tue 01-Mar-2011 18:31 UTC
//  file-initiator   : Robbie Morrison <robbie@actrix.co.nz>
//
//  file-role        : concrete LMP DC nodes 3 / implementation
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
//  $URL: file:///home/robbie/svn-root/xeona/futz/trunk/xeona1/b/node03.cc $
//
//  GENERAL NOTES FOR THIS FILE

//  LOCAL AND SYSTEM INCLUDES

#include "node03.h"           // companion header for this file (place first)

#include "../c/recset.h"      // records and fields and also record-sets
#include "../c/label.h"       // helper class to format solver labels
#include "../c/conex.h"       // create and connect block interfaces
#include "../b/optnode1.h"    // node optimization sub-problems for LMP nodes 1
#include "../b/commods.h"     // commodities hierarchy

#include "../a/logger.h"      // standard logging functionality (as required)
#include "../c/smart_ptr.h"   // toggle between Boost and TR1 smart pointers
#include ".././common.h"      // common definitions for project (place last)

#include <algorithm>          // STL copying, searching, and sorting, max()
#include <string>             // C++ strings
#include <sstream>            // string-streams

#include <cmath>              // C-style maths, abs(), ceil(), floor(), sqrt()

//  CODE

// ---------------------------------------------------------
//  CLASS           : Node3NulA <>
// ---------------------------------------------------------

// ---------------------------------------------------------
//  MEMBER FUNCTION : Node3NulA
// ---------------------------------------------------------

template <typename C>
Node3NulA<C>::Node3NulA
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
//  MEMBER FUNCTION : ~Node3NulA
// ---------------------------------------------------------

template <typename C>
Node3NulA<C>::~Node3NulA()
{
  // initial reporting
  s_logger->repx(logga::xtra, "destructor call", getIdAndKind());
}

// ---------------------------------------------------------
//  MEMBER FUNCTION : constrain
// ---------------------------------------------------------

template <typename C>
const int                                    // zero
Node3NulA<C>::constrain
(const xeona::DomainMode capacityMode)       // not used
{

  // initial reporting
  s_logger->repx(logga::adhc, "entering member function", "Node3NulA");

  // create and fill a label object
  const std::string junctionId = getIdentifier();
  Label lab(junctionId);

  // recreate new operations OSP of the required type
  d_ops.reset(new LmpNul3A(d_solver, d_commitmentMode));
  d_ops->loadOspLabel(lab.str());

  // define global cols for internal use
  int cab1Gol = -1;                          // nonsensical value
  int cab2Gol = -1;                          // nonsensical value
  int soc1Gol = -1;                          // nonsensical value

  // upload the engineering
  boost::tie(cab1Gol,                        // bidirectional
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
Node3NulA<C>::washup()
{
  // initial reporting
  s_logger->repx(logga::adhc, "entering member function, step", d_step);

  // results recovery
  double quantity1  = -1.0;                  // nonsensical value, cable 1
  double quantity2  = -1.0;                  // nonsensical value, cable 2
  double quantity3  = -1.0;                  // nonsensical value, socket
  double nodalPrice = -1.0;                  // nonsensical value
  boost::tie(quantity1, quantity2, quantity3, nodalPrice) = d_ops->downloadSolution();

  // store entity state information
  d_nodalPrices->at(d_step) = nodalPrice;

  // identify the greatest absolute flow
  std::vector<double> quantities;
  quantities.push_back(std::abs(quantity1));
  quantities.push_back(std::abs(quantity2));
  quantities.push_back(std::abs(quantity3));
  std::vector<double>::const_iterator pos;
  pos = std::max_element(quantities.begin(), quantities.end());  // refer <algorithm>
  const double max = *pos;                                       // no need to null test

  // store some on-the-fly statistics
  d_dutyStats(max);                          // functor provided by class 'Block'
  d_sizeStats();                             // functor provided by class 'Block'
}

// ---------------------------------------------------------
//  CLASS           : Node3NulB <>
// ---------------------------------------------------------

// ---------------------------------------------------------
//  MEMBER FUNCTION : Node3NulB
// ---------------------------------------------------------

template <typename C>
Node3NulB<C>::Node3NulB
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
             "grid-1",                       // hard-coded socket label
             d_nodeCommodity)),              // common value
  d_socket2(Socket<C>::create
            (entityId,                       // me
             "grid-2",                       // hard-coded socket label
             d_nodeCommodity)),              // common value
  d_ops()
{
  // initial reporting
  s_logger->repx(logga::xtra, "constructor call", getIdAndKind());

  // builtin remark
  d_builtinRemark = "beta";
}

// ---------------------------------------------------------
//  MEMBER FUNCTION : ~Node3NulB
// ---------------------------------------------------------

template <typename C>
Node3NulB<C>::~Node3NulB()
{
  // initial reporting
  s_logger->repx(logga::xtra, "destructor call", getIdAndKind());
}

// ---------------------------------------------------------
//  MEMBER FUNCTION : constrain
// ---------------------------------------------------------

template <typename C>
const int                                    // zero
Node3NulB<C>::constrain
(const xeona::DomainMode capacityMode)       // not used
{

  // initial reporting
  s_logger->repx(logga::adhc, "entering member function", "Node3NulB");

  // create and fill a label object
  const std::string junctionId = getIdentifier();
  Label lab(junctionId);

  // recreate new operations OSP of the required type
  d_ops.reset(new LmpNul3B(d_solver, d_commitmentMode));
  d_ops->loadOspLabel(lab.str());

  // define global cols for internal use
  int cabGol  = -1;                          // nonsensical value
  int soc1Gol = -1;                          // nonsensical value
  int soc2Gol = -1;                          // nonsensical value

  // upload the engineering
  boost::tie(cabGol,                         // bidirectional
             soc1Gol,                        // bidirectional
             soc2Gol)                        // bidirectional
    = d_ops->uploadEngineering();

  // bind global cols to the relevant interfaces
  d_cable  ->bindOsp(d_solver, cabGol);
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
Node3NulB<C>::washup()
{
  // initial reporting
  s_logger->repx(logga::adhc, "entering member function, step", d_step);

  // results recovery
  double quantity1  = -1.0;                  // nonsensical value, cable 1
  double quantity2  = -1.0;                  // nonsensical value, cable 2
  double quantity3  = -1.0;                  // nonsensical value, socket
  double nodalPrice = -1.0;                  // nonsensical value
  boost::tie(quantity1, quantity2, quantity3, nodalPrice) = d_ops->downloadSolution();

  // store entity state information
  d_nodalPrices->at(d_step) = nodalPrice;

  // identify the greatest absolute flow
  std::vector<double> quantities;
  quantities.push_back(std::abs(quantity1));
  quantities.push_back(std::abs(quantity2));
  quantities.push_back(std::abs(quantity3));
  std::vector<double>::const_iterator pos;
  pos = std::max_element(quantities.begin(), quantities.end());  // refer <algorithm>
  const double max = *pos;                                       // no need to null test

  // store some on-the-fly statistics
  d_dutyStats(max);                          // functor provided by class 'Block'
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

// class 'Node3NulA<>'

template Node3NulA<Oxid>::Node3NulA(const std::string, Record&);
template Node3NulA<Cert>::Node3NulA(const std::string, Record&);
template Node3NulA<Cseq>::Node3NulA(const std::string, Record&);
template Node3NulA<Elec>::Node3NulA(const std::string, Record&);
template Node3NulA<Work>::Node3NulA(const std::string, Record&);
template Node3NulA<Heat>::Node3NulA(const std::string, Record&);
template Node3NulA<Thrm>::Node3NulA(const std::string, Record&);
template Node3NulA<Fund>::Node3NulA(const std::string, Record&);

template Node3NulA<Oxid>::~Node3NulA();
template Node3NulA<Cert>::~Node3NulA();
template Node3NulA<Cseq>::~Node3NulA();
template Node3NulA<Elec>::~Node3NulA();
template Node3NulA<Work>::~Node3NulA();
template Node3NulA<Heat>::~Node3NulA();
template Node3NulA<Thrm>::~Node3NulA();
template Node3NulA<Fund>::~Node3NulA();

template const int Node3NulA<Oxid>::constrain(const xeona::DomainMode);
template const int Node3NulA<Cert>::constrain(const xeona::DomainMode);
template const int Node3NulA<Cseq>::constrain(const xeona::DomainMode);
template const int Node3NulA<Elec>::constrain(const xeona::DomainMode);
template const int Node3NulA<Work>::constrain(const xeona::DomainMode);
template const int Node3NulA<Heat>::constrain(const xeona::DomainMode);
template const int Node3NulA<Thrm>::constrain(const xeona::DomainMode);
template const int Node3NulA<Fund>::constrain(const xeona::DomainMode);

template void Node3NulA<Oxid>::washup();
template void Node3NulA<Cert>::washup();
template void Node3NulA<Cseq>::washup();
template void Node3NulA<Elec>::washup();
template void Node3NulA<Work>::washup();
template void Node3NulA<Heat>::washup();
template void Node3NulA<Thrm>::washup();
template void Node3NulA<Fund>::washup();

// class 'Node3NulB<>'

template Node3NulB<Oxid>::Node3NulB(const std::string, Record&);
template Node3NulB<Cert>::Node3NulB(const std::string, Record&);
template Node3NulB<Cseq>::Node3NulB(const std::string, Record&);
template Node3NulB<Elec>::Node3NulB(const std::string, Record&);
template Node3NulB<Work>::Node3NulB(const std::string, Record&);
template Node3NulB<Heat>::Node3NulB(const std::string, Record&);
template Node3NulB<Thrm>::Node3NulB(const std::string, Record&);
template Node3NulB<Fund>::Node3NulB(const std::string, Record&);

template Node3NulB<Oxid>::~Node3NulB();
template Node3NulB<Cert>::~Node3NulB();
template Node3NulB<Cseq>::~Node3NulB();
template Node3NulB<Elec>::~Node3NulB();
template Node3NulB<Work>::~Node3NulB();
template Node3NulB<Heat>::~Node3NulB();
template Node3NulB<Thrm>::~Node3NulB();
template Node3NulB<Fund>::~Node3NulB();

template const int Node3NulB<Oxid>::constrain(const xeona::DomainMode);
template const int Node3NulB<Cert>::constrain(const xeona::DomainMode);
template const int Node3NulB<Cseq>::constrain(const xeona::DomainMode);
template const int Node3NulB<Elec>::constrain(const xeona::DomainMode);
template const int Node3NulB<Work>::constrain(const xeona::DomainMode);
template const int Node3NulB<Heat>::constrain(const xeona::DomainMode);
template const int Node3NulB<Thrm>::constrain(const xeona::DomainMode);
template const int Node3NulB<Fund>::constrain(const xeona::DomainMode);

template void Node3NulB<Oxid>::washup();
template void Node3NulB<Cert>::washup();
template void Node3NulB<Cseq>::washup();
template void Node3NulB<Elec>::washup();
template void Node3NulB<Work>::washup();
template void Node3NulB<Heat>::washup();
template void Node3NulB<Thrm>::washup();
template void Node3NulB<Fund>::washup();

//  end of file

