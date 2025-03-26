//  XEONA ENERGY SYSTEMS MODELING ENVIRONMENT
//
//  file-create-name : node06.cc
//  file-create-date : Fri 03-Feb-2012 16:36 UTC
//  file-initiator   : Robbie Morrison <robbie@actrix.co.nz>
//
//  file-role        : concrete LMP AC nodes 3 / implementation
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
//  $URL: file:///home/robbie/svn-root/xeona/futz/trunk/xeona1/b/node06.cc $
//
//  GENERAL NOTES FOR THIS FILE

//  LOCAL AND SYSTEM INCLUDES

#include "node06.h"           // companion header for this file (place first)

#include "../c/recset.h"      // records and fields and also record-sets
#include "../c/label.h"       // helper class to format solver labels
#include "../c/conex.h"       // create and connect block interfaces
#include "../b/optnode2.h"    // node optimization sub-problems for LMP nodes 2
#include "../b/commods.h"     // commodities hierarchy

#include "../a/logger.h"      // standard logging functionality (as required)
#include "../c/smart_ptr.h"   // toggle between Boost and TR1 smart pointers
#include ".././common.h"      // common definitions for project (place last)

#include <string>             // C++ strings
#include <sstream>            // string-streams

#include <cmath>              // C-style maths, abs(), ceil(), floor(), sqrt()

//  CODE

// ---------------------------------------------------------
//  CLASS           : NodeAc3NulA <>
// ---------------------------------------------------------

// ---------------------------------------------------------
//  MEMBER FUNCTION : NodeAc3NulA
// ---------------------------------------------------------

template <typename C>
NodeAc3NulA<C>::NodeAc3NulA
(const std::string entityId,
 Record&           record) :
  LmpNodeAc(entityId, record),
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
//  MEMBER FUNCTION : ~NodeAc3NulA
// ---------------------------------------------------------

template <typename C>
NodeAc3NulA<C>::~NodeAc3NulA()
{
  // initial reporting
  s_logger->repx(logga::xtra, "destructor call", getIdAndKind());
}

// ---------------------------------------------------------
//  MEMBER FUNCTION : constrain
// ---------------------------------------------------------

template <typename C>
const int                                    // zero
NodeAc3NulA<C>::constrain
(const xeona::DomainMode capacityMode)       // not used
{

  // initial reporting
  s_logger->repx(logga::adhc, "entering member function", "NodeAc3NulA");

  // create and fill a label object
  const std::string junctionId = getIdentifier();
  Label lab(junctionId);

  // recreate new operations OSP of the required type
  d_ops.reset(new Lmp2Nul3A(d_solver, d_commitmentMode));
  d_ops->loadOspLabel(lab.str());

  // define global cols for internal use
  int cab1Gol  = -1;                         // nonsensical value
  int cab2Gol  = -1;                         // nonsensical value
  int soc1Gol  = -1;                         // nonsensical value
  int thetaGol = -1;                         // nonsensical value

  // upload the engineering
  boost::tie(cab1Gol,                        // bidirectional
             cab2Gol,                        // bidirectional
             soc1Gol,                        // bidirectional
             thetaGol)                       // theta
    = d_ops->uploadEngineering();

  // bind global cols to the relevant interfaces
  d_cable1->bindOsp(d_solver, cab1Gol, thetaGol);      // just the bidirectional
  d_cable2->bindOsp(d_solver, cab2Gol, thetaGol);      // just the bidirectional
  d_socket->bindOsp(d_solver, soc1Gol, thetaGol);      // just the bidirectional

  // return zero
  return 0;

} // function 'constrain'

// ---------------------------------------------------------
//  MEMBER FUNCTION : fixTheta
// ---------------------------------------------------------

template <typename C>
void
NodeAc3NulA<C>::fixTheta
(const double thetaDegrees)                  // reference voltage angle
{
  d_ops->pinTheta(thetaDegrees);
}

// ---------------------------------------------------------
//  MEMBER FUNCTION : washup
// ---------------------------------------------------------

template <typename C>
void
NodeAc3NulA<C>::washup()
{
  // initial reporting
  s_logger->repx(logga::adhc, "entering member function, step", d_step);

  // results recovery
  double quantity1    = -1.0;                // nonsensical value, cable 1
  double quantity2    = -1.0;                // nonsensical value, cable 2
  double quantity3    = -1.0;                // nonsensical value, socket
  double voltageAngle = -1.0;                // nonsensical value
  double nodalPrice   = -1.0;                // nonsensical value
  boost::tie(quantity1,
             quantity2,
             quantity3,
             voltageAngle,
             nodalPrice) = d_ops->downloadSolution();

  // store entity state information
  d_nodalPrices->at(d_step)   = nodalPrice;
  d_voltageAngles->at(d_step) = voltageAngle;

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
//  CLASS           : NodeAc3NulB <>
// ---------------------------------------------------------

// ---------------------------------------------------------
//  MEMBER FUNCTION : NodeAc3NulB
// ---------------------------------------------------------

template <typename C>
NodeAc3NulB<C>::NodeAc3NulB
(const std::string entityId,
 Record&           record) :
  LmpNodeAc(entityId, record),
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
//  MEMBER FUNCTION : ~NodeAc3NulB
// ---------------------------------------------------------

template <typename C>
NodeAc3NulB<C>::~NodeAc3NulB()
{
  // initial reporting
  s_logger->repx(logga::xtra, "destructor call", getIdAndKind());
}

// ---------------------------------------------------------
//  MEMBER FUNCTION : constrain
// ---------------------------------------------------------

template <typename C>
const int                                    // zero
NodeAc3NulB<C>::constrain
(const xeona::DomainMode capacityMode)       // not used
{

  // initial reporting
  s_logger->repx(logga::adhc, "entering member function", "NodeAc3NulB");

  // create and fill a label object
  const std::string junctionId = getIdentifier();
  Label lab(junctionId);

  // recreate new operations OSP of the required type
  d_ops.reset(new Lmp2Nul3B(d_solver, d_commitmentMode));
  d_ops->loadOspLabel(lab.str());

  // define global cols for internal use
  int cabGol   = -1;                         // nonsensical value
  int soc1Gol  = -1;                         // nonsensical value
  int soc2Gol  = -1;                         // nonsensical value
  int thetaGol = -1;                         // nonsensical value

  // upload the engineering
  boost::tie(cabGol,                         // bidirectional
             soc1Gol,                        // bidirectional
             soc2Gol,                        // bidirectional
             thetaGol)                       // theta
    = d_ops->uploadEngineering();

  // bind global cols to the relevant interfaces
  d_cable  ->bindOsp(d_solver, cabGol, thetaGol);      // just the bidirectional
  d_socket1->bindOsp(d_solver, soc1Gol, thetaGol);     // just the bidirectional
  d_socket2->bindOsp(d_solver, soc2Gol, thetaGol);     // just the bidirectional

  // return zero
  return 0;

} // function 'constrain'

// ---------------------------------------------------------
//  MEMBER FUNCTION : fixTheta
// ---------------------------------------------------------

template <typename C>
void
NodeAc3NulB<C>::fixTheta
(const double thetaDegrees)                  // reference voltage angle
{
  d_ops->pinTheta(thetaDegrees);
}

// ---------------------------------------------------------
//  MEMBER FUNCTION : washup
// ---------------------------------------------------------

template <typename C>
void
NodeAc3NulB<C>::washup()
{
  // initial reporting
  s_logger->repx(logga::adhc, "entering member function, step", d_step);

  // results recovery
  double quantity1    = -1.0;                // nonsensical value, cable 1
  double quantity2    = -1.0;                // nonsensical value, cable 2
  double quantity3    = -1.0;                // nonsensical value, socket
  double voltageAngle = -1.0;                // nonsensical value
  double nodalPrice   = -1.0;                // nonsensical value
  boost::tie(quantity1,
             quantity2,
             quantity3,
             voltageAngle,
             nodalPrice) = d_ops->downloadSolution();

  // store entity state information
  d_nodalPrices->at(d_step)   = nodalPrice;
  d_voltageAngles->at(d_step) = voltageAngle;

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

// class 'NodeAc3NulA<>'

template NodeAc3NulA<Elec>::NodeAc3NulA(const std::string, Record&);
template NodeAc3NulA<Elec>::~NodeAc3NulA();
template const int NodeAc3NulA<Elec>::constrain(const xeona::DomainMode);
template void NodeAc3NulA<Elec>::fixTheta(const double);
template void NodeAc3NulA<Elec>::washup();

// class 'NodeAc3NulB<>'

template NodeAc3NulB<Elec>::NodeAc3NulB(const std::string, Record&);
template NodeAc3NulB<Elec>::~NodeAc3NulB();
template const int NodeAc3NulB<Elec>::constrain(const xeona::DomainMode);
template void NodeAc3NulB<Elec>::fixTheta(const double);
template void NodeAc3NulB<Elec>::washup();

//  end of file

