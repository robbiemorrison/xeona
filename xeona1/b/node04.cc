//  XEONA ENERGY SYSTEMS MODELING ENVIRONMENT
//
//  file-create-name : node04.cc
//  file-create-date : Wed 11-Jan-2012 14:15 UTC
//  file-initiator   : Robbie Morrison <robbie@actrix.co.nz>
//
//  file-role        : concrete LMP AC nodes 1 / implementation
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
//  $URL: file:///home/robbie/svn-root/xeona/futz/trunk/xeona1/b/node04.cc $
//
//  GENERAL NOTES FOR THIS FILE

//  LOCAL AND SYSTEM INCLUDES

#include "node04.h"           // companion header for this file (place first)

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
//  CLASS           : NodeAc0InjXit <>
// ---------------------------------------------------------

// ---------------------------------------------------------
//  MEMBER FUNCTION : NodeAc0InjXit
// ---------------------------------------------------------

template <typename C>
NodeAc0InjXit<C>::NodeAc0InjXit
(const std::string entityId,
 Record&           record) :
  LmpNodeAc(entityId, record),
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
//  MEMBER FUNCTION : ~NodeAc0InjXit
// ---------------------------------------------------------

template <typename C>
NodeAc0InjXit<C>::~NodeAc0InjXit()
{
  // initial reporting
  s_logger->repx(logga::xtra, "destructor call", getIdAndKind());
}

// ---------------------------------------------------------
//  MEMBER FUNCTION : constrain
// ---------------------------------------------------------

template <typename C>
const int                                    // zero
NodeAc0InjXit<C>::constrain
(const xeona::DomainMode capacityMode)       // not used
{
  // initial reporting
  s_logger->repx(logga::adhc, "entering member function", "NodeAc0InjXit");

  // create and fill a label object
  const std::string junctionId = getIdentifier();
  Label lab(junctionId);

  // recreate new operations OSP of the required type
  d_ops.reset(new Lmp2CabSoc0(d_solver, d_commitmentMode));
  d_ops->loadOspLabel(lab.str());

  // define global cols for internal use
  int cabGol   = -1;                         // nonsensical value
  int socGol   = -1;                         // nonsensical value

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
//  MEMBER FUNCTION : fixTheta
// ---------------------------------------------------------

template <typename C>
void
NodeAc0InjXit<C>::fixTheta
(const double thetaDegrees)                  // reference voltage angle
{
  d_ops->pinTheta(thetaDegrees);
}

// ---------------------------------------------------------
//  MEMBER FUNCTION : washup
// ---------------------------------------------------------

template <typename C>
void
NodeAc0InjXit<C>::washup()
{
  // initial reporting
  s_logger->repx(logga::adhc, "entering member function, step", d_step);

  // results recovery
  double injectionQuantity = -1.0;           // nonsensical value
  double exitQuantity      = -1.0;           // nonsensical value
  double nodalPrice        = -1.0;           // nonsensical value
  boost::tie(injectionQuantity,
             exitQuantity,
             nodalPrice) = d_ops->downloadSolution();

  // set voltage angle here (required because of class hierarchy)
  const double voltageAngle = 0.0;

  // store entity state information
  d_nodalPrices->at(d_step)   = nodalPrice;
  d_voltageAngles->at(d_step) = voltageAngle;

  // store some on-the-fly statistics
  d_dutyStats(std::abs(injectionQuantity));  // functor provided by class 'Block'
  d_sizeStats();                             // functor provided by class 'Block'
}

// ---------------------------------------------------------
//  CLASS           : NodeAc1InjA
// ---------------------------------------------------------

// ---------------------------------------------------------
//  MEMBER FUNCTION : NodeAc1InjA
// ---------------------------------------------------------

template <typename C>
NodeAc1InjA<C>::NodeAc1InjA
(const std::string entityId,
 Record&           record) :
  LmpNodeAc(entityId, record),
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
//  MEMBER FUNCTION : ~NodeAc1InjA
// ---------------------------------------------------------

template <typename C>
NodeAc1InjA<C>::~NodeAc1InjA()
{
  // initial reporting
  s_logger->repx(logga::xtra, "destructor call", getIdAndKind());
}

// ---------------------------------------------------------
//  MEMBER FUNCTION : constrain
// ---------------------------------------------------------

template <typename C>
const int                                    // zero
NodeAc1InjA<C>::constrain
(const xeona::DomainMode capacityMode)       // not used
{
  // initial reporting
  s_logger->repx(logga::adhc, "entering member function", "NodeAc1InjA");

  // create and fill a label object
  const std::string junctionId = getIdentifier();
  Label lab(junctionId);

  // recreate new operations OSP of the required type
  d_ops.reset(new Lmp2Cab1A(d_solver, d_commitmentMode));
  d_ops->loadOspLabel(lab.str());

  // define global cols for internal use
  int cab1Gol  = -1;                         // nonsensical value
  int cab2Gol  = -1;                         // nonsensical value
  int thetaGol = -1;                         // nonsensical value

  // upload the engineering
  boost::tie(cab1Gol,                        // normal
             cab2Gol,                        // bidirectional
             thetaGol)                       // theta
    = d_ops->uploadEngineering();

  // bind global cols to the relevant interfaces
  d_cable1->bindOsp(d_solver, cab1Gol);
  d_cable2->bindOsp(d_solver, cab2Gol, thetaGol);      // just the bidirectional

  // return zero
  return 0;
}

// ---------------------------------------------------------
//  MEMBER FUNCTION : fixTheta
// ---------------------------------------------------------

template <typename C>
void
NodeAc1InjA<C>::fixTheta
(const double thetaDegrees)                  // reference voltage angle
{
  d_ops->pinTheta(thetaDegrees);
}

// ---------------------------------------------------------
//  MEMBER FUNCTION : washup
// ---------------------------------------------------------

template <typename C>
void
NodeAc1InjA<C>::washup()
{
  // initial reporting
  s_logger->repx(logga::adhc, "entering member function, step", d_step);

  // results recovery
  double injectionQuantity = -1.0;           // nonsensical value
  double voltageAngle      = -1.0;           // nonsensical value
  double nodalPrice        = -1.0;           // nonsensical value
  boost::tie(injectionQuantity, voltageAngle, nodalPrice) = d_ops->downloadSolution();

  // store entity state information
  d_nodalPrices->at(d_step)   = nodalPrice;
  d_voltageAngles->at(d_step) = voltageAngle;

  // store some on-the-fly statistics
  d_dutyStats(std::abs(injectionQuantity));  // functor provided by class 'Block'
  d_sizeStats();                             // functor provided by class 'Block'
}

// ---------------------------------------------------------
//  CLASS           : NodeAc1InjB <>
// ---------------------------------------------------------

// ---------------------------------------------------------
//  MEMBER FUNCTION : NodeAc1InjB
// ---------------------------------------------------------

template <typename C>
NodeAc1InjB<C>::NodeAc1InjB
(const std::string entityId,
 Record&           record) :
  LmpNodeAc(entityId, record),
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
//  MEMBER FUNCTION : ~NodeAc1InjB
// ---------------------------------------------------------

template <typename C>
NodeAc1InjB<C>::~NodeAc1InjB()
{
  // initial reporting
  s_logger->repx(logga::xtra, "destructor call", getIdAndKind());
}

// ---------------------------------------------------------
//  MEMBER FUNCTION : constrain
// ---------------------------------------------------------

template <typename C>
const int                                    // zero
NodeAc1InjB<C>::constrain
(const xeona::DomainMode capacityMode)       // not used
{
  // initial reporting
  s_logger->repx(logga::adhc, "entering member function", "NodeAc1InjB");

  // create and fill a label object
  const std::string junctionId = getIdentifier();
  Label lab(junctionId);

  // recreate new operations OSP of the required type
  d_ops.reset(new Lmp2Cab1B(d_solver, d_commitmentMode));
  d_ops->loadOspLabel(lab.str());

  // define global cols for internal use
  int cabGol   = -1;                         // nonsensical value
  int socGol   = -1;                         // nonsensical value
  int thetaGol = -1;                         // nonsensical value

  // upload the engineering
  boost::tie(cabGol,                         // normal
             socGol,                         // bidirectional
             thetaGol)                       // theta
    = d_ops->uploadEngineering();

  // bind global cols to the relevant interfaces
  d_cable ->bindOsp(d_solver, cabGol);
  d_socket->bindOsp(d_solver, socGol, thetaGol);  // just the bidirectional

  // return zero
  return 0;
}

// ---------------------------------------------------------
//  MEMBER FUNCTION : fixTheta
// ---------------------------------------------------------

template <typename C>
void
NodeAc1InjB<C>::fixTheta
(const double thetaDegrees)                  // reference voltage angle
{
  d_ops->pinTheta(thetaDegrees);
}

// ---------------------------------------------------------
//  MEMBER FUNCTION : washup
// ---------------------------------------------------------

template <typename C>
void
NodeAc1InjB<C>::washup()
{
  // initial reporting
  s_logger->repx(logga::adhc, "entering member function, step", d_step);

  // results recovery
  double injectionQuantity = -1.0;           // nonsensical value
  double voltageAngle      = -1.0;           // nonsensical value
  double nodalPrice        = -1.0;           // nonsensical value
  boost::tie(injectionQuantity,
             voltageAngle,
             nodalPrice) = d_ops->downloadSolution();

  // store entity state information
  d_nodalPrices->at(d_step)   = nodalPrice;
  d_voltageAngles->at(d_step) = voltageAngle;

  // store some on-the-fly statistics
  d_dutyStats(std::abs(injectionQuantity));  // functor provided by class 'Block'
  d_sizeStats();                             // functor provided by class 'Block'
}

// ---------------------------------------------------------
//  CLASS           : NodeAc1XitA <>
// ---------------------------------------------------------

// ---------------------------------------------------------
//  MEMBER FUNCTION : NodeAc1XitA
// ---------------------------------------------------------

template <typename C>
NodeAc1XitA<C>::NodeAc1XitA
(const std::string entityId,
 Record&           record) :
  LmpNodeAc(entityId, record),
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
//  MEMBER FUNCTION : ~NodeAc1XitA
// ---------------------------------------------------------

template <typename C>
NodeAc1XitA<C>::~NodeAc1XitA()
{
  // initial reporting
  s_logger->repx(logga::xtra, "destructor call", getIdAndKind());
}

// ---------------------------------------------------------
//  MEMBER FUNCTION : constrain
// ---------------------------------------------------------

template <typename C>
const int                                    // zero
NodeAc1XitA<C>::constrain
(const xeona::DomainMode capacityMode)       // not used
{
  // initial reporting
  s_logger->repx(logga::adhc, "entering member function", "NodeAc1Xit");

  // create and fill a label object
  const std::string junctionId = getIdentifier();
  Label lab(junctionId);

  // recreate new operations OSP of the required type
  d_ops.reset(new Lmp2Soc1A(d_solver, d_commitmentMode));
  d_ops->loadOspLabel(lab.str());

  // define global cols for internal use
  int cabGol   = -1;                         // nonsensical value
  int socGol   = -1;                         // nonsensical value
  int thetaGol = -1;                         // nonsensical value

  // upload the engineering
  boost::tie(cabGol,                         // bidirectional
             socGol,                         // normal
             thetaGol)                       // theta
    = d_ops->uploadEngineering();

  // bind global cols to the relevant interfaces
  d_cable ->bindOsp(d_solver, cabGol, thetaGol);  // just the bidirectional
  d_socket->bindOsp(d_solver, socGol);

  // return zero
  return 0;
}

// ---------------------------------------------------------
//  MEMBER FUNCTION : fixTheta
// ---------------------------------------------------------

template <typename C>
void
NodeAc1XitA<C>::fixTheta
(const double thetaDegrees)                  // reference voltage angle
{
  d_ops->pinTheta(thetaDegrees);
}

// ---------------------------------------------------------
//  MEMBER FUNCTION : washup
// ---------------------------------------------------------

template <typename C>
void
NodeAc1XitA<C>::washup()
{
  // initial reporting
  s_logger->repx(logga::adhc, "entering member function, step", d_step);

  // results recovery
  double exitQuantity = -1.0;                // nonsensical value
  double voltageAngle = -1.0;                // nonsensical value
  double nodalPrice   = -1.0;                // nonsensical value
  boost::tie(exitQuantity,
             voltageAngle,
             nodalPrice) = d_ops->downloadSolution();

  // store entity state information
  d_nodalPrices->at(d_step)   = nodalPrice;
  d_voltageAngles->at(d_step) = voltageAngle;

  // store some on-the-fly statistics
  d_dutyStats(std::abs(exitQuantity));       // functor provided by class 'Block'
  d_sizeStats();                             // functor provided by class 'Block'
}

// ---------------------------------------------------------
//  CLASS           : NodeAc1XitB
// ---------------------------------------------------------

// ---------------------------------------------------------
//  MEMBER FUNCTION : NodeAc1XitB
// ---------------------------------------------------------

template <typename C>
NodeAc1XitB<C>::NodeAc1XitB
(const std::string entityId,
 Record&           record) :
  LmpNodeAc(entityId, record),
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
//  MEMBER FUNCTION : ~NodeAc1XitB
// ---------------------------------------------------------

template <typename C>
NodeAc1XitB<C>::~NodeAc1XitB()
{
  // initial reporting
  s_logger->repx(logga::xtra, "destructor call", getIdAndKind());
}

// ---------------------------------------------------------
//  MEMBER FUNCTION : constrain
// ---------------------------------------------------------

template <typename C>
const int                                    // zero
NodeAc1XitB<C>::constrain
(const xeona::DomainMode capacityMode)       // not used
{
  // initial reporting
  s_logger->repx(logga::adhc, "entering member function", "Node1Xit");

  // create and fill a label object
  const std::string junctionId = getIdentifier();
  Label lab(junctionId);

  // recreate new operations OSP of the required type
  d_ops.reset(new Lmp2Soc1B(d_solver, d_commitmentMode));
  d_ops->loadOspLabel(lab.str());

  // define global cols for internal use
  int soc1Gol  = -1;                         // nonsensical value
  int soc2Gol  = -1;                         // nonsensical value
  int thetaGol = -1;                         // nonsensical value

  // upload the engineering
  boost::tie(soc1Gol,                        // bidirectional
             soc2Gol,                        // normal
             thetaGol)                       // theta
    = d_ops->uploadEngineering();

  // bind global cols to the relevant interfaces
  d_socket1->bindOsp(d_solver, soc1Gol, thetaGol);     // just the bidirectional
  d_socket2->bindOsp(d_solver, soc2Gol);

  // return zero
  return 0;
}

// ---------------------------------------------------------
//  MEMBER FUNCTION : fixTheta
// ---------------------------------------------------------

template <typename C>
void
NodeAc1XitB<C>::fixTheta
(const double thetaDegrees)                  // reference voltage angle
{
  d_ops->pinTheta(thetaDegrees);
}

// ---------------------------------------------------------
//  MEMBER FUNCTION : washup
// ---------------------------------------------------------

template <typename C>
void
NodeAc1XitB<C>::washup()
{
  // initial reporting
  s_logger->repx(logga::adhc, "entering member function, step", d_step);

  // results recovery
  double exitQuantity = -1.0;                // nonsensical value
  double voltageAngle = -1.0;                // nonsensical value
  double nodalPrice   = -1.0;                // nonsensical value
  boost::tie(exitQuantity, voltageAngle, nodalPrice) = d_ops->downloadSolution();

  // store entity state information
  d_nodalPrices->at(d_step)   = nodalPrice;
  d_voltageAngles->at(d_step) = voltageAngle;

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

// class 'NodeAc0InjXit<>'

template NodeAc0InjXit<Elec>::NodeAc0InjXit(const std::string, Record&);
template NodeAc0InjXit<Elec>::~NodeAc0InjXit();
template const int NodeAc0InjXit<Elec>::constrain(const xeona::DomainMode);
template void NodeAc0InjXit<Elec>::fixTheta(double);
template void NodeAc0InjXit<Elec>::washup();

// class 'NodeAc1InjA<>'

template NodeAc1InjA<Elec>::NodeAc1InjA(const std::string, Record&);
template NodeAc1InjA<Elec>::~NodeAc1InjA();
template const int NodeAc1InjA<Elec>::constrain(const xeona::DomainMode);
template void NodeAc1InjA<Elec>::fixTheta(double);
template void NodeAc1InjA<Elec>::washup();

// class 'NodeAc1InjB<>'

template NodeAc1InjB<Elec>::NodeAc1InjB(const std::string, Record&);
template NodeAc1InjB<Elec>::~NodeAc1InjB();
template const int NodeAc1InjB<Elec>::constrain(const xeona::DomainMode);
template void NodeAc1InjB<Elec>::fixTheta(double);
template void NodeAc1InjB<Elec>::washup();

// class 'NodeAc1XitA<>'

template NodeAc1XitA<Elec>::NodeAc1XitA(const std::string, Record&);
template NodeAc1XitA<Elec>::~NodeAc1XitA();
template const int NodeAc1XitA<Elec>::constrain(const xeona::DomainMode);
template void NodeAc1XitA<Elec>::fixTheta(double);
template void NodeAc1XitA<Elec>::washup();

// class 'NodeAc1XitB<>'

template NodeAc1XitB<Elec>::NodeAc1XitB(const std::string, Record&);
template NodeAc1XitB<Elec>::~NodeAc1XitB();
template const int NodeAc1XitB<Elec>::constrain(const xeona::DomainMode);
template void NodeAc1XitB<Elec>::fixTheta(double);
template void NodeAc1XitB<Elec>::washup();

//  end of file

