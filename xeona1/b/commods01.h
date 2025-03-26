//  XEONA ENERGY SYSTEMS MODELING ENVIRONMENT
//
//  file-create-name : commods01.h
//  file-create-date : Tue 05-May-2009 18:35 UTC
//  file-initiator   : Robbie Morrison <robbie@actrix.co.nz>
//
//  file-role        : concrete commodities 1 / header
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
//  $URL: file:///home/robbie/svn-root/xeona/futz/trunk/xeona1/b/commods01.h $
//
//  GENERAL NOTES FOR THIS FILE

//  HEADER GUARD

#ifndef _COMMODS01_H_
#define _COMMODS01_H_

//  LOCAL AND SYSTEM INCLUDES

#include "../b/commods.h"     // commodities hierarchy

#include "../a/logger_fwd.h"  // some forward declarations from "logger.h"
#include "../c/smart_ptr.h"   // toggle between Boost and TR1 smart pointers

#include <string>             // C++ strings
#include <sstream>            // string-streams

//  CODE

// ---------------------------------------------------------
//  CLASS           : CmOxidBiocoal
// ---------------------------------------------------------
//  Description  : biocoal (formed by hydro-thermal carbonization)
//  Role         : state invariant concrete commodity
//  Quantified   : kg
//  Status       : complete
// ---------------------------------------------------------

// NOTE: this particular 'CmOxidize' specialization offers very
// little further functionality, beyond a new class name -- but
// it could well do so

class CmOxidBiocoal :
  public CmOxidize
{
  // CREATORS

public:

  explicit
  CmOxidBiocoal
  (const std::string entityId,
   Record&           record);

  virtual
  ~CmOxidBiocoal();

};

//  ==== XEDOC =================================================
//
//  entity.cm-biocoal-0
//
//      class                                    > CmOxidBiocoal
//
//        biocoal as CmOxidize specialization with added data
//        range checking
//
//      builtin-remark s                         <
//
//      spec-combustion-enthalpy [J/kg] f        > 21.6e+06
//      spec-carbon-dioxide [kg/kg] f            > 3.6
//      spec-co2-equiv [kg/kg] f                 > 3.6
//
//        the specific combustion enthalpy given here is the AR
//        (as received) HHV (higher heating value) assuming 10%
//        moisture -- the modeler can use other protocols but the
//        chosen protocol must be consistent with the associated
//        technical assets
//
//  ============================================================

// ---------------------------------------------------------
//  CLASS           : CmOxidGas
// ---------------------------------------------------------
//  Description  : oxidizable gas
//  Role         : state invariant concrete commodity
//  Quantified   : kg
//  Status       : complete
// ---------------------------------------------------------

class CmOxidGas :
  public CmOxidize
{
  // CREATORS

public:

  explicit
  CmOxidGas
  (const std::string entityId,
   Record&           record);

  virtual
  ~CmOxidGas();

  virtual
  double
  getSpecGwp() const;                        // for leakage

protected:

  double    d_specGwp;                       // assuming unburnt release

};

//  ==== XEDOC =================================================
//
//  entity.cm-oxid-gas-0
//
//      class                                    > CmOxidGas
//
//        combustible gas as CmOxidize specialization -- the
//        values given below are for pure methane
//
//      builtin-remark s                         <
//
//      spec-combustion-enthalpy [J/kg] f        > 55.0e+06
//      spec-carbon-dioxide [kg/kg] f            > 2.74
//      spec-co2-equiv [kg/kg] f                 > 2.74
//      spec-gwp [kg/kg] f                       > 25.0
//
//        the spec-combustion-enthalpy given here is a typical
//        the AR (as received) HHV (higher heating value) for
//        pure methane -- the modeler can use other protocols but
//        the chosen protocol must be consistent with the
//        associated technical assets
//
//        spec-carbon-dioxide describes the post-combustion
//        stoichiometry for CCS usage, the spec-co2-equiv
//        covers combustion products, and the spec-gwp
//        (global warming potential) covers unburnt leakage
//
//  ============================================================

// ---------------------------------------------------------
//  CLASS           : CmOxidNaturalGas
// ---------------------------------------------------------
//  Description  : oxidizable gas
//  Role         : state invariant concrete commodity
//  Quantified   : kg
//  Status       : complete
// ---------------------------------------------------------

class CmOxidNaturalGas :
  public CmOxidGas
{
  // CREATORS

public:

  explicit
  CmOxidNaturalGas
  (const std::string entityId,
   Record&           record);

  virtual
  ~CmOxidNaturalGas();

};

//  ==== XEDOC =================================================
//
//  entity.cm-natural-gas-0
//
//      class                                    > CmOxidNaturalGas
//
//        fossil natural gas as CmOxidGas specialization with
//        added data range checking
//
//      builtin-remark s                         <
//
//      spec-combustion-enthalpy [J/kg] f        > 54.0e+06
//      spec-carbon-dioxide [kg/kg] f            > 2.74
//      spec-co2-equiv [kg/kg] f                 > 2.71
//      spec-gwp [kg/kg] f                       > 25.0
//
//        the spec-combustion-enthalpy given here is a typical
//        the AR (as received) HHV (higher heating value) for
//        natural gas -- the modeler can use other protocols but
//        the chosen protocol must be consistent with the
//        associated technical assets
//
//        spec-carbon-dioxide describes the post-combustion
//        stoichiometry for CCS usage, the spec-co2-equiv
//        covers combustion products, and the spec-gwp
//        (global warming potential) covers unburnt leakage
//
//  ============================================================

// ---------------------------------------------------------
//  CLASS           : CmOxidHydrogen
// ---------------------------------------------------------
//  Description  : oxidizable gas
//  Role         : state invariant concrete commodity
//  Quantified   : kg
//  Status       : complete
// ---------------------------------------------------------

class CmOxidHydrogen :
  public CmOxidGas
{
  // CREATORS

public:

  explicit
  CmOxidHydrogen
  (const std::string entityId,
   Record&           record);

  virtual
  ~CmOxidHydrogen();

};

//  ==== XEDOC =================================================
//
//  entity.cm-hydrogen-0
//
//      class                                    > CmOxidHydrogen
//
//        hydrogen gas as CmOxidGas specialization with added
//        data range checking
//
//      builtin-remark s                         <
//
//      spec-combustion-enthalpy [J/kg] f        > 141.8e+06
//      spec-carbon-dioxide [kg/kg] f            > 0.0
//      spec-co2-equiv [kg/kg] f                 > 0.0
//      spec-gwp [kg/kg] f                       > 0.0
//
//        the spec-combustion-enthalpy given here is a typical
//        HHV (higher heating value) for hydrogen gas -- the
//        modeler can use other protocols but the chosen protocol
//        must be consistent with the associated technical assets
//
//        spec-carbon-dioxide describes the post-combustion
//        stoichiometry for CCS usage, the spec-co2-equiv covers
//        combustion products, and the spec-gwp (global warming
//        potential) covers unburnt leakage
//
//  ============================================================

#endif // _COMMODS01_H_

//  end of file

