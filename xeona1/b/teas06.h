//  XEONA ENERGY SYSTEMS MODELING ENVIRONMENT
//
//  file-create-name : teas06.h
//  file-create-date : Mon 10-Jan-2011 16:52 UTC
//  file-initiator   : Robbie Morrison <robbie@actrix.co.nz>
//
//  file-role        : concrete technical assets 6 / header
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
//  $URL: file:///home/robbie/svn-root/xeona/futz/trunk/xeona1/b/teas06.h $
//
//  GENERAL NOTES FOR THIS FILE

//  HEADER GUARD

#ifndef _TEAS06_H_
#define _TEAS06_H_

//  LOCAL AND SYSTEM INCLUDES

#include "../h/sandia.h"      // Sandia photovoltaic array model
#include "../b/teas.h"        // technical asset entity
#include "../b/costreg.h"     // cost registers

#include "../a/logger_fwd.h"  // some forward declarations from "logger.h"
#include "../c/smart_ptr.h"   // toggle between Boost and TR1 smart pointers

#include <string>             // C++ strings

//  FORWARD (PARTIAL) DECLARATIONS

class Record;                                // constructor argument
namespace svif { class SolverIf; }           // member function argument

template <typename C> class Socket;          // 'C' is of base class 'Commodity' etc

class CmElectricity;                         // 'CmElectricity' is from 'Commodity' etc

class CxAmbientSolar;
class CxAmbientAir;

//  CODE

// ---------------------------------------------------------
//  CLASS           : TeasPvInstallation
// ---------------------------------------------------------
//  Description  : a photovoltaic installation with identical panels
//  Role         : concrete entity
//  Techniques   : 'OpsFac0Out1' source-style OSP, class 'PvModule'
//  Status       : complete
//
//  Design notes
//
//      This class is "powered" by class 'PvModule' -- which
//      implements the Sandia Photovoltaic Array Performance
//      Model, developed by the US Sandia National Laboratories:
//
//        http://photovoltaics.sandia.gov
//
//      For more details, consult the documentation for unit
//      'h/sandia'.
//
//      At the time of writing, only one Sandia dataset had been
//      included in this code, namely the "Siemens SM55".
//      Additional datasets can be easily added.
//
// ---------------------------------------------------------

class OpsFac0Out1_A;                         // necessary for OSP typedef below

class TeasPvInstallation :
  public TechnicalAsset,
  public CostRegisterSRFin,
  public CostRegisterEmbFin
{
  // USING DECLARATIONS

protected:

  using Entity::s_logger;                    // place in common scope for this class

  // TYPEDEFS

private:

  typedef OpsFac0Out1_A OpsFac0Out1;         // used for switching implementations

  // DISABLED

private:

  TeasPvInstallation();                                            // zero-argument ctor
  TeasPvInstallation(const TeasPvInstallation& orig);              // copy constructor
  TeasPvInstallation& operator= (const TeasPvInstallation& orig);  // copy assignment oper

  // CREATORS

public:

  explicit
  TeasPvInstallation
  (const std::string entityId,
   Record&           record);

  virtual
  ~TeasPvInstallation();

  // CALLS

public:

  virtual
  void
  establish();                               // // with low priority development reporting

  virtual
  const int                                  // duty gol
  constrain
  (const xeona::DomainMode capacityMode);

  virtual
  void
  washup();

  virtual
  void
  conclude();

  // INSTANCE DATA

private:

  // tied quantities

  assign_ptr<CxAmbientSolar>           d_ambientSolarContext;
  assign_ptr<CxAmbientAir>             d_ambientAirContext;

  const std::string&                   d_panelModel;
  const int&                           d_count;
  const double&                        d_panelZenith;
  const double&                        d_panelAzimuth;
  const double&                        d_systemLossFactor;
  const double&                        d_siteAltitude;
  const bool&                          d_internalDumpFlag;

  shared_ptr<std::vector<double> >     d_potentialProductions;
  shared_ptr<std::vector<double> >     d_actualProductions;
  double&                              d_discard;

  shared_ptr<Socket<CmElectricity> >   d_outElec;

  // local quantities

  shared_ptr<OpsFac0Out1>              d_ops;     // specialization required
  shared_ptr<PvModule>                 d_module;  // uses Sandia model

};

//  regarding the "system-loss-factor", grid-tie inverters
//  normally have losses of [0.04,0.12] not including resistance
//  losses in the DC wiring -- hence the suggested value of
//  [0.1,0.2] overall

//  ==== XEDOC =================================================
//
//  entity.teas-pv-installation-0
//
//      class                                    > TeasPvInstallation
//
//        a photovoltaic installation comprising one or more
//        identical flat panels plus inverter and wiring -- and
//        based on the 2010 Sandia photovoltaic array performance
//        model
//
//        only Siemens Solar SM55 modules are currently supported
//        (but more characterizations can easily be added)
//
//        my socket label is 'elec-1'
//
//      builtin-remark s                         <
//
//      panel-model [s]                          > "Siemens Solar SM55"
//      count [-] i                              > 5
//      panel-zenith [degrees] f                 > 30.0
//      panel-azimuth [degrees] f                > 180.0
//      system-loss-factor [-] f                 > 0.2
//      site-altitude [m] f                      > 100.0
//
//        the panel-model is the descriptor from the Sandia
//        database, the panel-zenith [0,90] is the tilt from
//        horizontal, the panel-azimuth [0,360] is the
//        orientation relative to north, the system-loss-factor
//        accounts for general losses including wiring and
//        electronics and is usually [0.1,0.2], and the
//        site-altitude need only be approximate
//
//        the site elevation from the solar context, if non-zero,
//        will trump the site-altitude
//
//      internal-dump-flag b                     > 1
//
//        if the internal-dump-flag is 1 (true) then internal
//        dumping is allowed, else if 0 (false) then the entire
//        supply must be taken
//
//      socket-electricity-commodity l           > "cm-electricity-0"
//
//        socket-electricity-commodity defines the shared
//        electricity commodity
//
//      ambient-solar-context l                  > "cx-ambient-solar-0"
//      ambient-air-context l                    > "cx-ambient-air-0"
//
//        the context entities must be sub-classes of
//        CxAmbientSolar and CxAmbientAir
//
//      potential-productions [W] F              < 0.0 ..
//      actual-productions [W] F                 < 0.0 ..
//      discard [-] f                            < 0.0
//
//        discard [0,1] is the ratio of discarded to potential
//        output
//
//      nameplate-capacity [W] f                 > 5000e+03
//      duty-specific-cost-financial [$/J] f     > 100.0
//      size-specific-cost-financial [$/W/s] f   > 200.0
//      standing-cost-financial [$/s] f          > 300.0
//
//      variable-costs-financial [$] F           < 0.0 ..
//      fixed-costs-financial [$] F              < 0.0 ..
//
//      annual-discount-rate-decimal [-] f       > 0.10
//      economic-life [y] i                      > 25
//      capex-initial [$] f                      > +120e+03
//      capex-terminal [$] f                     > -10e+03
//      current-age [y] i                        > 2
//
//        a negative capex-terminal indicates salvage income as
//        opposed to decommissioning cost
//
//      embedded-costs-financial [$] F           < 0.0 ..
//
//  ============================================================

#endif // _TEAS06_H_

//  end of file

