//  XEONA ENERGY SYSTEMS MODELING ENVIRONMENT
//
//  file-create-name : assop.h
//  file-create-date : Mon 25-Aug-2008 12:31 UTC
//  file-initiator   : Robbie Morrison <robbie@actrix.co.nz>
//
//  file-role        : asset operator entity / header
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
//  $URL: file:///home/robbie/svn-root/xeona/futz/trunk/xeona1/b/asop.h $

//  HEADER GUARD

#ifndef _ASOP_H_
#define _ASOP_H_

//  LOCAL AND SYSTEM INCLUDES

#include "../b/tictoc.h"      // inherited interface for entities using common calls
#include "../b/actor.h"       // actor entity

#include "../a/logger_fwd.h"  // some forward declarations from "logger.h"
#include "../c/smart_ptr.h"   // toggle between Boost and TR1 smart pointers

#include <string>             // C++ strings
#include <sstream>            // string-streams

//  FORWARD (PARTIAL) DECLARATIONS

class Record;                                // constructor argument
namespace svif { class SolverIf; }           // member function argument

class TechnicalAsset;                        // data member and function type
class LmpNode;                               // data member and function type

//  CODE

// ---------------------------------------------------------
//  ENUM            : xeona::MarketSide
// ---------------------------------------------------------
//  Description  : encode demand-side and supply-side
//  Role         : the market side is needed because demand-side bidsets require negation
//  Techniques   : (nothing special)
//  Status       : complete
//
//  Design notes
//
//      If needed more generally, move to dedicated unit.
//
// ---------------------------------------------------------

namespace xeona
{
  enum MarketSide
    {
      e_notSpecified   = 0,
      e_demandSide     = 1,
      e_supplySide     = 2
    };

} // namespace 'xeona'

// ---------------------------------------------------------
//  CLASS           : AssetOperator (abstract)
// ---------------------------------------------------------
//  Description  : asset operator
//  Role         : operates technical assets and gateways
//  Techniques   : (nothing special)
//  Status       : complete
// ---------------------------------------------------------

class AssetOperator :
  public Actor,
  public TicToc
{
  // DISABLED

private:

  AssetOperator();                                          // zero-argument constructor
  AssetOperator(const AssetOperator& orig);                 // copy constructor
  AssetOperator& operator= (const AssetOperator& orig);     // copy assignment operator

  // CREATORS

public:

  explicit
  AssetOperator
  (const std::string entityId,
   Record&           record,
   const int         commitmentModeSum);

  virtual
  ~AssetOperator() = 0;                      // create abstract class

  // DOMAIN CONTROLLER POINTS OF ENTRY

public:

  // used to generate domain-based calls to technical assets and to LMP nodes

  std::vector<shared_ptr<TechnicalAsset> >
  getTechnicalAssets();

  virtual                                    // virtual because 'd_lmp_nodes' in not base
  std::vector<shared_ptr<LmpNode> >
  getLmpNodes();

  // standard calls

  virtual void establish();
  virtual void restructure(const xeona::DomainMode commitmentMode);
  virtual void initialize (const int step, shared_ptr<svif::SolverIf> solver);
  virtual void washup();
  virtual void conclude();

  // AUXILIARY CLASS ROUTINES

public:

  virtual
  void
  setCogenHeatWeight
  (const double cogenHeatLeadWeight);

  virtual
  const double
  getCogenHeatWeight() const;

  // INSTANCE DATA

protected:

  // tied quantities

  const std::string&                          d_technical_assets;

  // CAUTION: std::string 'd_lmp_nodes' should be set in
  // appropriate sub-classes

  // local quantities

  std::vector<shared_ptr<TechnicalAsset> >    d_technicalAssets;
  std::vector<shared_ptr<LmpNode> >           d_lmpNodes;

}; // class 'AssetOperator'

#endif // _ASOP_H_

//  end of file

