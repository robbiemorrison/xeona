//  XEONA ENERGY SYSTEMS MODELING ENVIRONMENT
//
//  file-create-name : ospmodes.h
//  file-create-date : Thu 05-Feb-2009 23:08 UTC
//  file-initiator   : Robbie Morrison <robbie@actrix.co.nz>
//
//  file-role        : domain mode enums / header (only)
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
//  $URL: file:///home/robbie/svn-root/xeona/futz/trunk/xeona1/f/ospmodes.h $

//  HEADER GUARD

#ifndef _OSPMODES_H_
#define _OSPMODES_H_

//  LOCAL AND SYSTEM INCLUDES

//  none required

//  CODE

// ---------------------------------------------------------
//  ENUM            : xeona::DomainMode
// ---------------------------------------------------------
//
//  Design notes
//
//      This enum embeds two pieces of information: the capacity
//      mode and the commitment mode.
//
//      This allows a single argument to be passed around,
//      thereby simplifying the coding (although two arguments or
//      a std::pair could equally well have been used).
//
//      Capacity modes
//
//          Capacity modes deal with the way capacities are set
//          on gateways.  Withholding need not be commercially
//          strategic, but is often so.
//
//      Commitment modes
//
//          Commitment modes deal with the way transactions are
//          solved within control domains.
//
//  CAUTION: do no hardcode integer values
//
//      NEVER EVER use the underlying integer value in client
//      code -- ALWAYS use the respective enumerator (that is,
//      the enumeration identifier) instead!  It is expected that
//      the values assigned here can be changed at will!
//
//  CAUTION: 15 enumerators only
//
//      To play safe, the maximum enumerator value should not
//      exceed 16384 -- meaning a limit of 15 values, counting
//      any gaps but excluding 'e_modeNotSpecified'.
//
//      Lischner (2003 pp26-27) discusses the integer ranges for
//      enums in considerable detail (the key message is that the
//      compiler chooses the underlying type from 'signed char'
//      to 'unsigned int').
//
//  Committment modes in detail
//
//      The commitment mode determines in part how the
//      optimization sub-problems will be uploaded and
//      interpreted.
//
//      Values for this enum are held within the various
//      'TechnicalAsset' and 'AssetOperator' sub-classes.
//
//      This value is, in turn, sent thru to the derived
//      'OptimSubProb' instances so that they can respond
//      appropriately.
//
//      In addition, integrity checks are carried out to ensure
//      the selected commitment strategy is consistent with the
//      various domain-specific assets and objects present (this
//      being a modeling issue and not a coding error).
//
//      Note also that it will probably be rather difficult to
//      add to the current cost types (fin, ghg, nox, dep, luc)
//      and commitment modes (shortrunFin, etc).  Both concepts
//      are deeply embedded within the design of 'xeona'.
//
// ---------------------------------------------------------

namespace xeona
{
  enum DomainMode                  // CAUTION: add new enums to the or'ed lists below
    {
      e_modeNotSpecified =     0,

      // capacity modes

      e_usePresets       =     1,  // use XEM-input capacities, no run-time calculation *
      e_withholdOkay     =     2,  // withholding acceptable
      e_withholdBan      =     4,  // withholding banned, run at normal technical capacity
      e_crisisOperation  =     8,  // relax all further non-mandatory constraints *

      // CAUTION: * = not supported by the codebase as it stands,
      // nonetheless provided for possible future use

      // commitment modes

      e_shortrunFin      =    32,  // short-run financial cost minimization
      e_shortrunGhg      =    64,  // short-run GHG contribution minimization
      e_shortrunNox      =   128,  // short-run NOx contribution minimization
      e_shortrunDep      =   256,  // short-run depletable resource use minimization
      e_shortrunLuc      =   512,  // short-run land use minimization

      e_auctionLmp       =  2048,  // locational marginal (nodal) pricing auction
      e_adminMerit       =  4096,  // prescribed merit order
      e_adminFirst       =  8192,  // first feasible solution

      // define some mode sums -- especially useful for 'xeona::isTwoContained' calls

      e_normalCapModes   =  e_withholdOkay     |
                            e_withholdBan,

      e_crisisCapModes   =  e_crisisOperation,

      e_capacityModes    =  e_usePresets       |
                            e_normalCapModes   |
                            e_crisisCapModes,

      e_shortrunModes    =  e_shortrunFin      |
                            e_shortrunGhg      |
                            e_shortrunNox      |
                            e_shortrunDep      |
                            e_shortrunLuc,

      e_commitmentModes  =  e_shortrunModes    |
                            e_auctionLmp       |
                            e_adminMerit       |
                            e_adminFirst,

      e_all              =  e_capacityModes    |
                            e_commitmentModes,

      e_maxAggregate     =  32768            // 2**15
    };

} // namespace 'xeona'

#endif // _OSPMODES_H_

//  end of file

