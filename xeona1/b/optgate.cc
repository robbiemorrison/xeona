//  XEONA ENERGY SYSTEMS MODELING ENVIRONMENT
//
//  file-create-name : optgate.cc
//  file-create-date : Wed 25-Mar-2009 21:08 UTC
//  file-initiator   : Robbie Morrison <robbie@actrix.co.nz>
//
//  file-role        : various OSPs for gateways / implementation
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
//  $URL: file:///home/robbie/svn-root/xeona/futz/trunk/xeona1/b/optgate.cc $
//
//  GENERAL NOTES FOR THIS FILE

//  LOCAL AND SYSTEM INCLUDES

#include "optgate.h"          // companion header for this file (place first)

#include "../c/util2.h"       // free functions which offer general utilities 2
#include "../c/label.h"       // helper class to format solver labels
#include "../b/optops1.h"     // operate optimization sub-problems for hard assets 1
#include "../b/optctl.h"      // control optimization sub-problems for asset operators
#include "../b/bandtaf.h"     // banded tariff set and support

#include "../a/logger.h"      // standard logging functionality (as required)
#include "../c/smart_ptr.h"   // toggle between Boost and TR1 smart pointers
#include ".././common.h"      // common definitions for project (place last)

#include <string>             // C++ strings
#include <sstream>            // string-streams

#include <boost/format.hpp>   // printf style formatting

//  CODE

// NOTE: this simple way of swapping OSP implementations, the
// "_X" postfix needs changing in this one place only -- note
// also that an implementation change is required to honor the
// same function names and signatures, but not necessarily the
// behavior at large.

// ---------------------------------------------------------
//  CLASS           : QanTechCapacity_A
// ---------------------------------------------------------

// ---------------------------------------------------------
//  MEMBER FUNCTION : QanTechCapacity_A
// ---------------------------------------------------------

QanTechCapacity_A::QanTechCapacity_A
(shared_ptr<svif::SolverIf> solver,
 const xeona::DomainMode    commitmentMode,
 const std::string          tag) :
  QuantityOsp(solver, commitmentMode, "qan-tech-cap-a"),
  d_cols(),
  d_capacity()
{
  s_logger->repx(logga::xtra, "constructor call, description", d_ospDesc);
}

// ---------------------------------------------------------
//  MEMBER FUNCTION : ~QanTechCapacity_A
// ---------------------------------------------------------

QanTechCapacity_A::~QanTechCapacity_A()
{
  s_logger->repx(logga::xtra, "destructor call", "");
}

// ---------------------------------------------------------
//  MEMBER FUNCTION : uploadCapacity
// ---------------------------------------------------------
//  Description  : capacity characterization
//  Role         : host call
//  Techniques   : (nothing special)
//  Status       : complete
// ---------------------------------------------------------

QanTechCapacity_A::index_type
QanTechCapacity_A::uploadCapacity            // wrapper
(const std::pair<double, double> capacity)
{
  return uploadCapacity(capacity.first, capacity.second);
}

QanTechCapacity_A::index_type
QanTechCapacity_A::uploadCapacity
(const double loCapacity,
 const double hiCapacity)
{
  // initial reporting
  s_logger->repx(logga::xtra, "entering member function", "");

  // PREAMBLE

  // create and fill a label object
  Label lab(d_label);

  // create a stand-in specific costs cost set
  const CostSet zeroSpecCosts(0.0);

  // INTEGRITY CHECKS

  // very basic integrity checking
  if ( loCapacity > hiCapacity
       ||
       loCapacity < 0.0 )
    {
      std::ostringstream oss;
      oss << loCapacity << " : " << hiCapacity;
      s_logger->repx(logga::warn, "aphysical capacities, lo : hi", oss.str());
    }

  // save capacity for use in 'downloadSolution'
  d_capacity  = hiCapacity;

  // EXPOSED VARIABLES

  // create an exposed variable -- the zero specific cost is, of
  // course, fine
  const int capCol = pushObj(zeroSpecCosts, lab.str("duty"));

  // load the local col index into a 1-tuple
  d_cols = boost::make_tuple(capCol);

  // CAPACITY BALANCES

  // create the capacity balances
  const int loRow  = pushRhs(loCapacity, svif::G, lab.str("lo-bal"));
  const int hiRow  = pushRhs(hiCapacity, svif::L, lab.str("hi-bal"));

  // add the exposed variable
  d_cofCount       = pushCof(loRow, capCol, +1.0);
  d_cofCount       = pushCof(hiRow, capCol, +1.0);

  // GLOBALIZE AND RETURN

  // note that external usage requires global indexing
  return globalcols(d_cols);

} // function 'QanTechCapacity_A::uploadCapacity'

// ---------------------------------------------------------
//  MEMBER FUNCTION : downloadSolution
// ---------------------------------------------------------
//  Description  : solution recovery
//  Role         : host call
//  Techniques   : (nothing special)
//  Status       : complete
// ---------------------------------------------------------

QanTechCapacity_A::results_type
QanTechCapacity_A::downloadSolution() const
{
  // initial reporting
  s_logger->repx(logga::xtra, "entering member function", "");

  // identify transmission results
  const double duty = downloadVar(d_cols.get<0>());

  // additional reporting as appropriate
  // YEEK 32 CODE (set by '--yeek')
  if ( xeona::yeek == 32 || xeona::yeek == 1 || xeona::yeek == 2 )
    {
      const std::string func = XEONA_FUNC;   // preprocessor macro defined in 'common.h'
      std::ostringstream put;
      put << "  OSP report (+ means returned)"         << "\n"
          << "      function    : " << func            << "\n"
          << "      description : " << d_ospDesc       << "\n"
          << "      label       : " << d_label         << "\n"
          << "      interval    : " << "not passed in" << "\n"
          << "      size        : " << d_capacity      << "\n"
          << "    calculated"                          << "\n"
          << "    + duty        : " << duty            << "\n";
      s_logger->repx(logga::dbug, "additional reporting follows, yeek", xeona::yeek);
      s_logger->putx(logga::dbug, put);
    }

  // return
  return boost::make_tuple(duty);

} // function 'QanTechCapacity_A::downloadSolution'

// ---------------------------------------------------------
//  CLASS           : OfrTariffSet_A
// ---------------------------------------------------------

// ---------------------------------------------------------
//  MEMBER FUNCTION : OfrTariffSet_A
// ---------------------------------------------------------

OfrTariffSet_A::OfrTariffSet_A
(shared_ptr<svif::SolverIf> solver,
 const xeona::DomainMode    commitmentMode,
 const std::string          tag) :
  OfferOsp(solver, commitmentMode, "ofr-tariffset-a"),
  d_cols(),
  d_tariffset(),
  d_capacity()
{
  s_logger->repx(logga::xtra, "constructor call", "");
}

// UPLOAD CALLS

// ---------------------------------------------------------
//  MEMBER FUNCTION : uploadTariffSet
// ---------------------------------------------------------
//  Description  : fill the solver using intermediate calls
//  Role         : host usage
//  Techniques   : sorted vectors
//  Status       : complete
//
//  Design notes
//
//      The ORDER OF INSERTION for a 'BandedTariffSet' instance
//      is significant -- in contrast to an LMP bidset in which
//      the bids are sorted on load.  This function must
//      therefore respect the order of insertion.
//
//      The 'original' tariff set argument is copy constructed to
//      local 'tariffset' to avoid damaging the original.
//
//  Piecewise linearization
//
//      The piecewise linearization formultation used here relies
//      on a posting by GLPK author Andrew Makhorin in 2007.  See
//      'glpk-sos2_02.pdf' (or latter) for a typeset version of
//      that same posting, available from:
//
//        http://winglpk.sourceforge.net/media/glpk-sos2_02.pdf
//
//      Very similar material is contained in my (Robbie
//      Morrison) thesis write-up.
//
//      For additional material, see Croxton etal (2003) and Keha
//      etal (2004).
//
//  References
//
//      Croxton, Keely L, Bernard Gendron, and Thomas L Magnanti.
//        2003.  A comparison of mixed-integer programming models
//        for non-convex piecewise linear cost minimization
//        problems.  Management Science.  v49 pp1268-1273.
//
//      Keha, Ahmet B, de Farias, Ismael R, Jr, and Nemhauser,
//        George L.  2004.  Models for representing piecewise
//        linear cost functions.  Operations Research Letters
//        vol32 iss1 pp44-48.  ISSN 0167-6377.  doi:
//        10.1016/S0167-6377(03)00059-2.
//
// ---------------------------------------------------------

OfrTariffSet_A::index_type
OfrTariffSet_A::uploadTariffSet              // forget about the fixed charge for now
(const shared_ptr<BandedTariffSet> original,
 const double                      capacity)
{
  // initial reporting
  s_logger->repx(logga::xtra, "entering member function", "");

  // PREAMBLE

  Label colLabel(d_label);                   // 'OptimSubProb' data member
  Label rowLabel(d_label);                   // 'OptimSubProb' data member

  int cofCount = 0;                          // not actually used for much

  // COPY CONSTRUCT THE TARIFFSET
  // avoids destroying the original

  shared_ptr<BandedTariffSet> tariffset(new BandedTariffSet(*original));

  // PRELIMINARY CHECKS

  if ( tariffset->empty() )
    {
      const std::string label = tariffset->getLabel();
      s_logger->repx(logga::rankJumpy, "empty tariff set submitted", label);
      return boost::make_tuple(0);
    }
  if ( capacity < 0.0 )
    {
      s_logger->repx(logga::warn, "negative capacity supplied", capacity);
    }

  // CODE

  // save tariff set and capacity for use in 'downloadSolution'
  d_tariffset = tariffset;
  d_capacity  = capacity;

  // load up OSP if using short-run financial cost minimization,
  // else create an empty problem
  switch ( d_commitmentMode )
    {
    case xeona::e_shortrunFin:               // short-run financial cost minimization
      {
        // ---------------------------------
        //  short-run financial commitment
        // ---------------------------------

        // the initial code is the same for convex and non-convex
        // tariffs

        // process labels
        colLabel << tariffset->getLabel();
        rowLabel << tariffset->getLabel();

        // set up sale variable and constraint
        const int saleCol = pushObj(0.0, colLabel.str("sale"));
        const int saleRow = pushRhs(0.0, svif::E, rowLabel.str("sale-bal"));
        cofCount          = pushCof(saleRow, saleCol, -1.0);

        // load the local index into a 1-tuple
        d_cols = boost::make_tuple(saleCol);

        // process the fixed charge
        const double specFixed   = tariffset->getSpecificFixedCharge();

#if 1 // CAUTION: this macro is prompted by a definitional issue
      // for the clients of this OSP -- at the time of writing,
      // just class 'GateStatedTariff<>'
      //
      // 1 = time-specific fixed charge [$/s] returned
      // 0 = time-and-capacity-specific fixed charge [$/*/s/s] (energy [$/W/s]) returned
        const double fixedCharge = specFixed;
#else
        const double fixedCharge = specFixed * capacity;
#endif // 0

        s_logger->repx(logga::adhc, "fixed charge component", fixedCharge);
        if ( specFixed != 0.0 )
          {
            pushIncShift(fixedCharge);       // incremental
          }

        // process the bidsets based on tariff set convexity
        switch ( tariffset->isConvex() )
          {
          case true:                           // convex tariff set, linear programme
            {
              // ---------------------------------
              //  convex tariffs
              // ---------------------------------

              // convexity reporting
              s_logger->repx(logga::adhc, "convex tariffset", "");

              // loop the tariffs
              int loop = 1;
              while ( ! tariffset->empty() )
                {
                  // process labels
                  colLabel << boost::format("tariff-%d") % loop;
                  rowLabel << boost::format("tariff-%d") % loop;

                  // grab tariff, working left to right on price curve
                  const tariff_type tariff = tariffset->popFirst();  // oldest current

                  // tariff extensity call (band, price)
                  const int col = pushObj(tariff.second, colLabel.str(""));

                  // tariff constraint calls
                  const int row = pushRhs(tariff.first, svif::L, rowLabel.str(""));
                  cofCount      = pushCof(row, col, +1.0);

                  // bind tariff to sale
                  cofCount      = pushCof(saleRow, col, +1.0);

                  // pop the last on labelette in preparation for another loop
                  colLabel.trim(1);
                  rowLabel.trim(1);

                  // housekeeping
                  ++loop;
                }
            }
            break;

          case false:                          // non-convex tariff set, need binary vars
            {
              // ---------------------------------
              //  non-convex tariffs
              // ---------------------------------

              // the equation designators ONE, TWO, THREE, and
              // FOUR relate to the Makhorin piecewise linear
              // formulation -- which is described and cited in
              // the documentation for this function

              // convexity reporting
              s_logger->repx(logga::adhc, "non-convex tariffset", "");

              // first up, equation THREE: represented by 'saleRow'

              // define the segment endpoints: left and rite (right)
              double xleft = 0.0;
              double yleft = 0.0;
              double xrite = 0.0;            // CAUTION: must be zero
              double yrite = 0.0;            // CAUTION: must be zero

              // create equation ONE
              const int oneSegRow = pushRhs(1.0, svif::E, rowLabel.str("one-segment"));

              // loop the tariffs
              int loop = 1;
              while ( ! tariffset->empty() )
                {
                  // process labels
                  colLabel << boost::format("tariff-%d") % loop;
                  rowLabel << boost::format("tariff-%d") % loop;

                  // grab tariff, working left to right on price curve
                  const tariff_type tariff = tariffset->popFirst();  // oldest current
                  const double band        = tariff.first;  // band
                  const double price       = tariff.second; // unit price

                  // calculate the segment endpoints -- first
                  // swap right to left and then add the increments
                  xleft = xrite;
                  yleft = yrite;
                  xrite = xleft +  band;
                  yrite = yleft + (band * price);

                  // create two block local variables, trailed by "Col"
                  //   "b" is a binary variable with determine which tariff is active
                  //   "s" is the segment variable

                  // this code also enacts equation FOUR: yleft * b + yband * s
                  const int bCol = pushObj(yleft        , colLabel.str("binary"));
                  const int sCol = pushObj(yrite - yleft, colLabel.str("segment"));
                  markBinary(bCol);          // returns same col for convenience

                  // update constraint equation ONE: sum b = 1
                  cofCount       = pushCof(oneSegRow, bCol, 1.0);

                  // make new constraint equation TWO: 0 <= s <= b
                  // note default global non-negativity condition for structural variables
                  const int sRow = pushRhs(0.0, svif::L, rowLabel.str("s-equation"));
                  cofCount       = pushCof(sRow, sCol,  1.0);
                  cofCount       = pushCof(sRow, bCol, -1.0);

                  // update constraint equation THREE:  xleft * b + xband * s
                  cofCount       = pushCof(saleRow, bCol, xleft        );
                  cofCount       = pushCof(saleRow, sCol, xrite - xleft);

                  // pop the last on labelette in preparation for another loop
                  colLabel.trim(1);
                  rowLabel.trim(1);

                  // housekeeping
                  ++loop;
                }

            }
            break;

          } // inner 'switch' on 'isConvex' boolean

        // GLOBALIZE AND RETURN
        // note that external usage requires global indexing

        return boost::make_tuple(globalcol(saleCol));
      }

    case xeona::e_shortrunGhg:               // non-financial
    case xeona::e_shortrunNox:
    case xeona::e_shortrunDep:
    case xeona::e_shortrunLuc:
    case xeona::e_auctionLmp:
    case xeona::e_adminMerit:
    case xeona::e_adminFirst:
      {
        // ---------------------------------
        //  all other commitment modes
        // ---------------------------------

        // process labels
        colLabel << tariffset->getLabel();

        // set up sale variable and constraint
        const int saleCol = pushObj(0.0, colLabel.str("sale"));

        // load the local index into a 1-tuple
        d_cols = boost::make_tuple(saleCol);

        // GLOBALIZE AND RETURN
        // note that external usage requires global indexing

        return boost::make_tuple(globalcol(saleCol));
      }

    default:
      std::clog << "** coding error 01 in source file " << __FILE__ << std::endl;
      return boost::make_tuple(0);

    } // outer 'switch' on 'xeona::DomainMode' enum

  // should never get here

} // function 'OfrTariffSet_A::uploadTariffSet'

// ---------------------------------------------------------
//  MEMBER FUNCTION : downloadSolution
// ---------------------------------------------------------
//  Description  : download solution
//  Role         : host usage
//  Techniques   : essentially 'BandedTariffSet::interpretSale' passed thru
//  Status       : complete
//
//  Design notes
//
//      Most of the hard work is done buy
//      'BandedTariffSet::interpretSale'.
//
//      Note to that the capacity information given and utilized
//      in 'uploadTariffSet' is also needed here.
//
// ---------------------------------------------------------

OfrTariffSet_A::results_type
OfrTariffSet_A::downloadSolution
(const int interval) const
{
  // initial reporting
  s_logger->repx(logga::xtra, "entering member function", "");

  // additional reporting as required
  if ( xeona::releaseStatus == false )
    {
      const int rowCount = getRowCount();    // using accessor interface
      const int colCount = getColCount();

      std::ostringstream oss;
      oss << rowCount << " x " << colCount;

      if ( colCount == 0 )                   // priority given to no cols
        {
          s_logger->repx(logga::warn, "zero col count, rows x cols", oss.str());
          s_logger->repx(logga::dbug, "label", d_label);
        }
      else if ( rowCount == 0 )
        {
          s_logger->repx(logga::rankJumpy, "zero row count, rows x cols", oss.str());
          s_logger->repx(logga::dbug, "label", d_label);
        }
      else
        {
          s_logger->repx(logga::adhc, "rows x cols", oss.str());
          s_logger->repx(logga::adhc, "label", d_label);
        }
    }

  // identify contract results
  const double size = d_capacity;
  const double sale = downloadVar(d_cols.get<0>());

  // additional reporting as appropriate
  // YEEK 32 CODE (set by '--yeek')
  if ( xeona::yeek == 32 || xeona::yeek == 1 || xeona::yeek == 2 )
    {
      const std::string func = XEONA_FUNC;   // preprocessor macro
      std::ostringstream put;
      put << "  OSP report (+ means returned)"         << "\n"
          << "      function    : " << func            << "\n"
          << "      description : " << d_ospDesc       << "\n"
          << "      label       : " << d_label         << "\n"
          << "    + interval    : " << interval        << "\n"
          << "    + size        : " << size            << "\n"
          << "    calculated"                          << "\n"
          << "    + sale        : " << sale            << "\n";
      s_logger->repx(logga::dbug, "additional reporting follows, yeek", xeona::yeek);
      s_logger->putx(logga::dbug, put);
    }

  // return (direct out is okay)
  return d_tariffset->interpretSale(size, sale, interval);

} // function 'OfrTariffSet_A::downloadSolution'

// ---------------------------------------------------------
//  CLASS           : QanObligToSupply_A
// ---------------------------------------------------------

// ---------------------------------------------------------
//  MEMBER FUNCTION : QanObligToSupply_A
// ---------------------------------------------------------

QanObligToSupply_A::QanObligToSupply_A
(shared_ptr<svif::SolverIf> solver,
 const xeona::DomainMode    commitmentMode,
 const std::string          tag) :
  QuantityOsp(solver, commitmentMode, "qan-oblig-a"),
  d_cols()
{
  s_logger->repx(logga::xtra, "constructor call", "");
}

// UPLOAD CALLS

// ---------------------------------------------------------
//  MEMBER FUNCTION : uploadObligation
// ---------------------------------------------------------
//  Description  : fill the solver using intermediate calls
//  Role         : host usage
//  Techniques   : (nothing special)
//  Status       : complete
// ---------------------------------------------------------

QanObligToSupply_A::index_type
QanObligToSupply_A::uploadObligation
(const double supplyObligation)
{
  // initial reporting
  s_logger->repx(logga::xtra, "entering member function", "");

  // PRELIMINARY CHECKS

  // supply obligations only apply to 'transolve' calls and not
  // to 'capset' calls -- the call type could perhaps be passed
  // thru to here to allow improved scrutiny but at present this
  // information is not forwarded

  double supply = supplyObligation;
  std::string append;
  if ( xeona::isNan(supplyObligation) )
    {
      s_logger->repx(logga::info, "nil obligation received", supplyObligation);
      supply = 0.0;
      append = "-NIL";
    }
  else if ( supplyObligation < 0.0 )
    {
      s_logger->repx(logga::warn, "negative obligation received", supplyObligation);
      append = "-FAULTY";
    }

  // PREAMBLE

  // create and fill a label object
  Label lab(d_label);

  // create a stand-in specific costs cost set
  const CostSet zeroSpecCosts(0.0);

  // EXPOSED VARIABLES

  // create an exposed variable -- the zero specific costs is, of
  // course, fine
  const int oblCol = pushObj(zeroSpecCosts, lab.str("obligation"));

  // load the local col index into a 1-tuple
  d_cols = boost::make_tuple(oblCol);

  // OBLIGATION BALANCE

  // create the obligation balance
  const int oblRow = pushRhs(supply, svif::E, lab.str("supply-obl" + append));

  // add the exposed variable
  d_cofCount       = pushCof(oblRow, oblCol, +1.0);

  // GLOBALIZE AND RETURN

  // note that external usage requires global indexing
  return globalcols(d_cols);

} // function 'QanObligToSupply_A::uploadObligation'

//  end of file

