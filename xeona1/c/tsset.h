//  XEONA ENERGY SYSTEMS MODELING ENVIRONMENT
//
//  file-create-name : tsset.h
//  file-create-date : Tue 24-May-2011 23:57 UTC
//  file-initiator   : Robbie Morrison <robbie@actrix.co.nz>
//
//  file-role        : timeseries classes for added functionality / header
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
//  $URL: file:///home/robbie/svn-root/xeona/futz/trunk/xeona1/c/tsset.h $
//
//  GENERAL NOTES FOR THIS FILE
//
//  The treatment of timeseries here is based on the MID-POINT
//  convention -- whereby a value is deemed to represent the
//  middle of its given timespan.
//
//  As currently coded, timeseries involve only 'double's and
//  'int's must be 4-bytes or more to calculate time intervals in
//  seconds.

//  HEADER GUARD

#ifndef _TSSET_H_
#define _TSSET_H_

//  LOCAL AND SYSTEM INCLUDES

#include "../i/gnuplot.h"     // gnuplot interface, originally by Daniel Stahlke
#include "../b/entity.h"      // entity base class plus lazy linking

#include "../a/logger_fwd.h"  // some forward declarations from "logger.h"
#include "../c/smart_ptr.h"   // toggle between Boost and TR1 smart pointers

#include <iostream>           // standard io
#include <sstream>            // string-streams
#include <stdexcept>          // runtime_error()
#include <string>             // C++ strings
#include <vector>             // STL sequence container

#include <boost/tuple/tuple.hpp>             // n-tuples, ref(), cref()

//  FORWARD (PARTIAL) DECLARATIONS

//  CODE

// ---------------------------------------------------------
//  CLASS           : TsBase (abstract)
// ---------------------------------------------------------
//  Description  : base functionality including the actual timeseries vector
//  Role         : abstract base class for 'TsNormal' 'TsMonthly'
//  Techniques   : inheritance, 'swap' for copy constructing, value semantics preferred
//  Status       : complete
//
//  Design notes
//
//      This abstract class is used for two concrete
//      specializations:
//
//        * timeseries  : normally up to 366 days at various resolutions
//        * monthseries : normally up to 12 months in steps of one month
//
//      The treatment of timeseries is based on the mid-point
//      convention -- whereby a value is deemed to represent the
//      middle of its given sampling interval.
//
//      With regard to normal timeseries, some sampling interval
//      values values are acceptable, ranging from 300 (5 min) to
//      86400s (1 day).  Moreover the 'TsBase::s_intervals[]'
//      data needs to be entirely 'consistent' with the
//      TimeHorizon class.
//
//      Monthly timeseries are based on calendar months, one
//      entry for each calendar month.  Special regard is given
//      to leap years.
//
//      The statistics processing is fully described in the
//      implementation file.
//
//      The "swap" idiom is described in Sutter and Alexandrescu
//      (2005 pp100-101) under "Whenever it makes sense, provide
//      a no fail swap".  Note also "swapping is useful for
//      classes with value [as apposed to pointer] semantics"
//      (p101), which is the case here
//
//  CAUTION: member function 'std::vector::reserve'
//
//      STL vector reserve() can produce nasty physical
//      side-effects on partially filled vectors -- the function
//      will not change the logical contents, but it can
//      invalidate iterators and such, see Josuttis (1999 p151
//      footnote 7).
//
//  REFERENCES
//
//      Sutter, Herb and Andrei Alexandrescu.  2005.  C++ coding
//        standards : 101 rules, guidelines, and best practices
//        Addison-Wesley, Boston, USA.  ISBN 0-321-11358-6.
//
// ---------------------------------------------------------

class TsBase
{
  // LOCAL ENUMERATIONS

public:

  enum SampleMethod                          // using in sample calls
    {
      e_notSpecified          = 0,
      e_duplic                = 1,           // simple duplication
      e_linear                = 2,           // linear interpolation
      e_spline                = 9            // spline fit (for future use)
    };

  // DISABLED

private:

  TsBase();
  TsBase& operator= (const TsBase& orig);    // copy assignment oper, 'swap' used instead

  // CREATORS

public:

  TsBase                                     // no-data constructor
  (const std::string& label);

  TsBase                                     // usual constructor
  (const std::string&        label,
   const std::vector<double> series,
   const bool                wrappable = false);

  TsBase(const TsBase& orig);                // copy constructor

  virtual ~TsBase() = 0;                     // abstract

  // SWAP SUPPORT

protected:

  void swap(TsBase& rhs);                    // swap helper, used by derived classes

  // SIMPLE ACCESSORS

public:

  // individual value

  double
  value                                      // recover an individual value
  (const unsigned index) const               // see 'size' below
    throw(std::out_of_range);                // exception specification

  // obtain series in various formats, possibly after applying 'std::vector::resize'

  const std::vector<double>&                 // by constant reference
  recover() const;

  shared_ptr<std::vector<double> >           // uses 'new'
  pointer
  (const unsigned resize = 0) const;         // may call 'resize', zero to disable

  int                                        // size of target
  copyfill                                   // uses 'std::copy' and 'std::back_inserter'
  (std::vector<double>& vec,
   const unsigned       resize = 0) const;   // may 'resize', zero to disable

  int                                        // size of target
  copyfill                                   // uses 'std::copy' and 'std::back_inserter'
  (shared_ptr<std::vector<double> > vec,
   const unsigned                   resize = 0) const; // may 'resize', zero to disable

  // text information

  std::string getLabel()      const;         // mandatory
  std::string getCaller()     const;         // optional, but useful when debugging

  // metadata (check also for further relevant calls in the derived classes)

  bool        empty()         const;
  unsigned    size()          const;
  bool        isWrappable()   const;
  double      getScaling()    const;
  double      getOffsetting() const;

  // series statistics (calculated on construction)

  int         getCount()      const;
  double      getSum()        const;
  double      getMean()       const;
  double      getStdDev()     const;
  double      getMedian()     const;
  double      getMin()        const;
  double      getMax()        const;
  double      getRange()      const;
  double      getFirst()      const;
  double      getLast()       const;

  boost::tuple
  <int,                                      // count
   double,                                   // sum
   double,                                   // mean
   double,                                   // stddev
   double,                                   // median
   double,                                   // min
   double,                                   // max
   double,                                   // range
   double,                                   // first
   double>                                   // last
  getStatistics() const;

  // MANIPULATORS

  std::string
  setCaller                                  // optional registration
  (const std::string& caller);               // either __func__ or __PRETTY_FUNCTION__

  void
  rescale                                    // cumulative if called successively
  (const double       scaleFactor,           // applied BEFORE offsetting
   const std::string& sublabel);

  bool
  unscale                                    // remove all rescaling
  (const std::string& sublabel);

  void
  reoffset                                   // cumulative if called successively
  (const double       offsetFactor,          // applied AFTER scaling
   const std::string& sublabel);

  bool
  unoffset                                   // remove all offsetting
  (const std::string& sublabel);

  void
  clear();                                   // but 'd_label' and 'd_caller' persist

  // STATIC MANIPULATORS

  static
  bool                                       // prior value
  checksOn();                                // additional checks like comparing means

  static
  bool                                       // prior value
  checksOff();

  // UTILITY FUNCTIONS

protected:

  bool                                       // 'true' if supported
  confirmInterval                            // compare with 'TimeHorizon::checkInterval'
  (const int interval) const;

  void
  calcStatistics();                          // overarching call

  double
  calcStdDev() const;

  template <int N>
  double
  nthMoment() const;

  template <int N>                           // N is the nth power
  struct SumDiffNthPower                     // binary functor
  {
    SumDiffNthPower (const double mean);
    double operator() (double rol, double cur) const;
    const double d_mean;                     // remains unchanged
  };

  double
  calcPercentile
  (const double fraction) const;

  bool                                       // 'true' means actioned
  scalarMultiply                             // applied to each element
  (const double scaleFactor);

  bool                                       // 'true' means actioned
  scalarAdd                                  // applied to each element
  (const double offsetFactor);

  bool                                       // 'true' means consider equal
  considerEqual                              // includes close-to-zero leniency
  (const double mean1,
   const double mean2) const;

  // INSTANCE DATA

protected:

  std::string                      d_label;       // label, also used for gnuplotting
  std::string                      d_caller;      // name of caller (optional)

  std::vector<double>              d_series;      // timeseries or monthseries
  bool                             d_wrappable;   // some timeseries can be looped
  unsigned                         d_size;        // container length
  bool                             d_empty;       // 'true' if empty
  double                           d_scaling;     // cumulative rescaling
  double                           d_offsetting;  // cumulative offsetting

  unsigned                         d_count;
  double                           d_sum;
  double                           d_mean;
  double                           d_stddev;
  double                           d_median;      // 50 percentile
  double                           d_min;
  double                           d_max;
  double                           d_range;
  double                           d_first;
  double                           d_last;

  // STATIC DATA

private:

  static const int                 s_cvals[];     // 12 acceptable intervals [1]
  static const std::vector<int>    s_intervals;

  // [1] a C-style int array was required because it has to be
  // explicitly instantiated in the implementation file -- note
  // that the Boost.Assign library cannot do this on one line (as
  // is required), but that the upcoming C++11 standard will
  // support initializer lists

protected:

  static bool                  s_checks;     // 'true' for additional reporting and tests
  static const double          s_nan;        // not-a-number, streams as "nan"
  static const double          s_inf;        // infinity, streams as "inf"

  static logga::spLogger       s_logger;     // shared_ptr to single logger object

}; // class 'TsBase'

// ---------------------------------------------------------
//  CLASS           : TsNormal
// ---------------------------------------------------------
//  Description  : 'std::vector' wrapper with additional meta-data and statistics
//  Role         : client usage
//  Techniques   : explicit copy ctor using 'swap', 'std::vector'
//  Status       : complete
//
//  Design notes
//
//      A container for so-called normal timeseries with uniform
//      sampling.  Twelve sampling intervals are supported
//      ranging from 300 (5 min) to 86400s (1 day) -- in contrast
//      to a monthseries which necessarily samples over calender
//      months of varying lengths.  Both containers may also
//      include data for 29-Feb.  And both can make the necessary
//      adjustments between normal and leap years on demand.
//
//      This class (and class 'TsMonthly') only supports the
//      'double' type as data (it was originally templated but
//      maintaining a consistent semantics across integral and
//      continuous data-types proved to be complicated.)
//
//      The statistics processing draws on the statistics code
//      contained in Stephens etal (2006 pp394-445 ch11).
//      However Stephens etal do not discuss on-the-fly
//      calculations, whereby the values are discarded and only
//      the statistics are kept.
//
//      See also the implementation file documentation.
//
// ---------------------------------------------------------

class TsNormal :
  public TsBase
{
  // FRIENDS

  friend bool operator== (const TsNormal&, const TsNormal&);

  // DISABLED

private:

  TsNormal();                                // zero-argument constructor

  // CREATORS

public:

  TsNormal                                   // no-data constructor, used as a return
  (const std::string& label);

  TsNormal                                   // usual constructor, direct
  (std::vector<double> original,
   const int           interval,
   const std::string&  label,
   const bool          hasFeb29  = false,
   const bool          wrappable = false)
    throw (std::domain_error);

  TsNormal                                   // usual constructor, smart pointer
  (shared_ptr<std::vector<double> > original,
   const int                        interval,
   const std::string&               label,
   const bool                       hasFeb29  = false,
   const bool                       wrappable = false)
    throw (std::domain_error);

  TsNormal                                   // repetition constructor
  (const int                        steps,
   const double                     value,
   const int                        interval,
   const std::string&               label,
   const bool                       hasFeb29  = false,
   const bool                       wrappable = false)
    throw (std::domain_error);

  TsNormal(const TsNormal& orig);                 // copy constructor

  TsNormal& operator= (const TsNormal& orig);     // copy assignment operator

  ~TsNormal();                                    // destructor

  // SWAP SUPPORT

protected:

  void swap(TsNormal& rhs);

  // LOAD - requires empty object

public:

  bool                                       // 'true' indicates success
  load                                       // as per constructor, direct
  (std::vector<double> original,
   const int           interval,
   const bool          hasFeb29  = false,
   const bool          wrappable = false)
    throw (std::domain_error);

  bool                                       // 'true' indicates success
  load                                       // as per constructor, smart pointer
  (shared_ptr<std::vector<double> > original,
   const int                        interval,
   const bool                       hasFeb29  = false,
   const bool                       wrappable = false)
    throw (std::domain_error);

  bool                                       // 'true' indicates success
  load                                       // as per constructor, repetition
  (const int    steps,
   const double value,
   const int    interval,
   const bool   hasFeb29  = false,
   const bool   wrappable = false)
    throw (std::domain_error);

  // SIMPLE ACCESSORS

public:

  // meta data

  int  getInterval() const;
  bool hasFeb29()    const;
  int  getSpan()     const;

  // comparison

  bool                                       // 'true' if the same
  sameMetaData
  (const TsNormal& other) const;

  // confirmation

  int                                        // whole seconds
  getDurationSeconds() const;

  double                                     // fractional hours
  getDurationHours() const;

  bool                                       // 'true' if sufficient
  hasSufficientData                          // irrespective of resampling
  (const int interval,                       // client seconds
   const int steps,                          // client steps count
   const int startOffset = 0) const;         // client count

  // reporting

  // monthly  = 12  21.22  [10.3 99.3]
  std::string
  summary                                    // no trailing newline
  (const int tab = 0) const;                 // 'tab' right-aligns the label

  void
  report                                     // multi-line report
  (std::ostream& os,
   const int     indent = 2) const;

  //  SAMPLING ACCESSORS

  TsNormal
  sampleDuplicate
  (const int          interval    = Entity::getHorizonInterval(),  // interval length [s]
   const int          startOffset = 0,
   const bool         leapYear    = false,
   const double       scaleFactor = 1.0,                           // applied first
   const double       offset      = 0.0) const;                    // applied second

  TsNormal
  sampleDuplicate
  (const std::string& label,                                       // label version
   const int          interval    = Entity::getHorizonInterval(),  // interval length [s]
   const int          startOffset = 0,
   const bool         leapYear    = false,
   const double       scaleFactor = 1.0,
   const double       offset      = 0.0) const;

  TsNormal
  sampleLinear
  (const int          interval    = Entity::getHorizonInterval(),  // interval length [s]
   const int          startOffset = 0,
   const bool         leapYear    = false,
   const double       scaleFactor = 1.0,                           // applied first
   const double       offset      = 0.0) const;                    // applied second

  TsNormal
  sampleLinear
  (const std::string& label,                                       // label version
   const int          interval    = Entity::getHorizonInterval(),  // interval length [s]
   const int          startOffset = 0,
   const bool         leapYear    = false,
   const double       scaleFactor = 1.0,
   const double       offset      = 0.0) const;

  TsNormal
  sample                                     // workhorse
  (const std::string&         label,
   const int                  interval,      // interval length [s]
   const int                  startOffset,
   const bool                 leapYear,
   const double               scaleFactor,
   const double               offset,
   const TsBase::SampleMethod method) const; // enum

  shared_ptr<std::vector<double> >
  dayInfo                                    // for convenience
  (const int                  pivot,         // centre-point of  interval
   const int                  interval = Entity::getHorizonInterval(),
   const bool                 leapYear = Entity::getLeapYear(),
   const TsBase::SampleMethod method   = TsBase::e_linear) const;

  void
  clear                                      // remove underlying data
  (const std::string& label = "");           // retains label if none given

  bool                                       // 'false' means incompatible metadata
  add                                        // add a commensurate timeseries
  (const TsNormal&    more,                  // as coded, can add itself
   const std::string& sublabel);

  // UTILITY FUNCTIONS

private:

  bool
  sameMeta                                   // used to determine commensurability
  (const TsNormal& other) const;

  // INSTANCE DATA

private:

  int     d_interval;                        // interval [s]
  bool    d_hasFeb29;                        // 'true' means timeseries has 29-Feb data
  int     d_span;                            // span [s]

}; // class 'TsNormal'

// ---------------------------------------------------------
//  CLASS           : TsMonthly
// ---------------------------------------------------------
//  Description  : like 'TsNormal' but tailored for monthly timeseries data
//  Role         : client usage
//  Techniques   : explicit copy ctor, 'std::vector'
//  Status       : complete
//
//  Design notes
//
//      A container for monthly timeseries, which may (as
//      currently coded) range between 1 and 12 values inclusive.
//      Such series do not have a uniform time interval, but
//      naturally vary in span from 28 to 31 days.  This
//      non-uniform sampling prevents this data from being
//      treated as a normal timeseries.  In passing, the average
//      (non-leap year) month would be 30.42 days in length.
//      This class, like 'TsMonthly' also supports data for
//      29-Feb and can make the necessary adjustments between
//      normal and leap years on demand.
//
//      The admittedly unusual term 'monthseries' was coined for
//      this kind of data.
//
//      This class only supports the 'double' type in timeseries.
//
// ---------------------------------------------------------

class TsMonthly :
  public TsBase
{
  // FRIENDS

  friend bool operator== (const TsMonthly&, const TsMonthly&);

  // DISABLED

private:

  TsMonthly();                                    // zero-argument constructor

  // CREATORS

public:

  TsMonthly                                       // usual constructor, direct
  (const std::string& label);

  TsMonthly                                       // usual constructor, smart pointer
  (std::vector<double> original,
   const std::string&  label);

  TsMonthly                                       // monthly values constructor
  (shared_ptr<std::vector<double> > original,
   const std::string&               label);

  TsMonthly(const TsMonthly& orig);               // copy constructor

  TsMonthly& operator= (const TsMonthly& orig);   // copy assignment operator

  ~TsMonthly();                                   // destructor

  // SWAP SUPPORT

protected:

  void swap(TsMonthly& rhs);

  // SIMPLE ACCESSORS

public:

  bool                                       // 'true' if the same
  sameMetaData
  (const TsMonthly& other) const;

  // reporting

  // example:   new =   730  43200s  8760h  26.5793  [-49.6593 97.0657]
  std::string
  summary                                    // no trailing newline
  (const int tab = 0) const;                 // 'tab' right-aligns the label

  void
  report                                     // multi-line report
  (std::ostream& os,
   const int     indent = 2) const;

  // SAMPLING ACCESSORS

  TsNormal
  sampleDuplicate
  (const int          interval    = Entity::getHorizonInterval(),  // interval length [s]
   const int          startOffset = 0,
   const bool         leapYear    = false,
   const double       scaleFactor = 1.0,                           // applied first
   const double       offset      = 0.0) const;                    // applied second

  TsNormal
  sampleDuplicate
  (const std::string& label,
   const int          interval    = Entity::getHorizonInterval(),  // interval length [s]
   const int          startOffset = 0,
   const bool         leapYear    = false,
   const double       scaleFactor = 1.0,
   const double       offset      = 0.0) const;

  TsNormal
  sample                                     // workhorse
  (const std::string&         label,
   const int                  interval,      // interval length [s]
   const int                  startOffset,
   const bool                 leapYear,
   const double               scaleFactor,
   const double               offset,
   const TsBase::SampleMethod method) const; // enum

  // UTILITY FUNCTIONS

private:

  bool
  sameMeta
  (const TsMonthly& other) const;

  // STATIC DATA

private:

  static const unsigned                 s_mls[];            // month-lengths
  static const std::vector<unsigned>    s_monthLengths;     // month-length data

}; // class 'TsMonthly'

// ---------------------------------------------------------
//  CLASS           : TsGnuplot
// ---------------------------------------------------------
//  Description  : plot one or more timeseries in a consistent way, truncate on request
//  Role         : client usage to plot data, particularly during code development
//  Techniques   : 'gnuplot' utility, gnuplot interface, 'Boost.Date_Time' library
//  Status       : complete -- works well but may need to be tweaked for your system
//
//  Design notes
//
//      The 'gnuplot' command is hard-coded as
//      'TsGnuplot::s_gnuplot', obtained in turn from
//      'xeona::gnuplot' in 'common.cc'.  This string can take
//      command-line options too.
//
//      Objects of this class cannot be copied.
//
//  Gnuplot 4.4
//
//     Gnuplot version 4.4 is recommended.  Although this code
//     was originally developed with 4.2.
//
// ---------------------------------------------------------

class TsGnuplot
{
  // DISABLED

private:

  TsGnuplot(const TsGnuplot& orig);               // copy constructor
  TsGnuplot& operator= (const TsGnuplot& orig);   // copy assignment operator

  // CREATORS

public:

  TsGnuplot();                               // zero-argument constructor

  ~TsGnuplot();                              // destructor

  // ACCESSORS

  std::string
  generateTimestampUtc() const;              // normally for internal use

  // MANIPULATORS

  void
  addCaller                                  // added to right-hand side of plot
  (const std::string file,                   // usually __FILE__
   const int         line);                  // usually __LINE__

  bool                                       // 'false' means 'series' rejected
  addTs
  (const TsNormal&  series);

  bool                                       // 'false' means 'series' rejected
  addTs
  (const TsMonthly& series,
   const int        fakeInterval = 1);       // monthseries lack consistent sampling [1]

  // [1] but the sample base is known, so this could be extended to plot properly

  int                                        // number of deletions
  reset();

  bool                                       // 'false' means no attempt to plot
  plot
  (const std::string& title,                 // plot title
   const int          truncate = 0,          // zero is do not truncate
   const char         plottype = 'p') const; // for gnuplot "stylespec" in { 'l' 'p' 's' }

  // INTERNAL DATA

private:

  std::vector<TsNormal>        d_metas;
  std::string                  d_caller;
  std::string                  d_gnuplot;    // gnuplot invocation string with options

  static logga::spLogger       s_logger;     // shared_ptr to single logger object

}; // class 'TsGnuplot'

// ---------------------------------------------------------
//  FREE FUNCTION   : operator== (TsNormal&, TsNormal&)
// ---------------------------------------------------------
//  Description  : operates on two 'TsNormal' objects
//  Role         : general use
//  Techniques   : member data comparison
//  Status       : complete
// ---------------------------------------------------------

bool
operator==
(const TsNormal& lhs,
 const TsNormal& rhs);

// ---------------------------------------------------------
//  FREE FUNCTION   : operator!= (TsNormal&, TsNormal&)
// ---------------------------------------------------------
//  Description  : operates on two 'TsNormal' objects
//  Role         : general use
//  Techniques   : member data comparison
//  Status       : complete
// ---------------------------------------------------------

bool
operator!=
(const TsNormal& lhs,
 const TsNormal& rhs);

// ---------------------------------------------------------
//  FREE FUNCTION   : operator== (TsMonthly&, TsMonthly&)
// ---------------------------------------------------------
//  Description  : operates on two 'TsMonthly' objects
//  Role         : general use
//  Techniques   : member data comparison
//  Status       : complete
// ---------------------------------------------------------

bool
operator==
(const TsMonthly& lhs,
 const TsMonthly& rhs);

// ---------------------------------------------------------
//  FREE FUNCTION   : operator!= (TsMonthly&, TsMonthly&)
// ---------------------------------------------------------
//  Description  : operates on two 'TsMonthly' objects
//  Role         : general use
//  Techniques   : member data comparison
//  Status       : complete
// ---------------------------------------------------------

bool
operator!=
(const TsMonthly& lhs,
 const TsMonthly& rhs);

// ---------------------------------------------------------
//  FREE FUNCTION   : sameMetaData (TsNormal)
// ---------------------------------------------------------

bool
sameMetaData
(const TsNormal& lhs,
 const TsNormal& rhs);

// ---------------------------------------------------------
//  FREE FUNCTION   : sameMetaData (TsMonthly)
// ---------------------------------------------------------

bool
sameMetaData
(const TsMonthly& lhs,
 const TsMonthly& rhs);

#endif // _TSSET_H_

//  end of file

