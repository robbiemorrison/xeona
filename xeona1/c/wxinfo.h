//  XEONA ENERGY SYSTEMS MODELING ENVIRONMENT
//
//  file-create-name : wxinfo.h
//  file-create-date : Fri 05-Nov-2010 11:28 UTC
//  file-initiator   : Robbie Morrison <robbie@actrix.co.nz>
//
//  file-role        : weather data class for testing / header
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
//  $URL: file:///home/robbie/svn-root/xeona/futz/trunk/xeona1/c/wxinfo.h $
//
//  GENERAL NOTES FOR THIS FILE
//
//  The high-resolution data is stored in a header file in
//  subdirectory 'g'.  See the implementation file for details.

//  HEADER GUARD

#ifndef _WXINFO_H_
#define _WXINFO_H_

//  LOCAL AND SYSTEM INCLUDES

#include "../a/logger_fwd.h"  // some forward declarations from "logger.h"
#include "../c/smart_ptr.h"   // toggle between Boost and TR1 smart pointers

#include <string>             // C++ strings
#include <vector>             // STL sequence container

//  CODE

// ---------------------------------------------------------
//  CLASS           : WeatherData
// ---------------------------------------------------------
//  Description  : class to load and serve weather timeseries
//  Role         : supply unit test data for 'BuildingSim' and similar
//  Techniques   : 'std::vector's initialized by C-style arrays
//  Status       : complete
//
//  Design notes
//
//      This class gets its high resolution data from elsewhere.
//      See the implementation for details.
//
//      The returned smart pointer timeseries are SEPARATE from
//      the underlying data members and can therefore be modified
//      with impunity.
//
//      Some datasets are loopable, meaning the dataset can be
//      treated as a ring.  This feature is normally limited to
//      specially constructed TMY (typical meteorological year)
//      timeseries.  TMY do not typically have a year.
//
//      The option of making this class abstract and then placing
//      the data-sets in suitably-named derived classes was
//      considered.  But rejected because, at this point, there
//      was no compelling reason to do so.
//
// Underlying data
//
//      With regard to the underlying data, the array length
//      variable '::length' submitted to the constructor MUST BE
//      accurate.  If too long, the vectors will silently load
//      garbage!  It is difficult to test for poor data
//      preparation, so the best defense is to get it right first
//      time.  (Perhaps 'Boost.Assign' would be better practice.)
//
//  Deep copying
//
//      If further deep copies are required, note the vector copy
//      free function 'xeona::vectorCopy<>':
//
//          shared_ptr<std::vector<double> > source
//          shared_ptr<std::vector<double> > target
//          xeona::vectorCopy(source, target)
//
//  CAUTION: this class is NOT an entity
//
//      This class provides unit test support to other classes
//      like 'BuildingSim', but is not an entity in its own
//      right.
//
//      This class is also used by 'CxAmbientSolarTMY', which
//      provides a direct interface to the solar data.
//
// ---------------------------------------------------------

class WeatherData
{

  // DISABLED

private:

  WeatherData(const WeatherData& orig);                // copy constructor
  WeatherData& operator= (const WeatherData& orig);    // copy assignment operator

  // CREATORS

public:

  WeatherData();                             // zero-argument constructor
  ~WeatherData();                            // destructor

  // ACCESSORS

public:

  void metadata (std::ostream& os) const;    // meta-data includes location and length

  std::string getDescription() const;
  std::string getFile()        const;

  int    getInterval()  const;               // interval length [s]
  int    getLength()    const;               // number of elements
  int    getYear()      const;               // calendar year, can be zero (if TMY)
  double getLatitude()  const;               // latitude [degrees]
  double getLongitude() const;               // longitude [degrees]
  double getElevation() const;               // height above mean sea level [m]
  double getTimezone()  const;               // offset from UTC [h]
  bool   isLoopable()   const;               // 'true' if Dec to Jan is seamless
  bool   hasFeb29()     const;               // 'true' if 29-Feb data present

  // the returned vectors are, in all cases, "new" vectors and
  // can therefore be modifed at will

  typedef shared_ptr<std::vector<double> > timeseries_type;

  timeseries_type getSolarDirect()   const;
  timeseries_type getSolarDifuse()   const;
  timeseries_type getZenithAngles()  const;
  timeseries_type getAzimuthAngles() const;
  timeseries_type getAirTemps()      const;
  timeseries_type getWindSpeeds()    const;

  // truncated versions of above, which return the first 'trunc'
  // elements, duly offset by 'offset'
  //
  // if the underlying dataset is loopable, then these calls will
  // loop as required
  //
  // if there is insufficient data for the request, then the
  // returned vector is tail-padded with zeros and a 'xeona'
  // warnings are logged

  timeseries_type getSolarDirect  (const int trunc, const int offset = 0) const;
  timeseries_type getSolarDifuse  (const int trunc, const int offset = 0) const;
  timeseries_type getZenithAngles (const int trunc, const int offset = 0) const;
  timeseries_type getAzimuthAngles(const int trunc, const int offset = 0) const;
  timeseries_type getAirTemps     (const int trunc, const int offset = 0) const;
  timeseries_type getWindSpeeds   (const int trunc, const int offset = 0) const;

  // STATIC ACCESSORS

public:

  static
  bool                                       // 'false' means problems encountered
  summarizeTimeseries                        // tag, length, first, last, min, max, mean
  (shared_ptr<std::vector<double> > input,   // timeseries
   const std::string&               tag,     // note that "" gives "(not set)"
   std::ostream&                    os,
   const int                        skip = 0);

  // UTILITY FUNCTIONS

private:

  std::vector<double>
  transform                                  // used to initialize data members
  (const double* array);                     // const double array[]

  bool
  rework
  (shared_ptr<std::vector<double> > ts,      // timeseries
   const int                        trunc,
   const int                        offset) const;

  // INSTANCE DATA

private:

  const std::string            d_description;     // optional description
  const std::string            d_file;            // originating file
  const int                    d_interval;        // horizon interval [s]
  const int                    d_length;          // length of one of the C-arrays
  const int                    d_year;            // zero if a TMY composite or similar
  const double                 d_latitude;        // latitude [degrees]
  const double                 d_longitude;       // longitude [degrees]
  const double                 d_elevation;       // height above mean sea level [m]
  const double                 d_timeZone;        // offset from UTC [h]
  const bool                   d_loopable;        // 'true' if Dec to Jan is seamless
  const bool                   d_hasFeb29;        // 'true' for leap year data

  const std::vector<double>    d_solarDirect;
  const std::vector<double>    d_solarDifuse;
  const std::vector<double>    d_zenithAngles;
  const std::vector<double>    d_azimuthAngles;
  const std::vector<double>    d_airTemps;
  const std::vector<double>    d_windSpeeds;

  // STATIC DATA

protected:

  static logga::spLogger       s_logger;     // shared_ptr to single logger object

};

#endif // _WXINFO_H_

//  end of file

