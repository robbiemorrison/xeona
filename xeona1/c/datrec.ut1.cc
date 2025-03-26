//  XEONA ENERGY SYSTEMS MODELING ENVIRONMENT
//
//  file-create-name : datrec.ut1.cc
//  file-create-date : Tue 09-Oct-2007 17:26 UTC
//  file-initiator   : Robbie Morrison <robbie@actrix.co.nz>
//
//  file-role        : model io (file reading, processing, and writing) / unit test
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
//  $URL: file:///home/robbie/svn-root/xeona/futz/trunk/xeona1/c/datrec.ut1.cc $
//
//  GENERAL NOTES FOR THIS FILE
//
//  This is a unit test file and hence not essential to the
//  application proper.

//  LOCAL AND SYSTEM INCLUDES

#include "datio.h"            // unit under test (place early)
#include "recset.h"           // unit under test (place early)

#include "../a/logger.h"      // standard logging functionality (as required)
#include "../c/smart_ptr.h"   // toggle between Boost and TR1 smart pointers
#include ".././common.h"      // common definitions for project (place last)

#include <iostream>           // standard io
#include <string>             // C++ strings

#include <boost/algorithm/string.hpp>   // string recasing, trimming, splitting
#include <boost/filesystem.hpp>         // path objects, iterators, useful operations
#include <boost/foreach.hpp>            // BOOST_FOREACH macro

//  CODE

#if 0 // 0 = normal name, 1 = experiment name
const std::string leafName = "experiment.xem";    // remember to check file exists
# warning "remember to reset .xem file name"
#else
const std::string leafName = "test.xem";
#endif // 0

// ---------------------------------------------------------
//  MAIN FUNCTION   : main
// ---------------------------------------------------------

int
main(int argc, char* argv[])
{
  logga::spLogger logger = logga::ptrLogStream();  // main function logger
  logger->repx(logga::info, "beginning of main", "~~~~~~~~~~");
  logger->addSmartBlank();
  logger->setReportLevel(logga::adhc);       // maximum reporting

  std::ostringstream put;
  put << std::boolalpha;

  std::string stubName(leafName);
  if ( boost::ends_with(stubName, xeona::modelExt) )
    boost::erase_tail(stubName, xeona::modelExt.length());

  {

    // ---------------------------------------------------------
    //  test ONE        : input processing
    // ---------------------------------------------------------

    logger->test(1);

    // create a record set and a data input/output admin object
    RecordSet recset;
    DataIo dataIo(recset);

    // establish model path (simplified code)
    boost::filesystem::path model = boost::filesystem::absolute(leafName);
    put << "model path (complete) : " <<  model            << "\n";
    put << "model path (leaf)     : " <<  model.filename() << "\n";
    logger->putx(logga::dbug, put);

    // read in model
    put << ""                                          << "\n";
    put << "about to call readModel()"                 << "\n";
    put << ""                                          << "\n";
    logger->putx(logga::dbug, put);
    dataIo.readModel(model);                 // only 'd_rawStr' is loaded

    // horizon
    put << ""                                          << "\n";
    put << "about to call locateModelHorizon()"        << "\n";
    logger->putx(logga::dbug, put);
    unsigned horizon
      = dataIo.locateModelHorizon();         // locate and set horizon
    put << "    locateModelHorizon() yields : " << horizon << "\n";
    logger->putx(logga::dbug, put);

    // process model
    put << ""                                          << "\n";
    put << "about to call processModel()"              << "\n";
    logger->putx(logga::dbug, put);
    dataIo.processModel();                   // process model values

    // ---------------------------------------------------------
    //  test TWO        : dump
    // ---------------------------------------------------------

    logger->test(2);

    // dump record set
    put << "about to call dump()"                      << "\n";
    logger->putx(logga::dbug, put);
    recset.dump();                           // call using default "---"

    // ---------------------------------------------------------
    //  test THREE      : recover specific information
    // ---------------------------------------------------------

    logger->test(3);

    put << "about to call getSubset(xeona::e_entity)"  << "\n";
    logger->putx(logga::dbug, put);
    std::vector<shared_ptr<Record> > users = dataIo.getSubset(xeona::e_entity);
    put << "    size of users vector    : " << users.size() << "\n";
    logger->putx(logga::dbug, put);

    put << ""                                          << "\n";
    put << "about to call getSubset(xeona::e_program)" << "\n";
    logger->putx(logga::dbug, put);
    std::vector<shared_ptr<Record> > programs = dataIo.getSubset(xeona::e_program);
    put << "    size of programs vector : " << programs.size() << "\n";
    logger->putx(logga::dbug, put);

    put << ""                                          << "\n";
    put << "about to call getSubset(xeona::e_note)"    << "\n";
    logger->putx(logga::dbug, put);
    std::vector<shared_ptr<Record> > notes = dataIo.getSubset(xeona::e_note);
    put << "    size of notes vector    : " << notes.size() << "\n";
    logger->putx(logga::dbug, put);

    // ---------------------------------------------------------
    //  test FOUR       : write out model
    // ---------------------------------------------------------

    // CAUTION: _XUTEST may well, depending on the current state
    // of development, disable some of the process order
    // protection code

    logger->test(4);

    std::string leafName2(stubName);         // generated generically
    leafName2 += ".write";                   // add .write
    leafName2 += xeona::modelExt;            // add .xem
    boost::filesystem::path model2 = boost::filesystem::absolute(leafName2);

    put << "about to call writeModel(" << model2 << ")" << "\n";
    logger->putx(logga::dbug, put);
    dataIo.writeModel(model2);               // write out model to file

    // ---------------------------------------------------------
    //  test FIVE       : write to console (will fail)
    // ---------------------------------------------------------

    logger->test(5);

    put << "about to call writeModel() (will fail as currently implemented)" << "\n";
    logger->putx(logga::dbug, put);
    logger->flush();

    dataIo.writeModel();                     // write out model to console

    // ---------------------------------------------------------
    //  test SIX        : reinstate original model file
    // ---------------------------------------------------------

    logger->test(6);

    put << "about to reinstate original model file if possible"  << "\n";
    logger->putx(logga::dbug, put);

    // reinstate original model file if possible

    std::string leafName3(stubName);     // generated generically
    leafName3 += ".guard";               // add .guard
    leafName3 += xeona::modelExt;        // add .xem
    boost::filesystem::path modelGuard = boost::filesystem::absolute(leafName3);

    if ( exists(modelGuard) )
      {
        logger->repx(logga::dbug, "reinstating model using", modelGuard.filename());
        boost::filesystem::remove(model);    // copy_file will not overwrite
        boost::filesystem::copy_file(modelGuard, model);
      }
    else
      {
        logger->repx(logga::info, "file not found", modelGuard.filename());
        logger->repx(logga::info, "not possible to reinsate model", "");
      }

  } // block scope causes Records to be destructed

  // ---------------------------------
  //  HOUSEKEEPING
  // ---------------------------------

  logger->test(0);                           // test end

  logger->repx(logga::info, "end of main", "~~~~~~~~~~");
  return xeona::exit_success;

} // main function

//  end of file

