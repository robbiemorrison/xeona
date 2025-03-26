//  XEONA ENERGY SYSTEMS MODELING ENVIRONMENT
//
//  file-create-name : app_except.h
//  file-create-date : Tue 28-Apr-2009 10:14 UTC
//  file-initiator   : Robbie Morrison <robbie@actrix.co.nz>
//
//  file-role        : application exception classes / header
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
//  $URL: file:///home/robbie/svn-root/xeona/futz/trunk/xeona1/a/exapp.h $
//
//  GENERAL NOTES FOR THIS FILE
//
//  This unit implements a hierarchy of custom exception classes
//  for application use.  This hierarchy does not derive from
//  'std::exception'.

//  HEADER GUARD

#ifndef _EXAPP_H_
#define _EXAPP_H_

//  LOCAL AND SYSTEM INCLUDES

#include "../a/exbase.h"      // xeona exception base class

#include "../c/smart_ptr.h"   // toggle between Boost and TR1 smart pointers

#include <iostream>           // standard io
#include <sstream>            // string-streams
#include <string>             // C++ strings

//  CAUTION: the '<stdexcept>' header is not needed here but is
//  useful in client code which employs exception specifications
//  in member function declarations: type func(..) throw(..);

#include <stdexcept>          // standard exception classes, runtime_error()

//  CODE

// ---------------------------------------------------------
//  documentation   : Exception classes
// ---------------------------------------------------------
//
//  Entity authors
//
//      Authors coding concrete entities should use class
//      'ent_exception' or (better) a dedicated specialization of
//      this class.  These new specialization can be added to (a
//      possibly new) unit 'ent_except' and not this unit
//      'app_except'.
//
//  New 'xeona::reason' exception file modifications
//
//      common.{h,cc}   : new 'xeona::exit_reason' exit status
//      a/exapp.{h,cc}  : new 'xeona::reason' exception
//      a/exitstat.cc   : new 'add' call
//      z/client.{h,cc} : add exception specification and, of course, throw call
//
//  Design considerations
//
//      The following authors were consulted on the design of
//      such classes: Lischner (2003), Loudon (2003), Stephens
//      etal (2006), Stroustrup (1997), and Sutter and
//      Alexandrescu (2005).
//
//      The only advice not taken was that from Sutter and
//      Alexandrescu who suggested (p56) it was "preferable" to
//      virtually derive from 'std::exception'.  The inheritance
//      tree used here is stand alone.  And, on reflection, there
//      are clear benefits in keeping STL and xeona exceptions
//      separate.
//
//      Exception classes need to supply:
//
//          - a public destructor
//          - a no-fail constructor
//          - a no-fail copy constructor
//
//      The design here allows for custom information to be
//      embedded, but no logging calls occur.
//
//  Class hierarchy (in namespace 'xeona')
//
//      exception (abstract)            base class
//        + app_exception (abstract)    application use intermediary
//            + kill_on_log             exit if "exit on kill log" set
//            + non_registration        unregistered entity name requested
//            + empty_wrap              misalignment of model data and entity code
//            + short_timeseries        short timeseries
//        + ent_exception (abstract)    for use by entity authors intermediary
//            + entity_issue            general use
//            + ...
//
//  Compiler-supplied copy constructor
//
//      Note the "exception(const exception& orig)" copy
//      constructor supplied by the compiler is okay.
//
//  String formatting conventions
//
//      The formatting of 'd_stringExpl' should add a trailing
//      new line.  The member function 'expl' simply returns this
//      string.
//
//      The formatting of 'd_stringTell' should not add a
//      trailing new line.  The member function 'tell' simply
//      returns this string.
//
//  Throw call example
//
//      throw ( xeona::short_timeseries(getName(), delta) );
//
//  Catch block example
//
//      // within 'main' function
//
//      catch ( const xeona::app_exception& x )
//      {
//        std::ostringstream put;
//        put << x.expl();
//        logger->putx(logga::warn, put);
//        logger->flush();
//
//        std::cout << "  " << x.tell() << " caught"
//                  << ", execution abandoned" << std::endl;
//        std::cout << std::flush;
//
//        return x.code();
//      }
//
// ---------------------------------------------------------

namespace xeona
{

  // ---------------------------------------------------------
  //  CLASS           : xeona::app_exception (abstract)
  // ---------------------------------------------------------
  //  Description  : abstract base class for application exceptions
  //  Role         : use in application code rather than concrete entities
  //  Techniques   : exception class, inheritance tree
  //  Status       : complete
  // ---------------------------------------------------------

  class app_exception :
    public exception
  {
  private:

    app_exception();
    app_exception& operator= (const app_exception& orig);   // copy assignment operator

  public:

    app_exception
    (const int exitcode);

    virtual ~app_exception() = 0;            // create abstract class

  };

  // ---------------------------------------------------------
  //  CLASS           : xeona::kill_on_log
  // ---------------------------------------------------------
  //  Description  : exit on kill "exception" (see design notes)
  //  Role         : 'a/logger.cc' and 'Logger::endOfLogCall (const logga::Rank)'
  //  Techniques   : exception class, inheritance tree
  //  Status       : complete
  //
  //  Design notes
  //
  //      This usage could be considered an abuse of the
  //      exception handling system.
  //
  //      This exception is hopefully but not necessarily caught
  //      by 'main' because loggers can reside in global objects.
  //      But, at the time of writing (r2500), that is not the
  //      case.
  //
  //  Actual function
  //
  //      void
  //      Logger::endOfLogCall
  //      (const logga::Rank rank)
  //
  // ---------------------------------------------------------

  class kill_on_log :
    public app_exception
  {
  private:

    kill_on_log& operator= (const kill_on_log& orig);  // copy assignment oper

  public:

    kill_on_log();

  };

  // ---------------------------------------------------------
  //  CLASS           : xeona::non_registration
  // ---------------------------------------------------------
  //  Description  : non-registration exception
  //  Role         : 'c/factory.cc' and 'EntityFactory::createEntityBind'
  //  Techniques   : exception class, inheritance tree
  //  Status       : complete
  //
  //  Actual function
  //
  //      shared_ptr<Entity>
  //      EntityFactory::createEntityBind
  //      (const std::string  entityRegn,
  //       const std::string& entityId,
  //       Record&            r)
  //
  //  Known data
  //
  //       Identifier, requested class (but read client code for
  //       discussion of issues), and entity registration known.
  //
  // ---------------------------------------------------------

  class non_registration :
    public app_exception
  {
  private:

    non_registration();                                          // zero-argument ctor
    non_registration& operator= (const non_registration& orig);  // copy assignment oper

  public:

    non_registration
    (const std::string entityIdentifier,     // 'entityId'
     const std::string entityRegistration,   // 'entityRegn'
     const std::string requestedClass);      // 'r.locateClass()'

  };

  // ---------------------------------------------------------
  //  CLASS           : xeona::empty_wrap
  // ---------------------------------------------------------
  //  Description  : empty wrap exception
  //  Role         : 'c/recset.h' and 'wrap<T>::wrapExit'
  //  Techniques   : exception class, inheritance tree
  //  Status       : complete
  //
  //  Actual function
  //
  //      template <typename T>
  //      void
  //      wrap<T>::wrapExit()
  //
  //  Known data
  //
  //      Type known.  And the field name would have been logged
  //      just prior.
  //
  // ---------------------------------------------------------

  class empty_wrap :
    public app_exception
  {
  private:

    empty_wrap();                                      // zero-argument ctor
    empty_wrap& operator= (const empty_wrap& orig);    // copy assignment operator

  public:

    empty_wrap
    (const std::string type);                // 'xeona::demangle(typeid(T).name())'

  };

  // ---------------------------------------------------------
  //  CLASS           : xeona::short_timeseries
  // ---------------------------------------------------------
  //  Description  : short timeseries exception
  //  Role         : 'c/recset.cc' and 'Field::splitRawStr'
  //  Techniques   : exception class, inheritance tree
  //  Status       : complete
  //
  //  Actual function
  //
  //      void
  //      Field::splitRawStr()
  //
  //  Known data
  //
  //      Field name and shortfall known.
  //
  // ---------------------------------------------------------

  class short_timeseries :
    public app_exception
  {
  private:

    short_timeseries();                                          // zero-argument ctor
    short_timeseries& operator= (const short_timeseries& orig);  // copy assignment oper

  public:

    short_timeseries
    (const std::string fieldName,            // 'getName()'
     const int         shortfall);           // 'int delta = horizon - len'

  };

  // ---------------------------------------------------------
  //  CLASS           : xeona::file_not_found
  // ---------------------------------------------------------
  //  Description  : file not found exception
  //  Role         : various, including 'main.cc' and 'main'
  //  Techniques   : exception class, inheritance tree
  //  Status       : complete
  //
  //  Known data
  //
  //      File name, both leaf and absolute.
  //
  // ---------------------------------------------------------

  class file_not_found :
    public app_exception
  {
  private:

    file_not_found();                                            // zero-argument ctor
    file_not_found& operator= (const file_not_found& orig);      // copy assignment oper

  public:

    file_not_found
    (const std::string filename,
     const std::string comment);

  };

  // ---------------------------------------------------------
  //  CLASS           : xeona::xem_data_issue
  // ---------------------------------------------------------
  //  Description  : xem issue
  //  Role         : used in unit 'recset'
  //  Techniques   : exception class, inheritance tree
  //  Status       : complete
  //
  //  Known data
  //
  //      In most cases, the "type" and identifier.
  //
  // ---------------------------------------------------------

  class xem_data_issue :
    public app_exception
  {
  private:

    xem_data_issue();                                            // zero-argument ctor
    xem_data_issue& operator= (const xem_data_issue& orig);      // copy assignment oper

  public:

    xem_data_issue
    (const std::string comment,
     const std::string name,
     const std::string valueStr);

  };

  // ---------------------------------------------------------
  //  CLASS           : xeona::lazy_link_fail
  // ---------------------------------------------------------
  //  Description  : lazy link issue
  //  Role         : used in unit 'recset'
  //  Techniques   : exception class, inheritance tree (see also 'full_link_fail')
  //  Status       : complete
  //
  //  Known data
  //
  //      The sought linkname.
  //
  // ---------------------------------------------------------

  class lazy_link_fail :
    public app_exception
  {
  private:

    lazy_link_fail();                                            // zero-argument ctor
    lazy_link_fail& operator= (const lazy_link_fail& orig);      // copy assignment oper

  public:

    lazy_link_fail
    (const std::string comment,
     const std::string linkname);

  };

  // ---------------------------------------------------------
  //  CLASS           : xeona::cannot_run_guard
  // ---------------------------------------------------------
  //  Description  : cannot run guard file exception
  //  Role         : use in 'main.cc' and 'main'
  //  Techniques   : exception class, inheritance tree
  //  Status       : complete
  //
  //  Known data
  //
  //      File name.
  //
  // ---------------------------------------------------------

  class cannot_run_guard :
    public app_exception
  {
  private:

    cannot_run_guard();                                          // zero-argument ctor
    cannot_run_guard& operator= (const cannot_run_guard& orig);  // copy assignment oper

  public:

    cannot_run_guard
    (const std::string filename);

  };

  // ---------------------------------------------------------
  //  CLASS           : xeona::bad_authorship
  // ---------------------------------------------------------
  //  Description  : application-centric tests on badly authored entities
  //  Role         : use in 'DomainController::establishDomain'
  //  Techniques   : exception class, inheritance tree
  //  Status       : complete
  //
  //  Known data
  //
  //      Number of failed entities.
  //
  // ---------------------------------------------------------

  class bad_authorship :
    public app_exception
  {
  private:

    bad_authorship();                                            // zero-argument ctor
    bad_authorship& operator= (const bad_authorship& orig);      // copy assignment oper

  public:

    bad_authorship
    (const int numberFailedEntities);

  };

  // ---------------------------------------------------------
  //  CLASS           : xeona::full_link_fail
  // ---------------------------------------------------------
  //  Description  : link entity (information flows) failure
  //  Role         : use in 'Entity::retFull'
  //  Techniques   : exception class, inheritance tree (see also 'lazy_link_fail')
  //  Status       : complete
  //
  //  Known data (perhaps)
  //
  //      Identifier, cast type, my type (with full/link information).
  //
  // ---------------------------------------------------------

  class full_link_fail :
    public app_exception
  {
  private:

    full_link_fail();                                        // zero-argument constructor
    full_link_fail& operator= (const full_link_fail& orig);  // copy assignment operator

  public:

    full_link_fail
    (const std::string identifier,
     const std::string etype,
     const std::string mtype);

  };

  // ---------------------------------------------------------
  //  CLASS           : xeona::empty_field_on_write
  // ---------------------------------------------------------
  //  Description  : 'Field::d_timeseries' remains default constructed on write out
  //  Role         : use in 'Field::getTimeseries' (zero argument variant)
  //  Techniques   : exception class, inheritance tree
  //  Status       : complete
  //
  //  Known data
  //
  //      Single/timeseries, field name.
  //
  // ---------------------------------------------------------

  class empty_field_on_write :
    public app_exception
  {
  private:

    empty_field_on_write();                                             // zero-arg ctor
    empty_field_on_write& operator= (const empty_field_on_write& orig); // copy assgn opor

  public:

    empty_field_on_write
    (const std::string fieldType,            // "single" or "timeseries"
     const std::string fieldname);

  };

  // ---------------------------------------------------------
  //  CLASS           : xeona::timeseries_not_found
  // ---------------------------------------------------------
  //  Description  : timeseries field is missing
  //  Role         : use in 'Field::tieTimeseries'
  //  Techniques   : exception class, inheritance tree
  //  Status       : complete
  //
  //  Known data
  //
  //      Sought field name, template type.
  //
  // ---------------------------------------------------------

  class timeseries_not_found :
    public app_exception
  {
  private:

    timeseries_not_found();                                             // zero-arg ctor
    timeseries_not_found& operator= (const timeseries_not_found& orig); // copy assgn opor

  public:

    timeseries_not_found
    (const std::string fieldname,            // "timeseries"
     const std::string templateType);

  };

  // ---------------------------------------------------------
  //  CLASS           : xeona::yeek_abandon
  // ---------------------------------------------------------
  //  Description  : yeek abandon
  //  Role         : debugging
  //  Techniques   : exception class, inheritance tree
  //  Status       : complete
  //
  //  Known data
  //
  //      Value of 'xeona::yeek'.
  //
  // ---------------------------------------------------------

  class yeek_abandon :
    public app_exception
  {
  private:

    yeek_abandon();                                         // zero-arg ctor
    yeek_abandon& operator= (const yeek_abandon& orig);     // copy assgn opor

  public:

    yeek_abandon
    (const std::string explanation);         // as required

  };

  // ---------------------------------------------------------
  //  CLASS           : xeona::bad_submodel
  // ---------------------------------------------------------
  //  Description  : bad submodel (like the Sandia PV model) encountered
  //  Role         : normal usage
  //  Techniques   : exception class, inheritance tree
  //  Status       : complete
  //
  //  Known data
  //
  //      Submodel name, explanation.
  //
  // ---------------------------------------------------------

  class bad_submodel :
    public app_exception
  {
  private:

    bad_submodel();                                         // zero-arg ctor
    bad_submodel& operator= (const bad_submodel& orig);     // copy assgn opor

  public:

    bad_submodel
    (const std::string submodel,
     const std::string explanation);

  };

  // ---------------------------------------------------------
  //  CLASS           : xeona::hour_resolution_only
  // ---------------------------------------------------------
  //  Description  : entity encountered which only supports hourly resolution
  //  Role         : normal usage
  //  Techniques   : exception class, inheritance tree
  //  Status       : complete
  //
  //  Known data
  //
  //      Entity name, identity, comment, interval.
  //
  // ---------------------------------------------------------

  class hour_resolution_only :
    public app_exception
  {
  private:

    hour_resolution_only();                                             // zero-arg ctor
    hour_resolution_only& operator= (const hour_resolution_only& orig); // copy assgn oper

  public:

    hour_resolution_only
    (const std::string entity,
     const std::string identity,
     const std::string comment,
     const unsigned    interval);

  };

  // ---------------------------------------------------------
  //  CLASS           : xeona::invalid_interval
  // ---------------------------------------------------------
  //  Description  : only certain interval values are valid
  //  Role         : TimeHorizon
  //  Techniques   : exception class, inheritance tree
  //  Status       : complete
  //
  //  Known data
  //
  //      Offending interval value, list of valid values.
  //
  // ---------------------------------------------------------

  class invalid_interval :
    public app_exception
  {
  private:

    invalid_interval();                                          // zero-arg constructor
    invalid_interval& operator= (const invalid_interval& orig ); // copy assgn operator

  public:

    invalid_interval
    (const int         invalid,
     const std::string valids);

  };

  // ---------------------------------------------------------
  //  CLASS           : xeona::bad_subentity_label
  // ---------------------------------------------------------
  //  Description  : for incorrect sub-entity labels
  //  Role         : function 'Interface::getCm'
  //  Techniques   : exception class, inheritance tree
  //  Status       : complete
  //
  //  Known data
  //
  //      Host identity, caller name (__func__ or __PRETTY_FUNCTION__).
  //
  // ---------------------------------------------------------

  class bad_subentity_label :
    public app_exception
  {
  private:

    bad_subentity_label();                                              // zero-arg ctor
    bad_subentity_label& operator= (const bad_subentity_label& orig );  // copy assgn oper

  public:

    bad_subentity_label
    (const std::string hostId,
     const std::string function);

  };

  // ---------------------------------------------------------
  //  CLASS           : xeona::no_gateway_demander
  // ---------------------------------------------------------
  //  Description  : when the gateway cannot obtain the demander
  //  Role         : function 'Gate<>::getDemander' in two different places
  //  Techniques   : exception class, inheritance tree
  //  Status       : complete
  //
  //  Roles
  //
  //       * first  : get socket partner fails
  //       * second : pointer cast fails
  //
  //  Known data
  //
  //      My identity, caller name (__func__ or __PRETTY_FUNCTION__),
  //      and perhaps my socket partner identity.
  //
  // ---------------------------------------------------------

  class no_gateway_demander :
    public app_exception
  {
  private:

    no_gateway_demander();                                              // zero-arg ctor
    no_gateway_demander& operator= (const no_gateway_demander& orig );  // copy assgn oper

  public:

    no_gateway_demander
    (const std::string myId,
     const std::string function,
     const std::string mySocketPartnerId = "(not available)");

  };

  // ---------------------------------------------------------
  //  CLASS           : xeona::no_gateway_controller
  // ---------------------------------------------------------
  //  Description  : gateway cannot obtain either buy or selside controller
  //  Role         : function 'GateCom<C>::hop' in two different places
  //  Techniques   : exception class, inheritance tree
  //  Status       : complete
  //
  //  Roles
  //
  //       * failure to resolve gateway controller
  //
  //  Known data
  //
  //      My identity, caller name (__func__ or __PRETTY_FUNCTION__),
  //      and buy or selside.
  //
  // ---------------------------------------------------------

  class no_gateway_controller :
    public app_exception
  {
  private:

    no_gateway_controller();                                               // zero-arg ctr
    no_gateway_controller& operator= (const no_gateway_controller& orig ); // copy as oper

  public:

    no_gateway_controller
    (const std::string myId,
     const std::string function,
     const std::string side);

  };

  // ---------------------------------------------------------
  //  CLASS           : xeona::hop_limit_reached
  // ---------------------------------------------------------
  //  Description  : predefined hop limit reached
  //  Role         : function 'GateCom<C>::hop'
  //  Techniques   : exception class, inheritance tree
  //  Status       : complete
  //
  //  Roles
  //
  //       * prevent endless hopping, most likely from an ill-formed model
  //
  //  Known data
  //
  //      My identity, caller name (__func__ or __PRETTY_FUNCTION__),
  //      call count, and hop limit.
  //
  // ---------------------------------------------------------

  class hop_limit_reached :
    public app_exception
  {
  private:

    hop_limit_reached();                                              // zero-arg ctor
    hop_limit_reached& operator= (const hop_limit_reached& orig );    // copy assgn oper

  public:

    hop_limit_reached
    (const std::string myId,
     const std::string function,
     const int         callCount,
     const int         hopLimit);

  };

} // namespace 'xeona'

#endif // _EXAPP_H_

//  end of file

