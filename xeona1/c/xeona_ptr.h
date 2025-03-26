//  XEONA ENERGY SYSTEMS MODELING ENVIRONMENT
//
//  file-create-name : xeona_ptr.h
//  file-create-date : Fri 31-Jul-2009 16:26 UTC
//  file-initiator   : Robbie Morrison <robbie@actrix.co.nz>
//
//  file-role        : remappable counted pointer which mimics shared_ptr / header
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
//  $URL: file:///home/robbie/svn-root/xeona/futz/trunk/xeona1/c/xeona_ptr.h $
//
//  GENERAL NOTES FOR THIS FILE
//
//  This is "header-only" code -- meaning that 'xeona_ptr.cc' is
//  empty (and indeed only present to facilitate both object file
//  building and linking and also unit testing).
//
//  Unlike most other units, class template 'assign_ptr<>' does
//  NOT use 'xeona'-style logging, but rather relies on the
//  locally-defined 'Deport' class.  The default output stream
//  was (at the time of writing) set to 'std::clog'.
//
//  It is intended that this code could be used in a stand-alone
//  context with a minimum of change.  However, the hash-includes
//  and namespace declarations from 'smart_ptr' will need to be
//  expressly provided.  Furthermore, the <typeinfo> and
//  <stdexcept> code is protective and can removed without loss
//  of core functionality.

//  HEADER GUARD

#ifndef _XEONA_PTR_H_
#define _XEONA_PTR_H_

//  AD-HOC NOTES
//
//    design issues
//
//      - get the caller-side C type information working in 'reVamp'
//
//    implementation issues and/or bugs
//
//      - the data members in 'xeona::applied_ptr<>' should be private but are public
//      - resolve the 'remove_if' "problem"
//      - perhaps offer more control over 'Deport' verboseness

//  LOCAL AND SYSTEM INCLUDES

#include "../c/util1.h"       // free functions which offer general utilities 1
#include "../c/smart_ptr.h"   // toggle between Boost and TR1 smart pointers
#include ".././common.h"      // common definitions for project (place last)

#include <algorithm>          // STL copying, searching, and sorting
#include <functional>         // STL function objects
#include <iomanip>            // setw() and family
#include <iostream>           // standard io
#include <sstream>            // string-streams
#include <stdexcept>          // standard exception classes, runtime_error()
#include <string>             // C++ strings
#include <typeinfo>           // run-time type information (RTTI), std::bad_cast exception
#include <vector>             // STL sequence container

#include <boost/any.hpp>      // safe type-heterogeneous storage
#include <boost/foreach.hpp>  // BOOST_FOREACH iteration macro
#include <boost/format.hpp>   // printf style formatting

// ELECT USAGE AS INTERNAL OR EXTERNAL TO 'XEONA'

#define XE_ASSIGN_PTR_XEONA_USE              // active means usage by 'xeona'

// ---------------------------------
//  internal use code
// ---------------------------------

#ifdef XE_ASSIGN_PTR_XEONA_USE

#  include "../a/logger.h"    // run-time logging functionality (as required)

// verbosity control
// 0 = fully silent, 1 = warn messages, 2 = internal, 3 = + ctors, 4 = + dtors
#  ifdef _XUTEST                              // originally _XDEBUG
    namespace { const unsigned verbose = 4; }
#  else
    namespace { const unsigned verbose = 0; }
#  endif // _XUTEST

// ---------------------------------
//  external use code
// ---------------------------------

#else // XE_ASSIGN_PTR_XEONA_USE

  namespace xeona { const bool nopro        = false; }
  namespace xeona { const int  exit_success = 0; }
  namespace xeona { std::string demangle (std::string tid) { return tid; } }

#  include <boost/shared_ptr.hpp>                 // Boost shared pointer (serializable)
#  include <boost/weak_ptr.hpp>                   // Boost weak pointer (serializable)
#  include <boost/enable_shared_from_this.hpp>    // Boost smart pointer support

  using boost::shared_ptr;
  using boost::weak_ptr;
  using boost::enable_shared_from_this;
  using boost::const_pointer_cast;
  using boost::static_pointer_cast;
  using boost::dynamic_pointer_cast;
  using boost::bad_weak_ptr;                      // exception class

#endif // XE_ASSIGN_PTR_XEONA_USE

//  CODE

// ---------------------------------------------------------
//  notes           : xeona::assign_ptr overview
// ---------------------------------------------------------
//  Description  : "remappable" counted pointer which partly mimics 'shared_ptr'
//  Role         : primarily developed for the 'xeona' lazy link process
//  Techniques   : pointer semantics, "revamp" function, 'shared_ptr'
//  Status       : complete
//
//  Design notes
//
//      This reference counted pointer class has the ability to
//      switch the controlled resource via the 'revamp' function
//      -- a feature which is intrinsically IMPOSSIBLE with
//      'shared_ptr', as described here:
//
//          Boost Libraries
//          + Smart Pointers
//            + shared_ptr class template
//              + Smart Pointer Programming Techniques
//                + Obtaining a shared_ptr from a raw pointer
//
//      Having said that, this class template relies on
//      'shared_ptr's to do most of its work!
//
//      The idea of self-implemented counted pointers came from
//      Dattatri (2002 pp487-496).
//
//      The interface and usage is modeled on 'shared_ptr'
//      although not all its free and member functions are
//      offered here.  Read the code for details.
//
//      No attempt has been made to make this code run fast.
//      Rather the emphasis has been on design cleanliness and
//      maintainability.
//
//      It is recommended practice not to deploy smart pointers
//      as unnamed temporaries.
//
//      See also:
//      '/usr/local/include/boost-1_39/boost/smart_ptr/shared_ptr.hpp'
//
//  Potential extensions
//
//      Add a 'subsume' function, perhaps by applying
//      'std::set_union' to 'd_clients':
//
//          void subsume(assign_ptr<T> inferiorPool)
//
//      Note that 'swap' and some other functions from
//      'shared_ptr' are not implemented, neither are some of the
//      associated free functions.
//
//      Neither is any support offered for interoperation with
//      'weak_ptr' and 'auto_ptr'.
//
//      The use of custom 'shared_ptr' deleters, passed thru to
//      the underlying resource, could also be considered.
//
//  CAUTION: avoid language-defined types
//
//      This class should not be used on language-defined types
//      such as 'int' because '->' produces strange semantics
//      (Dattatri 2002 p496).
//
//  CAUTION: heap objects only
//
//       Stack objects should not be deployed and will yield
//       run-time faults on attempted deletion.
//
//  CAUTION: 'shared_ptr' construction is non-exclusive
//
//      The use of pre-existing 'shared_ptr' objects in
//      construction and also in 'revamp' and 'reset' calls means
//      that the resource might not be exclusive.  This
//      eventuality can be tested using the member function
//      'exclusive'.
//
//  CAUTION: possibly thread-UNSAFE as implemented
//
//      Consult Dattatri for ideas as to how one might confirm or
//      make this class thread-safe.
//
//  Background
//
//      Alexandrescu (2001, ch7, pp157-195) devotes a chapter to
//      smart pointers and the underlying design trade-offs --
//      however "revamping" is not considered:
//
//      The notion of "revamping" is never far away in Meyers
//      (1996 item29 pp183-213) reference counting example, but
//      neither is the concept directly mentioned.
//
//      Some ideas on the use of the Boost.Any library came from
//      Karlsson (2006 ch6 pp159-189).
//
//      Finally, Dattatri (2002 pp487-496) provides an
//      implementation of counted pointers and discusses the
//      design issues in some depth.
//
//  References
//
//      Alexandrescu, Andrei.  2001.  Modern C++ design : generic
//        programming and design patterns applied.  Addison-Wesley,
//        Boston, USA.  ISBN 0-201-70431-5.
//
//      Dattatri, Kayshav.  2002.  C++ : effective-object
//        oriented software construction : concepts, principles,
//        industrial strategies and practices -- Second edition.
//        Prentice Hall PTR, Upper Saddle River, New Jersey, USA.
//        ISBN 0-13-086769-1.
//
//      Karlsson, Bjoern.  2006.  Beyond the C++ Standard Library
//        : an introduction to Boost.  Addison-Wesley, Upper Saddle
//        River, New Jersey, USA.  ISBN 0-321-13354-4.
//
//      Meyers, Scott.  1996.  More effective C++ : 35 new ways
//        to improve your programs and design.  Addison-Wesley,
//        Boston, USA.  ISBN 0-201-63371-X.
//
// ---------------------------------------------------------

// ---------------------------------------------------------
//  CLASS           : ::Deport (local)
// ---------------------------------------------------------
//  Description  : reporting class
//  Role         : debug reporting
//  Techniques   : file scope, Boost.Format library, '::verbose'
//  Status       : complete
//
//  Overall verbosity control via local constant '::verbose'
//
//      0 = fully silent
//      1 = warn (right-side) messages
//      2 = all internal messages
//      3 = above plus constructor reports
//      4 = above plus destructor reports
//
// ---------------------------------------------------------

namespace
{
  class Deport
  {
    // LOCAL ENUMERATIONS

  private:

    enum Verbose
      {
        silent = 0,
        report = 1
      };

    // DISABLED

  private:

    Deport();                                // zero-argument constructor
    Deport(const Deport& orig);              // copy constructor
    Deport& operator= (const Deport& orig);  // copy assignment operator

  public:

    // CREATORS

    explicit
    Deport
    (const std::string& host,                     // class name
     const std::string& call,                     // member function name
     const Verbose      verbose = report,
     std::ostream&      ostream = std::clog) :    // default ostream set here
       d_ostream(ostream),
       d_flag("-- assign pointer : "),            // margin flag string set here
       d_lead(boost::str(boost::format("%s %12s : %-28s") % d_flag % host % call)),
       d_verbose(verbose)
    {
      // assumes that the object instantiation call was placed at
      // the beginning of the function block
      if ( ::verbose > 2 ) operator()("enter", "", d_verbose);
    }

    ~Deport()
    {
      if ( ::verbose > 3 ) operator()("exit", "", d_verbose);
    }

    // MEMBER FUNCTIONS

    void
    operator()
    (const std::string& msg1,                // left column message
     const std::string& msg2  = "",          // right column message
     const Verbose      verbose = report)    // can set to 'Deport::silent'
    {
      if ( ::verbose == 0 )                  return;   // '::verbose' defined in file
      if ( ::verbose == 1 && msg2.empty() )  return;
      if ( verbose == silent)                return;   // 'verbose' is an argument
      if ( msg1.empty() && msg2.empty() )    return;

      d_ostream << d_lead << " : ";
      if ( msg2.empty() )
        d_ostream << msg1 << std::endl;
      else
        d_ostream << boost::format("%-30s  %38s **") % msg1 % msg2 << std::endl;
    }

    // INSTANCE DATA

  private:

    std::ostream&        d_ostream;
    const std::string    d_flag;
    const std::string    d_lead;
    Verbose              d_verbose;          // private enum

  };

} // unnamed namespace

namespace xeona
{

  // ---------------------------------------------------------
  //  CLASS           : xeona::bad_assign_ptr
  // ---------------------------------------------------------
  //  Description  : C++ exception class
  //  Role         : (none thus far)
  //  Techniques   : C++ exception mechanism
  //  Status       : complete
  // ---------------------------------------------------------

  class bad_assign_ptr
    : public std::exception
  {
  public:

    bad_assign_ptr() :
      d_message()
    {
    }

    bad_assign_ptr
    (const std::string message) :
      d_message(message)
    {
    }

    ~bad_assign_ptr() throw()                // no-throw
    {
    }

    virtual
    char const*
    what() const throw()                     // no-throw
    {
      std::string msg = "xeona::bad_assign_ptr";
      if ( ! d_message.empty() ) msg += ": " + d_message;
      return msg.c_str();
    }

  private:

    const std::string    d_message;

  };

  // ---------------------------------------------------------
  //  CLASS           : xeona::bad_assign_cast
  // ---------------------------------------------------------
  //  Description  : C++ exception class
  //  Role         : as required
  //  Techniques   : C++ exception mechanism
  //  Status       : complete
  // ---------------------------------------------------------

  class bad_assign_cast
    : public std::exception
  {
  public:

    bad_assign_cast() :
      d_message()
    {
    }

    bad_assign_cast
    (const std::string message) :
      d_message(message)
    {
    }

    ~bad_assign_cast() throw()               // no-throw
    {
    }

    virtual
    char const*
    what() const throw()                     // no-throw
    {
      std::string msg = "xeona::bad_assign_cast";
      if ( ! d_message.empty() ) msg += ": " + d_message;
      return msg.c_str();
    }

  private:

    const std::string    d_message;

  };

  // ---------------------------------------------------------
  //  CLASS           : xeona::const_cast_tag
  //  CLASS           : xeona::dynamic_cast_tag
  //  CLASS           : xeona::polymorphic_cast_tag
  // ---------------------------------------------------------
  //  Description  : built into overloaded cast constructors
  //  Role         : used by the associated free function
  //  Techniques   : 'struct'
  //  Status       : complete
  // ---------------------------------------------------------

  struct const_cast_tag       {};
//struct static_cast_tag      {};            // not currently implemented
  struct dynamic_cast_tag     {};
  struct polymorphic_cast_tag {};

  // ---------------------------------------------------------
  //  CLASS           : xeona::Client (abstract base class)
  // ---------------------------------------------------------
  //  Description  : interface class for class 'ClientImp<>'
  //  Role         : support client registration
  //  Techniques   : inheritance, pure virtual functions
  //  Status       : complete -- but see comments elsewhere about polymorphic behavior
  // ---------------------------------------------------------

  class Client
  {
    // DISABLED

  private:

    Client& operator= (const Client& orig);  // copy assignment operator

  public:

    // CREATORS

    virtual
    ~Client()
    {
      Deport deport("Client", "destructor");
    }

    Client()
    {
      Deport deport("Client", "constructor");
    }

    Client(const Client& orig)               // copy constructor, required by 'clone'
    {
    }

    // ACCESSORS

    virtual
    const std::type_info&
    type() const = 0;

    // MANIPULATORS

    virtual
    Client*                                  // derived class template is 'ClientImp<C>*'
    get() = 0;

    virtual
    bool
    update
    (shared_ptr<void> replacement) = 0;      // CAUTION: smart 'void*' generic pointer

    virtual
    bool
    updateAny
    (const boost::any& replacement) = 0;     // CAUTION: payload will need to carry a 'C'

  };

  // ---------------------------------------------------------
  //  CLASS           : xeona::ClientImp <>
  // ---------------------------------------------------------

  template <typename T> class assign_ptr;    // forward (partial) declaration

  template <typename C>
  class ClientImp
    : public Client
  {
    // DISABLED

  private:

    ClientImp();                                       // zero-argument constructor
    ClientImp& operator= (const ClientImp& orig);      // copy assignment operator

  public:

    ~ClientImp()
    {
      Deport deport("ClientImp", "destructor");
    }

    explicit
    ClientImp
    (assign_ptr<C>* client) :
      Client(),
      d_client(client)
    {
      Deport deport("ClientImp", "constructor");
      std::ostringstream oss;
      oss << "C = " << xeona::demangle(typeid(**client).name());
      deport(oss.str());
    }

    ClientImp(const ClientImp& orig) :       // copy constructor, required by 'clone'
      d_client(orig.d_client)                // shallow copy is correct
    {
    }

    // COMPARATORS

    // logical equality function
    friend
    bool
    operator== (const ClientImp<C>& a,
                const ClientImp<C>& b)
    {
      Deport deport("free func", "ClientImp operator==");
      return a.d_client == b.d_client;
    }

    // ACCESSORS

    virtual
    const std::type_info&
    type() const
    {
      return typeid(C);
    }

    // MANIPULATORS

    // "If the function's return type is a pointer (or a
    // reference) to a base class, the derived class's function
    // may return a pointer (or reference) to a class derived
    // from that base class." (Meyers 1996 pp126-127).

    virtual
    ClientImp<C>*
    get()
    {
      return this;
    }

    virtual
    bool
    update
    (shared_ptr<void> replacement)
    {
      //  WARNING: the generic pointer signature here defeats the
      //  strong type system!  And in particular, the following
      //  is not caught because 'static_pointer_cast' and not
      //  'dynamic_pointer_cast' (see below) must be used:
      //
      //     assign_ptr<Next> next(new Next());
      //     next.revamp(new Base());
      //
      //  Shared_ptr type 'void' is deployed because the
      //  following declaration will not compile:
      //
      //      template <typename Y>
      //      virtual
      //      bool update(shared_ptr<Y> globalReplacement)

      // reporting
      Deport deport("ClientImp", __func__);

      // key call
      d_client->d_resource = static_pointer_cast<C>(replacement);

      // check result and return accordingly
      if ( d_client->d_resource == 0 )
        {
          deport("cast from void*", "BAD STATIC_POINTER_CAST");
          return false;                      // return failure
        }
      else
        {
          return true;                       // return success
        }
    }

    virtual
    bool
    updateAny
    (const boost::any& replacement)
    {
      Deport deport("ClientImp", __func__);

      // CAUTION: The Boost.Any cast specifier must be exact --
      // which means the 'replacement' payload must carry a 'C'
      // and nothing else.

      // CAUTION: the definition of 'buffer' in an 'if' test is
      // legal and remains visible in both the 'if' and 'else'
      // blocks (Alexandrescu 2001 p238).

      if ( const shared_ptr<C>* buffer = boost::any_cast<shared_ptr<C> >(&replacement) )
        {
          d_client->d_resource = *buffer;    // guaranteed type match on good cast
          deport("good boost::any_cast");
          return true;
        }
      else
        {
          deport("bad boost::any_cast");
          return false;
        }
    }

    template <typename Y>
    bool
    updateNonVirtual
    (shared_ptr<Y> replacement)
    {
      // reporting
      Deport deport("ClientImp", __func__);

      // key call
      d_client->d_resource = replacement;

      // return success
      return true;
    }

    // INSTANCE DATA

  private:

    assign_ptr<C>*    d_client;

  };

  // ---------------------------------------------------------
  //  CLASS           : xeona::Common
  // ---------------------------------------------------------
  //  Description  : helper class, holds client list, manages the revamp process
  //  Role         : held by 'assign_ptr' as an aggregate data member
  //  Techniques   : 'std::vector', casting
  //  Status       : complete -- but could refactor the 'std::remove_if' predicate
  //
  //  Design notes
  //
  //  CAUTION: 'std::remove_if' design flaws
  //
  //      The member function 'detach' MAY NEED more work related
  //      to the 'std::remove_if' predicate -- in particular read
  //      Karlsson (2006 pp186-188), Josuttis (1999 pp302-304,
  //      378-379), and Lischner (2003 p355).  Note that Lischner
  //      mistakenly states that 'remove_if' acts on 'false'.
  //
  //      Indeed, the functor class template 'match' should
  //      either be wrapped by or reimplemented as a predicate
  //      function.  Moreover, the predicate function/functor
  //      could also meet the STL library adapter requirements.
  //
  //      This is only a problem for 'std::remove_if'.
  //
  // ---------------------------------------------------------

  template <typename T> class assign_ptr;    // forward (partial) declaration
  template <typename M> class match;         // forward (partial) declaration

  class Common
  {
    // DISABLED

  private:

    Common(const Common& orig);              // copy constructor
    Common& operator= (const Common& orig);  // copy assignment operator

  public:

    // CREATORS

    ~Common()                                // destructor
    {
      Deport deport("Common", "destructor");
    }

    Common() :                               // zero-argument constructor
      d_clients(),                           // empty vector
      d_revampCount(0)
    {
      Deport deport("Common", "constructor");
    }

    // GENERAL PURPOSE FUNCTIONS

    // ---------------------------------------------------------
    //  MEMBER FUNCTION : xeona::Common::attach <>
    // ---------------------------------------------------------

    // register me
    template <typename T>
    void attach(assign_ptr<T>* client)
    {
      Deport deport("Common", __func__);

      shared_ptr<Client> temp(new ClientImp<T>(client));
      d_clients.push_back(temp);

      std::ostringstream oss;
      oss << "client count = " << d_clients.size()
          << ", "
          << "revamp count = " << d_revampCount;
      deport(oss.str());
    }

    // ---------------------------------------------------------
    //  MEMBER FUNCTION : xeona::Common::detach <>
    // ---------------------------------------------------------

    // unregister me
    template <typename T, typename Y>
    void detach(assign_ptr<Y>* client)
    {
      Deport deport("Common", __func__);

      shared_ptr<ClientImp<Y> > temp(new ClientImp<Y>(client));

      // delete (remove and erase) ALL matches -- whilst noting
      // that 'erase' is a 'std::vector' member function and
      // 'std::remove_if' is from <algorithm>

      const unsigned before = d_clients.size();        // [1]
      d_clients.erase(std::remove_if(d_clients.begin(),
                                     d_clients.end(),
                                     match<Y>(temp)),  // [2]
                      d_clients.end());

      // [1] size type: strictly speaking, member function 'size'
      // returns type 'std::vector<shared_ptr<Client>
      // >::size_type' -- which is 'unsigned' on my system
      //
      // [2] CAUTION: 'std::remove_if' predicate: see notes
      // elsewhere!

      const int diff = before - d_clients.size();
      if ( diff != 1 )
        {
          deport("unique removal failed", "DIFF NOT ONE");
          if ( xeona::nopro == false )       // meaning option '--krazy' not applied
            {
              std::ostringstream ess;
              ess << "'xeona::assign_ptr<T>::reset' 'Common::detach'"
                  << " coding error, 'diff' not one but " << diff;
              throw std::logic_error(ess.str());
            }
        }
      std::ostringstream oss;
      oss << "diff = " << diff;
      deport(oss.str());
    }

    // ---------------------------------------------------------
    //  MEMBER FUNCTION : xeona::Common::reVamp <>
    // ---------------------------------------------------------
    //
    //  CAUTION: incomplete design
    //
    //      This code does not implement resource convertibility
    //      in a way that covers all cases and is type-checked --
    //      in fact, achieving both would appear to be a major
    //      challenge.
    //
    //      Check the pre-processor macros in this code to see
    //      which conversions are currently supported.
    //
    //  Type-convertibility requirements
    //
    //          type T - results from the caller of 'revamp'
    //          type C - relates to the CURRENT client under traversal
    //          type Y - is from the replacement object
    //
    //      type-convertibility (where "convertible" implies "is
    //      or derives from" -- in other words, only direct
    //      assignment or implicit upcasting are acceptable):
    //
    //          Y convertible to C convertible to T
    //
    //      T and Y are known directly from this call under
    //      compile-time polymorphism
    //
    //      C is known indirectly thru run-time polymorphism and
    //      the 'Client' class hierarchy, namely:
    //
    //          class Client
    //          template <typename C> class ClientImp : public Client
    //
    //      Alexandrescu (2001 p264) gives an overview of
    //      polymorphism in C++.
    //
    // ---------------------------------------------------------

    template <typename T, typename Y>        // 'T' is nominated in call, 'Y' is implicit
    bool reVamp(shared_ptr<Y> globalReplacement)
    {
      Deport deport("Common", __func__);

      // administration
      d_revampCount++;
#ifdef XE_ASSIGN_PTR_XEONA_USE               // indicates 'xeona' proper
      bool retval = false;                   // assume false initially
#else
      bool retval = true;                    // assume true initially
#endif // XE_ASSIGN_PTR_XEONA_USE
      // set client count
      std::ostringstream oss1;
      oss1 << "client count = " << d_clients.size();
      const std::string clients = oss1.str();;

      // initial reporting
      deport(clients);

      // loop the clients
      int loopCount = 0;
      BOOST_FOREACH( shared_ptr<Client> client, d_clients )
        {
          // update and set loop count
          ++loopCount;
          std::ostringstream oss;
          oss << "count = " << std::setw(2) << std::setfill(' ') << loopCount;
          const std::string loops = oss.str();

          // reporting
          deport("loop enter, " + loops);

          // integrity check
          if ( client == 0 )
            {
              deport("client equals zero", "EMPTY OR NULL CLIENT");
              continue;
            }

#ifdef XE_ASSIGN_PTR_XEONA_USE               // indicates 'xeona' proper

          // ---------------------------------
          //  'xeona' code
          // ---------------------------------

          // CAUTION: special code for 'xeona' in which 'client'
          // must be exactly of type 'Y' for an update to occur
          // -- this code uses Boost.Any -- see r3280 for earlier
          // code based on typeid comparison and 'void*'

          const boost::any payload = globalReplacement;               // naturally Y
          const bool retY          = client->updateAny(payload);      // [1] key call

          // [1] this call uses "boost::any<shared_ptr<C> >" as
          // its cast specifier, which means that C and Y must
          // match for a successful outcome.

          static logga::spLogger logger = logga::ptrLogStream();      // bind logger
          static int loggerCount = 0;                                 // just for interest

          if ( retY )
            {
              // only one such update per pool should occur
              retval = true;                 // one or more true means success
              deport("client and resource type match", "UPDATE");
              logger->repx(logga::dbug, "update occured", ++loggerCount);
            }
          else
            {
              // CAUTION: at one stage, 'client->d_resource' was
              // set to empty pointer "shared_ptr<T>()" but this
              // created a bad bug whereby the 'xeona' 'polylink'
              // caller object was disabled and then destroyed
              // before the call had completed and returned.

              deport("client and resource type differ - skipping");
              logger->repx(logga::adhc, "update skipped", "");
            }

#else // XE_ASSIGN_PTR_XEONA_USE, in other words, code external to xeona

          // ---------------------------------
          //  normal code
          // ---------------------------------

          std::ostringstream oss8;
          oss8 << "payload = " <<  xeona::demangle(client->type().name());
          deport(oss8.str());

# if 0 // 0 = type-unsafe code, 1 = process only Y- and T-payloads

          // try Y payload
          const boost::any payloadY = globalReplacement;
          if ( client->updateAny(payloadY) == false )
            {
              // try T payload
              const boost::any payloadT = dynamic_pointer_cast<T>(globalReplacement);
              if ( client->updateAny(payloadT) == false )
                {
                  retval = false;            // one or more false means failure
                }
            }

# else

          if ( client->update(globalReplacement) == false )      // key call
            {
              retval = false;                // one or false false means failure
            }

# endif // 0

          // BUGGY: direct calls not working as expected
          //
          // client->get()->updateNonVirtual(globalReplacement);
          // client->clone()->updateNonVirtual(globalReplacement);
          //
          // note: error: 'class xeona::Client' has
          // no member named 'updateNonVirtual'

          // ---------------------------------

#endif // XE_ASSIGN_PTR_XEONA_USE

          deport("loop leave");

        } // BOOST_FOREACH

      std::ostringstream oss2;
      oss2 << "revamp count = " << d_revampCount;
      const std::string revamps = oss2.str();
      deport(clients + ", " + revamps);

      // return statement
      return retval;
    }

    long clientCount() const
    {
      Deport deport("Common", __func__);
      return d_clients.size();
    }

    long revampCount() const
    {
      Deport deport("Common", __func__);
      return d_revampCount;
    }

    // INSTANCE DATA

  private:

    std::vector<shared_ptr<Client> >    d_clients;          // actually 'ClientImp<C>'
    long                                d_revampCount;      // initially zero

  };

  // ---------------------------------------------------------
  //  CLASS (FUNCTOR) : xeona::match <>
  // ---------------------------------------------------------

  template <typename M>
  class match
  {
    // DISABLED

  private:

    match();                                 // zero-argument constructor
    match& operator= (const match& orig);    // copy assignment operator

    // CREATORS

  public:

    match
    (const shared_ptr<ClientImp<M> >& target) :
      d_target(target),
      d_match(0),
      d_count(new int(0))
    {
      Deport deport("match", "constructor", Deport::silent);
      deport("in constructor");
      if ( d_target == 0 )
        {
          deport("target equals zero", "BAD TARGET");
        }
    }

    match(const match& orig) :               // copy constructor
      d_target(orig.d_target),               // shallow copy is correct
      d_match(orig.d_match),
      d_count(orig.d_count)                  // shallow copy is correct
    { }

    ~match()                                 // destructor
    {
      Deport deport("match", "destructor", Deport::silent);
      deport(boost::str(boost::format("matches = %d, count = %d") % d_match % *d_count));
    }

    // FUNCTION CALL OPERATOR

    bool
    operator()
    (const shared_ptr<Client>& current)
    {
      Deport deport("match", "function call operator", Deport::silent);
      std::stringstream oss;
      oss << "client = " << current
          << ", "
          << "target = " << d_target;
      deport(oss.str());

      shared_ptr<ClientImp<M> > downcast = dynamic_pointer_cast<ClientImp<M> >(current);
      if ( downcast == 0 )
        {
          deport("Client downcast failed", "BAD DOWNCAST");
          return false;
        }

      const bool match = ( *downcast == *d_target );   // object-wise comparison
      if ( match == true )
        {
          ++d_match;
          ++*d_count;
        }
      return match;
    }

    // CAUTION: [1] according to Josuttis (1999 p378) 'remove_if'
    // acts on 'true' but Lischner (2003 p355) says 'remove_if'
    // acts on 'false' -- however Josuttis is correct.

    // INSTANCE DATA

  private:

    const shared_ptr<ClientImp<M> >    d_target;
    int                                d_match;
    shared_ptr<int>                    d_count;

  };

  // ---------------------------------------------------------
  //  CLASS           : xeona::assign_ptr <>
  // ---------------------------------------------------------
  //  Description  : "remappable" counted pointer which partly mimics 'shared_ptr
  //  Role         : point of access
  //  Techniques   : 'shared_ptr'
  //  Status       : complete -- but see preliminary notes for potential extensions
  //
  //  Template arguments
  //
  //      'T' is invoked class
  //      'Y' is argument class, which needs to be a sub-class of 'T'
  //      'U' is argument class, which needs to be a super-class of 'T'
  //
  // ---------------------------------------------------------

  template <typename T>
  class assign_ptr
  {
    // FRIENDS

    friend class Common;                     // used by Common::reVamp

  public:

    // CREATORS

    // UPCASTABLE: the various Y-to-T creators (more precisely,
    // function templates) require that the underlying resource
    // be naturally convertible from 'shared_ptr<Y>' to
    // 'shared_ptr<T>'.

    virtual ~assign_ptr();                        // destructor

    explicit
    assign_ptr();                                 // zero-argument (hollow) constructor

    template <typename Y>
    explicit assign_ptr(Y* resource);             // upclass raw pointer constructor

    // NON-EXCLUSIVITY: the provision of an overloaded
    // 'shared_ptr' constructor together with the equivalent
    // 'revamp' and 'reset' member functions require the client to
    // acknowledge that non-exclusive use may result -- in other
    // words, not all instances of the controlled resource are
    // under the jurisdiction of the current 'assign_ptr' pool.

    template <typename Y>                         // can be NON-EXCLUSIVE
    explicit assign_ptr(shared_ptr<Y> resource);  // upclass shared pointer constructor

    assign_ptr(const assign_ptr& orig);           // normal copy constructor

    template <typename Y>
    assign_ptr(const assign_ptr<Y>& orig);        // upclass copy constructor

    // COPY ASSIGNMENT OPERATORS

    assign_ptr<T>& operator= (const assign_ptr& orig);     // copy assignment operator

    template <typename Y>
    assign_ptr<T>& operator= (const assign_ptr<Y>& orig);  // upclass copy assignment oper

    // POINTER SEMANTICS

    operator bool() const;                   // object status test
    T& operator*  () const;                  // dereference operator
    T* operator-> () const                   // member access operator
      throw (std::logic_error);              // exception specification

    // COMPARATORS -- free functions

    // logical equality function
    template <typename Y>
    friend
    bool
    operator== (const assign_ptr&    a,
                const assign_ptr<Y>& b);

    // logical NOT equality function
    template <typename Y>
    friend
    bool
    operator!= (const assign_ptr&    a,
                const assign_ptr<Y>& b);

    // POOL-WIDE FUNCTIONS

    template <typename Y>
    bool revamp(Y* resource);                // replace resource for entire pool

    template <typename Y>                    // can be NON-EXCLUSIVE
    bool revamp(shared_ptr<Y> resource);     // replace resource for entire pool

    long client_count() const;               // pool count
    bool unique() const;                     // returns 'true' if sole client

    long revamp_count() const;               // revamp count
    bool original() const;                   // returns 'true' if never revamped

    long external_count() const;                  // number of "uncontrolled" resources
    bool exclusive() const;                       // 'true' is no "uncontrolled" resource

    // INDIVIDUAL-ACTION FUNCTIONS

    template <typename Y>
    void reset();                            // split off from existing pool

    template <typename Y>
    void reset(Y* resource);                 // split off from existing pool

    template <typename Y>                    // can be NON-EXCLUSIVE
    void reset(shared_ptr<Y> resource);      // split off from existing pool

    // DOWNCAST CONSTRUCTORS -- normally interfaced by free functions

    template <typename U>
    assign_ptr(assign_ptr<U> const & pointer,     // 'U' is the "cast to" type
               xeona::const_cast_tag);

    template <typename U>
    assign_ptr(assign_ptr<U> const & pointer,     // 'U' is the "cast to" type
               xeona::dynamic_cast_tag);          // returns zero on cast failure

    template <typename U>
    assign_ptr(assign_ptr<U> const & pointer,     // 'U' is the "cast to" type
               xeona::polymorphic_cast_tag)
      throw(std::bad_cast);                       // throws on cast failure

    // REPORT FUNCTION

    std::string                                   // with trailing newline
    report(const std::string& identifier = "(not supplied)",
           const std::string& comment    = "",
           const unsigned     leftIndent = 2) const;

    // CODE DEVELOPMENT CALLS

    T*             get() const;                   // null not tested, unlike 'operator->'
    assign_ptr<T>* get_assign(); // const         // simply returns 'this'

    void trash() { revamp(static_cast<T>(0)); }   // DANGEROUS

    // INSTANCE DATA

  public:  // [1]

    shared_ptr<T>         d_resource;        // locally held resource
    shared_ptr<Common>    d_common;

    // [1] CAUTION: data access: the upclass copy constructor
    // requires that these be public -- when they really should
    // be private (I remain puzzled!).

  };

  // ---------------------------------
  //  implementation
  // ---------------------------------

  // DESTRUCTOR

  // destructor
  // no action required due to use of 'shared_ptr's
  template <typename T>
  assign_ptr<T>::~assign_ptr()
  {
    Deport deport("assign_ptr", "destructor");
  }

  // CONSTRUCTORS

  // zero-argument (hollow) constructor
  // makes a new 'Common' in the process
  template <typename T>
  assign_ptr<T>::assign_ptr() :
    d_resource(),
    d_common(new Common())
  {
    Deport deport("assign_ptr", "constructor - hollow");
    deport("using new Common()");
  }

  // upcast raw pointer constructor
  template <typename T>
  template <typename Y>
  assign_ptr<T>::assign_ptr(Y* resource) :
    d_resource(shared_ptr<Y>(resource)),
    d_common(new Common())
  {
    Deport deport("assign_ptr", "constructor - raw upcast");
    d_common->attach(this);
  }

  // upcast shared pointer constructor
  template <typename T>
  template <typename Y>
  assign_ptr<T>::assign_ptr(shared_ptr<Y> resource) :
    d_resource(resource),
    d_common(new Common())
  {
    Deport deport("assign_ptr", "constructor - shared upcast");
    d_common->attach(this);
  }

  // normal copy constructor
  template <typename T>
  assign_ptr<T>::assign_ptr(const assign_ptr& orig) :
    d_resource(orig.d_resource),
    d_common(orig.d_common)
  {
    Deport deport("assign_ptr", "copy constr - no cast");
    d_common->attach(this);
  }

  // upcast copy constructor (requires public data members)
  template <typename T>
  template <typename Y>
  assign_ptr<T>::assign_ptr(const assign_ptr<Y>& orig) :
    d_resource(orig.d_resource),
    d_common(orig.d_common)
  {
    Deport deport("assign_ptr", "copy constr - upcast");
    d_common->attach(this);
  }

  // COPY ASSIGNMENT OPERATORS

  // copy assignment operator
  template <typename T>
  assign_ptr<T>& assign_ptr<T>::operator= (const assign_ptr& orig)
  {
    Deport deport("assign_ptr", "copy assign - no cast");
    this->d_resource = orig.d_resource;
    this->d_common   = orig.d_common;
    d_common->attach(this);
    return *this;
  }

  // upcast copy assignment operator
  template <typename T>
  template <typename Y>
  assign_ptr<T>& assign_ptr<T>::operator= (const assign_ptr<Y>& orig)
  {
    Deport deport("assign_ptr", "copy assign - upcast");
    if ( this != &orig )                     // protect against self-assignment
      {
        this->d_resource = orig.d_resource;
        this->d_common   = orig.d_common;
        d_common->attach(this);
      }
    return *this;
  }

  // OVERLOADED OPERATORS

  // bool operator
  //
  // based on 'std::tr1::shared_ptr' code, see also Becker (2007
  // pp35-36) for a discussion on this same operator for
  // 'shared_ptr' --this streams as 1/0 or true/false, if used
  // as follows:
  //
  //     std::cout << myAssignPtr << std::endl;
  //
  template <typename T>
  assign_ptr<T>::operator bool() const
  {
    return d_resource == 0 ? 0 : d_resource.get();
  }

  // dereference operator *
  template <typename T>
  T& assign_ptr<T>::operator* () const
  {
    return *d_resource;
  }

  // member access operator ->
  template <typename T>
  T* assign_ptr<T>::operator-> () const
    throw (std::logic_error)                 // exception specification
  {
    if ( d_resource.get() == 0 )
      {
        if ( xeona::nopro == false )         // meaning option '--krazy' not applied
          {
            const std::string emsg
              = "'xeona::assign_ptr<T>::operator->' call on null resource";
            throw std::logic_error(emsg);
          }
      }
    return d_resource.get();
  }

  // COMPARATORS -- free functions

  // friends injected into enclosing namespace and found by
  // argument dependent (name) lookup (ADL)

  // logical equality function
  template <typename T, typename Y>
  bool
  operator== (const assign_ptr<T>& a,
              const assign_ptr<Y>& b)
  {
    return a.get() == b.get();
  }

  // logical NOT equality function
  template <typename T, typename Y>
  bool
  operator!= (const assign_ptr<T>& a,
              const assign_ptr<Y>& b)
  {
    return !(a == b);
  }

  // POOL-WIDE FUNCTIONS

  // raw revamp
  // wrapper to shared pointer version
  template <typename T>
  template <typename Y>
  bool
  assign_ptr<T>::revamp
  (Y* resource)
  {
    Deport deport("assign_ptr", "revamp - raw");
    shared_ptr<Y> namedTemporary(resource);
    return revamp(namedTemporary);
  }

  // shared revamp
  template <typename T>
  template <typename Y>
  bool
  assign_ptr<T>::revamp
  (shared_ptr<Y> resource)
  {
    Deport deport("assign_ptr", "revamp - shared");
    std::ostringstream oss;
    oss << "T = " << xeona::demangle(typeid(T).name())
        << "   "
        << "Y = " << xeona::demangle(typeid(Y).name());
    deport(oss.str());

    const bool ret = d_common->reVamp<T>(resource);
    return ret;
  }

  // client count
  template <typename T>
  long
  assign_ptr<T>::client_count() const
  {
    return d_common->clientCount();
  }

  // unique test
  template <typename T>
  bool
  assign_ptr<T>::unique() const
  {
    return ( client_count() == 1 );
  }

  // revamp count
  template <typename T>
  long
  assign_ptr<T>::revamp_count() const
  {
    return d_common->revampCount();
  }

  // original test
  template <typename T>
  bool
  assign_ptr<T>::original() const
  {
    return ( revamp_count() == 0 );
  }

  // external count
  template <typename T>
  long
  assign_ptr<T>::external_count() const
  {
    return d_resource.use_count() - client_count();
  }

  // exclusivity test
  template <typename T>
  bool
  assign_ptr<T>::exclusive() const
  {
    return ( external_count() == 0 );
  }

  // INDIVIDUAL-ACTION FUNCTIONS

  // empty reset
  // wrapper to shared pointer version
  template <typename T>
  template <typename Y>
  void
  assign_ptr<T>::reset()
  {
    Deport deport("assign_ptr", "reset - empty");
    reset(shared_ptr<Y>());                  // empty shared pointer
  }

  // raw reset
  // wrapper to shared pointer version
  template <typename T>
  template <typename Y>
  void
  assign_ptr<T>::reset
  (Y* resource)
  {
    Deport deport("assign_ptr", "reset - raw");
    shared_ptr<Y> namedTemporary(resource);
    reset(namedTemporary);
  }

  // shared reset
  template <typename T>
  template <typename Y>
  void
  assign_ptr<T>::reset(shared_ptr<Y> resource)
  {
    Deport deport("assign_ptr", "reset - shared");
    // CAUTION: note the explicit 'T' template argument below
    d_common->detach<T>(this);
    d_common = shared_ptr<Common>(new Common());
    d_common->attach<T>(this);
    d_resource = resource;
  }

  // DOWNCAST CONSTRUCTORS -- normally interfaced by free functions

  template <typename T>
  template <typename U>
  assign_ptr<T>::assign_ptr                  // a kind of copy constructor
  (assign_ptr<U> const & pointer,            // 'Y' is the "cast to" type
   xeona::dynamic_cast_tag) :                // tag 'struct' defined earlier
     d_resource(dynamic_pointer_cast<T>(pointer.d_resource)),
     d_common(pointer.d_common)
  {
    Deport deport("assign_ptr", "dynamic cast ctor");
    if ( d_resource == 0 )                   // the cast failed
      {
        deport("resource equals zero", "CAST FAILED");
        d_common = shared_ptr<Common>(new Common());
      }
    d_common->attach(this);
  }

  template <typename T>
  template <typename U>
  assign_ptr<T>::assign_ptr                  // a kind of copy constructor
  (assign_ptr<U> const & pointer,            // 'Y' is the "cast to" type
   xeona::const_cast_tag) :                  // tag 'struct' defined earlier
     d_resource(const_pointer_cast<T>(pointer.d_resource)),
     d_common(pointer.d_common)
  {
    Deport deport("assign_ptr", "const cast ctor");
    // it is believed that this cast cannot fail
    d_common->attach(this);
  }

  template <typename T>
  template <typename U>
  assign_ptr<T>::assign_ptr                  // a kind of copy constructor
  (assign_ptr<U> const & pointer,            // 'Y' is the "cast to" type
   xeona::polymorphic_cast_tag)              // tag 'struct' defined earlier
    throw(std::bad_cast):
     d_resource(dynamic_pointer_cast<T>(pointer.d_resource)),
     d_common(pointer.d_common)
  {
    Deport deport("assign_ptr", "polymorphic cast ctor");
    if ( d_resource == 0 )                   // the cast failed
      {
        if ( xeona::nopro == false )         // meaning option '--krazy' not applied
          {
            std::ostringstream oss;
            oss << "'xeona::assign_ptr<T>' 'dynamic_pointer_cast' failed"
                << ", resource = " << pointer.d_resource.get();
            throw xeona::bad_assign_cast(oss.str());
          }
        d_common = shared_ptr<Common>(new Common());
      }
    d_common->attach(this);
  }

  // REPORT FUNCTION

  // report details
  template <typename T>
  std::string
  assign_ptr<T>::report
  (const std::string& identifier,            // note default
   const std::string& comment,               // note default
   const unsigned     leftIndent) const      // note default
  {
    std::string rsctype       = "(empty shared_ptror null resource)";
    if ( d_resource ) rsctype = xeona::demangle(typeid(*d_resource).name());

    const std::string fill(leftIndent, ' ');
    std::ostringstream oss;
    oss << fill << "assign_ptr details :";
    if ( !comment.empty() ) oss << " " << comment  << "\n";
    else                    oss << "\n";
    oss
      << fill << "  identifier : " << identifier                                  << "\n"
      << fill << "  resource   : " << d_resource.get()                            << "\n"
      << fill << "  ptr type   : " << xeona::demangle(typeid(T).name())           << "\n"
      << fill << "  rsc type   : " << rsctype                                     << "\n"
      << fill << "  revamps    : " << revamp_count()                              << "\n"
      << fill << "  clients    : " << client_count()                              << "\n"
      << fill << "  externals  : " << external_count()                            << "\n";
    return oss.str();
  }

  // CODE DEVELOPMENT CALLS

  // get
  // no null protection, unlike 'operator->'
  template <typename T>
  T*
  assign_ptr<T>::get() const
  {
    return d_resource.get();
  }

  // get assign_ptr
  template <typename T>
  assign_ptr<T>*
  assign_ptr<T>::get_assign()
  {
    return this;
  }

  // DOWNCAST CONSTRUCTORS

  // ---------------------------------------------------------
  //  FREE FUNCTION   : xeona::const_assign_cast <>
  // ---------------------------------------------------------
  //  Description  : wrapper to modified copy constructor
  //  Role         : support for downcast functionality
  //  Techniques   : tag struct, 'const_cast'
  //  Status       : complete
  // ---------------------------------------------------------

  template <typename T, typename U>
  assign_ptr<T>
  const_assign_cast
  (assign_ptr<U> const & pointer)
  {
    Deport deport("free func", __func__);
    return assign_ptr<T>(pointer, xeona::const_cast_tag());
  }

  // ---------------------------------------------------------
  //  FREE FUNCTION   : xeona::dynamic_assign_cast <>
  // ---------------------------------------------------------
  //  Description  : wrapper to modified copy constructor
  //  Role         : support for downcast functionality
  //  Techniques   : tag struct, 'dynamic_cast', return zero on cast fail
  //  Status       : complete
  // ---------------------------------------------------------

  template <typename T, typename U>
  assign_ptr<T>
  dynamic_assign_cast
  (assign_ptr<U> const & pointer)
  {
    Deport deport("free func", __func__);
    return assign_ptr<T>(pointer, xeona::dynamic_cast_tag());
  }

  // ---------------------------------------------------------
  //  FREE FUNCTION   : xeona::polymorphic_assign_cast <>
  // ---------------------------------------------------------
  //  Description  : wrapper to modified copy constructor
  //  Role         : support for downcast functionality
  //  Techniques   : tag struct, 'dynamic_cast', throw on cast fail
  //  Status       : complete
  //
  //  Design notes
  //
  //      Unlike dynamic_assign_cast, the function throws a
  //      'std::bad_cast' on failure.
  //
  //      See Karlsson (2006 pp54-61) for a discussion of
  //      polymorphic casting.
  //
  // ---------------------------------------------------------

  template <typename T, typename U>
  assign_ptr<T>
  polymorphic_assign_cast
  (assign_ptr<U> const & pointer)
    throw(std::bad_cast)
  {
    Deport deport("free func", __func__);
    return assign_ptr<T>(pointer, xeona::polymorphic_cast_tag());
  }

} // namespace 'xeona'

#endif // _XEONA_PTR_H_

//  end of file

