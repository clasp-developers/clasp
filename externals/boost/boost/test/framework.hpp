//  (C) Copyright Gennadiy Rozental 2005-2012.
//  Distributed under the Boost Software License, Version 1.0.
//  (See accompanying file LICENSE_1_0.txt or copy at 
//  http://www.boost.org/LICENSE_1_0.txt)

//  See http://www.boost.org/libs/test for the library home page.
//
//  File        : $RCSfile$
//
//  Version     : $Revision$
//
//  Description : defines framework interface
// ***************************************************************************

#ifndef BOOST_TEST_FRAMEWORK_HPP_020805GER
#define BOOST_TEST_FRAMEWORK_HPP_020805GER

// Boost.Test
#include <boost/test/detail/global_typedef.hpp>
#include <boost/test/detail/fwd_decl.hpp>
#include <boost/test/utils/trivial_singleton.hpp>

#include <boost/test/detail/suppress_warnings.hpp>

// STL
#include <stdexcept>

//____________________________________________________________________________//

namespace boost {
namespace unit_test {

// ************************************************************************** //
// **************              init_unit_test_func             ************** //
// ************************************************************************** //

#ifdef BOOST_TEST_ALTERNATIVE_INIT_API
typedef bool        (*init_unit_test_func)();
#else
typedef test_suite* (*init_unit_test_func)( int, char* [] );
#endif

// ************************************************************************** //
// **************                   framework                  ************** //
// ************************************************************************** //

namespace framework {

// initialization
BOOST_TEST_DECL void    init( init_unit_test_func init_func, int argc, char* argv[] );
BOOST_TEST_DECL bool    is_initialized();

// shutdown
BOOST_TEST_DECL void    shutdown();

// mutation access methods
BOOST_TEST_DECL void    register_test_unit( test_case* tc );
BOOST_TEST_DECL void    register_test_unit( test_suite* ts );
BOOST_TEST_DECL void    deregister_test_unit( test_unit* tu );
BOOST_TEST_DECL void    clear();

BOOST_TEST_DECL void    register_observer( test_observer& );
BOOST_TEST_DECL void    deregister_observer( test_observer& );
BOOST_TEST_DECL void    reset_observers();

// Assertions context support
struct BOOST_TEST_DECL context_generator {
    context_generator() : m_curr_frame( 0 ) {}

    // is there any context?
    bool            is_empty() const;

    // give me next frame; empty - last frame
    const_string    next() const;

private:
    // Data members
    mutable unsigned m_curr_frame;
};

BOOST_TEST_DECL int                 add_context( lazy_ostream const& context_descr, bool sticky );
BOOST_TEST_DECL void                clear_context( int context_id = -1 );
BOOST_TEST_DECL context_generator   get_context();

// Master test suite access
BOOST_TEST_DECL master_test_suite_t& master_test_suite();
BOOST_TEST_DECL test_suite&          current_auto_test_suite( test_suite* ts = 0, bool push_or_pop = true );

// constant access methods
BOOST_TEST_DECL test_case const&    current_test_case();
BOOST_TEST_DECL test_unit_id        current_test_case_id(); /* safe version of above */

BOOST_TEST_DECL test_unit&  get( test_unit_id, test_unit_type );
template<typename UnitType>
inline UnitType&            get( test_unit_id id )
{
    return static_cast<UnitType&>( get( id, static_cast<test_unit_type>(UnitType::type) ) );
}

// test initiation
BOOST_TEST_DECL void    run( test_unit_id = INV_TEST_UNIT_ID, bool continue_test = true );
BOOST_TEST_DECL void    run( test_unit const*, bool continue_test = true );

// public test events dispatchers
BOOST_TEST_DECL void    assertion_result( unit_test::assertion_result ar );
BOOST_TEST_DECL void    exception_caught( execution_exception const& );
BOOST_TEST_DECL void    test_unit_aborted( test_unit const& );

namespace impl { // publisized to facilitate internal unit test only

void                    apply_filters( test_unit_id );

} // namespace impl

// ************************************************************************** //
// **************                framework errors              ************** //
// ************************************************************************** //

struct BOOST_TEST_DECL internal_error : public std::runtime_error {
    internal_error( const_string m ) : std::runtime_error( std::string( m.begin(), m.size() ) ) {}
};

//____________________________________________________________________________//

struct BOOST_TEST_DECL setup_error : public std::runtime_error {
    setup_error( const_string m ) : std::runtime_error( std::string( m.begin(), m.size() ) ) {}
};

#define BOOST_TEST_SETUP_ASSERT( cond, msg ) if( cond ) {} else throw unit_test::framework::setup_error( msg )

//____________________________________________________________________________//

struct BOOST_TEST_DECL test_being_aborted {};

//____________________________________________________________________________//

struct nothing_to_test {}; // not really an error

//____________________________________________________________________________//

} // namespace framework
} // unit_test
} // namespace boost

#include <boost/test/detail/enable_warnings.hpp>

#endif // BOOST_TEST_FRAMEWORK_HPP_020805GER

