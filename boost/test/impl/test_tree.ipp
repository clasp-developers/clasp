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
//  Description : privides core implementation for Unit Test Framework.
//                Extensions can be provided in separate files
// ***************************************************************************

#ifndef BOOST_TEST_UNIT_TEST_SUITE_IPP_012205GER
#define BOOST_TEST_UNIT_TEST_SUITE_IPP_012205GER

// Boost.Test
#include <boost/detail/workaround.hpp>

#include <boost/test/framework.hpp>
#include <boost/test/results_collector.hpp>

#include <boost/test/tree/test_unit.hpp>
#include <boost/test/tree/visitor.hpp>
#include <boost/test/tree/traverse.hpp>
#include <boost/test/tree/auto_registration.hpp>
#include <boost/test/tree/global_fixture.hpp>

#include <boost/test/utils/foreach.hpp>

#include <boost/test/unit_test_parameters.hpp>

// Boost
#include <boost/timer.hpp>

// STL
#include <algorithm>
#include <vector>

#include <boost/test/detail/suppress_warnings.hpp>

#if BOOST_WORKAROUND(__BORLANDC__, < 0x600) && BOOST_WORKAROUND(_STLPORT_VERSION, <= 0x450)
    using std::rand; // rand is in std and random_shuffle is in _STL
#endif

//____________________________________________________________________________//

namespace boost {
namespace unit_test {

// ************************************************************************** //
// **************                   test_unit                  ************** //
// ************************************************************************** //

test_unit::test_unit( const_string name, const_string file_name, std::size_t line_num, test_unit_type t )
: p_type( t )
, p_type_name( t == TUT_CASE ? "case" : "suite" )
, p_file_name( file_name )
, p_line_num( line_num )
, p_id( INV_TEST_UNIT_ID )
, p_name( std::string( name.begin(), name.size() ) )
, p_enabled( true )
{
}

//____________________________________________________________________________//

test_unit::test_unit( const_string module_name )
: p_type( TUT_SUITE )
, p_type_name( "module" )
, p_line_num( 0 )
, p_id( INV_TEST_UNIT_ID )
, p_name( std::string( module_name.begin(), module_name.size() ) )
, p_enabled( true )
{
}

//____________________________________________________________________________//

test_unit::~test_unit()
{
    framework::deregister_test_unit( this );
}

//____________________________________________________________________________//

void
test_unit::depends_on( test_unit* tu )
{
    BOOST_TEST_SETUP_ASSERT( p_id != framework::master_test_suite().p_id, "Can't add dependency to the master test suite" );

    p_dependencies.value.push_back( tu->p_id );
}

//____________________________________________________________________________//

bool
test_unit::check_dependencies() const
{
    BOOST_TEST_FOREACH( test_unit_id, tu_id, p_dependencies.get() ) {
        if( !unit_test::results_collector.results( tu_id ).passed() )
            return false;
    }

    return true;
}

//____________________________________________________________________________//

void
test_unit::increase_exp_fail( unsigned num )
{
    p_expected_failures.value += num;

    if( p_parent_id != 0 )
        framework::get<test_suite>( p_parent_id ).increase_exp_fail( num );
}

//____________________________________________________________________________//

void
test_unit::add_label( const_string l )
{
    m_labels.push_back( std::string() + l );
}

//____________________________________________________________________________//

bool
test_unit::has_label( const_string l ) const
{
    return std::find( m_labels.begin(), m_labels.end(), l ) != m_labels.end();
}

//____________________________________________________________________________//

// ************************************************************************** //
// **************                   test_case                  ************** //
// ************************************************************************** //

test_case::test_case( const_string name, boost::function<void ()> const& test_func )
: test_unit( name, "", 0, static_cast<test_unit_type>(type) )
, p_test_func( test_func )
{
    framework::register_test_unit( this );
}

//____________________________________________________________________________//

test_case::test_case( const_string name, const_string file_name, std::size_t line_num, boost::function<void ()> const& test_func )
: test_unit( name, file_name, line_num, static_cast<test_unit_type>(type) )
, p_test_func( test_func )
{
    framework::register_test_unit( this );
}

//____________________________________________________________________________//

// ************************************************************************** //
// **************                  test_suite                  ************** //
// ************************************************************************** //

//____________________________________________________________________________//

test_suite::test_suite( const_string name, const_string file_name, std::size_t line_num )
: test_unit( name, file_name, line_num, static_cast<test_unit_type>(type) )
{
    framework::register_test_unit( this );
}

//____________________________________________________________________________//

test_suite::test_suite( const_string module_name )
: test_unit( module_name )
{
    framework::register_test_unit( this );
}

//____________________________________________________________________________//

void
test_suite::add( test_unit* tu, counter_t expected_failures, unsigned timeout )
{
    if( timeout != 0 )
        tu->p_timeout.value = timeout;

    m_members.push_back( tu->p_id );
    tu->p_parent_id.value = p_id;

    if( tu->p_expected_failures != 0 )
        increase_exp_fail( tu->p_expected_failures );

    if( expected_failures )
        tu->increase_exp_fail( expected_failures );
}

//____________________________________________________________________________//

void
test_suite::add( test_unit_generator const& gen, unsigned timeout )
{
    test_unit* tu;
    while((tu = gen.next(), tu))
        add( tu, 0, timeout );
}

//____________________________________________________________________________//

void
test_suite::remove( test_unit_id id )
{
    std::vector<test_unit_id>::iterator it = std::find( m_members.begin(), m_members.end(), id );

    if( it != m_members.end() )
        m_members.erase( it );
}

//____________________________________________________________________________//

test_unit_id
test_suite::get( const_string tu_name ) const
{
    BOOST_TEST_FOREACH( test_unit_id, id, m_members ) {
        if( tu_name == framework::get( id, ut_detail::test_id_2_unit_type( id ) ).p_name.get() )
            return id;
    }

    return INV_TEST_UNIT_ID;
}

//____________________________________________________________________________//

// ************************************************************************** //
// **************               master_test_suite              ************** //
// ************************************************************************** //

master_test_suite_t::master_test_suite_t() 
: test_suite( "Master Test Suite" )
, argc( 0 )
, argv( 0 )
{
}

// ************************************************************************** //
// **************               traverse_test_tree             ************** //
// ************************************************************************** //

void
traverse_test_tree( test_case const& tc, test_tree_visitor& V, bool ignore_status )
{
    if( tc.p_enabled || ignore_status )
        V.visit( tc );
}

//____________________________________________________________________________//

void
traverse_test_tree( test_suite const& suite, test_tree_visitor& V, bool ignore_status )
{
    if( (!ignore_status && !suite.p_enabled) || !V.test_suite_start( suite ) )
        return;

    try {
        if( runtime_config::random_seed() == 0 ) {
            unsigned total_members = suite.m_members.size();
            for( unsigned i=0; i < total_members; ) {
                // this statement can remove the test unit from this list
                traverse_test_tree( suite.m_members[i], V, ignore_status );
                if( total_members > suite.m_members.size() )
                    total_members = suite.m_members.size();
                else
                    ++i;
            }
        }
        else {
            std::vector<test_unit_id> members( suite.m_members );
            std::random_shuffle( members.begin(), members.end() );
            BOOST_TEST_FOREACH( test_unit_id, id, members )
                traverse_test_tree( id, V, ignore_status );
        }
        
    } catch( framework::test_being_aborted const& ) {
        V.test_suite_finish( suite );
        framework::test_unit_aborted( suite );

        throw;
    }

    V.test_suite_finish( suite );
}

//____________________________________________________________________________//

void
traverse_test_tree( test_unit_id id, test_tree_visitor& V, bool ignore_status )
{
    if( ut_detail::test_id_2_unit_type( id ) == TUT_CASE )
        traverse_test_tree( framework::get<test_case>( id ), V, ignore_status );
    else
        traverse_test_tree( framework::get<test_suite>( id ), V, ignore_status );
}

//____________________________________________________________________________//

// ************************************************************************** //
// **************               object generators              ************** //
// ************************************************************************** //

namespace ut_detail {

std::string
normalize_test_case_name( const_string name )
{
    return ( name[0] == '&'
                ? std::string( name.begin()+1, name.size()-1 )
                : std::string( name.begin(), name.size() ) );
}

//____________________________________________________________________________//

// ************************************************************************** //
// **************           auto_test_unit_registrar           ************** //
// ************************************************************************** //

auto_test_unit_registrar::auto_test_unit_registrar( test_case* tc, decorator::collector* decorators, counter_t exp_fail )
{
    framework::current_auto_test_suite().add( tc, exp_fail );

    if( decorators )
        decorators->store_in( *tc );
}

//____________________________________________________________________________//

auto_test_unit_registrar::auto_test_unit_registrar( const_string ts_name, const_string ts_file, std::size_t ts_line, decorator::collector* decorators )
{
    test_unit_id id = framework::current_auto_test_suite().get( ts_name );

    test_suite* ts;

    if( id != INV_TEST_UNIT_ID ) {
        ts = &framework::get<test_suite>( id );
        BOOST_ASSERT( ts->p_parent_id == framework::current_auto_test_suite().p_id );
    }
    else {
        ts = new test_suite( ts_name, ts_file, ts_line );
        framework::current_auto_test_suite().add( ts );
    }

    if( decorators )
        decorators->store_in( *ts );

    framework::current_auto_test_suite( ts );
}

//____________________________________________________________________________//

auto_test_unit_registrar::auto_test_unit_registrar( test_unit_generator const& tc_gen, decorator::collector* /*decorators*/ )
{
    framework::current_auto_test_suite().add( tc_gen );

    // !! ?? if( decorators )
    // decorators->apply( *tc );
}

//____________________________________________________________________________//

auto_test_unit_registrar::auto_test_unit_registrar( int )
{
    framework::current_auto_test_suite( 0, false );
}

//____________________________________________________________________________//

} // namespace ut_detail

// ************************************************************************** //
// **************                global_fixture                ************** //
// ************************************************************************** //

global_fixture::global_fixture()
{
    framework::register_observer( *this );
} 

//____________________________________________________________________________//

} // namespace unit_test
} // namespace boost

#include <boost/test/detail/enable_warnings.hpp>

#endif // BOOST_TEST_UNIT_TEST_SUITE_IPP_012205GER
