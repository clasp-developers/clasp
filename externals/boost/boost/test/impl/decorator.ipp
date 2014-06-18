//  (C) Copyright Gennadiy Rozental 2011-2012.
//  Distributed under the Boost Software License, Version 1.0.
//  (See accompanying file LICENSE_1_0.txt or copy at 
//  http://www.boost.org/LICENSE_1_0.txt)

//  See http://www.boost.org/libs/test for the library home page.
//
//  File        : $RCSfile$
//
//  Version     : $Revision$
//
//  Description : unit test decorators implementation
// ***************************************************************************

#ifndef BOOST_TEST_TREE_DECORATOR_IPP_091911GER
#define BOOST_TEST_TREE_DECORATOR_IPP_091911GER

// Boost.Test
#include <boost/test/tree/decorator.hpp>
#include <boost/test/tree/test_unit.hpp>

#include <boost/test/framework.hpp>
#if BOOST_TEST_SUPPORT_TOKEN_ITERATOR
#include <boost/test/utils/iterator/token_iterator.hpp>
#endif

#include <boost/test/detail/suppress_warnings.hpp>

//____________________________________________________________________________//

namespace boost {
namespace unit_test {
namespace decorator {

// ************************************************************************** //
// **************             decorator::collector             ************** //
// ************************************************************************** //

collector::collector( for_test_unit const& D )
{
    m_tu_decorator.reset( D.clone() );

    if( instance() != NULL ) {
        for_test_unit_ptr leaf = m_tu_decorator;
        while( leaf->m_next )
            leaf = leaf->m_next;

        leaf->m_next = instance()->m_tu_decorator;
        instance()->m_tu_decorator.reset();
    }

    instance() = this;
}

//____________________________________________________________________________//

collector*&
collector::instance()
{
    static collector* s_instance = 0;

    return s_instance;
}

//____________________________________________________________________________//

void
collector::store_in( test_unit& tu )
{
    tu.p_decorators.value = m_tu_decorator;
    m_tu_decorator.reset();
    instance() = 0;
}

//____________________________________________________________________________//

// ************************************************************************** //
// **************           decorator::for_test_unit           ************** //
// ************************************************************************** //

for_test_unit const&
for_test_unit::operator+( for_test_unit const& rhs ) const
{
    rhs.m_next.reset( clone() );

    return rhs;
}

//____________________________________________________________________________//

void
for_test_unit::apply( test_unit& tu )
{
    if( m_next )
        m_next->apply( tu );
    do_apply( tu );
}

//____________________________________________________________________________//

for_test_unit*
for_test_unit::clone() const
{
    for_test_unit* res = do_clone();
    res->m_next = m_next;
    return res;
}

//____________________________________________________________________________//

// ************************************************************************** //
// **************               decorator::label               ************** //
// ************************************************************************** //

void
label::do_apply( test_unit& tu )
{
    tu.add_label( m_label );
}

//____________________________________________________________________________//

// ************************************************************************** //
// **************         decorator::expected_failures         ************** //
// ************************************************************************** //

void
expected_failures::do_apply( test_unit& tu )
{
    tu.increase_exp_fail( m_exp_fail );
}

//____________________________________________________________________________//

// ************************************************************************** //
// **************              decorator::timeout              ************** //
// ************************************************************************** //

void
timeout::do_apply( test_unit& tu )
{
    tu.p_timeout.value = m_timeout;
}

//____________________________________________________________________________//

// ************************************************************************** //
// **************            decorator::description            ************** //
// ************************************************************************** //

void
description::do_apply( test_unit& tu )
{
    tu.p_description.value += m_description;
}

//____________________________________________________________________________//

// ************************************************************************** //
// **************             decorator::depends_on            ************** //
// ************************************************************************** //

void
depends_on::do_apply( test_unit& tu )
{
#if !BOOST_TEST_SUPPORT_TOKEN_ITERATOR
    throw setup_error( "depends_on decorator is not supported on this platform" );
#else
    string_token_iterator tit( m_dependency, (dropped_delimeters = "/", kept_delimeters = dt_none) );

    test_unit* dep = &framework::master_test_suite();
    while( tit != string_token_iterator() ) {
        BOOST_TEST_SETUP_ASSERT( dep->p_type == TUT_SUITE, std::string( "incorrect dependency specification " ) + m_dependency );

        test_unit_id next_id = static_cast<test_suite*>(dep)->get( *tit );

        if( next_id == INV_TEST_UNIT_ID )
            throw framework::setup_error( std::string( "incorrect dependency specification " ) + m_dependency );

        dep = &framework::get( next_id, TUT_ANY );
        ++tit;           
    }

    tu.depends_on( dep );
#endif
}

//____________________________________________________________________________//

// ************************************************************************** //
// **************        decorator::enable_if/disable_if       ************** //
// ************************************************************************** //

void
enable_if::do_apply( test_unit& tu )
{
    tu.p_enabled.value = m_condition;
}

//____________________________________________________________________________//

// ************************************************************************** //
// **************              decorator::fixture              ************** //
// ************************************************************************** //

void
fixture_t::do_apply( test_unit& tu )
{
    tu.p_fixtures.value.push_back( m_impl );
}

//____________________________________________________________________________//

} // namespace decorator
} // namespace unit_test
} // namespace boost

#include <boost/test/detail/enable_warnings.hpp>

#endif // BOOST_TEST_TREE_DECORATOR_IPP_091911GER
