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
//  Description : Facilities to perform interaction based testng of logged expectations
// ***************************************************************************

#ifndef BOOST_TEST_LOGGED_EXPECTATIONS_HPP_120905GER
#define BOOST_TEST_LOGGED_EXPECTATIONS_HPP_120905GER

// Boost.Test
#include <boost/test/detail/config.hpp>
#include <boost/test/unit_test_parameters.hpp>

// Boost
#include <boost/function/function0.hpp>

#include <boost/test/detail/suppress_warnings.hpp>

//____________________________________________________________________________//

// ************************************************************************** //
// **************        BOOST_TEST_LOGGED_EXPECTATIONS        ************** //
// ************************************************************************** //

#define BOOST_TEST_LOGGED_EXPECTATIONS( test_name )                     \
struct test_name : public BOOST_AUTO_TEST_CASE_FIXTURE                  \
{ void test_method(); };                                                \
                                                                        \
static void BOOST_AUTO_TC_INVOKER( test_name )()                        \
{                                                                       \
    test_name t;                                                        \
    ::boost::itest::logged_expectations(                                \
        boost::bind( &test_name::test_method, t ),                      \
        BOOST_STRINGIZE(test_name) ".elog",                             \
        !::boost::unit_test::runtime_config::save_pattern() );          \
}                                                                       \
                                                                        \
struct BOOST_AUTO_TC_UNIQUE_ID( test_name ) {};                         \
                                                                        \
BOOST_AUTO_TU_REGISTRAR( test_name )(                                   \
    boost::unit_test::make_test_case(                                   \
        &BOOST_AUTO_TC_INVOKER( test_name ), #test_name,                \
       __FILE__, __LINE__ ),                                            \
    boost::unit_test::decorator::collector::instance() );               \
                                                                        \
void test_name::test_method()                                           \
/**/

namespace boost {
namespace itest {

// ************************************************************************** //
// **************           logged expectations test           ************** //
// ************************************************************************** //

void    BOOST_TEST_DECL
logged_expectations( boost::function<void ()> const&  F, unit_test::const_string log_file_name, bool test_or_log = true );

} // namespace itest
} // namespace boost

#include <boost/test/detail/enable_warnings.hpp>

#endif // BOOST_TEST_LOGGED_EXPECTATIONS_HPP_120905GER
