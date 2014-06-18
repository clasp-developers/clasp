//  (C) Copyright Gennadiy Rozental 2001-2012.
//  Distributed under the Boost Software License, Version 1.0.
//  (See accompanying file LICENSE_1_0.txt or copy at
//  http://www.boost.org/LICENSE_1_0.txt)

//  See http://www.boost.org/libs/test for the library home page.
//
//  File        : $RCSfile$
//
//  Version     : $Revision$
//
//  Description : main function implementation for Unit Test Framework
// ***************************************************************************

#ifndef BOOST_TEST_UNIT_TEST_MAIN_IPP_012205GER
#define BOOST_TEST_UNIT_TEST_MAIN_IPP_012205GER

// Boost.Test
#include <boost/test/framework.hpp>
#include <boost/test/results_collector.hpp>
#include <boost/test/results_reporter.hpp>

#include <boost/test/tree/visitor.hpp>
#include <boost/test/tree/test_unit.hpp>
#include <boost/test/tree/traverse.hpp>

#include <boost/test/unit_test_parameters.hpp>

// Boost
#include <boost/cstdlib.hpp>

// STL
#include <cstdio>
#include <stdexcept>
#include <iostream>
#include <iomanip>

#include <boost/test/detail/suppress_warnings.hpp>

//____________________________________________________________________________//

namespace boost {
namespace unit_test {

namespace ut_detail {

// ************************************************************************** //
// **************                test_tree_reporter            ************** //
// ************************************************************************** //

struct test_tree_reporter : test_tree_visitor {
    test_tree_reporter() : m_indent( -4 ) {} // skip master test suite

private:
    virtual void    visit( test_case const& tc )
    {
        results_reporter::get_stream() << std::setw( m_indent ) << "" << tc.p_name << "\n";
    }
    virtual bool    test_suite_start( test_suite const& ts )
    {
        if( m_indent >= 0 )
            results_reporter::get_stream() << std::setw( m_indent ) << "" << ts.p_name << "\n";
        m_indent += 4;
        return true;
    }
    virtual void    test_suite_finish( test_suite const& )
    {
        m_indent -= 4;
    }

    // Data members
    int             m_indent;
};

} // namespace ut_detail

// ************************************************************************** //
// **************                  unit_test_main              ************** //
// ************************************************************************** //

int BOOST_TEST_DECL
unit_test_main( init_unit_test_func init_func, int argc, char* argv[] )
{
    int result_code = 0;

    try {
        framework::init( init_func, argc, argv );

        if( runtime_config::list_content() ) {
            ut_detail::test_tree_reporter content_reporter;

            traverse_test_tree( framework::master_test_suite().p_id, content_reporter );

            return boost::exit_success;
        }

        if( runtime_config::wait_for_debugger() ) {
            results_reporter::get_stream() << "Press any key to continue..." << std::endl;

            std::getchar();
            results_reporter::get_stream() << "Continuing..." << std::endl;
        }

        framework::run();
        
        results_reporter::make_report();

        result_code = runtime_config::no_result_code() 
                        ? boost::exit_success
                        : results_collector.results( framework::master_test_suite().p_id ).result_code();
    }
    catch( framework::nothing_to_test const& ) {
        result_code = boost::exit_success;
    }
    catch( framework::internal_error const& ex ) {
        results_reporter::get_stream() << "Boost.Test framework internal error: " << ex.what() << std::endl;
        
        result_code = boost::exit_exception_failure;
    }
    catch( framework::setup_error const& ex ) {
        results_reporter::get_stream() << "Test setup error: " << ex.what() << std::endl;
        
        result_code = boost::exit_exception_failure;
    }
    catch( ... ) {
        results_reporter::get_stream() << "Boost.Test framework internal error: unknown reason" << std::endl;
        
        result_code = boost::exit_exception_failure;
    }

    framework::shutdown();

    return result_code;
}

} // namespace unit_test
} // namespace boost

#if !defined(BOOST_TEST_DYN_LINK) && !defined(BOOST_TEST_NO_MAIN)

// ************************************************************************** //
// **************        main function for tests using lib     ************** //
// ************************************************************************** //

int BOOST_TEST_CALL_DECL
main( int argc, char* argv[] )
{
    // prototype for user's unit test init function
#ifdef BOOST_TEST_ALTERNATIVE_INIT_API
    extern bool init_unit_test();

    boost::unit_test::init_unit_test_func init_func = &init_unit_test;
#else
    extern ::boost::unit_test::test_suite* init_unit_test_suite( int argc, char* argv[] );

    boost::unit_test::init_unit_test_func init_func = &init_unit_test_suite;
#endif

    return ::boost::unit_test::unit_test_main( init_func, argc, argv );
}

#endif // !BOOST_TEST_DYN_LINK && !BOOST_TEST_NO_MAIN

//____________________________________________________________________________//

#include <boost/test/detail/enable_warnings.hpp>

#endif // BOOST_TEST_UNIT_TEST_MAIN_IPP_012205GER
