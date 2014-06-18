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
//  Description : storage for unit test framework parameters information
// ***************************************************************************

#ifndef BOOST_TEST_UNIT_TEST_PARAMETERS_HPP_071894GER
#define BOOST_TEST_UNIT_TEST_PARAMETERS_HPP_071894GER

#include <boost/test/detail/global_typedef.hpp>
#include <boost/test/detail/log_level.hpp>

#include <boost/test/detail/suppress_warnings.hpp>

// STL
#include <iosfwd>
#include <list>

//____________________________________________________________________________//

namespace boost {
namespace unit_test {
namespace runtime_config {

// ************************************************************************** //
// **************                 runtime_config               ************** //
// ************************************************************************** //

BOOST_TEST_DECL void                    init( int& argc, char** argv );

BOOST_TEST_DECL bool                    auto_start_dbg();
BOOST_TEST_DECL const_string            break_exec_path();
BOOST_TEST_DECL bool                    catch_sys_errors();
BOOST_TEST_DECL bool                    color_output();
BOOST_TEST_DECL bool                    detect_fp_exceptions();
BOOST_TEST_DECL long                    detect_memory_leaks();
BOOST_TEST_DECL bool                    list_content();
BOOST_TEST_DECL output_format           log_format();
BOOST_TEST_DECL unit_test::log_level    log_level();
BOOST_TEST_DECL std::ostream*           log_sink();
BOOST_TEST_DECL const_string            memory_leaks_report_file();
BOOST_TEST_DECL bool                    no_result_code();
BOOST_TEST_DECL int                     random_seed();
BOOST_TEST_DECL output_format           report_format();
BOOST_TEST_DECL unit_test::report_level report_level();
BOOST_TEST_DECL std::ostream*           report_sink();
BOOST_TEST_DECL bool                    save_pattern();
BOOST_TEST_DECL bool                    show_build_info();
BOOST_TEST_DECL bool                    show_progress();
BOOST_TEST_DECL std::list<std::string> const& test_to_run();
BOOST_TEST_DECL bool                    use_alt_stack();
BOOST_TEST_DECL bool                    wait_for_debugger();

} // namespace runtime_config
} // namespace unit_test
} // namespace boost

//____________________________________________________________________________//

#include <boost/test/detail/enable_warnings.hpp>

#endif // BOOST_TEST_UNIT_TEST_PARAMETERS_HPP_071894GER
