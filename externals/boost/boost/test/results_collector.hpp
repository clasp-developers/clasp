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
//  Description : defines class unit_test_result that is responsible for 
//  gathering test results and presenting this information to end-user
// ***************************************************************************

#ifndef BOOST_TEST_RESULTS_COLLECTOR_HPP_071894GER
#define BOOST_TEST_RESULTS_COLLECTOR_HPP_071894GER

// Boost.Test
#include <boost/test/tree/observer.hpp>

#include <boost/test/detail/global_typedef.hpp>
#include <boost/test/detail/fwd_decl.hpp>

#include <boost/test/utils/trivial_singleton.hpp>
#include <boost/test/utils/class_properties.hpp>

#include <boost/test/detail/suppress_warnings.hpp>

//____________________________________________________________________________//

namespace boost {
namespace unit_test {

// ************************************************************************** //
// **************      first failed assertion debugger hook    ************** //
// ************************************************************************** //

namespace {
inline void first_failed_assertion() {}
}

// ************************************************************************** //
// **************                 test_results                 ************** //
// ************************************************************************** //

class BOOST_TEST_DECL test_results {
public:
    test_results();

    typedef BOOST_READONLY_PROPERTY( counter_t, (results_collector_t)(test_results)(results_collect_helper) ) counter_prop;
    typedef BOOST_READONLY_PROPERTY( bool,      (results_collector_t)(test_results)(results_collect_helper) ) bool_prop;

    counter_prop    p_assertions_passed;
    counter_prop    p_assertions_failed;
    counter_prop    p_warnings_failed;
    counter_prop    p_expected_failures;
    counter_prop    p_test_cases_passed;
    counter_prop    p_test_cases_warned;
    counter_prop    p_test_cases_failed;
    counter_prop    p_test_cases_skipped;
    counter_prop    p_test_cases_aborted;
    bool_prop       p_aborted;
    bool_prop       p_skipped;

    // "conclusion" methods
    bool            passed() const;
    int             result_code() const;

    // collection helper
    void            operator+=( test_results const& );

    void            clear();
};

// ************************************************************************** //
// **************               results_collector              ************** //
// ************************************************************************** //

class BOOST_TEST_DECL results_collector_t : public test_observer, public singleton<results_collector_t> {
public:
    // test_observer interface implementation
    virtual void        test_start( counter_t test_cases_amount );

    virtual void        test_unit_start( test_unit const& );
    virtual void        test_unit_finish( test_unit const&, unsigned long );
    virtual void        test_unit_skipped( test_unit const& );
    virtual void        test_unit_aborted( test_unit const& );

    virtual void        assertion_result( unit_test::assertion_result );
    virtual void        exception_caught( execution_exception const& );

    virtual int         priority() { return 2; }

    // results access
    test_results const& results( test_unit_id ) const;

private:
    BOOST_TEST_SINGLETON_CONS( results_collector_t )
};

BOOST_TEST_SINGLETON_INST( results_collector )

} // namespace unit_test
} // namespace boost

#include <boost/test/detail/enable_warnings.hpp>

#endif // BOOST_TEST_RESULTS_COLLECTOR_HPP_071894GER

