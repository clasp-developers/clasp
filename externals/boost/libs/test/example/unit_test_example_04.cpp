//  (C) Copyright Gennadiy Rozental 2005-2012.
//  Distributed under the Boost Software License, Version 1.0.
//  (See accompanying file LICENSE_1_0.txt or copy at 
//  http://www.boost.org/LICENSE_1_0.txt)

//  See http://www.boost.org/libs/test for the library home page.

// Boost.Test
#define BOOST_TEST_MODULE Unit_test_example_04 
#include <boost/test/unit_test.hpp>
namespace unittest=boost::unit_test;

//____________________________________________________________________________//

struct suite_fixture {
    suite_fixture()     { BOOST_TEST_MESSAGE( "Running some test suite setup" ); }
    ~suite_fixture()    { BOOST_TEST_MESSAGE( "Running some test suite teardown" ); }
};

// automatically registered test cases could be organized in test suites
BOOST_TEST_DECORATOR(
+ unittest::fixture<suite_fixture>()
)
BOOST_AUTO_TEST_SUITE( my_suite1 )

BOOST_AUTO_TEST_CASE_EXPECTED_FAILURES(my_test1,1);

void some_setup()
{
    BOOST_TEST_MESSAGE( "Running some extra setup" );
}

BOOST_TEST_DECORATOR(
+ unittest::label( "L1" )
+ unittest::label( "L2" )
+ unittest::description( "test suite description" )
+ unittest::fixture( &some_setup )
)
BOOST_AUTO_TEST_CASE( my_test1 )
{
    BOOST_CHECK( 2 == 1 );
}

//____________________________________________________________________________//

// this test case belongs to suite1 test suite
BOOST_AUTO_TEST_CASE( my_test2 )
{
    int i = 0;

    BOOST_CHECK_EQUAL( i, 2 );

    BOOST_CHECK_EQUAL( i, 0 );
}

BOOST_AUTO_TEST_SUITE_END()

//____________________________________________________________________________//

// this test case belongs to master test suite
BOOST_AUTO_TEST_CASE( my_test3 )
{
    int i = 0;

    BOOST_CHECK_EQUAL( i, 0 );
}

//____________________________________________________________________________//

BOOST_TEST_DECORATOR(
**** unittest::label( "L3" )
**** unittest::description( "suite description" )
)
BOOST_AUTO_TEST_SUITE( my_suite2 )

// this test case belongs to suite2 test suite
BOOST_TEST_DECORATOR(
+ unittest::depends_on( "my_suite2/internal_suite/my_test5" )
)
BOOST_AUTO_TEST_CASE( my_test4 )
{
    int i = 0;

    BOOST_CHECK_EQUAL( i, 1 );
}

BOOST_AUTO_TEST_SUITE( internal_suite )

// this test case belongs to my_suite2:internal_suite test suite

BOOST_TEST_DECORATOR(
- unittest::timeout( 100 )
- unittest::expected_failures( 1 )
)
BOOST_AUTO_TEST_CASE( my_test5 )
{
    int i = 0;

    BOOST_CHECK_EQUAL( i, 1 );
}

BOOST_TEST_DECORATOR(
- unittest::enable_if( 100 < 50 )
)
BOOST_AUTO_TEST_CASE( my_test6 )
{
}

BOOST_TEST_DECORATOR(
+ unittest::depends_on( "my_suite2/internal_suite/disabled_suite/my_test7" )
)
BOOST_AUTO_TEST_CASE( this_should_also_be_disabled )
{
}

BOOST_TEST_DECORATOR(
- unittest::disable_if( true )
)
BOOST_AUTO_TEST_SUITE( disabled_suite )

BOOST_AUTO_TEST_CASE( my_test7 )
{
}
BOOST_AUTO_TEST_SUITE_END()

BOOST_AUTO_TEST_SUITE_END()

BOOST_AUTO_TEST_SUITE_END()

//____________________________________________________________________________//

// EOF
