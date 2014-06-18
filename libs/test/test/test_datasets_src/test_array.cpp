//  (C) Copyright Gennadiy Rozental 2011.
//  Distributed under the Boost Software License, Version 1.0.
//  (See accompanying file LICENSE_1_0.txt or copy at
//  http://www.boost.org/LICENSE_1_0.txt)

//  See http://www.boost.org/libs/test for the library home page.
//
//  File        : $RCSfile$
//
//  Version     : $Revision$
//
//  Description : tests C array based dataset
// ***************************************************************************

// Boost.Test
#include <boost/test/unit_test.hpp>
#include <boost/test/data/monomorphic/array.hpp>
namespace data=boost::unit_test::data;

#include "test_datasets.hpp"

BOOST_AUTO_TEST_CASE( test_array )
{
    int arr1[] = {1,2,3};
    BOOST_TEST( data::make( arr1 ).size() == 3 );
    double const arr2[] = {7.4,3.2};
    BOOST_TEST( data::make( arr2 ).size() == 2 );

    int arr3[] = {7,11,13,17};
    int* ptr3 = arr3;
    data::for_each_sample( data::make( arr3 ), check_arg_type<int>() );

#ifndef BOOST_NO_CXX11_LAMBDAS
    int c = 0;
    data::for_each_sample( data::make( arr3 ), [&c,ptr3](int i) {
        BOOST_TEST( i == ptr3[c++] );
    });
#endif

    invocation_count ic;

    ic.m_value = 0;
    data::for_each_sample( data::make( arr3 ), ic );
    BOOST_TEST( ic.m_value == 4 );

    ic.m_value = 0;
    data::for_each_sample( data::make( arr3 ), ic, 2 );
    BOOST_TEST( ic.m_value == 2 );

    ic.m_value = 0;
    data::for_each_sample( data::make( arr3 ), ic, 0 );
    BOOST_TEST( ic.m_value == 0 );

    copy_count::value() = 0;
    copy_count arr4[] = { copy_count(), copy_count() };
    data::for_each_sample( data::make( arr4 ), check_arg_type<copy_count>() );
    BOOST_TEST( copy_count::value() == 0 );

    copy_count::value() = 0;
    copy_count const arr5[] = { copy_count(), copy_count() };
    data::for_each_sample( data::make( arr5 ), check_arg_type<copy_count>() );
    BOOST_TEST( copy_count::value() == 0 );
}

//____________________________________________________________________________//

// EOF
