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
//  Description : tests stl collection based dataset
// ***************************************************************************

// Boost.Test
#include <boost/test/data/monomorphic/collection.hpp>

#include <boost/test/unit_test.hpp>
namespace data=boost::unit_test::data;

#include "test_datasets.hpp"

//____________________________________________________________________________//

BOOST_AUTO_TEST_CASE( test_collection )
{
    BOOST_TEST( data::make( std::vector<int>() ).size() == 0 );
    BOOST_TEST( data::make( std::vector<int>( 3 ) ).size() == 3 );
    BOOST_TEST( data::make( std::list<double>() ).size() == 0 );
    BOOST_TEST( data::make( std::list<double>( 2 ) ).size() == 2 );

    data::for_each_sample( data::make( std::vector<int>( 3 ) ), check_arg_type<int>() );
    data::for_each_sample( data::make( std::list<double>( 2 ) ), check_arg_type<double>() );

    invocation_count ic;

    ic.m_value = 0;
    data::for_each_sample( data::make( std::vector<int>( 3 ) ), ic );
    BOOST_TEST( ic.m_value == 3 );

    ic.m_value = 0;
    data::for_each_sample( data::make( std::list<double>( 2 ) ), ic, 4 );
    BOOST_TEST( ic.m_value == 2 );

    ic.m_value = 0;
    data::for_each_sample( data::make( std::vector<int>( 3 ) ), ic, 1 );
    BOOST_TEST( ic.m_value == 1 );

    std::vector<int> samples1;
    samples1.push_back(5);
    samples1.push_back(567);
    samples1.push_back(13);

#ifndef BOOST_NO_CXX11_LAMBDAS
    int c = 0;
    data::for_each_sample( data::make( samples1 ), [&c,samples1](int i) {
        BOOST_TEST( i == samples1[c++] );
    });

    std::list<char const*> samples2;
    samples2.push_back("sd");
    samples2.push_back("bg");
    samples2.push_back( "we3eq3" );

    auto it = samples2.begin();
    data::for_each_sample( data::make( samples2 ), [&it](char const* str ) {
        BOOST_TEST( str == *it++ );
    });
#endif

#ifndef BOOST_NO_CXX11_RVALUE_REFERENCES
    int exp_copy_count = 0;
#else
    int exp_copy_count = 4;
#endif

    copy_count::value() = 0;
    data::for_each_sample( data::make( std::vector<copy_count>( 2 ) ), check_arg_type<copy_count>() );
    BOOST_TEST( copy_count::value() == exp_copy_count );

    copy_count::value() = 0;
    std::vector<copy_count> samples3( 2 );
    data::for_each_sample( data::make( samples3 ), check_arg_type<copy_count>() );
    BOOST_TEST( copy_count::value() == exp_copy_count );

    copy_count::value() = 0;
    std::vector<copy_count> const samples4( 2 );
    data::for_each_sample( data::make( samples4 ), check_arg_type<copy_count>() );
    BOOST_TEST( copy_count::value() == exp_copy_count );

#ifndef BOOST_NO_CXX11_AUTO_DECLARATIONS
    copy_count::value() = 0;
    auto ds1 = data::make( make_copy_count_collection() );
    BOOST_TEST( ds1.size() == 3 );
    data::for_each_sample( ds1, check_arg_type<copy_count>() );
    BOOST_TEST( copy_count::value() == exp_copy_count );

    copy_count::value() = 0;
    auto ds2 = data::make( make_copy_count_const_collection() );
    BOOST_TEST( ds2.size() == 3 );
    data::for_each_sample( ds2, check_arg_type<copy_count>() );
    BOOST_TEST( copy_count::value() == 3 ); // !! ?? no const rvalue rev constructor for std::list
#endif
}

//____________________________________________________________________________//

// EOF
