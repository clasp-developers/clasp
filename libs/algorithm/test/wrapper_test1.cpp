/* 
   Copyright (c) Marshall Clow 2012.

   Distributed under the Boost Software License, Version 1.0. (See accompanying
   file LICENSE_1_0.txt or copy at http://www.boost.org/LICENSE_1_0.txt)

    For more information, see http://www.boost.org
*/

#include <boost/config.hpp>
#include <boost/algorithm/wrappers.hpp>

#define BOOST_TEST_MAIN
#include <boost/test/unit_test.hpp>

#include <functional>
#include <string>
#include <map>

namespace ba = boost::algorithm;

void test_int ()
{
    std::map<int, int> m;
    std::multimap<int, int> mm;
    
    int *ptr;
    
//  try with an empty map
    BOOST_CHECK ( ba::find_ptr ( m , 3 ) == NULL );
    BOOST_CHECK ( ba::find_ptr ( mm, 3 ) == NULL );
    
    m.insert  ( std::make_pair <int, int> ( 5, 5 ));
    mm.insert ( std::make_pair <int, int> ( 9, 9 ));
    BOOST_CHECK ( ba::find_ptr ( m , 3 ) == NULL );
    BOOST_CHECK ( ba::find_ptr ( mm, 3 ) == NULL );

    ptr = ba::find_ptr ( m, 5 );
    BOOST_CHECK ( ptr != NULL && *ptr == 5 );
    BOOST_CHECK ( ba::find_ptr ( m , 9 ) == NULL );

    ptr = ba::find_ptr ( mm, 9 );
    BOOST_CHECK ( ptr != NULL && *ptr == 9 );
    BOOST_CHECK ( ba::find_ptr ( mm, 5 ) == NULL );
    
}

void test_str ()
{
    std::map<int, std::string> m;
    std::multimap<int, std::string> mm;
    std::string *ptr;

//  try with an empty map
    BOOST_CHECK ( ba::find_ptr ( m , 31 ) == NULL );
    BOOST_CHECK ( ba::find_ptr ( mm, 31 ) == NULL );
    
    m.insert  ( std::make_pair <int, std::string> ( 55, "fifty-five" ));
    mm.insert ( std::make_pair <int, std::string> ( 66, "sixty-six" ));
    BOOST_CHECK ( ba::find_ptr ( m , 3 ) == NULL );
    BOOST_CHECK ( ba::find_ptr ( mm, 3 ) == NULL );

    ptr = ba::find_ptr ( m, 55 );
    BOOST_CHECK ( ptr != NULL && *ptr == "fifty-five" );
    BOOST_CHECK ( ba::find_ptr ( m , 66 ) == NULL );

    ptr = ba::find_ptr ( mm, 66 );
    BOOST_CHECK ( ptr != NULL && *ptr == "sixty-six" );
    BOOST_CHECK ( ba::find_ptr ( mm, 55 ) == NULL );
}

BOOST_AUTO_TEST_CASE( test_main )
{
  test_int ();
  test_str ();
}
