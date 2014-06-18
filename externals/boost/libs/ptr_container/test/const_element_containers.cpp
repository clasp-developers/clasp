//
// Boost.Pointer Container
//
//  Copyright Thorsten Ottosen 2009. Use, modification and
//  distribution is subject to the Boost Software License, Version
//  1.0. (See accompanying file LICENSE_1_0.txt or copy at
//  http://www.boost.org/LICENSE_1_0.txt)
//
// For more information, see http://www.boost.org/libs/ptr_container/
//

#include <boost/ptr_container/ptr_container.hpp>
#include <boost/test/unit_test.hpp>

template< class T >
void test_instantiations()
{
    // force instantiation of all members
    template class
    boost::ptr_array<T,42>;

    template class
    boost::ptr_deque<T>;

    template class
    boost::ptr_list<T>;
    
    template class
    boost::ptr_map<int,T>;

    template class
    boost::ptr_vector<T>;

    //@todo problem with constructor forwarding
    //template class
    //boost::ptr_unordered_map<int,T>;
    
    // @todo: there seems to be some problems with
    //        argument passing in circular_buffer
    //boost::ptr_circular_buffer<T> buffer(32);
    //buffer.push_back( new int(42)  );   
}



void test_const_element_container()
{    
    test_instantiations<const int>();
    test_instantiations< boost::nullable<const int> >();
    
    template class
    boost::ptr_set<const int>;

    // @todo: problem with constructor forwarding
    //template class
    //boost::ptr_unordered_set<T>;

}



using boost::unit_test::test_suite;

test_suite* init_unit_test_suite( int argc, char* argv[] )
{
    test_suite* test = BOOST_TEST_SUITE( "Pointer Container Test Suite" );

    test->add( BOOST_TEST_CASE( &test_const_element_container ) );

    return test;
}






