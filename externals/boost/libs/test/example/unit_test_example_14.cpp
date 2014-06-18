#define BOOST_TEST_TOOL_REPORT_WARN_FAILURE( assertion_descr ) \
    std::cout << "Condition " << assertion_descr << " is not satisfied\n"

#include <boost/test/tools/prod_tools.hpp>
#include <boost/exception/all.hpp>
#include <boost/noncopyable.hpp>

#include <iostream>

void report( void (*test_func)())
{
    try {
        (*test_func)();
    } catch ( std::runtime_error const& x ) {
        std::cout << *boost::get_error_info<boost::throw_file>(x) 
            << '(' 
            << *boost::get_error_info<boost::throw_line>(x)
            << "): Error in '" << *boost::get_error_info<boost::throw_function>(x)
            << "': " << x.what() << std::endl;
    }
}

//------------------------------------------------------------------//

void do_check()
{
    int i = 62;
    BOOST_CHECK( i == 144 );
}

//------------------------------------------------------------------//

void do_check_msg()
{
    int i = 62;
    BOOST_CHECK_MESSAGE( i == 144, "Expecting i == 62" );
}

//------------------------------------------------------------------//

void do_check_eq()
{
    int i = 62;
    BOOST_CHECK_EQUAL( i, 62 );
    BOOST_CHECK_EQUAL( i, 144 );
}

//------------------------------------------------------------------//

void do_check_le()
{
    int i = 62;
    BOOST_CHECK_LE( i, 16 );
}

//------------------------------------------------------------------//

void do_bitwise_eq()
{
    int i = 62;

    BOOST_CHECK_BITWISE_EQUAL( i, 144 );
}

//------------------------------------------------------------------//

int goo()
{
    static int i = 0;
    return i++;
}

struct Foo : boost::noncopyable {
    static int copy_counter;

    Foo( int i_ = 0 ) : i(i_) {}
    Foo( Foo const& ) { copy_counter++; }

    int i;
};

int Foo::copy_counter = 0;

bool operator==( Foo const&, Foo const& ) { return true; }
std::ostream& operator<<( std::ostream& os, Foo const& ) { return os << "Foo"; }

bool some_pred( Foo const& foo1, Foo const& foo2, Foo const& foo3, Foo const& foo4 )
{
    return foo1.i + foo2.i != foo3.i + foo4.i;
}

void do_check_pred()
{
    BOOST_CHECK_PREDICATE( some_pred, (Foo( 1 ))(Foo( 4 ))(Foo( 2 ))(Foo( 3 )) );
    BOOST_CHECK_EQUAL( Foo::copy_counter, 0 );
}

//------------------------------------------------------------------//

int main()
{
    report( &do_check );
    report( &do_check_msg );
    report( &do_check_eq );
    report( &do_bitwise_eq );
    report( &do_check_pred );
    return 0;
}

//------------------------------------------------------------------//

// EOF
