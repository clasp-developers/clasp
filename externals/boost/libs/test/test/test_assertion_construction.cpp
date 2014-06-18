//  (C) Copyright Gennadiy Rozental 2011.
//  Distributed under the Boost Software License, Version 1.0.
//  (See accompanying file LICENSE_1_0.txt or copy at 
//  http://www.boost.org/LICENSE_1_0.txt)

//  See http://www.boost.org/libs/test for the library home page.
//
//  File        : $RCSfile$
//
//  Version     : $Revision: 62023 $
//
//  Description : unit test for new assertion construction based on input expression
// ***************************************************************************

// Boost.Test
#define BOOST_TEST_MODULE Boost.Test assertion consruction test
#include <boost/test/unit_test.hpp>
#include <boost/test/tools/assertion.hpp>
#ifdef BOOST_NO_CXX11_AUTO_DECLARATIONS
#include <boost/test/tools/detail/expression_holder.hpp>
#endif

#include <boost/noncopyable.hpp>

//____________________________________________________________________________//

#ifdef BOOST_NO_CXX11_AUTO_DECLARATIONS

#   define EXPR_TYPE( E, expr ) tt_detail::expression_holder const& E = tt_detail::hold_expression(assertion::seed() ->* expr)
#   define BOOST_TEST_FWD( a )  BOOST_CHECK( a )

#else

#   define EXPR_TYPE( E, expr ) auto const& E = assertion::seed() ->* expr
#   define BOOST_TEST_FWD( a )  BOOST_TEST( a )
#endif

BOOST_AUTO_TEST_CASE( test_basic_value_expression_construction )
{
    using namespace boost::test_tools;

    {
        EXPR_TYPE( E, 1 );
        predicate_result const& res = E.evaluate();
        BOOST_TEST_FWD( res );
        BOOST_TEST_FWD( res.message().is_empty() );
    }

    {
        EXPR_TYPE( E, 0 );
        predicate_result const& res = E.evaluate();
        BOOST_TEST_FWD( !res );
        BOOST_TEST_FWD( res.message() == " [(bool)0 is false]" );
    }

    {
        EXPR_TYPE( E, true );
        predicate_result const& res = E.evaluate();
        BOOST_TEST_FWD( res );
        BOOST_TEST_FWD( res.message().is_empty() );
    }

    {
        EXPR_TYPE( E, 1.5 );
        predicate_result const& res = E.evaluate();
        BOOST_TEST_FWD( res );
    }

#ifndef BOOST_NO_DECLTYPE
    {
        EXPR_TYPE( E,  "abc" );
        predicate_result const& res = E.evaluate();
        BOOST_TEST_FWD( res );
    }
#endif

    {
        EXPR_TYPE( E,  1>2 );
        predicate_result const& res = E.evaluate();
        BOOST_TEST_FWD( !res );
        BOOST_TEST_FWD( res.message() == " [1 <= 2]" );
    }

}

//____________________________________________________________________________//

BOOST_AUTO_TEST_CASE( test_comparison_expression )
{
    using namespace boost::test_tools;

    {
        EXPR_TYPE( E,  1>2 );
        predicate_result const& res = E.evaluate();
        BOOST_TEST_FWD( !res );
        BOOST_TEST_FWD( res.message() == " [1 <= 2]" );
    }

    {
        EXPR_TYPE( E,  100 < 50 );
        predicate_result const& res = E.evaluate();
        BOOST_TEST_FWD( !res );
        BOOST_TEST_FWD( res.message() == " [100 >= 50]" );
    }

    {
        EXPR_TYPE( E,  5 <= 4 );
        predicate_result const& res = E.evaluate();
        BOOST_TEST_FWD( !res );
        BOOST_TEST_FWD( res.message() == " [5 > 4]" );
    }

    {
        EXPR_TYPE( E,  10>=20 );
        predicate_result const& res = E.evaluate();
        BOOST_TEST_FWD( !res );
        BOOST_TEST_FWD( res.message() == " [10 < 20]" );
    }

    {
        int i = 10;
        EXPR_TYPE( E,  i != 10 );
        predicate_result const& res = E.evaluate();
        BOOST_TEST_FWD( !res );
        BOOST_TEST_FWD( res.message() == " [10 == 10]" );
    }

    {
        int i = 5;
        EXPR_TYPE( E,  i == 3 );
        predicate_result const& res = E.evaluate();
        BOOST_TEST_FWD( !res );
        BOOST_TEST_FWD( res.message() == " [5 != 3]" );
    }
}

//____________________________________________________________________________//

#ifndef BOOST_NO_DECLTYPE

BOOST_AUTO_TEST_CASE( test_arithmetic_ops )
{
    using namespace boost::test_tools;

    {
        int i = 3;
        int j = 5;
        EXPR_TYPE( E,  i+j !=8 );
        predicate_result const& res = E.evaluate();
        BOOST_TEST_FWD( !res );
        BOOST_TEST_FWD( res.message() == " [3 + 5 == 8]" );
    }

    {
        int i = 3;
        int j = 5;
        EXPR_TYPE( E,  2*i-j > 1 );
        predicate_result const& res = E.evaluate();
        BOOST_TEST_FWD( !res );
        BOOST_TEST_FWD( res.message() == " [2 * 3 - 5 <= 1]" );
    }

    {
        int j = 5;
        EXPR_TYPE( E,  2<<j < 30 );
        predicate_result const& res = E.evaluate();
        BOOST_TEST_FWD( !res );
        BOOST_TEST_FWD( res.message() == " [2 << 5 >= 30]" );
    }

    {
        int i = 2;
        int j = 5;
        EXPR_TYPE( E,  i&j );
        predicate_result const& res = E.evaluate();
        BOOST_TEST_FWD( !res );
        BOOST_TEST_FWD( res.message() == " [2 & 5]" );
    }

    {
        int i = 3;
        int j = 5;
        EXPR_TYPE( E,  i^j^6 );
        predicate_result const& res = E.evaluate();
        BOOST_TEST_FWD( !res );
        BOOST_TEST_FWD( res.message() == " [3 ^ 5 ^ 6]" );
    }

    // do not support
    // EXPR_TYPE( E, 99/2 == 48 || 101/2 > 50 );
    // EXPR_TYPE( E,  a ? 100 < 50 : 25*2 == 50 );
    // EXPR_TYPE( E,  true,false );
}

//____________________________________________________________________________//

#endif // BOOST_NO_DECLTYPE

struct Testee {
    static int s_copy_counter;

    Testee() : m_value( false ) {}
    Testee( Testee const& ) : m_value(false) { s_copy_counter++; }
#ifndef BOOST_NO_CXX11_RVALUE_REFERENCES
    Testee( Testee&& ) : m_value(false)     {}
    Testee( Testee const&& ) : m_value(false)     {}
#endif

    bool foo() { return m_value; }
    operator bool() const { return m_value; }

    friend std::ostream& operator<<( std::ostream& ostr, Testee const& ) { return ostr << "Testee"; }

    bool m_value;
};

int Testee::s_copy_counter = 0;

#ifndef BOOST_NO_CXX11_RVALUE_REFERENCES
Testee          get_obj() { return std::move( Testee() ); }
Testee const    get_const_obj() { return std::move( Testee() ); }
#else
Testee          get_obj() { return Testee(); }
Testee const    get_const_obj() { return Testee(); }
#endif

class NC : boost::noncopyable {
public:
    NC() {}

    bool operator==(NC const&)  const { return false; }
    friend std::ostream& operator<<(std::ostream& ostr, NC const&)
    {
        return ostr << "NC";
    }
};

BOOST_AUTO_TEST_CASE( test_objects )
{
    using namespace boost::test_tools;

#if !defined(BOOST_NO_CXX11_RVALUE_REFERENCES) && !defined(BOOST_NO_CXX11_AUTO_DECLARATIONS)
    int expected_copy_count = 0;
#else
    int expected_copy_count = 2;
#endif

    {
        Testee obj;
        Testee::s_copy_counter = 0;

        EXPR_TYPE( E,  obj );
        predicate_result const& res = E.evaluate();
        BOOST_TEST_FWD( !res );
        BOOST_TEST_FWD( res.message() == " [(bool)Testee is false]" );
        BOOST_TEST_FWD( Testee::s_copy_counter == expected_copy_count );
    }

    {
        Testee const obj;
        Testee::s_copy_counter = 0;

        EXPR_TYPE( E,  obj );
        predicate_result const& res = E.evaluate();
        BOOST_TEST_FWD( !res );
        BOOST_TEST_FWD( res.message() == " [(bool)Testee is false]" );
        BOOST_TEST_FWD( Testee::s_copy_counter == expected_copy_count );
    }

    {
        Testee::s_copy_counter = 0;

        EXPR_TYPE( E,  get_obj() );
        predicate_result const& res = E.evaluate();
        BOOST_TEST_FWD( !res );
        BOOST_TEST_FWD( res.message() == " [(bool)Testee is false]" );
        BOOST_TEST_FWD( Testee::s_copy_counter == expected_copy_count );
    }

    {
        Testee::s_copy_counter = 0;

        EXPR_TYPE( E,  get_const_obj() );
        predicate_result const& res = E.evaluate();
        BOOST_TEST_FWD( !res );
        BOOST_TEST_FWD( res.message() == " [(bool)Testee is false]" );
        BOOST_TEST_FWD( Testee::s_copy_counter == expected_copy_count );
    }

    {
        Testee::s_copy_counter = 0;

        Testee t1;
        Testee t2;

        EXPR_TYPE( E,  t1 != t2 );
        predicate_result const& res = E.evaluate();
        BOOST_TEST_FWD( !res );
        BOOST_TEST_FWD( res.message() == " [Testee == Testee]" );
#ifndef BOOST_NO_CXX11_RVALUE_REFERENCES
        BOOST_TEST_FWD( Testee::s_copy_counter == 0 );
#endif
    }

#if !defined(BOOST_NO_CXX11_AUTO_DECLARATIONS)
    {
        NC nc1;
        NC nc2;

        EXPR_TYPE( E,  nc1 == nc2 );
        predicate_result const& res = E.evaluate();
        BOOST_TEST_FWD( !res );
        BOOST_TEST_FWD( res.message() == " [NC != NC]" );
    }
#endif
}

//____________________________________________________________________________//

BOOST_AUTO_TEST_CASE( test_pointers )
{
    using namespace boost::test_tools;

    {
        Testee* ptr = 0;

        EXPR_TYPE( E,  ptr );
        predicate_result const& res = E.evaluate();
        BOOST_TEST_FWD( !res );
    }

    {
        Testee obj1;
        Testee obj2;

        EXPR_TYPE( E,  &obj1 == &obj2 );
        predicate_result const& res = E.evaluate();
        BOOST_TEST_FWD( !res );
    }

    {
        Testee obj;
        Testee* ptr =&obj;

        EXPR_TYPE( E,  *ptr );
        predicate_result const& res = E.evaluate();
        BOOST_TEST_FWD( !res );
        BOOST_TEST_FWD( res.message() == " [(bool)Testee is false]" );
    }

#ifndef BOOST_NO_DECLTYPE
    {
        Testee obj;
        Testee* ptr =&obj;
        bool Testee::* mem_ptr =&Testee::m_value;

        EXPR_TYPE( E,  ptr->*mem_ptr );
        predicate_result const& res = E.evaluate();
        BOOST_TEST_FWD( !res );
    }
#endif

    // do not support
    // Testee obj;
    // bool Testee::* mem_ptr =&Testee::m_value;
    // EXPR_TYPE( E,  obj.*mem_ptr );
}

//____________________________________________________________________________//

BOOST_AUTO_TEST_CASE( test_mutating_ops )
{
    using namespace boost::test_tools;

    {
        int j = 5;

        EXPR_TYPE( E,  j = 0 );
        predicate_result const& res = E.evaluate();
        BOOST_TEST_FWD( !res );
        BOOST_TEST_FWD( res.message() == " [(bool)0 is false]" );
#ifndef BOOST_NO_CXX11_RVALUE_REFERENCES
        BOOST_TEST_FWD( j == 0 );
#else
        BOOST_TEST_FWD( j == 5 );
#endif
    }

    {
        int j = 5;

        EXPR_TYPE( E,  j -= 5 );
        predicate_result const& res = E.evaluate();
        BOOST_TEST_FWD( !res );
        BOOST_TEST_FWD( res.message() == " [(bool)0 is false]" );
#ifndef BOOST_NO_CXX11_RVALUE_REFERENCES
        BOOST_TEST_FWD( j == 0 );
#else
        BOOST_TEST_FWD( j == 5 );
#endif
    }

    {
        int j = 5;

        EXPR_TYPE( E,  j *= 0 );
        predicate_result const& res = E.evaluate();
        BOOST_TEST_FWD( !res );
        BOOST_TEST_FWD( res.message() == " [(bool)0 is false]" );
#ifndef BOOST_NO_CXX11_RVALUE_REFERENCES
        BOOST_TEST_FWD( j == 0 );
#else
        BOOST_TEST_FWD( j == 5 );
#endif
    }

    {
        int j = 5;

        EXPR_TYPE( E,  j /= 10 );
        predicate_result const& res = E.evaluate();
        BOOST_TEST_FWD( !res );
        BOOST_TEST_FWD( res.message() == " [(bool)0 is false]" );
#ifndef BOOST_NO_CXX11_RVALUE_REFERENCES
        BOOST_TEST_FWD( j == 0 );
#else
        BOOST_TEST_FWD( j == 5 );
#endif
    }

    {
        int j = 4;

        EXPR_TYPE( E,  j %= 2 );
        predicate_result const& res = E.evaluate();
        BOOST_TEST_FWD( !res );
        BOOST_TEST_FWD( res.message() == " [(bool)0 is false]" );
#ifndef BOOST_NO_CXX11_RVALUE_REFERENCES
        BOOST_TEST_FWD( j == 0 );
#else
        BOOST_TEST_FWD( j == 4 );
#endif
    }

    {
        int j = 5;

        EXPR_TYPE( E,  j ^= j );
        predicate_result const& res = E.evaluate();
        BOOST_TEST_FWD( !res );
        BOOST_TEST_FWD( res.message() == " [(bool)0 is false]" );
#ifndef BOOST_NO_CXX11_RVALUE_REFERENCES
        BOOST_TEST_FWD( j == 0 );
#else
        BOOST_TEST_FWD( j == 5 );
#endif
    }
}

//____________________________________________________________________________//

#ifndef BOOST_NO_CXX11_AUTO_DECLARATIONS

BOOST_AUTO_TEST_CASE( collection_comparison )
{
    using namespace boost::test_tools;

    {
        std::vector<int> v;
        v.push_back( 1 );
        v.push_back( 2 );
        v.push_back( 3 );

        std::list<int> l;
        l.push_back( 1 );
        l.push_back( 3 );
        l.push_back( 2 );

        EXPR_TYPE( E,  v < l );
        predicate_result const& res = E.evaluate();
        BOOST_TEST_FWD( !res );
        BOOST_TEST_FWD( res.message() == ". \nMismatch in a position 0: 1 >= 1\nMismatch in a position 2: 3 >= 2" );
    }

    {
        std::vector<int> v;
        v.push_back( 1 );

        std::list<int> l;

        EXPR_TYPE( E,  v == l );
        predicate_result const& res = E.evaluate();
        BOOST_TEST_FWD( !res );
        BOOST_TEST_FWD( res.message() == ". Collections size mismatch: 1 != 0");
    }

    {
        std::vector<int> v;
        v.push_back( 1 );
        v.push_back( 2 );
        v.push_back( 3 );

        std::list<int> l;
        l.push_back( 1 );
        l.push_back( 3 );
        l.push_back( 2 );

        EXPR_TYPE( E,  v >= l );
        predicate_result const& res = E.evaluate();
        BOOST_TEST_FWD( !res );
        BOOST_TEST_FWD( res.message() == ". \nMismatch in a position 1: 2 < 3");
    }

    {
        std::string s1 = "asdfhjk";
        std::string s2 = "asdfgjk";

        EXPR_TYPE( E,  s1 == s2 );
        predicate_result const& res = E.evaluate();
        BOOST_TEST_FWD( !res );
        BOOST_TEST_FWD( res.message() == " [asdfhjk != asdfgjk]");
    }

    {
        std::string       str1 = "hello world";
        std::string       str2 = "helko worlt";

        EXPR_TYPE( E,  boost::unit_test::const_string( str1 ) == boost::unit_test::const_string( str2 ) );
        predicate_result const& res = E.evaluate();
        BOOST_TEST_FWD( !res );
        BOOST_TEST_FWD( res.message() == " [hello world != helko worlt]");
    }
}

//____________________________________________________________________________//

#endif

#if 0
struct Callable {
    int operator()() { return 0; }
    int operator()( int ) { return 1; }
    int operator()( int, int ) { return 2; }
};

BOOST_AUTO_TEST_CASE( test_predicate_invocation )
{
    using namespace boost::test_tools;

    {
        Callable c;

        EXPR_TYPE( E,  c );
        predicate_result const& res = E.evaluate();
        BOOST_TEST_FWD( !res );
        BOOST_TEST_FWD( res.message() == " [(bool)0 is false]" );
    }
}

//____________________________________________________________________________//
#endif

// EOF

