//  (C) Copyright Gennadiy Rozental 2011-2012.
//  Distributed under the Boost Software License, Version 1.0.
//  (See accompanying file LICENSE_1_0.txt or copy at 
//  http://www.boost.org/LICENSE_1_0.txt)

//  See http://www.boost.org/libs/test for the library home page.
//
//  File        : $RCSfile$
//
//  Version     : $Revision: 74663 $
//
//  Description : defines framework for automated assertion construction
// ***************************************************************************

#ifndef BOOST_TEST_TOOLS_ASSERTION_HPP_100911GER
#define BOOST_TEST_TOOLS_ASSERTION_HPP_100911GER

// Boost.Test
#include <boost/test/utils/is_forward_iterable.hpp>
#include <boost/test/utils/is_cstring.hpp>
#include <boost/test/utils/basic_cstring/compare.hpp>

#include <boost/test/tools/floating_point_comparison.hpp>
#include <boost/test/tools/fpc_tolerance.hpp>

// Boost
#include <boost/mpl/assert.hpp>
#include <boost/utility/declval.hpp>
#include <boost/utility/enable_if.hpp>
#include <boost/type_traits/remove_reference.hpp>
#include <boost/type_traits/is_floating_point.hpp>

// STL
#ifndef BOOST_NO_CXX11_RVALUE_REFERENCES
#include <utility>
#endif

#include <boost/test/detail/suppress_warnings.hpp>

//____________________________________________________________________________//

namespace boost {
namespace test_tools {
namespace assertion {

// ************************************************************************** //
// **************             assertion::operators             ************** //
// ************************************************************************** //
// precedence 4: ->*, .*
// precedence 5: *, /, %
// precedence 6: +, -
// precedence 7: << , >>
// precedence 8: <, <=, > and >=
// precedence 9: == and !=
// precedence 10: bitwise AND
// precedence 11: bitwise XOR
// precedence 12: bitwise OR
// precedence 13: logical AND
//  disabled
// precedence 14: logical OR
//  disabled
// precedence 15: ternary conditional
//  disabled
// precedence 16: = and OP= operators
// precedence 17: throw operator
//  not supported
// precedence 18: comma
//  not supported

namespace op {

#define BOOST_TEST_FOR_EACH_COMP_OP(action) \
    action( < , LT, >= )                    \
    action( <=, LE, >  )                    \
    action( > , GT, <= )                    \
    action( >=, GE, <  )                    \
    action( ==, EQ, != )                    \
    action( !=, NE, == )                    \
/**/

//____________________________________________________________________________//

#ifndef BOOST_NO_CXX11_DECLTYPE

#define BOOST_TEST_FOR_EACH_CONST_OP(action)\
    action(->*, MEMP, ->* )                 \
                                            \
    action( * , MUL, * )                    \
    action( / , DIV, / )                    \
    action( % , MOD, % )                    \
                                            \
    action( + , ADD, + )                    \
    action( - , SUB, - )                    \
                                            \
    action( <<, LSH, << )                   \
    action( >>, RSH, >> )                   \
                                            \
    BOOST_TEST_FOR_EACH_COMP_OP(action)     \
                                            \
    action( & , BAND, & )                   \
    action( ^ , XOR, ^ )                    \
    action( | , BOR, | )                    \
/**/

#else

#define BOOST_TEST_FOR_EACH_CONST_OP(action)\
    BOOST_TEST_FOR_EACH_COMP_OP(action)     \
/**/

#endif

//____________________________________________________________________________//

#define BOOST_TEST_FOR_EACH_MUT_OP(action)  \
    action( = , SET , =  )                  \
    action( +=, IADD, += )                  \
    action( -=, ISUB, -= )                  \
    action( *=, IMUL, *= )                  \
    action( /=, IDIV, /= )                  \
    action( %=, IMOD, %= )                  \
    action(<<=, ILSH, <<=)                  \
    action(>>=, IRSH, >>=)                  \
    action( &=, IAND, &= )                  \
    action( ^=, IXOR, ^= )                  \
    action( |=, IOR , |= )                  \
/**/

//____________________________________________________________________________//

#ifndef BOOST_NO_CXX11_DECLTYPE
#   define DEDUCE_RESULT_TYPE( oper )                                   \
    decltype(boost::declval<Lhs>() oper boost::declval<Rhs>() ) optype; \
    typedef typename boost::remove_reference<optype>::type              \
/**/
#else
#   define DEDUCE_RESULT_TYPE( oper ) bool
#endif

#define DEFINE_CONST_OPER( oper, name, rev )        \
template<typename Lhs, typename Rhs,                \
         typename Enabler=void>                     \
struct name {                                       \
    typedef DEDUCE_RESULT_TYPE( oper ) result_type; \
                                                    \
    static result_type                              \
    eval( Lhs const& lhs, Rhs const& rhs )          \
    {                                               \
        return lhs oper rhs;                        \
    }                                               \
                                                    \
    template<typename PrevExprType>                 \
    static void                                     \
    report( std::ostream&       ostr,               \
            PrevExprType const& lhs,                \
            Rhs const&          rhs)                \
    {                                               \
        lhs.report( ostr );                         \
        ostr << revert()                            \
             << tt_detail::print_helper( rhs );     \
    }                                               \
                                                    \
    static char const* revert()                     \
    { return " " #rev " "; }                        \
};                                                  \
/**/

BOOST_TEST_FOR_EACH_CONST_OP( DEFINE_CONST_OPER )

#undef DEDUCE_RESULT_TYPE
#undef DEFINE_CONST_OPER

//____________________________________________________________________________//

namespace op_detail {

template <typename OP, typename Lhs, typename Rhs>
inline assertion_result
compare_collections( Lhs const& lhs, Rhs const& rhs )
{
    assertion_result pr( true );

    if( lhs.size() != rhs.size() ) {
        pr = false;
        pr.message() << "Collections size mismatch: " << lhs.size() << " != " << rhs.size();
        return pr;
    }
        
    typename Lhs::const_iterator left  = lhs.begin();
    typename Rhs::const_iterator right = rhs.begin();
    std::size_t                  pos   = 0;

    for( ; pos < lhs.size(); ++left, ++right, ++pos ) {
        if( OP::eval( *left, *right ) )
            continue;

        pr = false;
        pr.message() << "\nMismatch in a position " << pos << ": "  << *left << OP::revert() << *right;
    }

    return pr;
}

} // namespace op_detail

//____________________________________________________________________________//

#define DEFINE_CSTRING_COMPARISON( oper, name, rev )                \
template<typename Lhs,typename Rhs>                                 \
struct name<Lhs,Rhs,typename boost::enable_if_c<                    \
    unit_test::is_cstring<Lhs>::value &&                            \
    unit_test::is_cstring<Rhs>::value>::type> {                     \
    typedef typename boost::add_const<                              \
                typename remove_pointer<                            \
                    typename decay<Lhs>::type>::type>::type         \
        lhs_char_type;                                              \
    typedef typename boost::add_const<                              \
                typename remove_pointer<                            \
                    typename decay<Rhs>::type>::type>::type         \
        rhs_char_type;                                              \
public:                                                             \
    typedef assertion_result result_type;                           \
                                                                    \
    static bool                                                     \
    eval( Lhs const& lhs, Rhs const& rhs)                           \
    {                                                               \
        return unit_test::basic_cstring<lhs_char_type>(lhs) oper    \
               unit_test::basic_cstring<rhs_char_type>(rhs);        \
    }                                                               \
                                                                    \
    template<typename PrevExprType>                                 \
    static void                                                     \
    report( std::ostream&       ostr,                               \
            PrevExprType const& lhs,                                \
            Rhs const&          rhs)                                \
    {                                                               \
        lhs.report( ostr );                                         \
        ostr << revert()                                            \
             << tt_detail::print_helper( rhs );                     \
    }                                                               \
                                                                    \
    static char const* revert()                                     \
    { return " " #rev " "; }                                        \
};                                                                  \
/**/

BOOST_TEST_FOR_EACH_COMP_OP( DEFINE_CSTRING_COMPARISON )
#undef DEFINE_CSTRING_COMPARISON

//____________________________________________________________________________//

#define DEFINE_COLLECTION_COMPARISON( oper, name, _ )               \
template<typename Lhs,typename Rhs>                                 \
struct name<Lhs,Rhs,typename boost::enable_if_c<                    \
    unit_test::is_forward_iterable<Lhs>::value &&                   \
    unit_test::is_forward_iterable<Rhs>::value>::type> {            \
public:                                                             \
    typedef assertion_result result_type;                           \
                                                                    \
    static assertion_result                                         \
    eval( Lhs const& lhs, Rhs const& rhs)                           \
    {                                                               \
        typedef name<typename Lhs::value_type,                      \
                     typename Rhs::value_type> OP;                  \
        return op_detail::compare_collections<OP>(lhs, rhs);        \
    }                                                               \
                                                                    \
    template<typename PrevExprType>                                 \
    static void                                                     \
    report( std::ostream&,                                          \
            PrevExprType const&,                                    \
            Rhs const& ) {}                                         \
};                                                                  \
/**/

BOOST_TEST_FOR_EACH_COMP_OP( DEFINE_COLLECTION_COMPARISON )
#undef DEFINE_COLLECTION_COMPARISON

//____________________________________________________________________________//

namespace op_detail {

template<typename OP, typename FPT>
struct compare_fpv {
    enum { cmp_direct = true };

    template <typename Lhs, typename Rhs>
    static assertion_result
    compare( Lhs const& lhs, Rhs const& rhs )
    {
        fpc::close_at_tolerance<FPT> P( fpc_tolerance<FPT>(), fpc::FPC_STRONG );

        assertion_result ar( P( lhs, rhs ) );
        if( !ar )
            ar.message() << "Relative difference exceeds tolerance ["
                         << P.failed_fraction() << " > " << P.fraction_tolerance() << ']';
        return ar;
    }

    static assertion_result
    compare_0( FPT const& fpv )
    {
        fpc::small_with_tolerance<FPT> P( fpc_tolerance<FPT>() );

        assertion_result ar( P( fpv ) );
        if( !ar )
            ar.message() << "Absolute value exceeds tolerance [|" << fpv << "| > "<< fpc_tolerance<FPT>() << ']';

        return ar;
    }
};

//____________________________________________________________________________//

template<typename Lhs, typename Rhs, typename FPT>
struct compare_fpv<op::NE<Lhs,Rhs>,FPT> {
    enum { cmp_direct = false };

    static assertion_result
    compare( Lhs const& lhs, Rhs const& rhs )
    {
        fpc::close_at_tolerance<FPT> P( fpc_tolerance<FPT>(), fpc::FPC_STRONG );
        
        assertion_result ar( !P( lhs, rhs ) );
        if( !ar )
            ar.message() << "Relative difference is within tolerance ["
                         << P.failed_fraction() << " < " << fpc_tolerance<FPT>() << ']';
        return ar;
    }

    static assertion_result
    compare_0( FPT const& fpv )
    {
        fpc::small_with_tolerance<FPT> P( fpc_tolerance<FPT>() );

        assertion_result ar( !P( fpv ) );
        if( !ar )
            ar.message() << "Absolute value is within tolerance [|" << fpv << "| < "<< fpc_tolerance<FPT>() << ']';
        return ar;
    }
};

//____________________________________________________________________________//

} // namespace op_detail

#define DEFINE_FPV_COMPARISON( oper, name, rev )                    \
template<typename Lhs,typename Rhs>                                 \
struct name<Lhs,Rhs,typename boost::enable_if_c<                    \
    is_floating_point<Lhs>::value &&                                \
    is_floating_point<Rhs>::value>::type> {                         \
        typedef typename numeric::conversion_traits<Lhs,Rhs         \
    >::supertype FPT;                                               \
    typedef name<Lhs,Rhs> OP;                                       \
public:                                                             \
    typedef assertion_result result_type;                           \
                                                                    \
    static bool                                                     \
    eval_direct( Lhs const& lhs, Rhs const& rhs)                    \
    {                                                               \
        return lhs oper rhs;                                        \
    }                                                               \
                                                                    \
    static assertion_result                                         \
    eval( Lhs const& lhs, Rhs const& rhs)                           \
    {                                                               \
        if( lhs == Lhs() )                                          \
            return op_detail::compare_fpv<OP,Rhs>::compare_0(rhs);  \
                                                                    \
        if( rhs == Rhs() )                                          \
            return op_detail::compare_fpv<OP,Lhs>::compare_0(lhs);  \
                                                                    \
        bool direct_res = eval_direct( lhs, rhs );                  \
                                                                    \
        if(direct_res && op_detail::compare_fpv<OP,FPT>::cmp_direct \
            || fpc_tolerance<FPT>() == FPT())                       \
            return direct_res;                                      \
                                                                    \
        return op_detail::compare_fpv<OP,FPT>::compare(lhs, rhs);   \
    }                                                               \
                                                                    \
    template<typename PrevExprType>                                 \
    static void                                                     \
    report( std::ostream& ostr,                                     \
            PrevExprType const& lhs,                                \
            Rhs const& rhs )                                        \
    {                                                               \
        lhs.report( ostr );                                         \
        ostr << revert()                                            \
             << tt_detail::print_helper( rhs );                     \
    }                                                               \
                                                                    \
    static char const* revert()                                     \
    { return " " #rev " "; }                                        \
};                                                                  \
/**/

BOOST_TEST_FOR_EACH_COMP_OP( DEFINE_FPV_COMPARISON )
#undef DEFINE_FPV_COMPARISON

//____________________________________________________________________________//

} // namespace op

// ************************************************************************** //
// **************          assertion::expression_base          ************** //
// ************************************************************************** //
// Defines expression operators

template<typename Lhs, typename Rhs, typename OP> class binary_expr;

template<typename ExprType,typename ValType>
class expression_base {
public:

#ifndef BOOST_NO_CXX11_RVALUE_REFERENCES

#define ADD_OP_SUPPORT( oper, name, _ )                         \
    template<typename T>                                        \
    binary_expr<ExprType,T,                                     \
        op::name<ValType,typename remove_reference<T>::type> >  \
    operator oper( T&& rhs )                                    \
    {                                                           \
        return binary_expr<ExprType,T,                          \
         op::name<ValType,typename remove_reference<T>::type> > \
            ( std::forward<ExprType>(                           \
                *static_cast<ExprType*>(this) ),                \
              std::forward<T>(rhs) );                           \
    }                                                           \
/**/
#else

#define ADD_OP_SUPPORT( oper, name, _ )                         \
    template<typename T>                                        \
    binary_expr<ExprType,typename decay<T const>::type,         \
        op::name<ValType,typename decay<T const>::type> >       \
    operator oper( T const& rhs ) const                         \
    {                                                           \
        typedef typename decay<T const>::type Rhs;              \
        return binary_expr<ExprType,Rhs,op::name<ValType,Rhs> > \
            ( *static_cast<ExprType const*>(this),              \
              rhs );                                            \
    }                                                           \
/**/
#endif

    BOOST_TEST_FOR_EACH_CONST_OP( ADD_OP_SUPPORT )
    #undef ADD_OP_SUPPORT

#ifndef BOOST_NO_CXX11_AUTO_DECLARATIONS
    // Disabled operators
    template<typename T>
    ExprType&
    operator ||( T const& rhs )
    {
        BOOST_MPL_ASSERT_MSG(false, CANT_USE_LOGICAL_OPERATOR_OR_WITHIN_THIS_TESTING_TOOL, () );

        return *static_cast<ExprType*>(this);
    }

    template<typename T>
    ExprType&
    operator &&( T const& rhs )
    {
        BOOST_MPL_ASSERT_MSG(false, CANT_USE_LOGICAL_OPERATOR_AND_WITHIN_THIS_TESTING_TOOL, () );

        return *static_cast<ExprType*>(this);
    }

    operator bool()
    {
        BOOST_MPL_ASSERT_MSG(false, CANT_USE_TERNARY_OPERATOR_WITHIN_THIS_TESTING_TOOL, () );

        return false;
    }
#endif
};

// ************************************************************************** //
// **************            assertion::value_expr             ************** //
// ************************************************************************** //
// simple value expression

template<typename T>
class value_expr : public expression_base<value_expr<T>,typename remove_reference<T>::type> {
public:
    // Public types
    typedef T                   result_type;

    // Constructor
#ifndef BOOST_NO_CXX11_RVALUE_REFERENCES
    value_expr( value_expr&& ve )
    : m_value( std::forward<T>(ve.m_value) )
    {}
    explicit                    value_expr( T&& val )
    : m_value( std::forward<T>(val) )
    {}
#else
    explicit                    value_expr( T const& val )
    : m_value( val )
    {}
#endif

    // Specific expresson interface
    T const&                    value() const
    {
        return m_value;
    }
    void                        report( std::ostream& ostr ) const
    {
        ostr << tt_detail::print_helper( m_value );
    }

    // Mutating operators
#define ADD_OP_SUPPORT( OPER, ID, _ )   \
    template<typename U>                \
    value_expr<T>&                      \
    operator OPER( U const& rhs )       \
    {                                   \
        m_value OPER rhs;               \
                                        \
        return *this;                   \
    }                                   \
/**/

    BOOST_TEST_FOR_EACH_MUT_OP( ADD_OP_SUPPORT )
#undef ADD_OP_SUPPORT

    // expression interface
    assertion_result            evaluate( bool no_message = false ) const
    {
        assertion_result res( value() );
        if( no_message || res )
            return res;

        format_message( res.message(), value() );

        return tt_detail::format_assertion_result( "", res.message().str() );
    }

private:
    template<typename U>
    static void format_message( wrap_stringstream& ostr, U const& v )   { ostr << "[(bool)" << v << " is false]"; }
    static void format_message( wrap_stringstream& ostr, bool v )       {}
    static void format_message( wrap_stringstream& ostr, assertion_result const& v ) {}

    // Data members
    T                           m_value;
};

// ************************************************************************** //
// **************            assertion::binary_expr            ************** //
// ************************************************************************** //
// binary expression

template<typename LExpr, typename Rhs, typename OP>
class binary_expr : public expression_base<binary_expr<LExpr,Rhs,OP>,typename OP::result_type> {
public:
    // Public types
    typedef typename OP::result_type result_type;

    // Constructor
#ifndef BOOST_NO_CXX11_RVALUE_REFERENCES
    binary_expr( binary_expr&& be )
    : m_lhs( std::forward<LExpr>(be.m_lhs) )
    , m_rhs( std::forward<Rhs>(be.m_rhs) )
    {}
    binary_expr( LExpr&& lhs, Rhs&& rhs )
    : m_lhs( std::forward<LExpr>(lhs) )
    , m_rhs( std::forward<Rhs>(rhs) )
    {}
#else
    binary_expr( LExpr const& lhs, Rhs const& rhs )
    : m_lhs( lhs )
    , m_rhs( rhs )
    {}
#endif

    // Specific expression interface
    result_type                 value() const
    {
        return OP::eval( m_lhs.value(), m_rhs );
    }
    void                        report( std::ostream& ostr ) const
    {
        return OP::report( ostr, m_lhs, m_rhs );
    }

    assertion_result            evaluate( bool no_message = false ) const
    {
        assertion_result const expr_res( value() );
        if( no_message || expr_res )
            return expr_res;

        wrap_stringstream buff;
        report( buff.stream() );

        return tt_detail::format_assertion_result( buff.stream().str(), expr_res.message() );
    }

    // To support custom manipulators
    LExpr const&                lhs() const     { return m_lhs; }
    Rhs const&                  rhs() const     { return m_rhs; }
private:
    // Data members
    LExpr                       m_lhs;
    Rhs                         m_rhs;
};

// ************************************************************************** //
// **************               assertion::seed                ************** //
// ************************************************************************** //
// seed added ot the input expression to form an assertion expression

class seed {
public:
    // ->* is highest precedence left to right operator
    template<typename T>
    value_expr<T>
#ifndef BOOST_NO_CXX11_RVALUE_REFERENCES
    operator->*( T&& v ) const
    {
        return value_expr<T>( std::forward<T>( v ) );
    }
#else
    operator->*( T const& v )  const
    {
        return value_expr<T>( v );
    }
#endif

};

#undef BOOST_TEST_FOR_EACH_CONST_OP

} // namespace assertion
} // namespace test_tools
} // namespace boost

#include <boost/test/detail/enable_warnings.hpp>

#endif // BOOST_TEST_TOOLS_ASSERTION_HPP_100911GER

