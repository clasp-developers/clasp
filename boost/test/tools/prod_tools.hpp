//  (C) Copyright Gennadiy Rozental 2009-2012.
//  Distributed under the Boost Software License, Version 1.0.
//  (See accompanying file LICENSE_1_0.txt or copy at 
//  http://www.boost.org/LICENSE_1_0.txt)

//  See http://www.boost.org/libs/test for the library home page.
//
//  File        : $RCSfile$
//
//  Version     : $Revision$
//
//  Description : contains implementation of test tools usable in production
// ***************************************************************************

#ifndef BOOST_TEST_PROD_TOOLS_HPP_122109GER
#define BOOST_TEST_PROD_TOOLS_HPP_122109GER

// Boost.Test
#define BOOST_TEST_PROD
#include <boost/test/test_tools.hpp>

// Boost
#include <boost/throw_exception.hpp>
#include <boost/assert.hpp>

#include <boost/test/detail/suppress_warnings.hpp>

//____________________________________________________________________________//

// ************************************************************************** //
// **************                    TOOL BOX                  ************** //
// ************************************************************************** //

//____________________________________________________________________________//

#ifndef BOOST_TEST_TOOL_REPORT_WARN_FAILURE
#define BOOST_TEST_TOOL_REPORT_WARN_FAILURE( failure_descr ) 
#endif

//____________________________________________________________________________//

#ifndef BOOST_TEST_TOOL_REPORT_CHECK_FAILURE
#define BOOST_TEST_TOOL_REPORT_CHECK_FAILURE( failure_descr ) \
    BOOST_THROW_EXCEPTION( std::runtime_error( failure_descr ) )
#endif

//____________________________________________________________________________//

#ifndef BOOST_TEST_TOOL_REPORT_REQUIRE_FAILURE
#define BOOST_TEST_TOOL_REPORT_REQUIRE_FAILURE( failure_descr ) \
    BOOST_ASSERT( false )
#endif

//____________________________________________________________________________//

#ifndef BOOST_TEST_TOOL_REPORT_FORMAT

#define BOOST_TEST_TOOL_REPORT_ARG( r, _, arg ) , arg, BOOST_STRINGIZE( arg )
#define BOOST_TEST_TOOL_REPORT_ARG_DESCR( r, _, arg ) , BOOST_STRINGIZE( arg )

#define BOOST_TEST_TOOL_REPORT_ARGS1(ARGS) \
    BOOST_PP_SEQ_SIZE(ARGS) BOOST_PP_SEQ_FOR_EACH( BOOST_TEST_TOOL_REPORT_ARG_DESCR, _, ARGS )
#define BOOST_TEST_TOOL_REPORT_ARGS2(ARGS) 0

#define BOOST_TEST_TOOL_REPORT_FORMAT( frwd_type, pred_res, assertion_descr, CT, ARGS )     \
    ::boost::test_tools::tt_detail::prod_report_format( pred_res,                           \
        BOOST_TEST_LAZY_MSG( assertion_descr ),                                             \
        ::boost::test_tools::tt_detail::CT,                                                 \
        BOOST_JOIN( BOOST_TEST_TOOL_REPORT_ARGS, frwd_type )( ARGS ) )                      \
/**/

#endif

//____________________________________________________________________________//

// 0 - args exists and need to be forwarded; call prod_check_frwd
#define BOOST_TEST_TOOL_IMPL0( P, assertion_descr, TL, CT, ARGS )                           \
    if( BOOST_TEST_TOOL_PRED_HOLDER( P, assertion_descr, CT, ARGS ) )                       \
        ((void)0);                                                                          \
    else BOOST_JOIN( BOOST_JOIN( BOOST_TEST_TOOL_REPORT_, TL), _FAILURE)(                   \
        PH.failure_descr() )                                                                \
/**/

//____________________________________________________________________________//

// 1 - args exists, but do not need to be forwarded
#define BOOST_TEST_TOOL_IMPL1( P, assertion_descr, TL, CT, ARGS )                           \
    if( ::boost::test_tools::assertion_result const& pr = P BOOST_PP_SEQ_TO_TUPLE( ARGS ) ) \
        ((void)0);                                                                          \
    else BOOST_JOIN( BOOST_JOIN( BOOST_TEST_TOOL_REPORT_, TL), _FAILURE)(                   \
            BOOST_TEST_TOOL_REPORT_FORMAT( 1, pr, assertion_descr, CT, ARGS ) )             \
/**/

//____________________________________________________________________________//

// 2 - assertion with no arguments; 
#define BOOST_TEST_TOOL_IMPL2( P, assertion_descr, TL, CT, _ )                              \
    if( ::boost::test_tools::assertion_result const& pr = P )                               \
        ((void)0);                                                                          \
    else BOOST_JOIN( BOOST_JOIN( BOOST_TEST_TOOL_REPORT_, TL), _FAILURE)(                   \
            BOOST_TEST_TOOL_REPORT_FORMAT( 2, pr, assertion_descr, CT, _ ) )                \
/**/

//____________________________________________________________________________//

#define BOOST_TEST_TOOL_IMPL( frwd_type, P, check_descr, TL, CT, ARGS ) \
    BOOST_JOIN( BOOST_TEST_TOOL_IMPL, frwd_type )( P, check_descr, TL, CT, ARGS )

//____________________________________________________________________________//

namespace boost {
namespace test_tools {
namespace tt_detail {

// ************************************************************************** //
// **************               prod_report_format             ************** //
// ************************************************************************** //

BOOST_TEST_DECL std::string 
prod_report_format( assertion_result const& pr, 
                    unit_test::lazy_ostream const& assertion_descr, 
                    check_type ct, std::size_t num_args, ... );

//____________________________________________________________________________//

// ************************************************************************** //
// **************                predicate_holder              ************** //
// ************************************************************************** //

#define BOOST_TEST_TOOL_PASS_ARG( r, _, arg ) , arg, BOOST_STRINGIZE( arg )

#define BOOST_TEST_TOOL_PRED_HOLDER( P, assertion_descr, CT, ARGS )                 \
::boost::test_tools::tt_detail::predicate_holder const& PH =                        \
::boost::test_tools::tt_detail::predicate_holder( P, assertion_descr,               \
    ::boost::test_tools::tt_detail::CT                                              \
    BOOST_PP_SEQ_FOR_EACH( BOOST_TEST_TOOL_PASS_ARG, _, ARGS ) )                    \
/**/

#define TEMPL_PARAMS( z, m, dummy ) , typename BOOST_JOIN( Arg, m )

#define FUNC_PARAMS( z, m, dummy )                                                  \
 , BOOST_JOIN( Arg, m ) const& BOOST_JOIN( arg, m )                                 \
 , char const* BOOST_JOIN( BOOST_JOIN( arg, m ), _descr )                           \
/**/

#define PRED_PARAMS( z, m, dummy ) BOOST_PP_COMMA_IF( m ) BOOST_JOIN( arg, m ) 

#define ARG_INFO( z, m, dummy )                                                     \
 , BOOST_JOIN( BOOST_JOIN( arg, m ), _descr )                                       \
 , &static_cast<const unit_test::lazy_ostream&>(unit_test::lazy_ostream::instance() \
        << ::boost::test_tools::tt_detail::print_helper( BOOST_JOIN( arg, m ) ))    \
/**/

#define CONSTRUCTOR_IMPL( z, n, dummy )                                             \
template<typename Pred                                                              \
         BOOST_PP_REPEAT_ ## z( BOOST_PP_ADD( n, 1 ), TEMPL_PARAMS, _ )>            \
predicate_holder( Pred P, char const* assertion_descr, check_type ct                \
                  BOOST_PP_REPEAT_ ## z( BOOST_PP_ADD( n, 1 ), FUNC_PARAMS, _ ) )   \
: m_failure_descr( 0 )                                                              \
{                                                                                   \
    assertion_result const& pr =                                                    \
        P( BOOST_PP_REPEAT_ ## z( BOOST_PP_ADD( n, 1 ), PRED_PARAMS, _ ) );         \
    if( pr ) return;                                                                \
                                                                                    \
    m_failure_descr = new std::string;                                              \
    *m_failure_descr = prod_report_format( pr,                                      \
        BOOST_TEST_LAZY_MSG( assertion_descr ),                                     \
        ct,                                                                         \
        BOOST_PP_ADD( n, 1 )                                                        \
        BOOST_PP_REPEAT_ ## z( BOOST_PP_ADD( n, 1 ), ARG_INFO, _ ) );               \
}                                                                                   \
/**/

#ifndef BOOST_TEST_MAX_PREDICATE_ARITY
#define BOOST_TEST_MAX_PREDICATE_ARITY 5
#endif

struct predicate_holder {
    BOOST_PP_REPEAT( BOOST_TEST_MAX_PREDICATE_ARITY, CONSTRUCTOR_IMPL, _ )

    ~predicate_holder()                         { if( m_failure_descr ) delete m_failure_descr; }
    std::string const&  failure_descr() const   { return *m_failure_descr; }
    operator            bool() const            { return !m_failure_descr; }
private:
    // Data members
    std::string*        m_failure_descr;
};

#undef TEMPL_PARAMS
#undef FUNC_PARAMS
#undef PRED_INFO
#undef ARG_INFO
#undef CONSTRUCTOR_IMPL

//____________________________________________________________________________//

} // namespace tt_detail
} // namespace test_tools
} // namespace boost

#include <boost/test/detail/enable_warnings.hpp>

#endif // BOOST_TEST_PROD_TOOLS_HPP_122109GER
