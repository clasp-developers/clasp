//  (C) Copyright Gennadiy Rozental 2011-2012.
//  Distributed under the Boost Software License, Version 1.0.
//  (See accompanying file LICENSE_1_0.txt or copy at
//  http://www.boost.org/LICENSE_1_0.txt)

//  See http://www.boost.org/libs/test for the library home page.
//
//  File        : $RCSfile$
//
//  Version     : $Revision: 74248 $
//
//  Description : FPC tolerance manipulator implementation
// ***************************************************************************

#ifndef BOOST_TEST_TOOLS_DETAIL_TOLERANCE_MANIP_HPP_012705GER
#define BOOST_TEST_TOOLS_DETAIL_TOLERANCE_MANIP_HPP_012705GER

// Boost Test
#include <boost/test/tools/detail/fwd.hpp>
#include <boost/test/tools/detail/indirections.hpp>

#include <boost/test/tools/fpc_tolerance.hpp>

// Boost
#include <boost/type_traits/is_floating_point.hpp>

#include <boost/test/detail/suppress_warnings.hpp>

//____________________________________________________________________________//

namespace boost {
namespace test_tools {
namespace tt_detail {

// ************************************************************************** //
// **************           fpc tolerance manipulator          ************** //
// ************************************************************************** //

template<typename FPT>
struct tolerance_manip {
    explicit    tolerance_manip( FPT tol ) : m_value( tol ) {}

    FPT m_value;
};

//____________________________________________________________________________//

struct tolerance_manip_delay {};

template<typename FPT>
inline tolerance_manip<FPT>
operator%( FPT v, tolerance_manip_delay const& )
{
    BOOST_STATIC_ASSERT( is_floating_point<FPT>::value );

    return tolerance_manip<FPT>( v * static_cast<FPT>(0.01) ); 
}

//____________________________________________________________________________//

template<typename E, typename FPT>
inline assertion_result
operator<<(assertion_evaluate_t<E> const& ae, tolerance_manip<FPT> const& tol)
{
    local_fpc_tolerance<FPT> lt( tol.m_value );

    return ae.m_e.evaluate();
}

//____________________________________________________________________________//

template<typename FPT>
inline int
operator<<( unit_test::lazy_ostream const&, tolerance_manip<FPT> const& )   { return 0; }

//____________________________________________________________________________//

template<typename FPT>
inline check_type
operator<<( assertion_type const& at, tolerance_manip<FPT> const& )         { return CHECK_BUILT_ASSERTION; }

//____________________________________________________________________________//
 
} // namespace tt_detail

template<typename FPT>
inline tt_detail::tolerance_manip<FPT>
tolerance( FPT v )
{
    BOOST_STATIC_ASSERT( is_floating_point<FPT>::value );

    return tt_detail::tolerance_manip<FPT>( v );
}

//____________________________________________________________________________//

template<typename FPT>
inline tt_detail::tolerance_manip<FPT>
tolerance( fpc::percent_tolerance_t<FPT> v )
{
    BOOST_STATIC_ASSERT( is_floating_point<FPT>::value );

    return tt_detail::tolerance_manip<FPT>( v.m_value * static_cast<FPT>(0.01) );
}

//____________________________________________________________________________//

inline tt_detail::tolerance_manip_delay
tolerance()
{
    return tt_detail::tolerance_manip_delay();
}

//____________________________________________________________________________//

} // namespace test_tools
} // namespace boost

#include <boost/test/detail/enable_warnings.hpp>

#endif // BOOST_TEST_TOOLS_DETAIL_TOLERANCE_MANIP_HPP_012705GER
