
#ifndef BOOST_MPL_AUX_VALUE_WKND_HPP_INCLUDED
#define BOOST_MPL_AUX_VALUE_WKND_HPP_INCLUDED

// Copyright Aleksey Gurtovoy 2000-2004
//
// Distributed under the Boost Software License, Version 1.0. 
// (See accompanying file LICENSE_1_0.txt or copy at 
// http://www.boost.org/LICENSE_1_0.txt)
//
// See http://www.boost.org/libs/mpl for documentation.

// $Id$
// $Date$
// $Revision$

#include <boost/mpl/aux_/static_cast.hpp>
#include <boost/mpl/aux_/config/integral.hpp>
#include <boost/mpl/aux_/config/workaround.hpp>

#if defined(BOOST_MPL_CFG_BCC_INTEGRAL_CONSTANTS)

#   include <boost/mpl/int.hpp>

namespace boost { namespace mpl { namespace aux {
template< typename C_ > struct value_wknd
    : C_
{
};

}}}


#   define BOOST_MPL_AUX_VALUE_WKND(C) \
    ::BOOST_MPL_AUX_ADL_BARRIER_NAMESPACE::aux::value_wknd< C > \
/**/
#    define BOOST_MPL_AUX_MSVC_VALUE_WKND(C) BOOST_MPL_AUX_VALUE_WKND(C)

#else // BOOST_MPL_CFG_BCC_INTEGRAL_CONSTANTS

#   define BOOST_MPL_AUX_VALUE_WKND(C) C
#   define BOOST_MPL_AUX_MSVC_VALUE_WKND(C) C

#endif

#if BOOST_WORKAROUND(__EDG_VERSION__, <= 238)
#   define BOOST_MPL_AUX_NESTED_VALUE_WKND(T, C) \
    BOOST_MPL_AUX_STATIC_CAST(T, C::value) \
/**/
#else
#   define BOOST_MPL_AUX_NESTED_VALUE_WKND(T, C) \
    BOOST_MPL_AUX_VALUE_WKND(C)::value \
/**/
#endif


namespace boost { namespace mpl { namespace aux {

template< typename T > struct value_type_wknd
{
    typedef typename T::value_type type;
};


}}}

#endif // BOOST_MPL_AUX_VALUE_WKND_HPP_INCLUDED
