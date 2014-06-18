
#ifndef BOOST_MPL_MULTISET_AUX_COUNT_IMPL_HPP_INCLUDED
#define BOOST_MPL_MULTISET_AUX_COUNT_IMPL_HPP_INCLUDED

// Copyright Aleksey Gurtovoy 2003-2004
//
// Distributed under the Boost Software License, Version 1.0. 
// (See accompanying file LICENSE_1_0.txt or copy at 
// http://www.boost.org/LICENSE_1_0.txt)
//
// See http://www.boost.org/libs/mpl for documentation.

// $Id$
// $Date$
// $Revision$

#include <boost/mpl/multiset/aux_/tag.hpp>
#include <boost/mpl/count_fwd.hpp>
#include <boost/mpl/int.hpp>
#include <boost/mpl/aux_/type_wrapper.hpp>
#include <boost/mpl/aux_/static_cast.hpp>
#include <boost/mpl/aux_/config/static_constant.hpp>
#include <boost/mpl/aux_/config/workaround.hpp>
#include <boost/mpl/aux_/config/msvc.hpp>

namespace boost { namespace mpl {

template<>
struct count_impl< aux::multiset_tag >
{
    template< typename Set, typename Key > struct apply
    {
        enum { msvc71_wknd_ = sizeof(Set::key_count(BOOST_MPL_AUX_STATIC_CAST(aux::type_wrapper<Key>*,0))) - 1 };
        typedef int_< msvc71_wknd_ > type;
        BOOST_STATIC_CONSTANT(int, value = msvc71_wknd_);
    };
};

}}

#endif // BOOST_MPL_MULTISET_AUX_COUNT_IMPL_HPP_INCLUDED
