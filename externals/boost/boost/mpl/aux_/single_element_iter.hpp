
#ifndef BOOST_MPL_AUX_SINGLE_ELEMENT_ITER_HPP_INCLUDED
#define BOOST_MPL_AUX_SINGLE_ELEMENT_ITER_HPP_INCLUDED

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

#include <boost/mpl/iterator_tags.hpp>
#include <boost/mpl/advance_fwd.hpp>
#include <boost/mpl/distance_fwd.hpp>
#include <boost/mpl/next_prior.hpp>
#include <boost/mpl/deref.hpp>
#include <boost/mpl/int.hpp>
#include <boost/mpl/aux_/value_wknd.hpp>
#include <boost/mpl/aux_/config/ctps.hpp>

namespace boost { namespace mpl { 


namespace aux {

template< typename T, int is_last_ >
struct sel_iter;

template< typename T >
struct sel_iter<T,0>
{
    typedef random_access_iterator_tag category;
    typedef sel_iter<T,1> next;
    typedef T type;
};

template< typename T >
struct sel_iter<T,1>
{
    typedef random_access_iterator_tag category;
    typedef sel_iter<T,0> prior;
};

} // namespace aux

template< typename T, int is_last_, typename Distance >
struct advance< aux::sel_iter<T,is_last_>,Distance>
{
    typedef aux::sel_iter<
          T
        , ( is_last_ + BOOST_MPL_AUX_NESTED_VALUE_WKND(int, Distance) )
        > type;
};

template< 
      typename T
    , int l1
    , int l2 
    >
struct distance< aux::sel_iter<T,l1>, aux::sel_iter<T,l2> >
    : int_<( l2 - l1 )>
{
};


}}

#endif // BOOST_MPL_AUX_SINGLE_ELEMENT_ITER_HPP_INCLUDED
