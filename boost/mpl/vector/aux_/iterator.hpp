
#ifndef BOOST_MPL_AUX_VECTOR_ITERATOR_HPP_INCLUDED
#define BOOST_MPL_AUX_VECTOR_ITERATOR_HPP_INCLUDED

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

#include <boost/mpl/vector/aux_/at.hpp>
#include <boost/mpl/iterator_tags.hpp>
#include <boost/mpl/plus.hpp>
#include <boost/mpl/minus.hpp>
#include <boost/mpl/advance_fwd.hpp>
#include <boost/mpl/distance_fwd.hpp>
#include <boost/mpl/next.hpp>
#include <boost/mpl/prior.hpp>
#include <boost/mpl/aux_/value_wknd.hpp>
#include <boost/mpl/aux_/config/ctps.hpp>
#include <boost/mpl/aux_/config/workaround.hpp>

namespace boost { namespace mpl {

template<
      typename Vector
    , long n_
    >
struct v_iter
{
    typedef aux::v_iter_tag tag;
    typedef random_access_iterator_tag category;
    typedef typename v_at<Vector,n_>::type type;

    typedef Vector vector_;
    typedef mpl::long_<n_> pos;


};



template<
      typename Vector
    , long n_
    >
struct next< v_iter<Vector,n_> >
{
    typedef v_iter<Vector,(n_ + 1)> type;
};

template<
      typename Vector
    , long n_
    >
struct prior< v_iter<Vector,n_> >
{
    typedef v_iter<Vector,(n_ - 1)> type;
};

template<
      typename Vector
    , long n_
    , typename Distance
    >
struct advance< v_iter<Vector,n_>,Distance>
{
    typedef v_iter<
          Vector
        , (n_ + BOOST_MPL_AUX_NESTED_VALUE_WKND(long, Distance))
        > type;
};

template< 
      typename Vector
    , long n_
    , long m_
    > 
struct distance< v_iter<Vector,n_>, v_iter<Vector,m_> >
    : mpl::long_<(m_ - n_)>
{
};


}}

#endif // BOOST_MPL_AUX_VECTOR_ITERATOR_HPP_INCLUDED
