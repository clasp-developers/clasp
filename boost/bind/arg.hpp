#ifndef BOOST_BIND_ARG_HPP_INCLUDED
#define BOOST_BIND_ARG_HPP_INCLUDED

// MS compatible compilers support #pragma once

#if defined(_MSC_VER)
# pragma once
#endif

//
//  bind/arg.hpp
//
//  Copyright (c) 2002 Peter Dimov and Multi Media Ltd.
//
// Distributed under the Boost Software License, Version 1.0. (See
// accompanying file LICENSE_1_0.txt or copy at
// http://www.boost.org/LICENSE_1_0.txt)
//
//  See http://www.boost.org/libs/bind/bind.html for documentation.
//

#include <boost/config.hpp>
#include <boost/is_placeholder.hpp>

namespace boost
{

template< int I > struct arg
{
    arg()
    {
    }

    template< class T > arg( T const & /* t */ )
    {
        // static assert I == is_placeholder<T>::value
        typedef char T_must_be_placeholder[ I == is_placeholder<T>::value? 1: -1 ];
    }
};

template< int I > bool operator==( arg<I> const &, arg<I> const & )
{
    return true;
}


template< int I > struct is_placeholder< arg<I> >
{
    enum _vt { value = I };
};

template< int I > struct is_placeholder< arg<I> (*) () >
{
    enum _vt { value = I };
};


} // namespace boost

#endif // #ifndef BOOST_BIND_ARG_HPP_INCLUDED
