#ifndef BOOST_REF_HPP_INCLUDED
#define BOOST_REF_HPP_INCLUDED

// MS compatible compilers support #pragma once

#if defined(_MSC_VER)
# pragma once
#endif

#include <boost/config.hpp>
#include <boost/utility/addressof.hpp>
#include <boost/mpl/bool.hpp>
#include <boost/detail/workaround.hpp>

//
//  ref.hpp - ref/cref, useful helper functions
//
//  Copyright (C) 1999, 2000 Jaakko Jarvi (jaakko.jarvi@cs.utu.fi)
//  Copyright (C) 2001, 2002 Peter Dimov
//  Copyright (C) 2002 David Abrahams
//
// Distributed under the Boost Software License, Version 1.0. (See
// accompanying file LICENSE_1_0.txt or copy at
// http://www.boost.org/LICENSE_1_0.txt)
//
//  See http://www.boost.org/libs/bind/ref.html for documentation.
//

namespace boost
{

template<class T> class reference_wrapper
{
public:
    typedef T type;

    BOOST_FORCEINLINE explicit reference_wrapper(T& t): t_(boost::addressof(t)) {}

    BOOST_FORCEINLINE operator T& () const { return *t_; }

    BOOST_FORCEINLINE T& get() const { return *t_; }

    BOOST_FORCEINLINE T* get_pointer() const { return t_; }

private:

    T* t_;
};

# if defined( __BORLANDC__ ) && BOOST_WORKAROUND( __BORLANDC__, BOOST_TESTED_AT(0x581) )
#  define BOOST_REF_CONST
# else
#  define BOOST_REF_CONST const
# endif

template<class T> BOOST_FORCEINLINE  reference_wrapper<T> BOOST_REF_CONST ref(T & t)
{
    return reference_wrapper<T>(t);
}

template<class T> BOOST_FORCEINLINE  reference_wrapper<T const> BOOST_REF_CONST cref(T const & t)
{
    return reference_wrapper<T const>(t);
}

# undef BOOST_REF_CONST


template<typename T>
class is_reference_wrapper
    : public mpl::false_
{
};

template<typename T>
class unwrap_reference
{
 public:
    typedef T type;
};

#  define AUX_REFERENCE_WRAPPER_METAFUNCTIONS_DEF(X) \
template<typename T> \
class is_reference_wrapper< X > \
    : public mpl::true_ \
{ \
}; \
\
template<typename T> \
class unwrap_reference< X > \
{ \
 public: \
    typedef T type; \
}; \
/**/

AUX_REFERENCE_WRAPPER_METAFUNCTIONS_DEF(reference_wrapper<T>)
#if !defined(BOOST_NO_CV_SPECIALIZATIONS)
AUX_REFERENCE_WRAPPER_METAFUNCTIONS_DEF(reference_wrapper<T> const)
AUX_REFERENCE_WRAPPER_METAFUNCTIONS_DEF(reference_wrapper<T> volatile)
AUX_REFERENCE_WRAPPER_METAFUNCTIONS_DEF(reference_wrapper<T> const volatile)
#endif

#  undef AUX_REFERENCE_WRAPPER_METAFUNCTIONS_DEF


template <class T> BOOST_FORCEINLINE  typename unwrap_reference<T>::type&
unwrap_ref(T& t)
{
    return t;
}

template<class T> BOOST_FORCEINLINE  T* get_pointer( reference_wrapper<T> const & r )
{
    return r.get_pointer();
}

} // namespace boost

#endif // #ifndef BOOST_REF_HPP_INCLUDED
