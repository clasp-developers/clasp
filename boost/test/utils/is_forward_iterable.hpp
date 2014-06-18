//  (C) Copyright Gennadiy Rozental 2012.
//  Distributed under the Boost Software License, Version 1.0.
//  (See accompanying file LICENSE_1_0.txt or copy at 
//  http://www.boost.org/LICENSE_1_0.txt)

//  See http://www.boost.org/libs/test for the library home page.
//
//  File        : $RCSfile$
//
//  Version     : $Revision$
//
//  Description : defines the is_forward_iterable collection type trait
// ***************************************************************************

#ifndef BOOST_TEST_IS_FORWARD_ITERABLE_HPP_110612GER
#define BOOST_TEST_IS_FORWARD_ITERABLE_HPP_110612GER

#ifdef BOOST_NO_CXX11_DECLTYPE
// Boost
#include <boost/mpl/bool.hpp>

// STL
#include <list>
#include <vector>

#else

// Boost
#include <boost/utility/declval.hpp>
#include <boost/type_traits/is_same.hpp>
#include <boost/type_traits/remove_reference.hpp>
#include <boost/type_traits/remove_cv.hpp>

// STL
#include <utility> 
#include <type_traits> 

#endif
//____________________________________________________________________________//

namespace boost {
namespace unit_test {

// ************************************************************************** //
// **************             is_forward_iterable              ************** //
// ************************************************************************** //

#ifdef BOOST_NO_CXX11_DECLTYPE
template<typename T>
struct is_forward_iterable : public mpl::false_ {};

template<typename T>
struct is_forward_iterable<T const> : public is_forward_iterable<T> {};

template<typename T>
struct is_forward_iterable<T&> : public is_forward_iterable<T> {};

template<typename T>
struct is_forward_iterable<std::vector<T> > : public mpl::true_ {};

template<typename T>
struct is_forward_iterable<std::list<T> > : public mpl::true_ {};

#else

namespace ut_detail {

template<typename T>
struct is_present : public mpl::true_ {};

struct is_forward_iterable_impl {
    template<typename T>
    static typename std::enable_if<
        is_present<typename T::const_iterator>::value            &&
        is_present<typename T::value_type>::value                &&
        is_present<decltype(boost::declval<T>().size())>::value  &&
        is_present<decltype(boost::declval<T>().begin())>::value &&
        !is_same<typename remove_cv<typename T::value_type>::type,char>::value &&
        !is_same<typename remove_cv<typename T::value_type>::type,wchar_t>::value
    , mpl::true_>::type
    test(int);

    template<typename> 
    static std::false_type  test(...); 
}; 

} // namespace ut_detail

template<typename T> 
struct is_forward_iterable { 
    typedef decltype(ut_detail::is_forward_iterable_impl::test<typename std::remove_reference<T>::type>(0)) type;
    enum { value = type::value };
}; 

#endif

} // namespace unit_test
} // namespace boost

#endif // BOOST_TEST_IS_FORWARD_ITERABLE_HPP_110612GER

