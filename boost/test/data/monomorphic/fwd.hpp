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
//  Description : forward declares monomorphic datasets interfaces
// ***************************************************************************

#ifndef BOOST_TEST_DATA_MONOMORPHIC_FWD_HPP_102212GER
#define BOOST_TEST_DATA_MONOMORPHIC_FWD_HPP_102212GER

// Boost.Test
#include <boost/test/data/config.hpp>
#include <boost/test/data/size.hpp>

#include <boost/test/utils/is_forward_iterable.hpp>

// Boost
#ifdef BOOST_NO_CXX11_RVALUE_REFERENCES
#include <boost/utility/enable_if.hpp>
#else
#include <boost/utility/declval.hpp>
#endif
#include <boost/mpl/bool.hpp>
#include <boost/smart_ptr/make_shared.hpp>
#include <boost/shared_ptr.hpp>
#include <boost/type_traits/decay.hpp>

#include <boost/test/detail/suppress_warnings.hpp>

//____________________________________________________________________________//

namespace boost {
namespace unit_test {
namespace data {

namespace monomorphic {

template<typename T>
struct traits;

template<typename T>
class dataset;

template<typename T>
class singleton;

template<typename C>
class collection;

template<typename T>
class array;

#ifndef BOOST_NO_CXX11_RVALUE_REFERENCES
#  define BOOST_TEST_ENABLE_IF std::enable_if
#else
#  define BOOST_TEST_ENABLE_IF boost::enable_if_c
#endif

// ************************************************************************** //
// **************            monomorphic::is_dataset           ************** //
// ************************************************************************** //

template<typename DS>
struct is_dataset : mpl::false_ {};

//____________________________________________________________________________//

template<typename DS>
struct is_dataset<DS&> : is_dataset<DS> {};

//____________________________________________________________________________//

template<typename DS>
struct is_dataset<DS const> : is_dataset<DS> {};

//____________________________________________________________________________//

} // namespace monomorphic

// ************************************************************************** //
// **************                  data::make                  ************** //
// ************************************************************************** //

#ifndef BOOST_NO_CXX11_RVALUE_REFERENCES

template<typename DS>
inline typename BOOST_TEST_ENABLE_IF<monomorphic::is_dataset<DS>::value,DS>::type
make(DS&& ds)
{
    return std::forward<DS>( ds );
}

//____________________________________________________________________________//

template<typename T>
inline typename BOOST_TEST_ENABLE_IF<!is_forward_iterable<T>::value && 
                                     !monomorphic::is_dataset<T>::value, 
                                     monomorphic::singleton<T>
>::type
make( T&& v );

//____________________________________________________________________________//

template<typename C>
inline monomorphic::collection<typename BOOST_TEST_ENABLE_IF<unit_test::is_forward_iterable<C>::value,C>::type>
make( C&& c );

//____________________________________________________________________________//

#else

template<typename DS>
inline typename BOOST_TEST_ENABLE_IF<monomorphic::is_dataset<DS>::value,DS const&>::type
make(DS const& ds)
{
    return ds;
}

//____________________________________________________________________________//

template<typename T>
inline typename BOOST_TEST_ENABLE_IF<!is_forward_iterable<T>::value && 
                                     !monomorphic::is_dataset<T>::value, 
                                     monomorphic::singleton<T>
>::type
make( T const& v );

//____________________________________________________________________________//

template<typename C>
inline monomorphic::collection<typename BOOST_TEST_ENABLE_IF<unit_test::is_forward_iterable<C>::value,C>::type>
make( C const& c );

//____________________________________________________________________________//

#endif // BOOST_NO_CXX11_RVALUE_REFERENCES

template<typename T, std::size_t size>
inline monomorphic::array<T>
make( T (&a)[size] );

//____________________________________________________________________________//

inline monomorphic::singleton<char*>
make( char* str );

//____________________________________________________________________________//

inline monomorphic::singleton<char const*>
make( char const* str );

//____________________________________________________________________________//

#ifndef BOOST_NO_CXX11_DECLTYPE

namespace result_of {

template<typename DS>
struct make
{
    typedef decltype(data::make(boost::declval<DS>())) type;
};

} // namespace result_of

#endif // BOOST_NO_CXX11_DECLTYPE

//____________________________________________________________________________//

} // namespace data
} // namespace unit_test
} // namespace boost

#include <boost/test/detail/enable_warnings.hpp>

#endif // BOOST_TEST_DATA_MONOMORPHIC_FWD_HPP_102212GER

