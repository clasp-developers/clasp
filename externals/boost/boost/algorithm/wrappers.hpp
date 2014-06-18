/* 
   Copyright (c) Marshall Clow 2012.

   Distributed under the Boost Software License, Version 1.0. (See accompanying
   file LICENSE_1_0.txt or copy at http://www.boost.org/LICENSE_1_0.txt)
   
   Alternate interfaces (aka "wrappers") for algorithms.
*/

#ifndef BOOST_ALGORITHM_WRAPPERS_HPP
#define BOOST_ALGORITHM_WRAPPERS_HPP

namespace boost { namespace algorithm {

/// \fn find_ptr ( Container &c, Key k )
/// \return a pointer to the value matching the key in the container, 
/// or NULL if the key does not exist in the container.
///
/// \note: This is a wrapper around Container::find, with a useful interface.
/// Suggested by Olaf van der Spek 
/// 
/// \param c    The container to be searched
/// \param k    The key value to search with
template <class Container, class Key>
typename Container::value_type::second_type* 
find_ptr ( Container &c, Key k )
{
    typename Container::iterator iter = c.find ( k );
    return iter == c.end() ? NULL : &iter->second;
}

/// \fn find_ptr ( const Container &c, Key k )
/// \return a pointer to the value matching the key in the container, 
/// or NULL if the key does not exist in the container.
///
/// \note: This is a wrapper around Container::find, with a useful interface.
/// Suggested by Olaf van der Spek 
/// 
/// \param c    The container to be searched
/// \param k    The key value to search with
template <class Container, class Key>
const typename Container::value_type::second_type* 
find_ptr ( const Container &c, Key k )
{
    typename Container::const_iterator iter = c.find ( k );
    return iter == c.end() ? NULL : &iter->second;
}


}}

#endif
