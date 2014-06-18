//  (C) Copyright Gennadiy Rozental 2011-2012. JIM
//  Distributed under the Boost Software License, Version 1.0.
//  (See accompanying file LICENSE_1_0.txt or copy at 
//  http://www.boost.org/LICENSE_1_0.txt)

//  See http://www.boost.org/libs/test for the library home page.
//
//  File        : $RCSfile$
//
//  Version     : $Revision$
//
//  Description : defines common dataset macros
// ***************************************************************************

#ifndef BOOST_TEST_DATA_CONFIG_HPP_112611GER
#define BOOST_TEST_DATA_CONFIG_HPP_112611GER

// Boost.Test
#include <boost/test/detail/config.hpp>

// STL
#include <stdexcept> // for std::logic_error

//____________________________________________________________________________//

#define BOOST_TEST_DS_ERROR( msg ) throw std::logic_error( msg )
#define BOOST_TEST_DS_ASSERT( cond, msg ) if( cond ) {} else BOOST_TEST_DS_ERROR( msg )

#endif // BOOST_TEST_DATA_CONFIG_HPP_112611GER

