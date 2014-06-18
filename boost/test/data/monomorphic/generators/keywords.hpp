//  (C) Copyright Gennadiy Rozental 2011-2012.
//  Distributed under the Boost Software License, Version 1.0.
//  (See accompanying file LICENSE_1_0.txt or copy at 
//  http://www.boost.org/LICENSE_1_0.txt)

//  See http://www.boost.org/libs/test for the library home page.
//
//  File        : $RCSfile$
//
//  Version     : $Revision$
//
//  Description : keywords used in generator interfaces
// ***************************************************************************

#ifndef BOOST_TEST_DATA_MONOMORPHIC_GENERATORS_KEYWORDS_HPP_101512GER
#define BOOST_TEST_DATA_MONOMORPHIC_GENERATORS_KEYWORDS_HPP_101512GER

// Boost.Test
#include <boost/test/data/config.hpp>
#include <boost/test/utils/named_params.hpp>

#include <boost/test/detail/suppress_warnings.hpp>

//____________________________________________________________________________//

namespace boost {
namespace unit_test {
namespace data {

namespace {
nfp::keyword<struct begin_t >   begin;

//check_is_close_t const& check_is_close = unit_test::ut_detail::static_constant<check_is_close_t>::value;

nfp::keyword<struct end_t >     end;
nfp::keyword<struct step_t >    step;
} // local namespace

} // namespace data
} // namespace unit_test
} // namespace boost

#include <boost/test/detail/enable_warnings.hpp>

#endif // BOOST_TEST_DATA_MONOMORPHIC_GENERATORS_KEYWORDS_HPP_101512GER
