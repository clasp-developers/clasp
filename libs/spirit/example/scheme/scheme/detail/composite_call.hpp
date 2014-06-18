/*=============================================================================
    Copyright (c) 2001-2010 Joel de Guzman

    Distributed under the Boost Software License, Version 1.0. (See accompanying
    file LICENSE_1_0.txt or copy at http://www.boost.org/LICENSE_1_0.txt)
==============================================================================*/
#ifndef BOOST_PP_IS_ITERATING
#ifndef SCHEME_FUNCTION_COMPOSER_CALL_HPP
#define SCHEME_FUNCTION_COMPOSER_CALL_HPP

#include <boost/preprocessor/iterate.hpp>
#include <boost/preprocessor/repetition/enum_params.hpp>
#include <boost/preprocessor/repetition/enum_binary_params.hpp>
#include <boost/preprocessor/repetition/repeat.hpp>

#define SCHEME_PUSH_ELEMENT(z, n, _) elements.push_back(as_function(_##n));

#define BOOST_PP_ITERATION_PARAMS_1                                             \
    (3, (3, BOOST_PP_DEC(SCHEME_COMPOSITE_LIMIT),                               \
    "scheme/detail/composite_call.hpp"))
#include BOOST_PP_ITERATE()

#undef SCHEME_PUSH_ELEMENT

#endif

///////////////////////////////////////////////////////////////////////////////
//
//  Preprocessor vertical repetition code
//
///////////////////////////////////////////////////////////////////////////////
#else // defined(BOOST_PP_IS_ITERATING)

#define N BOOST_PP_ITERATION()

    template <BOOST_PP_ENUM_PARAMS(N, typename A)>
    function operator()(BOOST_PP_ENUM_BINARY_PARAMS(N, A, const& _)) const
    {
        actor_list elements;
        BOOST_PP_REPEAT(N, SCHEME_PUSH_ELEMENT, _);
        return derived()(elements);
    }

#undef N
#endif // defined(BOOST_PP_IS_ITERATING)


