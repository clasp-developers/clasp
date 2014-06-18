//  Copyright (c) 2001-2010 Hartmut Kaiser
//  Copyright (c) 2001-2010 Joel de Guzman
//
//  Distributed under the Boost Software License, Version 1.0. (See accompanying
//  file LICENSE_1_0.txt or copy at http://www.boost.org/LICENSE_1_0.txt)

#if !defined(BOOST_SPIRIT_PARSE_SEXPR)
#define BOOST_SPIRIT_PARSE_SEXPR

#include <input/sexpr.hpp>
#include <boost/utility/enable_if.hpp>
#include <boost/type_traits/is_base_of.hpp>
#include <iosfwd>
#include <string>

namespace scheme { namespace input
{
    ///////////////////////////////////////////////////////////////////////////
    template <typename Char>
    bool parse_sexpr(
        std::basic_istream<Char>& is,
        utree& result,
        std::string const& source_file = "");

    template <typename Char>
    bool parse_sexpr_list(
        std::basic_istream<Char>& is,
        utree& result,
        std::string const& source_file = "");

    ///////////////////////////////////////////////////////////////////////////
    template <typename Range>
    typename boost::disable_if<
        boost::is_base_of<std::ios_base, Range>, bool>::type
    parse_sexpr(
        Range const& rng,
        utree& result,
        std::string const& source_file = "");

    template <typename Range>
    typename boost::disable_if<
        boost::is_base_of<std::ios_base, Range>, bool>::type
    parse_sexpr_list(
        Range const& rng,
        utree& result,
        std::string const& source_file = "");

    ///////////////////////////////////////////////////////////////////////////
    bool parse_sexpr(
        utree const& in,
        utree& result,
        std::string const& source_file = "");

    bool parse_sexpr_list(
        utree const& in,
        utree& result,
        std::string const& source_file = "");
}}

#endif


