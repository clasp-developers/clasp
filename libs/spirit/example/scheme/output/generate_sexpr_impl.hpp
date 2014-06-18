//  Copyright (c) 2001-2010 Hartmut Kaiser, Bryce Lelbach
// 
//  Distributed under the Boost Software License, Version 1.0. (See accompanying 
//  file LICENSE_1_0.txt or copy at http://www.boost.org/LICENSE_1_0.txt)

#if !defined(SCHEME_OUTPUT_GENERATE_SEXPR_IMPL_MAR_29_2010_1210PM)
#define SCHEME_OUTPUT_GENERATE_SEXPR_MAR_IMPL_29_2010_1210PM

#include <boost/spirit/include/karma_generate.hpp>
#include <boost/spirit/include/karma_char.hpp>
#include <boost/spirit/include/karma_list.hpp>
#include <boost/spirit/include/support_ostream_iterator.hpp>

#include <output/sexpr.hpp>
#include <output/generate_sexpr.hpp>

namespace scheme { namespace output
{
    ///////////////////////////////////////////////////////////////////////////
    template <typename Char>
    bool generate_sexpr(std::basic_ostream<Char>& os, utree const& tree)
    {
        typedef boost::spirit::ostream_iterator output_iterator_type;

        using boost::spirit::karma::space;
        using boost::spirit::karma::generate_delimited;

        scheme::output::sexpr<output_iterator_type> g;

        return generate_delimited(output_iterator_type(os), g, space, tree);
    }

    ///////////////////////////////////////////////////////////////////////////
    template <typename Char>
    bool generate_sexpr_list(std::basic_ostream<Char>& os, utree const& tree)
    {
        typedef boost::spirit::ostream_iterator output_iterator_type;

        using boost::spirit::karma::space;
        using boost::spirit::karma::eol;
        using boost::spirit::karma::generate_delimited;

        scheme::output::sexpr<output_iterator_type> g;

        return generate_delimited(output_iterator_type(os), g % eol, space, tree);
    }

    ///////////////////////////////////////////////////////////////////////////
    template <typename Char>
    bool generate_sexpr(std::basic_string<Char>& os, utree const& tree)
    {
        typedef std::basic_string<Char> string_type;
        typedef std::back_insert_iterator<string_type> output_iterator_type;

        using boost::spirit::karma::space;
        using boost::spirit::karma::generate_delimited;

        scheme::output::sexpr<output_iterator_type> g;
        return generate_delimited(output_iterator_type(os), g, space, tree);
    }

    ///////////////////////////////////////////////////////////////////////////
    template <typename Char>
    bool generate_sexpr_list(std::basic_string<Char>& os, utree const& tree)
    {
        typedef std::basic_string<Char> string_type;
        typedef std::back_insert_iterator<string_type> output_iterator_type;

        using boost::spirit::karma::space;
        using boost::spirit::karma::eol;
        using boost::spirit::karma::generate_delimited;

        scheme::output::sexpr<output_iterator_type> g;

        return generate_delimited(output_iterator_type(os), g % eol, space, tree);
    }
}}

#endif


