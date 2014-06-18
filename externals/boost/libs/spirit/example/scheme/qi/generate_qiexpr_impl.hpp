//  Copyright (c) 2001-2010 Hartmut Kaiser
// 
//  Distributed under the Boost Software License, Version 1.0. (See accompanying 
//  file LICENSE_1_0.txt or copy at http://www.boost.org/LICENSE_1_0.txt)

#if !defined(BOOST_SPIRIT_GENERATE_QIEXPR_IMPL)
#define BOOST_SPIRIT_GENERATE_QIEXPR_IMPL

#include <iostream>
#include <boost/spirit/include/karma_generate.hpp>

#include <qi/qiexpr_generator.hpp>
#include <qi/generate_qiexpr.hpp>

namespace scheme { namespace output
{
    ///////////////////////////////////////////////////////////////////////////
    template <typename String>
    bool generate_qi_expr(utree& u, String& str)
    {
        using boost::spirit::karma::space;

        typedef std::back_insert_iterator<String> output_iterator_type;

        scheme::qi::qiexpr_generator<output_iterator_type> g;
        return generate_delimited(output_iterator_type(str), g, space, u);
    }

    ///////////////////////////////////////////////////////////////////////////
    template <typename String>
    bool generate_qi_expr_list(utree& u, String& str)
    {
        using boost::spirit::karma::space;

        typedef std::back_insert_iterator<String> output_iterator_type;

        scheme::qi::qiexpr_generator<output_iterator_type> g;
        return generate_delimited(output_iterator_type(str), g.grammar_, space, u);
    }
}}

#endif


