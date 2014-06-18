//  Copyright (c) 2001-2010 Hartmut Kaiser
// 
//  Distributed under the Boost Software License, Version 1.0. (See accompanying 
//  file LICENSE_1_0.txt or copy at http://www.boost.org/LICENSE_1_0.txt)

#if !defined(BOOST_SPIRIT_GENERATE_QIEXPR)
#define BOOST_SPIRIT_GENERATE_QIEXPR

#include <output/sexpr.hpp>

namespace scheme { namespace output
{
    template <typename String>
    bool generate_qi_expr(utree& result, String& str);

    template <typename String>
    bool generate_qi_expr_list(utree& result, String& str);
}}

#endif


