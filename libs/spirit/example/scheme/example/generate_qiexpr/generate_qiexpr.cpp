//  Copyright (c) 2001-2010 Hartmut Kaiser
// 
//  Distributed under the Boost Software License, Version 1.0. (See accompanying 
//  file LICENSE_1_0.txt or copy at http://www.boost.org/LICENSE_1_0.txt)

// #define BOOST_SPIRIT_KARMA_DEBUG

#include <qi/generate_qiexpr.hpp>
#include <qi/generate_qiexpr_impl.hpp>

// explicit template instantiation for the function generate_qiexpr 
namespace scheme { namespace output
{
    template bool generate_qi_expr(utree& u, std::string& str);
    template bool generate_qi_expr_list(utree& u, std::string& str);
}}

namespace scheme
{
    std::ostream& operator<<(std::ostream& out, boost::spirit::nil const& x);
}

