/*=============================================================================
    Copyright (c) 2001-2010 Joel de Guzman

    Distributed under the Boost Software License, Version 1.0. (See accompanying
    file LICENSE_1_0.txt or copy at http://www.boost.org/LICENSE_1_0.txt)
=============================================================================*/
#include <boost/config/warning_disable.hpp>
#include <input/parse_sexpr_impl.hpp>
#include <input/sexpr.hpp>
#include <input/parse_sexpr_impl.hpp>
#include <scheme/compiler.hpp>

///////////////////////////////////////////////////////////////////////////////
//  Main program
///////////////////////////////////////////////////////////////////////////////
int main()
{
    using scheme::interpreter;
    using scheme::function;
    using boost::spirit::utree;

    utree src =
        "(define (factorial n) "
            "(if (<= n 0) 1 (* n (factorial (- n 1)))))";

    interpreter program(src);
    function factorial = program["factorial"];
    std::cout << factorial(10) << std::endl;

    return 0;
}


