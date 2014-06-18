/*=============================================================================
    Copyright (c) 2001-2010 Joel de Guzman

    Distributed under the Boost Software License, Version 1.0. (See accompanying
    file LICENSE_1_0.txt or copy at http://www.boost.org/LICENSE_1_0.txt)
=============================================================================*/
#include <boost/detail/lightweight_test.hpp>
#include <boost/config/warning_disable.hpp>

#include <input/sexpr.hpp>
#include <input/parse_sexpr_impl.hpp>
#include <scheme/compiler.hpp>

///////////////////////////////////////////////////////////////////////////////
//  Main program
///////////////////////////////////////////////////////////////////////////////
int main()
{
    using scheme::interpreter;
    using scheme::utree;

    {
        utree src = "(define n 123)";
        scheme::interpreter program(src);
        BOOST_TEST(program["n"]() == 123);
    }

    {
        utree src = "(define (factorial n) (if (<= n 0) 1 (* n (factorial (- n 1)))))";
        scheme::interpreter program(src);
        BOOST_TEST(program["factorial"](10) == 3628800);
    }

    {
        // test forward declaration (a scheme extension)
        utree src =
            "(define (dbl n))" // multiple forward declarations allowed
            "(define (dbl n))"
            "(define foo (dbl 10))"
            "(define (dbl n) (* n 2))"
            ;
        scheme::interpreter program(src);
        BOOST_TEST(program["foo"](10) == 20);
    }

    return boost::report_errors();
}


