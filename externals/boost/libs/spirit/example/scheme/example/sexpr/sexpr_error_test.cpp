/*=============================================================================
    Copyright (c) 2001-2010 Joel de Guzman

    Distributed under the Boost Software License, Version 1.0. (See accompanying
    file LICENSE_1_0.txt or copy at http://www.boost.org/LICENSE_1_0.txt)
=============================================================================*/
#include <boost/config/warning_disable.hpp>

#include <input/sexpr.hpp>
#include <input/parse_sexpr_impl.hpp>
#include <iostream>
#include <fstream>

inline std::ostream& println(std::ostream& out, boost::spirit::utree const& val)
{
    out << val << std::endl;
    return out;
}

void test(std::string const& in, std::string const& file)
{
    boost::spirit::utree result;
    if (scheme::input::parse_sexpr(in, result, file))
    {
        std::cout << "success: ";
        println(std::cout, result);
        std::cout << std::endl;
    }
    else
    {
        std::cout << "parse error" << std::endl;
    }
}

///////////////////////////////////////////////////////////////////////////////
//  Main program
///////////////////////////////////////////////////////////////////////////////
int main()
{
    test("(abc def)", "ok.sexpr");
    test("(abc (123 456) def)", "ok.sexpr");
    test("(abc \n(\"a string\" 456) \ndef)", "ok.sexpr");
    test("(abc \n(\"a string\" 456 \ndef)", "missing close paren.sexpr");
    test("(abc \n(\"a string 456) \ndef)", "missing double quote.sexpr");
    test("(abc \n(\"a string\" 0xggg) \ndef)", "erronoeus hex.sexpr");
    test("(abc \n(\"a \\zstring\" 999) \ndef)", "erronoeus escape.sexpr");
    test("(abc \n(\"a \\uzstring\" 999) \ndef)", "erronoeus escape.sexpr");
    return 0;
}


