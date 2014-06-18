/*=============================================================================
    Copyright (c) 2001-2010 Joel de Guzman

    Distributed under the Boost Software License, Version 1.0. (See accompanying
    file LICENSE_1_0.txt or copy at http://www.boost.org/LICENSE_1_0.txt)
=============================================================================*/
#include <boost/detail/lightweight_test.hpp>
#include <boost/config/warning_disable.hpp>

#include <input/parse_sexpr_impl.hpp>
#include <scheme/compiler.hpp>
#include <iostream>
#include <fstream>

///////////////////////////////////////////////////////////////////////////////
//  Main program
///////////////////////////////////////////////////////////////////////////////
int main(int argc, char **argv)
{
    using scheme::utree;

    BOOST_TEST(argc > 1);
    char const* filename = filename = argv[1];
    std::ifstream in(filename, std::ios_base::in);

    BOOST_TEST(in);

    // Ignore the BOM marking the beginning of a UTF-8 file in Windows
    char c = in.peek();
    if (c == '\xef')
    {
        char s[3];
        in >> s[0] >> s[1] >> s[2];
        s[3] = '\0';
        BOOST_TEST(s != std::string("\xef\xbb\xbf"));
    }

    using scheme::interpreter;
    using scheme::_1;

    scheme::interpreter program(in);

    for (int i = 2; i < argc; ++i)
    {
        bool r = program[argv[i]]() == true;
        if (r)
            std::cout << "Success: " << argv[i] << std::endl;
        else
            std::cout << "Fail: " << argv[i] << std::endl;
        BOOST_TEST(r);
    }

    return boost::report_errors();
}


