/*=============================================================================
    Copyright (c) 2001-2010 Joel de Guzman

    Distributed under the Boost Software License, Version 1.0. (See accompanying
    file LICENSE_1_0.txt or copy at http://www.boost.org/LICENSE_1_0.txt)
=============================================================================*/
#include <boost/config/warning_disable.hpp>
#include <input/parse_sexpr_impl.hpp>
#include <scheme/compiler.hpp>
#include <iostream>
#include <fstream>

bool ignore_bom(std::ifstream& in, char const* filename)
{
    // Ignore the BOM marking the beginning of a UTF-8 file in Windows
    char c = in.peek();
    if (c == '\xef')
    {
        char s[3];
        in >> s[0] >> s[1] >> s[2];
        s[3] = '\0';
        if (s != std::string("\xef\xbb\xbf"))
        {
            std::cerr << "Error: Unexpected characters from input file: "
                << filename << std::endl;
            return false;
        }
    }
    return true;
}

///////////////////////////////////////////////////////////////////////////////
//  Main program
///////////////////////////////////////////////////////////////////////////////
int main()
{
    char const* filename = "factorial.scm";
    std::ifstream in(filename, std::ios_base::in);

    if (!in)
    {
        std::cerr << "Error: Could not open input file: "
            << filename << std::endl;
        return -1;
    }
    if (!ignore_bom(in, filename))
        return -1;

    using scheme::interpreter;
    using scheme::function;

    interpreter program(in);
    function factorial = program["factorial"];
    std::cout << factorial(10) << std::endl;

    return 0;
}


