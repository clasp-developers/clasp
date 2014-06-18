/*=============================================================================
    Copyright (c) 2001-2011 Joel de Guzman

    Distributed under the Boost Software License, Version 1.0. (See accompanying
    file LICENSE_1_0.txt or copy at http://www.boost.org/LICENSE_1_0.txt)
=============================================================================*/
#include <boost/config/warning_disable.hpp>

#include <input/sexpr.hpp>
#include <input/parse_sexpr_impl.hpp>
#include <scheme/compiler.hpp>
#include <fstream>

int check_file(std::ifstream& in, char const* filename)
{
    if (!in)
    {
        std::cerr << filename << " not found" << std::endl;
        return -1;
    }

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
            return -1;
        }
    }
    return 0;
}

///////////////////////////////////////////////////////////////////////////////
//  Main program
///////////////////////////////////////////////////////////////////////////////
int main(int argc, char **argv)
{
    char const* filename = filename = argv[1];
    std::ifstream in(filename, std::ios_base::in);
    if (check_file(in, filename) != 0)
        return -1;

    scheme::interpreter program(in, filename);
    scheme::function main_ = program["main"];
    if (!main_.empty())
        main_(); // call main
    return 0;
}


