//  Copyright (c) 2001-2011 Hartmut Kaiser
//
//  Distributed under the Boost Software License, Version 1.0. (See accompanying
//  file LICENSE_1_0.txt or copy at http://www.boost.org/LICENSE_1_0.txt)

#include <boost/config/warning_disable.hpp>

#define BOOST_SPIRIT_UNICODE

#include <iostream>
#include <fstream>
#include <iterator>

#include <input/parse_sexpr_impl.hpp>
#include <output/generate_sexpr_impl.hpp>

namespace client
{
    bool parse_sexpr_from_file(char const* filename, boost::spirit::utree& result)
    {
        std::ifstream in(filename, std::ios_base::in);

        if (!in)
        {
            std::cerr << "Error: Could not open input file: "
                << filename << std::endl;
            exit(-1);
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
                exit(-1);
            }
        }

        return scheme::input::parse_sexpr(in, result);
    }

    bool generate_sexpr_to_file(boost::spirit::utree const& tree, char const* filename)
    {
        std::ofstream out(filename);

        if (!out)
        {
            std::cerr << "Error: Could not open output file: "
                << filename << std::endl;
            exit(-1);
        }

        return scheme::output::generate_sexpr(out, tree);
    }
}

int main(int argc, char **argv)
{
    char const* filename_in = NULL;
    if (argc > 1)
    {
        filename_in = argv[1];
    }
    else
    {
        std::cerr << "Error: No input file provided." << std::endl;
        return -1;
    }

    char const* filename_out = NULL;
    if (argc > 2)
    {
        filename_out = argv[2];
    }
    else
    {
        std::cerr << "Error: No output file provided." << std::endl;
        return -1;
    }

    boost::spirit::utree result(boost::spirit::nil);
//     if (client::parse_sexpr_from_file(filename_in, result))
    {
        if (client::generate_sexpr_to_file(result, filename_out))
        {
            std::cout << "success!" << std::endl;
        }
        else
        {
            std::cout << "generate error" << std::endl;
        }
    }
//     else
//     {
//         std::cout << "parse error" << std::endl;
//     }

    return 0;
}
