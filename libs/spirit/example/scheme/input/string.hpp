/*=============================================================================
    Copyright (c) 2001-2010 Joel de Guzman

    Distributed under the Boost Software License, Version 1.0. (See accompanying
    file LICENSE_1_0.txt or copy at http://www.boost.org/LICENSE_1_0.txt)
=============================================================================*/
#if !defined(BOOST_SPIRIT_SEXPR_STRING)
#define BOOST_SPIRIT_SEXPR_STRING

#include <string>

#include <boost/cstdint.hpp>
#include <boost/spirit/include/qi.hpp>
#include <boost/spirit/include/phoenix_core.hpp>
#include <boost/spirit/include/phoenix_container.hpp>
#include <boost/spirit/include/phoenix_statement.hpp>
#include <boost/spirit/include/phoenix_operator.hpp>
#include <boost/regex/pending/unicode_iterator.hpp>

namespace scheme { namespace input
{
    using boost::spirit::standard::char_;
    using boost::spirit::qi::grammar;
    using boost::spirit::qi::rule;
    using boost::spirit::qi::_val;
    using boost::spirit::qi::_r1;
    using boost::spirit::qi::_1;
    using boost::spirit::qi::uint_parser;
    using boost::phoenix::function;

    typedef boost::uint32_t uchar; // a unicode code point

    namespace detail
    {
        struct push_utf8
        {
            template <typename S, typename C>
            struct result { typedef void type; };

            void operator()(std::string& utf8, uchar code_point) const
            {
                typedef std::back_insert_iterator<std::string> insert_iter;
                insert_iter out_iter(utf8);
                boost::utf8_output_iterator<insert_iter> utf8_iter(out_iter);
                *utf8_iter++ = code_point;
            }
        };

        struct push_esc
        {
            template <typename S, typename C>
            struct result { typedef void type; };

            void operator()(std::string& utf8, uchar c) const
            {
                switch (c)
                {
                    case 'b': utf8 += '\b';     break;
                    case 't': utf8 += '\t';     break;
                    case 'n': utf8 += '\n';     break;
                    case 'f': utf8 += '\f';     break;
                    case 'r': utf8 += '\r';     break;
                    case '"': utf8 += '"';      break;
                    case '\\': utf8 += '\\';    break;
                }
            }
        };
    }

    template <typename Iterator>
    struct string : grammar<Iterator, std::string()>
    {
        string() : string::base_type(start)
        {
            uint_parser<uchar, 16, 4, 4> hex4;
            uint_parser<uchar, 16, 8, 8> hex8;
            function<detail::push_utf8> push_utf8;
            function<detail::push_esc> push_esc;

            char_esc
                = '\\'
                > (   ('u' > hex4)                  [push_utf8(_r1, _1)]
                  |   ('U' > hex8)                  [push_utf8(_r1, _1)]
                  |   char_("btnfr\\\"'")           [push_esc(_r1, _1)]
                  )
                ;
            
            char_lit
                = '\''
                > (char_esc(_val) | (~char_('\''))  [_val += _1])
                > '\''
                ;

            start
                = '"'
                > *(char_esc(_val) | (~char_('"'))  [_val += _1])
                > '"'
                ;

            char_esc.name("char_esc");
            char_lit.name("char_lit");
            start.name("string");
        }

        rule<Iterator, void(std::string&)> char_esc;
        rule<Iterator, std::string()> char_lit;
        rule<Iterator, std::string()> start;
    };
}}

#endif
