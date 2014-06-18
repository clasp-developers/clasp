/*=============================================================================
    Copyright (c) 2001-2010 Joel de Guzman

    Distributed under the Boost Software License, Version 1.0. (See accompanying
    file LICENSE_1_0.txt or copy at http://www.boost.org/LICENSE_1_0.txt)
=============================================================================*/
#if !defined(BOOST_SPIRIT_SEXPR)
#define BOOST_SPIRIT_SEXPR

#include <string>

#include <boost/cstdint.hpp>
#include <boost/spirit/include/qi.hpp>
#include <boost/spirit/include/support_utree.hpp>
#include <boost/spirit/include/phoenix_core.hpp>
#include <boost/spirit/include/phoenix_container.hpp>
#include <boost/spirit/include/phoenix_statement.hpp>
#include <boost/spirit/include/phoenix_operator.hpp>
#include <boost/regex/pending/unicode_iterator.hpp>

#include <input/string.hpp>
#include <input/error_handler.hpp>

namespace scheme { namespace input
{
    using boost::spirit::standard::char_;
    using boost::spirit::standard::space;
    using boost::spirit::qi::grammar;
    using boost::spirit::qi::rule;
    using boost::spirit::qi::eol;
    using boost::spirit::qi::uint_parser;
    using boost::spirit::qi::real_parser;
    using boost::spirit::qi::strict_real_policies;
    using boost::spirit::qi::int_;
    using boost::spirit::qi::hex;
    using boost::spirit::qi::oct;
    using boost::spirit::qi::bool_;
    using boost::spirit::qi::no_case;
    using boost::spirit::qi::lexeme;
    using boost::spirit::qi::on_error;
    using boost::spirit::qi::fail;
    using boost::spirit::qi::_val;
    using boost::spirit::qi::_1;
    using boost::spirit::qi::_2;
    using boost::spirit::qi::_3;
    using boost::spirit::qi::_4;
    using boost::spirit::qi::locals;
    using boost::spirit::qi::raw;
    using boost::spirit::qi::eps;
    using boost::spirit::qi::omit;
    using boost::spirit::info;
    
    using boost::spirit::utree;
    using boost::spirit::utree_type;
    using boost::spirit::scope;
    using boost::spirit::shallow;
    using boost::spirit::stored_function;
    using boost::spirit::function_base;
    using boost::spirit::binary_string_type;
    using boost::spirit::utf8_symbol_type;
    using boost::spirit::utf8_string_type;
    using boost::spirit::binary_range_type;
    using boost::spirit::utf8_symbol_range_type;
    using boost::spirit::utf8_string_range_type;
    using boost::spirit::nil_type;

    typedef boost::uint32_t uchar; // a unicode code point

    template <typename Iterator>
    struct sexpr_white_space : grammar<Iterator>
    {
        sexpr_white_space() : sexpr_white_space::base_type(start)
        {
            start =
                    space                           // tab/space/cr/lf
                |   ';' >> *(char_ - eol) >> eol    // comments
                ;
        }

        rule<Iterator> start;
    };

    struct save_line_pos_
    {
        template <typename Utree, typename Range>
        struct result { typedef void type; };

        template <typename Range>
        void operator()(utree& ast, Range const& rng) const
        {
            int n = boost::spirit::get_line(rng.begin());
            BOOST_ASSERT(n <= (std::numeric_limits<short>::max)());
            ast.tag(n);
        }
    };

    boost::phoenix::function<save_line_pos_> const
        save_line_pos = save_line_pos_();

    template <typename Iterator,
        typename ErrorHandler = input::error_handler<Iterator> >
    struct sexpr : grammar<Iterator, sexpr_white_space<Iterator>, utree()>
    {
        sexpr(std::string const& source_file = "")
          : sexpr::base_type(start), error_handler(ErrorHandler(source_file))
        {
            real_parser<double, strict_real_policies<double> > strict_double;
            uint_parser<unsigned char, 16, 2, 2> hex2;

            start   = element.alias();
            element = atom | list;

            list    %= '('
                    > omit[raw[eps]         [save_line_pos(_val, _1)]]
                    > *element
                    > ')'
                    ;

            atom    = strict_double
                    | integer
                    | bool_
                    | string
                    | byte_str
                    | symbol
                    ;

            std::string exclude = std::string(" ();\"\x01-\x1f\x7f") + '\0';
            symbol  = lexeme[+(~char_(exclude))];

            integer = lexeme[no_case["0x"] > hex]
                    | lexeme['0' >> oct]
                    | int_
                    ;

            byte_str = lexeme['#' > +hex2 > '#'];

            start.name("sexpr");
            list.name("list");
            atom.name("atom");
            symbol.name("symbol");
            integer.name("integer");
            byte_str.name("byte_str");

            on_error<fail>(start, error_handler(_1, _2, _3, _4));
        }

        rule<Iterator, sexpr_white_space<Iterator>, utree()>
            start, element, list;
        rule<Iterator, int()> integer;
        rule<Iterator, utree()> atom;
        rule<Iterator, utf8_symbol_type()> symbol;
        rule<Iterator, binary_string_type()> byte_str;
        scheme::input::string<Iterator> string;

        function<ErrorHandler> const error_handler;
    };
}}

#endif
