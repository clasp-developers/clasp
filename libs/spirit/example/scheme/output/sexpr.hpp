//  Copyright (c) 2001-2010 Hartmut Kaiser, Bryce Lelbach
//
//  Distributed under the Boost Software License, Version 1.0. (See accompanying
//  file LICENSE_1_0.txt or copy at http://www.boost.org/LICENSE_1_0.txt)

#if !defined(SCHEME_OUTPUT_SEXPR_MAR_8_2010_829AM)
#define SCHEME_OUTPUT_SEXPR_MAR_8_2010_829AM

#include <string>

#include <boost/cstdint.hpp>
#include <boost/mpl/bool.hpp>
#include <boost/spirit/include/support_utree.hpp>
#include <boost/spirit/include/karma.hpp>

///////////////////////////////////////////////////////////////////////////////
namespace scheme { namespace output
{
    using boost::spirit::karma::grammar;
    using boost::spirit::karma::space_type;
    using boost::spirit::karma::rule;
    using boost::spirit::karma::double_;
    using boost::spirit::karma::int_;
    using boost::spirit::karma::string;
    using boost::spirit::karma::bool_;
    using boost::spirit::karma::eps;
    using boost::spirit::karma::uint_generator;
    using boost::spirit::karma::hex;
    using boost::spirit::karma::right_align;

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

    template <typename OutputIterator>
    struct sexpr : grammar<OutputIterator, space_type, utree()>
    {
        sexpr() : sexpr::base_type(start)
        {
            uint_generator<unsigned char, 16> hex2;

            start     = double_
                      | int_
                      | bool_
                      | string_
                      | symbol
                      | byte_str
                      | list
                      | nil_
                      | ref_
                      ;

            list      = '(' << *start << ')';

            string_   = '"' << string << '"';
            symbol    = string;
            byte_str  = '#' << *right_align(2, '0')[hex2] << '#';
            nil_      = eps << "<nil>";
            ref_      = start;

            start.name("start");
            list.name("list");
            string_.name("string_");
            symbol.name("symbol");
            byte_str.name("byte_str");
            nil_.name("nil");
            ref_.name("ref_");
        }

        typedef boost::iterator_range<utree::const_iterator> utree_list;

        rule<OutputIterator, space_type, utree()> start;
        rule<OutputIterator, space_type, utree_list()> list;
        rule<OutputIterator, utf8_symbol_range_type()> symbol;
        rule<OutputIterator, utf8_string_range_type()> string_;
        rule<OutputIterator, binary_range_type()> byte_str;
        rule<OutputIterator, nil_type()> nil_;
        rule<OutputIterator, space_type, utree()> ref_;
    };
}}

#endif
