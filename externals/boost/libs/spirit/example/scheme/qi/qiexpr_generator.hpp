//  Copyright (c) 2001-2010 Hartmut Kaiser
// 
//  Distributed under the Boost Software License, Version 1.0. (See accompanying 
//  file LICENSE_1_0.txt or copy at http://www.boost.org/LICENSE_1_0.txt)

#if !defined(BOOST_SPIRIT_QIEXPR_GENERATOR)
#define BOOST_SPIRIT_QIEXPR_GENERATOR

#include <string>

#include <boost/cstdint.hpp>
#include <boost/spirit/include/karma.hpp>
#include <boost/spirit/include/support_utree.hpp>
#include <boost/spirit/include/phoenix.hpp>

#include <qi/component_names.hpp>

///////////////////////////////////////////////////////////////////////////////
namespace boost { namespace spirit { namespace traits
{
    template <typename Out>
    void print_attribute(Out& out, utree const& val);
}}}

///////////////////////////////////////////////////////////////////////////////
namespace scheme { namespace qi
{
    using boost::spirit::karma::grammar;
    using boost::spirit::karma::rule;
    using boost::spirit::karma::space_type;
    using boost::spirit::karma::symbols;
    
    using boost::spirit::utree;
    using boost::spirit::utree_type;
    using boost::spirit::scope;
    using boost::spirit::shallow;
    using boost::spirit::stored_function;
    using boost::spirit::function_base;
    using boost::spirit::binary_string;
    using boost::spirit::utf8_symbol;
    using boost::spirit::utf8_string;
    using boost::spirit::binary_range;
    using boost::spirit::utf8_symbol_range;
    using boost::spirit::utf8_string_range;
    using boost::spirit::nil_type;

    ///////////////////////////////////////////////////////////////////////////
    namespace traits
    {
        template <typename Count>
        struct deref_spec 
          : boost::spirit::result_of::terminal<boost::spirit::tag::repeat(Count)>
        {};
    }

    template <typename Count>
    inline typename traits::deref_spec<Count>::type
    deref_spec(Count const& count)
    {
        using boost::spirit::karma::repeat;
        return repeat(count);
    }

    typedef traits::deref_spec<int>::type deref_tag_type;
    deref_tag_type const deref = deref_spec(1);

    ///////////////////////////////////////////////////////////////////////////
    template <typename OutputIterator>
    struct qiexpr_generator : grammar<OutputIterator, space_type, utree()>
    {
        qiexpr_generator() : qiexpr_generator::base_type(start)
        {
            namespace phoenix = boost::phoenix;

            using boost::spirit::karma::eps;
            using boost::spirit::karma::ascii::string;
            using boost::spirit::karma::omit;
            using boost::spirit::karma::_r1;
            using boost::spirit::karma::strict;
            using boost::spirit::karma::eol;
            using boost::phoenix::ref;

            start = 
                    nil_ 
                |   rule_
                ;

            grammar_ =
                    nil_ 
                |   rule_ % eol
                ;

            rule_ =
                    &symbol(ref("define")) 
                        << deref[rule_name] << '=' << deref[alternative]
                |   alternative
                ;

            alternative = 
                    &symbol(ref("qi:|")) 
                        << '(' << strict[permutation % '|'] << ')'
                |   permutation
                ;

            permutation = 
                    &symbol(ref("qi:^")) 
                        << '(' << strict[sequence % '^'] << ')'
                |   sequence
                ;

            sequence = 
                    &symbol(ref("qi:>>")) 
                        << '(' << strict[term % ">>"] << ')'
                |   term
                ;

            term = strict[
                        unary << '(' << deref[alternative] << ')'
                    |   primitive2 << '(' << literal << ',' << literal << ')'
                    |   primitive1 << '(' << literal << ')'
                    |   primitive0_rule
                    |   directive0 << '[' << deref[alternative] << ']'
                    |   alternative_rule 
                ];

            primitive0_rule = strict[deref[primitive0]];
            alternative_rule = alternative;

            rule_name = strict[deref[any_symbol]];

            any_symbol = string;
            symbol = string(_r1);
            literal = '"' << string << '"';
            nil_ = eps;

            // fill the symbol tables with all known primitive parser names
            std::string name("qi:");
            for (char const* const* p = primitives0; *p; ++p)
                primitive0.add(utf8_symbol(name + *p));

            for (char const* const* p = primitives1; *p; ++p)
                primitive1.add(utf8_symbol(name + *p));

            for (char const* const* p = primitives2; *p; ++p)
                primitive2.add(utf8_symbol(name + *p));

            for (char const* const* p = unary_names; *p; ++p)
                unary.add(utf8_symbol(name + *p));

            for (char const* const* p = directives0; *p; ++p)
                directive0.add(utf8_symbol(name + *p));

            BOOST_SPIRIT_DEBUG_NODE(start);
            BOOST_SPIRIT_DEBUG_NODE(grammar_);
            BOOST_SPIRIT_DEBUG_NODE(rule_);
            BOOST_SPIRIT_DEBUG_NODE(rule_name);
            BOOST_SPIRIT_DEBUG_NODE(alternative);
            BOOST_SPIRIT_DEBUG_NODE(permutation);
            BOOST_SPIRIT_DEBUG_NODE(sequence);
            BOOST_SPIRIT_DEBUG_NODE(term);
            BOOST_SPIRIT_DEBUG_NODE(nil_);
            BOOST_SPIRIT_DEBUG_NODE(literal);
            BOOST_SPIRIT_DEBUG_NODE(symbol);
            BOOST_SPIRIT_DEBUG_NODE(any_symbol);
            BOOST_SPIRIT_DEBUG_NODE(primitive0_rule);
            BOOST_SPIRIT_DEBUG_NODE(alternative_rule);
        }

        typedef rule<OutputIterator, space_type, utree()> delimiting_rule_type;

        delimiting_rule_type start, alternative, permutation, sequence, term;
        delimiting_rule_type grammar_, rule_;
        delimiting_rule_type rule_name, primitive0_rule, alternative_rule;
        rule<OutputIterator, nil_type()> nil_;
        rule<OutputIterator, utf8_string()> literal;
        rule<OutputIterator, utf8_symbol(std::string)> symbol;
        rule<OutputIterator, utf8_symbol()> any_symbol;

        symbols<utf8_symbol> unary, directive0;
        symbols<utf8_symbol> primitive0, primitive1, primitive2;
    };
}}

#endif



