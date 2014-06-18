//  Copyright (c) 2001-2010 Hartmut Kaiser
//
//  Distributed under the Boost Software License, Version 1.0. (See accompanying
//  file LICENSE_1_0.txt or copy at http://www.boost.org/LICENSE_1_0.txt)

#if !defined(BOOST_SPIRIT_QIEXPR_PARSER)
#define BOOST_SPIRIT_QIEXPR_PARSER

#include <string>

#include <boost/cstdint.hpp>
#include <boost/detail/iterator.hpp>
#include <boost/spirit/include/qi.hpp>
#include <boost/spirit/include/support_utree.hpp>
#include <boost/spirit/include/phoenix_core.hpp>
#include <boost/spirit/include/phoenix_stl.hpp>
#include <boost/spirit/include/phoenix_statement.hpp>
#include <boost/spirit/include/phoenix_operator.hpp>

#include <input/string.hpp>
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
    using boost::spirit::ascii::space;
    using boost::spirit::ascii::char_;
    using boost::spirit::qi::grammar;
    using boost::spirit::qi::rule;
    using boost::spirit::qi::symbols;
    using boost::spirit::qi::eol;
    using boost::spirit::qi::_val;
    using boost::spirit::qi::_1;
    using boost::spirit::qi::_2;
    using boost::spirit::qi::lexeme;
    using boost::phoenix::push_back;
    
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
    template <typename Iterator>
    struct qiexpr_white_space : grammar<Iterator>
    {
        qiexpr_white_space() : qiexpr_white_space::base_type(start)
        {
            start =
                    space                           // tab/space/cr/lf
                |   "//" >> *(char_ - eol) >> eol   // comments
                |   "/*" >> *(char_ - "*/") >> "*/"
                ;
        }

        rule<Iterator> start;
    };

    namespace detail
    {
        ///////////////////////////////////////////////////////////////////////
        // return true if the utree instance represents a list whose first
        // element is a symbol node equal to the second argument
        inline bool is_list_node(utree const& u, utf8_symbol const& symbol)
        {
            if (u.which() != utree_type::list_type)
                return false;
            return u.front() == symbol;
        }

        inline bool is_list_node(utree const& u, utree const& symbol)
        {
            if (u.which() != utree_type::list_type)
                return false;
            if (symbol.which() == utree_type::list_type)
                return u.front() == symbol.front();
            return u.front() == symbol;
        }

        ///////////////////////////////////////////////////////////////////////
        // ensure the given utree instance represents a list whose first
        // element is the symbol this function object has been constructed from
        struct make_list_node
        {
            template <typename T1, typename T2 = nil_type>
            struct result { typedef void type; };

            explicit make_list_node(char const* symbol_)
              : symbol(symbol_)
            {}

            // If called with one parameter the given node needs to be
            // converted into a list whose first element is the symbol.
            //
            // i.e:
            //   lit: ("abc") --> (lit "abc")
            void operator()(utree& u) const
            {
                u.push_front(symbol);
            }

            // If called with two parameters we ensure the given node is a
            // (new) list whose first element is the symbol and we append the
            // given element to that list.
            //
            // i.e.:
            //   >>: (char_), (char_ "abc")    --> (>> (char_) (char_ "abc"))
            //   >>: (>> (char_ "a")), (char_) --> (>> (char_ "a") (char_))
            void operator()(utree& val, utree const& element) const
            {
                if (!is_list_node(val, symbol)) {
                    utree u;
                    u.push_back(symbol);
                    if (val.which() != utree_type::nil_type)
                        u.push_back(val);
                    val = u;
                }
                val.push_back(element);
            }

            utf8_symbol symbol;
        };

        ///////////////////////////////////////////////////////////////////////
        struct make_directive_node
        {
            template <typename T1, typename T2, typename T3>
            struct result { typedef void type; };

            void operator()(utree& val, utree const& element, utree const& sym) const
            {
                if (!is_list_node(val, sym)) {
                    utree u;
                    u.push_back(sym);
                    if (val.which() != utree_type::nil_type)
                        u.push_back(val);
                    val = u;
                }
                val.push_back(element);
            }
        };

        ///////////////////////////////////////////////////////////////////////
        // this creates a scheme definition:
        //
        //  i.e. (define (_1) exp)
        struct make_define_node
        {
            template <typename T1, typename T2, typename T3>
            struct result { typedef void type; };

            explicit make_define_node() : define_("define") {}

            void operator()(utree& val, utree const& name, utree const& exp) const
            {
                val.push_back(define_);
                utree n;
                n.push_back(name);
                val.push_back(n);
                val.push_back(exp);
            }

            utf8_symbol define_;
        };
    }

    ///////////////////////////////////////////////////////////////////////////
    template <typename Iterator>
    struct qiexpr_parser
      : grammar<Iterator, qiexpr_white_space<Iterator>, utree()>
    {
        typedef typename boost::detail::iterator_traits<Iterator>::value_type
            char_type;

        qiexpr_parser() : qiexpr_parser::base_type(rhs)
        {
            namespace phoenix = boost::phoenix;
            typedef phoenix::function<detail::make_list_node> make_list_type;
            typedef phoenix::function<detail::make_directive_node> make_directive_type;
            typedef phoenix::function<detail::make_define_node> make_define_type;

            make_directive_type make_directive = detail::make_directive_node();

            make_define_type make_define = detail::make_define_node();

            make_list_type make_sequence = detail::make_list_node("qi:>>");
            make_list_type make_permutation = detail::make_list_node("qi:^");
            make_list_type make_alternative = detail::make_list_node("qi:|");

            make_list_type make_kleene = detail::make_list_node("qi:*");
            make_list_type make_plus = detail::make_list_node("qi:+");
            make_list_type make_optional = detail::make_list_node("qi:-");
            make_list_type make_and_pred = detail::make_list_node("qi:&");
            make_list_type make_not_pred = detail::make_list_node("qi:!");

            make_list_type make_literal = detail::make_list_node("qi:lit");

            // grammar definition
            grammar_ = +rule_
                ;

            // rule definition
            rule_ = 
                    (symbol >> '=' >> alternative)
                    [
                        make_define(_val, _1, _2)
                    ]
                ;

            // right hand side of a rule (any parser expression)
            rhs = -alternative;

            // A | B
            alternative =
                    permutation           [ _val = _1 ]
                >> *( "|" >> permutation  [ make_alternative(_val, _1) ] )
                ;

            // A ^ B
            permutation =
                    sequence              [ _val = _1 ]
                >> *( "^" >> sequence     [ make_permutation(_val, _1) ] )
                ;

            // A >> B
            sequence =
                    unary_term            [ _val = _1 ]
                >> *( ">>" >> unary_term  [ make_sequence(_val, _1) ] )
                ;

            // unary operators
            unary_term =
                    "*" >> unary_term     [ make_kleene(_val, _1) ]
                |   "+" >> unary_term     [ make_plus(_val, _1) ]
                |   "-" >> unary_term     [ make_optional(_val, _1) ]
                |   "&" >> unary_term     [ make_and_pred(_val, _1) ]
                |   "!" >> unary_term     [ make_not_pred(_val, _1) ]
                |   term                  [ _val = _1 ]
                ;

            // A, (A)
            term =
                    primitive
                |   directive
                |   '(' >> alternative >> ')'
                ;

            // any parser directive
            directive =
                    (directive0 >> '[' >> alternative >> ']')
                    [
                        make_directive(_val, _2, _1)
                    ]
                ;

            // any primitive parser
            primitive %=
                    primitive2 >> '(' >> literal >> ',' >> literal >> ')'
                |   primitive1 >> '(' >> literal >> ')'
                |   primitive0        // taking no parameter
                |   literal               [ make_literal(_val) ]
                ;

            // a literal (either 'x' or "abc")
            literal =
                    string_lit            [ phoenix::push_back(_val, _1) ]
                |   string_lit.char_lit   [ phoenix::push_back(_val, _1) ]
                ;

            std::string exclude = std::string(" ();\"\x01-\x1f\x7f") + '\0';
            symbol  = lexeme[+(~char_(exclude))];

            // fill the symbol tables with all known primitive parser names
            std::string name("qi:");
            for (char const* const* p = primitives0; *p; ++p)
            {
                utree u;
                u.push_back(utf8_symbol(name + *p));
                primitive0.add(*p, u);
            }

            for (char const* const* p = primitives1; *p; ++p)
            {
                utree u;
                u.push_back(utf8_symbol(name + *p));
                primitive1.add(*p, u);
            }

            for (char const* const* p = primitives2; *p; ++p)
            {
                utree u;
                u.push_back(utf8_symbol(name + *p));
                primitive2.add(*p, u);
            }

            for (char const* const* p = directives0; *p; ++p)
            {
                utree u = utree(utf8_symbol(name + *p));
                directive0.add(*p, u);
            }

            BOOST_SPIRIT_DEBUG_NODE(grammar_);
            BOOST_SPIRIT_DEBUG_NODE(rule_);
            BOOST_SPIRIT_DEBUG_NODE(rhs);
            BOOST_SPIRIT_DEBUG_NODE(directive);
            BOOST_SPIRIT_DEBUG_NODE(primitive);
            BOOST_SPIRIT_DEBUG_NODE(unary_term);
            BOOST_SPIRIT_DEBUG_NODE(term);
            BOOST_SPIRIT_DEBUG_NODE(literal);
            BOOST_SPIRIT_DEBUG_NODE(symbol);
            BOOST_SPIRIT_DEBUG_NODE(alternative);
            BOOST_SPIRIT_DEBUG_NODE(permutation);
            BOOST_SPIRIT_DEBUG_NODE(sequence);
        }

        typedef rule<Iterator, qiexpr_white_space<Iterator>, utree()> rule_type;

        rule_type grammar_, rule_;
        rule_type rhs, directive, primitive, unary_term, term, literal;
        rule_type alternative, permutation, sequence;
        rule<Iterator, utf8_symbol()> symbol;

        symbols<char_type, utree> directive0, directive1;
        symbols<char_type, utree> primitive0, primitive1, primitive2;
        scheme::input::string<Iterator> string_lit;
    };
}}

#endif
