/*=============================================================================
    Copyright (c) 2001-2010 Joel de Guzman

    Distributed under the Boost Software License, Version 1.0. (See accompanying
    file LICENSE_1_0.txt or copy at http://www.boost.org/LICENSE_1_0.txt)
=============================================================================*/
#include <boost/detail/lightweight_test.hpp>
#include <boost/config/warning_disable.hpp>

#include <input/sexpr.hpp>
#include <input/parse_sexpr_impl.hpp>
#include <scheme/compiler.hpp>
#include <boost/spirit/include/qi.hpp>

#include <iostream>
#include <fstream>
#include <strstream>
#include <map>

#include "../../../../test/qi/test.hpp"

#define SCHEME_QI_COMPILER_LIMIT 20

namespace scheme { namespace qi
{
    ///////////////////////////////////////////////////////////////////////////
    // parser compiler
    ///////////////////////////////////////////////////////////////////////////
    namespace qi = boost::spirit::qi;
    namespace spirit = boost::spirit;

    typedef qi::rule<char const*> skipper_type;
    typedef qi::rule<char const*, skipper_type> rule_type;

    ///////////////////////////////////////////////////////////////////////////
    // All rule are stored here. Rules are held in the utree by its id;
    // i.e. its index in the vector.
    ///////////////////////////////////////////////////////////////////////////
    template <typename Rule>
    class rule_fragments
    {
    public:

        rule_fragments()
          : index(0)
        {
        };

        int new_rule()
        {
            rules[index];
            return index++;
        }

        template <typename Expr>
        void define_rule(int id, Expr const& expr)
        {
            rules[id] = expr;
            std::stringstream str;
            str << qi::what(expr);
            rules[id].name(str.str());
        }

        Rule const& operator[](int id) const
        {
            typename std::map<int, Rule>::const_iterator
                iter = rules.find(id);
            BOOST_ASSERT(iter != rules.end());
            return iter->second;
        }

        Rule const& operator[](utree const& id) const
        {
            return (*this)[id.get<int>()];
        }

    private:

        int index;
        std::map<int, Rule> rules;
    };

    ///////////////////////////////////////////////////////////////////////////
    // Composes primitive parsers held by index. Handles the compilation of
    // primitive (nullary) parsers such as int_, double_, alpha, space and all
    // those that require no arguments.
    ///////////////////////////////////////////////////////////////////////////
    struct primitive_parser_composite : composite<primitive_parser_composite>
    {
        int id;
        primitive_parser_composite(int id)
          : id(id)
        {
        }

        function compose(actor_list const& elements) const
        {
            return val(id);
        }
    };

    template <typename Fragments, typename Expr>
    inline primitive_parser_composite
    make_primitive_parser_composite(Fragments& fragments, Expr const& expr)
    {
        int id = fragments.new_rule();
        fragments.define_rule(id, expr);
        return primitive_parser_composite(id);
    }

    ///////////////////////////////////////////////////////////////////////////
    // Handles the compilation of char_
    ///////////////////////////////////////////////////////////////////////////
    template <typename Fragments>
    struct char_function : actor<char_function<Fragments> >
    {
        mutable int id;
        function a;
        function b;
        Fragments& fragments;
        char_function(
            Fragments& fragments, function const& a, function const& b)
          : id(-1), a(a), b(b), fragments(fragments)
        {
        }

        void define() const
        {
            // char_
            fragments.define_rule(id, qi::char_);
        }

        void define(utree const& a) const
        {
            // $$$ use exceptions here $$$.
            BOOST_ASSERT(a.which() == utree_type::string_type);

            utf8_string_range a_ = a.get<utf8_string_range>();
            if (a_.size() == 1)
            {
                // char_('x')
                fragments.define_rule(id, qi::char_(a_[0]));
            }
            else
            {
                // char_("some-regex")
                fragments.define_rule(id,
                    qi::char_(std::string(a_.begin(), a_.end())));
            }
        }

        void define(utree const& a, utree const& b) const
        {
            // $$$ use exceptions here $$$.
            BOOST_ASSERT(a.which() == utree_type::string_type);
            BOOST_ASSERT(b.which() == utree_type::string_type);

            utf8_string_range a_ = a.get<utf8_string_range>();
            utf8_string_range b_ = b.get<utf8_string_range>();
            // $$$ use exceptions here $$$.
            BOOST_ASSERT(a_.size() == 1);
            BOOST_ASSERT(b_.size() == 1);

            // char_('x', 'y')
            fragments.define_rule(id, qi::char_(a_[0], b_[0]));
        }

        utree eval(scope const& env) const
        {
            if (id != -1)
                return id;
            id = fragments.new_rule();

            if (a.empty())
                define();
            else if (b.empty())
                define(a(env));
            else
                define(a(env), b(env));

            return id;
        }
    };

    template <typename Fragments>
    struct char_composite
      : composite<char_composite<Fragments> >
    {
        Fragments& fragments;
        char_composite(Fragments& fragments)
          : fragments(fragments) {}

        function compose(actor_list const& elements) const
        {
            typedef char_function<Fragments> function_type;
            actor_list::const_iterator i = elements.begin();

            function empty;
            function const& a = (i == elements.end())? empty : *i++;
            function const& b = (i == elements.end())? empty : *i;
            return function(function_type(fragments, a, b));
        }
    };

    ///////////////////////////////////////////////////////////////////////////
    // Handles the compilation of kleene *a
    ///////////////////////////////////////////////////////////////////////////
    template <typename Fragments>
    struct kleene_function : actor<kleene_function<Fragments> >
    {
        mutable int id;
        function a;
        Fragments& fragments;
        kleene_function(
            Fragments& fragments, function const& a)
          : id(-1), a(a), fragments(fragments)
        {
        }

        void define(utree const& a) const
        {
            fragments.define_rule(id, *fragments[a]); // *a
        }

        utree eval(scope const& env) const
        {
            if (id != -1)
                return id;
            id = fragments.new_rule();
            define(a(env));
            return id;
        }
    };

    template <typename Fragments>
    struct kleene_composite
      : composite<kleene_composite<Fragments> >
    {
        Fragments& fragments;
        kleene_composite(Fragments& fragments)
          : fragments(fragments) {}

        function compose(actor_list const& elements) const
        {
            typedef kleene_function<Fragments> function_type;
            return function(function_type(fragments, elements.front()));
        }
    };

    ///////////////////////////////////////////////////////////////////////////
    // Handles the compilation of difference a - b
    ///////////////////////////////////////////////////////////////////////////
    template <typename Fragments>
    struct difference_function : actor<difference_function<Fragments> >
    {
        mutable int id;
        function a;
        function b;
        Fragments& fragments;
        difference_function(
            Fragments& fragments, function const& a, function const& b)
          : id(-1), a(a), b(b), fragments(fragments)
        {
        }

        void define(utree const& a, utree const& b) const
        {
            fragments.define_rule(id,
                fragments[a] - fragments[b]); // a - b
        }

        utree eval(scope const& env) const
        {
            if (id != -1)
                return id;
            id = fragments.new_rule();
            define(a(env), b(env));
            return id;
        }
    };

    template <typename Fragments>
    struct difference_composite
      : composite<difference_composite<Fragments> >
    {
        Fragments& fragments;
        difference_composite(Fragments& fragments)
          : fragments(fragments) {}

        function compose(actor_list const& elements) const
        {
            typedef difference_function<Fragments> function_type;
            actor_list::const_iterator i = elements.begin();

            function const& a = *i++;
            function const& b = *i;
            return function(function_type(fragments, a, b));
        }
    };

    ///////////////////////////////////////////////////////////////////////////
    // Handles the compilation of sequence a >> b
    ///////////////////////////////////////////////////////////////////////////
    template <typename Fragments>
    struct sequence_function : actor<sequence_function<Fragments> >
    {
        mutable int id;
        actor_list elements;
        Fragments& fragments;
        sequence_function(
            Fragments& fragments, actor_list const& elements)
          : id(-1), elements(elements), fragments(fragments)
        {
        }

        void define(utree const& a, utree const& b) const
        {
            // a >> b
            fragments.define_rule(id,
                fragments[a] >> fragments[b]);
        }

        void define(utree const& a, utree const& b, utree const& c) const
        {
            // a >> b >> c
            fragments.define_rule(id,
                fragments[a] >> fragments[b] >> fragments[c]);
        }

        void define(utree const& a, utree const& b, utree const& c,
            utree const& d) const
        {
            // a >> b >> c >> d
            fragments.define_rule(id,
                fragments[a] >> fragments[b] >> fragments[c] >>
                fragments[d]);
        }

        void define(utree const& a, utree const& b, utree const& c,
            utree const& d, utree const& e) const
        {
            // a >> b >> c >> d >> e
            fragments.define_rule(id,
                fragments[a] >> fragments[b] >> fragments[c] >>
                fragments[d] >> fragments[e]);
        }

        utree eval(scope const& env) const
        {
            if (id != -1)
                return id;
            id = fragments.new_rule();

            actor_list::const_iterator i = elements.begin();
            switch (elements.size())
            {
                case 2:
                {
                    function const& a = *i++;
                    function const& b = *i;
                    define(a(env), b(env));
                    break;
                }
                case 3:
                {
                    function const& a = *i++;
                    function const& b = *i++;
                    function const& c = *i;
                    define(a(env), b(env), c(env));
                    break;
                }
                case 4:
                {
                    function const& a = *i++;
                    function const& b = *i++;
                    function const& c = *i++;
                    function const& d = *i;
                    define(a(env), b(env), c(env), d(env));
                    break;
                }
                case 5:
                {
                    function const& a = *i++;
                    function const& b = *i++;
                    function const& c = *i++;
                    function const& d = *i++;
                    function const& e = *i;
                    define(a(env), b(env), c(env), d(env), e(env));
                    break;
                }

                // $$$ Use Boost PP using SCHEME_QI_COMPILER_LIMIT $$$
            }
            return id;
        }
    };

    template <typename Fragments>
    struct sequence_composite
      : composite<sequence_composite<Fragments> >
    {
        Fragments& fragments;
        sequence_composite(Fragments& fragments)
          : fragments(fragments) {}

        function compose(actor_list const& elements) const
        {
            typedef sequence_function<Fragments> function_type;
            return function(function_type(fragments, elements));
        }
    };

    ///////////////////////////////////////////////////////////////////////////
    // Handles the compilation of alternatives a | b
    ///////////////////////////////////////////////////////////////////////////
    template <typename Fragments>
    struct alternative_function : actor<alternative_function<Fragments> >
    {
        mutable int id;
        actor_list elements;
        Fragments& fragments;
        alternative_function(
            Fragments& fragments, actor_list const& elements)
          : id(-1), elements(elements), fragments(fragments)
        {
        }

        void define(utree const& a, utree const& b) const
        {
            // a | b
            fragments.define_rule(id,
                fragments[a] | fragments[b]);
        }

        void define(utree const& a, utree const& b, utree const& c) const
        {
            // a | b | c
            fragments.define_rule(id,
                fragments[a] | fragments[b] | fragments[c]);
        }

        void define(utree const& a, utree const& b, utree const& c,
            utree const& d) const
        {
            // a | b | c | d
            fragments.define_rule(id,
                fragments[a] | fragments[b] | fragments[c] |
                fragments[d]);
        }

        void define(utree const& a, utree const& b, utree const& c,
            utree const& d, utree const& e) const
        {
            // a | b | c | d | e
            fragments.define_rule(id,
                fragments[a] | fragments[b] | fragments[c] |
                fragments[d] | fragments[e]);
        }

        utree eval(scope const& env) const
        {
            if (id != -1)
                return id;
            id = fragments.new_rule();

            actor_list::const_iterator i = elements.begin();
            switch (elements.size())
            {
                case 2:
                {
                    function const& a = *i++;
                    function const& b = *i;
                    define(a(env), b(env));
                    break;
                }
                case 3:
                {
                    function const& a = *i++;
                    function const& b = *i++;
                    function const& c = *i;
                    define(a(env), b(env), c(env));
                    break;
                }
                case 4:
                {
                    function const& a = *i++;
                    function const& b = *i++;
                    function const& c = *i++;
                    function const& d = *i;
                    define(a(env), b(env), c(env), d(env));
                    break;
                }
                case 5:
                {
                    function const& a = *i++;
                    function const& b = *i++;
                    function const& c = *i++;
                    function const& d = *i++;
                    function const& e = *i;
                    define(a(env), b(env), c(env), d(env), e(env));
                    break;
                }

                // $$$ Use Boost PP using SCHEME_QI_COMPILER_LIMIT $$$
            }
            return id;
        }
    };

    template <typename Fragments>
    struct alternative_composite
      : composite<alternative_composite<Fragments> >
    {
        Fragments& fragments;
        alternative_composite(Fragments& fragments)
          : fragments(fragments) {}

        function compose(actor_list const& elements) const
        {
            typedef alternative_function<Fragments> function_type;
            return function(function_type(fragments, elements));
        }
    };

    ///////////////////////////////////////////////////////////////////////////
    // Build our scheme compiler environment.
    ///////////////////////////////////////////////////////////////////////////
    template <typename Fragments>
    void build_environment(Fragments& fragments, environment& env)
    {
        build_basic_environment(env);

        env.define("qi:space",
            make_primitive_parser_composite(fragments, qi::space), 0, true);

        env.define("qi:alpha",
            make_primitive_parser_composite(fragments, qi::alpha), 0, true);

        env.define("qi:int_",
            make_primitive_parser_composite(fragments, qi::int_), 0, true);

        env.define("qi:char_",
            char_composite<Fragments>(fragments), 0, false);

        env.define("qi:*",
            kleene_composite<Fragments>(fragments), 1, true);

        env.define("qi:-",
            difference_composite<Fragments>(fragments), 2, true);

        env.define("qi:>>",
            sequence_composite<Fragments>(fragments), 2, false);

        env.define("qi:|",
            alternative_composite<Fragments>(fragments), 2, false);
    }
}}

///////////////////////////////////////////////////////////////////////////////
//  Main program
///////////////////////////////////////////////////////////////////////////////
int main()
{
    using scheme::utree;
    using scheme::interpreter;
    using scheme::environment;
    using scheme::qi::build_environment;
    using scheme::qi::rule_fragments;
    using scheme::qi::rule_type;
    using spirit_test::test;

    environment env;
    rule_fragments<rule_type> fragments;
    build_environment(fragments, env);

    scheme::qi::skipper_type space = boost::spirit::qi::space;

    {
        utree src =
            "(define charx (qi:char_ \"x\"))"
            "(define integer (qi:int_))"
            "(define nonzero (qi:- (qi:int_) (qi:char_ \"0\")))"
            "(define integers (qi:* (qi:int_)))"
            "(define intpair (qi:>> "
                "(qi:char_ \"(\") "
                "(qi:int_) "
                "(qi:char_ \",\") "
                "(qi:int_) "
                "(qi:char_ \")\")))"
            ;
        interpreter parser(src, "parse.scm", &env);

        BOOST_TEST(test("z",        fragments[parser["qi:char_"]()],    space));
        BOOST_TEST(test("x",        fragments[parser["charx"]()],       space));
        BOOST_TEST(!test("y",       fragments[parser["charx"]()],       space));
        BOOST_TEST(test("1234",     fragments[parser["integer"]()],     space));
        BOOST_TEST(!test("x1234",   fragments[parser["integer"]()],     space));
        BOOST_TEST(test("1 2 3 4",  fragments[parser["integers"]()],    space));
        BOOST_TEST(test("1",        fragments[parser["nonzero"]()],     space));
        BOOST_TEST(!test("0",       fragments[parser["nonzero"]()],     space));
        BOOST_TEST(test("(1, 2)",   fragments[parser["intpair"]()],     space));
        BOOST_TEST(!test("(1, x)",  fragments[parser["intpair"]()],     space));
    }

    {
        char const* filename = filename = "calc.scm";
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

        interpreter parser(in, filename, &env);
        rule_type calc = fragments[parser["expression"]()].alias();
        std::string str;

        while (std::getline(std::cin, str))
        {
            if (str.empty() || str[0] == 'q' || str[0] == 'Q')
                break;

            char const* iter = str.c_str();
            char const* end = iter + strlen(iter);
            bool r = phrase_parse(iter, end, calc, space);

            if (r && iter == end)
            {
                std::cout << "-------------------------\n";
                std::cout << "Parsing succeeded\n";
                std::cout << "-------------------------\n";
            }
            else
            {
                std::string rest(iter, end);
                std::cout << "-------------------------\n";
                std::cout << "Parsing failed\n";
                std::cout << "stopped at: \": " << rest << "\"\n";
                std::cout << "-------------------------\n";
            }
        }

    }

    return boost::report_errors();
}


