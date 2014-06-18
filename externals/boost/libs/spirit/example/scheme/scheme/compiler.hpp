/*=============================================================================
    Copyright (c) 2001-2010 Joel de Guzman

    Distributed under the Boost Software License, Version 1.0. (See accompanying
    file LICENSE_1_0.txt or copy at http://www.boost.org/LICENSE_1_0.txt)
=============================================================================*/
#if !defined(BOOST_SPIRIT_SCHEME_COMPILER)
#define BOOST_SPIRIT_SCHEME_COMPILER

#include <vector>
#include <map>
#include <exception>

#include <boost/bind.hpp>
#include <boost/tuple/tuple.hpp>
#include <boost/lexical_cast.hpp>
#include <scheme/intrinsics.hpp>
#include <scheme/interpreter.hpp>
#include <input/parse_sexpr.hpp>

namespace scheme
{
///////////////////////////////////////////////////////////////////////////////
//  Exceptions
///////////////////////////////////////////////////////////////////////////////
    struct scheme_exception : std::exception {};

    struct compilation_error : std::exception
    {
        ~compilation_error() throw() {}
        virtual const char* what() const throw()
        {
            return "scheme: Compilation error.";
        }
    };

    struct identifier_expected : scheme_exception
    {
        ~identifier_expected() throw() {}
        virtual const char* what() const throw()
        {
            return "scheme: Identifier expected.";
        }
    };

    struct identifier_not_found : scheme_exception
    {
        std::string msg;
        identifier_not_found(std::string const& id)
          : msg("scheme: Identifier (" + id + ") not found.") {}
        ~identifier_not_found() throw() {}

        virtual const char* what() const throw()
        {
            return msg.c_str();;
        }
    };

    struct duplicate_identifier : scheme_exception
    {
        std::string msg;
        duplicate_identifier(std::string const& id)
          : msg("scheme: Duplicate identifier (" + id + ").") {}
        ~duplicate_identifier() throw() {}

        virtual const char* what() const throw()
        {
            return msg.c_str();
        }
    };

    struct body_already_defined : scheme_exception
    {
        std::string msg;
        body_already_defined(std::string const& id)
          : msg("scheme: Multiple definition (" + id + ").") {}
        ~body_already_defined() throw() {}

        virtual const char* what() const throw()
        {
            return msg.c_str();
        }
    };

    struct incorrect_arity : scheme_exception
    {
        std::string msg;
        incorrect_arity(std::string const& id, int arity, bool fixed)
          : msg("scheme: Invalid number of parameters to function call ("
                + id + ").")
        {
            if (!fixed)
                msg += std::string(" Expecting at least ");
            else
                msg += std::string(" Expecting ");

            msg += boost::lexical_cast<std::string>(arity) + " arguments.";
        }
        ~incorrect_arity() throw() {}

        virtual const char* what() const throw()
        {
            return msg.c_str();
        }
    };

    struct function_application_expected : scheme_exception
    {
        std::string msg;
        function_application_expected(utree const& got)
        {
            // $$$ TODO: add got to message $$$
            msg = "scheme: Function application expected";
        }
        ~function_application_expected() throw() {}

        virtual const char* what() const throw()
        {
            return msg.c_str();
        }
    };

    struct no_body : scheme_exception
    {
        ~no_body() throw() {}
        virtual const char* what() const throw()
        {
            return "scheme: No expression in body.";
        }
    };

///////////////////////////////////////////////////////////////////////////////
//  The environment
///////////////////////////////////////////////////////////////////////////////
    typedef boost::function<function(actor_list const&)> compiled_function;

    class environment
    {
    public:

        environment(environment* parent = 0)
          : outer(parent),
            depth(parent? parent->depth + 1 : 0)
        {}

        template <typename Function>
        void define(std::string const& name, Function const& f, int arity, bool fixed)
        {
            if (definitions.find(name) != definitions.end())
                throw duplicate_identifier(name);
            definitions[name] = boost::make_tuple(compiled_function(f), arity, fixed);
        }

        boost::tuple<compiled_function*, int, bool>
        find(std::string const& name)
        {
            std::map<std::string, map_element>::iterator
                i = definitions.find(name);
            if (i != definitions.end())
                return boost::make_tuple(
                    &boost::get<0>(i->second),
                    boost::get<1>(i->second),
                    boost::get<2>(i->second)
                );
            else if (outer != 0)
                return outer->find(name);
            return boost::make_tuple((compiled_function*)0, 0, false);
        }

        void undefine(std::string const& name)
        {
            definitions.erase(name);
        }

        bool defined(std::string const& name)
        {
            return definitions.find(name) != definitions.end();
        }

        void forward_declare(std::string const& name, function* f)
        {
            forwards[name] = f;
        }

        function* find_forward(std::string const& name)
        {
            std::map<std::string, function*>::iterator
                iter = forwards.find(name);
            if (iter == forwards.end())
                return 0;
            else
                return iter->second;
        }

        environment* parent() const { return outer; }
        int level() const { return depth; }

    private:

        typedef boost::tuple<compiled_function, int, bool> map_element;

        environment* outer;
        std::map<std::string, map_element> definitions;
        std::map<std::string, function*> forwards;
        int depth;
    };

///////////////////////////////////////////////////////////////////////////////
//  The compiler
///////////////////////////////////////////////////////////////////////////////
    function compile(
        utree const& ast,
        environment& env,
        actor_list& fragments,
        int parent_line,
        std::string const& source_file = "");

    struct external_function : composite<external_function>
    {
        // we must hold f by reference because functions can be recursive
        boost::reference_wrapper<function const> f;
        int level;

        external_function(function const& f, int level)
          : f(f), level(level) {}

        using base_type::operator();
        function operator()(actor_list const& elements) const
        {
            return function(lambda_function(f, elements, level));
        }
    };

    struct compiler
    {
        typedef function result_type;
        environment& env;
        actor_list& fragments;
        int line;
        std::string source_file;

        compiler(
            environment& env,
            actor_list& fragments,
            int line,
            std::string const& source_file = "")
          : env(env), fragments(fragments),
            line(line), source_file(source_file)
        {
        }

        function operator()(nil) const
        {
            return scheme::val(utree());
        }

        template <typename T>
        function operator()(T const& val) const
        {
            return scheme::val(utree(val));
        }

        function operator()(utf8_symbol_range const& str) const
        {
            std::string name(str.begin(), str.end());
            boost::tuple<compiled_function*, int, bool> r = env.find(name);
            if (boost::get<0>(r))
            {
                actor_list flist;
                return (*boost::get<0>(r))(flist);
            }
            throw identifier_not_found(name);
            return function();
        }

        function make_lambda(
            std::vector<std::string> const& args,
            bool fixed_arity,
            utree const& body) const
        {
            environment local_env(&this->env);
            for (std::size_t i = 0; i < args.size(); ++i)
            {
                if (!fixed_arity && (args.size() - 1) == i)
                    local_env.define(args[i],
                        boost::bind(varg, i, local_env.level()), 0, false);
                else
                    local_env.define(args[i],
                        boost::bind(arg, i, local_env.level()), 0, false);
            }

            actor_list flist;
            if (body.size() == 0)
                return function();
                //~ throw no_body();

            BOOST_FOREACH(utree const& item, body)
            {
                function f = compile(item, local_env, fragments, line, source_file);
                if (!is_define(item))
                    flist.push_back(f);
            }
            if (flist.size() > 1)
                return protect(block(flist));
            else
                return protect(flist.front());
        }

        static bool is_define(utree const& item)
        {
            if (item.which() != utree_type::list_type ||
                item.begin()->which() != utree_type::symbol_type)
                return false;
            return get_symbol(*item.begin()) == "define";
        }

        function define_function(
            std::string const& name,
            std::vector<std::string>& args,
            bool fixed_arity,
            utree const& body) const
        {
            try
            {
                function* fp = 0;
                if (env.defined(name))
                {
                    fp = env.find_forward(name);
                    if (fp != 0 && !fp->empty())
                        throw body_already_defined(name);
                }

                if (fp == 0)
                {
                    fragments.push_back(function());
                    fp = &fragments.back();
                    env.define(name, external_function(*fp, env.level()), args.size(), fixed_arity);
                }

                function lambda = make_lambda(args, fixed_arity, body);
                if (!lambda.empty())
                {
                    // unprotect (eval returns a function)
                    *fp = lambda();
                }
                else
                {
                    // allow forward declaration of scheme functions
                    env.forward_declare(name, fp);
                }
                return *fp;
            }
            catch (std::exception const&)
            {
                env.undefine(name);
                throw;
            }
        }

        function operator()(utree::const_range const& range) const
        {
            typedef utree::const_range::iterator iterator;

            if (range.begin()->which() != utree_type::symbol_type)
                throw function_application_expected(*range.begin());

            std::string name(get_symbol(*range.begin()));

            if (name == "quote")
            {
                iterator i = range.begin(); ++i;
                return scheme::val(*i);
            }

            if (name == "define")
            {
                std::string fname;
                std::vector<std::string> args;
                bool fixed_arity = true;

                iterator i = range.begin(); ++i;
                if (i->which() == utree_type::list_type)
                {
                    // (define (f x) ...body...)
                    utree const& decl = *i++;
                    iterator di = decl.begin();
                    fname = get_symbol(*di++);
                    while (di != decl.end())
                    {
                        std::string sym = get_symbol(*di++);
                        if (sym == ".")
                           // check that . is one pos behind the last arg
                           fixed_arity = false;
                        else
                            args.push_back(sym);
                    }
                }
                else
                {
                    // (define f ...body...)
                    fname = get_symbol(*i++);

                    // (define f (lambda (x) ...body...))
                    if (i != range.end()
                        && i->which() == utree_type::list_type
                        && get_symbol((*i)[0]) == "lambda")
                    {
                        utree const& arg_names = (*i)[1];
                        iterator ai = arg_names.begin();
                        while (ai != arg_names.end())
                        {
                            std::string sym = get_symbol(*ai++);
                            if (sym == ".")
                                // check that . is one pos behind the last arg
                                fixed_arity = false;
                            else
                                args.push_back(sym);
                        };

                        iterator bi = i->begin(); ++bi; ++bi; // (*i)[2]
                        utree body(utree::const_range(bi, i->end()), shallow);
                        return define_function(fname, args, fixed_arity, body);
                    }
                }

                utree body(utree::const_range(i, range.end()), shallow);
                return define_function(fname, args, fixed_arity, body);
            }

            if (name == "lambda")
            {
                // (lambda (x) ...body...)
                iterator i = range.begin(); ++i;
                utree const& arg_names = *i++;
                iterator ai = arg_names.begin();
                std::vector<std::string> args;
                bool fixed_arity = true;

                while (ai != arg_names.end())
                {
                    std::string sym = get_symbol(*ai++);
                    if (sym == ".")
                        // check that . is one pos behind the last arg
                        fixed_arity = false;
                    else
                        args.push_back(sym);
                }

                utree body(utree::const_range(i, range.end()), shallow);
                return make_lambda(args, fixed_arity, body);
            }

            // (f x)
            boost::tuple<compiled_function*, int, bool> r = env.find(name);
            if (boost::get<0>(r))
            {
                compiled_function* cf = boost::get<0>(r);
                int arity = boost::get<1>(r);
                bool fixed_arity = boost::get<2>(r);

                actor_list flist;
                iterator i = range.begin(); ++i;
                int size = 0;
                for (; i != range.end(); ++i, ++size)
                {
                    flist.push_back(
                        compile(*i, env, fragments, line, source_file));
                }

                // Arity check
                if (!fixed_arity) // non-fixed arity
                {
                    if (size < arity)
                        throw incorrect_arity(name, arity, false);
                }
                else // fixed arity
                {
                    if (size != arity)
                        throw incorrect_arity(name, arity, true);
                }
                return (*cf)(flist);
            }
            else
            {
                throw identifier_not_found(name);
            }

            // Can't reach here
            throw compilation_error();
            return function();
        }

        function operator()(function_base const& pf) const
        {
            // Can't reach here. Surely, at this point, we don't have
            // utree functions yet. The utree AST should be pure data.
            throw compilation_error();
            return function();
        }

        static std::string get_symbol(utree const& s)
        {
            if (s.which() != utree_type::symbol_type)
                throw identifier_expected();
            utf8_symbol_range symbol = s.get<utf8_symbol_range>();
            return std::string(symbol.begin(), symbol.end());
        }
    };

    inline function compile(
        utree const& ast,
        environment& env,
        actor_list& fragments,
        int parent_line,
        std::string const& source_file)
    {
        int line = (ast.which() == utree_type::list_type)
            ? ast.tag() : parent_line;

        try
        {
            return utree::visit(ast,
                compiler(env, fragments, line, source_file));
        }
        catch (scheme_exception const& x)
        {
            if (source_file != "")
                std::cerr << source_file;

            if (line != -1)
                std::cerr << '(' << line << ')';

            std::cerr << " : Error! "  << x.what() << std::endl;
            throw compilation_error();
        }

        return function();
    }

    void compile_all(
        utree const& ast,
        environment& env,
        actor_list& results,
        actor_list& fragments,
        std::string const& source_file = "")
    {
        int line = (ast.which() == utree_type::list_type)
            ? ast.tag() : 1;
        BOOST_FOREACH(utree const& program, ast)
        {
            scheme::function f;
            try
            {
                if (!compiler::is_define(program))
                {
                    if (source_file != "")
                        std::cerr << source_file;

                    int progline = (program.which() == utree_type::list_type)
                        ? program.tag() : line;

                    std::cerr << '(' << progline << ')';

                    std::cerr << " : Error! scheme: Function definition expected." << std::endl;
                    continue; // try the next expression
                }
                else
                {
                    f = compile(program, env, fragments, line, source_file);
                }
            }
            catch (compilation_error const&)
            {
                continue; // try the next expression
            }
            results.push_back(f);
        }
    }

    void build_basic_environment(environment& env)
    {
        env.define("if", if_, 3, true);
        env.define("begin", block, 1, false);
        env.define("list", list, 1, false);
        env.define("display", display, 1, true);
        env.define("front", front, 1, true);
        env.define("back", back, 1, true);
        env.define("rest", rest, 1, true);
        env.define("=", equal, 2, true);
        env.define("<", less_than, 2, true);
        env.define("<=", less_than_equal, 2, true);
        env.define("+", plus, 2, false);
        env.define("-", minus, 2, false);
        env.define("*", times, 2, false);
        env.define("/", divide, 2, false);
    }

    ///////////////////////////////////////////////////////////////////////////
    // interpreter
    ///////////////////////////////////////////////////////////////////////////
    struct interpreter
    {
        template <typename Source>
        interpreter(
            Source& in,
            std::string const& source_file = "<string>",
            environment* envp = 0)
        {
            if (envp == 0)
                build_basic_environment(env);
            else
                env = *envp;

            if (input::parse_sexpr_list(in, program, source_file))
            {
                compile_all(program, env, flist, fragments, source_file);
            }
        }

        interpreter(
            utree const& program,
            environment* envp = 0)
        {
            if (envp == 0)
                build_basic_environment(env);
            else
                env = *envp;

            compile_all(program, env, flist, fragments);
        }

        function operator[](std::string const& name)
        {
            boost::tuple<compiled_function*, int, bool> r = env.find(name);
            if (boost::get<0>(r))
            {
                compiled_function* cf = boost::get<0>(r);
                int arity = boost::get<1>(r);
                bool fixed_arity = boost::get<2>(r);
                actor_list flist;

                if (arity > 0)
                {
                    for (int i = 0; i < (arity-1); ++i)
                        flist.push_back(arg(i));

                    if (fixed_arity)
                        flist.push_back(arg(arity-1));
                    else
                        flist.push_back(varg(arity-1));
                }
                return (*cf)(flist);
            }
            else
            {
                std::cerr
                    << " : Error! scheme: Function "
                    << name
                    << " not found."
                    << std::endl;
                return function();
            }
        }

        environment env;
        utree program;
        actor_list fragments;
        actor_list flist;
    };
}

#endif
