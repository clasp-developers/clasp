/*=============================================================================
    Copyright (c) 2001-2010 Joel de Guzman

    Distributed under the Boost Software License, Version 1.0. (See accompanying
    file LICENSE_1_0.txt or copy at http://www.boost.org/LICENSE_1_0.txt)
=============================================================================*/
#if !defined(BOOST_SPIRIT_SCHEME_INTERPRETER)
#define BOOST_SPIRIT_SCHEME_INTERPRETER

#include <list>
#include <boost/function.hpp>
#include <boost/foreach.hpp>
#include <boost/array.hpp>
#include <boost/scoped_array.hpp>
#include <boost/preprocessor/repetition/enum_params.hpp>
#include <boost/spirit/include/support_utree.hpp>

#define SCHEME_COMPOSITE_LIMIT 10

#if defined(BOOST_MSVC)
# pragma warning(push)
# pragma warning(disable: 4018)
#endif

namespace scheme
{
///////////////////////////////////////////////////////////////////////////////
//  The runtime interpreter
///////////////////////////////////////////////////////////////////////////////
    
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
    using boost::spirit::nil;

    ///////////////////////////////////////////////////////////////////////////
    // typedefs
    ///////////////////////////////////////////////////////////////////////////
    struct function;
    typedef std::list<function> actor_list;

    ///////////////////////////////////////////////////////////////////////////
    // actor
    ///////////////////////////////////////////////////////////////////////////
    template <typename Derived>
    struct actor
    {
        typedef utree result_type;
        typedef actor<Derived> base_type;

        utree operator()(scope const& env) const
        {
            return derived().eval(env);
        }

        utree operator()() const
        {
            return derived().eval(scope());
        }

        template <typename A0>
        utree operator()(A0 const& _0) const
        {
            boost::array<utree, 1> elements;
            elements[0] = _0;
            return derived().eval(get_range(elements));
        }

        template <typename A0, typename A1>
        utree operator()(A0 const& _0, A1 const& _1) const
        {
            boost::array<utree, 2> elements;
            elements[0] = _0;
            elements[1] = _1;
            return derived().eval(get_range(elements));
        }

        // More operators
        #include <scheme/detail/function_call.hpp>

        template <std::size_t n>
        static scope
        get_range(boost::array<utree, n>& array)
        {
            return scope(array.begin(), array.end());
        }

        Derived const& derived() const
        {
            return *static_cast<Derived const*>(this);
        }
    };

    ///////////////////////////////////////////////////////////////////////////
    // function
    ///////////////////////////////////////////////////////////////////////////
    struct function : actor<function>
    {
        utree f;
        function()
          : f() {}

        function(utree const& f)
          : f(f) {}

        template <typename F>
        function(F const& f)
          : f(stored_function<F>(f))
        {
        }

        bool empty() const
        {
            return f.which() != utree_type::function_type;
        }

        utree eval(scope const& env) const
        {
            return f.eval(env);
        }
    };

    ///////////////////////////////////////////////////////////////////////////
    // values
    ///////////////////////////////////////////////////////////////////////////
    struct value_function : actor<value_function>
    {
        utree val;
        value_function(utree const& val) : val(val) {}

        utree eval(scope const& /*env*/) const
        {
            return utree(boost::ref(val));
        }
    };

    struct value
    {
        typedef function result_type;
        function operator()(utree const& val) const
        {
            return function(value_function(val));
        }
    };

    value const val = {};

    inline function protect(function const& f)
    {
        return val(f.f);
    }

    ///////////////////////////////////////////////////////////////////////////
    // arguments
    ///////////////////////////////////////////////////////////////////////////
    template <bool scoped = true>
    struct argument_function : actor<argument_function<scoped> >
    {
        std::size_t n;
        std::size_t level;
        argument_function(std::size_t n, std::size_t level = 0)
          : n(n),
            level(level)
        {}

        utree eval(scope const& env) const
        {
            scope const* eptr = &env;
            while (level != eptr->level())
                eptr = eptr->outer();

            utree const& arg = (*eptr)[n];
            if (arg.which() != utree_type::function_type)
                return utree(boost::ref(arg));
            else
                return arg.eval(*eptr);
        }
    };

    template <> // scoped = false
    struct argument_function<false> : actor<argument_function<false> >
    {
        std::size_t n;
        argument_function(std::size_t n, std::size_t level = 0)
          : n(n)
        {}

        utree eval(scope const& env) const
        {
            scope const* eptr = &env;
            utree const& arg = (*eptr)[n];
            if (arg.which() != utree_type::function_type)
                return utree(boost::ref(arg));
            else
                return arg.eval(*eptr);
        }
    };

    template <bool scoped = true>
    struct argument
    {
        typedef function result_type;
        function operator()(std::size_t n, std::size_t level = 0) const
        {
            return function(argument_function<scoped>(n, level));
        }
    };

    // scoped arg
    argument<true> const arg = {};

    // unscoped arg
    argument<false> const unscoped_arg = {};

    // unscoped args
    function const _1 = unscoped_arg(0);
    function const _2 = unscoped_arg(1);
    function const _3 = unscoped_arg(2);
    function const _4 = unscoped_arg(3);
    function const _5 = unscoped_arg(4);
    function const _6 = unscoped_arg(5);
    function const _7 = unscoped_arg(6);
    function const _8 = unscoped_arg(7);
    function const _9 = unscoped_arg(8);
    function const _10 = unscoped_arg(10);

    ///////////////////////////////////////////////////////////////////////////
    // variable arguments.
    // Collects the arguments from n to last in a utree list.
    ///////////////////////////////////////////////////////////////////////////
    template <bool scoped = true>
    struct vararg_function : actor<vararg_function<scoped> >
    {
        std::size_t n;
        std::size_t level;
        vararg_function(std::size_t n, std::size_t level = 0)
          : n(n),
            level(level)
        {}

        utree eval(scope const& env) const
        {
            scope const* eptr = &env;
            while (level != eptr->level())
                eptr = eptr->outer();

            utree result;
            for (std::size_t i = n; i < eptr->size(); ++i)
            {
                utree const& arg = (*eptr)[i];
                if (arg.which() != utree_type::function_type)
                    result.push_back(utree(boost::ref(arg)));
                else
                    result.push_back(arg.eval(*eptr));
            }
            return result;
        }
    };

    template <> // scoped = false
    struct vararg_function<false> : actor<vararg_function<false> >
    {
        std::size_t n;
        vararg_function(std::size_t n, std::size_t level = 0)
          : n(n)
        {}

        utree eval(scope const& env) const
        {
            scope const* eptr = &env;
            utree result;
            for (std::size_t i = n; i < eptr->size(); ++i)
            {
                utree const& arg = (*eptr)[i];
                if (arg.which() != utree_type::function_type)
                    result.push_back(utree(boost::ref(arg)));
                else
                    result.push_back(arg.eval(*eptr));
            }
            return result;
        }
    };

    template <bool scoped = true>
    struct vararg
    {
        typedef function result_type;
        function operator()(std::size_t n, std::size_t level = 0) const
        {
            return function(vararg_function<scoped>(n, level));
        }
    };

    // scoped varg
    vararg<true> const varg = {};

    // unscoped varg
    vararg<false> const unscoped_varg = {};

    // unscoped vargs
    function const _1_ = unscoped_varg(0);
    function const _2_ = unscoped_varg(1);
    function const _3_ = unscoped_varg(2);
    function const _4_ = unscoped_varg(3);
    function const _5_ = unscoped_varg(4);
    function const _6_ = unscoped_varg(5);
    function const _7_ = unscoped_varg(6);
    function const _8_ = unscoped_varg(7);
    function const _9_ = unscoped_varg(8);
    function const _10_ = unscoped_varg(10);

    ///////////////////////////////////////////////////////////////////////////
    // composite
    ///////////////////////////////////////////////////////////////////////////
    template <typename Derived>
    struct composite
    {
        typedef function result_type;
        typedef composite<Derived> base_type;

        function operator()(actor_list const& elements) const
        {
            return derived().compose(elements);
        }

        template <typename A0>
        function operator()(A0 const& _0) const
        {
            actor_list elements;
            elements.push_back(as_function(_0));
            return derived().compose(elements);
        }

        template <typename A0, typename A1>
        function operator()(A0 const& _0, A1 const& _1) const
        {
            actor_list elements;
            elements.push_back(as_function(_0));
            elements.push_back(as_function(_1));
            return derived().compose(elements);
        }

        // More operators
        #include <scheme/detail/composite_call.hpp>

        Derived const& derived() const
        {
            return *static_cast<Derived const*>(this);
        }

        template <typename T>
        static function as_function(T const& val)
        {
            return scheme::val(utree(val));
        }

        static function const& as_function(function const& f)
        {
            return f;
        }
    };

    ///////////////////////////////////////////////////////////////////////////
    // unary_function
    ///////////////////////////////////////////////////////////////////////////
    template <typename Derived>
    struct unary_function : actor<unary_function<Derived> >
    {
        function a;
        typedef unary_function<Derived> base_type;

        unary_function(function const& a)
          : a(a)
        {
            BOOST_ASSERT(!a.empty());
        }

        utree eval(scope const& env) const
        {
            return derived().eval(a(env));
        }

        Derived const& derived() const
        {
            return *static_cast<Derived const*>(this);
        }
    };

    template <typename Function>
    struct unary_composite : composite<unary_composite<Function> >
    {
        function compose(actor_list const& elements) const
        {
            return function(Function(elements.front()));
        }
    };

    ///////////////////////////////////////////////////////////////////////////
    // binary_function
    ///////////////////////////////////////////////////////////////////////////
    template <typename Derived>
    struct binary_function : actor<binary_function<Derived> >
    {
        function a;
        function b;
        typedef binary_function<Derived> base_type;

        binary_function(function const& a, function const& b)
          : a(a), b(b)
        {
            BOOST_ASSERT(!a.empty());
            BOOST_ASSERT(!b.empty());
        }

        utree eval(scope const& env) const
        {
            return derived().eval(a(env), b(env));
        }

        Derived const& derived() const
        {
            return *static_cast<Derived const*>(this);
        }
    };

    template <typename Function>
    struct binary_composite : composite<binary_composite<Function> >
    {
        function compose(actor_list const& elements) const
        {
            actor_list::const_iterator i = elements.begin();
            function a = *i++;
            function b = *i;
            return function(Function(a, b));
        }
    };

    ///////////////////////////////////////////////////////////////////////////
    // nary_function
    ///////////////////////////////////////////////////////////////////////////
    template <typename Derived>
    struct nary_function : actor<nary_function<Derived> >
    {
        actor_list elements;
        typedef nary_function<Derived> base_type;

        nary_function(actor_list const& elements)
          : elements(elements)
        {
            BOOST_FOREACH(function const& element, elements)
            {
                BOOST_ASSERT(!element.empty());
            }
        }

        utree eval(scope const& env) const
        {
            BOOST_ASSERT(!elements.empty());
            actor_list::const_iterator i = elements.begin();
            utree result = (*i++)(env);
            boost::iterator_range<actor_list::const_iterator>
                rest(i++, elements.end());
            BOOST_FOREACH(function const& element, rest)
            {
                if (!derived().eval(result, element(env)))
                    break; // allow short-circuit evaluation
            }
            return result;
        }

        Derived const& derived() const
        {
            return *static_cast<Derived const*>(this);
        }
    };

    template <typename Function>
    struct nary_composite : composite<nary_composite<Function> >
    {
        function compose(actor_list const& elements) const
        {
            return function(Function(elements));
        }
    };

    ///////////////////////////////////////////////////////////////////////////
    // lambda
    ///////////////////////////////////////////////////////////////////////////
    struct lambda_function : actor<lambda_function>
    {
        int level;
        actor_list elements;
        // we must hold f by reference because functions can be recursive
        boost::reference_wrapper<function const> f;

        lambda_function(function const& f, actor_list const& elements, int level = 0)
          : level(level), elements(elements), f(f) {}

        typedef utree result_type;
        utree eval(scope const& env) const
        {
            // Get the parent scope
            scope const* outer = &env;
            while (level != outer->level())
                outer = outer->outer();

            if (!elements.empty())
            {
                boost::scoped_array<utree>
                    fargs(new utree[elements.size()]);
                std::size_t i = 0;
                BOOST_FOREACH(function const& element, elements)
                {
                    fargs[i++] = element(env);
                }
                utree* fi = fargs.get();
                return f.get()(scope(fi, fi+elements.size(), outer));
            }
            else
            {
                return f.get()(scope(0, 0, outer));
            }
        }
    };

    struct lambda : composite<lambda>
    {
        function f;

        lambda() : f() {}
        lambda(function const& f) : f(f) {}

        function compose(actor_list const& elements) const
        {
            return function(lambda_function(f, elements));
        }

        lambda& operator=(lambda const& other)
        {
            f = other.f;
            return *this;
        }

        lambda& operator=(function const& f_)
        {
            f = f_;
            return *this;
        }
    };
}

#if defined(BOOST_MSVC)
# pragma warning(pop)
#endif

#endif
