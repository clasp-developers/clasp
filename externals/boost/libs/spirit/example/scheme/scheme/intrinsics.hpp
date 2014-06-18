/*=============================================================================
    Copyright (c) 2001-2010 Joel de Guzman, Bryce Lelbach

    Distributed under the Boost Software License, Version 1.0. (See accompanying
    file LICENSE_1_0.txt or copy at http://www.boost.org/LICENSE_1_0.txt)
=============================================================================*/
#if !defined(BOOST_SPIRIT_SCHEME_INTRINSICS)
#define BOOST_SPIRIT_SCHEME_INTRINSICS

#include <scheme/interpreter.hpp>
#include <iostream>

namespace scheme
{
    namespace detail {

      inline utree rest(utree& x)
      {
        utree::iterator i = x.begin(); ++i;
        return utree(utree::range(i, x.end()), shallow);
      }

      inline utree rest(utree const& x)
      {
        utree::const_iterator i = x.begin(); ++i;
        return utree(utree::const_range(i, x.end()), shallow);
      }

    } 

    ///////////////////////////////////////////////////////////////////////////
    // if
    ///////////////////////////////////////////////////////////////////////////
    struct if_function : actor<if_function>
    {
        function cond;
        function then;
        function else_;
        if_function(
            function const& cond, function const& then, function const& else_)
          : cond(cond), then(then), else_(else_)
        {
            BOOST_ASSERT(!cond.empty());
            BOOST_ASSERT(!then.empty());
            BOOST_ASSERT(!else_.empty());
        }

        typedef utree result_type;
        utree eval(scope const& env) const
        {
            return cond(env).get<bool>() ? then(env) : else_(env);
        }
    };

    struct if_composite : composite<if_composite>
    {
        function compose(actor_list const& elements) const
        {
            actor_list::const_iterator i = elements.begin();
            function if_ = *i++;
            function then = *i++;
            function else_ = *i;
            return function(if_function(if_, then, else_));
        }
    };

    if_composite const if_ = if_composite();

    ///////////////////////////////////////////////////////////////////////////
    // list
    ///////////////////////////////////////////////////////////////////////////
    struct list_function : actor<list_function>
    {
        actor_list elements;
        list_function(actor_list const& elements)
          : elements(elements)
        {
            BOOST_FOREACH(function const& element, elements)
            {
                BOOST_ASSERT(!element.empty());
            }
        }

        utree eval(scope const& env) const
        {
            utree result;
            BOOST_FOREACH(function const& element, elements)
            {
                result.push_back(element(env));
            }
            return result;
        }
    };

    struct list_composite : composite<list_composite>
    {
        function compose(actor_list const& elements) const
        {
            return function(list_function(elements));
        }
    };

    list_composite const list = list_composite();

    ///////////////////////////////////////////////////////////////////////////
    // block
    ///////////////////////////////////////////////////////////////////////////
    struct block_function : actor<block_function>
    {
        actor_list elements;
        block_function(actor_list const& elements)
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
            actor_list::const_iterator end = elements.end(); --end;
            boost::iterator_range<actor_list::const_iterator>
                head_elements(elements.begin(), end);
            BOOST_FOREACH(function const& element, head_elements)
            {
                element(env);
            }
            return (*end)(env);
        }
    };

    struct block_composite : composite<block_composite>
    {
        function compose(actor_list const& elements) const
        {
            return function(block_function(elements));
        }
    };

    block_composite const block = block_composite();

    ///////////////////////////////////////////////////////////////////////////
    // SCHEME_UNARY_INTRINSIC
    ///////////////////////////////////////////////////////////////////////////
#define SCHEME_UNARY_INTRINSIC(name, expression)                                \
    struct name##_function : unary_function<name##_function>                    \
    {                                                                           \
        name##_function(function const& a)                                      \
          : base_type(a) {}                                                     \
                                                                                \
        utree eval(utree const& element) const                                  \
        {                                                                       \
            return expression;                                                  \
        }                                                                       \
    };                                                                          \
                                                                                \
    struct name##_composite : unary_composite<name##_function> {};              \
    name##_composite const name = name##_composite()                            \
    /***/

    ///////////////////////////////////////////////////////////////////////////
    // SCHEME_BINARY_INTRINSIC
    ///////////////////////////////////////////////////////////////////////////
#define SCHEME_BINARY_INTRINSIC(name, expression)                               \
    struct name##_function                                                      \
      : binary_function<name##_function>                                        \
    {                                                                           \
        name##_function(function const& a, function const& b)                   \
          : base_type(a, b) {}                                                  \
                                                                                \
        typedef utree result_type;                                              \
        utree eval(utree const& a, utree const& b) const                        \
        {                                                                       \
            return expression;                                                  \
        }                                                                       \
    };                                                                          \
                                                                                \
    struct name##_composite                                                     \
      : binary_composite<name##_function> {};                                   \
                                                                                \
    name##_composite const name = name##_composite()                            \
    /***/

    ///////////////////////////////////////////////////////////////////////////
    // SCHEME_NARY_INTRINSIC
    ///////////////////////////////////////////////////////////////////////////
#define SCHEME_NARY_INTRINSIC(name, expression)                                 \
    struct name##_function : nary_function<name##_function>                     \
    {                                                                           \
        name##_function(actor_list const& elements)                             \
          : base_type(elements) {}                                              \
                                                                                \
        bool eval(utree& result, utree const& element) const                    \
        {                                                                       \
            expression;                                                         \
            return true;                                                        \
        }                                                                       \
    };                                                                          \
                                                                                \
    struct name##_composite : nary_composite<name##_function> {};               \
    name##_composite const name = name##_composite()                            \
    /***/

    ///////////////////////////////////////////////////////////////////////////
    // unary intrinsics
    ///////////////////////////////////////////////////////////////////////////
    SCHEME_UNARY_INTRINSIC(display, (std::cout << element, utree()));
    SCHEME_UNARY_INTRINSIC(front, element.front());
    SCHEME_UNARY_INTRINSIC(back, element.back());
    SCHEME_UNARY_INTRINSIC(rest, detail::rest(element));

    ///////////////////////////////////////////////////////////////////////////
    // binary intrinsics
    ///////////////////////////////////////////////////////////////////////////
    SCHEME_BINARY_INTRINSIC(equal, a == b);
    equal_composite const eq = equal; // synonym

    SCHEME_BINARY_INTRINSIC(less_than, a < b);
    less_than_composite const lt = less_than; // synonym

    SCHEME_BINARY_INTRINSIC(less_than_equal, a <= b);
    less_than_equal_composite const lte = less_than_equal; // synonym

    ///////////////////////////////////////////////////////////////////////////
    // nary intrinsics
    ///////////////////////////////////////////////////////////////////////////
    SCHEME_NARY_INTRINSIC(plus, result = result + element);
    SCHEME_NARY_INTRINSIC(minus, result = result - element);
    SCHEME_NARY_INTRINSIC(times, result = result * element);
    SCHEME_NARY_INTRINSIC(divide, result = result / element);
}

#endif
