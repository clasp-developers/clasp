// Copyright Daniel Wallin 2008. Use, modification and distribution is
// subject to the Boost Software License, Version 1.0. (See accompanying
// file LICENSE_1_0.txt or copy at http://www.boost.org/LICENSE_1_0.txt)

#ifndef CLBIND_FUNCTION2_081014_HPP
# define CLBIND_FUNCTION2_081014_HPP

#include <clbind/policies.h>
#include <clbind/details.h>
#include <clbind/scope.h>

namespace clbind {


    class GoofBase{};
    template <typename F>
    class Goof : public GoofBase {};

    template <>
    class Goof<int(int,int)> : public GoofBase {
    public:
        typedef int(*F)(int,int);
        Goof( F y ) {};
    };

    template <typename FunctionPtrType,typename Policies>
    class VariadicFunctoid : public core::Functoid {};

#include "clbind_functoids.h"


    namespace detail
    {


        template <typename FunctionPointerType>
        struct CountFunctionArguments {
            enum {value = 0 };
        };

        template <typename RT, typename... ARGS>
        struct CountFunctionArguments<RT(*)(ARGS...)> {
            enum {value = sizeof...(ARGS)};
        };


        template <class FunctionPointerType,class Policies>
        struct function_registration : registration
        {
            function_registration(char const* name, FunctionPointerType f
                                  , Policies const& policies
                                  , string const& lambdalist
                                  , string const& declares
                                  , string const& docstring
                )
                : name(name)
                , functionPtr(f)
                , policies(policies)
                , m_lambdalist(lambdalist)
                , m_declares(declares)
                , m_docstring(docstring)
            {}

            void register_() const
            {
                core::Functoid* functoid = gctools::ClassAllocator<VariadicFunctoid<FunctionPointerType,Policies>>::allocateClass(name,functionPtr);
                core::lisp_defun_lispify_name(core::lisp_currentPackageName()
                                              , name
                                              , functoid
                                              , m_lambdalist
                                              , m_declares
                                              , m_docstring
                                              , true
                                              , true
                                              , (CountFunctionArguments<FunctionPointerType>::value
                                                 - CountPureOutValues<Policies>::value)
                    );
#if 0
                object fn = make_function(L, f, deduce_signature(f), policies);

                add_overload(
                    object(from_stack(L, -1))
                    , name
                    , fn
                    );
#endif
            }

            char const* name;
            FunctionPointerType functionPtr;
            Policies policies;
            string m_lambdalist;
            string m_declares;
            string m_docstring;
        };


    } // namespace detail

    template < typename F, class Policies>
    scope def(char const* name, F f
              , Policies const& policies
              , string const& lambdalist=""
              , string const& declares=""
              , string const& docstring="" )
    {
        return scope(std::auto_ptr<detail::registration>(
                         new detail::function_registration<F,Policies>(name, f, policies
                                                                        ,lambdalist,declares,docstring)));
    }

    template <class F>
    scope def(char const* name, F f
              , string const& lambdalist=""
              , string const& declares=""
              , string const& docstring="" )
    {
        return def(name, f, policies<>(), lambdalist,declares,docstring);
    }

} // namespace clbind

#endif // CLBIND_FUNCTION2_081014_HPP

