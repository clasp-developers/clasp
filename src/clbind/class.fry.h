// Copyright (c) 2003 Daniel Wallin and Arvid Norberg

// Permission is hereby granted, free of charge, to any person obtaining a
// copy of this software and associated documentation files (the "Software"),
// to deal in the Software without restriction, including without limitation
// the rights to use, copy, modify, merge, publish, distribute, sublicense,
// and/or sell copies of the Software, and to permit persons to whom the
// Software is furnished to do so, subject to the following conditions:

// The above copyright notice and this permission notice shall be included
// in all copies or substantial portions of the Software.

// THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF
// ANY KIND, EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED
// TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A
// PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT
// SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR
// ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN
// ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
// OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE
// OR OTHER DEALINGS IN THE SOFTWARE.


#ifndef CLBIND_CLASS_HPP_INCLUDED
#define CLBIND_CLASS_HPP_INCLUDED

/*
	ISSUES:
	------------------------------------------------------

	* solved for member functions, not application operator *
	if we have a base class that defines a function a derived class must be able to
	override that function (not just overload). Right now we just add the other overload
	to the overloads list and will probably get an ambiguity. If we want to support this
	each method_rep must include a vector of type_info pointers for each parameter.
	Operators do not have this problem, since operators always have to have
	it's own type as one of the arguments, no ambiguity can occur. Application
	operator, on the other hand, would have this problem.
	Properties cannot be overloaded, so they should always be overridden.
	If this is to work for application operator, we really need to specify if an application
	operator is const or not.

	If one class registers two functions with the same name and the same
	signature, there's currently no error. The last registered function will
	be the one that's used.
	How do we know which class registered the function? If the function was
	defined by the base class, it is a legal operation, to override it.
	we cannot look at the pointer offset, since it always will be zero for one of the bases.



	TODO:
	------------------------------------------------------

 	finish smart pointer support
		* the adopt policy should not be able to adopt pointers to held_types. This
		must be prohibited.
		* name_of_type must recognize holder_types and not return "custom"

	document custom policies, custom converters

	store the instance object for policies.

	support the __concat metamethod. This is a bit tricky, since it cannot be
	treated as a normal operator. It is a binary operator but we want to use the
	__tostring implementation for both arguments.
	
*/

//#include <clbind/prefix.hpp>
#include <clbind/config.h>

#include <utility>
#include <string>
#include <map>
#include <vector>
#include <cassert>

#include <boost/bind.hpp>
#include <boost/preprocessor/repetition/enum_params.hpp>
#include <boost/preprocessor/repetition/enum_params_with_a_default.hpp>
#include <boost/preprocessor/repetition/repeat.hpp>
#include <boost/type_traits/is_same.hpp>
#include <boost/type_traits/is_member_object_pointer.hpp>
#include <boost/mpl/apply.hpp>
#include <boost/mpl/lambda.hpp>
#include <boost/mpl/logical.hpp>
#include <boost/mpl/find_if.hpp>
#include <boost/mpl/eval_if.hpp>
#include <boost/mpl/logical.hpp>


#include <clbind/config.h>
#include <clbind/scope.h>
// #include <clbind/back_reference.hpp>
// #include <clbind/function.hpp>
// #include <clbind/dependency_policy.hpp>
//#include <clbind/detail/constructor.h>
// #include <clbind/detail/call.hpp>
// #include <clbind/detail/deduce_signature.hpp>
// #include <clbind/detail/compute_score.hpp>
#include <clbind/primitives.h>
// #include <clbind/detail/property.hpp>
// #include <clbind/detail/typetraits.hpp>
// #include <clbind/detail/class_rep.hpp>
// #include <clbind/detail/call.hpp>
// #include <clbind/detail/object_rep.hpp>
// #include <clbind/detail/calc_arity.hpp>
// #include <clbind/detail/call_member.hpp>
// #include <clbind/detail/enum_maker.hpp>
// #include <clbind/detail/operator_id.hpp>
// #include <clbind/detail/pointee_typeid.hpp>
#include <clbind/link_compatibility.h>
#include <clbind/inheritance.h>
#include <clbind/typeid.h>
#include <clbind/constructor.h>
#include <clbind/memberFunction.h>
#include <clbind/property.h>

// to remove the 'this' used in initialization list-warning
#ifdef _MSC_VER
#pragma warning(push)
#pragma warning(disable: 4355)
#endif

namespace boost
{

  template <class T> class shared_ptr;

} // namespace boost

namespace clbind
{	
    namespace detail
    {
        struct unspecified {};

        template<class Derived> struct operator_;

        struct you_need_to_define_a_get_const_holder_function_for_your_smart_ptr {};


        
    }


    extern default_constructor globalDefaultConstructorSignature;




    template<class T, class X1 = detail::unspecified, class X2 = detail::unspecified, class X3 = detail::unspecified>
    struct class_;

    // TODO: this function will only be invoked if the user hasn't defined a correct overload
    // maybe we should have a static assert in here?
    inline detail::you_need_to_define_a_get_const_holder_function_for_your_smart_ptr*
    get_const_holder(...)
    {
        return 0;
    }

    template <class T>
    boost::shared_ptr<T const>* get_const_holder(boost::shared_ptr<T>*)
    {
        return 0;
    }

    template <typename... Bases>
    struct bases
    {};

    typedef bases<reg::null_type> no_bases;

    namespace detail
    {

        namespace mpl = boost::mpl;

        template <class T>
        struct is_bases
            : mpl::false_
        {};

        template <typename... Bases>
        struct is_bases<bases<Bases...> >
            : mpl::true_
        {};

        template <class T, class P>
        struct is_unspecified
            : mpl::apply1<P, T>
        {};

        template <class P>
        struct is_unspecified<unspecified, P>
            : mpl::true_
        {};

        template <class P>
        struct is_unspecified_mfn
        {
            template <class T>
            struct apply
                : is_unspecified<T, P>
            {};
        };

        template<class Predicate>
        struct get_predicate
        {
            typedef mpl::protect<is_unspecified_mfn<Predicate> > type;
        };

        template <class Result, class Default>
        struct result_or_default
        {
            typedef Result type;
        };

        template <class Default>
        struct result_or_default<unspecified, Default>
        {
            typedef Default type;
        };

        template<class Parameters, class Predicate, class DefaultValue>
        struct extract_parameter
        {
            typedef typename get_predicate<Predicate>::type pred;
            typedef typename boost::mpl::find_if<Parameters, pred>::type iterator;
            typedef typename result_or_default<
                typename iterator::type, DefaultValue
                >::type type;
        };

	
        struct CLBIND_API create_class
        {
            static int stage1();
            static int stage2();
        };

    } // detail

    namespace detail {

        template<class T>
        struct static_scope
        {
            static_scope(T& self_) : self(self_)
            {
            }

            T& operator[](scope s) const
            {
                self.add_inner_scope(s);
                return self;
            }

        private:
            template<class U> void operator,(U const&) const;
            void operator=(static_scope const&);
			
            T& self;
        };

        struct class_registration;

        struct CLBIND_API class_base : scope
        {
        public:
            class_base(char const* name);		

            struct base_desc
            {
                type_id type;
                int ptr_offset;
            };

            void init(
                type_id const& type, class_id id
                , type_id const& wrapped_type, class_id wrapper_id
                , bool derivable );

            void add_base(type_id const& base, cast_function cast);

            void set_default_constructor(registration* member);
            void add_member(registration* member);
            void add_default_member(registration* member);

            const char* name() const;

            void add_static_constant(const char* name, int val);
            void add_inner_scope(scope& s);

            void add_cast(class_id src, class_id target, cast_function cast);

        private:
            class_registration* m_registration;
        };

// MSVC complains about member being sensitive to alignment (C4121)
// when F is a pointer to member of a class with virtual bases.
# ifdef BOOST_MSVC
#  pragma pack(push)
#  pragma pack(16)
# endif

        template<int N> 
        struct print_value_as_warning
        { 
            char operator()() { return N + 256; } //deliberately causing overflow
        };
        template <typename MethodPointerType>
        struct CountMethodArguments {
//            enum {value = 0 };
        };

        template <typename RT, typename OT, typename... ARGS>
        struct CountMethodArguments<RT(OT::*)(ARGS...)> {
            enum {value = sizeof...(ARGS)};
        };

        template <typename RT, typename OT, typename... ARGS>
        struct CountMethodArguments<RT(OT::*)(ARGS...) const> {
            enum {value = sizeof...(ARGS)};
        };

        template <class Class, class MethodPointerType, class Policies>
        struct memfun_registration : registration
        {
            memfun_registration(char const* name, MethodPointerType f, Policies const& policies
                                , string const& arguments
                                , string const& declares
                                , string const& docstring )
                : name(name)
                , methodPtr(f)
                , policies(policies)
                , m_arguments(arguments)
                , m_declares(declares)
                , m_docstring(docstring)
            {}

            void register_() const
            {
                core::Functoid* methoid = new IndirectVariadicMethoid<Policies,Class,MethodPointerType>(name,methodPtr);
                core::Symbol_sp classSymbol = reg::lisp_classSymbol<Class>();
//                int*** i = MethodPointerType(); printf("%p\n", i); // generate error to check type
//                print_value_as_warning<CountMethodArguments<MethodPointerType>::value>()();
                

                lisp_defineSingleDispatchMethod(name
                                                , classSymbol
                                                , methoid
                                                , 0
                                                , m_arguments
                                                , m_declares
                                                , m_docstring
                                                , true
                                                , CountMethodArguments<MethodPointerType>::value
                                                - CountPureOutValues<Policies>::value + 1 );
                // I'm going to comment out the luabind way of defining member functions
                // and use my way of defining member functions
                //
#if 0
                object fn = make_function(
                    L, f, deduce_signature(f, (Class*)0), policies);

                add_overload(
                    object(from_stack(L, -1))
                    , name
                    , fn
                    );
#endif
            }

            char const* name;
            MethodPointerType methodPtr;
            Policies policies;
            string m_arguments;
            string m_declares;
            string m_docstring;
        };

# ifdef BOOST_MSVC
#  pragma pack(pop)
# endif

        template <class P, class T>
        struct default_pointer
        {
            typedef P type;
        };

        template <class T>
        struct default_pointer<reg::null_type, T>
        {
            typedef std::auto_ptr<T> type;
        };


        template <typename ConstructorType>
        struct CountConstructorArguments {
            enum {value = 0 };
        };

        template <typename... ARGS>
        struct CountConstructorArguments<constructor<ARGS...> > {
            enum {value = sizeof...(ARGS)};
        };



        template <class Class, class Pointer, class Signature, class Policies>
        struct constructor_registration_base : public registration
        {
            constructor_registration_base(Policies const& policies, string const& name, string const& arguments, string const& declares, string const& docstring)
                : policies(policies), m_name(name), m_arguments(arguments), m_declares(declares), m_docstring(docstring)
            {}


            core::Functoid* makeConstructorFunctoid() const
            {
                string tname = m_name;
                if (m_name == "") { tname = "default-ctor"; };
                core::Functoid* f = new VariadicConstructorFunctoid<Policies,Pointer,Class,Signature>(tname);
                return f;
            }

            void register_() const
            {
                core::Functoid* f = this->makeConstructorFunctoid();
                lisp_defun_lispify_name(core::lisp_currentPackageName(),m_name,f,m_arguments,m_declares,m_docstring,true,true,CountConstructorArguments<Signature>::value);
            }


            Policies policies;
            string m_name;
            string m_arguments;
            string m_declares;
            string m_docstring;
        };


        template <class Class, class Pointer, class Signature, class Policies>
        struct constructor_registration : public constructor_registration_base<Class,Pointer,Signature,Policies> {
            constructor_registration(Policies const& policies, string const& name, string const& arguments, string const& declares, string const& docstring) : constructor_registration_base<Class,Pointer,Signature,Policies>(policies,name,arguments,declares,docstring) {};
        };


        template <class Class, class Pointer, class Policies>
        struct constructor_registration<Class,Pointer,constructor<>,Policies> : public constructor_registration_base<Class,Pointer,constructor<>,Policies> 
        {
            constructor_registration(Policies const& policies, string const& name, string const& arguments, string const& declares, string const& docstring) : constructor_registration_base<Class,Pointer,constructor<>,Policies>(policies,name,arguments,declares,docstring) {};
            AllocatorFunctor* registerDefaultConstructor_() const {
                AllocatorFunctor* allocator = new DefaultConstructorAllocatorFunctor<Class,Pointer>();
                return allocator;
            }
        };




#if 0 // begin_meister_disabled

        template <class T>
        struct reference_result
            : mpl::if_<
            mpl::or_<boost::is_pointer<T>, is_primitive<T> >
            , T
            , typename boost::add_reference<T>::type
            >
        {};

        template <class T, class Policies>
        struct inject_dependency_policy
            : mpl::if_<
            is_primitive<T>
            , Policies
            , policy_cons<dependency_policy<0, 1>, Policies>
            >
        {};

#endif 

        template <
            class Class
            , class Get, class GetPolicies
            , class Set = reg::null_type, class SetPolicies = reg::null_type
            >
        struct property_registration : registration
        {
            property_registration(
                char const* name
                , Get const& get
                , GetPolicies const& get_policies
                , Set const& set = Set()
                , SetPolicies const& set_policies = SetPolicies()
                , string const& arguments =""
                , string const& declares =""
                , string const& docstring ="" )
                : name(name)
                , get(get)
                , get_policies(get_policies)
                , set(set)
                , set_policies(set_policies)
                , m_arguments(arguments)
                , m_declares(declares)
                , m_docstring(docstring)
            {}

            void register_() const
            {
                const string n(name);
                core::Functoid* getter = new GetterMethoid<reg::null_type,Class,Get>(n,get);
//                int*** i = GetterMethoid<reg::null_type,Class,Get>(n,get);
//                printf("%p\n", i);
                core::Symbol_sp classSymbol = reg::lisp_classSymbol<Class>();
                lisp_defineSingleDispatchMethod(name
                                                , classSymbol
                                                , getter
                                                , 0
                                                , m_arguments
                                                , m_declares
                                                , m_docstring
                                                , true
                                                , 1 );
//                printf("%s:%d - allocated a getter@%p for %s\n", __FILE__, __LINE__, getter, name);
                // register the getter here
            }
#if 0
                object context(from_stack(L, -1));
                register_aux(
                    L
                    , context
                    , make_get(L, get, boost::is_member_object_pointer<Get>())
                    , set
                    );
            }

            template <class F>
            object make_get(cl_State* L, F const& f, mpl::false_) const
            {
                return make_function(
                    L, f, deduce_signature(f, (Class*)0), get_policies);
            }

            template <class T, class D>
            object make_get(cl_State* L, D T::* mem_ptr, mpl::true_) const
            {
                typedef typename reference_result<D>::type result_type;
                typedef typename inject_dependency_policy<
                    D, GetPolicies>::type policies;

                return make_function(
                    L
                    , access_member_ptr<T, D, result_type>(mem_ptr)
                    , mpl::vector2<result_type, Class const&>()
                    , policies()
                    );
            }

            template <class F>
            object make_set(cl_State* L, F const& f, mpl::false_) const
            {
                return make_function(
                    f, deduce_signature(f, (Class*)0), set_policies);
            }

            template <class T, class D>
            object make_set(cl_State* L, D T::* mem_ptr, mpl::true_) const
            {
                return make_function(
                    access_member_ptr<T, D>(mem_ptr)
                    , mpl::vector3<void, Class&, D const&>()
                    , set_policies
                    );
            }

            template <class S>
            void register_aux(
                object const& context
                , object const& get_, S const&) const
            {
                context[name] = property(
                    get_
                    , make_set(L, set, boost::is_member_object_pointer<Set>())
                    );
            }

            void register_aux(
                cl_State*, object const& context
                , object const& get_, null_type) const
            {
                context[name] = property(get_);
            }
#endif
            char const* name;
            Get get;
            GetPolicies get_policies;
            Set set;
            SetPolicies set_policies;
            string m_arguments;
            string m_declares;
            string m_docstring;
        };
    } // namespace detail




    // registers a class in the cl environment
    template<class T, class X1, class X2, class X3>
    struct class_: detail::class_base 
    {
        typedef class_<T, X1, X2, X3> self_t;

    private:

        template<class A, class B, class C, class D>
        class_(const class_<A,B,C,D>&);

    public:

        typedef boost::mpl::vector4<X1, X2, X3, detail::unspecified> parameters_type;

        // WrappedType MUST inherit from T
        typedef typename detail::extract_parameter<
            parameters_type
            , boost::is_base_and_derived<T, boost::mpl::_>
            , reg::null_type
            >::type WrappedType;

        typedef typename detail::extract_parameter<
            parameters_type
            , boost::mpl::not_<
                  boost::mpl::or_<
                      detail::is_bases<boost::mpl::_>
                      , boost::is_base_and_derived<boost::mpl::_, T>
                      , boost::is_base_and_derived<T, boost::mpl::_>
                      >
                  >
            , T* // Default is to put the pointer into a T*
            >::type HeldType;

        template <class Src, class Target>
        void add_downcast(Src*, Target*, boost::mpl::true_)
        {
            add_cast(
                reg::registered_class<Src>::id
                , reg::registered_class<Target>::id
                , detail::dynamic_cast_<Src, Target>::execute
                );
        }

        template <class Src, class Target>
        void add_downcast(Src*, Target*, boost::mpl::false_)
        {}

        // this function generates conversion information
        // in the given class_rep structure. It will be able
        // to implicitly cast to the given template type
        // Dummy return value
        template<class To>
        int gen_base_info(detail::type_<To>)
        {
            add_base(typeid(To), detail::static_cast_<T, To>::execute);
            add_cast(
                reg::registered_class<T>::id
                , reg::registered_class<To>::id
                , detail::static_cast_<T, To>::execute
                );

            add_downcast((To*)0, (T*)0, boost::is_polymorphic<To>());
            return 0;
        }

        int gen_base_info(detail::type_<reg::null_type>)
        { return 0;}

#define CLBIND_GEN_BASE_INFO(z, n, text) gen_base_info(detail::type_<BaseClass##n>());

        template<class...BaseClass> // BOOST_PP_ENUM_PARAMS(CLBIND_MAX_BASES, class BaseClass)>
        void generate_baseclass_list(detail::type_<bases<BaseClass...> > ) // BOOST_PP_ENUM_PARAMS(CLBIND_MAX_BASES, BaseClass)> >)
        {
#pragma clang diagnostic push
#pragma clang diagnostic ignored "-Wunused-variable"
            int dummy[sizeof...(BaseClass)] = {(gen_base_info(detail::type_<BaseClass>()))...};
#pragma clang diagnostic pop
//            BOOST_PP_REPEAT(CLBIND_MAX_BASES, CLBIND_GEN_BASE_INFO, _)
        }

#undef CLBIND_GEN_BASE_INFO

        class_(const char* name): class_base(name), scope(*this)
        {
#ifndef NDEBUG
            detail::check_link_compatibility();
#endif
            init(); 
            this->def_default_constructor_("default-ctor",&globalDefaultConstructorSignature,policies<>(),"","","");
        }



        class_(const char* name, no_default_constructor_type  ): class_base(name), scope(*this)
        {
#ifndef NDEBUG
            detail::check_link_compatibility();
#endif
            init(); 
        }





        template<typename... Types>
        class_& def_constructor(const string& name,
                    constructor<Types...> sig,
                    string const& arguments="",
                    string const& declares="",
                    string const& docstring="")
        {
            return this->def_constructor_(name, &sig, policies<>(),arguments,declares,docstring);
        }

        template<typename...Types, class Policies>
        class_& def_constructor(const string& name,
                    constructor<Types...> sig,
                    const Policies& policies,
                    string const& arguments="",
                    string const& declares="",
                    string const& docstring="")
        {
            return this->def_constructor_(name,&sig, policies,arguments,declares,docstring);
        }

        template<class F>
        class_& def(const char* name, F f)
        //                        string const& arguments="",
        //                        string const& declares="",
        //                        string const& docstring="")
        {
            return this->virtual_def(
                name, f, policies<>()
                , reg::null_type(), boost::mpl::true_()
                , "", "", "" );
             
//                , arguments, declares, docstring);
        }


        // virtual functions
        template<class F, class DefaultOrPolicies>
        class_& def(char const* name, F fn, DefaultOrPolicies default_or_policies
                    , string const& arguments=""
                    , string const& declares=""
                    , string const& docstring=""
            )
        {
            return this->virtual_def(
                name, fn, default_or_policies, reg::null_type()
                , CLBIND_MSVC_TYPENAME is_policy_list<DefaultOrPolicies>::type()
                , arguments
                , declares
                , docstring );
        }





        template <class Getter>
        class_& property(const char* name, Getter g
                    , string const& arguments=""
                    , string const& declares=""
                    , string const& docstring=""
            )
        {
            this->add_member(
                new detail::property_registration<T, Getter, reg::null_type>(
                    name // name
                    , g // Get
                    , reg::null_type() // GetPolicies
                    , reg::null_type()
                    , reg::null_type()
                    , arguments
                    , declares
                    , docstring));
            return *this;
        }

#if 0
        template <class Getter, class MaybeSetter>
        class_& property(const char* name, Getter g, MaybeSetter s)
        {
            return property_impl(
                name, g, s
                , boost::mpl::bool_<detail::is_policy_cons<MaybeSetter>::value>()
                );
        }

        template<class Getter, class Setter, class GetPolicies>
        class_& property(const char* name, Getter g, Setter s, const GetPolicies& get_policies)
        {
            typedef detail::property_registration<
                T, Getter, GetPolicies, Setter, null_type
                > registration_type;

            this->add_member(
                new registration_type(name, g, get_policies, s));
            return *this;
        }

        template<class Getter, class Setter, class GetPolicies, class SetPolicies>
        class_& property(
            const char* name
            , Getter g, Setter s
            , GetPolicies const& get_policies
            , SetPolicies const& set_policies)
        {
            typedef detail::property_registration<
                T, Getter, GetPolicies, Setter, SetPolicies
                > registration_type;

            this->add_member(
                new registration_type(name, g, get_policies, s, set_policies));
            return *this;
        }

        template <class C, class D>
        class_& def_readonly(const char* name, D C::*mem_ptr)
        {
            typedef detail::property_registration<T, D C::*, null_type>
                registration_type;

            this->add_member(
                new registration_type(name, mem_ptr, null_type()));
            return *this;
        }

        template <class C, class D, class Policies>
        class_& def_readonly(const char* name, D C::*mem_ptr, Policies const& policies)
        {
            typedef detail::property_registration<T, D C::*, Policies>
                registration_type;

            this->add_member(
                new registration_type(name, mem_ptr, policies));
            return *this;
        }

        template <class C, class D>
        class_& def_readwrite(const char* name, D C::*mem_ptr)
        {
            typedef detail::property_registration<
                T, D C::*, null_type, D C::*
                > registration_type;

            this->add_member(
                new registration_type(
                    name, mem_ptr, null_type(), mem_ptr));
            return *this;
        }

        template <class C, class D, class GetPolicies>
        class_& def_readwrite(
            const char* name, D C::*mem_ptr, GetPolicies const& get_policies)
        {
            typedef detail::property_registration<
                T, D C::*, GetPolicies, D C::*
                > registration_type;

            this->add_member(
                new registration_type(
                    name, mem_ptr, get_policies, mem_ptr));
            return *this;
        }

        template <class C, class D, class GetPolicies, class SetPolicies>
        class_& def_readwrite(
            const char* name
            , D C::*mem_ptr
            , GetPolicies const& get_policies
            , SetPolicies const& set_policies
            )
        {
            typedef detail::property_registration<
                T, D C::*, GetPolicies, D C::*, SetPolicies
                > registration_type;

            this->add_member(
                new registration_type(
                    name, mem_ptr, get_policies, mem_ptr, set_policies));
            return *this;
        }

        template<class Derived, class Policies>
        class_& def(detail::operator_<Derived>, Policies const& policies)
        {
            return this->def(
                Derived::name()
                , &Derived::template apply<T, Policies>::execute
                , policies
                );
        }

        template<class Derived>
        class_& def(detail::operator_<Derived>)
        {
            return this->def(
                Derived::name()
                , &Derived::template apply<T, null_type>::execute
                );
        }

        detail::enum_maker<self_t> enum_(const char*)
        {
            return detail::enum_maker<self_t>(*this);
        }
#endif // end_meister_disabled		
        detail::static_scope<self_t> scope;
		
    private:
        void operator=(class_ const&);

        void add_wrapper_cast(reg::null_type*)
        {}

        template <class U>
        void add_wrapper_cast(U*)
        {
            add_cast(
                reg::registered_class<U>::id
                , reg::registered_class<T>::id
                , detail::static_cast_<U,T>::execute
                );

            add_downcast((T*)0, (U*)0, boost::is_polymorphic<T>());
        }

        void init()
        {
            typedef typename detail::extract_parameter<
                parameters_type
                ,	boost::mpl::or_<
                    detail::is_bases<boost::mpl::_>
                            ,	boost::is_base_and_derived<boost::mpl::_, T>
                            >
                ,	no_bases
                >::type bases_t;

            typedef typename 
                boost::mpl::if_<detail::is_bases<bases_t>
                                ,	bases_t
                                ,	bases<bases_t>
				>::type Base;
	
            class_base::init(
                typeid(T)
                , reg::registered_class<T>::id
                , typeid(WrappedType)
                , reg::registered_class<WrappedType>::id
                , isDerivableCxxClass<T>(0)
                );

            add_wrapper_cast((WrappedType*)0);

            generate_baseclass_list(detail::type_<Base>());
        }


#if 0 // begin_meister_disabled
        template<class Getter, class GetPolicies>
        class_& property_impl(const char* name,
                              Getter g,
                              GetPolicies policies,
                              boost::mpl::bool_<true>)
        {
            this->add_member(
                new detail::property_registration<T, Getter, GetPolicies>(
                    name, g, policies));
            return *this;
        }

        template<class Getter, class Setter>
        class_& property_impl(const char* name,
                              Getter g,
                              Setter s,
                              boost::mpl::bool_<false>)
        {
            typedef detail::property_registration<
                T, Getter, null_type, Setter, null_type
                > registration_type;

            this->add_member(
                new registration_type(name, g, null_type(), s));
            return *this;
        }

        template<class F, class Default, class Policies>
        class_& virtual_def(char const* name, F const& fn
                            , Default const& default_, Policies const&, boost::mpl::false_)
        {
            this->add_member(
                new detail::memfun_registration<T, F, Policies>(
                    name, fn, Policies()));

            this->add_default_member(
                new detail::memfun_registration<T, Default, Policies>(
                    name, default_, Policies()));

            return *this;
        }

#endif // end_meister_disabled



        // these handle default implementation of virtual functions
        template<class F, class Policies>
        class_& virtual_def(char const* name, F const& fn
                            , Policies const&, reg::null_type, boost::mpl::true_,
                            string const& arguments, string const& declares, string const& docstring)
        {
            this->add_member(
                new detail::memfun_registration<T, F, Policies>(
                    name, fn, Policies(),arguments,declares,docstring));
            return *this;
        }




        template<class Signature, class Policies>
        class_& def_default_constructor_(const char* name, Signature*, Policies const&,string const& arguments, string const& declares, string const& docstring)
        {
            typedef typename boost::mpl::if_<
                boost::is_same<WrappedType, reg::null_type>
                , T
                , WrappedType
                >::type construct_type;


            this->set_default_constructor(
                new detail::constructor_registration<
                construct_type, HeldType, Signature, Policies>(
                    Policies(),name,arguments,declares,docstring));
            return *this;
        }



        template<class Signature, class Policies>
        class_& def_constructor_(const string& name, Signature*, Policies const&,string const& arguments, string const& declares, string const& docstring)
        {
            typedef Signature signature;

            typedef typename boost::mpl::if_<
                boost::is_same<WrappedType, reg::null_type>
                , T
                , WrappedType
                >::type construct_type;


            this->add_member(
                new detail::constructor_registration<
                construct_type, HeldType, signature, Policies>(
                    Policies(),name,arguments,declares,docstring));

#if 0
            this->add_default_member(
                new detail::constructor_registration<
                construct_type, HeldType, signature, Policies>(
                    Policies(),name,arguments,declares,docstring));
#endif
            return *this;
        }
    };

}

#ifdef _MSC_VER
#pragma warning(pop)
#endif

#endif // CLBIND_CLASS_HPP_INCLUDED

