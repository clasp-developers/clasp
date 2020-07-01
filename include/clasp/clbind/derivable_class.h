/*
    File: derivable_class.h
*/

/*
Copyright (c) 2014, Christian E. Schafmeister
 
CLASP is free software; you can redistribute it and/or
modify it under the terms of the GNU Library General Public
License as published by the Free Software Foundation; either
version 2 of the License, or (at your option) any later version.
 
See directory 'clasp/licenses' for full details.
 
The above copyright notice and this permission notice shall be included in
all copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
THE SOFTWARE.
*/
/* -^- */
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

#ifndef CLBIND_DERIVABLE_CLASS_HPP_INCLUDED
#define CLBIND_DERIVABLE_CLASS_HPP_INCLUDED

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

//#include <clasp/clbind/prefix.hpp>
#include <clasp/clbind/config.h>

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

#include <clasp/core/foundation.h>
#include <clasp/core/instance.h>
#include <clasp/clbind/config.h>
#include <clasp/clbind/function.h>
#include <clasp/clbind/scope.h>
#include <clasp/clbind/class.h>
// #include <clasp/clbind/back_reference.hpp>
// #include <clasp/clbind/function.hpp>
// #include <clasp/clbind/dependency_policy.hpp>
//#include <clasp/clbind/detail/constructor.h>
// #include <clasp/clbind/detail/call.hpp>
// #include <clasp/clbind/detail/deduce_signature.hpp>
// #include <clasp/clbind/detail/compute_score.hpp>
#include <clasp/clbind/primitives.h>
// #include <clasp/clbind/detail/property.hpp>
// #include <clasp/clbind/detail/typetraits.hpp>
// #include <clasp/clbind/detail/class_rep.hpp>
// #include <clasp/clbind/detail/call.hpp>
// #include <clasp/clbind/detail/object_rep.hpp>
// #include <clasp/clbind/detail/calc_arity.hpp>
// #include <clasp/clbind/detail/call_member.hpp>
#include <clasp/clbind/enum_maker.h>
// #include <clasp/clbind/detail/operator_id.hpp>
// #include <clasp/clbind/detail/pointee_typeid.hpp>
#include <clasp/clbind/link_compatibility.h>
#include <clasp/clbind/inheritance.h>
#include <clasp/clbind/iteratorMemberFunction.h>
#include <clasp/clbind/typeid.h>
#include <clasp/clbind/constructor.h>
#include <clasp/clbind/memberFunction.h>
#include <clasp/clbind/property.h>

// to remove the 'this' used in initialization list-warning
#ifdef _MSC_VER
#pragma warning(push)
#pragma warning(disable : 4355)
#endif

namespace boost {

template <class T>
class shared_ptr;

} // namespace boost

namespace clbind {

template <class T, class X1 = detail::unspecified, class X2 = detail::unspecified, class X3 = detail::unspecified>
struct derivable_class_;

namespace detail {

  struct derivable_class_registration;

  struct derivable_class_registration : registration {
    derivable_class_registration(char const *name);

    void register_() const;

    const char *m_name;

  virtual std::string name() const { return this->m_name;}
  virtual std::string kind() const { return "derivable_class_registration"; };
    
    mutable std::map<const char *, int, detail::ltstr> m_static_constants;

    typedef std::pair<type_id, cast_function> base_desc;
    mutable std::vector<base_desc> m_bases;

    type_id m_type;
    class_id m_id;
    class_id m_wrapper_id;
    type_id m_wrapper_type;
    std::vector<cast_entry> m_casts;

    scope_ m_scope;
    scope_ m_members;
    detail::registration *m_default_constructor;
    scope_ m_default_members;
    bool m_derivable;
  };

  struct CLBIND_API derivable_class_base : scope_ {
  public:
    derivable_class_base(char const *name);

    struct base_desc {
      type_id type;
      int ptr_offset;
    };

    void init(type_id const &type, class_id id, type_id const &wrapped_type, class_id wrapper_id, bool derivable);

    void add_base(type_id const &base, cast_function cast);

    void set_default_constructor(registration *member);
    void add_member(registration *member);
    void add_default_member(registration *member);

    const char *name() const;

    void add_static_constant(const char *name, int val);
    void add_inner_scope(scope_ &s);

    void add_cast(class_id src, class_id target, cast_function cast);

  private:
    derivable_class_registration *m_registration;
  };

/*! This is the constructor registration for default constructors of non derivable classes,
         Specialized by making second template parameter reg::null_type
        */
template <class Class, class Policies>
  struct constructor_registration<Class, reg::null_type, default_constructor, Policies, construct_derivable_class> : public constructor_registration_base<Class, reg::null_type, default_constructor, Policies> {
  constructor_registration(Policies const &policies, string const &name, string const &arguments, string const &declares, string const &docstring) : constructor_registration_base<Class, reg::null_type, default_constructor, Policies>(policies, name, arguments, declares, docstring), mm_name(name){};
  core::Creator_sp registerDefaultConstructor_() const {
    //                printf("%s:%d In constructor_registration::registerDefaultConstructor derivable_default_constructor<> ----- Make sure that I'm being called for derivable classes\n", __FILE__, __LINE__ );
    return gc::As_unsafe<core::Creator_sp>(gctools::GC<DerivableDefaultConstructorCreator_O<Class>>::allocate());
  }
  virtual std::string name() const { return this->mm_name;}
  virtual std::string kind() const { return "derivable constructor_registration"; };
  std::string mm_name;
};

} // namespace detail


 void validateRackOffset(size_t wrapped_type_offset);
  

// registers a class in the cl environment
template <class T, class X1, class X2, class X3>
struct derivable_class_ : detail::derivable_class_base {
  typedef derivable_class_<T, X1, X2, X3> self_t;

private:
  template <class A, class B, class C, class D>
  derivable_class_(const derivable_class_<A, B, C, D> &);

public:
  typedef boost::mpl::vector4<X1, X2, X3, detail::unspecified> parameters_type;

// WrappedType MUST inherit from T
#if 0
  typedef typename detail::extract_parameter<
    parameters_type
    , boost::is_base_and_derived<T, boost::mpl::_>
    , reg::null_type
    >::type WrappedType;
#endif
  typedef T WrappedType;

  typedef typename detail::extract_parameter<
    parameters_type, boost::mpl::not_<
                       boost::mpl::or_<
                         detail::is_bases<boost::mpl::_>, boost::is_base_and_derived<boost::mpl::_, T>, boost::is_base_and_derived<T, boost::mpl::_>>>,
    T * // Default is to put the pointer into a T*
    >::type HeldType;

  template <class Src, class Target>
  void add_downcast(Src *, Target *, boost::mpl::true_) {
    add_cast(
             reg::registered_class<Src>::id, reg::registered_class<Target>::id, detail::dynamic_cast_<Src, Target>::execute);
  }

  template <class Src, class Target>
  void add_downcast(Src *, Target *, boost::mpl::false_) {}

  // this function generates conversion information
  // in the given class_rep structure. It will be able
  // to implicitly cast to the given template type
  // Dummy return value
  template <class To>
  int gen_base_info(detail::type_<To>) {
    add_base(typeid(To), detail::static_cast_<T, To>::execute);
    add_cast(
             reg::registered_class<T>::id, reg::registered_class<To>::id, detail::static_cast_<T, To>::execute);

    add_downcast((To *)0, (T *)0, boost::is_polymorphic<To>());
    return 0;
  }

  int gen_base_info(detail::type_<reg::null_type>) { return 0; }

#define CLBIND_GEN_BASE_INFO(z, n, text) gen_base_info(detail::type_<BaseClass##n>());

  template <class... BaseClass>                                    // BOOST_PP_ENUM_PARAMS(CLBIND_MAX_BASES, class BaseClass)>
  void generate_baseclass_list(detail::type_<bases<BaseClass...>>) // BOOST_PP_ENUM_PARAMS(CLBIND_MAX_BASES, BaseClass)> >)
  {
#pragma clang diagnostic push
#pragma clang diagnostic ignored "-Wunused-variable"
    int dummy[sizeof...(BaseClass)] = {(gen_base_info(detail::type_<BaseClass>()))...};
#pragma clang diagnostic pop
    //            BOOST_PP_REPEAT(CLBIND_MAX_BASES, CLBIND_GEN_BASE_INFO, _)
  }

#undef CLBIND_GEN_BASE_INFO

  derivable_class_(scope_& outer_scope, const char *name, default_constructor_type) : derivable_class_base(name), _outer_scope(&outer_scope), scope(*this) {
#ifndef NDEBUG
//            detail::check_link_compatibility();
#endif
    init(true /* has constructor */);
    // I have a constructor and I can test isDerivableCxxClass<T>(0)
    // I should dispatch to def_derivable_default_constructor
    this->def_default_constructor_("default_ctor", NULL, policies<>(), "", "", "");
    validateRackOffset(offsetof(WrappedType,_Rack));
    this->_outer_scope->operator,(*this);
  }
  
  derivable_class_(scope_& outer_scope, const char *name) : derivable_class_base(name), scope(*this) {
#ifndef NDEBUG
//            detail::check_link_compatibility();
#endif
    init(false /* Does not have constructor */);
    validateRackOffset(offsetof(WrappedType,_Rack));
  }

  template <typename... Types>
  derivable_class_ &def_constructor(const string &name,
                                    constructor<Types...> sig,
                                    string const &arguments = "",
                                    string const &declares = "",
                                    string const &docstring = "") {
    if (isDerivableCxxClass<T>(0)) {
      THROW_HARD_ERROR(BF("ERROR - The derivable class %s should not have other constructors") % this->name());
    }
    return this->def_constructor_(name, &sig, policies<>(), arguments, declares, docstring);
  }

  template <typename... Types, class Policies>
  derivable_class_ &def_constructor(const string &name,
                                    constructor<Types...> sig,
                                    const Policies &policies,
                                    string const &arguments = "",
                                    string const &declares = "",
                                    string const &docstring = "") {
    if (isDerivableCxxClass<T>(0)) {
      THROW_HARD_ERROR(BF("ERROR - The derivable class %s should not have other constructors with policies") % this->name());
    }
    return this->def_constructor_(name, &sig, policies, arguments, declares, docstring);
  }

  template <class F>
  derivable_class_ &def(const char *name, F f,
                        string const& docstring="",
                        string const& arguments="",
                        string const& declares="")
  {
    return this->virtual_def(
                             name, f, policies<>(), reg::null_type(), boost::mpl::true_(), "", "", "");

    //                , arguments, declares, docstring);
  }

  // virtual functions
  template <class F, class DefaultOrPolicies>
  derivable_class_ &def(char const *name, F fn, DefaultOrPolicies default_or_policies, string const &docstring = "", string const &arguments = "", string const &declares = "") {
    return this->virtual_def(
                             name, fn, default_or_policies, reg::null_type(), typename is_policy_list<DefaultOrPolicies>::type(), arguments, declares, docstring);
  }

  template <class Getter>
  derivable_class_ &property(const char *name, Getter g, string const &arguments = "", string const &declares = "", string const &docstring = "") {
    this->add_member(
                     new detail::property_registration<T, Getter, reg::null_type>(
                                                                                  name // name
                                                                                  ,
                                                                                  g // Get
                                                                                  ,
                                                                                  reg::null_type() // GetPolicies
                                                                                  ,
                                                                                  reg::null_type(), reg::null_type(), arguments, declares, docstring));
    return *this;
  }

  template <class Begin, class End>
  derivable_class_ &iterator(const char *iteratorName, Begin beginFn, End endFn, string const &arguments = "", string const &declares = "", string const &docstring = "") {
    this->add_member(
                     new detail::iterator_registration<T, Begin, End, reg::null_type>(
                                                                                      iteratorName // name
                                                                                      ,
                                                                                      beginFn // begin
                                                                                      ,
                                                                                      endFn // end
                                                                                      ,
                                                                                      reg::null_type() // null policies
                                                                                      ,
                                                                                      arguments, declares, docstring));
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

#endif // end_meister_disabled

#if 0
  enum_maker enum_(core::Symbol_sp converter) {
    return enum_maker(this, converter);
  }
#endif
  scope_* _outer_scope;
  detail::static_scope<self_t> scope;

private:
  void operator=(derivable_class_ const &);

  void add_wrapper_cast(reg::null_type *) {}

  template <class U>
  void add_wrapper_cast(U *) {
    add_cast(
             reg::registered_class<U>::id, reg::registered_class<T>::id, detail::static_cast_<U, T>::execute);

    add_downcast((T *)0, (U *)0, boost::is_polymorphic<T>());
  }

  void init(bool hasConstructor) {
    typedef typename detail::extract_parameter<
      parameters_type, boost::mpl::or_<
        detail::is_bases<boost::mpl::_>, boost::is_base_and_derived<boost::mpl::_, T>>,
      no_bases>::type bases_t;

    typedef typename boost::mpl::if_<detail::is_bases<bases_t>, bases_t, bases<bases_t>>::type Base;

    if (!hasConstructor && isDerivableCxxClass<T>(0)) {
      THROW_HARD_ERROR(BF("ERROR - The derivable class %s must have a default constructor"
                          " - otherwise how can you create instances of their derived classes?\n") %
                       this->name());
    }
    derivable_class_base::init(
                               typeid(T), reg::registered_class<T>::id, typeid(WrappedType), reg::registered_class<WrappedType>::id, isDerivableCxxClass<T>(0));

    //            printf("%s:%d Should I be adding a wrapper cast???\n", __FILE__, __LINE__ );
    add_wrapper_cast((WrappedType *)0);

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
  derivable_class_& property_impl(const char* name,
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
  derivable_class_& virtual_def(char const* name, F const& fn
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
  template <class F, class Policies>
  derivable_class_ &virtual_def(char const *name, F const &fn, Policies const &, reg::null_type, boost::mpl::true_,
                                string const &arguments, string const &declares, string const &docstring) {
    this->add_member(
                     new detail::memfun_registration<T, F, Policies>(
                                                                     name, fn, Policies(), arguments, declares, docstring));
    return *this;
  }

  template <class Policies>
  derivable_class_ &def_default_constructor_(const char *name, void *, Policies const &, string const &arguments, string const &declares, string const &docstring) {
    typedef T construct_type;
    this->set_default_constructor(new detail::constructor_registration<
                                  construct_type,
                                  reg::null_type,
                                  default_constructor,
                                  Policies,
                                  detail::construct_derivable_class>(Policies(),
                                                                     name,
                                                                     arguments,
                                                                     declares,
                                                                     docstring));
    return *this;
  }

  template <class Signature, class Policies>
  derivable_class_ &def_constructor_(const string &name, Signature *, Policies const &, string const &arguments, string const &declares, string const &docstring) {
    typedef Signature signature;

    typedef typename boost::mpl::if_<
      boost::is_same<WrappedType, reg::null_type>, T, WrappedType>::type construct_type;

    this->add_member(
                     new detail::constructor_registration<
                     construct_type, HeldType, signature, Policies, detail::construct_derivable_class>(
                                                                                                       Policies(), name, arguments, declares, docstring));

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
