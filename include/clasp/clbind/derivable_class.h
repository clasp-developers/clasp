#pragma once

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

// #include <clasp/clbind/prefix.hpp>
#include <clasp/clbind/config.h>

#include <utility>
#include <string>
#include <map>
#include <vector>
#include <cassert>

#include <clasp/core/foundation.h>
#include <clasp/core/instance.h>
#include <clasp/clbind/config.h>
#include <clasp/clbind/function.h>
#include <clasp/clbind/scope.h>
#include <clasp/clbind/class.h>
#include <clasp/clbind/primitives.h>
#include <clasp/clbind/enum_maker.h>
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

template <class T> class shared_ptr;

} // namespace boost

namespace clbind {

namespace detail {

struct derivable_class_registration;

struct derivable_class_registration : registration {
  derivable_class_registration(char const* name, type_id const& type_id_,
                               class_id id, type_id const& wrapper_type,
                               class_id wrapper_id, bool derivable);

  void register_() const;

  const char* m_name;

  virtual std::string name() const { return this->m_name; }
  virtual std::string kind() const { return "derivable_class_registration"; };

  mutable std::map<const char*, int, detail::ltstr> m_static_constants;

  typedef std::pair<type_id, cast_function> base_desc;
  mutable std::vector<base_desc> m_bases;

  type_id m_type;
  class_id m_id;
  class_id m_wrapper_id;
  type_id m_wrapper_type;
  std::vector<cast_entry> m_casts;

  scope_ m_scope;
  scope_ m_members;
  detail::registration* m_default_constructor;
  scope_ m_default_members;
  bool m_derivable;
};

struct CLBIND_API derivable_class_base : scope_ {
public:
  derivable_class_base(char const* name, type_id const& type_id_, class_id id,
                       type_id const& wrapper_type, class_id wrapper_id,
                       bool derivable);

  struct base_desc {
    type_id type;
    int ptr_offset;
  };

  void add_base(type_id const& base, cast_function cast);

  void set_default_constructor(registration* member);
  void add_member(registration* member);
  void add_default_member(registration* member);

  const char* name() const;

  void add_static_constant(const char* name, int val);
  void add_inner_scope(scope_& s);

  void add_cast(class_id src, class_id target, cast_function cast);

private:
  derivable_class_registration* m_registration;

public:
  int m_init_counter;
};

/*! This is the constructor registration for default constructors of non derivable classes,
         Specialized by making second template parameter reg::null_type
        */
template <class Class, class Policies>
struct constructor_registration<Class, reg::null_type, default_constructor, Policies, construct_derivable_class>
    : public constructor_registration_base<Class, reg::null_type, default_constructor, Policies> {
  constructor_registration(Policies const& policies, string const& name, string const& arguments, string const& declares,
                           string const& docstring)
      : constructor_registration_base<Class, reg::null_type, default_constructor, Policies>(policies, name, arguments, declares,
                                                                                            docstring),
        mm_name(name){};
  core::Creator_sp registerDefaultConstructor_() const {
    //                printf("%s:%d In constructor_registration::registerDefaultConstructor derivable_default_constructor<> -----
    //                Make sure that I'm being called for derivable classes\n", __FILE__, __LINE__ );
    core::SimpleFun_sp entryPoint =
        core::makeSimpleFunAndFunctionDescription<DerivableDefaultConstructorCreator_O<Class>>(nil<core::T_O>());
    return gc::As_unsafe<core::Creator_sp>(gctools::GC<DerivableDefaultConstructorCreator_O<Class>>::allocate(entryPoint));
  }
  virtual std::string name() const { return this->mm_name; }
  virtual std::string kind() const { return "derivable constructor_registration"; };
  std::string mm_name;
};

} // namespace detail

void validateRackOffset(size_t wrapped_type_offset);

// registers a class in the cl environment
template <class T, class Base = no_bases> struct derivable_class_ : detail::derivable_class_base {
  typedef derivable_class_<T, Base> self_t;

private:
  template <class A, class B> derivable_class_(const derivable_class_<A, B>&);

public:
  // WrappedType MUST inherit from T
  typedef T WrappedType;

  typedef std::unique_ptr<T> HoldType;

  template <class Src, class Target> void add_downcast() {
    if constexpr (std::is_polymorphic_v<Src>) {
      add_cast(reg::registered_class<Src>::id, reg::registered_class<Target>::id, detail::dynamic_cast_<Src, Target>::execute);
    }
  }

  // this function generates conversion information
  // in the given class_rep structure. It will be able
  // to implicitly cast to the given template type
  template <class To> void gen_base_info() {
    if constexpr(!std::is_same_v<To, reg::null_type>) {
      add_base(typeid(To), detail::static_cast_<T, To>::execute);
      add_cast(reg::registered_class<T>::id, reg::registered_class<To>::id, detail::static_cast_<T, To>::execute);

      add_downcast<To, T>();
    }
  }

  template <class... BaseClass> void generate_baseclass_list(bases<BaseClass...>) {
    (gen_base_info<BaseClass>(), ...);
  }

  derivable_class_(scope_& outer_scope, const char* name, default_constructor_type, const char* docstring = "")
    : derivable_class_base(name, typeid(T), reg::registered_class<T>::id,
                           typeid(WrappedType), reg::registered_class<WrappedType>::id,
                           isDerivableCxxClass<T>(0)),
      _outer_scope(&outer_scope), scope(*this) {
#ifndef NDEBUG
//            detail::check_link_compatibility();
#endif
    init(true /* has constructor */);
    // I have a constructor and I can test isDerivableCxxClass<T>(0)
    // I should dispatch to def_derivable_default_constructor
    this->def_default_constructor_("default_ctor", NULL, policies<>(), "", "", "");
    validateRackOffset(offsetof(WrappedType, _Rack));
    this->_outer_scope->operator,(*this);
  }

  derivable_class_(scope_& outer_scope, const char* name, const char* docstring = "")
    : derivable_class_base(name, typeid(T), reg::registered_class<T>::id,
                           typeid(WrappedType), reg::registered_class<WrappedType>::id,
                           isDerivableCxxClass<T>(0)),
      scope(*this)
  {
#ifndef NDEBUG
//            detail::check_link_compatibility();
#endif
    init(false /* Does not have constructor */);
    //    validateRackOffset(offsetof(WrappedType,_Rack));
  }

  template <typename... Types>
  derivable_class_& def_constructor(const string& name, constructor<Types...> sig, string const& arguments = "",
                                    string const& declares = "", string const& docstring = "") {
    if (isDerivableCxxClass<T>(0)) {
      THROW_HARD_ERROR("ERROR - The derivable class %s should not have other constructors", this->name());
    }
    return this->def_constructor_(name, &sig, policies<>(), arguments, declares, docstring);
  }

  template <typename... Types, class Policies>
  derivable_class_& def_constructor(const string& name, constructor<Types...> sig, const Policies& policies,
                                    string const& arguments = "", string const& declares = "", string const& docstring = "") {
    if (isDerivableCxxClass<T>(0)) {
      THROW_HARD_ERROR("ERROR - The derivable class %s should not have other constructors with policies", this->name());
    }
    return this->def_constructor_(name, &sig, policies, arguments, declares, docstring);
  }

  template <class F, class... PTypes> derivable_class_& def(const char* name, F f, PTypes... pols) {
    typedef policies<PTypes...> Policies;
    Policies curPolicies;
    maybe_register_symbol_using_dladdr(*(void**)&f, sizeof(f), name);
    walk_policy(curPolicies, pols...);
    return this->virtual_def(PrepareName(name), f, curPolicies, reg::null_type());
  }

  template <class Getter>
  derivable_class_& property(const char* name, Getter g, string const& arguments = "", string const& declares = "",
                             string const& docstring = "") {
    this->add_member(new detail::property_registration<T, Getter, reg::null_type>(name // name
                                                                                  ,
                                                                                  g // Get
                                                                                  ,
                                                                                  reg::null_type() // GetPolicies
                                                                                  ,
                                                                                  reg::null_type(), reg::null_type(), arguments,
                                                                                  declares, docstring));
    return *this;
  }

  template <class Begin, class End>
  derivable_class_& iterator(const char* iteratorName, Begin beginFn, End endFn, string const& arguments = "",
                             string const& declares = "", string const& docstring = "") {
    this->add_member(new detail::iterator_registration<T, Begin, End, reg::null_type>(iteratorName // name
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

  scope_* _outer_scope;
  detail::static_scope<self_t> scope;

private:
  void operator=(derivable_class_ const&);

  template <class U> void add_wrapper_cast() {
    if (!std::is_same_v<U, reg::null_type>) {
      add_cast(reg::registered_class<U>::id, reg::registered_class<T>::id, detail::static_cast_<U, T>::execute);

      add_downcast<T, U>();
    }
  }

  void init(bool hasConstructor) {
    typedef std::conditional_t<detail::is_bases<Base>::value, Base, bases<Base>> bases_t;

    if (!hasConstructor && isDerivableCxxClass<T>(0)) {
      THROW_HARD_ERROR("ERROR - The derivable class %s must have a default constructor"
                       " - otherwise how can you create instances of their derived classes?\n",
                       this->name());
    }

    //            printf("%s:%d Should I be adding a wrapper cast???\n", __FILE__, __LINE__ );
    add_wrapper_cast<WrappedType>();

    generate_baseclass_list(bases_t());
  }

  // these handle default implementation of virtual functions
  template <class F, class Policies>
  derivable_class_& virtual_def(const std::string& name, F const& fn, Policies const& policies, reg::null_type) {
    maybe_register_symbol_using_dladdr(*(void**)&fn, sizeof(fn), name);
    this->add_member(new detail::memfun_registration<T, F, Policies>(name, fn, policies));
    return *this;
  }

  template <class Policies>
  derivable_class_& def_default_constructor_(const char* name, void*, Policies const&, string const& arguments,
                                             string const& declares, string const& docstring) {
    typedef T construct_type;
    this->set_default_constructor(
        new detail::constructor_registration<construct_type, reg::null_type, default_constructor, Policies,
                                             detail::construct_derivable_class>(Policies(), name, arguments, declares, docstring));
    return *this;
  }

  template <class Signature, class Policies>
  derivable_class_& def_constructor_(const string& name, Signature*, Policies const&, string const& arguments,
                                     string const& declares, string const& docstring) {
    typedef Signature signature;

    typedef std::conditional_t<std::is_same_v<WrappedType, reg::null_type>, T, WrappedType> construct_type;

    this->add_member(
        new detail::constructor_registration<construct_type, HoldType, signature, Policies, detail::construct_derivable_class>(
            Policies(), name, arguments, declares, docstring));

#if 0
    this->add_default_member(
                             new detail::constructor_registration<
                             construct_type, HoldType, signature, Policies>(
                                                                            Policies(),name,arguments,declares,docstring));
#endif
    return *this;
  }

public:
  // static functions
  template <typename... Types> derivable_class_& def(constructor<Types...> sig) {
    printf("%s:%d def(expose::init...)\n", __FILE__, __LINE__);
    stringstream ss;
    ss << "make-";
    ss << this->name();
    if (this->m_init_counter) {
      ss << this->m_init_counter;
    }
    maybe_register_symbol_using_dladdr((void*)sig, sizeof(sig), ss.str());
    this->def_constructor_(ss.str(), &sig, policies<>(), "", "", "");
    this->m_init_counter++;
    return *this;
  }
};
} // namespace clbind

#ifdef _MSC_VER
#pragma warning(pop)
#endif
