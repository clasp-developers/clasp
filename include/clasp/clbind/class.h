#pragma once

/*
    File: class.h
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

// #include <clbind/prefix.hpp>
#include <clasp/clbind/config.h>

#include <utility>
#include <string>
#include <map>
#include <vector>
#include <cassert>

#include <clasp/clbind/config.h>
#include <clasp/clbind/names.h>
#include <clasp/clbind/scope.h>
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

class DummyCreator_O : public core::Creator_O {
  LISP_CLASS(clbind, ClbindPkg, DummyCreator_O, "DummyCreator", core::Creator_O);
  core::T_sp _name;

public:
  DummyCreator_O(core::SimpleFun_sp ep, core::T_sp name) : core::Creator_O(ep), _name(name){};

public:
  virtual size_t templatedSizeof() const override { return sizeof(*this); };
  virtual bool allocates() const override { return false; };
  virtual core::T_sp creator_allocate() override {
    SIMPLE_ERROR("This class named: {} cannot allocate instances", core::_rep_(this->_name));
  } // return _Nil<core::T_O>(); };
  core::Creator_sp duplicateForClassName(core::Symbol_sp className) override {
    core::SimpleFun_sp entryPoint =
        core::makeSimpleFunAndFunctionDescription<DummyCreator_O>(nil<T_O>());
    return gc::GC<DummyCreator_O>::allocate(entryPoint, className);
  }
};

namespace detail {

template <class Derived> struct operator_;

} // namespace detail

extern constructor<> globalDefaultConstructorSignature;

template <typename... Bases> struct bases {};

typedef bases<> no_bases;

namespace detail {

template <class T> struct is_bases : std::false_type {};

template <typename... Bases> struct is_bases<bases<Bases...>> : std::true_type {};

struct CLBIND_API create_class {
  static int stage1();
  static int stage2();
};

} // namespace detail

}; // namespace clbind

namespace clbind {
namespace detail {

template <class T> struct static_scope {
  static_scope(T& self_) : self(self_) {}

  T& operator[](scope_ s) const {
    self.add_inner_scope(s);
    return self;
  }

private:
  template <class U> void operator,(U const&) const;
  void operator=(static_scope const&);

  T& self;
};

struct class_registration;
namespace {
struct cast_entry {
  cast_entry(class_id src, class_id target, cast_function cast) : src(src), target(target), cast(cast) {}

  class_id src;
  class_id target;
  cast_function cast;
};

} // namespace

struct class_registration : registration {
  class_registration(const string& name);

  void register_() const;

  std::string m_name;

  virtual std::string name() const { return this->m_name; }
  virtual std::string kind() const { return "class_registration"; };

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

struct CLBIND_API class_base : scope_ {
public:
  class_base(const string& name);

  struct base_desc {
    type_id type;
    int ptr_offset;
  };

  void init(type_id const& type, class_id id, type_id const& wrapped_type, class_id wrapper_id, bool derivable);

  void add_base(type_id const& base, cast_function cast);

  void set_default_constructor(registration* member);
  void add_member(registration* member);
  void add_default_member(registration* member);

  string name() const;

  void add_static_constant(const char* name, int val);
  void add_inner_scope(scope_& s);

  void add_cast(class_id src, class_id target, cast_function cast);

private:
  class_registration* m_registration;

public:
  int m_init_counter;
};

// MSVC complains about member being sensitive to alignment (C4121)
// when F is a pointer to member of a class with virtual bases.
#ifdef BOOST_MSVC
#pragma pack(push)
#pragma pack(16)
#endif

template <typename MethodPointerType> struct CountMethodArguments {
  //            enum {value = 0 };
};

template <typename RT, typename OT, typename... ARGS> struct CountMethodArguments<RT (OT::*)(ARGS...)> {
  enum { value = sizeof...(ARGS) };
};

template <typename RT, typename OT, typename... ARGS> struct CountMethodArguments<RT (OT::*)(ARGS...) const> {
  enum { value = sizeof...(ARGS) };
};

template <class Class, typename MethodPointerType, class Policies> struct memfun_registration : registration {
  memfun_registration(const std::string& name, MethodPointerType f, Policies const& policies)
      : m_name(name), methodPtr(f), policies(policies) {
    this->m_arguments = policies.lambdaList();
    this->m_doc_string = policies.docstring();
    this->m_declares = policies.declares();
    this->m_auto_export = policies.autoExport();
  }

  void register_() const {
    LOG_SCOPE(("%s:%d register_ %s/%s\n", __FILE__, __LINE__, this->kind().c_str(), this->name().c_str()));
    core::Symbol_sp classSymbol = reg::lisp_classSymbol<Class>();
    core::Symbol_sp symbol = core::lisp_intern(m_name, symbol_packageName(classSymbol));
    using VariadicType = WRAPPER_AlienVariadicMethod<MethodPointerType, Policies>;
    core::FunctionDescription_sp fdesc = makeFunctionDescription(symbol, nil<core::T_O>());
    auto entry = gc::GC<VariadicType>::allocate(methodPtr, fdesc, nil<core::T_O>());
    lisp_defineSingleDispatchMethod(symbol, classSymbol, entry, 0, true, m_arguments, m_declares,
                                    m_doc_string, m_auto_export,
                                    CountMethodArguments<MethodPointerType>::value + 1, // +1 for the self argument
                                    GatherPureOutValues<Policies, 0>::gather());
  }

  virtual std::string name() const { return this->m_name; };
  virtual std::string kind() const { return "memfun_registration"; };

  std::string m_name;
  MethodPointerType methodPtr;
  Policies policies;
  string m_arguments;
  string m_declares;
  string m_doc_string;
  bool m_auto_export;
};

template <class Class, class Begin, class End, class Policies> struct iterator_registration : registration {
  iterator_registration(const string& name, Begin begin, End end, Policies const& policies, string const& arguments,
                        string const& declares, string const& docstring)
      : m_name(name), beginPtr(begin), endPtr(end), policies(policies), m_arguments(arguments), m_declares(declares),
        m_doc_string(docstring) {}

  void register_() const {
    LOG_SCOPE(("%s:%d register_ %s/%s\n", __FILE__, __LINE__, this->kind().c_str(), this->name().c_str()));
    core::Symbol_sp classSymbol = reg::lisp_classSymbol<Class>();
    core::Symbol_sp symbol = core::lisp_intern(m_name, symbol_packageName(classSymbol));
    using VariadicType = WRAPPER_Iterator<Policies, Class, Begin, End>;
    core::FunctionDescription_sp fdesc = makeFunctionDescription(symbol, nil<core::T_O>());
    auto entry = gc::GC<VariadicType>::allocate(fdesc, nil<core::T_O>(), beginPtr, endPtr);
    lisp_defineSingleDispatchMethod(symbol, classSymbol, entry, 0, true, m_arguments, m_declares,
                                    m_doc_string, true, 1); // one argument required for iterator - the object that has the sequence
  }

  virtual std::string name() const { return this->m_name; };
  virtual std::string kind() const { return "iterator_registration"; };

  string m_name;
  Begin beginPtr;
  End endPtr;
  Policies policies;
  string m_arguments;
  string m_declares;
  string m_doc_string;
};

#ifdef BOOST_MSVC
#pragma pack(pop)
#endif

template <typename ConstructorType> struct CountConstructorArguments {
  enum { value = 0 };
};

template <typename... ARGS> struct CountConstructorArguments<constructor<ARGS...>> {
  enum { value = sizeof...(ARGS) };
};

template <class Class, class Pointer, class Signature, class Policies> struct constructor_registration_base : public registration {
  constructor_registration_base(Policies const& policies, string const& name, string const& arguments, string const& declares,
                                string const& docstring)
      : policies(policies), m_name(name), m_arguments(arguments), m_declares(declares), m_doc_string(docstring) {}

  void register_() const {
    LOG_SCOPE(("%s:%d register_ %s/%s\n", __FILE__, __LINE__, this->kind().c_str(), this->name().c_str()));
    string tname = m_name;
    if (m_name == "") {
      tname = "DEFAULT-CTOR";
    };
    //                printf("%s:%d    constructor_registration_base::register_ called for %s\n", __FILE__, __LINE__,
    //                m_name.c_str());
    core::Symbol_sp sym = core::lisp_intern(tname, core::lisp_currentPackageName());
    using VariadicType = WRAPPER_Constructor_O<Signature, Policies, Pointer, Class>;
    core::FunctionDescription_sp fdesc = makeFunctionDescription(sym, nil<core::T_O>());
    auto entry = gctools::GC<VariadicType>::allocate(fdesc);
    lisp_bytecode_defun(core::symbol_function, sym, core::lisp_currentPackageName(), entry,
                        m_arguments, m_declares, m_doc_string, "=external=", 0, CountConstructorArguments<Signature>::value);
  }
  virtual std::string name() const { return this->m_name; }
  virtual std::string kind() const { return "constructor_registration_base"; };

  Policies policies;
  string m_name;
  string m_arguments;
  string m_declares;
  string m_doc_string;
};

/*! constructor_registration can construct either a derivable class or a non-derivable class */

class construct_non_derivable_class {};
class construct_derivable_class {};

template <class Class, class HoldType, class Signature, class Policies, class DerivableType>
struct constructor_registration : public constructor_registration_base<Class, HoldType, Signature, Policies> {
  typedef constructor_registration_base<Class, HoldType, Signature, Policies> Base;
  constructor_registration(Policies const& policies, string const& name, string const& arguments, string const& declares,
                           string const& docstring)
      : constructor_registration_base<Class, HoldType, Signature, Policies>(policies, name, arguments, declares, docstring){};
};

/*! This is the constructor registration for default constructors */
template <class Class, class HoldType, class Policies>
struct constructor_registration<Class, HoldType, default_constructor, Policies, construct_non_derivable_class>
    : public constructor_registration_base<Class, HoldType, default_constructor, Policies> {
  constructor_registration(Policies const& policies, string const& name, string const& arguments, string const& declares,
                           string const& docstring)
      : constructor_registration_base<Class, HoldType, default_constructor, Policies>(policies, name, arguments, declares,
                                                                                      docstring){};
  core::Creator_sp registerDefaultConstructor_() const {
    core::SimpleFun_sp ep = core::makeSimpleFunAndFunctionDescription<DefaultConstructorCreator_O<Class, HoldType>>(
        nil<core::T_O>());
    core::Creator_sp allocator = gc::As<core::Creator_sp>(gc::GC<DefaultConstructorCreator_O<Class, HoldType>>::allocate(ep));
    return allocator;
  }
};

/*!
 * Derivable classes require constructors that use the garbage collector
 * Specialize constructor_registration_base so that it's register_ function
 * instantiates a constructor functoid that uses the garbage collector
 */
template <class Class, class Policies>
struct constructor_registration_base<Class, reg::null_type, default_constructor, Policies> : public registration {
  constructor_registration_base(Policies const& policies, string const& name, string const& arguments, string const& declares,
                                string const& docstring)
      : policies(policies), m_name(name), m_arguments(arguments), m_declares(declares), m_doc_string(docstring) {}

  void register_() const { HARD_IMPLEMENT_MEF("Do I use this code?"); }

  Policies policies;
  string m_name;
  string m_arguments;
  string m_declares;
  string m_doc_string;
};

/*! This is the constructor registration for default constructors of non derivable classes,
         Specialized by making second template parameter reg::null_type
        */
template <class Class, class Policies>
struct constructor_registration<Class, reg::null_type, default_constructor, Policies, construct_non_derivable_class>
    : public constructor_registration_base<Class, reg::null_type, default_constructor, Policies> {
  constructor_registration(Policies const& policies, string const& name, string const& arguments, string const& declares,
                           string const& docstring)
      : constructor_registration_base<Class, reg::null_type, default_constructor, Policies>(policies, name, arguments, declares,
                                                                                            docstring){};
  core::Creator_sp registerDefaultConstructor_() const {
    //                printf("%s:%d In constructor_registration::registerDefaultConstructor derivable_default_constructor<> -----
    //                Make sure that I'm being called for derivable classes\n", __FILE__, __LINE__ );
    //    return gctools::GC<DerivableDefaultConstructorCreator_O<Class>>::allocate();
    return gctools::GC<DefaultConstructorCreator_O<Class, Class*>>::allocate();
  }
};

template <class Class, class Get, class GetPolicies, class Set = reg::null_type, class SetPolicies = reg::null_type>
struct property_registration : registration {
  property_registration(const string& name, Get const& get, GetPolicies const& get_policies, Set const& set = Set(),
                        SetPolicies const& set_policies = SetPolicies(), string const& arguments = "", string const& declares = "",
                        string const& docstring = "")
      : m_name(name), get(get), get_policies(get_policies), set(set), set_policies(set_policies), m_arguments(arguments),
        m_declares(declares), m_doc_string(docstring) {}

  void register_() const {
    LOG_SCOPE(("%s:%d class_ register_ %s\n", __FILE__, __LINE__, this->m_name.c_str()));
    const string n(m_name);
    core::Symbol_sp classSymbol = reg::lisp_classSymbol<Class>();
    core::Symbol_sp sym = core::lisp_intern(n, symbol_packageName(classSymbol));
    using VariadicGetterType = WRAPPER_Getter<reg::null_type, Class, Get>;
    core::SimpleFun_sp entryPoint = makeSimpleFunAndFunctionDescription<VariadicGetterType>(sym);
    maybe_register_symbol_using_dladdr((void*)VariadicGetterType::entry_point);
    auto raw_getter = gc::GC<VariadicGetterType>::allocate(entryPoint, get);
    core::BuiltinClosure_sp getter = gc::As<core::BuiltinClosure_sp>(raw_getter);
    lisp_defineSingleDispatchMethod(sym, classSymbol, getter, 0, true, m_arguments, m_declares,
                                    m_doc_string, true, 1);
    core::T_sp setf_name = core::Cons_O::createList(cl::_sym_setf, sym);
    using VariadicSetterType = WRAPPER_Setter<reg::null_type, Class, Set>;
    core::SimpleFun_sp setterEntryPoint =
        makeSimpleFunAndFunctionDescription<VariadicSetterType>(setf_name);
    maybe_register_symbol_using_dladdr((void*)VariadicSetterType::entry_point);
    auto raw_setter = gc::GC<VariadicSetterType>::allocate(setterEntryPoint, set);
    core::BuiltinClosure_sp setter = gc::As<core::BuiltinClosure_sp>(raw_setter);
    lisp_defineSingleDispatchMethod(setf_name, classSymbol, setter, 1, true, m_arguments, m_declares,
                                    m_doc_string, true, 2);
    //                printf("%s:%d - allocated a getter@%p for %s\n", __FILE__, __LINE__, getter, name);
    // register the getter here
  }
  virtual std::string name() const { return this->m_name; }
  virtual std::string kind() const { return "property_registration"; };
  std::string m_name;
  Get get;
  GetPolicies get_policies;
  Set set;
  SetPolicies set_policies;
  string m_arguments;
  string m_declares;
  string m_doc_string;
};

template <class Class, class Get, class GetPolicies>
struct property_registration<Class, Get, GetPolicies, reg::null_type, reg::null_type> : registration {
  property_registration(const string& name, Get const& get, GetPolicies const& get_policies,
                        reg::null_type const& set = reg::null_type(), reg::null_type const& set_policies = reg::null_type(),
                        string const& arguments = "", string const& declares = "", string const& docstring = "")
      : m_name(name), get(get), get_policies(get_policies), m_arguments(arguments), m_declares(declares), m_doc_string(docstring) {}

  void register_() const {
    LOG_SCOPE(("%s:%d register_ %s/%s\n", __FILE__, __LINE__, this->kind().c_str(), this->name().c_str()));
    const string n(m_name);
    core::Symbol_sp classSymbol = reg::lisp_classSymbol<Class>();
    core::Symbol_sp symbol = core::lisp_intern(n, symbol_packageName(classSymbol));
    using VariadicType = WRAPPER_Getter<reg::null_type, Class, Get>;
    core::FunctionDescription_sp fdesc = makeFunctionDescription(symbol, nil<core::T_O>());
    auto entry = gc::GC<VariadicType>::allocate(get, fdesc, nil<core::T_O>());
    lisp_defineSingleDispatchMethod(symbol, classSymbol, entry, 0, true, m_arguments, m_declares,
                                    m_doc_string, true, 1);
  }
  virtual std::string name() const { return this->m_name; }
  virtual std::string kind() const { return "property_registration"; };
  std::string m_name;
  Get get;
  GetPolicies get_policies;
  string m_arguments;
  string m_declares;
  string m_doc_string;
};

} // namespace detail

// registers a class in the cl environment
template <class T, class Base = no_bases, class WrappedType = reg::null_type> struct class_ : detail::class_base {
  typedef class_<T, Base> self_t;

private:
  template <class A, class B> class_(const class_<A, B>&);

public:
  static_assert(std::is_same_v<WrappedType, reg::null_type> || std::is_base_of_v<T, WrappedType>,
                "If provided, WrappedType must inherit from T");

  // TODO: Assert that if Base is a bases<...>, everything in it is a base of T.
  static_assert(detail::is_bases<Base>::value || std::is_base_of_v<Base, T>, "If provided, Base must be bases of T");

  typedef std::unique_ptr<T> HoldType;

  template <class Src, class Target> void add_downcast() {
    // We use if constexpr. This will discard the add_cast when Src is not
    // polymorphic, which is important as add_cast would not be instantiable.
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
    // Fold expression to call gen_base_info for each base.
    (gen_base_info<BaseClass>(), ...);
  }

  template <typename NameType>
  class_(scope_& outer_scope, const NameType& name, const std::string& docstring = "")
      : class_base(PrepareName(name)), _outer_scope(&outer_scope), scope(*this) {
#ifndef NDEBUG
    detail::check_link_compatibility();
#endif
    init();
    this->_outer_scope->operator,(*this);
  }

  template <typename NameType, typename... Types>
  class_& def_constructor(const NameType& name, constructor<Types...> sig, string const& arguments = "",
                          string const& declares = "", string const& docstring = "") {
    return this->def_constructor_(PrepareName(name), &sig, policies<adopt<result>>(), arguments, declares, docstring);
  }

  template <typename NameType, class F, class... PTypes> class_& def(const NameType& name, F f, PTypes... pols) {
    typedef policies<PTypes...> Policies;
    Policies curPolicies;
    walk_policy(curPolicies, pols...);
    return this->virtual_def(PrepareName(name), f, curPolicies, reg::null_type());
  }

  // static functions
  template <typename NameType, class F, class Policies>
  class_& def_static(const char* name, F fn, string const& docstring = "", string const& arguments = "",
                     string const& declares = "", Policies policies = Policies()) {

    this->scope_::def(name, fn, policies, docstring, arguments, declares);
    return *this;
  }
  // static functions
  template <typename NameType, class F>
  class_& def_static(const NameType& name, F fn, string const& docstring = "", string const& arguments = "",
                     string const& declares = "") {
    this->scope_::def(name, fn, docstring.c_str(), arguments.c_str(), declares.c_str());
    return *this;
  }

  // static functions
  template <typename... Types> class_& def(constructor<Types...> sig) {
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

  template <typename NameType, class Getter>
  class_& property(const NameType& name, Getter g, string const& arguments = "", string const& declares = "",
                   string const& docstring = "") {
    this->add_member(new detail::property_registration<T, Getter, reg::null_type>(PrepareName(name) // name
                                                                                  ,
                                                                                  g // Get
                                                                                  ,
                                                                                  reg::null_type() // GetPolicies
                                                                                  ,
                                                                                  reg::null_type(), reg::null_type(), arguments,
                                                                                  declares, docstring));
    return *this;
  }

  template <typename NameType, class Begin, class End>
  class_& iterator(const NameType& iteratorName, Begin beginFn, End endFn, string const& arguments = "",
                   string const& declares = "", string const& docstring = "") {
    this->add_member(new detail::iterator_registration<T, Begin, End, reg::null_type>(PrepareName(iteratorName),
                                                                                      beginFn // begin
                                                                                      ,
                                                                                      endFn // end
                                                                                      ,
                                                                                      reg::null_type() // null policies
                                                                                      ,
                                                                                      arguments, declares, docstring));
    return *this;
  }

  template <typename NameType, class C, class D> class_& def_readonly(const NameType& name, D C::*mem_ptr) {
    typedef detail::property_registration<T, D C::*, detail::null_type> registration_type;

    this->add_member(new registration_type(PrepareName(name), mem_ptr, detail::null_type()));
    return *this;
  }

  template <typename NameType, class C, class D, class Policies>
  class_& def_readonly(const NameType& name, D C::*mem_ptr, Policies const& policies) {
    typedef detail::property_registration<T, D C::*, Policies> registration_type;

    this->add_member(new registration_type(PrepareName(name), mem_ptr, policies));
    return *this;
  }

  template <typename NameType, class C, class D> class_& def_readwrite(const NameType& name, D C::*mem_ptr) {
    typedef detail::property_registration<T, D C::*, detail::null_type, D C::*> registration_type;

    this->add_member(new registration_type(PrepareName(name), mem_ptr, detail::null_type(), mem_ptr));
    return *this;
  }

  template <typename NameType, class C, class D, class GetPolicies>
  class_& def_readwrite(const NameType& name, D C::*mem_ptr, GetPolicies const& get_policies) {
    typedef detail::property_registration<T, D C::*, GetPolicies, D C::*> registration_type;

    this->add_member(new registration_type(PrepareName(name), mem_ptr, get_policies, mem_ptr));
    return *this;
  }

  template <typename NameType, class C, class D, class GetPolicies, class SetPolicies>
  class_& def_readwrite(const NameType& name, D C::*mem_ptr, GetPolicies const& get_policies, SetPolicies const& set_policies) {
    typedef detail::property_registration<T, D C::*, GetPolicies, D C::*, SetPolicies> registration_type;

    this->add_member(new registration_type(PrepareName(name), mem_ptr, get_policies, mem_ptr, set_policies));
    return *this;
  }

  template <class Derived, class Policies> class_& def(detail::operator_<Derived>, Policies const& policies) {
    return this->def(Derived::name(), &Derived::template apply<T, Policies>::execute, policies);
  }

  template <class Derived> class_& def(detail::operator_<Derived>) {
    return this->def(Derived::name(), &Derived::template apply<T, detail::null_type>::execute);
  }

  // #endif // end_meister_disabled

  scope_* _outer_scope;
  detail::static_scope<self_t> scope;

private:
  void operator=(class_ const&);

  template <class U> void add_wrapper_cast() {
    if constexpr(!std::is_same_v<U, reg::null_type>) {
      add_cast(reg::registered_class<U>::id, reg::registered_class<T>::id, detail::static_cast_<U, T>::execute);

      add_downcast<T, U>();
    }
  }

  void init() {
    typedef std::conditional_t<detail::is_bases<Base>::value, Base, bases<Base>> bases_t;

    class_base::init(typeid(T), reg::registered_class<T>::id, typeid(WrappedType), reg::registered_class<WrappedType>::id,
                     isDerivableCxxClass<T>(0));

    add_wrapper_cast<WrappedType>();
    generate_baseclass_list(bases_t());
  }

  // these handle default implementation of virtual functions
  template <class F, class Policies>
  class_& virtual_def(const std::string& name, F const& fn, Policies const& policies, reg::null_type) {
    maybe_register_symbol_using_dladdr(*(void**)&fn, sizeof(fn), name);
    this->add_member(new detail::memfun_registration<T, F, Policies>(name, fn, policies));
    return *this;
  }

  template <class Signature, class Policies>
  class_& def_default_constructor_(const char* name, Signature*, Policies const&, string const& docstring, string const& arguments,
                                   string const& declares) {
    typedef std::conditional_t<std::is_same_v<WrappedType, reg::null_type>, T, WrappedType> construct_type;

    this->set_default_constructor(
        new detail::constructor_registration<construct_type, HoldType, Signature, Policies, detail::construct_non_derivable_class>(
            Policies(), name, arguments, declares, docstring));
    return *this;
  }

  template <class Signature, class Policies>
  class_& def_constructor_(const string& name, Signature*, Policies const&, string const& arguments, string const& declares,
                           string const& docstring) {
    typedef Signature signature;

    typedef std::conditional_t<std::is_same_v<WrappedType, reg::null_type>, T, WrappedType> construct_type;
    this->add_member(
        new detail::constructor_registration<construct_type, HoldType, signature, Policies, detail::construct_non_derivable_class>(
            Policies(), name, arguments, declares, docstring));

    return *this;
  }
};
} // namespace clbind

#ifdef _MSC_VER
#pragma warning(pop)
#endif
