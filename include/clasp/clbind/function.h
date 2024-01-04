#pragma once

/*
    File: function.h
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
// Copyright Daniel Wallin 2008. Use, modification and distribution is
// subject to the Boost Software License, Version 1.0. (See accompanying
// file LICENSE_1_0.txt or copy at http://www.boost.org/LICENSE_1_0.txt)

#if 0
#define DEBUG_SCOPE 1
#define LOG_SCOPE(xxx) printf xxx;
#else
#define LOG_SCOPE(xxx)
#endif

#include <clasp/core/lambdaListHandler.fwd.h>
// #include "clbind/prefix.h"
#include <clasp/clbind/config.h>
#include <clasp/clbind/cl_include.h>
#include <clasp/llvmo/intrinsics.h>
#include <memory>

namespace clbind {

struct scope_;

} // namespace clbind

namespace clbind {
namespace detail {

struct CLBIND_API registration {
  registration();
  virtual ~registration();

public:
  virtual gc::smart_ptr<core::Creator_O> registerDefaultConstructor_() const { HARD_SUBCLASS_MUST_IMPLEMENT(); };
  virtual std::string name() const = 0;
  virtual std::string kind() const = 0;

protected:
  virtual void register_() const = 0;

private:
  friend struct ::clbind::scope_;
  registration* m_next;
};
} // namespace detail
} // namespace clbind

#include <clasp/clbind/clbindPackage.h>
#include <clasp/clbind/policies.h>
#include <clasp/clbind/details.h>
#include <clasp/core/arguments.h>
#include <clasp/clbind/apply.h>

namespace clbind {
template <typename... Types> class my_tuple : public std::tuple<Types...> {
  my_tuple() { printf("%s:%d:%s\n", __FILE__, __LINE__, __FUNCTION__); }
};
}; // namespace clbind

namespace clbind {
template <typename FunctionPtrType, typename Policies, typename PureOuts, typename ArgumentWrapper> class WRAPPER_VariadicFunction;
};

namespace clbind {
template <typename RT, typename... ARGS, typename Policies, typename... PUREOUTS, typename ArgumentWrapper>
class WRAPPER_VariadicFunction<RT (*)(ARGS...), Policies, clbind::pureOutsPack<PUREOUTS...>, ArgumentWrapper>
    : public core::SimpleFun_O {
public:
  typedef WRAPPER_VariadicFunction<RT (*)(ARGS...), Policies, clbind::pureOutsPack<PUREOUTS...>, ArgumentWrapper> MyType;
  typedef core::SimpleFun_O TemplatedBase;
  typedef RT (*FuncType)(ARGS...);

public:
  FuncType fptr;

public:
  static constexpr auto inValueMask = clbind::inValueMaskMuple<sizeof...(ARGS), Policies>();

  enum { NumParams = sizeof...(ARGS) };

  WRAPPER_VariadicFunction(FuncType ptr, core::FunctionDescription_sp fdesc, core::T_sp code)
    : SimpleFun_O(fdesc, code, core::XepStereotype<MyType>()), fptr(ptr) {
    this->validateCodePointer((void**)&this->fptr, sizeof(this->fptr));
  };

  virtual size_t templatedSizeof() const { return sizeof(*this); };

  void fixupInternalsForSnapshotSaveLoad(snapshotSaveLoad::Fixup* fixup) {
    this->TemplatedBase::fixupInternalsForSnapshotSaveLoad(fixup);
    this->fixupOneCodePointer(fixup, (void**)&this->fptr);
  };

  static inline LCC_RETURN handler_entry_point_n(const BytecodeWrapper& dummy, core::T_O* lcc_closure, size_t lcc_nargs,
                                                 core::T_O** lcc_args) {
    MyType* closure = gctools::untag_general<MyType*>((MyType*)lcc_closure);
    INCREMENT_FUNCTION_CALL_COUNTER(closure);
    DO_DRAG_CXX_CALLS();
    if (lcc_nargs != NumParams)
      cc_wrong_number_of_arguments(lcc_closure, lcc_nargs, NumParams, NumParams);
    std::tuple<translate::from_object<ARGS, PUREOUTS>...> all_args(arg_tuple<0, Policies, ARGS...>::goFrame(lcc_args));
    return apply_and_return<Policies, RT, decltype(closure->fptr), decltype(all_args)>::go(std::move(closure->fptr),
                                                                                           std::move(all_args));
  }

  static inline LCC_RETURN entry_point_n(core::T_O* lcc_closure, size_t lcc_nargs, core::T_O** lcc_args) {
    //    if (lcc_nargs != NumParams) cc_wrong_number_of_arguments(lcc_closure,lcc_nargs,NumParams,NumParams);
    return handler_entry_point_n(ArgumentWrapper(), lcc_closure, lcc_nargs, lcc_args);
  }

  template <typename... Ts>
  static inline LCC_RETURN entry_point_fixed(core::T_O* lcc_closure,
                                             Ts... args) {
    core::T_O* lcc_args[sizeof...(Ts)] = {args...};
    return entry_point_n(lcc_closure, sizeof...(Ts), lcc_args);
  }
};
}; // namespace clbind

template <typename FunctionPtrType, typename Policies, typename PureOutsPack, typename ArgumentWrapper>
class gctools::GCStamp<clbind::WRAPPER_VariadicFunction<FunctionPtrType, Policies, PureOutsPack, ArgumentWrapper>> {
public:
  static gctools::GCStampEnum const StampWtag =
      gctools::GCStamp<typename clbind::WRAPPER_VariadicFunction<FunctionPtrType, Policies, PureOutsPack,
                                                                 ArgumentWrapper>::TemplatedBase>::StampWtag;
};

template <typename FunctionPtrType, typename Policies, typename PureOutsPack, typename ArgumentWrapper>
struct gctools::Inherits<
    typename clbind::WRAPPER_VariadicFunction<FunctionPtrType, Policies, PureOutsPack, ArgumentWrapper>::TemplatedBase,
    clbind::WRAPPER_VariadicFunction<FunctionPtrType, Policies, PureOutsPack, ArgumentWrapper>> : public std::true_type {};

namespace clbind {

namespace detail {

template <typename FunctionPointerType> struct CountFunctionArguments {
  enum { value = 0 };
};

template <typename RT, typename... ARGS> struct CountFunctionArguments<RT (*)(ARGS...)> {
  enum { value = sizeof...(ARGS) };
};

template <class FunctionPointerType, class Policies = policies<>, class PureOutsPack = clbind::pureOutsPack<>>
struct function_registration;

template <class FunctionPointerType, class... Policies, class PureOutsPack>
struct function_registration<FunctionPointerType, policies<Policies...>, PureOutsPack> : registration {
  function_registration(
      const std::string& name, FunctionPointerType f,
      policies<Policies...> const& policies) // , string const &lambdalist, string const &declares, string const &docstring)
      : m_name(name), functionPtr(f), m_policies(policies) {
    this->m_lambdalist = policies.lambdaList();
    this->m_docstring = policies.docstring();
    this->m_declares = policies.declares();
    this->m_autoExport = policies.autoExport();
    this->m_setf = policies.setf();
  }

  void register_() const {
    LOG_SCOPE(("%s:%d register_ %s/%s\n", __FILE__, __LINE__, this->kind().c_str(), this->name().c_str()));
    core::Symbol_sp symbol = core::lisp_intern(m_name, core::lisp_currentPackageName());
    using inValuePack = clbind::inValueTrueFalseMaskPack<FunctionArgCount<FunctionPointerType>::value, policies<Policies...>>;
    using VariadicType =
        WRAPPER_VariadicFunction<FunctionPointerType, policies<Policies...>, typename inValuePack::type, clbind::DefaultWrapper>;
    core::FunctionDescription_sp fdesc = makeFunctionDescription(symbol, nil<core::T_O>());
    auto entry = gctools::GC<VariadicType>::allocate(this->functionPtr, fdesc, nil<core::T_O>());
    core::lisp_bytecode_defun(this->m_setf ? core::symbol_function_setf : core::symbol_function,
                              symbol, core::lisp_currentPackageName(), entry, m_lambdalist, m_declares, m_docstring,
                              "=external=", 0, (CountFunctionArguments<FunctionPointerType>::value), m_autoExport,
                              GatherPureOutValues<policies<Policies...>, -1>::gather());
  }

  virtual std::string name() const { return this->m_name; }
  virtual std::string kind() const { return "function_registration"; };

  std::string m_name;
  FunctionPointerType functionPtr;
  policies<Policies...> m_policies;
  string m_lambdalist;
  string m_declares;
  string m_docstring;
  bool m_autoExport;
  bool m_setf;
};

} // namespace detail

} // namespace clbind
