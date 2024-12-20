#pragma once
/*
    File: external_wrappers.h
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

#include <clasp/core/lispDefinitions.h>

#include <clasp/core/wrappers.h>
// #include "clbind.h"

namespace clbind {
template <typename MethodPtrType, typename Policies, typename OT> class WRAPPER_IndirectMethod;
};

namespace clbind {

template <typename RT, typename OT, typename... ARGS, typename Policies>
class WRAPPER_IndirectMethod<RT (OT::ExternalType::*)(ARGS...), Policies, OT>
    : public core::SimpleFun_O {
public:
  typedef WRAPPER_IndirectMethod<RT (OT::ExternalType::*)(ARGS...), Policies, OT> MyType;
  typedef RT (OT::ExternalType::*MethodType)(ARGS...);
  typedef core::SimpleFun_O TemplatedBase;

public:
  MethodType mptr;

public:
  enum { NumParams = sizeof...(ARGS) + 1 };

  WRAPPER_IndirectMethod(MethodType ptr, core::FunctionDescription_sp fdesc, core::T_sp code)
    : mptr(ptr), SimpleFun_O(fdesc, code, core::XepStereotype<MyType>()) {
    this->validateCodePointer((void**)&this->mptr, sizeof(this->mptr));
  };

  virtual const char* describe() const { return "IndirectVariadicMethoid"; };

  virtual size_t templatedSizeof() const { return sizeof(*this); };

  void fixupInternalsForSnapshotSaveLoad(snapshotSaveLoad::Fixup* fixup) {
    this->TemplatedBase::fixupInternalsForSnapshotSaveLoad(fixup);
    this->fixupOneCodePointer(fixup, (void**)&this->mptr);
  };

  static inline LCC_RETURN entry_point_n(core::T_O* lcc_closure, size_t lcc_nargs,
                                         core::T_O** lcc_args) {
    MyType* closure = gctools::untag_general<MyType*>((MyType*)lcc_closure);
    DO_DRAG_CXX_CALLS();
    if (lcc_nargs != NumParams)
      cc_wrong_number_of_arguments(lcc_closure, lcc_nargs, NumParams, NumParams);
    core::T_sp ootep((gctools::Tagged)lcc_args[0]);
    OT* otep = &*gc::As<gctools::smart_ptr<OT>>(ootep);
    auto all_args = clbind::arg_tuple<1, Policies, ARGS...>::goFrame(lcc_args);
    return clbind::external_method_apply_and_return<Policies, RT, decltype(closure->mptr), OT, decltype(all_args)>::go(
        std::move(closure->mptr), otep, std::move(all_args));
  }

  template <typename... Ts>
  static inline LCC_RETURN entry_point_fixed(core::T_O* lcc_closure,
                                             Ts... args) {
    DO_DRAG_CXX_CALLS();
    if constexpr(sizeof...(Ts) != NumParams) {
      cc_wrong_number_of_arguments(lcc_closure, sizeof...(Ts), NumParams, NumParams);
      UNREACHABLE();
    } else {
      MyType* closure = gctools::untag_general<MyType*>((MyType*)lcc_closure);
      core::T_sp ootep((gctools::Tagged)std::get<0>(std::make_tuple(args...)));
      OT* otep = &*gc::As<gctools::smart_ptr<OT>>(ootep);
      auto all_args = clbind::arg_tuple<1, Policies, ARGS...>::goArgs(args...);
      return clbind::external_method_apply_and_return<Policies, RT, decltype(closure->mptr), OT, decltype(all_args)>::go(std::move(closure->mptr), otep, std::move(all_args));
    }
  }
};
}; // namespace clbind

namespace clbind {
template <typename RT, typename OT, typename... ARGS, typename Policies>
class WRAPPER_IndirectMethod<RT (OT::ExternalType::*)(ARGS...) const, Policies, OT>
    : public core::SimpleFun_O {
public:
  typedef WRAPPER_IndirectMethod<RT (OT::ExternalType::*)(ARGS...) const, Policies, OT> MyType;
  typedef RT (OT::ExternalType::*MethodType)(ARGS...) const;
  typedef SimpleFun_O TemplatedBase;

public:
  MethodType mptr;

public:
  enum { NumParams = sizeof...(ARGS) + 1 };

  WRAPPER_IndirectMethod(MethodType ptr, core::FunctionDescription_sp fdesc, core::T_sp code)
    : mptr(ptr), SimpleFun_O(fdesc, code, core::XepStereotype<MyType>()) {
    this->validateCodePointer((void**)&this->mptr, sizeof(this->mptr));
  };

  virtual const char* describe() const { return "IndirectVariadicMethoid"; };

  virtual size_t templatedSizeof() const { return sizeof(*this); };

  void fixupInternalsForSnapshotSaveLoad(snapshotSaveLoad::Fixup* fixup) {
    this->TemplatedBase::fixupInternalsForSnapshotSaveLoad(fixup);
    this->fixupOneCodePointer(fixup, (void**)&this->mptr);
  };

  static inline LCC_RETURN entry_point_n(core::T_O* lcc_closure, size_t lcc_nargs,
                                         core::T_O** lcc_args) {
    MyType* closure = gctools::untag_general<MyType*>((MyType*)lcc_closure);
    DO_DRAG_CXX_CALLS();
    if (lcc_nargs != NumParams)
      cc_wrong_number_of_arguments(lcc_closure, lcc_nargs, NumParams, NumParams);
    core::T_sp ootep((gctools::Tagged)lcc_args[0]);
    OT* otep = &*gc::As<gctools::smart_ptr<OT>>(ootep);
    auto all_args = clbind::arg_tuple<1, Policies, ARGS...>::goFrame(lcc_args);
    return clbind::external_method_apply_and_return<Policies, RT, decltype(closure->mptr), OT, decltype(all_args)>::go(
        std::move(closure->mptr), otep, std::move(all_args));
  }

  template <typename... Ts>
  static inline LCC_RETURN entry_point_fixed(core::T_O* lcc_closure,
                                             Ts... args) {
    DO_DRAG_CXX_CALLS();
    if constexpr(sizeof...(Ts) != NumParams) {
      cc_wrong_number_of_arguments(lcc_closure, sizeof...(Ts), NumParams, NumParams);
      UNREACHABLE();
    } else {
      MyType* closure = gctools::untag_general<MyType*>((MyType*)lcc_closure);
      core::T_sp ootep((gctools::Tagged)std::get<0>(std::make_tuple(args...)));
      OT* otep = &*gc::As<gctools::smart_ptr<OT>>(ootep);
      auto all_args = clbind::arg_tuple<1, Policies, ARGS...>::goArgs(args...);
      return clbind::external_method_apply_and_return<Policies, RT, decltype(closure->mptr), OT, decltype(all_args)>::go(std::move(closure->mptr), otep, std::move(all_args));
    }
  }
};
}; // namespace clbind

namespace core {
template <class D, class C> class WRAPPER_Getter : public SimpleFun_O {
public:
  typedef SimpleFun_O TemplatedBase;
  typedef WRAPPER_Getter<D, C> MyType;

public:
  //        typedef std::function<void (OT& ,)> Type;
  typedef D(C::*MemPtr);
  MemPtr mptr;

public:
  WRAPPER_Getter(MemPtr ptr, core::FunctionDescription_sp fdesc, core::T_sp code)
    : mptr(ptr), SimpleFun_O(fdesc, code, core::XepStereotype<MyType>()) {
    this->validateCodePointer((void**)&this->mptr, sizeof(this->mptr));
  }

  virtual size_t templatedSizeof() const { return sizeof(*this); };

  void fixupInternalsForSnapshotSaveLoad(snapshotSaveLoad::Fixup* fixup) {
    this->TemplatedBase::fixupInternalsForSnapshotSaveLoad(fixup);
    printf("%s:%d:%s What do we do with mptr %p\n", __FILE__, __LINE__, __FUNCTION__, *(void**)&this->mptr);
    // this->fixupOneCodePointer( fixup, (void**)&this->mptr );
  };

  static inline LCC_RETURN LISP_CALLING_CONVENTION() { SIMPLE_ERROR("What do I do here"); }
  template <typename...Ts>
  static inline LCC_RETURN entry_point_fixed(core::T_O* lcc_closure,
                                             Ts... args) {
    SIMPLE_ERROR("What do I do here");
  }
};
}; // namespace core

template <typename Policies, typename OT, typename MethodPtrType>
class gctools::GCStamp<clbind::WRAPPER_IndirectMethod<MethodPtrType, Policies, OT>> {
public:
  static gctools::GCStampEnum const StampWtag = gctools::GCStamp<
      typename clbind::WRAPPER_IndirectMethod<MethodPtrType, Policies, OT>::TemplatedBase>::StampWtag;
};

template <typename Policies, typename OT, typename MethodPtrType>
struct gctools::Inherits<typename clbind::WRAPPER_IndirectMethod<MethodPtrType, Policies, OT>::TemplatedBase,
                         clbind::WRAPPER_IndirectMethod<MethodPtrType, Policies, OT>> : public std::true_type {};

namespace core {

// ----------------------------------------
// ----------------------------------------
// ----------------------------------------
// ----------------------------------------
extern Symbol_sp& _sym_STARallCxxClassesSTAR;

template <typename OT> class externalClass_ {
private:
  Symbol_sp _ClassSymbol;

public:
  void setup_class(const string& makerName) {
    _G();
    if (IS_SYMBOL_UNDEFINED(OT::static_classSymbol())) {
      SIMPLE_ERROR("Attempting to add methods for class that isn't defined yet");
    }
    this->_ClassSymbol = OT::static_classSymbol();
    reg::lisp_registerClassSymbol<OT>(this->_ClassSymbol);
    /*! Accumulate all of the classes in reverse order of how they were initialized
              in the core::*all-cxx-classes* variable */
    if (_sym_STARallCxxClassesSTAR->boundP()) {
      _sym_STARallCxxClassesSTAR->setf_symbolValue(
          Cons_O::create(OT::static_classSymbol(), _sym_STARallCxxClassesSTAR->symbolValue()));
    }

    //
    // If the class isn't in the class table then add it
    //
    T_sp theClass = lisp_boot_findClassBySymbolOrNil(OT::static_classSymbol());
    if (theClass.nilp()) {
      LOG(("Adding class(%s) to environment"), OT::static_className());
      DEPRECATED();
    }
  }

  //
  //
  // ctor
  //
  //

  externalClass_() {
    _G();
    this->setup_class("");
  }

  externalClass_(const string& makerName) {
    _G();
    this->setup_class(makerName);
  }

  template <typename RT, class... ARGS>
  externalClass_& def(const names_& names, RT (OT::*mp)(ARGS...), string const& lambda_list = "", const string& declares = "",
                      const string& docstring = "", bool autoExport = true) {
    for (auto& name : names._Names) {
      maybe_register_symbol_using_dladdr(*(void**)&mp, sizeof(mp), name);
      Symbol_sp symbol = lispify_intern(name, symbol_packageName(this->_ClassSymbol));
      using VariadicType = clbind::WRAPPER_VariadicMethod<RT (OT::*)(ARGS...), core::policy::clasp_policy>;
      FunctionDescription_sp fdesc = makeFunctionDescription(symbol, nil<T_O>());
      auto entry = gctools::GC<VariadicType>::allocate(mp, fdesc, nil<T_O>());
      lisp_defineSingleDispatchMethod(symbol, this->_ClassSymbol, entry, 0, true, lambda_list, declares,
                                      docstring, autoExport, sizeof...(ARGS) + 1);
    }
    return *this;
  }

  template <typename RT, class... ARGS>
  externalClass_& def(const names_& names, RT (OT::*mp)(ARGS...) const, string const& lambda_list = "", const string& declares = "",
                      const string& docstring = "", bool autoExport = true) {
    for (auto& name : names._Names) {
      maybe_register_symbol_using_dladdr(*(void**)&mp, sizeof(mp), name);
      Symbol_sp symbol = lispify_intern(name, symbol_packageName(this->_ClassSymbol));
      using VariadicType =
          clbind::WRAPPER_VariadicMethod<RT (OT::*)(ARGS...) const, core::policy::clasp_policy>;
      FunctionDescription_sp fdesc = makeFunctionDescription(symbol, nil<T_O>());
      auto entry = gctools::GC<VariadicType>::allocate(mp, fdesc, nil<T_O>());
      lisp_defineSingleDispatchMethod(symbol, this->_ClassSymbol, entry, 0, true, lambda_list, declares,
                                      docstring, autoExport, sizeof...(ARGS) + 1);
    }
    return *this;
  }

  template <typename RT, class... ARGS>
  externalClass_& def(const names_& names, RT (OT::ExternalType::*mp)(ARGS...), const string& lambda_list = "",
                      const string& declares = "", const string& docstring = "", bool autoExport = true) {
    for (auto& name : names._Names) {
      maybe_register_symbol_using_dladdr(*(void**)&mp, sizeof(mp), name);
      Symbol_sp symbol = lispify_intern(name, symbol_packageName(this->_ClassSymbol));
      using VariadicType =
          clbind::WRAPPER_IndirectMethod<RT (OT::ExternalType::*)(ARGS...), clbind::policies<>, OT>;
      FunctionDescription_sp fdesc = makeFunctionDescription(symbol, nil<T_O>());
      auto entry = gctools::GC<VariadicType>::allocate(mp, fdesc, nil<T_O>());
      lisp_defineSingleDispatchMethod(symbol, this->_ClassSymbol, entry, 0, true, lambda_list, declares,
                                      docstring, autoExport, sizeof...(ARGS) + 1);
    }
    return *this;
  }

  template <typename RT, class... ARGS>
  externalClass_& def(const names_& names, RT (OT::ExternalType::*mp)(ARGS...) const, const string& lambda_list = "",
                      const string& declares = "", const string& docstring = "", bool autoExport = true) {
    for (auto& name : names._Names) {
      maybe_register_symbol_using_dladdr(*(void**)&mp, sizeof(mp), name);
      Symbol_sp symbol = lispify_intern(name, symbol_packageName(this->_ClassSymbol));
      using VariadicType =
          clbind::WRAPPER_IndirectMethod<RT (OT::ExternalType::*)(ARGS...) const, clbind::policies<>, OT>;
      FunctionDescription_sp fdesc = makeFunctionDescription(symbol, nil<T_O>());
      auto entry = gctools::GC<VariadicType>::allocate(mp, fdesc, nil<T_O>());
      lisp_defineSingleDispatchMethod(symbol, this->_ClassSymbol, entry, 0, true, lambda_list, declares,
                                      docstring, autoExport, sizeof...(ARGS) + 1);
    }
    return *this;
  }

  template <class C, class D> externalClass_& def_readonly(string const& name, D C::*mem_ptr) { return *this; }
};
}; // namespace core
