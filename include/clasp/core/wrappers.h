#pragma once
/*
    File: wrappers.h
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

#include <functional>
#include <clasp/core/lispDefinitions.h>
#include <clasp/core/exceptions.h>
#include <clasp/core/symbolToEnumConverter.h>
#include <clasp/clbind/apply.h>
#include <clasp/clbind/function.h>
#include <clasp/clbind/policies.h>

// #define DEBUG_METHOIDS 1

#include <clasp/core/arguments.h>
#include <clasp/core/lambdaListHandler.fwd.h>
#include <type_traits>

namespace clbind {
template <typename FunctionPtrType, typename Policies> class WRAPPER_VariadicMethod;
};

namespace clbind {
template <typename RT, typename OT, typename... ARGS, typename Policies>
class WRAPPER_VariadicMethod<RT (OT::*)(ARGS...), Policies> : public core::SimpleFun_O {
public:
  typedef WRAPPER_VariadicMethod<RT (OT::*)(ARGS...), Policies> MyType;
  typedef RT (OT::*MethodType)(ARGS...);
  typedef core::SimpleFun_O TemplatedBase;

public:
  MethodType mptr;

public:
  enum { NumParams = sizeof...(ARGS) + 1 };

  WRAPPER_VariadicMethod(MethodType ptr, core::FunctionDescription_sp fdesc, core::T_sp code)
    : core::SimpleFun_O(fdesc, code, core::XepStereotype<MyType>()), mptr(ptr) {
    this->validateCodePointer((void**)&this->mptr, sizeof(this->mptr));
  };

  virtual const char* describe() const { return "VariadicMethoid"; };

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
    OT& oto = *gc::As<gctools::smart_ptr<OT>>(core::T_sp((gctools::Tagged)lcc_args[0]));
    auto all_args = clbind::arg_tuple<1, clbind::policies<>, ARGS...>::goFrame(lcc_args);
    return clbind::method_apply_and_return<RT, core::policy::clasp_policy, decltype(closure->mptr), OT, decltype(all_args)>::go(
        std::move(closure->mptr), std::move(oto), std::move(all_args));
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
      OT& oto = *gc::As<gctools::smart_ptr<OT>>(core::T_sp((gctools::Tagged)std::get<0>(std::make_tuple(args...))));
      auto all_args = clbind::arg_tuple<1, clbind::policies<>, ARGS...>::goArgs(args...);
      return clbind::method_apply_and_return<RT, core::policy::clasp_policy, decltype(closure->mptr), OT, decltype(all_args)>::go(std::move(closure->mptr), std::move(oto), std::move(all_args));
    }
  }
};
}; // namespace clbind

//
// A copy of the specializer above but now with "cost" WHY, WHY, WHY!
//
namespace clbind {
template <typename RT, typename OT, typename... ARGS, typename Policies>
class WRAPPER_VariadicMethod<RT (OT::*)(ARGS...) const, Policies> : public core::SimpleFun_O {
public:
  typedef WRAPPER_VariadicMethod<RT (OT::*)(ARGS...) const, Policies> MyType;
  typedef RT (OT::*MethodType)(ARGS...) const;
  typedef core::SimpleFun_O TemplatedBase;

public:
  MethodType mptr;

public:
  enum { NumParams = sizeof...(ARGS) + 1 };

  WRAPPER_VariadicMethod(MethodType ptr, core::FunctionDescription_sp fdesc, core::T_sp code)
    : core::SimpleFun_O(fdesc, code, core::XepStereotype<MyType>()), mptr(ptr) {
    this->validateCodePointer((void**)&this->mptr, sizeof(this->mptr));
  };

  virtual const char* describe() const { return "VariadicMethoid"; };

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
    OT& oto = *gc::As<gctools::smart_ptr<OT>>(core::T_sp((gctools::Tagged)lcc_args[0]));
    auto all_args = clbind::arg_tuple<1, clbind::policies<>, ARGS...>::goFrame(lcc_args);
    return clbind::method_apply_and_return<RT, core::policy::clasp_policy, decltype(closure->mptr), OT, decltype(all_args)>::go(
        std::move(closure->mptr), std::move(oto), std::move(all_args));
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
      OT& oto = *gc::As<gctools::smart_ptr<OT>>(core::T_sp((gctools::Tagged)std::get<0>(std::make_tuple(args...))));
      auto all_args = clbind::arg_tuple<1, clbind::policies<>, ARGS...>::goArgs(args...);
      return clbind::method_apply_and_return<RT, core::policy::clasp_policy, decltype(closure->mptr), OT, decltype(all_args)>::go(std::move(closure->mptr), std::move(oto), std::move(all_args));
    }
  }
};
}; // namespace clbind

namespace core {

template <typename RT, typename... ARGS>
void wrap_function(const string& packageName, const string& name, RT (*fp)(ARGS...), const string& arguments = "",
                   const string& declares = "", const string& docstring = "", const string& sourceFile = "", int sourceLine = 0) {
  maybe_register_symbol_using_dladdr(*(void**)&fp, sizeof(fp), name);
  Symbol_sp symbol = _lisp->intern(name, packageName);
  using VariadicType =
      clbind::WRAPPER_VariadicFunction<RT (*)(ARGS...), core::policy::clasp_policy>;
  FunctionDescription_sp fdesc = makeFunctionDescription(symbol, nil<T_O>());
  auto entry = gctools::GC<VariadicType>::allocate(fp, fdesc, nil<T_O>());
  lisp_bytecode_defun(core::symbol_function, symbol, packageName, entry, arguments, declares,
                      docstring, sourceFile, sourceLine, sizeof...(ARGS));
}

// this is used in gc_interface.cc expose_function_setf
template <typename RT, typename... ARGS>
void wrap_function_setf(const string& packageName, const string& name, RT (*fp)(ARGS...), const string& arguments = "",
                        const string& declares = "", const string& docstring = "", const string& sourceFile = "",
                        int sourceLine = 0) {
  maybe_register_symbol_using_dladdr(*(void**)&fp, sizeof(fp), name);
  Symbol_sp symbol = _lisp->intern(name, packageName);
  using VariadicType =
      clbind::WRAPPER_VariadicFunction<RT (*)(ARGS...), core::policy::clasp_policy>;
  FunctionDescription_sp fdesc = makeFunctionDescription(symbol, nil<T_O>());
  auto entry = gctools::GC<VariadicType>::allocate(fp, fdesc, nil<T_O>());
  lisp_bytecode_defun(core::symbol_function_setf, symbol, packageName, entry, arguments,
                      declares, docstring, sourceFile, sourceLine, sizeof...(ARGS));
}

}; // namespace core

template <typename FunctionPtrType, typename Policies>
class gctools::GCStamp<clbind::WRAPPER_VariadicMethod<FunctionPtrType, Policies>> {
public:
  static gctools::GCStampEnum const StampWtag = gctools::GCStamp<
      typename clbind::WRAPPER_VariadicMethod<FunctionPtrType, Policies>::TemplatedBase>::StampWtag;
};
template <typename FunctionPtrType, typename Policies>
struct gctools::Inherits<typename clbind::WRAPPER_VariadicMethod<FunctionPtrType, Policies>::TemplatedBase,
                         clbind::WRAPPER_VariadicMethod<FunctionPtrType, Policies>> : public std::true_type {};

namespace core {

class Function_O;

// -----------------------------------
//
// Macro stuff
//
//
// Wrapper for ActivationFrameMacroPtr

// basically like wrap_function.
inline void defmacro(const string& packageName, const string& name, T_mv (*fp)(List_sp, T_sp env), const string& arguments,
                     const string& declares, const string& docstring, const string& sourcePathname, int lineno) {
  maybe_register_symbol_using_dladdr(*(void**)&fp, sizeof(fp), name);
  Symbol_sp symbol = lispify_intern(name, packageName);
  using VariadicType = clbind::WRAPPER_VariadicFunction<T_mv (*)(List_sp, T_sp), core::policy::clasp_policy>;
  FunctionDescription_sp fdesc = makeFunctionDescription(symbol, nil<T_O>());
  SimpleFun_sp entry = gctools::GC<VariadicType>::allocate(fp, fdesc, nil<T_O>());
  lisp_bytecode_defun(symbol_function_macro, symbol, packageName, entry, arguments, declares,
                      docstring);
}

template <int N> struct DispatchOn {
  enum { value = N };
};

// ----------------------------------------
// ----------------------------------------
// ----------------------------------------
// ----------------------------------------

extern Symbol_sp& _sym_STARallCxxClassesSTAR;

struct names_ {
  std::vector<std::string> _Names;
  names_(const std::string& name1) : _Names({name1}){};
  names_(const std::string& name1, const std::string& name2) : _Names({name1, name2}){};
};

template <typename OT> class class_ {
public:
  typedef OT wrapped_type;

private:
  Symbol_sp _ClassSymbol;

public:
  void setup_class(const string& makerName = "") {
    _G();
    if (IS_SYMBOL_UNDEFINED(OT::static_classSymbol())) {
      SIMPLE_ERROR("Attempting to add methods for class that isn't defined yet");
    }

    this->_ClassSymbol = OT::static_classSymbol();

    reg::lisp_registerClassSymbol<OT>(this->_ClassSymbol);

    /*! Accumulate all of the classes in reverse order of how they were initialized
              in the core::*all-cxx-classes* variable */
    lisp_pushClassSymbolOntoSTARallCxxClassesSTAR(OT::static_classSymbol());

    //
    // If the class isn't in the class table then add it
    //
    if (makerName != "") {
      // use make-<className>
      std::string magic_maker_name = core::magic_name(makerName, OT::static_packageName());
      std::string pkg_part;
      std::string symbol_part;
      core::colon_split(magic_maker_name, pkg_part, symbol_part);
      wrap_function(pkg_part, symbol_part, &new_LispObject<OT>);
    }
  }

  //
  //
  // ctor
  //
  //

  class_() {
    _G();
    this->setup_class("");
  }

  class_(const string& makerName) {
    _G();
    this->setup_class(makerName);
  }

  // non-const function dispatch on parameter 0
  template <typename RT, class... ARGS>
  class_& def(const names_& names, RT (OT::*mp)(ARGS...), string const& lambda_list = "", const string& declares = "",
              const string& docstring = "", bool autoExport = true) {
    for (auto& name : names._Names) {
      maybe_register_symbol_using_dladdr(*(void**)&mp, sizeof(mp), name);
      std::string pkgName;
      std::string symbolName;
      core::colon_split(name, pkgName, symbolName);
      if (pkgName == "") {
        pkgName = symbol_packageName(this->_ClassSymbol);
      }
      Symbol_sp symbol = _lisp->intern(symbolName, pkgName);
      using VariadicType = clbind::WRAPPER_VariadicMethod<RT (OT::*)(ARGS...), core::policy::clasp_policy>;
      FunctionDescription_sp fdesc = makeFunctionDescription(symbol, nil<T_O>());
      auto entry = gctools::GC<VariadicType>::allocate(mp, fdesc, nil<T_O>());
      lisp_defineSingleDispatchMethod(symbol, this->_ClassSymbol, entry, 0, true, lambda_list, declares,
                                      docstring, autoExport, sizeof...(ARGS) + 1);
    }
    return *this;
  }

  // const function dispatch on parameter 0
  template <typename RT, class... ARGS>
  class_& def(const names_& names, RT (OT::*mp)(ARGS...) const, string const& lambda_list = "", const string& declares = "",
              const string& docstring = "", bool autoExport = true) {
    for (auto& name : names._Names) {
      maybe_register_symbol_using_dladdr(*(void**)&mp, sizeof(mp), name);
      std::string pkgName;
      std::string symbolName;
      core::colon_split(name, pkgName, symbolName);
      if (pkgName == "") {
        pkgName = symbol_packageName(this->_ClassSymbol);
      }
      Symbol_sp symbol = _lisp->intern(symbolName, pkgName);
      using VariadicType =
          clbind::WRAPPER_VariadicMethod<RT (OT::*)(ARGS...) const, core::policy::clasp_policy>;
      FunctionDescription_sp fdesc = makeFunctionDescription(symbol, nil<T_O>());
      auto entry = gctools::GC<VariadicType>::allocate(mp, fdesc, nil<T_O>());
      lisp_defineSingleDispatchMethod(symbol, this->_ClassSymbol, entry, 0, true, lambda_list, declares,
                                      docstring, autoExport, sizeof...(ARGS) + 1);
    }
    return *this;
  }
};

template <typename oclass> void defaultExposeCando(LispPtr lisp) {
  _G();
  // Only expose the class, don't create any methods
  // By default put class in the Cl package
  class_<oclass>();
}

struct EnumValueDefinition {
  string _Name;
  int _Value;
};

template <typename X> class enum_ {
private:
  SymbolToEnumConverter_sp _Converter;
  Symbol_sp _PredefinedConverterSymbolId;

public:
  enum_(Symbol_sp symbol, const string& title) {
    _G();
    this->_PredefinedConverterSymbolId = symbol;
    this->_Converter = SymbolToEnumConverter_O::create(title);
    lisp_defparameter(symbol, this->_Converter);
  }

  enum_& value(Symbol_sp const& sym, X value) {
    _G();
    lisp_extendSymbolToEnumConverter(this->_Converter, sym, sym, static_cast<int>(value));
    return *this;
  }
  enum_& value(Symbol_sp const& name, Symbol_sp const& archiveName, X value) {
    _G();
    lisp_extendSymbolToEnumConverter(this->_Converter, name, archiveName, value);
    return *this;
  }
  Symbol_sp symbolFromEnum(int value) {
    _G();
    return lisp_lookupSymbolForEnum(this->_PredefinedConverterSymbolId, (int)(value));
  }
};
}; // namespace core

// ostream& operator<<(ostream& out, gctools::smart_ptr<core::T_O>
