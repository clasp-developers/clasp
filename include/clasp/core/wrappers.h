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
#ifndef core_wrappers_H
#define core_wrappers_H

#include <functional>
#include <clasp/core/lispDefinitions.h>
#include <clasp/core/exceptions.h>
#include <clasp/core/symbolToEnumConverter.h>
#include <clasp/core/activationFrame.h>
#include <clasp/clbind/apply.h>
#include <clasp/clbind/function.h>
#include <clasp/clbind/policies.h>

//#define DEBUG_METHOIDS 1




namespace core {

  class TranslationFunctor_O : public BuiltinClosure_O {
    LISP_CLASS(core,CorePkg,TranslationFunctor_O,"TranslationFunctor",BuiltinClosure_O);
  public:
    typedef core::T_O* (*Type)(core::T_O* arg);
    Type fptr;
  public:
    TranslationFunctor_O(GlobalEntryPoint_sp ep, Type ptr) : BuiltinClosure_O(ep), fptr(ptr) {
      this->validateCodePointer((void**)&this->fptr,sizeof(this->fptr));
    };
  public:
    typedef BuiltinClosure_O TemplatedBase;
    virtual size_t templatedSizeof() const override { return sizeof(TranslationFunctor_O); };
    virtual void fixupInternalsForSnapshotSaveLoad( snapshotSaveLoad::Fixup* fixup ) {
      this->fixupOneCodePointer( fixup, (void**)&this->fptr, sizeof(this->fptr) );
    }
    static inline LCC_RETURN LISP_CALLING_CONVENTION() {
      TranslationFunctor_O* closure = gctools::untag_general<TranslationFunctor_O*>((TranslationFunctor_O*)lcc_closure);
      return gctools::return_type((closure->fptr)(lcc_fixed_arg0),1);
    }
#if 0
    LISP_ENTRY_n() {wrong_number_of_arguments(closure,nargs,1);}
    LISP_ENTRY_0() {wrong_number_of_arguments(closure,0,1);}
    LISP_ENTRY_1() {
      TranslationFunctor_O* closure = gctools::untag_general<TranslationFunctor_O*>((TranslationFunctor_O*)closure);
      return gctools::return_type((closure->fptr)(farg0),1);
    }
    LISP_ENTRY_2() {wrong_number_of_arguments(closure,2,1);}
    LISP_ENTRY_3() {wrong_number_of_arguments(closure,3,1);}
#endif
  };

};


#include <clasp/core/arguments.h>
#include <clasp/core/lambdaListHandler.fwd.h>


#if 1
namespace core {
//#include <clasp/core/wrappers_functoids.h>

template <int DispatchOn, typename Policies, typename FunctionPtrType>
class TEMPLATED_FUNCTION_VariadicMethoid : public BuiltinClosure_O {
public:
  typedef BuiltinClosure_O TemplatedBase;
  size_t templatedSizeof() const { return sizeof(*this); };
};

template <int DispatchOn, typename RT, typename OT, typename... ARGS>
class TEMPLATED_FUNCTION_VariadicMethoid <DispatchOn, core::policy::clasp, RT(OT::*)(ARGS...)> : public BuiltinClosure_O {
public:
  typedef TEMPLATED_FUNCTION_VariadicMethoid<DispatchOn, core::policy::clasp,RT(OT::*)(ARGS...) > MyType;
  typedef BuiltinClosure_O TemplatedBase;
public:
  virtual const char* describe() const {return "VariadicMethoid";};
  typedef RT(OT::*MethodType)(ARGS...) ;
  MethodType mptr;
public:
  enum { NumParams = sizeof...(ARGS)+1 };
  TEMPLATED_FUNCTION_VariadicMethoid(GlobalEntryPoint_sp ep, MethodType ptr) : BuiltinClosure_O(ep), mptr(ptr) {
    this->validateCodePointer((void**)&this->mptr,sizeof(this->mptr));
  };
  virtual size_t templatedSizeof() const { return sizeof(*this);};
  virtual void fixupInternalsForSnapshotSaveLoad( snapshotSaveLoad::Fixup* fixup ) {
    this->fixupOneCodePointer( fixup, (void**)&this->mptr, sizeof(this->mptr) );
  }
  static inline gctools::return_type LISP_CALLING_CONVENTION()
  {
    MyType* closure = gctools::untag_general<MyType*>((MyType*)lcc_closure);
    INCREMENT_FUNCTION_CALL_COUNTER(closure);
    COPY_VA_LIST();
    MAKE_STACK_FRAME(frame,closure->asSmartPtr().raw_(),sizeof...(ARGS)+1);
    MAKE_SPECIAL_BINDINGS_HOLDER(numSpecialBindings, specialBindingsVLA,
                                 lisp_lambda_list_handler_number_of_specials(closure->_lambdaListHandler));
    core::StackFrameDynamicScopeManager scope(numSpecialBindings,specialBindingsVLA,frame);
    lambdaListHandler_createBindings(closure->asSmartPtr(),closure->_lambdaListHandler,scope,LCC_PASS_ARGS_LLH);
    core::MultipleValues& returnValues = core::lisp_multipleValues();
    OT& oto = *gc::As<gctools::smart_ptr<OT>>(frame->arg(0));
    std::tuple<translate::from_object<ARGS>...> all_args = clbind::arg_tuple<1,clbind::policies<>,ARGS...>::go(frame->arguments(0));
    return clbind::method_apply_and_return<RT,core::policy::clasp,decltype(closure->mptr),OT,decltype(all_args)>::go(returnValues,std::move(closure->mptr),std::move(oto),std::move(all_args));
  }
};


template <int DispatchOn, typename RT, typename OT, typename... ARGS>
class TEMPLATED_FUNCTION_VariadicMethoid <DispatchOn, core::policy::clasp, RT(OT::*)(ARGS...) const>
: public BuiltinClosure_O {
public:
  typedef TEMPLATED_FUNCTION_VariadicMethoid<DispatchOn, core::policy::clasp,RT(OT::*)(ARGS...) const > MyType;
  typedef BuiltinClosure_O TemplatedBase;
public:
  virtual const char* describe() const {return "VariadicMethoid";};
  typedef RT(OT::*MethodType)(ARGS...) const ;
  MethodType mptr;
public:
  enum { NumParams = sizeof...(ARGS)+1 };
  TEMPLATED_FUNCTION_VariadicMethoid(GlobalEntryPoint_sp ep, MethodType ptr) : BuiltinClosure_O(ep), mptr(ptr) {
    this->validateCodePointer((void**)&this->mptr,sizeof(this->mptr));
  };
  virtual size_t templatedSizeof() const { return sizeof(*this);};
  virtual void fixupInternalsForSnapshotSaveLoad( snapshotSaveLoad::Fixup* fixup ) {
    this->fixupOneCodePointer(fixup,(void**)&this->mptr,sizeof(this->mptr));
  }
  static inline gctools::return_type LISP_CALLING_CONVENTION()
  {
    MyType* closure = gctools::untag_general<MyType*>((MyType*)lcc_closure);
    INCREMENT_FUNCTION_CALL_COUNTER(closure);
    COPY_VA_LIST();
    MAKE_STACK_FRAME(frame,closure->asSmartPtr().raw_(),sizeof...(ARGS)+1);
    MAKE_SPECIAL_BINDINGS_HOLDER(numSpecialBindings, specialBindingsVLA,
                                 lisp_lambda_list_handler_number_of_specials(closure->_lambdaListHandler));
    core::StackFrameDynamicScopeManager scope(numSpecialBindings,specialBindingsVLA,frame);
    lambdaListHandler_createBindings(closure->asSmartPtr(),closure->_lambdaListHandler,scope,LCC_PASS_ARGS_LLH);
    core::MultipleValues& returnValues = core::lisp_multipleValues();
    OT& oto = *gc::As<gctools::smart_ptr<OT>>(frame->arg(0));
    std::tuple<translate::from_object<ARGS>...> all_args = clbind::arg_tuple<1,clbind::policies<>,ARGS...>::go(frame->arguments(0));
    return clbind::method_apply_and_return<RT,core::policy::clasp,decltype(closure->mptr),OT,decltype(all_args)>::go(returnValues,std::move(closure->mptr),std::move(oto),std::move(all_args));
  }
};




#else

#include <clasp/core/wrappers_methoids.h>
#endif
};

namespace core {

  inline void wrap_translator(const string &packageName, const string &name, core::T_O* (*fp)(core::T_O*), const string& filename,  const string &arguments = "", const string &declares = "", const string &docstring = "", const string &sourceFile = "", int sourceLine = 0) {
    Symbol_sp symbol = lispify_intern(name, packageName);
    using VariadicType = TranslationFunctor_O;
    GlobalEntryPoint_sp entryPoint = makeGlobalEntryPointAndFunctionDescription<VariadicType>(symbol);
    BuiltinClosure_sp f = gctools::GC<VariadicType>::allocate(entryPoint,fp);
    lisp_defun(symbol, packageName, f, arguments, declares, docstring, sourceFile, sourceLine, 1 );
    validateFunctionDescription(__FILE__,__LINE__,f);
  }



// this is used in gc_interface.cc expose_function
 template <typename RT, typename... ARGS>
void wrap_function(const string &packageName, const string &name, RT (*fp)(ARGS...), const string &arguments = "", const string &declares = "", const string &docstring = "", const string &sourceFile = "", int sourceLine = 0) {
   maybe_register_symbol_using_dladdr(*(void**)&fp,sizeof(fp),name);
   Symbol_sp symbol = _lisp->intern(name, packageName);
   using VariadicType = clbind::TEMPLATED_FUNCTION_VariadicFunctor<RT(*)(ARGS...),core::policy::clasp,clbind::pureOutsPack<>>;
   GlobalEntryPoint_sp entryPoint = makeGlobalEntryPointAndFunctionDescription<VariadicType>( symbol );
   BuiltinClosure_sp f = gc::As<BuiltinClosure_sp>(gctools::GC<VariadicType>::allocate(entryPoint,fp));
   lisp_defun(symbol, packageName, f, arguments, declares, docstring, sourceFile, sourceLine, sizeof...(ARGS));
   validateFunctionDescription(__FILE__,__LINE__,f);
 }

// this is used in gc_interface.cc expose_function_setf
  template <typename RT, typename... ARGS>
void wrap_function_setf(const string &packageName, const string &name, RT (*fp)(ARGS...), const string &arguments = "", const string &declares = "", const string &docstring = "", const string &sourceFile = "", int sourceLine = 0) {
    maybe_register_symbol_using_dladdr(*(void**)&fp,sizeof(fp),name);
  Symbol_sp symbol = _lisp->intern(name, packageName);
  using VariadicType = clbind::TEMPLATED_FUNCTION_VariadicFunctor<RT(*)(ARGS...),core::policy::clasp,clbind::pureOutsPack<>>;
  GlobalEntryPoint_sp entryPoint = makeGlobalEntryPointAndFunctionDescription<VariadicType>( symbol );
  BuiltinClosure_sp f = gc::As<BuiltinClosure_sp>(gctools::GC<VariadicType>::allocate(entryPoint,fp));
  lisp_defun_setf(symbol, packageName, f, arguments, declares, docstring, sourceFile, sourceLine, sizeof...(ARGS));
  validateFunctionDescription(__FILE__,__LINE__,f);
  }



};

template <int DispatchOn, typename Policies, typename FunctionPtrType>
class gctools::GCStamp<core::TEMPLATED_FUNCTION_VariadicMethoid<DispatchOn, Policies, FunctionPtrType>> {
public:
  static gctools::GCStampEnum const StampWtag = gctools::GCStamp<typename core::TEMPLATED_FUNCTION_VariadicMethoid<DispatchOn, Policies, FunctionPtrType>::TemplatedBase>::Stamp;
};

namespace core {

class Function_O;

// -----------------------------------
//
// Macro stuff
//
//
// Wrapper for ActivationFrameMacroPtr

// basically like wrap_function.
inline void defmacro(const string &packageName, const string &name, T_mv (*mp)(List_sp, T_sp env), const string &arguments, const string &declares, const string &docstring, const string &sourcePathname, int lineno) {
  maybe_register_symbol_using_dladdr(*(void**)&mp,sizeof(mp),name);
  Symbol_sp symbol = lispify_intern(name, packageName);
  using VariadicType = clbind::TEMPLATED_FUNCTION_VariadicFunctor<T_mv(*)(List_sp,T_sp),core::policy::clasp,clbind::pureOutsPack<>>;
  GlobalEntryPoint_sp entryPoint = makeGlobalEntryPointAndFunctionDescription<VariadicType>( symbol );
  BuiltinClosure_sp f = gc::As<BuiltinClosure_sp>(gc::GC<VariadicType>::allocate(entryPoint,mp));
  lisp_defmacro(symbol, packageName, f, arguments, declares, docstring);
  validateFunctionDescription(__FILE__,__LINE__,f);
}
 
template <int N>
struct DispatchOn {
  enum { value = N };
};

// ----------------------------------------
// ----------------------------------------
// ----------------------------------------
// ----------------------------------------


extern Symbol_sp& _sym_STARallCxxClassesSTAR;


template <typename OT>
class class_ {
public:
  typedef OT wrapped_type;

private:
  Symbol_sp _ClassSymbol;

public:
  void setup_class(const string &makerName = "") {
    _G();
    if (IS_SYMBOL_UNDEFINED(OT::static_classSymbol())) {
      SIMPLE_ERROR_SPRINTF("Attempting to add methods for class that isn't defined yet");
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
      std::string magic_maker_name = core::magic_name(makerName,OT::static_packageName());
      std::string pkg_part;
      std::string symbol_part;
      core::colon_split( magic_maker_name, pkg_part, symbol_part);
      wrap_function(pkg_part,symbol_part, &new_LispObject<OT>);
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

  class_(const string &makerName) {
    _G();
    this->setup_class(makerName);
  }
 
  // non-const function dispatch on parameter 0
  template <typename RT, class... ARGS>
  class_& def(string const &name, RT (OT::*mp)(ARGS...),
              string const &lambda_list = "", const string &declares = "", const string &docstring = "", bool autoExport = true)
  {
    maybe_register_symbol_using_dladdr(*(void**)&mp,sizeof(mp),name);
    std::string pkgName;
    std::string symbolName;
    core::colon_split(name,pkgName,symbolName);
    if (pkgName == "") {
      pkgName = symbol_packageName(this->_ClassSymbol);
    }
    Symbol_sp symbol = _lisp->intern(symbolName,pkgName);
    using VariadicType = TEMPLATED_FUNCTION_VariadicMethoid<0, core::policy::clasp, RT (OT::*)(ARGS...)>;
    GlobalEntryPoint_sp entryPoint = makeGlobalEntryPointAndFunctionDescription<VariadicType>(symbol);
    BuiltinClosure_sp m = gc::As<BuiltinClosure_sp>(gc::GC<VariadicType>::allocate(entryPoint,mp));
    lisp_defineSingleDispatchMethod(symbol, this->_ClassSymbol, m, 0, true, lambda_list, declares, docstring, autoExport, sizeof...(ARGS)+1);
    validateFunctionDescription(__FILE__,__LINE__,m);
    return *this;
  }
 
  // const function dispatch on parameter 0
  template <typename RT, class... ARGS>
  class_& def(string const &name, RT (OT::*mp)(ARGS...) const,
              string const &lambda_list = "", const string &declares = "", const string &docstring = "", bool autoExport = true)
  {
    maybe_register_symbol_using_dladdr(*(void**)&mp,sizeof(mp),name);
    std::string pkgName;
    std::string symbolName;
    core::colon_split(name,pkgName,symbolName);
    if (pkgName == "") {
      pkgName = symbol_packageName(this->_ClassSymbol);
    }
    Symbol_sp symbol = _lisp->intern(symbolName,pkgName);
    using VariadicType = TEMPLATED_FUNCTION_VariadicMethoid<0, core::policy::clasp, RT (OT::*)(ARGS...) const>;
    GlobalEntryPoint_sp entryPoint = makeGlobalEntryPointAndFunctionDescription<VariadicType>(symbol);
    BuiltinClosure_sp m = gc::As<BuiltinClosure_sp>(gc::GC<VariadicType>::allocate(entryPoint,mp));
    lisp_defineSingleDispatchMethod(symbol, this->_ClassSymbol, m, 0, true, lambda_list, declares, docstring, autoExport, sizeof...(ARGS)+1);
    validateFunctionDescription(__FILE__,__LINE__,m);
    return *this;
  }
};

template <typename oclass>
void defaultExposeCando(LispPtr lisp) {
  _G();
  // Only expose the class, don't create any methods
  // By default put class in the Cl package
  class_<oclass>();
}

struct EnumValueDefinition {
  string _Name;
  int _Value;
};

template <typename X>
class enum_ {
private:
  SymbolToEnumConverter_sp _Converter;
  Symbol_sp _PredefinedConverterSymbolId;

public:
  enum_(Symbol_sp symbol, const string &title) {
    _G();
    this->_PredefinedConverterSymbolId = symbol;
    this->_Converter = SymbolToEnumConverter_O::create(title);
    lisp_defparameter(symbol, this->_Converter);
  }

  enum_ &value(Symbol_sp const &sym, X value) {
    _G();
    lisp_extendSymbolToEnumConverter(this->_Converter, sym, sym, static_cast<int>(value));
    return *this;
  }
  enum_ &value(Symbol_sp const &name, Symbol_sp const &archiveName, X value) {
    _G();
    lisp_extendSymbolToEnumConverter(this->_Converter, name, archiveName, value);
    return *this;
  }
  Symbol_sp symbolFromEnum(int value) {
    _G();
    return lisp_lookupSymbolForEnum(this->_PredefinedConverterSymbolId, (int)(value));
  }
};
};

//ostream& operator<<(ostream& out, gctools::smart_ptr<core::T_O>

#endif // wrappers_h
