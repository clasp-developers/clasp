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
#ifndef external_wrappers_H
#define external_wrappers_H


#ifdef DEBUG_FRAME
#define DEBUG_DUMP_FRAME(frame) frame->dump();
#else
#define DEBUG_DUMP_FRAME(frame)
#endif

#include <clasp/core/lispDefinitions.h>

#include <clasp/core/wrappers.h>
//#include "clbind.h"

namespace core {

template <typename Policies, typename OT, typename MethodPtrType>
class TEMPLATED_FUNCTION_IndirectMethoid : public BuiltinClosure_O {
  typedef BuiltinClosure_O TemplatedBase;
  virtual size_t templatedSizeof() const { return sizeof(*this); };
  virtual const char *describe() const { return "IndirectMethoid"; };
};
};

#if 1
namespace core {
//#include <clasp/core/external_wrappers_indirect_methoids.h>

template <typename Policies, typename RT, typename OT, typename... ARGS>
class TEMPLATED_FUNCTION_IndirectMethoid <Policies, OT, RT(OT::ExternalType::*)(ARGS...)> : public BuiltinClosure_O {
public:
  typedef TEMPLATED_FUNCTION_IndirectMethoid<Policies,OT,RT(OT::ExternalType::*)(ARGS...) > MyType;
  typedef BuiltinClosure_O TemplatedBase;
public:
  virtual const char* describe() const {return "IndirectVariadicMethoid";};
  typedef RT(OT::ExternalType::*MethodType)(ARGS...) ;
  MethodType mptr;
public:
  enum { NumParams = sizeof...(ARGS)+1 };
  TEMPLATED_FUNCTION_IndirectMethoid(GlobalEntryPoint_sp ep, MethodType ptr) : BuiltinClosure_O(ep), mptr(ptr) {
    this->validateCodePointer((void**)&this->mptr,sizeof(this->mptr));
  };
  virtual size_t templatedSizeof() const { return sizeof(*this);};
  virtual void fixupInternalsForSnapshotSaveLoad( snapshotSaveLoad::Fixup* fixup ) {
    this->fixupOneCodePointer( fixup,(void**)&this->mptr,sizeof(this->mptr));
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
    DEBUG_DUMP_FRAME(frame);
    core::MultipleValues& returnValues = core::lisp_multipleValues();
    OT* otep  = &*gc::As<gctools::smart_ptr<OT>>(frame->arg(0));
    std::tuple<translate::from_object<ARGS>...> all_args = clbind::arg_tuple<1,Policies,ARGS...>::go(frame->arguments(0));
    return clbind::external_method_apply_and_return<Policies,RT,decltype(closure->mptr),OT,decltype(all_args)>::go(returnValues,std::move(closure->mptr),otep,std::move(all_args));
  }
};

template <typename Policies, typename RT, typename OT, typename... ARGS>
class TEMPLATED_FUNCTION_IndirectMethoid <Policies, OT, RT(OT::ExternalType::*)(ARGS...) const> : public BuiltinClosure_O {
public:
  typedef TEMPLATED_FUNCTION_IndirectMethoid<Policies,OT,RT(OT::ExternalType::*)(ARGS...) const > MyType;
  typedef BuiltinClosure_O TemplatedBase;
public:
  virtual const char* describe() const {return "IndirectVariadicMethoid";};
  typedef RT(OT::ExternalType::*MethodType)(ARGS...) const ;
  MethodType mptr;
public:
  enum { NumParams = sizeof...(ARGS)+1 };
  TEMPLATED_FUNCTION_IndirectMethoid(GlobalEntryPoint_sp ep, MethodType ptr) : BuiltinClosure_O(ep), mptr(ptr) {
    this->validateCodePointer((void**)&this->mptr,sizeof(this->mptr));
  };
  virtual size_t templatedSizeof() const { return sizeof(*this);};
  virtual void fixupInternalsForSnapshotSaveLoad( snapshotSaveLoad::Fixup* fixup ) {
    this->fixupOneCodePointer( fixup,(void**)&this->mptr,sizeof(this->mptr));
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
    DEBUG_DUMP_FRAME(frame);
    core::MultipleValues& returnValues = core::lisp_multipleValues();
    OT* otep = &*gc::As<gctools::smart_ptr<OT>>(frame->arg(0));
    std::tuple<translate::from_object<ARGS>...> all_args = clbind::arg_tuple<1,Policies,ARGS...>::go(frame->arguments(0));
    return clbind::external_method_apply_and_return<Policies,RT,decltype(closure->mptr),OT,decltype(all_args)>::go(returnValues,std::move(closure->mptr),otep,std::move(all_args));
  }
};

};

#else
namespace core {
#include <clasp/core/external_wrappers_indirect_methoids.h>
};
#endif
namespace core {
template <class D, class C>
class TEMPLATED_FUNCTION_GetterMethoid : public BuiltinClosure_O {
public:
  typedef BuiltinClosure_O TemplatedBase;

public:
  //        typedef std::function<void (OT& ,)> Type;
  typedef D(C::*MemPtr);
  MemPtr mptr;
  TEMPLATED_FUNCTION_GetterMethoid(core::GlobalEntryPoint_sp ep, MemPtr ptr) : BuiltinClosure_O(ep), mptr(ptr){
    this->validateCodePointer((void**)&this->mptr,sizeof(this->mptr));
  };
  virtual size_t templatedSizeof() const { return sizeof(*this); };
  virtual void fixupInternalsForSnapshotSaveLoad( snapshotSaveLoad::Fixup* fixup ) {
    this->fixupOneCodePointer(fixup,(void**)&this->mptr,sizeof(this->mptr));
  }
  static inline LCC_RETURN LISP_CALLING_CONVENTION() {
    SIMPLE_ERROR_SPRINTF("What do I do here");
  }
};
};

template <typename Policies, typename OT, typename MethodPtrType>
class gctools::GCStamp<core::TEMPLATED_FUNCTION_IndirectMethoid<Policies, OT, MethodPtrType>> {
public:
  static gctools::GCStampEnum const StampWtag = gctools::GCStamp<typename core::TEMPLATED_FUNCTION_IndirectMethoid<Policies, OT, MethodPtrType>::TemplatedBase>::Stamp;
};

namespace core {

// ----------------------------------------
// ----------------------------------------
// ----------------------------------------
// ----------------------------------------
extern Symbol_sp& _sym_STARallCxxClassesSTAR;

template <typename OT>
class externalClass_ {
private:
  Symbol_sp _ClassSymbol;

public:
  void setup_class(const string &makerName) {
    _G();
    if (IS_SYMBOL_UNDEFINED(OT::static_classSymbol())) {
      SIMPLE_ERROR_SPRINTF("Attempting to add methods for class that isn't defined yet");
    }
    this->_ClassSymbol = OT::static_classSymbol();
    reg::lisp_registerClassSymbol<OT>(this->_ClassSymbol);
    /*! Accumulate all of the classes in reverse order of how they were initialized
              in the core::*all-cxx-classes* variable */
    if (_sym_STARallCxxClassesSTAR->symbolValueUnsafe()) {
      _sym_STARallCxxClassesSTAR->setf_symbolValue(Cons_O::create(OT::static_classSymbol(), _sym_STARallCxxClassesSTAR->symbolValue()));
    }

    //
    // If the class isn't in the class table then add it
    //
    T_sp theClass = lisp_boot_findClassBySymbolOrNil(OT::static_classSymbol());
    if (theClass.nilp()) {
      LOG(BF("Adding class(%s) to environment") % OT::static_className());
      lisp_addClassSymbol(OT::static_classSymbol(),
                          OT::staticCreator(),
                          OT::Bases::baseClass1Id() );
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

  externalClass_(const string &makerName) {
    _G();
    this->setup_class(makerName);
  }

  template <typename RT, class... ARGS>
  externalClass_ &def(string const &name, RT (OT::*mp)(ARGS...), string const &lambda_list = "", const string &declares = "", const string &docstring = "", bool autoExport = true) {
    maybe_register_symbol_using_dladdr(*(void**)&mp,sizeof(mp),name);
    Symbol_sp symbol = lispify_intern(name, symbol_packageName(this->_ClassSymbol));
    using VariadicType = TEMPLATED_FUNCTION_VariadicMethoid<0, core::policy::clasp, RT (OT::*)(ARGS...)>;
    GlobalEntryPoint_sp entryPoint = makeGlobalEntryPointAndFunctionDescription<VariadicType>(symbol);
    BuiltinClosure_sp m = gc::As<BuiltinClosure_sp>(gc::GC<VariadicType>::allocate(entryPoint, mp));
    lisp_defineSingleDispatchMethod(symbol, this->_ClassSymbol, m, 0, true, lambda_list, declares, docstring, autoExport, sizeof...(ARGS)+1);
    validateFunctionDescription(__FILE__,__LINE__,m);
    return *this;
  }

  template <typename RT, class... ARGS>
  externalClass_ &def(string const &name, RT (OT::*mp)(ARGS...) const,
                      string const &lambda_list = "", const string &declares = "", const string &docstring = "", bool autoExport = true) {
    maybe_register_symbol_using_dladdr(*(void**)&mp,sizeof(mp),name);
    Symbol_sp symbol = lispify_intern(name, symbol_packageName(this->_ClassSymbol));
    using VariadicType = TEMPLATED_FUNCTION_VariadicMethoid<0, core::policy::clasp, RT (OT::*)(ARGS...) const>;
    GlobalEntryPoint_sp entryPoint = makeGlobalEntryPointAndFunctionDescription<VariadicType>(symbol);
    BuiltinClosure_sp m = gc::As<BuiltinClosure_sp>(gctools::GC<VariadicType>::allocate(entryPoint, mp));
    lisp_defineSingleDispatchMethod(symbol, this->_ClassSymbol, m, 0, true, lambda_list, declares, docstring, autoExport, sizeof...(ARGS)+1);
    validateFunctionDescription(__FILE__,__LINE__,m);
    return *this;
  }

  template <typename RT, class... ARGS>
  externalClass_ &def(const string &name, RT (OT::ExternalType::*mp)(ARGS...),
                      const string &lambda_list = "", const string &declares = "", const string &docstring = "", bool autoExport = true) {
    maybe_register_symbol_using_dladdr(*(void**)&mp,sizeof(mp),name);
    Symbol_sp symbol = lispify_intern(name, symbol_packageName(this->_ClassSymbol));
    using VariadicType = TEMPLATED_FUNCTION_IndirectMethoid<clbind::policies<>, OT, RT (OT::ExternalType::*)(ARGS...)>;
    GlobalEntryPoint_sp entryPoint = makeGlobalEntryPointAndFunctionDescription<VariadicType>(symbol);
    BuiltinClosure_sp m = gc::As<BuiltinClosure_sp>(gctools::GC<VariadicType>::allocate(entryPoint, mp));
    lisp_defineSingleDispatchMethod(symbol, this->_ClassSymbol, m, 0, true, lambda_list, declares, docstring, autoExport, sizeof...(ARGS)+1);
    validateFunctionDescription(__FILE__,__LINE__,m);
    return *this;
  }

  template <typename RT, class... ARGS>
  externalClass_ &def(const string &name, RT (OT::ExternalType::*mp)(ARGS...) const,
                      const string &lambda_list = "", const string &declares = "", const string &docstring = "", bool autoExport = true) {
    maybe_register_symbol_using_dladdr(*(void**)&mp,sizeof(mp),name);
    Symbol_sp symbol = lispify_intern(name, symbol_packageName(this->_ClassSymbol));
    using VariadicType = TEMPLATED_FUNCTION_IndirectMethoid<clbind::policies<>, OT, RT (OT::ExternalType::*)(ARGS...) const>;
    GlobalEntryPoint_sp entryPoint = makeGlobalEntryPointAndFunctionDescription<VariadicType>(symbol);
    BuiltinClosure_sp m = gc::As<BuiltinClosure_sp>(gctools::GC<VariadicType>::allocate(entryPoint, mp));
    lisp_defineSingleDispatchMethod(symbol, this->_ClassSymbol, m, 0, true, lambda_list, declares, docstring, autoExport, sizeof...(ARGS)+1);
    validateFunctionDescription(__FILE__,__LINE__,m);
    return *this;
  }

  template <class C, class D>
  externalClass_ &def_readonly(string const &name, D C::*mem_ptr) {
    return *this;
  }
};
};

#endif // external_wrappers_h
