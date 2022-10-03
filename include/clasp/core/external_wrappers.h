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


#include <clasp/core/lispDefinitions.h>

#include <clasp/core/wrappers.h>
//#include "clbind.h"

namespace core {
template < typename MethodPtrType, typename Policies, typename OT, typename Wrapper >
class TEMPLATED_FUNCTION_IndirectMethoid;
};

namespace core {
template <typename RT, typename OT, typename... ARGS, typename Policies >
class TEMPLATED_FUNCTION_IndirectMethoid < RT(OT::ExternalType::*)(ARGS...), Policies, OT, clbind::LambdaListHandlerWrapper > : public GlobalEntryPointBase_O {
public:
  typedef TEMPLATED_FUNCTION_IndirectMethoid< RT(OT::ExternalType::*)(ARGS...), Policies, OT, clbind::LambdaListHandlerWrapper > MyType;
  typedef RT(OT::ExternalType::*MethodType)(ARGS...) ;
  typedef GlobalEntryPointBase_O TemplatedBase;
public:
  MethodType           mptr;
public:

  enum { NumParams = sizeof...(ARGS)+1 };

  TEMPLATED_FUNCTION_IndirectMethoid(MethodType ptr, core::FunctionDescription_sp fdesc, core::T_sp code)
      : mptr(ptr), GlobalEntryPointBase_O(fdesc,core::ClaspXepFunction::make<MyType>(),code)  {
    this->validateCodePointer((void**)&this->mptr,sizeof(this->mptr));
  };

  virtual const char* describe() const {return "IndirectVariadicMethoid";};

  virtual size_t templatedSizeof() const { return sizeof(*this);};

  void fixupInternalsForSnapshotSaveLoad( snapshotSaveLoad::Fixup* fixup ) {
    this->TemplatedBase::fixupInternalsForSnapshotSaveLoad(fixup);
    this->fixupOneCodePointer( fixup, (void**)&this->mptr );
  };

  static inline LCC_RETURN entry_point_n(core::T_O* lcc_closure, size_t lcc_nargs, core::T_O** lcc_args )
  {
    MyType* closure = gctools::untag_general<MyType*>((MyType*)lcc_closure);
    INCREMENT_FUNCTION_CALL_COUNTER(closure);
    MAKE_STACK_FRAME(frame,sizeof...(ARGS)+1);
    MAKE_SPECIAL_BINDINGS_HOLDER(numSpecialBindings, specialBindingsVLA,
                                 lisp_lambdaListHandlerNumberOfSpecialVariables(closure->_lambdaListHandler));
    core::StackFrameDynamicScopeManager scope(closure->_lambdaListHandler, numSpecialBindings, specialBindingsVLA, frame, sizeof...(ARGS)+1);
    lambdaListHandler_createBindings(closure->asSmartPtr(),closure->_lambdaListHandler,&scope,lcc_nargs, lcc_args );
    OT* otep  = &*gc::As<gctools::smart_ptr<OT>>(frame->arg(0));
    std::tuple<translate::from_object<ARGS>...> all_args = clbind::arg_tuple<1,Policies,ARGS...>::goFrame(frame->arguments(0));
    return clbind::external_method_apply_and_return<Policies,RT,decltype(closure->mptr),OT,decltype(all_args)>::go(std::move(closure->mptr),otep,std::move(all_args));
  }
  static inline LISP_ENTRY_0() {
    return general_entry_point_redirect_0(lcc_closure);
  }
  static inline LISP_ENTRY_1() {
    return general_entry_point_redirect_1(lcc_closure,lcc_farg0);
  }
  static inline LISP_ENTRY_2() {
    return general_entry_point_redirect_2(lcc_closure,lcc_farg0,lcc_farg1);
  }
  static inline LISP_ENTRY_3() {
    return general_entry_point_redirect_3(lcc_closure,lcc_farg0,lcc_farg1,lcc_farg2);
  }
  static inline LISP_ENTRY_4() {
    return general_entry_point_redirect_4(lcc_closure,lcc_farg0,lcc_farg1,lcc_farg2,lcc_farg3);
  }
  static inline LISP_ENTRY_5() {
    return general_entry_point_redirect_5(lcc_closure,lcc_farg0,lcc_farg1,lcc_farg2,lcc_farg3,lcc_farg4);
  }
};
};

namespace core {
template <typename RT, typename OT, typename... ARGS, typename Policies >
class TEMPLATED_FUNCTION_IndirectMethoid < RT(OT::ExternalType::*)(ARGS...) const, Policies, OT, clbind::LambdaListHandlerWrapper > : public GlobalEntryPointBase_O {
public:
  typedef TEMPLATED_FUNCTION_IndirectMethoid< RT(OT::ExternalType::*)(ARGS...) const, Policies, OT, clbind::LambdaListHandlerWrapper > MyType;
  typedef RT(OT::ExternalType::*MethodType)(ARGS...) const;
  typedef GlobalEntryPointBase_O TemplatedBase;
public:
  MethodType           mptr;
public:

  enum { NumParams = sizeof...(ARGS)+1 };

  TEMPLATED_FUNCTION_IndirectMethoid(MethodType ptr, core::FunctionDescription_sp fdesc, core::T_sp code)
      : mptr(ptr), GlobalEntryPointBase_O(fdesc,core::ClaspXepFunction::make<MyType>(),code)  {
    this->validateCodePointer((void**)&this->mptr,sizeof(this->mptr));
  };

  virtual const char* describe() const {return "IndirectVariadicMethoid";};

  virtual size_t templatedSizeof() const { return sizeof(*this);};

  void fixupInternalsForSnapshotSaveLoad( snapshotSaveLoad::Fixup* fixup ) {
    this->TemplatedBase::fixupInternalsForSnapshotSaveLoad(fixup);
    this->fixupOneCodePointer( fixup, (void**)&this->mptr );
  };

  static inline LCC_RETURN entry_point_n(core::T_O* lcc_closure, size_t lcc_nargs, core::T_O** lcc_args )
  {
    MyType* closure = gctools::untag_general<MyType*>((MyType*)lcc_closure);
    INCREMENT_FUNCTION_CALL_COUNTER(closure);
    MAKE_STACK_FRAME(frame,sizeof...(ARGS)+1);
    MAKE_SPECIAL_BINDINGS_HOLDER(numSpecialBindings, specialBindingsVLA,
                                 lisp_lambdaListHandlerNumberOfSpecialVariables(closure->_lambdaListHandler));
    core::StackFrameDynamicScopeManager scope(closure->_lambdaListHandler, numSpecialBindings, specialBindingsVLA, frame, sizeof...(ARGS)+1);
    lambdaListHandler_createBindings(closure->asSmartPtr(),closure->_lambdaListHandler,&scope,lcc_nargs, lcc_args );
    OT* otep  = &*gc::As<gctools::smart_ptr<OT>>(frame->arg(0));
    std::tuple<translate::from_object<ARGS>...> all_args = clbind::arg_tuple<1,Policies,ARGS...>::goFrame(frame->arguments(0));
    return clbind::external_method_apply_and_return<Policies,RT,decltype(closure->mptr),OT,decltype(all_args)>::go(std::move(closure->mptr),otep,std::move(all_args));
  }
  static inline LISP_ENTRY_0() {
    return general_entry_point_redirect_0(lcc_closure);
  }
  static inline LISP_ENTRY_1() {
    return general_entry_point_redirect_1(lcc_closure,lcc_farg0);
  }
  static inline LISP_ENTRY_2() {
    return general_entry_point_redirect_2(lcc_closure,lcc_farg0,lcc_farg1);
  }
  static inline LISP_ENTRY_3() {
    return general_entry_point_redirect_3(lcc_closure,lcc_farg0,lcc_farg1,lcc_farg2);
  }
  static inline LISP_ENTRY_4() {
    return general_entry_point_redirect_4(lcc_closure,lcc_farg0,lcc_farg1,lcc_farg2,lcc_farg3);
  }
  static inline LISP_ENTRY_5() {
    return general_entry_point_redirect_5(lcc_closure,lcc_farg0,lcc_farg1,lcc_farg2,lcc_farg3,lcc_farg4);
  }
};
};



namespace core {
template <class D, class C>
class TEMPLATED_FUNCTION_GetterMethoid : public GlobalEntryPointBase_O {
public:
  typedef GlobalEntryPointBase_O TemplatedBase;
  typedef TEMPLATED_FUNCTION_GetterMethoid< D, C > MyType;

public:
  //        typedef std::function<void (OT& ,)> Type;
  typedef D(C::*MemPtr);
  MemPtr mptr;

public:
  
  TEMPLATED_FUNCTION_GetterMethoid(MemPtr ptr, core::FunctionDescription_sp fdesc, core::T_sp code)
      : mptr(ptr), GlobalEntryPointBase_O(fdesc,core::ClaspXepFunction::make<MyType>(),code) {
    this->validateCodePointer((void**)&this->mptr,sizeof(this->mptr));
  }
  
  virtual size_t templatedSizeof() const { return sizeof(*this); };
  
  void fixupInternalsForSnapshotSaveLoad( snapshotSaveLoad::Fixup* fixup ) {
    this->TemplatedBase::fixupInternalsForSnapshotSaveLoad(fixup);
    printf("%s:%d:%s What do we do with mptr %p\n", __FILE__, __LINE__, __FUNCTION__, *(void**)&this->mptr );
    // this->fixupOneCodePointer( fixup, (void**)&this->mptr );
  };
  
  static inline LCC_RETURN LISP_CALLING_CONVENTION() {
    SIMPLE_ERROR_SPRINTF("What do I do here");
  }
  static inline LISP_ENTRY_0() {
    return entry_point_n(lcc_closure,0,NULL);
  }
  static inline LISP_ENTRY_1() {
    core::T_O* args[1] = {lcc_farg0};
    return entry_point_n(lcc_closure,1,args);
  }
  static inline LISP_ENTRY_2() {
    core::T_O* args[2] = {lcc_farg0,lcc_farg1};
    return entry_point_n(lcc_closure,2,args);
  }
  static inline LISP_ENTRY_3() {
    core::T_O* args[3] = {lcc_farg0,lcc_farg1,lcc_farg2};
    return entry_point_n(lcc_closure,3,args);
  }
  static inline LISP_ENTRY_4() {
    core::T_O* args[4] = {lcc_farg0,lcc_farg1,lcc_farg2,lcc_farg3};
    return entry_point_n(lcc_closure,4,args);
  }
  static inline LISP_ENTRY_5() {
    core::T_O* args[5] = {lcc_farg0,lcc_farg1,lcc_farg2,lcc_farg3,lcc_farg4};
    return entry_point_n(lcc_closure,5,args);
  }
};
};

template <typename Policies, typename OT, typename MethodPtrType, typename ArgumentHandler >
class gctools::GCStamp<core::TEMPLATED_FUNCTION_IndirectMethoid<MethodPtrType, Policies, OT, ArgumentHandler >> {
public:
  static gctools::GCStampEnum const StampWtag = gctools::GCStamp<typename core::TEMPLATED_FUNCTION_IndirectMethoid< MethodPtrType, Policies, OT, ArgumentHandler >::TemplatedBase>::StampWtag;
};

template <typename Policies, typename OT, typename MethodPtrType, typename ArgumentHandler >
struct gctools::Inherits<typename core::TEMPLATED_FUNCTION_IndirectMethoid<MethodPtrType, Policies, OT, ArgumentHandler >::TemplatedBase, core::TEMPLATED_FUNCTION_IndirectMethoid<MethodPtrType, Policies, OT, ArgumentHandler >> : public std::true_type {};


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
      LOG(("Adding class(%s) to environment") , OT::static_className());
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
  externalClass_ &def(const names_& names, RT (OT::*mp)(ARGS...), string const &lambda_list = "", const string &declares = "", const string &docstring = "", bool autoExport = true) {
    for ( auto& name : names._Names ) {
      maybe_register_symbol_using_dladdr(*(void**)&mp,sizeof(mp),name);
      Symbol_sp symbol = lispify_intern(name, symbol_packageName(this->_ClassSymbol));
      using VariadicType = TEMPLATED_FUNCTION_VariadicMethoid<0, RT (OT::*)(ARGS...), core::policy::clasp_policy, clbind::DefaultWrapper>;
      FunctionDescription_sp fdesc = makeFunctionDescription(symbol,nil<T_O>());
      auto entry = gctools::GC<VariadicType>::allocate(mp,fdesc,nil<T_O>());
      lisp_defineSingleDispatchMethod(clbind::DefaultWrapper(), symbol, this->_ClassSymbol, entry, 0, true, lambda_list, declares, docstring, autoExport, sizeof...(ARGS)+1);
    }
    return *this;
  }

  template <typename RT, class... ARGS>
  externalClass_ &def(const names_& names, RT (OT::*mp)(ARGS...) const,
                      string const &lambda_list = "", const string &declares = "", const string &docstring = "", bool autoExport = true) {
    for ( auto& name : names._Names ) {
      maybe_register_symbol_using_dladdr(*(void**)&mp,sizeof(mp),name);
      Symbol_sp symbol = lispify_intern(name, symbol_packageName(this->_ClassSymbol));
      using VariadicType = TEMPLATED_FUNCTION_VariadicMethoid<0, RT (OT::*)(ARGS...) const, core::policy::clasp_policy, clbind::DefaultWrapper >;
      FunctionDescription_sp fdesc = makeFunctionDescription(symbol,nil<T_O>());
      auto entry = gctools::GC<VariadicType>::allocate(mp,fdesc,nil<T_O>());
      lisp_defineSingleDispatchMethod(clbind::DefaultWrapper(), symbol, this->_ClassSymbol, entry, 0, true, lambda_list, declares, docstring, autoExport, sizeof...(ARGS)+1);
    }
    return *this;
  }

  template <typename RT, class... ARGS>
  externalClass_ &def(const names_& names, RT (OT::ExternalType::*mp)(ARGS...),
                      const string &lambda_list = "", const string &declares = "", const string &docstring = "", bool autoExport = true) {
    for ( auto& name : names._Names ) {
      maybe_register_symbol_using_dladdr(*(void**)&mp,sizeof(mp),name);
      Symbol_sp symbol = lispify_intern(name, symbol_packageName(this->_ClassSymbol));
      using VariadicType = TEMPLATED_FUNCTION_IndirectMethoid< RT (OT::ExternalType::*)(ARGS...), clbind::policies<>, OT, clbind::DefaultWrapper>;
      FunctionDescription_sp fdesc = makeFunctionDescription(symbol,nil<T_O>());
      auto entry = gctools::GC<VariadicType>::allocate(mp,fdesc,nil<T_O>());
      lisp_defineSingleDispatchMethod( clbind::DefaultWrapper(), symbol, this->_ClassSymbol, entry, 0, true, lambda_list, declares, docstring, autoExport, sizeof...(ARGS)+1);
    }
    return *this;
  }

  template <typename RT, class... ARGS>
  externalClass_ &def(const names_& names, RT (OT::ExternalType::*mp)(ARGS...) const,
                      const string &lambda_list = "", const string &declares = "", const string &docstring = "", bool autoExport = true) {
    for (auto& name : names._Names ) {
      maybe_register_symbol_using_dladdr(*(void**)&mp,sizeof(mp),name);
      Symbol_sp symbol = lispify_intern(name, symbol_packageName(this->_ClassSymbol));
      using VariadicType = TEMPLATED_FUNCTION_IndirectMethoid< RT (OT::ExternalType::*)(ARGS...) const, clbind::policies<>, OT, clbind::DefaultWrapper >;
      FunctionDescription_sp fdesc = makeFunctionDescription(symbol,nil<T_O>());
      auto entry = gctools::GC<VariadicType>::allocate(mp,fdesc,nil<T_O>());
      lisp_defineSingleDispatchMethod( clbind::DefaultWrapper(), symbol, this->_ClassSymbol, entry, 0, true, lambda_list, declares, docstring, autoExport, sizeof...(ARGS)+1);
    }
    return *this;
  }

  template <class C, class D>
  externalClass_ &def_readonly(string const &name, D C::*mem_ptr) {
    return *this;
  }
};
};

#endif // external_wrappers_h
