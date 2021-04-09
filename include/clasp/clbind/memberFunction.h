/*
    File: memberFunction.h
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
#ifndef clbind_memberFunction_H
#define clbind_memberFunction_H

#include <clasp/clbind/clbind_wrappers.h>
#include <clasp/clbind/policies.h>
#include <clasp/clbind/details.h>
namespace clbind {

template <typename Policies, typename OT, typename MethodPtrType>
class TEMPLATED_FUNCTION_IndirectVariadicMethoid : public core::BuiltinClosure_O {
  typedef BuiltinClosure_O TemplatedBase;
  virtual size_t templatedSizeof() const { return sizeof(*this); };
  virtual const char *describe() const { return "IndirectVariadicMethoid"; };
};
};

namespace clbind {

template <typename Policies, typename RT, typename OT, typename... ARGS>
class TEMPLATED_FUNCTION_IndirectVariadicMethoid <Policies, OT, RT(OT::*)(ARGS...)> : public core::BuiltinClosure_O {
public:
  typedef TEMPLATED_FUNCTION_IndirectVariadicMethoid<Policies,OT,RT(OT::*)(ARGS...) > MyType;
  typedef core::BuiltinClosure_O TemplatedBase;
public:
  virtual const char* describe() const {return "IndirectVariadicMethoid";};
  typedef RT(OT::*MethodType)(ARGS...) ;
  MethodType mptr;
public:
  enum { NumParams = sizeof...(ARGS)+1 };
  TEMPLATED_FUNCTION_IndirectVariadicMethoid(core::GlobalEntryPoint_sp ep, MethodType ptr) : core::BuiltinClosure_O(ENSURE_ENTRY_POINT(ep,&MyType::method_entry_point)), mptr(ptr) {
    this->validateCodePointer((void**)&this->mptr,sizeof(this->mptr));
  };
  virtual size_t templatedSizeof() const { return sizeof(*this);};
  virtual void fixupInternalsForImageSaveLoad( imageSaveLoad::Fixup* fixup ) {
    this->fixupOneCodePointer( fixup,(void**)&this->mptr,sizeof(this->mptr));
  }
  static inline gctools::return_type method_entry_point(LCC_ARGS_ELLIPSIS)
  {
    MyType* closure = gctools::untag_general<MyType*>((MyType*)lcc_closure);
    INCREMENT_FUNCTION_CALL_COUNTER(closure);
    COPY_VA_LIST();
    INVOCATION_HISTORY_FRAME();
    MAKE_STACK_FRAME(frame,closure->asSmartPtr().raw_(),sizeof...(ARGS)+1);
    MAKE_SPECIAL_BINDINGS_HOLDER(numSpecialBindings, specialBindingsVLA,
                                 lisp_lambda_list_handler_number_of_specials(closure->_lambdaListHandler));
    core::StackFrameDynamicScopeManager scope(numSpecialBindings,specialBindingsVLA,frame);
    lambdaListHandler_createBindings(closure->asSmartPtr(),closure->_lambdaListHandler,scope,LCC_PASS_ARGS_LLH);
    core::MultipleValues& returnValues = core::lisp_multipleValues();
    translate::from_object<OT*> otep(frame->arg(0));
    std::tuple<translate::from_object<ARGS>...> all_args = clbind::arg_tuple<1,Policies,ARGS...>::go(frame->arguments(0));
    return clbind::clbind_external_method_apply_and_return<Policies,RT,decltype(closure->mptr),OT*,decltype(all_args)>::go(returnValues,std::move(closure->mptr),otep._v,std::move(all_args));
  }
};

template <typename Policies, typename RT, typename OT, typename... ARGS>
class TEMPLATED_FUNCTION_IndirectVariadicMethoid <Policies, OT, RT(OT::*)(ARGS...) const> : public core::BuiltinClosure_O {
public:
  typedef TEMPLATED_FUNCTION_IndirectVariadicMethoid<Policies,OT,RT(OT::*)(ARGS...) const > MyType;
  typedef core::BuiltinClosure_O TemplatedBase;
public:
  virtual const char* describe() const {return "IndirectVariadicMethoid";};
  typedef RT(OT::*MethodType)(ARGS...) const ;
  MethodType mptr;
public:
  enum { NumParams = sizeof...(ARGS)+1 };
  TEMPLATED_FUNCTION_IndirectVariadicMethoid(core::GlobalEntryPoint_sp ep, MethodType ptr) : core::BuiltinClosure_O(ENSURE_ENTRY_POINT(ep,&MyType::method_entry_point)), mptr(ptr) {
    this->validateCodePointer((void**)&this->mptr,sizeof(this->mptr));
  };
  virtual size_t templatedSizeof() const { return sizeof(*this);};
  virtual void fixupInternalsForImageSaveLoad( imageSaveLoad::Fixup* fixup ) {
    this->fixupOneCodePointer( fixup,(void**)&this->mptr,sizeof(this->mptr));
  }
  static inline gctools::return_type method_entry_point(LCC_ARGS_ELLIPSIS)
  {
    MyType* closure = gctools::untag_general<MyType*>((MyType*)lcc_closure);
    INCREMENT_FUNCTION_CALL_COUNTER(closure);
    COPY_VA_LIST();
    INVOCATION_HISTORY_FRAME();
    MAKE_STACK_FRAME(frame,closure->asSmartPtr().raw_(),sizeof...(ARGS)+1);
    MAKE_SPECIAL_BINDINGS_HOLDER(numSpecialBindings, specialBindingsVLA,
                                 lisp_lambda_list_handler_number_of_specials(closure->_lambdaListHandler));
    core::StackFrameDynamicScopeManager scope(numSpecialBindings,specialBindingsVLA,frame);
    lambdaListHandler_createBindings(closure->asSmartPtr(),closure->_lambdaListHandler,scope,LCC_PASS_ARGS_LLH);
    core::MultipleValues& returnValues = core::lisp_multipleValues();
    translate::from_object<OT*> otep(frame->arg(0));
    std::tuple<translate::from_object<ARGS>...> all_args = clbind::arg_tuple<1,Policies,ARGS...>::go(frame->arguments(0));
    return clbind::clbind_external_method_apply_and_return<Policies,RT,decltype(closure->mptr),OT*,decltype(all_args)>::go(returnValues,std::move(closure->mptr),otep._v,std::move(all_args));
  }
};

};


template <typename Pols, typename OT, typename MethodPtrType>
class gctools::GCStamp<clbind::TEMPLATED_FUNCTION_IndirectVariadicMethoid<Pols, OT, MethodPtrType>> {
public:
  static gctools::GCStampEnum const StampWtag = gctools::GCStamp<typename clbind::TEMPLATED_FUNCTION_IndirectVariadicMethoid<Pols, OT, MethodPtrType>::TemplatedBase>::Stamp;
};

#endif
