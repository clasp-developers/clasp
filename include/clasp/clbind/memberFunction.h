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
  TEMPLATED_FUNCTION_IndirectVariadicMethoid(core::GlobalEntryPoint_sp ep, MethodType ptr) : core::BuiltinClosure_O(ep), mptr(ptr) {
    this->validateCodePointer((void**)&this->mptr,sizeof(this->mptr));
  };
  virtual size_t templatedSizeof() const { return sizeof(*this);};
  virtual void fixupInternalsForSnapshotSaveLoad( snapshotSaveLoad::Fixup* fixup ) {
    this->fixupOneCodePointer( fixup,(void**)&this->mptr,sizeof(this->mptr));
  }
  static inline LCC_RETURN LISP_CALLING_CONVENTION()
  {
    MyType* closure = gctools::untag_general<MyType*>((MyType*)lcc_closure);
    INCREMENT_FUNCTION_CALL_COUNTER(closure);
    MAKE_STACK_FRAME(frame,sizeof...(ARGS)+1);
    MAKE_SPECIAL_BINDINGS_HOLDER(numSpecialBindings, specialBindingsVLA,
                                 lisp_lambdaListHandlerNumberOfSpecialVariables(closure->_lambdaListHandler));
    core::StackFrameDynamicScopeManager scope(numSpecialBindings,specialBindingsVLA,frame,sizeof...(ARGS)+1);
    lambdaListHandler_createBindings(closure->asSmartPtr(),closure->_lambdaListHandler,&scope,lcc_nargs, lcc_args );
    core::MultipleValues& returnValues = core::lisp_multipleValues();
    translate::from_object<OT*> otep(frame->arg(0));
    std::tuple<translate::from_object<ARGS>...> all_args = clbind::arg_tuple<1,Policies,ARGS...>::goFrame(frame->arguments(0));
    return clbind::clbind_external_method_apply_and_return<Policies,RT,decltype(closure->mptr),OT*,decltype(all_args)>::go(returnValues,std::move(closure->mptr),otep._v,std::move(all_args));
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
  TEMPLATED_FUNCTION_IndirectVariadicMethoid(core::GlobalEntryPoint_sp ep, MethodType ptr) : core::BuiltinClosure_O(ep), mptr(ptr) {
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
    MAKE_STACK_FRAME(frame,sizeof...(ARGS)+1);
    MAKE_SPECIAL_BINDINGS_HOLDER(numSpecialBindings, specialBindingsVLA,
                                 lisp_lambdaListHandlerNumberOfSpecialVariables(closure->_lambdaListHandler));
    core::StackFrameDynamicScopeManager scope(numSpecialBindings,specialBindingsVLA,frame,sizeof...(ARGS)+1);
    lambdaListHandler_createBindings(closure->asSmartPtr(),closure->_lambdaListHandler,&scope,lcc_nargs, lcc_args );
    core::MultipleValues& returnValues = core::lisp_multipleValues();
    translate::from_object<OT*> otep(frame->arg(0));
    std::tuple<translate::from_object<ARGS>...> all_args = clbind::arg_tuple<1,Policies,ARGS...>::goFrame(frame->arguments(0));
    return clbind::clbind_external_method_apply_and_return<Policies,RT,decltype(closure->mptr),OT*,decltype(all_args)>::go(returnValues,std::move(closure->mptr),otep._v,std::move(all_args));
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


template <typename Pols, typename OT, typename MethodPtrType>
class gctools::GCStamp<clbind::TEMPLATED_FUNCTION_IndirectVariadicMethoid<Pols, OT, MethodPtrType>> {
public:
  static gctools::GCStampEnum const StampWtag = gctools::GCStamp<typename clbind::TEMPLATED_FUNCTION_IndirectVariadicMethoid<Pols, OT, MethodPtrType>::TemplatedBase>::Stamp;
};

#endif
