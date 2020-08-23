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
#include <clasp/clbind/clbind_tuple.h>
namespace clbind {

template <typename Pols, typename OT, typename MethodPtrType>
class IndirectVariadicMethoid;

#if 0

#include <clasp/clbind/clbind_methoids.h>

template <typename Pols, typename OT, typename RT, typename ARG1>
class IndirectVariadicMethoid
< Pols, OT, RT (OT::*)(ARG1)> : public core::BuiltinClosure_O {
public:
  typedef IndirectVariadicMethoid <Pols, OT, RT(OT::*)(ARG1)> MyType;
  typedef BuiltinClosure_O TemplatedBase;
public:
  virtual const char* describe() const { return "IndirectVariadicMethoid"; };
  typedef RT (OT::*MethodType)( ARG1 );
  MethodType mptr;
public:
  enum { NumParams = 1 /*sizeof...(ARGS)*/ };
  IndirectVariadicMethoid(const Pols& pols, core::FunctionDescription* fdesc, MethodType ptr) : core::BuiltinClosure_O(entry_point,fdesc), mptr(ptr) {};
  virtual size_t templatedSizeof() const { return sizeof(*this);};
  
  static inline LCC_RETURN LISP_CALLING_CONVENTION()
  {
    MyType* closure = gctools::untag_general<MyType*>((MyType*)lcc_closure);
    INCREMENT_FUNCTION_CALL_COUNTER(closure);
    COPY_VA_LIST();
    INVOCATION_HISTORY_FRAME();
    MAKE_STACK_FRAME(frame,closure->asSmartPtr().raw_(),5);
    MAKE_SPECIAL_BINDINGS_HOLDER(numSpecialBindings, specialBindingsVLA,
                                 lisp_lambda_list_handler_number_of_specials(closure->_lambdaListHandler));
    core::StackFrameDynamicScopeManager scope(numSpecialBindings,specialBindingsVLA,frame);
    lambdaListHandler_createBindings(closure->asSmartPtr(),closure->_lambdaListHandler,scope,LCC_PASS_ARGS_LLH);
    core::MultipleValues& returnValues = core::lisp_multipleValues();
    arg_tuple<Pols,OT,ARG1> all_args(frame->data());
    return apply_and_return<RT,Pols,MethodType,arg_tuple<Pols,OT,ARG1>>::go(returnValues,(closure->mptr),all_args);
  }
};
template <typename Pols, typename OT, typename RT, typename ARG1 >
class IndirectVariadicMethoid
< Pols,OT ,RT(OT::*)( ARG1) const >
: public core::BuiltinClosure_O {
public:
typedef IndirectVariadicMethoid < Pols,OT ,RT(OT::*)( ARG1) const > MyType;
typedef BuiltinClosure_O TemplatedBase;
public:
virtual const char* describe() const { return "IndirectVariadicMethoid"; };
typedef RT (OT::*MethodType)( ARG1) const;
MethodType mptr;
public:
enum { NumParams = 2 };
IndirectVariadicMethoid(const Pols& pols, core::FunctionDescription* fdesc, MethodType ptr) : core::BuiltinClosure_O(entry_point,fdesc), mptr(ptr) {};
virtual size_t templatedSizeof() const { return sizeof(*this);};

static inline LCC_RETURN LISP_CALLING_CONVENTION()
{
MyType* closure = gctools::untag_general<MyType*>((MyType*)lcc_closure);
INCREMENT_FUNCTION_CALL_COUNTER(closure);
COPY_VA_LIST();
INVOCATION_HISTORY_FRAME();
MAKE_STACK_FRAME(frame,closure->asSmartPtr().raw_(),2);
MAKE_SPECIAL_BINDINGS_HOLDER(numSpecialBindings, specialBindingsVLA,
    lisp_lambda_list_handler_number_of_specials(closure->_lambdaListHandler));
core::StackFrameDynamicScopeManager scope(numSpecialBindings,specialBindingsVLA,frame);
lambdaListHandler_createBindings(closure->asSmartPtr(),closure->_lambdaListHandler,scope,LCC_PASS_ARGS_LLH);
translate::from_object<OT*> objPtr(frame->arg(0));
translate::from_object<ARG1,typename DoesNotContain_<Pols,pureOutValue<1> >::type > a1(frame->arg(1));
((*objPtr._v).*(closure->mptr))(a1._v);
 core::MultipleValues& returnValues = core::lisp_multipleValues();
 arg_tuple<Pols,OT,ARG1> all_args(frame->data());
 return apply_and_return<RT,Pols,MethodType,arg_tuple<Pols,OT,ARG1>>::go(returnValues,(closure->mptr),all_args);
}
};
#else
template <typename Pols, typename OT, typename RT, typename... ARGS>
class IndirectVariadicMethoid
< Pols, OT, RT (OT::*)(ARGS...)> : public core::BuiltinClosure_O {
public:
  typedef IndirectVariadicMethoid <Pols, OT, RT(OT::*)(ARGS...)> MyType;
  typedef BuiltinClosure_O TemplatedBase;
public:
  virtual const char* describe() const { return "IndirectVariadicMethoid"; };
  typedef RT (OT::*MethodType)( ARGS... );
  MethodType mptr;
public:
  enum { NumParams = sizeof...(ARGS) };
  IndirectVariadicMethoid(const Pols& pols, core::FunctionDescription* fdesc, MethodType ptr) : core::BuiltinClosure_O(entry_point,fdesc), mptr(ptr) {};
  virtual size_t templatedSizeof() const { return sizeof(*this);};
  
  static inline LCC_RETURN LISP_CALLING_CONVENTION()
  {
    MyType* closure = gctools::untag_general<MyType*>((MyType*)lcc_closure);
    INCREMENT_FUNCTION_CALL_COUNTER(closure);
    COPY_VA_LIST();
    INVOCATION_HISTORY_FRAME();
    MAKE_STACK_FRAME(frame,closure->asSmartPtr().raw_(),5);
    MAKE_SPECIAL_BINDINGS_HOLDER(numSpecialBindings, specialBindingsVLA,
                                 lisp_lambda_list_handler_number_of_specials(closure->_lambdaListHandler));
    core::StackFrameDynamicScopeManager scope(numSpecialBindings,specialBindingsVLA,frame);
    lambdaListHandler_createBindings(closure->asSmartPtr(),closure->_lambdaListHandler,scope,LCC_PASS_ARGS_LLH);
    core::MultipleValues& returnValues = core::lisp_multipleValues();
    arg_tuple<Pols,OT,ARGS...> all_args(frame->data());
    return apply_and_return<RT,Pols,MethodType,arg_tuple<Pols,OT,ARGS...>>::go(returnValues,(closure->mptr),all_args);
  }
};

template <typename Pols, typename OT, typename RT, typename... ARGS>
class IndirectVariadicMethoid
< Pols, OT, RT (OT::*)(ARGS...) const> : public core::BuiltinClosure_O {
public:
  typedef IndirectVariadicMethoid <Pols, OT, RT(OT::*)(ARGS...) const> MyType;
  typedef BuiltinClosure_O TemplatedBase;
public:
  virtual const char* describe() const { return "IndirectVariadicMethoid"; };
  typedef RT (OT::*MethodType)( ARGS... ) const;
  MethodType mptr;
public:
  enum { NumParams = sizeof...(ARGS) };
  IndirectVariadicMethoid(const Pols& pols, core::FunctionDescription* fdesc, MethodType ptr) : core::BuiltinClosure_O(entry_point,fdesc), mptr(ptr) {};
  virtual size_t templatedSizeof() const { return sizeof(*this);};
  
  static inline LCC_RETURN LISP_CALLING_CONVENTION()
  {
    MyType* closure = gctools::untag_general<MyType*>((MyType*)lcc_closure);
    INCREMENT_FUNCTION_CALL_COUNTER(closure);
    COPY_VA_LIST();
    INVOCATION_HISTORY_FRAME();
    MAKE_STACK_FRAME(frame,closure->asSmartPtr().raw_(),5);
    MAKE_SPECIAL_BINDINGS_HOLDER(numSpecialBindings, specialBindingsVLA,
                                 lisp_lambda_list_handler_number_of_specials(closure->_lambdaListHandler));
    core::StackFrameDynamicScopeManager scope(numSpecialBindings,specialBindingsVLA,frame);
    lambdaListHandler_createBindings(closure->asSmartPtr(),closure->_lambdaListHandler,scope,LCC_PASS_ARGS_LLH);
    core::MultipleValues& returnValues = core::lisp_multipleValues();
    arg_tuple<Pols,OT,ARGS...> all_args(frame->data());
    return apply_and_return<RT,Pols,MethodType,arg_tuple<Pols,OT,ARGS...>>::go(returnValues,(closure->mptr),all_args);
  }
};

// User template deduction guide
template <typename Pols, typename OT, typename RT, typename... ARGS>
IndirectVariadicMethoid(Pols& pols, core::FunctionDescription* fdesc, RT(OT::*)(ARGS...)) -> IndirectVariadicMethoid< Pols, OT, RT (OT::*)(ARGS...)>;

template <typename Pols, typename OT, typename RT, typename... ARGS>
IndirectVariadicMethoid(Pols& pols, core::FunctionDescription* fdesc, RT(OT::*)(ARGS...) const) -> IndirectVariadicMethoid< Pols, OT, RT (OT::*)(ARGS...) const>;

#endif

}

//#include <clasp/clbind/clbind_static_members.h>


template <typename Pols, typename OT, typename MethodPtrType>
class gctools::GCStamp<clbind::IndirectVariadicMethoid<Pols, OT, MethodPtrType>> {
public:
  static gctools::GCStampEnum const Stamp = gctools::GCStamp<typename clbind::IndirectVariadicMethoid<Pols, OT, MethodPtrType>::TemplatedBase>::Stamp;
};

#endif
