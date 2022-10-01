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

#ifndef CLBIND_FUNCTION2_081014_HPP
#define CLBIND_FUNCTION2_081014_HPP

#if 0
# define DEBUG_SCOPE 1
# define LOG_SCOPE(xxx) printf xxx;
#else
# define LOG_SCOPE(xxx)
#endif


#include <clasp/core/lambdaListHandler.fwd.h>
//#include "clbind/prefix.h"
#include <clasp/clbind/config.h>
#include <clasp/clbind/cl_include.h>
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
  virtual std::string kind()const  = 0;
protected:
  virtual void register_() const = 0;

private:
  friend struct ::clbind::scope_;
  registration *m_next;
};
}
} // namespace clbind::detail


#include <clasp/clbind/clbindPackage.h>
#include <clasp/clbind/policies.h>
#include <clasp/clbind/details.h>
#include <clasp/core/arguments.h>
#include <clasp/clbind/apply.h>

namespace clbind {
template <typename... Types>
class my_tuple : public std::tuple<Types...> {
  my_tuple() {
    printf("%s:%d:%s\n", __FILE__, __LINE__, __FUNCTION__ );
  }
};
};

namespace clbind {
template <typename FunctionPtrType, typename Policies, typename PureOuts, typename ArgumentHandling>
class TEMPLATED_FUNCTION_VariadicFunctor;
};

namespace clbind {
template <typename RT, typename...ARGS, typename Policies, typename...PUREOUTS>
class TEMPLATED_FUNCTION_VariadicFunctor< RT(*)(ARGS...), Policies, clbind::pureOutsPack<PUREOUTS...>, LambdaListHandlerWrapper > : public core::GlobalEntryPointBase_O {
public:
  typedef TEMPLATED_FUNCTION_VariadicFunctor < RT(*)(ARGS...), Policies, clbind::pureOutsPack<PUREOUTS...>, LambdaListHandlerWrapper > MyType;
  typedef core::GlobalEntryPointBase_O TemplatedBase;
  typedef RT(*FuncType)(ARGS...);
public:
  FuncType                   fptr;
  core::LambdaListHandler_sp _lambdaListHandler;
public:
  static constexpr auto inValueMask = clbind::inValueMaskMuple<sizeof...(ARGS), Policies>();

  enum { NumParams = sizeof...(ARGS)};

  TEMPLATED_FUNCTION_VariadicFunctor(FuncType ptr, core::FunctionDescription_sp fdesc, core::T_sp code, core::LambdaListHandler_sp llh)
      : fptr(ptr), GlobalEntryPointBase_O(fdesc,core::ClaspXepFunction::make<MyType>(),code), _lambdaListHandler(llh)  {
    this->validateCodePointer((void**)&this->fptr,sizeof(this->fptr));
  };

  virtual const char* describe() const { return "VariadicFunctor"; };

  virtual size_t templatedSizeof() const { return sizeof(*this);};

  virtual void setLambdaListHandler(core::LambdaListHandler_sp llh) {this->_lambdaListHandler = llh; };

  void fixupInternalsForSnapshotSaveLoad( snapshotSaveLoad::Fixup* fixup ) {
    this->TemplatedBase::fixupInternalsForSnapshotSaveLoad(fixup);
    this->fixupOneCodePointer( fixup, (void**)&this->fptr );
  };

  static inline LCC_RETURN entry_point_n(core::T_O* lcc_closure, size_t lcc_nargs, core::T_O** lcc_args )
  {
    MyType* closure = gctools::untag_general<MyType*>((MyType*)lcc_closure);
    INCREMENT_FUNCTION_CALL_COUNTER(closure);
    DO_DRAG_CXX_CALLS();
    MAKE_STACK_FRAME(frame,sizeof...(ARGS));
    MAKE_SPECIAL_BINDINGS_HOLDER(numSpecialBindings, specialBindingsVLA,
                                 lisp_lambdaListHandlerNumberOfSpecialVariables(closure->_lambdaListHandler));
    core::StackFrameDynamicScopeManager scope(closure->_lambdaListHandler, numSpecialBindings,specialBindingsVLA,frame,sizeof...(ARGS));
    lambdaListHandler_createBindings(closure->asSmartPtr(),closure->_lambdaListHandler,&scope, lcc_nargs, lcc_args  );
    std::tuple<translate::from_object<ARGS,PUREOUTS>...> all_args(arg_tuple<0,Policies,ARGS...>::goFrame(frame->arguments()));
    return apply_and_return<Policies,RT,decltype(closure->fptr),decltype(all_args)>::go(std::move(closure->fptr),std::move(all_args));
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
}; // namespace clbind


//
//
// External BytecodeWrapper wrapper, no LambdaListHandler
//
//
namespace clbind {
template <typename RT, typename...ARGS, typename Policies, typename...PUREOUTS>
class TEMPLATED_FUNCTION_VariadicFunctor< RT(*)(ARGS...), Policies, clbind::pureOutsPack<PUREOUTS...>, BytecodeWrapper > : public core::GlobalEntryPointBase_O {
public:
  typedef TEMPLATED_FUNCTION_VariadicFunctor < RT(*)(ARGS...), Policies, clbind::pureOutsPack<PUREOUTS...>, BytecodeWrapper > MyType;
  typedef core::GlobalEntryPointBase_O TemplatedBase;
  typedef RT(*FuncType)(ARGS...);
public:
  FuncType                   fptr;
public:
  static constexpr auto inValueMask = clbind::inValueMaskMuple<sizeof...(ARGS), Policies>();

  enum { NumParams = sizeof...(ARGS)};

  TEMPLATED_FUNCTION_VariadicFunctor(FuncType ptr, core::FunctionDescription_sp fdesc, core::T_sp code)
      : fptr(ptr), GlobalEntryPointBase_O(fdesc,core::ClaspXepFunction::make<MyType>(),code)  {
    this->validateCodePointer((void**)&this->fptr,sizeof(this->fptr));
  };

  virtual const char* describe() const { return "VariadicFunctor"; };

  virtual size_t templatedSizeof() const { return sizeof(*this);};

  virtual void setLambdaListHandler(core::LambdaListHandler_sp llh) {
    printf("%s:%d:%s BZZZZZZT We don't use LambdaListHandler_O anymore - don't call this.\n", __FILE__, __LINE__, __FUNCTION__ );
    abort();
  }

// Fixup the pointers through snapshot save-load
  void fixupInternalsForSnapshotSaveLoad( snapshotSaveLoad::Fixup* fixup ) {
    this->TemplatedBase::fixupInternalsForSnapshotSaveLoad(fixup);
    this->fixupOneCodePointer( fixup, (void**)&this->fptr );
  };

  static inline LCC_RETURN entry_point_n(core::T_O* lcc_closure, size_t lcc_nargs, core::T_O** lcc_args )
  {
    MyType* closure = gctools::untag_general<MyType*>((MyType*)lcc_closure);
    INCREMENT_FUNCTION_CALL_COUNTER(closure);
    DO_DRAG_CXX_CALLS();
    std::tuple<translate::from_object<ARGS,PUREOUTS>...> all_args(arg_tuple<0,Policies,ARGS...>::goFrame(lcc_args));
    return apply_and_return<Policies,RT,decltype(closure->fptr),decltype(all_args)>::go(std::move(closure->fptr),std::move(all_args));
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
}; // namespace clbind

#if 0
namespace clbind {
template <typename Pols, typename RT, typename...ARGS, typename...PUREOUTS>
class TEMPLATED_FUNCTION_VariadicFunctor< RT(*)(ARGS...), Pols, clbind::pureOutsPack<PUREOUTS...>, BytecodeWrapper> : public core::BuiltinClosure_O {
public:
  typedef TEMPLATED_FUNCTION_VariadicFunctor < RT(*)(ARGS...), Pols, clbind::pureOutsPack<PUREOUTS...>, BytecodeWrapper > MyType;
  typedef core::BuiltinClosure_O TemplatedBase;
public:
  typedef RT(*FuncType)(ARGS...);
  FuncType fptr;
public:
  static constexpr auto inValueMask = clbind::inValueMaskMuple<sizeof...(ARGS),Pols>();
  virtual const char* describe() const { return "VariadicFunctor"; };
  enum { NumParams = sizeof...(ARGS)};
  TEMPLATED_FUNCTION_VariadicFunctor(core::GlobalEntryPoint_sp ep, FuncType ptr)
    : core::BuiltinClosure_O(ep)
    , fptr(ptr) {
    this->validateCodePointer((void**)&this->fptr,sizeof(this->fptr));
  };
  virtual size_t templatedSizeof() const { return sizeof(*this);};
  virtual void fixupInternalsForSnapshotSaveLoad(snapshotSaveLoad::Fixup* fixup) {
    this->fixupOneCodePointer(fixup,(void**)&this->fptr,sizeof(this->fptr));
  }
  static inline gctools::return_type entry_point_n(core::T_O* lcc_closure, size_t lcc_nargs, core::T_O** lcc_args )
  {
    MyType* closure = gctools::untag_general<MyType*>((MyType*)lcc_closure);
    DO_DRAG_CXX_CALLS();
    INCREMENT_FUNCTION_CALL_COUNTER(closure);
    std::tuple<translate::from_object<ARGS,PUREOUTS>...> all_args(arg_tuple<0,Pols,ARGS...>::goFrame(lcc_args));
    return apply_and_return<Pols,RT,decltype(closure->fptr),decltype(all_args)>::go(std::move(closure->fptr),std::move(all_args));
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
#endif

//
//
// BytecodeWrapper wrapper, no LambdaListHandler
//
//
#if 0
namespace clbind {
template <typename RT  ,typename...ARGS>
class TEMPLATED_FUNCTION_VariadicFunctor< RT(*)(ARGS...), core::policy::clasp_policy,clbind::pureOutsPack<>,BytecodeWrapper> : public core::BuiltinClosure_O {
public:
  typedef TEMPLATED_FUNCTION_VariadicFunctor < RT(*)(ARGS...), core::policy::clasp_policy,clbind::pureOutsPack<>,BytecodeWrapper> MyType;
  typedef core::BuiltinClosure_O TemplatedBase;
public:
  typedef RT(*FuncType)(ARGS...);
  FuncType fptr;
public:
  virtual const char* describe() const { return "VariadicFunctor"; };
  enum { NumParams = sizeof...(ARGS)};
  TEMPLATED_FUNCTION_VariadicFunctor(core::GlobalEntryPoint_sp ep, FuncType ptr) : core::BuiltinClosure_O(ep), fptr(ptr) {
    this->validateCodePointer((void**)&this->fptr,sizeof(this->fptr));
  };
  virtual size_t templatedSizeof() const { return sizeof(*this);};
  virtual void fixupInternalsForSnapshotSaveLoad(snapshotSaveLoad::Fixup* fixup ) {
    this->fixupOneCodePointer( fixup, (void**)&this->fptr, sizeof(this->fptr) );
  }
  static inline LCC_RETURN LISP_CALLING_CONVENTION()
  {
    MyType* closure = gctools::untag_general<MyType*>((MyType*)lcc_closure);
    DO_DRAG_CXX_CALLS();
    INCREMENT_FUNCTION_CALL_COUNTER(closure);
    std::tuple<translate::from_object<ARGS>...> all_args(arg_tuple<0,policies<>,ARGS...>::goFrame(lcc_args));
    return apply_and_return<core::policy::clasp_policy,RT,decltype(closure->fptr),decltype(all_args)>::go(std::move(closure->fptr),std::move(all_args));
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
#endif

namespace clbind {
//#include <clasp/clbind/clbind_functoids.h>
};

template <typename FunctionPtrType, typename Policies, typename PureOutsPack, typename ArgumentHandler>
class gctools::GCStamp<clbind::TEMPLATED_FUNCTION_VariadicFunctor<FunctionPtrType, Policies, PureOutsPack, ArgumentHandler> > {
public:
  static gctools::GCStampEnum const StampWtag = gctools::GCStamp<typename clbind::TEMPLATED_FUNCTION_VariadicFunctor<FunctionPtrType, Policies, PureOutsPack, ArgumentHandler >::TemplatedBase>::StampWtag;
};

namespace clbind {

namespace detail {

template <typename FunctionPointerType>
struct CountFunctionArguments {
  enum { value = 0 };
};

template <typename RT, typename... ARGS>
struct CountFunctionArguments<RT (*)(ARGS...)> {
  enum { value = sizeof...(ARGS) };
};

template <class FunctionPointerType, class Policies=policies<>, class PureOutsPack=clbind::pureOutsPack<>>
struct function_registration;

template <class FunctionPointerType, class...Policies, class PureOutsPack>
struct function_registration<FunctionPointerType,policies<Policies...>,PureOutsPack> : registration {
  function_registration(const std::string& name, FunctionPointerType f, policies<Policies...> const &policies) // , string const &lambdalist, string const &declares, string const &docstring)
    : m_name(name), functionPtr(f), m_policies(policies) {
    this->m_lambdalist = policies.lambdaList();
    this->m_docstring = policies.docstring();
    this->m_declares = policies.declares();
  }

  void register_() const {
    LOG_SCOPE(("%s:%d register_ %s/%s\n", __FILE__, __LINE__, this->kind().c_str(), this->name().c_str()));
    core::Symbol_sp symbol = core::lisp_intern(m_name, core::lisp_currentPackageName());
    using inValuePack = clbind::inValueTrueFalseMaskPack<FunctionArgCount<FunctionPointerType>::value,policies<Policies...>>;
    using VariadicType = TEMPLATED_FUNCTION_VariadicFunctor<FunctionPointerType, policies<Policies...>, typename inValuePack::type, clbind::DefaultWrapper >;
    core::FunctionDescription_sp fdesc = makeFunctionDescription(symbol,nil<core::T_O>());
    auto entry = gctools::GC<VariadicType>::allocate(this->functionPtr,fdesc,nil<core::T_O>(),unbound<core::LambdaListHandler_O>());
    core::lisp_bytecode_defun( core::symbol_function, clbind::DefaultWrapper::BytecodeP, symbol, core::lisp_currentPackageName(), entry, m_lambdalist, m_declares, m_docstring, "=external=", 0, (CountFunctionArguments<FunctionPointerType>::value), GatherPureOutValues<policies<Policies...>, -1>::gather());
  }

  virtual std::string name() const { return this->m_name;}
  virtual std::string kind() const { return "function_registration"; };
  
  std::string m_name;
  FunctionPointerType functionPtr;
  policies<Policies...> m_policies;
  string m_lambdalist;
  string m_declares;
  string m_docstring;
};

} // namespace detail

} // namespace clbind

#endif // CLBIND_FUNCTION2_081014_HPP
