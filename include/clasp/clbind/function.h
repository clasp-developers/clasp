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
//#include <clasp/clbind/scope.h>
#include <clasp/core/arguments.h>

namespace clbind {

template <typename FunctionPtrType, typename Policies, typename PureOuts>
class TEMPLATED_FUNCTION_VariadicFunctor : public core::BuiltinClosure_O {
public:
  typedef core::BuiltinClosure_O TemplatedBase;
  virtual size_t templatedSizeof() const { return sizeof(*this); };
};

};

#include <clasp/clbind/apply.h>


namespace clbind {


template <typename... Types>
class my_tuple : public std::tuple<Types...> {
  my_tuple() {
    printf("%s:%d:%s\n", __FILE__, __LINE__, __FUNCTION__ );
  }
};

template <typename Pols, typename RT, typename...ARGS, typename...PUREOUTS>
class TEMPLATED_FUNCTION_VariadicFunctor< RT(*)(ARGS...), Pols, clbind::pureOutsPack<PUREOUTS...>> : public core::BuiltinClosure_O {
public:
  typedef TEMPLATED_FUNCTION_VariadicFunctor < RT(*)(ARGS...), Pols, clbind::pureOutsPack<PUREOUTS...>> MyType;
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
  static inline LCC_RETURN LISP_CALLING_CONVENTION()
  {
    MyType* closure = gctools::untag_general<MyType*>((MyType*)lcc_closure);
    INCREMENT_FUNCTION_CALL_COUNTER(closure);
    INITIALIZE_VA_LIST();
    MAKE_STACK_FRAME(frame,closure->asSmartPtr().raw_(),sizeof...(ARGS));
    MAKE_SPECIAL_BINDINGS_HOLDER(numSpecialBindings, specialBindingsVLA,
                                 lisp_lambda_list_handler_number_of_specials(closure->_lambdaListHandler));
    core::StackFrameDynamicScopeManager scope(numSpecialBindings,specialBindingsVLA,frame);
    lambdaListHandler_createBindings(closure->asSmartPtr(),closure->_lambdaListHandler,scope,LCC_PASS_ARGS_LLH);
    core::MultipleValues& returnValues = core::lisp_multipleValues();
#ifdef DEBUG_EVALUATE
    if (core::_sym_STARdebugEvalSTAR && core::_sym_STARdebugEvalSTAR->symbolValue().notnilp()) {
      for (size_t ia=0; ia<sizeof...(ARGS); ++ia) {
        core::T_sp obj = ((frame->arg(ia)));
        printf("  apply  arg[%lu] -> %s\n", ia, _rep_(obj).c_str());
      }
    }
#endif
    std::tuple<translate::from_object<ARGS,PUREOUTS>...> all_args(arg_tuple<0,Pols,ARGS...>::go(frame->arguments()));
    return apply_and_return<RT,Pols,decltype(closure->fptr),decltype(all_args)>::go(returnValues,std::move(closure->fptr),std::move(all_args));
  }
};


  template <typename RT  ,typename...ARGS>
  class TEMPLATED_FUNCTION_VariadicFunctor< RT(*)(ARGS...), core::policy::clasp,clbind::pureOutsPack<>> : public core::BuiltinClosure_O {
public:
    typedef TEMPLATED_FUNCTION_VariadicFunctor < RT(*)(ARGS...), core::policy::clasp,clbind::pureOutsPack<>> MyType;
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
//    printf("%s:%d Entered entry_point of a VariadicFunctor\n", __FILE__, __LINE__ );
    MyType* closure = gctools::untag_general<MyType*>((MyType*)lcc_closure);
    INCREMENT_FUNCTION_CALL_COUNTER(closure);
    INITIALIZE_VA_LIST();
    MAKE_STACK_FRAME(frame,closure->asSmartPtr().raw_(),sizeof...(ARGS));
    MAKE_SPECIAL_BINDINGS_HOLDER(numSpecialBindings, specialBindingsVLA,
                                 lisp_lambda_list_handler_number_of_specials(closure->_lambdaListHandler));
    core::StackFrameDynamicScopeManager scope(numSpecialBindings,specialBindingsVLA,frame);
//    printf("%s:%d About to create bindings for closure->_lambdaListHandler->%s\n", __FILE__, __LINE__, _rep_(closure->_lambdaListHandler).c_str());
    lambdaListHandler_createBindings(closure->asSmartPtr(),closure->_lambdaListHandler,scope,LCC_PASS_ARGS_LLH);
#ifdef DEBUG_EVALUATE
    if (core::_sym_STARdebugEvalSTAR && core::_sym_STARdebugEvalSTAR->symbolValue().notnilp()) {
      for (size_t ia=0; ia<sizeof...(ARGS); ++ia) {
        core::T_sp obj = ((frame->arg(ia)));
        printf("  apply  arg[%lu] -> %s\n", ia, _rep_(obj).c_str());
      }
    }
#endif
    core::MultipleValues& returnValues = core::lisp_multipleValues();
    std::tuple<translate::from_object<ARGS>...> all_args(arg_tuple<0,policies<>,ARGS...>::go(frame->arguments()));
    return clasp_apply_and_return<RT,core::policy::clasp,decltype(closure->fptr),decltype(all_args)>::go(returnValues,std::move(closure->fptr),std::move(all_args));
  }
};
};

namespace clbind {
//#include <clasp/clbind/clbind_functoids.h>
};

template <typename FunctionPtrType, typename Policies, typename PureOutsPack>
class gctools::GCStamp<clbind::TEMPLATED_FUNCTION_VariadicFunctor<FunctionPtrType, Policies, PureOutsPack>> {
public:
  static gctools::GCStampEnum const StampWtag = gctools::GCStamp<typename clbind::TEMPLATED_FUNCTION_VariadicFunctor<FunctionPtrType, Policies, PureOutsPack>::TemplatedBase>::Stamp;
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
struct function_registration : registration {
  function_registration(const std::string& name, FunctionPointerType f, Policies const &policies) // , string const &lambdalist, string const &declares, string const &docstring)
    : m_name(name), functionPtr(f), m_policies(policies) {
    this->m_lambdalist = policies.lambdaList();
    this->m_docstring = policies.docstring();
    this->m_declares = policies.declares();
  }

  void register_() const {
    LOG_SCOPE(("%s:%d register_ %s/%s\n", __FILE__, __LINE__, this->kind().c_str(), this->name().c_str()));
    core::Symbol_sp symbol = core::lisp_intern(m_name, core::lisp_currentPackageName());
    using inValuePack = clbind::inValueTrueFalseMaskPack<FunctionArgCount<FunctionPointerType>::value,Policies>;
    using VariadicType = TEMPLATED_FUNCTION_VariadicFunctor<FunctionPointerType, Policies, typename inValuePack::type >;
    core::GlobalEntryPoint_sp entryPoint = makeGlobalEntryPointAndFunctionDescription<VariadicType>(symbol);
    core::BuiltinClosure_sp functoid = gc::As<core::BuiltinClosure_sp>(gc::GC<VariadicType>::allocate(entryPoint,functionPtr));
    core::lisp_defun(symbol, core::lisp_currentPackageName(), functoid, m_lambdalist, m_declares, m_docstring, "=external=", 0, (CountFunctionArguments<FunctionPointerType>::value), GatherPureOutValues<Policies, -1>::gather());
    core::validateFunctionDescription(__FILE__,__LINE__,functoid);
  }

  virtual std::string name() const { return this->m_name;}
  virtual std::string kind() const { return "function_registration"; };
  
  std::string m_name;
  FunctionPointerType functionPtr;
  Policies m_policies;
  string m_lambdalist;
  string m_declares;
  string m_docstring;
};

} // namespace detail

} // namespace clbind

#endif // CLBIND_FUNCTION2_081014_HPP
