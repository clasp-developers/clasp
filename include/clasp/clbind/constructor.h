/*
    File: constructor.h
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
#ifndef clbind_constructor_H
#define clbind_constructor_H

#include <clasp/clbind/adapter.fwd.h>
#include <clasp/clbind/clbind_wrappers.h>
#include <clasp/clbind/policies.h>
#include <clasp/clbind/adapter.h>
#include <clasp/clbind/details.h>

namespace clbind {

template <typename... SIGS>
struct constructor {};

typedef enum { create_default_constructor } default_constructor_type;

typedef constructor<> default_constructor;

class ConstructorCreator_O : public core::Creator_O {
  LISP_ABSTRACT_CLASS(clbind,ClbindPkg,ConstructorCreator_O,"ConstructorCreator",core::Creator_O);
public:
  ConstructorCreator_O(core::GlobalEntryPoint_sp ep, core::Symbol_sp c) : Creator_O(ep), _mostDerivedClassSymbol(c){};
  core::Symbol_sp _mostDerivedClassSymbol;
  virtual ~ConstructorCreator_O() {};
};
};

namespace clbind {

template <typename T, typename Pointer>
class DefaultConstructorCreator_O : public ConstructorCreator_O {
public:
  typedef ConstructorCreator_O TemplatedBase;
public:
  typedef Wrapper<T, Pointer> WrapperType;
  gctools::Header_s::StampWtagMtag _HeaderValue;
  int _duplicationLevel;
public:
  virtual size_t templatedSizeof() const { return sizeof(*this); };
public:
#if 0
  DefaultConstructorCreator_O() : ConstructorCreator_O(core::makeGlobalEntryPointAndFunctionDescription(_Nil<core::T_O>(),entry_point),reg::lisp_classSymbol<T>()) 
    , _duplicationLevel(0){
//    printf("%s:%d  Constructing DefaultConstructorCreator_O with kind: %u\n", __FILE__, __LINE__, gctools::GCStamp<WrapperType>::Kind);
  };
#endif
  DefaultConstructorCreator_O(core::GlobalEntryPoint_sp fdesc) : ConstructorCreator_O(fdesc,reg::lisp_classSymbol<T>()) 
    , _duplicationLevel(0){
//    printf("%s:%d  Constructing DefaultConstructorCreator_O with kind: %u\n", __FILE__, __LINE__, gctools::GCStamp<WrapperType>::Kind);
  };
  DefaultConstructorCreator_O(core::GlobalEntryPoint_sp fdesc, core::Symbol_sp cn, const gctools::Header_s::StampWtagMtag headerValue, int dupnum)
      : ConstructorCreator_O(fdesc, cn), _HeaderValue(headerValue), _duplicationLevel(dupnum){
//    printf("%s:%d  Constructing non trivial DefaultConstructorCreator_O with kind: %u\n", __FILE__, __LINE__, gctools::GCStamp<WrapperType>::Kind);
  };

  /*! If this is the allocator for the original Adapter class return true - otherwise false */
  virtual int duplicationLevel() const { return this->_duplicationLevel; };
  core::T_sp creator_allocate() {
    T *naked_ptr(new T());
    //            printf("%s:%d - creating WrapperType\n", __FILE__,__LINE__);
    gctools::smart_ptr<WrapperType> retval = WrapperType::make_wrapper(naked_ptr, reg::registered_class<T>::id);
    //            clbind::support_enable_wrapper_from_this<T,Pointer>(retval,naked_ptr,naked_ptr);
    return retval;
  }
  core::Creator_sp duplicateForClassName(core::Symbol_sp className) {
    printf("%s:%d  duplicateForClassName %s  this->_HeaderValue = %lu\n", __FILE__, __LINE__, _rep_(className).c_str(), (uintptr_t)this->_HeaderValue._value);
    core::GlobalEntryPoint_sp fdesc = core::makeGlobalEntryPointAndFunctionDescription(_Nil<core::T_O>(),DefaultConstructorCreator_O<T, Pointer>::entry_point);
    core::Creator_sp allocator = gc::As<core::Creator_sp>(gc::GC<DefaultConstructorCreator_O<T, Pointer>>::allocate(fdesc,className, this->_HeaderValue, this->_duplicationLevel + 1));
    return allocator;
  }
};
};

template <typename T, typename Pointer>
class gctools::GCStamp<clbind::DefaultConstructorCreator_O<T, Pointer>> {
public:
  static gctools::GCStampEnum const StampWtag = gctools::GCStamp<typename clbind::DefaultConstructorCreator_O<T, Pointer>::TemplatedBase>::Stamp;
};

namespace clbind {

template <typename T>
class DerivableDefaultConstructorCreator_O : public ConstructorCreator_O {
public:
  typedef ConstructorCreator_O TemplatedBase;
public:
  gctools::Header_s::StampWtagMtag _Header;
  int _duplicationLevel;
public:
public:
  virtual size_t templatedSizeof() const { return sizeof(*this); };
public:
  DerivableDefaultConstructorCreator_O(core::GlobalEntryPoint_sp fdesc) : ConstructorCreator_O(fdesc,reg::lisp_classSymbol<T>())
    , _duplicationLevel(0){};
  DerivableDefaultConstructorCreator_O(core::GlobalEntryPoint_sp fdesc, core::Symbol_sp cn, const gctools::Header_s::StampWtagMtag& header, int dupnum)
      : ConstructorCreator_O(fdesc,cn), _Header(header), _duplicationLevel(dupnum){};

  /*! If this is the allocator for the original Adapter class return true - otherwise false */
  virtual int duplicationLevel() const { return this->_duplicationLevel; };
  core::T_sp creator_allocate() {
    GC_ALLOCATE(T, obj);
    return obj;
  }
  core::Creator_sp duplicateForClassName(core::Symbol_sp className) {
//    printf("%s:%d DerivableDefaultConstructorCreator_O  duplicateForClassName %s  this->_Kind = %u\n", __FILE__, __LINE__, _rep_(className).c_str(), this->_Kind);
    core::GlobalEntryPoint_sp entryPoint = core::makeGlobalEntryPointAndFunctionDescription(_Nil<core::T_O>(),DerivableDefaultConstructorCreator_O<T>::entry_point);
    return gc::As_unsafe<core::Creator_sp>(gc::GC<DerivableDefaultConstructorCreator_O<T>>::allocate(entryPoint,className, this->_Header, this->_duplicationLevel + 1));
  }
};
};

template <typename T>
class gctools::GCStamp<clbind::DerivableDefaultConstructorCreator_O<T>> {
public:
  static gctools::GCStampEnum const StampWtag = gctools::GCStamp<typename clbind::DerivableDefaultConstructorCreator_O<T>::TemplatedBase>::Stamp;
};

namespace clbind {
template <typename Policies, typename T>
class DerivableDefaultConstructorFunctor : public core::Closure_O {
public:
  typedef core::Function_O TemplatedBase;
public:
  enum { NumParams = 0 };
  DerivableDefaultConstructorFunctor(core::FunctionDescription_sp fdesc) : core::Closure_O(ENSURE_ENTRY_POINT(fdesc,entry_point)){};
public:
  virtual size_t templatedSizeof() const { return sizeof(*this); };
public:
  static inline LCC_RETURN LISP_CALLING_CONVENTION() {
    SIMPLE_ERROR_SPRINTF("What do we do when this is called?");
  }
};
};

template <typename Policies, typename T>
class gctools::GCStamp<clbind::DerivableDefaultConstructorFunctor<Policies, T>> {
public:
  static gctools::GCStampEnum const StampWtag = gctools::GCStamp<typename clbind::DerivableDefaultConstructorFunctor<Policies, T>::TemplatedBase>::Stamp;
};

namespace clbind {

template <typename Pols, typename Pointer, typename T, typename Sig> class VariadicConstructorFunction_O;

template <typename Policies, typename Pointer, typename ConstructType ,typename... ARGS >
class VariadicConstructorFunction_O < Policies, Pointer, ConstructType, constructor<ARGS...> > : public core::BuiltinClosure_O {
public:
  typedef VariadicConstructorFunction_O< Policies, Pointer, ConstructType, constructor<ARGS...> > MyType;
  typedef core::BuiltinClosure_O TemplatedBase;
public:
  typedef Wrapper<ConstructType,Pointer>  WrapperType;
public:
  virtual const char* describe() const { return "VariadicConstructorFunctor"; };
  enum { NumParams = sizeof...(ARGS) };
  VariadicConstructorFunction_O(core::GlobalEntryPoint_sp ep) : core::BuiltinClosure_O(ENSURE_ENTRY_POINT(ep,entry_point)) {};
  virtual size_t templatedSizeof() const { return sizeof(*this);};
  virtual void fixupInternalsForImageSaveLoad( imageSaveLoad::Fixup* fixup ) {
    // nothing to do - no wrapped functions
  }
  static inline LCC_RETURN LISP_CALLING_CONVENTION()
  {
    MyType* closure = gctools::untag_general<MyType*>((MyType*)lcc_closure);
    INCREMENT_FUNCTION_CALL_COUNTER(closure);
    INITIALIZE_VA_LIST();
    INVOCATION_HISTORY_FRAME();
    MAKE_STACK_FRAME(frame,closure->asSmartPtr().raw_(),2);
    MAKE_SPECIAL_BINDINGS_HOLDER(numSpecialBindings, specialBindingsVLA,
                                 lisp_lambda_list_handler_number_of_specials(closure->_lambdaListHandler));
    core::StackFrameDynamicScopeManager scope(numSpecialBindings,specialBindingsVLA,frame);
    lambdaListHandler_createBindings(closure->asSmartPtr(),closure->_lambdaListHandler,scope,LCC_PASS_ARGS_LLH);
    core::MultipleValues& returnValues = core::lisp_multipleValues();
    std::tuple<translate::from_object<ARGS>...> all_args = arg_tuple<0,policies<>,ARGS...>::go(frame->arguments());
    return constructor_apply_and_return<WrapperType,Policies,ConstructType,decltype(all_args)>::go(returnValues,std::move(all_args));
  }
};
};

template <typename Pols, typename Pointer, typename T, typename Sig>
class gctools::GCStamp<clbind::VariadicConstructorFunction_O<Pols, Pointer, T, Sig>> {
public:
  static gctools::GCStampEnum const StampWtag = gctools::GCStamp<typename clbind::VariadicConstructorFunction_O<Pols, Pointer, T, Sig>::TemplatedBase>::Stamp;
};


namespace clbind {
template <typename... SIGS>
using init = constructor<SIGS...>;

};
#endif
