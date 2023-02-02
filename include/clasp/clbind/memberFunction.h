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
template <typename MethodPtrType, typename Policies, typename ArgumentWrapper >
class WRAPPER_AlienVariadicMethod;
};

namespace clbind {

template <typename Policies, typename RT, typename OT, typename... ARGS, typename ArgumentWrapper >
class WRAPPER_AlienVariadicMethod < RT(OT::*)(ARGS...), Policies, ArgumentWrapper > : public core::GlobalSimpleFunBase_O {
public:
  typedef WRAPPER_AlienVariadicMethod< RT(OT::*)(ARGS...), Policies, ArgumentWrapper > MyType;
  typedef RT(OT::*MethodType)(ARGS...) ;
  typedef core::GlobalSimpleFunBase_O TemplatedBase;
public:
  MethodType           mptr;
public:

  enum { NumParams = sizeof...(ARGS)+1 };

  WRAPPER_AlienVariadicMethod(MethodType ptr, core::FunctionDescription_sp fdesc, core::T_sp code)
    : GlobalSimpleFunBase_O(fdesc,core::ClaspXepFunction::make<MyType>(),code), mptr(ptr)  {
    this->validateCodePointer((void**)&this->mptr,sizeof(this->mptr));
  };

  virtual const char* describe() const {return "IndirectVariadicMethoid";};

  virtual size_t templatedSizeof() const { return sizeof(*this);};

  void fixupInternalsForSnapshotSaveLoad( snapshotSaveLoad::Fixup* fixup ) {
    this->TemplatedBase::fixupInternalsForSnapshotSaveLoad(fixup);
    this->fixupOneCodePointer( fixup, (void**)&this->mptr );
  };

  static inline LCC_RETURN wrapper_entry_point_n(const BytecodeWrapper& dummy, core::T_O* lcc_closure, size_t lcc_nargs, core::T_O** lcc_args )
  {
    MyType* closure = gctools::untag_general<MyType*>((MyType*)lcc_closure);
    INCREMENT_FUNCTION_CALL_COUNTER(closure);
    DO_DRAG_CXX_CALLS();
    if (lcc_nargs!=NumParams) cc_wrong_number_of_arguments(lcc_closure,lcc_nargs,NumParams,NumParams);
    core::T_sp ootep((gctools::Tagged)lcc_args[0]);
    translate::from_object<OT*> otep(ootep);
    std::tuple<translate::from_object<ARGS>...> all_args = clbind::arg_tuple<1,Policies,ARGS...>::goFrame(lcc_args);
    return clbind::clbind_external_method_apply_and_return<Policies,RT,decltype(closure->mptr),OT*,decltype(all_args)>::go(std::move(closure->mptr),otep._v,std::move(all_args));
  }

  static inline LCC_RETURN entry_point_n(core::T_O* lcc_closure, size_t lcc_nargs, core::T_O** lcc_args ) {
    return wrapper_entry_point_n(ArgumentWrapper(),lcc_closure,lcc_nargs,lcc_args);
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

namespace clbind {

template <typename Policies, typename RT, typename OT, typename... ARGS, typename ArgumentWrapper >
class WRAPPER_AlienVariadicMethod < RT(OT::*)(ARGS...) const, Policies, ArgumentWrapper > : public core::GlobalSimpleFunBase_O {
public:
  typedef WRAPPER_AlienVariadicMethod< RT(OT::*)(ARGS...) const, Policies, ArgumentWrapper > MyType;
  typedef RT(OT::*MethodType)(ARGS...) const;
  typedef core::GlobalSimpleFunBase_O TemplatedBase;
public:
  MethodType           mptr;
public:

  enum { NumParams = sizeof...(ARGS)+1 };

  WRAPPER_AlienVariadicMethod(MethodType ptr, core::FunctionDescription_sp fdesc, core::T_sp code)
    : GlobalSimpleFunBase_O(fdesc,core::ClaspXepFunction::make<MyType>(),code), mptr(ptr) {
    this->validateCodePointer((void**)&this->mptr,sizeof(this->mptr));
  };

  virtual const char* describe() const {return "IndirectVariadicMethoid";};

  virtual size_t templatedSizeof() const { return sizeof(*this);};

  void fixupInternalsForSnapshotSaveLoad( snapshotSaveLoad::Fixup* fixup ) {
    this->TemplatedBase::fixupInternalsForSnapshotSaveLoad(fixup);
    this->fixupOneCodePointer( fixup, (void**)&this->mptr );
  };

  static inline LCC_RETURN wrapper_entry_point_n(const BytecodeWrapper& dummy, core::T_O* lcc_closure, size_t lcc_nargs, core::T_O** lcc_args )
  {
    MyType* closure = gctools::untag_general<MyType*>((MyType*)lcc_closure);
    INCREMENT_FUNCTION_CALL_COUNTER(closure);
    DO_DRAG_CXX_CALLS();
    if (lcc_nargs!=NumParams) cc_wrong_number_of_arguments(lcc_closure,lcc_nargs,NumParams,NumParams);
    core::T_sp ootep((gctools::Tagged)lcc_args[0]);
    translate::from_object<OT*> otep(ootep);
    std::tuple<translate::from_object<ARGS>...> all_args = clbind::arg_tuple<1,Policies,ARGS...>::goFrame(lcc_args);
    return clbind::clbind_external_method_apply_and_return<Policies,RT,decltype(closure->mptr),OT*,decltype(all_args)>::go(std::move(closure->mptr),otep._v,std::move(all_args));
  }

  static inline LCC_RETURN entry_point_n(core::T_O* lcc_closure, size_t lcc_nargs, core::T_O** lcc_args ) {
    return wrapper_entry_point_n(ArgumentWrapper(),lcc_closure,lcc_nargs,lcc_args);
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


template < typename MethodPtrType, typename Pols, typename ArgumentHandler >
class gctools::GCStamp<clbind::WRAPPER_AlienVariadicMethod< MethodPtrType, Pols, ArgumentHandler >> {
public:
  static gctools::GCStampEnum const StampWtag = gctools::GCStamp<typename clbind::WRAPPER_AlienVariadicMethod< MethodPtrType, Pols, ArgumentHandler >::TemplatedBase>::StampWtag;
};

template < typename MethodPtrType, typename Pols, typename ArgumentHandler >
class gctools::Inherits<typename clbind::WRAPPER_AlienVariadicMethod< MethodPtrType, Pols, ArgumentHandler >::TemplatedBase, clbind::WRAPPER_AlienVariadicMethod< MethodPtrType, Pols, ArgumentHandler >> : public std::true_type {};




#endif
