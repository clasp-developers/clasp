#pragma once

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

#include <clasp/clbind/adapter.fwd.h>
#include <clasp/clbind/clbind_wrappers.h>
#include <clasp/clbind/policies.h>
#include <clasp/clbind/adapter.h>
#include <clasp/clbind/details.h>

namespace clbind {

template <typename... SIGS> struct constructor {};

typedef enum { create_default_constructor } default_constructor_type;

typedef constructor<> default_constructor;

class ConstructorCreator_O : public core::Creator_O {
  LISP_ABSTRACT_CLASS(clbind, ClbindPkg, ConstructorCreator_O, "ConstructorCreator", core::Creator_O);

public:
  ConstructorCreator_O(core::SimpleFun_sp ep, core::Symbol_sp c) : Creator_O(ep), _mostDerivedClassSymbol(c){};
  core::Symbol_sp _mostDerivedClassSymbol;
};
}; // namespace clbind

namespace clbind {

template <typename T, typename Pointer> class DefaultConstructorCreator_O : public ConstructorCreator_O {
public:
  typedef ConstructorCreator_O TemplatedBase;

public:
  typedef Wrapper<T, Pointer> WrapperType;
  gctools::Header_s::StampWtagMtag _HeaderValue;
  int _duplicationLevel;

public:
  virtual size_t templatedSizeof() const { return sizeof(*this); };

public:
  DefaultConstructorCreator_O(core::SimpleFun_sp ep)
      : ConstructorCreator_O(ep, reg::lisp_classSymbol<T>()),
        _duplicationLevel(0){};
  DefaultConstructorCreator_O(core::SimpleFun_sp ep, core::Symbol_sp cn,
                              const gctools::Header_s::StampWtagMtag headerValue, int dupnum)
      : ConstructorCreator_O(ep, cn), _HeaderValue(headerValue),
        _duplicationLevel(dupnum){};

  /*! If this is the allocator for the original Adapter class return true - otherwise false */
  virtual int duplicationLevel() const { return this->_duplicationLevel; };
  core::T_sp creator_allocate() {
    T* naked_ptr(new T());
    //            printf("%s:%d - creating WrapperType\n", __FILE__,__LINE__);
    gctools::smart_ptr<WrapperType> retval = WrapperType::make_wrapper(naked_ptr, reg::registered_class<T>::id);
    //            clbind::support_enable_wrapper_from_this<T,Pointer>(retval,naked_ptr,naked_ptr);
    return retval;
  }
  core::Creator_sp duplicateForClassName(core::Symbol_sp className) {
    printf("%s:%d  duplicateForClassName %s  this->_HeaderValue = %lu\n", __FILE__, __LINE__, _rep_(className).c_str(),
           (uintptr_t)this->_HeaderValue._value);
    core::SimpleFun_sp fdesc = core::makeSimpleFunAndFunctionDescription<DefaultConstructorCreator_O<T, Pointer>>(
        nil<core::T_O>());
    core::Creator_sp allocator = gc::As<core::Creator_sp>(gc::GC<DefaultConstructorCreator_O<T, Pointer>>::allocate(
        fdesc, className, this->_HeaderValue, this->_duplicationLevel + 1));
    return allocator;
  }
};
}; // namespace clbind

template <typename T, typename Pointer> class gctools::GCStamp<clbind::DefaultConstructorCreator_O<T, Pointer>> {
public:
  static gctools::GCStampEnum const StampWtag =
      gctools::GCStamp<typename clbind::DefaultConstructorCreator_O<T, Pointer>::TemplatedBase>::StampWtag;
};

namespace clbind {

template <typename T> class DerivableDefaultConstructorCreator_O : public ConstructorCreator_O {
public:
  typedef ConstructorCreator_O TemplatedBase;

public:
  gctools::Header_s::StampWtagMtag _Header;
  int _duplicationLevel;

public:
public:
  virtual size_t templatedSizeof() const { return sizeof(*this); };

public:
  DerivableDefaultConstructorCreator_O(core::SimpleFun_sp ep)
      : ConstructorCreator_O(ep, reg::lisp_classSymbol<T>()), _duplicationLevel(0){};
  DerivableDefaultConstructorCreator_O(core::SimpleFun_sp ep, core::Symbol_sp cn,
                                       const gctools::Header_s::StampWtagMtag& header, int dupnum)
    : ConstructorCreator_O(ep, cn), _Header(header), _duplicationLevel(dupnum){};

  /*! If this is the allocator for the original Adapter class return true - otherwise false */
  virtual int duplicationLevel() const { return this->_duplicationLevel; };
  core::T_sp creator_allocate() {
    auto obj = gctools::GC<T>::allocate_with_default_constructor();
    return obj;
  }
  core::Creator_sp duplicateForClassName(core::Symbol_sp className) {
    //    printf("%s:%d DerivableDefaultConstructorCreator_O  duplicateForClassName %s  this->_Kind = %u\n", __FILE__, __LINE__,
    //    _rep_(className).c_str(), this->_Kind);
    core::SimpleFun_sp entryPoint = core::makeSimpleFunAndFunctionDescription<DerivableDefaultConstructorCreator_O<T>>(
        nil<core::T_O>());
    return gc::As_unsafe<core::Creator_sp>(gc::GC<DerivableDefaultConstructorCreator_O<T>>::allocate(
        entryPoint, className, this->_Header, this->_duplicationLevel + 1));
  }
};
}; // namespace clbind

template <typename T> class gctools::GCStamp<clbind::DerivableDefaultConstructorCreator_O<T>> {
public:
  static gctools::GCStampEnum const StampWtag =
      gctools::GCStamp<typename clbind::DerivableDefaultConstructorCreator_O<T>::TemplatedBase>::StampWtag;
};

namespace clbind {
template <typename Policies, typename T> class DerivableDefaultConstructorFunctor : public core::Function_O {
public:
  typedef core::Function_O TemplatedBase;

public:
  enum { NumParams = 0 };
  DerivableDefaultConstructorFunctor(core::SimpleFun_sp ep) : core::Function_O(ep){};

public:
  virtual size_t templatedSizeof() const { return sizeof(*this); };

public:
  static inline LCC_RETURN LISP_CALLING_CONVENTION() { SIMPLE_ERROR("What do we do when this is called?"); }
  template <typename... Ts>
  static inline LCC_RETURN entry_point_fixed(core::T_O* lcc_closure,
                                             Ts... args) {
    SIMPLE_ERROR("What do we do when this is called?");
  }
};
}; // namespace clbind

namespace clbind {

template <typename Sig, typename Pols, typename Pointer, typename T> class WRAPPER_Constructor_O;
};

namespace clbind {
template <typename ConstructorPtrType, typename Policies, typename Pointer, typename ConstructType>
class WRAPPER_Constructor_O : public core::SimpleFun_O {};

template <typename... ARGS, typename Policies, typename Pointer, typename ConstructType>
class WRAPPER_Constructor_O<constructor<ARGS...>, Policies, Pointer, ConstructType>
    : public core::SimpleFun_O {
public:
  typedef WRAPPER_Constructor_O<constructor<ARGS...>, Policies, Pointer, ConstructType> MyType;
  typedef core::SimpleFun_O TemplatedBase;
  typedef Wrapper<ConstructType, Pointer> WrapperType;

public:
public:
  virtual const char* describe() const { return "VariadicConstructorFunctor"; };
  enum { NumParams = sizeof...(ARGS) };
  WRAPPER_Constructor_O(core::FunctionDescription_sp fdesc)
    : core::SimpleFun_O(fdesc, nil<core::T_O>(), core::XepStereotype<MyType>()){};
  virtual size_t templatedSizeof() const { return sizeof(*this); };

  static inline LCC_RETURN entry_point_n(core::T_O* lcc_closure, size_t lcc_nargs,
                                         core::T_O** lcc_args) {
    DO_DRAG_CXX_CALLS();
    if (lcc_nargs != NumParams)
      cc_wrong_number_of_arguments(lcc_closure, lcc_nargs, NumParams, NumParams);
    auto all_args = arg_tuple<0, policies<>, ARGS...>::goFrame(lcc_args);
    return constructor_apply_and_return<WrapperType, Policies, ConstructType, decltype(all_args)>::go(std::move(all_args));
  }

  template <typename... Ts>
  static inline LCC_RETURN entry_point_fixed(core::T_O* lcc_closure,
                                             Ts... args) {
    DO_DRAG_CXX_CALLS();
    if constexpr(sizeof...(Ts) != NumParams) {
      cc_wrong_number_of_arguments(lcc_closure, sizeof...(Ts), NumParams, NumParams);
      UNREACHABLE();
    } else {
      auto all_args = arg_tuple<0, policies<>, ARGS...>::goArgs(args...);
      return constructor_apply_and_return<WrapperType, Policies, ConstructType, decltype(all_args)>::go(std::move(all_args));
    }
  }
};
}; // namespace clbind

template <typename Sig, typename Pols, typename Pointer, typename T>
class gctools::GCStamp<clbind::WRAPPER_Constructor_O<Sig, Pols, Pointer, T>> {
public:
  static gctools::GCStampEnum const StampWtag =
      gctools::GCStamp<typename clbind::WRAPPER_Constructor_O<Sig, Pols, Pointer, T>::TemplatedBase>::StampWtag;
};

template <typename Sig, typename Pols, typename Pointer, typename T>
struct gctools::Inherits<typename clbind::WRAPPER_Constructor_O<Sig, Pols, Pointer, T>::TemplatedBase,
                         clbind::WRAPPER_Constructor_O<Sig, Pols, Pointer, T>> : public std::true_type {};

template <typename Policies, typename T> class gctools::GCStamp<clbind::DerivableDefaultConstructorFunctor<Policies, T>> {
public:
  static gctools::GCStampEnum const StampWtag =
      gctools::GCStamp<typename clbind::DerivableDefaultConstructorFunctor<Policies, T>::TemplatedBase>::StampWtag;
};
template <typename Policies, typename T>
struct gctools::Inherits<typename clbind::DerivableDefaultConstructorFunctor<Policies, T>::TemplatedBase,
                         clbind::DerivableDefaultConstructorFunctor<Policies, T>> : public std::true_type {};

namespace clbind {
template <typename... SIGS> using init = constructor<SIGS...>;

};
