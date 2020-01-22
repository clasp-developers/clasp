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

typedef enum { no_default_constructor } no_default_constructor_type;

typedef constructor<> default_constructor;

class ConstructorCreator_O : public core::Creator_O {
  LISP_CLASS(clbind,ClbindPkg,ConstructorCreator_O,"ConstructorCreator",core::Creator_O);
public:
  ConstructorCreator_O(core::Symbol_sp c) : _mostDerivedClassSymbol(c){};
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
 DefaultConstructorCreator_O() : ConstructorCreator_O(reg::lisp_classSymbol<T>()) 
#ifndef USE_CXX_DYNAMIC_CAST
    , _HeaderValue(gctools::Header_s::StampWtagMtag::make<WrapperType>())
#endif
    , _duplicationLevel(0){
//    printf("%s:%d  Constructing DefaultConstructorCreator_O with kind: %u\n", __FILE__, __LINE__, gctools::GCStamp<WrapperType>::Kind);
  };
 DefaultConstructorCreator_O(core::Symbol_sp cn, const gctools::Header_s::StampWtagMtag headerValue, int dupnum)
   : ConstructorCreator_O(cn), _HeaderValue(headerValue), _duplicationLevel(dupnum){
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
    printf("%s:%d  duplicateForClassName %s  this->_HeaderValue = %" Ptagged_stamp_t "\n", __FILE__, __LINE__, _rep_(className).c_str(), this->_HeaderValue._value);
    core::Creator_sp allocator = gc::As<core::Creator_sp>(gc::GC<DefaultConstructorCreator_O<T, Pointer>>::allocate(className, this->_HeaderValue, this->_duplicationLevel + 1));
    return allocator;
  }
};
};

template <typename T, typename Pointer>
class gctools::GCStamp<clbind::DefaultConstructorCreator_O<T, Pointer>> {
public:
  static gctools::GCStampEnum const Stamp = gctools::GCStamp<typename clbind::DefaultConstructorCreator_O<T, Pointer>::TemplatedBase>::Stamp;
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
  DerivableDefaultConstructorCreator_O() : ConstructorCreator_O(reg::lisp_classSymbol<T>())
#ifdef USE_CXX_DYNAMIC_CAST
    , _Header(gctools::Header_s::StampWtagMtag::make<T>())
#endif
    , _duplicationLevel(0){};
 DerivableDefaultConstructorCreator_O(core::Symbol_sp cn, const gctools::Header_s::StampWtagMtag& header, int dupnum)
      : ConstructorCreator_O(cn), _Header(header), _duplicationLevel(dupnum){};

  /*! If this is the allocator for the original Adapter class return true - otherwise false */
  virtual int duplicationLevel() const { return this->_duplicationLevel; };
  core::T_sp creator_allocate() {
    GC_ALLOCATE(T, obj);
    return obj;
  }
  core::Creator_sp duplicateForClassName(core::Symbol_sp className) {
//    printf("%s:%d DerivableDefaultConstructorCreator_O  duplicateForClassName %s  this->_Kind = %u\n", __FILE__, __LINE__, _rep_(className).c_str(), this->_Kind);
    return gc::As_unsafe<core::Creator_sp>(gc::GC<DerivableDefaultConstructorCreator_O<T>>::allocate(className, this->_Header, this->_duplicationLevel + 1));
  }
};
};

template <typename T>
class gctools::GCStamp<clbind::DerivableDefaultConstructorCreator_O<T>> {
public:
  static gctools::GCStampEnum const Stamp = gctools::GCStamp<typename clbind::DerivableDefaultConstructorCreator_O<T>::TemplatedBase>::Stamp;
};

namespace clbind {
template <typename Policies, typename T>
class DerivableDefaultConstructorFunctor : public core::Closure_O {
public:
  typedef core::Function_O TemplatedBase;
public:
  enum { NumParams = 0 };
 DerivableDefaultConstructorFunctor(core::FunctionDescription* fdesc) : core::Closure_O(entry_point,fdesc){};
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
  static gctools::GCStampEnum const Stamp = gctools::GCStamp<typename clbind::DerivableDefaultConstructorFunctor<Policies, T>::TemplatedBase>::Stamp;
};

namespace clbind {

template <typename Pols, typename Pointer, typename T, typename Sig> class VariadicConstructorFunction_O : public core::BuiltinClosure_O {
public:
  typedef core::BuiltinClosure_O TemplatedBase;
public:
  virtual size_t templatedSizeof() const { return sizeof(*this); };
};

#include <clasp/clbind/clbind_constructor_functoids.h>
};

template <typename Pols, typename Pointer, typename T, typename Sig>
class gctools::GCStamp<clbind::VariadicConstructorFunction_O<Pols, Pointer, T, Sig>> {
public:
  static gctools::GCStampEnum const Stamp = gctools::GCStamp<typename clbind::VariadicConstructorFunction_O<Pols, Pointer, T, Sig>::TemplatedBase>::Stamp;
};

#endif
