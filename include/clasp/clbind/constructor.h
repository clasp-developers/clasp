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
public:
  ConstructorCreator_O(core::Symbol_sp c) : _mostDerivedClassSymbol(c){};
  core::Symbol_sp _mostDerivedClassSymbol;
};
};

namespace clbind {

template <typename T, typename Pointer>
class DefaultConstructorCreator_O : public ConstructorCreator_O {
public:
  typedef ConstructorCreator_O TemplatedBase;
public:
  typedef Wrapper<T, Pointer> WrapperType;
  int _Kind;
  int _duplicationLevel;

public:
  virtual size_t templatedSizeof() const { return sizeof(*this); };

public:
public:
  DefaultConstructorCreator_O() : ConstructorCreator_O(reg::lisp_classSymbol<T>())
#ifndef USE_CXX_DYNAMIC_CAST
                                ,
                                _Kind(gctools::GCKind<WrapperType>::Kind)
#endif
                                ,
                                _duplicationLevel(0){};
  DefaultConstructorCreator_O(core::Symbol_sp cn, int kind, int dupnum)
      : ConstructorCreator_O(cn), _Kind(kind), _duplicationLevel(dupnum){};

  /*! If this is the allocator for the original Adapter class return true - otherwise false */
  virtual int duplicationLevel() const { return this->_duplicationLevel; };
  void describe() const {
    stringstream ss;
    core::Symbol_sp baseClassSymbol = reg::lisp_classSymbol<T>();
    ss << "DefaultConstructorCreator for class " << _rep_(baseClassSymbol);
    if (baseClassSymbol != this->_mostDerivedClassSymbol) {
      ss << " derived class " << _rep_(this->_mostDerivedClassSymbol);
    }
    printf("%s", ss.str().c_str());
  }
  core::T_sp allocate() {
    T *naked_ptr(new T());
    //            printf("%s:%d - creating WrapperType\n", __FILE__,__LINE__);
    gctools::smart_ptr<WrapperType> retval = WrapperType::create(naked_ptr, reg::registered_class<T>::id);
    //            clbind::support_enable_wrapper_from_this<T,Pointer>(retval,naked_ptr,naked_ptr);
    return retval;
  }
  core::Creator_sp duplicateForClassName(core::Symbol_sp className) {
    core::Creator_sp allocator = gc::GC<DefaultConstructorCreator_O<T, Pointer>>::allocate(className, this->_Kind, this->_duplicationLevel + 1);
    return allocator;
  }
};
};

template <typename T, typename Pointer>
class gctools::GCKind<clbind::DefaultConstructorCreator_O<T, Pointer>> {
public:
  static gctools::GCKindEnum const Kind = gctools::GCKind<typename clbind::DefaultConstructorCreator_O<T, Pointer>::TemplatedBase>::Kind;
};

namespace clbind {

template <typename T>
class DerivableDefaultConstructorCreator_O : public ConstructorCreator_O {
public:
  typedef ConstructorCreator_O TemplatedBase;
public:
  int _Kind;
  int _duplicationLevel;
public:
  DISABLE_NEW();
public:
  virtual size_t templatedSizeof() const { return sizeof(*this); };
public:
  DerivableDefaultConstructorCreator_O() : ConstructorCreator_O(reg::lisp_classSymbol<T>())
#ifdef USE_CXX_DYNAMIC_CAST
                                         ,
                                         _Kind(gctools::GCKind<T>::Kind)
#endif
                                         ,
                                         _duplicationLevel(0){};
  DerivableDefaultConstructorCreator_O(core::Symbol_sp cn, int kind, int dupnum)
      : ConstructorCreator_O(cn), _Kind(kind), _duplicationLevel(dupnum){};

  /*! If this is the allocator for the original Adapter class return true - otherwise false */
  virtual int duplicationLevel() const { return this->_duplicationLevel; };
  void describe() const {
    stringstream ss;
    core::Symbol_sp baseClassSymbol = reg::lisp_classSymbol<T>();
    ss << "DerivableDefaultConstructorCreator for class " << _rep_(baseClassSymbol);
    if (baseClassSymbol != this->_mostDerivedClassSymbol) {
      ss << " derived class " << _rep_(this->_mostDerivedClassSymbol);
    }
    printf("%s", ss.str().c_str());
  }
  core::T_sp allocate() {
    GC_ALLOCATE(T, obj);
    return obj;
  }
  core::Creator_sp duplicateForClassName(core::Symbol_sp className) {
    return gc::GC<DerivableDefaultConstructorCreator_O<T>>::allocate(className, this->_Kind, this->_duplicationLevel + 1);
  }
};
};

template <typename T>
class gctools::GCKind<clbind::DerivableDefaultConstructorCreator_O<T>> {
public:
  static gctools::GCKindEnum const Kind = gctools::GCKind<typename clbind::DerivableDefaultConstructorCreator_O<T>::TemplatedBase>::Kind;
};

namespace clbind {
template <typename Policies, typename T>
class DerivableDefaultConstructorFunctor : public core::Functor_O {
public:
  typedef core::Functor_O TemplatedBase;
public:
  enum { NumParams = 0 };
  DerivableDefaultConstructorFunctor(core::T_sp name) : core::Functor_O(name){};
public:
  virtual size_t templatedSizeof() const { return sizeof(*this); };
public:
};
};

template <typename Policies, typename T>
class gctools::GCKind<clbind::DerivableDefaultConstructorFunctor<Policies, T>> {
public:
  static gctools::GCKindEnum const Kind = gctools::GCKind<typename clbind::DerivableDefaultConstructorFunctor<Policies, T>::TemplatedBase>::Kind;
};

namespace clbind {

template <typename Pols, typename Pointer, typename T, typename Sig> class VariadicConstructorFunctor_O : public core::BuiltinClosure_O {
public:
  typedef core::BuiltinClosure_O TemplatedBase;
public:
  virtual size_t templatedSizeof() const { return sizeof(*this); };
};

#include <clasp/clbind/generated/clbind_constructor_functoids.h>
};

template <typename Pols, typename Pointer, typename T, typename Sig>
class gctools::GCKind<clbind::VariadicConstructorFunctor_O<Pols, Pointer, T, Sig>> {
public:
  static gctools::GCKindEnum const Kind = gctools::GCKind<typename clbind::VariadicConstructorFunctor_O<Pols, Pointer, T, Sig>::TemplatedBase>::Kind;
};

#endif
