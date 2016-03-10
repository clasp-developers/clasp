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

class ConstructorCreator : public core::Creator {
public:
  ConstructorCreator(core::Symbol_sp c) : _mostDerivedClassSymbol(c){};
  core::Symbol_sp _mostDerivedClassSymbol;
};
};

namespace clbind {

template <typename T, typename Pointer>
class DefaultConstructorCreator : public ConstructorCreator {
public:
  typedef ConstructorCreator TemplatedBase;

public:
  typedef Wrapper<T, Pointer> WrapperType;
  int _Kind;
  int _duplicationLevel;

public:
  virtual size_t templatedSizeof() const { return sizeof(*this); };

public:
#if 0
        virtual GC_RESULT onHeapScanGCRoots(GC_SCAN_ARGS_PROTOTYPE)
        {
#ifdef USE_MPS
            MPS_SCAN_BEGIN(GC_SCAN_STATE) {
                SMART_PTR_FIX(this->_mostDerivedClassSymbol);
            } MPS_SCAN_END(GC_SCAN_STATE);
#endif
            return GC_RES_OK;
        }
#endif
public:
  DISABLE_NEW();
  DefaultConstructorCreator() : ConstructorCreator(reg::lisp_classSymbol<T>())
#ifdef USE_MPS
                                ,
                                _Kind(gctools::GCKind<WrapperType>::Kind)
#endif
                                ,
                                _duplicationLevel(0){};
  DefaultConstructorCreator(core::Symbol_sp cn, int kind, int dupnum)
      : ConstructorCreator(cn), _Kind(kind), _duplicationLevel(dupnum){};

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
  gc::tagged_pointer<Creator> duplicateForClassName(core::Symbol_sp className) {
    gc::tagged_pointer<Creator> allocator = gctools::ClassAllocator<DefaultConstructorCreator<T, Pointer>>::allocate_class(className, this->_Kind, this->_duplicationLevel + 1);
    return allocator;
  }
};
};

template <typename T, typename Pointer>
class gctools::GCKind<clbind::DefaultConstructorCreator<T, Pointer>> {
public:
  static gctools::GCKindEnum const Kind = gctools::GCKind<typename clbind::DefaultConstructorCreator<T, Pointer>::TemplatedBase>::Kind;
};

namespace clbind {

template <typename T>
class DerivableDefaultConstructorCreator : public ConstructorCreator {
public:
  typedef ConstructorCreator TemplatedBase;

public:
  int _Kind;
  int _duplicationLevel;

public:
  DISABLE_NEW();

public:
  virtual size_t templatedSizeof() const { return sizeof(*this); };
#if 0
        virtual GC_RESULT onHeapScanGCRoots(GC_SCAN_ARGS_PROTOTYPE)
        {
#ifdef USE_MPS
            MPS_SCAN_BEGIN(GC_SCAN_STATE) {
                SMART_PTR_FIX(this->_mostDerivedClassSymbol);
            } MPS_SCAN_END(GC_SCAN_STATE);
#endif
            return GC_RES_OK;
        }
#endif
public:
  DerivableDefaultConstructorCreator() : ConstructorCreator(reg::lisp_classSymbol<T>())
#ifdef USE_MPS
                                         ,
                                         _Kind(gctools::GCKind<T>::Kind)
#endif
                                         ,
                                         _duplicationLevel(0){};
  DerivableDefaultConstructorCreator(core::Symbol_sp cn, int kind, int dupnum)
      : ConstructorCreator(cn), _Kind(kind), _duplicationLevel(dupnum){};

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
  gc::tagged_pointer<Creator> duplicateForClassName(core::Symbol_sp className) {
    return gctools::ClassAllocator<DerivableDefaultConstructorCreator<T>>::allocate_class(className, this->_Kind, this->_duplicationLevel + 1);
  }
};
};

template <typename T>
class gctools::GCKind<clbind::DerivableDefaultConstructorCreator<T>> {
public:
  static gctools::GCKindEnum const Kind = gctools::GCKind<typename clbind::DerivableDefaultConstructorCreator<T>::TemplatedBase>::Kind;
};

namespace clbind {
template <typename Policies, typename T>
class DerivableDefaultConstructorFunctoid : public core::Functoid {
public:
  typedef core::Functoid TemplatedBase;

public:
  enum { NumParams = 0 };
  DerivableDefaultConstructorFunctoid(core::T_sp name) : core::Functoid(name){};

public:
  virtual size_t templatedSizeof() const { return sizeof(*this); };

public:
  DISABLE_NEW();
#if 0
        core::T_mv activate( core::ActivationFrame_sp closedOverFrame, int numArgs, ArgArray args )
        {
            int countPureOutValues = CountPureOutValues<Policies>::value;
            if ( numArgs != (NumParams-countPureOutValues) )
            {
                core::wrongNumberOfArguments(numArgs,(NumParams-countPureOutValues));
            }
            GC_ALLOCATE(T,obj_gc_safe);
            printf("%s:%d Allocating instance of Derivable class: %s\n", __FILE__, __LINE__, this->_Name.c_str() );
            int oidx = 1;
            return gctools::multiple_values<core::T_O>(obj_gc_safe,oidx);
        }
#endif
};
};

template <typename Policies, typename T>
class gctools::GCKind<clbind::DerivableDefaultConstructorFunctoid<Policies, T>> {
public:
  static gctools::GCKindEnum const Kind = gctools::GCKind<typename clbind::DerivableDefaultConstructorFunctoid<Policies, T>::TemplatedBase>::Kind;
};

namespace clbind {

template <typename Pols, typename Pointer, typename T, typename Sig>
class VariadicConstructorFunctoid : public core::BuiltinClosure {
public:
  typedef core::BuiltinClosure TemplatedBase;

public:
  virtual size_t templatedSizeof() const { return sizeof(*this); };
};

#include <clasp/clbind/generated/clbind_constructor_functoids.h>
};

template <typename Pols, typename Pointer, typename T, typename Sig>
class gctools::GCKind<clbind::VariadicConstructorFunctoid<Pols, Pointer, T, Sig>> {
public:
  static gctools::GCKindEnum const Kind = gctools::GCKind<typename clbind::VariadicConstructorFunctoid<Pols, Pointer, T, Sig>::TemplatedBase>::Kind;
};

#endif
