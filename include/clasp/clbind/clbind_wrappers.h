/*
    File: clbind_wrappers.h
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
#ifndef clbind_wrappers_H
#define clbind_wrappers_H

#include <clasp/core/foundation.h>
#include <clasp/core/wrappedPointer.h>
#include <clasp/core/instance.h>
#include <clasp/clbind/adapter.fwd.h>
#include <clasp/clbind/derivable.h>
#include <clasp/clbind/inheritance.h>

namespace clbind {

template <class OT, class WT>
gctools::smart_ptr<OT> RP_Create_wrapper() {
  _G();
  GC_ALLOCATE(OT, wrapper);
  return wrapper;
}

template <typename T>
struct no_deleter {
  static void deleter(T *p){};
};

template <typename T>
struct new_deleter {
  static void deleter(T *p) {
    delete p;
  };
};

template <typename T, typename = void>
struct maybe_release {
  static void call(T &obj) {
    //            printf("%s:%d - no release to call\n", __FILE__, __LINE__);
  }
};

template <typename T>
struct maybe_release<T, decltype(std::declval<T>().release(), void(0))> {
  static void call(T &obj) {
    //            printf("%s:%d - calling release\n", __FILE__, __LINE__);
    obj.release();
  }
};

struct is_deletable_impl {
  template <class T, class U = decltype(delete std::declval<T>())>
  static std::true_type test(int);
  template <class>
  static std::false_type test(...);
};

template <class T>
struct is_deletable : decltype(is_deletable_impl::test<T>(0)) {};

template <typename OT, bool Deletable>
struct maybe_delete {
  static void doit(OT *ptr) { delete ptr; };
};

template <typename OT>
struct maybe_delete<OT, false> {
  static void doit(OT *ptr){};
};
};

namespace clbind {

/*! Wrappers wrap external pointers - 
      The wrapper does not own the pointer unless the HolderType is a std::unique_ptr or some other
      smart_ptr type */
template <class OT, class HolderType = OT *>
class Wrapper : public core::WrappedPointer_O /*, public gctools::GC_MergeKinds*/ {
public:
  typedef core::WrappedPointer_O TemplatedBase;

public:
  typedef Wrapper<OT, HolderType> WrapperType;
  typedef OT ExternalType;

public: // Do NOT declare any smart_ptr's or weak_smart_ptr's here!!!!
  HolderType externalPtr_gc_ignore;
  OT *nakedPtr_gc_ignore; // weak pointer
  class_id _classId;

public:
  Wrapper(OT *naked, class_id cid) : externalPtr_gc_ignore(naked), nakedPtr_gc_ignore(naked), _classId(cid){
                                                                                                  //            printf("\n%s:%d - ctor for Wrapper@%p HolderType=%s OT*=%p adapter@%p cid=%lu  symbol=%s\n", __FILE__, __LINE__, this, typeid(HolderType).name(),this->nakedPtr,clbind::support_adapterAddress<ExternalType>(this->nakedPtr), cid, _rep_(reg::globalClassIdToClassSymbol[cid]).c_str() );
                                                                                              };

  // ctor that takes a unique_ptr
  Wrapper(std::unique_ptr<OT> naked, class_id cid) : externalPtr_gc_ignore(std::move(naked)), _classId(cid) {
    nakedPtr_gc_ignore = &(*this->externalPtr_gc_ignore); // seriously - do I need nakedPtr?????????
                                                          //            printf("\n%s:%d - ctor for Wrapper@%p HolderType=%s OT*=%p adapter@%p cid=%lu  symbol=%s\n", __FILE__, __LINE__, this, typeid(HolderType).name(),this->nakedPtr,clbind::support_adapterAddress<ExternalType>(this->nakedPtr), cid, _rep_(reg::globalClassIdToClassSymbol[cid]).c_str() );
  };

  size_t templatedSizeof() const { return sizeof(*this); };
  void *mostDerivedPointer() const { return (void *)(this->nakedPtr_gc_ignore); };

  virtual class_id classId() const { return this->_classId; };

  /*! Release the pointer - invalidate the wrapper and return the pointer */
  virtual void pointerDelete() {
    if (this->nakedPtr_gc_ignore != NULL) {
      maybe_delete<OT, is_deletable<OT>::value>::doit(this->nakedPtr_gc_ignore);
      this->nakedPtr_gc_ignore = NULL;
      maybe_release<HolderType>::call(this->externalPtr_gc_ignore);
    }
  }

  static gctools::smart_ptr<WrapperType> create(OT *naked, class_id classId) {
    GC_ALLOCATE_VARIADIC(WrapperType, obj, naked, classId);
    core::Symbol_sp classSymbol = reg::lisp_classSymbol<OT>();
    obj->setInstanceClassUsingSymbol(classSymbol);
    return obj;
  }

  static gctools::smart_ptr<WrapperType> create(const OT &val, class_id classId) {
    OT *naked = new OT(val);
    GC_ALLOCATE_VARIADIC(WrapperType, obj, naked, classId);
    core::Symbol_sp classSymbol = reg::lisp_classSymbol<OT>();
    obj->setInstanceClassUsingSymbol(classSymbol);
    return obj;
  }

  static gctools::smart_ptr<WrapperType> create(std::unique_ptr<OT> val, class_id classId) {
    GC_ALLOCATE_VARIADIC(WrapperType, obj, std::move(val), classId);
    core::Symbol_sp classSymbol = reg::lisp_classSymbol<OT>();
    obj->setInstanceClassUsingSymbol(classSymbol);
    return obj;
  }

public:
  bool validp() const { return this->nakedPtr_gc_ignore != NULL; };
  void throwIfInvalid() const {
    if (!this->validp()) {
      SIMPLE_ERROR(BF("The wrapper is invalid"));
    }
  };

  /*! Release the pointer - invalidate the wrapper and return the pointer */
  virtual void *pointerRelease() {
    if (this->nakedPtr_gc_ignore != NULL) {
      void *ptr = const_cast<typename std::remove_const<OT>::type *>(this->nakedPtr_gc_ignore);
      this->nakedPtr_gc_ignore = NULL;
      maybe_release<HolderType>::call(this->externalPtr_gc_ignore);
      return ptr;
    }
    return NULL;
  }

  void initializeSlots(int numberOfSlots) {
    this->throwIfInvalid();
    clbind::support_initializeSlots<ExternalType>(numberOfSlots, this->nakedPtr_gc_ignore);
  }

  core::T_sp instanceSigSet() {
    this->throwIfInvalid();
    return clbind::support_instanceSigSet<ExternalType>(this->nakedPtr_gc_ignore);
  }

  core::T_sp instanceSig() const {
    this->throwIfInvalid();
    return clbind::support_instanceSig<ExternalType>(this->nakedPtr_gc_ignore);
  }

  core::T_sp instanceRef(int idx) const {
    this->throwIfInvalid();
    return clbind::support_instanceRef<ExternalType>(idx, this->nakedPtr_gc_ignore);
  }

  core::T_sp instanceSet(int idx, core::T_sp val) {
    this->throwIfInvalid();
    return clbind::support_instanceSet<ExternalType>(idx, val, this->nakedPtr_gc_ignore);
  }

  virtual void *castTo(class_id cid) const {
    this->throwIfInvalid();
    std::pair<void *, int> res = globalCastGraph->cast(const_cast<typename std::remove_const<OT>::type *>(this->nakedPtr_gc_ignore) // ptr
                                                       ,
                                                       this->_classId // src
                                                       ,
                                                       cid // target
                                                       ,
                                                       this->_classId // dynamic_id
                                                       ,
                                                       this->nakedPtr_gc_ignore // dynamic_ptr
                                                       );
    return res.first;
  }

  explicit Wrapper(){
      //            printf("\n%s:%d - explicit ctor for Wrapper@%p\n", __FILE__, __LINE__, this );
  };
  virtual ~Wrapper(){
      //            TRACE();
      //            printf("\n%s:%d - dtor for Wrapper@%p HolderType=%s OT*=%p adapter@%p cid=%lu  symbol=%s\n", __FILE__, __LINE__, this, typeid(HolderType).name(),this->nakedPtr_gc_ignore,clbind::support_adapterAddress<ExternalType>(this->nakedPtr_gc_ignore), this->classId, _rep_(reg::globalClassIdToClassSymbol[this->classId]).c_str() );

  };
};
};

template <typename T>
class gctools::GCKind<clbind::Wrapper<T, T *>> {
public:
  static gctools::GCKindEnum const Kind = gctools::GCKind<typename clbind::Wrapper<T, T *>::TemplatedBase>::Kind;
};
template <typename T>
struct gctools::GCInfo<clbind::Wrapper<T, T *>> {
  static bool constexpr NeedsInitialization = false;
  static bool constexpr NeedsFinalization = false;
  static GCInfo_policy constexpr Policy = normal;
};

/*! Wrappers of unique_ptr need to be finalized */
template <typename T>
class gctools::GCKind<clbind::Wrapper<T, std::unique_ptr<T>>> {
public:
  static gctools::GCKindEnum const Kind = gctools::GCKind<typename clbind::Wrapper<T, std::unique_ptr<T>>::TemplatedBase>::Kind;
};
template <typename T>
struct gctools::GCInfo<clbind::Wrapper<T, std::unique_ptr<T>>> {
  static bool constexpr NeedsInitialization = false;
  static bool constexpr NeedsFinalization = true;
  static GCInfo_policy constexpr Policy = normal;
};

namespace translate {

template <typename T>
struct debug_deleter {
  void operator()(T *p) {
    printf("%s:%d:%s Deleting object of type %s\n", __FILE__, __LINE__, __FUNCTION__, typeid(T).name());
    delete p;
  };
};

/*! Translate pointers that I adopt */
template <typename T>
class to_object<const std::unique_ptr<T> &, translate::adopt_pointer> {
public:
  typedef std::unique_ptr<const T /*,debug_deleter<T>*/> HolderType;
  typedef clbind::Wrapper<const T, HolderType> WrapperType;
  typedef WrapperType GivenType;
  static core::T_sp convert(const std::unique_ptr<T> ptr) {
    if (ptr == NULL) {
      return _Nil<core::T_O>();
    }
    gctools::smart_ptr<WrapperType> wrapper = WrapperType::create(ptr, reg::registered_class<T>::id);
    return wrapper;
  }
};

/*! Translate pointers that I adopt */
template <typename T>
class to_object<std::unique_ptr<T>, translate::adopt_pointer> {
public:
  typedef std::unique_ptr<T /*,debug_deleter<T>*/> HolderType;
  typedef clbind::Wrapper<T, HolderType> WrapperType;
  typedef WrapperType GivenType;
  static core::T_sp convert(std::unique_ptr<T> ptr) {
    if (ptr == NULL) {
      return _Nil<core::T_O>();
    }
    return WrapperType::create(std::move(ptr), reg::registered_class<T>::id);
  }
};

/*! Translate pointers that I adopt - ignore the dont_adopt_pointer
     because returning unique_ptr's are always adopted.
    This should never be invoked because I specialize clbind_functoids and clbind_methoids 
    on std::unique_ptr */
template <typename T>
class to_object<std::unique_ptr<T>, translate::dont_adopt_pointer> {
public:
  typedef std::unique_ptr<T /*,debug_deleter<T>*/> HolderType;
  typedef clbind::Wrapper<T, HolderType> WrapperType;
  typedef WrapperType GivenType;
  static core::T_sp convert(std::unique_ptr<T> ptr) {
    if (ptr == NULL) {
      return _Nil<core::T_O>();
    }
    return WrapperType::create(std::move(ptr), reg::registered_class<T>::id);
  }
};

/*! Translate pointers that I adopt */
template <typename T>
class to_object<const T *&, translate::adopt_pointer> {
public:
  typedef std::unique_ptr<const T /*,debug_deleter<T>*/> HolderType;
  typedef clbind::Wrapper<const T, HolderType> WrapperType;
  typedef WrapperType GivenType;
  static core::T_sp convert(const T *ptr) {
    if (ptr == NULL) {
      return _Nil<core::T_O>();
    }
    gctools::smart_ptr<WrapperType> wrapper = WrapperType::create(ptr, reg::registered_class<T>::id);
    return wrapper;
  }
};

/*! Translate pointers that I dont adopt */
template <typename T>
class to_object<const T *&, translate::dont_adopt_pointer> {
public:
  typedef clbind::Wrapper<const T> WrapperType;
  typedef WrapperType GivenType;
  static core::T_sp convert(const T *ptr) {
    if (ptr == NULL) {
      return _Nil<core::T_O>();
    }
    gctools::smart_ptr<WrapperType> wrapper = WrapperType::create(ptr, reg::registered_class<T>::id);
    return wrapper;
  }
};

/*! Translate pointers that I adopt */
template <typename T>
class to_object<T *, translate::adopt_pointer> {
public:
  typedef std::unique_ptr<T /*,debug_deleter<T>*/> HolderType;
  typedef clbind::Wrapper<T, HolderType> WrapperType;
  typedef WrapperType GivenType;
  static core::T_sp convert(T *ptr) {
    if (ptr == NULL) {
      return _Nil<core::T_O>();
    }
    gctools::smart_ptr<WrapperType> wrapper = WrapperType::create(ptr, reg::registered_class<T>::id);
    return wrapper;
  }
};

template <typename T>
class to_object<T *, translate::dont_adopt_pointer> {
public:
  typedef clbind::Wrapper<T, T *> WrapperType;
  typedef WrapperType GivenType;
  static core::T_sp convert(T *ptr) {
    if (ptr == NULL) {
      return _Nil<core::T_O>();
    }
    gctools::smart_ptr<WrapperType> wrapper = WrapperType::create(ptr, reg::registered_class<T>::id);
    return wrapper;
  }
};

/*! Translate pointers that I adopt */
template <typename T>
class to_object<T *const, translate::adopt_pointer> {
public:
  typedef std::unique_ptr<T /*,debug_deleter<T>*/> HolderType;
  typedef clbind::Wrapper<T, HolderType> WrapperType;
  typedef WrapperType GivenType;
  static core::T_sp convert(T *const ptr) {
    if (ptr == NULL) {
      return _Nil<core::T_O>();
    }
    gctools::smart_ptr<WrapperType> wrapper = WrapperType::create(ptr, reg::registered_class<T>::id);
    return wrapper;
  }
};

template <typename T>
class to_object<T *const, translate::dont_adopt_pointer> {
public:
  typedef clbind::Wrapper<T, T *const> WrapperType;
  static core::T_sp convert(T *const ptr) {
    if (ptr == NULL) {
      return _Nil<core::T_O>();
    }
    gctools::smart_ptr<WrapperType> wrapper = WrapperType::create(ptr, reg::registered_class<T>::id);
    return wrapper;
  }
};

template <typename T>
class to_object<T &, translate::dont_adopt_pointer> {
public:
  typedef clbind::Wrapper<T, T *> WrapperType;
  static core::T_sp convert(T &val) {
    gctools::smart_ptr<WrapperType> wrapper = WrapperType::create(&val, reg::registered_class<T>::id);
    return wrapper;
  }
};

/*! Translate return by value into wrapped pointers that I adopt */
template <typename T>
class to_object<T, translate::adopt_pointer> {
public:
  typedef std::unique_ptr<T> HolderType;
  typedef clbind::Wrapper<T, HolderType> WrapperType;
  typedef WrapperType GivenType;
  static core::T_sp convert(const T &val) {
    T *ptr = new T(val);
    gctools::smart_ptr<WrapperType> wrapper = WrapperType::create(ptr, reg::registered_class<T>::id);
    return wrapper;
  }
};

/*! Translate return by value into wrapped pointers that I adopt
      Ignore the translate::dont_adopt_pointer policy - we always adopt return by value values*/
template <typename T>
class to_object<T, translate::dont_adopt_pointer> {
public:
  typedef std::unique_ptr<T> HolderType;
  typedef clbind::Wrapper<T, HolderType> WrapperType;
  typedef WrapperType GivenType;
  static core::T_sp convert(const T &val) {
    T *ptr = new T(val);
    gctools::smart_ptr<WrapperType> wrapper = WrapperType::create(ptr, reg::registered_class<T>::id);
    return wrapper;
  }
};

/*! Translate return by value into wrapped pointers that I adopt */
template <typename T>
class to_object<const T, translate::adopt_pointer> {
public:
  typedef std::unique_ptr<T> HolderType;
  typedef clbind::Wrapper<T, HolderType> WrapperType;
  typedef WrapperType GivenType;
  static core::T_sp convert(const T &val) {
    T *ptr = new T(val);
    gctools::smart_ptr<WrapperType> wrapper = WrapperType::create(ptr, reg::registered_class<T>::id);
    return wrapper;
  }
};

/*! Translate return by value into wrapped pointers that I adopt
      Ignore the translate::dont_adopt_pointer policy - we always adopt return by value values*/
template <typename T>
class to_object<const T, translate::dont_adopt_pointer> {
public:
  typedef std::unique_ptr<T> HolderType;
  typedef clbind::Wrapper<T, HolderType> WrapperType;
  typedef WrapperType GivenType;
  static core::T_sp convert(const T &val) {
    IMPLEMENT_MEF(BF("This doesn't make sense - copy but don't adopt pointer???"));
    T *ptr = new T(val);
    gctools::smart_ptr<WrapperType> wrapper = WrapperType::create(ptr, reg::registered_class<T>::id);
    return wrapper;
  }
};

/*! from_object translators
     *
     *
     *
     *
     */

/*! This specialization presents a problem.
      How can xxx._v  where from_object<std::unique_ptr<T>> xxx(...) be passed
      to a function without using std::move(xxx._v)???????  
      The compiler should throw an error because the copy_constructor of unique_ptr has
      been deleted!   Has a function that consumes a std::unique_ptr not been wrapped yet
    by Clasp?    How does this work???
    */
template <typename T>
struct from_object<std::unique_ptr<T>> {
  typedef std::unique_ptr<T> DeclareType;
  DeclareType _v;
  from_object(core::T_sp o) {
    if (o.nilp()) {
      this->_v = std::unique_ptr<T>(static_cast<T *>(NULL));
      return;
    } else if (core::WrappedPointer_sp wp = o.asOrNull<core::WrappedPointer_O>()) {
      this->_v = std::unique_ptr<T>(/*gc::As<core::WrappedPointer_sp>(o)*/ wp->cast<T>());
      return;
    } else if (core::Pointer_sp pp = o.asOrNull<core::Pointer_O>()) {
      this->_v = std::unique_ptr<T>(static_cast<T *>(pp->ptr()));
      return;
    } else if (clbind::Derivable<T> *dp = dynamic_cast<clbind::Derivable<T> *>(&(*o))) {
      this->_v = std::unique_ptr<T>(dp->pointerToAlienWithin());
      return;
    }

#if 1
    printf("%s:%d  A problem was encountered while trying to convert the Common Lisp value: %s  into  a C++ object that can be passed to a C++ function/method\nWhat follows may or may not be useful for diagnosing the problem.\nYou may need to write a from_object translator for the destination type\n",
           __FILE__, __LINE__, _rep_(o).c_str());
    //            clbind::Derivable<T>* dtptr = dynamic_cast<clbind::Derivable<T>*>(o.px_ref());
    printf("%s:%d In from_object<T*>(core::T_sp o)\n", __FILE__, __LINE__);
    printf("dynamic_cast<clbind::Derivable<T>*>(o.px_ref()) = %p (SHOULD NOT BE NULL!!!)\n", dynamic_cast<clbind::Derivable<T> *>(o.raw_()));
    printf("o.px_ref() = %p\n", o.raw_());
    printf("typeid(T*)@%p  typeid(T*).name=%s\n", &typeid(T *), typeid(T *).name());
    printf("typeid(clbind::Derivable<T>*)@%p   typeid(clbind::Derivable<T>*).name() = %s\n", &typeid(clbind::Derivable<T> *), typeid(clbind::Derivable<T> *).name());
    printf("dynamic_cast<void*>(o.px_ref()) = %p\n", dynamic_cast<void *>(&(*o)));
    printf("Invoking o.px_ref()->describe(); /* A virtual function */\n");
    (*o).describe(core::lisp_true());
#endif
    SIMPLE_ERROR(BF("Could not convert %s of RTTI type %s to %s") % _rep_(o) % typeid(o).name() % typeid(T *).name());
  }
};

template <typename T>
struct from_object<T *> {
  typedef T *DeclareType;
  DeclareType _v;
  from_object(core::T_sp o) {
    if (o.nilp()) {
      this->_v = static_cast<T *>(NULL);
      return;
    } else if (core::WrappedPointer_sp wp = o.asOrNull<core::WrappedPointer_O>()) {
      this->_v = /*gc::As<core::WrappedPointer_sp>(o)*/ wp->cast<T>();
      return;
    } else if (core::Pointer_sp pp = o.asOrNull<core::Pointer_O>()) {
      this->_v = static_cast<T *>(pp->ptr());
      return;
    } else if (clbind::Derivable<T> *dp = dynamic_cast<clbind::Derivable<T> *>(&(*o))) {
      this->_v = dp->pointerToAlienWithin();
      return;
    }

#if 1
    printf("%s:%d  A problem was encountered while trying to convert the Common Lisp value: %s  into  a C++ object that can be passed to a C++ function/method\nWhat follows may or may not be useful for diagnosing the problem.\nYou may need to write a from_object translator for the destination type\n",
           __FILE__, __LINE__, _rep_(o).c_str());
    //            clbind::Derivable<T>* dtptr = dynamic_cast<clbind::Derivable<T>*>(o.px_ref());
    printf("%s:%d In from_object<T*>(core::T_sp o)\n", __FILE__, __LINE__);
    printf("dynamic_cast<clbind::Derivable<T>*>(o.px_ref()) = %p (SHOULD NOT BE NULL!!!)\n", dynamic_cast<clbind::Derivable<T> *>(&(*o)));
    printf("o.px_ref() = %p\n", o.raw_());
    printf("typeid(T*)@%p  typeid(T*).name=%s\n", &typeid(T *), typeid(T *).name());
    printf("typeid(clbind::Derivable<T>*)@%p   typeid(clbind::Derivable<T>*).name() = %s\n", &typeid(clbind::Derivable<T> *), typeid(clbind::Derivable<T> *).name());
    printf("dynamic_cast<void*>(o.px_ref()) = %p\n", dynamic_cast<void *>(&(*o)));
    printf("Invoking o.px_ref()->describe(); /* A virtual function */\n");
    (*o).describe(core::lisp_true());
#endif
    SIMPLE_ERROR(BF("Could not convert %s of RTTI type %s to %s") % _rep_(o) % typeid(o).name() % typeid(T *).name());
  }
};

template <typename T>
struct from_object<const T *&> {
  typedef const T *DeclareType;
  DeclareType _v;
  from_object(core::T_sp o) {
    if (o.nilp()) {
      this->_v = static_cast<T *>(NULL);
      return;
    } else if (core::WrappedPointer_sp wp = o.asOrNull<core::WrappedPointer_O>()) {
      this->_v = /*gc::As<core::WrappedPointer_sp>(o)*/ wp->cast<T>();
      return;
    } else if (core::Pointer_sp pp = o.asOrNull<core::Pointer_O>()) {
      this->_v = static_cast<T *>(pp->ptr());
      return;
    }
    SIMPLE_ERROR(BF("Could not convert %s of RTTI type %s to %s") % _rep_(o) % typeid(o).name() % typeid(T *&).name());
  }
};

/*! If the argument is a pure-out-value then don't use the passed to initialize _v */
template <typename T>
struct from_object<const T *&, std::false_type> {
  typedef const T *DeclareType;
  DeclareType _v;
  from_object(const core::T_sp &o) : _v(NULL){};
};

template <typename T>
struct from_object<const T &> {
  typedef const T &DeclareType;
  DeclareType _v;
  from_object(core::T_sp o) : _v(*(from_object<T *>(o)._v)){};
};

template <typename T>
struct from_object<T &> {
  typedef T &DeclareType;
  DeclareType _v;
  from_object(core::T_sp o) : _v(*(from_object<T *>(o)._v)){};
};

template <typename T>
struct from_object<T> {
  typedef T DeclareType;
  DeclareType _v;
  from_object(core::T_sp o) : _v(*(from_object<T *>(o)._v)){};
};
};

#endif
