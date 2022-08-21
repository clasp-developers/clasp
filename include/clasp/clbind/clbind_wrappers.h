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

//#include <llvm/ADT/Optional.h>
#include <clasp/core/wrappedPointer.h>
#include <clasp/core/instance.h>
#include <clasp/clbind/adapter.fwd.h>
#include <clasp/clbind/derivable.h>
#include <clasp/clbind/inheritance.h>

namespace core {
bool maybe_demangle(const std::string& fnName, std::string& output);
};

namespace clbind {

template <class OT, class WT>
gctools::smart_ptr<OT> RP_Create_wrapper() {
  _G();
  auto  wrapper = gctools::GC<OT>::allocate_with_default_constructor();
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
      The wrapper does not own the pointer unless the HolderType 
      is a std::unique_ptr or std::shared_ptr some other holder that takes care of ownership
 */
template <class OT, class HolderType = OT*>
class Wrapper : public core::WrappedPointer_O {
public:
  typedef core::WrappedPointer_O TemplatedBase;

public:
  typedef Wrapper<OT, HolderType> WrapperType;
  typedef OT ExternalType;

public: // Do NOT declare any smart_ptr's or weak_smart_ptr's here!!!!
  HolderType p_gc_ignore;
  void*      weak;
  class_id   dynamic_id;
  void*      dynamic_ptr;

 public:
//
// Get a raw pointer from whatever HolderType we have
//
template <typename HType>
struct RawGetter {
  static HType get_pointer(HType& ptr) { return ptr; };
  static const HType get_pointer(const HType& ptr) { return ptr; };
};

template <typename PtrType>
struct RawGetter<std::unique_ptr<PtrType>> {
  static PtrType* get_pointer(std::unique_ptr<PtrType>& ptr) { return ptr.get(); };
  static const PtrType* get_pointer(const std::unique_ptr<PtrType>& ptr) { return ptr.get(); };
 };

template <typename PtrType>
struct RawGetter<std::shared_ptr<PtrType>> {
  static PtrType* get_pointer(std::shared_ptr<PtrType>& ptr) { return ptr.get(); };
  static const PtrType* get_pointer(const std::shared_ptr<PtrType>& ptr) { return ptr.get(); };
 };

public:
  void do_checks() {
#if 0
    OT* rawPtr = RawGetter<HolderType>::get_pointer(this->p_gc_ignore);
    void* basePtr = dynamic_cast<void*>(rawPtr);
    if (basePtr==NULL) {
      printf("%s:%d:%s The basePtr from dynamic_cast<void*>(%p) was NULL!\n", __FILE__, __LINE__, __FUNCTION__, rawPtr );
    }
    if (basePtr!=(void*)rawPtr) {
      printf("%s:%d:%s The basePtr %p from dynamic_cast<void*>(%p) was different!\n", __FILE__, __LINE__, __FUNCTION__, basePtr, rawPtr );
    }
#endif
  }
      
  Wrapper(OT *naked, class_id dynamic_id, void* dynamic_ptr)
      : p_gc_ignore(naked)
      , weak(0)
      , dynamic_id(dynamic_id)
      , dynamic_ptr(dynamic_ptr)
  {
//    printf("%s:%d:%s naked ctor OT\n", __FILE__, __LINE__, __FUNCTION__ );
    this->do_checks();
  };

  // ctor that takes a unique_ptr
  Wrapper(std::unique_ptr<OT> naked, class_id dynamic_id, void* dynamic_ptr)
      : p_gc_ignore(std::move(naked))
      , weak(0)
      , dynamic_id(dynamic_id)
      , dynamic_ptr(dynamic_ptr)
  {
//    printf("%s:%d:%s unique_ptr ctor OT\n", __FILE__, __LINE__, __FUNCTION__ );
    this->do_checks();
  };

size_t templatedSizeof() const { return sizeof(*this); };
void* mostDerivedPointer() const { return (void*)RawGetter<HolderType>::get_pointer(this->p_gc_ignore); };

  virtual class_id classId() const { return this->dynamic_id; };

  /*! Release the pointer - invalidate the wrapper and return the pointer */
  virtual void pointerDelete() {
    if (RawGetter<HolderType>::get_pointer(this->p_gc_ignore) != NULL) {
      maybe_delete<OT, is_deletable<OT>::value>::doit(RawGetter<HolderType>::get_pointer(this->p_gc_ignore));
      maybe_release<HolderType>::call(this->p_gc_ignore);
    }
  }

  static gctools::smart_ptr<WrapperType> make_wrapper(OT *naked, class_id dynamic_id) {
//    printf("%s:%d:%s DEBUG_WRAPPER with OT*\n", __FILE__, __LINE__, __FUNCTION__ );
    void* dynamic_ptr = (void*)naked;
    auto  obj = gctools::GC<WrapperType>::allocate( naked, dynamic_id, dynamic_ptr );
    core::Symbol_sp classSymbol = reg::lisp_classSymbol<OT>();
    if (!classSymbol.unboundp()) {
      obj->_setInstanceClassUsingSymbol(classSymbol);
      return obj;
    }
    SIMPLE_ERROR("In make_wrapper for a class class_id %d\n"
                 "  the classSymbol could not be identified\n"
                 "  this probably means that you are trying to wrap an unexposed class\n"
                 "  OR you don't have an appropriate to_object translator for the type\n"
                 "  that corresponds to this class_id.\n"
                 "  Connect a debugger and check if:\n"
                 "  struct to_object<T &, translate::dont_adopt_pointer> is on the stack",  dynamic_id );
  }

  static gctools::smart_ptr<WrapperType> make_wrapper(const OT &val, class_id dynamic_id ) {
    printf("%s:%d:%s with OT&\n", __FILE__, __LINE__, __FUNCTION__ );
    OT *naked = new OT(val);
    void* dynamic_ptr = (void*)naked;
    auto  obj = gctools::GC<WrapperType>::allocate( naked, dynamic_id, dynamic_ptr );
    core::Symbol_sp classSymbol = reg::lisp_classSymbol<OT>();
    obj->_setInstanceClassUsingSymbol(classSymbol);
    return obj;
  }

  static gctools::smart_ptr<WrapperType> make_wrapper(std::unique_ptr<OT> val, class_id dynamic_id) {
//    printf("%s:%d:%s with unique_ptr\n", __FILE__, __LINE__, __FUNCTION__ );
    void* dynamic_ptr = (void*)val.get();
    auto  obj = gctools::GC<WrapperType>::allocate( std::move(val), dynamic_id, dynamic_ptr );
    core::Symbol_sp classSymbol = reg::lisp_classSymbol<OT>();
    obj->_setInstanceClassUsingSymbol(classSymbol);
    return obj;
  }

public:
bool validp() const { return RawGetter<HolderType>::get_pointer(this->p_gc_ignore) != NULL; };
  void throwIfInvalid() const {
    if (!this->validp()) {
      SIMPLE_ERROR_SPRINTF("The wrapper is invalid");
    }
  };

  /*! Release the pointer - invalidate the wrapper and return the pointer */
  virtual void *pointerRelease() {
    if (RawGetter<HolderType>::get_pointer(this->p_gc_ignore) != NULL) {
      void *ptr = const_cast<typename std::remove_const<OT>::type *>(RawGetter<HolderType>::get_pointer(this->p_gc_ignore));
      maybe_release<HolderType>::call(this->p_gc_ignore);
      return ptr;
    }
    return NULL;
  }

  void initializeSlots(int numberOfSlots) {
    this->throwIfInvalid();
    clbind::support_initializeSlots<ExternalType>(numberOfSlots, RawGetter<HolderType>::get_pointer(this->p_gc_ignore)); 
  }

  core::T_sp instanceSigSet() {
    this->throwIfInvalid();
    return clbind::support_instanceSigSet<ExternalType>(RawGetter<HolderType>::get_pointer(this->p_gc_ignore));
  }

  core::T_sp instanceSig() const {
    this->throwIfInvalid();
    return clbind::support_instanceSig<ExternalType>(RawGetter<HolderType>::get_pointer(this->p_gc_ignore));
  }

  core::T_sp instanceRef(size_t idx) const {
    this->throwIfInvalid();
    return clbind::support_instanceRef<ExternalType>(idx, RawGetter<HolderType>::get_pointer(this->p_gc_ignore));
  }

  core::T_sp instanceSet(size_t idx, core::T_sp val) {
    this->throwIfInvalid();
    return clbind::support_instanceSet<ExternalType>(idx, val, RawGetter<HolderType>::get_pointer(this->p_gc_ignore));
  }

  virtual void *castTo(class_id cid) const {
    this->throwIfInvalid();
    std::pair<void *, int> res = globalCastGraph->cast(const_cast<typename std::remove_const<OT>::type *>(RawGetter<HolderType>::get_pointer(
                                                                                                              this->p_gc_ignore)) // ptr
                                                       , reg::registered_class<OT>::id // src
                                                       , cid // target
                                                       , this->dynamic_id
                                                       , this->dynamic_ptr
                                                       );
    return res.first;
  }

  explicit Wrapper(){
    printf("\n%s:%d - explicit ctor for Wrapper@%p\n", __FILE__, __LINE__, this );
  };
  virtual ~Wrapper(){
      //            TRACE();
#if 0
    printf("\n%s:%d - DEBUG_WRAPPER dtor for Wrapper@%p HolderType=%s OT*=%p adapter@%p cid=%lu  symbol=%s\n", __FILE__, __LINE__,
           this,
           typeid(HolderType).name(),
           RawGetter<HolderType>::get_pointer(this->p_gc_ignore),
           clbind::support_adapterAddress<ExternalType>(RawGetter<HolderType>::get_pointer(this->p_gc_ignore)),
           this->classId(),
           _rep_(reg::lisp_classSymbolFromClassId(this->classId())).c_str() );
#endif
  };
};
};

template <typename T>
struct gctools::GCInfo<clbind::Wrapper<T, T *>> {
  static bool constexpr NeedsInitialization = false;
  static bool constexpr NeedsFinalization = false;
  static GCInfo_policy constexpr Policy = normal;
};

template <typename T>
class gctools::GCStamp<clbind::Wrapper<T, T *>> {
public:
  static gctools::GCStampEnum const StampWtag = gctools::GCStamp<typename clbind::Wrapper<T, T *>::TemplatedBase>::StampWtag;
};

template <typename T>
struct gctools::GCInfo<clbind::Wrapper<T, std::unique_ptr<T>>> {
  static bool constexpr NeedsInitialization = false;
  static bool constexpr NeedsFinalization = true;
  static GCInfo_policy constexpr Policy = normal;
};


/*! Wrappers of unique_ptr need to be finalized */
template <typename T>
class gctools::GCStamp<clbind::Wrapper<T, std::unique_ptr<T>>> {
public:
  static gctools::GCStampEnum const StampWtag = gctools::GCStamp<typename clbind::Wrapper<T, std::unique_ptr<T>>::TemplatedBase>::StampWtag;
};

template <typename T>
struct gctools::GCInfo<clbind::Wrapper<T, std::shared_ptr<T>>> {
  static bool constexpr NeedsInitialization = false;
  static bool constexpr NeedsFinalization = true;
  static GCInfo_policy constexpr Policy = normal;
};

/*! Wrappers of shared_ptr need to be finalized */
template <typename T>
class gctools::GCStamp<clbind::Wrapper<T, std::shared_ptr<T>>> {
public:
  static gctools::GCStampEnum const StampWtag = gctools::GCStamp<typename clbind::Wrapper<T, std::shared_ptr<T>>::TemplatedBase>::StampWtag;
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
struct to_object<const std::unique_ptr<T> &, translate::adopt_pointer> {
public:
  typedef std::unique_ptr<const T /*,debug_deleter<T>*/> HolderType;
  typedef clbind::Wrapper<const T, HolderType> WrapperType;
  static core::T_sp convert(const std::unique_ptr<T> ptr) {
    if (ptr == NULL) {
      return nil<core::T_O>();
    }
    gctools::smart_ptr<WrapperType> wrapper = WrapperType::make_wrapper(ptr, reg::registered_class<T>::id);
    return wrapper;
  }
};

/*! Translate pointers that I adopt */
template <typename T>
struct to_object<std::unique_ptr<T>, translate::adopt_pointer> {
public:
  typedef std::unique_ptr<T /*,debug_deleter<T>*/> HolderType;
  typedef clbind::Wrapper<T, HolderType> WrapperType;
  static core::T_sp convert(std::unique_ptr<T>& ptr) {
    if (ptr == NULL) {
      return nil<core::T_O>();
    }
    return WrapperType::make_wrapper(std::move(ptr), reg::registered_class<T>::id);
  }
};

/*! Translate pointers that I adopt - ignore the dont_adopt_pointer
     because returning unique_ptr's are always adopted.
    This should never be invoked because I specialize clbind_functoids and clbind_methoids 
    on std::unique_ptr */
template <typename T>
struct to_object<std::unique_ptr<T>, translate::dont_adopt_pointer> {
public:
  typedef std::unique_ptr<T /*,debug_deleter<T>*/> HolderType;
  typedef clbind::Wrapper<T, HolderType> WrapperType;
  static core::T_sp convert(std::unique_ptr<T>& ptr) {
    if (ptr == NULL) {
      return nil<core::T_O>();
    }
    return WrapperType::make_wrapper(std::move(ptr), reg::registered_class<T>::id);
  }
};

/*! Translate pointers that I adopt */
template <typename T>
struct to_object<const T *&, translate::adopt_pointer> {
public:
  typedef std::unique_ptr<const T /*,debug_deleter<T>*/> HolderType;
  typedef clbind::Wrapper<const T, HolderType> WrapperType;
  static core::T_sp convert(const T *ptr) {
    if (ptr == NULL) {
      return nil<core::T_O>();
    }
    gctools::smart_ptr<WrapperType> wrapper = WrapperType::make_wrapper(ptr, reg::registered_class<T>::id);
    return wrapper;
  }
};

/*! Translate pointers that I dont adopt */
template <typename T>
struct to_object<const T *&, translate::dont_adopt_pointer> {
public:
  typedef clbind::Wrapper<const T, const T*> WrapperType;
  static core::T_sp convert(const T *ptr) {
    if (ptr == NULL) {
      return nil<core::T_O>();
    }
    gctools::smart_ptr<WrapperType> wrapper = WrapperType::make_wrapper(ptr, reg::registered_class<T>::id);
    return wrapper;
  }
};

/*! Translate pointers that I adopt */
template <typename T>
struct to_object<T *, translate::adopt_pointer> {
public:
  typedef std::unique_ptr<T /*,debug_deleter<T>*/> HolderType;
  typedef clbind::Wrapper<T, HolderType> WrapperType;
  static core::T_sp convert(T *ptr) {
    if (ptr == NULL) {
      return nil<core::T_O>();
    }
    gctools::smart_ptr<WrapperType> wrapper = WrapperType::make_wrapper(ptr, reg::registered_class<T>::id);
    return wrapper;
  }
};

template <typename T>
struct to_object<T *, translate::dont_adopt_pointer> {
public:
  typedef clbind::Wrapper<T, T *> WrapperType;
  static core::T_sp convert(T *ptr) {
    if (ptr == NULL) {
      return nil<core::T_O>();
    }
    gctools::smart_ptr<WrapperType> wrapper = WrapperType::make_wrapper(ptr, reg::registered_class<T>::id);
    return wrapper;
  }
};

/*! Translate pointers that I adopt */
template <typename T>
struct to_object<T *const, translate::adopt_pointer> {
public:
  typedef std::unique_ptr<T /*,debug_deleter<T>*/> HolderType;
  typedef clbind::Wrapper<T, HolderType> WrapperType;
  static core::T_sp convert(T *const ptr) {
    if (ptr == NULL) {
      return nil<core::T_O>();
    }
    gctools::smart_ptr<WrapperType> wrapper = WrapperType::make_wrapper(ptr, reg::registered_class<T>::id);
    return wrapper;
  }
};

template <typename T>
struct to_object<T *const, translate::dont_adopt_pointer> {
public:
  typedef clbind::Wrapper<T, T *const> WrapperType;
  static core::T_sp convert(T *const ptr) {
    if (ptr == NULL) {
      return nil<core::T_O>();
    }
    gctools::smart_ptr<WrapperType> wrapper = WrapperType::make_wrapper(ptr, reg::registered_class<T>::id);
    return wrapper;
  }
};

template <typename T>
struct to_object<T &, translate::dont_adopt_pointer> {
public:
  typedef clbind::Wrapper<T, T *> WrapperType;
  static core::T_sp convert(T &val) {
    gctools::smart_ptr<WrapperType> wrapper = WrapperType::make_wrapper(&val, reg::registered_class<T>::id);
    return wrapper;
  }
};

/*! Translate return by value into wrapped pointers that I adopt */
template <typename T>
struct to_object<T, translate::adopt_pointer> {
public:
  typedef std::unique_ptr<T> HolderType;
  typedef clbind::Wrapper<T, HolderType> WrapperType;
  static core::T_sp convert(const T &val) {
    T *ptr = new T(val);
    gctools::smart_ptr<WrapperType> wrapper = WrapperType::make_wrapper(ptr, reg::registered_class<T>::id);
    return wrapper;
  }
};

/*! Translate return by value into wrapped pointers that I adopt
      Ignore the translate::dont_adopt_pointer policy - we always adopt return by value values*/
template <typename T>
struct to_object<T, translate::dont_adopt_pointer> {
public:
  typedef std::unique_ptr<T> HolderType;
  typedef clbind::Wrapper<T, HolderType> WrapperType;
  static core::T_sp convert(const T &val) {
    T *ptr = new T(val);
    gctools::smart_ptr<WrapperType> wrapper = WrapperType::make_wrapper(ptr, reg::registered_class<T>::id);
    return wrapper;
  }
};

/*! Translate return by value into wrapped pointers that I adopt */
template <typename T>
struct to_object<const T, translate::adopt_pointer> {
public:
  typedef std::unique_ptr<T> HolderType;
  typedef clbind::Wrapper<T, HolderType> WrapperType;
  static core::T_sp convert(const T &val) {
    T *ptr = new T(val);
    gctools::smart_ptr<WrapperType> wrapper = WrapperType::make_wrapper(ptr, reg::registered_class<T>::id);
    return wrapper;
  }
};

/*! Translate return by value into wrapped pointers that I adopt
      Ignore the translate::dont_adopt_pointer policy - we always adopt return by value values*/
template <typename T>
struct to_object<const T, translate::dont_adopt_pointer> {
public:
  typedef std::unique_ptr<T> HolderType;
  typedef clbind::Wrapper<T, HolderType> WrapperType;
  static core::T_sp convert(const T &val) {
    HARD_IMPLEMENT_MEF("This doesn't make sense - copy but don't adopt pointer???");
    T *ptr = new T(val);
    gctools::smart_ptr<WrapperType> wrapper = WrapperType::make_wrapper(ptr, reg::registered_class<T>::id);
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
    static_assert(!std::is_pod<T>::value, "T must NOT be POD");
    if (o.nilp()) {
      this->_v = std::unique_ptr<T>(static_cast<T *>(NULL));
      return;
    } else if (core::WrappedPointer_sp wp = o.asOrNull<core::WrappedPointer_O>()) {
      this->_v = std::unique_ptr<T>(/*gc::As<core::WrappedPointer_sp>(o)*/ wp->cast<T>());
      return;
    } else if (core::Pointer_sp pp = o.asOrNull<core::Pointer_O>()) {
      this->_v = std::unique_ptr<T>(static_cast<T *>(pp->ptr()));
      return;
    } else if ( o.generalp() ) {
      core::General_O* gp = (core::General_O*)&(*o);
      T* v_alien = reinterpret_cast<T*>(gp->pointerToAlienWithin());
      if (!v_alien) {
        SIMPLE_ERROR_SPRINTF("Wrong type of argument - clbind object@%p of type: %s", (void*)gp, typeid(T).name());
      }

      ASSERT(v_alien);
      this->_v = std::unique_ptr<T>(v_alien);
      return;
    }
    printf("%s:%d  A problem was encountered while trying to convert the Common Lisp value: %s  into  a C++ object that can be passed to a C++ function/method\nWhat follows may or may not be useful for diagnosing the problem.\nYou may need to write a from_object translator for the destination type\n",
           __FILE__, __LINE__, _rep_(o).c_str());
    //            clbind::Derivable<T>* dtptr = dynamic_cast<clbind::Derivable<T>*>(o.px_ref());
    printf("%s:%d In from_object<T*>(core::T_sp o)\n", __FILE__, __LINE__);
    if ( o.generalp() ) {
      core::General_sp go(o.unsafe_general());
      printf("dynamic_cast<clbind::Derivable<T>*>(o.px_ref()) = %p (SHOULD NOT BE NULL!!!)\n", dynamic_cast<clbind::Derivable<T> *>(&(*go)));
      printf("o.px_ref() = %p\n", go.raw_());
      printf("typeid(T*)@%p  typeid(T*).name=%s\n", &typeid(T *), typeid(T *).name());
      printf("typeid(clbind::Derivable<T>*)@%p   typeid(clbind::Derivable<T>*).name() = %s\n", &typeid(clbind::Derivable<T> *), typeid(clbind::Derivable<T> *).name());
      SIMPLE_ERROR_SPRINTF("Could not convert %s of RTTI type %s to %s", _rep_(go).c_str(), typeid(o).name(), typeid(T*).name());
    } else {
      printf("%s:%d Can't handle object\n", __FILE__, __LINE__ );
    }
  }
};



template <typename T>
struct from_object<T *> {
  typedef T *DeclareType;
  DeclareType _v;
  from_object(core::T_sp o) {
    // static_assert(!std::is_pod<T>::value, "T must NOT be POD");
//    int*** i = T(); 
    if (o.nilp()) {
      this->_v = static_cast<T *>(NULL);
      return;
    } else if (core::WrappedPointer_sp wp = o.asOrNull<core::WrappedPointer_O>()) {
      this->_v = /*gc::As<core::WrappedPointer_sp>(o)*/ wp->cast<T>();
      return;
    } else if (core::Pointer_sp pp = o.asOrNull<core::Pointer_O>()) {
      this->_v = static_cast<T *>(pp->ptr());
      return;
    } else if (o.generalp()) {
      core::General_O* gp = o.unsafe_general();
      T* v_alien = reinterpret_cast<T*>(gp->pointerToAlienWithin());
      if (!v_alien) {
        std::string expectedType(typeid(T).name());
        std::string demangled;
        bool success = core::maybe_demangle(expectedType,demangled);
        if (success) {
          SIMPLE_ERROR(("Incorrect object %s passed to function - expected type %s") , _rep_(o).c_str() , demangled);
        } else {
          SIMPLE_ERROR(("Incorrect object %s passed to function - expected type %s") , _rep_(o).c_str() , typeid(T).name());
        }
      }
      ASSERT(v_alien);
      this->_v = v_alien;
      return;
    }
    printf("%s:%d  A problem was encountered while trying to convert the Common Lisp value: %s\n"
           "into  a C++ object that can be passed to a C++ function/method\n"
           "You need to write a from_object translator for the destination type\n",
           __FILE__, __LINE__, _rep_(o).c_str());
    printf("%s:%d In from_object<T*>(core::T_sp o)\n", __FILE__, __LINE__);
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
    } else if ( core::General_sp gp = o.asOrNull<core::General_O>() ) {
      // What do I do here?
    }
    SIMPLE_ERROR_SPRINTF("Could not convert %s of RTTI type %s to %s",  _rep_(o).c_str(), typeid(o).name(), typeid(T *&).name());
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
T& safe_deref(T* ptr) {
  if (ptr) {
    return *ptr;
  }
  SIMPLE_ERROR(("Passing NULL to a from_object that dereferences - this probably means that NIL was passed to a clbind wrapped function that expects a real object"));
}

template <typename T>
struct from_object<T &> {
  typedef T &DeclareType;
  DeclareType _v;
  from_object(core::T_sp o) : _v(safe_deref<T>((from_object<T *>(o)._v))){};
  ~from_object() {/*non trivial*/};
};

template <typename T>
struct from_object<T &, std::false_type> {
  typedef T &DeclareType;
  T _v;
  from_object(core::T_sp o) {};
  ~from_object() {/*non trivial*/};
};

template <typename T>
struct from_object<T> {
  typedef T DeclareType;
  DeclareType _v;
  from_object(core::T_sp o) : _v(*(from_object<T *>(o)._v)) {
    /*!!!!!!!! Did a EXC_BAD_ACCESS happen here???
      !!!!!!!! If it did - maybe this isn't the right from_object translator
      !!!!!!!! and you need to implement a more specialized one.
      !!!!!!!! And you need to make sure it's visible when the function that
      !!!!!!!! uses it is exposed.
      !!!!!!!! See from_object<llvm::DIFile::ChecksumKind,std::true_type> 
      !!!!!!!! for an example where a specific translator was implemented
    */
  };
};
};

////////////////////////////////////////////////////////////
////////////////////////////////////////////////////////////
////////////////////////////////////////////////////////////
////////////////////////////////////////////////////////////
//
//
// Type specific reference translators
//
//


namespace translate {

template <>
struct from_object<int&,std::true_type> {
  typedef int DeclareType;
  int _v;
  from_object(gctools::smart_ptr<core::T_O> vv) : _v(core::clasp_to_int(vv)) {};
  ~from_object() { /* Non-trivial */ };
};

template <>
struct from_object<int&,std::false_type> {
  typedef int DeclareType;
  int _v;
  from_object(gctools::smart_ptr<core::T_O> vv) {
    (void)vv;
  };
  ~from_object() {
    // non-trivial dtor to keep _v around
  };
};

  template <>
struct from_object<int*,std::false_type> {
  typedef int DeclareType;
  int _v;
  from_object(gctools::smart_ptr<core::T_O> vv) {
    (void)vv;
  };
  ~from_object() {
    // non-trivial dtor to keep _v around
  };
};


template <>
struct from_object<const char*, std::true_type> {
  typedef const char* DeclareType;
  mutable char* _v;
  from_object(gctools::smart_ptr<core::T_O> vv) {
    core::String_sp strng = gc::As<core::String_sp>(vv);
    size_t len = core::cl__length(strng);
    this->_v = (char*)malloc(len+1);
    strncpy(this->_v,strng->get_std_string().data(),len);
    this->_v[len] = '\0';
  }
  from_object(const from_object<const char*,std::true_type>& other) {
    this->_v = other._v;
    other._v = NULL;
  }
  ~from_object() {
    if (this->_v) {
      free(this->_v);
      this->_v = NULL;
    }
  };
};

};
#endif
