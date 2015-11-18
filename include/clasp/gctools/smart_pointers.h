// Disable this once we have List_sp working
#define USE_BAD_CAST_ERROR 1
//Turn these on only if x.nilp() are found in ASSERT(...) statements
#define ALLOW_NIL_OTHER 1

/*
  File: smart_pointers.h
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

//
// (C) 2004 Christian E. Schafmeister
//

#ifndef _core_smart_pointers_H
#define _core_smart_pointers_H

#include <boost/utility/binary.hpp>

#include <iostream>
#include <cstring>
//#include "tagged_ptr.h"
//#define TAGGED_PTR_BASE tagged_ptr

//#define	IsUndefined(x) (x)
//#define	NotUndefined(x) (!(x))

//#define	_FWPLock(x)	(x)

//#define	TAGGED_PTR core::T_O*

namespace gctools {
template <class T>
class tagged_ptr {
public:
  typedef T Type;
  Type *theObject;

public:
  //Default constructor, set theObject to NULL
  inline tagged_ptr() : theObject(NULL){};
  //    	explicit smart_ptr(uintptr_t p) : theObject(p) {}; // TODO: this converts ints to smart_ptr's - its dangerous
  //! Construct a FRAME object - I need to get rid of these
  //smart_ptr( core::T_O** p ) : theObject(tag_frame(p)) { /*printf("%s:%d Creating Frame \n", __FILE__, __LINE__ );*/ };
  //smart_ptr( Type* objP) : theObject(tag_object(objP)) {};
  // explicit smart_ptr( void* objP) : theObject(reinterpret_cast<Type*>(objP)) {};

  /*! Create a smart pointer from an existing tagged pointer */
  explicit inline tagged_ptr(Tagged ptr) : theObject(reinterpret_cast<Type *>(ptr)){};

  explicit inline tagged_ptr(Type *ptr) : theObject(ptr ? tag_general<Type *>(ptr) : NULL) {
    GCTOOLS_ASSERT((reinterpret_cast<uintptr_t>(ptr) & tag_mask) == 0);
  };

  inline tagged_ptr(const tagged_ptr<Type> &obj) : theObject(obj.theObject){};

  template <class From>
  inline tagged_ptr(tagged_ptr<From> const &rhs) : theObject(rhs.raw_()){};

public:
  uintptr_t tag() const { return reinterpret_cast<uintptr_t>(this->theObject) & tag_mask; };

  /*! Get the pointer typcast to an integer quantity for hashing */
  cl_intptr_t intptr() const { return ((uintptr_t)(this->theObject)); };

  void reset_() { this->theObject = NULL; };

  inline void swap(smart_ptr<Type> &other) {
    Type *temp;
    temp = this->theObject;
    this->theObject = other.theObject;
    other.theObject = temp;
  }

  template <class o_class>
  inline smart_ptr<o_class> asOrNull() {
    return smart_ptr<o_class>((Tagged)TaggedCast<o_class *, Type *>::castOrNULL(this->theObject));
  }

  template <class o_class>
  inline smart_ptr<o_class> asOrNull() const {
    return smart_ptr<o_class>((Tagged)TaggedCast<o_class *, Type *>::castOrNULL(this->theObject));
  }

  /*! Return the offset in bytes between this.px and this - you need to modify the base
	  class of smart_ptr to make px protected */
  //	int offset_of_px_from_this() const { return ((char*)(&this->px)) - ((char*)(this));}
  /*! Return the size in bytes of px - you need to modify the base class
	  of smart_ptr to make px protected */
  //	int size_of_px() const { return sizeof(this->px); };

  int number_of_values() const { return this->theObject == NULL ? 0 : 1; };

  inline Type *untag_object() const {
    GCTOOLS_ASSERT(this->generalp() || this->consp());
    if (this->generalp()) {
      return untag_general<Type *>(this->theObject);
    } else if (this->consp()) {
      return untag_cons<Type *>(this->theObject);
    }
    lisp_errorDereferencedNonPointer(this->theObject);
    HARD_UNREACHABLE();
  }

  /*! Dereferencing operator - remove the other tag */
  inline Type *operator->() {
    GCTOOLS_ASSERT(this->theObject);
    return this->untag_object();
  };
  inline Type *operator->() const {
    GCTOOLS_ASSERT(this->theObject);
    return this->untag_object();
  };
  inline Type &operator*() const {
    GCTOOLS_ASSERT(this->theObject);
    return *this->untag_object();
  };

  Type *get() const { return this->untag_object(); };
  bool _NULLp() const { return this->theObject == NULL; };

  /*! If theObject!=NULL then return true */
  explicit operator bool() const { return this->theObject != NULL; };

  bool nilp() const { return tagged_nilp(this->theObject); }
  bool notnilp() const { return (!this->nilp()); };
  bool isTrue() const { return !this->nilp(); };
  bool fixnump() const { return tagged_fixnump(this->theObject); };
  Fixnum unsafe_fixnum() const { return untag_fixnum(this->theObject); };
  bool unboundp() const { return tagged_unboundp(this->theObject); };
  bool deletedp() const { return tagged_deletedp(this->theObject); };
  bool sameAsKeyp() const { return tagged_sameAsKeyp(this->theObject); };
  bool characterp() const { return tagged_characterp<Type *>(this->theObject); };
  int unsafe_character() const { return untag_character(this->theObject); };
  bool single_floatp() const { return tagged_single_floatp<Type *>(this->theObject); };
  float unsafe_single_float() const { return untag_single_float<Type *>(this->theObject); };
  // This replaces pointerp()
  bool objectp() const { return this->generalp() || this->consp(); };
  bool generalp() const { return tagged_generalp<Type *>(this->theObject); };
  bool consp() const { return tagged_consp<Type *>(this->theObject); };
  core::Cons_O *unsafe_cons() const {
    GCTOOLS_ASSERT(this->consp());
    return reinterpret_cast<core::Cons_O *>(reinterpret_cast<uintptr_t>(this->theObject) - cons_tag);
  };

  Fixnum asFixnum() const {
    GCTOOLS_ASSERT(this->fixnump());
    return untag_fixnum<Type *>(this->theObject);
  };

  bool valistp() const { return tagged_valistp(this->theObject); };
  void *unsafe_valist() const { return untag_valist(this->theObject); };
  void *safe_valist() const {
    GCTOOLS_ASSERT(this->valistp());
    return this->unsafe_valist();
  };

  bool sameAsKeyP() const { return tagged_sameAsKeyp(this->theObject); }

  /*! Return the raw smart_ptr value interpreted as a T_O* */
  inline core::T_O *raw_() const { return reinterpret_cast<core::T_O *>(this->theObject); }

  inline void setRaw_(Tagged p) { this->theObject = reinterpret_cast<core::T_O *>(p); }

  /*! This should almost NEVER be used!!!!!!   

	  List all uses of rawRef_ here:
	  gcweak.h>>WeakPointerManager
	  gcweak.h>>~WeakPointerManager
	  gcweak.h>>Mapping(const Type& val)
	  gcweak.h>>Buckets::set
	  intrinsics.cc>>cc_loadTimeValueReference
          record.h>>field specialized on gc::smart_ptr<OT>&
	*/
  Type *&rawRef_() { return this->theObject; };

  /*! Check if this tagged theObject matches the templated type.
	  The most common case is this is an object.*/
  bool valid() const {
    GCTOOLS_ASSERT(false); // BF("Implement me"));
    return true;
  }

  template <class U>
  inline bool operator==(smart_ptr<U> const other) const {
    return this->theObject == other.theObject;
  }

  template <class U>
  inline bool operator!=(smart_ptr<U> const other) const {
    return this->theObject != other.theObject;
  }
};
};
namespace gctools {
template <class T>
class smart_ptr /*: public tagged_ptr<T>*/ {
public:
  typedef T Type;
  Type *theObject;

public:
  //Default constructor, set theObject to NULL
  inline smart_ptr() : theObject(NULL){};
  //    	explicit smart_ptr(uintptr_t p) : theObject(p) {}; // TODO: this converts ints to smart_ptr's - its dangerous
  //! Construct a FRAME object - I need to get rid of these
  //smart_ptr( core::T_O** p ) : theObject(tag_frame(p)) { /*printf("%s:%d Creating Frame \n", __FILE__, __LINE__ );*/ };
  //smart_ptr( Type* objP) : theObject(tag_object(objP)) {};
  // explicit smart_ptr( void* objP) : theObject(reinterpret_cast<Type*>(objP)) {};

  /*! Create a smart pointer from an existing tagged pointer */
  explicit inline smart_ptr(Tagged ptr) : theObject(reinterpret_cast<Type *>(ptr)){};

  explicit inline smart_ptr(Type *ptr) : theObject(ptr ? tag_general<Type *>(ptr) : NULL) {
    GCTOOLS_ASSERT((reinterpret_cast<uintptr_t>(ptr) & tag_mask) == 0);
  };
  inline smart_ptr(const return_type &rt) : theObject((Type *)rt.ret0){};

  inline smart_ptr(const smart_ptr<Type> &obj) : theObject(obj.theObject){};

  template <class From>
  inline smart_ptr(smart_ptr<From> const &rhs) {
    if (TaggedCast<Type *, From *>::isA(rhs.theObject)) {
      this->theObject = TaggedCast<Type *, From *>::castOrNULL(rhs.theObject); //reinterpret_cast<From*>(rhs.raw_()));
      return;
    }
    lisp_errorCast<Type, From>(rhs.theObject);
  }

  uintptr_t tag() const { return reinterpret_cast<uintptr_t>(this->theObject) & tag_mask; };

public:
  /*! Get the pointer typcast to an integer quantity for hashing */
  cl_intptr_t intptr() const { return ((uintptr_t)(this->theObject)); };

  void reset_() { this->theObject = NULL; };

  inline void swap(smart_ptr<Type> &other) {
    Type *temp;
    temp = this->theObject;
    this->theObject = other.theObject;
    other.theObject = temp;
  };

  template <class o_class>
  inline smart_ptr<o_class> asOrNull() {
    o_class *cast = gctools::TaggedCast<o_class *, Type *>::castOrNULL(this->theObject);
    smart_ptr<o_class> ret((Tagged)cast);
    return ret;
  }

  template <class o_class>
  inline smart_ptr<o_class> asOrNull() const {
    o_class *cast = gctools::TaggedCast<o_class *, Type *>::castOrNULL(this->theObject);
    smart_ptr<o_class> ret((Tagged)cast);
    return ret;
  }

  template <class o_class>
  inline smart_ptr<o_class> as() {
    smart_ptr<o_class> ret = this->asOrNull<o_class>();
    if (ret)
      return ret;
    lisp_errorCast<o_class, Type>(this->theObject);
  }

  template <class o_class>
  inline smart_ptr<o_class> as() const {
    smart_ptr<o_class> ret = this->asOrNull<o_class>();
    if (ret)
      return ret;
    lisp_errorCast<o_class, Type>(this->theObject);
  }

  template <class o_class>
  inline bool isA() {
    smart_ptr<o_class> ret = this->asOrNull<o_class>();
    return ((bool)ret);
  }

  template <class o_class>
  inline bool isA() const {
    smart_ptr<o_class> ret = this->asOrNull<o_class>();
    return ((bool)ret);
  }

  /*! Return the offset in bytes between this.px and this - you need to modify the base
	  class of smart_ptr to make px protected */
  //	int offset_of_px_from_this() const { return ((char*)(&this->px)) - ((char*)(this));}
  /*! Return the size in bytes of px - you need to modify the base class
	  of smart_ptr to make px protected */
  //	int size_of_px() const { return sizeof(this->px); };

  int number_of_values() const { return this->theObject == NULL ? 0 : 1; };

  /*! Dereferencing operator - remove the other tag */
  Type *operator->() {
    GCTOOLS_ASSERT(this->theObject);
    GCTOOLS_ASSERT(this->generalp());
    return untag_general(this->theObject);
  };

  Type *operator->() const {
    GCTOOLS_ASSERT(this->theObject);
    GCTOOLS_ASSERT(this->generalp());
    return untag_general(this->theObject);
  };

  Type &operator*() const {
    GCTOOLS_ASSERT(this->theObject);
    GCTOOLS_ASSERT(this->objectp());
    return *(this->untag_object());
  };

  Type *untag_object() const {
    GCTOOLS_ASSERT(this->generalp() || this->consp());
    if (this->generalp()) {
      return untag_general<Type *>(this->theObject);
    } else if (this->consp()) {
      return untag_cons<Type *>(this->theObject);
    }
    THROW_HARD_ERROR(BF("This should never happen"));
  }

  Type *get() const { return this->untag_object(); };
  bool _NULLp() const { return this->theObject == NULL; };

  /*! If theObject!=NULL then return true */
  explicit operator bool() const { return this->theObject != NULL; };

#if ALLOW_NIL_OTHER
  bool nilp() const { return tagged_nilp(this->theObject); }
  bool notnilp() const { return (!this->nilp()); };
  bool isTrue() const { return !this->nilp(); };
  core::Cons_O *unsafe_cons() const {
    GCTOOLS_ASSERT(this->consp());
    return reinterpret_cast<core::Cons_O *>(reinterpret_cast<uintptr_t>(this->theObject) - cons_tag);
  };
#else
  //bool nilp() const { return tagged_nilp(this->theObject); }
  //bool notnilp() const { return (!this->nilp());};
  bool isTrue() const { return true; };
#endif
  bool objectp() const { return this->generalp() || this->consp(); };
  bool generalp() const { return tagged_generalp<Type *>(this->theObject); };
  bool consp() const { return tagged_consp<Type *>(this->theObject); };
  bool unboundp() const { return tagged_unboundp(this->theObject); };
  bool deletedp() const { return tagged_deletedp(this->theObject); };
  bool sameAsKeyp() const { return tagged_sameAsKeyp(this->theObject); };
  bool fixnump() const { return tagged_fixnump(this->theObject); };
  Fixnum unsafe_fixnum() const { return untag_fixnum(this->theObject); };
  bool characterp() const { return tagged_characterp<Type *>(this->theObject); };
  int unsafe_character() const { return untag_character(this->theObject); };
  bool single_floatp() const { return tagged_single_floatp<Type *>(this->theObject); };
  float unsafe_single_float() const { return untag_single_float<Type *>(this->theObject); };
  // This replaces pointerp()

  Fixnum asFixnum() const {
    GCTOOLS_ASSERT(this->fixnump());
    return untag_fixnum<Type *>(this->theObject);
  };

  bool sameAsKeyP() const { return tagged_sameAsKeyp(this->theObject); }

  /*! Return the raw smart_ptr value interpreted as a T_O* */
  inline core::T_O *raw_() const { return reinterpret_cast<core::T_O *>(this->theObject); }

  inline void setRaw_(Tagged p) { this->theObject = reinterpret_cast<Type *>(p); }

  /*! This should almost NEVER be used!!!!!!   
	  The only reason to ever use this is when theObject will be set to NULL
	  and you are sure that it will not be interpreted as a Fixnum!!!

	  List actual uses here:
	  gcweak.h>>WeakPointerManager
	  gcweak.h>>~WeakPointerManager
	  gcweak.h>>Mapping(const Type& val)
	  gcweak.h>>Buckets::set
	  intrinsics.cc>>cc_loadTimeValueReference
	*/
  Type *&rawRef_() { return this->theObject; };

  /*! Check if this tagged theObject matches the templated type.
	  The most common case is this is an object.*/
  bool valid() const {
    GCTOOLS_ASSERT(false); // BF("Implement me"));
  }

  template <class U>
  inline bool operator==(smart_ptr<U> const other) const {
    return reinterpret_cast<uintptr_t>(this->theObject) == reinterpret_cast<uintptr_t>(other.theObject);
  }

  template <class U>
  inline bool operator!=(smart_ptr<U> const other) const {
    return reinterpret_cast<uintptr_t>(this->theObject) != reinterpret_cast<uintptr_t>(other.theObject);
  }
};
};

namespace gctools {
template <typename Type>
inline static smart_ptr<Type> make_tagged_fixnum(Fixnum val) { return smart_ptr<Type>((Tagged)tag_fixnum<Type *>(val)); }
template <typename Type>
inline static smart_ptr<Type> make_tagged_single_float(float val) { return smart_ptr<Type>((Tagged)tag_single_float<Type *>(val)); }

inline static smart_ptr<core::Character_I> make_tagged_character(claspCharacter val) { return smart_ptr<core::Character_I>((Tagged)tag_character<core::Character_I *>(val)); };

template <typename Type>
inline static smart_ptr<Type> make_tagged_other(Type *p) { return smart_ptr<Type>(p); }
template <typename Type>
inline static smart_ptr<Type> make_tagged_nil() { return smart_ptr<Type>((Tagged)global_tagged_Symbol_OP_nil); };
template <typename Type>
inline static smart_ptr<Type> make_tagged_unbound() { return smart_ptr<Type>((Tagged)global_tagged_Symbol_OP_unbound); };
template <typename Type>
inline static smart_ptr<Type> make_tagged_deleted() { return smart_ptr<Type>((Tagged)global_tagged_Symbol_OP_deleted); };
template <typename Type>
inline static smart_ptr<Type> make_tagged_sameAsKey() { return smart_ptr<Type>((Tagged)global_tagged_Symbol_OP_sameAsKey); };
};

namespace core {

class List_V {}; // Virtual class representing Common Lisp LIST
typedef gctools::smart_ptr<T_O> T_sp;
typedef gctools::smart_ptr<List_V> List_sp;

extern gctools::smart_ptr<T_O> cons_car(Cons_O *cur);
extern gctools::smart_ptr<T_O> cons_cdr(Cons_O *cur);
};

#if 0
namespace core {
 // A struct that wraps va_list and behaves like a Common Lisp LIST
  typedef gctools::smart_ptr<VaList_S> VaList_sp;
  struct VaList_S {
/* WARNING WARNING WARNING WARNING
DO NOT CHANGE THE ORDER OF THESE OBJECTS WITHOUT UPDATING THE DEFINITION OF +va_list+ in cmpintrinsics.lsp
*/
    mutable va_list _Args;
    core::T_O* asTaggedPtr() {
      return gctools::tag_valist<core::T_O*>(this);
    }
//#define SIMULATE_LIST
#ifdef SIMULATE_LIST
    mutable size_t _NumArgs;
    mutable core::T_O* _Car;
    mutable bool _Called_va_arg;
    size_t numberOfArguments() const { return this->_NumArgs;};
    core::T_O* car() {
      if ( this->_NumArgs == 0 ) return reinterpret_cast<core::T_O*>(gctools::global_tagged_Symbol_OP_nil);
      if (this->_Called_va_arg) return this->_Car;
      this->_Car = va_arg(this->_Args,core::T_O*);
      --this->_NumArgs;
      this->_Called_va_arg = true;
      return this->_Car;
    }
    core::T_O* cdr() {
      if ( this->_NumArgs == 0 ) return reinterpret_cast<core::T_O*>(gctools::global_tagged_Symbol_OP_nil);
      if ( this->_Called_va_arg ) {
        this->_Called_va_arg = false;
        return gctools::tag_valist<core::T_O*>(this);
      }
      this->_Car = va_arg(this->_Args,core::T_O*);
      this->_Called_va_arg = true;
      --this->_NumArgs;
      return gctools::tag_valist<core::T_O*>(this);
    };
#endif
  VaList_S(int nargs, va_list vargs)
#ifdef SIMULATE_LIST
  : _NumArgs(nargs), _Called_va_arg(false)
#endif
    {
      va_copy(this->_Args,vargs);
    };
  VaList_S(const VaList_S& other)
#ifdef SIMULATE_LIST
  : _NumArgs(other._NumArgs), _Called_va_arg(false)
#endif
    {
      va_copy(this->_Args,other._Args);
    }
    
  VaList_S(int nargs)
#ifdef SIMULATE_LIST
  : _NumArgs(nargs), _Called_va_arg(false)
#endif
    {
   // Nothing, caller must initialize _Args
  };
  VaList_S()
#ifdef SIMULATE_LIST
  : _NumArgs(0), _Called_va_arg(false)
#endif
    {
    };
    virtual ~VaList_S() {}; // Make it polymorphic
  };
};
#endif

namespace gctools {
//////////////////////////////////////////////////////////////////////
//
// Declare AsOrNull and As converters
//
template <typename To_SP>
inline bool IsA(return_type const &rhs) {
  return TaggedCast<typename To_SP::Type *, typename core::T_O *>::isA(rhs.ret0);
};
template <typename To_SP, typename From_SP>
inline bool IsA(From_SP const &rhs) {
  return TaggedCast<typename To_SP::Type *, typename From_SP::Type *>::isA(reinterpret_cast<typename From_SP::Type *>(rhs.raw_()));
};
template <typename To_SP, typename From_SP>
inline To_SP AsOrNull(From_SP const &rhs) {
  if (LIKELY(rhs.generalp())) {
    typename To_SP::Type *cast = TaggedCast<typename To_SP::Type *, typename From_SP::Type *>::castOrNULL(untag_general<typename From_SP::Type *>(reinterpret_cast<typename From_SP::Type *>(rhs.raw_())));
    if (cast == NULL)
      return To_SP();
    To_SP ret((Tagged)tag_general<typename To_SP::Type *>(cast));
    return ret;
  } else if (LIKELY(rhs.consp())) {
    typename To_SP::Type *cast = TaggedCast<typename To_SP::Type *, typename From_SP::Type *>::castOrNULL(untag_cons<typename From_SP::Type *>(reinterpret_cast<typename From_SP::Type *>(rhs.raw_())));
    if (cast == NULL)
      return To_SP();
    To_SP ret((Tagged)tag_cons<typename To_SP::Type *>(cast));
    return ret;
  }
  class_id expected_typ = reg::registered_class<typename To_SP::Type>::id;
  class_id this_typ = reg::registered_class<typename From_SP::Type>::id;
  lisp_errorBadCast(expected_typ, this_typ, rhs.raw_());
  // unreachable
  HARD_UNREACHABLE();
};
template <typename To_SP, typename From_SP>
inline To_SP As(From_SP const &rhs) {
  if (IsA<To_SP>(rhs)) {
    To_SP ret((Tagged)rhs.raw_());
    return ret;
  }
  class_id expected_typ = reg::registered_class<typename To_SP::Type>::id;
  class_id this_typ = reg::registered_class<typename From_SP::Type>::id;
  lisp_errorBadCast(expected_typ, this_typ, reinterpret_cast<core::T_O *>(rhs.raw_()));
  HARD_UNREACHABLE();
}
template <typename To_SP>
inline To_SP As(const return_type &rhs) {
  GCTOOLS_ASSERT(rhs.nvals == 1);
  if (IsA<To_SP>(rhs)) {
    To_SP ret((Tagged)rhs.ret0);
    return ret;
  }
  class_id expected_typ = reg::registered_class<typename To_SP::Type>::id;
  class_id this_typ = reg::registered_class<core::T_O *>::id;
  lisp_errorBadCast(expected_typ, this_typ, reinterpret_cast<core::T_O *>(rhs.ret0));
  HARD_UNREACHABLE();
}
};

namespace gctools {
template <>
class smart_ptr<core::T_O> : public tagged_ptr<core::T_O> {
public:
  typedef core::T_O Type;

public:
  //Default constructor, set theObject to NULL
  smart_ptr() : tagged_ptr<Type>(){};
  explicit inline smart_ptr(Type *ptr) : tagged_ptr<Type>(ptr){};
  /*! Create a smart pointer from an existing tagged pointer */
  explicit inline smart_ptr(Tagged ptr) : tagged_ptr<Type>((Tagged)ptr){};
  inline smart_ptr(const smart_ptr<Type> &obj) : tagged_ptr<Type>((Tagged)obj.theObject){};
  inline smart_ptr(const return_type &rt) : tagged_ptr<Type>((Tagged)rt.ret0){};
  template <class From>
  inline smart_ptr(smart_ptr<From> const &rhs) : tagged_ptr<Type>((Tagged)rhs.theObject){};

  template <class o_class>
  inline smart_ptr<o_class> asOrNull() {
    return smart_ptr<o_class>((Tagged)TaggedCast<o_class *, Type *>::castOrNULL(this->theObject));
  }

  template <class o_class>
  inline smart_ptr<o_class> asOrNull() const {
    return smart_ptr<o_class>((Tagged)TaggedCast<o_class *, Type *>::castOrNULL(this->theObject));
  }

  template <class o_class>
  inline smart_ptr<o_class> as() {
    smart_ptr<o_class> ret = this->asOrNull<o_class>();
    if (ret)
      return ret;
    lisp_errorCast<o_class, Type>(this->theObject);
  }

  template <class o_class>
  inline smart_ptr<o_class> as() const {
    smart_ptr<o_class> ret = this->asOrNull<o_class>();
    if (ret)
      return ret;
    lisp_errorCast<o_class, Type>(this->theObject);
  }

  template <class o_class>
  inline bool isA() {
    smart_ptr<o_class> ret = this->asOrNull<o_class>();
    return ((bool)ret);
  }

  template <class o_class>
  inline bool isA() const {
    smart_ptr<o_class> ret = this->asOrNull<o_class>();
    return ((bool)ret);
  }
};
};

namespace gctools {
template <>
class smart_ptr<core::Fixnum_I> /*: public tagged_ptr<Type>*/ {
public:
  typedef core::Fixnum_I Type;
  Type *theObject;

public:
  //Default constructor, set theObject to NULL
  smart_ptr() : theObject(NULL){};

  smart_ptr(Type *fn) : theObject(fn){};
  template <typename From>
  inline smart_ptr(smart_ptr<From> const &rhs) {
    if (rhs.fixnump()) {
      this->theObject = reinterpret_cast<Type *>(rhs.raw_());
      return;
    }
    class_id from_typ = reg::registered_class<From>::id;
    lisp_errorBadCastToFixnum(from_typ, rhs.raw_());
  }
  /*! Constructor that takes Tagged assumes that the pointer is tagged.
	  Any ptr passed to this constructor must have the CONS tag.
	*/
  explicit inline smart_ptr(Tagged ptr)
      : theObject(reinterpret_cast<Type *>(ptr)) {
    GCTOOLS_ASSERT(tagged_fixnump<Type *>(reinterpret_cast<Type *>(ptr)));
  };

public:
  inline operator bool() { return this->theObject != NULL; };
  inline operator smart_ptr<core::T_O>() const { return smart_ptr<core::T_O>((Tagged) this->theObject); };

public:
  inline bool nilp() const { return tagged_nilp(this->theObject); }
  inline bool notnilp() const { return (!this->nilp()); };
  inline bool fixnump() const { return tagged_fixnump(this->theObject); };
  inline bool generalp() const { return tagged_generalp(this->theObject); };
  inline bool consp() const { return tagged_consp(this->theObject); };
  inline bool objectp() const { return this->generalp() || this->consp(); };
  inline Fixnum unsafe_fixnum() const { return untag_fixnum(this->theObject); };
  inline core::T_O *raw_() const { return reinterpret_cast<core::T_O *>(this->theObject); };
};
};

namespace core {
typedef gctools::smart_ptr<Fixnum_I> Fixnum_sp;
typedef gctools::smart_ptr<SingleFloat_I> SingleFloat_sp;
typedef gctools::smart_ptr<Character_I> Character_sp;
};

namespace gctools {
template <>
class smart_ptr<core::Symbol_O> /*: public tagged_ptr<Type>*/ {
public:
  typedef core::Symbol_O Type;
  Type *theObject;

public:
  //Default constructor, set theObject to NULL
  smart_ptr() : theObject(NULL){};
  //    	explicit smart_ptr(uintptr_t p) : theObject(p) {}; // TODO: this converts ints to smart_ptr's - its dangerous
  //! Construct a FRAME object - I need to get rid of these
  //smart_ptr( core::T_O** p ) : theObject(tag_valist(p)) { /*printf("%s:%d Creating Frame \n", __FILE__, __LINE__ );*/ };
  //smart_ptr( Type* objP) : theObject(tag_object(objP)) {};
  // explicit smart_ptr( void* objP) : theObject(reinterpret_cast<Type*>(objP)) {};

  /*! Create a smart pointer from an existing tagged pointer */
  explicit inline smart_ptr(Tagged ptr) : theObject(reinterpret_cast<Type *>(ptr)) {
    GCTOOLS_ASSERT(!ptr || (reinterpret_cast<uintptr_t>(ptr) & tag_mask) != 0);
  };

  explicit inline smart_ptr(Type *ptr) : theObject(ptr ? tag_general<Type *>(ptr) : NULL) {
    GCTOOLS_ASSERT((reinterpret_cast<uintptr_t>(ptr) & tag_mask) == 0);
  };

  inline smart_ptr(const smart_ptr<Type> &obj) : theObject(obj.theObject){};

  template <class From>
  inline smart_ptr(smart_ptr<From> const &rhs) {
    if (LIKELY(rhs.objectp())) {
      Type *px = TaggedCast<Type *, From *>::castOrNULL(rhs.theObject);
      if (px == 0) {
        THROW_HARD_ERROR(BF("TaggedCast<Type*,From*> failed due to an illegal cast To* = %s  From* = %s") % typeid(Type *).name() % typeid(From *).name());
      }
      this->theObject = px;
    } else {
      this->theObject = reinterpret_cast<Type *>(rhs.theObject);
    }
  }

  uintptr_t tag() const { return reinterpret_cast<uintptr_t>(this->theObject) & tag_mask; };

public:
  //----------------------------------------------------------------------
  //
  // Constructors
  //
  //
  // Make a tagged fixnum

  /*! Get the pointer typcast to an integer quantity for hashing */
  cl_intptr_t intptr() const { return ((uintptr_t)(this->theObject)); };

  void reset_() { this->theObject = NULL; };

  inline void swap(smart_ptr<Type> &other) {
    Type *temp;
    temp = this->theObject;
    this->theObject = other.theObject;
    other.theObject = temp;
  }

  template <class o_class>
  inline smart_ptr<o_class> asOrNull() {
    o_class *cast = gctools::TaggedCast<o_class *, Type *>::castOrNULL(this->theObject);
    return smart_ptr<o_class>((Tagged)cast);
  }

  template <class o_class>
  inline smart_ptr<o_class> asOrNull() const {
    o_class *cast = gctools::TaggedCast<o_class *, Type *>::castOrNULL(this->theObject);
    return smart_ptr<o_class>((Tagged)cast);
  }

  /*! Return the offset in bytes between this.px and this - you need to modify the base
	  class of smart_ptr to make px protected */
  //	int offset_of_px_from_this() const { return ((char*)(&this->px)) - ((char*)(this));}
  /*! Return the size in bytes of px - you need to modify the base class
	  of smart_ptr to make px protected */
  //	int size_of_px() const { return sizeof(this->px); };

  int number_of_values() const { return this->theObject == NULL ? 0 : 1; };

  /*! Dereferencing operator - remove the other tag */
  Type *operator->() {
    GCTOOLS_ASSERT(this->generalp());
    GCTOOLS_ASSERT(!this->unboundp());
    return untag_general(this->theObject);
  };

  Type *operator->() const {
    GCTOOLS_ASSERT(this->generalp());
    GCTOOLS_ASSERT(!this->unboundp());
    return untag_general(this->theObject);
  };

  Type &operator*() const {
    GCTOOLS_ASSERT(this->objectp());
    GCTOOLS_ASSERT(!this->unboundp());
    return *(this->untag_object());
  };

  Type *untag_object() const {
    GCTOOLS_ASSERT(this->generalp() || this->consp());
    if (this->generalp()) {
      return untag_general<Type *>(this->theObject);
    } else if (this->consp()) {
      return untag_cons<Type *>(this->theObject);
    }
    THROW_HARD_ERROR(BF("This should never happen"));
  }

  Type *get() const { return this->untag_object(); };
  bool _NULLp() const { return this->theObject == NULL; };

  /*! If theObject!=NULL then return true */
  explicit operator bool() const { return this->theObject != NULL; };

  bool nilp() const { return tagged_nilp(this->theObject); }
  bool notnilp() const { return (!this->nilp()); };
  bool isTrue() const { return !this->nilp(); };
  bool fixnump() const { return tagged_fixnump(this->theObject); };
  Fixnum unsafe_fixnum() const { return untag_fixnum(this->theObject); };
  bool unboundp() const { return tagged_unboundp(this->theObject); };
  bool deletedp() const { return tagged_deletedp(this->theObject); };
  bool sameAsKeyp() const { return tagged_sameAsKeyp(this->theObject); };
  bool characterp() const { return tagged_characterp<Type *>(this->theObject); };
  int unsafe_character() const { return untag_character(this->theObject); };
  bool single_floatp() const { return tagged_single_floatp<Type *>(this->theObject); };
  float unsafe_single_float() const { return untag_single_float<Type *>(this->theObject); };
  // This replaces pointerp()
  bool objectp() const { return this->generalp() || this->consp(); };
  bool generalp() const { return tagged_generalp<Type *>(this->theObject); };
  bool consp() const { return tagged_consp<Type *>(this->theObject); };
  core::Cons_O *unsafe_cons() const {
    GCTOOLS_ASSERT(this->consp());
    return reinterpret_cast<core::Cons_O *>(reinterpret_cast<uintptr_t>(this->theObject) - cons_tag);
  };

  Fixnum asFixnum() const {
    GCTOOLS_ASSERT(this->fixnump());
    return untag_fixnum<Type *>(this->theObject);
  };
  bool sameAsKeyP() const { return tagged_sameAsKeyp(this->theObject); }
  /*! Return the raw smart_ptr value interpreted as a T_O* */
  inline core::T_O *raw_() const { return reinterpret_cast<core::T_O *>(this->theObject); }

  inline void setRaw_(Tagged p) { this->theObject = reinterpret_cast<Type *>(p); }

  /*! This should almost NEVER be used!!!!!!   
	  The only reason to ever use this is when theObject will be set to NULL
	  and you are sure that it will not be interpreted as a Fixnum!!!

	  List actual uses here:
	  gcweak.h>>WeakPointerManager
	  gcweak.h>>~WeakPointerManager
	  gcweak.h>>Mapping(const Type& val)
	  gcweak.h>>Buckets::set
	  intrinsics.cc>>cc_loadTimeValueReference
	*/
  Type *&rawRef_() { return this->theObject; };

  /*! Check if this tagged theObject matches the templated type.
	  The most common case is this is an object.*/
  bool valid() const {
    GCTOOLS_ASSERT(false); // BF("Implement me"));
    return true;
  }

  template <class U>
  inline bool operator==(smart_ptr<U> const other) const {
    return this->theObject == other.theObject;
  }

  template <class U>
  inline bool operator!=(smart_ptr<U> const other) const {
    return this->theObject != other.theObject;
  }
};
};

namespace cl {
extern gctools::smart_ptr<core::Symbol_O> _sym_list;
extern gctools::smart_ptr<core::Symbol_O> _sym_typeError;
}

namespace kw {
extern gctools::smart_ptr<core::Symbol_O> _sym_datum;
extern gctools::smart_ptr<core::Symbol_O> _sym_expectedType;
}
namespace core {
extern gctools::smart_ptr<core::T_O> lisp_createList(gctools::smart_ptr<core::T_O> a1, gctools::smart_ptr<core::T_O> a2, gctools::smart_ptr<core::T_O> a3, gctools::smart_ptr<core::T_O> a4);
extern void lisp_error_condition(const char *functionName, const char *fileName, int lineNumber, gctools::smart_ptr<core::T_O> baseCondition, gctools::smart_ptr<core::T_O> initializers);
}

namespace gctools {

template <>
class smart_ptr<core::Cons_O> {
public:
  typedef core::Cons_O Type;
  core::Cons_O *theObject;

public:
  //! The default constructor returns an invalid smart_ptr
  smart_ptr() : theObject(NULL){};
  // Constructor that takes Cons_O* assumes its untagged
  explicit inline smart_ptr(core::Cons_O *ptr) : theObject(ptr ? tag_cons<core::Cons_O *>(ptr) : NULL) {
    GCTOOLS_ASSERT((reinterpret_cast<uintptr_t>(ptr) & tag_mask) == 0);
  };
  /*! Constructor that takes Tagged assumes that the pointer is tagged.
	  Any ptr passed to this constructor must have the CONS tag.
	*/
  explicit inline smart_ptr(Tagged ptr) : theObject(reinterpret_cast<core::Cons_O *>(ptr)) {
    GCTOOLS_ASSERT(!ptr || tagged_consp<core::Cons_O *>(reinterpret_cast<core::Cons_O *>(ptr))
                   //			   ||tagged_nilp<core::Cons_O>(reinterpret_cast<core::Cons_O*>(ptr))
                   );
  };

  // A constructor used by the iterator to bypass any tagged pointer checking
  explicit inline smart_ptr(Tagged ptr, bool dummy) : theObject(reinterpret_cast<core::Cons_O *>(ptr)){};

public:
  explicit operator bool() const { return this->theObject != NULL; }

public:
  void reset_() { this->theObject = NULL; };
  inline bool generalp() const { return tagged_generalp<core::Cons_O *>(this->theObject); };
  inline bool objectp() const { return this->generalp() || this->consp(); };
  inline bool isTrue() const { return true; };
  inline bool consp() const { return tagged_consp(this->theObject); };
  inline bool valid() const { return this->consp(); } // || this->nilp(); };
  inline bool unboundp() const { return tagged_unboundp(this->theObject); };
  inline Type *&rawRef_() { return this->theObject; };

  inline void setRaw_(Tagged p) { this->theObject = reinterpret_cast<Type *>(p); }

  operator smart_ptr<core::T_O>() const { return smart_ptr<core::T_O>((Tagged) const_cast<core::T_O *const>(reinterpret_cast<core::T_O *>(this->theObject))); };

  //	operator smart_ptr<core::List_V>() const { return smart_ptr<core::List_V>((Tagged)const_cast<core::T_O* const>(reinterpret_cast<core::T_O*>(this->theObject)));};

  inline core::Cons_O *untag_object() const {
    GCTOOLS_ASSERT(this->consp());
    return untag_cons<core::Cons_O *>(this->theObject);
  }

  inline void swap(smart_ptr<core::Cons_O> &other) {
    core::Cons_O *temp;
    temp = this->theObject;
    this->theObject = other.theObject;
    other.theObject = temp;
  }

  /*! Dereferencing operator - remove the other tag */
  inline core::Cons_O *operator->() {
    GCTOOLS_ASSERT(this->objectp());
    return this->untag_object();
  };
  inline core::Cons_O *operator->() const {
    GCTOOLS_ASSERT(this->objectp());
    return this->untag_object();
  };

  inline core::Cons_O &operator*() {
    GCTOOLS_ASSERT(this->objectp());
    return *(this->untag_object());
  };

  core::T_O *raw_() const { return reinterpret_cast<core::T_O *>(this->theObject); }
  bool _NULLp() const { return this->theObject == NULL; };

  template <class U>
  inline bool operator==(smart_ptr<U> const other) const {
    return this->theObject == other.theObject;
  }

  template <class U>
  inline bool operator!=(smart_ptr<U> const other) const {
    return this->theObject != other.theObject;
  }
};
};

/*! List_sp implementation with iterator by Georgiy Tugai  April 27, 2015
 */

namespace gctools {
// my comment
class List_sp_iterator;

template <>
class smart_ptr<core::List_V> {
public:
  typedef core::T_O
      Type; // The best common type for both Cons_O and Symbol_O is T_O
  Type *theObject;

public:
  inline void setRaw_(Tagged p) { this->theObject = reinterpret_cast<Type *>(p); }
  inline Type *&rawRef_() { return this->theObject; };

public:
  //! The default constructor returns an invalid smart_ptr
  inline smart_ptr() : theObject(NULL){};
  inline smart_ptr(const return_type &rt) : theObject((Type *)rt.ret0){};
  inline smart_ptr(const smart_ptr<core::T_O> &other) {
    if (other.consp()) {
      this->theObject = other.theObject;
    } else if (other.nilp()) {
      this->theObject = other.theObject;
    } else {
      lisp_error_condition(__FUNCTION__, __FILE__, __LINE__, cl::_sym_typeError, core::lisp_createList(kw::_sym_datum, other, kw::_sym_expectedType, cl::_sym_list));
    }
  }
  inline smart_ptr(const smart_ptr<core::Cons_O> &other)
      : theObject(other.raw_()) {
    GCTOOLS_ASSERT(other.consp());
  };
  // Constructor that takes Cons_O* assumes its untagged
  explicit inline smart_ptr(core::Cons_O *ptr)
      : theObject(tag_cons<Type *>(reinterpret_cast<core::T_O *>(ptr))) {
    GCTOOLS_ASSERT((reinterpret_cast<uintptr_t>(ptr) & tag_mask) == 0);
  };
  explicit inline smart_ptr(core::Symbol_O *ptr)
      : theObject(tag_general<Type *>(reinterpret_cast<core::T_O *>(ptr))) {
    GCTOOLS_ASSERT((reinterpret_cast<uintptr_t>(ptr) & tag_mask) == 0);
  };
  /*! Constructor that takes Tagged assumes that the pointer is tagged.
	  Any ptr passed to this constructor must have the CONS tag.
	*/
  explicit inline smart_ptr(Tagged ptr)
      : theObject(reinterpret_cast<Type *>(ptr)) {
    GCTOOLS_ASSERT(!ptr || tagged_consp<Type *>(reinterpret_cast<Type *>(ptr)) ||
                   tagged_nilp<Type *>(reinterpret_cast<Type *>(ptr)));
  };

public:
  explicit operator bool() const { return this->theObject != NULL; }

public:
  void reset_() { this->theObject = NULL; };
  inline bool generalp() const { return tagged_generalp<Type *>(this->theObject); };
  inline bool consp() const { return tagged_consp(this->theObject); };
  inline core::Cons_O *unsafe_cons() const {
    GCTOOLS_ASSERT(this->consp());
    return reinterpret_cast<core::Cons_O *>(reinterpret_cast<uintptr_t>(this->theObject) - cons_tag);
  };
  inline bool objectp() const { return this->generalp() || this->consp(); };
  inline bool nilp() const { return tagged_nilp(this->theObject); };
  inline bool notnilp() const { return !this->nilp(); };
  inline bool isTrue() const { return !this->nilp(); };
  inline bool unboundp() const { return tagged_unboundp(this->theObject); };

  //	inline operator smart_ptr<core::Cons_O>() const { GCTOOLS_ASSERT(this->consp());return smart_ptr<core::Cons_O>((Tagged)this->theObject); };
  inline smart_ptr<core::Cons_O> asCons() const {
    GCTOOLS_ASSERT(this->consp());
    return smart_ptr<core::Cons_O>((Tagged) this->theObject);
  };

  inline bool valid() const { return this->consp() || this->nilp(); };

  operator smart_ptr<core::T_O>() const {
    return smart_ptr<Type>((Tagged) const_cast<Type *const>(reinterpret_cast<Type *>(this->theObject)));
  };

  Type *untag_object() const {
    GCTOOLS_ASSERT(this->generalp() || this->consp());
    if (this->consp()) {
      return untag_cons<Type *>(this->theObject);
    } else {
      if (this->nilp()) {
        return tag_nil<Type *>();
      }
    }
    THROW_HARD_ERROR(BF("Figure out what to do when untag_object doesn't have "
                        "a Cons_O or NIL"));
  }

  inline void swap(smart_ptr<core::List_V> &other) {
    Type *temp;
    temp = this->theObject;
    this->theObject = other.theObject;
    other.theObject = temp;
  }

  /*! Dereferencing operator - remove the other tag */
  inline Type *operator->() {
    GCTOOLS_ASSERT(this->objectp());
    return this->untag_object();
  };
  inline Type *operator->() const {
    GCTOOLS_ASSERT(this->objectp());
    return this->untag_object();
  };

  inline Type &operator*() {
    GCTOOLS_ASSERT(this->objectp());
    return *(this->untag_object());
  };
  inline const Type &operator*() const {
    GCTOOLS_ASSERT(this->objectp());
    return *(this->untag_object());
  };

  core::T_O *raw_() const { return reinterpret_cast<Type *>(this->theObject); }
  bool _NULLp() const { return this->theObject == NULL; };

  template <class U>
  inline bool operator==(smart_ptr<U> const other) const {
    return this->theObject == other.theObject;
  }

  template <class U>
  inline bool operator!=(smart_ptr<U> const other) const {
    return this->theObject != other.theObject;
  }

public:
private:
  template <bool fast>
  class List_sp_iterator {
  public:
    // This is the end iterator - it sets the ptr to the symbol NIL!
    // Note: It's spoofing the iterator ptr field by sticking
    // the SYMBOL NIL into a slot that is set up to be a tagged CONS!!!!
    // This is what we have to do to make things fast.
    // To do this I'm using a special smart_ptr<core::Cons_O> constructor
    // that takes TWO arguments, where the second one is a dummy
    //
    // XXX: What happens if someone tries to increment end()?
    //    Answer: Something bad - don't do this
    List_sp_iterator() : ptr((Tagged)tag_nil<core::Cons_O *>(), false) {}
    List_sp_iterator(const core::List_sp &ptr) : ptr(ptr.asCons()){};
    List_sp_iterator &operator++() {
      GCTOOLS_ASSERT(this->consp());
      core::T_O *rawcdr = cons_cdr(&*ptr).raw_();
      ptr.rawRef_() = reinterpret_cast<core::Cons_O *>(rawcdr);
      return *this;
    }
    List_sp_iterator &operator++(int) { // postfix
      auto clone = new List_sp_iterator(*this);
      ++*this;
      return *clone;
    }
    bool consp() const { return tagged_consp(ptr.raw_()); };
    smart_ptr<core::Cons_O> *operator->() { return &ptr; }
    const smart_ptr<core::Cons_O> *operator->() const { return &ptr; }
    const smart_ptr<core::Cons_O> &operator*() const { return ptr; }
    smart_ptr<core::Cons_O> &operator*() { return ptr; }
    // Unsafe but fast cast of T_O* to Cons_O* - should only be done within a loop
    /* smart_ptr<core::Cons_O> operator*() { return smart_ptr<core::Cons_O>((Tagged)(ptr)); } */
  public:
    smart_ptr<core::Cons_O> ptr;
  };

public:
  typedef List_sp_iterator<false> iterator;
  typedef List_sp_iterator<true> fast_iterator;

public:
  iterator begin() {
    if (consp())
      return iterator(*this);
    else
      return iterator();
  }
  iterator end() { return iterator(); }

  iterator const begin() const {
    if (consp())
      return iterator(*this);
    else
      return iterator();
  }
  iterator const end() const { return iterator(); }

private:
  class fast_iterator_proxy {
  public:
    fast_iterator_proxy(const core::List_sp &ptr) : ptr(ptr) {}
    fast_iterator begin() {
      if (ptr.consp())
        return fast_iterator(ptr);
      else
        return fast_iterator();
    }
    fast_iterator end() { return fast_iterator(); }

    fast_iterator const begin() const {
      if (ptr.consp())
        return fast_iterator(ptr);
      else
        return fast_iterator();
    }
    fast_iterator const end() const { return fast_iterator(); }

  private:
    const core::List_sp &ptr;
  };

public:
  fast_iterator_proxy full() { return fast_iterator_proxy(*this); }

  smart_ptr<core::List_V> &operator=(const smart_ptr<core::List_V> &other) {
    if (this == &other)
      return *this;
    this->theObject = other.theObject;
    return *this;
  };

  template <typename From>
  smart_ptr<core::List_V> &operator=(const smart_ptr<From> &other) {
    if (this == reinterpret_cast<smart_ptr<core::List_V> *>(const_cast<smart_ptr<From> *>(&other)))
      return *this;
    if (tagged_consp<From *>(other.theObject)) {
      this->theObject = other.theObject;
    } else if (tagged_nilp<From *>(other.theObject)) {
      this->theObject = other.theObject;
    } else {
      lisp_error_condition(__FUNCTION__, __FILE__, __LINE__, cl::_sym_typeError, core::lisp_createList(kw::_sym_datum, other, kw::_sym_expectedType, cl::_sym_list));
    }
    return *this;
  };
};

inline bool operator==(const core::List_sp::iterator &a, const core::List_sp::iterator &b) { return UNLIKELY(*a == *b); }
inline bool operator!=(const core::List_sp::iterator &a, const core::List_sp::iterator &b) { return LIKELY(*a != *b); }
// XXX: BAD VOODOO!
// Justification:
// http://www.open-std.org/jtc1/sc22/wg21/docs/papers/2007/n2243.html#the-range-based-for-statement
// Range-for will, supposedly, use != as current != end, always.
inline bool operator==(const core::List_sp::fast_iterator &a, const core::List_sp::fast_iterator &b) { return !a->consp(); }
inline bool operator!=(const core::List_sp::fast_iterator &a, const core::List_sp::fast_iterator &b) { return a->consp(); }
};

template <class T>
gctools::smart_ptr<T> _Nil() {
  gctools::smart_ptr<T> x((gctools::Tagged)gctools::tag_nil<T *>());
  return x;
}

template <class T>
gctools::smart_ptr<T> _Unbound() {
  gctools::smart_ptr<T> x((gctools::Tagged)gctools::tag_unbound<T *>());
  return x;
}

template <class T>
gctools::smart_ptr<T> _Deleted() {
  gctools::smart_ptr<T> x((gctools::Tagged)gctools::tag_deleted<T *>());
  return x;
}

template <class T>
gctools::smart_ptr<T> _SameAsKey() {
  gctools::smart_ptr<T> x((gctools::Tagged)gctools::tag_sameAsKey<T *>());
  return x;
}

//template <class T> inline bool Null(const gctools::smart_ptr<T>& ptr) { return ptr.nilp();};

namespace gctools {

// LambdaListHandler_sp llh(ptr)

#if defined(USE_BOEHM) || defined(USE_MPS)

template <class TO, class FROM>
smart_ptr<TO> dynamic_pointer_cast(const smart_ptr<FROM> &ptr) {
  return smart_ptr<TO>(dynamic_cast<TO *>(ptr.pxget()));
};

template <class TO, class FROM>
smart_ptr<TO> dynamic_pointer_cast(FROM ptr) {
  return smart_ptr<TO>(dynamic_cast<typename TO::PointerType>(ptr.pxget()));
};

#else

template <class TO, class FROM>
smart_ptr<TO> dynamic_pointer_cast(const smart_ptr<FROM> &ptr) {
  return smart_ptr<TO>(boost::dynamic_pointer_cast<TO>(ptr));
};

template <class TO, class FROM>
smart_ptr<TO> dynamic_pointer_cast(FROM ptr) {
  return smart_ptr<TO>(boost::dynamic_pointer_cast<TO>(ptr));
};
#endif

#if 0
    template <>
	bool smart_ptr<core::T_O>::isA() const {return true;}
    // Do I need ther isA tests for Fixnum, Character - etc?
#endif
};

namespace core {
using gctools::Fixnum;
};

#if 0
namespace gctools {
/*! Maintain tagged pointers to Functoids and their derived classes
      It would be better to have a separate tag for these.  
      Maybe if I can figure out 16-byte alignment with Boehm
    */
template <typename T>
class tagged_functor {
public:
  typedef T Type;
  Type *thePointer;

public:
 explicit tagged_functor() : thePointer(NULL){};
  template <typename From>
  explicit inline tagged_functor(tagged_functor<From> const &rhs) {
    if (LIKELY(rhs.generalp())) {
      Type *px = dynamic_cast<Type *>(untag_general<From *>(rhs.thePointer));
      if (px) {
        this->thePointer = tag_general<Type *>(px);
        return;
      }
      THROW_HARD_ERROR(BF("Cannot cast tagged_functor in constructor"));
    }
    THROW_HARD_ERROR(BF("Bad tag on tagged_functor in constructor"));
  };

 explicit tagged_functor(Type *f) : thePointer(reinterpret_cast<Type *>(reinterpret_cast<char *>(f) + general_tag)){};
  Type *operator->() {
    GCTOOLS_ASSERT(this->generalp());
    return untag_general(this->thePointer);
  };
  Type *operator->() const {
    GCTOOLS_ASSERT(this->generalp());
    return untag_general(this->thePointer);
  };
  Type &operator*() const {
    GCTOOLS_ASSERT(this->generalp());
    return *untag_general(this->thePointer);
  };
  bool generalp() const {
    return tagged_generalp(this->thePointer);
  }
  void reset_() {
    this->thePointer = NULL;
  }
  explicit operator bool() const {
    return this->thePointer != NULL;
  }

  template <class o_class>
  inline tagged_functor<o_class> asOrNull() {
    if (this->generalp()) {
      o_class *cast = dynamic_cast<o_class *>(untag_general<T *>(this->thePointer));
      if (cast == NULL)
        return tagged_functor<o_class>();
      tagged_functor<o_class> ret(cast);
      return ret;
    }
    THROW_HARD_ERROR(BF("Illegal tagged pointer for tagged_functor"));
    // unreachable
    tagged_functor<o_class> fail;
    return fail;
  }

  template <class o_class>
  inline tagged_functor<o_class> asOrNull() const {
    if (this->generalp()) {
      o_class *cast = dynamic_cast<o_class *>(untag_general<T *>(this->thePointer));
      if (cast == NULL)
        return tagged_functor<o_class>();
      tagged_functor<o_class> ret(cast);
      return ret;
    }
    THROW_HARD_ERROR(BF("Illegal tagged pointer for tagged_functor"));
    // unreachable
    tagged_functor<o_class> fail;
    return fail;
  }
  template <class o_class>
  inline tagged_functor<o_class> as() {
    tagged_functor<o_class> ret = this->asOrNull<o_class>();
    if (ret)
      return ret;
    THROW_HARD_ERROR(BF("Illegal cast of tagged_functor"));
  }

  template <class o_class>
  inline tagged_functor<o_class> as() const {
    tagged_functor<o_class> ret = this->asOrNull<o_class>();
    if (ret)
      return ret;
    THROW_HARD_ERROR(BF("Illegal cast of tagged_functor"));
  }
};
};
#endif

namespace gctools {
/*! Maintain tagged pointers to stretchable arrays */
template <typename T>
class tagged_pointer {
public:
  typedef T Type;
  Type *thePointer;

public:
  explicit tagged_pointer() : thePointer(NULL){};
  template <typename From>
  inline tagged_pointer(tagged_pointer<From> const &rhs) {
    if (LIKELY(rhs.generalp())) {
#if 0
        // Old way using dynamic_cast
        Type *px = dynamic_cast<Type *>(untag_general<From *>(rhs.thePointer));
        if (px) {
          this->thePointer = tag_general<Type *>(px);
          return;
        }
#else
      // New way using TaggedCast
      Type *px = TaggedCast<Type *, From *>::castOrNULL(rhs.thePointer);
      //        printf("%s:%d Trying TaggedCast in place of dynamic_cast\n", __FILE__, __LINE__ );
      if (px) {
        this->thePointer = px;
        return;
      }
#endif
      printf("%s:%d Cannot cast tagged_pointer from %s/%zu to some other type (check with debugger)\n", __FILE__, __LINE__, obj_kind_name(reinterpret_cast<core::T_O *>(rhs.thePointer)), obj_kind(reinterpret_cast<core::T_O *>(rhs.thePointer))); //% obj_name(gctools::GCKind<Type>::Kind) );
      Type *tpx = TaggedCast<Type *, From *>::castOrNULL(rhs.thePointer);
      printf("tpx = %p\n", tpx);
      THROW_HARD_ERROR(BF("Cannot cast tagged_pointer from %s/%zu to some other type (check with debugger)") % obj_kind_name(reinterpret_cast<core::T_O *>(rhs.thePointer)) % obj_kind(reinterpret_cast<core::T_O *>(rhs.thePointer))); //% obj_name(gctools::GCKind<Type>::Kind) );
    }
    THROW_HARD_ERROR(BF("Bad tag on tagged_pointer in constructor"));
  };

  explicit tagged_pointer(Type *f) : thePointer(reinterpret_cast<Type *>(reinterpret_cast<char *>(f) + general_tag)) {
    GCTOOLS_ASSERTF((f != NULL), BF("Don't initialize tagged_pointer with NULL - use the constructor with zero arguments"));
  };

  inline Type *operator->() {
    GCTOOLS_ASSERT(this->generalp());
    return untag_general(this->thePointer);
  };
  inline Type *operator->() const {
    GCTOOLS_ASSERT(this->generalp());
    return untag_general(this->thePointer);
  };
  inline Type &operator*() const {
    GCTOOLS_ASSERT(this->generalp());
    return *untag_general(this->thePointer);
  };

  template <class U>
  inline bool operator==(tagged_pointer<U> const other) const {
    return this->thePointer == other.thePointer;
  }

  template <class U>
  inline bool operator!=(tagged_pointer<U> const other) const {
    return this->thePointer != other.thePointer;
  }

  inline Type *raw_() { return this->thePointer; };
  inline Type *&rawRef_() { return this->thePointer; };
  inline bool generalp() const {
    return tagged_generalp(this->thePointer);
  }
  void reset_() {
    this->thePointer = NULL;
  }
  explicit inline operator bool() const {
    return this->thePointer != NULL;
  }

  // Should never need to convert types
  template <class o_class>
  inline tagged_pointer<o_class> asOrNull() {
    if (this->generalp()) {
      o_class *cast = dynamic_cast<o_class *>(untag_general<T *>(this->thePointer));
      if (cast == NULL)
        return tagged_pointer<o_class>();
      tagged_pointer<o_class> ret(cast);
      return ret;
    }
    THROW_HARD_ERROR(BF("Illegal tagged pointer for tagged_pointer"));
    // unreachable
    tagged_pointer<o_class> fail;
    return fail;
  }

  template <class o_class>
  inline tagged_pointer<o_class> asOrNull() const {
    if (this->generalp()) {
      o_class *cast = dynamic_cast<o_class *>(untag_general<T *>(this->thePointer));
      if (cast == NULL)
        return tagged_pointer<o_class>();
      tagged_pointer<o_class> ret(cast);
      return ret;
    }
    THROW_HARD_ERROR(BF("Illegal tagged pointer for tagged_pointer"));
    // unreachable
    tagged_pointer<o_class> fail;
    return fail;
  }
  template <class o_class>
  inline tagged_pointer<o_class> as() {
    tagged_pointer<o_class> ret = this->asOrNull<o_class>();
    if (ret)
      return ret;
    THROW_HARD_ERROR(BF("Illegal cast of tagged_pointer"));
  }

  template <class o_class>
  inline tagged_pointer<o_class> as() const {
    tagged_pointer<o_class> ret = this->asOrNull<o_class>();
    if (ret)
      return ret;
    THROW_HARD_ERROR(BF("Illegal cast of tagged_pointer"));
  }
};
};

namespace gctools {

////////////////////////////////////////////////////////////////////////
///
/// Specialize type conversions to simulate Common Lisp semantics and
/// Common Lisp type hierarchy (is that the term)
///

template <>
inline smart_ptr<core::List_V> smart_ptr<core::T_O>::asOrNull<core::List_V>() {
  if (this->consp() || this->nilp())
    return smart_ptr<core::List_V>((Tagged) this->theObject);
  return smart_ptr<core::List_V>();
};

template <>
inline smart_ptr<core::List_V> smart_ptr<core::T_O>::asOrNull<core::List_V>() const {
  if (this->consp() || this->nilp())
    return smart_ptr<core::List_V>((Tagged) this->theObject);
  return smart_ptr<core::List_V>();
};

template <>
inline smart_ptr<core::List_V> smart_ptr<core::Symbol_O>::asOrNull<core::List_V>() {
  if (this->nilp())
    return smart_ptr<core::List_V>((Tagged) this->theObject);
  return smart_ptr<core::List_V>();
};

template <>
inline smart_ptr<core::List_V> smart_ptr<core::Symbol_O>::asOrNull<core::List_V>() const {
  if (this->nilp())
    return smart_ptr<core::List_V>((Tagged) this->theObject);
  return smart_ptr<core::List_V>();
};
};

namespace gctools {
// An idea suggested by Georgiy Tugai.
// Nilable<Foo_sp> is a variable that has the type (OR NULL FOO)
// It inherits from Foo_sp

template <typename T>
class Nilable {};

template <typename T>
class Nilable<smart_ptr<T>> : public smart_ptr<T> {
public:
  typedef T Type;
  typedef smart_ptr<Type> Base;
  typedef Nilable<Base> MyType;

public:
  Nilable() : Base(make_tagged_nil<Type>()){};
  Nilable(smart_ptr<core::T_O> const &ot) {
    if (Base b = ot.asOrNull<Type>()) {
      this->theObject = b.theObject;
      return;
    } else if (tagged_nilp(ot.theObject)) {
      this->theObject = reinterpret_cast<Type *>(global_tagged_Symbol_OP_nil);
      return;
    }
    class_id expected_typ = reg::registered_class<Type>::id;
    lisp_errorBadCastFromT_O(expected_typ, reinterpret_cast<core::T_O *>(this->theObject));
  };

  /*! Construct from a smart_ptr of a compatible type */
  template <typename U>
  Nilable(smart_ptr<U> other) {
    if (other.nilp()) {
      this->theObject = reinterpret_cast<Type *>(global_tagged_Symbol_OP_nil);
      return;
    }
    this->theObject = TaggedCast<U *, Type *>::castOrNULL(other.theObject);
    if (!this->theObject) {
      lisp_errorCast<U *, Type *>(other.theObject);
    }
  }

  template <typename U>
  Nilable(Nilable<smart_ptr<U>> other) {
    if (other.nilp()) {
      this->theObject = reinterpret_cast<Type *>(global_tagged_Symbol_OP_nil);
      return;
    }
    this->theObject = TaggedCast<U *, Type *>::castOrNULL(other.theObject);
    if (!this->theObject) {
      lisp_errorCast<U *, Type *>(other.theObject);
    }
    return;
  }

  // Construct from the Base type
  Nilable(Base const &b) : Base(b){};
  inline Nilable(Base &&b) : Base(std::move(b)){};

  //Copy constructor
  Nilable(MyType const &b) : Base(b){};

  MyType &operator=(Base const &orig) {
    this->theObject = orig.theObject;
    return *this;
  }

  MyType &operator=(smart_ptr<core::T_O> const &orig) {
    if (tagged_nilp(orig.theObject)) {
      this->theObject = reinterpret_cast<Type *>(global_tagged_Symbol_OP_nil);
      return *this;
    } else if (Base foo = orig.asOrNull<Type>()) {
      this->theObject = foo.theObject;
      return *this;
    }
    class_id expected_typ = reg::registered_class<Type>::id;
    lisp_errorBadCastFromT_O(expected_typ, reinterpret_cast<core::T_O *>(this->theObject));
    THROW_HARD_ERROR(BF("Unreachable"));
  }

  bool nilp() const { return tagged_nilp(this->theObject); }
  bool notnilp() const { return !tagged_nilp(this->theObject); }

  Type *operator->() {
    GCTOOLS_ASSERT(this->notnilp());
    return untag_general(this->theObject);
  };

  const Type *operator->() const {
    GCTOOLS_ASSERT(this->notnilp());
    return untag_general(this->theObject);
  };

  const Type &operator*() const {
    GCTOOLS_ASSERT(this->notnilp());
    return *(this->untag_object());
  };

  Type &operator*() {
    GCTOOLS_ASSERT(this->notnilp());
    return *(this->untag_object());
  };

  //Type conversion operator to T_sp can be Base or NIL
  operator smart_ptr<core::T_O>() const {
    if (tagged_nilp(this->theObject)) {
      return smart_ptr<core::T_O>((Tagged)tag_nil<core::T_O *>());
    }
    return smart_ptr<core::T_O>(*this);
  }

  // Convert Nilable<Foo> to smart_ptr<Foo> - signal error if it was NIL
  operator smart_ptr<Type>() const {
    if (!tagged_nilp(this->theObject))
      return smart_ptr<Type>(*this);
    class_id this_typ = reg::registered_class<Type>::id;
    lisp_errorUnexpectedNil(this_typ);
    HARD_UNREACHABLE();
  }
};

template <>
class Nilable<smart_ptr<core::Fixnum_I>> : public smart_ptr<core::Fixnum_I> {
public:
  typedef core::Fixnum_I Type;
  typedef smart_ptr<Type> Base;
  typedef Nilable<Base> MyType;

public:
  Nilable() : Base(make_tagged_nil<Type>()){};

  Nilable(smart_ptr<core::T_O> const &ot) {
    if (ot.fixnump()) {
      this->theObject = reinterpret_cast<Type *>(ot.theObject);
      return;
    } else if (tagged_nilp(ot.theObject)) {
      this->theObject = reinterpret_cast<Type *>(global_tagged_Symbol_OP_nil);
      return;
    }
    class_id expected_typ = reg::registered_class<Type>::id;
    lisp_errorBadCastFromT_O(expected_typ, reinterpret_cast<core::T_O *>(this->theObject));
  };

  // Construct from the Base type
  Nilable(Base const &b) : Base(b){};
  inline Nilable(Base &&b) : Base(std::move(b)){};

  //Copy constructor
  Nilable(MyType const &b) : Base(b){};

  MyType &operator=(Base const &orig) {
    this->theObject = orig.theObject;
    return *this;
  }

  MyType &operator=(smart_ptr<core::T_O> const &orig) {
    if (tagged_nilp(orig.theObject)) {
      this->theObject = reinterpret_cast<Type *>(global_tagged_Symbol_OP_nil);
      return *this;
    } else if (Base foo = orig.asOrNull<Type>()) {
      this->theObject = foo.theObject;
      return *this;
    }
    class_id expected_typ = reg::registered_class<Type>::id;
    lisp_errorBadCastFromT_O(expected_typ, reinterpret_cast<core::T_O *>(this->theObject));
    THROW_HARD_ERROR(BF("Unreachable"));
  }

  bool nilp() const { return tagged_nilp(this->theObject); }
  bool notnilp() const { return !tagged_nilp(this->theObject); }

  Type *operator->() {
    GCTOOLS_ASSERT(this->notnilp());
    return untag_general(this->theObject);
  };

#if 0
        const Type* operator->() const {
            GCTOOLS_ASSERT(this->notnilp());
            return untag_general(this->theObject);
        };

        const Type& operator*() const {
            GCTOOLS_ASSERT(this->notnilp());
            return *(this->untag_object());
        };

        Type& operator*() {
            GCTOOLS_ASSERT(this->notnilp());
            return *(this->untag_object());
        };
#endif
  //Type conversion operator to T_sp can be Base or NIL
  operator smart_ptr<core::T_O>() const {
    if (tagged_nilp(this->theObject)) {
      return smart_ptr<core::T_O>((Tagged)tag_nil<core::T_O *>());
    }
    return smart_ptr<core::T_O>(*this);
  }
#if 0
	// Convert Nilable<Foo> to smart_ptr<Foo> - signal error if it was NIL
	operator smart_ptr<Type>() const {
	    if ( !tagged_nilp(this->theObject) ) return smart_ptr<Type>(*this);
	    class_id this_typ = reg::registered_class<Type>::id;
	    lisp_errorUnexpectedNil(this_typ);
	    HARD_UNREACHABLE();
	}
#endif
};
};

namespace gctools {
};
namespace gc = gctools;

namespace gctools {

#if 0
    //  T_sp <-- T_sp
    template <typename OneType>
	inline OneType As(OneType const& rhs) {
	return rhs;
    }
#endif
// List_sp <-- T_sp
template <>
inline core::List_sp As(core::T_sp const &rhs) {
  return core::List_sp(rhs);
}
};

namespace gctools {
template <typename ToType, typename FromType>
smart_ptr<ToType> reinterpret_cast_smart_ptr(smart_ptr<FromType> x) {
  return smart_ptr<ToType>((Tagged)x.raw_());
}
};

namespace core {
string _rep_(T_sp obj);
};

template <class T>
std::ostream &operator<<(std::ostream &os, const gctools::smart_ptr<T> &obj) {
  os << core::_rep_(obj);
  return os;
}

#endif
