#pragma once

// Disable this once we have List_sp working
#define USE_BAD_CAST_ERROR 1

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

#include <iostream>
#include <cstring>
#include <type_traits> // is_base_of
#include <ranges>

#ifndef SCRAPING
#ifdef USE_PRECISE_GC
#define DECLARE_FORWARDS
#include CLASP_GC_CC
#undef DECLARE_FORWARDS
#else
#define DECLARE_FORWARDS
#include INIT_CLASSES_INC_H
#undef DECLARE_FORWARDS
#endif
#endif

namespace gctools {

#if (defined(DO_ASSERT_TYPE_CAST) && !defined(SCRAPING))
template <typename Super, typename Sub> struct Inherits : std::is_base_of<Super, Sub> {};
/// Add special Inheritance info here
template <> struct Inherits<core::Number_O, ::core::SingleFloat_I> : public std::true_type {};
template <> struct Inherits<core::Real_O, ::core::SingleFloat_I> : public std::true_type {};
template <> struct Inherits<core::Float_O, ::core::SingleFloat_I> : public std::true_type {};
template <> struct Inherits<core::Number_O, ::core::Fixnum_I> : public std::true_type {};
template <> struct Inherits<core::Real_O, ::core::Fixnum_I> : public std::true_type {};
template <> struct Inherits<core::Rational_O, ::core::Fixnum_I> : public std::true_type {};
template <> struct Inherits<core::Integer_O, ::core::Fixnum_I> : public std::true_type {};

/// Stop special Inheritance
#else
template <typename T1, typename T2> struct Inherits : std::true_type {};
#endif

template <typename Super, typename Sub>
concept InheritsC = Inherits<Super, Sub>::value;
}; // namespace gctools

namespace gctools {
// I'd like to make this private or better yet anonymous,
// but I guess that's not allowed.
template <typename O, typename Index>
concept Indexable = requires(O o, Index i) { o[i]; };

template <class T> class base_ptr {
public:
  typedef T Type;
  Type* theObject;

public:
  // Default constructor, set theObject to NULL
  inline base_ptr() noexcept : theObject(NULL){};
  /*! Create a smart pointer from an existing tagged pointer */
  explicit inline base_ptr(Tagged ptr) : theObject(reinterpret_cast<Type*>(ptr)){};
  explicit inline base_ptr(Type* ptr) : theObject(ptr ? tag_general<Type*>(ptr) : NULL) {
    GCTOOLS_ASSERT((reinterpret_cast<uintptr_t>(ptr) & ptag_mask) == 0);
  };
  inline base_ptr(const return_type& rt) : theObject((Type*)rt.ret0[0]){};

  template <class From> inline base_ptr(base_ptr<From> const& rhs)
    requires InheritsC<Type, From>
    : theObject(reinterpret_cast<Type*>(rhs.theObject)) {}

  uintptr_t ptag() const { return reinterpret_cast<uintptr_t>(this->theObject) & ptag_mask; };

public:
  /*! Get the pointer typcast to an integer quantity for hashing */
  uintptr_t intptr() const { return ((uintptr_t)(this->theObject)); };

  void reset_() { this->theObject = NULL; };

  inline void swap(base_ptr<Type>& other) {
    Type* temp;
    temp = this->theObject;
    this->theObject = other.theObject;
    other.theObject = temp;
  };

  template <class o_class> inline base_ptr<o_class> asOrNull() const {
    o_class* cast = TaggedCast<o_class*, Type*>::castOrNULL(this->theObject);
    base_ptr<o_class> ret((Tagged)cast);
    return ret;
  }

  template <class o_class> inline base_ptr<o_class> as() const {
    base_ptr<o_class> ret = this->asOrNull<o_class>();
    if (ret)
      return ret;
    core::lisp_errorCast<o_class, Type>(this->theObject);
  }

  template <class o_class> inline base_ptr<o_class> as_unsafe() const {
    base_ptr<o_class> ret((Tagged)this->theObject);
    return ret;
  }

  template <class o_class> inline base_ptr<o_class> as_assert() const {
#ifdef DEBUG_ASSERT
    GCTOOLS_ASSERT((TaggedCast<o_class*, Type*>::isA(this->theObject)))
#endif
    base_ptr<o_class> ret((Tagged)this->theObject);
    return ret;
  }

  template <class o_class> inline bool isA() const { return TaggedCast<o_class*, Type*>::isA(this->theObject); }

  int number_of_values() const { return this->theObject == NULL ? 0 : 1; };

  /*! Dereferencing operator - remove the other tag */
  inline Type* operator->() const {
    GCTOOLS_ASSERT(this->theObject);
    GCTOOLS_ASSERT(this->generalp());
    return untag_general(this->theObject);
  };

  inline Type& operator*() const {
    GCTOOLS_ASSERT(this->theObject);
    GCTOOLS_ASSERT(this->objectp());
    return *(this->untag_object());
  };

  inline Type* untag_object() const { return ::gctools::untag_object(this->theObject); }

  // pass to underlying object, if it has an operator[].
  template <typename S> requires Indexable<Type, S>
  inline decltype(auto) operator[](S index) {
    return (*untag_object())[index];
  }
  template <typename S> requires Indexable<const Type, S>
  inline decltype(auto) operator[](S index) const {
    return (*untag_object())[index];
  }

  // and if it's a range, forward begin and end
  decltype(auto) begin() requires std::ranges::range<Type> {
    return untag_object()->begin();
  }
  decltype(auto) end() requires std::ranges::range<Type> {
    return untag_object()->end();
  }
  decltype(auto) begin() const requires std::ranges::range<const Type> {
    // FIXME: const overload untag_object to avoid this crap
    const Type* p = untag_object();
    return p->begin();
  }
  decltype(auto) end() const requires std::ranges::range<const Type> {
    const Type* p = untag_object();
    return p->end();
  }

  /*! If theObject!=NULL then return true */
  explicit operator bool() const { return this->theObject != NULL; };

  inline return_type as_return_type() const { return return_type(this->theObject, 1); };

  bool nilp() const { return tagged_nilp(this->theObject); }
  bool notnilp() const { return (!this->nilp()); };
  bool isTrue() const { return !this->nilp(); };
  core::Cons_O* unsafe_cons() const {
    GCTOOLS_ASSERT(this->consp());
    return reinterpret_cast<core::Cons_O*>(reinterpret_cast<uintptr_t>(this->theObject) - cons_tag);
  };

  bool objectp() const { return this->generalp() || this->consp(); };
  bool generalp() const { return tagged_generalp<Type*>(this->theObject); };
  core::General_O* unsafe_general() const {
    GCTOOLS_ASSERT(this->generalp());
    return reinterpret_cast<core::General_O*>(reinterpret_cast<uintptr_t>(this->theObject) - general_tag);
  };
  bool consp() const { return tagged_consp<Type*>(this->theObject); };
  bool unboundp() const { return tagged_unboundp(this->theObject); };
  bool boundp() const { return !tagged_unboundp(this->theObject); };
  bool no_keyp() const { return tagged_no_keyp(this->theObject); };
  bool deletedp() const { return tagged_deletedp(this->theObject); };
  bool same_as_keyP() const { return tagged_same_as_keyp(this->theObject); };
  bool fixnump() const { return tagged_fixnump(this->theObject); };
  Fixnum unsafe_fixnum() const { return untag_fixnum(this->theObject); };
  bool characterp() const { return tagged_characterp<Type*>(this->theObject); };
  claspCharacter unsafe_character() const { return untag_character(this->theObject); };
  bool single_floatp() const { return tagged_single_floatp<Type*>(this->theObject); };
  float unsafe_single_float() const { return untag_single_float<Type*>(this->theObject); };
#ifdef CLASP_SHORT_FLOAT
  bool short_floatp() const { return tagged_short_floatp<Type*>(this->theObject); };
  short_float_t unsafe_short_float() const { return untag_short_float<Type*>(this->theObject); };
#endif
  Fixnum asFixnum() const {
    GCTOOLS_ASSERT(this->fixnump());
    return untag_fixnum<Type*>(this->theObject);
  };

  /*! Return the raw base_ptr value interpreted as a T_O* */
  inline core::T_O* raw_() const { return reinterpret_cast<core::T_O*>(this->theObject); }
  inline gctools::Tagged tagged_() const { return reinterpret_cast<gctools::Tagged>(this->theObject); }

  inline void setRaw_(Tagged p) { this->theObject = reinterpret_cast<Type*>(p); }

  /*! This should almost NEVER be used!!!!!!
          The only reason to ever use this is when theObject will be set to NULL
          and you are sure that it will not be interpreted as a Fixnum!!!

          List actual uses here:
          intrinsics.cc>>cc_loadTimeValueReference
        */
  Type*& rawRef_() { return this->theObject; };

  /*! Check if this tagged theObject matches the templated type.
          The most common case is this is an object.*/
  bool valid() const {
    GCTOOLS_ASSERT(false); // BF("Implement me"));
  }

  template <class U> inline bool operator==(const base_ptr<U>& other) const {
    return this->theObject == other.theObject;
  }

  template <class U> inline bool operator!=(const base_ptr<U>& other) const {
    return this->theObject != other.theObject;
  }
};
}; // namespace gctools

namespace gctools {
template <typename Type> class smart_ptr : public base_ptr<Type> {
public:
  // Default constructor, set theObject to NULL
  inline smart_ptr() noexcept : base_ptr<Type>((Type*)NULL){};
  /*! Create a smart pointer from an existing tagged pointer */
  explicit inline smart_ptr(Tagged ptr) : base_ptr<Type>(ptr){};
  explicit inline smart_ptr(Type* ptr) : base_ptr<Type>(ptr){};
  inline smart_ptr(const return_type& rt) : base_ptr<Type>(rt){};
  inline smart_ptr(base_ptr<Type> orig) : base_ptr<Type>((Tagged)orig.raw_()){};

  template <class From> inline smart_ptr(smart_ptr<From> const& rhs)
    requires InheritsC<Type, From>
    : base_ptr<Type>((Tagged)rhs.raw_()) {}
};

}; // namespace gctools

namespace gctools {
template <typename Type> inline static smart_ptr<Type> make_tagged_fixnum(Fixnum val) {
  return smart_ptr<Type>((Tagged)tag_fixnum<Type*>(val));
}
template <typename Type> inline static smart_ptr<Type> make_tagged_single_float(float val) {
  return smart_ptr<Type>((Tagged)tag_single_float<Type*>(val));
}

inline static smart_ptr<core::Character_I> make_tagged_character(claspCharacter val) {
  return smart_ptr<core::Character_I>((Tagged)tag_character<core::Character_I*>(val));
};

template <typename Type> inline static smart_ptr<Type> make_tagged_other(Type* p) { return smart_ptr<Type>(p); }
template <typename Type> inline static smart_ptr<Type> make_tagged_nil() {
  return smart_ptr<Type>((Tagged)global_tagged_Symbol_OP_nil);
};
template <typename Type> inline static smart_ptr<Type> make_tagged_unbound() { return smart_ptr<Type>(tag_unbound<Tagged>()); };
template <typename Type> inline static smart_ptr<Type> make_tagged_no_thread_local_binding() {
  return smart_ptr<Type>(tag_no_thread_local_binding<Tagged>());
};
template <typename Type> inline static smart_ptr<Type> make_tagged_no_key() { return smart_ptr<Type>(tag_no_key<Tagged>()); };
template <typename Type> inline static smart_ptr<Type> make_tagged_deleted() { return smart_ptr<Type>(tag_deleted<Tagged>()); };
template <typename Type> inline static smart_ptr<Type> make_tagged_same_as_key() {
  return smart_ptr<Type>(tag_same_as_key<Tagged>());
};
}; // namespace gctools

namespace core {

class List_V {};     // Virtual class representing Common Lisp LIST
class Sequence_V {}; // Virtual class representing SEQUENCE
typedef gctools::smart_ptr<T_O> T_sp;
typedef gctools::smart_ptr<Sequence_V> Sequence_sp;
typedef gctools::smart_ptr<List_V> List_sp;

extern gctools::smart_ptr<T_O> cons_car(Cons_O* cur);
extern gctools::smart_ptr<T_O> cons_cdr(Cons_O* cur);
}; // namespace core

namespace gctools {
//////////////////////////////////////////////////////////////////////
//
// Declare As converters
//
template <typename To_SP> inline bool IsA(return_type const& rhs) {
  return TaggedCast<typename To_SP::Type*, typename core::T_O*>::isA(reinterpret_cast<core::T_O*>(rhs.ret0[0]));
};
template <typename To_SP, typename From_SP> inline bool IsA(From_SP const& rhs) {
  return TaggedCast<typename To_SP::Type*, typename From_SP::Type*>::isA(reinterpret_cast<typename From_SP::Type*>(rhs.raw_()));
}

template <typename To_SP, typename From_SP> inline To_SP As(From_SP const& rhs) {
  if (IsA<To_SP>(rhs)) {
    To_SP ret((Tagged)rhs.raw_());
    return ret;
  }
  // If the cast didn't work then signal a type error
  gctools::GCStampEnum expectedStampWtag = gctools::GCStamp<typename To_SP::Type>::StampWtag;
  lisp_errorBadCastStampWtag((size_t)expectedStampWtag, rhs.raw_());
  HARD_UNREACHABLE();
}
template <typename To_SP> inline To_SP As(const return_type& rhs) {
  GCTOOLS_ASSERT(rhs.nvals == 1);
  if (IsA<To_SP>(rhs)) {
    To_SP ret((Tagged)rhs.ret0[0]);
    return ret;
  }
  class_id expected_typ = reg::registered_class<typename To_SP::Type>::id;
  lisp_errorBadCastFromT_O(expected_typ, reinterpret_cast<core::T_O*>(rhs.ret0[0]));
  HARD_UNREACHABLE();
}

// Cast the type without any concern if it is appropriate
template <typename To_SP, typename From_SP> inline To_SP As_unsafe(From_SP const& rhs) {
  To_SP ret((Tagged)rhs.raw_());
  return ret;
}

// Cast the type without any concern if it is appropriate
// If DEBUG_ASSERT then check if the type is appropriate.
template <typename To_SP, typename From_SP> inline To_SP As_assert(From_SP const& rhs) {
#ifdef DEBUG_ASSERT
  if (!gctools::IsA<To_SP>(rhs)) {
    throw_hard_error_cast_failed(typeid(To_SP).name(), typeid(From_SP).name());
  }
#endif
  To_SP ret((Tagged)rhs.raw_());
  return ret;
}

}; // namespace gctools

namespace gctools {
template <> class smart_ptr<core::T_O> {
public:
  typedef core::T_O Type;
  Type* theObject;

public:
  // Default constructor, set theObject to NULL
  smart_ptr() noexcept : theObject((Type*)NULL){};
  explicit inline smart_ptr(Type* ptr) : theObject(ptr){};
  /*! Create a smart pointer from an existing tagged pointer */
  explicit inline smart_ptr(Tagged ptr) : theObject((Type*)ptr){};
  inline smart_ptr(const return_type& rt) : theObject((Type*)rt.ret0[0]){};
  template <class From> inline smart_ptr(smart_ptr<From> const& rhs) : theObject((Type*)rhs.theObject){};

  inline return_type as_return_type() const { return return_type(this->theObject, 1); };

  template <class o_class> inline smart_ptr<o_class> asOrNull() const {
    return smart_ptr<o_class>((Tagged)TaggedCast<o_class*, Type*>::castOrNULL(this->theObject));
  }

  template <class o_class> inline smart_ptr<o_class> as() const {
    smart_ptr<o_class> ret = this->asOrNull<o_class>();
    if (ret)
      return ret;
    core::lisp_errorCast<o_class, Type>(this->theObject);
  }

  template <class o_class> inline smart_ptr<o_class> as_unsafe() const {
    smart_ptr<o_class> ret((Tagged)this->theObject);
    return ret;
  }

  template <class o_class> inline smart_ptr<o_class> as_assert() const {
#ifdef DEBUG_ASSERT
    GCTOOLS_ASSERT((TaggedCast<o_class*, Type*>::isA(this->theObject)))
#endif
    smart_ptr<o_class> ret((Tagged)this->theObject);
    return ret;
  }

  template <class o_class> inline bool isA() const {
    smart_ptr<o_class> ret = this->asOrNull<o_class>();
    return ((bool)ret);
  }

public:
  inline explicit operator bool() const { return this->theObject != NULL; };
  inline Type* untag_object() const { return ::gctools::untag_object(this->theObject); }
  /*! Dereferencing operator - remove the other tag */
  inline Type* operator->() const {
    GCTOOLS_ASSERT(this->theObject);
    return this->untag_object();
  };
  inline Type& operator*() const {
    GCTOOLS_ASSERT(this->theObject);
    return *this->untag_object();
  };
  /*! This should almost NEVER be used!!!!!!

          List all uses of rawRef_ here:
          intrinsics.cc>>cc_loadTimeValueReference
          record.h>>field specialized on gc::smart_ptr<OT>&
          SMART_PTR_FIX and smart_ptr fixing in general when SMART_PTR_FIX is replaced
                  with a direct call to the fixing template function
        */
  Type*& rawRef_() { return this->theObject; };
  inline void setRaw_(Tagged p) { this->theObject = reinterpret_cast<core::T_O*>(p); }
  void reset_() { this->theObject = NULL; };

public:
  /*! Get the pointer typcast to an integer quantity for hashing */
  uintptr_t intptr() const { return ((uintptr_t)(this->theObject)); };
  int number_of_values() const { return this->theObject == NULL ? 0 : 1; };
  bool unboundp() const { return tagged_unboundp(this->theObject); };
  bool boundp() const { return !tagged_unboundp(this->theObject); };
  bool no_keyp() const { return tagged_no_keyp(this->theObject); };
  bool deletedp() const { return tagged_deletedp(this->theObject); };
  bool same_as_keyP() const { return tagged_same_as_keyp(this->theObject); };
  inline bool nilp() const { return tagged_nilp(this->theObject); }
  inline bool notnilp() const { return (!this->nilp()); };
  bool isTrue() const { return !this->nilp(); };
  inline bool fixnump() const { return tagged_fixnump(this->theObject); };
  bool characterp() const { return tagged_characterp<Type*>(this->theObject); };
  claspCharacter unsafe_character() const { return untag_character(this->theObject); };
  inline bool generalp() const { return tagged_generalp(this->theObject); };
  inline bool consp() const { return tagged_consp(this->theObject); };
  inline bool objectp() const { return this->generalp() || this->consp(); };
  inline Fixnum unsafe_fixnum() const { return untag_fixnum(this->theObject); };
  bool single_floatp() const { return tagged_single_floatp<Type*>(this->theObject); };
  float unsafe_single_float() const { return untag_single_float<Type*>(this->theObject); };
#ifdef CLASP_SHORT_FLOAT
  bool short_floatp() const { return tagged_short_floatp<Type*>(this->theObject); };
  float unsafe_short_float() const { return untag_short_float<Type*>(this->theObject); };
#endif
  bool valistp() const { return tagged_vaslistp(this->theObject); };
  void* unsafe_valist() const { return untag_vaslist(this->theObject); };
  void* safe_valist() const {
    GCTOOLS_ASSERT(this->valistp());
    return this->unsafe_valist();
  };

  inline core::T_O* raw_() const { return reinterpret_cast<core::T_O*>(this->theObject); };
  inline gctools::Tagged tagged_() const { return reinterpret_cast<gctools::Tagged>(this->theObject); }
  inline core::Cons_O* unsafe_cons() const {
    GCTOOLS_ASSERT(this->consp());
    return untag_cons(reinterpret_cast<core::Cons_O*>(this->theObject));
  };
  core::General_O* unsafe_general() const {
    GCTOOLS_ASSERT(this->generalp());
    return untag_general(reinterpret_cast<core::General_O*>(this->theObject));
  };
  template <class U> inline bool operator==(smart_ptr<U> const other) const { return this->theObject == other.theObject; }

  template <class U> inline bool operator!=(smart_ptr<U> const other) const { return this->theObject != other.theObject; }
};
/* Smart pointers should be trivial so they can be passed/returned
 * in registers easily. But the default constructor is nontrivial,
 * so we're merely trivially copyable.
 */
static_assert(std::is_trivially_copyable_v<core::T_sp>);
}; // namespace gctools

namespace gctools {
template <> class smart_ptr<core::Fixnum_I> {
public:
  typedef core::Fixnum_I Type;
  Type* theObject;

public:
  // Default constructor, set theObject to NULL
  smart_ptr() noexcept : theObject(NULL){};

  smart_ptr(Type* fn) : theObject(fn){};
  template <typename From> inline smart_ptr(smart_ptr<From> const& rhs) {
    if (rhs.fixnump()) {
      this->theObject = reinterpret_cast<Type*>(rhs.raw_());
      return;
    }
    class_id from_typ = reg::registered_class<From>::id;
    lisp_errorBadCastToFixnum(from_typ, rhs.raw_());
  }
  /*! Constructor that takes Tagged assumes that the pointer is tagged.
          Any ptr passed to this constructor must have the CONS tag.
        */
  explicit inline smart_ptr(Tagged ptr) : theObject(reinterpret_cast<Type*>(ptr)) {
    GCTOOLS_ASSERT(tagged_fixnump<Type*>(reinterpret_cast<Type*>(ptr)));
  };

public:
  inline explicit operator bool() const { return this->theObject != NULL; };
  inline operator smart_ptr<core::T_O>() const { return smart_ptr<core::T_O>((Tagged)this->theObject); };

public:
  inline return_type as_return_type() const { return return_type(this->theObject, 1); };
  inline bool unboundp() const { return tagged_unboundp(this->theObject); };
  inline bool boundp() const { return !tagged_unboundp(this->theObject); };
  inline bool nilp() const { return tagged_nilp(this->theObject); }
  inline bool notnilp() const { return (!this->nilp()); };
  inline bool fixnump() const { return tagged_fixnump(this->theObject); };
  inline bool generalp() const { return tagged_generalp(this->theObject); };
  inline bool consp() const { return tagged_consp(this->theObject); };
  inline bool objectp() const { return this->generalp() || this->consp(); };
  inline Fixnum unsafe_fixnum() const { return untag_fixnum(this->theObject); };
  inline core::T_O* raw_() const { return reinterpret_cast<core::T_O*>(this->theObject); };
  inline gctools::Tagged tagged_() const { return reinterpret_cast<gctools::Tagged>(this->theObject); }
};
}; // namespace gctools

namespace core {
typedef gctools::smart_ptr<Fixnum_I> Fixnum_sp;
typedef gctools::smart_ptr<SingleFloat_I> SingleFloat_sp;
typedef gctools::smart_ptr<Character_I> Character_sp;
}; // namespace core

namespace gctools {
template <> class smart_ptr<core::Symbol_O> : public base_ptr<core::Symbol_O> {
public:
  // Default constructor, set theObject to NULL
  smart_ptr() noexcept : base_ptr<core::Symbol_O>(tag_unbound<Tagged>()){};

  /*! Create a smart pointer from an existing tagged pointer */
  explicit inline smart_ptr(Tagged ptr) : base_ptr((Tagged)ptr){};
  explicit inline smart_ptr(Type* ptr) : base_ptr((Type*)ptr){};
  inline smart_ptr(base_ptr<core::Symbol_O> orig) : base_ptr<core::Symbol_O>((Tagged)orig.raw_()){};

  template <class From> inline smart_ptr(smart_ptr<From> const& rhs) {
    if (LIKELY(rhs.objectp())) {
      Type* px = TaggedCast<Type*, From*>::castOrNULL(rhs.theObject);
      if (px == 0) {
        throw_hard_error_cast_failed(typeid(Type*).name(), typeid(From*).name());
      }
      this->theObject = px;
    } else {
      this->theObject = reinterpret_cast<Type*>(rhs.theObject);
    }
  }

  // specialized for List_V below.
  template <class o_class> inline smart_ptr<o_class> asOrNull() const {
    return this->base_ptr<Type>::asOrNull<o_class>();
  }

  // We need these because C++ gets stupid with implicit conversions for
  // templated functions.
  template <class o_class> inline smart_ptr<o_class> as() const {
    return this->base_ptr<Type>::as<o_class>();
  }
  template <class o_class> inline smart_ptr<o_class> as_unsafe() const {
    return this->base_ptr<Type>::as_unsafe<o_class>();
  }
  template <class o_class> inline smart_ptr<o_class> as_assert() const {
    return this->base_ptr<Type>::as_assert<o_class>();
  }
  template <class U> inline bool operator==(const smart_ptr<U>& other) const {
    // i don't think there's any simple way to call the base function. bleh.
    return this->theObject == other.theObject;
  }

  template <class U> inline bool operator!=(const smart_ptr<U>& other) const {
    return this->theObject != other.theObject;
  }
};
}; // namespace gctools

namespace cl {
extern gctools::smart_ptr<core::Symbol_O>& _sym_list;
extern gctools::smart_ptr<core::Symbol_O>& _sym_type_error;
} // namespace cl

namespace kw {
extern gctools::smart_ptr<core::Symbol_O>& _sym_datum;
extern gctools::smart_ptr<core::Symbol_O>& _sym_expected_type;
} // namespace kw

namespace gctools {

template <> class smart_ptr<core::Cons_O> {
public:
  typedef core::Cons_O Type;
  core::Cons_O* theObject;

public:
  //! The default constructor returns an invalid smart_ptr
  smart_ptr() noexcept : theObject(NULL){};
  // Constructor that takes Cons_O* assumes its untagged
  explicit inline smart_ptr(core::Cons_O* ptr) : theObject(ptr ? tag_cons<core::Cons_O*>(ptr) : NULL) {
    GCTOOLS_ASSERT((reinterpret_cast<uintptr_t>(ptr) & ptag_mask) == 0);
  };
  /*! Constructor that takes Tagged assumes that the pointer is tagged.
          Any ptr passed to this constructor must have the CONS tag.
        */
  explicit inline smart_ptr(Tagged ptr) : theObject(reinterpret_cast<core::Cons_O*>(ptr)) {
    GCTOOLS_ASSERT(!ptr || tagged_consp<core::Cons_O*>(reinterpret_cast<core::Cons_O*>(ptr)));
  };

public:
  explicit operator bool() const { return this->theObject != NULL; }

public:
  void reset_() { this->theObject = NULL; };
  inline bool generalp() const { return false; };
  inline bool objectp() const { return this->consp(); };
  inline bool isTrue() const { return true; };
  inline bool consp() const { return tagged_consp(this->theObject); };
  inline bool valid() const { return this->consp(); } // || this->nilp(); };
  inline bool unboundp() const { return tagged_unboundp(this->theObject); };
  bool boundp() const { return !tagged_unboundp(this->theObject); };
  inline Type*& rawRef_() { return this->theObject; };
  inline core::Cons_O* unsafe_cons() const {
    GCTOOLS_ASSERT(this->consp());
    return reinterpret_cast<core::Cons_O*>(reinterpret_cast<uintptr_t>(this->theObject) - cons_tag);
  };

  inline void setRaw_(Tagged p) { this->theObject = reinterpret_cast<Type*>(p); }

  operator smart_ptr<core::T_O>() const {
    return smart_ptr<core::T_O>((Tagged) const_cast<core::T_O* const>(reinterpret_cast<core::T_O*>(this->theObject)));
  };

  inline core::Cons_O* untag_object() const { return this->unsafe_cons(); }

  inline void swap(smart_ptr<core::Cons_O>& other) {
    core::Cons_O* temp;
    temp = this->theObject;
    this->theObject = other.theObject;
    other.theObject = temp;
  }

  /*! Dereferencing operator - remove the other tag */
  inline core::Cons_O* operator->() const {
    GCTOOLS_ASSERT(this->consp());
    return this->unsafe_cons();
  };

  inline core::Cons_O& operator*() {
    GCTOOLS_ASSERT(this->consp());
    return *(this->unsafe_cons());
  };

  inline core::T_O* raw_() const { return reinterpret_cast<core::T_O*>(this->theObject); }
  inline gctools::Tagged tagged_() const { return reinterpret_cast<gctools::Tagged>(this->theObject); }

  template <class o_class> inline smart_ptr<o_class> asOrNull() const {
    o_class* cast = TaggedCast<o_class*, Type*>::castOrNULL(this->theObject);
    smart_ptr<o_class> ret((Tagged)cast);
    return ret;
  }
  template <class o_class> inline smart_ptr<o_class> as() const {
    smart_ptr<o_class> ret = this->asOrNull<o_class>();
    if (ret)
      return ret;
    core::lisp_errorCast<o_class, Type>(this->theObject);
  }
  template <class o_class> inline smart_ptr<o_class> as_unsafe() const {
    smart_ptr<o_class> ret((Tagged)this->theObject);
    return ret;
  }
  template <class o_class> inline smart_ptr<o_class> as_assert() const {
#ifdef DEBUG_ASSERT
    GCTOOLS_ASSERT((TaggedCast<o_class*, Type*>::isA(this->theObject)))
#endif
    smart_ptr<o_class> ret((Tagged)this->theObject);
    return ret;
  }
  template <class o_class> inline bool isA() const { return TaggedCast<o_class*, Type*>::isA(this->theObject); }


  template <class U> inline bool operator==(smart_ptr<U> const other) const { return this->theObject == other.theObject; }

  template <class U> inline bool operator!=(smart_ptr<U> const other) const { return this->theObject != other.theObject; }
};
}; // namespace gctools

/*! List_sp implementation with iterator by Georgiy Tugai  April 27, 2015
 */

namespace gctools {
class List_sp_iterator;

template <> class smart_ptr<core::List_V> {
public:
  typedef core::T_O Type; // The best common type for both Cons_O and Symbol_O is T_O
  Type* theObject;

public:
  inline void setRaw_(Tagged p) { this->theObject = reinterpret_cast<Type*>(p); }
  inline Type*& rawRef_() { return this->theObject; };

public:
  //! The default constructor returns an invalid smart_ptr
  inline smart_ptr() noexcept : theObject(NULL){};
  inline smart_ptr(const return_type& rt) : theObject((Type*)rt.ret0[0]){};
  inline smart_ptr(smart_ptr<core::T_O> other) {
    // Null_O does not exist, so we have to do some shenanigans here.
    if (other.consp() || other.nilp()) [[likely]]
      this->theObject = other.theObject;
    else [[unlikely]]
      core::lisp_errorExpectedList(other.theObject);
  }
  inline smart_ptr(smart_ptr<core::Cons_O> other) : theObject(other.raw_()) { GCTOOLS_ASSERT(other.consp()); };
  // Constructor that takes Cons_O* assumes its untagged
  explicit inline smart_ptr(core::Cons_O* ptr) : theObject(tag_cons<Type*>(reinterpret_cast<core::T_O*>(ptr))) {
    GCTOOLS_ASSERT((reinterpret_cast<uintptr_t>(ptr) & ptag_mask) == 0);
  };
  explicit inline smart_ptr(core::Symbol_O* ptr) : theObject(tag_general<Type*>(reinterpret_cast<core::T_O*>(ptr))) {
    GCTOOLS_ASSERT((reinterpret_cast<uintptr_t>(ptr) & ptag_mask) == 0);
  };
  /*! Constructor that takes Tagged assumes that the pointer is tagged.
          Any ptr passed to this constructor must have the CONS tag.
        */
  explicit inline smart_ptr(Tagged ptr) : theObject(reinterpret_cast<Type*>(ptr)) {
    GCTOOLS_ASSERT(!ptr || tagged_consp<Type*>(reinterpret_cast<Type*>(ptr)) || tagged_nilp<Type*>(reinterpret_cast<Type*>(ptr)));
  };

public:
  explicit operator bool() const { return this->theObject != NULL; }

public:
  void reset_() { this->theObject = NULL; };
  inline bool generalp() const { return tagged_generalp<Type*>(this->theObject); };
  inline bool consp() const { return tagged_consp(this->theObject); };
  inline core::Cons_O* unsafe_cons() const {
    GCTOOLS_ASSERT(this->consp());
    return reinterpret_cast<core::Cons_O*>(reinterpret_cast<uintptr_t>(this->theObject) - cons_tag);
  };
  inline bool objectp() const { return this->generalp() || this->consp(); };
  inline return_type as_return_type() const { return return_type(this->theObject, 1); };
  inline bool nilp() const { return tagged_nilp(this->theObject); };
  inline bool notnilp() const { return !this->nilp(); };
  inline bool isTrue() const { return !this->nilp(); };
  inline bool unboundp() const { return tagged_unboundp(this->theObject); };
  inline bool boundp() const { return !tagged_unboundp(this->theObject); };

  //	inline operator smart_ptr<core::Cons_O>() const { GCTOOLS_ASSERT(this->consp());return
  // smart_ptr<core::Cons_O>((Tagged)this->theObject); };
  inline smart_ptr<core::Cons_O> asCons() const {
    GCTOOLS_ASSERT(this->consp());
    return smart_ptr<core::Cons_O>((Tagged)this->theObject);
  };

  inline bool valid() const { return this->consp() || this->nilp(); };

  operator smart_ptr<core::T_O>() const {
    return smart_ptr<Type>((Tagged) const_cast<Type* const>(reinterpret_cast<Type*>(this->theObject)));
  };

  Type* untag_object() const { return ::gctools::untag_object(this->theObject); }

  inline void swap(smart_ptr<core::List_V>& other) {
    Type* temp;
    temp = this->theObject;
    this->theObject = other.theObject;
    other.theObject = temp;
  }

  /*! Dereferencing operator - remove the other tag */
  inline Type* operator->() const {
    GCTOOLS_ASSERT(this->objectp());
    return this->untag_object();
  };

  inline Type& operator*() {
    GCTOOLS_ASSERT(this->objectp());
    return *(this->untag_object());
  };
  inline const Type& operator*() const {
    GCTOOLS_ASSERT(this->objectp());
    return *(this->untag_object());
  };

  core::T_O* raw_() const { return reinterpret_cast<Type*>(this->theObject); }
  inline gctools::Tagged tagged_() const { return reinterpret_cast<gctools::Tagged>(this->theObject); }
  
  template <class o_class> inline smart_ptr<o_class> asOrNull() const {
    o_class* cast = TaggedCast<o_class*, Type*>::castOrNULL(this->theObject);
    smart_ptr<o_class> ret((Tagged)cast);
    return ret;
  }
  template <class o_class> inline smart_ptr<o_class> as() const {
    smart_ptr<o_class> ret = this->asOrNull<o_class>();
    if (ret)
      return ret;
    core::lisp_errorCast<o_class, Type>(this->theObject);
  }
  template <class o_class> inline smart_ptr<o_class> as_unsafe() const {
    smart_ptr<o_class> ret((Tagged)this->theObject);
    return ret;
  }
  template <class o_class> inline smart_ptr<o_class> as_assert() const {
#ifdef DEBUG_ASSERT
    GCTOOLS_ASSERT((TaggedCast<o_class*, Type*>::isA(this->theObject)))
#endif
    smart_ptr<o_class> ret((Tagged)this->theObject);
    return ret;
  }
  template <class o_class> inline bool isA() const { return TaggedCast<o_class*, Type*>::isA(this->theObject); }

  template <class U> inline bool operator==(smart_ptr<U> const other) const { return this->theObject == other.theObject; }

  template <class U> inline bool operator!=(smart_ptr<U> const other) const { return this->theObject != other.theObject; }

private:
  class List_sp_iterator {
  public:
    List_sp_iterator() : ptr() {}
    List_sp_iterator(const core::List_sp& other)
      : ptr(other.consp() ? other.unsafe_cons() : NULL){};
    List_sp_iterator& operator++() {
      // iterating past the end will fault.
      smart_ptr<core::T_O> next = cons_cdr(ptr.unsafe_cons());
      if (next.consp()) [[likely]]
        ptr = next.as_unsafe<core::Cons_O>();
      else if (next.nilp())
        ptr = smart_ptr<core::Cons_O>();
      else [[unlikely]]
        core::lisp_errorExpectedList(next.raw_());
      return *this;
    }
    List_sp_iterator& operator++(int) { // postfix
      auto clone = new List_sp_iterator(*this);
      ++*this;
      return *clone;
    }
    inline const smart_ptr<core::Cons_O>& operator*() const { return ptr; }
    inline smart_ptr<core::Cons_O>& operator*() { return ptr; }
    inline bool operator==(const List_sp_iterator& other) const {
      return ptr.theObject == other.ptr.theObject;
    }
    inline bool operator!=(const List_sp_iterator& other) const {
      return ptr.theObject != other.ptr.theObject;
    }
  public:
    // either a real cons, or invalid (theObject = NULL) at end() and farther
    smart_ptr<core::Cons_O> ptr;
  };

public:
  typedef List_sp_iterator iterator;

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

public:

  smart_ptr<core::List_V>& operator=(const smart_ptr<core::List_V>& other) {
    this->theObject = other.theObject;
    return *this;
  };

  template <typename From> smart_ptr<core::List_V>& operator=(const smart_ptr<From>& other) {
    GCTOOLS_ASSERT(tagged_consp<From*>(other.theObject) || tagged_nilp<From*>(other.theObject));
    this->theObject = other.theObject;
    return *this;
  };
};
}; // namespace gctools

template <class T> gctools::smart_ptr<T> nil() {
  gctools::smart_ptr<T> x((gctools::Tagged)gctools::tag_nil<T*>());
  return x;
}

template <class T> gctools::smart_ptr<T> unbound() {
  gctools::smart_ptr<T> x((gctools::Tagged)gctools::tag_unbound<T*>());
  return x;
}

template <class T> gctools::smart_ptr<T> no_key() {
  gctools::smart_ptr<T> x((gctools::Tagged)gctools::tag_no_key<T*>());
  return x;
}

template <class T> gctools::smart_ptr<T> no_thread_local_binding() {
  gctools::smart_ptr<T> x((gctools::Tagged)gctools::tag_no_thread_local_binding<T*>());
  return x;
}

template <class T> gctools::smart_ptr<T> deleted() {
  gctools::smart_ptr<T> x((gctools::Tagged)gctools::tag_deleted<T*>());
  return x;
}

namespace core {
using gctools::Fixnum;
};

#ifdef TAGGED_POINTER
namespace gctools {
/*! Maintain tagged pointers to stretchable arrays */
template <typename T> class tagged_pointer {
public:
  typedef T Type;
  Type* thePointer;

public:
  explicit tagged_pointer() : thePointer(NULL){};
  template <typename From> inline tagged_pointer(tagged_pointer<From> const& rhs) {
    if (LIKELY(rhs.generalp())) {
      // New way using TaggedCast
      Type* px = TaggedCast<Type*, From*>::castOrNULL(rhs.thePointer);
      //        printf("%s:%d Trying TaggedCast in place of dynamic_cast\n", __FILE__, __LINE__ );
      if (px) {
        this->thePointer = px;
        return;
      }
      printf("%s:%d Cannot cast tagged_pointer from %s/%zu to some other type (check with debugger)\n", __FILE__, __LINE__,
             obj_kind_name(reinterpret_cast<core::T_O*>(rhs.thePointer)),
             obj_kind(reinterpret_cast<core::T_O*>(rhs.thePointer))); //% obj_name(gctools::GCStamp<Type>::Stamp) );
      Type* tpx = TaggedCast<Type*, From*>::castOrNULL(rhs.thePointer);
      printf("tpx = %p\n", tpx);
      throw_hard_error_cannot_cast_tagged_pointer(obj_kind_name(reinterpret_cast<core::T_O*>(rhs.thePointer)),
                                                  obj_kind(reinterpret_cast<core::T_O*>(rhs.thePointer)));
    }
    throw_hard_error("Bad tag on tagged_pointer in constructor");
  };

  explicit tagged_pointer(Type* f) : thePointer(reinterpret_cast<Type*>(reinterpret_cast<char*>(f) + general_tag)) {
    GCTOOLS_ASSERTF((f != NULL), "Don't initialize tagged_pointer with NULL - use the constructor with zero arguments");
  };

  inline Type* operator->() {
    GCTOOLS_ASSERT(this->generalp());
    return untag_general(this->thePointer);
  };
  inline Type* operator->() const {
    GCTOOLS_ASSERT(this->generalp());
    return untag_general(this->thePointer);
  };
  inline Type& operator*() const {
    GCTOOLS_ASSERT(this->generalp());
    return *untag_general(this->thePointer);
  };

  tagged_pointer<T>& operator=(tagged_pointer<T> const& orig) {
    this->thePointer = orig.thePointer;
    return *this;
  }

  template <class U> inline bool operator==(tagged_pointer<U> const other) const { return this->thePointer == other.thePointer; }

  template <class U> inline bool operator!=(tagged_pointer<U> const other) const { return this->thePointer != other.thePointer; }

  inline Type* raw_() { return this->thePointer; };
  inline Type*& rawRef_() { return this->thePointer; };
  inline bool generalp() const { return tagged_generalp(this->thePointer); }
  void reset_() { this->thePointer = NULL; }
  explicit inline operator bool() const { return this->thePointer != NULL; }

  // Should never need to convert types

  template <class o_class> inline tagged_pointer<o_class> asOrNull() const {
    if (this->generalp()) {
      o_class* cast = dynamic_cast<o_class*>(untag_general<T*>(this->thePointer));
      if (cast == NULL)
        return tagged_pointer<o_class>();
      tagged_pointer<o_class> ret(cast);
      return ret;
    }
    throw_hard_error("Illegal tagged pointer for tagged_pointer");
    // unreachable
    tagged_pointer<o_class> fail;
    return fail;
  }

  template <class o_class> inline tagged_pointer<o_class> as() const {
    tagged_pointer<o_class> ret = this->asOrNull<o_class>();
    if (ret)
      return ret;
    throw_hard_error("Illegal cast of tagged_pointer");
  }
};
};     // namespace gctools
#endif // end TAGGED_POINTER

namespace gctools {

////////////////////////////////////////////////////////////////////////
///
/// Specialize type conversions to simulate Common Lisp semantics and
/// Common Lisp type hierarchy (is that the term)
///

template <> inline smart_ptr<core::List_V> smart_ptr<core::T_O>::asOrNull<core::List_V>() const {
  if (this->consp() || this->nilp())
    return smart_ptr<core::List_V>((Tagged)this->theObject);
  return smart_ptr<core::List_V>();
};

template <> inline smart_ptr<core::List_V> smart_ptr<core::Symbol_O>::asOrNull<core::List_V>() const {
  if (this->nilp())
    return smart_ptr<core::List_V>((Tagged)this->theObject);
  return smart_ptr<core::List_V>();
};
}; // namespace gctools

namespace gctools {
// An idea suggested by Georgiy Tugai.
// Nilable<Foo_sp> is a variable that has the type (OR NULL FOO)
// It inherits from Foo_sp

template <typename T> class Nilable {};

template <typename T> class Nilable<smart_ptr<T>> : public smart_ptr<T> {
public:
  typedef T Type;
  typedef smart_ptr<Type> Base;
  typedef Nilable<Base> MyType;

public:
  Nilable() : Base(make_tagged_nil<Type>()){};
  Nilable(smart_ptr<core::T_O> const& ot) {
    if (Base b = ot.asOrNull<Type>()) {
      this->theObject = b.theObject;
      return;
    } else if (tagged_nilp(ot.theObject)) {
      this->theObject = reinterpret_cast<Type*>(global_tagged_Symbol_OP_nil);
      return;
    }
    class_id expected_typ = reg::registered_class<Type>::id;
    lisp_errorBadCastFromT_O(expected_typ, reinterpret_cast<core::T_O*>(this->theObject));
  };

  /*! Construct from a smart_ptr of a compatible type */
  template <typename U> Nilable(smart_ptr<U> other) {
    if (other.nilp()) {
      this->theObject = reinterpret_cast<Type*>(global_tagged_Symbol_OP_nil);
      return;
    }
    this->theObject = TaggedCast<U*, Type*>::castOrNULL(other.theObject);
    if (!this->theObject) {
      core::lisp_errorCast<U*, Type*>(other.theObject);
    }
  }

  template <typename U> Nilable(Nilable<smart_ptr<U>> other) {
    if (other.nilp()) {
      this->theObject = reinterpret_cast<Type*>(global_tagged_Symbol_OP_nil);
      return;
    }
    this->theObject = TaggedCast<U*, Type*>::castOrNULL(other.theObject);
    if (!this->theObject) {
      core::lisp_errorCast<U*, Type*>(other.theObject);
    }
    return;
  }

  // Construct from the Base type
  Nilable(Base const& b) : Base(b){};
  inline Nilable(Base&& b) : Base(std::move(b)){};

  // Copy constructor
  Nilable(MyType const& b) : Base(b){};

  MyType& operator=(Base const& orig) {
    this->theObject = orig.theObject;
    return *this;
  }

  MyType& operator=(smart_ptr<core::T_O> const& orig) {
    GCTOOLS_ASSERT(tagged_nilp(orig.theObject) || orig.asOrNull<Type>());
    this->theObject = reinterpret_cast<Type*>(orig.theObject);
    return *this;
  }
  inline return_type as_return_type() const { return return_type(this->theObject, 1); };
  inline bool nilp() const { return tagged_nilp(this->theObject); }
  inline bool notnilp() const { return !tagged_nilp(this->theObject); }

  inline Type* operator->() {
    GCTOOLS_ASSERT(this->notnilp());
    return untag_general(this->theObject);
  };

  inline const Type* operator->() const {
    GCTOOLS_ASSERT(this->notnilp());
    return untag_general(this->theObject);
  };

  inline const Type& operator*() const {
    GCTOOLS_ASSERT(this->notnilp());
    return *(this->untag_object());
  };

  inline Type& operator*() {
    GCTOOLS_ASSERT(this->notnilp());
    return *(this->untag_object());
  };

  // Type conversion operator to T_sp can be Base or NIL
  operator smart_ptr<core::T_O>() const {
    if (tagged_nilp(this->theObject)) {
      return smart_ptr<core::T_O>((Tagged)tag_nil<core::T_O*>());
    }
    return smart_ptr<core::T_O>((Tagged)this->theObject);
  }

  // Convert Nilable<Foo> to smart_ptr<Foo> - signal error if it was NIL
  operator smart_ptr<Type>() const {
    if (!tagged_nilp(this->theObject))
      return smart_ptr<Type>(*this);
    class_id this_typ = reg::registered_class<Type>::id;
    core::lisp_errorUnexpectedNil(this_typ);
    HARD_UNREACHABLE();
  }
};

}; // namespace gctools

namespace gc = gctools;

namespace gctools {
// List_sp <-- T_sp
template <> inline core::List_sp As(core::T_sp const& rhs) { return core::List_sp(rhs); }
}; // namespace gctools

namespace core {
string _rep_(T_sp obj);
};

template <class T> std::ostream& operator<<(std::ostream& os, const gctools::smart_ptr<T>& obj) {
  os << core::_rep_(obj);
  return os;
}
