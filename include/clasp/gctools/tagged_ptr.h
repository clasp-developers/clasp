/*
    File: tagged_ptr.h
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
#ifndef _TAGGED_PTR_HPP_INCLUDED
#define _TAGGED_PTR_HPP_INCLUDED

//
//  tagged_ptr.hpp
//
//  Modifed by Christian Schafmeister 2013 to add tagging using the 2 least significant bits
//  Copyright (c) 2001, 2002 Peter Dimov
//
// Distributed under the Boost Software License, Version 1.0. (See
// accompanying file LICENSE_1_0.txt or copy at
// http://www.boost.org/LICENSE_1_0.txt)
//
//  See http://www.boost.org/libs/smart_ptr/intrusive_ptr.html for documentation.
//

#include <boost/config.hpp>
#include <boost/utility/binary.hpp>
#include <boost/assert.hpp>
#include <boost/detail/workaround.hpp>
#include <boost/smart_ptr/detail/sp_convertible.hpp>

#include <boost/config/no_tr1/functional.hpp> // for std::less

#if !defined(BOOST_NO_IOSTREAM)
#if !defined(BOOST_NO_IOSFWD)
#include <iosfwd> // for std::basic_ostream
#else
#include <ostream>
#endif
#endif

namespace gctools {

// Dummy class to store tagged fixnums as smart_ptr<Fixnum_ty>
class Fixnum_ty {};

template <class T>
class tagged_ptr {
private:
  typedef T *PointerType;
  typedef tagged_ptr this_type;

public:
  static const uintptr_t tag_mask = BOOST_BINARY(0011);
  static const uintptr_t ptr_tag = BOOST_BINARY(0000);     // xxx00 means ptr
  static const uintptr_t special_tag = BOOST_BINARY(0001); // xxx01 means special val
  static const uintptr_t frame_tag = BOOST_BINARY(0010);   // xxx10 means a ValueFrame stored entirely on the stack
  static const uintptr_t fixnum_tag = BOOST_BINARY(0011);  // xxx11 means fixnum
  static const uintptr_t fixnum_shift = 2;

  static const uintptr_t ptr_mask = ~tag_mask;

public:
  /*! Special tagged values */
  static const uintptr_t _NULL = BOOST_BINARY(000000);
  static const uintptr_t tagged_NULL = BOOST_BINARY(000000) | special_tag;    // Should I have this????
  static const uintptr_t tagged_unbound = BOOST_BINARY(000100) | special_tag; // 0x05
  static const uintptr_t tagged_nil = BOOST_BINARY(001000) | special_tag;     // 0x09
  static const uintptr_t tagged_deleted = BOOST_BINARY(001100) | special_tag; // 0x0D - used by WeakHashTable
  static const uintptr_t tagged_sameAsKey = BOOST_BINARY(010000) | special_tag;
  static const uintptr_t tagged_character = BOOST_BINARY(010100) | special_tag;
  static const uintptr_t character_shift = 8;

public:
  static T *make_tagged_nil() {
    return reinterpret_cast<T *>(tagged_nil);
  }
  static T *make_tagged_unbound() {
    return reinterpret_cast<T *>(tagged_unbound);
  }
  static T *make_tagged_frame(core::T_O **p) {
    return reinterpret_cast<T *>((reinterpret_cast<uintptr_t>(p) & ptr_mask) | frame_tag);
  }

  static T *untagged_frame(T *ptr) {
    return reinterpret_cast<core::T_O **>(reinterpret_cast<uintptr_t>(ptr) & ptr_mask);
  }

  static T *make_tagged_fixnum(int fn) {
    return reinterpret_cast<T *>((fn << fixnum_shift) | fixnum_tag);
  }

  static int untagged_fixnum(T *ptr) {
    return (int)(reinterpret_cast<uintptr_t>(ptr) >> fixnum_shift);
  }

  static bool tagged_pointerp(T *ptr) {
    return ((uintptr_t)(ptr) & tag_mask) == ptr_tag // Is ptr
           && ((uintptr_t)(ptr) & ptr_mask);        // Is not NULL
  }

  static bool tagged_nilp(T *ptr) {
    return (ptr == tagged_ptr<T>::make_tagged_nil());
  }
  static bool tagged_unboundp(T *ptr) {
    return (ptr == tagged_ptr<T>::make_tagged_unbound());
  }

public:
  typedef T element_type;

  tagged_ptr() : px(0) {
  }

  tagged_ptr(const T *p) {
    typedef typename std::remove_const<T>::type *no_const_T_ptr;
    if (p != NULL) {
      this->px = const_cast<no_const_T_ptr>((T *)(((uintptr_t)p) + ptr_tag));
    } else {
      this->px = NULL;
    }
  }

  tagged_ptr(core::T_O **p) : px(make_tagged_frame(p)){};

  explicit tagged_ptr(int p) : px(reinterpret_cast<T *>((p << fixnum_shift) | fixnum_tag)){};

  explicit tagged_ptr(uintptr_t p) : px((T *)p) {
    // ASSERT that the p does not correspond to a pointer
    BOOST_ASSERT((p & tag_mask) != ptr_tag);
  }

  template <class From>
  tagged_ptr(tagged_ptr<From> const &rhs) {
    if (LIKELY(rhs.pointerp())) {
      px = DynamicCast<T *, From *>::castOrNULL(rhs.pxget());
      if (px == 0) {
        THROW_HARD_ERROR(BF("DynamicCast<T*,From*> failed due to an illegal cast T* = %s  From* = %s") % typeid(T *).name() % typeid(From *).name());
      }
    } else {
      uintptr_t upx = reinterpret_cast<uintptr_t>(rhs.pxget());
      px = (T *)upx;
    }
  }

  tagged_ptr(tagged_ptr const &rhs) : px(rhs.px) {}

  ~tagged_ptr() {
  }

#if !defined(BOOST_NO_MEMBER_TEMPLATES) || defined(BOOST_MSVC6_MEMBER_TEMPLATES)

  template <class U>
  tagged_ptr &operator=(tagged_ptr<U> const &rhs) {
    this_type(rhs).swap(*this);
    return *this;
  }

#endif

// Move support

#if defined(BOOST_HAS_RVALUE_REFS)

  tagged_ptr(tagged_ptr &&rhs) : px(rhs.px) {
    rhs.px = 0;
  }

  core::T_O *asTPtr() const {
    if (LIKELY(this->pointerp())) {
      return static_cast<core::T_O *>(this->px);
    } else {
#pragma clang diagnostic push
#pragma clang diagnostic ignored "-Wreinterpret-base-class"
      return reinterpret_cast<core::T_O *>(this->px);
#pragma clang diagnostic pop
    }
  }

  tagged_ptr &operator=(tagged_ptr &&rhs) {
    this_type(static_cast<tagged_ptr &&>(rhs)).swap(*this);
    return *this;
  }

#endif

  tagged_ptr &operator=(tagged_ptr const &rhs) {
    this_type(rhs).swap(*this);
    return *this;
  }

  tagged_ptr &operator=(T *rhs) {
    this_type(rhs).swap(*this);
    return *this;
  }

  void reset() {
    this_type().swap(*this);
  }

  void reset(T *rhs) {
    this_type(rhs).swap(*this);
  }

  /*! Return true if px contains a non-NULL pointer */
  bool pointerp() const {
    return ((uintptr_t)(this->px) & tag_mask) == ptr_tag // Is ptr
           && ((uintptr_t)(this->px) & ptr_mask);        // Is not NULL
  };

  bool not_pointerp() const { return !this->pointerp(); };

  /*! THROW exception if px is not a pointer */
  void assert_pointer() const { BOOST_ASSERT(this->pointerp()); }

  uintptr_t tag() const { return reinterpret_cast<uintptr_t>(this->px) & tag_mask; };
  bool taggedp() const { return (this->px & tag_mask); };

  bool _NULLp() const { return (uintptr_t) this->px == tagged_NULL; };

  bool deletedp() const { return (uintptr_t) this->px == tagged_deleted; };

  bool unboundp() const { return (uintptr_t) this->px == tagged_unbound; };
  bool notunboundp() const { return !this->unboundp(); };

  bool sameAsKeyP() const { return (uintptr_t) this->px == tagged_sameAsKey; };
  bool notsameAsKeyP() const { return !this->sameAsKeyP(); };

  bool nilp() const { return (uintptr_t) this->px == tagged_nil; };

  bool framep() const { return ((reinterpret_cast<uintptr_t>(this->px) & tag_mask) == frame_tag); };
  core::T_O **frame() const { return reinterpret_cast<core::T_O **>(reinterpret_cast<uintptr_t>(this->px) & ptr_mask); };

  //        bool characterp() const { return ((reinterpret_cast<uintptr_t>(this->px)&tag_mask)==character_tag);};

  bool specialp() const { return ((reinterpret_cast<uintptr_t>(this->px) & tag_mask) == special_tag); }
  bool fixnump() const { return ((reinterpret_cast<uintptr_t>(this->px) & tag_mask) == fixnum_tag); };
  // Handle get_fixnum

  inline claspChar character() const { return ((reinterpret_cast<uintptr_t>(this->px) >> character_shift)); };
  inline Fixnum fixnum() const { return ((reinterpret_cast<uintptr_t>(this->px) >> fixnum_shift)); };

  T *get() const {
    if (LIKELY(pointerp() || px == 0)) {
      return px;
    }
    if (nilp())
      lisp_errorDereferencedNil();
    if (unboundp())
      lisp_errorDereferencedUnbound();
    lisp_errorIllegalDereference(px);
    return NULL; // should not get here
                 //        return px;
  }

  T &operator*() const {
#ifdef RUN_SAFE
    return *(this->get());
#else
    //            return *this->px;
    return *(T *)(((uintptr_t) this->px) - ptr_tag); //   *((cons *) (((unsigned long long int) ptr) - 3))
#endif
  }

  T *operator->() const {
#ifdef RUN_SAFE
    return this->get();
#else
    return (T *)(((uintptr_t) this->px) - ptr_tag); //   *((cons *) (((unsigned long long int) ptr) - 3))
#endif
  }

  inline T *pxget() const {
    return (T *)(((uintptr_t) this->px) - ptr_tag); //   *((cons *) (((unsigned long long int) ptr) - 3))
  }

  /*! Use pointer rather than pxget */
  inline T *pointer() const {
    return (T *)(((uintptr_t) this->px) - ptr_tag); //   *((cons *) (((unsigned long long int) ptr) - 3))
  }

// implicit conversion to "bool"
#include <clasp/gctools/tagged_operator_bool.h>
  //<boost/smart_ptr/detail/operator_bool.hpp>

  void swap(tagged_ptr &rhs) {
    T *tmp = this->px;
    this->px = rhs.px;
    rhs.px = tmp;
  }

  PointerType &px_ref() const { return this->px; };

  void _pbase_set(void *npbase) {
    this->pbase = reinterpret_cast<PointerType>(npbase);
  }

  /* Satisfy const correctness checks but p0 is mutable */
  void _pbase_set(void *npbase) const {
    this->pbase = reinterpret_cast<PointerType>(npbase);
  }

  void _pxset(void *npx) {
    this->px = reinterpret_cast<PointerType>(npx);
  }

  /* Satisfy const correctness checks but px is mutable */
  void _pxset(void *npx) const {
    this->px = reinterpret_cast<PointerType>(npx);
  }

  core::T_O *asArg() const {
    if (this->pointerp()) {
      return dynamic_cast<core::T_O *>(this->px_ref());
    }
    return reinterpret_cast<core::T_O *>(this->px);
  }

  void *pbase() const {
    if (this->pointerp()) {
      return reinterpret_cast<void *>(this->px);
      // Shouldn't this point to the mps base?
      //                return dynamic_cast<void*>(this->px);
    }
    return NULL;
  }

public:
  mutable PointerType px;
};
};

namespace gctools {

/* working on base_ptr */

/*! A class that stores a base pointer of a garbage collected object.
      It can be constructed from any smart_ptr or any pointer to any kind
      of GC managed object */
class tagged_base_ptr {
public:
  static const uintptr_t _NULL = tagged_ptr<typename GCHeader<void>::HeaderType>::tagged_NULL;
  static const uintptr_t unbound = tagged_ptr<typename GCHeader<void>::HeaderType>::tagged_unbound;
  static const uintptr_t deleted = tagged_ptr<typename GCHeader<void>::HeaderType>::tagged_deleted;
  tagged_base_ptr(uintptr_t v) : base(v){};
  tagged_base_ptr() : base(){};

  template <class U>
  static typename GCHeader<void>::HeaderType *toBasePtr(U *ptr) {
    return reinterpret_cast<typename GCHeader<void>::HeaderType *>(ClientPtrToBasePtr(ptr));
  }

  template <class U>
  tagged_base_ptr(tagged_ptr<U> objPtr) {
    if (objPtr.pointerp()) {
      this->base = toBasePtr(objPtr.px_ref());
    } else {
      this->base.px_ref() = reinterpret_cast<typename GCHeader<void>::HeaderType *>(objPtr.px_ref());
    }
  }

  inline void *pointer() const { return reinterpret_cast<void *>(this->base_ref().px_ref()); };
  inline bool NULLp() const { return this->base_ref().px_ref() == 0; };
  inline bool pointerp() const { return this->base_ref().pointerp(); };
  inline bool unboundp() const { return this->base_ref().unboundp(); };
  inline bool sameAsKeyP() const { return this->base_ref().sameAsKeyP(); };
  inline bool deletedp() const {
#ifdef USE_MPS
    return this->base_ref().deletedp();
#endif
#ifdef USE_BOEHM
    return this->base_ref().deletedp() || this->NULLp(); // splatted values are deleted
#endif
  };
  bool operator==(const tagged_base_ptr &other) { return this->base_ref().px_ref() == other.base_ref().px_ref(); };
  tagged_ptr<typename GCHeader<void>::HeaderType> &base_ref() { return this->base; };
  const tagged_ptr<typename GCHeader<void>::HeaderType> &base_ref() const { return this->base; };

  gctools::Header_s *&px_ref() { return this->base.px_ref(); };

  void **splattableAddress() const { return reinterpret_cast<void **>(&this->base.px_ref()); };

public:
  tagged_ptr<typename GCHeader<void>::HeaderType> base;
};

/*! A class that inherits from tagged_base_ptr 
      and can be backcast back to one type of internal pointer.
      It does this by calculating and storing an offset to the internal pointer 
      in the tagged Fixnum (offset).
    */
template <class T>
class tagged_backcastable_base_ptr : public tagged_base_ptr {
#if 0
    public:
        static const uintptr_t _NULL = tagged_base_ptr::_NULL;
        static const uintptr_t unbound = tagged_base_ptr::unbound;
        static const uintptr_t deleted = tagged_base_ptr::deleted;
#endif
public:
  explicit tagged_backcastable_base_ptr() : tagged_base_ptr(), offset(){};
  explicit tagged_backcastable_base_ptr(uintptr_t v) : tagged_base_ptr(v), offset(){};
  explicit tagged_backcastable_base_ptr(tagged_ptr<T> objPtr) : tagged_base_ptr(objPtr) {
    if (objPtr.pointerp()) {
      const char *base_addr = reinterpret_cast<const char *>(this->base.px_ref());
      const char *obj_addr = reinterpret_cast<const char *>(objPtr.px_ref());
      int diff = obj_addr - base_addr;
      this->offset = smart_ptr<Fixnum_ty>(diff);
    } else {
      this->offset = smart_ptr<Fixnum_ty>(gctools::tagged_ptr<Fixnum_ty>::tagged_nil);
    }
  }

  tagged_ptr<T> backcast() const {
    if (this->offset.fixnump()) {
      char *base_addr = reinterpret_cast<char *>(this->base.px_ref());
      int diff = this->offset.fixnum();
      char *obj_addr = base_addr + diff;
      return tagged_ptr<T>(reinterpret_cast<T *>(obj_addr));
    }
    return tagged_ptr<T>(reinterpret_cast<uintptr_t>(this->base.px_ref()));
  }

public:
  smart_ptr<Fixnum_ty> offset; // tagged fixnum to offset base
};
};

namespace gctools {

template <class T, class U>
inline bool operator==(tagged_ptr<T> const &a, tagged_ptr<U> const &b) {
  if (!(a.pointerp() && b.pointerp())) {
    return a.px_ref() == b.px_ref();
    THROW_HARD_ERROR(BF("Implement more == tagged_ptr comparisons"));
  };
  // Both a and b are pointers to objects in memory - compare their pbases
  return a.pbase() == b.pbase();
}

template <class T, class U>
inline bool operator!=(tagged_ptr<T> const &a, tagged_ptr<U> const &b) {
  if (!(a.pointerp() && b.pointerp())) {
    return a.px_ref() != b.px_ref();
    THROW_HARD_ERROR(BF("Implement more != tagged_ptr comparisons"));
  };
  // Both a and b are pointers to objects in memory - compare their pbases
  return a.pbase() != b.pbase();
}

template <class T, class U>
inline bool operator==(tagged_ptr<T> const &a, U *b) {
  return a.pxget() == b;
}

template <class T, class U>
inline bool operator!=(tagged_ptr<T> const &a, U *b) {
  return a.pxget() != b;
}

template <class T, class U>
inline bool operator==(T *a, tagged_ptr<U> const &b) {
  return a == b.pxget();
}

template <class T, class U>
inline bool operator!=(T *a, tagged_ptr<U> const &b) {
  return a != b.pxget();
}

#if __GNUC__ == 2 && __GNUC_MINOR__ <= 96

// Resolve the ambiguity between our op!= and the one in rel_ops

template <class T>
inline bool operator!=(tagged_ptr<T> const &a, tagged_ptr<T> const &b) {
  return a.pxget() != b.pxget();
}

#endif

template <class T>
inline bool operator<(tagged_ptr<T> const &a, tagged_ptr<T> const &b) {
  return std::less<T *>()(a.pxget(), b.pxget());
}

template <class T>
void swap(tagged_ptr<T> &lhs, tagged_ptr<T> &rhs) {
  lhs.swap(rhs);
}

// mem_fn support

template <class T>
T *get_pointer(tagged_ptr<T> const &p) {
  return p.get();
}

template <class T, class U>
tagged_ptr<T> static_pointer_cast(tagged_ptr<U> const &p) {
  return static_cast<T *>(p.get());
}

template <class T, class U>
tagged_ptr<T> const_pointer_cast(tagged_ptr<U> const &p) {
  return const_cast<T *>(p.get());
}

template <class T, class U>
tagged_ptr<T> dynamic_pointer_cast(tagged_ptr<U> const &p) {
  return dynamic_cast<T *>(p.get());
}

}; // namespace gctools

// operator<<

#if !defined(BOOST_NO_IOSTREAM)

#if defined(BOOST_NO_TEMPLATED_IOSTREAMS) || (defined(__GNUC__) && (__GNUC__ < 3))

template <class Y>
std::ostream &operator<<(std::ostream &os, tagged_ptr<Y> const &p) {
  os << p.get();
  return os;
}

#else

// in STLport's no-iostreams mode no iostream symbols can be used
#ifndef _STLP_NO_IOSTREAMS

#if defined(BOOST_MSVC) && BOOST_WORKAROUND(BOOST_MSVC, < 1300 && __SGI_STL_PORT)
// MSVC6 has problems finding std::basic_ostream through the using declaration in namespace _STL
using std::basic_ostream;
template <class E, class T, class Y>
basic_ostream<E, T> &operator<<(basic_ostream<E, T> &os, tagged_ptr<Y> const &p)
#else
template <class E, class T, class Y>
std::basic_ostream<E, T> &operator<<(std::basic_ostream<E, T> &os, gctools::tagged_ptr<Y> const &p)
#endif
{
  os << p.get();
  return os;
}

#endif // _STLP_NO_IOSTREAMS

#endif // __GNUC__ < 3

#endif // !defined(BOOST_NO_IOSTREAM)

#endif // #ifndef _TAGGED_PTR_HPP_INCLUDED
