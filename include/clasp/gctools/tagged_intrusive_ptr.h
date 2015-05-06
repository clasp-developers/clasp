/*
    File: tagged_intrusive_ptr.h
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
#ifndef BOOST_SMART_PTR_TAGGED_INTRUSIVE_PTR_HPP_INCLUDED
#define BOOST_SMART_PTR_TAGGED_INTRUSIVE_PTR_HPP_INCLUDED

//
//  tagged_intrusive_ptr.hpp
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

namespace boost {

//
//  tagged_intrusive_ptr
//
//  A smart pointer that uses intrusive reference counting.
//
//  Relies on unqualified calls to
//
//      void intrusive_ptr_add_ref(T& p);
//      void intrusive_ptr_release(T& p);
//
//          (p != 0)
//
//  The object is responsible for destroying itself.
//

template <class T>
class tagged_intrusive_ptr {
private:
  typedef T *PointerType;
  typedef tagged_intrusive_ptr this_type;

public:
  static const uintptr_t tag_mask = BOOST_BINARY(0011);
  static const uintptr_t ptr_tag = BOOST_BINARY(0000);       // xxx00 means ptr
  static const uintptr_t special_tag = BOOST_BINARY(0001);   // xxx01 means special val
  static const uintptr_t character_tag = BOOST_BINARY(0010); // xxx10 means character
  static const uintptr_t fixnum_tag = BOOST_BINARY(0011);    // xxx11 means fixnum
  static const uintptr_t ptr_mask = ~tag_mask;

public:
  static const uintptr_t tagged_NULL = BOOST_BINARY(0000) | special_tag;
  static const uintptr_t tagged_unbound = BOOST_BINARY(0100) | special_tag;
  static const uintptr_t tagged_nil = BOOST_BINARY(1000) | special_tag;
  static const uintptr_t tagged_deleted = BOOST_BINARY(1100) | special_tag;

public:
  typedef T element_type;

  tagged_intrusive_ptr() : px(0) {
  }

  //    this -> isNil() --> true
  //    tagged_intrusive_ptr x(this);
  //    tagged_intrusive_ptr x((A*)this);

  tagged_intrusive_ptr(const T *p, bool add_ref = true) {
    this->px = const_cast<T *>(p);
    BOOST_ASSERT(p == 0 || pointerp());
    if (p != 0) {
#ifdef USE_REFCOUNT
      if (add_ref)
        intrusive_ptr_add_ref((this->px)); // pointerp() not necessary
#endif
    }
  }

  explicit tagged_intrusive_ptr(claspChar c) : px(reinterpret_cast<T *>((c << 8) | character_tag)){};
  explicit tagged_intrusive_ptr(int p) : px(reinterpret_cast<T *>((p << 2) | fixnum_tag)){};

  explicit tagged_intrusive_ptr(uintptr_t p) : px(reinterpret_cast<T *>(p)) {
  }

#if !defined(BOOST_NO_MEMBER_TEMPLATES) || defined(BOOST_MSVC6_MEMBER_TEMPLATES)

  template <class U>
#if !defined(BOOST_SP_NO_SP_CONVERTIBLE)

  tagged_intrusive_ptr(tagged_intrusive_ptr<U> const &rhs, typename boost::detail::sp_enable_if_convertible<U, T>::type = boost::detail::sp_empty())

#else

  tagged_intrusive_ptr(tagged_intrusive_ptr<U> const &rhs)

#endif
  //    	: px( rhs.get() )
  {
    if (rhs.pointerp()) {
      px = rhs.pxget(); // implicit cast
#ifdef USE_REFCOUNT
      intrusive_ptr_add_ref((this->px)); // pointerp() not necessary
#endif
      return;
    }
    // Copy the bit pattern in rhs.px into this->px
    uintptr_t upx = (uintptr_t)rhs.pxget();
    px = (T *)upx;
  }

#endif

  tagged_intrusive_ptr(tagged_intrusive_ptr const &rhs) : px(rhs.px) {
#ifdef USE_REFCOUNT
    if (pointerp())
      intrusive_ptr_add_ref((this->px)); // pointerp() not necessary
#endif
  }

  ~tagged_intrusive_ptr() {
#ifdef USE_REFCOUNT
    if (pointerp())
      intrusive_ptr_release((this->px));
#endif
  }

#if !defined(BOOST_NO_MEMBER_TEMPLATES) || defined(BOOST_MSVC6_MEMBER_TEMPLATES)

  template <class U>
  tagged_intrusive_ptr &operator=(tagged_intrusive_ptr<U> const &rhs) {
    this_type(rhs).swap(*this);
    return *this;
  }

#endif

// Move support

#if defined(BOOST_HAS_RVALUE_REFS)

  tagged_intrusive_ptr(tagged_intrusive_ptr &&rhs) : px(rhs.px) {
    rhs.px = 0;
  }

  tagged_intrusive_ptr &operator=(tagged_intrusive_ptr &&rhs) {
    this_type(static_cast<tagged_intrusive_ptr &&>(rhs)).swap(*this);
    return *this;
  }

#endif

  tagged_intrusive_ptr &operator=(tagged_intrusive_ptr const &rhs) {
    this_type(rhs).swap(*this);
    return *this;
  }

  tagged_intrusive_ptr &operator=(T *rhs) {
    this_type(rhs).swap(*this);
    return *this;
  }

  void reset() {
    this_type().swap(*this);
  }

  void reset(T *rhs) {
    this_type(rhs).swap(*this);
  }

  /*! Return true if px contains a pointer */
  bool pointerp() const {
    return ((uintptr_t)(this->px) & tag_mask) == ptr_tag && ((uintptr_t)(this->px) & ptr_mask);
  };

  bool not_pointerp() const { return !this->pointerp(); };

  /*! THROW exception if px is not a pointer */
  void assert_pointer() const { BOOST_ASSERT(this->pointerp()); }

  uintptr_t tag() const { return reinterpret_cast<uintptr_t>(this->px) & tag_mask; };
  bool taggedp() const { return (this->px & tag_mask); };

  bool _NULLp() const { return (uintptr_t) this->px == tagged_NULL; };

  bool unboundp() const { return (uintptr_t) this->px == tagged_unbound; };
  bool notunboundp() const { return !this->unboundp(); };

  bool nilp() const { return (uintptr_t) this->px == tagged_nil; };

  bool characterp() const { return ((reinterpret_cast<uintptr_t>(this->px) & tag_mask) == character_tag); };
  bool fixnump() const { return ((reinterpret_cast<uintptr_t>(this->px) & tag_mask) == fixnum_tag); };
  // Handle get_fixnum

  inline claspChar character() const { return ((reinterpret_cast<uintptr_t>(this->px) >> 8)); };
  inline Fixnum fixnum() const { return ((reinterpret_cast<uintptr_t>(this->px) >> 2)); };

  T *pxget() const {
    return px;
  }

  T *get() const {
    if (LIKELY(px == 0 || pointerp())) {
      return px;
    }
    if (nilp())
      lisp_errorDereferencedNil();
    if (unboundp())
      lisp_errorDereferencedUnbound();
    //	BOOST_ASSERT_MSG( !nilp(), "You tried to dereference NIL");
    //	BOOST_ASSERT_MSG( !unboundp(), "You tried to dereference UNBOUND");
    //	nilp() and unboundp() no longer dereference to nil and unbound instances
    //	if ( nilp() ) return intrusive_ptr_nil_instance<T>();
    //	if ( unboundp() ) return intrusive_ptr_unbound_instance<T>();
    // handle more cases
    BOOST_ASSERT_MSG(false, "You tried to dereference something that wasn't a pointer");
    return NULL;
    //        return px;
  }

  T &operator*() const {
    return *(this->get());
  }

  T *operator->() const {
    return this->get();
  }

// implicit conversion to "bool"
#include <clasp/gctools/tagged_operator_bool.h>
  //<boost/smart_ptr/detail/operator_bool.hpp>

  void swap(tagged_intrusive_ptr &rhs) {
    T *tmp = px;
    px = rhs.px;
    rhs.px = tmp;
  }

  PointerType &px_ref() const { return this->px; };

protected:
  mutable PointerType px;
};

template <class T, class U>
inline bool operator==(tagged_intrusive_ptr<T> const &a, tagged_intrusive_ptr<U> const &b) {
  if (!(a.pointerp() && b.pointerp())) {
    bool anilp = a.nilp();
    bool bnilp = b.nilp();
    if (anilp || bnilp) {
      return anilp && bnilp;
    }
    if (a.unboundp() || b.unboundp())
      return false;
    THROW_HARD_ERROR(BF("Implement more == comparisons for tagged ptrs"));
  }
  return dynamic_cast<void *>(a.pxget()) == dynamic_cast<void *>(b.pxget());
}

template <class T, class U>
inline bool operator!=(tagged_intrusive_ptr<T> const &a, tagged_intrusive_ptr<U> const &b) {
  if (!(a.pointerp() && b.pointerp())) {
    bool anilp = a.nilp();
    bool bnilp = b.nilp();
    if (anilp || bnilp) {
      return !(anilp && bnilp);
    }
    if (a.unboundp() || b.unboundp())
      return true;
    THROW_HARD_ERROR(BF("Implement more != comparisons for tagged ptrs"));
  }
  return (dynamic_cast<void *>(a.pxget()) != dynamic_cast<void *>(b.pxget()));
}

template <class T, class U>
inline bool operator==(tagged_intrusive_ptr<T> const &a, U *b) {
  if (a.nilp())
    return false;
  return dynamic_cast<void *>(a.get()) == dynamic_cast<void *>(b);
}

template <class T, class U>
inline bool operator!=(tagged_intrusive_ptr<T> const &a, U *b) {
  if (a.nilp())
    return true;
  return dynamic_cast<void *>(a.pxget()) != dynamic_cast<void *>(b);
}

template <class T, class U>
inline bool operator==(T *a, tagged_intrusive_ptr<U> const &b) {
  if (b.nilp())
    return false;
  return dynamic_cast<void *>(const_cast<T *>(a)) == dynamic_cast<void *>(const_cast<U *>(b.pxget()));
}

template <class T, class U>
inline bool operator!=(T *a, tagged_intrusive_ptr<U> const &b) {
  if (b.nilp())
    return true;
  return dynamic_cast<void *>(a) != dynamic_cast<const void *>(const_cast<U *>(b.pxget()));
}

#if __GNUC__ == 2 && __GNUC_MINOR__ <= 96

// Resolve the ambiguity between our op!= and the one in rel_ops

template <class T>
inline bool operator!=(tagged_intrusive_ptr<T> const &a, tagged_intrusive_ptr<T> const &b) {
  return dynamic_cast<void *>(a.pxget()) != dynamic_cast<void *>(b.pxget());
}

#endif

template <class T>
inline bool operator<(tagged_intrusive_ptr<T> const &a, tagged_intrusive_ptr<T> const &b) {
  return std::less<T *>()(a.pxget(), b.pxget());
}

template <class T>
void swap(tagged_intrusive_ptr<T> &lhs, tagged_intrusive_ptr<T> &rhs) {
  lhs.swap(rhs);
}

// mem_fn support

template <class T>
T *get_pointer(tagged_intrusive_ptr<T> const &p) {
  return p.get();
}

template <class T, class U>
tagged_intrusive_ptr<T> static_pointer_cast(tagged_intrusive_ptr<U> const &p) {
  return static_cast<T *>(p.get());
}

template <class T, class U>
tagged_intrusive_ptr<T> const_pointer_cast(tagged_intrusive_ptr<U> const &p) {
  return const_cast<T *>(p.get());
}

template <class T, class U>
tagged_intrusive_ptr<T> dynamic_pointer_cast(tagged_intrusive_ptr<U> const &p) {
  return dynamic_cast<T *>(p.get());
}

// operator<<

#if !defined(BOOST_NO_IOSTREAM)

#if defined(BOOST_NO_TEMPLATED_IOSTREAMS) || (defined(__GNUC__) && (__GNUC__ < 3))

template <class Y>
std::ostream &operator<<(std::ostream &os, tagged_intrusive_ptr<Y> const &p) {
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
basic_ostream<E, T> &operator<<(basic_ostream<E, T> &os, tagged_intrusive_ptr<Y> const &p)
#else
template <class E, class T, class Y>
std::basic_ostream<E, T> &operator<<(std::basic_ostream<E, T> &os, tagged_intrusive_ptr<Y> const &p)
#endif
{
  os << p.get();
  return os;
}

#endif // _STLP_NO_IOSTREAMS

#endif // __GNUC__ < 3

#endif // !defined(BOOST_NO_IOSTREAM)

// hash_value

template <class T>
struct hash;

template <class T>
std::size_t hash_value(boost::tagged_intrusive_ptr<T> const &p) {
  return boost::hash<T *>()(p.get());
}

} // namespace boost

#endif // #ifndef BOOST_SMART_PTR_TAGGED_INTRUSIVE_PTR_HPP_INCLUDED
