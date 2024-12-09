#pragma once
/*
    File: cons.h
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

#include <stdio.h>
#include <string>
#include <vector>
#include <set>
#include <atomic>
#include <clasp/core/cons.fwd.h>
#include <clasp/core/foundation.h>

// #include <clasp/core/lispList.h>

namespace cl {
extern core::Symbol_sp& _sym_type_error;
extern core::Symbol_sp& _sym_Cons_O;
}; // namespace cl

namespace kw {
extern core::Symbol_sp& _sym_datum;
extern core::Symbol_sp& _sym_expected_type;
}; // namespace kw

namespace core {

T_sp oCar(T_sp obj);
T_sp oCdr(T_sp obj);
T_sp oCaar(T_sp o);
T_sp oCadr(T_sp o);
T_sp oCdar(T_sp o);
T_sp oCddr(T_sp o);
T_sp oCaaar(T_sp o);
T_sp oCaadr(T_sp o);
T_sp oCadar(T_sp o);
T_sp oCaddr(T_sp o);
T_sp oCdaar(T_sp o);
T_sp oCdadr(T_sp o);
T_sp oCddar(T_sp o);
T_sp oCdddr(T_sp o);
T_sp oCaaaar(T_sp o);
T_sp oCaadar(T_sp o);
T_sp oCadaar(T_sp o);
T_sp oCaddar(T_sp o);
T_sp oCdaaar(T_sp o);
T_sp oCdadar(T_sp o);
T_sp oCddaar(T_sp o);
T_sp oCdddar(T_sp o);
T_sp oCaaadr(T_sp o);
T_sp oCaaddr(T_sp o);
T_sp oCadadr(T_sp o);
T_sp oCadddr(T_sp o);
T_sp oCdaadr(T_sp o);
T_sp oCdaddr(T_sp o);
T_sp oCddadr(T_sp o);
T_sp oCddddr(T_sp o);
T_sp oFirst(T_sp o);
T_sp oSecond(T_sp o);
T_sp oThird(T_sp o);
T_sp oFourth(T_sp o);
T_sp oFifth(T_sp o);
T_sp oSixth(T_sp o);
T_sp oSeventh(T_sp o);
T_sp oEighth(T_sp o);
T_sp oNinth(T_sp o);
T_sp oTenth(T_sp o);

#define CONS_CAR(x) ((x).as_unsafe<core::Cons_O>()->car())
#define CONS_CDR(x) ((x).as_unsafe<core::Cons_O>()->cdr())
#define CAR(x) oCar(x)
#define CDR(x) oCdr(x)
}; // namespace core

namespace core {
SMART(Cons);
};

template <> struct gctools::GCInfo<core::Cons_O> {
  static bool constexpr NeedsInitialization = false;
  static bool constexpr NeedsFinalization = false;
  static GCInfo_policy constexpr Policy = normal;
};

extern "C" {
void cc_validate_tagged_pointer(core::T_O* ptr);
};

namespace core {

class Cons_O : public T_O {
  LISP_ABSTRACT_CLASS(core, ClPkg, Cons_O, "Cons", T_O);

  friend T_sp oCar(T_sp o);
  friend T_sp oCdr(T_sp o);
#ifdef USE_PRECISE_GC
public: // Garbage collector functions
  uintptr_t rawRefCar() const { return (uintptr_t)this->car().raw_(); }
  uintptr_t rawRefCdr() const { return (uintptr_t)this->cdr().raw_(); }
  void rawRefSetCar(uintptr_t val) {
    T_sp tval((gctools::Tagged)val);
    this->setCarNoValidate(tval);
  }
  void rawRefSetCdr(uintptr_t val) {
    T_sp tval((gctools::Tagged)val);
    this->setCdrNoValidate(tval);
  }
#endif
public:
  //
  // Return true if the address points to a Cons_O cell header
  //
  inline bool cons_header_p() {
    uintptr_t val = *(uintptr_t*)this;
    return (val & gctools::Header_s::mtag_mask) == gctools::Header_s::cons_mtag;
  }

public:
  std::atomic<T_sp> _Car;
  std::atomic<T_sp> _Cdr;

public:
  template <class T> static List_sp createFromVec0(const gctools::Vec0<T>& vec) {
    List_sp res(nil<T_O>());
    for (cl_index i(vec.size() - 1); i >= 0; --i) {
      res = Cons_O::create(vec[i], res);
    }
    return res;
  }

  static Cons_sp createFrom_vaslist(Vaslist& va_args);
  static Cons_sp createList(T_sp o1);
  static Cons_sp createList(T_sp o1, T_sp o2);
  static Cons_sp createList(T_sp o1, T_sp o2, T_sp o3);
  static Cons_sp createList(T_sp o1, T_sp o2, T_sp o3, T_sp o4);
  static Cons_sp createList(T_sp o1, T_sp o2, T_sp o3, T_sp o4, T_sp o5);
  static Cons_sp createList(T_sp o1, T_sp o2, T_sp o3, T_sp o4, T_sp o5, T_sp o6);
  static Cons_sp createList(T_sp o1, T_sp o2, T_sp o3, T_sp o4, T_sp o5, T_sp o6, T_sp o7);
  static Cons_sp createList(T_sp o1, T_sp o2, T_sp o3, T_sp o4, T_sp o5, T_sp o6, T_sp o7, T_sp o8);
  static Cons_sp createList(T_sp o1, T_sp o2, T_sp o3, T_sp o4, T_sp o5, T_sp o6, T_sp o7, T_sp o8, T_sp o9);
  static Cons_sp createList(T_sp o1, T_sp o2, T_sp o3, T_sp o4, T_sp o5, T_sp o6, T_sp o7, T_sp o8, T_sp o9, T_sp o10);
#ifdef ALWAYS_INLINE_MPS_ALLOCATIONS
  __attribute__((always_inline))
#else
  inline
#endif
  static Cons_sp
  create(T_sp car, T_sp cdr) {
    gctools::smart_ptr<Cons_O> ll = gctools::ConsAllocator<gctools::RuntimeStage, Cons_O>::allocate(car, cdr);
    return ll;
  };
  template <typename Stage> static Cons_sp createAtStage(T_sp car, T_sp cdr) {
    gctools::smart_ptr<Cons_O> ll = gctools::ConsAllocator<Stage, Cons_O>::allocate(car, cdr);
    return ll;
  };

public:
  inline static size_t car_offset() { return offsetof(Cons_O, _Car); }
  inline static size_t cdr_offset() { return offsetof(Cons_O, _Cdr); }

public: // basic access
  inline T_sp car() const { return _Car.load(std::memory_order_relaxed); }
  inline T_sp cdr() const { return _Cdr.load(std::memory_order_relaxed); }
  inline void setCarNoValidate(T_sp o) { _Car.store(o, std::memory_order_relaxed); }
  inline void setCdrNoValidate(T_sp o) { _Cdr.store(o, std::memory_order_relaxed); }
  inline void setCar(T_sp o) {
#ifdef DEBUG_STORES
    cc_validate_tagged_pointer(o.raw_());
#endif
    this->setCarNoValidate(o);
  }
  inline void setCdr(T_sp o) {
#ifdef DEBUG_STORES
    cc_validate_tagged_pointer(o.raw_());
#endif
    this->setCdrNoValidate(o);
  }

public: // atomic access
  inline T_sp carAtomic(std::memory_order order) const { return _Car.load(order); }
  inline T_sp cdrAtomic(std::memory_order order) const { return _Cdr.load(order); }
  inline void setCarAtomic(T_sp o, std::memory_order order) { _Car.store(o, order); }
  inline void setCdrAtomic(T_sp o, std::memory_order order) { _Cdr.store(o, order); }
  inline bool carCAS(T_sp& expected, T_sp desired, std::memory_order order) {
    return _Car.compare_exchange_strong(expected, desired, order);
  }
  inline bool cdrCAS(T_sp& expected, T_sp desired, std::memory_order order) {
    return _Cdr.compare_exchange_strong(expected, desired, order);
  }

public:
  inline Cons_sp rplaca(T_sp o) {
    setCar(o);
    return this->asSmartPtr();
  }
  inline Cons_sp rplacd(T_sp o) {
    setCdr(o);
    return this->asSmartPtr();
  };

  T_sp onth(cl_index idx) const;
  T_sp onthcdr(cl_index idx) const;

  T_sp elt(cl_index index) const;
  T_sp setf_elt(cl_index index, T_sp value);

  inline T_sp ocadr() const {
    T_sp cdr = this->cdr();
    if (UNLIKELY(!cdr.consp()))
      return nil<T_O>();
    return cdr.unsafe_cons()->car();
  }

  /*! Get the data for the first element */
  template <class o_class> gctools::smart_ptr<o_class> car() {
    ASSERTNOTNULL(this->car());
    return gc::As<gc::smart_ptr<o_class>>(this->car());
  };
  T_sp setf_nth(cl_index index, T_sp val);
  /*! Return the last cons (not the last element) of list.
          If we are nil then return nil */
  T_sp last(cl_index idx = 1) const;

public:
  static T_sp append(List_sp x, List_sp y);

public:
  /*! Recursively hash the car and cdr parts - until the HashGenerator fills up */
  inline void sxhash_equal(HashGenerator& hg) const {
    if (hg.isFilling()) hg.hashObject(this->car());
    if (hg.isFilling()) hg.hashObject(this->cdr());
  }
  inline void sxhash_equalp(HashGenerator& hg) const {
    if (hg.isFilling()) hg.hashObjectEqualp(this->car());
    if (hg.isFilling()) hg.hashObjectEqualp(this->cdr());
  }

  bool equal(T_sp obj) const;
  bool equalp(T_sp obj) const;

  /*! Return a new list by combinding the given list of elements to our list
   */
  List_sp extend(List_sp rest);

  List_sp revappend(T_sp tail);
  List_sp nreconc(T_sp tail);

  //    CL_LISPIFY_NAME("core:cons-setf-cdr");
  //    CL_DEFMETHOD
  T_sp setf_cdr(T_sp o) {
    this->setCdr(o);
    return o;
  };
  /*! Like Common Lisp copy-list */
  List_sp copyList() const;

  /*! Like Common Lisp copy-tree */
  List_sp copyTree() const;

  /*! Return a new Cons with a tree copy of the current car*/
  List_sp copyTreeCar() const;

  /*! Return the number of elements in the list*/
  size_t length() const;

  /*! Calculate the length the fastest way I can think of.
      It only works on proper lists. */
  inline size_t proper_list_length() const {
    size_t sz = 1;
    T_sp cur = this->_Cdr;
    while (cur.consp()) {
      ++sz;
      cur = cur.unsafe_cons()->_Cdr;
    }
    return sz;
  }

  List_sp reverse();
  List_sp nreverse();

  List_sp memberEq(T_sp item) const;
  List_sp memberEql(T_sp item) const;

  List_sp member1(T_sp item, T_sp key, T_sp test, T_sp testNot) const;
  List_sp member(T_sp item, T_sp key, T_sp test, T_sp testNot) const;
  List_sp assoc(T_sp item, T_sp key, T_sp test, T_sp testNot) const;

  void describe(T_sp stream);
  string __repr__() const;
  void __write__(T_sp stream) const;
  bool maybe_write_quoted_form(bool tail, T_sp stream) const;

  /*!Set the owner of every car in the list
   */
  //	void setOwnerOfAllEntries(T_sp obj);

  inline bool eq(T_sp o) const {
    if (o.consp()) {
      return this == o.unsafe_cons();
    }
    return false;
  }
  List_sp subseq(cl_index start, T_sp end) const;
  T_sp setf_subseq(cl_index start, T_sp end, T_sp new_subseq) { HARD_IMPLEMENT_ME(); };

  /*! Return the value associated with the property of the plist - implements CL getf */
  T_sp getf(T_sp key, T_sp defValue) const;

  inline static uintptr_t cons_header(uintptr_t val) {
    return (val & (~gctools::Header_s::mtag_mask)) | gctools::Header_s::cons_mtag;
  };
  explicit Cons_O() : _Car(nil<T_O>()), _Cdr(nil<T_O>()){};
  explicit Cons_O(T_sp car, T_sp cdr) : _Car(car), _Cdr(cdr){};
  // These are necessary because atomics are not copyable.
  // More specifically they are necessary if you want to store conses in vectors,
  // which the hash table code does.
  Cons_O(const Cons_O& other) : _Car(other.car()), _Cdr(other.cdr()){};
  Cons_O& operator=(const Cons_O& other) {
    if (this != &other) {
      setCar(other.car());
      setCdr(other.cdr());
    }
    return *this;
  }
};
}; // namespace core

namespace gctools {
template <> struct StackAllocate<core::Cons_O> {
  ConsHeader_s _Header;
  core::Cons_O _Object;

  template <class... ARGS>
  StackAllocate(ARGS&&... args)
      : _Header(ConsHeader_s::StampWtagMtag::make<core::Cons_O>()), _Object(std::forward<ARGS>(args)...){};

  smart_ptr<core::Cons_O> asSmartPtr() { return smart_ptr<core::Cons_O>((core::Cons_O*)&this->_Object); }
};

}; // namespace gctools
namespace core {

CL_PKG_NAME(ClPkg, car);
CL_LAMBDA(list)
CL_DOCSTRING("Return the first object in a list.")
DOCGROUP(clasp)
CL_DEFUN inline core::T_sp oCar(T_sp obj) {
  if (obj.consp())
    return obj.unsafe_cons()->car();
  if (obj.nilp())
    return obj;
  TYPE_ERROR(obj, cl::_sym_Cons_O);
}

CL_PKG_NAME(ClPkg, cdr);
CL_LAMBDA(list)
CL_DOCSTRING("Return all but the first object in a list.")
DOCGROUP(clasp)
CL_DEFUN inline T_sp oCdr(T_sp obj) {
  if (obj.consp())
    return obj.unsafe_cons()->cdr();
  if (obj.nilp())
    return obj;
  TYPE_ERROR(obj, cl::_sym_Cons_O);
}

CL_LAMBDA(list)
CL_DOCSTRING("Return all but the first object in a list.")
DOCGROUP(clasp)
CL_DEFUN inline T_sp cl__rest(List_sp obj) { return oCdr(obj); }

CL_PKG_NAME(ClPkg, caar);
CL_LAMBDA(list)
CL_DOCSTRING("Return the car of the first sublist.")
DOCGROUP(clasp)
CL_DEFUN inline T_sp oCaar(T_sp o) { return oCar(oCar(o)); }

CL_PKG_NAME(ClPkg, cadr);
CL_LAMBDA(list)
CL_DOCSTRING("Return the second object in a list.")
DOCGROUP(clasp)
CL_DEFUN inline T_sp oCadr(T_sp o) { return oCar(oCdr(o)); }

CL_PKG_NAME(ClPkg, cdar);
CL_LAMBDA(list)
CL_DOCSTRING("Return the cdr of the first sublist.")
DOCGROUP(clasp)
CL_DEFUN inline T_sp oCdar(T_sp o) { return oCdr(oCar(o)); }

CL_PKG_NAME(ClPkg, cddr);
CL_LAMBDA(list)
CL_DOCSTRING("Return all but the first two objects of a list.")
DOCGROUP(clasp)
CL_DEFUN inline T_sp oCddr(T_sp o) { return oCdr(oCdr(o)); }

CL_PKG_NAME(ClPkg, caaar);
CL_LAMBDA(list)
CL_DOCSTRING("Return the first object in the caar of a list.")
DOCGROUP(clasp)
CL_DEFUN inline T_sp oCaaar(T_sp o) { return oCar(oCar(oCar(o))); }

CL_PKG_NAME(ClPkg, caadr);
CL_LAMBDA(list)
CL_DOCSTRING("Return the first object in the cadr of a list.")
DOCGROUP(clasp)
CL_DEFUN inline T_sp oCaadr(T_sp o) { return oCar(oCar(oCdr(o))); }

CL_PKG_NAME(ClPkg, cadar);
CL_LAMBDA(list)
CL_DOCSTRING("Return the car of the cdar of a list.")
DOCGROUP(clasp)
CL_DEFUN inline T_sp oCadar(T_sp o) { return oCar(oCdr(oCar(o))); }

CL_PKG_NAME(ClPkg, caddr);
CL_LAMBDA(list)
CL_DOCSTRING("Return the first object in the cddr of a list.")
DOCGROUP(clasp)
CL_DEFUN inline T_sp oCaddr(T_sp o) { return oCar(oCdr(oCdr(o))); }

CL_PKG_NAME(ClPkg, cdaar);
CL_LAMBDA(list)
CL_DOCSTRING("Return the cdr of the caar of a list.")
DOCGROUP(clasp)
CL_DEFUN inline T_sp oCdaar(T_sp o) { return oCdr(oCar(oCar(o))); }

CL_PKG_NAME(ClPkg, cdadr);
CL_LAMBDA(list)
CL_DOCSTRING("Return the cdr of the cadr of a list.")
DOCGROUP(clasp)
CL_DEFUN inline T_sp oCdadr(T_sp o) { return oCdr(oCar(oCdr(o))); }

CL_PKG_NAME(ClPkg, cddar);
CL_LAMBDA(list)
CL_DOCSTRING("Return the cdr of the cdar of a list.")
DOCGROUP(clasp)
CL_DEFUN inline T_sp oCddar(T_sp o) { return oCdr(oCdr(oCar(o))); }

CL_PKG_NAME(ClPkg, cdddr);
CL_LAMBDA(list)
CL_DOCSTRING("Return the cdr of the cddr of a list.")
DOCGROUP(clasp)
CL_DEFUN inline T_sp oCdddr(T_sp o) { return oCdr(oCdr(oCdr(o))); }

CL_PKG_NAME(ClPkg, caaaar);
CL_LAMBDA(list)
CL_DOCSTRING("Return the car of the caaar of a list.")
DOCGROUP(clasp)
CL_DEFUN inline T_sp oCaaaar(T_sp o) { return oCar(oCar(oCar(oCar(o)))); }

CL_PKG_NAME(ClPkg, caadar);
CL_LAMBDA(list)
CL_DOCSTRING("Return the car of the cadar of a list.")
DOCGROUP(clasp)
CL_DEFUN inline T_sp oCaadar(T_sp o) { return oCar(oCar(oCdr(oCar(o)))); }

CL_PKG_NAME(ClPkg, cadaar);
CL_LAMBDA(list)
CL_DOCSTRING("Return the car of the cdaar of a list.")
DOCGROUP(clasp)
CL_DEFUN inline T_sp oCadaar(T_sp o) { return oCar(oCdr(oCar(oCar(o)))); }

CL_PKG_NAME(ClPkg, caddar);
CL_LAMBDA(list)
CL_DOCSTRING("Return the car of the cddar of a list.")
DOCGROUP(clasp)
CL_DEFUN inline T_sp oCaddar(T_sp o) { return oCar(oCdr(oCdr(oCar(o)))); }

CL_PKG_NAME(ClPkg, cdaaar);
CL_LAMBDA(list)
CL_DOCSTRING("Return the cdr of the caaar of a list.")
DOCGROUP(clasp)
CL_DEFUN inline T_sp oCdaaar(T_sp o) { return oCdr(oCar(oCar(oCar(o)))); }

CL_PKG_NAME(ClPkg, cdadar);
CL_LAMBDA(list)
CL_DOCSTRING("Return the cdr of the cadar of a list.")
DOCGROUP(clasp)
CL_DEFUN inline T_sp oCdadar(T_sp o) { return oCdr(oCar(oCdr(oCar(o)))); }

CL_PKG_NAME(ClPkg, cddaar);
CL_LAMBDA(list)
CL_DOCSTRING("Return the cdr of the cdaar of a list.")
DOCGROUP(clasp)
CL_DEFUN inline T_sp oCddaar(T_sp o) { return oCdr(oCdr(oCar(oCar(o)))); }

CL_PKG_NAME(ClPkg, cdddar);
CL_LAMBDA(list)
CL_DOCSTRING("Return the cdr of the cddar of a list.")
DOCGROUP(clasp)
CL_DEFUN inline T_sp oCdddar(T_sp o) { return oCdr(oCdr(oCdr(oCar(o)))); }

CL_PKG_NAME(ClPkg, caaadr);
CL_LAMBDA(list)
CL_DOCSTRING("Return the car of the caadr of a list.")
DOCGROUP(clasp)
CL_DEFUN inline T_sp oCaaadr(T_sp o) { return oCar(oCar(oCar(oCdr(o)))); }

CL_PKG_NAME(ClPkg, caaddr);
CL_LAMBDA(list)
CL_DOCSTRING("Return the car of the caddr of a list.")
DOCGROUP(clasp)
CL_DEFUN inline T_sp oCaaddr(T_sp o) { return oCar(oCar(oCdr(oCdr(o)))); }

CL_PKG_NAME(ClPkg, cadadr);
CL_LAMBDA(list)
CL_DOCSTRING("Return the car of the cdadr of a list.")
DOCGROUP(clasp)
CL_DEFUN inline T_sp oCadadr(T_sp o) { return oCar(oCdr(oCar(oCdr(o)))); }

CL_PKG_NAME(ClPkg, cadddr);
CL_LAMBDA(list)
CL_DOCSTRING("Return the car of the cdddr of a list.")
DOCGROUP(clasp)
CL_DEFUN inline T_sp oCadddr(T_sp o) { return oCar(oCdr(oCdr(oCdr(o)))); }

CL_PKG_NAME(ClPkg, cdaadr);
CL_LAMBDA(list)
CL_DOCSTRING("Return the cdr of the caadr of a list.")
DOCGROUP(clasp)
CL_DEFUN inline T_sp oCdaadr(T_sp o) { return oCdr(oCar(oCar(oCdr(o)))); }

CL_PKG_NAME(ClPkg, cdaddr);
CL_LAMBDA(list)
CL_DOCSTRING("Return the cdr of the caddr of a list.")
DOCGROUP(clasp)
CL_DEFUN inline T_sp oCdaddr(T_sp o) { return oCdr(oCar(oCdr(oCdr(o)))); }

CL_PKG_NAME(ClPkg, cddadr);
CL_LAMBDA(list)
CL_DOCSTRING("Return the cdr of the cdadr of a list.")
DOCGROUP(clasp)
CL_DEFUN inline T_sp oCddadr(T_sp o) { return oCdr(oCdr(oCar(oCdr(o)))); }

CL_PKG_NAME(ClPkg, cddddr);
CL_LAMBDA(list)
CL_DOCSTRING("Return the cdr of the cdddr of a list.")
DOCGROUP(clasp)
CL_DEFUN inline T_sp oCddddr(T_sp o) { return oCdr(oCdr(oCdr(oCdr(o)))); }

CL_PKG_NAME(ClPkg, First);
CL_LAMBDA(list)
CL_DOCSTRING("Return the first object in a list.")
DOCGROUP(clasp)
CL_DEFUN inline T_sp oFirst(T_sp o) { return oCar(o); }

CL_PKG_NAME(ClPkg, Second);
CL_LAMBDA(list)
CL_DOCSTRING("Return the secon object in a list.")
DOCGROUP(clasp)
CL_DEFUN inline T_sp oSecond(T_sp o) { return oCar(oCdr(o)); }

CL_PKG_NAME(ClPkg, Third);
CL_LAMBDA(list)
CL_DOCSTRING("Return the third object in a list.")
DOCGROUP(clasp)
CL_DEFUN inline T_sp oThird(T_sp o) { return oCar(oCdr(oCdr(o))); }

CL_PKG_NAME(ClPkg, Fourth);
CL_LAMBDA(list)
CL_DOCSTRING("Return the fourth object in a list.")
DOCGROUP(clasp)
CL_DEFUN inline T_sp oFourth(T_sp o) { return oCar(oCdr(oCdr(oCdr(o)))); }

CL_PKG_NAME(ClPkg, Fifth);
CL_LAMBDA(list)
CL_DOCSTRING("Return the fifth object in a list.")
DOCGROUP(clasp)
CL_DEFUN inline T_sp oFifth(T_sp o) { return oCar(oCdr(oCdr(oCdr(oCdr(o))))); }

CL_PKG_NAME(ClPkg, Sixth);
CL_LAMBDA(list)
CL_DOCSTRING("Return the sixth object in a list.")
DOCGROUP(clasp)
CL_DEFUN inline T_sp oSixth(T_sp o) { return oCar(oCdr(oCdr(oCdr(oCdr(oCdr(o)))))); }

CL_PKG_NAME(ClPkg, Seventh);
CL_LAMBDA(list)
CL_DOCSTRING("Return the seventh object in a list.")
DOCGROUP(clasp)
CL_DEFUN inline T_sp oSeventh(T_sp o) { return oCar(oCdr(oCdr(oCdr(oCdr(oCdr(oCdr(o))))))); }

CL_PKG_NAME(ClPkg, Eighth);
CL_LAMBDA(list)
CL_DOCSTRING("Return the eighth object in a list.")
DOCGROUP(clasp)
CL_DEFUN inline T_sp oEighth(T_sp o) { return oCar(oCdr(oCdr(oCdr(oCdr(oCdr(oCdr(oCdr(o)))))))); }

CL_PKG_NAME(ClPkg, Ninth);
CL_LAMBDA(list)
CL_DOCSTRING("Return the ninth object in a list.")
DOCGROUP(clasp)
CL_DEFUN inline T_sp oNinth(T_sp o) { return oCar(oCdr(oCdr(oCdr(oCdr(oCdr(oCdr(oCdr(oCdr(o))))))))); }

CL_PKG_NAME(ClPkg, Tenth);
CL_LAMBDA(list)
CL_DOCSTRING("Return the tenth object in a list.")
DOCGROUP(clasp)
CL_DEFUN inline T_sp oTenth(T_sp o) { return oCar(oCdr(oCdr(oCdr(oCdr(oCdr(oCdr(oCdr(oCdr(oCdr(o)))))))))); }

inline T_sp cons_car(T_sp x) {
  return x.as_assert<Cons_O>()->car();
};

inline T_sp cons_cdr(T_sp x) {
  return x.as_assert<Cons_O>()->cdr();
};

inline T_sp cons_car(Cons_sp x) {
  ASSERT(x.consp());
  return x->car();
};

inline T_sp cons_cdr(Cons_sp x) {
  ASSERT(x.consp());
  return x->cdr();
};

inline T_sp cons_car(Cons_O* x) { return x->car(); };

inline T_sp cons_cdr(Cons_O* x) { return x->cdr(); };

}; // namespace core

namespace gctools {

inline void fill_frame_list(Frame* frame, size_t& idx, core::List_sp list) {
  for (auto val : list) {
    gctools::fill_frame_one(frame, idx, CONS_CAR(val).raw_());
    ;
  }
};

}; // namespace gctools

namespace core {

template <typename T> List_sp asCons(const gctools::Vec0<T>& vec) {
  List_sp res(nil<T_O>());
  for (cl_index i(vec.size() - 1); i >= 0; --i) {
    res = Cons_O::create(vec[i], res);
  }
  return res;
}

template <typename T> void fillVec0FromCons(gctools::Vec0<T>& vec, List_sp list) {
  vec.clear();
  for (auto cur : list) {
    vec.push_back(oCar(cur));
  }
}

}; // namespace core

namespace core {
/*! Lookup the key and return the Cons containing the key/val pair - or return NIL if not found */
List_sp core__alist_assoc_eq(List_sp alist, T_sp key);
List_sp core__alist_assoc_eql(List_sp alist, T_sp key);
List_sp core__alist_assoc_equal(List_sp alist, T_sp key);

}; // namespace core

namespace core {
List_sp coerce_to_list(T_sp o);

T_sp cl__getf(List_sp plist, T_sp indicator, T_sp default_value);
List_sp core__put_f(List_sp plist, T_sp value, T_sp indicator);
T_mv core__rem_f(List_sp plist, T_sp indicator);
List_sp cl__make_list(Fixnum osize, T_sp initial_element);

void not_alist_error(T_sp l);
}; // namespace core

namespace core {
template <class T> void fillVec0(core::List_sp c, gctools::Vec0<T>& vec) {
  vec.clear();
  for (auto me : (List_sp)(c)) {
    vec.emplace_back(gc::As<T>(me->car()));
  }
}

}; // namespace core
