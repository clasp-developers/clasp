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
#ifndef _core_Cons_H //[
#define _core_Cons_H

#include <stdio.h>
#include <string>
#include <vector>
#include <set>
#include <clasp/core/cons.fwd.h>
//#include <clasp/core/lispList.h>

namespace cl {
extern core::Symbol_sp& _sym_type_error;
extern core::Symbol_sp& _sym_Cons_O;
};

namespace kw {
extern core::Symbol_sp& _sym_datum;
extern core::Symbol_sp& _sym_expected_type;
};

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

#define CONS_CAR(x) (gctools::reinterpret_cast_smart_ptr<::core::Cons_O>(x)->_Car)
#define CONS_CDR(x) (gctools::reinterpret_cast_smart_ptr<::core::Cons_O>(x)->_Cdr)
#define CAR(x) oCar(x)
#define CDR(x) oCdr(x)
#define CONSP(x) ((x).consp())
};

namespace core {
SMART(Cons);
};

template <>
struct gctools::GCInfo<core::Cons_O> {
  static bool constexpr NeedsInitialization = false;
  static bool constexpr NeedsFinalization = false;
  static GCInfo_policy constexpr Policy = normal;
};

namespace core {

  class Cons_O : public T_O {
    LISP_ABSTRACT_CLASS(core, ClPkg, Cons_O, "Cons",T_O);

    friend T_sp oCar(T_sp o);
    friend T_sp oCdr(T_sp o);
#ifdef USE_MPS
  public: // Garbage collector functions
    uintptr_t& rawRef(int idx) {return *(uintptr_t*)((uintptr_t*)this+idx);};
    uintptr_t& rawRef(int idx) const {return *(uintptr_t*)((uintptr_t*)this+idx);};
    
    bool hasGcTag() const {
      return ((((uintptr_t)(this->rawRef(0))&gctools::ptag_mask) == gctools::gc_tag));
    }
    bool fwdP() const {
      return ((((uintptr_t)(this->rawRef(0))&gctools::ptag_mask) == gctools::gc_tag)
              && (((uintptr_t)(this->rawRef(1))&gctools::ptag_mask) == gctools::Header_s::fwd_tag));
    }
    bool pad1P() const {
      return ((uintptr_t)(this->rawRef(0)) == gctools::gc_tag);
    }
    bool padP() const {
      return ((((uintptr_t)(this->rawRef(0))&gctools::ptag_mask) == gctools::gc_tag)
              && (((uintptr_t)(this->rawRef(1))&gctools::ptag_mask) == gctools::Header_s::pad_tag));
    }
    size_t padSize() const {
      size_t sz = (size_t)(((uintptr_t)this->rawRef(1)) >> gctools::tag_shift);
      return sz;
    }
    void setFwdPointer(void* ptr) {
      this->rawRef(0) = (uintptr_t)((uintptr_t)(ptr) | gctools::gc_tag);
      this->rawRef(1) = (uintptr_t)(gctools::Header_s::fwd_tag);
    }
    void* fwdPointer() {
      return (void*)((uintptr_t)(this->rawRef(0)) & gctools::ptr_mask);
    }
    void setPad1()
    {
    // Just a gc_tag means pad1
      this->rawRef(0) = (uintptr_t)(gctools::gc_tag | 0);
    }
    void setPad(size_t sz)
    {
      this->rawRef(0) = (uintptr_t)(gctools::gc_tag | gctools::ptr_mask);
      this->rawRef(1) = (uintptr_t)(gctools::Header_s::pad_tag | (sz << gctools::tag_shift));
    }
#endif

  public:
    T_sp _Car;
    T_sp _Cdr;

  public:
    template <class T>
      static List_sp createFromVec0(const gctools::Vec0<T> &vec) {
      List_sp res(_Nil<T_O>());
      for (cl_index i(vec.size() - 1); i >= 0; --i) {
        res = Cons_O::create(vec[i], res);
      }
      return res;
    }

    static Cons_sp createFrom_va_list(va_list &va_args);
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
    static Cons_sp create(T_sp car, T_sp cdr) {
      gctools::smart_ptr<Cons_O> ll = gctools::ConsAllocator<Cons_O>::allocate(car,cdr);
      return ll;
    };
  public:
    inline static int car_offset() {
      Cons_O x;
      return (int)(reinterpret_cast<char *>(&x._Car) - reinterpret_cast<char *>(&x));
    }
    inline static int cdr_offset() {
      Cons_O x;
      return (int)(reinterpret_cast<char *>(&x._Cdr) - reinterpret_cast<char *>(&x));
    }

  public:
    static void appendInto(T_sp head, T_sp *&tailP, T_sp l);
    static T_sp append(List_sp x, List_sp y);

  public:
  /*! Recursively hash the car and cdr parts - until the HashGenerator fills up */
    inline void sxhash_(HashGenerator &hg) const {
      if (hg.isFilling())
        hg.hashObject(this->_Car);
      if (hg.isFilling())
        hg.hashObject(this->_Cdr);
    }


    inline Cons_sp rplaca(T_sp o) {
      this->_Car = o;
#ifdef DEBUG_VALIDATE_GUARD
      client_validate(this->_Car.raw_());
#endif
      return this->asSmartPtr();
    };
    inline Cons_sp rplacd(T_sp o) {
      this->_Cdr = o;
#ifdef DEBUG_VALIDATE_GUARD
      client_validate(this->_Cdr.raw_());
#endif
      return this->asSmartPtr();
    };

    T_sp onth(cl_index idx) const;
    T_sp onthcdr(cl_index idx) const;

    T_sp elt(cl_index index) const;
    T_sp setf_elt(cl_index index, T_sp value);

  /* TODO:
	   Remove the following member functions and replace them
	   with the real functions  oCar, oCdr, cCdr etc */

    inline T_sp cdr() const { return this->_Cdr; };
    inline T_sp ocar() const { return this->_Car; };
    inline T_sp ocadr() const {
      T_sp cdr = this->_Cdr;
      if (UNLIKELY(!this->_Cdr.consp()))
        return _Nil<T_O>();
      return this->_Cdr.unsafe_cons()->ocar();
    // return this->cdr()->ocar(); };
    }
    T_sp ocaddr() const {
      TESTING();
      if (UNLIKELY(!this->_Cdr.consp()))
        return _Nil<T_O>();
      return this->_Cdr.unsafe_cons()->ocadr();
    //return this->cdr()->cdr()->ocar();
    }

  /*! Set the data for this element */
    inline void setCar(T_sp o) {
      this->_Car = o;
    };

  /*! Get the data for the first element */
    template <class o_class>
      gctools::smart_ptr<o_class> car() {
      ASSERTNOTNULL(this->_Car);
      return gc::As<gc::smart_ptr<o_class>>(this->_Car);
    };

    bool equal(T_sp obj) const;
    bool equalp(T_sp obj) const;

    T_sp setf_nth(cl_index index, T_sp val);

  /*! Return a new list by combinding the given list of elements to our list
	 */
    List_sp extend(List_sp rest);

    List_sp revappend(T_sp tail);
    List_sp nreconc(T_sp tail);

  /*! Set the next pointer for this element */
    void setCdr(T_sp o);

    CL_LISPIFY_NAME("core:cons-setf-cdr");
    CL_DEFMETHOD   T_sp setf_cdr(T_sp o) {
      this->setCdr(o);
      return o;
    };
  /*! Return the last cons (not the last element) of list.
	  If we are nil then return nil */
    T_sp last(cl_index idx = 1) const;

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
    T_sp setf_subseq(cl_index start, T_sp end, T_sp new_subseq) {
      HARD_IMPLEMENT_ME();
    };

  /*! Return the value associated with the property of the plist - implements CL getf */
    T_sp getf(T_sp key, T_sp defValue) const;

    explicit Cons_O();
    explicit Cons_O(T_sp car, T_sp cdr) : _Car(car), _Cdr(cdr){};
  };

 CL_PKG_NAME(ClPkg,car);
CL_DEFUN inline core::T_sp oCar(T_sp obj) {
   if (obj.consp())
     return obj.unsafe_cons()->_Car;
   if (obj.nilp())
     return obj;
   TYPE_ERROR(obj, cl::_sym_Cons_O);
 };
 CL_PKG_NAME(ClPkg,cdr);
 CL_DEFUN inline T_sp oCdr(T_sp obj) {
  if (obj.consp())
    return obj.unsafe_cons()->_Cdr;
  if (obj.nilp())
    return obj;
  TYPE_ERROR(obj, cl::_sym_Cons_O);
};

 CL_DEFUN inline T_sp cl__rest(List_sp obj) {
   return oCdr(obj);
};

 CL_PKG_NAME(ClPkg,caar);
CL_DEFUN inline T_sp oCaar(T_sp o) { return oCar(oCar(o)); };
 CL_PKG_NAME(ClPkg,cadr);
 CL_DEFUN inline T_sp oCadr(T_sp o) { return oCar(oCdr(o)); };
 CL_PKG_NAME(ClPkg,cdar);
 CL_DEFUN inline T_sp oCdar(T_sp o) { return oCdr(oCar(o)); };
 CL_PKG_NAME(ClPkg,cddr);
 CL_DEFUN inline T_sp oCddr(T_sp o) { return oCdr(oCdr(o)); };
 CL_PKG_NAME(ClPkg,caaar);
 CL_DEFUN inline T_sp oCaaar(T_sp o) { return oCar(oCar(oCar(o))); };
 CL_PKG_NAME(ClPkg,caadr);
 CL_DEFUN inline T_sp oCaadr(T_sp o) { return oCar(oCar(oCdr(o))); };
 CL_PKG_NAME(ClPkg,cadar);
 CL_DEFUN inline T_sp oCadar(T_sp o) { return oCar(oCdr(oCar(o))); };
 CL_PKG_NAME(ClPkg,caddr);
 CL_DEFUN inline T_sp oCaddr(T_sp o) { return oCar(oCdr(oCdr(o))); };
 CL_PKG_NAME(ClPkg,cdaar);
 CL_DEFUN inline T_sp oCdaar(T_sp o) { return oCdr(oCar(oCar(o))); };
 CL_PKG_NAME(ClPkg,cdadr);
 CL_DEFUN inline T_sp oCdadr(T_sp o) { return oCdr(oCar(oCdr(o))); };
 CL_PKG_NAME(ClPkg,cddar);
 CL_DEFUN inline T_sp oCddar(T_sp o) { return oCdr(oCdr(oCar(o))); };
 CL_PKG_NAME(ClPkg,cdddr);
 CL_DEFUN inline T_sp oCdddr(T_sp o) { return oCdr(oCdr(oCdr(o))); };
 CL_PKG_NAME(ClPkg,caaaar);
 CL_DEFUN inline T_sp oCaaaar(T_sp o) { return oCar(oCar(oCar(o))); };
 CL_PKG_NAME(ClPkg,caadar);
 CL_DEFUN inline T_sp oCaadar(T_sp o) { return oCar(oCar(oCdr(oCar(o)))); };
 CL_PKG_NAME(ClPkg,cadaar);
 CL_DEFUN inline T_sp oCadaar(T_sp o) { return oCar(oCdr(oCar(oCar(o)))); };
 CL_PKG_NAME(ClPkg,caddar);
 CL_DEFUN inline T_sp oCaddar(T_sp o) { return oCar(oCdr(oCdr(oCar(o)))); };
 CL_PKG_NAME(ClPkg,cdaaar);
 CL_DEFUN inline T_sp oCdaaar(T_sp o) { return oCdr(oCar(oCar(oCar(o)))); };
 CL_PKG_NAME(ClPkg,cdadar);
 CL_DEFUN inline T_sp oCdadar(T_sp o) { return oCdr(oCar(oCdr(oCar(o)))); };
 CL_PKG_NAME(ClPkg,cddaar);
 CL_DEFUN inline T_sp oCddaar(T_sp o) { return oCdr(oCdr(oCar(oCar(o)))); };
 CL_PKG_NAME(ClPkg,cdddar);
 CL_DEFUN inline T_sp oCdddar(T_sp o) { return oCdr(oCdr(oCdr(oCar(o)))); };
 CL_PKG_NAME(ClPkg,caaadr);
 CL_DEFUN inline T_sp oCaaadr(T_sp o) { return oCar(oCar(oCar(oCar(o)))); };
 CL_PKG_NAME(ClPkg,caaddr);
 CL_DEFUN inline T_sp oCaaddr(T_sp o) { return oCar(oCar(oCdr(oCdr(o)))); };
 CL_PKG_NAME(ClPkg,cadadr);
 CL_DEFUN inline T_sp oCadadr(T_sp o) { return oCar(oCdr(oCar(oCdr(o)))); };
 CL_PKG_NAME(ClPkg,cadddr);
 CL_DEFUN inline T_sp oCadddr(T_sp o) { return oCar(oCdr(oCdr(oCdr(o)))); };
 CL_PKG_NAME(ClPkg,cdaadr);
 CL_DEFUN inline T_sp oCdaadr(T_sp o) { return oCdr(oCar(oCar(oCdr(o)))); };
 CL_PKG_NAME(ClPkg,cdaddr);
 CL_DEFUN inline T_sp oCdaddr(T_sp o) { return oCdr(oCar(oCdr(oCdr(o)))); };
 CL_PKG_NAME(ClPkg,cddadr);
 CL_DEFUN inline T_sp oCddadr(T_sp o) { return oCdr(oCdr(oCar(oCdr(o)))); };
 CL_PKG_NAME(ClPkg,cddddr);
 CL_DEFUN inline T_sp oCddddr(T_sp o) { return oCdr(oCdr(oCdr(oCdr(o)))); };
 CL_PKG_NAME(ClPkg,First);
 CL_DEFUN inline T_sp oFirst(T_sp o) { return oCar(o); };
 CL_PKG_NAME(ClPkg,Second);
 CL_DEFUN inline T_sp oSecond(T_sp o) { return oCar(oCdr(o)); };
 CL_PKG_NAME(ClPkg,Third);
 CL_DEFUN inline T_sp oThird(T_sp o) { return oCar(oCdr(oCdr(o))); };
 CL_PKG_NAME(ClPkg,Fourth);
 CL_DEFUN inline T_sp oFourth(T_sp o) { return oCar(oCdr(oCdr(oCdr(o)))); };
 CL_PKG_NAME(ClPkg,Fifth);
 CL_DEFUN inline T_sp oFifth(T_sp o) { return oCar(oCdr(oCdr(oCdr(oCdr(o))))); };
 CL_PKG_NAME(ClPkg,Sixth);
 CL_DEFUN inline T_sp oSixth(T_sp o) { return oCar(oCdr(oCdr(oCdr(oCdr(oCdr(o)))))); };
 CL_PKG_NAME(ClPkg,Seventh);
 CL_DEFUN inline T_sp oSeventh(T_sp o) { return oCar(oCdr(oCdr(oCdr(oCdr(oCdr(oCdr(o))))))); };
 CL_PKG_NAME(ClPkg,Eighth);
 CL_DEFUN inline T_sp oEighth(T_sp o) { return oCar(oCdr(oCdr(oCdr(oCdr(oCdr(oCdr(oCdr(o)))))))); };
 CL_PKG_NAME(ClPkg,Ninth);
 CL_DEFUN inline T_sp oNinth(T_sp o) { return oCar(oCdr(oCdr(oCdr(oCdr(oCdr(oCdr(oCdr(oCdr(o))))))))); };
 CL_PKG_NAME(ClPkg,Tenth);
 CL_DEFUN inline T_sp oTenth(T_sp o) { return oCar(oCdr(oCdr(oCdr(oCdr(oCdr(oCdr(oCdr(oCdr(oCdr(o)))))))))); };


 inline T_sp cons_car(T_sp x) {ASSERT(x.consp());return gctools::reinterpret_cast_smart_ptr<Cons_O>(x)->_Car;};
 inline T_sp cons_cdr(T_sp x) {ASSERT(x.consp());return gctools::reinterpret_cast_smart_ptr<Cons_O>(x)->_Cdr;};
 inline T_sp cons_car(Cons_sp x) {ASSERT(x.consp());return x->_Car;};
 inline T_sp cons_cdr(Cons_sp x) {ASSERT(x.consp());return x->_Cdr;};
 inline T_sp cons_car(Cons_O* x) {return x->_Car;};
 inline T_sp cons_cdr(Cons_O* x) {return x->_Cdr;};

};



//TRANSLATE(core::Cons_O);


namespace core {

template <typename T>
List_sp asCons(const gctools::Vec0<T> &vec) {
  List_sp res(_Nil<T_O>());
  for (cl_index i(vec.size() - 1); i >= 0; --i) {
    res = Cons_O::create(vec[i], res);
  }
  return res;
}

template <typename T>
void fillVec0FromCons(gctools::Vec0<T> &vec, List_sp list) {
  vec.clear();
  for (auto cur : list) {
    vec.push_back(oCar(cur));
  }
}

}; // core namespace

namespace core {
/*! Lookup the key and return the Cons containing the key/val pair - or return NIL if not found */
List_sp core__alist_assoc_eq(List_sp alist, T_sp key);
List_sp core__alist_assoc_eql(List_sp alist, T_sp key);
 
};

namespace core {
List_sp coerce_to_list(T_sp o);

T_sp cl__getf(List_sp plist, T_sp indicator, T_sp default_value);
List_sp core__put_f(List_sp plist, T_sp value, T_sp indicator);
T_mv core__rem_f(List_sp plist, Symbol_sp indicator);
 List_sp cl__make_list(Fixnum_sp osize, T_sp initial_element);

 void not_alist_error(T_sp l);
};

namespace core {
template <class T>
void fillVec0(core::List_sp c, gctools::Vec0<T> &vec) {
  vec.clear();
  for (auto me : (List_sp)(c)) {
    vec.emplace_back(gc::As<T>(me->_Car));
  }
}

};
#endif //]
