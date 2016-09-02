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
extern core::Symbol_sp& _sym_typeError;
extern core::Symbol_sp& _sym_Cons_O;
};

namespace kw {
extern core::Symbol_sp& _sym_datum;
extern core::Symbol_sp& _sym_expectedType;
};

namespace core {

T_sp oCar(List_sp obj);
T_sp oCdr(List_sp obj);
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

#define CONS_CAR(x) (gctools::reinterpret_cast_smart_ptr<Cons_O>(x)->_Car)
#define CONS_CDR(x) (gctools::reinterpret_cast_smart_ptr<Cons_O>(x)->_Cdr)
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

    friend T_sp oCar(List_sp o);
    friend T_sp oCdr(List_sp o);
#ifdef USE_MPS
  public: // Garbage collector functions
    uintptr_t& rawRef(int idx) {return *(uintptr_t*)((uintptr_t*)this+idx);};
    uintptr_t& rawRef(int idx) const {return *(uintptr_t*)((uintptr_t*)this+idx);};
    
    bool hasGcTag() const {
      return ((((uintptr_t)(this->rawRef(0))&gctools::tag_mask) == gctools::gc_tag));
    }
    bool fwdP() const {
      return ((((uintptr_t)(this->rawRef(0))&gctools::tag_mask) == gctools::gc_tag)
              && (((uintptr_t)(this->rawRef(1))&gctools::tag_mask) == gctools::Header_s::fwd_tag));
    }
    bool pad1P() const {
      return ((uintptr_t)(this->rawRef(0)) == gctools::gc_tag);
    }
    bool padP() const {
      return ((((uintptr_t)(this->rawRef(0))&gctools::tag_mask) == gctools::gc_tag)
              && (((uintptr_t)(this->rawRef(1))&gctools::tag_mask) == gctools::Header_s::pad_tag));
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
    typedef T_O CarType_O;
    typedef T_O CdrType_O;
    typedef T_sp CarType_sp;
    typedef T_sp CdrType_sp;

  public:
    CarType_sp _Car;
    CdrType_sp _Cdr;
  /*! Keep track of the length of the cons along the cdr chain
	 * every time the cdr is set add 1 to the length of the cons
	 * being added and store that here.  This will keep a running
	 * tab of how many cons elements are in the list.
	 */
  //	uint		_CdrLength;	// Keep track of the length of the cons
  public:
    template <class T>
      static List_sp createFromVec0(const gctools::Vec0<T> &vec) {
      List_sp res = _Nil<T_O>();
      for (int i(vec.size() - 1); i >= 0; --i) {
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
    inline static Cons_sp create(T_sp car, T_sp cdr) {
      gctools::smart_ptr<Cons_O> ll = gctools::ConsAllocator<Cons_O>::allocate(car,cdr);
      return ll;
    };
    inline static Cons_sp create(T_sp obj) {
      return create(obj,_Nil<T_O>());
    }

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

    T_sp onth(int idx) const;
    List_sp onthcdr(int idx) const;

    T_sp elt(int index) const;
    T_sp setf_elt(int index, T_sp value);

    CdrType_sp *cdrPtr() { return &(this->_Cdr); };

  /* TODO:
	   Remove the following member functions and replace them
	   with the real functions  oCar, oCdr, cCdr etc */

    inline T_sp cdr() const { return this->_Cdr; };
    inline T_sp ocar() const { return this->_Car; };
    inline T_sp ocadr() const {
      TESTING();
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

#if 0
    Cons_sp cdr() const 	{ return this->_Cdr.as<Cons_O>();};
    Cons_sp cddr() const 	{ return oCdr(this->_Cdr).as<Cons_O>();};
    Cons_sp cdddr() const 	{ return oCddr(this->_Cdr).as<Cons_O>();};
    Cons_sp cddddr() const 	{ return oCdddr(this->_Cdr).as<Cons_O>();};
#endif
  /*! Set the data for this element */
    inline void setCar(T_sp o) {
      this->_Car = o;
    };

    CL_LISPIFY_NAME("core:cons-setf-car");
    CL_DEFMETHOD   T_sp setf_car(T_sp o) {
      ANN(o);
      this->_Car = o;
      return o;
    };

  /*! Get the data for the first element */
    template <class o_class>
      gctools::smart_ptr<o_class> car() {
      ASSERTNOTNULL(this->_Car);
      return gc::As<gc::smart_ptr<o_class>>(this->_Car);
    };

    bool equal(T_sp obj) const;
    bool equalp(T_sp obj) const;

    T_sp setf_nth(int index, T_sp val);

  /*! Return a Cons that has all the same elements
	 * in the same order but with nil objects removed.
	 */
    List_sp filterOutNil();

  /*! Return a new list by combinding the given list of elements to our list
	 */
    List_sp extend(List_sp rest);

  /*! Return the reversed list */
    List_sp reverse();

  /*! Return the reversed list */
    List_sp nreverse();

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
    List_sp last(int idx = 1) const;

  /*! Like Common Lisp copy-list */
    List_sp copyList() const;

  /*! Like Common Lisp copy-list */
    Cons_sp copyListCar() const;

  /*! Like Common Lisp copy-tree */
    List_sp copyTree() const;

  /*! Return a new Cons with a tree copy of the current car*/
    List_sp copyTreeCar() const;

  /*! Return the number of elements in the list*/
    uint length() const;

  /*! Calculate the length the fastest way I can think of */
    inline uint fastUnsafeLength() const {
      TESTING();
      uint sz = 1;
      T_sp cur = this->_Cdr;
      while (cur.consp()) {
        ++sz;
        cur = cur.unsafe_cons()->_Cdr;
      }
      return sz;
#if 0
      uint sz=1;
      Cons_O* cur = reinterpret_cast<Cons_O*>(this->_Cdr.px_ref());
      while (!gctools::tagged_ptr<Cons_O>::tagged_nilp(cur)) {
        ++sz;
        cur = reinterpret_cast<Cons_O*>(reinterpret_cast<Cons_O*>(cur)->_Cdr.px_ref());
      }
      return sz;
#endif
    }

#if 0
	/*! Return an arbitrary member of the list or an empty member*/
    template <class o_class>
      gctools::smart_ptr<o_class> listref(int i) { return this->olistref(i).as<o_class>();};
#endif

  /*! Return an arbitrary member of the list or an empty member*/
    T_sp olistref(int index);

  /*! Return an arbitrary member of the list or an empty member - used only for passing arguments from Lisp to C++*/
    T_sp olistrefArgument(int index);

  /*! Lookup the association given a key 
	 * (associations are two element lists or KeyedObjects)
	 */
    T_sp olookupKeyObject(Symbol_sp key);

    T_sp olookupKeyObjectDefault(Symbol_sp key, T_sp dflt);

    List_sp memberEq(T_sp item) const;
    List_sp memberEql(T_sp item) const;

    List_sp member1(T_sp item, T_sp key, T_sp test, T_sp testNot) const;
    List_sp member(T_sp item, T_sp key, T_sp test, T_sp testNot) const;
    List_sp assoc(T_sp item, T_sp key, T_sp test, T_sp testNot) const;

  /*! Return true if every CAR of other matches the cooresponding CAR of this pointer */
    bool exactlyMatches(List_sp other) const;

  /*!If the Cons doesn't contained KeyedObjects
	 * return nil.
	 * If it does and they are all at the end of the list
	 * then set the cdr of the last non-keyed entry to nil
	 * and return the Cons of the first KeyedObject.
	 * This will separate the list into two lists.
	 * If they aren't all at the end of the list then throw
	 * an exception
	 */
  //	void splitListIntoNonKeyedAndKeyedObjects(Cons_sp& nonKeyed, Cons_sp& keyed);
  //	void splitListIntoNonKeyedListAndDictionary(Cons_sp& nonKeyed, ObjectDictionary_sp& keyed,bool& allArgsIn__args);

    template <class string>
      T_sp olookup(Symbol_sp key) {
      return this->olookupKeyObject(key);
    }
    template <class oreturnType, class string>
      gctools::smart_ptr<oreturnType> lookup(Symbol_sp s) {
      T_sp oret = this->olookupKeyObject(s);
      return gc::As<oreturnType>(oret);
    }

    void describe(T_sp stream);
    string __repr__() const;
    void __write__(T_sp stream) const;

    T_sp product(Cons_sp list);
    T_sp max(Cons_sp list);
    T_sp min(Cons_sp list);
    T_sp booleanOr(Cons_sp list);
    T_sp booleanAnd(Cons_sp list);

  /*!Set the owner of every car in the list
	 */
  //	void setOwnerOfAllEntries(T_sp obj);

    inline bool eq(T_sp o) const {
      if (o.consp()) {
        return this == o.unsafe_cons();
      }
      return false;
    }
    List_sp subseq(int start, T_sp end) const;
    T_sp setf_subseq(int start, T_sp end, T_sp new_subseq) {
      _G();
      IMPLEMENT_ME();
    };

  /*! Return the value associated with the property of the plist - implements CL getf */
    T_sp getf(T_sp key, T_sp defValue) const;

    explicit Cons_O();
    explicit Cons_O(T_sp car, T_sp cdr) : _Car(car), _Cdr(cdr){};
  };

//
// The LispParserPos structure is used to keep track of the parse position
// while parsing text
//

  typedef struct {
    int first_line;
    int first_column;
    int last_line;
    int last_column;
  } LispParserPos;

 CL_PKG_NAME(ClPkg,car);
CL_DEFUN inline core::T_sp oCar(List_sp obj) {
   if (obj.consp())
     return obj.unsafe_cons()->_Car;
   if (obj.nilp())
     return obj;
   TYPE_ERROR(obj, cl::_sym_Cons_O);
 };
 CL_PKG_NAME(ClPkg,cdr);
 CL_DEFUN inline T_sp oCdr(List_sp obj) {
  if (obj.consp())
    return obj.unsafe_cons()->_Cdr;
  if (obj.nilp())
    return obj;
  TYPE_ERROR(obj, cl::_sym_Cons_O);
};

 CL_DEFUN inline T_sp cl__rest(List_sp obj) {
   return oCdr(obj);
};

// inline Cons_sp cCar(Cons_sp obj) { return obj->_Car.as<Cons_O>();};
// inline Cons_sp cCdr(Cons_sp obj) { return obj->_Cdr.as<Cons_O>();};

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
  for (int i(vec.size() - 1); i >= 0; --i) {
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
/* Erase the entry with _key_ from the list. Return the new list. 
     In cases where the key was in the first entry the first entry is unhooked and the CDR is returned.
    In other cases the entry is unhooked from the inside of the alist*/
List_sp core__alist_erase(List_sp alist, T_sp key);

/*! Push the key/val onto the alist.  This will shadow other entries with the same val */
List_sp core__alist_push(List_sp alist, T_sp key, T_sp val);

/*! Lookup the key and return the Cons containing the key/val pair - or return NIL if not found */
List_sp core__alist_get(List_sp alist, T_sp key);

string core__alist_asString(List_sp alist);
};

namespace core {
List_sp plistErase(List_sp &plist, T_sp key);
List_sp plistSetf(List_sp &plist, T_sp key, T_sp val);
T_sp plistGetf(List_sp plist, T_sp key, T_sp defaultValue);
};

namespace core {
List_sp coerce_to_list(T_sp o);

T_sp cl__getf(List_sp plist, T_sp indicator, T_sp default_value);
List_sp core__put_f(List_sp plist, T_sp value, T_sp indicator);
T_mv core__rem_f(List_sp plist, Symbol_sp indicator);
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
