/*
    File: cons.cc
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
// #define DEBUG_LEVEL_FULL

#include <atomic> // memory_order_etc
#include <clasp/core/foundation.h>
#include <clasp/core/object.h>
#include <clasp/core/lisp.h>
#include <clasp/core/corePackage.h>
#include <clasp/core/ql.h>
#include <clasp/core/sourceFileInfo.h>
#include <clasp/core/cons.h>
#include <clasp/core/predicates.h>
#include <clasp/core/symbolTable.h>
#include <clasp/core/debugger.h>
#include <clasp/core/evaluator.h>
#include <clasp/core/designators.h>
#include <clasp/core/numbers.h>
#include <clasp/core/array.h>
#include <clasp/core/wrappers.h>

namespace core {

List_sp coerce_to_list(T_sp o) {
  if (o.consp()) {
    return gctools::smart_ptr<List_V>((gctools::Tagged)(o.raw_()));
  } else if (o.nilp()) {
    return gctools::smart_ptr<List_V>((gctools::Tagged)(o.raw_()));
  }
  TYPE_ERROR(o, cl::_sym_list);
}

CL_LAMBDA(plist value indicator);
CL_DECLARE();
CL_UNWIND_COOP(true);
CL_DOCSTRING(R"dx(putF)dx");
DOCGROUP(clasp);
CL_DEFUN List_sp core__put_f(List_sp place, T_sp value, T_sp indicator) {
  auto it = place.begin();
  auto end = place.end();
  while (it != end) {
    List_sp cur = *it; // cur is guaranteed to be a Cons_sp
    ++it;
    if (it == end) // odd plist
      SIMPLE_ERROR("odd plist: {}", _rep_(place));
    if (oCar(cur) == indicator) {
      (*it)->rplaca(value); // *it is guaranteed to be a Cons_sp
      return place;
    }
    ++it;
  }
  place = Cons_O::create(value, place);
  place = Cons_O::create(indicator, place);
  return place;
};

CL_LAMBDA(plist indicator &optional default-value);
CL_DECLARE();
CL_UNWIND_COOP(true);
CL_DOCSTRING(R"dx(getf)dx");
DOCGROUP(clasp);
CL_DEFUN T_sp cl__getf(List_sp plist, T_sp indicator, T_sp default_value) {
  if (plist.consp())
    return gc::As_unsafe<Cons_sp>(plist)->getf(indicator, default_value);
  return default_value;
};

CL_LAMBDA(plist indicator);
CL_DECLARE();
CL_UNWIND_COOP(true);
CL_DOCSTRING(
    R"dx(Removes the property with the indicator from the property list in place if present and returns MultipleValues with the new property list and T if the property was found)dx");
DOCGROUP(clasp);
CL_DEFUN T_mv core__rem_f(List_sp plist, T_sp indicator) {
  if (oCar(plist) == indicator) {
    plist = oCddr(plist);
    T_sp tplist = plist;
    return (Values(tplist, _lisp->_true()));
  }
  for (List_sp cur = plist; oCddr(cur).notnilp(); cur = oCddr(cur)) {
    T_sp k = oCaddr(cur);
    if (k == indicator) {
      List_sp curcdr = oCdr(cur);
      curcdr.asCons()->setCdr(oCddr(oCddr(cur)));
      T_sp tplist = plist;
      return (Values(tplist, _lisp->_true()));
    }
  }
  T_sp tplist = plist;
  return (Values(tplist, _lisp->_false()));
};

CL_LAMBDA(object1 object2);
CL_DECLARE();
CL_DOCSTRING(R"dx(cons)dx");
CL_UNWIND_COOP(true);
DOCGROUP(clasp);
CL_DEFUN Cons_sp cl__cons(T_sp obj1, T_sp obj2) { return Cons_O::create(obj1, obj2); };

CL_LAMBDA(order cons);
CL_DECLARE();
CL_DOCSTRING(R"dx(car-atomic)dx");
CL_UNWIND_COOP(true);
DOCGROUP(clasp);
CL_DEFUN T_sp core__car_atomic(T_sp order, Cons_sp c) {
  // FIXME: We laze out here and ignore the order.
  // This is valid but could make clever concurrent code less efficient.
  return c->carAtomic(std::memory_order_seq_cst);
}

CL_LAMBDA(order cons);
CL_DECLARE();
CL_DOCSTRING(R"dx(cdr-atomic)dx");
CL_UNWIND_COOP(true);
DOCGROUP(clasp);
CL_DEFUN T_sp core__cdr_atomic(T_sp order, Cons_sp c) { return c->cdrAtomic(std::memory_order_seq_cst); }

CL_LAMBDA(c o);
CL_DECLARE();
CL_UNWIND_COOP(true);
CL_DOCSTRING(R"dx()dx");
DOCGROUP(clasp);
CL_DEFUN Cons_sp cl__rplaca(Cons_sp c, T_sp o) { return c->rplaca(o); };

CL_LAMBDA(c o);
CL_DECLARE();
CL_UNWIND_COOP(true);
CL_DOCSTRING(R"dx()dx");
DOCGROUP(clasp);
CL_DEFUN Cons_sp cl__rplacd(Cons_sp c, T_sp o) { return c->rplacd(o); };

CL_LAMBDA(order value cons);
CL_DECLARE();
CL_UNWIND_COOP(true);
CL_DOCSTRING(R"dx()dx");
DOCGROUP(clasp);
CL_DEFUN Cons_sp core__rplaca_atomic(T_sp order, T_sp value, Cons_sp c) {
  c->setCarAtomic(value, std::memory_order_seq_cst);
  return c;
}

CL_LAMBDA(order value cons);
CL_DECLARE();
CL_UNWIND_COOP(true);
CL_DOCSTRING(R"dx()dx");
DOCGROUP(clasp);
CL_DEFUN Cons_sp core__rplacd_atomic(T_sp order, T_sp value, Cons_sp c) {
  c->setCdrAtomic(value, std::memory_order_seq_cst);
  return c;
}

CL_LAMBDA(order old new cons);
CL_DECLARE();
CL_UNWIND_COOP(true);
CL_DOCSTRING(R"dx()dx");
DOCGROUP(clasp);
CL_DEFUN T_sp core__cas_car(T_sp order, T_sp old, T_sp newv, Cons_sp c) {
  c->carCAS(old, newv, std::memory_order_seq_cst);
  return old;
}

CL_LAMBDA(order old new cons);
CL_DECLARE();
CL_UNWIND_COOP(true);
CL_DOCSTRING(R"dx()dx");
DOCGROUP(clasp);
CL_DEFUN T_sp core__cas_cdr(T_sp order, T_sp old, T_sp newv, Cons_sp c) {
  c->cdrCAS(old, newv, std::memory_order_seq_cst);
  return old;
}

CL_LAMBDA(osize &key initial-element);
CL_DECLARE();
CL_UNWIND_COOP(true);
CL_DOCSTRING(R"dx(make_list)dx");
DOCGROUP(clasp);
CL_DEFUN List_sp cl__make_list(Fixnum size, T_sp initial_element) {
  // Might be a negative Fixnum, take the right type, size_t is unsigned
  // osize must be 0 or positive
  if (size < 0)
    TYPE_ERROR(make_fixnum(size), cl::_sym_UnsignedByte);
  else {
    T_sp result = nil<T_O>();
    for (size_t i = 0; i < size; i++) {
      result = Cons_O::create(initial_element, result);
    }
    return result;
  }
};

Cons_sp Cons_O::createList(T_sp o1) { return (Cons_O::create(o1, nil<T_O>())); }

Cons_sp Cons_O::createList(T_sp o1, T_sp o2) { return (Cons_O::create(o1, Cons_O::create(o2, nil<T_O>()))); }

Cons_sp Cons_O::createList(T_sp o1, T_sp o2, T_sp o3) { return ((Cons_O::create(o1, Cons_O::createList(o2, o3)))); }

Cons_sp Cons_O::createList(T_sp o1, T_sp o2, T_sp o3, T_sp o4) { return ((Cons_O::create(o1, Cons_O::createList(o2, o3, o4)))); }

Cons_sp Cons_O::createList(T_sp o1, T_sp o2, T_sp o3, T_sp o4, T_sp o5) {
  return ((Cons_O::create(o1, Cons_O::createList(o2, o3, o4, o5))));
}

Cons_sp Cons_O::createList(T_sp o1, T_sp o2, T_sp o3, T_sp o4, T_sp o5, T_sp o6) {
  return ((Cons_O::create(o1, Cons_O::createList(o2, o3, o4, o5, o6))));
}

Cons_sp Cons_O::createList(T_sp o1, T_sp o2, T_sp o3, T_sp o4, T_sp o5, T_sp o6, T_sp o7) {
  return Cons_O::create(o1, Cons_O::createList(o2, o3, o4, o5, o6, o7));
}
Cons_sp Cons_O::createList(T_sp o1, T_sp o2, T_sp o3, T_sp o4, T_sp o5, T_sp o6, T_sp o7, T_sp o8) {
  return Cons_O::create(o1, Cons_O::createList(o2, o3, o4, o5, o6, o7, o8));
}
Cons_sp Cons_O::createList(T_sp o1, T_sp o2, T_sp o3, T_sp o4, T_sp o5, T_sp o6, T_sp o7, T_sp o8, T_sp o9) {
  return Cons_O::create(o1, Cons_O::createList(o2, o3, o4, o5, o6, o7, o8, o9));
}
Cons_sp Cons_O::createList(T_sp o1, T_sp o2, T_sp o3, T_sp o4, T_sp o5, T_sp o6, T_sp o7, T_sp o8, T_sp o9, T_sp o10) {
  return Cons_O::create(o1, Cons_O::createList(o2, o3, o4, o5, o6, o7, o8, o9, o10));
}

CL_LAMBDA(l1 l2);
CL_DECLARE();
CL_UNWIND_COOP(true);
CL_DOCSTRING(R"dx(append2 - append l2 to l1 by copying l1 and pointing the end of it to l2)dx");
DOCGROUP(clasp);
CL_DEFUN T_sp core__append2(List_sp x, List_sp y) { return Cons_O::append(x, y); };

T_sp Cons_O::append(List_sp x, List_sp y) {
  ql::list result;
  while (x.notnilp()) {
    result << cons_car(x);
    x = cons_cdr(x);
  }
  result.dot(y);
  return result.result();
}

struct Tester {
  T_sp _item;
  Function_sp _test_func;
  bool _test_pass;
  bool _use_key_func;
  Function_sp _key_func;
  Tester(T_sp item, T_sp key, T_sp test, T_sp testNot, bool applyKey) { this->setup(item, key, test, testNot, applyKey); }

  void setup(T_sp item, T_sp key, T_sp test, T_sp testNot, bool apply_key_to_item) {
    this->_test_pass = true;
    if (testNot.notnilp()) {
      if (test.notnilp()) {
        SIMPLE_ERROR("both test and test-not were defined");
      }
      this->_test_func = coerce::functionDesignator(testNot);
      this->_test_pass = false;
    } else if (test.notnilp()) {
      this->_test_func = coerce::functionDesignator(test);
    } else {
      this->_test_func = cl::_sym_eql->symbolFunction();
    }
    this->_use_key_func = false;
    if (key.notnilp()) {
      this->_use_key_func = true;
      this->_key_func = coerce::functionDesignator(key);
    }
    this->_item = item;
    if (apply_key_to_item && this->_use_key_func) {
      this->_item = eval::funcall(this->_key_func, item);
    }
  }
  bool test(T_sp obj) {
    if (this->_use_key_func) {
      obj = eval::funcall(this->_key_func, obj);
    }
    bool result = T_sp(eval::funcall(this->_test_func, this->_item, obj)).isTrue();
    return ((result == this->_test_pass));
  }
};

List_sp Cons_O::memberEq(T_sp item) const {
  for (auto cur : (List_sp)this->asSmartPtr()) {
    if (CONS_CAR(cur) == item) {
      return cur;
    }
  }
  return nil<T_O>();
}

List_sp Cons_O::memberEql(T_sp item) const {
  for (auto cur : (List_sp)this->asSmartPtr()) {
    if (cl__eql(CONS_CAR(cur), item))
      return cur;
  }
  return nil<T_O>();
}

List_sp Cons_O::member(T_sp item, T_sp key, T_sp test, T_sp testNot) const {
  Tester t(item, key, test, testNot, false);
  for (auto cur : (List_sp)this->asSmartPtr()) {
    LOG("Testing for member with item={} entry = {}", item, oCar(cur));
    T_sp obj = CONS_CAR(cur);
    if (t.test(obj))
      return ((cur));
  }
  return ((nil<T_O>()));
}

/*! Just like member except if there is a key function then apply it to the item
  before you start the test (see ecl:list.d:member1 function) */
List_sp Cons_O::member1(T_sp item, T_sp key, T_sp test, T_sp testNot) const {

  Tester t(item, key, test, testNot, true);
  for (auto cur : (List_sp)this->asSmartPtr()) {
    LOG("Testing for member with item={} entry = {}", item, oCar(cur));
    T_sp obj = CONS_CAR(cur);
    if (t.test(obj))
      return ((cur));
  }
  return ((nil<T_O>()));
}

List_sp Cons_O::assoc(T_sp item, T_sp key, T_sp test, T_sp testNot) const {

  Tester t(item, key, test, testNot, false);
  for (auto cur : (List_sp)this->asSmartPtr()) {
    LOG("Testing for assoc with item={} entry = {}", item, oCar(cur));
    T_sp obj = CONS_CAR(cur);
    if (!obj.nilp()) {
      if (obj.consp()) {
        T_sp obj1 = CONS_CAR(obj);
        if (t.test(obj1))
          return (coerce_to_list(obj));
      } else {
        // Real error is that (car cur) is not a list, not that cur is not a list
        TYPE_ERROR(obj, cl::_sym_list);
      }
    }
  }
  return coerce_to_list(nil<T_O>());
}

List_sp Cons_O::subseq(cl_index start, T_sp end) const {
  size_t_pair bounds = sequenceStartEnd(cl::_sym_subseq, this->length(), start, end);
  ql::list l;
  List_sp cur = this->onthcdr(start);
  for (size_t start(bounds.start); start < bounds.end; ++start) {
    l << oCar(cur);
    cur = oCdr(cur);
  }
  return l.cons();
}

//
// Constructor
//
SYMBOL_EXPORT_SC_(ClPkg, getf);
#if 1
T_sp Cons_O::getf(T_sp key, T_sp defVal) const {
  T_sp cur = this->asSmartPtr();
  while (cur.consp()) {
    T_sp ccur = CONS_CDR(cur);
    unlikely_if(!ccur.consp()) TYPE_ERROR_PROPER_LIST(ccur);
    if (key == CONS_CAR(cur))
      return CONS_CAR(ccur);
    cur = CONS_CDR(ccur);
  }
  unlikely_if(cur.notnilp()) TYPE_ERROR_PROPER_LIST(cur);
  return defVal;
}
#else

T_sp Cons_O::getf(T_sp key, T_sp defVal) const {
  T_sp cur = this->asSmartPtr();
  while (cur.notnilp()) {
    if (!cur.consp() || !oCdr(cur).consp())
      TYPE_ERROR_PROPER_LIST(cur);
    if (key == oCar(cur)) {
      return oCadr(cur);
    }
    cur = oCddr(cur);
  }
  return defVal;
}
#endif

bool Cons_O::equal(T_sp obj) const {
  if (!obj.consp())
    return false;
  if (this == obj.unsafe_cons())
    return true;
  List_sp other = obj;
  if (!cl__equal(this->car(), CONS_CAR(other)))
    return false;
  T_sp this_cdr = this->cdr();
  T_sp other_cdr = cons_cdr(other);
  return cl__equal(this_cdr, other_cdr);
}

bool Cons_O::equalp(T_sp obj) const {
  if (!obj.consp())
    return false;
  if (this == obj.unsafe_cons())
    return true;
  List_sp other = obj;
  if (!cl__equalp(this->car(), oCar(other)))
    return false;
  T_sp this_cdr = this->cdr();
  T_sp other_cdr = oCdr(other);
  return cl__equalp(this_cdr, other_cdr);
}

List_sp Cons_O::reverse() {
  List_sp reversed = nil<T_O>();
  List_sp cur = this->asSmartPtr();
  while (cur.notnilp()) {
    reversed = Cons_O::create(oCar(cur), reversed);
    cur = oCdr(cur);
  }
  return ((reversed));
}

CL_UNWIND_COOP(true);
DOCGROUP(clasp);
CL_DEFUN List_sp core__list_reverse(List_sp list) {
  if (list.nilp())
    return list;
  else
    return list.unsafe_cons()->reverse();
}

List_sp Cons_O::nreverse() {

  List_sp reversed = nil<T_O>();
  List_sp cur = this->asSmartPtr();
  List_sp hold = nil<T_O>();
  while (cur.consp()) {
    hold = oCdr(cur);
    cur.asCons()->setCdr(reversed);
    reversed = cur;
    cur = hold;
  }
  return ((reversed));
}

CL_UNWIND_COOP(true);
DOCGROUP(clasp);
CL_DEFUN List_sp core__list_nreverse(List_sp list) {
  if (list.nilp())
    return list;
  else
    return list.unsafe_cons()->nreverse();
}

List_sp Cons_O::revappend(T_sp tail) {
  List_sp reversed = nil<T_O>();
  T_sp cur = this->asSmartPtr();
  T_sp initial = cur;
  List_sp first_reversed;
  while (cur.consp()) {
    reversed = Cons_O::create(CONS_CAR(cur), reversed);
    if (!first_reversed)
      first_reversed = reversed;
    cur = CONS_CDR(cur);
  }
  if (cur.notnilp())
    TYPE_ERROR_PROPER_LIST(cur);
  first_reversed.asCons()->setCdr(tail);
  return reversed;
}

List_sp Cons_O::nreconc(T_sp tail) {
  List_sp reversed = nil<T_O>();
  Cons_sp original_first = this->asSmartPtr();
  T_sp cur = original_first;
  T_sp hold = nil<T_O>();
  while (cur.consp()) {
    hold = CONS_CDR(cur);
    gc::As_unsafe<Cons_sp>(cur)->setCdr(reversed);
    reversed = cur;
    cur = hold;
  }
  if (cur.notnilp())
    TYPE_ERROR_PROPER_LIST(cur);
  original_first->setCdr(tail);
  return reversed;
}

T_sp Cons_O::setf_nth(cl_index index, T_sp val) {
  if (index >= (int)this->length()) {
    SIMPLE_ERROR("Index[{}] is beyond the length[{}] of the cons", index, this->length());
  }
  List_sp cur = this->asSmartPtr();
  for (cl_index i = 0; i < index; i++)
    cur = oCdr(cur);
  cur.asCons()->setCar(val);
  return ((val));
}

T_sp Cons_O::elt(cl_index index) const {

  size_t max = this->length();
  unlikely_if(index < 0 || index >= max) {
    ERROR(core::_sym_sequence_out_of_bounds,
          core::lisp_createList(
              kw::_sym_expected_type,
              core::lisp_createList(cl::_sym_integer, clasp_make_fixnum(0), core::lisp_createList(clasp_make_fixnum(max))),
              kw::_sym_datum, clasp_make_fixnum(index), kw::_sym_object, this->asSmartPtr()));
  }
  return ((this->onth(index)));
}

T_sp Cons_O::setf_elt(cl_index index, T_sp value) {

  size_t max = this->length();
  unlikely_if(index < 0 || index >= this->length()) {
    ERROR(core::_sym_sequence_out_of_bounds,
          core::lisp_createList(
              kw::_sym_expected_type,
              core::lisp_createList(cl::_sym_integer, clasp_make_fixnum(0), core::lisp_createList(clasp_make_fixnum(max))),
              kw::_sym_datum, clasp_make_fixnum(index), kw::_sym_object, this->asSmartPtr()));
  }
  return ((this->setf_nth(index, value)));
}

T_sp Cons_O::onth(cl_index idx) const {
  T_sp cur = this->asSmartPtr();
  for (cl_index i = 0; i < idx; i++) {
    LIKELY_if(cur.consp()) { cur = cons_cdr(cur); }
    else if (cur.nilp()) {
      return cur;
    }
    else {
      TYPE_ERROR(cur, cl::_sym_Cons_O);
    }
  }
  LIKELY_if(cur.consp()) { return CONS_CAR(cur); }
  else if (cur.nilp()) {
    return cur;
  }
  TYPE_ERROR(cur, cl::_sym_Cons_O);
}

T_sp Cons_O::onthcdr(cl_index idx) const {
  T_sp cur = this->asSmartPtr();
  for (cl_index i = 0; i < idx; i++) {
    LIKELY_if(cur.consp()) { cur = cur.unsafe_cons()->cdr(); }
    else if (cur.nilp()) {
      return cur;
    }
    else {
      TYPE_ERROR(cur, cl::_sym_Cons_O);
    }
  }
  return cur;
}

/*! This algorithm works by first stepping through (n) CONS elements with (r)
and then stepping to the end with (l) and (r).  Once (r) hits the end
(l) will point to the (n)th from the end CONS cell */
T_sp Cons_O::last(cl_index n) const {
  ASSERT(n >= 0);
  List_sp l = this->asSmartPtr();
  T_sp r = l;
  for (r = l; n && (r).consp(); --n, r = oCdr(r))
    ;
  if (r == l) {
    if (!cl__listp(r)) {
      SIMPLE_ERROR("Type not list");
    }
    while ((r).consp()) {
      r = oCdr(r);
    }
    return ((r));
  } else if (n == 0) {
    while ((r).consp()) {
      r = oCdr(r);
      l = oCdr(l);
    }
    return ((l));
  }
  return ((l));
}

List_sp Cons_O::copyList() const {
  List_sp first, cur;
  List_sp p = this->asSmartPtr();
  T_sp cdr;
  first = Cons_O::create(oCar(p), nil<T_O>());
  cur = first;
  cdr = oCdr(p);
  while (cdr.consp()) {
    p = cdr;
    List_sp newCar = Cons_O::create(oCar(p), nil<T_O>());
    cur.asCons()->setCdr(newCar);
    cur = newCar;
    cdr = oCdr(p);
  }
  cur.asCons()->setCdr(cdr);
  return first;
};

List_sp Cons_O::copyTree() const {

  List_sp first, cur;
  first = Cons_O::create(nil<T_O>(), nil<T_O>());
  cur = first;
  List_sp p = this->asSmartPtr();
  T_sp op;
  while (p.notnilp()) {
    List_sp carCopy = p.nilp() ? p : p.asCons()->copyTreeCar();
    cur.asCons()->setCdr(carCopy);
    cur = carCopy;
    op = oCdr(p);
    if (op.consp()) {
      p = gc::As<Cons_sp>(op);
    } else {
      cur.asCons()->setCdr(op);
      break;
    }
  }
  return ((oCdr(first)));
}

List_sp Cons_O::copyTreeCar() const {

  T_sp obj = this->car();
  ASSERTNOTNULL(obj);
  Cons_sp rootCopy = Cons_O::create(nil<T_O>(), nil<T_O>());
  List_sp cobj;
  if (Cons_sp ccobj = obj.asOrNull<Cons_O>()) {
    List_sp cobj = ccobj;
    List_sp carTree = cobj.nilp() ? cobj : cobj.asCons()->copyTree();
    rootCopy->setCar(carTree);
  } else {
    rootCopy->setCar(obj);
  }
  return ((rootCopy));
}

CL_UNWIND_COOP(true);
DOCGROUP(clasp);
CL_DEFUN size_t core__cons_length(Cons_sp cons) {
  size_t sz = 1;
  T_sp cur;
  for (cur = oCdr(cons); cur.consp(); cur = gc::As_unsafe<Cons_sp>(cur)->cdr())
    ++sz;
  if (cur.notnilp()) {
    TYPE_ERROR_PROPER_LIST(cur->asSmartPtr());
  }
  return sz;
};

// FIXME: Redundant
size_t Cons_O::length() const {
  size_t sz = 1;
  T_sp cur;
  for (cur = this->cdr(); cur.consp(); cur = gc::As_unsafe<Cons_sp>(cur)->cdr())
    ++sz;
  if (cur.notnilp()) {
    TYPE_ERROR_PROPER_LIST(cur);
  }
  return sz;
};

void Cons_O::describe(T_sp stream) { clasp_write_string(this->__repr__(), stream); }

string Cons_O::__repr__() const {
  Cons_sp start = this->asSmartPtr();
  T_sp car = start->car();
  T_sp cdr = start->cdr();
  stringstream sout;
  sout << "(" << _safe_rep_(car);
  while (cdr.consp()) {
    Cons_sp p = gc::As<Cons_sp>(cdr);
    car = p->car();
    sout << " " << _safe_rep_(car);
    cdr = oCdr(p);
  }
  if (cdr.notnilp()) {
    sout << " . " << _safe_rep_(cdr) << ")";
  } else {
    sout << ")";
  }
  return ((sout.str()));
}

void not_alist_error(T_sp obj) { SIMPLE_ERROR("Not an alist -> {}", _rep_(obj)); }

CL_UNWIND_COOP(true);
DOCGROUP(clasp);
CL_DEFUN List_sp core__alist_assoc_eq(List_sp alist, T_sp key) {
  if (alist.consp()) {
    for (auto cur : alist) {
      T_sp pair = CONS_CAR(cur);
      if (pair.consp()) {
        if (CONS_CAR(pair) == key)
          return pair;
      } else {
        not_alist_error(alist);
      }
    }
  }
  return nil<T_O>();
}

CL_UNWIND_COOP(true);
DOCGROUP(clasp);
CL_DEFUN List_sp core__alist_assoc_eql(List_sp alist, T_sp key) {
  if (alist.consp()) {
    for (auto cur : alist) {
      T_sp pair = CONS_CAR(cur);
      if (pair.consp()) {
        if (cl__eql(CONS_CAR(pair), key))
          return pair;
      } else {
        not_alist_error(alist);
      }
    }
  }
  return nil<T_O>();
}

CL_UNWIND_COOP(true);
DOCGROUP(clasp);
CL_DEFUN List_sp core__alist_assoc_equal(List_sp alist, T_sp key) {
  if (alist.consp()) {
    for (auto cur : alist) {
      T_sp pair = CONS_CAR(cur);
      if (pair.consp()) {
        if (cl__equal(CONS_CAR(pair), key))
          return pair;
      } else {
        not_alist_error(alist);
      }
    }
  }
  return nil<T_O>();
}

SYMBOL_EXPORT_SC_(CorePkg, cons_length);
SYMBOL_EXPORT_SC_(ClPkg, make_list);
SYMBOL_EXPORT_SC_(ClPkg, cons);
SYMBOL_EXPORT_SC_(ClPkg, getf);
SYMBOL_EXPORT_SC_(CorePkg, rem_f);
SYMBOL_SC_(CorePkg, put_f);

CL_UNWIND_COOP(true);
DOCGROUP(clasp);
CL_DEFUN void core__verify_cons_layout(size_t cons_size, size_t cons_car_offset, size_t cons_cdr_offset) {
  if (cons_size != sizeof(Cons_O))
    SIMPLE_ERROR("The cmpintrinsics.lisp cons_size {} does not match sizeof(Cons_O) {}", cons_size, sizeof(Cons_O));
  if (cons_car_offset != offsetof(Cons_O, _Car))
    SIMPLE_ERROR("cons_rack_offset {} does not match offsetof(_Car,Cons_O) {}", cons_car_offset, offsetof(Cons_O, _Car));
  if (cons_cdr_offset != offsetof(Cons_O, _Cdr))
    SIMPLE_ERROR("cons_rack_offset {} does not match offsetof(_Cdr,Cons_O) {}", cons_cdr_offset, offsetof(Cons_O, _Cdr));
}

}; // namespace core
