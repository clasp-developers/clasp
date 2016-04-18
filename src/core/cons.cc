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
#define DEBUG_LEVEL_FULL

#include <clasp/core/foundation.h>
#include <clasp/core/object.h>
#include <clasp/core/lisp.h>
#include <clasp/core/corePackage.h>
#include <clasp/core/ql.h>
#include <clasp/core/sourceFileInfo.h>
#include <clasp/core/cons.h>
#include <clasp/core/predicates.h>
#include <clasp/core/symbolTable.h>
#include <clasp/core/serialize.h>
#include <clasp/core/evaluator.h>
#include <clasp/core/environment.h>
#include <clasp/core/designators.h>
#include <clasp/core/numbers.h>
#include <clasp/core/str.h>
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
CL_DOCSTRING("putF");
CL_DEFUN List_sp core__put_f(List_sp place, T_sp value, T_sp indicator) {
  auto it = place.begin();
  auto end = place.end();
  List_sp cur;
  while (it.consp()) {
    cur = *it; // cur is guaranteed to be a Cons_sp
    ++it;
    if (!it.consp())
      break;
    if (oCar(cur) == indicator) {
      (*it)->rplaca(value); // *it is guaranteed to be a Cons_sp
      return place;
    }
    ++it;
  }
  if (it != end)
    SIMPLE_ERROR(BF("type_error_plist %s") % _rep_(place));
  place = Cons_O::create(value, place);
  place = Cons_O::create(indicator, place);
  return place;
};

CL_LAMBDA(plist indicator &optional default-value);
CL_DECLARE();
CL_DOCSTRING("getf");
CL_DEFUN T_sp cl__getf(List_sp plist, T_sp indicator, T_sp default_value) {
  if (plist.nilp())
    return (default_value);
  return plist.asCons()->getf(indicator, default_value);
};

CL_LAMBDA(plist indicator);
CL_DECLARE();
CL_DOCSTRING("Removes the property with the indicator from the property list in place if present and returns MultipleValues with the new property list and T if the property was found");
CL_DEFUN T_mv core__rem_f(List_sp plist, Symbol_sp indicator) {
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
CL_DOCSTRING("cons");
CL_DEFUN Cons_sp cl__cons(T_sp obj1, T_sp obj2) {
  ASSERTNOTNULL(obj1);
  ASSERTNOTNULL(obj2);
  return Cons_O::create(obj1, obj2);
};

CL_LAMBDA(c o);
CL_DECLARE();
CL_DOCSTRING("");
CL_DEFUN Cons_sp cl__rplaca(Cons_sp c, T_sp o) {
  return c->rplaca(o);
};

CL_LAMBDA(c o);
CL_DECLARE();
CL_DOCSTRING("");
CL_DEFUN Cons_sp cl__rplacd(Cons_sp c, T_sp o) {
  return c->rplacd(o);
};

CL_LAMBDA(osize &key initial_element);
CL_DECLARE();
CL_DOCSTRING("make_list");
CL_DEFUN List_sp cl__make_list(Integer_sp osize, T_sp initial_element) {
  int size = clasp_to_int(osize);
  if (size < 0) {
    SIMPLE_ERROR(BF("Illegal size %d for list") % size);
  }
  ql::list result(_lisp);
  for (int i = 0; i < size; i++) {
    result << initial_element;
  }
  return (result.cons());
};

Cons_sp Cons_O::createList(T_sp o1) {
  return (Cons_O::create(o1, _Nil<T_O>()));
}

Cons_sp Cons_O::createList(T_sp o1, T_sp o2) {
  return (Cons_O::create(o1, Cons_O::create(o2)));
}

Cons_sp Cons_O::createList(T_sp o1, T_sp o2, T_sp o3) {
  return ((Cons_O::create(o1, Cons_O::createList(o2, o3))));
}

Cons_sp Cons_O::createList(T_sp o1, T_sp o2, T_sp o3, T_sp o4) {
  return ((Cons_O::create(o1, Cons_O::createList(o2, o3, o4))));
}

Cons_sp Cons_O::createList(T_sp o1, T_sp o2, T_sp o3, T_sp o4, T_sp o5) {
  return ((Cons_O::create(o1, Cons_O::createList(o2, o3, o4, o5))));
}

Cons_sp Cons_O::createList(T_sp o1, T_sp o2, T_sp o3, T_sp o4, T_sp o5, T_sp o6) {
  return ((Cons_O::create(o1, Cons_O::createList(o2, o3, o4, o5, o6))));
}

Cons_sp Cons_O::createList(T_sp o1, T_sp o2, T_sp o3, T_sp o4, T_sp o5, T_sp o6, T_sp o7) {
  return ((Cons_O::create(o1, Cons_O::createList(o2, o3, o4, o5, o6, o7))));
}
Cons_sp Cons_O::createList(T_sp o1, T_sp o2, T_sp o3, T_sp o4, T_sp o5, T_sp o6, T_sp o7, T_sp o8) {
  return ((Cons_O::create(o1, Cons_O::createList(o2, o3, o4, o5, o6, o7, o8))));
}

#if 0
    Cons_sp	Cons_O::createFromCommandLineArguments(int argc, char* argv[] )
    {
	Cons_sp args = _Nil<T_O>();
	Cons_sp curArg = _Nil<T_O>();
	for ( int i=0;i!=argc; i++ )
	{
	    Cons_sp carg = Cons_O::create(Str_O::create(argv[i]),_Nil<T_O>());
	    if ( curArg.nilp() )
	    {
		args = carg;
		curArg = carg;
	    } else
	    {
		curArg->setCdr(carg);
		curArg = carg;
	    }
	}
	return((args));
    }
#endif

bool isAllDigits(const string &s) {
  for (uint i = 0; i < s.size(); i++) {
    if (!isdigit(s[i]))
      return ((false));
  }
  return ((true));
}

T_sp stringToObject(Lisp_sp e, const string &s) {
  if (isAllDigits(s)) {
    return ((make_fixnum(atoi(s.c_str()))));
  }
  if (s == "false" || s == "False")
    return ((_Nil<T_O>()));
  return (Str_O::create(s));
}

#if 1
/*! Copied from ecl append_into
     This copies the CAR's in l into new CONS nodes
    that are appended to the list pointed to by **tailPP
    which is advanced with every element.
    **tailPP is at the end of the list pointed to by head
    */
void Cons_O::appendInto(T_sp head, T_sp *&tailP, T_sp l) {
  if (!(*tailP).nilp()) {
    /* (APPEND '(1 . 2) 3) */
    TYPE_ERROR_PROPER_LIST(head);
  }
  while (cl__consp(l)) {
    Cons_sp cons = Cons_O::create(cons_car(l));
    *tailP = cons;
    tailP = &(cons->_Cdr);
    l = cons_cdr(l);
  }
  *tailP = l;
}

#else
/*! Copied from ecl append_into */
void Cons_O::appendInto(T_sp head, gctools::StackRootedPointerToSmartPtr<T_O> &tail, T_sp l) {
  if (!(tail.getPointee()).nilp()) {
    /* (APPEND '(1 . 2) 3) */
    TYPE_ERROR_PROPER_LIST(head);
  }
  while (cl__consp(l)) {
    Cons_sp cons = Cons_O::create(cons_car(l));
    tail.setPointee(cons);
    tail.setPointer(&(cons->_Cdr)); // = &cons->_Cdr;
    l = cons_cdr(l);
  }
  tail.setPointee(l);
}
#endif

CL_LAMBDA(l1 l2);
CL_DECLARE();
CL_DOCSTRING("append2 - append l2 to l1 by copying l1 and pointing the end of it to l2");
CL_DEFUN T_sp core__append2(List_sp x, List_sp y) {
  return Cons_O::append(x, y);
};

T_sp Cons_O::append(List_sp x, List_sp y) {
  T_sp head(_Nil<T_O>()); // This will root the new list
  T_sp *tailP = &head;    // This will keep track of the end of the new list
  if (x.notnilp()) {
    Cons_O::appendInto(head, tailP, x);
  }
  if ((*tailP).notnilp()) {
    TYPE_ERROR_PROPER_LIST(head);
  }
  /* I WAS DOING THIS WHY??? head = y; */
  *tailP = y;
  return head;
}

#if 0
    T_sp Cons_O::append(T_sp x, T_sp y)
    {
	T_sp head(_Nil<List_O>());
	T_sp* tail_gc_safe = &head;
	if ( x.notnilp() ) {
	    tail_gc_safe = Cons_O::appendInto(head,tail_gc_safe,x);
	}
	if ( (*tail_gc_safe).notnilp() ) {
	    TYPE_ERROR_PROPER_LIST(head);
	}
	*tail_gc_safe = y;
	return head.as<List_O>();
    }
#endif


struct Tester {
  T_sp _item;
  Function_sp _test_func;
  bool _test_pass;
  bool _use_key_func;
  Function_sp _key_func;
  Tester(T_sp item, T_sp key, T_sp test, T_sp testNot, bool applyKey) {
    this->setup(item, key, test, testNot, applyKey);
  }

  void setup(T_sp item, T_sp key, T_sp test, T_sp testNot, bool apply_key_to_item) {
    this->_test_pass = true;
    if (testNot.notnilp()) {
      if (test.notnilp()) {
        SIMPLE_ERROR(BF("both test and test-not were defined"));
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

CL_LISPIFY_NAME("core:exactlyMatches");
CL_DEFMETHOD bool Cons_O::exactlyMatches(List_sp other) const {
  List_sp me = this->asSmartPtr();
  while (me.notnilp()) {
    if (oCar(other) != oCar(me))
      return false;
    other = oCdr(other);
    me = oCdr(me);
  }
  return true;
}

List_sp Cons_O::memberEq(T_sp item) const {
  for (auto cur : (List_sp) this->asSmartPtr()) {
    if (oCar(cur) == item)
      return cur;
  }
  return _Nil<T_O>();
}

List_sp Cons_O::memberEql(T_sp item) const {
  for (auto cur : (List_sp) this->asSmartPtr()) {
    if (cl__eql(oCar(cur), item))
      return cur;
  }
  return _Nil<T_O>();
}

List_sp Cons_O::member(T_sp item, T_sp key, T_sp test, T_sp testNot) const {
  _OF();
  Tester t(item, key, test, testNot, false);
  for (auto cur : (List_sp) this->asSmartPtr()) {
    LOG(BF("Testing for member with item=%s entry = %s") % item % oCar(cur));
    T_sp obj = oCar(cur);
    if (t.test(obj))
      return ((cur));
  }
  return ((_Nil<T_O>()));
}

/*! Just like member except if there is a key function then apply it to the item
  before you start the test (see ecl:list.d:member1 function) */
List_sp Cons_O::member1(T_sp item, T_sp key, T_sp test, T_sp testNot) const {
  _OF();
  Tester t(item, key, test, testNot, true);
  for (auto cur : (List_sp) this->asSmartPtr()) {
    LOG(BF("Testing for member with item=%s entry = %s") % item % oCar(cur));
    T_sp obj = oCar(cur);
    if (t.test(obj))
      return ((cur));
  }
  return ((_Nil<T_O>()));
}

List_sp Cons_O::assoc(T_sp item, T_sp key, T_sp test, T_sp testNot) const {
  _OF();
  Tester t(item, key, test, testNot, false);
  for (auto cur : (List_sp) this->asSmartPtr()) {
    LOG(BF("Testing for assoc with item=%s entry = %s") % item % oCar(cur));
    if (oCar(cur).consp()) {
      T_sp obj = oCar(oCar(cur));
      if (t.test(obj))
        return (coerce_to_list(oCar(cur)));
    }
  }
  return coerce_to_list(_Nil<T_O>());
}

List_sp Cons_O::subseq(int start, T_sp end) const {
  ql::list l(_lisp);
  int iend;
  if (end.nilp()) {
    iend = this->length();
  } else if (core__fixnump(end)) {
    iend = unbox_fixnum(gc::As<Fixnum_sp>(end));
  } else {
    SIMPLE_ERROR(BF("Illegal end for subseq[%s]") % _rep_(end));
  }
  List_sp cur = this->onthcdr(start);
  for (; start < iend; start++) {
    l << oCar(cur);
    cur = oCdr(cur);
  }
  return ((l.cons()));
}

#if 0
/*!
 * Convert command line arguments into a Cons
 * keyed Arguments that start with "---" treat the words that follow them until
 *     another argument is hit as a list and assemble a Cons
 * keyed arguments that start with "--" treat the single word that follow them
 *     as a single argument
 * numbers are converted to integers (no reals right now)
 * everything else is treated as a string
 * eg: --file xxx.yyy ---entries 1 2 3 4 5 -- -to hello
 * 	-> file: "xxx.yyy" entries: (: 1 2 3 4 5) to: "hello"
 */
    Vector_sp	Cons_O::createFromVectorStringsCommandLineArguments(const vector<string>& strings )
    {
	vector<string>::const_iterator it;
	Cons_sp first = Cons_O::create(_Nil<T_O>(),_Nil<T_O>());
	Cons_sp args = first;
	it = strings.begin();
	while ( it != strings.end() )
	{
	    T_sp obj;
	    // Arguments that start with "---" parse their arguments as a list
	    //
	    if ( (*it).substr(0,3)=="---" )
	    {
		LOG(BF( "Hit keyed list argument(%s)") % (*it).c_str() );
		string keyStr = (*it).substr(3,999999);
		obj = lisp->internKeyword(keyStr);
		Cons_sp entry = Cons_O::create(obj,_Nil<T_O>());
		args->setCdr(entry);
		args = entry;
		Cons_sp afirst = Cons_O::create(_Nil<T_O>());
		Cons_sp acur = afirst;
		while (1)
		{
		    LOG( BF("Trying accumulating list argument"));
		    it++;
		    if (it==strings.end() || (*it).substr(0,2) == "--" ) break;
		    T_sp o = stringToObject(lisp,*it);
		    Cons_sp aone = Cons_O::create(o);
		    LOG(BF( "Accumulating object(%s) into list for argument") % _rep_(aone) );
		    acur->setCdr(aone);
		    acur = aone;
		}
		obj = cCdr(afirst);
	    } else if ( (*it).substr(0,2)=="--" )
	    {
		LOG(BF( "Hit keyed single argument(%s)") % (*it) );
		string keyStr = (*it).substr(2,999999);
		obj = lisp->internKeyword(keyStr);
		it++;
	    } else
	    {
		LOG(BF( "Hit regular argument |%s|") % (*it) );
		T_sp o = stringToObject(lisp,*(it));
		obj = o;
		it++;
	    }
	    LOG( BF("Accumulating entry(%s) in main argument list") %_rep_( obj) );
	    Cons_sp entry = Cons_O::create(obj,_Nil<T_O>());
	    args->setCdr(entry);
	    args = entry;
	}
	LOG(BF( "After parse|%s|") % _rep_(oCdr(first)));
	return((cCdr(first)));
    }
#endif

//
// Constructor
//
Cons_O::Cons_O() : _Car(_Nil<T_O>()), _Cdr(_Nil<T_O>()) // , _CdrLength(0)
{
  ASSERTNOTNULL(this->_Car);
  ASSERTNOTNULL(this->_Cdr);
}

/*! Write out all of the elements of this list as a list to
 * avoid excessive nesting
 */
#if 0
void Cons_O::archiveBase(ArchiveP node) {
#if 1
  if (node->saving()) {
    core__stack_monitor(); // make sure the stack isn't exhausted.
    // Convert the the list that this Cons points to into a vector of elements where
    // the last element is the very last CDR
    T_sp cur = this->asSmartPtr();
    // TODO: Fix this loop - it will go into an infinite loop
    for (; cur.notnilp(); cur = oCdr(cur)) {
      if (cl__consp(cur)) {
        T_sp obj = oCar(cur);
        node->pushVector(obj); // A Cons - push the car
      } else {
        node->pushVector(cur); // improper list - last element not nil
        return;
      }
    }
    node->pushVector(_Nil<T_O>()); // proper list - last element nil
  } else {                         // loading
    Vector_sp vec = node->getVectorSNodes();
    int len = vec->length();
    Cons_sp cur = Cons_O::create(gc::As<SNode_sp>((*vec)[len - 2])->object(), gc::As<SNode_sp>((*vec)[len - 1])->object());
    for (int i(len - 3); i >= 0; --i) {
      Cons_sp one = Cons_O::create(gc::As<SNode_sp>((*vec)[i])->object(), cur);
      cur = one;
    }
    this->_Car = cur->_Car;
    this->_Cdr = cur->_Cdr;
  }
#else
  node->attributeIfNotNil("A", this->_Car); // use attributeIfNotNil
  node->attributeIfNotNil("D", this->_Cdr); // use attributeIfNotNil
#endif
}
#endif


SYMBOL_EXPORT_SC_(ClPkg, getf);
T_sp Cons_O::getf(T_sp key, T_sp defVal) const {
  _OF();
  for (List_sp cur = this->asSmartPtr(); cur.notnilp(); cur = oCddr(cur)) {
    if (key == oCar(cur)) {
      return ((oCadr(cur)));
    }
  }
  return ((defVal));
}

#if defined(OLD_SERIALIZE)
void Cons_O::serialize(serialize::SNode node) {
  _OF();
  node->attribute("cdrl", this->_CdrLength);
  node->attributeIfNotNil("car", this->_Car);
  node->attributeIfNotNil("cdr", this->_Cdr);
}
#endif

bool Cons_O::equal(T_sp obj) const {
  if (!obj.consp()) return false;
  if (this == obj.unsafe_cons()) return true;
  List_sp other = obj;
  if (!cl__equal(this->_Car, oCar(other)))
    return false;
  T_sp this_cdr = this->_Cdr;
  T_sp other_cdr = oCdr(other);
  return cl__equal(this_cdr, other_cdr);
}

bool Cons_O::equalp(T_sp obj) const {
  if (!obj.consp()) return false;
  if (this == obj.unsafe_cons()) return true;
  List_sp other = obj;
  if (!cl__equalp(this->_Car, oCar(other)))
    return false;
  T_sp this_cdr = this->_Cdr;
  T_sp other_cdr = oCdr(other);
  return cl__equalp(this_cdr, other_cdr);
}

CL_LISPIFY_NAME("core:extend");
CL_DEFMETHOD List_sp Cons_O::extend(List_sp rest) {
  Cons_sp first = Cons_O::create(_Nil<T_O>(), _Nil<T_O>());
  Cons_sp nc = first;
  Cons_sp next, newCur;
  List_sp cur = this->asSmartPtr();
  while (cur.notnilp()) {
    newCur = Cons_O::create(oCar(cur), _Nil<T_O>());
    nc->setCdr(newCur);
    nc = newCur;
    cur = oCdr(cur);
  }
  // Now attach the rest
  cur = rest;
  while (cur.consp()) {
    newCur = Cons_O::create(oCar(cur), _Nil<T_O>());
    nc->setCdr(newCur);
    nc = newCur;
    cur = oCdr(cur);
  }
  return ((oCdr(first)));
}

List_sp Cons_O::reverse() {
  List_sp reversed = _Nil<T_O>();
  List_sp cur = this->asSmartPtr();
  while (cur.notnilp()) {
    reversed = Cons_O::create(oCar(cur), reversed);
    cur = oCdr(cur);
  }
  return ((reversed));
}

List_sp Cons_O::revappend(T_sp tail) {
  List_sp reversed = _Nil<T_O>();
  List_sp cur = this->asSmartPtr();
  List_sp first_reversed;
  while (cur.notnilp()) {
    reversed = Cons_O::create(oCar(cur), reversed);
    if (!first_reversed) {
      first_reversed = reversed;
    }
    cur = oCdr(cur);
  }
  first_reversed.asCons()->setCdr(tail);
  return ((reversed));
}

List_sp Cons_O::nreverse() {
  _OF();
  List_sp reversed = _Nil<T_O>();
  List_sp cur = this->asSmartPtr();
  List_sp hold = _Nil<T_O>();
  while (cur.consp()) {
    hold = oCdr(cur);
    cur.asCons()->setCdr(reversed);
    reversed = cur;
    cur = hold;
  }
  return ((reversed));
}

List_sp Cons_O::nreconc(T_sp tail) {
  _OF();
  List_sp reversed = _Nil<T_O>();
  Cons_sp original_first = this->asSmartPtr();
  List_sp cur = original_first;
  List_sp hold = _Nil<T_O>();
  while (cur.notnilp()) {
    hold = oCdr(cur);
    cur.asCons()->setCdr(reversed);
    reversed = cur;
    cur = hold;
  }
  original_first->setCdr(tail);
  return ((reversed));
}

T_sp Cons_O::setf_nth(int index, T_sp val) {
  _OF();
  if (index >= (int)this->length()) {
    SIMPLE_ERROR(BF("Index[%d] is beyond the length[%d] of the cons") % index % this->length());
  }
  List_sp cur = this->asSmartPtr();
  for (int i = 0; i < index; i++)
    cur = oCdr(cur);
  cur.asCons()->setCar(val);
  return ((val));
}

T_sp Cons_O::elt(int index) const {
  _OF();
  if (index < 0 || index >= this->length()) {
    SIMPLE_ERROR(BF("Illegal index %d for Cons containing %d elements") % index % this->length());
  }
  return ((this->onth(index)));
}

T_sp Cons_O::setf_elt(int index, T_sp value) {
  _OF();
  if (index < 0 || index >= this->length()) {
    SIMPLE_ERROR(BF("Illegal index %d for Cons containing %d elements") % index % this->length());
  }
  return ((this->setf_nth(index, value)));
}

CL_LISPIFY_NAME("core:filterOutNil");
CL_DEFMETHOD List_sp Cons_O::filterOutNil() {
  Cons_sp first = Cons_O::create(_Nil<T_O>(), _Nil<T_O>());
  List_sp newCur = first;
  for (auto cur : (List_sp) this->asSmartPtr()) {
    if (oCar(cur).notnilp()) {
      Cons_sp one = Cons_O::create(oCar(cur), _Nil<T_O>());
      newCur.asCons()->setCdr(one);
      newCur = oCdr(newCur);
    }
  }
  return ((oCdr(first)));
}

#if 0
    void	Cons_O::setOwnerOfAllEntries(T_sp obj)
    {
	Cons_sp cur;
	for ( cur=this->asSmartPtr(); cur.notnilp(); cur = cCdr(cur) )
	{
	    cur->ocar()->setOwner(obj);
	}
    }
#endif

T_sp Cons_O::onth(int idx) const {
  _OF();
  List_sp cur = this->asSmartPtr();
  for (int i = 0; i < idx; i++) {
    cur = oCdr(cur);
  }
  return ((oCar(cur)));
}

List_sp Cons_O::onthcdr(int idx) const {
  List_sp cur = this->asSmartPtr();
  for (int i = 0; i < idx; i++) {
    cur = oCdr(cur);
  }
  return ((cur));
}

/*! This algorithm works by first stepping through (n) CONS elements with (r)
and then stepping to the end with (l) and (r).  Once (r) hits the end
(l) will point to the (n)th from the end CONS cell */
List_sp Cons_O::last(int n) const {
  ASSERT(n >= 0);
  List_sp l = this->asSmartPtr();
  T_sp r = l;
  for (r = l; n && cl__consp(r); --n, r = oCdr(r))
    ;
  if (r == l) {
    if (!cl__listp(r)) {
      SIMPLE_ERROR(BF("Type not list"));
    }
    while (cl__consp(r)) {
      r = oCdr(r);
    }
    return ((r));
  } else if (n == 0) {
    while (cl__consp(r)) {
      r = oCdr(r);
      l = oCdr(l);
    }
    return ((l));
  }
  return ((l));
}

List_sp Cons_O::copyList() const {
  _OF();
  List_sp first, cur;
  List_sp p = this->asSmartPtr();
  T_sp cdr;
  first = Cons_O::create(oCar(p), _Nil<T_O>());
  cur = first;
  cdr = oCdr(p);
  while (cdr.consp()) {
    p = cdr;
    List_sp newCar = Cons_O::create(oCar(p), _Nil<T_O>());
    cur.asCons()->setCdr(newCar);
    cur = newCar;
    cdr = oCdr(p);
  }
  cur.asCons()->setCdr(cdr);
  return first;
};

Cons_sp Cons_O::copyListCar() const {
  _OF();
  T_sp obj = this->_Car;
  ASSERTNOTNULL(obj);
  Cons_sp rootCopy = Cons_O::create(_Nil<T_O>(), _Nil<T_O>());
  rootCopy->setCar(obj);
  return ((rootCopy));
}

List_sp Cons_O::copyTree() const {
  _OF();
  List_sp first, cur;
  first = Cons_O::create(_Nil<T_O>(), _Nil<T_O>());
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
  _OF();
  T_sp obj = this->_Car;
  ASSERTNOTNULL(obj);
  Cons_sp rootCopy = Cons_O::create(_Nil<T_O>(), _Nil<T_O>());
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

uint Cons_O::length() const {
  int sz = 1;
#pragma GCC diagnostic push
#pragma clang diagnostic ignored "-Wunused-variable"
  for (auto p : coerce_to_list(this->_Cdr))
    ++sz;
#pragma GCC diagnostic pop
  return ((sz));
};

T_sp Cons_O::olistref(int idx) {
  int i = 0;
  List_sp p;
  LOG(BF("Evaluating the length of list: %s") % this->__repr__());
  p = this->asSmartPtr();
  while (p.consp()) {
    if (i == idx)
      break;
    i++;
    LOG(BF("in loop i = %d looking for %d") % i % idx);
    p = oCdr(p);
  };
  if (p.nilp()) {
    stringstream ss;
    ss << "List[" << idx << "] is out of bounds, size=" << i;
    SIMPLE_ERROR(BF("%s") % ss.str());
  }
  return ((oCar(p)));
};

T_sp Cons_O::olistrefArgument(int idx) {
  int i = 0;
  List_sp p;
  p = this->asSmartPtr();
  while (p.notnilp()) {
    if (i == idx)
      break;
    i++;
    LOG(BF("in loop i = %d looking for %d") % i % idx);
    p = oCdr(p);
  };
  if (p.nilp()) {
    stringstream ss;
    ss << "You did not provide enough arguments for a call to a C++ function - it expected at least " << idx << " arguments and you passed only " << i;
    SIMPLE_ERROR(BF("%s") % ss.str());
  }
  return ((oCar(p)));
};

void Cons_O::setCdr(T_sp c) {
  this->_Cdr = c;
}

CL_LISPIFY_NAME("core:lookupDefault");
CL_DEFMETHOD T_sp Cons_O::olookupKeyObjectDefault(Symbol_sp keyword, T_sp dft) {
  _OF();
  ASSERTP(keyword->isKeywordSymbol(), "You can only search for keyword symbols");
  LOG(BF("lookup %s in %s") % _rep_(keyword) % this->__repr__());
  List_sp lp = this->asSmartPtr();
  LOG(BF("Got start of list to search: %s") % _rep_(lp));
  for (auto p : lp) {
    if (cl__symbolp(oCar(p))) {
      Symbol_sp ps = gc::As<Symbol_sp>(oCar(p));
      if (ps->isKeywordSymbol()) {
        if (ps == keyword) {
          return ((oCadr(p)));
        }
      }
    }
  }
  LOG(BF("Returning default"));
  return ((dft));
}

CL_LISPIFY_NAME("core:lookup");
CL_DEFMETHOD T_sp Cons_O::olookupKeyObject(Symbol_sp key) {
  Cons_sp p;
  return ((this->olookupKeyObjectDefault(key, _Nil<T_O>())));
}

void Cons_O::describe(T_sp stream)
{
  clasp_write_string(this->__repr__(),stream);
}

string Cons_O::__repr__() const {
  Cons_sp start = this->asSmartPtr();
  T_sp cdr = start;
  stringstream sout;
  if (oCar(start) == cl::_sym_quote) {
    if (cl__consp(oCdr(start))) {
      sout << "'" << _rep_(oCadr(start)) << " ";
    } else {
      sout << "QUOTE ." << _rep_(oCdr(start)) << " ";
    }
    return ((sout.str()));
  }
  sout << "(";
  while (cdr.notnilp()) {
    if (cl__consp(cdr)) {
      Cons_sp p = gc::As<Cons_sp>(cdr);
      T_sp po = p->_Car;
      sout << _rep_(po) << " ";
      cdr = oCdr(p);
    } else {
      if (cdr.notnilp()) {
        sout << " . ";
        sout << _rep_(cdr);
      }
      cdr = _Nil<T_O>();
    }
  }
  sout << " )";
#if 0 // also checkout Cons_O::
        sout <<"@" << (void*)(this) << " ";
#endif
  return ((sout.str()));
}

CL_DEFUN List_sp core__alist_erase(List_sp alist, T_sp key) {
  if (alist.nilp())
    return alist;
  if (oCar(oCar(alist)) == key)
    return oCdr(alist);
  List_sp prev_alist = alist;
  List_sp cur = oCdr(alist);
  while (alist.notnilp()) {
    if (oCar(oCar(alist)) == key) {
      prev_alist.asCons()->rplacd(oCdr(alist.asCons()));
      return alist;
    }
    prev_alist = cur;
    cur = oCdr(cur);
  }
  return alist;
}

CL_DEFUN List_sp core__alist_push(List_sp alist, T_sp key, T_sp val) {
  Cons_sp one = Cons_O::create(key, val);
  alist = Cons_O::create(one, alist);
  return alist;
}

CL_DEFUN List_sp core__alist_get(List_sp alist, T_sp key) {
  while (alist.notnilp()) {
    if (oCar(oCar(alist)) == key) {
      return alist;
    }
    alist = oCdr(alist);
  }
  return _Nil<T_O>();
}

CL_DEFUN string core__alist_asString(List_sp alist) {
  stringstream ss;
  while (alist.notnilp()) {
    ss << _rep_(oCar(oCar(alist))) << " ";
    alist = oCdr(alist);
  }
  return ss.str();
}


  SYMBOL_EXPORT_SC_(ClPkg, make_list);
  SYMBOL_EXPORT_SC_(ClPkg, cons);
  SYMBOL_EXPORT_SC_(ClPkg, getf);
  SYMBOL_EXPORT_SC_(CorePkg, rem_f);
  SYMBOL_SC_(CorePkg, put_f);

};
