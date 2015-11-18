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
#include <clasp/core/executables.h>
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

#define ARGS_core_put_f "(plist value indicator)"
#define DECL_core_put_f ""
#define DOCS_core_put_f "putF"
List_sp core_put_f(List_sp place, T_sp value, T_sp indicator) {
  _G();
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

#define ARGS_cl_getf "(plist indicator &optional default-value)"
#define DECL_cl_getf ""
#define DOCS_cl_getf "getf"
T_sp cl_getf(List_sp plist, T_sp indicator, T_sp default_value) {
  _G();
  if (plist.nilp())
    return (default_value);
  return plist.asCons()->getf(indicator, default_value);
};

#define ARGS_core_rem_f "(plist indicator)"
#define DECL_core_rem_f ""
#define DOCS_core_rem_f "Removes the property with the indicator from the property list in place if present and returns MultipleValues with the new property list and T if the property was found"
T_mv core_rem_f(List_sp plist, Symbol_sp indicator) {
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

#define DOCS_af_cons "cons"
#define LOCK_af_cons 1
#define ARGS_af_cons "(object1 object2)"
#define DECL_af_cons ""
Cons_sp af_cons(T_sp obj1, T_sp obj2) {
  _G();
  ASSERTNOTNULL(obj1);
  ASSERTNOTNULL(obj2);
  return Cons_O::create(obj1, obj2);
};

#define ARGS_cl_rplaca "(c o)"
#define DECL_cl_rplaca ""
#define DOCS_cl_rplaca ""
Cons_sp cl_rplaca(Cons_sp c, T_sp o) {
  return c->rplaca(o);
};

#define ARGS_cl_rplacd "(c o)"
#define DECL_cl_rplacd ""
#define DOCS_cl_rplacd ""
Cons_sp cl_rplacd(Cons_sp c, T_sp o) {
  return c->rplacd(o);
};

#define DOCS_af_make_list "make_list"
#define LOCK_af_make_list 1
#define ARGS_af_make_list "(osize &key initial_element)"
#define DECL_af_make_list ""
List_sp af_make_list(Integer_sp osize, T_sp initial_element) {
  _G();
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
  _G();
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
  while (cl_consp(l)) {
    Cons_sp cons = Cons_O::create(CONS_CAR(l));
    *tailP = cons;
    tailP = &(cons->_Cdr);
    l = CONS_CDR(l);
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
  while (cl_consp(l)) {
    Cons_sp cons = Cons_O::create(CONS_CAR(l));
    tail.setPointee(cons);
    tail.setPointer(&(cons->_Cdr)); // = &cons->_Cdr;
    l = CONS_CDR(l);
  }
  tail.setPointee(l);
}
#endif

#define ARGS_af_append2 "(l1 l2)"
#define DECL_af_append2 ""
#define DOCS_af_append2 "append2 - append l2 to l1 by copying l1 and pointing the end of it to l2"
T_sp af_append2(List_sp x, List_sp y) {
  _G();
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

List_sp Cons_O::walkToFindParsePos() const {
  _G();
  //	if ( this->hasParsePos() ) return((this->asSmartPtr()));
  if (this->_Cdr.notnilp() && cl_consp(this->_Cdr)) {
    List_sp wcdr = gc::As<Cons_sp>(this->_Cdr)->walkToFindParsePos();
    if (wcdr.notnilp())
      return ((wcdr));
  }
  if (this->_Car.notnilp() && cl_consp(this->_Car)) {
    List_sp wcar = gc::As<Cons_sp>(this->_Car)->walkToFindParsePos();
    if (wcar.notnilp())
      return ((wcar));
  }
  return ((_Nil<T_O>()));
}

void Cons_O::sxhash_(HashGenerator &hg) const {
  _OF();
  if (hg.isFilling())
    hg.hashObject(this->_Car);
  if (hg.isFilling())
    hg.hashObject(this->_Cdr);
}

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
    _G();
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
    _G();
    if (this->_use_key_func) {
      obj = eval::funcall(this->_key_func, obj);
    }
    bool result = T_sp(eval::funcall(this->_test_func, this->_item, obj)).isTrue();
    return ((result == this->_test_pass));
  }
};

bool Cons_O::exactlyMatches(List_sp other) const {
  _G();
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
    if (cl_eql(oCar(cur), item))
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
  _G();
  ql::list l(_lisp);
  int iend;
  if (end.nilp()) {
    iend = this->length();
  } else if (af_fixnumP(end)) {
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
    {_G();
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
Cons_O::Cons_O() : T_O(), _Car(_Nil<T_O>()), _Cdr(_Nil<T_O>()) // , _CdrLength(0)
{
  ASSERTNOTNULL(this->_Car);
  ASSERTNOTNULL(this->_Cdr);
}

/*! Write out all of the elements of this list as a list to
 * avoid excessive nesting
 */
void Cons_O::archiveBase(ArchiveP node) {
  _G();
#if 1
  if (node->saving()) {
    af_stackMonitor(); // make sure the stack isn't exhausted.
    // Convert the the list that this Cons points to into a vector of elements where
    // the last element is the very last CDR
    T_sp cur = this->asSmartPtr();
    // TODO: Fix this loop - it will go into an infinite loop
    for (; cur.notnilp(); cur = oCdr(cur)) {
      if (cl_consp(cur)) {
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
  _OF();
  if (this->eq(obj))
    return ((true));
  if (!obj.consp())
    return false;
  List_sp other = obj;
  if (!cl_equal(this->_Car, oCar(other)))
    return false;
  T_sp this_cdr = this->_Cdr;
  T_sp other_cdr = oCdr(other);
  return cl_equal(this_cdr, other_cdr);
}

bool Cons_O::equalp(T_sp obj) const {
  if (this->eq(obj))
    return true;
  if (!obj.consp())
    return false;
  List_sp other = obj;
  if (!cl_equalp(this->_Car, oCar(other)))
    return false;
  T_sp this_cdr = this->_Cdr;
  T_sp other_cdr = oCdr(other);
  return cl_equalp(this_cdr, other_cdr);
}

List_sp Cons_O::extend(List_sp rest) {
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
  _G();
  List_sp reversed = _Nil<T_O>();
  List_sp cur = this->asSmartPtr();
  while (cur.notnilp()) {
    reversed = Cons_O::create(oCar(cur), reversed);
    cur = oCdr(cur);
  }
  return ((reversed));
}

List_sp Cons_O::revappend(T_sp tail) {
  _G();
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

List_sp Cons_O::filterOutNil() {
  _G();
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
    {_G();
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
  for (r = l; n && cl_consp(r); --n, r = oCdr(r))
    ;
  if (r == l) {
    if (!cl_listp(r)) {
      SIMPLE_ERROR(BF("Type not list"));
    }
    while (cl_consp(r)) {
      r = oCdr(r);
    }
    return ((r));
  } else if (n == 0) {
    while (cl_consp(r)) {
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
  first = Cons_O::create(_Nil<T_O>(), _Nil<T_O>());
  cur = first;
  List_sp p = this->asSmartPtr();
  while (p.consp()) {
    List_sp carNode = p.asCons()->copyListCar();
    cur.asCons()->setCdr(carNode);
    cur = oCdr(cur);
    T_sp cdr = oCdr(p);
    if (cdr.nilp())
      break;
    if (!cdr.consp()) {
      cur.asCons()->setCdr(cdr);
      break;
    }
    p = cdr;
  }
  return ((oCdr(first)));
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
  _G();
  int sz = 1;
#pragma GCC diagnostic push
#pragma clang diagnostic ignored "-Wunused-variable"
  for (auto p : coerce_to_list(this->_Cdr))
    ++sz;
#pragma GCC diagnostic pop
  return ((sz));
};

T_sp Cons_O::olistref(int idx) {
  _G();
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
  _G();
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

#define WARN_CONS_LENGTH 1000000
void Cons_O::setCdr(T_sp c) {
  _G();
  this->_Cdr = c;
//	this->_CdrLength = c->cdrLength()+1;
#if 0
	if ( this->_CdrLength > WARN_CONS_LENGTH )
	{
	    string sss = _rep_(this->_Car).substr(0,30);
	    _lisp->print(BF("WARNING@%s:%d  Cons length is > %d - its cdr length is(%d) : trunated head: %s") % __FILE__ % __LINE__ % WARN_CONS_LENGTH % this->_CdrLength % sss );
	    LOG(BF("WARNING: Cons length is > %d it is %d cdr cells : truncated head: %s") % WARN_CONS_LENGTH % this->_CdrLength % _rep_(this->_Car).substr(0,30));
	    SIMPLE_ERROR(BF("Cons length was too long length=%d") % this->_CdrLength );
	}
#endif
}

#if 0
    void	Cons_O::setOCdr(T_sp c)
    {_G();
	this->_Cdr = c;
	if ( cl_consp(c) )
	{
	    List_sp cc = c;
//	    this->_CdrLength = cc->cdrLength()+1;
	} else
	{
//	    if ( c.nilp() ) this->_CdrLength = 0;
//	    else              this->_CdrLength = 1;
	}
#if 0
	if ( this->_CdrLength > WARN_CONS_LENGTH )
	{
	    string sss = _rep_(this->_Car).substr(0,30);
	    _lisp->print(BF("WARNING@%s:%d  Cons length is > %d - its cdr length is(%d) : trunated head: %s") % __FILE__ % __LINE__ % WARN_CONS_LENGTH % this->_CdrLength % sss );
	    LOG(BF("WARNING: Cons length is > %d it is %d cdr cells : truncated head: %s") % WARN_CONS_LENGTH % this->_CdrLength % _rep_(this->_Car).substr(0,30));
	    SIMPLE_ERROR(BF("Cons length was too long length=%d") % this->_CdrLength );
	}
#endif
    }
#endif

T_sp Cons_O::olookupKeyObjectDefault(Symbol_sp keyword, T_sp dft) {
  _OF();
  ASSERTP(keyword->isKeywordSymbol(), "You can only search for keyword symbols");
  LOG(BF("lookup %s in %s") % _rep_(keyword) % this->__repr__());
  List_sp lp = this->asSmartPtr();
  LOG(BF("Got start of list to search: %s") % _rep_(lp));
  for (auto p : lp) {
    if (cl_symbolp(oCar(p))) {
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

T_sp Cons_O::olookupKeyObject(Symbol_sp key) {
  _G();
  Cons_sp p;
  return ((this->olookupKeyObjectDefault(key, _Nil<T_O>())));
}

string Cons_O::__repr__() const {
  _G();
  Cons_sp start = this->asSmartPtr();
  T_sp cdr = start;
  stringstream sout;
  if (oCar(start) == cl::_sym_quote) {
    if (cl_consp(oCdr(start))) {
      sout << "'" << _rep_(oCadr(start)) << " ";
    } else {
      sout << "QUOTE ." << _rep_(oCdr(start)) << " ";
    }
    return ((sout.str()));
  }
  sout << "(";
  while (cdr.notnilp()) {
    if (cl_consp(cdr)) {
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

#if 0
    SourceCodeCons_sp SourceCodeCons_O::createList(T_sp o1, T_sp o2, uint ln, uint col, SourceFileInfo_sp fileName)
    {
	return(SourceCodeCons_O::create(o1,SourceCodeCons_O::create(o2,_Nil<SourceCodeCons_O>(),ln,col,fileName),ln,col,fileName));
    }

    SourceCodeCons_sp SourceCodeCons_O::createList(T_sp o1, T_sp o2, T_sp o3, uint ln, uint col, SourceFileInfo_sp fileName)
    {
	return((SourceCodeCons_O::create(o1,SourceCodeCons_O::createList(o2,o3,ln,col,fileName),ln,col,fileName)));
    }

    SourceCodeCons_sp SourceCodeCons_O::createList(T_sp o1, T_sp o2, T_sp o3, T_sp o4, uint ln, uint col, SourceFileInfo_sp fileName)
    {
	return((SourceCodeCons_O::create(o1,SourceCodeCons_O::createList(o2,o3,o4,ln,col,fileName),ln,col,fileName)));
    }

    SourceCodeCons_sp SourceCodeCons_O::createList(T_sp o1, T_sp o2, T_sp o3, T_sp o4, T_sp o5, uint ln, uint col, SourceFileInfo_sp fileName)
    {
	return((SourceCodeCons_O::create(o1,SourceCodeCons_O::createList(o2,o3,o4,o5,ln,col,fileName),ln,col,fileName)));
    }

    SourceCodeCons_sp SourceCodeCons_O::createList(T_sp o1, T_sp o2, T_sp o3, T_sp o4, T_sp o5, T_sp o6, uint ln, uint col, SourceFileInfo_sp fileName)
    {
	return((SourceCodeCons_O::create(o1,SourceCodeCons_O::createList(o2,o3,o4,o5,o6,ln,col,fileName),ln,col,fileName)));
    }



    SourceCodeCons_sp SourceCodeCons_O::createList(T_sp o1, T_sp o2, const LispParserPos& pos, SourceFileInfo_sp fileName)
    { return((SourceCodeCons_O::createList(o1,o2,pos.first_line,pos.first_column,fileName)));    }
    SourceCodeCons_sp SourceCodeCons_O::createList(T_sp o1, T_sp o2, T_sp o3, const LispParserPos& pos, SourceFileInfo_sp fileName )
    { return((SourceCodeCons_O::createList(o1,o2,o3,pos.first_line,pos.first_column,fileName)));    }
    SourceCodeCons_sp SourceCodeCons_O::createList(T_sp o1, T_sp o2, T_sp o3, T_sp o4, const LispParserPos& pos, SourceFileInfo_sp fileName )
    { return((SourceCodeCons_O::createList(o1,o2,o3,o4,pos.first_line,pos.first_column,fileName)));    }
    SourceCodeCons_sp SourceCodeCons_O::createList(T_sp o1, T_sp o2, T_sp o3, T_sp o4, T_sp o5, const LispParserPos& pos, SourceFileInfo_sp fileName )
    { return((SourceCodeCons_O::createList(o1,o2,o3,o4,o5,pos.first_line,pos.first_column,fileName)));    }
    SourceCodeCons_sp SourceCodeCons_O::createList(T_sp o1, T_sp o2, T_sp o3, T_sp o4, T_sp o5, T_sp o6,const LispParserPos& pos, SourceFileInfo_sp fileName)
    { return((SourceCodeCons_O::createList(o1,o2,o3,o4,o5,o6,pos.first_line,pos.first_column,fileName)));    }
    SourceCodeCons_sp SourceCodeCons_O::create(T_sp car, Cons_sp cdr, const LispParserPos& pos, SourceFileInfo_sp fileName,Lisp_sp lisp)
    { return((SourceCodeCons_O::create(car,cdr,pos.first_line,pos.first_column,fileName)));}

    SourceCodeCons_sp SourceCodeCons_O::create(T_sp car, T_sp cdr,
					       int lineNumber,
					       int column,
					       SourceFileInfo_sp fileName, Lisp_sp e )
    {

        GC_ALLOCATE(SourceCodeCons_O,ll );
	    ll->setCar(car);
	    ll->setOCdr(cdr);
	    ll->_ParsePosLineNumber = lineNumber;
	    ll->_ParsePosColumn = column;
	    ll->_SourceFileInfo = fileName;
	return((ll));
    };



    SourceCodeCons_sp SourceCodeCons_O::create(	int lineNumber,	int column,
						SourceFileInfo_sp fileName, Lisp_sp e )
    {
        GC_ALLOCATE(SourceCodeCons_O,ll );
	    ll->_ParsePosLineNumber = lineNumber;
	    ll->_ParsePosColumn = column;
	    ll->_SourceFileInfo = fileName;
	return((ll));
    };


    SourceCodeCons_sp SourceCodeCons_O::createWithDuplicateSourceCodeInfo(T_sp car, Cons_sp cdr,
									  List_sp parsed)
    {
	int lineNumber, col;
	parsed->getParsePos(lineNumber,col);
	SourceFileInfo_sp fileName = core_sourceFileInfo(parsed);
	return((SourceCodeCons_O::create(car,cdr,lineNumber,col,fileName,env)));
    }

    SourceCodeCons_sp SourceCodeCons_O::createWithDuplicateSourceCodeInfo(T_sp car, Cons_sp parsed, Lisp_sp lisp)
    {
	int lineNumber, col;
	parsed->getParsePos(lineNumber,col);
	SourceFileInfo_sp fileName = core_sourceFileInfo(parsed);
	return((SourceCodeCons_O::create(car,_Nil<T_O>(),lineNumber,col,fileName,lisp)));
    }


    SourceCodeCons_sp SourceCodeCons_O::createWithDuplicateSourceCodeInfo(Cons_sp parsed, Lisp_sp env)
    {
	if ( parsed.nilp() ) return((_Nil<SourceCodeCons_O>()));
	int lineNumber, col;
	parsed->getParsePos(lineNumber,col);
	SourceFileInfo_sp fileName = core_sourceFileInfo(parsed);
	return((SourceCodeCons_O::create(lineNumber,col,fileName,env)));
    }


    SourceCodeCons_O::SourceCodeCons_O(): T_O(), Base(), _SourceFileInfo(_Nil<SourceFileInfo_O>()) {};

    SourceCodeCons_O::~SourceCodeCons_O() {};

    void SourceCodeCons_O::initialize()
    {
	this->Base::initialize();
	this->_SourceFileInfo = _Nil<SourceFileInfo_O>();
    }

    void SourceCodeCons_O::duplicateSourceCodeInfo(Cons_sp c)
    {
	int lineNumber, col;
	string fileName;
	c->getParsePos(lineNumber,col);
	this->_SourceFileInfo = core_sourceFileInfo(c);
	this->_ParsePosLineNumber = lineNumber;
	this->_ParsePosColumn = col;
    }

#if 0
    bool SourceCodeCons_O::equal(T_sp obj) const
    {_OF();
	if ( this->eq(obj) ) return((true));
	if ( !obj->sourceCodeConsP() ) return((false));
	SourceCodeCons_sp other = obj.as<SourceCodeCons_O>();
	if ( this->_FileName != other->_FileName ) return((false));
	if ( this->_ParsePosLineNumber != other->_ParsePosLineNumber ) return((false));
	if ( this->_ParsePosColumn != other->_ParsePosColumn ) return((false));
	if ( !cl_equal(this->ocar(),other->ocar() ) ) return((false));
	if ( !cl_equal(this->cdr(),other->cdr() ) ) return((false));
	return((true));
    }
#endif


    SourceFileInfo_sp SourceCodeCons_O::sourceFileInfo() const
    {_G();
	return((this->_SourceFileInfo));
    }

    string	SourceCodeCons_O::__repr__() const
    {_G();
	T_sp		op;
	Cons_sp 	p;
	stringstream	sout;
//#define	DOT_NOTATION
	sout << "( ";
	p = this->asSmartPtr();
	while ( p.notnilp() )
	{
	    T_sp obj = oCar(p);
	    if ( !obj )
	    {
		sout << ">>>UNDEFINED OCAR<<<";
	    } else if ( obj.nilp() )
	    {
		sout << "nil ";
	    } else
	    {
		sout << _rep_(obj) << " ";
	    }
	    op = oCdr(p);
	    if ( !op )
	    {
		sout << ">>>>> NULL CDR <<<<<<";
		break;
	    }
	    if ( op.unboundp() )
	    {
		sout << ">>>>> UNBOUND CDR <<<<<<";
		break;
	    }
	    if ( !cl_consp(op) )
	    {
		p = _Nil<T_O>();
		if ( op.notnilp() )
		{
		    sout << " . " << _rep_(op);
		}
		break;
	    }
	    p = op;
	}
	if ( _sym_STARprint_source_code_consSTAR->symbolValue().isTrue() )
	{
	    sout << "#<@";
	    if ( this->hasParsePos() )
	    {
		sout << "\"" << this->_SourceFileInfo->permanentFileName() << "\"";
		sout << this->_ParsePosLineNumber;
		sout << ":" << this->_ParsePosColumn;
	    } else
	    {
		sout << "NO-POS";
	    }
	    sout << "@";
	    sout << ">";
	}
	sout << ")";
	return((sout.str()));
    }


    Cons_sp SourceCodeCons_O::walkToFindParsePos() const
    {_G();
	if ( this->hasParsePos() ) return((this->asSmartPtr()));
	return((this->Base::walkToFindParsePos()));
    }

#if defined(XML_ARCHIVE)
    void	SourceCodeCons_O::archiveBase(::core::ArchiveP node)
    {_G();
	this->Base::archiveBase(node);
	node->attribute("ParsePosLineNumber",this->_ParsePosLineNumber);
	node->attribute("ParsePosColumn",this->_ParsePosColumn);
    }
#endif // defined(XML_ARCHIVE)

#if defined(OLD_SERIALIZE)
    void SourceCodeCons_O::serialize(serialize::SNode node)
    {_OF();
	this->Base::serialize(node);
	node->attribute("ParsePosLineNumber",this->_ParsePosLineNumber);
	node->attribute("ParsePosColumn",this->_ParsePosColumn);
    }
#endif


    Cons_sp SourceCodeCons_O::copyList() const
    {_OF();
	Cons_sp p = this->asSmartPtr();
	ql::source_code_list list;
	while ( p.notnilp() )
	{
	    T_sp obj = oCar(p);
	    list << obj;
	    list.set_tail_source_info(p);
	    T_sp ocdr = oCdr(p);
	    if ( !cl_consp(ocdr) )
	    {
		list.dot(ocdr);
		break;
	    }
	    p = cCdr(p);
	}
	return((list.cons()));
    }



    Cons_sp SourceCodeCons_O::copyListCar() const
    {_OF();
	T_sp obj = this->_Car;
	Cons_sp rootCopy = SourceCodeCons_O::create(_Nil<T_O>(),_Nil<T_O>(),this->lineNumber(),this->column(),this->sourceFileInfo());
	rootCopy->setCar(obj);
	return((rootCopy));
    }


WORKING
    List_sp SourceCodeCons_O::copyTreeCar() const
    {_OF();
	Cons_sp rootCopy = SourceCodeCons_O::create(_Nil<T_O>(),_Nil<T_O>(),this->lineNumber(),this->column(),this->sourceFileInfo());
	T_sp obj = this->_Car;
	if ( cl_consp(obj) )
	{
	    Cons_sp carTree = obj.as<Cons_O>()->copyTree().as<Cons_O>();
	    rootCopy->setCar(carTree);
	} else
	{
	    rootCopy->setCar(obj);
	}
	return((rootCopy));
    }

#endif

List_sp alist_erase(List_sp alist, T_sp key) {
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

List_sp alist_push(List_sp alist, T_sp key, T_sp val) {
  Cons_sp one = Cons_O::create(key, val);
  alist = Cons_O::create(one, alist);
  return alist;
}

List_sp alist_get(List_sp alist, T_sp key) {
  while (alist.notnilp()) {
    if (oCar(oCar(alist)) == key) {
      return alist;
    }
    alist = oCdr(alist);
  }
  return _Nil<T_O>();
}

string alist_asString(List_sp alist) {
  stringstream ss;
  while (alist.notnilp()) {
    ss << _rep_(oCar(oCar(alist))) << " ";
    alist = oCdr(alist);
  }
  return ss.str();
}

void Cons_O::exposeCando(Lisp_sp lisp) {
  _G();
  class_<Cons_O>()
      .def("core:exactlyMatches", &Cons_O::exactlyMatches)
      .def("core:lookup", &Cons_O::olookupKeyObject)
      .def("core:lookupDefault", &Cons_O::olookupKeyObjectDefault)
      .def("core:filterOutNil", &Cons_O::filterOutNil)
      .def("core:extend", &Cons_O::extend)
      .def("core:cons-setf-car", &Cons_O::setf_car)
      .def("core:cons-setf-cdr", &Cons_O::setf_cdr)
      //	    .def("walkToFindParsePos",&Cons_O::walkToFindParsePos)
      //	    .def("sourceFileInfo-lineno",&Cons_O::lineNumber)
      //	    .def("sourceFileInfo-column",&Cons_O::column)
      //	    .def("sourceFileInfo",&Cons_O::sourceFileInfo)
      //        .def_raw("eval",&Cons_O::evaluateArgsAsExpression)
      ;
  SYMBOL_EXPORT_SC_(ClPkg, make_list);
  Defun(make_list);
  Defun(append2);
  SYMBOL_EXPORT_SC_(ClPkg, cons);
  Defun(cons);
  SYMBOL_EXPORT_SC_(ClPkg, getf);
  ClDefun(getf);
  SYMBOL_EXPORT_SC_(CorePkg, rem_f);
  CoreDefun(rem_f);
  SYMBOL_SC_(CorePkg, put_f);
  CoreDefun(put_f);
  af_def(ClPkg, "rplaca", &cl_rplaca);
  af_def(ClPkg, "rplacd", &cl_rplacd);
  af_def(ClPkg, "rest", &oCdr);
  af_def(ClPkg, "car", &oCar);
  af_def(ClPkg, "cdr", &oCdr);
  af_def(ClPkg, "caar", &oCaar);
  af_def(ClPkg, "cadr", &oCadr);
  af_def(ClPkg, "cdar", &oCdar);
  af_def(ClPkg, "cddr", &oCddr);
  af_def(ClPkg, "caaar", &oCaaar);
  af_def(ClPkg, "caadr", &oCaadr);
  af_def(ClPkg, "cadar", &oCadar);
  af_def(ClPkg, "caddr", &oCaddr);
  af_def(ClPkg, "cdaar", &oCdaar);
  af_def(ClPkg, "cdadr", &oCdadr);
  af_def(ClPkg, "cddar", &oCddar);
  af_def(ClPkg, "cdddr", &oCdddr);
  af_def(ClPkg, "caaaar", &oCaaaar);
  af_def(ClPkg, "caadar", &oCaadar);
  af_def(ClPkg, "cadaar", &oCadaar);
  af_def(ClPkg, "caddar", &oCaddar);
  af_def(ClPkg, "cdaaar", &oCdaaar);
  af_def(ClPkg, "cdadar", &oCdadar);
  af_def(ClPkg, "cddaar", &oCddaar);
  af_def(ClPkg, "cdddar", &oCdddar);
  af_def(ClPkg, "caaadr", &oCaaadr);
  af_def(ClPkg, "caaddr", &oCaaddr);
  af_def(ClPkg, "cadadr", &oCadadr);
  af_def(ClPkg, "cadddr", &oCadddr);
  af_def(ClPkg, "cdaadr", &oCdaadr);
  af_def(ClPkg, "cdaddr", &oCdaddr);
  af_def(ClPkg, "cddadr", &oCddadr);
  af_def(ClPkg, "cddddr", &oCddddr);
  af_def(ClPkg, "First", &oFirst);
  af_def(ClPkg, "Second", &oSecond);
  af_def(ClPkg, "Third", &oThird);
  af_def(ClPkg, "Fourth", &oFourth);
  af_def(ClPkg, "Fifth", &oFifth);
  af_def(ClPkg, "Sixth", &oSixth);
  af_def(ClPkg, "Seventh", &oSeventh);
  af_def(ClPkg, "Eighth", &oEighth);
  af_def(ClPkg, "Ninth", &oNinth);
  af_def(ClPkg, "Tenth", &oTenth);

  af_def(CorePkg, "alist_erase", &alist_erase);
  af_def(CorePkg, "alist_push", &alist_push);
  af_def(CorePkg, "alist_get", &alist_get);
  af_def(CorePkg, "alist_asString", &alist_asString);
}

void Cons_O::exposePython(Lisp_sp lisp) {
  _G();
#ifdef USEBOOSTPYTHON //[
  PYTHON_CLASS(CorePkg, Cons, "", "", _lisp)
      .def("__repr__", &Cons_O::__repr__);
#endif
}

EXPOSE_CLASS(core, Cons_O);
};
