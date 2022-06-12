/*
    File: lispList.cc
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
//#define DEBUG_LEVEL_FULL

#include <iostream>

#include <clasp/core/foundation.h>
#include <clasp/core/common.h>
#include <clasp/core/environment.h>
#include <clasp/core/ql.h>
#include <clasp/core/lispList.h>
#include <clasp/core/symbolTable.h>
#include <clasp/core/designators.h>
#include <clasp/core/wrappers.h>
#include <clasp/core/bignum.h>
namespace core {

/*! Duplicated from ECL list.d
     */
struct cl_test {
  bool (*test_c_function)(struct cl_test *, T_sp);
  T_sp (*key_c_function)(struct cl_test *, T_sp);
  //cl_env_ptr env;
  T_sp key_function;
  Function_sp key_fn;
  T_sp test_function;
  Function_sp test_fn;
  T_sp item_compared;
};

#if 0
    static T_sp subst(struct cl_test *t, T_sp new_obj, T_sp tree);
    static T_sp nsubst(struct cl_test *t, T_sp new_obj, T_sp tree);
    static T_sp sublis(struct cl_test *t, T_sp alist, T_sp tree);
    static T_sp nsublis(struct cl_test *t, T_sp alist, T_sp tree);
    static T_sp do_assoc(struct cl_test *t, T_sp alist);
#endif
#define TEST(t, k) ((t)->test_c_function)((t), (k))
#define KEY(t, x) ((t)->key_c_function)((t), (x))
#define close_test(t) (void)0

static bool
test_compare(struct cl_test *t, T_sp x) {
  x = KEY(t, x);
  //t->env->function = t->test_function;
  MAKE_STACK_FRAME( frame, 2 );
  gctools::fill_frame_one_indexed( frame, 0, t->item_compared.raw_() );
  gctools::fill_frame_one_indexed( frame, 1, x.raw_() );
  T_sp res = (*t->test_fn).entry()(t->test_fn.raw_(),2,frame->arguments());
  return res.notnilp();
}

static bool
test_compare_not(struct cl_test *t, T_sp x) {
  x = KEY(t, x);
  //t->env->function = t->test_function;
  MAKE_STACK_FRAME( frame, 2 );
  gctools::fill_frame_one_indexed( frame, 0, t->item_compared.raw_() );
  gctools::fill_frame_one_indexed( frame, 1, x.raw_() );
  T_sp res = (*t->test_fn).entry()(t->test_fn.raw_(),2,frame->arguments());
  return res.nilp();
}

static bool
test_eq(struct cl_test *t, T_sp x) {
  return (t->item_compared == KEY(t, x));
}

static bool
test_eql(struct cl_test *t, T_sp x) {
  return cl__eql(t->item_compared, KEY(t, x));
}

static bool
test_equal(struct cl_test *t, T_sp x) {
  return cl__equal(t->item_compared, KEY(t, x));
}

static bool
test_equalp(struct cl_test *t, T_sp x) {
  return cl__equalp(t->item_compared, KEY(t, x));
}

static T_sp
key_function(struct cl_test *t, T_sp x) {
  //t->env->function = t->key_function;
  T_mv result;
  MAKE_STACK_FRAME( frame, 1 );
  gctools::fill_frame_one_indexed( frame, 0, x.raw_() );
  return (*t->key_fn).entry()(t->key_fn.raw_(),1,frame->arguments(0));
}

static T_sp
key_identity(struct cl_test *t, T_sp x) {
  return x;
}

static void
setup_test(struct cl_test *t, T_sp item, T_sp test,
           T_sp test_not, T_sp key) {
  //cl_env_ptr env = t->env = ecl_process_env();
  t->item_compared = item;
  if (test.notnilp()) {
    if (test_not.notnilp())
      FEerror("Both :TEST and :TEST-NOT are specified.", 0);
    t->test_function = test = coerce::functionDesignator(test);
    if (test == cl::_sym_eq) {
      t->test_c_function = test_eq;
    } else if (test == cl::_sym_eql) {
      t->test_c_function = test_eql;
    } else if (test == cl::_sym_equal) {
      t->test_c_function = test_equal;
    } else if (test == cl::_sym_equalp) {
      t->test_c_function = test_equalp;
    } else {
      t->test_c_function = test_compare;
      t->test_fn = gc::As<Function_sp>(test); //ecl_function_dispatch(env, test);
      t->test_function = t->test_fn;          // env->function;
    }
  } else if (test_not.notnilp()) {
    t->test_c_function = test_compare_not;
    test_not = coerce::functionDesignator(test_not);
    t->test_fn = gc::As<Function_sp>(test_not); // ecl_function_dispatch(env, test_not);
    t->test_function = t->test_fn;              // env->function;
  } else {
    t->test_c_function = test_eql;
  }
  if (key.notnilp()) {
    key = coerce::functionDesignator(key);
    t->key_fn = gc::As<Function_sp>(key); // ecl_function_dispatch(env, key);
    t->key_function = t->key_fn;          // env->function;
    t->key_c_function = key_function;
  } else {
    t->key_c_function = key_identity;
  }
}

// ----------------------------------------------------------------------
//

/*! Duplicated from ECL rassoc */
CL_LAMBDA(item a-list &key test test-not key)
CL_DECLARE();
CL_UNWIND_COOP(true);
CL_DOCSTRING(R"dx(See CLHS rassoc)dx")
DOCGROUP(clasp)
CL_DEFUN T_sp cl__rassoc(T_sp item, List_sp a_list, T_sp test, T_sp test_not, T_sp key) {
  struct cl_test t;
  if (test.notnilp())
    test = coerce::functionDesignator(test);
  if (test_not.notnilp())
    test_not = coerce::functionDesignator(test_not);
  if (key.notnilp())
    key = coerce::functionDesignator(key);
  setup_test(&t, item, test, test_not, key);
  for (auto calist : a_list) {
    T_sp pair = CONS_CAR(calist);
    LIKELY_if (pair.consp()) {
      if (TEST(&t,CONS_CDR(pair))) {
        close_test(&t);
        return pair;
      }
    } else if (pair.notnilp()) {
      TYPE_ERROR_LIST(pair);
    }
  } //end_loop_for_in;
  close_test(&t);
  return nil<T_O>();
}

CL_LAMBDA(idx arg)
CL_DECLARE();
CL_DOCSTRING(R"dx(See CLHS nth)dx")
DOCGROUP(clasp)
CL_DEFUN T_sp cl__nth(Integer_sp idx, List_sp arg) {
  // should error on negative number
  // should return nil on positive bignums
  LIKELY_if (arg.consp()) {
    if (idx.fixnump()) {
      gc::Fixnum n = idx.unsafe_fixnum();
      if (n < 0) 
        TYPE_ERROR(idx, cl::_sym_UnsignedByte);
      else return arg.unsafe_cons()->onth(n);
    } else { // index is a bignum, i.e. out of range
      if (clasp_plusp (idx))
        return nil<T_O>();
      else TYPE_ERROR(idx, cl::_sym_UnsignedByte);
    }
  } else if (arg.nilp()) {
    return arg;
  }
  TYPE_ERROR(arg, cl::_sym_list);
};

CL_LAMBDA(idx arg)
CL_DECLARE();
CL_DOCSTRING(R"dx(See CLHS nthcdr)dx")
DOCGROUP(clasp)
CL_DEFUN T_sp cl__nthcdr(Integer_sp idx, List_sp arg) {
  LIKELY_if (arg.consp()) {
    if (idx.fixnump()) {
      gc::Fixnum n = idx.unsafe_fixnum();
      if (n < 0) 
        TYPE_ERROR(idx, cl::_sym_UnsignedByte);
      else return arg.unsafe_cons()->onthcdr(n);
    } else { // bignum, out of range
      if (clasp_plusp (idx))
        return nil<T_O>();
      else 
        TYPE_ERROR(idx, cl::_sym_UnsignedByte);
    }
  } else if (arg.nilp()) {
    return arg;
  }
  TYPE_ERROR(arg, cl::_sym_list);
}

CL_LAMBDA(arg)
CL_DECLARE();
CL_DOCSTRING(R"dx(copyList)dx")
DOCGROUP(clasp)
CL_DEFUN T_sp cl__copy_list(List_sp arg) {
  if (arg.consp()) return arg.unsafe_cons()->copyList();
  if (arg.nilp()) return arg;
  TYPE_ERROR(arg, cl::_sym_list);
};
/*! Code translated from ecl_butlast */
CL_LAMBDA(list &optional (n 1))
CL_DECLARE();
CL_DOCSTRING(R"dx(butlast)dx")
DOCGROUP(clasp)
CL_DEFUN List_sp cl__butlast(List_sp ll, Integer_sp in) {
   if (ll.nilp())
    return ll;
   if (in.fixnump()) {
     gc::Fixnum n = in.unsafe_fixnum();
     if (n < 0) 
       TYPE_ERROR(in, cl::_sym_UnsignedByte);
     // n is postive fixnum
	  T_sp r;
	  T_sp l = ll;
	  for (r = l; n && (r).consp(); --n, r = oCdr(r))
		;
	  if (r.nilp())
		return nil<T_O>();
	  else if (!cl__listp(r)) {
		if (r == l) {
		  TYPE_ERROR_LIST(r);
		}
		return nil<T_O>();
	  } else {
		Cons_sp head;
		Cons_sp tail;
		head = tail = Cons_O::create(oCar(l),nil<T_O>());
		while (l = oCdr(l), r = oCdr(r), (r).consp()) {
		  Cons_sp cons = Cons_O::create(oCar(l),nil<T_O>());
		  tail->rplacd(cons);
		  tail = cons;
		}
		return head;
	  }
   } else { // must be a bignum
     if (clasp_plusp (in))
      return nil<T_O>();
     else 
       TYPE_ERROR(in, cl::_sym_UnsignedByte);
   }
}

CL_LAMBDA(list &optional (n 1))
CL_DECLARE();
CL_DOCSTRING(R"dx(nbutlast)dx")
DOCGROUP(clasp)
CL_DEFUN List_sp cl__nbutlast(List_sp l, Integer_sp in) {
  if (l.nilp()) return l;
  if (in.fixnump()) {
    gc::Fixnum n = in.unsafe_fixnum();
    if (n < 0) {
      TYPE_ERROR(in, cl::_sym_UnsignedByte);
    }
    // n is postive fixnum
    T_sp r;
    if (clasp_unlikely(!cl__listp(l)))
      ERROR_WRONG_TYPE_ONLY_ARG(cl::_sym_nbutlast, l, cl::_sym_list);
    for (n++, r = l; n && (r).consp(); n--, r = oCdr(r))
      ;
    if (n == 0) {
      Cons_sp tail = gc::As<Cons_sp>(l);
      while ((r).consp()) {
        tail = gc::As<Cons_sp>(oCdr(tail));
        r = oCdr(r);
      }
      tail->rplacd(nil<T_O>());
      return l;
    }
    return nil<T_O>();
  } else {
    // if it is a positive bignum, return nil
    if (clasp_plusp (in)) 
      return nil<T_O>();
    else 
    // negative bignum 
      TYPE_ERROR(in, cl::_sym_UnsignedByte);
  }
}

CL_LAMBDA(&rest objects)
CL_DOCSTRING(R"dx(See CLHS: list)dx")
DOCGROUP(clasp)
CL_DEFUN T_sp cl__list(T_sp objects) {
  return objects;
};

CL_LAMBDA(core:&va-rest objects)
CL_DECLARE();
CL_DOCSTRING(R"dx(list* see CLHS)dx")
DOCGROUP(clasp)
CL_DEFUN T_sp cl__listSTAR(Vaslist_sp vargs) {
  size_t nargs = vargs->remaining_nargs();
  if (nargs == 0) throwTooFewArgumentsError(nil<T_O>(),
                                            clasp_make_fixnum(0),
                                            clasp_make_fixnum(1));
  ql::list result;
  while (nargs > 1) {
    T_O* tcsp = ENSURE_VALID_OBJECT(vargs->next_arg().raw_());
    T_sp csp((gctools::Tagged)tcsp);
    result << csp;
    nargs--;
  }
  T_O* tailptr = ENSURE_VALID_OBJECT(vargs->next_arg().raw_());
  T_sp tail((gctools::Tagged)tailptr);
  result.dot(tail);
  return result.cons();
}

// Should verify the type of the argument
// Theoretically it could be a bignum
// But there are not bignum length list (there is not enough memory)
// so for a Fixnum we do the real test
// and for a Bignum we test if it is positive and than return the list
// last does not necessarily return a list, see (last '(a . b) 0) -> B

CL_LAMBDA(list &optional (on 1))
CL_DECLARE();
CL_DOCSTRING(R"dx(last - see CLHS)dx")
DOCGROUP(clasp)
CL_DEFUN T_sp cl__last(List_sp list, Integer_sp in) {
  if (list.nilp())
    return list;
  //drmeister says we should test the common case fixnum first
  if (in.fixnump()) {
    gc::Fixnum n = in.unsafe_fixnum();
     if (n < 0) TYPE_ERROR(in, cl::_sym_UnsignedByte);
     if (Cons_sp clist =  gc::As<Cons_sp>(list))
       return clist->last(n);
     TYPE_ERROR(list, cl::_sym_list);
  } else { // must be a bignum
    if (clasp_plusp (in))
      return list;
    else
      TYPE_ERROR(in, cl::_sym_UnsignedByte);     
  }
};

/* Adapted from ECL list.d nconc function */

CL_LAMBDA(&rest lists)
CL_DECLARE();
DOCGROUP(clasp)
CL_DEFUN T_sp cl__nconc(List_sp lists) {
  T_sp head = nil<T_O>();
  T_sp tail = nil<T_O>();
  for (auto cur : lists) {
    T_sp new_tail;
    T_sp other = oCar(cur);
    if (other.nilp()) {
      new_tail = tail;
    } else if (Cons_sp cother = other.asOrNull<Cons_O>()) {
      new_tail = cl__last(cother, make_fixnum(1));
    } else {
      if (oCdr(cur).notnilp()) {
        TYPE_ERROR_LIST(other);
      }
      new_tail = tail;
    }
    if (head.nilp()) {
      head = other;
    } else {
      gc::As<Cons_sp>(tail)->rplacd(other);
    }
    tail = new_tail;
  }
  return head;
}

T_sp clasp_nconc(T_sp l, T_sp y) {
  if (l.nilp()) {
    return y;
  }
  if (Cons_sp conslist = l.asOrNull<Cons_O>()) {
    Cons_sp last = gc::As<Cons_sp>(cl__last(conslist, make_fixnum(1)));
    last->rplacd(y);
    return conslist;
  }
  TYPE_ERROR(l, cl::_sym_list);
}

CL_LAMBDA(list tail)
CL_DECLARE();
CL_DOCSTRING(R"dx(revappend)dx")
DOCGROUP(clasp)
CL_DEFUN T_sp cl__revappend(List_sp list, T_sp tail) {
  if (list.nilp())
    return (tail);
  return list.asCons()->revappend(tail);
};

CL_LAMBDA(list tail)
CL_DECLARE();
CL_DOCSTRING(R"dx(nreconc)dx")
DOCGROUP(clasp)
CL_DEFUN T_sp cl__nreconc(List_sp list, T_sp tail) {
  if (list.nilp())
    return (tail);
  return list.asCons()->nreconc(tail);
};

List_sp remove_equal(T_sp element, List_sp alist) {
  ql::list new_list;
  for (List_sp cur = alist; cur.notnilp(); cur = oCdr(cur)) {
    if (!cl__equal(element, oCar(cur)))
      new_list << oCar(cur);
  }   
  return new_list.cons();
};

CL_LAMBDA(element alist)
CL_DECLARE();
CL_DOCSTRING(R"dx(remove an element from a list with test equall)dx")
DOCGROUP(clasp)
CL_DEFUN List_sp core__remove_equal(T_sp element, List_sp alist) {
  return remove_equal(element, alist);
};

  SYMBOL_EXPORT_SC_(ClPkg, revappend);
  SYMBOL_EXPORT_SC_(ClPkg, nreconc);
  SYMBOL_EXPORT_SC_(ClPkg, list);
  SYMBOL_EXPORT_SC_(ClPkg, listSTAR);
  SYMBOL_EXPORT_SC_(ClPkg, butlast);
  SYMBOL_EXPORT_SC_(ClPkg, nbutlast);
  SYMBOL_EXPORT_SC_(ClPkg, nth);
  SYMBOL_EXPORT_SC_(ClPkg, nthcdr);
  SYMBOL_EXPORT_SC_(ClPkg, copyList);
  SYMBOL_EXPORT_SC_(ClPkg, last);
  SYMBOL_EXPORT_SC_(CorePkg, remove_equal);

}; /* core */
