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
#define DEBUG_LEVEL_FULL

#include <clasp/core/common.h>
#include <clasp/core/environment.h>
#include <clasp/core/ql.h>
#include <clasp/core/lispList.h>
#include <clasp/core/symbolTable.h>
#include <clasp/core/designators.h>
#include <clasp/core/wrappers.h>
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
  T_sp res = (*t->test_fn->closure)(LCC_PASS_ARGS2_ELLIPSIS(t->item_compared.raw_(), x.raw_()));
  return res.notnilp();
}

static bool
test_compare_not(struct cl_test *t, T_sp x) {
  x = KEY(t, x);
  //t->env->function = t->test_function;
  T_sp res = (*t->test_fn->closure)(LCC_PASS_ARGS2_ELLIPSIS(t->item_compared.raw_(), x.raw_()));
  return res.nilp();
}

static bool
test_eq(struct cl_test *t, T_sp x) {
  return (t->item_compared == KEY(t, x));
}

static bool
test_eql(struct cl_test *t, T_sp x) {
  return cl_eql(t->item_compared, KEY(t, x));
}

static bool
test_equal(struct cl_test *t, T_sp x) {
  return cl_equal(t->item_compared, KEY(t, x));
}

static bool
test_equalp(struct cl_test *t, T_sp x) {
  return cl_equalp(t->item_compared, KEY(t, x));
}

static T_sp
key_function(struct cl_test *t, T_sp x) {
  //t->env->function = t->key_function;
  T_mv result;
  return (*t->key_fn->closure)(LCC_PASS_ARGS1_ELLIPSIS(x.raw_()));
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
#define ARGS_cl_rassoc "(item a-list &key test test-not key)"
#define DECL_cl_rassoc ""
#define DOCS_cl_rassoc "See CLHS rassoc"
T_sp cl_rassoc(T_sp item, List_sp a_list, T_sp test, T_sp test_not, T_sp key) {
  struct cl_test t;
  if (test.notnilp())
    test = coerce::functionDesignator(test);
  if (test_not.notnilp())
    test_not = coerce::functionDesignator(test_not);
  if (key.notnilp())
    key = coerce::functionDesignator(key);
  setup_test(&t, item, test, test_not, key);
  for (auto calist : a_list) {
    T_sp pair = oCar(calist);
    if (pair.notnilp()) {
      if (!pair.consp())
        TYPE_ERROR_LIST(pair);
      if (TEST(&t, oCdr(pair))) {
        a_list = pair;
        break;
      }
    }
  } //end_loop_for_in;
  close_test(&t);
  return a_list;
}

#define ARGS_cl_nth "(idx arg)"
#define DECL_cl_nth ""
#define DOCS_cl_nth "See CLHS nth"
T_sp cl_nth(int idx, T_sp arg) {
  _G();
  if (arg.nilp())
    return _Nil<T_O>();
  if (Cons_sp list = arg.asOrNull<Cons_O>()) {
    return list->onth(idx);
  }
  TYPE_ERROR(arg, cl::_sym_list);
};

#define ARGS_cl_nthcdr "(idx arg)"
#define DECL_cl_nthcdr ""
#define DOCS_cl_nthcdr "See CLHS nth"
T_sp cl_nthcdr(int idx, T_sp arg) {
  _G();
  if (arg.nilp())
    return arg;
  if (Cons_sp list = arg.asOrNull<Cons_O>()) {
    return list->onthcdr(idx);
  }
  TYPE_ERROR(arg, cl::_sym_list);
};

#define ARGS_cl_copyList "(arg)"
#define DECL_cl_copyList ""
#define DOCS_cl_copyList "copyList"
T_sp cl_copyList(T_sp arg) {
  _G();
  if (arg.nilp())
    return arg;
  if (Cons_sp l = arg.asOrNull<Cons_O>()) {
    return l->copyList();
  }
  TYPE_ERROR(arg, cl::_sym_list);
};
/*! Code translated from ecl_butlast */
#define ARGS_af_butlast "(list &optional (n 1))"
#define DECL_af_butlast ""
#define DOCS_af_butlast "butlast"
List_sp af_butlast(List_sp ll, Integer_sp in) {
  gc::Fixnum n = clasp_to_int(in);
  T_sp r;
  T_sp l = ll;
  for (r = l; n && cl_consp(r); --n, r = oCdr(r))
    ;
  if (r.nilp())
    return _Nil<T_O>();
  else if (!cl_listp(r)) {
    if (r == l) {
      TYPE_ERROR_LIST(r);
    }
    return _Nil<T_O>();
  } else {
    Cons_sp head;
    Cons_sp tail;
    head = tail = Cons_O::create(oCar(l));
    while (l = oCdr(l), r = oCdr(r), cl_consp(r)) {
      Cons_sp cons = Cons_O::create(oCar(l));
      tail->rplacd(cons);
      tail = cons;
    }
    return head;
  }
}
#define ARGS_cl_nbutlast "(list &optional (n 1))"
#define DECL_cl_nbutlast ""
#define DOCS_cl_nbutlast "butlast"
List_sp cl_nbutlast(List_sp l, Integer_sp in) {
  T_sp r;
  gc::Fixnum n = clasp_to_fixnum(in);
  if (clasp_unlikely(!cl_listp(l)))
    ERROR_WRONG_TYPE_ONLY_ARG(cl::_sym_nbutlast, l, cl::_sym_list);
  for (n++, r = l; n && cl_consp(r); n--, r = oCdr(r))
    ;
  if (n == 0) {
    Cons_sp tail = gc::As<Cons_sp>(l);
    while (cl_consp(r)) {
      tail = gc::As<Cons_sp>(oCdr(tail));
      r = oCdr(r);
    }
    tail->rplacd(_Nil<T_O>());
    return l;
  }
  return _Nil<T_O>();
}

#define ARGS_cl_list "(&rest objects)"
#define DECL_cl_list ""
#define DOCS_cl_list "See CLHS: list"
T_sp cl_list(T_sp objects) {
  _G();
  return objects;
};

#define DOCS_cl_listSTAR "list* see CLHS"
#define LOCK_cl_listSTAR 0
#define ARGS_cl_listSTAR "(&rest objects)"
#define DECL_cl_listSTAR ""
T_sp cl_listSTAR(T_sp tobjects) {
  _G();
  T_sp objects = tobjects;
  if (objects.nilp()) FEargument_number_error(clasp_make_fixnum(0),clasp_make_fixnum(1),_Nil<T_O>());
  if (oCdr(objects).nilp())
    return (oCar(objects));
  Cons_sp cur;
  ql::list result(_lisp);
  for (; oCdr(objects).notnilp(); objects = oCdr(objects)) {
    //	    printf("Adding %s\n", _rep_(oCar(objects)).c_str());
    result << oCar(objects);
  }
  //	printf("dotting %s\n", _rep_(objects).c_str());
  result.dot(oCar(objects));
  return result.cons();
}

#define DOCS_cl_last "last - see CLHS"
#define LOCK_cl_last 1
#define ARGS_cl_last "(list &optional (on 1))"
#define DECL_cl_last ""
T_sp cl_last(T_sp list, int n) {
  if (list.nilp())
    return list;
  if (n < 0)
    CELL_ERROR(make_fixnum(n));
  if (Cons_sp clist = list.asOrNull<Cons_O>()) {
    return clist->last(n);
  }
  TYPE_ERROR(list, cl::_sym_list);
};

/* Adapted from ECL list.d nconc function */

#define ARGS_cl_nconc "(&rest lists)"
#define DECL_cl_nconc ""
#define DOCS_cl_nconc "tnconc"
T_sp cl_nconc(List_sp lists) {
  _G();
  T_sp head = _Nil<T_O>();
  T_sp tail = _Nil<T_O>();
  for (auto cur : lists) {
    T_sp new_tail;
    T_sp other = oCar(cur);
    if (other.nilp()) {
      new_tail = tail;
    } else if (Cons_sp cother = other.asOrNull<Cons_O>()) {
      new_tail = cl_last(cother, 1);
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
    Cons_sp last = gc::As<Cons_sp>(cl_last(conslist, 1));
    last->rplacd(y);
    return conslist;
  }
  TYPE_ERROR(l, cl::_sym_list);
}

#define DOCS_af_revappend "revappend"
#define LOCK_af_revappend 1
#define ARGS_af_revappend "(list tail)"
#define DECL_af_revappend ""
T_sp af_revappend(List_sp list, T_sp tail) {
  _G();
  if (list.nilp())
    return (tail);
  return list.asCons()->revappend(tail);
};

#define DOCS_cl_nreconc "nreconc"
#define LOCK_cl_nreconc 1
#define ARGS_cl_nreconc "(list tail)"
#define DECL_cl_nreconc ""
T_sp cl_nreconc(List_sp list, T_sp tail) {
  _G();
  if (list.nilp())
    return (tail);
  return list.asCons()->nreconc(tail);
};

#if 0
    EXPOSE_CLASS(core,List_O);

    void List_O::exposeCando(Lisp_sp lisp)
    {_G();
	class_<List_O>()
	    //	    .def("revappend",&List_O::revappend)    I need to wrap this
	    //	    .def("nreconc",&List_O::nreconc)	I need to wrap this
	    //	.initArgs("(self)")
	    ;
    }
#endif

void initialize_list() {
  SYMBOL_EXPORT_SC_(ClPkg, revappend);
  Defun(revappend);
  SYMBOL_EXPORT_SC_(ClPkg, nreconc);
  ClDefun(nreconc);
  SYMBOL_EXPORT_SC_(ClPkg, list);
  ClDefun(list);
  SYMBOL_EXPORT_SC_(ClPkg, listSTAR);
  ClDefun(listSTAR);
  SYMBOL_EXPORT_SC_(ClPkg, butlast);
  Defun(butlast);
  SYMBOL_EXPORT_SC_(ClPkg, nbutlast);
  ClDefun(nbutlast);
  SYMBOL_EXPORT_SC_(ClPkg, nth);
  ClDefun(nth);
  ClDefun(rassoc);
  SYMBOL_EXPORT_SC_(ClPkg, nthcdr);
  ClDefun(nthcdr);
  SYMBOL_EXPORT_SC_(ClPkg, copyList);
  ClDefun(copyList);
  SYMBOL_EXPORT_SC_(ClPkg, last);
  ClDefun(last);
  ClDefun(nconc);
}

#if 0
    void List_O::exposePython(::core::Lisp_sp lisp)
    {
#ifdef USEBOOSTPYTHON
	PYTHON_CLASS(Pkg(),List,"","",_LISP)
	    //	.initArgs("(self)")
	    ;
#endif
    }
#endif

#if 0
    void List_O::serialize(::serialize::SNodeP node)
    {
	IMPLEMENT_ME();
	this->Bases::serialize(node);
	// Archive other instance variables here
    }
#endif

#if 0
    void List_O::archiveBase(::core::ArchiveP node)
    {
	IMPLEMENT_ME();
	this->Base1::archiveBase(node);
	// Archive other instance variables here
    }
#endif

EXPOSE_CLASS(core, VaList_dummy_O);

void VaList_dummy_O::exposeCando(::core::Lisp_sp lisp) {
  ::core::class_<VaList_dummy_O>();
};

void VaList_dummy_O::exposePython(::core::Lisp_sp lisp){};

}; /* core */
