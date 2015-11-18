/*
    File: backquote.cc
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
#define DEBUG_LEVEL_NONE
/* Hand converted to C++ from:
;;; Common Lisp backquote implementation, written in Common Lisp. 
;;; Author: Guy L. Steele Jr.     Date: 27 December 1985 
;;; Tested under Symbolics Common Lisp and Lucid Common Lisp. 
;;; This software is in the public domain.
;;; $ is pseudo-backquote and % is pseudo-comma.  This makes it 
;;; possible to test this code without interfering with normal 
;;; Common Lisp syntax.
;;; The following are unique tokens used during processing. 
;;; They need not be symbols; they need not even be atoms.

*/

#define TRACK_CONS 0

#include <clasp/core/common.h>
#include <clasp/core/environment.h>
#include <clasp/core/ql.h>
#include <clasp/core/symbolTable.h>
#include <clasp/core/multipleValues.h>
#include <clasp/core/environment.h>
#include <clasp/core/backquote.h>
#include <clasp/core/evaluator.h>
#include <clasp/core/lispList.h>
#include <clasp/core/primitives.h>
#include <clasp/core/sequence.h>
#include <clasp/core/wrappers.h>

SYMBOL_SC_(CorePkg, STARbq_listSTAR);
SYMBOL_SC_(CorePkg, STARbq_appendSTAR);
SYMBOL_SC_(CorePkg, STARbq_listSTARSTAR);
SYMBOL_SC_(CorePkg, STARbq_nconcSTAR);
SYMBOL_SC_(CorePkg, STARbq_clobberableSTAR);
SYMBOL_SC_(CorePkg, STARbq_quoteSTAR);
SYMBOL_SC_(CorePkg, STARbq_quote_nilSTAR);
SYMBOL_SC_(CorePkg, STARbq_simplifySTAR);

SYMBOL_SC_(CorePkg, bq_simplify);
SYMBOL_SC_(CorePkg, bq_remove_tokens);

namespace core {

#define ARGS_af_quasiquote "(whole env)"
#define DECL_af_quasiquote ""
#define DOCS_af_quasiquote "quasiquote"
T_mv af_quasiquote(List_sp whole, T_sp env) {
  _G();
  ASSERT(cl_length(whole) == 2);
  T_sp form = oCadr(whole);
  return af_backquote_completely_process(form);
};

/*!
  Equivalent to Common Lisps append function
  (append a b c)
  It recreates the list structures of the first arguments a and b and strings
  them together into one list and then points the cdr of the last element of this new list
  to c.
*/
#define ARGS_af_backquote_append "(&rest lists)"
#define DECL_af_backquote_append ""
#define DOCS_af_backquote_append "append as in clhs"
T_sp af_backquote_append(List_sp lists) {
  _G();
  ql::list list; // (lists);
  LOG(BF("Carrying out append with arguments: %s") % _rep_(lists));
  List_sp appendArg = lists;
  for (; oCdr(appendArg).notnilp(); appendArg = oCdr(appendArg)) {
    T_sp head = oCar(appendArg);
    ASSERT(head.consp() || head.nilp());
    List_sp oneList = head;
    for (auto element : oneList) {
      list << oCar(element);
    }
  }
  /* Now append the last argument by setting the new lists last element cdr
	   to the last argument of append */
  List_sp result = list.dot(oCar(appendArg)).cons();
  return result;
}

#define ARGS_af_backquote_completely_process "(x)"
#define DECL_af_backquote_completely_process ""
#define DOCS_af_backquote_completely_process "backquote_completely_process"
#define LOCK_af_backquote_completely_process 0
T_mv af_backquote_completely_process(T_sp x) {
  _G();
  T_sp raw_result = af_backquote_process(x);
  if (_sym_STARbq_simplifySTAR->symbolValue().isTrue()) {
    T_sp process_result = af_backquote_simplify(raw_result);
    T_sp result = af_backquote_remove_tokens(process_result);
    (void)result;
  }
  T_sp result = af_backquote_remove_tokens(raw_result);
  return Values(result);
}

#define ARGS_af_backquote_process "(ox)"
#define DECL_af_backquote_process ""
#define DOCS_af_backquote_process "bq_process"
T_sp af_backquote_process(T_sp ox) {
  _G();
  // C-version if 0    Lisp-version if 1
  T_sp result = _Nil<T_O>();
  T_sp p;
  T_sp q;
  int step;
  T_sp cp;
  T_sp nr;
  T_sp x;
  T_sp nreconc;
  T_sp ax;
  if (cl_atom(ox)) {
    result = Cons_O::createList(_sym_STARbq_quoteSTAR, ox);
    goto DONE;
  }
  x = ox;
  ax = oCar(x);
  if (ax == _sym_backquote) {
    T_sp passone = af_backquote_completely_process(oCadr(x)); // eval::funcall(_sym_backquote_completely_process,oCadr(x));
    result = af_backquote_process(passone);                   // eval::funcall(_sym_backquote_process,passone);
    goto DONE;
  } else if (ax == _sym_unquote) {
    result = oCadr(x);
    goto DONE;
  } else if (ax == _sym_unquote_splice) {
    SIMPLE_ERROR(BF(",@%s after `") % _rep_(oCadr(x)));
  } else if (ax == _sym_unquote_nsplice) {
    SIMPLE_ERROR(BF(",.%s after `") % _rep_(oCadr(x)));
  }
  p = x;
  q = _Nil<T_O>();
  step = 0;
  while (!cl_atom(p)) {
    cp = p;
    if (oCar(cp) == _sym_unquote) {
      if (!oCddr(cp).nilp()) {
        SIMPLE_ERROR(BF("Malformed ,%s") % _rep_(p));
      }
      nreconc = cl_nreconc(q, Cons_O::create(oCadr(cp))); // eval::funcall(cl::_sym_nreconc,q,Cons_O::create(oCadr(cp)))); // HERE_scCONS_CREATE(oCadr(cp)));
      result = Cons_O::create(_sym_STARbq_appendSTAR, nreconc);
      goto DONE;
    }
    if (oCar(cp) == _sym_unquote_splice) {
      SIMPLE_ERROR(BF("Dotted ,@%s") % _rep_(p));
    }
    if (oCar(cp) == _sym_unquote_nsplice) {
      SIMPLE_ERROR(BF("Dotted ,.%s") % _rep_(p));
    }

    // Advance
    T_sp next_p = oCdr(cp);
    T_sp bracketed = af_backquote_bracket(oCar(cp)); // eval::funcall(_sym_backquote_bracket,oCar(cp));
    Cons_sp next_q = Cons_O::create(bracketed, q);
    p = next_p;
    q = next_q;
    step++;
  }
  {
    nr = cl_nreconc(q, Cons_O::create(Cons_O::createList(_sym_STARbq_quoteSTAR, p)));
    //	    nr = eval::funcall(cl::_sym_nreconc,q,Cons_O::create(Cons_O::createList(_sym_STARbq_quoteSTAR, p)));
  }
  result = Cons_O::create(_sym_STARbq_appendSTAR, nr);

DONE:
  return result;
}

#define ARGS_af_backquote_bracket "(x)"
#define DECL_af_backquote_bracket ""
#define DOCS_af_backquote_bracket "bracket"
T_sp af_backquote_bracket(T_sp x) {
  _G();
#if 0
	// Call out to LISP for this one
	SYMBOL_SC_(CorePkg,bq_bracket);
	return(Values(eval::funcall(_sym_bq_bracket,x)));
#else
  if (cl_atom(x)) {
    //	    return Cons_O::createList(_sym_STARbq_listSTAR,eval::funcall(_sym_backquote_process,x));
    return Cons_O::createList(_sym_STARbq_listSTAR, af_backquote_process(x));
  }
  List_sp cx = x;
  if (oCar(cx) == _sym_unquote) {
    return (Values(Cons_O::createList(_sym_STARbq_listSTAR, oCadr(cx))));
  } else if (oCar(cx) == _sym_unquote_splice) {
    return (Values(oCadr(cx)));
  } else if (oCar(cx) == _sym_unquote_nsplice) {
    return (Values(Cons_O::createList(_sym_STARbq_clobberableSTAR, oCadr(cx))));
  }
  //	return Cons_O::createList(_sym_STARbq_listSTAR,eval::funcall(_sym_backquote_process,x));
  return Cons_O::createList(_sym_STARbq_listSTAR, af_backquote_process(x));

#endif
};

#define ARGS_af_backquote_splicing_frob "(x)"
#define DECL_af_backquote_splicing_frob ""
#define DOCS_af_backquote_splicing_frob "backquote_splicing_frob is true if a form that when read looked like ,@foo or ,.foo"
bool af_backquote_splicing_frob(T_sp x) {
  _G();
  if (cl_consp(x)) {
    List_sp cx = x;
    T_sp head = oCar(cx);
    return ((((head == _sym_unquote_splice) || (head == _sym_unquote_nsplice))));
  }
  return ((false));
};

#define ARGS_af_backquote_frob "(x)"
#define DECL_af_backquote_frob ""
#define DOCS_af_backquote_frob "backquote_frob is true if a form that when read looked like ,foo or ,@foo or ,.foo"
bool af_backquote_frob(T_sp x) {
  _G();
  if (cl_consp(x)) {
    List_sp cx = x;
    T_sp head = oCar(cx);
    return ((((head == _sym_unquote) || (head == _sym_unquote_splice) || (head == _sym_unquote_nsplice))));
  }
  return ((false));
};

#define ARGS_af_backquote_maptree "(op x)"
#define DECL_af_backquote_maptree ""
#define DOCS_af_backquote_maptree "backquote_maptree"
SYMBOL_SC_(CorePkg, backquote_maptree);
T_sp af_backquote_maptree(Function_sp op, T_sp x) {
  _G();
  if (cl_atom(x)) {
    T_sp result = eval::funcall(op, x);
    return result;
  }
  List_sp cx = x;
  T_sp a = eval::funcall(op, oCar(cx));
  T_sp d = af_backquote_maptree(op, oCdr(cx));
  if (cl_eql(a, oCar(cx)) && cl_eql(d, oCdr(cx))) {
    return x;
  }
  return (Cons_O::create(a, d));
};

#if 0 //   Function pointer version - this should be faster than the version above
    T_sp backquote_maptree(T_sp (*op)(T_sp), T_sp x)
    {_G();
	if ( cl_atom(x) )
	{
	    T_sp result = op(x);
	    return((result));
	}
	List_sp cx = x;
	T_sp a = op(x);
	T_sp d = backquote_maptree(op,cCdr(cx));
	if ( cl_eql(a,oCar(cx)) && cl_eql(d,cCdr(cx)))
	{
	    return((x));
	}
	return((Cons_O::create(a,d)));
    };
#endif

#define ARGS_af_backquote_simplify "(x)"
#define DECL_af_backquote_simplify ""
#define DOCS_af_backquote_simplify "temp_backquote_simplify"
T_sp af_backquote_simplify(T_sp x) {
  _G();
  if (cl_atom(x))
    return (Values(x));
  List_sp cx = x;
  if (oCar(cx) == _sym_STARbq_quoteSTAR) {
    // do nothing x = x;
  } else {
    SYMBOL_SC_(CorePkg, backquote_simplify);
    x = af_backquote_maptree(_sym_backquote_simplify->symbolFunction(), x);
    cx = x;
  }
  if (oCar(cx) != _sym_STARbq_appendSTAR) {
    return (x);
  }
  T_sp s = af_backquote_simplify_args(x);
  return (s);
};

#define ARGS_af_backquote_simplify_args "(x)"
#define DECL_af_backquote_simplify_args ""
#define DOCS_af_backquote_simplify_args "backquote_simplify_args"
T_sp af_backquote_simplify_args(T_sp x) {
  _G();
  List_sp cx = x;
  List_sp args = gc::As<Cons_sp>(oCdr(cx))->reverse();
  T_sp result = _Nil<T_O>();
  while (args.notnilp()) {
    // Advance args
    args = oCdr(args);
    // Advance result
    if (cl_atom(oCar(args))) {
      result = af_backquote_attach_append(_sym_STARbq_appendSTAR, oCar(args), result);
    } else if ((oCaar(args) == _sym_STARbq_listSTAR) &&
               af_notany(_sym_backquote_splicing_frob,
                         Cons_O::create(oCdar(args))).isTrue()) {
      result = af_backquote_attach_conses(oCdar(args), result);
    } else if ((oCaar(args) == _sym_STARbq_listSTARSTAR) &&
               af_notany(_sym_backquote_splicing_frob,
                         Cons_O::create(oCdar(args))).isTrue()) {
      List_sp rev1 = cl_reverse(oCdar(args));
      List_sp rev2 = cl_reverse(oCdr(rev1));
      List_sp car_last_car_args = oCar(cl_last(oCar(args)));
      result = af_backquote_attach_conses(rev2,
                                          af_backquote_attach_append(_sym_STARbq_appendSTAR,
                                                                     car_last_car_args,
                                                                     result));
    } else if ((oCaar(args) == _sym_STARbq_quoteSTAR) &&
               (cl_consp(oCadar(args))) &&
               (!af_backquote_frob(oCadar(args))) &&
               (oCddar(args).nilp())) {
      Cons_sp tl = Cons_O::createList(_sym_STARbq_quoteSTAR, oCaadar(args));
      Cons_sp tc = Cons_O::create(tl);
      result = af_backquote_attach_conses(tc, result);
    } else if (oCaar(args) == _sym_STARbq_clobberableSTAR) {
      result = af_backquote_attach_append(_sym_STARbq_nconcSTAR, oCadar(args), result);
    } else {
      result = af_backquote_attach_append(_sym_STARbq_appendSTAR, oCar(args), result);
    }
  }
  return (result);
};

#define ARGS_af_backquote_null_or_quoted "(x)"
#define DECL_af_backquote_null_or_quoted ""
#define DOCS_af_backquote_null_or_quoted "backquote_null_or_quoted"
T_sp af_backquote_null_or_quoted(T_sp x) {
  _G();
  if (x.nilp())
    return (Values(_lisp->_true()));
  if (cl_consp(x)) {
    List_sp cx = x;
    if (oCar(cx) == _sym_STARbq_quoteSTAR)
      return (_lisp->_true());
  }
  return _Nil<T_O>();
};

#define ARGS_af_backquote_attach_append "(op item result)"
#define DECL_af_backquote_attach_append ""
#define DOCS_af_backquote_attach_append "backquote_attach_append"
T_sp af_backquote_attach_append(T_sp op, T_sp item, T_sp result) {
  _G();
  if (af_backquote_null_or_quoted(item).isTrue() && af_backquote_null_or_quoted(result).isTrue()) {
    List_sp tl = Cons_O::createList(oCadr(item), oCadr(result));
    return Cons_O::createList(_sym_STARbq_quoteSTAR,
                              af_backquote_append(tl));
  } else if (cl_equal(result, _sym_STARbq_quote_nilSTAR->symbolValue())) {
    if (af_backquote_splicing_frob(item)) {
      return Cons_O::createList(op, item);
    } else
      return item;
  } else if (cl_consp(result) && oCar(result) == op) {
    return Cons_O::create(oCar(result), Cons_O::create(item, oCdr(result)));
  }
  return Cons_O::createList(op, item, result);
}

#define ARGS_af_backquote_attach_conses "(items result)"
#define DECL_af_backquote_attach_conses ""
#define DOCS_af_backquote_attach_conses "backquote_attach_conses"
List_sp af_backquote_attach_conses(T_sp items, T_sp result) {
  _G();
  if (af_every(_sym_backquote_null_or_quoted, Cons_O::create(items)).isTrue() && af_backquote_null_or_quoted(result).isTrue()) {
    Cons_sp ti = Cons_O::create(items);
    Cons_sp tl = Cons_O::createList(cl_mapcar(cl::_sym_cadr, ti), oCadr(result));
    return Cons_O::createList(_sym_STARbq_quoteSTAR, af_backquote_append(tl));
  } else if (cl_equal(result, _sym_STARbq_quote_nilSTAR->symbolValue())) {
    return Cons_O::create(_sym_STARbq_listSTAR, items);
  } else if (cl_consp(result) && ((oCar(result) == _sym_STARbq_listSTAR) || (oCar(result) == _sym_STARbq_listSTARSTAR))) {
    List_sp tl = Cons_O::createList(items, oCdr(result));
    return Cons_O::create(oCar(result), af_backquote_append(tl));
  }
  Cons_sp ta = Cons_O::create(result);
  Cons_sp tr = Cons_O::createList(items, ta);
  return Cons_O::create(_sym_STARbq_listSTARSTAR, af_backquote_append(tr));
}

#define ARGS_af_backquote_remove_tokens "(x)"
#define DECL_af_backquote_remove_tokens ""
#define DOCS_af_backquote_remove_tokens "backquote_remove_tokens"
T_sp af_backquote_remove_tokens(T_sp x) {
  _G();
  if (x == _sym_STARbq_listSTAR)
    return cl::_sym_list;
  if (x == _sym_STARbq_appendSTAR)
    return _sym_backquote_append;
  if (x == _sym_STARbq_nconcSTAR)
    return cl::_sym_nconc;
  if (x == _sym_STARbq_listSTARSTAR)
    return cl::_sym_listSTAR;
  if (x == _sym_STARbq_quoteSTAR)
    return cl::_sym_quote;
  if (cl_atom(x))
    return x;
  List_sp cx = x;
  if (oCar(cx) == _sym_STARbq_clobberableSTAR) {
    return ((af_backquote_remove_tokens(oCadr(cx))));
  }
  if ((oCar(cx) == _sym_STARbq_listSTARSTAR) && cl_consp(oCddr(cx)) && oCdddr(cx).nilp()) {
    return Cons_O::create(cl::_sym_cons,
                          af_backquote_maptree(_sym_backquote_remove_tokens->symbolFunction(), oCdr(cx)));
  }
  T_sp mapped = af_backquote_maptree(_sym_backquote_remove_tokens->symbolFunction(), x);
  return mapped;
}

void initialize_backquote(Lisp_sp lisp) {
  _G();
  SYMBOL_SC_(CorePkg, backquote_completely_process);
  Defun(backquote_completely_process);
  SYMBOL_SC_(CorePkg, backquote_process);
  Defun(backquote_process);
  SYMBOL_SC_(CorePkg, backquote_bracket);
  Defun(backquote_bracket);
  Defun(backquote_maptree);
  Defun(backquote_simplify);
  Defun(backquote_simplify_args);
  SYMBOL_SC_(CorePkg, backquote_null_or_quoted);
  Defun(backquote_null_or_quoted);
  SYMBOL_SC_(CorePkg, backquote_attach_append);
  Defun(backquote_attach_append);
  SYMBOL_SC_(CorePkg, backquote_attach_conses);
  Defun(backquote_attach_conses);
  SYMBOL_SC_(CorePkg, backquote_remove_tokens);
  Defun(backquote_remove_tokens);
  SYMBOL_SC_(CorePkg, backquote_frob);
  Defun(backquote_frob);
  SYMBOL_SC_(CorePkg, backquote_splicing_frob);
  Defun(backquote_splicing_frob);

  SYMBOL_SC_(CorePkg, backquote_append);
  Defun(backquote_append);

  SYMBOL_SC_(CorePkg, quasiquote);
  defmacro(CorePkg, "quasiquote", af_quasiquote, ARGS_af_quasiquote, DECL_af_quasiquote, DOCS_af_quasiquote, __FILE__, __LINE__, false);

  SYMBOL_SC_(CorePkg, STARbq_simplifySTAR);
  _sym_STARbq_simplifySTAR->setf_symbolValue(_lisp->_true());

  _sym_STARbq_listSTAR->defconstant(_sym_STARbq_listSTAR);
  _sym_STARbq_appendSTAR->defconstant(_sym_STARbq_appendSTAR);
  _sym_STARbq_listSTARSTAR->defconstant(_sym_STARbq_listSTARSTAR);
  _sym_STARbq_nconcSTAR->defconstant(_sym_STARbq_nconcSTAR);
  _sym_STARbq_clobberableSTAR->defconstant(_sym_STARbq_clobberableSTAR);
  _sym_STARbq_quoteSTAR->defconstant(_sym_STARbq_quoteSTAR);
  _sym_STARbq_quote_nilSTAR->defconstant(Cons_O::createList(_sym_STARbq_quoteSTAR, _Nil<T_O>()));
}
};
