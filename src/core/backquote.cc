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

#include <clasp/core/foundation.h>
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

#define ARGS_macro_core__quasiquote "(whole env)"
#define DECL_macro_core__quasiquote ""
#define DOCS_macro_core__quasiquote ""
T_mv macro_core__quasiquote(List_sp whole, T_sp env) {
  ASSERT(cl__length(whole) == 2);
  T_sp form = oCadr(whole);
  return core__backquote_completely_process(form);
};

/*!
  Equivalent to Common Lisps append function
  (append a b c)
  It recreates the list structures of the first arguments a and b and strings
  them together into one list and then points the cdr of the last element of this new list
  to c.
*/
CL_LAMBDA(core:&va-rest lists);
CL_DECLARE();
CL_DOCSTRING("append as in clhs");
CL_DEFUN T_sp core__backquote_append(VaList_sp lists) {
  ql::list list; // (lists);
  LOG(BF("Carrying out append with arguments: %s") % _rep_(lists));
  unlikely_if (lists->total_nargs()==0) {
    SIMPLE_ERROR(BF("backquote-append was called with zero arguments"));
  }
  if (lists->total_nargs()>1) {
    size_t nargs = lists->total_nargs()-1;
    for ( size_t i(0); i<nargs; ++i ) {
      T_sp head = lists->next_arg();
      List_sp oneList = head;
      for (auto element : oneList) {
        list << CONS_CAR(element);
      }
    }
  }
  /* Now append the last argument by setting the new lists last element cdr
	   to the last argument of append */
  List_sp result = list.dot(lists->next_arg()).cons();
  return result;
}


CL_LAMBDA(&rest lists);
CL_DECLARE();
CL_DOCSTRING("append as in clhs");
CL_DEFUN T_sp core__backquote_append_list(List_sp lists) {
  ql::list list; // (lists);
  LOG(BF("Carrying out append with arguments: %s") % _rep_(lists));
  List_sp appendArg = lists;
  if (appendArg.consp()) {
    for (; CONS_CDR(appendArg).consp(); appendArg = CONS_CDR(appendArg)) {
      T_sp head = CONS_CAR(appendArg);
      ASSERT(head.consp() || head.nilp());
      List_sp oneList = head;
      for (auto element : oneList) {
        list << CONS_CAR(element);
      }
    }
  }
  /* Now append the last argument by setting the new lists last element cdr
	   to the last argument of append */
  List_sp result = list.dot(oCar(appendArg)).cons();
  return result;
}

CL_LAMBDA(x);
CL_DECLARE();
CL_DOCSTRING("backquote_completely_process");
CL_DEFUN T_mv core__backquote_completely_process(T_sp x) {
  T_sp raw_result = core__backquote_process(x);
  if (_sym_STARbq_simplifySTAR->symbolValue().isTrue()) {
    T_sp process_result = core__backquote_simplify(raw_result);
    T_sp result = core__backquote_remove_tokens(process_result);
    (void)result;
  }
  T_sp result = core__backquote_remove_tokens(raw_result);
  return Values(result);
}

CL_LAMBDA(ox);
CL_DECLARE();
CL_DOCSTRING("bq_process");
CL_DEFUN T_sp core__backquote_process(T_sp ox) {
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
  if (cl__atom(ox)) {
    result = Cons_O::createList(_sym_STARbq_quoteSTAR, ox);
    goto DONE;
  }
  x = ox;
  ax = oCar(x);
  if (ax == _sym_backquote) {
    T_sp passone = core__backquote_completely_process(oCadr(x)); // eval::funcall(_sym_backquote_completely_process,oCadr(x));
    result = core__backquote_process(passone);                   // eval::funcall(_sym_backquote_process,passone);
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
  while (!cl__atom(p)) {
    cp = p;
    T_sp head = oCar(cp);
    if (oCar(cp) == _sym_unquote) {
      ASSERT(oCddr(cp).nilp());
      nreconc = cl__nreconc(q, Cons_O::create(oCadr(cp)));
      result = Cons_O::create(_sym_STARbq_appendSTAR, nreconc);
      goto DONE;
    } else if (head == _sym_unquote_splice) {
      SIMPLE_ERROR(BF("Dotted ,@%s") % _rep_(p));
    } if ( head == _sym_unquote_nsplice) {
      SIMPLE_ERROR(BF("Dotted ,.%s") % _rep_(p));
    }

    // Advance
    T_sp next_p = oCdr(cp);
    T_sp bracketed = core__backquote_bracket(oCar(cp)); // eval::funcall(_sym_backquote_bracket,oCar(cp));
    Cons_sp next_q = Cons_O::create(bracketed, q);
    p = next_p;
    q = next_q;
    step++;
  }
  nr = cl__nreconc(q, Cons_O::create(Cons_O::createList(_sym_STARbq_quoteSTAR, p)));
  result = Cons_O::create(_sym_STARbq_appendSTAR, nr);
DONE:
  return result;
}

CL_DOCSTRING("bracket");
CL_DEFUN T_sp core__backquote_bracket(T_sp x) {
  if (!x.consp()) {
    //	    return Cons_O::createList(_sym_STARbq_listSTAR,eval::funcall(_sym_backquote_process,x));
    return Cons_O::createList(_sym_STARbq_listSTAR, core__backquote_process(x));
  }
  Cons_sp cx((gctools::Tagged)x.raw_());
  T_sp head = CONS_CAR(x);
  if (head == _sym_unquote) {
    return (Values(Cons_O::createList(_sym_STARbq_listSTAR, oCadr(cx))));
  } else if (head == _sym_unquote_splice) {
    return (Values(oCadr(cx)));
  } else if (head == _sym_unquote_nsplice) {
    return (Values(Cons_O::createList(_sym_STARbq_clobberableSTAR, oCadr(cx))));
  }
  //	return Cons_O::createList(_sym_STARbq_listSTAR,eval::funcall(_sym_backquote_process,x));
  return Cons_O::createList(_sym_STARbq_listSTAR, core__backquote_process(x));
};

CL_LAMBDA(x);
CL_DECLARE();
CL_DOCSTRING("backquote_splicing_frob is true if a form that when read looked like ,@foo or ,.foo");
CL_DEFUN bool core__backquote_splicing_frob(T_sp x) {
  if (x.consp()) {
    Cons_sp cx((gctools::Tagged)x.raw_());
    T_sp head = CONS_CAR(cx);
    return (head == _sym_unquote_splice) || (head == _sym_unquote_nsplice);
  }
  return false;
};

CL_LAMBDA(x);
CL_DECLARE();
CL_DOCSTRING("backquote_frob is true if a form that when read looked like ,foo or ,@foo or ,.foo");
CL_DEFUN bool core__backquote_frob(T_sp x) {
  if (x.consp()) {
    Cons_sp cx((gctools::Tagged)x.raw_());
    T_sp head = CONS_CAR(cx);
    return ((((head == _sym_unquote) || (head == _sym_unquote_splice) || (head == _sym_unquote_nsplice))));
  }
  return false;
};

SYMBOL_SC_(CorePkg, backquote_maptree);

CL_LAMBDA(op x);
CL_DECLARE();
CL_DOCSTRING("backquote_maptree");
CL_DEFUN T_sp core__backquote_maptree(Function_sp op, T_sp x) {
  if (!x.consp()) {
    T_sp result = eval::funcall(op, x);
    return result;
  }
  Cons_sp cx((gctools::Tagged)x.raw_());
  T_sp a = eval::funcall(op, CONS_CAR(cx));
  T_sp d = core__backquote_maptree(op, CONS_CDR(cx));
  if (cl__eql(a, oCar(cx)) && cl__eql(d, CONS_CDR(cx))) return x;
  return (Cons_O::create(a, d));
};


CL_LAMBDA(x);
CL_DECLARE();
CL_DOCSTRING("temp_backquote_simplify");
CL_DEFUN T_sp core__backquote_simplify(T_sp x) {
  if (!x.consp()) return x;
  Cons_sp cx((gctools::Tagged)x.raw_());
  if (CONS_CAR(cx) == _sym_STARbq_quoteSTAR) {
    // do nothing x = x;
  } else {
    SYMBOL_SC_(CorePkg, backquote_simplify);
    x = core__backquote_maptree(_sym_backquote_simplify->symbolFunction(), x);
    ASSERT(cx.consp());
    cx = gc::As_unsafe<Cons_sp>(x);
  }
  if (CONS_CAR(cx) != _sym_STARbq_appendSTAR) {
    return (x);
  }
  T_sp s = core__backquote_simplify_args(x);
  return (s);
};

CL_LAMBDA(x);
CL_DECLARE();
CL_DOCSTRING("backquote_simplify_args");
CL_DEFUN T_sp core__backquote_simplify_args(T_sp x) {
  ASSERT(x.consp());
  Cons_sp cx((gctools::Tagged)x.raw_());
  List_sp args = gc::As<Cons_sp>(CONS_CDR(cx))->reverse();
  T_sp result = _Nil<T_O>();
  while (args.consp()) {
    // Advance args
    args = CONS_CDR(args);
    // Advance result
    T_sp head = oCar(args);
    if (!head.consp()) {
      result = core__backquote_attach_append(_sym_STARbq_appendSTAR, head, result);
    } else {
      T_sp caar = CONS_CAR(head);
      if (caar == _sym_STARbq_listSTAR &&
          core__notany_list(_sym_backquote_splicing_frob, Cons_O::create(oCdar(args))).isTrue()) {
        result = core__backquote_attach_conses(oCdar(args), result);
      } else if ((caar == _sym_STARbq_listSTARSTAR) &&
                 core__notany_list(_sym_backquote_splicing_frob, Cons_O::create(oCdar(args))).isTrue()) {
        List_sp rev1 = cl__reverse(oCdar(args));
        List_sp rev2 = cl__reverse(oCdr(rev1));
        List_sp car_last_car_args = oCar(cl__last(head));
        result = core__backquote_attach_conses(rev2,
                                               core__backquote_attach_append(_sym_STARbq_appendSTAR,
                                                                             car_last_car_args,
                                                                             result));
      } else if ((caar == _sym_STARbq_quoteSTAR) &&
                 (oCadar(args).consp()) &&
                 (!core__backquote_frob(oCadar(args))) &&
                 (oCddar(args).nilp())) {
        Cons_sp tl = Cons_O::createList(_sym_STARbq_quoteSTAR, oCaadar(args));
        Cons_sp tc = Cons_O::create(tl);
        result = core__backquote_attach_conses(tc, result);
      } else if (caar == _sym_STARbq_clobberableSTAR) {
        result = core__backquote_attach_append(_sym_STARbq_nconcSTAR, oCadar(args), result);
      } else {
        result = core__backquote_attach_append(_sym_STARbq_appendSTAR, head, result);
      }
    }
  }
  return result;
};

CL_LAMBDA(x);
CL_DECLARE();
CL_DOCSTRING("backquote_null_or_quoted");
CL_DEFUN T_sp core__backquote_null_or_quoted(T_sp x) {
  if (x.nilp())
    return (Values(_lisp->_true()));
  if ((x).consp()) {
    Cons_sp cx((gctools::Tagged)x.raw_());
    if (CONS_CAR(cx) == _sym_STARbq_quoteSTAR)
      return (_lisp->_true());
  }
  return _Nil<T_O>();
};

CL_LAMBDA(op item result);
CL_DECLARE();
CL_DOCSTRING("backquote_attach_append");
CL_DEFUN T_sp core__backquote_attach_append(T_sp op, T_sp item, T_sp result) {
  if (core__backquote_null_or_quoted(item).isTrue() && core__backquote_null_or_quoted(result).isTrue()) {
    List_sp tl = Cons_O::createList(oCadr(item), oCadr(result));
    return Cons_O::createList(_sym_STARbq_quoteSTAR, core__backquote_append_list(tl));
  } else if (cl__equal(result, _sym_STARbq_quote_nilSTAR->symbolValue())) {
    if (core__backquote_splicing_frob(item)) {
      return Cons_O::createList(op, item);
    } else
      return item;
  } else if ((result).consp() && CONS_CAR(result) == op) {
    return Cons_O::create(CONS_CAR(result), Cons_O::create(item, CONS_CDR(result)));
  }
  return Cons_O::createList(op, item, result);
}


CL_LAMBDA(items result);
CL_DECLARE();
CL_DOCSTRING("backquote_attach_conses");
CL_DEFUN List_sp core__backquote_attach_conses(T_sp items, T_sp result) {
  if (core__every_list(_sym_backquote_null_or_quoted, Cons_O::create(items)).isTrue() && core__backquote_null_or_quoted(result).isTrue()) {
    Cons_sp ti = Cons_O::create(items);
    Cons_sp tl = Cons_O::createList(cl__mapcar(cl::_sym_cadr, ti), oCadr(result));
    return Cons_O::createList(_sym_STARbq_quoteSTAR, core__backquote_append_list(tl));
  } else if (cl__equal(result, _sym_STARbq_quote_nilSTAR->symbolValue())) {
    return Cons_O::create(_sym_STARbq_listSTAR, items);
  } else if ((result).consp() && ((oCar(result) == _sym_STARbq_listSTAR) || (oCar(result) == _sym_STARbq_listSTARSTAR))) {
    List_sp tl = Cons_O::createList(items, oCdr(result));
    return Cons_O::create(oCar(result), core__backquote_append_list(tl));
  }
  Cons_sp ta = Cons_O::create(result);
  Cons_sp tr = Cons_O::createList(items, ta);
  return Cons_O::create(_sym_STARbq_listSTARSTAR, core__backquote_append_list(tr));
}

CL_LAMBDA(x);
CL_DECLARE();
CL_DOCSTRING("backquote_remove_tokens");
CL_DEFUN T_sp core__backquote_remove_tokens(T_sp x) {
  if (x == _sym_STARbq_listSTAR) return cl::_sym_list;
  if (x == _sym_STARbq_appendSTAR) return _sym_backquote_append;
  if (x == _sym_STARbq_nconcSTAR) return cl::_sym_nconc;
  if (x == _sym_STARbq_listSTARSTAR) return cl::_sym_listSTAR;
  if (x == _sym_STARbq_quoteSTAR) return cl::_sym_quote;
  if (!x.consp()) return x;
  Cons_sp cx((gctools::Tagged)x.raw_());
  T_sp head = CONS_CAR(cx);
  if (head == _sym_STARbq_clobberableSTAR) {
    return core__backquote_remove_tokens(oCadr(cx));
  }
  if ((head == _sym_STARbq_listSTARSTAR) && (oCddr(cx)).consp() && oCdddr(cx).nilp()) {
    return Cons_O::create(cl::_sym_cons,
                          core__backquote_maptree(_sym_backquote_remove_tokens->symbolFunction(), CONS_CDR(cx)));
  }
  T_sp mapped = core__backquote_maptree(_sym_backquote_remove_tokens->symbolFunction(), x);
  return mapped;
}
  SYMBOL_SC_(CorePkg, backquote_completely_process);
  SYMBOL_SC_(CorePkg, backquote_process);
  SYMBOL_SC_(CorePkg, backquote_bracket);
  SYMBOL_SC_(CorePkg, backquote_null_or_quoted);
  SYMBOL_SC_(CorePkg, backquote_attach_append);
  SYMBOL_SC_(CorePkg, backquote_attach_conses);
  SYMBOL_SC_(CorePkg, backquote_remove_tokens);
  SYMBOL_SC_(CorePkg, backquote_frob);
  SYMBOL_SC_(CorePkg, backquote_splicing_frob);
  SYMBOL_SC_(CorePkg, backquote_append);
  SYMBOL_SC_(CorePkg, quasiquote);
  SYMBOL_SC_(CorePkg, STARbq_simplifySTAR);

void initialize_backquote() {
  defmacro(CorePkg, "quasiquote", macro_core__quasiquote, ARGS_macro_core__quasiquote, DECL_macro_core__quasiquote, DOCS_macro_core__quasiquote, __FILE__, __LINE__);

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
