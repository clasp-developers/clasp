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

#include <clasp/core/foundation.h>
#include <clasp/core/common.h>
#include <clasp/core/ql.h>
#include <clasp/core/symbolTable.h>
#include <clasp/core/multipleValues.h>
#include <clasp/core/backquote.h>
#include <clasp/core/evaluator.h>
#include <clasp/core/lispList.h>
#include <clasp/core/primitives.h>
#include <clasp/core/sequence.h>
#include <clasp/core/wrappers.h>
#include <clasp/core/array.h>

SYMBOL_SC_(CorePkg, STARbq_listSTAR);
SYMBOL_SC_(CorePkg, STARbq_appendSTAR);
SYMBOL_SC_(CorePkg, STARbq_listSTARSTAR);
SYMBOL_SC_(CorePkg, STARbq_nconcSTAR);
SYMBOL_SC_(CorePkg, STARbq_vectorizeSTAR);
SYMBOL_SC_(CorePkg, STARbq_clobberableSTAR);
SYMBOL_SC_(CorePkg, STARbq_quoteSTAR);
SYMBOL_SC_(CorePkg, STARbq_quote_nilSTAR);
SYMBOL_SC_(CorePkg, STARbq_simplifySTAR);

SYMBOL_SC_(CorePkg, bq_simplify);
SYMBOL_SC_(CorePkg, bq_remove_tokens);

namespace core {

/*!
  Equivalent to Common Lisps append function
  (append a b c)
  It recreates the list structures of the first arguments a and b and strings
  them together into one list and then points the cdr of the last element of this new list
  to c.
*/
CL_LAMBDA(core:&va-rest lists);
CL_DOCSTRING(R"dx(append as in clhs)dx");
DOCGROUP(clasp);
CL_DEFUN T_sp core__backquote_append(Vaslist_sp lists) {
  ql::list list; // (lists);
  LOG("Carrying out append with arguments: {}", _rep_(lists));
  unlikely_if(lists->nargs_zero()) { SIMPLE_ERROR("backquote-append was called with zero arguments"); }
  size_t nargs = lists->nargs();
  size_t fnargs = nargs - 1;
  if (nargs > 1) {
    for (size_t i(0); i < fnargs; ++i) {
      T_sp head = lists->next_arg_indexed(i);
      List_sp oneList = head;
      for (auto element : oneList) {
        list << CONS_CAR(element);
      }
    }
  }
  /* Now append the last argument by setting the new lists last element cdr
           to the last argument of append */
  T_sp lastArg = lists->next_arg_indexed(fnargs);
  return list.dot(lastArg).cons();
}

CL_LAMBDA(&rest lists);
CL_DECLARE();
CL_DOCSTRING(R"dx(append as in clhs)dx");
DOCGROUP(clasp);
CL_DEFUN T_sp core__backquote_append_list(List_sp lists) {
  ql::list list; // (lists);
  LOG("Carrying out append with arguments: {}", _rep_(lists));
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

CL_LAMBDA(op x);
CL_DECLARE();
CL_DOCSTRING(R"dx(backquote_maptree)dx");
DOCGROUP(clasp);
CL_DEFUN T_sp core__backquote_maptree(Function_sp op, T_sp x) {
  if (!x.consp()) {
    T_sp result = eval::funcall(op, x);
    return result;
  }
  Cons_sp cx = x.as_unsafe<Cons_O>();
  T_sp a = eval::funcall(op, cx->car());
  T_sp d = core__backquote_maptree(op, cx->cdr());
  if (cl__eql(a, cx->car()) && cl__eql(d, cx->cdr()))
    return x;
  return Cons_O::create(a, d);
};

CL_LAMBDA(x);
CL_DECLARE();
CL_DOCSTRING(R"dx(backquote_remove_tokens)dx");
DOCGROUP(clasp);
CL_DEFUN T_sp core__backquote_remove_tokens(T_sp x) {
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
  if (!x.consp())
    return x;
  Cons_sp cx = x.as_unsafe<Cons_O>();
  T_sp head = cx->car();
  if (head == _sym_STARbq_clobberableSTAR) {
    return core__backquote_remove_tokens(oCadr(cx));
  }
  if ((head == _sym_STARbq_listSTARSTAR) && cx->cdr().consp() && oCddr(cx).nilp()) {
    // (list* x) => x
    return core__backquote_maptree(_sym_backquote_remove_tokens->symbolFunction(), oCadr(cx));
  } else if ((head == _sym_STARbq_vectorizeSTAR) && oCdr(cx).consp() && oCddr(cx).nilp()) {
    // (*bq-vectorize* x) => (apply #'vector x)
    T_sp inner = core__backquote_maptree(_sym_backquote_remove_tokens->symbolFunction(), oCadr(cx));
    T_sp fvector = Cons_O::createList(cl::_sym_Function_O, cl::_sym_vector);
    return Cons_O::createList(cl::_sym_apply, fvector, inner);
  }
  T_sp mapped = core__backquote_maptree(_sym_backquote_remove_tokens->symbolFunction(), x);
  return mapped;
}

CL_LAMBDA(x);
CL_DECLARE();
CL_DOCSTRING(R"dx(backquote_null_or_quoted)dx");
DOCGROUP(clasp);
CL_DEFUN T_sp core__backquote_null_or_quoted(T_sp x) {
  if (x.nilp())
    return (Values(_lisp->_true()));
  if ((x).consp()) {
    Cons_sp cx = x.as_unsafe<Cons_O>();
    if (cx->car() == _sym_STARbq_quoteSTAR)
      return (_lisp->_true());
  }
  return nil<T_O>();
};

CL_LAMBDA(items result);
CL_DECLARE();
CL_DOCSTRING(R"dx(backquote_attach_conses)dx");
DOCGROUP(clasp);
CL_DEFUN List_sp core__backquote_attach_conses(T_sp items, T_sp result) {
  if (core__every_list(_sym_backquote_null_or_quoted, Cons_O::create(items, nil<T_O>())).isTrue() &&
      core__backquote_null_or_quoted(result).isTrue()) {
    Cons_sp ti = Cons_O::create(items, nil<T_O>());
    Cons_sp tl = Cons_O::createList(cl__mapcar(cl::_sym_cadr, ti), oCadr(result));
    return Cons_O::createList(_sym_STARbq_quoteSTAR, core__backquote_append_list(tl));
  } else if (cl__equal(result, _sym_STARbq_quote_nilSTAR->symbolValue())) {
    return Cons_O::create(_sym_STARbq_listSTAR, items);
  } else if ((result).consp() && ((oCar(result) == _sym_STARbq_listSTAR) || (oCar(result) == _sym_STARbq_listSTARSTAR))) {
    List_sp tl = Cons_O::createList(items, oCdr(result));
    return Cons_O::create(oCar(result), core__backquote_append_list(tl));
  }
  Cons_sp ta = Cons_O::create(result, nil<T_O>());
  Cons_sp tr = Cons_O::createList(items, ta);
  return Cons_O::create(_sym_STARbq_listSTARSTAR, core__backquote_append_list(tr));
}

CL_LAMBDA(x);
CL_DECLARE();
CL_DOCSTRING(R"dx(backquote_splicing_frob is true if a form that when read looked like ,@foo or ,.foo)dx");
DOCGROUP(clasp);
CL_DEFUN bool core__backquote_splicing_frob(T_sp x) {
  if (x.consp()) {
    Cons_sp cx = x.as_unsafe<Cons_O>();
    T_sp head = cx->car();
    return (head == _sym_unquote_splice) || (head == _sym_unquote_nsplice);
  }
  return false;
};

CL_LAMBDA(x);
CL_DECLARE();
CL_DOCSTRING(R"dx(backquote_frob is true if a form that when read looked like ,foo or ,@foo or ,.foo)dx");
DOCGROUP(clasp);
CL_DEFUN bool core__backquote_frob(T_sp x) {
  if (x.consp()) {
    Cons_sp cx = x.as_unsafe<Cons_O>();
    T_sp head = cx->car();
    return (head == _sym_unquote) || (head == _sym_unquote_splice) || (head == _sym_unquote_nsplice);
  }
  return false;
};

CL_LAMBDA(op item result);
CL_DECLARE();
CL_DOCSTRING(R"dx(backquote_attach_append)dx");
DOCGROUP(clasp);
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

CL_LAMBDA(x);
CL_DECLARE();
CL_DOCSTRING(R"dx(backquote_simplify_args)dx");
DOCGROUP(clasp);
CL_DEFUN T_sp core__backquote_simplify_args(Cons_sp x) {
  List_sp args = x->cdr().as_assert<Cons_O>()->reverse();
  T_sp result = nil<T_O>();
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
          core__notany_list(_sym_backquote_splicing_frob, Cons_O::create(oCdar(args), nil<T_O>())).isTrue()) {
        result = core__backquote_attach_conses(oCdar(args), result);
      } else if ((caar == _sym_STARbq_listSTARSTAR) &&
                 core__notany_list(_sym_backquote_splicing_frob, Cons_O::create(oCdar(args), nil<T_O>())).isTrue()) {
        List_sp rev1 = cl__reverse(oCdar(args));
        List_sp rev2 = cl__reverse(oCdr(rev1));
        List_sp car_last_car_args = oCar(cl__last(head, make_fixnum(1)));
        result =
            core__backquote_attach_conses(rev2, core__backquote_attach_append(_sym_STARbq_appendSTAR, car_last_car_args, result));
      } else if ((caar == _sym_STARbq_quoteSTAR) && (oCadar(args).consp()) && (!core__backquote_frob(oCadar(args))) &&
                 (oCddar(args).nilp())) {
        Cons_sp tl = Cons_O::createList(_sym_STARbq_quoteSTAR, oCaadar(args));
        Cons_sp tc = Cons_O::create(tl, nil<T_O>());
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
CL_DOCSTRING(R"dx(temp_backquote_simplify)dx");
DOCGROUP(clasp);
CL_DEFUN T_sp core__backquote_simplify(T_sp x) {
  if (!x.consp())
    return x;
  Cons_sp cx = x.as_unsafe<Cons_O>();
  if (cx->car() == _sym_STARbq_quoteSTAR) {
    // do nothing x = x;
  } else {
    SYMBOL_SC_(CorePkg, backquote_simplify);
    x = core__backquote_maptree(_sym_backquote_simplify->symbolFunction(), x);
    cx = gc::As_unsafe<Cons_sp>(x);
  }
  if (cx->car() != _sym_STARbq_appendSTAR) {
    return x;
  }
  T_sp s = core__backquote_simplify_args(cx);
  return s;
};

T_sp core__backquote_process(T_sp x);

CL_LAMBDA(x);
CL_DECLARE();
CL_DOCSTRING(R"dx(backquote_completely_process)dx");
DOCGROUP(clasp);
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


CL_DOCSTRING(R"dx(bracket)dx");
DOCGROUP(clasp);
CL_DEFUN T_sp core__backquote_bracket(T_sp x) {
  if (!x.consp()) {
    return Cons_O::createList(_sym_STARbq_listSTAR, core__backquote_process(x));
  }
  Cons_sp cx = x.as_unsafe<Cons_O>();
  T_sp head = cx->car();
  if (head == _sym_unquote) {
    return (Values(Cons_O::createList(_sym_STARbq_listSTAR, oCadr(cx))));
  } else if (head == _sym_unquote_splice) {
    return (Values(oCadr(cx)));
  } else if (head == _sym_unquote_nsplice) {
    return (Values(Cons_O::createList(_sym_STARbq_clobberableSTAR, oCadr(cx))));
  }
  return Cons_O::createList(_sym_STARbq_listSTAR, core__backquote_process(x));
};

CL_LAMBDA(ox);
CL_DECLARE();
CL_DOCSTRING(R"dx(bq_process)dx");
DOCGROUP(clasp);
CL_DEFUN T_sp core__backquote_process(T_sp p) {
  if (p.consp()) {
    T_sp ax = oCar(p);
    if (ax == _sym_quasiquote) {
      return core__backquote_process(core__backquote_completely_process(oCadr(p)));
    } else if (ax == _sym_unquote) {
      return oCadr(p);
    } else if (ax == _sym_unquote_splice) {
      SIMPLE_ERROR(",@{} after `", _rep_(oCadr(p)));
    } else if (ax == _sym_unquote_nsplice) {
      SIMPLE_ERROR(",.{} after `", _rep_(oCadr(p)));
    }
    T_sp q = nil<T_O>();
    int step = 0;
    do {
      T_sp head = oCar(p);
      if (oCar(p) == _sym_unquote) {
        ASSERT(oCddr(p).nilp());
        return Cons_O::create(_sym_STARbq_appendSTAR,
                              cl__nreconc(q, Cons_O::create(oCadr(p), nil<T_O>())));
      } else if (head == _sym_unquote_splice) {
        SIMPLE_ERROR("Dotted ,@{}", _rep_(p));
      }
      if (head == _sym_unquote_nsplice) {
        SIMPLE_ERROR("Dotted ,.{}", _rep_(p));
      }

      // Advance
      T_sp next_p = oCdr(p);
      T_sp bracketed = core__backquote_bracket(oCar(p));
      Cons_sp next_q = Cons_O::create(bracketed, q);
      p = next_p;
      q = next_q;
      step++;
    } while (p.consp());
    return Cons_O::create(_sym_STARbq_appendSTAR,
                          cl__nreconc(q, Cons_O::create(Cons_O::createList(_sym_STARbq_quoteSTAR, p), nil<T_O>())));
  } else if (p.isA<SimpleVector_O>()) {
    ql::list subs;
    for (const auto& e : p.as_unsafe<SimpleVector_O>())
      subs << core__backquote_bracket(e);
    return Cons_O::createList(_sym_STARbq_vectorizeSTAR,
                              Cons_O::create(_sym_STARbq_appendSTAR,
                                             subs.cons()));
  } else { // not a cons or vector
    return Cons_O::createList(_sym_STARbq_quoteSTAR, p);
  }
}

SYMBOL_SC_(CorePkg, backquote_maptree);

#define ARGS_macro_core__quasiquote "(whole env)"
#define DECL_macro_core__quasiquote ""
#define DOCS_macro_core__quasiquote ""
T_mv macro_core__quasiquote(List_sp whole, T_sp env) {
  ASSERT(cl__length(whole) == 2);
  T_sp form = oCadr(whole);
  return core__backquote_completely_process(form);
};

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
  defmacro(CorePkg, "quasiquote", macro_core__quasiquote, ARGS_macro_core__quasiquote, DECL_macro_core__quasiquote,
           DOCS_macro_core__quasiquote, __FILE__, __LINE__);

  _sym_STARbq_simplifySTAR->setf_symbolValue(_lisp->_true());
  _sym_STARbq_listSTAR->defconstant(_sym_STARbq_listSTAR);
  _sym_STARbq_appendSTAR->defconstant(_sym_STARbq_appendSTAR);
  _sym_STARbq_listSTARSTAR->defconstant(_sym_STARbq_listSTARSTAR);
  _sym_STARbq_nconcSTAR->defconstant(_sym_STARbq_nconcSTAR);
  _sym_STARbq_clobberableSTAR->defconstant(_sym_STARbq_clobberableSTAR);
  _sym_STARbq_quoteSTAR->defconstant(_sym_STARbq_quoteSTAR);
  _sym_STARbq_quote_nilSTAR->defconstant(Cons_O::createList(_sym_STARbq_quoteSTAR, nil<T_O>()));
}
}; // namespace core
