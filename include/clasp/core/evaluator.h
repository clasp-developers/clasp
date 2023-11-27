#pragma once
/*
    File: evaluator.h
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

#include <clasp/core/ql.h>
#include <clasp/core/evaluator.fwd.h>
#include <clasp/core/designators.h>

namespace cl {
extern core::Symbol_sp &_sym_findClass;
extern core::Symbol_sp &_sym_undefinedFunction;
}; // namespace cl
namespace kw {
extern core::Symbol_sp &_sym_name;
};

namespace core {
T_mv cl__eval(T_sp form);
T_mv cl__apply(T_sp head, Vaslist_sp args);
T_mv core__apply0(Function_sp func, T_sp args);

T_sp ext__symbol_macro(Symbol_sp sym, T_sp env);

extern bool cl__functionp(T_sp fn);

/*! Evaluate within env.
         See ecl/src/c/compiler.d:eval-with-env */
T_mv core__eval_with_env_default(T_sp form, T_sp env);

Function_sp interpreter_lookup_function_or_error(T_sp functionDesignator, T_sp env);

namespace eval {

extern List_sp evaluateList(List_sp args, T_sp environment);
extern T_mv evaluateListReturnLast(List_sp args, T_sp environment);

extern T_mv sp_progn(List_sp code, T_sp env);
extern T_mv sp_setq(List_sp args, T_sp environment);

bool aclasp_special_operator_p(Symbol_sp symbol);
List_sp core__aclasp_list_of_all_special_operators();

/*! See the CLHS for "apply" - all arguments are in args
  (functionDesignator) can be a Symbol or an Function
*/

inline LCC_RETURN funcall(T_sp fn) {
  Function_sp func = coerce::calledFunctionDesignator(fn);
  return func->entry_0()(func.raw_());
}

inline LCC_RETURN funcall(T_sp fn, T_sp arg0) {
  Function_sp func = coerce::calledFunctionDesignator(fn);
  return func->entry_1()(func.raw_(), arg0.raw_());
}

inline LCC_RETURN funcall(T_sp fn, T_sp arg0, T_sp arg1) {
  Function_sp func = coerce::calledFunctionDesignator(fn);
  return func->entry_2()(func.raw_(), arg0.raw_(), arg1.raw_());
}

inline LCC_RETURN funcall(T_sp fn, T_sp arg0, T_sp arg1, T_sp arg2) {
  Function_sp func = coerce::calledFunctionDesignator(fn);
  return func->entry_3()(func.raw_(), arg0.raw_(), arg1.raw_(), arg2.raw_());
}

inline LCC_RETURN funcall(T_sp fn, T_sp arg0, T_sp arg1, T_sp arg2, T_sp arg3) {
  Function_sp func = coerce::calledFunctionDesignator(fn);
  return func->entry_4()(func.raw_(), arg0.raw_(), arg1.raw_(), arg2.raw_(), arg3.raw_());
}

inline LCC_RETURN funcall(T_sp fn, T_sp arg0, T_sp arg1, T_sp arg2, T_sp arg3, T_sp arg4) {
  Function_sp func = coerce::calledFunctionDesignator(fn);
  return func->entry_5()(func.raw_(), arg0.raw_(), arg1.raw_(), arg2.raw_(), arg3.raw_(), arg4.raw_());
}

template <class... ARGS> inline LCC_RETURN funcall(T_sp fn, ARGS &&...args) {
  Function_sp func = coerce::calledFunctionDesignator(fn);
  size_t nargs = sizeof...(ARGS);
  T_O *aargs[sizeof...(ARGS)] = {args.raw_()...};
  return func->entry()(func.raw_(), nargs, &aargs[0]);
}

}; // namespace eval

namespace eval {
void extract_declares_docstring_code_specials(List_sp inputBody, List_sp &declares, bool expectDocString,
                                              gc::Nilable<String_sp> &documentation, List_sp &code, List_sp &specials);

void parse_lambda_body(List_sp body, List_sp &declares, gc::Nilable<String_sp> &docstring, List_sp &code);
}; // namespace eval
}; // namespace core

namespace core {

T_mv core__apply0(Function_sp func, T_sp lastArg);
T_mv core__apply1(Function_sp func, T_sp lastArg, T_sp arg0);
T_mv core__apply2(Function_sp func, T_sp lastArg, T_sp arg0, T_sp arg1);
T_mv core__apply3(Function_sp func, T_sp lastArg, T_sp arg0, T_sp arg1, T_sp arg2);

}; // namespace core
