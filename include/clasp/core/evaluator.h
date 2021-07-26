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
#ifndef evaluator_H
#define evaluator_H

#include <clasp/core/ql.h>
#include <clasp/core/evaluator.fwd.h>
#include <clasp/core/activationFrame.h>

namespace cl {
extern core::Symbol_sp& _sym_findClass;
extern core::Symbol_sp& _sym_undefinedFunction;
};
namespace kw {
extern core::Symbol_sp& _sym_name;
};

namespace core {
T_mv cl__eval(T_sp form);
T_mv cl__apply(T_sp head, VaList_sp args);
T_mv core__apply0( Function_sp func, T_sp args);

T_sp af_interpreter_lookup_variable(Symbol_sp sym, T_sp env);
T_sp af_interpreter_lookup_function(Symbol_sp sym, T_sp env);
T_sp af_interpreter_lookup_macro(Symbol_sp sym, T_sp env);
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

/*! Evaluate a list of expressions (args) into an ActivationFrame that has
	  enough storage to accept each of the objects that is generated by the list of args */
extern void evaluateIntoActivationFrame(ActivationFrame_sp af, List_sp args, T_sp environment);

/*! See the CLHS for "apply" - all arguments are in args 
  (functionDesignator) can be a Symbol or an Function
*/

/*! I want a variadic template function that does APPLY.  C++ variadic template parameter packs
	  must be the last arguments of a function.   APPLY has as its last arguments argsPLUS.
	  So we move argsPLUS up to be the second argument (after the function designator) and list
	  the variadic arguments following it */
 
template <class... Args>
inline T_mv applyLastArgsPLUSFirst(T_sp fn, List_sp argsPLUS, Args&&... args) {
  Function_sp func;
  if (cl__functionp(fn)) {
    func = gc::As_unsafe<Function_sp>(fn);
  } else {
    func = interpreter_lookup_function_or_error(fn, nil<T_O>());
  }
  int numArgsPassed = sizeof...(Args);
  int numArgsPlus = argsPLUS.consp() ? argsPLUS.unsafe_cons()->proper_list_length() : 0;
  int nargs = numArgsPassed + numArgsPlus;
  T_sp initialContents[sizeof...(Args)] = {args...};
  MAKE_STACK_FRAME( frame, func.raw_(), nargs);
  size_t i(0);
  for ( ; i< sizeof...(Args); ++i ) {
    (*frame)[i] = initialContents[i].raw_();
  }
  for ( auto cur : argsPLUS ) {
    (*frame)[i] = CONS_CAR(cur).raw_();
    ++i;
  }
  Vaslist valist_struct(frame);
  VaList_sp valist(&valist_struct);
  return funcall_consume_valist_<core::T_O>(func.tagged_(),valist);
}













 
inline LCC_RETURN funcall(T_sp fn) {
  /* If the following assertion fails then the funcall functions in this header
     need to be made consistent with lispCallingConvention.h */
  ASSERT(4 == LCC_ARGS_IN_REGISTERS);
  Function_sp func = interpreter_lookup_function_or_error(fn, nil<T_O>());
  ASSERT(gc::IsA<Function_sp>(func));
  return func->entry()(LCC_PASS_ARGS0_ELLIPSIS(func.raw_()));
}

template <class ARG0>
inline LCC_RETURN funcall(T_sp fn, ARG0 arg0) {
  /* If the following assertion fails then the funcall functions in this header
     need to be made consistent with lispCallingConvention.h */
  ASSERT(4 == LCC_ARGS_IN_REGISTERS);
  Function_sp func = interpreter_lookup_function_or_error(fn, nil<T_O>());
  ASSERT(gc::IsA<Function_sp>(func));
  return func->entry()(LCC_PASS_ARGS1_ELLIPSIS(func.raw_(),arg0.raw_()));
}

template <class ARG0, class ARG1>
inline LCC_RETURN funcall(T_sp fn, ARG0 arg0, ARG1 arg1) {
  /* If the following assertion fails then the funcall functions in this header
     need to be made consistent with lispCallingConvention.h */
  ASSERT(4 == LCC_ARGS_IN_REGISTERS);
  Function_sp func = interpreter_lookup_function_or_error(fn, nil<T_O>());
  ASSERT(gc::IsA<Function_sp>(func));
  return func->entry()(LCC_PASS_ARGS2_ELLIPSIS(func.raw_(),arg0.raw_(), arg1.raw_()));
}

template <class ARG0, class ARG1, class ARG2>
inline LCC_RETURN funcall(T_sp fn, ARG0 arg0, ARG1 arg1, ARG2 arg2) {
  /* If the following assertion fails then the funcall functions in this header
     need to be made consistent with lispCallingConvention.h */
  ASSERT(4 == LCC_ARGS_IN_REGISTERS);
  Function_sp func = interpreter_lookup_function_or_error(fn, nil<T_O>());
  ASSERT(gc::IsA<Function_sp>(func));
  return func->entry()(LCC_PASS_ARGS3_ELLIPSIS(func.raw_(),LCC_FROM_SMART_PTR(arg0), LCC_FROM_SMART_PTR(arg1), LCC_FROM_SMART_PTR(arg2)));
}

 template <class ARG0, class ARG1, class ARG2, class ARG3>
   inline LCC_RETURN funcall(T_sp fn, ARG0 arg0, ARG1 arg1, ARG2 arg2, ARG3 arg3) {
  /* If the following assertion fails then the funcall functions in this header
     need to be made consistent with lispCallingConvention.h */
  ASSERT(4 == LCC_ARGS_IN_REGISTERS);
  Function_sp func = interpreter_lookup_function_or_error(fn, nil<T_O>());
  ASSERT(gc::IsA<Function_sp>(func));
  return func->entry()(LCC_PASS_ARGS4_ELLIPSIS(func.raw_(),LCC_FROM_SMART_PTR(arg0), LCC_FROM_SMART_PTR(arg1), LCC_FROM_SMART_PTR(arg2), LCC_FROM_SMART_PTR(arg3)));
}

// Do I need a variadic funcall???
 template <class ARG0, class ARG1, class ARG2, class ARG3, class... ARGS>
  inline LCC_RETURN funcall(T_sp fn, ARG0 arg0, ARG1 arg1, ARG2 arg2, ARG3 arg3, ARGS &&... args) {
  /* If the following assertion fails then the funcall functions in this header
     need to be made consistent with lispCallingConvention.h */
  ASSERT(4 == LCC_ARGS_IN_REGISTERS);
  Function_sp func = interpreter_lookup_function_or_error(fn, nil<T_O>());
  ASSERT(gc::IsA<Function_sp>(func));
  size_t vnargs = sizeof...(ARGS);
  size_t nargs = vnargs + LCC_FIXED_NUM;
  return func->entry()(func.raw_(), nargs, LCC_FROM_SMART_PTR(arg0), LCC_FROM_SMART_PTR(arg1), LCC_FROM_SMART_PTR(arg2), LCC_FROM_SMART_PTR(arg3), std::forward<ARGS>(args).raw_()...);
}

inline LCC_RETURN funcall_function(Function_sp func) {
  /* If the following assertion fails then the funcall functions in this header
     need to be made consistent with lispCallingConvention.h */
  ASSERT(4 == LCC_ARGS_IN_REGISTERS);
  ASSERT(gc::IsA<Function_sp>(func));
  return func->entry()(LCC_PASS_ARGS0_ELLIPSIS(func.raw_()));
}

template <class ARG0>
inline LCC_RETURN funcall_function(Function_sp func, ARG0 arg0) {
  /* If the following assertion fails then the funcall functions in this header
     need to be made consistent with lispCallingConvention.h */
  ASSERT(4 == LCC_ARGS_IN_REGISTERS);
  ASSERT(gc::IsA<Function_sp>(func));
  return func->entry()(LCC_PASS_ARGS1_ELLIPSIS(func.raw_(),arg0.raw_()));
}

template <class ARG0, class ARG1>
inline LCC_RETURN funcall_function(Function_sp func, ARG0 arg0, ARG1 arg1) {
  /* If the following assertion fails then the funcall functions in this header
     need to be made consistent with lispCallingConvention.h */
  ASSERT(4 == LCC_ARGS_IN_REGISTERS);
  ASSERT(gc::IsA<Function_sp>(func));
  return func->entry()(LCC_PASS_ARGS2_ELLIPSIS(func.raw_(),arg0.raw_(), arg1.raw_()));
}

template <class ARG0, class ARG1, class ARG2>
inline LCC_RETURN funcall_function(Function_sp func, ARG0 arg0, ARG1 arg1, ARG2 arg2) {
  /* If the following assertion fails then the funcall functions in this header
     need to be made consistent with lispCallingConvention.h */
  ASSERT(4 == LCC_ARGS_IN_REGISTERS);
  ASSERT(gc::IsA<Function_sp>(func));
  return func->entry()(LCC_PASS_ARGS3_ELLIPSIS(func.raw_(),LCC_FROM_SMART_PTR(arg0), LCC_FROM_SMART_PTR(arg1), LCC_FROM_SMART_PTR(arg2)));
}

 template <class ARG0, class ARG1, class ARG2, class ARG3>
   inline LCC_RETURN funcall_function(Function_sp func, ARG0 arg0, ARG1 arg1, ARG2 arg2, ARG3 arg3) {
  /* If the following assertion fails then the funcall functions in this header
     need to be made consistent with lispCallingConvention.h */
  ASSERT(4 == LCC_ARGS_IN_REGISTERS);
  ASSERT(gc::IsA<Function_sp>(func));
  return func->entry()(LCC_PASS_ARGS4_ELLIPSIS(func.raw_(),LCC_FROM_SMART_PTR(arg0), LCC_FROM_SMART_PTR(arg1), LCC_FROM_SMART_PTR(arg2), LCC_FROM_SMART_PTR(arg3)));
}

// Do I need a variadic funcall???
 template <class ARG0, class ARG1, class ARG2, class ARG3, class... ARGS>
  inline LCC_RETURN funcall_function(Function_sp func, ARG0 arg0, ARG1 arg1, ARG2 arg2, ARG3 arg3, ARGS &&... args) {
  /* If the following assertion fails then the funcall functions in this header
     need to be made consistent with lispCallingConvention.h */
  ASSERT(4 == LCC_ARGS_IN_REGISTERS);
  ASSERT(gc::IsA<Function_sp>(func));
  size_t vnargs = sizeof...(ARGS);
  size_t nargs = vnargs + LCC_FIXED_NUM;
  return func->entry()(func.raw_(), nargs, LCC_FROM_SMART_PTR(arg0), LCC_FROM_SMART_PTR(arg1), LCC_FROM_SMART_PTR(arg2), LCC_FROM_SMART_PTR(arg3), std::forward<ARGS>(args).raw_()...);
}

};

 namespace eval {
 void extract_declares_docstring_code_specials(List_sp inputBody, List_sp &declares, bool expectDocString, gc::Nilable<String_sp> &documentation, List_sp &code, List_sp &specials);
 
 void parse_lambda_body(List_sp body, List_sp &declares, gc::Nilable<String_sp> &docstring, List_sp &code);
 };

 /*! Funcall with a gctools::Frame of arguments */
 gctools::return_type funcall_frame(Function_sp func, gctools::Frame* frame);

};

#endif
