/*
    File: evaluator.cc
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
// If you turn on SOURCE_DEBUG and uncomment the DEBUG_LEVEL_FULL define
//    - it will slow the interpreter down a lot!!!!!!!!!
//#define DEBUG_LEVEL_FULL
//#include "core/foundation.h"
#include <clasp/core/foundation.h>
#include <clasp/core/common.h>
#include <clasp/core/corePackage.h>
#include <clasp/core/evaluator.h>
#include <clasp/core/iterator.h>
#include <clasp/core/metaClass.h>
#include <clasp/core/array.h>
#include <clasp/core/symbolTable.h>
#include <clasp/core/hashTable.h>
//#i n c l u d e "setfExpander.h"
#include <clasp/core/bytecode_compiler.h> // Lexenv
#include <clasp/core/designators.h>
#include <clasp/core/lambdaListHandler.h>
#include <clasp/core/predicates.h>
#include <clasp/core/debugger.h>
#include <clasp/core/predicates.h>
#include <clasp/core/lisp.h>
#include <clasp/core/backquote.h>
#include <clasp/core/sysprop.h>
#include <clasp/core/hashTableEq.h>
#include <clasp/core/hashTableEqual.h>
#include <clasp/core/multipleValues.h>
#include <clasp/core/primitives.h>
#include <clasp/core/array.h>
#include <clasp/core/wrappers.h>
#include <clasp/core/ql.h>
#include <clasp/core/unwind.h> // funwind_protect, etc

namespace cl {
extern core::Symbol_sp& _sym_or;
extern core::Symbol_sp& _sym_Symbol_O;
extern core::Symbol_sp& _sym_Cons_O;
};

namespace core {
namespace eval {
int _evaluateVerbosity = 0;

T_mv t1Evaluate(T_sp exp, T_sp environment);

void errorApplyZeroArguments() {
  SIMPLE_ERROR("Illegal to have zero arguments for APPLY");
}

void errorApplyLastArgumentNotList(T_sp lastArg ) {
  SIMPLE_ERROR("Last argument of APPLY is not a list/frame/activation-frame - passed {}", _rep_(lastArg));
}


};
};

namespace core {
CL_LAMBDA(form &optional env stepping compiler-env-p (execute t));
CL_DECLARE();
CL_UNWIND_COOP(true);
CL_DOCSTRING(R"dx(compileFormAndEvalWithEnv)dx");
DOCGROUP(clasp);
CL_DEFUN T_mv core__compile_form_and_eval_with_env(T_sp form, T_sp env, T_sp stepping, T_sp compiler_env_p, T_sp execute) {
  T_mv result = eval::funcall(comp::_sym_STARimplicit_compile_hookSTAR->symbolValue(), form, env);
  return result;
};





/*! The following APPLY function works by exploiting variadic arrays in C++.
Variadic arrays are allocated at the bottom of the stack frame (that grows down from high memory).
Since there is only 0 or 1 variadic array allocated in any path of the code 
the variadic array is always at the very bottom of the stack frame.
The TRICK: when you call a function from this APPLY function, the return address gets pushed 
on the stack immediately below the start of the variadic array and the T_O* variadic array 
will appear exactly like a vector of arguments passed on the stack!
Say you use (apply <func> a0 a1 list-of-5-arguments-a2-a7)
On x86-64 six (6) arguments are passed in the registers (di, si, dx, cx, r8, r9) respectively.
The closure object is passed in (di) and the total number of arguments is passed in (si)
This APPLY will allocate a variadic array and fill it - then just after the 
callee function is called, the stack will look like...
a7 <- variadic[3]          sp+4w
a6 <- variadic[2]          sp+3w
a5 <- variadic[1]          sp+2w
a4 <- variadic[0]          sp+1w
<return-address-to-apply>  sp
<where-bp-will-be-pushed>  sp-1w (stack-pointer)   
... and arguments a0 a1 a2 a3 will be in registers dx, cx, r8, r9
-----
Note: If you pass fewer than four Common Lisp arguments the remaining
register arguments are passed with values NULL.  The overhead of loading
constants into registers is insignificant.
Note: The call to cc_protect_alloca passes the pointer to variadic
to cc_protect_alloca that is declared __attribute__((optnone) and 
cc_protect_alloca immediately returns.
The TRICK: The act of passing the pointer of the lexical (variadic)
to another function prevents the compiler from optimizing (variadic)
lexical variable away!!
-----
Refinements:
(1) Write specialized versions that take 0, 1, 2, 3, 4 fixed
    arguments in registers and a list/vaslist as the last argument.
    See core__apply0 and core__apply1 below.
    Then use a compiler macro to select between these.
    This will optimize the common cases where APPLY is used with entirely
    register arguments.
(2) Specialize this function to accept a Function object as the head and get rid of the
    runtime test in coerce::functionDesignator(head);

*/

extern "C" void cc_protect_alloca(char* ptr);
#define REG_ARGS 4  // 4 common lisp arguments in registers
#define ALLOCA_variadic() T_O* variadic[nargs-REG_ARGS]; cc_protect_alloca((char*)&variadic[0]);
#define GET_AND_ADVANCE_LIST(x_,cur_) {x_ = CONS_CAR(cur_).raw_(); cur_ = CONS_CDR(gc::As_unsafe<Cons_sp>(cur_)); }
#define GET_AND_ADVANCE_VASLIST(x_,cur_) {x_ = cur_->next_arg_raw(); };
// NOTE that changes to REG_ARGS require changes to code below.

T_mv apply0_inner_valist(Function_sp func, Vaslist_sp var) {
  T_O *a0, *a1, *a2, *a3;
  int lenRest = var->nargs();
  int nargs = lenRest + 0;
  Vaslist* vaslist = &*var;
  switch (lenRest) {
  case 0: 
      return (*func).entry_0()(func.raw_());
      break;
  case 1: 
      a0 = (*var)[0];
      return (*func).entry_1()(func.raw_(),a0);
      break;
  case 2: 
      a0 = (*var)[0];
      a1 = (*var)[1];
      return (*func).entry_2()(func.raw_(),a0,a1);
      break;
  case 3: 
      a0 = (*var)[0];
      a1 = (*var)[1];
      a2 = (*var)[2];
      return (*func).entry_3()(func.raw_(),a0,a1,a2);
      break;
  case 4: 
      a0 = (*var)[0];
      a1 = (*var)[1];
      a2 = (*var)[2];
      a3 = (*var)[3];
      return (*func).entry_4()(func.raw_(),a0,a1,a2,a3);
      break;
  default: { // lenRest>=4
    return (*func).entry()(func.raw_(),nargs,var->args());
  }
  }
}

T_mv apply1_inner_valist(Function_sp func, T_O* a0, Vaslist_sp var) {
  T_O *a1, *a2, *a3;
  size_t lenRest = var->nargs();
  size_t nargs = lenRest + 1;
  switch (lenRest) {
  case 0: 
      return (*func).entry_1()(func.raw_(),a0);
      break;
  case 1: 
      a1 = (*var)[0];
      return (*func).entry_2()(func.raw_(),a0,a1);
      break;
  case 2: 
      a1 = (*var)[0];
      a2 = (*var)[1];
      return (*func).entry_3()(func.raw_(),a0,a1,a2);
      break;
  case 3: 
      a1 = (*var)[0];
      a2 = (*var)[1];
      a3 = (*var)[2];
      return (*func).entry_4()(func.raw_(),a0,a1,a2,a3);
      break;
  default: { // lenRest>=4
    MAKE_STACK_FRAME( frame, nargs );
    size_t idx(0);
    gctools::fill_frame_one( frame, idx, a0 );
    gctools::fill_frame_vaslist( frame, idx, var );
    CHECK_FRAME( frame, idx, nargs );
    return (*func).entry()(func.raw_(),nargs,frame->arguments());
  }
  }
}

T_mv apply2_inner_valist(Function_sp func, T_O* a0, T_O* a1, Vaslist_sp var) {
  T_O *a2, *a3;
  size_t lenRest = var->nargs();
  size_t nargs = lenRest + 2;
  switch (lenRest) {
  case 0: 
      return (*func).entry_2()(func.raw_(),a0,a1);
      break;
  case 1: 
      a2 = (*var)[0];
      return (*func).entry_3()(func.raw_(),a0,a1,a2);
      break;
  case 2: 
      a2 = (*var)[0];
      a3 = (*var)[1];
      return (*func).entry_4()(func.raw_(),a0,a1,a2,a3);
      break;
  default: { // lenRest>=4
    MAKE_STACK_FRAME( frame, nargs );
    size_t idx(0);
    gctools::fill_frame_one( frame, idx, a0 );
    gctools::fill_frame_one( frame, idx, a1 );
    gctools::fill_frame_vaslist( frame, idx, var );
    CHECK_FRAME( frame, idx, nargs );
    return (*func).entry()(func.raw_(),nargs,frame->arguments(0));
  }
  }
}

T_mv apply3_inner_valist(Function_sp func, T_O* a0, T_O* a1, T_O* a2, Vaslist_sp var) {
  T_O *a3;
  size_t lenRest = var->nargs();
  size_t nargs = lenRest + 3;
  switch (lenRest) {
  case 0: 
      return (*func).entry_3()(func.raw_(),a0,a1,a2);
      break;
  case 1: 
      a3 = (*var)[0];
      return (*func).entry_4()(func.raw_(),a0,a1,a2,a3);
      break;
  default: { // lenRest>=4
    MAKE_STACK_FRAME( frame, nargs );
    size_t idx(0);
    gctools::fill_frame_one( frame, idx, a0 );
    gctools::fill_frame_one( frame, idx, a1 );
    gctools::fill_frame_one( frame, idx, a2 );
    gctools::fill_frame_vaslist( frame, idx, var );
    CHECK_FRAME( frame, idx, nargs );
    return (*func).entry()(func.raw_(),nargs,frame->arguments(0));
  }
  }
}

T_mv apply4_inner_valist(Function_sp func, Vaslist_sp v,
                         T_O* a0, T_O* a1, T_O* a2, T_O *a3,
                         Vaslist_sp var) {
  size_t lenRest = var->nargs();
  size_t nargs = lenRest + 4 + v->nargs();
  MAKE_STACK_FRAME( frame, nargs );
  size_t idx(0);
  gctools::fill_frame_one( frame, idx, a0 );
  gctools::fill_frame_one( frame, idx, a1 );
  gctools::fill_frame_one( frame, idx, a2 );
  gctools::fill_frame_one( frame, idx, a3 );
  gctools::fill_frame_vaslist( frame, idx, var );
  gctools::fill_frame_vaslist( frame, idx, v );
  CHECK_FRAME( frame, idx, nargs );
  return (*func).entry()(func.raw_(),idx,frame->arguments(0));
}


T_mv apply0_inner_list(Function_sp func, T_sp var )
{
  const size_t fargs = 0;
  T_O* a0;
  T_O* a1;
  T_O* a2;
  T_O* a3;
  size_t rargs = 0;
  {
    T_sp cur = var;
    while (cur.consp()) {
      ++rargs;
      cur = CONS_CDR(cur);
    }
    if (cur.notnilp()) TYPE_ERROR_PROPER_LIST(var);
  }
  size_t nargs = fargs + rargs;
  switch (rargs) {
  case 0:
      return (*func).entry_0()(func.raw_());
      break;
  case 1: 
      GET_AND_ADVANCE_LIST(a0,var);
      return (*func).entry_1()(func.raw_(),a0);
      break;
  case 2: 
      GET_AND_ADVANCE_LIST(a0,var);
      GET_AND_ADVANCE_LIST(a1,var);
      return (*func).entry_2()(func.raw_(),a0,a1);
      break;
  case 3: 
      GET_AND_ADVANCE_LIST(a0,var);
      GET_AND_ADVANCE_LIST(a1,var);
      GET_AND_ADVANCE_LIST(a2,var);
      return (*func).entry_3()(func.raw_(),a0,a1,a2);
      break;
  case 4: 
      GET_AND_ADVANCE_LIST(a0,var);
      GET_AND_ADVANCE_LIST(a1,var);
      GET_AND_ADVANCE_LIST(a2,var);
      GET_AND_ADVANCE_LIST(a3,var);
      return (*func).entry_4()(func.raw_(),a0,a1,a2,a3);
      break;
  default: { // lenRest>=1
    MAKE_STACK_FRAME( frame, nargs );
    size_t idx(0);
    gctools::fill_frame_list( frame, idx, var );
    CHECK_FRAME( frame, idx, nargs );
    return (*func).entry()(func.raw_(),nargs,frame->arguments(0));
  }
  }
}


T_mv apply1_inner_list(Function_sp func, T_O* a0, T_sp var )
{
  const size_t fargs = 1;
  T_O* a1;
  T_O* a2;
  T_O* a3;
  size_t rargs = 0;
  {
    T_sp cur = var;
    while (cur.consp()) {
      ++rargs;
      cur = CONS_CDR(cur);
    }
    if (cur.notnilp()) TYPE_ERROR_PROPER_LIST(var);
  }
  size_t nargs = fargs + rargs;
  switch (rargs) {
  case 0: 
      return (*func).entry_1()(func.raw_(),a0);
      break;
  case 1: 
      GET_AND_ADVANCE_LIST(a1,var);
      return (*func).entry_2()(func.raw_(),a0,a1);
      break;
  case 2: 
      GET_AND_ADVANCE_LIST(a1,var);
      GET_AND_ADVANCE_LIST(a2,var);
      return (*func).entry_3()(func.raw_(),a0,a1,a2);
      break;
  case 3: 
      GET_AND_ADVANCE_LIST(a1,var);
      GET_AND_ADVANCE_LIST(a2,var);
      GET_AND_ADVANCE_LIST(a3,var);
      return (*func).entry_4()(func.raw_(),a0,a1,a2,a3);
      break;
  default: { // lenRest>=1
    MAKE_STACK_FRAME( frame, nargs );
    size_t idx(0);
    gctools::fill_frame_one( frame, idx, a0 );
    gctools::fill_frame_list( frame, idx, var );
    CHECK_FRAME( frame, idx, nargs );
    return (*func).entry()(func.raw_(),nargs,frame->arguments(0));
  }
  }
}


T_mv apply2_inner_list(Function_sp func, T_O* a0, T_O* a1, T_sp var )
{
  const size_t fargs = 2;
  T_O* a2;
  T_O* a3;
  size_t rargs = 0;
  {
    T_sp cur = var;
    while (cur.consp()) {
      ++rargs;
      cur = CONS_CDR(cur);
    }
    if (cur.notnilp()) TYPE_ERROR_PROPER_LIST(var);
  }
  size_t nargs = fargs + rargs;
  switch (rargs) {
  case 0: 
      return (*func).entry_2()(func.raw_(),a0,a1);
      break;
  case 1: 
      GET_AND_ADVANCE_LIST(a2,var);
      return (*func).entry_3()(func.raw_(),a0,a1,a2);
      break;
  case 2: 
      GET_AND_ADVANCE_LIST(a2,var);
      GET_AND_ADVANCE_LIST(a3,var);
      return (*func).entry_4()(func.raw_(),a0,a1,a2,a3);
      break;
  default: { // lenRest>=1
    MAKE_STACK_FRAME( frame, nargs );
    size_t idx(0);
    gctools::fill_frame_one( frame, idx, a0 );
    gctools::fill_frame_one( frame, idx, a1 );
    gctools::fill_frame_list( frame, idx, var );
    CHECK_FRAME( frame, idx, nargs );
    return (*func).entry()(func.raw_(),nargs,frame->arguments(0));
  }
  }
}


T_mv apply3_inner_list(Function_sp func, T_O* a0, T_O* a1, T_O* a2, T_sp var )
{
  const size_t fargs = 3;
  T_O* a3;
  size_t rargs = 0;
  {
    T_sp cur = var;
    while (cur.consp()) {
      ++rargs;
      cur = CONS_CDR(cur);
    }
    if (cur.notnilp()) TYPE_ERROR_PROPER_LIST(var);
  }
  size_t nargs = fargs + rargs;
  switch (rargs) {
  case 0: 
      return (*func).entry_3()(func.raw_(),a0,a1,a2);
      break;
  case 1: 
      GET_AND_ADVANCE_LIST(a3,var);
      return (*func).entry_4()(func.raw_(),a0,a1,a2,a3);
      break;
  default: { // lenRest>=1
    MAKE_STACK_FRAME( frame, nargs );
    size_t idx(0);
    gctools::fill_frame_one( frame, idx, a0 );
    gctools::fill_frame_one( frame, idx, a1 );
    gctools::fill_frame_one( frame, idx, a2 );
    gctools::fill_frame_list( frame, idx, var );
    CHECK_FRAME( frame, idx, nargs );
    return (*func).entry()(func.raw_(),nargs,frame->arguments(0));
  }
  }
}

T_mv apply4_inner_list(Function_sp func, T_sp var,
                       T_O* a0, T_O* a1, T_O* a2, T_O* a3,
                       Vaslist_sp fixed) {
  size_t lenRest = 0;
  {
    T_sp cur = var;
    while (cur.consp()) {
      ++lenRest;
      cur = CONS_CDR(cur);
    }
    if (cur.notnilp()) TYPE_ERROR_PROPER_LIST(var);
  }
  size_t nargs = lenRest + fixed->nargs() + 4;
  MAKE_STACK_FRAME( frame, nargs );
  size_t idx(0);
  gctools::fill_frame_one( frame, idx, a0 );
  gctools::fill_frame_one( frame, idx, a1 );
  gctools::fill_frame_one( frame, idx, a2 );
  gctools::fill_frame_one( frame, idx, a3 );
  gctools::fill_frame_vaslist( frame, idx, fixed );
  gctools::fill_frame_list( frame, idx, var );
  CHECK_FRAME( frame, idx, nargs );
  return (*func).entry()(func.raw_(),nargs,frame->arguments(0));
}


/* The idea is that given a call to apply: (apply f1 f2... fn var),
 * we end up here with var = var, lenFixed = n, fixed = the apply valist.
 * When var is a Vaslist, naturally. */
T_mv apply_inner_valist(Function_sp func, size_t lenFixed, Vaslist_sp fixed, Vaslist_sp var) {
  size_t nargs_var = var->nargs();
  size_t total_args = lenFixed + nargs_var;
  MAKE_STACK_FRAME( frame, total_args );
  size_t idx(0);
  gctools::fill_frame_nargs_args( frame, idx, lenFixed, fixed->args() );
  gctools::fill_frame_vaslist( frame, idx, var );
  CHECK_FRAME( frame, idx, total_args );
  return (*func).entry()(func.raw_(),total_args,frame->arguments());
}

T_mv apply_inner_list(Function_sp func, size_t lenFixed, Vaslist_sp fixed, List_sp var) {
  size_t nargs_var = cl__length(var);
  size_t total_args = lenFixed + nargs_var;
  MAKE_STACK_FRAME( frame, total_args );
  size_t idx(0);
  gctools::fill_frame_nargs_args( frame, idx, lenFixed, fixed->args() );
  gctools::fill_frame_list( frame, idx, var );
  CHECK_FRAME( frame, idx, total_args );
  return (*func).entry()(func.raw_(),total_args,frame->arguments());
}

CL_LAMBDA(head core:&va-rest args);
CL_DECLARE();
CL_UNWIND_COOP(true);
CL_DOCSTRING(R"dx(apply)dx");
DOCGROUP(clasp);
CL_DEFUN T_mv cl__apply(T_sp head, Vaslist_sp args) {
  Function_sp func = coerce::calledFunctionDesignator( head );
  if (args->nargs_zero()) eval::errorApplyZeroArguments();
  size_t lenArgs = args->nargs();
  T_O* lastArgRaw = (*args)[lenArgs - 1];
  if (gctools::tagged_vaslistp(lastArgRaw)) {
    Vaslist_sp valast((gc::Tagged)lastArgRaw);
    return apply_inner_valist(func, lenArgs - 1, args, valast);
  } else if ( gctools::tagged_consp(lastArgRaw) || gctools::tagged_nilp(lastArgRaw)) {
    T_sp lastArg((gc::Tagged)lastArgRaw);
    return apply_inner_list(func, lenArgs - 1, args, lastArg);
  } else {
    T_sp lastArg((gc::Tagged)lastArgRaw);
    eval::errorApplyLastArgumentNotList(lastArg);
  }
  UNREACHABLE();
}

// Calls to these APPLYN functions are compiler-macroexpanded from regular APPLY calls.

CL_LAMBDA(func args);
CL_DECLARE();
CL_UNWIND_COOP(true);
CL_DOCSTRING(R"dx((apply f m) = (apply0 (coerce-fdesignator f) m))dx");
DOCGROUP(clasp);
CL_DEFUN T_mv core__apply0(Function_sp func, T_sp lastArg) {
  if (lastArg.valistp()) {
    return apply0_inner_valist(func, gc::As_unsafe<Vaslist_sp>(lastArg));
  }
  else if (lastArg.consp() || lastArg.nilp())
    return apply0_inner_list(func, lastArg);
  else eval::errorApplyLastArgumentNotList(lastArg);
  UNREACHABLE();
}

CL_LAMBDA(func args);
CL_DECLARE();
CL_DOCSTRING(R"dx((apply f m) = (apply0 (coerce-fdesignator f) m))dx");
DOCGROUP(clasp);
CL_DEFUN T_mv core__trace_apply0(Function_sp func, T_sp lastArg) {
  if (lastArg.valistp()) {
    return apply0_inner_valist(func, gc::As_unsafe<Vaslist_sp>(lastArg));
  }
  else if (lastArg.consp() || lastArg.nilp())
    return apply0_inner_list(func, lastArg);
  else eval::errorApplyLastArgumentNotList(lastArg);
  UNREACHABLE();
}

CL_LAMBDA(func args arg0);
CL_DECLARE();
CL_UNWIND_COOP(true);
CL_DOCSTRING(R"dx((apply f a m) = (apply1 (coerce-fdesignator f) m a))dx");
DOCGROUP(clasp);
CL_DEFUN T_mv core__apply1(Function_sp func, T_sp lastArg, T_sp arg0) {
  if (lastArg.valistp())
    return apply1_inner_valist(func, arg0.raw_(), gc::As_unsafe<Vaslist_sp>(lastArg));
  else if (lastArg.consp() || lastArg.nilp())
    return apply1_inner_list(func, arg0.raw_(), lastArg);
  else eval::errorApplyLastArgumentNotList(lastArg);
  UNREACHABLE();
}

CL_LAMBDA(func args arg0 arg1);
CL_DECLARE();
CL_UNWIND_COOP(true);
CL_DOCSTRING(R"dx((apply f a b m) = (apply2 (coerce-fdesignator f) m a b))dx");
DOCGROUP(clasp);
CL_DEFUN T_mv core__apply2(Function_sp func, T_sp lastArg,
                           T_sp arg0, T_sp arg1) {
  if (lastArg.valistp())
    return apply2_inner_valist(func, arg0.raw_(), arg1.raw_(), gc::As_unsafe<Vaslist_sp>(lastArg));
  else if (lastArg.consp() || lastArg.nilp())
    return apply2_inner_list(func, arg0.raw_(), arg1.raw_(), lastArg);
  else eval::errorApplyLastArgumentNotList(lastArg);
  UNREACHABLE();
}


CL_LAMBDA(func args arg0 arg1 arg2);
CL_DECLARE();
CL_UNWIND_COOP(true);
CL_DOCSTRING(R"dx((apply f a b c m) = (apply3 (coerce-fdesignator f) m a b c))dx");
DOCGROUP(clasp);
CL_DEFUN T_mv core__apply3(Function_sp func, T_sp lastArg,
                           T_sp arg0, T_sp arg1, T_sp arg2) {
  if (lastArg.valistp())
    return apply3_inner_valist(func, arg0.raw_(), arg1.raw_(), arg2.raw_(), gc::As_unsafe<Vaslist_sp>(lastArg));
  else if (lastArg.consp() || lastArg.nilp())
    return apply3_inner_list(func, arg0.raw_(), arg1.raw_(), arg2.raw_(), lastArg);
  else eval::errorApplyLastArgumentNotList(lastArg);
  UNREACHABLE();
}

CL_LAMBDA(func args arg0 arg1 arg2 arg3 core:&va-rest more);
CL_DECLARE();
CL_UNWIND_COOP(true);
CL_DOCSTRING(R"dx((apply f a b c d ... m) = (apply4 (coerce-fdesignator f) m a b c d ...))dx");
DOCGROUP(clasp);
CL_DEFUN T_mv core__apply4(Function_sp func, T_sp lastArg,
                           T_sp arg0, T_sp arg1, T_sp arg2, T_sp arg3,
                           Vaslist_sp more) {
  if (lastArg.valistp())
    return apply4_inner_valist(func, gc::As_unsafe<Vaslist_sp>(lastArg),
                               arg0.raw_(), arg1.raw_(), arg2.raw_(), arg3.raw_(),
                               more);
  else if (lastArg.consp() || lastArg.nilp())
    return apply4_inner_list(func, lastArg,
                             arg0.raw_(), arg1.raw_(), arg2.raw_(), arg3.raw_(),
                             more);
  else eval::errorApplyLastArgumentNotList(lastArg);
  UNREACHABLE();
}

CL_LAMBDA(form);
CL_DECLARE();
CL_UNWIND_COOP(true);
CL_DOCSTRING(R"dx(eval)dx");
DOCGROUP(clasp);
CL_DEFUN T_mv cl__eval(T_sp form) {
  if (!core::_sym_STAReval_with_env_hookSTAR->boundP() ||
      core::_sym_STARuseInterpreterForEvalSTAR->symbolValue().isTrue()
      ) {
    return eval::evaluate(form, nil<T_O>());
  } else {
    return eval::funcall(core::_sym_STAReval_with_env_hookSTAR->symbolValue(), form, nil<T_O>());
  }
};

CL_DECLARE();
CL_DOCSTRING(R"dx(Interpret FORM in the interpreter environment ENV.)dx");
CL_UNWIND_COOP(true);
DOCGROUP(clasp);
CL_DEFUN T_mv core__interpret(T_sp form, T_sp env) {
  return eval::evaluate(form, env);
}


// fast funcall. FIXME: use va-rest
CL_LAMBDA(function-desig &rest args);
CL_DECLARE((declare (dynamic-extent args)));
CL_UNWIND_COOP(true);
CL_DOCSTRING(R"dx(See CLHS: funcall)dx");
DOCGROUP(clasp);
CL_DEFUN T_mv cl__funcall(T_sp function_desig, List_sp args) {
  //    printf("%s:%d cl__funcall should be inlined after the compiler starts up\n", __FILE__, __LINE__ );
  Function_sp func = coerce::calledFunctionDesignator(function_desig);
  if (func.nilp()) {
    ERROR_UNDEFINED_FUNCTION(function_desig);
  }
  if (func.unboundp()) {
    if (function_desig.nilp()) SIMPLE_ERROR("The function designator was NIL");
    if (function_desig.unboundp()) SIMPLE_ERROR("The function designator was UNBOUND");
    SIMPLE_ERROR("The function {} was unbound", _rep_(function_desig));
  }
  size_t nargs = cl__length(args);
  MAKE_STACK_FRAME( fargs, nargs );
  size_t ia(0);
  gctools::fill_frame_list( fargs, ia, args );
  T_mv res = funcall_general<core::Function_O>(func.tagged_(), nargs, fargs->arguments(0));
  return res;
}

CL_LAMBDA(arg);
CL_DECLARE();
CL_UNWIND_COOP(true);
CL_DOCSTRING(R"dx(coerce_to_function)dx");
DOCGROUP(clasp);
CL_DEFUN Function_sp core__coerce_to_function(T_sp arg) {
  if (Function_sp fnobj = arg.asOrNull<Function_O>()) {
    return fnobj;
  } else if (Symbol_sp sym = arg.asOrNull<Symbol_O>()) {
    return sym->symbolFunction();
  } else if (Cons_sp carg = arg.asOrNull<Cons_O>()) {
    T_sp head = oCar(carg);
    if (head == cl::_sym_setf) {
      Symbol_sp sym = oCadr(carg).as<Symbol_O>();
      return sym->getSetfFdefinition();
    } else if (head == cl::_sym_lambda) {
      // Really, this will always return a function, but we'll sanity check.
      // NOTE: Hypothetically we could speed this up a bit by using the parts
      // of the evaluator that make functions directly, but then it's hard to
      // use the Cleavir hooks, and for not much reason.
      return gc::As<Function_sp>(cl__eval(arg));
    }
  }
  SIMPLE_ERROR("Illegal function designator {}", _rep_(arg));
};

CL_LAMBDA(body &optional expectDocString);
CL_DECLARE();
CL_DOCSTRING(R"dx(Handle special declarations and remove declarations from body. Return MultipleValues: declarations body documentation specials)dx");
DOCGROUP(clasp);
CL_DEFUN T_mv core__process_declarations(List_sp inputBody, T_sp expectDocString) {
  bool b_expect_doc = expectDocString.isTrue();
  List_sp declares = nil<T_O>();
  gc::Nilable<String_sp> docstring;
  List_sp code;
  List_sp specials;
  eval::extract_declares_docstring_code_specials(inputBody, declares,
                                                 b_expect_doc, docstring, code, specials);
  T_sp tdeclares = core__canonicalize_declarations(declares);
  return Values(tdeclares, code, (T_sp)docstring, specials);
};

CL_LAMBDA(declare-list &optional default);
CL_DECLARE();
CL_DOCSTRING(R"dx(If form has is a list of declares ((function-name xxx) ...) or else looks like `(lambda lambda-list [[declaration* | documentation]] (block xxx form*) ) then return XXX)dx");
DOCGROUP(clasp);
CL_DEFUN T_sp core__extract_lambda_name_from_declares(List_sp declares, T_sp defaultValue) {
  // First check for a (declare (core:function-name XXX))
  for (; declares.consp(); declares = oCdr(declares)) {
    List_sp decl = oCar(declares);
    if (oCar(decl) == core::_sym_lambdaName) {
      return oCadr(decl);
    }
  }
  return defaultValue;
}


CL_LAMBDA(declare-list);
CL_DECLARE();
CL_DOCSTRING(R"dx(If form has is a list of declares ((function-name xxx) ...) or else looks like `(lambda lambda-list [[declaration* | documentation]] (block xxx form*) ) then return XXX)dx");
DOCGROUP(clasp);
CL_DEFUN T_sp core__extract_dump_module_from_declares(List_sp declares) {
  // First check for a (declare (core:function-name XXX))
  for ( auto cur : declares ) {
    T_sp decl = CONS_CAR(declares);
    if (decl.consp()) {
      if (oCar(decl) == core::_sym_dump_module) {
        return oCadr(decl);
      } else if (decl == core::_sym_dump_module) {
        return _lisp->_true();
      }
    }
  }
  return nil<T_O>();
}

CL_LAMBDA(form &optional default);
CL_DECLARE();
CL_DOCSTRING(R"dx(If form has is a list of declares ((function-name xxx) ...) or else looks like `(lambda lambda-list [[declaration* | documentation]] (block xxx form*) ) then return XXX)dx");
DOCGROUP(clasp);
CL_DEFUN T_sp core__extract_lambda_name(List_sp lambdaExpression, T_sp defaultValue) {
  List_sp body = oCddr(lambdaExpression);
  List_sp declares;
  gc::Nilable<String_sp> docstring;
  List_sp form;
  eval::parse_lambda_body(body, declares, docstring, form);
  // First check for a (declare (core:function-name XXX))
  T_sp name = core__extract_lambda_name_from_declares(declares, nil<T_O>());
  if (name.notnilp())
    return name;
  // Next check if there is a (lambda (...) (block XXX ...))
  if ((form).consp() && cl__length(form) == 1) {
    T_sp first = oCar(form);
    if ((first).consp()) {
      if (oCar(first) == cl::_sym_block) {
        T_sp second = oCadr(first);
        Symbol_sp name = gc::As<Symbol_sp>(oCadr(first));
        if (name.notnilp()) {
          // Only return block name if not nil
          return name;
        }
      }
    }
  }
  // Fallback return LAMBDA as the name
  return defaultValue;
}

CL_LISPIFY_NAME("ext:symbol-macro");
CL_LAMBDA(symbol &optional env);
CL_DECLARE();
CL_DOCSTRING(R"dx(Returns the macro expansion function for a symbol if it exists, or else NIL.)dx");
DOCGROUP(clasp);
CL_DEFUN T_sp ext__symbol_macro(Symbol_sp sym, T_sp env) {
  if (env.nilp()) { // nothing
  } else if (gc::IsA<comp::Lexenv_sp>(env)) {
    T_sp local = gc::As_unsafe<comp::Lexenv_sp>(env)->lookupSymbolMacro(sym);
    if (local.notnilp()) return local;
  } else { // pass to cleavir (which also checks global environment)
    SYMBOL_EXPORT_SC_(CorePkg, cleavirSymbolMacro);
    return eval::funcall(core::_sym_cleavirSymbolMacro, sym, env);
  }
  // check global environment
  SYMBOL_SC_(ExtPkg, symbolMacro);
  T_sp fn = nil<T_O>();
  T_mv result = core__get_sysprop(sym, ext::_sym_symbolMacro);
  MultipleValues& mvn = core::lisp_multipleValues();
  if (gc::As<T_sp>(mvn.valueGet(1,result.number_of_values())).notnilp()) {
    fn = gc::As<Function_sp>(result);
  }
  return fn;
};

CL_LAMBDA(arg);
CL_DECLARE();
CL_DOCSTRING(R"dx(evaluateVerbosity)dx");
DOCGROUP(clasp);
CL_DEFUN void core__evaluate_verbosity(Fixnum_sp level) {
  eval::_evaluateVerbosity = unbox_fixnum(level);
};

CL_LAMBDA(form &optional env);
CL_DECLARE();
CL_UNWIND_COOP(true);
CL_DOCSTRING(R"dx(Evaluate the form in the environment using the interpreter)dx");
DOCGROUP(clasp);
CL_DEFUN T_mv core__interpret_eval_with_env(T_sp form, T_sp environment) {
  return comp::bytecode_toplevel_eval(form, environment);
}

};

namespace core {

    //void parse_lambda_body(List_sp body, List_sp &declares, gc::Nilable<String_sp> &docstring, List_sp &code);


    namespace interpret {
        SYMBOL_EXPORT_SC_(ClPkg, case);

        SYMBOL_EXPORT_SC_(ClPkg, multipleValueSetq);

        SYMBOL_EXPORT_SC_(ClPkg, prog1);
    };

namespace eval {
        /*! LAMBDA expression processing.
	  Process ( [[declaration* | documentation]] form* ) and
	  aggregate the declarations into a list ( (decl-1) (decl-2) ... )
	  and identify the last documentation string seen.
	  Return the list of declarations, the documentation string and the rest
	  of the forms in CODE.  Identify the local special declarations and
          return them in SPECIALS but leave them in the DECLARES list */
void extract_declares_docstring_code_specials(List_sp inputBody, List_sp &declares, bool expectDocString, gc::Nilable<String_sp> &documentation, List_sp &code, List_sp &specials) {
  List_sp body = inputBody;
  declares = nil<T_O>();
  specials = nil<T_O>();
  for (; body.notnilp(); body = oCdr(body)) {
    if (!cl__listp(body)) {
      SIMPLE_ERROR("Bad input to processDeclares: {}", _rep_(inputBody));
    }
    T_sp form = oCar(body);
                // If we are expecting docstring and we hit a string, then we hit a possible docstring
    if (expectDocString && cl__stringp(form)) {
                    // If there is something following the current element then treat it as a docstring
      if (oCdr(body).notnilp()) {
                        // Here we are in undefined behavior CLHS 3.4.11
                        // we may be replacing previous docstrings
        documentation = gc::As<String_sp>(form);
        continue;
      } else {
                        // Nothing follows so the current form is a form
                        // and stop looking for docstrings or declares
        break;
      }
    }
    if (cl__atom(form) || oCar(form) != cl::_sym_declare) {
      break;
    }
    List_sp cform = form;
    for (cform = oCdr(form); cform.notnilp();) {
      List_sp sentence = oCar(cform);
      cform = oCdr(cform);
      declares = Cons_O::create(sentence, declares);
      T_sp sentenceHead = oCar(sentence);
      sentence = oCdr(sentence);
      if (sentenceHead == cl::_sym_special) {
        while (sentence.notnilp()) {
          T_sp v = oCar(sentence);
          sentence = oCdr(sentence);
          if (!cl__symbolp(v)) {
            SIMPLE_ERROR("Illegal object[{}] in declare special", _rep_(v));
          }
          specials = Cons_O::create(v, specials);
        }
      }
    }
  }
  code = body;
  declares = cl__nreverse(declares);
}

void extract_declares_code(List_sp args, List_sp &declares, List_sp &code) {
  gc::Nilable<String_sp> dummy_docstring;
  List_sp specials;
  IMPLEMENT_MEF("Check who is using this and why they aren't calling extract_declares_docstring_code_specials directly");
  extract_declares_docstring_code_specials(args, declares, false, dummy_docstring, code, specials);
}

void parse_lambda_body(List_sp body, List_sp &declares, gc::Nilable<String_sp> &docstring, List_sp &code) {
  LOG("Parsing lambda body: {}" , body->__repr__());
  List_sp specials;
  extract_declares_docstring_code_specials(body, declares, true, docstring, code, specials);
}

SYMBOL_EXPORT_SC_(KeywordPkg, execute);
SYMBOL_EXPORT_SC_(KeywordPkg, load_toplevel);
};

    namespace eval {

        SYMBOL_EXPORT_SC_(CompPkg, compileInEnv);
    
    T_mv evaluate(T_sp exp, T_sp environment) {
      return comp::cmp__bytecode_implicit_compile_form(exp, environment);
    }

        SYMBOL_EXPORT_SC_(ClPkg, block);
        SYMBOL_EXPORT_SC_(ClPkg, quote);
        SYMBOL_EXPORT_SC_(ClPkg, progn);
        SYMBOL_EXPORT_SC_(ClPkg, throw);
    };


};


namespace core {


CL_DOCSTRING(R"dx(Return a list of all special operators as defined in aclasp)dx");
DOCGROUP(clasp);
CL_DEFUN core::List_sp core__aclasp_list_of_all_special_operators() {
  ql::list l;
  l << cl::_sym_progn;
  l << cl::_sym_block;
  l << cl::_sym_catch;
  l << cl::_sym_eval_when;
  l << cl::_sym_flet;
  l << cl::_sym_function;
  l << cl::_sym_the;
  l << cl::_sym_go;
  l << cl::_sym_if;
  l << cl::_sym_labels;
  l << cl::_sym_let;
  l << cl::_sym_letSTAR;
  l << cl::_sym_locally;
  l << cl::_sym_macrolet;
  l << cl::_sym_multiple_value_prog1;
  l << cl::_sym_multiple_value_call;
  l << core::_sym_debug_message;
  l << core::_sym_multiple_value_foreign_call;
  l << core::_sym_foreign_call;
  l << core::_sym_foreign_call_pointer;
  l << cl::_sym_progv;
  l << cl::_sym_quote;
  l << cl::_sym_return_from;
  l << cl::_sym_setq;
  l << cl::_sym_tagbody;
  l << cl::_sym_throw;
  l << cl::_sym_unwind_protect;
  l << cl::_sym_symbol_macrolet;
  l << cl::_sym_load_time_value;
  l << ext::_sym_specialVar;
  l << ext::_sym_lexicalVar;
  return l.cons();
}

SYMBOL_SC_(CorePkg, processDeclarations);
SYMBOL_EXPORT_SC_(ClPkg, eval);
    //	    SYMBOL_SC_(CorePkg,extractDeclaresDocstringCode);
    //	    Defun(extractDeclaresDocstringCode);
SYMBOL_SC_(CorePkg, evaluateVerbosity);
SYMBOL_SC_(CorePkg, classifyLetVariablesAndDeclares);
SYMBOL_EXPORT_SC_(ClPkg, ignore);
SYMBOL_EXPORT_SC_(ClPkg, apply);
SYMBOL_EXPORT_SC_(ClPkg, funcall);
SYMBOL_EXPORT_SC_(CorePkg, STAReval_with_env_hookSTAR);
SYMBOL_EXPORT_SC_(CorePkg, interpret_eval_with_env);


};
