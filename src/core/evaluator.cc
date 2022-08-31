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
#include <clasp/core/specialForm.h>
//#i n c l u d e "setfExpander.h"
#include <clasp/core/environment.h>
#include <clasp/core/bytecode_compiler.h> // BytecodeCmpEnv
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
  SIMPLE_ERROR(("Illegal to have zero arguments for APPLY"));
}

void errorApplyLastArgumentNotList(T_sp lastArg ) {
  SIMPLE_ERROR(("Last argument of APPLY is not a list/frame/activation-frame - passed %s") , _rep_(lastArg));
}


};
};

namespace core {
CL_LAMBDA(form &optional env stepping compiler-env-p (execute t))
CL_DECLARE();
CL_UNWIND_COOP(true);
CL_DOCSTRING(R"dx(compileFormAndEvalWithEnv)dx")
DOCGROUP(clasp)
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
  int lenRest = var->remaining_nargs();
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
  size_t lenRest = var->remaining_nargs();
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
  size_t lenRest = var->remaining_nargs();
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
  size_t lenRest = var->remaining_nargs();
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
  size_t lenRest = var->remaining_nargs();
  size_t nargs = lenRest + 4 + v->remaining_nargs();
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

#if 0
T_mv apply4_inner_list(Function_sp func, T_O* a0, T_O* a1, T_O* a2, T_O* a3, T_sp var )
{
  const size_t fargs = 4;
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
      return (*func).entry_4()(func.raw_(),a0,a1,a2,a3);
      break;
  default: { // lenRest>=1
    MAKE_STACK_FRAME( frame, nargs );
    size_t idx(0);
    gctools::fill_frame_one( frame, idx, a0 );
    gctools::fill_frame_one( frame, idx, a1 );
    gctools::fill_frame_one( frame, idx, a2 );
    gctools::fill_frame_one( frame, idx, a4 );
    gctools::fill_frame_list( frame, idx, var );
    CHECK_FRAME( frame, idx, nargs );
    return (*func).entry()(func.raw_(),nargs,frame->arguments(0));
  }
  }
}
#endif
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
  size_t nargs = lenRest + fixed->remaining_nargs() + 4;
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
  size_t nargs_var = var->remaining_nargs();
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

CL_LAMBDA(head core:&va-rest args)
CL_DECLARE();
CL_UNWIND_COOP(true);
CL_DOCSTRING(R"dx(apply)dx")
DOCGROUP(clasp)
CL_DEFUN T_mv cl__apply(T_sp head, Vaslist_sp args) {
  Function_sp func = coerce::functionDesignator( head );
  if (args->total_nargs() == 0) eval::errorApplyZeroArguments();
  size_t lenArgs = args->remaining_nargs();
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

CL_LAMBDA(func args)
CL_DECLARE();
CL_UNWIND_COOP(true);
CL_DOCSTRING(R"dx((apply f m) = (apply0 (coerce-fdesignator f) m))dx")
DOCGROUP(clasp)
CL_DEFUN T_mv core__apply0(Function_sp func, T_sp lastArg) {
  if (lastArg.valistp()) {
    return apply0_inner_valist(func, gc::As_unsafe<Vaslist_sp>(lastArg));
  }
  else if (lastArg.consp() || lastArg.nilp())
    return apply0_inner_list(func, lastArg);
  else eval::errorApplyLastArgumentNotList(lastArg);
  UNREACHABLE();
}

CL_LAMBDA(func args)
CL_DECLARE();
CL_DOCSTRING(R"dx((apply f m) = (apply0 (coerce-fdesignator f) m))dx")
DOCGROUP(clasp)
CL_DEFUN T_mv core__trace_apply0(Function_sp func, T_sp lastArg) {
  if (lastArg.valistp()) {
    return apply0_inner_valist(func, gc::As_unsafe<Vaslist_sp>(lastArg));
  }
  else if (lastArg.consp() || lastArg.nilp())
    return apply0_inner_list(func, lastArg);
  else eval::errorApplyLastArgumentNotList(lastArg);
  UNREACHABLE();
}

CL_LAMBDA(func args arg0)
CL_DECLARE();
CL_UNWIND_COOP(true);
CL_DOCSTRING(R"dx((apply f a m) = (apply1 (coerce-fdesignator f) m a))dx")
DOCGROUP(clasp)
CL_DEFUN T_mv core__apply1(Function_sp func, T_sp lastArg, T_sp arg0) {
  if (lastArg.valistp())
    return apply1_inner_valist(func, arg0.raw_(), gc::As_unsafe<Vaslist_sp>(lastArg));
  else if (lastArg.consp() || lastArg.nilp())
    return apply1_inner_list(func, arg0.raw_(), lastArg);
  else eval::errorApplyLastArgumentNotList(lastArg);
  UNREACHABLE();
}

CL_LAMBDA(func args arg0 arg1)
CL_DECLARE();
CL_UNWIND_COOP(true);
CL_DOCSTRING(R"dx((apply f a b m) = (apply2 (coerce-fdesignator f) m a b))dx")
DOCGROUP(clasp)
CL_DEFUN T_mv core__apply2(Function_sp func, T_sp lastArg,
                           T_sp arg0, T_sp arg1) {
  if (lastArg.valistp())
    return apply2_inner_valist(func, arg0.raw_(), arg1.raw_(), gc::As_unsafe<Vaslist_sp>(lastArg));
  else if (lastArg.consp() || lastArg.nilp())
    return apply2_inner_list(func, arg0.raw_(), arg1.raw_(), lastArg);
  else eval::errorApplyLastArgumentNotList(lastArg);
  UNREACHABLE();
}


CL_LAMBDA(func args arg0 arg1 arg2)
CL_DECLARE();
CL_UNWIND_COOP(true);
CL_DOCSTRING(R"dx((apply f a b c m) = (apply3 (coerce-fdesignator f) m a b c))dx")
DOCGROUP(clasp)
CL_DEFUN T_mv core__apply3(Function_sp func, T_sp lastArg,
                           T_sp arg0, T_sp arg1, T_sp arg2) {
  if (lastArg.valistp())
    return apply3_inner_valist(func, arg0.raw_(), arg1.raw_(), arg2.raw_(), gc::As_unsafe<Vaslist_sp>(lastArg));
  else if (lastArg.consp() || lastArg.nilp())
    return apply3_inner_list(func, arg0.raw_(), arg1.raw_(), arg2.raw_(), lastArg);
  else eval::errorApplyLastArgumentNotList(lastArg);
  UNREACHABLE();
}

CL_LAMBDA(func args arg0 arg1 arg2 arg3 core:&va-rest more)
CL_DECLARE();
CL_UNWIND_COOP(true);
CL_DOCSTRING(R"dx((apply f a b c d ... m) = (apply4 (coerce-fdesignator f) m a b c d ...))dx")
DOCGROUP(clasp)
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


#if 0
gctools::return_type fast_apply_general(T_O* func_tagged, T_O* args_tagged) {
  ASSERT(gctools::tagged_consp(args_tagged));
  Cons_O* cons_args = reinterpret_cast<Cons_O*>(gctools::untag_cons(args_tagged));
  int front_nargs = 0;
  Cons_O* cur = cons_args;
  for ( ; cur->cdr().consp(); cur = reinterpret_cast<Cons_O*>(gctools::untag_cons(cur->cdr().raw_()))) ++front_nargs;
  T_O* tail_tagged = cur->ocar().raw_();
  if (gctools::tagged_consp(tail_tagged)) {
    Cons_O* cons_tail = reinterpret_cast<Cons_O*>(gctools::untag_cons(tail_tagged));
    int tail_nargs = 1+cons_tail->length();
    int nargs = front_nargs+tail_nargs;
    MAKE_STACK_FRAME( frame, nargs );
    size_t idx(0);
    gctools::fill_frame_list( frame, idx, cons_args );
    Cons_O* tail_cur = cons_tail;
    for (int j=front_nargs; j<nargs; ++j ) {
      (*frame)[j] = tail_cur->ocar().raw_();
      tail_cur = reinterpret_cast<Cons_O*>(gctools::untag_cons(tail_cur->cdr().raw_()));
    }
    Vaslist valist_struct(nargs,frame);
    Vaslist_sp valist(&valist_struct); // = frame.setupVaslist(valist_struct);;
    return funcall_general<core::Function_O>((gc::Tagged)func_tagged, valist_struct._nargs, valist_struct._args );
  }
  Cons_O* cons_tail = reinterpret_cast<Cons_O*>(gctools::untag_cons(tail_tagged));
  int nargs = front_nargs;
  MAKE_STACK_FRAME( frame, nargs );
  Cons_O* front_cur = cons_args;
  for (int i=0; i<front_nargs; ++i ) {
    (*frame)[i] = front_cur->ocar().raw_();
    front_cur = reinterpret_cast<Cons_O*>(gctools::untag_cons(front_cur->cdr().raw_()));
  }
  Vaslist valist_struct(nargs,frame);
  Vaslist_sp valist(&valist_struct); // = frame.setupVaslist(valist_struct);;
  return funcall_general<core::Function_O>((gc::Tagged)func_tagged, valist_struct._nargs, valist_struct._args );
}
#endif

#if 0
template <typename... FixedArgs>
LCC_RETURN fast_apply_(T_O* function_tagged, T_O* rest_args_tagged, FixedArgs&&...fixedArgs) {
  int nargs;
  if (gctools::tagged_consp(rest_args_tagged)) {
    Cons_sp cons_rest_args((gctools::Tagged)rest_args_tagged);
    List_sp list_rest_args((gctools::Tagged)rest_args_tagged);
    nargs = sizeof...(FixedArgs)+cons_rest_args->length();
    MAKE_STACK_FRAME( frame, nargs );
    T_O* _[] = {fixedArgs...};
    for (int i=0; i<sizeof...(FixedArgs); ++i ) (*frame)[i] = _[i];
    for (int j=sizeof...(FixedArgs);j<nargs; ++j ) {
      (*frame)[j] = oCar(list_rest_args).raw_();
      list_rest_args = oCdr(list_rest_args);
    }
    Vaslist valist_struct(nargs,frame);
    Vaslist_sp valist(&valist_struct); // = frame.setupVaslist(valist_struct);;
    return funcall_general<core::Function_O>((gc::Tagged)function_tagged, valist_struct._nargs, valist_struct._args );
  }
  nargs = sizeof...(FixedArgs);
  MAKE_STACK_FRAME( frame, nargs );
  T_O* _[] = {fixedArgs...};
  for (int i=0; i<sizeof...(FixedArgs); ++i ) (*frame)[i] = _[i];
  Vaslist valist_struct(nargs,frame);
  Vaslist_sp valist(&valist_struct); // = frame.setupVaslist(valist_struct);;
  return funcall_general<core::Function_O>((gc::Tagged)function_tagged, valist_struct._nargs, valist_struct._args );
}


extern "C" {
gctools::return_type fast_apply0(T_O* function_tagged) {
  return fast_apply_(function_tagged,nil<T_O>().raw_());
}
gctools::return_type fast_apply1(T_O* function_tagged, T_O* rest) {
  return fast_apply_(function_tagged,rest);
}
gctools::return_type fast_apply2(T_O* function_tagged, T_O* arg0, T_O* rest) {
  return fast_apply_(function_tagged,rest,arg0);
};
gctools::return_type fast_apply3(T_O* function_tagged, T_O* arg0, T_O* arg1, T_O* rest) {
  return fast_apply_(function_tagged,rest,arg0,arg1);
};
gctools::return_type fast_apply4(T_O* function_tagged, T_O* arg0, T_O* arg1, T_O* arg2, T_O* rest) {
  return fast_apply_(function_tagged,rest,arg0,arg1,arg2);
};
gctools::return_type fast_apply5(T_O* function_tagged, T_O* arg0, T_O* arg1, T_O* arg2, T_O* arg3, T_O* rest) {
  return fast_apply_(function_tagged,rest,arg0,arg1,arg2,arg3);
};
gctools::return_type fast_apply6(T_O* function_tagged, T_O* arg0, T_O* arg1, T_O* arg2, T_O* arg3, T_O* arg4, T_O* rest) {
  return fast_apply_(function_tagged,rest,arg0,arg1,arg2,arg3,arg4);
};
gctools::return_type fast_apply7(T_O* function_tagged, T_O* arg0, T_O* arg1, T_O* arg2, T_O* arg3, T_O* arg4, T_O* arg5, T_O* rest) {
  return fast_apply_(function_tagged,rest,arg0,arg1,arg2,arg3,arg4,arg5);
};
gctools::return_type fast_apply8(T_O* function_tagged, T_O* arg0, T_O* arg1, T_O* arg2, T_O* arg3, T_O* arg4, T_O* arg5, T_O* arg6, T_O* rest) {
  return fast_apply_(function_tagged,rest,arg0,arg1,arg2,arg3,arg4,arg5,arg6);
};
};
#endif // FAST_APPLY

CL_LAMBDA(form)
CL_DECLARE();
CL_UNWIND_COOP(true);
CL_DOCSTRING(R"dx(eval)dx")
DOCGROUP(clasp)
CL_DEFUN T_mv cl__eval(T_sp form) {
  if (core::_sym_STAReval_with_env_hookSTAR.unboundp() ||
      !core::_sym_STAReval_with_env_hookSTAR->boundP() ||
      core::_sym_STARuseInterpreterForEvalSTAR->symbolValue().isTrue()
      ) {
    return eval::evaluate(form, nil<T_O>());
  } else {
    return eval::funcall(core::_sym_STAReval_with_env_hookSTAR->symbolValue(), form, nil<T_O>());
  }
};

CL_DECLARE();
CL_DOCSTRING(R"dx(Interpret FORM in the interpreter environment ENV.)dx")
CL_UNWIND_COOP(true);
DOCGROUP(clasp)
CL_DEFUN T_mv core__interpret(T_sp form, T_sp env) {
  return eval::evaluate(form, env);
}


// fast funcall
CL_LAMBDA(function-desig &rest args)
CL_DECLARE((declare (dynamic-extent args)));
CL_UNWIND_COOP(true);
CL_DOCSTRING(R"dx(See CLHS: funcall)dx")
DOCGROUP(clasp)
CL_DEFUN T_mv cl__funcall(T_sp function_desig, List_sp args) {
  //    printf("%s:%d cl__funcall should be inlined after the compiler starts up\n", __FILE__, __LINE__ );
  Function_sp func = coerce::functionDesignator(function_desig);
  if (func.nilp()) {
    ERROR_UNDEFINED_FUNCTION(function_desig);
  }
  if (func.unboundp()) {
    if (function_desig.nilp()) SIMPLE_ERROR(("The function designator was NIL"));
    if (function_desig.unboundp()) SIMPLE_ERROR(("The function designator was UNBOUND"));
    SIMPLE_ERROR(("The function %s was unbound") , _rep_(function_desig));
  }
  size_t nargs = cl__length(args);
  MAKE_STACK_FRAME( fargs, nargs );
  size_t ia(0);
  gctools::fill_frame_list( fargs, ia, args );
  T_mv res = funcall_general<core::Function_O>(func.tagged_(), nargs, fargs->arguments(0));
  return res;
}

CL_LAMBDA(arg)
CL_DECLARE();
CL_UNWIND_COOP(true);
CL_DOCSTRING(R"dx(coerce_to_function)dx")
DOCGROUP(clasp)
CL_DEFUN Function_sp core__coerce_to_function(T_sp arg) {
  if (Function_sp fnobj = arg.asOrNull<Function_O>()) {
    return fnobj;
  } else if (Symbol_sp sym = arg.asOrNull<Symbol_O>()) {
    if (!sym->fboundp())
      SIMPLE_ERROR(("Function value for %s is unbound") , _rep_(sym));
    return sym->symbolFunction();
  } else if (Cons_sp carg = arg.asOrNull<Cons_O>()) {
    T_sp head = oCar(carg);
    if (head == cl::_sym_setf) {
      Symbol_sp sym = oCadr(carg).as<Symbol_O>();
      if (!sym->fboundp_setf()) {
        SIMPLE_ERROR(("SETF function value for %s is unbound") , _rep_(sym));
      }
      return sym->getSetfFdefinition();
    } else if (head == cl::_sym_lambda) {
      // Really, this will always return a function, but we'll sanity check.
      // NOTE: Hypothetically we could speed this up a bit by using the parts
      // of the evaluator that make functions directly, but then it's hard to
      // use the Cleavir hooks, and for not much reason.
      return gc::As<Function_sp>(cl__eval(arg));
    }
  }
  SIMPLE_ERROR(("Illegal function designator %s") , _rep_(arg));
};

CL_LAMBDA(body &optional expectDocString)
CL_DECLARE();
CL_DOCSTRING(R"dx(Handle special declarations and remove declarations from body. Return MultipleValues: declarations body documentation specials)dx")
DOCGROUP(clasp)
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

CL_LAMBDA(declare-list &optional default)
CL_DECLARE();
CL_DOCSTRING(R"dx(If form has is a list of declares ((function-name xxx) ...) or else looks like `(lambda lambda-list [[declaration* | documentation]] (block xxx form*) ) then return XXX)dx")
DOCGROUP(clasp)
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


CL_LAMBDA(declare-list)
CL_DECLARE();
CL_DOCSTRING(R"dx(If form has is a list of declares ((function-name xxx) ...) or else looks like `(lambda lambda-list [[declaration* | documentation]] (block xxx form*) ) then return XXX)dx")
DOCGROUP(clasp)
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

CL_LAMBDA(form &optional default)
CL_DECLARE();
CL_DOCSTRING(R"dx(If form has is a list of declares ((function-name xxx) ...) or else looks like `(lambda lambda-list [[declaration* | documentation]] (block xxx form*) ) then return XXX)dx")
DOCGROUP(clasp)
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
CL_LAMBDA(symbol &optional env)
CL_DECLARE();
CL_DOCSTRING(R"dx(Returns the macro expansion function for a symbol if it exists, or else NIL.)dx")
DOCGROUP(clasp)
CL_DEFUN T_sp ext__symbol_macro(Symbol_sp sym, T_sp env) {
  if (env.nilp()) { // nothing
  } else if (gc::IsA<Environment_sp>(env)) {
    int depth = 0;
    int level = 0;
    bool shadowed = false;
    Function_sp macro;
    bool found = Environment_O::clasp_findSymbolMacro(env, sym, depth, level, shadowed, macro);
    if (found)
      return macro;
  } else if (gc::IsA<BytecodeCmpEnv_sp>(env)) {
    return gc::As_unsafe<BytecodeCmpEnv_sp>(env)->lookupSymbolMacro(sym);
  } else { // pass to cleavir (which also checks global environment)
    SYMBOL_EXPORT_SC_(CorePkg, cleavirSymbolMacro);
    return eval::funcall(core::_sym_cleavirSymbolMacro, sym, env);
  }
  // check global environment
  SYMBOL_SC_(ExtPkg, symbolMacro);
  T_sp fn = nil<T_O>();
  T_mv result = core__get_sysprop(sym, ext::_sym_symbolMacro);
  if (gc::As<T_sp>(result.valueGet_(1)).notnilp()) {
    fn = gc::As<Function_sp>(result);
  }
  return fn;
};
CL_LAMBDA(variables declared-specials)
CL_DECLARE();
CL_DOCSTRING(R"dx(classifyLetVariablesAndDeclares - return (values classified-variables num-lexicals) - For each variable name in variables and declared-specials classify each as special-var, lexical-var or declared-special using the declared-specials list)dx")
DOCGROUP(clasp)
CL_DEFUN
T_mv core__classify_let_variables_and_declares(List_sp variables, List_sp declaredSpecials) {
  HashTableEq_sp specialsSet = HashTableEq_O::create_default();
  for (auto cur : declaredSpecials)
    specialsSet->insert(oCar(cur)); //make(declaredSpecials);
  HashTableEq_sp specialInVariables(HashTableEq_O::create_default());
  HashTable_sp indices = gc::As_unsafe<HashTable_sp>(cl__make_hash_table(cl::_sym_eq, make_fixnum(8),
                                                                         DoubleFloat_O::create(1.5),
                                                                         DoubleFloat_O::create(1.0)));
  ql::list classified;
  size_t indicesSize = 0;
  size_t totalSpecials = 0;
  for (auto cur : variables) {
    Symbol_sp sym = gc::As<Symbol_sp>(oCar(cur));
    if (specialsSet->contains(sym)) {
      classified << Cons_O::create(ext::_sym_specialVar, sym);
      specialInVariables->insert(sym);
      totalSpecials++;
    } else if (sym->specialP()) {
      classified << Cons_O::create(ext::_sym_specialVar, sym);
      specialInVariables->insert(sym);
      totalSpecials++;
    } else {
      Fixnum idx;
      T_sp fi = indices->gethash(sym, unbound<T_O>());
      if (fi.fixnump()) {
        idx = fi.unsafe_fixnum();
      } else {
        idx = indicesSize;
        indices->hash_table_setf_gethash(sym, make_fixnum(idx));
        ++indicesSize;
      }
      classified << Cons_O::create(ext::_sym_lexicalVar,
                                   Cons_O::create(sym, make_fixnum(idx)));
    }
  }
  specialsSet->maphash([&totalSpecials,&classified, &specialInVariables](T_sp s, T_sp val) {
      if ( !specialInVariables->contains(s) ) {
        classified << Cons_O::create(core::_sym_declaredSpecial,s);
        totalSpecials++;
      }
    });
  T_sp tclassified = classified.cons();
  return Values(tclassified, make_fixnum((int)indicesSize), make_fixnum(totalSpecials));
}

CL_LAMBDA(arg)
CL_DECLARE();
CL_DOCSTRING(R"dx(evaluateVerbosity)dx")
DOCGROUP(clasp)
CL_DEFUN void core__evaluate_verbosity(Fixnum_sp level) {
  eval::_evaluateVerbosity = unbox_fixnum(level);
};

CL_LAMBDA(form &optional env)
CL_DECLARE();
CL_UNWIND_COOP(true);
CL_DOCSTRING(R"dx(Evaluate the form in the environment using the interpreter)dx")
DOCGROUP(clasp)
CL_DEFUN T_mv core__interpret_eval_with_env(T_sp form, T_sp environment) {
  return eval::t1Evaluate(form, environment);
}

};

namespace core {

    //void parse_lambda_body(List_sp body, List_sp &declares, gc::Nilable<String_sp> &docstring, List_sp &code);

    List_sp separateTopLevelForms(List_sp accumulated, T_sp possibleForms) {
        if (Cons_sp cpf = possibleForms.asOrNull<Cons_O>()) {
            if (gc::As<Symbol_sp>(oCar(cpf)) == cl::_sym_progn) {
                for (auto cur : (List_sp)oCdr(cpf)) {
                    accumulated = separateTopLevelForms(accumulated, oCar(cur));
                }
                return accumulated;
            }
        }
        accumulated = Cons_O::create(possibleForms, accumulated);
        return accumulated;
    }


    T_sp af_interpreter_lookup_variable(Symbol_sp sym, T_sp env) {
        if (env.notnilp()) {
            int depth, index;
            bool crossesFunction;
            Environment_O::ValueKind valueKind;
            T_sp value;
            T_sp result_env;
            bool found = Environment_O::clasp_findValue(env, sym, depth, index, crossesFunction, valueKind, value, result_env);
            if (found) {
                switch (valueKind) {
                case Environment_O::lexicalValue:
                    return value;
                case Environment_O::specialValue:
                    return sym->symbolValue();
                default:
                    // do nothing;
                    break;
                }
            }
        }
        if (sym->specialP() || sym->boundP()) {
            return sym->symbolValue();
        }
        SIMPLE_ERROR(("Could not find variable %s in lexical/global environment") , _rep_(sym));
    };

    Function_sp interpreter_lookup_function_or_error(T_sp name, T_sp env) {
        unlikely_if (env.notnilp()) {
            Function_sp fn;
            int depth;
            int index;
            T_sp functionEnv;
            if (Environment_O::clasp_findFunction(env, name, depth, index, fn, functionEnv)) return fn;
        }
        if (name.consp()) {
            T_sp head = CONS_CAR(name);
            if (head == cl::_sym_setf) {
                Symbol_sp sym = oCar(CONS_CDR(name)).template as<Symbol_O>();
                if (sym->fboundp_setf()) {
                    return sym->getSetfFdefinition();
                }
                SIMPLE_ERROR(("SETF function value for %s is unbound") , _rep_(sym));
            }
        }
        if (gc::IsA<Symbol_sp>(name)) {
            Symbol_sp sname = gc::As_unsafe<Symbol_sp>(name);
            return sname->symbolFunction();
#if 0    
            if (sname->fboundp()) return sname->symbolFunction();
            SIMPLE_ERROR(("Could not find special-operator/macro/function(%s) in the lexical/dynamic environment") , _rep_(name) );
#endif
        }
        ASSERT(gc::IsA<Function_sp>(name));
        return gc::As_unsafe<Function_sp>(name);
    };

#define ARGS_af_interpreter_lookup_setf_function "(symbol env)"
#define DECL_af_interpreter_lookup_setf_function ""
#define DOCS_af_interpreter_lookup_setf_function "environment_lookup_setf_function"
    T_sp af_interpreter_lookup_setf_function(List_sp setf_name, T_sp env) {
        Symbol_sp name = gc::As<Symbol_sp>(oCadr(setf_name));
        if (env.notnilp()) {
            Function_sp fn;
            int depth;
            int index;
            T_sp functionEnv;
            // TODO: This may not work properly - it looks like it will find regular functions
            if (Environment_O::clasp_findFunction(env, name, depth, index, fn, functionEnv))
                return fn;
        }
        if (name->fboundp_setf())
            return name->getSetfFdefinition();
        return nil<T_O>();
    };


#define ARGS_af_interpreter_lookup_macro "(symbol env)"
#define DECL_af_interpreter_lookup_macro ""
#define DOCS_af_interpreter_lookup_macro "environment_lookup_macro_definition"
    T_sp af_interpreter_lookup_macro(Symbol_sp sym, T_sp env) {
        if (sym.nilp())
            return nil<T_O>();
        if (core__lexical_function(sym, env).notnilp())
            return nil<T_O>();
        int depth = 0;
        int level = 0;
        Function_sp macro;
        bool found = Environment_O::clasp_findMacro(env, sym, depth, level, macro);
        if (found)
            return macro;
        if (sym->fboundp()) {
            if (sym->macroP()) {
                if (Function_sp fn = sym->symbolFunction().asOrNull<Function_O>()) {
                    return fn;
                }
            }
        }
        return nil<T_O>();
    };

    namespace interpret {
        T_mv interpreter_cond(List_sp args, T_sp environment) {
            for (auto cur : args) {
                T_sp cond;
                List_sp condProgn = oCar(cur);
                cond = eval::evaluate(oCar(condProgn), environment);
                if (cond.isTrue()) {
                    List_sp code = oCdr(condProgn);
                    if (code.notnilp()) {
                        return eval::sp_progn(code, environment);
                    }
                    return (Values(cond));
                }
            }
            return Values(nil<T_O>());
        }

        SYMBOL_EXPORT_SC_(ClPkg, case);
        T_mv interpreter_case(List_sp args, T_sp environment) {
            T_sp keyform = oCar(args);
            List_sp clauses = oCdr(args);
            T_sp test_key = eval::evaluate(keyform, environment);
            LOG("Evaluated test_key = %s\n" , _rep_(test_key));
            for (auto cur : clauses) {
                T_sp oclause = oCar(cur);
                if ((oclause).consp()) {
                    List_sp clause = oclause;
                    T_sp keys = oCar(clause);
                    List_sp forms = oCdr(clause);
                    SYMBOL_EXPORT_SC_(ClPkg, otherwise);
                    if (keys == cl::_sym_otherwise || keys == _lisp->_true()) {
                        if (oCdr(cur).notnilp()) {
                            SIMPLE_ERROR(("otherwise-clause must be the last clause of case - it is not"));
                        }
                        return eval::sp_progn(forms, environment);
                    } else if (cl__atom(keys)) {
                        if (cl__eql(keys, test_key)) {
                            return eval::sp_progn(forms, environment);
                        }
                    } else if (keys.consp()) {
                        List_sp lkeys = keys;
                        for (auto kcur : lkeys) {
                            if (cl__eql(oCar(kcur), test_key)) {
                                return eval::sp_progn(forms, environment);
                            }
                        }
                    }
                } else {
                    SIMPLE_ERROR(("Bad case clause: %s") , _rep_(oclause));
                }
            }
            return (Values(nil<T_O>()));
        }

        void setq_symbol_value(Symbol_sp symbol, T_sp value, T_sp environment) {
            if (symbol->specialP() || Environment_O::clasp_lexicalSpecialP(environment, symbol)) {
                if (symbol->getReadOnly())
                    SIMPLE_ERROR(("Cannot modify value of constant %s") , _rep_(symbol));
                symbol->setf_symbolValue(value);
                return;
            } else  {
                bool updated = false;
                if (environment.notnilp()) {
                    updated = clasp_updateValue(environment, symbol, value);
                }
                if (!updated) {
                    symbol->setf_symbolValue(value);
                }
            }
        }

        SYMBOL_EXPORT_SC_(ClPkg, multipleValueSetq);
        T_sp interpreter_multipleValueSetq(List_sp args, T_sp environment) {
            List_sp lcur = oCar(args);
            T_sp form = oCadr(args);
            T_mv result = eval::evaluate(form, environment);
            T_O* primary = result.raw_(); // save for return
            result.saveToMultipleValue0(); // save to iterate over below
            MultipleValues& values = lisp_multipleValues();
            Cons_sp skipFirst = Cons_O::create(nil<T_O>(), nil<T_O>());
            Cons_sp add = skipFirst;
            // Assemble a Cons for sp_setq
            size_t valuesLength = values.getSize();
            int i = 0;
            for (auto cur : lcur) {
                Symbol_sp symbol = gc::As<Symbol_sp>(oCar(cur));
                T_sp value = i < valuesLength ? T_sp((gctools::Tagged)values[i]) : nil<T_O>();
                Cons_sp one = Cons_O::create(symbol, nil<T_O>());
                add->setCdr(one);
                add = one;
                Cons_sp quotedValue = Cons_O::createList(cl::_sym_quote, value);
                Cons_sp two = Cons_O::create(quotedValue, nil<T_O>());
                add->setCdr(two);
                add = two;
                ++i;
            }
            eval::sp_setq(oCdr(skipFirst), environment);
            return T_sp((gctools::Tagged)primary);
        }

        SYMBOL_EXPORT_SC_(ClPkg, prog1);
        T_mv interpreter_prog1(List_sp args, T_sp environment) {
            T_sp firstForm = oCar(args);
            List_sp forms = oCdr(args);
            T_sp result = eval::evaluate(firstForm, environment);
            eval::sp_progn(forms, environment);
            return (Values(result));
        }
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
      SIMPLE_ERROR(("Bad input to processDeclares: %s") , _rep_(inputBody));
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
            SIMPLE_ERROR(("Illegal object[%s] in declare special") , _rep_(v));
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
  LOG("Parsing lambda body: %s" , body->__repr__());
  List_sp specials;
  extract_declares_docstring_code_specials(body, declares, true, docstring, code, specials);
}

T_mv sp_progn(List_sp args, T_sp environment) {
  ASSERT(environment.generalp());
  return eval::evaluateListReturnLast(args, environment);
}

T_mv sp_loadTimeValue(List_sp args, T_sp environment) {
  ASSERT(environment.generalp());
  T_sp form = oCar(args);
  return eval::evaluate(form, nil<T_O>());
}

T_mv do_progv(List_sp symbols, List_sp values, List_sp forms, T_sp environment)
{
  if (symbols.notnilp()) {
    return call_with_variable_bound(gc::As<Symbol_sp>(CONS_CAR(symbols)),
                                    CONS_CAR(values),
                                    [&]() {
                                      return do_progv(CONS_CDR(symbols),
                                                      CONS_CDR(values),
                                                      forms,
                                                      environment);
                                    });
  } else {
    return sp_progn(forms, environment);
  }
}
    
T_mv sp_progv(List_sp args, T_sp environment) {
  ASSERT(environment.generalp());
  List_sp symbols = eval::evaluate(oCar(args), environment);
  List_sp values = eval::evaluate(oCadr(args), environment);
  if (cl__length(symbols) != cl__length(values)) {
    SIMPLE_ERROR(("PROGV number of symbols don't match the number of values"));
  }
  List_sp forms = oCddr(args);
  return do_progv(symbols,values,forms,environment);
}

T_mv sp_debug_message(List_sp args, T_sp env) {
  ASSERT(env.generalp());
  String_sp msg = gc::As<String_sp>(oCar(args));
  printf("+++DEBUG-MESSAGE[%s]\n", msg->get_std_string().c_str());
  return (Values(nil<T_O>()));
}

SYMBOL_EXPORT_SC_(KeywordPkg, execute);
SYMBOL_EXPORT_SC_(KeywordPkg, load_toplevel);
T_mv sp_evalWhen(List_sp args, T_sp environment) {
  ASSERT(environment.generalp());
  List_sp situations = oCar(args);
  List_sp body = oCdr(args);
  bool execute = false;
  if (cl__member(kw::_sym_execute, situations, nil<T_O>(), nil<T_O>(), nil<T_O>()).isTrue()) {
    execute = true;
  }
  if (execute) {
    return sp_progn(body, environment);
  }
  return (Values(nil<T_O>()));
}

T_mv sp_the(List_sp args, T_sp env) {
  ASSERT(env.generalp());
  T_mv val = eval::evaluate(oCadr(args), env);
  return (val);
}

T_mv sp_specialVar(List_sp args, T_sp env) {
  ASSERT(env.generalp());
  Symbol_sp sym = gc::As<Symbol_sp>(oCar(args));
  return Values(sym->symbolValue());
}

T_mv sp_lexicalVar(List_sp args, T_sp env) {
  ASSERT(env.generalp());
  int depth = unbox_fixnum(gc::As<Fixnum_sp>(oCadr(args)));
  int index = unbox_fixnum(gc::As<Fixnum_sp>(oCddr(args)));
  ActivationFrame_sp af = gctools::reinterpret_cast_smart_ptr<ActivationFrame_O>(env);
  T_sp val = core::value_frame_lookup_reference(af,depth,index);
  return Values(val);
}

T_mv sp_locally(List_sp args, T_sp env) {
  ASSERT(env.generalp());
  List_sp declares;
  gc::Nilable<String_sp> docstring;
  List_sp code;
  List_sp specials;
  extract_declares_docstring_code_specials(args, declares, false, docstring, code, specials);
  ValueEnvironment_sp le = ValueEnvironment_O::createForLocallySpecialEntries(specials, env);
            // ignore everything else for now
  return eval::sp_progn(code, le);
}

T_mv sp_eval_when(List_sp args, T_sp env) {
            // The evaluator is always, well, evaluating.
            // So we only need to worry about :execute and cl:eval.
  ASSERT(env.generalp());
  List_sp situations = oCar(args);
  for ( auto cur : situations ) {
    T_sp sit = CONS_CAR(cur);
    if ( !(sit == kw::_sym_execute ||
           sit == kw::_sym_eval ||
           sit == kw::_sym_load_toplevel ||
           sit == cl::_sym_load ||
           sit == kw::_sym_compile_toplevel ||
           sit == cl::_sym_compile ) ) {
      SIMPLE_ERROR(("Illegal eval-when situation %s") , _rep_(sit) );
    }
  }
  List_sp body = oCdr(args);
  bool execute = cl__member(kw::_sym_execute, situations, nil<T_O>(), nil<T_O>(), nil<T_O>()).isTrue();
  execute |= cl__member(cl::_sym_eval, situations, nil<T_O>(), nil<T_O>(), nil<T_O>()).isTrue();
  if (execute)
    return eval::sp_progn(body, env);
  else return Values(nil<T_O>());
};

T_mv sp_step(List_sp args, T_sp env) {
  IMPLEMENT_ME();
};
T_mv sp_tagbody(List_sp args, T_sp env) {
  ASSERT(env.generalp());
  TagbodyEnvironment_sp tagbodyEnv = TagbodyEnvironment_O::make(env);
  ValueFrame_sp vframe = gc::As<ValueFrame_sp>(tagbodyEnv->getActivationFrame());
            //
            // Find all the tags and tell the TagbodyEnvironment where they are in the list of forms.
            //
  for (auto cur : args) {
    T_sp tagOrForm = CONS_CAR(cur);
    if (!tagOrForm.consp() && cl__symbolp(tagOrForm)) {
      Symbol_sp tag = gc::As<Symbol_sp>(tagOrForm);
                    // The tag is associated with its position in list of forms
      tagbodyEnv->addTag(tag, cur);
    }
  }
  LOG("sp_tagbody has extended the environment to: %s" , tagbodyEnv->__repr__());
            // Start to evaluate the tagbody
            // Evaluate the tagbody
  call_with_tagbody([&](TagbodyDynEnv_sp tb, size_t index) {
    List_sp ip;
    if (index == 0) { // initialize and run prefix
      (*vframe)[0] = tb;
      ip = args;
    } else
      // there was a +1 for setjmp in sp_go, so we undo that here.
      ip = tagbodyEnv->codePos(index - 1);
    while (ip.consp()) {
      T_sp tagOrForm = CONS_CAR(ip);
      if ((tagOrForm).consp())
        eval::evaluate(tagOrForm, tagbodyEnv);
      ip = CONS_CDR(ip);
    }
  });
  LOG("Leaving sp_tagbody");
  return Values(nil<T_O>());
};

T_mv sp_go(List_sp args, T_sp env) {
  ASSERT(env.generalp());
  Symbol_sp tag = gc::As<Symbol_sp>(oCar(args));
  int depth = 0;
  int index = 0;
  bool interFunction;
  T_sp tagbodyEnv;
  bool foundTag = Environment_O::clasp_findTag(env, tag, depth, index, interFunction, tagbodyEnv);
  if (!foundTag) {
    SIMPLE_ERROR(("Could not find tag[%s] in the lexical environment: %s") , _rep_(tag) , _rep_(env));
  }
  ValueFrame_sp af = gc::As<ValueFrame_sp>(Environment_O::clasp_getActivationFrame(env));
  T_sp handle = (*af)[0];
  TagbodyDynEnv_sp tde = gc::As_unsafe<TagbodyDynEnv_sp>(handle);
  T_sp tagbodyId = core::tagbody_frame_lookup(af,depth,index);
  sjlj_unwind(tde, index + 1); // +1 for setjmp.
}
};

    namespace eval {

        T_mv sp_let(List_sp args, T_sp parentEnvironment) {
            ASSERT(parentEnvironment.generalp());
            List_sp assignments = oCar(args);
            T_mv pairOfLists = core__separate_pair_list(assignments);
            List_sp variables = coerce_to_list(pairOfLists);
            List_sp expressions = pairOfLists.valueGet_(1);
            List_sp body = oCdr(args);
            //    LOG("Extended the environment - result -->\n%s" , newEnvironment->__repr__() );
            //    LOG("Evaluating code in this new lexical environment: %s" , body->__repr__() );
            List_sp declares;
            gc::Nilable<String_sp> docstring;
            List_sp code;
            List_sp declaredSpecials;
            extract_declares_docstring_code_specials(body, declares, false, docstring, code, declaredSpecials);
            LOG("Assignment part=%s" , assignments->__repr__());
            T_mv classifiedAndCount = core__classify_let_variables_and_declares(variables, declaredSpecials);
            size_t totalSpecials = classifiedAndCount.third().unsafe_fixnum();
            List_sp classified = coerce_to_list(classifiedAndCount);
            int numberOfLexicalVariables = unbox_fixnum(gc::As<Fixnum_sp>(classifiedAndCount.valueGet_(1)));
            ValueEnvironment_sp newEnvironment =
                ValueEnvironment_O::createForNumberOfEntries(numberOfLexicalVariables, parentEnvironment);
            MAKE_SPECIAL_BINDINGS_HOLDER(numSpecials,specialsVLA,totalSpecials);
            ValueEnvironmentDynamicScopeManager scope(numSpecials,specialsVLA,newEnvironment);
            /* KLUDGE: We cheap out and ban use of our unwinder
             * through here. Would be more efficient to undo
             * the VEDSM there, but that's complicated. */
            gctools::StackAllocate<UnknownDynEnv_O> ude;
            gctools::StackAllocate<Cons_O> sa_ec(ude.asSmartPtr(), my_thread->dynEnvStackGet());
            DynEnvPusher dep(my_thread, sa_ec.asSmartPtr());
            ValueFrame_sp valueFrame = gc::As<ValueFrame_sp>(newEnvironment->getActivationFrame());
            // Figure out which environment to evaluate in
            List_sp curExp = expressions;
            T_sp evaluateEnvironment;
            // SPECIFIC TO LET FROM HERE ON DOWN
            evaluateEnvironment = parentEnvironment;
            //		printf("%s:%d In LET\n", __FILE__, __LINE__);

            size_t numTemps = cl__length(classified);
            core::T_O **tempValues = (core::T_O **)__builtin_alloca(sizeof(core::T_O *) * numTemps);
            size_t valueIndex = 0;
            for (auto curClassified : classified) {
                List_sp classified = oCar(curClassified);
                Symbol_sp shead = gc::As<Symbol_sp>(oCar(classified));
                if (shead == ext::_sym_specialVar || shead == ext::_sym_lexicalVar) {
                    T_sp expr = oCar(curExp);
                    T_sp result = eval::evaluate(expr, evaluateEnvironment);
                    if (valueIndex >= numTemps) {
                        SIMPLE_ERROR(("Overflow in LET temporary variables only %d available") , numTemps);
                    }
                    tempValues[valueIndex] = result.raw_();
                    ++valueIndex;
                    curExp = oCdr(curExp);
                }
            }
            valueIndex = 0;
            T_sp result;
            for (auto curClassified : classified) {
                List_sp classified = oCar(curClassified);
                Symbol_sp shead = gc::As<Symbol_sp>(oCar(classified));
                if (shead == ext::_sym_specialVar || shead == ext::_sym_lexicalVar) {
                    if (valueIndex >= numTemps) {
                        SIMPLE_ERROR(("Overflow in LET temporary variables only %d available") , numTemps);
                    }
                    result = gctools::smart_ptr<T_O>((gc::Tagged)tempValues[valueIndex]);
                    ++valueIndex;
                    scope.new_variable(classified, result);
                } else if (shead == _sym_declaredSpecial) {
                    scope.new_special(classified);
                }
            }
            EVO(newEnvironment);
            return eval::sp_progn(code, newEnvironment);
        }

        T_mv sp_letSTAR(List_sp args, T_sp parentEnvironment) {
            ASSERT(parentEnvironment.generalp());
            List_sp assignments = oCar(args);
            T_mv pairOfLists = core__separate_pair_list(assignments);
            List_sp variables = coerce_to_list(pairOfLists);
            List_sp expressions = pairOfLists.valueGet_(1);
            List_sp body = oCdr(args);
            //    LOG("Extended the environment - result -->\n%s" , newEnvironment->__repr__() );
            //    LOG("Evaluating code in this new lexical environment: %s" , body->__repr__() );
            List_sp declares;
            gc::Nilable<String_sp> docstring;
            List_sp code;
            List_sp declaredSpecials;
            extract_declares_docstring_code_specials(body, declares, false, docstring, code, declaredSpecials);
            LOG("Assignment part=%s" , assignments->__repr__());
            T_mv classifiedAndCount = core__classify_let_variables_and_declares(variables, declaredSpecials);
            size_t totalSpecials = classifiedAndCount.third().unsafe_fixnum();
            List_sp classified = coerce_to_list(classifiedAndCount);
            int numberOfLexicalVariables = unbox_fixnum(gc::As<Fixnum_sp>(classifiedAndCount.valueGet_(1)));
            ValueEnvironment_sp newEnvironment =
                ValueEnvironment_O::createForNumberOfEntries(numberOfLexicalVariables, parentEnvironment);
            MAKE_SPECIAL_BINDINGS_HOLDER(numSpecials,specialsVLA,totalSpecials);
            ValueEnvironmentDynamicScopeManager scope(numSpecials,specialsVLA,newEnvironment);
            // See KLUDGE in let, above.
            gctools::StackAllocate<UnknownDynEnv_O> ude;
            gctools::StackAllocate<Cons_O> sa_ec(ude.asSmartPtr(), my_thread->dynEnvStackGet());
            DynEnvPusher dep(my_thread, sa_ec.asSmartPtr());
            ValueFrame_sp valueFrame = gc::As<ValueFrame_sp>(newEnvironment->getActivationFrame());
            // Figure out which environment to evaluate in
            List_sp curExp = expressions;
            T_sp evaluateEnvironment;
            // SPECIFIC TO LET* FROM HERE ON DOWN
            evaluateEnvironment = newEnvironment; // SPECIFIC TO LET*
            T_sp result;
            for (auto curClassified : classified) {
                List_sp cl = CONS_CAR(curClassified);
                Symbol_sp shead = gc::As<Symbol_sp>(oCar(cl));
                if (shead == ext::_sym_specialVar || shead == ext::_sym_lexicalVar) {
                    T_sp expr = oCar(curExp);
                    result = eval::evaluate(expr, evaluateEnvironment);
                    scope.new_variable(cl, result);
                    curExp = oCdr(curExp);
                } else if (shead == _sym_declaredSpecial) {
                    scope.new_special(cl);
                }
            }
            EVO(newEnvironment);
            return eval::sp_progn(code, newEnvironment);
        }

        T_mv sp_if(List_sp args, T_sp environment) {
            ASSERT(environment.generalp());
            if (oCdddr(args).notnilp()) {
                SIMPLE_ERROR(("Illegal if has too many expressions: %s") , _rep_(args));
            }
            T_sp res = eval::evaluate(oCar(args), environment);
            if (res.isTrue()) {
                return eval::evaluate(oCadr(args), environment);
            } else {
                if (oCdr(oCdr(args)).notnilp()) {
                    return eval::evaluate(oCaddr(args), environment);
                }
            }
            return Values(nil<T_O>());
        }

        T_mv sp_cond(List_sp args, T_sp environment) {
            ASSERT(environment.generalp());
            for (auto cur : args) {
                T_sp cond;
                List_sp condProgn;
                {
                    condProgn = oCar(cur);
                    cond = eval::evaluate(oCar(condProgn), environment);
                }
                if (cond.isTrue()) {
                    List_sp code = oCdr(condProgn);
                    if (code.notnilp()) {
                        return eval::sp_progn(code, environment);
                    }
                    return (Values(cond));
                }
            }
            return (Values(nil<T_O>()));
        }

    struct MonitorBlock {
      T_O* _tag;
      MonitorBlock(T_O* tag) : _tag(tag) {
        printf("%s:%d:%s Entered BLOCK %p\n", __FILE__, __LINE__, __FUNCTION__, tag );
      }
      ~MonitorBlock() {
        printf("%s:%d:%s Leaving BLOCK %p\n", __FILE__, __LINE__, __FUNCTION__, this->_tag );
      }
    };
        T_mv sp_block(List_sp args, T_sp environment) {
          ASSERT(environment.generalp());
          Symbol_sp blockSymbol = gc::As<Symbol_sp>(oCar(args));
          BlockEnvironment_sp newEnvironment = BlockEnvironment_O::make(blockSymbol, environment);
          ValueFrame_sp vframe = gc::As<ValueFrame_sp>(newEnvironment->getActivationFrame());
          return call_with_escape([&](BlockDynEnv_sp block) {
            vframe->operator[](0) = block;
            return eval::sp_progn(oCdr(args), newEnvironment);
          });
        }

        T_mv sp_returnFrom(List_sp args, T_sp environment) {
            ASSERT(environment.generalp());
            Symbol_sp blockSymbol = gc::As<Symbol_sp>(oCar(args));
            int depth = 0;
            bool interFunction = false;
            T_sp tblockEnv = nil<T_O>();
            Environment_O::clasp_findBlock(environment,blockSymbol,depth,interFunction,tblockEnv);
            BlockEnvironment_sp blockEnv = gc::As_unsafe<BlockEnvironment_sp>(tblockEnv);
            T_mv result = Values(nil<T_O>());
            if (oCdr(args).notnilp()) result = eval::evaluate(oCadr(args), environment);
            result.saveToMultipleValue0();
            T_sp handle = gc::As_unsafe<ValueFrame_sp>(blockEnv->getActivationFrame())->operator[](0);
            BlockDynEnv_sp bde = gc::As_unsafe<BlockDynEnv_sp>(handle);
            sjlj_unwind(bde, 1); // index irrelevant
        }

        T_mv sp_unwindProtect(List_sp args, T_sp environment) {
          return funwind_protect([&](){return eval::evaluate(oCar(args), environment);},
                                 [&](){eval::sp_progn(oCdr(args), environment);});
        }

        T_mv sp_catch(List_sp args, T_sp environment) {
          ASSERT(environment.generalp());
          return call_with_catch(eval::evaluate(oCar(args), environment),
                                 [&]() { return eval::sp_progn(oCdr(args), environment); });
        }

        T_mv sp_throw(List_sp args, T_sp environment) {
          ASSERT(environment.generalp());
          T_sp throwTag = eval::evaluate(oCar(args), environment);
          T_mv result = eval::evaluate(oCadr(args), environment);
          result.saveToMultipleValue0();
          sjlj_throw(throwTag);
        }

        T_mv sp_multipleValueProg1(List_sp args, T_sp environment) {
            ASSERT(environment.generalp());
            T_mv vals0 = eval::evaluate(oCar(args), environment);
            // Allocate multiple value temporary and save
            size_t nvals = vals0.number_of_values();
            T_O* mv_temp[nvals];
            returnTypeSaveToTemp(nvals, vals0.raw_(), mv_temp);
            // Evaluate the remaining forms
            eval::evaluateListReturnLast(oCdr(args), environment);
            // Restore first form's values
            return returnTypeLoadFromTemp(nvals, mv_temp);
        }

        T_mv sp_multipleValueForeignCall(List_sp args, T_sp env) {
            IMPLEMENT_ME();
        }
        T_mv sp_foreignCall(List_sp args, T_sp env) {
            IMPLEMENT_ME();
        }
        T_mv sp_foreignCallPointer(List_sp args, T_sp env) {
            IMPLEMENT_ME();
        }

        T_mv sp_multipleValueCall(List_sp args, T_sp env) {
            ASSERT(env.generalp());
            T_sp funcdesig = eval::evaluate(oCar(args),env);
            Function_sp func;
            unlikely_if (!gc::IsA<Function_sp>(funcdesig)) {
                unlikely_if (!gc::IsA<Symbol_sp>(funcdesig)) {
                    TYPE_ERROR(funcdesig,Cons_O::createList(cl::_sym_or,cl::_sym_Function_O,cl::_sym_Symbol_O));
                }
                func = gc::As_unsafe<Function_sp>(gc::As_unsafe<Symbol_sp>(funcdesig)->symbolFunction());
            } else {
                func = gc::As_unsafe<Function_sp>(funcdesig);
            }
            MAKE_STACK_FRAME(fargs, MultipleValues::MultipleValuesLimit);
            size_t idx = 0;
            core::MultipleValues& mv = core::lisp_multipleValues();
            for (auto forms : (List_sp)oCdr(args)) {
                T_sp oneForm = oCar(forms);
                T_mv retval = eval::evaluate(oneForm, env);
                if (retval.number_of_values()>0) {
                  gctools::fill_frame_one( fargs, idx, retval.raw_() );
                  gctools::fill_frame_nargs_args( fargs, idx, retval.number_of_values()-1, &mv._Values[1] );
                }
            }
            Vaslist valist_struct(idx,fargs);
            Vaslist_sp valist(&valist_struct); // = valist_struct.fargs.setupVaslist(valist_struct);
            return funcall_general<core::Function_O>(func.tagged_(), valist_struct.remaining_nargs(), valist_struct.args() );
        }


        /*! Parse a lambda expression of the form ([declare*] ["docstring"] body...) */
        Function_sp lambda(T_sp name, bool wrap_block, T_sp lambda_list, List_sp body, T_sp env) {
            List_sp declares;
            gc::Nilable<String_sp> docstring;
            List_sp form;
            parse_lambda_body(body, declares, docstring, form);
            LOG("lambda is closing over environment\n%s" , env->__repr__());
            LambdaListHandler_sp llh;
            if (lambda_list.nilp()) {
                llh = lisp_function_lambda_list_handler(nil<List_V>(), declares);
            } else if ((lambda_list).consp()) {
                llh = lisp_function_lambda_list_handler(lambda_list, declares);
                LOG("Passed lambdaList: %s" , lambda_list->__repr__());
            } else if (core__lambda_list_handler_p(lambda_list)) {
                llh = gc::As<LambdaListHandler_sp>(lambda_list);
            } else {
                SIMPLE_ERROR(("Illegal object for lambda-list you can "
                                "only pass a Cons or LambdaListHandler"));
            }
            // If the name is NIL then check if the form has the form (BLOCK XXX ...)
            // if it does then use XXX as the name
            if (name.nilp()) {
                name = core__extract_lambda_name(form, cl::_sym_lambda);
            }

            List_sp code(form);
            if (wrap_block) {
                code = Cons_O::create(Cons_O::create(cl::_sym_block, Cons_O::create(core__function_block_name(name), code)),nil<T_O>());
            }
            //            printf("%s:%d Creating InterpretedClosure with no source information - fix this\n", __FILE__, __LINE__ );
            T_sp spi(nil<T_O>());
            if (spi.nilp()) {
                if ( _sym_STARcurrentSourcePosInfoSTAR->symbolValue().notnilp() ) {
                    spi = _sym_STARcurrentSourcePosInfoSTAR->symbolValue();
                }
            }
            Closure_sp ic = Closure_O::make_interpreted_closure(name, kw::_sym_function, lambda_list, llh, declares, docstring, code, env, SOURCE_POS_INFO_FIELDS(spi));
            return ic;
        }

        T_mv sp_function(List_sp args, T_sp environment) {
            ASSERT(environment.generalp());
            ASSERTP(oCdr(args).nilp(), "You can provide only one argument - a symbol that has a function bound to it or a lambda");
            T_sp arg = oCar(args);
            if (arg.nilp()) {
                WRONG_TYPE_ARG(arg, Cons_O::createList(cl::_sym_or, cl::_sym_Symbol_O, cl::_sym_Cons_O));
            } else if (Symbol_sp fnSymbol = arg.asOrNull<Symbol_O>()) {
                LOG("In sp_function - looking up for for[%s]" , fnSymbol->__repr__());
                T_sp fn = interpreter_lookup_function_or_error(fnSymbol, environment);
                LOG("     Found form: %s" , fn->__repr__());
                return (Values(fn));
            } else if (Cons_sp cconsArg = arg.asOrNull<Cons_O>()) {
                List_sp consArg = cconsArg;
                T_sp head = oCar(consArg);
                if (head == cl::_sym_setf) {
                    T_sp fn = af_interpreter_lookup_setf_function(consArg, environment);
                    if (fn.nilp()) {
                        SIMPLE_ERROR(("Could not find function %s args: %s") , _rep_(consArg) , _rep_(args));
                    }
                    return (Values(fn));
                } else if (head == cl::_sym_lambda || head == ext::_sym_lambda_block) {
                    T_sp name;
                    List_sp lambdaList;
                    List_sp body;
                    bool wrapBlock = false;
                    if (head == cl::_sym_lambda) {
                        name = core__extract_lambda_name(consArg, cl::_sym_lambda);
                        lambdaList = oCadr(consArg);
                        body = oCddr(consArg);
                        wrapBlock = false;
                    } else // head==cl::_sym_lambda_block
                        {
                            name = core__function_block_name(oCadr(consArg));
                            lambdaList = oCaddr(consArg);
                            body = oCdddr(consArg);
                            wrapBlock = true;
                        }
                    //		    HALT(BF("Check name/lambdaList/body and if ok remove me"));
                    // Create an anonymous function and close it over the current environment
                    Function_sp lambdaFunction = lambda(name,
                                                        wrapBlock,
                                                        lambdaList,
                                                        body,
                                                        environment);
                    return (Values(lambdaFunction));
                }
            }
            SIMPLE_ERROR(("Illegal argument[%s] for function") , _rep_(arg));
        }

        T_mv sp_quote(List_sp args, T_sp environment) {
            ASSERT(environment.generalp());
            ASSERTF(cl__length(args) == 1, BF("Only one argument allowed for QUOTE"));
            return (Values(oCar(args)));
        }

        T_mv sp_setq(List_sp args, T_sp environment) {
            ASSERT(environment.generalp());
            ASSERTP(oCdr(args).notnilp(), "You must provide at least 2 arguments");
            List_sp pairs = args;
            T_sp result = nil<T_O>();
            while (pairs.notnilp()) {
                T_sp target = oCar(pairs);
                if (Symbol_sp symbol = target.asOrNull<Symbol_O>()) {
                    if (oCdr(pairs).nilp()) {
                        SIMPLE_ERROR(("Missing value for setq of target[%s] - body of setq: %s") , _rep_(target) , _rep_(args));
                    }
                    T_sp expr = oCadr(pairs);
                    T_sp texpr = cl__macroexpand(symbol, environment);
                    if (texpr != symbol) {
                        // The target symbol was a symbol-macro so we
                        // switch from SETQ to a SETF to define it
                        eval::evaluate(Cons_O::createList(cl::_sym_setf, texpr, expr), environment);
                    } else {
                        result = eval::evaluate(expr, environment);
                        interpret::setq_symbol_value(symbol, result, environment);
                    }
                    pairs = oCddr(pairs);
                } else {
                    SIMPLE_ERROR(("Illegal target[%s] for setq - body of setq: %s") , _rep_(target) , _rep_(args));
                }
            }
            return (Values(result));
        }

        T_mv sp_flet(List_sp args, T_sp environment) {
            ASSERT(environment.generalp());
            T_sp functionName;
            List_sp functions = oCar(args);
            FunctionValueEnvironment_sp newEnvironment = FunctionValueEnvironment_O::createForEntries(cl__length(functions), environment);
            List_sp body = oCdr(args);
            List_sp cur = functions;
            LOG("functions part=%s" , functions->__repr__());
            while (cur.notnilp()) {
                List_sp oneDef = oCar(cur);
                functionName = oCar(oneDef);
                Function_sp func = lambda(functionName, true, oCadr(oneDef), oCddr(oneDef), environment);
                newEnvironment->bind_function(functionName, func);
                cur = oCdr(cur);
            }
            List_sp declares;
            List_sp code;
            gc::Nilable<String_sp> docstring;
            List_sp specials;
            extract_declares_docstring_code_specials(body, declares, false, docstring, code, specials);
            return eval::sp_progn(code, newEnvironment);
        }

        T_mv sp_labels(List_sp args, T_sp environment) {
            ASSERT(environment.generalp());
            T_sp name;
            List_sp functions = oCar(args);
            List_sp body = oCdr(args);
            List_sp cur = functions;
            LOG("functions part=%s" , functions->__repr__());
            FunctionValueEnvironment_sp newEnvironment = FunctionValueEnvironment_O::createForEntries(cl__length(functions), environment);
            while (cur.notnilp()) {
                List_sp oneDef = oCar(cur);
                name = oCar(oneDef);
                Function_sp func = lambda(name, true, oCadr(oneDef) /*lambda-list*/, oCddr(oneDef) /*body with decls/docstring*/, newEnvironment);
                LOG("func = %s" , func->__repr__());
                newEnvironment->bind_function(name, func);
                cur = oCdr(cur);
            }
            List_sp declares;
            List_sp code;
            gc::Nilable<String_sp> docstring;
            List_sp specials;
            extract_declares_docstring_code_specials(body, declares, false, docstring, code, specials);
            return eval::sp_progn(code, newEnvironment);
        }

        T_mv t1Progn(List_sp args, T_sp environment);

        T_mv doMacrolet(List_sp args, T_sp env, bool toplevel) {
            // TODO: handle trace
            List_sp macros = oCar(args);
            MacroletEnvironment_sp newEnv(MacroletEnvironment_O::make(env));
            List_sp body = oCdr(args);
            List_sp cur = macros;
            LOG("macros part=%s" , macros->__repr__());
            while (cur.notnilp()) {
                List_sp oneDef = oCar(cur);
                //		printf( "%s:%d  oneDef = %s\n", __FILE__, __LINE__, _rep_(oneDef).c_str());
                Symbol_sp name = gc::As<Symbol_sp>(oCar(oneDef));
                T_sp olambdaList = oCadr(oneDef);
                List_sp inner_body = oCdr(oCdr(oneDef));
                List_sp inner_declares;
                gc::Nilable<String_sp> inner_docstring;
                List_sp inner_code;
                parse_lambda_body(inner_body, inner_declares, inner_docstring, inner_code);
                // printf("   name = %s\n", _rep_(name).c_str());
                // printf("   olambdaList = %s\n", _rep_(olambdaList).c_str());
                // printf("   inner_body = %s\n", _rep_(inner_body).c_str());
                // printf("   inner_declares = %s\n", _rep_(inner_declares).c_str());
                // printf("   inner_docstring = %s\n", _rep_(inner_docstring).c_str());
                // printf("   inner_code = %s\n", _rep_(inner_code).c_str());
                List_sp outer_func_cons = eval::funcall(ext::_sym_parse_macro, name, olambdaList, inner_body);
                //		printf("%s:%d sp_macrolet outer_func_cons = %s\n", __FILE__, __LINE__, _rep_(outer_func_cons).c_str());
                Function_sp outer_func;
                List_sp outer_ll = oCadr(outer_func_cons);
                //		printf("%s:%d sp_macrolet outer_ll = %s\n", __FILE__, __LINE__, _rep_(outer_ll).c_str());
                List_sp outer_body = oCddr(outer_func_cons);
                //		printf("%s:%d sp_macrolet outer_body = %s\n", __FILE__, __LINE__, _rep_(outer_body).c_str());
                List_sp declares;
                gc::Nilable<String_sp> docstring;
                List_sp code;
                parse_lambda_body(outer_body, declares, docstring, code);
                LambdaListHandler_sp outer_llh = LambdaListHandler_O::create(outer_ll, declares, cl::_sym_function);
                //    printf("%s:%d Creating InterpretedClosure with no source information - fix this\n", __FILE__, __LINE__);
                Closure_sp ic = Closure_O::make_interpreted_closure(name, kw::_sym_macro, outer_ll, outer_llh, declares, docstring, code, newEnv, SOURCE_POS_INFO_FIELDS(nil<T_O>()));
                outer_func = ic;
                LOG("func = %s" , ic->__repr__());
                newEnv->addMacro(name, outer_func);
                //		newEnv->bind_function(name,outer_func);
                cur = oCdr(cur);
            }
            List_sp declares;
            List_sp code;
            gc::Nilable<String_sp> docstring;
            List_sp specials;
            extract_declares_docstring_code_specials(body, declares, false, docstring, code, specials);
            if (toplevel) {
                return t1Progn(code, newEnv);
            } else {
                return eval::sp_progn(code, newEnv);
            }
        }

        T_mv sp_macrolet(List_sp args, T_sp env) {
            ASSERT(env.generalp());
            return doMacrolet(args, env, false /* toplevel */);
        }

        extern T_mv t1Locally(List_sp args, T_sp env);

        T_mv do_symbolMacrolet(List_sp args, T_sp env, bool topLevelForm) {
            ASSERT(env.generalp());
            List_sp macros = oCar(args);
            SymbolMacroletEnvironment_sp newEnv(SymbolMacroletEnvironment_O::make(env));
            List_sp body = oCdr(args);
            List_sp cur = macros;
            LOG("macros part=%s" , macros->__repr__());
            SYMBOL_SC_(CorePkg, whole);
            SYMBOL_SC_(CorePkg, env);
            List_sp outer_ll = Cons_O::createList(_sym_whole, _sym_env);
            SYMBOL_EXPORT_SC_(ClPkg, ignore);
            List_sp declares = Cons_O::createList(cl::_sym_declare, Cons_O::createList(cl::_sym_ignore, _sym_whole, _sym_env));
            while (cur.notnilp()) {
                List_sp oneDef = oCar(cur);
                Symbol_sp name = gc::As<Symbol_sp>(oCar(oneDef));
                List_sp expansion = Cons_O::create(Cons_O::createList(cl::_sym_quote, oCadr(oneDef)), nil<T_O>());
                LambdaListHandler_sp outer_llh = LambdaListHandler_O::create(outer_ll,
                                                                             oCadr(declares),
                                                                             cl::_sym_function);
                //    printf("%s:%d Creating InterpretedClosure with no source information and empty name- fix this\n", __FILE__, __LINE__);
                Closure_sp ic = Closure_O::make_interpreted_closure(_sym_symbolMacroletLambda, kw::_sym_macro, outer_ll, outer_llh, declares, nil<T_O>(), expansion, newEnv, SOURCE_POS_INFO_FIELDS(nil<T_O>()));
                Function_sp outer_func = ic;
                newEnv->addSymbolMacro(name, outer_func);
                cur = oCdr(cur);
            }
            if (topLevelForm) {
                return t1Locally(body, newEnv);
            } else {
                return eval::sp_locally(body, newEnv);
            }
        }

        T_mv sp_symbolMacrolet(List_sp args, T_sp env) {
            ASSERT(env.generalp());
            return do_symbolMacrolet(args, env, false);
        }

        /*! Returns NIL if no function is found */
        T_sp lookupFunction(T_sp functionDesignator, T_sp env) {
            if (gc::IsA<Symbol_sp>(functionDesignator)) {
                Symbol_sp shead = gc::As<Symbol_sp>(functionDesignator);
                T_sp exec = interpreter_lookup_function_or_error(shead, env);
                return exec;
            }
            ASSERT(gc::IsA<Function_sp>(functionDesignator));
            return functionDesignator;
        }
    
        T_mv evaluate_atom(T_sp exp, T_sp environment) {
            T_mv result;
            LOG("Evaluating atom: %s" , exp->__repr__());
            if (exp.fixnump() || exp.characterp() || exp.single_floatp()) {
#ifdef DEBUG_EVALUATE
              if (_sym_STARdebugEvalSTAR && _sym_STARdebugEvalSTAR->symbolValue().notnilp()) {
          //printf("%s:%d evaluate %s\n", __FILE__, __LINE__, _rep_(exp).c_str());
                printf("%s:%d evaluate_atom returning %s\n", __FILE__, __LINE__, _rep_(exp).c_str());
              }
#endif
                return Values(exp);
            } else if (Symbol_sp sym = exp.asOrNull<Symbol_O>()) {
                if (sym->isKeywordSymbol()) {
#ifdef DEBUG_EVALUATE
              if (_sym_STARdebugEvalSTAR && _sym_STARdebugEvalSTAR->symbolValue().notnilp()) {
          //printf("%s:%d evaluate %s\n", __FILE__, __LINE__, _rep_(exp).c_str());
                printf("%s:%d evaluate_atom returning %s\n", __FILE__, __LINE__, _rep_(sym).c_str());
              }
#endif
                    return Values(sym);
                }
                if (ext__symbol_macro(sym, environment).notnilp()) {
                    T_sp texpr;
                    {
                        texpr = cl__macroexpand(sym, environment);
                    }
                    result = eval::evaluate(texpr, environment);
#ifdef DEBUG_EVALUATE
              if (_sym_STARdebugEvalSTAR && _sym_STARdebugEvalSTAR->symbolValue().notnilp()) {
          //printf("%s:%d evaluate %s\n", __FILE__, __LINE__, _rep_(exp).c_str());
                printf("%s:%d evaluate_atom returning %s\n", __FILE__, __LINE__, _rep_(result).c_str());
              }
#endif
                    return (result);
                }
                result = af_interpreter_lookup_variable(sym, environment);
#ifdef DEBUG_EVALUATE
                if (sym.nilp() && gc::IsA<HashTable_sp>(result)) {
                  printf("%s:%d:%s Hit place where nil @ %p -> HashTable %s\n", __FILE__, __LINE__, __FUNCTION__, (void*)sym.raw_(), _rep_(result).c_str() );
                }
                if (_sym_STARdebugEvalSTAR && _sym_STARdebugEvalSTAR->symbolValue().notnilp()) {
                  printf("%s:%d evaluate variable %s -> %s\n", __FILE__, __LINE__, _rep_(sym).c_str(), _rep_(result).c_str());
                }
#endif                
                return (result);
            }
            LOG(" Its the self returning object: %s" , exp->__repr__());
#ifdef DEBUG_EVALUATE
          if (_sym_STARdebugEvalSTAR && _sym_STARdebugEvalSTAR->symbolValue().notnilp()) {
            printf("%s:%d evaluate return  %s\n", __FILE__, __LINE__, _rep_(exp).c_str());
          }
#endif                
            return (Values(exp));
        }


        T_mv evaluate_specialForm(SpecialForm_sp specialForm, List_sp form, T_sp environment) {
            return specialForm->evaluate(oCdr(form), environment);
        }

        T_mv evaluate_cond(List_sp form, T_sp environment) {
            T_mv result;
            result = interpret::interpreter_cond(oCdr(form), environment);
            //  ASSERTNOTNULL(result);
            return (result);
        }

        T_mv evaluate_case(List_sp form, T_sp environment) {
            T_mv result;
            result = interpret::interpreter_case(oCdr(form), environment);
            ASSERTNOTNULL(result);
            return (result);
        }

        T_mv evaluate_multipleValueSetq(List_sp form, T_sp environment) {
            T_mv result;
            SYMBOL_EXPORT_SC_(ClPkg, multipleValueSetq);
            result = interpret::interpreter_multipleValueSetq(oCdr(form), environment);
            ASSERTNOTNULL(result);
            return (result);
        }

        T_mv evaluate_prog1(List_sp form, T_sp environment) {
            T_mv result;
            SYMBOL_EXPORT_SC_(ClPkg, prog1);
            result = interpret::interpreter_prog1(oCdr(form), environment);
            ASSERTNOTNULL(result);
            return (result);
        }

        SYMBOL_EXPORT_SC_(CompPkg, compileInEnv);

        T_mv t1Progn(List_sp args, T_sp environment) {
#ifdef DEBUG_EVALUATE
            if (_sym_STARdebugEvalSTAR && _sym_STARdebugEvalSTAR->symbolValue().notnilp()) {
                printf("%s:%d t1Progn args: %s\n", __FILE__, __LINE__, _rep_(args).c_str());
            }
#endif
            T_mv result(nil<T_O>());
            T_sp localEnv(environment);
            for (auto cur : args) {
                result = t1Evaluate(oCar(cur), localEnv);
            }
            return result;
        }

        T_mv t1EvalWhen(T_sp args, T_sp environment) {
#ifdef DEBUG_EVALUATE
            if (_sym_STARdebugEvalSTAR && _sym_STARdebugEvalSTAR->symbolValue().notnilp()) {
                printf("%s:%d t1EvalWhen args: %s\n", __FILE__, __LINE__, _rep_(args).c_str());
            }
#endif
            List_sp situations = oCar(args);
            List_sp body = oCdr(args);
            bool execute = cl__member(kw::_sym_execute, situations, nil<T_O>(), nil<T_O>(), nil<T_O>()).isTrue();
            execute |= cl__member(cl::_sym_eval, situations, nil<T_O>(), nil<T_O>(), nil<T_O>()).isTrue();
            if (execute)
                return t1Progn(body, environment);
            return (Values(nil<T_O>()));
        }

    T_mv t1Locally(List_sp args, T_sp env) {
#ifdef DEBUG_EVALUATE
      if (_sym_STARdebugEvalSTAR && _sym_STARdebugEvalSTAR->symbolValue().notnilp()) {
        printf("%s:%d t1Locally args: %s\n", __FILE__, __LINE__, _rep_(args).c_str());
      }
#endif
      List_sp declares;
      gc::Nilable<String_sp> docstring;
      List_sp code;
      List_sp specials;
      extract_declares_docstring_code_specials(args, declares, false, docstring, code, specials);
      ValueEnvironment_sp le = ValueEnvironment_O::createForLocallySpecialEntries(specials, env);
            // ignore everything else for now
      return eval::t1Progn(code, le);
    }

        T_mv t1Macrolet(List_sp args, T_sp env) {
            return doMacrolet(args, env, true /*toplevel*/);
        }

        T_mv t1SymbolMacrolet(List_sp args, T_sp env) {
            return do_symbolMacrolet(args, env, true);
        }

        T_mv t1Evaluate(T_sp exp, T_sp environment) {
            if ((exp).consp()) {
                T_sp head = oCar(exp);
#ifdef DEBUG_EVALUATE
                if (_sym_STARdebugEvalSTAR && _sym_STARdebugEvalSTAR->symbolValue().notnilp()) {
                    printf("%s:%d Checking if top-level head: %s  cl::_sym_eval_when: %s eq=%d    form: %s\n", __FILE__, __LINE__, _rep_(head).c_str(), _rep_(cl::_sym_eval_when).c_str(), (head == cl::_sym_eval_when), _rep_(exp).c_str());
                }
#endif
                // TODO: Deal with Compiler macros here
                T_sp macroFunction(nil<T_O>());
                if (cl__symbolp(head)) {
                    macroFunction = eval::funcall(cl::_sym_macroFunction, head, environment);
                    if (macroFunction.notnilp()) {
                        T_sp expanded = eval::funcall(macroFunction, exp, environment);
                        return t1Evaluate(expanded, environment);
                    } else if (head == cl::_sym_progn) {
                        return t1Progn(oCdr(exp), environment);
                    } else if (head == cl::_sym_eval_when) {
                        //                        printf("%s:%d   head is eval-when\n", __FILE__, __LINE__ );
                        return t1EvalWhen(oCdr(exp), environment);
                    } else if (head == cl::_sym_locally) {
                        return t1Locally(oCdr(exp), environment);
                    } else if (head == cl::_sym_macrolet) {
                        return t1Macrolet(oCdr(exp), environment);
                    } else if (head == cl::_sym_symbol_macrolet) {
                        return t1SymbolMacrolet(oCdr(exp), environment);
                    }
                }
            }
#ifdef DEBUG_EVALUATE
            if (_sym_STARdebugEvalSTAR && _sym_STARdebugEvalSTAR->symbolValue().notnilp()) {
                printf("%s:%d About to compileFormAndEvalWithEnv: %s\n", __FILE__, __LINE__, _rep_(exp).c_str());
            }
#endif
            return eval::funcall(comp::_sym_STARimplicit_compile_hookSTAR->symbolValue(), exp, environment);
        }

    
    T_mv evaluate(T_sp exp, T_sp environment) {
#ifdef DEBUG_EVALUATE
          if (_sym_STARdebugEvalSTAR && _sym_STARdebugEvalSTAR->symbolValue().notnilp()) {
          //printf("%s:%d evaluate %s\n", __FILE__, __LINE__, _rep_(exp).c_str());
            printf("%s:%d evaluate %s\n", __FILE__, __LINE__, _rep_(exp).c_str());
            T_sp localEnv = environment;
            while (localEnv.notnilp()) {
              printf("    in environment@%p -> %s\n", environment.raw_(), Environment_O::clasp_summaryOfContents(localEnv).c_str());
              localEnv = gc::As<Environment_sp>(localEnv)->getParentEnvironment();
            }
          }
#endif
            //            printf("    environment: %s\n", _rep_(environment).c_str() );
          ASSERT(environment.generalp());
          T_mv result;
          List_sp form;
          T_sp head;
          core__stack_monitor();
          if (!exp.consp()) {
            T_sp result = evaluate_atom(exp, environment);
#ifdef DEBUG_EVALUATE
            if (_sym_STARdebugEvalSTAR && _sym_STARdebugEvalSTAR->symbolValue().notnilp()) {
          //printf("%s:%d evaluate %s\n", __FILE__, __LINE__, _rep_(exp).c_str());
              printf("%s:%d evaluate_atom returned  %s\n", __FILE__, __LINE__, _rep_(result).c_str());
            }
#endif              
            return Values(result);
          }
            //
            // If it reached here then exp is a cons
            //
            //	    LOG("Evaluating cons[%s]" , exp->__repr__() );
            //	    printf("    Evaluating: %s\n", _rep_(exp).c_str() );
            //	    printf("    In env: %s\n", _rep_(environment).c_str() );
          Cons_sp cform((gctools::Tagged)exp.raw_());
          form = cform;
          ASSERTNOTNULL(form);
          head = CONS_CAR(form);
          T_sp tail = CONS_CDR(form);
          if (head.consp()) {
            Cons_sp chead((gctools::Tagged)head.raw_());
            if (CONS_CAR(chead)==cl::_sym_lambda) {
#ifdef DEBUG_EVALUATE
              printf("%s:%d %s is lambda\n", __FILE__, __LINE__, _rep_(chead).c_str());
#endif
              return core::eval::evaluate(Cons_O::create(cl::_sym_funcall,exp),environment);
            }
            SIMPLE_ERROR(("Illegal head of form %s") , _rep_(head));
          } else if (Symbol_sp headSym = head.asOrNull<Symbol_O>()) {
                // ------------------------------------------------------------
                //
                // These must EXACTLY match the special operators defined in evaluator.cc::defineSpecialOperatorsAndMacros(...)
                //
            if (headSym == cl::_sym_progn) return sp_progn(tail,environment);
            else if (headSym == cl::_sym_block) return sp_block(tail,environment);
            else if (headSym == cl::_sym_catch) return sp_catch(tail,environment);
            else if (headSym == cl::_sym_eval_when) return sp_eval_when(tail,environment);
            else if (headSym == cl::_sym_flet) return sp_flet(tail,environment);
            else if (headSym == cl::_sym_function) return sp_function(tail,environment);
            else if (headSym == cl::_sym_the) return sp_the(tail,environment);
            else if (headSym == cl::_sym_go) return sp_go(tail,environment);
            else if (headSym == cl::_sym_if) return sp_if(tail,environment);
            else if (headSym == cl::_sym_labels) return sp_labels(tail,environment);
            else if (headSym == cl::_sym_let) return sp_let(tail,environment);
            else if (headSym == cl::_sym_letSTAR) return sp_letSTAR(tail,environment);
            else if (headSym == cl::_sym_locally) return sp_locally(tail,environment);
            else if (headSym == cl::_sym_macrolet) return sp_macrolet(tail,environment);
            else if (headSym == cl::_sym_multiple_value_prog1) return sp_multipleValueProg1(tail,environment);
            else if (headSym == cl::_sym_multiple_value_call) return sp_multipleValueCall(tail,environment);
            else if (headSym == core::_sym_debug_message) return sp_debug_message(tail,environment);
            else if (headSym == core::_sym_multiple_value_foreign_call) return sp_multipleValueForeignCall(tail,environment);
            else if (headSym == core::_sym_foreign_call) return sp_foreignCall(tail,environment);
            else if (headSym == core::_sym_foreign_call_pointer) return sp_foreignCallPointer(tail,environment);
            else if (headSym == cl::_sym_progv) return sp_progv(tail,environment);
            else if (headSym == cl::_sym_quote) return sp_quote(tail,environment);
            else if (headSym == cl::_sym_return_from) return sp_returnFrom(tail,environment);
            else if (headSym == cl::_sym_setq) return sp_setq(tail,environment);
            else if (headSym == cl::_sym_tagbody) return sp_tagbody(tail,environment);
            else if (headSym == cl::_sym_throw) return sp_throw(tail,environment);
            else if (headSym == cl::_sym_unwind_protect) return sp_unwindProtect(tail,environment);
            else if (headSym == cl::_sym_symbol_macrolet) return sp_symbolMacrolet(tail,environment);
            else if (headSym == cl::_sym_load_time_value) return sp_loadTimeValue(tail,environment);
            else if (headSym == ext::_sym_specialVar) return sp_specialVar(tail,environment);
            else if (headSym == ext::_sym_lexicalVar) return sp_lexicalVar(tail,environment);
                // ------------------------------------------------------------

                //
                // The following speeds up a couple of other forms
                //
            if (headSym == cl::_sym_cond) {
              return evaluate_cond(form, environment);
            } else if (headSym == cl::_sym_case) {
              return evaluate_case(form, environment);
            } else if (headSym == cl::_sym_multipleValueSetq) {
              return evaluate_multipleValueSetq(form, environment);
            } else if (headSym == cl::_sym_prog1) {
              return evaluate_prog1(form, environment);
            }

            T_sp theadFunc = af_interpreter_lookup_macro(headSym, environment);
            if (theadFunc.notnilp()) {
#ifdef DEBUG_EVALUATE
              if (_sym_STARdebugEvalSTAR && _sym_STARdebugEvalSTAR->symbolValue().notnilp()) {
                printf("%s:%d %s is macro\n", __FILE__, __LINE__, _rep_(headSym).c_str());
              }
#endif
              T_sp expanded;
                    /* macros are expanded again and again and again */
              if (_sym_STARcache_macroexpandSTAR->symbolValue().notnilp()) {
                HashTableEqual_sp ht = gc::As<HashTableEqual_sp>(_sym_STARcache_macroexpandSTAR->symbolValue());
                T_mv expanded_mv = ht->gethash(form);
                if (expanded_mv.second().notnilp()) {
                  expanded = expanded_mv;
                } else {
                  expanded = cl__macroexpand(form,environment);
                  ht->setf_gethash(form,expanded);
                }
              } else {
                expanded = cl__macroexpand(form, environment);
              }
              return eval::evaluate(expanded, environment);
            }
            theadFunc = interpreter_lookup_function_or_error(headSym, environment);
                //
                // It is a form and its head is a symbol,
                // evaluate the arguments and apply the function bound to the head to them
                //
                //		LOG("Symbol[%s] is a normal form - evaluating arguments" , head->__repr__() );
#ifdef DEBUG_EVALUATE
            if (_sym_STARdebugEvalSTAR && _sym_STARdebugEvalSTAR->symbolValue().notnilp()) {
              printf("%s:%d %s is function\n", __FILE__, __LINE__, _rep_(headSym).c_str());
            }
#endif
            size_t nargs = cl__length(oCdr(form));
            T_sp headFunc = theadFunc;
            MAKE_STACK_FRAME(callArgs, nargs);
            size_t argIdx = 0;
            for (auto cur : (List_sp)oCdr(form)) {
              gctools::fill_frame_one(callArgs,argIdx,eval::evaluate(CONS_CAR(cur), environment).raw_());
            }
#ifdef DEBUG_EVALUATE
            if (_sym_STARdebugEvalSTAR && _sym_STARdebugEvalSTAR->symbolValue().notnilp()) {
              printf("%s:%d evaluate %s is function\n", __FILE__, __LINE__, _rep_(headSym).c_str());
              for (size_t ia=0; ia<argIdx; ++ia) {
                T_sp obj((gctools::Tagged)callArgs->value_(ia));
                printf("    arg[%lu] -> %s\n", ia, _rep_(obj).c_str());
              }
              if (_rep_(headSym)=="REPLACE-ALL-USES-WITH") {
                printf("%s:%d About to hit error\n", __FILE__, __LINE__ );
              }
            }
#endif
            Vaslist valist_struct(nargs,callArgs);
            Vaslist_sp valist(&valist_struct); // = callArgs.setupVaslist(valist_struct);
            return funcall_general<core::Function_O>(headFunc.tagged_(), valist_struct.remaining_nargs(), valist_struct.args() );
          }
          SIMPLE_ERROR(("Illegal form %s") , _rep_(exp));
        }
    
        void evaluateIntoActivationFrame(ActivationFrame_sp af,
                                         List_sp args, T_sp environment) {
            if (args.nilp()) {
                LOG("Arguments before evaluateList: Nil ---> returning Nil");
                return;
            }
            LOG("Arguments before evaluateList: %s" , _rep_(args));
            {
                int idx = 0;
                // Iterate through each car in exp and
                // evaluate it (handling Nil objects and results)
                // and string the results into a linked list
                ValueFrame_sp vframe = af.as<ValueFrame_O>();
                for (auto p : args) {
                    T_sp inObj = oCar(p);
                    T_sp result = eval::evaluate(inObj, environment);
                    LOG("After evaluation result = %s" , _rep_(result));
                    ValueFrame_sp vf = gctools::As_unsafe<ValueFrame_sp>(af);
                    vf->set_entry(idx, result);
                    ++idx;
                }
            }
            LOG("Arguments after evaluateList: %s" , _rep_(af));
        }

        List_sp evaluateList(List_sp args, T_sp environment) {
            Cons_sp firstCons = Cons_O::create(nil<T_O>(),nil<T_O>());
            Cons_sp curCons = firstCons;
            if (args.nilp()) {
                LOG("Arguments before evaluateList: Nil ---> returning Nil");
                return nil<T_O>();
            }
            LOG("Arguments before evaluateList: %s" , args->__repr__());
            {
                // Iterate through each car in exp and
                // evaluate it (handling Nil objects and results)
                // and string the results into a linked list
                for (auto p : args) {
                    T_sp inObj = oCar(p);
                    T_sp result = eval::evaluate(inObj, environment);
                    ASSERTNOTNULL(result);
                    LOG("After evaluation result = %s @ %X" , result->__repr__() , (void *)(result.get()));
                    Cons_sp outCons = Cons_O::create(result,nil<T_O>());
                    curCons->setCdr(outCons);
                    curCons = outCons;
                }
            }
#ifdef DEBUG_ON
            List_sp tempCons = firstCons->cdr();
            while (tempCons.notnilp()) {
                T_sp zobj = oCar(tempCons);
                LOG("Argument after evaluateList in order: %s @ %X" , zobj->__repr__() , (void *)(zobj.get()));
                tempCons = tempCons->cdr();
            }
#endif
            LOG("Arguments after evaluateList: %s" , _rep_(oCdr(firstCons)));
            return oCdr(firstCons);
        }

        T_mv evaluateListReturnLast(List_sp args, T_sp environment) {
            T_sp inObj;
            T_mv outObj;
            outObj = Values(nil<T_O>());
            // Iterate through each car in exp and
            // evaluate it (handling Nil objects and results)
            // and string the results into a linked list
            //
            //
            for (auto p : args) {
                inObj = oCar(p);
                outObj = eval::evaluate(inObj, environment); // used to use newEnvironment
            }
            return outObj;
        }

        SYMBOL_EXPORT_SC_(ClPkg, block);
        SYMBOL_EXPORT_SC_(ClPkg, quote);
        SYMBOL_EXPORT_SC_(ClPkg, progn);
        SYMBOL_EXPORT_SC_(ClPkg, throw);

        bool aclasp_special_operator_p(Symbol_sp headSym) {
            //
            // IMPORTANT!!!! These all need to be tested in evaluator.cc::evaluate(...)
            //
            if (headSym == cl::_sym_progn) return true;
            else if (headSym == cl::_sym_block) return true;
            else if (headSym == cl::_sym_catch) return true;
            else if (headSym == cl::_sym_eval_when) return true;
            else if (headSym == cl::_sym_flet) return true;
            else if (headSym == cl::_sym_function) return true;
            else if (headSym == cl::_sym_the) return true;
            else if (headSym == cl::_sym_go) return true;
            else if (headSym == cl::_sym_if) return true;
            else if (headSym == cl::_sym_labels) return true;
            else if (headSym == cl::_sym_let) return true;
            else if (headSym == cl::_sym_letSTAR) return true;
            else if (headSym == cl::_sym_locally) return true;
            else if (headSym == cl::_sym_macrolet) return true;
            else if (headSym == cl::_sym_multiple_value_prog1) return true;
            else if (headSym == cl::_sym_multiple_value_call) return true;
            else if (headSym == core::_sym_debug_message) return true;
            else if (headSym == core::_sym_multiple_value_foreign_call) return true;
            else if (headSym == core::_sym_foreign_call) return true;
            else if (headSym == core::_sym_foreign_call_pointer) return true;
            else if (headSym == cl::_sym_progv) return true;
            else if (headSym == cl::_sym_quote) return true;
            else if (headSym == cl::_sym_return_from) return true;
            else if (headSym == cl::_sym_setq) return true;
            else if (headSym == cl::_sym_tagbody) return true;
            else if (headSym == cl::_sym_throw) return true;
            else if (headSym == cl::_sym_unwind_protect) return true;
            else if (headSym == cl::_sym_symbol_macrolet) return true;
            else if (headSym == cl::_sym_load_time_value) return true;
            else if (headSym == ext::_sym_specialVar) return true;
            else if (headSym == ext::_sym_lexicalVar) return true;
            return false;
        };
    };


};


namespace core {


CL_DOCSTRING(R"dx(Return a list of all special operators as defined in aclasp)dx")
DOCGROUP(clasp)
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
SYMBOL_EXPORT_SC_(ClPkg, apply);
SYMBOL_EXPORT_SC_(ClPkg, funcall);
SYMBOL_EXPORT_SC_(CorePkg, STAReval_with_env_hookSTAR);
SYMBOL_EXPORT_SC_(CorePkg, interpret_eval_with_env);


};
