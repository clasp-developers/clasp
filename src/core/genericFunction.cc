/*
    File: genericFunction.cc
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

#include <clasp/core/foundation.h>
#include <clasp/core/object.h>
#include <clasp/core/lisp.h>
#include <clasp/core/funcallableInstance.h>
#include <clasp/core/bformat.h>
#include <clasp/core/primitives.h>
#include <clasp/core/evaluator.h>
#include <clasp/core/multipleValues.h>
#include <clasp/core/mpPackage.h>
#include <clasp/core/predicates.h>
#include <clasp/core/cache.h>
#include <clasp/core/symbolTable.h>
#include <clasp/core/genericFunction.h>
#include <clasp/core/wrappers.h>
#include <clasp/llvmo/intrinsics.h>

#define CACHE_METHOD_LOOKUP

namespace core {

SYMBOL_EXPORT_SC_(ClPkg, compute_applicable_methods);
SYMBOL_SC_(ClosPkg, compute_applicable_methods_using_classes);
SYMBOL_SC_(ClosPkg, compute_effective_method_function);

CL_LAMBDA(args);
CL_DECLARE();
CL_DOCSTRING("maybeExpandGenericFunctionArguments: expands first argument into a list if it is a Frame or an ActivationFrame");
CL_DEFUN T_sp core__maybe_expand_generic_function_arguments(T_sp args) {
  if ((args).consp()) {
    T_sp first = oCar(args);
    if (first.nilp()) {
      return args;
    } else if (first.valistp()) {
      VaList_sp vafirst = gc::As<VaList_sp>(first);
      List_sp expanded = _Nil<T_O>();
      size_t nargs = vafirst->remaining_nargs();//LCC_VA_LIST_NUMBER_OF_ARGUMENTS(vafirst);
      for (int i(0), iEnd(nargs); i < iEnd; ++i) {
        T_sp v = vafirst->next_arg();
        expanded = Cons_O::create(v, expanded);
      }
      return cl__nreverse(expanded);
    } else {
      SIMPLE_ERROR(BF("Handle %s") % _rep_(first));
    }
  }
  return args;
}




#if 0
/*! Mimic ECL>>gfun.d fill_spec_vector */
gctools::Vec0<T_sp> &fill_spec_vector(FuncallableInstance_sp gf, gctools::Vec0<T_sp> &vektor, VaList_sp vargs) {
  va_list cargs;
  va_copy(cargs, (*vargs)._Args);
  int narg = vargs->remaining_nargs();//LCC_VA_LIST_NUMBER_OF_ARGUMENTS(vargs);
  // cl_object *argtype = vector->vector.self.t; // ??
  gctools::Vec0<T_sp> &argtype = vektor;
  argtype[0] = gf;
  int spec_no = 1;
  for (T_sp spec_how_list = gf->GFUN_SPECIALIZERS(); spec_how_list.consp(); spec_how_list = cons_cdr(spec_how_list)) {
    List_sp spec_how = cons_car(spec_how_list);
    T_sp spec_type = cons_car(spec_how);
    ASSERT(cons_cdr(spec_how).fixnump());
    int spec_position = unbox_fixnum(cons_cdr(spec_how));
    if (spec_position >= narg) {
      SIMPLE_ERROR(BF("Insufficient arguments for %s - expected specializer argument at "
                      "position %d of specializer-type %s but only %d arguments were passed") %
                   _rep_(gf) % (spec_position + 1) % _rep_(spec_type) % narg);
    } else if (spec_no >= vektor.capacity()) {
      SIMPLE_ERROR(BF("Too many arguments to fill_spec_vector()"));
    }
    // Stassats fixed a bug in ECL eql-specializer dispatch cacheing
    // https://gitlab.com/embeddable-common-lisp/ecl/commit/85165d989a563abdf0e31e14ece2e97b5d821187?view=parallel
    // I'm duplicating the fix here - there is also a change in lisp.cc
    T_sp spec_position_arg = T_sp((gc::Tagged)va_arg(cargs, T_O *));
    List_sp eql_spec = _Nil<core::T_O>();
    if (spec_type.consp() && (eql_spec = spec_type.unsafe_cons()->memberEql(spec_position_arg)).notnilp()) {
// For immediate types we need to make sure that EQL will be true
      argtype[spec_no++] = eql_spec;
    } else {
      Class_sp mc = lisp_instance_class(spec_position_arg);
      argtype[spec_no++] = mc;
      //      argtype[spec_no++] = _Nil<T_O>();
    }
  }
  vektor.unsafe_set_end(spec_no);
  return vektor;
}
#endif

/*! Mimic ECL>>gfun.d fill_spec_vector */
void fill_key_vector(FuncallableInstance_sp gf, SimpleVector_sp key, VaList_sp vargs) {
  va_list cargs;
  va_copy(cargs, (*vargs)._Args);
  int narg = vargs->remaining_nargs();//LCC_VA_LIST_NUMBER_OF_ARGUMENTS(vargs);
  // cl_object *argtype = vector->vector.self.t; // ??
  int spec_no = 0;
  for (T_sp spec_how_list = gf->GFUN_SPECIALIZERS(); spec_how_list.consp(); spec_how_list = cons_cdr(spec_how_list)) {
    if (spec_no>=key->length()) goto DONE;
    List_sp spec_how = CONS_CAR(spec_how_list);
    T_sp spec_type = CONS_CAR(spec_how);
    ASSERT(CONS_CDR(spec_how).fixnump());
    int spec_position = CONS_CDR(spec_how).unsafe_fixnum();
    if (spec_position >= narg) {
      SIMPLE_ERROR(BF("Insufficient arguments for %s - expected specializer argument at "
                      "position %d of specializer-type %s but only %d arguments were passed") %
                   _rep_(gf) % (spec_position + 1) % _rep_(spec_type) % narg);
    }
    // Stassats fixed a bug in ECL eql-specializer dispatch cacheing
    // https://gitlab.com/embeddable-common-lisp/ecl/commit/85165d989a563abdf0e31e14ece2e97b5d821187?view=parallel
    // I'm duplicating the fix here - there is also a change in lisp.cc
    T_sp spec_position_arg = T_sp((gc::Tagged)va_arg(cargs, T_O *));
    List_sp eql_spec = _Nil<core::T_O>();
    if (spec_type.consp() && (eql_spec = spec_type.unsafe_cons()->memberEql(spec_position_arg)).notnilp()) {
// For immediate types we need to make sure that EQL will be true
      (*key)[spec_no++] = eql_spec;
    } else {
      Class_sp mc = lisp_instance_class(spec_position_arg);
      (*key)[spec_no++] = mc;
      //      argtype[spec_no++] = _Nil<T_O>();
    }
  }
 DONE:
  va_end(cargs);
}

CL_DEFUN T_sp clos__memoization_key(FuncallableInstance_sp gf, VaList_sp vargs, Fixnum len) {
  SimpleVector_sp key = SimpleVector_O::make(len /*(*vargs).remaining_nargs()*/ ,_Nil<T_O>());
  fill_key_vector(gf,key,vargs);
  return key;
}

LCC_RETURN invalidated_dispatch(gctools::Tagged tgf, gctools::Tagged tvargs) {
  FuncallableInstance_sp gf(tgf);
  VaList_sp vargs(tvargs);
  // This goes straight to clos::dispatch-miss
  return eval::funcall(clos::_sym_invalidated_dispatch_function,gf,vargs);
}

#if 0
/*! Reproduces functionality in user_function_dispatch */
LCC_RETURN user_function_dispatch(gctools::Tagged tgf, gctools::Tagged tvargs) {
  FuncallableInstance_sp gf(tgf);
  VaList_sp vargs(tvargs);
  Function_sp func = gc::As<Function_sp>(gf->instanceRef(gf->numberOfSlots()-1));
  BFORMAT_T(BF("%s:%d a user-dispatch generic-function %s is being invoked\n") % __FILE__ % __LINE__ % _rep_(gf->functionName()) );
  return core::funcall_consume_valist_<core::Function_O>(func.tagged_(),vargs); // cl__apply(func,vargs).as_return_type();
}
#endif

/*! Reproduces functionality in FEnot_funcallable_vararg */
LCC_RETURN not_funcallable_dispatch(gctools::Tagged tgf, gctools::Tagged tvargs) {
  FuncallableInstance_sp gf(tgf);
  VaList_sp vargs(tvargs);
  SIMPLE_ERROR(BF("Not a funcallable instance %s") % _rep_(gf));
}



CL_DEFUN void core__print_object_address(T_sp obj)
{
  printf("%s:%d print_object %s @%p\n", __FILE__, __LINE__, _rep_(obj).c_str(), (void*)obj.raw_());
}

CL_DEFUN T_sp core__object_address(T_sp obj)
{
  return Pointer_O::create(&*obj);
}

};
