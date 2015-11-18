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
#define DEBUG_LEVEL_FULL

#include <clasp/core/foundation.h>
#include <clasp/core/object.h>
#include <clasp/core/lisp.h>
#include <clasp/core/instance.h>
#include <clasp/core/primitives.h>
#include <clasp/core/evaluator.h>
#include <clasp/core/multipleValues.h>
#include <clasp/core/str.h>
#include <clasp/core/predicates.h>
#include <clasp/core/vectorObjectsWithFillPtr.h>
#include <clasp/core/cache.h>
#include <clasp/core/symbolTable.h>
#include <clasp/core/genericFunction.h>
#include <clasp/core/wrappers.h>

#define CACHE_METHOD_LOOKUP

namespace core {

/*! Trying to figure out dispatching.
      ecl/src/c/eval.d:68 - calls functions in instance.entry
	case t_instance:
		switch (fun->instance.isgf) {
		case ECL_STANDARD_DISPATCH:
		case ECL_RESTRICTED_DISPATCH:
			return _ecl_standard_dispatch(frame, fun);
		case ECL_USER_DISPATCH:
			fun = fun->instance.slots[fun->instance.length - 1];
                        goto AGAIN;
		case ECL_READER_DISPATCH:
		case ECL_WRITER_DISPATCH:
			return APPLY(narg, fun->instance.entry, sp);
		default:
			FEinvalid_function(fun);
		}


In ecl/src/c/interpreter.d  is the following code
		case t_instance:
			switch (reg0->instance.isgf) {
			case ECL_STANDARD_DISPATCH:
			case ECL_RESTRICTED_DISPATCH:
				reg0 = _ecl_standard_dispatch(frame, reg0);
				break;
			case ECL_USER_DISPATCH:
				reg0 = reg0->instance.slots[reg0->instance.length - 1];
				goto AGAIN;
			case ECL_READER_DISPATCH:
			case ECL_WRITER_DISPATCH:
				the_env->function = reg0;
				reg0 = APPLY(narg, reg0->instance.entry, frame_aux.base);
				break;
			default:
				FEinvalid_function(reg0);
			}
			break;
    */

SYMBOL_EXPORT_SC_(ClPkg, compute_applicable_methods);
SYMBOL_SC_(ClosPkg, compute_applicable_methods_using_classes);
SYMBOL_SC_(ClosPkg, compute_effective_method_function);

List_sp listOfObjects(VaList_sp vargs) {
  core::List_sp list = _Nil<core::T_O>();
  va_list cargs;
  va_copy(cargs, (*vargs)._Args);
  size_t nargs = LCC_VA_LIST_NUMBER_OF_ARGUMENTS(vargs);
  core::Cons_sp *cur = reinterpret_cast<core::Cons_sp *>(&list);
  for (int p = 0; p < nargs; ++p) {
    core::T_sp obj = T_sp((gc::Tagged)va_arg(cargs, T_O *));
    *cur = core::Cons_O::create(obj, _Nil<core::T_O>());
    cur = reinterpret_cast<core::Cons_sp *>(&(*cur)->_Cdr);
  }
  //  va_end(cargs);
  return list;
}

List_sp listOfClasses(VaList_sp vargs) {
  core::List_sp list = _Nil<core::T_O>();
  va_list cargs;
  va_copy(cargs, (*vargs)._Args);
  size_t nargs = LCC_VA_LIST_NUMBER_OF_ARGUMENTS(vargs);
  core::Cons_sp *cur = reinterpret_cast<core::Cons_sp *>(&list);
  for (int p = 0; p < nargs; ++p) {
    core::T_sp obj = T_sp((gc::Tagged)va_arg(cargs, T_O *));
    core::Class_sp cobj = af_classOf(obj);
    *cur = core::Cons_O::create(cobj, _Nil<core::T_O>());
    cur = reinterpret_cast<core::Cons_sp *>(&(*cur)->_Cdr);
  }
  //  va_end(cargs);
  return list;
}

/*! This function copies ECL>>gfun.d generic_compute_applicable_method */
T_mv generic_compute_applicable_method(Instance_sp gf, VaList_sp vargs) {
  /* method not cached */
  //cl_object memoize;
  T_sp memoize;
  T_mv methods = eval::funcall(clos::_sym_compute_applicable_methods_using_classes,
                               gf, listOfClasses(vargs));
  memoize = methods.valueGet(1); // unlikely_if (Null(memoize = env->values[1])) {
  if (memoize.nilp()) {
    List_sp arglist = listOfObjects(vargs);
    //    T_sp arglist = lisp_va_list_toCons(vargs); // used to be frame_to_list
    methods = eval::funcall(cl::_sym_compute_applicable_methods,
                            gf, arglist);
    if (methods.nilp()) {
      SYMBOL_EXPORT_SC_(ClPkg, no_applicable_method);
      T_sp func = eval::funcall(cl::_sym_no_applicable_method,
                                gf, arglist);
      // Why was I setting the first argument to NIL???
      // I could use LCC_VA_LIST_REGISTER_ARG0(vargs) = gctools::tag_nil<T_O*>();
      //    args[0] = (T_O *)(gctools::tag_nil<T_O *>());

      return (Values(func, _Nil<T_O>()));
    }
  }
  methods = eval::funcall(clos::_sym_compute_effective_method_function,
                          gf, gf->GFUN_COMB(), methods);
  return (Values(methods, _lisp->_true()));
}

SYMBOL_SC_(ClosPkg, std_compute_applicable_methods);
SYMBOL_SC_(ClosPkg, std_compute_effective_method);

/*! This function copies ECL>>gfun.d restricted_compute_applicable_method */
T_mv restricted_compute_applicable_method(Instance_sp gf, VaList_sp vargs) {
  Instance_sp igf = gf;
  /* method not cached */
  List_sp arglist = listOfObjects(vargs);
  //  T_sp arglist = lisp_ArgArrayToCons(nargs, args); // used to be frame_to_list
  //  printf("%s:%d  restricted_compute_applicable_method gf: %s  args: %s\n", __FILE__, __LINE__, _rep_(gf).c_str(), _rep_(arglist).c_str());
  T_sp methods = eval::funcall(clos::_sym_std_compute_applicable_methods, igf, arglist);
  if (methods.nilp()) {
    Function_sp func = gc::As<Function_sp>(eval::funcall(cl::_sym_no_applicable_method,
                                                         igf, arglist));
    // Why was I setting the first argument to NIL???
    // I could use LCC_VA_LIST_REGISTER_ARG0(vargs) = gctools::tag_nil<T_O*>();
    //    args[0] = (T_O *)(gctools::tag_nil<T_O *>());
    return (Values(func, _Nil<T_O>()));
  }
  methods = eval::funcall(clos::_sym_std_compute_effective_method, igf, gf->GFUN_COMB(), methods);
  return (Values(methods, _lisp->_true()));
}

#define ARGS_core_maybeExpandGenericFunctionArguments "(args)"
#define DECL_core_maybeExpandGenericFunctionArguments ""
#define DOCS_core_maybeExpandGenericFunctionArguments "maybeExpandGenericFunctionArguments: expands first argument into a list if it is a Frame or an ActivationFrame"
T_sp core_maybeExpandGenericFunctionArguments(T_sp args) {
  if (cl_consp(args)) {
    T_sp first = oCar(args);
    if (first.nilp()) {
      return args;
    } else if (first.valistp()) {
      VaList_sp vafirst = gc::As<VaList_sp>(first);
      List_sp expanded = _Nil<T_O>();
      size_t nargs = LCC_VA_LIST_NUMBER_OF_ARGUMENTS(vafirst);
      for (int i(0), iEnd(nargs); i < iEnd; ++i) {
        T_sp v(LCC_NEXT_ARG(vafirst, i));
        expanded = Cons_O::create(v, expanded);
      }
      return cl_nreverse(expanded);
    } else {
      SIMPLE_ERROR(BF("Handle %s") % _rep_(first));
    }
  }
  return args;
}

T_mv compute_applicable_method(Instance_sp gf, VaList_sp vargs) {
  if (gc::As<Instance_sp>(gf)->isgf() == ECL_RESTRICTED_DISPATCH) {
    return restricted_compute_applicable_method(gf, vargs);
  } else {
    return generic_compute_applicable_method(gf, vargs);
  }
}

/*! Mimic ECL>>gfun.d fill_spec_vector */
gctools::Vec0<T_sp> &fill_spec_vector(Instance_sp gf, gctools::Vec0<T_sp> &vektor, VaList_sp vargs) {
  va_list cargs;
  va_copy(cargs, (*vargs)._Args);
  int narg = LCC_VA_LIST_NUMBER_OF_ARGUMENTS(vargs);
  T_sp spec_how_list = gf->GFUN_SPECIALIZERS();
  // cl_object *argtype = vector->vector.self.t; // ??
  gctools::Vec0<T_sp> &argtype = vektor;
  argtype[0] = gf;
  int spec_no = 1;
  for (; spec_how_list.notnilp(); spec_how_list = oCdr(spec_how_list)) {
    List_sp spec_how = oCar(spec_how_list);
    T_sp spec_type = oCar(spec_how);
    int spec_position = unbox_fixnum(gc::As<Fixnum_sp>(oCdr(spec_how)));
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
    List_sp eql_spec;
    if (cl_listp(spec_type) &&
        (eql_spec = gc::As<Cons_sp>(spec_type)->memberEql(spec_position_arg)).notnilp()) {
// For immediate types we need to make sure that EQL will be true
#if 1
      argtype[spec_no++] = eql_spec;
#else
      argtype[spec_no++] = spec_position_arg;
      argtype[spec_no++] = _lisp->_true();
#endif
    } else {
      Class_sp mc = lisp_instance_class(spec_position_arg);
      argtype[spec_no++] = mc;
      //      argtype[spec_no++] = _Nil<T_O>();
    }
  }
  vektor.unsafe_set_end(spec_no);
  return vektor;
}

// Arguments are passed in the multiple_values array
LCC_RETURN standard_dispatch(T_sp gf, VaList_sp arglist, gc::tagged_pointer<Cache> cache) {
  /* Lookup the generic-function/arguments invocation in a cache and if an effective-method
	   exists then use that.   If an effective-method does not exist then calculate it and put it in the cache.
	   Then call the effective method with the saved arguments.
	*/
  gctools::Vec0<T_sp> &vektor = fill_spec_vector(gf, cache->keys(), arglist);
  CacheRecord *e; //gctools::StackRootedPointer<CacheRecord> e;
  try {
    cache->search_cache(e); // e = ecl_search_cache(cache);
  } catch (CacheError &err) {
    printf("%s:%d - There was an CacheError searching the GF cache for the keys"
           "  You should try and get into cache->search_cache to see where the error is\n",
           __FILE__, __LINE__);
    abort();
    //SIMPLE_ERROR(BF("Try #1 generic function cache search error looking for %s") % _rep_(gf));
  }
  ASSERT(e != NULL);
  Function_sp func;
  if (e->_key.notnilp()) {
    func = gc::As<Function_sp>(e->_value);
  } else {
    /* The keys and the cache may change while we
	     * compute the applicable methods. We must save
	     * the keys and recompute the cache location if
	     * it was filled. */
    T_sp keys = VectorObjects_O::create(vektor);
    T_mv mv = compute_applicable_method(gf, arglist);
    func = gc::As<Function_sp>(mv);
    if (mv.valueGet(1).notnilp()) {
      if (e->_key.notnilp()) {
        try {
          cache->search_cache(e); // e = ecl_search_cache(cache);
        } catch (CacheError &err) {
          printf("%s:%d - There was an CacheError searching the GF cache for the keys"
                 "  You should try and get into cache->search_cache to see where the error is\n",
                 __FILE__, __LINE__);
          abort();
          //          SIMPLE_ERROR(BF("Try #2 generic function cache search error looking for %s") % _rep_(gf));
        }
      }
      e->_key = keys;
      e->_value = func;
    }
  }
  return eval::funcall(func, arglist, _Nil<T_O>());
}

/*! Reproduces functionality in generic_function_dispatch_vararg
static cl_object
generic_function_dispatch_vararg(cl_narg narg, ...)
{
        cl_object output;
        ECL_STACK_FRAME_VARARGS_BEGIN(narg, narg, frame) {
		output = _ecl_standard_dispatch(frame, frame->frame.env->function);
	} ECL_STACK_FRAME_VARARGS_END(frame);
        return output;
}
 */
LCC_RETURN generic_function_dispatch(Instance_sp gf, VaList_sp vargs) {
  gc::tagged_pointer<Cache> cache(_lisp->methodCachePtr());
  return standard_dispatch(gf, vargs, cache);
}

/*! Reproduces functionality in ecl_slot_reader_dispatch */
LCC_RETURN slotReaderDispatch(Instance_sp gf, VaList_sp vargs) {
  gc::tagged_pointer<Cache> cache(_lisp->slotCachePtr());
  // Should I use standard_dispatch or do something special for slots?
  return standard_dispatch(gf, vargs, cache);
}

/*! Reproduces functionality in ecl_slot_writer_dispatch */
LCC_RETURN slotWriterDispatch(Instance_sp gf, VaList_sp vargs) {
  gc::tagged_pointer<Cache> cache(_lisp->slotCachePtr());
  // Should I use standard_dispatch or do something special for slots?
  return standard_dispatch(gf, vargs, cache);
}

/*! Reproduces functionality in user_function_dispatch */
LCC_RETURN userFunctionDispatch(Instance_sp gf, VaList_sp vargs) {
  IMPLEMENT_MEF(BF("Implement userFunctionDispatch"));
}

/*! Reproduces functionality in FEnot_funcallable_vararg */
LCC_RETURN notFuncallableDispatch(Instance_sp gf, VaList_sp vargs) {
  IMPLEMENT_MEF(BF("Implement notFuncallableDispatch"));
}

#define ARGS_af_clearGfunHash "(what)"
#define DECL_af_clearGfunHash ""
#define DOCS_af_clearGfunHash "See ecl/src/c/gfun.d:si_clear_gfun_hash. This function clears the generic function call hashes selectively. If what=T then clear the hash completely.  If what=generic_function then clear only these entries."
void af_clearGfunHash(T_sp what) {
  _G();
  ASSERT(_lisp->methodCachePtr());
  ASSERT(_lisp->slotCachePtr());
  _lisp->methodCachePtr()->removeOne(what);
  _lisp->slotCachePtr()->removeOne(what);
};

void initialize_genericFunction() {
  SYMBOL_SC_(ClosPkg, clearGfunHash);
  Defun(clearGfunHash);
  CoreDefun(maybeExpandGenericFunctionArguments);
}
};
