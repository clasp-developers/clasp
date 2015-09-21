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

List_sp listOfObjects(VaList_sp vargs)
{
  core::List_sp list = _Nil<core::T_O>(); 
  va_list cargs; 
  va_copy(cargs,(*vargs)._Args);
  size_t nargs = LCC_VA_LIST_NUMBER_OF_ARGUMENTS(vargs); 
  core::Cons_sp* cur = reinterpret_cast<core::Cons_sp*>(&list); 
  for( int p=0; p<nargs; ++p ) { 
    core::T_sp obj = T_sp((gc::Tagged)va_arg(cargs,T_O*)); 
    *cur = core::Cons_O::create(obj, _Nil<core::T_O>()); 
    cur = reinterpret_cast<core::Cons_sp*>(&(*cur)->_Cdr); 
  } 
//  va_end(cargs);
  return list;
}

List_sp listOfClasses(VaList_sp vargs)
{
  core::List_sp list = _Nil<core::T_O>(); 
  va_list cargs; 
  va_copy(cargs,(*vargs)._Args);
  size_t nargs = LCC_VA_LIST_NUMBER_OF_ARGUMENTS(vargs); 
  core::Cons_sp* cur = reinterpret_cast<core::Cons_sp*>(&list); 
  for( int p=0; p<nargs; ++p ) { 
    core::T_sp obj = T_sp((gc::Tagged)va_arg(cargs,T_O*));
    core::Class_sp cobj = af_classOf(obj);
    *cur = core::Cons_O::create(cobj, _Nil<core::T_O>()); 
    cur = reinterpret_cast<core::Cons_sp*>(&(*cur)->_Cdr); 
  } 
//  va_end(cargs);
  return list;
}

/*! This function copies ECL>>gfun.d generic_compute_applicable_method */
T_mv generic_compute_applicable_method(Instance_sp gf, VaList_sp vargs ) {
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
T_mv restricted_compute_applicable_method(Instance_sp gf, VaList_sp vargs ) {
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
  IMPLEMENT_MEF(BF("Handle new valists"));
#if 0
  if (cl_consp(args)) {
    T_sp first = oCar(args);
    if (first.nilp()) {
      return args;
    } else if (first.valistp()) {
      List_sp expanded = _Nil<T_O>();
      core::T_O **frameImpl(first.unsafe_frame());
      frame::ElementType *values(frame::ValuesArray(frameImpl));
      for (int i(0), iEnd(frame::ValuesArraySize(frameImpl)); i < iEnd; ++i) {
        expanded = Cons_O::create(gctools::smart_ptr<T_O>((gc::Tagged)values[i]), expanded);
      }
      return cl_nreverse(expanded);
    } else {
      SIMPLE_ERROR(BF("Handle %s") % _rep_(first));
    }
  }
  return args;
#endif
}

T_mv compute_applicable_method(Instance_sp gf, VaList_sp vargs) {
  if (gc::As<Instance_sp>(gf)->isgf() == ECL_RESTRICTED_DISPATCH) {
    return restricted_compute_applicable_method(gf, vargs );
  } else {
    return generic_compute_applicable_method(gf, vargs );
  }
}

/*! Mimic ECL>>gfun.d fill_spec_vector */
gctools::Vec0<T_sp>& fill_spec_vector(Instance_sp gf, gctools::Vec0<T_sp>& vektor, VaList_sp vargs) {
  va_list cargs;
  va_copy(cargs,(*vargs)._Args);
#if DEBUG_CLOS >= 2
  printf("MLOG fill_spec_vector - entered with gf  %s\n", gf->GFUN_NAME()->__repr__().c_str());
#endif
  int narg = LCC_VA_LIST_NUMBER_OF_ARGUMENTS(vargs);
  T_sp spec_how_list = gf->GFUN_SPECIALIZERS();
  // cl_object *argtype = vector->vector.self.t; // ??
  gctools::Vec0<T_sp>& argtype = vektor;
  argtype[0] = gf;
  int spec_no = 1;
#if DEBUG_CLOS >= 2
  printf("MLOG fill_spec_vector - writing to argtype[%d] at %p wrote: %lX\n",
         0, argtype[0].px_address(), argtype[0].intptr());
#endif
  for (; spec_how_list.notnilp(); spec_how_list = oCdr(spec_how_list)) {
    List_sp spec_how = oCar(spec_how_list);
    T_sp spec_type = oCar(spec_how);
    int spec_position = unbox_fixnum(gc::As<Fixnum_sp>(oCdr(spec_how)));
    if (spec_position >= narg) {
      SIMPLE_ERROR(BF("Insufficient arguments for %s - expected specializer argument at "
                      "position %d of specializer-type %s but only %d arguments were passed")
                   % _rep_(gf) % (spec_position + 1) % _rep_(spec_type) % narg);
    } else if (spec_no >= vektor.capacity()) {
      SIMPLE_ERROR(BF("Too many arguments to fill_spec_vector()"));
    }
    T_sp spec_position_arg = T_sp((gc::Tagged)va_arg(cargs,T_O*));
    if (!cl_listp(spec_type) ||
        gc::As<Cons_sp>(spec_type)->memberEql(spec_position_arg).nilp()) // Was as_or_nil
    {
      Class_sp mc = lisp_instance_class(spec_position_arg);
#if DEBUG_CLOS >= 2
      printf("MLOG fill_spec_vector argtype[%d] using class_of(args[%d]): %s\n",
             spec_no, spec_position, mc->__repr__().c_str());
#endif
      argtype[spec_no] = mc;
    } else {
#if DEBUG_CLOS >= 2
      printf("MLOG fill_spec_vector argtype[%d] using args[%d]\n", spec_no, spec_position);
#endif
      // For immediate types we need to make sure that EQL will be true
      argtype[spec_no] = spec_position_arg;
    }
#if DEBUG_CLOS >= 2
    printf("MLOG fill_spec_vector - from arg[%d] val=%lX writing to argtype[%d] at %p wrote: %lX --> argtype/%s",
           spec_position, ERROR/*args[spec_position].intptr()*/, spec_no, argtype->operator[](spec_no).px_address(), argtype->operator[](spec_no).intptr(), argtype->operator[](spec_no)->__repr__().c_str());
    if (spec_position_arg.consp()) {
      printf(" arg/CONS...\n");
    } else {
      printf(" arg/%s\n", args[spec_position]->__repr__().c_str());
    }
#endif
    ++spec_no;
  }
  vektor.unsafe_set_end(spec_no);
  return vektor;
}

// Arguments are passed in the multiple_values array
LCC_RETURN standard_dispatch(T_sp gf,VaList_sp arglist) {
  /* Lookup the generic-function/arguments invocation in a cache and if an effective-method
	   exists then use that.   If an effective-method does not exist then calculate it and put it in the cache.

	   Then call the effective method with the saved arguments.
	*/
  gc::tagged_pointer<Cache> cache(_lisp->methodCachePtr());
  gctools::Vec0<T_sp>& vektor = fill_spec_vector(gf, cache->keys(), arglist );
  CacheRecord *e; //gctools::StackRootedPointer<CacheRecord> e;
  try {
    cache->search_cache(e); // e = ecl_search_cache(cache);
  } catch (CacheError &err) {
    printf("%s:%d - There was an CacheError searching the GF cache for the keys"
           "  You should try and get into cache->search_cache to see where the error is\n",
           __FILE__, __LINE__);
    SIMPLE_ERROR(BF("Try #1 generic function cache search error looking for %s") % _rep_(gf));
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
    T_mv mv = compute_applicable_method(gf, arglist);
    func = gc::As<Function_sp>(mv);
    if (mv.valueGet(1).notnilp()) {
      T_sp keys = VectorObjects_O::create(vektor);
      if (e->_key.notnilp()) {
        try {
          cache->search_cache(e); // e = ecl_search_cache(cache);
        } catch (CacheError &err) {
          SIMPLE_ERROR(BF("Try #2 generic function cache search error looking for %s") % _rep_(gf));
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
  return standard_dispatch(gf,vargs);
}

/*! Reproduces functionality in ecl_slot_reader_dispatch */
LCC_RETURN slotReaderDispatch(Instance_sp gf, VaList_sp vargs) {
  IMPLEMENT_MEF(BF("Implement slotReaderDispatch"));
}

/*! Reproduces functionality in ecl_slot_writer_dispatch */
LCC_RETURN slotWriterDispatch(Instance_sp gf, VaList_sp vargs ) {
  IMPLEMENT_MEF(BF("Implement slotWriterDispatch"));
}

/*! Reproduces functionality in user_function_dispatch */
LCC_RETURN userFunctionDispatch(Instance_sp gf, VaList_sp vargs ) {
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
