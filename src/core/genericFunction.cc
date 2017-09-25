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

List_sp listOfObjects(VaList_sp vargs) {
  core::List_sp list = _Nil<core::T_O>();
  va_list cargs;
  va_copy(cargs, (*vargs)._Args);
  size_t nargs = vargs->remaining_nargs();//LCC_VA_LIST_NUMBER_OF_ARGUMENTS(vargs);
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
  size_t nargs = vargs->remaining_nargs();//LCC_VA_LIST_NUMBER_OF_ARGUMENTS(vargs);
  core::Cons_sp *cur = reinterpret_cast<core::Cons_sp *>(&list);
  for (int p = 0; p < nargs; ++p) {
    core::T_sp obj = T_sp((gc::Tagged)va_arg(cargs, T_O *));
    core::Class_sp cobj = cl__class_of(obj);
    *cur = core::Cons_O::create(cobj, _Nil<core::T_O>());
    cur = reinterpret_cast<core::Cons_sp *>(&(*cur)->_Cdr);
  }
  //  va_end(cargs);
  return list;
}

/*! This function copies ECL>>gfun.d generic_compute_applicable_method */
T_mv generic_compute_applicable_method(FuncallableInstance_sp gf, VaList_sp args) {
  /* method not cached */
  //cl_object memoize;
  T_sp memoize;
  T_mv methods = eval::funcall(clos::_sym_compute_applicable_methods_using_classes, gf, listOfClasses(args));
  memoize = methods.valueGet_(1); // unlikely_if (Null(memoize = env->values[1])) {
  if (memoize.nilp()) {
    //    T_sp arglist = lisp_va_list_toCons(vargs); // used to be frame_to_list
    methods = eval::funcall(cl::_sym_compute_applicable_methods, gf, listOfObjects(args));
    if (methods.nilp()) {
      SYMBOL_EXPORT_SC_(ClPkg, no_applicable_method);
      T_sp func = eval::applyLastArgsPLUSFirst(cl::_sym_no_applicable_method, listOfObjects(args), gf);
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
T_mv restricted_compute_applicable_method(FuncallableInstance_sp gf, VaList_sp args) {
  FuncallableInstance_sp igf = gf;
  /* method not cached */
  //  printf("%s:%d  restricted_compute_applicable_method gf: %s  args: %s\n", __FILE__, __LINE__, _rep_(gf).c_str(), _rep_(arglist).c_str());
  T_sp methods = eval::funcall(clos::_sym_std_compute_applicable_methods, igf, listOfObjects(args));
  if (methods.nilp()) {
    T_sp func = eval::applyLastArgsPLUSFirst(cl::_sym_no_applicable_method, listOfObjects(args), igf);
    // Why was I setting the first argument to NIL???
    // I could use LCC_VA_LIST_REGISTER_ARG0(vargs) = gctools::tag_nil<T_O*>();
    //    args[0] = (T_O *)(gctools::tag_nil<T_O *>());
    return (Values(func, _Nil<T_O>()));
  }
  methods = eval::funcall(clos::_sym_std_compute_effective_method, igf, gf->GFUN_COMB(), methods);
  return (Values(methods, _lisp->_true()));
}

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

T_mv compute_applicable_method(FuncallableInstance_sp gf, VaList_sp vargs) {
  if (gc::As<FuncallableInstance_sp>(gf)->isgf() == CLASP_RESTRICTED_DISPATCH) {
    return restricted_compute_applicable_method(gf, vargs);
  } else {
    return generic_compute_applicable_method(gf, vargs);
  }
}

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


CL_DEFUN T_sp clos__memoization_key(FuncallableInstance_sp gf, VaList_sp vargs) {
  gctools::Vec0<T_sp> key;
  key.resize((*vargs).remaining_nargs(),_Nil<T_O>());
  fill_spec_vector(gf,key,vargs);
  return SimpleVector_O::make(key.size()-1,_Nil<T_O>(),true,key.size()-1,&key[1]);
}

// Arguments are passed in the multiple_values array
LCC_RETURN standard_dispatch(T_sp gf, VaList_sp arglist, Cache_sp cache) {
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
    func = gc::reinterpret_cast_smart_ptr<Function_O>(e->_value);
  } else {
    /* The keys and the cache may change while we
	     * compute the applicable methods. We must save
	     * the keys and recompute the cache location if
	     * it was filled. */
    T_sp keys = SimpleVector_O::make(vektor.size(),_Nil<T_O>(),true,vektor.size(),&(vektor[0])); // VectorObjects_O::create(vektor);
    T_mv mv = compute_applicable_method(gf, arglist);
    func = Function_sp((gc::Tagged)mv.raw_());
    if (mv.valueGet_(1).notnilp()) {
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
      // Save the results in the call history for later optimization
      // Strip the first element of the key - which from ECL is the generic function
      T_sp call_history_key = SimpleVector_O::make(vektor.size()-1,_Nil<T_O>(),true,vektor.size()-1,&(vektor[1]));
#ifdef DEBUG_GFDISPATH
  if (_sym_STARdebug_dispatchSTAR->symbolValue().notnilp()) {
      printf("%s:%d call_history_key -> %s\n", __FILE__, __LINE__, _rep_(call_history_key).c_str());
  }
#endif
    }
  }
  return cc_dispatch_effective_method(func.raw_(),gf.raw_(), arglist.raw_());
//  return eval::funcall(func, arglist, _Nil<T_O>());
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
LCC_RETURN generic_function_dispatch(gctools::Tagged tgf, gctools::Tagged tvargs) {
  FuncallableInstance_sp gf(tgf);
  VaList_sp vargs(tvargs);
  Cache_sp cache = my_thread->_MethodCachePtr;
  return standard_dispatch(gf, vargs, cache);
}

LCC_RETURN invalidated_dispatch(gctools::Tagged tgf, gctools::Tagged tvargs) {
  FuncallableInstance_sp gf(tgf);
  VaList_sp vargs(tvargs);
  // This goes straight to clos::dispatch-miss
  return eval::funcall(clos::_sym_invalidated_dispatch_function,gf,vargs);
}

#if 0
/*! Reproduces functionality in ecl_slot_reader_dispatch */
LCC_RETURN slot_reader_dispatch(gctools::Tagged tgf, gctools::Tagged tvargs) {
  FuncallableInstance_sp gf(tgf);
  VaList_sp vargs(tvargs);
  Cache_sp cache = _lisp->slotCachePtr();
  // Should I use standard_dispatch or do something special for slots?
  return standard_dispatch(gf, vargs, cache);
}

/*! Reproduces functionality in ecl_slot_writer_dispatch */
LCC_RETURN slot_writer_dispatch(FuncallableInstance_sp gf, VaList_sp vargs) {
  Cache_sp cache = _lisp->slotCachePtr();
  // Should I use standard_dispatch or do something special for slots?
  return standard_dispatch(gf, vargs, cache);
}
#endif

/*! Reproduces functionality in user_function_dispatch */
LCC_RETURN user_function_dispatch(gctools::Tagged tgf, gctools::Tagged tvargs) {
  FuncallableInstance_sp gf(tgf);
  VaList_sp vargs(tvargs);
  Function_sp func = gc::As<Function_sp>(gf->instanceRef(gf->numberOfSlots()-1));
  BFORMAT_T(BF("%s:%d a user-dispatch generic-function %s is being invoked\n") % __FILE__ % __LINE__ % _rep_(gf->functionName()) );
  return core::funcall_consume_valist_<core::Function_O>(func.tagged_(),vargs); // cl__apply(func,vargs).as_return_type();
}

/*! Reproduces functionality in FEnot_funcallable_vararg */
LCC_RETURN not_funcallable_dispatch(gctools::Tagged tgf, gctools::Tagged tvargs) {
  FuncallableInstance_sp gf(tgf);
  VaList_sp vargs(tvargs);
  SIMPLE_ERROR(BF("Not a funcallable instance %s") % _rep_(gf));
}

CL_LAMBDA(what);
CL_DECLARE();
CL_DOCSTRING("See ecl/src/c/gfun.d:si_clear_gfun_hash. This function clears the generic function call hashes selectively. If what=T then clear the hash completely.  If what=generic_function then clear only these entries.");
CL_DEFUN void core__clear_gfun_hash(T_sp what) {
#ifdef CLASP_THREADS
  List_sp processes = _lisp->processes();
  for ( auto cur : processes ) {
    mp::Process_sp process = gc::As<mp::Process_sp>(CONS_CAR(cur));
    ThreadLocalState* other_thread = process->_ThreadInfo;
    if (other_thread != my_thread) {
      other_thread->_MethodCachePtr->removeOne(what);
      other_thread->_SlotCachePtr->removeOne(what);
    }
  }
#endif
  my_thread->_MethodCachePtr->removeOne(what);
  my_thread->_SlotCachePtr->removeOne(what);
};

};


namespace core {
//#define DEBUG_APPLY_METHOD
#ifdef DEBUG_APPLY_METHOD
void describe_apply_method(T_O* func_tagged, int nargs, T_O* arg0_tagged, T_O* arg1_tagged, T_O* rest_args_tagged, int call_type ) {
  Function_sp func((gctools::Tagged)func_tagged);
  T_sp arg0((gctools::Tagged)arg0_tagged);
  T_sp arg1((gctools::Tagged)arg1_tagged);
  T_sp rest_args((gctools::Tagged)rest_args_tagged);
  stringstream scmd;
  if (call_type<0) {
    scmd << "call-next-method.";
    scmd << -call_type;
  } else {
    scmd << "apply-method";
    scmd << call_type;
  }
  stringstream sarg0;
  sarg0 << _rep_(arg0);
  if (arg0.valistp()) {
    VaList_sp varg0 = gc::As_unsafe<VaList_sp>(arg0);
    sarg0 << " " << _rep_(core__list_from_va_list(varg0));
  }
  stringstream srest_args;
  srest_args << _rep_(rest_args);
  if (rest_args.valistp()) {
    VaList_sp vrest_args = gc::As_unsafe<VaList_sp>(rest_args);
    srest_args << " " << _rep_(core__list_from_va_list(vrest_args));
  }
  printf("%s:%d>>> %s  %s\n", __FILE__, __LINE__, scmd.str().c_str(), _rep_(func).c_str());
  printf("        to %d arguments\n", nargs );
  printf("        method_args: %s\n", sarg0.str().c_str());
  if (arg0.valistp()) {
    dump_VaList_S_ptr(&*arg0);
  }
  printf("        next_methods: %s\n", _rep_(arg1).c_str() );
  printf("        rest_args: %s\n", srest_args.str().c_str() );
  if (rest_args.valistp()) {
    dump_VaList_S_ptr(&*rest_args);
  }
}
#endif

LCC_RETURN apply_method(T_O* func_tagged, T_O* arg0_tagged, T_O* arg1_tagged, T_O* rest_args_tagged, int call_type=0) {
//  int lenTotalArgs = args->total_nargs();
//  if (lenTotalArgs == 0) eval::errorApplyZeroArguments();
  T_O* lastArgRaw = rest_args_tagged;
  int lenTotalArgs;
  LIKELY_if (gctools::tagged_valistp(rest_args_tagged)) {
    VaList_sp rest_args_as_VaList_sp((gctools::Tagged)rest_args_tagged);
    VaList_S va_rest_copy_S(*rest_args_as_VaList_sp);
    VaList_sp va_rest_args(&va_rest_copy_S);
    lenTotalArgs = 2+va_rest_args->remaining_nargs();
#ifdef DEBUG_APPLY_METHOD
    describe_apply_method(func_tagged,lenTotalArgs,arg0_tagged,arg1_tagged,rest_args_tagged,call_type);
#endif
    MAKE_STACK_FRAME( frame, func_tagged, lenTotalArgs);
    size_t idx = 0;
    (*frame)[0] = arg0_tagged;
    (*frame)[1] = arg1_tagged;
    for (int i(2); i < lenTotalArgs; ++i) {
      (*frame)[i] = va_rest_args->next_arg_raw();
    }
    VaList_S valist_struct(frame);
    VaList_sp valist(&valist_struct); // = frame.setupVaList(valist_struct);;
#if 0
    printf("%s:%d:%s   func_tagged = %p \n", __FILE__, __LINE__, __FUNCTION__, (void*)func_tagged);
    for (int i(0); i<lenTotalArgs; ++i ) {
      printf("       %s:%d:%s   (*frame)[%d]=%p\n", __FILE__,__LINE__,__FUNCTION__,i,(*frame)[i]);
    }
#endif
    return funcall_consume_valist_<core::Function_O>((gctools::Tagged)func_tagged, valist);
  } else if (gctools::tagged_consp(rest_args_tagged)) {
    Cons_sp cons_rest_args((gctools::Tagged)rest_args_tagged);
    List_sp list_rest_args((gctools::Tagged)rest_args_tagged);
    lenTotalArgs = 2+cons_rest_args->length();
#ifdef DEBUG_APPLY_METHOD
    describe_apply_method(func_tagged,lenTotalArgs,arg0_tagged,arg1_tagged,rest_args_tagged,call_type);
#endif
    MAKE_STACK_FRAME( frame, func_tagged, lenTotalArgs);
    (*frame)[0] = arg0_tagged;
    (*frame)[1] = arg1_tagged;
    for (int i(2); i<lenTotalArgs; ++i ) {
      (*frame)[i] = oCar(list_rest_args).raw_();
      list_rest_args = oCdr(list_rest_args);
    }
    VaList_S valist_struct(frame);
    VaList_sp valist(&valist_struct); // = frame.setupVaList(valist_struct);;
#if 0
    printf("%s:%d:%s   func_tagged = %p \n", __FILE__, __LINE__, __FUNCTION__, (void*)func_tagged);
    for (int i(0); i<lenTotalArgs; ++i ) {
      printf("       %s:%d:%s   (*frame)[%d]=%p\n", __FILE__,__LINE__,__FUNCTION__,i,(*frame)[i]);
    }
#endif
    return funcall_consume_valist_<core::Function_O>((gctools::Tagged)func_tagged, valist);
  }
  lenTotalArgs = 2;
#ifdef DEBUG_APPLY_METHOD
  describe_apply_method(func_tagged,lenTotalArgs,arg0_tagged,arg1_tagged,rest_args_tagged,call_type);
#endif
  Function_O* func = reinterpret_cast<Function_O*>(gctools::untag_general(func_tagged));
//  printf("%s:%d  func = %p  func_tagged = %p  arg0_tagged=%p arg1_tagged=%p\n", __FILE__, __LINE__, (void*)func, (void*)func_tagged, (void*)arg0_tagged, (void*)arg1_tagged);
  return (*func).entry(LCC_PASS_ARGS2_ELLIPSIS(func_tagged,arg0_tagged, arg1_tagged));
}


CL_DEFUN void core__print_object_address(T_sp obj)
{
  printf("%s:%d print_object %s @%p\n", __FILE__, __LINE__, _rep_(obj).c_str(), (void*)obj.raw_());
}

CL_DEFUN T_sp core__object_address(T_sp obj)
{
  return Pointer_O::create(&*obj);
}

extern "C" {


LCC_RETURN apply_method0(T_O* func_tagged, T_O* arg0_tagged, T_O* arg1_tagged, T_O* rest_args_tagged) {
//  printf("%s:%d:%s   func_tagged=%p arg0_tagged=%p arg1_tagged=%p rest_args_tagged=%p\n", __FILE__, __LINE__, __FUNCTION__, (void*)func_tagged, (void*)arg0_tagged, (void*)arg1_tagged, (void*)rest_args_tagged);
  return apply_method(func_tagged,arg0_tagged,arg1_tagged,rest_args_tagged,0);
}
LCC_RETURN apply_method1(T_O* func_tagged, T_O* arg0_tagged, T_O* arg1_tagged, T_O* rest_args_tagged) {
  return apply_method(func_tagged,arg0_tagged,arg1_tagged,rest_args_tagged,1);
}
LCC_RETURN apply_method2(T_O* func_tagged, T_O* arg0_tagged, T_O* arg1_tagged, T_O* rest_args_tagged) {
  return apply_method(func_tagged,arg0_tagged,arg1_tagged,rest_args_tagged,2);
}

LCC_RETURN apply_method3(T_O* func_tagged, T_O* arg0_tagged, T_O* arg1_tagged, T_O* rest_args_tagged) {
  return apply_method(func_tagged,arg0_tagged,arg1_tagged,rest_args_tagged,3);
}
LCC_RETURN apply_method4(T_O* func_tagged, T_O* arg0_tagged, T_O* arg1_tagged, T_O* rest_args_tagged) {
  return apply_method(func_tagged,arg0_tagged,arg1_tagged,rest_args_tagged,4);
}
LCC_RETURN apply_method5(T_O* func_tagged, T_O* arg0_tagged, T_O* arg1_tagged, T_O* rest_args_tagged) {
  return apply_method(func_tagged,arg0_tagged,arg1_tagged,rest_args_tagged,5);
}
LCC_RETURN apply_method6(T_O* func_tagged, T_O* arg0_tagged, T_O* arg1_tagged, T_O* rest_args_tagged) {
  return apply_method(func_tagged,arg0_tagged,arg1_tagged,rest_args_tagged,6);
}
LCC_RETURN apply_method7(T_O* func_tagged, T_O* arg0_tagged, T_O* arg1_tagged, T_O* rest_args_tagged) {
  return apply_method(func_tagged,arg0_tagged,arg1_tagged,rest_args_tagged,7);
}
LCC_RETURN apply_method8(T_O* func_tagged, T_O* arg0_tagged, T_O* arg1_tagged, T_O* rest_args_tagged) {
  return apply_method(func_tagged,arg0_tagged,arg1_tagged,rest_args_tagged,8);
}
LCC_RETURN apply_method9(T_O* func_tagged, T_O* arg0_tagged, T_O* arg1_tagged, T_O* rest_args_tagged) {
  return apply_method(func_tagged,arg0_tagged,arg1_tagged,rest_args_tagged,9);
}
LCC_RETURN apply_method10(T_O* func_tagged, T_O* arg0_tagged, T_O* arg1_tagged, T_O* rest_args_tagged) {
  return apply_method(func_tagged,arg0_tagged,arg1_tagged,rest_args_tagged,10);
}
LCC_RETURN apply_method11(T_O* func_tagged, T_O* arg0_tagged, T_O* arg1_tagged, T_O* rest_args_tagged) {
  return apply_method(func_tagged,arg0_tagged,arg1_tagged,rest_args_tagged,11);
}

LCC_RETURN apply_call_next_method1(T_O* func_tagged, T_O* arg0_tagged, T_O* arg1_tagged, T_O* rest_args_tagged) {
  return apply_method(func_tagged,arg0_tagged,arg1_tagged,rest_args_tagged,-1);
}
LCC_RETURN apply_call_next_method2(T_O* func_tagged, T_O* arg0_tagged, T_O* arg1_tagged, T_O* rest_args_tagged) {
  return apply_method(func_tagged,arg0_tagged,arg1_tagged,rest_args_tagged,-2);
}
};

};
