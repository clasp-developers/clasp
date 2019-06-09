/*
    File: instance.cc
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

#pragma clang diagnostic push
#pragma GCC diagnostic ignored "-Warray-bounds"
#pragma GCC diagnostic ignored "-Wunused-variable"
#pragma GCC diagnostic ignored "-Wunneeded-internal-declaration"
//#pragma GCC diagnostic ignored "-Wunused-local-typedef"
#include <boost/graph/vector_as_graph.hpp>
#include <boost/graph/topological_sort.hpp>
#pragma clang diagnostic pop

#include <clasp/core/foundation.h>
#include <clasp/core/object.h>
#include <clasp/core/lisp.h>
#include <clasp/core/symbolTable.h>
#include <clasp/gctools/gcFunctions.h>
#include <clasp/core/serialize.h>
#include <clasp/core/hashTable.h>
#include <clasp/core/lispList.h>
#include <clasp/core/debugger.h>
#include <clasp/core/wrappedPointer.h>
#include <clasp/core/derivableCxxObject.h>
#include <clasp/core/hashTable.h>
#include <clasp/core/hashTableEq.h>
#include <clasp/core/hashTableEql.h>
#include <clasp/core/evaluator.h>
#include <clasp/core/primitives.h>
#include <clasp/core/genericFunction.h>
#include <clasp/llvmo/intrinsics.h>
#include <clasp/core/funcallableInstance.h>
#include <clasp/core/wrappers.h>

namespace core {


void FuncallableInstance_O::initializeSlots(gctools::ShiftedStamp stamp, size_t numberOfSlots) {
  ASSERT(gctools::Header_s::Value::is_rack_shifted_stamp(stamp));
  this->_Rack = Rack_O::make(numberOfSlots,_Unbound<T_O>());
  this->stamp_set(stamp);
#ifdef DEBUG_GUARD_VALIDATE
  client_validate(this->_Rack);
#endif
}

void FuncallableInstance_O::initializeClassSlots(Creator_sp creator, gctools::ShiftedStamp stamp) {
  ASSERT(gctools::Header_s::Value::is_rack_shifted_stamp(stamp));
  DEPRECATED();
}

// Identical to allocate_new_instance in instance.cc, except for the type.
CL_DEFUN T_sp core__allocate_new_funcallable_instance(Instance_sp cl, size_t numberOfSlots) {
  // cl is known to be a funcallable-standard-class.
  ASSERT(cl->CLASS_has_creator());
  Creator_sp creator = gctools::As<Creator_sp>(cl->CLASS_get_creator());
  FuncallableInstance_sp obj = gc::As_unsafe<FuncallableInstance_sp>(creator->creator_allocate());
  obj->_Class = cl;
  obj->initializeSlots(cl->CLASS_stamp_for_instances(), numberOfSlots);
  obj->_Sig = cl->slots();
  return obj;
}

CL_DEFUN FuncallableInstance_sp core__reallocate_funcallable_instance(FuncallableInstance_sp instance,
                                                                      Instance_sp new_class,
                                                                      size_t new_size) {
  instance->_Class = new_class;
  instance->initializeSlots(new_class->CLASS_stamp_for_instances(), new_size);
  instance->_Sig = new_class->slots();
  return instance;
}

size_t FuncallableInstance_O::rack_stamp_offset() {
  SimpleVector_O dummy_rack(0);
  return (char*)&(dummy_rack.operator[](0))-(char*)&dummy_rack;
}

Fixnum FuncallableInstance_O::stamp() const {
  return this->_Rack->stamp_get();
};

void FuncallableInstance_O::stamp_set(Fixnum s) {
  this->_Rack->stamp_set(s);
};

size_t FuncallableInstance_O::numberOfSlots() const {
  return this->_Rack->length();
};

T_sp FuncallableInstance_O::instanceSig() const {
#if DEBUG_CLOS >= 2
  stringstream ssig;
  if (this->_Sig) {
    ssig << this->_Sig->__repr__();
  } else {
    ssig << "UNDEFINED ";
  }
  printf("\nMLOG INSTANCE-SIG of Instance %p \n", (void *)(this));
#endif
  return ((this->_Sig));
}

SYMBOL_EXPORT_SC_(ClosPkg, setFuncallableInstanceFunction);
SYMBOL_EXPORT_SC_(CorePkg, instanceClassSet);

T_sp FuncallableInstance_O::instanceClassSet(Instance_sp mc) {
  this->_Class = mc;
#ifdef DEBUG_GUARD_VALIDATE
  client_validate(this->_Rack);
#endif
  return (this->sharedThis<FuncallableInstance_O>());
}

T_sp FuncallableInstance_O::instanceRef(size_t idx) const {
#ifdef DEBUG_GUARD_VALIDATE
  client_validate(this->_Rack);
#endif
#if DEBUG_CLOS >= 2
  printf("\nMLOG INSTANCE-REF[%d] of Instance %p --->%s\n", idx, (void *)(this), this->_Rack[idx]->__repr__().c_str());
#endif
  return low_level_instanceRef(this->_Rack,idx);
}
T_sp FuncallableInstance_O::instanceSet(size_t idx, T_sp val) {
#if DEBUG_CLOS >= 2
  printf("\nMLOG SI-INSTANCE-SET[%d] of Instance %p to val: %s\n", idx, (void *)(this), val->__repr__().c_str());
#endif
  low_level_instanceSet(this->_Rack,idx,val);
#ifdef DEBUG_GUARD_VALIDATE
  client_validate(this->_Rack);
#endif
  return val;
}

CL_DEFUN T_sp core__low_level_standard_generic_function_name(FuncallableInstance_sp gfun)
{
  return gfun->GFUN_NAME();
}

string FuncallableInstance_O::__repr__() const {
  stringstream ss;
  ss << "#S(";
  if (Instance_sp mc = this->_Class.asOrNull<Instance_O>()) {
    ss << mc->_classNameAsString() << " ";
  } else {
    ss << "<ADD SUPPORT FOR INSTANCE _CLASS=" << _rep_(this->_Class) << " >";
  }
  ss << _rep_(this->GFUN_NAME());
  if (this->_Rack)
  {
    ss << " #slots[" << this->numberOfSlots() << "]";
  } else {
    ss << " rack-undef";
  }
  ss << ")" ;
  return ss.str();
}

void FuncallableInstance_O::LISP_INVOKE() {
  IMPLEMENT_ME();
}

LCC_RETURN FuncallableInstance_O::funcallable_entry_point(LCC_ARGS_ELLIPSIS) {
  SETUP_CLOSURE(FuncallableInstance_O,closure);
  INCREMENT_FUNCTION_CALL_COUNTER(closure);
  if (lcc_nargs<=LCC_ARGS_IN_REGISTERS) {
    return (gc::As_unsafe<Function_sp>(closure->GFUN_DISPATCHER())->entry.load())(closure->GFUN_DISPATCHER().raw_(),lcc_nargs,lcc_fixed_arg0,lcc_fixed_arg1,lcc_fixed_arg2,lcc_fixed_arg3);
  }
  INITIALIZE_VA_LIST();
  // This is where we could decide to compile the dtree and switch the GFUN_DISPATCHER() or not
//  printf("%s:%d:%s About to call %s\n", __FILE__, __LINE__, __FUNCTION__, _rep_(closure->functionName()).c_str());
  return funcall_consume_valist_<core::Function_O>(closure->GFUN_DISPATCHER().tagged_(),lcc_vargs);
}

T_sp FuncallableInstance_O::copyInstance() const {
  DEPRECATED();
  Instance_sp cl = this->_Class;
  FuncallableInstance_sp copy = gc::As_unsafe<FuncallableInstance_sp>(cl->CLASS_get_creator()->creator_allocate());
  copy->_Class = cl;
  copy->_Rack = this->_Rack;
  copy->_Sig = this->_Sig;
  return copy;
}

T_sp FuncallableInstance_O::setFuncallableInstanceFunction(T_sp function) {
  SYMBOL_EXPORT_SC_(ClPkg, standardGenericFunction);
  /* We have to be cautious about thread safety here. We don't want to crash
   * if one thread set-funcallable-instance-function's an instance that another
   * thread is calling.
   * As an optimization, if a function isn't a closure, we just use its entry
   * point directly, to avoid the overhead from funcallable_entry_point.
   * But in general we have funcallable_entry_point just call the GFUN_DISPATCHER.
   * Accessing both the entry point and the GFUN_DISPATCHER is atomic.
   * So here's what we do: first, change the GFUN_DISPATCHER. Then, change the
   * entry point.
   * If we had funcallable_entry_point before the set-funcallable-instance,
   * a call between the two sets will just use the new function.
   * If we had some other entry point, a call will use that, and it must be
   * insensitive to the GFUN_DISPATCHER.
   * So in either case something coherent is called. */
  /* TODO: We could make this work with any closure, without using locks:
   * 1) GFUN_DISPATCHER_set. If the entry_point is funcallable_entry_point,
   *    now we are using the new function. Otherwise this is meaningless.
   * 2) Set the entry to funcallable_entry_point. Now the instance's closure
   *    vector is irrelevant and we are using the new function.
   * 3) Copy the closure vector into the instance. Doesn't need to be atomic.
   * 4) Set the entry to the closure's entry.
   * The only reason I'm not doing this now is that funcallable instances
   * aren't actually closures at the moment. */
  if (gc::IsA<Function_sp>(function)) {
    this->GFUN_DISPATCHER_set(function);
    // If the function has no closure slots, we can use its entry point.
    if (gc::IsA<ClosureWithSlots_sp>(function)) {
      ClosureWithSlots_sp closure = gc::As_unsafe<ClosureWithSlots_sp>(function);
      if (closure->openP())
        this->entry.store(closure->entry.load());
      else this->entry.store(funcallable_entry_point);
    } else this->entry.store(funcallable_entry_point);
  } else {
    TYPE_ERROR(function, cl::_sym_function);
  }

  return ((this->sharedThis<FuncallableInstance_O>()));
}

void FuncallableInstance_O::describe(T_sp stream) {
  stringstream ss;
  ss << (BF("FuncallableInstance\n")).str();
  ss << (BF("_Class: %s\n") % _rep_(this->_Class).c_str()).str();
  for (int i(1); i < this->_Rack->length(); ++i) {
    ss << (BF("_Rack[%d]: %s\n") % i % _rep_((*this->_Rack)[i]).c_str()).str();
  }
  clasp_write_string(ss.str(), stream);
}

CL_DEFUN T_mv clos__getFuncallableInstanceFunction(T_sp obj) {
  if (FuncallableInstance_sp iobj = obj.asOrNull<FuncallableInstance_O>()) {
    return Values(_lisp->_true(),Pointer_O::create((void*)iobj->entry.load()));
  } else return Values(_Nil<T_O>(),_Nil<T_O>());
};

CL_DEFUN T_sp clos__setFuncallableInstanceFunction(T_sp obj, T_sp func) {
  if (FuncallableInstance_sp iobj = obj.asOrNull<FuncallableInstance_O>()) {
    return iobj->setFuncallableInstanceFunction(func);
  }
  SIMPLE_ERROR(BF("You can only setFuncallableInstanceFunction on funcallable instances - you tried to set it on a: %s") % _rep_(obj));
};
 
};


namespace core {
           
T_sp FuncallableInstance_O::GFUN_CALL_HISTORY_compare_exchange(T_sp expected, T_sp new_value) {
#ifdef DEBUG_GFDISPATCH
  if (_sym_STARdebug_dispatchSTAR->symbolValue().notnilp()) {
    printf("%s:%d   GFUN_CALL_HISTORY_set gf: %s\n", __FILE__, __LINE__, this->__repr__().c_str());
    printf("%s:%d                      history: %s\n", __FILE__, __LINE__, _rep_(h).c_str());
  }
#endif
  bool exchanged = this->_CallHistory.compare_exchange_strong(expected,new_value);
  return exchanged ? new_value : expected;
}

T_sp FuncallableInstance_O::GFUN_SPECIALIZER_PROFILE_compare_exchange(T_sp expected, T_sp new_value) {
  bool exchanged = this->_SpecializerProfile.compare_exchange_strong(expected,new_value);
  return exchanged ? new_value : expected;
}

CL_DEFUN void clos__generic_function_increment_compilations(FuncallableInstance_sp gf) {
  gf->increment_compilations();
}

CL_DEFUN size_t clos__generic_function_compilations(FuncallableInstance_sp gf) {
  return gf->compilations();
}

CL_DEFUN T_sp clos__generic_function_specializer_profile(FuncallableInstance_sp gf) {
  return gf->GFUN_SPECIALIZER_PROFILE();
}

CL_DEFUN T_sp clos__generic_function_specializer_profile_compare_exchange(FuncallableInstance_sp gf, T_sp expected, T_sp new_value) {
  return gf->GFUN_SPECIALIZER_PROFILE_compare_exchange(expected,new_value);
}

CL_DEFUN T_sp clos__generic_function_call_history(FuncallableInstance_sp obj) {
  return obj->GFUN_CALL_HISTORY();
}

CL_DEFUN T_sp clos__generic_function_call_history_compare_exchange(FuncallableInstance_sp gf, T_sp expected, T_sp new_value) {
  return gf->GFUN_CALL_HISTORY_compare_exchange(expected,new_value);
}

CL_DEFUN T_sp clos__generic_function_compiled_dispatch_function(T_sp obj) {
  return gc::As<FuncallableInstance_sp>(obj)->GFUN_DISPATCHER();
}
CL_DEFUN void clos__set_generic_function_compiled_dispatch_function(T_sp obj, T_sp val) {
  gc::As<FuncallableInstance_sp>(obj)->GFUN_DISPATCHER_set(val);
}
}

SYMBOL_EXPORT_SC_(ClosPkg,inode);
SYMBOL_EXPORT_SC_(ClosPkg,outcome);
SYMBOL_EXPORT_SC_(ClosPkg,range);
SYMBOL_EXPORT_SC_(ClosPkg,tag_test);
SYMBOL_EXPORT_SC_(ClosPkg,skip);
SYMBOL_EXPORT_SC_(ClosPkg,optimized_slot_reader);
SYMBOL_EXPORT_SC_(ClosPkg,optimized_slot_writer);
SYMBOL_EXPORT_SC_(ClosPkg,fast_method_call);
SYMBOL_EXPORT_SC_(ClosPkg,effective_method_outcome);

#include <clasp/llvmo/read-stamp.cc>

namespace core {
#if 1

CL_DEF_CLASS_METHOD DtreeInterpreter_sp DtreeInterpreter_O::make_dtree_interpreter(T_sp generic_function, T_sp dtree_root) {
  FunctionDescription* fdesc = makeFunctionDescription(clos::_sym_inode,_Nil<T_O>());
  if (!gc::IsA<SimpleVector_sp>(dtree_root)) {
    printf("%s:%d Trying to create a dtree-interpreter %s with no node\n", __FILE__, __LINE__, _rep_(dtree_root).c_str());
  }
  GC_ALLOCATE_VARIADIC(DtreeInterpreter_O,dt,fdesc,generic_function,dtree_root);
//  printf("%s:%d Created a dtree-interpreter @%p  dtree -> @%p with node -> %s\n", __FILE__, __LINE__, (void*)dt.raw_(), (void*)dtree.raw_(), _rep_(dtree).c_str());
  return dt;
}

#if DEBUG_DTREE_INTERPRETER
#define DTLOG(x) if (_sym_STARdebug_dtree_interpreterSTAR.boundp()&&_sym_STARdebug_dtree_interpreterSTAR->symbolValue().notnilp()) { FILE* fout= monitor_file("dtree-interp");  fprintf( fout, "%s", (x).str().c_str()); fflush(fout); }
#else
#define DTLOG(x)
#endif

SYMBOL_EXPORT_SC_(ClosPkg,codegen_dispatcher);
SYMBOL_EXPORT_SC_(KeywordPkg,force_compile);
SYMBOL_EXPORT_SC_(KeywordPkg,generic_function_name);
/*!
* The Closure that is passed to LISP_CALLING_CONVENTION is a FuncallableInstance_O that contains 
  a DtreeInterpreter_sp.   In the code below the funcallable_instance is the FuncallableInstance_O closure
  and interpreter is the DtreeInterpreter_O.   The DtreeInterpreter_O keeps track of the number of times that
  it was called and if it is called too many times we can COMPILE the dtree and replace the funcallable_instance's
  GFUN_DISPATCHER, and 'entry' pointer with the compiled version.
  This will cause the interpreter to be collected and the generic function will use the compiled version from
  then on.
*/

#define COMPILE_TRIGGER 1024
LCC_RETURN DtreeInterpreter_O::LISP_CALLING_CONVENTION() {
  SETUP_CLOSURE(DtreeInterpreter_O,interpreter);
  FuncallableInstance_sp generic_function = gc::As_unsafe<FuncallableInstance_sp>(interpreter->_GenericFunction);
  DTLOG(BF("=======================================================\n"));
  DTLOG(BF("%s:%d:%s Entered generic function -> %s\n") % __FILE__% __LINE__% __FUNCTION__ % _safe_rep_(generic_function));
  SimpleVector_sp dtree = gc::As_unsafe<SimpleVector_sp>(interpreter->_DtreeRoot);
#ifdef DEBUG_ASSERT
  if (!gc::IsA<SimpleVector_sp>(dtree)) {
    SIMPLE_ERROR(BF("Entered Dtree interpreter entry-point with %s as the dtree - that is NOT allowed") % _rep_(dtree).c_str());
  }
#endif
#if 0
  interpreter->_CallCount++;
  if (interpreter->_CallCount==COMPILE_TRIGGER) {
    T_sp fn = generic_function->functionName();
//    printf("%s:%d:%s  interpreter->_CallCount hit %lu for %s\n", __FILE__, __LINE__, __FUNCTION__, interpreter->_CallCount, _rep_(fn).c_str());
    T_sp call_history = generic_function->GFUN_CALL_HISTORY();
    T_sp specializer_profile = generic_function->GFUN_SPECIALIZER_PROFILE();
//    printf("%s:%d:%s  About to call compiler\n", __FILE__, __LINE__, __FUNCTION__);
    T_sp compiled_discriminator = eval::funcall(clos::_sym_codegen_dispatcher,call_history,specializer_profile,generic_function->asSmartPtr(),
                                                kw::_sym_force_compile,_lisp->_true(),
                                                kw::_sym_generic_function_name, fn );
//    printf("%s:%d:%s about to setFuncallableInstanceFunction\n", __FILE__, __LINE__, __FUNCTION__);
    generic_function->setFuncallableInstanceFunction(compiled_discriminator);
    // The next call should use the compiled discriminator
    // Can I fall through from here and continue using the interpreter one more time?
//    printf("%s:%d:%s  falling through to interpreter\n", __FILE__, __LINE__, __FUNCTION__);
  }
#endif
  // Here is where we can check interpreter->_CallCount and maybe compile the dtree and replace ourselves and jump
  // to the compiled version of the dtree.
  DTLOG(BF("%s:%d Entered with dtree-interpreter @%p with dtree.root -> %s\n") % __FILE__% __LINE__% (void*)interpreter% _rep_(interpreter->_DtreeRoot).c_str());
  INITIALIZE_VA_LIST(); // lcc_vargs now points to the rewound argument list
  Vaslist dispatch_args_s(*lcc_vargs);
  VaList_sp dispatch_args(&dispatch_args_s);
  DTLOG(BF("%s:%d     Arguments: %s\n") % __FILE__% __LINE__% _safe_rep_(lcc_vargs));
  int nargs = dispatch_args->remaining_nargs();
  ASSERT(gc::IsA<SimpleVector_sp>(interpreter->_DtreeRoot));
  SimpleVector_sp node = gc::As_unsafe<SimpleVector_sp>(interpreter->_DtreeRoot);
 TOP:
  DTLOG(BF("%s:%d:%s node parts\n") % __FILE__ % __LINE__ % __FUNCTION__ );
  DTLOG(BF("%s:%d:%s eql specializers:           %s\n") % __FILE__ % __LINE__ % __FUNCTION__ % _safe_rep_((*node)[REF_NODE_EQL_SPECIALIZERS]));
  DTLOG(BF("%s:%d:%s skip:                       %s\n") % __FILE__ % __LINE__ % __FUNCTION__ % _safe_rep_((*node)[REF_NODE_SKIP]));
  DTLOG(BF("%s:%d:%s tag tests:                  %s\n") % __FILE__ % __LINE__ % __FUNCTION__ % _safe_rep_((*node)[REF_NODE_TAG_TESTS]));
  DTLOG(BF("%s:%d:%s c++ class specializers:     %s\n") % __FILE__ % __LINE__ % __FUNCTION__ % _safe_rep_((*node)[REF_NODE_CXX_CLASS_SPECIALIZERS]));
  DTLOG(BF("%s:%d:%s complex class specializers: %s\n") % __FILE__ % __LINE__ % __FUNCTION__ % _safe_rep_((*node)[REF_NODE_CLASS_SPECIALIZERS]));
  core::T_sp arg = dispatch_args->next_arg();
#ifdef DEBUG_ASSERT
  if (!gc::IsA<SimpleVector_sp>(node)) {
    printf("%s:%d node is not a SimpleVector_sp -> |%s| dtree -> |%s|\n", __FILE__, __LINE__, _safe_rep_(node).c_str(), _safe_rep_(dtree).c_str());
  }
#endif
  T_sp type = (*node)[REF_TYPE];
  DTLOG(BF("%s:%d  node type = %s  nargs remaining %d\n") % __FILE__% __LINE__% _rep_(type).c_str()% nargs);
  if (type == clos::_sym_inode) {
    if (nargs<=0) {
      SIMPLE_ERROR(BF("Insufficient arguments"));
    }
    nargs--;
    //
    //  Test EQL specializers
    //
    DTLOG(BF("%s:%d:%s On a dtree node\n") % __FILE__% __LINE__% __FUNCTION__);
    DTLOG(BF("%s:%d:%s      Argument@%p\n") % __FILE__% __LINE__% __FUNCTION__% (void*)arg.raw_());
    DTLOG(BF("About to check eql_specializers -> %s\n") % _safe_rep_((*node)[REF_NODE_EQL_SPECIALIZERS]));
    SimpleVector_sp eql_specializers = gc::As_unsafe<SimpleVector_sp>((*node)[REF_NODE_EQL_SPECIALIZERS]);
    for ( size_t index = 0; index< eql_specializers->length(); index += DtreeInterpreter_O::dtree_eql_test_step) {
      if (cl__eql(arg,(*eql_specializers)[index+DtreeInterpreter_O::dtree_eql_test_spec_index])) {
        node = gc::As_unsafe<SimpleVector_sp>((*eql_specializers)[index+DtreeInterpreter_O::dtree_eql_test_outcome_index]);
        goto TOP;
      }
    }
    //
    // Now check skip
    //
    DTLOG(BF("    About to try skip is there skip? -> %s\n") % _safe_rep_((*node)[REF_NODE_SKIP]));
    if ((*node)[REF_NODE_SKIP].notnilp()) {
      node = gc::As_unsafe<SimpleVector_sp>((*node)[REF_NODE_SKIP]);
      goto TOP;
    }

    //
    // Now check tag specializers
    //
    DTLOG(BF("    About to try tag-tests\n"));
    SimpleVector_sp tag_tests = gc::As_unsafe<SimpleVector_sp>((*node)[REF_NODE_TAG_TESTS]);
    if ((*tag_tests)[FIXNUM_TEST].notnilp() && arg.fixnump()) {
      DTLOG(BF("%s:%d:%s      The arg ptr %p matched fixnump!!!\n") % __FILE__% __LINE__% __FUNCTION__% arg.raw_());
      node = gc::As_unsafe<SimpleVector_sp>((*tag_tests)[FIXNUM_TEST]);
      goto TOP;
    }
    if ((*tag_tests)[CONS_TEST].notnilp() && arg.consp()) {
      DTLOG(BF("%s:%d:%s      The arg ptr %p matched consp!!!\n") % __FILE__% __LINE__% __FUNCTION__% arg.raw_());
      node = gc::As_unsafe<SimpleVector_sp>((*tag_tests)[CONS_TEST]);
      goto TOP;
    }
    if ((*tag_tests)[SINGLE_FLOAT_TEST].notnilp() && arg.single_floatp()) {
      DTLOG(BF("%s:%d:%s      The arg ptr %p matched single_floatp!!!\n") % __FILE__% __LINE__% __FUNCTION__% arg.raw_());
      node = gc::As_unsafe<SimpleVector_sp>((*tag_tests)[SINGLE_FLOAT_TEST]);
      goto TOP;
    }
    if ((*tag_tests)[CHARACTER_TEST].notnilp() && arg.characterp()) {
      DTLOG(BF("%s:%d:%s      The arg ptr %p matched characterp!!!\n") % __FILE__% __LINE__% __FUNCTION__% arg.raw_());
      node = gc::As_unsafe<SimpleVector_sp>((*tag_tests)[CHARACTER_TEST]);
      goto TOP;
    }

    //
    // Now if this isn't a generalp object then it's a dispatch miss (it may be worse - VASLIST???)
    //
    if (!arg.generalp()) {
      DTLOG(BF("The arg is not generalp goto DISPATCH_MISS\n"));
      goto DISPATCH_MISS;
    }
    //
    // Read the header
    //
    core::General_O* client_ptr = gctools::untag_general<General_O*>((General_O*)arg.raw_());
    uintptr_t raw_stamp = (uintptr_t)(llvmo::template_read_general_stamp(client_ptr)); // The stamp is shifted by the fixnum_shift
    // Get the where tag
    uintptr_t where = raw_stamp&gctools::Header_s::where_mask;
    DTLOG(BF("Read the raw_stamp %lu  where_tag -> 0x%x  complex_stamp? -> %s\n") % raw_stamp % where % ((where==gctools::Header_s::header_wtag) ? "No" : "Yes" ));
    //
    // IS THIS RIGHT???????????????????????????
    // If this is a complex object - then we can skip cxx_class_specializers
    //

    if (where != gctools::Header_s::header_wtag) {
      DTLOG(BF("where is not a simple header_wtag ... Skipping to COMPLEX_SPECIALIZERS\n"));
      goto COMPLEX_SPECIALIZERS;
    }
    //
    // Check the header stamp against the c++ class specializers
    //
    {
      SimpleVector_sp  cxx_class_specializers = gc::As_unsafe<SimpleVector_sp>((*node)[REF_NODE_CXX_CLASS_SPECIALIZERS]);
      DTLOG(BF("Checking cxx_class_specializers: %s\n") % _safe_rep_(cxx_class_specializers));
      for ( size_t index = 0, iEnd(cxx_class_specializers->length()); index < iEnd; index = index + DtreeInterpreter_O::dtree_range_step ) {
        size_t outcome_index = index + DtreeInterpreter_O::dtree_range_outcome_index;
        uintptr_t low_stamp = (uintptr_t)((*cxx_class_specializers)[index+DtreeInterpreter_O::dtree_range_low_index].raw_());
        uintptr_t high_stamp = (uintptr_t)((*cxx_class_specializers)[index+DtreeInterpreter_O::dtree_range_high_index].raw_());
        DTLOG(BF("  About to compare raw_stamp %lu to range [ %lu %lu ]\n") % raw_stamp % low_stamp % high_stamp );
        if (low_stamp <= raw_stamp && raw_stamp <= high_stamp) {
          node = gc::As_unsafe<SimpleVector_sp>((*cxx_class_specializers)[outcome_index]);
          DTLOG(BF("%s:%d:%s      The stamp %lu matched the C++ class range [%lu %lu]   next node -> %s!!!\n") % __FILE__% __LINE__% __FUNCTION__% raw_stamp% low_stamp% high_stamp% _safe_rep_(node).c_str());
          goto TOP;
        } else {
          DTLOG(BF("  It did not match\n"));
        }
      }
    }
  COMPLEX_SPECIALIZERS:
    //
    // IS THIS RIGHT????????
    // If the raw_stamp is <= shifted STAMP_max then we have a dispatch miss
    //
    if (where == gctools::Header_s::header_wtag) {
      DTLOG(BF("About to check complex specializers but the where tag says it's a simple header tag - jumping to DISPATCH_MISS\n"));
      goto DISPATCH_MISS;
    }
    SimpleVector_sp complex_class_specializers = gc::As_unsafe<SimpleVector_sp>((*node)[REF_NODE_CLASS_SPECIALIZERS]);
      // If there are no complex specializers then it's a dispatch miss
    if (complex_class_specializers->length() == 0) {
      DTLOG(BF("There are no complex_class_specializers - going to DISPATCH_MISS\n"));
      goto DISPATCH_MISS;
    }
      // If where is header_wtag then it's a C++ class but we just exhausted those outcomes.
    switch (where) {
    case gctools::Header_s::header_wtag: goto DISPATCH_MISS;  // Redundant?????
    case gctools::Header_s::rack_wtag:
        raw_stamp = (uintptr_t)(llvmo::template_read_rack_stamp(client_ptr));
        DTLOG(BF("Read the rack_wtag stamp -> %lu\n") % raw_stamp);
        break;
    case gctools::Header_s::wrapped_wtag:
        raw_stamp = (uintptr_t)(llvmo::template_read_wrapped_stamp(client_ptr));
        DTLOG(BF("Read the wrapped_wtag stamp -> %lu\n") % raw_stamp);
        break;
    case gctools::Header_s::derivable_wtag:
        raw_stamp = (uintptr_t)(llvmo::template_read_derived_stamp(client_ptr));
        DTLOG(BF("Read the derivable_wtag stamp -> %lu\n") % raw_stamp);
        break;
    }
    DTLOG(BF("Checking complex_class_specializers: %s\n") % _safe_rep_(complex_class_specializers));
    for ( size_t index = 0, iEnd(complex_class_specializers->length()); index < iEnd; index = index + DtreeInterpreter_O::dtree_range_step ) {
      size_t outcome_index = index + DtreeInterpreter_O::dtree_range_outcome_index;
      uintptr_t low_stamp = (uintptr_t)((*complex_class_specializers)[index+DtreeInterpreter_O::dtree_range_low_index].raw_());
      uintptr_t high_stamp = (uintptr_t)((*complex_class_specializers)[index+DtreeInterpreter_O::dtree_range_high_index].raw_());
      if (low_stamp <= raw_stamp && raw_stamp <= high_stamp) {
        node = gc::As_unsafe<SimpleVector_sp>((*complex_class_specializers)[outcome_index]);
        DTLOG(BF("%s:%d:%s      The stamp %lu matched the range [%lu %lu]   next node -> %s!!!\n") % __FILE__% __LINE__% __FUNCTION__% raw_stamp% low_stamp% high_stamp% _safe_rep_(node).c_str());
        goto TOP;
      }
    }
    DTLOG(BF("Fell through to DISPATCH_MISS\n"));
    goto DISPATCH_MISS;
  } else if (type == clos::_sym_outcome) {
    DTLOG(BF("%s:%d:%s Handle outcome %s\n") % __FILE__% __LINE__% __FUNCTION__% _safe_rep_(node));
    T_sp outcome_type = (*node)[REF_OUTCOME_SUBTYPE];
    if (outcome_type == clos::_sym_optimized_slot_reader) {
      T_sp location = (*node)[REF_OPTIMIZED_SLOT_READER_INDEX];
      T_sp slot_name = (*node)[REF_OPTIMIZED_SLOT_READER_SLOT_NAME];
      T_sp class_ = (*node)[REF_OPTIMIZED_SLOT_READER_CLASS];
      if (location.fixnump()) {
        size_t index = location.unsafe_fixnum();
        Instance_sp instance((gc::Tagged)lcc_fixed_arg0);
        T_sp value = instance->instanceRef(index);
        if (value.unboundp()) return core::eval::funcall(cl::_sym_slot_unbound,class_,instance,slot_name);
        return gctools::return_type(value.raw_(),1);
      } else if (location.consp()) {
        Instance_sp instance((gc::Tagged)lcc_fixed_arg0);
        Cons_sp cell = gc::As_unsafe<Cons_sp>(location);
        T_sp value = oCar(cell);
        if (value.unboundp()) return core::eval::funcall(cl::_sym_slot_unbound,class_,instance,slot_name);
        return gctools::return_type(value.raw_(),1);
      }
    } else if (outcome_type == clos::_sym_optimized_slot_writer) {
      T_sp location = (*node)[REF_OPTIMIZED_SLOT_READER_INDEX];
      if (location.fixnump()) {
        size_t index = location.unsafe_fixnum();
        T_sp value((gc::Tagged)lcc_fixed_arg0);
        Instance_sp instance((gc::Tagged)lcc_fixed_arg1);
        instance->instanceSet(index,value);
        return gctools::return_type(value.raw_(),1);
      } else if (location.consp()) {
        Cons_sp cell = gc::As_unsafe<Cons_sp>(location);
        T_sp value((gc::Tagged)lcc_fixed_arg0);
        cell->rplaca(value);
        return gctools::return_type(value.raw_(),1);
      }
    } else if (outcome_type == clos::_sym_fast_method_call) {
      Function_sp func = gc::As_unsafe<Function_sp>((*node)[REF_FAST_METHOD_CALL_FUNCTION]);
      return (*func).entry.load()(func.raw_(),lcc_nargs,lcc_fixed_arg0,lcc_fixed_arg1,lcc_fixed_arg2,lcc_fixed_arg3);
    } else if (outcome_type == clos::_sym_effective_method_outcome) {
      Function_sp func = gc::As_unsafe<Function_sp>((*node)[REF_EFFECTIVE_METHOD_OUTCOME_FUNCTION]);
      return core::eval::funcall(func,lcc_vargs,_Nil<T_O>());
    }
    DTLOG(BF("%s:%d:%s Bad outcome %s\n") % __FILE__% __LINE__% __FUNCTION__% _safe_rep_(outcome_type));
    abort();
      // Do outcomes.
  }
DISPATCH_MISS:
  DTLOG(BF("%s:%d:%s    It's a DISPATCH-MISS!!! For gf %s  -- invoking with args: %s\n") % __FILE__% __LINE__% __FUNCTION__
        % _safe_rep_(generic_function)
        % _safe_rep_(core__list_from_va_list(lcc_vargs)));
  DTLOG(BF(" Call-history: %s\n") % _safe_rep_(generic_function->GFUN_CALL_HISTORY()));
  DTLOG(BF(" Specializer-profile: %s\n") % _safe_rep_(generic_function->GFUN_SPECIALIZER_PROFILE()));
  DTLOG(BF("%s:%d:%s      (core:next-number) -> %s\n") % __FILE__ % __LINE__ % __FUNCTION__ % _rep_(core__next_number()));
  DTLOG(BF("^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^\n"));
  return core::eval::funcall(clos::_sym_dispatch_miss,generic_function,lcc_vargs);
}
#endif


  CL_DEFUN void core__verify_funcallable_instance_layout(size_t funcallableInstance_size, size_t funcallableInstance_rack_offset)
  {
    if (funcallableInstance_size!=sizeof(FuncallableInstance_O)) SIMPLE_ERROR(BF("The cmpintrinsics.lsp funcallableInstance_size %lu does not match sizeof(FuncallableInstance_O)") % funcallableInstance_size % sizeof(FuncallableInstance_O));
    if (funcallableInstance_rack_offset!=offsetof(FuncallableInstance_O,_Rack))
      SIMPLE_ERROR(BF("funcallableInstance_rack_offset %lu does not match offsetof(_Rack,FuncallableInstance_O) %lu") % funcallableInstance_rack_offset % offsetof(FuncallableInstance_O,_Rack));
  }

  CL_DEFUN void core__verify_dtree_interpreter_layout(size_t eql_test_step, size_t range_step)
  {
    if (eql_test_step != DtreeInterpreter_O::dtree_eql_test_step_fn()) {
      SIMPLE_ERROR(BF("eql_test_step %lu does not match DtreeInterpreter_O::dtree_eql_test_step %lu") % eql_test_step % DtreeInterpreter_O::dtree_eql_test_step_fn());
    }
    if (range_step != DtreeInterpreter_O::dtree_range_step_fn()) {
      SIMPLE_ERROR(BF("range_step %lu does not match DtreeInterpreter_O::dtree_range_step %lu") % range_step % DtreeInterpreter_O::dtree_range_step_fn());
    }
  }

};
