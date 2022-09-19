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
#include <clasp/core/lambdaListHandler.h>
#include <clasp/core/primitives.h>
#include <clasp/core/singleDispatchMethod.h>
#include <clasp/llvmo/intrinsics.h>
#include <clasp/core/funcallableInstance.h>
#include <clasp/core/wrappers.h>

namespace core {


void FuncallableInstance_O::initializeSlots(gctools::BaseHeader_s::StampWtagMtag stamp,
                                            T_sp sig, size_t numberOfSlots) {
  ASSERT(gctools::BaseHeader_s::StampWtagMtag::is_rack_shifted_stamp(stamp));
  this->_Rack = Rack_O::make(numberOfSlots,sig,unbound<T_O>());
  this->stamp_set(stamp.as_fixnum());
#ifdef DEBUG_GUARD_VALIDATE
  client_validate(rack());
#endif
}

void FuncallableInstance_O::initializeClassSlots(Creator_sp creator, gctools::BaseHeader_s::StampWtagMtag stamp) {
  ASSERT(gctools::Header_s::BaseHeader_s::StampWtagMtag::is_rack_shifted_stamp(stamp));
  DEPRECATED();
}

// FIXME: Exists solely for cases where the list of slotds is hard to get.
CL_LAMBDA(class slot-count)
DOCGROUP(clasp)
CL_DEFUN T_sp core__allocate_funcallable_standard_instance(Instance_sp cl,
                                                           size_t slot_count) {
  GlobalEntryPoint_sp entryPoint = makeGlobalEntryPointAndFunctionDescription<FuncallableInstance_O>(cl::_sym_lambda,nil<core::T_sp>());
  auto  obj = gctools::GC<FuncallableInstance_O>::allocate( entryPoint);
  obj->_Class = cl;
  obj->initializeSlots(cl->CLASS_stamp_for_instances(), cl->slots(), slot_count);
  return obj;
}

DOCGROUP(clasp)
CL_DEFUN FuncallableInstance_sp core__allocate_raw_funcallable_instance(Instance_sp cl,
                                                                        Rack_sp rack) {
  GlobalEntryPoint_sp entryPoint = makeGlobalEntryPointAndFunctionDescription<FuncallableInstance_O>(cl::_sym_lambda,nil<core::T_sp>());
  auto  obj = gctools::GC<FuncallableInstance_O>::allocate( entryPoint, cl, rack);
  return obj;
}

size_t FuncallableInstance_O::rack_stamp_offset() {
  SimpleVector_O dummy_rack(0,nil<T_O>(),false);
  return (char*)&(dummy_rack.operator[](0))-(char*)&dummy_rack;
}

Fixnum FuncallableInstance_O::stamp() const {
  return rack()->stamp_get();
};

void FuncallableInstance_O::stamp_set(Fixnum s) {
  rack()->stamp_set(s);
};

size_t FuncallableInstance_O::numberOfSlots() const {
  return rack()->length();
};

T_sp FuncallableInstance_O::instanceSig() const {
#if DEBUG_CLOS >= 2
  stringstream ssig;
  if (rack()->_Sig) {
    ssig << rack()->_Sig->__repr__();
  } else {
    ssig << "UNDEFINED ";
  }
  printf("\nMLOG INSTANCE-SIG of Instance %p \n", (void *)(this));
#endif
  return ((rack()->_Sig));
}

SYMBOL_EXPORT_SC_(ClosPkg, setFuncallableInstanceFunction);
SYMBOL_EXPORT_SC_(CorePkg, instanceClassSet);

T_sp FuncallableInstance_O::instanceClassSet(Instance_sp mc) {
  this->_Class = mc;
#ifdef DEBUG_GUARD_VALIDATE
  client_validate(rack());
#endif
  return (this->sharedThis<FuncallableInstance_O>());
}

T_sp FuncallableInstance_O::instanceRef(size_t idx) const {
#ifdef DEBUG_GUARD_VALIDATE
  client_validate(rack());
#endif
#if DEBUG_CLOS >= 2
  printf("\nMLOG INSTANCE-REF[%d] of Instance %p --->%s\n", idx, (void *)(this), low_level_instanceRef(rack(), idx)->__repr__().c_str());
#endif
  return low_level_instanceRef(rack(),idx);
}
T_sp FuncallableInstance_O::instanceSet(size_t idx, T_sp val) {
#if DEBUG_CLOS >= 2
  printf("\nMLOG SI-INSTANCE-SET[%d] of Instance %p to val: %s\n", idx, (void *)(this), val->__repr__().c_str());
#endif
  low_level_instanceSet(rack(),idx,val);
#ifdef DEBUG_GUARD_VALIDATE
  client_validate(rack());
#endif
  return val;
}

// Get the name of a generic function without calling any generic functions
// (e.g., generic-function-name). Nice for debugging CLOS.
DOCGROUP(clasp)
CL_DEFUN T_sp core__low_level_standard_generic_function_name(FuncallableInstance_sp gfun)
{
  return gfun->functionName();
}

string FuncallableInstance_O::__repr__() const {
  stringstream ss;
  ss << "#S(";
  if (Instance_sp mc = this->_Class.asOrNull<Instance_O>()) {
    ss << mc->_classNameAsString() << " ";
  } else {
    ss << "<ADD SUPPORT FOR INSTANCE _CLASS=" << _rep_(this->_Class) << " >";
  }
  ss << _rep_(this->functionName());
  if (rack())
  {
    ss << " #slots[" << this->numberOfSlots() << "]";
  } else {
    ss << " rack-undef";
  }
  ss << ")" ;
  return ss.str();
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
    if (gc::IsA<Closure_sp>(function)) {
      Closure_sp closure = gc::As_unsafe<Closure_sp>(function);
      if (closure->openP())
        this->setEntryPoint(closure->entryPoint());
      else {
        GlobalEntryPoint_sp entryPoint = templated_makeGlobalEntryPointCopy<FuncallableInstance_O>(gctools::As<GlobalEntryPoint_sp>(this->entryPoint()));
        this->setEntryPoint(entryPoint);
      }
    } else {
      GlobalEntryPoint_sp entryPoint = templated_makeGlobalEntryPointCopy<FuncallableInstance_O>(gc::As<GlobalEntryPoint_sp>(this->entryPoint()));
      this->setEntryPoint(entryPoint);
    }
  } else {
    TYPE_ERROR(function, cl::_sym_function);
  }
  return ((this->sharedThis<FuncallableInstance_O>()));
}

void FuncallableInstance_O::describe(T_sp stream) {
  stringstream ss;
  ss << (fmt::sprintf("FuncallableInstance\n"));
  ss << (fmt::sprintf("_Class: %s\n", _rep_(this->_Class) ));
  for (int i(1); i < rack()->length(); ++i) {
    ss << fmt::sprintf("_Rack[%d]: %s\n", i , _rep_(low_level_instanceRef(rack(), i)) );
  }
  clasp_write_string(ss.str(), stream);
}

DOCGROUP(clasp)
CL_DEFUN T_mv clos__getFuncallableInstanceFunction(T_sp obj) {
  if (FuncallableInstance_sp iobj = obj.asOrNull<FuncallableInstance_O>()) {
    return Values(_lisp->_true(),Pointer_O::create((void*)iobj->entry()));
  } else return Values(nil<T_O>(),nil<T_O>());
};

DOCGROUP(clasp)
CL_DEFUN T_sp clos__setFuncallableInstanceFunction(T_sp obj, T_sp func) {
  if (FuncallableInstance_sp iobj = obj.asOrNull<FuncallableInstance_O>()) {
    return iobj->setFuncallableInstanceFunction(func);
  }
  SIMPLE_ERROR(("You can only setFuncallableInstanceFunction on funcallable instances - you tried to set it on a: %s") , _rep_(obj));
};
 
};


namespace core {

DOCGROUP(clasp)
CL_DEFUN size_t clos__generic_function_interpreted_calls(FuncallableInstance_sp gf) {
  return gf->interpreted_calls();
}

DOCGROUP(clasp)
CL_DEFUN T_sp clos__generic_function_compiled_dispatch_function(T_sp obj) {
  return gc::As<FuncallableInstance_sp>(obj)->GFUN_DISPATCHER();
}
DOCGROUP(clasp)
CL_DEFUN void clos__set_generic_function_compiled_dispatch_function(T_sp obj, T_sp val) {
  gc::As<FuncallableInstance_sp>(obj)->GFUN_DISPATCHER_set(val);
}
}

#define READ_RACK_STAMP
#define READ_WRAPPED_STAMP
#define READ_RACK_STAMP
#define READ_DERIVED_STAMP
#define READ_GENERAL_STAMP
#include <clasp/llvmo/read-stamp.cc>
#undef READ_WRAPPED_STAMP
#undef READ_RACK_STAMP
#undef READ_DERIVED_STAMP
#undef READ_GENERAL_STAMP
#undef READ_RACK_STAMP

namespace core {

#ifdef DEBUG_DTREE_INTERPRETER // for debugging
#define DTILOG(...) { fprintf( DTILOG_fout, "%s", (fmt::sprintf(__VA_ARGS__).c_str())); fflush(DTILOG_fout); }
#define DTIDO(x) do { x; } while(0)
#define DTIDO_ALWAYS(x) x;
#else
#define DTILOG(...)
#define DTIDO(x) do {} while(0)
#define DTIDO_ALWAYS(x) do {} while(0)
#endif

#define DTREE_OP_MISS 0
#define DTREE_OP_ADVANCE 1
#define DTREE_OP_TAG_TEST 2
#define DTREE_OP_STAMP_READ 3
#define DTREE_OP_LT_BRANCH 4
#define DTREE_OP_EQ_CHECK 5
#define DTREE_OP_RANGE_CHECK 6
#define DTREE_OP_EQL 7
#define DTREE_OP_SLOT_READ 8
#define DTREE_OP_SLOT_WRITE 9
#define DTREE_OP_CAR 10
#define DTREE_OP_RPLACA 11
#define DTREE_OP_EFFECTIVE_METHOD 12

#define DTREE_OP_MISS_LENGTH 1

#define DTREE_FIXNUM_TAG_OFFSET 1
#define DTREE_SINGLE_FLOAT_TAG_OFFSET 2
#define DTREE_CHARACTER_TAG_OFFSET 3
#define DTREE_CONS_TAG_OFFSET 4
#define DTREE_GENERAL_TAG_OFFSET 5
#define DTREE_OP_TAG_TEST_LENGTH 6

#define DTREE_READ_HEADER_OFFSET 1
#define DTREE_READ_OTHER_OFFSET 2
#define DTREE_OP_STAMP_READ_LENGTH 3

#define DTREE_LT_PIVOT_OFFSET 1
#define DTREE_LT_LEFT_OFFSET 2
#define DTREE_LT_RIGHT_OFFSET 3
#define DTREE_OP_LT_BRANCH_LENGTH 4

#define DTREE_EQ_PIVOT_OFFSET 1
#define DTREE_EQ_NEXT_OFFSET 2
#define DTREE_OP_EQ_CHECK_LENGTH 3

#define DTREE_RANGE_MIN_OFFSET 1
#define DTREE_RANGE_MAX_OFFSET 2
#define DTREE_RANGE_NEXT_OFFSET 3
#define DTREE_OP_RANGE_CHECK_LENGTH 4

#define DTREE_EQL_OBJECT_OFFSET 1
#define DTREE_EQL_BRANCH_OFFSET 2
#define DTREE_EQL_NEXT_OFFSET 3
#define DTREE_OP_EQL_LENGTH 4

#define DTREE_SLOT_READER_INDEX_OFFSET 1
#define DTREE_SLOT_READER_SLOT_NAME_OFFSET 2
#define DTREE_OP_SLOT_READ_LENGTH 3
#define DTREE_OP_CAR_LENGTH 3

#define DTREE_SLOT_WRITER_INDEX_OFFSET 1
#define DTREE_OP_SLOT_WRITE_LENGTH 2
#define DTREE_OP_RPLACA_LENGTH 2

#define DTREE_EFFECTIVE_METHOD_OFFSET 1
#define DTREE_OP_EFFECTIVE_METHOD_LENGTH 2

SYMBOL_EXPORT_SC_(CorePkg,STARdtreeSymbolsSTAR);


void registerOneDtreeInfo(const std::string& name, int val ) {
  printf("%s:%d:%s  name: %s   val: %d\n", __FILE__, __LINE__, __FUNCTION__, name.c_str(), val );
  if (_sym_STARdtreeSymbolsSTAR->symbolValue().nilp()) {
    _sym_STARdtreeSymbolsSTAR->defparameter(HashTableEq_O::create_default());
  }
  HashTableEq_sp ht = gc::As<HashTableEq_sp>(_sym_STARdtreeSymbolsSTAR->symbolValue());
  Symbol_sp key = lisp_upcase_intern(name,"KEYWORD");
  ht->setf_gethash(key,make_fixnum(val));
}

void registerOrDumpDtreeInfo(std::ostream& fout) {
#define DTREE_EXPOSE(_name_) if (fout) fmt::fprintf(fout,"Init_global_ints(name=\"%s\",value=%d);\n", #_name_, _name_); else registerOneDtreeInfo(#_name_,_name_);
   DTREE_EXPOSE(DTREE_OP_MISS);
   DTREE_EXPOSE(DTREE_OP_ADVANCE);
   DTREE_EXPOSE(DTREE_OP_TAG_TEST);
   DTREE_EXPOSE(DTREE_OP_STAMP_READ);
   DTREE_EXPOSE(DTREE_OP_LT_BRANCH);
   DTREE_EXPOSE(DTREE_OP_EQ_CHECK);
   DTREE_EXPOSE(DTREE_OP_RANGE_CHECK);
   DTREE_EXPOSE(DTREE_OP_EQL);
   DTREE_EXPOSE(DTREE_OP_SLOT_READ);
   DTREE_EXPOSE(DTREE_OP_SLOT_WRITE);
   DTREE_EXPOSE(DTREE_OP_CAR);
   DTREE_EXPOSE(DTREE_OP_RPLACA);
   DTREE_EXPOSE(DTREE_OP_EFFECTIVE_METHOD);
   DTREE_EXPOSE(DTREE_OP_MISS_LENGTH);
   DTREE_EXPOSE(DTREE_FIXNUM_TAG_OFFSET);
   DTREE_EXPOSE(DTREE_SINGLE_FLOAT_TAG_OFFSET);
   DTREE_EXPOSE(DTREE_CHARACTER_TAG_OFFSET);
   DTREE_EXPOSE(DTREE_CONS_TAG_OFFSET);
   DTREE_EXPOSE(DTREE_GENERAL_TAG_OFFSET);
   DTREE_EXPOSE(DTREE_OP_TAG_TEST_LENGTH);
   DTREE_EXPOSE(DTREE_READ_HEADER_OFFSET);
   DTREE_EXPOSE(DTREE_READ_OTHER_OFFSET);
   DTREE_EXPOSE(DTREE_OP_STAMP_READ_LENGTH);
   DTREE_EXPOSE(DTREE_LT_PIVOT_OFFSET);
   DTREE_EXPOSE(DTREE_LT_LEFT_OFFSET);
   DTREE_EXPOSE(DTREE_LT_RIGHT_OFFSET);
   DTREE_EXPOSE(DTREE_OP_LT_BRANCH_LENGTH);
   DTREE_EXPOSE(DTREE_EQ_PIVOT_OFFSET);
   DTREE_EXPOSE(DTREE_EQ_NEXT_OFFSET);
   DTREE_EXPOSE(DTREE_OP_EQ_CHECK_LENGTH);
   DTREE_EXPOSE(DTREE_RANGE_MIN_OFFSET);
   DTREE_EXPOSE(DTREE_RANGE_MAX_OFFSET);
   DTREE_EXPOSE(DTREE_RANGE_NEXT_OFFSET);
   DTREE_EXPOSE(DTREE_OP_RANGE_CHECK_LENGTH);
   DTREE_EXPOSE(DTREE_EQL_OBJECT_OFFSET);
   DTREE_EXPOSE(DTREE_EQL_BRANCH_OFFSET);
   DTREE_EXPOSE(DTREE_EQL_NEXT_OFFSET);
   DTREE_EXPOSE(DTREE_OP_EQL_LENGTH);
   DTREE_EXPOSE(DTREE_SLOT_READER_INDEX_OFFSET);
   DTREE_EXPOSE(DTREE_SLOT_READER_SLOT_NAME_OFFSET);
   DTREE_EXPOSE(DTREE_OP_SLOT_READ_LENGTH);
   DTREE_EXPOSE(DTREE_OP_CAR_LENGTH);
   DTREE_EXPOSE(DTREE_SLOT_WRITER_INDEX_OFFSET);
   DTREE_EXPOSE(DTREE_OP_SLOT_WRITE_LENGTH);
   DTREE_EXPOSE(DTREE_OP_RPLACA_LENGTH);
   DTREE_EXPOSE(DTREE_EFFECTIVE_METHOD_OFFSET);
   DTREE_EXPOSE(DTREE_OP_EFFECTIVE_METHOD_LENGTH);
 }



  
#define CASE_OP_NAME(op) case op: return #op;
std::string dtree_op_name(int dtree_op) {
  switch (dtree_op) {
    CASE_OP_NAME(DTREE_OP_MISS);
    CASE_OP_NAME(DTREE_OP_ADVANCE);
    CASE_OP_NAME(DTREE_OP_TAG_TEST);
    CASE_OP_NAME(DTREE_OP_STAMP_READ);
    CASE_OP_NAME(DTREE_OP_LT_BRANCH);
    CASE_OP_NAME(DTREE_OP_EQ_CHECK);
    CASE_OP_NAME(DTREE_OP_RANGE_CHECK);
    CASE_OP_NAME(DTREE_OP_EQL);
    CASE_OP_NAME(DTREE_OP_SLOT_READ);
    CASE_OP_NAME(DTREE_OP_SLOT_WRITE);
    CASE_OP_NAME(DTREE_OP_CAR);
    CASE_OP_NAME(DTREE_OP_RPLACA);
    CASE_OP_NAME(DTREE_OP_EFFECTIVE_METHOD);
  default: return "UNKNOWN_OP";
  };
};

SYMBOL_EXPORT_SC_(ClosPkg,interp_wrong_nargs);
SYMBOL_EXPORT_SC_(ClosPkg, compile_discriminating_function);

#define COMPILE_TRIGGER 1024 // completely arbitrary

//
CL_LAMBDA(program gf core:&va-rest args)
CL_UNWIND_COOP(true);
DOCGROUP(clasp)
CL_DEFUN T_mv clos__interpret_dtree_program(SimpleVector_sp program, T_sp generic_function, Vaslist_sp pass_args) {
  DO_DRAG_INTERPRET_DTREE();
  DTIDO_ALWAYS(
      FILE* DTILOG_fout = monitor_file("dtree-interp");
               );
  DTILOG("=============================== Entered clos__interpret_dtree_program\n");
  DTILOG("---- generic function: %s\n" , _safe_rep_(generic_function));
  DTILOG("---- program length: %d\n" , program->length());
  DTIDO(
      for ( size_t i=0; i<program->length(); ++i ) {
          DTILOG("[%3d] : %5s ;; .tagged_() = %lu!!!!\n" , i , _safe_rep_((*program)[i]), (uintptr_t)((*program)[i].tagged_()));
      });
  size_t argi(0);
  
  // Increment the call count, and if it's high enough, compile the thing
  size_t calls = gc::As_unsafe<FuncallableInstance_sp>(generic_function)->increment_calls();
  //
  // if calls == COMPILE_TRIGGER then compile the discriminating function.
  //  ONLY use == here - so that compilation is only triggered once and if
  //  the GF is part of the compiler and it continues to be called while it is being
  //  compiled then you avoid a recursive cycle of compilations that will hang the system.
  //
  // Disable this for now
  // fixme2022
#if 1
  if (calls == COMPILE_TRIGGER) {
    eval::funcall(clos::_sym_compile_discriminating_function, generic_function);
  }
#endif
  
  // Regardless of whether we triggered the compile, we next
  // Dispatch
  DTILOG("About to dump incoming pass_args Vaslist and then copy to dispatch_args\n");
  DTIDO(dump_Vaslist_ptr(monitor_file("dtree-interp"),&*pass_args));
  Vaslist valist_copy(*pass_args);
  Vaslist_sp dispatch_args(&valist_copy);
  DTILOG("About to dump copied dispatch_args Vaslist\n");
  DTIDO(dump_Vaslist_ptr(monitor_file("dtree-interp"),&*dispatch_args));
  T_sp arg;
  uintptr_t stamp;
  size_t ip = 0; // instruction pointer
  size_t nargs = dispatch_args->nargs(); // used in error signalling
  while (1) {
    size_t op = (*program)[ip].unsafe_fixnum();
    DTILOG("ip[%lu]: %lu/%s\n" , ip , op , dtree_op_name(op));
    switch (op) {
    case DTREE_OP_MISS:
        goto DISPATCH_MISS;
    case DTREE_OP_ADVANCE: {
      DTILOG("About to read arg dispatch_args-> %p\n" , (void*)dispatch_args.raw_());
      DTILOG("About to dump dispatch_args Vaslist\n");
      DTIDO(dump_Vaslist_ptr(monitor_file("dtree-interp"),&*dispatch_args));
      if (dispatch_args->nargs_zero())
          // we use an intermediate function, in lisp, to get a nice error message.
        return core::eval::funcall(clos::_sym_interp_wrong_nargs,
                                   generic_function, make_fixnum(nargs));
      arg = dispatch_args->next_arg();
      DTILOG("Got arg@%p %s\n" , (void*)arg.raw_() , _safe_rep_(arg));
      ++ip;
    }
        break;
    case DTREE_OP_TAG_TEST:
        DTILOG("tag-test: ");
        if (arg.fixnump()) {
          DTILOG("fixnum\n");
          ip = (*program)[ip+DTREE_FIXNUM_TAG_OFFSET].unsafe_fixnum();
          break;
        } else if (arg.consp()) {
          DTILOG("cons\n");
          ip = (*program)[ip+DTREE_CONS_TAG_OFFSET].unsafe_fixnum();
          break;
        } else if (arg.single_floatp()) {
          DTILOG("single-float\n");
          ip = (*program)[ip+DTREE_SINGLE_FLOAT_TAG_OFFSET].unsafe_fixnum();
          break;
        } else if (arg.characterp()) {
          DTILOG("character\n");
          ip = (*program)[ip+DTREE_CHARACTER_TAG_OFFSET].unsafe_fixnum();
          break;
        } else if (arg.generalp()) {
          DTILOG("general\n");
          ip += DTREE_GENERAL_TAG_OFFSET;
          break;
        }
        DTILOG("unknown\n");
        // FIXME: We should be able to specialize on class valist and stuff.
        SIMPLE_ERROR(("unknown tag for arg %s") , arg);
        goto DISPATCH_MISS;
    case DTREE_OP_STAMP_READ:
      {
        General_O* client_ptr = gctools::untag_general<General_O*>((General_O*)arg.raw_());
        stamp = (uintptr_t)(llvmo::template_read_general_stamp(client_ptr));
        uintptr_t where = stamp & gctools::Header_s::where_mask;
        switch (where) {
        case gctools::Header_s::header_wtag:
            ip = (*program)[ip+DTREE_READ_HEADER_OFFSET].unsafe_fixnum(); break;
        case gctools::Header_s::rack_wtag:
            stamp = (uintptr_t)(llvmo::template_read_rack_stamp(client_ptr));
            ip += DTREE_READ_OTHER_OFFSET; break;
        case gctools::Header_s::wrapped_wtag:
            stamp = (uintptr_t)(llvmo::template_read_wrapped_stamp(client_ptr));
            ip += DTREE_READ_OTHER_OFFSET; break;
        case gctools::Header_s::derivable_wtag:
            stamp = (uintptr_t)(llvmo::template_read_derived_stamp(client_ptr));
            ip += DTREE_READ_OTHER_OFFSET; break;
        }
        DTILOG(" stamp read: %lu\n" , stamp );
        break;
      }
    case DTREE_OP_LT_BRANCH:
      {
        // The stamps are from Common Lisp, so they're tagged fixnums. Don't untag.
        uintptr_t pivot = (*program)[ip+DTREE_LT_PIVOT_OFFSET].tagged_();
        DTILOG("testing < pivot %lu\n" , pivot);
        if (stamp < pivot)
          ip = (*program)[ip+DTREE_LT_LEFT_OFFSET].unsafe_fixnum();
        else ip += DTREE_LT_RIGHT_OFFSET;
        break;
      }
    case DTREE_OP_EQ_CHECK:
      {
        uintptr_t pivot = (*program)[ip+DTREE_EQ_PIVOT_OFFSET].tagged_();
        DTILOG("testing - pivot %lu  stamp: %lu  EQ -> %d\n" , pivot , stamp , (stamp == pivot) );
        if (stamp != pivot) goto DISPATCH_MISS;
        ip += DTREE_EQ_NEXT_OFFSET;
        break;
      }
    case DTREE_OP_RANGE_CHECK:
      {
        uintptr_t min = (*program)[ip+DTREE_RANGE_MIN_OFFSET].tagged_();
        uintptr_t max = (*program)[ip+DTREE_RANGE_MAX_OFFSET].tagged_();
        DTILOG("testing > %lu and < %lu\n" , min , max);
        if (stamp < min || stamp > max) goto DISPATCH_MISS;
        ip += DTREE_RANGE_NEXT_OFFSET;
        break;
      }
    case DTREE_OP_EQL:
      {
        T_sp object = (*program)[ip+DTREE_EQL_OBJECT_OFFSET];
        if (cl__eql(arg, object))
          ip = (*program)[ip+DTREE_EQL_BRANCH_OFFSET].unsafe_fixnum();
        else ip += DTREE_EQL_NEXT_OFFSET;
        break;
      }
    case DTREE_OP_SLOT_READ:
      {
        DTILOG("reading slot: ");
        T_sp location = (*program)[ip+DTREE_SLOT_READER_INDEX_OFFSET];
        T_sp slot_name = (*program)[ip+DTREE_SLOT_READER_SLOT_NAME_OFFSET];
        size_t index = location.unsafe_fixnum();
        DTILOG("About to dump pass_args Vaslist\n");
        DTIDO(dump_Vaslist_ptr(monitor_file("dtree-interp"),&*pass_args));
        T_sp tinstance = pass_args->next_arg();
        DTILOG("Got tinstance@%p %s\n" , (void*)tinstance.raw_() , _safe_rep_(tinstance));
        DTILOG("About to dump pass_args Vaslist AFTER next_arg\n");
        DTIDO(dump_Vaslist_ptr(monitor_file("dtree-interp"),&*pass_args));
        Instance_sp instance((gc::Tagged)tinstance.raw_());
        DTILOG("instance %p index %lu\n" , (void*)instance.raw_() , index);
        T_sp value = instance->instanceRef(index);
        if (value.unboundp())
          return core::eval::funcall(cl::_sym_slot_unbound,
                                     lisp_instance_class(tinstance),
                                     instance,slot_name);
        return gctools::return_type(value.raw_(),1);
      }
    case DTREE_OP_CAR:
      {
        DTILOG("class cell\n");
        T_sp location = (*program)[ip+DTREE_SLOT_READER_INDEX_OFFSET];
        T_sp slot_name = (*program)[ip+DTREE_SLOT_READER_SLOT_NAME_OFFSET];
        DTILOG("About to dump pass_args Vaslist\n");
        DTIDO(dump_Vaslist_ptr(monitor_file("dtree-interp"),&*pass_args));
        Instance_sp instance = gc::As_unsafe<Instance_sp>(pass_args->next_arg());
        DTILOG("Got instance@%p %s\n" , (void*)instance.raw_() , _safe_rep_(instance));
        Cons_sp cell = gc::As_unsafe<Cons_sp>(location);
        T_sp value = CONS_CAR(cell);
        if (value.unboundp())
          return core::eval::funcall(cl::_sym_slot_unbound,
                                     lisp_instance_class(instance),
                                     instance,slot_name);
        return gctools::return_type(value.raw_(),1);
      }
    case DTREE_OP_SLOT_WRITE:
      {
        DTILOG("writing slot: ");
        T_sp location = (*program)[ip+DTREE_SLOT_WRITER_INDEX_OFFSET];
        size_t index = location.unsafe_fixnum();
        DTILOG("index %lu\n" , index);
        DTILOG("About to dump pass_args Vaslist\n");
        DTIDO(dump_Vaslist_ptr(monitor_file("dtree-interp"),&*pass_args));
        T_sp value((gc::Tagged)pass_args->next_arg_raw());
        DTILOG("About to dump pass_args Vaslist\n");
        DTIDO(dump_Vaslist_ptr(monitor_file("dtree-interp"),&*pass_args));
        T_sp tinstance = pass_args->next_arg();
        DTILOG("Got tinstance@%p %s\n" , (void*)tinstance.raw_() , _safe_rep_(tinstance));
        Instance_sp instance((gc::Tagged)tinstance.raw_());
        instance->instanceSet(index,value);
        return gctools::return_type(value.raw_(),1);
      }
    case DTREE_OP_RPLACA:
      {
        DTILOG("class cell\n");
        T_sp location = (*program)[ip+DTREE_SLOT_WRITER_INDEX_OFFSET];
        size_t index = location.unsafe_fixnum();
        Cons_sp cell = gc::As_unsafe<Cons_sp>(location);
        DTILOG("About to dump pass_args Vaslist\n");
        DTIDO(dump_Vaslist_ptr(monitor_file("dtree-interp"),&*pass_args));
        T_sp value((gc::Tagged)pass_args->next_arg());
        DTILOG("Got value@%p %s\n" , (void*)value.raw_() , _safe_rep_(value));
        cell->rplaca(value);
        return gctools::return_type(value.raw_(),1);
      }
    case DTREE_OP_EFFECTIVE_METHOD:
      {
        DTILOG("effective method call\n");
        T_sp tfunc = (*program)[ip+DTREE_EFFECTIVE_METHOD_OFFSET];
        Function_sp func = gc::As_unsafe<Function_sp>(tfunc);
        // Use the pass_args here because it points to the original arguments
        DTILOG("About to dump pass_args Vaslist\n");
        DTIDO(dump_Vaslist_ptr(monitor_file("dtree-interp"),&*pass_args));
        return func->entry()(func.raw_(), pass_args->nargs(), pass_args->args());
      }
    default:
        SIMPLE_ERROR(("%zu is not a valid dtree opcode") , op);
    }
  }
 DISPATCH_MISS:
  DTILOG("dispatch miss. arg %lu stamp %lu\n" , arg , stamp);
  return core::eval::funcall(clos::_sym_dispatch_miss_va,generic_function,pass_args);
}

SYMBOL_EXPORT_SC_(KeywordPkg,force_compile);
SYMBOL_EXPORT_SC_(KeywordPkg,generic_function_name);

DOCGROUP(clasp)
CL_DEFUN void core__verify_funcallable_instance_layout(size_t funcallableInstance_size, size_t funcallableInstance_rack_offset)
  {
    if (funcallableInstance_size!=sizeof(FuncallableInstance_O)) SIMPLE_ERROR(("The cmpintrinsics.lisp funcallableInstance_size %lu does not match sizeof(FuncallableInstance_O) %lu") , funcallableInstance_size , sizeof(FuncallableInstance_O));
    if (funcallableInstance_rack_offset!=offsetof(FuncallableInstance_O,_Rack))
      SIMPLE_ERROR(("funcallableInstance_rack_offset %lu does not match offsetof(_Rack,FuncallableInstance_O) %lu") , funcallableInstance_rack_offset , offsetof(FuncallableInstance_O,_Rack));
  }

};
