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


void FuncallableInstance_O::initializeSlots(gctools::ShiftedStamp stamp,
                                            T_sp sig, size_t numberOfSlots) {
  ASSERT(gctools::Header_s::StampWtagMtag::is_rack_shifted_stamp(stamp));
  this->_Rack = Rack_O::make(numberOfSlots,sig,_Unbound<T_O>());
  this->stamp_set(stamp);
#ifdef DEBUG_GUARD_VALIDATE
  client_validate(rack());
#endif
}

void FuncallableInstance_O::initializeClassSlots(Creator_sp creator, gctools::ShiftedStamp stamp) {
  ASSERT(gctools::Header_s::StampWtagMtag::is_rack_shifted_stamp(stamp));
  DEPRECATED();
}

// Identical to allocate_new_instance in instance.cc, except for the type.
CL_DEFUN T_sp core__allocate_new_funcallable_instance(Instance_sp cl, size_t numberOfSlots) {
  // cl is known to be a funcallable-standard-class.
  ASSERT(cl->CLASS_has_creator());
  Creator_sp creator = gctools::As<Creator_sp>(cl->CLASS_get_creator());
  FuncallableInstance_sp obj = gc::As_unsafe<FuncallableInstance_sp>(creator->creator_allocate());
  obj->_Class = cl;
  obj->initializeSlots(cl->CLASS_stamp_for_instances(), cl->slots(), numberOfSlots);
  return obj;
}

CL_DEFUN FuncallableInstance_sp core__reallocate_funcallable_instance(FuncallableInstance_sp instance,
                                                                      Instance_sp new_class,
                                                                      size_t new_size) {
  instance->_Class = new_class;
  instance->initializeSlots(new_class->CLASS_stamp_for_instances(),
                            new_class->slots(), new_size);
  return instance;
}

size_t FuncallableInstance_O::rack_stamp_offset() {
  SimpleVector_O dummy_rack(0);
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

LCC_RETURN FuncallableInstance_O::funcallable_entry_point(LCC_ARGS_ELLIPSIS) {
  SETUP_CLOSURE(FuncallableInstance_O,closure);
  INCREMENT_FUNCTION_CALL_COUNTER(closure);
  // We need to be sure to load the GFUN_DISPATCHER only once.
  // We used to load it twice, which caused a race condition in that other threads
  // could call setFuncallableInstanceFunction between the loads, meaning we called
  // the code for one function but pass it the closure object for another.
  T_sp funcallable_closure = closure->GFUN_DISPATCHER();
  if (lcc_nargs<=LCC_ARGS_IN_REGISTERS) {
    return (gc::As_unsafe<Function_sp>(funcallable_closure)->entry.load())(funcallable_closure.raw_(),lcc_nargs,lcc_fixed_arg0,lcc_fixed_arg1,lcc_fixed_arg2,lcc_fixed_arg3);
  }
  INITIALIZE_VA_LIST();
  // This is where we could decide to compile the dtree and switch the GFUN_DISPATCHER() or not
//  printf("%s:%d:%s About to call %s\n", __FILE__, __LINE__, __FUNCTION__, _rep_(closure->functionName()).c_str());
  return funcall_consume_valist_<core::Function_O>(funcallable_closure.tagged_(),lcc_vargs);
}

T_sp FuncallableInstance_O::copyInstance() const {
  DEPRECATED();
  Instance_sp cl = this->_Class;
  FuncallableInstance_sp copy = gc::As_unsafe<FuncallableInstance_sp>(cl->CLASS_get_creator()->creator_allocate());
  copy->_Class = cl;
  copy->_Rack = rack();
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
  for (int i(1); i < rack()->length(); ++i) {
    ss << (BF("_Rack[%d]: %s\n") % i % _rep_(low_level_instanceRef(rack(), i)).c_str()).str();
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

CL_DEFUN size_t clos__generic_function_interpreted_calls(FuncallableInstance_sp gf) {
  return gf->interpreted_calls();
}

CL_DEFUN T_sp clos__generic_function_compiled_dispatch_function(T_sp obj) {
  return gc::As<FuncallableInstance_sp>(obj)->GFUN_DISPATCHER();
}
CL_DEFUN void clos__set_generic_function_compiled_dispatch_function(T_sp obj, T_sp val) {
  gc::As<FuncallableInstance_sp>(obj)->GFUN_DISPATCHER_set(val);
}
}

#include <clasp/llvmo/read-stamp.cc>

namespace core {

#if 0 // for debugging
#define DTILOG(x) { FILE* fout= monitor_file("dtree-interp"); fprintf( fout, "%s", (x).str().c_str()); fflush(fout); }
#define DTIDO(x) { x; };
#else
#define DTILOG(x)
#define DTIDO(x)
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

#define DTREE_FIXNUM_TAG_OFFSET 1
#define DTREE_SINGLE_FLOAT_TAG_OFFSET 2
#define DTREE_CHARACTER_TAG_OFFSET 3
#define DTREE_CONS_TAG_OFFSET 4
#define DTREE_GENERAL_TAG_OFFSET 5

#define DTREE_READ_HEADER_OFFSET 1
#define DTREE_READ_OTHER_OFFSET 2

#define DTREE_LT_PIVOT_OFFSET 1
#define DTREE_LT_LEFT_OFFSET 2
#define DTREE_LT_RIGHT_OFFSET 3

#define DTREE_EQ_PIVOT_OFFSET 1
#define DTREE_EQ_NEXT_OFFSET 2

#define DTREE_RANGE_MIN_OFFSET 1
#define DTREE_RANGE_MAX_OFFSET 2
#define DTREE_RANGE_NEXT_OFFSET 3

#define DTREE_EQL_OBJECT_OFFSET 1
#define DTREE_EQL_BRANCH_OFFSET 2
#define DTREE_EQL_NEXT_OFFSET 3

#define DTREE_SLOT_READER_INDEX_OFFSET 1
#define DTREE_SLOT_READER_SLOT_NAME_OFFSET 2

#define DTREE_SLOT_WRITER_INDEX_OFFSET 1

#define DTREE_EFFECTIVE_METHOD_OFFSET 1

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

CL_LAMBDA(program gf args);
CL_DEFUN T_mv clos__interpret_dtree_program(SimpleVector_sp program, T_sp generic_function,
                                            VaList_sp args) {
  DTILOG(BF("=============================== Entered clos__interpret_dtree_program\n"));
  DTILOG(BF("---- generic function: %s\n") % _safe_rep_(generic_function));
  DTILOG(BF("---- program length: %d\n") % program->length());
  for ( size_t i=0; i<program->length(); ++i ) {
    DTILOG(BF("[%3d] : %s\n") % i % _safe_rep_((*program)[i]));
  }
#if 0 // Compilation disabled for now because it increases build time
  // Increment the call count, and if it's high enough, compile the thing
  size_t calls = gc::As_unsafe<FuncallableInstance_sp>(generic_function)->increment_calls();
  if (calls >= COMPILE_TRIGGER)
    eval::funcall(clos::_sym_compile_discriminating_function, generic_function);
#endif
  // Regardless of whether we triggered the compile, we next
  // Dispatch
  Vaslist valist_copy(*args);
  VaList_sp dispatch_args(&valist_copy);
  DTILOG(BF("About to dump incoming args Vaslist\n"));
  DTIDO(dump_Vaslist_ptr(monitor_file("dtree-interp"),&*args));
  DTILOG(BF("About to dump copied dispatch_args Vaslist\n"));
  DTIDO(dump_Vaslist_ptr(monitor_file("dtree-interp"),&*dispatch_args));
  T_sp arg;
  uintptr_t stamp;
  size_t ip = 0; // instruction pointer
  size_t nargs = dispatch_args->remaining_nargs(); // used in error signalling
  while (1) {
    size_t op = (*program)[ip].unsafe_fixnum();
    DTILOG(BF("ip[%lu]: %lu/%s\n") % ip % op % dtree_op_name(op));
    switch (op) {
    case DTREE_OP_MISS:
        goto DISPATCH_MISS;
    case DTREE_OP_ADVANCE:
        DTILOG(BF("About to read arg dispatch_args-> %p\n") % dispatch_args.raw_());
        DTILOG(BF("About to dump dispatch_args Vaslist\n"));
        DTIDO(dump_Vaslist_ptr(monitor_file("dtree-interp"),&*dispatch_args));
        if (dispatch_args->remaining_nargs() == 0)
          // we use an intermediate function, in lisp, to get a nice error message.
          return core::eval::funcall(clos::_sym_interp_wrong_nargs,
                                     generic_function, make_fixnum(nargs));
        arg = dispatch_args->next_arg();
        DTILOG(BF("Got arg@%p %s\n") % arg.raw_() % _safe_rep_(arg));
        ++ip;
        break;
    case DTREE_OP_TAG_TEST:
        DTILOG(BF("tag-test: "));
        if (arg.fixnump()) {
          DTILOG(BF("fixnum\n"));
          ip = (*program)[ip+DTREE_FIXNUM_TAG_OFFSET].unsafe_fixnum();
          break;
        } else if (arg.consp()) {
          DTILOG(BF("cons\n"));
          ip = (*program)[ip+DTREE_CONS_TAG_OFFSET].unsafe_fixnum();
          break;
        } else if (arg.single_floatp()) {
          DTILOG(BF("single-float\n"));
          ip = (*program)[ip+DTREE_SINGLE_FLOAT_TAG_OFFSET].unsafe_fixnum();
          break;
        } else if (arg.characterp()) {
          DTILOG(BF("character\n"));
          ip = (*program)[ip+DTREE_CHARACTER_TAG_OFFSET].unsafe_fixnum();
          break;
        } else if (arg.generalp()) {
          DTILOG(BF("general\n"));
          ip += DTREE_GENERAL_TAG_OFFSET;
          break;
        }
        DTILOG(BF("unknown\n"));
        // FIXME: We should be able to specialize on class valist and stuff.
        SIMPLE_ERROR(BF("unknown tag for arg %s") % arg);
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
        break;
      }
    case DTREE_OP_LT_BRANCH:
      {
        // The stamps are from Common Lisp, so they're tagged fixnums. Don't untag.
        uintptr_t pivot = (*program)[ip+DTREE_LT_PIVOT_OFFSET].tagged_();
        DTILOG(BF("testing < pivot %s\n") % pivot);
        if (stamp < pivot)
          ip = (*program)[ip+DTREE_LT_LEFT_OFFSET].unsafe_fixnum();
        else ip += DTREE_LT_RIGHT_OFFSET;
        break;
      }
    case DTREE_OP_EQ_CHECK:
      {
        uintptr_t pivot = (*program)[ip+DTREE_EQ_PIVOT_OFFSET].tagged_();
        DTILOG(BF("testing = pivot %s\n") % pivot);
        if (stamp != pivot) goto DISPATCH_MISS;
        ip += DTREE_EQ_NEXT_OFFSET;
        break;
      }
    case DTREE_OP_RANGE_CHECK:
      {
        uintptr_t min = (*program)[ip+DTREE_RANGE_MIN_OFFSET].tagged_();
        uintptr_t max = (*program)[ip+DTREE_RANGE_MAX_OFFSET].tagged_();
        DTILOG(BF("testing > %s and < %s\n") % min % max);
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
        DTILOG(BF("reading slot: "));
        T_sp location = (*program)[ip+DTREE_SLOT_READER_INDEX_OFFSET];
        T_sp slot_name = (*program)[ip+DTREE_SLOT_READER_SLOT_NAME_OFFSET];
        size_t index = location.unsafe_fixnum();
        DTILOG(BF("About to dump args Vaslist\n"));
        DTIDO(dump_Vaslist_ptr(monitor_file("dtree-interp"),&*args));
        T_sp tinstance = args->next_arg();
        DTILOG(BF("tinstance.raw_() -> %p\n") % tinstance.raw_());
        DTILOG(BF("About to dump args Vaslist AFTER next_arg\n"));
        DTIDO(dump_Vaslist_ptr(monitor_file("dtree-interp"),&*args));
        Instance_sp instance((gc::Tagged)tinstance.raw_());
        DTILOG(BF("instance %p index %s\n") % instance.raw_() % index);
        T_sp value = instance->instanceRef(index);
        if (value.unboundp())
          return core::eval::funcall(cl::_sym_slot_unbound,
                                     lisp_instance_class(tinstance),
                                     instance,slot_name);
        return gctools::return_type(value.raw_(),1);
      }
    case DTREE_OP_CAR:
      {
        DTILOG(BF("class cell\n"));
        T_sp location = (*program)[ip+DTREE_SLOT_READER_INDEX_OFFSET];
        T_sp slot_name = (*program)[ip+DTREE_SLOT_READER_SLOT_NAME_OFFSET];
        DTILOG(BF("About to dump args Vaslist\n"));
        DTIDO(dump_Vaslist_ptr(monitor_file("dtree-interp"),&*args));
        Instance_sp instance((gc::Tagged)args->next_arg().raw_());
        Cons_sp cell = gc::As_unsafe<Cons_sp>(location);
        T_sp value = oCar(cell);
        if (value.unboundp())
          return core::eval::funcall(cl::_sym_slot_unbound,
                                     lisp_instance_class(instance),
                                     instance,slot_name);
        return gctools::return_type(value.raw_(),1);
      }
    case DTREE_OP_SLOT_WRITE:
      {
        DTILOG(BF("writing slot: "));
        T_sp location = (*program)[ip+DTREE_SLOT_WRITER_INDEX_OFFSET];
        size_t index = location.unsafe_fixnum();
        DTILOG(BF("index %s\n") % index);
        DTILOG(BF("About to dump args Vaslist\n"));
        DTIDO(dump_Vaslist_ptr(monitor_file("dtree-interp"),&*args));
        T_sp value((gc::Tagged)args->next_arg().raw_());
        DTILOG(BF("About to dump args Vaslist\n"));
        DTIDO(dump_Vaslist_ptr(monitor_file("dtree-interp"),&*args));
        T_sp tinstance = args->next_arg();
        Instance_sp instance((gc::Tagged)tinstance.raw_());
        instance->instanceSet(index,value);
        return gctools::return_type(value.raw_(),1);
      }
    case DTREE_OP_RPLACA:
      {
        DTILOG(BF("class cell\n"));
        T_sp location = (*program)[ip+DTREE_SLOT_WRITER_INDEX_OFFSET];
        size_t index = location.unsafe_fixnum();
        Cons_sp cell = gc::As_unsafe<Cons_sp>(location);
        DTILOG(BF("About to dump args Vaslist\n"));
        DTIDO(dump_Vaslist_ptr(monitor_file("dtree-interp"),&*args));
        T_sp value((gc::Tagged)args->next_arg().raw_());
        cell->rplaca(value);
        return gctools::return_type(value.raw_(),1);
      }
    case DTREE_OP_EFFECTIVE_METHOD:
      {
        DTILOG(BF("effective method call\n"));
        T_sp tfunc = (*program)[ip+DTREE_EFFECTIVE_METHOD_OFFSET];
        Function_sp func = gc::As_unsafe<Function_sp>(tfunc);
        return funcall_consume_valist_<core::Function_O>(func.tagged_(), args);
      }
    default:
        SIMPLE_ERROR(BF("%zu is not a valid dtree opcode") % op);
    }
  }
 DISPATCH_MISS:
  DTILOG(BF("dispatch miss. arg %s stamp %s\n") % arg % stamp);
  return core::eval::funcall(clos::_sym_dispatch_miss_va,generic_function,args);
}

SYMBOL_EXPORT_SC_(ClosPkg,codegen_dispatcher);
SYMBOL_EXPORT_SC_(KeywordPkg,force_compile);
SYMBOL_EXPORT_SC_(KeywordPkg,generic_function_name);

  CL_DEFUN void core__verify_funcallable_instance_layout(size_t funcallableInstance_size, size_t funcallableInstance_rack_offset)
  {
    if (funcallableInstance_size!=sizeof(FuncallableInstance_O)) SIMPLE_ERROR(BF("The cmpintrinsics.lsp funcallableInstance_size %lu does not match sizeof(FuncallableInstance_O) %lu") % funcallableInstance_size % sizeof(FuncallableInstance_O));
    if (funcallableInstance_rack_offset!=offsetof(FuncallableInstance_O,_Rack))
      SIMPLE_ERROR(BF("funcallableInstance_rack_offset %lu does not match offsetof(_Rack,FuncallableInstance_O) %lu") % funcallableInstance_rack_offset % offsetof(FuncallableInstance_O,_Rack));
  }

};
