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
#include <clasp/core/lispList.h>
#include <clasp/core/hashTable.h>
#include <clasp/core/hashTableEq.h>
#include <clasp/core/evaluator.h>
#include <clasp/core/genericFunction.h>
#include <clasp/core/funcallableInstance.h>
#include <clasp/core/wrappers.h>

namespace core {


void FuncallableInstance_O::initializeSlots(gctools::Stamp stamp, size_t numberOfSlots) {
  this->_Rack = SimpleVector_O::make(numberOfSlots+1,_Unbound<T_O>(),true);
  this->stamp_set(stamp);
#ifdef DEBUG_GUARD_VALIDATE
  client_validate(this->_Rack);
#endif
}

void FuncallableInstance_O::initializeClassSlots(Creator_sp creator, gctools::Stamp stamp) {
  DEPRECATED();
}

// Identical to allocate_new_instance in instance.cc, except for the type.
CL_DEFUN T_sp core__allocate_new_funcallable_instance(Instance_sp cl, size_t numberOfSlots) {
  // cl is known to be a funcallable-standard-class.
  ASSERT(cl->CLASS_has_creator());
  Creator_sp creator = gctools::As<Creator_sp>(cl->CLASS_get_creator());
  FuncallableInstance_sp obj = creator->creator_allocate();
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
  return (*this->_Rack)[0].unsafe_fixnum();
};

void FuncallableInstance_O::stamp_set(Fixnum s) {
  (*this->_Rack)[0] = clasp_make_fixnum(s);
};

size_t FuncallableInstance_O::numberOfSlots() const {
  return this->_Rack->length()-1;
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
  printf("\nMLOG INSTANCE-REF[%d] of Instance %p --->%s\n", idx, (void *)(this), this->_Rack[idx+1]->__repr__().c_str());
#endif
  return ((*this->_Rack)[idx+1]);
}
T_sp FuncallableInstance_O::instanceSet(size_t idx, T_sp val) {
#if DEBUG_CLOS >= 2
  printf("\nMLOG SI-INSTANCE-SET[%d] of Instance %p to val: %s\n", idx, (void *)(this), val->__repr__().c_str());
#endif
  (*this->_Rack)[idx+1] = val;
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
  if (this->isgf()) {
      ss << _rep_(this->GFUN_NAME());
  }
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

LCC_RETURN FuncallableInstance_O::invalidated_entry_point(LCC_ARGS_ELLIPSIS) {
  SETUP_CLOSURE(FuncallableInstance_O,closure);
  INCREMENT_FUNCTION_CALL_COUNTER(closure);
  INITIALIZE_VA_LIST();
  return core::eval::funcall(clos::_sym_invalidated_dispatch_function, closure->asSmartPtr(), lcc_vargs);
}

LCC_RETURN FuncallableInstance_O::not_funcallable_entry_point(LCC_ARGS_ELLIPSIS) {
  SETUP_CLOSURE(FuncallableInstance_O,closure);
  INCREMENT_FUNCTION_CALL_COUNTER(closure);
  INITIALIZE_VA_LIST();
  return core::eval::funcall(clos::_sym_not_funcallable_dispatch_function, closure->asSmartPtr(), lcc_vargs);
}

Instance_sp FuncallableInstance_O::create(Symbol_sp symbol, Instance_sp metaClass, Creator_sp creator ) {
  DEPRECATED();
};

T_sp FuncallableInstance_O::copyInstance() const {
  DEPRECATED();
  Instance_sp cl = this->_Class;
  FuncallableInstance_sp copy = cl->CLASS_get_creator()->creator_allocate();
  copy->_Class = cl;
  copy->_Rack = this->_Rack;
  copy->_Sig = this->_Sig;
  copy->_isgf = this->_isgf;
  return copy;
}

T_sp FuncallableInstance_O::setFuncallableInstanceFunction(T_sp functionOrT) {
  SYMBOL_EXPORT_SC_(ClPkg, standardGenericFunction);
  SYMBOL_SC_(ClosPkg, standardOptimizedReaderFunction);
  SYMBOL_SC_(ClosPkg, standardOptimizedWriterFunction);
  if (functionOrT == clos::_sym_invalidated_dispatch_function) {
    this->_isgf = CLASP_INVALIDATED_DISPATCH;
    // FIXME Jump straight to the invalidated-dispatch-function
    this->entry.store(this->invalidated_entry_point);
  } else if (functionOrT.nilp()) {
    this->_isgf = CLASP_NOT_FUNCALLABLE;
    this->entry.store(this->not_funcallable_entry_point);
  } else if (gc::IsA<Function_sp>(functionOrT)) {
    this->_isgf = CLASP_NORMAL_DISPATCH;
    this->GFUN_DISPATCHER_set(functionOrT);
    this->entry.store(gc::As_unsafe<Function_sp>(functionOrT)->entry.load());
  } else {
    TYPE_ERROR(functionOrT, cl::_sym_function);
    //SIMPLE_ERROR(BF("Wrong type argument: %s") % functionOrT->__repr__());
  }

  return ((this->sharedThis<FuncallableInstance_O>()));
}

void FuncallableInstance_O::describe(T_sp stream) {
  stringstream ss;
  ss << (BF("FuncallableInstance\n")).str();
  ss << (BF("isgf %d\n") % this->_isgf).str();
  ss << (BF("_Class: %s\n") % _rep_(this->_Class).c_str()).str();
  for (int i(1); i < this->_Rack->length(); ++i) {
    ss << (BF("_Rack[%d]: %s\n") % i % _rep_((*this->_Rack)[i]).c_str()).str();
  }
  clasp_write_string(ss.str(), stream);
}

CL_DEFUN void core__set_funcallable_instance_debug_on(FuncallableInstance_sp instance, bool debugOn) {
  instance->_DebugOn = debugOn;
}

CL_DEFUN bool core__get_funcallable_instance_debug_on(FuncallableInstance_sp instance) {
  return instance->_DebugOn;
}

CL_DEFUN T_mv clos__getFuncallableInstanceFunction(T_sp obj) {
  if (FuncallableInstance_sp iobj = obj.asOrNull<FuncallableInstance_O>()) {
    switch (iobj->_isgf) {
    case CLASP_NORMAL_DISPATCH:
        return Values(_lisp->_true(),Pointer_O::create((void*)iobj->entry.load()));
    case CLASP_INVALIDATED_DISPATCH:
        return Values(clos::_sym_invalidated_dispatch_function,Pointer_O::create((void*)iobj->entry.load()));
    case CLASP_NOT_FUNCALLABLE:
        return Values(clos::_sym_not_funcallable);
    }
    return Values(clasp_make_fixnum(iobj->_isgf),_Nil<T_O>());
  }
  return Values(_Nil<T_O>(),_Nil<T_O>());
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

};
