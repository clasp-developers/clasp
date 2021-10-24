/*
    File: singleDispatchGenericFunction.h
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
#ifndef _SINGLEDISPATCHGENERICFUNCTION_H_
#define _SINGLEDISPATCHGENERICFUNCTION_H_

#include <clasp/core/hashTable.fwd.h>
#include <clasp/core/singleDispatchGenericFunction.fwd.h>
#include <clasp/core/singleDispatchMethod.fwd.h>
#include <clasp/core/singleDispatchMethod.h>
#include <atomic>

namespace core {
FORWARD(SingleDispatchMethod);
class SingleDispatchGenericFunction_O : public Closure_O {
  LISP_CLASS(core,CorePkg,SingleDispatchGenericFunction_O,"SingleDispatchGenericFunction",Closure_O);
public:
  SingleDispatchGenericFunction_O(GlobalEntryPoint_sp ep) : Base(ep) {};
public:
  typedef enum {
      REF_SINGLE_DISPATCH_SPECIALIZER_CALL_HISTORY = 0,
      REF_SINGLE_DISPATCH_SPECIALIZER_LAMBDA_LIST_HANDLER = 1,
      REF_SINGLE_DISPATCH_SPECIALIZER_DISPATCH_ARGUMENT_INDEX = 2,
      REF_SINGLE_DISPATCH_SPECIALIZER_METHODS = 3,
      REF_SINGLE_DISPATCH_SPECIALIZER_SLOTS = 4
  } SingleDispatchSlots;
public:
  static SingleDispatchGenericFunction_sp create_single_dispatch_generic_function(T_sp gfname, LambdaListHandler_sp llhandler, size_t singleDispatchArgumentIndex);
public:
  std::atomic<T_sp> callHistory;
  LambdaListHandler_sp lambdaListHandler;
  Fixnum_sp argumentIndex;
  std::atomic<T_sp> methods;

  static inline LCC_RETURN LISP_CALLING_CONVENTION() {
    SETUP_CLOSURE(SingleDispatchGenericFunction_O,closure);
    INCREMENT_FUNCTION_CALL_COUNTER(closure);
    size_t singleDispatchArgumentIndex = closure->argumentIndex.unsafe_fixnum();
    Instance_sp dispatchArgClass;
    // SingleDispatchGenericFunctions can dispatch on the first or second argument
    // so we need this switch here.
#ifdef DEBUG_EVALUATE
    if (_sym_STARdebugEvalSTAR && _sym_STARdebugEvalSTAR->symbolValue().notnilp()) {
      printf("%s:%d single dispatch arg0 %s\n", __FILE__, __LINE__, _rep_(LCC_ARG0()).c_str());
    }
#endif
    switch (singleDispatchArgumentIndex) {
    case 0:
      dispatchArgClass = lisp_instance_class(LCC_ARG0());
      break;
    case 1:
      dispatchArgClass = lisp_instance_class(LCC_ARG1());
      break;
    default:
      SIMPLE_ERROR(BF("Add support to dispatch off of something other than one of the first two arguments - arg: %d") % singleDispatchArgumentIndex);
    }
    List_sp callHistory = gc::As_unsafe<List_sp>(closure->callHistory.load(std::memory_order_relaxed));
    INITIALIZE_VA_LIST();
    while (callHistory.consp()) {
      Cons_sp entry = gc::As_unsafe<Cons_sp>(CONS_CAR(callHistory));
      callHistory = CONS_CDR(callHistory);
      if (CONS_CAR(entry) == dispatchArgClass) {
        SingleDispatchMethod_sp method = gc::As_unsafe<SingleDispatchMethod_sp>(CONS_CDR(entry));
        Function_sp method_function = method->_function;
        return (method_function->entry())(LCC_PASS_ARGS_VASLIST(method_function.raw_(),lcc_vargs));
      }
    }
    // There wasn't a direct match in the call history - so search the class-precedence list of the
    // argument to see if any of the ancestor classes are handled by this single dispatch generic function
    // This is the slow path for discriminating functions.
    // Update the call-history with what we find.
    List_sp classPrecedenceList = dispatchArgClass->instanceRef(Instance_O::REF_CLASS_CLASS_PRECEDENCE_LIST);
    List_sp methods = gc::As_unsafe<List_sp>(closure->methods.load(std::memory_order_relaxed));
    while (classPrecedenceList.consp()) {
      Instance_sp class_ = gc::As<Instance_sp>(CONS_CAR(classPrecedenceList));
      classPrecedenceList = CONS_CDR(classPrecedenceList);
      List_sp curMethod = methods;
      while (curMethod.consp()) {
        SingleDispatchMethod_sp method = gc::As_unsafe<SingleDispatchMethod_sp>(CONS_CAR(curMethod));
        curMethod = CONS_CDR(curMethod);
        Instance_sp methodClass = method->receiver_class();
        if (methodClass == class_) {
          // Update the call-history using CAS
          T_sp expected;
          Cons_sp entry = Cons_O::create(dispatchArgClass,method);
          Cons_sp callHistoryEntry = Cons_O::create(entry,nil<T_O>());
          do {
            expected = closure->callHistory.load(std::memory_order_relaxed);
            callHistoryEntry->rplacd(expected);
          } while (!closure->callHistory.compare_exchange_weak(expected, callHistoryEntry));
          Function_sp method_function = method->_function;
          return (method_function->entry())(LCC_PASS_ARGS_VASLIST(method_function.raw_(),lcc_vargs));
        }
      }
    }
    SIMPLE_ERROR(BF("This single dispatch generic function %s does not recognize argument class %s") % _rep_(closure->asSmartPtr()) % _rep_(dispatchArgClass));
  }

};

};


namespace core {
SingleDispatchGenericFunction_sp core__ensure_single_dispatch_generic_function(T_sp gfname, LambdaListHandler_sp llhandler, bool autoExport, size_t singleDispatchArgumentIndex);
// void core__satiateSingleDispatchGenericFunctions();

};


#endif /* _SINGLEDISPATCHGENERICFUNCTION_H_ */
