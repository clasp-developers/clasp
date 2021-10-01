//#define DEBUG_SINGLE_DISPATCH 1
/*
    File: singleDispatchGenericFunction.cc
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
#include <clasp/core/common.h>
#include <clasp/core/environment.h>
#include <clasp/core/lispCallingConvention.h>
#include <clasp/core/symbolTable.h>
#include <clasp/core/hashTable.h>
#include <clasp/core/hashTableEql.h>
#include <clasp/core/primitives.h>
#include <clasp/core/package.h>
#include <clasp/core/array.h>
#include <clasp/core/funcallableInstance.h>
#include <clasp/core/documentation.h>
#include <clasp/core/multipleValues.h>
#include <clasp/core/lambdaListHandler.h>
#include <clasp/core/singleDispatchGenericFunction.h>
#include <clasp/core/singleDispatchMethod.h>
#include <clasp/core/wrappers.h>
#include <clasp/core/sort.h>

namespace core {


FuncallableInstance_sp SingleDispatchGenericFunctionClosure_O::create_single_dispatch_generic_function(T_sp gfname, LambdaListHandler_sp llhandler, size_t singleDispatchArgumentIndex)
{
  size_t number_of_required_arguments = llhandler->numberOfRequiredArguments();
  Rack_sp rack = Rack_O::make(REF_SINGLE_DISPATCH_SPECIALIZER_SLOTS,nil<T_O>(),nil<T_O>());
  rack->low_level_rackSet(REF_SINGLE_DISPATCH_SPECIALIZER_CALL_HISTORY,nil<T_O>());
  rack->low_level_rackSet(REF_SINGLE_DISPATCH_SPECIALIZER_LAMBDA_LIST_HANDLER,llhandler);
  rack->low_level_rackSet(REF_SINGLE_DISPATCH_SPECIALIZER_DISPATCH_ARGUMENT_INDEX,
                          make_fixnum(singleDispatchArgumentIndex));
  rack->low_level_rackSet(REF_SINGLE_DISPATCH_SPECIALIZER_METHODS,nil<T_O>());
  GlobalEntryPoint_sp entryPoint = makeGlobalEntryPointAndFunctionDescription(gfname,single_dispatch_funcallable_entry_point,llhandler->lambdaList());
  Instance_sp class_ = gc::As<Instance_sp>(cl__find_class(_sym_SingleDispatchGenericFunctionClosure_O));
  rack->stamp_set(class_->CLASS_stamp_for_instances());
  auto gfun = gctools::GC<FuncallableInstance_O>::allocate(entryPoint,class_,rack);
//  gfun->entry = single_dispatch_funcallable_entry_point;
  return gfun;
}

LCC_RETURN SingleDispatchGenericFunctionClosure_O::single_dispatch_funcallable_entry_point(LCC_ARGS_ELLIPSIS) {
  SETUP_CLOSURE(FuncallableInstance_O,closure);
  INCREMENT_FUNCTION_CALL_COUNTER(closure);
  size_t singleDispatchArgumentIndex = closure->_Rack->low_level_rackRef(REF_SINGLE_DISPATCH_SPECIALIZER_DISPATCH_ARGUMENT_INDEX).unsafe_fixnum();
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
  List_sp callHistory = gc::As_unsafe<List_sp>(closure->_Rack->low_level_rackRef(REF_SINGLE_DISPATCH_SPECIALIZER_CALL_HISTORY));
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
  List_sp methods = gc::As_unsafe<List_sp>(closure->_Rack->low_level_rackRef(REF_SINGLE_DISPATCH_SPECIALIZER_METHODS));
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
          expected = closure->_Rack->low_level_rackRef(REF_SINGLE_DISPATCH_SPECIALIZER_CALL_HISTORY);
          callHistoryEntry->rplacd(expected);
        } while (!closure->_Rack->low_level_rack_compare_exchange_weak(REF_SINGLE_DISPATCH_SPECIALIZER_CALL_HISTORY,expected,callHistoryEntry));
        Function_sp method_function = method->_function;
        return (method_function->entry())(LCC_PASS_ARGS_VASLIST(method_function.raw_(),lcc_vargs));
      }
    }
  }
  SIMPLE_ERROR(BF("This single dispatch generic function %s does not recognize argument class %s") % _rep_(closure->asSmartPtr()) % _rep_(dispatchArgClass));
}


CL_DECLARE();
CL_DOCSTRING(R"dx(ensureSingleDispatchGenericFunction)dx")
DOCGROUP(clasp)
CL_DEFUN FuncallableInstance_sp core__ensure_single_dispatch_generic_function(T_sp gfname, LambdaListHandler_sp llhandler, bool autoExport, size_t singleDispatchArgumentIndex) {
  T_sp tgfn;
  if (!cl__fboundp(gfname)) {
    tgfn = nil<T_O>();
  } else {
    tgfn = cl__fdefinition(gfname);
  }
  FuncallableInstance_sp gfn;
  if (tgfn.nilp()) {
    // Use CAS to push the new gfname into the list of single dispatch generic functions
    T_sp expected;
    Cons_sp cell = core::Cons_O::create(gfname,nil<T_O>());
    do {
      expected = _lisp->_Roots._SingleDispatchGenericFunctions.load();
      cell->rplacd(expected);
    } while (!_lisp->_Roots._SingleDispatchGenericFunctions.compare_exchange_weak(expected,cell));
    if (gfname.consp() && CONS_CAR(gfname) == cl::_sym_setf) {
      Symbol_sp setf_gfname = CONS_CAR(CONS_CDR(gfname));
      if (setf_gfname->fboundp_setf()) {
        SIMPLE_ERROR(BF("The name %s has something bound to its setf function slot but no generic function with that name was found") % _rep_(gfname));
      }
      gfn = SingleDispatchGenericFunctionClosure_O::create_single_dispatch_generic_function(gfname, llhandler,singleDispatchArgumentIndex);
      setf_gfname->setSetfFdefinition(gfn);
      if (autoExport) setf_gfname->exportYourself();
    } else {
      ASSERT(gc::IsA<Symbol_sp>(gfname));
      Symbol_sp gfname_symbol = gc::As_unsafe<Symbol_sp>(gfname);
      if (gfname_symbol->fboundp()) {
        T_sp symFunc = gfname_symbol->symbolFunction();
        SIMPLE_ERROR(BF("The symbol %s has something bound to its function slot but no FuncallableInstance with that name was found") % _rep_(gfname));
      }
      gfn = SingleDispatchGenericFunctionClosure_O::create_single_dispatch_generic_function(gfname, llhandler,singleDispatchArgumentIndex);
      gfname_symbol->setf_symbolFunction(gfn);
      if (autoExport) gfname_symbol->exportYourself();
    }
  } else {
    gfn = gc::As<FuncallableInstance_sp>(tgfn);
  }
  return gfn;
};


CL_LAMBDA("gf gfname receiver-class &key lambda-list-handler declares (docstring \"\") body ")
CL_DECLARE();
CL_DOCSTRING(R"dx(ensureSingleDispatchMethod creates a method and adds it to the single-dispatch-generic-function)dx")
DOCGROUP(clasp)
CL_DEFUN void core__ensure_single_dispatch_method(FuncallableInstance_sp gfunction, T_sp tgfname, Instance_sp receiver_class, LambdaListHandler_sp lambda_list_handler, List_sp declares, T_sp docstring, Function_sp body) {
  //	string docstr = docstring->get();
//  SingleDispatchGenericFunctionClosure_sp gf = gc::As<SingleDispatchGenericFunctionClosure_sp>(gfname->symbolFunction());
  SingleDispatchMethod_sp method = SingleDispatchMethod_O::create(tgfname,
                                                                  receiver_class,
                                                                  lambda_list_handler,
                                                                  declares,
                                                                  docstring,
                                                                  body);
  ASSERT(lambda_list_handler.notnilp());
  LambdaListHandler_sp gf_llh = gc::As<LambdaListHandler_sp>(gfunction->_Rack->low_level_rackRef(SingleDispatchGenericFunctionClosure_O::REF_SINGLE_DISPATCH_SPECIALIZER_LAMBDA_LIST_HANDLER));
  if (lambda_list_handler->numberOfRequiredArguments() != gf_llh->numberOfRequiredArguments()) {
    SIMPLE_ERROR(BF("There is a mismatch between the number of required arguments\n"
                    " between the single-dispatch-generic-function %s which expects %d arguments\n"
                    " for methods: %s\n"
                    " and another method with the same name in %s which expects %d arguments\n"
                    " - this is probably due to the way you can overload method names with\n"
                    " different argument signatures in C++ which does not translate well\n"
                    " to Common Lisp.\n"
                    " --> The solution is to give the most recent Common Lisp method you defined\n"
                    " a new name by prefixing it with the class name\n"
                    " eg: getFilename -> PresumedLoc-getFilename") %
                 _rep_(tgfname) % gf_llh->numberOfRequiredArguments() % _rep_(gfunction->_Rack->low_level_rackRef(SingleDispatchGenericFunctionClosure_O::REF_SINGLE_DISPATCH_SPECIALIZER_CALL_HISTORY)) % _rep_(receiver_class) % lambda_list_handler->numberOfRequiredArguments());
  }
  // Update the methods using CAS
  {
    T_sp expected;
    Cons_sp entry = Cons_O::create(method,nil<T_O>());
    do {
      expected = gfunction->_Rack->low_level_rackRef(SingleDispatchGenericFunctionClosure_O::REF_SINGLE_DISPATCH_SPECIALIZER_METHODS);
      entry->rplacd(expected);
    } while (!gfunction->_Rack->low_level_rack_compare_exchange_weak(SingleDispatchGenericFunctionClosure_O::REF_SINGLE_DISPATCH_SPECIALIZER_METHODS, expected, entry));
  }
  if (docstring.notnilp()) {
    core::ext__annotate(method,cl::_sym_documentation,core::_sym_single_dispatch_method, docstring );
  }
};


/*! I should probably get the key for each element first and then sort */
class OrderByClassPrecedence {
private:
public:
  OrderByClassPrecedence() {
  }
  bool operator()(Instance_sp x, Instance_sp y) {
    List_sp yClassPrecedence = y->instanceRef(Instance_O::REF_CLASS_CLASS_PRECEDENCE_LIST);
    if (yClassPrecedence.notnilp()) {
      Cons_sp yCons = gc::As<Cons_sp>(yClassPrecedence);
      return (yCons->memberEq(x).notnilp());
    }
    return false;
  }
};


ComplexVector_T_sp sortDispatchVectorByClassPrecedence(ComplexVector_T_sp dispatchVector)
{
  if (dispatchVector->length()<=2) return dispatchVector;
  gctools::Vec0<Instance_sp> classes;
  classes.resize(dispatchVector->length()/2);
  for ( size_t ii = 0; ii< dispatchVector->length(); ii += 2) {
    classes[ii/2] = gc::As<Instance_sp>((*dispatchVector)[ii]);
  }
  OrderByClassPrecedence orderer;
  sort::quickSortVec0(classes,0,classes.size(),orderer);
  ComplexVector_T_sp sorted = ComplexVector_T_O::make(dispatchVector->length(),nil<T_O>(),make_fixnum(0));
  for ( size_t ii = 0; ii<classes.size(); ii++ ) {
    for (size_t jj = 0; jj<dispatchVector->length(); jj+=2) {
      if ((*dispatchVector)[jj] == classes[ii]) {
        sorted->vectorPushExtend(classes[ii]);
        sorted->vectorPushExtend((*dispatchVector)[jj+1]);
        goto FOUND;
      }
    }
    printf("%s:%d Could not find class %s\n", __FILE__, __LINE__, _rep_(classes[ii]).c_str());
    abort();
  FOUND:
    (void)0;
  }
  return sorted;
}


/*! Recursively descend through dispatchClass subclasses and add them to the dispatch vector */
void recursivelySatiate(FuncallableInstance_sp gfun, Instance_sp dispatchClass, SingleDispatchMethod_sp method, ComplexVector_T_sp newDispatchVector)
{
  for ( size_t ii = 0; ii<newDispatchVector->length(); ii+=2) {
    if (dispatchClass == gc::As<Instance_sp>((*newDispatchVector)[ii])) {
      (*newDispatchVector)[ii+1] = method;
      goto FOUND;
    }
  }
#ifdef DEBUG_SINGLE_DISPATCH
  printf("%s:%d      In recursivelySatiate gfun: %s for receiver class %s\n", __FILE__, __LINE__, _rep_(gfun).c_str(), _rep_(dispatchClass).c_str());
#endif
  newDispatchVector->vectorPushExtend(dispatchClass);
  newDispatchVector->vectorPushExtend(method);
 FOUND:
  List_sp directSubClasses = gc::As<List_sp>(dispatchClass->instanceRef(Instance_O::REF_CLASS_DIRECT_SUBCLASSES));
  while (directSubClasses.notnilp()) {
    Instance_sp directSubClass = gc::As<Instance_sp>(oCar(directSubClasses));
    recursivelySatiate(gfun,directSubClass,method,newDispatchVector);
    directSubClasses = oCdr(directSubClasses);
  }
}

// ----------------------------------------------------------------------
//

  SYMBOL_EXPORT_SC_(CorePkg, ensureSingleDispatchGenericFunction);
  SYMBOL_EXPORT_SC_(CorePkg, ensureSingleDispatchMethod);



}; /* core */
