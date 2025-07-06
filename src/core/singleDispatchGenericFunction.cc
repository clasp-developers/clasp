// #define DEBUG_SINGLE_DISPATCH 1
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

// #define DEBUG_LEVEL_FULL
#include <clasp/core/foundation.h>
#include <clasp/core/common.h>
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

SingleDispatchGenericFunction_O::SingleDispatchGenericFunction_O(FunctionDescription_sp fdesc, size_t newArgumentIndex)
// we make a XEP based on the dispatch index.
  // in practice this is always 0 or 1
  : Base(fdesc, nil<T_O>(), selectXEP(newArgumentIndex)), callHistory(nil<T_O>()), argumentIndex(newArgumentIndex), methods(nil<T_O>()) {};

SingleDispatchGenericFunction_sp
SingleDispatchGenericFunction_O::create_single_dispatch_generic_function(T_sp gfname, size_t singleDispatchArgumentIndex,
                                                                         List_sp lambdaList) {
  FunctionDescription_sp fdesc = makeFunctionDescription(gfname, lambdaList);
  return gctools::GC<SingleDispatchGenericFunction_O>::allocate(fdesc, singleDispatchArgumentIndex);
}

CL_LISPIFY_NAME(SingleDispatchGenericFunction/callHistory);
CL_DEFUN List_sp core__callHistory(SingleDispatchGenericFunction_sp func) { return func->callHistory.load(std::memory_order_relaxed); }

CL_LISPIFY_NAME(SingleDispatchGenericFunction/specializerIndices);
CL_DEFUN List_sp core__specializerIndices(SingleDispatchGenericFunction_sp func) { return Cons_O::createList(Integer_O::create(func->argumentIndex)); }

CL_LISPIFY_NAME(SingleDispatchGenericFunction/methods);
CL_DEFUN T_sp core__sdgf_methods(SingleDispatchGenericFunction_sp func) { return func->methods.load(std::memory_order_relaxed); }

CL_DECLARE();
CL_DOCSTRING(R"dx(ensureSingleDispatchGenericFunction)dx");
DOCGROUP(clasp);
CL_DEFUN SingleDispatchGenericFunction_sp core__ensure_single_dispatch_generic_function(T_sp gfname, bool autoExport,
                                                                                        size_t singleDispatchArgumentIndex,
                                                                                        List_sp lambdaList) {
  T_sp tgfn;
  if (!cl__fboundp(gfname)) {
    tgfn = nil<T_O>();
  } else {
    tgfn = cl__fdefinition(gfname);
  }
  SingleDispatchGenericFunction_sp gfn;
  if (tgfn.nilp()) {
    // Use CAS to push the new gfname into the list of single dispatch generic functions
    T_sp expected;
    Cons_sp cell = core::Cons_O::create(gfname, nil<T_O>());
    do {
      expected = _lisp->_Roots._SingleDispatchGenericFunctions.load();
      cell->rplacd(expected);
    } while (!_lisp->_Roots._SingleDispatchGenericFunctions.compare_exchange_weak(expected, cell));
    if (gfname.consp() && CONS_CAR(gfname) == cl::_sym_setf) {
      Symbol_sp setf_gfname = CONS_CAR(CONS_CDR(gfname));
      if (setf_gfname->fboundp_setf()) {
        SIMPLE_ERROR("The name {} has something bound to its setf function slot but no generic function with that name was found",
                     _rep_(gfname));
      }
      gfn =
          SingleDispatchGenericFunction_O::create_single_dispatch_generic_function(gfname, singleDispatchArgumentIndex, lambdaList);
      setf_gfname->setSetfFdefinition(gfn);
      if (autoExport)
        setf_gfname->exportYourself();
    } else {
      ASSERT(gc::IsA<Symbol_sp>(gfname));
      Symbol_sp gfname_symbol = gc::As_unsafe<Symbol_sp>(gfname);
      if (gfname_symbol->fboundp()) {
        SIMPLE_ERROR("The symbol {} has something bound to its function slot but not a single dispatch generic function",
                     _rep_(gfname));
      }
      gfn =
          SingleDispatchGenericFunction_O::create_single_dispatch_generic_function(gfname, singleDispatchArgumentIndex, lambdaList);
      gfname_symbol->setf_symbolFunction(gfn);
      if (autoExport)
        gfname_symbol->exportYourself();
    }
  } else {
    gfn = gc::As<SingleDispatchGenericFunction_sp>(tgfn);
  }
  return gfn;
};

CL_LAMBDA("gf gfname receiver-class &key declares (docstring \"\") body ");
CL_DECLARE();
CL_DOCSTRING(R"dx(ensureSingleDispatchMethod creates a method and adds it to the single-dispatch-generic-function)dx");
DOCGROUP(clasp);
CL_DEFUN void core__ensure_single_dispatch_method(SingleDispatchGenericFunction_sp gfunction, T_sp tgfname,
                                                  Instance_sp receiver_class, List_sp declares, T_sp docstring, Function_sp body) {
  SingleDispatchMethod_sp method = SingleDispatchMethod_O::create(tgfname, receiver_class, declares, docstring, body);
  // Update the methods using CAS
  {
    T_sp expected;
    Cons_sp entry = Cons_O::create(method, nil<T_O>());
    do {
      expected = gfunction->methods.load(std::memory_order_relaxed);
      entry->rplacd(expected);
    } while (!gfunction->methods.compare_exchange_weak(expected, entry));
  }
  if (docstring.notnilp()) {
    core::ext__annotate(method, cl::_sym_documentation, core::_sym_single_dispatch_method, docstring);
  }
};

/*! I should probably get the key for each element first and then sort */
class OrderByClassPrecedence {
private:
public:
  OrderByClassPrecedence() {}
  bool operator()(Instance_sp x, Instance_sp y) {
    List_sp yClassPrecedence = y->instanceRef(Instance_O::REF_CLASS_CLASS_PRECEDENCE_LIST);
    if (yClassPrecedence.notnilp()) {
      Cons_sp yCons = gc::As<Cons_sp>(yClassPrecedence);
      return (yCons->memberEq(x).notnilp());
    }
    return false;
  }
};

ComplexVector_T_sp sortDispatchVectorByClassPrecedence(ComplexVector_T_sp dispatchVector) {
  if (dispatchVector->length() <= 2)
    return dispatchVector;
  gctools::Vec0<Instance_sp> classes;
  classes.resize(dispatchVector->length() / 2);
  for (size_t ii = 0; ii < dispatchVector->length(); ii += 2) {
    classes[ii / 2] = gc::As<Instance_sp>(dispatchVector[ii]);
  }
  OrderByClassPrecedence orderer;
  sort::quickSortVec0(classes, 0, classes.size(), orderer);
  ComplexVector_T_sp sorted = ComplexVector_T_O::make(dispatchVector->length(), nil<T_O>(), make_fixnum(0));
  for (size_t ii = 0; ii < classes.size(); ii++) {
    for (size_t jj = 0; jj < dispatchVector->length(); jj += 2) {
      if (dispatchVector[jj] == classes[ii]) {
        sorted->vectorPushExtend(classes[ii]);
        sorted->vectorPushExtend(dispatchVector[jj + 1]);
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
void recursivelySatiate(FuncallableInstance_sp gfun, Instance_sp dispatchClass, SingleDispatchMethod_sp method,
                        ComplexVector_T_sp newDispatchVector) {
  for (size_t ii = 0; ii < newDispatchVector->length(); ii += 2) {
    if (dispatchClass == gc::As<Instance_sp>(newDispatchVector[ii])) {
      newDispatchVector[ii + 1] = method;
      goto FOUND;
    }
  }
#ifdef DEBUG_SINGLE_DISPATCH
  printf("%s:%d      In recursivelySatiate gfun: %s for receiver class %s\n", __FILE__, __LINE__, _rep_(gfun).c_str(),
         _rep_(dispatchClass).c_str());
#endif
  newDispatchVector->vectorPushExtend(dispatchClass);
  newDispatchVector->vectorPushExtend(method);
FOUND:
  List_sp directSubClasses = gc::As<List_sp>(dispatchClass->instanceRef(Instance_O::REF_CLASS_DIRECT_SUBCLASSES));
  while (directSubClasses.notnilp()) {
    Instance_sp directSubClass = gc::As<Instance_sp>(oCar(directSubClasses));
    recursivelySatiate(gfun, directSubClass, method, newDispatchVector);
    directSubClasses = oCdr(directSubClasses);
  }
}

// ----------------------------------------------------------------------
//

SYMBOL_EXPORT_SC_(CorePkg, ensureSingleDispatchGenericFunction);
SYMBOL_EXPORT_SC_(CorePkg, ensureSingleDispatchMethod);

}; // namespace core
