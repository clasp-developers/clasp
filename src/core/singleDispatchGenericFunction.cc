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

#define DEBUG_LEVEL_FULL
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
#include <clasp/core/documentation.h>
#include <clasp/core/multipleValues.h>
#include <clasp/core/lambdaListHandler.h>
#include <clasp/core/singleDispatchGenericFunction.h>
#include <clasp/core/singleDispatchMethod.h>
#include <clasp/core/wrappers.h>
#include <clasp/core/sort.h>

namespace core {

CL_DECLARE();
CL_DOCSTRING("ensureSingleDispatchGenericFunction");
CL_DEFUN SingleDispatchGenericFunctionClosure_sp core__ensure_single_dispatch_generic_function(T_sp gfname, LambdaListHandler_sp llhandler, bool autoExport, size_t singleDispatchArgumentIndex) {
  SingleDispatchGenericFunctionClosure_sp gfn = Lisp_O::find_single_dispatch_generic_function(gfname, false);
  //        printf("%s:%d find_single_dispatch_generic_function(%s) --> %p\n", __FILE__, __LINE__, _rep_(gfname).c_str(), gfn.raw_() );
  if (gfn.nilp()) {
    if (gfname.consp() && CONS_CAR(gfname) == cl::_sym_setf) {
      Symbol_sp setf_gfname = CONS_CAR(CONS_CDR(gfname));
      if (setf_gfname->setf_fboundp()) {
        T_sp symFunc = setf_gfname->getSetfFdefinition();
      // printf("%s:%d   gfname->symbolFunction() --> %p\n", __FILE__, __LINE__, gfname->symbolFunction().raw_());
        if (SingleDispatchGenericFunctionClosure_sp existingGf = symFunc.asOrNull<SingleDispatchGenericFunctionClosure_O>()) {
          (void)existingGf;
          SIMPLE_ERROR(BF("The name %s has a SingleDispatchGenericFunction bound to its function slot but no SingleDispatchGenericFunction with that name was found") % _rep_(gfname));
        } else {
          SIMPLE_ERROR(BF("The name %s already has a function %s bound to it and it is not a SingleDispatchGenericFunction - it cannot become a SingleDispatchGenericFunction") % _rep_(gfname) % _rep_(symFunc) );
        }
      }
      gfn = SingleDispatchGenericFunctionClosure_O::create(gfname, llhandler,singleDispatchArgumentIndex);
      Lisp_O::setf_find_single_dispatch_generic_function(gfname, gfn);
      setf_gfname->setSetfFdefinition(gfn);
      if (autoExport) setf_gfname->exportYourself();
    } else {
      ASSERT(gc::IsA<Symbol_sp>(gfname));
      Symbol_sp gfname_symbol = gc::As_unsafe<Symbol_sp>(gfname);
      if (gfname_symbol->fboundp()) {
        T_sp symFunc = gfname_symbol->symbolFunction();
        if (SingleDispatchGenericFunctionClosure_sp existingGf = symFunc.asOrNull<SingleDispatchGenericFunctionClosure_O>()) {
          (void)existingGf;
          SIMPLE_ERROR(BF("The symbol %s has a SingleDispatchGenericFunction bound to its function slot but no SingleDispatchGenericFunction with that name was found") % _rep_(gfname));
        } else {
          SIMPLE_ERROR(BF("The symbol %s already has a function bound to it and it is not a SingleDispatchGenericFunction - it cannot become a SingleDispatchGenericFunction") % _rep_(gfname));
        }
      }
      gfn = SingleDispatchGenericFunctionClosure_O::create(gfname, llhandler,singleDispatchArgumentIndex);
      Lisp_O::setf_find_single_dispatch_generic_function(gfname, gfn);
      gfname_symbol->setf_symbolFunction(gfn);
      if (autoExport) gfname_symbol->exportYourself();
    }
  }
  return gfn;
};


CL_LAMBDA("gf gfname receiver-class &key lambda-list-handler declares (docstring \"\") body ");
CL_DECLARE();
CL_DOCSTRING("ensureSingleDispatchMethod creates a method and adds it to the single-dispatch-generic-function");
CL_DEFUN void core__ensure_single_dispatch_method(SingleDispatchGenericFunctionClosure_sp gfunction, T_sp tgfname, Instance_sp receiver_class, LambdaListHandler_sp lambda_list_handler, List_sp declares, gc::Nilable<String_sp> docstring, Function_sp body) {
  //	string docstr = docstring->get();
//  SingleDispatchGenericFunctionClosure_sp gf = gc::As<SingleDispatchGenericFunctionClosure_sp>(gfname->symbolFunction());
  SingleDispatchMethod_sp method = SingleDispatchMethod_O::create(tgfname,
                                                                  receiver_class,
                                                                  lambda_list_handler,
                                                                  declares,
                                                                  docstring,
                                                                  body);
  ASSERT(lambda_list_handler.notnilp());
  LambdaListHandler_sp gf_llh = gfunction->_lambdaListHandler;
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
                 _rep_(tgfname) % gf_llh->numberOfRequiredArguments() % _rep_(gfunction->methods()) % _rep_(receiver_class) % lambda_list_handler->numberOfRequiredArguments());
  }
  gfunction->addMethod(method);
  if (docstring.notnilp()) {
    core::ext__annotate(method,cl::_sym_documentation,core::_sym_single_dispatch_method, docstring );
  }
};






LCC_RETURN SingleDispatchCxxEffectiveMethodFunction_O::LISP_CALLING_CONVENTION() {
  SETUP_CLOSURE(SingleDispatchCxxEffectiveMethodFunction_O,closure);
  INCREMENT_FUNCTION_CALL_COUNTER(closure);
  COPY_VA_LIST();
  // INITIALIZE_VA_LIST(); // This was done by the caller
  return (closure->_onlyCxxMethodFunction)->entry.load()(LCC_PASS_ARGS_VASLIST(closure->_onlyCxxMethodFunction.raw_(),lcc_vargs));
};


LCC_RETURN SingleDispatchEffectiveMethodFunction_O::LISP_CALLING_CONVENTION() {
  SETUP_CLOSURE(SingleDispatchEffectiveMethodFunction_O,closure);
  INCREMENT_FUNCTION_CALL_COUNTER(closure);
  COPY_VA_LIST();
  for ( auto cur : closure->_Befores ) {
    Vaslist before_args_s(*lcc_vargs);
    VaList_sp before_args(&before_args_s);
    Function_sp before((gctools::Tagged)oCar(cur).raw_());
    (*before).entry.load()(LCC_PASS_ARGS_VASLIST(before.raw_(),before_args));
  }
  MultipleValues save;
  Function_sp primary0((gctools::Tagged)oCar(closure->_Primaries).raw_());
  Vaslist primary_args_s(*lcc_vargs);
  VaList_sp primary_args(&primary_args_s);
  T_mv val0 = (*primary0).entry.load()(LCC_PASS_ARGS_VASLIST(primary0.raw_(),primary_args));
  multipleValuesSaveToMultipleValues(val0, &save);
  for ( auto cur : closure->_Afters ) {
    Vaslist after_args_s(*lcc_vargs);
    VaList_sp after_args(&after_args_s);
    Function_sp after((gctools::Tagged)oCar(cur).raw_());
    (*after).entry.load()(LCC_PASS_ARGS_VASLIST(after.raw_(),after_args));
  }
  return multipleValuesLoadFromMultipleValues(&save);
}


// ----------------------------------------------------------------------
//

T_sp SingleDispatchGenericFunctionClosure_O::lambdaList() const {
  return this->_lambdaListHandler->lambdaList();
}

void SingleDispatchGenericFunctionClosure_O::addMethod(SingleDispatchMethod_sp method) {
  _OF();
  // Look to see if the method is already defined
//  LOG(BF("defmethod for symbol[%s] called with method with receiverClass[%s]") % _rep_(this->name) % _rep_(method->receiver_class()));
  bool replacedMethod = false;
  {
    _BLOCK_TRACEF(BF("Checking if the receiver class already has a method"));
    for (auto cur : this->_Methods) {
      SingleDispatchMethod_sp existing = gc::As<SingleDispatchMethod_sp>(oCar(cur));
      LOG(BF("An existing method has receiverClass[%s]") % _rep_(existing->receiver_class()));
      if (existing->receiver_class() == method->receiver_class()) {
        cur->setCar(method);
        replacedMethod = true;
        break;
        //		    SIMPLE_ERROR(BF("You tried to overwrite a locked method with name[%s]") % _rep_(name));
      }
    }
  }
  if (!replacedMethod) {
    LOG(BF("This is a new method - adding it to the Methods list"));
    this->_Methods = Cons_O::create(method, this->_Methods);
  }
}

/*! I think this fills the role of the lambda returned by
      std-compute-discriminating-function (gf) AMOP-303 top
    */
LCC_RETURN SingleDispatchGenericFunctionClosure_O::LISP_CALLING_CONVENTION() {
  SETUP_CLOSURE(SingleDispatchGenericFunctionClosure_O,closure);
  INCREMENT_FUNCTION_CALL_COUNTER(closure);
  INITIALIZE_VA_LIST(); //  lcc_vargs now points to argument list
  Function_sp func;
  Cache_sp cache = my_thread->_SingleDispatchMethodCachePtr;
  gctools::Vec0<T_sp> &vektor = cache->keys();
  vektor[0] = closure->functionName();
  Instance_sp dispatchArgClass;
  switch (closure->_SingleDispatchArgumentIndex) {
  case 0:
      dispatchArgClass = lisp_instance_class(LCC_ARG0());
      break;
  case 1:
      dispatchArgClass = lisp_instance_class(LCC_ARG1());
      break;
  default:
      SIMPLE_ERROR(BF("Add support to dispatch off of something other than one of the first two arguments - arg: %d") % closure->_SingleDispatchArgumentIndex);
  }
  vektor[1] = dispatchArgClass;
  CacheRecord *e; //gctools::StackRootedPointer<CacheRecord> e;
  try {
    cache->search_cache(e); // e = ecl_search_cache(cache);
  } catch (CacheError &err) {
    printf("%s:%d - There was an CacheError searching the GF cache for the keys  You should try and get into cache->search_cache to see where the error is\n", __FILE__, __LINE__);
    SIMPLE_ERROR(BF("Try #1 generic function cache search error looking for %s") % _rep_(closure->functionName()));
  }
  //        printf("%s:%d searched on %s/%s  cache record = %p\n", __FILE__, __LINE__, _rep_(vektor[0]).c_str(), _rep_(vektor[1]).c_str(), e );
  if (e->_key.notnilp()) {
    func = gc::As<Function_sp>(e->_value);
  } else {
//    printf("%s:%d Cache miss in SingleDispatchGenericFunctionClosure_O for %s\n", __FILE__, __LINE__, _rep_(closure->_name).c_str());
    func = closure->slowMethodLookup(dispatchArgClass);
    T_sp keys = SimpleVector_O::make(vektor.size(),_Nil<T_O>(),true,vektor.size(),&(vektor[0]));
    e->_key = keys;
    e->_value = func;
  }
  // WARNING: DO NOT alter contents of _lisp->callArgs() or _lisp->multipleValues() above.
  // LISP_PASS ARGS relys on the extra arguments being passed transparently
  return func->entry.load()(LCC_PASS_ARGS_VASLIST(func.raw_(),lcc_vargs));
}

class SingleDispatch_OrderByClassPrecedence {
public:
  bool operator()(T_sp const &x, T_sp const &y) {
    SingleDispatchMethod_sp sx = gc::As<SingleDispatchMethod_sp>(x);
    SingleDispatchMethod_sp sy = gc::As<SingleDispatchMethod_sp>(y);
    return sx->receiver_class()->isSubClassOf(sy->receiver_class());
  }
};

Function_sp SingleDispatchGenericFunctionClosure_O::slowMethodLookup(Instance_sp mc) {
  _OF();
  LOG(BF("Looking for applicable methods for receivers of class[%s]") % _rep_(mc));
  gctools::Vec0<SingleDispatchMethod_sp> applicableMethods;
  for (auto cur : this->_Methods) {
    SingleDispatchMethod_sp sdm = gc::As<SingleDispatchMethod_sp>(oCar(cur));
    Instance_sp ac = sdm->receiver_class();
    if (mc->isSubClassOf(ac)) {
      LOG(BF("Found applicable method with receiver class[%s]") % _rep_(ac));
      applicableMethods.push_back(sdm);
    }
  }
  if (UNLIKELY(applicableMethods.size() == 0)) {
    printf("%s:%d   slowMethodLookup for %s\n", __FILE__, __LINE__, _rep_(this->functionName()).c_str());
    printf("%s:%d    mc-> %s\n", __FILE__, __LINE__, mc->_classNameAsString().c_str());
    for (auto cur : this->_Methods) {
      SingleDispatchMethod_sp sdm = gc::As<SingleDispatchMethod_sp>(oCar(cur));
      Instance_sp ac = sdm->receiver_class();
      printf("%s:%d   ac->className -> %s\n", __FILE__, __LINE__, _rep_(ac->_className()).c_str());
      printf("%s:%d   mc->isSubClassOf(ac) -> %d\n", __FILE__, __LINE__, mc->isSubClassOf(ac));
      printf("%s:%d    class-precedence-list of ac -> %s\n", __FILE__, __LINE__, _rep_(ac->_className()).c_str() );
      List_sp cpl = ac->instanceRef(Instance_O::REF_CLASS_CLASS_PRECEDENCE_LIST);
      for (auto xxx : cpl ) {
        Instance_sp sc = gc::As<Instance_sp>(CONS_CAR(xxx));
        printf("%s:%d    :   %s matches mc -> %d\n", __FILE__, __LINE__, _rep_(sc->_className()).c_str(), (mc==sc));
      }
    }
    SIMPLE_ERROR(BF("There are no applicable methods of %s for receiver class %s") % _rep_(this->functionName()) % mc->instanceClassName() );
  }
  /* Sort the methods from most applicable to least applicable */
  SingleDispatch_OrderByClassPrecedence sort_by_class_precedence;
//  sort::quickSort(applicableMethods.begin(), applicableMethods.end(), sort_by_class_precedence);
  sort::quickSortVec0(applicableMethods,0,applicableMethods.size(),sort_by_class_precedence);
  List_sp applicableMethodsList = _Nil<T_O>();
  for ( int i=applicableMethods.size()-1; i>=0; --i ) {
    applicableMethodsList = Cons_O::create(applicableMethods[i],applicableMethodsList);
  }
  Function_sp emf = this->computeEffectiveMethodFunction(applicableMethodsList);
  return emf;
}

Function_sp SingleDispatchGenericFunctionClosure_O::computeEffectiveMethodFunction(List_sp applicableMethodsList)
{
  if ( core::cl__length(applicableMethodsList) == 1 ) {
    SingleDispatchMethod_sp cur_method = gc::As<SingleDispatchMethod_sp>(oCar(applicableMethodsList));
    SingleDispatchMethodFunction_sp mf = cur_method->_body;
    if ( CxxMethodFunction_sp cmf = mf.as<CxxMethodFunction_O>() ) {
      FunctionDescription* fdesc = makeFunctionDescription(this->functionName());
      Function_sp emf = gctools::GC<SingleDispatchCxxEffectiveMethodFunction_O>::allocate(fdesc,mf);
      return emf;
    }
  }
  // For now I'm going to just return the first method
  SingleDispatchMethod_sp cur_method = gc::As<SingleDispatchMethod_sp>(oCar(applicableMethodsList));
  List_sp befores = _Nil<T_O>();
  List_sp primaries = Cons_O::create(cur_method->_body,_Nil<T_O>());
  List_sp afters = _Nil<T_O>();
  FunctionDescription* fdesc = makeFunctionDescription(this->functionName());
  Function_sp emf = gctools::GC<SingleDispatchEffectiveMethodFunction_O>::allocate(fdesc,befores,primaries,afters);
  return emf;
#if 1
  printf("%s:%d   in computeEffectiveMethodFunction name: %s  contains %zu methods\n", __FILE__, __LINE__, _rep_(this->functionName()).c_str(), core::cl__length(applicableMethodsList) );
  int i = 0;
  for ( auto cur : applicableMethodsList ) {
    SingleDispatchMethod_sp method = gctools::As<SingleDispatchMethod_sp>(oCar(cur));
    printf("      selector[%d]: %s\n", i++, _rep_(method->receiver_class()->_className()).c_str());
  }
#endif
  SIMPLE_ERROR(BF("Generate an effective-single-dispatch-generic-function"));
}



  SYMBOL_EXPORT_SC_(CorePkg, ensureSingleDispatchGenericFunction);
  SYMBOL_EXPORT_SC_(CorePkg, ensureSingleDispatchMethod);




SingleDispatchGenericFunctionClosure_sp SingleDispatchGenericFunctionClosure_O::create(T_sp name, LambdaListHandler_sp llh, size_t singleDispatchArgumentIndex) {
//  GC_ALLOCATE(SingleDispatchGenericFunctionClosure_O, gf);
  FunctionDescription* fdesc = makeFunctionDescription(name,llh->lambdaList());
  SingleDispatchGenericFunctionClosure_sp gfc = gctools::GC<SingleDispatchGenericFunctionClosure_O>::allocate(fdesc,singleDispatchArgumentIndex);
  gfc->finishSetup(llh);
  return gfc;
}


}; /* core */
