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
#include <clasp/core/symbolTable.h>
#include <clasp/core/hashTable.h>
#include <clasp/core/hashTableEql.h>
#include <clasp/core/primitives.h>
#include <clasp/core/vectorObjectsWithFillPtr.h>
#include <clasp/core/package.h>
#include <clasp/core/str.h>
#include <clasp/core/executables.h>
#include <clasp/core/multipleValues.h>
#include <clasp/core/lambdaListHandler.h>
#include <clasp/core/singleDispatchEffectiveMethodFunction.h>
#include <clasp/core/singleDispatchGenericFunction.h>
#include <clasp/core/singleDispatchMethod.h>
#include <clasp/core/wrappers.h>
#include <clasp/core/sort.h>

namespace core {

#define ARGS_af_ensureSingleDispatchGenericFunction "(gfname llhandler)"
#define DECL_af_ensureSingleDispatchGenericFunction ""
#define DOCS_af_ensureSingleDispatchGenericFunction "ensureSingleDispatchGenericFunction"
T_sp af_ensureSingleDispatchGenericFunction(Symbol_sp gfname, LambdaListHandler_sp llhandler) {
  _G();
  T_sp gfn = Lisp_O::find_single_dispatch_generic_function(gfname, false);
  //        printf("%s:%d find_single_dispatch_generic_function(%s) --> %p\n", __FILE__, __LINE__, _rep_(gfname).c_str(), gfn.raw_() );
  if (gfn.nilp()) {
    if (gfname->fboundp()) {
      T_sp symFunc = gfname->symbolFunction();
      // printf("%s:%d   gfname->symbolFunction() --> %p\n", __FILE__, __LINE__, gfname->symbolFunction().raw_());
      if (SingleDispatchGenericFunction_sp existingGf = symFunc.asOrNull<SingleDispatchGenericFunction_O>()) {
        (void)existingGf;
        SIMPLE_ERROR(BF("The symbol %s has a SingleDispatchGenericFunction bound to its function slot but no SingleDispatchGenericFunction with that name was found") % _rep_(gfname));
      } else {
        SIMPLE_ERROR(BF("The symbol %s already has a function bound to it and it is not a SingleDispatchGenericFunction - it cannot become a SingleDispatchGenericFunction") % _rep_(gfname));
      }
    }
    gfn = SingleDispatchGenericFunction_O::create(gfname, llhandler);
    Lisp_O::setf_find_single_dispatch_generic_function(gfname, gfn);
    gfname->setf_symbolFunction(gfn);
  }
  return gfn;
};

#define ARGS_af_ensureSingleDispatchMethod "(gfname receiver-class &key lambda-list-handler declares (docstring \"\") body )"
#define DECL_af_ensureSingleDispatchMethod ""
#define DOCS_af_ensureSingleDispatchMethod "ensureSingleDispatchMethod creates a method and adds it to the single-dispatch-generic-function"
void af_ensureSingleDispatchMethod(Symbol_sp gfname, Class_sp receiver_class, LambdaListHandler_sp lambda_list_handler, List_sp declares, gc::Nilable<Str_sp> docstring, Function_sp body) {
  //	string docstr = docstring->get();
  if (!gfname->fboundp()) {
    SIMPLE_ERROR(BF("single-dispatch-generic-function %s is not defined") % _rep_(gfname));
  }
  SingleDispatchGenericFunction_sp gf = gc::As<SingleDispatchGenericFunction_sp>(gfname->symbolFunction());
  SingleDispatchMethod_sp method = SingleDispatchMethod_O::create(gfname, receiver_class, lambda_list_handler, declares, docstring, body);
  ASSERT(lambda_list_handler.notnilp());
  gctools::tagged_pointer<SingleDispatchGenericFunctionClosure> gfc = gf->closure.as<SingleDispatchGenericFunctionClosure>();
  LambdaListHandler_sp gf_llh = gfc->_lambdaListHandler;
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
                 _rep_(gfname) % gf_llh->numberOfRequiredArguments() % _rep_(gf->methods()) % _rep_(receiver_class) % lambda_list_handler->numberOfRequiredArguments());
  }
  gfc->addMethod(method);
};

// ----------------------------------------------------------------------
//

T_sp SingleDispatchGenericFunctionClosure::lambdaList() const {
  return this->_lambdaListHandler->lambdaList();
}

void SingleDispatchGenericFunctionClosure::addMethod(SingleDispatchMethod_sp method) {
  _OF();
  // Look to see if the method is already defined
  LOG(BF("defmethod for symbol[%s] called with method with receiverClass[%s]") % _rep_(this->name) % _rep_(method->receiver_class()));
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
LCC_RETURN SingleDispatchGenericFunctionClosure::LISP_CALLING_CONVENTION() {
  Function_sp func;
  gc::tagged_pointer<Cache> cache(_lisp->singleDispatchMethodCachePtr());
  gctools::Vec0<T_sp> &vektor = cache->keys();
  vektor[0] = this->name;
  Class_sp dispatchArgClass = vektor[1] = lisp_instance_class(LCC_ARG0());
  CacheRecord *e; //gctools::StackRootedPointer<CacheRecord> e;
  try {
    cache->search_cache(e); // e = ecl_search_cache(cache);
  } catch (CacheError &err) {
    printf("%s:%d - There was an CacheError searching the GF cache for the keys  You should try and get into cache->search_cache to see where the error is\n", __FILE__, __LINE__);
    SIMPLE_ERROR(BF("Try #1 generic function cache search error looking for %s") % _rep_(this->name));
  }
  //        printf("%s:%d searched on %s/%s  cache record = %p\n", __FILE__, __LINE__, _rep_(vektor[0]).c_str(), _rep_(vektor[1]).c_str(), e );
  if (e->_key.notnilp()) {
    func = gc::As<Function_sp>(e->_value);
  } else {
    func = this->slowMethodLookup(dispatchArgClass);
    T_sp keys = VectorObjects_O::create(vektor);
    e->_key = keys;
    e->_value = func;
  }
  // WARNING: DO NOT alter contents of _lisp->callArgs() or _lisp->multipleValues() above.
  // LISP_PASS ARGS relys on the extra arguments being passed transparently
  return func->closure->invoke_va_list(LCC_PASS_ARGS);
}

class SingleDispatch_OrderByClassPrecedence {
public:
  bool operator()(T_sp const &x, T_sp const &y) {
    SingleDispatchMethod_sp sx = gc::As<SingleDispatchMethod_sp>(x);
    SingleDispatchMethod_sp sy = gc::As<SingleDispatchMethod_sp>(y);
    return sx->receiver_class()->isSubClassOf(sy->receiver_class()); // or should it be y->isSubClassOf(x)?????
  }
};

Function_sp SingleDispatchGenericFunctionClosure::slowMethodLookup(Class_sp mc) {
  _OF();
  LOG(BF("Looking for applicable methods for receivers of class[%s]") % _rep_(mc));
  gctools::Vec0<SingleDispatchMethod_sp> applicableMethods;
  for (auto cur : this->_Methods) {
    SingleDispatchMethod_sp sdm = gc::As<SingleDispatchMethod_sp>(oCar(cur));
    Class_sp ac = sdm->receiver_class();
    if (mc->isSubClassOf(ac)) {
      LOG(BF("Found applicable method with receiver class[%s]") % _rep_(ac));
      applicableMethods.push_back(sdm);
    }
  }
  if (UNLIKELY(applicableMethods.size() == 0)) {
    SIMPLE_ERROR(BF("There are no applicable methods of %s for receiver class %s") % _rep_(this->name) % mc->instanceClassName());
  }
#if 1
  /* Sort the methods from most applicable to least applicable */
  SingleDispatch_OrderByClassPrecedence sort_by_class_precedence;
  sort::quickSort(applicableMethods.begin(), applicableMethods.end(), sort_by_class_precedence);
#else // look for the most applicable method
  applicable_method = applicableMethods->elt(0).as<SingleDispatchMethod_O>();
  for (int i(1), iEnd(applicableMethods->length()); i < iEnd; ++i) {
    SingleDispatchMethod_sp sdm = applicableMethods->elt(i).as<SingleDispatchMethod_O>();
    if (sdm->receiver_class()->isSubClassOf(applicable_method->receiver_class())) {
      applicable_method = sdm;
    }
  }
  printf("%s:%d:%s The most applicable method is for class %s\n", __FILE__, __LINE__, __FUNCTION__, _rep_(applicable_method).c_str());
#endif

#if 0
	Function_sp emf = this->computeEffectiveMethodFunction(applicableMethods);
        return emf;
#else
  SingleDispatchMethod_sp cur_method = applicableMethods[0];
  ASSERTF(cur_method.notnilp(), BF("There is no method to compute_effective_method_function for"));
  // Construct a name for the emf by stringing together the generic function name
  // with the name of the receiver class - this is to help with debugging
  return cur_method->code;
#endif
}

#if 0
    Function_sp SingleDispatchGenericFunctionClosure::computeEffectiveMethodFunction(gctools::Vec0<SingleDispatchMethod_sp> const& applicableMethods)
    {_OF();
        SingleDispatchMethod_sp cur_method = applicableMethods[0];
        ASSERTF(cur_method.notnilp(),BF("There is no method to compute_effective_method_function for"));
        // Construct a name for the emf by stringing together the generic function name
        // with the name of the receiver class - this is to help with debugging
        Function_sp code = cur_method->code;
        return code;
    }
#endif

EXPOSE_CLASS(core, SingleDispatchGenericFunction_O);

void SingleDispatchGenericFunction_O::exposeCando(::core::Lisp_sp lisp) {
  ::core::class_<SingleDispatchGenericFunction_O>()
      //	    .def_readonly("single_dispatch_generic_function_methods",&SingleDispatchGenericFunction_O::_Methods,"Access the methods of the single-dispatch-generic-function")
      //	.initArgs("(self)")
      //	    .def("single-dispatch-generic-function-dispatch-on-index",&SingleDispatchGenericFunction_O::dispatch_on_index)
      .def("SingleDispatchGenericFunction-methods", &SingleDispatchGenericFunction_O::methods);
  SYMBOL_EXPORT_SC_(CorePkg, ensureSingleDispatchGenericFunction);
  Defun(ensureSingleDispatchGenericFunction);
  SYMBOL_EXPORT_SC_(CorePkg, ensureSingleDispatchMethod);
  Defun(ensureSingleDispatchMethod);
}

void SingleDispatchGenericFunction_O::exposePython(::core::Lisp_sp lisp) {
#ifdef USEBOOSTPYTHON
  PYTHON_CLASS(Pkg(), SingleDispatchGenericFunction, "", "", _LISP)
      //	.initArgs("(self)")
      ;
#endif
}

SingleDispatchGenericFunction_sp SingleDispatchGenericFunction_O::create(T_sp name, LambdaListHandler_sp llh) {
  _G();
  GC_ALLOCATE(SingleDispatchGenericFunction_O, gf);
  gctools::tagged_pointer<SingleDispatchGenericFunctionClosure> gfc = gctools::ClassAllocator<SingleDispatchGenericFunctionClosure>::allocateClass(name);
  gfc->finishSetup(llh, kw::_sym_function);
  gf->closure = gfc;
  return gf;
}

#if 0
#if defined(OLD_SERIALIZE)
    void SingleDispatchGenericFunction_O::serialize(::serialize::SNodeP node)
    {
	IMPLEMENT_ME();
        this->Bases::serialize(node);
	// Archive other instance variables here
    }
#endif
#endif

#if 0
    /*! I think this fills the role of the lambda returned by
      std-compute-discriminating-function (gf) AMOP-303 top
    */
    void SingleDispatchGenericFunction_O::LISP_INVOKE()
    {_OF();
#ifdef DEBUG_ON
	if ( nargs <= 0 ) {
	    SIMPLE_ERROR(BF("Insufficient arguments %d for single-dispatch-generic-function - dispatching on %d") % nargs % 0 );
	}
#endif
	T_sp dispatchArg = LCC_ARG0();
	ASSERTF(!dispatchArg.unboundp(),BF("The dispatch object is UNBOUND"));
	LispCompiledFunctionIHF _frame(_lisp->invocationHistoryStack(),this->asSmartPtr());
	Class_sp dispatchArgClass = lisp_instance_class(dispatchArg);
	ASSERTF(!dispatchArgClass.nilp(),BF("The dispatch class is NIL!!!! for dispatch obj: %s") % _rep_(dispatchArg));
	LOG(BF("Invoked SingleDispatchGenericFunction[%s] with receiver class[%s]")
	    % _rep_(this->getFunctionName()) % _rep_(dispatchArgClass) );
	Function_sp emf =
	    this->_classes_to_emf_table->gethash(dispatchArgClass,_Nil<SingleDispatchEffectiveMethodFunction_O>()).as<Function_O>();
	if ( emf.nilp() )
	{
	    LOG(BF("There was no effective method function defined - building one"));
	    emf = this->slow_method_lookup(dispatchArgClass);
	    this->_classes_to_emf_table->hash_table_setf_gethash(dispatchArgClass,emf);
//	    printf( "%s:%d - performed slow_method_lookup for sdgf: %s dispatchArgClass: %s\n", __FILE__, __LINE__, this->getFunctionName()->__repr__().c_str(), dispatchArgClass->__repr__().c_str() );
	} else
	{
//	    printf( "%s:%d - used cached sdgf: %s dispatchArgClass: %s\n", __FILE__, __LINE__, this->getFunctionName()->__repr__().c_str(), dispatchArgClass->__repr__().c_str() );
	    LOG(BF("Found hashed effective method - using that"));
	}
	LOG(BF("Invoking the effective method function"));
        IMPLEMENT_MEF(BF("Handle invoke"));
#if 0
	return emf->INVOKE(nargs,args);
#endif
    };
#endif

#if 0 // old algorithm



    class Lambda_emf : public Functoid
    {
    private:
        /*! Store the name of the function that this Lambda_emf invokes - for debugging */
        Symbol_sp		_name;
        /*! Store the method_function that this emf invokes.
          This function takes two arguments: (args next-emfun) */
        Function_sp		_method_function;
        /*! Store the next-emfun that will be passed to the _method_function */
        T_sp		_next_emfun;
    public:
        string describe() const { return "Lambda_emf";};
        bool requires_activation_frame() const { return true;};
    public:
        Lambda_emf(const string& name,
                   SingleDispatchGenericFunction_sp gf,
                   Symbol_sp emf_name,
                   SingleDispatchMethod_sp cur_method,
                   Cons_sp next_methods) : Functoid("Lambda_emf->"+name)
        {_G();
            this->_name = emf_name;
            this->_method_function = cur_method->_chainable_method_function;
            ASSERTF(this->_method_function->getLambdaListHandler().notnilp()
                    , BF("The method-function should never have a nil LambdaListHandler"));
            // Calculate the function to call with call_next_method
            // Do this by recursively calling gf->compute_effective_method_function with next_methods
            if ( next_methods.nilp() )
            {
                this->_next_emfun = _Nil<T_O>();
            } else
            {
                this->_next_emfun = gf->compute_effective_method_function(next_methods);
            }
        }

        DISABLE_NEW();

        virtual T_mv activate(ActivationFrame_sp frame)
        {
            // TODO:: Here I'm converting the ActivationFrame back into a Cons and passing it
            // as the first argument to the _method_function and the second argument is _next_emfun
            // I should pass (frame) within (method_function_args) [an ActivationFrame within an ActivationFrame ]
            // Then I don't need to convert it back into an activation frame on the receiving end
            ValueFrame_sp method_function_args(ValueFrame_O::create_fill(frame,this->_next_emfun,_Nil<ActivationFrame_O>()));
            return this->_method_function->INVOKE(method_function_args);
	    
        }
    };


        Function_sp SingleDispatchGenericFunction_O::compute_effective_method_function(List_sp applicable_methods)
        {_OF();
            if ( applicable_methods.nilp() )
            {
                SIMPLE_ERROR(BF("You cannot compute_effective_method_function for gf[%s] because there are no methods!") % _rep_(this->getFunctionName()) );
            }
            SingleDispatchMethod_sp cur_method = oCar(applicable_methods).as<SingleDispatchMethod_O>();
            ASSERTF(cur_method.notnilp(),BF("There is no method to compute_effective_method_function for"));
            // Construct a name for the emf by stringing together the generic function name
            // with the name of the receiver class - this is to help with debugging
            stringstream emf_name_ss;
            string gfname = _rep_(this->getFunctionName());
            Class_sp receiverClass = cur_method->receiver_class();
            LambdaListHandler_sp method_llh = cur_method->method_lambda_list_handler();
            Symbol_sp receiverClassNameSymbol = receiverClass->className();
            string receiverClassName = receiverClassNameSymbol->symbolNameAsString();
            emf_name_ss << gfname << "->" << receiverClassName;
            Symbol_sp emf_name = _lisp->intern(emf_name_ss.str(),af_functionBlockName(this->getFunctionName())->getPackage());
            Lambda_emf* l_emf = _NEW_(Lambda_emf(emf_name_ss.str(),this->sharedThis<SingleDispatchGenericFunction_O>(),
                                                 emf_name, cur_method,cCdr(applicable_methods)));
            CompiledBody_sp cb_l_emf = CompiledBody_O::create(l_emf,_Nil<T_O>(),_lisp);
            Function_sp emf = BuiltIn_O::create(emf_name,
                                                _Nil<LambdaListHandler_O>(),
                                                cb_l_emf,
                                                _Nil<ActivationFrame_O>(),
                                                kw::_sym_function );
            return emf;
        }
#endif

}; /* core */
