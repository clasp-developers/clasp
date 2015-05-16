/*
    File: multipleDispatchGenericFunction.cc
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
#include "core/foundation.h"
#include "core/common.h"
#include "core/environment.h"
#include "core/symbolTable.h"
#include "core/hashTable.h"
#include "core/hashTableEql.h"
#include "core/package.h"
#include "core/executables.h"
#include "core/multipleValues.h"
#include "core/lambdaListHandler.h"
#include "core/multipleDispatchEffectiveMethodFunction.h"
#include "core/multipleDispatchGenericFunction.h"
#include "core/multipleDispatchMethod.h"
#include "core/wrappers.h"
#include "core/sort.h"

namespace core {

// ----------------------------------------------------------------------
//

EXPOSE_CLASS(core, MultipleDispatchGenericFunction_O);

void MultipleDispatchGenericFunction_O::exposeCando(::core::Lisp_sp lisp) {
  ::core::class_<MultipleDispatchGenericFunction_O>()
      .def_readonly("multiple_dispatch_generic_function_methods", &MultipleDispatchGenericFunction_O::_Methods, "Access the methods of the multiple-dispatch-generic-function")
      //	.initArgs("(self)")
      ;
}

void MultipleDispatchGenericFunction_O::exposePython(::core::Lisp_sp lisp) {
  PYTHON_CLASS(Pkg(), MultipleDispatchGenericFunction, "", "", lisp->lisp())
      //	.initArgs("(self)")
      ;
}

MultipleDispatchGenericFunction_sp MultipleDispatchGenericFunction_O::create(Symbol_sp name, int dispatch_on_required_argument_index, Lisp_sp lisp) {
  _G();
  GC_RESERVE_TRY(MultipleDispatchGenericFunction_O, gf) {
    GC_RESERVE_GET(MultipleDispatchGenericFunction_O, gf);
    gf->setSymbol(name);
    gf->_DispatchOnIndex = dispatch_on_required_argument_index;
  }
  return gf;
}

::core::T_sp MultipleDispatchGenericFunction_O::__init__(::core::Executable_sp exec, ::core::Cons_sp args, ::core::Environment_sp env, ::core::Lisp_sp lisp) {
  _G();
  //      this->Base::__init__(exec,args,env,lisp);
  //      arg = translate::from_object<XXXX>::convert(env->lookup(this->Package(),"YYY"));
  return _lisp->onil();
}

#if 0
    void MultipleDispatchGenericFunction_O::serialize(::serialize::SNodeP node)
    {
	IMPLEMENT_ME();
        this->Bases::serialize(node);
	// Archive other instance variables here
    }
#endif

void MultipleDispatchGenericFunction_O::initialize() {
  _OF();
  this->Base::initialize();
  this->_Methods = _lisp->cnil();
  this->_classes_to_emf_table = HashTableEql_O::create_default(_lisp);
}

void MultipleDispatchGenericFunction_O::defmethod(Symbol_sp name, MultipleDispatchMethod_sp method) {
  _OF();
  // Look to see if the method is already defined
  LOG(BF("defmethod for symbol[%s] called with method with receiverClass[%s]") % name->__repr__() % method->receiver_class()->__repr__());
  bool replacedMethod = false;
  {
    _BLOCK_TRACEF(BF("Checking if the receiver class already has a method"));
    for (Cons_sp cur = this->_Methods; cur->notNil(); cur = cur->cdr()) {
      MultipleDispatchMethod_sp existing = cur->car<MultipleDispatchMethod_O>();
      LOG(BF("An existing method has receiverClass[%s]") % existing->receiver_class()->__repr__());
      if (existing->receiver_class() == method->receiver_class()) {
        LOG(BF("... and that matches the defmethod reciever class"));
        if (!existing->can_be_redefined()) {
          THROW(_lisp->error(BF("You tried to overwrite a locked method with name[%s]") % name->__repr__()));
        } else {
          cur->setCar(method);
          replacedMethod = true;
          break;
        }
      }
    }
  }
  if (!replacedMethod) {
    LOG(BF("This is a new method - adding it to the Methods list"));
    this->_Methods = Cons_O::create(method, this->_Methods, _lisp);
  }
  this->_classes_to_emf_table->clrhash();
}

/*! I think this fills the role of the lambda returned by
      std-compute-discriminating-function (gf) AMOP-303 top
    */
T_sp MultipleDispatchGenericFunction_O::INVOKE(Cons_sp args) {
  _OF();
  T_sp dispatchArg = args->elt(this->_DispatchOnIndex);
  MetaClass_sp dispatchArgClass = dispatchArg->_class();
  LOG(BF("Invoked MultipleDispatchGenericFunction[%s] with receiver class[%s]") % this->getSymbol()->__repr__() % dispatchArgClass->__repr__());
  Function_sp emf =
      this->_classes_to_emf_table->gethash(dispatchArgClass, MultipleDispatchEffectiveMethodFunction_O::nil(_lisp))
          ->object()
          ->as<MultipleDispatchEffectiveMethodFunction_O>();
  if (emf->isNil()) {
    LOG(BF("There was no effective method function defined - building one"));
    emf = this->slow_method_lookup(dispatchArgClass);
  }
  LOG(BF("Invoking the effective method function"));
  return emf->INVOKE(args);
};

class MultipleDispatch_OrderByClassPrecedence {
public:
  bool operator()(MultipleDispatchMethod_sp x, MultipleDispatchMethod_sp y) {
    return x->receiver_class()->isSubClassOf(y->receiver_class()); // or should it be y->isSubClassOf(x)?????
  }
};

Function_sp MultipleDispatchGenericFunction_O::slow_method_lookup(MetaClass_sp mc) {
  _OF();
  LOG(BF("Looking for applicable methods for receivers of class[%s]") % mc->__repr__());
  Vector0<MultipleDispatchMethod_O> applicable_methods;
  for (Cons_sp cur = this->_Methods; cur->notNil(); cur = cur->cdr()) {
    MultipleDispatchMethod_sp sdm = cur->ocar()->as<MultipleDispatchMethod_O>();
    MetaClass_sp ac = sdm->receiver_class();
    if (mc->isSubClassOf(ac)) {
      LOG(BF("Found applicable method with receiver class[%s]") % ac->__repr__());
      applicable_methods.push_back(sdm);
    } else {
      LOG(BF("%s is not a subclass of %s") % mc->__repr__() % ac->__repr__());
    }
  }
  if (applicable_methods.size() == 0) {
    THROW(_lisp->error(BF("There are no applicable methods of %s for receiver class %s") % this->getSymbol()->__repr__() % mc->instanceClassName()));
  }
  /* Sort the methods from most applicable to least applicable */
  MultipleDispatch_OrderByClassPrecedence sort_by_class_precedence;
  sort::quickSort(applicable_methods.begin(), applicable_methods.end(), sort_by_class_precedence, _lisp);
  if (applicable_methods.size() >= 2) {
    // Check if we got the sort order right - later take this check out
    Vector0<MultipleDispatchMethod_O>::const_iterator am1 = applicable_methods.begin();
    Vector0<MultipleDispatchMethod_O>::const_iterator am2 = am1 + 1;
    if (!(*am1)->receiver_class()->isSubClassOf((*am2)->receiver_class())) {
      THROW(_lisp->error(BF("Sort of applicable_methods got the order wrong")));
    }
#if 0
	    printf("%s:%d - Check of sorted order of applicable_methods for [%s] got "
		   "first method receiver class[%s] and second method receiver class[%s]\n",
		   __FILE__,__LINE__, this->getSymbol()->__repr__().c_str(),
		   (*am1)->receiver_class()->__repr__().c_str(),(*am2)->receiver_class()->__repr__().c_str() );
#endif
  }
  Cons_sp methods = applicable_methods.asCons(_lisp);
  Function_sp emf = this->compute_effective_method_function(methods);
  return emf;
}

class Lambda_emf : public Functoid {
private:
  /*! Store the name of the function that this Lambda_emf invokes - for debugging */
  Symbol_sp _name;
  /*! Store the method_function that this emf invokes.
	  This function takes two arguments: (args next-emfun) */
  Function_sp _method_function;
  /*! Store the next-emfun that will be passed to the _method_function */
  Function_sp _next_emfun;

public:
  Lambda_emf(MultipleDispatchGenericFunction_sp gf,
             Symbol_sp emf_name,
             MultipleDispatchMethod_sp cur_method,
             Cons_sp next_methods) {
    _G();
    this->_name = emf_name;
    this->_method_function = cur_method->_method_function;
    // Calculate the function to call with call_next_method
    // Do this by recursively calling gf->compute_effective_method_function with next_methods
    if (next_methods->isNil()) {
      this->_next_emfun = Function_O::nil(_lisp);
    } else {
      this->_next_emfun = gf->compute_effective_method_function(next_methods);
    }
  }

  DISABLE_NEW();

  /*! This invoke will get the arguments for the method.  
	  They will be passed to Lambda_emf::_function as the first argument of the (args next-emfun) pair
	  of arguments.  The next-emfun 
	*/
  virtual T_sp invoke(Executable_sp e, Cons_sp args, Environment_sp environment, Lisp_sp lisp) {
    _G();
    Cons_sp method_function_args = Cons_O::create(args, Cons_O::create(this->_next_emfun, _lisp), _lisp);
    return this->_method_function->INVOKE(method_function_args);
  }
};

Function_sp MultipleDispatchGenericFunction_O::compute_effective_method_function(Cons_sp applicable_methods) {
  _OF();
  if (applicable_methods->isNil()) {
    THROW(_lisp->error(BF("You cannot compute_effective_method_function for gf[%s] because there are no methods!") % this->getSymbol()->__repr__()));
  }
  MultipleDispatchMethod_sp cur_method = applicable_methods->ocar()->as<MultipleDispatchMethod_O>();
  ASSERTF(cur_method->notNil(), BF("There is no method to compute_effective_method_function for"));
  // Construct a name for the emf by stringing together the generic function name
  // with the name of the receiver class - this is to help with debugging
  stringstream emf_name_ss;
  string gfname = this->getSymbol()->symbolName();
  MetaClass_sp receiverClass = cur_method->receiver_class();
  Symbol_sp receiverClassNameSymbol = receiverClass->classNameSymbol();
  string receiverClassName = receiverClassNameSymbol->symbolName();
  emf_name_ss << gfname << "->" << receiverClassName;
  Symbol_sp emf_name = _lisp->intern(emf_name_ss.str(), this->getSymbol()->getPackage());
  Lambda_emf *l_emf = _NEW_(Lambda_emf(this->sharedThis<MultipleDispatchGenericFunction_O>(),
                                       emf_name, cur_method, applicable_methods->cdr()));
#if 0
	FunctionPrimitive_sp emf = FunctionPrimitive_O::create(emf_name,l_emf,_lisp->nil<LambdaListHandler_O>(),"",_kw_function,_lisp);
#else
  CompiledBody_sp cb_l_emf = CompiledBody_O::create(l_emf, _lisp);
  Function_sp emf = Function_O::create(emf_name,
                                       _lisp->nil<LambdaListHandler_O>(),
                                       _lisp->cnil(),
                                       _lisp->nil<Str_O>(),
                                       cb_l_emf,
                                       _lisp->top_level_environment(),
                                       _kw_function,
                                       _lisp);
#endif
  return emf;
}

}; /* core */
