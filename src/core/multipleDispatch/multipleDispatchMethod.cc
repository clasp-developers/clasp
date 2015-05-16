/*
    File: multipleDispatchMethod.cc
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
#include "core/common.h"
#include "core/environment.h"
#include "core/symbolTable.h"
#include "core/lambdaListHandler.h"
#include "core/multipleDispatchMethod.h"
#include "core/wrappers.h"
namespace core {

// ----------------------------------------------------------------------
//

EXPOSE_CLASS(core, MultipleDispatchMethod_O);

void MultipleDispatchMethod_O::exposeCando(::core::Lisp_sp lisp) {
  ::core::class_<MultipleDispatchMethod_O>()
      //	.initArgs("(self)")
      ;
}

void MultipleDispatchMethod_O::exposePython(::core::Lisp_sp lisp) {
  PYTHON_CLASS(Pkg(), MultipleDispatchMethod, "", "", lisp->lisp())
      //	.initArgs("(self)")
      ;
}

class Lambda_call_next_method : public Functoid {
private:
  /* Store the name of the previous function */
  Symbol_sp _previous_emf_name;
  /*! Store the next function to call */
  Function_sp _next_emfun;
  /*! Store the arguments that were passed to the function that called us */
  Cons_sp _arguments;

public:
  Lambda_call_next_method(Symbol_sp previous_emf_name, Cons_sp args, Function_sp next_emfun) {
    _G();
    this->_previous_emf_name = previous_emf_name;
    this->_next_emfun = next_emfun;
    this->_arguments = args;
  }

  DISABLE_NEW();

  /*! The argument list is: (&rest cnm_args)
	  If no arguments are passed to this invoke then
	  use the arguments that are stored in _arguments */
  T_sp invoke(Executable_sp e, Cons_sp cnm_args, Environment_sp env, Lisp_sp lisp) {
    _G();
    if (this->_next_emfun->isNil()) {
      THROW(_lisp->error(BF("No next method for generic function %s") % this->_previous_emf_name->__repr__()));
    }
    Cons_sp args = cnm_args;
    if (args->isNil())
      args = this->_arguments;
    return this->_next_emfun->INVOKE(args);
  }
};

class Lambda_next_method_p : public Functoid {
private:
  /*! Store the next function to call */
  Function_sp _next_emfun;

public:
  Lambda_next_method_p(Function_sp next_emfun) {
    this->_next_emfun = next_emfun;
  }
  DISABLE_NEW();

  /*! Doesn't take any arguments */
  T_sp invoke(Executable_sp e, Cons_sp cnm_args, Environment_sp env, Lisp_sp lisp) {
    _G();
    if (this->_next_emfun->notNil())
      return _lisp->_true();
    return _lisp->onil();
  }
};

/*! A method function when invoked is given two arguments: (args next-emfun)
      It creates a FunctionEnvironment that defines call-next-method and next-method-p 
      with the method environment as its parent and then invokes the method-function
      with (args next-emfun) */
class Lambda_method_function_primitive : public Functoid {
private:
  MultipleDispatchMethod_sp _method;
  Functoid *_methoid; // Take ownership of this
public:
  Lambda_method_function_primitive(MultipleDispatchMethod_sp method, Functoid *methoid) {
    _G();
    this->_method = method;
    this->_methoid = methoid;
  }

  DISABLE_NEW();

  /*! The argument list is: (args next-emfun)
	  Use next-emfun to set up a FunctionEnvironment that defines call-next-method and next-method-p */
  T_sp invoke(Executable_sp e, Cons_sp args, Environment_sp environ, Lisp_sp lisp) {
    _G();
    Cons_sp function_args = args->ocar()->as<Cons_O>();
    //
    // primitives will never ask for next-emfun so don't bother putting it together
    //
    // ignore Function_sp next_emfun = args->ocadr()->as<Function_O>();
    //
    // Build a FunctionEnvironment that stores call-next-method and next-method-p
    //
    // ignore FunctionEnvironment_sp funcEnv = FunctionEnvironment_O::create(_lisp->nil<Symbol_O>(),this->_method->_environment,_lisp);
    // ignore FunctionPrimitive_sp fp_call_next_method = FunctionPrimitive_O::create(_NEW(Lambda_call_next_method(function_args,next_emfun)),_lisp);
    // ignore FunctionPrimitive_sp fp_next_method_p = FunctionPrimitive_O::create(_NEW(Lambda_next_method(next_emfun)),_lisp);
    // ignore funcEnv->extend_function(_sym_call_next_function,fp_call_next_method);
    // ignore funcEnv->extend_function(_sym_next_method_p,fp_next_method_p);
    return this->_methoid->invoke(e, function_args, _lisp->topEnv(), lisp);
  }
};

/*! A method function when invoked is given two arguments: (args next-emfun)
      It creates a FunctionEnvironment that defines call-next-method and next-method-p 
      with the method environment as its parent and then invokes the method-function
      with (args next-emfun) */
class Lambda_method_function : public Functoid {
private:
  MultipleDispatchMethod_sp _method;

public:
  Lambda_method_function(MultipleDispatchMethod_sp method) {
    _G();
    this->_method = method;
  }

  /*! The argument list is: (args next-emfun)
	  Use next-emfun to set up a FunctionEnvironment that defines call-next-method and next-method-p */
  T_sp invoke(Executable_sp e, Cons_sp args, Environment_sp environ, Lisp_sp lisp) {
    _G();
    ASSERTF(this->_method->_body->notNil(), BF("The method body should never by nil"));
    Cons_sp function_args = args->ocar()->as<Cons_O>();
    Function_sp next_emfun = args->ocadr()->as<Function_O>();
    //
    // Build a FunctionEnvironment that stores call-next-method and next-method-p
    //
    SYMBOL_SC_(call_next_method);
    SYMBOL_SC_(next_method_p);
    FunctionEnvironment_sp funcEnv = FunctionEnvironment_O::create(_lisp->nil<Symbol_O>(), this->_method->_environment, _lisp);
#if 0
	    FunctionPrimitive_sp fp_call_next_method = FunctionPrimitive_O::create(_sym_call_next_method,
										   _NEW_(Lambda_call_next_method(this->_method->_name,function_args,next_emfun)),_lisp);
	    FunctionPrimitive_sp fp_next_method_p = FunctionPrimitive_O::create(_sym_next_method_p,
										_NEW_(Lambda_next_method_p(next_emfun)),_lisp);
#else
    CompiledBody_sp cb_call_next_method = CompiledBody_O::create(_NEW_(Lambda_call_next_method(this->_method->_name, function_args, next_emfun)), _lisp);
    Function_sp fp_call_next_method = Function_O::create(_sym_call_next_method, cb_call_next_method);
    CompiledBody_sp cb_next_method_p = CompiledBody_O::create(_NEW_(Lambda_next_method_p(next_emfun)), _lisp);
    Function_sp fp_next_method_p = Function_O::create(_sym_next_method_p, cb_next_method_p);
#endif

    funcEnv->extend_function(_sym_call_next_method, fp_call_next_method);
    funcEnv->extend_function(_sym_next_method_p, fp_next_method_p);
    //
    // At this point I have a new environment that defines
    // the local function call-next-method
    // and the local function next-method-p
    //
    // HOW DO I CALL THE METHOD CODE WITH THIS ENVIRONMENT?? and the function_args???/
    //
    // I cant call a function because that will have its own closed environment.
    //
    // CLOS std-compute-method-function uses the method-body which doesn't have an
    // environment and it defines a new function with that method-body and closes
    // it over the method-environment extended by the two local functions.
    // 1) How do I do that with a lisp method body?
    // 2) How do I do that with a methoid primitive? See Lambda_method_function_primitive above
    //
    // For the methoid primitive - what if I just invoke the methoid primitive
    // with the environment right here?
    // eg: return this->_methoid->invoke(e,function_args,funcEnv,_lisp); - that should work
    //
    // For a lisp body what if I create a Function_sp, give it the code, the environment and the lambda list
    // and then invoke that - but then it will be immediately freed after that won't it? and won't that be
    // expensive?
    // It looks like the most expensive step will be creating the ArgumentHandler - but I could generate that
    // once for the method and use it for this transient function
    // I could also lighten the Function so that the cost of setting one up and
    // destroying it will be smaller.
    //
    //
    // This is a Lambda function - make it more light-weight than this!
    // Can I create this Function_sp in the ctor for the Lambda_method_function and then just set
    // funcEnv everytime?
    // I could also create the funcEnv in the ctor and
    // redirect the call-next-method and next-method-p instance variables?
    // !!!! I probably can't because the ctor is only called once when the method is defined
    // then this method might be used in a lot of effective method functions.
    Function_sp tempFunc = Function_O::create_multiple_dispatch_function(this->_method->_name,
                                                                         this->_method->_argument_handler, // pass the ArgumentHandler
                                                                         this->_method->_body,             // Body code is here
                                                                         funcEnv,                          // here is the call-next-method/next-method-p extended method env
                                                                         _lisp);
    // --- The following is just apply - why don't I create an apply_in_environment function
    // and just apply it in funcEnv - then I don't have to ctor/ctor a Function object
    // -----> I can't do this because apply just called Invoke on the function object and that
    //        uses its closed over environment
    return eval::apply(tempFunc, function_args, _lisp);
    // At this point tempFunc will fall out of scope and be dtor'd
    // is that the cost of doing business?
  }
};

MultipleDispatchMethod_sp MultipleDispatchMethod_O::create(Symbol_sp name, MetaClass_sp receiverClass,
                                                           LambdaListHandler_sp llh,
                                                           Cons_sp declares,
                                                           Str_sp docstr,
                                                           Cons_sp body,
                                                           Lisp_sp lisp) {
  _G();
  GC_RESERVE_TRY(MultipleDispatchMethod_O, method) {
    GC_RESERVE_GET(MultipleDispatchMethod_O, method);
    method->_name = name;
    method->_receiver_class = receiverClass;
    ASSERTF(body->notNil(), BF("The body of a method should never be nil"));
    method->_body = body;
    method->_argument_handler = llh;
    method->_declares = declares;
    method->_docstring = docstr;
    method->_can_be_redefined = true;
    // method->_Function this is what we need to set up NOW.
    // -- this function has to accept two arguments: (args next-emfun)
    // So it's a chainable methoid, it can be called with a next-emfun argument
    // which can be called by applying arguments to the local function "call-next-method"
    CompiledBody_sp cb_method_function_primitive = CompiledBody_O::create(_NEW_(Lambda_method_function(method)), _lisp);
    method->_method_function = Function_O::create(name, cb_method_function_primitive);
  }
  return method;
}

::core::T_sp MultipleDispatchMethod_O::__init__(::core::Executable_sp exec, ::core::Cons_sp args, ::core::Environment_sp env, ::core::Lisp_sp lisp) {
  _G();
  //      this->Base::__init__(exec,args,env,lisp);
  //      arg = translate::from_object<XXXX>::convert(env->lookup(this->Package(),"YYY"));
  return _lisp->onil();
}

#if 0
    void MultipleDispatchMethod_O::serialize(::serialize::SNodeP node)
    {
	IMPLEMENT_ME();
        this->Bases::serialize(node);
	// Archive other instance variables here
    }
    
    void MultipleDispatchMethod_O::archiveBase(::core::ArchiveP node)
    {
	IMPLEMENT_ME();
        this->Base1::archiveBase(node);
	// Archive other instance variables here
    }
#endif

void MultipleDispatchMethod_O::initialize() {
  _OF();
  this->Base::initialize();
  this->_environment = _lisp->topEnv();
  this->_can_be_redefined = true;
}

string MultipleDispatchMethod_O::__repr__() const {
  stringstream ss;
  ss << "#<" << this->className()
     << " :name " << this->_name->__repr__()
     << " :receiver-class " << this->_receiver_class->__repr__()
     << " :body " << this->_body->__repr__()
     << " :method_function " << this->_method_function->__repr__()
     << " >";
  return ss.str();
}

}; /* core */
