/*
    File: singleDispatchMethod.cc
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
#include <clasp/core/common.h>
#include <clasp/core/str.h>
#include <clasp/core/environment.h>
#include <clasp/core/symbolTable.h>
#include <clasp/core/lambdaListHandler.h>
#include <clasp/core/evaluator.h>
#include <clasp/core/singleDispatchMethod.h>
#include <clasp/core/wrappers.h>

namespace core {

/*! The argument list is: (args next-emfun)
	  Use next-emfun to set up a FunctionValueEnvironment that defines call-next-method and next-method-p */
void Lambda_method_function::LISP_INVOKE() {
#if 0
	    ASSERTF(this->_method->_body.notnilp(),BF("The method body should never by nil"));
// TODO: Make this more efficient - this is crazy to put the arguments into a Cons and then
// into an ActivationFrame and then back into a Cons for each method call - or am I really doing that?
// Sep 21 2012
	    // First argument is an ActivationFrame_sp
	    ActivationFrame_sp function_args = args[0].as<ActivationFrame_O>();
	    Function_sp next_emfun = args[1].as<Function_O>();
	    
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
#if 0
	    Function_sp tempFunc = Function_O::create_single_dispatch_function(this->_method->_name,
									       this->_method->_argument_handler, // pass the ArgumentHandler
									       this->_method->_body,	// Body code is here
									       funcEnv,		// here is the call-next-method/next-method-p extended method env
									       _lisp);
	    // --- The following is just apply - why don't I create an apply_in_environment function
	    // and just apply it in funcEnv - then I don't have to ctor/ctor a Function object
	    // -----> I can't do this because apply just called Invoke on the function object and that 
	    //        uses its closed over environment
#else
	    if ( this->_temporary_function.nilp() )
	    {
		Function_sp tempFunc = BuiltIn_O::create_single_dispatch_function(this->_method->_name,
											   this->_method->_argument_handler, // pass the ArgumentHandler
											   this->_method->_body );
//		printf("%s:%d - created temporary single-dispatch-function\n", __FILE__, __LINE__ );
		this->_temporary_function = tempFunc;
	    }
#endif
#endif

  IMPLEMENT_MEF(BF("Handle single dispatch method - fixup the code below"));
#if 0 // Fix the following so that it will work 
	    Functoid* functoid = this->_method->_body->functoid();
	    ValueFrame_sp frame(ValueFrame_O::createForLambdaListHandler(this->_method->_argument_handler,_Nil<ActivationFrame_O>()));
	    ActivationFrameDynamicScopeManager scope(frame);
	    this->_method->_argument_handler->createBindingsInScope_argArray(llc_nargs,llc_fixed_arg0,llc_fixed_arg1,llc_fixed_arg2,llc_arglist,scope);
//	    DBG_HOOK(BF("Check the new arguments"));
	    return varargs_activateWithFrameef(functoid,frame->activate(,frame->length(),frame->argArray());
#endif
}
};

namespace core {

// ----------------------------------------------------------------------
//






SingleDispatchMethod_sp SingleDispatchMethod_O::create(Symbol_sp name,
                                                       Class_sp receiverClass,
                                                       LambdaListHandler_sp llh,
                                                       List_sp declares,
                                                       gc::Nilable<Str_sp> docstr,
                                                       Function_sp body) {
  GC_ALLOCATE(SingleDispatchMethod_O, method);
  method->_name = name;
  method->_receiver_class = receiverClass;
  ASSERTF(body.notnilp(), BF("The body of a method should never be nil"));
  method->code = body;
  method->_argument_handler = llh;
  method->_declares = declares;
  method->_docstring = docstr;
// method->_Function this is what we need to set up NOW.
// -- this function has to accept two arguments: (args next-emfun)
// So it's a chainable methoid, it can be called with a next-emfun argument
// which can be called by applying arguments to the local function "call-next-method"
#if 0
	CompiledBody_sp cb_method_function_primitive = CompiledBody_O::create(method_functoid,_Nil<T_O>());
	LambdaListHandler_sp llh_pass_arguments_through(_Nil<LambdaListHandler_O>());
	BuiltinClosure* method_functoid = gctools::ClassAllocator<Lambda_method_function>::allocate_class(name,method);
	method->_method_builtin = BuiltIn_O::make(name,llh_pass_arguments_through,cb_method_function_primitive);
#endif
  return method;
}

string SingleDispatchMethod_O::__repr__() const {
  stringstream ss;
  ss << "#<" << this->_instanceClass()->classNameAsString()
     << " :name " << _rep_(this->_name)
     << " :receiver-class " << _rep_(this->_receiver_class)
     << " :code " << _rep_(this->code)
     //	   << " :method_builtin " << _rep_(this->_method_builtin)
     << " >";
  return ss.str();
}

}; /* core */
