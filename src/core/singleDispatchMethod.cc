
#define	DEBUG_LEVEL_FULL
#include "core/common.h"
#include "core/environment.h"
#include "symbolTable.h"
#include "core/lambdaListHandler.h"
#include "core/evaluator.h"
#include "core/singleDispatchMethod.h"
#include "core/wrappers.h"
namespace core
{
    
// ----------------------------------------------------------------------
//
    
    EXPOSE_CLASS(core,SingleDispatchMethod_O);
    
    void SingleDispatchMethod_O::exposeCando(::core::Lisp_sp lisp)
    {
	::core::class_<SingleDispatchMethod_O>()
//	.initArgs("(self)")
	    ;
    }
    
    void SingleDispatchMethod_O::exposePython(::core::Lisp_sp lisp)
    {
#ifdef USEBOOSTPYTHON
	PYTHON_CLASS(Pkg(),SingleDispatchMethod,"","",_LISP)
//	.initArgs("(self)")
	    ;
#endif
    }

    class Lambda_call_next_method : public Functoid
    {
    private:
	/* Store the name of the previous function */
	Symbol_sp	_previous_emf_name;
	/*! Store the next function to call */
	Function_sp	_next_emfun;
	/*! Store the arguments that were passed to the function that called us */
	ActivationFrame_sp		_arguments;
    public:
	string describe() const { return "Lambda_call_next_method";};
    public:
	Lambda_call_next_method(const string& name, Symbol_sp previous_emf_name, ActivationFrame_sp args, Function_sp next_emfun) : Functoid("Lambda_call_next_method->"+name)
	{_G();
	    this->_previous_emf_name = previous_emf_name;
	    this->_next_emfun = next_emfun;
	    this->_arguments = args;
	}

        DISABLE_NEW();

	/*! Indicates if this Functoid uses activation frames to get arguments */
	virtual bool	requires_activation_frame() const { return true;}

#if 0
	/*! The argument list is: (&rest cnm_args)
	  If no arguments are passed to this invoke then
	  use the arguments that are stored in _arguments */
	T_sp invoke(Function_sp e,Cons_sp cnm_args, Environment_sp env, Lisp_sp lisp )
	{_G();
	    if ( this->_next_emfun.nilp() )
	    {
		SIMPLE_ERROR(BF("No next method for generic function %s") % this->_previous_emf_name->__repr__() );
	    }
	    Cons_sp args = cnm_args;
	    if ( args.nilp() ) args = this->_arguments;
	    return this->_next_emfun->INVOKE(args);
	}

#endif



    };


    class Lambda_next_method_p : public Functoid
    {
    private:
	/*! Store the next function to call */
	Function_sp	_next_emfun;
    public:
	string describe() const { return "Lambda_next_method_p";};
    public:
	Lambda_next_method_p(const string& name, Function_sp next_emfun) : Functoid("Lambda_next_method_p->"+name)
	{
	    this->_next_emfun = next_emfun;
	}

        DISABLE_NEW();

	/*! Doesn't take any arguments */
	T_sp invoke(Function_sp e,Cons_sp cnm_args, Environment_sp env, Lisp_sp lisp )
	{_G();
	    if ( this->_next_emfun.notnilp() ) return _lisp->_true();
	    return _Nil<T_O>();
	}
    };





    /*! A method function when invoked is given two arguments: (args next-emfun)
      It creates a FunctionValueEnvironment that defines call-next-method and next-method-p 
      with the method environment as its parent and then invokes the method-function
      with (args next-emfun) */
    class Lambda_method_function : public Functoid
    {
    private:
	SingleDispatchMethod_sp	_method;
	Function_sp 		_temporary_function;
    public:
	string describe() const { return "Lambda_method_function";};
    public:
	Lambda_method_function(const string& name, SingleDispatchMethod_sp method)
	    : Functoid("Lambda_method_function->"+name)
	{_G();
	    this->_method = method;
	    this->_temporary_function = _Nil<Function_O>();
	}

        DISABLE_NEW();

	bool requires_activation_frame() const { return true; };




	/*! The argument list is: (args next-emfun)
	  Use next-emfun to set up a FunctionValueEnvironment that defines call-next-method and next-method-p */
	T_mv activate( ActivationFrame_sp closedEnv, int nargs, ArgArray args)
	{_G();
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
	    Functoid* functoid = this->_method->_body->functoid();
	    ValueFrame_sp frame(ValueFrame_O::createForLambdaListHandler(this->_method->_argument_handler,_Nil<ActivationFrame_O>()));
	    ActivationFrameDynamicScopeManager scope(frame);
	    this->_method->_argument_handler->createBindingsInScope_argArray(nargs,args,scope);
//	    DBG_HOOK(BF("Check the new arguments"));
	    return functoid->activate(_Nil<ActivationFrame_O>(),frame->length(),frame->argArray());
	}
    };





    SingleDispatchMethod_sp SingleDispatchMethod_O::create(Symbol_sp name,
							   Class_sp receiverClass,
							   LambdaListHandler_sp llh,
							   Cons_sp declares,
							   Str_sp docstr,
							   CompiledBody_sp body )
    {_G();
        GC_ALLOCATE(SingleDispatchMethod_O,method );
	method->_name = name;
	method->_receiver_class = receiverClass;
	ASSERTF(body.notnilp(),BF("The body of a method should never be nil"));
	method->_body = body; 
	method->_argument_handler = llh;
	method->_declares = declares;
	method->_docstring = docstr;
	// method->_Function this is what we need to set up NOW.
	// -- this function has to accept two arguments: (args next-emfun)
	// So it's a chainable methoid, it can be called with a next-emfun argument
	// which can be called by applying arguments to the local function "call-next-method"
	Functoid* method_functoid = gctools::allocateFunctoid<Lambda_method_function>(name->fullName(),method);
	CompiledBody_sp cb_method_function_primitive = CompiledBody_O::create(method_functoid,_Nil<T_O>());
	LambdaListHandler_sp llh_pass_arguments_through(_Nil<LambdaListHandler_O>());
	method->_method_builtin = BuiltIn_O::make(name,llh_pass_arguments_through,cb_method_function_primitive);
	return method;
    }










    string SingleDispatchMethod_O::__repr__() const
    {
	stringstream ss;
	ss << "#<" << this->_instanceClass()->classNameAsString()
	   << " :name " << _rep_(this->_name)
	   << " :receiver-class " << _rep_(this->_receiver_class)
	   << " :body " << _rep_(this->_body)
	   << " :method_builtin " << _rep_(this->_method_builtin)
	   << " >";
	return ss.str();
    }
    
    
}; /* core */
