#ifndef _singleDispatchMethod_H_
#define _singleDispatchMethod_H_

#include "core/foundation.h"
#include "core/object.h"
#include "core/singleDispatchMethod.fwd.h"


namespace core
{
    class SingleDispatchMethod_O : public T_O
    {
        friend class SingleDispatchGenericFunctionClosure;
	LISP_BASE1(T_O);
	LISP_CLASS(core,CorePkg,SingleDispatchMethod_O,"SingleDispatchMethod");
	DECLARE_INIT();
//    DECLARE_ARCHIVE();
    public:
	friend class SingleDispatchMethodPrimitive_O;
	friend class SingleDispatchGeneralFunction_O;
	friend class Lambda_emf;
	friend class Lambda_method_function;
    public: // Simple default ctor/dtor
	DEFAULT_CTOR_DTOR(SingleDispatchMethod_O);
    private: // instance variables here
		/*! Store the generic function name */
	Symbol_sp	_name;
	/*! Store the receiver class for this method */
	Class_sp	_receiver_class;
	/*! Store the body of the method */
	Function_sp     code;
//        CompiledBody_sp		_body;
//	BuiltIn_sp	_method_builtin;
	/*! This is the LambdaListHandler for the Builtin method */
	LambdaListHandler_sp	_argument_handler;
	Cons_sp 	_declares;
	/*! Store the docstring */
	Str_sp		_docstring;
    public: // creation function
	// The creates above are depreciated
	static SingleDispatchMethod_sp create(Symbol_sp name,
					      Class_sp receiver,
					      LambdaListHandler_sp lambda_list_handler,
					      Cons_sp declares, Str_sp docstr,
					      Function_sp body );
    public: // Functions here

	Class_sp receiver_class() const { return this->_receiver_class; };
	LambdaListHandler_sp method_lambda_list_handler() const { return this->_argument_handler;};
	string __repr__() const;


    }; // SingleDispatchMethod class
    
}; // core namespace
template<> struct gctools::GCInfo<core::SingleDispatchMethod_O> {
    static bool constexpr NeedsInitialization = false;
    static bool constexpr NeedsFinalization = false;
    static bool constexpr Moveable = true;
    static bool constexpr Atomic = false;
};
TRANSLATE(core::SingleDispatchMethod_O);



namespace core {

#if 0
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
#endif

#if 0
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
        void LISP_CALLING_CONVENTION()
        {
            IMPLEMENT_MEF(BF("Handle new calling method"));
#if 0
	T_sp invoke(Function_sp e,Cons_sp cnm_args, Environment_sp env, Lisp_sp lisp )
	{_G();
	    if ( this->_next_emfun.notnilp() ) return _lisp->_true();
	    return _Nil<T_O>();
        }
#endif
        }
    };
#endif




    /*! A method function when invoked is given two arguments: (args next-emfun)
      It creates a FunctionValueEnvironment that defines call-next-method and next-method-p 
      with the method environment as its parent and then invokes the method-function
      with (args next-emfun) */
    class Lambda_method_function : public BuiltinClosure
    {
        FRIEND_GC_SCANNER();
    private:
	SingleDispatchMethod_sp	_method;
	Function_sp 		_temporary_function;
    public:
	string describe() const { return "Lambda_method_function";};
    public:
	Lambda_method_function(T_sp name, SingleDispatchMethod_sp method)
	    : BuiltinClosure(name)
	{_G();
	    this->_method = method;
	    this->_temporary_function = _Nil<Function_O>();
	}

        DISABLE_NEW();
        virtual size_t templatedSizeof() const { return sizeof(*this);};
	bool requires_activation_frame() const { return true; };

	/*! The argument list is: (args next-emfun)
	  Use next-emfun to set up a FunctionValueEnvironment that defines call-next-method and next-method-p */
	void LISP_INVOKE();
    };

};


#endif /* _singleDispatchMethod_H_ */
