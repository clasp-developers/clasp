#ifndef _singleDispatchMethod_H_
#define _singleDispatchMethod_H_

#include "core/foundation.h"
#include "core/object.h"
#include "core/singleDispatchMethod.fwd.h"


namespace core
{
    class SingleDispatchMethod_O : public T_O
    {
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
	CompiledBody_sp		_body;
	BuiltIn_sp	_method_builtin;
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
					      CompiledBody_sp body );
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




#endif /* _singleDispatchMethod_H_ */
