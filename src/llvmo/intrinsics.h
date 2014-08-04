#ifndef	llvmo_primitives_H
#define llvmo_primitives_H

extern "C"
{

    typedef void (*fnLispCallingConvention)(LISP_CALLING_CONVENTION_RETURN, LISP_CALLING_CONVENTION_CLOSED_ENVIRONMENT, LISP_CALLING_CONVENTION_ARGS );
    typedef void (*fnVoidType)();

#if 0
    class  JITClosure : public core::FunctionClosure
    {
    protected:
	fnLispCallingConvention	_FuncPtr;
    public:
	// By default the zeroth argument is dispatched on
	explicit JITClosure(core::T_sp name,core::T_sp closedEnv,fnLispCallingConvention fptr) : FunctionClosure(name,closedEnv), _FuncPtr(fptr) {};
        DISABLE_NEW();
        virtual size_t templatedSizeof() const { return sizeof(*this);};
	virtual string describe() const {return "JITClosure";};
	void LISP_CALLING_CONVENTION()
	{
            IMPLEMENT_MEF(BF("Handle LISP_CALLING_CONVENTION"));
#if 0
	    core::T_mv result;
	    core::ActivationFrame_sp tclosedEnv(closedEnv);
	    this->_FuncPtr(&result,&tclosedEnv,nargs, args);
	    return result;
#endif
	}
    };
#endif


};




namespace llvmo
{

    void redirect_llvm_interface_addSymbol();

    void initialize_intrinsics();
}

#endif
