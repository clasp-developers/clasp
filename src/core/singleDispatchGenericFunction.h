#ifndef _SINGLEDISPATCHGENERICFUNCTION_H_
#define _SINGLEDISPATCHGENERICFUNCTION_H_

#include "foundation.h"
#include "hashTable.fwd.h"
#include "singleDispatchGenericFunction.fwd.h"
#include "singleDispatchMethod.fwd.h"
#include "singleDispatchEffectiveMethodFunction.fwd.h"



namespace core
{



    class SingleDispatchGenericFunctionClosure : public FunctionClosure
    {
    public:
	/*! Store the methods here */
	Cons_sp		_Methods;
        LambdaListHandler_sp _lambdaListHandler;
	/*! Store the method functions hashed on the receiver class */
//	HashTable_sp	classesToClosures;
    public:
        DISABLE_NEW();
        SingleDispatchGenericFunctionClosure(T_sp name, SourcePosInfo_sp sp, Symbol_sp k)
            : FunctionClosure(name,sp,k,_Nil<T_O>()/*Env*/)
            , _Methods(_Nil<Cons_O>())
            , _lambdaListHandler(_Nil<LambdaListHandler_O>()) {};
        SingleDispatchGenericFunctionClosure(T_sp name)
            : FunctionClosure(name)
            , _Methods(_Nil<Cons_O>())
            , _lambdaListHandler(_Nil<LambdaListHandler_O>()) {};
        void finishSetup(LambdaListHandler_sp llh, Symbol_sp k) {
            this->_lambdaListHandler = llh;
            this->kind = k;
        }
        virtual size_t templatedSizeof() const { return sizeof(*this); };
	virtual string describe() const {return "SingleDispatchGenericFunctionClosure";};
	virtual void LISP_CALLING_CONVENTION();
        bool singleDispatchGenericP() const { return true; };

	/*! Define a method to this SingleDispatchGenericFunction
	  If there is already a method with the same receiver then replace it
	  unless it's locked. Whenever a method is defined the method combination table
	  is wiped out */
        void addMethod(SingleDispatchMethod_sp method);
        LambdaListHandler_sp lambdaListHandler() const { return this->_lambdaListHandler;};

        Function_sp slowMethodLookup(Class_sp mc);
        Function_sp computeEffectiveMethodFunction(gctools::Vec0<SingleDispatchMethod_sp> const& applicableMethods);


    };






    class SingleDispatchGenericFunction_O : public Function_O
    {
	LISP_BASE1(Function_O);
	LISP_CLASS(core,CorePkg,SingleDispatchGenericFunction_O,"single-dispatch-generic-function");
	DECLARE_INIT();
//    DECLARE_ARCHIVE();
	friend class SingleDispatchGenericFunctoid;
    public: // Simple default ctor/dtor
	DEFAULT_CTOR_DTOR(SingleDispatchGenericFunction_O);
    public: // ctor/dtor for classes with shared virtual base
//    explicit SingleDispatchGenericFunction_O(core::Class_sp const& mc) : T_O(mc), Function(mc) {};
//    virtual ~SingleDispatchGenericFunction_O() {};
    public:
	static SingleDispatchGenericFunction_sp create(T_sp functionName, LambdaListHandler_sp llhandler);
    public: // Functions here

	/*! Return the Cons of methods attached to this SingleDispatchGenericFunction */
	Cons_sp methods() const {
            SingleDispatchGenericFunctionClosure* cl = dynamic_cast<SingleDispatchGenericFunctionClosure*>(this->closure);
            return cl->_Methods;
        };

    }; // SingleDispatchGenericFunction class
    
}; // core namespace
TRANSLATE(core::SingleDispatchGenericFunction_O);



namespace core
{
    class SingleDispatchGenericFunctoid : public Closure
    {
    private:
	SingleDispatchGenericFunction_sp 	_sdgf;
    public:
        DISABLE_NEW();
	virtual string describe() const {return "SingleDispatchGenericFunctoid";};
	virtual void LISP_CALLING_CONVENTION()
	{
            IMPLEMENT_MEF(BF("Handle single dispatch"));
#if 0            
	    *lcc_resultP = this->_sdgf->INVOKE(lcc_nargs, nargs,args);
#endif
	}
    };


    class Lambda_emf : public FunctionClosure
    {
        FRIEND_GC_SCANNER();
    private:
	/*! Store the method_function that this emf invokes.
	  This function takes two arguments: (args next-emfun) */
	Function_sp		_method_function;
    public:
	string describe() const { return "Lambda_emf";};
	bool requires_activation_frame() const { return true;};
        virtual size_t templatedSizeof() const { return sizeof(*this);};
    public:
	Lambda_emf(T_sp name,
		   SingleDispatchGenericFunction_sp gf,
		   SingleDispatchMethod_sp cur_method );
        DISABLE_NEW();

        virtual void LISP_CALLING_CONVENTION()
        {
            IMPLEMENT_ME();
#if 0
            // The closedEnv 
            ASSERTF(closedEnv.nilp(),BF("Since I don't pass the closedEnv forward it I expect that it should always be nil - this time it wasn't - figure out what is up with that"));
            return this->_method_function->INVOKE(nargs,args);	    
#endif
        }
    };


    SingleDispatchGenericFunction_sp af_ensureSingleDispatchGenericFunction(Symbol_sp gfname, LambdaListHandler_sp llhandler );

};


#endif /* _SINGLEDISPATCHGENERICFUNCTION_H_ */
