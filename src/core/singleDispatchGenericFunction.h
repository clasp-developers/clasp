#ifndef _SINGLEDISPATCHGENERICFUNCTION_H_
#define _SINGLEDISPATCHGENERICFUNCTION_H_

#include "foundation.h"
#include "hashTable.fwd.h"
#include "singleDispatchGenericFunction.fwd.h"
#include "singleDispatchMethod.fwd.h"
#include "singleDispatchEffectiveMethodFunction.fwd.h"



namespace core
{






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
	void initialize();
	
    public: // instance variables here
        T_sp            name;
	/*! Store the methods here */
	Cons_sp		_Methods;
        LambdaListHandler_sp lambdaListHandler;
	/*! Store the effective method functions hashed on the receiver class */
	HashTable_sp	_classes_to_emf_table;
    private:
	void LISP_INVOKE();
    public:
	static SingleDispatchGenericFunction_sp create(T_sp functionName, LambdaListHandler_sp llhandler);
    public: // Functions here

	/*! Return the index of the required argument that is dispatched on */
//	int dispatch_on_index() const { return this->_DispatchOnIndex;};


	/*! Define a method to this SingleDispatchGenericFunction
	  If there is already a method with the same receiver then replace it
	  unless it's locked. Whenever a method is defined the method combination table
	  is wiped out */
	void defmethod(Symbol_sp name, SingleDispatchMethod_sp method);

	/*! Calculate the effective method function based on the receiver class */
	Function_sp slow_method_lookup(Class_sp mc);

	/*! Calculate and effective method function */
	Function_sp compute_effective_method_function(VectorObjects_sp applicableMethods);

	/*! Return the Cons of methods attached to this SingleDispatchGenericFunction */
	Cons_sp methods() const { return this->_Methods;};

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

};


#endif /* _SINGLEDISPATCHGENERICFUNCTION_H_ */
