#ifndef _SINGLEDISPATCHGENERICFUNCTION_H_
#define _SINGLEDISPATCHGENERICFUNCTION_H_

#include "foundation.h"
#include "hashTable.fwd.h"
#include "singleDispatchGenericFunction.fwd.h"
#include "singleDispatchMethod.fwd.h"
#include "singleDispatchEffectiveMethodFunction.fwd.h"



namespace core
{







    class SingleDispatchGenericFunction_O : public BuiltIn_O
    {
	LISP_BASE1(BuiltIn_O);
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
	
    private: // instance variables here
	/*! Store the methods here */
	Cons_sp		_Methods;

	/*! Store the effective method functions hashed on the receiver class */
	HashTable_sp	_classes_to_emf_table;
    private:
	T_mv INVOKE(int nargs, ArgArray args);
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
	virtual T_mv activate( ActivationFrame_sp closedEnv,int nargs, ArgArray args )
	{
	    return this->_sdgf->INVOKE(nargs,args);
	}
    };


    class Lambda_emf : public Functoid
    {
    private:
	/*! Store the name of the function that this Lambda_emf invokes - for debugging */
	Symbol_sp		_name;
	/*! Store the method_function that this emf invokes.
	  This function takes two arguments: (args next-emfun) */
	Function_sp		_method_function;
    public:
	string describe() const { return "Lambda_emf";};
	bool requires_activation_frame() const { return true;};
    public:
	Lambda_emf(const string& name,
		   SingleDispatchGenericFunction_sp gf,
		   Symbol_sp emf_name,
		   SingleDispatchMethod_sp cur_method );
        DISABLE_NEW();

        virtual T_mv activate( ActivationFrame_sp closedEnv,int nargs, ArgArray args )
        {
            // The closedEnv 
            ASSERTF(closedEnv.nilp(),BF("Since I don't pass the closedEnv forward it I expect that it should always be nil - this time it wasn't - figure out what is up with that"));
            return this->_method_function->INVOKE(nargs,args);	    
        }
    };

};


#endif /* _SINGLEDISPATCHGENERICFUNCTION_H_ */
