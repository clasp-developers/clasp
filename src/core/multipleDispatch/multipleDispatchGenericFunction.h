#ifndef _MULTIPLEDISPATCHGENERICFUNCTION_H_
#define _MULTIPLEDISPATCHGENERICFUNCTION_H_

#include "foundation.h"
#include "hashTable.fwd.h"
#include "multipleDispatchGenericFunction.fwd.h"
#include "multipleDispatchMethod.fwd.h"
#include "multipleDispatchEffectiveMethodFunction.fwd.h"



namespace core
{



    class MultipleDispatchGenericFunction_O : public Function_O
    {
	LISP_BASE1(Function_O);
	LISP_CLASS(CorePkg,multipleDispatchGenericFunction_O,"multiple-dispatch-generic-function");
	DECLARE_INIT();
//    DECLARE_ARCHIVE();
    public: // Simple default ctor/dtor
	DEFAULT_CTOR_DTOR(MultipleDispatchGenericFunction_O);
    public: // ctor/dtor for classes with shared virtual base
//    explicit MultipleDispatchGenericFunction_O(core::MetaClass_sp const& mc) : T_O(mc), Function(mc) {};
//    virtual ~MultipleDispatchGenericFunction_O() {};
    public:
	void initialize();
	
    private: // instance variables here
	/*! The index of the argument on which we dispatch */
	int		_DispatchOnIndex;
	/*! Store the methods here */
	Cons_sp		_Methods;

	/*! Store the effective method functions hashed on the receiver class */
	HashTable_sp	_classes_to_emf_table;

    private:
	/*! Invoke the function which looks up the method combination for the
	  class of the first argument or it creates one */
	T_sp INVOKE(Cons_sp args);
    public:
	static MultipleDispatchGenericFunction_sp create(Symbol_sp name, int dispatch_on_required_argument_index, Lisp_sp lisp);
    public: // Functions here

	/*! Define a method to this MultipleDispatchGenericFunction
	  If there is already a method with the same receiver then replace it
	  unless it's locked. Whenever a method is defined the method combination table
	  is wiped out */
	void defmethod(Symbol_sp name, MultipleDispatchMethod_sp method);

	/*! Calculate the effective method function based on the receiver class */
	Function_sp slow_method_lookup(MetaClass_sp mc);

	/*! Calculate and effective method function */
	Function_sp compute_effective_method_function(Cons_sp methods);
    }; // MultipleDispatchGenericFunction class
    
}; // core namespace
TRANSLATE(core::MultipleDispatchGenericFunction_O);





#endif /* _MULTIPLEDISPATCHGENERICFUNCTION_H_ */
