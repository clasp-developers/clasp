#ifndef _core_instance_H_
#define _core_instance_H_

#include "core/foundation.h"
#include "core/object.h"
#include "core/instance.fwd.h"



#define GCINFO_KIND_GCARRAY_gctools__GCArray_moveable_class_mem__smart_ptr_class_core__T_O__0_
#define GCINFO_KIND_GCARRAY_gctools__GCArray_moveable_class_mem__smart_ptr_class_core__T_O__1_
#define GCINFO_KIND_GCARRAY_gctools__GCArray_moveable_class_mem__smart_ptr_class_core__T_O__2_
#define GCINFO_KIND_GCARRAY_gctools__GCArray_moveable_class_mem__smart_ptr_class_core__T_O__3_
#define GCINFO_KIND_GCARRAY_gctools__GCArray_moveable_class_mem__smart_ptr_class_core__T_O__4_
#define GCINFO_KIND_GCARRAY_gctools__GCArray_moveable_class_mem__smart_ptr_class_core__T_O__5_
#define GCINFO_KIND_GCARRAY_gctools__GCArray_moveable_class_mem__smart_ptr_class_core__T_O__6_
#define GCINFO_KIND_GCARRAY_gctools__GCArray_moveable_class_mem__smart_ptr_class_core__T_O__7_
#define GCINFO_KIND_GCARRAY_gctools__GCArray_moveable_class_mem__smart_ptr_class_core__T_O__8_
#define GCINFO_KIND_GCARRAY_gctools__GCArray_moveable_class_mem__smart_ptr_class_core__T_O__9_
#define GCINFO_KIND_GCARRAY_gctools__GCArray_moveable_class_mem__smart_ptr_class_core__T_O__10_
#define GCINFO_KIND_GCARRAY_gctools__GCArray_moveable_class_mem__smart_ptr_class_core__T_O__11_
// may need more later
#include GC_INTERFACE_HEADER
#undef GCINFO_KIND_GCARRAY_gctools__GCArray_moveable_class_mem__smart_ptr_class_core__T_O__0_
#undef GCINFO_KIND_GCARRAY_gctools__GCArray_moveable_class_mem__smart_ptr_class_core__T_O__1_
#undef GCINFO_KIND_GCARRAY_gctools__GCArray_moveable_class_mem__smart_ptr_class_core__T_O__2_
#undef GCINFO_KIND_GCARRAY_gctools__GCArray_moveable_class_mem__smart_ptr_class_core__T_O__3_
#undef GCINFO_KIND_GCARRAY_gctools__GCArray_moveable_class_mem__smart_ptr_class_core__T_O__4_
#undef GCINFO_KIND_GCARRAY_gctools__GCArray_moveable_class_mem__smart_ptr_class_core__T_O__5_
#undef GCINFO_KIND_GCARRAY_gctools__GCArray_moveable_class_mem__smart_ptr_class_core__T_O__6_
#undef GCINFO_KIND_GCARRAY_gctools__GCArray_moveable_class_mem__smart_ptr_class_core__T_O__7_
#undef GCINFO_KIND_GCARRAY_gctools__GCArray_moveable_class_mem__smart_ptr_class_core__T_O__8_
#undef GCINFO_KIND_GCARRAY_gctools__GCArray_moveable_class_mem__smart_ptr_class_core__T_O__9_
#undef GCINFO_KIND_GCARRAY_gctools__GCArray_moveable_class_mem__smart_ptr_class_core__T_O__10_
#undef GCINFO_KIND_GCARRAY_gctools__GCArray_moveable_class_mem__smart_ptr_class_core__T_O__11_


/*! Different values for Instance_O.isgf */
#define ECL_NOT_FUNCALLABLE	0
#define ECL_STANDARD_DISPATCH	1
#define ECL_RESTRICTED_DISPATCH	2
#define ECL_READER_DISPATCH	3
#define ECL_WRITER_DISPATCH	4
#define ECL_USER_DISPATCH	5



namespace core
{



    class Instance_O : public Function_O
    {
	LISP_BASE1(Function_O);
	LISP_CLASS(core,CorePkg,Instance_O,"Instance");
	friend class Class_O;
	void archiveBase(ArchiveP node);
    public: // ctor/dtor for classes with shared virtual base
	explicit Instance_O();
	virtual ~Instance_O();
    protected: // instance variables here
	int					_isgf;
	Class_sp 				_Class;
        gctools::Vec0<T_sp>			_Slots;
	ArgArrayGenericFunctionPtr 	        _Entry;
	/*! TODO: I don't know what this is for
	  - mimicking ECL instance->sig generation signature */
	T_sp 		_Sig;
    public:
#if 0
    public: // Functions that mimic ECL_XXXX_XXXX macros (eg: ECL_CLASS_SLOTS(x))
	// Instances that represent CL classes have slots that are hard-coded to represent
	// class instance variables
	const Class_sp& _CLASS_OF() const	{ return this->_Class;};
	T_sp& _SPECIALIZER_FLAG()  	{ return this->_Slots[0];};
	T_sp& _SPECIALIZER_OBJECT() 	{ return this->_Slots[3];};
	T_sp& _CLASS_NAME()   		{ return this->_Slots[3+0];}; // 3
	T_sp& _CLASS_SUPERIORS() 	{ return this->_Slots[3+1];}; // 4
	T_sp& _CLASS_INFERIORS() 	{ return this->_Slots[3+2];}; // 5 
	T_sp& _CLASS_SLOTS() 	{ return this->_Slots[3+3];}; // 6
	T_sp& _CLASS_CPL() 	{ return this->_Slots[3+4];};

#endif
    public: // Generic function ECL macros are replicated here
	T_sp GFUN_NAME() const { return this->_Slots[0]; };
	T_sp GFUN_SPECIALIZERS() const { return this->_Slots[1]; };
	T_sp GFUN_COMB() const { return this->_Slots[2]; };
    public: // The hard-coded indexes above are defined below to be used by Class
#if 0
	// These must match the +class-slots+ defined in hierarchy.lsp
	static const int REF_EQL_SPECIALIZER_FLAG 	= 0;
	static const int REF_SPECIALIZER_DIRECT_METHODS = 1;
	static const int REF_SPECIALIZER_DIRECT_GENERIC_FUNCTIONS 	= 2;
	static const int REF_CLASS_NAME 		= 3;
	static const int REF_DIRECT_SUPERCLASSES	= 4;
	static const int REF_DIRECT_SUBCLASSES		= 5;
	static const int REF_SLOTS      		= 6;
	static const int REF_CLASS_PRECEDENCE_LIST	= 7;
	static const int REF_DIRECT_SLOTS 		= 8;
	static const int REF_DIRECT_DEFAULT_INITARGS 	= 9;
	static const int REF_DEFAULT_INITARGS 		= 10;
	static const int REF_FINALIZED 			= 11;
	static const int REF_DOCSTRING 			= 12;
	static const int REF_SIZE 			= 13;
	static const int REF_SEALEDP 			= 14;
	static const int REF_PROTOTYPE 			= 15;
	static const int REF_DEPENDENTS 		= 16;
	static const int REF_VALID_INITARGS		= 17;
	static const int REF_SLOT_TABLE 		= 18;
	static const int REF_LOCATION_TABLE 		= 19;
	static const int REF_OPTIMIZE_SLOT_ACCESS       = 20;
	static const int REF_FORWARD		        = 21;
	static const int REF_NUMBER_OF_SLOTS_IN_CLASSES = 22;
#endif
    protected:
	void initializeSlots(int numberOfSlots);
    public:
	static T_sp allocateInstance(T_sp _theClass, int numberOfSlots=0);
	static T_sp allocateRawInstance(T_sp orig, T_sp _theClass, int numberOfSlots);

    private:
	void reshapeInstance(int delta);

    public:
	virtual T_mv INVOKE(int nargs, ArgArray argArray );
    public: // Functions here
	int numberOfSlots() const { return this->_Slots.size();};
	/*! Return number of slots if not nil otherwise nil */
	T_sp oinstancepSTAR() const;
	/*! Return number of slots if not nil otherwise nil */
	T_sp oinstancep() const;

	int isgf() const { return this->_isgf;};

	Class_sp _instanceClass() const { return this->_Class;};

	T_sp instanceClassSet(Class_sp mc);

	virtual T_sp instanceSigSet();
	virtual T_sp instanceSig() const;


        virtual bool equalp(T_sp obj) const;


	/*! Return the value of a slot */
	T_sp instanceRef(int idx) const; // { return this->_Slots[idx];};
	/*! Set the value of a slot and return the new value */
	T_sp instanceSet(int idx,T_sp val); // { this->_Slots[idx] = val; return val;};

	string __repr__() const;

	T_sp copyInstance() const;

	T_sp setFuncallableInstanceFunction(T_sp functionOrT);

	bool genericFunctionP() const;


        void describe();

	void __write__(Stream_sp sout) const; // Look in write_ugly.cc


    }; // Instance class
    
}; // core namespace
TRANSLATE(core::Instance_O);






#endif /* _core_instance_H_ */
