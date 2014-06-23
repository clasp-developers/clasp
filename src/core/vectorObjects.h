#ifndef	_core_VectorObjects_H
#define _core_VectorObjects_H

#include "core/object.h"
#include "core/lispVector.h"
#include "corePackage.fwd.h"



template<> struct gctools::GCInfo<core::VectorObjects_O> {
    static bool constexpr NeedsInitialization = false;
    static bool constexpr NeedsFinalization = false;
    static bool constexpr Moveable = true;
    static bool constexpr Atomic = false;
};


extern "C"
{
    extern void sp_copyLoadTimeValue(core::T_sp* resultP, core::LoadTimeValues_O** ltvPP, int index);
};

namespace core
{

    FORWARD(VectorObjects);
    class VectorObjects_O : public Vector_O
    {
        friend void (::sp_copyLoadTimeValue(T_sp* resultP, LoadTimeValues_O** ltvPP, int index));
	LISP_BASE1(Vector_O);
	LISP_CLASS(core,CorePkg,VectorObjects_O,"VectorObjects");
	DECLARE_INIT();

	void archiveBase(SNode_sp node);
    public:
	VectorObjects_O();
	virtual ~VectorObjects_O() {};
    public:
        typedef gctools::Vec0<T_sp>        vector_type;
        typedef gctools::Vec0<T_sp>::iterator iterator;

    protected: // instance variables here -- REMEMBER to update swap(...) if you add/remove variables
	T_sp 		_ElementType;
	bool		_Adjustable;
        vector_type      _Values;
    public:
	void fillInitialContents(Sequence_sp ic);
    public:
        iterator begin() { return this->_Values.begin();};
        iterator end() { return this->_Values.begin()+this->length();};
    public:
	static VectorObjects_sp create(T_sp initial_element, int dimension, T_sp elementType);
	static VectorObjects_sp make(T_sp initial_element, Sequence_sp initialContents, int dimension, bool adjustable );


    public:
	void setup(T_sp initial_element, Sequence_sp initialContents, int dimension, bool adjustable);
	void adjust(T_sp initial_element, Sequence_sp initialContents, int dimension );

	void setElementType(T_sp elementType) { this->_ElementType = elementType;};
	T_sp elementType() const { return this->_ElementType;};
    public: // Functions here
	bool adjustableArrayP() const { return this->_Adjustable; };

	uint dimension() const { return this->_Values.size(); };
	virtual void rowMajorAset( int idx, T_sp value);
	virtual T_sp rowMajorAref(int idx) const;
	virtual int arrayRowMajorIndex(Cons_sp indices) const;

	T_sp& operator[](uint index) {return this->_Values[index];}

	virtual void swapElements(uint i1, uint i2)
	{
	    T_sp t = this->_Values[i2];
	    this->_Values[i2] = this->_Values[i1];
	    this->_Values[i1] = t;
	}

	/*! Swap the contents of the VectorObjects */
	void swap(VectorObjects_sp vec);
	
	virtual T_sp aref(Cons_sp indices) const;
	virtual T_sp setf_aref(Cons_sp indices_val);
	
	virtual T_sp elt(int index) const;
	virtual T_sp setf_elt(int index, T_sp value);

	virtual T_sp svref(int index) const { return this->elt(index); };
	virtual T_sp setf_svref(int index, T_sp value) { return this->setf_elt(index,value);};

	virtual void fillArrayWithElt(T_sp element, Fixnum_sp start, T_sp end);
	uint length() const { return this->_Values.size();};
	string __repr__() const;

	virtual Sequence_sp subseq(int start, T_sp end) const;
	virtual Sequence_sp setf_subseq(int start, T_sp end, Sequence_sp new_subseq) {_G(); IMPLEMENT_ME();};


    };

}; /* core */

TRANSLATE(core::VectorObjects_O);

#endif /* _core_VectorObjects_H */


