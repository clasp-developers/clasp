#ifndef	_core_VectorObjectsWithFillPtr_H
#define _core_VectorObjectsWithFillPtr_H
#include "core/foundation.h"
#include "core/object.h"
#include "core/vectorObjects.h"
#include "corePackage.fwd.h"




namespace core
{
    FORWARD(VectorObjectsWithFillPtr);
    class VectorObjectsWithFillPtr_O : public VectorObjects_O
    {
	LISP_BASE1(VectorObjects_O);
	LISP_CLASS(core,CorePkg,VectorObjectsWithFillPtr_O,"VectorObjectsWithFillPtr");
	void archiveBase(SNode_sp node);
    public:
	VectorObjectsWithFillPtr_O();
	virtual ~VectorObjectsWithFillPtr_O() {};
    private: // instance variables here
	int 		_FillPtr;
    public:
	static VectorObjectsWithFillPtr_sp make(T_sp initial_element, T_sp initial_values, int dimension, int fillPtr, bool adjustable );

    public: // Functions here

	uint length() const { return this->_FillPtr;};

	virtual bool arrayHasFillPointerP() const { return true;};
	virtual T_sp& operator[](uint index);

	virtual T_sp elt(int index) const;
	virtual T_sp setf_elt(int index, T_sp value);

	string __repr__() const;

	int fillPointer() const { return this->_FillPtr;};
	void setf_fillPointer(int fp);

	Fixnum_sp vectorPush(T_sp newElement);
	Fixnum_sp vectorPushExtend(T_sp newElement, int extension=16);

    };

}; /* core */

TRANSLATE(core::VectorObjectsWithFillPtr_O);


template<> struct gctools::GCInfo<core::VectorObjectsWithFillPtr_O> {
    static bool constexpr NeedsInitialization = false;
    static bool constexpr NeedsFinalization = false;
    static bool constexpr Moveable = true;
    static bool constexpr Atomic = false;
};

#endif /* _core_VectorObjectsWithFillPtr_H */


