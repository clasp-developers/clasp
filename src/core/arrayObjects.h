#ifndef	_core_ArrayObjects_H
#define _core_ArrayObjects_H

#include "core/foundation.h"
#include "core/array.h"
#include "corePackage.fwd.h"

namespace core
{

FORWARD(ArrayObjects);
class ArrayObjects_O : public Array_O
{
    LISP_BASE1(Array_O);
    LISP_CLASS(core,CorePkg,ArrayObjects_O,"ArrayObjects");
#if defined(XML_ARCHIVE)
    DECLARE_ARCHIVE();
#endif // defined(XML_ARCHIVE)
#if defined(OLD_SERIALIZE)
    DECLARE_SERIALIZE();
#endif // defined(OLD_SERIALIZE)
public:
    explicit ArrayObjects_O() : T_O(), Base() {};
    virtual ~ArrayObjects_O() {};
public:
	void initialize();

private: // instance variables here
    vector<int>			_Dimensions;
    T_sp			_ElementType;
    gctools::Vec0<T_sp>	_Values;

public: // Functions here
    static ArrayObjects_sp make(T_sp dim, T_sp elementType, T_sp initialElement );

public:
    virtual T_sp asetUnsafe(int j, T_sp val);
    T_sp elementType() const { return this->_ElementType;};

	virtual void rowMajorAset( int idx, T_sp value);
	virtual T_sp rowMajorAref(int idx) const;

    virtual int rank() const { return this->_Dimensions.size();};

    virtual int arrayDimension(int axisNumber) const;

	LongLongInt setDimensions(Cons_sp dims,T_sp initialElement);

    void setElementType(T_sp et) { this->_ElementType = et;};
	/*! Return the value at the indices */
    virtual T_sp aref(Cons_sp indices) const;

	/*! Return the value at the indices */
	virtual T_sp setf_aref(Cons_sp indices_val);


	/*! Return a shallow copy of this object */
	virtual T_sp shallowCopy() const;

	/*! Return the value at the indices */
	virtual void arrayFill(T_sp val);


	/*! Return a deepCopy of the ArrayObjects */
	virtual T_sp deepCopy() const;



    virtual T_sp svref(int index) const;
    virtual T_sp setf_svref(int index, T_sp value);





};

}; /* core */

TRANSLATE(core::ArrayObjects_O);

#endif /* _core_ArrayObjects_H */


