#ifndef	_core_VectorWithFillPtr_template_H
#define _core_VectorWithFillPtr_template_H
#include "core/foundation.h"
#include "core/object.h"
#include "core/vectorTemplate.h"
#include "corePackage.fwd.h"

namespace core
{
    template <class T> class VectorWithFillPtr_template_O : public Vector_template_O<T>
    {
	LISP_TEMPLATE_BASE1(Vector_template_O<T>)
	LISP_TEMPLATE_CLASS(VectorWithFillPtr_template_O);
    public:
	VectorWithFillPtr_template_O() : T_O(), Vector_template_O<T>() {};
	virtual ~VectorWithFillPtr_template_O() {};
    public:
	typedef T valueType;
	typedef mem::smart_ptr<T> sharedValueType;
	typedef VectorWithFillPtr_template_O<T> vectorType;
	typedef mem::smart_ptr<vectorType> sharedVectorType;
    private: // instance variables here
	int 		_FillPtr;
    public:
	void setup(sharedValueType initialElement, Cons_sp initialContents, int dimension, int fillPtr)
	{_G();
	    sharedVectorType vo = RP_Create<vectorType>(_lisp);
	    if (fillPtr < 0 ) fillPtr = 0;
	    if ( fillPtr > dimension ) fillPtr = dimension;
	    vo->_FillPtr = fillPtr;
	    vo->Base::setup(initialElement,initialContents,dimension);
	}

    public: // Functions here

	uint length() const { return this->_FillPtr;};

	virtual bool arrayHasFillPointerP() const { return true;};

	virtual T_sp elt(int index) const
	{_G();
	    if ( index >= this->_FillPtr )
	    {
//	    ERROR(make_condition(_sym_indexTooLargeError) << _kw_datum << index << _kw_expectedType << this->_FillPtr );
		THROW(_lisp->error(BF("Index %d is too large - must be less than %d") % index % this->_FillPtr ));
	    }
	    return this->Base::elt(index);
	}

	virtual T_sp setf_elt(int index, T_sp value)
	{_G();
	    if ( index >= this->_FillPtr )
	    {
//	    ERROR(make_condition(_sym_indexTooLargeError) << _kw_datum << index << _kw_expectedType << this->_FillPtr );
		THROW(_lisp->error(BF("Index %d is too large - must be less than %d") % index % this->_FillPtr ));
	    }
	    return this->Base::setf_elt(index,value);
	}

	string __repr__() const
	{_G();
	    stringstream ss;
	    ss << "#" << this->_FillPtr << "( ";
	    for ( int i=0; i<this->_FillPtr; i++ )
	    {
		ss << this->elt(i)->__repr__() << " ";
	    }
	    ss << ")";
	    return ss.str();
	}

	int fillPointer() const { return this->_FillPtr;};
	void setf_fillPointer(int fp)
	{_G();
	    if ( fp >= this->_Values.size() ) fp = this->_Values.size();
	    this->_FillPtr = fp;
	}


	int vectorPush(T_sp newElement)
	{_G();	
	    ASSERTF(this->_FillPtr<this->_Values.size(),BF("fill-pointer %d has reached the end %d of the vector") % this->_FillPtr % this->_Values.size() );
	    int idx = this->_FillPtr;
	    this->_Values[idx] = newElement->as<valueType>();
	    this->_FillPtr++;
	    return idx;
	};


	int vectorPushExtend(T_sp newElement, int extension)
	{_G();
	    if ( this->_FillPtr >= this->length() )
	    {
		if ( extension <= 0 ) extension = 1;
		this->_Values.resize(this->_Values.size() + extension, valueType::_nil);
	    }
	    int idx = this->_FillPtr;
	    this->_Values[idx] = newElement->as<valueType>();
	    this->_FillPtr++;
	    return idx;
	} ;


    };

}; /* core */


#endif /* _core_VectorWithFillPtr_template_H */


