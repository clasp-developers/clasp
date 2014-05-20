#ifndef	_core_Vector_H
#define _core_Vector_H

#include "core/object.h"
#include "core/array.h"
#include "sequence.h"
#include "corePackage.fwd.h"

namespace core
{

FORWARD(Vector);



/*! A one dimensional vector of objects */
class Vector_O : public Array_O, public Sequence_O
{
    LISP_VIRTUAL_BASE2(T_O,Array_O,Sequence_O);
    LISP_CLASS(core,ClPkg,Vector_O,"vector");


public:
    void archiveBase(core::ArchiveP node);
public:
    explicit Vector_O(): T_O(), Array_O(), Sequence_O() {} ;
    virtual ~Vector_O() {};
public:
    void initialize();
private: // instance variables here

public: // Functions here

    bool adjustableArrayP() const { return false;};
    uint vector_length() const { return this->dimension();};
    virtual uint dimension() const { SUBIMP(); };
    uint length() const { return this->dimension(); };

    Sequence_sp reverse();
    Sequence_sp nreverse();

    virtual T_sp& operator[](uint index) {SUBIMP();}

    virtual void swapElements(uint idx1, uint idx2) {SUBIMP();};

    virtual size_t elementSizeInBytes() const {SUBIMP();}
    virtual T_sp elementType() const {SUBIMP();}
    virtual int rank() const { return 1;};
    virtual int arrayDimension(int axisNumber) const;
    virtual Cons_sp arrayDimensions() const;
    virtual int arrayTotalSize() const { return this->length();};

    virtual Fixnum_sp vectorPush(T_sp newElement) {SUBIMP();};
    virtual Fixnum_sp vectorPushExtend(T_sp newElement, int extension=1) {SUBIMP();};


    virtual void setFillPointer(size_t idx) {SUBIMP();};

    virtual void* addressOfBuffer() const {SUBIMP();};

}; /* core */
};
TRANSLATE(core::Vector_O);



namespace core {
    // Like ecl_vector_start_end
    T_mv brcl_vectorStartEnd(Symbol_sp fn, T_sp thing, Fixnum_sp start, Fixnum_sp end); 

    Vector_sp af_make_vector(Symbol_sp element_type,
			int dimension,
			bool adjustable,
			T_sp fill_pointer=Fixnum_O::create(0),
			T_sp displaced_to=_Nil<T_O>(),
			T_sp displaced_index_offset=_Nil<T_O>(),
			T_sp initial_element=_Nil<T_O>(),
			Sequence_sp initial_contents=_Nil<Sequence_O>() );

};
#endif /* _core_Vector_H */


