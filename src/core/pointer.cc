#include "common.h"
#include "pointer.h"
#include "wrappers.h"

namespace core
{

    void Pointer_O::initialize()
    {
	this->Base::initialize();
	this->_Pointer = NULL;
    }


    Pointer_sp Pointer_O::create(void* p)
    {_G();
	GC_RESERVE_BEGIN(Pointer_O,ptr ){
	    GC_RESERVE_GET(Pointer_O,ptr );
	} GC_RESERVE_END(Pointer_O,ptr );
	    ptr->_Pointer = p;
	return ptr;
    }


    EXPOSE_CLASS(core,Pointer_O);

    void Pointer_O::exposeCando(core::Lisp_sp lisp)
    {
	core::class_<Pointer_O>()
	    ;
    }

    void Pointer_O::exposePython(core::Lisp_sp lisp)
    {_G();
#ifdef USEBOOSTPYTHON
	PYTHON_CLASS(CorePkg,Pointer,"","",_lisp)
	    ;
#endif
    }

    bool Pointer_O::eql(T_sp obj) const
    {
        if ( this->eq(obj) ) return true;
        if ( Pointer_sp pobj = obj.asOrNull<Pointer_O>() ) {
            return (this->_Pointer == pobj->_Pointer);
        }
        return false;
    }

    string Pointer_O::__repr__() const
    {
	stringstream ss;
	ss << "#<" << this->_instanceClass()->classNameAsString() << " :ptr " << (BF("%p") % this->_Pointer).str() << ">";
	return ss.str();
    }

};

