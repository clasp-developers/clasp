#include "wrappedPointer.h"
#include "wrappers.h"

namespace core {



    
    
#define ARGS_af_pointerRelease "(arg)"
#define DECL_af_pointerRelease ""
#define DOCS_af_pointerRelease "pointerRelease"
    Pointer_sp af_pointerRelease(T_sp ptr)
    {_G();
        if ( ptr.nilp() ) {return _Nil<Pointer_O>();};
        if ( WrappedPointer_sp wp = ptr.asOrNull<WrappedPointer_O>() ) {
            return Pointer_O::create(wp->pointerRelease());
        }
        SIMPLE_ERROR(BF("Could not release pointer for %s") % _rep_(ptr));
    }


#define ARGS_af_pointerDelete "(arg)"
#define DECL_af_pointerDelete ""
#define DOCS_af_pointerDelete "pointerDelete"
    void af_pointerDelete(T_sp ptr)
    {_G();
        if ( ptr.nilp() ) {return;};
        if ( WrappedPointer_sp wp = ptr.asOrNull<WrappedPointer_O>() ) {
            wp->pointerDelete();
            return;
        }
        SIMPLE_ERROR(BF("Could not release pointer for %s") % _rep_(ptr));
    }

    EXPOSE_CLASS(core,WrappedPointer_O);


    T_sp WrappedPointer_O::instanceClassSet(Class_sp cl)
    {
        this->_Class = cl;
        return this->asSmartPtr();
    }

    void WrappedPointer_O::setInstanceClassUsingSymbol(Symbol_sp classSymbol)
    {
        Class_sp cl = af_findClass(classSymbol).as<Class_O>();
        this->instanceClassSet(cl);
    }


    bool WrappedPointer_O::eq(T_sp obj) const
    {_G();
	if ( WrappedPointer_sp wo = obj.as<WrappedPointer_O>() )
	{
	    return (obj.as<WrappedPointer_O>()->mostDerivedPointer() == this->mostDerivedPointer() );
	}
	return false;
    }

    Pointer_sp WrappedPointer_O::address() const
    {_G();
        void* addr = this->mostDerivedPointer();
        return Pointer_O::create(addr);
    }




    
    
#define ARGS_af_pointerAddress "(arg)"
#define DECL_af_pointerAddress ""
#define DOCS_af_pointerAddress "pointerAddress"
    T_sp af_pointerAddress(T_sp ptr)
    {_G();
        if ( ptr.nilp() ) {return _Nil<Pointer_O>();};
        if ( WrappedPointer_sp wp = ptr.asOrNull<WrappedPointer_O>() ) {
            return wp->address();
        }
        SIMPLE_ERROR(BF("Could not get address of pointer for %s") % _rep_(ptr));
    };

    void WrappedPointer_O::exposeCando(core::Lisp_sp e)
    {
        class_<WrappedPointer_O>()
            .def("validp",&WrappedPointer_O::validp)
            ;
        Defun(pointerRelease);
        Defun(pointerDelete);
        Defun(pointerAddress);
    }

    void WrappedPointer_O::exposePython(core::Lisp_sp lisp)
    {_G();
#ifdef	USEBOOSTPYTHON //[
	PYTHON_CLASS(CorePkg,WrappedPointer,"","",_lisp)
            ;
#endif //]
    }




};
