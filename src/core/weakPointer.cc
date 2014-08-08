#define	DEBUG_LEVEL_FULL

#include "core/common.h"
#include "core/environment.h"
#include "weakReference.h"
#include "core/wrappers.h"
namespace core
{

// ----------------------------------------------------------------------
//

#define ARGS_WeakPointer_O_make "(obj)"
#define DECL_WeakPointer_O_make ""
#define DOCS_WeakPointer_O_make "make WeakPointer args: obj"
    WeakPointer_sp WeakPointer_O::make(T_sp obj)
    {_G();
        GC_ALLOCATE_VARIADIC(WeakPointer_O,me,obj);
	return me;
    };


    EXPOSE_CLASS(core,WeakPointer_O);

    void WeakPointer_O::exposeCando(Lisp_sp lisp)
    {
	class_<WeakPointer_O>()
	    .def("weakPointerValid",&WeakPointer_O::valid)
	    .def("weakPointerValue",&WeakPointer_O::value)
	;
	Defun_maker(CorePkg,WeakPointer);
       
    }

    void WeakPointer_O::exposePython(Lisp_sp lisp)
    {_G();
#ifdef USEBOOSTPYTHON
	PYTHON_CLASS(CorePkg,WeakPointer,"","",_lisp)
	    .def("weakPointerValid",&WeakPointer_O::valid)
	    .def("weakPointerValue",&WeakPointer_O::value)
	;
#endif
    }






#if defined(OLD_SERIALIZE)
    void WeakPointer_O::serialize(serialize::SNode snode)
    {
	CR_HINT(snode,false);
	snode->archiveWeakPointer("weakObject",this->_WeakObject);
	CR_HINT(snode,false);
    }
#endif // defined(OLD_SERIALIZE)

#if defined(XML_ARCHIVE)
    void WeakPointer_O::archiveBase(ArchiveP node)
    {
        this->Base::archiveBase(node);
	node->archiveWeakPointer("weakObject",this->_WeakObject);
    }
#endif // defined(XML_ARCHIVE)


    bool WeakPointer_O::valid() const
    {_OF();
        return this->_WeakObject.valid();
    }

    /*! Return (values value t) or (values nil nil) */
    T_mv WeakPointer_O::value() const
    {_OF();
        if (!this->valid()) {
            return Values(_Nil<T_O>(),_Nil<T_O>());
        }
        return Values(gctools::smart_ptr<T_O>(this->_WeakObject.value().backcast()),_lisp->_true());
    }


    

}; /* core */
