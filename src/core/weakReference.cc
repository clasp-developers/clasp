#define	DEBUG_LEVEL_FULL

#include "core/common.h"
#include "core/environment.h"
#include "weakReference.h"
#include "core/wrappers.h"
namespace core
{

// ----------------------------------------------------------------------
//

    EXPOSE_CLASS(core,WeakReference_O);

    void WeakReference_O::exposeCando(Lisp_sp lisp)
    {
	class_<WeakReference_O>()
	    .def("valid",&WeakReference_O::valid)
	    .def("lock",&WeakReference_O::lock)
	;
    }

    void WeakReference_O::exposePython(Lisp_sp lisp)
    {_G();
#ifdef USEBOOSTPYTHON
	PYTHON_CLASS(CorePkg,WeakReference,"","",_lisp)
	    .def("valid",&WeakReference_O::valid)
	    .def("lock",&WeakReference_O::lock)
	;
#endif
    }




#define ARGS_WeakReference_O_make "(obj)"
#define DECL_WeakReference_O_make ""
#define DOCS_WeakReference_O_make "make WeakReference args: obj"
    WeakReference_sp WeakReference_O::make(T_sp obj)
    {_G();
        GC_ALLOCATE(WeakReference_O,me);
	    me->_WeakObject = obj;
	return me;
    };



#if defined(OLD_SERIALIZE)
    void WeakReference_O::serialize(serialize::SNode snode)
    {
	CR_HINT(snode,false);
	snode->archiveWeakPointer("weakObject",this->_WeakObject);
	CR_HINT(snode,false);
    }
#endif // defined(OLD_SERIALIZE)

#if defined(XML_ARCHIVE)
    void WeakReference_O::archiveBase(ArchiveP node)
    {
        this->Base::archiveBase(node);
	node->archiveWeakPointer("weakObject",this->_WeakObject);
    }
#endif // defined(XML_ARCHIVE)


    void WeakReference_O::initialize()
    {_OF();
        this->Base::initialize();
	this->_WeakObject.reset();
    }

    bool WeakReference_O::valid() const
    {_OF();
	if ( !this->_WeakObject ) return false;
	return true;
    }

    T_sp WeakReference_O::lock() const
    {_OF();
	if ( !this->_WeakObject ) return _Nil<T_O>();
	return this->_WeakObject.lock();
    }


    

}; /* core */
