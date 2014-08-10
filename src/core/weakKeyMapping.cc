#define	DEBUG_LEVEL_FULL

#include "core/common.h"
#include "core/environment.h"
#include "weakKeyMapping.h"
#include "core/wrappers.h"
namespace core
{

// ----------------------------------------------------------------------
//

#define ARGS_WeakKeyMapping_O_make "(key val)"
#define DECL_WeakKeyMapping_O_make ""
#define DOCS_WeakKeyMapping_O_make "make WeakKeyMapping args: obj"
    WeakKeyMapping_sp WeakKeyMapping_O::make(T_sp key, T_sp tval)
    {_G();
        T_sp val(tval);
        if ( key == tval ) {
            val = gctools::smart_ptr<T_O>(gctools::tagged_ptr<T_O>::tagged_sameAsKey);
        }
        GC_ALLOCATE_VARIADIC(WeakKeyMapping_O,me,key,val);
	return me;
    };


    EXPOSE_CLASS(core,WeakKeyMapping_O);

    void WeakKeyMapping_O::exposeCando(Lisp_sp lisp)
    {
	class_<WeakKeyMapping_O>()
	    .def("weakKeyMappingValid",&WeakKeyMapping_O::valid)
	    .def("weakKeyMappingKeyValue",&WeakKeyMapping_O::keyValue)
	;
	Defun_maker(CorePkg,WeakKeyMapping);
       
    }

    void WeakKeyMapping_O::exposePython(Lisp_sp lisp)
    {_G();
#ifdef USEBOOSTPYTHON
	PYTHON_CLASS(CorePkg,WeakKeyMapping,"","",_lisp)
	    .def("weakKeyMappingValid",&WeakKeyMapping_O::valid)
	    .def("weakKeyMappingValue",&WeakKeyMapping_O::value)
	;
#endif
    }






#if defined(OLD_SERIALIZE)
    void WeakKeyMapping_O::serialize(serialize::SNode snode)
    {
	CR_HINT(snode,false);
	snode->archiveWeakKeyMapping("weakObject",this->_WeakObject);
	CR_HINT(snode,false);
    }
#endif // defined(OLD_SERIALIZE)

#if defined(XML_ARCHIVE)
    void WeakKeyMapping_O::archiveBase(ArchiveP node)
    {
        this->Base::archiveBase(node);
	node->archiveWeakKeyMapping("weakObject",this->_WeakObject);
    }
#endif // defined(XML_ARCHIVE)


    bool WeakKeyMapping_O::valid() const
    {_OF();
        return !this->_WeakObject.Key->bucket.NULLp() && !this->_WeakObject.Key->bucket.unboundp();
    }

    /*! Return (values key value t) or (values nil nil nil) */
    T_mv WeakKeyMapping_O::keyValue() const
    {_OF();
        if (!this->valid()) {
            return Values(_Nil<T_O>(),_Nil<T_O>(),_Nil<T_O>());
        }
        value_type& key_ref = this->_WeakObject.Key->bucket;
        value_type& value_ref = this->_WeakObject.Value->bucket;
        T_sp key = key_ref.backcast();
        T_sp value;
        if ( value_ref.sameAsKeyP() ) {
            value = key;
        } else { 
            value = value_ref.backcast();
        }
        return Values(key,value,_lisp->_true());
    }

    

}; /* core */
