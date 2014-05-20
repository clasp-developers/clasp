#define	DEBUG_LEVEL_FULL

#include "core/common.h"
#include "core/environment.h"
#include "forwardReferencedClass.h"
#include "builtInClass.h"
#include "core/wrappers.h"
namespace core
{

// ----------------------------------------------------------------------
//

    EXPOSE_CLASS(core,ForwardReferencedClass_O);

    void ForwardReferencedClass_O::exposeCando(::core::Lisp_sp lisp)
    {
	::core::class_<ForwardReferencedClass_O>()
//	.initArgs("(self)")
	;
    }

    void ForwardReferencedClass_O::exposePython(::core::Lisp_sp lisp)
    {
#ifdef USEBOOSTPYTHON
	PYTHON_CLASS(Pkg(),ForwardReferencedClass,"","",_LISP)
//	.initArgs("(self)")
	;
#endif
    }




#if 0
    void ForwardReferencedClass_O::serialize(::serialize::SNodeP node)
    {
	IMPLEMENT_ME();
        this->Bases::serialize(node);
	// Archive other instance variables here
    }

    void ForwardReferencedClass_O::archiveBase(::core::ArchiveP node)
    {
	IMPLEMENT_ME();
        this->Base1::archiveBase(node);
	// Archive other instance variables here
    }
#endif


    void ForwardReferencedClass_O::initialize()
    {_OF();
        this->Base::initialize();
	this->_InstanceCoreClass = _Nil<BuiltInClass_O>();
    }


    void ForwardReferencedClass_O::setInstanceCoreClass(BuiltInClass_sp bic)
    {_OF();
	this->_InstanceCoreClass = bic;
    }


    void ForwardReferencedClass_O::defineYourSlotsFromBinderArchiveNode(ArchiveP node)
    {_OF();
	IMPLEMENT_MEF(BF("Implement %s") % __FUNCTION__);
    }

    

}; /* core */
