#define	DEBUG_LEVEL_FULL

#include "core/foundation.h"
#include "core/lisp.h"
#include "core/environment.h"
#include "null.h"
#include "core/wrappers.h"
namespace core
{

// ----------------------------------------------------------------------
//

    EXPOSE_CLASS(core,Null_O);

    void Null_O::exposeCando(::core::Lisp_sp lisp)
    {
	::core::class_<Null_O>()
//	.initArgs("(self)")
	;
    }

    void Null_O::exposePython(::core::Lisp_sp lisp)
    {
#ifdef USEBOOSTPYTHON
	PYTHON_CLASS_2BASES(Pkg(),Null,"","",_LISP)
//	.initArgs("(self)")
	;
#endif
    }



#if 0
    void Null_O::serialize(::serialize::SNodeP node)
    {
	IMPLEMENT_ME();
        this->Bases::serialize(node);
	// Archive other instance variables here
    }

#endif
#if defined(XML_ARCHIVE)
    void Null_O::archiveBase(::core::ArchiveP node)
    {
	this->T_O::archiveBase(node);
	this->Symbol_O::archiveBase(node);
	this->List_O::archiveBase(node);
    }
#endif // defined(XML_ARCHIVE)


    void Null_O::initialize()
    {_OF();
        this->T_O::initialize();
	this->Symbol_O::initialize();
	this->List_O::initialize();
    }



    string Null_O::__repr__() const
    {_OF();
	return "#<NULL--- NON-NULL instance of NULL!!!!!>";
    }
    

}; /* core */
