#define	DEBUG_LEVEL_FULL

#include "lisp.h"
#include "standardObject.h"
#include "symbolTable.h"
#include "evaluator.h"
#include "metaobject.h"
#include "wrappers.h"


namespace core
{




    Metaobject_O::Metaobject_O() : Metaobject_O::Base()  {};
    Metaobject_O::~Metaobject_O() {};


    void Metaobject_O::exposeCando(Lisp_sp lisp)
	{
	    class_<Metaobject_O>()
		;
	}

    void Metaobject_O::exposePython(Lisp_sp lisp)
    {_G();
#ifdef USEBOOSTPYTHON
	PYTHON_CLASS(CorePkg,Metaobject,"","",_lisp)
		;
#endif
	}



    EXPOSE_CLASS(core,Metaobject_O);
};
