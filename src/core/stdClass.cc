#define	DEBUG_LEVEL_FULL

#include "lisp.h"
#include "standardObject.h"
#include "symbolTable.h"
#include "evaluator.h"
#include "stdClass.h"
#include "wrappers.h"


namespace core
{




    StdClass_O::StdClass_O() : StdClass_O::Base()  {};
    StdClass_O::~StdClass_O() {};




    void StdClass_O::exposeCando(Lisp_sp lisp)
	{
	    class_<StdClass_O>()
		;
	}

    void StdClass_O::exposePython(Lisp_sp lisp)
    {_G();
#ifdef USEBOOSTPYTHON
	PYTHON_CLASS(CorePkg,StdClass,"","",_lisp)
		;
#endif
	}



    EXPOSE_CLASS(core,StdClass_O);
};
