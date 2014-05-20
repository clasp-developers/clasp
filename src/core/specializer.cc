#define	DEBUG_LEVEL_FULL

#include "lisp.h"
#include "standardObject.h"
#include "symbolTable.h"
#include "evaluator.h"
#include "specializer.h"
#include "wrappers.h"


namespace core
{




    Specializer_O::Specializer_O() : Base()  {};
    Specializer_O::~Specializer_O() {};




    void Specializer_O::exposeCando(Lisp_sp lisp)
	{
	    class_<Specializer_O>()
		;
	}

    void Specializer_O::exposePython(Lisp_sp lisp)
    {_G();
#ifdef USEBOOSTPYTHON
	PYTHON_CLASS(CorePkg,Specializer,"","",_lisp)
		;
#endif
	}



    EXPOSE_CLASS(core,Specializer_O);
};
