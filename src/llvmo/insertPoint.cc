#define DEBUG_LEVEL_FULL

#include "core/foundation.h"
#include "core/object.h"
#include "core/lisp.h"
#include "core/builtInClass.h"

#include "insertPoint.h"
#include "core/wrappers.h"



namespace llvmo
{


    InsertPoint_sp InsertPoint_O::create(llvm::IRBuilderBase::InsertPoint& ip)
    {_G();
        GC_ALLOCATE(InsertPoint_O,oip );
	oip->_InsertPoint = ip;
	return oip;
    }

    EXPOSE_CLASS(llvmo,InsertPoint_O);

    void InsertPoint_O::exposeCando(core::Lisp_sp lisp)
    {
	core::class_<InsertPoint_O>()
	    ;
    }

    void InsertPoint_O::exposePython(core::Lisp_sp lisp)
    {_G();
#ifdef USEBOOSTPYTHON
	PYTHON_CLASS(LlvmoPkg,InsertPoint,"","",_lisp)
	    ;
#endif
    }


};
