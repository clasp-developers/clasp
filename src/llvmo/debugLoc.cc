#define DEBUG_LEVEL_FULL

#include "core/foundation.h"
#include "core/object.h"
#include "core/lisp.h"
#include "core/builtInClass.h"
#include "llvmoExpose.h"
#include "debugLoc.h"
#include "llvmo/debugInfoExpose.h"
#include "core/wrappers.h"


namespace llvmo
{


    DebugLoc_sp DebugLoc_O::get(int lineno, int column, DebugInfo_sp debugInfo)
    {_G();
	GC_RESERVE_BEGIN(DebugLoc_O,oip ){
	    GC_RESERVE_GET(DebugLoc_O,oip );
	} GC_RESERVE_END(DebugLoc_O,oip );
	llvm::DIDescriptor* didescriptor = debugInfo->operator llvm::DIDescriptor* ();
	llvm::DebugLoc dl = llvm::DebugLoc::get(lineno,column,didescriptor->operator llvm::MDNode* ());
	oip->_DebugLoc = dl;
	return oip;
    }




    EXPOSE_CLASS(llvmo,DebugLoc_O);

    void DebugLoc_O::exposeCando(core::Lisp_sp lisp)
    {
	core::class_<DebugLoc_O>()
	    .def("getLine",&DebugLoc_O::getLine)
	    .def("getCol",&DebugLoc_O::getCol)
	    .def("getScope",&DebugLoc_O::getScope)
	    ;
	core::af_def(LlvmoPkg,"DebugLoc-get",&DebugLoc_O::get);
    }

    void DebugLoc_O::exposePython(core::Lisp_sp lisp)
    {_G();
#ifdef USEBOOSTPYTHON
	PYTHON_CLASS(LlvmoPkg,DebugLoc,"","",_lisp)
	    ;
#endif
    }

    MDNode_sp DebugLoc_O::getScope(LLVMContext_sp context) const
    {_G();
	return translate::to_object<llvm::MDNode*>::convert(this->_DebugLoc.getScope(*(context->wrappedPtr())));
    }

};
