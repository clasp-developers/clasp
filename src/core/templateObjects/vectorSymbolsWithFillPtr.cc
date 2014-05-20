#define	DEBUG_LEVEL_FULL

#include "core/common.h"
#include "core/environment.h"
#include "core/symbolTable.h"
#include "vectorSymbolsWithFillPtr.h"
#include "core/wrappers.h"
namespace core
{

// ----------------------------------------------------------------------
//

    EXPOSE_CLASS(core,VectorSymbolsWithFillPtr_O);




#define ARGS_VectorSymbolsWithFillPtr_O_make "(initial-element initial-contents dimension)"
#define DECL_VectorSymbolsWithFillPtr_O_make ""
#define DOCS_VectorSymbolsWithFillPtr_O_make "make VectorSymbolsWithFillPtr args: initial-element initial-contents dimension"
    VectorSymbolsWithFillPtr_sp VectorSymbolsWithFillPtr_O::make(Symbol_sp initialElement, Cons_sp initialContents, int dimension, int fillPtr)
    {_G();
	GC_RESERVE_TRY(VectorSymbolsWithFillPtr_O,vo ){
	    GC_RESERVE_GET(VectorSymbolsWithFillPtr_O,vo );
	    vo->setup(initialElement,initialContents,dimension,fillPtr);
	}
	return vo;
    }




    void VectorSymbolsWithFillPtr_O::exposeCando(::core::Lisp_sp lisp)
    {
	::core::class_<VectorSymbolsWithFillPtr_O>(lisp->lisp())
	    ;
	Defun_maker(VectorSymbolsWithFillPtr);
    }

    void VectorSymbolsWithFillPtr_O::exposePython(::core::Lisp_sp lisp)
    {
	PYTHON_CLASS(Pkg(),VectorSymbolsWithFillPtr,"","",lisp->lisp())
//	.initArgs("(self)")
	;
    }


    T_sp VectorSymbolsWithFillPtr_O::elementType() const
    {
	return _sym_Symbol_O;
    }



template <> core::Vector_O_template_<core::Symbol_O>* core::Vector_O_template_<core::Symbol_O>::___staticDereferencedNilInstance;
template <> core::Vector_O_template_<core::Symbol_O>* core::Vector_O_template_<core::Symbol_O>::___staticDereferencedUnboundInstance;




    
}; /* core */


namespace boost
{
//    INTRUSIVE_POINTER_REFERENCE_COUNT_ACCESSORS(core::VectorWithFillPtr_O_template_<core::Symbol_O>);
};
