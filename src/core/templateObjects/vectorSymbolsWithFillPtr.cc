/*
    File: vectorSymbolsWithFillPtr.cc
*/

/*
Copyright (c) 2014, Christian E. Schafmeister
 
CLASP is free software; you can redistribute it and/or
modify it under the terms of the GNU Library General Public
License as published by the Free Software Foundation; either
version 2 of the License, or (at your option) any later version.
 
See directory 'clasp/licenses' for full details.
 
The above copyright notice and this permission notice shall be included in
all copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
THE SOFTWARE.
*/
/* -^- */
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
