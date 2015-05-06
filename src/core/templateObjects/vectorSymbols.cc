/*
    File: vectorSymbols.cc
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
#define DEBUG_LEVEL_FULL

#include "core/foundation.h"
#include "core/common.h"
#include "core/environment.h"
#include "core/symbolTable.h"
#include "vectorSymbols.h"
#include "core/wrappers.h"
namespace core {

// ----------------------------------------------------------------------
//

EXPOSE_CLASS(core, VectorSymbols_O);

#define ARGS_VectorSymbols_O_make "(initial-element initial-contents dimension)"
#define DECL_VectorSymbols_O_make ""
#define DOCS_VectorSymbols_O_make "make VectorSymbols args: initial-element initial-contents dimension"
VectorSymbols_sp VectorSymbols_O::make(Symbol_sp initialElement, Cons_sp initialContents, int dimension) {
  _G();
  GC_RESERVE_TRY(VectorSymbols_O, vo) {
    GC_RESERVE_GET(VectorSymbols_O, vo);
    vo->setup(initialElement, initialContents, dimension);
  }
  return vo;
}

void VectorSymbols_O::exposeCando(::core::Lisp_sp lisp) {
  ::core::class_<VectorSymbols_O>(lisp->lisp());
  Defun_maker(VectorSymbols);
}

void VectorSymbols_O::exposePython(::core::Lisp_sp lisp) {
  PYTHON_CLASS(Pkg(), VectorSymbols, "", "", lisp->lisp())
      //	.initArgs("(self)")
      ;
}

T_sp VectorSymbols_O::elementType() const {
  return _sym_Symbol_O;
}

}; /* core */
