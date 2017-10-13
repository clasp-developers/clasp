/*
    File: compPackage.cc
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

#include <clasp/core/foundation.h>
#include <clasp/core/object.h>
#include <clasp/core/lisp.h>
#include <clasp/core/symbol.h>
#include <clasp/core/compPackage.h>
#include <clasp/core/multipleValues.h>
#include <clasp/core/package.h>

namespace comp {

SYMBOL_SC_(CompPkg, aSingleCompilerSymbol);
SYMBOL_EXPORT_SC_(CompPkg, STARlowLevelTraceSTAR);
SYMBOL_EXPORT_SC_(CompPkg, STARthe_moduleSTAR);
SYMBOL_EXPORT_SC_(CompPkg, STARlowLevelTracePrintSTAR);
SYMBOL_EXPORT_SC_(CompPkg, jit_remove_module);
SYMBOL_EXPORT_SC_(CompPkg, jit_register_symbol);
SYMBOL_EXPORT_SC_(CompPkg, STARjit_saved_symbol_infoSTAR);
SYMBOL_EXPORT_SC_(CompPkg, STARsave_module_for_disassembleSTAR);
SYMBOL_EXPORT_SC_(CompPkg, STARsaved_module_from_clasp_jitSTAR);
SYMBOL_EXPORT_SC_(CompPkg, optimize_module_for_compile);
SYMBOL_EXPORT_SC_(CompPkg, compile_quick_module_dump);
void initialize_compPackage() {
  list<string> lnicknames = {"CMP"};
  list<string> luse = {"COMMON-LISP"};
  _lisp->makePackage("COMPILER", lnicknames, luse);
}
};
