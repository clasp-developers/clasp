/*
    File: cleavirPrimopsPackage.cc
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
#include <clasp/core/cleavirPrimopsPackage.h>
#include <clasp/core/multipleValues.h>
#include <clasp/core/package.h>

namespace cleavirPrimops {

SYMBOL_EXPORT_SC_(CleavirPrimopsPkg, typeq);
SYMBOL_EXPORT_SC_(CleavirPrimopsPkg, callWithVariableBound);
SYMBOL_EXPORT_SC_(CleavirPrimopsPkg, eq);
SYMBOL_EXPORT_SC_(CleavirPrimopsPkg, fixnumLess);
SYMBOL_EXPORT_SC_(CleavirPrimopsPkg, fixnumNotGreater);
SYMBOL_EXPORT_SC_(CleavirPrimopsPkg, fixnumEqual);
SYMBOL_EXPORT_SC_(CleavirPrimopsPkg, car);
SYMBOL_EXPORT_SC_(CleavirPrimopsPkg, cdr);
SYMBOL_EXPORT_SC_(CleavirPrimopsPkg, funcall);
SYMBOL_EXPORT_SC_(CleavirPrimopsPkg, unreachable);

void initialize_cleavirPrimopsPackage() {
  list<string> lnicknames;
  list<string> luse = {};
  _lisp->makePackage(CleavirPrimopsPkg, lnicknames, luse);
  // We don't have to create the CLEAVIR-PRIMOPS symbols here - it's done in bootStrapCoreSymbolMap
}
};
