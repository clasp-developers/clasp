/*
    File: extensionPackage.cc
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
#include <clasp/core/symbolTable.h>
#include <clasp/core/extensionPackage.h>
#include <clasp/core/predicates.h>
#include <clasp/core/multipleValues.h>
#include <clasp/core/package.h>

namespace ext {
using namespace core;

#pragma GCC visibility push(default)
#define ExtPkg_SYMBOLS
#define DO_SYMBOL(cname, idx, pkgName, lispName, export) core::Symbol_sp cname;
  #ifndef SCRAPING
    #include SYMBOLS_SCRAPED_INC_H
  #endif
#undef DO_SYMBOL
#undef ExtPkg_SYMBOLS
#pragma GCC visibility pop

SYMBOL_EXPORT_SC_(ExtPkg, STARloadHooksSTAR);
SYMBOL_SC_(ExtPkg, aSingleExtSymbol);
SYMBOL_SC_(ExtPkg, lambda_block);
SYMBOL_EXPORT_SC_(ExtPkg, STARinvokeDebuggerHookSTAR);
SYMBOL_EXPORT_SC_(ExtPkg, compiledFunctionName);

#define ARGS_af_maybeQuote "(form)"
#define DECL_af_maybeQuote ""
#define DOCS_af_maybeQuote "Quotes a form only if strictly required. This happens when FORM is either a symbol and not a keyword"
T_sp af_maybeQuote(T_sp form) {
  _G();
  if (cl_atom(form)) {
    if (form.nilp())
      goto DONTQUOTEIT; // nil
    if (form == _lisp->_true())
      goto DONTQUOTEIT; // t
    if (cl_symbolp(form)) {
      if (af_keywordP(form)) {
        goto DONTQUOTEIT; // symbol keyword
      } else
        goto QUOTEIT; // symbol not keyword
    }
    goto DONTQUOTEIT; // every other atom
  } else {
    if (oFirst(form) == cl::_sym_quote)
      goto DONTQUOTEIT; // already quoted
  }
QUOTEIT:
  return Cons_O::createList(cl::_sym_quote, form);
DONTQUOTEIT:
  return form;
}

void initialize_extension_functions() {
  SYMBOL_EXPORT_SC_(ExtPkg, maybeQuote);
  Defun(maybeQuote);
};

void initialize_extensionPackage() {
  list<string> lnicknames;
  list<string> luse = {"COMMON-LISP"};
  _lisp->makePackage("EXT", lnicknames, luse);
  // We don't have to create the EXTENSION symbols here - it's done in bootStrapCoreSymbolMap
}
};
