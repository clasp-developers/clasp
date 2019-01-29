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


SYMBOL_SC_(ExtPkg, aSingleExtSymbol);
SYMBOL_EXPORT_SC_(ExtPkg, compiledFunctionName);
SYMBOL_EXPORT_SC_(ExtPkg, ansi_stream);

SYMBOL_EXPORT_SC_(ExtPkg, constant_form_value);
SYMBOL_EXPORT_SC_(ExtPkg, parse_macro);

#define ARGS_af_maybeQuote "(form)"
#define DECL_af_maybeQuote ""
#define DOCS_af_maybeQuote "Quotes a form only if strictly required. This happens when FORM is either a symbol and not a keyword"
CL_DEFUN core::T_sp ext__maybeQuote(core::T_sp form) {
  if (cl__atom(form)) {
    if (form.nilp())
      goto DONTQUOTEIT; // nil
    if (form == _lisp->_true())
      goto DONTQUOTEIT; // t
    if (cl__symbolp(form)) {
      if (cl__keywordp(form)) {
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

SYMBOL_EXPORT_SC_(ExtPkg, maybeQuote);

SYMBOL_EXPORT_SC_(ExtPkg, undefinedClass);

};
