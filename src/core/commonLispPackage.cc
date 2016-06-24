/*
    File: commonLispPackage.cc
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
#include <clasp/core/commonLispPackage.h>
#include <clasp/core/multipleValues.h>
#include <clasp/core/package.h>

namespace cl {

SYMBOL_EXPORT_SC_(ClPkg, length);
SYMBOL_EXPORT_SC_(ClPkg, condition);
SYMBOL_EXPORT_SC_(ClPkg, defvar);
SYMBOL_EXPORT_SC_(ClPkg, defconstant);
SYMBOL_EXPORT_SC_(ClPkg, defparameter);
SYMBOL_EXPORT_SC_(ClPkg, intersection);
SYMBOL_EXPORT_SC_(ClPkg, union);
SYMBOL_EXPORT_SC_(ClPkg, remove);
SYMBOL_EXPORT_SC_(ClPkg, pprint_dispatch);
SYMBOL_EXPORT_SC_(ClPkg, fileStream);

#pragma GCC visibility push(default)

#define ClPkg_SYMBOLS
#define DO_SYMBOL(cname, idx, pkgName, lispName, export) core::Symbol_sp cname;
  #ifndef SCRAPING
    #include SYMBOLS_SCRAPED_INC_H
  #endif
#undef DO_SYMBOL
#undef ClPkg_SYMBOLS

#pragma GCC visibility pop

//    SYMBOL_EXPORT_SC_(ClPkg,defaultPathnameDefaults);

core::Package_sp initialize_commonLispPackage() {
  list<string> lnicknames = {"CL"};
  list<string> luse;
  return _lisp->makePackage("COMMON-LISP", lnicknames, luse);
}
};
