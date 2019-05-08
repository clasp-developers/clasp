/*
    File: closPackage.cc
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
#include <clasp/core/closPackage.h>
#include <clasp/core/multipleValues.h>
#include <clasp/core/package.h>

namespace clos {

SYMBOL_SC_(ClosPkg, aSingleClosSymbol);
SYMBOL_EXPORT_SC_(ClosPkg, class_precedence_list);
SYMBOL_EXPORT_SC_(ClosPkg,slot_definition);
SYMBOL_EXPORT_SC_(ClosPkg,NAME);
SYMBOL_EXPORT_SC_(ClosPkg,subclassesSTAR);
SYMBOL_EXPORT_SC_(ClosPkg,LOCATION_TABLE);
SYMBOL_EXPORT_SC_(ClosPkg,slot_value_set);
SYMBOL_EXPORT_SC_(ClosPkg,update_instance);
SYMBOL_EXPORT_SC_(ClosPkg,dispatch_miss);
SYMBOL_EXPORT_SC_(ClosPkg,_PLUS_the_standard_class_PLUS_);
SYMBOL_EXPORT_SC_(ClosPkg,funcallable_standard_class);
SYMBOL_EXPORT_SC_(ClosPkg,dispatcher_count);

SYMBOL_EXPORT_SC_(ClosPkg, DIRECT_SUPERCLASSES);
SYMBOL_EXPORT_SC_(ClosPkg, DIRECT_SUBCLASSES);
SYMBOL_EXPORT_SC_(ClosPkg, DEPENDENTS);
SYMBOL_EXPORT_SC_(ClosPkg, SLOTS);
SYMBOL_EXPORT_SC_(ClosPkg, DIRECT_DEFAULT_INITARGS);
SYMBOL_EXPORT_SC_(ClosPkg, FINALIZED);
SYMBOL_EXPORT_SC_(ClosPkg, PRECEDENCE_LIST);
SYMBOL_EXPORT_SC_(ClosPkg, DIRECT_SLOTS);
SYMBOL_EXPORT_SC_(ClosPkg, DEFAULT_INITARGS);
SYMBOL_EXPORT_SC_(ClosPkg, CALL_HISTORY_GENERIC_FUNCTIONS);
SYMBOL_EXPORT_SC_(ClosPkg, SPECIALIZER_MUTEX);
SYMBOL_EXPORT_SC_(ClosPkg, NUMBER_OF_SLOTS_IN_STANDARD_CLASS);
SYMBOL_EXPORT_SC_(ClosPkg, NUMBER_OF_SLOTS_IN_STRUCTURE_CLASS);

void initialize_closPackage() {
  list<string> lnicknames;
  list<string> luse = {"COMMON-LISP"};
  _lisp->makePackage("CLOS", lnicknames, luse);
  // We don't have to create the CLOS symbols here - it's done in bootStrapCoreSymbolMap
}
};
