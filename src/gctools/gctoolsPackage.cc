/*
    File: gctoolsPackage.cc
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
#include <boost/mpl/list.hpp>
#ifdef USE_BOEHM
#include <gc/gc_mark.h>
#endif
#ifdef USE_MPS
extern "C" {
#include <clasp/mps/code/mpscamc.h>
};
#endif

#include <stdint.h>

#include <clasp/core/foundation.h>
#include <clasp/gctools/symbolTable.h>
#include <clasp/gctools/gctoolsPackage.h>
#include <clasp/core/wrappers.h>

using namespace core;

namespace cl {
extern core::Symbol_sp _sym_fixnum;
};

namespace gctools {

uint64_t globalBytesAllocated = 0;
bool _GlobalDebugAllocations = false;


// -----------------------------------------------------------
// -----------------------------------------------------------
// -----------------------------------------------------------

#if 0

/*! Hardcode a few kinds of objects for bootstrapping
 */

const char *global_HardcodedKinds[] = {
    "", "core::T_O", "core::StandardObject_O", "core::Metaobject_O", "core::Specializer_O", "core::Class_O", "core::BuiltInClass_O", "core::StdClass_O", "core::StandardClass_O", "core::StructureClass_O", "core::Symbol_O", "core::Str_O"};

#define ARGS_af_maxBootstrapKinds "()"
#define DECL_af_maxBootstrapKinds ""
#define DOCS_af_maxBootstrapKinds "maxBootstrapKinds"
int af_maxBootstrapKinds() {
  _G();
  return sizeof(global_HardcodedKinds) / sizeof(global_HardcodedKinds[0]);
}

int iBootstrapKind(const string &name) {
  for (int i(0), iEnd(af_maxBootstrapKinds()); i < iEnd; ++i) {
    //            printf("%s:%d i[%d]Checking if %s == %s\n", __FILE__, __LINE__, i, global_HardcodedKinds[i], name.c_str());
    if (strcmp(global_HardcodedKinds[i], name.c_str()) == 0) {
      return i;
    }
  }
  SIMPLE_ERROR(BF("Illegal bootstrap-kind %s") % name);
}

void initialize_bootstrap_kinds() {
  DEPRECIATED();
// Hard-coded bootstrap kinds
#define SetupKind(_x_) _x_::static_Kind = iBootstrapKind(#_x_)
  SetupKind(core::T_O);
  SetupKind(core::StandardObject_O);
  SetupKind(core::Metaobject_O);
  SetupKind(core::Specializer_O);
  SetupKind(core::Class_O);
  SetupKind(core::BuiltInClass_O);
  SetupKind(core::StdClass_O);
  SetupKind(core::StandardClass_O);
  SetupKind(core::StructureClass_O);
  SetupKind(core::Symbol_O);
  SetupKind(core::Str_O);
}

#define ARGS_af_bootstrapKindSymbols "()"
#define DECL_af_bootstrapKindSymbols ""
#define DOCS_af_bootstrapKindSymbols "bootstrapKindSymbols"
core::Cons_sp af_bootstrapKindSymbols() {
  _G();
  core::Cons_sp list(_Nil<core::Cons_O>());
  for (int i(af_maxBootstrapKinds() - 1); i > 0; --i) {
    string name = global_HardcodedKinds[i];
    list = core::Cons_O::create(core::Str_O::create(name), list);
  }
  return list;
}

#define ARGS_af_bootstrapKindP "(arg)"
#define DECL_af_bootstrapKindP ""
#define DOCS_af_bootstrapKindP "bootstrap-kind-p return a generalized boolean of the bootstrap-kind - either the boostrap kind index or nil"
core::T_sp af_bootstrapKindP(const string &name) {
  _G();
  for (int i(0), iEnd(af_maxBootstrapKinds()); i < iEnd; ++i) {
    //            printf("%s:%d i[%d]Checking if %s == %s\n", __FILE__, __LINE__, i, global_HardcodedKinds[i], name.c_str());
    if (strcmp(global_HardcodedKinds[i], name.c_str()) == 0) {
      return core::make_fixnum(i);
    }
  }
  return _Nil<core::T_O>();
}

#endif





#pragma GCC visibility push(default)
#define GcToolsPkg_SYMBOLS
#define DO_SYMBOL(cname, idx, pkgName, lispName, export) core::Symbol_sp cname;
#include SYMBOLS_SCRAPED_INC_H
#undef DO_SYMBOL
#undef GcToolsPkg_SYMBOLS
#pragma GCC visibility pop

void GcToolsExposer::expose(core::Lisp_sp lisp, core::Exposer::WhatToExpose what) const {
  _G();
  switch (what) {
  case candoClasses: {

#define GcToolsPkg_SYMBOLS
#define DO_SYMBOL(cname, idx, pkg, lispname, exportp)                   \
  {                                                                     \
    gctools::cname = _lisp->internUniqueWithPackageName(pkg, lispname); \
    gctools::cname->exportYourself(exportp);                            \
  }
#include SYMBOLS_SCRAPED_INC_H
#undef DO_SYMBOL
#undef GcToolsPkg_SYMBOLS

  } break;
  case candoFunctions: {

    SYMBOL_EXPORT_SC_(GcToolsPkg, garbageCollect);
    SYMBOL_EXPORT_SC_(GcToolsPkg, maxBootstrapKinds);
    SYMBOL_EXPORT_SC_(GcToolsPkg, bootstrapKindsP);
    SYMBOL_EXPORT_SC_(GcToolsPkg, bootstrapKindSymbols);
    //nothing
  };
      break;
  case candoGlobals: {
  };
      break;
  case pythonClasses:
  case pythonFunctions:
  case pythonGlobals: {
    IMPLEMENT_ME();
  } break;
  }
}
};

#if USE_INTRUSIVE_SMART_PTR == 1
#define EXPAND_CLASS_MACROS
#define _CLASS_MACRO(_U_) \
  STATIC_CLASS_INFO(_U_); \
  INTRUSIVE_POINTER_REFERENCE_COUNT_ACCESSORS(_U_)
#include INIT_CLASSES_INC_H
#undef _CLASS_MACRO
#undef EXPAND_CLASS_MACROS
#endif
