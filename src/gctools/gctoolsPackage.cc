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
#include <clasp/gc/gc_mark.h>
#endif
#ifdef USE_MPS
extern "C" {
#include <clasp/mps/code/mpscamc.h>
};
#endif

#include <stdint.h>

#include <clasp/core/foundation.h>
#include <clasp/core/symbolTable.h>
#include <clasp/gctools/gcFunctions.h>
#include <clasp/gctools/gctoolsPackage.h>
#include <clasp/core/wrappers.h>

using namespace core;

namespace cl {
extern core::Symbol_sp& _sym_fixnum;
};

namespace gctools {

uint64_t globalBytesAllocated = 0;
bool _GlobalDebugAllocations = false;

// -----------------------------------------------------------
// -----------------------------------------------------------
// -----------------------------------------------------------

void GcToolsExposer_O::expose(core::Lisp_sp lisp, core::Exposer_O::WhatToExpose what) const {
  switch (what) {
  case candoClasses: {

  } break;
  case candoFunctions: {

    SYMBOL_EXPORT_SC_(GcToolsPkg, garbageCollect);
    SYMBOL_EXPORT_SC_(GcToolsPkg, maxBootstrapKinds);
    SYMBOL_EXPORT_SC_(GcToolsPkg, bootstrapKindsP);
    SYMBOL_EXPORT_SC_(GcToolsPkg, bootstrapKindSymbols);
    SYMBOL_EXPORT_SC_(GcToolsPkg, STARstamp_field_layout_table_cmdsSTAR );

    SYMBOL_EXPORT_SC_(GcToolsPkg,class_kind);
    SYMBOL_EXPORT_SC_(GcToolsPkg,container_kind);
    SYMBOL_EXPORT_SC_(GcToolsPkg,templated_kind);
    SYMBOL_EXPORT_SC_(GcToolsPkg,fixed_field);
    SYMBOL_EXPORT_SC_(GcToolsPkg,variable_array0);
    SYMBOL_EXPORT_SC_(GcToolsPkg,variable_capacity);
    SYMBOL_EXPORT_SC_(GcToolsPkg,variable_field);
    SYMBOL_EXPORT_SC_(GcToolsPkg,templated_class_jump_table_index);
    SYMBOL_EXPORT_SC_(GcToolsPkg,container_jump_table_index);
    SYMBOL_EXPORT_SC_(GcToolsPkg,layout_end);

    initialize_gc_functions();
    //nothing
  };
      break;
  case candoGlobals: {
    core::SymbolToEnumConverter_sp conv = core::SymbolToEnumConverter_O::create("stamp field layout cmds");
    _sym_STARstamp_field_layout_table_cmdsSTAR->defparameter(conv);
    conv->addSymbolEnumPair(_sym_class_kind,_sym_class_kind,class_kind);
    conv->addSymbolEnumPair(_sym_container_kind,_sym_container_kind,container_kind);
    conv->addSymbolEnumPair(_sym_templated_kind,_sym_templated_kind,templated_kind);
    conv->addSymbolEnumPair(_sym_fixed_field,_sym_fixed_field,fixed_field);
    conv->addSymbolEnumPair(_sym_variable_array0,_sym_variable_array0,variable_array0);
    conv->addSymbolEnumPair(_sym_variable_capacity,_sym_variable_capacity,variable_capacity);
    conv->addSymbolEnumPair(_sym_variable_field,_sym_variable_field,variable_field);
    conv->addSymbolEnumPair(_sym_templated_class_jump_table_index,_sym_templated_class_jump_table_index,templated_class_jump_table_index);
    conv->addSymbolEnumPair(_sym_container_jump_table_index,_sym_container_jump_table_index,container_jump_table_index);
    conv->addSymbolEnumPair(_sym_layout_end,_sym_layout_end,layout_end);
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

