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
#include <clasp/gctools/telemetry.h>
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
    initialize_gc_functions();
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

