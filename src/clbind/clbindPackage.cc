/*
    File: clbindPackage.cc
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

#include <clasp/core/foundation.h>
#include <clasp/core/object.h>
#include <clasp/core/lisp.h>
#include <clasp/core/array.h>
#include <clasp/clbind/clbindPackage.h>
#include <clasp/clbind/clbind.h>
#include <clasp/clbind/adapter.h>
#include <clasp/clbind/class_registry.h>
#include <clasp/clbind/class_rep.h>
#include <clasp/core/wrappers.h>

SYMBOL_EXPORT_SC_(ClbindPkg,class_rep);

//
// Load the gctools::GcInfo<core-classes>::Kind specializers
//
#define NAMESPACE_clbind
#include <clasp/gctools/gc_interface.h>
#undef NAMESPACE_clbind

using namespace core;

namespace clbind {

void ClbindExposer_O::expose(core::Lisp_sp lisp, core::Exposer_O::WhatToExpose what) const {
  switch (what) {
  case candoClasses: {
  } break;
  case candoFunctions: {
    //nothing
    //	    initialize_clbind();
  };
      break;
  case candoGlobals: {
    list<string> nicknames;
    list<string> usePackages = {"COMMON-LISP", "CLOS", ClbindPkg};
    _lisp->makePackage("SB-BSD-CLBIND", nicknames, usePackages);
    initialize_clbind();
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
