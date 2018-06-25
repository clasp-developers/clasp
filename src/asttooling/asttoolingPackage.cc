/*
    File: asttoolingPackage.cc
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
#include <clasp/core/package.h>
#include <clasp/asttooling/asttoolingPackage.h>
#include <clasp/asttooling/astExpose.h>
#include <clasp/asttooling/astVisitor.h>
#include <clasp/asttooling/clangTooling.h>
#include <clasp/asttooling/tools.h>
#include <clasp/core/array.h>
#include <clasp/core/wrappers.h>

using namespace core;

namespace asttooling {

void AsttoolingExposer_O::expose(core::Lisp_sp lisp, core::Exposer_O::WhatToExpose what) const {
  switch (what) {
  case candoClasses: {
  } break;
  case candoFunctions: {
    initialize_astExpose();
    //	    initialize_tools();
    initialize_clangTooling();
    initialize_astVisitor();
//    initialize_Registry();
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
