/*
    File: cffiPackage.cc
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
#include <clasp/core/builtInClass.h>
#include <clasp/cffi/cffiPackage.h>
#include <clasp/cffi/cffi.h>
#include <clasp/core/str.h>
#include <clasp/core/wrappers.h>

using namespace core;

namespace cffi {

void CffiExposer::expose(core::Lisp_sp lisp, core::Exposer::WhatToExpose what) const {
  switch (what) {
  case candoClasses: {
#if 0
#define ALL_STAGES
#define Use_CffiPkg
#define INVOKE_REGISTER
#define LOOKUP_SYMBOL(s, p) DEFAULT_LOOKUP_SYMBOL(s, p)
#include <clasp/core/initClasses.h>
#undef LOOKUP_SYMBOL
#undef INVOKE_REGISTER
#undef Use_CffiPkg
#undef ALL_STAGES
#endif
  } break;
  case candoFunctions: {
    //nothing
    initialize_cffi();
  };
      break;
  case candoGlobals: {

    //	initializeLlvmConstants(_lisp);
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

#ifdef USE_MPS
//
// Include the Kinds
//
#ifndef RUNNING_GC_BUILDER
#define NAMESPACE_cffi
#include "clasp_gc.cc"
#undef NAMESPACE_cffi
#endif
#endif

