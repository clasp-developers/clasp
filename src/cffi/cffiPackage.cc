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

namespace cffi {

#define EXPOSE_TO_CANDO
#define Use_CffiPkg
#define EXTERN_REGISTER
#include INIT_CLASSES_INC_H
#undef EXTERN_REGISTER
#undef Use_CffiPkg
#undef EXPOSE_TO_CANDO
};

using namespace core;

namespace cffi {

#pragma GCC visibility push(default)
#define CffiPkg_SYMBOLS
#define DO_SYMBOL(cname, idx, pkgName, lispName, export) core::Symbol_sp cname;
  #ifndef SCRAPING
    #include SYMBOLS_SCRAPED_INC_H
#endif
#undef DO_SYMBOL
#undef CffiPkg_SYMBOLS
#pragma GCC visibility pop

void CffiExposer::expose(core::Lisp_sp lisp, core::Exposer::WhatToExpose what) const {
  _G();
  switch (what) {
  case candoClasses: {
#define CffiPkg_SYMBOLS
#define DO_SYMBOL(cname, idx, pkg, lispname, exportp)          \
  {                                                            \
    cname = _lisp->internUniqueWithPackageName(pkg, core::lispify_symbol_name(lispname)); \
    cname->exportYourself(exportp);                            \
  }
  #ifndef SCRAPING
    #include SYMBOLS_SCRAPED_INC_H
#endif
#undef DO_SYMBOL
#undef CffiPkg_SYMBOLS

#define ALL_STAGES
#define Use_CffiPkg
#define INVOKE_REGISTER
#define LOOKUP_SYMBOL(s, p) DEFAULT_LOOKUP_SYMBOL(s, p)
#include INIT_CLASSES_INC_H
#undef LOOKUP_SYMBOL
#undef INVOKE_REGISTER
#undef Use_CffiPkg
#undef ALL_STAGES

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
#include STATIC_ANALYZER_PRODUCT
#undef NAMESPACE_cffi
#endif
#endif

#if USE_INTRUSIVE_SMART_PTR == 1
#define EXPAND_CLASS_MACROS

#if defined(USE_MPS) // MPS doesn't require INTRUSIVE_POINTER_REFERENCE_COUNT_ACCESSORS
#define _CLASS_MACRO(_T_) \
  STATIC_CLASS_INFO(_T_);
#else
#define _CLASS_MACRO(_T_) \
  STATIC_CLASS_INFO(_T_); \
  INTRUSIVE_POINTER_REFERENCE_COUNT_ACCESSORS(_T_);
#endif

#include INIT_CLASSES_INC_H
#undef _CLASS_MACRO
#undef EXPAND_CLASS_MACROS
#endif
