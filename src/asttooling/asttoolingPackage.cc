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
#include <clasp/core/builtInClass.h>
#include <clasp/asttooling/asttoolingPackage.h>
#include <clasp/asttooling/astExpose.h>
#include <clasp/asttooling/astVisitor.h>
#include <clasp/asttooling/clangTooling.h>
#include <clasp/asttooling/tools.h>
#include <clasp/core/str.h>
#include <clasp/core/wrappers.h>
#include <clasp/asttooling/Registry.h>

namespace asttooling {

#define EXPOSE_TO_CANDO
#define Use_AstToolingPkg
#define EXTERN_REGISTER
#include INIT_CLASSES_INC_H
#undef EXTERN_REGISTER
#undef Use_AstToolingPkg
#undef EXPOSE_TO_CANDO
};

using namespace core;

namespace asttooling {

#pragma GCC visibility push(default)
#define AstToolingPkg_SYMBOLS
#define DO_SYMBOL(cname, idx, pkgName, lispName, export) core::Symbol_sp cname;
  #ifndef SCRAPING
    #include SYMBOLS_SCRAPED_INC_H
#endif
#undef DO_SYMBOL
#undef AstToolingPkg_SYMBOLS
#pragma GCC visibility pop

void AsttoolingExposer::expose(core::Lisp_sp lisp, core::Exposer::WhatToExpose what) const {
  _G();
  switch (what) {
  case candoClasses: {
#define AstToolingPkg_SYMBOLS
#define DO_SYMBOL(cname, idx, pkg, lispname, exportp)          \
  {                                                            \
    cname = _lisp->internUniqueWithPackageName(pkg, core::lispify_symbol_name(lispname)); \
    cname->exportYourself(exportp);                            \
  }
  #ifndef SCRAPING
    #include SYMBOLS_SCRAPED_INC_H
#endif
#undef DO_SYMBOL
#undef AstToolingPkg_SYMBOLS

#define ALL_STAGES
#define Use_AstToolingPkg
#define INVOKE_REGISTER
#define LOOKUP_SYMBOL(s, p) DEFAULT_LOOKUP_SYMBOL(s, p)
#include INIT_CLASSES_INC_H
#undef LOOKUP_SYMBOL
#undef INVOKE_REGISTER
#undef Use_AstToolingPkg
#undef ALL_STAGES

  } break;
  case candoFunctions: {
    initialize_astExpose();
    //	    initialize_tools();
    initialize_clangTooling();
    initialize_astVisitor();
    initialize_Registry();
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
