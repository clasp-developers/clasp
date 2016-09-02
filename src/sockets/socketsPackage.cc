/*
    File: socketsPackage.cc
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
#include <clasp/core/package.h>
#include <clasp/core/object.h>
#include <clasp/core/lisp.h>
#include <clasp/core/builtInClass.h>
#include <clasp/sockets/socketsPackage.h>
#include <clasp/sockets/sockets.h>
#include <clasp/core/str.h>
#include <clasp/core/wrappers.h>

namespace sockets {

#define EXPOSE_TO_CANDO
#define Use_SocketsPkg
#define EXTERN_REGISTER
//#include <clasp/core/initClasses.h>
#undef EXTERN_REGISTER
#undef Use_SocketsPkg
#undef EXPOSE_TO_CANDO
};

using namespace core;

namespace sockets {


void SocketsExposer_O::expose(core::Lisp_sp lisp, core::Exposer_O::WhatToExpose what) const {
  switch (what) {
  case candoClasses: {
#define ALL_STAGES
#define Use_SocketsPkg
#define INVOKE_REGISTER
//#include <clasp/core/initClasses.h>
#undef INVOKE_REGISTER
#undef Use_SocketsPkg
#undef ALL_STAGES

  } break;
  case candoFunctions: {
    //nothing
  };
      break;
  case candoGlobals: {
    list<string> nicknames;
    list<string> usePackages = {"COMMON-LISP", "CLOS", SocketsPkg};
    _lisp->makePackage("SB-BSD-SOCKETS", nicknames, usePackages);
    initialize_sockets_globals();
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

