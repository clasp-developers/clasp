/*
    File: debugLoc.cc
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
#define DEBUG_LEVEL_FULL

#include <clasp/core/foundation.h>
#include <clasp/core/object.h>
#include <clasp/core/lisp.h>
#include <clasp/core/builtInClass.h>
#include <clasp/llvmo/llvmoExpose.h>
#include <clasp/llvmo/debugLoc.h>
#include <clasp/llvmo/debugInfoExpose.h>
#include <clasp/core/wrappers.h>

namespace llvmo {

#if 0
CL_NAME(debug-loc-get);
CL_DEFUN DebugLoc_sp DebugLoc_O::get(int lineno, int column, DebugInfo_sp debugInfo) {
  GC_ALLOCATE(DebugLoc_O, oip);
  llvm::DebugLoc dl = llvm::DebugLoc::get(lineno, column, debugInfo->operator llvm::MDNode *());
  oip->_DebugLoc = dl;
  return oip;
}

CL_LISPIFY_NAME("getScope");
CL_DEFMETHOD MDNode_sp DebugLoc_O::getScope(LLVMContext_sp context) const {
  return translate::to_object<llvm::MDNode *>::convert(this->_DebugLoc.getScope(*(context->wrappedPtr())));
}
#endif

};
