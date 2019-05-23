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
//#define DEBUG_LEVEL_FULL

#include <clasp/core/foundation.h>
#include <clasp/core/object.h>
#include <clasp/core/lisp.h>
#include <clasp/llvmo/llvmoExpose.h>
#include <clasp/llvmo/debugLoc.h>
#include <clasp/llvmo/debugInfoExpose.h>
#include <clasp/core/wrappers.h>

namespace llvmo {


CL_LISPIFY_NAME(DebugLoc_get);
CL_DEFUN DebugLoc_sp DebugLoc_O::get(int lineno, int column, MDNode_sp scope) {
  GC_ALLOCATE(DebugLoc_O, oip);
  llvm::DebugLoc dl = llvm::DebugLoc::get(lineno, column, scope->wrappedPtr()); //debugInfo->operator llvm::MDNode *());
  oip->_DebugLoc = dl;
  return oip;
}

CL_LISPIFY_NAME(DebugLoc_make);
CL_DEFUN DebugLoc_sp DebugLoc_O::make(const llvm::DebugLoc& dl) {
  GC_ALLOCATE(DebugLoc_O, oip);
  oip->_DebugLoc = dl;
  return oip;
}

CL_LISPIFY_NAME("getScope");
CL_DEFMETHOD MDNode_sp DebugLoc_O::getScope() const {
  return translate::to_object<llvm::MDNode *>::convert(this->_DebugLoc.getScope());
}

CL_LISPIFY_NAME(DebugLoc_is_valid);
CL_DEFMETHOD bool DebugLoc_O::is_valid() const {
  return !!this->_DebugLoc; // bool operator
}

};
