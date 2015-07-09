/*
    File: insertPoint.cc
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

#include <clasp/llvmo/insertPoint.h>
#include <clasp/core/wrappers.h>

namespace llvmo {

InsertPoint_sp InsertPoint_O::create(llvm::IRBuilderBase::InsertPoint &ip) {
  _G();
  GC_ALLOCATE(InsertPoint_O, oip);
  oip->_InsertPoint = ip;
  return oip;
}

EXPOSE_CLASS(llvmo, InsertPoint_O);

void InsertPoint_O::exposeCando(core::Lisp_sp lisp) {
  core::class_<InsertPoint_O>();
}

void InsertPoint_O::exposePython(core::Lisp_sp lisp) {
  _G();
#ifdef USEBOOSTPYTHON
  PYTHON_CLASS(LlvmoPkg, InsertPoint, "", "", _lisp);
#endif
}
};
