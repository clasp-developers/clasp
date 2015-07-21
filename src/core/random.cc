/*
    File: random.cc
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

#include <boost/format.hpp>
#include <clasp/core/common.h>
#include <clasp/core/numbers.h>
#include <clasp/core/symbol.h>
#include <clasp/core/conditions.h>
#include <clasp/core/hashTable.h>
#include <clasp/core/random.h>
#include <clasp/core/wrappers.h>

namespace core {



#define ARGS_RandomState_O_make "(&optional state)"
#define DECL_RandomState_O_make ""
#define DOCS_RandomState_O_make "getUniversalTime"
RandomState_sp RandomState_O::make(T_sp state) {
  IMPLEMENT_MEF(BF("Implement make-random-state"));
}



EXPOSE_CLASS(core,RandomState_O);


void RandomState_O::exposeCando(core::Lisp_sp lisp) {
  core::class_<RandomState_O>()
    ;
  af_def(CorePkg, "make-random-state", &RandomState_O::make, ARGS_RandomState_O_make, DECL_RandomState_O_make, DOCS_RandomState_O_make);
}

void RandomState_O::exposePython(core::Lisp_sp lisp) {
  _G();
#ifdef USEBOOSTPYTHON
  PYTHON_CLASS(CurrentPkg, RandomState, "", "", _lisp);
#endif
}

};
