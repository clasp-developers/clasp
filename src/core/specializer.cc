/*
    File: specializer.cc
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

#include <clasp/core/lisp.h>
#include <clasp/core/standardObject.h>
#include <clasp/core/symbolTable.h>
#include <clasp/core/evaluator.h>
#include <clasp/core/specializer.h>
#include <clasp/core/wrappers.h>

namespace core {

Specializer_O::Specializer_O() : Base(){};
Specializer_O::~Specializer_O(){};

void Specializer_O::exposeCando(Lisp_sp lisp) {
  class_<Specializer_O>();
}

void Specializer_O::exposePython(Lisp_sp lisp) {
  _G();
#ifdef USEBOOSTPYTHON
  PYTHON_CLASS(CorePkg, Specializer, "", "", _lisp);
#endif
}

EXPOSE_CLASS(core, Specializer_O);
};
