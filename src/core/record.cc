/*
    File: record.cc
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
#include <clasp/core/hashTable.h>
#include <clasp/core/str.h>
#include <clasp/core/arguments.h>
#include <clasp/core/hashTableEq.h>
#include <clasp/core/symbolTable.h>
#include <clasp/core/vectorObjectsWithFillPtr.h>
#include <clasp/core/evaluator.h>
#include <clasp/core/record.h>

#include <clasp/core/wrappers.h>

namespace core {

EXPOSE_CLASS(core, Record_O);

T_sp record_circle_subst(T_sp replacement_table, T_sp tree) {
  return eval::funcall(_sym_circle_subst, replacement_table, tree);
}

Record_O::Record_O(RecordStage stage, bool dummy, List_sp data) : _stage(stage), _alist(data), _Seen(_Nil<T_O>()) {
  if (stage == initializing) {
    this->_Seen = VectorObjectsWithFillPtr_O::make(_Nil<T_O>(), _Nil<T_O>(), 16, 0, true, cl::_sym_T_O);
  }
}

void Record_O::flagSeen(Cons_sp pair) {
  VectorObjectsWithFillPtr_sp vvec = gc::As<VectorObjectsWithFillPtr_sp>(this->_Seen);
  vvec->vectorPushExtend(pair);
}

void Record_O::errorIfInvalidArguments() {
  VectorObjectsWithFillPtr_sp seenvec = gc::As<VectorObjectsWithFillPtr_sp>(this->_Seen);
  //  printf("%s:%d arguments seen: %s\n", __FILE__, __LINE__, _rep_(seenvec).c_str());
  //  printf("       arguments passed: %s\n", _rep_(this->_alist).c_str());
  List_sp badArgs(_Nil<T_O>());
  for (auto cur : this->_alist) {
    Cons_sp apair = gc::As<Cons_sp>(oCar(cur));
    T_sp argName = oCar(apair);
    bool found = false;
    for (int i(0), iEnd(cl_length(seenvec)); i < iEnd; ++i) {
      if (oCar((*seenvec)[i]) == argName) {
        found = true;
        break;
      }
    }
    if (!found) {
      badArgs = Cons_O::create(argName, badArgs);
    }
  }
  if (badArgs.notnilp()) {
    SIMPLE_ERROR(BF("Initialization of CXX-OBJECT had illegal arguments: %s") % _rep_(badArgs));
  }
}

void Record_O::exposeCando(Lisp_sp lisp) {
  _G();
  class_<Record_O>();
}
void Record_O::exposePython(Lisp_sp lisp) {
  _G();
}
};
