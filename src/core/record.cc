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
//#define DEBUG_LEVEL_FULL

#include <clasp/core/foundation.h>
#include <clasp/core/object.h>
#include <clasp/core/hashTable.h>
#include <clasp/core/array.h>
#include <clasp/core/arguments.h>
#include <clasp/core/hashTableEq.h>
#include <clasp/core/symbolTable.h>
#include <clasp/core/evaluator.h>
#include <clasp/core/record.h>

#include <clasp/core/wrappers.h>

namespace core {



T_sp record_circle_subst(T_sp replacement_table, T_sp tree) {
  RECORD_LOG(BF("Checking record_circle_subst orig@%p: %s\n") % (void *)(tree.raw_()) %  _rep_(tree) );
  T_sp result = eval::funcall(_sym_circle_subst, replacement_table, tree);
#ifdef DEBUG_RECORD
  if (result.raw_() != tree.raw_()) {
    RECORD_LOG(BF("  YES!!! record_circle_subst tree@%p subst@%p: %s\n") % (void*)(tree.raw_()) % (void *)(result.raw_()) %  _rep_(result) );
  }
#endif
  return result;
}

Record_O::Record_O(RecordStage stage, bool dummy, List_sp data) : _stage(stage), _alist(data), _Seen(_Nil<T_O>()) {}

void Record_O::initialize() {
  if (this->_stage == initializing) {
    this->_Seen = VectorObjects_O::make(16, _Nil<T_O>(), clasp_make_fixnum(0));
  }
}
void Record_O::flagSeen(Cons_sp pair) {
  VectorObjects_sp vvec = gc::As<VectorObjects_sp>(this->_Seen);
  vvec->vectorPushExtend(pair);
}

void Record_O::errorIfInvalidArguments() {
  VectorObjects_sp seenvec = gc::As<VectorObjects_sp>(this->_Seen);
  //  printf("%s:%d arguments seen: %s\n", __FILE__, __LINE__, _rep_(seenvec).c_str());
  //  printf("       arguments passed: %s\n", _rep_(this->_alist).c_str());
  List_sp badArgs(_Nil<T_O>());
  for (auto cur : this->_alist) {
    Cons_sp apair = gc::As<Cons_sp>(oCar(cur));
    T_sp argName = oCar(apair);
    bool found = false;
    for (int i(0), iEnd(cl__length(seenvec)); i < iEnd; ++i) {
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

CL_DEFUN Record_sp core__make_record_patcher(HashTable_sp circle_table)
{
  return Record_O::create_patcher(circle_table);
}

CL_DEFUN void core__patch_object(General_sp tree, Record_sp record) {
  if (tree->fieldsp()) {
    tree->fields(record);
  }
}

CL_DEFMETHOD T_sp Record_O::field_read(Symbol_sp name) {
  if (this->_stage==loading) {
    T_sp result;
    this->field(name,result);
    return result;
  }
  SIMPLE_ERROR(BF("field-read called on a record that is not loading"));
}

CL_DEFMETHOD void Record_O::field_write(Symbol_sp name, T_sp object) {
  if (this->_stage==saving || this->_stage==initializing) {
    this->field(name,object);
    return;
  }
  SIMPLE_ERROR(BF("field-write called on a record that is not saving or initializing"));
}

CL_DEFMETHOD T_sp Record_O::field_patch(Symbol_sp name, T_sp object) {
  if (this->_stage==patching) {
    this->field(name,object);
    return object;
  }
  SIMPLE_ERROR(BF("field-patch called on a record that is not patching"));
}

SYMBOL_EXPORT_SC_(KeywordPkg,initializing);
SYMBOL_EXPORT_SC_(KeywordPkg,saving);
SYMBOL_EXPORT_SC_(KeywordPkg,loading);
SYMBOL_EXPORT_SC_(KeywordPkg,patching);
CL_DEFMETHOD Symbol_sp Record_O::record_stage() const {
  switch (this->_stage) {
  case initializing:
      return kw::_sym_initializing;
  case loading:
      return kw::_sym_loading;
  case saving:
      return kw::_sym_saving;
  case patching:
      return kw::_sym_patching;
  default:
      SIMPLE_ERROR(BF("Illegal stage"));
  }
}

};
