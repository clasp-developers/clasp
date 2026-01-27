/*
    File: hashTableCustom.cc
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
// #define DEBUG_LEVEL_FULL

#include <clasp/core/foundation.h>
#include <clasp/core/common.h>
#include <clasp/core/hashTableCustom.h>
#include <clasp/core/wrappers.h>
#include <clasp/core/evaluator.h>
#include <clasp/core/designators.h>
namespace core {

// ----------------------------------------------------------------------
//

HashTableCustom_sp HashTableCustom_O::create(Mapping_sp mapping, Number_sp rehashSize, double rehashThreshold, Function_sp comparator, Function_sp hasher) {
  return gctools::GC<HashTableCustom_O>::allocate(mapping, rehashSize, rehashThreshold, comparator, hasher);
}

HashTableCustom_sp HashTableCustom_O::create(uint sz, Number_sp rehashSize, double rehashThreshold, Function_sp comparator,
                                             Function_sp hasher) {
  return create(StrongMapping_O::make(sz), rehashSize, rehashThreshold, comparator, hasher);
}

bool HashTableCustom_O::keyTest(T_sp entryKey, T_sp searchKey) const {

  T_sp c = eval::funcall(comparator, entryKey, searchKey);
  return c.notnilp();
}

void HashTableCustom_O::sxhashEffect(T_sp obj, HashGenerator& hg) const {
  T_sp hash = eval::funcall(hasher, obj);
  if (hash.fixnump()) {
    gc::Fixnum fxhash = hash.unsafe_fixnum();
    if (fxhash >= 0) {
      hg.addValue(fxhash);
      return;
    }
  }
  TYPE_ERROR(hash, Cons_O::createList(cl::_sym_and, cl::_sym_fixnum, Cons_O::createList(cl::_sym_Integer_O, Integer_O::create(0))));
}

// Get the name of a function if it's global, or else signal an error.
// KLUDGE
static T_sp global_fname(Function_sp f) {
  T_sp name = f->functionName();
  // setf functions don't make sense here anyway, so don't bother handling
  if (name.isA<Symbol_O>()) {
    Symbol_sp sname = name.as_unsafe<Symbol_O>();
    if (sname->symbolFunction() == f) return name;
  }
  SIMPLE_ERROR("Can't serialize custom hash table with non-global function {}",
               _rep_(f));
}

void HashTableCustom_O::fields(Record_sp node) {
  switch (node->stage()) {
  case Record_O::initializing:
  case Record_O::loading: {
    T_sp testname, hashname;
    node->field(INTERN_(core, comparator), testname);
    node->field(INTERN_(core, hasher), hashname);
    comparator = coerce::functionDesignator(testname);
    hasher = coerce::functionDesignator(hashname);
  } break;
  case Record_O::saving: {
    T_sp testname = global_fname(comparator);
    T_sp hashname = global_fname(hasher);
    node->field(INTERN_(core, hasher), hashname);
    node->field(INTERN_(core, comparator), testname);
  } break;
  case Record_O::patching: {
    IMPLEMENT_MEF("Add support to patch hash tables");
  } break;
  }
  // We call the parent fields _afterward_ so that any setf gethash it does
  // uses the correct comparator and hasher.
  HashTable_O::fields(node);
}


}; // namespace core
