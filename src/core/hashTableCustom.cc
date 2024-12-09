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
namespace core {

// ----------------------------------------------------------------------
//

HashTableCustom_sp HashTableCustom_O::create(uint sz, Number_sp rehashSize, double rehashThreshold, Function_sp comparator,
                                             Function_sp hasher) {
  auto hashTable = gctools::GC<HashTableCustom_O>::allocate_with_default_constructor();
  hashTable->setup(sz, rehashSize, rehashThreshold);
  hashTable->comparator = comparator;
  hashTable->hasher = hasher;
  return hashTable;
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

}; // namespace core
