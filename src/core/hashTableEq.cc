/*
    File: hashTableEq.cc
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
#include <clasp/core/common.h>
#include <clasp/core/environment.h>
#include <clasp/core/hashTableEq.h>
#include <clasp/core/wrappers.h>
namespace core {

// ----------------------------------------------------------------------
//






HashTableEq_sp HashTableEq_O::create(uint sz, Number_sp rehashSize, double rehashThreshold) {
  GC_ALLOCATE(HashTableEq_O, hashTable);
  hashTable->setup(sz, rehashSize, rehashThreshold);
  return hashTable;
}

HashTableEq_sp HashTableEq_O::create_default() {
  DoubleFloat_sp rhs = DoubleFloat_O::create(2.0);
  return HashTableEq_O::create(8, rhs, 1.0);
}

HashTableEq_sp HashTableEq_O::createFromPList(List_sp plist, Symbol_sp nilTerminatedValidKeywords[]) {
  HashTableEq_sp ht = HashTableEq_O::create_default();
  while (plist.notnilp()) {
    Symbol_sp key = gc::As<Symbol_sp>(oCar(plist));
    T_sp val = oCadr(plist);
    plist = oCddr(plist);
    bool hit = false;
    for (int i(0); nilTerminatedValidKeywords[i].notnilp(); ++i) {
      if (key == nilTerminatedValidKeywords[i]) {
        hit = true;
        break;
      }
    }
    if (!hit) {
      stringstream ss;
      for (int j(0); nilTerminatedValidKeywords[j].notnilp(); ++j) {
        ss << " " << _rep_(nilTerminatedValidKeywords[j]);
      }
      SIMPLE_ERROR(BF("Illegal keyword %s valid keywords: %s") % _rep_(key) % ss.str());
    } else {
      ht->setf_gethash(key, val);
    }
  }
  return ht;
}

List_sp HashTableEq_O::tableRef_no_lock(T_sp key) {
    ASSERT(gc::IsA<Array_sp>(this->_HashTable));
    cl_index length = ENSURE_VALID_OBJECT(this->_HashTable)->length();
    cl_index index = this->safe_sxhashKey(key, length, false);
    List_sp pair = _Nil<T_O>();
    for (auto cur : gc::As_unsafe<List_sp>((*ENSURE_VALID_OBJECT(this->_HashTable))[index])) {
      pair = CONS_CAR(cur);
      ASSERT(pair.consp());
      if (CONS_CAR(pair) == key) return pair;
    }
#if defined(USE_MPS)
  // Location dependency test if key is stale
    if (key.objectp()) {
      void *blockAddr = &(*key);
      if (mps_ld_isstale(const_cast<mps_ld_t>(&(this->_LocationDependency)), global_arena, blockAddr)) {
        return this->rehash_no_lock(false, key);
      }
    }
#endif
    return _Nil<T_O>();
  }


bool HashTableEq_O::keyTest(T_sp entryKey, T_sp searchKey) const {
  return cl__eq(entryKey, searchKey);
}

gc::Fixnum HashTableEq_O::sxhashKey(T_sp obj, gc::Fixnum bound, bool willAddKey) const {
  Hash1Generator hg;
#ifdef USE_MPS
  HashTable_O::sxhash_eq(hg, obj, willAddKey ? const_cast<mps_ld_t>(&(this->_LocationDependency)) : NULL);
#endif
#ifdef USE_BOEHM
  HashTable_O::sxhash_eq(hg, obj, NULL);
#endif
  return hg.hashBound(bound);
}

}; /* core */
