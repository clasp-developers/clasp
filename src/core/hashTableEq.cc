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
// #define DEBUG_LEVEL_FULL

#include <clasp/core/foundation.h>
#include <clasp/core/common.h>
#include <clasp/core/hashTableEq.h>
#include <clasp/core/wrappers.h>
namespace core {

// ----------------------------------------------------------------------
//

HashTableEq_sp HashTableEq_O::create(uint sz, Number_sp rehashSize, double rehashThreshold) {
  auto hashTable = gctools::GC<HashTableEq_O>::allocate_with_default_constructor();
  hashTable->setup(sz, rehashSize, rehashThreshold);
  return hashTable;
}

HashTableEq_sp HashTableEq_O::create_default() {
  DoubleFloat_sp rhs = DoubleFloat_O::create(2.0);
  return HashTableEq_O::create(8, rhs, DEFAULT_REHASH_THRESHOLD);
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
      SIMPLE_ERROR("Illegal keyword {} valid keywords: {}", _rep_(key), ss.str());
    } else {
      ht->setf_gethash(key, val);
    }
  }
  return ht;
}

KeyValuePair* HashTableEq_O::searchTable_no_read_lock(T_sp key, cl_index index) {
  for (size_t cur = index, curEnd(this->_Table.size()); cur < curEnd; ++cur) {
    KeyValuePair& entry = this->_Table[cur];
    if (entry._Key == key)
      return &entry;
    if (entry._Key.no_keyp())
      goto NOT_FOUND;
  }
  for (size_t cur = 0, curEnd(index); cur < curEnd; ++cur) {
    KeyValuePair& entry = this->_Table[cur];
    if (entry._Key == key)
      return &entry;
    if (entry._Key.no_keyp())
      goto NOT_FOUND;
  }
NOT_FOUND:
  return nullptr;
}

bool HashTableEq_O::keyTest(T_sp entryKey, T_sp searchKey) const { return cl__eq(entryKey, searchKey); }

void HashTableEq_O::sxhashEffect(T_sp obj, HashGenerator& hg) const {
  if (obj.generalp()) hg.addGeneralAddress(obj.as_unsafe<General_O>());
  else if (obj.consp()) hg.addConsAddress(obj.as_unsafe<Cons_O>());
  else hg.addValue((uintptr_t)obj.raw_());
}

}; // namespace core
