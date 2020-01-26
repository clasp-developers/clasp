/*
    File: hashTableEql.cc
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
#include <clasp/core/symbolTable.h>
#include <clasp/core/hashTableEql.h>
#include <clasp/core/wrappers.h>
namespace core {

// ----------------------------------------------------------------------
//






HashTableEql_sp HashTableEql_O::create(uint sz, Number_sp rehashSize, double rehashThreshold) {
  if (sz == 0)
    sz = 16;
  GC_ALLOCATE(HashTableEql_O, hashTable);
  hashTable->setup(sz, rehashSize, rehashThreshold);
  return hashTable;
}

SYMBOL_EXPORT_SC_(ClPkg, eql);
HashTableEql_sp HashTableEql_O::create_default() {
  DoubleFloat_sp rhs = DoubleFloat_O::create(2.0);
  HashTableEql_sp ht = HashTableEql_O::create(16, rhs, DEFAULT_REHASH_THRESHOLD);
  return ht;
}

#if 0
    void HashTableEql_O::serialize(::serialize::SNodeP node)
    {
        this->Bases::serialize(node);
	// Archive other instance variables here
    }
#endif

#if defined(XML_ARCHIVE)
void HashTableEql_O::archiveBase(::core::ArchiveP node) {
  this->Base::archiveBase(node);
  // Archive other instance variables here
}
#endif // defined(XML_ARCHIVE)

bool HashTableEql_O::keyTest(T_sp entryKey, T_sp searchKey) const {
  _OF();
  return cl__eql(entryKey, searchKey);
}

gc::Fixnum HashTableEql_O::sxhashKey(T_sp obj, gc::Fixnum bound, HashGenerator& hg) const {
  HashTable_O::sxhash_eql(hg, obj);
  gc::Fixnum hash = hg.hashBound(bound);
  LOG(BF("HashTableEql_O::sxhashKey obj[%s] raw_hash[%s] bound[%d] hash[%d]") % _rep_(obj) % hg.asString() % bound % hash);
  return hash;
}

}; /* core */
