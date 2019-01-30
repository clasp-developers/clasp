/*
    File: hashTableEqual.cc
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
#include <clasp/core/hashTableEqual.h>
#include <clasp/core/wrappers.h>
namespace core {

// ----------------------------------------------------------------------
//






HashTableEqual_sp HashTableEqual_O::create(uint sz, Number_sp rehashSize, double rehashThreshold) {
  GC_ALLOCATE(HashTableEqual_O, hashTable);
  hashTable->setup(sz, rehashSize, rehashThreshold);
  return hashTable;
}

SYMBOL_EXPORT_SC_(ClPkg, equal);
HashTableEqual_sp HashTableEqual_O::create_default() {
  DoubleFloat_sp rhs = DoubleFloat_O::create(2.0);
  HashTableEqual_sp ht = HashTableEqual_O::create(16, rhs, DEFAULT_REHASH_THRESHOLD);
  return ht;
}

#if 0
    void HashTableEqual_O::serialize(::serialize::SNodeP node)
    {
        this->Bases::serialize(node);
	// Archive other instance variables here
    }
#endif

#if defined(XML_ARCHIVE)
void HashTableEqual_O::archiveBase(::core::ArchiveP node) {
  this->Base::archiveBase(node);
  // Archive other instance variables here
}
#endif // defined(XML_ARCHIVE)

bool HashTableEqual_O::keyTest(T_sp entryKey, T_sp searchKey) const {
  _OF();
  return cl__equal(entryKey, searchKey);
}

gc::Fixnum HashTableEqual_O::sxhashKey(T_sp obj, gc::Fixnum bound, bool willAddKey) const {
#if defined(DEBUG_HASH_TABLE) && defined(DEBUG_HASH_GENERATOR)
  HashGenerator hg(this->_DebugHashTable);
#else
  HashGenerator hg;
#endif
#ifdef USE_MPS
  HashTable_O::sxhash_equal(hg, obj, willAddKey ? const_cast<mps_ld_t>(&(this->_LocationDependency)) : NULL);
#else
  HashTable_O::sxhash_equal(hg, obj, NULL);
#endif
  return hg.hash(bound);
}

}; /* core */
