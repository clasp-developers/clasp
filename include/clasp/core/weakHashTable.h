/*
    File: weakHashTable.h
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
#ifndef _core_WeakHashTable_H
#define _core_WeakHashTable_H

#include <clasp/core/foundation.h>
#include <clasp/core/object.h>
#include <clasp/gctools/gcweak.h>
//#include <clasp/core/hashTable.h>
#include <clasp/core/symbolTable.h>
#include <clasp/core/corePackage.fwd.h>

namespace core {

FORWARD(WeakHashTable);
class WeakHashTable_O : public T_O {
  LISP_BASE1(T_O);
  LISP_CLASS(core, CorePkg, WeakHashTable_O, "WeakHashTable");
#if defined(XML_ARCHIVE)
  DECLARE_ARCHIVE();
#endif // defined(XML_ARCHIVE)
  DEFAULT_CTOR_DTOR(WeakHashTable_O);

private: // instance variables here
public:  // Functions here
};

}; /* core */
template <>
struct gctools::GCInfo<core::WeakHashTable_O> {
  static bool constexpr NeedsInitialization = false;
  static bool constexpr NeedsFinalization = false;
  static GCInfo_policy constexpr Policy = normal;
};

TRANSLATE(core::WeakHashTable_O);

namespace core {

FORWARD(WeakKeyHashTable);
class WeakKeyHashTable_O : public WeakHashTable_O {
  LISP_BASE1(WeakHashTable_O);
  LISP_CLASS(core, CorePkg, WeakKeyHashTable_O, "WeakKeyHashTable");
#if defined(XML_ARCHIVE)
  DECLARE_ARCHIVE();
#endif  // defined(XML_ARCHIVE)
public: // instance variables here
#if 1
  typedef typename gctools::WeakHashTable::value_type value_type;
  typedef typename gctools::WeakHashTable::KeyBucketsType KeyBucketsType;
  typedef typename gctools::WeakHashTable::ValueBucketsType ValueBucketsType;
  typedef typename gctools::WeakHashTable::KeyBucketsAllocatorType KeyBucketsAllocatorType;
  typedef typename gctools::WeakHashTable::ValueBucketsAllocatorType ValueBucketsAllocatorType;
  typedef gctools::WeakHashTable HashTableType;
#else
  typedef gctools::tagged_backcastable_base_ptr<T_O> value_type;
  typedef gctools::Buckets<value_type, value_type, gctools::WeakLinks> KeyBucketsType;
  typedef gctools::Buckets<value_type, value_type, gctools::StrongLinks> ValueBucketsType;
  typedef gctools::GCBucketAllocator<KeyBucketsType> KeyBucketsAllocatorType;
  typedef gctools::GCBucketAllocator<ValueBucketsType> ValueBucketsAllocatorType;
  typedef gctools::WeakHashTable<KeyBucketsType, ValueBucketsType> HashTableType;
#endif
  HashTableType _HashTable;

public:
  WeakKeyHashTable_O() : _HashTable(16){};
  WeakKeyHashTable_O(uint sz) : _HashTable(sz){};

public:
  virtual int tableSize() const;
  int size() const { return this->tableSize(); };

  void setf_gethash(T_sp key, T_sp value);

  bool fullp();

  void describe(T_sp stream);
  virtual T_sp hashTableTest() const { return cl::_sym_eq; };
  bool keyTest(T_sp entryKey, T_sp searchKey) const;

  gc::Fixnum sxhashKey(T_sp key, gc::Fixnum bound, bool willAddKey) const;

  void maphash(std::function<void(T_sp, T_sp)> const &fn);

  T_mv gethash(T_sp key, T_sp defaultValue = _Nil<T_O>());
  void remhash(T_sp key);
  void clrhash();
};
}; /* core */
template <>
struct gctools::GCInfo<core::WeakKeyHashTable_O> {
  static bool constexpr NeedsInitialization = false;
  static bool constexpr NeedsFinalization = false;
  static GCInfo_policy constexpr Policy = normal;
};

TRANSLATE(core::WeakKeyHashTable_O);

namespace core {
WeakKeyHashTable_sp core_makeWeakKeyHashTable(Fixnum_sp size);
};

#endif /* _core_WeakHashTable_H */
