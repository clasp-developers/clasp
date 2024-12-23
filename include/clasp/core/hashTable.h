#pragma once
/*
    File: hashTable.h
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

#include <clasp/core/object.h>
#include <clasp/core/record.h>
#include <clasp/core/array.h>
#include <clasp/core/hashTableBase.h>
#include <clasp/core/mpPackage.fwd.h>
#include <clasp/core/corePackage.fwd.h>

namespace core {
double maybeFixRehashThreshold(double rt);
#define DEFAULT_REHASH_THRESHOLD 0.7

T_sp cl__make_hash_table(T_sp test, Fixnum_sp size, Number_sp rehash_size, Real_sp orehash_threshold,
                         Symbol_sp weakness = nil<T_O>(), T_sp debug = nil<T_O>(), T_sp thread_safe = nil<T_O>(),
                         T_sp hashf = nil<T_O>());

size_t next_hash_table_id();

}; // namespace core

namespace core {

FORWARD(Mapping);
class Mapping_O : public General_O {
  LISP_ABSTRACT_CLASS(core, CorePkg, Mapping_O, "Mapping", General_O);
public:
  // This count is increased by newEntry and decreased by remove.
  // It is therefore an exact count for strong mappings but potentially
  // an undercount for weak mappings, for which the GC may delete an
  // entry without decrementing this count. It is maintained anyway so
  // that the hash table can decide to rehash based on an easily
  // accessible count rather than having to iterate over the whole mapping.
  // This means weak tables may be rehashed "early", but there's no harm
  // done, and the count is reset to the exact count on rehash so error
  // can't accumulate indefinitely.
  size_t _Count = 0;
public:
  virtual size_t size() const = 0;
  virtual size_t count() const = 0; // exact
  size_t countInexact() const { return _Count; }
  virtual Mapping_sp realloc(size_t) const = 0;
  virtual gctools::KVPair get(size_t) const = 0;
  virtual void setValue(size_t, T_sp) = 0;
  virtual void newEntry(size_t, T_sp, T_sp) = 0;
  virtual void remove(size_t) = 0;
  size_t computeCount() const {
    size_t count = 0, sz = size();
    for (size_t i = 0; i < sz; ++i) {
      auto p = get(i);
      if (!p.key.no_keyp() && !p.key.deletedp()) ++count;
    }
    return count;
  }
};

FORWARD(StrongMapping);
class StrongMapping_O final : public Mapping_O {
  LISP_CLASS(core, CorePkg, StrongMapping_O, "StrongMapping", Mapping_O);
public:
  // need typedefs for e.g. sizeof_container
  typedef gctools::StrongMapping::value_type value_type;
public:
  StrongMapping_O(size_t size) : _Mapping(size) {}
  static StrongMapping_sp make(size_t);
public:
  gctools::StrongMapping _Mapping;
public:
  virtual size_t size() const { return _Mapping.size(); }
  virtual size_t count() const { return _Count; }
  virtual Mapping_sp realloc(size_t sz) const { return make(sz); }
  virtual gctools::KVPair get(size_t i) const { return _Mapping.get(i); }
  virtual void setValue(size_t i, T_sp v) { _Mapping.setValue(i, v); }
  virtual void newEntry(size_t i, T_sp k, T_sp v) {
    _Mapping.newEntry(i, k, v);
    _Count++;
  }
  virtual void remove(size_t i) {
    _Mapping.remove(i);
    _Count--;
  }
};

FORWARD(WeakKeyMapping);
class WeakKeyMapping_O final : public Mapping_O {
  LISP_CLASS(core, CorePkg, WeakKeyMapping_O, "WeakKeyMapping", Mapping_O);
public:
  // need typedefs for e.g. sizeof_container
  typedef gctools::EphemeronMapping::value_type value_type;
public:
  WeakKeyMapping_O(size_t size) : _Mapping(size) {}
  static WeakKeyMapping_sp make(size_t);
public:
  gctools::EphemeronMapping _Mapping;
public:
  virtual size_t size() const { return _Mapping.size(); }
  virtual size_t count() const { return computeCount(); }
  virtual Mapping_sp realloc(size_t sz) const { return make(sz); }
  virtual gctools::KVPair get(size_t i) const { return _Mapping.get(i); }
  virtual void setValue(size_t i, T_sp v) { _Mapping.setValue(i, v); }
  virtual void newEntry(size_t i, T_sp k, T_sp v) { ++_Count; _Mapping.newEntry(i, k, v); }
  virtual void remove(size_t i) { --_Count; _Mapping.remove(i); }
};

FORWARD(HashTable);
class HashTable_O : public HashTableBase_O {
  friend T_sp cl__make_hash_table(T_sp test, Fixnum_sp size, Number_sp rehash_size, Real_sp orehash_threshold, Symbol_sp weakness,
                                  T_sp debug, T_sp thread_safe, T_sp hashf);
  friend class HashTableReadLock;
  friend class HashTableWriteLock;
  LISP_CLASS(core, ClPkg, HashTable_O, "HashTable", HashTableBase_O);
  bool fieldsp() const override { return true; };
  void fields(Record_sp node) override;

  friend T_sp cl__maphash(T_sp function_desig, T_sp hash_table);
  HashTable_O()
      : _RehashSize(nil<Number_O>()), _RehashThreshold(maybeFixRehashThreshold(0.7))
                           {};
  HashTable_O(Mapping_sp mapping,
              Number_sp rehashSize, double rehashThreshold)
    : _RehashSize(rehashSize), _RehashThreshold(maybeFixRehashThreshold(rehashThreshold)),
      _Table(mapping) {}
  friend T_sp cl__maphash(T_sp function_desig, HashTable_sp hash_table);
  friend T_sp cl__clrhash(HashTable_sp hash_table);

public: // instance variables here
  Number_sp _RehashSize;
  double _RehashThreshold;
  Mapping_sp _Table;
#ifdef CLASP_THREADS
  mutable mp::SharedMutex_sp _Mutex;
#endif
public:
  static HashTable_sp create(T_sp test); // set everything up with defaults
  static HashTable_sp createEq(size_t sz = 8);
  static HashTable_sp createEqWeakKey(size_t sz = 8);
  static HashTable_sp createEql();
  static HashTable_sp createEqual();
  static HashTable_sp createEqualp();
  static HashTable_sp create_thread_safe(T_sp test, SimpleBaseString_sp readLockName,
                                         SimpleBaseString_sp writeLockName); // set everything up with defaults

public:
  void setupThreadSafeHashTable();

private:
  uint resizeEmptyTable_no_lock(size_t sz);
  uint calculateHashTableCount() const;

  void rehash_no_lock(bool expandTable);
  T_sp setf_gethash_no_write_lock(T_sp key, T_sp value);
  gc::Fixnum sxhashKey(T_sp key) const; // NOTE: Only call with (read) lock held

  std::optional<size_t> searchTable_no_read_lock(T_sp key, cl_index index);
  inline size_t count_no_lock() const {
    return _Table->count();
  }

protected:
  virtual void sxhashEffect(T_sp key, HashGenerator& hg) const { SUBIMP() };

public:
  void rehash(bool expandTable);

  CL_LISPIFY_NAME("hash-table-shared-mutex");
  CL_DEFMETHOD T_sp hash_table_shared_mutex() const {
    if (this->_Mutex)
      return this->_Mutex;
    else
      return nil<T_O>();
  };

public: // Functions here
  virtual bool equalp(T_sp other) const override;

  /*! See CLHS */
  virtual T_sp hashTableTest() const { SUBIMP(); };

  /*! Return a count of the number of keys */
  size_t hashTableCount() const override;
  size_t hashTableSize() const override;
  size_t size() { return this->hashTableCount(); };

  T_sp operator[](const std::string& key);

  virtual bool keyTest(T_sp entryKey, T_sp searchKey) const;

  /*! Return true if the key is within the hash table */
  bool contains(T_sp key);

  std::optional<T_sp> find(T_sp key); // gethash but more convenient for C++

  T_mv gethash(T_sp key, T_sp defaultValue = nil<T_O>()) override;
  gc::Fixnum hashIndex(T_sp key) const;

  T_sp hash_table_setf_gethash(T_sp key, T_sp value) override;
  void setf_gethash(T_sp key, T_sp val) { this->hash_table_setf_gethash(key, val); };

  Number_sp rehash_size() const override;
  double rehash_threshold() const override;
  T_sp hash_table_test() const override;

  T_sp clrhash() override;

  bool remhash(T_sp key) override;

  string __repr__() const override;

  string hash_table_dump();

  void lowLevelMapHash(KeyValueMapper* mapper) const;

  void maphash(T_sp fn) const override;

  void mapHash(std::function<void(T_sp, T_sp)> const& fn) const;
  void maphash(std::function<void(T_sp, T_sp)> const& fn) const { this->mapHash(fn); };

  /*! maps function across a hash table until the function returns false */
  bool /*terminatingMapHash*/ map_while_true(std::function<bool(T_sp, T_sp)> const& fn) const;

  string keysAsString() const;

  /*! Look like a set */
  void insert(T_sp obj) { this->setf_gethash(obj, nil<T_O>()); };
  /*! Return a Cons of all keys */
  List_sp keysAsCons();
  /* Return a vector of all keys and values. */
  Vector_sp pairs() const;
};

T_mv clasp_gethash_safe(T_sp key, T_sp hashTable, T_sp default_);

}; // namespace core
