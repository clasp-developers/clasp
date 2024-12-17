/*
    File: hashTable.cc
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

#if 0
/**/                                                                                                                              \
#if 0
/**/                                                                                                                              \
#define DEBUG_HASH_TABLE(expr)                                                                                                     \
  if (core::_sym_STARdebug_hash_tableSTAR.boundp() && core::_sym_STARdebug_hash_tableSTAR->boundP() &&                             \
      core::_sym_STARdebug_hash_tableSTAR->symbolValue().notnilp())                                                                \
  expr
/**/                                                                                                                             \
#else
/**/                                                                                                                              \
#define DEBUG_HASH_TABLE(expr)                                                                                                     \
  if (this->_Debug) {                                                                                                              \
    expr                                                                                                                           \
  }
/**/                                                                                                                                \
#endif
#else
#define DEBUG_HASH_TABLE(expr)
#endif

#if 0
/**/                                                                                                                              \
#define DEBUG_HASH_TABLE1(expr)                                                                                                    \
  if (this->_Debug)                                                                                                                \
  expr
#else
/**/ #define DEBUG_HASH_TABLE1(expr)
#endif

// #define DEBUG_LEVEL_FULL

#include <limits>
#include <clasp/core/foundation.h>
#include <clasp/core/common.h>
#include <clasp/core/corePackage.h>
#include <clasp/core/multipleValues.h>
#include <clasp/core/symbolTable.h>
#include <clasp/core/mpPackage.h>
#include <clasp/core/lispStream.h>
#include <clasp/core/hashTable.h>
#include <clasp/core/hashTableEq.h>
#include <clasp/core/hashTableEql.h>
#include <clasp/core/hashTableEqual.h>
#include <clasp/core/hashTableEqualp.h>
#include <clasp/core/hashTableCustom.h>
#include <clasp/gctools/gcFunctions.h>
#include <clasp/core/array.h>
#include <clasp/core/instance.h>
#include <clasp/core/debugger.h>
#include <clasp/core/fileSystem.h>
#include <clasp/core/evaluator.h>
#include <clasp/core/designators.h>
#include <clasp/core/weakHashTable.h>
#include <clasp/core/wrappers.h>
#ifdef CLASP_THREADS
#ifdef _TARGET_OS_LINUX
#include <sched.h>
#else
#include <pthread.h>
#endif
#endif
namespace core {

std::atomic<size_t> global_next_hash_table_id;

size_t next_hash_table_id() { return global_next_hash_table_id++; }

#ifdef CLASP_THREADS
struct HashTableReadLock {
  const HashTable_O* _hashTable;
  HashTableReadLock(const HashTable_O* ht) : _hashTable(ht) {
    if (this->_hashTable->_Mutex) {
      this->_hashTable->_Mutex->lock_shared();
    }
  }
  ~HashTableReadLock() {
    if (this->_hashTable->_Mutex) {
      this->_hashTable->_Mutex->unlock_shared();
    }
  }
};
struct HashTableWriteLock {
  const HashTable_O* _hashTable;
  HashTableWriteLock(const HashTable_O* ht, bool upgrade = false) : _hashTable(ht) {
    if (this->_hashTable->_Mutex) {
      this->_hashTable->_Mutex->lock(upgrade);
    }
  }
  ~HashTableWriteLock() {
    if (this->_hashTable->_Mutex) {
      this->_hashTable->_Mutex->unlock();
    }
  }
};
#endif

#ifdef CLASP_THREADS
#define HT_READ_LOCK(me) HashTableReadLock _zzz(me)
#define HT_WRITE_LOCK(me) HashTableWriteLock _zzz(me)
#define HT_UPGRADE_WRITE_LOCK(me) HashTableWriteLock _zzz(me, true)
#else
#define HT_READ_LOCK(me)
#define HT_WRITE_LOCK(me)
#endif

Vector_sp HashTable_O::pairs() const {
  // FIXME: use maphash for this, but without the overhead of a std::function
  HT_READ_LOCK(this);
  SimpleVector_sp keyvalues = SimpleVector_O::make(_HashTableCount * 2);
  size_t idx(0);
  for (size_t it(0), itEnd(_Table.size()); it < itEnd; ++it) {
    const KeyValuePair& entry = _Table[it];
    if (!entry._Key.no_keyp() && !entry._Key.deletedp()) {
      keyvalues[idx++] = entry._Key;
      keyvalues[idx++] = entry._Value;
    }
  }
  return keyvalues;
}

DOCGROUP(clasp);
CL_DEFUN Vector_sp core__hash_table_pairs(HashTableBase_sp hash_table_base) {
  if (gc::IsA<HashTable_sp>(hash_table_base)) {
    return hash_table_base.as_unsafe<HashTable_O>()->pairs();
  } else if (gc::IsA<WeakKeyHashTable_sp>(hash_table_base)) {
    WeakKeyHashTable_sp hash_table = gc::As_unsafe<WeakKeyHashTable_sp>(hash_table_base);
    return hash_table->_HashTable.pairs();
  }
  TYPE_ERROR(hash_table_base, Cons_O::createList(cl::_sym_or, cl::_sym_HashTable_O, core::_sym_WeakKeyHashTable_O));
}

// ----------------------------------------------------------------------
//

CL_LAMBDA(&key (test (function eql)) (size 0) (rehash-size 2.0) (rehash-threshold 0.7) weakness debug (thread-safe t) hash-function);
CL_DECLARE();
CL_DOCSTRING(
    R"dx(See CLHS for most behavior. As an extension, Clasp allows a TEST other than the four standard ones to be passed. In this case it must be a designator for a function of two arguments, and a :HASH-FUNCTION must be passed as well; this should be a designator of a function analogous to SXHASH, i.e. it accepts one argument, returns a nonnegative fixnum, and (TEST x y) implies (= (HASH x) (HASH y)).)dx");
DOCGROUP(clasp);
CL_DEFUN T_sp cl__make_hash_table(T_sp test, Fixnum_sp size, Number_sp rehash_size, Real_sp orehash_threshold, Symbol_sp weakness,
                                  T_sp debug, T_sp thread_safe, T_sp hashf) {
  SYMBOL_EXPORT_SC_(KeywordPkg, key);
  if (weakness.notnilp()) {
    if (weakness == INTERN_(kw, key)) {
      // We use fboundp because we actually make hash
      // tables before eq etc. are bound, so symbolFunction would
      // signal an error.
      if (test == cl::_sym_eq || (cl::_sym_eq->fboundp() && test == cl::_sym_eq->symbolFunction())) {
        return core__make_weak_key_hash_table(size);
      } else {
        SIMPLE_ERROR("Weak hash tables non-EQ tests are not yet supported");
      }
    }
    SIMPLE_ERROR("Only :weakness :key (weak-key hash tables) are currently supported");
  }
  double rehash_threshold = maybeFixRehashThreshold(clasp_to_double(orehash_threshold));
  HashTable_sp table = nil<HashTable_O>();
  size_t isize = clasp_to_int(size);
  if (isize == 0)
    isize = 16;
#ifdef DEBUG_REHASH_COUNT
  this->_InitialSize = isize;
#endif
  //	clasp_write_string(fmt::format("{}:{} - make_hash_table - fix me so that I grow by powers of 2\n" , __FILE__ , __LINE__ ));
  if (test == cl::_sym_eq || (cl::_sym_eq->fboundp() && test == cl::_sym_eq->symbolFunction())) {
    table = HashTableEq_O::create(isize, rehash_size, rehash_threshold);
  } else if (test == cl::_sym_eql || (cl::_sym_eql->fboundp() && test == cl::_sym_eql->symbolFunction())) {
    table = HashTableEql_O::create(isize, rehash_size, rehash_threshold);
  } else if (test == cl::_sym_equal || (cl::_sym_equal->fboundp() && test == cl::_sym_equal->symbolFunction())) {
    table = HashTableEqual_O::create(isize, rehash_size, rehash_threshold);
  } else if (test == cl::_sym_equalp || (cl::_sym_equalp->fboundp() && test == cl::_sym_equalp->symbolFunction())) {
    table = HashTableEqualp_O::create(isize, rehash_size, rehash_threshold);
  } else {
    Function_sp comparator = coerce::functionDesignator(test);
    Function_sp hasher = coerce::functionDesignator(hashf);
    table = HashTableCustom_O::create(isize, rehash_size, rehash_threshold, comparator, hasher);
  }
  if (thread_safe.notnilp()) {
    table->setupThreadSafeHashTable();
  }
  return table;
}

void HashTable_O::setupThreadSafeHashTable() {
#ifdef CLASP_THREADS
  SimpleBaseString_sp sbsread = SimpleBaseString_O::make("USRHSHR");
  SimpleBaseString_sp sbswrite = SimpleBaseString_O::make("USRHSHW");
  this->_Mutex = mp::SharedMutex_O::make_shared_mutex(sbsread, sbswrite);
#endif
}

void HashTable_O::setupDebug() {
#ifdef DEBUG_HASH_TABLE_DEBUG
  this->_Debug = true;
#else
  // do nothing
#endif
}

CL_LAMBDA(ht);
CL_DECLARE();
CL_DOCSTRING(R"dx(hash_table_weakness)dx");
DOCGROUP(clasp);
CL_DEFUN Symbol_sp core__hash_table_weakness(T_sp ht) {
  if (gc::IsA<WeakKeyHashTable_sp>(ht)) {
    return kw::_sym_key;
  }
  return nil<Symbol_O>();
}

HashTable_sp HashTable_O::create(T_sp test) {
  Fixnum_sp size = make_fixnum(16);
  DoubleFloat_sp rehashSize = DoubleFloat_O::create(2.0);
  DoubleFloat_sp rehashThreshold = DoubleFloat_O::create(DEFAULT_REHASH_THRESHOLD);
  HashTable_sp ht = gc::As_unsafe<HashTable_sp>(cl__make_hash_table(test, size, rehashSize, rehashThreshold));
  return ht;
}

HashTable_sp HashTable_O::createEq(size_t sz) { return HashTableEq_O::create(sz); }
HashTable_sp HashTable_O::createEql() { return HashTableEql_O::create_default(); }
HashTable_sp HashTable_O::createEqual() { return HashTableEqual_O::create_default(); }
HashTable_sp HashTable_O::createEqualp() { return HashTableEqualp_O::create_default(); }

HashTable_sp HashTable_O::create_thread_safe(T_sp test, SimpleBaseString_sp readLockName, SimpleBaseString_sp writeLockName) {
  Fixnum_sp size = make_fixnum(16);
  DoubleFloat_sp rehashSize = DoubleFloat_O::create(2.0);
  DoubleFloat_sp rehashThreshold = DoubleFloat_O::create(DEFAULT_REHASH_THRESHOLD);
  HashTable_sp ht = gc::As_unsafe<HashTable_sp>(cl__make_hash_table(test, size, rehashSize, rehashThreshold));
  ht->_Mutex = mp::SharedMutex_O::make_shared_mutex(readLockName, writeLockName);
  return ht;
}

// FIXME: contents read could just be atomic maybe?
#define HASH_TABLE_ITER(tablep, key, value)                                                                                        \
  gctools::tagged_pointer<gctools::GCVector_moveable<KeyValuePair>> iter_datap;                                                    \
  T_sp key;                                                                                                                        \
  T_sp value;                                                                                                                      \
  {                                                                                                                                \
    HT_READ_LOCK(tablep);                                                                                                          \
    iter_datap = tablep->_Table._Vector._Contents;                                                                                 \
  }                                                                                                                                \
  for (size_t it(0), itEnd(iter_datap->_End); it < itEnd; ++it) {                                                                  \
    KeyValuePair& entry = (*iter_datap)[it];                                                                                       \
    {                                                                                                                              \
      HT_READ_LOCK(tablep);                                                                                                        \
      key = entry._Key;                                                                                                            \
      value = entry._Value;                                                                                                        \
    }                                                                                                                              \
    if (!key.no_keyp() && !key.deletedp())

#define HASH_TABLE_ITER_END }

void HashTable_O::maphash(T_sp function_desig) const {
  Function_sp func = coerce::functionDesignator(function_desig);
  HASH_TABLE_ITER(this, key, value) { eval::funcall(func, key, value); }
  HASH_TABLE_ITER_END;
}

CL_LAMBDA(function-desig hash-table);
CL_DECLARE();
CL_DOCSTRING(R"dx(see CLHS)dx");
DOCGROUP(clasp);
CL_DEFUN T_sp cl__maphash(T_sp function_desig, HashTableBase_sp hash_table) {
  if (hash_table.nilp()) {
    SIMPLE_ERROR("maphash called with nil hash-table");
  }
  hash_table->maphash(function_desig);
  return nil<T_O>();
}

CL_LAMBDA(hash-table);
CL_DECLARE();
CL_DOCSTRING(R"dx(See CLHS)dx");
DOCGROUP(clasp);
CL_DEFUN T_sp cl__clrhash(HashTableBase_sp hash_table) {
  hash_table->clrhash();
  return hash_table;
};

CL_LAMBDA(&rest args);
CL_DECLARE();
CL_DOCSTRING(R"dx(hash_equal generates an equal hash for a list of objects)dx");
DOCGROUP(clasp);
CL_DEFUN Fixnum core__hash_equal(List_sp args) {
  HashGenerator hg;
  for (auto cur : args) {
    clasp_sxhash(oCar(cur), hg);
    if (hg.isFull()) break;
  }
  return hg.rawhash();
};

CL_LAMBDA(&rest args);
CL_DECLARE();
CL_DOCSTRING(R"dx(hash_equalp generates an equalp hash for a list of objects)dx");
DOCGROUP(clasp);
CL_DEFUN Fixnum core__hash_equalp(List_sp args) {
  HashGenerator hg;
  for (auto cur : args) {
    clasp_sxhash_equalp(oCar(cur), hg);
    if (hg.isFull()) break;
  }
  return hg.rawhash();
};

CL_LAMBDA(key hashtable);
CL_DECLARE();
CL_DOCSTRING(R"dx(remhash)dx");
DOCGROUP(clasp);
CL_DEFUN bool cl__remhash(T_sp key, HashTableBase_sp ht) { return ht->remhash(key); };

T_sp HashTable_O::clrhash() {
  ASSERT(!Number_O::zerop(this->_RehashSize));
  this->_HashTableCount = 0;
  T_sp no_key = ::no_key<T_O>();
  this->_Table.resize(0, KeyValuePair(no_key, no_key));
  this->setup(16, this->_RehashSize, this->_RehashThreshold);
  return this->asSmartPtr();
}

double maybeFixRehashThreshold(double rt) {
  if (rt < 0.0 || rt > DEFAULT_REHASH_THRESHOLD)
    return DEFAULT_REHASH_THRESHOLD;
  return rt;
}
void HashTable_O::setup(uint sz, Number_sp rehashSize, double rehashThreshold) {
  HT_WRITE_LOCK(this);
  sz = this->resizeEmptyTable_no_lock(sz);
  this->_RehashSize = rehashSize;
  ASSERT(!Number_O::zerop(this->_RehashSize));
  this->_RehashThreshold = maybeFixRehashThreshold(rehashThreshold);
}

bool HashTable_O::equalp(T_sp other) const {
  if (this == &(*other))
    return true;
  if (!other.generalp())
    return false;
  if (!gc::IsA<HashTable_sp>(other))
    return false;
  HashTable_sp hto = gc::As_unsafe<HashTable_sp>(other);
  if (this->hashTableTest() != hto->hashTableTest())
    return false;
  if (this->hashTableCount() != hto->hashTableCount())
    return false;
  return this->map_while_true([&hto](T_sp key, T_sp val) -> bool const {
    T_sp other_value = hto->gethash(key);
    if (!cl__equalp(val, other_value)) {
      return false;
    }
    // return true to continue looping the hash-table;
    return true;
  });
}

List_sp HashTable_O::keysAsCons() {
  HT_READ_LOCK(this);
  List_sp res = nil<T_O>();
  this->mapHash([&res](T_sp key, T_sp val) { res = Cons_O::create(key, res); });
  return res;
}

void HashTable_O::fields(Record_sp node) {
  // this->Base::fields(node);
  node->field(INTERN_(core, rehash_size), this->_RehashSize);
  node->/*pod_*/ field(INTERN_(core, rehash_threshold), this->_RehashThreshold);
  switch (node->stage()) {
  case Record_O::initializing:
  case Record_O::loading: {
    Vector_sp keyValueVec;
    node->field(INTERN_(core, data), keyValueVec);
    this->clrhash();
    for (size_t i(0), iEnd(cl__length(keyValueVec)); i < iEnd; ++ ++i) {
      T_sp key = keyValueVec->rowMajorAref(i + 0);
      T_sp val = keyValueVec->rowMajorAref(i + 1);
      this->hash_table_setf_gethash(key, val);
    };
  } break;
  case Record_O::saving: {
    Vector_sp keyValueVec = core__make_vector(cl::_sym_T_O, 2 * this->hashTableCount());
    size_t idx = 0;
    this->mapHash([&idx, &keyValueVec](T_sp key, T_sp val) {
      keyValueVec->rowMajorAset(idx++, key);
      keyValueVec->rowMajorAset(idx++, val);
    });
    node->field(INTERN_(core, data), keyValueVec);
  } break;
  case Record_O::patching: {
    IMPLEMENT_MEF("Add support to patch hash tables");
  } break;
  }
}

uint HashTable_O::resizeEmptyTable_no_lock(size_t sz) {
  if (sz < 16)
    sz = 16;
  T_sp no_key = ::no_key<T_O>();
  this->_HashTableCount = 0;
  this->_Table.resize(sz, KeyValuePair(no_key, no_key));
  return sz;
}

CL_LAMBDA(arg);
CL_DECLARE();
CL_DOCSTRING(R"dx(hash-table-count)dx");
DOCGROUP(clasp);
CL_DEFUN uint cl__hash_table_count(HashTableBase_sp ht) { return ht->hashTableCount(); }

size_t HashTable_O::hashTableCount() const {
  HT_READ_LOCK(this);
  return this->_HashTableCount;
}

uint HashTable_O::calculateHashTableCount() const {
  HT_READ_LOCK(this);
  uint cnt = 0;
  for (size_t it(0), itEnd(this->_Table.size()); it < itEnd; ++it) {
    const KeyValuePair& entry = this->_Table[it];
    if (!entry._Key.no_keyp() && !entry._Key.deletedp())
      ++cnt;
  }
  return cnt;
}

CL_LAMBDA(arg);
CL_DECLARE();
CL_DOCSTRING(R"dx(hash-table-size)dx");
DOCGROUP(clasp);
CL_DEFUN uint cl__hash_table_size(HashTableBase_sp ht) { return ht->hashTableSize(); }

T_sp HashTable_O::operator[](const std::string& key) {
  T_sp tkey = _lisp->internKeyword(key);
  T_mv val = this->gethash(tkey);
  MultipleValues& mvn = core::lisp_multipleValues();
  if (mvn.second(val.number_of_values()).nilp()) {
    SIMPLE_ERROR("Could not find key: {}", tkey);
  }
  return val;
}

size_t HashTable_O::hashTableSize() const {
  HT_READ_LOCK(this);
  return this->_Table.size();
}

bool HashTable_O::keyTest(T_sp entryKey, T_sp searchKey) const { SUBCLASS_MUST_IMPLEMENT(); }

gc::Fixnum HashTable_O::sxhashKey(T_sp obj) const {
  HashGenerator hg;
  this->sxhashEffect(obj, hg);
  // We don't use hashTableSize() here because sxhashKey is only called while
  // the table is already locked.
  return hg.hashBound(this->_Table.size());
}

CL_LAMBDA(key hash-table &optional default-value);
CL_DOCSTRING(R"dx(gethash)dx");
DOCGROUP(clasp);
CL_DEFUN T_mv cl__gethash(T_sp key, HashTableBase_sp hashTable, T_sp default_value) {
  return hashTable->gethash(key, default_value);
};

CL_DOCSTRING(R"dx(gethash3)dx");
DOCGROUP(clasp);
CL_DEFUN T_mv core__gethash3(T_sp key, T_sp hashTable, T_sp default_value) {
  HashTable_sp ht = gc::As_unsafe<HashTable_sp>(hashTable);
  return ht->gethash(key, default_value);
};

KeyValuePair* HashTable_O::searchTable_no_read_lock(T_sp key, cl_index index) {
  size_t tableSize = this->_Table.size();
  size_t cur = index;
  do {
    KeyValuePair& entry = this->_Table[cur];
    if (entry._Key.no_keyp()) break;
    if (!entry._Key.deletedp()) {
      DEBUG_HASH_TABLE(
          { core::clasp_write_string(fmt::format("{}:{} search-end !deletedp index = {}\n", __FILE__, __LINE__, cur)); });
      if (this->keyTest(entry._Key, key)) {
        DEBUG_HASH_TABLE({
          core::clasp_write_string(
              fmt::format("{}:{} search-end found key index = {} entry._Key->{}\n .... {}\n  key->{}\n .... {}\n", __FILE__,
                          __LINE__, cur, (void*)entry._Key.raw_(), dbg_safe_repr((uintptr_t)(void*)entry._Key.raw_()).c_str(),
                          (void*)key.raw_(), dbg_safe_repr((uintptr_t)(void*)key.raw_()).c_str()));
        });
        return &entry;
      }
    }
    cur = (cur + 1) % tableSize;
  } while (cur != index); // loop over the whole table if necessary
  DEBUG_HASH_TABLE({ core::clasp_write_string(fmt::format("{}:{} key not found\n", __FILE__, __LINE__)); });
  return nullptr;
}

CL_LAMBDA(ht);
CL_DECLARE();
CL_DOCSTRING(R"dx(hashTableForceRehash)dx");
DOCGROUP(clasp);
CL_DEFUN void core__hash_table_force_rehash(HashTable_sp ht) {
  ht->rehash(false);
}

T_mv HashTable_O::gethash(T_sp key, T_sp default_value) {
  auto res = this->find(key);
  if (res) return Values(*res, _lisp->_true());
  else return Values(default_value, nil<T_O>());
}

CL_LISPIFY_NAME("core:hashIndex");
CL_DEFMETHOD gc::Fixnum HashTable_O::hashIndex(T_sp key) const {
  return this->sxhashKey(key);
}

std::optional<T_sp> HashTable_O::find(T_sp key) {
  HT_READ_LOCK(this);
  cl_index index = this->sxhashKey(key);
  KeyValuePair* keyValue = this->searchTable_no_read_lock(key, index);
  if (!keyValue) return std::optional<T_sp>();
  else return std::optional<T_sp>(keyValue->_Value);
}

bool HashTable_O::contains(T_sp key) {
  return this->find(key).has_value();
}

bool HashTable_O::remhash(T_sp key) {
  HT_WRITE_LOCK(this);
  cl_index index = this->sxhashKey(key);
  KeyValuePair* keyValuePair = this->searchTable_no_read_lock(key, index);
  if (keyValuePair) {
    keyValuePair->_Key = deleted<T_O>();
    this->_HashTableCount--;
    return true;
  }
  return false;
}

T_sp HashTable_O::setf_gethash_no_write_lock(T_sp key, T_sp value) {
  DEBUG_HASH_TABLE({
    core::clasp_write_string(
        fmt::format("{}:{}:{}   key->{}  value->{}\n", __FILE__, __LINE__, __FUNCTION__, _rep_(key), _rep_(value)));
  });
  if (key.no_keyp()) {
    SIMPLE_ERROR("Do not use {} as a key!!", _rep_(key));
  }

  cl_index index = this->sxhashKey(key);
  DEBUG_HASH_TABLE({
    core::clasp_write_string(fmt::format("{}:{}:{}   index = {}  this->_Table.size() = {}\n", __FILE__, __LINE__, __FUNCTION__,
                                         index, this->_Table.size()));
  });
  KeyValuePair* keyValuePair = this->searchTable_no_read_lock(key, index);
  if (keyValuePair) {
    // rewrite value
    keyValuePair->_Value = value;
    DEBUG_HASH_TABLE({
      core::clasp_write_string(fmt::format("{}:{} Found key/value pair: {},{}\n", __FILE__, __LINE__, _rep_(keyValuePair->_Key),
                                           _rep_(keyValuePair->_Value)));
    });
    DEBUG_HASH_TABLE({
      core::clasp_write_string(
          fmt::format("{}:{}  Did rplacd value: {} to cons at {}\n", __FILE__, __LINE__, _rep_(value), (void*)keyValuePair));
    });
    DEBUG_HASH_TABLE({
      core::clasp_write_string(fmt::format("{}:{}  After rplacd value: {}\n", __FILE__, __LINE__, _rep_(keyValuePair->_Value)));
    });
    return value;
  }
  DEBUG_HASH_TABLE({
    core::clasp_write_string(fmt::format("{}:{}:{} Looking for empty slot index = {}\n", __FILE__, __LINE__, __FUNCTION__, index));
  });
  KeyValuePair* entryP = &this->_Table[index];
  size_t write;
  size_t curEnd = this->_Table.size();
  for (write = index; write < curEnd; ++write, ++entryP) {
    if (entryP->_Key.no_keyp() || entryP->_Key.deletedp())
      goto ADD_KEY_VALUE;
  }
  entryP = &this->_Table[0]; // wrap around
  for (write = 0; write < index; ++write, ++entryP) {
    if (entryP->_Key.no_keyp() || entryP->_Key.deletedp())
      goto ADD_KEY_VALUE;
  }
  goto NO_ROOM;
ADD_KEY_VALUE:
  DEBUG_HASH_TABLE1({
    cl_index index2 = this->sxhashKey(key);
    if (index2 != index) {
      lisp_write(fmt::format("{}:{}:{} INDEX mismatch!!! key = {} badge = {} index = {} write = {} index2 = {} size = {}\n",
                             __FILE__, __LINE__, __FUNCTION__, _rep_(key), lisp_general_badge(gc::As_unsafe<General_sp>(key)),
                             index, write, index2, this->_Table.size()));
    }
  });
  entryP->_Key = key;
  entryP->_Value = value;
  this->_HashTableCount++;
  DEBUG_HASH_TABLE({ core::clasp_write_string(fmt::format("{}:{} Found empty slot at index = {}\n", __FILE__, __LINE__, write)); });
  if (this->_HashTableCount > this->_RehashThreshold * this->_Table.size()) {
    LOG("Expanding hash table");
    this->rehash_no_lock(true);
  }
  return value;
NO_ROOM:
  // ---------
  // There was no room in the Table!!!!!
  // This should never happen!  There should always be room.
  // So print some stuff and then rehash and expand the table and keep going
  //    If this happens change the code that increases the size of the table when the rehash trigger is hit
  //
  printf("%s:%d There is absolutely no room in the hash-table _RehashThreshold = %lf - _HashTableCount -> %lu size -> %lu "
         "increasing size\n",
         __FILE__, __LINE__, this->_RehashThreshold, this->_HashTableCount, this->_Table.size());
  this->rehash_no_lock(true);
  return this->setf_gethash_no_write_lock(key, value);
  // ------------
  // Here we add the key
}

T_sp HashTable_O::hash_table_setf_gethash(T_sp key, T_sp value) {
  LOG("About to hash_table_setf_gethash for {}@{} -> {}@{}\n", _safe_rep_(key), (void*)key.raw_(), _safe_rep_(value),
      (void*)value.raw_());
  HashTableWriteLock _guard(this);
  return this->setf_gethash_no_write_lock(key, value);
}

CL_LISPIFY_NAME("gethash")
CL_LAMBDA(new-value key hash-table &optional default);
DOCGROUP(clasp);
CL_DEFUN_SETF T_sp setf_gethash(T_sp value, T_sp key, HashTableBase_sp hash_table, T_sp default_value) {
  (void)default_value;
  return hash_table->hash_table_setf_gethash(key, value);
}

void HashTable_O::rehash_no_lock(bool expandTable) {
  DEBUG_HASH_TABLE1({ core::clasp_write_string(fmt::format("{}:{} rehash_no_lock\n", __FILE__, __LINE__)); });
  ASSERTF(!Number_O::zerop(this->_RehashSize), "RehashSize is zero - it shouldn't be");
#ifdef DEBUG_HASH_TABLE_DEBUG
  if (this->_Debug) {
    core::T_sp info = INTERN_(kw, rehash);
    T_sp expected;
    Cons_sp cell = core::Cons_O::create(info, nil<T_O>());
    do {
      expected = this->_History.load();
      cell->rplacd(expected);
    } while (!this->_History.compare_exchange_weak(expected, cell));
  }
#endif

  gc::Fixnum curSize = this->_Table.size();
  ASSERTF(this->_Table.size() != 0, "HashTable is empty in expandHashTable curSize={}  this->_Table.size()= {} this shouldn't be",
          curSize, this->_Table.size());
  LOG("At start of expandHashTable current hash table size: {}", this->_Table.size());
  gc::Fixnum newSize = 0;
  if (expandTable) {
    if (cl__integerp(this->_RehashSize)) {
      newSize = curSize + clasp_to_int(gc::As<Integer_sp>(this->_RehashSize));
    } else if (cl__floatp(this->_RehashSize)) {
      newSize = curSize * clasp_to_double(this->_RehashSize);
    }
  } else {
    newSize = curSize;
  }
  gc::Vec0<KeyValuePair> oldTable;
  oldTable.swap(this->_Table);
  newSize = this->resizeEmptyTable_no_lock(newSize);
  LOG("Resizing table to size: {}", newSize);
  size_t oldSize = oldTable.size();
  for (size_t it(0), itEnd(oldSize); it < itEnd; ++it) {
    KeyValuePair& entry = oldTable[it];
    T_sp key = entry._Key;
    T_sp value = entry._Value;
    if (!key.no_keyp() && !key.deletedp()) {
      // key/value represent a valid entry in the hash table
      this->setf_gethash_no_write_lock(key, value);
    }
  }
#ifdef DEBUG_REHASH_COUNT
  this->_RehashCount++;
  MONITOR(BF("Hash-table rehash id %lu initial-size %lu rehash-number %lu rehash-size %lu oldHashTableCount %lu _HashTableCount "
             "%lu\n") %
          this->_HashTableId % this->_InitialSize % this->_RehashCount % newSize % oldHashTableCount % this->_HashTableCount);
#endif
}

void HashTable_O::rehash(bool expandTable) {
  HT_WRITE_LOCK(this);
  rehash_no_lock(expandTable);
}

string HashTable_O::__repr__() const {
  stringstream ss;
  ss << "#<" << this->_instanceClass()->_classNameAsString();
  ss << " :COUNT " << this->_HashTableCount;
  // the calculator is only useful to check that the count is consistent;
  // uncomment this if you need to debug, but otherwise it's redundant.
  //    ss << " :calculated-entries " << this->calculateHashTableCount();
  ss << " :SIZE " << this->_Table.size();
  ss << " @" << (void*)(this) << ">";
  return ss.str();
}

CL_DEFMETHOD string HashTable_O::hash_table_dump() {
  stringstream ss;
  HT_READ_LOCK(this);
  return ss.str();
}

void HashTable_O::mapHash(std::function<void(T_sp, T_sp)> const& fn) const {
  HASH_TABLE_ITER(this, key, value) { fn(key, value); }
  HASH_TABLE_ITER_END;
}

bool HashTable_O::map_while_true(std::function<bool(T_sp, T_sp)> const& fn) const {
  HASH_TABLE_ITER(this, key, value) {
    bool cont = fn(key, value);
    if (!cont)
      return false;
  }
  HASH_TABLE_ITER_END;
  return true;
}

void HashTable_O::lowLevelMapHash(KeyValueMapper* mapper) const {
  HASH_TABLE_ITER(this, key, value) {
    if (!mapper->mapKeyValue(key, value))
      goto DONE;
  }
  HASH_TABLE_ITER_END;
DONE:
  return;
}

string HashTable_O::keysAsString() const {
  stringstream ss;
  this->mapHash([&ss, this](T_sp key, T_sp val) { ss << _rep_(key) << " "; });
  return ss.str();
}

Number_sp HashTable_O::rehash_size() const {
  HT_READ_LOCK(this);
  return this->_RehashSize;
}

double HashTable_O::rehash_threshold() const {
  HT_READ_LOCK(this);
  return this->_RehashThreshold;
}

T_sp HashTable_O::hash_table_test() const { return this->hashTableTest(); }

CL_LAMBDA(arg);
CL_DECLARE();
CL_DOCSTRING(R"dx(hash-table-rehash-size)dx");
DOCGROUP(clasp);
CL_DEFUN Number_sp cl__hash_table_rehash_size(HashTableBase_sp ht) { return ht->rehash_size(); };

CL_LAMBDA(arg);
CL_DECLARE();
CL_DOCSTRING(R"dx(hash-table-rehash-threshold)dx");
DOCGROUP(clasp);
CL_DEFUN double cl__hash_table_rehash_threshold(HashTableBase_sp ht) { return ht->rehash_threshold(); };

CL_LAMBDA(arg);
CL_DECLARE();
CL_DOCSTRING(R"dx(hash-table-test)dx");
DOCGROUP(clasp);
CL_DEFUN T_sp cl__hash_table_test(HashTableBase_sp ht) { return ht->hash_table_test(); };

T_mv clasp_gethash_safe(T_sp key, T_sp thashTable, T_sp default_) {
  HashTable_sp hashTable = gc::As<HashTable_sp>(thashTable);
  return hashTable->gethash(key, default_);
}

SYMBOL_EXPORT_SC_(ClPkg, make_hash_table);
SYMBOL_EXPORT_SC_(ClPkg, maphash);
SYMBOL_EXPORT_SC_(ClPkg, clrhash);
SYMBOL_SC_(CorePkg, hash_eql);
SYMBOL_SC_(CorePkg, hash_equal);
SYMBOL_SC_(CorePkg, hash_equalp);
SYMBOL_EXPORT_SC_(ClPkg, remhash);
SYMBOL_EXPORT_SC_(ClPkg, gethash);

}; // namespace core
