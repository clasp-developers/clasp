/* NOTES:

(1) _deleted is not being used or updated properly.
(2) There is something wrong with WeakKeyHashTable - weak pointers end up pointing to memory that is not the start of an object
(3) The other weak objects (weak pointer, weak mapping) are doing allocations in their constructors.

*/

/*
    File: gcweak.cc
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
#include <clasp/core/foundation.h>
#include <clasp/gctools/memoryManagement.h>
#include <clasp/gctools/gcweak.h>
#include <clasp/core/object.h>
#include <clasp/core/evaluator.h>
#include <clasp/core/mpPackage.h>

namespace gctools {

#ifdef CLASP_THREADS
struct WeakKeyHashTableReadLock {
  const WeakKeyHashTable* _hashTable;
  WeakKeyHashTableReadLock(const WeakKeyHashTable* ht) : _hashTable(ht) {
    if (this->_hashTable->_Mutex) {
      this->_hashTable->_Mutex->lock_shared();
    }
  }
  ~WeakKeyHashTableReadLock() {
    if (this->_hashTable->_Mutex) {
      this->_hashTable->_Mutex->unlock_shared();
    }
  }
};
struct WeakKeyHashTableWriteLock {
  const WeakKeyHashTable* _hashTable;
  WeakKeyHashTableWriteLock(const WeakKeyHashTable* ht, bool upgrade = false) : _hashTable(ht) {
    if (this->_hashTable->_Mutex) {
      this->_hashTable->_Mutex->lock(upgrade);
    }
  }
  ~WeakKeyHashTableWriteLock() {
    if (this->_hashTable->_Mutex) {
      this->_hashTable->_Mutex->unlock();
    }
  }
};
#endif

#ifdef CLASP_THREADS
#define HT_READ_LOCK(me) WeakKeyHashTableReadLock _zzz(me)
#define HT_WRITE_LOCK(me) WeakKeyHashTableWriteLock _zzz(me)
#define HT_UPGRADE_WRITE_LOCK(me) WeakKeyHashTableWriteLock _zzz(me, true)
#else
#define HT_READ_LOCK(me)
#define HT_WRITE_LOCK(me)
#define HT_UPGRADE_WRITE_LOCK(me)
#endif

void WeakKeyHashTable::initialize() {
  int length = this->_Length;
  /* round up to next power of 2 */
  if (length == 0)
    length = 2;
  size_t l;
  for (l = 1; l < length; l *= 2)
    ;
  this->_Keys = KeyBucketsAllocatorType::allocate(Header_s::BadgeStampWtagMtag(Header_s::WeakBucketKind), l);
  this->_Values = ValueBucketsAllocatorType::allocate(Header_s::BadgeStampWtagMtag(Header_s::StrongBucketKind), l);
  this->_Keys->dependent = this->_Values;
  //  GCTOOLS_ASSERT((reinterpret_cast<uintptr_t>(this->_Keys->dependent) & 0x3) == 0);
  this->_Values->dependent = this->_Keys;
}

void WeakKeyHashTable::setupThreadSafeHashTable() {
#ifdef CLASP_THREADS
  core::SimpleBaseString_sp sbsread = core::SimpleBaseString_O::make("WEAKHSHR");
  core::SimpleBaseString_sp sbswrite = core::SimpleBaseString_O::make("WEAKHSHW");
  this->_Mutex = mp::SharedMutex_O::make_shared_mutex(sbsread, sbswrite);
#endif
}

uint WeakKeyHashTable::sxhashKey(const value_type& key) {
  return core::lisp_hash(static_cast<uintptr_t>(gctools::lisp_badge(key)));
}

/*! Return 0 if there is no more room in the sequence of entries for the key
          Return 1 if the element is found or an unbound or deleted entry is found.
          Return the entry index in (b)
        */
size_t WeakKeyHashTable::find_no_lock(gctools::tagged_pointer<KeyBucketsType> keys, const value_type& key, size_t& b) {
  unsigned long i, h, probe;
  unsigned long l = keys->length() - 1;
  int result = 0;
  h = WeakKeyHashTable::sxhashKey(key);

  probe = (h >> 8) | 1;
  h &= l;
  i = h;
  do {
    value_type& k = (*keys)[i];
    if (k.unboundp() || k == key) {
      b = i;
      return 1;
    }
#if defined(USE_BOEHM)
    // Handle splatting
    if (!k.raw_()) {
      auto deleted = value_type(gctools::make_tagged_deleted<core::T_O*>());
      keys->set(i, deleted);
      ValueBucketsType* values = dynamic_cast<ValueBucketsType*>(&*keys->dependent);
      (*values)[i] = value_type(gctools::make_tagged_unbound<core::T_O*>());
    }
#else
    MISSING_GC_SUPPORT();
#endif
    if (result == 0 && (k.deletedp())) {
      b = i;
      result = 1;
    }
    i = (i + probe) & l;
  } while (i != h);
  return result;
}
int WeakKeyHashTable::rehash_not_safe(const value_type& key, size_t& key_bucket) {
  HT_WRITE_LOCK(this);
  size_t newLength;
  if (this->_RehashSize.fixnump()) {
    newLength = this->_Keys->length() + this->_RehashSize.unsafe_fixnum();
  } else if (gc::IsA<core::Float_sp>(this->_RehashSize)) {
    double size = core::clasp_to_double(this->_RehashSize);
    newLength = this->_Keys->length() * size;
  } else {
    SIMPLE_ERROR("Illegal rehash size {}", _rep_(this->_RehashSize));
  }
  int result;
  size_t i, length;
  // buckets_t new_keys, new_values;
  result = 0;
  length = this->_Keys->length();
  MyType newHashTable(newLength, this->_RehashSize, this->_RehashThreshold);
  newHashTable.initialize();
  for (i = 0; i < length; ++i) {
    value_type& old_key = (*this->_Keys)[i];
    if (!old_key.unboundp() && !old_key.deletedp() && old_key.raw_()) {
      size_t found;
      size_t b;
      found = WeakKeyHashTable::find_no_lock(newHashTable._Keys, old_key, b);
      GCTOOLS_ASSERT(found); /* new table shouldn't be full */
      GCTOOLS_ASSERT((*newHashTable._Keys)[b].unboundp()); /* shouldn't be in new table */
      newHashTable._Keys->set(b, old_key);
      (*newHashTable._Values)[b] = (*this->_Values)[i];
      if (key && old_key == key) {
        key_bucket = b;
        result = 1;
      }
      (*newHashTable._Keys).setUsed((*newHashTable._Keys).used() + 1); // TAG_COUNT(UNTAG_COUNT(new_keys->used) + 1);
    }
  }
  GCTOOLS_ASSERT((*newHashTable._Keys).used() == (newHashTable.tableSize()));
  // assert(UNTAG_COUNT(new_keys->used) == table_size(tbl));
  this->swap(newHashTable);
  return result;
}

int WeakKeyHashTable::rehash(const value_type& key, size_t& key_bucket) {
  int result;
  safeRun([&result, this, &key, &key_bucket]() { result = this->rehash_not_safe(key, key_bucket); });
  return result;
}

/*! trySet returns 0 only if there is no room in the hash-table */
int WeakKeyHashTable::trySet(core::T_sp tkey, core::T_sp value) {
  HT_WRITE_LOCK(this);
  size_t b;
  if (tkey == value) {
    value = gctools::make_tagged_same_as_key<core::T_O>();
  }
  value_type key(tkey);
  size_t result = WeakKeyHashTable::find_no_lock(this->_Keys, key, b);
  if ((*this->_Keys)[b].unboundp()) {
    this->_Keys->set(b, key);
    (*this->_Keys).setUsed((*this->_Keys).used() + 1);
  } else if ((*this->_Keys)[b].deletedp()) {
    this->_Keys->set(b, key);
    GCTOOLS_ASSERT((*this->_Keys).deleted() > 0);
    (*this->_Keys).setDeleted((*this->_Keys).deleted() - 1);
  }
  (*this->_Values).set(b, value_type(value));
  return 1;
}

string WeakKeyHashTable::dump(const string& prefix) {
  stringstream sout;
  safeRun([this, &prefix, &sout]() {
    HT_READ_LOCK(this);
    size_t i, length;
    length = this->_Keys->length();
    sout << "===== Dumping WeakKeyHashTable length = " << length << std::endl;
    for (i = 0; i < length; ++i) {
      value_type& old_key = (*this->_Keys)[i];
      sout << prefix << "  [" << i << "]  key= " << old_key.raw_() << "  value = " << (*this->_Values)[i].raw_() << std::endl;
    }
  });
  return sout.str();
};

// ----------------------------------------------------------------------
// ----------------------------------------------------------------------
// ----------------------------------------------------------------------
//
// Use safeRun from here on down
//
// ----------------------------------------------------------------------
// ----------------------------------------------------------------------
// ----------------------------------------------------------------------

core::T_mv WeakKeyHashTable::gethash(core::T_sp tkey, core::T_sp defaultValue) {
  core::T_mv result_mv;
  safeRun([&result_mv, this, tkey, defaultValue]() {
    HT_READ_LOCK(this);
    value_type key(tkey);
    size_t pos;
    size_t result = gctools::WeakKeyHashTable::find_no_lock(this->_Keys, key, pos);
    if (result) {
      value_type& k = (*this->_Keys)[pos];
      if (k.raw_() && !k.unboundp() && !k.deletedp()) {
        core::T_sp value = smart_ptr<core::T_O>((*this->_Values)[pos]);
        if (value.same_as_keyP()) {
          value = smart_ptr<core::T_O>(k);
        }
        result_mv = Values(value, core::lisp_true());
        return;
      }
    }
    result_mv = Values(defaultValue, nil<core::T_O>());
    return;
  });
  return result_mv;
}

void WeakKeyHashTable::set(core::T_sp key, core::T_sp value) {
  safeRun([key, value, this]() {
    if (this->fullp_not_safe() || !this->trySet(key, value)) {
      int res;
      value_type dummyKey;
      size_t dummyPos;
      this->rehash(dummyKey, dummyPos);
      res = this->trySet(key, value);
      GCTOOLS_ASSERT(res);
    }
  });
}

#define HASH_TABLE_ITER(table_type, tablep, key, value)              \
  gctools::tagged_pointer<table_type::KeyBucketsType> iter_Keys;     \
  gctools::tagged_pointer<table_type::ValueBucketsType> iter_Values; \
  core::T_sp key;                                                    \
  core::T_sp value;                                                  \
  {                                                                  \
    HT_READ_LOCK(tablep);                                            \
    iter_Keys = tablep->_Keys;                                       \
    iter_Values = tablep->_Values;                                   \
  }                                                                  \
  for (size_t it(0), itEnd(iter_Keys->length()); it < itEnd; ++it) { \
    {                                                                \
      HT_READ_LOCK(tablep);                                          \
      key = (*iter_Keys)[it];                                        \
      value = (*iter_Values)[it];                                    \
    }                                                                \
    if (key.raw_() && !key.unboundp() && !key.deletedp())

#define HASH_TABLE_ITER_END }

void WeakKeyHashTable::maphash(std::function<void(core::T_sp, core::T_sp)> const& fn) const {
  safeRun([fn, this]() {
    HASH_TABLE_ITER(WeakKeyHashTable, this, key, value) { fn(key, value); }
    HASH_TABLE_ITER_END;
  });
}

void WeakKeyHashTable::maphashFn(core::T_sp fn) const {
  safeRun([fn, this]() {
    HASH_TABLE_ITER(WeakKeyHashTable, this, key, value) { core::eval::funcall(fn, key, value); }
    HASH_TABLE_ITER_END;
  });
}

bool WeakKeyHashTable::remhash(core::T_sp tkey) {
  bool bresult = false;
  safeRun([this, tkey, &bresult]() {
    HT_WRITE_LOCK(this);
    size_t b;
    value_type key(tkey);
    size_t result = gctools::WeakKeyHashTable::find_no_lock(this->_Keys, key, b);
    if (!result || !((*this->_Keys)[b]).raw_() || (*this->_Keys)[b].unboundp() || (*this->_Keys)[b].deletedp()) {
      if (!this->rehash(key, b)) {
        bresult = false;
        return;
      }
    }
    if ((*this->_Keys)[b].raw_() && !(*this->_Keys)[b].unboundp() && !(*this->_Keys)[b].deletedp()) {
      auto deleted = value_type(gctools::make_tagged_deleted<core::T_O*>());
      this->_Keys->set(b, deleted);
      (*this->_Keys).setDeleted((*this->_Keys).deleted() + 1);
      (*this->_Values)[b] = value_type(gctools::make_tagged_unbound<core::T_O*>());
      bresult = true;
      return;
    }
    bresult = false;
  });
  return bresult;
}

void WeakKeyHashTable::clrhash() {
  safeRun([this]() {
    HT_WRITE_LOCK(this);
    size_t len = (*this->_Keys).length();
    for (size_t i(0); i < len; ++i) {
      this->_Keys->set(i, value_type(gctools::make_tagged_deleted<core::T_O*>()));
      (*this->_Values)[i] = value_type(gctools::make_tagged_unbound<core::T_O*>());
    }
    (*this->_Keys).setUsed(0);
    (*this->_Keys).setDeleted(0);
  });
};

core::Vector_sp WeakKeyHashTable::pairs() const {
  size_t len = (*_Keys).length();
  core::ComplexVector_T_sp keyvalues = core::ComplexVector_T_O::make(len * 2, nil<core::T_O>(), core::make_fixnum(0));
  HT_READ_LOCK(this);
  for (size_t i(0); i < len; ++i) {
    if ((*_Keys)[i].raw_() && !(*_Keys)[i].unboundp() && !(*_Keys)[i].deletedp()) {
      keyvalues->vectorPushExtend((*_Keys)[i], 16);
      keyvalues->vectorPushExtend((*_Values)[i], 16);
    }
  }
  return keyvalues;
};

#ifdef USE_BOEHM
WeakPointer::WeakPointer(core::T_sp o) : _value(o.tagged_()) {
  if (o.objectp()) { // pointer, so we're actually weak
    _splattablep = true;
    // note: deregistered automatically if the weak pointer itself is dealloc'd
    GC_general_register_disappearing_link((void**)&_value, &*o);
  }
}

void* WeakPointer::value_helper(void* data) {
  value_helper_s* vhsp = (value_helper_s*)data;
  if (vhsp->wp->_value || !vhsp->wp->_splattablep) // not splatted
    // construct a T_sp in the result
    vhsp->result.emplace(vhsp->wp->_value);
  // otherwise, leave the result default constructed (no T_sp)
  return nullptr; // unused
}

std::optional<core::T_sp> WeakPointer::value() const {
  value_helper_s vhs(this);
  // TODO: Use GC_call_with_reader_lock, but it's too new
  GC_call_with_alloc_lock(value_helper, &vhs);
  return vhs.result;
}
#else // not-actually-weak pointers - TODO for your other GC!
WeakPointer::WeakPointer(core::T_sp o) : _value(o.tagged_()) {}

// always valid
std::optional<core::T_sp> WeakPointer::value() const { return core::T_sp(_value); }
#endif

#ifdef USE_BOEHM
Ephemeron::Ephemeron(core::T_sp k, core::T_sp v)
  : _key(GC_HIDE_POINTER(k.tagged_())), _value(v) {
  GCTOOLS_ASSERT(_key); // basically asserts that ~0 is never passed in,
  // since if it was there'd be no way to tell if it's splatted
  if (k.objectp()) {
    GC_general_register_disappearing_link((void**)&_key, &*k);
    GC_general_register_disappearing_link((void**)&_value, &*k);
  }
}

void Ephemeron::reinit(core::T_sp k, core::T_sp v) {
  if (k.objectp()) {
    // These will implicitly undo any previously registered
    // disappearing link, according to Boehm docs.
    // We undo the old registrations first so that it is not possible
    // for the new _key to get wiped by the old key being collected,
    // in the brief window before the _value is also put in.
    // But FIXME: Do we need to put in a fence or something to ensure
    // the key stays alive within reinit?
    GC_general_register_disappearing_link((void**)&_key, &*k);
    GC_general_register_disappearing_link((void**)&_value, &*k);
  } else {
    GC_unregister_disappearing_link((void**)&_key);
    GC_unregister_disappearing_link((void**)&_value);
  }
  _value = v;
  _key = GC_HIDE_POINTER(k.tagged_());
}

void* Ephemeron::get_helper(void* data) {
  result_helper_s* rhsp = (result_helper_s*)data;
  if (rhsp->eph->_key) { // not splatted
    rhsp->result.key = core::T_sp((Tagged)GC_REVEAL_POINTER(rhsp->eph->_key));
    rhsp->result.value = rhsp->eph->_value;
  } else {
    rhsp->result.key = rhsp->result.value = deleted<core::T_O>();
  }
  return nullptr;
}

KVPair Ephemeron::get() const {
  result_helper_s rhs(this);
  // same TODO with GC_call_with_reader_lock.
  GC_call_with_alloc_lock(get_helper, &rhs);
  return rhs.result;
}
#else // not-actually-weak ephemeron default - FIXME for your GC!
Ephemeron::Ephemeron(core::T_sp key, core::T_sp value) : _key(key), _value(value) {}

void Ephemeron::reinit(core::T_sp k, core::T_sp v) {
  _key = k; _value = v;
}

KVPair Ephemeron::get() const { return KVPair(_key, _value); }
#endif

// Clang says this definition has to be out-of-line. Sure whatever.
const KVPair StrongMapping::initKV = {.key = core::T_sp(tag_no_key<Tagged>()),
    .value = core::T_sp(tag_no_key<Tagged>())};

// note that Ephemeron's constructor doesn't have to do anything
// interesting for non-objects, so we shouldn't need to worry about
// static constructor ordering.
const Ephemeron EphemeronMapping::initEph{no_key<core::T_O>(), no_key<core::T_O>()};

} // namespace gctools
