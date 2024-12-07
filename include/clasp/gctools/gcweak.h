#pragma once

/*
    File: gcweak.h
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

/* Derived from scheme-advanced.c by ravenbrook */

/* C. COPYRIGHT AND LICENSE
 *
 * Copyright (C) 2001-2014 Ravenbrook Limited <http://www.ravenbrook.com/>.
 * All rights reserved.  This is an open source license.  Contact
 * Ravenbrook for commercial licensing options.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions are
 * met:
 *
 * 1. Redistributions of source code must retain the above copyright
 * notice, this list of conditions and the following disclaimer.
 *
 * 2. Redistributions in binary form must reproduce the above copyright
 * notice, this list of conditions and the following disclaimer in the
 * documentation and/or other materials provided with the distribution.
 *
 * 3. Redistributions in any form must be accompanied by information on how
 * to obtain complete source code for this software and any accompanying
 * software that uses this software.  The source code must either be
 * included in the distribution or be available for no more than the cost
 * of distribution plus a nominal fee, and must be freely redistributable
 * under reasonable conditions.  For an executable file, complete source
 * code means the source code for all modules it contains. It does not
 * include source code for modules or files that typically accompany the
 * major components of the operating system on which the executable file
 * runs.
 *
 * THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS
 * IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED
 * TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR
 * PURPOSE, OR NON-INFRINGEMENT, ARE DISCLAIMED. IN NO EVENT SHALL THE
 * COPYRIGHT HOLDERS AND CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT,
 * INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT
 * NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF
 * USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON
 * ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
 * (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF
 * THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
 */

#include <functional>

namespace gctools {

#if defined(USE_BOEHM) && !defined(CLASP_THREADS)
#define call_with_alloc_lock GC_call_with_alloc_lock
#else
typedef void* (*fn_type)(void* client_data);
inline void call_with_alloc_lock(fn_type fn, void* client_data) { fn(client_data); }

#endif

template <class F> requires std::invocable<F>
static void* wrapRun(void* f) {
  F* gf = (F*)f;
  (*gf)();
  return nullptr;
}

template <class F> requires std::invocable<F>
void safeRun(F&& f) {
  call_with_alloc_lock(wrapRun<F>, (void*)&f);
}
}; // namespace gctools

namespace gctools {

struct WeakObject {
  virtual void* dependentPtr() const { return NULL; };
};

struct weak_fwd_s : public WeakObject {
  WeakObject* fwd;                         /* forwarded object */
  gctools::smart_ptr<core::Fixnum_I> size; /* total size of this object */
};

struct weak_fwd2_s : public WeakObject {
  WeakObject* fwd; /* forwarded object */
};

struct weak_pad_s : public WeakObject {
  //  WeakObject *fwd;                         /* forwarded object */  /*WHY!!*//
  gctools::smart_ptr<core::Fixnum_I> size; /* total size of this object */
};

struct weak_pad1_s : public WeakObject {};

template <class T, class U> struct BucketsBase : public WeakObject {
  BucketsBase() = default;
  BucketsBase(int l) : _length(l), _used(0), _deleted(0) {
    for (size_t i(0); i < l; ++i) {
      this->bucket[i] = T((gctools::Tagged)gctools::tag_unbound<typename T::Type*>());
    }
  }

  T& operator[](size_t idx) { return this->bucket[idx]; };
  typedef T value_type;
  typedef gctools::tagged_pointer<BucketsBase<U, T>> dependent_type;
  dependent_type dependent;                    /* the dependent object */
  size_t _length;                              /* number of buckets */
  size_t _used;                                /* number of buckets in use */
  size_t _deleted;                             /* number of deleted buckets */
  T bucket[0];                                 /* hash buckets */

  void* dependentPtr() const {
    if (this->dependent)
      return reinterpret_cast<void*>(&*this->dependent);
    return NULL;
  };

  size_t length() const { return _length; }
  void setLength(size_t l) { _length = l; }
  int used() const { return _used; }
  void setUsed(size_t val) { _used = val; }
  int deleted() const { return _deleted; }
  void setDeleted(size_t val) { _deleted = val; }
};

template <class T, class U, class Link> struct Buckets;

#ifdef USE_BOEHM
inline bool unboundOrDeletedOrSplatted(core::T_sp bucket) {
  return (bucket.unboundp()    // unbound
          || bucket.deletedp() // deleted
          || !bucket           // splatted by Boehm
  );
}
#endif

template <class T, class U> struct Buckets<T, U, WeakLinks> : public BucketsBase<T, U> {
  typedef typename BucketsBase<T, U>::value_type value_type;
  Buckets(int l) : BucketsBase<T, U>(l){};
  Buckets(snapshotSaveLoad::snapshot_save_load_init_s* isl) { isl->fill((void*)this); }

  void set(size_t idx, const value_type& val) {
    if (!(val.objectp() || val.deletedp() || val.unboundp())) {
      printf("%s:%d Only  objectp() objects can be added to Mapping - tried to add %p\n", __FILE__, __LINE__, val.raw_());
      abort();
    }
#if defined(USE_BOEHM)
    //	    printf("%s:%d ---- Buckets set idx: %zu   this->bucket[idx] = %p\n", __FILE__, __LINE__, idx, this->bucket[idx].raw_()
    //);
    if (!unboundOrDeletedOrSplatted(this->bucket[idx])) {
      auto& rawRef = this->bucket[idx].rawRef_();
      void** linkAddress = reinterpret_cast<void**>(&rawRef);
      int result = GC_unregister_disappearing_link(linkAddress); // reinterpret_cast<void**>(&this->bucket[idx].rawRef_()));
      if (!result)
        throw_hard_error("The link was not registered as a disappearing link!");
    }
    this->bucket[idx] = val;
    GCTOOLS_ASSERT(val.objectp());
    // We need the base of the object that we want a weak pointer to...
    // general, cons and later weak objects have different header sizes
    void* base = NULL;
    if (val.generalp()) {
      base = gctools::GeneralPtrToHeaderPtr(&*(val));
    } else if (val.consp()) {
      base = gctools::ConsPtrToHeaderPtr(&*(val));
    }
    if (base)
      GC_general_register_disappearing_link(reinterpret_cast<void**>(&this->bucket[idx].rawRef_()), base);
#elif defined(USE_MPS)
    this->bucket[idx] = val;
#elif defined(USE_MMTK)
    THROW_HARD_ERROR("Add support for mmtk");
#endif
  }
};

template <class T, class U> struct Buckets<T, U, StrongLinks> : public BucketsBase<T, U> {
  typedef typename BucketsBase<T, U>::value_type value_type;
  Buckets(int l) : BucketsBase<T, U>(l){};
  Buckets(snapshotSaveLoad::snapshot_save_load_init_s* isl) { isl->fill((void*)this); }
  void set(size_t idx, const value_type& val) {
    this->bucket[idx] = val;
  }
};

typedef gctools::smart_ptr<core::T_O> BucketValueType;
typedef gctools::Buckets<BucketValueType, BucketValueType, gctools::WeakLinks> WeakBucketsObjectType;
typedef gctools::Buckets<BucketValueType, BucketValueType, gctools::StrongLinks> StrongBucketsObjectType;

class WeakKeyHashTable {
  friend class core::WeakKeyHashTable_O;

public:
  typedef BucketValueType value_type;
  typedef WeakBucketsObjectType KeyBucketsType;
  typedef StrongBucketsObjectType ValueBucketsType;

public:
  typedef WeakKeyHashTable MyType;

public:
  typedef gctools::GCBucketAllocator<KeyBucketsType> KeyBucketsAllocatorType;
  typedef gctools::GCBucketAllocator<ValueBucketsType> ValueBucketsAllocatorType;

public:
  size_t _Length;
  core::Number_sp _RehashSize;
  double _RehashThreshold;
  gctools::tagged_pointer<KeyBucketsType> _Keys;     // hash buckets for keys
  gctools::tagged_pointer<ValueBucketsType> _Values; // hash buckets for values
#ifdef CLASP_THREADS
  mutable mp::SharedMutex_sp _Mutex;
#endif
public:
  WeakKeyHashTable(size_t length, core::Number_sp rehashSize, double rehashThreshold)
      : _Length(length), _RehashSize(rehashSize), _RehashThreshold(rehashThreshold){};
  void initialize();

public:
  static uint sxhashKey(const value_type& key);

  /*! Return 0 if there is no more room in the sequence of entries for the key
          Return 1 if the element is found or an unbound or deleted entry is found.
          Return the entry index in (b)
        */
  static size_t find_no_lock(gctools::tagged_pointer<KeyBucketsType> keys, const value_type& key, size_t& b);

public:
  void setupThreadSafeHashTable();
  size_t length() const {
    if (!this->_Keys) {
      throw_hard_error("Keys should never be null");
    }
    return this->_Keys->length();
  }

  void swap(MyType& other) {
    gctools::tagged_pointer<KeyBucketsType> tempKeys = this->_Keys;
    gctools::tagged_pointer<ValueBucketsType> tempValues = this->_Values;
    core::Number_sp rehashSize = this->_RehashSize;
    double rehashThreshold = this->_RehashThreshold;
    this->_Keys = other._Keys;
    this->_Values = other._Values;
    this->_RehashSize = other._RehashSize;
    this->_RehashThreshold = other._RehashThreshold;
    other._Keys = tempKeys;
    other._Values = tempValues;
    other._RehashSize = rehashSize;
    other._RehashThreshold = rehashThreshold;
  }

  bool fullp_not_safe() const {
    bool fp;
    fp = (*this->_Keys).used() >= (this->_RehashThreshold * (*this->_Keys).length());
    return fp;
  }

  bool fullp() const {
    bool fp;
    safeRun([&fp, this]() { fp = (*this->_Keys).used() >= (*this->_Keys).length() / 2; });
    return fp;
  }

  int tableSize() const {
    int result;
    safeRun([&result, this]() {
      size_t used, deleted;
      used = this->_Keys->used();
      deleted = this->_Keys->deleted();
      GCTOOLS_ASSERT(used >= deleted);
      result = used - deleted;
    });
    return result;
  }

  int rehash_not_safe(const value_type& key, size_t& key_bucket);
  int rehash(const value_type& key, size_t& key_bucket);
  int trySet(core::T_sp tkey, core::T_sp value);

  string dump(const string& prefix);

  core::T_mv gethash(core::T_sp tkey, core::T_sp defaultValue);
  void set(core::T_sp key, core::T_sp value);
  void maphash(std::function<void(core::T_sp, core::T_sp)> const& fn);
  void maphashFn(core::T_sp fn);
  bool remhash(core::T_sp tkey);
  void clrhash();
};

core::Vector_sp weak_key_hash_table_pairs(const gctools::WeakKeyHashTable& ht);

// ======================================================================
// ----------------------------------------------------------------------

template <class T, class U> struct MappingBase : public WeakObject {
  MappingBase(const T& val) : bucket(val){};
  typedef T value_type;
  void* dependentPtr() const {
    if (this->dependent)
      return reinterpret_cast<void*>(&*this->dependent);
    return NULL;
  };
  typedef gctools::tagged_pointer<MappingBase<U, T>> dependent_type;
  dependent_type dependent; /* the dependent object */
  T bucket;                 /* single buckets */
};

template <class T, class U, class Link> struct Mapping;

template <class T, class U> struct Mapping<T, U, WeakLinks> : public MappingBase<T, U> {
  typedef typename MappingBase<T, U>::value_type value_type;
  typedef typename MappingBase<T, U>::dependent_type dependent_type;
  Mapping(const T& val) : MappingBase<T, U>(val) {
    if (!val.objectp()) {
      printf("%s:%d Only  objectp() objects can be added to Mapping\n", __FILE__, __LINE__);
      abort();
    }
#if defined(USE_BOEHM)
    GCTOOLS_ASSERT(this->bucket.objectp());
    if (!unboundOrDeletedOrSplatted(this->bucket)) {
      // printf("%s:%d Mapping register disappearing link\n", __FILE__, __LINE__);
      GCTOOLS_ASSERT(val.objectp());
      GC_general_register_disappearing_link(reinterpret_cast<void**>(&this->bucket.rawRef_()),
                                            reinterpret_cast<void*>(this->bucket.rawRef_()));
    }
#else
    THROW_HARD_ERROR("Add support for new GC");
#endif
  };
};

template <class T, class U> struct Mapping<T, U, StrongLinks> : public MappingBase<T, U> {
  typedef typename MappingBase<T, U>::value_type value_type;
  Mapping(const T& val) : MappingBase<T, U>(val){};
};

typedef gctools::smart_ptr<core::T_O> MappingValueType;
typedef gctools::Mapping<BucketValueType, BucketValueType, gctools::WeakLinks> WeakMappingObjectType;
typedef gctools::Mapping<BucketValueType, BucketValueType, gctools::StrongLinks> StrongMappingObjectType;

template <typename FROM>
struct TaggedCast<gctools::BucketsBase<gctools::smart_ptr<core::T_O>, gctools::smart_ptr<core::T_O>>*, FROM> {
  typedef gctools::BucketsBase<gctools::smart_ptr<core::T_O>, gctools::smart_ptr<core::T_O>>* ToType;
  typedef FROM FromType;
  inline static bool isA(FromType tagged_client) {
    if (tagged_generalp(tagged_client)) {
      // Should I have more here?
      return dynamic_cast<ToType>(untag_general(tagged_client)) != NULL;
    }
    return false;
  }
  inline static ToType castOrNULL(FromType client) {
    if (TaggedCast<ToType, FromType>::isA(client))
      return reinterpret_cast<ToType>(client);
    return NULL;
  }
};

}; // namespace gctools
