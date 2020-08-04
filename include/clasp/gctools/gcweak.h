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
#ifndef gctools_gcweak_H
#define gctools_gcweak_H

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
//#define DEBUG_GCWEAK
#ifdef DEBUG_GCWEAK
#define GCWEAK_LOG(x) printf("%s:%d %s\n", __FILE__, __LINE__, (x).str().c_str())
#else
#define GCWEAK_LOG(x)
#endif

#include <functional>

namespace core {
string lisp_rep(T_sp obj);
};

namespace gctools {

#if defined(USE_BOEHM) &&  !defined(CLASP_THREADS)
#define call_with_alloc_lock GC_call_with_alloc_lock
#else
typedef void *(*fn_type)(void *client_data);
inline void call_with_alloc_lock(fn_type fn, void *client_data) {
  fn(client_data);
}

#endif

template <class Proto>
void *wrapRun(void *wrappedFn) {
  std::function<Proto> *fn = reinterpret_cast<std::function<Proto> *>(wrappedFn);
  (*fn)();
  return NULL;
}

template <class Proto>
void safeRun(std::function<Proto> f) {
#ifdef DEBUG_GCWEAK
  printf("Entered safeRun\n");
#endif
  call_with_alloc_lock(wrapRun<Proto>, reinterpret_cast<void *>(&f));
#ifdef DEBUG_GCWEAK
  printf("Leaving safeRun\n");
#endif
};
};

namespace gctools {

typedef enum { WeakBucketKind,
               StrongBucketKind,
               WeakMappingKind,
               StrongMappingKind,
               WeakPointerKind,
               WeakImmediateKind,
               WeakFwdKind,
               WeakFwd2Kind,
               WeakPadKind,
               WeakPad1Kind,
               // Other MPS kinds here
               MaxWeakKind } WeakKinds;

struct WeakObject {
  struct metadata_always_fix_pointers_to_derived_classes;
  typedef gctools::smart_ptr<core::Fixnum_I> KindType;
  WeakObject(WeakKinds k) : Kind(gctools::make_tagged_fixnum<core::Fixnum_I>(k)){};
  KindType Kind;
  int kind() const {
    GCTOOLS_ASSERT(this->Kind.unsafe_fixnum() < MaxWeakKind);
    return (int)this->Kind.unsafe_fixnum();
  };
  void setKind(WeakKinds k) { this->Kind = gc::make_tagged_fixnum<core::Fixnum_I>(k); };
  virtual void *dependentPtr() const { return NULL; };
};

struct weak_fwd_s : public WeakObject {
  WeakObject *fwd;                         /* forwarded object */
  gctools::smart_ptr<core::Fixnum_I> size; /* total size of this object */
};

struct weak_fwd2_s : public WeakObject {
  WeakObject *fwd; /* forwarded object */
};

struct weak_pad_s : public WeakObject {
//  WeakObject *fwd;                         /* forwarded object */  /*WHY!!*//
  gctools::smart_ptr<core::Fixnum_I> size; /* total size of this object */
};

struct weak_pad1_s : public WeakObject {
};

template <class T, class U>
struct BucketsBase : public WeakObject {
  BucketsBase(WeakKinds k, int l) : WeakObject(k), _length(gctools::make_tagged_fixnum<core::Fixnum_I>(l)), _used(gctools::make_tagged_fixnum<core::Fixnum_I>(0)), _deleted(gctools::make_tagged_fixnum<core::Fixnum_I>(0)) {
    GCWEAK_LOG(BF("Created BucketsBase with length: %d") % this->length());
    for (size_t i(0); i < l; ++i) {
      this->bucket[i] = T((gctools::Tagged)gctools::tag_unbound<typename T::Type *>());
    }
  }

  virtual ~BucketsBase(){};

  T &operator[](size_t idx) { return this->bucket[idx]; };
  typedef T value_type;
  typedef gctools::tagged_pointer<BucketsBase<U, T>> dependent_type;
  dependent_type dependent;                    /* the dependent object */
  gctools::smart_ptr<core::Fixnum_I> _length;  /* number of buckets (tagged) */
  gctools::smart_ptr<core::Fixnum_I> _used;    /* number of buckets in use (tagged) */
  gctools::smart_ptr<core::Fixnum_I> _deleted; /* number of deleted buckets (tagged) */
  T bucket[0];                                 /* hash buckets */

  void *dependentPtr() const {
    if (this->dependent)
      return reinterpret_cast<void *>(&*this->dependent);
    return NULL;
  };

  int length() const {
    GCTOOLS_ASSERT(this->_length.fixnump());
    return this->_length.unsafe_fixnum();
  };
  void setLength(int l) { this->_length = gctools::make_tagged_fixnum<core::Fixnum_I>(l); };
  int used() const {
    GCTOOLS_ASSERT(this->_used.fixnump());
    return this->_used.unsafe_fixnum();
  };
  void setUsed(int val) { this->_used = gctools::make_tagged_fixnum<core::Fixnum_I>(val); };
  int deleted() const {
    GCTOOLS_ASSERT(this->_deleted.fixnump());
    return this->_deleted.unsafe_fixnum();
  };
  void setDeleted(int val) { this->_deleted = gctools::make_tagged_fixnum<core::Fixnum_I>(val); };
};

template <class T, class U, class Link>
struct Buckets;

#ifdef USE_BOEHM
inline bool unboundOrDeletedOrSplatted(core::T_sp bucket) {
  return (bucket.unboundp()    // unbound
          || bucket.deletedp() // deleted
          || !bucket           // splatted by Boehm
          );
}
#endif

template <class T, class U>
struct Buckets<T, U, WeakLinks> : public BucketsBase<T, U> {
  typedef typename BucketsBase<T, U>::value_type value_type;
  Buckets(int l) : BucketsBase<T, U>(WeakBucketKind, l){};
  virtual ~Buckets() {
#ifdef USE_BOEHM
    for (size_t i(0), iEnd(this->length()); i < iEnd; ++i) {
      if (!unboundOrDeletedOrSplatted(this->bucket[i])) {
        //		    printf("%s:%d Buckets dtor idx: %zu unregister disappearing link @%p\n", __FILE__, __LINE__, i, &this->bucket[i].rawRef_());
        int result = GC_unregister_disappearing_link(reinterpret_cast<void **>(&this->bucket[i].rawRef_()));
        if (!result) {
          printf("%s:%d The link was not registered as a disappearing link!", __FILE__, __LINE__);
          abort();
        }
      }
    }
#endif
  }

  void set(size_t idx, const value_type &val) {
    if (!val.objectp()) {
      printf("%s:%d Only  objectp() objects can be added to Mapping\n", __FILE__, __LINE__);
      abort();
    }
#ifdef USE_BOEHM
    //	    printf("%s:%d ---- Buckets set idx: %zu   this->bucket[idx] = %p\n", __FILE__, __LINE__, idx, this->bucket[idx].raw_() );
    if (!unboundOrDeletedOrSplatted(this->bucket[idx])) {
      auto &rawRef = this->bucket[idx].rawRef_();
      void **linkAddress = reinterpret_cast<void **>(&rawRef);
      //		printf("%s:%d Buckets set idx: %zu unregister disappearing link @%p\n", __FILE__, __LINE__, idx, linkAddress );
      int result = GC_unregister_disappearing_link(linkAddress); //reinterpret_cast<void**>(&this->bucket[idx].rawRef_()));
      if (!result) {
        throw_hard_error("The link was not registered as a disappearing link!");
      }
    }
    if (!unboundOrDeletedOrSplatted(val)) {
      this->bucket[idx] = val;
      //		printf("%s:%d Buckets set idx: %zu register disappearing link @%p\n", __FILE__, __LINE__, idx, &this->bucket[idx].rawRef_());
      GCTOOLS_ASSERT(val.objectp());
      GC_general_register_disappearing_link(reinterpret_cast<void **>(&this->bucket[idx].rawRef_()), reinterpret_cast<void *>(this->bucket[idx].rawRef_()));
    } else {
      this->bucket[idx] = val;
    }
#endif
#ifdef USE_MPS
    GCWEAK_LOG(BF("Setting Buckets<T,U,WeakLinks> idx=%d  address=%p") % idx % ((void *)(val.raw_())));
    this->bucket[idx] = val;
#endif
  }
};

template <class T, class U>
struct Buckets<T, U, StrongLinks> : public BucketsBase<T, U> {
  typedef typename BucketsBase<T, U>::value_type value_type;
  Buckets(int l) : BucketsBase<T, U>(StrongBucketKind, l){};
  virtual ~Buckets() {}
  void set(size_t idx, const value_type &val) {
    GCWEAK_LOG(BF("Setting Buckets<T,U,StrongLinks> idx=%d  address=%p") % idx % ((void *)(val.raw_())));
    this->bucket[idx] = val;
  }
};

#ifdef USE_BACKCASTABLE_POINTERS
typedef gctools::tagged_backcastable_base_ptr<core::T_O> BucketValueType;
#else
typedef gctools::smart_ptr<core::T_O> BucketValueType;
#endif
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
  core::Number_sp _RehashSize;
  double _RehashThreshold;
  size_t _Length;
  gctools::tagged_pointer<KeyBucketsType> _Keys;     // hash buckets for keys
  gctools::tagged_pointer<ValueBucketsType> _Values; // hash buckets for values
#ifdef CLASP_THREADS
    mutable mp::SharedMutex_sp _Mutex;
#endif
public:
  WeakKeyHashTable(size_t length, core::Number_sp rehashSize, double rehashThreshold) : _Length(length), _RehashSize(rehashSize), _RehashThreshold(rehashThreshold) {};
  void initialize();
public:
  static uint sxhashKey(const value_type &key);

  /*! Return 0 if there is no more room in the sequence of entries for the key
	  Return 1 if the element is found or an unbound or deleted entry is found.
	  Return the entry index in (b)
	*/
  static size_t find_no_lock(gctools::tagged_pointer<KeyBucketsType> keys, const value_type &key, size_t &b
#ifdef DEBUG_FIND
                  ,
                  bool debugFind = false, stringstream *reportP = NULL
#endif
                  );

public:
  void setupThreadSafeHashTable();
  size_t length() const {
    if (!this->_Keys) {
      throw_hard_error("Keys should never be null");
    }
    return this->_Keys->length();
  }

  void swap(MyType &other) {
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
    safeRun<void()>([&fp, this]() -> void {
                    fp = (*this->_Keys).used() >= (*this->_Keys).length()/2;
    });
    return fp;
  }

  int tableSize() const {
    int result;
    safeRun<void()>([&result, this]() -> void {
                    size_t used, deleted;
                    used = this->_Keys->used();
                    deleted = this->_Keys->deleted();
                    GCTOOLS_ASSERT(used >= deleted);
                    result = used - deleted;
    });
    return result;
  }

  int rehash_not_safe( const value_type &key, size_t &key_bucket);
  int rehash(const value_type &key, size_t &key_bucket);
  int trySet(core::T_sp tkey, core::T_sp value);

  string dump(const string &prefix);

  core::T_mv gethash(core::T_sp tkey, core::T_sp defaultValue);
  void set(core::T_sp key, core::T_sp value);
  void maphash(std::function<void(core::T_sp, core::T_sp)> const &fn);
  void maphashFn(core::T_sp fn);
  bool remhash(core::T_sp tkey);
  void clrhash();
};


 core::Vector_sp weak_key_hash_table_pairs(const WeakKeyHashTable& ht);


class StrongKeyHashTable {
  friend class core::StrongKeyHashTable_O;

public:
  typedef BucketValueType value_type;
  typedef StrongBucketsObjectType KeyBucketsType;
  typedef StrongBucketsObjectType ValueBucketsType;

public:
  typedef StrongKeyHashTable MyType;

public:
  typedef gctools::GCBucketAllocator<KeyBucketsType> KeyBucketsAllocatorType;
  typedef gctools::GCBucketAllocator<ValueBucketsType> ValueBucketsAllocatorType;

public:
  size_t    _Rehashes;
  size_t    _Length;
  gctools::tagged_pointer<KeyBucketsType> _Keys;     // hash buckets for keys
  gctools::tagged_pointer<ValueBucketsType> _Values; // hash buckets for values
public:
  StrongKeyHashTable(size_t length) : _Rehashes(0), _Length(length) {};
  void initialize();
public:
  static uint sxhashKey(const value_type &key);
  /*! Return 0 if there is no more room in the sequence of entries for the key
	  Return 1 if the element is found or an unbound or deleted entry is found.
	  Return the entry index in (b)
	*/
  static size_t find_no_lock(gctools::tagged_pointer<KeyBucketsType> keys, const value_type &key , size_t &b
#ifdef DEBUG_FIND
                  ,
                  bool debugFind = false, stringstream *reportP = NULL
#endif
                  );

public:
  size_t length() const {
    if (!this->_Keys) {
      throw_hard_error("Keys should never be null");
    }
    return this->_Keys->length();
  }

  void swap(MyType &other) {
    gctools::tagged_pointer<KeyBucketsType> tempKeys = this->_Keys;
    gctools::tagged_pointer<ValueBucketsType> tempValues = this->_Values;
    this->_Keys = other._Keys;
    this->_Values = other._Values;
    other._Keys = tempKeys;
    other._Values = tempValues;
  }

  bool fullp_not_safe() const {
    bool fp;
    fp = (*this->_Keys).used() >= (*this->_Keys).length()/2;
    return fp;
  }

  bool fullp() const {
    bool fp;
    safeRun<void()>([&fp, this]() -> void {
                    fp = (*this->_Keys).used() >= (*this->_Keys).length()/2;
    });
    return fp;
  }

  size_t tableSize() const {
    size_t result;
    safeRun<void()>([&result, this]() -> void {
                    size_t used, deleted;
                    used = this->_Keys->used();
                    deleted = this->_Keys->deleted();
                    GCTOOLS_ASSERT(used >= deleted);
                    result = used - deleted;
    });
    return result;
  }

  size_t rehash_not_safe(size_t newLength, const value_type &key, size_t &key_bucket);
  size_t rehash(size_t newLength, const value_type &key, size_t &key_bucket);
  int trySet(core::T_sp tkey, core::T_sp value);

  string dump(const string &prefix);

  core::T_mv gethash(core::T_sp tkey, core::T_sp defaultValue);
  void set(core::T_sp key, core::T_sp value);
  void maphash(std::function<void(core::T_sp, core::T_sp)> const &fn);
  core::T_mv maphashFn(core::T_sp fn);
  bool remhash(core::T_sp tkey);
  void clrhash();
};

// ======================================================================
// ----------------------------------------------------------------------

template <class T, class U>
struct MappingBase : public WeakObject {
  MappingBase(const T &val) : WeakObject(WeakMappingKind), bucket(val){};
  virtual ~MappingBase(){};
  typedef T value_type;
  void *dependentPtr() const {
    if (this->dependent)
      return reinterpret_cast<void *>(&*this->dependent);
    return NULL;
  };
  typedef gctools::tagged_pointer<MappingBase<U, T>> dependent_type;
  dependent_type dependent; /* the dependent object */
  T bucket;                 /* single buckets */
};

template <class T, class U, class Link>
struct Mapping;

template <class T, class U>
struct Mapping<T, U, WeakLinks> : public MappingBase<T, U> {
  typedef typename MappingBase<T, U>::value_type value_type;
  typedef typename MappingBase<T, U>::dependent_type dependent_type;
  Mapping(const T &val) : MappingBase<T, U>(val) {
    if (!val.objectp()) {
      printf("%s:%d Only  objectp() objects can be added to Mapping\n", __FILE__, __LINE__);
      abort();
    }
#ifdef USE_BOEHM
    GCTOOLS_ASSERT(this->bucket.objectp());
    if (!unboundOrDeletedOrSplatted(this->bucket)) {
      // printf("%s:%d Mapping register disappearing link\n", __FILE__, __LINE__);
      GCTOOLS_ASSERT(val.objectp());
      GC_general_register_disappearing_link(reinterpret_cast<void **>(&this->bucket.rawRef_()), reinterpret_cast<void *>(this->bucket.rawRef_()));
    }
#endif
  };


#pragma clang diagnostic push
#pragma clang diagnostic ignored "-Wexceptions"
  virtual ~Mapping() {
#ifdef USE_BOEHM
    GCTOOLS_ASSERT(this->bucket.objectp());
    if (!unboundOrDeletedOrSplatted(this->bucket)) {
      // printf("%s:%d Mapping unregister disappearing link\n", __FILE__, __LINE__);
      int result = GC_unregister_disappearing_link(reinterpret_cast<void **>(&this->bucket.rawRef_()));
      if (!result) {
        printf("%s:%d The link was not registered as a disappearing link!", __FILE__, __LINE__);
        abort();
      }
    }
#endif
  }
#pragma clang diagnostic pop
  
};

template <class T, class U>
struct Mapping<T, U, StrongLinks> : public MappingBase<T, U> {
  typedef typename MappingBase<T, U>::value_type value_type;
  Mapping(const T &val) : MappingBase<T, U>(val){};
  virtual ~Mapping() {}
};

#ifdef USE_BACKCASTABLE_POINTERS
typedef gctools::tagged_backcastable_base_ptr<core::T_O> MappingValueType;
#else
typedef gctools::smart_ptr<core::T_O> MappingValueType;
#endif
typedef gctools::Mapping<BucketValueType, BucketValueType, gctools::WeakLinks> WeakMappingObjectType;
typedef gctools::Mapping<BucketValueType, BucketValueType, gctools::StrongLinks> StrongMappingObjectType;

struct WeakPointer : public WeakObject {
#ifdef USE_BACKCASTABLE_POINTERS
  typedef gctools::tagged_backcastable_base_ptr<core::T_O> value_type;
#else
  typedef gctools::smart_ptr<core::T_O> value_type;
#endif

  WeakPointer(const value_type &val) : WeakObject(WeakPointerKind), value(val){};
  
  value_type value;
};

struct WeakPointerManager {
  typedef typename gctools::WeakPointer::value_type value_type;
  typedef WeakPointerManager MyType;
  typedef gctools::GCWeakPointerAllocator<WeakPointer> AllocatorType;

  WeakPointerManager(const value_type &val) {
    if (!val.objectp()) {
      printf("%s:%d WeakPointerManager - only  objectp() objects can be added to Mapping\n", __FILE__, __LINE__);
      abort();
    }
#if 0
    this->pointer = AllocatorType::allocate(val);
#ifdef USE_BOEHM
    GCTOOLS_ASSERT(this->pointer->value.objectp());
    if (!unboundOrDeletedOrSplatted(this->pointer->value)) {
      GCTOOLS_ASSERT(val.objectp());
      GC_general_register_disappearing_link(reinterpret_cast<void **>(&this->pointer->value.rawRef_()), reinterpret_cast<void *>(this->pointer->value.rawRef_()));
    } else {
      GCTOOLS_ASSERT(false); // ERROR("value can never contain anything but a pointer - if it does then when it gets set to NULL by the BoehmGC it will be interpreted as a Fixnum 0!!!!!");
    }
#endif
#endif
  }
#pragma clang diagnostic push
#pragma clang diagnostic ignored "-Wexceptions"
  virtual ~WeakPointerManager() {
#ifdef USE_BOEHM
    GCTOOLS_ASSERT(this->pointer->value.objectp());
    if (!unboundOrDeletedOrSplatted(this->pointer->value)) {
      int result = GC_unregister_disappearing_link(reinterpret_cast<void **>(&this->pointer->value.rawRef_()));
      if (!result) {
        printf("%s:%d The link was not registered as a disappearing link!", __FILE__, __LINE__);
        abort();
      }
    }
#endif
  };
#pragma clang diagnostic pop

  // This will need to be a tagged_backcastable_base_ptr
  gctools::tagged_pointer<WeakPointer> pointer;
  core::T_mv value() const {
    core::T_mv result_mv;
    safeRun<void()>([&result_mv, this]() -> void {
                    if ((bool)this->pointer->value) {
                        result_mv = Values(gctools::smart_ptr<core::T_O>(this->pointer->value),core::lisp_true());
                        return;
                    }
                    result_mv = Values(_Nil<core::T_O>(),_Nil<core::T_O>());
    });
    return result_mv;
  }

  bool valid() const {
    bool result;
    safeRun<void()>([&result, this]() -> void {
                    result = (bool)this->pointer->value;
    });
    return result;
  }
};




 template <typename FROM>
struct TaggedCast<gctools::BucketsBase<gctools::smart_ptr<core::T_O>, gctools::smart_ptr<core::T_O>> *, FROM> {
  typedef gctools::BucketsBase<gctools::smart_ptr<core::T_O>, gctools::smart_ptr<core::T_O>> *ToType;
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

};

#ifdef USE_MPS
extern "C" {

mps_res_t weak_obj_scan(mps_ss_t ss, mps_addr_t base, mps_addr_t limit);
mps_addr_t weak_obj_skip(mps_addr_t base);
void weak_obj_fwd(mps_addr_t old, mps_addr_t newv);
mps_addr_t weak_obj_isfwd(mps_addr_t addr);
void weak_obj_pad(mps_addr_t addr, size_t size);
};
#endif

#endif // gctools_gcweak_H
