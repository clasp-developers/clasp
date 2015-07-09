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

#ifdef DEBUG_GCWEAK
#define GCWEAK_LOG(x) printf("%s:%d %s\n", __FILE__, __LINE__, (x).str().c_str())
#else
#define GCWEAK_LOG(x)
#endif

namespace core {
string lisp_rep(T_sp obj);
};

namespace gctools {

#ifdef USE_BOEHM
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
               WeakFwdKind,
               WeakFwd2Kind,
               WeakPadKind,
               WeakPad1Kind
               /*Other MPS kinds here */ } WeakKinds;

struct WeakObject {
  struct metadata_always_fix_pointers_to_derived_classes;
  typedef gctools::tagged_ptr<gctools::Fixnum_ty> KindType;
  WeakObject(WeakKinds k) : Kind(gctools::tagged_ptr<gctools::Fixnum_ty>(k)){};
  KindType Kind;
  int kind() const { return this->Kind.fixnum(); };
  void setKind(WeakKinds k) { this->Kind = gctools::tagged_ptr<gctools::Fixnum_ty>(k); };
  virtual void *dependentPtr() const { return NULL; };
};

struct weak_fwd_s : public WeakObject {
  WeakObject *fwd;                              /* forwarded object */
  gctools::tagged_ptr<gctools::Fixnum_ty> size; /* total size of this object */
};

struct weak_fwd2_s : public WeakObject {
  WeakObject *fwd; /* forwarded object */
};

struct weak_pad_s : public WeakObject {
  WeakObject *fwd;                              /* forwarded object */
  gctools::tagged_ptr<gctools::Fixnum_ty> size; /* total size of this object */
};

struct weak_pad1_s : public WeakObject {
};

template <class T, class U>
struct BucketsBase : public WeakObject {
  BucketsBase(WeakKinds k, int l) : WeakObject(k), dependent(NULL), _length(gctools::tagged_ptr<gctools::Fixnum_ty>(l)), _used(gctools::tagged_ptr<gctools::Fixnum_ty>(0)), _deleted(gctools::tagged_ptr<gctools::Fixnum_ty>(0)) {
    GCWEAK_LOG(BF("Created BucketsBase with length: %d") % this->length());
    for (size_t i(0); i < l; ++i)
      this->bucket[i] = T(T::unbound);
  }

  virtual ~BucketsBase(){};

  void *dependentPtr() const { return reinterpret_cast<void *>(this->dependent); };

  T &operator[](size_t idx) { return this->bucket[idx]; };
  typedef T value_type;
  BucketsBase<U, T> *dependent;                     /* the dependent object */
  gctools::tagged_ptr<gctools::Fixnum_ty> _length;  /* number of buckets (tagged) */
  gctools::tagged_ptr<gctools::Fixnum_ty> _used;    /* number of buckets in use (tagged) */
  gctools::tagged_ptr<gctools::Fixnum_ty> _deleted; /* number of deleted buckets (tagged) */
  T bucket[0];                                      /* hash buckets */

  int length() const { return this->_length.fixnum(); };
  void setLength(int l) { this->_length = gctools::tagged_ptr<gctools::Fixnum_ty>(l); };
  int used() const { return this->_used.fixnum(); };
  void setUsed(int val) { this->_used = gctools::tagged_ptr<gctools::Fixnum_ty>(val); };
  int deleted() const { return this->_deleted.fixnum(); };
  void setDeleted(int val) { this->_deleted = gctools::tagged_ptr<gctools::Fixnum_ty>(val); };
};

template <class T, class U, class Link>
struct Buckets;

template <class T, class U>
struct Buckets<T, U, WeakLinks> : public BucketsBase<T, U> {
  typedef typename BucketsBase<T, U>::value_type value_type;
  Buckets(int l) : BucketsBase<T, U>(WeakBucketKind, l){};
  virtual ~Buckets() {
#ifdef USE_BOEHM
    for (size_t i(0), iEnd(this->length()); i < iEnd; ++i) {
      if (this->bucket[i].pointerp()) {
        int result = GC_unregister_disappearing_link(reinterpret_cast<void **>(&this->bucket[i].px_ref()));
        if (!result) {
          THROW_HARD_ERROR(BF("The link was not registered as a disappearing link!"));
        }
      }
    }
#endif
  }

  void set(size_t idx, const value_type &val) {
#ifdef USE_BOEHM
    if (this->bucket[idx].pointerp()) {
      int result = GC_unregister_disappearing_link(reinterpret_cast<void **>(&this->bucket[idx].px_ref()));
      if (!result) {
        THROW_HARD_ERROR(BF("The link was not registered as a disappearing link!"));
      }
    }
    if (val.pointerp()) {
      this->bucket[idx] = val;
      GC_general_register_disappearing_link(reinterpret_cast<void **>(&this->bucket[idx].px_ref()), reinterpret_cast<void *>(this->bucket[idx].px_ref()));
    } else {
      this->bucket[idx] = val;
    }
#endif
#ifdef USE_MPS
    GCWEAK_LOG(BF("Setting Buckets<T,U,WeakLinks> idx=%d  address=%p") % idx % ((void *)(val.px)));
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
    GCWEAK_LOG(BF("Setting Buckets<T,U,StrongLinks> idx=%d  address=%p") % idx % ((void *)(val.px)));
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

class WeakHashTable {
  friend class core::WeakKeyHashTable_O;

public:
  typedef BucketValueType value_type;
  typedef WeakBucketsObjectType KeyBucketsType;
  typedef StrongBucketsObjectType ValueBucketsType;

public:
  typedef WeakHashTable MyType;

public:
  typedef gctools::GCBucketAllocator<KeyBucketsType> KeyBucketsAllocatorType;
  typedef gctools::GCBucketAllocator<ValueBucketsType> ValueBucketsAllocatorType;

public:
  KeyBucketsType *_Keys;     // hash buckets for keys
  ValueBucketsType *_Values; // hash buckets for values
#ifdef USE_MPS
  mps_ld_s _LocationDependency;
#endif

public:
  WeakHashTable(size_t length = 0);

public:
  static uint sxhashKey(const value_type &key
#ifdef USE_MPS
                        ,
                        mps_ld_s *locationDependencyP
#endif
                        );

  /*! Return 0 if there is no more room in the sequence of entries for the key
	  Return 1 if the element is found or an unbound or deleted entry is found.
	  Return the entry index in (b)
	*/
  static int find(KeyBucketsType *keys, const value_type &key
#ifdef USE_MPS
                  ,
                  mps_ld_s *ldP
#endif
                  ,
                  size_t &b
#ifdef DEBUG_FIND
                  ,
                  bool debugFind = false, stringstream *reportP = NULL
#endif
                  );

public:
  size_t length() const {
    if (this->_Keys == NULL) {
      THROW_HARD_ERROR(BF("Keys should never be null"));
    }
    return this->_Keys->length();
  }

  void swap(MyType &other) {
    KeyBucketsType *tempKeys = this->_Keys;
    ;
    ValueBucketsType *tempValues = this->_Values;
    this->_Keys = other._Keys;
    this->_Values = other._Values;
    other._Keys = tempKeys;
    other._Values = tempValues;
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

  int rehash(size_t newLength, const value_type &key, size_t &key_bucket);
  int trySet(core::T_sp tkey, core::T_sp value);

  string dump(const string &prefix);

  core::T_mv gethash(core::T_sp tkey, core::T_sp defaultValue);
  void set(core::T_sp key, core::T_sp value);
  void maphash(std::function<void(core::T_sp, core::T_sp)> const &fn);
  void remhash(core::T_sp tkey);
  void clrhash();
};

// ======================================================================
// ----------------------------------------------------------------------

template <class T, class U>
struct MappingBase : public WeakObject {
  MappingBase(const T &val) : WeakObject(WeakMappingKind), dependent(NULL), bucket(val){};
  virtual ~MappingBase(){};
  typedef T value_type;
  void *dependentPtr() const { return reinterpret_cast<void *>(this->dependent); };
  MappingBase<U, T> *dependent; /* the dependent object */
  T bucket;                     /* single buckets */
};

template <class T, class U, class Link>
struct Mapping;

template <class T, class U>
struct Mapping<T, U, WeakLinks> : public MappingBase<T, U> {
  typedef typename MappingBase<T, U>::value_type value_type;
  Mapping(const T &val) : MappingBase<T, U>(val) {
#ifdef USE_BOEHM
    if (this->bucket.pointerp()) {
      printf("%s:%d Mapping register disappearing link\n", __FILE__, __LINE__);
      GC_general_register_disappearing_link(reinterpret_cast<void **>(&this->bucket.px_ref()), reinterpret_cast<void *>(this->bucket.px_ref()));
    }
#endif
  };
  virtual ~Mapping() {
#ifdef USE_BOEHM
    if (this->bucket.pointerp()) {
      printf("%s:%d Mapping unregister disappearing link\n", __FILE__, __LINE__);
      int result = GC_unregister_disappearing_link(reinterpret_cast<void **>(&this->bucket.px_ref()));
      if (!result) {
        THROW_HARD_ERROR(BF("The link was not registered as a disappearing link!"));
      }
    }
#endif
  }
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

class WeakKeyMappingPair {
  friend class core::WeakKeyMapping_O;

protected:
  typedef MappingValueType value_type;
  typedef WeakMappingObjectType KeyType;
  typedef StrongMappingObjectType ValueType;
  typedef WeakKeyMappingPair MyType;
  typedef gctools::GCMappingAllocator<KeyType> KeyAllocatorType;
  typedef gctools::GCMappingAllocator<ValueType> ValueAllocatorType;

public:
  KeyType *Key;     // hash buckets for keys
  ValueType *Value; // hash buckets for values
public:
  WeakKeyMappingPair(const value_type &key, const value_type &value) {
    this->Key = KeyAllocatorType::allocate(key);
    this->Value = ValueAllocatorType::allocate(value);
    this->Key->dependent = this->Value;
    GCTOOLS_ASSERT((reinterpret_cast<uintptr_t>(this->Key->dependent) & 0x3) == 0);
    this->Value->dependent = this->Key;
    GCTOOLS_ASSERT((reinterpret_cast<uintptr_t>(this->Value->dependent) & 0x3) == 0);
  }
  void swap(MyType &other) {
    KeyType *tempKey = this->Key;
    ;
    ValueType *tempValue = this->Value;
    this->Key = other.Key;
    this->Value = other.Value;
    other.Key = tempKey;
    other.Value = tempValue;
  }

  bool unsafeValid() const {
    return !this->Key->bucket.NULLp() && !this->Key->bucket.unboundp();
  }
  bool valid() const {
    bool result;
    safeRun<void()>([&result, this]() -> void {
                    result = this->unsafeValid();
    });
    return result;
  };

  /*! Return (values key value t) or (values nil nil nil) */
  core::T_mv keyValue() const {
    core::T_mv result_mv;
    safeRun<void()>([&result_mv, this]() -> void {   
            
                    if (!this->unsafeValid()) {
                        result_mv = Values(_Nil<core::T_O>(),_Nil<core::T_O>(),_Nil<core::T_O>());
                        return;
                    }
                    value_type& key_ref = this->Key->bucket;
                    value_type& value_ref = this->Value->bucket;
                    core::T_sp key(key_ref);
                    core::T_sp value;
                    if ( value_ref.sameAsKeyP() ) {
                        value = key;
                    } else { 
                        value = smart_ptr<core::T_O>(value_ref);
                    }
                    result_mv = Values(key,value,core::lisp_true());
                    return;
    });
    return result_mv;
  };
};

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
    this->pointer = AllocatorType::allocate(val);
#ifdef USE_BOEHM
    if (this->pointer->value.pointerp()) {
      GC_general_register_disappearing_link(reinterpret_cast<void **>(&this->pointer->value.px_ref()), reinterpret_cast<void *>(this->pointer->value.px_ref()));
    }
#endif
  }
  virtual ~WeakPointerManager() {
#ifdef USE_BOEHM
    if (this->pointer->value.pointerp()) {
      int result = GC_unregister_disappearing_link(reinterpret_cast<void **>(&this->pointer->value.px_ref()));
      if (!result) {
        THROW_HARD_ERROR(BF("The link was not registered as a disappearing link!"));
      }
    }
#endif
  };

  // This will need to be a tagged_backcastable_base_ptr
  WeakPointer *pointer;
  core::T_mv value() const {
    core::T_mv result_mv;
    safeRun<void()>([&result_mv, this]() -> void {
                    if (!this->pointer->value.NULLp()) {
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
                    result = !this->pointer->value.NULLp();
    });
    return result;
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
