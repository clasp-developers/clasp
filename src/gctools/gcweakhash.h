#ifndef gctools_gcweakhash_H
#define gctools_gcweakhash_H

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


namespace gctools {

    typedef enum { WeakBucketKind, WeakPointerKind, WeakMappingKind } WeakKinds;

    struct WeakObject {
        typedef gctools::tagged_ptr<gctools::Fixnum_ty> KindType;
        WeakObject(WeakKinds k) : Kind(gctools::tagged_ptr<gctools::Fixnum_ty>(k)) {};
        KindType Kind;
    };


    template <class T,class U>
    struct BucketsBase : public WeakObject {
        BucketsBase(int l) : WeakObject(WeakBucketKind)
                           , _length(l)
                           , _used(gctools::tagged_ptr<gctools::Fixnum_ty>(0))
                           , _deleted(gctools::tagged_ptr<gctools::Fixnum_ty>(0))
        {
            for (size_t i(0); i<l; ++i ) this->bucket[i] = T(T::unbound);
        }

        virtual ~BucketsBase() {};

        T& operator[](size_t idx) { return this->bucket[idx];};
        typedef T   value_type;
        BucketsBase<U,T>* dependent;  /* the dependent object */
        gctools::tagged_ptr<gctools::Fixnum_ty> _length;                /* number of buckets (tagged) */
        gctools::tagged_ptr<gctools::Fixnum_ty> _used;                  /* number of buckets in use (tagged) */
        gctools::tagged_ptr<gctools::Fixnum_ty> _deleted;               /* number of deleted buckets (tagged) */
        T bucket[0];              /* hash buckets */

        int length() const { return this->_length.fixnum(); };
        int used() const { return this->_used.fixnum(); };
        void setUsed(int val) { this->_used = gctools::tagged_ptr<gctools::Fixnum_ty>(val); };
        int deleted() const { return this->_deleted.fixnum(); };
        void setDeleted(int val) { this->_deleted = gctools::tagged_ptr<gctools::Fixnum_ty>(val); };
    };


    template <class T, class U, class Link>
    struct Buckets;

    template <class T,class U>
    struct Buckets<T,U,WeakLinks> : public BucketsBase<T,U> {
        typedef typename BucketsBase<T,U>::value_type value_type;
        Buckets(int l) : BucketsBase<T,U>(l) {};
        virtual ~Buckets() {
#ifdef USE_BOEHM
            for (size_t i(0),iEnd(this->length()); i<iEnd; ++i ) {
                if (this->bucket[i].pointerp()) {
                    int result = GC_unregister_disappearing_link(reinterpret_cast<void**>(&this->bucket[i].base_ref().px_ref()));
                    if ( !result ) {
                        THROW_HARD_ERROR(BF("The link was not registered as a disappearing link!"));
                    }
                }
            }
#endif
        }

        void set(size_t idx, const value_type& val) {
#ifdef USE_BOEHM
            if (this->bucket[idx].pointerp()) {
                int result = GC_unregister_disappearing_link(reinterpret_cast<void**>(&this->bucket[idx].base_ref().px_ref()));
                if (!result) {
                    THROW_HARD_ERROR(BF("The link was not registered as a disappearing link!"));
                }
            }
            if (val.pointerp()) {
                this->bucket[idx] = val;
                GC_general_register_disappearing_link(reinterpret_cast<void**>(&this->bucket[idx].base_ref().px_ref())
                                                      ,reinterpret_cast<void*>(this->bucket[idx].base_ref().px_ref()));
            } else {
                this->bucket[idx] = val;
            }
#endif
#ifdef USE_MPS
            this->bucket[idx] = val;
#endif
        }
    };


    template <class T,class U>
    struct Buckets<T,U,StrongLinks> : public BucketsBase<T,U> {
        typedef typename BucketsBase<T,U>::value_type value_type;
        Buckets(int l) : BucketsBase<T,U>(l) {};
        virtual ~Buckets() {}
        void set(size_t idx, const value_type& val) {
            this->bucket[idx] = val;
        }
    };


    template <class KeyBucketsType, class ValueBucketsType >
    class WeakHashTable {
        friend class core::WeakKeyHashTable_O;
    protected:
        typedef WeakHashTable<KeyBucketsType,ValueBucketsType> MyType;
        typedef gctools::GCBucketAllocator<KeyBucketsType> KeyBucketsAllocatorType;
        typedef gctools::GCBucketAllocator<ValueBucketsType> ValueBucketsAllocatorType;
    public:
        KeyBucketsType*           Keys;           // hash buckets for keys
        ValueBucketsType*         Values;         // hash buckets for values
    public:
        WeakHashTable(size_t length=0)
        {
            /* round up to next power of 2 */
            if ( length == 0 ) length = 2;
            size_t l;
            for( l = 1; l < length; l *= 2);
            this->Keys = KeyBucketsAllocatorType::allocate(l);
            this->Values = ValueBucketsAllocatorType::allocate(l);
            this->Keys->dependent = this->Values;
            this->Values->dependent = this->Keys;
#ifdef USE_MPS
            mps_ld_reset(&this->ld, _global_arena);
#endif
        }

        size_t length() const {
            if ( this->Keys==NULL ) {
                THROW_HARD_ERROR(BF("Keys should never be null"));
            }
            return this->Keys->length();
        }

        void swap(MyType& other)
        {
            KeyBucketsType* tempKeys = this->Keys;;
            ValueBucketsType* tempValues = this->Values;
            this->Keys = other.Keys;
            this->Values = other.Values;
            other.Keys = tempKeys;
            other.Values = tempValues;
        }
            
    };




// ======================================================================
// ----------------------------------------------------------------------


    template <class T,class U>
    struct MappingBase : public WeakObject {
        MappingBase(int l) : WeakObject(WeakMappingKind), bucket(T(T::unbound)) {};
        virtual ~MappingBase() {};
        typedef T   value_type;
        MappingBase<U,T>* dependent;  /* the dependent object */
        T bucket;              /* single buckets */
    };


    template <class T, class U, class Link>
    struct Mapping;

    template <class T,class U>
    struct Mapping<T,U,WeakLinks> : public MappingBase<T,U> {
        typedef typename MappingBase<T,U>::value_type value_type;
        Mapping() : MappingBase<T,U>() {};
        virtual ~Mapping() {
#ifdef USE_BOEHM
            if (this->bucket.pointerp()) {
                int result = GC_unregister_disappearing_link(reinterpret_cast<void**>(&this->bucket.base_ref().px_ref()));
                if ( !result ) {
                    THROW_HARD_ERROR(BF("The link was not registered as a disappearing link!"));
                }
            }
#endif
        }

        void set(const value_type& val) {
#ifdef USE_BOEHM
            if (this->bucket.pointerp()) {
                int result = GC_unregister_disappearing_link(reinterpret_cast<void**>(&this->bucket.base_ref().px_ref()));
                if (!result) {
                    THROW_HARD_ERROR(BF("The link was not registered as a disappearing link!"));
                }
            }
            if (val.pointerp()) {
                this->bucket = val;
                GC_general_register_disappearing_link(reinterpret_cast<void**>(&this->bucket.base_ref().px_ref())
                                                      ,reinterpret_cast<void*>(this->bucket.base_ref().px_ref()));
            } else {
                this->bucket = val;
            }
#endif
#ifdef USE_MPS
            this->bucket = val;
#endif
        }
    };


    template <class T,class U>
    struct Mapping<T,U,StrongLinks> : public MappingBase<T,U> {
        typedef typename MappingBase<T,U>::value_type value_type;
        Mapping() : MappingBase<T,U>() {};
        virtual ~Mapping() {}
        void set(const value_type& val) {
            this->bucket = val;
        }
    };



    template <class KeyType, class ValueType >
    class WeakKeyMappingPair {
        friend class core::WeakKeyMapping_O;
    protected:
        typedef WeakKeyMappingPair<KeyType,ValueType> MyType;
        typedef gctools::GCBucketAllocator<KeyType> KeyAllocatorType;
        typedef gctools::GCBucketAllocator<ValueType> ValueAllocatorType;
    public:
        KeyType*           Key;           // hash buckets for keys
        ValueType*         Value;         // hash buckets for values
    public:
        WeakKeyMappingPair()
        {
            this->Key = KeyAllocatorType::allocate();
            this->Value = ValueAllocatorType::allocate();
            this->Key->dependent = this->Value;
            this->Value->dependent = this->Key;
        }
        void swap(MyType& other)
        {
            KeyType* tempKey = this->Key;;
            ValueType* tempValue = this->Value;
            this->Key = other.Key;
            this->Value = other.Value;
            other.Key = tempKey;
            other.Value = tempValue;
        }
    };





#if 0


/* MPS globals                                                  %%MPS
 *
 * These are global variables holding MPS values for use by the
 * interpreter.  In a more sophisticated integration some of these might
 * be thread local.  See `main` for where these are set up.
 *
 * `arena` is the global state of the MPS, and there's usually only one
 * per process. See topic/arena.
 *
 * `obj_pool` is the memory pool in which the Scheme objects are allocated.
 * It is an instance of the Automatic Mostly Copying (AMC) pool class, which
 * is a general-purpose garbage collector for use when there are formatted
 * objects in the pool, but ambiguous references in thread stacks and
 * registers. See pool/amc.
 *
 * `obj_ap` is an Allocation Point that allows fast in-line non-locking
 * allocation in a memory pool.  This would usually be thread-local, but
 * this interpreter is single-threaded.  See `make_pair` etc. for how this
 * is used with the reserve/commit protocol.
 *
 * `buckets_pool` is the memory pool for hash table buckets. There are
 * two allocation points, one for buckets containing exact (strong)
 * references, the other for buckets containing weak references.
 */

    static mps_arena_t arena;       /* the arena */
    static mps_pool_t obj_pool;     /* pool for ordinary Scheme objects */
    static mps_ap_t obj_ap;         /* allocation point used to allocate objects */
    static mps_pool_t leaf_pool;    /* pool for leaf objects */
    static mps_ap_t leaf_ap;        /* allocation point for leaf objects */
    static mps_pool_t buckets_pool; /* pool for hash table buckets */
    static mps_ap_t strong_buckets_ap; /* allocation point for strong buckets */
    static mps_ap_t weak_buckets_ap; /* allocation point for weak buckets */

    static buckets_t make_buckets(size_t length, mps_ap_t ap)
    {
        buckets_t buckets;
        mps_addr_t addr;
        size_t size;
        size = ALIGN_OBJ(offsetof(buckets_s, bucket) + length * sizeof(buckets->bucket[0]));
        do {
            mps_res_t res = mps_reserve(&addr, ap, size);
            size_t i;
            if (res != MPS_RES_OK) error("out of memory in make_buckets");
            buckets = addr;
            buckets->dependent = NULL;
            buckets->length = TAG_COUNT(length);
            buckets->used = TAG_COUNT(0);
            buckets->deleted = TAG_COUNT(0);
            for(i = 0; i < length; ++i) {
                buckets->bucket[i] = obj_unused;
            }
        } while(!mps_commit(ap, addr, size));
        total += size;
        return buckets;
    }



    static int buckets_find(obj_t tbl, buckets_t buckets, obj_t key, mps_ld_t ld, size_t *b)
    {
        unsigned long i, h, probe;
        unsigned long l = UNTAG_COUNT(buckets->length) - 1;
        int result = 0;
        assert(TYPE(tbl) == TYPE_TABLE);
        h = this->hash(key, ld);
        probe = (h >> 8) | 1;
        h &= l;
        i = h;
        do {
            obj_t k = buckets->bucket[i];
            if(k == obj_unused || this->cmp(k, key)) {
                *b = i;
                return 1;
            }
            if(result == 0 && k == obj_deleted) {
                *b = i;
                result = 1;
            }
            i = (i+probe) & l;
        } while(i != h);
        return result;
    }

    static size_t table_size(obj_t tbl)
    {
        size_t used, deleted;
        assert(TYPE(tbl) == TYPE_TABLE);
        used = UNTAG_COUNT(this->keys->used);
        deleted = UNTAG_COUNT(this->keys->deleted);
        assert(used >= deleted);
        return used - deleted;
    }

/* Rehash 'tbl' so that it has 'new_length' buckets. If 'key' is found
 * during this process, update 'key_bucket' to be the index of the
 * bucket containing 'key' and return true, otherwise return false.
 * 
 * %%MPS: When re-hashing the table we reset the associated location
 * dependency and re-add a dependency on each object in the table.
 * This is because the table gets re-hashed when the locations of
 * objects have changed. See topic/location.
 */
    static int table_rehash(obj_t tbl, size_t new_length, obj_t key, size_t *key_bucket)
    {
        size_t i, length;
        buckets_t new_keys, new_values;
        int result = 0;

        assert(TYPE(tbl) == TYPE_TABLE);
        length = UNTAG_COUNT(this->keys->length);
        new_keys = make_buckets(new_length, this->key_ap);
        new_values = make_buckets(new_length, this->value_ap);
        new_keys->dependent = new_values;
        new_values->dependent = new_keys;
        mps_ld_reset(&this->ld, arena);

        for (i = 0; i < length; ++i) {
            obj_t old_key = this->keys->bucket[i];
            if (old_key != obj_unused && old_key != obj_deleted) {
                int found;
                size_t b;
                found = buckets_find(tbl, new_keys, old_key, &this->ld, &b);
                assert(found);            /* new table shouldn't be full */
                assert(new_keys->bucket[b] == obj_unused); /* shouldn't be in new table */
                new_keys->bucket[b] = old_key;
                new_values->bucket[b] = this->values->bucket[i];
                if (key != NULL && this->cmp(old_key, key)) {
                    *key_bucket = b;
                    result = 1;
                }
                new_keys->used = TAG_COUNT(UNTAG_COUNT(new_keys->used) + 1);
            }
        }

        assert(UNTAG_COUNT(new_keys->used) == table_size(tbl));
        this->keys = new_keys;
        this->values = new_values;
        return result;
    }

/* %%MPS: If we fail to find 'key' in the table, and if mps_ld_isstale
 * returns true, then some of the keys in the table might have been
 * moved by the garbage collector: in this case we need to re-hash the
 * table. See topic/location.
 */
    static obj_t table_ref(obj_t tbl, obj_t key)
    {
        size_t b;
        assert(TYPE(tbl) == TYPE_TABLE);
        if (buckets_find(tbl, this->keys, key, NULL, &b)) {
            obj_t k = this->keys->bucket[b];
            if (k != obj_unused && k != obj_deleted)
                return this->values->bucket[b];
        }
        if (mps_ld_isstale(&this->ld, arena, key))
            if (table_rehash(tbl, UNTAG_COUNT(this->keys->length), key, &b))
                return this->values->bucket[b];
        return NULL;
    }



    static int table_try_set(obj_t tbl, obj_t key, obj_t value)
    {
        size_t b;
        assert(TYPE(tbl) == TYPE_TABLE);
        if (!buckets_find(tbl, this->keys, key, &this->ld, &b))
            return 0;
        if (this->keys->bucket[b] == obj_unused) {
            this->keys->bucket[b] = key;
            this->keys->used = TAG_COUNT(UNTAG_COUNT(this->keys->used) + 1);
        } else if (this->keys->bucket[b] == obj_deleted) {
            this->keys->bucket[b] = key;
            assert(this->keys->deleted > TAG_COUNT(0));
            this->keys->deleted
                = TAG_COUNT(UNTAG_COUNT(this->keys->deleted) - 1);
        }
        this->values->bucket[b] = value;
        return 1;
    }

    static int table_full(obj_t tbl)
    {
        assert(TYPE(tbl) == TYPE_TABLE);
        return this->keys->used >= this->keys->length / 2;
    }

    static void table_set(obj_t tbl, obj_t key, obj_t value)
    {
        assert(TYPE(tbl) == TYPE_TABLE);
        if (table_full(tbl) || !table_try_set(tbl, key, value)) {
            int res;
            table_rehash(tbl, UNTAG_COUNT(this->keys->length) * 2, NULL, NULL);
            res = table_try_set(tbl, key, value);
            assert(res);                /* rehash should have made room */
        }
    }

    static void table_delete(obj_t tbl, obj_t key)
    {
        size_t b;
        assert(TYPE(tbl) == TYPE_TABLE);
        if(!buckets_find(tbl, this->keys, key, NULL, &b) ||
           this->keys->bucket[b] == obj_unused ||
           this->keys->bucket[b] == obj_deleted)
        {
            if(!mps_ld_isstale(&this->ld, arena, key))
                return;
            if(!table_rehash(tbl, UNTAG_COUNT(this->keys->length), key, &b))
                return;
        }
        if(this->keys->bucket[b] != obj_unused &&
           this->keys->bucket[b] != obj_deleted) 
        {
            this->keys->bucket[b] = obj_deleted;
            this->keys->deleted
                = TAG_COUNT(UNTAG_COUNT(this->keys->deleted) + 1);
            this->values->bucket[b] = NULL;
        }
    }




/* buckets_scan -- buckets format scan method                        %%MPS
 */

    static mps_res_t buckets_scan(mps_ss_t ss, mps_addr_t base, mps_addr_t limit)
    {
        MPS_SCAN_BEGIN(ss) {
            while (base < limit) {
                buckets_t buckets = base;
                size_t i, length = UNTAG_COUNT(buckets->length);
                FIX(buckets->dependent);
                if(buckets->dependent != NULL)
                    assert(buckets->dependent->length == buckets->length);
                for (i = 0; i < length; ++i) {
                    mps_addr_t p = buckets->bucket[i];
                    if (MPS_FIX1(ss, p)) {
                        mps_res_t res = MPS_FIX2(ss, &p);
                        if (res != MPS_RES_OK) return res;
                        if (p == NULL) {
                            /* key/value was splatted: splat value/key too */
                            p = obj_deleted;
                            buckets->deleted = TAG_COUNT(UNTAG_COUNT(buckets->deleted) + 1);
                            if (buckets->dependent != NULL) {
                                buckets->dependent->bucket[i] = p;
                                buckets->dependent->deleted
                                    = TAG_COUNT(UNTAG_COUNT(buckets->dependent->deleted) + 1);
                            }
                        }
                        buckets->bucket[i] = p;
                    }
                }
                base = (char *)base + ALIGN_OBJ(offsetof(buckets_s, bucket) +
                                                length * sizeof(buckets->bucket[0]));
            }
        } MPS_SCAN_END(ss);
        return MPS_RES_OK;
    }


/* buckets_skip -- buckets format skip method                        %%MPS
 */

    static mps_addr_t buckets_skip(mps_addr_t base)
    {
        buckets_t buckets = base;
        size_t length = UNTAG_COUNT(buckets->length);
        return (char *)base + ALIGN_OBJ(offsetof(buckets_s, bucket) +
                                        length * sizeof(buckets->bucket[0]));
    }


/* buckets_find_dependent -- find dependent object for buckets       %%MPS
 *
 * Each object in an AWL pool can have a "dependent object". The MPS
 * ensures that when an object is being scanned, its dependent object
 * is unprotected. This allows prompt deletion of values in a weak-key
 * hash table, and keys in a weak-value hash table.
 */

    static mps_addr_t buckets_find_dependent(mps_addr_t addr)
    {
        buckets_t buckets = addr;
        return buckets->dependent;
    }


/* globals_scan -- scan static global variables                 %%MPS
 *
 * The static global variables are all used to hold values that are set
 * up using the `sptab` and `isymtab` tables, and conveniently we have
 * a list of pointers to those variables.  This is a custom root scanning
 * method that uses them to fix those variables. See topic/root.
 */
#endif

    template <typename T>
    struct WeakPointer : public WeakObject {
        typedef T value_type;
        WeakPointer(const T& val) : WeakObject(WeakPointerKind), value(val) {};
        T      value;
    };
       

    template <typename T,typename Allocator>
    struct WeakPointerManager {
        WeakPointerManager(const T& val) {
            this->pointer = GCWeakPointerAllocator<WeakPointer<T>>::allocate(val);
            if ( this->pointer->value.pointerp() ) {
                GC_general_register_disappearing_link(reinterpret_cast<void**>(&this->pointer->value.base_ref().px_ref())
                                                      , reinterpret_cast<void*>(this->pointer->value.base_ref().px_ref()));
            }
        }
        virtual ~WeakPointerManager() {
#ifdef USE_BOEHM
            if (this->pointer->value.pointerp()) {
                int result = GC_unregister_disappearing_link(reinterpret_cast<void**>(&this->pointer->value.base_ref().px_ref()));
                if ( !result ) {
                    THROW_HARD_ERROR(BF("The link was not registered as a disappearing link!"));
                }
            }
#endif
        };

        // This will need to be a tagged_backcastable_base_ptr
        WeakPointer<T>*      pointer;
        T value() const { return this->pointer->value; };
        bool valid() const { return !this->pointer->value.NULLp(); };
    };



};

#endif // gctools_gcweakhash_H
