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


#define GCWEAK_LOG(x) printf("%s:%d %s\n", __FILE__, __LINE__, (x).str().c_str())

namespace gctools {

#ifdef USE_BOEHM
#define call_with_alloc_lock GC_call_with_alloc_lock
#else
    typedef void* (*fn_type)(void* client_data);
    void call_with_alloc_lock( fn_type fn, void* client_data);
#endif

    template <class Proto>
    void* wrapRun(void* wrappedFn)
    {
        std::function<Proto>* fn = reinterpret_cast<std::function<Proto>*>(wrappedFn);
        (*fn)();
        return NULL;
    }




    template <class Proto>
    void safeRun( std::function<Proto> f ) {
        printf("Entered safeRun\n");
        call_with_alloc_lock(wrapRun<Proto>,reinterpret_cast<void*>(&f));
        printf("Leaving safeRun\n");
    };




};

namespace gctools {


    typedef enum { WeakBucketKind, StrongBucketKind, WeakMappingKind, StrongMappingKind, WeakPointerKind
                   , WeakFwdKind, WeakFwd2Kind
                   , WeakPadKind, WeakPad1Kind
                   /*Other MPS kinds here */ } WeakKinds;

    struct WeakObject {
        typedef gctools::tagged_ptr<gctools::Fixnum_ty> KindType;
        WeakObject(WeakKinds k) : Kind(gctools::tagged_ptr<gctools::Fixnum_ty>(k)) {};
        KindType Kind;
        int kind() const { return this->Kind.fixnum(); };
        void setKind(WeakKinds k) { this->Kind = gctools::tagged_ptr<gctools::Fixnum_ty>(k); };
        virtual void* dependentPtr() const { return NULL; };
        
    };

    struct weak_fwd_s : public WeakObject {
        WeakObject* fwd;                    /* forwarded object */
        gctools::tagged_ptr<gctools::Fixnum_ty> size; /* total size of this object */
    };

    struct weak_fwd2_s : public WeakObject {
        WeakObject* fwd;                    /* forwarded object */
    };


    struct weak_pad_s : public WeakObject {
        WeakObject* fwd;                    /* forwarded object */
        gctools::tagged_ptr<gctools::Fixnum_ty> size; /* total size of this object */
    };

    struct weak_pad1_s : public WeakObject {
    };



    template <class T,class U>
    struct BucketsBase : public WeakObject {
        BucketsBase(WeakKinds k,int l) : WeakObject(k)
                                       , _length(gctools::tagged_ptr<gctools::Fixnum_ty>(l))
                                       , _used(gctools::tagged_ptr<gctools::Fixnum_ty>(0))
                                       , _deleted(gctools::tagged_ptr<gctools::Fixnum_ty>(0))
        {
            for (size_t i(0); i<l; ++i ) this->bucket[i] = T(T::unbound);
        }

        virtual ~BucketsBase() {};

        void* dependentPtr() const { return reinterpret_cast<void*>(this->dependent); };

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
        Buckets(int l) : BucketsBase<T,U>(WeakBucketKind,l) {};
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
        Buckets(int l) : BucketsBase<T,U>(StrongBucketKind,l) {};
        virtual ~Buckets() {}
        void set(size_t idx, const value_type& val) {
            this->bucket[idx] = val;
        }
    };

    typedef gctools::tagged_backcastable_base_ptr<core::T_O> BucketValueType;
    typedef gctools::Buckets<BucketValueType,BucketValueType,gctools::WeakLinks> WeakBucketsObjectType;
    typedef gctools::Buckets<BucketValueType,BucketValueType,gctools::StrongLinks> StrongBucketsObjectType;

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
        KeyBucketsType*           _Keys;           // hash buckets for keys
        ValueBucketsType*         _Values;         // hash buckets for values
#ifdef USE_MPS
        mps_ld_s        _LocationDependency;
#endif        

    public:
        WeakHashTable(size_t length=0)
        {
            /* round up to next power of 2 */
            if ( length == 0 ) length = 2;
            size_t l;
            for( l = 1; l < length; l *= 2);
            this->_Keys = KeyBucketsAllocatorType::allocate(l);
            this->_Values = ValueBucketsAllocatorType::allocate(l);
            this->_Keys->dependent = this->_Values;
            this->_Values->dependent = this->_Keys;
#ifdef USE_MPS
            mps_ld_reset(&this->_LocationDependency, _global_arena);
#endif
        }

    public:
        static uint sxhashKey(const value_type& key
#ifdef USE_MPS
                              , mps_ld_s* locationDependencyP
#endif
            )
        {
#ifdef USE_MPS
            if ( locationDependencyP && key.pointerp()) {
                mps_ld_add(locationDependencyP
                           ,gctools::_global_arena
                           ,key.base_ref().px_ref() );
            }
#endif
            return core::lisp_hash(reinterpret_cast<uintptr_t>(key.base_ref().px_ref()));
        }



        static int find(KeyBucketsType* keys, const value_type& key
#ifdef USE_MPS
                        , mps_ld_s* ldP
#endif
                        , size_t& b )
        {
            unsigned long i, h, probe;
            unsigned long l = keys->length()-1;
            int result = 0;
            h = WeakHashTable::sxhashKey(key
#ifdef USE_MPS
                                         ,ldP
#endif
                );
            printf("%s:%d find key = %p  h = %lx   l = %lu\n", __FILE__, __LINE__, key.pointer(), h, l );
            probe = (h >> 8) | 1;
            h &= l;
            i = h;
            do {
                value_type& k = (*keys)[i];
                printf("%s:%d i = %lu   k = %p\n", __FILE__, __LINE__, i, k.pointer() );
                if( k.unboundp() || k == key ) {
                    b = i;
                    printf("%s:%d  returning 1   b=%lu  k = %p\n", __FILE__, __LINE__, b, k.pointer() );
                    return 1;
                }
#ifdef USE_BOEHM
                // Handle splatting
                if (k.NULLp()) {
                    keys->set(i,value_type(gctools::tagged_ptr<core::T_O>::tagged_deleted));
                    ValueBucketsType* values = dynamic_cast<ValueBucketsType*>(keys->dependent);
                    (*values)[i] = value_type(gctools::tagged_ptr<core::T_O>::tagged_unbound);
                }
#endif
                if ( result == 0 && ( k.deletedp() )) {
                    b = i;
                    result = 1;
                }
                i = (i+probe) & l;
            } while(i != h);
            return result;
        }



    public:
        size_t length() const {
            if ( this->_Keys==NULL ) {
                THROW_HARD_ERROR(BF("Keys should never be null"));
            }
            return this->_Keys->length();
        }


        void swap(MyType& other)
        {
            KeyBucketsType* tempKeys = this->_Keys;;
            ValueBucketsType* tempValues = this->_Values;
            this->_Keys = other._Keys;
            this->_Values = other._Values;
            other._Keys = tempKeys;
            other._Values = tempValues;
        }

        bool fullp() const {
            bool fp;
            safeRun<void()>( [&fp,this] ()->void {
                    fp = (*this->_Keys).used() >= (*this->_Keys).length()/2;
                } );
            return fp;
        }

        int tableSize() const
        {
            int result;
            safeRun<void()>( [&result,this] ()->void {
                    size_t used, deleted;
                    used = this->_Keys->used();
                    deleted = this->_Keys->deleted();
                    GCTOOLS_ASSERT(used >= deleted);
                    result = used - deleted;
                });
            return result;
        }





        int rehash(size_t newLength, const value_type& key, size_t& key_bucket)
        {
            int result;
            safeRun<void()>( [&result,this,newLength,&key,&key_bucket] ()->void {
                    GCWEAK_LOG(BF("entered rehash newLength = %d") % newLength );
                    size_t i, length;
                    // buckets_t new_keys, new_values;
                    int result = 0;
                    length = this->_Keys->length();
                    MyType newHashTable(newLength);
                    //new_keys = make_buckets(newLength, this->key_ap);
                    //new_values = make_buckets(newLength, this->value_ap);
                    //new_keys->dependent = new_values;
                    //new_values->dependent = new_keys;
#ifdef USE_MPS
                    mps_ld_reset(&this->_LocationDependency,gctools::_global_arena);
#endif
                    for (i = 0; i < length; ++i) {
                        value_type& old_key = (*this->_Keys)[i];
                        if (!old_key.unboundp() && !old_key.deletedp()) {
                            int found;
                            size_t b;
#ifdef USE_MPS
                            found = WeakHashTable::find(newHashTable._Keys, old_key, &this->_LocationDependency, b);
#else
                            found = WeakHashTable::find(newHashTable._Keys, old_key, b);
#endif
                            GCTOOLS_ASSERT(found);// assert(found);            /* new table shouldn't be full */
                            GCTOOLS_ASSERT((*this->_Keys)[b].unboundp()); /* shouldn't be in new table */
                            newHashTable._Keys->set(b,old_key);
                            (*newHashTable._Values)[b] = (*this->_Values)[i];
                            if (!key.NULLp() && old_key == key ) {
                                key_bucket = b;
                                result = 1;
                            }
                            (*newHashTable._Keys).setUsed((*newHashTable._Keys).used()+1); // TAG_COUNT(UNTAG_COUNT(new_keys->used) + 1);
                        }
                    }
                    GCTOOLS_ASSERT((*newHashTable._Keys).used() == this->tableSize() );
                    // assert(UNTAG_COUNT(new_keys->used) == table_size(tbl));
                    this->swap(newHashTable);
                } );
            return result;
        }

        int trySet(core::T_sp tkey, core::T_sp value)
        {
            size_t b;
            if ( tkey == value ) { value = gctools::smart_ptr<core::T_O>(gctools::tagged_ptr<core::T_O>::tagged_sameAsKey); };
            value_type key(tkey);
            int result = WeakHashTable::find(this->_Keys,key
#if USE_MPS
                                             , &this->_LocationDependency
#endif
                                             ,b);
            if (!result) { // this->find(key, this->_HashTable.Keys, true, b)) {
                printf("%s:%d find returned 0\n", __FILE__, __LINE__ );
                return 0;
            }
            if ((*this->_Keys)[b].unboundp()) {
                this->_Keys->set(b,key);
                (*this->_Keys).setUsed((*this->_Keys).used()+1);
                printf("%s:%d key was unboundp at %lu  used = %d\n", __FILE__, __LINE__, b, this->_Keys->used() );
            } else if ((*this->_Keys)[b].deletedp()) {
                this->_Keys->set(b,key);
                GCTOOLS_ASSERT((*this->_Keys).deleted() > 0 );
                (*this->_Keys).setDeleted((*this->_Keys).deleted()-1);
                printf("%s:%d key was deletedp at %lu  deleted = %d\n", __FILE__, __LINE__, b, (*this->_Keys).deleted() );
            }
            (*this->_Values).set(b,value_type(value));
            return 1;
        }



        core::T_mv gethash(core::T_sp tkey, core::T_sp defaultValue)
        {
            core::T_mv result_mv;
            safeRun<void()>( [&result_mv,this,tkey,defaultValue] ()->void
                {
                    value_type key(tkey);
                    size_t pos;
                    int result = gctools::WeakHashTable::find(this->_Keys,key
#ifdef USE_MPS
                                                              ,NULL
#endif
                                                              ,pos);
                    if (result) { // WeakHashTable::find(this->_Keys,key,false,pos)) { //buckets_find(tbl, this->keys, key, NULL, &b)) {
                        value_type& k = (*this->_Keys)[pos];
                        GCWEAK_LOG(BF("gethash find successful pos = %d  k= %p k.unboundp()=%d k.base_ref().deletedp()=%d k.NULLp()=%d") % pos % k.pointer() % k.unboundp() % k.base_ref().deletedp() % k.NULLp() );
                        if ( !k.unboundp() && !k.deletedp() ) {
                            GCWEAK_LOG(BF("Returning success!"));
                            core::T_sp value = (*this->_Values)[pos].backcast();
                            if ( value.sameAsKeyP() ) {
                                value = k.backcast();
                            }
                            result_mv = Values(value,core::lisp_true());
                            return;
                        }
                        GCWEAK_LOG(BF("Falling through"));
                    }
#ifdef USE_MPS
                    if (key.pointerp() && mps_ld_isstale(&this->_LocationDependency, gctools::_global_arena, key.pointer() )) {
                        if (this->rehash( this->_Keys->length(), key, pos)) {
                            core::T_sp value = (*this->_Values)[pos].backcast();
                            if ( value.sameAsKeyP() ) {
                                value = key.backcast();
                            }
                            result_mv = Values(value,core::lisp_true());
                            return;
                        }
                    }
#endif
                    result_mv = Values(defaultValue,_Nil<core::T_O>());
                    return;
                } );
            return result_mv;
        }






        void set( core::T_sp key, core::T_sp value )
        {
            safeRun<void()>(
                [key,value,this] ()->void
                {
                    if (this->fullp() || !this->trySet(key,value) ) {
                        int res;
                        value_type dummyKey;
                        size_t dummyPos;
                        this->rehash( (*this->_Keys).length() * 2, dummyKey, dummyPos );
                        core::T_sp tvalue = value;
                        if ( key == value ) {
                            tvalue = gctools::smart_ptr<core::T_O>(gctools::tagged_ptr<core::T_O>::tagged_sameAsKey);
                        }
                        res = this->trySet( key, tvalue);
                        GCTOOLS_ASSERT(res);
                    }
                });
        }


        void remhash( core::T_sp tkey )    {
            safeRun<void()>( [this,tkey] ()->void
                {
                    size_t b;
                    value_type key(tkey);
#ifdef USE_MPS
                    int result = gctools::WeakHashTable::find(this->_Keys, key, NULL, b);
#endif
#ifdef USE_BOEHM
                    int result = gctools::WeakHashTable::find(this->_Keys, key, b);
#endif
                    if( ! result ||
                        (*this->_Keys)[b].unboundp() ||
                        (*this->_Keys)[b].deletedp() )
                    {
#ifdef USE_MPS
                        if(key.pointerp() && !mps_ld_isstale(&this->_LocationDependency, gctools::_global_arena, key.pointer()))
                            return;
#endif
                        if(!this->rehash( (*this->_Keys).length(), key, b))
                            return;
                    }
                    if( !(*this->_Keys)[b].unboundp() &&
                        !(*this->_Keys)[b].deletedp() )
                    {
                        this->_Keys->set(b, value_type(value_type::deleted)); //[b] = value_type(gctools::tagged_ptr<T_O>::tagged_deleted);
                        (*this->_Keys).setDeleted((*this->_Keys).deleted()+1);
                        (*this->_Values)[b] = value_type(gctools::tagged_ptr<core::T_O>::tagged_unbound);
                    }
                } );
        }




        void clrhash()
        {   
            safeRun<void()>( [this] ()->void
                {
                    size_t len = (*this->_Keys).length();
                    for ( size_t i(0); i<len; ++i ) {
                        this->_Keys->set(i,value_type(gctools::tagged_ptr<core::T_O>::tagged_unbound));
                        (*this->_Values)[i] = value_type(gctools::tagged_ptr<core::T_O>::tagged_unbound);
                    }
                    (*this->_Keys).setUsed(0);
                    (*this->_Keys).setDeleted(0);
#ifdef USE_MPS
                    mps_ld_reset(&this->_LocationDependency,gctools::_global_arena);
#endif
                } );
        };


    

    };




// ======================================================================
// ----------------------------------------------------------------------


    template <class T,class U>
    struct MappingBase : public WeakObject {
        MappingBase(const T& val) : WeakObject(WeakMappingKind), bucket(val) {};
        virtual ~MappingBase() {};
        typedef T   value_type;
        void* dependentPtr() const { return reinterpret_cast<void*>(this->dependent); };
        MappingBase<U,T>* dependent;  /* the dependent object */
        T bucket;              /* single buckets */
    };


    template <class T, class U, class Link>
    struct Mapping;

    template <class T,class U>
    struct Mapping<T,U,WeakLinks> : public MappingBase<T,U> {
        typedef typename MappingBase<T,U>::value_type value_type;
        Mapping(const T& val) : MappingBase<T,U>(val) {
#ifdef USE_BOEHM
            if (this->bucket.pointerp()) {
                printf("%s:%d Mapping register disappearing link\n", __FILE__, __LINE__);
                GC_general_register_disappearing_link(reinterpret_cast<void**>(&this->bucket.base_ref().px_ref())
                                                      ,reinterpret_cast<void*>(this->bucket.base_ref().px_ref()));
            }
#endif
        };
        virtual ~Mapping() {
#ifdef USE_BOEHM
            if (this->bucket.pointerp()) {
                printf("%s:%d Mapping unregister disappearing link\n", __FILE__, __LINE__);
                int result = GC_unregister_disappearing_link(reinterpret_cast<void**>(&this->bucket.base_ref().px_ref()));
                if ( !result ) {
                    THROW_HARD_ERROR(BF("The link was not registered as a disappearing link!"));
                }
            }
#endif
        }

    };


    template <class T,class U>
    struct Mapping<T,U,StrongLinks> : public MappingBase<T,U> {
        typedef typename MappingBase<T,U>::value_type value_type;
        Mapping(const T& val) : MappingBase<T,U>(val) {};
        virtual ~Mapping() {}
    };


    typedef gctools::tagged_backcastable_base_ptr<core::T_O> MappingValueType;
    typedef gctools::Mapping<BucketValueType,BucketValueType,gctools::WeakLinks> WeakMappingObjectType;
    typedef gctools::Mapping<BucketValueType,BucketValueType,gctools::StrongLinks> StrongMappingObjectType;


    class WeakKeyMappingPair {
        friend class core::WeakKeyMapping_O;
    protected:
        typedef MappingValueType value_type;
        typedef WeakMappingObjectType KeyType;
        typedef StrongMappingObjectType ValueType;
        typedef WeakKeyMappingPair              MyType;
        typedef gctools::GCMappingAllocator<KeyType> KeyAllocatorType;
        typedef gctools::GCMappingAllocator<ValueType> ValueAllocatorType;
    public:
        KeyType*           Key;           // hash buckets for keys
        ValueType*         Value;         // hash buckets for values
    public:
        WeakKeyMappingPair(const value_type& key, const value_type& value)
        {
            this->Key = KeyAllocatorType::allocate(key);
            this->Value = ValueAllocatorType::allocate(value);
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

        bool unsafeValid() const {
            return !this->Key->bucket.NULLp() && !this->Key->bucket.unboundp();
        }
        bool valid() const
        {
            bool result;
            safeRun<void()>( [&result,this] ()->void
                {
                    result = this->unsafeValid();
                } );
            return result;
        };

        /*! Return (values key value t) or (values nil nil nil) */
        core::T_mv keyValue() const
        {
            core::T_mv result_mv;
            safeRun<void()>( [&result_mv,this] ()->void
                {   
            
                    if (!this->unsafeValid()) {
                        result_mv = Values(_Nil<core::T_O>(),_Nil<core::T_O>(),_Nil<core::T_O>());
                        return;
                    }
                    value_type& key_ref = this->Key->bucket;
                    value_type& value_ref = this->Value->bucket;
                    core::T_sp key = key_ref.backcast();
                    core::T_sp value;
                    if ( value_ref.sameAsKeyP() ) {
                        value = key;
                    } else { 
                        value = value_ref.backcast();
                    }
                    result_mv = Values(key,value,core::lisp_true());
                    return;
                } );
            return result_mv;
        };

    };



    struct WeakPointer : public WeakObject {
        typedef gctools::tagged_backcastable_base_ptr<core::T_O> value_type;
        WeakPointer(const value_type& val) : WeakObject(WeakPointerKind), value(val) {};
        value_type      value;
    };
       

    struct WeakPointerManager {
        typedef typename gctools::WeakPointer::value_type  value_type;
        typedef WeakPointerManager MyType;
        typedef gctools::GCWeakPointerAllocator<WeakPointer> AllocatorType;

        WeakPointerManager(const value_type& val) {
            this->pointer = AllocatorType::allocate(val);
#ifdef USE_BOEHM
            if ( this->pointer->value.pointerp() ) {
                GC_general_register_disappearing_link(reinterpret_cast<void**>(&this->pointer->value.base_ref().px_ref())
                                                      , reinterpret_cast<void*>(this->pointer->value.base_ref().px_ref()));
            }
#endif
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
        WeakPointer*      pointer;
        core::T_mv value() const { 
            core::T_mv result_mv;
            safeRun<void()>( [&result_mv,this] ()->void
                {
                    if (!this->pointer->value.NULLp()) {
                        result_mv = Values(gctools::smart_ptr<core::T_O>(this->pointer->value.backcast()),core::lisp_true());
                        return;
                    }
                    result_mv = Values(_Nil<core::T_O>(),_Nil<core::T_O>());
                } );
            return result_mv;
        }

        bool valid() const
        {
            bool result;
            safeRun<void()>( [&result,this] ()->void
                {
                    result = !this->pointer->value.NULLp();
                } );
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
