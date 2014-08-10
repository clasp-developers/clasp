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

#define call_with_alloc_lock GC_call_with_alloc_lock

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

    class WeakHashTable {
        friend class core::WeakKeyHashTable_O;
    public:
        typedef gctools::tagged_backcastable_base_ptr<core::T_O> value_type;
        typedef gctools::Buckets<value_type,value_type,gctools::WeakLinks> KeyBucketsType;
        typedef gctools::Buckets<value_type,value_type,gctools::StrongLinks> ValueBucketsType;
    public:
        typedef WeakHashTable MyType;
    public:
        typedef gctools::GCBucketAllocator<KeyBucketsType> KeyBucketsAllocatorType;
        typedef gctools::GCBucketAllocator<ValueBucketsType> ValueBucketsAllocatorType;
    public:
        KeyBucketsType*           _Keys;           // hash buckets for keys
        ValueBucketsType*         _Values;         // hash buckets for values
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
            mps_ld_reset(&this->ld, _global_arena);
#endif
        }

    public:
#ifdef USE_MPS
        static uint sxhashKey(const value_type& key, mps_ld_s* locationDependencyP );
#else
        static uint sxhashKey(const value_type& key );
#endif

#ifdef USE_MPS
        static int find(KeyBucketsType* keys, const value_type& key, mps_ld_s* ldP, size_t& b );
#else
        static int find(KeyBucketsType* keys, const value_type& key, size_t& b );
#endif


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





        int rehash(size_t newLength, const value_type& key, size_t& key_bucket);
        int trySet(core::T_sp tkey, core::T_sp value);
        core::T_mv gethash(core::T_sp tkey, core::T_sp defaultValue);
        void set( core::T_sp key, core::T_sp value );
        void remhash( core::T_sp tkey );
        void clrhash();

    

    };




// ======================================================================
// ----------------------------------------------------------------------


    template <class T,class U>
    struct MappingBase : public WeakObject {
        MappingBase(const T& val) : WeakObject(WeakMappingKind), bucket(val) {};
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



    class WeakKeyMappingPair {
        friend class core::WeakKeyMapping_O;
    protected:
        typedef gctools::tagged_backcastable_base_ptr<core::T_O> value_type;
        typedef Mapping<value_type,value_type,WeakLinks> KeyType;
        typedef Mapping<value_type,value_type,StrongLinks> ValueType;
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


        bool valid() const
        {
            bool result;
            safeRun<void()>( [&result,this] ()->void
                {
                    result = !this->Key->bucket.NULLp() && !this->Key->bucket.unboundp();
                } );
            return result;
        };

        /*! Return (values key value t) or (values nil nil nil) */
        core::T_mv keyValue() const
        {
            core::T_mv result_mv;
            safeRun<void()>( [&result_mv,this] ()->void
                {   
            
                    if (!this->valid()) {
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

#endif // gctools_gcweak_H
