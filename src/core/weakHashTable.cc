#include "foundation.h"
#include "object.h"
#include "lisp.h"
#include "weakHashTable.h"
#include "wrappers.h"


#define WEAK_LOG(x) printf("%s:%d %s\n", __FILE__, __LINE__, (x).str().c_str())

namespace core {

    EXPOSE_CLASS(core,WeakHashTable_O);

        
    void WeakHashTable_O::exposeCando(::core::Lisp_sp lisp)
    {
	::core::class_<WeakHashTable_O>()
	    ;
    }
    
    void WeakHashTable_O::exposePython(Lisp_sp lisp)
    {_G();
#ifdef USEBOOSTPYTHON
	PYTHON_CLASS(CorePkg,WeakHashTable,"","",_lisp)
	    ;
#endif
    }

    EXPOSE_CLASS(core,WeakKeyHashTable_O);


    void WeakKeyHashTable_O::describe()
    {
        KeyBucketsType& keys = *this->_HashTable._Keys;
        ValueBucketsType& values = *this->_HashTable._Values;
        printf("WeakKeyHashTable   size: %zu\n", this->_HashTable.length());
        printf("   keys memory range:  %p  - %p \n", &keys[0].base_ref().px_ref(), &keys[this->_HashTable.length()].base_ref().px_ref());
        printf("   _HashTable.length = %d\n", keys.length() );
        printf("   _HashTable.used = %d\n", keys.used() );
        printf("   _HashTable.deleted = %d\n", keys.deleted() );
        for ( int i(0), iEnd(this->_HashTable.length()); i<iEnd; ++i ) {
            value_type& key = keys[i];
            stringstream sentry;
            sentry.width(3);
            sentry << i << "  key.px@" << (void*)(&key.base_ref().px_ref()) << "  ";
            if ( !key.base_ref() ) {
                sentry << "splatted";
            } else if ( key.base_ref().unboundp() ) {
                sentry << "unbound";
            } else if ( key.base_ref().deletedp() ) {
                sentry << "deleted";
            } else {
                // key.base_ref().nilp() ) {
                T_sp okey = key.backcast();
                sentry << _rep_(okey);
                sentry << "@" << (void*)(key.base_ref().px_ref());
                sentry << "   -->   ";
                value_type val = values[i];
                if ( val.sameAsKeyP() ) {
                    sentry << "sameAsKey!!!";
                } else {
                    sentry << _rep_(val.backcast());
                }
            }
            printf("   %s\n", sentry.str().c_str() );
        }
    }



    int WeakKeyHashTable_O::tableSize() const {
        return this->_HashTable.tableSize();
    }



    bool WeakKeyHashTable_O::fullp()
    {
        return this->_HashTable.fullp();
    }


#if 0
    /*! Return the key/value as three values
      (values key value invalid-key)
      invalid-key is one of nil, :unused :deleted :splatted 
      depending if the key is valid, unused, deleted or a weak link was splatted */
    T_mv WeakKeyHashTable_O::get(int idx)
    {
        T_sp key = (*this->_HashTable.Keys)[idx].backcast();
        T_sp val = (*this->_HashTable.Values)[idx].backcast();
        if ( val.sameAsKeyP() ) {
            val = key;
        }
        if ( key.pointerp() ) {
            return Values(key,val,_Nil<T_O>());
        }
        T_sp keyInfo(_Nil<T_O>());
        if ( !key ) {
            keyInfo = kw::_sym_splatted;
        } else if ( key.unboundp()) {
            keyInfo = kw::_sym_unbound;
        } else if ( key.deletedp()) {
            keyInfo = kw::_sym_deleted;
        }
        return Values(_Nil<T_O>(), val, keyInfo );
    }
#endif
    SYMBOL_EXPORT_SC_(KeywordPkg,splatted);
    SYMBOL_EXPORT_SC_(KeywordPkg,unbound);
    SYMBOL_EXPORT_SC_(KeywordPkg,deleted);


#if 0
/* Rehash 'tbl' so that it has 'new_length' buckets. If 'key' is found
 * during this process, update 'key_bucket' to be the index of the
 * bucket containing 'key' and return true, otherwise return false.
 * 
 * %%MPS: When re-hashing the table we reset the associated location
 * dependency and re-add a dependency on each object in the table.
 * This is because the table gets re-hashed when the locations of
 * objects have changed. See topic/location.
 */
    int WeakKeyHashTable_O::rehash(size_t newLength, const value_type& key, size_t& key_bucket)
    {
        WEAK_LOG(BF("entered rehash newLength = %d") % newLength );
        size_t i, length;
        // buckets_t new_keys, new_values;
        int result = 0;
        length = this->_HashTable.Keys->length();
        HashTableType newHashTable(newLength);
        //new_keys = make_buckets(newLength, this->key_ap);
        //new_values = make_buckets(newLength, this->value_ap);
        //new_keys->dependent = new_values;
        //new_values->dependent = new_keys;
#ifdef USE_MPS
        mps_ld_reset(&this->_LocationDependency,gctools::_global_arena);
#endif
        for (i = 0; i < length; ++i) {
            value_type& old_key = (*this->_HashTable.Keys)[i];
            if (!old_key.unboundp() && !old_key.deletedp()) {
                int found;
                size_t b;
#ifdef USE_MPS
                found = WeakKeyHashTable_O::find(newHashTable.Keys, old_key, &this->_LocationDependency, b);
#else
                found = WeakKeyHashTable_O::find(newHashTable.Keys, old_key, b);
#endif
                ASSERT(found);// assert(found);            /* new table shouldn't be full */
                ASSERT((*this->_HashTable.Keys)[b].unboundp()); /* shouldn't be in new table */
                newHashTable.Keys->set(b,old_key);
                (*newHashTable.Values)[b] = (*this->_HashTable.Values)[i];
                if (!key.NULLp() && old_key == key ) {
                    key_bucket = b;
                    result = 1;
                }
                (*newHashTable.Keys).setUsed((*newHashTable.Keys).used()+1); // TAG_COUNT(UNTAG_COUNT(new_keys->used) + 1);
            }
        }
        ASSERT((*newHashTable.Keys).used() == this->_HashTable.tableSize() );
        // assert(UNTAG_COUNT(new_keys->used) == table_size(tbl));
        this->_HashTable.swap(newHashTable);
        return result;
    }
#endif
        

#if 0
    int WeakKeyHashTable_O::trySet(T_sp tkey, T_sp value)
    {
        size_t b;
        if ( tkey == value ) { value = gctools::smart_ptr<T_O>(gctools::tagged_ptr<T_O>::tagged_sameAsKey); };
        value_type key(tkey);
#ifdef USE_MPS
        int result = this->_HashTable.find(this->_HashTable.Keys,key,&this->_LocationDependency,b);
#else
        int result = this->_HashTable.find(this->_HashTable.Keys,key,b);
#endif
        if (!result) { // this->find(key, this->_HashTable.Keys, true, b)) {
            printf("%s:%d find returned 0\n", __FILE__, __LINE__ );
            return 0;
        }
        if ((*this->_HashTable.Keys)[b].unboundp()) {
            this->_HashTable.Keys->set(b,key);
            (*this->_HashTable.Keys).setUsed((*this->_HashTable.Keys).used()+1);
            printf("%s:%d key was unboundp at %lu  used = %d\n", __FILE__, __LINE__, b, this->_HashTable.Keys->used() );
        } else if ((*this->_HashTable.Keys)[b].deletedp()) {
            this->_HashTable.Keys->set(b,key);
            ASSERT((*this->_HashTable.Keys).deleted() > 0 );
            (*this->_HashTable.Keys).setDeleted((*this->_HashTable.Keys).deleted()-1);
            printf("%s:%d key was deletedp at %lu  deleted = %d\n", __FILE__, __LINE__, b, (*this->_HashTable.Keys).deleted() );
        }
        (*this->_HashTable.Values).set(b,value_type(value));
        return 1;
    }

#endif


#if 0
/* %%MPS: If we fail to find 'key' in the table, and if mps_ld_isstale
 * returns true, then some of the keys in the table might have been
 * moved by the garbage collector: in this case we need to re-hash the
 * table. See topic/location.
 * Return (values value t) or (values nil nil)
 */
    T_mv WeakKeyHashTable_O::gethash(T_sp tkey, T_sp defaultValue)
    {
        value_type key(tkey);
        size_t pos;
#ifdef USE_MPS
        int result = gctools::WeakHashTable::find(this->_HashTable.Keys,key,NULL,pos);
#endif
#ifdef USE_BOEHM
        int result = gctools::WeakHashTable::find(this->_HashTable.Keys,key,pos);
#endif
        
        if (result) { // WeakKeyHashTable_O::find(this->_HashTable.Keys,key,false,pos)) { //buckets_find(tbl, this->keys, key, NULL, &b)) {
            value_type& k = (*this->_HashTable.Keys)[pos];
            WEAK_LOG(BF("gethash find successful pos = %d  k= %p k.unboundp()=%d k.base_ref().deletedp()=%d k.NULLp()=%d") % pos % k.pointer() % k.unboundp() % k.base_ref().deletedp() % k.NULLp() );
            if ( !k.unboundp() && !k.deletedp() ) {
                WEAK_LOG(BF("Returning success!"));
                T_sp value = (*this->_HashTable.Values)[pos].backcast();
                if ( value.sameAsKeyP() ) {
                    value = k.backcast();
                }
                return Values(value,_lisp->_true());
            }
            WEAK_LOG(BF("Falling through"));
        }
#ifdef USE_MPS
        if (key.pointerp() && mps_ld_isstale(&this->_LocationDependency, gctools::_global_arena, key.pointer() )) {
            if (this->_HashTable.rehash( this->_HashTable.Keys->length(), key, &pos)) {
                T_sp value = (*this->_HashTable.Values)[pos].backcast();
                if ( value.sameAsKeyP() ) {
                    value = key.backcast();
                }
                return Values(value,_lisp->_true());
            }
        }
#endif
        return Values(defaultValue,_Nil<T_O>());
    }
#endif

#if 0
    void WeakKeyHashTable_O::set( T_sp key, T_sp value )
    {
        if (this->fullp() || !this->trySet(key,value) ) {
            int res;
            value_type dummyKey;
            size_t dummyPos;
            this->_HashTable.rehash( (*this->_HashTable.Keys).length() * 2, dummyKey, dummyPos );
            if ( key == value ) {
                value = gctools::smart_ptr<T_O>(gctools::tagged_ptr<T_O>::tagged_sameAsKey);
            }
            res = this->trySet( key, value);
            ASSERT(res);
        }
    }
#endif
#if 0
    void WeakKeyHashTable_O::remhash( T_sp tkey )
    {
        size_t b;
        value_type key(tkey);
#ifdef USE_MPS
        int result = gctools::WeakHashTable::find(this->_HashTable.Keys, key, NULL, b);
#endif
#ifdef USE_BOEHM
        int result = gctools::WeakHashTable::find(this->_HashTable.Keys, key, b);
#endif
        if( ! result ||
            (*this->_HashTable.Keys)[b].unboundp() ||
            (*this->_HashTable.Keys)[b].deletedp() )
        {
#ifdef USE_MPS
            if(key.pointerp() && !mps_ld_isstale(&this->_LocationDependency, gctools::_global_arena, key.pointer()))
                return;
#endif
            if(!this->_HashTable.rehash( (*this->_HashTable.Keys).length(), key, b))
                return;
        }
        if( !(*this->_HashTable.Keys)[b].unboundp() &&
            !(*this->_HashTable.Keys)[b].deletedp() )
        {
#ifdef USE_BOEHM
            IMPLEMENT_MEF(BF("Remove disappearing link to this entry"));
#endif
            this->_HashTable.Keys->set(b, value_type(value_type::deleted)); //[b] = value_type(gctools::tagged_ptr<T_O>::tagged_deleted);
            (*this->_HashTable.Keys).setDeleted((*this->_HashTable.Keys).deleted()+1);
            (*this->_HashTable.Values)[b] = value_type(gctools::tagged_ptr<T_O>::tagged_unbound);
        }
    }
#endif

#if 0
    void WeakKeyHashTable_O::clrhash()
    {
        size_t len = (*this->_HashTable.Keys).length();
        for ( size_t i(0); i<len; ++i ) {
#ifdef USE_BOEHM
            IMPLEMENT_MEF(BF("Remove disappearing links to this entry"));
#endif
            this->_HashTable.Keys->set(i,value_type(gctools::tagged_ptr<T_O>::tagged_unbound));
            (*this->_HashTable.Values)[i] = value_type(gctools::tagged_ptr<T_O>::tagged_unbound);
        }
        (*this->_HashTable.Keys).setUsed(0);
        (*this->_HashTable.Keys).setDeleted(0);
#ifdef USE_MPS
        mps_ld_reset(&this->_LocationDependency,gctools::_global_arena);
#endif
    }

#endif



    
    
#define ARGS_core_makeWeakKeyHashTable "(&optional (size 16))"
#define DECL_core_makeWeakKeyHashTable ""
#define DOCS_core_makeWeakKeyHashTable "makeWeakKeyHashTable"
    WeakKeyHashTable_sp core_makeWeakKeyHashTable(Fixnum_sp size)
    {_G();
        int sz = size->get();
        WeakKeyHashTable_sp ht = gctools::GCObjectAllocator<WeakKeyHashTable_O>::allocate(sz);
        return ht;
    }



    
    
#define ARGS_core_weakGethash "(key hash-table &optional default-value)"
#define DECL_core_weakGethash ""
#define DOCS_core_weakGethash "weakGethash"
    T_mv core_weakGethash(T_sp tkey, WeakKeyHashTable_sp ht, T_sp defaultValue)
    {_G();
        return ht->gethash(tkey,defaultValue);
    };

#define ARGS_core_weakSetfGethash "(ht key value)"
#define DECL_core_weakSetfGethash ""
#define DOCS_core_weakSetfGethash "weakSetfGethash"
    void core_weakSetfGethash(T_sp key, WeakKeyHashTable_sp ht, T_sp val)
    {_G();
        ht->set(key,val);
    };

    
#define ARGS_core_weakRemhash "(ht key)"
#define DECL_core_weakRemhash ""
#define DOCS_core_weakRemhash "weakRemhash"
    void core_weakRemhash(WeakKeyHashTable_sp ht, T_sp key)
    {_G();
        ht->remhash(key);
    };


#define ARGS_core_weakClrhash "(ht)"
#define DECL_core_weakClrhash ""
#define DOCS_core_weakClrhash "weakClrhash"
    void core_weakClrhash(WeakKeyHashTable_sp ht)
    {_G();
        ht->clrhash();
    };


#define ARGS_core_weakSplat "(ht idx)"
#define DECL_core_weakSplat ""
#define DOCS_core_weakSplat "weakSplat"
    void core_weakSplat(WeakKeyHashTable_sp ht, Fixnum_sp idx)
    {_G();
        (*ht->_HashTable._Keys).set(idx->get(),WeakKeyHashTable_O::value_type(gctools::tagged_ptr<T_O>::_NULL));
    };
    

#define ARGS_core_weakRehash "(ht &optional sz)"
#define DECL_core_weakRehash ""
#define DOCS_core_weakRehash "weakRehash"
    void core_weakRehash(WeakKeyHashTable_sp ht, Fixnum_sp sz)
    {_G();
        size_t newLength;
        if ( sz.nilp() ) {
            newLength = ht->_HashTable._Keys->length()*2;
        } else {
            newLength = sz->get();
        }
        WeakKeyHashTable_O::value_type dummyKey;
        size_t dummyPos;
        ht->_HashTable.rehash(newLength,dummyKey,dummyPos);
    };
    
    



    void WeakKeyHashTable_O::exposeCando(::core::Lisp_sp lisp)
    {
	::core::class_<WeakKeyHashTable_O>()
            .def("weakHashTableSize",&WeakKeyHashTable_O::tableSize)
	    ;
        CoreDefun(makeWeakKeyHashTable);
        CoreDefun(weakGethash);
        CoreDefun(weakSetfGethash);
        CoreDefun(weakRemhash);
        CoreDefun(weakClrhash);
        CoreDefun(weakSplat);
        CoreDefun(weakRehash);
    }
    
    void WeakKeyHashTable_O::exposePython(Lisp_sp lisp)
    {_G();
#ifdef USEBOOSTPYTHON
	PYTHON_CLASS(CorePkg,WeakKeyHashTable,"","",_lisp)
	    ;
#endif
    }

};

