#include "core/foundation.h"
#include "gcweak.h"
#include "core/object.h"

namespace gctools {

    /*! Does not need safeRun */
    uint WeakHashTable::sxhashKey(const value_type& key
#ifdef USE_MPS
                                  , mps_ld_s* locationDependencyP
#endif
        )
    {
        core::HashGenerator hg;
#ifdef USE_MPS
        if ( locationDependencyP && key.pointerp()) {
            mps_ld_add(locationDependencyP
                       ,gctools::_global_arena
                       ,key.base_ref().px_ref() );
        }
#endif
        hg.addPart(reinterpret_cast<uintptr_t>(key.base_ref().px_ref()));
        return hg.hash();
    }


    /*! No need to use safeRun - callers do that */
    int WeakHashTable::find(KeyBucketsType* keys, const value_type& key
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


    /*! No need to safeRun this - WeakHashTable::set does that */
    int WeakHashTable::trySet(core::T_sp tkey, core::T_sp value)
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
            ASSERT((*this->_Keys).deleted() > 0 );
            (*this->_Keys).setDeleted((*this->_Keys).deleted()-1);
            printf("%s:%d key was deletedp at %lu  deleted = %d\n", __FILE__, __LINE__, b, (*this->_Keys).deleted() );
        }
        (*this->_Values).set(b,value_type(value));
        return 1;
    }


    // ----------------------------------------------------------------------
    // ----------------------------------------------------------------------
    // ----------------------------------------------------------------------
    //
    // Use safeRun from here on down
    //
    // ----------------------------------------------------------------------
    // ----------------------------------------------------------------------
    // ----------------------------------------------------------------------

    int WeakHashTable::rehash(size_t newLength, const value_type& key, size_t& key_bucket)
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




/* %%MPS: If we fail to find 'key' in the table, and if mps_ld_isstale
 * returns true, then some of the keys in the table might have been
 * moved by the garbage collector: in this case we need to re-hash the
 * table. See topic/location.
 * Return (values value t) or (values nil nil)
 */
    core::T_mv WeakHashTable::gethash(core::T_sp tkey, core::T_sp defaultValue)
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
                        result_mv = Values(value,_lisp->_true());
                        return;
                    }
                    GCWEAK_LOG(BF("Falling through"));
                }
#ifdef USE_MPS
                if (key.pointerp() && mps_ld_isstale(&this->_LocationDependency, gctools::_global_arena, key.pointer() )) {
                    if (this->rehash( this->_Keys->length(), key, &pos)) {
                        T_sp value = (*this->_Values)[pos].backcast();
                        if ( value.sameAsKeyP() ) {
                            value = key.backcast();
                        }
                        result_mv = Values(value,_lisp->_true());
                        return
                            }
                }
#endif
                result_mv = Values(defaultValue,_Nil<core::T_O>());
                return;
            } );
        return result_mv;
    }



    void WeakHashTable::set( core::T_sp key, core::T_sp value )
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
                    ASSERT(res);
                }
            });
    }

    void WeakHashTable::remhash( core::T_sp tkey )
    {
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
#ifdef USE_BOEHM
                    IMPLEMENT_MEF(BF("Remove disappearing link to this entry"));
#endif
                    this->_Keys->set(b, value_type(value_type::deleted)); //[b] = value_type(gctools::tagged_ptr<T_O>::tagged_deleted);
                    (*this->_Keys).setDeleted((*this->_Keys).deleted()+1);
                    (*this->_Values)[b] = value_type(gctools::tagged_ptr<core::T_O>::tagged_unbound);
                }
            } );
    }



    void WeakHashTable::clrhash()
    {
        safeRun<void()>( [this] ()->void
            {
                size_t len = (*this->_Keys).length();
                for ( size_t i(0); i<len; ++i ) {
#ifdef USE_BOEHM
                    IMPLEMENT_MEF(BF("Remove disappearing links to this entry"));
#endif
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
