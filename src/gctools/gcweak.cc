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
#include <clasp/gctools/gcweak.h>
#include <clasp/core/object.h>
#ifdef USE_MPS
#include <clasp/mps/code/mps.h>
#endif

namespace gctools {

#if 0 //def USE_MPS
    void call_with_alloc_lock( fn_type fn, void* client_data)
    {
        fn(client_data);
    }
#endif

WeakHashTable::WeakHashTable(size_t length) {
  /* round up to next power of 2 */
  if (length == 0)
    length = 2;
  size_t l;
  for (l = 1; l < length; l *= 2)
    ;
  this->_Keys = KeyBucketsAllocatorType::allocate(l);
  this->_Values = ValueBucketsAllocatorType::allocate(l);
  this->_Keys->dependent = this->_Values;
  //  GCTOOLS_ASSERT((reinterpret_cast<uintptr_t>(this->_Keys->dependent) & 0x3) == 0);
  this->_Values->dependent = this->_Keys;
#ifdef USE_MPS
  mps_ld_reset(&this->_LocationDependency, _global_arena);
#endif
}

uint WeakHashTable::sxhashKey(const value_type &key
#ifdef USE_MPS
                              ,
                              mps_ld_s *locationDependencyP
#endif
                              ) {
#ifdef USE_MPS
  if (locationDependencyP && key.objectp()) {
    GCWEAK_LOG(BF("Calling mps_ld_add for key: %p") % (void *)key.raw_());
    mps_ld_add(locationDependencyP, gctools::_global_arena, key.raw_());
  }
#endif
  GCWEAK_LOG(BF("Calling lisp_hash for key: %p") % (void *)key.raw_());
  return core::lisp_hash(reinterpret_cast<uintptr_t>(key.raw_()));
}

/*! Return 0 if there is no more room in the sequence of entries for the key
	  Return 1 if the element is found or an unbound or deleted entry is found.
	  Return the entry index in (b)
	*/
int WeakHashTable::find(gctools::tagged_pointer<KeyBucketsType> keys, const value_type &key
#ifdef USE_MPS
                        ,
                        mps_ld_s *ldP
#endif
                        ,
                        size_t &b
#ifdef DEBUG_FIND
                        ,
                        bool debugFind, stringstream *reportP
#endif
                        ) {
  unsigned long i, h, probe;
  unsigned long l = keys->length() - 1;
  int result = 0;
#ifdef USE_MPS
  h = WeakHashTable::sxhashKey(key, ldP);
#else
  h = WeakHashTable::sxhashKey(key);
#endif

#ifdef DEBUG_FIND
  if (debugFind) {
    *reportP << __FILE__ << ":" << __LINE__ << " starting find key = " << (void *)(key.raw_()) << " h = " << h << " l = " << l << std::endl;
  }
#endif
  probe = (h >> 8) | 1;
  h &= l;
  i = h;
  do {
    value_type &k = (*keys)[i];
#ifdef DEBUG_FIND
    if (debugFind) {
      *reportP << "  i = " << i << "   k = " << (void *)(k.raw_()) << std::endl;
    }
#endif
    if (k.unboundp() || k == key) {
      b = i;
#ifdef DEBUG_FIND
      if (debugFind && k != key) {
        *reportP << "find  returning 1 b= " << b << " k = " << k.raw_() << std::endl;
      }
#endif
      return 1;
    }
#ifdef USE_BOEHM
    // Handle splatting
    if (!k.raw_()) {
      keys->set(i, value_type((Tagged)gctools::tag_deleted<core::T_O *>()));
      ValueBucketsType *values = dynamic_cast<ValueBucketsType *>(&*keys->dependent);
      (*values)[i] = value_type((Tagged)gctools::tag_unbound<core::T_O *>());
    }
#endif
    if (result == 0 && (k.deletedp())) {
      b = i;
      result = 1;
    }
    i = (i + probe) & l;
  } while (i != h);
  return result;
}

int WeakHashTable::rehash(size_t newLength, const value_type &key, size_t &key_bucket) {
  int result;
  safeRun<void()>([&result, this, newLength, &key, &key_bucket]() -> void {
		GCWEAK_LOG(BF("entered rehash newLength = %d") % newLength );
		size_t i, length;
		// buckets_t new_keys, new_values;
		result = 0;
		length = this->_Keys->length();
		MyType newHashTable(newLength);
		//new_keys = make_buckets(newLength, this->key_ap);
		//new_values = make_buckets(newLength, this->value_ap);
		//new_keys->dependent = new_values;
		//new_values->dependent = new_keys;
#ifdef USE_MPS
		GCWEAK_LOG(BF("Calling mps_ld_reset"));
		mps_ld_reset(&this->_LocationDependency,gctools::_global_arena);
#endif
		for (i = 0; i < length; ++i) {
		    value_type& old_key = (*this->_Keys)[i];
		    if (!old_key.unboundp() && !old_key.deletedp() && old_key.raw_() ) {
			int found;
			size_t b;
#ifdef USE_MPS
			found = WeakHashTable::find(newHashTable._Keys, old_key, &this->_LocationDependency, b);
#else
			found = WeakHashTable::find(newHashTable._Keys, old_key, b);
#endif
			GCTOOLS_ASSERT(found);// assert(found);            /* new table shouldn't be full */
			if ( !(*newHashTable._Keys)[b].unboundp() ) {
			    printf("%s:%d About to copy key: %12p   at index %zu    to newHashTable at index: %zu\n", __FILE__, __LINE__,
				   old_key.raw_(), i, b );
			    printf("Key = %s\n", core::lisp_rep(old_key).c_str());
			    printf("    original value@%p = %s\n", (*this->_Values)[i].raw_(), core::lisp_rep((*this->_Values)[i]).c_str());
			    printf("newHashTable value@%p = %s\n", (*newHashTable._Values)[b].raw_(), core::lisp_rep((*newHashTable._Values)[b]).c_str());
			    printf("--------- Original table\n");
			    printf("%s\n", this->dump("Original").c_str());
			    printf("--------- New table\n");
			    printf("%s\n", newHashTable.dump("Copy").c_str());
			}
			GCTOOLS_ASSERT((*newHashTable._Keys)[b].unboundp()); /* shouldn't be in new table */
			newHashTable._Keys->set(b,old_key);
			(*newHashTable._Values)[b] = (*this->_Values)[i];
			if (key && old_key == key ) {
			    key_bucket = b;
			    result = 1;
			}
			(*newHashTable._Keys).setUsed((*newHashTable._Keys).used()+1); // TAG_COUNT(UNTAG_COUNT(new_keys->used) + 1);
		    }
		}
		GCTOOLS_ASSERT( (*newHashTable._Keys).used() == (newHashTable.tableSize()) );
		// assert(UNTAG_COUNT(new_keys->used) == table_size(tbl));
		this->swap(newHashTable);
  });
  return result;
}

/*! trySet returns 0 only if there is no room in the hash-table */
int WeakHashTable::trySet(core::T_sp tkey, core::T_sp value) {
  GCWEAK_LOG(BF("Entered trySet with key %p") % tkey.raw_());
  size_t b;
  if (tkey == value) {
    value = gctools::make_tagged_sameAsKey<core::T_O>();
  }
  value_type key(tkey);
#ifdef DEBUG_TRYSET
  stringstream report;
  report << "About to trySet with the key " << tkey.raw_() << std::endl;
  report << this->dump("trySet-incoming WeakHashTable: ") << std::endl;
  // Count the number of times the key is in the table
  int alreadyThere = 0;
  int firstOtherIndex = 0;
  for (int i(0); i < (*this->_Keys).length(); ++i) {
    if ((*this->_Keys)[i] == tkey) {
      firstOtherIndex = i;
      ++alreadyThere;
    }
  }
#endif
#if USE_MPS
  int result = WeakHashTable::find(this->_Keys, key, NULL, b
#ifdef DEBUG_FIND
                                   ,
                                   alreadyThere, &report
#endif
                                   ); // &this->_LocationDependency,b);
#else
  int result = WeakHashTable::find(this->_Keys, key, b);
#endif
  if ((!result || (*this->_Keys)[b] != key)) {
    GCWEAK_LOG(BF("then case - Returned from find with result = %d     (*this->_Keys)[b=%d] = %p") % result % b % (*this->_Keys)[b].raw_());
#ifdef DEBUG_TRYSET
    if (alreadyThere) {
      report << "Could not find the key @" << key.raw_() << std::endl;
      if (!result) {
        report << " because find return result = " << result << " - which means that the hash table was searched and the key was not found before the hash list was exhausted" << std::endl;
      }
      if ((*this->_Keys)[b] != key) {
        report << " because find hit an empty (unbound or deleted) slot before it found the key" << std::endl;
      }
      report << " ...  about to check if mps_ld_isstale" << std::endl;
    }
#endif

#if USE_MPS
    GCWEAK_LOG(BF("About to call mps_ld_isstale"));
    if (mps_ld_isstale(&this->_LocationDependency, gctools::_global_arena, key.raw_())) {
      GCWEAK_LOG(BF("Key has gone stale"));
#ifdef DEBUG_TRYSET
      if (alreadyThere)
        report << " mps_ld_isstale returned TRUE - The key was stale - rehashing" << std::endl;
#endif
      // The key was not found and the address is stale - rehash
      size_t rehashb;
      if (this->rehash(this->_Keys->length(), key, rehashb)) {
#ifdef DEBUG_TRYSET
        if (alreadyThere)
          report << "Rehashed and found the key = " << rehashb << std::endl;
#endif
        GCWEAK_LOG(BF("rehashed table, key is in table rehashb = %d") % rehashb);
        b = rehashb;
        goto DO_SET;
      } else {
#ifdef DEBUG_TRYSET
        if (alreadyThere)
          report << "The table was rehashed but rehash returned 0" << std::endl;
#endif
// At this point the key definitely is NOT in the hash-table
#if USE_MPS
        int result2 = WeakHashTable::find(this->_Keys, key, &this->_LocationDependency, b
#ifdef DEBUG_FIND
                                          ,
                                          alreadyThere, &report
#endif
                                          ); // &this->_LocationDependency,b);
#else
        int result2 = WeakHashTable::find(this->_Keys, key, b);
#endif
        if (!result2) {
          GCWEAK_LOG(BF("Find returning 0 - a string of hash-table entries with the same hash did not match and had no empties"));
          return 0;
        }
        GCWEAK_LOG(BF("rehashed table, key is not in table new b = %d") % b);
      }
    } else {
      GCWEAK_LOG(BF("mps_ld_isstale returned false"));
#ifdef DEBUG_TRYSET
      if (alreadyThere) {
        report << __FILE__ << ":" << __LINE__ << " mps_ld_isstale returned FALSE for the key " << key.raw_() << "  !!!!!" << std::endl;
        report << "  despite the fact that key was NOT found even though it is at " << firstOtherIndex << std::endl;
        report << " some info...   result = " << result << "   _rep_(keys[b=" << b << "]) = " << core::lisp_rep((*this->_Keys)[b]) << std::endl;
      }
#endif // DEBUG_TRYSET
      GCWEAK_LOG(BF("Calling mps_ld_add for key: %p") % (void *)key.raw_());
      mps_ld_add(&this->_LocationDependency, gctools::_global_arena, key.raw_());
    }
#endif
  } else {
    GCWEAK_LOG(BF("else case - Returned from find with result = %d     (*this->_Keys)[b=%d] = %p") % result % b % (*this->_Keys)[b].raw_());
    GCWEAK_LOG(BF("Calling mps_ld_add for key: %p") % (void *)key.raw_());
#if USE_MPS
    mps_ld_add(&this->_LocationDependency, gctools::_global_arena, key.raw_());
#endif
  }
  if ((*this->_Keys)[b].unboundp()) {
    GCWEAK_LOG(BF("Writing key over unbound entry"));
    this->_Keys->set(b, key);
    (*this->_Keys).setUsed((*this->_Keys).used() + 1);
#ifdef DEBUG_GCWEAK
    printf("%s:%d key was unboundp at %zu  used = %d\n", __FILE__, __LINE__, b, this->_Keys->used());
#endif
  } else if ((*this->_Keys)[b].deletedp()) {
    GCWEAK_LOG(BF("Writing key over deleted entry"));
    this->_Keys->set(b, key);
    GCTOOLS_ASSERT((*this->_Keys).deleted() > 0);
    (*this->_Keys).setDeleted((*this->_Keys).deleted() - 1);
#ifdef DEBUG_GCWEAK
    printf("%s:%d key was deletedp at %zu  deleted = %d\n", __FILE__, __LINE__, b, (*this->_Keys).deleted());
#endif // DEBUG_GCWEAK
  }
#if USE_MPS
DO_SET:
#endif
  GCWEAK_LOG(BF("Setting value at b = %d") % b);
  (*this->_Values).set(b, value_type(value));
#ifdef DEBUG_TRYSET
  // Count the number of times the key is in the table
  int count = 0;
  int otherIndex;
  for (int i(0); i < (*this->_Keys).length(); ++i) {
    if ((*this->_Keys)[i] == tkey) {
      if (i != b) {
        otherIndex = i;
      }
      ++count;
    }
  }
  if (count > 1) {
    printf("Found %d duplicate keys %p in hash table\n", count, tkey.raw_());
    printf("Log of trySet action:\n%s\n", report.str().c_str());
    printf("The new key is at %zu and another instance of the key is at %d\n", b, otherIndex);
    printf("%s\n", this->dump("Final trySet table").c_str());
  }
#endif
  GCWEAK_LOG(BF("Leaving trySet"));
  return 1;
}

string WeakHashTable::dump(const string &prefix) {
  stringstream sout;
  safeRun<void()>([this, &prefix, &sout]() -> void {
		size_t i, length;
		length = this->_Keys->length();
		sout << "===== Dumping WeakHashTable length = " << length << std::endl;
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

core::T_mv WeakHashTable::gethash(core::T_sp tkey, core::T_sp defaultValue) {
  core::T_mv result_mv;
  safeRun<void()>([&result_mv, this, tkey, defaultValue]() -> void {
		value_type key(tkey);
		size_t pos;
		int result = gctools::WeakHashTable::find(this->_Keys,key
#ifdef USE_MPS
							  ,NULL
#endif
							  ,pos);
		if (result) { // WeakHashTable::find(this->_Keys,key,false,pos)) { //buckets_find(tbl, this->keys, key, NULL, &b)) {
		    value_type& k = (*this->_Keys)[pos];
		    GCWEAK_LOG(BF("gethash find successful pos = %d  k= %p k.unboundp()=%d k.base_ref().deletedp()=%d k.NULLp()=%d") % pos % k.raw_() % k.unboundp() % k.deletedp() % k.NULLp() );
		    if ( !k.unboundp() && !k.deletedp() ) {
			GCWEAK_LOG(BF("Returning success!"));
			core::T_sp value = smart_ptr<core::T_O>((*this->_Values)[pos]);
			if ( value.sameAsKeyP() ) {
			    value = smart_ptr<core::T_O>(k);
			}
			result_mv = Values(value,core::lisp_true());
			return;
		    }
		    GCWEAK_LOG(BF("Falling through"));
		}
#ifdef USE_MPS
		if (key.objectp() && mps_ld_isstale(&this->_LocationDependency, gctools::_global_arena, key.raw_() )) {
		    if (this->rehash( this->_Keys->length(), key, pos)) {
			core::T_sp value((*this->_Values)[pos]);
			if ( value.sameAsKeyP() ) {
			    value = smart_ptr<core::T_O>(key);
			}
			result_mv = Values(value,core::lisp_true());
			return;
		    }
		}
#endif
		result_mv = Values(defaultValue,_Nil<core::T_O>());
		return;
  });
  return result_mv;
}

void WeakHashTable::set(core::T_sp key, core::T_sp value) {
  safeRun<void()>([key, value, this]() -> void {
		if (this->fullp() || !this->trySet(key,value) ) {
		    int res;
		    value_type dummyKey;
		    size_t dummyPos;
		    this->rehash( (*this->_Keys).length() * 2, dummyKey, dummyPos );
		    res = this->trySet( key, value);
		    GCTOOLS_ASSERT(res);
		}
  });
}

void WeakHashTable::maphash(std::function<void(core::T_sp, core::T_sp)> const &fn) {
  safeRun<void()>(
      [fn, this]() -> void {
		size_t length = this->_Keys->length();
		for (int i = 0; i < length; ++i) {
		    value_type& old_key = (*this->_Keys)[i];
		    if (!old_key.unboundp() && !old_key.deletedp()) {
			core::T_sp tkey(old_key);
			core::T_sp tval((*this->_Values)[i]);
			fn(tkey,tval);
		    }
		}
      });
}

void WeakHashTable::remhash(core::T_sp tkey) {
  safeRun<void()>([this, tkey]() -> void {
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
			if(key.objectp() && !mps_ld_isstale(&this->_LocationDependency, gctools::_global_arena, key.raw_()))
			    return;
#endif
			if(!this->rehash( (*this->_Keys).length(), key, b))
			    return;
		    }
		if( !(*this->_Keys)[b].unboundp() &&
		    !(*this->_Keys)[b].deletedp() )
		    {
			this->_Keys->set(b, value_type(gctools::tag_deleted<core::T_O*>())); //[b] = value_type(gctools::tagged_ptr<T_O>::tagged_deleted);
			(*this->_Keys).setDeleted((*this->_Keys).deleted()+1);
			(*this->_Values)[b] = value_type(gctools::tag_unbound<core::T_O*>());
		    }
  });
}

void WeakHashTable::clrhash() {
  safeRun<void()>([this]() -> void {
		size_t len = (*this->_Keys).length();
		for ( size_t i(0); i<len; ++i ) {
		    this->_Keys->set(i,value_type((Tagged)gctools::tag_unbound<core::T_O*>()));
		    (*this->_Values)[i] = value_type((Tagged)gctools::tag_unbound<core::T_O*>());
		}
		(*this->_Keys).setUsed(0);
		(*this->_Keys).setDeleted(0);
#ifdef USE_MPS
		mps_ld_reset(&this->_LocationDependency,gctools::_global_arena);
#endif
  });
};
};

#ifdef USE_MPS
extern "C" {
using namespace gctools;
mps_res_t weak_obj_scan(mps_ss_t ss, mps_addr_t base, mps_addr_t limit) {
  MPS_SCAN_BEGIN(ss) {
    while (base < limit) {
      WeakObject *weakObj = reinterpret_cast<WeakObject *>(base);
      switch (weakObj->kind()) {
      case WeakBucketKind: {
        WeakBucketsObjectType *obj = reinterpret_cast<WeakBucketsObjectType *>(weakObj);
        MPS_FIX12(ss, reinterpret_cast<mps_addr_t *>(&obj->dependent));
        for (int i(0), iEnd(obj->length()); i < iEnd; ++i) {
          core::T_O *p = reinterpret_cast<core::T_O *>(obj->bucket[i].raw_());
          if (gctools::tagged_objectp(p) && MPS_FIX1(ss, p)) {
            core::T_O *pobj = gctools::untag_object<core::T_O *>(p);
            core::T_O *tag = gctools::tag<core::T_O *>(p);
            mps_res_t res = MPS_FIX2(ss, reinterpret_cast<mps_addr_t *>(&pobj));
            if (res != MPS_RES_OK)
              return res;
            if (pobj == NULL && obj->dependent) {
              obj->dependent->bucket[i] = WeakBucketsObjectType::value_type(gctools::make_tagged_deleted<core::T_O *>());
              obj->bucket[i] = WeakBucketsObjectType::value_type(gctools::make_tagged_deleted<core::T_O *>());
            } else {
              p = reinterpret_cast<core::T_O *>(reinterpret_cast<uintptr_t>(pobj) | reinterpret_cast<uintptr_t>(tag));
              obj->bucket[i].setRaw_(reinterpret_cast<gc::Tagged>(p)); //reinterpret_cast<gctools::Header_s*>(p);
            }
          }
        }
        base = (char *)base + sizeof(WeakBucketsObjectType) + sizeof(typename WeakBucketsObjectType::value_type) * obj->length();
      } break;
      case StrongBucketKind: {
        StrongBucketsObjectType *obj = reinterpret_cast<StrongBucketsObjectType *>(base);
        MPS_FIX12(ss, reinterpret_cast<mps_addr_t *>(&obj->dependent));
        for (int i(0), iEnd(obj->length()); i < iEnd; ++i) {
          // MPS_FIX12(ss,reinterpret_cast<mps_addr_t*>(&(obj->bucket[i].raw_())));
          core::T_O *p = reinterpret_cast<core::T_O *>(obj->bucket[i].raw_());
          if (gctools::tagged_objectp(p) && MPS_FIX1(ss, p)) {
            core::T_O *pobj = gctools::untag_object<core::T_O *>(p);
            core::T_O *tag = gctools::tag<core::T_O *>(p);
            mps_res_t res = MPS_FIX2(ss, reinterpret_cast<mps_addr_t *>(&pobj));
            if (res != MPS_RES_OK)
              return res;
            p = reinterpret_cast<core::T_O *>(reinterpret_cast<uintptr_t>(pobj) | reinterpret_cast<uintptr_t>(tag));
            obj->bucket[i].setRaw_((gc::Tagged)(p)); //reinterpret_cast<gctools::Header_s*>(p);
          }
        }
        base = (char *)base + sizeof(StrongBucketsObjectType) + sizeof(typename StrongBucketsObjectType::value_type) * obj->length();
      } break;
      case WeakMappingKind: {
        WeakMappingObjectType *obj = reinterpret_cast<WeakMappingObjectType *>(weakObj);
        MPS_FIX12(ss, reinterpret_cast<mps_addr_t *>(&obj->dependent));
        core::T_O *p = reinterpret_cast<core::T_O *>(obj->bucket.raw_());
        if (gctools::tagged_objectp(p) && MPS_FIX1(ss, p)) {
          core::T_O *pobj = gctools::untag_object<core::T_O *>(p);
          core::T_O *tag = gctools::tag<core::T_O *>(p);
          mps_res_t res = MPS_FIX2(ss, reinterpret_cast<mps_addr_t *>(&pobj));
          if (res != MPS_RES_OK)
            return res;
          if (p == NULL && obj->dependent) {
            obj->dependent->bucket = WeakBucketsObjectType::value_type(gctools::make_tagged_deleted<core::T_O *>());
            obj->bucket = WeakBucketsObjectType::value_type(gctools::make_tagged_deleted<core::T_O *>());
          } else {
            p = reinterpret_cast<core::T_O *>(reinterpret_cast<uintptr_t>(pobj) | reinterpret_cast<uintptr_t>(tag));
            obj->bucket.setRaw_((gc::Tagged)(p)); // raw_() = reinterpret_cast<core::T_O*>(p);
          }
        }
        base = (char *)base + sizeof(WeakMappingObjectType);
      } break;
      case StrongMappingKind: {
        StrongMappingObjectType *obj = reinterpret_cast<StrongMappingObjectType *>(base);
        MPS_FIX12(ss, reinterpret_cast<mps_addr_t *>(&obj->dependent));
        //                    MPS_FIX12(ss,reinterpret_cast<mps_addr_t*>(&(obj->bucket.raw_())));
        core::T_O *p = reinterpret_cast<core::T_O *>(obj->bucket.raw_());
        if (gctools::tagged_objectp(p) && MPS_FIX1(ss, p)) {
          core::T_O *pobj = gctools::untag_object<core::T_O *>(p);
          core::T_O *tag = gctools::tag<core::T_O *>(p);
          mps_res_t res = MPS_FIX2(ss, reinterpret_cast<mps_addr_t *>(&pobj));
          if (res != MPS_RES_OK)
            return res;
          p = reinterpret_cast<core::T_O *>(reinterpret_cast<uintptr_t>(pobj) | reinterpret_cast<uintptr_t>(tag));
          obj->bucket.setRaw_((gc::Tagged)(p)); //reinterpret_cast<gctools::Header_s*>(p);
        }
        base = (char *)base + sizeof(StrongMappingObjectType);
      } break;
      case WeakPointerKind: {
        WeakPointer *obj = reinterpret_cast<WeakPointer *>(base);
        // MPS_FIX12(ss,reinterpret_cast<mps_addr_t*>(&(obj->value.raw_())));
        core::T_O *p = reinterpret_cast<core::T_O *>(obj->value.raw_());
        if (gctools::tagged_objectp(p) && MPS_FIX1(ss, p)) {
          core::T_O *pobj = gctools::untag_object<core::T_O *>(p);
          core::T_O *tag = gctools::tag<core::T_O *>(p);
          mps_res_t res = MPS_FIX2(ss, reinterpret_cast<mps_addr_t *>(&pobj));
          if (res != MPS_RES_OK)
            return res;
          p = reinterpret_cast<core::T_O *>(reinterpret_cast<uintptr_t>(pobj) | reinterpret_cast<uintptr_t>(tag));
          obj->value.setRaw_((gc::Tagged)(p)); //reinterpret_cast<gctools::Header_s*>(p);
        }
        base = (char *)base + sizeof(WeakPointer);
      } break;
      default:
        THROW_HARD_ERROR(BF("Handle other weak kind %d") % weakObj->kind());
      }
    };
  }
  MPS_SCAN_END(ss);
  return MPS_RES_OK;
}

mps_addr_t weak_obj_skip(mps_addr_t base) {
  GCWEAK_LOG(BF("weak_obj_skip base=%p") % ((void *)base));
  WeakObject *weakObj = reinterpret_cast<WeakObject *>(base);
  switch (weakObj->kind()) {
  case WeakBucketKind: {
    WeakBucketsObjectType *obj = reinterpret_cast<WeakBucketsObjectType *>(weakObj);
    GCWEAK_LOG(BF("WeakBucketKind sizeof(WeakBucketsObjectType)=%d + sizeof(typename WeakBucketsObjectType::value_type)=%d * obj->length()=%d") % sizeof(WeakBucketsObjectType) % sizeof(typename WeakBucketsObjectType::value_type) % obj->length());
    base = (char *)base + sizeof(WeakBucketsObjectType) + sizeof(typename WeakBucketsObjectType::value_type) * obj->length();
  } break;
  case StrongBucketKind: {
    StrongBucketsObjectType *obj = reinterpret_cast<StrongBucketsObjectType *>(base);
    GCWEAK_LOG(BF("StrongBucketKind sizeof(StrongBucketsObjectType)=%d + sizeof(typename StrongBucketsObjectType::value_type)=%d * obj->length()=%d") % sizeof(StrongBucketsObjectType) % sizeof(typename StrongBucketsObjectType::value_type) % obj->length());
    base = (char *)base + sizeof(StrongBucketsObjectType) + sizeof(typename StrongBucketsObjectType::value_type) * obj->length();
  } break;
  case WeakMappingKind: {
    GCWEAK_LOG(BF("WeakMappingKind"));
    base = (char *)base + sizeof(WeakMappingObjectType);
  } break;
  case StrongMappingKind: {
    GCWEAK_LOG(BF("StrongMappingKind"));
    base = (char *)base + sizeof(StrongMappingObjectType);
  } break;
  case WeakPointerKind: {
    GCWEAK_LOG(BF("WeakPointerKind"));
    base = (char *)base + sizeof(WeakPointer);
  } break;
  case WeakFwdKind: {
    GCWEAK_LOG(BF("WeakFwdKind"));
    weak_fwd_s *obj = reinterpret_cast<weak_fwd_s *>(base);
    base = (char *)base + Align(obj->size.unsafe_fixnum());
  } break;
  case WeakFwd2Kind: {
    GCWEAK_LOG(BF("WeakFwd2Kind"));
    base = (char *)base + Align(sizeof(weak_fwd2_s));
  } break;
  case WeakPadKind: {
    GCWEAK_LOG(BF("WeakPadKind"));
    weak_pad_s *obj = reinterpret_cast<weak_pad_s *>(base);
    base = (char *)base + Align(obj->size.unsafe_fixnum());
  } break;
  case WeakPad1Kind: {
    GCWEAK_LOG(BF("WeakPad1Kind"));
    base = (char *)base + Align(sizeof(weak_pad1_s));
  }
  default:
    THROW_HARD_ERROR(BF("Handle weak_obj_skip other weak kind %d") % weakObj->kind());
  }
  GCWEAK_LOG(BF("weak_obj_skip returning base=%p") % ((void *)base));
  return base;
};

void weak_obj_fwd(mps_addr_t old, mps_addr_t newv) {
  WeakObject *weakObj = reinterpret_cast<WeakObject *>(old);
  mps_addr_t limit = weak_obj_skip(old);
  size_t size = (char *)limit - (char *)old;
  assert(size >= Align(sizeof(weak_fwd2_s)));
  if (size == Align(sizeof(weak_fwd2_s))) {
    weak_fwd2_s *weak_fwd2_obj = reinterpret_cast<weak_fwd2_s *>(weakObj);
    weak_fwd2_obj->setKind(WeakFwd2Kind);
    weak_fwd2_obj->fwd = reinterpret_cast<WeakObject *>(newv);
  } else {
    weak_fwd_s *weak_fwd_obj = reinterpret_cast<weak_fwd_s *>(weakObj);
    weak_fwd_obj->setKind(WeakFwdKind);
    weak_fwd_obj->fwd = reinterpret_cast<WeakObject *>(newv);
    weak_fwd_obj->size = gc::make_tagged_fixnum<core::Fixnum_I *>(size);
  }
}

mps_addr_t weak_obj_isfwd(mps_addr_t addr) {
  WeakObject *obj = reinterpret_cast<WeakObject *>(addr);
  switch (obj->kind()) {
  case WeakFwd2Kind: {
    weak_fwd2_s *weak_fwd2_obj = reinterpret_cast<weak_fwd2_s *>(obj);
    return weak_fwd2_obj->fwd;
  } break;
  case WeakFwdKind: {
    weak_fwd_s *weak_fwd_obj = reinterpret_cast<weak_fwd_s *>(obj);
    return weak_fwd_obj->fwd;
  }
  }
  return NULL;
}

void weak_obj_pad(mps_addr_t addr, size_t size) {
  WeakObject *weakObj = reinterpret_cast<WeakObject *>(addr);
  assert(size >= Align(sizeof(weak_pad1_s)));
  if (size == Align(sizeof(weak_pad1_s))) {
    weakObj->setKind(WeakPad1Kind);
  } else {
    weakObj->setKind(WeakPadKind);
    weak_pad_s *weak_pad_obj = reinterpret_cast<weak_pad_s *>(addr);
    weak_pad_obj->size = gctools::make_tagged_fixnum<core::Fixnum_I *>(size);
  }
}
};
#endif
