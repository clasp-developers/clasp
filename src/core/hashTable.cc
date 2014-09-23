/*
    File: hashTable.cc
*/

/*
Copyright (c) 2014, Christian E. Schafmeister
 
CLASP is free software; you can redistribute it and/or
modify it under the terms of the GNU Library General Public
License as published by the Free Software Foundation; either
version 2 of the License, or (at your option) any later version.
 
See file 'clasp/Copyright' for full details.
 
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
#define	DEBUG_LEVEL_FULL

#include <limits>
#include "core/common.h"
#include "core/corePackage.h"
#include "core/environment.h"
#include "multipleValues.h"
#include "symbolTable.h"
#include "hashTable.h"
#include "hashTableEq.h"
#include "hashTableEql.h"
#include "hashTableEqual.h"
#include "hashTableEqualp.h"
#include "vectorObjects.h"
#include "fileSystem.h"
#include "serialize.h"
#include "evaluator.h"
#include "designators.h"
#include "core/wrappers.h"
namespace core
{



#ifdef USE_MPS
        static int LockDepth = 0;
    struct HashTableLocker {
        HashTableLocker() {
            if ( LockDepth == 0 ) {
//                printf("%s:%d clamping the arena\n", __FILE__, __LINE__ );
                mps_arena_clamp(gctools::_global_arena);
            }
            ++LockDepth;
        };
        ~HashTableLocker() {
            if ( LockDepth == 1 ) {
//                printf("%s:%d releasing the arena\n", __FILE__, __LINE__ );
                mps_arena_release(gctools::_global_arena);
            }
            --LockDepth;
        }
    };
#define HASH_TABLE_LOCK() HashTableLocker zzzzHashTableLocker;
#endif
#ifdef USE_BOEHM
#define HASH_TABLE_LOCK()
#endif


// ----------------------------------------------------------------------
//

    EXPOSE_CLASS(core,HashTable_O);


#define LOCK_af_make_hash_table 1
#define DOCS_af_make_hash_table "see CLHS"
#define	ARGS_af_make_hash_table "(&key (test (function eql)) (size 16) (rehash-size 1.5) (rehash_threshold 1.0))"
#define DECL_af_make_hash_table ""    
    HashTable_mv af_make_hash_table(T_sp test, Fixnum_sp size, Number_sp rehash_size, DoubleFloat_sp orehash_threshold)
    {_G();
	int isize = size->as_int();
	double rehash_threshold = orehash_threshold->get();
	HashTable_sp table = _Nil<HashTable_O>();
//	_lisp->print(BF("%s:%d - make_hash_table - fix me so that I grow by powers of 2\n") % __FILE__ % __LINE__ );
	if ( test == cl::_sym_eq || test == cl::_sym_eq->symbolFunction() )
	{
	    table = HashTableEq_O::create(isize,rehash_size,rehash_threshold);
	} else if ( test == cl::_sym_eql || test == cl::_sym_eql->symbolFunction() )
	{
	    table = HashTableEql_O::create(isize,rehash_size,rehash_threshold);
	} else if ( test == cl::_sym_equal || test == cl::_sym_equal->symbolFunction() )
	{
	    table = HashTableEqual_O::create(isize,rehash_size,rehash_threshold);
	} else if ( test == cl::_sym_equalp || test == cl::_sym_equalp->symbolFunction() )
	{
	    table = HashTableEqualp_O::create(isize,rehash_size,rehash_threshold);
	} else
	{
	    SIMPLE_ERROR(BF("Illegal test[%s] for make-hash-table") % _rep_(test) );
	}
	return(Values(table));
    }



    HashTable_sp HashTable_O::create(T_sp test)
    {_G();
	Fixnum_sp size = Fixnum_O::create(16);
	DoubleFloat_sp rehashSize = DoubleFloat_O::create(2.0);
	DoubleFloat_sp rehashThreshold = DoubleFloat_O::create(0.9);
	HashTable_sp ht = af_make_hash_table(test,size,rehashSize,rehashThreshold);
	return ht;
    }


#define ARGS_af_maphash "(function_desig hash_table)"
#define DECL_af_maphash ""    
#define	DOCS_af_maphash "see CLHS"
#define	FILE_af_maphash __FILE__
#define LINE_af_maphash __LINE__
    T_mv af_maphash(T_sp function_desig, HashTable_sp hash_table)
    {_G();
//        printf("%s:%d starting maphash on hash-table@%p\n", __FILE__, __LINE__, hash_table.px_ref());
	Function_sp func = coerce::functionDesignator(function_desig);
        if ( hash_table.nilp() ) {
            SIMPLE_ERROR(BF("maphash called with nil hash-table"));
        }
//        HASH_TABLE_LOCK();
        VectorObjects_sp table = hash_table->_HashTable;
	for ( size_t it=0, itEnd = cl_length(table); it<itEnd; ++it )
	{
	    Cons_sp first = (*table)[it].as<Cons_O>();
	    for ( Cons_sp cur=first; cur.notnilp(); cur = cCdr(cur) )
	    {
		Cons_sp entry = oCar(cur).as<Cons_O>();
                T_sp key = oCar(entry);
                T_sp value = oCdr(entry);
                if ( value.notunboundp() ) {
                    eval::funcall(func,key,value);
                }
	    }
	}
//        printf("%s:%d finished maphash on hash-table@%p\n", __FILE__, __LINE__, hash_table.px_ref());
	return(Values(_Nil<T_O>()));
    }


#define ARGS_af_clrhash "(hash_table)"
#define DECL_af_clrhash ""    
#define DOCS_af_clrhash "See CLHS"
    T_mv af_clrhash(HashTable_sp hash_table)
    {_G();
	hash_table->clrhash();
	return(Values(_Nil<T_O>()));
    };




    
    
#define ARGS_af_hashTableEntryDeletedP "(cons)"
#define DECL_af_hashTableEntryDeletedP ""
#define DOCS_af_hashTableEntryDeletedP "hashTableEntryDeletedP"
    bool af_hashTableEntryDeletedP(Cons_sp cons)
    {_G();
        return oCdr(cons).unboundp();
    };




    
    
#define ARGS_af_hash_eql "(&rest args)"
#define DECL_af_hash_eql ""
#define DOCS_af_hash_eql "hash_eql generates an eql hash for a list of objects"
    int af_hash_eql(Cons_sp args)
    {_G();
	HashGenerator hg;
	for ( Cons_sp cur=args; cur.notnilp(); cur=cCdr(cur) )
	{	
	    HashTable_O::sxhash_eql(hg,oCar(cur),NULL);
	    if ( hg.isFull() ) break;
	}
	return hg.hash();
    };


#define ARGS_af_hash_equal "(&rest args)"
#define DECL_af_hash_equal ""
#define DOCS_af_hash_equal "hash_equal generates an equal hash for a list of objects"
    int af_hash_equal(Cons_sp args)
    {_G();
	HashGenerator hg;
	for ( Cons_sp cur=args; cur.notnilp(); cur=cCdr(cur) )
	{	
	    HashTable_O::sxhash_equal(hg,oCar(cur),NULL);
	    if ( hg.isFull() ) break;
	}
	return hg.hash();
    };


#define ARGS_af_hash_equalp "(&rest args)"
#define DECL_af_hash_equalp ""
#define DOCS_af_hash_equalp "hash_equalp generates an equalp hash for a list of objects"
    int af_hash_equalp(Cons_sp args)
    {_G();
	HashGenerator hg;
	for ( Cons_sp cur=args; cur.notnilp(); cur=cCdr(cur) )
	{	
	    HashTable_O::sxhash_equalp(hg,oCar(cur),NULL);
	    if ( hg.isFull() ) break;
	}
	return hg.hash();
    };







    
    
#define ARGS_af_remhash "(key hashtable)"
#define DECL_af_remhash ""
#define DOCS_af_remhash "remhash"
    bool af_remhash(T_sp key, HashTable_sp ht)
    {_G();
	return ht->remhash(key);
    };




    void HashTable_O::clrhash()
    {_G();
	ASSERT(!this->_RehashSize->zerop());
	this->setup(4,this->_RehashSize,this->_RehashThreshold);
    }



    void HashTable_O::setup( uint sz,  Number_sp rehashSize, double rehashThreshold)
    {_OF();
	sz = this->resizeEmptyTable(sz);
	this->_InitialSize = sz;
	this->_RehashSize = rehashSize;
	ASSERT(!this->_RehashSize->zerop());
	this->_RehashThreshold = rehashThreshold;
    }







    void HashTable_O::sxhash_eq(HashGenerator& hg, T_sp obj, LocationDependencyPtrT ld )
    {_G();
	if ( obj.nilp() ) {
	    hg.addPart(0);
	    return;
	}
#ifdef USE_MPS
        if (ld) mps_ld_add(ld,gctools::_global_arena, SmartPtrToBasePtr(obj));
#endif
	obj->T_O::sxhash(hg);
    }

						    

    void HashTable_O::sxhash_eql(HashGenerator& hg, T_sp obj, LocationDependencyPtrT ld )
    {_G();
	if ( obj.nilp() )
	{
	    hg.addPart(0);
	    return;
	} else if ( af_numberP(obj) || af_characterP(obj) )
	{
	    hg.hashObject(obj);
	    return;
	}
#ifdef USE_MPS
        if (ld) mps_ld_add(ld, gctools::_global_arena, SmartPtrToBasePtr(obj));
#endif
	obj->T_O::sxhash(hg);
    }


    void HashTable_O::sxhash_equal(HashGenerator& hg, T_sp obj, LocationDependencyPtrT ld )
    {_G();
	if ( obj.nilp() )
	{
	    hg.addPart(0);
	    return;
	} else if ( af_symbolp(obj) || af_numberP(obj) || af_stringP(obj) || af_pathnamep(obj) || af_consP(obj) )
	{
	    hg.hashObject(obj);
	    return;
	}
#ifdef USE_MPS
        if (ld) mps_ld_add(ld, gctools::_global_arena, SmartPtrToBasePtr(obj));
#endif
	obj->T_O::sxhash(hg);
    }


    void HashTable_O::sxhash_equalp(HashGenerator& hg, T_sp obj, LocationDependencyPtrT ld )
    {_G();
        IMPLEMENT_MEF(BF("Implement HashTable_O::sxhash_equalp"));
#if 0
	if ( obj.nilp() )
	{
	    hg.addPart(0);
	    return;
	} else if ( af_symbolp(obj)
                    || af_numberP(obj)
                    || af_stringP(obj)
                    || af_pathnamep(obj)
                    || af_consP(obj)
                    || obj->instancep()
                    || af_arrayP(obj))
	{
	    hg.hashObject(obj);
	    return;
	}
	obj->T_O::sxhash(hg);
#endif
    }

    bool HashTable_O::equalp(T_sp other) const
    {
        IMPLEMENT_MEF(BF("Implement HashTable_O::equalp"));
    }









	
#if 0
    void HashTable_O::serialize(::serialize::SNodeP node)
    {
        this->Bases::serialize(node);
	// Archive other instance variables here
	node->attribute("test",this->_Test);
	node->archiveObject("rehashSize",this->_RehashSize);
	node->attribute("rehashThreshold",this->_RehashThreshold);
    }
#endif

    void HashTable_O::archiveBase(::core::ArchiveP node)
    {
	SYMBOL_EXPORT_SC_(KeywordPkg,rehashSize);
	SYMBOL_EXPORT_SC_(KeywordPkg,rehashThreshold);
	node->attribute(kw::_sym_rehashSize,this->_RehashSize);
	node->attribute(kw::_sym_rehashThreshold,this->_RehashThreshold);
	if ( node->loading() ) {
	    this->clrhash();
	    node->mapVector( [this] (T_sp keyValue) {
		    T_sp key = oCar(keyValue);
		    T_sp val = oCdr(keyValue);
		    this->hash_table_setf_gethash(key,val);
		} );
	} else {
	    this->mapHash( [&node] (T_sp key, T_sp val) {
		    Cons_sp keyValue = Cons_O::create(key,val);
		    node->pushVector(keyValue);
		} );
	}
    }



    uint HashTable_O::resizeEmptyTable(uint sz)
    {_OF();
	if ( sz < 4 ) sz = 4;
	this->_HashTable = VectorObjects_O::make(_Nil<Cons_O>(),_Nil<Cons_O>(),sz,false);
#ifdef USE_MPS
        mps_ld_reset(const_cast<mps_ld_t>(&(this->_LocationDependencyTracker)),gctools::_global_arena);
#endif
	return sz;
    }




    uint HashTable_O::hashTableCount() const
    {_OF();
	return this->_HashTableCount;
    }

    uint HashTable_O::calculateHashTableCount() const
    {_OF();
	uint cnt = 0;
	for ( size_t it(0), itEnd(cl_length(this->_HashTable)); it<itEnd; ++it )
	{
	    cnt += cl_length((this->_HashTable->operator[](it)).as_or_nil<Cons_O>());
	}
	return cnt;
    }



    uint HashTable_O::hashTableSize() const
    {_OF();
	return cl_length(this->_HashTable);
    }




    bool HashTable_O::keyTest(T_sp entryKey, T_sp searchKey) const
    {_OF();
	SUBCLASS_MUST_IMPLEMENT();
    }

    int HashTable_O::sxhashKey(T_sp obj,int bound, bool willAddKey ) const
    {_OF();
	SUBCLASS_MUST_IMPLEMENT();
    }

    Cons_sp HashTable_O::findAssoc(uint index, T_sp key) const
    {_OF();
	for ( Cons_sp cur = this->_HashTable->operator[](index).as_or_nil<Cons_O>();
	      cur.notnilp(); cur = cCdr(cur) )
	{
            Cons_sp pair = cCar(cur);
	    if ( this->keyTest(oCar(pair),key) )
	    {
//		LOG(BF("Found match: %s") % cur->__repr__());
		return pair;
	    }
	}
	return _Nil<Cons_O>();
    }



    
    
#define ARGS_cl_gethash "(key hash-table &optional default_value)"
#define DECL_cl_gethash ""
#define DOCS_cl_gethash "gethash"
    T_mv cl_gethash(T_sp key, T_sp hashTable, T_sp default_value)
    {_G();
        HashTable_sp ht = hashTable.as<HashTable_O>();
        return ht->gethash(key,default_value);
    };



    Cons_sp HashTable_O::bucketsFind(T_sp key) const
    {
        ASSERT(this->_HashTable);
        uint index = this->sxhashKey(key,cl_length(this->_HashTable), false );
        Cons_sp keyValueCons = this->findAssoc(index,key);
        return keyValueCons;
    }


    Cons_sp HashTable_O::tableRef(T_sp key)
    {
        Cons_sp keyValueCons = this->bucketsFind(key);
        if ( keyValueCons.notnilp() ) return keyValueCons;
#ifdef USE_MPS
        // Location dependency test if key is stale
        if (key.pointerp()) {
            void* blockAddr = SmartPtrToBasePtr(key);
            if (mps_ld_isstale(const_cast<mps_ld_t>(&(this->_LocationDependencyTracker)),gctools::_global_arena,blockAddr)) {
                keyValueCons = this->rehash(false,key);
            }
        }
#endif
        return keyValueCons;
    }

#define ARGS_HashTable_O_gethash "(key (self hash-table) &optional default_value)"
#define DECL_HashTable_O_gethash ""
#define DOCS_HashTable_O_gethash "See CLHS"
    T_mv HashTable_O::gethash(T_sp key,T_sp default_value )
    {_OF();
	LOG(BF("gethash looking for key[%s]") % _rep_(key) );
        Cons_sp keyValuePair = this->tableRef(key);
	LOG(BF("Found keyValueCons"));// % keyValueCons->__repr__() ); INFINITE-LOOP
	if ( keyValuePair.nilp() ) {
	    LOG(BF("valueOrUnbound is unbound - returning default"));
	    return(Values(default_value,_Nil<T_O>()));
	}
        T_sp value = oCdr(keyValuePair);
        if ( value.unboundp() ) {
	    LOG(BF("valueOrUnbound is unbound - returning default"));
	    return(Values(default_value,_Nil<T_O>()));
	}
	LOG(BF("Found assoc - returning")); // : %s") % res->__repr__() );  INFINITE-LOOP
	return(Values(value,_lisp->_true()));
    }


    Cons_sp HashTable_O::find(T_sp key)
    {
        Cons_sp keyValue = this->tableRef(key);
        if ( keyValue.nilp() ) return keyValue;
        if ( oCdr(keyValue).unboundp() ) return _Nil<Cons_O>();
        return keyValue;
    }

    bool HashTable_O::contains(T_sp key)
    {
        Cons_sp keyValue = this->find(key);
        if ( keyValue.nilp() ) return false;
        return true;
    }

#define ARGS_HashTable_O_remhash "(self key)"
#define DECL_HashTable_O_remhash ""
#define DOCS_HashTable_O_remhash "setf into the hash-table"
    bool HashTable_O::remhash(T_sp key)
    {_OF();
        Cons_sp keyValuePair = this->tableRef(key);
        if ( keyValuePair.nilp() || oCdr(keyValuePair).unboundp() ) return false;
        keyValuePair->setOCdr(_Unbound<T_O>());
        return true;
    }




#define ARGS_HashTable_O_hash_table_setf_gethash "(self key value)"
#define DECL_HashTable_O_hash_table_setf_gethash ""
#define DOCS_HashTable_O_hash_table_setf_gethash "setf into the hash-table"
    T_sp HashTable_O::hash_table_setf_gethash(T_sp key, T_sp value)
    {_OF();
//        printf("%s:%d key@%p value@%p\n", __FILE__, __LINE__, key.px_ref(), value.px_ref() );
        Cons_sp keyValuePair = this->tableRef(key);
        if ( keyValuePair.nilp() ) {
            uint index = this->sxhashKey(key,cl_length(this->_HashTable),true /*Will add key*/);
            Cons_sp newKeyValue = Cons_O::create(key,value);
//            printf("%s:%d  Inserted newKeyValue@%p\n", __FILE__, __LINE__, newKeyValue.px_ref());
            Cons_sp newEntry = Cons_O::create(newKeyValue,this->_HashTable->operator[](index));
            this->_HashTable->operator[](index) = newEntry;
//            this->_HashTableEntryCount++;
            ++(this->_HashTableCount);
        } else if ( oCdr(keyValuePair).unboundp() )
        {
            keyValuePair->setOCdr(value);
            ++(this->_HashTableCount);
        } else {
            keyValuePair->setOCdr(value);
        }
        if ( this->_HashTableCount > this->_RehashThreshold*cl_length(this->_HashTable) )
        {
            LOG(BF("Expanding hash table"));
            this->rehash(true,_Unbound<T_O>());
        }
        return value;
    }

    Cons_sp HashTable_O::rehash(bool expandTable, T_sp findKey )
    {_OF();
//        printf("%s:%d rehash of hash-table@%p\n", __FILE__, __LINE__,  this );
	ASSERTF(!this->_RehashSize->zerop(),BF("RehashSize is zero - it shouldn't be"));
	ASSERTF(cl_length(this->_HashTable) != 0, BF("HashTable is empty in expandHashTable - this shouldn't be"));
        Cons_sp foundKeyValuePair(_Nil<Cons_O>());
        uint startCount = this->hashTableCount();
	LOG(BF("At start of expandHashTable current hash table size: %d") % startSize );
        uint newSize = 0;
        if ( expandTable ) {
            if ( af_integerP(this->_RehashSize) ) {
                newSize = cl_length(this->_HashTable) + this->_RehashSize.as<Integer_O>()->as_int();
            } else if ( af_floatP(this->_RehashSize) ) {
                newSize = cl_length(this->_HashTable) * this->_RehashSize->as_double();
            }
        } else {
            newSize = cl_length(this->_HashTable);
        }
	VectorObjects_sp oldTable = this->_HashTable;
	newSize = this->resizeEmptyTable(newSize);
	LOG(BF("Resizing table to size: %d") % newSize );
	for ( size_t it(0), itEnd(cl_length(oldTable)); it<itEnd; ++it )
	{
	    {_BLOCK_TRACEF(BF("Re-indexing hash table row index[%d]") % it );
#ifdef DEBUG_ALL
		stringstream sk;
		for (Cons_sp scur=oldTable->operator[](it);scur.notnilp(); scur=cCdr(scur))
		{
		    sk << scur->ocar().as_or_nil<Cons_O>()->ocar()->__repr__() << " ";
		}
		LOG(BF("About to re-index hash table row index[%d] keys: %s") % (it-oldTable.begin()) % sk.str());
#endif
		Cons_sp next;
		for ( Cons_sp cur = oldTable->operator[](it).as_or_nil<Cons_O>(); cur.notnilp(); cur = next )
		{
		    next = cCdr(cur);
                    Cons_sp pair = cCar(cur);
		    T_sp key = oCar(pair);
                    T_sp value = oCdr(pair);
                    if ( value.notunboundp() ) {
                        // key/value represent a valid entry in the hash table
                        //
                        // If findKey is not unbound and we haven't already found
                        // the value that it points to.
                        // then while we are rehashing the hash table we are also looking
                        // for the key it points to.
                        // Check if the current key matches findKey and if it does
                        // set foundKeyValuePair so that it will be returned when
                        // the rehash is complete.
                        if ( foundKeyValuePair.nilp() && !findKey.unboundp() ) {
                            if ( this->keyTest(key,findKey) ) {
                                foundKeyValuePair = pair;
                            }
                        }
                        uint index = this->sxhashKey(key,cl_length(this->_HashTable),true /* Will add key */);
                        LOG(BF("Re-indexing key[%s] to index[%d]") % _rep_(key) % index );
                        Cons_sp newCur = Cons_O::create(pair,this->_HashTable->operator[](index).as_or_nil<Cons_O>());
                        this->_HashTable->operator[](index) = newCur;
                    }
		}
	    }
	}
        uint endCount = this->hashTableCount();
        if (startCount!=endCount) {
            SIMPLE_ERROR(BF("After rehash the hash-table-count is %d but at start it was %d") % endCount % startCount );
        }
        return foundKeyValuePair;
    }
    

    string HashTable_O::__repr__() const
    {
	stringstream ss;
	ss << "#<" << this->_instanceClass()->classNameAsString() << " :count " << this->_HashTableCount;
        ss << " :total-alist-entries " << this->calculateHashTableCount();
	ss << " @" << (void*)(this) <<  "> ";
	return ss.str();
//	return this->hash_table_dump();
    }


#define ARGS_HashTable_O_hash_table_dump "()"
#define DECL_HashTable_O_hash_table_dump ""
#define DOCS_HashTable_O_hash_table_dump "Dump the hash-table"
#define DUMP_LOW_LEVEL 1
    string HashTable_O::hash_table_dump() const
    {_OF();
	stringstream ss;
#ifndef DUMP_LOW_LEVEL
	ss << "#<" << this->_instanceClass()->classNameAsString() << std::endl;
#endif
	for ( size_t it(0),itEnd(cl_length(this->_HashTable)); it<itEnd; ++it )
	{
	    Cons_sp first = this->_HashTable->operator[](it).as_or_nil<Cons_O>();
	    ss << "HashTable["<<it<<"]: " << std::endl;
	    for ( Cons_sp cur=first; cur.notnilp(); cur = cCdr(cur) )
	    {
                Cons_sp pair = oCar(cur).as<Cons_O>();
                T_sp key = oCar(pair);
                T_sp value = oCdr(pair);
#ifdef DUMP_LOW_LEVEL
                ss << "     (" << key.px_ref() << ", " << value.px_ref() << ")@" << pair.px_ref() << " "<< std::endl;
#else
		ss << "     " << _rep_(pair) << std::endl;
#endif
	    }
	}
#ifndef DUMP_LOW_LEVEL
	ss << "> " << std::endl;
#endif
	return ss.str();
    }


    void HashTable_O::mapHash(std::function<void(T_sp,T_sp)> const& fn)
    {
//        HASH_TABLE_LOCK();
        VectorObjects_sp table = this->_HashTable;
	for ( size_t it(0),itEnd(cl_length(table)); it<itEnd; ++it )
	{
	    Cons_sp first = (*table)[it].as<Cons_O>();
	    for ( Cons_sp cur=first; cur.notnilp(); cur = cCdr(cur) )
	    {
                Cons_sp pair = cCar(cur);
                T_sp key = oCar(pair);
                T_sp value = oCdr(pair);
                if ( value.notunboundp()) fn(key,value);
	    }
	}
    }

    void HashTable_O::terminatingMapHash(std::function<bool(T_sp,T_sp)> const& fn)
    {
//        HASH_TABLE_LOCK();
        VectorObjects_sp table = this->_HashTable;
	for ( size_t it(0),itEnd(cl_length(table)); it<itEnd; ++it )
	{
	    Cons_sp first = (*table)[it].as<Cons_O>();
	    for ( Cons_sp cur=first; cur.notnilp(); cur = cCdr(cur) )
	    {
                Cons_sp pair = cCar(cur);
                T_sp key = oCar(pair);
                T_sp value = oCdr(pair);
                if ( value.notunboundp()) {
                    bool cont = fn(key,value);
                    if (!cont) return;
                }
	    }
	}
    }

    void HashTable_O::lowLevelMapHash(KeyValueMapper* mapper) const
    {_OF();
//        HASH_TABLE_LOCK();
        VectorObjects_sp table = this->_HashTable;
	for ( size_t it(0),itEnd(cl_length(table)); it<itEnd; ++it )
	{
	    Cons_sp first = (*table)[it].as<Cons_O>();
	    for ( Cons_sp cur=first; cur.notnilp(); cur = cCdr(cur) )
	    {
                Cons_sp pair = cCar(cur);
                T_sp key = oCar(pair);
                T_sp value = oCdr(pair);
                if ( value.notunboundp() ) {
                    if (!mapper->mapKeyValue(oCar(pair),oCdr(pair))) goto DONE;
                }
	    }
	}
    DONE: return;
    }


    int HashTable_O::hashTableNumberOfHashes() const
    {
	return cl_length(this->_HashTable);
    }

    Cons_sp HashTable_O::hashTableAlistAtHash(int hash) const
    {
	ASSERTF(hash >=0 && hash < cl_length(this->_HashTable),BF("Illegal hash value[%d] must between [0,%d)") % hash % cl_length(this->_HashTable));
	return this->_HashTable->operator[](hash).as_or_nil<Cons_O>();
    }


    string HashTable_O::keysAsString()
    {
        stringstream ss;
        this->maphash( [&ss,this] (T_sp key, T_sp val) {
                ss << _rep_(key) << " ";
            } );
        return ss.str();
    }


    void HashTable_O::exposeCando(::core::Lisp_sp lisp)
    {_G();
	::core::class_<HashTable_O> ht;
	ht
//	.initArgs("(self)")
            .def("hash-table-count",&HashTable_O::hashTableCount)
            .def("hash-table-size",&HashTable_O::hashTableSize)
            .def("hash-table-rehash-size",&HashTable_O::hashTableRehashSize)
            .def("hash-table-rehash-threshold",&HashTable_O::hashTableRehashThreshold)
	    .def("hash-table-test",&HashTable_O::hashTableTest)
	    .def("core:hashTableNumberOfHashes",&HashTable_O::hashTableNumberOfHashes)
	    .def("core:hashTableAlistAtHash",&HashTable_O::hashTableAlistAtHash)
            ;
	ht
	    .def("core:hashTableSetfGethash",&HashTable_O::hash_table_setf_gethash)
	    .def("core:hashTableDump", &HashTable_O::hash_table_dump)
            ;
	SYMBOL_EXPORT_SC_(ClPkg,make_hash_table);
	Defun(make_hash_table);
	SYMBOL_EXPORT_SC_(ClPkg,maphash);
	Defun(maphash);
//	SYMBOL_EXPORT_SC_(ClPkg,gethash);
//	Defun(gethash);
	SYMBOL_EXPORT_SC_(ClPkg,clrhash);
	Defun(clrhash);
	SYMBOL_SC_(CorePkg,hash_eql);
	Defun(hash_eql);
	SYMBOL_SC_(CorePkg,hash_equal);
	Defun(hash_equal);
	SYMBOL_SC_(CorePkg,hash_equalp);
	Defun(hash_equalp);
	SYMBOL_EXPORT_SC_(ClPkg,remhash);
	Defun(remhash);
        SYMBOL_EXPORT_SC_(ClPkg,gethash);
        ClDefun(gethash);

        Defun(hashTableEntryDeletedP);

    }

    void HashTable_O::exposePython(::core::Lisp_sp lisp)
    {
#ifdef USEBOOSTPYTHON
	PYTHON_CLASS(Pkg(),HashTable,"","",_LISP)
//	.initArgs("(self)")
	    .def("hash-table-count",&HashTable_O::hashTableCount)
	    .def("hash-table-size",&HashTable_O::hashTableSize)
	    .def("hash-table-rehash-size",&HashTable_O::hashTableRehashSize)
	    .def("hash-table-rehash-threshold",&HashTable_O::hashTableRehashThreshold)
	    .def("hash-table-test",&HashTable_O::hashTableTest)
//	    .def("gethash",&HashTable_O::gethash)
	    .def("hash-table-dump",&HashTable_O::hash_table_dump)
//	    .def_raw("maphash",&HashTable_O::maphash)
	;
#endif
    }

    

}; /* core */
