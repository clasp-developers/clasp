#define	DEBUG_LEVEL_FULL

#include "core/common.h"
#include "core/environment.h"
#include "symbolTable.h"
#include "hashTableEql.h"
#include "core/wrappers.h"
namespace core
{

// ----------------------------------------------------------------------
//

    EXPOSE_CLASS(core,HashTableEql_O);

    void HashTableEql_O::exposeCando(::core::Lisp_sp lisp)
    {
	::core::class_<HashTableEql_O>()
//	.initArgs("(self)")
	;
    }

    void HashTableEql_O::exposePython(::core::Lisp_sp lisp)
    {
#ifdef USEBOOSTPYTHON
	PYTHON_CLASS(Pkg(),HashTableEql,"","",_LISP)
//	.initArgs("(self)")
	;
#endif
    }


    HashTableEql_sp HashTableEql_O::create(uint sz,  Number_sp rehashSize, double rehashThreshold)
    {_G();
	if ( sz == 0 ) sz = 16;
        GC_ALLOCATE(HashTableEql_O,hashTable );
	hashTable->setup(sz,rehashSize,rehashThreshold);
	return hashTable;
    }

    SYMBOL_EXPORT_SC_(ClPkg,eql);
    HashTableEql_sp HashTableEql_O::create_default()
    {_G();
	DoubleFloat_sp rhs = DoubleFloat_O::create(2.0);
	HashTableEql_sp ht = HashTableEql_O::create(16,rhs, 1.0);
	return ht;
    }

						    



#if 0
    void HashTableEql_O::serialize(::serialize::SNodeP node)
    {
        this->Bases::serialize(node);
	// Archive other instance variables here
    }
#endif

#if defined(XML_ARCHIVE)
    void HashTableEql_O::archiveBase(::core::ArchiveP node)
    {
        this->Base::archiveBase(node);
	// Archive other instance variables here
    }
#endif // defined(XML_ARCHIVE)



    bool HashTableEql_O::keyTest(T_sp entryKey, T_sp searchKey) const
    {_OF();
	return af_eql(entryKey,searchKey);
    }

    int HashTableEql_O::sxhashKey(T_sp obj ,int bound, bool willAddKey) const
    {_OF();
	HashGenerator hg;
#ifdef USE_MPS
        HashTable_O::sxhash_eql(hg,obj, willAddKey ? const_cast<mps_ld_t>(&(this->_LocationDependencyTracker)) : NULL );
#else
	HashTable_O::sxhash_eql(hg,obj,NULL);
#endif
	int hash = hg.hash(bound);
	LOG(BF("HashTableEql_O::sxhashKey obj[%s] raw_hash[%s] bound[%d] hash[%d]")
	    % _rep_(obj) % hg.asString() % bound % hash );
	return hash;
    }
    

}; /* core */
