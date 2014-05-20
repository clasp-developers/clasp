#define	DEBUG_LEVEL_FULL

#include "core/common.h"
#include "core/environment.h"
#include "symbolTable.h"
#include "hashTableEqual.h"
#include "core/wrappers.h"
namespace core
{

// ----------------------------------------------------------------------
//

    EXPOSE_CLASS(core,HashTableEqual_O);

    void HashTableEqual_O::exposeCando(::core::Lisp_sp lisp)
    {
	::core::class_<HashTableEqual_O>()
//	.initArgs("(self)")
	;
    }

    void HashTableEqual_O::exposePython(::core::Lisp_sp lisp)
    {
#ifdef USEBOOSTPYTHON
	PYTHON_CLASS(Pkg(),HashTableEqual,"","",_LISP)
//	.initArgs("(self)")
	;
#endif
    }


    HashTableEqual_sp HashTableEqual_O::create(uint sz,  Number_sp rehashSize, double rehashThreshold)
    {_G();
        GC_RESERVE(HashTableEqual_O,hashTable);
	hashTable->setup(sz,rehashSize,rehashThreshold);
	return hashTable;
    }


    SYMBOL_EXPORT_SC_(ClPkg,equal);
    HashTableEqual_sp HashTableEqual_O::create_default()
    {_G();
	DoubleFloat_sp rhs = DoubleFloat_O::create(2.0);
	HashTableEqual_sp ht = HashTableEqual_O::create(16,rhs,1.0);
	return ht;
    }


#if 0
    void HashTableEqual_O::serialize(::serialize::SNodeP node)
    {
        this->Bases::serialize(node);
	// Archive other instance variables here
    }
#endif

#if defined(XML_ARCHIVE)
    void HashTableEqual_O::archiveBase(::core::ArchiveP node)
    {
        this->Base::archiveBase(node);
	// Archive other instance variables here
    }
#endif // defined(XML_ARCHIVE)


    void HashTableEqual_O::initialize()
    {_OF();
        this->Base::initialize();
    }


    bool HashTableEqual_O::keyTest(T_sp entryKey, T_sp searchKey) const
    {_OF();
	return af_equal(entryKey,searchKey);
    }

    int HashTableEqual_O::sxhashKey(T_sp obj,int bound, bool willAddKey ) const
    {_OF();
	HashGenerator hg;
#ifdef USE_MPS
        HashTable_O::sxhash_equal(hg,obj, willAddKey ? const_cast<mps_ld_t>(&(this->_LocationDependencyTracker)) : NULL );
#else
	HashTable_O::sxhash_equal(hg,obj,NULL);
#endif
	return hg.hash(bound);
    }
    

}; /* core */
