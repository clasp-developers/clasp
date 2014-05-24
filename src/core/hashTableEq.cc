#define	DEBUG_LEVEL_FULL

#include "core/common.h"
#include "core/environment.h"
#include "hashTableEq.h"
#include "core/wrappers.h"
namespace core
{

// ----------------------------------------------------------------------
//

    EXPOSE_CLASS(core,HashTableEq_O);

    void HashTableEq_O::exposeCando(::core::Lisp_sp lisp)
    {
	::core::class_<HashTableEq_O>()
//	.initArgs("(self)")
	;
    }

    void HashTableEq_O::exposePython(::core::Lisp_sp lisp)
    {
#ifdef USEBOOSTPYTHON
	PYTHON_CLASS(Pkg(),HashTableEq,"","",_LISP)
//	.initArgs("(self)")
	;
#endif
    }


    HashTableEq_sp HashTableEq_O::create(uint sz,  Number_sp rehashSize, double rehashThreshold)
    {_G();
        GC_ALLOCATE(HashTableEq_O,hashTable );
	hashTable->setup(sz,rehashSize,rehashThreshold);
	return hashTable;
    }


    HashTableEq_sp HashTableEq_O::create_default()
    {
        DoubleFloat_sp rhs = DoubleFloat_O::create(2.0);
        return HashTableEq_O::create(8,rhs,1.0);
    }




#if 0
    void HashTableEq_O::serialize(::serialize::SNodeP node)
    {
        this->Bases::serialize(node);
	// Archive other instance variables here
    }
#endif

#if defined(XML_ARCHIVE)
    void HashTableEq_O::archiveBase(::core::ArchiveP node)
    {
        this->Base::archiveBase(node);
	// Archive other instance variables here
    }
#endif // defined(XML_ARCHIVE)



    bool HashTableEq_O::keyTest(T_sp entryKey, T_sp searchKey) const
    {_OF();
	return af_eq(entryKey,searchKey);
    }

    int HashTableEq_O::sxhashKey(T_sp obj,int bound,bool willAddKey) const
    {_OF();
#ifdef USE_MPS
        if ( willAddKey ) {
            void* blockAddr = GC_BASE_ADDRESS_FROM_SMART_PTR(obj);
            mps_ld_add(const_cast<mps_ld_t>(&(this->_LocationDependencyTracker))
                       ,gctools::_global_arena,blockAddr);
        }
#endif
	HashGenerator hg;
#ifdef USE_MPS
        HashTable_O::sxhash_eq(hg,obj, willAddKey ? const_cast<mps_ld_t>(&(this->_LocationDependencyTracker)) : NULL );
#else
        HashTable_O::sxhash_eq(hg,obj,NULL);
#endif
	return hg.hash(bound);
    }
    

}; /* core */
