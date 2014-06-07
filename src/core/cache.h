#ifndef _core_cache_H
#define _core_cache_H

#include "foundation.h"
#include "object.h"
#include "vectorObjectsWithFillPtr.fwd.h"

namespace core
{

    struct CacheError
    {
    };

    struct CacheRecord
    {
	T_sp 	_key;
	T_sp 	_value;
	int	_generation;
	CacheRecord(T_sp k, T_sp v, int g) : _key(k), _value(v), _generation(g) {};

    };


    class Cache
    {
	VectorObjectsWithFillPtr_sp 	_keys;
        gctools::Vec0<CacheRecord>	_table;
	int				_generation;

    private:
	void empty();
	void clearOneFromCache(T_sp target);
    public:
    
	/*! Constructor - like ecl_make_cache */
	explicit Cache(int keySize, int cacheSize);

#if 0
        GC_RESULT onHeapScanGCRoots(GC_SCAN_ARGS_PROTOTYPE)
        {
            GC_SCANNER_BEGIN() {
                SMART_PTR_FIX(this->_keys);
                for ( auto& itr_gc_safe : this->_table ) {
                    SMART_PTR_FIX(itr_gc_safe._key);
                    SMART_PTR_FIX(itr_gc_safe._value);
                }
            } GC_SCANNER_END();
            return GC_RES_OK;
        }
#endif
	/*! Search cache - like ecl_search_cache
	  It takes no arguments - what is it searching????*/
	void search_cache(CacheRecord*& e);

	/*! Like ecl_cache_remove_one */
	void removeOne(T_sp firstKey);


	cl_intptr_t vector_hash_key(T_sp keys);


	VectorObjectsWithFillPtr_sp& keys() { return this->_keys;};
	const VectorObjectsWithFillPtr_sp& keys() const { return this->_keys;};


    };
};
#endif // _core_cache_H
