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

        GC_RESULT onHeapScanGCRoots(GC_SCAN_ARGS_PROTOTYPE) {
            IMPLEMENT_MEF(BF("Don't implement anthing here - the Cache should scan each CacheRecord"));
        };

    };


    class Cache
    {
	VectorObjectsWithFillPtr_sp 	_keys;
	vector<CacheRecord>		_table;
	int				_generation;

    private:
	void empty();
	void clearOneFromCache(const T_sp& target);
    public:
    
	/*! Constructor - like ecl_make_cache */
	explicit Cache(int keySize, int cacheSize);

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

	/*! Search cache - like ecl_search_cache
	  It takes no arguments - what is it searching????*/
	void search_cache(gctools::StackRootedPointer<CacheRecord>& e);

	/*! Like ecl_cache_remove_one */
	void removeOne(T_sp firstKey);


	cl_intptr_t vector_hash_key(T_sp keys);


	VectorObjectsWithFillPtr_sp& keys() { return this->_keys;};
	const VectorObjectsWithFillPtr_sp& keys() const { return this->_keys;};

        GC_RESULT scanGCRoots(GC_SCAN_ARGS_PROTOTYPE);

    };
};
#endif // _core_cache_H
