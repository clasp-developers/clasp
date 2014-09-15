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

    class CacheRecord
    {
    public:
        FRIEND_GC_SCANNER();
	T_sp 	_key;
	T_sp 	_value;
	int	_generation;
	CacheRecord(T_sp k, T_sp v, int g) : _key(k), _value(v), _generation(g) {};

    };


    class Cache
    {
        FRIEND_GC_SCANNER();
	gctools::Vec0<T_sp> 	        _keys;
        gctools::Vec0<CacheRecord>	_table;
	int				_generation;

    private:
	void clearOneFromCache(T_sp target);
    public:
	void empty();
    
	/*! Constructor - like ecl_make_cache */
	explicit Cache() {};
	void setup(int keySize,int cacheSize);

	/*! Search cache - like ecl_search_cache
	  It takes no arguments - what is it searching????*/
	void search_cache(CacheRecord*& e);

	/*! Like ecl_cache_remove_one */
	void removeOne(T_sp firstKey);


	cl_intptr_t vector_hash_key(gctools::Vec0<T_sp>& keys);


	gctools::Vec0<T_sp>& keys() { return this->_keys;};
	const gctools::Vec0<T_sp>& keys() const { return this->_keys;};




    };


    void initialize_cache();
};
#endif // _core_cache_H
