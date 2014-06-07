
#include "foundation.h"
#include "object.h"
#include "lisp.h"
#include "vectorObjectsWithFillPtr.h"
#include "newhash.h"
#include "cache.h"

namespace core
{


    Cache::Cache(int keySize, int cacheSize)
    {_G();
	this->_keys = VectorObjectsWithFillPtr_O::make(_Nil<T_O>(), _Nil<Cons_O>(), keySize, 0,false);
	CacheRecord empty(_Nil<T_O>(),_Nil<T_O>(),0);
	this->_table.resize(cacheSize,empty);
    }

    void Cache::removeOne(T_sp firstKey)
    {
	// For multithreading ecl_cache_remove_one does an ecl_atomic_push
	this->clearOneFromCache(firstKey);
    }


    void Cache::clearOneFromCache(T_sp target)
    {
	for ( int i(0); i< this->_table.size(); ++i)
	{
	    T_sp key = this->_table[i]._key;
	    if ( key.notnilp() )
	    {
		if ( target == key.as<VectorObjects_O>()->operator[](0) )
		{
		    this->_table[i]._key = _Nil<T_O>();
		    this->_table[i]._generation = 0;
		}
	    }
	}
    }


    void Cache::empty()
    {
	this->_generation = 0;
	for ( int i(0); i<this->_table.size(); ++i)
	{
	    CacheRecord& self = this->_table[i];
	    self._key = _Nil<T_O>();	 // ecl [i]
	    self._value = _Nil<T_O>(); // ecl [i+1]
	    self._generation = 0; // ecl [i+2]
	}
    }


    cl_intptr_t Cache::vector_hash_key(T_sp tkeys)
    {
	VectorObjectsWithFillPtr_sp keys = tkeys.as<VectorObjectsWithFillPtr_O>();
#if DEBUG_CLOS>=2
	printf("MLOG vector_hash_key keys->vector.fillp = %d %s:%d\n", keys->fillPointer(),  __FILE__, __LINE__ );
	if ( keys->fillPointer() > 0 )
	{
	    for ( int uu=0; uu<keys->fillPointer(); ++uu )
	    {
		printf( "MLOG    vector_hash_key   key[%d] --> %lX\n", uu, (keys->operator[](uu).intptr()));
	    }
	}
#endif

	cl_intptr_t c, n, a = GOLDEN_RATIO, b = GOLDEN_RATIO;
	for ( c=0, n=keys->fillPointer(); n >= 3; )
	{
	    c += (cl_intptr_t)(keys->operator[](--n).get());
	    b += (cl_intptr_t)(keys->operator[](--n).get());
	    a += (cl_intptr_t)(keys->operator[](--n).get());
	    mix(a,b,c);
	}
	switch (n) {
	case 2: b += (cl_intptr_t)(keys->operator[](--n).get());
	case 1: a += (cl_intptr_t)(keys->operator[](--n).get());
	    c += keys->dimension();
	    mix(a,b,c);
	}
	return c;
    }



    void Cache::search_cache(CacheRecord*& min_e)
    {
        gctools::Vec0<CacheRecord>& table = this->_table;
	VectorObjectsWithFillPtr_sp& keys = this->_keys;
	int argno = keys->fillPointer();
	cl_intptr_t hi = this->vector_hash_key(keys);
	int total_size = table.size();
	int min_gen, gen;
//        gctools::StackRootedPointer<CacheRecord> min_e;	//    cl_object *min_e;
	int k;
	int idx = hi % total_size;
	ASSERTF(idx>=0,BF("idx must be positive"));
#if 0
	if ( idx==0 ) {
	    stringstream ss;
	    VectorObjectsWithFillPtr_sp vkeys = keys.as<VectorObjectsWithFillPtr_O>();
	    ss << "Number of keys: " << vkeys->length() << std::endl;
	    ss << "key values: " << std::endl;
	    for ( int uu=0; uu<keys->length(); uu++ ) {
		ss << (BF("MLOG    vector_hash_key   key[%d] --> %ld") % uu % (keys->operator[](uu).intptr())).str() << std::endl;
	    }
	    string serr = ss.str();
	    cl_intptr_t hitest = this->vector_hash_key(keys);
	    SIMPLE_ERROR(BF("idx should never be == 0 but it was???  Check vector_hash_keys on keys to see if hi==0 isn't a valid hash index\n%s") % ss.str());
	}
#endif
#if DEBUG_CLOS>=2
	printf("MLOG search_cache hash=%ld   hash-index=%d\n", hi, i);
#endif
//	i = i - (i % 3);
	min_gen = this->_generation;
	for (k = 20; k--; ) {
	    CacheRecord& e = table.operator[](idx); //cl_object *e = table->vector.self.t + i;
	    T_sp& hkey = e._key; // cl_object hkey = RECORD_KEY(e);
	    if (hkey.nilp()) { // if (hkey == OBJNULL) {
		min_gen = -1; // min_gen = -1;
		min_e = &e; // min_e.set(&e); // min_e = e;
		if ( e._value.nilp() ) { // if (RECORD_VALUE(e) == OBJNULL) {
		    /* This record is not only deleted but empty
		     * Hence we cannot find our method ahead */
		    break;
		}
		/* Else we only know that the record has been
		 * deleted, but we might find our data ahead. */
	    } else if ( argno == af_length(hkey.as<VectorObjects_O>()) ) { // if (argno == hkey->vector.fillp) {
		int n; // cl_index n;
		for (n = 0; n < argno; n++) {
		    if ( keys->operator[](n) != hkey.as<VectorObjects_O>()->operator[](n))
// if (keys->vector.self.t[n] != hkey->vector.self.t[n])
			goto NO_MATCH;
		}
		min_e = &e; // min_e.set(&e);
		goto FOUND;
	    } else if (min_gen >= 0) {
	    NO_MATCH:
		/* Unless we have found a deleted record, keep
		 * looking for the oldest record that we can
		 * overwrite with the new data. */
		gen = e._generation; // gen = RECORD_GEN(e);
		if ( gen < min_gen ) {// if (gen < min_gen) {
		    min_gen = gen;
		    min_e = &e; // min_e.set(&e);
		}
	    }
	    idx++; //i += 3;
	    if (idx >= total_size) idx = 0;
	}
	if (!min_e) { // (min_e.nullP()) {
            throw CacheError();
	    SIMPLE_ERROR(BF("An error occured while searching the generic function method hash table - min_e == NULL - I should put this in a try/catch block"));
	}
	min_e->_key = _Nil<T_O>(); // RECORD_KEY(min_e) = OBJNULL;
	this->_generation++; // cache->generation++;
    FOUND:
	/*
	 * Once we have reached here, we set the new generation of
	 * this record and perform a global shift so that the total
	 * generation number does not become too large and we can
	 * expire some elements.
	 */
	gen = this->_generation; // gen = cache->generation;
	min_e->_generation = gen; // RECORD_GEN_SET(min_e, gen);
	if (gen >= total_size/2) {
            CacheRecord* e = &table.operator[](0);
//            gctools::StackRootedPointer<CacheRecord> e(&(table.operator[](0))); // cl_object *e = table->vector.self.t;
	    gen = 0.5*gen;
	    this->_generation = gen; // cache->generation -= gen;
	    for ( int i=table.size(); --i; ++e ) { // for (i = table->vector.dim; i; i-= 3, e += 3) {
		int g = e->_generation - gen; // cl_fixnum g = RECORD_GEN(e) - gen;
		if (g <= 0) {
		    e->_key = _Nil<T_O>(); // RECORD_KEY(e) = OBJNULL;
		    e->_value = _Nil<T_O>(); // RECORD_VALUE(e) = ECL_NIL;
		    g = 0;
		}
		e->_generation = g; // RECORD_GEN_SET(e, g);
	    }
	}
    }



#if 0
    GC_RESULT Cache::scanGCRoots(GC_SCAN_ARGS_PROTOTYPE)
    {
	GC_SCANNER_BEGIN() {
	    SMART_PTR_FIX(this->_keys);
	    for ( auto it= this->_table.begin(); it<this->_table.end(); ++it )
	    {
		SMART_PTR_FIX(it->_key);
		SMART_PTR_FIX(it->_value);
	    }
	} GC_SCANNER_END();
	return GC_RES_OK;
    }
#endif
    
    
    


    void initialize_cache()
    {_G();
    }





};
