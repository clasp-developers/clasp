/*
    File: cache.cc
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
#include <clasp/core/object.h>
#include <clasp/core/lisp.h>
#include <clasp/core/vectorObjectsWithFillPtr.h>
#include <clasp/core/newhash.h>
#include <clasp/core/cache.h>
#include <clasp/core/wrappers.h>

namespace core
{


  void Cache::setup(int keySize, int cacheSize)
  {
    this->_keys.resize(keySize,_Nil<T_O>());
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


  cl_intptr_t Cache::vector_hash_key(gctools::Vec0<T_sp>& keys)
  {
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
    for ( c=0, n=keys.size(); n >= 3; )
      {
	c += (cl_intptr_t)(keys[--n].get());
	b += (cl_intptr_t)(keys[--n].get());
	a += (cl_intptr_t)(keys[--n].get());
	mix(a,b,c);
      }
    switch (n) {
    case 2: b += (cl_intptr_t)(keys[--n].get());
    case 1: a += (cl_intptr_t)(keys[--n].get());
      c += keys.size();
      mix(a,b,c);
    }
    return c;
  }


    /*! TODO: I don't think this cache is location aware - it may need to be tuned to handle a moving garbage collector */
  void Cache::search_cache(CacheRecord*& min_e)
  {
    gctools::Vec0<CacheRecord>& table = this->_table;
    gctools::Vec0<T_sp>& keys = this->_keys;
    int argno = keys.size();
    cl_intptr_t hi = this->vector_hash_key(keys);
    int total_size = table.size();
    int min_gen, gen;
    //        gctools::StackRootedPointer<CacheRecord> min_e;	//    cl_object *min_e;
    int k;
    int idx = hi % total_size;
    ASSERTF(idx>=0,BF("idx must be positive"));
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
      } else if ( argno == cl_length(hkey.as<VectorObjects_O>()) ) { // if (argno == hkey->vector.fillp) {
	int n; // cl_index n;
	for (n = 0; n < argno; n++) {
	  if ( keys[n] != hkey.as<VectorObjects_O>()->operator[](n))
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
	int g = e->_generation - gen; // gctools::Fixnum g = RECORD_GEN(e) - gen;
	if (g <= 0) {
	  e->_key = _Nil<T_O>(); // RECORD_KEY(e) = OBJNULL;
	  e->_value = _Nil<T_O>(); // RECORD_VALUE(e) = ECL_NIL;
	  g = 0;
	}
	e->_generation = g; // RECORD_GEN_SET(e, g);
      }
    }
  }




    
    
#define ARGS_core_clearGenericFunctionDispatchCache "()"
#define DECL_core_clearGenericFunctionDispatchCache ""
#define DOCS_core_clearGenericFunctionDispatchCache "clearGenericFunctionDispatchCache"
    void core_clearGenericFunctionDispatchCache()
    {_G();
        printf("%s:%d Clearing generic function dispatch cache\n",__FILE__,__LINE__);
        _lisp->methodCachePtr()->empty();
    };



  void initialize_cache()
  {_G();
      CoreDefun(clearGenericFunctionDispatchCache);
  }





};
