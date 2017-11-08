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
#include <clasp/core/array.h>
#include <clasp/core/newhash.h>
#include <clasp/core/mpPackage.h>
#include <clasp/core/cache.h>
#include <clasp/core/wrappers.h>

namespace core {

void Cache_O::setup(int keySize, int cacheSize) {
  this->_keys.resize(keySize, _Nil<T_O>());
  CacheRecord empty(_Nil<T_O>(), _Nil<T_O>(), 0);
  this->_table.resize(cacheSize, empty);
}

void Cache_O::removeOne(T_sp firstKey) {
  // For multithreading ecl_cache_remove_one does an ecl__atomic_push
#ifdef DEBUG_CACHE
  if (this->_debug)
    printf("%s:%d removeOne  -> %s\n", __FILE__, __LINE__, _rep_(firstKey).c_str());
#endif
#ifdef CLASP_THREADS
  mp::atomic_push(this->_clear_list_spinlock,this->_clear_list_safe,firstKey);
#else
  this->clearOneFromCache(firstKey);
#endif
}

void Cache_O::clearOneFromCache(T_sp target) {
  for (int i(0); i < this->_table.size(); ++i) {
    if (gc::IsA<SimpleVector_sp>(this->_table[i]._key)) {
      SimpleVector_sp key = gc::As_unsafe<SimpleVector_sp>(this->_table[i]._key);
      if (target == (*key)[0]) {
        this->_table[i]._key = _Nil<T_O>();
        this->_table[i]._generation = 0;
      }
    }
  }
}

#ifdef CLASP_THREADS
void Cache_O::clearListFromCache()
{
  T_sp tlist = mp::atomic_get_and_set_to_Nil(this->_clear_list_spinlock,this->_clear_list_safe);
#ifdef DEBUG_CACHE
  if (this->_debug) printf("%s:%d   clearListFromCache generic functions: %s\n", __FILE__, __LINE__, _rep_(tlist).c_str());
#endif
  if (tlist.consp()) {
    Cons_O* list = tlist.unsafe_cons();
    gctools::Vec0<CacheRecord>& table = this->_table;
    for (int i(0); i < this->_table.size(); ++i) {
      if (gc::IsA<SimpleVector_sp>(this->_table[i]._key)) {
        SimpleVector_sp key = gc::As_unsafe<SimpleVector_sp>(this->_table[i]._key);
        T_sp member = list->memberEq((*key)[0]);
        if (member.notnilp()) {
#ifdef DEBUG_CACHE
  if (this->_debug) printf("%s:%d:%s    Clearing cache[%d] key: %s\n", __FILE__, __LINE__, __FUNCTION__, i, _rep_((*key)[0]).c_str());
#endif
          this->_table[i]._key = _Nil<T_O>();
          this->_table[i]._generation = 0;
        }
      }
    }
  }
}
#endif

void Cache_O::empty() {
  this->_generation = 0;
  for (int i(0); i < this->_table.size(); ++i) {
    CacheRecord &self = this->_table[i];
    self._key = _Nil<T_O>();   // ecl [i]
    self._value = _Nil<T_O>(); // ecl [i+1]
    self._generation = 0;      // ecl [i+2]
  }
}

cl_intptr_t Cache_O::vector_hash_key(gctools::Vec0<T_sp> &keys) {
#if DEBUG_CLOS >= 2
  printf("MLOG vector_hash_key keys->vector.fillp = %d %s:%d\n", keys->fillPointer(), __FILE__, __LINE__);
  if (keys->fillPointer() > 0) {
    for (int uu = 0; uu < keys->fillPointer(); ++uu) {
      printf("MLOG    vector_hash_key   key[%d] --> %lX\n", uu, (keys->operator[](uu).intptr()));
    }
  }
#endif

  cl_intptr_t c, n, a = GOLDEN_RATIO, b = GOLDEN_RATIO;
  for (c = 0, n = keys.size(); n >= 3;) {
    c += (cl_intptr_t)(keys[--n].get());
    b += (cl_intptr_t)(keys[--n].get());
    a += (cl_intptr_t)(keys[--n].get());
    hash_mix(a, b, c);
  }
  switch (n) {
  case 2:
    b += (cl_intptr_t)(keys[--n].get());
  case 1:
    a += (cl_intptr_t)(keys[--n].get());
    c += keys.size();
    hash_mix(a, b, c);
  }
  return c;
}


/*! TODO: I don't think this cache is location aware - it may need to be tuned to handle a moving garbage collector */
void Cache_O::search_cache(CacheRecord *&min_e) {
#ifdef CLASP_THREADS
  if (this->_clear_list_safe.notnilp()) {
    this->clearListFromCache();
  }
#endif
  ++this->_searches;
  gctools::Vec0<CacheRecord> &table = this->_table;
  gctools::Vec0<T_sp> &keys = this->_keys;
  int argno = keys.size();
#ifdef DEBUG_CACHE
  if (this->_debug) {
    printf("%s:%d ===================  search_cache argno=%d\n", __FILE__, __LINE__, argno);
    for (size_t zi=0; zi<keys.size(); ++zi ) {
      printf("%s:%d    key[%" PRsize_t "] -> %s\n", __FILE__, __LINE__, zi, _rep_(keys[zi]).c_str());
    }
  }
#endif
  cl_intptr_t hi = this->vector_hash_key(keys);
  int total_size = table.size();
  int min_gen, gen;
  //        gctools::StackRootedPointer<CacheRecord> min_e;	//    cl_object *min_e;
  int k;
  int idx = hi % total_size;
  ASSERTF(idx >= 0, BF("idx must be positive"));
#ifdef DEBUG_CACHE
  if (this->_debug) {
    printf("%s:%d search_cache hash=%llu   hash-index=%d\n", __FILE__, __LINE__, hi, idx);
  }
#endif
  //	i = i - (i % 3);
  min_gen = this->_generation;
  for (k = 20; k--; ++this->_total_depth) {
    CacheRecord &e = table.operator[](idx); //cl_object *e = table->vector.self.t + i;
    T_sp &hkey = e._key;                    // cl_object hkey = RECORD_KEY(e);
#ifdef DEBUG_CACHE
    if (this->_debug) {
      printf("%s:%d Hash trial countdown %d idx=%d\n", __FILE__, __LINE__, k, idx);
      printf("%s:%d    hash key = %s\n", __FILE__, __LINE__, _rep_(hkey).c_str());
    }
#endif
    if (hkey.nilp()) {                      // if (hkey == OBJNULL) {
      min_gen = -1;                         // min_gen = -1;
      min_e = &e;                           // min_e.set(&e); // min_e = e;
      if (e._value.nilp()) {                // if (RECORD_VALUE(e) == OBJNULL) {
        /* This record is not only deleted but empty
	   * Hence we cannot find our method ahead */
        break;
      }
      /* Else we only know that the record has been
	 * deleted, but we might find our data ahead. */
    } else if (argno == (reinterpret_cast<SimpleVector_O*>(&*hkey))->length()) {
#ifdef DEBUG_CACHE
      if (this->_debug) {
        printf("%s:%d    argno length matches\n",__FILE__,__LINE__);
      }
#endif
      int n;                                                         // cl_index n;
      for (n = 0; n < argno; n++) {
        if (keys[n] != (*reinterpret_cast<SimpleVector_O*>(&*hkey))[n]) {
#ifdef DEBUG_CACHE
          if (this->_debug) {
            printf("%s:%d    mismatch in arg %d  keys[n]=%p    hkey[n]=%p\n", __FILE__, __LINE__, n, (void*)keys[n].raw_(), (void*)((*reinterpret_cast<SimpleVector_O*>(&*hkey))[n]).raw_());
          }
#endif

          // if (keys->vector.self.t[n] != hkey->vector.self.t[n])
          goto NO_MATCH;
        }
      }
      min_e = &e; // min_e.set(&e);
      goto FOUND;
    } else if (min_gen >= 0) {
    NO_MATCH:
      /* Unless we have found a deleted record, keep
	 * looking for the oldest record that we can
	 * overwrite with the new data. */
      gen = e._generation; // gen = RECORD_GEN(e);
      if (gen < min_gen) { // if (gen < min_gen) {
        min_gen = gen;
        min_e = &e; // min_e.set(&e);
      }
    }
    idx++; //i += 3;
    if (idx >= total_size)
      idx = 0;
  }
  if (!min_e) { // (min_e.nullP()) {
    throw CacheError();
    SIMPLE_ERROR(BF("An error occured while searching the generic function method hash table - min_e == NULL - I should put this in a try/catch block"));
  }
  ++this->_misses;
  min_e->_key = _Nil<T_O>(); // RECORD_KEY(min_e) = OBJNULL;
  this->_generation++;       // cache->generation++;
FOUND:
#ifdef DEBUG_CACHE
  if (this->_debug)
    printf("%s:%d Found!  min_e->_key = %s\n", __FILE__, __LINE__, _rep_(min_e->_key).c_str());
#endif
  /*
     * Once we have reached here, we set the new generation of
     * this record and perform a global shift so that the total
     * generation number does not become too large and we can
     * expire some elements.
     */
  gen = this->_generation;  // gen = cache->generation;
  min_e->_generation = gen; // RECORD_GEN_SET(min_e, gen);
  if (gen >= total_size / 2) {
    CacheRecord *e = &table.operator[](0);
    //            gctools::StackRootedPointer<CacheRecord> e(&(table.operator[](0))); // cl_object *e = table->vector.self.t;
    gen = 0.5 * gen;
    this->_generation = gen;               // cache->generation -= gen;
    for (int i = table.size(); --i; ++e) { // for (i = table->vector.dim; i; i-= 3, e += 3) {
      int g = e->_generation - gen;        // gctools::Fixnum g = RECORD_GEN(e) - gen;
      if (g <= 0) {
        e->_key = _Nil<T_O>();   // RECORD_KEY(e) = OBJNULL;
        e->_value = _Nil<T_O>(); // RECORD_VALUE(e) = ECL_NIL;
        g = 0;
      }
      e->_generation = g; // RECORD_GEN_SET(e, g);
    }
  }
#ifdef DEBUG_CACHE
  if (this->_debug) fflush(stdout);
#endif

  return;
}

CL_LAMBDA(pow);
CL_DECLARE();
CL_DOCSTRING("cache_resize - Resize the cache to 2^pow");
CL_DEFUN void core__single_dispatch_method_cache_resize(Fixnum pow) {
  if (pow < 2 || pow > 64) {
    SIMPLE_ERROR(BF("Cache power must be in the range of 2...64"));
  }
  size_t size = 1 << pow;
  return my_thread->_SingleDispatchMethodCachePtr->setup(2, size);
}

CL_LAMBDA();
CL_DECLARE();
CL_DOCSTRING("cache_status - (values searches misses total-depth)");
CL_DEFUN T_mv core__single_dispatch_method_cache_status() {
  return Values(clasp_make_fixnum(my_thread->_SingleDispatchMethodCachePtr->_searches),
                clasp_make_fixnum(my_thread->_SingleDispatchMethodCachePtr->_misses),
                clasp_make_fixnum(my_thread->_SingleDispatchMethodCachePtr->_total_depth));
}

#ifdef DEBUG_CACHE
CL_DOCSTRING("Turn debugging on and off for cache");
CL_DEFUN void core__debug_single_dispatch_method_cache(bool debug)
{
  my_thread->_SingleDispatchMethodCachePtr->_debug = debug;
}
#endif


   
                                                                            
void initialize_cache() {
}
};
