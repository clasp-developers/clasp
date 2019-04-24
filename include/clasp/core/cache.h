/*
    File: cache.h
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
#ifndef _core_cache_H
#define _core_cache_H

#include <clasp/core/object.h>

namespace core {

  struct CacheError {
  };

  class CacheRecord {
  public:
    FRIEND_GC_SCANNER(core::CacheRecord);
    T_sp _key;
    T_sp _value;
    int _generation;
  CacheRecord(T_sp k, T_sp v, int g) : _key(k), _value(v), _generation(g){};
  };

  FORWARD(Cache);
  class Cache_O : public General_O {
    FRIEND_GC_SCANNER(core::CacheRecord);
    LISP_CLASS(core, CorePkg, Cache_O, "Cache", General_O );
  public:
    size_t _searches;
    size_t _misses;
    size_t _total_depth;
    gctools::Vec0<T_sp> _keys;
    gctools::Vec0<CacheRecord> _table;
    int _generation;
#ifdef CLASP_THREADS
    mp::SpinLock      _clear_list_spinlock;
    T_sp              _clear_list_safe;
#endif
#ifdef DEBUG_CACHE
    bool _debug;
#endif
  Cache_O() : _misses(0), _searches(0), _total_depth(0), _generation(0)
#ifdef CLASP_THREADS
      , _clear_list_safe(_Nil<T_O>())
#endif
#ifdef DEBUG_CACHE
      , _debug(0)
#endif
    {};
  private:
    void clearOneFromCache(T_sp target);
#ifdef CLASP_THREADS
    void clearListFromCache();
#endif
  public:
    void empty();
  /*! Constructor - like ecl_make_cache */
    void setup(int keySize, int cacheSize);

  /*! Search cache - like ecl_search_cache
	  It takes no arguments - what is it searching????*/
    void search_cache(CacheRecord *&e);

  /*! Like ecl_cache_remove_one */
    void removeOne(T_sp firstKey);

    uintptr_t vector_hash_key(gctools::Vec0<T_sp> &keys);

    gctools::Vec0<T_sp> &keys() { return this->_keys; };
    const gctools::Vec0<T_sp> &keys() const { return this->_keys; };
  };
  void initialize_cache();
};
#endif // _core_cache_H
