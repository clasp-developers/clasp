/*
    File: accessor.cc
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
//#define DEBUG_LEVEL_FULL

#include <clasp/core/foundation.h>
#include <clasp/core/object.h>
#include <clasp/core/lisp.h>
#include <clasp/core/instance.h>
#include <clasp/core/primitives.h>
#include <clasp/core/evaluator.h>
#include <clasp/core/multipleValues.h>
#include <clasp/core/str.h>
#include <clasp/core/predicates.h>
#include <clasp/core/vectorObjectsWithFillPtr.h>
#include <clasp/core/hashTable.h>
#include <clasp/core/cache.h>
#include <clasp/core/symbolTable.h>
#include <clasp/core/genericFunction.h>
#include <clasp/core/wrappers.h>

//#define DEBUG_ACCESSORS 1
#define DEBUG_ACCESSORS_ON() (core::_sym_STARdebug_accessorsSTAR->symbolValue().notnilp())


namespace core {


static void
no_applicable_method(T_sp gfun, List_sp args )
{
  eval::applyLastArgsPLUSFirst(cl::_sym_no_applicable_method,args,gfun);
}


static gctools::Vec0<T_sp>&
fill_spec_vector(T_sp gfun, gctools::Vec0<T_sp>& vektor, T_sp instance)
{
  vektor[0] = gfun;
  vektor[1] = instance;
  vektor.unsafe_set_end(2);
  return vektor;
}

static CacheRecord*
search_slot_index(T_sp gfun, T_sp instance, Cache_sp cache)
{
  gctools::Vec0<T_sp>& vector = fill_spec_vector(gfun,cache->keys(), instance);
  CacheRecord* e;
  try {
    cache->search_cache(e);
  } catch (CacheError &err) {
    printf("%s:%d - There was an CacheError searching the GF cache for the keys"
           "  You should try and get into cache->search_cache to see where the error is\n",
           __FILE__, __LINE__);
    abort();
  }
#ifdef DEBUG_ACCESSORS
  if (DEBUG_ACCESSORS_ON()) {
    printf("%s:%d search_slot_index index = %s\n", __FILE__, __LINE__, _rep_(e->_value).c_str());
  }
#endif
  return e;
}



static T_sp
slot_method_name(T_sp gfun, T_sp args)
{
  T_sp methods = eval::funcall(cl::_sym_compute_applicable_methods, gfun, args);
  unlikely_if (methods.nilp()) {
    return _Nil<core::T_O>();
  }
  T_sp first = oCar(methods);
  T_sp slotd = eval::funcall(cl::_sym_slot_value,first,clos::_sym_slot_definition);
  return eval::funcall(cl::_sym_slot_value,slotd,clos::_sym_name);
}



static T_mv
slot_method_index(T_sp gfun, T_sp instance, T_sp args)
{
  T_sp slot_name = slot_method_name(gfun, args);
  unlikely_if (slot_name.nilp())
    return _Nil<T_O>();
  else {
    T_sp table = eval::funcall(cl::_sym_slot_value,cl__class_of(instance),clos::_sym_location_table);
                /* The class might not be a standard class. This happens
                 * when a nonstandard class inherits from a standard class
                 * and does not add any new slot accessor.
                 */
    unlikely_if (table.nilp()) return slot_name;
    HashTable_sp ht = gc::As<HashTable_sp>(table);
    return ht->gethash(slot_name, _Unbound<core::T_O>());
  }
}





static CacheRecord*
add_new_index(T_sp gfun, T_sp instance, List_sp args, Cache_sp cache)
{
        /* The keys and the cache may change while we compute the
         * applicable methods. We must save the keys and recompute the
         * cache location if it was filled. */
  T_mv index = slot_method_index(gfun, instance, args);
#ifdef DEBUG_ACCESSORS
  if (DEBUG_ACCESSORS_ON()) {
    printf("%s:%d add_new_index index = %s\n", __FILE__, __LINE__, _rep_(index).c_str());
  }
#endif
  unlikely_if (index.second().nilp()) {
    no_applicable_method(gfun, args);
  }
  {
    CacheRecord* e;
    gctools::Vec0<T_sp>& vektor = fill_spec_vector(gfun,cache->keys(),instance);
    cache->search_cache(e);
    T_sp keys = VectorObjects_O::create(vektor);
    e->_key = keys;
    e->_value = index;
    return e;
  }
}




static void
ensure_up_to_date_instance(T_sp tinstance)
{
  Instance_sp instance = gc::As<Instance_sp>(tinstance);
  Class_sp clas = cl__class_of(instance);
  T_sp slots = clas->instanceRef(Class_O::REF_SLOTS);
  unlikely_if (!slots.unboundp() && instance->instanceSig() != slots ) {
    eval::funcall(clos::_sym_update_instance,instance);
  }
}




LCC_RETURN optimized_slot_reader_dispatch(Instance_sp gf, VaList_sp vargs) 
{
#ifdef DEBUG_ACCESSORS
  if (DEBUG_ACCESSORS_ON()) {
    printf("%s:%d optimized_slot_reader_dispatch gf=%s\n", __FILE__, __LINE__, _rep_(gf).c_str() );
  }
#endif
  int nargs = vargs->remaining_nargs();
  if ( nargs != 1 ) {
    SIMPLE_ERROR(BF("Wrong number of arguments for reader %s") % _rep_(gf));
  }
  CacheRecord* e;
  T_sp value;
  T_sp index;
  T_sp tinstance = vargs->next_arg();
  if (Instance_sp instance = tinstance.asOrNull<Instance_O>() ) {
    Cache_sp cache = _lisp->slotCachePtr();
    e = search_slot_index(gf,instance,cache);
    unlikely_if (e->_key.nilp()) {
      List_sp args = Cons_O::createList(instance);
      e = add_new_index(gf, instance, args, cache);
                /* no_applicable_method() was called */
      unlikely_if (e == NULL) {
        SIMPLE_ERROR(BF("What do I do here?  e==NULL"));
      }
    }
    index = e->_value;
    if (index.fixnump()) {
      value = instance->_Slots[index.unsafe_fixnum()];
#ifdef DEBUG_ACCESSORS
  if (DEBUG_ACCESSORS_ON()) {
    printf("%s:%d optimized_slot_reader_dispatch getting slot[index = %s] value = %s\n", __FILE__, __LINE__, _rep_(index).c_str(), _rep_(value).c_str() );
  }
#endif
    } else if (!index.asOrNull<Cons_O>()) {
      value = eval::funcall( clos::_sym_slot_value, instance, index);
#ifdef DEBUG_ACCESSORS
  if (DEBUG_ACCESSORS_ON()) {
    printf("%s:%d optimized_slot_reader_dispatch getting slot-value index = %s value = %s\n", __FILE__, __LINE__, _rep_(index).c_str(), _rep_(value).c_str() );
  }
#endif
    } else {
      Cons_sp cindex = gc::As<Cons_sp>(index);
      value = cindex->_Car;
#ifdef DEBUG_ACCESSORS
  if (DEBUG_ACCESSORS_ON()) {
    printf("%s:%d optimized_slot_reader_dispatch getting slot cons index = %s value = %s\n", __FILE__, __LINE__, _rep_(index).c_str(), _rep_(value).c_str() );
  }
#endif
    }
    unlikely_if (value.unboundp()) {
      Cons_sp linstance = Cons_O::createList(instance);
      T_sp slot_name = slot_method_name(gf,linstance);
      value = eval::funcall(cl::_sym_slot_unbound,
                            cl__class_of(instance),
                            instance,
                            slot_name);
    }
    return value.as_return_type();
  }
  eval::funcall(cl::_sym_no_applicable_method, gf, value, tinstance);
  UNREACHABLE();
}

LCC_RETURN optimized_slot_writer_dispatch(Instance_sp gf, VaList_sp vargs) 
{
#ifdef DEBUG_ACCESSORS
  if (DEBUG_ACCESSORS_ON()) {
    printf("%s:%d optimized_slot_writer_dispatch gf=%s\n", __FILE__, __LINE__, _rep_(gf).c_str() );
  }
#endif
  int nargs = vargs->remaining_nargs();
  if ( nargs != 2 ) {
    SIMPLE_ERROR(BF("Wrong number of arguments for generic function %s") % _rep_(gf));
  }
  CacheRecord* e;
  T_sp index;
  T_sp value = vargs->next_arg();
  T_sp tinstance = vargs->next_arg();
  if (Instance_sp instance = tinstance.asOrNull<Instance_O>() ) {
    Cache_sp cache = _lisp->slotCachePtr();
    e = search_slot_index(gf,instance,cache);
    unlikely_if (e->_key.nilp()) {
      List_sp args = Cons_O::createList(value,instance);
      e = add_new_index(gf, instance, args, cache);
                /* no_applicable_method() was called */
      unlikely_if (e == NULL) {
        SIMPLE_ERROR(BF("What do I do here?  e==NULL"));
      }
    }
    index = e->_value;
    if (index.fixnump()) {
#ifdef DEBUG_ACCESSORS
  if (DEBUG_ACCESSORS_ON()) {
    printf("%s:%d optimized_slot_writer_dispatch setting slot[index = %s]  value = %s\n", __FILE__, __LINE__, _rep_(index).c_str(), _rep_(value).c_str() );
  }
#endif
      instance->_Slots[index.unsafe_fixnum()] = value;
    } else if (!index.asOrNull<Cons_O>()) {
#ifdef DEBUG_ACCESSORS
  if (DEBUG_ACCESSORS_ON()) {
    printf("%s:%d optimized_slot_writer_dispatch setting slot-value index = %s  value = %s\n", __FILE__, __LINE__, _rep_(index).c_str(), _rep_(value).c_str() );
  }
#endif
      eval::funcall( clos::_sym_slot_value_set, value, instance, index);
    } else {
#ifdef DEBUG_ACCESSORS
  if (DEBUG_ACCESSORS_ON()) {
    printf("%s:%d optimized_slot_writer_dispatch setting slot cons index = %s value = %s\n", __FILE__, __LINE__, _rep_(index).c_str(), _rep_(value).c_str() );
  }
#endif
      Cons_sp cindex = gc::As<Cons_sp>(index);
      cindex->_Car = value;
    }
    return value.as_return_type();
  }
  eval::funcall(cl::_sym_no_applicable_method, gf, value, tinstance);
  UNREACHABLE();
}





};
