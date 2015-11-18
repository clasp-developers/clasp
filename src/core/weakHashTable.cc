/*
    File: weakHashTable.cc
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
#include <clasp/core/weakHashTable.h>
#include <clasp/core/wrappers.h>

#define WEAK_LOG(x) printf("%s:%d %s\n", __FILE__, __LINE__, (x).str().c_str())

namespace core {

EXPOSE_CLASS(core, WeakHashTable_O);

void WeakHashTable_O::exposeCando(::core::Lisp_sp lisp) {
  ::core::class_<WeakHashTable_O>();
}

void WeakHashTable_O::exposePython(Lisp_sp lisp) {
  _G();
#ifdef USEBOOSTPYTHON
  PYTHON_CLASS(CorePkg, WeakHashTable, "", "", _lisp);
#endif
}

EXPOSE_CLASS(core, WeakKeyHashTable_O);

void WeakKeyHashTable_O::describe(T_sp stream) {
  KeyBucketsType &keys = *this->_HashTable._Keys;
  ValueBucketsType &values = *this->_HashTable._Values;
  stringstream ss;
  ss << (BF("WeakKeyHashTable   size: %zu\n") % this->_HashTable.length()).str();
  ss << (BF("   keys memory range:  %p  - %p \n") % &keys[0].rawRef_() % &keys[this->_HashTable.length()].rawRef_()).str();
  ss << (BF("   _HashTable.length = %d\n") % keys.length()).str();
  ss << (BF("   _HashTable.used = %d\n") % keys.used()).str();
  ss << (BF("   _HashTable.deleted = %d\n") % keys.deleted()).str();
  for (int i(0), iEnd(this->_HashTable.length()); i < iEnd; ++i) {
    value_type &key = keys[i];
    stringstream sentry;
    sentry.width(3);
    sentry << i << "  key.px@" << (void *)(&key.rawRef_()) << "  ";
    if (!key) {
      sentry << "splatted";
    } else if (key.unboundp()) {
      sentry << "unbound";
    } else if (key.deletedp()) {
      sentry << "deleted";
    } else {
      // key.base_ref().nilp() ) {
      T_sp okey = key;
      sentry << _rep_(okey);
      sentry << "@" << (void *)(key.raw_());
      sentry << "   -->   ";
      value_type val = values[i];
      if (val.sameAsKeyP()) {
        sentry << "sameAsKey!!!";
      } else {
        sentry << _rep_(val);
      }
    }
    ss << "      " << sentry.str();
    clasp_write_string(ss.str(), stream);
  }
}

int WeakKeyHashTable_O::tableSize() const {
  return this->_HashTable.tableSize();
}

bool WeakKeyHashTable_O::fullp() {
  return this->_HashTable.fullp();
}

#if 0
    /*! Return the key/value as three values
      (values key value invalid-key)
      invalid-key is one of nil, :unused :deleted :splatted 
      depending if the key is valid, unused, deleted or a weak link was splatted */
    T_mv WeakKeyHashTable_O::get(int idx)
    {
        T_sp key = (*this->_HashTable.Keys)[idx].backcast();
        T_sp val = (*this->_HashTable.Values)[idx].backcast();
        if ( val.sameAsKeyP() ) {
            val = key;
        }
        if ( key.objectp() ) {
            return Values(key,val,_Nil<T_O>());
        }
        T_sp keyInfo(_Nil<T_O>());
        if ( !key ) {
            keyInfo = kw::_sym_splatted;
        } else if ( key.unboundp()) {
            keyInfo = kw::_sym_unbound;
        } else if ( key.deletedp()) {
            keyInfo = kw::_sym_deleted;
        }
        return Values(_Nil<T_O>(), val, keyInfo );
    }
#endif
SYMBOL_EXPORT_SC_(KeywordPkg, splatted);
SYMBOL_EXPORT_SC_(KeywordPkg, unbound);
SYMBOL_EXPORT_SC_(KeywordPkg, deleted);

#if 0
/* Rehash 'tbl' so that it has 'new_length' buckets. If 'key' is found
 * during this process, update 'key_bucket' to be the index of the
 * bucket containing 'key' and return true, otherwise return false.
 * 
 * %%MPS: When re-hashing the table we reset the associated location
 * dependency and re-add a dependency on each object in the table.
 * This is because the table gets re-hashed when the locations of
 * objects have changed. See topic/location.
 */
    int WeakKeyHashTable_O::rehash(size_t newLength, const value_type& key, size_t& key_bucket)
    {
        WEAK_LOG(BF("entered rehash newLength = %d") % newLength );
        size_t i, length;
        // buckets_t new_keys, new_values;
        int result = 0;
        length = this->_HashTable.Keys->length();
        HashTableType newHashTable(newLength);
        //new_keys = make_buckets(newLength, this->key_ap);
        //new_values = make_buckets(newLength, this->value_ap);
        //new_keys->dependent = new_values;
        //new_values->dependent = new_keys;
#ifdef USE_MPS
        mps_ld_reset(&this->_LocationDependency,gctools::_global_arena);
#endif
        for (i = 0; i < length; ++i) {
            value_type& old_key = (*this->_HashTable.Keys)[i];
            if (!old_key.unboundp() && !old_key.deletedp()) {
                int found;
                size_t b;
#ifdef USE_MPS
                found = WeakKeyHashTable_O::find(newHashTable.Keys, old_key, &this->_LocationDependency, b);
#else
                found = WeakKeyHashTable_O::find(newHashTable.Keys, old_key, b);
#endif
                ASSERT(found);// assert(found);            /* new table shouldn't be full */
                ASSERT((*this->_HashTable.Keys)[b].unboundp()); /* shouldn't be in new table */
                newHashTable.Keys->set(b,old_key);
                (*newHashTable.Values)[b] = (*this->_HashTable.Values)[i];
                if (!key.NULLp() && old_key == key ) {
                    key_bucket = b;
                    result = 1;
                }
                (*newHashTable.Keys).setUsed((*newHashTable.Keys).used()+1); // TAG_COUNT(UNTAG_COUNT(new_keys->used) + 1);
            }
        }
        ASSERT((*newHashTable.Keys).used() == this->_HashTable.tableSize() );
        // assert(UNTAG_COUNT(new_keys->used) == table_size(tbl));
        this->_HashTable.swap(newHashTable);
        return result;
    }
#endif

/* %%MPS: If we fail to find 'key' in the table, and if mps_ld_isstale
 * returns true, then some of the keys in the table might have been
 * moved by the garbage collector: in this case we need to re-hash the
 * table. See topic/location.
 * Return (values value t) or (values nil nil)
 */
T_mv WeakKeyHashTable_O::gethash(T_sp key, T_sp defaultValue) {
  return this->_HashTable.gethash(key, defaultValue);
}

void WeakKeyHashTable_O::setf_gethash(T_sp key, T_sp value) {
  this->_HashTable.set(key, value);
}

void WeakKeyHashTable_O::maphash(std::function<void(T_sp, T_sp)> const &fn) {
  this->_HashTable.maphash(fn);
}

void WeakKeyHashTable_O::remhash(T_sp tkey) {
  this->_HashTable.remhash(tkey);
}

void WeakKeyHashTable_O::clrhash() {
  this->_HashTable.clrhash();
}

#define ARGS_core_makeWeakKeyHashTable "(&optional (size 16))"
#define DECL_core_makeWeakKeyHashTable ""
#define DOCS_core_makeWeakKeyHashTable "makeWeakKeyHashTable"
WeakKeyHashTable_sp core_makeWeakKeyHashTable(Fixnum_sp size) {
  _G();
  int sz = unbox_fixnum(size);
  WeakKeyHashTable_sp ht = gctools::GCObjectAllocator<WeakKeyHashTable_O>::allocate(sz);
  return ht;
}

#define ARGS_core_weakGethash "(key hash-table &optional default-value)"
#define DECL_core_weakGethash ""
#define DOCS_core_weakGethash "weakGethash"
T_mv core_weakGethash(T_sp tkey, WeakKeyHashTable_sp ht, T_sp defaultValue) {
  _G();
  return ht->gethash(tkey, defaultValue);
};

#define ARGS_core_weakSetfGethash "(ht key value)"
#define DECL_core_weakSetfGethash ""
#define DOCS_core_weakSetfGethash "weakSetfGethash"
void core_weakSetfGethash(T_sp key, WeakKeyHashTable_sp ht, T_sp val) {
  _G();
  ht->setf_gethash(key, val);
};

#define ARGS_core_weakRemhash "(ht key)"
#define DECL_core_weakRemhash ""
#define DOCS_core_weakRemhash "weakRemhash"
void core_weakRemhash(WeakKeyHashTable_sp ht, T_sp key) {
  _G();
  ht->remhash(key);
};

#define ARGS_core_weakClrhash "(ht)"
#define DECL_core_weakClrhash ""
#define DOCS_core_weakClrhash "weakClrhash"
void core_weakClrhash(WeakKeyHashTable_sp ht) {
  _G();
  ht->clrhash();
};

#define ARGS_core_weakSplat "(ht idx)"
#define DECL_core_weakSplat ""
#define DOCS_core_weakSplat "weakSplat"
void core_weakSplat(WeakKeyHashTable_sp ht, Fixnum_sp idx) {
  _G();
  T_sp splatted;     // This will be NULL
  splatted.reset_(); // This will force it to be NULL
  TESTING();         // Test the NULL value
  (*ht->_HashTable._Keys).set(unbox_fixnum(idx), WeakKeyHashTable_O::value_type(splatted));
};

#define ARGS_core_weakRehash "(ht &optional sz)"
#define DECL_core_weakRehash ""
#define DOCS_core_weakRehash "weakRehash"
void core_weakRehash(WeakKeyHashTable_sp ht, T_sp sz) {
  _G();
  size_t newLength;
  if (sz.nilp()) {
    newLength = ht->_HashTable._Keys->length() * 2;
  } else {
    newLength = unbox_fixnum(gc::As<Fixnum_sp>(sz));
    //	    newLength = unbox_fixnum(As<Fixnum_O>(sz));
  }
  WeakKeyHashTable_O::value_type dummyKey;
  size_t dummyPos;
  ht->_HashTable.rehash(newLength, dummyKey, dummyPos);
};

void WeakKeyHashTable_O::exposeCando(::core::Lisp_sp lisp) {
  ::core::class_<WeakKeyHashTable_O>()
      .def("weakHashTableSize", &WeakKeyHashTable_O::tableSize);
  CoreDefun(makeWeakKeyHashTable);
  CoreDefun(weakGethash);
  CoreDefun(weakSetfGethash);
  CoreDefun(weakRemhash);
  CoreDefun(weakClrhash);
  CoreDefun(weakSplat);
  CoreDefun(weakRehash);
}

void WeakKeyHashTable_O::exposePython(Lisp_sp lisp) {
  _G();
#ifdef USEBOOSTPYTHON
  PYTHON_CLASS(CorePkg, WeakKeyHashTable, "", "", _lisp);
#endif
}
};
