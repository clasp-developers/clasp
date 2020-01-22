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

    WeakKeyHashTable_O::WeakKeyHashTable_O() : _HashTable(16, core::make_single_float(2.0),0.5) {};
  
void WeakKeyHashTable_O::initialize() {
  this->_HashTable.initialize();
}
};


namespace core {



Number_sp WeakKeyHashTable_O::rehash_size()
{
  return this->_HashTable._RehashSize;
}

double WeakKeyHashTable_O::rehash_threshold() {
  return this->_HashTable._RehashThreshold;
}

T_sp WeakKeyHashTable_O::hash_table_test() {
  return cl::_sym_eq;
}


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

SYMBOL_EXPORT_SC_(KeywordPkg, splatted);
SYMBOL_EXPORT_SC_(KeywordPkg, unbound);
SYMBOL_EXPORT_SC_(KeywordPkg, deleted);

/* %%MPS: If we fail to find 'key' in the table, and if mps_ld_isstale
 * returns true, then some of the keys in the table might have been
 * moved by the garbage collector: in this case we need to re-hash the
 * table. See topic/location.
 * Return (values value t) or (values nil nil)
 */
T_mv WeakKeyHashTable_O::gethash(T_sp key, T_sp defaultValue) {
  return this->_HashTable.gethash(key, defaultValue);
}

void WeakKeyHashTable_O::maphashLowLevel(std::function<void(T_sp, T_sp)> const &fn) {
  this->_HashTable.maphash(fn);
}

void WeakKeyHashTable_O::maphash(T_sp func) {
  this->_HashTable.maphashFn(func);
}

bool WeakKeyHashTable_O::remhash(T_sp tkey) {
  return this->_HashTable.remhash(tkey);
}

T_sp WeakKeyHashTable_O::clrhash() {
  this->_HashTable.clrhash();
  return this->asSmartPtr();
}

string WeakKeyHashTable_O::__repr__() const {
  stringstream ss;
  ss << "#<" << this->className() << " :size " << this->_HashTable.tableSize() << ">";
  return ss.str();
}

CL_LAMBDA(&optional (size 16));
CL_DECLARE();
CL_DOCSTRING("makeWeakKeyHashTable");
CL_DEFUN WeakKeyHashTable_sp core__make_weak_key_hash_table(Fixnum_sp size) {
  int sz = unbox_fixnum(size);
  WeakKeyHashTable_sp ht = gctools::GC<WeakKeyHashTable_O>::allocate(sz,DoubleFloat_O::create(2.0),0.5);
  return ht;
}

CL_LAMBDA(key hash-table &optional default-value);
CL_DECLARE();
CL_DOCSTRING("weakGethash");
CL_DEFUN T_mv core__weak_gethash(T_sp tkey, WeakKeyHashTable_sp ht, T_sp defaultValue) {
  return ht->gethash(tkey, defaultValue);
};

T_sp WeakKeyHashTable_O::hash_table_setf_gethash(T_sp key, T_sp value) {
  this->_HashTable.set(key, value);
  return value;
}

CL_LAMBDA(ht key value);
CL_DECLARE();
CL_DOCSTRING("weakSetfGethash");
CL_DEFUN void core__weak_setf_gethash(T_sp key, WeakKeyHashTable_sp ht, T_sp val) {
  ht->hash_table_setf_gethash(key, val);
};

CL_LAMBDA(ht key);
CL_DECLARE();
CL_DOCSTRING("weakRemhash");
CL_DEFUN void core__weak_remhash(WeakKeyHashTable_sp ht, T_sp key) {
  ht->remhash(key);
};


CL_LAMBDA(ht);
CL_DECLARE();
CL_DOCSTRING("weakClrhash");
CL_DEFUN void core__weak_clrhash(WeakKeyHashTable_sp ht) {
  ht->clrhash();
};

CL_LAMBDA(ht idx);
CL_DECLARE();
CL_DOCSTRING("weakSplat");
CL_DEFUN void core__weak_splat(WeakKeyHashTable_sp ht, Fixnum_sp idx) {
  T_sp splatted;     // This will be NULL
  splatted.reset_(); // This will force it to be NULL
  TESTING();         // Test the NULL value
  (*ht->_HashTable._Keys).set(unbox_fixnum(idx), WeakKeyHashTable_O::value_type(splatted));
};
CL_LAMBDA(ht &optional sz);
CL_DECLARE();
CL_DOCSTRING("weakRehash");
CL_DEFUN void core__weak_rehash(WeakKeyHashTable_sp ht, T_sp sz) {
  size_t newLength;
  if (sz.nilp()) {
    newLength = ht->_HashTable._Keys->length() * 2;
  } else {
    newLength = unbox_fixnum(gc::As<Fixnum_sp>(sz));
    //	    newLength = unbox_fixnum(As<Fixnum_O>(sz));
  }
  WeakKeyHashTable_O::value_type dummyKey;
  size_t dummyPos;
  ht->_HashTable.rehash(dummyKey, dummyPos);
};
};




SYMBOL_EXPORT_SC_(KeywordPkg, splatted);
SYMBOL_EXPORT_SC_(KeywordPkg, unbound);
SYMBOL_EXPORT_SC_(KeywordPkg, deleted);

