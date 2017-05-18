/*
    File: hashTable.cc
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

#define DEBUG_LEVEL_NONE

#include <limits>
#include <clasp/core/common.h>
#include <clasp/core/corePackage.h>
#include <clasp/core/environment.h>
#include <clasp/core/multipleValues.h>
#include <clasp/core/symbolTable.h>
#include <clasp/core/hashTable.h>
#include <clasp/core/hashTableEq.h>
#include <clasp/core/hashTableEql.h>
#include <clasp/core/hashTableEqual.h>
#include <clasp/core/hashTableEqualp.h>
#include <clasp/core/array.h>
#include <clasp/core/instance.h>
#include <clasp/core/fileSystem.h>
#include <clasp/core/serialize.h>
#include <clasp/core/evaluator.h>
#include <clasp/core/designators.h>
#include <clasp/core/weakHashTable.h>
#include <clasp/core/wrappers.h>
namespace core {


#ifdef USE_MPS
static int LockDepth = 0;
struct HashTableLocker {
  HashTableLocker() {
    if (LockDepth == 0) {
      //                printf("%s:%d clamping the arena\n", __FILE__, __LINE__ );
      mps_arena_clamp(global_arena);
    }
    ++LockDepth;
  };
  ~HashTableLocker() {
    if (LockDepth == 1) {
      //                printf("%s:%d releasing the arena\n", __FILE__, __LINE__ );
      mps_arena_release(global_arena);
    }
    --LockDepth;
  }
};
#define HASH_TABLE_LOCK() HashTableLocker zzzzHashTableLocker;
#endif
#ifdef USE_BOEHM
#define HASH_TABLE_LOCK()
#endif

// ----------------------------------------------------------------------
//



CL_LAMBDA(&key (test (function eql)) (size 16) (rehash-size 1.5) (rehash-threshold 1.0) weakness debug);
CL_DECLARE();
CL_DOCSTRING("see CLHS");
CL_DEFUN T_sp cl__make_hash_table(T_sp test, Fixnum_sp size, Number_sp rehash_size, Real_sp orehash_threshold, Symbol_sp weakness, T_sp debug) {
  SYMBOL_EXPORT_SC_(KeywordPkg, key);
  if (weakness.notnilp()) {
    if (weakness == INTERN_(kw, key)) {
      return core__make_weak_key_hash_table(clasp_make_fixnum(size));
    }
    SIMPLE_ERROR(BF("Only :weakness :key (weak-key hash tables) are currently supported"));
  }
  int isize = clasp_to_int(size);
  double rehash_threshold = clasp_to_double(orehash_threshold);
  HashTable_sp table = _Nil<HashTable_O>();
  //	_lisp->print(BF("%s:%d - make_hash_table - fix me so that I grow by powers of 2\n") % __FILE__ % __LINE__ );
  if (test == cl::_sym_eq || test == cl::_sym_eq->symbolFunction()) {
    table = HashTableEq_O::create(isize, rehash_size, rehash_threshold);
  } else if (test == cl::_sym_eql || test == cl::_sym_eql->symbolFunction()) {
    table = HashTableEql_O::create(isize, rehash_size, rehash_threshold);
  } else if (test == cl::_sym_equal || test == cl::_sym_equal->symbolFunction()) {
    table = HashTableEqual_O::create(isize, rehash_size, rehash_threshold);
  } else if (test == cl::_sym_equalp || test == cl::_sym_equalp->symbolFunction()) {
    table = HashTableEqualp_O::create(isize, rehash_size, rehash_threshold);
  } else {
    SIMPLE_ERROR(BF("Illegal test[%s] for make-hash-table") % _rep_(test));
  }
  return table;
}

CL_LAMBDA(ht);
CL_DECLARE();
CL_DOCSTRING("hash_table_weakness");
CL_DEFUN Symbol_sp core__hash_table_weakness(T_sp ht) {
  if (WeakKeyHashTable_sp wkht = ht.asOrNull<WeakKeyHashTable_O>()) {
    (void)wkht;
    return kw::_sym_key;
  }
  return _Nil<Symbol_O>();
}

HashTable_sp HashTable_O::create(T_sp test) {
  Fixnum_sp size = make_fixnum(16);
  DoubleFloat_sp rehashSize = DoubleFloat_O::create(2.0);
  DoubleFloat_sp rehashThreshold = DoubleFloat_O::create(0.9);
  HashTable_sp ht = cl__make_hash_table(test, size, rehashSize, rehashThreshold);
  return ht;
}

CL_LAMBDA(function-desig hash-table);
CL_DECLARE();
CL_DOCSTRING("see CLHS");
CL_DEFUN T_mv cl__maphash(T_sp function_desig, T_sp thash_table) {
  //        printf("%s:%d starting maphash on hash-table@%p\n", __FILE__, __LINE__, hash_table.raw_());
  Function_sp func = coerce::functionDesignator(function_desig);
  if (thash_table.nilp()) {
    SIMPLE_ERROR(BF("maphash called with nil hash-table"));
  }
  //        HASH_TABLE_LOCK();
  HashTable_sp hash_table = gc::As<HashTable_sp>(thash_table);
  VectorObjects_sp table = hash_table->_HashTable;
  for (size_t it = 0, itEnd = cl__length(table); it < itEnd; ++it) {
    List_sp first = (*table)[it];
    for (auto cur : first) {
      List_sp entry = oCar(cur);
      T_sp key = oCar(entry);
      T_sp value = oCdr(entry);
      if (!value.unboundp()) {
        eval::funcall(func, key, value);
      }
    }
  }
  //        printf("%s:%d finished maphash on hash-table@%p\n", __FILE__, __LINE__, hash_table.raw_());
  return (Values(_Nil<T_O>()));
}

CL_LAMBDA(hash-table);
CL_DECLARE();
CL_DOCSTRING("See CLHS");
CL_DEFUN T_sp cl__clrhash(HashTable_sp hash_table) {
  hash_table->clrhash();
  return hash_table;
};

CL_LAMBDA(cons);
CL_DECLARE();
CL_DOCSTRING("hashTableEntryDeletedP");
CL_DEFUN bool core__hash_table_entry_deleted_p(T_sp cons) {
  if (!cons.consp())
    SIMPLE_ERROR(BF("Arg must be a cons"));
  return oCdr(gc::As<Cons_sp>(cons)).unboundp();
};

CL_LAMBDA(&rest args);
CL_DECLARE();
CL_DOCSTRING("hash_eql generates an eql hash for a list of objects");
CL_DEFUN int core__hash_eql(List_sp args) {
  HashGenerator hg;
  for (auto cur : args) {
    HashTable_O::sxhash_eql(hg, oCar(cur), NULL);
    if (hg.isFull())
      break;
  }
  return hg.hash();
};

CL_LAMBDA(&rest args);
CL_DECLARE();
CL_DOCSTRING("hash_equal generates an equal hash for a list of objects");
CL_DEFUN int core__hash_equal(List_sp args) {
  HashGenerator hg;
  for (auto cur : args) {
    HashTable_O::sxhash_equal(hg, oCar(cur), NULL);
    if (hg.isFull())
      break;
  }
  return hg.hash();
};

CL_LAMBDA(&rest args);
CL_DECLARE();
CL_DOCSTRING("hash_equalp generates an equalp hash for a list of objects");
CL_DEFUN int core__hash_equalp(List_sp args) {
  HashGenerator hg;
  for (auto cur : args) {
    HashTable_O::sxhash_equalp(hg, oCar(cur), NULL);
    if (hg.isFull())
      break;
  }
  return hg.hash();
};

CL_LAMBDA(key hashtable);
CL_DECLARE();
CL_DOCSTRING("remhash");
CL_DEFUN bool cl__remhash(T_sp key, HashTable_sp ht) {
  return ht->remhash(key);
};

void HashTable_O::clrhash() {
  ASSERT(!clasp_zerop(this->_RehashSize));
  this->setup(4, this->_RehashSize, this->_RehashThreshold);
}

void HashTable_O::setup(uint sz, Number_sp rehashSize, double rehashThreshold) {
  sz = this->resizeEmptyTable(sz);
  this->_InitialSize = sz;
  this->_RehashSize = rehashSize;
  ASSERT(!clasp_zerop(this->_RehashSize));
  this->_RehashThreshold = rehashThreshold;
  this->_HashTableCount = 0;
}

void HashTable_O::sxhash_eq(HashGenerator &hg, T_sp obj, LocationDependencyPtrT ld) {
  if (obj.fixnump()) {
    if (hg.isFilling())
      hg.addPart(obj.unsafe_fixnum());
    return;
  } else if (obj.single_floatp()) {
    if (hg.isFilling())
      hg.addPart(std::abs(::floor(obj.unsafe_single_float())));
    return;
  } else if (obj.characterp()) {
    if (hg.isFilling())
      hg.addPart(obj.unsafe_character());
    return;
  }
  if (obj.objectp()) {
    volatile void* address = &(*obj);
#ifdef USE_MPS
    if (ld) mps_ld_add(ld, global_arena, (mps_addr_t)address );
#endif
    hg.addPart((Fixnum)(((uintptr_clasp_t)address)>>gctools::tag_shift));
    return;
  }
  SIMPLE_ERROR(BF("sxhash_eq cannot hash object"));
}

void HashTable_O::sxhash_eql(HashGenerator &hg, T_sp obj, LocationDependencyPtrT ld) {
  if (obj.fixnump()) {
    if (hg.isFilling())
      hg.addPart(obj.unsafe_fixnum());
    return;
  } else if (obj.single_floatp()) {
    if (hg.isFilling())
      hg.addPart(std::abs(::floor(obj.unsafe_single_float())));
    return;
  } else if (obj.characterp()) {
    if (hg.isFilling())
      hg.addPart(obj.unsafe_character());
    return;
  }
  if (obj.objectp()) {
    if (cl__numberp(obj)) {
      hg.hashObject(obj);
      return;
    }
  }
  volatile void* address = &(*obj);
#ifdef USE_MPS
  if (ld) mps_ld_add(ld, global_arena, (mps_addr_t)address );
#endif
  hg.addPart((Fixnum)(((uintptr_clasp_t)address)>>gctools::tag_shift));
  return;
}

void HashTable_O::sxhash_equal(HashGenerator &hg, T_sp obj, LocationDependencyPtrT ld) {
  if (obj.fixnump()) {
    if (hg.isFilling())
      hg.addPart(obj.unsafe_fixnum());
    return;
  } else if (obj.single_floatp()) {
    if (hg.isFilling())
      hg.addPart(std::abs(::floor(obj.unsafe_single_float())));
    return;
  } else if (obj.characterp()) {
    if (hg.isFilling())
      hg.addPart(obj.unsafe_character());
    return;
  }
  if (obj.objectp()) {
    if (cl__numberp(obj)) {
      hg.hashObject(obj);
      return;
    } else if (String_sp str_obj = obj.asOrNull<String_O>()) {
      if (hg.isFilling())
        str_obj->sxhash_(hg);
      return;
    } else if (BitVector_sp bv_obj = obj.asOrNull<BitVector_O>()) {
      if (hg.isFilling()) bv_obj->sxhash_(hg);
      return;
    } else if (Pathname_sp pobj = obj.asOrNull<Pathname_O>()) {
      if (hg.isFilling())
        HashTable_O::sxhash_equal(hg, pobj->_Host, ld);
      if (hg.isFilling())
        HashTable_O::sxhash_equal(hg, pobj->_Device, ld);
      if (hg.isFilling())
        HashTable_O::sxhash_equal(hg, pobj->_Directory, ld);
      if (hg.isFilling())
        HashTable_O::sxhash_equal(hg, pobj->_Name, ld);
      if (hg.isFilling())
        HashTable_O::sxhash_equal(hg, pobj->_Type, ld);
      if (hg.isFilling())
        HashTable_O::sxhash_equal(hg, pobj->_Version, ld);
      return;
    } else if (Cons_sp cobj = obj.asOrNull<Cons_O>()) {
      if (hg.isFilling())
        HashTable_O::sxhash_equal(hg, oCar(cobj), ld);
      if (hg.isFilling())
        HashTable_O::sxhash_equal(hg, oCdr(cobj), ld);
      return;
    }
  }
  volatile void* address = &(*obj);
#ifdef USE_MPS
  if (ld) mps_ld_add(ld, global_arena, (mps_addr_t)address );
#endif
  hg.addPart((Fixnum)(((uintptr_clasp_t)address)>>gctools::tag_shift));
  return;
}

void HashTable_O::sxhash_equalp(HashGenerator &hg, T_sp obj, LocationDependencyPtrT ld) {
  if (obj.fixnump()) {
    if (hg.isFilling())
      hg.addPart(obj.unsafe_fixnum());
    return;
  } else if (obj.single_floatp()) {
    if (hg.isFilling())
      hg.addPart(std::abs(::floor(obj.unsafe_single_float())));
    return;
  } else if (obj.characterp()) {
    if (hg.isFilling())
      hg.addPart(obj.unsafe_character());
    return;
  }
  if (obj.objectp()) {
    if (cl__numberp(obj)) {
      hg.hashObject(obj);
      return;
    }
    if (cl__stringp(obj)) {
      SimpleString_sp upstr = cl__string_upcase(obj);
      hg.hashObject(upstr);
      return;
    } else if (cl__numberp(obj)) {
      if (hg.isFilling())
        clasp_sxhash(obj, hg);
      return;
    } else if (Pathname_sp pobj = obj.asOrNull<Pathname_O>()) {
      if (hg.isFilling())
        HashTable_O::sxhash_equalp(hg, pobj->_Host, ld);
      if (hg.isFilling())
        HashTable_O::sxhash_equalp(hg, pobj->_Device, ld);
      if (hg.isFilling())
        HashTable_O::sxhash_equalp(hg, pobj->_Directory, ld);
      if (hg.isFilling())
        HashTable_O::sxhash_equalp(hg, pobj->_Name, ld);
      if (hg.isFilling())
        HashTable_O::sxhash_equalp(hg, pobj->_Type, ld);
      if (hg.isFilling())
        HashTable_O::sxhash_equalp(hg, pobj->_Version, ld);
      return;
    } else if (Cons_sp cobj = obj.asOrNull<Cons_O>()) {
      if (hg.isFilling())
        HashTable_O::sxhash_equalp(hg, oCar(cobj), ld);
      if (hg.isFilling())
        HashTable_O::sxhash_equalp(hg, oCdr(cobj), ld);
      return;
    } else if ( obj.generalp() ) {
      General_sp gobj = General_sp((gctools::Tagged)obj.raw_());
      if (gobj->instancep()) {
        Instance_sp iobj = gc::As<Instance_sp>(gobj);
        if (hg.isFilling())
          HashTable_O::sxhash_equalp(hg, iobj->_Class->className(), ld);
        for (size_t i(0), iEnd(iobj->numberOfSlots()); i < iEnd; ++i) {
            if (!iobj->instanceRef(i).unboundp() && hg.isFilling())
              HashTable_O::sxhash_equalp(hg, iobj->instanceRef(i), ld);
        }
        return;
      } else if (BitVector_sp bv_obj = gobj.asOrNull<BitVector_O>()) {
        (void)bv_obj; // silence warning
        IMPLEMENT_MEF(BF("Handle HashTable_O::sxhash_equalp for BitVector"));
      } else if (Array_sp aobj = gobj.asOrNull<Array_O>()) {
        for (size_t i = 0; i < aobj->length(); ++i) {
          if (hg.isFilling()) {
            T_sp obj = aobj->rowMajorAref(i);
            HashTable_O::sxhash_equalp(hg,obj,ld);
          } else {
            break;
          }
        }
        return;
      } else if (HashTable_sp hobj = gobj.asOrNull<HashTable_O>()) {
        (void)hobj; // silence warning
        IMPLEMENT_MEF(BF("Handle HashTable_O::sxhash_equalp for HashTables"));
      }
    }
  }
  volatile void* address = &(*obj);
#ifdef USE_MPS
  if (ld) mps_ld_add(ld, global_arena, (mps_addr_t)address );
#endif
  hg.addPart((Fixnum)(((uintptr_clasp_t)address)>>gctools::tag_shift));
  return;
}

bool HashTable_O::equalp(T_sp other) const {
  if (this == &(*other)) return true;
  if (!other.generalp()) return false;
  if (!gc::IsA<HashTable_sp>(other)) return false;
  HashTable_sp hto = gc::As_unsafe<HashTable_sp>(other);
  if (this->hashTableTest() != hto->hashTableTest()) return false;
  if (this->hashTableCount() != hto->hashTableCount()) return false;
  this->map_while_true( [&hto] (T_sp key, T_sp val)->bool const {
      T_sp other_value = hto->gethash(key);
      if (!cl__equalp(val,other_value)) return false;
      return true;
    }
    );
  return true;
}

List_sp HashTable_O::keysAsCons() {
  List_sp res = _Nil<T_O>();
  this->mapHash([&res](T_sp key, T_sp val) {
      res = Cons_O::create(key,res);
  });
  return res;
}

#if 0
    void HashTable_O::serialize(::serialize::SNodeP node)
    {
        this->Bases::serialize(node);
	// Archie other instance variables here
	node->attribute("test",this->_Test);
	node->archiveObject("rehashSize",this->_RehashSize);
	node->attribute("rehashThreshold",this->_RehashThreshold);
    }
#endif

void HashTable_O::fields(Record_sp node) {
  // this->Base::fields(node);
  node->field(INTERN_(core, rehash_size), this->_RehashSize);
  node->/*pod_*/ field(INTERN_(core, rehash_threshold), this->_RehashThreshold);
  switch (node->stage()) {
  case Record_O::initializing:
  case Record_O::loading: {
    Vector_sp keyValueVec;
    node->field(INTERN_(core, data), keyValueVec);
    this->clrhash();
    for (size_t i(0), iEnd(cl__length(keyValueVec)); i < iEnd; ++++i) {
      T_sp key = keyValueVec->rowMajorAref(i + 0);
      T_sp val = keyValueVec->rowMajorAref(i + 1);
      this->hash_table_setf_gethash(key, val);
    };
  } break;
  case Record_O::saving: {
    Vector_sp keyValueVec = core__make_vector(cl::_sym_T_O, 2 * this->hashTableCount());
    size_t idx = 0;
    this->mapHash([&idx, &keyValueVec](T_sp key, T_sp val) {
        keyValueVec->rowMajorAset(idx++,key);
        keyValueVec->rowMajorAset(idx++,val);
    });
    node->field(INTERN_(core, data), keyValueVec);
  } break;
  case Record_O::patching: {
    IMPLEMENT_MEF(BF("Add support to patch hash tables"));
  } break;
  }
}

uint HashTable_O::resizeEmptyTable(uint sz) {
  if (sz < 16) sz = 16;
  this->_HashTable = VectorObjects_O::make(sz, _Nil<T_O>() );
  ENSURE_VALID_OBJECT(this->_HashTable);
#ifdef USE_MPS
  mps_ld_reset(const_cast<mps_ld_t>(&(this->_LocationDependency)), global_arena);
#endif
  return sz;
}

CL_LISPIFY_NAME("hash-table-count");
CL_DEFMETHOD uint HashTable_O::hashTableCount() const {
  return this->_HashTableCount;
}

uint HashTable_O::calculateHashTableCount() const {
  uint cnt = 0;
  for (size_t it(0), itEnd(cl__length(ENSURE_VALID_OBJECT(this->_HashTable))); it < itEnd; ++it) {
    cnt += cl__length((ENSURE_VALID_OBJECT(this->_HashTable)->operator[](it)));
  }
  return cnt;
}

CL_LISPIFY_NAME("hash-table-size");
CL_DEFMETHOD uint HashTable_O::hashTableSize() const {
  return cl__length(ENSURE_VALID_OBJECT(this->_HashTable));
}

bool HashTable_O::keyTest(T_sp entryKey, T_sp searchKey) const {
  SUBCLASS_MUST_IMPLEMENT();
}

gc::Fixnum HashTable_O::sxhashKey(T_sp obj, gc::Fixnum bound, bool willAddKey) const {
  SUBCLASS_MUST_IMPLEMENT();
}

List_sp HashTable_O::findAssoc(gc::Fixnum index, T_sp key) const {
  LOG(BF("findAssoc at index %d\n") %index);
  for (auto cur : gc::As_unsafe<List_sp>((*ENSURE_VALID_OBJECT(this->_HashTable))[index])) {
    List_sp pair = CONS_CAR(cur);
    ASSERT(pair.consp());
    if (this->keyTest(CONS_CAR(pair), key)) return pair;
  }
  return _Nil<T_O>();
}

  CL_LAMBDA(key hash-table &optional default-value);
  CL_DECLARE();
  CL_DOCSTRING("gethash");
  CL_DEFUN T_mv cl__gethash(T_sp key, T_sp hashTable, T_sp default_value) {
    HashTable_sp ht = gc::As<HashTable_sp>(hashTable);
    return ht->gethash(key, default_value);
  };

  List_sp HashTable_O::bucketsFind(T_sp key) const {
    ASSERT(this->_HashTable);
    cl_index length = cl__length(ENSURE_VALID_OBJECT(this->_HashTable));
//  printf("%s:%d:%s  _HashTable length = %ld\n", __FILE__, __LINE__, __FUNCTION__, length );
    cl_index index = this->safe_sxhashKey(key, length, false);
    List_sp keyValueCons = this->findAssoc(index, key);
    return keyValueCons;
  }

  List_sp HashTable_O::tableRef(T_sp key) {
    List_sp keyValueCons = this->bucketsFind(key);
    if (keyValueCons.notnilp())
      return keyValueCons;
#if defined(USE_MPS)
  // Location dependency test if key is stale
    if (key.objectp()) {
      void *blockAddr = &(*key);
      if (mps_ld_isstale(const_cast<mps_ld_t>(&(this->_LocationDependency)), global_arena, blockAddr)) {
        keyValueCons = this->rehash(false, key);
      }
    }
#endif
    return keyValueCons;
  }

  CL_LAMBDA(ht);
  CL_DECLARE();
  CL_DOCSTRING("hashTableForceRehash");
  CL_DEFUN void core__hash_table_force_rehash(HashTable_sp ht) {
    ht->rehash(false, _Unbound<T_O>());
  }

  T_mv HashTable_O::gethash(T_sp key, T_sp default_value) {
    LOG(BF("gethash looking for key[%s]") % _rep_(key));
    List_sp keyValuePair = this->tableRef(key);
    LOG(BF("Found keyValueCons")); // % keyValueCons->__repr__() ); INFINITE-LOOP
    if (keyValuePair.nilp()) {
      LOG(BF("valueOrUnbound is unbound - returning default"));
      return (Values(default_value, _Nil<T_O>()));
    }
    T_sp value = oCdr(keyValuePair);
    if (value.unboundp()) {
      LOG(BF("valueOrUnbound is unbound - returning default"));
      return (Values(default_value, _Nil<T_O>()));
    }
    LOG(BF("Found assoc - returning")); // : %s") % res->__repr__() );  INFINITE-LOOP
    return (Values(value, _lisp->_true()));
  }

  CL_LISPIFY_NAME("core:hashIndex");
  CL_DEFMETHOD gc::Fixnum HashTable_O::hashIndex(T_sp key) const {
    gc::Fixnum idx = this->safe_sxhashKey(key, cl__length(ENSURE_VALID_OBJECT(this->_HashTable)), false);
    return idx;
  }

  List_sp HashTable_O::find(T_sp key) {
    List_sp keyValue = this->tableRef(key);
    if (keyValue.nilp())
      return keyValue;
    if (oCdr(keyValue).unboundp())
      return _Nil<T_O>();
    return keyValue;
  }

  bool HashTable_O::contains(T_sp key) {
    List_sp keyValue = this->find(key);
    if (keyValue.nilp())
      return false;
    return true;
  }

  bool HashTable_O::remhash(T_sp key) {
    List_sp keyValuePair = this->tableRef(key);
    if (keyValuePair.nilp() || oCdr(keyValuePair).unboundp())
      return false;
    keyValuePair.asCons()->setCdr(_Unbound<T_O>());
    this->_HashTableCount--;
    return true;
  }

  CL_LISPIFY_NAME("core:hashTableSetfGethash");
  CL_DEFMETHOD T_sp HashTable_O::hash_table_setf_gethash(T_sp key, T_sp value) {
    LOG(BF("About to hash_table_setf_gethash for %s@%p -> %s@%p\n") % _rep_(key) % (void*)&(*key) % _rep_(value) % (void*)&(*value));
    List_sp keyValuePair = this->tableRef(key);
    if (keyValuePair.nilp()) {
      gc::Fixnum index = this->safe_sxhashKey(key, cl__length(ENSURE_VALID_OBJECT(this->_HashTable)), true /*Will add key*/);
      Cons_sp newKeyValue = Cons_O::create(key, value);
    //            printf("%s:%d  Inserted newKeyValue@%p\n", __FILE__, __LINE__, newKeyValue.raw_());
      Cons_sp newEntry = Cons_O::create(newKeyValue, ENSURE_VALID_OBJECT(this->_HashTable)->operator[](index));
      ENSURE_VALID_OBJECT(this->_HashTable)->operator[](index) = newEntry;
    //            this->_HashTableEntryCount++;
      ++(this->_HashTableCount);
    } else if (oCdr(keyValuePair).unboundp()) {
      keyValuePair.asCons()->setCdr(value);
      ++(this->_HashTableCount);
    } else {
      keyValuePair.asCons()->setCdr(value);
    }
    if (this->_HashTableCount > this->_RehashThreshold * cl__length(ENSURE_VALID_OBJECT(this->_HashTable))) {
      LOG(BF("Expanding hash table"));
      this->rehash(true, _Unbound<T_O>());
    }
    return value;
  }

  List_sp HashTable_O::rehash(bool expandTable, T_sp findKey) {
  //        printf("%s:%d rehash of hash-table@%p\n", __FILE__, __LINE__,  this );
    ASSERTF(!clasp_zerop(this->_RehashSize), BF("RehashSize is zero - it shouldn't be"));
    ASSERTF(cl__length(ENSURE_VALID_OBJECT(this->_HashTable)) != 0, BF("HashTable is empty in expandHashTable - this shouldn't be"));
    List_sp foundKeyValuePair(_Nil<T_O>());
    LOG(BF("At start of expandHashTable current hash table size: %d") % cl__length(ENSURE_VALID_OBJECT(this->_HashTable)));
    gc::Fixnum newSize = 0;
    if (expandTable) {
      if (cl__integerp(this->_RehashSize)) {
        newSize = cl__length(ENSURE_VALID_OBJECT(this->_HashTable)) + clasp_to_int(gc::As<Integer_sp>(this->_RehashSize));
      } else if (cl__floatp(this->_RehashSize)) {
        newSize = cl__length(ENSURE_VALID_OBJECT(this->_HashTable)) * clasp_to_double(this->_RehashSize);
      }
    } else {
      newSize = cl__length(ENSURE_VALID_OBJECT(this->_HashTable));
    }
    VectorObjects_sp oldTable = ENSURE_VALID_OBJECT(this->_HashTable);
    newSize = this->resizeEmptyTable(newSize);
    LOG(BF("Resizing table to size: %d") % newSize);
    size_t oldSize = cl__length(oldTable);
    for (size_t it(0), itEnd(oldSize); it < itEnd; ++it) {
      for (auto cur : coerce_to_list((*oldTable)[it])) {
        List_sp pair = oCar(cur);
        T_sp key = oCar(pair);
        T_sp value = oCdr(pair);
        if (!value.unboundp()) {
          // key/value represent a valid entry in the hash table
          //
          // If findKey is not unbound and we haven't already found
          // the value that it points to.
          // then while we are rehashing the hash table we are also looking
          // for the key it points to.
          // Check if the current key matches findKey and if it does
          // set foundKeyValuePair so that it will be returned when
          // the rehash is complete.
          if (foundKeyValuePair.nilp() && !findKey.unboundp()) {
            if (this->keyTest(key, findKey)) {
              foundKeyValuePair = pair;
            }
          }
          gc::Fixnum index = this->safe_sxhashKey(key, newSize, true /* Will add key */);
          LOG(BF("Re-indexing key[%s] to index[%d]") % _rep_(key) % index);
          Cons_sp newCur = Cons_O::create(pair, ENSURE_VALID_OBJECT(this->_HashTable)->operator[](index));
          ENSURE_VALID_OBJECT(this->_HashTable)->operator[](index) = newCur;
        }
      }
    }
    return foundKeyValuePair;
  }

  string HashTable_O::__repr__() const {
    stringstream ss;
    ss << "#<" << this->_instanceClass()->classNameAsString() << " :count " << this->_HashTableCount;
    ss << " :total-alist-entries " << this->calculateHashTableCount();
    ss << " @" << (void *)(this) << "> ";
    return ss.str();
  //	return this->hash_table_dump();
  }

#define DUMP_LOW_LEVEL 1

  void dump_one_entry(HashTable_sp ht, size_t it, stringstream &ss, List_sp first) {
    for (auto cur : first) {
      List_sp pair = oCar(cur);
      T_sp key = oCar(pair);
      T_sp value = oCdr(pair);
#ifdef DUMP_LOW_LEVEL
      ss << "     ( ";
      size_t hi = ht->hashIndex(key);
      if (hi != it)
        ss << "!!!ERROR-wrong bucket!!! hi=" << hi;
      ss << "hashIndex(key)=" << ht->hashIndex(key) << " ";
      if ((key).consp()) {
        List_sp ckey = key;
        ss << "(cons " << oCar(ckey).raw_() << " . " << oCdr(ckey).raw_() << ")";
      } else {
        ss << key.raw_();
      }
      ss << ", " << value.raw_() << ")@" << pair.raw_() << " " << std::endl;
#else
      ss << "     " << _rep_(pair) << std::endl;
#endif
    }
  };
  CL_LISPIFY_NAME("core:hashTableDump");
  CL_DEFMETHOD string HashTable_O::hash_table_dump(Fixnum start, T_sp end) const {
    stringstream ss;
#ifndef DUMP_LOW_LEVEL
    ss << "#<" << this->_instanceClass()->classNameAsString() << std::endl;
#endif
    int iend(cl__length(ENSURE_VALID_OBJECT(this->_HashTable)));
    if (end.notnilp()) {
      iend = clasp_to_fixnum(gc::As<Fixnum_sp>(end));
    }
    if (start < 0 || start >= cl__length(ENSURE_VALID_OBJECT(this->_HashTable))) {
      SIMPLE_ERROR(BF("start must be [0,%d)") % cl__length(ENSURE_VALID_OBJECT(this->_HashTable)));
    }
    if (iend < start || iend > cl__length(ENSURE_VALID_OBJECT(this->_HashTable))) {
      SIMPLE_ERROR(BF("end must be nil or [%d,%d)") % start % cl__length(ENSURE_VALID_OBJECT(this->_HashTable)));
    }
    for (size_t it(start), itEnd(iend); it < itEnd; ++it) {
      List_sp first = ENSURE_VALID_OBJECT(this->_HashTable)->operator[](it);
      ss << "HashTable[" << it << "]: " << std::endl;
      dump_one_entry(this->asSmartPtr(), it, ss, first);
    }
#ifndef DUMP_LOW_LEVEL
    ss << "> " << std::endl;
#endif
    return ss.str();
  }

  void HashTable_O::mapHash(std::function<void(T_sp, T_sp)> const &fn) {
  //        HASH_TABLE_LOCK();
    VectorObjects_sp table = ENSURE_VALID_OBJECT(this->_HashTable);
    for (size_t it(0), itEnd(cl__length(table)); it < itEnd; ++it) {
      List_sp first = coerce_to_list((*table)[it]);
      for (auto cur : first) {
        List_sp pair = oCar(cur);
        T_sp key = oCar(pair);
        T_sp value = oCdr(pair);
        if (!value.unboundp())
          fn(key, value);
      }
    }
  }

  void HashTable_O::map_while_true(std::function<bool(T_sp, T_sp)> const &fn) const {
  //        HASH_TABLE_LOCK();
    VectorObjects_sp table = ENSURE_VALID_OBJECT(this->_HashTable);
    for (size_t it(0), itEnd(cl__length(table)); it < itEnd; ++it) {
      List_sp first = (*table)[it];
      for (auto cur : first) {
        List_sp pair = oCar(cur);
        T_sp key = oCar(pair);
        T_sp value = oCdr(pair);
        if (!value.unboundp()) {
          bool cont = fn(key, value);
          if (!cont)
            return;
        }
      }
    }
  }

  void HashTable_O::lowLevelMapHash(KeyValueMapper *mapper) const {
  //        HASH_TABLE_LOCK();
    VectorObjects_sp table = ENSURE_VALID_OBJECT(this->_HashTable);
    for (size_t it(0), itEnd(cl__length(table)); it < itEnd; ++it) {
      T_sp first = (*table)[it];
      List_sp l = coerce_to_list(first);
      for (auto cur : l) {
      //Cons_sp cur(*it); //it.asCons());
      //		Cons_sp cur(reinterpret_cast<core::Cons_O*>(it));

      //	    for ( auto it = l.begin(); it != l.end(); ++it ) {
      //Cons_sp cur(reinterpret_cast<core::Cons_O*>(&**it));
      //	    	for ( Cons_sp cur=first.as<List_V>(); cur.consp(); cur = cCdr(cur) )
        Cons_sp pair = gc::As<Cons_sp>(oCar(cur));
        T_sp key = oCar(pair);
        T_sp value = oCdr(pair);
        if (!value.unboundp()) {
          if (!mapper->mapKeyValue(key, value))
            goto DONE;
        }
      }
    }
  DONE:
    return;
  }

  CL_LISPIFY_NAME("core:hashTableNumberOfHashes");
  CL_DEFMETHOD int HashTable_O::hashTableNumberOfHashes() const {
    return cl__length(ENSURE_VALID_OBJECT(this->_HashTable));
  }

  CL_LISPIFY_NAME("core:hashTableAlistAtHash");
  CL_DEFMETHOD List_sp HashTable_O::hashTableAlistAtHash(int hash) const {
    ASSERTF(hash >= 0 && hash < cl__length(ENSURE_VALID_OBJECT(this->_HashTable)), BF("Illegal hash value[%d] must between [0,%d)") % hash % cl__length(ENSURE_VALID_OBJECT(this->_HashTable)));
    return ENSURE_VALID_OBJECT(this->_HashTable)->operator[](hash);
  }

  string HashTable_O::keysAsString() {
    stringstream ss;
    this->mapHash([&ss, this](T_sp key, T_sp val) {
        ss << _rep_(key) << " ";
      });
    return ss.str();
  }

  T_mv clasp_gethash_safe(T_sp key, T_sp thashTable, T_sp default_) {
    HashTable_sp hashTable = gc::As<HashTable_sp>(thashTable);
    return hashTable->gethash(key,default_);
  }



  SYMBOL_EXPORT_SC_(ClPkg, make_hash_table);
  SYMBOL_EXPORT_SC_(ClPkg, maphash);
  SYMBOL_EXPORT_SC_(ClPkg, clrhash);
  SYMBOL_SC_(CorePkg, hash_eql);
  SYMBOL_SC_(CorePkg, hash_equal);
  SYMBOL_SC_(CorePkg, hash_equalp);
  SYMBOL_EXPORT_SC_(ClPkg, remhash);
  SYMBOL_EXPORT_SC_(ClPkg, gethash);




}; /* core */
