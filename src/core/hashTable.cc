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
#define DEBUG_LEVEL_FULL

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
#include <clasp/core/vectorObjects.h>
#include <clasp/core/str.h>
#include <clasp/core/lispString.h>
#include <clasp/core/instance.h>
#include <clasp/core/bitVector.h>
#include <clasp/core/fileSystem.h>
#include <clasp/core/serialize.h>
#include <clasp/core/evaluator.h>
#include <clasp/core/designators.h>
#include <clasp/core/weakHashTable.h>
#include <clasp/core/wrappers.h>
namespace core {

bool DebugHashTable = false;

#define ARGS_core_DebugHashTable "(on)"
#define DECL_core_DebugHashTable ""
#define DOCS_core_DebugHashTable "DebugHashTable"
void core_DebugHashTable(bool don) {
  DebugHashTable = don;
}

#ifdef USE_MPS
static int LockDepth = 0;
struct HashTableLocker {
  HashTableLocker() {
    if (LockDepth == 0) {
      //                printf("%s:%d clamping the arena\n", __FILE__, __LINE__ );
      mps_arena_clamp(gctools::_global_arena);
    }
    ++LockDepth;
  };
  ~HashTableLocker() {
    if (LockDepth == 1) {
      //                printf("%s:%d releasing the arena\n", __FILE__, __LINE__ );
      mps_arena_release(gctools::_global_arena);
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

EXPOSE_CLASS(core, HashTable_O);

#define LOCK_cl_make_hash_table 1
#define DOCS_cl_make_hash_table "see CLHS"
#define ARGS_cl_make_hash_table "(&key (test (function eql)) (size 16) (rehash-size 1.5) (rehash_threshold 1.0) weakness debug)"
#define DECL_cl_make_hash_table ""
T_sp cl_make_hash_table(T_sp test, Fixnum_sp size, Number_sp rehash_size, Real_sp orehash_threshold, Symbol_sp weakness, T_sp debug) {
  SYMBOL_EXPORT_SC_(KeywordPkg, key);
  if (weakness.notnilp()) {
    if (weakness == INTERN_(kw, key)) {
      return core_makeWeakKeyHashTable(clasp_make_fixnum(size));
    }
    SIMPLE_ERROR(BF("Only :weakness :key (weak-key hash tables) are currently supported"));
  }
#ifdef DEBUG_HASH_TABLE
//  printf("%s:%d make_hash_table WARNING DEBUG_HASH_TABLE is on\n", __FILE__, __LINE__);
#endif
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
#ifdef DEBUG_HASH_TABLE
  if (table.notnilp()) {
    table->_DebugHashTable = debug.isTrue();
  }
#endif
  return table;
}

#define ARGS_core_hash_table_weakness "(ht)"
#define DECL_core_hash_table_weakness ""
#define DOCS_core_hash_table_weakness "hash_table_weakness"
Symbol_sp core_hash_table_weakness(T_sp ht) {
  if (WeakKeyHashTable_sp wkht = ht.asOrNull<WeakKeyHashTable_O>()) {
    (void)wkht;
    return kw::_sym_key;
  }
  return _Nil<Symbol_O>();
}

HashTable_sp HashTable_O::create(T_sp test) {
  _G();
  Fixnum_sp size = make_fixnum(16);
  DoubleFloat_sp rehashSize = DoubleFloat_O::create(2.0);
  DoubleFloat_sp rehashThreshold = DoubleFloat_O::create(0.9);
  HashTable_sp ht = cl_make_hash_table(test, size, rehashSize, rehashThreshold);
  return ht;
}

#define ARGS_cl_maphash "(function_desig hash_table)"
#define DECL_cl_maphash ""
#define DOCS_cl_maphash "see CLHS"
#define FILE_cl_maphash __FILE__
#define LINE_cl_maphash __LINE__
T_mv cl_maphash(T_sp function_desig, T_sp thash_table) {
  _G();
  //        printf("%s:%d starting maphash on hash-table@%p\n", __FILE__, __LINE__, hash_table.raw_());
  Function_sp func = coerce::functionDesignator(function_desig);
  if (thash_table.nilp()) {
    SIMPLE_ERROR(BF("maphash called with nil hash-table"));
  }
  //        HASH_TABLE_LOCK();
  HashTable_sp hash_table = gc::As<HashTable_sp>(thash_table);
  VectorObjects_sp table = hash_table->_HashTable;
  for (size_t it = 0, itEnd = cl_length(table); it < itEnd; ++it) {
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

#define ARGS_cl_clrhash "(hash_table)"
#define DECL_cl_clrhash ""
#define DOCS_cl_clrhash "See CLHS"
T_mv cl_clrhash(HashTable_sp hash_table) {
  _G();
  hash_table->clrhash();
  return (Values(_Nil<T_O>()));
};

#define ARGS_af_hashTableEntryDeletedP "(cons)"
#define DECL_af_hashTableEntryDeletedP ""
#define DOCS_af_hashTableEntryDeletedP "hashTableEntryDeletedP"
bool af_hashTableEntryDeletedP(T_sp cons) {
  _G();
  if (!cons.consp())
    SIMPLE_ERROR(BF("Arg must be a cons"));
  return oCdr(gc::As<Cons_sp>(cons)).unboundp();
};

#define ARGS_af_hash_eql "(&rest args)"
#define DECL_af_hash_eql ""
#define DOCS_af_hash_eql "hash_eql generates an eql hash for a list of objects"
int af_hash_eql(List_sp args) {
  _G();
  HashGenerator hg;
  for (auto cur : args) {
    HashTable_O::sxhash_eql(hg, oCar(cur), NULL);
    if (hg.isFull())
      break;
  }
  return hg.hash();
};

#define ARGS_af_hash_equal "(&rest args)"
#define DECL_af_hash_equal ""
#define DOCS_af_hash_equal "hash_equal generates an equal hash for a list of objects"
int af_hash_equal(List_sp args) {
  _G();
  HashGenerator hg;
  for (auto cur : args) {
    HashTable_O::sxhash_equal(hg, oCar(cur), NULL);
    if (hg.isFull())
      break;
  }
  return hg.hash();
};

#define ARGS_af_hash_equalp "(&rest args)"
#define DECL_af_hash_equalp ""
#define DOCS_af_hash_equalp "hash_equalp generates an equalp hash for a list of objects"
int af_hash_equalp(List_sp args) {
  _G();
  HashGenerator hg;
  for (auto cur : args) {
    HashTable_O::sxhash_equalp(hg, oCar(cur), NULL);
    if (hg.isFull())
      break;
  }
  return hg.hash();
};

#define ARGS_af_remhash "(key hashtable)"
#define DECL_af_remhash ""
#define DOCS_af_remhash "remhash"
bool af_remhash(T_sp key, HashTable_sp ht) {
  _G();
  return ht->remhash(key);
};

void HashTable_O::clrhash() {
  _G();
  ASSERT(!clasp_zerop(this->_RehashSize));
  this->setup(4, this->_RehashSize, this->_RehashThreshold);
}

void HashTable_O::setup(uint sz, Number_sp rehashSize, double rehashThreshold) {
  _OF();
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
#ifdef USE_MPS
    if (ld)
      mps_ld_add(ld, gctools::_global_arena, SmartPtrToBasePtr(obj));
#endif
    obj->T_O::sxhash_(hg);
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
    if (cl_numberp(obj)) {
      hg.hashObject(obj);
      return;
    }
  }
#ifdef USE_MPS
  if (ld)
    mps_ld_add(ld, gctools::_global_arena, SmartPtrToBasePtr(obj));
#endif
  obj->T_O::sxhash_(hg);
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
    if (cl_numberp(obj)) {
      hg.hashObject(obj);
      return;
    } else if (String_sp str_obj = obj.asOrNull<String_O>()) {
      if (hg.isFilling())
        str_obj->sxhash_(hg);
      return;
    } else if (BitVector_sp bv_obj = obj.asOrNull<BitVector_O>()) {
      (void)bv_obj;
      IMPLEMENT_MEF(BF("Hash bit-vectors"));
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
#ifdef USE_MPS
  if (ld)
    mps_ld_add(ld, gctools::_global_arena, SmartPtrToBasePtr(obj));
#endif
  obj->T_O::sxhash_(hg);
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
    if (cl_numberp(obj)) {
      hg.hashObject(obj);
      return;
    }
    if (Str_sp str = obj.asOrNull<Str_O>()) {
      Str_sp upstr = cl_string_upcase(str);
      hg.hashObject(upstr);
      return;
    } else if (cl_numberp(obj)) {
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
    } else if (obj->instancep()) {
      Instance_sp iobj = gc::As<Instance_sp>(obj);
      if (hg.isFilling())
        HashTable_O::sxhash_equalp(hg, iobj->_Class->className(), ld);
      for (int i(0), iEnd(iobj->_Slots.size()); i < iEnd; ++i) {
        if (!iobj->_Slots[i].unboundp() && hg.isFilling())
          HashTable_O::sxhash_equalp(hg, iobj->_Slots[i], ld);
      }
      return;
    } else if (BitVector_sp bv_obj = obj.asOrNull<Array_O>()) {
      (void)bv_obj; // silence warning
      IMPLEMENT_MEF(BF("Handle HashTable_O::sxhash_equalp for BitVector"));
    } else if (Array_sp aobj = obj.asOrNull<Array_O>()) {
      (void)aobj; // silence warning
      IMPLEMENT_MEF(BF("Handle HashTable_O::sxhash_equalp for Arrays"));
    } else if (HashTable_sp hobj = obj.asOrNull<HashTable_O>()) {
      (void)hobj; // silence warning
      IMPLEMENT_MEF(BF("Handle HashTable_O::sxhash_equalp for HashTables"));
    }
  }
#ifdef USE_MPS
  if (ld)
    mps_ld_add(ld, gctools::_global_arena, SmartPtrToBasePtr(obj));
#endif
  obj->T_O::sxhash_(hg);
}

bool HashTable_O::equalp(T_sp other) const {
  IMPLEMENT_MEF(BF("Implement HashTable_O::equalp"));
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
    for (size_t i(0), iEnd(cl_length(keyValueVec)); i < iEnd; ++++i) {
      T_sp key = (*keyValueVec)[i + 0];
      T_sp val = (*keyValueVec)[i + 1];
      this->hash_table_setf_gethash(key, val);
    };
  } break;
  case Record_O::saving: {
    Vector_sp keyValueVec = core_make_vector(cl::_sym_T_O, 2 * this->hashTableCount());
    size_t idx = 0;
    this->mapHash([&idx, &keyValueVec](T_sp key, T_sp val) {
        (*keyValueVec)[idx++] = key;
        (*keyValueVec)[idx++] = val;
    });
    node->field(INTERN_(core, data), keyValueVec);
  } break;
  case Record_O::patching: {
    IMPLEMENT_MEF(BF("Add support to patch hash tables"));
  } break;
  }
}

uint HashTable_O::resizeEmptyTable(uint sz) {
  if (sz < 4)
    sz = 4;
  this->_HashTable = VectorObjects_O::make(_Nil<T_O>(), _Nil<T_O>(), sz, false, cl::_sym_T_O);
#ifdef USE_MPS
  mps_ld_reset(const_cast<mps_ld_t>(&(this->_LocationDependencyTracker)), gctools::_global_arena);
#endif
  return sz;
}

uint HashTable_O::hashTableCount() const {
  return this->_HashTableCount;
}

uint HashTable_O::calculateHashTableCount() const {
  uint cnt = 0;
  for (size_t it(0), itEnd(cl_length(this->_HashTable)); it < itEnd; ++it) {
    cnt += cl_length((this->_HashTable->operator[](it)));
  }
  return cnt;
}

uint HashTable_O::hashTableSize() const {
  return cl_length(this->_HashTable);
}

bool HashTable_O::keyTest(T_sp entryKey, T_sp searchKey) const {
  SUBCLASS_MUST_IMPLEMENT();
}

gc::Fixnum HashTable_O::sxhashKey(T_sp obj, gc::Fixnum bound, bool willAddKey) const {
  _OF();
  SUBCLASS_MUST_IMPLEMENT();
}

List_sp HashTable_O::findAssoc(gc::Fixnum index, T_sp key) const {
  _OF();
  List_sp rib = coerce_to_list((*this->_HashTable)[index]);
  for (auto cur : rib) {
    List_sp pair = oCar(cur);
    if (this->keyTest(oCar(pair), key)) {
      //		LOG(BF("Found match: %s") % cur->__repr__());
      return pair;
    }
  }
  return _Nil<T_O>();
}

#define ARGS_cl_gethash "(key hash-table &optional default_value)"
#define DECL_cl_gethash ""
#define DOCS_cl_gethash "gethash"
T_mv cl_gethash(T_sp key, T_sp hashTable, T_sp default_value) {
  _G();
#ifdef DEBUG_HASH_TABLE
  if (gc::As<HashTable_sp>(hashTable)->_DebugHashTable) {
    string className = "NULL";
    if (key.objectp()) {
      className = key->_instanceClass()->classNameAsString();
    }
    printf("%s:%d DebugHashTable hashTable = %s  className(key) = %s   key = %s\n", __FILE__, __LINE__, _rep_(hashTable).c_str(), className.c_str(), _rep_(key).c_str());
  }
#endif
  HashTable_sp ht = gc::As<HashTable_sp>(hashTable);
  return ht->gethash(key, default_value);
};

List_sp HashTable_O::bucketsFind(T_sp key) const {
  ASSERT(this->_HashTable);
#ifdef DEBUG_HASH_TABLE
  if (this->_DebugHashTable) {
    printf("%s:%d  About to sxhashKey: %s hash table length: %d\n", __FILE__, __LINE__, _rep_(key).c_str(), cl_length(this->_HashTable));
  }
#endif
  gc::Fixnum index = this->sxhashKey(key, cl_length(this->_HashTable), false);
#ifdef DEBUG_HASH_TABLE
  if (this->_DebugHashTable) {
    printf("%s:%d  bucketsFind index = %ld\n", __FILE__, __LINE__, index);
  }
#endif
  List_sp keyValueCons = this->findAssoc(index, key);
  return keyValueCons;
}

List_sp HashTable_O::tableRef(T_sp key) {
  List_sp keyValueCons = this->bucketsFind(key);
  if (keyValueCons.notnilp())
    return keyValueCons;
#ifdef USE_MPS
  // Location dependency test if key is stale
  if (key.objectp()) {
    void *blockAddr = SmartPtrToBasePtr(key);
    if (mps_ld_isstale(const_cast<mps_ld_t>(&(this->_LocationDependencyTracker)), gctools::_global_arena, blockAddr)) {
      keyValueCons = this->rehash(false, key);
    }
  }
#endif
  return keyValueCons;
}

#define ARGS_core_hashTableForceRehash "(ht)"
#define DECL_core_hashTableForceRehash ""
#define DOCS_core_hashTableForceRehash "hashTableForceRehash"
void core_hashTableForceRehash(HashTable_sp ht) {
  ht->rehash(false, _Unbound<T_O>());
}

#define ARGS_HashTable_O_gethash "(key (self hash-table) &optional default_value)"
#define DECL_HashTable_O_gethash ""
#define DOCS_HashTable_O_gethash "See CLHS"
T_mv HashTable_O::gethash(T_sp key, T_sp default_value) {
  LOG(BF("gethash looking for key[%s]") % _rep_(key));
  List_sp keyValuePair = this->tableRef(key);
  LOG(BF("Found keyValueCons")); // % keyValueCons->__repr__() ); INFINITE-LOOP
#ifdef DEBUG_HASH_TABLE
  if (this->_DebugHashTable) {
    DebugHashTable = false; // Turn it off to print
    printf("%s:%d DebugHashTable after tableRef() keyValuePair=%s\n", __FILE__, __LINE__, _rep_(keyValuePair).c_str());
    DebugHashTable = true;
  }
#endif

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

gc::Fixnum HashTable_O::hashIndex(T_sp key) const {
  gc::Fixnum idx = this->sxhashKey(key, cl_length(this->_HashTable), false);
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

#define ARGS_HashTable_O_remhash "(self key)"
#define DECL_HashTable_O_remhash ""
#define DOCS_HashTable_O_remhash "setf into the hash-table"
bool HashTable_O::remhash(T_sp key) {
  _OF();
  List_sp keyValuePair = this->tableRef(key);
  if (keyValuePair.nilp() || oCdr(keyValuePair).unboundp())
    return false;
  keyValuePair.asCons()->setCdr(_Unbound<T_O>());
  this->_HashTableCount--;
  return true;
}

#define ARGS_HashTable_O_hash_table_setf_gethash "(self key value)"
#define DECL_HashTable_O_hash_table_setf_gethash ""
#define DOCS_HashTable_O_hash_table_setf_gethash "setf into the hash-table"
T_sp HashTable_O::hash_table_setf_gethash(T_sp key, T_sp value) {
  _OF();
  //        printf("%s:%d key@%p value@%p\n", __FILE__, __LINE__, key.raw_(), value.raw_() );
  List_sp keyValuePair = this->tableRef(key);
#ifdef DEBUG_HASH_TABLE
  if (this->_DebugHashTable) {
    printf("%s:%d hash_table_setf_gethash hashTable=%s\n", __FILE__, __LINE__, _rep_(this->asSmartPtr()).c_str());
    printf("%s:%d hash_table_setf_gethash key=%s\n", __FILE__, __LINE__, _rep_(key).c_str());
    printf("%s:%d hash_table_setf_gethash keyValuePair = %s\n", __FILE__, __LINE__, _rep_(keyValuePair).c_str());
  }
#endif
  if (keyValuePair.nilp()) {
    gc::Fixnum index = this->sxhashKey(key, cl_length(this->_HashTable), true /*Will add key*/);
    Cons_sp newKeyValue = Cons_O::create(key, value);
    //            printf("%s:%d  Inserted newKeyValue@%p\n", __FILE__, __LINE__, newKeyValue.raw_());
    Cons_sp newEntry = Cons_O::create(newKeyValue, this->_HashTable->operator[](index));
    this->_HashTable->operator[](index) = newEntry;
#ifdef DEBUG_HASH_TABLE
    if (this->_DebugHashTable) {
      printf("%s:%d hash_table_setf_gethash insert at index: %d\n", __FILE__, __LINE__, index);
    }
#endif
    //            this->_HashTableEntryCount++;
    ++(this->_HashTableCount);
  } else if (oCdr(keyValuePair).unboundp()) {
#ifdef DEBUG_HASH_TABLE
    if (this->_DebugHashTable) {
      printf("%s:%d hash_table_setf_gethash overwriting Unbound\n", __FILE__, __LINE__);
    }
#endif
    keyValuePair.asCons()->setCdr(value);
    ++(this->_HashTableCount);
  } else {
#ifdef DEBUG_HASH_TABLE
    if (this->_DebugHashTable) {
      printf("%s:%d hash_table_setf_gethash changing value\n", __FILE__, __LINE__);
    }
#endif
    keyValuePair.asCons()->setCdr(value);
  }
  if (this->_HashTableCount > this->_RehashThreshold * cl_length(this->_HashTable)) {
    LOG(BF("Expanding hash table"));
    this->rehash(true, _Unbound<T_O>());
  }
  return value;
}

List_sp HashTable_O::rehash(bool expandTable, T_sp findKey) {
  _OF();
  //        printf("%s:%d rehash of hash-table@%p\n", __FILE__, __LINE__,  this );
  ASSERTF(!clasp_zerop(this->_RehashSize), BF("RehashSize is zero - it shouldn't be"));
  ASSERTF(cl_length(this->_HashTable) != 0, BF("HashTable is empty in expandHashTable - this shouldn't be"));
  List_sp foundKeyValuePair(_Nil<T_O>());
  uint startCount = this->hashTableCount();
  LOG(BF("At start of expandHashTable current hash table size: %d") % startSize);
  gc::Fixnum newSize = 0;
  if (expandTable) {
    if (af_integerP(this->_RehashSize)) {
      newSize = cl_length(this->_HashTable) + clasp_to_int(gc::As<Integer_sp>(this->_RehashSize));
    } else if (af_floatP(this->_RehashSize)) {
      newSize = cl_length(this->_HashTable) * clasp_to_double(this->_RehashSize);
    }
  } else {
    newSize = cl_length(this->_HashTable);
  }
  VectorObjects_sp oldTable = this->_HashTable;
  newSize = this->resizeEmptyTable(newSize);
  LOG(BF("Resizing table to size: %d") % newSize);
  size_t oldSize = cl_length(oldTable);
  for (size_t it(0), itEnd(oldSize); it < itEnd; ++it) {
    {
      _BLOCK_TRACEF(BF("Re-indexing hash table row index[%d]") % it);
#ifdef DEBUG_ALL
      stringstream sk;
      for (Cons_sp scur = oldTable->operator[](it); scur.notnilp(); scur = cCdr(scur)) {
        sk << scur->ocar()->ocar()->__repr__() << " ";
      }
      LOG(BF("About to re-index hash table row index[%d] keys: %s") % (it - oldTable.begin()) % sk.str());
#endif
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
          gc::Fixnum index = this->sxhashKey(key, newSize, true /* Will add key */);
          LOG(BF("Re-indexing key[%s] to index[%d]") % _rep_(key) % index);
          Cons_sp newCur = Cons_O::create(pair, this->_HashTable->operator[](index));
          this->_HashTable->operator[](index) = newCur;
        }
      }
#if 0
      List_sp next;
      for ( List_sp cur = oldTable->operator[](it); cur.notnilp(); cur = next )
      {
        T_sp tnext = oCdr(cur);
        next = coerce_to_list(tnext);//;cCdr(cur);
        Cons_sp pair = cCar(cur);
        T_sp key = oCar(pair);
        T_sp value = oCdr(pair);
        if ( !value.unboundp() ) {
                        // key/value represent a valid entry in the hash table
                        //
                        // If findKey is not unbound and we haven't already found
                        // the value that it points to.
                        // then while we are rehashing the hash table we are also looking
                        // for the key it points to.
                        // Check if the current key matches findKey and if it does
                        // set foundKeyValuePair so that it will be returned when
                        // the rehash is complete.
          if ( foundKeyValuePair.nilp() && !findKey.unboundp() ) {
            if ( this->keyTest(key,findKey) ) {
              foundKeyValuePair = pair;
            }
          }
          uint index = this->sxhashKey(key,cl_length(this->_HashTable),true /* Will add key */);
          LOG(BF("Re-indexing key[%s] to index[%d]") % _rep_(key) % index );
          Cons_sp newCur = Cons_O::create(pair,this->_HashTable->operator[](index));
          this->_HashTable->operator[](index) = newCur;
        }
      }
#endif
    }
  }
  uint endCount = this->hashTableCount();
  if (startCount != endCount) {
    SIMPLE_ERROR(BF("After rehash the hash-table-count is %d but at start it was %d") % endCount % startCount);
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
    if (cl_consp(key)) {
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
#define ARGS_HashTable_O_hash_table_dump "(&optional (start 0) end)"
#define DECL_HashTable_O_hash_table_dump ""
#define DOCS_HashTable_O_hash_table_dump "Dump the hash-table"
string HashTable_O::hash_table_dump(Fixnum start, T_sp end) const {
  stringstream ss;
#ifndef DUMP_LOW_LEVEL
  ss << "#<" << this->_instanceClass()->classNameAsString() << std::endl;
#endif
  int iend(cl_length(this->_HashTable));
  if (end.notnilp()) {
    iend = clasp_to_fixnum(gc::As<Fixnum_sp>(end));
  }
  if (start < 0 || start >= cl_length(this->_HashTable)) {
    SIMPLE_ERROR(BF("start must be [0,%d)") % cl_length(this->_HashTable));
  }
  if (iend < start || iend >= cl_length(this->_HashTable)) {
    SIMPLE_ERROR(BF("end must be nil or [%d,%d)") % start % cl_length(this->_HashTable));
  }
  for (size_t it(start), itEnd(iend); it < itEnd; ++it) {
    List_sp first = this->_HashTable->operator[](it);
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
  VectorObjects_sp table = this->_HashTable;
  for (size_t it(0), itEnd(cl_length(table)); it < itEnd; ++it) {
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

void HashTable_O::map_while_true(std::function<bool(T_sp, T_sp)> const &fn) {
  //        HASH_TABLE_LOCK();
  VectorObjects_sp table = this->_HashTable;
  for (size_t it(0), itEnd(cl_length(table)); it < itEnd; ++it) {
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
  _OF();
  //        HASH_TABLE_LOCK();
  VectorObjects_sp table = this->_HashTable;
  for (size_t it(0), itEnd(cl_length(table)); it < itEnd; ++it) {
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

int HashTable_O::hashTableNumberOfHashes() const {
  return cl_length(this->_HashTable);
}

List_sp HashTable_O::hashTableAlistAtHash(int hash) const {
  ASSERTF(hash >= 0 && hash < cl_length(this->_HashTable), BF("Illegal hash value[%d] must between [0,%d)") % hash % cl_length(this->_HashTable));
  return this->_HashTable->operator[](hash);
}

string HashTable_O::keysAsString() {
  stringstream ss;
  this->mapHash([&ss, this](T_sp key, T_sp val) {
                ss << _rep_(key) << " ";
  });
  return ss.str();
}

void HashTable_O::exposeCando(::core::Lisp_sp lisp) {
  _G();
  ::core::class_<HashTable_O> ht;
  ht
      //	.initArgs("(self)")
      .def("hash-table-count", &HashTable_O::hashTableCount)
      .def("hash-table-size", &HashTable_O::hashTableSize)
      .def("hash-table-rehash-size", &HashTable_O::hashTableRehashSize)
      .def("hash-table-rehash-threshold", &HashTable_O::hashTableRehashThreshold)
      .def("hash-table-test", &HashTable_O::hashTableTest)
      .def("core:hashIndex", &HashTable_O::hashIndex)
      .def("core:hashTableNumberOfHashes", &HashTable_O::hashTableNumberOfHashes)
      .def("core:hashTableAlistAtHash", &HashTable_O::hashTableAlistAtHash);
  ht
      .def("core:hashTableSetfGethash", &HashTable_O::hash_table_setf_gethash)
      .def("core:hashTableDump", &HashTable_O::hash_table_dump);
  SYMBOL_EXPORT_SC_(ClPkg, make_hash_table);
  ClDefun(make_hash_table);
  SYMBOL_EXPORT_SC_(ClPkg, maphash);
  ClDefun(maphash);
  SYMBOL_EXPORT_SC_(ClPkg, clrhash);
  ClDefun(clrhash);
  SYMBOL_SC_(CorePkg, hash_eql);
  Defun(hash_eql);
  SYMBOL_SC_(CorePkg, hash_equal);
  Defun(hash_equal);
  SYMBOL_SC_(CorePkg, hash_equalp);
  Defun(hash_equalp);
  SYMBOL_EXPORT_SC_(ClPkg, remhash);
  Defun(remhash);
  SYMBOL_EXPORT_SC_(ClPkg, gethash);
  ClDefun(gethash);
  CoreDefun(DebugHashTable);
  CoreDefun(hashTableForceRehash);

  Defun(hashTableEntryDeletedP);
}

void HashTable_O::exposePython(::core::Lisp_sp lisp) {
#ifdef USEBOOSTPYTHON
  PYTHON_CLASS(Pkg(), HashTable, "", "", _LISP)
      //	.initArgs("(self)")
      .def("hash-table-count", &HashTable_O::hashTableCount)
      .def("hash-table-size", &HashTable_O::hashTableSize)
      .def("hash-table-rehash-size", &HashTable_O::hashTableRehashSize)
      .def("hash-table-rehash-threshold", &HashTable_O::hashTableRehashThreshold)
      .def("hash-table-test", &HashTable_O::hashTableTest)
      //	    .def("gethash",&HashTable_O::gethash)
      .def("hash-table-dump", &HashTable_O::hash_table_dump)
      //	    .def_raw("maphash",&HashTable_O::maphash)
      ;
#endif
}

}; /* core */
