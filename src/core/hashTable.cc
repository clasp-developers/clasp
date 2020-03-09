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

#if 0
/**/ #if 1
/**/   #define DEBUG_HASH_TABLE(expr) if (core::_sym_STARdebug_hash_tableSTAR.boundp()&&core::_sym_STARdebug_hash_tableSTAR->boundP()&&core::_sym_STARdebug_hash_tableSTAR->symbolValue().notnilp()) expr
/**/ #else
/**/   #define DEBUG_HASH_TABLE(expr) expr
/**/ #endif
#else
#define DEBUG_HASH_TABLE(expr)
#endif


//#define DEBUG_LEVEL_FULL

#include <limits>
#include <clasp/core/foundation.h>
#include <clasp/core/common.h>
#include <clasp/core/corePackage.h>
#include <clasp/core/environment.h>
#include <clasp/core/multipleValues.h>
#include <clasp/core/symbolTable.h>
#include <clasp/core/mpPackage.h>
#include <clasp/core/lispStream.h>
#include <clasp/core/hashTable.h>
#include <clasp/core/hashTableEq.h>
#include <clasp/core/hashTableEql.h>
#include <clasp/core/hashTableEqual.h>
#include <clasp/core/hashTableEqualp.h>
#include <clasp/gctools/gcFunctions.h>
#include <clasp/core/array.h>
#include <clasp/core/instance.h>
#include <clasp/core/debugger.h>
#include <clasp/core/fileSystem.h>
#include <clasp/core/serialize.h>
#include <clasp/core/evaluator.h>
#include <clasp/core/designators.h>
#include <clasp/core/weakHashTable.h>
#include <clasp/core/wrappers.h>
#ifdef CLASP_THREADS
#include <pthread.h>
#endif
namespace core {


std::atomic<size_t> global_next_hash_table_id;

size_t next_hash_table_id() {
  return global_next_hash_table_id++;
}

CL_DEFUN Vector_sp core__hash_table_pairs(HashTable_sp hash_table)
{
  SimpleVector_sp keyvalues = SimpleVector_O::make(hash_table->_HashTableCount*2);
  size_t idx(0);
  for (size_t it(0), itEnd(hash_table->_Table.size()); it < itEnd; ++it) {
    Cons_O& entry = hash_table->_Table[it];
    if (!entry.ocar().no_keyp()&&!entry.ocar().deletedp()) {
      (*keyvalues)[idx++] = entry.ocar();
      (*keyvalues)[idx++] = entry.cdr();
    }
  }
  return keyvalues;
}
  

void verifyHashTable(bool print, std::ostream& ss, HashTable_O* ht, const char* filename, size_t line, size_t index=0, T_sp key=_Nil<core::T_O>() )
{
  size_t cnt = 0;
  Vector_sp keys = core__make_vector(_lisp->_true(),ht->_HashTableCount+16, true, make_fixnum(0));
  for (size_t it(0), itEnd(ht->_Table.size()); it < itEnd; ++it) {
    Cons_O& entry = ht->_Table[it];
    if (!entry.ocar().no_keyp()&&!entry.ocar().deletedp()) {
        if (print) {
            ss << "Entry["<<it<<"] at " << (void*)&entry << "   key: " << _rep_(entry.ocar()) << " value: " << (entry.cdr()) << "\n";
        }
      keys->vectorPushExtend(entry.ocar());
    }
  }
  gctools::gctools__garbage_collect();
  gctools::gctools__garbage_collect();
  gctools::gctools__garbage_collect();
  for (size_t it(0), itEnd(cl__length(keys)); it<itEnd; ++it ) {
    T_sp key = keys->rowMajorAref(it);
    T_mv lookup = ht->gethash(key,_Nil<core::T_O>());
    if (lookup.second().nilp()) {
      if (print) {
        ss << filename << ":" << line << " Could not find key " << _rep_(key) << "\n";
      } else {
        SIMPLE_ERROR(BF("Could not find key: %s") % _rep_(key) );
      }
    } else {
      if (print) {
        ss << filename << ":" << line << " Found key " << _rep_(key) << "  value: " << _rep_(lookup) << "\n";
      }
    }
  }
  if (!print) {
    if (cl__length(keys)!=ht->_HashTableCount) {
      std::cerr << filename << ":" << line << " Working on index " << index << " - dumping hash-table\n";
      verifyHashTable(true,std::cerr,ht,filename,line);
      std::cerr << "    Added key: " << (void*)key.raw_();
      SIMPLE_ERROR(BF("%s:%d There is a mismatch in _HashTableCount %lu vs calcd %lu\n") % filename % line % ht->_HashTableCount % cnt);
    }
  }
}

#if 0
#define VERIFY_HASH_TABLE(xxx) verifyHashTable(false,std::cerr,xxx,__FILE__,__LINE__)
#define VERIFY_HASH_TABLE_VA(xxx,...) verifyHashTable(false,std::cerr,xxx,__FILE__,__LINE__,__VA_ARGS__)
#else
#define VERIFY_HASH_TABLE(xxx,...)
#define VERIFY_HASH_TABLE_VA(xxx,...)
#endif


#ifdef CLASP_THREADS
struct HashTableReadLock {
  const HashTable_O* _hashTable;
  HashTableReadLock(const HashTable_O* ht) : _hashTable(ht) {
    if (this->_hashTable->_Mutex) {
      this->_hashTable->_Mutex->shared_lock();
    }
  }
  ~HashTableReadLock() {
    if (this->_hashTable->_Mutex) {
      this->_hashTable->_Mutex->shared_unlock();
    }
  }
};
struct HashTableWriteLock {
  const HashTable_O* _hashTable;
  HashTableWriteLock(const HashTable_O* ht,bool upgrade = false) : _hashTable(ht) {
    if (this->_hashTable->_Mutex) {
      this->_hashTable->_Mutex->write_lock(upgrade);
    }
  }
  ~HashTableWriteLock() {
    if (this->_hashTable->_Mutex) {
      this->_hashTable->_Mutex->write_unlock();
    }
  }
};
#endif

#ifdef CLASP_THREADS
#define HT_READ_LOCK(me) HashTableReadLock _zzz(me)
#define HT_WRITE_LOCK(me) HashTableWriteLock _zzz(me)
#define HT_UPGRADE_WRITE_LOCK(me) HashTableWriteLock _zzz(me,true)
#else
#define HT_READ_LOCK(me) 
#define HT_WRITE_LOCK(me) 
#endif


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

#if 0
void HashTable_O::set_thread_safe(bool thread_safe)
{
#ifdef CLASP_THREADS
  if (thread_safe) {
    SimpleBaseString_sp sbs = SimpleBaseString_O::make("HASHTABL");
    this->_Mutex = mp::SharedMutex_O::make_shared_mutex(sbs);
  } else {
    this->_Mutex.reset_();
  }
#endif
}
#endif

CL_LAMBDA(&key (test (function eql)) (size 0) (rehash-size 2.0) (rehash-threshold 0.7) weakness debug thread-safe);
CL_DECLARE();
CL_DOCSTRING("see CLHS");
CL_DEFUN T_sp cl__make_hash_table(T_sp test, Fixnum_sp size, Number_sp rehash_size, Real_sp orehash_threshold, Symbol_sp weakness, T_sp debug, T_sp thread_safe) {
  SYMBOL_EXPORT_SC_(KeywordPkg, key);
  if (weakness.notnilp()) {
    if (weakness == INTERN_(kw, key)) {
      return core__make_weak_key_hash_table(clasp_make_fixnum(size));
    }
    SIMPLE_ERROR(BF("Only :weakness :key (weak-key hash tables) are currently supported"));
  }
  double rehash_threshold = maybeFixRehashThreshold(clasp_to_double(orehash_threshold));
  HashTable_sp table = _Nil<HashTable_O>();
  size_t isize = clasp_to_int(size);
  if (isize==0) isize = 16;
#ifdef DEBUG_REHASH_COUNT
  this->_InitialSize = isize;
#endif
  //	write_bf_stream(BF("%s:%d - make_hash_table - fix me so that I grow by powers of 2\n") % __FILE__ % __LINE__ );
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
  if (thread_safe.notnilp()) {
    table->setupThreadSafeHashTable();
  }
  return table;
}

void HashTable_O::setupThreadSafeHashTable() {
#ifdef CLASP_THREADS
  SimpleBaseString_sp sbsread = SimpleBaseString_O::make("USRHSHR");
  SimpleBaseString_sp sbswrite = SimpleBaseString_O::make("USRHSHW");
  this->_Mutex = mp::SharedMutex_O::make_shared_mutex(sbsread,sbswrite);
#endif
}

CL_LAMBDA(ht);
CL_DECLARE();
CL_DOCSTRING("hash_table_weakness");
CL_DEFUN Symbol_sp core__hash_table_weakness(T_sp ht) {
  if (gc::IsA<WeakKeyHashTable_sp>(ht)) {
    return kw::_sym_key;
  }
  return _Nil<Symbol_O>();
}

HashTable_sp HashTable_O::create(T_sp test) {
  Fixnum_sp size = make_fixnum(16);
  DoubleFloat_sp rehashSize = DoubleFloat_O::create(2.0);
  DoubleFloat_sp rehashThreshold = DoubleFloat_O::create(DEFAULT_REHASH_THRESHOLD);
  HashTable_sp ht = gc::As_unsafe<HashTable_sp>(cl__make_hash_table(test, size, rehashSize, rehashThreshold));
  return ht;
}

HashTable_sp HashTable_O::create_thread_safe(T_sp test, SimpleBaseString_sp readLockName, SimpleBaseString_sp writeLockName) {
  Fixnum_sp size = make_fixnum(16);
  DoubleFloat_sp rehashSize = DoubleFloat_O::create(2.0);
  DoubleFloat_sp rehashThreshold = DoubleFloat_O::create(DEFAULT_REHASH_THRESHOLD);
  HashTable_sp ht = gc::As_unsafe<HashTable_sp>(cl__make_hash_table(test, size, rehashSize, rehashThreshold));
  ht->_Mutex = mp::SharedMutex_O::make_shared_mutex(readLockName,writeLockName);
  return ht;
}

void HashTable_O::maphash(T_sp function_desig) {
    //        printf("%s:%d starting maphash on hash-table@%p\n", __FILE__, __LINE__, hash_table.raw_());
  Function_sp func = coerce::functionDesignator(function_desig);
  gctools::Vec0<Cons_O> tableCopy;
  tableCopy.resize(this->_Table.size());
  {
    HT_READ_LOCK(this);
    for ( size_t i=0; i<this->_Table.size(); ++i ) {
      tableCopy[i] = this->_Table[i];
    }
  }
  for (size_t it(0), itEnd(tableCopy.size()); it < itEnd; ++it) {
    Cons_O& entry = tableCopy[it];
    if (!entry.ocar().no_keyp()&&!entry.ocar().deletedp()) {
      eval::funcall(func,entry.ocar(),entry.cdr());
    }
  }
  //        printf("%s:%d finished maphash on hash-table@%p\n", __FILE__, __LINE__, hash_table.raw_());
}

CL_LAMBDA(function-desig hash-table);
CL_DECLARE();
CL_DOCSTRING("see CLHS");
CL_DEFUN T_sp cl__maphash(T_sp function_desig, HashTableBase_sp hash_table) {
  //        printf("%s:%d starting maphash on hash-table@%p\n", __FILE__, __LINE__, hash_table.raw_());
  Function_sp func = coerce::functionDesignator(function_desig);
  if (hash_table.nilp()) {
    SIMPLE_ERROR(BF("maphash called with nil hash-table"));
  }
  hash_table->maphash(function_desig);
  return _Nil<T_O>();
}

CL_LAMBDA(hash-table);
CL_DECLARE();
CL_DOCSTRING("See CLHS");
CL_DEFUN T_sp cl__clrhash(HashTableBase_sp hash_table) {
  hash_table->clrhash();
  return hash_table;
};

CL_LAMBDA(cons);
CL_DECLARE();
CL_DOCSTRING("hashTableEntryDeletedP");
CL_DEFUN bool core__hash_table_entry_deleted_p(T_sp cons) {
  if (!cons.consp())
    SIMPLE_ERROR(BF("Arg must be a cons"));
  return oCdr(gc::As<Cons_sp>(cons)).no_keyp();
};

CL_LAMBDA(&rest args);
CL_DECLARE();
CL_DOCSTRING("hash_eql generates an eql hash for a list of objects");
CL_DEFUN int core__hash_eql(List_sp args) {
  HashGenerator hg;
  for (auto cur : args) {
    HashTable_O::sxhash_eql(hg, oCar(cur));
    if (!hg.isFilling()) break;
  }
  return hg.rawhash();
};

CL_LAMBDA(&rest args);
CL_DECLARE();
CL_DOCSTRING("hash_equal generates an equal hash for a list of objects");
CL_DEFUN int core__hash_equal(List_sp args) {
  HashGenerator hg;
  for (auto cur : args) {
    HashTable_O::sxhash_equal(hg, oCar(cur));
    if (hg.isFull())
      break;
  }
  return hg.rawhash();
};

CL_LAMBDA(&rest args);
CL_DECLARE();
CL_DOCSTRING("hash_equalp generates an equalp hash for a list of objects");
CL_DEFUN int core__hash_equalp(List_sp args) {
  HashGenerator hg;
  for (auto cur : args) {
    HashTable_O::sxhash_equalp(hg, oCar(cur));
    if (hg.isFull())
      break;
  }
  return hg.rawhash();
};

CL_LAMBDA(key hashtable);
CL_DECLARE();
CL_DOCSTRING("remhash");
CL_DEFUN bool cl__remhash(T_sp key, HashTableBase_sp ht) {
  return ht->remhash(key);
};

T_sp HashTable_O::clrhash() {
  ASSERT(!clasp_zerop(this->_RehashSize));
  this->_HashTableCount = 0;
  T_sp no_key = _NoKey<T_O>();
  this->_Table.resize(0,Cons_O(no_key,no_key));
  this->setup(16, this->_RehashSize, this->_RehashThreshold);
  VERIFY_HASH_TABLE(this);
  return this->asSmartPtr();
}

double maybeFixRehashThreshold(double rt)
{
  if (rt < 0.0 || rt > DEFAULT_REHASH_THRESHOLD) return DEFAULT_REHASH_THRESHOLD;
  return rt;
}
void HashTable_O::setup(uint sz, Number_sp rehashSize, double rehashThreshold) {
  HT_WRITE_LOCK(this);
  sz = this->resizeEmptyTable_no_lock(sz);
  this->_RehashSize = rehashSize;
  ASSERT(!clasp_zerop(this->_RehashSize));
  this->_RehashThreshold = maybeFixRehashThreshold(rehashThreshold);
}

void HashTable_O::sxhash_eq(HashGenerator &hg, T_sp obj) {
  if (obj.generalp()) {
    hg.addAddress((void*)obj.unsafe_general());
    return;
  } else if (obj.consp()) {
    hg.addAddress((void*)obj.unsafe_cons());
    return;
  } else {
    hg.addValue((uintptr_t)obj.raw_());
  }
}

void HashTable_O::sxhash_eq(Hash1Generator &hg, T_sp obj) {
  if (obj.generalp()) {
    hg.addAddress((void*)obj.unsafe_general());
    return;
  } else if (obj.consp()) {
    hg.addAddress((void*)obj.unsafe_cons());
    return;
  } else {
    hg.addValue((uintptr_t)obj.raw_());
  }
}

void HashTable_O::sxhash_eql(HashGenerator &hg, T_sp obj) {
  uintptr_t tag = (uintptr_t)gctools::ptag<core::T_O*>(obj.raw_());
  switch (tag) {
  case gctools::fixnum0_tag:
  case gctools::fixnum1_tag:
    {
      hg.addValue0(obj.unsafe_fixnum());
      return;
    }
  case gctools::single_float_tag:
    {
      hg.addValue0(std::abs(::floor(obj.unsafe_single_float())));
      return;
    }
  case gctools::character_tag:
    {
      hg.addValue0(obj.unsafe_character());
      return;
    }
  case gctools::general_tag:
    {
      if (cl__numberp(obj)) {
        hg.hashObject(obj);
        return;
      }
      hg.addAddress((void*)(obj.unsafe_general()));
      return;
    }
  case gctools::cons_tag:
    {
      hg.addAddress((void*)(obj.unsafe_cons()));
      return;
    }
  default:
      break;
  }
  SIMPLE_ERROR(BF("Illegal object for eql hash %s") % _rep_(obj));
}

void HashTable_O::sxhash_eql(Hash1Generator &hg, T_sp obj) {
  if (obj.fixnump()) {
    if (hg.isFilling())
      hg.addValue(obj.unsafe_fixnum());
    return;
  } else if (obj.single_floatp()) {
    if (hg.isFilling())
      hg.addValue(std::abs(::floor(obj.unsafe_single_float())));
    return;
  } else if (obj.characterp()) {
    if (hg.isFilling())
      hg.addValue(obj.unsafe_character());
    return;
  } else if (obj.generalp()) {
    if ( gc::IsA<Number_sp>(obj)) {
      hg.hashObject(obj);
      return;
    }
    hg.addAddress((void*)(obj.unsafe_general()));
    return;
  } else if (obj.consp()) {
    hg.addAddress((void*)(obj.unsafe_cons()));
    return;
  }
  SIMPLE_ERROR(BF("Illegal object for eql hash %s") % _rep_(obj));
}

void HashTable_O::sxhash_equal(HashGenerator &hg, T_sp obj) {
  if (obj.fixnump()) {
    if (hg.isFilling()) hg.addValue(obj.unsafe_fixnum());
    return;
  } else if (obj.single_floatp()) {
    if (hg.isFilling()) hg.addValue(std::abs(::floor(obj.unsafe_single_float())));
    return;
  } else if (obj.characterp()) {
    if (hg.isFilling()) hg.addValue(obj.unsafe_character());
    return;
  } else if (obj.consp()) {
    Cons_sp cobj = gc::As_unsafe<Cons_sp>(obj);
    if (hg.isFilling()) HashTable_O::sxhash_equal(hg, CONS_CAR(cobj));
    if (hg.isFilling()) HashTable_O::sxhash_equal(hg, CONS_CAR(cobj));
    return;
  } else if (obj.generalp()) {
    if (cl__numberp(obj)) {
      hg.hashObject(obj);
      return;
    } else if (String_sp str_obj = obj.asOrNull<String_O>()) {
      if (hg.isFilling()) str_obj->sxhash_(hg);
      return;
    } else if (BitVector_sp bv_obj = obj.asOrNull<BitVector_O>()) {
      if (hg.isFilling()) bv_obj->sxhash_(hg);
      return;
    }
    General_sp gobj = gc::As_unsafe<General_sp>(obj);
    gobj->sxhash_equal(hg);
    return;
  }
  SIMPLE_ERROR(BF("You cannot EQUAL hash on %s") % _rep_(obj));
}

void HashTable_O::sxhash_equalp(HashGenerator &hg, T_sp obj) {
  if (obj.fixnump()) {
    if (hg.isFilling())
      hg.addValue(obj.unsafe_fixnum());
    return;
  } else if (obj.single_floatp()) {
    if (hg.isFilling())
      hg.addValue(std::abs(::floor(obj.unsafe_single_float())));
    return;
  } else if (obj.characterp()) {
    if (hg.isFilling()) 
      hg.addValue(claspCharacter_upcase(clasp_as_claspCharacter(gc::As<Character_sp>(obj))));
    return;
  } else if (obj.consp()) {
    Cons_sp cobj = gc::As_unsafe<Cons_sp>(obj);
    if (hg.isFilling())
      HashTable_O::sxhash_equalp(hg, CONS_CAR(cobj));
    if (hg.isFilling())
      HashTable_O::sxhash_equalp(hg, CONS_CDR(cobj));
    return;
  } else if (obj.generalp()) {
    if (cl__numberp(obj)) {
      if (hg.isFilling()) hg.hashObject(obj);
      return;
    } else if (cl__stringp(obj)) {
      SimpleString_sp upstr = cl__string_upcase(obj);
      hg.hashObject(upstr);
      return;
    }
    General_sp gobj = gc::As_unsafe<General_sp>(obj);
    gobj->sxhash_equalp(hg);
    return;
  }
  SIMPLE_ERROR(BF("You cannot EQUALP hash on %s") % _rep_(obj));
}

bool HashTable_O::equalp(T_sp other) const {
  if (this == &(*other)) return true;
  if (!other.generalp()) return false;
  if (!gc::IsA<HashTable_sp>(other)) return false;
  HashTable_sp hto = gc::As_unsafe<HashTable_sp>(other);
  if (this->hashTableTest() != hto->hashTableTest()) return false;
  if (this->hashTableCount() != hto->hashTableCount()) return false;
  return this->map_while_true( [&hto] (T_sp key, T_sp val)->bool const {
                                 T_sp other_value = hto->gethash(key);
                                 if (!cl__equalp(val,other_value)) {
                                   return false;
                                 }
      // return true to continue looping the hash-table;
                                 return true;
                               }
    );
}

List_sp HashTable_O::keysAsCons() {
  HT_READ_LOCK(this);
  List_sp res = _Nil<T_O>();
  this->mapHash([&res](T_sp key, T_sp val) {
                  res = Cons_O::create(key,res);
                });
  return res;
}

void HashTable_O::fields(Record_sp node) {
  VERIFY_HASH_TABLE(this);
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
    IMPLEMENT_MEF("Add support to patch hash tables");
  } break;
  }
  VERIFY_HASH_TABLE(this);
}

uint HashTable_O::resizeEmptyTable_no_lock(size_t sz) {
  if (sz < 16) sz = 16;
  T_sp no_key = _NoKey<T_O>();
  this->_HashTableCount = 0;
  this->_Table.resize(sz,Cons_O(no_key,no_key));
#ifdef USE_MPS
  mps_ld_reset(const_cast<mps_ld_t>(&(this->_LocationDependency)), global_arena);
#endif
  return sz;
}

CL_LAMBDA(arg);
CL_DECLARE();
CL_DOCSTRING("hash-table-count");
CL_DEFUN uint cl__hash_table_count(HashTableBase_sp ht) {
  return ht->hashTableCount();
}

size_t HashTable_O::hashTableCount() const {
  HT_READ_LOCK(this);
  return this->_HashTableCount;
}

uint HashTable_O::calculateHashTableCount() const {
  HT_READ_LOCK(this);
  uint cnt = 0;
  for (size_t it(0), itEnd(this->_Table.size()); it < itEnd; ++it) {
    const Cons_O& entry = this->_Table[it];
    if (!entry.ocar().no_keyp()&&!entry.ocar().deletedp()) ++cnt;
  }
  return cnt;
}

CL_LAMBDA(arg);
CL_DECLARE();
CL_DOCSTRING("hash-table-size");
CL_DEFUN uint cl__hash_table_size(HashTableBase_sp ht) {
  return ht->hashTableSize();
}

size_t HashTable_O::hashTableSize() const {
  HT_READ_LOCK(this);
  return this->_Table.size();
}

bool HashTable_O::keyTest(T_sp entryKey, T_sp searchKey) const {
  SUBCLASS_MUST_IMPLEMENT();
}

gc::Fixnum HashTable_O::sxhashKey(T_sp obj, gc::Fixnum bound, HashGenerator& hg) const {
  SUBCLASS_MUST_IMPLEMENT();
}

CL_LAMBDA(key hash-table &optional default-value);
CL_DOCSTRING("gethash");
CL_DEFUN T_mv cl__gethash(T_sp key, HashTableBase_sp hashTable, T_sp default_value) {
  return hashTable->gethash(key, default_value);
};

CL_DOCSTRING("gethash3");
CL_DEFUN T_mv core__gethash3(T_sp key, T_sp hashTable, T_sp default_value) {
  HashTable_sp ht = gc::As_unsafe<HashTable_sp>(hashTable);
  return ht->gethash(key, default_value);
};

List_sp HashTable_O::tableRef_no_read_lock(T_sp key, bool under_write_lock, cl_index index, HashGenerator& hg) {
    DEBUG_HASH_TABLE({core::write_bf_stream(BF("%s:%d key = %s  index = %ld\n") % __FILE__ % __LINE__ % _rep_(key) % index , T_sp());});
  VERIFY_HASH_TABLE(this);
  for (size_t cur = index, curEnd(this->_Table.size()); cur<curEnd; ++cur ) {
    Cons_O& entry = this->_Table[cur];
    if (entry.ocar().no_keyp()) goto NOT_FOUND;
    if (!entry.ocar().deletedp()) {
      DEBUG_HASH_TABLE({core::write_bf_stream(BF("%s:%d search-end !deletedp index = %ld\n") % __FILE__ % __LINE__ % cur , T_sp());});
      if (this->keyTest(entry.ocar(), key)) {
        DEBUG_HASH_TABLE({core::write_bf_stream(BF("%s:%d search-end found key index = %ld entry.ocar()->%p\n .... %s\n  key->%p\n .... %s\n") % __FILE__ % __LINE__ % cur % (void*)entry.ocar().raw_() % dbg_safe_repr((uintptr_t)(void*)entry.ocar().raw_()).c_str() % (void*)key.raw_() % dbg_safe_repr((uintptr_t)(void*)key.raw_()).c_str() , T_sp());});
        
        return gc::smart_ptr<Cons_O>((Cons_O*)&entry);
      }
    }
  }
  for (size_t cur = 0, curEnd(index); cur<curEnd; ++cur ) {
    Cons_O& entry = this->_Table[cur];
    if (entry.ocar().no_keyp()) goto NOT_FOUND;
    if (!entry.ocar().deletedp()) {
      DEBUG_HASH_TABLE({core::write_bf_stream(BF("%s:%d search-begin !deletedp index = %ld\n") % __FILE__ % __LINE__ % cur , T_sp());});
      if (this->keyTest(entry.ocar(), key)) {
        DEBUG_HASH_TABLE({core::write_bf_stream(BF("%s:%d search-begin found key index = %ld\n") % __FILE__ % __LINE__ % cur , T_sp());});
        return gc::smart_ptr<Cons_O>((Cons_O*)&entry);
      }
    }
  }
 NOT_FOUND:
  DEBUG_HASH_TABLE({core::write_bf_stream(BF("%s:%d key not found\n") % __FILE__ % __LINE__, T_sp());});
#if defined(USE_MPS)
  if (key.objectp()) {
    if (hg.isstale(&this->_LocationDependency)) {
      DEBUG_HASH_TABLE({core::write_bf_stream(BF("%s:%d  mps_ld_isstale->TRUE\n") % __FILE__ % __LINE__, T_sp());});
      if (under_write_lock) {
        return this->rehash_no_lock(false /*expandTable*/, key);
      } else {
        return this->rehash_upgrade_write_lock(false /*expandTable*/, key);
      }
    } else {
      DEBUG_HASH_TABLE({core::write_bf_stream(BF("%s:%d  mps_ld_isstale->FALSE\n") % __FILE__ % __LINE__, T_sp());});
        // mps_ld_isstale returned false - so we haven't seen this block before - proceed to fail to find the key
    }
  }
#endif
  VERIFY_HASH_TABLE(this);
  return _Nil<T_O>();
}

CL_LAMBDA(ht);
CL_DECLARE();
CL_DOCSTRING("hashTableForceRehash");
CL_DEFUN void core__hash_table_force_rehash(HashTable_sp ht) {
  HT_WRITE_LOCK(&*ht);
  ht->rehash_no_lock(false, _NoKey<T_O>());
}

T_mv HashTable_O::gethash(T_sp key, T_sp default_value) {
  LOG(BF("gethash looking for key[%s]") % _rep_(key));
  HT_READ_LOCK(this);
  VERIFY_HASH_TABLE(this);
  HashGenerator hg;
  cl_index index = this->sxhashKey(key, this->_Table.size(), hg );
  List_sp keyValuePair = this->tableRef_no_read_lock(key, false /*under_write_lock*/, index, hg);
  LOG(BF("Found keyValueCons")); // % keyValueCons->__repr__() ); INFINITE-LOOP
  if (keyValuePair.consp()) {
    T_sp value = CONS_CDR(keyValuePair);
    if (value.no_keyp()) {
      LOG(BF("valueOrUnbound is unbound - returning default"));
      return (Values(default_value, _Nil<T_O>()));
    }
    LOG(BF("Found assoc - returning")); // : %s") % res->__repr__() );  INFINITE-LOOP
    return Values(value, _lisp->_true());
  }      
  return Values(default_value, _Nil<T_O>());
}

CL_LISPIFY_NAME("core:hashIndex");
CL_DEFMETHOD gc::Fixnum HashTable_O::hashIndex(T_sp key) const {
  HashGenerator hg;
  gc::Fixnum idx = this->sxhashKey(key, this->_Table.size(), hg );
  return idx;
}

List_sp HashTable_O::find(T_sp key) {
  HT_READ_LOCK(this);
  HashGenerator hg;
  cl_index index = this->sxhashKey(key, this->_Table.size(), hg );
  List_sp keyValue = this->tableRef_no_read_lock(key, false /*under_write_lock*/, index, hg);
  if (!keyValue.consp()) return keyValue;
  if (CONS_CDR(keyValue).no_keyp()) return _Nil<T_O>();
  return keyValue;
}

bool HashTable_O::contains(T_sp key) {
  HT_READ_LOCK(this);
  List_sp keyValue = this->find(key);
  return keyValue.consp();
}

bool HashTable_O::remhash(T_sp key) {
  HT_WRITE_LOCK(this);
  HashGenerator hg;
  cl_index index = this->sxhashKey(key, this->_Table.size(), hg );
  List_sp keyValuePair = this->tableRef_no_read_lock( key, true /*under_write_lock*/, index, hg );
  if (keyValuePair.consp()) {
    Cons_sp pair = gc::As_unsafe<Cons_sp>(keyValuePair);
    pair->rplaca(_Deleted<T_O>());
    this->_HashTableCount--;
    VERIFY_HASH_TABLE(this);
    return true;
  }
  VERIFY_HASH_TABLE(this);
  return false;
}

T_sp HashTable_O::setf_gethash_no_write_lock(T_sp key, T_sp value)
{
    DEBUG_HASH_TABLE({core::write_bf_stream(BF("%s:%d:%s   key->%s  value->%s\n")  % __FILE__ % __LINE__ % __FUNCTION__ % _rep_(key) % _rep_(value), T_sp());});
  if (key.no_keyp()) {
    SIMPLE_ERROR(BF("Do not use %s as a key!!") % _rep_(key));
  }
  HashGenerator hg;
  cl_index index = this->sxhashKey(key, this->_Table.size(), hg );
  List_sp keyValuePair = this->tableRef_no_read_lock( key, true /*under_write_lock*/, index, hg);
  if (keyValuePair.consp()) {
    Cons_sp pair = gc::As_unsafe<Cons_sp>(keyValuePair);
    // rewrite value
    pair->rplacd(value);
    DEBUG_HASH_TABLE({core::write_bf_stream(BF("%s:%d Found key/value pair: %s\n") % __FILE__ % __LINE__ % _rep_(keyValuePair), T_sp());});
    DEBUG_HASH_TABLE({core::write_bf_stream(BF("%s:%d  Did rplacd value: %s to cons at %p\n") % __FILE__ % __LINE__ % _rep_(value) % &*pair, T_sp());});
    DEBUG_HASH_TABLE({core::write_bf_stream(BF("%s:%d  After rplacd value: %s\n") % __FILE__ % __LINE__ % _rep_(pair->cdr()), T_sp());});
    VERIFY_HASH_TABLE(this);
    return value;
  }
  // not found
#ifdef USE_MPS  
  // DO NOT! I repeat DO NOT comment out the following line because you see we calculate index above
  // The one below has "true /*will-add-key*/ - this means it will add to the MPS location dependency
  // object - this is essential if this is to work with MPS
  DEBUG_HASH_TABLE({core::write_bf_stream(BF("%s:%d setf_gethash_no_write_lock getting index for will-add-key ht->is_eq=%d\n") % __FILE__ % __LINE__ % this->is_eq_hashtable(), T_sp());});
  hg.addAddressesToLocationDependency(&this->_LocationDependency);
#endif
  DEBUG_HASH_TABLE({core::write_bf_stream(BF("%s:%d Looking for empty slot index = %ld\n")  % __FILE__ % __LINE__ % index, T_sp());});
  Cons_O* entryP = nullptr;
  entryP = &this->_Table[index];
  size_t cur;
  size_t curEnd = this->_Table.size();
  for (cur = index; cur<curEnd; ++cur, ++entryP ) {
    if (entryP->ocar().no_keyp()||entryP->ocar().deletedp()) goto ADD_KEY_VALUE;
  }
  entryP = &this->_Table[0]; // wrap around
  for (cur = 0; cur<index; ++cur, ++entryP ) {
    if (entryP->ocar().no_keyp()||entryP->ocar().deletedp()) goto ADD_KEY_VALUE;
  }
  goto NO_ROOM;
 ADD_KEY_VALUE:
  entryP->rplaca(key);
  entryP->rplacd(value);
  this->_HashTableCount++;
  DEBUG_HASH_TABLE({core::write_bf_stream(BF("%s:%d Found empty slot at index = %ld\n")  % __FILE__ % __LINE__ % cur , T_sp());});
  VERIFY_HASH_TABLE_VA(this,cur,key);
  if (this->_HashTableCount > this->_RehashThreshold * this->_Table.size()) {
    LOG(BF("Expanding hash table"));
    this->rehash_no_lock(true, _NoKey<T_O>());
    VERIFY_HASH_TABLE(this);
  }
  return value;
 NO_ROOM:
  // ---------
  // There was no room in the Table!!!!!
  // This should never happen!  There should always be room.
  // So print some stuff and then rehash and expand the table and keep going
  //    If this happens change the code that increases the size of the table when the rehash trigger is hit
  //
  printf("%s:%d There is absolutely no room in the hash-table _RehashThreshold = %lf - _HashTableCount -> %lu size -> %lu increasing size\n", __FILE__, __LINE__, this->_RehashThreshold, this->_HashTableCount, this->_Table.size());
  verifyHashTable(true,std::cerr,this,__FILE__, __LINE__);
  printf("%s:%d ---- done verify\n", __FILE__, __LINE__ );
  this->rehash_no_lock(true, _NoKey<T_O>());
  VERIFY_HASH_TABLE_VA(this,cur);
  return this->setf_gethash_no_write_lock(key,value);
  // ------------
  // Here we add the key
}


T_sp HashTable_O::hash_table_setf_gethash(T_sp key, T_sp value) {
  LOG(BF("About to hash_table_setf_gethash for %s@%p -> %s@%p\n") % _safe_rep_(key) % (void*)key.raw_() % _safe_rep_(value) % (void*)value.raw_());
  HashTableWriteLock _guard(this);
  return this->setf_gethash_no_write_lock(key, value);
}

CL_LISPIFY_NAME("core:hash-table-setf-gethash");
CL_DEFUN T_sp core__hash_table_setf_gethash(HashTableBase_sp hash_table, T_sp key, T_sp value) {
  return hash_table->hash_table_setf_gethash(key,value);
}


List_sp HashTable_O::rehash_no_lock(bool expandTable, T_sp findKey) {
  //        printf("%s:%d rehash of hash-table@%p\n", __FILE__, __LINE__,  this );
  DEBUG_HASH_TABLE({core::write_bf_stream(BF("%s:%d rehash_no_lock\n") % __FILE__ % __LINE__ , T_sp());});
  ASSERTF(!clasp_zerop(this->_RehashSize), BF("RehashSize is zero - it shouldn't be"));
  ASSERTF(this->_Table.size() != 0, BF("HashTable is empty in expandHashTable - this shouldn't be"));
  List_sp foundKeyValuePair(_Nil<T_O>());
  LOG(BF("At start of expandHashTable current hash table size: %d") % this->_Table.size());
  gc::Fixnum curSize = this->_Table.size();
  gc::Fixnum newSize = 0;
  if (expandTable) {
    if (cl__integerp(this->_RehashSize)) {
      newSize = curSize + clasp_to_int(gc::As<Integer_sp>(this->_RehashSize));
    } else if (cl__floatp(this->_RehashSize)) {
      newSize = curSize * clasp_to_double(this->_RehashSize);
    }
  } else {
    newSize = curSize;
  }
  gc::Vec0<Cons_O> oldTable;
  oldTable.swap(this->_Table);
  size_t oldHashTableCount = this->_HashTableCount;
  newSize = this->resizeEmptyTable_no_lock(newSize);
  LOG(BF("Resizing table to size: %d") % newSize);
  size_t oldSize = oldTable.size();
  for (size_t it(0), itEnd(oldSize); it < itEnd; ++it) {
    Cons_O& entry = oldTable[it];
    T_sp key = entry.ocar();
    T_sp value = entry.cdr();
    if (!key.no_keyp()&&!key.deletedp()) {
          // key/value represent a valid entry in the hash table
          //
          // If findKey is not no_key and we haven't already found
          // the value that it points to.
          // then while we are rehashing the hash table we are also looking
          // for the key it points to.
          // Check if the current key matches findKey and if it does
          // set foundKeyValuePair so that it will be returned when
          // the rehash is complete.
      if (foundKeyValuePair.nilp() && !findKey.no_keyp()) {
        if (this->keyTest(key, findKey)) {
          foundKeyValuePair = gc::smart_ptr<Cons_O>((Cons_O*)&entry);
        }
      }
      this->setf_gethash_no_write_lock(key,value);
    }
  }
#ifdef DEBUG_REHASH_COUNT
  this->_RehashCount++;
  MONITOR(BF("Hash-table rehash id %lu initial-size %lu rehash-number %lu rehash-size %lu oldHashTableCount %lu _HashTableCount %lu\n")
          % this->_HashTableId
          % this->_InitialSize
          % this->_RehashCount
          % newSize
          % oldHashTableCount
          % this->_HashTableCount);
#endif
  VERIFY_HASH_TABLE(this);
  //
  // The following lookup is important for (setf (gethash key ht) val) when using MPS
  // If the lookup of a reference for the key failed because of a stale pointer
  // then that triggers a rehash and rehash searches for the key/value
  // in the OLD table as it rehashes.  So we can't return that reference
  // because setf will then write into the OLD table!  So below
  // we lookup the reference again with tableRef_no_read_lock because
  // it is guaranteed to return a reference to the current table of the hash-table.
  if (foundKeyValuePair.consp()) {
      // Return the foundKeyValuePair in the latest table
      Cons_sp pair = gc::As_unsafe<Cons_sp>(foundKeyValuePair);
      T_sp key = CONS_CAR(pair);
      HashGenerator hg;
      cl_index index = this->sxhashKey(key, this->_Table.size(), hg );
      foundKeyValuePair = this->tableRef_no_read_lock(CONS_CAR(pair),true,index,hg);
  }
  DEBUG_HASH_TABLE({core::write_bf_stream(BF("%s:%d:%s  Returning foundKeyValuePair: %s at %p \n") % __FILE__ % __LINE__ % __FUNCTION__ % _rep_(foundKeyValuePair) % &*foundKeyValuePair);});
  return foundKeyValuePair;
}

List_sp HashTable_O::rehash_upgrade_write_lock(bool expandTable, T_sp findKey) {
  if (this->_Mutex) {
  tryAgain:
    if (this->_Mutex->write_try_lock(true /*upgrade*/)) {
      List_sp result = this->rehash_no_lock(expandTable,findKey);
        // Releasing the read lock will be done by the caller using RAII
      this->_Mutex->write_unlock( false /*releaseReadLock*/);
      return result;
    }
#ifdef _TARGET_OS_DARWIN
    pthread_yield_np();
#else
    pthread_yield();
#endif
    goto tryAgain;
  } else {
    return this->rehash_no_lock(expandTable,findKey);
  }
}

  string HashTable_O::__repr__() const {
    stringstream ss;
    ss << "#<" << this->_instanceClass()->_classNameAsString() << " :HashTableCount " << this->_HashTableCount;
    ss << " :calculated-entries " << this->calculateHashTableCount();
    ss << " :size " << this->_Table.size();
    ss << " @" << (void *)(this) << ">";
    return ss.str();
  }

#define DUMP_LOW_LEVEL 1

  void dump_one_entry(HashTable_sp ht, size_t it, stringstream &ss, Cons_O entry) {
    T_sp key = entry.ocar();
    T_sp value = entry.cdr();
#ifdef DUMP_LOW_LEVEL
    ss << "     ( ";
    size_t hi = ht->hashIndex(key);
    if (hi != it)
      ss << "!!!out-of-place-bucket!!! hi=" << hi;
    ss << " hashIndex(key)=" << ht->hashIndex(key) << " ";
    if ((key).consp()) {
      List_sp ckey = key;
      ss << "(cons " << oCar(ckey).raw_() << " . " << oCdr(ckey).raw_() << ")@" << (void*)ckey.raw_();
    } else {
      ss << key.raw_();
    }
    ss << ", " << value.raw_() << ")" << " " << std::endl;
#else
    ss << "     " << _rep_(entry.ocar()) << " " << _rep_(entry.cdr()) << std::endl;
#endif
  };


CL_DEFMETHOD List_sp HashTable_O::hash_table_bucket(size_t index)
{
  Cons_O& entry = this->_Table[index];
  if (!entry.ocar().no_keyp()&&!entry.ocar().deletedp()) {
    T_sp result = gctools::smart_ptr<Cons_O>((Cons_O*)&entry);
    return result;
  }
  return _Nil<T_O>();
}


CL_DEFMETHOD T_sp HashTable_O::hash_table_average_search_length()
{
  HT_READ_LOCK(this);
  gc::Fixnum iend(this->_Table.size());
  double sum = 0.0;
  gc::Fixnum count = 0;
  for (gc::Fixnum it(0), itEnd(iend); it < itEnd; ++it) {
    const Cons_O& entry = this->_Table[it];
    if (!(entry.ocar().no_keyp()||entry.ocar().deletedp())) {
      HashGenerator hg;
      gc::Fixnum index = this->sxhashKey(entry.ocar(), this->_Table.size(), hg );
      gc::Fixnum delta;
      if (index > it) {
        delta = (it+iend)-index;
      } else {
        delta = (it-index);
      }
//      printf("%s:%d  index = %lld  it = %lld  delta=%lld\n", __FILE__, __LINE__, index, it, delta );
      sum = sum + delta;
      count++;
    }
  }
  if (count>0) {
    return core::clasp_make_double_float(sum / count);
  }
  return _Nil<T_O>();
}  

CL_DEFMETHOD string HashTable_O::hash_table_dump() {
  stringstream ss;
  HT_READ_LOCK(this);
  verifyHashTable(true,ss,this,__FILE__,__LINE__);
  return ss.str();
}

  void HashTable_O::mapHash(std::function<void(T_sp, T_sp)> const &fn) {
    gctools::Vec0<Cons_O> tableCopy;
    tableCopy.resize(this->_Table.size());
    {
      HT_READ_LOCK(this);
      for ( size_t i=0; i<this->_Table.size(); ++i ) {
        tableCopy[i] = this->_Table[i];
      }
    }
    for (size_t it(0), itEnd(tableCopy.size()); it < itEnd; ++it) {
      Cons_O& entry = tableCopy[it];
      T_sp key = entry.ocar();
      T_sp value = entry.cdr();
      if (!entry.ocar().no_keyp()&&!entry.ocar().deletedp()) {
        fn(key, value);
      }
    }
  }

  bool HashTable_O::map_while_true(std::function<bool(T_sp, T_sp)> const &fn) const {
  //        HASH_TABLE_LOCK();
    gctools::Vec0<Cons_O> tableCopy;
    tableCopy.resize(this->_Table.size());
    {
      HT_READ_LOCK(this);
      for ( size_t i=0; i<this->_Table.size(); ++i ) {
        tableCopy[i] = this->_Table[i];
      }
    }
    for (size_t it(0), itEnd(tableCopy.size()); it < itEnd; ++it) {
      const Cons_O& entry = tableCopy[it];
      T_sp key = entry.ocar();
      T_sp value = entry.cdr();
      if (!entry.ocar().no_keyp()&&!entry.ocar().deletedp()) {
        bool cont = fn(key, value);
        if (!cont)
          return false;
      }
    }
    return true;
  }

  void HashTable_O::lowLevelMapHash(KeyValueMapper *mapper) const {
    gctools::Vec0<Cons_O> tableCopy;
    tableCopy.resize(this->_Table.size());
    {
      HT_READ_LOCK(this);
      for ( size_t i=0; i<this->_Table.size(); ++i ) {
        tableCopy[i] = this->_Table[i];
      }
    }
    for (size_t it(0), itEnd(tableCopy.size()); it < itEnd; ++it) {
      const Cons_O& entry = tableCopy[it];
      T_sp key = entry.ocar();
      T_sp value = entry.cdr();
      if (!entry.ocar().no_keyp()&&!entry.ocar().deletedp()) {
        if (!mapper->mapKeyValue(key, value))
          goto DONE;
      }
    }
  DONE:
    return;
  }

  CL_LISPIFY_NAME("core:hashTableNumberOfHashes");
  CL_DEFMETHOD int HashTable_O::hashTableNumberOfHashes() const {
    HT_READ_LOCK(this);
    return this->_Table.size();
  }

  string HashTable_O::keysAsString() {
    stringstream ss;
    this->mapHash([&ss, this](T_sp key, T_sp val) {
        ss << _rep_(key) << " ";
      });
    return ss.str();
  }

Number_sp HashTable_O::rehash_size() {
  HT_READ_LOCK(this);
  return this->_RehashSize;
}

double HashTable_O::rehash_threshold() {
  HT_READ_LOCK(this);
  return this->_RehashThreshold;
}

T_sp HashTable_O::hash_table_test() {
  return this->hashTableTest();
}

CL_LAMBDA(arg);
CL_DECLARE();
CL_DOCSTRING("hash-table-rehash-size");
CL_DEFUN Number_sp cl__hash_table_rehash_size(HashTableBase_sp ht) {
  return ht->rehash_size();
};



CL_LAMBDA(arg);
CL_DECLARE();
CL_DOCSTRING("hash-table-rehash-threshold");
CL_DEFUN double cl__hash_table_rehash_threshold(HashTableBase_sp ht) {
  return ht->rehash_threshold();
};

CL_LAMBDA(arg);
CL_DECLARE();
CL_DOCSTRING("hash-table-test");
CL_DEFUN T_sp cl__hash_table_test(HashTableBase_sp ht) {
  return ht->hash_table_test();
};




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
