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
/**/ #if 0
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
#include <clasp/core/hashTableCustom.h>
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


void verifyHashTable(bool print, std::ostream& ss, HashTable_O* ht, const char* filename, size_t line, size_t index=0, T_sp key=nil<core::T_O>() )
{
  size_t cnt = 0;
  Vector_sp keys = core__make_vector(_lisp->_true(),ht->_HashTableCount+16, true, make_fixnum(0));
  for (size_t it(0), itEnd(ht->_Table.size()); it < itEnd; ++it) {
    KeyValuePair& entry = ht->_Table[it];
    if (!entry._Key.no_keyp()&&!entry._Key.deletedp()) {
        if (print) {
            ss << "Entry["<<it<<"] at " << (void*)&entry << "   key: " << _rep_(entry._Key) << " value: " << (entry._Value) << "\n";
        }
      keys->vectorPushExtend(entry._Key);
    }
  }
  gctools::gctools__garbage_collect();
  gctools::gctools__garbage_collect();
  gctools::gctools__garbage_collect();
  for (size_t it(0), itEnd(cl__length(keys)); it<itEnd; ++it ) {
    T_sp key = keys->rowMajorAref(it);
    T_mv lookup = ht->gethash(key,nil<core::T_O>());
    if (lookup.second().nilp()) {
      if (print) {
        ss << filename << ":" << line << " Could not find key " << _rep_(key) << "\n";
      } else {
        SIMPLE_ERROR(("Could not find key: %s") , _rep_(key) );
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
      SIMPLE_ERROR(("%s:%d There is a mismatch in _HashTableCount %lu vs calcd %lu\n") , filename , line , ht->_HashTableCount , cnt);
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


DOCGROUP(clasp)
CL_DEFUN Vector_sp core__hash_table_pairs(HashTableBase_sp hash_table_base)
{
  if (gc::IsA<HashTable_sp>(hash_table_base)) {
    HashTable_sp hash_table = gc::As_unsafe<HashTable_sp>(hash_table_base);
    HT_READ_LOCK(&*hash_table);
    SimpleVector_sp keyvalues = SimpleVector_O::make(hash_table->_HashTableCount*2);
    size_t idx(0);
    for (size_t it(0), itEnd(hash_table->_Table.size()); it < itEnd; ++it) {
      KeyValuePair& entry = hash_table->_Table[it];
      if (!entry._Key.no_keyp()&&!entry._Key.deletedp()) {
        (*keyvalues)[idx++] = entry._Key;
        (*keyvalues)[idx++] = entry._Value;
      }
    }
    return keyvalues;
  } else if (gc::IsA<WeakKeyHashTable_sp>(hash_table_base)) {
    WeakKeyHashTable_sp hash_table = gc::As_unsafe<WeakKeyHashTable_sp>(hash_table_base);
    gctools::WeakKeyHashTable& wkht = hash_table->_HashTable;
    return gctools::weak_key_hash_table_pairs(wkht);
  }
  TYPE_ERROR(hash_table_base, Cons_O::createList(cl::_sym_or,cl::_sym_HashTable_O,core::_sym_WeakKeyHashTable_O));
}
  


#if defined(USE_MPS)
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
# define HASH_TABLE_LOCK() HashTableLocker zzzzHashTableLocker;
#elif defined(USE_BOEHM) || defined(USE_MMTK)
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

CL_LAMBDA(&key (test (function eql)) (size 0) (rehash-size 2.0) (rehash-threshold 0.7) weakness debug thread-safe hash-function)
CL_DECLARE();
CL_DOCSTRING(R"dx(See CLHS for most behavior. As an extension, Clasp allows a TEST other than the four standard ones to be passed. In this case it must be a designator for a function of two arguments, and a :HASH-FUNCTION must be passed as well; this should be a designator of a function analogous to SXHASH, i.e. it accepts one argument, returns a nonnegative fixnum, and (TEST x y) implies (= (HASH x) (HASH y)).)dx")
DOCGROUP(clasp)
CL_DEFUN T_sp cl__make_hash_table(T_sp test, Fixnum_sp size,
                                  Number_sp rehash_size,
                                  Real_sp orehash_threshold,
                                  Symbol_sp weakness, T_sp debug,
                                  T_sp thread_safe, T_sp hashf) {
  SYMBOL_EXPORT_SC_(KeywordPkg, key);
  if (weakness.notnilp()) {
    if (weakness == INTERN_(kw, key)) {
      if (test == cl::_sym_eq || test == cl::_sym_eq->symbolFunction()) {
        return core__make_weak_key_hash_table(size);
      } else {
        SIMPLE_ERROR(("Weak hash tables non-EQ tests are not yet supported"));
      }
    }
    SIMPLE_ERROR(("Only :weakness :key (weak-key hash tables) are currently supported"));
  }
  double rehash_threshold = maybeFixRehashThreshold(clasp_to_double(orehash_threshold));
  HashTable_sp table = nil<HashTable_O>();
  size_t isize = clasp_to_int(size);
  if (isize==0) isize = 16;
#ifdef DEBUG_REHASH_COUNT
  this->_InitialSize = isize;
#endif
  //	write_bf_stream(fmt::sprintf("%s:%d - make_hash_table - fix me so that I grow by powers of 2\n" , __FILE__ , __LINE__ ));
  if (test == cl::_sym_eq || test == cl::_sym_eq->symbolFunction()) {
    table = HashTableEq_O::create(isize, rehash_size, rehash_threshold);
  } else if (test == cl::_sym_eql || test == cl::_sym_eql->symbolFunction()) {
    table = HashTableEql_O::create(isize, rehash_size, rehash_threshold);
  } else if (test == cl::_sym_equal || test == cl::_sym_equal->symbolFunction()) {
    table = HashTableEqual_O::create(isize, rehash_size, rehash_threshold);
  } else if (test == cl::_sym_equalp || test == cl::_sym_equalp->symbolFunction()) {
    table = HashTableEqualp_O::create(isize, rehash_size, rehash_threshold);
  } else {
    Function_sp comparator = coerce::functionDesignator(test);
    Function_sp hasher = coerce::functionDesignator(hashf);
    table = HashTableCustom_O::create(isize, rehash_size, rehash_threshold,
                                      comparator, hasher);
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

CL_LAMBDA(ht)
CL_DECLARE();
CL_DOCSTRING(R"dx(hash_table_weakness)dx")
DOCGROUP(clasp)
CL_DEFUN Symbol_sp core__hash_table_weakness(T_sp ht) {
  if (gc::IsA<WeakKeyHashTable_sp>(ht)) {
    return kw::_sym_key;
  }
  return nil<Symbol_O>();
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

// FIXME: contents read could just be atomic maybe?
#define HASH_TABLE_ITER(tablep, key, value) \
  gctools::tagged_pointer<gctools::GCVector_moveable<KeyValuePair>> iter_datap; \
  T_sp key; \
  T_sp value; \
  {\
    HT_READ_LOCK(tablep);\
    iter_datap = tablep->_Table._Vector._Contents;\
  }\
  for (size_t it(0), itEnd(iter_datap->_End); it < itEnd; ++it) {\
  KeyValuePair& entry = (*iter_datap)[it];\
  { \
    HT_READ_LOCK(tablep);\
    key = entry._Key;\
    value = entry._Value;\
  } \
  if (!key.no_keyp()&&!key.deletedp())

#define HASH_TABLE_ITER_END }

void HashTable_O::maphash(T_sp function_desig) {
  Function_sp func = coerce::functionDesignator(function_desig);
  HASH_TABLE_ITER(this, key, value) {
    eval::funcall(func, key, value);
  } HASH_TABLE_ITER_END;
}

CL_LAMBDA(function-desig hash-table)
CL_DECLARE();
CL_DOCSTRING(R"dx(see CLHS)dx")
DOCGROUP(clasp)
CL_DEFUN T_sp cl__maphash(T_sp function_desig, HashTableBase_sp hash_table) {
  if (hash_table.nilp()) {
    SIMPLE_ERROR(("maphash called with nil hash-table"));
  }
  hash_table->maphash(function_desig);
  return nil<T_O>();
}

CL_LAMBDA(hash-table)
CL_DECLARE();
CL_DOCSTRING(R"dx(See CLHS)dx")
DOCGROUP(clasp)
CL_DEFUN T_sp cl__clrhash(HashTableBase_sp hash_table) {
  hash_table->clrhash();
  return hash_table;
};

CL_LAMBDA(cons)
CL_DECLARE();
CL_DOCSTRING(R"dx(hashTableEntryDeletedP)dx")
DOCGROUP(clasp)
CL_DEFUN bool core__hash_table_entry_deleted_p(T_sp cons) {
  if (!cons.consp())
    SIMPLE_ERROR(("Arg must be a cons"));
  return oCdr(gc::As<Cons_sp>(cons)).no_keyp();
};

CL_LAMBDA(&rest args)
CL_DECLARE();
CL_DOCSTRING(R"dx(hash_eql generates an eql hash for a list of objects)dx")
DOCGROUP(clasp)
CL_DEFUN int core__hash_eql(List_sp args) {
  HashGenerator hg;
  for (auto cur : args) {
    HashTable_O::sxhash_eql(hg, oCar(cur));
    if (!hg.isFilling()) break;
  }
  return hg.rawhash();
};

CL_LAMBDA(&rest args)
CL_DECLARE();
CL_DOCSTRING(R"dx(hash_equal generates an equal hash for a list of objects)dx")
DOCGROUP(clasp)
CL_DEFUN int core__hash_equal(List_sp args) {
  HashGenerator hg;
  for (auto cur : args) {
    HashTable_O::sxhash_equal(hg, oCar(cur));
    if (hg.isFull())
      break;
  }
  return hg.rawhash();
};

CL_LAMBDA(&rest args)
CL_DECLARE();
CL_DOCSTRING(R"dx(hash_equalp generates an equalp hash for a list of objects)dx")
DOCGROUP(clasp)
CL_DEFUN int core__hash_equalp(List_sp args) {
  HashGenerator hg;
  for (auto cur : args) {
    HashTable_O::sxhash_equalp(hg, oCar(cur));
    if (hg.isFull())
      break;
  }
  return hg.rawhash();
};

CL_LAMBDA(key hashtable)
CL_DECLARE();
CL_DOCSTRING(R"dx(remhash)dx")
DOCGROUP(clasp)
CL_DEFUN bool cl__remhash(T_sp key, HashTableBase_sp ht) {
  return ht->remhash(key);
};

T_sp HashTable_O::clrhash() {
  ASSERT(!clasp_zerop(this->_RehashSize));
  this->_HashTableCount = 0;
  T_sp no_key = ::no_key<T_O>();
  this->_Table.resize(0,KeyValuePair(no_key,no_key));
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
    hg.addGeneralAddress(gc::As_unsafe<General_sp>(obj));
    return;
  } else if (obj.consp()) {
    hg.addConsAddress(gc::As_unsafe<Cons_sp>(obj));
    return;
  } else {
    hg.addValue((uintptr_t)obj.raw_());
  }
}

void HashTable_O::sxhash_eq(Hash1Generator &hg, T_sp obj) {
  if (obj.generalp()) {
    hg.addGeneralAddress(gc::As_unsafe<General_sp>(obj));
    return;
  } else if (obj.consp()) {
    hg.addConsAddress(gc::As_unsafe<Cons_sp>(obj));
    return;
  } else {
    hg.addValue((uintptr_t)obj.raw_());
  }
}

void HashTable_O::sxhash_eql(HashGenerator &hg, T_sp obj) {
  uintptr_t tag = (uintptr_t)gctools::ptag<core::T_O*>(obj.raw_());
  switch (tag) {
  case gctools::fixnum00_tag:
  case gctools::fixnum01_tag:
#if TAG_BITS==4
  case gctools::fixnum10_tag:
  case gctools::fixnum11_tag:
#endif
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
      hg.addGeneralAddress(gc::As_unsafe<General_sp>(obj));
      return;
    }
  case gctools::cons_tag:
    {
      hg.addConsAddress(gc::As_unsafe<Cons_sp>(obj));
      return;
    }
  default:
      break;
  }
  SIMPLE_ERROR(("Illegal object (object.raw_() = %p) for eql hash %s  tag = %lu") , (void*)obj.raw_() , _rep_(obj) , tag);
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
    hg.addGeneralAddress(gc::As_unsafe<General_sp>(obj));
    return;
  } else if (obj.consp()) {
    hg.addConsAddress(gc::As_unsafe<Cons_sp>(obj));
    return;
  }
  SIMPLE_ERROR(("Illegal object for eql hash %s") , _rep_(obj));
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
  SIMPLE_ERROR(("You cannot EQUAL hash on %s") , _rep_(obj));
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
      hg.addValue(clasp_toupper(clasp_as_claspCharacter(gc::As<Character_sp>(obj))));
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
  SIMPLE_ERROR(("You cannot EQUALP hash on %s") , _rep_(obj));
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
  List_sp res = nil<T_O>();
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
  T_sp no_key = ::no_key<T_O>();
  this->_HashTableCount = 0;
  this->_Table.resize(sz,KeyValuePair(no_key,no_key));
  return sz;
}

CL_LAMBDA(arg)
CL_DECLARE();
CL_DOCSTRING(R"dx(hash-table-count)dx")
DOCGROUP(clasp)
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
    const KeyValuePair& entry = this->_Table[it];
    if (!entry._Key.no_keyp()&&!entry._Key.deletedp()) ++cnt;
  }
  return cnt;
}

CL_LAMBDA(arg)
CL_DECLARE();
CL_DOCSTRING(R"dx(hash-table-size)dx")
DOCGROUP(clasp)
CL_DEFUN uint cl__hash_table_size(HashTableBase_sp ht) {
  return ht->hashTableSize();
}

T_sp HashTable_O::operator[](const std::string& key) {
  T_sp tkey = _lisp->internKeyword(key);
  T_mv val = this->gethash(tkey);
  if (val.second().nilp()) {
    SIMPLE_ERROR(("Could not find key: %s") , tkey);
  }
  return val;
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

CL_LAMBDA(key hash-table &optional default-value)
CL_DOCSTRING(R"dx(gethash)dx")
DOCGROUP(clasp)
CL_DEFUN T_mv cl__gethash(T_sp key, HashTableBase_sp hashTable, T_sp default_value) {
  return hashTable->gethash(key, default_value);
};

CL_DOCSTRING(R"dx(gethash3)dx")
DOCGROUP(clasp)
CL_DEFUN T_mv core__gethash3(T_sp key, T_sp hashTable, T_sp default_value) {
  HashTable_sp ht = gc::As_unsafe<HashTable_sp>(hashTable);
  return ht->gethash(key, default_value);
};

KeyValuePair* HashTable_O::tableRef_no_read_lock(T_sp key, bool under_write_lock, cl_index index, HashGenerator& hg) {
  DEBUG_HASH_TABLE({core::write_bf_stream(fmt::sprintf("%s:%d key = %s  index = %ld\n" , __FILE__ , __LINE__ , _rep_(key) , index ));});
  VERIFY_HASH_TABLE(this);
  BOUNDS_ASSERT(index<this->_Table.size());
  for (size_t cur = index, curEnd(this->_Table.size()); cur<curEnd; ++cur ) {
    KeyValuePair& entry = this->_Table[cur];
    if (entry._Key.no_keyp()) goto NOT_FOUND;
    if (!entry._Key.deletedp()) {
      DEBUG_HASH_TABLE({core::write_bf_stream(fmt::sprintf("%s:%d search-end !deletedp index = %ld\n" , __FILE__ , __LINE__ , cur ));});
      if (this->keyTest(entry._Key, key)) {
        DEBUG_HASH_TABLE({core::write_bf_stream(fmt::sprintf("%s:%d search-end found key index = %ld entry._Key->%p\n .... %s\n  key->%p\n .... %s\n" , __FILE__ , __LINE__ , cur , (void*)entry._Key.raw_() , dbg_safe_repr((uintptr_t)(void*)entry._Key.raw_()).c_str() , (void*)key.raw_() , dbg_safe_repr((uintptr_t)(void*)key.raw_()).c_str() ));});
        
        return &entry;
      }
    }
  }
  for (size_t cur = 0, curEnd(index); cur<curEnd; ++cur ) {
    KeyValuePair& entry = this->_Table[cur];
    if (entry._Key.no_keyp()) goto NOT_FOUND;
    if (!entry._Key.deletedp()) {
      DEBUG_HASH_TABLE({core::write_bf_stream(fmt::sprintf("%s:%d search-begin !deletedp index = %ld\n" , __FILE__ , __LINE__ , cur ));});
      if (this->keyTest(entry._Key, key)) {
        DEBUG_HASH_TABLE({core::write_bf_stream(fmt::sprintf("%s:%d search-begin found key index = %ld\n" , __FILE__ , __LINE__ , cur ));});
        return &entry;
      }
    }
  }
 NOT_FOUND:
  DEBUG_HASH_TABLE({core::write_bf_stream(fmt::sprintf("%s:%d key not found\n" , __FILE__ , __LINE__));});
  VERIFY_HASH_TABLE(this);
  return nullptr;
}

CL_LAMBDA(ht)
CL_DECLARE();
CL_DOCSTRING(R"dx(hashTableForceRehash)dx")
DOCGROUP(clasp)
CL_DEFUN void core__hash_table_force_rehash(HashTable_sp ht) {
  HT_WRITE_LOCK(&*ht);
  ht->rehash_no_lock(false, no_key<T_O>());
}

T_mv HashTable_O::gethash(T_sp key, T_sp default_value) {
  LOG("gethash looking for key[%s]" , _rep_(key));
  HT_READ_LOCK(this);
  VERIFY_HASH_TABLE(this);
  HashGenerator hg;
  size_t sz = this->_Table.size();
  cl_index index = this->sxhashKey(key, this->_Table.size(), hg );
  KeyValuePair* keyValuePair = this->tableRef_no_read_lock(key, false /*under_write_lock*/, index, hg);
  LOG("Found keyValueCons"); // % keyValueCons->__repr__() ); INFINITE-LOOP
  if (keyValuePair) {
    T_sp value = keyValuePair->_Value;
    if (value.no_keyp()) {
      LOG("valueOrUnbound is unbound - returning default");
      return (Values(default_value, nil<T_O>()));
    }
    LOG("Found assoc - returning"); // : %s") % res->__repr__() );  INFINITE-LOOP
    return Values(value, _lisp->_true());
  }      
  return Values(default_value, nil<T_O>());
}

CL_LISPIFY_NAME("core:hashIndex");
CL_DEFMETHOD gc::Fixnum HashTable_O::hashIndex(T_sp key) const {
  HashGenerator hg;
  gc::Fixnum idx = this->sxhashKey(key, this->_Table.size(), hg );
  return idx;
}

KeyValuePair* HashTable_O::find(T_sp key) {
  HT_READ_LOCK(this);
  HashGenerator hg;
  cl_index index = this->sxhashKey(key, this->_Table.size(), hg );
  KeyValuePair* keyValue = this->tableRef_no_read_lock(key, false /*under_write_lock*/, index, hg);
  if (!keyValue) return keyValue;
  if (keyValue->_Value.no_keyp()) return nullptr;
  return keyValue;
}

bool HashTable_O::contains(T_sp key) {
  HT_READ_LOCK(this);
  KeyValuePair* keyValue = this->find(key);
  return keyValue!=nullptr;
}

bool HashTable_O::remhash(T_sp key) {
  HT_WRITE_LOCK(this);
  HashGenerator hg;
  cl_index index = this->sxhashKey(key, this->_Table.size(), hg );
  KeyValuePair* keyValuePair = this->tableRef_no_read_lock( key, true /*under_write_lock*/, index, hg );
  if (keyValuePair) {
    keyValuePair->_Key = deleted<T_O>();
    this->_HashTableCount--;
    VERIFY_HASH_TABLE(this);
    return true;
  }
  VERIFY_HASH_TABLE(this);
  return false;
}

T_sp HashTable_O::setf_gethash_no_write_lock(T_sp key, T_sp value)
{
  DEBUG_HASH_TABLE({core::write_bf_stream(fmt::sprintf("%s:%d:%s   key->%s  value->%s\n"  , __FILE__ , __LINE__ , __FUNCTION__ , _rep_(key) , _rep_(value)));});
  if (key.no_keyp()) {
    SIMPLE_ERROR(("Do not use %s as a key!!") , _rep_(key));
  }
  HashGenerator hg;
  cl_index index = this->sxhashKey(key, this->_Table.size(), hg );
  KeyValuePair* keyValuePair = this->tableRef_no_read_lock( key, true /*under_write_lock*/, index, hg);
  if (keyValuePair) {
    // rewrite value
    keyValuePair->_Value = value;
    DEBUG_HASH_TABLE({core::write_bf_stream(fmt::sprintf("%s:%d Found key/value pair: %s,%s\n" , __FILE__ , __LINE__ , _rep_(keyValuePair->_Key) , _rep_(keyValuePair->_Value)));});
    DEBUG_HASH_TABLE({core::write_bf_stream(fmt::sprintf("%s:%d  Did rplacd value: %s to cons at %p\n" , __FILE__ , __LINE__ , _rep_(value) , (void*)keyValuePair));});
    DEBUG_HASH_TABLE({core::write_bf_stream(fmt::sprintf("%s:%d  After rplacd value: %s\n" , __FILE__ , __LINE__ , _rep_(keyValuePair->_Value)));});
    VERIFY_HASH_TABLE(this);
    return value;
  }
  DEBUG_HASH_TABLE({core::write_bf_stream(fmt::sprintf("%s:%d Looking for empty slot index = %ld\n"  , __FILE__ , __LINE__ , index));});
  KeyValuePair* entryP = &this->_Table[index];
  size_t cur;
  size_t curEnd = this->_Table.size();
  for (cur = index; cur<curEnd; ++cur, ++entryP ) {
    if (entryP->_Key.no_keyp()||entryP->_Key.deletedp()) goto ADD_KEY_VALUE;
  }
  entryP = &this->_Table[0]; // wrap around
  for (cur = 0; cur<index; ++cur, ++entryP ) {
    if (entryP->_Key.no_keyp()||entryP->_Key.deletedp()) goto ADD_KEY_VALUE;
  }
  goto NO_ROOM;
 ADD_KEY_VALUE:
  entryP->_Key = key;
  entryP->_Value = value;
  this->_HashTableCount++;
  DEBUG_HASH_TABLE({core::write_bf_stream(fmt::sprintf("%s:%d Found empty slot at index = %ld\n"  , __FILE__ , __LINE__ , cur ));});
  VERIFY_HASH_TABLE_VA(this,cur,key);
  if (this->_HashTableCount > this->_RehashThreshold * this->_Table.size()) {
    LOG("Expanding hash table");
    this->rehash_no_lock(true, no_key<T_O>());
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
  this->rehash_no_lock(true, no_key<T_O>());
  VERIFY_HASH_TABLE_VA(this,cur);
  return this->setf_gethash_no_write_lock(key,value);
  // ------------
  // Here we add the key
}


T_sp HashTable_O::hash_table_setf_gethash(T_sp key, T_sp value) {
  LOG("About to hash_table_setf_gethash for %s@%p -> %s@%p\n" , _safe_rep_(key) , (void*)key.raw_() , _safe_rep_(value) , (void*)value.raw_());
  HashTableWriteLock _guard(this);
  return this->setf_gethash_no_write_lock(key, value);
}

CL_LISPIFY_NAME("core:hash-table-setf-gethash");
DOCGROUP(clasp)
CL_DEFUN T_sp core__hash_table_setf_gethash(HashTableBase_sp hash_table, T_sp key, T_sp value) {
  return hash_table->hash_table_setf_gethash(key,value);
}


KeyValuePair* HashTable_O::rehash_no_lock(bool expandTable, T_sp findKey) {
  //        printf("%s:%d rehash of hash-table@%p\n", __FILE__, __LINE__,  this );
  DEBUG_HASH_TABLE({core::write_bf_stream(fmt::sprintf("%s:%d rehash_no_lock\n" , __FILE__ , __LINE__ ));});
  ASSERTF(!clasp_zerop(this->_RehashSize), BF("RehashSize is zero - it shouldn't be"));
  gc::Fixnum curSize = this->_Table.size();
  ASSERTF(this->_Table.size() != 0, BF("HashTable is empty in expandHashTable curSize=%ld  this->_Table.size()= %lu this shouldn't be") % curSize % this->_Table.size());
  KeyValuePair* foundKeyValuePair = nullptr;
  LOG("At start of expandHashTable current hash table size: %d" , this->_Table.size());
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
  gc::Vec0<KeyValuePair> oldTable;
  oldTable.swap(this->_Table);
  size_t oldHashTableCount = this->_HashTableCount;
  newSize = this->resizeEmptyTable_no_lock(newSize);
  LOG("Resizing table to size: %d" , newSize);
  size_t oldSize = oldTable.size();
  for (size_t it(0), itEnd(oldSize); it < itEnd; ++it) {
    KeyValuePair& entry = oldTable[it];
    T_sp key = entry._Key;
    T_sp value = entry._Value;
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
      if (foundKeyValuePair==nullptr && !findKey.no_keyp()) {
        if (this->keyTest(key, findKey)) {
          foundKeyValuePair = &entry;
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
  if (foundKeyValuePair!=nullptr) {
      // Return the foundKeyValuePair in the latest table
    T_sp key = foundKeyValuePair->_Key;
    HashGenerator hg;
    cl_index index = this->sxhashKey(key, this->_Table.size(), hg );
    foundKeyValuePair = this->tableRef_no_read_lock(foundKeyValuePair->_Key,true,index,hg);
  }
  DEBUG_HASH_TABLE({core::write_bf_stream(fmt::sprintf("%s:%d:%s  Returning foundKeyValuePair: %s,%s at %p \n" , __FILE__ , __LINE__ , __FUNCTION__ , _rep_(foundKeyValuePair->_Key) , _rep_(foundKeyValuePair->_Value) , &*foundKeyValuePair));});
  return foundKeyValuePair;
}

KeyValuePair* HashTable_O::rehash_upgrade_write_lock(bool expandTable, T_sp findKey) {
  if (this->_Mutex) {
  tryAgain:
    if (this->_Mutex->write_try_lock(true /*upgrade*/)) {
      KeyValuePair* result = this->rehash_no_lock(expandTable,findKey);
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
    ss << "#<" << this->_instanceClass()->_classNameAsString();
    ss << " :COUNT " << this->_HashTableCount;
    // the calculator is only useful to check that the count is consistent;
    // uncomment this if you need to debug, but otherwise it's redundant.
//    ss << " :calculated-entries " << this->calculateHashTableCount();
    ss << " :SIZE " << this->_Table.size();
    ss << " @" << (void *)(this) << ">";
    return ss.str();
  }

#define DUMP_LOW_LEVEL 1

  void dump_one_entry(HashTable_sp ht, size_t it, stringstream &ss, KeyValuePair& entry) {
    T_sp key = entry._Key;
    T_sp value = entry._Value;
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
    ss << "     " << _rep_(entry._Key) << " " << _rep_(entry._Value) << std::endl;
#endif
  };


CL_DEFMETHOD List_sp HashTable_O::hash_table_bucket(size_t index)
{
  KeyValuePair& entry = this->_Table[index];
  if (!entry._Key.no_keyp()&&!entry._Value.deletedp()) {
    T_sp result = Cons_O::create(entry._Key, entry._Value);
    return result;
  }
  return nil<T_O>();
}


CL_DEFMETHOD T_sp HashTable_O::hash_table_average_search_length()
{
  HT_READ_LOCK(this);
  gc::Fixnum iend(this->_Table.size());
  double sum = 0.0;
  gc::Fixnum count = 0;
  for (gc::Fixnum it(0), itEnd(iend); it < itEnd; ++it) {
    const KeyValuePair& entry = this->_Table[it];
    if (!(entry._Key.no_keyp()||entry._Key.deletedp())) {
      HashGenerator hg;
      gc::Fixnum index = this->sxhashKey(entry._Key, this->_Table.size(), hg );
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
  return nil<T_O>();
}  

CL_DEFMETHOD string HashTable_O::hash_table_dump() {
  stringstream ss;
  HT_READ_LOCK(this);
  verifyHashTable(true,ss,this,__FILE__,__LINE__);
  return ss.str();
}

void HashTable_O::hash_table_pointers_dump() {
  HASH_TABLE_ITER(this, key, value) {
    printf("%s:%d key@%p: %s  value@%p: %s\n", __FILE__, __LINE__, key.raw_(), _rep_(key).c_str(), value.raw_(), _rep_(value).c_str());
  } HASH_TABLE_ITER_END;
}

void HashTable_O::hash_table_early_dump() {
  HASH_TABLE_ITER(this, key, value) {
    printf("%s:%d key@%p: %s  value@%p: %s\n", __FILE__, __LINE__, key.raw_(), _rep_(key).c_str(), value.raw_(), _rep_(value).c_str());
  } HASH_TABLE_ITER_END;
}


  void HashTable_O::mapHash(std::function<void(T_sp, T_sp)> const &fn) {
    HASH_TABLE_ITER(this, key, value) {
      fn(key, value);
    } HASH_TABLE_ITER_END;
  }

  bool HashTable_O::map_while_true(std::function<bool(T_sp, T_sp)> const &fn) const {
    HASH_TABLE_ITER(this, key, value) {
      bool cont = fn(key, value);
      if (!cont)
        return false;
    } HASH_TABLE_ITER_END;
    return true;
  }

  void HashTable_O::lowLevelMapHash(KeyValueMapper *mapper) const {
    HASH_TABLE_ITER(this, key, value) {
      if (!mapper->mapKeyValue(key, value))
        goto DONE;
    } HASH_TABLE_ITER_END;
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

CL_LAMBDA(arg)
CL_DECLARE();
CL_DOCSTRING(R"dx(hash-table-rehash-size)dx")
DOCGROUP(clasp)
CL_DEFUN Number_sp cl__hash_table_rehash_size(HashTableBase_sp ht) {
  return ht->rehash_size();
};



CL_LAMBDA(arg)
CL_DECLARE();
CL_DOCSTRING(R"dx(hash-table-rehash-threshold)dx")
DOCGROUP(clasp)
CL_DEFUN double cl__hash_table_rehash_threshold(HashTableBase_sp ht) {
  return ht->rehash_threshold();
};

CL_LAMBDA(arg)
CL_DECLARE();
CL_DOCSTRING(R"dx(hash-table-test)dx")
DOCGROUP(clasp)
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
