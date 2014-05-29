#ifndef gctools_gcweakhash_H
#define gctools_gcweakhash_H

#if 0

/* %%MPS: Objects in AWL pools must be formatted so that aligned
 * pointers (with bottom bit(s) zero) can be distinguished from other
 * data types (with bottom bit(s) non-zero). Here we use a bottom
 * bit of 1 for integers. See pool/awl. */
#define TAG_COUNT(i) (((i) << 1) + 1)
#define UNTAG_COUNT(i) ((i) >> 1)

template <class T>
struct Bucket {
    Bucket* dependent;  /* the dependent object */
    size_t length;                /* number of buckets (tagged) */
    size_t used;                  /* number of buckets in use (tagged) */
    size_t deleted;               /* number of deleted buckets (tagged) */
    obj_t bucket[1];              /* hash buckets */
};




template <class KeyType, class ValueType, class KeyBucketsAllocator, class ValueBucketsAllocator >
class WeakHashTable {
//  type_t type;                  /* TYPE_TABLE */
//  hash_t hash;                  /* hash function */
//  cmp_t cmp;                    /* comparison function */
#ifdef USE_MPS
    mps_ld_s ld;                  /* location dependency */
    mps_ap_t key_ap, value_ap;    /* allocation points for keys and values */
#endif 
    Buckets*    Keys;           // hash buckets for keys
    Buckets*    Values;         // hash buckets for values


#if 0
    int buckets_find(buckets_t buckets, obj_t key, mps_ld_t ld, size_t *b)
    {
        unsigned long i, h, probe;
        unsigned long l = UNTAG_COUNT(buckets->length) - 1;
        int result = 0;
        h = this->hash(key, ld);
        probe = (h >> 8) | 1;
        h &= l;
        i = h;
        do {
            obj_t k = buckets->bucket[i];
            if(k == obj_unused || this->cmp(k, key)) {
                *b = i;
                return 1;
            }
            if(result == 0 && k == obj_deleted) {
                *b = i;
                result = 1;
            }
            i = (i+probe) & l;
        } while(i != h);
        return result;
    }
#endif

    WeakHashTable(size_t length)
    {
#if 0
        mps_addr_t addr;
        size_t l, size = ALIGN_OBJ(sizeof(table_s));
        do {
            mps_res_t res = mps_reserve(&addr, obj_ap, size);
            if (res != MPS_RES_OK) error("out of memory in make_table");
            obj = addr;
            obj->table.type = TYPE_TABLE;
            obj->table.keys = obj->table.values = NULL;
        } while(!mps_commit(obj_ap, addr, size));
#endif
        /* round up to next power of 2 */
        for(l = 1; l < length; l *= 2);
        this->keys = KeyBucketsAllocator::allocate(l);
        this->values = ValueBucketsAllocator::allocate(l);
        this->keys->dependent = this->values;
        this->values->dependent = this->keys;
#ifdef USE_MPS
        mps_ld_reset(&this->ld, _global_arena);
#endif
    }
};



#if 0
/* MPS globals                                                  %%MPS
 *
 * These are global variables holding MPS values for use by the
 * interpreter.  In a more sophisticated integration some of these might
 * be thread local.  See `main` for where these are set up.
 *
 * `arena` is the global state of the MPS, and there's usually only one
 * per process. See topic/arena.
 *
 * `obj_pool` is the memory pool in which the Scheme objects are allocated.
 * It is an instance of the Automatic Mostly Copying (AMC) pool class, which
 * is a general-purpose garbage collector for use when there are formatted
 * objects in the pool, but ambiguous references in thread stacks and
 * registers. See pool/amc.
 *
 * `obj_ap` is an Allocation Point that allows fast in-line non-locking
 * allocation in a memory pool.  This would usually be thread-local, but
 * this interpreter is single-threaded.  See `make_pair` etc. for how this
 * is used with the reserve/commit protocol.
 *
 * `buckets_pool` is the memory pool for hash table buckets. There are
 * two allocation points, one for buckets containing exact (strong)
 * references, the other for buckets containing weak references.
 */

static mps_arena_t arena;       /* the arena */
static mps_pool_t obj_pool;     /* pool for ordinary Scheme objects */
static mps_ap_t obj_ap;         /* allocation point used to allocate objects */
static mps_pool_t leaf_pool;    /* pool for leaf objects */
static mps_ap_t leaf_ap;        /* allocation point for leaf objects */
static mps_pool_t buckets_pool; /* pool for hash table buckets */
static mps_ap_t strong_buckets_ap; /* allocation point for strong buckets */
static mps_ap_t weak_buckets_ap; /* allocation point for weak buckets */

static buckets_t make_buckets(size_t length, mps_ap_t ap)
{
  buckets_t buckets;
  mps_addr_t addr;
  size_t size;
  size = ALIGN_OBJ(offsetof(buckets_s, bucket) + length * sizeof(buckets->bucket[0]));
  do {
    mps_res_t res = mps_reserve(&addr, ap, size);
    size_t i;
    if (res != MPS_RES_OK) error("out of memory in make_buckets");
    buckets = addr;
    buckets->dependent = NULL;
    buckets->length = TAG_COUNT(length);
    buckets->used = TAG_COUNT(0);
    buckets->deleted = TAG_COUNT(0);
    for(i = 0; i < length; ++i) {
      buckets->bucket[i] = obj_unused;
    }
  } while(!mps_commit(ap, addr, size));
  total += size;
  return buckets;
}



static int buckets_find(obj_t tbl, buckets_t buckets, obj_t key, mps_ld_t ld, size_t *b)
{
  unsigned long i, h, probe;
  unsigned long l = UNTAG_COUNT(buckets->length) - 1;
  int result = 0;
  assert(TYPE(tbl) == TYPE_TABLE);
  h = this->hash(key, ld);
  probe = (h >> 8) | 1;
  h &= l;
  i = h;
  do {
    obj_t k = buckets->bucket[i];
    if(k == obj_unused || this->cmp(k, key)) {
      *b = i;
      return 1;
    }
    if(result == 0 && k == obj_deleted) {
      *b = i;
      result = 1;
    }
    i = (i+probe) & l;
  } while(i != h);
  return result;
}

static size_t table_size(obj_t tbl)
{
  size_t used, deleted;
  assert(TYPE(tbl) == TYPE_TABLE);
  used = UNTAG_COUNT(this->keys->used);
  deleted = UNTAG_COUNT(this->keys->deleted);
  assert(used >= deleted);
  return used - deleted;
}

/* Rehash 'tbl' so that it has 'new_length' buckets. If 'key' is found
 * during this process, update 'key_bucket' to be the index of the
 * bucket containing 'key' and return true, otherwise return false.
 * 
 * %%MPS: When re-hashing the table we reset the associated location
 * dependency and re-add a dependency on each object in the table.
 * This is because the table gets re-hashed when the locations of
 * objects have changed. See topic/location.
 */
static int table_rehash(obj_t tbl, size_t new_length, obj_t key, size_t *key_bucket)
{
  size_t i, length;
  buckets_t new_keys, new_values;
  int result = 0;

  assert(TYPE(tbl) == TYPE_TABLE);
  length = UNTAG_COUNT(this->keys->length);
  new_keys = make_buckets(new_length, this->key_ap);
  new_values = make_buckets(new_length, this->value_ap);
  new_keys->dependent = new_values;
  new_values->dependent = new_keys;
  mps_ld_reset(&this->ld, arena);

  for (i = 0; i < length; ++i) {
    obj_t old_key = this->keys->bucket[i];
    if (old_key != obj_unused && old_key != obj_deleted) {
      int found;
      size_t b;
      found = buckets_find(tbl, new_keys, old_key, &this->ld, &b);
      assert(found);            /* new table shouldn't be full */
      assert(new_keys->bucket[b] == obj_unused); /* shouldn't be in new table */
      new_keys->bucket[b] = old_key;
      new_values->bucket[b] = this->values->bucket[i];
      if (key != NULL && this->cmp(old_key, key)) {
        *key_bucket = b;
        result = 1;
      }
      new_keys->used = TAG_COUNT(UNTAG_COUNT(new_keys->used) + 1);
    }
  }

  assert(UNTAG_COUNT(new_keys->used) == table_size(tbl));
  this->keys = new_keys;
  this->values = new_values;
  return result;
}

/* %%MPS: If we fail to find 'key' in the table, and if mps_ld_isstale
 * returns true, then some of the keys in the table might have been
 * moved by the garbage collector: in this case we need to re-hash the
 * table. See topic/location.
 */
static obj_t table_ref(obj_t tbl, obj_t key)
{
  size_t b;
  assert(TYPE(tbl) == TYPE_TABLE);
  if (buckets_find(tbl, this->keys, key, NULL, &b)) {
    obj_t k = this->keys->bucket[b];
    if (k != obj_unused && k != obj_deleted)
      return this->values->bucket[b];
  }
  if (mps_ld_isstale(&this->ld, arena, key))
    if (table_rehash(tbl, UNTAG_COUNT(this->keys->length), key, &b))
      return this->values->bucket[b];
  return NULL;
}

static int table_try_set(obj_t tbl, obj_t key, obj_t value)
{
  size_t b;
  assert(TYPE(tbl) == TYPE_TABLE);
  if (!buckets_find(tbl, this->keys, key, &this->ld, &b))
    return 0;
  if (this->keys->bucket[b] == obj_unused) {
    this->keys->bucket[b] = key;
    this->keys->used = TAG_COUNT(UNTAG_COUNT(this->keys->used) + 1);
  } else if (this->keys->bucket[b] == obj_deleted) {
    this->keys->bucket[b] = key;
    assert(this->keys->deleted > TAG_COUNT(0));
    this->keys->deleted
      = TAG_COUNT(UNTAG_COUNT(this->keys->deleted) - 1);
  }
  this->values->bucket[b] = value;
  return 1;
}

static int table_full(obj_t tbl)
{
  assert(TYPE(tbl) == TYPE_TABLE);
  return this->keys->used >= this->keys->length / 2;
}

static void table_set(obj_t tbl, obj_t key, obj_t value)
{
  assert(TYPE(tbl) == TYPE_TABLE);
  if (table_full(tbl) || !table_try_set(tbl, key, value)) {
    int res;
    table_rehash(tbl, UNTAG_COUNT(this->keys->length) * 2, NULL, NULL);
    res = table_try_set(tbl, key, value);
    assert(res);                /* rehash should have made room */
  }
}

static void table_delete(obj_t tbl, obj_t key)
{
  size_t b;
  assert(TYPE(tbl) == TYPE_TABLE);
  if(!buckets_find(tbl, this->keys, key, NULL, &b) ||
     this->keys->bucket[b] == obj_unused ||
     this->keys->bucket[b] == obj_deleted)
  {
    if(!mps_ld_isstale(&this->ld, arena, key))
      return;
    if(!table_rehash(tbl, UNTAG_COUNT(this->keys->length), key, &b))
      return;
  }
  if(this->keys->bucket[b] != obj_unused &&
     this->keys->bucket[b] != obj_deleted) 
  {
    this->keys->bucket[b] = obj_deleted;
    this->keys->deleted
      = TAG_COUNT(UNTAG_COUNT(this->keys->deleted) + 1);
    this->values->bucket[b] = NULL;
  }
}




/* buckets_scan -- buckets format scan method                        %%MPS
 */

static mps_res_t buckets_scan(mps_ss_t ss, mps_addr_t base, mps_addr_t limit)
{
  MPS_SCAN_BEGIN(ss) {
    while (base < limit) {
      buckets_t buckets = base;
      size_t i, length = UNTAG_COUNT(buckets->length);
      FIX(buckets->dependent);
      if(buckets->dependent != NULL)
        assert(buckets->dependent->length == buckets->length);
      for (i = 0; i < length; ++i) {
        mps_addr_t p = buckets->bucket[i];
        if (MPS_FIX1(ss, p)) {
          mps_res_t res = MPS_FIX2(ss, &p);
          if (res != MPS_RES_OK) return res;
          if (p == NULL) {
            /* key/value was splatted: splat value/key too */
            p = obj_deleted;
            buckets->deleted = TAG_COUNT(UNTAG_COUNT(buckets->deleted) + 1);
            if (buckets->dependent != NULL) {
              buckets->dependent->bucket[i] = p;
              buckets->dependent->deleted
                = TAG_COUNT(UNTAG_COUNT(buckets->dependent->deleted) + 1);
            }
          }
          buckets->bucket[i] = p;
        }
      }
      base = (char *)base + ALIGN_OBJ(offsetof(buckets_s, bucket) +
                                      length * sizeof(buckets->bucket[0]));
    }
  } MPS_SCAN_END(ss);
  return MPS_RES_OK;
}


/* buckets_skip -- buckets format skip method                        %%MPS
 */

static mps_addr_t buckets_skip(mps_addr_t base)
{
  buckets_t buckets = base;
  size_t length = UNTAG_COUNT(buckets->length);
  return (char *)base + ALIGN_OBJ(offsetof(buckets_s, bucket) +
                                  length * sizeof(buckets->bucket[0]));
}


/* buckets_find_dependent -- find dependent object for buckets       %%MPS
 *
 * Each object in an AWL pool can have a "dependent object". The MPS
 * ensures that when an object is being scanned, its dependent object
 * is unprotected. This allows prompt deletion of values in a weak-key
 * hash table, and keys in a weak-value hash table.
 */

static mps_addr_t buckets_find_dependent(mps_addr_t addr)
{
  buckets_t buckets = addr;
  return buckets->dependent;
}


/* globals_scan -- scan static global variables                 %%MPS
 *
 * The static global variables are all used to hold values that are set
 * up using the `sptab` and `isymtab` tables, and conveniently we have
 * a list of pointers to those variables.  This is a custom root scanning
 * method that uses them to fix those variables. See topic/root.
 */
#endif

#endif // gctools_gcweakhash_H
