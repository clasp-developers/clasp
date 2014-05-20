#ifndef VENDOR_HASH_JENKINS_H
#define VENDOR_HASH_JENKINS_H

#ifdef HAVE_CONFIG_H
#include "config.h"
#endif

#include <stddef.h>       /* defines size_t etc */
#include <OTF_inttypes.h> /* defines uint32_t etc */

#ifdef SELF_TEST
# include <stdio.h>      /* defines printf for tests */
# include <time.h>       /* defines time_t for timings in the test */
#endif

#ifdef __cplusplus
extern "C" {
#endif


/*
 * hash():
 * compiletime multiplexer for littel- and big-endian.
 * Note that the hash value is NOT endias agnostic.
 */
#ifdef WORDS_BIGENDIAN
# define hash(key, length, initval) hashbig((key), (length), (initval))
#else
# define hash(key, length, initval) hashlittle((key), (length), (initval))
#endif

#define hashsize(n) ((uint32_t)1<<(n))
#define hashmask(n) (hashsize(n)-1)


/*
--------------------------------------------------------------------
 This works on all machines.  To be useful, it requires
 -- that the key be an array of uint32_t's, and
 -- that the length be the number of uint32_t's in the key

 The function hashword() is identical to hashlittle() on little-endian
 machines, and identical to hashbig() on big-endian machines,
 except that the length has to be measured in uint32_ts rather than in
 bytes.  hashlittle() is more complicated than hashword() only because
 hashlittle() has to dance around fitting the key bytes into registers.
--------------------------------------------------------------------
*/
uint32_t hashword(
const uint32_t *k,          /* the key, an array of uint32_t values */
size_t          length,     /* the length of the key, in uint32_ts */
uint32_t        initval);   /* the previous hash, or an arbitrary value */


/*
--------------------------------------------------------------------
hashword2() -- same as hashword(), but take two seeds and return two
32-bit values.  pc and pb must both be nonnull, and *pc and *pb must
both be initialized with seeds.  If you pass in (*pb)==0, the output
(*pc) will be the same as the return value from hashword().
--------------------------------------------------------------------
*/
void hashword2 (
const uint32_t *k,      /* the key, an array of uint32_t values */
size_t          length, /* the length of the key, in uint32_ts */
uint32_t       *pc,     /* IN: seed OUT: primary hash value */
uint32_t       *pb);    /* IN: more seed OUT: secondary hash value */


/*
-------------------------------------------------------------------------------
hashlittle() -- hash a variable-length key into a 32-bit value
  k       : the key (the unaligned variable-length array of bytes)
  length  : the length of the key, counting by bytes
  initval : can be any 4-byte value
Returns a 32-bit value.  Every bit of the key affects every bit of
the return value.  Two keys differing by one or two bits will have
totally different hash values.

The best hash table sizes are powers of 2.  There is no need to do
mod a prime (mod is sooo slow!).  If you need less than 32 bits,
use a bitmask.  For example, if you need only 10 bits, do
  h = (h & hashmask(10));
In which case, the hash table should have hashsize(10) elements.

If you are hashing n strings (uint8_t **)k, do it like this:
  for (i=0, h=0; i<n; ++i) h = hashlittle( k[i], len[i], h);

By Bob Jenkins, 2006.  bob_jenkins@burtleburtle.net.  You may use this
code any way you wish, private, educational, or commercial.  It's free.

Use for hash table lookup, or anything where one collision in 2^^32 is
acceptable.  Do NOT use for cryptographic purposes.
-------------------------------------------------------------------------------
*/
uint32_t hashlittle( const void *key, size_t length, uint32_t initval);


/*
 * hashlittle2: return 2 32-bit hash values
 *
 * This is identical to hashlittle(), except it returns two 32-bit hash
 * values instead of just one.  This is good enough for hash table
 * lookup with 2^^64 buckets, or if you want a second hash if you're not
 * happy with the first, or if you want a probably-unique 64-bit ID for
 * the key.  *pc is better mixed than *pb, so use *pc first.  If you want
 * a 64-bit value do something like "*pc + (((uint64_t)*pb)<<32)".
 */
void hashlittle2(
  const void *key,      /* the key to hash */
  size_t      length,   /* length of the key */
  uint32_t   *pc,       /* IN: primary initval, OUT: primary hash */
  uint32_t   *pb);      /* IN: secondary initval, OUT: secondary hash */


/*
 * hashbig():
 * This is the same as hashword() on big-endian machines.  It is different
 * from hashlittle() on all machines.  hashbig() takes advantage of
 * big-endian byte ordering.
 */
uint32_t hashbig( const void *key, size_t length, uint32_t initval);


#ifdef __cplusplus
}
#endif

#endif /* VENDOR_HASH_JENKINS_H */
