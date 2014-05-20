/*
 This is part of the OTF library. Copyright by ZIH, TU Dresden 2005-2013.
 Authors: Michael Heyde
*/

#ifndef HASH_H
#define HASH_H

#include "OTF_inttypes.h"

/* 0x1000 = 4096 */
#define HASH_SIZE         0X1000
#define HASH_GET_KEY(key) \
{                         \
  key += ~(key << 15);    \
  key ^=  (key >> 10);    \
  key +=  (key << 3);     \
  key ^=  (key >> 6);     \
  key += ~(key << 11);    \
  key ^=  (key >> 16);    \
  key &= HASH_SIZE - 1;   \
}

typedef struct mapInfoProcessS
{
  uint32_t process;
  uint64_t lastValue;
  uint64_t lastTime;
  double   highestRate;
  struct mapInfoProcessS *next;
} mapInfoProcessT;

/* initialize the hash */
mapInfoProcessT* hash_new( void );

/* add an entry to the hash */
mapInfoProcessT* hash_add( mapInfoProcessT *hash, uint32_t process );

/* search an entry in the hash */
mapInfoProcessT* hash_search( mapInfoProcessT *hash, uint32_t process );

/* free all mem of the hash */
void hash_delete( mapInfoProcessT *hash );

#endif /* HASH_H */
