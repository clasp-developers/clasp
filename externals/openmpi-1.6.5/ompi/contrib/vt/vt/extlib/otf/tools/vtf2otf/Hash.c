/*
 This is part of the OTF library. Copyright by ZIH, TU Dresden 2005-2013.
 Authors: Andreas Knuepfer, Holger Brunst, Ronny Brendel, Thomas Kriebitzsch
*/

#include "Hash.h"

#include <stdlib.h>
#include <memory.h>

/* *** macros ****************************************/
#ifndef sun

#define HASH_GET_KEY(key)	\
{				\
  key += ~(key << 15);		\
  key ^=  (key >> 10);		\
  key +=  (key << 3);		\
  key ^=  (key >> 6);		\
  key += ~(key << 11);		\
  key ^=  (key >> 16);		\
  key &= HASHSIZE - 1;		\
}

#else /* sun */

/* stupid stupid gcc2.95 on sun */

uint32_t hash_get_key( uint32_t key ) {

  key += ~(key << 15);
  key ^=  (key >> 10);
  key +=  (key << 3);
  key ^=  (key >> 6);
  key += ~(key << 11);
  key ^=  (key >> 16);
  key &= HASH_SIZE - 1;

  return key;
}

#define HASH_GET_KEY(key) key= hash_get_key(key)

#endif /* sun */


Hash *initHash() {


	Hash *ret= (Hash*) malloc( sizeof( Hash ) );
	memset( ret, 0, sizeof( Hash ) );
	
	return ret;
}


void closeHash( Hash *hash ) {

	
	int i;
	
	for( i= 0; i < HASHSIZE; ++i ) {
	
		if( NULL != hash->entrys[i].children ) {
			free( hash->entrys[i].children );
		}
		hash->entrys[i].children= NULL;
		hash->entrys[i].childrenc= 0;
		
	}
	
	free( hash );
}


uint32_t searchHash( Hash *hash, uint32_t src ) {


	int i;
	uint32_t key= src;
	
	if( NULL == hash ) {
		return 0;
	}
	
	HASH_GET_KEY( key );	
	
	if( src == hash->entrys[key].src ) {
		return hash->entrys[key].dst;
	}
	
	for( i= 0; i < hash->entrys[key].childrenc; ++i ) {
	
		if( src == hash->entrys[key].children[i].src ) {
			return hash->entrys[key].children[i].dst;
		}
	}
	
	
	return 0;
}


void addHash( Hash *hash, uint32_t src, uint32_t dst ) {


	uint32_t key= src;
	
	HASH_GET_KEY( key );	
	
	/* free entry */
	if( 0 == hash->entrys[key].src ) {
		
		hash->entrys[key].src= src;
		hash->entrys[key].dst= dst;
		
	} else {
	
		hash->entrys[key].children= (SubEntry*) realloc(
			hash->entrys[key].children, sizeof( SubEntry ) *
			(hash->entrys[key].childrenc + 1) );
		
		hash->entrys[key].children[hash->entrys[key].childrenc].src= src;
		hash->entrys[key].children[hash->entrys[key].childrenc].dst= dst;
		
		++hash->entrys[key].childrenc;
	}
	
	
}
