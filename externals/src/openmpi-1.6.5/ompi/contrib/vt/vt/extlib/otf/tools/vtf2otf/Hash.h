/*
 This is part of the OTF library. Copyright by ZIH, TU Dresden 2005-2013.
 Authors: Andreas Knuepfer, Holger Brunst, Ronny Brendel, Thomas Kriebitzsch
*/

#ifndef HASH_H
#define HASH_H

#ifdef HAVE_CONFIG_H
#include "config.h"
#endif


#include "OTF_inttypes.h"

#define HASHSIZE 1024


struct struct_SubEntry {

	uint32_t src;
	uint32_t dst;
};
typedef struct struct_SubEntry SubEntry;


struct struct_MainEntry {

	uint32_t src;
	uint32_t dst;
	
	SubEntry *children;
	int childrenc;
};
typedef struct struct_MainEntry MainEntry;


struct struct_Hash {

	MainEntry entrys[HASHSIZE];
};
typedef struct struct_Hash Hash;


Hash *initHash( void );
void closeHash( Hash *hash );
uint32_t searchHash( Hash *hash, uint32_t src );
void addHash( Hash *hash, uint32_t src, uint32_t dst );

#endif /* HASH_H */
