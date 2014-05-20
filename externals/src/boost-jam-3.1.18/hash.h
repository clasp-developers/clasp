/*
 * Copyright 1993, 1995 Christopher Seiwald.
 *
 * This file is part of Jam - see jam.c for Copyright information.
 */

/*
 * hash.h - simple in-memory hashing routines
 */

#ifndef BOOST_JAM_HASH_H
#define BOOST_JAM_HASH_H

typedef struct hashdata HASHDATA;

struct hash * hashinit     ( int datalen, char * name );
int           hashitem     ( struct hash * hp, HASHDATA * * data, int enter );
void          hashdone     ( struct hash * hp );
void          hashenumerate( struct hash * hp, void (* f)( void *, void * ), void * data );
int           hash_free    ( struct hash * hp, HASHDATA * data);

#define hashenter( hp, data ) ( !hashitem( hp, data, !0 ) )
#define hashcheck( hp, data ) hashitem( hp, data, 0 )

#endif
