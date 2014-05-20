/*
 * Copyright 1993, 1995 Christopher Seiwald.
 *
 * This file is part of Jam - see jam.c for Copyright information.
 */

# include "jam.h"
# include "newstr.h"
# include "hash.h"
# include "compile.h"
# include <stddef.h>
# include <stdlib.h>

/*
 * newstr.c - string manipulation routines
 *
 * To minimize string copying, string creation, copying, and freeing
 * is done through newstr.
 *
 * External functions:
 *
 *    newstr() - return a dynamically allocated copy of a string
 *    copystr() - return a copy of a string previously returned by newstr()
 *    freestr() - free a string returned by newstr() or copystr()
 *    str_done() - free string tables
 *
 * Once a string is passed to newstr(), the returned string is readonly.
 *
 * This implementation builds a hash table of all strings, so that multiple
 * calls of newstr() on the same string allocate memory for the string once.
 * Strings are never actually freed.
 */

typedef char * STRING;

static struct hash * strhash      = 0;
static int           strtotal     = 0;
static int           strcount_in  = 0;
static int           strcount_out = 0;


/*
 * Immortal string allocator implementation speeds string allocation and cuts
 * down on internal fragmentation.
 */

# define STRING_BLOCK 4096
typedef struct strblock
{
    struct strblock * next;
    char              data[STRING_BLOCK];
} strblock;

static strblock * strblock_chain = 0;

/* Storage remaining in the current strblock */
static char * storage_start = 0;
static char * storage_finish = 0;


/*
 * allocate() - Allocate n bytes of immortal string storage.
 */

static char * allocate( size_t const n )
{
#ifdef BJAM_NEWSTR_NO_ALLOCATE
    return (char*)BJAM_MALLOC_ATOMIC(n);
#else
    /* See if we can grab storage from an existing block. */
    size_t remaining = storage_finish - storage_start;
    if ( remaining >= n )
    {
        char * result = storage_start;
        storage_start += n;
        return result;
    }
    else /* Must allocate a new block. */
    {
        strblock * new_block;
        size_t nalloc = n;
        if ( nalloc < STRING_BLOCK )
            nalloc = STRING_BLOCK;

        /* Allocate a new block and link into the chain. */
        new_block = (strblock *)BJAM_MALLOC( offsetof( strblock, data[0] ) + nalloc * sizeof( new_block->data[0] ) );
        if ( new_block == 0 )
            return 0;
        new_block->next = strblock_chain;
        strblock_chain = new_block;

        /* Take future allocations out of the larger remaining space. */
        if ( remaining < nalloc - n )
        {
            storage_start = new_block->data + n;
            storage_finish = new_block->data + nalloc;
        }
        return new_block->data;
    }
#endif
}


/*
 * newstr() - return a dynamically allocated copy of a string.
 */

char * newstr( char * string )
{
    STRING str;
    STRING * s = &str;

    if ( !strhash )
        strhash = hashinit( sizeof( STRING ), "strings" );

    *s = string;

    if ( hashenter( strhash, (HASHDATA **)&s ) )
    {
        int l = strlen( string );
        char * m = (char *)allocate( l + 1 );

        strtotal += l + 1;
        memcpy( m, string, l + 1 );
        *s = m;
    }

    strcount_in += 1;
    return *s;
}


/*
 * copystr() - return a copy of a string previously returned by newstr()
 */

char * copystr( char * s )
{
    strcount_in += 1;
    return s;
}


/*
 * freestr() - free a string returned by newstr() or copystr()
 */

void freestr( char * s )
{
    strcount_out += 1;
}


/*
 * str_done() - free string tables.
 */

void str_done()
{
    /* Reclaim string blocks. */
    while ( strblock_chain != 0 )
    {
        strblock * n = strblock_chain->next;
        BJAM_FREE(strblock_chain);
        strblock_chain = n;
    }

    hashdone( strhash );

    if ( DEBUG_MEM )
        printf( "%dK in strings\n", strtotal / 1024 );

    /* printf( "--- %d strings of %d dangling\n", strcount_in-strcount_out, strcount_in ); */
}
