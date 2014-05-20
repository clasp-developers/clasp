/*
 This is part of the OTF library. Copyright by ZIH, TU Dresden 2005-2013.
 Authors: Michael Heyde
*/

#include "hash.h"

#include <stdlib.h>

static mapInfoProcessT *add_inner( mapInfoProcessT **current, uint32_t process )
{
  if( NULL != *current )
  {
    return add_inner( &((mapInfoProcessT*)(*current))->next, process );
  }
  else
  {
    *current = (mapInfoProcessT*)calloc( 1, sizeof(mapInfoProcessT) );
    (*current)->process = process;
    return *current;
  }
}

static mapInfoProcessT *search_inner( mapInfoProcessT *current, uint32_t process )
{
  if( current )
  {
    if( current->process == process )
      return current;
    return search_inner( current->next, process );
  }
  return NULL;
}

mapInfoProcessT *hash_new( )
{
  mapInfoProcessT *ret;
  int i;

  ret = (mapInfoProcessT*)calloc( HASH_SIZE, sizeof(mapInfoProcessT) );
  for( i = 0; i < HASH_SIZE; i++)
  {
    ret[i].process = (uint32_t)-1;
  }

  return ret;
}

mapInfoProcessT *hash_add( mapInfoProcessT *hash, uint32_t process)
{
  uint32_t hashkey = process;

  HASH_GET_KEY( hashkey );

  if( hash[hashkey].process == (uint32_t)-1 )
  {
    hash[hashkey].process = process;
    return &(hash[hashkey]);
  }
  else
  {
    return add_inner( &(hash[hashkey].next), process );
  }
}

mapInfoProcessT *hash_search( mapInfoProcessT *hash, uint32_t process)
{
  uint32_t hashkey = process;

  HASH_GET_KEY( hashkey );

  if( hash[hashkey].process == process )
  {
    return &(hash[hashkey]);
  }
  else
  {
    return search_inner( hash[hashkey].next, process );
  }
}

void hash_delete( mapInfoProcessT *hash )
{
  int i;

  for( i = 0; i < HASH_SIZE; i++ )
  {
    mapInfoProcessT *currentElement = hash[i].next;
    while( NULL != currentElement )
    {
      mapInfoProcessT *tmpElement = currentElement;
      currentElement = currentElement->next;
      free( tmpElement );
    }
  }

  free( hash );
}
