/*
 This is part of the OTF library. Copyright by ZIH, TU Dresden 2005-2013.
 Authors: Andreas Knuepfer, Holger Brunst, Ronny Brendel, Thomas Kriebitzsch
*/

#include <stdlib.h>
#include <string.h>

#include <stdio.h>

#include "OTF_Platform.h"

#include "vtf3.h"

#include "Treehash.h"


/* *** macros ****************************************/
#define HASH_SIZE	65536

#ifndef sun

#define HASH_GET_KEY(key)	\
{				\
  key += ~(key << 15);		\
  key ^=  (key >> 10);		\
  key +=  (key << 3);		\
  key ^=  (key >> 6);		\
  key += ~(key << 11);		\
  key ^=  (key >> 16);		\
  key &= HASH_SIZE - 1;		\
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


/* *** variables ****************************************/
hashtabT* p_hashtab= NULL;


void treehash_init( nodeT **pp_root, hashtabT **pp_hashtab )
{
	int i;
	uint32_t hashkey;
	
	/* init the tree */
	(*pp_root) = (nodeT *) malloc( sizeof( nodeT ) );
	(*pp_root)->process = 0;
	(*pp_root)->processi = 0;
	(*pp_root)->name = 0;
	(*pp_root)->childrensize = 0;
	(*pp_root)->p_children = 0;
    
    (*pp_root)->stack = NULL;
    (*pp_root)->stackc = 0;
    (*pp_root)->stacks = 0;
    
	
	/* init the hashtab */
	(*pp_hashtab) = (hashtabT *) malloc ( sizeof( hashtabT ) * HASH_SIZE );
	
	/* zero init the whole hashtable */
	for( i = 0; i < HASH_SIZE; i++ )
	{
		(*pp_hashtab)[i].p_node = 0;
		(*pp_hashtab)[i].entryvecsize = 0;
		(*pp_hashtab)[i].p_entryvec = 0;
	}
	
	/* insert root to the hashtable */
	hashkey = 0;
	HASH_GET_KEY( hashkey );
	(*pp_hashtab)[hashkey].p_node = (*pp_root);
	(*pp_hashtab)[hashkey].entryvecsize = 0;
	(*pp_hashtab)[hashkey].p_entryvec = 0;
}

int treehash_addnode( hashtabT *p_hashtab, uint32_t process, const char* name, uint32_t parent )
{
	nodeT *p_newnode;
	nodeT *p_parentnode;
	
	uint32_t hashkey;
	
	/* -- get and fill the new node -- */
	p_newnode = (nodeT *) malloc( sizeof( nodeT ) );
	
	p_newnode->process = process;
	p_newnode->processi = 0;
	if ( name )
		p_newnode->name = strdup( name );
	else
		p_newnode->name = 0;
	p_newnode->childrensize = 0;
	p_newnode->p_children = 0;
    
    p_newnode->stack = NULL;
    p_newnode->stackc = 0;
    p_newnode->stacks = 0;
	
	/* -- search parent node and connect the parent node to the new node -- */
	p_parentnode = treehash_searchnode( p_hashtab, parent );
	
	if( p_parentnode->childrensize == 0 )
	{
		p_parentnode->p_children = (nodeT **) malloc( sizeof( nodeT *) );
	}
	else
	{
		p_parentnode->p_children = (nodeT **) realloc( p_parentnode->p_children,
			sizeof( nodeT *) * p_parentnode->childrensize + 1 );
	}
	
	p_parentnode->p_children[p_parentnode->childrensize] = p_newnode;
	p_parentnode->childrensize++;
	
	/* -- insert the new node to the hashtable -- */
	hashkey = process;
	
	HASH_GET_KEY( hashkey );
	
	if (p_hashtab[hashkey].p_node == 0)
	{ /* if hashkey is not used */
		p_hashtab[hashkey].p_node = p_newnode;
	}
	else
	{ /* if hashkey is used */
		int i;
		
		if (p_hashtab[hashkey].p_entryvec == 0)
		{ /* if entryvec == 0 take malloc, and copy first item into it */
			p_hashtab[hashkey].p_entryvec =
				( entryvecT * )malloc( sizeof( entryvecT ) * 2);

			/* no memory left */
			if (p_hashtab[hashkey].p_entryvec == 0)
				return -1;
						
			/* insert first node of the hash */
			p_hashtab[hashkey].p_entryvec[0].p_node = p_hashtab[hashkey].p_node;
			p_hashtab[hashkey].entryvecsize = 1;	
		}
		else
		{ /* if entryvec != 0 take realloc */
			p_hashtab[hashkey].p_entryvec = ( entryvecT * )
				realloc( p_hashtab[hashkey].p_entryvec, sizeof( entryvecT ) *
					(p_hashtab[hashkey].entryvecsize + 1) );
			
			if (p_hashtab[hashkey].p_entryvec == 0)
				return -1;
		}
				
		/* search right position for the new node and make space for newnode*/
		for( i = p_hashtab[hashkey].entryvecsize - 1; i >= 0 &&
			p_hashtab[hashkey].p_entryvec[i].p_node->process > process; i-- )
		{
			p_hashtab[hashkey].p_entryvec[i+1]
				= p_hashtab[hashkey].p_entryvec[i];
		}
			
		/* insert newnode */
		p_hashtab[hashkey].p_entryvec[i+1].p_node = p_newnode;
		
		p_hashtab[hashkey].entryvecsize++;
	}

	return 0;
}

nodeT *treehash_searchnode ( hashtabT *p_hashtab, uint32_t process )
{
	uint32_t hashkey = process;
	
	HASH_GET_KEY( hashkey );
	
	if ( 0 == p_hashtab[hashkey].p_node )
		return 0;
	
	if ( p_hashtab[hashkey].p_node->process == process )
		return p_hashtab[hashkey].p_node;
	else if ( !p_hashtab[hashkey].p_entryvec )
		return 0;
	else
	{ /* search the vector of the hashkey */
		return treehash_searchvec(p_hashtab[hashkey].p_entryvec,
			p_hashtab[hashkey].entryvecsize, process );
	}
}

nodeT *treehash_searchvec ( entryvecT* vec, int vecsize, uint32_t process )
{
	int l;
	int r;
	entryvecT *a;
  
	a = vec;
	l = 0;
	r = vecsize - 1;
	while ( r >= l )
	{
		int32_t m = ( l + r ) / 2;
     
		if ( process == a[m].p_node->process )
		{ 
			return a[m].p_node;
		}

		if ( process < a[m].p_node->process )
			r = m - 1;
		else
			l = m + 1;
	}
   
	return 0;
}

int treehash_createindices ( int index, nodeT *p_node )
{
	int i;
	
	p_node->processi = index;
	index++;
	
	for( i = 0; i < p_node->childrensize; i++ )
		index = treehash_createindices ( index,
			p_node->p_children[i] );
	
	return index;
}


void treehash_deleteall ( hashtabT *p_hashtab )
{
	int i;
	int a;
	
	for( i = 0; i < HASH_SIZE; i++ )
	{
		treehash_deletenode ( p_hashtab[i].p_node );
		if ( p_hashtab[i].p_entryvec )
			p_hashtab[i].p_entryvec[0].p_node = 0;
			
		
		if ( p_hashtab[i].p_entryvec )
		{
			for( a = 0; a < p_hashtab[i].entryvecsize; a++ )
			{
				treehash_deletenode ( p_hashtab[i].p_entryvec[a].p_node );
			}
			
			free( p_hashtab[i].p_entryvec );
			p_hashtab[i].p_entryvec = 0;
		}
	}
}

void treehash_deletenode ( nodeT *p_node )
{
	if ( p_node )
	{
		if ( p_node->name )
		{
			free( p_node->name );
			p_node->name = 0;
		}
		if ( p_node->p_children )
		{
			free( p_node-> p_children );
			p_node-> p_children = 0;
		}
        
        if( p_node->stack != NULL ) {
            free(p_node->stack);
            p_node->stack = NULL;
            p_node->stackc = 0;
            p_node->stacks = 0;
        }
			
		free ( p_node );
		p_node = 0;
	}
}


static void FileIOEndQueue_pop( FileIOEndRecord** queue );

FileIOEndRecord* FileIOEndQueue_init() {


	return NULL;
}


void FileIOEndQueue_check( FileIOEndRecord** queue, uint64_t time, void* fha ) {


	if( NULL == (*queue) ) return;

	while( NULL != (*queue) && (*queue)->time < time ) {

		VTF3_WriteFileioend( ((fcbT*) fha)->fcb, (*queue)->time,
			(*queue)->process, (*queue)->iotype, (*queue)->fileid,
			(*queue)->bytes, (*queue)->source );

		FileIOEndQueue_pop( queue );
	}
}


void FileIOEndQueue_finish( FileIOEndRecord** queue ) {


	while( NULL != (*queue) ) {

		FileIOEndQueue_pop( queue );
	}
}


void FileIOEndQueue_pop( FileIOEndRecord** queue ) {


	FileIOEndRecord* tmp;

	if( (*queue) != NULL ) {

		if( (*queue)->next != NULL ) {

			tmp= (*queue)->next;
			free( *queue );
			
		} else {

			tmp= NULL;
			free( *queue );
			
		}

		*queue= tmp;
	}
}


void FileIOEndQueue_push( FileIOEndRecord** queue, FileIOEndRecord* append ) {


	FileIOEndRecord* tmp;
	FileIOEndRecord* last;


	tmp= (*queue);
	last= NULL;

	while( NULL != tmp ) {

		if( append->time < tmp->time ) break;
		
		last= tmp;
		tmp= tmp->next;
	}

	if( NULL != last ) {
	
		append->next= tmp;
		last->next= append;
		
	} else {

		append->next= tmp;
		*queue= append;
	}
}
