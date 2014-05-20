/*
 This is part of the OTF library. Copyright by ZIH, TU Dresden 2005-2013.
 Authors: Andreas Knuepfer, Holger Brunst, Ronny Brendel, Thomas Kriebitzsch
*/

#ifndef OTFTOVTF3_TREEHASH_H
#define OTFTOVTF3_TREEHASH_H

#ifdef HAVE_CONFIG_H
#include "config.h"
#endif


#include <assert.h>

#include "OTF_inttypes.h"
#include "OTF_Platform.h"


#define STACK_ALLOC_WIDTH 10


/* *** queue of FileIOEnd records *********************/
typedef struct FileIOEndRecord_s {

	uint64_t time;
	uint32_t process;
	int iotype;
	int fileid;
	uint32_t bytes;
	int source;

	struct FileIOEndRecord_s* next;
	
} FileIOEndRecord;


/* *** tree structure *******************************/
typedef struct nodeS
{
	uint32_t process; /* cpu token of otf */
	uint32_t processi; /* index of cpu for vtf3 */
	char *name; /* process name */
	int childrensize; /* sizeof children vector */
	struct nodeS **p_children; /* vector of children */

    /* function stack */
    uint32_t *stack;
    uint32_t stackc;
    uint32_t stacks;

}nodeT;

/* *** hash structures *******************************/
typedef struct entryvecS
{
	nodeT *p_node; /* pointer to the treenode */
}entryvecT;

typedef struct hashtabS
{
	nodeT *p_node;
	
	int entryvecsize;
	entryvecT * p_entryvec;
}hashtabT;

typedef struct {

	void *fcb;
	hashtabT *p_hashtab;
	FileIOEndRecord* FileIOQueue;
	
}fcbT;

/* *** FileIOEndQueue functions *************************/
/* init */
FileIOEndRecord* FileIOEndQueue_init( void );


void FileIOEndQueue_finish( FileIOEndRecord** queue );

/* check if there are events matching the current timestamp */
void FileIOEndQueue_check( FileIOEndRecord** queue, uint64_t time, void* fha );

/* add an element at the end */
void FileIOEndQueue_push( FileIOEndRecord** queue, FileIOEndRecord* append );


/* *** treehash functions *******************************/

/* initialize the tree and the hash */
void treehash_init( nodeT **pp_root, hashtabT **pp_hashtab );

/* add a node to the tree and hash */
int treehash_addnode( hashtabT *p_hashtab, uint32_t process, const char* name, uint32_t parent );

/* search a node using the hash */
nodeT *treehash_searchnode ( hashtabT *p_hashtab, uint32_t process );

/* search the entryvector of a tree (binary search) -- only used intern -- */
nodeT *treehash_searchvec ( entryvecT* vec, int vecsize, uint32_t process );

/* create indices (treeform -> linear for vtf3 */
int treehash_createindices ( int index, nodeT *p_node );

/* free all memory ... */
void treehash_deleteall ( hashtabT *p_hashtab );

/* free a treenode */
void treehash_deletenode ( nodeT *p_node );

#endif /* OTFTOVTF3_TREEHASH_H */
